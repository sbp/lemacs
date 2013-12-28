/* Efficient caching of X GCs (graphics contexts).
   Copyright (C) 1993 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Emacs uses a lot of different display attributes; for example, assume
   that only four fonts are in use (normal, bold, italic, and bold-italic.)
   Then assume that one stipple or background is used for text selections,
   and another is used for highlighting mousable regions.  That makes 16
   GCs already.  Add in the fact that another GC may be needed to display
   the text cursor in any of those regions, and you've got 32.  Add in 
   more fonts, and it keeps increasing exponentially.

   We used to keep these GCs in a cache of merged (fully qualified) faces.
   However, a lot of other code in xterm.c used XChangeGC of existing GCs,
   which is kind of slow and kind of random.  Also, managing the face cache
   was tricky because it was hard to know when a face was no longer visible
   on the screen -- we had to mark all screens as garbaged whenever a face
   was changed, which caused an unpleasant amount of flicker (since faces are
   created/destroyed (= changed) whenever a screen is created/destroyed.

   So this code maintains a cache at the GC level instead of at the face 
   level.  There is an upper limit on the size of the cache, after which we
   will stop creating GCs and start reusing them (reusing the least-recently-
   used ones first.)  So if faces get changed, their GCs will eventually be
   recycled.  Also more sharing of GCs is possible.

   This code uses hashtables.  It could be that, if the cache size is small
   enough, a linear search might be faster; but I doubt it, since we need
   `equal' comparisons, not `eq', and I expect that the optimal cache size
   will be ~100.

   Written by jwz, 14 jun 93
 */

#include "config.h"
#include "blockio.h"
#include <X11/Xlib.h>
#include "xgccache.h"


#define GC_CACHE_SIZE 10

#define GCCACHE_HASH


#ifdef GCCACHE_HASH
#include "lisp.h"
#include "hash.h"
#endif

struct gcv_and_mask {
  XGCValues gcv;
  unsigned long mask;
};

struct gc_cache_cell {
  GC gc;
  struct gcv_and_mask gcvm;
  struct gc_cache_cell *prev, *next;
};

struct gc_cache {
  Display *dpy;		/* used only as arg to XCreateGC/XFreeGC */
  Window window;	/* used only as arg to XCreateGC */
  int size;
  struct gc_cache_cell *head;
  struct gc_cache_cell *tail;
#ifdef GCCACHE_HASH
  c_hashtable table;
#endif

  int create_count;
  int delete_count;
};

#ifdef GCCACHE_HASH
static unsigned long 
gc_cache_hash (CONST void *arg)
{
  struct gcv_and_mask *gcvm = (struct gcv_and_mask *) arg;
  register unsigned long *longs = (unsigned long *) &gcvm->gcv;
  register unsigned long hash = gcvm->mask;
  register int i;
  /* This could look at the mask and only use the used slots in the
     hash code.  That would win in that we wouldn't have to initialize
     every slot of the gcv when calling gc_cache_lookup.  But we need
     the hash function to be as fast as possible; some timings should
     be done. */
  for (i = 0; i < (sizeof (XGCValues) / sizeof (unsigned long)); i++)
    hash = (hash<<1) ^ *longs++;
  return hash;
}

#endif /* GCCACHE_HASH */

static int 
gc_cache_eql (CONST void *arg1, CONST void *arg2)
{
  /* See comment in gc_cache_hash */
  return (!memcmp (arg1, arg2, sizeof (struct gcv_and_mask)));
}

struct gc_cache *
make_gc_cache (Display *dpy, Window window)
{
  struct gc_cache *cache =
    (struct gc_cache *) xmalloc (sizeof (struct gc_cache));
  cache->dpy = dpy;
  cache->window = window;
  cache->size = 0;
  cache->head = cache->tail = 0;
  cache->create_count = cache->delete_count = 0;
#ifdef GCCACHE_HASH
  cache->table =
    make_general_hashtable (GC_CACHE_SIZE, gc_cache_hash, gc_cache_eql);
#endif
  return cache;
}

void
free_gc_cache (struct gc_cache *cache)
{
  struct gc_cache_cell *rest, *next;
  BLOCK_INPUT;
  rest = cache->head;
  while (rest)
    {
      XFreeGC (cache->dpy, rest->gc);
      next = rest->next;
      xfree (rest);
      rest = next;
    }
  UNBLOCK_INPUT;
#ifdef GCCACHE_HASH
  free_hashtable (cache->table);
#endif
  xfree (cache);
}

GC
gc_cache_lookup (struct gc_cache *cache, XGCValues *gcv, unsigned long mask)
{
  struct gc_cache_cell *cell, *next, *prev;
  struct gcv_and_mask gcvm;

  if ((!!cache->head) != (!!cache->tail)) abort ();
  if (cache->head && (cache->head->prev || cache->tail->next)) abort ();

  gcvm.mask = mask;
  gcvm.gcv = *gcv;	/* this copies... */

#ifdef GCCACHE_HASH

  if (gethash (&gcvm, cache->table, (void *) &cell))

#else /* !GCCACHE_HASH */

  cell = cache->tail;	/* start at the end (most recently used) */
  while (cell)
    {
      if (gc_cache_eql (&gcvm, &cell->gcvm))
	break;
      else
	cell = cell->prev;
    }

  if (cell)

#endif /* !GCCACHE_HASH */

    {
      /* Found a cell.  Move this cell to the end of the list, so that it
	 will be less likely to be collected than a cell that was accessed
	 less recently.
       */
      if (cell == cache->tail)
	return cell->gc;

      next = cell->next;
      prev = cell->prev;
      if (prev) prev->next = next;
      if (next) next->prev = prev;
      if (cache->head == cell) cache->head = next;
      cell->next = 0;
      cell->prev = cache->tail;
      cache->tail->next = cell;
      cache->tail = cell;
      if (cache->head == cell) abort ();
      if (cell->next) abort ();
      if (cache->head->prev) abort ();
      if (cache->tail->next) abort ();
      return cell->gc;
    }

  /* else, cache miss. */

  if (cache->size == GC_CACHE_SIZE)
    /* Reuse the first cell on the list (least-recently-used.)
       Remove it from the list, and unhash it from the table.
     */
    {
      cell = cache->head;
      cache->head = cell->next;
      cache->head->prev = 0;
      if (cache->tail == cell) cache->tail = 0; /* only one */
      BLOCK_INPUT;
      XFreeGC (cache->dpy, cell->gc);
      cache->delete_count++;
      UNBLOCK_INPUT;
#ifdef GCCACHE_HASH
      remhash (&cell->gcvm, cache->table);
#endif
    }
  else if (cache->size > GC_CACHE_SIZE)
    abort ();
  else
    {
      /* Allocate a new cell (don't put it in the list or table yet.)
       */
      cell = (struct gc_cache_cell *) xmalloc (sizeof (struct gc_cache_cell));
      cache->size++;
    }

  /* Now we've got a cell (new or reused).  Fill it in. */
  memcpy (&cell->gcvm.gcv, gcv, sizeof (XGCValues));
  cell->gcvm.mask = mask;

  /* Put the cell on the end of the list. */
  cell->next = 0;
  cell->prev = cache->tail;
  if (cache->tail) cache->tail->next = cell;
  cache->tail = cell;
  if (! cache->head) cache->head = cell;

  cache->create_count++;
#ifdef GCCACHE_HASH
  /* Hash it in the table */
  puthash (&cell->gcvm, cell, cache->table);
#endif

  /* Now make and return the GC. */
  BLOCK_INPUT;
  cell->gc = XCreateGC (cache->dpy, cache->window, mask, gcv);
  UNBLOCK_INPUT;

  /* debug */
  if (cell->gc != gc_cache_lookup (cache, gcv, mask)) abort ();

  return cell->gc;
}


#if 1

#include <stdio.h>

void
describe_gc_cache (struct gc_cache *cache)
{
  int count = 0;
  struct gc_cache_cell *cell = cache->head;
  fprintf (stderr, "\nsize:    %d", cache->size);
  fprintf (stderr, "\ncreated: %d", cache->create_count);
  fprintf (stderr, "\ndeleted: %d", cache->delete_count);
  while (cell)
  {
    struct gc_cache_cell *cell2;
    int i = 0;
    fprintf (stderr,"\n%d:\t0x%x  GC: 0x%08x  hash: 0x%08x\n",
	     count, (int) cell, (int) cell->gc, gc_cache_hash (&cell->gcvm));
    for (cell2 = cache->head; cell2; cell2 = cell2->next, i++)
      if (count != i &&
	  gc_cache_hash (&cell->gcvm) == gc_cache_hash (&cell2->gcvm))
	fprintf (stderr, "\tHASH COLLISION with cell %d\n", i);
    fprintf (stderr,"\tmask:       %8x\n", cell->gcvm.mask);
#define F(x) (int)cell->gcvm.gcv.x
#define G(w,x) if (F(x) != (~0)) fprintf (stderr, "\t%-12s%8x\n", w, F(x))
    G("function:", function);
    G("plane_mask:", plane_mask);
    G("foreground:", foreground);
    G("background:", background);
    G("line_width:", line_width);
    G("line_style:", line_style);
    G("cap_style:", cap_style);
    G("join_style:", join_style);
    G("fill_style:", fill_style);
    G("fill_rule:", fill_rule);
    G("arc_mode:", arc_mode);
    G("tile:", tile);
    G("stipple:", stipple);
    G("tsx_origin:", ts_x_origin);
    G("tsy_origin:", ts_y_origin);
    G("font:", font);
    G("subwindow:", subwindow_mode);
    G("gexposures:", graphics_exposures);
    G("clip_x:", clip_x_origin);
    G("clip_y:", clip_y_origin);
    G("clip_mask:", clip_mask);
    G("dash_off:", dash_offset);
#undef F
#undef G
    count++;
    if (cell->next && cell == cache->tail)
      fprintf (stderr, "\nERROR!  tail is here!\n\n");
    else if (!cell->next && cell != cache->tail)
      fprintf (stderr, "\nERROR!  tail is not at the end\n\n");
    cell = cell->next;
  }
  if (count != cache->size)
    fprintf (stderr, "\nERROR!  count should be %d\n\n", cache->size);
}

#endif
