/* This file is part of GNU Emacs.

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

#include "config.h"
#include "lisp.h"
#include "intl.h"
#include "buffer.h" 
#include "process.h"

#include "xterm.h" 	/* for struct x_bitmap in set_extent_glyph() */
#include "xobjs.h"	/* for PIXMAPP in extent-glyph */

#include "extents.h"
#include "faces.h"
#include "hash.h"

#include <stdio.h>		/* for sprintf */

#ifdef ENERGIZE
extern void restore_energize_extent_state (EXTENT extent);
extern struct Energize_Extent_Data *energize_extent_data (EXTENT);
extern Lisp_Object Qenergize;
#endif


/* These structures are closures used to communicate between the various
   callers and map-functions of map_extents().
 */
struct slow_map_extents_arg
{
  Lisp_Object map_arg;
  Lisp_Object map_routine;
  Lisp_Object result;
};

struct slow_map_extent_children_arg
{
  Lisp_Object map_arg;
  Lisp_Object map_routine;
  Lisp_Object result;
  int start_min;
  int prev_start;
  int prev_end;
};

struct replicate_extents_arg
{
  int from;
  int length;
  struct buffer *buf;
  Lisp_Object head;
  Lisp_Object nconc_cell;
};

struct process_extents_for_insertion_arg
{
  int opoint;
  int length;
  struct buffer *buf;
};
   
struct process_extents_for_deletion_arg
{
  int start;
  int end;
  int destroy_included_extents;
};

struct extent_at_arg
{
  EXTENT best_match;
  int best_start;
  int best_end;
  Lisp_Object prop;
  EXTENT before;
};

struct verify_extents_arg
{
  struct buffer *buf;
  int closed;
  int start;
  int end;
};


/************** Functions ***********************/
static void soe_push (EXTENT extent, struct buffer *b);
static void soe_delq (EXTENT extent, struct buffer *b);
static void soe_clear (struct buffer *b);

static EXTENT_FRAGMENT befa_internal (int pos, struct buffer *buf,
				      struct screen *s);
EXTENT_FRAGMENT buffer_extent_fragment_at (int pos, struct buffer *buf,
                                           struct screen *s,
					   int include_zero_width);

static Lisp_Object insert_extent(EXTENT extent, int new_start, int new_end,
				 struct buffer *buf, int run_hooks);
static void add_to_replicas_lists (c_hashtable table, Lisp_Object dup_list, 
                                   int offset, int length,
				   int clip_parts, int total_length,
				   Lisp_Object *cells_vec);


static int plists_differ (Lisp_Object a, Lisp_Object b, int depth);

/************** Macros ***********************/

#define max(A,B) ((A) > (B) ? (A) : (B))
#define min(A,B) ((A) <= (B) ? (A) : (B))

#define MAX_INT ((long) ((1L << VALBITS) - 1))

#define BUFFER_POS_TO_START_INDEX(x,buf,extent) \
	buffer_pos_to_extent_index (x, buf, EXTENT_START_OPEN_P (extent))
#define BUFFER_POS_TO_END_INDEX(x,buf,extent) \
	buffer_pos_to_extent_index (x, buf, !EXTENT_END_OPEN_P (extent))

/* if buf is the one in the cache, invalidate the cache -- used in
   places where changes to extents list occur that might not affect 
   the buffer modiff */
#define CHECK_RESET_EXTENT_FRAGMENT(x) \
{if (((char *) extent_fragment.buf) == ((char *)(x))) \
  {extent_fragment.buf = 0; Vextent_fragment_buffer = Qnil;}}

/* Do low-level comparisons with a 1/2 bit shifted into the LSB. */
#define REGION_START_VAL(start, start_open) \
	(((start) << 1) + (start_open))
#define REGION_END_VAL(end, end_open) \
	(((end) << 1) + 1-(end_open))

/* These two macros yield the comparison keys for extent ordering.
   These keys are **NOT** buffer positions or buffer addresses.
   They must be compared only to other values of the same type.
 */
#define EXTENT_START_VAL(e) \
	REGION_START_VAL(extent_start ((e)), EXTENT_START_OPEN_P(e))
#define EXTENT_END_VAL(e) \
	REGION_END_VAL  (extent_end ((e)),   EXTENT_END_OPEN_P(e))

/* Extent A is "less than" extent B, that is, earlier in the display order,
   if:    A-start < B-start,
   or if: A-start = B-start, and A-end > B-end
          which is to say, A-length > B-length
   So if two extents begin at the same position, the larger of them is the
   earlier one in the display order (EXTENT_LESS is true.)

   For the e-order, the same thing holds: Extent A is "less than" extent B
   in e-order, that is, later in the buffer,
   if:    A-end < B-end,
   or if: A-end = B-end, and A-start < B-start
          which is to say, A-length > B-length
   So if two extents end at the same position, the larger of them is the
   earlier one in the e-order (EXTENT_E-LESS is true.)
 */

#define EXTENT_LESS_VALS(e,st,nd) ((EXTENT_START_VAL(e) < (st)) || \
				   ((EXTENT_START_VAL(e) == (st)) && \
				   (EXTENT_END_VAL(e) > (nd))))

#define EXTENT_LESS(e,x) \
	EXTENT_LESS_VALS(e, EXTENT_START_VAL(x), EXTENT_END_VAL(x))

#define EXTENT_E_LESS_VALS(e,st,nd) ((EXTENT_END_VAL(e) < (nd)) || \
				     ((EXTENT_END_VAL(e) == (nd)) && \
				     (EXTENT_START_VAL(e) < (st))))

#define EXTENT_E_LESS(e,x) \
	EXTENT_E_LESS_VALS(e,EXTENT_START_VAL(x),EXTENT_END_VAL(x))

#define EXTENT_OVER_INDEX(e,i) (((extent_start ((e)) <= (i)) \
				 && (extent_end ((e)) > (i))) \
				|| ((extent_start ((e)) == (i)) \
				    && (extent_end ((e)) == (i))))
  
#define EXTENT_PAST_INDEX(e,i) ((extent_end ((e)) > (i)) || \
				(((extent_start ((e)) == (i)) \
				  && (extent_end ((e)) == (i)))))

/************** Variables ***********************/

Lisp_Object Vlast_highlighted_extent;

static Lisp_Object Vextent_fragment_buffer;
static struct extent_fragment extent_fragment;
static struct extent_fragment default_extent_fragment;

int extent_cache_invalid;	/* set this to force a recomputation */

Lisp_Object Qextentp;
Lisp_Object Qextent_replica_p;

Lisp_Object Qdetached;
Lisp_Object Qdestroyed;
Lisp_Object Qbegin_glyph;
Lisp_Object Qend_glyph;
Lisp_Object Qstart_open;
Lisp_Object Qend_open;
Lisp_Object Qstart_closed;
Lisp_Object Qend_closed;
Lisp_Object Qread_only;
Lisp_Object Qhighlight;
Lisp_Object Qunique;
Lisp_Object Qduplicable;
Lisp_Object Qinvisible;
Lisp_Object Qpriority;

Lisp_Object Qglyph_layout;
Lisp_Object Qoutside_margin;
Lisp_Object Qinside_margin;
Lisp_Object Qwhitespace;
Lisp_Object Qtext;

Lisp_Object Qglyph_invisible;

Lisp_Object Qcopy_function;
Lisp_Object Qpaste_function;


static Lisp_Object mark_extent (Lisp_Object, void (*) (Lisp_Object));
static Lisp_Object mark_extent_replica (Lisp_Object, void (*) (Lisp_Object));
static int extent_equal (Lisp_Object, Lisp_Object, int depth);
static int extent_replica_equal (Lisp_Object, Lisp_Object, int depth);

DEFINE_LRECORD_IMPLEMENTATION ("extent", lrecord_extent,
                               mark_extent, print_extent_or_replica, 0,
			       extent_equal, sizeof (struct extent));
DEFINE_LRECORD_IMPLEMENTATION ("extent-replica", lrecord_extent_replica,
                               mark_extent_replica, print_extent_or_replica,
                               0, extent_replica_equal,
			       sizeof (struct extent_replica));


static Lisp_Object
mark_extent (Lisp_Object obj, void (*markobj) (Lisp_Object))
{
  struct extent *extent = XEXTENT (obj);
  Lisp_Object next;
  if (gc_record_type_p (extent->ehead.buf, lrecord_extent))
    /* Can't be a replica here! */
    abort ();

  ((markobj) (extent->plist));

  ((markobj) (extent_buffer (extent)));

  if (EXTENT_BEGIN_GLYPH_P (extent))
    ((markobj) (glyph_to_pixmap (extent_begin_glyph (extent))));
  if (EXTENT_END_GLYPH_P (extent))
    ((markobj) (glyph_to_pixmap (extent_end_glyph (extent))));

  if (!extent->next) return (Qnil);
  if (extent == extent->next) abort ();
  /* Mark the extent and all following extents in the chain.  We can
     stop marking as soon as we hit one that is already marked, because
     we know that we have either already marked the rest of the chain,
     or are about to (higher up on the stack.)

     This relies on the fact that a detached extent has no `next',
     and that extents of dead buffers are detached.
     */
  XSETEXTENT (next, extent->next);
  return (next);
}

static Lisp_Object
mark_extent_replica (Lisp_Object obj, void (*markobj) (Lisp_Object))
{
  struct extent_replica *dup = XDUP (obj);
  if (!gc_record_type_p (dup->ehead.buf, lrecord_extent))
    /* Can't be an extent here! */
    abort ();
  return (dup_extent (dup));
}


static char *
print_extent_1 (char *buf, Lisp_Object extent_obj)
{
  int from = XINT (Fextent_start_position (extent_obj));
  int to = XINT (Fextent_end_position (extent_obj));
  EXTENT ext = XEXTENT (extent_obj);
  char *bp = buf;
  Lisp_Object tail;

  if (EXTENT_BEGIN_GLYPH_P (ext)) *bp++ = '*';
  *bp++ = (EXTENT_START_OPEN_P (ext) ? '(': '[');
  if (EXTENT_DETACHED_P (ext))
    sprintf (bp, "detached");
  else
    sprintf (bp, "%d, %d", from, to);
  bp += strlen (bp);
  *bp++ = (EXTENT_END_OPEN_P (ext)   ? ')': ']');
  if (EXTENT_END_GLYPH_P (ext)) *bp++ = '*';
  *bp++ = ' ';

  if (EXTENT_READ_ONLY_P (ext)) *bp++ = '%';
  if (EXTENT_HIGHLIGHT_P (ext)) *bp++ = 'H';
  if (EXTENT_UNIQUE_P (ext)) *bp++ = 'U';
  else if (EXTENT_DUPLICABLE_P (ext)) *bp++ = 'D';
  if (EXTENT_INVISIBLE_P (ext)) *bp++ = 'I';

  if (EXTENT_READ_ONLY_P (ext) || EXTENT_HIGHLIGHT_P (ext) ||
      EXTENT_UNIQUE_P (ext) || EXTENT_DUPLICABLE_P (ext) ||
      EXTENT_INVISIBLE_P (ext))
    *bp++ = ' ';

  tail = ext->plist;

  /* If there are two glyphs, one of them stole the first cons of the plist. */
  if (EXTENT_BEGIN_GLYPH_P (ext) && EXTENT_END_GLYPH_P (ext))
    {
      if (!CONSP (tail)) abort ();
      tail = XCONS (tail)->cdr;
    }

  for (; !NILP (tail); tail = Fcdr (Fcdr (tail)))
    {
      struct Lisp_String *k = XSYMBOL (XCONS (tail)->car)->name;
      Lisp_Object v = XCONS (XCONS (tail)->cdr)->car;
      if (NILP (v)) continue;
      memcpy (bp, (char *) k->data, k->size);
      bp += k->size;
      *bp++ = ' ';
    }
  sprintf (bp, "0x%x", (int) ext);
  bp += strlen (bp);

  *bp++ = 0;
  return buf;
}

void
print_extent_or_replica (Lisp_Object obj, 
                         Lisp_Object printcharfun, int escapeflag)
{
  char buf2[256];

  if (EXTENTP (obj))
    {
      if (escapeflag)
	{
	  CONST char *title = "";
	  CONST char *name = "";

	  if (NILP (extent_buffer (XEXTENT (obj))))
	    title = "no buffer";
	  else if (BUFFERP (extent_buffer (XEXTENT (obj))))
	    {
	      struct buffer *buf = XBUFFER (extent_buffer (XEXTENT (obj)));
	      if (STRINGP (buf->name))
		{
		  title = "buffer ";
		  name = (char *) XSTRING ((buf)->name)->data;
		}
	      else
		{
		  title = "Killed Buffer";
		  name = "";
		}
	    }
	  else
	    title = "(INTERNAL ERROR)";
	  
	  if (print_readably)
	    {
	      if (EXTENT_DESTROYED_P (XEXTENT (obj)))
		error (GETTEXT ("printing unreadable object #<destroyed extent>"));
	      else
		error (GETTEXT ("printing unreadable object #<extent %s>"),
		       print_extent_1 (buf2, obj));
	    }
	  
	  if (EXTENT_DESTROYED_P (XEXTENT (obj)))
	    write_string_1 ("#<destroyed extent", -1, printcharfun);
	  else
	    {
	      char buf[256];
	      write_string_1 ("#<extent ", -1, printcharfun);
	      if (EXTENT_DETACHED_P (XEXTENT (obj)))
		sprintf (buf, "%s from %s%s",
			 print_extent_1 (buf2, obj), title, name);
	      else
		sprintf (buf, "%s in %s%s",
			 print_extent_1 (buf2, obj),
			 title, name);
	      write_string_1 (buf, -1, printcharfun);
	    }
	}
      else
	{
	  if (print_readably)
	    error (GETTEXT ("printing unreadable object #<extent>"));
	  write_string_1 ("#<extent", -1, printcharfun);
	}
      write_string_1 (">", 1, printcharfun);
    }
  else if (EXTENT_REPLICA_P (obj))
    {
      if (escapeflag)
	{
	  char buf[256];

	  if (print_readably)
	    error (GETTEXT ("printing unreadable object #<dup [%d, %d)>"),
		   dup_start (XDUP (obj)), dup_end (XDUP (obj)));

	  write_string_1 ("#<dup ", -1, printcharfun);
	  if (EXTENTP (dup_extent (XDUP (obj)))
	      && !(EXTENT_DESTROYED_P (XEXTENT (dup_extent (XDUP (obj))))))
	    {
	      Lisp_Object extent_obj = dup_extent (XDUP (obj));
 	      sprintf (buf, "[%d, %d) of extent %s",
 		       dup_start (XDUP (obj)), dup_end (XDUP (obj)),
		       print_extent_1 (buf2, extent_obj));
	    }
	  else
	    {
	      sprintf (buf, "[%d, %d) of 0x%x",
		       dup_start (XDUP (obj)), dup_end (XDUP (obj)),
		       (int) LISP_TO_VOID (dup_extent (XDUP (obj))));
	    }
	  write_string_1 (buf, -1, printcharfun);
	}
      else
	{
	  if (print_readably)
	    error (GETTEXT ("printing unreadable object #<extent>"));
	  write_string_1 ("#<dup", -1, printcharfun);
	}
      write_string_1 (">", -1, printcharfun);
    }
}


static int
extent_equal (Lisp_Object o1, Lisp_Object o2, int depth)
{
  struct extent *e1 = XEXTENT (o1);
  struct extent *e2 = XEXTENT (o2);
  Lisp_Object p1, p2;
  if (! (extent_start (e1) == extent_start (e2) &&
	 extent_end (e1) == extent_end (e2) &&
	 extent_face_id (e1) == extent_face_id (e2) &&
	 extent_priority (e1) == extent_priority (e2) &&
	 !memcmp (&e1->flags, &e2->flags, sizeof (e1->flags)) &&
	 internal_equal (extent_buffer (e1), extent_buffer (e2), depth + 1)))
    return 0;
  p1 = e1->plist;
  p2 = e2->plist;
  /* If there are two glyphs, one of them stole the first cons of the plist. */
  if (EXTENT_BEGIN_GLYPH_P (e1) && EXTENT_END_GLYPH_P (e1)) /* e2 is same */
    {
      if (!CONSP (p1)) abort ();
      if (!CONSP (p2)) abort ();
      /* compare the end-glyphs */
      if (! internal_equal (XCONS (p1)->car, XCONS (p2)->car, depth + 1))
	return 0;
      p1 = XCONS (p1)->cdr;
      p2 = XCONS (p2)->cdr;
    }
  /* compare the random elements of the plists. */
  return (!plists_differ (p1, p2, depth + 1));
}

static int
extent_replica_equal (Lisp_Object o1, Lisp_Object o2, int depth)
{
  struct extent_replica *e1 = XEXTENT_REPLICA (o1);
  struct extent_replica *e2 = XEXTENT_REPLICA (o2);
  if (!dup_live_p (e1) && !dup_live_p (e2))
    return 1;
  return (dup_start (e1) == dup_start (e2) &&
	  dup_end (e1) == dup_end (e2) &&
	  internal_equal (dup_extent (e1), dup_extent (e2), depth + 1));
}


/* For properties of text, we need to do order-insensitive comparison of
   plists.  That is, we need to compare two plists such that they are the
   same if they have the same set of keys with non-nil values, and equivalent
   values.  So (a 1 b 2 c nil) would be equal to (b 2 a 1).
 */
static int 
plists_differ (Lisp_Object a, Lisp_Object b, int depth)
{
  int eqp = (depth == -1);	/* -1 as depth means us eq, not equal. */
  int la, lb, m, i, fill;
  Lisp_Object *keys, *vals;
  char *flags;
  Lisp_Object rest;

  if (NILP (a) && NILP (b))
    return 0;

  la = XINT (Flength (a));
  lb = XINT (Flength (b));
  m = (la > lb ? la : lb);
  fill = 0;
  keys = (Lisp_Object *) alloca (m * sizeof (Lisp_Object));
  vals = (Lisp_Object *) alloca (m * sizeof (Lisp_Object));
  flags = (char *) alloca (m * sizeof (char));

  /* First extract the pairs from A whose value is not nil. */
  for (rest = a; !NILP (rest); rest = XCONS (XCONS (rest)->cdr)->cdr)
    {
      Lisp_Object k = XCONS (rest)->car;
      Lisp_Object v = XCONS (XCONS (rest)->cdr)->car;
      if (NILP (v)) continue;
      keys [fill] = k;
      vals [fill] = v;
      flags[fill] = 0;
      fill++;
    }
  /* Now iterate over B, and stop if we find something that's not in A,
     or that doesn't match.  As we match, mark them. */
  for (rest = b; !NILP (rest); rest = XCONS (XCONS (rest)->cdr)->cdr)
    {
      Lisp_Object k = XCONS (rest)->car;
      Lisp_Object v = XCONS (XCONS (rest)->cdr)->car;
      if (NILP (v)) continue;
      for (i = 0; i < fill; i++)
	{
	  if (EQ (k, keys [i]))
	    {
	      if ((eqp
		   ? !EQ (v, vals [i])
		   : !internal_equal (v, vals [i], depth)))
		/* a property in B has a different value than in A */
		goto MISMATCH;
	      flags [i] = 1;
	      break;
	    }
	}
      if (i == fill)
	/* there are some properties in B that are not in A */
	goto MISMATCH;
    }
  /* Now check to see that all the properties in A were also in B */
  for (i = 0; i < fill; i++)
    if (flags [i] == 0)
      goto MISMATCH;

  /* Ok. */
  return 0;

 MISMATCH:
  return 1;
}

/*
 * DEFUN ("plists-differ", Fplists_differ, Splists_differ, 2, 3, 0, 0)
 *      (a, b, equalp)
 *      Lisp_Object a, b, equalp;
 * {
 *   return (plists_differ (a, b, NILP (equalp) ? -1 : 1) ? Qt : Qnil);
 * }
 */


/* internal utilities */

static void
check_from_to (int from, int to, struct buffer* buf,
               int check_order)
{ 
  if (from > to)
    {
      if (check_order)
	args_out_of_range (make_number (from), make_number (to));
      else
	{
	  int tem = from;
	  from = to;
	  to = tem;
	}
    }
  if (from < BUF_BEG (buf))
    args_out_of_range_3 (make_number (from), make_number (BUF_BEG (buf)),
			 make_number (BUF_Z (buf)));
  if (to > BUF_Z (buf))
    args_out_of_range_3 (make_number (to), make_number (BUF_BEG (buf)),
			 make_number (BUF_Z (buf)));
}

/* Same basic logic as adjust_markers(), to adjust indices when the gap moves.
   The flag "up" controls "fencepost" behavior:  If up!=0, place the
   index above the gap if it falls on the gap, else place it below.
   (Note:  adjust_markers() can leave markers in the gap if amount<0.)
 */
static int
adjust_extent_index (int i, int from, int to, int amount, int up)
{
  /* Some sanity checking */
  if (amount > 0)
    {
      if (i > to && i < to + amount)
	abort ();
    }
  else 
    {
      if (i > from + amount && i < from)
	abort ();
    }

  if (!up && amount > 0)
    {
      if (i > from && i <= to)
	i += amount;
    }
  else if (up && amount < 0)
    {
      if (i >= from && i < to)
	i += amount;
    }
  else
    {
  if (i >= from && i <= to)
    i += amount;
    }

  return i;
}

static int
extent_index_to_buffer_pos (int i, struct buffer *buf)
{
  if (i >= BUF_GPT (buf) + BUF_GAP_SIZE (buf))
    i -= BUF_GAP_SIZE (buf);
  else if (i > BUF_GPT (buf))
    i = BUF_GPT (buf);

  if ((i < BUF_BEG (buf)) || (i > BUF_Z (buf)))
    abort();
  else
    return i;
}

/* If on the fencepost, "up" controls whether to go above or below the gap. */
static int
buffer_pos_to_extent_index (int pos, struct buffer *buf, int up)
{
  if ((pos < BUF_BEG (buf)) || (pos > BUF_Z (buf)))
    abort();

  if (up
      ? pos < BUF_GPT (buf)
      : pos <= BUF_GPT (buf))
    return pos;
  else
    return (pos + BUF_GAP_SIZE (buf));
}

static int
buffer_pos_to_map_limit (int to, struct buffer* buf, int closed_end)
{
  if (closed_end)
    {
      if (to == BUF_Z(buf))
        return buffer_pos_to_extent_index (to, buf, 0) + 1;
      else
        return buffer_pos_to_extent_index (to + 1, buf, 0);
    }
  else
    return buffer_pos_to_extent_index (to, buf, 0);
}

static EXTENT 
buffer_starting_extent (int index, struct buffer *buf)
{
  struct stack_of_extents *soe = buf->cached_stack;
  EXTENT first_extent = NULL;
  EXTENT ext;
  int i;

  /* no extents on the buffer */
  if (!EXTENTP (buf->extents))
    return NULL;
  
  first_extent = XEXTENT (buf->extents);

  /* no cache or cache not valid */
  if (!soe || soe->buf_index <= 0)
    return first_extent;

  /* index is after the cached values */
  if (index >= soe->buf_index)
    {
      ext = NULL;
      for (i = 0; i < soe->stack_index; i++)
	if (extent_end (soe->stack [i]) > index)
	  {
	    ext = soe->stack [i];
	    break;
	  }
      if (!ext)
	ext = soe->previous_extent;
      return ext ? ext : first_extent;
    }

  /* index is before the cached values */

  /* The extent which is before the first one in the 
     cache is a good starting point for backing up
     previous extent. */
  if (soe->stack_index)
    ext = soe->stack [0];
  else
    /* if there is nothing in the cache the previous extent
       can be used */
    ext = soe->previous_extent;

  /* no previous extent in the cache */
  if (!ext)
    return first_extent;
	
  /* previous is closer to index than to the beginning of the buffer */
  if ((extent_end (ext) / 2) < index)
    {
      EXTENT starting_one = ext;

      while (ext && extent_end (ext) >= index)
	{
          ext = ext->e_previous;
          if (ext && extent_start (ext) < extent_start (starting_one))
	    starting_one = ext;
        }
      starting_one = starting_one->previous;
      return (ext && starting_one) ? starting_one : first_extent;
    }

  /* default */
  return first_extent;
}


/* At the moment only this and a few other functions
   know how to "cdr" down a list of extents. 
   See the comment at map_extents_after() for information 
   about the ordering rule. */
void
adjust_extents (int from, int to, int amount, struct buffer* buf)
{
  EXTENT current;

  if (NILP (buf->extents))
    return;
  else if (!EXTENTP (buf->extents))
    abort ();

  current = buffer_starting_extent (from, buf);
  
  CHECK_RESET_EXTENT_FRAGMENT (buf);
  
  while (current)
    {
      if (extent_end (current) < from)
	;
      else if (extent_start (current) <= to)
	{
	  int start_up = EXTENT_START_OPEN_P (current);
	  extent_start (current) = adjust_extent_index (extent_start (current),
							from, to, amount,
							start_up);
	  if (extent_end (current) <= to)
	    {
	      int end_up = !EXTENT_END_OPEN_P (current);
	      extent_end (current) = adjust_extent_index (extent_end (current),
							  from, to,
							  amount, end_up);
	      /* This should only happen if the extent is empty and on the gap,
		 and the start is open and the end is closed.  */
	      if (extent_end (current) < extent_start (current))
		extent_end (current) = extent_start (current);
	    }
	}
      else 
	break;
      
      if (current == current->next) abort ();
      current = current->next;
    }
  
  if (buf->cached_stack
      && buf->cached_stack->buf_index > 0
      && buf->cached_stack->buf_index > from
      && buf->cached_stack->buf_index <= to)
    buf->cached_stack->buf_index += amount;
}


/* verify_extent_modification() is called when a buffer is modified to 
   check whether the modification is occuring inside a read-only extent.
 */

#ifdef ENERGIZE
extern int inside_parse_buffer; /* total kludge */
#endif

static int verify_extent_mapper (EXTENT extent, void *arg);

void
verify_extent_modification (struct buffer *buf, int from, int to)
{
  int closed;
  struct verify_extents_arg closure;

  if (inside_undo ||
#ifdef ENERGIZE
      inside_parse_buffer ||
#endif
      NILP (buf->extents))
    return;

  check_from_to (from, to, buf, 0);
  closure.buf = buf;
  closure.closed = closed = (from==to); /* if insertion, visit touching extents */
  closure.start = buffer_pos_to_extent_index (from, buf, !closed);
  closure.end = buffer_pos_to_extent_index (to, buf, closed);

  if (!EXTENTP (buf->extents))
    abort();

  map_extents (from, to, 0, verify_extent_mapper,
	       (void *) &closure, buf, closed);
}

static int
verify_extent_mapper (EXTENT extent, void *arg)
{
  if (EXTENT_READ_ONLY_P (extent))
    {
      struct verify_extents_arg *closure = (struct verify_extents_arg *) arg;
      int start = extent_start (extent);
      int end = extent_end (extent);
      struct buffer *buf;
  
      /* The comparisons below assume normal "non-up" end points: [start..end).
	 Therefore, if an endpoint is "up", increment it. */
      if (!closure->closed)
	{
	  /* When deleting, always pretend the endpoints are open.
	     Must move start up, to make it open; end is already open.
	   */
	  start += 1;
	}
      else
	{
	  if (!EXTENT_END_OPEN_P (extent))
	    end += 1;
	  if (EXTENT_START_OPEN_P (extent))
	    start += 1;
	}

      if (end <= closure->start)
	return 0;
      if (start > closure->end)
	return 0;

      /* Allow deletion if the extent is completely contained in
	 the region being deleted.
	 This is important for supporting tokens which are internally
	 write-protected, but which can be killed and yanked as a whole.
	 Ignore open/closed distinctions at this point.
	 -- Rose
       */
      buf = closure->buf;
      if ((extent_index_to_buffer_pos (extent_end (extent), buf)
	   <= extent_index_to_buffer_pos (closure->end, buf))
	  &&
	  (extent_index_to_buffer_pos (extent_start (extent), buf)
	   >= extent_index_to_buffer_pos (closure->start, buf)))
	return 0;
      /* >>> Should obey inhibit-read-only */
      /* >>> Should signal a better error */
      error (GETTEXT ("Attempt to modify a read-only region"));
      /* Fsignal (Qbuffer_read_only, (list1 (Fcurrent_buffer ()))); */
      return 1;
    }
  else
    return 0;
}


/* At the moment only this and a few other functions
   know how to "cdr" down a list of extents. 
   See the comment at map_extents_after() for information 
   about the ordering rule. */
static void 
splice_extent_into_buffer (EXTENT extent, Lisp_Object buffer)
{
  Lisp_Object extents_root;
  Lisp_Object extent_obj;
  struct buffer *buf = XBUFFER(buffer);

  CHECK_BUFFER (buffer, 0);  
  CHECK_RESET_EXTENT_FRAGMENT (buf);
   
  XSETEXTENT (extent_obj, extent);

  init_buffer_cached_stack (buf);

  extents_root = buf->extents;

  if (NILP (extents_root))
    buf->extents = extent_obj;
  else if (!EXTENTP (extents_root))
    abort();
  else
    {
      int start = EXTENT_START_VAL(extent);
      int end = EXTENT_END_VAL(extent);
      EXTENT tmp = buffer_starting_extent (extent_start (extent), buf);
      EXTENT prev = (tmp ? tmp->previous : 0);

      while (tmp && EXTENT_LESS_VALS (tmp, start, end))
        {
          prev = tmp;
          tmp = tmp->next;
	  if (prev == tmp) abort ();
        }

      if (!tmp && !prev)
        abort();

      if (prev)
        { 
          EXTENT caboose = prev->next;
	  if (extent == prev) abort ();
	  if (extent == caboose) abort ();
          prev->next = extent; 
          extent->previous = prev; 
          extent->next = caboose; 
          if (caboose)
            caboose->previous = extent;
        }
      else 
        {
          EXTENT engine = tmp->previous;
          if (engine)
            engine->next = tmp;
	  if (extent == tmp) abort ();
          extent->previous = engine; 
          extent->next = tmp; 
          tmp->previous = extent; 

	  buf->extents = extent_obj;
        }

      if (!tmp)		/* one of these exists; pick one */
        tmp = prev;

      /* if tmp (some arbitrary extent in the middle of nowhere but likely
	 to be close to where we want to be) is before the extents we want
	 to insert between (in e order) then go forward
       */
      if (EXTENT_E_LESS_VALS (tmp, start, end))
	{
	  prev = tmp->e_previous;
	  while (tmp && EXTENT_E_LESS_VALS (tmp, start, end))
	    {
	      prev = tmp;
	      tmp = tmp->e_next;
	      if (prev == tmp) abort ();
	    }
	}
      else	/* go backward */
        {
          prev = tmp;
	  while (prev && !EXTENT_E_LESS_VALS (prev, start, end))
            {
	      /* we always go into this loop at least once */
              tmp = prev;
              prev = tmp->e_previous;
            }
	  if (prev == tmp) abort ();
        }

      if (!tmp && !prev)
        abort();

      if (prev)
        { 
          EXTENT caboose = prev->e_next;
	  if (extent == prev) abort ();
	  if (extent == caboose) abort ();
          prev->e_next = extent; 
          extent->e_previous = prev; 
          extent->e_next = caboose; 
          if (caboose)
            caboose->e_previous = extent;
        }
      else 
        {
          EXTENT engine = tmp->e_previous;
          if (engine)
            engine->e_next = tmp;
	  if (extent == tmp) abort ();
          extent->e_previous = engine; 
          extent->e_next = tmp; 
          tmp->e_previous = extent; 
        }
    }

  soe_push (extent, buf);
  extent->flags.detached = 0;
}

Lisp_Object
make_extent_internal (int from, int to, Lisp_Object buffer)
{
  EXTENT extent;
  Lisp_Object extent_obj = Qnil;
  struct buffer *buf = XBUFFER (buffer);

  CHECK_BUFFER (buffer, 0);  
  if (NILP (buf->name))
    /* I18N3 */
    error (GETTEXT ("Can't put an extent in a killed buffer"));

  check_from_to (from, to, buf, 0);
  if (from > to)
    {
      int tem = from;
      from = to;
      to = tem;
    }

  extent = make_extent ();
  XSETEXTENT (extent_obj, extent);

  extent_buffer (extent) = buffer;
  extent_face_id (extent) = -1;
  extent_priority (extent) = 0;

  extent_start (extent) = BUFFER_POS_TO_START_INDEX (from, buf, extent);
  extent_end (extent) = BUFFER_POS_TO_END_INDEX (to, buf, extent);

  /* At this point the extent is known to be [closed,open) so no
     games are necessary if the endpoints are (should be) equal. */

  splice_extent_into_buffer (extent, buffer);

  return extent_obj;
}


static Lisp_Object
make_extent_detached (Lisp_Object buffer)
{
  EXTENT extent;
  Lisp_Object extent_obj = Qnil;
  struct buffer *buf = XBUFFER (buffer);

  if (!NILP (buffer))
    {
      CHECK_BUFFER (buffer, 0);
      if (NILP (buf->name))
	error (GETTEXT ("Can't put an extent in a killed buffer"));
    }

  extent = make_extent ();
  XSETEXTENT (extent_obj, extent);

  extent_buffer (extent) = buffer;
  extent->flags.detached = 1;
  extent_face_id (extent) = -1;

  extent_start (extent) = 0;
  extent_end (extent) = 0;

#ifdef ENERGIZE
  restore_energize_extent_state (extent);
#endif
  return extent_obj;
}


static Lisp_Object
copy_extent_internal (EXTENT original, int from, int to, Lisp_Object buffer)
{
  Lisp_Object extent;
  EXTENT e;

  if (from == -1)
    extent = make_extent_detached (buffer);
  else
    extent = make_extent_internal (from, to, buffer);

  e = XEXTENT (extent);

  e->plist = Fcopy_sequence (original->plist);

  /* copy all of the flags except destroyed/detached */
  extent_face_id (e) = extent_face_id (original);
  e->flags.glyph = original->flags.glyph;
  e->flags.glyph_layout = original->flags.glyph_layout;
  e->flags.begin_glyph_p = original->flags.begin_glyph_p;
  e->flags.end_glyph_p = original->flags.end_glyph_p;
  e->flags.start_open = original->flags.start_open;
  e->flags.end_open = original->flags.end_open;
  e->flags.read_only = original->flags.read_only;
  e->flags.highlight = original->flags.highlight;
  e->flags.unique = original->flags.unique;
  e->flags.duplicable = original->flags.duplicable;
  e->flags.invisible = original->flags.invisible;

#ifdef ENERGIZE
  if (energize_extent_data (original))
    {
      e->plist = Qnil; /* slightly antisocial... */
      restore_energize_extent_state (e);
    }
#endif

  return extent;
}


int 
extent_endpoint (EXTENT extent, int endp)
{
  int i = (endp)?(extent_end (extent)):(extent_start (extent));
  if (EXTENT_DESTROYED_P (extent))
    return -1;
  else if (EXTENT_DETACHED_P (extent))
    return 0;
  else if (BUFFERP (extent_buffer (extent)))
    return extent_index_to_buffer_pos (i, XBUFFER(extent_buffer (extent)));
  else
    return i;
}


/* external utilities */

void 
detach_extent (EXTENT extent) 
{ 
  EXTENT prev = extent->previous; 
  EXTENT next = extent->next; 
  EXTENT e_prev = extent->e_previous; 
  EXTENT e_next = extent->e_next; 
  struct buffer *buf = XBUFFER(extent_buffer (extent));
  Lisp_Object obj_extent;

  if (!BUFFERP (extent_buffer (extent)))
    return;

  obj_extent = buf->extents;

  CHECK_RESET_EXTENT_FRAGMENT (buf);

  soe_delq (extent, buf);

  if (XEXTENT (obj_extent) == extent)
    {
      if (next)
        XSETEXTENT (obj_extent, next);
      else
        obj_extent = Qnil;
      
      buf->extents = obj_extent;
    }

  if (prev) 
    {
      prev->next = next; 
      extent->previous = 0;
    }
  if (next) 
    {
      next->previous = prev; 
      extent->next = 0; 
    }


  if (e_prev) 
    {
      if (e_next == e_prev) abort ();
      e_prev->e_next = e_next; 
      extent->e_previous = 0;
    }
  if (e_next) 
    {
      if (e_next == e_prev) abort ();
      e_next->e_previous = e_prev; 
      extent->e_next = 0; 
    }

  extent->flags.detached = 1;

  extent_start (extent) = 0;
  extent_end (extent) = 0;
}

/* Detatch all extents in the buffer (called when buffer is killed.) */
void
detach_buffer_extents (struct buffer *b)
{
  EXTENT e;
  if (!EXTENTP (b->extents)) return;
  e = XEXTENT (b->extents);
  b->extents = Qnil;
  while (e)
    {
      EXTENT next = e->next;
      detach_extent (e);
      e = next;
    }
}

static void 
destroy_extent (EXTENT extent) 
{
  detach_extent (extent);
  extent->flags.destroyed = 1;
  extent_start (extent) = 0;
  extent_end (extent) = 0;
  extent->next = 0;
  extent->previous = 0;
  extent->e_next = 0;
  extent->e_previous = 0;
  extent_buffer (extent) = Qnil;
}

void   
update_extent_1 (EXTENT extent, int from, int to, int set_endpoints,
		 struct buffer *buf)
{
  if (!BUFFERP (extent_buffer (extent)) ||
      XBUFFER (extent_buffer (extent)) != buf)
    {
      Lisp_Object b, e;
      XSETR (b, Lisp_Buffer, buf);
      XSETEXTENT (e, extent);
      signal_simple_error_2 (GETTEXT ("extent not part of buffer"), e, b);
    }

  if (set_endpoints)
    {
      check_from_to (from, to, buf, 1);
      
      /* most of the time the extent doesn't need to be changed -- the
	 big problem is that the kernel actually doesn't know what is going
	 all too often */
      if ((from < to) && (EXTENT_DETACHED_P (extent) ||
			  (extent_endpoint (extent, 0) != from) ||
			  (extent_endpoint (extent, 1) != to)))
	{
	  int new_start = BUFFER_POS_TO_START_INDEX (from, buf, extent);
	  int new_end = BUFFER_POS_TO_END_INDEX (to, buf, extent);
	  Lisp_Object buffer;
	  XSETR (buffer, Lisp_Buffer, buf);
	  
	  detach_extent (extent);
	  extent_start (extent) = new_start;
	  extent_end (extent) = new_end;

	  /* #### Kludge!!!  If the extent is (open,open) and the endpoints are
	     the same, the above calls to BUFFER_POS_TO_* will give it the
	     bogus endpoints (N,N-1).  So, hack it out...
	   */
	  if (from == to)
	    extent_end (extent) = extent_start (extent);

	  splice_extent_into_buffer (extent, buffer);
	}
    }

#ifdef ENERGIZE
  restore_energize_extent_state (extent);
#endif
}


/* map-extents */

/* Get rid of: */
struct fixup_emf_arg
{
  elisp_emf efn;
  void* arg;
};
static int
fixup_emf (EXTENT extent, void *arg)
{
  struct fixup_emf_arg *closure = (struct fixup_emf_arg *) arg;
  Lisp_Object extent_obj;
  XSETEXTENT (extent_obj, extent);
  return (*closure->efn)(extent_obj, closure->arg);
}

/* Apply a function to each extent overlapping [from, to) (or [from, to],
   if the closed_end arg is non-zero) in buffer. If the function ever
   returns a non-zero value, quit immediately.  At the moment only the
   following functions know how to "cdr" down a list of extents:
	adjust_extents()
	splice_extent_into_buffer()  [the only fn. which modifies the list]
	map_extents_after()
	update_cache_forward()
	buffer_extent_fragment_at()  [actually, befa_internal]
	extent_in_region_p()	     [uses the ordering rule only]
   Extents are ordered with increasing start position and then decreasing
   end position. (This is what might be called "display order" -- if extent
   A occurs after extent B, then the display attributes of extent A
   override those of extent B in the region covered by both A and B. Note
   that multiple extents with the same start and end postions may be in any
   order.)

   The effect of a closed end or open beginning is as if 1/2 were added
   to the index, and the resulting rational number used for comparisons.
   This rule is easier to implement than the other obvious rule, which
   ignores open/closed bits for ordering, and must therefore re-sort
   extents after insertions.  The other rule is perhaps more intuitive.
   Most functions ignore the open/closed distinction, but those which
   maintain the ordering invariants or move the gap must pay attention.

   Also, map_extents pays attention to the open/closed bits when
   calculating extent overlaps.  The region that map_extents traverses
   is either a [closed, open) or [closed, closed] extent, depending on
   the closed_end argument.  The extents [a,b) and [c,d) overlap if
   and only if ((a<d && c<b) || (a<=d && c<=b && a==c)).  All comparisons 
   use the 1/2 rule for comparing endpoints.  (Note: we could simplify
   the rule to ((a<d && c<b) || a==c) if we knew that a<b and c<d always,
   but this is violated by the 1/2 rule for regions like (3,3).)

   Part of this comment is duplicated at update_cache_forward().

   The "after" aspect of this function supports iteration restarting.
   If this extent is non-null, skip any extents up to and including it.

   The "efn" argument is ill-conceived because it is redundant with the
   "fn" argument.  It is unused in this file.  It should be removed.  -- Rose */
static void
map_extents_after (int from, int to, elisp_emf efn, emf fn, void *arg,
		   struct buffer *buf, EXTENT after, int closed_end,
		   int might_modify)
{
  Lisp_Object extents_root = buf->extents;
  int start;
  int end;

  /* Get rid of: */
  struct fixup_emf_arg closure;
  if (!fn)
    {
      closure.efn = efn;
      closure.arg = arg;
      fn = fixup_emf;
      arg = (void*) &closure;
    }

  if ((from > to) || (NILP (extents_root)))
    return;
  
  if (after)
    {
      if (!BUFFERP (extent_buffer (after))
	  || XBUFFER (extent_buffer (after)) != buf)
	return;
      if (EXTENT_DETACHED_P (after))
	return;
    }

  /* making an error here is wrong as this can be called from within
     * make_gap () during which the buffer state is incoherent, ie the new
     * GPT is already set but the new Z is not. 
     * This is a little scary, maybe the function that updates the
     * extents when the gap is moved should not call map_extents. 
     * --Matthieu. */
  /* check_from_to (from, to, buf, 0); */
  /* We silently return if mapping out of the buffer valid positions */
  if ((from < BUF_BEG (buf)) || (from > BUF_Z (buf))
      || (to < BUF_BEG (buf)) || (to > BUF_Z (buf)))
    return;

  if (closed_end != 0)
    /* Normalize closed_end to 0 or 1 (instead of 0 or non-0.) */
    closed_end = 1; 

  start = buffer_pos_to_extent_index (from, buf, 0);
  end = buffer_pos_to_map_limit (to, buf, closed_end);

  if (!EXTENTP (extents_root))
    abort();
  else 
    {
      EXTENT tmp = buffer_starting_extent (start, buf);
      EXTENT next;

      /* Adjust by 1/2 upward for open start and closed end. */
      start = REGION_START_VAL(start, 0);
      end -= closed_end;	/* change buffer_pos_to_map_limit() offset to 1/2 */
      end = REGION_END_VAL(end, 1-closed_end);

      for (; tmp; tmp = next)
	{
	  int zero_check;	/* used to make 0-width extents show up */
	  /* this lets the map function be delete_extent, too */
	  next = tmp->next;

	  if (EXTENT_DETACHED_P (tmp))
	    abort ();
	  if (next && EXTENT_DETACHED_P (next))
	    abort ();

	  if (next == tmp) abort ();
	  if (after)
	    {
	      /* make sure the map suppresses everything up through after */
	      if (extent_start (tmp) < extent_start (after)) continue;
	      else if (extent_start (tmp) > extent_start (after))  after = 0;
	      else if (tmp != after) continue;
	      else	{ after = 0; continue; }
	    }

	  zero_check = 0;
	  if (extent_index_to_buffer_pos (extent_start (tmp), buf) ==
	      extent_index_to_buffer_pos (extent_end (tmp), buf))
	    {
	      if ((EXTENT_END_VAL (tmp) == (start + EXTENT_START_OPEN_P (tmp)))
		  && (EXTENT_START_VAL (tmp) == (end + !closed_end)))
		zero_check = 1;
	    }

	  /* Only map those extents which have an end point whose comparison
	     value falls within the requested region, as measured by strict
	     comparisons on the values produced by the 1/2 rule.

	     The effect of this restriction on fencepost cases is as follows:

	     If the end of an extent meets the beginning of the region at a
	     mutual character position, then the extent is mapped if
	     its end is closed (i.e., the end was bumped up by 1/2, and
	     the region start was not), or if the extent start and end are
	     equal.  (The region start is never open.)
	     This is the logic of the first arm of the immediately following
	     if/else chain.

	     Similarly, if the beginning of an extent meets the end of the
	     region at a mutual character position, then the extent is mapped
	     if its start is closed (the usual case) and the region's
	     end is closed (i.e., the region end was bumped up and the
	     extent start was not), or if the region start and end are equal.
             This is the logic of the second arm of the if/else chain.
	   */
	  if (EXTENT_END_VAL(tmp) < start ||
	      (EXTENT_END_VAL(tmp) == start &&
	       EXTENT_START_VAL(tmp) != start))
	    continue;
	  else if (EXTENT_START_VAL(tmp) < end ||
		   (EXTENT_START_VAL(tmp) == end &&
		    EXTENT_START_VAL(tmp) == start))
	    {
	      /* Call the fn and test the value. */
	      if (!might_modify)
		{
		  if ((*fn)(tmp, arg))
		    return;

		  /* The fn better not mess with the rest of the extent
		     chain after this extent, or we won't be able to
		     continue mapping correctly. */
		  if (next && EXTENT_DETACHED_P (next))
		    abort ();
		}
	      else 
		{
		  /* The fn might change the buffer.
		     Take the following defensive steps:
		     - remember end's distance from the eob (use "save_to")
		     - restore end after the call, from that distance
		     - restore start from scratch after the call
		     This algorithm is cheap, but will fail if the
		     buffer is changed between end and eob or bob and start.
		     This flaw seems similar to the failure that occurs if
		     Fmap_extents deletes the extent after the current one.
		     As long as fn operates only on the current extent, all
		     should be well.  A better, slower solution would be to
		     use a marker.  -- Rose
		     */
		  int save_to = to;
		  save_to -= (BUF_Z (buf) - BUF_BEG(buf));
		  start = 0;
		  if ((*fn)(tmp, arg))
		    return;

		  if (next && EXTENT_DETACHED_P (next))
		    {
		      Lisp_Object e;
		      XSETEXTENT (e, next);
		      /* The mapping function caused the following extent to
			 be detached (possibly by deleting some text.)  It
			 also asked for us to continue mapping, but we can't
			 do that, because now we've lost track of the extent
			 chain.

			 Possibly we could keep going by keeping a count of
			 how many extents we mapped over which did *not* end
			 up getting detached, and then regenerating `next'
			 by going back to the beginning and counting from
			 there; but obviously that's not foolproof either, if
			 the fn causes some earlier extent to become detached.
		       */
		      signal_simple_error (
	                "next extent now detached, map-extents can't continue",
					   e);
		    }

		  save_to += (BUF_Z (buf) - BUF_BEG(buf));
		  if (save_to < BUF_BEG (buf))  save_to = BUF_BEG (buf);
		  else if (save_to > BUF_Z (buf))  save_to = BUF_Z (buf);
		  to = save_to;
		  if (from > BUF_Z (buf))  from = BUF_Z (buf);

		  /* Reconstruct start & end, just as at the beginning: */
		  start = buffer_pos_to_extent_index (from, buf, 0);
		  end = buffer_pos_to_map_limit (to, buf, closed_end);

		  /* Adjust by 1/2 upward for open start and closed end. */
		  start = REGION_START_VAL(start, 0);
		  end -= closed_end;	/* change buffer_pos_to_map_limit() 
					   offset to 1/2 */
		  end = REGION_END_VAL(end, 1-closed_end);
		}
	    }
	  else 
	    return;
	}
    }
  return;
}


void
map_extents (int from, int to,
	     elisp_emf efn, emf fn,
	     void *arg, struct buffer *buf, 
	     int closed_end)
{
  map_extents_after (from, to, efn, fn, arg, buf, 0, closed_end, 0);
}

/* Returns true iff map_extents() would visit the given extent. */
int extent_in_region_p (EXTENT extent, int from, int to, int closed_end)
{
  struct buffer *buf = XBUFFER (extent_buffer (extent));
  int start;
  int end;

  if (!BUFFERP (extent_buffer (extent))
      || NILP (XBUFFER (extent_buffer (extent))->name))
    return 0;

  /* We silently return if mapping out of the buffer valid positions */
  if ((from < BUF_BEG (buf)) || (from > BUF_Z (buf))
      || (to < BUF_BEG (buf)) || (to > BUF_Z (buf)))
    return 0;

  if (EXTENT_DETACHED_P (extent))
    return 0;

  start = buffer_pos_to_extent_index (from, buf, 0);
  if (closed_end)
    {
      if (to == BUF_Z(buf))
        end = buffer_pos_to_extent_index (to, buf, 0) + 1;
      else
        end = buffer_pos_to_extent_index (to + 1, buf, 0);
    }
  else
    end = buffer_pos_to_extent_index (to, buf, 0);

  if (extent_end (extent) < start)
    return 0;
  else if (extent_start (extent) < end)
    return 1;
  else
    return 0;
}

static int
slow_map_extents_function (EXTENT extent, void *arg)
{
  struct slow_map_extents_arg *closure = (struct slow_map_extents_arg *) arg;
  Lisp_Object extent_obj;
  XSETEXTENT (extent_obj, extent);
  closure->result = call2 (closure->map_routine, extent_obj, closure->map_arg);
  if (NILP (closure->result))
    return 0;
  else
    return 1;
}

static void
extent_not_in_buffer_error (Lisp_Object extent)
{
  signal_simple_error (GETTEXT ("extent doesn't belong to a buffer"), extent);
}

DEFUN ("map-extents", Fmap_extents, Smap_extents, 1, 6, 0,
       "Map FUNCTION over the extents which overlap region in BUFFER,\n\
starting at FROM and ending at TO.  FUNCTION is called with the arguments\n\
 (extent, MAPARG).\n\
The arguments FROM, TO, MAPARG, and BUFFER default to the beginning of\n\
 BUFFER, the end of BUFFER, nil, and (current-buffer), respectively.\n\
MAP-EXTENTS returns the first non-null result produced by FUNCTION,\n\
and no more calls to FUNCTION are made after it returns non-null.\n\
If BUFFER is an extent, FROM and TO default to the extent's endpoints,\n\
and the mapping omits that extent and its predecessors.\n\
This feature supports restarting a loop based on `map-extents'.")
  (function, buffer, from, to, maparg, closed_end)
  Lisp_Object function, buffer, from, to, maparg, closed_end;
   /* >>> closed_end undocumented */
{
  struct slow_map_extents_arg closure;
  int closed;
  int start, end;
  struct gcpro gcpro1, gcpro2, gcpro3;
  EXTENT after = 0;

  closed = !NILP (closed_end);

  if (NILP (buffer))
    XSETR (buffer, Lisp_Buffer, current_buffer);
  else if (EXTENTP (buffer))
    {
      after = XEXTENT (buffer);
      if (NILP (from)) from = Fextent_start_position (buffer);
      if (NILP (to)) to = Fextent_end_position (buffer);
      buffer = extent_buffer (after);
      if (!BUFFERP (buffer)
	  || NILP (XBUFFER (extent_buffer (after))->name)
	  || EXTENT_DETACHED_P (after))
	signal_simple_error (GETTEXT ("extent is not attached to a buffer"), buffer);
    }
  else
    CHECK_BUFFER (buffer, 0);

  if (NILP (from))
    start = BUF_BEG (XBUFFER (buffer));
  else
    {
      CHECK_FIXNUM_COERCE_MARKER (from, 1);
      start = XINT (from);
    }
  if (NILP (to))
    end = BUF_Z (XBUFFER (buffer));
  else
    {
      CHECK_FIXNUM_COERCE_MARKER (to, 2);
      end = XINT (to);
    }
  check_from_to (start, end, (XBUFFER (buffer)), 1);

  GCPRO3 (function, maparg, buffer);

  closure.map_arg = maparg;
  closure.map_routine = function;
  closure.result = Qnil;
  map_extents_after (start, end, 0,
		     slow_map_extents_function, (void *) &closure, 
		     XBUFFER (buffer), after, closed, 1);

  UNGCPRO;
  return closure.result;
}

DEFUN ("extent-in-region-p", Fextent_in_region_p, Sextent_in_region_p, 1, 4, 0,
       "Whether map-extents would visit EXTENT when called with these args.")
     (extent_obj, from, to, closed_end)
     Lisp_Object extent_obj, from, to, closed_end;
{
  int closed;
  EXTENT extent;
  struct buffer *buf;

  CHECK_EXTENT (extent_obj, 0);
  extent = XEXTENT (extent_obj);

  if (!BUFFERP (extent_buffer (extent))
      || NILP (XBUFFER (extent_buffer (extent))->name))
    extent_not_in_buffer_error (extent_obj);

  buf = XBUFFER (extent_buffer (extent));
  
  if (!NILP (closed_end))
    closed = 1;
  else 
    closed = 0;

  if (NILP (from)) XSET (from, Lisp_Int, BUF_BEG (buf));
  if (NILP (to)) XSET (to, Lisp_Int, BUF_Z (buf));
  CHECK_FIXNUM_COERCE_MARKER (from, 1);
  CHECK_FIXNUM_COERCE_MARKER (to, 2);
  check_from_to (XINT (from), XINT (to), (buf), 0);

  if (extent_in_region_p(extent, XINT(from), XINT(to), closed))
    return Qt;
  return Qnil;
}

static int
slow_map_extent_children_function (EXTENT extent, void *arg)
{
  struct slow_map_extent_children_arg *closure =
    (struct slow_map_extent_children_arg *) arg;
  Lisp_Object extent_obj;
  int start = extent_endpoint (extent, 0);
  int end = extent_endpoint (extent, 1);
  /* Make sure the extent starts inside the region of interest,
     rather than just overlaps it.
     */
  if (start < closure->start_min)
    return 0;
  /* Make sure the extent is not a child of a previous visited one.
     We know already, because of extent ordering,
     that start >= prev_start, and that if
     start == prev_start, then end <= prev_end.
     */
  if (start == closure->prev_start)
    {
      if (end < closure->prev_end)
	return 0;
    }
  else /* start > prev_start */
    {
      if (start < closure->prev_end)
	return 0;
      /* corner case:  prev_end can be -1 if there is no prev */
    }
  XSETEXTENT (extent_obj, extent);
  closure->result = call2 (closure->map_routine, extent_obj,
			   closure->map_arg);

  /* Since the callback may change the buffer, compute all stored
     buffer positions here.
     */
  closure->start_min = -1;	/* no need for this any more */
  closure->prev_start = extent_endpoint (extent, 0);
  closure->prev_end = extent_endpoint (extent, 1);

  if (NILP (closure->result))
    return 0;
  else
    return 1;
}

DEFUN ("map-extent-children", Fmap_extent_children, Smap_extent_children,
       1, 6, 0,
       "Map FUNCTION over the extents in the region from FROM to TO.\n\
FUNCTION is called with arguments (extent, MAPARG).\n\
The arguments are the same as for `map-extents', but this function differs\n\
in that it only visits extents which start in the given region, and also\n\
in that, after visiting an extent E, it skips all other extents which start\n\
inside E but end before E's end.\n\
\n\
Thus, this function may be used to walk a tree of extents in a buffer:\n\
	(defun walk-extents (buffer &optional ignore)\n\
	 (map-extent-children 'walk-extents buffer))")
     (function, buffer, from, to, maparg, closed_end)
     Lisp_Object function, buffer, from, to, maparg, closed_end;
{
  struct slow_map_extent_children_arg closure;
  int closed;
  int start, end;
  struct gcpro gcpro1, gcpro2, gcpro3;
  EXTENT after = 0;

  closed = !NILP (closed_end);

  if (NILP (buffer))
    XSETR (buffer, Lisp_Buffer, current_buffer);
  else if (EXTENTP (buffer))
    {
      after = XEXTENT (buffer);
      if (NILP (from)) from = Fextent_start_position (buffer);
      if (NILP (to)) to = Fextent_end_position (buffer);
      buffer = extent_buffer (after);
      if (!BUFFERP (buffer)
	  || NILP (XBUFFER (extent_buffer (after))->name)
	  || EXTENT_DETACHED_P (after))
	signal_simple_error (GETTEXT ("extent is not attached to a buffer"),
			     buffer);
    }
  else
    CHECK_BUFFER (buffer, 0);

  if (NILP (from))
        start = BUF_BEG (XBUFFER (buffer));
  else
    {
      CHECK_FIXNUM_COERCE_MARKER (from, 1);
      start = XINT (from);
    }
  if (NILP (to))
    end = BUF_Z (XBUFFER (buffer));
  else
    {
      CHECK_FIXNUM_COERCE_MARKER (to, 2);
      end = XINT (to);
    }
  check_from_to (start, end, (XBUFFER (buffer)), 1);

  GCPRO3 (function, maparg, buffer);

  closure.map_arg = maparg;
  closure.map_routine = function;
  closure.result = Qnil;
  closure.start_min = XINT (from);
  closure.prev_start = -1;
  closure.prev_end = -1;
  map_extents_after (start, end, 0,
		     slow_map_extent_children_function, (void *) &closure, 
		     XBUFFER (buffer), after, closed, 1);

  UNGCPRO;
  return closure.result;
}


/* The display code looks into the Vlast_highlighted_extent variable to 
   correctly display highlighted extents.  This updates that variable,
   and marks the appropriate buffers as needing some redisplay.
 */
static void
do_highlight (Lisp_Object extent_obj, int highlight_p)
{
  if (( highlight_p && (EQ (Vlast_highlighted_extent, extent_obj))) ||
      (!highlight_p && (EQ (Vlast_highlighted_extent, Qnil))))
    return;
  else
    {
      Lisp_Object old_parent = 
        (EXTENTP (Vlast_highlighted_extent)
	 ? extent_buffer (XEXTENT(Vlast_highlighted_extent))
	 : Qnil);
      Lisp_Object new_parent = 
        (EXTENTP (extent_obj)
	 ? extent_buffer (XEXTENT (extent_obj))
	 : Qnil);

      if (BUFFERP (old_parent))
        {
          Vlast_highlighted_extent = Qnil;
          BUF_FACECHANGE (XBUFFER (old_parent))++;
	  extents_changed++;
        }

      if (BUFFERP (new_parent) && highlight_p)
        {
          Vlast_highlighted_extent = extent_obj;
          BUF_FACECHANGE (XBUFFER (new_parent))++;
	  extents_changed++;
        }
      else
        Vlast_highlighted_extent = Qnil;
    }
}

DEFUN ("force-highlight-extent", Fforce_highlight_extent, 
       Sforce_highlight_extent, 1, 2, 0,
 "Highlight or unhighlight the given extent.\n\
If the second arg is non-nil, it will be highlighted, else dehighlighted.\n\
This is the same as `highlight-extent', except that it will work even\n\
on extents without the 'highlight property.")
     (extent_obj, highlight_p)
     Lisp_Object extent_obj, highlight_p;
{
  if (NILP (extent_obj))
    highlight_p = Qnil;
  else
    CHECK_EXTENT (extent_obj, 0);
  do_highlight (extent_obj, !NILP (highlight_p));
  return Qnil;
}

DEFUN ("highlight-extent", Fhighlight_extent, Shighlight_extent, 1, 2, 0,
 "Highlight the given extent, if it is highlightable\n(\
that is, if it has the 'highlight property).\n\
If the second arg is non-nil, it will be highlighted, else dehighlighted.\n\
Highlighted extents are displayed as if they were merged with the 'highlight\n\
face.")
     (extent_obj, highlight_p)
     Lisp_Object extent_obj, highlight_p;
{
  if (EXTENTP (extent_obj) && !EXTENT_HIGHLIGHT_P (XEXTENT (extent_obj)))
    return Qnil;
  else
    return (Fforce_highlight_extent (extent_obj, highlight_p));
}


/* Extent accessors */

GLYPH
extent_glyph_at (EXTENT extent, int pos, int endp)
{
  if (! extent)
    return 0;
  else if (! BUFFERP (extent_buffer (extent)))
    return 0;
  else if (endp
	   && EXTENT_END_GLYPH_P (extent)
	   && pos == (extent_endpoint (extent, 1) - 1))
    return extent_end_glyph (extent);
  else if (!endp
	   && EXTENT_BEGIN_GLYPH_P (extent)
	   && pos == extent_endpoint (extent, 0))
    return extent_begin_glyph (extent);
  else
    return 0;
}

static Lisp_Object
extent_endpoint_position (Lisp_Object extent_obj, int endp)
{
  EXTENT extent;
  int pos;
  CHECK_EXTENT (extent_obj, 0);
  extent = XEXTENT(extent_obj);
  pos = extent_endpoint (extent, endp);
  if (pos <= 0)
    {
      if (EXTENT_DESTROYED_P (extent))
	error (GETTEXT ("EXTENT arg doesn't belong to a buffer"));
      else if (EXTENT_DETACHED_P (extent))
	return Qnil;
    }
  return make_number (pos);
}


DEFUN ("extent-start-position", Fextent_start_position, 
       Sextent_start_position, 1, 1, 0,
       "Return start position of EXTENT.")
     (extent_obj)
     Lisp_Object extent_obj;
{
  return extent_endpoint_position (extent_obj, 0);
}


DEFUN ("extent-end-position", Fextent_end_position, 
       Sextent_end_position, 1, 1, 0,
       "Return first position after EXTENT.")
     (extent_obj)
     Lisp_Object extent_obj;
{
  return extent_endpoint_position (extent_obj, 1);
}

DEFUN ("extent-length", Fextent_length, Sextent_length, 1, 1, 0,
       "Return length of EXTENT in characters.")
     (extent_obj)
     Lisp_Object extent_obj;
{
  CHECK_EXTENT (extent_obj, 0);
  return make_number (extent_endpoint (XEXTENT(extent_obj), 1) - 
                      extent_endpoint (XEXTENT(extent_obj), 0));
}

DEFUN ("extent-buffer", Fextent_buffer, Sextent_buffer, 1, 1, 0,
       "Return buffer of EXTENT.")
     (extent_obj)
     Lisp_Object extent_obj;
{
  CHECK_EXTENT (extent_obj, 0);
  return (extent_buffer (XEXTENT (extent_obj)));
}

DEFUN ("extentp", Fextentp, Sextentp, 1, 1, 0,
  "T if OBJECT is an extent..")
  (extent)
     Lisp_Object extent;
{
  if (EXTENTP (extent))
    return Qt;
  return Qnil;
}


/* This is the only place `PT' is an lvalue in all of emacs. */

static void
set_point_internal (int charno)
{
  current_buffer->text.pt = charno;
  if (charno > GPT)
    charno += GAP_SIZE;
  if (!MARKERP (current_buffer->point_marker))
    abort ();
  XMARKER (current_buffer->point_marker)->bufpos = charno;
}

void
set_point (position)
     int position;
{
  int opoint = PT;

  if (position == opoint)
    return;
  else if (position > Z)
    abort();

  if (position > ZV || position < BEGV)
    abort ();

  set_point_internal (position);

  if (EQ (current_buffer->extents, Qzero))
    abort();

  /* If there were to be hooks which were run when point entered/left an
     extent, this would be the place to put them.

     However, it's probably the case that such hooks should be implemented
     using a post-command-hook instead, to avoid running the hooks as a
     result of intermediate motion inside of save-excursions, for example.
   */
}

void
set_buffer_point (buffer, position)
     struct buffer *buffer;
     int position;
{
  if (buffer == current_buffer)
    set_point (position);
  else
    {
      int speccount = specpdl_depth ();
      record_unwind_protect (Fset_buffer, Fcurrent_buffer ());
      set_buffer_internal (buffer);
      set_point (position);
      unbind_to (speccount, Qnil);
    }
}

/* Don't have invisible extents at the moment */
int
last_visible_position (int opoint, struct buffer *buf)
{
  return opoint;
}


/* find "smallest" matching extent containing pos -- (flag == 0) means 
   all extents match, else (EXTENT_FLAGS (extent) & flag) must be true;
   for more than one matching extent with precisely the same endpoints,
   we choose the last extent in the extents_list.
   The search stops just before "before", if that is non-null.
   */

static int extent_at_mapper (EXTENT e, void *arg);

static EXTENT
extent_at_before (int pos, struct buffer *buf, Lisp_Object prop, EXTENT before)
{
  if (NILP (buf->extents))
    return 0;
  else if (!EXTENTP (buf->extents))
    abort();
  else
    {
      struct extent_at_arg closure;
      closure.best_match = 0;
      closure.prop = prop;
      closure.before = before;
      
      map_extents (pos, pos, 0, extent_at_mapper, (void *) &closure, buf, 1);
      return closure.best_match;
    }
}

EXTENT
extent_at (int pos, struct buffer *buf, Lisp_Object prop)
{
  return extent_at_before (pos, buf, prop, 0);
}

static int
extent_at_mapper (EXTENT e, void *arg)
{
  struct extent_at_arg *closure = (struct extent_at_arg *) arg;

  if (e == closure->before)
    return 1;

  /* If closure->prop is non-nil, then the extent is only acceptable
     if it has a non-nil value for that property. */
  if (!NILP (closure->prop))
    {
      Lisp_Object extent;
      XSETEXTENT (extent, e);
      if (NILP (Fextent_property (extent, closure->prop)))
	return 0;
    }

    {
      EXTENT current = closure->best_match;

      if (!current)
	goto accept;
      /* redundant but quick test */
      else if (extent_start (current) > extent_start (e))
	return 0;

      /* we return the "last" best fit, instead of the first --
	 this is because then the glyph closest to two equivalent
	 extents corresponds to the "extent-at" the text just past
	 that same glyph */
      else if (!EXTENT_LESS_VALS (e, closure->best_start,
				  closure->best_end))
        goto accept;
      else
	return 0;
    accept:
      closure->best_match = e;
      closure->best_start = EXTENT_START_VAL (e);
      closure->best_end = EXTENT_END_VAL (e);
    }

  return 0;
}


DEFUN ("make-extent", Fmake_extent, Smake_extent, 2, 3, 0,
       "Make an extent for the range [FROM, TO) in BUFFER.\n\
BUFFER defaults to the current buffer.  Insertions at point TO will be\n\
outside of the extent; insertions at FROM will be inside the extent,\n\
causing the extent to grow. (This is the same way that markers behave.)\n\
The extent is initially detached if both FROM and TO are nil, and in this\n\
case BUFFER defaults to nil, meaning the extent is in no buffer.")
  (from, to, buffer)
   Lisp_Object from, to, buffer;
{
  Lisp_Object extent_obj;
  if (NILP (from) && NILP (to))
    extent_obj = make_extent_detached (buffer);
  else
    {
      if (NILP (buffer))
	XSETR (buffer, Lisp_Buffer, current_buffer);
      CHECK_BUFFER (buffer, 0);
      CHECK_NUMBER_COERCE_MARKER (from, 0);
      CHECK_NUMBER_COERCE_MARKER (to, 1);
      extent_obj = make_extent_internal (XINT (from), XINT (to), buffer);
    }
  return extent_obj;
}

DEFUN ("copy-extent", Fcopy_extent, Scopy_extent, 1, 2, 0,
 "Make a copy of EXTENT.  It is initially detached.\n\
Optional argument BUFFER defaults to EXTENT's buffer.")
  (extent_obj, buffer)
   Lisp_Object extent_obj, buffer;
{
  EXTENT extent;

  CHECK_EXTENT (extent_obj, 0);
  if (NILP (buffer))
    buffer = extent_buffer (XEXTENT (extent_obj));
  else
    CHECK_BUFFER (buffer, 1);

  extent = XEXTENT (extent_obj);

  if (EXTENT_DESTROYED_P (extent))
    error (GETTEXT ("deleted EXTENT cannot be copied"));

  return copy_extent_internal (extent, -1, -1, buffer);
}

DEFUN ("delete-extent", Fdelete_extent, Sdelete_extent, 1, 1, 0,
 "Remove EXTENT from its buffer.\n\
This does not modify the buffer's text, only its display properties.\n\
The extent cannot be used thereafter.")
  (extent_obj)
   Lisp_Object extent_obj;
{
  EXTENT extent;

  CHECK_EXTENT (extent_obj, 0);
  extent = XEXTENT (extent_obj);

  if (EXTENT_DESTROYED_P (extent))
    return Qnil;
  if (BUFFERP (extent_buffer (extent)) &&
      !NILP (XBUFFER (extent_buffer (extent))->name))
    {
      BUF_FACECHANGE (XBUFFER (extent_buffer (extent)))++;
      extents_changed++;
    }
  destroy_extent (extent);
  return Qnil;
}

DEFUN ("detach-extent", Fdetach_extent, Sdetach_extent, 1, 1, 0,
   "Remove EXTENT from its buffer in such a way that it can be re-inserted.\n\
An extent is also detached when all of its characters are all killed by a\n\
deletion.\n\
\n\
Extents which have the `duplicable' attribute are tracked by the undo\n\
mechanism.  Detachment via `detach-extent' and string deletion is recorded,\n\
as is attachment via `insert-extent' and string insertion.  Extent motion,\n\
face changes, and attachment via `make-extent' are not recorded.  This means\n\
that extent changes which are to be undo-able must be performed by character\n\
editing, or by insertion and detachment of duplicable extents.")
  (extent_obj)
   Lisp_Object extent_obj;
{
  EXTENT extent;
  CHECK_EXTENT (extent_obj, 0);
  extent = XEXTENT (extent_obj);
  if (EXTENT_DETACHED_P (extent))
    return extent_obj;
  if (!BUFFERP (extent_buffer (extent)))
    {
      detach_extent (extent);
      return extent_obj;
    }
  if (EXTENT_DUPLICABLE_P (extent))
    record_extent (extent_obj, 0);
  detach_extent (extent);

  BUF_FACECHANGE (XBUFFER (extent_buffer (extent)))++;
  extents_changed++;

  return extent_obj;
}

/* Note:  If you track non-duplicable extents by undo, you'll get bogus
   undo records for transient extents via update-extent.
   For example, query-replace will do this.
 */

DEFUN ("set-extent-endpoints", Fset_extent_endpoints, Sset_extent_endpoints,
       3, 3, 0,
       "Set the endpoints of EXTENT to START, END.\n\
If START and END are null, call detach-extent on EXTENT.\n\
See documentation on `detach-extent' for a discussion of undo recording.")
  (extent_obj, start, end)
   Lisp_Object extent_obj, start, end;
{
  EXTENT extent;
  int from;
  int to;

  if (NILP (start) && NILP (end))
    return Fdetach_extent (extent_obj);

  CHECK_EXTENT (extent_obj, 0);
  CHECK_FIXNUM_COERCE_MARKER (start, 1);
  CHECK_FIXNUM_COERCE_MARKER (end, 2);

  extent = XEXTENT (extent_obj);
  from = XINT (start);
  to = XINT (end);

  if (!BUFFERP (extent_buffer (extent))
      || NILP (XBUFFER (extent_buffer (extent))->name))
    /* I18N3 */
    extent_not_in_buffer_error (extent_obj);

  check_from_to (from, to, XBUFFER (extent_buffer (extent)), 1);

  if (from > to)
    /* I18N3 */
    error (GETTEXT ("Bad START (== %d) and END (== %d) args to UPDATE-EXTENT"),
	   from, to);

  if (EXTENT_DETACHED_P (extent))
    {
      if (EXTENT_DUPLICABLE_P (extent))
	record_extent (extent_obj, 1);
    }
  else /*move it:*/
    detach_extent (extent);
  extent_start (extent) =
    BUFFER_POS_TO_START_INDEX (from, XBUFFER (extent_buffer (extent)), extent);
  extent_end (extent) =
    BUFFER_POS_TO_END_INDEX (to, XBUFFER (extent_buffer (extent)), extent);

  /* #### Kludge!!!  If the extent is (open,open) and the endpoints are
     the same, the above calls to BUFFER_POS_TO_* will give it the bogus
     endpoints (N,N-1).  So, hack it out...
   */
  if (from == to)
    extent_end (extent) = extent_start (extent);

  splice_extent_into_buffer (extent, extent_buffer (extent));

  BUF_FACECHANGE (XBUFFER (extent_buffer (extent)))++;
  extents_changed++;

  return extent_obj;
}


DEFUN ("insert-extent", Finsert_extent, Sinsert_extent, 1, 4, 0,
 "Insert EXTENT from START to END in the current buffer.\n\
This operation does not insert any characters,\n\
but otherwise acts like `insert' of a string whose\n\
string-extent-data calls for EXTENT to be inserted.\n\
Returns the newly-inserted extent.\n\
The fourth arg, NO-HOOKS, can be used to inhibit the running of the\n\
 extent's `paste-function' property if it has one.\n\
See documentation on `detach-extent' for a discussion of undo recording.")
  (extent_obj, start, end, no_hooks)
   Lisp_Object extent_obj, start, end, no_hooks;
{
  EXTENT extent;
  Lisp_Object copy;

  CHECK_EXTENT (extent_obj, 0);
  CHECK_FIXNUM_COERCE_MARKER (start, 1);
  CHECK_FIXNUM_COERCE_MARKER (end, 2);

  extent = XEXTENT (extent_obj);

  if (EXTENT_DESTROYED_P (extent))
    {
      if (inside_undo)
	return extent_obj;
      error (GETTEXT ("deleted EXTENT cannot be inserted"));
    }

  copy = insert_extent (extent, XINT (start), XINT (end), current_buffer,
			NILP (no_hooks));
  if (EXTENTP (copy))
    {
      if (EXTENT_DUPLICABLE_P (XEXTENT (copy)))
	record_extent (copy, 1);
      BUF_FACECHANGE (current_buffer)++;
      extents_changed++;
    }
  return copy;
}



/* Extent properties */

Lisp_Object
extent_getf (EXTENT extent, Lisp_Object property)
{
  Lisp_Object tail = extent->plist;
  if (!SYMBOLP (property)) abort ();

  /* If there are two glyphs, one of them stole the first cons of the plist. */
  if (EXTENT_BEGIN_GLYPH_P (extent) && EXTENT_END_GLYPH_P (extent))
    {
      if (!CONSP (tail)) abort ();
      tail = XCONS (tail)->cdr;
    }

  while (!NILP (tail))
    {
      struct Lisp_Cons *c = XCONS (tail);
      if (!CONSP (tail)) abort ();
      if (!SYMBOLP (c->car)) abort ();
      if (EQ (c->car, property))
	{
	  if (!CONSP (c->cdr)) abort ();
	  return XCONS (c->cdr)->car;
	}
      tail = XCONS (c->cdr)->cdr;
    }
  return Qnil;
}

extern Lisp_Object Fextent_face (Lisp_Object);
extern Lisp_Object Fset_extent_face (Lisp_Object, Lisp_Object);

DEFUN ("extent-property", Fextent_property, Sextent_property, 2, 2, 0,
 "Returns the extent's value of the given property.\n\
See `set-extent-property' for the built-in property names.")
  (extent, property)
   Lisp_Object extent, property;
{
  EXTENT e;
  CHECK_EXTENT (extent, 0);
  CHECK_SYMBOL (property, 0);
  e = XEXTENT (extent);

#define RETURN_FLAG(flag) return (e->flags.flag ? Qt : Qnil)
  if      (EQ (property, Qdetached))	 RETURN_FLAG (detached);
  else if (EQ (property, Qdestroyed))	 RETURN_FLAG (destroyed);
  else if (EQ (property, Qstart_open))	 RETURN_FLAG (start_open);
  else if (EQ (property, Qend_open))	 RETURN_FLAG (end_open);
  else if (EQ (property, Qread_only))	 RETURN_FLAG (read_only);
  else if (EQ (property, Qhighlight))	 RETURN_FLAG (highlight);
  else if (EQ (property, Qunique))	 RETURN_FLAG (unique);
  else if (EQ (property, Qduplicable))	 RETURN_FLAG (duplicable);
  else if (EQ (property, Qinvisible))	 RETURN_FLAG (invisible);
#undef RETURN_FLAG
  /* Support (but don't document...) the obvious antonyms. */
  else if (EQ (property, Qstart_closed))
    return (e->flags.start_open ? Qnil : Qt);
  else if (EQ (property, Qend_closed))
    return (e->flags.end_open ? Qnil : Qt);
  else if (EQ (property, Qpriority))
    return make_number (extent_priority (e));
  else if (EQ (property, Qface))
    return Fextent_face (extent);
  else if (EQ (property, Qglyph_layout))
    switch (e->flags.glyph_layout)
      {
      case GL_TEXT: return Qtext;
      case GL_OUTSIDE_MARGIN: return Qoutside_margin;
      case GL_INSIDE_MARGIN: return Qinside_margin;
      case GL_WHITESPACE: return Qwhitespace;
      default: abort ();
      }
  else if (EQ (property, Qbegin_glyph))
    {
      if (EXTENT_BEGIN_GLYPH_P (e))
	return (glyph_to_pixmap (extent_begin_glyph (XEXTENT (extent))));
      else
	return Qnil;
    }
  else if (EQ (property, Qend_glyph))
    {
      if (EXTENT_END_GLYPH_P (e))
	return (glyph_to_pixmap (extent_end_glyph (XEXTENT (extent))));
      else
	return Qnil;
    }
  else
    return extent_getf (e, property);
}


/* This comment supplies the doc string for set-extent-property,
   for make-docfile to see.  We cannot put this in the real DEFUN
   due to limits in the Unix cpp.

DEFUN ("set-extent-property", Foo, Sfoo, 3, 3, 0,
  "Change a property of an extent.\n\
PROPERTY may be any symbol; the value stored may be accessed with\n\
 the `extent-property' function.\n\
The following symbols have predefined meanings:\n\
\n\
 detached	Removes the extent from its buffer; setting this is the same\n\
		as calling `detach-extent'.\n\
\n\
 destroyed	Removes the extent from its buffer, and makes it unusable in\n\
		the future; this is the same calling `delete-extent'.\n\
\n\
 priority	Change redisplay priority; same as `set-extent-priority'.\n\
\n\
 start-open	Whether the set of characters within the extent is treated\n\
		being open on the left, that is, whether the start position\n\
		is an exclusive, rather than inclusive, boundary.  If true,\n\
		then characters inserted exactly at the beginning of the\n\
		extent will remain outside of the extent; otherwise they\n\
		will go into the extent, extending it.\n\
\n\
 end-open	Whether the set of characters within the extent is treated\n\
		being open on the right, that is, whether the end position\n\
		is an exclusive, rather than inclusive, boundary.  If true,\n\
		then characters inserted exactly at the end of the extent\n\
		will remain outside of the extent; otherwise they will go\n\
		into the extent, extending it.\n\
\n\
 read-only	Text within this extent will be unmodifiable.\n\
\n\
 face		The face in which to display the text.  Setting this is the\n\
		same as calling `set-extent-face'.\n\
\n\
 highlight	Highlight the extent when the mouse moves over it.\n\
\n\
 duplicable	Whether this extent should be copied into strings, so that\n\
		kill, yank, and undo commands will restore or copy it.\n\
\n\
 unique		Meaningful only in conjunction with `duplicable'.  When this\n\
		is set, there may be only one instance of this extent\n\
		attached at a time: if it is copied to the kill ring and\n\
		then yanked, the extent is not copied.  If, however, it is\n\
		killed (removed from the buffer) and then yanked, it will\n\
		be re-attached at the new position.\n\
\n\
 invisible	Text under this extent is elided (not yet implemented).\n\
\n\
 keymap		This keymap is consulted for mouse clicks on this extent, or\n\
		keypresses made while point is within the extent.\n\
\n\
 copy-function	This is a hook that is run when a duplicable extent is about\n\
		to be copied from a buffer to a string (or the kill ring.)\n\
		It is called with three arguments, the extent, and the\n\
		buffer-positions within it which are being copied.  If this\n\
		function returns nil, then the extent will not be copied;\n\
		otherwise it will.\n\
\n\
 paste-function This is a hook that is run when a duplicable extent is\n\
		about to be copied from a string (or the kill ring) into a\n\
		buffer.  It is called with three arguments, the original\n\
		extent, and the buffer positions which the copied extent\n\
		will occupy.  (This hook is run after the corresponding text\n\
		has already been inserted into the buffer.)  Note that the\n\
		extent argument may be detached when this function is run.\n\
		If this function returns nil, no extent will be inserted.\n\
		Otherwise, there will be an extent covering the range in\n\
		question.\n\
\n\
		If the original extent is not attached to a buffer, then it\n\
		will be re-attached at this range.  Otherwise, a copy will\n\
		be made, and that copy attached here.\n\
\n\
		The copy-function and paste-function are meaningful only for\n\
		extents with the `duplicable' flag set, and if they are not\n\
		specified, behave as if `t' was the returned value.  When\n\
		these hooks are invoked, the current buffer is the buffer\n\
		which the extent is being copied from/to, respectively.")
     (extent, property, value)
*/

DEFUN ("set-extent-property", Fset_extent_property, Sset_extent_property,
       3, 3, 0, 0
       /* See very large comment above */)
     (extent, property, value)
     Lisp_Object extent, property, value;
{
  int impacts_redisplay_p = 1;
  EXTENT e;
  CHECK_EXTENT (extent, 0);
  CHECK_SYMBOL (property, 0);
  e = XEXTENT (extent);
  
  if (EXTENT_DESTROYED_P (e)
      || (BUFFERP (extent_buffer (e))
	  && NILP (XBUFFER (extent_buffer (e))->name)))
    /* I18N3 */
    error (GETTEXT ("extent is destroyed"));

  if      (EQ (property, Qread_only))	 e->flags.read_only = !NILP (value);
  else if (EQ (property, Qhighlight))	 e->flags.highlight = !NILP (value);
  else if (EQ (property, Qunique))	 e->flags.unique = !NILP (value);
  else if (EQ (property, Qduplicable))	 e->flags.duplicable = !NILP (value);
  else if (EQ (property, Qinvisible))	 e->flags.invisible = !NILP (value);

  else if (EQ (property, Qdetached))
    {
      if (NILP (value)) error (GETTEXT ("can only set `detached' to t"));
      Fdetach_extent (extent);
    }
  else if (EQ (property, Qdestroyed))
    {
      if (NILP (value)) error (GETTEXT ("can only set `destroyed' to t"));
      Fdelete_extent (extent);
    }
  else if (EQ (property, Qpriority))
    {
      Fset_extent_priority (extent, value);
    }
  else if (EQ (property, Qface))
    {
      Fset_extent_face (extent, value);
    }
  else if (EQ (property, Qglyph_layout))
    {
      CHECK_SYMBOL (value, 0);
      if (EQ (value, Qtext))
	e->flags.glyph_layout = GL_TEXT;
      else if (EQ (value, Qoutside_margin))
	e->flags.glyph_layout = GL_OUTSIDE_MARGIN;
      else if (EQ (value, Qinside_margin))
	e->flags.glyph_layout = GL_INSIDE_MARGIN;
      else if (EQ (value, Qwhitespace))
	e->flags.glyph_layout = GL_WHITESPACE;
      else
	error (GETTEXT ("glyph-layout must be text, outside-margin, inside-margin, or whitespace"));
    }

  else if (EQ (property, Qbegin_glyph))
    Fset_extent_begin_glyph (extent, value, Qnil);

  else if (EQ (property, Qend_glyph))
    Fset_extent_end_glyph (extent, value, Qnil);

  else if (EQ (property, Qstart_open) ||
	   EQ (property, Qend_open) ||
	   EQ (property, Qstart_closed) ||
	   EQ (property, Qend_closed))
    {
      if (EQ (property, Qstart_open))
	e->flags.start_open = !NILP (value);
      else if (EQ (property, Qend_open))
	e->flags.end_open = !NILP (value);
      /* Support (but don't document...) the obvious antonyms. */
      else if (EQ (property, Qstart_closed))
	e->flags.start_open = NILP (value);
      else
	e->flags.end_open = NILP (value);

      /* These flags can affect the ordering of extents. */
      if (!EXTENT_DETACHED_P (e))
	{
	  struct buffer *buf = XBUFFER (extent_buffer (e));
	  int from = extent_endpoint (e, 0);
	  int to = extent_endpoint (e, 1);
	  detach_extent (e);
	  extent_start (e) = BUFFER_POS_TO_START_INDEX (from, buf, e);
	  extent_end (e) = BUFFER_POS_TO_END_INDEX (to, buf, e);

	  /* #### Kludge!!!  If the extent is (open,open) and the endpoints are
	     the same, the above calls to BUFFER_POS_TO_* will give it the
	     bogus endpoints (N,N-1).  So, hack it out...
	   */
	  if (from == to)
	    extent_end (e) = extent_start (e);

	  splice_extent_into_buffer (e, extent_buffer (e));
	}
    }
  else
    {
      /* else it's a user-defined prop, and there's no need to tick
	 BUF_FACECHANGE. */
      Lisp_Object tail;

      impacts_redisplay_p = 0;

#ifdef ENERGIZE
      if (EQ (property, Qenergize))
	error (GETTEXT ("Thou shalt not change the `energize' extent property"));
#endif

      if (EQ (property, Qkeymap))
	while (NILP (Fkeymapp (value)))
	  value = wrong_type_argument (Qkeymapp, value);

      for (tail = e->plist; !NILP (tail); tail = Fcdr (Fcdr (tail)))
	{
	  if (EQ (XCONS (tail)->car, property))
	    {
	      XCONS (XCONS (tail)->cdr)->car = value;
	      break;
	    }
	}
      if (NILP (tail) && !NILP (value))
	{
	  /* If there are two glyphs, one of them stole the first cons of
	     the plist.  Add this item to the cdr. */
	  if (EXTENT_BEGIN_GLYPH_P (e) && EXTENT_END_GLYPH_P (e))
	    {
	      if (!CONSP (e->plist)) abort ();
	      XCONS (e->plist)->cdr =
		Fcons (property, Fcons (value, XCONS (e->plist)->cdr));
	    }
	  else
	    e->plist = Fcons (property, Fcons (value, e->plist));
	}
    }

  /* We've set an internally-used slot, so inform redisplay of this. */
  if (impacts_redisplay_p && BUFFERP (extent_buffer (e)))
    {
      BUF_FACECHANGE (XBUFFER (extent_buffer (e)))++;
      extents_changed++;
    }

  return value;
}


DEFUN ("extent-properties", Fextent_properties, Sextent_properties, 1, 1, 0,
 "Return a property list of the attributes of the given extent.\n\
Do not modify this list; use `set-extent-property' instead.")
  (extent)
   Lisp_Object extent;
{
  EXTENT e;
  Lisp_Object result, face;
  CHECK_EXTENT (extent, 0);
  e = XEXTENT (extent);

  result = e->plist;

  /* If there are two glyphs, one of them stole the first cons of the plist. */
  if (EXTENT_BEGIN_GLYPH_P (e) && EXTENT_END_GLYPH_P (e))
    {
      if (!CONSP (result)) abort ();
      result = XCONS (result)->cdr;
    }

  face = Fextent_face (extent);
  if (!NILP (face))
    result = Fcons (Qface, Fcons (face, result));

  if (e->flags.glyph_layout == GL_TEXT)
    ;
  else if (e->flags.glyph_layout == GL_OUTSIDE_MARGIN)
    result = Fcons (Qglyph_layout, Fcons (Qoutside_margin, result));
  else if (e->flags.glyph_layout == GL_INSIDE_MARGIN)
    result = Fcons (Qglyph_layout, Fcons (Qinside_margin, result));
  else if (e->flags.glyph_layout == GL_WHITESPACE)
    result = Fcons (Qglyph_layout, Fcons (Qwhitespace, result));
  else
    abort ();

  if (EXTENT_END_GLYPH_P (e))
    result = Fcons (Qend_glyph,
		    Fcons (glyph_to_pixmap (extent_end_glyph (e)),
			   result));
  if (EXTENT_BEGIN_GLYPH_P (e))
    result = Fcons (Qbegin_glyph,
		    Fcons (glyph_to_pixmap (extent_begin_glyph (e)),
			   result));

  if (extent_priority (e) != 0)
    result = Fcons (Qpriority, Fcons (make_number (extent_priority (e)),
				      result));

#define CONS_FLAG(flag, sym) \
	if (e->flags.flag) result = Fcons (sym, Fcons (Qt, result))
  CONS_FLAG (end_open, Qend_open);
  CONS_FLAG (start_open, Qstart_open);
  CONS_FLAG (invisible, Qinvisible);
  CONS_FLAG (duplicable, Qduplicable);
  CONS_FLAG (unique, Qunique);
  CONS_FLAG (highlight, Qhighlight);
  CONS_FLAG (read_only, Qread_only);
  CONS_FLAG (detached, Qdetached);
  CONS_FLAG (destroyed, Qdestroyed);
#undef CONS_FLAG

  return result;
}

extern GLYPH pixmap_to_glyph (Lisp_Object);

void
set_extent_glyph (EXTENT extent, GLYPH glyph, int endp, unsigned int layout)
{
  int change_p;

  if (glyph == 0)
    {
      /* We are (maybe) removing a glyph. */
      if (!endp)
	{
	  /* We are (maybe) removing a begin glyph.
	     If there were two glyphs, move the end glyph forward. */
	  if (EXTENT_BEGIN_GLYPH_P (extent) && EXTENT_END_GLYPH_P (extent))
	    {
	      extent->flags.glyph = extent_end_glyph (extent);
	      extent->plist = XCONS (extent->plist)->cdr;
	      change_p = 1;
	    }
	  else if (EXTENT_BEGIN_GLYPH_P (extent))
	    {
	      /* Otherwise the begin glyph is in the glyph slot. */
	      extent->flags.glyph = (GLYPH) -1;
	      change_p = 1;
	    }
	  EXTENT_BEGIN_GLYPH_P (extent) = 0;
	}
      else
	{
	  /* We are (maybe) removing an end glyph.
	     If there were two glyphs, the end was in `plist'.
	     Otherwise, it was in the `glyph' slot (if at all.)
	   */
	  if (EXTENT_BEGIN_GLYPH_P (extent) && EXTENT_END_GLYPH_P (extent))
	    {
	      if (!CONSP (extent->plist)) abort ();
	      extent->plist = XCONS (extent->plist)->cdr;
	      change_p = 1;
	    }
	  else if (EXTENT_END_GLYPH_P (extent))
	    {
	      extent->flags.glyph = (GLYPH) 0;
	      change_p = 1;
	    }
	  EXTENT_END_GLYPH_P (extent) = 0;
	}
    }
  else
    {
      extent->flags.glyph_layout = layout;

      if (EXTENT_BEGIN_GLYPH_P (extent) && EXTENT_END_GLYPH_P (extent))
	{
	  /* Both glyphs already exist, and we're replacing one of them.
	     The glyph may be in the `glyph' slot or on the front of `plist'.
	   */
	  if (!CONSP (extent->plist)) abort ();
	  change_p = (glyph != (endp
				? extent_end_glyph (extent)
				: extent_begin_glyph (extent)));
	  if (endp)
	    extent->flags.glyph = glyph;
	  else
	    XCONS (extent->plist)->car = make_number (glyph);
	}
      else if ((EXTENT_BEGIN_GLYPH_P (extent) || EXTENT_END_GLYPH_P (extent))
	       && (endp
		   ? EXTENT_END_GLYPH_P (extent)
		   : EXTENT_BEGIN_GLYPH_P (extent)))
	{
	  /* Only one glyph already exists, and we're replacing it.
	     When there is only one glyph, it's in the `glyph' slot.
	   */
	  change_p = (extent->flags.glyph != glyph);
	  extent->flags.glyph = glyph;
	}
      else if (EXTENT_BEGIN_GLYPH_P (extent) || EXTENT_END_GLYPH_P (extent))
	{
	  /* Only one glyph already exists, and we're adding the other.
	     So we need to end up with the correct glyphs in the `glyph'
	     and `plist' slots.
	   */
	  if (!endp)
	    {
	      /* adding start - move end from `glyph' to `plist',
		 and put the new start in `glyph'. */
	      extent->plist = Fcons (make_number (extent->flags.glyph),
				     extent->plist);
	      extent->flags.glyph = glyph;
	      EXTENT_BEGIN_GLYPH_P (extent) = 1;
	      change_p = 1;
	    }
	  else
	    {
	      /* adding end - put it in `plist' and leave start alone. */
	      extent->plist = Fcons (make_number (glyph), extent->plist);
	      EXTENT_END_GLYPH_P (extent) = 1;
	      change_p = 1;
	    }
	}
      else
	{
	  /* Neither glyph exists, and we're adding one.
	     When there is only one glyph, it goes into the `glyph' slot.
	   */
	  extent->flags.glyph = glyph;
	  if (endp)
	    EXTENT_END_GLYPH_P (extent) = 1;
	  else
	    EXTENT_BEGIN_GLYPH_P (extent) = 1;
	  change_p = 1;
	}
    }

  if (change_p)
    {
      BUF_FACECHANGE (XBUFFER (extent_buffer (extent)))++;
      extents_changed++;
    }
}

static Lisp_Object
set_extent_glyph_1 (Lisp_Object extent_obj, Lisp_Object glyph_obj, int endp,
		    Lisp_Object layout_obj)
{
  EXTENT extent;
  GLYPH glyph;
  unsigned int layout;
  CHECK_EXTENT (extent_obj, 0);
  extent = XEXTENT (extent_obj);

  if (!BUFFERP (extent_buffer (extent))
      || NILP (XBUFFER (extent_buffer (extent))->name))
    extent_not_in_buffer_error (extent_obj);

  if (NILP (layout_obj))
    layout = GL_TEXT;
  else
    {
      CHECK_SYMBOL (layout_obj, 0);
      if (EQ (Qoutside_margin, layout_obj))
	layout = GL_OUTSIDE_MARGIN;
      else if (EQ (Qinside_margin, layout_obj))
	layout = GL_INSIDE_MARGIN;
      else if (EQ (Qwhitespace, layout_obj))
	layout = GL_WHITESPACE;
      else if (EQ (Qtext, layout_obj))
	layout = GL_TEXT;
      else
	signal_simple_error ("unknown layout type", layout_obj);
    }

  if (NILP (glyph_obj))
    glyph = 0;
  else if (PIXMAPP (glyph_obj) ||
	   STRINGP (glyph_obj) ||
	   SUBWINDOWP (glyph_obj))
    glyph = pixmap_to_glyph (glyph_obj);
  else
    error ("Glyph must be a bitmap or string object.");

  set_extent_glyph (extent, glyph, endp, layout);
  return glyph_obj;
}

DEFUN ("set-extent-begin-glyph", Fset_extent_begin_glyph, 
       Sset_extent_begin_glyph, 2, 3, 0,
 "Display a bitmap, subwindow or string at the beginning of the given extent.\n\
The begin-glyph should either be a pixmap object, subwindow object or the\n\
string to display.  The layout policy defaults to `text'.")
  (extent, begin_glyph, layout)
   Lisp_Object extent, begin_glyph, layout;
{
  return set_extent_glyph_1 (extent, begin_glyph, 0, layout);
}


DEFUN ("set-extent-end-glyph", Fset_extent_end_glyph, 
       Sset_extent_end_glyph, 2, 3, 0,
 "Display a bitmap, subwindow or string at the end of the given extent.\n\
The end-glyph should either be a pixmap object, subwindow object or the\n\
string to display.  The layout policy defaults to `text'.")
  (extent, end_glyph, layout)
   Lisp_Object extent, end_glyph, layout;
{
  return set_extent_glyph_1 (extent, end_glyph, 1, layout);
}

static Lisp_Object
Fextent_glyph_1 (Lisp_Object extent_obj, int begin_p)
{
  struct extent *extent;
  Lisp_Object obj;
  GLYPH g;

  CHECK_EXTENT (extent_obj, 0);
  extent = XEXTENT (extent_obj);
  g = (begin_p
       ? extent_begin_glyph (extent)
       : extent_end_glyph (extent));
  if (!g) return Qnil;
  obj = glyph_to_pixmap (g);
  if (!PIXMAPP (obj) && !STRINGP (obj) && !SUBWINDOWP (obj))
    abort ();
  return (obj);
}

DEFUN ("extent-begin-glyph", Fextent_begin_glyph, Sextent_begin_glyph, 1, 1, 0,
  "Return the glyph object displayed at the beginning of EXTENT.\n\
Returns nil, a pixmap, or a string.")
     (extent_obj)
     Lisp_Object extent_obj;
{
  return (Fextent_glyph_1 (extent_obj, 1));
}

DEFUN ("extent-end-glyph", Fextent_end_glyph, Sextent_end_glyph, 1, 1, 0,
  "Return the glyph object displayed at the end of EXTENT.\n\
Returns nil, a pixmap, or a string.")
     (extent_obj)
     Lisp_Object extent_obj;
{
  return (Fextent_glyph_1 (extent_obj, 0));
}

DEFUN ("set-extent-layout", Fset_extent_layout,
       Sset_extent_layout, 2, 2, 0,
  "Set the layout policy of the given extent.\n\
Access this using the `extent-layout' function.")
	(extent, layout)
	Lisp_Object extent, layout;
{
  EXTENT e;

  CHECK_EXTENT (extent, 0);
  CHECK_SYMBOL (layout, 0);
  e = XEXTENT (extent);

  if (EQ (Qoutside_margin, layout))
    e->flags.glyph_layout = GL_OUTSIDE_MARGIN;
  else if (EQ (Qinside_margin, layout))
    e->flags.glyph_layout = GL_INSIDE_MARGIN;
  else if (EQ (Qwhitespace, layout))
    e->flags.glyph_layout = GL_WHITESPACE;
  else if (EQ (Qtext, layout))
    e->flags.glyph_layout = GL_TEXT;
  else
    signal_simple_error (GETTEXT ("unknown layout type"), layout);
  
  return layout;
}

DEFUN ("extent-layout", Fextent_layout, Sextent_layout, 1, 1, 0,
  "Return the layout policy associated with the given extent.\n\
Set this using the `set-extent-layout' function.")
	(extent)
	Lisp_Object extent;
{
  struct extent *e;

  CHECK_EXTENT (extent, 0);
  e = XEXTENT (extent);

  if (EXTENT_GLYPH_LAYOUT_P (e, GL_OUTSIDE_MARGIN))
    return Qoutside_margin;
  else if (EXTENT_GLYPH_LAYOUT_P (e, GL_INSIDE_MARGIN))
    return Qinside_margin;
  else if (EXTENT_GLYPH_LAYOUT_P (e, GL_WHITESPACE))
    return Qwhitespace;
  else if (EXTENT_GLYPH_LAYOUT_P (e, GL_TEXT))
    return Qtext;
  else
    return Qnil;
}

DEFUN ("extent-priority", Fextent_priority, Sextent_priority, 1, 1, 0,
  "Returns the display priority of EXTENT; see `set-extent-priority'.")
     (extent)
     Lisp_Object extent;
{
  CHECK_EXTENT (extent, 0);
  return make_number (extent_priority (XEXTENT (extent)));
}

DEFUN ("set-extent-priority", Fset_extent_priority, Sset_extent_priority,
       2, 2, 0,
  "Changes the display priority of EXTENT.\n\
When the extent attributes are being merged for display, the priority\n\
is used to determine which extent takes precedence in the event of a\n\
conflict (two extents whose faces both specify font, for example: the\n\
font of the extent with the higher priority will be used.)\n\
Extents are created with priority 0; priorities may be negative.")
	(extent, pri)
	Lisp_Object extent, pri;
{
  int p;
  CHECK_EXTENT (extent, 0);
  CHECK_FIXNUM (pri, 0);
  p = XINT (pri);
  if (p < -0x8000 || p > 0x7fff)	/* must fit in a short */
    error (GETTEXT ("extent priority out of range"));
  extent_priority (XEXTENT (extent)) = p;
  return pri;
}

/* ####  A lot of this stuff is going to change, don't use it yet  -- jwz */

DEFUN ("string-extent-data", Fstring_extent_data, Sstring_extent_data, 1, 1, 0,
 "Return the saved extent data associated with the given string.\n\
\n\
  NOTE: this function may go away in the future, in favor of making\n\
  map-extents accept a string as an argument.\n\
\n\
The format is a list of extent-replica objects, each with an extent\n\
and start and end positions within the string itself.\n\
Set this using the `set-string-extent-data' function.\n\
\n\
The `concat' function logically concatenates this list, reconstructing\n\
the extent information with adjusted start and end positions.\n\
\n\
When `buffer-substring' or a similar function creates a string,\n\
it stores an entry on this list for every `duplicable' extent overlapping\n\
the string.  See `set-extent-property'.\n\
\n\
When `insert' or a similar function inserts the string into a buffer,\n\
each saved extent is copied into the buffer.  If the saved extent is\n\
already in the buffer at an adjacent location, it is extended.  If the\n\
saved extent is detached from the buffer, it is reattached.  If the saved\n\
extent is already attached, or is detached from a different buffer, it is\n\
copied as if by `copy-extent', and the extent's `paste-function' is\n\
consulted.  This entire sequence of events is also available in the\n\
function `insert-extent'.")
	(string)
	Lisp_Object string;
{
  CHECK_STRING (string, 0);
  return XSTRING (string)->dup_list;
}

DEFUN ("set-string-extent-data", Fset_string_extent_data,
       Sset_string_extent_data, 2, 2, 0,
 "Set the saved extent data associated with the given string.\n\
Access this using the `string-extent-data' function.")
	(string, data)
	Lisp_Object string, data;
{
  CHECK_STRING (string, 0);
  CHECK_LIST (data, 1);

  XSTRING (string)->dup_list = data;
  return string;
}


/* Extent replica goo.
   This is a read-only data structure.
   As far as the Lisp programmer is concerned, it is used ONLY as a carrier for
   string-extent-data information.
   */
DEFUN ("make-extent-replica", Fmake_extent_replica, Smake_extent_replica,
       3, 3, 0,
 "Make an object suitable for use with set-string-extent-data.\n\
The arguments are EXTENT, START, and END.\n\
There are no mutator functions for this data structure, only accessors.")
	(extent, start, end)
	Lisp_Object extent, start, end;
{
  DUP dup;
  Lisp_Object res;

  CHECK_EXTENT (extent, 0);
  CHECK_NUMBER_COERCE_MARKER (start, 1);
  CHECK_NUMBER_COERCE_MARKER (end, 2);

  dup = make_extent_replica (extent, XINT (start), XINT (end));
  XSETEXTENT (res, dup);
  return res;
}
DEFUN ("extent-replica-extent", Fextent_replica_extent, Sextent_replica_extent,
       1, 1, 0,
 "See `make-extent-replica'.")
     (dup)
     Lisp_Object dup;
{
  CHECK_EXTENT_REPLICA (dup, 1);
  return dup_extent (XDUP (dup));
}

DEFUN ("extent-replica-start", Fextent_replica_start, Sextent_replica_start,
       1, 1, 0,
 "See `make-extent-replica'.")
     (dup)
     Lisp_Object dup;
{
  CHECK_EXTENT_REPLICA (dup, 1);
  return make_number (dup_start (XDUP (dup)));
}

DEFUN ("extent-replica-end", Fextent_replica_end, Sextent_replica_end,
       1, 1, 0,
 "See `make-extent-replica'.")
     (dup)
     Lisp_Object dup;
{
  CHECK_EXTENT_REPLICA (dup, 1);
  return make_number (dup_end (XDUP (dup)));
}


DEFUN ("extent-at", Fextent_at, Sextent_at, 1, 4, 0,
       "Find \"smallest\" extent at POS in BUFFER having PROPERTY set.\n\
BUFFER defaults to the current buffer.\n\
PROPERTY defaults to nil, meaning that any extent will do.\n\
Properties are attached to extents with `set-extent-property', which see.\n\
Returns nil if there is no matching extent at POS.\n\
If the fourth argument BEFORE is not nil, it must be an extent; any returned\n\
extent will precede that extent.  This feature allows `extent-at' to be used\n\
by a loop over extents.")
     (pos, buffer, property, before)
     Lisp_Object pos, buffer, property, before;
{
  int position;
  Lisp_Object extent_obj;
  EXTENT extent;

  if (NILP (buffer))
    XSETR (buffer, Lisp_Buffer, current_buffer);

  CHECK_FIXNUM_COERCE_MARKER (pos, 0);
  CHECK_BUFFER (buffer, 0);
  CHECK_SYMBOL (property, 0);
  if (NILP (before))
    extent = 0;
  else
    {
      CHECK_EXTENT (before, 0);
      extent = XEXTENT (before);
    }

  position = XINT (pos);
  check_from_to (position, position, XBUFFER (buffer), 1);

  extent = extent_at_before (position, XBUFFER (buffer), property, extent);
  if (!extent)
    return (Qnil);

  XSETEXTENT (extent_obj, extent);
  return extent_obj;
}

DEFUN ("next-extent", Fnext_extent, Snext_extent, 1, 1, 0,
       "Find next extent after EXTENT. If EXTENT is a buffer\n\
return the first extent in the buffer.")
  (extent)
   Lisp_Object extent;
{
  Lisp_Object val;
  EXTENT next;

  if (BUFFERP (extent))
    return XBUFFER (extent)->extents;

  CHECK_EXTENT (extent, 0);
  next = XEXTENT (extent)->next;
  if (next == XEXTENT (extent)) abort ();

  if (!next)
    return (Qnil);
  XSETEXTENT (val, next);
  return (val);
}

#if 0 /* debugging code */

/* There are 2 total orders on extents in a buffer -- the normal
   (or "display") order, and the "e-order". This returns the
   "next extent" in the e-ordering.  This might be a temporary function
   or it might be permanent. */

**DEFUN ("next-e-extent", Fnext_e_extent, Snext_e_extent, 1, 1, 0,
       "Find next extent after EXTENT using the \"e\" order. If \n\
EXTENT is a buffer, return the first extent in the buffer.")
  (extent_obj)
   Lisp_Object extent_obj;
{
  if (BUFFERP (extent_obj))
    {
      Lisp_Object return_val;
      EXTENT tmp;

      if (EXTENTP (XBUFFER (extent_obj)->extents))
        tmp = XEXTENT (XBUFFER (extent_obj)->extents);
      else
        return Qnil;

      while (tmp->e_previous)
        tmp = tmp->e_previous;

      XSET (return_val, Lisp_Extent, tmp);
      return return_val;
    }
  else if (EXTENTP (extent_obj))
    {
      Lisp_Object return_val = Qnil;
      EXTENT next = XEXTENT(extent_obj)->e_next;

      if (next)
        XSET (return_val, Lisp_Extent, next);
      return return_val;
    }
  else if (NILP (extent_obj))
    return Qnil;
  else
    CHECK_EXTENT (extent_obj, 0);
}


/* Purportedly temporary debugging function -- turns a stack of
   extents into something that can be looked at in elisp. For
   a detailed explanation of a stack of extents, see below. */

static Lisp_Object
soe_to_lisp (struct stack_of_extents *soe, struct buffer *buf)
{
  if (!soe || 
      (soe->buf_index <= 0))
    return Qnil;
  else
    {
      Lisp_Object struct_vec = make_vector (6, Qzero);
      Lisp_Object *struct_v = &(XVECTOR (struct_vec)->contents[0]);
      Lisp_Object stack_vec = make_vector (soe->stack_index, Qzero);
      Lisp_Object *stack_v = &(XVECTOR (stack_vec)->contents[0]);

      struct_v[0] = intern (":PRIOR-EXTENT");
      if (soe->previous_extent)
        XSETEXTENT (struct_v[1], soe->previous_extent);
      else
        struct_v[1] = Qnil;
    
      struct_v[2] = intern (":BUFFER-POS");
      XSET(struct_v[3], Lisp_Int, 
           extent_index_to_buffer_pos (soe->buf_index, buf));
    
      struct_v[4] = intern (":STACK");
      struct_v[5] = stack_vec;

      {
        int i;
      
        for (i = 0; i < soe->stack_index; i++)
          {
            EXTENT tmp = soe->stack[i];

            if (tmp)
              {
                XSETEXTENT (stack_v[i], tmp);
              }
            else 
              stack_v[i] = Qnil;
          }
      }

      return struct_vec;
    }
}

/* Elisp interface for debugging stacks of extents */
**DEFUN ("stack-of-extents", Fstack_of_extents, 
       Sstack_of_extents, 1, 2, 0,
       "Return stack of extents for BUFFER. Optional arg POSITION supplied\n\
means compute the correct stack of extents for POSITION in BUFFER.")
  (buffer, position)
   Lisp_Object buffer, position;
{
  Lisp_Object return_value = Qnil;
  struct stack_of_extents *soe;
  struct buffer *buf = XBUFFER (buffer);
  CHECK_BUFFER (buffer, 0);
  if (!NILP (position))
    {
      struct stack_of_extents *tmp = buf->cached_stack;
      int pos;
      CHECK_FIXNUM_COERCE_MARKER (position, 1);
      pos = XINT (position);
      if ((pos < BUF_BEG(buf)) || (BUF_Z (buf) < pos))
        arg_out_of_range (pos, Qnil);
      
      buf->cached_stack = 0;
      init_buffer_cached_stack (buf);
      befa_internal (pos, buf, 0);
      soe = buf->cached_stack;
      buf->cached_stack = tmp;
    }
  else
    soe = buf->cached_stack;
      
  if (!soe)
    return Qnil;
  else
    return_value = soe_to_lisp (soe, buf);
  
  if (soe && (soe != buf->cached_stack))
    {
      EXTENT *tmp_stack = soe->stack;
      soe->stack = 0;
      if (tmp_stack)
        xfree (tmp_stack);
      xfree (soe);
    }

  return return_value;
}

#endif /* debugging code */

#if 0
/* Debugging and test function -- maybe permanent. */
/* Return 0 if stack is correct, 1 if stack has been cleared (which
   is not incorrect but isn't good news), and -1 if it is provably
   incorrect. */
static int
verify_buffer_stack (struct buffer *buf)
{
  struct stack_of_extents *soe;

  if (!buf)
    buf = current_buffer;

  soe = buf->cached_stack;

  if (!soe)
    {
      if (EXTENTP (buf->extents))
        return -1;
      else if (NILP (buf->extents))
        return 0;
      else
        abort ();
    }
  else if (soe->buf_index <= 0)
    return 1;
  else
    {
      int pos = extent_index_to_buffer_pos (soe->buf_index, buf);
      int return_value;
      buf->cached_stack = 0;
      init_buffer_cached_stack (buf);
      befa_internal (pos, buf, 0);
      
      return_value = memcmp ((char *) soe, (char *) buf->cached_stack,
			     sizeof (*soe));
      if (!return_value)
        return_value = 
          memcmp ((char *) soe->stack, (char *) buf->cached_stack->stack, 
		  soe->stack_index * sizeof (EXTENT));
      if (return_value)
        return_value = -1;
      free_buffer_cached_stack (buf);
      buf->cached_stack = soe;
      return return_value;
    }
}
#endif /* 0 */


/* Long comment: 

   A "Stack Of Extents" (abbreviated SOE) is a data structure that is
   an abbreviated version of an extent_fragment.  However, unlike the
   extent_fragment, of which there are really only 2, which are used
   by redisplay, and which are supposed to be FAST to manage, every
   buffer with extents has an SOE, these are supposed to be robust,
   and they don't have to be that fast to manage. What an SOE does is
   cache the information needed to try to find, given a buffer index
   I, some extent E which is "before" I in the display order (meaning
   that E doesn't include I, and any extent E' that does include I is
   "greater than" E in the display order). The purpose of this is to
   make functions like Fextent_at() and buffer_extent_fragment_at() be
   as fast at the bottom of a buffer as they are at the top, no matter
   how many extents the buffer has. (We may want to consider having
   detach_extent() and splice_extent_into_buffer() update the SOE,
   too, so that updating extents will be faster and adding extents "in
   order" will be as fast as adding them in "reverse order" is.)

   The two orderings of extents in a buffer, normal (or display) order
   and e-order, are defined by macros at the top of the file.
   Basically, though, normal order sorts by increasing start index and
   e-order by increasing end index.

   The components of an SOE are the buffer index, the vector of
   extents (in display order) that lie over that index, and the
   "previous extent", which is the last extent in the e-order whose
   end index is <= the given buffer index and which is NOT over the
   buffer index (if the extent has 0 length, it will be over the index
   if the start and end values equal the index).

   An SOE is "cleared" or invalidated iff the buffer index value is <=
   0, since this is never a legal buffer index -- this clearing is
   done by soe_clear(), and is called any time the SOE can't get
   updated properly. 

   Each time an extent is spliced into the buffer, an attempt is made
   to "push" it onto the SOE (which means that we examine the new
   extent to see if it goes over that buffer index or if it should
   replace the previous extent).  When an extent is detached, we
   similarly attempt to delq it from the SOE.

   Any call to the function befa_internal() changes the SOE to use the
   new buffer index via soe_duplicate(). The function
   process_extents_for_deletion() may change the endpoints of extents,
   of adjust the buffer index, so it calls soe_prune(). This function
   removes any extents that are not over the buffer index, and
   corrects the previous extent value.

   There are also init and free functions for the SOE, called by
   splice_extent_into_buffer() and Fkill_buffer, respectively.



   Finally, what's the point of all of this? The point is that given
   an index I, the set S of extents over I, and the previous extent P,
   it is possible to compute an extent before an index I' (before in
   the display order). If I' >= I, this is just P (which can be easily
   recomputed from S[0] if S exists and P does not -- see
   soe_prune()).  If I' < I, then consider the interval [E, P] of
   extents in the e-order such that E is the first extent with end
   index less than I'. Then the minimum (in the normal order) E' of
   these extents is either strictly before I' or is the "top of the
   stack at" I', and in the latter case, E'->previous is before I'.

   The proof is left as an exercise for the readers.
 */

static void
soe_push (EXTENT extent, struct buffer *b)
{
  struct stack_of_extents *soe = b->cached_stack;
  if (soe)
    {
      int index = soe->buf_index;
      int on_stack = 0;

      if (index <= 0)
	{
	  soe_clear(b);
	  return;
	}
      else if (EXTENT_OVER_INDEX (extent, index))
	on_stack = 1;

      if (on_stack)
	{
	  if (soe->stack_index == soe->stack_length)
	    {
	      soe->stack_length *= 2;
	      soe->stack = 
		(EXTENT*)xrealloc (soe->stack, 
				   soe->stack_length * sizeof (EXTENT));
	    }

	  if (soe->stack_index == 0)
	    {
	      soe->stack[0] = extent;
	      soe->stack_index = 1;
	    }
	  else
	    {
	      EXTENT *cef, *cef_bound, *cef_copy, *cef_put_here;

	      /* make sure that this guy isn't in the stack already */
	      soe_delq (extent, b);
      
	      cef = soe->stack;
	      cef_copy = cef;
	      cef_bound = cef + soe->stack_index;
	      cef_put_here = cef_bound;

	      while (cef < cef_bound)
		{
		  if (EXTENT_LESS (extent, *cef))
		    {
		      cef_put_here = cef;
		      break;
		    }
		  cef++;
		}
  
	      cef = cef_bound;
	      while (cef > cef_put_here)
		{
		  *cef = *(cef - 1);
		  cef--;
		}
	      *cef_put_here = extent;
	      soe->stack_index++;
	    }
	}
      else if (extent_end (extent) < index)
	{
	  EXTENT prev = soe->previous_extent;
	  if (!prev || EXTENT_E_LESS (prev, extent))
	    soe->previous_extent = extent;
	}
    }
}

static void
soe_duplicate (int index, EXTENT *copy_from, int copy_from_size, 
	       EXTENT trial_prev, struct buffer *b)
{
  struct stack_of_extents *soe = b->cached_stack;

  if (!soe)
    return;

  if (copy_from_size > 0)
    {
      if (soe->stack_length < copy_from_size)
	{
	  soe->stack_length = copy_from_size + 1;
	  soe->stack = 
            (EXTENT*)xrealloc (soe->stack, 
                               soe->stack_length * sizeof (EXTENT));
	}
      memcpy ((char *) soe->stack, (char *) copy_from,
	      copy_from_size * sizeof (EXTENT));

      trial_prev = soe->stack[0];
    }
  
  soe->stack_index = copy_from_size;
  soe->buf_index = index;

  if (!trial_prev || !EXTENT_PAST_INDEX (trial_prev, index))
    trial_prev = 0;

  while (trial_prev && EXTENT_PAST_INDEX (trial_prev, index))
    trial_prev = trial_prev->e_previous;

  if (!trial_prev)
    soe_clear(b);
  else
    soe->previous_extent = trial_prev;
}


static void
soe_delq (EXTENT extent, struct buffer *b)
{
  struct stack_of_extents *soe = b->cached_stack;
  if (soe)
    {
      EXTENT* cef = soe->stack;
      EXTENT* cef_copy = cef;
      EXTENT* cef_bound = cef + soe->stack_index;
      int found = 0;

      while (cef < cef_bound)
	{
	  if (extent == *cef)
	    {
	      *cef = 0;
	      found++;
	    }
	  cef++;
	}
  
      if (found > 0)
	{
	  cef = cef_copy; 
	  while (cef < cef_bound)
	    {
	      if (*cef)
		{
		  *cef_copy = *cef;
		  cef_copy++;
		}
	      cef++;
	    }

	  soe->stack_index = cef_copy - soe->stack;
	}

      if (extent == soe->previous_extent)
	soe->previous_extent = extent->e_previous;
    }
}

void 
init_buffer_cached_stack (struct buffer *b)
{
  if (!b->cached_stack)
    {
      int default_stack_size = 10;
      struct stack_of_extents *new =
        (struct stack_of_extents *) 
          xmalloc (sizeof (struct stack_of_extents));
      memset ((char *) new, 0, sizeof (struct stack_of_extents));
      new->stack_length = default_stack_size;
      new->stack = (EXTENT *) xmalloc (default_stack_size * sizeof (EXTENT));
      b->cached_stack = new;
    }
}

static void
soe_clear (struct buffer *b)
{
  struct stack_of_extents *soe = b->cached_stack;
  if (soe)
    {
      soe->stack_index = 0;
      soe->buf_index = 0;
      soe->previous_extent = 0;
    }
}

static void
soe_prune (struct buffer *b)
{
  struct stack_of_extents *soe = b->cached_stack;

  if (!soe)
    return;
  
  if (soe->buf_index <= 0)
    {
      soe_clear(b);
      return;
    }
  else
    {
      EXTENT* cef = soe->stack;
      EXTENT* cef_copy = cef;
      EXTENT* cef_bound = cef + soe->stack_index;
      int index = soe->buf_index;
      int removedp = 0;

      while (cef < cef_bound)
	{
	  EXTENT extent = *cef;
	  if (!EXTENT_OVER_INDEX (extent, index))
	    {
	      *cef = 0;
	      removedp = 1;
	    }
	  cef++;
	}
  
      if (removedp)
	{
	  cef = cef_copy; 
	  while (cef < cef_bound)
	    {
	      if (*cef)
		{
		  *cef_copy = *cef;
		  cef_copy++;
		}
	      cef++;
	    }

	  soe->stack_index = cef_copy - soe->stack;
	}
    }

  {
    EXTENT prev;
    int index = soe->buf_index;

    if (soe->stack_index > 0)
      prev = soe->stack[0];
    else
      prev = soe->previous_extent;

    while (prev && EXTENT_PAST_INDEX (prev, index))
      prev = prev->e_previous;
    if (prev)
      soe->previous_extent = prev;
    else
      soe_clear (b);
  }
}

void
free_buffer_cached_stack (struct buffer *b)
{
  struct stack_of_extents *tmp = b->cached_stack;
  b->cached_stack = 0;
  if (tmp)
    {
      EXTENT *tmp_stack = tmp->stack;
      tmp->stack = 0;
      if (tmp_stack)
        xfree (tmp_stack);
      xfree (tmp);
    }
}

/* This function assumes that ef is correct and up to date, and then
   ef->start_index is incremented and ef->first_extent_past_stack is either
   0'd or certified correct, and then this function is called. This
   function removes all extents from the extents_stack that are "too small"
   (the only way the can be invalid here, since if they were "too big" now,
   then they would have been "too big" when ef->start_index was smaller).
   In particular, ef->extents_stack will be correct unless there are 
   "bigger extents" than those in the stack that remain to be pushed on it.
   
   At the same time, it sets ef->end_index to be the minimum of all of the
   end indicies of the extents left in the stack, if any, and the
   start_index of ef->first_extent_past_stack, if that exists. If none of
   these exist, the ef->end_index is set to be the max index of the buffer.
   This value of ef->end_index is at least as large as the correct value
   for ef->buffer and given ef->start_index, and it is correct if the
   extents_stack is complete and the value of ef->first_extent_past_stack
   passed in is correct. */
   
static void
cleanup_old_stack (EXTENT_FRAGMENT ef)
{
  EXTENT* cef = ef->extents_stack;
  EXTENT* cef_copy = cef;
  EXTENT* cef_bound = cef + ef->number_of_extents;
  LISP_WORD_TYPE new_start = ef->start_index;
  LISP_WORD_TYPE min_for_end = 
    (ef->first_extent_past_stack
     ? extent_start (ef->first_extent_past_stack)
     : MAX_INT);

  while (cef < cef_bound)
    {
      LISP_WORD_TYPE end = extent_end (*cef);
      LISP_WORD_TYPE start = extent_start (*cef);
   
      /* flush any extent ending too soon, which includes any extent ending
         before this point or any extent of positive length ending AT this
         point */
      if ((end < new_start) ||
          ((end == new_start) && (start < end)))
	*cef = 0;
      /* if the extent wasn't flushed and is of positive length,
         then if it ends before our min so far, use this as the
         new min */
      else if ((end < min_for_end) && (start < end))
	min_for_end = end;
      cef++;
    }
  
    cef = cef_copy; 
    while (cef < cef_bound)
    {
      if (*cef)
	{
          *cef_copy = *cef;
          cef_copy++;
	}
      cef++;
    }

  ef->end_index = min_for_end;
  ef->number_of_extents = cef_copy - ef->extents_stack;
}

/* Long comment:
   
   This function updates the cached ef to "surround" the new buf_index 
   that is promised to be == the current ef->end_index. The on

   Extents are ordered with increasing start position and then decreasing
   end position. (This is what might be called "display order" -- if extent
   A occurs after extent B, then the display attributes of extent A
   override those of extent B in the region covered by both A and B. Note
   that multiple extents with the same start and end postions may be in any
   order.)

   The extents_stack has the extents that are "over" a given buffer
   index in the same order as in the buffer's extent list. The state
   coming in might look like this:


   extents_stack |
                 v

   [.......................................)
   [............................)
       [...........................)
       [.....................)
            [...........................)
                             |  <<-------  buf_index required to be this
            [________________)  <<-------  fragment boundaries

                                                    first_extent_past_stack |
                                                                            v
                                                    [........................)

                             [__)  new fragment boundaries
   
   If there is no first_extent_past_stack, then we just prune the 
   extents_stack as described below, and if there are no extents over
   the buf_index, the new fragment goes from buf_index to the end of
   the buffer. 
   
   Otherwise, the first_extent_past_stack is guaranteed to have its
   start index be >= buf_index of the fragment.

   If the first_extent_past_stack start is > buf_index, then the
   first_extent_past_stack will still be the same after the update.  In
   that case all that is needed is to update the extents_stack by deleting
   anything that "fell off the end", set the new start_index to the
   buf_index and compute the new end_index. (In the special case where
   there is one extent in the extents_stack, this calculation is easy,
   since that means that the new stack is empty and the new end_index for
   the fragment is next->start. If the extents_stack is empty, then
   the caller of this function is broken and we abort().)

   The remaining case is when the first_extent_past_stack starts at
   buf_index. If there are extents in the stack, then we need to remove all
   of the ones that have fallen off the end.  After that
   first_extent_past_stack gets pushed, and we look for any other extents
   that need pushing. While we are doing this, we need to figure out where
   the new fragment ends, and what the new first_extent_past_stack is. (We
   could consider optimizing the case of their being one extent in the
   stack


   NOTE: At the moment only this and a few other functions
   know how to "cdr" down a list of extents. 
   See the comment at map_extents_after() for information 
   about the ordering rule. */

static EXTENT_FRAGMENT
update_cache_forward (EXTENT_FRAGMENT ef, int buf_index, struct buffer* buf)
{
  /* Incrementally update the cache forward.  This has to be FAST. */
  EXTENT next = ef->first_extent_past_stack;
  int g0 = BUF_GPT (buf);
  int g1 = g0 + BUF_GAP_SIZE (buf);
  int next_start;
  int next_end;

  ef->start_index = buf_index;

  if (!next)
    {
      /* ef->first_extent_past_stack is correct here, because there
         isn't any */
      if (ef->number_of_extents == 1)
        {
          /* easy case for updating stack */
          ef->number_of_extents = 0;   
	  ef->end_index = MAX_INT;
        }
      else if (ef->number_of_extents == 0)
        abort();
      else
        /* this will fix ef->end_index, since ef->first_extent_past_stack
           is correctly 0 and there are no more extents to add to the stack */
        cleanup_old_stack (ef);
   
      /* the stack, ef->start_index, and ef->end_index are all
         correct, so just wrap_up */   
      goto wrap_up;
    }
  else if (buf_index < (extent_start (next) == g1 ? g0 : extent_start (next)))
    {
      /* ef->first_extent_past_stack is correct here, because it  won't
         change as the result of this operation */
      if (ef->number_of_extents == 1)
        {
          /* easy case for updating stack -- moving into a "hole" between
             extents */
          ef->number_of_extents = 0;
          ef->end_index =
	    (extent_start (next) == g1 ? g0 : extent_start (next));
        }
      else if (ef->number_of_extents == 0)
        abort();
      else
        /* the stack will add nothing new, because
           ef->first_extent_past_stack is too big to go on the stack, and
           so it is correct, too -- this means that call will set
           ef->end_index to be == correct value; see comment for
           cleanup_old_stack() */
        cleanup_old_stack (ef);

      /* the stack, ef->start_index, and ef->end_index are all
         correct, so just wrap_up */   
      goto wrap_up;   
    }
  else if (ef->number_of_extents > 0)
    {
      /* ef->first_extent_past_stack wrong here, because it is next,
         and next is going onto the stack -- the stack is wrong, too */
      ef->first_extent_past_stack = 0;
      /* this will set ef->end_index to be >= correct value; see
         comment for function */
      cleanup_old_stack (ef);
    }
  else 
    /* stack is empty so needs no pruning, and we know that extent_end (next)
       is at least as big as the fragment end_index */
    ef->end_index = (extent_end (next) == g1 ? g0 : extent_end (next));

  /* If we get to this point, ef->start_index is correct,
     ef->end_index is >= the correct value, and
     ef->first_extent_past_stack is completely wrong */

  {
    /* retain last extent to be pushed on the stack */
    EXTENT last;
    do
      {
        if (ef->number_of_extents == ef->extents_stack_length)
          {
            ef->extents_stack_length *= 2;
            ef->extents_stack =
              (EXTENT*)xrealloc (ef->extents_stack,
                                 ef->extents_stack_length * sizeof (EXTENT));
          }
        ef->extents_stack [ef->number_of_extents] = next;
        ef->number_of_extents += 1;

        last = next;
        next = next->next;
	if (next == last) abort ();
	if (next && next->previous != last) abort ();
	if (next)
	  next_start = (extent_start (next) == g1 ? g0 : extent_start (next));
      }
    while (next && (next_start == buf_index));

    /* make sure that ef->end_index is correct, since we may have
       come into this function with a value that is too big -- 
       recall that since the end values of the extents are
       decreasing while the start value stay the same, last->end
       has the smallest "end" of all things pushed onto the stack */
    next_end = (extent_end (last) == g1 ? g0 : extent_end (last));
    if (next_end < ef->end_index)
      ef->end_index = next_end;
    if (next)
      next_start = (extent_start (next) == g1 ? g0 : extent_start (next));
    if (next &&
        (next_start < ef->end_index))
      ef->end_index = next_start;
  }
  ef->first_extent_past_stack = next;

 wrap_up:
  ef->from = extent_index_to_buffer_pos (ef->start_index, buf);
  ef->to = 
    ((ef->end_index == MAX_INT)?MAX_INT:
     extent_index_to_buffer_pos (ef->end_index, buf));
  return ef;
}


/* Find extent fragment at pos in buf.
   NOTE: At the moment only this and a few other functions
   know how to "cdr" down a list of extents. 
   See the comment at map_extents_after() for information 
   about the ordering rule. */

/* The screen argument was added because of the faces being able to be
   screen-local.  The same buffer on different screens has to be
   considered to be two distinct buffers in order to ensure correct
   redisplay.  There are two functions which can call this which have
   no way of knowing which screen the buffer is one.  Both are commented
   out so it isn't a problem. */
static EXTENT_FRAGMENT
befa_internal (int pos, struct buffer *buf, struct screen *s)
{
  EXTENT_FRAGMENT ef;
  EXTENT current;
  EXTENT trial_prev = 0;
  int buf_index;
  LISP_WORD_TYPE new_start;
  LISP_WORD_TYPE new_end;
  int g0 = BUF_GPT (buf);
  int g1 = g0 + BUF_GAP_SIZE (buf);
  int real_start, real_end;

  buf_index = buffer_pos_to_extent_index (pos, buf, 0);
  ef = &extent_fragment;

  current = buffer_starting_extent (buf_index, buf);
  
  new_start = buffer_pos_to_extent_index (BUF_BEG(buf), buf, 0);
  new_end = MAX_INT;
  ef->number_of_extents = 0;

  /* Find all of the extents "over" this fragment, and at the same time
     find the "first_extent_past_stack" for the fragment and the 
     start_index. */
  while (current && 
         ((extent_start (current) == g1
	   ? g0
	   : extent_start (current)) <= buf_index))
    {
      trial_prev = current;

      real_end = (extent_end (current) == g1 ? g0 : extent_end (current));
      real_start = (extent_start(current) == g1 ? g0 : extent_start (current));

      if ((real_end < buf_index) &&
	  (real_end > new_start))
        {
          new_start = real_end;
        }
      /* we repeat some code in this clause and the next to save tests */
      else if (real_end > buf_index)
	{
          if (real_start > new_start)
            new_start = real_start;
          if (real_end < new_end)
            new_end = real_end;

	  if (ef->number_of_extents == ef->extents_stack_length)
	    {
	      ef->extents_stack_length *= 2;
	      ef->extents_stack =
		(EXTENT*)xrealloc (ef->extents_stack,
				   ef->extents_stack_length * sizeof (EXTENT));
	    }
	  ef->extents_stack [ef->number_of_extents] = current;
	  ef->number_of_extents += 1;
	}
      /* we repeat some code in this clause and the last to save tests */   
      else if (real_end == real_start &&
	       (real_end == buf_index))
	{
          new_end = new_start = buf_index;

	  if (ef->number_of_extents == ef->extents_stack_length)
	    {
	      ef->extents_stack_length *= 2;
	      ef->extents_stack =
		(EXTENT*)xrealloc (ef->extents_stack,
				   ef->extents_stack_length * sizeof (EXTENT));
	    }
	  ef->extents_stack [ef->number_of_extents] = current;
	  ef->number_of_extents += 1;
	}
      else if ((real_end == buf_index) &&
	       (real_end > new_start))
        {
          new_start = real_end;
        }
        
      if (current == current->next) abort ();
      current = current->next;
    }

  /* Check the end_index for this fragment. */
  if (current)
    real_start = (extent_start(current) == g1 ? g0 : extent_start (current));
  if (current && 
      (real_start < new_end))
    new_end = real_start;
  
  XSETR (Vextent_fragment_buffer, Lisp_Buffer, buf);
  ef->buf = buf;
  ef->s = s;
  ef->modiff = BUF_MODIFF (buf);
  ef->face_change = BUF_FACECHANGE (buf);
  ef->first_extent_past_stack = current;
  ef->start_index = new_start;
  ef->end_index = new_end;
  ef->from = extent_index_to_buffer_pos (new_start, buf);
  ef->to = 
    (new_end == MAX_INT)?MAX_INT:extent_index_to_buffer_pos (new_end, buf);

  soe_duplicate (buf_index, ef->extents_stack, 
                 ef->number_of_extents, trial_prev, buf);

  return ef;
}

/*
 * include_zero_width is used to tell setup_extent_fragment_face_ptr
 * whether to merge in the faces of 0-width extents.  It is used by
 * redisplay to get the correct face for 0-width extents which are often
 * used to hold glyphs.
 */
EXTENT_FRAGMENT
buffer_extent_fragment_at (int pos, struct buffer *buf, struct screen *s,
			   int include_zero_width)
{
  int cache_valid;
  EXTENT_FRAGMENT ef;
  LISP_WORD_TYPE buf_index;

  if (NILP (buf->extents))
    {
      Vextent_fragment_buffer = Qnil;
      extent_fragment.buf = 0;
      default_extent_fragment.from = BUF_BEG (buf);
      default_extent_fragment.to = MAX_INT;
      return &default_extent_fragment;
    }
  else if (!EXTENTP (buf->extents))
    abort ();

  buf_index = buffer_pos_to_extent_index (pos, buf, 0);
  ef = &extent_fragment;
  cache_valid = (!extent_cache_invalid &&
		 (buf == ef->buf) &&
		 (s == ef->s) &&
                 (BUF_MODIFF(buf) == ef->modiff) &&
		 (BUF_FACECHANGE (buf) == ef->face_change));

  if (cache_valid)
    {
      if (buf_index == ef->end_index)
	ef = update_cache_forward (ef, buf_index, buf); /* 99% of the time */
      else if ((pos >= extent_fragment.from) && (pos < extent_fragment.to))
	return ef;
      else 
	ef = befa_internal (pos, buf, s);
    }
  else 
    ef = befa_internal (pos, buf, s);

  setup_extent_fragment_face_ptr (s, ef, include_zero_width);
  extent_cache_invalid = 0;
  return ef;
}

static void
init_extent_fragment ()
{
  int l = 30;

  memset ((char *) &extent_fragment, 0, sizeof (extent_fragment));
  extent_fragment.extents_stack_length = l;
  extent_fragment.extents_stack = (EXTENT *) xmalloc (l * sizeof (EXTENT));

  memset ((char *) &default_extent_fragment, 0,
	  sizeof (default_extent_fragment));
  extent_cache_invalid = 1;
}



/* Modify all of the extents as required for the insertion, based on their
   start-open/end-open properties.

   Note that if the gap has just been moved, some of this might seem to have
   been done already (since the gap will have been positioned optimally w.r.t.
   the extent's endpoints.)
 */

static int process_extents_for_insertion_mapper (EXTENT extent, void *arg);

void
process_extents_for_insertion (int opoint, int length, struct buffer *buf)
{
  if (NILP (buf->extents))
    return;
  else if (!EXTENTP (buf->extents))
    abort();
  else
    {
      struct process_extents_for_insertion_arg closure;
      int from = opoint;
      int to = from + length;

      closure.opoint = opoint;
      closure.length = length;
      closure.buf = buf;

      /* Make sure we hit all relevant extents by mapping over a region
	 that is one character larger in either direction...  This is
	 kind of a kludge, I wonder if it ought to be done differently.
       */
      if (from > BUF_BEGV (buf))
	from--;
      if (to < BUF_ZV (buf))
	to++;

      map_extents (from, to, 0, process_extents_for_insertion_mapper,
                   (void *) &closure, buf, 1);
    }

}

static int
process_extents_for_insertion_mapper (EXTENT extent, void *arg)
{
  struct process_extents_for_insertion_arg *closure = 
    (struct process_extents_for_insertion_arg *) arg;
  struct buffer *buf = closure->buf;
  /* These are in buffer positions (ignore the gap). */
  int insertion_start = closure->opoint;
  int insertion_length = closure->length;
  int insertion_end = insertion_start + insertion_length;

  /* Convert the peis values to extent positions, for comparison.
     Straddle the gap when possible. (?)
   */
  if (insertion_start > BUF_GPT (buf))
    insertion_start += BUF_GAP_SIZE(buf);
  if (insertion_end >= BUF_GPT (buf))
    insertion_end += BUF_GAP_SIZE(buf);
  insertion_length = insertion_end - insertion_start;

  /* When this function is called, one end of the newly-inserted text should
     be adjascent to some endpoint of the extent, or disjoint from it.  If
     the insertion overlaps any existing extent, something is wrong.
   */
  if (extent_start (extent) > insertion_start &&
      extent_start (extent) < insertion_end)
    abort ();
  if (extent_end (extent) > insertion_start &&
      extent_end (extent) < insertion_end)
    abort ();

  if (insertion_end == extent_start (extent))
    {
      /* New text is currently before the start of this extent.
	 If the extent is closed on the left, then these characters should
	 go into the extent, which means that the extent's starting position
	 should be decremented by the length of the insertion (since the
	 current state is that the new characters end where the extent
	 begins.)
       */
      if (!EXTENT_START_OPEN_P (extent))
	extent_start (extent) -= insertion_length;
    }
  else if (insertion_start == extent_start (extent))
    {
      /* New text is currently at the start of this extent.
	 If the extent is open on the left, then these characters should
	 not go into the extent, which means that the extent's starting
	 position should be incremented by the length of the insertion
	 (since the current state is that the new characters are inside
	 the extent.)
       */
      if (EXTENT_START_OPEN_P (extent))
	{
	  /* This is a correction factor for 0-width extents.  The
             whole implementation with regards to 0-width extents
             still needs to be completely decided upon and done. */
	  if (extent_index_to_buffer_pos (extent_start (extent), buf) ==
	      extent_index_to_buffer_pos (extent_end (extent), buf))
	    extent_end (extent) += insertion_length;

	  extent_start (extent) += insertion_length;
	}
    }
  else if (insertion_start == extent_end (extent))
    {
      /* New text is currently after the end of this extent.
	 If the extent is closed on the right, then these characters should
	 go into the extent, which means that the extent's ending position
	 should be incremented by the length of the insertion (since the
	 current state is that the new characters start where the extent
	 ends.)
       */
      if (!EXTENT_END_OPEN_P (extent))
	extent_end (extent) += insertion_length;
    }
  else if (insertion_end == extent_end (extent))
    {
      /* New text is currently at the end of this extent.
	 If the extent is open on the right, then these characters should
	 not go into the extent, which means that the extent's ending position
	 should be decremented by the length of the insertion (since the
	 current state is that the new characters are inside the extent.)
       */
      if (EXTENT_END_OPEN_P (extent))
	extent_end (extent) -= insertion_length;
    }

  return 0;
}


/* Delete all of the extents that are completely inside the range [from, to).

   (Do we need to worry about opened/closed endpoints here too?  I don't
   think so. -jwz)

   NOTE: This function must either preserve the internal ordering of the
   extents automatically or it must explicitly fix that ordering before
   quitting.  At the moment the ordering is preserved automatically.
   [from to) is the range of POSITIONs being deleted and [start end) is the
   INDEX values of the gap when the deletion is completed.
   */

static int process_extents_for_deletion_mapper (EXTENT extent, void *arg);

void
process_extents_for_deletion (int from, int to, int start, int end,
			      struct buffer *buf)
{
  if (NILP (buf->extents))
    return;
  else if (!EXTENTP (buf->extents))
    abort();
  else
    {
      struct process_extents_for_deletion_arg closure;

      check_from_to (from, to, buf, 0);

      /* start and end don't need to be turned into index values because
         they are already -- from and to are buffer positions */
      closure.start = start;
      closure.end = end;
      closure.destroy_included_extents = 0;
      /* Need to do a map_extent with closed_end so that the extent
	 just beginning or ending on the old gap are processed too.
	 --Matthieu. */
      map_extents (from, to, 0, process_extents_for_deletion_mapper,
                   (void *) &closure, buf, 1);

      if (buf->cached_stack
	  && (buf->cached_stack->buf_index >= start)
	  && (buf->cached_stack->buf_index < end))
	buf->cached_stack->buf_index = start;
      soe_prune (buf);
    }
}

#if 0 /* unused */
void
process_extents_for_destruction (int from, int to, struct buffer *buf)
{
  if (NILP (buf->extents))
    return;
  else if (!EXTENTP (buf->extents))
    abort();
  else
    {
      struct process_extents_for_deletion_arg closure;

      check_from_to (from, to, buf, 0);

      closure.start = buffer_pos_to_extent_index (from, buf, 0);
      closure.end = buffer_pos_to_extent_index (to, buf, 1); /* closed-end */
      closure.destroy_included_extents = 1;
      /* Need to do a map_extent with closed_end so that the extent
	 just beginning or ending on the old gap are processed too.
	 --Matthieu. */
      map_extents (from, to, 0, process_extents_for_deletion_mapper,
                   (void *) &closure, buf, 1);
    }
}
#endif /* 0 */


static int
process_extents_for_deletion_mapper (EXTENT extent, void *arg)
{
  struct process_extents_for_deletion_arg *closure = 
    (struct process_extents_for_deletion_arg *) arg;

  if ((closure->start <= extent_start (extent)) &&
      (extent_end (extent) <= closure->end))
    {
      if (closure->destroy_included_extents)
        destroy_extent (extent);
      else
	{
	  /* Modified version of Fdetach_extent,
	   * removing argument checking and undo recording:
	   */
	  detach_extent (extent);

	  BUF_FACECHANGE (XBUFFER (extent_buffer (extent)))++;
	  extents_changed++;
	}
    }
  else if (closure->destroy_included_extents)
    return 0;
  /* the extent completely contains the deleted range, so we don't need to
     do anything about it */
  else if ((extent_start (extent) < closure->start) &&
	   (closure->end < extent_end (extent)))
    return 0;
  else
    /* these characters are going away, so the extent must be shortened 
       appropriately (do we need to worry about opened/closed endpoints
       here too?  I don't think so. -jwz) */
    {
      LISP_WORD_TYPE max_start = max (extent_start (extent), closure->start);
      LISP_WORD_TYPE min_end = min (extent_end (extent), closure->end);
      /* this test is really unneeded, since map_extents() promises the
         two "spans of text" will overlap but it's cheap and
         I'm nervous */
      if (max_start < min_end)
        {
#if 1
          if (max_start == extent_start (extent))
            extent_start (extent) = min_end;
          else
            extent_end (extent) = max_start;

#else /* new, non-correct code... */

	  struct buffer *buf = XBUFFER (extent_buffer (extent));

	  /* Adjust one of the endpoints of this extent so that the extent
	     does not cover any of the range between closure->start and
	     closure->end.

	     Except that if the extent used to contain the gap (adjascent to
	     one of its endpoints) make it contain the soon-to-be-new gap
	     position as well - otherwise doing a deletion exactly at the front
	     of an extent which contained the gap would cause that extent to
	     no longer contain the gap.

	     This assumes that closure->{start,end} represent what will be the
	     new endpoints of the gap.

	     This really seems pretty awful; possibly some more information
	     needs to be passed down from del_range_1() giving us more of a
	     concrete idea of what actually is happening?
	   */
          if (max_start == extent_start (extent))
	    {
	      int gap_at_front = (extent_start (extent) == BUF_GPT (buf));
	      extent_start (extent) = (gap_at_front
				       ? closure->start
				       : min_end);
	    }
          else
	    {
	      int gap_at_back = (extent_end (extent) == (BUF_GPT (buf) +
							 BUF_GAP_SIZE (buf)));
	      extent_end (extent) = (gap_at_back
				     ? closure->end
				     : max_start);
	    }
#endif /* losing code */

        }
    }
  return 0;
}


/* copy/paste hooks */

static int
run_extent_copy_paste_internal (EXTENT e, int from, int to,
				Lisp_Object buffer,
				Lisp_Object prop)
{
  Lisp_Object extent;
  Lisp_Object copy_fn;
  XSETEXTENT (extent, e);
  copy_fn = Fextent_property (extent, prop);
  if (!NILP (copy_fn))
    {
      Lisp_Object flag;
      struct gcpro gcpro1, gcpro2, gcpro3;
      int speccount = specpdl_depth ();
      if (!BUFFERP (buffer)) abort ();
      GCPRO3 (extent, copy_fn, buffer);
      record_unwind_protect (Fset_buffer, Fcurrent_buffer ());
      set_buffer_internal (XBUFFER (buffer));
      flag = call3 (copy_fn, extent, make_number (from), make_number (to));
      unbind_to (speccount, Qnil);
      UNGCPRO;
      if (NILP (flag) || EXTENT_DESTROYED_P (XEXTENT (extent)))
	return 0;
    }
  return 1;
}

static int
run_extent_copy_function (EXTENT e, int from, int to)
{
  return run_extent_copy_paste_internal (e, from, to, extent_buffer (e),
					 Qcopy_function);
}

static int
run_extent_paste_function (EXTENT e, int from, int to, struct buffer *b)
{
  Lisp_Object buf;
  XSETR (buf, Lisp_Buffer, b);
  return run_extent_copy_paste_internal (e, from, to, buf, Qpaste_function);
}


/* replicating extents */
static int replicate_extents_mapper (EXTENT extent, void *arg);

Lisp_Object 
replicate_extents (int opoint, int length, struct buffer *buf)
{
  if (NILP (buf->extents))
    return Qnil;
  else if (!EXTENTP (buf->extents))
    abort();
  else
    {
      struct replicate_extents_arg closure;
      closure.from = opoint;
      closure.length = length;
      closure.head = Qnil;
      closure.buf = buf;
      closure.nconc_cell = Qzero;
      map_extents (opoint, opoint + length, 0, replicate_extents_mapper, 
                   (void *) &closure, buf, 0);
      return closure.head;
    }
}

static int
replicate_extents_mapper (EXTENT extent, void *arg)
{
  struct replicate_extents_arg *closure = 
    (struct replicate_extents_arg *) arg;
  Lisp_Object head = closure->head;
  Lisp_Object tail = closure->nconc_cell;
  int start = (extent_index_to_buffer_pos (extent_start (extent), closure->buf)
	       - closure->from);
  int end = (extent_index_to_buffer_pos (extent_end (extent), closure->buf)
	     - closure->from);
  
  if (inside_undo || EXTENT_DUPLICABLE_P (extent))
    {
      start = max (start, 0);
      end = min (end, closure->length);

      /* this test should probably never fail, but I'm a bit confused at the
	 moment */
      if ((start < end) || 
	  ((start == end) && (extent_start (extent) == extent_end (extent))))
	{
	  /* Run the copy-function to give an extent the option of
	     not being copied into the string (or kill ring.)
	   */
	  if (EXTENT_DUPLICABLE_P (extent) &&
	      !run_extent_copy_function (extent,
					 start + closure->from,
					 end + closure->from))
	    return 0;

	  /* Make a dup and put it on the string-extent-data. */
	  {
	    Lisp_Object new_cell;   
	    Lisp_Object replica;
	    DUP dup;

	    XSETEXTENT (replica, extent);
	    dup = make_extent_replica (replica, start, end);
	    XSETEXTENT (replica, dup);
	    new_cell = Fcons (replica, Qnil);

	    if (NILP (head))
	      closure->head = new_cell;
	    else
	      Fsetcdr (tail, new_cell);
	    closure->nconc_cell = new_cell;
	  }
	}
    }  
  return 0;
}


/* Insert an extent, usually from the dup_list of a string which
   has just been inserted.
   This code does not handle the case of undo.
   */
static Lisp_Object
insert_extent (EXTENT extent, int new_start, int new_end,
	       struct buffer *buf, int run_hooks)
{

  Lisp_Object tmp;
  if (!BUFFERP (extent_buffer (extent)))
    goto copy_it;
  if (XBUFFER (extent_buffer (extent)) != buf)
    goto copy_it;

  if (EXTENT_DETACHED_P (extent))
    {
      if (run_hooks &&
	  !run_extent_paste_function (extent, new_start, new_end, buf))
	/* The paste-function said don't re-attach this extent here. */
	return Qnil;
      else
	update_extent_1 (extent, new_start, new_end, 1, buf);
    }
  else if (extent_index_to_buffer_pos (extent_end (extent), buf)
	   < new_start)
    goto copy_it;
  else if (extent_index_to_buffer_pos (extent_start (extent), buf)
	   > new_end)
    goto copy_it;
  else
    {
      int start =
	extent_index_to_buffer_pos (extent_start (extent), buf);
      int end =
	extent_index_to_buffer_pos (extent_end (extent), buf);
      new_start = min(start, new_start);
      new_end = max(end, new_end);
      if (start != new_start || end != new_end)
	update_extent_1 (extent, new_start, new_end, 1, buf);   
    }
  XSETEXTENT (tmp, extent);
  return tmp;

 copy_it:
  if (run_hooks &&
      !run_extent_paste_function (extent, new_start, new_end, buf))
    /* The paste-function said don't attach a copy of the extent here. */
    return Qnil;
  else
    {
      Lisp_Object buffer;
      XSETR (buffer, Lisp_Buffer, buf);
      return copy_extent_internal (extent, new_start, new_end, buffer);
    }
}


/* We have just inserted a string of size "length" at "opoint"; the string
   was taken from an original string at position pos.  We have the contents
   of the extents slot of the original string on hand, and we now need
   to do "whatever" is necessary to make the extents in the buffer be
   correctly updated. If there are no extents on the string, then that is
   nothing. If there are extents and we are inside_undo, then the extents
   argument is taken as revealed truth and the state of the buffer extents
   must be restored so that the function above would return the same string
   extents if this corresponding string were to be deleted. If we are not
   inside undo then we just splice in those extents that correspond to
   deleted extents.

   Note: At the moment we ONLY handle the case of the dup_list argument
   be a list of extent_replicas.
   */

void 
splice_in_extent_replicas (int opoint, int length, int pos,
                           Lisp_Object dup_list, struct buffer *buf)
{
  if ((!NILP(buf->extents) && (!EXTENTP (buf->extents))) || 
      (!NILP (dup_list) && (!CONSP (dup_list))))
    abort();
  if (NILP (dup_list))   
    return;
  else if (inside_undo)
    {
      Lisp_Object tail;
      int base_start = opoint;
      int base_end = opoint + length;

      for (tail = dup_list; !NILP (tail); tail = Fcdr (tail))
        {
          Lisp_Object current_replica = Fcar (tail);
          /* only process replicas at the moment */
          if (EXTENT_REPLICA_P (current_replica)) 
            {
              DUP dup = XDUP (current_replica);
              EXTENT extent = XEXTENT (dup_extent (dup));
              int new_start = base_start + dup_start (dup) - pos;
              int new_end = base_start + dup_end (dup) - pos;

              if (EXTENT_DESTROYED_P (extent))
                continue;

              /* paranoid testing which will go away eventually */
              if ((!EXTENTP (dup_extent (dup))) ||
                  (XBUFFER (extent_buffer (extent)) != buf))
                abort ();
              
              if (EXTENT_DETACHED_P (extent))
		update_extent_1 (extent, new_start, new_end, 1, buf);
              else if
                ((extent_index_to_buffer_pos (extent_start (extent), buf)
		  > base_end) ||
		 (extent_index_to_buffer_pos (extent_end (extent), buf)
		  < base_start))
		  /* I18N3 */
                error (GETTEXT ("extent 0x%x is all fouled up wrt. dup 0x%x"),
                       (int) extent, (int) dup);
              else
                {
                  /* this should be safe because if you delete some text
                     all of the extents that were effected stay in the
                     same order, so when you restore what was removed
                     they should still be in the correct order */
		  int from = BUFFER_POS_TO_START_INDEX (new_start, buf, extent);
		  int to = BUFFER_POS_TO_END_INDEX (new_end, buf, extent);
                  extent_start (extent) = min (from, extent_start (extent));
                  extent_end (extent) = max (to, extent_end (extent));

		  /* #### Kludge!!!  If the extent is (open,open) and the
		     endpoints are the same, the calls to BUFFER_POS_TO_* will
		     give it the bogus endpoints (N,N-1).  So, hack it out...
		   */
		  if (new_start == new_end)
		    extent_end (extent) = extent_start (extent);

		  soe_push (extent, buf);
                }
            }
        }
    }
  else
    {
      Lisp_Object tail;
      int base_start = opoint;

      for (tail = dup_list; !NILP (tail); tail = Fcdr (tail))
        {
          Lisp_Object current_replica = Fcar (tail);
          /* only process replicas at the moment */
          if (EXTENT_REPLICA_P (current_replica)) 
            {
              DUP dup = XDUP (current_replica);
              EXTENT extent = XEXTENT (dup_extent (dup));
	      int new_start = dup_start (dup) - pos;
	      int new_end = dup_end (dup) - pos;

	      /* The extra comparisons defend against set-string-extent-data
		 and support insert_from_string.  */
	      if (new_start < 0)  new_start = 0;
	      if (new_end > length) new_end = length;
	      if (new_end <= new_start)  continue;

              new_start += base_start;
              new_end += base_start;

              /* paranoid testing which will go away eventually */
              if ((!EXTENTP (dup_extent (dup))))
                abort ();

              if (EXTENT_DESTROYED_P (extent))
		continue;


#ifdef ENERGIZE
	      /* Energize extents like toplevel-forms can only be pasted 
	         in the buffer they come from.  This should be parametrized
	         in the generic extent objects.  Right now just silently
	         skip the extents if it's not from the same buffer.
	       */
	      if (XBUFFER (extent_buffer (extent)) != buf
		  && energize_extent_data (extent))
		continue;
#endif

	      /* If this is a `unique' extent, and it is currently attached
		 somewhere other than here (non-overlapping), then don't copy
		 it (that's what `unique' means).  If however it is detached,
		 or if we are inserting inside/adjascent to the original
		 extent, then insert_extent() will simply reattach it, which
		 is what we want.
	       */
	      if (EXTENT_UNIQUE_P (extent)
		  && !EXTENT_DETACHED_P (extent)
		  && (XBUFFER (extent_buffer (extent)) != buf
		      || (extent_index_to_buffer_pos (extent_start (extent),
						      buf)
			  > new_end)
		      || (extent_index_to_buffer_pos (extent_end (extent), buf)
			  < new_start)))
		continue;

	      insert_extent (extent, new_start, new_end, buf, 1);
            }
        }
    }
}


/* Merge dup_list[i] into a list of replicas -- if a dup
   in listi "overlaps at the end" matches a dup from listi+1 that "overlaps
   at the beginning", merge them into one contiguous dup in the returned
   list. It is weird and probably bogus if a "detached dup" doesn't merge 
   entirely, but it isn't an error.
   
   This code also handles construction of a dup_list for Fsubstring,
   by handing in a single list with a possibly negative offset and
   a length which is possibly less than the length of the original string.
   */
   
static void merge_replicas_concating_mapper (CONST void *, void *, void *);
static void merge_replicas_pruning_mapper  (CONST void *, void *, void *);

static Lisp_Object 
merge_replicas_internal (int number_of_lists,
			 struct merge_replicas_struct *vec,
			 int shiftp)
{
  c_hashtable table = 0;
  Lisp_Object cells_vec[2];
  int i;
  int total_length;
  int clip_parts = !shiftp;

  cells_vec[0] = Qnil;
  cells_vec[1] = Qnil;

  total_length = 0;
  for (i = 0; i < number_of_lists; i++)
    total_length += vec[i].entry_length;

  for (i = 0; i < number_of_lists; i++)
    {
      Lisp_Object dup_list = vec[i].dup_list;
      int offset = vec[i].entry_offset;
      int length = vec[i].entry_length;

      if (!NILP (dup_list))
        {
          if (!table)
            table = make_hashtable (10);
          add_to_replicas_lists (table, dup_list,
				 offset, length,
				 clip_parts, total_length,
				 cells_vec);
        }
    }

  if (table)
    {
      maphash (merge_replicas_pruning_mapper,   table, (void*)table);
      maphash (merge_replicas_concating_mapper, table, (void*)&(cells_vec[0]));
      free_hashtable (table);
    }
  return (cells_vec[0]);
}


Lisp_Object 
merge_replicas (int number_of_lists, struct merge_replicas_struct *vec)
{
  return merge_replicas_internal (number_of_lists, vec, 0);
}


/* Like merge_replicas, but operates on just one dup_list,
   applying an offset and clipping the results to [0..length).
   The offset is non-positive if the caller is Fsubstring.
   */
Lisp_Object shift_replicas (Lisp_Object dup_list, int offset, int length)
{
  struct merge_replicas_struct mr_struct;
  mr_struct.dup_list = dup_list;
  mr_struct.entry_offset = offset;
  mr_struct.entry_length = length;
  return merge_replicas_internal (1, &mr_struct, 1);
}


static void 
add_to_replicas_lists (c_hashtable table,
		       Lisp_Object dup_list,
		       int offset, int length,
		       int clip_parts, int total_length,
		       Lisp_Object *cells_vec)
{
  Lisp_Object tail;
  for (tail = dup_list; !NILP (tail); tail = Fcdr(tail))
    {
      Lisp_Object current_replica = Fcar (tail);
      if (EXTENT_REPLICA_P (current_replica)) 
        {
          DUP dup = XDUP (current_replica);
	  int new_start = dup_start (dup);
	  int new_end = dup_end (dup);
          EXTENT extent = XEXTENT (dup_extent (dup));
          Lisp_Object pre_existing_cell;
          Lisp_Object tmp;
          DUP new_dup;
	  CONST void *vval;

	  if (clip_parts)
	    {
	      /* The extra clipping defends against set-string-extent-data.
		 It is not necessary in shift_replicas, since the
		 check against total_length still applies below.
	       */
	      if (new_start > length)  new_start = length;
	      if (new_end > length)    new_end = length;
	    }

	  new_start += offset;
	  new_end += offset;

	  /* These checks are needed because of Fsubstring, and are a good
	     idea in any case:
	     */
	  if (new_end <= 0)
	    continue;
	  if (new_start >= total_length)
	    continue;
	  if (new_start <= 0)
	    new_start = 0;
	  if (new_end >= total_length)
	    new_end = total_length;

          if (EXTENT_DESTROYED_P (extent))
            continue;

          new_dup = make_extent_replica (dup_extent (dup), new_start, new_end);
   
          /* paranoid testing which will go away eventually */
          if (!EXTENTP (dup_extent (dup)))
            abort ();
              
          if (!gethash ((void *) extent, table, &vval))
            pre_existing_cell = Qnil;
	  else
	    VOID_TO_LISP (pre_existing_cell, vval);
   
          XSETEXTENT (tmp, new_dup);
          tmp = Fcons (tmp, pre_existing_cell);
          puthash (extent, LISP_TO_VOID (tmp), table);
        }
#if 0
      else
	{
	  /* Save away misc. trash in the order encountered. */
	  Lisp_Object cell;
	  cell = Fcons (current_replica, Qnil);
	  if (NILP (cells_vec[0]))
	    cells_vec[0] = cell;
	  else
	    nconc2 (cells_vec[1], cell);
	  cells_vec[1] = cell;
	}
#endif
    }
}

static void
merge_replicas_concating_mapper (CONST void *key, void *contents, void *arg)
{
  Lisp_Object extent_cell;
  Lisp_Object *cells_vec = (Lisp_Object *) arg;
  VOID_TO_LISP (extent_cell, contents);

  if (NILP (cells_vec[0]))
    cells_vec[0] = extent_cell;
  else
    nconc2 (cells_vec[1], extent_cell);

  cells_vec[1] = extent_cell;
  return;
}

static int 
mrp_pred (Lisp_Object x, Lisp_Object y, Lisp_Object dummy)
{
  DUP dup1 = XDUP(x);
  DUP dup2 = XDUP(y);

  if (dup_start (dup1) < dup_start (dup2))
    return 1;
  else if (dup_start (dup1) == dup_start (dup2))
    {
      if (dup_end (dup1) <= dup_end (dup2))
        return 1;
      else
        return -1;
    }
  return -1;
}
   
static void
merge_replicas_pruning_mapper (CONST void *key, void *contents, void *arg)
{
  Lisp_Object dup_list;
  c_hashtable table = (c_hashtable) arg;
  VOID_TO_LISP (dup_list, contents);

  if (NILP (dup_list))
    return;
  if (NILP (Fcdr (dup_list)))
    return;
   
  /* sort and merge the dup_list */
  dup_list = list_sort (dup_list, Qnil, mrp_pred);
  {
    Lisp_Object current = dup_list;
    Lisp_Object tail = Fcdr (dup_list);
    DUP current_dup = XDUP (Fcar (current));

    while (!NILP (tail))
      {
        DUP tail_dup = XDUP(Fcar (tail));   

        if (dup_start (tail_dup) <= dup_end (current_dup) - 1)
          {
            dup_end (current_dup) = max (dup_end (tail_dup),
					 dup_end (current_dup));
            Fsetcdr (current, Fcdr (tail));
          }
        else
          {
            current = tail;
            current_dup = XDUP (Fcar (current));
          }
   
        tail = Fcdr (tail);
      }
  }
   
  /* now put back the munged list */
  puthash (key, LISP_TO_VOID (dup_list), table);
}

/* Checklist for sanity checking:
   - {kill, yank, copy} at {open, closed} {start, end} of {writable, read-only} extent
   - {kill, copy} & yank {once, repeatedly} duplicable extent in {same, different} buffer
 */


/* Text properties
   Originally this stuff was implemented in lisp (all of the functionality
   exists to make that possible) but speed was a problem.
 */

Lisp_Object Qtext_prop;
Lisp_Object Qtext_prop_extent_paste_function;

struct put_text_prop_arg {
  Lisp_Object prop, value;	/* The property and value we are storing */
  int start, end;		/* The region into which we are storing it */
  struct buffer *buffer;
  int changed_p;		/* Output: whether we have modified anything */
  Lisp_Object the_extent;	/* Our chosen extent; this is used for
				   communication between subsequent passes. */
};

static int
put_text_prop_mapper (EXTENT e, void *arg)
{
  struct put_text_prop_arg *closure = (struct put_text_prop_arg *) arg;

  Lisp_Object value = closure->value;
  int e_start, e_end;
  int start = closure->start;
  int end   = closure->end;
  Lisp_Object extent, e_val;
  XSETEXTENT (extent, e);
  e_start = XINT (Fextent_start_position (extent));
  e_end   = XINT (Fextent_end_position (extent));
  e_val = Fextent_property (extent, closure->prop);

  if (NILP (e_val) ||
      NILP (Fextent_property (extent, Qtext_prop)))
    {
      /* It's not one of ours, or it's not for this property; do nothing. */
      ;
    }
  else if (!NILP (value) &&
	   NILP (closure->the_extent) &&
	   EQ (value, e_val))
    {
      /* We want there to be an extent here at the end, and we haven't picked
	 one yet, so use this one.  Extend it as necessary.  We only reuse an
	 extent which has an EQ value for the prop in question to avoid
	 side-effecting the kill ring (that is, we never change the property
	 on an extent after it has been created.)
       */
      if (e_start != start || e_end != end)
	{
	  Fset_extent_endpoints (extent,
				 make_number (min (e_start, start)),
				 make_number (max (e_end, end)));
	  closure->changed_p = 1;
	}
      closure->the_extent = extent;
    }

  /* Even if we're adding a prop, at this point, we want all other extents of
     this prop to go away (as now they overlap.)  So the theory here is that,
     when we are adding a prop to a region that has multiple (disjoint)
     occurences of that prop in it already, we pick one of those and extend
     it, and remove the others.
   */

  else if (EQ (extent, closure->the_extent))
    {
      /* just in case map-extents hits it again (does that happen?) */
      ;
    }
  else if (e_start >= start && e_end <= end)
    {
      /* Extent is contained in region; remove it.  Don't destroy or modify
	 it, because we don't want to change the attributes pointed to by the
	 duplicates in the kill ring.
       */
      Fdetach_extent (extent);
      closure->changed_p = 1;
    }
  else if (!NILP (closure->the_extent) &&
	   EQ (value, e_val) &&
	   e_start <= end &&
	   e_end >= start)
    {
      /* This extent overlaps, and has the same prop/value as the extent we've
	 decided to reuse, so we can remove this existing extent as well (the
	 whole thing, even the part outside of the region) and extend
	 the-extent to cover it, resulting in the minimum number of extents in
	 the buffer.
       */
      int the_start = XINT (Fextent_start_position (closure->the_extent));
      int the_end   = XINT (Fextent_end_position   (closure->the_extent));
      if (e_start != the_start &&  /* note AND not OR */
	  e_end   != the_end)
	{
	  Fset_extent_endpoints (closure->the_extent,
				 make_number (min (the_start, e_start)),
				 make_number (max (the_end,   e_end)));
	  closure->changed_p = 1;
	}
      Fdetach_extent (extent);
    }
/*  else if (XINT (Fextent_end_position (extent)) <= end) */
  else if (e_end <= end)
    {
      /* Extent begins before start but ends before end, so we can just
	 decrease its end position.
       */
/*      if (XINT (Fextent_start_position (extent)) != e_start || */
/*	  XINT (Fextent_end_position   (extent)) != start) */
      if (e_end != start)
	{
	  Fset_extent_endpoints (extent,
				 make_number (e_start),
				 make_number (start));
	  closure->changed_p = 1;
	}
    }
/*  else if (XINT (Fextent_start_position (extent)) >= start) */
  else if (e_start >= start)
    {
      /* Extent ends after end but begins after start, so we can just
	 increase its start position.
       */
/*      if (XINT (Fextent_start_position (extent)) != end || */
/* 	  XINT (Fextent_end_position   (extent)) != e_end) */
      if (e_start != end)
	{
	  Fset_extent_endpoints (extent,
				 make_number (end),
				 make_number (e_end));
	  closure->changed_p = 1;
	}
    }
  else
    {
      /* Otherwise, `extent' straddles the region.  We need to split it.
       */
      Fset_extent_endpoints (extent,
			     make_number (e_start), make_number (start));
      extent = Fcopy_extent (extent, Qnil);
      Fset_extent_endpoints (extent,
			     make_number (end), make_number (e_end));
      closure->changed_p = 1;
    }

  return 0;  /* to continue mapping. */
}

static int
put_text_prop (int start, int end, struct buffer *b,
	       Lisp_Object prop, Lisp_Object value,
	       int duplicable_p)
{
  struct put_text_prop_arg closure;
  if (start == end)   /* There are no characters in the region. */
    return 0;

  if (start > end)
    {
      int swap = end;
      end = start;
      start = swap;
    }

  closure.prop = prop;
  closure.value = value;
  closure.start = start;
  closure.end = end;
  closure.buffer = b;
  closure.changed_p = 0;
  closure.the_extent = Qnil;

  /* Map over one character past each end of the region just to make sure
     that we hit all relevant extents.  Maybe this isn't strictly necessary
     with judicious use of the closed_end arg, but it doesn't hurt.
   */
  map_extents (max (1, (start - 1)),
	       min (BUF_ZV (b), (end + 1)),
	       0, put_text_prop_mapper,
	       (void *) &closure,
	       b, 1);

  /* If we made it through the loop without reusing an extent
     (and we want there to be one) make it now.
   */
  if (!NILP (value) && NILP (closure.the_extent))
    {
      Lisp_Object buffer, extent;
      XSETR (buffer, Lisp_Buffer, b);
      extent = make_extent_internal (start, end, buffer);
      closure.changed_p = 1;
      Fset_extent_property (extent, Qtext_prop, prop);
      Fset_extent_property (extent, prop, value);
      if (duplicable_p)
	{
	  EXTENT_DUPLICABLE_P (XEXTENT (extent)) = 1;
	  Fset_extent_property (extent, Qpaste_function,
				Qtext_prop_extent_paste_function);
	}
    }

  return closure.changed_p;
}

DEFUN ("put-text-property", Fput_text_property, Sput_text_property, 4, 5, 0,
 "Adds the given property/value to all characters in the specified region.\n\
The property is conceptually attached to the characters rather than the\n\
region.  The properties are copied when the characters are copied/pasted.")
     (start, end, prop, value, buffer)
     Lisp_Object start, end, prop, value, buffer;
{
  CHECK_FIXNUM_COERCE_MARKER (start, 0);
  CHECK_FIXNUM_COERCE_MARKER (end, 0);
  CHECK_SYMBOL (prop, 0);
  if (!NILP (buffer))
    CHECK_BUFFER (buffer, 0);

  put_text_prop (XINT (start), XINT (end),
		 (NILP (buffer) ? current_buffer : XBUFFER (buffer)),
		 prop, value, 1);
  return prop;
}

DEFUN ("put-nonduplicable-text-property", Fput_nonduplicable_text_property,
       Sput_nonduplicable_text_property, 4, 5, 0,
 "Adds the given property/value to all characters in the specified region.\n\
The property is conceptually attached to the characters rather than the\n\
region, however the properties will not be copied the characters are copied.")
     (start, end, prop, value, buffer)
     Lisp_Object start, end, prop, value, buffer;
{
  CHECK_FIXNUM_COERCE_MARKER (start, 0);
  CHECK_FIXNUM_COERCE_MARKER (end, 0);
  CHECK_SYMBOL (prop, 0);
  if (!NILP (buffer))
    CHECK_BUFFER (buffer, 0);

  put_text_prop (XINT (start), XINT (end),
		 (NILP (buffer) ? current_buffer : XBUFFER (buffer)),
		 prop, value, 0);
  return prop;
}

DEFUN ("add-text-properties", Fadd_text_properties, Sadd_text_properties,
       3, 4, 0,
       "Add properties to the characters from START to END.\n\
The third argument PROPS is a property list specifying the property values\n\
to add.  The optional fourth argument, OBJECT, is the buffer containing the\n\
text.  Returns t if any property was changed, nil otherwise.")
	(start, end, props, buffer)
	Lisp_Object start, end, props, buffer;
{
  int changed = 0;
  int s, e;
  struct buffer *b;
  CHECK_FIXNUM_COERCE_MARKER (start, 0);
  CHECK_FIXNUM_COERCE_MARKER (end, 0);
  CHECK_LIST (props, 0);
  if (!NILP (buffer))
    CHECK_BUFFER (buffer, 0);
  s = XINT (start);
  e = XINT (end);
  b = (NILP (buffer) ? current_buffer : XBUFFER (buffer));
  for (; !NILP (props); props = Fcdr (Fcdr (props)))
    {
      Lisp_Object prop = XCONS (props)->car;
      Lisp_Object value = Fcar (XCONS (props)->cdr);
      CHECK_SYMBOL (prop, 0);
      changed |= put_text_prop (s, e, b, prop, value, 1);
    }
  return (changed ? Qt : Qnil);
}


DEFUN ("remove-text-properties", Fremove_text_properties,
       Sremove_text_properties, 3, 4, 0,
  "Remove the given properties from all characters in the specified region.\n\
PROPS should be a plist, but the values in that plist are ignored (treated\n\
as nil.)  Returns t if any property was changed, nil otherwise.")
	(start, end, props, buffer)
	Lisp_Object start, end, props, buffer;
{
  int changed = 0;
  int s, e;
  struct buffer *b;
  CHECK_FIXNUM_COERCE_MARKER (start, 0);
  CHECK_FIXNUM_COERCE_MARKER (end, 0);
  CHECK_LIST (props, 0);
  if (!NILP (buffer))
    CHECK_BUFFER (buffer, 0);
  s = XINT (start);
  e = XINT (end);
  b = (NILP (buffer) ? current_buffer : XBUFFER (buffer));
  for (; !NILP (props); props = Fcdr (Fcdr (props)))
    {
      Lisp_Object prop = XCONS (props)->car;
      CHECK_SYMBOL (prop, 0);
      changed |= put_text_prop (s, e, b, prop, Qnil, 1);
    }
  return (changed ? Qt : Qnil);
}

/* Whenever a text-prop extent is pasted into a buffer (via `yank' or `insert'
   or whatever) we attach the properties to the buffer by calling
   `put-text-property' instead of by simply alowing the extent to be copied or
   re-attached.  Then we return nil, telling the extents code not to attach it
   again.  By handing the insertion hackery in this way, we make kill/yank
   behave consistently iwth put-text-property and not fragment the extents
   (since text-prop extents must partition, not overlap.)

   The lisp implementation of this was probably fast enough, but since I moved
   the rest of the put-text-prop code here, I moved this as well for 
   completeness. 
 */
DEFUN ("text-prop-extent-paste-function", Ftext_prop_extent_paste_function,
       Stext_prop_extent_paste_function, 3, 3, 0,
       "Used as the `paste-function' property of `text-prop' extents.")
     (extent, from, to)
     Lisp_Object extent, from, to;
{
  Lisp_Object prop, val;
  prop = Fextent_property (extent, Qtext_prop);
  if (NILP (prop))
    signal_simple_error (GETTEXT ("internal error: no text-prop"), extent);
  val = Fextent_property (extent, prop);
  if (NILP (val))
    signal_simple_error_2 (GETTEXT ("internal error: no text-prop"),
			   extent, prop);
  Fput_text_property (from, to, prop, val, Qnil);
  return Qnil; /* important! */
}


void
syms_of_extents ()
{
  defsymbol (&Qextentp, "extentp");
  defsymbol (&Qextent_replica_p, "extent-replica-p");

  defsymbol (&Qdetached, "detached");
  defsymbol (&Qdestroyed, "destroyed");
  defsymbol (&Qbegin_glyph, "begin-glyph");
  defsymbol (&Qend_glyph, "end-glyph");
  defsymbol (&Qstart_open, "start-open");
  defsymbol (&Qend_open, "end-open");
  defsymbol (&Qstart_closed, "start-closed");
  defsymbol (&Qend_closed, "end-closed");
  defsymbol (&Qread_only, "read-only");
  defsymbol (&Qhighlight, "highlight");
  defsymbol (&Qunique, "unique");
  defsymbol (&Qduplicable, "duplicable");
  defsymbol (&Qinvisible, "invisible");
  defsymbol (&Qpriority, "priority");

  defsymbol (&Qglyph_layout, "glyph-layout");
  defsymbol (&Qoutside_margin, "outside-margin");
  defsymbol (&Qinside_margin, "inside-margin");
  defsymbol (&Qwhitespace, "whitespace");
  defsymbol (&Qtext, "text");

  defsymbol (&Qglyph_invisible, "glyph-invisible");

  defsymbol (&Qpaste_function, "paste-function");
  defsymbol (&Qcopy_function,  "copy-function");

  defsymbol (&Qtext_prop, "text-prop");
  defsymbol (&Qtext_prop_extent_paste_function,
	     "text-prop-extent-paste-function");

  defsubr(&Sextentp);
  defsubr(&Sextent_length);
  defsubr(&Sextent_start_position);
  defsubr(&Sextent_end_position);
  defsubr(&Sextent_buffer);
  defsubr(&Smap_extents);
  defsubr(&Smap_extent_children);
  defsubr(&Sextent_in_region_p);
  defsubr(&Shighlight_extent);
  defsubr(&Sforce_highlight_extent);
  defsubr(&Sextent_at);

  defsubr(&Smake_extent);
  defsubr(&Snext_extent);
  defsubr(&Scopy_extent);
  defsubr(&Sinsert_extent);
  defsubr(&Sdelete_extent);
  defsubr(&Sdetach_extent);
  defsubr(&Sset_extent_endpoints);
  defsubr(&Sextent_property);
  defsubr(&Sset_extent_property);
  defsubr(&Sextent_properties);
  defsubr(&Sset_extent_begin_glyph);
  defsubr(&Sset_extent_end_glyph);
  defsubr(&Sextent_begin_glyph);
  defsubr(&Sextent_end_glyph);
  defsubr(&Sset_extent_layout);
  defsubr(&Sextent_layout);
  defsubr(&Sextent_priority);
  defsubr(&Sset_extent_priority);
  defsubr(&Sstring_extent_data);
  defsubr(&Sset_string_extent_data);
  defsubr(&Smake_extent_replica);
  defsubr(&Sextent_replica_extent);
  defsubr(&Sextent_replica_start);
  defsubr(&Sextent_replica_end);
#if 0
  defsubr(&Snext_e_extent);
  defsubr(&Sstack_of_extents);
#endif

  defsubr(&Sput_text_property);
  defsubr(&Sput_nonduplicable_text_property);
  defsubr(&Sadd_text_properties);
  defsubr(&Sremove_text_properties);
  defsubr(&Stext_prop_extent_paste_function);

  staticpro (&Vlast_highlighted_extent);
  Vlast_highlighted_extent = Qnil;

  staticpro (&Vextent_fragment_buffer);
  Vextent_fragment_buffer = Qnil;

  init_extent_fragment ();
}


Lisp_Object Vthis_is_a_dead_extent_replica;

void
init_extents_once ()
{
  XSETEXTENT (Vthis_is_a_dead_extent_replica, make_extent ());
  staticpro (&Vthis_is_a_dead_extent_replica);
}
