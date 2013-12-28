/* "Face" primitives
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

#include "config.h"

#include <sys/types.h>
#include <sys/stat.h>

/* Display Context for the icons */ 
#include <X11/Intrinsic.h>
#include <X11/IntrinsicP.h>
#include <X11/CoreP.h>
#include <X11/StringDefs.h>
#include <X11/Xmu/Drawing.h>
#ifdef USG	/* ####KLUDGE for Solaris 2.2 and up */
#undef USG
#include <X11/Xos.h>
#define USG
#else
#include <X11/Xos.h>
#endif

#ifdef HAVE_XPM
#include <X11/xpm.h>
#endif

#include "lisp.h"

#include "xterm.h"
#include "xobjs.h"
#include "buffer.h"
#include "extents.h"
#include "screen.h"
#include "window.h"
#include "indent.h"

#include "dispmisc.h"
#include "dispextern.h"

#include "faces.h"
#include "hash.h"

#define MAX_CACHED_FACES	30

Lisp_Object Qfont, Qforeground, Qbackground, Qunderline, Qbackground_pixmap;
Lisp_Object Qmake_screen_initial_faces;

/* The priority of the mouse-highlighting attributes, for extent-merging.
 */
int mouse_highlight_priority;

extern int extent_cache_invalid;

void ensure_face_ready (struct screen* s, int f);
static void compute_screen_line_height (struct screen *s);


#ifdef LISP_FACE  /* in progress... */

static Lisp_Object mark_face (Lisp_Object, void (*) (Lisp_Object));
static void print_face (Lisp_Object, Lisp_Object, int);
static int sizeof_face (void *f) { return (sizeof (struct face)); }
/* #### add face_equal */
DEFINE_LRECORD_IMPLEMENTATION (lrecord_face, mark_face, print_face, 
                               0, sizeof_face, 0);

#define FACEP(obj) RECORD_TYPEP((obj), lrecord_face)
#define XFACE(obj) ((struct face *) (XPNTR (obj)))
#define CHECK_FACE(x, i) \
  { if (!FACEP((x))) wrong_type_argument (Qfacep, (x)); }

static Lisp_Object
mark_face (Lisp_Object obj, void (*markobj) (Lisp_Object))
{
  struct face *face = XFACE (obj);
  ((markobj) (face->foreground));
  ((markobj) (face->background));
  ((markobj) (face->back_pixmap));
  return (face->font);
}

static void
print_face (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  struct face *face = XFACE (obj);
  char buf[50];
  if (print_readably)
    error ("printing unreadable object #<face 0x%x>", face->header.uid);
  sprintf (buf, "#<internal-face 0x%x>", face->header.uid);
  write_string_1 (buf, -1, printcharfun);
}


/* Caching functions for faces */
static Lisp_Object Vface_cache;

/* This is a hash function used in the lisp weak hash table Vface_cache.
   It only works on face objects (that's all that goes in that table.)
 */
static unsigned long
face_hash_function (const void *arg)
{
  Lisp_Object lface;
  struct face *face;
  CVOID_TO_LISP (lface, arg);
  if (!FACEP (lface))
    abort ();
  face = XFACE (lface);
#define slot_unless_nil(extractor, slot1, slot2) \
  (NILP(face->slot1) ? 0 : ((unsigned long)(extractor(face->slot1)->slot2)))
  return (slot_unless_nil (XFONT, font, font) ^
	  slot_unless_nil (XPIXEL, foreground, color.pixel) ^
	  slot_unless_nil (XPIXEL, background, color.pixel) ^
	  slot_unless_nil (XPIXMAP, back_pixmap, pixmap) ^
	  (unsigned long) face->underline);
#undef slot_unless_nil
}

static int
face_eql (const void *arg1, const void *arg2)
{
  if (arg1 == arg2) 
    return 1;
  else
    {
      Lisp_Object f1, d2;
      const struct face *face1, face2;
      CVOID_TO_LISP (f1, arg1);
      CVOID_TO_LISP (f2, arg2);
      if (!FACEP (f1)) abort ();	/* #### debugging */
      if (!FACEP (f2)) abort ();
      face1 = XFACE (f1);
      face2 = XFACE (f2);

      /* #### Might get better caching if this compared the underlying
	 X objects directly instead of comparing the lisp objects. */
      if (EQ (face1->font, face2->font) &&
          EQ (face1->foreground, face2->foreground) &&
          EQ (face1->background, face2->background) &&
          EQ (face1->back_pixmap, face2->back_pixmap) &&
          (face1->underline == face2->underline))
        return 1;
      else
        return 0;
    }
}


static Lisp_Object *Vface_cache_tmp;

static void 
init_face_cache ()
{
  struct face *f;
  Vface_cache = make_weak_hashtable (50, face_hash_function, face_eql);
  f = (struct face *) xmalloc (sizeof (struct face));
  XSETFACE (Vface_cache_tmp, f);
}


static struct face *
get_cached_face (struct face* face)
{
  /* Copy the given face* into the face* which underlies our dummy face
     lisp object (since we need to pass a lisp object to gethash.) */
  struct face *tmp = XFACE (Vface_cache_tmp);
  *tmp = *face;
  return Fgethash (Vface_cache_tmp, Vface_cache, Qnil);
}

static void
cache_face (struct face *face)
{
  puthash (face, face, face_cache);
}


static void
invalidate_face_cache (struct screen *s, int deleting_screen_p)
{
  for (tail = Vscreen_list; CONSP (tail); tail = XCONS (tail)->cdr)
    SET_SCREEN_GARBAGED (XSCREEN (XCONS (tail)->car));

  if (! deleting_screen_p)
    compute_screen_line_height (s);
}


#else /* !LISP_FACE */

/* Caching functions for faces */
static c_hashtable face_cache;
static c_hashtable face_cache_pending_flush;

static int face_cache_inited;
int face_cache_invalid;

static void hash_faces_in_windows (Lisp_Object window, c_hashtable face_hash);
static void hash_faces_in_window (Lisp_Object window, c_hashtable face_hash);


static struct face *allocate_face ();

static unsigned long
face_hash_function (const void *arg)
{
  const struct face* face = arg;
#define slot_unless_nil(extractor, slot1, slot2) \
  (NILP(face->slot1) ? 0 : ((unsigned long)(extractor(face->slot1)->slot2)))
  return (slot_unless_nil (XFONT, font, font) ^
	  slot_unless_nil (XPIXEL, foreground, color.pixel) ^
	  slot_unless_nil (XPIXEL, background, color.pixel) ^
	  slot_unless_nil (XPIXMAP, back_pixmap, pixmap) ^
	  (unsigned long) face->underline);
#undef slot_unless_nil
}

static int
face_eql (const void *arg1, const void *arg2)
{
  if (arg1 == arg2) 
    return 1;
  else
    {
      const struct face* face1 = arg1;
      const struct face* face2 = arg2;

      /* #### Might get better caching if this compared the underlying
	 X objects directly instead of comparing the lisp objects. */
      if (EQ (face1->font, face2->font) &&
          EQ (face1->foreground, face2->foreground) &&
          EQ (face1->background, face2->background) &&
          EQ (face1->back_pixmap, face2->back_pixmap) &&
          (face1->underline == face2->underline))
        return 1;
      else
        return 0;
    }
}

static void 
init_face_cache ()
{
  face_cache_inited = 1;
  face_cache = make_general_hashtable (MAX_CACHED_FACES,
				       face_hash_function, face_eql);
}

static struct face*
get_cached_face (struct face* face)
{
  struct face *result;

  if (!face_cache_inited)
    init_face_cache ();

  if (gethash (face, face_cache, (const void **) &result))
    return result;
  else
    return 0;
}

static void
cache_face (struct face *face)
{
  if (!face_cache_inited)
    init_face_cache ();

  puthash (face, face, face_cache);
}


/* flush_face_cache is called from redisplay when face_cache_invalid is true.
   we set face_cache_invalid when we change a font or color of some face,
   meaning that the GCs need to be recomputed.  We don't just flush the
   cache at the time that we change a face because the redisplay structures
   still have pointers to the faces we want to free.
 */

static void
flush_face_cache_mapper (const void *hash_key, void *hash_contents,
                         void *closure)
{
  struct face *face = hash_contents;
  xfree (face);
}

void
flush_face_cache ()
{
  Display *dpy = x_current_display;
  if (! face_cache_inited)
    return;
  if (! face_cache_pending_flush)
    return;

  BLOCK_INPUT;
  maphash (flush_face_cache_mapper,
	   face_cache_pending_flush,
	   dpy);
  UNBLOCK_INPUT;
  free_hashtable (face_cache_pending_flush);
  face_cache_pending_flush = 0;
  face_cache_invalid = 0;
}

static void
invalidate_face_cache (struct screen *s, int deleting_screen_p)
{
  Lisp_Object tail;
  face_cache_invalid = 1;
  extent_cache_invalid = 1;

  for (tail = Vscreen_list; CONSP (tail); tail = XCONS (tail)->cdr)
    SET_SCREEN_GARBAGED (XSCREEN (XCONS (tail)->car));

  if (! deleting_screen_p)
    compute_screen_line_height (s);
  if (face_cache_pending_flush)
    return;
  face_cache_pending_flush = face_cache;
  init_face_cache ();
}

#endif /* !LISP_FACE */


/* Utility functions for faces */

#ifndef MAX
#define MAX(x,y) (((x)>(y))?(x):(y))
#endif

extern void EmacsScreenResize (Widget);

static void
compute_screen_line_height (struct screen *s)
{
  int old;
  struct Lisp_Font *font;
  if (! SCREEN_IS_X (s))
    return;
  if (NILP (SCREEN_DEFAULT_FONT (s)))
    return;
  font = XFONT (SCREEN_DEFAULT_FONT (s));

  old = s->display.x->text_height;
  s->display.x->text_height = font->ascent + font->descent;
  if (old != s->display.x->text_height)
    EmacsScreenResize (s->display.x->edit_widget);
}

static void
reset_face (struct screen* s, struct face* face)
{
  struct face* normal_face = &SCREEN_NORMAL_FACE (s);
  memset (face, 0, sizeof (struct face));
  face->foreground = normal_face->foreground;
  face->background = normal_face->background;
  face->back_pixmap = normal_face->back_pixmap;
  face->underline = normal_face->underline;
  face->font = normal_face->font;
}

static void 
merge_faces (struct face* from, struct face* to)
{
  if (!NILP (from->font))
    to->font = from->font;
  if (!NILP (from->foreground))
    to->foreground = from->foreground;
  if (!NILP (from->background))
    to->background = from->background;
  if (!NILP (from->back_pixmap))
    to->back_pixmap = from->back_pixmap;
  /* #### It's not possible for a non-underlined face to override an
     underlined one.  That is, one can't specify *no* underlining.
     Is that reasonable? */
  if (from->underline)
    to->underline = from->underline;
}


static void
merge_extent_face (struct screen* s, EXTENT extent, struct face* face)
{
  int index = extent->attr_index;
  
  /* no attribute: do not change anything */
  if (index <= 0)
    return;
    ensure_face_ready (s, index);

  /* do the merge */
  merge_faces (s->faces [index], face);
}



/* Get a face suitable for display (ie which has a GC) for a given
   set of attributes. */

static struct face *
get_display_face (struct screen* s, struct face* face)
{
  struct face *result;

  /* cache the default face */
  if (EQ (face->font, SCREEN_NORMAL_FACE (s).font)
      && EQ (face->foreground, SCREEN_NORMAL_FACE (s).foreground)
      && EQ (face->background, SCREEN_NORMAL_FACE (s).background)
      && EQ (face->back_pixmap, SCREEN_NORMAL_FACE (s).back_pixmap)
      && face->underline == SCREEN_NORMAL_FACE (s).underline)
    return &SCREEN_NORMAL_FACE (s);

  /* cache the modeline face */
  if (EQ (face->font, SCREEN_MODELINE_FACE (s).font)
      && EQ (face->foreground, SCREEN_MODELINE_FACE (s).foreground)
      && EQ (face->background, SCREEN_MODELINE_FACE (s).background)
      && EQ (face->back_pixmap, SCREEN_MODELINE_FACE (s).back_pixmap)
      && face->underline == SCREEN_MODELINE_FACE (s).underline)
    return &SCREEN_MODELINE_FACE (s);

  /* Is it cached */
  result = get_cached_face (face);
  if (result)
    return result;

  /* Add one to the cache */
  result = allocate_face ();

  result->font = face->font;
  result->foreground = face->foreground;
  result->background = face->background;
  result->back_pixmap = face->back_pixmap;
  result->underline = face->underline;

  cache_face (result);

  return result;
}


/* External entry points */

extern Lisp_Object Vlast_highlighted_extent;

/* Computes the face associated with an overlapping set of extents */
void 
setup_extent_fragment_face_ptr (struct screen *s, EXTENT_FRAGMENT extfrag)
{
  struct face face;
  EXTENT *vec = extfrag->extents_stack;
  int len = extfrag->number_of_extents;
  int i;
  
  /* optimize the default case. */
  if (len == 0)
    extfrag->fp = &SCREEN_NORMAL_FACE (s);
  else
    {
      EXTENT ebuf [200];
      EXTENT *extents;
      struct extent dummy_extent;

      /* Make a copy of the vector of extents... */
      if (len < (sizeof(ebuf) / (sizeof(EXTENT)) - 1))
	extents = ebuf; /* use a static buffer if it's small, for speed */
      else
	extents = (EXTENT *) alloca ((len * sizeof(EXTENT)) + 1);
      memcpy (extents, vec, len * sizeof(EXTENT));

      /* determine whether the last-highlighted-extent is present... */
      if (EXTENTP (Vlast_highlighted_extent))
	{
	  EXTENT lhe = XEXTENT (Vlast_highlighted_extent);
	  int lhe_index = -1;
	  for (i = 0; i < len; i++)
	    if (extents [i] == lhe)
	      {
		lhe_index = i;
		break;
	      }
	  /* ...and if it is, make up a dummy extent of the appropriate
	     priority, and add it to the list to be sorted with the rest.
	   */
	  if (lhe_index != -1)
	    {
	      /* memset isn't really necessary; we only deref `priority' */
	      memset (&dummy_extent, 0, sizeof (dummy_extent));
	      dummy_extent.priority = mouse_highlight_priority;
	      /* put the dummy extent just after the lhe in the stack,
		 as they're already sorted by size/starting point. */
	      for (i = len; i > lhe_index; i--)
		extents [i] = extents [i-1];
	      extents [lhe_index + 1] = &dummy_extent;
	      len++;
	    }
	}

      /* sort our copy of the stack by extent->priority... */
      for (i = 1; i < len; i++)
	{
	  int j = i - 1;
	  while (j >= 0 && extents[j]->priority > extents[j+1]->priority)
	    {
	      EXTENT tmp = extents [j];
	      extents [j] = extents [j+1];
	      extents [j+1] = tmp;
	      j--;
	    }
	}

      /* Now merge the faces of the extents together in order.

	 Remember that one of the extents in the list might be our dummy
	 extent representing the highlighting that is attached to some other
	 extent that is currently mouse-highlighted.  When an extent is
	 mouse-highlighted, it is as if there are two extents there, of
	 potentially different priorities: the extent being highlighted, with
	 whatever face and priority it has; and an ephemeral extent in the
	 `highlight' face with `mouse-highlight-priority'.
       */
      reset_face (s, &face);
      for (i = 0; i < len; i++)
	{
	  EXTENT current = extents [i];
	  if (current == &dummy_extent)
	    /* this isn't a real extent; use the highlight face. */
	    merge_faces (&SCREEN_HIGHLIGHT_FACE (s), &face);
	  else if (extent_end (current) != extent_start (current))
	    /* Skip 0-length extents from the merge (is this necessary?) */
	    merge_extent_face (s, current, &face);
	}

      extfrag->fp = get_display_face (s, &face);
    }
}


/* Allocate a new face */
static struct face *
allocate_face ()
{
  struct face* result = (struct face*)xmalloc (sizeof (struct face));
  result->font = Qnil;
  result->foreground = Qnil;
  result->background = Qnil;
  result->back_pixmap = Qnil;
  result->underline = 0;
  result->hilited = 0;		/* #### only used by dumb-tty code now... */
  return result;
}


void
ensure_face_ready (struct screen* s, int f)
{
  if (s->n_faces <= f)
    {
      int n = f + 10;
      int i;
      int start;
      if (!s->n_faces)
	{
	  s->faces = (struct face**)xmalloc (sizeof (struct face*) * n);
	  start = 0;
	}
      else
	{
	  s->faces =
	    (struct face**)xrealloc (s->faces, sizeof (struct face*) * n);
	  start = s->n_faces;
	}

      s->n_faces = n;

      for (i = start; i < n; i++)
	s->faces [i] = allocate_face ();
    }
}

static struct face *
get_screen_face (struct screen *s, int id)
{
  struct face *face;
  ensure_face_ready (s, id);
  face = s->faces [id];
  if (! face) abort ();
  return face;
}

/* screens */

void
init_screen_faces (struct screen *s)
{
  struct screen *other_screen = 0;
  Lisp_Object rest;

  for (rest = Vscreen_list; !NILP (rest); rest = Fcdr (rest))
    {
      struct screen *s2 = XSCREEN (Fcar (rest));
      if (s2 != s && SCREEN_IS_X (s2))
	{
	  other_screen = s2;
	  break;
	}
    }

  if (other_screen)
    /* make sure this screen's face vector is as big as the others */
    ensure_face_ready (s, other_screen->n_faces);

    {
      struct screen *oss = selected_screen;

      if (purify_flag)
	return;

      /* There's no reason to bother doing specbinds here, because if
	 make-initial-faces signals an error, emacs is going to crash
	 immediately.
       */
      gc_currently_forbidden = 1;
      Vinhibit_quit = Qt;
      selected_screen = s;
      call0 (Qmake_screen_initial_faces);
      selected_screen = oss;
      Vinhibit_quit = Qnil;
      gc_currently_forbidden = 0;

      if (SCREEN_IS_X (s))
	if (NILP (SCREEN_NORMAL_FACE (s).font))
	  fatal("Unable to load any useable ISO8859-1 font; check X resources");
    }
}


void
free_screen_faces (struct screen *s)	/* called from Fdelete_screen() */
{
  int i;

  /* This may be able to free some GCs, but unfortunately it will cause
     all screens to redisplay. */
  invalidate_face_cache (s, 1);

  for (i = 0; i < s->n_faces; i++)
    {
      struct face *face = s->faces [i];
      if (! face)
        continue;
      xfree (face);
    }
  xfree (s->faces);
  s->faces = 0;
  s->n_faces = 0;
}


static void
hash_faces_in_windows (Lisp_Object window, c_hashtable face_hash)
{
  for (; !NILP (window); window = XWINDOW(window)->next)
    hash_faces_in_window (window, face_hash);
}

static void
hash_faces_in_window (Lisp_Object window, c_hashtable face_hash)
{
  struct window *w = XWINDOW(window);
  struct char_block *cb;
  struct line_header *l;

  if (!NILP (w->vchild))
    {
      hash_faces_in_windows (w->vchild, face_hash);
      return;
    }
  if (!NILP (w->hchild))
    {
      hash_faces_in_windows (w->hchild, face_hash);
      return;
    }

  if (w->lines)
    {
      l = w->lines;
      while (l)
	{
	  cb = l->body;
	  while (cb != l->end)
	    {
	      puthash (cb->face, cb->face, face_hash);
	      cb = cb->next;
	    }
	  l = l->next;
	}
    }
  if (w->modeline)
    {
      cb = w->modeline->body;
      while (cb != w->modeline->end)
	{
	  puthash (cb->face, cb->face, face_hash);
	  cb = cb->next;
	}
    }
}

/*
 * Apply a function to each face used in window.  The function has to
 * be suitable for passing to maphash.  The first two args will both
 * end up being struct face *, the third void *.
 */
void
map_redisplay_faces_in_window (Lisp_Object window,
			       void (*map_function)
			       (const void *, void *, void*),
			       void *arg)
{
  c_hashtable face_hash;

  face_hash = make_general_hashtable (MAX_CACHED_FACES,
				      face_hash_function, face_eql);

  hash_faces_in_windows (window, face_hash);
  maphash (map_function, face_hash, arg);

  free_hashtable (face_hash);
}

/*
 * Apply a function to each face used in screen.  The function has to
 * be suitable for passing to maphash.  The first two args will both
 * end up being struct face *, the third void *
 */
void
map_redisplay_faces_in_screen (struct screen *s,
			       void (*map_function)
			       (const void *, void *, void*),
			       void *arg)
{
  map_redisplay_faces_in_window (s->root_window, map_function, arg);
}

/*
 * Apply a function to each face used on display.  The function has to
 * be suitable for passing to maphash.  The first two args will both
 * end up being struct face *, the third void *.  This function assumes
 * the display argument for now since multi-display's are not yet
 * supported
 */
void
map_redisplay_faces_on_display (void (*map_function)
				(const void *, void *, void*),
				void *arg)
{
  c_hashtable face_hash;
  Lisp_Object cur_s, rest;

  face_hash = make_general_hashtable (MAX_CACHED_FACES,
				      face_hash_function, face_eql);

  for (rest = Vscreen_list ; !NILP (rest) ; rest = XCONS(rest)->cdr)
    {
      cur_s = XCONS(rest)->car;
      if (!SCREENP (cur_s))
	abort();

      hash_faces_in_windows (XSCREEN(cur_s)->root_window, face_hash);
    }

  maphash (map_function, face_hash, arg);

  free_hashtable (face_hash);
}


/* Lisp interface */

DEFUN ("screen-face-alist", Fscreen_face_alist, Sscreen_face_alist, 1, 1, 0,
       "")
     (screen)
     Lisp_Object screen;
{
  CHECK_SCREEN (screen, 0);
  return XSCREEN (screen)->face_alist;
}

DEFUN ("set-screen-face-alist", Fset_screen_face_alist, Sset_screen_face_alist,
       2, 2, 0,
       "")
     (screen, value)
     Lisp_Object screen, value;
{
  CHECK_SCREEN (screen, 0);
  XSCREEN (screen)->face_alist = value;
  return value;
}


DEFUN ("make-face-internal", Fmake_face_internal, Smake_face_internal,
       3, 3, 0,
       "")
     (name, object, id_number)
     Lisp_Object name, object, id_number;
{
  Lisp_Object rest;
  int id = XINT (id_number);
  CHECK_SYMBOL (name, 0);
  CHECK_VECTOR (object, 0);
  CHECK_FIXNUM (id_number, 0);
  if (id < 0)
    signal_error (Qargs_out_of_range, list1 (id_number));
  for (rest = Vscreen_list; !NILP (rest); rest = XCONS (rest)->cdr)
    {
      struct screen *s = XSCREEN (XCONS (rest)->car);
      Lisp_Object old = Fassq (name, s->face_alist);
      if (NILP (old))
	{
	  s->face_alist = Fcons (Fcons (name, object), s->face_alist);
	  /* unless we're at the end now, copy the face object so that
	     each screen has it's own structure (because they can be
	     modified independently.)
	   */
	  if (!NILP (Fcdr (rest)))
	    object = Fcopy_sequence (object);
	}
      else if (EQ (rest, Vscreen_list)) /* first time */
	signal_error (Qerror,
                      list2 (build_string ("there is already a face named"),
                             name));
      else			     /* second time */
	abort ();

      ensure_face_ready (s, id);
      if (! s->faces [id])
	abort ();
    }
  return Qnil;
}

#ifdef HAVE_X_WINDOWS
static void
update_EmacsScreen (struct screen *s, char *name, void *val)
{
  Arg av[10];
  int ac = 0;
  if (! SCREEN_IS_X (s))
    return;
  XtSetArg (av[ac], name, val); ac++;
  BLOCK_INPUT;
  XtSetValues (s->display.x->edit_widget, av, ac);
  /* This is kind of a hack, but it works and it is kind of up in
     the air about what doing it properly is. */
  x_set_window_size (s, s->width, s->height);
  UNBLOCK_INPUT;
}
#endif

static void
ensure_not_builtin_face (int id, Lisp_Object attr_name)
{
#ifdef HAVE_X_WINDOWS
  if ((id == 0 || id == 1 || id == 3 || id == 4) &&
      (EQ (attr_name, Qforeground) ||
       EQ (attr_name, Qbackground) ||
       EQ (attr_name, Qfont)))
    {
      char buf [255];
      sprintf (buf, "can't set %s of `%s' face to nil",
	       (char *) XSYMBOL (attr_name)->name->data,
	       (id == 0 ? "default" :
		id == 1 ? "modeline" :
		id == 3 ? "left-margin" :
		id == 4 ? "right-margin" : "???"));
      error (buf);
    }
#endif /* HAVE_X_WINDOWS */
}

DEFUN ("set-face-attribute-internal", Fset_face_attribute_internal,
       Sset_face_attribute_internal, 4, 4, 0,
       "")
     (face_id, attr_name, attr_value, screen)
     Lisp_Object face_id, attr_name, attr_value, screen;
{
  struct face *face;
  struct screen *s;
  int id;
  CHECK_SCREEN (screen, 0);
  CHECK_FIXNUM (face_id, 0);
  CHECK_SYMBOL (attr_name, 0);
  s = XSCREEN (screen);
  id = XINT (face_id);
  if (id < 0)
    return Fsignal (Qargs_out_of_range, list1 (face_id));
  face = get_screen_face (s, id);
  if (NILP (attr_value)) ensure_not_builtin_face (id, attr_name);

  if (EQ (attr_name, Qfont))
    {
#ifdef HAVE_X_WINDOWS
      if (!NILP (attr_value)) CHECK_FONT (attr_value, 0);
      face->font = attr_value;
      invalidate_face_cache (s, 0);
      if (id == 0)
	update_EmacsScreen (s, XtNfont, XFONT (FACE_FONT (face))->font);
#endif /* HAVE_X_WINDOWS */
    }
  else if (EQ (attr_name, Qforeground))
    {
#ifdef HAVE_X_WINDOWS
      if (!NILP (attr_value)) CHECK_PIXEL (attr_value, 0);
      face->foreground = attr_value;
      invalidate_face_cache (s, 0);
      if (id == 0)
	update_EmacsScreen (s, XtNforeground, (void *) FACE_FG_PIXEL (face));
#endif /* HAVE_X_WINDOWS */
    }
  else if (EQ (attr_name, Qbackground))
    {
#ifdef HAVE_X_WINDOWS
      if (!NILP (attr_value)) CHECK_PIXEL (attr_value, 0);
      face->background = attr_value;
      invalidate_face_cache (s, 0);
      if (id == 0)
	update_EmacsScreen (s, XtNbackground, (void *) FACE_BG_PIXEL (face));
#endif /* HAVE_X_WINDOWS */
    }
  else if (EQ (attr_name, Qbackground_pixmap))
    {
#ifdef HAVE_X_WINDOWS
      if (!NILP (attr_value)) CHECK_PIXMAP (attr_value, 0);
      face->back_pixmap = attr_value;
      invalidate_face_cache (s, 0);
#endif /* HAVE_X_WINDOWS */
    }
  else if (EQ (attr_name, Qunderline))
    {
      int new = !NILP (attr_value);
      if (face->underline != new)
	invalidate_face_cache (s, 0);
      face->underline = new;
    }
  else
    signal_error (Qerror, list2 (build_string ("unknown face attribute"),
                                 attr_name));
  return Qnil;
}



void
syms_of_faces ()
{
  face_cache_invalid = 0;
  face_cache_pending_flush = 0;

  defsubr (&Sscreen_face_alist);
  defsubr (&Sset_screen_face_alist);
  defsubr (&Smake_face_internal);
  defsubr (&Sset_face_attribute_internal);

  defsymbol (&Qfont, "font");
  defsymbol (&Qforeground, "foreground");
  defsymbol (&Qbackground, "background");
  defsymbol (&Qunderline, "underline");
  defsymbol (&Qbackground_pixmap, "background-pixmap");
  defsymbol (&Qmake_screen_initial_faces, "make-screen-initial-faces");

  DEFVAR_INT ("mouse-highlight-priority", &mouse_highlight_priority,
"The priority to use for the mouse-highlighting pseudo-extent\n\
that is used to highlight extents with the `highlight' attribute set.\n\
See `set-extent-priority'.");
  mouse_highlight_priority = 10;
}
