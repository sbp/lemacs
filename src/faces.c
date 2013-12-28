/* "Face" primitives
This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <sys/types.h>
#include <sys/stat.h>

#include "config.h"
#include "lisp.h"

#include "xterm.h"
#include "buffer.h"
#include "extents.h"
#include "screen.h"
#include "window.h"
#include "indent.h"

/* Display Context for the icons */ 
#include <X11/Intrinsic.h>
#include <X11/IntrinsicP.h>
#include <X11/CoreP.h>
#include <X11/StringDefs.h>
#include <X11/Xmu/Drawing.h>
#include <X11/Xos.h>

/*#include "extents-data.h"*/
#include "faces.h"
#include "hash.h"

#ifndef ENERGIZE
#include "hash.h"
#endif

#ifdef HAVE_XPM
#include <X11/xpm.h>
#endif

/* The prioirty of the mouse-highlighting attributes, for extent-merging.
 */
int mouse_highlight_priority;


#define FACE_DEFAULT (~0)

extern int extent_cache_invalid;

void ensure_face_ready (struct screen* s, int f);
static void build_face (struct screen* s, struct face* face);
static void compute_screen_line_height (struct screen *s);


/* Caching functions for faces */
static c_hashtable face_cache;
static c_hashtable face_cache_pending_flush;

static int face_cache_inited;
int face_cache_invalid;

static struct face *allocate_face ();

static unsigned long
face_hash_function (void *arg)
{
  struct face* face = (struct face*) arg;

  return
    (((unsigned long) face->font) ^ 
     (((unsigned long) face->foreground) ^
      (((unsigned long) face->background) ^ 
       (((unsigned long) face->back_pixmap) ^
	((unsigned long) face->underline)))));
}

static int
face_eql (void *arg1, void *arg2)
{
  if (arg1 == arg2) 
    return 1;
  else
    {
      struct face* face1 = (struct face*) arg1;
      struct face* face2 = (struct face*) arg2;

      if ((face1->font == face2->font) &&
          (face1->foreground == face2->foreground) &&
          (face1->background == face2->background) &&
          (face1->back_pixmap == face2->back_pixmap) &&
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
  face_cache = make_general_hashtable (30, face_hash_function, face_eql);
}

static struct face*
get_cached_face (struct face* face)
{
  struct face *result;

  if (!face_cache_inited)
    init_face_cache ();

  if (gethash (face, face_cache, (void **)&result))
    return result;
  else
    return 0;
}

static void
cache_face (struct face *face)
{
  if (!face_cache_inited)
    init_face_cache ();

  puthash ((void *)face, (void *)face, face_cache);
}


/* flush_face_cache is called from redisplay when face_cache_invalid is true.
   we set face_cache_invalid when we change a font or color of some face,
   meaning that the GCs need to be recomputed.  We don't just flush the
   cache at the time that we change a face because the redisplay structures
   still have pointers to the faces we want to free.
 */

static void
flush_face_cache_mapper (void *hash_key, void *hash_contents, void *closure)
{
  struct face *face = (struct face *) hash_contents;
  Display *dpy = (Display *) closure;
  if (face->facegc)
    XFreeGC (dpy, face->facegc);
  xfree (face);
}

void
flush_face_cache ()
{
  Lisp_Object rest;
  Display *dpy = x_current_display;
  if (! face_cache_inited)
    return;
  if (! face_cache_pending_flush)
    return;

  BLOCK_INPUT;
  maphash (flush_face_cache_mapper, (void *)
	   face_cache_pending_flush,
	   (void *) dpy);
  UNBLOCK_INPUT;
  clrhash (face_cache_pending_flush);
  free_hashtable (face_cache_pending_flush);
  face_cache_pending_flush = 0;

  /* We need to compute the GCs for the normal and modeline faces of all
     screens right away.
   */
  for (rest = Vscreen_list; !NILP (rest); rest = XCONS (rest)->cdr)
    {
      struct screen *s = XSCREEN (XCONS (rest)->car);
      if (!SCREEN_IS_X (s))
	continue;
      if (! SCREEN_NORMAL_FACE (s).facegc)
	abort ();
      if (! SCREEN_MODELINE_FACE (s).facegc)
	abort ();
      XFreeGC (dpy, SCREEN_NORMAL_FACE (s).facegc);
      XFreeGC (dpy, SCREEN_MODELINE_FACE (s).facegc);
      build_face (s, &SCREEN_NORMAL_FACE (s));
      build_face (s, &SCREEN_MODELINE_FACE (s));
    }
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


/* Utility functions for faces */

static XFontStruct *
verify_font (struct screen* s, XFontStruct *font)
{
#if 0
  if (font == (XFontStruct *) FACE_DEFAULT)
    return font;
  else
    {
      XFontStruct *default_font = SCREEN_NORMAL_FACE (s).font;
      
      if (default_font->ascent + default_font->descent
	  != font->ascent + font->descent)
        return default_font;
      else
        return font;
    }
#else
  return font;
#endif
}

#define MAX(x,y) (((x)>(y))?(x):(y))

extern void EmacsScreenResize (Widget);

static void
compute_screen_line_height (struct screen *s)
{
  int ascent = 0;
  int descent = 0;
  int old, i;
  if (! SCREEN_IS_X (s))
    return;
  for (i = 0; i < s->n_faces; i++)
    if (s->faces [i]->font &&
	s->faces [i]->font != (XFontStruct *) FACE_DEFAULT)
      {
	ascent = MAX (ascent, s->faces [i]->font->ascent);
	descent = MAX (descent, s->faces [i]->font->descent);
      }
  old = s->display.x->text_height;
  s->display.x->text_height = ascent + descent;
  if (old != s->display.x->text_height)
    EmacsScreenResize (s->display.x->edit_widget);
}

static void
reset_face (struct screen* s, struct face* face)
{
  struct face* normal_face = &SCREEN_NORMAL_FACE (s);
  memset ((void*)face, 0, sizeof (struct face));
  face->foreground = normal_face->foreground;
  face->background = normal_face->background;
  face->back_pixmap = normal_face->back_pixmap;
  face->underline = normal_face->underline;
  face->font = normal_face->font;
#if 0
  face->font_name = 0;
#endif
}

static void 
merge_faces (struct face* from, struct face* to)
{
  if (from->font != (XFontStruct *)FACE_DEFAULT)
    {
      to->font = from->font;
#if 0
      to->font_name = 0;
#endif
    }
  if (from->foreground != FACE_DEFAULT)
    to->foreground = from->foreground;
  if (from->background != FACE_DEFAULT)
    to->background = from->background;
  if (from->back_pixmap != FACE_DEFAULT)
    to->back_pixmap = from->back_pixmap;
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

static void
build_face (struct screen* s, struct face* face)
{
  GC gc;
  XGCValues xgcv;
  unsigned long mask;

  xgcv.foreground = face->foreground;
  xgcv.background = face->background;
  xgcv.font = face->font->fid;
  xgcv.graphics_exposures = 0;
  mask = GCForeground | GCBackground | GCFont | GCGraphicsExposures;
  gc = XCreateGC (XtDisplay (s->display.x->widget),
		  XtWindow (s->display.x->widget),
		  mask, &xgcv);
  if (face->back_pixmap && face->back_pixmap != FACE_DEFAULT)
    XSetStipple (XtDisplay (s->display.x->widget), gc, face->back_pixmap);
  face->facegc = gc;
}


/* Get a face suitable for display (ie which has a GC) for a given
   set of attributes. */

static struct face *
get_display_face (struct screen* s, struct face* face)
{
  struct face *result;

  /* Does the face have a GC already */
  if (face->facegc)
    return face;
  
  /* cache the default face */
  if (face->font == SCREEN_NORMAL_FACE (s).font
      && face->foreground == SCREEN_NORMAL_FACE (s).foreground
      && face->background == SCREEN_NORMAL_FACE (s).background
      && face->back_pixmap == SCREEN_NORMAL_FACE (s).back_pixmap
      && face->underline == SCREEN_NORMAL_FACE (s).underline)
    return &SCREEN_NORMAL_FACE (s);

  /* cache the modeline face */
  if (face->font == SCREEN_MODELINE_FACE (s).font
      && face->foreground == SCREEN_MODELINE_FACE (s).foreground
      && face->background == SCREEN_MODELINE_FACE (s).background
      && face->back_pixmap == SCREEN_MODELINE_FACE (s).back_pixmap
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
  
  build_face (s, result);
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
	  else if (current->end != current->start)
	    /* Skip 0-length extents from the merge (is this necessary?) */
	    merge_extent_face (s, current, &face);
	}

      face.font = verify_font (s, face.font);
      extfrag->fp = get_display_face (s, &face);
    }
}


/* Allocate a new face */
static struct face *
allocate_face ()
{
  struct face* result = (struct face*)xmalloc (sizeof (struct face));
  memset ((void*)result, 0, sizeof (struct face));
  result->font = (XFontStruct *) FACE_DEFAULT;
  result->foreground = FACE_DEFAULT;
  result->background = FACE_DEFAULT;
  result->back_pixmap = FACE_DEFAULT;
  result->underline = 0;
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
/*
  if (!s->faces [f]->resourced)
    {
      BLOCK_INPUT;
      EmacsScreenResourceFace ((EmacsScreenWidget)s->display.x->edit_widget,
			       f);
      UNBLOCK_INPUT;
    }
 */
}



/* Allocating, freeing, and duplicating fonts, colors, and pixmaps.

   It would be cleaner if we parsed these things by simply calling the
   currently-installed Xt resource converters; that would let folks set
   the font of a face to "XtDefaultFont", and would make XPM support
   automatic if the right resource converter was linked in.

   However, there are two problems with this approach: first, I couldn't
   figure out how to make XtConvert() work at all.  Second, Xt is very
   statically-minded; if you ask for the same pixel/font/pixmap twice from
   the resource database, it returns the old value, not a copy, and doesn't
   maintain a reference count or provide a way to free it.  So that would
   mean that once emacs had allocated a color cell, or font, or pixmap,
   there would be no way to free it, since we would have no way of knowing
   whether some other widget in this process also had a pointer to it.
 */

#ifdef HAVE_X_WINDOWS

static XFontStruct *
load_font (struct screen *s, Lisp_Object name)
{
  XFontStruct *font;
  if (NILP (name) || !SCREEN_IS_X (s))
    return (XFontStruct *) FACE_DEFAULT;
  CHECK_STRING (name, 0);
  BLOCK_INPUT;
  font = XLoadQueryFont (XtDisplay (s->display.x->widget),
			 (char *) XSTRING (name)->data);
  UNBLOCK_INPUT;
  if (! font)
    while (1)
      Fsignal (Qerror, Fcons (build_string ("couldn't load font"),
			      Fcons (name, Qnil)));
  return font;
}

static void
unload_font (struct screen *s, XFontStruct *font)
{
  if (!font || font == ((XFontStruct *) FACE_DEFAULT))
    return;
  XFreeFont (XtDisplay (s->display.x->widget), font);
}

static unsigned long
load_pixel (struct screen *s, Lisp_Object name)
{
  Widget widget;
  Display *dpy;
  Colormap cmap;
  XColor color;
  int result;

  if (NILP (name) || !SCREEN_IS_X (s))
    return FACE_DEFAULT;

  widget = s->display.x->widget;
  dpy = XtDisplay (widget);
  cmap = DefaultColormapOfScreen (XtScreen (widget));

  CHECK_STRING (name, 0);
  BLOCK_INPUT;
  result = XParseColor (dpy, cmap, (char *) XSTRING (name)->data, &color);
  UNBLOCK_INPUT;
  if (! result)
    return Fsignal (Qerror, Fcons (build_string ("unrecognised color"),
				   Fcons (name, Qnil)));
  BLOCK_INPUT;
  result = XAllocColor (dpy, cmap, &color);
  UNBLOCK_INPUT;
  if (! result)
    return Fsignal (Qerror, Fcons (build_string ("couldn't allocate color"),
				   Fcons (name, Qnil)));
  return (unsigned long) color.pixel;
}

static void
unload_pixel (struct screen *s, Pixel pixel)
{
  Widget widget;
  Colormap cmap;
  Display *dpy;
  if (pixel == FACE_DEFAULT)
    return;
  widget = s->display.x->widget;
  cmap = DefaultColormapOfScreen (XtScreen (widget));
  dpy = XtDisplay (widget);
  BLOCK_INPUT;
  XFreeColors (dpy, cmap, &pixel, 1, 0);
  UNBLOCK_INPUT;
}

static int locate_pixmap_file (char *in, char *out);

Pixmap
load_pixmap_1 (Display *dpy, Window window,
	       Lisp_Object name, unsigned int *wP, unsigned int *hP,
	       unsigned int *dP, Pixmap *maskP)
{
  int result;
  char file [1024];
  int xhot, yhot;
  unsigned int w, h, d;
  unsigned char *data = 0;
  Pixmap new;
  Pixmap mask = 0;
#ifdef HAVE_XPM
  XpmAttributes xpmattrs;
  int retry_in_mono = 0;
#endif /* HAVE_XPM */

  if (CONSP (name))
    {
      if (!CONSP (Fcdr (name)) ||
	  !CONSP (Fcdr (Fcdr (name))) ||
	  !NILP (Fcdr (Fcdr (Fcdr (name)))) ||
	  !FIXNUMP (Fcar (name)) ||
	  !FIXNUMP (Fcar (Fcdr (name))) ||
	  !STRINGP (Fcar (Fcdr (Fcdr (name)))))
	return
	  Fsignal (Qerror,
		   Fcons (build_string ("must be of the form (W H \"bits\")"),
			  Fcons (name, Qnil)));
      w = XINT (Fcar (name));
      h = XINT (Fcar (Fcdr (name)));
      if (w <= 0)
	while (1) wrong_type_argument (Qnatnump, Fcar (name));
      if (h <= 0)
	while (1) wrong_type_argument (Qnatnump, Fcar (Fcdr (name)));
      if ((w * h / 8) > XSTRING (Fcar (Fcdr (Fcdr (name))))->size)
	while (1)
	  Fsignal (Qerror,
		   Fcons (build_string ("data is too short for W and H"),
			  Fcons (name, Qnil)));
      data = (unsigned char *) XSTRING (Fcar (Fcdr (Fcdr (name))))->data;
      xhot = yhot = 0;
      new = XCreatePixmapFromBitmapData (dpy, window,
					 (char *) data, w, h, 1, 0, 1);
      d = 0;
    }
  else
    {
      CHECK_STRING (name, 0);
      if (! locate_pixmap_file ((char *) XSTRING (name)->data, file))
	return Fsignal (Qfile_error,
			list3 (build_string ("Opening pixmap file"),
			       build_string ("no such file or directory"),
			       name));
      name = build_string (file);

#ifdef HAVE_XPM
      xpmattrs.valuemask = 0;
    RETRY:
      BLOCK_INPUT;
      result = XpmReadFileToPixmap (dpy, window, file, &new, &mask, &xpmattrs);
      w = xpmattrs.width;
      h = xpmattrs.height;
      XpmFreeAttributes (&xpmattrs);
      UNBLOCK_INPUT;

      switch (result)
	{
	case XpmSuccess:
	  /* XpmReadFileToPixmap() doesn't return the depth (bogus!) so we
	     need to get it ourself. */
#if 1
	  /* Actually, we might as well just assume that Xpm did the right
	     thing and gave us a pixmap of the same depth as the window we
	     passed it. */
	  /* #### but we shouldn't be using selected_screen here */
	  d = selected_screen->display.x->widget->core.depth;
#else
	  {
	    Window root;
	    int x, y;
	    unsigned int w2, h2, bw;
	    BLOCK_INPUT;
	    if (!XGetGeometry (dpy, new, &root, &x, &y, &w2, &h2, &bw, &d))
	      abort ();
	    if (w != w2 || h != h2) abort ();
	    UNBLOCK_INPUT;
	  }
#endif
	  
	  goto SUCCESS;

	case XpmFileInvalid:
	  /* Ok, we'll try to read it as an XBM and error if that fails */
	  break;
	  
	case XpmColorFailed:
	  /* If we couldn't allocate any colors for this image, then silently
	     retry in monochrome.  If that fails too, then signal an error.
	   */
	  if (retry_in_mono) /* second time; blow out. */
	    return Fsignal (Qfile_error,
			    list3 (build_string ("Reading pixmap file"),
				   build_string ("color allocation failed"),
				   name));
	  /* else... */
	  retry_in_mono = 1;
	  xpmattrs.depth = 1;
	  xpmattrs.valuemask |= XpmDepth;
	  goto RETRY;

	case XpmColorError:
	  {
	    /* Maybe we should just read it in monochrome instead of
	       allowing the colors to be substituted?
	     */
	    char buf [2000];
	    sprintf (buf, "color substitution performed for file \"%s\"",
		     (char *) XSTRING (name)->data);
	    message (buf);
	  }
	case XpmNoMemory:
	  return Fsignal (Qfile_error,
			  list3 (build_string ("Reading pixmap file"),
				 build_string ("out of memory"),
				 name));
	case XpmOpenFailed:
	  return
	    Fsignal (Qfile_error,
		     Fcons (build_string ("Opening pixmap file"),
			    Fcons (build_string ("no such file or directory"),
				   Fcons (name, Qnil))));
	default:
	  return Fsignal (Qfile_error,
			  list4 (build_string ("Reading pixmap file"),
				 build_string ("unknown error code"),
				 make_number (result), name));
	}
#endif /* HAVE_XPM */
      
      BLOCK_INPUT;
      result = XmuReadBitmapDataFromFile (file, &w, &h, &data, &xhot, &yhot);
      UNBLOCK_INPUT;
      
      switch (result)
	{
	case BitmapSuccess:
	  BLOCK_INPUT;
	  new = XCreatePixmapFromBitmapData (dpy, window,
					     (char *) data, w, h, 1, 0, 1);
	  d = 0;
	  XFree ((char *)data);
	  UNBLOCK_INPUT;
	  break;
	case BitmapOpenFailed:
	  return
	    Fsignal (Qfile_error,
		     Fcons (build_string ("Opening bitmap file"),
			    Fcons (build_string ("no such file or directory"),
				   Fcons (name, Qnil))));
	case BitmapFileInvalid:
	  return Fsignal (Qfile_error,
			  list3 (build_string ("Reading bitmap file"),
				 build_string ("invalid bitmap data"),
				 name));
	case BitmapNoMemory:
	  return Fsignal (Qfile_error,
			  list3 (build_string ("Reading bitmap file"),
				 build_string ("out of memory"),
				 name));
	default:
	  return Fsignal (Qfile_error,
			  list4 (build_string ("Reading bitmap file"),
				 build_string ("unknown error code"),
				 make_number (result), name));
	}
    }
  
 SUCCESS:
  *wP = w;
  *hP = h;
  *dP = d;
  if (maskP) *maskP = mask;
  return new;
}


unsigned long
load_pixmap (struct screen *s, Lisp_Object name,
	     unsigned int *wP, unsigned int *hP, unsigned int *dP,
	     unsigned long *maskP)
{
  if (NILP (name) || !SCREEN_IS_X (s))
    {
      *wP = *hP = *dP = 0;
      return FACE_DEFAULT;
    }
  else if (!SCREEN_IS_X (s))
    abort ();
  else
    {
      Widget widget = s->display.x->widget;
      return (unsigned long)
	load_pixmap_1 (XtDisplay (widget), XtWindow (widget),
		       name, wP, hP, dP, (Pixmap *) maskP);
    }
}


void
unload_pixmap (struct screen *s, unsigned long pix)
{
  if (!pix || pix == FACE_DEFAULT)
    return;
  BLOCK_INPUT;
  XFreePixmap (XtDisplay (s->display.x->widget), (Pixmap) pix);
  UNBLOCK_INPUT;
}


extern Lisp_Object Vx_bitmap_file_path;

extern void initialize_x_bitmap_file_path (void);

static int
locate_pixmap_file (char *in, char *out)
{
  Lisp_Object rest;
  if (in [0] == '/' || (in [0] == '.' && in [1] == '/'))
    {
      strcpy (out, in);
      return 1;
    }
  
  if (NILP (Vx_bitmap_file_path))
    initialize_x_bitmap_file_path ();
  
  for (rest = Vx_bitmap_file_path; CONSP (rest); rest = Fcdr (rest))
    {
      struct stat st;
      int length = XSTRING (XCONS (rest)->car)->size;
      if (length == 0) continue;
      strncpy (out, (char *) XSTRING (XCONS (rest)->car)->data, length+1);
      if (out [length] != '/')
	out [length++] = '/';
      strcpy (out + length, in);
      if (stat (out, &st) >= 0			/* exists */
	  && (st.st_mode & S_IFMT) != S_IFDIR	/* not a directory */
	  && access (out, X_OK))		/* readable */
	return 1;
    }
  return 0;
}

#endif /* HAVE_X_WINDOWS */


/* screens */

extern int gc_currently_forbidden;

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

      if (! NILP (Vpurify_flag))
	return;

      /* There's no reason to bother doing specbinds here, because if
	 make-initial-faces signals an error, emacs is going to crash
	 immediately.
       */
      gc_currently_forbidden = 1;
      Vinhibit_quit = Qt;
      selected_screen = s;
      call0 (intern ("make-screen-initial-faces"));
      selected_screen = oss;
      Vinhibit_quit = Qnil;
      gc_currently_forbidden = 0;

      if (SCREEN_IS_X (s))
	if (SCREEN_NORMAL_FACE (s).font == 0 ||
	    SCREEN_NORMAL_FACE (s).font == (XFontStruct *) FACE_DEFAULT)
	fatal ("Unable to load any useable ISO8859-1 font; check X resources");
    }
  
  if (SCREEN_IS_X (s))
    {
      build_face (s, &SCREEN_NORMAL_FACE (s));   /* the first two have GCs */
      build_face (s, &SCREEN_MODELINE_FACE (s));
    }
}


void
free_screen_faces (struct screen *s)	/* called from Fdelete_screen() */
{
  Display *dpy = XtDisplay (s->display.x->widget);
  int i;

  /* This may be able to free some GCs, but unfortunately it will cause
     all screens to redisplay. */
  invalidate_face_cache (s, 1);

  /* elts 0 and 1 of the face array are the only ones with GCs */
  XFreeGC (dpy, SCREEN_NORMAL_FACE (s).facegc);
  XFreeGC (dpy, SCREEN_MODELINE_FACE (s).facegc);
  SCREEN_NORMAL_FACE (s).facegc = 0;
  SCREEN_MODELINE_FACE (s).facegc = 0;

  for (i = 0; i < s->n_faces; i++)
    {
      struct face *face = s->faces [i];
      if (! face)
        continue;
      if (face->facegc)
	abort ();
      unload_font (s, face->font);
      unload_pixel (s, face->foreground);
      unload_pixel (s, face->background);
      unload_pixmap (s, face->back_pixmap);
      xfree (face);
    }
  xfree (s->faces);
  s->faces = 0;
  s->n_faces = 0;
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
       2, 2, 0, "")
     (screen, value)
     Lisp_Object screen, value;
{
  CHECK_SCREEN (screen, 0);
  XSCREEN (screen)->face_alist = value;
  return value;
}


DEFUN ("make-face-internal", Fmake_face_internal, Smake_face_internal,
       3, 3, 0, "")
     (name, object, id_number)
     Lisp_Object name, object, id_number;
{
  Lisp_Object rest;
  int id = XINT (id_number);
  CHECK_SYMBOL (name, 0);
  CHECK_VECTOR (object, 0);
  CHECK_FIXNUM (id_number, 0);
  if (id < 0)
    return Fsignal (Qerror, Fcons (build_string ("id must be positive"),
				   Fcons (id_number, Qnil)));
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
      else if (rest == Vscreen_list) /* first time */
	return Fsignal (Qerror,
			Fcons (build_string ("there is already a face named"),
			       Fcons (name, Qnil)));
      else			     /* second time */
	abort ();

      ensure_face_ready (s, id);
      if (! s->faces [id])
	abort ();
    }
  return Qnil;
}


DEFUN ("set-face-attribute-internal", Fset_face_attribute_internal,
       Sset_face_attribute_internal, 4, 4, 0, "")
     (face_id, attr_name, attr_value, screen)
     Lisp_Object face_id, attr_name, attr_value, screen;
{
  struct face *face;
  struct screen *s;
  int magic_p;
  int id;
  CHECK_SCREEN (screen, 0);
  CHECK_FIXNUM (face_id, 0);
  CHECK_SYMBOL (attr_name, 0);
  s = XSCREEN (screen);
  id = XINT (face_id);
  if (id < 0)
    return Fsignal (Qerror, Fcons (build_string ("invalid face id"),
				   Fcons (face_id, Fcons (screen, Qnil))));
  ensure_face_ready (s, id);
  face = s->faces [XFASTINT (face_id)];
  if (! face) abort ();
  magic_p = (NILP (attr_value) &&
	     (XFASTINT (face_id) == 0 ||
	      XFASTINT (face_id) == 1));

  if (EQ (attr_name, intern ("font")))
    {
#ifdef HAVE_X_WINDOWS
      XFontStruct *font;
      if (magic_p)
     error ("can't set the font of the `normal' or `modeline' faces to nil.");
      font = load_font (s, attr_value);
      unload_font (s, face->font);
      face->font = font;

#if 0
      if (face->font_name)
	xfree (face->font_name);
      if (font && font != ((XFontStruct *) FACE_DEFAULT))
	face->font_name =
	  (char *) strdup ((char *) XSTRING (attr_value)->data);
      else
	face->font_name = 0;
#endif

      if (id == 0) /* the "default" face; update the ScreenWidget as well */
	{
	  Arg av[10];
	  int ac = 0;
	  XtSetArg (av[ac], XtNfont, font); ac++;
	  BLOCK_INPUT;
	  XtSetValues (s->display.x->edit_widget, av, ac);
	  UNBLOCK_INPUT;
	}
      invalidate_face_cache (s, 0);
#endif /* HAVE_X_WINDOWS */
    }
  else if (EQ (attr_name, intern ("foreground")))
    {
#ifdef HAVE_X_WINDOWS
      unsigned long new_pixel;
      if (magic_p)
    error ("can't set the colors of the `normal' or `modeline' faces to nil.");
      new_pixel = load_pixel (s, attr_value);
      unload_pixel (s, face->foreground);
      face->foreground = new_pixel;
      invalidate_face_cache (s, 0);
      if (id == 0) /* the "default" face; update the ScreenWidget as well */
	{	   /* Possibly this isn't necessary for "foreground". */
	  Arg av[10];
	  int ac = 0;
	  XtSetArg (av[ac], XtNforeground, new_pixel); ac++;
	  BLOCK_INPUT;
	  XtSetValues (s->display.x->edit_widget, av, ac);
	  UNBLOCK_INPUT;
	}
#endif /* HAVE_X_WINDOWS */
    }
  else if (EQ (attr_name, intern ("background")))
    {
#ifdef HAVE_X_WINDOWS
      unsigned long new_pixel;
      if (magic_p)
    error ("can't set the colors of the `normal' or `modeline' faces to nil.");
      new_pixel = load_pixel (s, attr_value);
      unload_pixel (s, face->background);
      face->background = new_pixel;
      invalidate_face_cache (s, 0);
      if (id == 0) /* the "default" face; update the ScreenWidget as well */
	{
	  Arg av[10];
	  int ac = 0;
	  XtSetArg (av[ac], XtNbackground, new_pixel); ac++;
	  BLOCK_INPUT;
	  XtSetValues (s->display.x->edit_widget, av, ac);
	  UNBLOCK_INPUT;
	}
#endif /* HAVE_X_WINDOWS */
    }
  else if (EQ (attr_name, intern ("background-pixmap")))
    {
#ifdef HAVE_X_WINDOWS
      unsigned int w, h, d;
      unsigned long new_pixmap = load_pixmap (s, attr_value, &w, &h, &d, 0);
      unload_pixmap (s, face->back_pixmap);
      if (magic_p) new_pixmap = 0;
      face->back_pixmap = new_pixmap;
      face->pixmap_w = w;
      face->pixmap_h = h;
/*      face->pixmap_depth = d; */
      invalidate_face_cache (s, 0);
#endif /* HAVE_X_WINDOWS */
    }
  else if (EQ (attr_name, intern ("underline")))
    {
      int new = !NILP (attr_value);
      if (face->underline != new)
	invalidate_face_cache (s, 0);
      face->underline = new;
    }
  else
    return Fsignal (Qerror, Fcons (build_string ("unknown face attribute"),
				   Fcons (attr_name, Qnil)));
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

  DEFVAR_INT ("mouse-highlight-priority", &mouse_highlight_priority,
"The priority to use for the mouse-highlighting pseudo-extent\n\
that is used to highlight extents with the `highlight' attribute set.\n\
See `set-extent-priority'.");
  mouse_highlight_priority = 10;
}
