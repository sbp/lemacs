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
#include <X11/Xos.h>

#include "extents-data.h"
#include "faces.h"

#ifndef ENERGIZE
#include "hash.h"
#endif

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
  free (face);
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
invalidate_face_cache (struct screen *s)
{
  face_cache_invalid = 1;
  extent_cache_invalid = 1;
  SET_SCREEN_GARBAGED (s);
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
  bzero ((void*)face, sizeof (struct face));
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

struct face *
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
      reset_face (s, &face);
      for (i = 0; i < len; i++)
	{
	  EXTENT current = vec [i];
	  /* skip 0-length extents from the merge */
	  if (current->end != current->start)
	    {
	      merge_extent_face (s, vec [i], &face);
	      
	      if (EXTENTP (Vlast_highlighted_extent) &&
		  (current == XEXTENT (Vlast_highlighted_extent)))
		merge_faces (&SCREEN_HIGHLIGHT_FACE (s), &face);
	    }
	}
      
      face.font = verify_font (s, face.font);
      extfrag->fp = get_display_face (s, &face);
    }
}


/* Allocate a new face */
struct face*
allocate_face ()
{
  struct face* result = (struct face*)xmalloc (sizeof (struct face));
  bzero ((void*)result, sizeof (struct face));
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

unsigned long
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

void
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

unsigned long
load_pixmap (struct screen *s, Lisp_Object name, int *wP, int *hP, int *dP)
{
  Widget widget;
  Display *dpy;
  int result;
  Pixmap new;
  char file [1024];
  int xhot, yhot;
  int w, h, d;
  unsigned char *data = 0;

  if (NILP (name) || !SCREEN_IS_X (s))
    {
      *wP = *hP = *dP = 0;
      return FACE_DEFAULT;
    }

  widget = s->display.x->widget;
  dpy = XtDisplay (widget);

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
      new = XCreatePixmapFromBitmapData (XtDisplay (widget),
					 XtWindow (widget),
					 (char *) data, w, h, 1, 0, 1);
      d = 1;		/* always 1 plane for now */
    }
  else
    {
      CHECK_STRING (name, 0);
      if (! locate_pixmap_file ((char *) XSTRING (name)->data, file))
	return
	  Fsignal (Qfile_error,
		   Fcons (build_string ("Opening bitmap file"),
			  Fcons (build_string ("no such file or directory"),
				 Fcons (name, Qnil))));
      name = build_string (file);
      BLOCK_INPUT;
      result = XmuReadBitmapDataFromFile (file, &w, &h, &data, &xhot, &yhot);
      UNBLOCK_INPUT;

      switch (result)
	{
	case BitmapSuccess:
	  new = XCreatePixmapFromBitmapData (XtDisplay (widget),
					     XtWindow (widget),
					     (char *) data, w, h, 1, 0, 1);
	  d = 1;		/* always 1 plane for now */
	  XFree ((char *)data);
	  break;
	case BitmapOpenFailed:
	  return
	    Fsignal (Qfile_error,
		     Fcons (build_string ("Opening bitmap file"),
			    Fcons (build_string ("no such file or directory"),
				   Fcons (name, Qnil))));
	case BitmapFileInvalid:
	  return Fsignal (Qfile_error,
			  Fcons (build_string ("Reading bitmap file"),
				 Fcons (build_string ("invalid bitmap data"),
					Fcons (name, Qnil))));
	case BitmapNoMemory:
	  return Fsignal (Qfile_error,
			  Fcons (build_string ("Reading bitmap file"),
				 Fcons (build_string ("out of memory"),
					Fcons (name, Qnil))));
	default:
	  return Fsignal (Qfile_error,
			  Fcons (build_string ("Reading bitmap file"),
				 Fcons (build_string ("unknown error code"),
					Fcons (make_number (result),
					       Fcons (name, Qnil)))));

	}
    }
  *wP = w;
  *hP = h;
  *dP = d;
  return (unsigned long) new;
}


void
unload_pixmap (struct screen *s, unsigned long pix, int w, int h,int  d)
{
  if (!pix || pix == FACE_DEFAULT)
    return;
  BLOCK_INPUT;
  XFreePixmap (XtDisplay (s->display.x->widget), (Pixmap) pix);
  UNBLOCK_INPUT;
}


#ifndef BITMAPDIR
#define BITMAPDIR "/usr/include/X11/bitmaps"
#endif

extern Lisp_Object Vx_bitmap_file_path;

static int
locate_pixmap_file (char *in, char *out)
{
  Lisp_Object rest;
  if (in [0] == '/' || (in [0] == '.' && in [1] == '/'))
    {
      strcpy (out, in);
      return 1;
    }
  /* Compute the x-bitmap-file-path if it's not already set */
  if (NILP (Vx_bitmap_file_path))
    {
      Lisp_Object path = Fx_get_resource (build_string ("bitmapFilePath"),
					  build_string ("BitmapFilePath"),
					  intern ("string"), Qnil);
      if (!NILP (path))
	{
	  char *s1, *s2, *p = (char *) XSTRING (path)->data;
	  s1 = p;
	  for (s2 = p; *s2; s2++)
	    if (*s2 == ':' && s1 != s2)
	      {
		Vx_bitmap_file_path = Fcons (make_string (s1, s2 - s1),
					     Vx_bitmap_file_path);
		s1 = s2 + 1;
	      }
	}
      Vx_bitmap_file_path = Fnreverse (Fcons (build_string (BITMAPDIR),
					      Vx_bitmap_file_path));
    }

  /* search the x-bitmap-file-path */
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

  /* If this is not the first X screen, then make the face_alist a copy of
     the face_alist of some other screen.  Copy the values of the alist as
     well as the aconses.
   */
  if (other_screen)
    {
      Lisp_Object rest;
      Widget widget = s->display.x->widget;
      int i;

      s->n_faces = 0;
      s->faces = 0;
      ensure_face_ready (s, other_screen->n_faces);
    }


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


/* Lisp interface */

DEFUN ("screen-face-alist", Fscreen_face_alist, Sscreen_face_alist, 1, 1, 0, 0)
     (screen)
     Lisp_Object screen;
{
  CHECK_SCREEN (screen, 0);
  return XSCREEN (screen)->face_alist;
}

DEFUN ("set-screen-face-alist", Fset_screen_face_alist, Sset_screen_face_alist,
       2, 2, 0, 0)
     (screen, value)
     Lisp_Object screen, value;
{
  CHECK_SCREEN (screen, 0);
  XSCREEN (screen)->face_alist = value;
  return value;
}


DEFUN ("make-face-internal", Fmake_face_internal, Smake_face_internal,
       3, 3, 0, 0)
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
       Sset_face_attribute_internal, 4, 4, 0, 0)
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
	free (face->font_name);
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
	  XtSetValues (s->display.x->edit_widget, av, ac);
	}
      invalidate_face_cache (s);
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
      invalidate_face_cache (s);
      if (id == 0) /* the "default" face; update the ScreenWidget as well */
	{	   /* Possibly this isn't necessary for "foreground". */
	  Arg av[10];
	  int ac = 0;
	  XtSetArg (av[ac], XtNforeground, new_pixel); ac++;
	  XtSetValues (s->display.x->edit_widget, av, ac);
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
      invalidate_face_cache (s);
      if (id == 0) /* the "default" face; update the ScreenWidget as well */
	{
	  Arg av[10];
	  int ac = 0;
	  XtSetArg (av[ac], XtNbackground, new_pixel); ac++;
	  XtSetValues (s->display.x->edit_widget, av, ac);
	}
#endif /* HAVE_X_WINDOWS */
    }
  else if (EQ (attr_name, intern ("background-pixmap")))
    {
#ifdef HAVE_X_WINDOWS
      int w, h, d;
      unsigned long new_pixmap = load_pixmap (s, attr_value, &w, &h, &d);
      unload_pixmap (s, face->back_pixmap, face->pixmap_w, face->pixmap_h,
		     face->pixmap_depth);
      if (magic_p) new_pixmap = 0;
      face->back_pixmap = new_pixmap;
      face->pixmap_w = w;
      face->pixmap_h = h;
      face->pixmap_depth = d;
      invalidate_face_cache (s);
#endif /* HAVE_X_WINDOWS */
    }
  else if (EQ (attr_name, intern ("underline")))
    {
      int new = !NILP (attr_value);
      if (face->underline != new)
	invalidate_face_cache (s);
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
}
