/* X-specific Lisp objects: Pixmaps, Pixels, Cursors, and Fonts.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.

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

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>          /* for MAXPATHLEN */

#if NeXT
/* what is needed from here?  Do others need it too? */
# include <sys/fcntl.h>
#endif /* NeXT */

#include <X11/Xlib.h>
#include "xintrinsic.h"		/* for XtScreen... */
#ifdef HAVE_XMU
# include <X11/Xmu/Converters.h>
# include <X11/Xmu/CurUtil.h>
# include <X11/Xmu/Drawing.h>
#else
# include "xmu.h"
#endif

#ifdef HAVE_XPM
#include <X11/xpm.h>
#endif

#include "lisp.h"
#include "intl.h"
#include "lrecord.h"
#include "blockio.h"
#include "screen.h"
#include "xterm.h"
#include "xobjs.h"
#include "xselect.h"
#include "emacsfns.h"


#define LISP_SCREEN_TO_X_SCREEN(l) \
  XtScreen ((get_x_screen ((l)))->display.x->widget)

void
finalose (void *ptr)
{
  Lisp_Object obj; 
  XSET (obj, Lisp_Record, ptr);

  signal_error (Qerror, 
		list2 (build_string (GETTEXT
			(GETTEXT ("Can't dump an emacs containing window system objects"))),
		       obj));
}

Lisp_Object Qpixelp;
static Lisp_Object mark_pixel (Lisp_Object, void (*) (Lisp_Object));
static void print_pixel (Lisp_Object, Lisp_Object, int);
static void finalize_pixel (void *, int);
static int pixel_equal (Lisp_Object, Lisp_Object, int depth);
DEFINE_LRECORD_IMPLEMENTATION ("pixel", lrecord_pixel,
			       mark_pixel, print_pixel, finalize_pixel,
			       pixel_equal, sizeof(struct Lisp_Pixel));

static Lisp_Object
mark_pixel (Lisp_Object obj, void (*markobj) (Lisp_Object))
{
  struct Lisp_Pixel *p = XPIXEL (obj);
  return (p->color_name);
}

static void
print_pixel (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  char buf[100];
  struct Lisp_Pixel *p = XPIXEL (obj);
  if (print_readably)
    error (GETTEXT ("printing unreadable object #<pixel 0x%x>"), (long) p);
  write_string_1 ("#<pixel ", -1, printcharfun);
  print_internal (p->color_name, printcharfun, 0);
  sprintf (buf, " %ld=(%X,%X,%X) 0x%x>",
	   p->color.pixel,
	   p->color.red, p->color.green, p->color.blue,
	   (long) p);
  write_string_1 (buf, -1, printcharfun);
}

static void
finalize_pixel (void *header, int for_disksave)
{
  struct Lisp_Pixel *p = (struct Lisp_Pixel *) header;
  if (for_disksave) finalose (p);
  BLOCK_INPUT;
  XFreeColors (DisplayOfScreen (p->screen),
	       DefaultColormapOfScreen (p->screen),
	       &p->color.pixel, 1, 0);
  UNBLOCK_INPUT;
}

/* Pixels are equal if they resolve to the same color on the screen (have
   the same RGB values.)  I imagine that "same RGV values" == "same cell
   in the colormap."  Arguably we should be comparing their names instead. */
static int
pixel_equal (Lisp_Object o1, Lisp_Object o2, int depth)
{
/*  return (internal_equal (XPIXEL (o1)->color_name, XPIXEL (o2)->color_name,
			  depth + 1)); */
  return (XPIXEL (o1)->color.red   == XPIXEL (o2)->color.red &&
	  XPIXEL (o1)->color.green == XPIXEL (o2)->color.green &&
	  XPIXEL (o1)->color.blue   == XPIXEL (o2)->color.blue);
}


DEFUN ("make-pixel", Fmake_pixel, Smake_pixel, 1, 2, 0,
       "Creates a new `pixel' object of the specified color.\n\
The optional second argument is the screen on which to allocate the pixel\n\
 (in case some screens are running on different X servers.)\n\
This allocates a new color cell in the X server, and signals an error\n\
if the color is unknown or cannot be allocated.\n\
\n\
The returned object is a normal, first-class lisp object.  The way you\n\
`deallocate' the color is the way you deallocate any other lisp object:\n\
you drop all pointers to it and allow it to be garbage collected.  When\n\
these objects are GCed, the underlying X data is deallocated as well.")
  (name, screen)
  Lisp_Object name, screen;
{
  Display *dpy;
  Screen *xs;
  XColor color;
  Colormap cmap;
  int result;

  CHECK_STRING (name, 0);
  xs = LISP_SCREEN_TO_X_SCREEN (screen);
  dpy = DisplayOfScreen (xs);
  cmap = DefaultColormapOfScreen (xs);

  BLOCK_INPUT;
  memset (&color, 0, sizeof (color));
  result = XParseColor (dpy, cmap, (char *) XSTRING (name)->data, &color);
  UNBLOCK_INPUT;
  if (! result)
    signal_error (Qerror,
		  list2 (build_string (GETTEXT ("unrecognised color")), name));
  BLOCK_INPUT;
  result = XAllocColor (dpy, cmap, &color);
  UNBLOCK_INPUT;
  if (! result)
    signal_error (Qerror,
		  list2 (build_string (GETTEXT ("couldn't allocate color")),
			 name));

  {
    struct Lisp_Pixel *p = alloc_lcrecord (sizeof (struct Lisp_Pixel),
					   lrecord_pixel);
    Lisp_Object val;
    p->color_name = name;
    p->screen = xs;
    p->color = color;
    XSETR (val, Lisp_Pixel, p);
    return val;
  }
}

DEFUN ("pixelp", Fpixelp, Spixelp, 1, 1, 0,
       "Whether the given object is a pixel.")
  (obj)
  Lisp_Object obj;
{
  return (PIXELP (obj) ? Qt : Qnil);
}

DEFUN ("pixel-name", Fpixel_name, Spixel_name, 1, 1, 0,
       "Returns the name used to allocate the given pixel.")
  (pixel)
  Lisp_Object pixel;
{
  CHECK_PIXEL (pixel, 0);
  return (XPIXEL (pixel)->color_name);
}

Lisp_Object Qcursorp;
static Lisp_Object mark_cursor (Lisp_Object, void (*) (Lisp_Object));
static void print_cursor (Lisp_Object, Lisp_Object, int);
static void finalize_cursor (void *, int);
static int cursor_equal (Lisp_Object, Lisp_Object, int depth);
DEFINE_LRECORD_IMPLEMENTATION ("cursor", lrecord_cursor,
			       mark_cursor, print_cursor, finalize_cursor,
			       cursor_equal, sizeof(struct Lisp_Cursor));

static Lisp_Object
mark_cursor (Lisp_Object obj, void (*markobj) (Lisp_Object))
{
  struct Lisp_Cursor *c = XCURSOR (obj);
  ((markobj) (c->fg));
  ((markobj) (c->bg));
  return c->name;
}

static void
print_cursor (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  char buf[200];
  struct Lisp_Cursor *c = XCURSOR (obj);
  if (print_readably)
    error (GETTEXT ("printing unreadable object #<cursor 0x%x>"), (long) c);

  write_string_1 ("#<cursor ", -1, printcharfun);
  print_internal (c->name, printcharfun, 1);
  if (!NILP (c->fg))
    {
      write_string_1 (" (", -1, printcharfun);
      print_internal (XPIXEL (c->fg)->color_name, printcharfun, 0);
      write_string_1 ("/", -1, printcharfun);
      print_internal (XPIXEL (c->bg)->color_name, printcharfun, 0);
      write_string_1 (")", -1, printcharfun);
    }
  sprintf (buf, " 0x%x>", (long) c);
  write_string_1 (buf, -1, printcharfun);
}

static void
finalize_cursor (void *header, int for_disksave)
{
  struct Lisp_Cursor *c = (struct Lisp_Cursor *) header;
  if (for_disksave) finalose (c);
  BLOCK_INPUT;
  XFreeCursor (DisplayOfScreen (c->screen), c->cursor);
  UNBLOCK_INPUT;
}

/* Cursors are equal if their names are equal. */
static int
cursor_equal (Lisp_Object o1, Lisp_Object o2, int depth)
{
  return (internal_equal (XCURSOR (o1)->name, XCURSOR (o2)->name, depth + 1));
}

#ifdef USE_XMU_CURSORS

/* XmuCvtStringToCursor is bogus in the following ways:

   - When it can't convert the given string to a real cursor, it will
     sometimes return a "success" value, after triggering a BadPixmap
     error.  It then gives you a cursor that will itself generate BadCursor
     errors.  So we install this error handler to catch/notice the X error
     and take that as meaning "couldn't convert."

   - When you tell it to find a cursor file that doesn't exist, it prints
     an error message on stderr.  You can't make it not do that.

   - Also, using Xmu means we can't properly hack Lisp_Pixmap objects, or
     XPM files, or $XBMLANGPATH.
 */

static int XmuCvtStringToCursor_got_error;
static int XmuCvtStringToCursor_error_handler (dpy, error)
     Display *dpy;
     XErrorEvent *error;
{
  XmuCvtStringToCursor_got_error = 1;
  return 0;
}


Cursor 
make_cursor_1 (Lisp_Object screen, Lisp_Object name)
{
  int (*old_handler) ();
  XrmValue arg, from, to;
  Cardinal nargs = 1;
  Cursor cursor;
  Screen *xs = LISP_SCREEN_TO_X_SCREEN (screen);

  if (PIXMAPP (name))
    error (GETTEXT ("no support for converting lisp pixmaps to cursors."));
  CHECK_STRING (name, 0);

  arg.addr = (XtPointer) &xs;
  arg.size = sizeof (Screen *);
  from.addr = (XtPointer) (char *) XSTRING (name)->data;
  from.size = XSTRING (name)->size;
  to.addr = 0;
  to.size = 0;
  BLOCK_INPUT;
  XSync (DisplayOfScreen (xs), 0);
  XmuCvtStringToCursor_got_error = 0;
  old_handler = XSetErrorHandler (XmuCvtStringToCursor_error_handler);
  /* #### This fucker unconditionally writes an error message on stderr
     if it can't convert the cursor!  LOSERS!! */
  XmuCvtStringToCursor (&arg, &nargs, &from, &to);
  XSync (DisplayOfScreen (xs), 0);
  XSetErrorHandler (old_handler);
  UNBLOCK_INPUT;
  if (XmuCvtStringToCursor_got_error)
    cursor = 0;
  else if (to.addr)
    cursor = *((Cursor *) to.addr);
  else
    cursor = 0;

  return cursor;
}

#else /* !USE_XMU_CURSORS */

/* Duplicate the behavior of XmuCvtStringToCursor() to bypass its bogusness. */

static Lisp_Object locate_pixmap_file (Lisp_Object name);

static int XLoadFont_got_error;
static int XLoadFont_error_handler (dpy, error)
     Display *dpy;
     XErrorEvent *error;
{
  XLoadFont_got_error = 1;
  return 0;
}

static Font
safe_XLoadFont (Display *dpy, char *name)
{
  Font font;
  int (*old_handler) ();
  XLoadFont_got_error = 0;
  BLOCK_INPUT;
  XSync (dpy, 0);
  old_handler = XSetErrorHandler (XLoadFont_error_handler);
  font = XLoadFont (dpy, name);
  XSync (dpy, 0);
  XSetErrorHandler (old_handler);
  UNBLOCK_INPUT;
  if (XLoadFont_got_error) return 0;
  return font;
}

/* Check that this server supports cursors of this size. */
static void
check_pointer_sizes (Screen *xs, unsigned int width, unsigned int height,
		     Lisp_Object name, Lisp_Object object)
{
  unsigned int best_width, best_height;
  BLOCK_INPUT;
  if (! XQueryBestCursor (DisplayOfScreen (xs), RootWindowOfScreen (xs),
			  width, height, &best_width, &best_height))
    /* #### What does it mean when XQueryBestCursor() returns 0?
       I can't find that documented anywhere. */
    best_width = best_height = 0;
  UNBLOCK_INPUT;

  if (width > best_width || height > best_height)
    {
      char buf [255];
      sprintf (buf, "cursor too large (%dx%d): "
	       "server requires %dx%d or smaller",
	       width, height, best_width, best_height);
      signal_error (Qerror, list3 (build_string (buf), name, object));
    }
}


Cursor 
make_cursor_1 (Lisp_Object screen, Lisp_Object name)
{
  Screen *xs = LISP_SCREEN_TO_X_SCREEN (screen);
  Display *dpy = DisplayOfScreen (xs);
  XColor fg, bg;
  Cursor cursor;
  int i;

  fg.pixel = bg.pixel = 0;
  fg.red = fg.green = fg.blue = 0;
  bg.red = bg.green = bg.blue = ~0;

  if (STRINGP (name) &&
      !strncmp ("FONT ", (char *) XSTRING (name)->data, 5))
    {
      Font source, mask;
      char source_name [MAXPATHLEN], mask_name [MAXPATHLEN], dummy;
      int source_char, mask_char;
      int count = sscanf ((char *) XSTRING (name)->data,
			  "FONT %s %d %s %d %c",
			  source_name, &source_char,
			  mask_name, &mask_char, &dummy);
      /* Allow "%s %d %d" as well... */
      if (count == 3 && (1 == sscanf (mask_name, "%d %c", &mask_char, &dummy)))
	count = 4, mask_name[0] = 0;

      if (count != 2 && count != 4)
	signal_simple_error ("invalid cursor specification", name);
      BLOCK_INPUT;
      source = safe_XLoadFont (dpy, source_name);
      UNBLOCK_INPUT;
      if (! source)
	signal_simple_error_2 ("couldn't load font",
			       build_string (source_name),
			       name);
      if (count == 2)
	mask = 0;
      else if (! mask_name[0])
	mask = source;
      else
	{
	  BLOCK_INPUT;
	  mask = safe_XLoadFont (dpy, mask_name);
	  UNBLOCK_INPUT;
	  if (! mask) /* continuable */
	    Fsignal (Qerror, list3 (build_string ("couldn't load font"),
				    build_string (mask_name), name));
	}
      if (! mask) mask_char = 0;

      /* #### call XQueryTextExtents() and check_pointer_sizes() here. */

      BLOCK_INPUT;
      cursor = XCreateGlyphCursor (dpy, source, mask, source_char, mask_char,
				   &fg, &bg);
      XUnloadFont (dpy, source);
      if (mask && mask != source) XUnloadFont (dpy, mask);
      UNBLOCK_INPUT;
    }

  else if (STRINGP (name) &&
	   (i = XmuCursorNameToIndex ((char *) XSTRING (name)->data)) != -1)
    {
      BLOCK_INPUT;
      cursor = XCreateFontCursor (dpy, i);
      UNBLOCK_INPUT;
    }

  else
    {
      struct gcpro gcpro1, gcpro2, gcpro3;
      Lisp_Object lsource;
      Lisp_Object lmask = Qnil;
      Lisp_Object mask_file = Qnil;
      Pixmap source, mask;

      GCPRO3 (lsource, lmask, mask_file);

      if (PIXMAPP (name))
	lsource = name;
      else
	lsource = make_pixmap (name, screen, 1); /* may GC */

      source = XPIXMAP (lsource)->pixmap;
      mask = XPIXMAP (lsource)->mask;

      if (XPIXMAP (lsource)->depth > 1)
	signal_error (Qerror,
		      list3 (build_string ("cursor pixmaps must be 1 plane"),
			     name, lsource));
      if (!mask && STRINGP (name))
	{
	  mask_file =
	    locate_pixmap_file (concat2 (name, build_string ("Mask")));
	  if (NILP (mask_file))
	    mask_file =
	      locate_pixmap_file (concat2 (name, build_string ("msk")));
	  if (!NILP (mask_file))
	    {
	      lmask = make_pixmap (mask_file, screen, 1); /* may GC */
	      if (XPIXMAP (lmask)->depth != 0)
		signal_simple_error_2 ("mask must be 1 bit deep",
				       mask_file, lmask);
	      mask = XPIXMAP (lmask)->pixmap;
	      mask_file = Qnil;
	    }
	}

      check_pointer_sizes (xs,
			   XPIXMAP (lsource)->width,
			   XPIXMAP (lsource)->height,
			   name, lsource);

      /* If the loaded pixmap has colors allocated (meaning it came from an
	 XPM file), then use those as the default colors for the cursor we
	 create.  Otherwise, default to black and white.
       */
      if (XPIXMAP (lsource)->npixels >= 2)
	{
	  int i;
	  int npixels = XPIXMAP (lsource)->npixels;
	  unsigned long *pixels = XPIXMAP (lsource)->pixels;

	  /* With an XBM file, it's obvious which bit is foreground and which
	     is background, or rather, it's implicit: in an XBM file, a 1 bit
	     is foreground, and a 0 bit is background.

	     XCreatePixmapCursor() assumes this property of the pixmap it is
	     called with as well; the `foreground' color argument is used for
	     the 1 bits.

	     With an XPM file, it's tricker, since the elements of the pixmap
	     don't represent FG and BG, but are actual pixel values.  So we
	     need to figure out which of those pixels is the foreground color
	     and which is the background.  We do it by comparing RGB and
	     assuming that the darker color is the foreground.  This works
	     with the result of xbmtopbm|ppmtoxpm, at least.

	     It might be nice if there was some way to tag the colors in the
	     XPM file with whether they are the foreground - perhaps with
	     logical color names somehow?

	     Once we have decided which color is the foreground, we need to
	     ensure that that color corresponds to a `1' bit in the Pixmap.
	     The XPM library wrote into the (1-bit) pixmap with XPutPixel,
	     which will ignore all but the least significant bit.

	     This means that a 1 bit in the image corresponds to `fg' only if
	     `fg.pixel' is odd.

	     (This also means that the image will be all the same color if
	     both `fg' and `bg' are odd or even, but we can safely assume
	     that that won't happen if the XPM file is sensible I think.)

	     The desired result is that the image use `1' to represent the
	     foreground color, and `0' to represent the background color.
	     So, we may need to invert the image to accomplish this; we invert
	     if fg is odd (remember that WhitePixel and BlackPixel are not
	     necessarily 1 and 0 respectively, though I think it might be safe
	     to assume that one of them is always 1 and the other is always 0.
	     We also pretty much need to assume that one is even and the other
	     is odd.)
	   */

	  fg.pixel = pixels [0];	/* pick a pixel at random. */
	  bg.pixel = fg.pixel;
	  for (i = 1; i < npixels; i++)	/* Look for an "other" pixel value. */
	    {
	      bg.pixel = pixels [i];
	      if (fg.pixel != bg.pixel) break;
	    }

	  /* If (fg.pixel == bg.pixel) then probably something has gone wrong,
	     but I don't think signalling an error would be appropriate. */

	  BLOCK_INPUT;
	  XQueryColor (DisplayOfScreen(xs), DefaultColormapOfScreen(xs), &fg);
	  XQueryColor (DisplayOfScreen(xs), DefaultColormapOfScreen(xs), &bg);
	  UNBLOCK_INPUT;

	  /* If the foreground is lighter than the background, swap them.
	     (This occurs semi-randomly, depending on the ordering of the
	     color list in the XPM file.)
	   */
	  if (((fg.red / 3) + (fg.green / 3) + (fg.blue / 3)) >
	      ((bg.red / 3) + (bg.green / 3) + (bg.blue / 3)))
	    {
	      XColor swap;
	      swap = fg;
	      fg = bg;
	      bg = swap;
	    }

	  /* If the fg pixel corresponds to a `0' in the bitmap, invert it.
	     (This occurs (only?) on servers with Black=0, White=1.)
	   */
	  if ((fg.pixel & 1) == 0)
	    {
	      XGCValues gcv;
	      GC gc;
	      gcv.function = GXxor;
	      gcv.foreground = 1;
	      BLOCK_INPUT;
	      gc = XCreateGC (dpy, source, (GCFunction | GCForeground), &gcv);
	      XFillRectangle (dpy, source, gc, 0, 0,
			      XPIXMAP (lsource)->width,
			      XPIXMAP (lsource)->height);
	      XFreeGC (dpy, gc);
	      UNBLOCK_INPUT;
	    }
	}

      BLOCK_INPUT;
      cursor = XCreatePixmapCursor (dpy, source, mask, &fg, &bg,
				    XPIXMAP (lsource)->x,
				    XPIXMAP (lsource)->y);
      UNBLOCK_INPUT;
      UNGCPRO; /* can now collect and free `lsource', `lmask', and Pixmaps. */
    }
  return cursor;
}

#endif /* !USE_XMU_CURSORS */



DEFUN ("make-cursor", Fmake_cursor, Smake_cursor, 1, 4, 0,
       "Creates a new `cursor' object of the specified name.\n\
The optional second and third arguments are the foreground and background\n\
 colors.  They may be color name strings or `pixel' objects.\n\
The optional fourth argument is the screen on which to allocate the cursor\n\
 (in case some screens are running on different X servers.)\n\
This allocates a new cursor in the X server, and signals an error if the\n\
 cursor is unknown or cannot be allocated.\n\
\n\
A cursor name can take many different forms.  It can be:\n\
 - any of the standard cursor names from appendix B of the Xlib manual\n\
   (also known as the file <X11/cursorfont.h>) minus the XC_ prefix;\n\
 - the name of a font, and glyph index into it of the form\n\
   \"FONT fontname index [[mask-font] mask-index]\";\n\
 - the name of a bitmap or pixmap file;\n\
 - or a pixmap object, as returned by `make-pixmap'.\n\
\n\
If it is a pixmap or pixmap file, and that pixmap comes with a mask, then\n\
 that mask will be used.  If it is a pixmap, it must have only one plane,\n\
 since X cursors may only have two colors.  If it is a pixmap file, then\n\
 the file will be read in monochrome.\n\
\n\
If it is a bitmap file, and if a bitmap file whose name is the name of the\n\
 cursor with \"msk\" or \"Mask\" appended exists, then that second bitmap\n\
 will be used as the mask.  For example, a pair of files might be named\n\
 \"cursor.xbm\" and \"cursor.xbmmsk\".\n\
\n\
The returned object is a normal, first-class lisp object.  The way you\n\
`deallocate' the cursor is the way you deallocate any other lisp object:\n\
you drop all pointers to it and allow it to be garbage collected.  When\n\
these objects are GCed, the underlying X data is deallocated as well.")
  (name, fg, bg, screen)
  Lisp_Object name, fg, bg, screen;
{
  Screen *xs = LISP_SCREEN_TO_X_SCREEN (screen);
  Cursor cursor;

  if ((NILP (fg)) != (NILP (bg)))
    error (GETTEXT ("must specify both foreground and background, or neither."));

  if (STRINGP (fg))
    fg = Fmake_pixel (fg, screen);
  else if (!NILP (fg) || PIXELP (fg))
    CHECK_STRING (fg, 0);

  if (STRINGP (bg))
    bg = Fmake_pixel (bg, screen);
  else if (!NILP (bg) || PIXELP (bg))
    CHECK_STRING (bg, 0);

  cursor = make_cursor_1 (screen, name);

  if (! cursor)
    signal_error (Qerror, list2 (build_string (GETTEXT ("unknown cursor")), name));

  /* Got the cursor, now color it in.
     (Either both are specified or neither.) */
  if (!NILP (fg))
    XRecolorCursor (DisplayOfScreen (xs), cursor,
		    &(XPIXEL (fg)->color),
		    &(XPIXEL (bg)->color));
  
  /* Now make the lisp object. */
  {
    struct Lisp_Cursor *c = alloc_lcrecord (sizeof (struct Lisp_Cursor),
					    lrecord_cursor);
    Lisp_Object val;
    c->screen = xs;
    c->name = name;
    c->cursor = cursor;
    c->fg = fg;
    c->bg = bg;
    XSETR (val, Lisp_Cursor, c);
    return val;
  }
}

DEFUN ("cursorp", Fcursorp, Scursorp, 1, 1, 0,
       "Whether the given object is a cursor.")
  (obj)
  Lisp_Object obj;
{
  return (CURSORP (obj) ? Qt : Qnil);
}

DEFUN ("cursor-name", Fcursor_name, Scursor_name, 1, 1, 0,
       "Returns the name used to allocate the given cursor.")
  (cursor)
  Lisp_Object cursor;
{
  CHECK_CURSOR (cursor, 0);
  return (XCURSOR (cursor)->name);
}

DEFUN ("cursor-foreground", Fcursor_foreground, Scursor_foreground, 1, 1, 0,
   "Returns the foreground color of the given cursor, or nil if unspecified.")
  (cursor)
  Lisp_Object cursor;
{
  CHECK_CURSOR (cursor, 0);
  return (XCURSOR (cursor)->fg);
}

DEFUN ("cursor-background", Fcursor_background, Scursor_background, 1, 1, 0,
   "Returns the background color of the given cursor, or nil if unspecified.")
  (cursor)
  Lisp_Object cursor;
{
  CHECK_CURSOR (cursor, 0);
  return (XCURSOR (cursor)->bg);
}

Lisp_Object Qfontp;
static Lisp_Object mark_font (Lisp_Object, void (*) (Lisp_Object));
static void print_font (Lisp_Object, Lisp_Object, int);
static void finalize_font (void *, int);
static int font_equal (Lisp_Object o1, Lisp_Object o2, int depth);
DEFINE_LRECORD_IMPLEMENTATION ("font", lrecord_font,
			       mark_font, print_font, finalize_font,
			       font_equal, sizeof (struct Lisp_Font));

static Lisp_Object font_truename (Lisp_Object font, int no_error);

static Lisp_Object
mark_font (Lisp_Object obj, void (*markobj) (Lisp_Object))
{
  ((markobj) (XFONT (obj)->truename));
  return XFONT (obj)->name;
}

static void
print_font (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  char buf[200];
  struct Lisp_Font *f = XFONT (obj);
  if (print_readably)
#ifdef I18N4
    error (GETTEXT ("printing unreadable object #<font set>"));
#else
    error (GETTEXT ("printing unreadable object #<font 0x%x>"), f->font->fid);
#endif
  write_string_1 ("#<font ", -1, printcharfun);
  print_internal (f->name, printcharfun, 0);
#ifdef I18N4
  sprintf (buf, " set>");
#else
  sprintf (buf, " 0x%lx>", (unsigned long) f->font->fid);
#endif
  write_string_1 (buf, -1, printcharfun);
}

static void
finalize_font (void *header, int for_disksave)
{
  struct Lisp_Font *f = (struct Lisp_Font *) header;
  if (! f->font) /* font is 0 on tty screens */
    return;
  if (for_disksave) finalose (f);
  BLOCK_INPUT;
#ifdef I18N4
  XFreeFontSet (DisplayOfScreen (f->screen), f->font);
#else
  XFreeFont (DisplayOfScreen (f->screen), f->font);
#endif
  UNBLOCK_INPUT;
}

/* Fonts are equal if they resolve to the same name.
   Since we call `font-truename' to do this, and since font-truename is lazy,
   this means the `equal' could cause XListFonts to be run the first time.
 */
static int
font_equal (Lisp_Object o1, Lisp_Object o2, int depth)
{
  return (internal_equal (font_truename (o1, 1), font_truename (o2, 1),
			  depth + 1));
}


DEFUN ("make-font", Fmake_font, Smake_font, 1, 2, 0,
       "Creates a new `font' object of the specified name.\n\
The optional second argument is the screen on which to allocate the font\n\
 (in case some screens are running on different X servers.)\n\
This allocates a font in the X server, and signals an error if the font is\n\
 unknown or cannot be allocated.\n\
\n\
The returned object is a normal, first-class lisp object.  The way you\n\
`deallocate' the font is the way you deallocate any other lisp object:\n\
you drop all pointers to it and allow it to be garbage collected.  When\n\
these objects are GCed, the underlying X data is deallocated as well.")
  (name, screen)
  Lisp_Object name, screen;
{
  Screen *xs;
#ifdef I18N4
  XFontSet xf;
  char **missing_charset_list = 0;
  int missing_charset_count = 0;
  char *def_string = "";
#else
  XFontStruct *xf;
#endif

  CHECK_STRING (name, 0);
  xs = LISP_SCREEN_TO_X_SCREEN (screen);
  BLOCK_INPUT;
#ifdef I18N4
  xf = XCreateFontSet (DisplayOfScreen (xs), (char *) XSTRING (name)->data,
		       &missing_charset_list, &missing_charset_count,
		       &def_string);
#else
  xf = XLoadQueryFont (DisplayOfScreen (xs), (char *) XSTRING (name)->data);
#endif
  UNBLOCK_INPUT;
  if (! xf)
#ifdef I18N4
    {
      if ( !missing_charset_list)
	signal_error (Qerror,
		      list2 (build_string (GETTEXT ("couldn't load font set")),
			     name));
      else
	{
	  Lisp_Object lost = Qnil;
	  int i;
	  for (i = missing_charset_count-1; i >= 0; i--)
	    lost = Fcons (build_string (missing_charset_list[i]),
			  lost);
	  signal_error (Qerror,
			list3 (build_string
			       (GETTEXT ("couldn't load font set (missing required fonts)")),
			       name, lost));
	}
    }
#else
    signal_error (Qerror, list2 (build_string (GETTEXT ("couldn't load font")),
				 name));
#endif

#ifdef I18N4
  if (!XExtentsOfFontSet(xf)->max_logical_extent.width)
#else
  if (!xf->max_bounds.width)
#endif
    {
      /* yes, this has been known to happen. */
#ifdef I18N4
      XFreeFontSet (DisplayOfScreen (xs), xf);
#else
      XFreeFont (DisplayOfScreen (xs), xf);
#endif
      signal_error (Qerror,
		    list2 (build_string (GETTEXT ("font is too small")),
			   name));
    }

  {
    struct Lisp_Font *lf;
    Lisp_Object val;
    unsigned int def_char;

    lf = alloc_lcrecord (sizeof (struct Lisp_Font), lrecord_font);
    lf->name = name;
    lf->truename = Qnil;	/* initialized on demand */
    lf->screen = xs;
    lf->font = xf;
#ifdef I18N4
    lf->height = XExtentsOfFontSet(xf)->max_logical_extent.height;
    lf->ascent = -XExtentsOfFontSet(xf)->max_logical_extent.y;
    lf->descent = lf->height - lf->ascent;
#else
    lf->ascent = xf->ascent;
    lf->descent = xf->descent;
    lf->height = xf->ascent + xf->descent;
#endif
#ifdef I18N4
    def_char = 'N';
    lf->width = XExtentsOfFontSet(xf)->max_logical_extent.width;
#else
    /* Old versions of the R5 font server have garbage (>63k) as def_char. */
    def_char = ((xf->default_char >= xf->min_char_or_byte2 &&
		 xf->default_char <= xf->max_char_or_byte2)
		? xf->default_char
		: 'N');
  once_more:
    lf->width = (xf->per_char
		 /* #### what are we supposed to do with byte1 here? */
		 ? xf->per_char [def_char - xf->min_char_or_byte2].width
		 : xf->max_bounds.width);

    /* Some fonts have a default char whose width is 0.  This is no good.
       If that's the case, first try 'N' as the default char, and if N has
       0 width too (unlikely) then just use the max width. */
    if (lf->width == 0)
      {
	if (def_char == 'N')
	  lf->width = xf->max_bounds.width;
	else
	  {
	    def_char = 'N';
	    goto once_more;
	  }
      }
#endif
#ifdef I18N4
    lf->proportional_p = 1;
#else
    /* If all characters don't exist then there could potentially be
       0-width characters lurking out there.  Not setting this flag
       trips an optimization that would make them appear to have width
       to redisplay.  This is bad.  So we set it if not all characters
       have the same width or if not all characters are defined.
     */
    lf->proportional_p = (xf->min_bounds.width != xf->max_bounds.width ||
			  !xf->all_chars_exist);
#endif
    XSETR (val, Lisp_Font, lf);
    return val;
  }
}

DEFUN ("fontp", Ffontp, Sfontp, 1, 1, 0,
       "Whether the given object is a font.")
  (obj)
  Lisp_Object obj;
{
  return (FONTP (obj) ? Qt : Qnil);
}

DEFUN ("font-name", Ffont_name, Sfont_name, 1, 1, 0,
       "Returns the name used to allocate the given font.")
  (font)
  Lisp_Object font;
{
  CHECK_FONT (font, 0);
  return (XFONT (font)->name);
}



/* Determining the truename of a font is hard.  (Big surprise.)

   By "truename" we mean an XLFD-form name which contains no wildcards, yet
   which resolves to *exactly* the same font as the one which we already have
   the (probably wildcarded) name and `XFontStruct' of.

   One might think that the first font returned by XListFonts would be the one
   that XOpenFont would pick.  Apparently this is the case on some servers,
   but not on others.  It would seem not to be specified.

   The MIT R5 server sometimes appears to be picking the lexicographically
   smallest font which matches the name (thus picking "adobe" fonts before
   "bitstream" fonts even if the bitstream fonts are earlier in the path, and
   also picking 100dpi adobe fonts over 75dpi adobe fonts even though the
   75dpi are in the path earlier) but sometimes appears to be doing something
   else entirely (for example, removing the bitsream fonts from the path will
   cause the 75dpi adobe fonts to be used instead of the100dpi, even though
   their relative positions in the path (and their names!) have not changed.)

   The documentation for XSetFontPath() seems to indicate that the order of
   entries in the font path means something, but it's pretty noncommital about
   it, and the spirit of the law is apparently not being obeyed...

   All the fonts I've seen have a property named `FONT' which contains the
   truename of the font.  However, there are two problems with using this: the
   first is that the X Protocol Document is quite explicit that all properties
   are optional, so we can't depend on it being there.  The second is that
   it's concievable that this alleged truename isn't actually accessible as a
   font, due to some difference of opinion between the font designers and
   whoever installed the font on the system.

   So, our first attempt is to look for a FONT property, and then verify that
   the name there is a valid name by running XListFonts on it.  There's still
   the potential that this could be true but we could still be being lied to,
   but that seems pretty remote.

     Late breaking news: I've gotten reports that SunOS 4.1.3U1
     with OpenWound 3.0 has a font whose truename is really
     "-Adobe-Courier-Medium-R-Normal--12-120-75-75-M-70-ISO8859-1"
     but whose FONT property contains "Courier".

     So we disbelieve the FONT property unless it begins with a dash and
     is more than 30 characters long.  X Windows: The defacto substandard.
     X Windows: Complex nonsolutions to simple nonproblems.  X Windows:
     Live the nightmare.

   If the FONT property doesn't exist, then we try and construct an XLFD name
   out of the other font properties (FOUNDRY, FAMILY_NAME, WEIGHT_NAME, etc).
   This is necessary at least for some versions of OpenWound.  But who knows
   what the future will bring.

   If that doesn't work, then we use XListFonts and either take the first font
   (which I think is the most sensible thing) or we find the lexicographically
   least, depending on whether the preprocessor constant `XOPENFONT_SORTS' is
   defined.  This sucks because the two behaviors are a property of the server
   being used, not the architecture on which emacs has been compiled.  Also,
   as I described above, sorting isn't ALWAYS what the server does.  Really it
   does something seemingly random.  There is no reliable way to win if the
   FONT property isn't present.

   Another possibility which I haven't bothered to implement would be to map
   over all of the matching fonts and find the first one that has the same
   character metrics as the font we already have loaded.  Even if this didn't
   return exactly the same font, it would at least return one whose characters
   were the same sizes, which would probably be good enough.

   More late-breaking news: on RS/6000 AIX 3.2.4, the expression
        XLoadQueryFont (dpy, "-*-Fixed-Medium-R-*-*-*-130-75-75-*-*-ISO8859-1")
   actually returns the font
        -Misc-Fixed-Medium-R-Normal--13-120-75-75-C-80-ISO8859-1
   which is crazy, because that font doesn't even match that pattern!  It is
   also not included in the output produced by `xlsfonts' with that pattern.

   So this is yet another example of XListFonts() and XOpenFont() using
   completely different algorithms.  This, however, is a goofier example of
   this bug, because in this case, it's not just the search order that is 
   different -- the sets don't even intersect.

   If anyone has any better ideas how to do this, or any insights on what it is
   that the various servers are actually doing, please let me know!  -- jwz. */


static int
valid_font_name_p (Display *dpy, char *name)
{
  /* Maybe this should be implemented by callign XLoadFont and trapping
     the error.  That would be a lot of work, and wasteful as hell, but
     might be more correct.
   */
  int nnames = 0;
  char **names;
  if (! name)
    return 0;
  BLOCK_INPUT;
  names = XListFonts (dpy, name, 1, &nnames);
  if (names)
    XFreeFontNames (names);
  UNBLOCK_INPUT;
  return (nnames != 0);
}

static char *
truename_via_FONT_prop (Screen *xs, XFontStruct *font)
{
  Display *dpy = DisplayOfScreen (xs);
  unsigned long value = 0;
  char *result = 0;
  BLOCK_INPUT;
  if (XGetFontProperty (font, XA_FONT, &value))
    result = XGetAtomName (dpy, value);
  /* result is now 0, or the string value of the FONT property. */
  if (result)
    {
      /* Verify that result is an XLFD name (roughly...) */
      if (result [0] != '-' || strlen (result) < 30)
	{
	  XFree (result);
	  result = 0;
	}
    }
  UNBLOCK_INPUT;
  return result;	/* this must be freed by caller if non-0 */
}

Atom Xatom_FOUNDRY, Xatom_FAMILY_NAME, Xatom_WEIGHT_NAME, Xatom_SLANT,
  Xatom_SETWIDTH_NAME, Xatom_ADD_STYLE_NAME, Xatom_PIXEL_SIZE, 
  Xatom_POINT_SIZE, Xatom_RESOLUTION_X, Xatom_RESOLUTION_Y, 
  Xatom_SPACING, Xatom_AVERAGE_WIDTH, Xatom_CHARSET_REGISTRY,
  Xatom_CHARSET_ENCODING;

static char *
truename_via_random_props (Screen *xs, XFontStruct *font)
{
  Display *dpy = DisplayOfScreen (xs);
  unsigned long value = 0;
  char *foundry, *family, *weight, *slant, *setwidth, *add_style;
  unsigned long pixel, point, res_x, res_y;
  char *spacing;
  unsigned long avg_width;
  char *registry, *encoding;
  char composed_name [2048];
  int ok = 0;
  BLOCK_INPUT;
#define get_string(atom,var)				\
  if (XGetFontProperty (font, (atom), &value))		\
    var = XGetAtomName (dpy, value);			\
  else							\
    goto FAIL;
#define get_number(atom,var)				\
  if (!XGetFontProperty (font, (atom), &var) ||		\
      var > 999)					\
    goto FAIL;
  get_string (Xatom_FOUNDRY, foundry);
  get_string (Xatom_FAMILY_NAME, family);
  get_string (Xatom_WEIGHT_NAME, weight);
  get_string (Xatom_SLANT, slant);
  get_string (Xatom_SETWIDTH_NAME, setwidth);
  get_string (Xatom_ADD_STYLE_NAME, add_style);
  get_number (Xatom_PIXEL_SIZE, pixel);
  get_number (Xatom_POINT_SIZE, point);
  get_number (Xatom_RESOLUTION_X, res_x);
  get_number (Xatom_RESOLUTION_Y, res_y);
  get_string (Xatom_SPACING, spacing);
  get_number (Xatom_AVERAGE_WIDTH, avg_width);
  get_string (Xatom_CHARSET_REGISTRY, registry);
  get_string (Xatom_CHARSET_ENCODING, encoding);
#undef get_number
#undef get_string

  sprintf (composed_name,
	   "-%s-%s-%s-%s-%s-%s-%ld-%ld-%ld-%ld-%s-%ld-%s-%s",
	   foundry, family, weight, slant, setwidth, add_style, pixel,
	   point, res_x, res_y, spacing, avg_width, registry, encoding);
  ok = 1;

 FAIL:
  UNBLOCK_INPUT;
  if (ok)
    {
      int L = strlen (composed_name) + 1;
      char *result = malloc (L);
      strncpy (result, composed_name, L);
      return result;
    }
  else
    return 0;
}


/* Unbounded, for sufficiently small values of infinity... */
#define MAX_FONT_COUNT 5000

static char *
truename_via_XListFonts (Screen *xs, char *font_name)
{
  Display *dpy = DisplayOfScreen (xs);
  char *result = 0;
  char **names;
  int count = 0;
  BLOCK_INPUT;

#ifndef XOPENFONT_SORTS
  /* In a sensible world, the first font returned by XListFonts()
     would be the font that XOpenFont() would use.  */
  names = XListFonts (dpy, font_name, 1, &count);
  if (count) result = names [0];
#else
  /* But the world I live in is much more perverse. */
  names = XListFonts (dpy, font_name, MAX_FONT_COUNT, &count);
  while (count--)
    /* If names[count] is lexicographically less than result, use it.
       (#### Should we be comparing case-insensitively?) */
    if (result == 0 || (strcmp (result, names [count]) < 0))
      result = names [count];
#endif

  if (result)
    result = xstrdup (result);
  if (names)
    XFreeFontNames (names);

  UNBLOCK_INPUT;
  return result;	/* this must be freed by caller if non-0 */
}


static Lisp_Object
x_font_truename (Screen *screen, char *name, XFontStruct *font)
{
  Display *dpy = DisplayOfScreen (screen);
  char *truename_FONT = 0;
  char *truename_random = 0;
  char *truename = 0;
  
  /* The search order is:
     - if FONT property exists, and is a valid name, return it.
     - if the other props exist, and add up to a valid name, return it.
     - if we find a matching name with XListFonts, return it.
     - if FONT property exists, return it regardless.
     - if other props exist, return the resultant name regardless.
     - else return 0.
   */

  truename = truename_FONT = truename_via_FONT_prop (screen, font);
  if (truename && !valid_font_name_p (dpy, truename))
    truename = 0;
  if (!truename)
    truename = truename_random = truename_via_random_props (screen, font);
  if (truename && !valid_font_name_p (dpy, truename))
    truename = 0;
  if (!truename && name)
    truename = truename_via_XListFonts (screen, name);

  if (!truename)
    {
      /* Gag - we weren't able to find a seemingly-valid truename.
	 Well, maybe we're on one of those braindead systems where
	 XListFonts() and XLoadFont() are in violent disagreement.
	 If we were able to compute a truename, try using that even
	 if evidence suggests that it's not a valid name - because
	 maybe it is, really, and that's better than nothing.
	 X Windows: You'll envy the dead.
       */
      if (truename_FONT)
	truename = truename_FONT;
      else if (truename_random)
	truename = truename_random;
    }

  BLOCK_INPUT;
  /* One or both of these are not being used - free them. */
  if (truename_FONT && truename_FONT != truename)
    XFree (truename_FONT);
  if (truename_random && truename_random != truename)
    XFree (truename_random);
  UNBLOCK_INPUT;

  if (truename)
    {
      Lisp_Object result = build_string (truename);
      xfree (truename);
      return result;
    }
  else
    return Qnil;
}

static Lisp_Object
font_truename (Lisp_Object font, int no_error)
{
  CHECK_FONT (font, 0);
  if (NILP (XFONT (font)->truename))
    {
      char *name = (char *) XSTRING (XFONT (font)->name)->data;
#ifdef I18N4
      {
	XFontStruct **fonts;
	char **names;
	int count = XFontsOfFontSet (XFONT (font)->font, &fonts, &names);
	Lisp_Object *lnames =
	  (Lisp_Object *) alloca (count * 2 * sizeof (Lisp_Object));
	Lisp_Object comma = build_string (","); /* Q'ing this would be silly */
	int i, j;
	for (i = 0, j = 0; i < count; i++)
	  {
	    Lisp_Object tn = x_font_truename (XFONT (font)->screen,
					      names [i], fonts [i]);
	    if (NILP (tn)) tn = build_string (names [i]);
	    if (i != 0) lnames [j++] = comma;
	    lnames [j++] = tn;
	  }
	XFONT (font)->truename = Fconcat (j, lnames);
      }
#else
      XFONT (font)->truename = x_font_truename (XFONT (font)->screen, name,
						XFONT (font)->font);
#endif
      if (NILP (XFONT (font)->truename))
	{
	  if (no_error)
	    /* Ok, just this once, return the font name as the truename.
	       (This is only used by Fequal() right now.) */
	    return XFONT (font)->name;
	  else
	    signal_simple_error ("couldn't determine font truename", font);
	}
    }
  return (XFONT (font)->truename);
}

DEFUN ("font-truename", Ffont_truename, Sfont_truename, 1, 1, 0,
       "Returns the canonical name of the given font.\n\
Font names are patterns which may match any number of fonts, of which\n\
the first found is used.  This returns an unambiguous name for that font\n(\
but not necessarily its only unambiguous name.)")
  (font)
  Lisp_Object font;
{
  return font_truename (font, 0);
}

DEFUN ("x-list-fonts", Fx_list_fonts, Sx_list_fonts, 1, 2, 0,
       "Returns a list of font names matching the given pattern.")
  (pattern, screen)
  Lisp_Object pattern, screen;
{
  Screen *xs;
  char **names;
  int count = 0;
  Lisp_Object result = Qnil;
  CHECK_STRING (pattern, 0);
  xs = LISP_SCREEN_TO_X_SCREEN (screen);
  BLOCK_INPUT;
  names = XListFonts (DisplayOfScreen (xs),
		      (char *) XSTRING (pattern)->data,
		      MAX_FONT_COUNT, &count);
  while (count--)
    result = Fcons (build_string (names [count]), result);
  if (names)
    XFreeFontNames (names);
  UNBLOCK_INPUT;
  return result;
}


#ifndef I18N4
DEFUN ("x-font-properties", Fx_font_properties, Sx_font_properties, 1, 1, 0,
       "Returns the properties (an alist) of the given font.")
  (font)
  Lisp_Object font;
{
  int i;
  Lisp_Object result = Qnil;
  XFontProp *props;
  Display *dpy;
  CHECK_FONT (font, 0);
  dpy = DisplayOfScreen (XFONT (font)->screen);
  props = XFONT (font)->font->properties;
  for (i = XFONT (font)->font->n_properties - 1; i >= 0; i--)
    {
      char *name_str = 0;
      char *val_str = 0;
      Lisp_Object name, value;
      Atom atom = props [i].name;
      BLOCK_INPUT;
      name_str = XGetAtomName (dpy, atom);
      UNBLOCK_INPUT;
      name = (name_str ? intern (name_str) : Qnil);
      if (name_str &&
	  (atom == XA_FONT ||
	   atom == Xatom_FOUNDRY ||
	   atom == Xatom_FAMILY_NAME ||
	   atom == Xatom_WEIGHT_NAME ||
	   atom == Xatom_SLANT ||
	   atom == Xatom_SETWIDTH_NAME ||
	   atom == Xatom_ADD_STYLE_NAME ||
	   atom == Xatom_SPACING ||
	   atom == Xatom_CHARSET_REGISTRY ||
	   atom == Xatom_CHARSET_ENCODING ||
	   !strcmp (name_str, "CHARSET_COLLECTIONS") ||
	   !strcmp (name_str, "FONTNAME_REGISTRY") ||
	   !strcmp (name_str, "CLASSIFICATION") ||
	   !strcmp (name_str, "COPYRIGHT") ||
	   !strcmp (name_str, "DEVICE_FONT_NAME") ||
	   !strcmp (name_str, "FULL_NAME") ||
	   !strcmp (name_str, "MONOSPACED") ||
	   !strcmp (name_str, "QUALITY") ||
	   !strcmp (name_str, "RELATIVE_SET") ||
	   !strcmp (name_str, "RELATIVE_WEIGHT") ||
	   !strcmp (name_str, "STYLE")))
	{
	  val_str = XGetAtomName (dpy, props [i].card32);
	  value = (val_str ? build_string (val_str) : Qnil);
	}
      else
	value = make_number (props [i].card32);
      BLOCK_INPUT;
      if (name_str) XFree (name_str);
      UNBLOCK_INPUT;
      result = Fcons (Fcons (name, value), result);
    }
  return result;
}
#endif

Lisp_Object Qpixmapp;
static Lisp_Object mark_pixmap (Lisp_Object, void (*) (Lisp_Object));
static void print_pixmap (Lisp_Object, Lisp_Object, int);
static void finalize_pixmap (void *, int);
static int pixmap_equal (Lisp_Object o1, Lisp_Object o2, int depth);
DEFINE_LRECORD_IMPLEMENTATION ("pixmap", lrecord_pixmap,
			       mark_pixmap, print_pixmap, finalize_pixmap,
			       pixmap_equal, sizeof(struct Lisp_Pixmap));

static Lisp_Object
mark_pixmap (Lisp_Object obj, void (*markobj) (Lisp_Object))
{
  struct Lisp_Pixmap *p = XPIXMAP (obj);
  return p->file_name;
}


static void
print_pixmap (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  char buf[100];
  struct Lisp_Pixmap *p = XPIXMAP (obj);
  char *s;
  if (print_readably)
    error (GETTEXT ("printing unreadable object #<pixmap 0x%x>"), (long) p);

  write_string_1 ((p->depth ? "#<pixmap " : "#<bitmap "), -1, printcharfun);
  if (STRINGP (p->file_name) &&
      (s = strrchr ((char *) XSTRING (p->file_name)->data, '/')))
    /* >>> Not GC safe.  printcharfun may relocate p->file_name->data */
    write_string_1 (s+1, -1, printcharfun);
  else
    print_internal (p->file_name, printcharfun, 0);
  if (p->depth > 1)
    sprintf (buf, " %dx%dx%d", p->width, p->height, p->depth);
  else
    sprintf (buf, " %dx%d", p->width, p->height);
  write_string_1 (buf, -1, printcharfun);
  if (p->x || p->y)
    {
      sprintf (buf, " @%d,%d", p->x, p->y);
      write_string_1 (buf, -1, printcharfun);
    }
  sprintf (buf, " (0x%lx", (unsigned long) p->pixmap);
  write_string_1 (buf, -1, printcharfun);
  if (p->mask)
    {
      sprintf (buf, "/0x%lx", (unsigned long) p->mask);
      write_string_1 (buf, -1, printcharfun);
    }
  sprintf (buf, ") 0x%x>", (long) p);
  write_string_1 (buf, -1, printcharfun);
}

static void
finalize_pixmap (void *header, int for_disksave)
{
  struct Lisp_Pixmap *p = (struct Lisp_Pixmap *) header;
  if (for_disksave) finalose (p);
  BLOCK_INPUT;
  XFreePixmap (DisplayOfScreen (p->screen), p->pixmap);
  if (p->mask && p->mask != p->pixmap)
    XFreePixmap (DisplayOfScreen (p->screen), p->mask);
  if (p->npixels != 0)
    XFreeColors (DisplayOfScreen (p->screen),
		 DefaultColormapOfScreen (p->screen),
		 p->pixels, p->npixels, 0);
  if (p->pixels)
    xfree (p->pixels);
  UNBLOCK_INPUT;
}

/* Pixmaps are equal if their names are non-nil and equal.
   This means that two pixmaps constructed from the same lisp data won't
   be equal, but that's life.  (It's better than all lisp-data pixmaps being
   equal, and it's also better than keeping that lisp data around for the
   lifetime of the pixmap.)
 */
static int
pixmap_equal (Lisp_Object o1, Lisp_Object o2, int depth)
{
  if (NILP (XPIXMAP (o1)->file_name))
    return 0;
  /* In case one has been colorized and the other hasn't. */
  if (XPIXMAP (o1)->npixels != XPIXMAP (o2)->npixels)
    return 0;
  return (internal_equal (XPIXMAP (o1)->file_name, XPIXMAP (o2)->file_name,
			  depth + 1));
}


static Lisp_Object
make_pixmap_1 (Lisp_Object name, Screen *xs,
	       int x, int y, Dimension w, Dimension h, unsigned int d,
	       Pixmap pixmap, Pixmap mask,
	       Pixel *pixels, int npixels)
{
  struct Lisp_Pixmap *lp = alloc_lcrecord (sizeof (struct Lisp_Pixmap),
					   lrecord_pixmap);
  Lisp_Object val;
  if (x == -1) x = 0;
  if (y == -1) y = 0;
  lp->file_name = name;
  lp->screen = xs;
  lp->pixmap = pixmap;
  lp->mask = mask;
  lp->x = x;
  lp->y = y;
  lp->width = w;
  lp->height = h;
  lp->depth = d;
  lp->pixels = pixels;
  lp->npixels = npixels;
  lp->contrib_p = 1;
  XSETR (val, Lisp_Pixmap, lp);
  return val;
}


/* Where bitmaps are; initialized from resource database */
Lisp_Object Vx_bitmap_file_path;

#ifndef BITMAPDIR
#define BITMAPDIR "/usr/include/X11/bitmaps"
#endif

#define USE_XBMLANGPATH

static Lisp_Object
locate_pixmap_file (Lisp_Object name)
{
  /* Check non-absolute pathnames with a directory component relative to
     the search path; that's the way Xt does it. */
  /* >>> Unix-specific */
  if (XSTRING (name)->data[0] == '/' ||
      (XSTRING (name)->data[0] == '.' &&
       (XSTRING (name)->data[1] == '/' ||
	(XSTRING (name)->data[1] == '.' &&
	 (XSTRING (name)->data[2] == '/')))))
    {
      if (!NILP (Ffile_readable_p (name)))
	return name;
      else
	return Qnil;
    }

#ifdef USE_XBMLANGPATH
  {
    char *path = egetenv ("XBMLANGPATH");
    SubstitutionRec subs[1];
    subs[0].match = 'B';
    subs[0].substitution = (char *) XSTRING (name)->data;
    /* #### Motif uses a big hairy default if $XBMLANGPATH isn't set.
       We don't.  If you want it used, set it. */
    if (path &&
	(path = XtResolvePathname (x_current_display, "bitmaps", 0, 0, path,
				   subs, XtNumber (subs), 0)))
      {
	name = build_string (path);
	XtFree (path);
        return (name);
      }
  }
#endif

  if (NILP (Vx_bitmap_file_path))
    {
      char *type = 0;
      XrmValue value;
      if (XrmGetResource (XtDatabase (x_current_display),
			  "bitmapFilePath", "BitmapFilePath", &type, &value)
	  && !strcmp (type, "String"))
	Vx_bitmap_file_path = decode_env_path (0, (char *) value.addr);
      Vx_bitmap_file_path = nconc2 (Vx_bitmap_file_path,
				    (list1 (build_string (BITMAPDIR))));
    }

  locate_file (Vx_bitmap_file_path, name, "", &name, R_OK);

  return (name);
}

#ifdef HAVE_XPM
 /* xpm 3.2g and better has XpmCreatePixmapFromBuffer()...
    There was no version number in xpm.h before 3.3, but this should do.
  */
# if (XpmVersion >= 3) || defined(XpmExactColors)
#  define XPM_DOES_BUFFERS
# endif

Lisp_Object Vxpm_color_symbols;

static XpmColorSymbol *
extract_xpm_color_names (XpmAttributes *xpmattrs, Lisp_Object screen)
{
  Screen *xs = LISP_SCREEN_TO_X_SCREEN (screen);
  Display *dpy = DisplayOfScreen (xs);
  Colormap cmap = DefaultColormapOfScreen (xs);
  XColor color;
  Lisp_Object rest;
  Lisp_Object results = Qnil;
  int i;
  XpmColorSymbol *symbols;
  struct gcpro gcpro1, gcpro2;

  if (NILP (Vxpm_color_symbols))
    return 0;
  CHECK_CONS (Vxpm_color_symbols, 0);

  GCPRO2 (results, screen);

  /* We built up results to be (("name" . #<pixel>) ...) so that if an
     error happens we don't lose any malloced data, or more importantly,
     leave any pixels allocated in the server. */
  i = 0;
  for (rest = Vxpm_color_symbols; !NILP (rest); rest = Fcdr (rest))
    {
      Lisp_Object name, value;
      Lisp_Object cons = XCONS (rest)->car;
      CHECK_CONS (cons, 0);
      name = XCONS (cons)->car;
      CHECK_STRING (name, 0);
      value = XCONS (cons)->cdr;
      CHECK_CONS (value, 0);
      value = XCONS (value)->car;
      value = Feval (value);
      if (NILP (value))
	continue;
      if (STRINGP (value))
	value = Fmake_pixel (value, screen);
      CHECK_PIXEL (value, 0);
      results = Fcons (Fcons (name, value), results);
      i++;
    }
  UNGCPRO;			/* no more evaluation */

  if (i == 0) return 0;

  symbols = (XpmColorSymbol *) xmalloc (i * sizeof (XpmColorSymbol));
  xpmattrs->valuemask |= XpmColorSymbols;
  xpmattrs->colorsymbols = symbols;
  xpmattrs->numsymbols = i;

  while (--i >= 0)
    {
      Lisp_Object cons = XCONS (results)->car;
      color = XPIXEL (XCONS (cons)->cdr)->color;
      /* Duplicate the pixel value so that we still have a lock on it if
	 the pixel we were passed is later freed. */
      BLOCK_INPUT;
      if (! XAllocColor (dpy, cmap, &color))
	abort ();  /* it must be allocable since we're just duplicating it */
      UNBLOCK_INPUT;

      symbols [i].name = (char *) XSTRING (XCONS (cons)->car)->data;
      symbols [i].pixel = color.pixel;
      symbols [i].value = 0;
      results = XCONS (results)->cdr;
      free_cons (XCONS (cons));
    }
  return symbols;
}

static void
xpm_free (XpmAttributes *xpmattrs)
{
  BLOCK_INPUT;
  /* Could concievably lose if XpmXXX returned an error without first
     initializing this structure, if we didn't know that initializing it
     to all zeros was ok (and also that it's ok to call XpmFreeAttributes()
     multiple times, since it zeros slots as it frees them...) */
  XpmFreeAttributes (xpmattrs);
  UNBLOCK_INPUT;
}

static void
xpm_error (Lisp_Object type, Lisp_Object data, XpmAttributes *xpmattrs)
{
  xpm_free (xpmattrs);
  signal_error (type, data);
}

static Lisp_Object
try_reading_xpm_bitmap (Lisp_Object screen, Lisp_Object name,
			int raw_data_p, int force_mono_p)
{
  int result;
  int retry_in_mono = force_mono_p;
  Screen *xs = LISP_SCREEN_TO_X_SCREEN (screen);

  Pixmap pixmap;
  Pixmap mask = 0;
  XpmAttributes xpmattrs;

  {
    XpmColorSymbol *color_symbols;

  retry:

    memset (&xpmattrs, 0, sizeof (xpmattrs)); /* want XpmInitAttributes() */
    xpmattrs.valuemask = XpmReturnPixels;
    if (retry_in_mono)
      {
#ifdef XpmColorKey	/* 3.2g or better */
	/* Without this, we get a 1-bit version of the color image, which
	   isn't quite right.  With this, we get the mono image, which might
	   be very different looking. */
	xpmattrs.valuemask |= XpmColorKey;
	xpmattrs.color_key = XPM_MONO;
#endif
	xpmattrs.depth = 1;
	xpmattrs.valuemask |= XpmDepth;
      }

    color_symbols = extract_xpm_color_names (&xpmattrs, screen);

    BLOCK_INPUT;
# ifdef XPM_DOES_BUFFERS
    if (raw_data_p)
      result = XpmCreatePixmapFromBuffer (DisplayOfScreen (xs),
					  RootWindowOfScreen (xs),
					  (char *) XSTRING (name)->data,
					  &pixmap, &mask, &xpmattrs);
    else
# endif/* XPM_DOES_BUFFERS */
      result = XpmReadFileToPixmap (DisplayOfScreen (xs),
				    RootWindowOfScreen (xs), 
                                    (char *) XSTRING (name)->data,
				    &pixmap, &mask, &xpmattrs);

    if (color_symbols)
      {
	xfree (color_symbols);
	xpmattrs.colorsymbols = 0; /* in case XpmFreeAttr is too smart... */
	xpmattrs.numsymbols = 0;
      }
    UNBLOCK_INPUT;
  }

  switch (result)
    {
    case XpmSuccess:
      break;
    case XpmFileInvalid:
      {
	if (raw_data_p)
	  xpm_error (Qerror, list2 (build_string (GETTEXT ("invalid XPM data")), name),
		     &xpmattrs);
	xpm_free (&xpmattrs);
	return (Qnil);
      }
    case XpmColorFailed:
      {
	if (retry_in_mono)
	  /* second time; blow out. */
	  xpm_error (Qerror, list3 (build_string (GETTEXT ("Reading pixmap file")),
				    build_string (GETTEXT ("color allocation failed")),
				    name),
		     &xpmattrs);
	else
	  retry_in_mono = 1;

	xpm_free (&xpmattrs);
	goto retry;
      }
    case XpmColorError:
      {
	/* Maybe we should just read it in monochrome instead of allowing the
	   colors to be substituted?
	   */
	if (raw_data_p)
	  message (GETTEXT ("color substitution performed for XPM data"));
	else
	  message (GETTEXT ("color substitution performed for file \"%s\""), 
		   XSTRING (name)->data);
	break;
      }
    case XpmNoMemory:
      {
	xpm_error (Qfile_error, list3 (build_string (raw_data_p
						     ? GETTEXT ("Parsing pixmap data")
						     : GETTEXT ("Reading pixmap file")),
				       build_string (GETTEXT ("out of memory")),
				       name),
		   &xpmattrs);
      }
    case XpmOpenFailed:
      {
	xpm_error (Qfile_error,
		   list3 (build_string (GETTEXT ("Opening pixmap file")),
			  build_string (GETTEXT ("no such file or directory")),
			  name),
		   &xpmattrs);
      }
    default:
      {
	xpm_error (Qfile_error, list4 (build_string (raw_data_p
						     ? GETTEXT ("Parsing pixmap data")
						     : GETTEXT ("Reading pixmap file")),
				       build_string (GETTEXT ("unknown error code")),
				       make_number (result), name),
		   &xpmattrs);
      }
    }
  {
    /* XpmReadFileToPixmap() doesn't return the depth (bogus!) so we need to
       get it ourself.  (No, xpmattrs.depth is not it; that's an input slot,
       not output.)  We could just assume that it has the same depth as the
       root window, but some screens allow more than one depth, so that isn't
       necessarily correct (I guess?) */
    Window root;
    int x, y;
    unsigned int w2, h2, bw;

    unsigned int w = xpmattrs.width;
    unsigned int h = xpmattrs.height;
    unsigned int d;
    int xhot = ((xpmattrs.valuemask & XpmHotspot) ? xpmattrs.x_hotspot : -1);
    int yhot = ((xpmattrs.valuemask & XpmHotspot) ? xpmattrs.y_hotspot : -1);
    int npixels = xpmattrs.npixels;
    Pixel *pixels = 0;

    if (npixels != 0)
      {
	pixels = xmalloc (npixels * sizeof (Pixel));
	memcpy (pixels, xpmattrs.pixels, npixels * sizeof (Pixel));
      }
    else
      pixels = 0;

    xpm_free (&xpmattrs);	/* after we've read pixels and hotspot */

    BLOCK_INPUT;
    if (!XGetGeometry (DisplayOfScreen (xs), pixmap, &root, &x, &y,
                       &w2, &h2, &bw, &d))
      abort ();
    if (w != w2 || h != h2)
      abort ();
    UNBLOCK_INPUT;

    return make_pixmap_1 (((raw_data_p) ? Qnil : name), xs, xhot, yhot, w, h,
			  d, pixmap, mask, pixels, npixels);
  }
}
#endif /* HAVE_XPM */

static Lisp_Object
try_reading_Xmu_bitmap (Screen *xs, Lisp_Object name)
{
  Pixmap pixmap;
  unsigned int w, h;
  int xhot, yhot;
  unsigned char *data;
  int result;

  BLOCK_INPUT;
  result = XmuReadBitmapDataFromFile ((char *) XSTRING (name)->data,
				      &w, &h, &data, &xhot, &yhot);
  UNBLOCK_INPUT;

  switch (result)
    {
    case BitmapSuccess:
      {
	BLOCK_INPUT;
	pixmap = XCreatePixmapFromBitmapData (DisplayOfScreen (xs),
					      RootWindowOfScreen (xs),
					      (char *) data, w, h, 1, 0, 1);
	XFree ((char *)data);
	UNBLOCK_INPUT;
        return make_pixmap_1 (name, xs, xhot, yhot, w, h, 0, pixmap, 0, 0, 0);
      }
    case BitmapOpenFailed:
      {
	signal_error (Qfile_error,
		      list3 (build_string (GETTEXT ("Opening bitmap file")),
			     build_string (GETTEXT ("no such file or directory")),
			     name));
      }
    case BitmapFileInvalid:
      {
        return Qnil;
      }
    case BitmapNoMemory:
      {
	signal_error (Qfile_error,
		      list3 (build_string (GETTEXT ("Reading bitmap file")),
			     build_string (GETTEXT ("out of memory")),
			     name));
      }
    default:
      {
	signal_error (Qfile_error,
		      list4 (build_string (GETTEXT ("Reading bitmap file")),
			     build_string (GETTEXT ("unknown error code")),
			     make_number (result), name));
      }
    }
}


static Lisp_Object
make_pixmap_from_file (Lisp_Object name, Lisp_Object screen,
		       int force_mono_p)
{
  Screen *xs = LISP_SCREEN_TO_X_SCREEN (screen);
  int raw_data_p = 0;
  Lisp_Object result;

  CHECK_STRING (name, 0);

  if (XSTRING (name)->size > 9 &&
      !strncmp ("/* XPM */", (char *) XSTRING (name)->data, 9))
    {
#ifdef XPM_DOES_BUFFERS
      raw_data_p = 1;
#else
# ifdef HAVE_XPM
      signal_error (Qerror,
		    list2 (build_string
			   (GETTEXT ("XPM library is too old: no raw data")),
			   name));
# else /* !XPM */
      signal_error (Qerror, list2 (build_string
				   (GETTEXT ("no support for XPM data")),
				   name));
# endif /* !XPM */
#endif /* !XPM_DOES_BUFFERS */
    }
  else
    {
      Lisp_Object file = locate_pixmap_file (name);
      if (NILP (file))
        signal_error (Qfile_error,
		      list3 (build_string (GETTEXT ("Opening pixmap file")),
			     build_string (GETTEXT ("no such file or directory")),
			     name));
      name = file;
    }

#ifdef HAVE_XPM
  result = try_reading_xpm_bitmap (screen, name, raw_data_p, force_mono_p);
  if (!NILP (result))
    return (result);
#endif /* HAVE_XPM */

  result = try_reading_Xmu_bitmap (xs, name);
  if (!NILP (result))
    return (result);

  signal_error (Qfile_error,
                list3 (build_string (GETTEXT ("Reading pixmap file")),
#ifdef HAVE_XPM
                       build_string (GETTEXT ("invalid pixmap or bitmap data")),
#else
                       build_string (GETTEXT ("invalid bitmap data")),
#endif
                       name));
}


Lisp_Object
make_pixmap_from_data (Screen *screen, char *bits, int width, int height)
{
  Pixmap pixmap = XCreatePixmapFromBitmapData (DisplayOfScreen (screen),
					       RootWindowOfScreen (screen),
					       bits, width, height, 1, 0, 1);
  return make_pixmap_1 (Qnil, screen, 0, 0, width, height, 0, pixmap, 0, 0, 0);
}


static Lisp_Object
make_pixmap_from_lisp_data (Lisp_Object data, Lisp_Object screen)
{
  Screen *xs = LISP_SCREEN_TO_X_SCREEN (screen);
  Dimension w, h;
  char *bits;

  CHECK_CONS (data, 0);
  
  if (!CONSP (Fcdr (data)) ||
      !CONSP (Fcdr (Fcdr (data))) ||
      !NILP (Fcdr (Fcdr (Fcdr (data)))) ||
      !FIXNUMP (Fcar (data)) ||
      !FIXNUMP (Fcar (Fcdr (data))) ||
      !STRINGP (Fcar (Fcdr (Fcdr (data)))))
    signal_error (Qerror,
		  list2 (build_string (GETTEXT ("must be of the form (W H \"bits\")")),
			 data));
  w = XINT (Fcar (data));
  h = XINT (Fcar (Fcdr (data)));
  if (w <= 0)
    while (1) wrong_type_argument (Qnatnump, Fcar (data));
  if (h <= 0)
    while (1) wrong_type_argument (Qnatnump, Fcar (Fcdr (data)));
  if (((unsigned) (w * h) / 8)
      > string_length (XSTRING (Fcar (Fcdr (Fcdr (data))))))
    signal_error (Qerror,
		  list2 (build_string (GETTEXT ("data is too short for W and H")),
			 data));

  bits = (char *) XSTRING (Fcar (Fcdr (Fcdr (data))))->data;
  return make_pixmap_from_data (xs, bits, w, h);
}


Lisp_Object
make_pixmap (Lisp_Object name, Lisp_Object screen, int force_mono_p)
{
  if (CONSP (name))
    return make_pixmap_from_lisp_data (name, screen);
  else
    return make_pixmap_from_file (name, screen, force_mono_p);
}

DEFUN ("make-pixmap", Fmake_pixmap, Smake_pixmap, 1, 2, 0,
       "Loads a new `pixmap' object from the specified file.\n\
The file should be in `XBM' or `XPM' format.\n\
If the XBMLANGPATH environment variable is set, it will be searched for\n\
 matching files.  Next, the directories listed in the `x-bitmap-file-path'\n\
 variable will be searched (this variable is initialized from the\n\
 \"*bitmapFilePath\" resource.)\n\
The file argument may also be a list of the form (width height data) where\n\
 width and height are the size in pixels, and data is a string, containing\n\
 the raw bits of the bitmap.  (Bitmaps specified this way can only be one bit\n\
 deep.)\n\
If compiled with support for XPM, the file argument may also be a string\n\
 which is the contents of an XPM file (that is, a string beginning with the\n\
 characters \"/* XPM */\"; see the XPM documentation.)\n\
The optional second argument is the screen on which to allocate the pixmap\n\
 (in case some screens are running on different X servers.)\n\
This allocates a new Pixmap in the X server, and signals an error if the\n\
 file can't be found, or the Pixmap cannot be allocated.\n\
\n\
The returned object is a normal, first-class lisp object.  The way you\n\
`deallocate' the pixmap is the way you deallocate any other lisp object:\n\
you drop all pointers to it and allow it to be garbage collected.  When\n\
these objects are GCed, the underlying X data is deallocated as well.")
  (name, screen)
  Lisp_Object name, screen;
{
  return make_pixmap (name, screen, 0);
}

DEFUN ("pixmapp", Fpixmapp, Spixmapp, 1, 1, 0,
       "Whether the given object is a pixmap.")
  (obj)
  Lisp_Object obj;
{
  return (PIXMAPP (obj) ? Qt : Qnil);
}

/* I wonder if this is really the right thing.
   Perhaps pixmaps should just always contribute;
   or perhaps it should be a buffer-local property;
   or perhaps pixmaps should come with a "clip region".
 */
DEFUN ("pixmap-contributes-to-line-height-p",
       Fpixmap_contributes_to_line_height_p,
       Spixmap_contributes_to_line_height_p, 1, 1, 0,
  "Whether the given PIXMAP contributes to the height of its line,\n\
when used as an extent glyph.  If a pixmap contributes to the line height,\n\
then if the pixmap is taller than the tallest font displayed on the line,\n\
the line will be the height of the pixmap, and the full pixmap will be\n\
visible.  If the pixmap does not contribute, and the line ends up being\n\
smaller than the pixmap, then the pixmap will be truncated.")
  (pixmap)
  Lisp_Object pixmap;
{
  CHECK_PIXMAP (pixmap, 0);
  return (XPIXMAP (pixmap)->contrib_p ? Qt : Qnil);
}

DEFUN ("set-pixmap-contributes-to-line-height",
       Fset_pixmap_contributes_to_line_height,
       Sset_pixmap_contributes_to_line_height, 2, 2, 0,
  "Change whether PIXMAP contributes to the height of its line\n\
when used as an extent glyph.  If a pixmap contributes to the line height,\n\
then if the pixmap is taller than the tallest font displayed on the line,\n\
the line will be the height of the pixmap, and the full pixmap will be\n\
visible.  If the pixmap does not contribute, and the line ends up being\n\
smaller than the pixmap, then the pixmap will be truncated.\n\
Otherwise the pixmap will be clipped to the height of the line.")
  (pixmap, val)
  Lisp_Object pixmap, val;
{
  CHECK_PIXMAP (pixmap, 0);
  XPIXMAP (pixmap)->contrib_p = !NILP (val);
  return (NILP (val) ? Qnil : Qt);
}
	 
DEFUN ("set-pixmap-hotspot", Fset_pixmap_hotspot, Sset_pixmap_hotspot,
       3, 3, 0,
       "Set the pixmap's hotspot.\n\
This is a point relative to the origin of the pixmap.  When a pixmap is\n\
used as a cursor or similar pointing indicator, the hotspot is the point\n\
on the pixmap that sits over the location that the pointer points to.\n\
This is, for example, the tip of the arrow or the center of the crosshairs.")
     (pixmap, x, y)
     Lisp_Object pixmap, x, y;
{
  struct Lisp_Pixmap *p;

  CHECK_PIXMAP (pixmap, 0);
  CHECK_FIXNUM (x, 0);
  CHECK_FIXNUM (y, 0);
  p = XPIXMAP (pixmap);
  p->x = XINT (x);
  p->y = XINT (y);
  return Qnil;
}

DEFUN ("pixmap-hotspot-x", Fpixmap_hotspot_x, Spixmap_hotspot_x, 1, 1, 0,
       "Returns the X coordinate of the pixmap's hotspot.\n\
See `set-pixmap-hotspot'.")
     (pixmap)
     Lisp_Object pixmap;
{
  CHECK_PIXMAP (pixmap, 0);
  return make_number (XPIXMAP (pixmap)->x);
}

DEFUN ("pixmap-hotspot-y", Fpixmap_hotspot_y, Spixmap_hotspot_y, 1, 1, 0,
       "Returns the Y coordinate of the pixmap's hotspot.\n\
See `set-pixmap-hotspot'.")
     (pixmap)
     Lisp_Object pixmap;
{
  CHECK_PIXMAP (pixmap, 0);
  return make_number (XPIXMAP (pixmap)->y);
}

DEFUN ("pixmap-depth", Fpixmap_depth, Spixmap_depth, 1, 1, 0,
       "Return the depth of the pixmap.\n\
This is 0 for a bitmap, or a positive integer for a pixmap.")
     (pixmap)
     Lisp_Object pixmap;
{
  CHECK_PIXMAP (pixmap, 0);
  return (make_number (XPIXMAP (pixmap)->depth));
}

DEFUN ("pixmap-width", Fpixmap_width, Spixmap_width, 1, 1, 0,
       "Return the width of the pixmap, in pixels.")
     (pixmap)
     Lisp_Object pixmap;
{
  CHECK_PIXMAP (pixmap, 0);
  return (make_number (XPIXMAP (pixmap)->width));
}

DEFUN ("pixmap-height", Fpixmap_height, Spixmap_height, 1, 1, 0,
       "Return the height of the pixmap, in pixels.")
     (pixmap)
     Lisp_Object pixmap;
{
  CHECK_PIXMAP (pixmap, 0);
  return (make_number (XPIXMAP (pixmap)->height));
}

DEFUN ("set-pixmap-data", Fset_pixmap_data, Sset_pixmap_data, 2, 2, 0,
       "Set the pixmap's data.  The data must be in the form of a list;\n\
currently only the format (width height data) is supported.  See\n\
`make-pixmap'.")
     (pixmap, data)
     Lisp_Object pixmap, data;
{
  CHECK_PIXMAP (pixmap, 0);
  error ("Not yet supported");
  return Qnil;
}
 
DEFUN ("set-pixmap-mask", Fset_pixmap_mask, Sset_pixmap_mask, 2, 2, 0,
       "Set the pixmap's mask according to the specified data.\n\
The data must be in XY format and must be at least (width * height)\n\
bits long, one bit per pixel in the pixmap.  When the pixmap is used\n\
as a cursor or similar pointing indicator, the mask indicates which\n\
pixels in the pixmap's rectangular outline are to be taken from the\n\
pixmap and which from the background.")
     (pixmap, data)
     Lisp_Object pixmap, data;
{
  CHECK_PIXMAP (pixmap, 0);
  error ("Not yet supported");
  return Qnil;
}

DEFUN ("pixmap-file-name", Fpixmap_file_name, Spixmap_file_name, 1, 1, 0,
       "Returns the file name from which the given pixmap was read, or nil\n\
if the pixmap was created from Lisp data (the lisp data is not retained,\n\
since it usually won't be needed again might be quite large.)")
  (pixmap)
  Lisp_Object pixmap;
{
  CHECK_PIXMAP (pixmap, 0);
  return (XPIXMAP (pixmap)->file_name);
}

DEFUN ("colorize-pixmap", Fcolorize_pixmap, Scolorize_pixmap, 3, 3, 0,
       "Make the pixmap be displayed in the given colors.\n\
Pixmaps come in two varieties: bitmaps, which are 1 bit deep which are\n\
rendered in the prevailing foreground and background colors; and pixmaps,\n\
which are of arbitrary depth (including 1) and which have the colors\n\
explicitly specified.  This function converts a bitmap to a pixmap.\n\
If the pixmap was a pixmap already, nothing is done (and nil is returned.)\n\
Otherwise t is returned.")
  (pixmap, foreground, background)
  Lisp_Object pixmap, foreground, background;
{
  struct Lisp_Pixmap *p;
  CHECK_PIXMAP (pixmap, 0);
  CHECK_PIXEL (foreground, 0);
  CHECK_PIXEL (background, 0);
  p = XPIXMAP (pixmap);
  if (p->depth > 0) return Qnil;
  BLOCK_INPUT;
  {
    Display *dpy = DisplayOfScreen (p->screen);
    Dimension d = DefaultDepthOfScreen (p->screen);
    Colormap cmap = DefaultColormapOfScreen (p->screen);
    Pixmap new = XCreatePixmap (dpy, RootWindowOfScreen (p->screen),
				p->width, p->height, d);
    XColor color;
    XGCValues gcv;
    GC gc;
    /* Duplicate the pixel values so that we still have a lock on them if
       the pixels we were passed are later freed. */
    color = XPIXEL (foreground)->color;
    if (! XAllocColor (dpy, cmap, &color)) abort ();
    gcv.foreground = color.pixel;
    color = XPIXEL (background)->color;
    if (! XAllocColor (dpy, cmap, &color)) abort ();
    gcv.background = color.pixel;
    gc = XCreateGC (dpy, new, GCBackground|GCForeground, &gcv);
    XCopyPlane (dpy, p->pixmap, new, gc, 0, 0, p->width, p->height, 0, 0, 1);
    XFreeGC (dpy, gc);
    XFreePixmap (dpy, p->pixmap);
    p->pixmap = new;
    p->depth = d;
  }
  UNBLOCK_INPUT;
  return Qt;
}

DEFUN ("force-x-window-glyph-map", Fforce_x_window_glyph_map,
       Sforce_x_window_glyph_map, 1, 1, 0,
  "Generate a Map event for X-WINDOW-GLYPH.")
     (subwindow)
     Lisp_Object subwindow;
{
  CHECK_SUBWINDOW (subwindow, 0);

  XMapWindow (DisplayOfScreen (XSUBWINDOW (subwindow)->xscreen),
	      XSUBWINDOW (subwindow)->subwindow);

  return subwindow;
}


Lisp_Object Qx_window_glyph_p;
static Lisp_Object mark_subwindow (Lisp_Object, void (*) (Lisp_Object));
static void print_subwindow (Lisp_Object, Lisp_Object, int);
static void finalize_subwindow (void *, int);
static int subwindow_equal (Lisp_Object o1, Lisp_Object o2, int depth);
DEFINE_LRECORD_IMPLEMENTATION ("subwindow", lrecord_subwindow,
			       mark_subwindow, print_subwindow,
			       finalize_subwindow, subwindow_equal,
			       sizeof (struct Lisp_Subwindow));

static Lisp_Object
mark_subwindow (Lisp_Object obj, void (*markobj) (Lisp_Object))
{
  struct Lisp_Subwindow *sw = XSUBWINDOW (obj);
  return sw->screen;
}

static void
print_subwindow (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  char buf[100];
  struct Lisp_Subwindow *sw = XSUBWINDOW (obj);
  struct screen *scr = XSCREEN (sw->screen);

  if (print_readably)
    error (GETTEXT ("printing unreadable object #<x-window-glyph 0x%x>"),
	   (long) sw);

  write_string_1 ("#<x-window-glyph", -1, printcharfun);
  sprintf (buf, " %dx%d", sw->width, sw->height);
  write_string_1 (buf, -1, printcharfun);

  /* This is stolen from screen.c.  Subwindows are strange in that they
     are specific to a particular screen so we want to print in their
     description what that screen is. */
  
  write_string_1 (" on #<", -1, printcharfun);
  if (!SCREEN_LIVE_P (scr))
    write_string_1 ("dead", -1, printcharfun);
  else if (SCREEN_IS_TERMCAP (scr))
    write_string_1 ("termcap", -1, printcharfun);
  else if (SCREEN_IS_X (scr))
    write_string_1 ("x", -1, printcharfun);
  else
    write_string_1 ("UNKNOWN", -1, printcharfun);
  write_string_1 ("-screen ", -1, printcharfun);
  print_internal (scr->name, printcharfun, 1);
  sprintf (buf, " 0x%x>", (long) scr);
  write_string_1 (buf, -1, printcharfun);

  sprintf (buf, ") 0x%x>", (long) sw);
  write_string_1 (buf, -1, printcharfun);
}

static void
finalize_subwindow (void *header, int for_disksave)
{
  struct Lisp_Subwindow *sw = (struct Lisp_Subwindow *) header;
  if (for_disksave) finalose (sw);
  BLOCK_INPUT;
  XDestroyWindow (DisplayOfScreen (sw->xscreen), sw->subwindow);
  UNBLOCK_INPUT;
}

/* subwindows are equal iff they have the same window XID */
static int
subwindow_equal (Lisp_Object o1, Lisp_Object o2, int depth)
{
  return (XSUBWINDOW (o1)->subwindow == XSUBWINDOW (o2)->subwindow);
}

/* #### PROBLEM: The display routines assume that the glyph is only
 being displayed in one buffer.  If it is in two different buffers
 which are both being displayed simultaneously you will lose big time.
 Fixing this problem requires the addition of even more cruft which I
 may or may not do in the current redisplay. */

DEFUN ("make-x-window-glyph", Fmake_x_window_glyph, Smake_x_window_glyph,
       0, 3, 0,
       "Creates a new `x-window' object of size WIDTH x HEIGHT.\n\
The default is a window of size 1x1, which is also the minimum allowed\n\
window size.  Subwindows are per-screen.  A buffer being shown in two\n\
different screens will only display a subwindow glyph in the screen in\n\
which it was actually created.  If two windows on the same screen are\n\
displaying the buffer then the most recently used window will actually\n\
display the window.  If the screen is not specified, the selected screen\n\
is used.")
  (width, height, screen)
  Lisp_Object width, height, screen;
{
  Display *dpy;
  Screen *xs;
  Window pw;
  struct screen *s;
  unsigned int iw, ih;
  XSetWindowAttributes xswa;
  Mask valueMask = 0;

  if (NILP (screen)) screen = Fselected_screen();

  xs = LISP_SCREEN_TO_X_SCREEN (screen);
  s = XSCREEN (screen);
  dpy = DisplayOfScreen (xs);
  pw = XtWindow (s->display.x->edit_widget);

  if (NILP (width))
    iw = 1;
  else
    {
      CHECK_FIXNUM (width, 0);
      iw = XINT (width);
      if (iw < 1) iw = 1;
    }
  if (NILP (height))
    ih = 1;
  else
    {
      CHECK_FIXNUM (height, 0);
      ih = XINT (height);
      if (ih < 1) ih = 1;
    }

  {
    struct Lisp_Subwindow *sw = alloc_lcrecord (sizeof (struct Lisp_Subwindow),
						lrecord_subwindow);
    Lisp_Object val;
    sw->screen = screen;
    sw->xscreen = xs;
    sw->parent_window = pw;
    sw->height = ih;
    sw->width = iw;

    xswa.backing_store = Always;
    valueMask |= CWBackingStore;

    xswa.colormap = DefaultColormapOfScreen (xs);
    valueMask |= CWColormap;

    sw->subwindow = XCreateWindow (dpy, pw, 0, 0, iw, ih, 0, CopyFromParent,
				   InputOutput, CopyFromParent, valueMask,
				   &xswa);

    XSETR (val, Lisp_Subwindow, sw);
    return val;
  }
}

/* This function is temporary at the moment and is likely to change or
   go away before the next release. */
DEFUN ("change-x-window-glyph-property", Fchange_x_window_glyph_property,
       Schange_x_window_glyph_property, 3, 3, 0,
       "For the given X-WINDOW-GLYPH, set PROPERTY to DATA, which is a string.")
  (subwindow, property, data)
  Lisp_Object subwindow, property, data;
{
  Atom property_atom;
  struct Lisp_Subwindow *sw;
  Display *dpy;

  CHECK_SUBWINDOW (subwindow, 0);
  CHECK_STRING (property, 0);
  CHECK_STRING (data, 0);

  sw = XSUBWINDOW (subwindow);
  dpy = DisplayOfScreen (LISP_SCREEN_TO_X_SCREEN (sw->screen));

  property_atom = XInternAtom (dpy, (char *) XSTRING (property)->data, False);
  XChangeProperty (dpy, sw->subwindow, property_atom, XA_STRING, 8,
		   PropModeReplace, XSTRING (data)->data,
		   XSTRING (data)->size);

  return (property);
}

DEFUN ("x-window-glyph-p", Fx_window_glyph_p, Sx_window_glyph_p, 1, 1, 0,
       "Whether the given object is an x-window-glyph.")
  (obj)
  Lisp_Object obj;
{
  return (SUBWINDOWP (obj) ? Qt : Qnil);
}

DEFUN ("x-window-glyph-width", Fx_window_glyph_width, Sx_window_glyph_width,
       1, 1, 0,
       "Width of X-WINDOW-GLYPH.")
  (subwindow)
  Lisp_Object subwindow;
{
  CHECK_SUBWINDOW (subwindow, 0);
  return (make_number (XSUBWINDOW (subwindow)->width));
}

DEFUN ("x-window-glyph-height", Fx_window_glyph_height, Sx_window_glyph_height,
       1, 1, 0,
       "Height of X-WINDOW-GLYPH.")
  (subwindow)
  Lisp_Object subwindow;
{
  CHECK_SUBWINDOW (subwindow, 0);
  return (make_number (XSUBWINDOW (subwindow)->height));
}

DEFUN ("x-window-glyph-xid", Fx_window_glyph_xid, Sx_window_glyph_xid, 1, 1, 0,
       "Return the xid of X-WINDOW-GLYPH as a number.")
  (subwindow)
  Lisp_Object subwindow;
{
  CHECK_SUBWINDOW (subwindow, 0);
  return (make_number (XSUBWINDOW (subwindow)->subwindow));
}

DEFUN ("resize-x-window-glyph", Fresize_x_window_glyph, Sresize_x_window_glyph,
       1, 3, 0,
  "Resize X-WINDOW-GLYPH to WIDTH x HEIGHT.\n\
If a value is nil that parameter is not changed.")
  (subwindow, width, height)
  Lisp_Object subwindow, width, height;
{
  int neww, newh;
  struct Lisp_Subwindow *sw;

  CHECK_SUBWINDOW (subwindow, 0);
  sw = XSUBWINDOW (subwindow);

  if (NILP (width))
    neww = sw->width;
  else
    neww = XINT (width);

  if (NILP (height))
    newh = sw->height;
  else
    newh = XINT (height);

  XResizeWindow (DisplayOfScreen (sw->xscreen), sw->subwindow, neww, newh);

  sw->height = newh;
  sw->width = neww;

  return subwindow;
}

#ifdef EPOCH

Lisp_Object Qx_resource_p;
static Lisp_Object mark_x_resource (Lisp_Object, void (*) (Lisp_Object));
static void print_x_resource (Lisp_Object, Lisp_Object, int);
static void finalize_x_resource (void *, int);
static int x_resource_equal (Lisp_Object o1, Lisp_Object o2, int depth);
DEFINE_LRECORD_IMPLEMENTATION ("x-resource", lrecord_x_resource,
			       mark_x_resource, print_x_resource,
			       finalize_x_resource, x_resource_equal,
			       sizeof (struct Lisp_X_Resource));

static Lisp_Object
mark_x_resource (Lisp_Object obj, void (*markobj) (Lisp_Object))
{
  return Qnil;
}

static void
print_x_resource (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  char buf[100];
  char *default_string = "Resource";
  Lisp_Object atom_symbol;

  atom_symbol = x_atom_to_symbol (x_current_display, XXRESOURCE (obj)->type);
  sprintf (buf, "#<x-resource %s %lx>",
	   (NILP (atom_symbol)
	    ? default_string
	    : (char *) XSTRING (Fsymbol_name (atom_symbol))->data),
	   XXRESOURCE (obj)->xid);
  write_string_1 (buf, -1, printcharfun);
}

static void
finalize_x_resource (void *header, int for_disksave)
{
}

static int
x_resource_equal (Lisp_Object o1, Lisp_Object o2, int depth)
{
  return (XXRESOURCE (o1)->xid == XXRESOURCE (o2)->xid);
}

/*
 * Epoch equivalent:  epoch::resourcep
 */
DEFUN ("x-resource-p", Fx_resource_p, Sx_resource_p, 1, 1, 0,
       "Whether the given object is an X resource object.")
     (obj)
     Lisp_Object obj;
{
  return (XRESOURCEP (obj) ? Qt : Qnil);
}

/*
 * Epoch equivalent:  epoch::set-resource-type
*/
DEFUN ("x-set-x-resource-type", Fx_set_x_resource_type, Sx_set_x_resource_type,
       2, 2, 0,
  "Set the type of RESOURE to TYPE.  The new type must be an atom.")
     (resource, type)
     Lisp_Object resource, type;
{
  CHECK_XRESOURCE (resource, 0);
  CHECK_XRESOURCE (type, 0);

  if (XXRESOURCE (type)->type != XA_ATOM)
    error ("New type must be an atom");

  XXRESOURCE (resource)->type = XXRESOURCE (type)->xid;
  return resource;
}

#endif /* EPOCH */

void
Xatoms_of_xobjs ()
     /* Once we have multiple displays, this will need to be done differently,
	since atoms are per-display.  This info should live on some per-
	connection object.
      */
{
#define ATOM(x) XInternAtom(x_current_display, (x), False)

  BLOCK_INPUT;
  Xatom_FOUNDRY = ATOM ("FOUNDRY");
  Xatom_FAMILY_NAME = ATOM ("FAMILY_NAME");
  Xatom_WEIGHT_NAME = ATOM ("WEIGHT_NAME");
  Xatom_SLANT = ATOM ("SLANT");
  Xatom_SETWIDTH_NAME = ATOM ("SETWIDTH_NAME");
  Xatom_ADD_STYLE_NAME = ATOM ("ADD_STYLE_NAME");
  Xatom_PIXEL_SIZE = ATOM ("PIXEL_SIZE");
  Xatom_POINT_SIZE = ATOM ("POINT_SIZE");
  Xatom_RESOLUTION_X = ATOM ("RESOLUTION_X");
  Xatom_RESOLUTION_Y = ATOM ("RESOLUTION_Y");
  Xatom_SPACING = ATOM ("SPACING");
  Xatom_AVERAGE_WIDTH = ATOM ("AVERAGE_WIDTH");
  Xatom_CHARSET_REGISTRY = ATOM ("CHARSET_REGISTRY");
  Xatom_CHARSET_ENCODING = ATOM ("CHARSET_ENCODING");
  UNBLOCK_INPUT;
}

void
syms_of_xobjs ()
{
  defsymbol (&Qpixelp, "pixelp");
  defsubr (&Smake_pixel);
  defsubr (&Spixelp);
  defsubr (&Spixel_name);

  defsymbol (&Qcursorp, "cursorp");
  defsubr (&Smake_cursor);
  defsubr (&Scursorp);
  defsubr (&Scursor_name);
  defsubr (&Scursor_foreground);
  defsubr (&Scursor_background);

  defsymbol (&Qfontp, "fontp");
  defsubr (&Smake_font);
  defsubr (&Sfontp);
  defsubr (&Sfont_name);
  defsubr (&Sfont_truename);
  defsubr (&Sx_list_fonts);
#ifndef I18N4
  defsubr (&Sx_font_properties);
#endif

  defsymbol (&Qpixmapp, "pixmapp");
  defsubr (&Smake_pixmap);
  defsubr (&Spixmapp);
  defsubr (&Spixmap_contributes_to_line_height_p);
  defsubr (&Sset_pixmap_contributes_to_line_height);
  defsubr (&Sset_pixmap_hotspot);
  defsubr (&Spixmap_hotspot_x);
  defsubr (&Spixmap_hotspot_y);
  defsubr (&Spixmap_depth);
  defsubr (&Spixmap_width);
  defsubr (&Spixmap_height);
  defsubr (&Sset_pixmap_data);
  defsubr (&Sset_pixmap_mask);
  defsubr (&Spixmap_file_name);
  defsubr (&Scolorize_pixmap);

  defsymbol (&Qx_window_glyph_p, "x-window-glyph-p");
  defsubr (&Smake_x_window_glyph);
  defsubr (&Schange_x_window_glyph_property);
  defsubr (&Sx_window_glyph_p);
  defsubr (&Sx_window_glyph_width);
  defsubr (&Sx_window_glyph_height);
  defsubr (&Sx_window_glyph_xid);
  defsubr (&Sresize_x_window_glyph);
  defsubr (&Sforce_x_window_glyph_map);

#ifdef EPOCH
  defsubr (&Sx_resource_p);
  defsubr (&Sx_set_x_resource_type);
#endif /* EPOCH */

  DEFVAR_LISP ("x-bitmap-file-path", &Vx_bitmap_file_path,
       "A list of the directories in which X bitmap files may be found.\n\
If nil, this is initialized from the \"*bitmapFilePath\" resource.\n\
This is used by the `make-pixmap' function (however, note that if the\n\
environment variable XBMLANGPATH is set, it is consulted first.)");
  Vx_bitmap_file_path = Qnil;

#ifdef HAVE_XPM
  DEFVAR_LISP ("xpm-color-symbols", &Vxpm_color_symbols,
       "Definitions of logical color-names used when reading XPM files.\n\
Elements of this list should be of the form (COLOR-NAME FORM-TO-EVALUATE).\n\
The COLOR-NAME should be a string, which is the name of the color to define;\n\
the FORM should evaluate to a `pixel' object, or a string to be passed to\n\
`make-pixel'.  If a loaded XPM file references a symbolic color called\n\
COLOR-NAME, it will display as the computed pixel instead.\n\
\n\
The default value of this variable defines the logical color names\n\
\"foreground\" and \"background\" to be the colors of the `default' face.");
  Vxpm_color_symbols = Qnil; /* initialized in x-faces.el */
#endif
}
