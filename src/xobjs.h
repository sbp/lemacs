/* X-specific Lisp objects: Pixmaps, Pixels, Cursors, and Fonts.
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

#ifndef _EMACS_XOBJS_H_
#define _EMACS_XOBJS_H_

#define XPIXEL(a) ((struct Lisp_Pixel *) XPNTR(a))
#define PIXELP(x) RECORD_TYPEP ((x), lrecord_pixel)
#define CHECK_PIXEL(x, i) \
  do { if (!PIXELP ((x))) x = wrong_type_argument (Qpixelp, (x)); } while (0)
extern const struct lrecord_implementation lrecord_pixel[];
extern Lisp_Object Fmake_pixel (Lisp_Object name, Lisp_Object screen);
extern Lisp_Object Fpixelp (Lisp_Object obj);
extern Lisp_Object Fpixel_name (Lisp_Object obj);
extern Lisp_Object Qpixelp;

struct Lisp_Pixel {
  struct lcrecord_header header;
  Lisp_Object color_name;
  Screen *screen;
  XColor color;
};

#define XCURSOR(a) ((struct Lisp_Cursor *) XPNTR(a))
#define CURSORP(x) RECORD_TYPEP ((x), lrecord_cursor)
#define CHECK_CURSOR(x, i) \
 do { if (!CURSORP ((x))) x = wrong_type_argument (Qcursorp, (x)); } while (0)
extern const struct lrecord_implementation lrecord_cursor[];
extern Lisp_Object Fmake_cursor (Lisp_Object name, Lisp_Object fg,
				 Lisp_Object bg, Lisp_Object screen);
extern Lisp_Object Fcursorp (Lisp_Object obj);
extern Lisp_Object Fcursor_name (Lisp_Object obj);
extern Lisp_Object Fcursor_foreground (Lisp_Object obj);
extern Lisp_Object Fcursor_background (Lisp_Object obj);
extern Lisp_Object Qcursorp;

struct Lisp_Cursor {
  struct lcrecord_header header;
  Lisp_Object name;
  Screen *screen;
  Cursor cursor;
  Lisp_Object fg, bg;	/* both Lisp_Pixels, or both Qnil */
};

#define XFONT(a) ((struct Lisp_Font *) XPNTR(a))
#define FONTP(x) RECORD_TYPEP ((x), lrecord_font)
#define CHECK_FONT(x, i) \
  do { if (!FONTP ((x))) x = wrong_type_argument (Qfontp, (x)); } while (0)
extern const struct lrecord_implementation lrecord_font[];
extern Lisp_Object Fmake_font (Lisp_Object name, Lisp_Object screen);
extern Lisp_Object Ffontp (Lisp_Object obj);
extern Lisp_Object Ffont_name (Lisp_Object obj);
extern Lisp_Object Ffont_truename (Lisp_Object obj);
extern Lisp_Object Qfontp;

struct Lisp_Font {
  struct lcrecord_header header;
  Lisp_Object name;
  Lisp_Object truename;
  Screen *screen;		/* An X screen, not an emacs screen */
  XFontStruct *font;		/* 0 if actually a tty */

  unsigned short ascent;	/* extracted from `font', or made up */
  unsigned short descent;
  unsigned short width;
  unsigned short height;
  char proportional_p;
};

#define XPIXMAP(a) ((struct Lisp_Pixmap *) XPNTR(a))
#define PIXMAPP(x) RECORD_TYPEP ((x), lrecord_pixmap)
#define CHECK_PIXMAP(x, i) \
 do { if (!PIXMAPP ((x))) x = wrong_type_argument (Qpixmapp, (x)); } while (0)
extern const struct lrecord_implementation lrecord_pixmap[];
extern Lisp_Object Fmake_pixmap (Lisp_Object name, Lisp_Object screen);
Lisp_Object make_pixmap (Lisp_Object name, Lisp_Object screen, int force_mono);
extern Lisp_Object Fpixmapp (Lisp_Object obj);
extern Lisp_Object Fpixmap_file_name (Lisp_Object obj);
extern Lisp_Object Fcolorize_pixmap (Lisp_Object, Lisp_Object, Lisp_Object);
extern Lisp_Object make_pixmap_from_data (Screen *screen, char *bits,
					  int width, int height);
extern Lisp_Object Qpixmapp;

struct Lisp_Pixmap {
  struct lcrecord_header header;
  Lisp_Object file_name;
  Screen *screen;
  unsigned short width, height;
  unsigned int depth;	/* depth 0 means bitmap rather than pixmap */
  int x, y;		/* hotspot */
  Pixmap pixmap;
  Pixmap mask;

  /* If depth>0, then that means that other colors were allocated when
     this pixmap was loaded.  These are they; we need to free them when
     finalizing the pixmap. */
  unsigned long *pixels;
  int npixels;

  /* Should we hang on to the extra info from the XpmAttributes, like
     the textual color table and the comments?   Is that useful? */
};

#endif /* _EMACS_XOBJS_H_ */
