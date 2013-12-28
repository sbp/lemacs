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

#ifndef _EMACS_FACES_H_
#define _EMACS_FACES_H_

struct face
{
#ifdef HAVE_X_WINDOWS
  Lisp_Object font;
  Lisp_Object foreground;
  Lisp_Object background;
  Lisp_Object back_pixmap;
#endif /* HAVE_X_WINDOWS */

  char underline;
  char hilited;		/* #### only used by dumb-tty code now... */
/*  char modif; */
};

#ifdef HAVE_X_WINDOWS
/* These access the slots of the underlying Lisp objects in the struct face.
   They all assume this is a fully qualified face (no slots are nil.) */
#define FACE_X_FONT(f) (XFONT (FACE_FONT(f))->font)
#define FACE_FG_PIXEL(f) (XPIXEL ((f)->foreground)->color.pixel)
#define FACE_BG_PIXEL(f) (XPIXEL ((f)->background)->color.pixel)
#define SCREEN_DEFAULT_X_FONT(s) (FACE_X_FONT(&SCREEN_NORMAL_FACE (s)))
#endif

/* This def is kind of useless at the moment but will make things easy
   when faces become Lisp_Object's */
#define FACE_FONT(f) ((f)->font)
#define SCREEN_DEFAULT_FONT(s) (FACE_FONT(&SCREEN_NORMAL_FACE (s)))

extern void make_face_ready (struct screen* s, struct face* face);
extern void flush_face_cache ();
extern void ensure_face_ready (struct screen* s, int f);
extern void init_screen_faces (struct screen *s);

#endif /* _EMACS_FACES_H_ */
