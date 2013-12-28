/* Interface definitions for display code.
   Copyright (C) 1985, 1992, 1993 Free Software Foundation, Inc.

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

#ifndef DISPEXTERN_DOT_H
#define DISPEXTERN_DOT_H

/* Nonzero means don't assume anything about current
   contents of actual terminal screen */
extern int screen_garbaged;

/* Nonzero means last display completed and cursor is really at
   cursX, cursY.  Zero means it was preempted. */
extern int display_completed;

/* Nonzero if window sizes or contents have changed
   since last redisplay that finished */
extern int windows_or_buffers_changed;

#ifdef HAVE_X_WINDOWS
#include <X11/Xlib.h>
#endif

struct face
{
  unsigned char underline;
  unsigned char hilited;
  unsigned char modif;
#ifdef HAVE_X_WINDOWS
  GC 		facegc;
  XFontStruct*	font;
#if 0
  char* 	font_name;	/* missing piece of info from XFontStruct... */
#endif				/* we store this up in lisp now */
  unsigned long	foreground;
  unsigned long	background;
  Pixmap	back_pixmap;
  unsigned int	pixmap_w, pixmap_h /* , pixmap_depth */;
#endif /* HAVE_X_WINDOWS */
};

enum run_type
{
  unused_run,
  glyph,
  column_glyph,
  font,
  space,
  window			/* indicate a window change */
};

struct run
{
  enum run_type type;
  int length;			/* Length in glyphs */
  struct face *faceptr;
  struct window* w;		/* window starting here */
  int bufp;			/* position in buffer */
  Lisp_Object class;		/* extent containing, if any */
  int begin_p;			/* 1 begin glyph 0 if end glyph*/
#ifdef HAVE_X_WINDOWS
  int pix_length;		/* Length in pixels */
#endif
#ifdef LINE_INFO_COLUMN
  int lineinfo_glyph_index;	/* Since glyphs in the lineinfo column don't
				   go into the glyphs array (because they
				   don't take up screen space) we need to
				   store them somewhere. */
#endif
};

/* This structure is used for the actual display of text on a screen.

   There are two instantiations of it:  the glyphs currently displayed,
   and the glyphs we desire to display.  The latter object is generated
   from buffers being displayed. */

struct screen_glyphs
  {
#ifdef MULTI_SCREEN
    struct screen *screen;	/* Screen these glyphs belong to. */
#endif /* MULTI_SCREEN */
    int height;
    int width;

    int *used;			/* Vector of widths (in chars) of lines. */
    GLYPH **glyphs;		/* glyphs[Y][X] is the GLYPH at X,Y. */
    GLYPH *total_contents;	/* The actual contents. `glyphs' points here */
    char *enable;		/* Vector indicating meaningful contents. */
    int   *bufp;		/* Buffer offset of this line's first char. */
    int *nruns;			/* N runs of differently displayed text. */
    struct run **face_list;
    struct run *faces;

#ifdef HAVE_X_WINDOWS
    short *top_left_x;		/* Pixel position of top left corner */
    short *top_left_y;
    short *pix_width;		/* Pixel width of line. */
    short *pix_height;		/* Pixel height of line. */
    short *max_ascent;		/* Pixel value of max font->ascent of line. */
#endif	/* HAVE_X_WINDOWS */
  };

#define GLYPH_CLASS_VECTOR_SIZE 100

struct glyphs_from_chars
  {
    UCHAR c;
    GLYPH *glyphs;
#ifdef LINE_INFO_COLUMN
    Lisp_Object info_column_glyphs;
#endif
    int columns;
    struct face *faceptr;
    Lisp_Object begin_class[GLYPH_CLASS_VECTOR_SIZE];
    Lisp_Object end_class[GLYPH_CLASS_VECTOR_SIZE];
    short n_nonfont;
    int next_visible;
    UCHAR begin_or_end;		/* 0:none, 1:end, 2:begin, 3:both */
#ifdef HAVE_X_WINDOWS
    int begin_columns;
    int begin_pixel_width;
    int end_columns;
    int end_pixel_width;
    int pixel_width;
    int pixel_height;
#endif
    /* The chars in the buffer being processed that lie between
       [run_pos_lower, run_pos_upper] are promised to be in the same run,
       and to be separated from begin and/or end glyphs, provided they fit
       on the same line. This means that there are NO pixel-maps 
       (sometimes called glyphs in other contexts) or font changes in the 
       display of the chars in this range. It is NOT promised to be a 
       maximal such range. */
    int run_pos_lower;
    int run_pos_upper;
  };

#ifdef HAVE_X_WINDOWS
struct glyph_dimensions
  {
    int top_left_x;
    int top_left_y;
    int width;
    int height;
    int run;
  };
#endif /* HAVE_X_WINDOWS */

#define GLYPH_LINE_SIZE(s) ((int) SCREEN_DESIRED_GLYPHS (s)->glyphs[1]   \
			    - (int) SCREEN_DESIRED_GLYPHS (s)->glyphs[0])
#define RUN_LINE_SIZE(s) ((int) SCREEN_DESIRED_GLYPHS (s)->face_list[1]     \
			    - (int) SCREEN_DESIRED_GLYPHS (s)->face_list[0])


#ifdef HAVE_X_WINDOWS
/* The window width preceding the truncation glyph */
#define TRUNCATE_WIDTH(s) (SCREEN_IS_X (s)			\
			? (MAX_LINE_WIDTH (s)			\
			   - builtin_truncator_pixmap.width)	\
			: max_width - 1)
/* The window width preceding the continuer glyph */
#define CONTINUE_WIDTH(s) (SCREEN_IS_X (s)			\
			? (MAX_LINE_WIDTH (s) -			\
			   builtin_continuer_pixmap.width)	\
			: max_width - 1)
#endif

extern void get_display_line ();
extern struct face default_face;

extern char *message_buf;
extern int message_buf_size;
extern int in_display;

extern struct face *change_attributes ();
extern void mark_attributes ();
extern struct face *copy_attributes ();
extern struct glyph_dimensions *get_glyph_dimensions ();
extern struct glyphs_from_chars *glyphs_from_bufpos ();

#endif /* DISPEXTERN_DOT_H */
