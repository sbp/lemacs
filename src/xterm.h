/* Definitions and headers for communication with X protocol.
   Copyright (C) 1989, 1992, 1993 Free Software Foundation, Inc.

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

#ifndef _EMACS_XTERM_H_
#define _EMACS_XTERM_H_

#include <X11/Xlib.h>
#include <X11/cursorfont.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <X11/Xatom.h>
#include <X11/Xresource.h>

#include <X11/Intrinsic.h>  /* must be before lisp.h */

#include "blockio.h"

#define MAX_LINE_WIDTH(s) (PIXEL_WIDTH (s) \
			    - (2 * (s)->display.x->internal_border_width))

struct x_pixmap
{
  Pixmap pixmap, mask;
  GLYPH glyph_id;		/* index into glyph_to_x_pixmaps */
  unsigned short face_id;	/* ~0 means use colors of current face */
  unsigned int width;
  unsigned int height;
  unsigned int depth;		/* 0 depth means this is a bitmap of depth 1,
				   and 1 represents the foreground color.
				   Nonzero depth means that the cells of the
				   pixmap are actual pixel values.  This
				   matters because BlackPixel/WhitePixel vary.
				 */
};

extern struct x_pixmap builtin_continuer_pixmap;
extern struct x_pixmap builtin_truncator_pixmap;
extern struct x_pixmap builtin_rarrow_pixmap;

extern struct x_pixmap *glyph_to_x_pixmap (GLYPH g);


#define EOL_CURSOR_WIDTH 4

#define XFlushQueue() {BLOCK_INPUT; XFlush(x_current_display); UNBLOCK_INPUT;}
#define FONT_TYPE XFontStruct

extern XCharStruct *x_char_info ();

#define X_CHAR_WIDTH(font,ch) ((font)->per_char 		    \
			       ? x_char_info ((font), (ch))->width  \
			       : ((ch), (font)->min_bounds.width))
#define X_DEFAULT_WIDTH(font) (X_CHAR_WIDTH (font, font->default_char))

#define FONT_WIDTH(f)	((f)->max_bounds.width)
#define FONT_HEIGHT(f)	((f)->ascent + (f)->descent)
#define FONT_BASE(f)    ((f)->ascent)

#ifndef sigmask
#define sigmask(no) (1L << ((no) - 1))
#endif


/* This is the X connection that we are using.  */

extern Display *x_current_display;

extern struct screen *x_window_to_screen (Window);

/* Variables associated with the X display screen this emacs is using. */

extern Lisp_Object Vx_gc_pointer_shape;


#define PIXEL_WIDTH(s) ((s)->display.x->pixel_width)
#define PIXEL_HEIGHT(s) ((s)->display.x->pixel_height)

/* Each X screen object points to its own struct x_display object
   in the display.x field.  The x_display structure contains all
   the information that is specific to X windows.  */

struct x_display
{
  /* Position of the X window (x and y offsets in root window).  */
  int left_pos;
  int top_pos;

  /* Size of the X window in pixels, including internal border. */
  int pixel_height, pixel_width;

  /* Here are the Graphics Contexts for the default font. */
  GC normal_gc;				/* Normal video */
  GC reverse_gc;			/* Reverse video */
  GC cursor_gc;				/* cursor drawing */
#ifdef LINE_INFO_COLUMN
  GC line_info_gc;			/* lineinfo column */
#endif

  /* Width of the internal border.  This is a line of background color
     just inside the window's border.  When the screen is selected,
     a highlighting is displayed inside the internal border.  */

  int internal_border_width;

#ifdef LINE_INFO_COLUMN
  /* Width of the line info column.  The line info column appears to
     the left of the left margin, inside the internal border.  It is
     used to display glyphs related to each line. */
  int line_info_column_width;
  int default_line_info_column_width;

  PIX_TYPE line_info_background_pixel;
#endif

/*  FONT_TYPE *font; */
  int text_height;

  /* Flag to set when the X window needs to be completely repainted. */
  int needs_exposure;

  /* The widget of this screen.  This is the window of a "shell" widget. */
  Widget widget;
  /* The XmPanedWindows... */
  Widget column_widget;
#ifdef LINE_INFO_WIDGET
  Widget row_widget;
  /* The widget of the line-info widget */
  Widget lineinfo_widget;
#endif
  /* The widget of the edit portion of this screen; the window in
     "window_desc" is inside of this. */
  Widget edit_widget;

  Widget menubar_widget;

#ifdef ENERGIZE
  /* The Energize property-sheets.  The current_ slots are the ones which are
     actually on the screen.  The desired_ slots are the ones which should
     be there.  Redisplay synchs these.
   */
  int *current_psheets;
  int *desired_psheets;
  int current_psheet_count;
  int desired_psheet_count;
  Lisp_Object current_psheet_buffer;
  Lisp_Object desired_psheet_buffer;
#endif

  /* This is true if we own the window, that is, it is not a window that
     was created by another process.  If we don't own the window, we aren't
     allowed to destroy it. "The window" referred to is always window_desc;
     if USE_WIDGET is true, we always own the window inside of the
     edit_widget. */
  char own_window;

  /* Whether this screen has the keyboard focus locked on it, whether the
     mouse is in this screen, and whether this is the screen currently
     receiving keyboard input.  In point-to-type mode, focus_p will never
     be true, and mouse_p and input_p will be the same.  In click-to-type
     mode, input_p will be the same as focus_p, and mouse_p will vary.
     Generally, input_p is the only interesting value, but the other two
     are necessary state to correctly interpret the interactions between
     FocusIn, FocusOut, EnterNotify, and LeaveNotify events.
     
     These must be per-screen properties instead of global variables,
     because screens are not necessarily on the same monitor, so more than
     one can have a mouse, and more than one can have keyboard focus.
   */
  char focus_p;
/*  char mouse_p; */
/*  char input_p; */
  
  /* 1 if the screen is completely visible on the display, 0 otherwise.
     if 0 the screen may have been iconified or may be totally
     or parrtially hidden by another X window */
  char totally_visible_p;
};

/* Number of pixels below each line. */
extern int x_interline_space;

#endif /* _EMACS_XTERM_H_ */
