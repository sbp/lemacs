/* Definitions and headers for communication with X protocol.
   Copyright (C) 1989 Free Software Foundation, Inc.

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

#include <X11/Xlib.h>
#include <X11/cursorfont.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <X11/Xatom.h>
#include <X11/Xresource.h>

#ifdef ENERGIZE
#include <cimage.h>
#endif

#include <X11/Intrinsic.h>  /* must be before lisp.h */

#include "blockio.h"

/* Define a queue for X-events.  One such queue is used for mouse clicks.
   Another is used for expose events.  */

#define EVENT_BUFFER_SIZE 64

#define RES_CLASS "emacs"

/* Max and Min sizes in character columns. */
#define MINWIDTH 10
#define MINHEIGHT 10
#define MAXWIDTH 300
#define MAXHEIGHT 80

/* Pixel sized limits */
#define MIN_BITMAP_WIDTH 4
#define MAX_LINE_WIDTH(s) (PIXEL_WIDTH (s) \
			    - (2 * (s)->display.x->internal_border_width))

struct x_bitmap
{
  short width;
  short height;
  Pixmap image;
#ifdef ENERGIZE
  Image *cimage;
#endif
};

#ifdef ENERGIZE
extern struct x_bitmap *x_bitmaps;
extern struct x_bitmap *x_stipples;
#else
extern struct x_bitmap x_bitmaps[];
extern struct x_bitmap x_stipples[];
#endif

#define SELECTION_BITMAP 0
#define COMPRESS_BITMAP 1
#define IBEGIN_BITMAP 2
#define IEND_BITMAP 3
#define CONTINUER_BITMAP 4
#define TRUNCATOR_BITMAP 5
#define RARROW_BITMAP 6

#ifdef ENERGIZE
/* please keep this number up to date */
#define PREDEFINED_BITMAPS_UPPER_BOUND 7
#endif

#define DEFAULT0_STIPPLE 0
#define DEFAULT1_STIPPLE 1
#define SELECTION_STIPPLE 2
#define SECONDARY_SELECTION_STIPPLE 3
#define OVERLAP_SELECTION_STIPPLE 4

#define EOL_CURSOR_WIDTH 4

#define XFlushQueue() {BLOCK_INPUT; XFlush(x_current_display); UNBLOCK_INPUT;}
#define ROOT_WINDOW RootWindow (x_current_display, XDefaultScreen (x_current_display))
#define FONT_TYPE XFontStruct
#define Color XColor

#define XExposeRegionEvent XExposeEvent
#define Bitmap Pixmap			/* In X11, Bitmaps are are kind of
					   Pixmap. */
extern XCharStruct *x_char_info ();

#define X_CHAR_WIDTH(font,ch) ((font)->per_char 		    \
			       ? x_char_info ((font), (ch))->width  \
			       : ((ch), (font)->min_bounds.width))
#define X_DEFAULT_WIDTH(font) (X_CHAR_WIDTH (font, font->default_char))

#define FONT_WIDTH(f)	((f)->max_bounds.width)
#define FONT_HEIGHT(f)	((f)->ascent + (f)->descent)
#define TOTAL_HEIGHT(f)	((f)->ascent + (f)->descent + x_interline_space)
#define FONT_BASE(f)    ((f)->ascent)

#ifndef sigmask
#define sigmask(no) (1L << ((no) - 1))
#endif


/* This is the X connection that we are using.  */

extern Display *x_current_display;

extern struct screen *x_window_to_screen (Window);

/* Variables associated with the X display screen this emacs is using. */

/* How many screens this X display has. */
extern Lisp_Object x_screen_count;

/* The vendor supporting this X server. */
extern Lisp_Object Vx_vendor;

/* The vendor's release number for this X server. */
extern Lisp_Object x_release;

/* Height of this X screen in pixels. */
extern Lisp_Object x_screen_height;

/* Height of this X screen in millimeters. */
extern Lisp_Object x_screen_height_mm;

/* Width of this X screen in pixels. */
extern Lisp_Object x_screen_width;

/* Width of this X screen in millimeters. */
extern Lisp_Object x_screen_width_mm;

/* Does this X screen do backing store? */
extern Lisp_Object Vx_backing_store;

/* Does this X screen do save-unders? */
extern Lisp_Object x_save_under;

/* Number of planes for this screen. */
extern Lisp_Object x_screen_planes;

/* X Visual type of this screen. */
extern Lisp_Object Vx_screen_visual;

/* Pointer shapes */
extern Lisp_Object Vx_pointer_shape;
extern Lisp_Object Vx_nontext_pointer_shape;
extern Lisp_Object Vx_mode_pointer_shape;
extern Lisp_Object Vx_gc_pointer_shape;

/* X Atoms used for client and window manager communication. */

/* Atom for indicating window state to the window manager. */
extern Atom Xatom_wm_change_state;

/* When emacs became the selection owner. */
extern Time x_begin_selection_own;

/* The value of the current emacs selection. */
extern Lisp_Object Vx_selection_value;

/* Emacs' selection property identifier. */
extern Atom Xatom_emacs_selection;

/* Clipboard selection atom. */
extern Atom Xatom_clipboard_selection;

/* Clipboard atom. */
extern Atom Xatom_clipboard;

/* Atom for indicating incremental selection transfer. */
extern Atom Xatom_incremental;

/* Atom for indicating multiple selection request list */
extern Atom Xatom_multiple;

/* Atom for what targets emacs handles. */
extern Atom Xatom_targets;

/* Atom for indicating timstamp selection request */
extern Atom Xatom_timestamp;

/* Atom requesting we delete our selection. */
extern Atom Xatom_delete;

/* Selection magic. */
extern Atom Xatom_insert_selection;

/* Type of property for INSERT_SELECTION. */
extern Atom Xatom_pair;

/* More selection magic. */
extern Atom Xatom_insert_property;

/* Atom for indicating property type TEXT */
extern Atom Xatom_text;

/* Communication with window managers. */
extern Atom Xatom_wm_protocols;

/* Kinds of protocol things we may receive. */
extern Atom Xatom_wm_take_focus;
extern Atom Xatom_wm_save_yourself;
extern Atom Xatom_wm_delete_window;


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

  FONT_TYPE *font;
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
  Lisp_Object last_selected_buffer;

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
  char mouse_p;
  char input_p;
  
  /* 1 if the screen is completely visible on the display, 0 otherwise.
     if 0 the screen may have been iconified or may be totally
     or parrtially hidden by another X window */
  char totally_visible_p;
};

extern struct screen *x_mouse_screen;

/* Number of pixels below each line. */
extern int x_interline_space;

extern int reasonable_glyph_index_p ();
