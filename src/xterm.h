/* Definitions and headers for communication with X protocol.
   Copyright (C) 1989, 1992, 1993, 1994 Free Software Foundation, Inc.

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

/* #include <signal.h>  use "syssignal.h" instead -jwz */
#include "syssignal.h"
#include <X11/Xlib.h>
#include <X11/cursorfont.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <X11/Xatom.h>
#include <X11/Xresource.h>

#include "xintrinsic.h"

#include "blockio.h"

/* R5 defines the XPointer type, but R4 doesn't.  
   R4 also doesn't define a version number, but R5 does. */
#if (XlibSpecificationRelease < 5)
# define XPointer char *
#endif


#define MAX_LINE_WIDTH(s) (PIXEL_WIDTH (s) \
			    - (2 * (s)->display.x->internal_border_width))

extern GLYPH truncator_glyph, continuer_glyph;
extern Lisp_Object glyph_to_pixmap (GLYPH g);
extern unsigned short glyph_width (GLYPH g, Lisp_Object font);
extern unsigned short glyph_height (GLYPH g, Lisp_Object font);

#define EOL_CURSOR_WIDTH 4

#define XFlushQueue() {BLOCK_INPUT; XFlush(x_current_display); UNBLOCK_INPUT;}

#ifdef I18N4
#define FONT_WIDTH(f)	(int) (XExtentsOfFontSet (f)->max_logical_extent.width)
#define FONT_HEIGHT(f)	(int) (XExtentsOfFontSet (f)->max_logical_extent.height)
#define FONT_BASE(f)    (short) (-XExtentsOfFontSet (f)->max_logical_extent.y)
#define FONT_ASCENT(f)	(short) (-XExtentsOfFontSet (f)->max_logical_extent.y)
#define FONT_DESCENT(f) (short) (FONT_HEIGHT(f) - FONT_ASCENT(f))
#else
#define FONT_WIDTH(f)	((f)->max_bounds.width)
#define FONT_HEIGHT(f)	((f)->ascent + (f)->descent)
#define FONT_BASE(f)    ((f)->ascent)
#define FONT_ASCENT(f)	((f)->ascent)
#define FONT_DESCENT(f)	((f)->descent)
#endif

#ifndef sigmask
#define sigmask(no) (1L << ((no) - 1))
#endif


/* This is the X connection that we are using.  */

extern Display *x_current_display;

extern struct screen *x_window_to_screen (Window);

/* Variables associated with the X display screen this emacs is using. */

extern Lisp_Object Vx_gc_pointer_shape;
extern Lisp_Object Vx_scrollbar_pointer_shape;


#define PIXEL_WIDTH(s) ((s)->display.x->pixel_width)
#define PIXEL_HEIGHT(s) ((s)->display.x->pixel_height)

/* The maximum number of widgets that can be displayed above the text
   area at one time.  Currently no more than 3 will ever actually be
   displayed (menubar, psheet, debugger panel). */
#define MAX_CONCURRENT_TOP_WIDGETS 8

/* Each X screen object points to its own struct x_display object
   in the display.x field.  The x_display structure contains all
   the information that is specific to X windows.  */

struct x_display
{
  /******************** Info about the X window **********************/

  /* This info is copied from the EmacsScreen, for easy reference */

  /* Size of the X window in pixels, including internal border. */
  int pixel_height, pixel_width;

  /* Width of the internal border.  This is a line of background color
     just inside the window's border. */
  int internal_border_width;

  /***************************** Xt fields ***************************/

  /* The widget of this screen.  This is an EmacsShell or an
     ExternalShell. */
  Widget widget;

  /* The parent of the EmacsScreen, the menubar, and the scrollbar area.
     This is an EmacsManager. */
  Widget container;

  /* The widget of the menubar, of whatever widget class it happens to be. */
  Widget menubar_widget;

  /* The widget of the edit portion of this screen; this is an EmacsScreen,
     and the window of this widget is what the redisplay code draws on. */
  Widget edit_widget;

  /* The parent of the scrollbars (a child of the `container'). */
  Widget scrollbar_manager;

  /* Lists the widgets above the text area, in the proper order.
     Used by the EmacsManager. */
  Widget top_widgets[MAX_CONCURRENT_TOP_WIDGETS];

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

  /*************************** Miscellaneous **************************/

  /* The icon pixmaps; these are Lisp_Pixmap objects, or Qnil. */
  Lisp_Object icon_pixmap;
  Lisp_Object icon_pixmap_mask;

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
  
  /* 1 if the screen is completely visible on the display, 0 otherwise.
     if 0 the screen may have been iconified or may be totally
     or partially hidden by another X window */
  char totally_visible_p;

  /* NB: Both of the following flags are derivable from the 'shell'
     field above, but it's easier if we also have them separately here. */

  /* Are we a top-level screen?  This means that our shell is a
     TopLevelShell, and we should do certain things to interact with
     the window manager. */
  char top_level_screen_p;

#ifdef EXTERNAL_WIDGET
  /* Are we using somebody else's window for our shell window?  This
     means that our shell is an ExternalShell.  If this flag is set, then
     `top_level_screen_p' will never be set. */
  char external_window_p;
#endif /* EXTERNAL_WIDGET */
};

/* Number of pixels below each line. */
extern int x_interline_space;

extern int reasonable_glyph_index_p (int index);

extern void x_clear_display_line (int vpos, int pix_width);
extern int x_write_glyphs (struct screen *s, int left, int top, int n,
                           GC force_gc, int box_p);
extern void x_clear_display_line_end (int vpos);
extern void x_new_selected_screen (struct screen *screen);
extern void x_focus_screen (struct screen *s);
/* >>> Need declaration of "union display" to be able to include this */
/* extern void x_destroy_window (struct screen *s, union display displ); */
extern void x_read_mouse_position (struct screen *s, int *x, int *y);
extern void x_set_mouse_position (struct screen *s, int x, int y);
extern void x_make_screen_visible (struct screen *s);
extern void x_make_screen_invisible (struct screen *s);
extern void x_iconify_screen (struct screen *s);
extern void x_report_screen_params (struct screen *s, Lisp_Object *alistptr);
extern void x_set_screen_values (struct screen *s, Lisp_Object alist);
extern void x_set_window_size (struct screen *s, int cols, int rows);
extern void x_set_offset (struct screen *s, int xoff, int yoff);
extern void x_set_scrollbar_width (struct screen *s, int width);
extern int x_scrollbar_width (struct screen *s);
extern void x_raise_screen (struct screen *s, int force);
extern void x_lower_screen (struct screen *s);
extern void x_format_screen_title (struct screen *s);
extern void x_screen_redraw_cursor (struct screen *screen);
extern Lisp_Object x_term_init (Lisp_Object argv_list);
extern void x_set_title_from_char (struct screen* s, char* name);
extern void x_set_icon_name_from_char (struct screen* s, char* name);

extern void x_handle_selection_notify (XSelectionEvent *event);
extern void x_handle_selection_request (XSelectionRequestEvent *event);
extern void x_handle_selection_clear (XSelectionClearEvent *event);
extern void x_handle_property_notify (XPropertyEvent *event);

extern void repaint_lines (struct screen *, 
                           int left, int top, int width, int height);
extern void Cdumprectangle (register int, register int, register int,
			    register int cols, struct screen *s);
extern void Fastmove_cursor (struct screen *);
extern void update_cursor (struct screen *s, int blit);

extern Lisp_Object Fx_get_resource (Lisp_Object name, Lisp_Object class, 
                                    Lisp_Object type, Lisp_Object screen);


extern void x_init_modifier_mapping (Display *display);

extern Lisp_Object Fx_create_screen (Lisp_Object parms, Lisp_Object lisp_window_id);

extern void x_destroy_window (struct screen *s);

extern int x_selection_timeout;

extern void emacs_Xt_focus_event_handler (XEvent *x_event,
                                          struct screen *s);
extern void emacs_Xt_make_event_stream (void);

extern struct screen *x_any_window_to_screen (Window);

extern void x_format_screen_title (struct screen *);

extern struct screen *get_x_screen (Lisp_Object);

extern Lisp_Object text_part_sym;     /* 'text-part */
extern Lisp_Object modeline_part_sym; /* 'modeline-part */

extern Lisp_Object x_term_init (Lisp_Object);
extern void Xatoms_of_xselect (void);
extern void Xatoms_of_xobjs (void);

extern void init_bitmap_tables_1 (void); /* xterm.c */
extern Lisp_Object WM_COMMAND_screen; /* in xfns.c */

extern void unmap_unmarked_subwindows_of_screen (struct screen *);
extern void mark_subwindow_display_status (struct screen *, int);

extern void x_wm_mark_shell_size_user_specified (Widget wmshell);
extern void x_wm_mark_shell_position_user_specified (Widget wmshell);
extern void x_wm_set_shell_iconic_p (Widget shell, int iconic_p);
extern void x_wm_set_cell_size (Widget wmshell, int cw, int ch);
extern void x_wm_set_variable_size (Widget wmshell, int width, int height);
extern void x_wm_maybe_move_wm_command (struct screen *s);

#ifdef DUBIOUS_X_ERROR_HANDLING

extern Lisp_Object Qx_error;

extern void expect_x_error (Display *dpy);
extern int x_error_occurred_p (Display *dpy);
extern int signal_if_x_error (Display *dpy, int resumable_p);

#define X_ERROR_OCCURRED(dpy, body)	\
     (expect_x_error ((dpy)), (body), x_error_occurred_p (dpy))

#define HANDLING_X_ERROR(dpy, body)	\
     ( expect_x_error ((dpy)), (body), signal_if_x_error ((dpy), 0))

#endif /* dubiousness */


#endif /* _EMACS_XTERM_H_ */
