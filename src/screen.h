/* Define screen-object for GNU Emacs.
   Copyright (C) 1988, 1992 Free Software Foundation, Inc.

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


#ifdef MULTI_SCREEN

enum output_method
{ output_termcap, output_x_window };

#include "dispextern.h"

#ifdef ENERGIZE
#include <cdisplayctx.h>
#endif

struct screen
{
  int size;
  struct Lisp_Vector *next;

  /* glyphs as they appear on the screen */
  struct screen_glyphs *current_glyphs;

  /* glyphs we'd like to appear on the screen */
  struct screen_glyphs *desired_glyphs;

  /* Cost of inserting 1 line on this screen */
  int *insert_line_cost;

  /* Cost of deleting 1 line on this screen */
  int *delete_line_cost;

  /* Cost of inserting n lines on this screen */
  int *insert_n_lines_cost;

  /* Cost of deleting n lines on this screen */
  int *delete_n_lines_cost;

  /* glyphs for the mode line */
  struct screen_glyphs *temp_glyphs;

  /* Intended cursor position of this screen.
     Measured in characters, counting from upper left corner
     within the screen.  */
  int cursor_x;
  int cursor_y;

  /* Is the cursor turned off? */
  int cursor_erased;

  /* Actual cursor position of this screen.
     (Not used for terminal screens.)  */
  int phys_cursor_x;
  int phys_cursor_y;

  /* Size of this screen, in units of characters.  */
  int height;
  int width;

  /* New height and width for pending size change.  0 if no change pending.  */
  int new_height, new_width;

  /* Name of this screen: a Lisp string.  */
  Lisp_Object name;

  /* This screen's root window.  Every screen has one.
     If the screen has only a minibuffer window, this is it.
     Otherwise, if the screen has a minibuffer window, this is its sibling.  */
  Lisp_Object root_window;

  /* This screen's selected window.
     Each screen has its own window hierarchy
     and one of the windows in it is selected within the screen.
     The selected window of the selected screen is Emacs's selected window.  */
  Lisp_Object selected_window;

  /* This screen's minibuffer window.
     Most screens have their own minibuffer windows,
     but only the selected screen's minibuffer window
     can actually appear to exist.  */
  Lisp_Object minibuffer_window;

  /* A copy of the global Vbuffer_list, to maintain a per-screen buffer
     ordering.  The Vbuffer_list variable and the buffer_list slot of each
     screen contain exactly the same data, just in different orders.  */
  Lisp_Object buffer_alist;

  /* Parameter alist of this screen.
     These are the parameters specified when creating the screen
     or modified with modify-screen-parameters.  */
  Lisp_Object param_alist;

  /* Vector representing the menubar currently displayed.  See menubar.c. */
  Lisp_Object menubar_data;

  /* The output method says how the contents of this screen
     are displayed.  It could be using termcap, or using an X window.  */
  enum output_method output_method;

  /* A structure of auxiliary data used for displaying the contents.
     struct x_display is used for X window screens;
     it is defined in xterm.h.  */
  union display { struct x_display *x; int nothing; } display;

  /* Nonzero if last attempt at redisplay on this screen was preempted.  */
  char display_preempted;

  /* Nonzero if screen is currently displayed.  */
  char visible;

  /* Nonzero if window is currently iconified.
     This and visible are mutually exclusive.  */
  char iconified;

  /* Nonzero if this screen should be redrawn.  */
  char garbaged;

  /* True if screen actually has a  minibuffer window on it.
     0 if using a minibuffer window that isn't on this screen.  */
  char has_minibuffer;
     
  /* 0 means, if this screen has just one window,
     show no modeline for that window.  */
  char wants_modeline;

  /* True if screen's root window can't be split.  */
  char no_split;

  /* Storage for messages to this screen. */
  char *message_buf;

  /* list of faces */
  struct face**	faces;
  int n_faces;

  /* the lisp data (shadowing the C data) */
  Lisp_Object face_alist;

#ifdef ENERGIZE
  DisplayContext*	display_context;
#endif
};

typedef struct screen *SCREEN_PTR;

#define XSCREEN(p) ((struct screen *) XPNTR (p))
#define XSETSCREEN(p, v) ((struct screen *) XSETPNTR (p, v))

#define WINDOW_SCREEN(w) (w)->screen

#define SET_SCREEN_GARBAGED(s) (screen_garbaged = 1, s->garbaged = 1)
#define SCREEN_IS_TERMCAP(s) ((s)->output_method == output_termcap)
#define SCREEN_IS_X(s) ((s)->output_method == output_x_window)
#define SCREEN_CURRENT_GLYPHS(s) (s)->current_glyphs
#define SCREEN_DESIRED_GLYPHS(s) (s)->desired_glyphs
#define SCREEN_TEMP_GLYPHS(s) (s)->temp_glyphs
#define SCREEN_HEIGHT(s) (s)->height
#define SCREEN_WIDTH(s) (s)->width
#define SCREEN_NEW_HEIGHT(s) (s)->new_height
#define SCREEN_NEW_WIDTH(s) (s)->new_width
#define SCREEN_CURSOR_X(s) (s)->cursor_x
#define SCREEN_CURSOR_Y(s) (s)->cursor_y
#define SCREEN_VISIBLE_P(s) (s)->visible
#define SCREEN_GARBAGED_P(s) (s)->garbaged
#define SCREEN_NO_SPLIT_P(s) (s)->no_split
#define SCREEN_WANTS_MODELINE_P(s) (s)->wants_modeline
#define SCREEN_ICONIFIED_P(s) (s)->iconified
#define SCREEN_MINIBUF_WINDOW(s) (s)->minibuffer_window
#define SCREEN_ROOT_WINDOW(s) (s)->root_window
#define SCREEN_SELECTED_WINDOW(s) (s)->selected_window
#define SET_GLYPHS_SCREEN(glyphs,screen) ((glyphs)->screen = (screen))
#define SCREEN_INSERT_COST(s) (s)->insert_line_cost    
#define SCREEN_DELETE_COST(s) (s)->delete_line_cost    
#define SCREEN_INSERTN_COST(s) (s)->insert_n_lines_cost
#define SCREEN_DELETEN_COST(s) (s)->delete_n_lines_cost
#define SCREEN_MESSAGE_BUF(s) (s)->message_buf

#define SCREEN_NORMAL_FACE(s) (*(s)->faces [0])
#define SCREEN_MODELINE_FACE(s) (*(s)->faces [1])
#define SCREEN_HIGHLIGHT_FACE(s) (*(s)->faces [2])

#define CHECK_SCREEN(x, i) \
  { if (!SCREENP ((x))) x = wrong_type_argument (Qscreenp, (x)); }
extern Lisp_Object Qscreenp;

extern struct screen *selected_screen;

extern struct screen *make_terminal_screen ();
extern struct screen *make_screen ();
extern struct screen *make_minibuffer_screen ();
extern struct screen *make_screen_without_minibuffer ();

extern Lisp_Object Vscreen_list;
extern Lisp_Object Vglobal_minibuffer_screen;

extern Lisp_Object Vterminal_screen;

#else /* not MULTI_SCREEN */

/* These definitions are used in a single-screen version of Emacs.  */

#define SCREEN_PTR int

extern int selected_screen;

#define XSCREEN(s) selected_screen
#define WINDOW_SCREEN(w) selected_screen

#define SET_SCREEN_GARBAGED(s) (screen_garbaged = 1)
#define SCREEN_IS_TERMCAP(s) 1
#define SCREEN_CURRENT_GLYPHS(s) current_glyphs
#define SCREEN_DESIRED_GLYPHS(s) desired_glyphs
#define SCREEN_TEMP_GLYPHS(s) temp_glyphs
#define SCREEN_HEIGHT(s) screen_height
#define SCREEN_WIDTH(s) screen_width
#define SCREEN_NEW_HEIGHT(s) delayed_screen_height
#define SCREEN_NEW_WIDTH(s) delayed_screen_width
#define SCREEN_CURSOR_X(s) cursX
#define SCREEN_CURSOR_Y(s) cursY
#define SCREEN_VISIBLE_P(s) 1
#define SCREEN_GARBAGED_P(s) screen_garbaged
#define SCREEN_NO_SPLIT_P(s) 0
#define SCREEN_WANTS_MODELINE_P(s) 1
#define SCREEN_ICONIFIED_P(s) 0
#define SCREEN_MINIBUF_WINDOW(s) minibuf_window
#define SCREEN_ROOT_WINDOW(s) root_window
#define SCREEN_SELECTED_WINDOW(s) selected_window
#define SCREENP(s) 0
#define SET_GLYPHS_SCREEN(glyphs,screen)
#define SCREEN_INSERT_COST(screen)  insert_line_cost    
#define SCREEN_DELETE_COST(screen)  delete_line_cost    
#define SCREEN_INSERTN_COST(screen) insert_n_lines_cost
#define SCREEN_DELETEN_COST(screen) delete_n_lines_cost
#define SCREEN_MESSAGE_BUF(s) message_buf

#define SCREEN_NORMAL_FACE(s) normal_face
#define SCREEN_HIGHLIGHT_FACE(s) highlight_face
#define SCREEN_MODELINE_FACE(s) modeline_face
#define SCREEN_SELECTION_FACE(s) selection_face
#define SCREEN_SECONDARY_SELECTION_FACE(s) secondary_selection_face
#define SCREEN_OVERLAP_SELECTION_FACE(s) overlap_selection_face

extern int screen_width, screen_height;
extern int cursX, cursY;

#endif /* not MULTI_SCREEN */

extern SCREEN_PTR choose_minibuf_screen ();
