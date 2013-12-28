/* Generic screen functions.
   Copyright (C) 1989 Free Software Foundation.

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

#include "config.h"
#include "lisp.h"
#include "dispextern.h"
#include "screen.h"
#include "window.h"
#include "extents.h"
#include "faces.h"
#include <string.h>
#ifdef HAVE_X_WINDOWS
#include "xterm.h"
#endif

Lisp_Object Vscreen_list;
Lisp_Object Vterminal_screen;
Lisp_Object Vglobal_minibuffer_screen;

extern Lisp_Object Vminibuffer_list;
extern Lisp_Object get_minibuffer ();

Lisp_Object Qselect_screen_hook, Qdeselect_screen_hook;



#define get_screen(s, screen)		\
{					\
  if (NILP (screen))			\
    {					\
      s = selected_screen;		\
      XSET (screen, Lisp_Screen, s);	\
    }					\
  else					\
    {					\
      CHECK_SCREEN ((screen), 0);	\
      s = XSCREEN ((screen));		\
    }					\
}


struct screen *
make_screen (mini_p)
     int mini_p;
{
  Lisp_Object screen;
  register struct screen *s;
  register Lisp_Object root_window;
  register Lisp_Object mini_window;

  screen = Fmake_vector (sizeof (struct screen) - sizeof (Lisp_Vector) + 1,
			 make_number (0));
  XSETTYPE (screen, Lisp_Screen);
  s = XSCREEN (screen);

  s->cursor_x = 0;
  s->cursor_y = 0;
  s->cursor_erased = 1;
  s->current_glyphs = 0;
  s->desired_glyphs = 0;
  s->visible = 0;
  s->display.nothing = 0;
  s->iconified = 0;
  s->wants_modeline = 1;
  s->no_split = 0;
  s->garbaged = 0;
  s->has_minibuffer = mini_p;

  /* init_screen_faces will do more, but this is needed before then */
  s->n_faces = 0;
  s->faces = 0;
  s->face_alist = Qnil;
  ensure_face_ready (s, 3);

  s->param_alist = Qnil;
  s->menubar_data = Qnil;

  s->buffer_alist = Fcopy_sequence (Vbuffer_alist);

  root_window = make_window ();
  if (mini_p)
    {
      mini_window = make_window ();
      XWINDOW (root_window)->next = mini_window;
      XWINDOW (mini_window)->prev = root_window;
      XWINDOW (mini_window)->mini_p = Qt;
      XWINDOW (mini_window)->screen = screen;
      s->minibuffer_window = mini_window;
    }
  else
    {
      mini_window = Qnil;
      XWINDOW (root_window)->next = Qnil;
      s->minibuffer_window = Qnil;
    }

  XWINDOW (root_window)->screen = screen;

  /* 10 is arbitrary,
     just so that there is "something there."
     Correct size will be set up later with change_screen_size.  */

  s->width = 10;
  s->height = 10;

  XFASTINT (XWINDOW (root_window)->width) = 10;
  XFASTINT (XWINDOW (root_window)->height) = (mini_p ? 9 : 10);

  if (mini_p)
    {
      XFASTINT (XWINDOW (mini_window)->width) = 10;
      XFASTINT (XWINDOW (mini_window)->top) = 9;
      XFASTINT (XWINDOW (mini_window)->height) = 1;
    }

  XWINDOW (root_window)->buffer = Qt;
  Fset_window_buffer (root_window, Fcurrent_buffer ());
  if (mini_p)
    {
      XWINDOW (mini_window)->buffer = Qt;
      Fset_window_buffer (mini_window,
			  (NILP (Vminibuffer_list)
			   ? get_minibuffer (0)
			   : Fcar (Vminibuffer_list)));
    }

  s->selected_window = root_window;
  s->root_window = root_window;

  return s;
}

/* Make a screen using a separate minibuffer window on another screen.
   MINI_WINDOW is the minibuffer window to use.  nil means use the
   default (the global minibuffer).  */

struct screen *
make_screen_without_minibuffer (mini_window)
     register Lisp_Object mini_window;
{
  register struct screen *s;

  /* Choose the minibuffer window to use.  */
  if (NILP (mini_window))
    {
      CHECK_SCREEN (Vglobal_minibuffer_screen, 0);
      mini_window = XSCREEN (Vglobal_minibuffer_screen)->minibuffer_window;
    }
  else
    {
      CHECK_WINDOW (mini_window, 0);
    }

  /* Make a screen containing just a root window.  */
  s = make_screen (0);

  /* Install the chosen minibuffer window, with proper buffer.  */
  s->minibuffer_window = mini_window;
  Fset_window_buffer (mini_window,
		      (NILP (Vminibuffer_list)
		       ? get_minibuffer (0)
		       : Fcar (Vminibuffer_list)));
  return s;
}

/* Make a screen containing only a minibuffer window.  */

struct screen *
make_minibuffer_screen ()
{
  /* First make a screen containing just a root window, no minibuffer.  */

  register struct screen *s = make_screen (0);
  register Lisp_Object mini_window;
  register Lisp_Object screen;

  XSET (screen, Lisp_Screen, s);

  s->no_split = 1;
  s->wants_modeline = 0;
  /* Note we leave has_minibuffer as 0.  This is a little strange.  */

  /* Now label the root window as also being the minibuffer.
     Avoid infinite looping on the window chain by marking next pointer
     as nil. */

  mini_window = s->minibuffer_window = s->root_window;
  XWINDOW (mini_window)->mini_p = Qt;
  XWINDOW (mini_window)->next = Qnil;
  XWINDOW (mini_window)->prev = mini_window;
  XWINDOW (mini_window)->screen = screen;

  /* Put the proper buffer in that window.  */

  Fset_window_buffer (mini_window,
		      (NILP (Vminibuffer_list)
		       ? get_minibuffer (0)
		       : Fcar (Vminibuffer_list)));
  return s;
}

/* Construct a screen that refers to the terminal (stdin and stdout).  */

struct screen *
make_terminal_screen ()
{
  register struct screen *s;

  Vscreen_list = Qnil;
  s = make_screen (1);
  s->name = build_string ("terminal");
  s->visible = 1;
  s->display.nothing = 1;   /* Nonzero means screen isn't deleted.  */
  XSET (Vterminal_screen, Lisp_Screen, s);

  Vscreen_list = Fcons (Vterminal_screen, Vscreen_list);

  return s;
}

DEFUN ("screenp", Fscreenp, Sscreenp, 1, 1, 0,
  "Return non-nil if OBJECT is a screen.\n\
Value is t for a termcap screen (a character-only terminal),\n\
`x' for an Emacs screen that is really an X window.")
  (screen)
     Lisp_Object screen;
{
  if (XTYPE (screen) != Lisp_Screen)
    return Qnil;

  switch (XSCREEN (screen)->output_method)
    {
    case output_termcap:
      return Qt;
    case output_x_window:
      return intern ("x");
    default:
      abort ();
    }
}

void
select_screen (s)
     struct screen* s;
{
  if (s->display.nothing == 0)
    error ("Cannot select a dead screen");
  
  if (s != selected_screen)
    {
      if (!NILP (Vrun_hooks))
	call1 (Vrun_hooks, Qdeselect_screen_hook);

#ifdef HAVE_X_WINDOWS
      /* Do this before setting selected_screen as x_new_selected_screen
	 looks at the value of selected_screen to dehighlight the
	 previous screen */
      if (SCREEN_IS_X (s))
	x_new_selected_screen (s);
      else
#endif
	selected_screen = s;

      Fselect_window (s->selected_window);
      choose_minibuf_screen ();

      if (!NILP (Vrun_hooks))
	call1 (Vrun_hooks, Qselect_screen_hook);
    }

#ifdef HAVE_X_WINDOWS
  if (SCREEN_IS_X (s))
    {
      x_focus_screen (s);
    }
#endif
}  

DEFUN ("select-screen", Fselect_screen, Sselect_screen, 1, 1, 0,
  "Select the screen S.\n\
S's selected window becomes the selected window.")
  (screen)
     Lisp_Object screen;
{
  CHECK_SCREEN (screen, 0);

  select_screen (XSCREEN (screen));
  return screen;
}

DEFUN ("selected-screen", Fselected_screen, Sselected_screen, 0, 0, 0,
  "Return the screen that is now selected.")
  ()
{
  Lisp_Object tem;
  XSET (tem, Lisp_Screen, selected_screen);
  return tem;
}

DEFUN ("window-screen", Fwindow_screen, Swindow_screen, 1, 1, 0,
  "Return the screen that window WINDOW is on.")
  (window)
     Lisp_Object window;
{
  CHECK_WINDOW (window, 0);
  return XWINDOW (window)->screen;
}

DEFUN ("screen-root-window", Fscreen_root_window, Sscreen_root_window, 0, 1, 0,
       "Returns the root-window of SCREEN.")
  (screen)
     Lisp_Object screen;
{
  struct screen* s;
  get_screen (s, screen);
  return s->root_window;
}

DEFUN ("screen-selected-window", Fscreen_selected_window,
       Sscreen_selected_window, 0, 1, 0,
  "Return the selected window of screen SCREEN.")
  (screen)
     Lisp_Object screen;
{
  struct screen* s;
  get_screen (s, screen);
  return s->selected_window;
}

DEFUN ("screen-list", Fscreen_list, Sscreen_list,
       0, 0, 0,
       "Return a list of all screens.")
  ()
{
  return Fcopy_sequence (Vscreen_list);
}

#ifdef MULTI_SCREEN

Lisp_Object
next_screen (screen, mini_screen)
     Lisp_Object screen;
     int mini_screen;
{
  Lisp_Object tail;
  if (NILP (Vscreen_list))
    abort ();
  for (tail = Vscreen_list;
       !NILP (tail);
       tail = XCONS (tail)->cdr)
    {
      if (!mini_screen &&
	  EQ (XCONS (tail)->car, Vglobal_minibuffer_screen))
	continue;

      if (EQ (screen, XCONS (tail)->car))
	{
	  if (NILP (XCONS (tail)->cdr))
	    return XCONS (Vscreen_list)->car;
	  else
	    return XCONS (XCONS (tail)->cdr)->car;
	}
      if (EQ (XCONS (tail)->cdr, Vscreen_list))
	abort ();
    }
  abort ();
}


Lisp_Object
prev_screen (screen, mini_screen)
     Lisp_Object screen;
     int mini_screen;
{
  Lisp_Object tail, prev;
  if (NILP (Vscreen_list))
    abort ();
  for (tail = Vscreen_list, prev = Qnil;
       !NILP (tail);
       prev = XCONS (tail)->car, tail = XCONS (tail)->cdr)
    {
      if (!mini_screen &&
	  EQ (XCONS (tail)->car, Vglobal_minibuffer_screen))
	continue;

      if (EQ (screen, XCONS (tail)->car))
	{
	  if (NILP (prev))  /* meaning this is the first screen on the list */
	    {
	      while (!NILP (XCONS (tail)->cdr))
		tail = XCONS (tail)->cdr;
	      prev = XCONS (tail)->car;
	    }
	  return prev;
	}
      if (EQ (XCONS (tail)->cdr, Vscreen_list))
	abort ();
    }
  abort ();
}


DEFUN ("next-screen", Fnext_screen, Snext_screen,
       0, 2, 0,
       "Return the next screen in the screen list after SCREEN.\n\
If MINISCREEN is non-nil, include the global-minibuffer-screen if it\n\
has its own screen.")
  (screen, miniscreen)
Lisp_Object screen, miniscreen;
{
  Lisp_Object tail;
  struct screen* s;
  get_screen (s, screen);

  return next_screen (screen, (NILP (miniscreen) ? 0 : 1));
}
#endif /* MULTI_SCREEN */

DEFUN ("delete-screen", Fdelete_screen, Sdelete_screen,
       0, 1, "",
       "Delete SCREEN, permanently eliminating it from use.\n\
Default is current screen.")
  (screen)
     Lisp_Object screen;
{
  struct screen *s;
  union display displ;
  get_screen (s, screen);

  /* Don't allow deleted screen to remain selected.  */
  if (s == selected_screen)
    {
      Lisp_Object next;

      next = next_screen (screen, 0);
      if (EQ (next, screen))
	error ("Attempt to delete the only screen");
      Fselect_screen (next);
    }

  /* Don't allow the global minibuffer screen to be deleted */
  if (s == XSCREEN (Vglobal_minibuffer_screen))
    error ("Attempt to delete the global minibuffer screen");

  /* Don't allow minibuf_window to remain on a deleted screen.  */
  if (EQ (s->minibuffer_window, minibuf_window))
    {
      Fset_window_buffer (selected_screen->minibuffer_window,
			  XWINDOW (minibuf_window)->buffer);
      minibuf_window = selected_screen->minibuffer_window;
    }

  Vscreen_list = Fdelq (screen, Vscreen_list);
  s->visible = 0;

  if (s->display.nothing != 0)	/* i.e., it's not already deleted */
    {
      displ = s->display;
      s->display.nothing = 0;

      if (SCREEN_IS_X (s))
	x_destroy_window (s, displ);
    }
  
  if (s->faces)
    {
      int i;
      for (i = 0; i < s->n_faces; i++)
	free (s->faces [i]);
      free (s->faces);
      s->faces = 0;
      s->n_faces = 0;
    }

  return Qnil;
}

/* Return mouse position in character cell units.  */

static void
read_mouse_position (screen, x, y)
     Lisp_Object screen;
     int *x, *y;
{
  CHECK_SCREEN (screen, 0);

  *x = 1;
  *y = 1;

#ifdef HAVE_X_WINDOWS
  if (XSCREEN (screen)->output_method == output_x_window)
    x_read_mouse_position (XSCREEN (screen), x, y);
#endif
}

DEFUN ("read-mouse-position", Fread_mouse_position, Sread_mouse_position, 1, 1, 0,
  "Return a cons (x . y) which represents the position of the mouse.")
  (screen)
     Lisp_Object screen;
{
  int x, y;
  struct screen *s;

  CHECK_SCREEN (screen, 0);

  read_mouse_position (screen, &x, &y);
  return Fcons (make_number (x), make_number (y));
}

DEFUN ("set-mouse-position", Fset_mouse_position, Sset_mouse_position, 3, 3, 0,
  "Move the mouse pointer to the center of character cell (X,Y) in SCREEN.")
  (screen, x, y)
     Lisp_Object screen, x, y;
{
  CHECK_SCREEN (screen, 0);
  CHECK_NUMBER (x, 2);
  CHECK_NUMBER (y, 1);

#ifdef HAVE_X_WINDOWS
  if (XSCREEN (screen)->output_method == output_x_window)
    /* Warping the mouse will cause  enternotify and focus events. */
    x_set_mouse_position (XSCREEN (screen), x, y);
#endif
  return Qnil;
}

#if 0
/* ??? Can this be replaced with a Lisp function?
   It is used in minibuf.c.  Can we get rid of that?  */

DEFUN ("screen-configuration", Fscreen_configuration, Sscreen_configuration,
       0, 0, 0,
  "Return object describing current screen configuration.\n\
The screen configuration is the current mouse position and selected screen.\n\
This object can be given to `restore-screen-configuration'\n\
to restore this screen configuration.")
  ()
{
  int x, y;
  Lisp_Object c, screen;
  struct screen *s;
  
  c = Fmake_vector (make_number(3), Qnil);
  XVECTOR (c)->contents[0] = screen = Fselected_screen();
  read_mouse_position (screen, &x, &y);
  XVECTOR (c)->contents[1] = make_number (x);
  XVECTOR (c)->contents[2] = make_number (y);

  return c;
}

DEFUN ("restore-screen-configuration", Frestore_screen_configuration,
       Srestore_screen_configuration,
       1, 1, 0,
  "Restores screen configuration CONFIGURATION.")
  (config)
  Lisp_Object config;
{
  Lisp_Object x_pos, y_pos, screen;

  CHECK_VECTOR (config, 0);
  if (XVECTOR (config)->size != 3)
    {
      error ("Wrong size vector passed to restore-screen-configuration");
    }
  screen = XVECTOR (config)->contents[0];
  CHECK_SCREEN (screen, 0);

  Fselect_screen (screen);

  return screen;
}    
#endif

DEFUN ("make-screen-visible", Fmake_screen_visible, Smake_screen_visible,
       1, 1, 0,
  "Make the screen SCREEN visible (assuming it is an X-window).\n\
Also raises the screen so that nothing obscures it.")
  (screen)
     Lisp_Object screen;
{
  CHECK_SCREEN (screen, 0);

  if (XSCREEN (screen)->display.nothing == 0)
    error ("Cannot make a dead screen visible");

  if (XSCREEN (screen)->output_method == output_x_window)
    x_make_screen_visible (XSCREEN (screen));

  return screen;
}

DEFUN ("make-screen-invisible", Fmake_screen_invisible, Smake_screen_invisible,
       1, 1, 0,
  "Make the screen SCREEN invisible (assuming it is an X-window).\n\
This unconditionally removes the window from the screen; if what you\n\
want to do is iconify the screen (if the window manager uses icons)\n\
then you should call `iconify-screen' instead.")
  (screen)
     Lisp_Object screen;
{
  CHECK_SCREEN (screen, 0);

  if (XSCREEN (screen)->output_method == output_x_window)
    x_make_screen_invisible (XSCREEN (screen));

  return Qnil;
}

DEFUN ("iconify-screen", Ficonify_screen, Siconify_screen,
       1, 1, 0,
 "Make the screen SCREEN into an icon, if the window manager supports icons.")
  (screen)
     Lisp_Object screen;
{
  CHECK_SCREEN (screen, 0);

  if (XSCREEN (screen)->display.nothing == 0)
    error ("Cannot iconify a dead screen");

  if (XSCREEN (screen)->output_method == output_x_window)
      x_iconify_screen (XSCREEN (screen));

  return Qnil;
}

DEFUN ("deiconify-screen", Fdeiconify_screen, Sdeiconify_screen,
       1, 1, 0,
  "Open (de-iconify) the iconified screen SCREEN.")
  (screen)
     Lisp_Object screen;
{
  CHECK_SCREEN (screen, 0);

  if (XSCREEN (screen)->display.nothing == 0)
    error ("Cannot deiconify a dead screen");

  if (XSCREEN (screen)->output_method == output_x_window)
      x_make_screen_visible (XSCREEN (screen));

  return screen;
}

DEFUN ("screen-visible-p", Fscreen_visible_p, Sscreen_visible_p,
       1, 1, 0,
       "Return t if SCREEN is now \"visible\" (actually in use for display).\n\
A screen that is not \"visible\" is not updated and, if it works through\n\
a window system, it may not show at all.\n\
Return the symbol `icon' if window is visible only as an icon.")
  (screen)
     Lisp_Object screen;
{
  CHECK_SCREEN (screen, 0);

  if (XSCREEN (screen)->visible)
    return Qt;
  /* ## It's hard to tell when a screen is iconified; we have to look at
     ## the window manager properties to tell; we should do that in this
     ## function, and not when the unmap event is recieved, to avoid a
     ## race condition with the WM.
   */
  if (XSCREEN (screen)->iconified)
    return intern ("icon");
  return Qnil;
}

DEFUN ("visible-screen-list", Fvisible_screen_list, Svisible_screen_list,
       0, 0, 0,
       "Return a list of all screens now \"visible\" (being updated).")
  ()
{
  Lisp_Object tail, screen;
  struct screen *s;
  Lisp_Object value;

  value = Qnil;
  for (tail = Vscreen_list; CONSP (tail); tail = XCONS (tail)->cdr)
    {
      screen = XCONS (tail)->car;
      if (XTYPE (screen) != Lisp_Screen)
	continue;
      s = XSCREEN (screen);
      if (s->visible)
	value = Fcons (screen, value);
    }
  return value;
}

Lisp_Object
get_screen_param (screen, prop)
     register struct screen *screen;
     Lisp_Object prop;
{
  register Lisp_Object tem;

  tem = Fassq (prop, screen->param_alist);
  if (EQ (tem, Qnil))
    return tem;
  return Fcdr (tem);
}

void
store_in_alist (alistptr, propname, val)
     Lisp_Object *alistptr, val;
     char *propname;
{
  register Lisp_Object tem;
  register Lisp_Object prop;

  prop = intern (propname);
  tem = Fassq (prop, *alistptr);
  if (EQ (tem, Qnil))
    *alistptr = Fcons (Fcons (prop, val), *alistptr);
  else
    Fsetcdr (tem, val);
}

void
store_screen_param (s, prop, val)
     struct screen *s;
     Lisp_Object prop, val;
{
  register Lisp_Object tem;

  tem = Fassq (prop, s->param_alist);
  if (EQ (tem, Qnil))
    s->param_alist = Fcons (Fcons (prop, val), s->param_alist);
  else
    Fsetcdr (tem, val);
}

DEFUN ("screen-parameters", Fscreen_parameters, Sscreen_parameters, 0, 1, 0,
  "Return the parameters-alist of screen SCREEN.\n\
It is a list of elements of the form (PARM . VALUE), where PARM is a symbol.\n\
The meaningful PARMs depend on the kind of screen.")
  (screen)
     Lisp_Object screen;
{
  Lisp_Object alist;
  struct screen* s;
  get_screen (s, screen);

  if (s->display.nothing == 0)
    return Qnil;

  alist = Fcopy_alist (s->param_alist);
  store_in_alist (&alist, "name", s->name);
  store_in_alist (&alist, "height", make_number (s->height));
  store_in_alist (&alist, "width", make_number (s->width));
  store_in_alist (&alist, "modeline", (s->wants_modeline ? Qt : Qnil));
  store_in_alist (&alist, "minibuffer", (s->has_minibuffer ? Qt : Qnil));
  store_in_alist (&alist, "unsplittable", (s->no_split ? Qt : Qnil));

  if (s->output_method == output_x_window)
    x_report_screen_params (s, &alist);
  return alist;
}

DEFUN ("modify-screen-parameters", Fmodify_screen_parameters, 
       Smodify_screen_parameters, 2, 2, 0,
  "Modify the parameters of screen SCREEN according to ALIST.\n\
ALIST is an alist of parameters to change and their new values.\n\
Each element of ALIST has the form (PARM . VALUE), where PARM is a symbol.\n\
The meaningful PARMs depend on the kind of screen; undefined PARMs are ignored.")
  (screen, alist)
     Lisp_Object screen, alist;
{
  register struct screen *s;
  register Lisp_Object tail, elt, prop, val;
  int length;
  get_screen (s, screen);

  if (s->display.nothing == 0)
    error ("Cannot modify parameters of a dead screen");
  
  if (s->output_method != output_x_window)
    error ("Can only modified parameters of an X window screen");

  x_set_screen_values (s, alist);

  return Qnil;
}

DEFUN ("screen-height", Fscreen_height, Sscreen_height, 0, 1, 0,
  "Return number of lines available for display on SCREEN.")
  (screen)
{
  struct screen* s;
  get_screen (s, screen);
  return make_number (SCREEN_HEIGHT (s));
}

DEFUN ("screen-width", Fscreen_width, Sscreen_width, 0, 1, 0,
  "Return number of columns available for display on SCREEN.")
  (screen)
{
  struct screen* s;
  get_screen (s, screen);
  return make_number (SCREEN_WIDTH (s));
}

DEFUN ("screen-name", Fscreen_name, Sscreen_name, 0, 1, 0,
       "Returns the name of SCREEN (defaulting to the selected screen).\n\
This is not the same as the `title' of the screen.")
     (screen)
{
  struct screen* s;
  get_screen (s, screen);
  return s->name;
}

DEFUN ("screen-totally-visible-p", Fscreen_totally_visible_p,
       Sscreen_totally_visible_p, 0, 1, 0,
  "Return T if screen is not obscured by any other X windows, NIL otherwise")
  (screen)
{
  struct screen* s;
  get_screen (s, screen);
  return s->display.x->totally_visible_p ? Qt : Qnil;
}


static void
internal_set_screen_size (s, cols, rows, pretend)
     struct screen* s;
     int cols;
     int rows;
     int pretend;
{
#ifdef HAVE_X_WINDOWS
  if (pretend)
    change_screen_size (s, rows, cols);
  else
    x_set_window_size (s, cols, rows);
#else
  change_screen_size (s, rows, cols);
#endif
}

DEFUN ("set-screen-height", Fset_screen_height, Sset_screen_height, 2, 3, 0,
  "Specify that the screen SCREEN has LINES lines.\n\
Optional third arg non-nil means that redisplay should use LINES lines\n\
but that the idea of the actual height of the screen should not be changed.")
  (screen, rows, pretend)
     Lisp_Object rows, pretend;
{
  struct screen *s;

  if (NILP (screen))
    XSET (screen, Lisp_Screen, selected_screen);
  else
    CHECK_SCREEN (screen, 0);
  s = XSCREEN (screen);
  CHECK_NUMBER (rows, 1);
  
  if (s->display.nothing == 0)
    error ("Cannot set the height of a dead screen");

  internal_set_screen_size (s, SCREEN_WIDTH (s), XINT (rows), !NILP (pretend));
  return screen;
}

DEFUN ("set-screen-width", Fset_screen_width, Sset_screen_width, 2, 3, 0,
  "Specify that the screen SCREEN has COLS columns.\n\
Optional third arg non-nil means that redisplay should use COLS columns\n\
but that the idea of the actual width of the screen should not be changed.")
  (screen, cols, pretend)
     Lisp_Object cols, pretend;
{
  struct screen *s;

  if (NILP (screen))
    XSET (screen, Lisp_Screen, selected_screen);
  else
    CHECK_SCREEN (screen, 0);
  s = XSCREEN (screen);
  CHECK_NUMBER (cols, 1);

  if (s->display.nothing == 0)
    error ("Cannot set the width of a dead screen");

  internal_set_screen_size (s, XINT (cols), SCREEN_HEIGHT (s),
			    !NILP (pretend));
  return screen;
}

DEFUN ("set-screen-size", Fset_screen_size, 
       Sset_screen_size, 3, 4, 0,
  "Sets size of SCREEN to COLS by ROWS.\n\
Optional fourth arg non-nil means that redisplay should use COLS by ROWS\n\
but that the idea oft eh acrual size of the screen should not be changed.")
  (screen, cols, rows, pretend)
     Lisp_Object screen, cols, rows, pretend;
{
  struct screen *s;

  if (NILP (screen))
    XSET (screen, Lisp_Screen, selected_screen);
  else
    CHECK_SCREEN (screen, 0);
  s = XSCREEN (screen);
  CHECK_NUMBER (cols, 1);
  CHECK_NUMBER (rows, 2);

  if (s->display.nothing == 0)
    error ("Cannot set the size of a dead screen");

  internal_set_screen_size (s, XINT (cols), XINT (rows), !NILP (pretend));
  return screen;
}

DEFUN ("set-screen-position", Fset_screen_position, 
       Sset_screen_position, 3, 3, 0,
  "Sets position of SCREEN in pixels to XOFFSET by YOFFSET.")
  (screen, xoffset, yoffset)
     Lisp_Object screen, xoffset, yoffset;
{
  register struct screen *s;
  int mask;

  CHECK_SCREEN (screen, 0);
  CHECK_NUMBER (xoffset, 1);
  CHECK_NUMBER (yoffset, 2);
  s = XSCREEN (screen);

  if (s->display.nothing == 0)
    error ("Cannot set the position of a dead screen");

  if (s->output_method == output_x_window)
    x_set_offset (s, XINT (xoffset), XINT (yoffset));

  return Qt;
}


DEFUN ("raise-screen", Fraise_screen, Sraise_screen, 1, 1, 0,
  "Make the window of SCREEN be the uppermost one (fully visible).")
  (screen)
     Lisp_Object screen;
{
  register struct screen *s;
  CHECK_SCREEN (screen, 0);
  s = XSCREEN (screen);
  if (s->display.nothing == 0)
    error ("Cannot raise a dead screen");
  if (s->output_method == output_x_window)
    x_raise_screen (s, 1);
  return Qt;
}

DEFUN ("lower-screen", Flower_screen, Slower_screen, 1, 1, 0,
  "Make the window of SCREEN be the bottommost one.")
  (screen)
     Lisp_Object screen;
{
  register struct screen *s;
  CHECK_SCREEN (screen, 0);
  s = XSCREEN (screen);
  if (s->display.nothing == 0)
    return Qt;
  if (s->output_method == output_x_window)
    x_lower_screen (s);
  return Qt;
}

int
coordinates_in_window (w, x, y)
     register struct window *w;
     register int *x, *y;
{
  register int left = XINT (w->left);
  register int width = XINT (w->width);
  register int screen_height = XINT ((XSCREEN (w->screen)->height));
  register int window_height = XINT (w->height);
  register int top = XFASTINT (w->top);

  if (*x < left || *x >= left + width
      ||
      *y == screen_height || *y < top || *y > top + window_height - 1)
    return 0;

  if (*y == top + window_height - 1
      && window_height > 1)	/* 1 line => minibuffer */
    /* in modeline */
    return -1;

  *x -= left;
  *y -= top;
  return 1;
}

DEFUN ("coordinates-in-window-p", Fcoordinates_in_window_p,
  Scoordinates_in_window_p, 2, 2, 0,
  "Return non-nil if COORDINATES are in WINDOW.\n\
COORDINATES is a cons of the form (X Y), X and Y being screen-relative.\n\
If COORDINATES are in the text portion of WINDOW, the coordinates relative\n\
to the window are returned.  If they are in the modeline of WINDOW, t is\n\
returned.")
  (coordinates, window)
     register Lisp_Object coordinates, window;
{
  int x, y;

  CHECK_WINDOW (window, 0);
  CHECK_CONS (coordinates, 1);
  x = XINT (Fcar (coordinates));
  y = XINT (Fcar (Fcdr (coordinates)));

  switch (coordinates_in_window (XWINDOW (window), &x, &y))
    {
    case -1:			/* In modeline of window. */
      return Qt;

    case 0:			/* NOT in window at all. */
      return Qnil;

    case 1:			/* In text part of window. */
      return Fcons (x, Fcons (y, Qnil));

    default:
      abort ();
    }
}

#ifdef HAVE_X_WINDOWS
/* Symbols indicating different parts of the window.  See xfns.c */

extern Lisp_Object text_part_sym;
extern Lisp_Object modeline_part_sym;
#endif

Lisp_Object
window_from_coordinates (screen, x, y, part)
     SCREEN_PTR screen;
     int x, y;
     Lisp_Object *part;
{
  register Lisp_Object tem, first;

  first = SCREEN_SELECTED_WINDOW (screen);
  tem = next_screen_window (screen, first, Qt);

#if 0
/*  This loses if there's a global minibuffer, as Fnext_window
    would force going to another screen.  Likewise below.  So use
    next_screen_window.  */

  tem = Fnext_window (first, Qt, Qnil);
#endif

  while (1)
    {
      int found = coordinates_in_window (XWINDOW (tem), &x, &y);

      if (found)
	{
#ifdef HAVE_X_WINDOWS
	  *part = (found == -1 ? modeline_part_sym : text_part_sym);
#endif
	  return tem;
	}

      if (EQ (tem, first))
	return Qnil;
      
      tem = next_screen_window (screen, tem, Qt);
    }
}

DEFUN ("locate-window-from-coordinates",
       Flocate_window_from_coordinates, Slocate_window_from_coordinates,
       2, 2, 0,
  "Return window on SCREEN containing position COORDINATES.\n\
COORDINATES is a list (SCREEN-X SCREEN-Y) of coordinates\n\
which are relative to 0,0 at the top left corner of the screen.")
  (screen, coordinates)
      Lisp_Object screen, coordinates;
{
  Lisp_Object part;

  CHECK_SCREEN (screen, 0);
  CHECK_CONS (coordinates, 1);

  return window_from_coordinates (XSCREEN (screen),
				  XINT (Fcar (coordinates)),
				  XINT (Fcar (Fcdr (coordinates))),
				  &part);
}


SCREEN_PTR
choose_minibuf_screen ()
{
  if (SCREENP (Vglobal_minibuffer_screen))
    return XSCREEN (Vglobal_minibuffer_screen);

  /* For lowest-level minibuf, put it on currently selected screen
     if screen has a minibuffer.  */
  if (minibuf_level == 0
      && selected_screen != 0
      && !EQ (minibuf_window, selected_screen->minibuffer_window)
      && !EQ (Qnil, selected_screen->minibuffer_window))
    {
      Fset_window_buffer (selected_screen->minibuffer_window,
			  XWINDOW (minibuf_window)->buffer);
      minibuf_window = selected_screen->minibuffer_window;
    }

  return selected_screen;
}

syms_of_screen ()
{
  DEFVAR_LISP ("terminal-screen", &Vterminal_screen,
    "The initial screen-object, which represents Emacs's stdout.");
  /* This is set in init_window_once. */

  DEFVAR_LISP ("global-minibuffer-screen", &Vglobal_minibuffer_screen,
 "A screen-object holding the default minibuffer for minibufferless screens.\n\
When you create a minibufferless screen, by default it will use the\n\
minibuffer of this screen.  It is up to you to create a suitable screen\n\
and store it in this variable.");
  Vglobal_minibuffer_screen = Qnil;

  /* This is set in init_window_once. */
  staticpro (&Vscreen_list);

  /* defvarred in screen.el so that they can be buffer-local */
  Qselect_screen_hook = intern ("select-screen-hook");
  staticpro (&Qselect_screen_hook);
  Qdeselect_screen_hook = intern ("deselect-screen-hook");
  staticpro (&Qdeselect_screen_hook);

  defsubr (&Sscreenp);
  defsubr (&Sselect_screen);
  defsubr (&Sselected_screen);
  defsubr (&Swindow_screen);
  defsubr (&Sscreen_root_window);
  defsubr (&Sscreen_selected_window);
  defsubr (&Sscreen_list);
  defsubr (&Snext_screen);
  defsubr (&Sdelete_screen);
  defsubr (&Sread_mouse_position);
  defsubr (&Sset_mouse_position);
#if 0
  defsubr (&Sscreen_configuration);
  defsubr (&Srestore_screen_configuration);
#endif
  defsubr (&Smake_screen_visible);
  defsubr (&Smake_screen_invisible);
  defsubr (&Sraise_screen);
  defsubr (&Slower_screen);
  defsubr (&Siconify_screen);
  defsubr (&Sdeiconify_screen);
  defsubr (&Sscreen_visible_p);
  defsubr (&Svisible_screen_list);
  defsubr (&Sscreen_parameters);
  defsubr (&Smodify_screen_parameters);
  defsubr (&Sscreen_height);
  defsubr (&Sscreen_width);
  defsubr (&Sscreen_name);
  defsubr (&Sscreen_totally_visible_p);
  defsubr (&Sset_screen_height);
  defsubr (&Sset_screen_width);
  defsubr (&Sset_screen_size);
  defsubr (&Sset_screen_position);
  defsubr (&Scoordinates_in_window_p);
  defsubr (&Slocate_window_from_coordinates);
}
