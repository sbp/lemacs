/* Generic screen functions.
   Copyright (C) 1989, 1992, 1993, 1994 Free Software Foundation.

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
#include "intl.h"
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
#include "buffer.h"             /* for Vbuffer_alist */
#include <stdio.h>              /* for sprintf */

Lisp_Object Vscreen_list;
Lisp_Object Vterminal_screen;
Lisp_Object Vglobal_minibuffer_screen;
int allow_deletion_of_last_visible_screen;

Lisp_Object Vcreate_screen_hook, Qcreate_screen_hook;
Lisp_Object Vdelete_screen_hook, Qdelete_screen_hook;
Lisp_Object Vmouse_enter_screen_hook, Qmouse_enter_screen_hook;
Lisp_Object Vmouse_leave_screen_hook, Qmouse_leave_screen_hook;
Lisp_Object Vmap_screen_hook, Qmap_screen_hook;
Lisp_Object Vunmap_screen_hook, Qunmap_screen_hook;
Lisp_Object Vmouse_motion_handler;
Lisp_Object Vsynchronize_minibuffers;

Lisp_Object Qicon;
Lisp_Object Qscreenp, Qlive_screen_p;
Lisp_Object Qdelete_screen;

Lisp_Object Qselect_screen_hook, Qdeselect_screen_hook;


static Lisp_Object mark_screen (Lisp_Object, void (*) (Lisp_Object));
static void print_screen (Lisp_Object, Lisp_Object, int);
static int sizeof_screen (void *h) { return (sizeof (struct screen)); }
DEFINE_LRECORD_IMPLEMENTATION (lrecord_screen,
                               mark_screen, print_screen, 
                               0, sizeof_screen, 0);

static Lisp_Object
mark_screen (Lisp_Object obj, void (*markobj) (Lisp_Object))
{
  struct screen *s = XSCREEN (obj);
  ((markobj) (s->name));
  ((markobj) (s->root_window));
  ((markobj) (s->selected_window));
  ((markobj) (s->minibuffer_window));
  ((markobj) (s->buffer_alist));
  ((markobj) (s->param_alist));
  ((markobj) (s->menubar_data));

  /* The scrollbars reference some Lisp_Objects.  Let them take care
     of it cause we don't know (or care) what they are referencing. */
  mark_scrollbar (s->scrollbar_instances, markobj);

#ifdef HAVE_X_WINDOWS
  if (SCREEN_IS_X (s))
    {
      ((markobj) (s->display.x->icon_pixmap));
      ((markobj) (s->display.x->icon_pixmap_mask));
# ifdef ENERGIZE
      ((markobj) (s->display.x->current_psheet_buffer));
      ((markobj) (s->display.x->desired_psheet_buffer));
# endif
    }
#endif

  return (s->face_data);
}

static void
print_screen (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  struct screen *scr = XSCREEN (obj);
  char buf[10];
  
  if (print_readably)
    error (GETTEXT ("printing unreadable object #<screen %s 0x%x>"),
           XSTRING (scr->name)->data, scr->header.uid);

  write_string_1 ("#<", -1, printcharfun);
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
  sprintf (buf, " 0x%x>", scr->header.uid);
  write_string_1 (buf, -1, printcharfun);
}


struct screen *
make_screen (int mini_p)
{
  Lisp_Object screen;
  Lisp_Object root_window;
  Lisp_Object mini_window;
  struct screen *s = alloc_lcrecord (sizeof (struct screen), lrecord_screen);

  XSETR (screen, Lisp_Screen, s);

  s->insert_line_cost = 0;
  s->delete_line_cost = 0;
  s->insert_n_lines_cost = 0;
  s->delete_n_lines_cost = 0;
  s->cursor_x = 0;
  s->cursor_y = 0;
  s->cursor_erased = 1;
  s->phys_cursor_x = 0;
  s->phys_cursor_y = 0;
  s->height = 0;
  s->width = 0;
  s->new_height = 0;
  s->new_width = 0;
  s->name = Qnil;
  s->root_window = Qnil;
  s->selected_window = Qnil;
  s->minibuffer_window = Qnil;
  s->buffer_alist = Fcopy_sequence (Vbuffer_alist);
  s->param_alist = Qnil;
  s->menubar_data = Qnil;
  s->scrollbar_instances = 0;
  s->scrollbar_count = 0;
  s->output_method = output_dead_screen;
  s->display.x = 0;
  s->display_preempted = 0;
  s->visible = 0;
  s->iconified = 0;
  s->garbaged = 0;
  s->has_minibuffer = mini_p;
  s->being_deleted = 0;
  s->wants_modeline = 1;
  s->no_split = 0;
  s->message_buf = 0;
  s->faces = 0;
  s->n_faces = 0;
  s->face_data = Qnil;
  s->size_change_pending = 0;

  /* init_screen_faces will do more, but this is needed before then */
  ensure_face_ready (s, 3);

  root_window = make_window ();

  s->cur_line = s->new_cur_line = XWINDOW(root_window)->lines;
  s->cur_char = s->new_cur_char = XWINDOW(root_window)->lines->body;
  s->cur_w = s->new_cur_w = XWINDOW(root_window);

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

  XWINDOW (root_window)->pixwidth = 10;
  XWINDOW (root_window)->pixheight = ((mini_p) ? 9 : 10);

  /* The size of the minibuffer window is now set in x_create_screen
     in xfns.c
     if (mini_p)
     {
     }
   */

  /* Choose a buffer for the screen's root window.  */
  XWINDOW (root_window)->buffer = Qt;
  {
    Lisp_Object buf;

    buf = Fcurrent_buffer ();
    /* If buf is a 'hidden' buffer (i.e. one whose name starts with
       a space), try to find another one.  */
    if (XSTRING (Fbuffer_name (buf))->data[0] == ' ')
      buf = Fother_buffer (buf, Qnil);
    Fset_window_buffer (root_window, buf);
  }

  if (mini_p)
    {
      XWINDOW (mini_window)->buffer = Qt;
      Fset_window_buffer (mini_window, Vminibuffer_zero);
    }

  s->root_window = root_window;
  s->selected_window = root_window;

#if 0
  /* Make sure this window seems more recently used than
     a newly-created, never-selected window.  */
  XWINDOW (s->selected_window)->use_time = make_number (++window_select_count);
#endif
  return s;
}

#if 0 /* Unused */
/* Make a screen using a separate minibuffer window on another screen.
   MINI_WINDOW is the minibuffer window to use.  nil means use the
   default (the global minibuffer).  */

static struct screen *
make_screen_without_minibuffer (mini_window)
     register Lisp_Object mini_window;
{
  register struct screen *s;

  /* Choose the minibuffer window to use.  */
  if (NILP (mini_window))
    {
      if (!SCREENP (Vglobal_minibuffer_screen))
	error (GETTEXT ("global-minibuffer-screen must be set when creating minibufferless screens"));
      if (!SCREEN_LIVE_P (XSCREEN (Vglobal_minibuffer_screen)))
	error (GETTEXT ("global-minibuffer-screen must be a live screen"));
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
  Fset_window_buffer (mini_window, Vminibuffer_zero);
  return s;
}
#endif

#if 0 /* Unused */
/* Make a screen containing only a minibuffer window.  */

static struct screen *
make_minibuffer_screen ()
{
  /* First make a screen containing just a root window, no minibuffer.  */

  register struct screen *s = make_screen (0);
  register Lisp_Object mini_window;
  register Lisp_Object screen;

  XSETR (screen, Lisp_Screen, s);

  s->no_split = 1;
  s->wants_modeline = 0;
  s->has_minibuffer = 1;

  /* Now label the root window as also being the minibuffer.
     Avoid infinite looping on the window chain by marking next pointer
     as nil. */

  mini_window = s->minibuffer_window = s->root_window;
  XWINDOW (mini_window)->mini_p = Qt;
  XWINDOW (mini_window)->next = Qnil;
  XWINDOW (mini_window)->prev = mini_window;
  XWINDOW (mini_window)->screen = screen;

  /* Put the proper buffer in that window.  */

  Fset_window_buffer (mini_window, Vminibuffer_zero);
  return s;
}
#endif

/* Construct a screen that refers to the terminal (stdin and stdout).  */

struct screen *
make_terminal_screen ()
{
  register struct screen *s;

  Vscreen_list = Qnil;
  s = make_screen (1);
  s->name = build_string (GETTEXT ("terminal"));
  s->visible = 1;
  s->output_method = output_termcap; /* Nonzero means screen isn't deleted.  */
  XSETR (Vterminal_screen, Lisp_Screen, s);

  Vscreen_list = Fcons (Vterminal_screen, Vscreen_list);

  return s;
}


static Lisp_Object
get_screen (Lisp_Object screen, int dead_ok)
{
  if (NILP (screen))
    {
      XSETR (screen, Lisp_Screen, selected_screen);
      return (screen);
    }
  else
    {
      CHECK_SCREEN (screen, 0);
      if (! dead_ok && ! SCREEN_LIVE_P (XSCREEN (screen)))
	screen = wrong_type_argument (Qlive_screen_p, screen);
      return (screen);
    }
}


/*
 * window size changes are held up during critical regions. afterwards,
 * we want to deal with any delayed changes
 */
void
hold_window_change ()
{
  in_display = 1;
}

void
unhold_window_change ()
{
  Lisp_Object rest;
  struct screen *s;

  in_display = 0;
  for (rest = Vscreen_list ; !NILP (rest) ; rest = XCONS(rest)->cdr)
    {
      if (!SCREENP (XCONS(rest)->car))
	abort();
      s = XSCREEN (XCONS (rest)->car);
      if (s->size_change_pending)
	change_screen_size(s, s->new_height, s->new_width, 1);
    }
}



DEFUN ("screenp", Fscreenp, Sscreenp, 1, 1, 0,
  "Return non-nil if OBJECT is a screen.\n\
Value is t for a termcap screen (a character-only terminal),\n\
`x' for an Emacs screen that is really an X window.")
  (screen)
     Lisp_Object screen;
{
  if (!SCREENP (screen))
    return Qnil;

  switch (XSCREEN (screen)->output_method)
    {
    case output_termcap:
      return (Qt);
    case output_x_window:
      return (Qx);
    case output_dead_screen:
      return (Qt);              /* What? */
    default:
      abort ();
    }
}

DEFUN ("live-screen-p", Flive_screen_p, Slive_screen_p, 1, 1, 0,
  "Return non-nil if OBJECT is a screen which has not been deleted.\n\
Value is nil if OBJECT is not a live screen.  If object is a live\n\
screen, the return value indicates what sort of output device it is\n\
displayed on.  Value is t for a termcap screen (a character-only\n\
terminal), `x' for an Emacs screen being displayed in an X window.")
  (object)
     Lisp_Object object;
{
  if (SCREENP (object) && SCREEN_LIVE_P (XSCREEN (object)))
    return (Fscreenp (object));
  else
    return (Qnil);
}


/* select_screen_internal changes selected_screen, hollows the cursor on
   the previously selected sceen, and fils the cursor on the newly selected
   screen.

   select_screen (and Fselect_screen) do that, and tell the WM to focus on
   this screen, with the timestamp of the last user event.  Therefore, 
   select_screen should only be called as a result of user action.

   select_screen_internal is called as a result of a FocusIn event.  It
   would be wrong to call select_screen from there, because then we would 
   use a user timestamp instead of the timestamp of the FocusIn event.
   (This is further complicated by the fact that FocusIn doesn't have a
   timestamp in it at all.)
 */

void
select_screen_internal (s)
     struct screen* s;
{
  if (!SCREEN_LIVE_P (s))
    error (GETTEXT ("Cannot select a dead screen"));
  
  if (selected_screen == s)
    return;

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

void
select_screen (s)
     struct screen* s;
{
  select_screen_internal (s);
#ifdef HAVE_X_WINDOWS
  if (SCREEN_IS_X (s))
    x_focus_screen (s);
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
  XSETR (tem, Lisp_Screen, selected_screen);
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
  if (NILP (screen)) screen = Fselected_screen ();
  CHECK_SCREEN (screen, 0);
  return (SCREEN_ROOT_WINDOW (XSCREEN (screen)));
}

DEFUN ("screen-selected-window", Fscreen_selected_window,
       Sscreen_selected_window, 0, 1, 0,
  "Return the selected window of screen SCREEN.")
  (screen)
     Lisp_Object screen;
{
  if (NILP (screen)) screen = Fselected_screen ();
  CHECK_SCREEN (screen, 0);
  return (SCREEN_SELECTED_WINDOW (XSCREEN (screen)));
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
next_screen (Lisp_Object screen, int mini_screen, int visible_only_p)
{
  int found_self_p = 0;
  Lisp_Object target, tail, oldest;

  /* Iterate over the list exactly once.
     The first time we come across a visible screen, remember it.
     When we find `screen' in the list, set a flag.
     The next time we find a visible screen, return it.
     If we reach the end of the list, return the first visible screen
      that we found (which is the "next" screen if we wrap around.)
   */
  if (NILP (Vscreen_list)) abort ();
  oldest = Qnil;
  for (tail = Vscreen_list; !NILP (tail); tail = XCONS (tail)->cdr)
    {
      target = XCONS (tail)->car;

      if (EQ (target, screen))
	found_self_p = 1;

      else if ((!visible_only_p || XSCREEN (target)->visible) &&
	       (mini_screen || !EQ (target, Vglobal_minibuffer_screen)))
	{
	  if (found_self_p)
	    return target;
	  else if (NILP (oldest))
	    oldest = target;
	}
    }
  if (NILP (oldest))
    return screen;
  else
    return oldest;
}

Lisp_Object
prev_screen (Lisp_Object screen, int mini_screen, int visible_only_p)
{
  Lisp_Object target, prev, tail;

  /* Iterate over the list exactly once.
     Each time we come across a visible screen, remember it.
     When we find `screen' in the list, return the last-seen visible screen,
      or keep going if we haven't seen one yet.
     Keep going until the end of the list, and return the last visible screen
      in the list.  In this case, `screen' was the first visible screen in
      the list, so it's "previous" screen is the last visible screen in the
      list.
   */
  if (NILP (Vscreen_list)) abort ();
  prev = Qnil;
  for (tail = Vscreen_list; !NILP (tail); tail = XCONS (tail)->cdr)
    {
      target = XCONS (tail)->car;

      if (EQ (target, screen))
	{
	  if (!NILP (prev))
	    return prev;
	}
      else if ((!visible_only_p || XSCREEN (target)->visible) &&
	       (mini_screen || !EQ (target, Vglobal_minibuffer_screen)))
	{
	  prev = target;
	}
    }
  if (NILP (prev))
    return screen;
  else
    return prev;
}

DEFUN ("next-screen", Fnext_screen, Snext_screen,
       0, 3, 0,
       "Return the next screen in the screen list after SCREEN.\n\
If MINISCREEN is non-nil, include the global-minibuffer-screen if it\n\
has its own screen.\n\
If VISIBLE-ONLY-P is non-nil, then cycle through the visible screens,\n\
instead of all screens.")
  (screen, miniscreen, visible_only_p)
  Lisp_Object screen, miniscreen, visible_only_p;
{
  screen = get_screen (screen, 1);

  return (next_screen (screen, !NILP (miniscreen), !NILP (visible_only_p)));
}

DEFUN ("previous-screen", Fprevious_screen, Sprevious_screen,
       0, 3, 0,
       "Return the previous screen in the screen list after SCREEN.\n\
If MINISCREEN is non-nil, include the global-minibuffer-screen if it\n\
has its own screen.\n\
If VISIBLE-ONLY-P is non-nil, then cycle through the visible screens,\n\
instead of all screens.")
  (screen, miniscreen, visible_only_p)
Lisp_Object screen, miniscreen, visible_only_p;
{
  screen = get_screen (screen, 1);
  return prev_screen (screen, !NILP (miniscreen), !NILP (visible_only_p));
}
#endif /* MULTI_SCREEN */


extern void free_screen_menubar (struct screen *s);
extern void free_screen_scrollbars (struct screen *s);
extern void free_display_structs (Lisp_Object win);
extern void free_line_insertion_deletion_costs (struct screen *s);

DEFUN ("delete-screen", Fdelete_screen, Sdelete_screen,
       0, 1, "",
       "Delete SCREEN, permanently eliminating it from use.\n\
Default is current screen.\n\
A screen may not be deleted if its minibuffer is used by other screens.")
  (screen)
     Lisp_Object screen;
{
  struct screen *s;

  screen = get_screen (screen, 1);
  s = XSCREEN (screen);

  if (! SCREEN_LIVE_P (s))
    return (Qnil);

#if 0
  /* Does this screen have a minibuffer, and is it the surrogate
     minibuffer for any other screen?  */
  if (SCREEN_HAS_MINIBUF_P (XSCREEN (screen)))
    {
      Lisp_Object screens;

      for (screens = Vscreen_list;
	   CONSP (screens);
	   screens = XCONS (screens)->cdr)
	{
	  Lisp_Object this = XCONS (screens)->car;

	  if (! EQ (this, screen)
	      && EQ (screen,
		     (WINDOW_SCREEN
		      (XWINDOW
		       (SCREEN_MINIBUF_WINDOW
			(XSCREEN (this)))))))
	    error (GETTEXT("Attempt to delete a surrogate minibuffer screen"));
	}
    }
#endif

  /* Don't allow deleted screen to remain selected.  */
  if (s == selected_screen)
    {
      Lisp_Object next;

      next = next_screen (screen, 0, 1);
      if (EQ (next, screen))
	{
	  Lisp_Object invisible_next = next_screen (screen, 0, 0);
	  if (EQ (screen, invisible_next))
	    error (GETTEXT ("Attempt to delete the only screen"));
	  else if (!allow_deletion_of_last_visible_screen)
	    error (GETTEXT ("Attempt to delete the only visible screen"));
	  else
	    next = invisible_next;
	}
      Fselect_screen (next);
    }

  /* Don't allow the global minibuffer screen to be deleted */
  if (s == XSCREEN (Vglobal_minibuffer_screen))
    error (GETTEXT ("Attempt to delete the global minibuffer screen"));

  /* Don't allow minibuf_window to remain on a deleted screen.  */
  if (EQ (s->minibuffer_window, minibuf_window))
    {
      Fset_window_buffer (selected_screen->minibuffer_window,
			  XWINDOW (minibuf_window)->buffer);
      minibuf_window = selected_screen->minibuffer_window;
    }

  /* Before here, we haven't made any dangerous changed (just checked for
     error conditions.)  Now run the delete-screen-hook.  Remember that
     user code there could do any number of dangerous things, including
     signalling an error.
   */

  run_hook_with_args (Qdelete_screen_hook, 1, screen);

  /* After this point, no errors must be allowed to occur.
     Remove the screen now from the list.  This way, any events generated
     on this screen by the maneuvers below will disperse themselves.
   */
  Vscreen_list = Fdelq (screen, Vscreen_list);


#ifdef HAVE_X_WINDOWS
  if (SCREEN_IS_X (s))
    {
      /* #### all that UnrealizeWidget crap should be in EmacsShell.c called
	 from x_destroy_window instead of here. */
      x_destroy_window (s);
      s->display.x = 0;
    }
#endif

  s->output_method = output_dead_screen;
  s->visible = 0;

  free_screen_menubar (s);
  free_screen_scrollbars (s);
  free_display_structs (s->root_window);
/*  free_line_insertion_deletion_costs (s); */

#if 0
  /* If we've deleted the last_nonminibuf_screen, then try to find
     another one.  */
  if (s == last_nonminibuf_screen)
    {
      Lisp_Object screens;

      last_nonminibuf_screen = 0;

      for (screens = Vscreen_list;
	   CONSP (screens);
	   screens = XCONS (screens)->cdr)
	{
	  s = XSCREEN (XCONS (screens)->car);
	  if (!SCREEN_MINIBUF_ONLY_P (s))
	    {
	      last_nonminibuf_screen = s;
	      break;
	    }
	}
    }

  /* If we've deleted Vdefault_minibuffer_screen, try to find another
     one.  Prefer minibuffer-only screens, but also notice screens
     with other windows.  */
  if (EQ (screen, Vdefault_minibuffer_screen))
    {
      Lisp_Object screens;

      /* The last screen we saw with a minibuffer, minibuffer-only or not.  */
      Lisp_Object screen_with_minibuf = Qnil;

      for (screens = Vscreen_list;
	   CONSP (screens);
	   screens = XCONS (screens)->cdr)
	{
	  Lisp_Object this = XCONS (screens)->car;

	  if (!SCREENP (this))
	    abort ();
	  s = XSCREEN (this);

	  if (SCREEN_HAS_MINIBUF_P (s))
	    {
	      screen_with_minibuf = this;
	      if (SCREEN_MINIBUF_ONLY_P (s))
		break;
	    }
	}

      /* We know that there must be some screen with a minibuffer out
	 there.  If this were not true, all of the screens present
	 would have to be minibufferless, which implies that at some
	 point their minibuffer screens must have been deleted, but
	 that is prohibited at the top; you can't delete surrogate
	 minibuffer screens.  */
      if (NILP (screen_with_minibuf))
	abort ();

      Vdefault_minibuffer_screen = screen_with_minibuf;
    }
#endif

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
  if (SCREEN_IS_X (XSCREEN (screen)))
    x_read_mouse_position (XSCREEN (screen), x, y);
#endif
}

DEFUN ("read-mouse-position", Fread_mouse_position, Sread_mouse_position, 1, 1, 0,
  "Return a cons (x . y) which represents the position of the mouse.")
  (screen)
     Lisp_Object screen;
{
  int x, y;
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
  CHECK_FIXNUM (x, 2);
  CHECK_FIXNUM (y, 1);

#ifdef HAVE_X_WINDOWS
  if (SCREEN_IS_X (XSCREEN (screen)))
    /* Warping the mouse will cause  enternotify and focus events. */
    x_set_mouse_position (XSCREEN (screen), XINT (x), XINT (y));
#endif
  return Qnil;
}

#if 0
/* ??? Can this be replaced with a Lisp function? */

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
  
  c = make_vector (3, Qnil);
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
      error (GETTEXT ("Wrong size vector passed to restore-screen-configuration"));
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
  CHECK_LIVE_SCREEN (screen, 0);

  if (SCREEN_IS_X (XSCREEN (screen)))
    x_make_screen_visible (XSCREEN (screen));

  return screen;
}

DEFUN ("make-screen-invisible", Fmake_screen_invisible, Smake_screen_invisible,
       1, 1, 0,
  "Unconditionally removes screen from the display (assuming it is an X-window).\n\
If what you want to do is iconify the screen (if the window manager uses\n\
icons) then you should call `iconify-screen' instead.")
  (screen)
     Lisp_Object screen;
{
  CHECK_LIVE_SCREEN (screen, 0);

  if (SCREEN_IS_X (XSCREEN (screen)))
    x_make_screen_invisible (XSCREEN (screen));

  return Qnil;
}

DEFUN ("iconify-screen", Ficonify_screen, Siconify_screen,
       1, 1, 0,
 "Make the screen SCREEN into an icon, if the window manager supports icons.")
  (screen)
     Lisp_Object screen;
{
  CHECK_LIVE_SCREEN (screen, 0);

  if (SCREEN_IS_X (XSCREEN (screen)))
    x_iconify_screen (XSCREEN (screen));

  return Qnil;
}

DEFUN ("deiconify-screen", Fdeiconify_screen, Sdeiconify_screen,
       1, 1, 0,
  "Open (de-iconify) the iconified screen SCREEN.")
  (screen)
     Lisp_Object screen;
{
  CHECK_LIVE_SCREEN (screen, 0);

  if (SCREEN_IS_X (XSCREEN (screen)))
    x_make_screen_visible (XSCREEN (screen));

  return screen;
}

DEFUN ("screen-visible-p", Fscreen_visible_p, Sscreen_visible_p,
       1, 1, 0,
       "Return t if SCREEN is now \"visible\" (actually in use for display).\n\
A screen that is not visible is not updated, and, if it works through a\n\
window system, may not show at all.")
  (screen)
     Lisp_Object screen;
{
  CHECK_LIVE_SCREEN (screen, 0);
  return (XSCREEN (screen)->visible ? Qt : Qnil);
}

extern int x_screen_iconified_p (Lisp_Object);

DEFUN ("screen-iconified-p", Fscreen_iconified_p, Sscreen_iconified_p,
       1, 1, 0,
       "Return t if SCREEN is iconified.\n\
Not all window managers use icons; some merely unmap the window, so this\n\
function is not the inverse of `screen-visible-p'.  It is possible for a\n\
screen to not be visible and not be iconified either.  However, if the\n\
screen is iconified, it will not be visible.")
  (screen)
     Lisp_Object screen;
{
  CHECK_LIVE_SCREEN (screen, 0);
  if (XSCREEN (screen)->visible)
    return Qnil;
#ifdef HAVE_X_WINDOWS
  if (SCREEN_IS_X (XSCREEN (screen)))
    XSCREEN (screen)->iconified = x_screen_iconified_p (screen);
#endif
  return (XSCREEN (screen)->iconified ? Qt : Qnil);
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
      if (!SCREENP (screen))
	continue;
      s = XSCREEN (screen);
      if (s->visible)
	value = Fcons (screen, value);
    }
  return value;
}

Lisp_Object
get_screen_param (struct screen *screen, Lisp_Object prop)
{
  register Lisp_Object tem;

  tem = Fassq (prop, screen->param_alist);
  if (EQ (tem, Qnil))
    return tem;
  return Fcdr (tem);
}

void
store_in_alist (Lisp_Object *alistptr, CONST char *propname, Lisp_Object val)
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
store_screen_param (struct screen *s, Lisp_Object prop, Lisp_Object val)
{
  register Lisp_Object tem;

  tem = Fassq (prop, s->param_alist);
  if (EQ (tem, Qnil))
    s->param_alist = Fcons (Fcons (prop, val), s->param_alist);
  else
    Fsetcdr (tem, val);

#if 0
  if (EQ (prop, Qminibuffer)
      && WINDOWP (val))
    {
      if (! MINI_WINDOW_P (XWINDOW (val)))
	error (GETTEXT ("Surrogate minibuffer windows must be minibuffer windows."));

      if (SCREEN_HAS_MINIBUF_P (s) || SCREEN_MINIBUF_ONLY_P (s))
	error (GETTEXT ("Can't change the surrogate minibuffer of a screen with its own minibuffer."));

      /* Install the chosen minibuffer window, with proper buffer.  */
      s->minibuffer_window = val;
    }
#endif
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
  screen = get_screen (screen, 1);
  s = XSCREEN (screen);

  if (!SCREEN_LIVE_P (XSCREEN (screen)))
    return Qnil;

  alist = Fcopy_alist (s->param_alist);
  store_in_alist (&alist, "name", s->name);
  store_in_alist (&alist, "height", make_number (SCREEN_HEIGHT (s)));
  store_in_alist (&alist, "width", make_number (SCREEN_WIDTH (s)));
  store_in_alist (&alist, "modeline", (SCREEN_WANTS_MODELINE_P (s)
                                       ? Qt : Qnil));
  store_in_alist (&alist, "minibuffer", (SCREEN_HAS_MINIBUF_P (s) 
#if 0
                                         ? Qnone
                                         : (SCREEN_MINIBUF_ONLY_P (s) ? Qonly
                                            : SCREEN_MINIBUF_WINDOW (s))
#else
                                         ? SCREEN_MINIBUF_WINDOW (s) : Qnil
#endif
                                         ));
  store_in_alist (&alist, "unsplittable", (SCREEN_NO_SPLIT_P (s)
                                           ? Qt : Qnil));

  if (SCREEN_IS_X (s))
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
  screen = get_screen (screen, 0);
  s = XSCREEN (screen);

  if (! SCREEN_IS_X (s))
    error (GETTEXT ("Can only modified parameters of an X window screen"));

  x_set_screen_values (s, alist);

  return Qnil;
}

/* >>> These could be written in Lisp
 * >>>  (defun screen-height (s) (cdr (assq 'height (screen-parameters s))))
 * But they shouldn't be; screen-parameters should be replaced with
 * a screen plist or something.
 */

DEFUN ("screen-height", Fscreen_height, Sscreen_height, 0, 1, 0,
  "Return number of lines available for display on SCREEN.")
  (screen)
     Lisp_Object screen;
{
  screen = get_screen (screen, 0);
  return (make_number (SCREEN_HEIGHT (XSCREEN (screen))));
}

DEFUN ("screen-width", Fscreen_width, Sscreen_width, 0, 1, 0,
  "Return number of columns available for display on SCREEN.")
  (screen)
     Lisp_Object screen;
{
  screen = get_screen (screen, 0);
  return (make_number (SCREEN_WIDTH (XSCREEN (screen))));
}

DEFUN ("screen-name", Fscreen_name, Sscreen_name, 0, 1, 0,
       "Returns the name of SCREEN (defaulting to the selected screen).\n\
This is not the same as the `title' of the screen.")
     (screen)
     Lisp_Object screen;
{
  screen = get_screen (screen, 0);
  return (XSCREEN (screen)->name);
}

DEFUN ("screen-totally-visible-p", Fscreen_totally_visible_p,
       Sscreen_totally_visible_p, 0, 1, 0,
  "Return T if screen is not obscured by any other X windows, NIL otherwise")
  (screen)
     Lisp_Object screen;
{
  screen = get_screen (screen, 0);
  if (! SCREEN_IS_X (XSCREEN (screen)))
    return (Qt);                /* >>>> */
  else
    return ((XSCREEN (screen)->display.x->totally_visible_p) 
            ? Qt : Qnil);
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
    change_screen_size (s, rows, cols, pretend);
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
     Lisp_Object screen, rows, pretend;
{
  struct screen *s;

  screen = get_screen (screen, 0);
  s = XSCREEN (screen);
  CHECK_FIXNUM (rows, 1);
  
  internal_set_screen_size (s, SCREEN_WIDTH (s), XINT (rows), 
                            !NILP (pretend));
  return screen;
}

DEFUN ("set-screen-width", Fset_screen_width, Sset_screen_width, 2, 3, 0,
  "Specify that the screen SCREEN has COLS columns.\n\
Optional third arg non-nil means that redisplay should use COLS columns\n\
but that the idea of the actual width of the screen should not be changed.")
  (screen, cols, pretend)
     Lisp_Object screen, cols, pretend;
{
  struct screen *s;

  screen = get_screen (screen, 0);
  s = XSCREEN (screen);
  CHECK_FIXNUM (cols, 1);

  internal_set_screen_size (s, XINT (cols), SCREEN_HEIGHT (s),
			    !NILP (pretend));
  return screen;
}

DEFUN ("set-screen-size", Fset_screen_size, 
       Sset_screen_size, 3, 4, 0,
  "Sets size of SCREEN to COLS by ROWS.\n\
Optional fourth arg non-nil means that redisplay should use COLS by ROWS\n\
but that the idea of the actual size of the screen should not be changed.")
  (screen, cols, rows, pretend)
     Lisp_Object screen, cols, rows, pretend;
{
  struct screen *s;

  CHECK_LIVE_SCREEN (screen, 0);
  s = XSCREEN (screen);
  CHECK_FIXNUM (cols, 1);
  CHECK_FIXNUM (rows, 2);

  internal_set_screen_size (s, XINT (cols), XINT (rows), !NILP (pretend));
  return screen;
}

DEFUN ("set-screen-position", Fset_screen_position, 
       Sset_screen_position, 3, 3, 0,
  "Sets position of SCREEN in pixels to XOFFSET by YOFFSET.\n\
If XOFFSET or YOFFSET are negative, they are interpreted relative to\n\
the leftmost or bottommost position SCREEN could occupy without going\n\
off the screen.")
  (screen, xoffset, yoffset)
     Lisp_Object screen, xoffset, yoffset;
{
  register struct screen *s;

  CHECK_LIVE_SCREEN (screen, 0);
  CHECK_FIXNUM (xoffset, 1);
  CHECK_FIXNUM (yoffset, 2);
  s = XSCREEN (screen);

  if (SCREEN_IS_X (s))
    x_set_offset (s, XINT (xoffset), XINT (yoffset));

  return Qt;
}


DEFUN ("raise-screen", Fraise_screen, Sraise_screen, 1, 1, 0,
  "Make the window of SCREEN be the uppermost one (fully visible).")
  (screen)
     Lisp_Object screen;
{
  CHECK_LIVE_SCREEN (screen, 0);

  if (SCREEN_IS_X (XSCREEN (screen)))
    x_raise_screen (XSCREEN (screen), 1);
  return Qt;
}

DEFUN ("lower-screen", Flower_screen, Slower_screen, 1, 1, 0,
  "Make the window of SCREEN be the bottommost one.")
  (screen)
     Lisp_Object screen;
{
  CHECK_SCREEN (screen, 0);

  if (!SCREEN_LIVE_P (XSCREEN (screen)))
    return Qt;
  if (SCREEN_IS_X (XSCREEN (screen)))
    x_lower_screen (XSCREEN (screen));
  return Qt;
}

int
coordinates_in_window (w, x, y)
     register struct window *w;
     register int *x, *y;
{
  register int left = window_char_left (w);
  register int width = window_char_width (w);
  register int screen_height = XSCREEN (w->screen)->height;
  register int window_height = window_char_height (w);
  register int top = window_char_top (w);

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
      return list2 (make_number (x), make_number (y));

    default:
      abort ();
    }
}

Lisp_Object
window_from_coordinates (Lisp_Object screen, int x, int y, 
                         Lisp_Object *part)
{
  struct screen *f = XSCREEN (screen);
  Lisp_Object first = SCREEN_SELECTED_WINDOW (f);
  Lisp_Object tem;

  tem = next_screen_window (f, first, Qt);

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
	  if (part)
	    *part = (found == -1 ? modeline_part_sym : text_part_sym);
#endif
	  return tem;
	}

      if (EQ (tem, first))
	return Qnil;
      
      tem = next_screen_window (f, tem, Qt);
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

  screen = get_screen (screen, 0);
  CHECK_CONS (coordinates, 1);

  return window_from_coordinates (screen,
				  XINT (Fcar (coordinates)),
				  XINT (Fcar (Fcdr (coordinates))),
				  &part);
}


struct screen *
choose_minibuf_screen ()
{
  if (SCREENP (Vglobal_minibuffer_screen))
    return XSCREEN (Vglobal_minibuffer_screen);

  /* For lowest-level minibuf, put it on currently selected screen
     if screen has a minibuffer.  */
  if (minibuf_level == 0
      && selected_screen != 0
      && !EQ (minibuf_window, SCREEN_MINIBUF_WINDOW (selected_screen))
      && !EQ (Qnil, SCREEN_MINIBUF_WINDOW (selected_screen)))
    {
      Fset_window_buffer (selected_screen->minibuffer_window,
			  XWINDOW (minibuf_window)->buffer);
      minibuf_window = selected_screen->minibuffer_window;
    }

  return selected_screen;
}

void
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

  DEFVAR_BOOL ("allow-deletion-of-last-visible-screen",
	       &allow_deletion_of_last_visible_screen,
 "*If nil, the last visible screen may not be deleted by `delete-window'\n\
You can never delete the last screen, but setting this to t will allow you\n\
to delete the last non-iconified screen.");
  allow_deletion_of_last_visible_screen = 0;

  DEFVAR_LISP ("delete-screen-hook", &Vdelete_screen_hook,
    "Function or functions of one argument,\
 called with each to-be-deleted screen.");
  Vdelete_screen_hook = Qnil;
  defsymbol (&Qdelete_screen_hook, "delete-screen-hook");

  /* This is set in init_window_once. */
  staticpro (&Vscreen_list);

  /* defvarred in screen.el so that they can be buffer-local */
  defsymbol (&Qselect_screen_hook, "select-screen-hook");
  defsymbol (&Qdeselect_screen_hook, "deselect-screen-hook");

  defsymbol (&Qdelete_screen, "delete-screen");

  DEFVAR_LISP ("create-screen-hook", &Vcreate_screen_hook,
	       "Function or functions of one argument,\
 called with each newly-created screen.");
  Vcreate_screen_hook = Qnil;
  defsymbol (&Qcreate_screen_hook, "create-screen-hook");

  DEFVAR_LISP ("mouse-enter-screen-hook", &Vmouse_enter_screen_hook,
     "Function or functions to call when mouse enters a screen.  \
One arg, the screen.\n\
Be careful not to make assumptions about the window manger's focus model.\n\
In most cases, the `deselect-screen-hook' is more appropriate.");
  Vmouse_enter_screen_hook = Qnil;
  defsymbol (&Qmouse_enter_screen_hook, "mouse-enter-screen-hook");

  DEFVAR_LISP ("mouse-leave-screen-hook", &Vmouse_leave_screen_hook,
     "Function or functions to call when mouse leaves screen.  \
One arg, the screen.\n\
Be careful not to make assumptions about the window manger's focus model.\n\
In most cases, the `select-screen-hook' is more appropriate.");
  Vmouse_leave_screen_hook = Qnil;
  defsymbol (&Qmouse_leave_screen_hook, "mouse-leave-screen-hook");

  DEFVAR_LISP ("map-screen-hook", &Vmap_screen_hook,
    "Function or functions to call when screen is mapped.\n\
One arg, the screen.");
  Vmap_screen_hook = Qnil;
  defsymbol (&Qmap_screen_hook, "map-screen-hook");

  DEFVAR_LISP ("unmap-screen-hook", &Vunmap_screen_hook,
    "Function or functions to call when screen is unmapped.\n\
One arg, the screen.");
  Vunmap_screen_hook = Qnil;
  defsymbol (&Qunmap_screen_hook, "unmap-screen-hook");

  DEFVAR_LISP ("mouse-motion-handler", &Vmouse_motion_handler,
    "Handler for motion events.  One arg, the event.\n\
For most applications, you should use `mode-motion-hook' instead of this.");
  Vmouse_motion_handler = Qnil;

  DEFVAR_LISP ("synchronize-minibuffers",&Vsynchronize_minibuffers,
	       "Set to t if all minibuffer windows are to be synchronized");
  Vsynchronize_minibuffers = Qnil;

  defsymbol (&Qscreenp, "screenp");
  defsymbol (&Qlive_screen_p, "live-screen-p");
  defsymbol (&Qdelete_screen, "delete-screen");
  defsymbol (&Qicon, "icon");

  defsubr (&Sscreenp);
  defsubr (&Slive_screen_p);
  defsubr (&Sselect_screen);
  defsubr (&Sselected_screen);
  defsubr (&Swindow_screen);
  defsubr (&Sscreen_root_window);
  defsubr (&Sscreen_selected_window);
  defsubr (&Sscreen_list);
  defsubr (&Snext_screen);
  defsubr (&Sprevious_screen);
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
  defsubr (&Sscreen_iconified_p);
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
