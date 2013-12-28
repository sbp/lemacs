/* Window creation, deletion and examination for GNU Emacs.
   Does not include redisplay.
   Copyright (C) 1985, 1986, 1987, 1992 Free Software Foundation, Inc.

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
#include "lisp.h"
#include "buffer.h"
#include "screen.h"
#include "window.h"
#include "commands.h"
#include "indent.h"
#include "termchar.h"
#include "disptab.h"

Lisp_Object Qwindowp;

Lisp_Object Fnext_window (), Fdelete_window (), Fselect_window ();
Lisp_Object Fset_window_buffer (), Fsplit_window (), Frecenter ();

static void delete_all_subwindows ();
static struct window *decode_window();

#ifdef ENERGIZE
extern void energize_buffer_shown_hook ();
extern void energize_buffer_hidden_hook ();
extern void energize_window_selected_hook ();
extern void energize_window_deselected_hook ();
#endif

extern int zmacs_region_stays;


/* This is the window in which the terminal's cursor should
   be left when nothing is being done with it.  This must
   always be a leaf window, and its buffer is selected by
   the top level editing loop at the end of each command.

   This value is always the same as
    SCREEN_SELECTED_WINDOW (selected_screen).  */

Lisp_Object selected_window;

#ifndef MULTI_SCREEN

/* The root window for the screen.
   This is accessed via SCREEN_ROOT_WINDOW (selected_screen).  */
Lisp_Object root_window;

#endif

/* The minibuffer window of the selected screen.
   Note that you cannot test for minibufferness of an arbitrary window
   by comparing against this; but you can test for minibufferness of
   the selected window.  */
Lisp_Object minibuf_window;

/* Non-nil means it is the window for C-M-v to scroll
   when the minibuffer is selected.  */
Lisp_Object Vminibuf_scroll_window;

/* Non-nil means this is the buffer whose window C-M-v should scroll.  */
Lisp_Object Vother_window_scroll_buffer;

/* Non-nil means it's function to call to display temp buffers.  */
Lisp_Object Vtemp_buffer_show_function;

/* If a window gets smaller than either of these, it is removed. */
int window_min_height;
int window_min_width;

/* Nonzero implies Fdisplay_buffer should create windows. */
int pop_up_windows;

/* Function to call *instead of* Fdisplay_buffer.  */
Lisp_Object Vdisplay_buffer_function;

/* Function to call as the first thing that Fdisplay_buffer does (can be
   used to select an appropriate screen, for example.)  */
Lisp_Object Vpre_display_buffer_function;

/* Fdisplay_buffer always splits the largest window 
   if that window is more than this high.  */
int split_height_threshold;

/* Number of lines of continuity in scrolling by screenfuls.  */
int next_screen_context_lines;

/* Incremented for each window created.  */
static int sequence_number;

DEFUN ("windowp", Fwindowp, Swindowp, 1, 1, 0,
  "Returns t if OBJ is a window.")
  (obj)
     Lisp_Object obj;
{
  return WINDOWP (obj) ? Qt : Qnil;
}

Lisp_Object
make_window ()
{
  register Lisp_Object val;
  register struct window *p;

  /* Add sizeof (Lisp_Object) here because sizeof (struct Lisp_Vector)
     includes the first element.  */
  val = Fmake_vector (
    make_number ((sizeof (struct window) - sizeof (struct Lisp_Vector)
		  + sizeof (Lisp_Object))
		 / sizeof (Lisp_Object)),
    Qnil);
  XSETTYPE (val, Lisp_Window);
  p = XWINDOW (val);
  XFASTINT (p->sequence_number) = ++sequence_number;
  XFASTINT (p->left) = XFASTINT (p->top)
    = XFASTINT (p->height) = XFASTINT (p->width)
      = XFASTINT (p->hscroll) = 0;
  XFASTINT (p->last_point_x) = XFASTINT (p->last_point_y) = 0;
  p->start = Fmake_marker ();
  p->pointm = Fmake_marker ();
  XFASTINT (p->use_time) = 0;
  p->screen = Qnil;
  p->display_table = Qnil;
  p->dedicated = Qnil;
  return val;
}

DEFUN ("selected-window", Fselected_window, Sselected_window, 0, 0, 0,
  "Return the window that the cursor now appears in and commands apply to.")
  ()
{
  return selected_window;
}

DEFUN ("minibuffer-window", Fminibuffer_window, Sminibuffer_window, 0, 0, 0,
  "Return the window used now for minibuffers.")
  ()
{
#ifdef MULTI_SCREEN
  if (minibuf_level == 0
      && !EQ (minibuf_window, selected_screen->minibuffer_window)
      && !EQ (Qnil, selected_screen->minibuffer_window))
    {
      Fset_window_buffer (selected_screen->minibuffer_window,
			  XWINDOW (minibuf_window)->buffer);
      minibuf_window = selected_screen->minibuffer_window;
    }

  if (SCREENP (Vglobal_minibuffer_screen))
    minibuf_window = XSCREEN (Vglobal_minibuffer_screen)->minibuffer_window;
  else
    minibuf_window = selected_screen->minibuffer_window;

#endif /* MULTI_SCREEN */
  return minibuf_window;
}

DEFUN ("window-minibuffer-p", Fwindow_minibuffer_p, Swindow_minibuffer_p, 1, 1, 0,
  "Returns non-nil if WINDOW is a minibuffer window.")
  (window)
     Lisp_Object window;
{
  struct window *w = decode_window (window);
  return (MINI_WINDOW_P (w) ? Qt : Qnil);
}

DEFUN ("pos-visible-in-window-p", Fpos_visible_in_window_p,
  Spos_visible_in_window_p, 0, 2, 0,
  "Return t if position POS is currently on the screen in WINDOW.\n\
Returns nil if that position is scrolled vertically out of view.\n\
POS defaults to point; WINDOW, to the selected window.")
  (pos, window)
     Lisp_Object pos, window;
{
  register struct window *w;
  register int top;
  register int height;
  register int posint;
  register struct buffer *buf;
  struct position posval;

  if (NILP (pos))
    posint = point;
  else
    {
      CHECK_FIXNUM_COERCE_MARKER (pos, 0);
      posint = XINT (pos);
    }

  if (NILP (window))
    window = selected_window;
  else
    CHECK_WINDOW (window, 1);
  w = XWINDOW (window);
  top = marker_position (w->start);

  if (posint < top)
    return Qnil;

  height = XFASTINT (w->height) - ! MINI_WINDOW_P (w);

  buf = XBUFFER (w->buffer);
  if (XFASTINT (w->last_modified) >= BUF_MODIFF (buf)
      && XFASTINT (w->last_facechange) >= BUF_FACECHANGE (buf))
    {
      /* If screen is up to date,
	 use the info recorded about how much text fit on it. */
      if (posint < BUF_Z (buf) - XFASTINT (w->window_end_pos)
	  || (XFASTINT (w->window_end_vpos) < height))
	return Qt;
      return Qnil;
    }
  else
    {
      if (posint > BUF_Z (buf))
	return Qnil;

      /* If that info is not correct, calculate afresh */
      posval = *compute_motion (window,
				top, 0, 0, posint, height, 0,
			       XFASTINT (w->width) - 1
				- (XFASTINT (w->width) + XFASTINT (w->left)
				   != XSCREEN (w->screen)->width),
				XINT (w->hscroll), 0, 0, 0);

      return posval.vpos < height ? Qt : Qnil;
    }
}

static struct window *
decode_window (window)
     register Lisp_Object window;
{
  if (NILP (window))
    return XWINDOW (selected_window);

  CHECK_WINDOW (window, 0);
  return XWINDOW (window);
}

DEFUN ("window-buffer", Fwindow_buffer, Swindow_buffer, 0, 1, 0,
  "Return the buffer that WINDOW is displaying.")
  (window)
     Lisp_Object window;
{
  return decode_window (window)->buffer;
}

DEFUN ("window-height", Fwindow_height, Swindow_height, 0, 1, 0,
  "Return the number of lines in WINDOW (including its mode line).")
  (window)
     Lisp_Object window;
{
  return decode_window (window)->height;
}

DEFUN ("window-width", Fwindow_width, Swindow_width, 0, 1, 0,
  "Return the number of columns in WINDOW.")
  (window)
     Lisp_Object window;
{
  register struct window *w = decode_window (window);
  register int width = w->width;

  /* If this window does not end at the right margin,
     must deduct one column for the border */
  if ((width + w->left) == SCREEN_WIDTH (XSCREEN (WINDOW_SCREEN (w))))
    return width;
  return width - 1;
}

DEFUN ("window-hscroll", Fwindow_hscroll, Swindow_hscroll, 0, 1, 0,
  "Return the number of columns by which WINDOW is scrolled from left margin.")
  (window)
     Lisp_Object window;
{
  return decode_window (window)->hscroll;
}

DEFUN ("set-window-hscroll", Fset_window_hscroll, Sset_window_hscroll, 2, 2, 0,
  "Set number of columns WINDOW is scrolled from left margin to NCOL.\n\
NCOL should be zero or positive.")
  (window, ncol)
     register Lisp_Object window, ncol;
{
  register struct window *w;

  CHECK_FIXNUM (ncol, 1);
  if (XINT (ncol) < 0) XFASTINT (ncol) = 0;
  if (XFASTINT (ncol) >= (1 << (SHORTBITS - 1)))
    args_out_of_range (ncol, Qnil);
  w = decode_window (window);
  if (w->hscroll != ncol)
    clip_changed = 1;		/* Prevent redisplay shortcuts */
  w->hscroll = ncol;
  return ncol;
}

DEFUN ("window-edges", Fwindow_edges, Swindow_edges, 0, 1, 0,
  "Return a list of the edge coordinates of WINDOW.\n\
\(LEFT TOP RIGHT BOTTOM), all relative to 0, 0 at top left corner of screen.\n\
RIGHT is one more than the rightmost column used by WINDOW,\n\
and BOTTOM is one more than the bottommost row used by WINDOW\n\
 and its mode-line.")
  (window)
     Lisp_Object window;
{
  register struct window *w = decode_window (window);

  return Fcons (w->left, Fcons (w->top,
           Fcons (make_number (XFASTINT (w->left) + XFASTINT (w->width)),
		  Fcons (make_number (XFASTINT (w->top)
				      + XFASTINT (w->height)),
			 Qnil))));
}

DEFUN ("window-point", Fwindow_point, Swindow_point, 0, 1, 0,
  "Return current value of point in WINDOW.\n\
For a nonselected window, this is the value point would have\n\
if that window were selected.\n\
\n\
Note that, when WINDOW is the selected window and its buffer\n\
is also currently selected, the value returned is the same as (point).\n\
It would be more strictly correct to return the `top-level' value\n\
of point, outside of any save-excursion forms.\n\
But that is hard to define.")
  (window)
     Lisp_Object window;
{
  register struct window *w = decode_window (window);

  if (w == XWINDOW (selected_window)
      && current_buffer == XBUFFER (w->buffer))
    return Fpoint ();
  return Fmarker_position (w->pointm);
}

DEFUN ("window-start", Fwindow_start, Swindow_start, 0, 1, 0,
  "Return position at which display currently starts in WINDOW.")
  (window)
     Lisp_Object window;
{
  return Fmarker_position (decode_window (window)->start);
}

DEFUN ("window-end", Fwindow_end, Swindow_end, 0, 1, 0,
  "Return position at which display currently ends in WINDOW.")
  (window)
     Lisp_Object window;
{
  Lisp_Object value;
  struct window *w = decode_window (window);
  
  XSET (value, Lisp_Int,
	BUF_Z (current_buffer) - XFASTINT (w->window_end_pos));

  return value;
}

DEFUN ("set-window-point", Fset_window_point, Sset_window_point, 2, 2, 0,
  "Make point value in WINDOW be at position POS in WINDOW's buffer.")
  (window, pos)
     Lisp_Object window, pos;
{
  register struct window *w = decode_window (window);

  CHECK_FIXNUM_COERCE_MARKER (pos, 1);
  if (w == XWINDOW (selected_window))
    Fgoto_char (pos);
  else
    set_marker_restricted (w->pointm, pos, w->buffer);

  return pos;
}

DEFUN ("set-window-start", Fset_window_start, Sset_window_start, 2, 3, 0,
  "Make display in WINDOW start at position POS in WINDOW's buffer.\n\
Optional third arg NOFORCE non-nil inhibits next redisplay\n\
from overriding motion of point in order to display at this exact start.")
  (window, pos, noforce)
     Lisp_Object window, pos, noforce;
{
  register struct window *w = decode_window (window);

  CHECK_FIXNUM_COERCE_MARKER (pos, 1);
  set_marker_restricted (w->start, pos, w->buffer);
  /* this is not right, but much easier than doing what is right. */
  w->start_at_line_beg = Qnil;
  if (NILP (noforce))
    w->force_start = Qt;
  w->redo_mode_line = Qt;
  XFASTINT (w->last_modified) = 0;
  XFASTINT (w->last_facechange) = 0;
  /* Lucid fix: added this from Darryl Okahata <darrylo@HPNMXX.SR.HP.COM> */
  windows_or_buffers_changed++;
  return pos;
}

DEFUN ("window-dedicated-p", Fwindow_dedicated_p, Swindow_dedicated_p,
       1, 1, 0,
  "Return WINDOW's dedicated object, usually t or nil.\n\
See also `set-window-buffer-dedicated'.")
  (window)
     Lisp_Object window;
{
  return decode_window (window)->dedicated;
}

DEFUN ("set-window-buffer-dedicated", Fset_window_buffer_dedicated,
       Sset_window_buffer_dedicated, 2, 2, 0,
  "Make WINDOW display BUFFER and be dedicated to that buffer.\n\
Then Emacs will not automatically change which buffer appears in WINDOW.\n\
If BUFFER is nil, make WINDOW not be dedicated (but don't change which\n\
buffer appears in it currently).")
  (window, arg)
       Lisp_Object window, arg;
{
  register struct window *w = decode_window (window);

  if (NILP (arg))
    w->dedicated = Qnil;
  else
    {
      Fset_window_buffer (window, Fget_buffer_create (arg));
      w->dedicated = Qt;
    }

  return w->dedicated;
}

DEFUN ("window-display-table", Fwindow_display_table, Swindow_display_table,
       0, 1, 0,
  "Return the display-table that WINDOW is using.")
  (window)
     Lisp_Object window;
{
  return decode_window (window)->display_table;
}

/* Get the display table for use currently on window W.
   This is either W's display table or W's buffer's display table.
   Ignore the specified tables if they are not valid;
   if no valid table is specified, return 0.  */

struct Lisp_Vector *
window_display_table (w)
     struct window *w;
{
  Lisp_Object tem;
  tem = w->display_table;
  if (VECTORP (tem) && XVECTOR (tem)->size == DISP_TABLE_SIZE)
    return XVECTOR (tem);
  tem = XBUFFER (w->buffer)->display_table;
  if (VECTORP (tem) && XVECTOR (tem)->size == DISP_TABLE_SIZE)
    return XVECTOR (tem);
  tem = Vstandard_display_table;
  if (VECTORP (tem) && XVECTOR (tem)->size == DISP_TABLE_SIZE)
    return XVECTOR (tem);
  return 0;
}

DEFUN ("set-window-display-table",
       Fset_window_display_table, Sset_window_display_table, 2, 2, 0,
  "Set WINDOW's display-table to TABLE.")
  (window, table)
     register Lisp_Object window, table;
{
  register struct window *w;
  register Lisp_Object z;	/* Return value. */

  w = decode_window (window);
  w->display_table = table;
  return table;
}

/* Record info on buffer window w is displaying
   when it is about to cease to display that buffer.  */
static void
unshow_buffer (w)
     register struct window *w;
{
  Lisp_Object buf = w->buffer;

  if (XBUFFER (buf) != XMARKER (w->pointm)->buffer)
    abort ();

#ifdef ENERGIZE
    energize_buffer_hidden_hook (w);
#endif

  if (w == XWINDOW (selected_window)
      || ! EQ (buf, XWINDOW (selected_window)->buffer))
    /* Do this except when the selected window's buffer
       is being removed from some other window.  */
    XBUFFER (buf)->last_window_start = marker_position (w->start);

  /* Point in the selected window's buffer
     is actually stored in that buffer, and the window's pointm isn't used.
     So don't clobber point in that buffer.  */
  if (! EQ (buf, XWINDOW (selected_window)->buffer))
    {
      struct buffer *b = XBUFFER (buf);
      int p = marker_position (w->pointm);
      if (p < BUF_BEGV (b))
	p = BUF_BEGV (b);
      else if (p > BUF_ZV (b))
	p = BUF_ZV (b);
      SET_BUF_PT (b, p);
    }
}

/* Put replacement into the window structure in place of old. */
static void
replace_window (old, replacement)
     Lisp_Object old, replacement;
{
  register Lisp_Object tem;
  register struct window *o = XWINDOW (old), *p = XWINDOW (replacement);

  /* If OLD is its screen's root_window, then replacement is the new
     root_window for that screen.  */

  if (old == XSCREEN (o->screen)->root_window)
    XSCREEN (o->screen)->root_window = replacement;

  p->left = o->left;
  p->top = o->top;
  p->width = o->width;
  p->height = o->height;

  p->next = tem = o->next;
  if (!NILP (tem))
    XWINDOW (tem)->prev = replacement;

  p->prev = tem = o->prev;
  if (!NILP (tem))
    XWINDOW (tem)->next = replacement;

  p->parent = tem = o->parent;
  if (!NILP (tem))
    {
      if (EQ (XWINDOW (tem)->vchild, old))
	XWINDOW (tem)->vchild = replacement;
      if (EQ (XWINDOW (tem)->hchild, old))
	XWINDOW (tem)->hchild = replacement;
    }

/*** Here, if replacement is a vertical combination
and so is its new parent, we should make replacement's
children be children of that parent instead.  ***/
}

DEFUN ("delete-window", Fdelete_window, Sdelete_window, 0, 1, "",
  "Remove WINDOW from the display.  Default is selected window.\n\
If window is the only one on the screen, the screen is destroyed.")
  (window)
     register Lisp_Object window;
{
  register Lisp_Object tem, parent, sib;
  register struct window *p;
  register struct window *par;

  if (NILP (window))
    window = selected_window;
  else
    CHECK_WINDOW (window, 0);

  p = XWINDOW (window);
  parent = p->parent;
#ifdef MULTI_SCREEN
  if (ONLY_WINDOW_P (p))
    {
      windows_or_buffers_changed++;
      if (NILP (memq_no_quit (p->screen, Vscreen_list)))
	/* this screen isn't fully initialized yet; don't blow up. */
	return Qnil;
      if (EQ (p->screen, next_screen (p->screen, 0, 1)))
	error ((EQ (p->screen, next_screen (p->screen, 0, 0))) ?
	       "Attempt to delete the only window on the only screen" :
	       "Attempt to delete the only window on the only visible screen");
      Fdelete_screen (p->screen);
      return Qnil;
    }
#else
  if (NILP (parent))
    error ("Attempt to delete minibuffer or sole ordinary window");
#endif

  par = XWINDOW (parent);

  windows_or_buffers_changed++;

  if (EQ (window, selected_window))
    Fselect_window (Fnext_window (window, Qnil, Qnil));

  tem = p->buffer;
  /* tem is null for dummy parent windows
     (which have inferiors but not any contents themselves) */
  if (!NILP (tem))
    {
      unshow_buffer (p);
      unchain_marker (p->pointm);
      unchain_marker (p->start);
      /* This breaks set-window-configuration if windows in the saved
	 configuration get deleted and multiple screens are in use. */
      /*      p->buffer = Qnil; */
    }

  tem = p->next;
  if (!NILP (tem))
    XWINDOW (tem)->prev = p->prev;

  tem = p->prev;
  if (!NILP (tem))
    XWINDOW (tem)->next = p->next;

  if (EQ (window, par->hchild))
    par->hchild = p->next;
  if (EQ (window, par->vchild))
    par->vchild = p->next;

  /* Find one of our siblings to give our space to.  */
  sib = p->prev;
  if (NILP (sib))
    sib = p->next;

  /* Stretch that sibling.  */
  if (!NILP (par->vchild)) {
    set_window_height (sib,
		       XFASTINT (XWINDOW (sib)->height) + XFASTINT (p->height),
		       1);
    if (sib == p->next)    /* Lucid fix */
      XFASTINT (XWINDOW(sib)->top) -= XFASTINT (p->height);
  }
  if (!NILP (par->hchild)) {
    set_window_width (sib,
		      XFASTINT (XWINDOW (sib)->width) + XFASTINT (p->width),
		      1);
    if (sib == p->next)    /* Lucid fix */
      XFASTINT (XWINDOW(sib)->left) -= XFASTINT (p->width);
  }

  /* If parent now has only one child,
     put the child into the parent's place.  */

  tem = par->hchild;
  if (NILP (tem))
    tem = par->vchild;
  if (NILP (XWINDOW (tem)->next))
    replace_window (parent, tem);
  return Qnil;
}

#ifdef MULTI_SCREEN
Lisp_Object
next_screen_window (screen, window, mini)
     Lisp_Object window, mini;
     SCREEN_PTR screen;
{
  Lisp_Object tem;

  if (NILP (window))
    window = SCREEN_SELECTED_WINDOW (screen);

  do
    {
      while (tem = XWINDOW (window)->next, NILP (tem))
	if (tem = XWINDOW (window)->parent, !NILP (tem))
	  window = tem;
        else  /* window must be minibuffer window now */
	  {
	    tem = SCREEN_ROOT_WINDOW (screen);
	    break;
	  }

      window = tem;
      while (1)
	{
	  if (!NILP (XWINDOW (window)->hchild))
	    window = XWINDOW (window)->hchild;
	  else if (!NILP (XWINDOW (window)->vchild))
	    window = XWINDOW (window)->vchild;
	  else break;
	}
    }
  while (MINI_WINDOW_P (XWINDOW (window))
	 && !EQ (mini, Qt)
	 && (!NILP (mini) || !minibuf_level));

  return window;
}
#endif

extern Lisp_Object next_screen (), prev_screen ();

DEFUN ("next-window", Fnext_window, Snext_window, 0, 3, 0,
  "Return next window after WINDOW in canonical ordering of windows.\n\
Optional second arg MINIBUF t means count the minibuffer window\n\
even if not active.  If MINIBUF is neither t nor nil it means\n\
not to count the minibuffer even if it is active.\n\
Optional third arg ALL-SCREENS t means include all windows in all visible\n\
screens; otherwise cycle within the selected screen, with the exception that\n\
if a global minibuffer screen is in use, all screens are used.")
  (window, mini, all_screens)
     register Lisp_Object window, mini, all_screens;
{
  register Lisp_Object tem;

  if (NILP (window))
    window = selected_window;
  else
    CHECK_WINDOW (window, 0);

#ifdef MULTI_SCREEN
  if (EQ (mini, Qt)
      || (! NILP (mini) && minibuf_level))
    {
      if (SCREENP (Vglobal_minibuffer_screen))
	all_screens = Qt;
    }
#endif

  do
    {
      while (tem = XWINDOW (window)->next, NILP (tem))
	if (tem = XWINDOW (window)->parent, !NILP (tem))
	  window = tem;
        else  /* window must be minibuffer window now */
	  {
	    tem = WINDOW_SCREEN (XWINDOW (window));
#ifdef MULTI_SCREEN
	    if (! NILP (all_screens))
	      tem = next_screen (tem, (NILP (mini) ? 0 : 1), 1);
#endif
	    tem = SCREEN_ROOT_WINDOW (XSCREEN (tem));
	    break;
	  }

      window = tem;
      while (1)
	{
	  if (!NILP (XWINDOW (window)->hchild))
	    window = XWINDOW (window)->hchild;
	  else if (!NILP (XWINDOW (window)->vchild))
	    window = XWINDOW (window)->vchild;
	  else break;
	}
    }
  while (MINI_WINDOW_P (XWINDOW (window))
	 && !EQ (mini, Qt)
	 && (! NILP (mini) || !minibuf_level)
	 && !EQ (SCREEN_ROOT_WINDOW (XSCREEN (XWINDOW (window)->screen)),
		 SCREEN_MINIBUF_WINDOW (XSCREEN (XWINDOW (window)->screen))));

  return window;
}

DEFUN ("previous-window", Fprevious_window, Sprevious_window, 0, 3, 0,
  "Return previous window before WINDOW in canonical ordering of windows.\n\
Optional second arg MINIBUF t means count the minibuffer window\n\
even if not active.  If MINIBUF is neither t nor nil it means\n\
not to count the minibuffer even if it is active.\n\
Optional third arg ALL-SCREENS t means include all windows in all visible\n\
screens; otherwise cycle within the selected screen, with the exception\n\
that if a global minibuffer screen is in use, all screens are used.")
  (window, mini, all_screens)
     register Lisp_Object window, mini, all_screens;
{
  register Lisp_Object tem;
  register Lisp_Object screen_root, screen_mini;

  if (NILP (window))
    window = selected_window;
  else
    CHECK_WINDOW (window, 0);

#ifdef MULTI_SCREEN
  if (! NILP (mini))
    {
      if (SCREENP (Vglobal_minibuffer_screen))
	all_screens = Qt;
      else
	all_screens = Qnil;
    }
#endif

  do  /* at least once, and until not the minibuffer */
    {
      while (tem = XWINDOW (window)->prev, NILP (tem))
	if (tem = XWINDOW (window)->parent, !NILP (tem))
	  window = tem;
        else  /* window must be the root window now */
	  {
	    tem = WINDOW_SCREEN (XWINDOW (window));
#ifdef MULTI_SCREEN
	    if (! NILP (all_screens))
	      tem = prev_screen (tem, (NILP (mini) ? 0 : 1), 1);
#endif
#if 0
	    tem = XWINDOW (XSCREEN (tem)->minibuffer_window)->prev;
#endif
	    tem = SCREEN_ROOT_WINDOW (XSCREEN (tem));
	    break;
	  }

      window = tem;
      while (1)
	{
	  if (!NILP (XWINDOW (window)->hchild))
	    window = XWINDOW (window)->hchild;
	  else if (!NILP (XWINDOW (window)->vchild))
	    window = XWINDOW (window)->vchild;
	  else break;
	  while (tem = XWINDOW (window)->next, !NILP (tem))
	    window = tem;
	}

      screen_root = SCREEN_ROOT_WINDOW (XSCREEN (XWINDOW (window)->screen));
      screen_mini = SCREEN_MINIBUF_WINDOW (XSCREEN (XWINDOW (window)->screen));
    }
  while (MINI_WINDOW_P (XWINDOW (window))
	 && !EQ (mini, Qt)
	 && (!NILP (mini) || !minibuf_level)
	 && !EQ (screen_root, screen_mini));

  return window;
}

DEFUN ("other-window", Fother_window, Sother_window, 1, 2, "p",
  "Select the ARG'th different window on this screen.\n\
All windows on current screen are arranged in a cyclic order.\n\
This command selects the window ARG steps away in that order.\n\
A negative ARG moves in the opposite order.  If the optional second\n\
argument ALL_SCREENS is non-nil, cycle through all screens.")
  (n, all_screens)
     register Lisp_Object n, all_screens;
{
  register int i;
  register Lisp_Object w;

  CHECK_FIXNUM (n, 0);
  w = selected_window;
  i = XINT (n);

  while (i > 0)
    {
      w = Fnext_window (w, Qnil, all_screens);
      i--;
    }
  while (i < 0)
    {
      w = Fprevious_window (w, Qnil, all_screens);
      i++;
    }
  Fselect_window (w);
  return Qnil;
}

/* Look at all windows, performing an operation specified by TYPE
   with argument OBJ.

   If SCREENS is Qt, look at all screens, if Qnil, look at just the selected
   screen.  If SCREENS is a screen, just look at windows on that screen.
*/

enum window_loop
{
  WINDOW_LOOP_UNUSED,
  GET_BUFFER_WINDOW,		/* Arg is buffer */
  GET_LRU_WINDOW,		/* Arg is t for full-width windows only */
  DELETE_OTHER_WINDOWS,		/* Arg is window not to delete */
  DELETE_BUFFER_WINDOWS,	/* Arg is buffer */
  GET_LARGEST_WINDOW,
  UNSHOW_BUFFER			/* Arg is buffer */
};

static Lisp_Object
window_loop (type, obj, mini, this_screen)
     enum window_loop type;
     register Lisp_Object obj, this_screen;
     int mini;
{
  register Lisp_Object w, tem;
  Lisp_Object start_w;
  register struct window *p, *q;
  register Lisp_Object ret_w = Qnil;
  register SCREEN_PTR screen;
  int lose_lose = 0;

  if (NILP (this_screen) || EQ (this_screen, Qt))
    screen = selected_screen;
  else
    screen = XSCREEN (this_screen);

  if (WINDOWP (obj))
    w = obj;
  else
    w = SCREEN_SELECTED_WINDOW (screen);
  ret_w = Qnil;

  while (1)
    {

      /* Given the outstanding quality of the rest of this code, 
	 I feel no shame about putting this piece of shit in. */
      if (++lose_lose >= 500)
	return Qnil;

      p = XWINDOW (w);

      if (!MINI_WINDOW_P (XWINDOW (w)))
	switch (type)
	  {
	  case GET_BUFFER_WINDOW:
	    if (XBUFFER (p->buffer) == XBUFFER (obj))
	      return w;
	    break;

	  case GET_LRU_WINDOW:
	    /* t as arg means consider only full-width windows */
	    if (!NILP (obj) && XFASTINT (p->width) != screen->width)
	      break;

	    /* Ignore dedicated windows and minibuffers.  */
	    if (MINI_WINDOW_P (XWINDOW (w))
		|| !NILP (XWINDOW (w)->dedicated))
	      break;
	    if (NILP (ret_w) ||
		XFASTINT (XWINDOW (ret_w)->use_time) > XFASTINT (p->use_time))
	      ret_w = w;
	    break;

	  case DELETE_OTHER_WINDOWS:
	    if (p != XWINDOW (obj))
	      Fdelete_window (w);
	    break;

	  case DELETE_BUFFER_WINDOWS:
	    if (EQ (p->buffer, obj))
	      {
		if (NILP (p->parent))
		  {
		    tem = Fother_buffer (obj, Qnil);
		    if (NILP (tem))
		      tem = Fget_buffer_create (build_string ("*scratch*"));
		    Fset_window_buffer (w, tem);
		    Fset_buffer (p->buffer);
		  }
		else
		  Fdelete_window (w);
	      }
	    break;

	  case GET_LARGEST_WINDOW:
	    /* Ignore dedicated windows and minibuffers.  */
	    if (MINI_WINDOW_P (XWINDOW (w))
		|| !NILP (XWINDOW (w)->dedicated))
	      break;
	    q = XWINDOW (ret_w);
	    if (NILP (ret_w) ||
		(XFASTINT (p->height) * XFASTINT (p->width))
		>
		(XFASTINT (q->height) * XFASTINT (q->width)))
	      ret_w = w;
	    break;

	  case UNSHOW_BUFFER:
	    if (EQ (p->buffer, obj))
	      {
		tem = Fother_buffer (obj, Qnil);
		if (NILP (tem))
		  tem = Fget_buffer_create (build_string ("*scratch*"));
		Fset_window_buffer (w, tem);
		if (EQ (w, selected_window))
		  Fset_buffer (p->buffer);
	      }
	    break;

	  case WINDOW_LOOP_UNUSED:
	    abort ();
	  }

      if (EQ (this_screen, Qt))
	w = Fnext_window (w, mini ? Qt : Qnil, Qt);
      else
	w = next_screen_window (screen, w, mini ? Qt : Qnil);

      if (EQ (w, SCREEN_SELECTED_WINDOW (screen)))
	return ret_w;
    }
}

DEFUN ("get-lru-window", Fget_lru_window, Sget_lru_window, 0, 1, 0,
  "Return the window least recently selected or used for display.\n\
If optional argument SCREENS is non-nil, search only that screen.")
  (screens)
  Lisp_Object screens;
{
  register Lisp_Object w;
  /* First try for a window that is full-width */
  w = window_loop (GET_LRU_WINDOW, Qt, 0, screens);
  if (!NILP (w) && !EQ (w, selected_window))
    return w;
  /* If none of them, try the rest */
  return window_loop (GET_LRU_WINDOW, Qnil, 0, screens);
}

DEFUN ("get-largest-window", Fget_largest_window, Sget_largest_window, 0, 1, 0,
  "Return the window largest in area.  If optional argument SCREENS\n\
is non-nil, search only that screen.")
  (screens)
  Lisp_Object screens;
{
  return window_loop (GET_LARGEST_WINDOW, Qnil, 0, screens);
}

DEFUN ("get-buffer-window", Fget_buffer_window, Sget_buffer_window, 1, 2, 0,
  "Return a window currently displaying BUFFER, or nil if none.\n\
Only the selected screen is searched; if the optional second argument\n\
SCREEN is non-nil, then that screen is searched instead.  If SCREEN is t,\n\
then all screens are searched.")
  (buffer, screen)
     Lisp_Object buffer, screen;
{
  buffer = Fget_buffer (buffer);
  if (BUFFERP (buffer))
    return window_loop (GET_BUFFER_WINDOW, buffer, 1, screen);
  else return Qnil;
}

DEFUN ("delete-other-windows", Fdelete_other_windows, Sdelete_other_windows,
  0, 1, "",
  "Make WINDOW (or the selected window) fill its screen.")
  (window)
     Lisp_Object window;
{
  struct window *w;
  int opoint, owpoint;
  struct buffer *obuf, *owbuf;
  int top, start;
  register Lisp_Object s;

  if (NILP (window))
    window = selected_window;
  else
    CHECK_WINDOW (window, 0);

  w = XWINDOW (window);
  top = XFASTINT (w->top);

  obuf = current_buffer;
  opoint = point;

  owbuf = XBUFFER (w->buffer);
  owpoint = BUF_PT (owbuf);

  window_loop (DELETE_OTHER_WINDOWS, window, 0, WINDOW_SCREEN (w));

  Fset_buffer (w->buffer); /* owbuf */
  
  start = marker_position (w->start);

  if (start < BEGV)
    start = BEGV;
  else if (start > ZV)
    start = ZV;

  SET_PT (start);
  Frecenter (make_number (top));
  SET_PT (owpoint);

  internal_set_buffer (obuf);
  SET_PT (opoint);
  return Qnil;
}

DEFUN ("delete-windows-on", Fdelete_windows_on, Sdelete_windows_on,
  1, 1, "bDelete windows on (buffer): ",
  "Delete all windows showing BUFFER.")
  (buffer)
     Lisp_Object buffer;
{
  if (!NILP (buffer))
    {
      buffer = Fget_buffer (buffer);
      CHECK_BUFFER (buffer, 0);
      window_loop (DELETE_BUFFER_WINDOWS, buffer, 0, Qt);
    }
  return Qnil;
}

DEFUN ("replace-buffer-in-windows", Freplace_buffer_in_windows,
  Sreplace_buffer_in_windows,
  1, 1, "bReplace buffer in windows: ",
  "Replace BUFFER with some other buffer in all windows showing it.")
  (buffer)
     Lisp_Object buffer;
{
  if (!NILP (buffer))
    {
      buffer = Fget_buffer (buffer);
      CHECK_BUFFER (buffer, 0);
      window_loop (UNSHOW_BUFFER, buffer, 0, Qt);
    }
  return Qnil;
}

/* Set the height of WINDOW and all its inferiors.  */
/* Normally the window is deleted if it gets too small.
   nodelete nonzero means do not do this.
   (The caller should check later and do so if appropriate)  */

set_window_height (window, height, nodelete)
     Lisp_Object window;
     int height;
     int nodelete;
{
  register struct window *w = XWINDOW (window);
  register struct window *c;
  int oheight = XFASTINT (w->height);
  int top, pos, lastbot, opos, lastobot;
  Lisp_Object child;

  if (!nodelete
      && ! NILP (w->parent)
      && height < window_min_height)
    {
      Fdelete_window (window);
      return;
    }

  XFASTINT (w->last_modified) = 0;
  XFASTINT (w->last_facechange) = 0;
  windows_or_buffers_changed++;
  XFASTINT (w->height) = height;
  if (!NILP (w->hchild))
    {
      for (child = w->hchild; !NILP (child); child = XWINDOW (child)->next)
	{
	  XWINDOW (child)->top = w->top;
	  set_window_height (child, height, nodelete);
	}
    }
  else if (!NILP (w->vchild))
    {
      lastbot = top = XFASTINT (w->top);
      lastobot = 0;
      for (child = w->vchild; !NILP (child); child = c->next)
	{
	  c = XWINDOW (child);

	  opos = lastobot + XFASTINT (c->height);

	  XFASTINT (c->top) = lastbot;

	  pos = (((opos * height) << 1) + oheight) / (oheight << 1);

	  /* Avoid confusion: inhibit deletion of child if becomes too small */
	  set_window_height (child, pos + top - lastbot, 1);

	  /* Now advance child to next window,
	     and set lastbot if child was not just deleted.  */
	  lastbot = pos + top, lastobot = opos;
	}
      /* Now delete any children that became too small.  */
      if (!nodelete)
	for (child = w->vchild; !NILP (child); child = XWINDOW (child)->next)
	  {
	    set_window_height (child, XINT (XWINDOW (child)->height), 0);
	  }
    }
}

/* Recursively set width of WINDOW and its inferiors. */

set_window_width (window, width, nodelete)
     Lisp_Object window;
     int width;
     int nodelete;
{
  register struct window *w = XWINDOW (window);
  register struct window *c;
  int owidth = XFASTINT (w->width);
  int left, pos, lastright, opos, lastoright;
  Lisp_Object child;

  if (!nodelete && width < window_min_width)
    {
      Fdelete_window (window);
      return;
    }

  XFASTINT (w->last_modified) = 0;
  XFASTINT (w->last_facechange) = 0;
  windows_or_buffers_changed++;
  XFASTINT (w->width) = width;
  if (!NILP (w->vchild))
    {
      for (child = w->vchild; !NILP (child); child = XWINDOW (child)->next)
	{
	  XWINDOW (child)->left = w->left;
	  set_window_width (child, width, nodelete);
	}
    }
  else if (!NILP (w->hchild))
    {
      lastright = left = XFASTINT (w->left);
      lastoright = 0;
      for (child = w->hchild; !NILP (child); child = c->next)
	{
	  c = XWINDOW (child);

	  opos = lastoright + XFASTINT (c->width);

	  XFASTINT (c->left) = lastright;

	  pos = (((opos * width) << 1) + owidth) / (owidth << 1);

	  /* Inhibit deletion for becoming too small */
	  set_window_width (child, pos + left - lastright, 1);

	  /* Now advance child to next window,
	     and set lastright if child was not just deleted.  */
	  lastright = pos + left, lastoright = opos;
	}
      /* Delete children that became too small */
      if (!nodelete)
	for (child = w->hchild; !NILP (child); child = XWINDOW (child)->next)
	  {
	    set_window_width (child, XINT (XWINDOW (child)->width), 0);
	  }
    }
}

static int window_select_count;

DEFUN ("set-window-buffer", Fset_window_buffer, Sset_window_buffer, 2, 2, 0,
  "Make WINDOW display BUFFER as its contents.\n\
BUFFER can be a buffer or buffer name.")
  (window, buffer)
     register Lisp_Object window, buffer;
{
  register Lisp_Object tem;
  register struct window *w = decode_window (window);

  buffer = Fget_buffer (buffer);
  CHECK_BUFFER (buffer, 1);

  if (NILP (XBUFFER (buffer)->name))
    error ("Attempt to display deleted buffer");

  tem = w->buffer;
  if (NILP (tem))
    error ("Window is deleted");
  else if (! EQ (tem, Qt))	/* w->buffer is t when the window
				   is first being set up.  */
    {
      if (!NILP (w->dedicated) && !EQ (tem, buffer))
	/* Lucid fix */
	error ("Window is dedicated to buffer %s",
	       XSTRING(XBUFFER(tem)->name)->data);

      unshow_buffer (w);
    }

  w->buffer = buffer;
  Fset_marker (w->pointm,
	       make_number (BUF_PT (XBUFFER (buffer))),
	       buffer);
  set_marker_restricted (w->start,
			 make_number (XBUFFER (buffer)->last_window_start),
			 buffer);
  w->start_at_line_beg = Qnil;
  w->force_start = Qnil; /* Lucid fix */
  XFASTINT (w->last_modified) = 0;
  XFASTINT (w->last_facechange) = 0;
  windows_or_buffers_changed++;
  if (EQ (window, selected_window)) {
    Fset_buffer (buffer);
#ifdef ENERGIZE
    energize_buffer_shown_hook (w);
#endif
  }
  return Qnil;
}

DEFUN ("select-window", Fselect_window, Sselect_window, 1, 1, 0,
  "Select WINDOW.  Most editing will apply to WINDOW's buffer.\n\
The main editor command loop selects the buffer of the selected window\n\
before each command.")
  (window)
     register Lisp_Object window;
{
  register struct window *w;
  register struct window *ow = XWINDOW (selected_window);
  register int new_point;

  CHECK_WINDOW (window, 0);

  w = XWINDOW (window);

  if (NILP (w->buffer))
    error ("Trying to select deleted window or non-leaf window");

  XFASTINT (w->use_time) = ++window_select_count;
  if (EQ (window, selected_window))
    return window;

#ifdef ENERGIZE
  if (! MINI_WINDOW_P (w))
    energize_window_deselected_hook (XWINDOW(selected_window));
#endif

  Fset_marker (ow->pointm, make_number (BUF_PT (XBUFFER (ow->buffer))),
	       ow->buffer);

  selected_window = window;
#ifdef MULTI_SCREEN
  if (XSCREEN (WINDOW_SCREEN (w)) != selected_screen)
    {
      XSCREEN (WINDOW_SCREEN (w))->selected_window = window;
      Fselect_screen (WINDOW_SCREEN (w), Qnil);
    }
  else
    selected_screen->selected_window = window;
#endif

  record_buffer (w->buffer);
  Fset_buffer (w->buffer);

  /* Go to the point recorded in the window.
     This is important when the buffer is in more
     than one window.  It also matters when
     redisplay_window has altered point after scrolling,
     because it makes the change only in the window.  */
  new_point = (marker_position (w->pointm));

  if (new_point < BEGV)
    new_point = BEGV;
  else if (new_point > ZV)
    new_point = ZV;

  SET_PT (new_point);

  windows_or_buffers_changed++;

#ifdef ENERGIZE
  if (! MINI_WINDOW_P (w))
    energize_window_selected_hook (w);
#endif

  return window;
}

DEFUN ("display-buffer", Fdisplay_buffer, Sdisplay_buffer, 1, 3, 0,
       "Make BUFFER appear in some window on the current screen, but don't select it.\n\
BUFFER can be a buffer or a buffer name.  If BUFFER is shown already\n\
in some window in the current screen, just uses that one, unless the\n\
window is the selected window and NOTTHISWINDOW is non-nil.  If BUFFER\n\
has a dedicated screen, display on that screen instead of the current\n\
screen, unless OVERRIDESCREEN is non-nil.  If OVERRIDESCREEN is\n\
non-nil, display on that screen instead of the current screen (or the\n\
dedicated screen).  If pop-up-windows is non-nil, always use the\n\
current screen and create a new window regardless of whether the\n\
buffer has a dedicated screen, and regardless of whether\n\
OVERRIDESCREEN was specified.\n\
Returns the window displaying BUFFER.")
  (buffer, notthiswindow, overridescreen)
     register Lisp_Object buffer, notthiswindow, overridescreen;
{
  register Lisp_Object window;
  Lisp_Object this_screen;
  register Lisp_Object screens = Qnil;
#ifdef MULTI_SCREEN
  extern struct screen *active_screen;	         /* Declared in minibuffer.c */

  if (active_screen)
    XSET (screens, Lisp_Screen, active_screen);
  else
    XSET (screens, Lisp_Screen, selected_screen);

  this_screen = screens;
#endif

  buffer = Fget_buffer (buffer);
  CHECK_BUFFER (buffer, 0);

  /* If the buffer has a dedicated screen,
     that takes precedence over the current screen */
  if (!NILP (XBUFFER (buffer)->dedicated_screen))
    screens = XBUFFER (buffer)->dedicated_screen;

  /* if overridescreen is supplied, that takes precedence over everything */
  if (!NILP (overridescreen))
    screens = overridescreen;

  if (!NILP (Vpre_display_buffer_function))
    call2 (Vpre_display_buffer_function, buffer, notthiswindow);

  if (!NILP (Vdisplay_buffer_function))
    return call2 (Vdisplay_buffer_function, buffer, notthiswindow);

  if (NILP (notthiswindow)
      && XBUFFER (XWINDOW (selected_window)->buffer) == XBUFFER (buffer))
    return selected_window;

  window = Fget_buffer_window (buffer, Qnil);

  if (!NILP (window)
      && (NILP (notthiswindow) || !EQ (window, selected_window)))
    return window;

  if (pop_up_windows)
    {
      /* Don't try to create a window if would get an error */
      if (split_height_threshold < window_min_height << 1)
	split_height_threshold = window_min_height << 1;

      window = Fget_largest_window (this_screen);

      if (!NILP (window)
	  && window_height (window) >= split_height_threshold
	  &&
	  (XFASTINT (XWINDOW (window)->width)
	   == SCREEN_WIDTH (XSCREEN (WINDOW_SCREEN (XWINDOW (window))))))
	window = Fsplit_window (window, Qnil, Qnil);
      else
	{
	  window = Fget_lru_window (screens);
	  if ((EQ (window, selected_window)
	       || EQ (XWINDOW (window)->parent, Qnil))
	      && window_height (window) >= window_min_height << 1)
	    window = Fsplit_window (window, Qnil, Qnil);
	}
    }
  else
    window = Fget_lru_window (screens);

  Fset_window_buffer (window, buffer);
  return window;
}

void
temp_output_buffer_show (buf, same_screen)
     register Lisp_Object buf, same_screen;
{
  register struct buffer *old = current_buffer;
  register Lisp_Object window;
  register struct window *w;

  Fset_buffer (buf);
  XBUFFER (buf)->save_modified = MODIFF;
  BEGV = BEG;
  ZV = Z;
  SET_PT (BEG);
  clip_changed = 1;
  internal_set_buffer (old);

  if (!EQ (Vtemp_buffer_show_function, Qnil))
    call1 (Vtemp_buffer_show_function, buf);
  else
    {
      window = Fdisplay_buffer (buf, Qnil, same_screen);

#ifdef MULTI_SCREEN
      if (XSCREEN (XWINDOW (window)->screen) != selected_screen)
	Fmake_screen_visible (XWINDOW (window)->screen);
#endif /* MULTI_SCREEN */
      Vminibuf_scroll_window = window;
      w = XWINDOW (window);
      XFASTINT (w->hscroll) = 0;
      set_marker_restricted (w->start, make_number (1), buf);
      set_marker_restricted (w->pointm, make_number (1), buf);
    }
}

static void
make_dummy_parent (window)
     Lisp_Object window;
{
  register Lisp_Object old, new;
  register struct window *o, *p;

  old = window;
  XSETTYPE (old, Lisp_Vector);
  new = Fcopy_sequence (old);
  XSETTYPE (new, Lisp_Window);

  o = XWINDOW (old);
  p = XWINDOW (new);
  XFASTINT (p->sequence_number) = ++sequence_number;

  /* Put new into window structure in place of window */
  replace_window (window, new);

  o->next = Qnil;
  o->prev = Qnil;
  o->vchild = Qnil;
  o->hchild = Qnil;
  o->parent = new;

  p->start = Qnil;
  p->pointm = Qnil;
  p->buffer = Qnil;
}

DEFUN ("split-window", Fsplit_window, Ssplit_window, 0, 3, "",
  "Split WINDOW, putting SIZE lines in the first of the pair.\n\
WINDOW defaults to selected one and SIZE to half its size.\n\
If optional third arg HOR-FLAG is non-nil, split side by side\n\
and put SIZE columns in the first of the pair.")
  (window, chsize, horflag)
     Lisp_Object window, chsize, horflag;
{
  register Lisp_Object new;
  register struct window *o, *p;
  register int size;

  if (NILP (window))
    window = selected_window;
  else
    CHECK_WINDOW (window, 0);

  o = XWINDOW (window);

  if (NILP (chsize))
    {
      if (!NILP (horflag))
	/* Round odd size up, since this is for the left-hand window,
	   and it will lose a column for the separators.  */
	size = ((XFASTINT (o->width) + 1) & -2) >> 1;
      else
	size = XFASTINT (o->height) >> 1;
    }
  else
    {
      CHECK_FIXNUM (chsize, 1);
      size = XINT (chsize);
    }

  if (MINI_WINDOW_P (o))
    error ("Attempt to split minibuffer window");
  else if (SCREEN_NO_SPLIT_P (XSCREEN (WINDOW_SCREEN (o))))
    error ("Attempt to split unsplittable screen");

  /* Smaller values might permit a crash.  */
  if (window_min_width < 2)
    window_min_width = 2;
  if (window_min_height < 2)
    window_min_height = 2;

  if (NILP (horflag))
    {
      if (size < window_min_height
	  || size + window_min_height > XFASTINT (o->height))
	args_out_of_range_3 (window, chsize, horflag);
      if (NILP (o->parent)
	  || NILP (XWINDOW (o->parent)->vchild))
	{
	  make_dummy_parent (window);
	  new = o->parent;
	  XWINDOW (new)->vchild = window;
	}
    }
  else
    {
      if (size < window_min_width
	  || size + window_min_width > XFASTINT (o->width))
	args_out_of_range_3 (window, chsize, horflag);
      if (NILP (o->parent)
	  || NILP (XWINDOW (o->parent)->hchild))
	{
	  make_dummy_parent (window);
	  new = o->parent;
	  XWINDOW (new)->hchild = window;
	}
    }

  /* Now we know that window's parent is a vertical combination
     if we are dividing vertically, or a horizontal combination
     if we are making side-by-side windows */

  windows_or_buffers_changed++;
  new = make_window ();
  p = XWINDOW (new);

  p->screen = o->screen;
  p->next = o->next;
  if (!NILP (p->next))
    XWINDOW (p->next)->prev = new;
  p->prev = window;
  o->next = new;
  p->parent = o->parent;
  p->buffer = Qt;

  Fset_window_buffer (new, o->buffer);

  /* Apportion the available screen space among the two new windows */

  if (!NILP (horflag))
    {
      p->height = o->height;
      p->top = o->top;
      XFASTINT (p->width) = XFASTINT (o->width) - size;
      XFASTINT (o->width) = size;
      XFASTINT (p->left) = XFASTINT (o->left) + size;
    }
  else
    {
      p->left = o->left;
      p->width = o->width;
      XFASTINT (p->height) = XFASTINT (o->height) - size;
      XFASTINT (o->height) = size;
      XFASTINT (p->top) = XFASTINT (o->top) + size;
    }

  return new;
}

DEFUN ("enlarge-window", Fenlarge_window, Senlarge_window, 1, 2, "p",
  "Make current window ARG lines bigger.\n\
From program, optional second arg non-nil means grow sideways ARG columns.")
  (n, side)
     register Lisp_Object n, side;
{
  CHECK_FIXNUM (n, 0);
  change_window_height (XINT (n), !NILP (side));
  zmacs_region_stays = 1;
  return Qnil;
}

DEFUN ("shrink-window", Fshrink_window, Sshrink_window, 1, 2, "p",
  "Make current window ARG lines smaller.\n\
From program, optional second arg non-nil means shrink sideways ARG columns.")
  (n, side)
     register Lisp_Object n, side;
{
  CHECK_FIXNUM (n, 0);
  change_window_height (-XINT (n), !NILP (side));
  zmacs_region_stays = 1;
  return Qnil;
}

int
window_height (window)
     Lisp_Object window;
{
  register struct window *p = XWINDOW (window);
  return XFASTINT (p->height);
}

int
window_width (window)
     Lisp_Object window;
{
  register struct window *p = XWINDOW (window);
  return XFASTINT (p->width);
}

#define MINSIZE(w) \
  (widthflag ? window_min_width : window_min_height)

#define CURBEG(w) \
  *(widthflag ? (int *) &(w)->left : (int *) &(w)->top)

#define CURSIZE(w) \
  *(widthflag ? (int *) &(w)->width : (int *) &(w)->height)

/* Unlike set_window_height, this function
   also changes the heights of the siblings so as to
   keep everything consistent. */

change_window_height (delta, widthflag)
     register int delta;
     int widthflag;
{
  register Lisp_Object parent;
  Lisp_Object window;
  register struct window *p;
  int *sizep;
  int (*sizefun) () = widthflag ? window_width : window_height;
  register int (*setsizefun) () = (widthflag
				   ? set_window_width
				   : set_window_height);
  if (delta == 0)
    return 0;

  if (EQ (selected_window,
	  SCREEN_ROOT_WINDOW (XSCREEN (XWINDOW (selected_window)->screen))))
    error ("Won't change only window");

  /* Smaller values might permit a crash.  */
  if (window_min_width < 2)
    window_min_width = 2;
  if (window_min_height < 2)
    window_min_height = 2;

  window = selected_window;
  while (1)
    {
      p = XWINDOW (window);
      parent = p->parent;
      if (NILP (parent))
	{
	  if (widthflag)
	    error ("No other window to side of this one");
	  break;
	}
      if (widthflag ? !NILP (XWINDOW (parent)->hchild)
	  : !NILP (XWINDOW (parent)->vchild))
	break;
      window = parent;
    }

  sizep = &CURSIZE (p);

  if (*sizep + delta < MINSIZE (p)
      && !NILP (XWINDOW (window)->parent))
    {
      Fdelete_window (window);
      return;
    }

  {
    register int maxdelta;
    register Lisp_Object tem;

    maxdelta = (!NILP (parent) ? (*sizefun) (parent) - *sizep
		: (tem = (!NILP (p->next) ? p->next : p->prev),
		   (*sizefun) (tem) - MINSIZE (tem)));

    if (delta > maxdelta)
      /* This case traps trying to make the minibuffer
	 the full screen, or make the only window aside from the
	 minibuffer the full screen.  */
      delta = maxdelta;
  }

  if (!NILP (p->next) &&
      (*sizefun) (p->next) - delta >= MINSIZE (p->next))
    {
      (*setsizefun) (p->next, (*sizefun) (p->next) - delta, 0);
      (*setsizefun) (window, *sizep + delta, 0);
      CURBEG (XWINDOW (p->next)) += delta;
      /* This does not change size of p->next,
	 but it propagates the new top edge to its children */
      (*setsizefun) (p->next, (*sizefun) (p->next), 0);
    }
  else if (!NILP (p->prev) &&
	   (*sizefun) (p->prev) - delta >= MINSIZE (p->prev))
    {
      (*setsizefun) (p->prev, (*sizefun) (p->prev) - delta, 0);
      CURBEG (p) -= delta;
      (*setsizefun) (window, *sizep + delta, 0);
    }
  else
    {
      register int delta1;
      register int opht = (*sizefun) (parent);

      /* If trying to grow this window to or beyond size of the parent,
	 make delta1 so big that, on shrinking back down,
	 all the siblings end up with less than one line and are deleted.  */
      if (opht <= *sizep + delta)
	delta1 = opht * opht * 2;
      /* Otherwise, make delta1 just right so that if we add delta1
	 lines to this window and to the parent, and then shrink
	 the parent back to its original size, the new proportional
	 size of this window will increase by delta.  */
      else
	delta1 = (delta * opht * 100) / ((opht - *sizep - delta) * 100);

      /* Add delta1 lines or columns to this window, and to the parent,
	 keeping things consistent while not affecting siblings.  */
      CURSIZE (XWINDOW (parent)) = opht + delta1;
      (*setsizefun) (window, *sizep + delta1, 0);

      /* Squeeze out delta1 lines or columns from our parent,
	 shriking this window and siblings proportionately.
	 This brings parent back to correct size.
	 Delta1 was calculated so this makes this window the desired size,
	 taking it all out of the siblings.  */
      (*setsizefun) (parent, opht, 0);
    }

  XFASTINT (p->last_modified) = 0;
  XFASTINT (p->last_facechange) = 0;
}
#undef MINSIZE
#undef CURBEG
#undef CURSIZE


/* Return number of lines of text (not counting mode line) in W.  */

int
window_internal_height (w)
     struct window *w;
{
  int ht = XFASTINT (w->height);

  if (MINI_WINDOW_P (w))
    return ht;

  if (!NILP (w->parent) || !NILP (w->vchild) || !NILP (w->hchild)
      || !NILP (w->next) || !NILP (w->prev)
      || SCREEN_WANTS_MODELINE_P (XSCREEN (WINDOW_SCREEN (w))))
    return ht - 1;

  return ht;
}

/* Scroll contents of window WINDOW up N lines.  */

void
window_scroll (window, n)
     Lisp_Object window;
     int n;
{
  register struct window *w = XWINDOW (window);
  register int opoint = point;
  register int pos;
  register int ht = window_internal_height (w);
  register Lisp_Object tem;
  int lose;
  Lisp_Object bolp, nmoved;

  XFASTINT (tem) = point;
  tem = Fpos_visible_in_window_p (tem, window);

  if (NILP (tem))
    {
      Fvertical_motion (make_number (- ht / 2), window);
      XFASTINT (tem) = point;
      Fset_marker (w->start, tem, w->buffer);
      w->force_start = Qt;
    }

  pos = marker_position (w->start);

  if (pos < BEGV)
    pos = BEGV;
  else if (pos > ZV)
    pos = ZV;

  SET_PT (pos);
  lose = n < 0 && point == BEGV;
  Fvertical_motion (make_number (n), window);
  pos = point;
  bolp = Fbolp ();
  SET_PT (opoint);

  if (lose)
    Fsignal (Qbeginning_of_buffer, Qnil);

  if (pos < ZV)
#if 0
      /* Allow scrolling to an empty screen (end of buffer)
	 if that is exactly how far we wanted to go.  */
      || XINT (nmoved) == n)
#endif
    {
      set_marker_restricted (w->start, make_number (pos), w->buffer);
      w->start_at_line_beg = bolp;
      w->redo_mode_line = Qt;
      XFASTINT (w->last_modified) = 0;
      XFASTINT (w->last_facechange) = 0;
      if (pos > opoint)
	SET_PT (pos);
      if (n < 0)
	{
	  SET_PT (pos);
	  tem = Fvertical_motion (make_number (ht), window);
	  if (point > opoint || XFASTINT (tem) < ht)
	    SET_PT (opoint);
	  else
	    Fvertical_motion (make_number (-1), window);
	}
    }
  else
    Fsignal (Qend_of_buffer, Qnil);
}

/* This is the guts of Fscroll_up and Fscroll_down.  */

static void
scroll_command (n, direction)
     register Lisp_Object n;
     int direction;
{
  register int defalt;
  int count = specpdl_ptr - specpdl;

  /* If selected window's buffer isn't current, make it current for the moment.
     But don't screw up if window_scroll gets an error.  */
  if (XBUFFER (XWINDOW (selected_window)->buffer) != current_buffer)
    {
      record_unwind_protect (save_excursion_restore, save_excursion_save ());
      Fset_buffer (XWINDOW (selected_window)->buffer);
    }

  defalt = (window_internal_height (XWINDOW (selected_window))
	    - next_screen_context_lines);
  defalt = direction * (defalt < 1 ? 1 : defalt);

  if (NILP (n))
    window_scroll (selected_window, defalt);
  else if (EQ (n, Qminus))
    window_scroll (selected_window, - defalt);
  else
    {
      n = Fprefix_numeric_value (n);
      window_scroll (selected_window, XINT (n) * direction);
    }

  zmacs_region_stays = 1;
  unbind_to (count);
}

DEFUN ("scroll-up", Fscroll_up, Sscroll_up, 0, 1, "P",
  "Scroll text of current window upward ARG lines; or near full screen if no ARG.\n\
A near full screen is `next-screen-context-lines' less than a full screen.\n\
When calling from a program, supply a number as argument or nil.")
  (n)
     Lisp_Object n;
{
  scroll_command (n, 1);
  return Qnil;
}

DEFUN ("scroll-down", Fscroll_down, Sscroll_down, 0, 1, "P",
  "Scroll text of current window downward ARG lines; or near full screen if no ARG.\n\
A near full screen is `next-screen-context-lines' less than a full screen.\n\
When calling from a program, supply a number as argument or nil.")
  (n)
     Lisp_Object n;
{
  scroll_command (n, -1);
  return Qnil;
}

DEFUN ("scroll-other-window", Fscroll_other_window, Sscroll_other_window, 0, 1, "P",
  "Scroll text of next window upward ARG lines; or near full screen if no ARG.\n\
The next window is the one below the current one; or the one at the top\n\
if the current one is at the bottom.\n\
When calling from a program, supply a number as argument or nil.\n\
\n\
If in the minibuffer, `minibuf-scroll-window' if non-nil\n\
specifies the window to scroll.\n\
If `other-window-scroll-buffer' is non-nil, scroll the window\n\
showing that buffer, popping the buffer up if necessary.")
  (n)
     register Lisp_Object n;
{
  register Lisp_Object window;
  register int ht, pos;
  register struct window *w;
  register int count = specpdl_ptr - specpdl;

  if (MINI_WINDOW_P (XWINDOW (selected_window))
      && !NILP (Vminibuf_scroll_window))
    window = Vminibuf_scroll_window;
  /* If buffer is specified, scroll that buffer.  */
  else if (!NILP (Vother_window_scroll_buffer))
    {
      window = Fget_buffer_window (Vother_window_scroll_buffer, Qnil);
      if (NILP (window))
	window = Fdisplay_buffer (Vother_window_scroll_buffer, Qt, Qnil);
    }
  else
    /* Nothing specified; pick a neighboring window in this screen.  */
    window = Fnext_window (selected_window, Qnil, Qnil);
  CHECK_WINDOW (window, 0);

  if (EQ (window, selected_window))
    error ("There is no other window");

  w = XWINDOW (window);
  ht = window_internal_height (w);

  /* Don't screw up if window_scroll gets an error.  */
  record_unwind_protect (save_excursion_restore, save_excursion_save ());

  Fset_buffer (w->buffer);

  pos = marker_position (w->pointm);

  if (pos < BEGV)
    pos = BEGV;
  else if (pos > ZV)
    pos = ZV;

  SET_PT (pos);

  if (NILP (n))
    window_scroll (window, ht - next_screen_context_lines);
  else if (EQ (n, Qminus))
    window_scroll (window, next_screen_context_lines - ht);
  else
    {
      if (CONSP (n))
	n = Fcar (n);
      CHECK_FIXNUM (n, 0);
      window_scroll (window, XINT (n));
    }
  Fset_marker (w->pointm, make_number (point), Qnil);

  unbind_to (count);
  zmacs_region_stays = 1;
  return Qnil;
}

DEFUN ("scroll-left", Fscroll_left, Sscroll_left, 1, 1, "P",
  "Scroll selected window display ARG columns left.\n\
Default for ARG is window width minus 2.")
  (arg)
     register Lisp_Object arg;
{
  zmacs_region_stays = 1;
  if (NILP (arg))
    XFASTINT (arg) = XFASTINT (XWINDOW (selected_window)->width) - 2;
  else
    arg = Fprefix_numeric_value (arg);

  return
    Fset_window_hscroll (selected_window,
			 make_number (XINT (XWINDOW (selected_window)->hscroll)
				      + XINT (arg)));
}

DEFUN ("scroll-right", Fscroll_right, Sscroll_right, 1, 1, "P",
  "Scroll selected window display ARG columns right.\n\
Default for ARG is window width minus 2.")
  (arg)
     register Lisp_Object arg;
{
  zmacs_region_stays = 1;
  if (NILP (arg))
    XFASTINT (arg) = XFASTINT (XWINDOW (selected_window)->width) - 2;
  else
    arg = Fprefix_numeric_value (arg);

  return
    Fset_window_hscroll (selected_window,
			 make_number (XINT (XWINDOW (selected_window)->hscroll)
				      - XINT (arg)));
}

DEFUN ("recenter", Frecenter, Srecenter, 0, 1, "P",
  "Center point in window and redisplay screen.  With ARG, put point on line ARG.\n\
The desired position of point is always relative to the current window.\n\
Just C-u as prefix means put point in the center of the screen.\n\
No arg (i.e., it is nil) erases the entire screen and then\n\
redraws with point in the center.")
  (n)
     register Lisp_Object n;
{
  register struct window *w = XWINDOW (selected_window);
  register int ht = window_internal_height (w);
  register int opoint = point;

  if (NILP (n))
    {
      extern int screen_garbaged;

      SET_SCREEN_GARBAGED (XSCREEN (WINDOW_SCREEN (w)));
      XFASTINT (n) = ht / 2;
    }
  else if (CONSP (n)) /* Just C-u. */
    {
      XFASTINT (n) = ht / 2;
    }
  else
    {
      n = Fprefix_numeric_value (n);
      CHECK_FIXNUM (n, 0);
    }

  if (XINT (n) < 0)
    XSETINT (n, XINT (n) + ht);

  XSETINT (n, - XINT (n));

  Fvertical_motion (n, selected_window);
  Fset_marker (w->start, make_number (point), w->buffer);
  w->start_at_line_beg = Fbolp ();

  SET_PT (opoint);
  w->force_start = Qt;
  zmacs_region_stays = 1;
  return Qnil;
}

DEFUN ("move-to-window-line", Fmove_to_window_line, Smove_to_window_line,
  1, 1, "P",
  "Position point relative to window.\n\
With no argument, position text at center of window.\n\
An argument specifies screen line; zero means top of window,\n\
negative means relative to bottom of window.")
  (arg)
     register Lisp_Object arg;
{
  register struct window *w = XWINDOW (selected_window);
  register int height = window_internal_height (w);
  register int start;

  if (NILP (arg))
    XFASTINT (arg) = height / 2;
  else
    {
      arg = Fprefix_numeric_value (arg);
      if (XINT (arg) < 0)
	XSETINT (arg, XINT (arg) + height);
    }

  start = marker_position (w->start);
  if (start < BEGV || start > ZV)
    {
      Fvertical_motion (make_number (- height / 2), selected_window);
      Fset_marker (w->start, make_number (point), w->buffer);
      w->start_at_line_beg = Fbolp ();
      w->force_start = Qt;
    }
  else
    SET_PT (start);

  zmacs_region_stays = 1;
  return Fvertical_motion (arg, selected_window);
}

/* #### This window configuration stuff has had serious bugs lurking in it
   for years; it would be a -huge- win if this was reimplemented in lisp.
 */

struct save_window_data
  {
    int size_from_Lisp_Vector_struct;
    struct Lisp_Vector *next_from_Lisp_Vector_struct;
    Lisp_Object screen_width, screen_height;
    Lisp_Object current_window;
    Lisp_Object current_buffer;
    Lisp_Object minibuf_scroll_window;
    Lisp_Object root_window;
    /* A vector, interpreted as a struct saved_window */
    Lisp_Object saved_windows;
#ifdef ENERGIZE
    /* The buffer whose p_sheets are visible */
    Lisp_Object p_sheet_buffer;
#endif
  };

#ifndef ENERGIZE
#define SAVE_WINDOW_DATA_SIZE 7 /* Arg to Fmake_vector */
#else
#define SAVE_WINDOW_DATA_SIZE 8 /* Arg to Fmake_vector */
#endif

/* This is saved as a Lisp_Vector */
struct saved_window
  {
    /* these first two must agree with struct Lisp_Vector in lisp.h */
    int size_from_Lisp_Vector_struct;
    struct Lisp_Vector *next_from_Lisp_Vector_struct;

    Lisp_Object window;
    Lisp_Object buffer, start, pointm, mark;
    Lisp_Object left, top, width, height, hscroll;
    Lisp_Object parent, prev;
    Lisp_Object start_at_line_beg;
    Lisp_Object display_table;
    Lisp_Object	dedicated;
  };

#define SAVED_WINDOW_VECTOR_SIZE 15 /* Arg to Fmake_vector */

#define SAVED_WINDOW_N(swv,n) \
  ((struct saved_window *) (XVECTOR ((swv)->contents[(n)])))

DEFUN ("set-window-configuration",
       Fset_window_configuration, Sset_window_configuration,
       1, 1, 0,
  "Set the configuration of windows and buffers as specified by CONFIGURATION.\n\
CONFIGURATION must be a value previously returned\n\
by `current-window-configuration' (which see).")
     (arg)
     Lisp_Object arg;
{
  register struct window *w;
  register struct save_window_data *data;
  struct Lisp_Vector *saved_windows;
  register struct saved_window *p;
  register Lisp_Object tem;
  Lisp_Object new_current_buffer;
  int k;
  Lisp_Object screen;
  SCREEN_PTR s;
  struct gcpro gcpro1;

  GCPRO1 (arg);

  while (XTYPE (arg) != Lisp_Window_Configuration)
    {
      /* the function window-configuration-p isn't actually defined
	 at present --- is there a need for it? */
      arg = wrong_type_argument (intern ("window-configuration-p"), arg);
    }

  data = (struct save_window_data *) XVECTOR (arg);
  saved_windows = XVECTOR (data->saved_windows);

  screen = XWINDOW (SAVED_WINDOW_N (saved_windows, 0)->window)->screen;
  s = XSCREEN (screen);

  /* If the screen was deleted you can't restore the windows as they
     were deleted too.  Raise an error.  */
  if (s->display.nothing == 0)
    error ("Cannot set-window-configuration for a deleted screen");

    /* restore the screen characteristics */
#ifdef ENERGIZE
  if (SCREEN_IS_X (s))
    {
      extern Lisp_Object desired_psheet_buffer();
      extern void make_psheets_desired ();
    
      Lisp_Object new_desired = data->p_sheet_buffer;
    
      if (BUFFERP (new_desired) &&
	  NILP (XBUFFER (new_desired)->name))
	new_desired = Qnil;	/* the desired buffer was killed */
    
      /* need to restore the desired buffer */
      if (new_desired != desired_psheet_buffer (s))
	make_psheets_desired (s, new_desired);
    }
#endif
    
  windows_or_buffers_changed++;
  new_current_buffer = data->current_buffer;
  if (NILP (XBUFFER (new_current_buffer)->name))
    new_current_buffer = Qnil;

#if 0
 *  /* This was NOT in 18 and I think it's useless --Matthieu. */
 *  /* Mark all windows now on screen as "deleted".
 *     Restoring the new configuration "undeletes" any that are in it.  */
 *
 *  delete_all_subwindows (XWINDOW (s->root_window));
#endif
#if 0
 *  /* This loses when the minibuf screen is not s. */
 *  delete_all_subwindows (XWINDOW (XWINDOW (minibuf_window)->prev));
#endif

  for (k = 0; k < saved_windows->size; k++)
    {
      p = SAVED_WINDOW_N (saved_windows, k);
      w = XWINDOW (p->window);
      w->next = Qnil;

      if (!NILP (p->parent))
	w->parent =
	  SAVED_WINDOW_N (saved_windows, XFASTINT (p->parent))->window;
      else
	w->parent = Qnil;

      if (!NILP (p->prev))
	{
	  w->prev = SAVED_WINDOW_N (saved_windows, XFASTINT (p->prev))->window;
#ifdef MULTI_SCREEN
	  /* This is true for a minibuffer-only screen. */
	  if (w->mini_p && EQ (w->prev, p->window))
	    w->next = Qnil;
	  else
#endif	/* MULTI_SCREEN */
	    XWINDOW (w->prev)->next = p->window;
	}
      else
	{
	  w->prev = Qnil;
	  if (!NILP (w->parent))
	    {
	      if (EQ (p->width, XWINDOW (w->parent)->width))
		{
		  XWINDOW (w->parent)->vchild = p->window;
		  XWINDOW (w->parent)->hchild = Qnil;
		}
	      else
		{
		  XWINDOW (w->parent)->hchild = p->window;
		  XWINDOW (w->parent)->vchild = Qnil;
		}
	    }
	}
      w->left = p->left;
      w->top = p->top;
      w->width = p->width;
      w->height = p->height;
      w->hscroll = p->hscroll;
      w->display_table = p->display_table;
      w->dedicated = p->dedicated;
      XFASTINT (w->last_modified) = 0;
      XFASTINT (w->last_facechange) = 0;

      /* Reinstall the saved buffer and pointers into it.  */
      if (NILP (p->buffer))
	w->buffer = p->buffer;
      else
	{
	  if (!NILP (XBUFFER (p->buffer)->name))
	    /* If saved buffer is alive, install it.  */
	    {
	      w->buffer = p->buffer;
	      w->start_at_line_beg = p->start_at_line_beg;
	      set_marker_restricted (w->start, Fmarker_position (p->start), w->buffer);
	      set_marker_restricted (w->pointm, Fmarker_position (p->pointm), w->buffer);
	      Fset_marker (XBUFFER (w->buffer)->mark,
			   Fmarker_position (p->mark), w->buffer);

	      if (!EQ (p->buffer, new_current_buffer) &&
		  XBUFFER (p->buffer) == current_buffer)
		Fgoto_char (w->pointm);
	    }
	  else if (NILP (w->buffer) || NILP (XBUFFER (w->buffer)->name))
	    /* Else if window's old buffer is dead too, get a live one.  */
	    {
	      /* ## The following line makes me nervous... */
 	      /* w->buffer = Fcdr (Fcar (XSCREEN (w->screen)->buffer_alist));*/
	      w->buffer = Fget_buffer_create (build_string ("*scratch*"));
 	      /* w->buffer = Fother_buffer (Qnil, w->screen); */
	      /* This will set the markers to beginning of visible range.  */
	      set_marker_restricted (w->start, make_number (0), w->buffer);
	      set_marker_restricted (w->pointm, make_number (0), w->buffer);
	      w->start_at_line_beg = Qt;
	    }
	  else
	    /* Keeping window's old buffer; make sure the markers are real.  */
	    {
	      /* Set window markers at start of visible range.  */
	      if (XMARKER (w->start)->buffer == 0)
		set_marker_restricted (w->start, make_number (0), w->buffer);
	      if (XMARKER (w->pointm)->buffer == 0)
		set_marker_restricted (w->pointm,
				       make_number (BUF_PT (XBUFFER (w->buffer))),
				       w->buffer);
	      w->start_at_line_beg = Qt;
	    }
	}
    }


  SCREEN_ROOT_WINDOW (s) = data->root_window;

  if (XFASTINT (data->screen_height) != SCREEN_HEIGHT (s)
      || XFASTINT (data->screen_width) != SCREEN_WIDTH (s))
    {
      int current_height = SCREEN_HEIGHT (s);
      int current_width = SCREEN_WIDTH (s);
      SCREEN_HEIGHT (s) = 0;
      SCREEN_WIDTH (s) = 0;
      change_screen_size (s, current_height, current_width, 0);
    }

#if 0
 * /* Selecting the window (below) will also select the screen.  If we call
 *    Fselect_screen now it will try to select whatever is in the 
 *    selected_window slot of the screen, which most likely is a deleted window
 *    which would be pretty bad. */
 * #ifdef MULTI_SCREEN
 *  if (s != selected_screen && ! SCREEN_IS_TERMCAP (s))
 *    Fselect_screen (screen, Qnil);
 *#endif
#endif

   /* If restoring in the current screen make the window current, otherwise
      just update the screen selected_window slot to be the restored
      current_window. */
  if (s == selected_screen)
    {
      Fselect_window (data->current_window);
      if (!NILP (new_current_buffer))
	Fset_buffer (new_current_buffer);
      else
	Fset_buffer (XWINDOW (selected_window)->buffer);
#ifdef ENERGIZE
      energize_buffer_shown_hook (XWINDOW(selected_window));
#endif
    }
  else
    s->selected_window = data->current_window;

  Vminibuf_scroll_window = data->minibuf_scroll_window;

  UNGCPRO;

  return (Qnil);
}

/* Mark all windows now on screen as deleted
   by setting their buffers to nil.  */

static void
delete_all_subwindows (w)
     register struct window *w;
{
  register int count = 1;
  w->buffer = Qnil;
  if (!NILP (w->next))
    delete_all_subwindows (XWINDOW (w->next));
  if (!NILP (w->vchild))
    delete_all_subwindows (XWINDOW (w->vchild));
  if (!NILP (w->hchild))
    delete_all_subwindows (XWINDOW (w->hchild));
}

static int
count_windows (window)
     register struct window *window;
{
  register int count = 1;
  if (!NILP (window->next))
    count += count_windows (XWINDOW (window->next));
  if (!NILP (window->vchild))
    count += count_windows (XWINDOW (window->vchild));
  if (!NILP (window->hchild))
    count += count_windows (XWINDOW (window->hchild));
  return count;
}

static int
save_window_save (window, vector, i)
     Lisp_Object window;
     struct Lisp_Vector *vector;
     int i;
{
  register struct saved_window *p;
  register struct window *w;
  register Lisp_Object tem;

  for (;!NILP (window); window = w->next)
    {
      p = SAVED_WINDOW_N (vector, i);
      w = XWINDOW (window);

      XFASTINT (w->temslot) = i++;
      p->window = window;
      p->buffer = w->buffer;
      p->left = w->left;
      p->top = w->top;
      p->width = w->width;
      p->height = w->height;
      p->hscroll = w->hscroll;
      p->display_table = w->display_table;
      if (!NILP (w->buffer))
	{
/* jwz: installed this patch from Jim Blandy */
#if 0
	  if (EQ (window, selected_window)
	      && XBUFFER (w->buffer) == current_buffer)
	    p->pointm = Fpoint_marker ();
#else
	  /* Save w's value of point in the window configuration.
	     If w is the selected window, then get the value of point
	     from the buffer; pointm is garbage in the selected window.  */
	  if (EQ (window, selected_window))
	    {
	      p->pointm = Fmake_marker ();
	      Fset_marker (p->pointm, BUF_PT (XBUFFER (w->buffer)),
			   w->buffer);
	    }
#endif
	  else
	    p->pointm = Fcopy_marker (w->pointm);

	  p->start = Fcopy_marker (w->start);
	  p->start_at_line_beg = w->start_at_line_beg;

	  tem = XBUFFER (w->buffer)->mark;
	  p->mark = Fcopy_marker (tem);
	}
      else
	{
	  p->pointm = Qnil;
	  p->start = Qnil;
	  p->mark = Qnil;
	  p->start_at_line_beg = Qnil;
	}

      if (NILP (w->parent))
	p->parent = Qnil;
      else
	p->parent = XWINDOW (w->parent)->temslot;

      if (NILP (w->prev))
	p->prev = Qnil;
      else
	p->prev = XWINDOW (w->prev)->temslot;

      p->dedicated = w->dedicated;
      if (!NILP (w->vchild))
	i = save_window_save (w->vchild, vector, i);
      if (!NILP (w->hchild))
	i = save_window_save (w->hchild, vector, i);
    }

  return i;
}

DEFUN ("current-window-configuration",
	Fcurrent_window_configuration, Scurrent_window_configuration, 0, 0, 0,
       "Return an object representing Emacs' current window configuration.\n\
This describes the number of windows, their sizes and current buffers,\n\
and for each displayed buffer, where display starts, and the positions of\n\
point and mark.  An exception is made for point in the current buffer:\n\
its value is -not- saved.")
  ()
{
  register Lisp_Object tem;
  register int n_windows;
  register struct save_window_data *data;
  register int i;

  n_windows = count_windows (XWINDOW (SCREEN_ROOT_WINDOW (selected_screen)));
  data = (struct save_window_data *)
           XVECTOR (Fmake_vector (make_number (SAVE_WINDOW_DATA_SIZE),
				  Qnil));
  XFASTINT (data->screen_width) = SCREEN_WIDTH (selected_screen);
  XFASTINT (data->screen_height) = SCREEN_HEIGHT (selected_screen);
  data->current_window = selected_window;
  XSET (data->current_buffer, Lisp_Buffer, current_buffer);
  data->minibuf_scroll_window = Vminibuf_scroll_window;
  data->root_window = SCREEN_ROOT_WINDOW (selected_screen);
  tem = Fmake_vector (make_number (n_windows), Qnil);
  data->saved_windows = tem;
  for (i = 0; i < n_windows; i++)
    XVECTOR (tem)->contents[i]
      = Fmake_vector (make_number (SAVED_WINDOW_VECTOR_SIZE), Qnil);
  save_window_save (SCREEN_ROOT_WINDOW (selected_screen),
		    XVECTOR (tem), 0);
#ifdef ENERGIZE
  {
    extern Lisp_Object desired_psheet_buffer();
    data->p_sheet_buffer = desired_psheet_buffer (selected_screen);
    if (!data->p_sheet_buffer)
      data->p_sheet_buffer = Qnil;
  }
#endif
  XSET (tem, Lisp_Window_Configuration, data);
  return (tem);
}

DEFUN ("save-window-excursion", Fsave_window_excursion, Ssave_window_excursion,
  0, UNEVALLED, 0,
  "Execute body, preserving window sizes and contents.\n\
Restores which buffer appears in which window, where display starts,\n\
as well as the current buffer.\n\
Does not restore the value of point in current buffer.")
  (args)
     Lisp_Object args;
{
  register Lisp_Object val;
  register int count = specpdl_ptr - specpdl;

  record_unwind_protect (Fset_window_configuration,
			 Fcurrent_window_configuration ());
  val = Fprogn (args);
  unbind_to (count);
  return val;
}

init_window_once ()
{
#ifdef MULTI_SCREEN
  selected_screen = make_terminal_screen ();
  XSET (Vterminal_screen, Lisp_Screen, selected_screen);
  minibuf_window = selected_screen->minibuffer_window;
  selected_window = selected_screen->selected_window;
  Vscreen_list = Fcons (Vterminal_screen, Qnil);
#else /* not MULTI_SCREEN */
  extern Lisp_Object get_minibuffer ();

  root_window = make_window (0);
  minibuf_window = make_window (0);

  XWINDOW (root_window)->next = minibuf_window;
  XWINDOW (minibuf_window)->prev = root_window;

  /* These values 9 and 10 are arbitrary,
     just so that there is "something there."
     Correct values are put in in init_xdisp */

  XFASTINT (XWINDOW (root_window)->width) = 10;
  XFASTINT (XWINDOW (minibuf_window)->width) = 10;

  XFASTINT (XWINDOW (root_window)->height) = 9;
  XFASTINT (XWINDOW (minibuf_window)->top) = 9;
  XFASTINT (XWINDOW (minibuf_window)->height) = 1;

  /* Prevent error in Fset_window_buffer.  */
  XWINDOW (root_window)->buffer = Qt;
  XWINDOW (minibuf_window)->buffer = Qt;

  /* Now set them up for real.  */
  Fset_window_buffer (root_window, Fcurrent_buffer ());
  Fset_window_buffer (minibuf_window, get_minibuffer (0));

  selected_window = root_window;
#endif /* not MULTI_SCREEN */
}

syms_of_window ()
{
  Qwindowp = intern ("windowp");
  staticpro (&Qwindowp);

  /* Make sure all windows get marked */
  staticpro (&minibuf_window);

  DEFVAR_LISP ("temp-buffer-show-function", &Vtemp_buffer_show_function,
    "Non-nil means call as function to display a help buffer.\n\
Used by `with-output-to-temp-buffer'.");
  Vtemp_buffer_show_function = Qnil;

  DEFVAR_LISP ("display-buffer-function", &Vdisplay_buffer_function,
    "If non-nil, function to call to handle `display-buffer'.\n\
It will receive two args, the buffer and a flag which if non-nil means\n\
 that the currently selected window is not acceptable.\n\
Commands such as `switch-to-buffer-other-window' and `find-file-other-window'\n\
work using this function.");
  Vdisplay_buffer_function = Qnil;

  DEFVAR_LISP ("pre-display-buffer-function", &Vpre_display_buffer_function,
    "If non-nil, function that will be called from `display-buffer'\n\
as the first thing.  It will receive two args, the buffer and a flag which\n\
if non-nil means  that the currently selected window is not acceptable.\n\
This function may be used to select an appropriate screen for the buffer,\n\
for example.  See also the variable `display-buffer-function', which may\n\
be used to completely replace the display-buffer function.");
  Vpre_display_buffer_function = Qnil;

  DEFVAR_LISP ("minibuffer-scroll-window", &Vminibuf_scroll_window,
    "Non-nil means it is the window that C-M-v in minibuffer should scroll.");
  Vminibuf_scroll_window = Qnil;

  DEFVAR_LISP ("other-window-scroll-buffer", &Vother_window_scroll_buffer,
    "If non-nil, this is a buffer and \\[scroll-other-window] should scroll its window.");
  Vother_window_scroll_buffer = Qnil;

  DEFVAR_BOOL ("pop-up-windows", &pop_up_windows,
    "*Non-nil means display-buffer should make new windows.");
  pop_up_windows = 1;

  DEFVAR_INT ("next-screen-context-lines", &next_screen_context_lines,
    "*Number of lines of continuity when scrolling by screenfuls.");
  next_screen_context_lines = 2;

  DEFVAR_INT ("split-height-threshold", &split_height_threshold,
    "*display-buffer would prefer to split the largest window if this large.\n\
If there is only one window, it is split regardless of this value.");
  split_height_threshold = 500;

  DEFVAR_INT ("window-min-height", &window_min_height,
    "*Delete any window less than this tall (including its mode line).");
  window_min_height = 4;

  DEFVAR_INT ("window-min-width", &window_min_width,
    "*Delete any window less than this wide.");
  window_min_width = 10;

  defsubr (&Sselected_window);
  defsubr (&Sminibuffer_window);
  defsubr (&Swindow_minibuffer_p);
  defsubr (&Swindowp);
  defsubr (&Spos_visible_in_window_p);
  defsubr (&Swindow_buffer);
  defsubr (&Swindow_height);
  defsubr (&Swindow_width);
  defsubr (&Swindow_hscroll);
  defsubr (&Sset_window_hscroll);
  defsubr (&Swindow_edges);
  defsubr (&Swindow_point);
  defsubr (&Swindow_start);
  defsubr (&Swindow_end);
  defsubr (&Sset_window_point);
  defsubr (&Sset_window_start);
  defsubr (&Swindow_dedicated_p);
  defsubr (&Sset_window_buffer_dedicated);
  defsubr (&Swindow_display_table);
  defsubr (&Sset_window_display_table);
  defsubr (&Snext_window);
  defsubr (&Sprevious_window);
  defsubr (&Sother_window);
  defsubr (&Sget_lru_window);
  defsubr (&Sget_largest_window);
  defsubr (&Sget_buffer_window);
  defsubr (&Sdelete_other_windows);
  defsubr (&Sdelete_windows_on);
  defsubr (&Sreplace_buffer_in_windows);
  defsubr (&Sdelete_window);
  defsubr (&Sset_window_buffer);
  defsubr (&Sselect_window);
  defsubr (&Sdisplay_buffer);
  defsubr (&Ssplit_window);
  defsubr (&Senlarge_window);
  defsubr (&Sshrink_window);
  defsubr (&Sscroll_up);
  defsubr (&Sscroll_down);
  defsubr (&Sscroll_left);
  defsubr (&Sscroll_right);
  defsubr (&Sscroll_other_window);
  defsubr (&Srecenter);
  defsubr (&Smove_to_window_line);
  defsubr (&Sset_window_configuration);
  defsubr (&Scurrent_window_configuration);
  defsubr (&Ssave_window_excursion);
}

keys_of_window ()
{
  initial_define_key (control_x_map, '1', "delete-other-windows");
  initial_define_key (control_x_map, '2', "split-window");
  initial_define_key (control_x_map, '0', "delete-window");
  initial_define_key (control_x_map, 'o', "other-window");
  initial_define_key (control_x_map, '^', "enlarge-window");
  initial_define_key (control_x_map, '<', "scroll-left");
  initial_define_key (control_x_map, '>', "scroll-right");

  initial_define_key (global_map, Ctl ('V'), "scroll-up");
  initial_define_key (meta_map, Ctl ('V'), "scroll-other-window");
  initial_define_key (meta_map, 'v', "scroll-down");

  initial_define_key (global_map, Ctl('L'), "recenter");
  initial_define_key (meta_map, 'r', "move-to-window-line");
}
