/* Window creation, deletion and examination for GNU Emacs.
   Does not include redisplay.
   Copyright (C) 1985, 1986, 1987, 1992, 1993, 1994
   Free Software Foundation, Inc.

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
#include "buffer.h"
#include "screen.h"
#include "window.h"
#include "commands.h"
#include "indent.h"
#include "termchar.h"
#include "disptab.h"
#ifdef HAVE_X_WINDOWS
#include "xterm.h"
#include "xobjs.h"
#endif
#include "dispmisc.h"
#include "dispextern.h"
#include "faces.h"

#include <stdio.h>              /* for sprintf */

Lisp_Object Qwindowp;
Lisp_Object Qwindow_configuration_p;
Lisp_Object Qscroll_up;
Lisp_Object Qscroll_down;

static void delete_all_subwindows (register struct window *w);
static struct window *decode_window (Lisp_Object window);

#ifdef ENERGIZE
extern void energize_buffer_shown_hook ();
extern void energize_buffer_hidden_hook ();
extern void energize_window_selected_hook ();
extern void energize_window_deselected_hook ();
extern Lisp_Object desired_psheet_buffer ();
extern void make_psheets_desired ();
#endif

/* This is the window in which the terminal's cursor should
   be left when nothing is being done with it.  This must
   always be a leaf window, and its buffer is selected by
   the top level editing loop at the end of each command.

   This value is always the same as
   SCREEN_SELECTED_WINDOW (selected_screen).  */

Lisp_Object selected_window;

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


DEFUN ("windowp", Fwindowp, Swindowp, 1, 1, 0,
  "Returns t if OBJ is a window.")
  (obj)
     Lisp_Object obj;
{
  return (WINDOWP (obj) ? Qt : Qnil);
}

static Lisp_Object mark_window (Lisp_Object, void (*) (Lisp_Object));
static void print_window (Lisp_Object, Lisp_Object, int);
DEFINE_LRECORD_IMPLEMENTATION ("window", lrecord_window,
                               mark_window, print_window, 0, 0,
			       sizeof (struct window));

static Lisp_Object
mark_window (Lisp_Object obj, void (*markobj) (Lisp_Object))
{
  struct window *window = XWINDOW (obj);
  ((markobj) (window->screen));
  ((markobj) (window->mini_p));
  ((markobj) (window->next));
  ((markobj) (window->prev));
  ((markobj) (window->hchild));
  ((markobj) (window->vchild));
  ((markobj) (window->parent));
  ((markobj) (window->buffer));
  ((markobj) (window->start));
  ((markobj) (window->pointm));
  ((markobj) (window->sb_point));	/* #### move to scrollbar.c? */
  ((markobj) (window->hscroll));
  ((markobj) (window->use_time));
  ((markobj) (window->last_modified));
  ((markobj) (window->last_point));
  ((markobj) (window->last_facechange));
  ((markobj) (window->last_point_x));
  ((markobj) (window->last_point_y));
  ((markobj) (window->last_mark_x));
  ((markobj) (window->last_mark_y));
  ((markobj) (window->display_table));
  ((markobj) (window->dedicated));
  return (Qnil);
}
  
static void
print_window (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  char buf[200];

  if (print_readably)
    error (GETTEXT ("printing unreadable object #<window 0x%x>"),
           (long) XWINDOW (obj));
      
  write_string_1 ("#<window", -1, printcharfun);
  if (!NILP (XWINDOW (obj)->buffer))
  {
    Lisp_Object name = XBUFFER (XWINDOW (obj)->buffer)->name;
    write_string_1 (" on ", -1, printcharfun);
    print_internal (name, printcharfun, 1);
  }
  sprintf (buf, " 0x%x>", (long) XWINDOW (obj));
  write_string_1 (buf, -1, printcharfun);
}


Lisp_Object
make_window ()
{
  Lisp_Object val;
  register struct window *p = alloc_lcrecord (sizeof (struct window),
                                              lrecord_window);

  XSETR (val, Lisp_Window, p);

  p->screen = Qnil;
  p->mini_p = Qnil;
  p->next = Qnil;
  p->prev = Qnil;
  p->hchild = Qnil;  
  p->vchild = Qnil; 
  p->parent = Qnil;
  p->pixleft = 0;
  p->pixtop = 0;
  p->pixheight = 0;
  p->pixwidth = 0;
  p->buffer = Qnil;
  p->start = Fmake_marker ();
  p->pointm = Fmake_marker ();
  p->sb_point = Fmake_marker ();
  p->force_start = 0;
  p->hscroll = Qzero;
  p->use_time = Qzero;
  p->last_modified = Qzero;
  p->last_point = Qnil;
  p->last_facechange = Qnil;
  p->config_mark = 0;
  p->last_point_x = Qzero;
  p->last_point_y = Qzero;
  p->last_mark_x = Qnil;
  p->last_mark_y = Qnil;
  p->window_end_pos = 0;
  p->window_end_valid = 0;
  p->window_end_vpos = 0;
  p->window_end_ppos = 0;
  p->redo_mode_line = 0;
  p->start_at_line_beg = 0;
  p->display_table = Qnil;
  p->dedicated = Qnil;
  p->size_change = 0;
  p->used_height = 0;

  return (val);
}

/*
 * After having done all of this mirror stuff I've decided that I
 * should have stuck it somewhere else since it is really a redisplay
 * thing.  But redisplay is going to be changing quite a bit, and this
 * along with it so I'm not going to move it for now.
 *
 * The redisplay structures used to be stored with each window.  While
 * they are logically something associated with screens they can't be
 * stored there with a redisplay which handles variable height lines.
 * Lines in horizontally split windows might not line up.  So they get
 * stored with the windows.
 *
 * The problem with this is window configurations.  When restoring a
 * window configuration it now becomes problematic to do an
 * incremental redisplay.  The solution is to store the redisplay
 * structures with the screen as they should be but laid out in the
 * same manner as the window structure.  Thus is born the window
 * mirror.
 */

/* Create a new mindow mirror structure and associated redisplay
   structs. */
static struct window_mirror *
new_window_mirror (struct screen *s)
{
  struct window_mirror *t;

  t = (struct window_mirror *)
    xmalloc (sizeof (struct window_mirror));

  t->screen = s;
  t->next = 0;
  t->hchild = t->vchild = 0;

  t->lines = get_line();
  t->modeline = get_line();
  t->modeline->modeline = 1;

  return t;
}

/* Synchronize the mirror structure with a given window structure.
   This is normally called from update_screen_window_mirror with a
   starting window of s->root_window. */
static struct window_mirror *
update_mirror_internal (Lisp_Object win, struct window_mirror *mir)
{
  if (NILP (win))
    {
      if (mir)
	{
	  free_window_mirror (mir);
	  mir = 0;
	}
      return mir;
    }
  else
    if (!mir)
      mir = new_window_mirror (XSCREEN (XWINDOW (win)->screen));

  mir->next = update_mirror_internal (XWINDOW (win)->next, mir->next);
  mir->hchild = update_mirror_internal (XWINDOW (win)->hchild, mir->hchild);
  mir->vchild = update_mirror_internal (XWINDOW (win)->vchild, mir->vchild);

  /*
   * If the redisplay structs are not empty and the mirror has
   * children, then this mirror structure was formerly being used for
   * display but is no longer.  Clear its display structs so that
   * redisplay doesn't accidentally think they are accurate if it is
   * later used for display purposes once again.
   */
  if ((mir->modeline->body != mir->modeline->end)
      || (mir->lines->body != mir->lines->end)
      || (mir->lines->margin_start != mir->lines->margin_end))
    if (mir->vchild || mir->hchild)
      {
	clear_display_structs (mir);
	mir->lines = get_line();
	mir->modeline = get_line();
	mir->modeline->modeline = 1;
      }

  return mir;
}

/* Given a window mirror, determine which real window it contains the
   redisplay structures for. */
static Lisp_Object
real_window_internal (Lisp_Object win, struct window_mirror *rmir,
		      struct window_mirror *mir)
{
  Lisp_Object retval;

  for (; !NILP (win) && rmir ; win = XWINDOW (win)->next, rmir = rmir->next)
    {
      if (mir == rmir)
	return win;
      if (!NILP (XWINDOW (win)->vchild))
	{
	  retval = real_window_internal (XWINDOW (win)->vchild, rmir->vchild,
					 mir);
	  if (!NILP (retval))
	    return retval;
	}
      if (!NILP (XWINDOW (win)->hchild))
	{
	  retval = real_window_internal (XWINDOW (win)->hchild, rmir->hchild,
					 mir);
	  if (!NILP (retval))
	    return retval;
	}
    }

  return Qnil;
}

/* Given a real window, find the mirror structure which contains its
   redisplay structures. */
static struct window_mirror *
find_window_mirror_internal (Lisp_Object win, struct window_mirror *rmir,
			    struct window *w)
{
  struct window_mirror *retval;

  for (; !NILP (win); win = XWINDOW (win)->next, rmir = rmir->next)
    {
      if (w == XWINDOW (win))
	return rmir;
      if (!NILP (XWINDOW (win)->vchild) &&
	  (retval = find_window_mirror_internal (XWINDOW(win)->vchild,
						 rmir->vchild, w)))
	return retval;
      if (!NILP (XWINDOW (win)->hchild) &&
	  (retval = find_window_mirror_internal (XWINDOW(win)->hchild,
						 rmir->hchild, w)))
	return retval;
    }

  return 0;
}

/* Update the mirror structure for the given screen. */
void
update_screen_window_mirror (struct screen *s)
{
  s->root_mirror = update_mirror_internal (s->root_window, s->root_mirror);
  s->mirror_dirty = 0;
}

/* Free a given mirror structure along with all of its children as
   well as their associated display structures. */
void
free_window_mirror (struct window_mirror *mir)
{
  struct window_mirror *p;

  p = mir;
  while (mir)
    {
      if (mir->hchild) free_window_mirror (mir->hchild);
      if (mir->vchild) free_window_mirror (mir->vchild);
      clear_display_structs (mir);
      mir = mir->next;
      xfree (p);
      p = mir;
    }
}

/* Given a mirror structure, return the window it mirrors.  Calls
   real_window_internal to do most of the work. */
Lisp_Object
real_window (struct window_mirror *mir, int no_abort)
{
  Lisp_Object retval;

  retval = real_window_internal (mir->screen->root_window,
				 mir->screen->root_mirror, mir);

  if (NILP (retval) && !no_abort)
    abort();

  return retval;
}

/* Given a real window, return its mirror structure.  Calls
   find_window_mirror_internal to do all of the work. */
struct window_mirror *
find_window_mirror (struct window *w)
{
  struct screen *s = XSCREEN (w->screen);
  if (s->mirror_dirty)
    update_screen_window_mirror (s);
  return find_window_mirror_internal (s->root_window, s->root_mirror, w);
}

/* Return a pointer to the display structures for the given window. */
struct line_header *
window_display_lines (struct window *w)
{
  struct window_mirror *t;

  if (XSCREEN (w->screen)->mirror_dirty)
    update_screen_window_mirror (XSCREEN (w->screen));
  t = find_window_mirror (w);
  if (!t)
    abort();

  return t->lines;
}

/* Return a pointer to the modeline display structures for the given
   window. */
struct line_header *
window_display_modeline (struct window *w)
{
  struct window_mirror *t;

  if (XSCREEN (w->screen)->mirror_dirty)
    update_screen_window_mirror (XSCREEN (w->screen));
  t = find_window_mirror (w);
  if (!t)
    abort();

  return t->modeline;
}

/*
 * The algorithm:
 *	Any window which has an ancestor whose parent was horizontally
 *	split and that ancestor is not the leftmost child of its parent
 *	needs a divider.
 */
int
window_needs_vertical_divider (struct window *w)
{
  /* we assume that we got a valid window to start with */

  Lisp_Object parent, current_ancestor, window;
  int needs_divider = 0;

  XSETR (window, Lisp_Window, w);

  parent = XWINDOW (window)->parent;
  current_ancestor = window;

  while (!NILP (parent) && !needs_divider)
    {
      if (!NILP (XWINDOW (parent)->hchild) &&
	  !EQ (XWINDOW (parent)->hchild, current_ancestor))
	needs_divider = 1;

      current_ancestor = parent;
      parent = XWINDOW (parent)->parent;
    }

  return needs_divider;
}


DEFUN ("selected-window", Fselected_window, Sselected_window, 0, 0, 0,
  "Return the window that the cursor now appears in and commands apply to.")
  ()
{
  return selected_window;
}

DEFUN ("minibuffer-window", Fminibuffer_window, Sminibuffer_window, 0, 1, 0,
  "Return the window used now for minibuffers.\n\
If the optional argument SCREEN is specified, return the minibuffer window\n\
used by that screen.")
     (screen)
     Lisp_Object screen;
{
  Lisp_Object result;
#ifdef MULTI_SCREEN
  if (NILP (screen))
    XSETR (screen, Lisp_Screen, selected_screen);
  else
    CHECK_LIVE_SCREEN (screen, 0);
#endif
  if (SCREENP (Vglobal_minibuffer_screen)) /* >>> This kludge again... */
    return (Vglobal_minibuffer_screen);

  result = SCREEN_MINIBUF_WINDOW (XSCREEN (screen)); 
  if (WINDOWP (result))
    return (result);

#ifdef MULTI_SCREEN
  choose_minibuf_screen ();     /*>>> Is this crock needed here??? */
#endif /* MULTI_SCREEN */
  return (minibuf_window);
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
    posint = PT;
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

  height = window_displayed_height (w, 0);

  buf = XBUFFER (w->buffer);
  if (XINT (w->last_modified) >= BUF_MODIFF (buf)
      && XINT (w->last_facechange) >= BUF_FACECHANGE (buf))
    {
      /* If screen is up to date,
	 use the info recorded about how much text fit on it. */
      if (posint < BUF_Z (buf) - w->window_end_pos
	  || (w->window_end_vpos < height))
	return Qt;
      return Qnil;
    }
  else
    {
      if (posint > BUF_Z (buf))
	return Qnil;

      /* If that info is not correct, calculate afresh */
      posval = *compute_motion (w, top, 0, 0, posint, height, 0,
			       XINT (w->hscroll), 0);

      return ((posval.vpos < height) ? Qt : Qnil);
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
  return make_number (window_char_height (decode_window (window)));
}

DEFUN ("window-pixheight", Fwindow_pixheight, Swindow_pixheight, 0, 1, 0,
       "Return the height of WINDOW in pixels.  Defaults to current window.")
     (window) Lisp_Object window;
{
  return (make_number (decode_window (window)->pixheight));
}

DEFUN ("window-width", Fwindow_width, Swindow_width, 0, 1, 0,
  "Return the number of columns in WINDOW.")
  (window)
     Lisp_Object window;
{
  register struct window *w = decode_window (window);
  /* If this window does not end at the right margin,
     must deduct one column for the border */
  if (window_needs_vertical_divider (w))
    return (make_number (window_char_width (w) - 1));
  return (make_number (window_char_width (w)));
}

DEFUN ("window-pixwidth", Fwindow_pixwidth, Swindow_pixwidth, 0, 1, 0,
       "Return the width of WINDOW in pixels.  Defaults to current window.")
     (window) Lisp_Object window;
{
  return (make_number (decode_window (window)->pixwidth));
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
  if (XINT (ncol) < 0) ncol = Qzero;
  if (XINT (ncol) >= (1 << (SHORTBITS - 1)))
    args_out_of_range (ncol, Qnil);
  w = decode_window (window);
  if (XINT (w->hscroll) != XINT (ncol))
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
  struct window *w = decode_window (window);

  struct screen *s = XSCREEN(w->screen);
  int left = w->pixleft - SCREEN_INT_BORDER (s);
  int top = w->pixtop - SCREEN_INT_BORDER (s);
  int right, bottom;
  struct Lisp_Font *font = XFONT (SCREEN_DEFAULT_FONT (s));

  left = left / font->width;
  top = top / font->height;

  bottom = window_char_height(w) + top;
  right = window_char_width(w) + left;

  if (window_needs_vertical_divider (w))
    left++;

  return list4 (make_number(left),
                make_number(top),
		make_number(right),
		make_number(bottom));
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
	BUF_Z (current_buffer) - w->window_end_pos);

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

  if (!EQ (window, selected_window))
    windows_changed++;

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
  w->start_at_line_beg = 0;
  if (NILP (noforce))
    w->force_start = 1;
  w->redo_mode_line = 1;
  w->last_modified = Qzero;
  w->last_facechange = Qzero;
  if (!EQ (window, selected_window))
    windows_changed++;
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
  abort();
/*  tem = Vstandard_display_table;
  if (VECTORP (tem) && XVECTOR (tem)->size == DISP_TABLE_SIZE)
    return XVECTOR (tem);
  return 0;
*/
}

DEFUN ("set-window-display-table",
       Fset_window_display_table, Sset_window_display_table, 2, 2, 0,
  "Set WINDOW's display-table to TABLE.")
  (window, table)
     register Lisp_Object window, table;
{
  register struct window *w;

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
      struct buffer *b= XBUFFER (buf);
      SET_BUF_PT (b, clip_to_bounds (BUF_BEGV (b),
                                     marker_position (w->pointm),
                                     BUF_ZV (b)));
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

  if (EQ (old, SCREEN_ROOT_WINDOW (XSCREEN (o->screen))))
    SCREEN_ROOT_WINDOW (XSCREEN (o->screen)) = replacement;

  p->pixleft = o->pixleft;
  p->pixtop = o->pixtop;
  p->pixwidth = o->pixwidth;
  p->pixheight = o->pixheight;

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
      windows_changed++;
      if (NILP (memq_no_quit (p->screen, Vscreen_list)))
	/* this screen isn't fully initialized yet; don't blow up. */
	return Qnil;
      if (MINI_WINDOW_P (XWINDOW (window)))
	error (GETTEXT ("Attempt to delete the minibuffer window"));

      /* It has been suggested that it's a good thing for C-x 0 to have this
	 behavior, but not such a good idea for #'delete-window to have it.
	 Maybe C-x 0 should be bound to something else, or maybe screen
	 deletion should only happen when this is called interactively.
       */
      if (EQ (p->screen, next_screen (p->screen, 0, 0)))
	error ("Attempt to delete the only window on the only screen");
      Fdelete_screen (p->screen);
      return Qnil;
    }
#else
  if (NILP (parent))
    error (GETTEXT ("Attempt to delete minibuffer or sole ordinary window"));
#endif

  par = XWINDOW (parent);

  windows_changed++;

  if (EQ (window, selected_window))
    Fselect_window (Fnext_window (window, Qnil, Qnil, Qnil));

  tem = p->buffer;
  /* tem is null for dummy parent windows
     (which have inferiors but not any contents themselves) */
  if (!NILP (tem))
    {
      unshow_buffer (p);
      unchain_marker (p->pointm);
      unchain_marker (p->start);
      unchain_marker (p->sb_point);
/*      clear_display_structs(window); */
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
    {
      /* If p gives its space to its next sibling, that sibling needs
	 to have its top/left side pulled back to where p's is.
	 set_window_{height,width} will re-position the sibling's
	 children.  */
      sib = p->next;
      XWINDOW (sib)->pixtop = p->pixtop;
      XWINDOW (sib)->pixleft = p->pixleft;
    }

  /* Stretch that sibling.  */
  if (!NILP (par->vchild))
    set_window_height (sib,
		       (XWINDOW (sib)->pixheight + p->pixheight),
		       1);
  if (!NILP (par->hchild))
    set_window_width (sib,
                      (XWINDOW (sib)->pixwidth + p->pixwidth),
		      1);

  /* In the loop
     (while t (split-window) (delete-window))
     we end up with a tree of deleted windows which are all connected
     through the `next' slot.  This might not seem so bad, as they're
     deleted, and will presumably be GCed - but if even *one* of those
     windows is still being pointed to, by the user, or by a window
     configuration, then *all* of those windows stick around.

     This is all complicated by the fact that it's not clear which
     elements of a deleted window are needed by window configurations
     which want to be able to restore deleted windows.

     It looks to me like window configs don't need the prev/next slots
     of the windows they point to - they regenerate that info.  So now
     we clear the next/prev slots when deleting the window, so that we
     are able to collect more actual garbage.

     We also don't need the hchild or vchild slots in the window
     config code. -cet
   */
  p->next = Qnil;
  p->prev = Qnil;
  p->hchild = Qnil;
  p->vchild = Qnil;

  /* If parent now has only one child,
     put the child into the parent's place.  */

  tem = par->hchild;
  if (NILP (tem))
    tem = par->vchild;
  if (NILP (XWINDOW (tem)->next))
    {
      replace_window (parent, tem);

      /* Also clear the hchild/vchild slots of the now-deleted parent,
	 for GC purposes (see comment above).
       */
      XWINDOW (parent)->hchild = Qnil;
      XWINDOW (parent)->vchild = Qnil;
    }

  XSCREEN (p->screen)->mirror_dirty = 1;
  return Qnil;
}

/*
 * Used by the new redisplay.  return 1 if W is found in window
 * hierarchy WIN.
*/
int
find_window (struct window *w, Lisp_Object win)
{
  int i;
  for (; !NILP(win) && !EQ(win,XSCREEN(XWINDOW(win)->screen)->minibuffer_window)
       ; win = XWINDOW(win)->next)

    {
      if (w == XWINDOW(win)) return 1;
      if (!NILP(XWINDOW(win)->vchild) &&
	  (i = find_window(w,XWINDOW(win)->vchild)))
	return i;
      if (!NILP(XWINDOW(win)->hchild) &&
	  (i = find_window(w,XWINDOW(win)->hchild)))
	return i;
    }
  return 0;
}

struct window *
find_window_by_pixel_pos (unsigned int pix_x, unsigned int pix_y, Lisp_Object win)
{
  struct window *w;

  if (NILP(win))
    return 0;

  for (; !NILP(win); win = XWINDOW(win)->next)
    {
      if (!NILP(XWINDOW(win)->vchild) &&
	  (w = find_window_by_pixel_pos(pix_x,pix_y,XWINDOW(win)->vchild)))
	return w;
      if (!NILP(XWINDOW(win)->hchild) &&
	  (w = find_window_by_pixel_pos(pix_x,pix_y,XWINDOW(win)->hchild)))
	return w;
      w = XWINDOW(win);
      if (pix_x >= w->pixleft
	  && pix_x <= (w->pixleft + w->pixwidth)
	  && pix_y >= w->pixtop
	  && pix_y <= (w->pixtop + w->pixheight))
	return w;
    }
  return 0;
}


#ifdef MULTI_SCREEN
Lisp_Object
next_screen_window (screen, window, mini)
     Lisp_Object window, mini;
     struct screen *screen;
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

DEFUN ("next-window", Fnext_window, Snext_window, 0, 4, 0,
  "Return next window after WINDOW in canonical ordering of windows.\n\
Optional second arg MINIBUF t means count the minibuffer window\n\
even if not active.  If MINIBUF is neither t nor nil it means\n\
not to count the minibuffer even if it is active.\n\
Optional third arg ALL-SCREENS t means include all windows in all visible\n\
screens; otherwise cycle within the selected screen, with the exception that\n\
if a global minibuffer screen is in use, all screens are used.\n\
Optional fourth argument INVISIBLE-TOO t means also visit invisible screens.")
  (window, mini, all_screens, invisible_too)
     register Lisp_Object window, mini, all_screens, invisible_too;
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
	      tem = next_screen (tem,
				 (NILP (mini) ? 0 : 1),
				 (NILP (invisible_too) ? 1 : 0));
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

DEFUN ("previous-window", Fprevious_window, Sprevious_window, 0, 4, 0,
  "Return previous window before WINDOW in canonical ordering of windows.\n\
Optional second arg MINIBUF t means count the minibuffer window\n\
even if not active.  If MINIBUF is neither t nor nil it means\n\
not to count the minibuffer even if it is active.\n\
Optional third arg ALL-SCREENS t means include all windows in all visible\n\
screens; otherwise cycle within the selected screen, with the exception\n\
that if a global minibuffer screen is in use, all visible screens are used.\n\
If optional fourth argument INVISIBLE-TOO is t also visit invisible screens.")
  (window, mini, all_screens, invisible_too)
     register Lisp_Object window, mini, all_screens, invisible_too;
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
	      tem = prev_screen (tem,
				 (NILP (mini) ? 0 : 1),
				 (NILP (invisible_too) ? 1 : 0));
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

DEFUN ("next-vertical-window", Fnext_vertical_window, Snext_vertical_window,
       0, 1, 0,
  "Return the next window which is vertically after WINDOW.\n")
  (window)
    Lisp_Object window;
{
  Lisp_Object root;

  if (NILP (window))
    window = selected_window;
  else
    CHECK_WINDOW (window, 0);

  if (MINI_WINDOW_P (XWINDOW (window)))
    return Qnil;

  root = SCREEN_ROOT_WINDOW (XSCREEN (WINDOW_SCREEN (XWINDOW (window))));

  if (EQ (window, root))
    {
      while (1)
	if (!NILP (XWINDOW (window)->hchild))
	  window = XWINDOW (window)->hchild;
	else if (!NILP (XWINDOW (window)->vchild))
	  window = XWINDOW (window)->vchild;
	else
	  return window;
    }

  do
    {
      if (!NILP (XWINDOW (window)->parent) &&
	  !NILP (XWINDOW (XWINDOW (window)->parent)->vchild))
	{
	  if (!NILP (XWINDOW (window)->next))
	    return XWINDOW (window)->next;
	  else
	    window = XWINDOW (window)->parent;
	}
      else
	window = XWINDOW (window)->parent;
    }
  while (!EQ (window, root));

  while (1)
    if (!NILP (XWINDOW (window)->hchild))
      window = XWINDOW (window)->hchild;
    else if (!NILP (XWINDOW (window)->vchild))
      window = XWINDOW (window)->vchild;
    else
      return window;

}


DEFUN ("other-window", Fother_window, Sother_window, 1, 3, "p",
  "Select the ARG'th different window on this screen.\n\
All windows on current screen are arranged in a cyclic order.\n\
This command selects the window ARG steps away in that order.\n\
A negative ARG moves in the opposite order.  If the optional second\n\
argument ALL-SCREENS is non-nil, cycle through all visible screens.\n\
If optional third argument INVISIBLE-TOO is t also search invisible screens.")
  (n, all_screens, invisible_too)
     register Lisp_Object n, all_screens, invisible_too;
{
  register int i;
  register Lisp_Object w;

  CHECK_FIXNUM (n, 0);
  w = selected_window;
  i = XINT (n);

  while (i > 0)
    {
      w = Fnext_window (w, Qnil, all_screens, invisible_too);
      i--;
    }
  while (i < 0)
    {
      w = Fprevious_window (w, Qnil, all_screens, invisible_too);
      i++;
    }
  Fselect_window (w);
  return Qnil;
}

/* Look at all windows, performing an operation specified by TYPE
   with argument OBJ.

   If SCREENS is Qt, look at all screens, if Qnil, look at just the selected
   screen.  If SCREENS is a screen, just look at windows on that screen.
   If MINI is non-zero, perform the operation on minibuffer windows too.
   If FORCE it 1 and SCREENS is Qt also look at the invisible screens.
*/

enum window_loop
{
  WINDOW_LOOP_UNUSED,
  GET_BUFFER_WINDOW,		/* Arg is buffer */
  GET_LRU_WINDOW,		/* Arg is t for full-width windows only */
  DELETE_OTHER_WINDOWS,		/* Arg is window not to delete */
  DELETE_BUFFER_WINDOWS,	/* Arg is buffer */
  GET_LARGEST_WINDOW,
  UNSHOW_BUFFER,		/* Arg is buffer */
  GET_BUFFER_WINDOW_COUNT,	/* Arg is buffer */
  GET_BUFFER_MRU_WINDOW		/* Arg is buffer */
};

static Lisp_Object
window_loop (enum window_loop type,
             Lisp_Object obj, 
             int mini,
             Lisp_Object this_screen,
             int force,
	     int dedicated_too)
{
  register Lisp_Object w;
  register Lisp_Object best_window;
  register Lisp_Object last_window;
  SCREEN_PTR screen;
  int count = 0;		/* for GET_BUFFER_WINDOW_COUNT */
  /* >>> I think the change of "precomputing" last_window and next_window
   * >>>  catch the lossage this is meant(?) to punt on...
   */
  int lose_lose = 0; 

  if (NILP (this_screen) || EQ (this_screen, Qt) || !SCREENP (this_screen))
    screen = selected_screen;
  else
    screen = XSCREEN (this_screen);

  /* Pick a window to start with.  */
  if (WINDOWP (obj))
    w = obj;
  else
    w = SCREEN_SELECTED_WINDOW (screen);

  if (MINI_WINDOW_P (XWINDOW (w)))
    mini = 1;

  {
    /* Figure out the last window we're going to mess with.  
       We can't just wait until we hit the first window again, because
       it might be deleted.  */
    Lisp_Object tem = w;
    Lisp_Object stop_point = w;
    int stop_point_visible = SCREEN_VISIBLE_P(screen);
    
    do
      {
	last_window = tem;
	if (! stop_point_visible && ! force
	    && SCREEN_VISIBLE_P(XSCREEN(WINDOW_SCREEN(XWINDOW(tem)))))
	  {
	    stop_point = tem;
	    stop_point_visible = 1;
	  }
	tem = ((EQ (this_screen, Qt))
	       ? Fnext_window (tem, 
			       ((mini) ? Qt : Qnil), 
			       Qt,
			       ((force) ? Qt : Qnil))
	       : next_screen_window (screen, tem, ((mini) ? Qt : Qnil)));
      } while (!EQ (tem, stop_point));
  }

  best_window = Qnil;

  while (1)
    {
      struct window *p = XWINDOW (w);
      Lisp_Object next_window;
      /* Pick the next window now, since some operations will delete
	 the current window.  */
      if (EQ (this_screen, Qt))
	next_window = Fnext_window (w,
                                    ((mini) ? Qt : Qnil), 
                                    Qt,
                                    ((force) ? Qt : Qnil));
      else
	next_window = next_screen_window (screen, w, ((mini) ? Qt : Qnil));

      /* >>> Still needed ?? */
      /* Given the outstanding quality of the rest of this code, 
	 I feel no shame about putting this piece of shit in. */
      if (++lose_lose >= 500)
	return Qnil;

      if (!MINI_WINDOW_P (p))
        {
	switch (type)
	  {
	  case GET_BUFFER_WINDOW:
            {
#if 0
              /* Ignore invisible and iconified screens.  */
              if (! SCREEN_VISIBLE_P (XSCREEN (WINDOW_SCREEN (p)))
                  || SCREEN_ICONIFIED_P (XSCREEN (WINDOW_SCREEN (p))))
                break;
#endif
              if (XBUFFER (p->buffer) == XBUFFER (obj))
                return w;
              break;
            }

	  case GET_BUFFER_WINDOW_COUNT:
	    {
	      if (XBUFFER (p->buffer) == XBUFFER (obj))
		count++;
	      break;
	    }

	  case GET_LRU_WINDOW:
	    {
	      /* t as arg means consider only full-width windows */
	      if (!NILP (obj)
		  && window_char_width (p) != SCREEN_WIDTH (screen))
		break;

#if 0
              /* Ignore invisible and iconified screens.  */
              if (! SCREEN_VISIBLE_P (XSCREEN (WINDOW_SCREEN (p)))
                  || SCREEN_ICONIFIED_P (XSCREEN (WINDOW_SCREEN (p))))
                break;
#endif
	      /* Ignore dedicated windows and minibuffers.  */
	      if (MINI_WINDOW_P (p)
		  || (dedicated_too ? 0 : !NILP (p->dedicated)))
		break;
	      if (NILP (best_window)
		  || (XINT (XWINDOW (best_window)->use_time)
                      > XINT (p->use_time)))
		best_window = w;
	      break;
	    }

	  case GET_BUFFER_MRU_WINDOW:
	    {
	      /* Ignore dedicated windows and minibuffers. */
	      if (MINI_WINDOW_P (p)
		  || (dedicated_too ? 0 : !NILP (p->dedicated)))
		break;

	      if (XBUFFER (p->buffer) == XBUFFER (obj))
		{
		  if (NILP (best_window)
		      || (XINT (XWINDOW (best_window)->use_time)
			  < XINT (p->use_time)))
		    best_window = w;
		}
	      break;
	    }

	  case DELETE_OTHER_WINDOWS:
	    {
	      /* Don't delete the last window on a screen; this can happen
		 when the minibuffer is selected, and would cause the screen
		 to be deleted. */
	      if (p != XWINDOW (obj) && !ONLY_WINDOW_P (XWINDOW (w)))
		Fdelete_window (w);
	      break;
	    }

	  case DELETE_BUFFER_WINDOWS:
	    {
	      if (EQ (p->buffer, obj))
		{
		  /* If we're deleting the buffer displayed in the only window
		   on the screen, find a new buffer to display there.  */
		  if (NILP (p->parent))
		    {
		      Lisp_Object new_buffer = Fother_buffer (obj, Qnil);
		      if (NILP (new_buffer))
			new_buffer = Fget_buffer_create (QSscratch);
		      Fset_window_buffer (w, new_buffer);
		      Fset_buffer (p->buffer);
		    }
		  else
		    Fdelete_window (w);
		}
	      break;
	    }

	  case GET_LARGEST_WINDOW:
	    {
#if 0
              /* Ignore invisible and iconified screens.  */
              if (! SCREEN_VISIBLE_P (XSCREEN (WINDOW_SCREEN (p)))
                  || SCREEN_ICONIFIED_P (XSCREEN (WINDOW_SCREEN (p))))
                break;
#endif
	      /* Ignore dedicated windows and minibuffers.  */
	      if (MINI_WINDOW_P (p)
		  || (dedicated_too ? 0 : !NILP (p->dedicated)))
		break;
	      {
                struct window *b = XWINDOW (best_window);
                if (NILP (best_window)
                    || ((p->pixheight * p->pixwidth)
                        > (b->pixheight) * b->pixwidth))
                  best_window = w;
              }
	      break;
            }

	  case UNSHOW_BUFFER:
            {
              if (EQ (p->buffer, obj))
		{
		  /* Find another buffer to show in this window.  */
		  Lisp_Object another_buffer = Fother_buffer (obj, Qnil);
		  if (NILP (another_buffer))
		    another_buffer
		      = Fget_buffer_create (QSscratch);
		  Fset_window_buffer (w, another_buffer);
		  if (EQ (w, selected_window))
		    Fset_buffer (p->buffer);
		}
              break;
            }

	  default: 
	    abort ();
	  }
        }
      if (EQ (w, last_window))
	break;

      w = next_window;
    }

  if (type == GET_BUFFER_WINDOW_COUNT)
    return (make_number (count));
  else
    return (best_window);
}

/* only display_glyph_in_buffer in xdisp.c is using this at the moment */
int
buffer_window_count (struct buffer *b, struct screen *s)
{
  Lisp_Object buffer, screen;

  XSETR (screen, Lisp_Screen, s);
  XSETR (buffer, Lisp_Buffer, b);

  return XINT (window_loop (GET_BUFFER_WINDOW_COUNT, buffer, 0, screen, 0, 1));
}

/* only display_glyph_in_buffer in xdisp.c is using this at the moment */
int
buffer_window_mru (struct window *w)
{
  Lisp_Object window = 
    window_loop (GET_BUFFER_MRU_WINDOW, w->buffer, 0, w->screen, 0, 1);

  if (NILP (window))
    return 0;
  else if (XWINDOW (window) == w)
    return 1;
  else
    return 0;
}


DEFUN ("get-lru-window", Fget_lru_window, Sget_lru_window, 0, 1, 0,
  "Return the window least recently selected or used for display.\n\
If optional argument SCREEN is non-nil, search only that screen.")
  (screen)
    Lisp_Object screen;
{
  register Lisp_Object w;
  if (!NILP (screen))
    CHECK_LIVE_SCREEN (screen, 0);
  /* First try for a non-dedicated window that is full-width */
  w = window_loop (GET_LRU_WINDOW, Qt, 0, screen, 0, 0);
  if (!NILP (w) && !EQ (w, selected_window))
    return w;

  /* then try for a dedicated window that is full-width */
  w = window_loop (GET_LRU_WINDOW, Qt, 0, screen, 0, 1);
  if (!NILP (w) && !EQ (w, selected_window))
    return w;

  /* If none of them, then all windows, dedicated or not. */
  w = window_loop (GET_LRU_WINDOW, Qnil, 0, screen, 0, 1);

  /* At this point we damn well better have found something. */
  if (NILP (w)) abort ();

  return (w);
}

DEFUN ("get-largest-window", Fget_largest_window, Sget_largest_window, 0, 1, 0,
  "Return the window largest in area.\n\
If optional argument SCREEN is non-nil, search only that screen.")
  (screen)
    Lisp_Object screen;
{
  /* Search dedicated windows too (is this sensible?) */
  return window_loop (GET_LARGEST_WINDOW, Qnil, 0, screen, 0, 1);
}

DEFUN ("get-buffer-window", Fget_buffer_window, Sget_buffer_window, 1, 3, 0,
  "Return a window currently displaying BUFFER, or nil if none.\n\
If optional argument SCREEN is t, search all visible screens.\n\
If SCREEN is a screen, search only that screen.\n\
If INVISIBLE-TOO is t invisible screens are searched too.")
  (buffer, screen, invisible_too)
    Lisp_Object buffer, screen, invisible_too;
{
  buffer = Fget_buffer (buffer);
  if (!NILP (screen) && !EQ (screen, Qt))
    CHECK_SCREEN (screen, 0);
  if (BUFFERP (buffer))
    /* Search dedicated windows too. */
    return window_loop (GET_BUFFER_WINDOW, buffer, 1,
			screen,
			(NILP (invisible_too) ? 0 : 1),
			1);
  else
    return Qnil;
}

/*
 * These are orphaned functions at the moment.  They don't have a good
 * home.  This will change with future redisplay work to allow display
 * on tty's.  In the meantime this is a little better than buffer.c
 * because needed header files are already here.
 */

DEFUN ("buffer-left-margin-pixwidth", Fbuffer_left_margin_pixwidth,
       Sbuffer_left_margin_pixwidth, 0, 1, 0,
  "Return the width in pixels of the left outside margin of\n\
buffer BUFFER.  If BUFFER is nil, the current buffer is assumed.")
  (buffer)
    Lisp_Object buffer;
{
  struct buffer *buf;

  if (NILP (buffer))
    buf = current_buffer;
  else
    {
      CHECK_BUFFER (buffer, 0);
      buf = XBUFFER(buffer);
    }

  return (make_number (LEFT_MARGIN (buf,selected_screen,
				   XWINDOW (selected_window))));
}

DEFUN ("buffer-right-margin-pixwidth", Fbuffer_right_margin_pixwidth,
       Sbuffer_right_margin_pixwidth, 0, 1, 0,
  "Return the width in pixels of the right outside margin of\n\
buffer BUFFER.  If BUFFER is nil, the current buffer is assumed.")
  (buffer)
    Lisp_Object buffer;
{
  struct buffer *buf;

  if (NILP (buffer))
    buf = current_buffer;
  else
    {
      CHECK_BUFFER (buffer, 0);
      buf = XBUFFER(buffer);
    }

  return (make_number (RIGHT_MARGIN(buf, selected_screen,
				    XWINDOW (selected_window))));
}


DEFUN ("delete-other-windows", Fdelete_other_windows, Sdelete_other_windows,
  0, 1, "",
  "Make WINDOW (or the selected window) fill its screen.\n\
Only the screen WINDOW is on is affected.")
  (window)
     Lisp_Object window;
{
  struct window *w;
  int opoint = PT;
  struct buffer *obuf = current_buffer;
  int top;
  struct Lisp_Font *font;

  if (NILP (window))
    window = selected_window;
  else
    CHECK_WINDOW (window, 0);

  w = XWINDOW (window);
  font = XFONT (SCREEN_DEFAULT_FONT (XSCREEN(w->screen)));
  top = w->pixtop / font->height;

  {
    struct buffer *owbuf = XBUFFER (w->buffer);
    int owpoint = BUF_PT (owbuf);
    int start;

    /* Ignore dedicated windows. */
    window_loop (DELETE_OTHER_WINDOWS, window, 0, WINDOW_SCREEN (w), 0, 0);

    Fset_buffer (w->buffer); /* owbuf */

    start = marker_position (w->start);

    if (start < BEGV)
      start = BEGV;
    else if (start > ZV)
      start = ZV;

    SET_PT (start);
    Fvertical_motion (make_number (-top), window);
    Fset_marker (w->start, make_number (PT), w->buffer);
    w->start_at_line_beg = !NILP (Fbolp ());
    SET_PT (owpoint);
  }

  set_buffer_internal (obuf);
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
      /* Ignore dedicated windows. */
      window_loop (DELETE_BUFFER_WINDOWS, buffer, 0, Qt, 1, 0);
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
      /* Ignore dedicated windows. */
      window_loop (UNSHOW_BUFFER, buffer, 0, Qt, 1, 0);
    }
  return Qnil;
}

/* The smallest acceptable dimensions for a window.  Anything smaller
   might crash Emacs.  */
#define MIN_SAFE_WINDOW_WIDTH  (2)
#define MIN_SAFE_WINDOW_HEIGHT (2)

/* Make sure that window_min_height and window_min_width are
   not too small; if they are, set them to safe minima.  */

static void
check_min_window_sizes ()
{
  /* Smaller values might permit a crash.  */
  if (window_min_width < MIN_SAFE_WINDOW_WIDTH)
    window_min_width = MIN_SAFE_WINDOW_WIDTH;
  if (window_min_height < MIN_SAFE_WINDOW_HEIGHT)
    window_min_height = MIN_SAFE_WINDOW_HEIGHT;
}

/* If *ROWS or *COLS are too small a size for SCREEN, set them to the
   minimum allowable size.  */
void
check_screen_size (SCREEN_PTR screen, int *rows, int *cols)
{
  /* For height, we have to see whether the screen has a minibuffer, and
     whether it wants a mode line.  */
  int min_height = ((SCREEN_MINIBUF_ONLY_P (screen)
                     || ! SCREEN_HAS_MINIBUF_P (screen))
                    ? MIN_SAFE_WINDOW_HEIGHT
                    : 2 * MIN_SAFE_WINDOW_HEIGHT - 1);

  if (*rows < min_height)
    *rows = min_height;
  if (*cols  < MIN_SAFE_WINDOW_WIDTH)
    *cols = MIN_SAFE_WINDOW_WIDTH;
}

/* Set the height of WINDOW and all its inferiors.  */
/* Normally the window is deleted if it gets too small.
   nodelete nonzero means do not do this.
   (The caller should check later and do so if appropriate)  */

void
set_window_height (window, height, nodelete)
     Lisp_Object window;
     int height;
     int nodelete;
{
  register struct window *w = XWINDOW (window);
  struct screen *s = XSCREEN (w->screen);
  register struct window *c;
  int oheight = w->pixheight;
  int top, pos, lastbot, opos, lastobot;
  Lisp_Object child;
  struct Lisp_Font *font = XFONT (SCREEN_DEFAULT_FONT (s));

  check_min_window_sizes ();

  if (!nodelete
      && ! ONLY_WINDOW_P (w)
      && height < (window_min_height * font->height))
    {
      Fdelete_window (window);
      return;
    }

  w->size_change = 1;
  w->last_modified = Qzero;
  w->last_facechange = Qzero;
  windows_changed++;
  w->pixheight = height;
  if (!NILP (w->hchild))
    {
      for (child = w->hchild; !NILP (child); child = XWINDOW (child)->next)
	{
	  XWINDOW (child)->pixtop = w->pixtop;
	  set_window_height (child, height, nodelete);
	}
    }
  else if (!NILP (w->vchild))
    {
      lastbot = top = w->pixtop;
      lastobot = 0;
      for (child = w->vchild; !NILP (child); child = c->next)
	{
	  c = XWINDOW (child);

	  opos = lastobot + c->pixheight;

	  c->pixtop = lastbot;

	  pos = (((opos * height) << 1) + oheight) / (oheight << 1);

	  /* Avoid confusion: inhibit deletion of child if becomes too small */
	  set_window_height (child, pos + top - lastbot, 1 | nodelete);

	  /* Now advance child to next window,
	     and set lastbot if child was not just deleted.  */
	  lastbot = pos + top;
	  lastobot = opos;
	}
      /* Now delete any children that became too small.  */
      if (!nodelete)
	for (child = w->vchild; !NILP (child); child = XWINDOW (child)->next)
	  {
	    set_window_height (child, XWINDOW (child)->pixheight, 0);
	  }
    }
}

/* Recursively set width of WINDOW and its inferiors. */

void
set_window_width (window, width, nodelete)
     Lisp_Object window;
     int width;
     int nodelete;
{
  register struct window *w = XWINDOW (window);
  struct screen *s = XSCREEN (w->screen);
  register struct window *c;
  int owidth = w->pixwidth;
  int left, pos, lastright, opos, lastoright;
  Lisp_Object child;

  struct Lisp_Font *font = XFONT (SCREEN_DEFAULT_FONT (s));

  if (!nodelete
      && ! ONLY_WINDOW_P (w)
      && width < (window_min_width * font->width))
    {
      Fdelete_window (window);
      return;
    }

  w->size_change = 1;
  w->last_modified = Qzero;
  w->last_facechange = Qzero;
  windows_changed++;
  w->pixwidth = width;
  if (!NILP (w->vchild))
    {
      for (child = w->vchild; !NILP (child); child = XWINDOW (child)->next)
	{
	  XWINDOW (child)->pixleft = w->pixleft;
	  set_window_width (child, width, nodelete);
	}
    }
  else if (!NILP (w->hchild))
    {
      lastright = left = w->pixleft;
      lastoright = 0;
      for (child = w->hchild; !NILP (child); child = c->next)
	{
	  c = XWINDOW (child);

	  opos = lastoright + c->pixwidth;

	  c->pixleft = lastright;

	  pos = (((opos * width) << 1) + owidth) / (owidth << 1);

	  /* Inhibit deletion for becoming too small */
	  set_window_width (child, pos + left - lastright, 1 | nodelete);

	  /* Now advance child to next window,
	     and set lastright if child was not just deleted.  */
	  lastright = pos + left, lastoright = opos;
	}
      /* Delete children that became too small */
      if (!nodelete)
	for (child = w->hchild; !NILP (child); child = XWINDOW (child)->next)
	  {
	    set_window_width (child, XWINDOW (child)->pixwidth, 0);
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
    error (GETTEXT ("Attempt to display deleted buffer"));

  tem = w->buffer;
  if (NILP (tem))
    error (GETTEXT ("Window is deleted"));

  /* While this seems like a logical thing to do, it causes problems
     because of saved window configurations.  It is possible for a
     buffer to get restored into a window in which it is already being
     displayed, but start and point are actually at completely
     different locations.  So we let this function complete fully and
     it will then make sure redisplay correctly updates things. */
#if 0
  else if (EQ (tem, buffer))
    return (Qnil);
#endif
  else if (! EQ (tem, Qt))	/* w->buffer is t when the window
				   is first being set up.  */
    {
      if (!NILP (w->dedicated) && !EQ (tem, buffer))
	error (GETTEXT ("Window is dedicated to buffer %s"),
	       XSTRING (XBUFFER (tem)->name)->data);

      unshow_buffer (w);
    }

  w->buffer = buffer;
  w->window_end_pos = 0;
  w->window_end_valid = 0;
  w->hscroll = Qzero;
  Fset_marker (w->pointm,
	       make_number (BUF_PT (XBUFFER (buffer))),
	       buffer);
  set_marker_restricted (w->start,
			 make_number (XBUFFER (buffer)->last_window_start),
			 buffer);
  Fset_marker (w->sb_point, w->start, buffer);
  w->start_at_line_beg = 0;
  w->force_start = 0;           /* Lucid fix */
  w->last_modified = Qzero;
  w->last_facechange = Qzero;
  windows_changed++;
  if (EQ (window, selected_window))
    {
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
  
  CHECK_WINDOW (window, 0);

  w = XWINDOW (window);

  if (NILP (w->buffer) ||  /* no longer enough - see Fdelete_window() */
      NILP (w->start) || NILP (w->pointm) ||
      XMARKER (w->start)->buffer == 0 ||
      XMARKER (w->pointm)->buffer == 0)
    error (GETTEXT ("Trying to select deleted window or non-leaf window"));

  w->use_time = make_number (++window_select_count);
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
      Fselect_screen (WINDOW_SCREEN (w));
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
  {
    register int new_point = marker_position (w->pointm);
    if (new_point < BEGV)
      new_point = BEGV;
    else if (new_point > ZV)
      new_point = ZV;

    SET_PT (new_point);
  }

  windows_changed++;

#ifdef ENERGIZE
  if (! MINI_WINDOW_P (w))
    energize_window_selected_hook (w);
#endif

  return window;
}

DEFUN ("display-buffer", Fdisplay_buffer, Sdisplay_buffer, 1, 3,
       "BDisplay buffer:\nP",
  "Make BUFFER appear in some window on the current screen, but don't\n\
select it.\n\
Arguments are BUFFER &optional NOT-THIS-WINDOW-P, OVERRIDE-SCREEN.\n\
BUFFER can be a buffer or a buffer name.\n\
If BUFFER is shown already in some window in the current screen,\n\
just uses that one, unless the window is the selected window and\n\
NOT-THIS-WINDOW-P is non-nil.\n\
\n\
If BUFFER has a dedicated screen, display on that screen instead of\n\
the current screen, unless OVERRIDE-SCREEN is non-nil.\n\
\n\
If OVERRIDE-SCREEN is non-nil, display on that screen instead of\n\
the current screen (or the dedicated screen).\n\
\n\
If `pop-up-windows' is non-nil, always use the\n\
current screen and create a new window regardless of whether the\n\
buffer has a dedicated screen, and regardless of whether\n\
OVERRIDE-SCREEN was specified.\n\
\n\
Returns the window displaying BUFFER.")
  (buffer, not_this_window_p, override_screen)
  Lisp_Object buffer, not_this_window_p, override_screen;
{
  Lisp_Object window;
  Lisp_Object old_screen, target_screen;
#ifdef MULTI_SCREEN
# if 0
  /* if global-minibuffer-mode, this is the screen which invoked the minibuf */
  if (active_screen)
    XSETR (old_screen, Lisp_Screen, active_screen);
  else
# endif /* 0 */
    XSETR (old_screen, Lisp_Screen, selected_screen);
#endif

  buffer = Fget_buffer (buffer);
  CHECK_BUFFER (buffer, 0);

  target_screen = old_screen;

  if (!NILP (Vpre_display_buffer_function))
    {
      struct gcpro gcpro1, gcpro2, gcpro3;
      /* No need to gcpro target_screen, since GC doesn't relocate */
      GCPRO3 (buffer, not_this_window_p, override_screen);
      call3 (Vpre_display_buffer_function,
	     buffer, not_this_window_p, override_screen);
      UNGCPRO;
      /* this hook may have selected some other screen, so take notice. */
      XSETR (target_screen, Lisp_Screen, selected_screen);
    }

  /* Give the user the ability to completely reimplement this function in
     lisp via the `display-buffer-function'.
   */
  if (!NILP (Vdisplay_buffer_function))
    {
      struct gcpro gcpro1, gcpro2, gcpro3;
      GCPRO3 (buffer, not_this_window_p, override_screen);
      RETURN_UNGCPRO (call3 (Vdisplay_buffer_function,
                             buffer, not_this_window_p, override_screen));
    }

  /* If the buffer has a dedicated screen, that takes precedence over the
     current screen, and over what the pre-display-buffer-function did.
   */
  if (!NILP (XBUFFER (buffer)->dedicated_screen))
    target_screen = XBUFFER (buffer)->dedicated_screen;

  /* if override_screen is supplied, that takes precedence over everything.
     (This is gonna look bad if the pre-display-buffer-function raised
     some other screen already.)
   */
  if (!NILP (override_screen))
    target_screen = override_screen;

  CHECK_SCREEN (target_screen, 0);

  /* If we have switched screens, then set not_this_window_p to false.
     Switching screens means that selected_window is no longer the same
     as it was on entry -- it's the selected_window of target_screen
     instead of old_screen, so it's a fine candidate for display. */
  if (!EQ (old_screen, target_screen))
    not_this_window_p = Qnil;

  /* if it's in the selected window, and that's ok, then we're done. */
  if (NILP (not_this_window_p)
      && XBUFFER (XWINDOW (selected_window)->buffer) == XBUFFER (buffer))
    return selected_window;

  /* Otherwise, find some window that it's already in, and return that,
     unless that window is the selected window and that isn't ok.
     What a contorted mess! */
  window = Fget_buffer_window (buffer, target_screen, Qnil);
  if (!NILP (window)
      && (NILP (not_this_window_p) || !EQ (window, selected_window)))
    return window;

#if 0
  /* If there are no screens open that have more than a minibuffer,
     we need to create a new screen.  */
  if (pop_up_screens || last_nonminibuf_screen == 0)
    {
      window
	= Fscreen_selected_window (call0 (Vpop_up_screen_function));
      Fset_window_buffer (window, buffer);
# if 0
      Fselect_screen (XWINDOW (window)->screen, Qnil);
# endif
      return window;
    }
#endif

  /* Otherwise, make it be in some window, splitting if appropriate/possible.
     Do not split a window if we are displaying the buffer in a different
     screen than that which was current when we were called.  (It is already
     in a different window by virtue of being in another screen.)
   */
  if ((pop_up_windows
       && EQ (target_screen, old_screen))
#ifdef MULTI_SCREEN
      || SCREEN_MINIBUF_ONLY_P (selected_screen)
#endif
      )
    {
#if 0
      if (SCREEN_MINIBUF_ONLY_P (selected_screen))
	XSETR (target_screen, Lisp_Screen, last_nonminibuf_screen);
#endif
      /* Don't try to create a window if would get an error */
      if (split_height_threshold < window_min_height << 1)
	split_height_threshold = window_min_height << 1;

      window = Fget_largest_window (target_screen);

      if (!NILP (window)
	  && window_char_height (XWINDOW (window)) >= split_height_threshold
	  &&
	  (window_char_width (XWINDOW (window))
	   == SCREEN_WIDTH (XSCREEN (WINDOW_SCREEN (XWINDOW (window))))))
	window = Fsplit_window (window, Qnil, Qnil);
      else
	{
	  window = Fget_lru_window (target_screen);
	  if ((EQ (window, selected_window)
	       || EQ (XWINDOW (window)->parent, Qnil))
	      && window_char_height (XWINDOW (window))
	      >= (window_min_height << 1))
	    window = Fsplit_window (window, Qnil, Qnil);
	}
    }
  else
    window = Fget_lru_window (target_screen);

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
  set_buffer_internal (old);

  if (!EQ (Vtemp_buffer_show_function, Qnil))
    call1 (Vtemp_buffer_show_function, buf);
  else
    {
      window = Fdisplay_buffer (buf, Qnil, same_screen);

#ifdef MULTI_SCREEN
      if (XSCREEN (XWINDOW (window)->screen) != selected_screen)
	Fmake_screen_visible (WINDOW_SCREEN (XWINDOW (window)));
#endif /* MULTI_SCREEN */
      Vminibuf_scroll_window = window;
      w = XWINDOW (window);
      w->hscroll = Qzero;
      set_marker_restricted (w->start, make_number (1), buf);
      set_marker_restricted (w->pointm, make_number (1), buf);
      set_marker_restricted (w->sb_point, make_number (1), buf);
    }
}

static void
make_dummy_parent (window)
     Lisp_Object window;
{
  Lisp_Object new;
  register struct window *o = XWINDOW (window);
  register struct window *p = alloc_lcrecord (sizeof (struct window), 
                                              lrecord_window);
  struct lcrecord_header preserve_me;

  XSETR (new, Lisp_Window, p);
  preserve_me = p->header;
  memcpy (p, o, sizeof (struct window));
  p->header = preserve_me;

  /* Put new into window structure in place of window */
  replace_window (window, new);

  o->next = Qnil;
  o->prev = Qnil;
  o->vchild = Qnil;
  o->hchild = Qnil;
  o->parent = new;

  p->start = Qnil;
  p->pointm = Qnil;
  p->sb_point = Qnil;
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
  int size;
  int psize;
  struct screen *s;
  struct Lisp_Font *font;

  if (NILP (window))
    window = selected_window;
  else
    CHECK_WINDOW (window, 0);

  o = XWINDOW (window);
  s = XSCREEN (o->screen);
  font = XFONT (SCREEN_DEFAULT_FONT (s));

  if (NILP (chsize))
    {
      if (!NILP (horflag))
	/* Round odd size up, since this is for the left-hand window,
	   and it will lose a column for the separators.  */
	{
          size = (((o->pixwidth + 1) / font->width) & -2) >> 1;
	  psize = size * font->width;
        }
      else
        {
	  size = (o->pixheight / font->height) >> 1;
	  psize = size * font->height;
        }
    }
  else
    {
      CHECK_FIXNUM (chsize, 1);
      size = XINT (chsize);
      psize = size * ((NILP (horflag)) 
                      ? font->height
                      : font->width);
    }

  if (MINI_WINDOW_P (o))
    error (GETTEXT ("Attempt to split minibuffer window"));
  else if (SCREEN_NO_SPLIT_P (XSCREEN (WINDOW_SCREEN (o))))
    error (GETTEXT ("Attempt to split unsplittable screen"));

  check_min_window_sizes ();

  if (NILP (horflag))
    {
      if (size < window_min_height
	  || size + window_min_height > window_char_height (o))
/*	args_out_of_range_3 (window, chsize, horflag); */
	error ("Window is not tall enough to split");
      if (NILP (o->parent)
	  || NILP (XWINDOW (o->parent)->vchild))
	{
	  make_dummy_parent (window);
	  new = o->parent;
	  XWINDOW (new)->vchild = window;
	  XSCREEN (o->screen)->mirror_dirty = 1;
	}
    }
  else
    {
      if (size < window_min_width
	  || size + window_min_width > window_char_width (o))
/*	args_out_of_range_3 (window, chsize, horflag); */
	error ("Window is not wide enough to split");
      if (NILP (o->parent)
	  || NILP (XWINDOW (o->parent)->hchild))
	{
	  make_dummy_parent (window);
	  new = o->parent;
	  XWINDOW (new)->hchild = window;
	  XSCREEN (o->screen)->mirror_dirty = 1;
	}
    }

  /* Now we know that window's parent is a vertical combination
     if we are dividing vertically, or a horizontal combination
     if we are making side-by-side windows */

  windows_changed++;
  new = make_window ();
  p = XWINDOW (new);
  p->size_change = 1;

  p->screen = o->screen;
  p->next = o->next;
  if (!NILP (p->next))
    XWINDOW (p->next)->prev = new;
  p->prev = window;
  o->next = new;
  p->parent = o->parent;
  p->buffer = Qt;
  p->used_height = o->used_height = 0;
  p->window_end_valid = 0;
  o->window_end_valid = 0;

  Fset_window_buffer (new, o->buffer);

  /* Apportion the available screen space among the two new windows */

  if (!NILP (horflag))
    {
      p->pixheight = o->pixheight;
      p->pixtop = o->pixtop;
      p->pixwidth = o->pixwidth - psize;
      o->pixwidth = psize;
      p->pixleft = o->pixleft + psize;
    }
  else
    {
      p->pixleft = o->pixleft;
      p->pixwidth = o->pixwidth;
      p->pixheight = o->pixheight - psize;
      o->pixheight = psize;
      p->pixtop = o->pixtop + psize;
    }

  XSCREEN (p->screen)->mirror_dirty = 1;
  return new;
}

static void change_window_height (int delta, int widthflag);

DEFUN ("enlarge-window", Fenlarge_window, Senlarge_window, 1, 2, "_p",
  "Make current window ARG lines bigger.\n\
From program, optional second arg non-nil means grow sideways ARG columns.")
  (n, side)
     register Lisp_Object n, side;
{
  CHECK_FIXNUM (n, 0);
/* >>> What do you think CHECK_FIXNUM does anyway? */
  /* Silent failure if no net change in size */
  if (NILP(n)) return Qnil;

  change_window_height (XINT (n), !NILP (side));
  return Qnil;
}

DEFUN ("shrink-window", Fshrink_window, Sshrink_window, 1, 2, "_p",
  "Make current window ARG lines smaller.\n\
From program, optional second arg non-nil means shrink sideways ARG columns.")
  (n, side)
     register Lisp_Object n, side;
{
  CHECK_FIXNUM (n, 0);
/* >>> What do you think CHECK_FIXNUM does anyway? */
  /* Silent failure if no change in size */
  if (NILP(n)) return Qnil;
  change_window_height (-XINT (n), !NILP (side));
  return Qnil;
}

int
window_height (window)
     Lisp_Object window;
{
  register struct window *p = XWINDOW (window);
  return (p->pixheight);
}

/*
 * Return window height as chars relative to size of window's base.
 */
int
window_char_height (w)
     register struct window *w;
{
  struct Lisp_Font *font;
  register int pix, height = 0, end = 0;
  int tmp;

  font = XFONT (SCREEN_DEFAULT_FONT (XSCREEN (w->screen)));

  if (w->window_end_valid)
    {
      height = w->used_height;
      end = (w->window_end_valid
             ? w->window_end_ppos - w->pixtop 
             : 0);
      height = 0;
      end = 0;
    }
  pix = w->pixheight - end;

  tmp = height + pix / font->height;
  return tmp;
}

/*
 * Return number of lines currently displayed in window w.  If
 * end-of-buffer is displayed return the window_char_height instead.
 * Do not include the modeline.
 */
int
window_displayed_height (struct window *w, int not_display_structs)
{
  struct line_header *l = window_display_lines (w);
  struct buffer *b = XBUFFER (w->buffer);
  int lines = 0;

  /* Chuck to Chuck:  Thou Shalt Not Assume What Valid Values Of
     		      Arguments To Compute_Motion Are. */
#if 0
  if (not_display_structs)
    {
      struct position pos;

      pos = *compute_motion (w, marker_position (w->start), 0, 0,
			     BUF_ZV (XBUFFER (w->buffer)),
			     -1, 0, XINT (w->hscroll), 0);

      return pos.height;
    }
#endif
  if ((w->window_end_pos == (BUF_Z (b) - BUF_ZV (b)))
      || (l && l->new)
      || w->size_change)
    {
      return window_internal_height (w);
    }
  else
    {
      while (l)
	{
	  lines++;
	  l = l->next;
	}

      return lines;
    }
}

int
window_width (window)
     Lisp_Object window;
{
  register struct window *p = XWINDOW (window);
  return (p->pixwidth);
}

int
window_real_width (w)
     register struct window *w;
{
  struct Lisp_Font *font;
  struct screen *s = XSCREEN (w->screen);

  font = XFONT (SCREEN_DEFAULT_FONT (s));
  return (font ?
	  (!window_needs_vertical_divider (w)
	   ? (w->pixwidth / font->width)
	   : (w->pixwidth / font->width - 1))
	  : 0);
}

int
window_char_width (w)
     register struct window *w;
{
  struct buffer *b = XBUFFER(w->buffer);
  struct screen *s = XSCREEN(w->screen);
  int margin_width = LEFT_MARGIN (b, s, w) +
    RIGHT_MARGIN (b, s, w);
  int window_width;

/*
 * This is really ugly.  It will have to be cleaned up when tty
 * support is added.  Not that it would hurt to do it sooner.
 */
#ifdef I18N4
  margin_width /= XmbTextEscapement (SCREEN_DEFAULT_X_FONT (s), "M", 1);
#else
  margin_width /= XTextWidth (SCREEN_DEFAULT_X_FONT (s), "M", 1);
#endif
  window_width = window_real_width (w);
		
  if ((margin_width + 2) > window_width)
    return 2;
  else
    return (window_width - margin_width);
}

#define MINSIZE(w, font)					\
  (widthflag							\
   ? window_min_width * font->width                             \
   : (MINI_WINDOW_P (XWINDOW (w)) ? 1 : window_min_height) * font->height)

#define CURBEG(w) \
  *(widthflag ? (int *) &(w)->pixleft : (int *) &(w)->pixtop)

#define CURSIZE(w) \
  *(widthflag ? (int *) &(w)->pixwidth : (int *) &(w)->pixheight)

#define CURCHARSIZE(w) \
  (widthflag ? window_char_width (w) : window_char_height (w))

#define MINCHARSIZE(window) \
  (widthflag ? window_min_width : MINI_WINDOW_P (XWINDOW (window)) \
   ? 1 : window_min_height)

/* Unlike set_window_height, this function
   also changes the heights of the siblings so as to
   keep everything consistent. */

static void
change_window_height (delta, widthflag)
     register int delta;
     int widthflag;
{
  register Lisp_Object parent;
  Lisp_Object window;
  register struct window *p;
  int *sizep;
  int (*sizefun) () = widthflag ? window_width : window_height;
  register void (*setsizefun) () = (widthflag
				    ? set_window_width
				    : set_window_height);
  struct Lisp_Font *font;
  int dim;

  if (delta == 0)
    return;

  check_min_window_sizes ();

  if (EQ (selected_window,
	  SCREEN_ROOT_WINDOW (XSCREEN (XWINDOW (selected_window)->screen))))
    error (GETTEXT ("Won't change only window"));

  font = XFONT (SCREEN_DEFAULT_FONT (selected_screen));

  window = selected_window;
  while (1)
    {
      p = XWINDOW (window);
      parent = p->parent;
      if (NILP (parent))
	{
	  if (widthflag)
	    error (GETTEXT ("No other window to side of this one"));
	  break;
	}
      if (widthflag
          ? !NILP (XWINDOW (parent)->hchild)
	  : !NILP (XWINDOW (parent)->vchild))
	break;
      window = parent;
    }

  sizep = &CURSIZE (p);
  dim = CURCHARSIZE (p);

  if ((dim + delta) < MINCHARSIZE (window))
    {
      if (MINI_WINDOW_P (XWINDOW (window)))
	return;
      else if (!NILP (parent))
	{
	  Fdelete_window (window);
	  return;
	}
    }

  delta *= (widthflag ? font->width : font->height);

  {
    register int maxdelta;

    maxdelta = ((!NILP (parent))
                ? (*sizefun) (parent) - *sizep
		: ((!NILP (p->next))
                   ? (*sizefun) (p->next) - MINSIZE (p->next, font)
                   : ((!NILP (p->prev))
                      ? (*sizefun) (p->prev) - MINSIZE (p->prev, font)
                      /* This is a screen with only one window, 
                         a minibuffer-only or a minibufferless screen.  */
                      : (delta = 0))));

    if (delta == 0)
      return;

    if (delta > maxdelta)
      /* This case traps trying to make the minibuffer
	 the full screen, or make the only window aside from the
	 minibuffer the full screen.  */
      delta = maxdelta;
  }

  if (!NILP (p->next) &&
      (*sizefun) (p->next) - delta >= MINSIZE (p->next, font))
    {
      (*setsizefun) (p->next, (*sizefun) (p->next) - delta, 0);
      (*setsizefun) (window, *sizep + delta, 0);
      CURBEG (XWINDOW (p->next)) += delta;
      /* This does not change size of p->next,
	 but it propagates the new top edge to its children */
      (*setsizefun) (p->next, (*sizefun) (p->next), 0);
    }
  else if (!NILP (p->prev) &&
	   (*sizefun) (p->prev) - delta >= MINSIZE (p->prev, font))
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

  p->last_modified = Qzero;
  p->last_facechange = Qzero;
}
#undef MINSIZE
#undef CURBEG
#undef CURSIZE
#undef CURCHARSIZE
#undef MINCHARSIZE


/* Return number of lines of text (not counting mode line) in W.  */

int
window_internal_height (w)
     struct window *w;
{
  int ht = window_char_height (w);

  if (MINI_WINDOW_P (w))
    return ht;

  if (!NILP (w->parent) || !NILP (w->vchild) || !NILP (w->hchild)
      || !NILP (w->next) || !NILP (w->prev)
      || SCREEN_WANTS_MODELINE_P (XSCREEN (WINDOW_SCREEN (w))))
    return ht - 1;

  return ht;
}

/* Scroll contents of window WINDOW up N lines.  */

static void
window_scroll (Lisp_Object window, int n, int noerror)
{
  register struct window *w = XWINDOW (window);
  register int opoint = PT;
  register int pos;
  register int ht = window_internal_height (w);
  register Lisp_Object tem;
  int lose;
  Lisp_Object bolp;

  tem = make_number (PT);
  tem = Fpos_visible_in_window_p (tem, window);

  if (NILP (tem))
    {
      Fvertical_motion (make_number (- ht / 2), window);
      tem = make_number (PT);
      Fset_marker (w->start, tem, w->buffer);
      w->force_start = 1;
    }

  pos = marker_position (w->start);

  if (pos < BEGV)
    pos = BEGV;
  else if (pos > ZV)
    pos = ZV;

  SET_PT (pos);
  lose = ((n < 0) && (PT == BEGV));
  Fvertical_motion (make_number (n), window);
  pos = PT;
  bolp = Fbolp ();
  SET_PT (opoint);

  if (lose)
    {
      if (noerror)
	return;
      else
	signal_error (Qbeginning_of_buffer, Qnil);
    }

  if (pos < ZV)
    {
      set_marker_restricted (w->start, make_number (pos), w->buffer);
      w->start_at_line_beg = !NILP (bolp);
      w->redo_mode_line = 1;
      w->last_modified = Qzero;
      w->last_facechange = Qzero;
      if (pos > opoint)
	SET_PT (pos);
      if (n < 0)
	{
	  SET_PT (pos);
	  /* #### Even though the "correct" values are getting
             returned whichever branch is taken this still isn't
             sufficient to scroll towards the top completely
             correctly.  Variable height line handling is hard. */
#if 0
	  /* This is a much cleaner way of doing this and it is
             working so far as its returning the correct value.  But
             it is also a hell of a lot slower.  That seems
             paradoxical at first thought but the visual evidence is
             quite persuasive.  I'm leaving it in just in case I get a
             chance to work on it some more. */
	  ht = window_displayed_height (w, 1);
#else
	  lock_redisplay (REDISPLAY_LOCK_CURSOR);
	  redisplay_1 (1);
	  unlock_redisplay ();

	  ht = window_displayed_height (w, 0);
#endif
	  tem = Fvertical_motion (make_number (ht), window);
	  if (PT > opoint || XINT (tem) < ht)
	    SET_PT (opoint);
	  else
	    Fvertical_motion (make_number (-1), window);
	}
    }
  else
    {
      if (noerror)
	return;
      else
	signal_error (Qend_of_buffer, Qnil);
    }
}

/* This is the guts of Fscroll_up and Fscroll_down.  */

void
scroll_command (n, direction, no_error)
     register Lisp_Object n;
     int direction;
     int no_error;
{
  register int defalt;
  int speccount = specpdl_depth ();

  /* If selected window's buffer isn't current, make it current for the moment.
     But don't screw up if window_scroll gets an error.  */
  if (XBUFFER (XWINDOW (selected_window)->buffer) != current_buffer)
    {
      record_unwind_protect (save_excursion_restore, save_excursion_save ());
      Fset_buffer (XWINDOW (selected_window)->buffer);
    }

  defalt = (window_displayed_height (XWINDOW (selected_window), 0)
	    - next_screen_context_lines);
  defalt = direction * (defalt < 1 ? 1 : defalt);

  if (NILP (n))
    window_scroll (selected_window, defalt, no_error);
  else if (EQ (n, Qminus))
    window_scroll (selected_window, - defalt, no_error);
  else
    {
      n = Fprefix_numeric_value (n);
      window_scroll (selected_window, XINT (n) * direction, no_error);
    }

  unbind_to (speccount, Qnil);
}

DEFUN ("scroll-up", Fscroll_up, Sscroll_up, 0, 1, "_P",
  "Scroll text of current window upward ARG lines; or near full screen if no ARG.\n\
A near full screen is `next-screen-context-lines' less than a full screen.\n\
When calling from a program, supply a number as argument or nil.")
  (n)
     Lisp_Object n;
{
  scroll_command (n, 1, 0);
  return Qnil;
}

DEFUN ("scroll-down", Fscroll_down, Sscroll_down, 0, 1, "_P",
  "Scroll text of current window downward ARG lines; or near full screen if no ARG.\n\
A near full screen is `next-screen-context-lines' less than a full screen.\n\
When calling from a program, supply a number as argument or nil.")
  (n)
     Lisp_Object n;
{
  scroll_command (n, -1, 0);
  return Qnil;
}

/*
 * As a partial fix for the problem of scrolling with different height
 * lines present, a call to redisplay was added in window_scroll.
 * scroll-other-window messes with the buffer state causing that call
 * to redisplay to act in unexpected ways.  Thus, these fun little
 * variables to provide a little extra state to the bastard redisplay
 * call in window_scroll so that it can avoid the problems.
 */
int bastard_flag_from_hell;
struct window *bastard_window_from_hell;

Lisp_Object
restore_bastards_from_hell (Lisp_Object val)
{
  bastard_flag_from_hell = 0;
  bastard_window_from_hell = 0;
  return Qnil;
}

DEFUN ("scroll-other-window", Fscroll_other_window, Sscroll_other_window, 0, 1, "_P",
  "Scroll next window upward ARG lines; or near full screen if no ARG.\n\
The next window is the one below the current one; or the one at the top\n\
if the current one is at the bottom.\n\
When calling from a program, supply a number as argument or nil.\n\
\n\
If in the minibuffer, `minibuf-scroll-window' if non-nil\n\
specifies the window to scroll.\n\
If `other-window-scroll-buffer' is non-nil, scroll the window\n\
showing that buffer, popping the buffer up if necessary.")
  (n)
     Lisp_Object n;
{
  Lisp_Object window;
  int ht, pos;
  struct window *w;
  int speccount = specpdl_depth ();

  if (MINI_WINDOW_P (XWINDOW (selected_window))
      && !NILP (Vminibuf_scroll_window))
    window = Vminibuf_scroll_window;
  /* If buffer is specified, scroll that buffer.  */
  else if (!NILP (Vother_window_scroll_buffer))
    {
      window = Fget_buffer_window (Vother_window_scroll_buffer, Qnil, Qnil);
      if (NILP (window))
	window = Fdisplay_buffer (Vother_window_scroll_buffer, Qt, Qnil);
    }
  else
    /* Nothing specified; pick a neighboring window in this screen.  */
    window = Fnext_window (selected_window, Qnil, Qnil, Qnil);
  CHECK_WINDOW (window, 0);

  if (EQ (window, selected_window))
    error (GETTEXT ("There is no other window"));

  w = XWINDOW (window);
  ht = window_displayed_height (w, 0);

  /* Don't screw up if window_scroll gets an error.  */
  record_unwind_protect (save_excursion_restore, save_excursion_save ());
  record_unwind_protect (restore_bastards_from_hell, Qnil);

  bastard_flag_from_hell = PT;
  bastard_window_from_hell = w;
  Fset_buffer (w->buffer);

  pos = marker_position (w->pointm);

  if (pos < BEGV)
    pos = BEGV;
  else if (pos > ZV)
    pos = ZV;

  SET_PT (pos);

  if (NILP (n))
    window_scroll (window, ht - next_screen_context_lines, 1);
  else if (EQ (n, Qminus))
    window_scroll (window, next_screen_context_lines - ht, 1);
  else
    {
      if (CONSP (n))
	n = Fcar (n);
      CHECK_FIXNUM (n, 0);
      window_scroll (window, XINT (n), 1);
    }
  Fset_marker (w->pointm, make_number (PT), Qnil);

  bastard_flag_from_hell = 0;
  bastard_window_from_hell = 0;
  return unbind_to (speccount, Qnil);
}

DEFUN ("scroll-left", Fscroll_left, Sscroll_left, 1, 1, "_P",
  "Scroll selected window display ARG columns left.\n\
Default for ARG is window width minus 2.")
  (arg)
     register Lisp_Object arg;
{
  if (NILP (arg))
    arg = make_number (window_char_width (XWINDOW (selected_window)) - 2);
  else
    arg = Fprefix_numeric_value (arg);

  return
    Fset_window_hscroll (selected_window,
			 make_number (XINT (XWINDOW (selected_window)->hscroll)
				      + XINT (arg)));
}

DEFUN ("scroll-right", Fscroll_right, Sscroll_right, 1, 1, "_P",
  "Scroll selected window display ARG columns right.\n\
Default for ARG is window width minus 2.")
  (arg)
     register Lisp_Object arg;
{
  if (NILP (arg))
    arg = make_number (window_char_width (XWINDOW (selected_window)) - 2);
  else
    arg = Fprefix_numeric_value (arg);

  return
    Fset_window_hscroll (selected_window,
			 make_number (XINT (XWINDOW (selected_window)->hscroll)
				      - XINT (arg)));
}

DEFUN ("recenter", Frecenter, Srecenter, 0, 1, "_P",
  "Center point in window and redisplay screen.  With ARG, put point on line ARG.\n\
The desired position of point is always relative to the current window.\n\
Just C-u as prefix means put point in the center of the window.\n\
No arg (i.e., it is nil) erases the entire screen and then\n\
redraws with point in the center of the current window.")
  (n)
     Lisp_Object n;
{
  struct window *w = XWINDOW (selected_window);
  int ht = window_internal_height (w);
  int opoint = PT;

  if (NILP (n))
    {
      SET_SCREEN_GARBAGED (XSCREEN (WINDOW_SCREEN (w)));
      n = make_number (ht / 2);
    }
  else if (CONSP (n)) 		/* Just C-u. */
    {
      n = make_number (ht / 2);
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
  Fset_marker (w->start, make_number (PT), w->buffer);
  w->start_at_line_beg = !NILP (Fbolp ());

  SET_PT (opoint);
  w->force_start = 1;
  return Qnil;
}

DEFUN ("move-to-window-line", Fmove_to_window_line, Smove_to_window_line,
  1, 1, "_P",
  "Position point relative to window.\n\
With no argument, position text at center of window.\n\
An argument specifies screen line; zero means top of window,\n\
negative means relative to bottom of window.")
  (arg)
     register Lisp_Object arg;
{
  register struct window *w = XWINDOW (selected_window);
  register int height = window_displayed_height (w, 0);
  register int start;

  if (NILP (arg))
    arg = make_number (height / 2);
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
      Fset_marker (w->start, make_number (PT), w->buffer);
      w->start_at_line_beg = !NILP (Fbolp ());
      w->force_start = 1;
    }
  else
    SET_PT (start);

  return Fvertical_motion (arg, selected_window);
}

/* #### This window configuration stuff has had serious bugs lurking in it
   for years; it would be a -huge- win if this was reimplemented in lisp.
 */

struct saved_window
  {
    Lisp_Object window;         /* window */
    Lisp_Object buffer;         /* buffer */
    Lisp_Object start;          /* copied marker */
    Lisp_Object pointm;         /* copied marker */
    Lisp_Object sb_point;	/* copied marker */
    Lisp_Object mark;           /* copied marker */
    int pixleft;
    int pixtop;
    int pixwidth;
    int pixheight;
    int hscroll;
    int parent_index;           /* index into saved_windows */
    int prev_index;             /* index into saved_windows */
    Lisp_Object display_table;
    Lisp_Object	dedicated;
    char start_at_line_beg; /* boolean */
  };

struct window_config
  {
    struct lcrecord_header header;
    int screen_width;
    int screen_height;
    Lisp_Object current_window;
    Lisp_Object current_buffer;
    Lisp_Object minibuf_scroll_window;
    Lisp_Object root_window;
#ifdef ENERGIZE
    /* The buffer whose p_sheets are visible */
    Lisp_Object p_sheet_buffer;
#endif
    int saved_windows_count;
    /* Zero-sized arrays aren't ANSI C */
    struct saved_window saved_windows[1];
  };
#define SAVED_WINDOW_N(conf,n) (&((conf)->saved_windows[(n)]))

static Lisp_Object mark_window_config (Lisp_Object, void (*) (Lisp_Object));
static void print_window_config (Lisp_Object, Lisp_Object, int);
static unsigned int sizeof_wconfig (CONST void *);
DEFINE_LRECORD_SEQUENCE_IMPLEMENTATION ("window-configuration",
					lrecord_window_configuration,
					mark_window_config,
					print_window_config, 
					0, 0, sizeof_wconfig);

static Lisp_Object
mark_window_config (Lisp_Object obj, void (*markobj) (Lisp_Object))
{
  struct window_config *config = XRECORD (obj);
  int i;
  ((markobj) (config->current_window));
  ((markobj) (config->current_buffer));
  ((markobj) (config->minibuf_scroll_window));
  ((markobj) (config->root_window));
#ifdef ENERGIZE
  ((markobj) (config->p_sheet_buffer));
#endif
  for (i = 0; i < config->saved_windows_count; i++)
    {
      struct saved_window *s = SAVED_WINDOW_N (config, i);
      ((markobj) (s->window));
      ((markobj) (s->buffer));
      ((markobj) (s->start));
      ((markobj) (s->pointm));
      ((markobj) (s->sb_point));
      ((markobj) (s->mark));
      ((markobj) (s->display_table));
      ((markobj) (s->dedicated));
    }
  return (Qnil);
}
  
static unsigned int
sizeof_wconfig (CONST void *h)
{
  CONST struct window_config *c = h;
  return (sizeof (*c)
          + ((c->saved_windows_count - 1) * sizeof (struct saved_window)));
}

static void
print_window_config (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  struct window_config *config = XRECORD (obj);
  char buf[200];
  if (print_readably)
    error (GETTEXT ("printing unreadable object #<window-configuration 0x%x>"),
           (long) config);
  write_string_1 ("#<window configuration ", -1, printcharfun);
  sprintf (buf, "0x%x>", (long) config);
  write_string_1 (buf, -1, printcharfun);
}


DEFUN ("window-configuration-p", Fwindow_configuration_p,
       Swindow_configuration_p, 1, 1, 0,
  "T if OBJECT is a window-configration object.")
  (obj)
     Lisp_Object obj;
{
  return ((RECORD_TYPEP (obj, lrecord_window_configuration)) ? Qt : Qnil);
}

/*
 * There are getting to be a lot of functions which traverse the
 * window structure doing various things.  It may be worth writing a
 * generic map-windows function.
 */
void
mark_windows_in_use (Lisp_Object window, int mark)
{
  for (; !NILP(window) ; window = XWINDOW(window)->next)
    {
      XWINDOW(window)->config_mark = mark;

      if (!NILP (XWINDOW(window)->vchild))
	mark_windows_in_use (XWINDOW(window)->vchild, mark);
      else if (!NILP (XWINDOW(window)->hchild))
	mark_windows_in_use (XWINDOW(window)->hchild, mark);
    }
}

DEFUN ("set-window-configuration",
       Fset_window_configuration, Sset_window_configuration,
       1, 1, 0,
  "Set the configuration of windows and buffers as specified by CONFIGURATION.\n\
CONFIGURATION must be a value previously returned\n\
by `current-window-configuration' (which see).")
     (configuration)
     Lisp_Object configuration;
{
  struct window *w;
  struct window_config *config;
  struct saved_window *p;
  Lisp_Object new_current_buffer;
  int k;
  Lisp_Object screen;
  SCREEN_PTR s;
  struct gcpro gcpro1;
  Lisp_Object old_window_config;

  GCPRO1 (configuration);

  CHECK_RECORD (configuration, lrecord_window_configuration,
		Qwindow_configuration_p, 0);

  config = XRECORD (configuration);

  screen = XWINDOW (SAVED_WINDOW_N (config, 0)->window)->screen;
  s = XSCREEN (screen);

  /* If the screen was deleted you can't restore the windows as they
     were deleted too.  Raise an error.  */
  if (!SCREEN_LIVE_P (s))
    error (GETTEXT ("Cannot set-window-configuration for a deleted screen"));

    /* restore the screen characteristics */
#ifdef ENERGIZE
  if (SCREEN_IS_X (s))
    {
      Lisp_Object new_desired = config->p_sheet_buffer;
    
      if (BUFFERP (new_desired) &&
	  NILP (XBUFFER (new_desired)->name))
	new_desired = Qnil;	/* the desired buffer was killed */
    
      /* need to restore the desired buffer */
      if (!EQ (new_desired, desired_psheet_buffer (s)))
	make_psheets_desired (s, new_desired);
    }
#endif
    
  new_current_buffer = config->current_buffer;
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

  /*
   * Assumed precondition:  w->config_mark = 0 for all w
   * This procedure should ensure this is true by the time it exits
   * to ensure the precondition for future calls.
   */
  old_window_config = Fcurrent_window_configuration (screen);
  mark_windows_in_use (s->root_window, 1);

  for (k = 0; k < config->saved_windows_count; k++)
    {
      p = SAVED_WINDOW_N (config, k);
      w = XWINDOW (p->window);
      w->next = Qnil;

      if (p->parent_index >= 0)
	w->parent = SAVED_WINDOW_N (config, p->parent_index)->window;
      else
	w->parent = Qnil;

      if (p->prev_index >= 0)
	{
	  w->prev = SAVED_WINDOW_N (config, p->prev_index)->window;
#ifdef MULTI_SCREEN
	  /* This is true for a minibuffer-only screen. */
	  if (!NILP (w->mini_p) && EQ (w->prev, p->window))
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
	      if (p->pixwidth == XWINDOW (w->parent)->pixwidth)
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
      /* Note:  If window has changed its position (left or top), mark it
       * has hosed and redraw/clear the whole thing
       */
      if (w->pixtop != p->pixtop
 	  || w->pixleft != p->pixleft
 	  || w->pixheight != p->pixheight
 	  || w->pixwidth != p->pixwidth)
 	w->size_change = 1;

      if (!w->config_mark)
	windows_changed++;
      else if (!EQ (w->buffer, p->buffer))
	windows_changed++;

      w->pixleft = p->pixleft;
      w->pixtop = p->pixtop;
      w->pixwidth = p->pixwidth;
      w->pixheight = p->pixheight;
      w->hscroll = make_number (p->hscroll);
      w->display_table = p->display_table;
      w->dedicated = p->dedicated;
      w->last_modified = Qzero;
      w->last_facechange = Qzero;
      w->config_mark = 0;

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
	      set_marker_restricted (w->sb_point, Fmarker_position (p->sb_point), w->buffer);
	      Fset_marker (XBUFFER (w->buffer)->mark,
			   Fmarker_position (p->mark), w->buffer);

              /* As documented in Fcurrent_window_configuration, don't
                 save the location of point in the buffer which was current
                 when the window configuration was recorded.  */
	      if (!EQ (p->buffer, new_current_buffer) &&
		  XBUFFER (p->buffer) == current_buffer)
		Fgoto_char (w->pointm);
	    }
	  else if (NILP (w->buffer) || NILP (XBUFFER (w->buffer)->name))
	    /* Else if window's old buffer is dead too, get a live one.  */
	    {
	      /* ## The following line makes me nervous... */
 	      /* w->buffer = Fcdr (Fcar (XSCREEN (w->screen)->buffer_alist));*/
	      w->buffer = Fget_buffer_create (QSscratch);
 	      /* w->buffer = Fother_buffer (Qnil, w->screen); */
	      /* This will set the markers to beginning of visible range.  */
	      set_marker_restricted (w->start, Qzero, w->buffer);
	      set_marker_restricted (w->pointm, Qzero, w->buffer);
	      set_marker_restricted (w->sb_point, Qzero, w->buffer);
	      w->start_at_line_beg = 1;
	    }
	  else
	    /* Keeping window's old buffer; make sure the markers are real.  */
	    /* Else if window's old buffer is dead too, get a live one.  */
	    {
	      /* Set window markers at start of visible range.  */
	      if (XMARKER (w->start)->buffer == 0)
		set_marker_restricted (w->start, Qzero, w->buffer);
	      if (XMARKER (w->sb_point)->buffer == 0)
		set_marker_restricted (w->sb_point, Qzero, w->buffer);
	      if (XMARKER (w->pointm)->buffer == 0)
		set_marker_restricted (w->pointm,
				       make_number (BUF_PT (XBUFFER (w->buffer))),
				       w->buffer);
	      w->start_at_line_beg = 1;
	    }
	}
    }

  SCREEN_ROOT_WINDOW (s) = config->root_window;

  if (config->screen_height != SCREEN_HEIGHT (s)
      || config->screen_width != SCREEN_WIDTH (s))
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
      Fselect_window (config->current_window);
      if (!NILP (new_current_buffer))
	Fset_buffer (new_current_buffer);
      else
	Fset_buffer (XWINDOW (selected_window)->buffer);
#ifdef ENERGIZE
      energize_buffer_shown_hook (XWINDOW(selected_window));
#endif
    }
  else
    s->selected_window = config->current_window;

  Vminibuf_scroll_window = config->minibuf_scroll_window;

  config = XRECORD (old_window_config);
  for (k = 0; k < config->saved_windows_count; k++)
    {
      p = SAVED_WINDOW_N (config, k);
      w = XWINDOW (p->window);
      w->config_mark = 0;
    }

  UNGCPRO;

  s->mirror_dirty = 1;

  return (Qnil);
}

/* Mark all windows now on screen as deleted
   by setting their buffers to nil.  */

static void
delete_all_subwindows (w)
     register struct window *w;
{
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
saved_window_index (Lisp_Object window, struct window_config *config, int lim)
{
  int j;
  for (j = 0; j < lim; j++)
  {
    if (EQ (SAVED_WINDOW_N (config, j)->window, window))
    {
      return (j);
    }
  }
  abort ();
}

static int
save_window_save (Lisp_Object window, struct window_config *config, int i)
{
  struct saved_window *p;
  struct window *w;

  for (;!NILP (window); window = w->next)
    {
      p = SAVED_WINDOW_N (config, i);
      w = XWINDOW (window);

      i++;
      /* w->temslot = make_number (i++); */
      p->window = window;
      p->buffer = w->buffer;
      p->pixleft = w->pixleft;
      p->pixtop = w->pixtop;
      p->pixwidth = w->pixwidth;
      p->pixheight = w->pixheight;
      p->hscroll = XINT (w->hscroll);
      p->display_table = w->display_table;
      if (!NILP (w->buffer))
	{
	  /* Save w's value of point in the window configuration.
	     If w is the selected window, then get the value of point
	     from the buffer; pointm is garbage in the selected window.  */
	  if (EQ (window, selected_window))
	    {
	      p->pointm = Fmake_marker ();
	      Fset_marker (p->pointm,
			   make_number (BUF_PT (XBUFFER (w->buffer))),
			   w->buffer);
	    }
	  else
	    p->pointm = Fcopy_marker (w->pointm);

	  p->start = Fcopy_marker (w->start);
	  p->sb_point = Fcopy_marker (w->sb_point);
	  p->start_at_line_beg = w->start_at_line_beg;

	  p->mark = Fcopy_marker (XBUFFER (w->buffer)->mark);
	}
      else
	{
	  p->pointm = Qnil;
	  p->start = Qnil;
	  p->sb_point = Qnil;
	  p->mark = Qnil;
	  p->start_at_line_beg = 0;
	}

      if (NILP (w->parent))
	p->parent_index = -1;
      else
        p->parent_index = saved_window_index (w->parent, config, i);
      if (NILP (w->prev))
	p->prev_index = -1;
      else
        p->prev_index = saved_window_index (w->prev, config, i);
      p->dedicated = w->dedicated;
      if (!NILP (w->vchild))
	i = save_window_save (w->vchild, config, i);
      if (!NILP (w->hchild))
	i = save_window_save (w->hchild, config, i);
    }

  return i;
}

DEFUN ("current-window-configuration",
	Fcurrent_window_configuration, Scurrent_window_configuration, 0, 1, 0,
  "Return an object representing the current window configuration of SCREEN.\n\
If SCREEN is nil or omitted, use the selected screen.\n\
This describes the number of windows, their sizes and current buffers,\n\
and for each displayed buffer, where display starts, and the positions of\n\
point and mark.  An exception is made for point in the current buffer:\n\
its value is -not- saved.")
  (screen)
     Lisp_Object screen;
{
  Lisp_Object result;
  SCREEN_PTR scr;
  register int n_windows;

  struct window_config *config;

  if (NILP (screen))
    scr = selected_screen;
  else
    {
      CHECK_LIVE_SCREEN (screen, 0);
      scr = XSCREEN (screen);
    }

  n_windows = count_windows (XWINDOW (SCREEN_ROOT_WINDOW (scr)));
  config = alloc_lcrecord ((sizeof (struct window_config)
                            /* n_windows - 1 because zero-sized arrays
                             *  aren't ANSI C */
                            + ((n_windows - 1)
                               * sizeof (struct saved_window))),
                           lrecord_window_configuration);
  XSETR (result, Lisp_Window_Configuration, config);

  config->screen_width = SCREEN_WIDTH (scr);
  config->screen_height = SCREEN_HEIGHT (scr);
  config->current_window = SCREEN_SELECTED_WINDOW (scr);
  XSETR (config->current_buffer, Lisp_Buffer, current_buffer);
  config->minibuf_scroll_window = Vminibuf_scroll_window;
  config->root_window = SCREEN_ROOT_WINDOW (scr);
  config->saved_windows_count = n_windows;
#ifdef ENERGIZE
  {
    config->p_sheet_buffer = desired_psheet_buffer (scr);
    if (EQ (config->p_sheet_buffer, Qzero)) /* #### necessaryp? */
      config->p_sheet_buffer = Qnil;
  }
#endif
  save_window_save (SCREEN_ROOT_WINDOW (scr), config, 0);
  return (result);
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
  register int speccount = specpdl_depth ();

  record_unwind_protect (Fset_window_configuration,
			 Fcurrent_window_configuration (Qnil));
  val = Fprogn (args);
  return unbind_to (speccount, val);
}

void
init_window_once ()
{
  bastard_flag_from_hell = 0;
  bastard_window_from_hell = 0;
#ifdef MULTI_SCREEN
  selected_screen = make_terminal_screen ();
  XSETR (Vterminal_screen, Lisp_Screen, selected_screen);
  minibuf_window = selected_screen->minibuffer_window;
  selected_window = selected_screen->selected_window;
  Vscreen_list = Fcons (Vterminal_screen, Qnil);
#else /* not MULTI_SCREEN */
  Lisp_Object root;
  minibuf_window = make_window ();
  root = make_window ();
  SCREEN_ROOT_WINDOW (selected_screen) = root;

  XWINDOW (root)->next = minibuf_window;
  XWINDOW (minibuf_window)->prev = root;
  XWINDOW (minibuf_window)->mini_p = Qt;

  /* These values 9 and 10 are arbitrary,
     just so that there is "something there."
     Correct values are put in in init_xdisp */

  XWINDOW (root)->pixwidth = 10;
  XWINDOW (minibuf_window)->pixwidth = 10;

  XWINDOW (root_window)->pixheight = 9;
  XWINDOW (minibuf_window)->pixtop = 9;
  XWINDOW (minibuf_window)->pixheight = 1;

  /* Prevent error in Fset_window_buffer.  */
  XWINDOW (root)->buffer = Qt;
  XWINDOW (minibuf_window)->buffer = Qt;

  /* Now set them up for real.  */
  Fset_window_buffer (root, Fcurrent_buffer ());
  Fset_window_buffer (minibuf_window, Vminibuffer_zero);

  selected_window = root;
  /* Make sure this window seems more recently used than
     a newly-created, never-selected window.  Increment
     window_select_count so the first selection ever will get
     something newer than this.  */
  XWINDOW (selected_window)->use_time = make_number (++window_select_count);
#endif /* not MULTI_SCREEN */
}

void
syms_of_window ()
{
  defsymbol (&Qwindowp, "windowp");
  defsymbol (&Qwindow_configuration_p, "window-configuration-p");
  defsymbol (&Qscroll_up, "scroll-up");
  defsymbol (&Qscroll_down, "scroll-down");

  /* Make sure all windows get marked */
  staticpro (&minibuf_window);

  DEFVAR_LISP ("temp-buffer-show-function", &Vtemp_buffer_show_function,
    "Non-nil means call as function to display a help buffer.\n\
Used by `with-output-to-temp-buffer'.");
  Vtemp_buffer_show_function = Qnil;

  DEFVAR_LISP ("display-buffer-function", &Vdisplay_buffer_function,
    "If non-nil, function to call to handle `display-buffer'.\n\
It will receive three args: the same as those to `display-buffer'.");
  Vdisplay_buffer_function = Qnil;

  DEFVAR_LISP ("pre-display-buffer-function", &Vpre_display_buffer_function,
    "If non-nil, function that will be called from `display-buffer'\n\
as the first action.  It will receive three args: the same as those\n\
to `display-buffer'.\n\
This function may be used to select an appropriate screen for the buffer,\n\
for example.  See also the variable `display-buffer-function', which may\n\
be used to completely replace the `display-buffer' function.");
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
  defsubr (&Swindow_pixheight);
  defsubr (&Swindow_pixwidth);
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
  defsubr (&Snext_vertical_window);
  defsubr (&Sother_window);
  defsubr (&Sget_lru_window);
  defsubr (&Sget_largest_window);
  defsubr (&Sget_buffer_window);
  defsubr (&Sbuffer_left_margin_pixwidth);
  defsubr (&Sbuffer_right_margin_pixwidth);
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
  defsubr (&Swindow_configuration_p);
  defsubr (&Sset_window_configuration);
  defsubr (&Scurrent_window_configuration);
  defsubr (&Ssave_window_excursion);
}
