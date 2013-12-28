/* Display generation from window structure and buffer text.
   Copyright (C) 1992 Free Software Foundation, Inc.

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
#include <stdio.h>

#include "lisp.h"
#include "window.h"
#include "termchar.h"
#include "buffer.h"
#include "extents.h"
#include "screen.h"
#include "indent.h"
#include "commands.h"
#include "macros.h"
#include "disptab.h"
#ifdef HAVE_X_WINDOWS
#include "xterm.h"
#endif

extern int interrupt_input;

/* Nonzero means print newline before next minibuffer message.  */

int noninteractive_need_newline;

#define min(a, b) ((a) < (b) ? (a) : (b))
#define max(a, b) ((a) > (b) ? (a) : (b))

/* The buffer position of the first character appearing
 entirely or partially on the current screen line.
 Or zero, which disables the optimization for the current screen line. */
static int this_line_bufpos;

/* Number of characters past the end of this line,
   including the terminating newline */
static int this_line_endpos;

/* The vertical position of this screen line. */
static int this_line_vpos;

/* Hpos value for start of display on this screen line.
   Usually zero, but negative if first character really began
   on previous line */
static int this_line_start_hpos;

/* Buffer that this_line variables are describing. */
static struct buffer *this_line_buffer;

/* Value of echo_area_glyphs when it was last acted on.
  If this is nonzero, there is a message on the screen
  in the minibuffer and it should be erased as soon
  as it is no longer requested to appear. */
char *previous_echo_glyphs;

/* Nonzero means truncate lines in all windows less wide than the screen */
int truncate_partial_width_windows;

Lisp_Object Vglobal_mode_string;

/* Marker for where to display an arrow on top of the buffer text.  */
Lisp_Object Voverlay_arrow_position;

/* String to display for the arrow.  */
Lisp_Object Voverlay_arrow_string;

/* Values of those variables at last redisplay.  */
Lisp_Object last_arrow_position, last_arrow_string;

/* Nonzero if overlay arrow has been displayed once in this window.  */
static int overlay_arrow_seen;

/* The number of lines to try scrolling a
  window by when point leaves the window; if
  it is <=0 then point is centered in the window */
int scroll_step;

/* Nonzero if try_window_id has made blank lines at window bottom
 since the last redisplay that paused */
static int blank_end_of_window;

/* Number of windows showing the buffer of the selected window.
   keyboard.c refers to this.  */
int buffer_shared;

/* display_text_line sets these to the screen position (origin 0) of point,
   whether the window is selected or not.
   Set one to -1 first to determine whether point was found afterwards.  */

static int cursor_vpos;
static int cursor_hpos;

int debug_end_pos;

/* Nonzero means display mode line highlighted */
int mode_line_inverse_video;

void mark_window_display_accurate ();
static void redisplay_windows ();
static void redisplay_window ();
static void try_window ();
static int try_window_id ();
static struct position *display_text_line ();
static void display_mode_line ();
static int display_mode_element ();
static int display_string ();

/* Prompt to display in front of the minibuffer contents */
char *minibuf_prompt;

/* Width in columns of current minibuffer prompt.  */
int minibuf_prompt_width;

/* Width in pixels of current minibuffer prompt.  */
int minibuf_prompt_pix_width;

/* Empty buffer made current in the minibuffer when echo display is on */
Lisp_Object echo_area_buffer;

extern Lisp_Object Vminibuffer_list;

/* Message to display instead of minibuffer contents
   This is what the functions error and message make,
   and command echoing uses it as well.
   It overrides the minibuf_prompt as well as the buffer.  */
char *echo_area_glyphs;

/* true iff we should redraw the mode lines on the next redisplay */
int redraw_mode_line;

/* Smallest number of characters before the gap
   at any time since last redisplay that finished.
   Valid for current buffer when try_window_id can be called.  */
int beg_unchanged;

/* Smallest number of characters after the gap
   at any time since last redisplay that finished.
   Valid for current buffer when try_window_id can be called.  */
int end_unchanged;

/* MODIFF as of last redisplay that finished;
   if it matches MODIFF, beg_unchanged and end_unchanged
   contain no useful information */
int unchanged_modified;

/* Nonzero if head_clip or tail_clip of current buffer has changed
   since last redisplay that finished */
int clip_changed;

/* Nonzero if window sizes or contents have changed
   since last redisplay that finished */
int windows_or_buffers_changed;

/* Screen currently being updated, or 0 if not in an update. */
extern SCREEN_PTR updating_screen;

/* This gets set to 1 when the user has changed fonts, colors, etc.,
   meaning that our cached GCs need to be rebuilt.
 */
extern int face_cache_invalid;


#ifdef MULTI_SCREEN
void
redraw_screen (s)
     SCREEN_PTR s;
{
  SCREEN_PTR s2 = 0;
#if 0 /* WTF? */
  if (s != selected_screen)
    return;
#endif

  /* clear_screen doesn't take a screen argument, 
     so swap s into selected_screen */
  if (s != selected_screen)
    s2 = selected_screen, selected_screen = s;
  clear_screen ();
  if (s2) selected_screen = s2;

  if (SCREEN_IS_TERMCAP (s))
    fflush (stdout);

  clear_screen_records (s);
  windows_or_buffers_changed++;

  /* Mark all windows as INaccurate,
     so that every window will have its redisplay done.  */
  mark_window_display_accurate (SCREEN_ROOT_WINDOW (s), 0);
  s->garbaged = 1;  /* this was 0, that can't have been right, right? */
}

DEFUN ("redraw-screen", Fredraw_screen, Sredraw_screen, 1, 1, 0,
  "Clear screen SCREEN and output again what is supposed to appear on it.")
  (screen)
     Lisp_Object screen;
{
  SCREEN_PTR s;

  CHECK_SCREEN (screen, 0);
  s = XSCREEN (screen);
  redraw_screen (s);

  return Qnil;
}

DEFUN ("redraw-display", Fredraw_display, Sredraw_display, 0, 0, "",
  "Redraw all screens marked as having their images garbled.")
  ()
{
  Lisp_Object screen, tail;

  for (tail = Vscreen_list; CONSP (tail); tail = XCONS (tail)->cdr)
    {
      screen = XCONS (tail)->car;
      if (XSCREEN (screen)->garbaged && XSCREEN (screen)->visible)
	Fredraw_screen (screen);
    }
  return Qnil;
}
#else  /* Not MULTI_SCREEN */
DEFUN ("redraw-display", Fredraw_display, Sredraw_display, 0, 0, "",
  "Clear the screen and output again what is supposed to appear on it.")
  ()
{
  clear_screen ();
  fflush (stdout);
  clear_screen_records (selected_screen);
  windows_or_buffers_changed++;
  /* Mark all windows as INaccurate,
     so that every window will have its redisplay done.  */
  mark_window_display_accurate (XWINDOW (minibuf_window)->prev, 0);
  return Qnil;
}
#endif /* not MULTI_SCREEN */

static void
echo_area_display (s)
     SCREEN_PTR s;
{
  register int vpos;

  if (!s->visible)
    return;

#if 0
#ifdef MULTI_SCREEN
  choose_minibuf_screen ();
  s = XSCREEN (XWINDOW (minibuf_window)->screen);
#endif
#endif

  if (SCREEN_GARBAGED_P (s))
    Fredraw_display ();

  if (echo_area_glyphs || minibuf_level == 0)
    {
      if (s != XSCREEN (XWINDOW (minibuf_window)->screen))
	return;

      vpos = XFASTINT (XWINDOW (minibuf_window)->top);
      get_display_line (s, vpos, 0);
      display_string (XWINDOW (minibuf_window), vpos,
		      echo_area_glyphs ? echo_area_glyphs : "",
		      0, 0, 0, SCREEN_WIDTH (s), &SCREEN_NORMAL_FACE (s), 0);

      /* If desired cursor location is on this line, put it at end of text */
      if (SCREEN_CURSOR_Y (s) == vpos)
	SCREEN_CURSOR_X (s) = s->desired_glyphs->used[vpos];
    }
  else if (!EQ (minibuf_window, selected_window))
    windows_or_buffers_changed++;

  if (EQ (minibuf_window, selected_window))
    {
      this_line_bufpos = 0;
    }
  if (echo_area_glyphs)
    previous_echo_glyphs = echo_area_glyphs;
}

char *message_buf;

/* dump an informative message to the minibuf */
/* VARARGS 1 */

void
message (m, a1, a2, a3)
     char *m;
     int a1, a2, a3;
{
  SCREEN_PTR s;

  if (noninteractive || !NILP (Vpurify_flag))
    {
      if (noninteractive_need_newline)
	putc ('\n', stderr);
      fprintf (stderr, m, a1, a2, a3);
      fprintf (stderr, "\n");
      fflush (stderr);
    }
  else if (INTERACTIVE)
    {
#ifdef NO_ARG_ARRAY
      int a[3];
      a[0] = a1;
      a[1] = a2;
      a[2] = a3;
#endif /* NO_ARG_ARRAY */

      s = choose_minibuf_screen ();

      /* It is bad to print gc messages on stdout before the first X screen
	 has been created.
       */
      if (!NILP (Vwindow_system) && !SCREEN_IS_X (s))
	return;

#ifdef NO_ARG_ARRAY
      doprnt (SCREEN_MESSAGE_BUF (s),
	      SCREEN_WIDTH (s), m, 0, 3, a);
#else
      doprnt (SCREEN_MESSAGE_BUF (s),
	      SCREEN_WIDTH (s), m, 0, 3, &a1);
#endif /* NO_ARG_ARRAY */

      echo_area_glyphs = SCREEN_MESSAGE_BUF (s);
      if (in_display > 0 || updating_screen != 0
	  || ! SCREEN_VISIBLE_P (s))
	return;

      echo_area_display (s);
      update_screen (s, 1, 1);
    }
}

/* Do a screen update, taking possible shortcuts into account.
   This is the main external entry point for redisplay.

   If the last redisplay displayed a minibuffer message and that
   message is no longer requested, we usually redraw the contents
   of the minibuffer, or clear the minibuffer if it is not in use.

   Everyone would like to have a hook here to call eval,
   but that cannot be done safely without a lot of changes elsewhere.
   This can be called from signal handlers; with alarms set up;
   or with synchronous processes running.
   See the echo functions in keyboard.c.
   See Fcall_process; if you called it from here, it could be
   entered recursively.  */

void
redisplay ()
{
  register struct window *w = XWINDOW (selected_window);
  register int pause = 0;
  int must_finish = 0;
  int all_windows;
  register int tlbufpos, tlendpos;
  struct position pos;

  extern int detect_input_pending ();
  int input_pending = detect_input_pending ();

  Lisp_Object tail;
  SCREEN_PTR s;

  if (noninteractive || in_display || input_pending)
    return;

  update_menubars ();

  in_display++;

  if (screen_garbaged)
    Fredraw_display ();

#ifdef MULTI_SCREEN
  for (tail = Vscreen_list; CONSP (tail); tail = XCONS (tail)->cdr){
    
    if (XTYPE (XCONS (tail)->car) != Lisp_Screen)
      continue;
    
    s = XSCREEN (XCONS (tail)->car);
    bzero (SCREEN_DESIRED_GLYPHS (s)->enable, SCREEN_HEIGHT (s));
  }
#else
  bzero (SCREEN_DESIRED_GLYPHS (selected_screen)->enable,
	 SCREEN_HEIGHT (selected_screen));
#endif

  if (echo_area_glyphs || previous_echo_glyphs)
    {
      echo_area_display (choose_minibuf_screen ());
      must_finish = 1;
    }

  if (clip_changed || windows_or_buffers_changed)
    redraw_mode_line++;

  /* Detect case that we need to write a star in the mode line.  */
  if (XFASTINT (w->last_modified) < MODIFF
      && XFASTINT (w->last_modified) <= current_buffer->save_modified)
    {
      w->redo_mode_line = Qt;
      if (buffer_shared > 1)
	redraw_mode_line++;
    }

  all_windows = redraw_mode_line || buffer_shared > 1;
#ifdef MULTI_SCREEN
#if 0
  all_windows |= (! NILP (Vglobal_minibuffer_screen)
		  && selected_screen != XSCREEN (Vglobal_minibuffer_screen));
#endif
#endif	/* MULTI_SCREEN */

  /* If specs for an arrow have changed, do thorough redisplay
     to ensure we remove any arrow that should no longer exist.  */
  if (Voverlay_arrow_position != last_arrow_position
      || Voverlay_arrow_string != last_arrow_string)
    all_windows = 1, clip_changed = 1;

  tlbufpos = this_line_bufpos;
  tlendpos = this_line_endpos;
  if (!all_windows && tlbufpos > 0 && NILP (w->redo_mode_line)
      && SCREEN_VISIBLE_P (XSCREEN (w->screen))
      /* Make sure recorded data applies to current buffer, etc */
      && this_line_buffer == current_buffer
      && current_buffer == XBUFFER (w->buffer)
      && NILP (w->force_start)
      /* Point must be on the line that we have info recorded about */
      && point >= tlbufpos
      && point <= Z - tlendpos

      /* All text outside that line, including its final newline,
	 must be unchanged */
      && (XFASTINT (w->last_modified) >= MODIFF
	  || (beg_unchanged >= tlbufpos - 1
	      && GPT >= tlbufpos
	      && end_unchanged >= tlendpos
	      && Z - GPT >= tlendpos)))
    {
      if (tlbufpos > BEGV && CHAR_AT (tlbufpos - 1) != '\n'
	  && (tlbufpos == ZV
	      || CHAR_AT (tlbufpos) == '\n'))
	/* Former continuation line has disappeared by becoming empty */
	goto cancel;
      if (XFASTINT (w->last_modified) < MODIFF
	  /* || (MINI_WINDOW_P (w) && 
             (previous_echo_glyphs || echo_area_glyphs)) */
	  )
	{
	  cursor_vpos = -1;
	  overlay_arrow_seen = 0;
	  display_text_line (w, tlbufpos, this_line_vpos, this_line_start_hpos,
			     pos_tab_offset (w, tlbufpos));
	  /* If line contains point, is not continued,
	     and ends at same distance from eob as before, we win */
	  if (cursor_vpos >= 0 && this_line_bufpos
	      && this_line_endpos == tlendpos)
	    {
	      if (XFASTINT (w->width) != 
                  SCREEN_WIDTH (XSCREEN (WINDOW_SCREEN (w))))
		preserve_other_columns (w);
	      goto update;
	    }
	  else
	    goto cancel;
	}
      else if (point == XFASTINT (w->last_point))
	{
	  if (!must_finish)
	    {
	      in_display--;
	      return;
	    }
	  goto update;
	}
      else
	{
	  int hscroll = XINT (w->hscroll);
	  pos = 
            *compute_motion
              (selected_window,
               tlbufpos, 0,
               (hscroll ? 1 - hscroll : 0) + 
               ((MINI_WINDOW_P (w) && tlbufpos == 1) ? 
                minibuf_prompt_width : 0),
               point, 2, - (1 << (SHORTBITS - 1)),
               XFASTINT (w->width) - 1
               - (XFASTINT (w->width) + XFASTINT (w->left)
                  != SCREEN_WIDTH (selected_screen)),
               hscroll, 0,
               ((MINI_WINDOW_P (w) && tlbufpos == 1)
                ? minibuf_prompt_pix_width
                : 0),
               0);
	  if (pos.vpos < 1)
	    {
	      SCREEN_CURSOR_X (selected_screen)
		= XFASTINT (w->left) + max (pos.hpos, 0);
	      SCREEN_CURSOR_Y (selected_screen) = this_line_vpos;
	      goto update;
	    }
	  else
	    goto cancel;
	}
    cancel:
      /* Text changed drastically or point moved off of line */
      cancel_line (this_line_vpos, selected_screen);
    }
  this_line_bufpos = 0;

  if (all_windows)
    {
#ifdef MULTI_SCREEN
      /* Recompute # windows showing selected buffer.
	 This will be incremented each time such a window is displayed.  */
      buffer_shared = 0;

      for (tail = Vscreen_list; CONSP (tail); tail = XCONS (tail)->cdr)
	{
	  SCREEN_PTR s;

	  if (XTYPE (XCONS (tail)->car) != Lisp_Screen)
	    continue;

	  s = XSCREEN (XCONS (tail)->car);
	  if (s->visible)
	    {
	      /* Clear the echo area on screens where the minibuffer isn't.  */
	      if (s != XSCREEN (XWINDOW (minibuf_window)->screen)
		  /* But only screens that have minibuffers.  */
		  && s->has_minibuffer)
		{
		  int vpos = XFASTINT (XWINDOW (s->minibuffer_window)->top);

		  cancel_line (vpos, s);
		  get_display_line (s, vpos, 0);
		  display_string 
                    (XWINDOW (s->minibuffer_window), vpos, "",
                     0, 0, 0, s->width, &SCREEN_NORMAL_FACE (s), 0);
		}

	      /* Redraw its windows.  */
	      redisplay_windows (SCREEN_ROOT_WINDOW (s));
	    }
	}
#else
    redisplay_windows (SCREEN_ROOT_WINDOW (s));
#endif /* not MULTI_SCREEN */
    }
  else if (SCREEN_VISIBLE_P (selected_screen))
    {
      redisplay_window (selected_window, 1);
      if (XFASTINT (w->width) != SCREEN_WIDTH (selected_screen))
	preserve_other_columns (w);
    }

update: 
  if (interrupt_input)
    unrequest_sigio ();

#ifdef MULTI_SCREEN
  if (all_windows)
    {
      pause = 0;

      for (tail = Vscreen_list; CONSP (tail); tail = XCONS (tail)->cdr)
	{
	  SCREEN_PTR s;

	  if (XTYPE (XCONS (tail)->car) != Lisp_Screen)
	    continue;

	  s = XSCREEN (XCONS (tail)->car);
	  if (s->visible)
	    {
	      pause |= update_screen (s, must_finish, 0);
	      if (must_finish && pause)
		abort ();
	      if (!pause)
		{
		  mark_window_display_accurate (s->root_window, 1);
		  s->garbaged = 0;
		}
	    }
	}
    }
  else
#endif /* MULTI_SCREEN */
    if (SCREEN_VISIBLE_P (selected_screen))
      {
	pause = update_screen (selected_screen, must_finish, 0);
	if (must_finish && pause)
	  abort ();
	if (!pause)
	  /* #### Should we do mark_window_display_accurate() here too? */
	  selected_screen->garbaged = 0;
      }

  /* If screen does not match, prevent doing single-line-update next time.
     Also, don't forget to check every line to update the arrow.  */
  if (pause)
    {
      this_line_bufpos = 0;
      if (!NILP (last_arrow_position))
	{
	  last_arrow_position = Qt;
	  last_arrow_string = Qt;
	}
      /* If we pause after scrolling, some lines in current_screen
	 may be null, so preserve_other_columns won't be able to
	 preserve all the vertical-bar separators.  So, avoid using it
	 in that case.  */
      if (XFASTINT (w->width) != SCREEN_WIDTH (selected_screen))
	redraw_mode_line = 1;
    }

  /* Now text on screen agrees with windows, so
     put info into the windows for partial redisplay to follow */

  if (!pause)
    {
      register struct buffer *b = XBUFFER (w->buffer);

      blank_end_of_window = 0;
      clip_changed = 0;
      unchanged_modified = BUF_MODIFF (b);
      beg_unchanged = BUF_GPT (b) - BUF_BEG (b);
      end_unchanged = BUF_Z (b) - BUF_GPT (b);

      XFASTINT (w->last_point) = BUF_PT (b);
      XFASTINT (w->last_point_x) = SCREEN_CURSOR_X (selected_screen);
      XFASTINT (w->last_point_y) = SCREEN_CURSOR_Y (selected_screen);

#ifdef ENERGIZE
      {
        extern void free_zombie_bitmaps ();
        free_zombie_bitmaps ();
      }
#endif
      if (face_cache_invalid)
	flush_face_cache ();

      if (all_windows)
	mark_window_display_accurate (XWINDOW (minibuf_window)->prev, 1);
      else
	{
	  w->redo_mode_line = Qnil;
	  XFASTINT (w->last_modified) = BUF_MODIFF (b);
	  w->window_end_valid = Qt;
	  last_arrow_position = Voverlay_arrow_position;
	  last_arrow_string = Voverlay_arrow_string;
	}
      redraw_mode_line = 0;
      windows_or_buffers_changed = 0;
    }

  /* Start SIGIO interrupts coming again.
     Having them off during the code above
     makes it less likely one will discard output,
     but not impossible, since there might be stuff
     in the system buffer here.
     But it is much hairier to try to do anything about that.  */

  if (interrupt_input)
    request_sigio ();

  in_display--;

}

/* Redisplay, but leave alone any recent echo area message
   unless another message has been requested in its place.  */

void
redisplay_preserving_echo_area ()
{
  if (echo_area_glyphs == 0 && previous_echo_glyphs != 0)
    {
      echo_area_glyphs = previous_echo_glyphs;
      redisplay ();
      echo_area_glyphs = 0;
    }
  else
    redisplay ();
}

void
mark_window_display_accurate (window, flag)
     Lisp_Object window;
     int flag;
{
  struct window *w;

  for (;!NILP (window); window = w->next)
    {
      w = XWINDOW (window);

      if (!NILP (w->buffer))
	{
	  XFASTINT (w->last_modified) =
	    flag ? BUF_MODIFF (XBUFFER (w->buffer)) : 0;
	  XFASTINT (w->last_facechange) =
	    flag ? BUF_FACECHANGE (XBUFFER (w->buffer)) : 0;
	}
      w->window_end_valid = flag ? Qt : Qnil;
      w->redo_mode_line = flag ? Qnil : Qt;

      if (!NILP (w->vchild))
	mark_window_display_accurate (w->vchild, flag);
      if (!NILP (w->hchild))
	mark_window_display_accurate (w->hchild, flag);
    }

  if (flag)
    {
      last_arrow_position = Voverlay_arrow_position;
      last_arrow_string = Voverlay_arrow_string;
    }
  else
    {
      /* t is unequal to any useful value of Voverlay_arrow_... */
      last_arrow_position = Qt;
      last_arrow_string = Qt;
    }
}

static void
redisplay_windows (window)
     Lisp_Object window;
{
  for (; !NILP (window); window = XWINDOW (window)->next)
    redisplay_window (window, 0);
}

static void
redisplay_window (window, just_this_one)
     Lisp_Object window;
     int just_this_one;
{
  register struct window *w = XWINDOW (window);
  SCREEN_PTR s = XSCREEN (w->screen);
  int height;
  struct buffer *old_buffer;
  register int width = XFASTINT (w->width) - 1
    - (XFASTINT (w->width) + XFASTINT (w->left)
       != SCREEN_WIDTH (XSCREEN (WINDOW_SCREEN (w))));
  register int startp;
  register int hscroll = XINT (w->hscroll);
  struct position pos;
  int opoint;
  int tem;

  if (SCREEN_HEIGHT (s) == 0)
    /* abort (); don't do this */
    return;

  /* If this is a combination window, do its children; that's all.  */

  if (!NILP (w->vchild))
    {
      redisplay_windows (w->vchild);
      return;
    }
  if (!NILP (w->hchild))
    {
      redisplay_windows (w->hchild);
      return;
    }
  if (NILP (w->buffer))
    /* abort ();   and don't do this */
    return;

  if (redraw_mode_line)
    w->redo_mode_line = Qt;

  /* Otherwise set up data on this window; select its buffer and point value */

  height = window_internal_height (w);

  if (MINI_WINDOW_P (w)
      && (echo_area_glyphs
	  /* Don't display minibuffers except minibuf_window.  */
	  || w != XWINDOW (minibuf_window)))
    return;

  old_buffer = current_buffer;
  current_buffer = XBUFFER (w->buffer);

  /* Count number of windows showing the selected buffer.  */
  if (!just_this_one
      && current_buffer == XBUFFER (XWINDOW (selected_window)->buffer))
    buffer_shared++;

  /* Go to window point for redisplay;  POINT will be restored afterwards. */
  opoint = point;
  if (!EQ (window, selected_window))
    {
      register int new_point = marker_position (w->pointm);

      /* If narrowing has changed, change the window min and max points
         accordingly. */
      if (new_point < BEGV)
	{
	  SET_PT (BEGV);
	  Fset_marker (w->pointm, make_number (BEGV), Qnil);
	}
      else if (new_point > ZV)
	{
	  SET_PT (ZV);
	  Fset_marker (w->pointm, make_number (ZV), Qnil);
	}
      else
	SET_PT (new_point);
    }

  /* If window-start is screwed up, choose a new one.  */
  if (XMARKER (w->start)->buffer != current_buffer)
    goto recenter;

  /* If window-start is pointing at point-max, that's bad.
     Unless it's also pointing at point-min, that is.
   */
  if (XMARKER (w->start)->bufpos == BUF_ZV (XMARKER (w->start)->buffer) &&
      XMARKER (w->start)->bufpos != BUF_BEG (XMARKER (w->start)->buffer))
    goto recenter;

  if (MINI_WINDOW_P (w) && !echo_area_glyphs && previous_echo_glyphs)
    {
      previous_echo_glyphs = 0;
      goto recenter;
    }

  startp = marker_position (w->start);

  if (startp < BEGV)
    startp = BEGV;
  else if (startp > ZV)
    startp = ZV;

  /* Handle case where place to start displaying has been specified */
  if (!NILP (w->force_start))
    {
      w->redo_mode_line = Qt;
      w->force_start = Qnil;
      XFASTINT (w->last_modified) = 0;
      try_window (window, startp);
      if (cursor_vpos < 0)
	{
	  /* If point does not appear, move point so it does appear */
	  pos = *compute_motion (window,
				 startp, 0,
				((EQ (window, minibuf_window) && startp == 1)
				 ? minibuf_prompt_width : 0)
				+
				(hscroll ? 1 - hscroll : 0),
				ZV, height / 2,
				- (1 << (SHORTBITS - 1)),
				width, hscroll, pos_tab_offset (w, startp),
				 ((EQ (window, minibuf_window) && startp == 1)
				  ? minibuf_prompt_pix_width
				  : 0),
				 0);
	  SET_PT (pos.bufpos);
	  if (w != XWINDOW (SCREEN_SELECTED_WINDOW (s)))
	    Fset_marker (w->pointm, make_number (point), Qnil);
	  else
	    {
	      /* We want to change point permanently,
		 so don't restore the old value.  */
	      opoint = point;
	      SCREEN_CURSOR_X (s) = max (0, pos.hpos) + XFASTINT (w->left);
	      SCREEN_CURSOR_Y (s) = pos.vpos + XFASTINT (w->top);
	    }
	}
      goto done;
    }

  /* Handle case where text has not changed, only point,
     and it has not moved off the screen.

     This code is not used for minibuffer for the sake of
     the case of redisplaying to replace an echo area message;
     since in that case the minibuffer contents per se are usually unchanged.
     This code is of no real use in the minibuffer since
     the handling of tlbufpos, etc., in redisplay handles the same cases. */

  if (XFASTINT (w->last_modified) >= MODIFF
      && XFASTINT (w->last_facechange) >= FACECHANGE
      && point >= startp && !clip_changed
      && (just_this_one || XFASTINT (w->width) == SCREEN_WIDTH (s))
      /* && !(EQ (window, minibuf_window) && 
         previous_echo_glyphs && !echo_area_glyphs) */
      )
    {
      pos = *compute_motion (window,
			     startp, 0,
			     (hscroll ? 1 - hscroll : 0)
			     + (MINI_WINDOW_P (w) && startp == 1 ? 
                                minibuf_prompt_width : 0),
			     point, height + 1, 10000, width, hscroll,
			     pos_tab_offset (w, startp),
			     (MINI_WINDOW_P (w) && startp == 1
			      ? minibuf_prompt_pix_width
			      : 0),
			     0);

      if (pos.vpos < height)
	{
	  /* Ok, point is still on screen */
	  if (w == XWINDOW (SCREEN_SELECTED_WINDOW (s)))
	    {
	      /* These variables are supposed to be origin 1 */
	      SCREEN_CURSOR_X (s) = max (0, pos.hpos) + XFASTINT (w->left);
	      SCREEN_CURSOR_Y (s) = pos.vpos + XFASTINT (w->top);
	    }

#if 0
/* This doesn't do the trick, because if a window to the right of
   this one must be redisplayed, this does nothing because there
   is nothing in DesiredScreen yet, and then the other window is
   redisplayed, making likes that are empty in this window's columns. */

	  if (XFASTINT (w->width) != SCREEN_WIDTH (s))
	    preserve_my_columns (w);
#endif

	  goto done;
	}
      /* Don't bother trying redisplay with same start;
	we already know it will lose */
    }
  /* If current starting point was originally the beginning of a line
     but no longer is, find a new starting point.  */
  else if (!NILP (w->start_at_line_beg)
	   && XFASTINT (w->last_facechange) >= FACECHANGE
	   && !(startp == BEGV
		|| CHAR_AT (startp - 1) == '\n'))
    {
      goto recenter;
    }
  else if (just_this_one
	   /* && !(MINI_WINDOW_P (w) && 
              (echo_area_glyphs || previous_echo_glyphs)) */
	   && point >= startp
	   && XFASTINT (w->last_modified)
	   && ! EQ (w->window_end_valid, Qnil)
	   && !clip_changed
	   && !blank_end_of_window
	   && XFASTINT (w->width) == SCREEN_WIDTH (s)
	   && EQ (last_arrow_position, Voverlay_arrow_position)
	   && EQ (last_arrow_string, Voverlay_arrow_string)
	   && XFASTINT (w->last_facechange) >= FACECHANGE
	   && (tem = try_window_id (SCREEN_SELECTED_WINDOW (s)))
	   && tem != -2)
    {
      /* tem > 0 means success.  tem == -1 means choose new start.
	 tem == -2 means try again with same start,
	  and nothing but whitespace follows the changed stuff.
	 tem == 0 means try again with same start.  */
      if (tem > 0)
	goto done;
    }
  else if (startp >= BEGV && startp <= ZV
	   /* Avoid starting display at end of buffer! */
	   && (startp <= ZV || startp == BEGV
	       || XFASTINT (w->last_modified) >= MODIFF
	       || XFASTINT (w->last_facechange) >= FACECHANGE))
    {
      /* Try to redisplay starting at same place as before */
      /* If point has not moved off screen, accept the results */
      try_window (window, startp);
      if (cursor_vpos >= 0)
	goto done;
      else
	cancel_my_columns (w);
    }

  XFASTINT (w->last_modified) = 0;
  w->redo_mode_line = Qt;

  /* Try to scroll by specified few lines */

  if (scroll_step && !clip_changed)
    {
      if (point > startp)
	{
	  pos = *vmotion (Z - XFASTINT (w->window_end_pos),
			  scroll_step, width, hscroll, window);
	  if (pos.vpos >= height)
	    goto scroll_fail;
	}

      pos = *vmotion (startp, point < startp ? - scroll_step : scroll_step,
		      width, hscroll, window);

      if (point >= pos.bufpos)
	{
	  try_window (window, pos.bufpos);
	  if (cursor_vpos >= 0)
	    goto done;
	  else
	    cancel_my_columns (w);
	}
    scroll_fail: ;
    }

  /* Finally, just choose place to start which centers point */

recenter:
  pos = *vmotion (point, - height / 2, width, hscroll, window);
  try_window (window, pos.bufpos);

  startp = marker_position (w->start);
  w->start_at_line_beg = 
    (startp == BEGV || CHAR_AT (startp - 1) == '\n') ? Qt : Qnil;

done:
  /* If window not full width, must redo its mode line
     if the window to its side is being redone */
  if ((!NILP (w->redo_mode_line)
       || (!just_this_one && width < SCREEN_WIDTH (s) - 1))
      && height != XFASTINT (w->height))
    display_mode_line (w);

  SET_PT (opoint);
  current_buffer = old_buffer;
}

void initialize_first_screen ()
{
  if (noninteractive) return;
  selected_screen->visible = 1;
  redisplay_windows (selected_screen->root_window);
  redisplay();
}


/* Do full redisplay on one window, starting at position `pos'. */

static void
try_window (window, pos)
     Lisp_Object window;
     register int pos;
{
  register struct window *w = XWINDOW (window);
  register int height = window_internal_height (w);
  register int vpos = XFASTINT (w->top);
  register int last_text_vpos = vpos;
  int tab_offset = pos_tab_offset (w, pos);
  SCREEN_PTR s = XSCREEN (w->screen);
  int width = XFASTINT (w->width) - 1
    - (XFASTINT (w->width) + XFASTINT (w->left) != SCREEN_WIDTH (s));
  struct position val;

  Fset_marker (w->start, make_number (pos), Qnil);
  cursor_vpos = -1;
  overlay_arrow_seen = 0;
  val.hpos = XINT (w->hscroll) ? 1 - XINT (w->hscroll) : 0;

  while (--height >= 0)
    {
      val = *display_text_line (w, pos, vpos, val.hpos, tab_offset);
      tab_offset += width;
      if (val.vpos) tab_offset = 0;
      vpos++;
      if (pos != val.bufpos)
	last_text_vpos
	  /* Next line, unless prev line ended in end of buffer with no cr */
	  = vpos - (val.vpos && CHAR_AT (val.bufpos - 1) != '\n');
      pos = val.bufpos;
    }

  /* If last line is continued in middle of character,
     include the split character in the text considered on the screen */
  if (val.hpos < (XINT (w->hscroll) ? 1 - XINT (w->hscroll) : 0))
    pos++;

  /* If bottom just moved off end of screen, change mode line percentage.  */
  if (XFASTINT (w->window_end_pos) == 0
      && Z != pos)
    w->redo_mode_line = Qt;

  /* Say where last char on screen will be, once redisplay is finished.  */
  if (pos > Z)
    abort ();
  XFASTINT (w->window_end_pos) = Z - pos;
  XFASTINT (w->window_end_vpos) = last_text_vpos - XFASTINT (w->top);
  /* But that is not valid info until redisplay finishes.  */
  w->window_end_valid = Qnil;
}

/* Try to redisplay when buffer is modified locally,
 computing insert/delete line to preserve text outside
 the bounds of the changes.
 Return 1 if successful, 0 if if cannot tell what to do,
 or -1 to tell caller to find a new window start,
 or -2 to tell caller to do normal redisplay with same window start.  */

static int
try_window_id (window)
     Lisp_Object window;
{
  int pos;
  register struct window *w = XWINDOW (window);
  register int height = window_internal_height (w);
  SCREEN_PTR s = XSCREEN (w->screen);
  int top = XFASTINT (w->top);
  int start = marker_position (w->start);
  int width = XFASTINT (w->width) - 1
    - (XFASTINT (w->width) + XFASTINT (w->left) != SCREEN_WIDTH (s));
  int hscroll = XINT (w->hscroll);
  int lmargin = hscroll > 0 ? 1 - hscroll : 0;
  register int vpos;
  register int i, tem;
  int last_text_vpos = 0;
  int stop_vpos;

  struct position val, bp, ep, xp, pp;
  int scroll_amount = 0;
  int delta;
  int tab_offset, epto;

  if (GPT - BEG < beg_unchanged)
    beg_unchanged = GPT - BEG;
  if (Z - GPT < end_unchanged)
    end_unchanged = Z - GPT;

  if (beg_unchanged + 1 < start)
    return 0;			/* Give up if changes go above top of window */

  /* Find position before which nothing is changed.  */
  bp = *compute_motion 
    (window,
     start, 0,
     lmargin + (MINI_WINDOW_P (w) ? minibuf_prompt_width : 0) ,
     beg_unchanged + 1, 10000, 10000, width, hscroll,
     pos_tab_offset (w, start),
     (MINI_WINDOW_P (w) ? minibuf_prompt_pix_width : 0),
     0);
  if (bp.vpos >= height)
    return point < bp.bufpos && !bp.contin;

  vpos = bp.vpos;

  /* Find beginning of that screen line.  Must display from there.  */
  bp = *vmotion (bp.bufpos, 0, width, hscroll, window);

  pos = bp.bufpos;
  val.hpos = lmargin;
  if (pos < start)
    return -1;

  /* If about to start displaying at the beginning of a continuation line,
     really start with previous screen line, in case it was not
     continued when last redisplayed */
  if (bp.contin && bp.contin <= beg_unchanged && vpos > 0)
    {
      bp = *vmotion (bp.bufpos, -1, width, hscroll, window);
      --vpos;
      pos = bp.bufpos;
    }

  if (bp.contin && bp.hpos != lmargin)
    {
      val.hpos = bp.prevhpos - width + lmargin;
      pos--;
    }

  bp.vpos = vpos;

  /* Find first visible newline after which no more is changed.  */
  tem = find_next_newline (Z - max (end_unchanged, Z - ZV), 1);
  if (XTYPE (current_buffer->selective_display) == Lisp_Int
      && XINT (current_buffer->selective_display) > 0)
    while (tem < ZV - 1
	   && (position_indentation (tem)
	       >= XINT (current_buffer->selective_display)))
      tem = find_next_newline (tem, 1);

  /* Compute the cursor position after that newline.  */
  ep = *compute_motion (window,
			pos, vpos, val.hpos, tem,
			height, - (1 << (SHORTBITS - 1)),
			width, hscroll, pos_tab_offset (w, bp.bufpos),
			0, 0);

  /* If changes reach past the text available on the screen,
     just display rest of screen.  */
  if (ep.bufpos > Z - XFASTINT (w->window_end_pos))
    stop_vpos = height;
  else
    stop_vpos = ep.vpos;

  /* If no newline before ep, the line ep is on includes some changes
     that must be displayed.  Make sure we don't stop before it.  */
  /* Also, if changes reach all the way until ep.bufpos,
     it is possible that something was deleted after the
     newline before it, so the following line must be redrawn. */
  if (stop_vpos == ep.vpos
      && (ep.bufpos == BEGV
	  || CHAR_AT (ep.bufpos - 1) != '\n'
	  || ep.bufpos == Z - end_unchanged))
    stop_vpos = ep.vpos + 1;

  cursor_vpos = -1;
  overlay_arrow_seen = 0;

  /* If changes do not reach to bottom of window,
     figure out how much to scroll the rest of the window */
  if (stop_vpos < height)
    {
      /* Now determine how far up or down the rest of the window has moved */
      epto = pos_tab_offset (w, ep.bufpos);
      xp = *compute_motion (window,
			    ep.bufpos, ep.vpos, ep.hpos,
			    Z - XFASTINT (w->window_end_pos),
			    10000, 0, width, hscroll, epto,
			    ep.pixpos, ep.column);
      scroll_amount = xp.vpos - XFASTINT (w->window_end_vpos);

      /* Is everything on screen below the changes whitespace?
	 If so, no scrolling is really necessary.  */
      for (i = ep.bufpos; i < xp.bufpos; i++)
	{
	  tem = CHAR_AT (i);
	  if (tem != ' ' && tem != '\n' && tem != '\t')
	    break;
	}
      if (i == xp.bufpos)
	return -2;

      XFASTINT (w->window_end_vpos) += scroll_amount;

      /* Before doing any scrolling, verify that point will be on screen. */
      if (point > ep.bufpos && !(point <= xp.bufpos && xp.bufpos < height))
	{
	  if (point <= xp.bufpos)
	    {
	      pp = *compute_motion (window,
				    ep.bufpos, ep.vpos, ep.hpos,
				    point, height, - (1 << (SHORTBITS - 1)),
				    width, hscroll, epto,
				    ep.pixpos, ep.column);
	    }
	  else
	    {
	      pp = 
                *compute_motion 
                  (window,
                   xp.bufpos, xp.vpos, xp.hpos,
                   point, height, - (1 << (SHORTBITS - 1)),
                   width, hscroll, pos_tab_offset (w, xp.bufpos),
                   xp.pixpos, xp.column);
	    }
	  if (pp.bufpos < point || pp.vpos == height)
	    return 0;
	  cursor_vpos = pp.vpos + top;
	  cursor_hpos = pp.hpos + XFASTINT (w->left);
	}

      if (stop_vpos - scroll_amount >= height
	  || ep.bufpos == xp.bufpos)
	{
	  if (scroll_amount < 0)
	    stop_vpos -= scroll_amount;
	  scroll_amount = 0;
	  /* In this path, we have altered window_end_vpos
	     and not left it negative.
	     We must make sure that, in case display is preempted
	     before the screen changes to reflect what we do here,
	     further updates will not come to try_window_id
	     and assume the screen and window_end_vpos match.  */
	  blank_end_of_window = 1;
	}
      else if (!scroll_amount)
	{}
      else if (bp.bufpos == Z - end_unchanged)
	{
	  /* If reprinting everything is nearly as fast as scrolling,
	     don't bother scrolling.  Can happen if lines are short.  */
	  if (scroll_cost (s, bp.vpos + top - scroll_amount,
			   top + height - max (0, scroll_amount),
			   scroll_amount)
	      > xp.bufpos - bp.bufpos - 20)
	    /* Return "try normal display with same window-start."
	       Too bad we can't prevent further scroll-thinking.  */
	    return -2;
	  /* If pure deletion, scroll up as many lines as possible.
	     In common case of killing a line, this can save the
	     following line from being overwritten by scrolling
	     and therefore having to be redrawn.  */
	  tem = scroll_screen_lines (s, bp.vpos + top - scroll_amount,
				     top + height - max (0, scroll_amount),
				     scroll_amount);
	  if (!tem) stop_vpos = height;
	}
      else if (scroll_amount)
	{
	  /* If reprinting everything is nearly as fast as scrolling,
	     don't bother scrolling.  Can happen if lines are short.  */
	  /* Note that if scroll_amount > 0, xp.bufpos - bp.bufpos is an
	     overestimate of cost of reprinting, since xp.bufpos
	     would end up below the bottom of the window.  */
	  if (scroll_cost (s, ep.vpos + top - scroll_amount,
			   top + height - max (0, scroll_amount),
			   scroll_amount)
	      > xp.bufpos - ep.bufpos - 20)
	    /* Return "try normal display with same window-start."
	       Too bad we can't prevent further scroll-thinking.  */
	    return -2;
	  tem = scroll_screen_lines (s, ep.vpos + top - scroll_amount,
				     top + height - max (0, scroll_amount),
				     scroll_amount);
	  if (!tem) stop_vpos = height;
	}
    }

  /* In any case, do not display past bottom of window */
  if (stop_vpos >= height)
    {
      stop_vpos = height;
      scroll_amount = 0;
    }

  /* Handle case where pos is before w->start --
     can happen if part of line had been clipped and is not clipped now */
  if (vpos == 0 && pos < marker_position (w->start))
    Fset_marker (w->start, make_number (pos), Qnil);

  /* Redisplay the lines where the text was changed */
  last_text_vpos = vpos;
  tab_offset = pos_tab_offset (w, pos);
  /* If we are starting display in mid-character, correct tab_offset
     to account for passing the line that that character really starts in.  */
  if (val.hpos < lmargin)
    tab_offset += width;
  while (vpos < stop_vpos)
    {
      val = *display_text_line (w, pos, top + vpos++, val.hpos, tab_offset);
      tab_offset += width;
      if (val.vpos) tab_offset = 0;
      if (pos != val.bufpos)
	last_text_vpos
	  /* Next line, unless prev line ended in end of buffer with no cr */
	    = vpos - (val.vpos && CHAR_AT (val.bufpos - 1) != '\n');
      pos = val.bufpos;
    }

  /* There are two cases:
     1) we have displayed down to the bottom of the window
     2) we have scrolled lines below stop_vpos by scroll_amount  */

  if (vpos == height)
    {
      /* If last line is continued in middle of character,
	 include the split character in the text considered on the screen */
      if (val.hpos < lmargin)
	val.bufpos++;
      XFASTINT (w->window_end_vpos) = last_text_vpos;
      if (Z < val.bufpos)
	abort ();
      XFASTINT (w->window_end_pos) = Z - val.bufpos;
    }

  /* If scrolling made blank lines at window bottom,
     redisplay to fill those lines */
  if (scroll_amount < 0)
    {
      vpos = xp.vpos;
      pos = xp.bufpos;
      val.hpos = lmargin;
      if (pos == ZV)
	vpos = height + scroll_amount;
      else if (xp.contin && xp.hpos != lmargin)
	{
	  val.hpos = xp.prevhpos - width + lmargin;
	  pos--;
	}

      blank_end_of_window = 1;
      tab_offset = pos_tab_offset (w, pos);
      /* If we are starting display in mid-character, correct tab_offset
	 to account for passing the line that that character starts in.  */
      if (val.hpos < lmargin)
	tab_offset += width;

      while (vpos < height)
	{
	  val = 
            *display_text_line (w, pos, top + vpos++, val.hpos, tab_offset);
	  tab_offset += width;
	  if (val.vpos) tab_offset = 0;
	  pos = val.bufpos;
	}

      /* Here is a case where display_line_text sets cursor_vpos wrong.
	 Make it be fixed up, below.  */
      if (xp.bufpos == ZV
	  && xp.bufpos == point)
	cursor_vpos = -1;
    }

  /* If bottom just moved off end of screen, change mode line percentage.  */
  if (XFASTINT (w->window_end_pos) == 0
      && Z != val.bufpos)
    w->redo_mode_line = Qt;

  /* Attempt to adjust end-of-text positions to new bottom line */
  if (scroll_amount)
    {
      delta = height - xp.vpos;
      if (delta < 0
	  || (delta > 0 && xp.bufpos <= ZV)
	  || (delta == 0 && xp.hpos))
	{
	  val = *vmotion (Z - XFASTINT (w->window_end_pos),
			  delta, width, hscroll, window);
	  if (Z < val.bufpos)
	    abort ();
	  XFASTINT (w->window_end_pos) = Z - val.bufpos;
	  XFASTINT (w->window_end_vpos) += val.vpos;
	}
    }

  w->window_end_valid = Qnil;

  /* If point was not in a line that was displayed, find it */
  if (cursor_vpos < 0)
    {
      val = *compute_motion (window,
			     start, 0, lmargin, point, 10000, 10000,
			     width, hscroll, pos_tab_offset (w, start),
			     0, 0);
      /* Admit failure if point is off screen now */
      if (val.vpos >= height)
	{
	  for (vpos = 0; vpos < height; vpos++)
	    cancel_line (vpos + top, s);
	  return 0;
	}
      cursor_vpos = val.vpos + top;
      cursor_hpos = val.hpos + XFASTINT (w->left);
    }

  SCREEN_CURSOR_X (s) = max (0, cursor_hpos);
  SCREEN_CURSOR_Y (s) = cursor_vpos;

  if (debug_end_pos)
    {
      val = *compute_motion (window,
			     start, 0, lmargin, ZV,
			     height, - (1 << (SHORTBITS - 1)),
			     width, hscroll, pos_tab_offset (w, start),
			     0, 0);
      if (val.vpos != XFASTINT (w->window_end_vpos))
	return 0;
      if (XFASTINT (w->window_end_pos)
	  != Z - val.bufpos)
	return 0;
    }

  return 1;
}

/* Copy part of the contents of the string FROM into a glyph-vector at S.
   But don't actually copy the parts that would come in before T.
   Value is T, advanced past the copied data.

   Characters in FROM are grouped into units of `sizeof GLYPH' chars;
   any extra chars at the end of FROM are ignored.  */

GLYPH *
copy_rope (t, s, from)
     register GLYPH *t; /* Copy to here. */
     register GLYPH *s; /* Starting point. */
     Lisp_Object from;    /* Data to copy; known to be a string.  */
{
  register int n = XSTRING (from)->size / sizeof (GLYPH);
  register GLYPH *f = (GLYPH *) XSTRING (from)->data;

  while (n--)
    {
      if (t >= s) *t = *f;
      ++t;
      ++f;
    }
  return t;
}

struct glyphs_from_chars char_glyphs;

static GLYPH measure_glyphs[80];

#define GLYPH_SET_VALUE(g, v) ((g) = (v))

struct glyphs_from_chars *
glyphs_from_char (screen, c, g, tab_width, ctl_arrow,
		  fp, dp, hscroll, columns, tab_offset, pixel_values)
     SCREEN_PTR screen;
     register unsigned int c;
     GLYPH *g;
     struct face *fp;
     register int tab_width, ctl_arrow, hscroll, columns, tab_offset;
     register struct Lisp_Vector *dp;
     int pixel_values;
{
  register GLYPH *gp = g;

  char_glyphs.columns = 0;
  char_glyphs.faceptr = fp;
  char_glyphs.next_visible = 0;
  char_glyphs.begin_or_end = 0;
  char_glyphs.pixel_width = 0;
  char_glyphs.pixel_height = 0;

  if (c == '\n')			/* Newline character. */
    return &char_glyphs;

  if (((c >= 040 && c < 0177) ||	/* Normal character. */
       (!EQ (ctl_arrow, Qnil) &&	/* 8-bit display */
	!EQ (ctl_arrow, Qt) && c >= 0240))
      && (dp == 0
	  || NILP (DISP_CHAR_ROPE (dp, c))))
    {
      GLYPH_SET_VALUE (*gp++, GLYPH_FROM_CHAR (c));
    }
  else if (c == '\t')		/* Tab character. */
    {
      register int j;

      j = tab_width - ((columns + tab_offset + hscroll - (hscroll > 0))
		       % tab_width);
      while (j--)
	GLYPH_SET_VALUE (*gp++, TABGLYPH);
    }
  else if (dp != 0 && XTYPE (DISP_CHAR_ROPE (dp, c)) == Lisp_String)
    {
      register int i = XSTRING (DISP_CHAR_ROPE (dp, c))->size / sizeof (GLYPH);
      GLYPH *data = (GLYPH *) XSTRING (DISP_CHAR_ROPE (dp, c))->data;

      /* Note that ropes are 16 bit entities stored as double chars */
      while (i--)
	GLYPH_SET_VALUE (*gp++, *data++);
    }
  else if (c < 0200 && !NILP (ctl_arrow))
    {
      if (dp && XTYPE (DISP_CTRL_GLYPH (dp)) == Lisp_Int)
	GLYPH_SET_VALUE (*gp, XINT (DISP_CTRL_GLYPH (dp)));
      else
	GLYPH_SET_VALUE (*gp, GLYPH_FROM_CHAR ('^'));
      gp++;
      GLYPH_SET_VALUE (*gp, GLYPH_FROM_CHAR (c ^ 0100));
      gp++;
    }
  else
    {
      GLYPH_SET_VALUE (*gp, (dp && XTYPE (DISP_ESCAPE_GLYPH (dp)) == Lisp_Int
			     ? XINT (DISP_ESCAPE_GLYPH (dp))
			     : GLYPH_FROM_CHAR ('\\')));
      gp++;
      GLYPH_SET_VALUE (*gp, GLYPH_FROM_CHAR (((c >> 6) + '0')));
      gp++;
      GLYPH_SET_VALUE (*gp, GLYPH_FROM_CHAR (((7 & (c >> 3)) + '0')));
      gp++;
      GLYPH_SET_VALUE (*gp, GLYPH_FROM_CHAR (((7 & c) + '0')));
      gp++;
    }

  char_glyphs.columns = gp - g;

#ifdef HAVE_X_WINDOWS
  if (pixel_values)
    {
      register GLYPH *p = g;

      while (p < gp)
	{
	  char_glyphs.pixel_width
	    += X_CHAR_WIDTH (fp->font, ((*p == TABGLYPH) ? ' ' : *p));
	  p++;
	}
      char_glyphs.pixel_height = FONT_HEIGHT (fp->font);
    }
#endif

  return &char_glyphs;
}

/* Cache the state of glyphs_from_bufpos. */

#define ASCII_BEGIN_GLYPH 99	/* some random number */
#define ASCII_END_GLYPH 399	/* another random number */

static struct buffer *last_buffer;
static struct screen* last_screen;
static int last_buffer_modiff;
static int last_buffer_facechange;
static EXTENT_FRAGMENT last_buffer_extfrag;
static int last_extfrag_from_pos, last_extfrag_to_pos;
static struct face *last_extfrag_face;

struct glyphs_from_chars displayed_glyphs;

/* Determine how the character at POS in BUFFER would be displayed on
   SCREEN.  ONLY_NONCHARS means calculate only glyphs displayed around
   or instead of the characters, PIXEL_VALUES means return the pixel
   dimensions of the displayed glyphs.  Return value is a
   struct glyphs_from_chars pointer. */

static void
update_cache (buffer, extfrag)
     struct buffer* buffer;
     EXTENT_FRAGMENT extfrag;
{
  last_extfrag_from_pos = extfrag->from;
  last_extfrag_to_pos = extfrag->to;
  last_buffer_extfrag = extfrag;
}
 
struct glyphs_from_chars *
glyphs_from_bufpos (screen, buffer, pos, dp, hscroll, columns, tab_offset,
		    direction, only_nonchars)
     SCREEN_PTR screen;
     struct buffer *buffer;
     register int pos, hscroll, columns, tab_offset;
     register struct Lisp_Vector *dp;
     int direction, only_nonchars;
{
  int pixel_values = SCREEN_IS_X (screen);
  register EXTENT_FRAGMENT extfrag;
  register struct face *fp;
  register UCHAR c;
  register GLYPH *gp;
  int ignore_glyphs = 0;
  /* we will cut our "run" short if we discover that the
     frag ends with some end glyphs */
  int frag_has_end_glyphs;

 retry:

  gp = measure_glyphs;
  frag_has_end_glyphs = 0;
  displayed_glyphs.glyphs = gp;

  /* Check if we can use our cached values */
  if (buffer == last_buffer
      && screen == last_screen
      && BUF_MODIFF (buffer) == last_buffer_modiff
      && BUF_FACECHANGE (buffer) == last_buffer_facechange)
    {
      if (pos >= last_extfrag_from_pos
	  && pos < last_extfrag_to_pos)
	{
	  extfrag = last_buffer_extfrag;
	  fp = last_extfrag_face;
	  goto cache_used;
	}
      else if (pos == last_extfrag_to_pos)
	{
	  extfrag = buffer_extent_fragment_at (pos, buffer, screen);
	  fp = extfrag->fp;
	  if (!fp)
	    fp = &SCREEN_NORMAL_FACE (screen);
	  last_extfrag_face = fp;
	  update_cache (buffer, extfrag);
	  goto cache_used;
	}
    }

  /* if (!cache_used) */
    {
      last_buffer = buffer;
      last_screen = screen;
      last_buffer_modiff = BUF_MODIFF (buffer);
      last_buffer_facechange = BUF_FACECHANGE (buffer);
      extfrag = buffer_extent_fragment_at (pos, buffer, screen);
      last_buffer_extfrag = extfrag;
      last_extfrag_from_pos = extfrag->from;
      last_extfrag_to_pos = extfrag->to;
      fp = extfrag->fp;
      if (!fp)
	fp = &SCREEN_NORMAL_FACE (screen);
      last_extfrag_face = fp;
    }

 cache_used:

  displayed_glyphs.faceptr = fp;

#ifdef HAVE_X_WINDOWS
  /* Deal with begin/end glyphs.  Should make all this a function. */
  if (pixel_values)
    {
      displayed_glyphs.begin_pixel_width = 0;
      displayed_glyphs.end_pixel_width = 0;
      displayed_glyphs.pixel_height = FONT_HEIGHT (fp->font);
    }
#endif

#ifdef LINE_INFO_COLUMN
  displayed_glyphs.info_column_glyphs = Qnil;
#endif
  displayed_glyphs.begin_or_end = 0;
  displayed_glyphs.n_nonfont = 0;
  displayed_glyphs.begin_columns = 0;
  displayed_glyphs.end_columns = 0;
  displayed_glyphs.begin_class[0] = Qnil;
  displayed_glyphs.end_class[0] = Qnil;

  /* Look for begin glyphs */
  if (extfrag && (pos == extfrag->from) && !ignore_glyphs)
    {
      int n_classes = 0;
      EXTENT *extents_vec = extfrag->extents_stack;
      EXTENT *extents_vec_bound = extents_vec + extfrag->number_of_extents;

      while (extents_vec < extents_vec_bound
	     && n_classes < GLYPH_CLASS_VECTOR_SIZE - 1)
        {
          EXTENT current_extent = *extents_vec++;
          Lisp_Object elt = EXTENT_BEGIN_GLYPH_AT (current_extent, pos);

          if (XTYPE (elt) == Lisp_Int)
            {
#ifdef LINE_INFO_COLUMN
              if (screen->display.x->line_info_column_width
                  && glyph_in_column_p (class))
                displayed_glyphs.info_column_glyphs = elt;
              else
#endif
#ifdef HAVE_X_WINDOWS
                if (pixel_values)
                  {
                    displayed_glyphs.n_nonfont++;
                    XSET (displayed_glyphs.begin_class[n_classes++],
                          Lisp_Extent, current_extent);
                    *gp++ = XINT (elt);
                    displayed_glyphs.begin_columns++;
                    displayed_glyphs.begin_pixel_width
                      += x_bitmaps[XINT (elt)].width;
                  }
                else
#endif
                  *gp++ = ASCII_BEGIN_GLYPH;		    
            }
        }

      /* Note begin glyphs */
      if (n_classes > 0)
	displayed_glyphs.begin_or_end = 2;
      displayed_glyphs.begin_class[n_classes] = Qnil;
    }
  displayed_glyphs.pixel_width = displayed_glyphs.begin_pixel_width;

  if (! only_nonchars)
    {
      GLYPH *g0 = gp;

      c = BUF_CHAR_AT (buffer, pos);
      displayed_glyphs.c = c;
      if (c == '\n')		/* Newline character. */
	goto exit;

      if (((c >= 040 && c < 0177) ||	/* Normal character. */
	   (!EQ (buffer->ctl_arrow, Qnil) &&	/* 8-bit display */
	    !EQ (buffer->ctl_arrow, Qt) && c >= 0240))  /* Normal + 8th bit */
	  && (dp == 0
	      || NILP (DISP_CHAR_ROPE (dp, c))))
	{
	  GLYPH_SET_VALUE (*gp++, GLYPH_FROM_CHAR (c));
	}
      else if (c == '\t')	/* Tab character. */
	{
	  register int j;
	  int tab_width = 
            ((XINT (buffer->tab_width) <= 0) || 
             (XINT (buffer->tab_width) > 20))
              ? 8:(XINT(buffer->tab_width));

	  j = tab_width - ((columns + tab_offset + hscroll - (hscroll > 0))
			   % tab_width);
	  while (j--)
	    GLYPH_SET_VALUE (*gp++, TABGLYPH);
	}
      else if (dp != 0 && XTYPE (DISP_CHAR_ROPE (dp, c)) == Lisp_String)
	{
	  register int i = 
            (XSTRING (DISP_CHAR_ROPE (dp, c))->size) / sizeof (GLYPH);
	  GLYPH *data = (GLYPH *) XSTRING (DISP_CHAR_ROPE (dp, c))->data;

	  /* Note that ropes are 16 bit entities stored as double chars */
	  while (i--)
	    GLYPH_SET_VALUE (*gp++, *data++);
	}
      else if (c < 0200 && !NILP (buffer->ctl_arrow))
	{
	  if (dp && XTYPE (DISP_CTRL_GLYPH (dp)) == Lisp_Int)
	    GLYPH_SET_VALUE (*gp, XINT (DISP_CTRL_GLYPH (dp)));
	  else
	    GLYPH_SET_VALUE (*gp, GLYPH_FROM_CHAR ('^'));
	  gp++;
	  GLYPH_SET_VALUE (*gp, GLYPH_FROM_CHAR (c ^ 0100));
	  gp++;
	}
      else
	{
	  GLYPH_SET_VALUE 
            (*gp, (dp && XTYPE (DISP_ESCAPE_GLYPH (dp)) == Lisp_Int
                   ? XINT (DISP_ESCAPE_GLYPH (dp))
                   : GLYPH_FROM_CHAR ('\\')));
	  gp++;
	  GLYPH_SET_VALUE (*gp, GLYPH_FROM_CHAR (((c >> 6) + '0')));
	  gp++;
	  GLYPH_SET_VALUE (*gp, GLYPH_FROM_CHAR (((7 & (c >> 3)) + '0')));
	  gp++;
	  GLYPH_SET_VALUE (*gp, GLYPH_FROM_CHAR (((7 & c) + '0')));
	  gp++;
	}

#ifdef HAVE_X_WINDOWS
      if (pixel_values)
	while (g0 < gp)
	  {
	    displayed_glyphs.pixel_width
	      += X_CHAR_WIDTH (fp->font, ((*g0==TABGLYPH) ? ' ' : *g0));
	    g0++;
	  }
#endif
    }

 exit:

  /* Look for end glyphs */
  if (extfrag && !ignore_glyphs)
    {
      int n_classes = 0;
      EXTENT *extents_vec = extfrag->extents_stack;
      EXTENT *extents_vec_bound = extents_vec + extfrag->number_of_extents;
      int endpos = extfrag->to - 1;
      int save_found_glyphs = (pos == endpos);

      while (extents_vec < extents_vec_bound
	     && n_classes < GLYPH_CLASS_VECTOR_SIZE - 1)
        {
          EXTENT current_extent = *extents_vec++;

          if (current_extent->flags & EF_END_GLYPH)
            {
              Lisp_Object elt = 
                EXTENT_END_GLYPH_AT (current_extent, endpos);
              if (XTYPE (elt) == Lisp_Int)
                {
                  frag_has_end_glyphs = 1;

                  if (save_found_glyphs)
                    {
#ifdef HAVE_X_WINDOWS
                      if (pixel_values)
                        {
                          displayed_glyphs.n_nonfont++;
                          XSET (displayed_glyphs.end_class[n_classes++],
                                Lisp_Extent, current_extent);
                          *gp++ = XINT (elt);
                          displayed_glyphs.end_columns++;
                          displayed_glyphs.end_pixel_width 
                            += x_bitmaps[XINT (elt)].width;
                        }
                      else
#endif
                        *gp++ = ASCII_END_GLYPH;
                    }
                }
            }
        }

      /* Note end glyphs */
      if (n_classes > 0)
        displayed_glyphs.begin_or_end++;
      displayed_glyphs.end_class[n_classes] = Qnil;

      displayed_glyphs.pixel_width += displayed_glyphs.end_pixel_width;
    }

  if (columns == 0
      && displayed_glyphs.pixel_width
      > (MAX_LINE_WIDTH (screen) - EOL_CURSOR_WIDTH))
    {
      /* Just remove all glyphs.  This is wrong, but better than looping */
      last_extfrag_from_pos = last_extfrag_to_pos = 0; /* Invalidate cache */
      ignore_glyphs = 1;
      goto retry;
    }

  /* set the displayed_glyphs "run limits" from the extent_fragment info
     and the selection state
   */
  {
    int run_end = last_extfrag_to_pos - 1;

    /* don't go to last position of an extent_fragment in the
       cheapo loop in display_text_line() if there are end glyphs there, 
       because you will lose */
    if (frag_has_end_glyphs)
      run_end -= 1;

    displayed_glyphs.run_pos_lower = pos;
    displayed_glyphs.run_pos_upper = run_end;
  }

  displayed_glyphs.columns = gp - measure_glyphs;
  return &displayed_glyphs;
}

int
new_run (screen, vpos, type, faceptr)
     SCREEN_PTR screen;
     int vpos, type;
     struct face *faceptr;
{
  struct screen_glyphs *glyph_lines = SCREEN_DESIRED_GLYPHS (screen);
  register int run;

  run = glyph_lines->nruns[vpos];
  glyph_lines->nruns[vpos]++;
  glyph_lines->face_list[vpos][run].type = type;
  glyph_lines->face_list[vpos][run].faceptr = faceptr;
  glyph_lines->face_list[vpos][run].length = 0;
  glyph_lines->face_list[vpos][run].w = 0;
  glyph_lines->face_list[vpos][run].bufp = 0;
#ifdef HAVE_X_WINDOWS
  glyph_lines->face_list[vpos][run].pix_length = 0;
#endif

  return run;
}

static int
glyph_pixel_width (g, t, f)
     register GLYPH g;
     register int t;
     struct face *f;
{
  switch (t)
    {
    case font:
      return X_CHAR_WIDTH (f->font, ((g == TABGLYPH) ? ' ' : g));
      break;

    case glyph:
      return x_bitmaps[g].width;
      break;

    case space:
      return g;
      break;

    default:
      abort ();
      break;
    }
}

void
stuff_glyph (glyph_lines, vpos, g, len)
     struct screen_glyphs *glyph_lines;
     int vpos, len;
     GLYPH g;
{
  register int run = glyph_lines->nruns[vpos] - 1;
  register int n = glyph_lines->used[vpos];

  switch (glyph_lines->face_list[vpos][run].type)
    {
    case font:
      {
	glyph_lines->glyphs[vpos][n] = g;
	glyph_lines->face_list[vpos][run].length++;
	glyph_lines->face_list[vpos][run].pix_length
	  += X_CHAR_WIDTH (glyph_lines->face_list[vpos][run].faceptr->font,
			   ((g == TABGLYPH) ? ' ' : g));

	glyph_lines->used[vpos]++;
	glyph_lines->pix_width[vpos] += x_bitmaps[g].width;
	glyph_lines->pix_height[vpos] = max (glyph_lines->pix_height[vpos],
					     x_bitmaps[g].height);
      }
      break;

    case column_glyph:
      abort();

    case glyph:
      {
	glyph_lines->glyphs[vpos][n] = g;
	glyph_lines->face_list[vpos][run].length++;
	glyph_lines->face_list[vpos][run].pix_length += x_bitmaps[g].width;

	glyph_lines->used[vpos]++;
	glyph_lines->pix_width[vpos] += x_bitmaps[g].width;
	glyph_lines->pix_height[vpos] = max (glyph_lines->pix_height[vpos],
					     x_bitmaps[g].height);
      }
      break;

    case space:
      {
#if 0
	glyph_lines->glyphs[vpos][n] = '?';
#endif
	glyph_lines->glyphs[vpos][n] = (GLYPH) len;
	glyph_lines->face_list[vpos][run].length++;
	glyph_lines->face_list[vpos][run].pix_length += len;

	glyph_lines->used[vpos]++;
	glyph_lines->pix_width[vpos] += len;
      }
      break;

    case window:
      abort ();
      break;

    default:
      abort ();
      break;
    }
}

static void
install_first_runs (s, vpos, pos, w)
     register SCREEN_PTR s;
     register int vpos, pos;
     struct window *w;
{
  register struct screen_glyphs *desired_glyphs = SCREEN_DESIRED_GLYPHS (s);

  desired_glyphs->bufp[vpos] = pos;

  /* put the window run */
  desired_glyphs->face_list[vpos][0].type = window;
  desired_glyphs->face_list[vpos][0].length = 0;
  desired_glyphs->face_list[vpos][0].faceptr = &SCREEN_NORMAL_FACE (s);
  desired_glyphs->face_list[vpos][0].w = w;
  desired_glyphs->face_list[vpos][0].bufp = pos;
  desired_glyphs->face_list[vpos][0].class = Qnil;
  desired_glyphs->face_list[vpos][0].begin_p = 0;
  if (SCREEN_IS_X (s))
    {
      desired_glyphs->face_list[vpos][0].pix_length = 0;
    }

#ifdef LINE_INFO_COLUMN
  if (s->display.x->line_info_column_width)
    {
      /* put the colum run */
      desired_glyphs->face_list[vpos][1].type = column_glyph;
      desired_glyphs->face_list[vpos][1].length = 1;
      desired_glyphs->face_list[vpos][1].faceptr
	= &SCREEN_NORMAL_FACE (s);
      desired_glyphs->face_list[vpos][1].w = 0;
      desired_glyphs->face_list[vpos][1].bufp = pos;
      desired_glyphs->face_list[vpos][1].class = Qnil;
      desired_glyphs->face_list[vpos][1].begin_p = 0;
      desired_glyphs->nruns[vpos] = 2;
      desired_glyphs->used[vpos] = 1;

      if (SCREEN_IS_X (s))
	{
	  desired_glyphs->face_list[vpos][1].pix_length =
	    s->display.x->line_info_column_width;
	  desired_glyphs->pix_width[vpos] = 
	    s->display.x->line_info_column_width;
	  desired_glyphs->face_list[vpos][1].lineinfo_glyph_index = -1;
	}
    }
  else
#endif
    {
      desired_glyphs->nruns[vpos] = 1;
      desired_glyphs->used[vpos] = 0;
    }
}

static struct run *
append_run (s, vpos, type, class, faceptr, begin_p)
     SCREEN_PTR s;
     int vpos, type;
     Lisp_Object class;
     struct face *faceptr;
     int begin_p;
{
  register int run = SCREEN_DESIRED_GLYPHS (s)->nruns[vpos];
  struct run *new_run;

  SCREEN_DESIRED_GLYPHS (s)->nruns[vpos] = run + 1;
  new_run = &SCREEN_DESIRED_GLYPHS (s)->face_list[vpos][run];
  new_run->length = 0;
  new_run->type = type;
  new_run->class = class;
  new_run->faceptr = faceptr;
  new_run->begin_p = begin_p;
  if (SCREEN_IS_X (s))
    new_run->pix_length = 0;

  return new_run;
}

#define FONT_ASCENT(f)  FONT_BASE((f))

static void
append_glyph (s, vpos, g, type, class, faceptr, begin_p)
     SCREEN_PTR s;
     int vpos, type;
     GLYPH g;
     Lisp_Object class;
     struct face *faceptr;
     int begin_p;
{
  register struct screen_glyphs *glyphs = SCREEN_DESIRED_GLYPHS (s);
  int run = glyphs->nruns[vpos] - 1;
  int n = glyphs->used[vpos];
  struct run *this_run = &glyphs->face_list[vpos][run];

  if ((type != this_run->type) || 
      (faceptr != this_run->faceptr) ||
      (!EQ (class, this_run->class)))
    this_run = append_run (s, vpos, type, class, faceptr, begin_p);

  this_run->length++;
  glyphs->used[vpos] = n + 1;
  glyphs->glyphs[vpos][n] = g;
  glyphs->glyphs[vpos][n + 1] = NULL_GLYPH;

  /* Adjust pix bounds */
  if (SCREEN_IS_X (s))
    {
      register int pix_width, pix_height;

      switch (type)
	{
	case font:
	case glyph:
	  pix_height = max (s->display.x->text_height,
			    FONT_HEIGHT (faceptr->font));
	  pix_height += x_interline_space;
	  break;

	case column_glyph:
	case window:
	case space:
	default:
	  pix_height = 0;
	  break;
	}

      glyphs->pix_height[vpos]
	= max (glyphs->pix_height[vpos], pix_height);
      glyphs->max_ascent[vpos]
	= max (glyphs->max_ascent[vpos],
	       FONT_ASCENT (faceptr->font));
      pix_width = glyph_pixel_width (g, type, faceptr);
      this_run->pix_length += pix_width;
      glyphs->pix_width[vpos] += pix_width;
    }
}

static void
remove_glyph (s, vpos)
     SCREEN_PTR s;
     register int vpos;
{
  register int dead = SCREEN_DESIRED_GLYPHS (s)->used[vpos] - 1;
  register GLYPH g = SCREEN_DESIRED_GLYPHS (s)->glyphs[vpos][dead];
  register int run = SCREEN_DESIRED_GLYPHS (s)->nruns[vpos] - 1;
  register struct run* last_run
    = &(SCREEN_DESIRED_GLYPHS (s)->face_list[vpos][run]);

  if (last_run->length == 0)
    abort ();

  SCREEN_DESIRED_GLYPHS (s)->used[vpos] = dead;
  SCREEN_DESIRED_GLYPHS (s)->glyphs[vpos][dead] = NULL_GLYPH;
  if (SCREEN_IS_X (s))
    {
      register int pix_width = glyph_pixel_width (g, last_run->type,
						  last_run->faceptr);

      last_run->pix_length -= pix_width;
      SCREEN_DESIRED_GLYPHS (s)->pix_width[vpos] -= pix_width;
    }

  if (last_run->length == 1)
    SCREEN_DESIRED_GLYPHS (s)->nruns[vpos] = run;
  else
    last_run->length--;
}

/* Right shift runs starting with RUN to the right by NSLOTS. */

static void
right_shift_runs (s, vpos, run, nslots)
     SCREEN_PTR s;
     register int vpos, run, nslots;
{
  register int nruns = SCREEN_DESIRED_GLYPHS (s)->nruns[vpos];
  if (run < nruns)
    {
      int n_to_move = nruns - run;

      bcopy (&SCREEN_DESIRED_GLYPHS (s)->face_list[vpos][run],
	     &SCREEN_DESIRED_GLYPHS (s)->face_list[vpos][run + nslots],
	     (n_to_move * sizeof (struct run)));
    }
  SCREEN_DESIRED_GLYPHS (s)->nruns[vpos] += nslots;
}

#ifdef LINE_INFO_COLUMN
#define SET_RUN(screen, vpos, run, type, len, face, class, b_or_e, pix) \
  { \
     register struct run* this \
       = &(SCREEN_DESIRED_GLYPHS ((s))->face_list[(vpos)][(run)]); \
     this->type = (type);              \
     this->length = (len);             \
     this->faceptr = (face);           \
     this->w = 0;                      \
     this->bufp = 0;                   \
     this->class = (class);            \
     this->begin_p = (b_or_e);         \
     this->pix_length = (pix);         \
     this->lineinfo_glyph_index = -1;  \
  }
#else
#define SET_RUN(screen, vpos, run, type, len, face, class, b_or_e, pix) \
  { \
     register struct run* this \
       = &(SCREEN_DESIRED_GLYPHS ((s))->face_list[(vpos)][(run)]); \
     this->type = (type);              \
     this->length = (len);             \
     this->faceptr = (face);           \
     this->w = 0;                      \
     this->bufp = 0;                   \
     this->class = (class);            \
     this->begin_p = (b_or_e);         \
     this->pix_length = (pix);         \
  }
#endif

static void
overlay_glyph (s, vpos, g, type, class, faceptr, column, begin_p)
     register SCREEN_PTR s;
     int vpos, type, column;
     GLYPH g;
     Lisp_Object class;
     struct face *faceptr;
     int begin_p;
{
  register int run = 0;
  register int got = SCREEN_DESIRED_GLYPHS (s)->face_list[vpos][run].length;
  int nruns = SCREEN_DESIRED_GLYPHS (s)->nruns[vpos];
  register struct run this_run;
  int old_pix_width, new_pix_width;

  while (column >= got)
    {
      run++;
      if (run >= nruns)
	/* abort (); */
	return;
      got += SCREEN_DESIRED_GLYPHS (s)->face_list[vpos][run].length;
    }
  this_run = SCREEN_DESIRED_GLYPHS (s)->face_list[vpos][run];

  if (SCREEN_IS_X (s))
    {
      GLYPH old_glyph = SCREEN_DESIRED_GLYPHS (s)->glyphs[vpos][column];
      old_pix_width = glyph_pixel_width (old_glyph, this_run.type,
					 this_run.faceptr);
      new_pix_width = glyph_pixel_width (g, type, faceptr);
    }

  if (type != this_run.type || !EQ (class, this_run.class)
      || faceptr != this_run.faceptr)
    {
      if (this_run.length == 1)
	{
	  /* Simply switch the type */
	  SET_RUN (s, vpos, run, type, 1, faceptr, class, begin_p,
		   new_pix_width);
	}
      else if (column == got - 1)
	{
	  /* Last glyph of this run */
	  right_shift_runs (s, vpos, run + 1, 1);
	  SCREEN_DESIRED_GLYPHS (s)->face_list[vpos][run].length--;
	  SCREEN_DESIRED_GLYPHS (s)->face_list[vpos][run].pix_length
	    -= old_pix_width;
	  run++;
	  SET_RUN (s, vpos, run, type, 1, faceptr, class, begin_p,
		   new_pix_width);
	}
      else if (column == got - this_run.length)
	{
	  /* First glyph of this run */
	  right_shift_runs (s, vpos, run, 1);
	  SCREEN_DESIRED_GLYPHS (s)->face_list[vpos][run + 1].length--;
	  SCREEN_DESIRED_GLYPHS (s)->face_list[vpos][run + 1].pix_length
	    -= old_pix_width;
	  SET_RUN (s, vpos, run, type, 1, faceptr, class, begin_p,
		   new_pix_width);
	}
      else
	{
	  /* Middle of the run */
	  /* Let's just lose for the moment, as we shouldn't arrive here. */
	  /* abort (); */
	  return;
	}
    }

  SCREEN_DESIRED_GLYPHS (s)->glyphs[vpos][column] = g;
  SCREEN_DESIRED_GLYPHS (s)->pix_width[vpos]
    += (new_pix_width - old_pix_width);
}

/* This appears to be dead code.  There are no references to it. */
#if 0
int
extract_glyph (desired_glyphs, vpos)
     register struct screen_glyphs *desired_glyphs;
     register vpos;
{
  register int last_run = desired_glyphs->nruns[vpos] - 1;
  register n = desired_glyphs->used[vpos] - 1;
  register int pix_width;
  register int val = 0;

  if (desired_glyphs->face_list[vpos][last_run].type == font)
    val = 1;

  if (desired_glyphs->face_list[vpos][last_run].length > 1)
    {
      desired_glyphs->face_list[vpos][last_run].length--;
      switch (desired_glyphs->face_list[vpos][last_run].type)
	{
	case font:
	  pix_width
	    = X_CHAR_WIDTH 
              (desired_glyphs->face_list[vpos][last_run].faceptr->font,
               ((desired_glyphs->glyphs[vpos][n] == TABGLYPH)
                ? ' '
                : desired_glyphs->glyphs[vpos][n]));
	  break;

	case glyph:
	  pix_width = x_bitmaps[desired_glyphs->glyphs[vpos][n]].width;
	  break;

	case space: /* Should never have space run whose len is > 1. */
	case column_glyph:
	default:
	  abort ();
	}
      desired_glyphs->face_list[vpos][last_run].pix_length -= pix_width;
    }
  else
    {
      pix_width = desired_glyphs->face_list[vpos][last_run].pix_length;
      desired_glyphs->nruns[vpos]--;
    }
  desired_glyphs->used[vpos]--;
  desired_glyphs->pix_width[vpos] -= pix_width;

  return val;
}
#endif

#ifdef HAVE_X_WINDOWS
#define LEFT_OF_MARGIN \
(SCREEN_IS_X (s) \
 ? (desired_glyphs->pix_width[vpos] < MAX_LINE_WIDTH (s)) \
 : (p1 < endp))
#else
#define LEFT_OF_MARGIN (p1 < endp)
#endif /* HAVE_X_WINDOWS */

/* Display one line of window w, starting at position START in W's buffer.
   Display starting at horizontal position HPOS, which is normally zero
   or negative.  A negative value causes output up to hpos = 0 to be discarded.
   This is done for negative hscroll, or when this is a continuation line
   and the continuation occurred in the middle of a multi-column character.

   TABOFFSET is an offset for ostensible hpos, used in tab stop calculations.

   Display on position VPOS on the screen.  (origin 0).

   Returns a STRUCT POSITION giving character to start next line with
   and where to display it, including a zero or negative hpos.
   The vpos field is not really a vpos; it is 1 unless the line is continued */

struct position val_display_text_line;


#ifdef LINE_INFO_COLUMN
#define TABOFFSET ((s->display.x->line_info_column_width > 0) \
		   ? taboffset - 1 : taboffset)
#define INFO_COLUMN_ADJUST(col) ((col) + 1)
#else
#define TABOFFSET 0
#define INFO_COLUMN_ADJUST(col) ((col))
#endif

/* Construct a display line from buffer text. */
static struct position *
display_text_line (w, start, vpos, hpos, taboffset)
     struct window *w;
     int start;
     int vpos;
     int hpos;
     int taboffset;
{
  struct position val;

  SCREEN_PTR s = XSCREEN (w->screen);
  register struct screen_glyphs *desired_glyphs = SCREEN_DESIRED_GLYPHS (s);
  struct buffer *buffer = XBUFFER (w->buffer);
  register struct Lisp_Vector *dp = window_display_table (w);

  int hscroll = XINT (w->hscroll);
  int truncate = hscroll
    || (truncate_partial_width_windows
	&& XFASTINT (w->width) < SCREEN_WIDTH (s))
      || !NILP (current_buffer->truncate_lines);

#if 0
  int selectively_truncated = 0;
#endif

  int start_column = XINT (w->left);
  int leftmost_column = start_column;
  int skip_columns = (hpos < 0 ? - hpos : 0);
  int window_column_width = XFASTINT (w->width)
    - (leftmost_column + XFASTINT (w->width) != SCREEN_WIDTH (s));

  GLYPH continuer = GLYPH_FROM_CHAR ('\\');
  GLYPH truncator = GLYPH_FROM_CHAR ('$');
#if 0
  GLYPH struncator = GLYPH_FROM_CHAR ('.');
#endif

  int buffer_position, pause, column, cpos, last_position;
  struct face *this_face = &SCREEN_NORMAL_FACE (s);

  register int line_width = 0;	/* Current total width of the line. */
  /* max_width is widest possible line terminated by newline */
  register int max_width;
  /* max_truncated_or_continued_width is widest possible line that is
     truncated or continued */
  int max_truncated_or_continued_width;
  register int this_width;	/* Width of last run of glyphs we examined. */
  int position_before_continuer = 0;
  int column_before_continuer;
  int cpos_before_continuer;

  /* Get the display line. */
  get_display_line (s, vpos, start_column);

  if (MINI_WINDOW_P (w) && start == 1 && vpos == XFASTINT (w->top))
    {
      int minibuf_len = (minibuf_prompt ? strlen (minibuf_prompt) : 0);
      install_first_runs (s, vpos, 1 - minibuf_len, w);
      if (minibuf_prompt)
	start_column = display_string (w, vpos, minibuf_prompt, hpos,
				       (!truncate ? continuer : truncator),
				       -1, -1, &SCREEN_NORMAL_FACE (s),
				       &minibuf_prompt_pix_width);
      minibuf_prompt_width = start_column - leftmost_column;
      SCREEN_DESIRED_GLYPHS (s)->bufp[vpos] = 1 - minibuf_len;
      if (SCREEN_IS_TERMCAP (s))
	line_width = minibuf_prompt_width;
    }
  else
    /* Set up the buffer pointer, window, etc.  */
    install_first_runs (s, vpos, start, w);

  if (SCREEN_IS_TERMCAP (s))
    {
      max_width = leftmost_column + window_column_width - 1;
      max_truncated_or_continued_width = max_width;
      line_width += start_column;
    }
  else
    {
      max_width = MAX_LINE_WIDTH (s) - EOL_CURSOR_WIDTH;
      max_truncated_or_continued_width = (truncate ? TRUNCATE_WIDTH (s)
					  : CONTINUE_WIDTH (s));
      line_width = desired_glyphs->pix_width[vpos];
    }

  column = 0;	/* Start counting for tabs at 0, even when wrapped */
  cpos = 0;	/* Columns including glyphs */
  last_position = buffer_position = start;
  pause = ZV;

  /* Deal with the overlay arrow. */
  if (XTYPE (Voverlay_arrow_position) == Lisp_Marker
      && current_buffer == XMARKER (Voverlay_arrow_position)->buffer
      && start == marker_position (Voverlay_arrow_position)
      && XTYPE (Voverlay_arrow_string) == Lisp_String
      && ! overlay_arrow_seen)
    {
      if (SCREEN_IS_TERMCAP (s))
	{
	  unsigned char *arrow = XSTRING (Voverlay_arrow_string)->data;
	  int arrow_len = XSTRING (Voverlay_arrow_string)->size;

	  while (arrow_len--)
	    {
	      append_glyph (s, vpos, GLYPH_FROM_CHAR (*arrow++), font, Qnil,
			    this_face, 0);
	      /* column++; */ /* The overlay arrow doesn't count in tabs */
	      cpos++;
	    }
	}
      else
	{
	  append_glyph (s, vpos, RARROW_BITMAP, glyph, Qnil, this_face, 0);
	  /* column++; */ /* The overlay arrow doesn't count in tabs */
	  cpos++;
	}
      overlay_arrow_seen = 1;
    }

  

  {
    register struct glyphs_from_chars *glyphs = 0;
    register int reusing_glyphs = 0;
    int screen_is_x = SCREEN_IS_X (s);
    XFontStruct *this_font = 0;
    int default_char_width = 0;

    while (1)
      {
        register int n_things;
        register GLYPH *gp;
        int save_column_before_continuer, save_cpos_before_continuer;
        UCHAR c;

        save_column_before_continuer = column;
        save_cpos_before_continuer = cpos;

        if (buffer_position == pause)
          {
            /* End of visible text */
            break;
          }

	c = BUF_CHAR_AT (buffer, buffer_position);

#if 0	/* This doesn't work */
	/* Handle the ^M variety of selective display... */
	if (c == 015 &&
	    !NILP (buffer->selective_display) &&
	    XTYPE (buffer->selective_display) != Lisp_Int)
	  {
	    while (buffer_position < BUF_ZV (buffer) &&
		   BUF_CHAR_AT (buffer, buffer_position) != '\n')
	      buffer_position++;
	    c = '\n';
	    selectively_truncated = 1;
	  }
	else if (c == '\n' &&
		 XTYPE (buffer->selective_display) == Lisp_Int)
	  {
	    /* #### */
	  }
#endif

        if (glyphs && 
            (buffer_position <= glyphs->run_pos_upper) &&
            (buffer_position >= glyphs->run_pos_lower) &&
            (((((c >= 040) && (c < 0177)) ||            /* Normal character. */
	       (!EQ (buffer->ctl_arrow, Qnil) &&	/* 8-bit display */
		!EQ (buffer->ctl_arrow, Qt) &&
		c >= 0240)) &&				/* Normal + 8th bit */
              (dp == 0 || NILP (DISP_CHAR_ROPE (dp, c)))) ||
             /* newline */
             (c == '\n') ||
             /* or a tab */
             (c == '\t')))
          {
            /* inside a run, so update the glyphs structure "by hand" */

            reusing_glyphs = 1;
#ifdef LINE_INFO_COLUMN
            glyphs->info_column_glyphs = Qnil;
#endif
            glyphs->begin_or_end = 0;
            glyphs->n_nonfont = 0;
            glyphs->glyphs = measure_glyphs;
            glyphs->c = c;

            if (c == '\n')
              {
                glyphs->pixel_width = 0;
                glyphs->columns = 0;
#ifdef HAVE_X_WINDOWS
                displayed_glyphs.pixel_width = 0;
#endif
              }
            else if (c == '\t')
              {
                int tab_width = ((XINT (buffer->tab_width) <= 0
                                  && XINT (buffer->tab_width) > 20)
                                 ? 8
                                 : XINT (buffer->tab_width));
                register int j = 
                  tab_width - ((column + TABOFFSET + hscroll - (hscroll > 0))
                               % tab_width);

                glyphs->columns = j;
#ifdef HAVE_X_WINDOWS
                if (screen_is_x)
                  {
                    register int pix_width;

                    if (this_font->per_char)
                      pix_width = x_char_info(this_font, ' ')->width;
                    else
                      pix_width = default_char_width;
                    displayed_glyphs.pixel_width = j * pix_width;
                  }
#endif
                while (j--)
                  GLYPH_SET_VALUE (measure_glyphs[j], TABGLYPH);
              }
            else
              {
                GLYPH_SET_VALUE (*measure_glyphs, GLYPH_FROM_CHAR (c));
#ifdef HAVE_X_WINDOWS
                if (screen_is_x)
                  {
                    register int pix_width;

                    if (this_font->per_char)
                      pix_width = x_char_info(this_font, c)->width;
                    else
                      pix_width = default_char_width;
                    displayed_glyphs.pixel_width = pix_width;
                  }
#endif
                glyphs->columns = 1;
              }
          }
        else
          {
            glyphs = glyphs_from_bufpos (s, buffer, buffer_position,
                                         dp, hscroll, column, TABOFFSET,
                                         0, 0);
            reusing_glyphs = 0;

            if (this_font != glyphs->faceptr->font)
              {
                this_font = glyphs->faceptr->font;
                if (this_font->per_char)
                  {
                    int default_index = this_font->default_char;
                    int limit_index = 
                      this_font->max_char_or_byte2 - 
                        this_font->min_char_or_byte2;

                    if ((default_index >= 0) && 
                        (default_index < limit_index))
                      default_char_width = 
                        this_font->per_char[default_index].width;
                    else
                      default_char_width = 
                        this_font->per_char[0].width;
                  }
                else
                  default_char_width = 
                    this_font->min_bounds.width;
              }
          }

#ifdef LINE_INFO_COLUMN
        if (glyphs->info_column_glyphs != Qnil
            && desired_glyphs->nruns[vpos] >= 2
            && desired_glyphs->face_list[vpos][1].type == column_glyph)
          {
            desired_glyphs->face_list[vpos][1].class = glyphs->begin_class[0];
            if (XTYPE (glyphs->info_column_glyphs) == Lisp_Int)
              desired_glyphs->face_list[vpos][1].lineinfo_glyph_index
                = XINT (glyphs->info_column_glyphs);
            else
              desired_glyphs->face_list[vpos][1].lineinfo_glyph_index = -1;
          }
#endif

        n_things = glyphs->columns;
        this_face = glyphs->faceptr;
        if (SCREEN_IS_TERMCAP (s))
          this_width = n_things;
        else
          this_width = glyphs->pixel_width;

        /* Handle skip columns */
        gp = glyphs->glyphs;
        if (skip_columns)
          {
            if (skip_columns >= n_things)
              skip_columns -= n_things;
            else
              while (skip_columns > 0 && n_things > 0)
                {
                  skip_columns--;
                  n_things--;
                  if (screen_is_x)
                    this_width -= glyph_pixel_width (*gp, font, this_face);
                  gp++;
                }
          }

        /* Handle the usual case, appending glyphs. */
        line_width += this_width;

        /* Insert any begin glyphs */
        if (glyphs->begin_or_end > 1)
          {
            register int i;
            for (i = 0; ! NILP (glyphs->begin_class[i]); i++)
              {
                append_glyph (s, vpos, *gp++, glyph,
                              glyphs->begin_class[i], this_face, 1);
                /* column++; */ /* Begin glyphs don't count in tabs */
                cpos++;
              }
          }

        /* We have to set cursor_vpos and cursor_hpos after all the
           begin glyphs and before any regular glyphs, unless we're at EOL */
        if (buffer_position == point && glyphs->c != '\n')
          {
            cursor_vpos = vpos;
            /* cpos now points to the first regular character */
            cursor_hpos = INFO_COLUMN_ADJUST (cpos);
          }

        n_things -= glyphs->n_nonfont;
        
        if (reusing_glyphs)
          {
            register struct screen_glyphs *glyphs = SCREEN_DESIRED_GLYPHS (s);
	    struct run *this_run =
	      &(glyphs->face_list[vpos][glyphs->nruns[vpos] - 1]);
            int n = glyphs->used[vpos];
      
            glyphs->used[vpos] = n + n_things;
            glyphs->glyphs[vpos][n + n_things] = NULL_GLYPH;
	    this_run->length += n_things;
	    column += n_things;
	    cpos += n_things;

            while (n_things--)
              {
                GLYPH g = *gp++;
                glyphs->glyphs[vpos][n++] = g;
              }
#ifdef HAVE_X_WINDOWS
            if (screen_is_x)                  
              {
                register int pix_width = displayed_glyphs.pixel_width;
                this_run->pix_length += pix_width;
                glyphs->pix_width[vpos] += pix_width;
              }
#endif
          }
        else
          while (n_things--)
            {
              append_glyph (s, vpos, *gp++, font, Qnil, this_face, 0);
              column++;
              cpos++;
            }

        /* Append any end glyphs */
        if (glyphs->begin_or_end & 0x1)
          {
            register int i;
            for (i = 0; ! NILP (glyphs->end_class[i]); i++)
              {
                append_glyph (s, vpos, *gp++, glyph,
                              glyphs->end_class[i], this_face, 0);
                /* column++; */ /* End glyphs don't count in tabs */
                cpos++;
              }
          }

        if (glyphs->c == '\n' && line_width <= max_truncated_or_continued_width)
          {
            /* End of line */
            if (buffer_position == point)
              {
                cursor_vpos = vpos;
                /* cpos now points to the first regular character */
                cursor_hpos = INFO_COLUMN_ADJUST (cpos);
              }
            break;
          }
 
        last_position = buffer_position;
        /* This should deal with a single character line that
           will wrap, since that will lose here, causing infinite
           wrapping */
        if ((!position_before_continuer)
            && (line_width > max_truncated_or_continued_width))
          {
            position_before_continuer = buffer_position;
            column_before_continuer = save_column_before_continuer;
            cpos_before_continuer = save_cpos_before_continuer;
          }
        if (line_width > max_truncated_or_continued_width)
          break;
        buffer_position++;
      }
  }

  val.hpos = - XINT (w->hscroll);
  if (val.hpos)
    val.hpos++;
  val.vpos = 1;

  if (buffer_position == ZV)	                 /* Quit due to EOT */
    {
      /* Don't miss point if it's after [visble] buffer text. */
      if (cursor_vpos < 0 && start <= point && point <= buffer_position)
	{
	  cursor_vpos = vpos;
	  cursor_hpos = INFO_COLUMN_ADJUST (cpos);
	}
      last_position = buffer_position;
    }
  else if (line_width > max_truncated_or_continued_width)
    {				/* Exceeded screen width */
      if (truncate)
	{
	  if (SCREEN_IS_X (s))
	    {
	      while (cpos > cpos_before_continuer)
		{
		  remove_glyph (s, vpos);
		  cpos--;
		}
	      buffer_position = position_before_continuer;
	      column = column_before_continuer;
	      /* Fix this to add space to align on right margin */
	      append_glyph (s, vpos, TRUNCATOR_BITMAP,
			    glyph, Qnil, this_face, 0);
	    }
	  else
	    append_glyph (s, vpos, truncator, font, Qnil, this_face, 0);

	  {
	    int newline_not_found;
	    last_position = 
              scan_buffer ('\n', buffer_position, 1, &newline_not_found);
	    if (point >= buffer_position
		&& (point < last_position
		    || (point == last_position
			&& newline_not_found)))
	      {
		cursor_vpos = vpos;
		cursor_hpos = INFO_COLUMN_ADJUST (cpos + 1);
	      }
	    val.hpos = (hscroll ? 1 - hscroll : 0);
	  }
	}
      else
	{
	  if (SCREEN_IS_X (s))
	    {
	      while (cpos > cpos_before_continuer)
		{
		  remove_glyph (s, vpos);
		  cpos--;
		}
	      /* Maybe should add space to align on right margin */
	      append_glyph 
                (s, vpos, CONTINUER_BITMAP, glyph, Qnil, this_face, 0);
	    }
	  else
	    append_glyph (s, vpos, continuer, font, Qnil, this_face, 0);

	  val.vpos = 0;
	  last_position = position_before_continuer;
	}
    }
#if 0	/* this doesn't work */
  else if (selectively_truncated &&
	   !NILP (current_buffer->selective_display_ellipses))
    {
      int i;
      if (column < window_column_width)
	append_glyph (s, vpos, struncator, font, Qnil, this_face, 0),
	column++;
      if (column < window_column_width)
	append_glyph (s, vpos, struncator, font, Qnil, this_face, 0),
	column++;
      if (column < window_column_width)
	append_glyph (s, vpos, struncator, font, Qnil, this_face, 0),
	column++;
      last_position = buffer_position + 1;
    }
#endif
  else				/* New line. */
    {
      last_position = buffer_position + 1;
    }

  /* If selected window and point is on this line,
     set global cursor X and Y position.  Also, set this_line_*
     variables for the one-line optimization. */
  if (cursor_vpos == vpos)
    {
      cursor_hpos += start_column;
      if (cursor_hpos < 0)
	cursor_hpos = INFO_COLUMN_ADJUST (0);
      if (SCREEN_IS_TERMCAP (s)
	  && cursor_hpos >= leftmost_column + window_column_width)
	cursor_hpos = window_column_width - 1;

      if (w == XWINDOW (selected_window))
	{
	  SCREEN_CURSOR_Y (s) = cursor_vpos;
	  SCREEN_CURSOR_X (s) = cursor_hpos;

	  /* Line is not continued and did not start in middle of character */
	  if (! skip_columns && val.vpos)
	    {
	      this_line_bufpos = start;
	      this_line_buffer = current_buffer;
	      this_line_vpos = cursor_vpos;
	      this_line_start_hpos = hpos;
	      this_line_endpos = Z - last_position;
	    }
	  else
	    this_line_bufpos = 0;
	}
    }

  /* If hscroll and line not empty, insert truncation-at-left marker */
  if (hscroll && desired_glyphs->used[vpos] > 0)
    overlay_glyph (s, vpos, CONTINUER_BITMAP, font, Qnil, this_face,
		   INFO_COLUMN_ADJUST (0), 0);
  
  /* If w is a vertical window, but not rightmost, insert spaces, then
     insert the '|' separator. */
  if (SCREEN_IS_TERMCAP (s)
      && (leftmost_column + window_column_width != SCREEN_WIDTH (s)))
    {
      while (desired_glyphs->used[vpos] <= max_truncated_or_continued_width)
	append_glyph (s, vpos, SPACEGLYPH, font, Qnil, this_face, 0);
      append_glyph (s, vpos, GLYPH_FROM_CHAR ('|'), font, Qnil, this_face, 0);
    }

  desired_glyphs->glyphs[vpos][desired_glyphs->used[vpos]] = 0;

  if (SCREEN_IS_X (s) && desired_glyphs->pix_height[vpos] == 0)
    {				/* Set the height of an empty line */
      desired_glyphs->pix_height[vpos] = s->display.x->text_height;
      desired_glyphs->max_ascent[vpos]
	= FONT_ASCENT (SCREEN_NORMAL_FACE (s).font);
    }

  val.bufpos = last_position;
  val_display_text_line = val;

  return &val_display_text_line;
}

#ifdef LINE_INFO_COLUMN
void 
add_in_column_glyph (desired_glyphs, vpos, element, class)
     struct screen_glyphs *desired_glyphs;
     int vpos;
     Lisp_Object element;
     Lisp_Object class;
{
  /* the column glyph is always the second glyph in the line */
  struct run* r;
  if (desired_glyphs->nruns [vpos] >= 2)
    {
      r = &desired_glyphs->face_list[vpos][1];
      if (r->type == column_glyph)
	{
	  r->class = class;
	  r->lineinfo_glyph_index = (XTYPE (element) == Lisp_Int ?
				     XINT(element) : -1);
	}
    }
}
#endif



static char *
decode_mode_spec (w, c, maxwidth)
     struct window *w;
     register char c;
     register int maxwidth;
{
  Lisp_Object obj = Qnil;
  SCREEN_PTR scr = XSCREEN (WINDOW_SCREEN (w));
  char *decode_mode_spec_buf
    = (char *) SCREEN_TEMP_GLYPHS (scr)->total_contents;
  struct buffer* buffer = XBUFFER (w->buffer);

  if (maxwidth > SCREEN_WIDTH (scr))
    maxwidth = SCREEN_WIDTH (scr);

  switch (c)
    {
    case 'b': 
      obj = buffer->name;
      break;

    case 'f': 
      obj = buffer->filename;
      break;

    case 'm': 
      obj = buffer->mode_name;
      break;

    case 'n':
      if (BUF_BEGV (buffer) > BUF_BEG (buffer)
	  || BUF_ZV (buffer) < BUF_Z (buffer))
	return " Narrow";
      break;

    case '*':
      return (!NILP (buffer->read_only)
	      ? "%"
	      : ((MODIFF > buffer->save_modified) ? "*" : "-"));

    case 's':
      /* status of process */
#ifdef subprocesses
      obj = Fget_buffer_process (w->buffer);
      if (NILP (obj))
	return "no process";
      obj = Fsymbol_name (Fprocess_status (obj));
      break;
#else
      return "no processes";
#endif /* subprocesses */

    case 'S':
      /* name of screen */
#ifdef MULTI_SCREEN
      obj = scr->name;
#endif
      break;

    case 'p':
      {
	int pos = marker_position (w->start);
	int total = BUF_ZV (buffer) - BUF_BEGV (buffer);

	if (XFASTINT (w->window_end_pos) <= BUF_Z (buffer) - BUF_ZV (buffer))
	  {
	    if (pos <= BUF_BEGV (buffer))
	      return "All";
	    else
	      return "Bottom";
	  }
	else if (pos <= BUF_BEGV (buffer))
	  return "Top";
	else
	  {
	    total = ((pos - BUF_BEGV (buffer)) * 100 + total - 1) / total;
	    /* We can't normally display a 3-digit number,
	       so get us a 2-digit number that is close.  */
	    if (total == 100)
	      total = 99;
	    sprintf (decode_mode_spec_buf, "%2d%%", total);
	    return decode_mode_spec_buf;
	  }
      }

    case '%':
      return "%";

    case '[': 
      {
	int i;
	char *p;

	if (command_loop_level > 5)
	  return "[[[... ";
	p = decode_mode_spec_buf;
	for (i = 0; i < command_loop_level; i++)
	  *p++ = '[';
	*p = 0;
	return decode_mode_spec_buf;
      }

    case ']': 
      {
	int i;
	char *p;

	if (command_loop_level > 5)
	  return " ...]]]";
	p = decode_mode_spec_buf;
	for (i = 0; i < command_loop_level; i++)
	  *p++ = ']';
	*p = 0;
	return decode_mode_spec_buf;
      }

    case '-':
      return "--------------------------------------------------------------------------------------------------------------------------------------------";
    }

  if (XTYPE (obj) == Lisp_String)
    return (char *) XSTRING (obj)->data;
  else
    return "";
}

/* Display NULL-terminated STRING on one line of window W, starting at HPOS.
   Display at position VPOS.  Caller should have done get_display_line.

   TRUNCATE is GLYPH to display at end if truncated.  Zero for none.

   MINCOL is the first column ok to end at.  (Pad with spaces to this col.)
   MAXCOL is the last column ok to end at.  Truncate here.
     -1 for MINCOL or MAXCOL means no explicit minimum or maximum.
   Both count from the left edge of the screen, as does HPOS.
   The right edge of W is an implicit maximum.
   If TRUNCATE is nonzero, the implicit maximum is one column before the edge.

   Display is performed using FACE, unless that is 0, then the screen
   default face is used.  Each new string on the smae line consitutes a
   different run.

   Returns ending hpos on the line.

   If PIX_WIDTH_PTR is nonzero, it is a pointer to an int variable that
   will be set to the pixel width of the string.   */

static int
display_string (w, vpos, string, hpos, truncate,
		mincol, maxcol, this_face, pix_width_ptr)
     struct window *w;
     unsigned char *string;
     int vpos, hpos;
     GLYPH truncate;
     int mincol, maxcol;
     struct face *this_face;
     int *pix_width_ptr;
{
  int hscroll = XINT (w->hscroll);
  int tab_width = XINT (current_buffer->tab_width);
  SCREEN_PTR s = XSCREEN (WINDOW_SCREEN (w));
  struct screen_glyphs *desired_glyphs = SCREEN_DESIRED_GLYPHS (s);
  register struct Lisp_Vector *dp = 0;
  register int end;
  register GLYPH *g
    = (GLYPH *) alloca (strlen ((char *) string) * sizeof (GLYPH));
  int pix_width = 0;

  if (tab_width <= 0 || tab_width > 20) tab_width = 8;

  /* Select a face */
  if (! this_face)
    this_face = &SCREEN_NORMAL_FACE (s);

#ifdef HAVE_X_WINDOWS
  if (SCREEN_IS_X (s))
    {
      if (this_face->font == (XFontStruct*)(~0) || !this_face->font)
	abort ();
      if (desired_glyphs->used[vpos] == 0)
	desired_glyphs->pix_height[vpos] = FONT_HEIGHT (this_face->font);
      else
	desired_glyphs->pix_height[vpos]
	  = max (FONT_HEIGHT (this_face->font),
                 desired_glyphs->pix_height[vpos]);
      desired_glyphs->max_ascent[vpos]
	= max (FONT_ASCENT (this_face->font), 
               desired_glyphs->max_ascent[vpos]);
    }
#endif

  /* Use the standard display table, not the window's display table.
     We don't want the mode line in rot13.  */
  if (XTYPE (Vstandard_display_table) == Lisp_Vector
      && XVECTOR (Vstandard_display_table)->size == DISP_TABLE_SIZE)
    dp = XVECTOR (Vstandard_display_table);

  while (desired_glyphs->used[vpos] < hpos)
    append_glyph (s, vpos, SPACEGLYPH, font, Qnil, this_face, 0);

  end = XINT (w->left) + XINT (w->width);
  if (maxcol > 0)
    end = min (maxcol, end);

  while (*string)
    {
      register struct glyphs_from_chars *glyphs;
      register int this_len;
      register GLYPH *gp = g;

      glyphs = glyphs_from_char (s, *string, g, tab_width,
				 buffer_defaults.ctl_arrow,
				 this_face, dp, hscroll, hpos, 0,
				 SCREEN_IS_X (s));
      this_len = glyphs->columns;
      pix_width += glyphs->pixel_width;
      if (hpos + this_len > end)
	break;

      gp = g;
      hpos += this_len;
      while (this_len--)
	{
	  if (hpos < desired_glyphs->used[vpos])
	    overlay_glyph (s, vpos, *gp, font, Qnil, this_face, hpos, 0);
	  else
	    append_glyph (s, vpos, *gp, font, Qnil, this_face, 0);
	  gp++;
	}

      string++;
    }

  /* Should adjust pix_width here ... */

  if (*string && truncate)
    {
      if (SCREEN_IS_TERMCAP (s))
	append_glyph (s, vpos, TRUNCATOR_BITMAP, glyph, Qnil, this_face, 0);
      else
	{
	  while (desired_glyphs->pix_width[vpos]
		 > MAX_LINE_WIDTH (s) - x_bitmaps[TRUNCATOR_BITMAP].width)
	    remove_glyph (s, vpos);
	  append_glyph (s, vpos, TRUNCATOR_BITMAP, glyph, Qnil, this_face, 0);
	}
    }
  else if (mincol > 0)
    {
      int window_edge = XFASTINT (w->width) + XFASTINT (w->left);

      while (desired_glyphs->used[vpos] < min (mincol, window_edge))
	append_glyph (s, vpos, SPACEGLYPH, font, Qnil, this_face, 0);

      if (window_edge != SCREEN_WIDTH (s))
	overlay_glyph (s, vpos, GLYPH_FROM_CHAR ('|'), font, Qnil,
		       this_face, window_edge - 1, 0);
    }

  if (pix_width_ptr)
    *pix_width_ptr = pix_width;

  return desired_glyphs->used[vpos];
}


/* Contribute ELT to the mode line for window W.
   How it translates into text depends on its data type.

   VPOS is the position of the mode line being displayed.

   HPOS is the position (absolute on screen) where this element's text
   should start.  The output is truncated automatically at the right
   edge of window W.

   DEPTH is the depth in recursion.  It is used to prevent
   infinite recursion here.

   MINENDCOL is the hpos before which the element may not end.
   The element is padded at the right with spaces if nec
   to reach this column.

   MAXENDCOL is the hpos past which this element may not extend.
   If MINENDCOL is > MAXENDCOL, MINENDCOL takes priority.
   (This is necessary to make nested padding and truncation work.)

   Returns the hpos of the end of the text generated by ELT.
   The next element will receive that value as its HPOS arg,
   so as to concatenate the elements.

   FN is a function which takes the same args as display_string:
   (w, vpos, string, hpos, truncate, mincol, maxcol, this_face, pix_width_ptr)
   It is used to actually output the data.
 */

static int
display_mode_element (fn, w, vpos, hpos, depth, minendcol, maxendcol, elt)
     int (*fn) ();
     struct window *w;
     register int vpos, hpos;
     int depth;
     int minendcol;
     register int maxendcol;
     register Lisp_Object elt;
{
  register SCREEN_PTR s = XSCREEN (WINDOW_SCREEN (w));

  /* check that the MODELINE_FACE is legal. */
  if (SCREEN_IS_X (s) &&
      (SCREEN_MODELINE_FACE (s).font == (XFontStruct*)(~0) ||
      !SCREEN_MODELINE_FACE (s).font))
    abort ();

 tail_recurse:
  if (depth > 10)
    goto invalid;

  depth++;

#ifdef SWITCH_ENUM_BUG
  switch ((int) XTYPE (elt))
#else
  switch (XTYPE (elt))
#endif
    {
    case Lisp_String:
      {
	/* A string: output it and check for %-constructs within it.  */
	register unsigned char c;
	register unsigned char *this = XSTRING (elt)->data;

	while (hpos < maxendcol && *this)
	  {
	    unsigned char *last = this;
	    while ((c = *this++) != '\0' && c != '%')
	      ;
	    if (this - 1 != last)
	      {
		register int lim = --this - last + hpos;
		hpos = fn (w, vpos, last, hpos, 0, hpos,
			   min (lim, maxendcol),
			   &SCREEN_MODELINE_FACE (s), 0);
	      }
	    else /* c == '%' */
	      {
		register int spec_width = 0;

		/* We can't allow -ve args due to the "%-" construct */
		/* Argument specifies minwidth but not maxwidth
		   (maxwidth can be specified by
		     (<negative-number> . <stuff>) mode-line elements) */

		while ((c = *this++) >= '0' && c <= '9')
		  {
		    spec_width = spec_width * 10 + (c - '0');
		  }

		spec_width += hpos;
		if (spec_width > maxendcol)
		  spec_width = maxendcol;

		if (c == 'M')
		  hpos = display_mode_element (fn, w, vpos, hpos, depth,
					       spec_width, maxendcol,
					       Vglobal_mode_string);
		else if (c != 0)
		  hpos = fn (w, vpos,
			     decode_mode_spec (w, c, maxendcol - hpos),
			     hpos, 0, spec_width, maxendcol,
			     &SCREEN_MODELINE_FACE (s), 0);
	      }
	  }
      }
      break;

    case Lisp_Symbol:
      /* A symbol: process the value of the symbol recursively
	 as if it appeared here directly.  Avoid error if symbol void.
	 Special case: if value of symbol is a string, output the string
	 literally.  */
      {
	register Lisp_Object tem;
	tem = Fboundp (elt);
	if (!NILP (tem))
	  {
	    tem = Fsymbol_value (elt);
	    /* If value is a string, output that string literally:
	       don't check for % within it.  */
	    if (XTYPE (tem) == Lisp_String)
	      hpos = fn (w, vpos, XSTRING (tem)->data,
			 hpos, 0, minendcol, maxendcol,
			 &SCREEN_MODELINE_FACE (s), 0);
	    /* Give up right away for nil or t.  */
	    else if (!EQ (tem, elt))
	      { elt = tem; goto tail_recurse; }
	  }
      }
      break;

    case Lisp_Cons:
      {
	register Lisp_Object car, tem;

	/* A cons cell: three distinct cases.
	   If first element is a string or a cons, process all the elements
	   and effectively concatenate them.
	   If first element is a negative number, truncate displaying cdr to
	   at most that many characters.  If positive, pad (with spaces)
	   to at least that many characters.
	   If first element is a symbol, process the cadr or caddr recursively
	   according to whether the symbol's value is non-nil or nil.  */
	car = XCONS (elt)->car;
	if (XTYPE (car) == Lisp_Symbol)
	  {
	    tem = Fboundp (car);
	    elt = XCONS (elt)->cdr;
	    if (XTYPE (elt) != Lisp_Cons)
	      goto invalid;
	    /* elt is now the cdr, and we know it is a cons cell.
	       Use its car if CAR has a non-nil value.  */
	    if (!NILP (tem))
	      {
		tem = Fsymbol_value (car);
		if (!NILP (tem))
		  { elt = XCONS (elt)->car; goto tail_recurse; }
	      }
	    /* Symbol's value is nil (or symbol is unbound)
	       Get the cddr of the original list
	       and if possible find the caddr and use that.  */
	    elt = XCONS (elt)->cdr;
	    if (NILP (elt))
	      break;
	    else if (XTYPE (elt) != Lisp_Cons)
	      goto invalid;
	    elt = XCONS (elt)->car;
	    goto tail_recurse;
	  }
	else if (XTYPE (car) == Lisp_Int)
	  {
	    register int lim = XINT (car);
	    elt = XCONS (elt)->cdr;
	    if (lim < 0)
	      /* Negative int means reduce maximum width.
		 DO NOT change MINENDCOL here!
		 (20 -10 . foo) should truncate foo to 10 col
		 and then pad to 20.  */
	      maxendcol = min (maxendcol, hpos - lim);
	    else if (lim > 0)
	      {
		/* Padding specified.  Don't let it be more than
		   current maximum.  */
		lim += hpos;
		if (lim > maxendcol)
		  lim = maxendcol;
		/* If that's more padding than already wanted, queue it.
		   But don't reduce padding already specified even if
		   that is beyond the current truncation point.  */
		if (lim > minendcol)
		  minendcol = lim;
	      }
	    goto tail_recurse;
	  }
	else if (XTYPE (car) == Lisp_String || XTYPE (car) == Lisp_Cons)
	  {
	    register int limit = 50;
	    /* LIMIT is to protect against circular lists.  */
	    while (XTYPE (elt) == Lisp_Cons && --limit > 0
		   && hpos < maxendcol)
	      {
		hpos = display_mode_element (fn, w, vpos, hpos, depth,
					     hpos, maxendcol,
					     XCONS (elt)->car);
		elt = XCONS (elt)->cdr;
	      }
	  }
      }
      break;

    default:
    invalid:
      return (fn (w, vpos, "*invalid*", hpos, 0,
		  minendcol, maxendcol, &SCREEN_MODELINE_FACE (s), 0));
    }

  if (minendcol > hpos)
    hpos = fn (w, vpos, "", hpos, 0, minendcol, -1,
	       &SCREEN_MODELINE_FACE (s), 0);

  return hpos;
}


/* Lucid addition */

Lisp_Object Vscreen_title_format, Vscreen_icon_title_format;
Lisp_Object Qscreen_title_format, Qscreen_icon_title_format;

#ifdef HAVE_X_WINDOWS

static char screen_title_buffer [1024];
static int screen_title_buffer_index;

static int
screen_title_display_string (w, vpos, string, hpos, truncate,
			     mincol, maxcol, this_face, pix_width_ptr)
     struct window *w;
     unsigned char *string;
     int vpos, hpos;
     GLYPH truncate;
     int mincol, maxcol;
     struct face *this_face;
     int *pix_width_ptr;
{
  int end = sizeof (screen_title_buffer) - 1;
  while (screen_title_buffer_index < hpos)
    screen_title_buffer [screen_title_buffer_index++] = ' ';
  if (maxcol > 0)
    end = min (maxcol, end);
  for (; *string && screen_title_buffer_index < end;
       *string++, screen_title_buffer_index++)
    screen_title_buffer [screen_title_buffer_index] = *string;
  while (screen_title_buffer_index < mincol)
    screen_title_buffer [screen_title_buffer_index++] = ' ';
  screen_title_buffer [screen_title_buffer_index] = '\0';
  return screen_title_buffer_index;
}

Lisp_Object
x_format_screen_title (s, w)
     struct screen *s;
     struct window *w;
{
  Lisp_Object title_format;
  Lisp_Object icon_format;
  Lisp_Object obuf = Fcurrent_buffer ();
  /* evaluate screen-title-format and screen-icon-title-format in the
     buffer of the selected window of the screen in question.
   */
  Fset_buffer (XWINDOW (s->selected_window)->buffer);
  title_format = Fsymbol_value (Qscreen_title_format);
  icon_format = Fsymbol_value (Qscreen_icon_title_format);
  Fset_buffer (obuf);
  screen_title_buffer_index = 0;
  display_mode_element (screen_title_display_string, w, 0, 0, 0, 0,
			sizeof (screen_title_buffer) -1,
			title_format);
  x_set_title_from_char (s, screen_title_buffer);

  if (!(EQ (icon_format, title_format) ||
	EQ (icon_format, Qscreen_title_format)))
    {
      screen_title_buffer_index = 0;
      display_mode_element (screen_title_display_string, w, 0, 0, 0, 0,
			    sizeof (screen_title_buffer) -1,
			    icon_format);
    }
  x_set_icon_name_from_char (s, screen_title_buffer);
  return Qnil;
}

#endif /* HAVE_X_WINDOWS */


/* Display the mode line for window w */

static void
display_mode_line (w)
     struct window *w;
{
  register int vpos = XFASTINT (w->height) + XFASTINT (w->top) - 1;
  register int left = XFASTINT (w->left);
  register int right = XFASTINT (w->width) + left;
  register SCREEN_PTR s = XSCREEN (WINDOW_SCREEN (w));

  if (vpos < 0)
    return;

  get_display_line (s, vpos, left);
  /* indicate this line is a mode line */
  SCREEN_DESIRED_GLYPHS (s)->bufp[vpos] = 0;
  display_mode_element (display_string, w, vpos, left, 0, right, right,
			current_buffer->mode_line_format);

#ifdef HAVE_X_WINDOWS
  if (SCREEN_IS_X (s))
      {
	if (s->desired_glyphs->pix_width[vpos]
	    < (PIXEL_WIDTH (s) - (2 * s->display.x->internal_border_width)))
	  {
	    int pix_len;
	    int nruns = s->desired_glyphs->nruns[vpos];
	    struct face *last_face
	      = s->desired_glyphs->face_list[vpos][nruns - 1].faceptr;

	    pix_len = 
              (PIXEL_WIDTH (s) - 
               (2 * s->display.x->internal_border_width)) - 
                 s->desired_glyphs->pix_width[vpos];
	    append_glyph (s, vpos, (GLYPH) pix_len, space, Qnil, last_face, 0);
	  }
      }
#endif
}

int
run_from_glyph_index (s, vpos, hpos)
     SCREEN_PTR s;
     register int vpos, hpos;
{
  register struct screen_glyphs *current_screen = SCREEN_CURRENT_GLYPHS (s);
  register struct run *face_list;
  register int run_len, this_run;

  /* Paranoia */
  if (hpos > current_screen->used[vpos])
    return 0;

  if (current_screen->nruns[vpos] == 1)
    return 0;
  if (hpos == current_screen->used[vpos])
    return current_screen->nruns[vpos] - 1;

  face_list = current_screen->face_list[hpos];
  if (face_list[0].length == 0)
    {
      /* Paranoia */
      if (hpos != 0)
	return 0;

      return 0;
    }

  this_run = 0;
  run_len = face_list[this_run].length;
  while (hpos >= run_len)
    {
      run_len += face_list[++this_run].length;
    }
  return this_run;
}

struct glyph_dimensions dimensions;

struct glyph_dimensions *
get_glyph_dimensions (s, hpos, vpos)
     SCREEN_PTR s;
     int hpos, vpos;
{
  register struct screen_glyphs *current_screen = SCREEN_CURRENT_GLYPHS (s);
  register struct run *face_list;
  register int run_len, this_run, pix_len;
  XFontStruct *default_font = s->display.x->font;

  if (!current_screen->enable[vpos] || s->garbaged)
    return 0;

  dimensions.top_left_y = current_screen->top_left_y[vpos];
  dimensions.top_left_x = current_screen->top_left_x[vpos];
  dimensions.height = current_screen->pix_height[vpos] - x_interline_space;

  /* Handle empty lines */
  if (current_screen->used[vpos] == 0
      || current_screen->enable[vpos] == 0
      || hpos >= current_screen->used[vpos])
    {
      if (current_screen->used[vpos] == 0 || current_screen->enable[vpos] == 0)
	{
	  dimensions.run = 0;
	}
      else
	{
	  dimensions.top_left_x += current_screen->pix_width[vpos];
	  dimensions.run = current_screen->nruns[vpos] - 1;
	}
      dimensions.width = X_DEFAULT_WIDTH (default_font);
      if (dimensions.top_left_y + dimensions.height
	  >= s->display.x->pixel_height)
	return &dimensions;

      return &dimensions;
    }

  face_list = current_screen->face_list[vpos];
  this_run = run_len = pix_len = 0;

  /* Advance to the correct run. */
  while (hpos >= run_len + face_list[this_run].length)
    {
      run_len += face_list[this_run].length;
      dimensions.top_left_x += face_list[this_run].pix_length;
      this_run++;
    }
  dimensions.run = this_run;

  switch (face_list[this_run].type)
    {
    case font:
      {
	register unsigned char this_ch = (current_screen->glyphs[vpos][hpos]
					  & 0377);
	int len = hpos - run_len;
	register int l = len;
	char *buf;
	register char *cp;
	register GLYPH *gp = &current_screen->glyphs[vpos][run_len];
	XFontStruct *this_font = face_list[this_run].faceptr->font;

	buf = (char *) alloca (len);
	cp = buf;

	if (this_font == 0)
	  this_font = default_font;

	while (l--)
	  *cp++ = *gp++;
	dimensions.top_left_x += XTextWidth (this_font, buf, len);
	dimensions.width = 
          X_CHAR_WIDTH (this_font,
                        (this_ch == TABGLYPH) ? ' ' : this_ch);
      }
      if (dimensions.top_left_y + dimensions.height
	  >= s->display.x->pixel_height)
	return &dimensions;

      return &dimensions;

    case column_glyph:
    case glyph:
    case space:
    case window:
      {
	dimensions.width = face_list[this_run].pix_length;
      }
      if (dimensions.top_left_y + dimensions.height
	  >= s->display.x->pixel_height)
	return &dimensions;

      return &dimensions;

    default:
      abort ();
    }
}


void
syms_of_xdisp ()
{
  staticpro (&last_arrow_position);
  staticpro (&last_arrow_string);
  last_arrow_position = Qnil;
  last_arrow_string = Qnil;

  staticpro (&echo_area_buffer);
  echo_area_buffer = Fget_buffer_create (build_string (" *echo area buffer*"));

  DEFVAR_LISP ("global-mode-string", &Vglobal_mode_string,
    "String displayed by mode-line-format's \"%m\" specification.");
  Vglobal_mode_string = Qnil;

  DEFVAR_LISP ("overlay-arrow-position", &Voverlay_arrow_position,
    "Marker for where to display an arrow on top of the buffer text.\n\
This must be the beginning of a line in order to work.\n\
See also `overlay-arrow-string'.");
  Voverlay_arrow_position = Qnil;

  DEFVAR_LISP ("overlay-arrow-string", &Voverlay_arrow_string,
    "String to display as an arrow.  See also `overlay-arrow-position'.");
  Voverlay_arrow_string = Qnil;

  DEFVAR_INT ("scroll-step", &scroll_step,
    "*The number of lines to try scrolling a window by when point moves out.\n\
If that fails to bring point back on screen, point is centered instead.\n\
If this is zero, point is always centered after it moves off screen.");

  DEFVAR_INT ("debug-end-pos", &debug_end_pos, "Don't ask");

  DEFVAR_BOOL ("truncate-partial-width-windows",
	       &truncate_partial_width_windows,
 "*Non-nil means truncate lines in all windows less than full screen wide.");
  truncate_partial_width_windows = 1;

  DEFVAR_BOOL ("mode-line-inverse-video", &mode_line_inverse_video,
    "*Non-nil means use inverse video for the mode line.");
  mode_line_inverse_video = 1;

  /* Lucid addition */
  DEFVAR_LISP ("screen-title-format", &Vscreen_title_format,
  "Controls the title of the X window corresponding to the selected screen.\n\
This is the same format as `mode-line-format'.");
  Vscreen_title_format = build_string ("%S: %b");
  Qscreen_title_format = intern ("screen-title-format");
  staticpro (&Qscreen_title_format);
  DEFVAR_LISP ("screen-icon-title-format", &Vscreen_icon_title_format,
  "Controls the title of the icon corresponding to the selected screen.\n\
See also the variable `screen-title-format'");
  Vscreen_icon_title_format = build_string ("%b");
  Qscreen_icon_title_format = intern ("screen-icon-title-format");

#ifdef MULTI_SCREEN
  defsubr (&Sredraw_screen);
#endif /* MULTI_SCREEN */
  defsubr (&Sredraw_display);
}

/* initialize the window system */
init_xdisp ()
{
  Lisp_Object root_window;
#ifndef COMPILER_REGISTER_BUG
  register
#endif /* COMPILER_REGISTER_BUG */
    struct window *mini_w;

  this_line_bufpos = 0;

  mini_w = XWINDOW (minibuf_window);
  root_window = mini_w->prev;

  echo_area_glyphs = 0;
  previous_echo_glyphs = 0;

  if (!noninteractive)
    {
      SCREEN_PTR s = XSCREEN (WINDOW_SCREEN (XWINDOW (root_window)));
      XFASTINT (XWINDOW (root_window)->top) = 0;
      set_window_height (root_window, SCREEN_HEIGHT (s) - 1, 0);
      XFASTINT (mini_w->top) = SCREEN_HEIGHT (s) - 1;
      set_window_height (minibuf_window, 1, 0);

      XFASTINT (XWINDOW (root_window)->width) = SCREEN_WIDTH (s);
      XFASTINT (mini_w->width) = SCREEN_WIDTH (s);
    }
}
