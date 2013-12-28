/* Generic scrollbar implementation.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.

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

#include "scrollbar.h"
#include "screen.h"
#include "xterm.h"
#include "dispextern.h"
#include "dispmisc.h"
#include "window.h"
#include "buffer.h"
#include "indent.h"
#include "lwlib.h"
#include "EmacsScreenP.h" /* so we can directly manipulate scrollbar width */
#include "commands.h"

#include <X11/StringDefs.h>

#include "EmacsManager.h"
#ifdef LWLIB_USES_MOTIF
# include <Xm/Xm.h>
# include <Xm/PanedW.h>
#else /* Athena */
# include <X11/Xmu/Converters.h>	/* For XtorientVertical */
# include <X11/Xaw/Paned.h>
#endif

#define min(a,b) ((a)<(b) ? (a) : (b))
#define max(a,b) ((a)>(b) ? (a) : (b))

#define DEFAULT_SCROLLBAR_WIDTH 15

#define SCROLLBAR_WIDTH(s) ((EmacsScreen) s->display.x->edit_widget)->emacs_screen.scrollbar_width

/* Used to prevent changing the size of the thumb while drag scrolling,
   under Motif.  This is necessary because the Motif scrollbar is
   incredibly stupid about updating the thumb and causes lots of flicker
   if it is done too often.
 */
static int inhibit_thumb_size_change;


/*
 * If w->sb_point is on the top line then return w->sb_point else
 * return w->start.  If flag, then return beginning point of line
 * which w->sb_point lies on.
 */
int
scrollbar_point (struct window *w, int flag)
{
  int start_pos, end_pos, sb_pos, shortage;
  Lisp_Object buf;
  struct buffer *b;

  if (NILP (w->buffer)) /* non-leaf window */
    return 0;

  start_pos = marker_position (w->start);
  sb_pos = marker_position (w->sb_point);

  if (!flag && sb_pos < start_pos)
    return start_pos;

  buf = get_buffer (w->buffer, 0);
  if (!NILP (buf))
    b = XBUFFER (buf);
  else
    return start_pos;

  if (flag)
    end_pos = scan_buffer (b, '\n', sb_pos, -1, &shortage, 0);
  else
    end_pos = scan_buffer (b, '\n', start_pos, 1, &shortage, 0);

  if (flag)
    return end_pos;
  else if (sb_pos > end_pos)
    return start_pos;
  else
    return sb_pos;
}

/*
 * This is a wrapper for the scroll-up and scroll-down functions to
 * set the current window to whichever window is actually being
 * scrolled.
 */
static void
scrollbar_page (struct screen *s, Lisp_Object win, int dir, Lisp_Object amount)
{
  Lisp_Object cur_select = SCREEN_SELECTED_WINDOW (s);
  struct screen *cur_screen = selected_screen;

  if (!EQ (cur_select, win) || (s != cur_screen))
    Fselect_window (win);

  if (dir == 1)
    scroll_command (amount, 1, 1);	/* no error */
  else
    scroll_command (amount, -1, 1);	/* no error */

  if (!EQ (cur_select, win) || (s != cur_screen))
    Fselect_window (cur_select);
}

/*
 * Redisplay has a number of optimizing assumptions involving the
 * selected window.  Scrollbars provide a way of breaking some of
 * these assumptions that was never possible before.  It is easiest
 * (for now) to just temporarily make the window which was just
 * scrolled the selected window, call redisplay, and restore state.
 * Of course, this doesn't really work all that well.  Sigh. */
static void
redisplay_scrollbar_window (struct screen *s, Lisp_Object win)
{
  Lisp_Object cur_select = SCREEN_SELECTED_WINDOW (s);
  struct screen *cur_screen = selected_screen;

  if (!EQ (cur_select, win) || (s != cur_screen))
    {
      lock_redisplay (REDISPLAY_LOCK_CURSOR);
      Fselect_window (win);
      redisplay ();
      Fselect_window (cur_select);
    }
  else
    {
      lock_redisplay (REDISPLAY_LOCK_COMPLETELY);
      redisplay ();
    }

  unlock_redisplay ();
}

/*
 * Gee, does everything assume interaction with the selected
 * window only, or what?
 */
static void
recenter_scrollbar_window (struct screen *s, Lisp_Object win)
{
  Lisp_Object cur_select = SCREEN_SELECTED_WINDOW (s);
  struct screen *cur_screen = selected_screen;

  if (!EQ (cur_select, win) || (s != cur_screen))
    {
      lock_redisplay (REDISPLAY_LOCK_CURSOR);
      Fselect_window (win);
      Frecenter (Qzero);
      redisplay ();
      Fselect_window (cur_select);
    }
  else
    {
      lock_redisplay (REDISPLAY_LOCK_COMPLETELY);
      Frecenter (Qzero);
    }

  unlock_redisplay ();
}

static void
scrollbar_move_to_window_line (struct screen *s, Lisp_Object win, int line)
{
  Lisp_Object cur_select = SCREEN_SELECTED_WINDOW (s);
  struct screen *cur_screen = selected_screen;

  if (!EQ (cur_select, win) || (s != cur_screen))
    {
      lock_redisplay (REDISPLAY_LOCK_CURSOR);
      Fselect_window (win);
      Fmove_to_window_line (make_number (line));
      redisplay ();
      Fselect_window (cur_select);
    }
  else
    {
      lock_redisplay (REDISPLAY_LOCK_COMPLETELY);
      Fmove_to_window_line (make_number (line));
    }

  unlock_redisplay ();
}

/*
 * If the original point is still visible, put the cursor back there.
 * Otherwise, when scrolling down stick it at the beginning of the
 * first visible line and when scrolling up stick it at the beginning
 * of the last visible line.
 */
static void
scrollbar_reset_cursor (struct screen *s, Lisp_Object win, int orig_pt)
{
  Lisp_Object cur_select = SCREEN_SELECTED_WINDOW (s);
  struct screen *cur_screen = selected_screen;
  struct buffer *b;
  Lisp_Object buf;
  int start_pos, end_pos;

  buf = get_buffer (XWINDOW (win)->buffer, 0);
  if (NILP (buf))
    return;
  else
    {
      b = XBUFFER (buf);
      start_pos = marker_position (XWINDOW (win)->start);
      end_pos = BUF_Z (b) - XWINDOW (win)->window_end_pos;
    }

  if (!EQ (cur_select, win) || (s != cur_screen))
    Fselect_window (win);

  if (orig_pt < start_pos)
    {
      Fgoto_char (make_number (start_pos));
    }
  else if (orig_pt >= end_pos)
    {
      Fmove_to_window_line (make_number (-1));
      Fbeginning_of_line (Qnil);
    }
  else
    Fgoto_char (make_number (orig_pt));

  if (!EQ (cur_select, win) || (s != cur_screen))
    Fselect_window (cur_select);
}

extern void pre_command_hook (void);
extern void post_command_hook (int old_region_p);
static void update_one_screen_scrollbars (struct screen *s);
static void update_one_screen_scrollbars_size (struct screen *s);
static void update_one_scrollbar_bs (struct screen *s, Widget sb_widget);

/*
 * This is the only callback provided.  It should be able to handle all
 * of the scrollbar events in scroll_action (see lwlib.h).  The client
 * data will be of type scroll_event (see lwlib.h). */
static void
update_scrollbar_callback (Widget widget, LWLIB_ID id, XtPointer client_data)
{
  scroll_event *data = (scroll_event *) client_data;
  struct screen *s = x_any_window_to_screen (XtWindow (widget));
  Lisp_Object win, buf;
  struct scrollbar_instance *instance;
  int orig_pt;
  int old_region_p = zmacs_region_active_p;

  if (!s)
    s = x_any_window_to_screen (XtWindow (XtParent (widget)));
  if (!s)
    return;

  instance = s->scrollbar_instances;
  win = Qnil;

  while (instance)
    {
      if (instance->scrollbar_id == id)
	{
	  win = real_window (instance->mirror, 1);
	  if (NILP (win))
	    return;
	  break;
	}
      instance = instance->next;
    }

  /* If the window has an hchild then it is actually not active so we
     can just exit quietly. */
  if (NILP (win) || !NILP (XWINDOW (win)->hchild))
    return;

  buf = get_buffer (XWINDOW (win)->buffer, 0);

  /* Multiple windows may be showing the same buffer so make sure we
     use the value of point in the window that is actually being
     scrolled. */
  orig_pt = XINT (Fwindow_point (win));
  
  switch (data->action)
    {
    case SCROLLBAR_LINE_UP:
      pre_command_hook ();
      scrollbar_page (s, win, -1, make_number (1));
      post_command_hook (old_region_p);
      break;

    case SCROLLBAR_LINE_DOWN:
      pre_command_hook ();
      scrollbar_page (s, win, 1, make_number (1));
      post_command_hook (old_region_p);
      break;

    case SCROLLBAR_PAGE_UP:
      pre_command_hook ();

      /* Motif and Athena scrollbars behave totally differently...  (wasn't
	 this just the kind of nonsense lwlib was supposed to abstract away?)
       */
#ifdef LWLIB_USES_MOTIF
      scrollbar_page (s, win, -1, Qnil);
#else /* Athena */
      {
	struct position pos;
	double tmp = ((double) data->slider_value /
		      (double) instance->data.scrollbar_height);
	double line = tmp * (double) window_displayed_height (XWINDOW (win),0);

	if (line > -1.0)
	  line = -1.0;
	scrollbar_move_to_window_line (s, win, 0);
	/* can't use Fvertical_motion() because it moves the buffer point
	   rather than the window's point */
	pos = *vmotion (XINT (Fwindow_point (win)), line,
			XINT (XWINDOW (win)->hscroll), win);
	Fset_window_point (win, make_number (pos.bufpos));
	recenter_scrollbar_window (s, win);
      }
#endif /* Athena */
      post_command_hook (old_region_p);
      break;

    case SCROLLBAR_PAGE_DOWN:
      pre_command_hook ();

#ifdef LWLIB_USES_MOTIF
      scrollbar_page (s, win, 1, Qnil);
#else /* Athena */
      {
	double tmp = ((double) data->slider_value /
		      (double) instance->data.scrollbar_height);
	double line = tmp * (double) window_displayed_height (XWINDOW (win),0);

	if (instance->data.maximum >
	    (instance->data.slider_size + instance->data.slider_position))
	  {
	    if (line < 1.0)
	      line = 1.0;
	    scrollbar_move_to_window_line (s, win, (int) line);
	    recenter_scrollbar_window (s, win);
	  }
      }
#endif
      post_command_hook (old_region_p);
      break;

    case SCROLLBAR_TOP:
      pre_command_hook ();
      Fset_window_point (win, Fpoint_min());
      recenter_scrollbar_window (s, win);
      /* post_command_hook ran at end of function */
      break;

    case SCROLLBAR_BOTTOM:
      pre_command_hook ();
      Fset_window_point (win, Fpoint_max());
      recenter_scrollbar_window (s, win);
      /* post_command_hook ran at end of function */
      break;

    case SCROLLBAR_CHANGE:
      inhibit_thumb_size_change = 0;
      break;

    case SCROLLBAR_DRAG:
      {
	int start_pos;
	int value;

	inhibit_thumb_size_change = 1;

#ifdef LWLIB_USES_MOTIF
	value = ((double)(instance->data.maximum - instance->data.minimum) *
		 (data->slider_value - instance->data.minimum)) /
		   (instance->data.maximum - instance->data.minimum -
		    instance->data.slider_size) + instance->data.minimum;
#else
	value = data->slider_value;
#endif

	if (value == instance->data.maximum)
	  value = instance->data.maximum - 1;

	pre_command_hook ();
	Fset_marker (XWINDOW (win)->sb_point, make_number (value), buf);
	start_pos = scrollbar_point (XWINDOW (win), 1);
	Fset_window_start (win, make_number (start_pos), Qnil);
	/* post_command_hook ran at end of function */
      }
      break;

    default:
      break;
    }

  redisplay_scrollbar_window (s, win);

  /*
   * The paging operations are identical to calling Fscroll_up and
   * Fscroll_down.  Doing additional work just screws things up.
   */
  if (data->action != SCROLLBAR_PAGE_UP &&
      data->action != SCROLLBAR_PAGE_DOWN &&
      data->action != SCROLLBAR_LINE_UP &&
      data->action != SCROLLBAR_LINE_DOWN)
    {
      scrollbar_reset_cursor (s, win, orig_pt);
      post_command_hook (old_region_p);
    }
}

extern LWLIB_ID new_lwlib_id (void);

/*
 * Create a new scrollbar for SCREEN.  Scrollbar's are only destroyed
 * when a screen is deleted.
 */
static struct scrollbar_instance *
create_scrollbar_instance (struct screen *s)
{
  char buffer[32];
  struct scrollbar_instance *instance =
    (struct scrollbar_instance *) xmalloc (sizeof (struct scrollbar_instance));

  instance->next = NULL;
  instance->mirror = 0;
  instance->scrollbar_is_active = 0;
  instance->scrollbar_instance_changed = 0;

  instance->data.line_increment = 0;
  instance->data.page_increment = 0;
  instance->data.minimum = 0;
  instance->data.maximum = 0;
  instance->data.slider_size = 0;
  instance->data.slider_position = 0;
  instance->data.scrollbar_height = 0;
  instance->data.scrollbar_pos = 0;

  instance->scrollbar_id = new_lwlib_id ();
  sprintf (buffer, "scrollbar_%d", instance->scrollbar_id);
  instance->scrollbar_name = xstrdup (buffer);
  instance->backing_store_initialized = 0;

  BLOCK_INPUT;
  instance->scrollbar_widget =
    lw_create_widget ("scrollbar", instance->scrollbar_name,
		      instance->scrollbar_id,
		      NULL, s->display.x->scrollbar_manager, 0,
		      update_scrollbar_callback, NULL, NULL);
  UNBLOCK_INPUT;

  return instance;
}

/*
 * Create a widget value structure for passing down to lwlib so that
 * it can update the scrollbar widgets.
 */
static widget_value *
scrollbar_instance_to_widget_values (struct scrollbar_instance *instance)
{
  widget_value *wv;

  if (instance == NULL)
    return NULL;

  BLOCK_INPUT;
  wv = malloc_widget_value ();
  UNBLOCK_INPUT;
  /* #### maybe should add malloc_scrollbar_values to resource these? */
  wv->scrollbar_data = (scrollbar_values *) malloc (sizeof (scrollbar_values));

  wv->name = instance->scrollbar_name;
  wv->value = 0;
  wv->key = 0;
  wv->enabled = instance->scrollbar_is_active;
  wv->selected = 0;
  wv->call_data = NULL;

  wv->scrollbar_data->line_increment = instance->data.line_increment;
  wv->scrollbar_data->page_increment = instance->data.page_increment;
  wv->scrollbar_data->minimum = instance->data.minimum;
  wv->scrollbar_data->maximum = instance->data.maximum;
  wv->scrollbar_data->slider_size = instance->data.slider_size;
  wv->scrollbar_data->slider_position = instance->data.slider_position;
  wv->scrollbar_data->scrollbar_height = instance->data.scrollbar_height;
  wv->scrollbar_data->scrollbar_pos = instance->data.scrollbar_pos;

  wv->next =
    scrollbar_instance_to_widget_values (instance->next);

  return wv;
}

static void
free_scrollbar_widget_value_tree (widget_value *wv)
{
  if (!wv) return;

  if (wv->next)
    {
      free_scrollbar_widget_value_tree (wv->next);
      wv->next = (widget_value *) NULL;
    }

  BLOCK_INPUT;
  if (! wv->scrollbar_data) abort ();
  /* #### maybe should add free_scrollbar_values to resource these?
     Does free_widget_value do something with the scrollbar_data?  Should it?
     */
  free (wv->scrollbar_data);
  wv->scrollbar_data = 0;
  free_widget_value (wv);
  UNBLOCK_INPUT;
}

#define UPDATE_DATA_FIELD(field, value) \
  if (instance->field != value) {\
    instance->field = value;\
    instance->scrollbar_instance_changed = 1;\
  }\

/*
 * Recursively update the list of scrollbars associated with a screen.
 * The value of instance passed in is either the next instance
 * structure to be updated or nil, indicating that if another scrollbar
 * needs to be updated it will have to be created first.
 */
static struct scrollbar_instance *
update_scrollbar_instance (struct screen *s, struct window *w,
			   struct scrollbar_instance *instance,
			   int next_pos)
{
  struct buffer *buf;
  int start_pos, end_pos, sb_pos, new_height;

  /*
   * If a window has vertical children, the scrollbars will actually
   * be associated with them or their descendants.
   */
  if (!NILP (w->vchild))
    {
      struct scrollbar_instance *first, *prev, *retval;
      struct window *cur;

      first = NULL;
      prev = instance;

      cur = XWINDOW (w->vchild);
      while (cur)
	{
	  retval = update_scrollbar_instance (s, cur, instance, next_pos);

	  if (retval && first == NULL)
	    first = retval;

	  if (prev == NULL)
	    {
	      prev = retval;
	    }
	  else if (prev == retval)
	    {
	      instance = prev->next;
	    }
	  else
	    {
	      prev->next = retval;
	      prev = retval;
	      instance = prev->next;
	    }

	  if (prev)
	    next_pos += prev->data.scrollbar_height;

	  /*
	   * Determine the next vertical child.  This is normally the
	   * next field of the current window.  The minibuffer is
	   * always the last window with a scrollbar.
	   */
	  if (!NILP (cur->next))
	    cur = XWINDOW (cur->next);
	  else if (NILP (cur->mini_p) && w == XWINDOW (SCREEN_ROOT_WINDOW (s)))
	    cur = XWINDOW (w->next);
	  else
	    cur = NULL;
	}

      return first;
    }

  /*
   * If no instance was passed in, create a new one.
   */
  if (instance == NULL)
    {
      instance = create_scrollbar_instance (s);
    }

  /*
   * Once we get to this point we are updating an actual scrollbar.
   */
  if (!NILP (w->buffer))
    {
      buf = XBUFFER (w->buffer);
      start_pos = marker_position (w->start);
      end_pos = BUF_Z (buf) - w->window_end_pos; /* This is a close approx. */
      sb_pos = scrollbar_point (w, 0);
      start_pos = sb_pos;
    }
  else
    {
      buf = 0;
      start_pos = 0;
      end_pos = 0;
      sb_pos = 0;
    }

  /*
   * The end position must be strictly greater than the start position,
   * at least for the Motify scrollbar.  It shouldn't hurt anything for
   * other scrollbar implmentations.
   */
  if (buf && (end_pos <= start_pos))
    end_pos = start_pos + 1;

  /*
   * The calculation of the scrollbar height is complete bullshit at
   * the moment.  Its complexity is mostly due to a mis-feature of the
   * EmacsScreen widget which will get corrected sometime in the
   * future.
   */
  new_height = w->pixheight;
  if (instance == s->scrollbar_instances || MINI_WINDOW_P (w))
    {
      Dimension sb_height, edit_height, edit_borderWidth;
      Position sb_y, edit_y;

      XtVaGetValues (s->display.x->scrollbar_manager,
		     XtNheight, &sb_height,
		     XtNy, &sb_y,
		     0);
      XtVaGetValues (s->display.x->edit_widget,
		     XtNheight, &edit_height,
		     XtNy, &edit_y,
		     XtNborderWidth, &edit_borderWidth,
		     0);

      new_height += SCREEN_INT_BORDER (s);

      if (instance == s->scrollbar_instances)
	new_height += (edit_y - sb_y);
      else
	new_height += (sb_height - edit_height - 2 * edit_borderWidth);
    }

  /*
   * The UPDATE_DATA_FIELD macro won't work for this because it is
   * a Lisp object.
   */
  if (!instance->mirror || XWINDOW (real_window (instance->mirror, 0)) != w)
    {
      instance->mirror = find_window_mirror (w);
      instance->scrollbar_instance_changed = 1;
    }
      
  /*
   * Only character-based scrollbars are implemented at the moment.
   * Line-based will be implemented in the future.
   */

  /* don't use UPDATE_DATA_FIELD because we want to set this without
     tripping the changed field. */
  instance->scrollbar_is_active = 1;
  UPDATE_DATA_FIELD (data.line_increment, 1);
  UPDATE_DATA_FIELD (data.page_increment, 1);
#ifdef LWLIB_USES_MOTIF
  if (!inhibit_thumb_size_change)
#endif
    {
      UPDATE_DATA_FIELD (data.scrollbar_height, new_height);
      UPDATE_DATA_FIELD (data.scrollbar_pos, next_pos);
    }

  /*
   * A disabled scrollbar has its slider sized to the entire height of
   * the scrollbar.  Currently the minibuffer scrollbar and all scrollbars
   * next to horizontally-split windows are disabled.
   */
  if (NILP (w->hchild) && !MINI_WINDOW_P (w))
    {
      int new_slider_size;

      if (! buf) abort ();
#ifdef LWLIB_USES_MOTIF
      if (!inhibit_thumb_size_change)
#endif
	{
	  UPDATE_DATA_FIELD (data.minimum, BUF_BEGV (buf));
	  UPDATE_DATA_FIELD (data.maximum,
			     max (BUF_ZV (buf),
				  instance->data.minimum + 1));
	  new_slider_size = min ((end_pos - start_pos),
				 (instance->data.maximum -
				  instance->data.minimum));
	  UPDATE_DATA_FIELD (data.slider_size, new_slider_size);
	  UPDATE_DATA_FIELD (data.slider_position, sb_pos);
	}
    }
  else
    {
      UPDATE_DATA_FIELD (data.minimum, 1);
      UPDATE_DATA_FIELD (data.maximum, 2);
      UPDATE_DATA_FIELD (data.slider_size, 1);
      UPDATE_DATA_FIELD (data.slider_position, 1);
      /* Actually, let's make these unusable scrollbars invisible. */
      /* No, that doesn't work because of FUCKING GEOMETRY MANAGEMENT
	 when there are nothing but horizontally split windows (ie no
	 scrollbars.)  Xt and Motif are such collossal steaming piles
	 of shit I can hardly believe it.
	 
	 Only make it invisible if it's the minibuffer scrollbar.
       */
      instance->scrollbar_is_active = !MINI_WINDOW_P (w);
    }

  /*
   * If the root window had no vertical children we still haven't
   * updated the minibuffer's scrollbar.  So do so now.
   */
  if (w == XWINDOW (SCREEN_ROOT_WINDOW (s)))
    {
      instance->next =
	update_scrollbar_instance (s, XWINDOW (w->next), instance->next,
				   instance->data.scrollbar_pos + new_height);
    }

  return instance;
}

/* For every screen, make sure all horizontal window areas have a
   scrollbar and that the scrollbars are up-to-date with the window view.
 */
static void
update_one_screen_scrollbars (struct screen *s)
{
  struct scrollbar_instance *instance, *cur_instance;

  BLOCK_INPUT;

  /* Mark all scrollbars on the screen inactive.  The scrollbar list
     may contain more scrollbars than are currently needed.
     update_scrollbar_instance will take care of marking those that
     are needed.
   */
  instance = s->scrollbar_instances;
  while (instance)
    {
      instance->scrollbar_is_active = 0;
      instance->mirror = 0;
      instance = instance->next;
    }

  if (s->scrollbar_instances)
    {
      widget_value *wv, *cur_wv;

      update_scrollbar_instance (s, XWINDOW (s->root_window),
				 s->scrollbar_instances, 0);

      /* Create a list of all widget_values for passing to
	 lwlib_modify_all_widgets.
	 */
      wv = scrollbar_instance_to_widget_values (s->scrollbar_instances);

      /* The widget_value tree doesn't contain the LWLIB_ID, so we actually
	 have to traverse down the scrollbar instance list to get it.
       */
      cur_wv = wv;
      cur_instance = s->scrollbar_instances;
      while (cur_instance)
	{
	  char active;
	  char managed;

	  if (!cur_wv)		/* length (wv) != length (instance) */
	    abort ();

	  if (cur_instance->scrollbar_instance_changed)
	    lw_modify_all_widgets (cur_instance->scrollbar_id, cur_wv, 0);
	  cur_instance->scrollbar_instance_changed = 0;

	  active = cur_instance->scrollbar_is_active;
	  managed = XtIsManaged (cur_instance->scrollbar_widget);

	  /*
	   * If the width gets changed while some scrollbars are
	   * unmanaged their width doesn't get adjusted.  The next
	   * time we attempt to manage it the geometry manager
	   * goes into a spin.  Thus this butt-ugly hack.
	   */
	  {
	    Dimension sb_width, new_width;

	    XtVaGetValues (cur_instance->scrollbar_widget,
			   XtNwidth, &sb_width,
			   0);

	    if (SCROLLBAR_WIDTH(s) < 1)
	      new_width = 1;
	    else if (SCROLLBAR_WIDTH(s) >= PIXW (s))
	      new_width = PIXW (s) - 1;
	    else
	      new_width = SCROLLBAR_WIDTH(s);

	    if (sb_width != new_width)
	      XtVaSetValues (cur_instance->scrollbar_widget,
			     XtNwidth, new_width, 0);
	  }

	  if (active && !managed)
	    XtManageChild (cur_instance->scrollbar_widget);
	  else if (!active && managed)
	    {
	      XtUnmanageChild (cur_instance->scrollbar_widget);
	      XtUnmapWidget (cur_instance->scrollbar_widget);
	    }

	  if (active && !cur_instance->backing_store_initialized) 
	    {
	      update_one_scrollbar_bs (s, cur_instance->scrollbar_widget);
	      cur_instance->backing_store_initialized = 1;
	    }
	  cur_instance = cur_instance->next;
	  cur_wv = cur_wv->next;
	}

      free_scrollbar_widget_value_tree (wv);
    }
  UNBLOCK_INPUT;

  update_one_screen_scrollbars_size (s);
}

static void
update_one_screen_scrollbars_size (struct screen *s)
{
  /* Update the visibility and size of the scrollbar container. */

  Widget scrollbar_manager = s->display.x->scrollbar_manager;
  int changed = 0;
  int old_screen_width = SCREEN_WIDTH (s);

  BLOCK_INPUT;

  if (SCROLLBAR_WIDTH(s) <= 0 && scrollbar_manager &&
      XtIsManaged (scrollbar_manager))
    {
      /* Scrollbar was on, and is going off. */
      XtUnmanageChild (scrollbar_manager);

      changed = 1;
    }

   else if (SCROLLBAR_WIDTH(s) > 0 && !XtIsManaged (scrollbar_manager))
     {
       /* Scrollbar was off, and is going on */
       XtManageChild (scrollbar_manager);

      changed = 1;
     }

  if (scrollbar_manager && XtIsManaged (scrollbar_manager))
    /* Check for width change. */
    {
      Dimension sb_height, sb_width, new_width;

      XtVaGetValues (scrollbar_manager,
		     XtNheight, &sb_height,
		     XtNwidth, &sb_width,
		     0);

      if (SCROLLBAR_WIDTH(s) < 1)
	new_width = 1;
      else if (SCROLLBAR_WIDTH(s) >= PIXW (s))
	new_width = PIXW (s) - 1;
      else
	new_width = SCROLLBAR_WIDTH(s);

      if (sb_width != new_width)
	{
	  XtVaSetValues (scrollbar_manager, XtNwidth, new_width, 0);
	  changed = 1;
	}

      /* Update scrollbar pointer shape. */
      {
	Lisp_Object screen;
	XSETR (screen, Lisp_Screen, s);
	Fx_set_scrollbar_pointer (screen, Vx_scrollbar_pointer_shape);
      }
    }
  UNBLOCK_INPUT;

  if (changed)
    {
      /* Ensure that the character width of the screen didn't change. */
      Lisp_Object screen;
      XSETR (screen, Lisp_Screen, s);
      Fset_screen_width (screen, make_number (old_screen_width), Qnil);
    }
}

static void
update_one_scrollbar_bs (struct screen *s, Widget sb_widget)
{
  EmacsScreen ew = (EmacsScreen) s->display.x->edit_widget;

  if (ew->emacs_screen.use_backing_store && sb_widget)
    {
      unsigned long mask = CWBackingStore;
      XSetWindowAttributes attrs;
	      
      attrs.backing_store = Always;
      XChangeWindowAttributes (XtDisplay(sb_widget),
			       XtWindow(sb_widget),
			       mask,
			       &attrs);
    }
}

void
update_scrollbars ()
{
  struct screen *s;
  Lisp_Object tail;

  /* Consider this code to be "in_display" so that we abort() if Fsignal()
     gets called. */
  in_display++;

  for (tail = Vscreen_list; CONSP (tail); tail = XCONS (tail)->cdr)
    {
      Lisp_Object screen = XCONS (tail)->car;
      if (!SCREENP (screen))
	continue;
      s = XSCREEN (screen);
      if (!SCREEN_IS_X (s))
	continue;
      update_one_screen_scrollbars (s);
    }

  in_display--;
  if (in_display < 0) abort ();
}

extern void construct_name_list (Widget widget, char *name, char *class);

/* Called from x_create_widgets() to create the inital scrollbars of a screen
   before it is mapped, so that the window is mapped with the scrollbars
   already there instead of us tacking them on later and thrashing the window
   after it is visible. */
int
initialize_screen_scrollbars (struct screen *s)
{
  /* If the width is -1 then the user didn't specify a value.  So, we
     first check the toolkit resource to see if they are set and then
     fallback to our own setting if they are not. */
  if (SCROLLBAR_WIDTH(s) < 0)
    {
      Display *dpy = XtDisplay (s->display.x->container);
      char name_str[1024], class_str[1024];
      char *type;
      XrmValue value;

      /* The name strings are wrong, but the scrollbar name is
	 non-deterministic so it is a poor way to set a resource for
	 the scrollbar anyhow. */
#ifdef LWLIB_USES_MOTIF
      construct_name_list (s->display.x->container, name_str, class_str);
      strcat (name_str, "sb_manager.scrollbar.width");
      strcat (class_str, "XmPanedWindow.XmScrollBar.width");
#else
      construct_name_list (s->display.x->container, name_str, class_str);
      strcat (name_str, "sb_manager.scrollbar.thickness");
      strcat (class_str, "Paned.Scrollbar.thickness");
#endif

      if (XrmGetResource (XtDatabase (dpy), name_str, class_str, &type, &value)
	  == True)
	{
	  if (!strcmp (type, XtRString))
	    SCROLLBAR_WIDTH(s) = atoi (value.addr);
	  else
	    SCROLLBAR_WIDTH(s) = DEFAULT_SCROLLBAR_WIDTH;
	}
#ifndef LWLIB_USES_MOTIF
	  /* The official Athena resource for specifying the scrollbar
             width is 'thickness'.  But 'width' is also accepted at
             least by xterm. */
      else
	{
	  construct_name_list (s->display.x->container, name_str, class_str);
	  strcat (name_str, "sb_manager.scrollbar.width");
	  strcat (class_str, "Paned.Scrollbar.width");
	  if (XrmGetResource (XtDatabase (dpy), name_str, class_str,
			      &type, &value) == True)
	    {
	      if (!strcmp (type, XtRString))
		SCROLLBAR_WIDTH(s) = atoi (value.addr);
	      else
		SCROLLBAR_WIDTH(s) = DEFAULT_SCROLLBAR_WIDTH;
	    }
	  else
	    SCROLLBAR_WIDTH(s) = DEFAULT_SCROLLBAR_WIDTH;
	}
#else
      else
	SCROLLBAR_WIDTH(s) = DEFAULT_SCROLLBAR_WIDTH;
#endif
    }

    {
      Arg av [20];
      int ac = 0;
      int width = SCROLLBAR_WIDTH(s);

      /* If scrollbar width is 0, that means the scrollbar is not
	 managed.  Go ahead and create it anyway, with a non-zero
	 width to avoid problems.  It will be resized as ncessary. */
      if (width <= 0)
	width = DEFAULT_SCROLLBAR_WIDTH;

#ifdef LWLIB_USES_MOTIF

      XtSetArg (av[ac], XmNmarginHeight, 0); ac++;
      XtSetArg (av[ac], XmNmarginWidth, 0); ac++;
      XtSetArg (av[ac], XmNsashHeight, 0); ac++;
      XtSetArg (av[ac], XmNsashIndent, 0); ac++;
      XtSetArg (av[ac], XmNsashShadowThickness, 0); ac++;
      XtSetArg (av[ac], XmNsashWidth, 0); ac++;
      XtSetArg (av[ac], XmNseparatorOn, False); ac++;
      XtSetArg (av[ac], XmNspacing, 0); ac++;
      XtSetArg (av[ac], XmNshadowThickness, 2); ac++;
      XtSetArg (av[ac], XmNwidth, width); ac++;
      XtSetArg (av[ac], XmNtraversalOn, False); ac++;
#if __alpha	/* Don't ask, I don't get it either. */
      XtSetArg (av[ac], XmNtopShadowColor, 0); ac++;
#endif
      /* Note:  If you change what widgets are created here you need
	 to reflect that change above where the width resource is
	 checked. */
      s->display.x->scrollbar_manager =
	XmCreatePanedWindow (s->display.x->container, "sb_manager", av, ac);

#else /* !LWLIB_USES_MOTIF (Athena) */

      XtSetArg (av[ac], XtNorientation, XtorientVertical); ac++;
      /* #### This doesn't seem to work all the time; the divider appears
	 but sometimes disappears when you click on the scrollbar.
	 Bug in Athena?  I can't figure it out. */
      XtSetArg (av[ac], XtNinternalBorderWidth, 2); ac++;
      XtSetArg (av[ac], XtNwidth, width); ac++;
      XtSetArg (av[ac], XtNheight, 10); ac++; /* reset later... */
      /* Note:  If you change what widgets are created here you need
	 to reflect that change above where the width resource is
	 checked. */
      s->display.x->scrollbar_manager =
	XtCreateWidget ("sb_manager", panedWidgetClass,
			s->display.x->container, av, ac);

#endif /* !LWLIB_USES_MOTIF (Athena) */

      if (!s->scrollbar_instances)
	s->scrollbar_instances = create_scrollbar_instance (s);
    }

  return SCROLLBAR_WIDTH(s) > 0;
}

/* Destroy all scrollbars associated with SCREEN.  Only called from
   Fdelete_screen().
 */
void
free_screen_scrollbars (struct screen *s)
{
  struct scrollbar_instance *instance;

  if (!SCREEN_IS_X (s))
    return;

  if (s->scrollbar_instances)
    instance = s->scrollbar_instances;
  else
    return;

  while (instance)
    {
      if (instance->scrollbar_name)
	xfree (instance->scrollbar_name);

      if (instance->scrollbar_widget)
	{
	  BLOCK_INPUT;
	  lw_destroy_all_widgets (instance->scrollbar_id);
	  UNBLOCK_INPUT;
	}
      instance = instance->next;
    }
}

void
x_set_scrollbar_width (struct screen *s, int width)
{
  Widget w = s->display.x->edit_widget;

  if (width < 0)
    width = 0;

  BLOCK_INPUT;
  XtVaSetValues (w, XtVaTypedArg, "scrollBarWidth", XtRInt, width,
		 sizeof (int), 0);
  UNBLOCK_INPUT;

  update_one_screen_scrollbars (s);
}

int
x_scrollbar_width (struct screen *s)
{
  EmacsScreen w = (EmacsScreen) s->display.x->edit_widget;

  return w->emacs_screen.scrollbar_width;
}

