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
#include "lwlib.h"

#include "xintrinsicp.h"     /* for XtResizeWidget */
#include <X11/StringDefs.h>

#ifdef LWLIB_USES_MOTIF
# include <Xm/Xm.h>
# include <Xm/MainW.h>
# include <Xm/PanedW.h>
#else /* Athena */
# include <X11/Xmu/Converters.h>	/* For XtorientVertical */
# include <X11/Xaw/Paned.h>
#endif

#define min(a,b) ((a)<(b) ? (a) : (b))
#define max(a,b) ((a)>(b) ? (a) : (b))

int scrollbar_width;

/* We need a unique id for every scrollbar.  In order to avoid
   tripping over the menus and dialog boxes, we just use their
   counter.  I think this system sucks rocks, but that's life.  There
   ought to be a common counter for all widgets using lwlib. */
extern unsigned int popup_id_tick;


void
mark_scrollbar (struct scrollbar_instance *instance,
		void (*markobj) (Lisp_Object))
{
  if (instance)
    {
      ((markobj) (instance->window));
      mark_scrollbar (instance->next, markobj);
    }
}


/*
 * This is essentially Fforward_line, duplicated here because
 * Fforward_line only works with the current buffer in the current
 * window.
 *
 * The start arg was added to let this also act as a beginning_of_line
 * function.  This function gets more and more bastardized every week.
 */
static Lisp_Object
scrollbar_forward_line (struct screen *s, Lisp_Object win, int n,
			Lisp_Object start)
{
  Lisp_Object buf;
  struct buffer *b, *cur_buf;
  int pos, pos2;
  int count, shortage, negp;

  buf = get_buffer (XWINDOW (win)->buffer, 0);
  if (!NILP (buf))
    b = XBUFFER (buf);
  else
    return Qnil;

  if (!NILP (start))
    pos2 = XINT (start);
  else if (!EQ (win, SCREEN_SELECTED_WINDOW (s)))
    {
      int pointm = marker_position (XWINDOW (win)->start);

      if (pointm < BUF_BEGV (b))
	pos2 = BUF_BEGV (b);
      else if (pointm > BUF_ZV (b))
	pos2 = BUF_ZV (b);
      else
	pos2 = pointm;
    }
  else
    pos2 = marker_position (XWINDOW (win)->start);

  count = n;
  negp = count <= 0;

  cur_buf = current_buffer;
  set_buffer_internal (b);
  pos = scan_buffer (current_buffer, '\n', pos2, count - negp, &shortage);
  set_buffer_internal (cur_buf);

  if (shortage > 0
      && (negp
	  || (BUF_ZV (b) > BUF_BEGV (b)
	      && pos != pos2
	      && BUF_CHAR_AT (b, pos - 1) != '\n')))
    shortage--;

  if (n == 0)
    return make_number (pos);
  else
    return (Fset_window_point (win, make_number (pos)));
}

/*
 * This is a wrapper for the scroll-up and scroll-down functions to
 * set the current window to whichever window is actually being
 * scrolled.
 */
static void
scrollbar_page (struct screen *s, Lisp_Object win, int count)
{
  Lisp_Object cur_select = SCREEN_SELECTED_WINDOW (s);
  struct screen *cur_screen = selected_screen;

  if (!EQ (cur_select, win) || (s != cur_screen))
    Fselect_window (win);

  if (count == 1)
    scroll_command (Qnil, 1, 1);	/* no error */
  else
    scroll_command (Qnil, -1, 1);	/* no error */

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

  lock_redisplay ();

  if (!EQ (cur_select, win) || (s != cur_screen))
    {
      Fselect_window (win);
      redisplay ();
      Fselect_window (cur_select);
    }
  else
    redisplay ();

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

  lock_redisplay ();

  if (!EQ (cur_select, win) || (s != cur_screen))
    {
      Fselect_window (win);
      Frecenter (Qzero);
      redisplay ();
      Fselect_window (cur_select);
    }
  else
    Frecenter (Qzero);

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
      Fforward_line (Qnil);
    }
  else
    Fgoto_char (make_number (orig_pt));

  if (!EQ (cur_select, win) || (s != cur_screen))
    Fselect_window (cur_select);
}

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
	  win = instance->window;
	  break;
	}
      instance = instance->next;
    }

  if (NILP (win))
    return;

  buf = get_buffer (XWINDOW (win)->buffer, 0);
  if (NILP (buf))
    orig_pt = 1;
  else
    orig_pt = BUF_PT (XBUFFER (buf));
  
  switch (data->action)
    {
    case SCROLLBAR_LINE_UP:
      scrollbar_forward_line (s, win, -1, Qnil);
      recenter_scrollbar_window (s, win);
      break;
    case SCROLLBAR_LINE_DOWN:
      scrollbar_forward_line (s, win, 1, Qnil);
      recenter_scrollbar_window (s, win);
      break;
    case SCROLLBAR_PAGE_UP:
      scrollbar_page (s, win, -1);
      break;
    case SCROLLBAR_PAGE_DOWN:
      scrollbar_page (s, win, 1);
      break;
    case SCROLLBAR_TOP:
      Fset_window_point (win, Fpoint_min());
      recenter_scrollbar_window (s, win);
      break;
    case SCROLLBAR_BOTTOM:
      Fset_window_point (win, Fpoint_max());
      recenter_scrollbar_window (s, win);
      break;
    case SCROLLBAR_CHANGE:
      if ((data->slider_value + 1) <= instance->data.maximum)
	data->slider_value++;
      Fset_window_point (win, make_number (data->slider_value));
      recenter_scrollbar_window (s, win);
      break;
    case SCROLLBAR_DRAG:
      {
	Lisp_Object start_pos;

	start_pos = scrollbar_forward_line (s, win, 0,
					    make_number (data->slider_value));
	if (XINT (start_pos) == instance->data.slider_position &&
	    data->slider_value >= instance->data.slider_position &&
	    XINT (start_pos) < instance->data.maximum)
	  {
	    scrollbar_forward_line (s, win, 1, Qnil);
	    recenter_scrollbar_window (s, win);
	  }
	else
	  Fset_window_start (win, start_pos, Qnil);
      }
      break;
    default:
      break;
    }

  /*
   * The paging operations are identical to calling Fscroll_up and
   * Fscroll_down.  Doing additional work just screws things up.
   */
  if (data->action != SCROLLBAR_PAGE_UP &&
      data->action != SCROLLBAR_PAGE_DOWN)
    {
      redisplay_scrollbar_window (s, win);
      scrollbar_reset_cursor (s, win, orig_pt);
    }
}

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
  instance->window = Qnil;
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

  instance->scrollbar_id = ++popup_id_tick;
  sprintf (buffer, "scrollbar_%d", instance->scrollbar_id);
  instance->scrollbar_name = xstrdup (buffer);

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
  int start_pos, end_pos, new_height;

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
  buf = XBUFFER (w->buffer);
  start_pos = marker_position (w->start);
  end_pos = BUF_Z (buf) - w->window_end_pos;	/* This is a close approx. */

  /*
   * The end position must be strictly greater than the start position,
   * at least for the Motify scrollbar.  It shouldn't hurt anything for
   * other scrollbar implmentations.
   */
  if (end_pos <= start_pos) end_pos = start_pos + 1;

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
  if (XWINDOW (instance->window) != w)
    {
      XSET (instance->window, Lisp_Record, w);
      instance->scrollbar_instance_changed = 1;
    }
      
  /*
   * Only character-based scrollbars are implemented at the moment.
   * Line-based will be implemented in the future.
   */
  UPDATE_DATA_FIELD (scrollbar_is_active, 1);
  UPDATE_DATA_FIELD (data.line_increment, 1);
  UPDATE_DATA_FIELD (data.page_increment, 1);
  UPDATE_DATA_FIELD (data.scrollbar_height, new_height);
  UPDATE_DATA_FIELD (data.scrollbar_pos, next_pos);

  /*
   * A disabled scrollbar has its slider sized to the entire height of
   * the scrollbar.  Currently the minibuffer scrollbar and all scrollbars
   * next to horizontally-split windows are disabled.
   */
  if (NILP (w->hchild) && !MINI_WINDOW_P (w))
    {
      UPDATE_DATA_FIELD (data.minimum, BUF_BEGV (buf));
      UPDATE_DATA_FIELD (data.maximum,
			 max (BUF_ZV (buf),
			      instance->data.minimum + 1));
      UPDATE_DATA_FIELD (data.slider_size,
			 min ((end_pos - start_pos),
			      (instance->data.maximum -
			       instance->data.minimum)));
      UPDATE_DATA_FIELD (data.slider_position, start_pos);
    }
  else
    {
      UPDATE_DATA_FIELD (data.minimum, 1);
      UPDATE_DATA_FIELD (data.maximum, 2);
      UPDATE_DATA_FIELD (data.slider_size, 1);
      UPDATE_DATA_FIELD (data.slider_position, 1);
      /* Actually, let's make these unusable scrollbars invisible. */
      instance->scrollbar_is_active = 0;
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

#ifdef LWLIB_USES_MOTIF
# define sb_margin 0
#else
  /* We need to put a border next to the scrollbars for Athena, because
     they don't border themselves... */
# define sb_margin 1
#endif

/* For every screen, make sure all horizontal window areas have a
   scrollbar and that the scrollbars are up-to-date with the window view.
 */
static void
update_one_screen_scrollbars (struct screen *s)
{
  struct scrollbar_instance *instance, *cur_instance;
  Widget scrollbar_manager = s->display.x->scrollbar_manager;

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
      instance = instance->next;
    }

  /* If there are going to be any scrollbars, make sure there's a 
     container before we try to create children of it. */
  if (scrollbar_width > 0 && scrollbar_manager == 0)
    {
      initialize_screen_scrollbars (s);
      scrollbar_manager = s->display.x->scrollbar_manager;
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

	  lw_modify_all_widgets (cur_instance->scrollbar_id, cur_wv, 0);

	  active = cur_instance->scrollbar_is_active;
	  managed = XtIsManaged (cur_instance->scrollbar_widget);

	  if (active && !managed)
	    XtManageChild (cur_instance->scrollbar_widget);
	  else if (!active && managed)
	    {
	      XtUnmanageChild (cur_instance->scrollbar_widget);
	      XtUnmapWidget (cur_instance->scrollbar_widget);
	    }

	  cur_instance = cur_instance->next;
	  cur_wv = cur_wv->next;
	}

      free_scrollbar_widget_value_tree (wv);
    }

  /* Now update the visibility and size of the scrollbar container.
   */

  if (scrollbar_width <= 0 && scrollbar_manager &&
      XtIsManaged (scrollbar_manager))
    {
      /* Scrollbar was on, and is going off.
	 For Motif, we just need to unmanage the container.
	 For Athena, we also need to reduce the window size by the
	 size of the scrollbar margin, sigh...
       */
#ifndef LWLIB_USES_MOTIF	/* Athena */
      Dimension width;
      XtVaGetValues (s->display.x->widget, XtNwidth, &width, 0);
#endif /* Athena */      
      XtUnmanageChild (scrollbar_manager);
#ifndef LWLIB_USES_MOTIF	/* Athena */
      XtVaSetValues (s->display.x->widget,
		     XtNwidth, (width - (scrollbar_manager->core.width +
					 sb_margin)),
		     0);
#endif /* Athena */
    }

   else if (scrollbar_width > 0 && !XtIsManaged (scrollbar_manager))
     {
       /* Scrollbar was off, and is going on */
#ifndef LWLIB_USES_MOTIF	/* Athena */
      Dimension width;
      XtVaGetValues (s->display.x->widget, XtNwidth, &width, 0);
      XtUnmanageChild (s->display.x->edit_widget);
      XawPanedSetRefigureMode (s->display.x->container2, False);
#endif /* Athena */      
       XtManageChild (scrollbar_manager);
#ifndef LWLIB_USES_MOTIF	/* Athena */
      XtManageChild (s->display.x->edit_widget);
      XtVaSetValues (s->display.x->widget,
		     XtNwidth, (width + scrollbar_manager->core.width +
				sb_margin),
		     0);
      XawPanedSetRefigureMode (s->display.x->container2, True);
#endif /* Athena */
     }

  if (scrollbar_manager && XtIsManaged (scrollbar_manager))
    /* Check for width change. */
    {
      Dimension sb_height, sb_width, new_width;

      XtVaGetValues (scrollbar_manager,
		     XtNheight, &sb_height,
		     XtNwidth, &sb_width,
		     0);

      if (scrollbar_width - sb_margin < 1)
	new_width = 1;
      else if (scrollbar_width - sb_margin >= PIXW (s))
	new_width = PIXW (s) - 1;
      else
	new_width = scrollbar_width - sb_margin;

      if (sb_width != new_width)
	{
	  Widget container = s->display.x->container;
	  Dimension parent_width = (container->core.width +
				    (new_width - sb_width));

#ifdef LWLIB_USES_MOTIF
	  XtResizeWidget (scrollbar_manager, new_width, sb_height, 0);
	  XtVaSetValues (container, XtNwidth, parent_width, 0);
#else /* Athena */
	  Widget container2 = XtParent (scrollbar_manager);
	  Widget text_area = s->display.x->edit_widget;
	  Dimension text_width = text_area->core.width;
	  Dimension text_height = text_area->core.height;

	  XawPanedSetRefigureMode (container2, False);
	  XawPanedSetRefigureMode (container, False);

	  XtVaSetValues (container,  XtNwidth, parent_width, 0);
	  XtVaSetValues (container2, XtNwidth, parent_width, 0);
	  XtUnmanageChild (text_area);
	  XtUnmanageChild (scrollbar_manager);
	  XtResizeWidget (scrollbar_manager, new_width, sb_height, 0);
	  XtResizeWidget (text_area, text_width, text_height, 0);
	  XtManageChild (scrollbar_manager);
	  XtManageChild (s->display.x->edit_widget);

	  XawPanedSetRefigureMode (container, True);
	  XawPanedSetRefigureMode (container2, True);
/* ####	  if (container2->core.width != parent_width) abort (); */
#endif /* Athena */

	  /* We have just attempted to resize the scrollbar_manager and
	     the scrollbar.  They'd better have the sizes we gave them.
	   */
/* ####	  if (container->core.width != parent_width ||
	      scrollbar_manager->core.width != new_width ||
	      scrollbar_manager->core.height != sb_height)
	    abort (); */
	}

      /* Update scrollbar pointer shape. */
      {
	Lisp_Object screen;
	XSETR (screen, Lisp_Screen, s);
	Fx_set_scrollbar_pointer (screen, Vx_scrollbar_pointer_shape);
      }
    }

  UNBLOCK_INPUT;
}


void
update_scrollbars ()
{
  struct screen *s;
  Lisp_Object tail;

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
}

/* Called from x_create_widgets() to create the inital scrollbars of a screen
   before it is mapped, so that the window is mapped with the scrollbars
   already there instead of us tacking them on later and thrashing the window
   after it is visible. */
void
initialize_screen_scrollbars (struct screen *s)
{
  if (!s->display.x->scrollbar_manager && scrollbar_width > 0)
    {
      Arg av [20];
      int ac = 0;

#ifdef LWLIB_USES_MOTIF

      Widget parent = s->display.x->container;
      XtSetArg (av[ac], XmNmarginHeight, 0); ac++;
      XtSetArg (av[ac], XmNmarginWidth, 0); ac++;
      XtSetArg (av[ac], XmNsashHeight, 0); ac++;
      XtSetArg (av[ac], XmNsashIndent, 0); ac++;
      XtSetArg (av[ac], XmNsashShadowThickness, 0); ac++;
      XtSetArg (av[ac], XmNsashWidth, 0); ac++;
      XtSetArg (av[ac], XmNseparatorOn, False); ac++;
      XtSetArg (av[ac], XmNspacing, 0); ac++;
      XtSetArg (av[ac], XmNshadowThickness, 2); ac++;
      XtSetArg (av[ac], XmNwidth, scrollbar_width); ac++;
      XtSetArg (av[ac], XmNtraversalOn, False); ac++;
#if __alpha	/* Don't ask, I don't get it either. */
      XtSetArg (av[ac], XmNtopShadowColor, 0); ac++;
#endif
      s->display.x->scrollbar_manager =
	XmCreatePanedWindow (parent, "sb_manager", av, ac);

      if (XtIsManaged (parent))
	/* meaning we are adding scrollbars after the screen has been mapped,
	   instead of adding it during the screen's initialization */
	{
	  /* I'm not certain that the MainWindow won't sometimes borrow the
	     space for the scrollbar from the text area, so don't let it.
	   */
	  Dimension width, height;
	  XtVaGetValues (XtParent (parent),
			 XtNwidth, &width, XtNheight, &height, 0);
	  XmMainWindowSetAreas (parent,
				s->display.x->menubar_widget, 0, 0,
				s->display.x->scrollbar_manager,
				s->display.x->edit_widget);
	  XtVaSetValues (XtParent (parent),
			 XtNwidth, width, XtNheight, height, 0);
	}

#else /* !LWLIB_USES_MOTIF (Athena) */

      XtSetArg (av[ac], XtNshowGrip, False); ac++;
      XtSetArg (av[ac], XtNallowResize, True); ac++;
      XtSetArg (av[ac], XtNresizeToPreferred, True); ac++;
      XtSetArg (av[ac], XtNorientation, XtorientVertical); ac++;
      /* #### This doesn't seem to work; possibly because the scrollbars
	 themselves have 0 borderwidth?  I can't figure it out. */
      XtSetArg (av[ac], XtNinternalBorderWidth, 1); ac++;
      XtSetArg (av[ac], XtNwidth, scrollbar_width - sb_margin); ac++;
      XtSetArg (av[ac], XtNheight, 10); ac++; /* reset later... */
      XtSetArg (av[ac], XtNskipAdjust, True); ac++;
      s->display.x->scrollbar_manager =
	XtCreateWidget ("sb_manager", panedWidgetClass,
			s->display.x->container2, av, ac);

#endif /* !LWLIB_USES_MOTIF (Athena) */

      if (!s->scrollbar_instances)
	s->scrollbar_instances = create_scrollbar_instance (s);
    }
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
syms_of_scrollbar ()
{
  DEFVAR_INT ("scrollbar-width", &scrollbar_width,
    "The width (in pixels) of the scrollbars.");
  scrollbar_width = 15;
}
