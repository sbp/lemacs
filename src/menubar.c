/* Implements an elisp-programmable menubar.

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

/* created 16-dec-91 by jwz

   form		:=  <something to pass to `eval'>
   command	:=  <a symbol or string, to pass to `call-interactively'>
   callback 	:=  command | form
   active-p	:=  <t or nil, whether this thing is selectable>
   text		:=  <a string, non selectable>
   menu-item	:=  '['  string callback active-p  ']'
   menu		:=  '(' string [ menu-item | menu | text ]+ ')'
   partition	:=  'nil'
   menubar	:=  '(' [ menu-item | menu | text | partition ]* ')'
   
   Or, in english...  A menu-item is a vector of three elements: a string,
   a callback, and whether the item is selectable.  A menu is a list of
   menu-items, strings, and submenus.  The strings are displayed in the
   menu but are not selectable.

   A menubar is just like a menu without the "name" at the front.  You
   can put buttons and static text in the menubar.

   #### This next bit isn't true because Motif is so broken.
   If there is a `nil' in the menubar, that is where the extra whitespace
   goes.  When the menubar is wider than the total width of the menu names,
   the extra space normally appears on the right, with the menu names packed
   in on the left.  If there is a `nil' in the menubar, then the menus to
   the left of it will be flushleft, and the menus to the right of it will
   be flushright.

   Menus and menu items may be safely modified; comparisons are (effectively)
   done with `equal.'  However, the callbacks are not copied, so don't change
   the contents of those (that is, if the callback of an item is a list,
   don't change the contents of that list.)

   Changes to menus and menu items have no effect until `set-screen-menubar'
   is called again.  

   After the menubar is clicked upon, but before any menus are popped up,
   the functions on the activate-menubar-hook are invoked to make changes to
   the menus and menubar.  This is intended to implement lazy alteration of
   the sensitivity of menu items.
 */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Paned.h>

#include "config.h"
#include "lisp.h"
#include "screen.h"
#include "xterm.h"
#include "events.h"
#include "window.h"
#include "buffer.h"

#include "lwlib.h"

static int menubar_disabled;

/* 1 if a menu is currently up, 0 otherwise. */
static int popup_menu_up_p;

int menubar_show_keybindings;

static widget_value *
menu_item_descriptor_to_widget_value (desc)
     Lisp_Object desc;
{
  int i;
  widget_value *wv =
    (widget_value *) calloc (sizeof (widget_value), 1);
  switch (XTYPE (desc))
    {
    case Lisp_String:
      wv->name = (char *) XSTRING (desc)->data;
      wv->enabled = 1;
      break;
    case Lisp_Vector:
      if (XVECTOR (desc)->size != 3)
	Fsignal (Qerror,
		 Fcons (build_string ("button descriptors must be 3 long"),
			Fcons (desc, Qnil)));
      CHECK_STRING (XVECTOR (desc)->contents [0], 0);
      wv->name = (char *) XSTRING (XVECTOR (desc)->contents [0])->data;
      wv->enabled = (Qnil != XVECTOR (desc)->contents [2]);
      wv->contents =
	(widget_value*)(wv->enabled && (Qt != XVECTOR (desc)->contents [2]));
      wv->call_data = (XtPointer) (XVECTOR (desc)->contents [1]);
      if (menubar_show_keybindings &&
	  XTYPE ((Lisp_Object) wv->call_data) == Lisp_Symbol)
	{
	  char buf [1024];
	  where_is_to_char ((Lisp_Object) wv->call_data,
			    Fcurrent_local_map (), Qnil, buf);
	  if (buf [0])
	    wv->key = (char *) strdup (buf);
	  else
	    wv->key = 0;
	}
      break;
    case Lisp_Cons:
      if (XTYPE (XCONS (desc)->car) == Lisp_String)
	{
	  wv->name = (char *) XSTRING (XCONS (desc)->car)->data;
	  desc = Fcdr (desc);
	}
      else
	wv->name = "menubar";

      wv->enabled = 1;
      {
	widget_value *prev = 0, *next;
	for (; !NILP (desc); desc = Fcdr (desc))
	  {
	    next = menu_item_descriptor_to_widget_value (Fcar (desc));
	    if (! next)
	      continue;
	    else if (prev)
	      prev->next = next;
	    else
	      wv->contents = next;
	    prev = next;
	  }
	if (! wv->contents)
	  {
	    for (i = 0; i < (sizeof (widget_value) / sizeof (int)); i++)
	      ((int *) wv) [i] = 0xDEADBEEF;
	    free (wv);
	    wv = 0;
	  }
      }
      break;
    default:
      for (i = 0; i < (sizeof (widget_value) / sizeof (int)); i++)
	((int *) wv) [i] = 0xDEADBEEF;
      free (wv);
      wv = 0;
      if (!NILP (desc)) /* ignore nil for now */
	Fsignal (Qerror, Fcons (build_string ("unrecognised descriptor"),
				Fcons (desc, Qnil)));
    }
  return wv;
}


static void
free_widget_value (wv)
     widget_value *wv;
{
  widget_value *a = wv->contents;
  widget_value *b = wv->next;
  if (wv->key) free (wv->key);
#if 0
  int i;
  if (((int *) wv) [0] == 0xDEADBEEF) abort ();
  if ((int)a == 0xDEADBEEF) abort ();
  if ((int)b == 0xDEADBEEF) abort ();
  for (i = 0; i < (sizeof (widget_value) / sizeof (int)); i++)
    ((int *) wv) [i] = 0xDEADBEEF;
#endif
  free (wv);
  if (a == (widget_value*)1) a = 0;
  if (a) free_widget_value (a);
  if (b) free_widget_value (b);
}


/* screen->menubar_data holds a Lisp_Vector.
   In this code, we treat that vector as a menubar_data structure.
 */
struct menubar_data
{
  int size;
  struct Lisp_Vector *next;

  /* This is the menubar that was most recently passed to `set-screen-menubar'.
     The user might stomp this list, so it doesn't necessarily have any
     relation to what's on the screen (lwlib.a has private data structures
     representing that to optimize updates.)  This value is returned by the
     `current-menubar' function, and is used for absolutely nothing else.
   */
  Lisp_Object menubar;

  /* This is a list of all of the callbacks of the menu buttons.  This is
     used only to protect them from being GCed, since the only other pointer
     to these lisp objects might be down in the private lwlib.c structures,
     which GC doesn't know about.
   */
  Lisp_Object all_callbacks;

  /* #### This is currently unused...
     This is an alist of (SYMBOL . MENU-DESC), where SYMBOL is one of the
     symbols passed to `define-popup-menu'.  The MENU-DESCs are a copy of the
     last-displayed version of this popup, and are needed for the same reason
     as the above.
   */
  Lisp_Object popup_table;
};


#define SCREEN_MENUBAR_DATA(screen) \
  ((struct menubar_data *) XVECTOR ((screen)->menubar_data))

#define MENUBAR_DATA_SIZE \
  ((sizeof (struct menubar_data) / sizeof (Lisp_Object)) - 2)


DEFUN ("screen-menubar", Fscreen_menubar, Sscreen_menubar, 0, 1, 0,
      "Returns the menubar description list of the given screen,\n\
defaulting to the current screen.")
     (screen)
     Lisp_Object screen;
{
  struct screen* s;
  if (NILP (screen))
    s = selected_screen;
  else {
    CHECK_SCREEN (screen, 0);
    s = XSCREEN (screen);
  }
  if (NILP (s->menubar_data))
    return Qnil;
  else
    return SCREEN_MENUBAR_DATA (s)->menubar;
}


static Lisp_Object
screen_displayed_popup_menu (screen, menu_name)
     struct screen *screen;
     Lisp_Object menu_name;
{
  if (NILP (screen->menubar_data))
    return Qnil;
  else
    return Fcdr (Fassoc (menu_name,
			 SCREEN_MENUBAR_DATA (screen)->popup_table));
}

static void
clear_displayed_popup_menu (screen, menu_name)
     struct screen *screen;
     Lisp_Object menu_name;
{
  Lisp_Object alist, rest, last;
  if (NILP (screen->menubar_data))
    return;
  alist = SCREEN_MENUBAR_DATA (screen)->popup_table;
  for (rest = alist, last = Qnil; !NILP (rest);
       last = rest, rest = Fcdr (rest))
    {
      if (Fcar (rest) == menu_name)	  /* splice it out... */
	{
	  if (NILP (last))
	    {
	      if (alist != rest) abort ();
	      alist = Fcdr (alist);
	    }
	  else
	    XCONS (last)->cdr = Fcdr (rest);
	  if (!NILP (screen_displayed_popup_menu (screen, menu_name))) /* ## */
	    abort ();
	  return;
	}
    }
  if (!NILP (screen_displayed_popup_menu (screen, menu_name))) /* ## */
    abort ();
}


static void
set_displayed_popup_menu (screen, menu_name, value)
     struct screen *screen;
     Lisp_Object menu_name;
     Lisp_Object value;
{
  Lisp_Object alist, acons;
  if (NILP (screen->menubar_data))
    screen->menubar_data = Fmake_vector (MENUBAR_DATA_SIZE, Qnil);

  alist = SCREEN_MENUBAR_DATA (screen)->popup_table;
  acons = Fassoc (menu_name, alist);
  if (! NILP (acons))
    XCONS (acons)->cdr = value;
  else
    {
      SCREEN_MENUBAR_DATA (screen)->popup_table =
	Fcons (Fcons (menu_name, value),
	       SCREEN_MENUBAR_DATA (screen)->popup_table);
      if (value != screen_displayed_popup_menu (screen, menu_name)) /* ## */
	abort ();
    }
}


/* returns a flat list of the leaves (callbacks) of the menu */
static Lisp_Object
gcpro_menu_callbacks (menu)
     Lisp_Object menu;
{
  switch (XTYPE (menu))
    {
    case Lisp_Vector:
      if (XVECTOR (menu)->size > 2)
	return (Fcons (XVECTOR (menu)->contents [1], Qnil));
      else
	return Qnil;
      
    case Lisp_String:
      return menu;

    case Lisp_Cons:
      {
	Lisp_Object head = Qnil;
	Lisp_Object tail = Qnil;
	for (; !NILP (menu); menu = Fcdr (menu))
	  {
	    Lisp_Object next = gcpro_menu_callbacks (XCONS (menu)->car);
	    if (NILP (next))
	      continue;
	    if (XTYPE (next) == Lisp_Cons)
	      {
		if (NILP (head))
		  head = tail = next;
		else
		  {
		    XCONS (tail)->cdr = next;
		    tail = next;
		  }
	      }
	    else
	      {
		if (NILP (head))
		  head = tail = Fcons (next, Qnil);
		else
		  {
		    XCONS (tail)->cdr = Fcons (next, Qnil);
		    tail = XCONS (tail)->cdr;
		  }
	      }
	  }
	return head;
      }
    default:
      return Qnil;
    }
}


extern Lisp_Object Qeval, Vrun_hooks;
Lisp_Object Qactivate_menubar_hook, Vactivate_menubar_hook;

static void
pre_activate_callback (widget, client_data)
     Widget widget;
     XtPointer client_data;
{
  struct gcpro gcpro1, gcpro2, gcpro3;
  Lisp_Object menubar = Fscreen_menubar (Qnil);
  Lisp_Object next, rest;
  int any_changes = 0;
  GCPRO3 (rest, menubar, next);
  for (rest = Vactivate_menubar_hook; !NILP (rest); rest = Fcdr (rest))
    {
      next = call1 (XCONS (rest)->car, menubar);
      if (!EQ (next, Qt))
	{
	  menubar = next;
	  any_changes = 1;
	}
    }
  if (any_changes)
    Fset_screen_menubar (menubar, Qnil);
  UNGCPRO;
}


extern Time mouse_timestamp;
extern Time global_mouse_timestamp;
extern Lisp_Object Faccept_process_output ();

static void
menubar_selection_callback (widget, client_data)
     Widget widget;
     XtPointer client_data;
{
  Lisp_Object event, fn, arg;
  Lisp_Object data = (Lisp_Object) client_data;

  if (((int) client_data) == -1)
    return;

  /* Flush the X and process input */
  Faccept_process_output (Qnil);

  if (XTYPE (data) == Lisp_Symbol)
    {
      fn = Qcall_interactively;
      arg = data;
    }
  else if (XTYPE (data) == Lisp_Cons)
    {
      fn = Qeval;
      arg = data;
    }
  else
    {
      fn = Qeval;
      arg =
	Fcons (intern ("signal"),
	       Fcons (Fcons (intern ("quote"), Fcons (Qerror, Qnil)),
		      Fcons (Fcons (intern ("quote"),
				    Fcons (Fcons (build_string ("illegal menu callback"),
						  Fcons (data, Qnil)),
					   Qnil)),
			     Qnil)));
    }

  event = Fallocate_event ();
  XEVENT (event)->event_type = menu_event;
  XEVENT (event)->event.eval.function = fn;
  XEVENT (event)->event.eval.object = arg;

  /* This is the timestamp used for asserting focus so we need to get an
     up-to-date value event if no events has been dispatched to emacs
   */
  mouse_timestamp = global_mouse_timestamp;

  enqueue_command_event (event);
}

DEFUN ("set-screen-menubar", Fset_screen_menubar, Sset_screen_menubar, 1, 2, 0,
       "Make MENUBAR be the menubar of the given SCREEN.")
     (menubar, screen)
     Lisp_Object menubar, screen;
{
  struct screen* s;
  widget_value *data = menu_item_descriptor_to_widget_value (menubar);

  if (NILP (screen))
    s = selected_screen;
  else
    {
      CHECK_SCREEN (screen, 0);
      s = XSCREEN (screen);
    }
  if (!NILP (menubar))
    CHECK_CONS (menubar, 0);

  if (NILP (s->menubar_data))
    s->menubar_data = Fmake_vector (MENUBAR_DATA_SIZE, Qnil);

  if (SCREEN_IS_X (s))
    {
      Widget menubar_widget = s->display.x->menubar_widget;
      int id = (int) s;

      if (data && !data->next && !data->contents)
	abort ();

      BLOCK_INPUT;

      if (!data)
	{
	  if (menubar_widget)
	    lw_destroy_all_widgets (id);
	  s->display.x->menubar_widget = 0;
	}
      else if (menubar_widget)
	lw_modify_all_widgets (id, data);
      else
	{
	  Widget parent = s->display.x->column_widget;
	  Widget edit = s->display.x->edit_widget;
	  menubar_widget =
	    lw_create_widget ("menubar", "menubar", id, data, parent,
			      0, pre_activate_callback,
			      menubar_selection_callback, 0);
	  s->display.x->menubar_widget = menubar_widget;
	  XtVaSetValues (menubar_widget,
			 XtNshowGrip, 0,
			 XtNresizeToPreferred, 1,
			 XtNallowResize, 1,
			 0);
	  /* We don't actually map the widget here; redisplay() will call
	     update_menubars() to update things with the optimization that
	     nothing happens until there is no longer typeahead.  Psheets
	     are hacked too.
	   */
	}
      UNBLOCK_INPUT;
    }
  if (data) free_widget_value (data);
  SCREEN_MENUBAR_DATA (s)->menubar = menubar;
  SCREEN_MENUBAR_DATA (s)->all_callbacks = gcpro_menu_callbacks (menubar);

  return menubar;
}


static void
popup_selection_callback (widget, client_data)
     Widget widget;
     XtPointer client_data;
{
  menubar_selection_callback (widget, client_data);
}

static void
popup_down_callback (widget, client_data)
     Widget widget;
     XtPointer client_data;
{
  popup_menu_up_p = 0;
}


DEFUN ("popup-menu", Fpopup_menu, Spopup_menu, 1, 1, 0,
       "Pop up the given menu.")
     (menu_desc)
     Lisp_Object menu_desc;
{
  static menu_id;

  struct screen *s = selected_screen;
  char *name = " unnamed popup ";
  widget_value *data;
  Widget parent, menu;
  int id;

  if (!SCREEN_IS_X (s)) error ("not an X screen");
  if (XTYPE (menu_desc) == Lisp_Symbol)
    {
      name = (char *) XSYMBOL (menu_desc)->name->data;
      menu_desc = Fsymbol_value (menu_desc);
    }
  data = menu_item_descriptor_to_widget_value (menu_desc);

  if (! data) error ("no menu");
  
  if (NILP (s->menubar_data))
    s->menubar_data = Fmake_vector (MENUBAR_DATA_SIZE, Qnil);

  parent = s->display.x->widget;

  BLOCK_INPUT;
  /* At this stage it is safe to destroy the previous menu popped-up */
  if (menu_id)
    lw_destroy_all_widgets (menu_id);
  menu_id = (int) name;
  menu = lw_create_widget ("popup", data->name, menu_id, data, parent, 1, 0,
			   popup_selection_callback, popup_down_callback);
  popup_menu_up_p = 1;
  lw_popup_menu (menu);
  UNBLOCK_INPUT;
  return Qnil;
}

DEFUN ("popup-menu-up-p", Fpopup_menu_up_p, Spopup_menu_up_p, 0, 0, 0,
       "Returns T if a popup menu is up, NIL otherwise.\n\
See popup-menu.")
     ()
{
  return popup_menu_up_p ? Qt : Qnil;
}

#if 0

DEFUN ("popup-dialog-box", Fpopup_dialog_box, Spopup_dialog_box, 1, 1, 0,
       "Pop up a dialog box and wait for the user to make a selection.")
     (dbox_desc)
     Lisp_Object dbox_desc;
{
  if (XTYPE (dbox_desc) == Lisp_Symbol)
    check_menu_syntax (Fsymbol_value (dbox_desc), 1);
  else
    check_menu_syntax (dbox_desc, 1);
  x_popup_menu (dbox_desc, selected_screen, 1);
  return Qnil;
}

#endif


#ifdef ENERGIZE
extern int desired_debuggerpanel_exposed_p;
extern int current_debuggerpanel_exposed_p;
extern Widget xc_widget_instance_from_id ();
extern Widget xc_instanciate_dbox ();
extern int debuggerpanel_sheet;
#endif

static void
update_menubar_internal
#ifdef ENERGIZE
  (screen, menubar_changed, debuggerpanel_changed, psheets_changed,
   old_sheets, new_sheets, old_count, new_count)
Lisp_Object screen;
int menubar_changed, psheets_changed, debuggerpanel_changed;
int *old_sheets, *new_sheets;
int old_count, new_count;
#else
  (screen, menubar_changed)
Lisp_Object screen;
int menubar_changed;
#endif
{
  struct screen* s = XSCREEN (screen);
  struct x_display *x = s->display.x;
  int just_created = 0;
  int i;

  BLOCK_INPUT;
  XawPanedSetRefigureMode (x->column_widget, 0);
  
  /* the order in which children are managed is the top to
     bottom order in which they are displayed in the paned window.
     First, remove the text-area widget.
   */
  XtUnmanageChild (x->edit_widget);

#ifdef ENERGIZE
  /* Remove the psheets that are there now
   */
  if (menubar_changed || debuggerpanel_changed || psheets_changed)
    {
      i = old_count;
      while (i)
	{
	  Widget w;
	  int sheet = old_sheets[--i];
	  w = xc_widget_instance_from_id (sheet, 0, x->column_widget);
	  if (psheets_changed && w && XtIsManaged (w))
	    {
	      notify_that_sheet_has_been_hidden (sheet);
	      XtVaSetValues (w, XtNmappedWhenManaged, 0, 0);
	    }
	  xc_hide_dbox_instance (sheet, 0, x->column_widget);
	  if (psheets_changed && w)
	    XtUnmapWidget (w);
	}
    }

  /* remove debugger panel if present */
  if (current_debuggerpanel_exposed_p && debuggerpanel_sheet &&
      (menubar_changed || debuggerpanel_changed))
    {
      Widget w;
      int sheet = debuggerpanel_sheet;
      w = xc_widget_instance_from_id (sheet, 0, x->column_widget);
      if (!desired_debuggerpanel_exposed_p && w && XtIsManaged (w))
	{
	  notify_that_sheet_has_been_hidden (sheet);
	  XtVaSetValues (w, XtNmappedWhenManaged, 0, 0);
	}
      xc_hide_dbox_instance (sheet, 0, x->column_widget);
      if (!desired_debuggerpanel_exposed_p && w)
	XtUnmapWidget (w);
    }
#endif

  /* remove the menubar that is there now, and put up the menubar that
     should be there.
   */
  if (menubar_changed)
    {
      XtManageChild (x->menubar_widget);
      XtMapWidget (x->menubar_widget);
      XtVaSetValues (x->menubar_widget, XtNmappedWhenManaged, 1, 0);
    }

#ifdef ENERGIZE
  /* add debugger panel if desired */
  if (desired_debuggerpanel_exposed_p && debuggerpanel_sheet &&
      (menubar_changed || debuggerpanel_changed))
    {
      Widget w;
      w = xc_instanciate_dbox (debuggerpanel_sheet, 0, x->column_widget, 0);
      if (w)
	fix_pane_constraints (w);
      xc_show_dbox_instance (debuggerpanel_sheet, 0, x->column_widget);
      w = xc_widget_instance_from_id (debuggerpanel_sheet, 0,
				      x->column_widget);
      XtManageChild (w);
      XtMapWidget (w);
      XtVaSetValues (w, XtNmappedWhenManaged, 1, 0);
    }
  
  /* Add the psheets that should be there now
   */
  i = new_count;
  while (i)
    {
      Widget w;
      int sheet = new_sheets[--i];
      w = xc_instanciate_dbox (sheet, 0, x->column_widget, 0);
      if (w)
	fix_pane_constraints (w);
      xc_show_dbox_instance (sheet, 0, x->column_widget);
      w = xc_widget_instance_from_id (sheet, 0, x->column_widget);
      /* Put the mappedWhenManaged property back in or the Motif widgets
	 refuse to take the focus! */
      XtVaSetValues (w, XtNmappedWhenManaged, 1, 0);
      XtManageChild (w);
    }

  /* Give back the focus to emacs if no p_sheets are displayed anymore */
  if (psheets_changed)
    Fselect_screen (screen);
#endif

  /* Re-manage the text-area widget */
  XtManageChild (x->edit_widget);

  /* and now thrash the sizes */
  XawPanedSetRefigureMode (x->column_widget, 1);
  UNBLOCK_INPUT;
}


void
update_menubars ()
{
  struct screen* s;
  Lisp_Object tail;

  if (menubar_disabled)
    return;
  else
    menubar_disabled = 1;

  for (tail = Vscreen_list; CONSP (tail); tail = XCONS (tail)->cdr)
    {
      Lisp_Object screen = XCONS (tail)->car;
      struct window* w;
      struct buffer* buf;
      if (XTYPE (screen) != Lisp_Screen)
	continue;
      s = XSCREEN (screen);
      w = XWINDOW (s->selected_window);
      buf = XBUFFER (w->buffer);
      if (SCREEN_IS_X (s)
	  && !MINI_WINDOW_P (w)
	  && !EQ (screen, Vglobal_minibuffer_screen))
	{
	  struct x_display *x = s->display.x;
	  int buffer_changed = (x->last_selected_buffer != w->buffer);
	  int menubar_changed = (x->menubar_widget
				 && !XtIsManaged (x->menubar_widget));

	  /* This is all the stuff that redisplay uses to know if the 
	     mode_line may have changed! */
	  int title_changed =
	    (buffer_changed
	     || redraw_mode_line
	     || clip_changed
	     || windows_or_buffers_changed
	     || (XFASTINT (w->last_modified) < BUF_MODIFF (buf)
		 && XFASTINT (w->last_modified) <= buf->save_modified)
	     || !NILP (XWINDOW (s->selected_window)->redo_mode_line));
	     

#ifdef ENERGIZE
	  int *old_sheets = x->current_psheets;
	  int *new_sheets = x->desired_psheets;
	  int old_count = x->current_psheet_count;
	  int new_count = x->desired_psheet_count;
	  Lisp_Object old_buf = x->current_psheet_buffer;
	  Lisp_Object new_buf = x->desired_psheet_buffer;
	  int psheets_changed = (old_sheets != new_sheets
				 || old_count != new_count
				 || old_buf != new_buf);
	  int debuggerpanel_changed = (desired_debuggerpanel_exposed_p
				       != current_debuggerpanel_exposed_p);
#endif

	  /* update the screen and icon titles */
	  if (title_changed)
	    x_format_screen_title (s, w);

	  /* update the p_sheets and menubars if needed */
	  if (buffer_changed || menubar_changed
#ifdef ENERGIZE
	      || psheets_changed || debuggerpanel_changed
#endif
	      )
	    {
	      x->last_selected_buffer = w->buffer;
#ifdef ENERGIZE
	      x->current_psheets = x->desired_psheets;
	      x->current_psheet_count = x->desired_psheet_count;
	      x->current_psheet_buffer = x->desired_psheet_buffer;

	      /* Update the sensitivity of the "psheet" button.
		 We do this by setting the "sensitive" property of the widget
		 directly, instead of by going through the usual interface of
		 set-screen-menubar, because it's a *lot* faster this way.
		 When we call set-screen-menubar, it syncs the menubar widgets
		 with what emacs thinks they should be, sensitivity, keyboard
		 equivalents, and all.  If the keybindings of the buffer have
		 changed, it might have to delete and recreate a bunch of
		 windows, which could take a second or two when using a really
		 slow X server (like an X terminal over a serial line).  It's
		 ok (and, in fact, unavoidable) to have to suffer that delay
		 between the time when the mouse goes down in the menubar and
		 when the menu pops up, but it's *not* ok to have that delay
		 when simply switching buffers or windows.  It's very
		 frustrating.

		 One side-effect of doing it this way is that the "sensitive-p"
		 element of the entry for the "sheet" button in the list
		 returned by (screen-menubar) has no correlation to reality.
		 Oh well.
	       */
	      if (x->menubar_widget && XtIsManaged (x->menubar_widget))
		{
		  Widget sheet = XtNameToWidget (x->menubar_widget, "*sheet");
		  Boolean active_p =
		    (!NILP (Fenergize_psheets_visible_p (screen)) ||
		     !NILP (Fenergize_buffer_has_psheets_p
			    (XWINDOW (XSCREEN (screen)->selected_window)
			     ->buffer)));
		  if (sheet)
		    XtVaSetValues (sheet, XtNsensitive, active_p, 0);
		}
	      update_menubar_internal (screen, menubar_changed,
				       debuggerpanel_changed, psheets_changed,
				       old_sheets, new_sheets,
				       old_count, new_count);
#else /* ! ENERGIZE */
	      update_menubar_internal (screen, menubar_changed);
#endif
	    }
	}
    }
#ifdef ENERGIZE
  current_debuggerpanel_exposed_p = desired_debuggerpanel_exposed_p;
#endif
  
  menubar_disabled = 0;
}



void
syms_of_menubar ()
{
  defsubr (&Sscreen_menubar);
  defsubr (&Sset_screen_menubar);
  defsubr (&Spopup_menu);
  defsubr (&Spopup_menu_up_p);
#if 0
  defsubr (&Spopup_dialog_box);
#endif

  DEFVAR_LISP ("activate-menubar-hook", &Vactivate_menubar_hook,
   "Function or functions called before a menubar menu is pulled down.\n\
These functions are called with one argument, a list describing the current\n\
contents of the menubar, and should return a list describing the desired\n\
contents of the menubar.\n\n\
As a special case, these functions may return the symbol `t' to assert that\n\
they have made no changes.  These functions should not call the function\n\
`set-screen-menubar' -- that happens at the end if necessary.\n\n\
The functions on this hook are invoked after the mouse goes down, but before\n\
the menu is mapped, and may be used to activate, deactivate, add, or delete\n\
items from the menus.");
  Vactivate_menubar_hook = Qnil;
  Qactivate_menubar_hook = intern ("activate-menubar-hook");
  staticpro (&Qactivate_menubar_hook);

  DEFVAR_BOOL ("menubar-show-keybindings", &menubar_show_keybindings,
    "If true, the menubar will display keyboard equivalents.\n\
If false, only the command names will be displayed.");
  menubar_show_keybindings = 1;

  DEFVAR_BOOL ("menubar-disabled", &menubar_disabled,
    "If true, the menubar update doesn't happen. Set on entry to \n\
menubar update code, cleared on normal exit.");
  menubar_disabled = 0;
}
