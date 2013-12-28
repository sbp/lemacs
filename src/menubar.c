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

/* created 16-dec-91 by jwz */

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

/* This variable is 1 if the menubar widget has to be updated. 
 It is set to 1 by set-menubar-dirty-flag and cleared when the widget
 has been indapted. */
int menubar_has_changed;

Lisp_Object Qcurrent_menubar;

static int menubar_disabled;

static void
set_screen_menubar (struct screen *s, int deep_p);

/* 1 if a menu is currently up, 0 otherwise. */
static int popup_menu_up_p;

int menubar_show_keybindings;

static widget_value *
menu_item_descriptor_to_widget_value (desc, menubar_p, deep_p)
     Lisp_Object desc;
     int menubar_p, deep_p;
{
  int i;
  widget_value *wv = (widget_value *) xmalloc (sizeof (widget_value));
  bzero (wv, sizeof (widget_value));

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
	  SYMBOLP ((Lisp_Object) wv->call_data))
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
      if (STRINGP (XCONS (desc)->car))
	{
	  wv->name = (char *) XSTRING (XCONS (desc)->car)->data;
	  desc = Fcdr (desc);
	}
      else if (menubar_p)
	wv->name = "menubar";
      else
	{
	  while (1)
	    Fsignal (Qerror,
		     Fcons (build_string
			    ("menu name (first element) must be a string"),
			    Fcons (desc, Qnil)));
	}

      wv->enabled = 1;
      if (deep_p || menubar_p)
	{
	  widget_value *prev = 0, *next;
	  for (; !NILP (desc); desc = Fcdr (desc))
	    {
	      next = menu_item_descriptor_to_widget_value (Fcar (desc),
							   0, deep_p);
	      if (! next)
		continue;
	      else if (prev)
		prev->next = next;
	      else
		wv->contents = next;
	      prev = next;
	    }
	}
      if (deep_p && !wv->contents)
	{
	  free (wv);
	  wv = 0;
	}
      break;
    default:
      free (wv);
      wv = 0;
      if (!NILP (desc)) /* ignore nil for now */
	while (1)
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

  /* This is the last buffer for which the menubar was displayed.
     If the buffer has changed, we may have to update things.
   */
  Lisp_Object last_menubar_buffer;

  /* This flag tells us if the menubar contents are up-to-date with respect
     to the current menubar structure.  If we want to actually pull down a
     menu and this is false, then we need to update things.
   */
  Lisp_Object menubar_contents_up_to_date;

  /* This is a vector of all of the callbacks of the menu buttons.  This is
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


/* Fill the screen menubar data vector with a flat representation of the 
   leaves of the menubar. */
static int
gcpro_menu_callbacks (s, menu, index)
     struct screen* s;
     Lisp_Object menu;
     int index;
{
  switch (XTYPE (menu))
    {
    case Lisp_Vector:
      if (XVECTOR (menu)->size > 2)
	{
	  Lisp_Object vector = SCREEN_MENUBAR_DATA (s)->all_callbacks;
	  if (!VECTORP (vector))
	    {
	      /* initialize the vector */
	      vector = Fmake_vector (10 + index * 2, Qnil);
	      SCREEN_MENUBAR_DATA (s)->all_callbacks = vector;
	    }
	  else if (XVECTOR (vector)->size < index)
	    {
	      /* reallocate the vector by doubling its size */
	      Lisp_Object new_vector = Fmake_vector (index * 2, Qnil);
	      SCREEN_MENUBAR_DATA (s)->all_callbacks = new_vector;
	      bcopy (XVECTOR (vector)->contents,
		     XVECTOR (new_vector)->contents,
		     XVECTOR (vector)->size * sizeof (Lisp_Object));
	      vector = new_vector;
	    }
	  XVECTOR (vector)->contents [index] = XVECTOR (menu)->contents [1];
	  index += 1;
	}
      break;
      
    case Lisp_String:
      break;

    case Lisp_Cons:
      {
	Lisp_Object current;
	for (current = menu; !NILP (current); current = Fcdr (current))
	  index = gcpro_menu_callbacks (s, Fcar (current), index);
      }
    }
  return index;
}


extern Lisp_Object Qeval, Vrun_hooks;
Lisp_Object Qactivate_menubar_hook, Vactivate_menubar_hook;
struct screen *x_any_window_to_screen (Window);

static void
pre_activate_callback (widget, id, client_data)
     Widget widget;
     BITS32 id;
     XtPointer client_data;
{
  struct gcpro gcpro1;
  struct screen* s = x_any_window_to_screen (XtWindow (widget));
  Lisp_Object menubar_data;
  Lisp_Object rest = Qnil;
  int any_changes = 0;

  if (!s)
    return;

  menubar_data = s->menubar_data;
  if (!VECTORP (menubar_data))
    return;

  GCPRO1 (rest);
  for (rest = Vactivate_menubar_hook; !NILP (rest); rest = Fcdr (rest))
    if (!EQ (call1 (XCONS (rest)->car, Qnil), Qt))
      any_changes = 1;
  if (any_changes ||
      NILP (SCREEN_MENUBAR_DATA (s)->menubar_contents_up_to_date))
    set_screen_menubar (s, 1);
  UNGCPRO;
}


extern Time mouse_timestamp;
extern Time global_mouse_timestamp;
extern Lisp_Object Faccept_process_output ();

static void
menubar_selection_callback (widget, id, client_data)
     Widget widget;
     BITS32 id;
     XtPointer client_data;
{
  Lisp_Object event, fn, arg;
  Lisp_Object data = (Lisp_Object) client_data;

  if (((int) client_data) == 0 || ((int) client_data) == -1)
    return;

  /* Flush the X and process input */
  Faccept_process_output (Qnil);

  if (SYMBOLP (data))
    {
      fn = Qcall_interactively;
      arg = data;
    }
  else if (CONSP (data))
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

DEFUN ("set-menubar-dirty-flag", Fset_menubar_dirty_flag,
       Sset_menubar_dirty_flag, 0, 0, 0,
       "Tells emacs that the menubar widget has to be updated")
     ()
{
  menubar_has_changed = 1;
  return Qnil;
}

#ifdef ENERGIZE
static void
set_panel_button_sensitivity (struct screen* s, widget_value* data)
{
  struct x_display *x = s->display.x;
  struct window *window = XWINDOW (s->selected_window);
  int current_buffer_psheets_count = 0;
  int current_buffer_psheets =
    get_psheets_for_buffer (window->buffer, &current_buffer_psheets_count);
  int panel_enabled = x->desired_psheets || current_buffer_psheets_count;
  widget_value* val;
  for (val = data->contents; val; val = val->next)
    if (!strcmp (val->name, "sheet"))
      {
	val->enabled = panel_enabled;
	return;
      }
}
#endif

static widget_value*
compute_menubar_data (struct screen* s, Lisp_Object menubar, int deep_p)
{
  widget_value* data;

  if (NILP (menubar))
    data = 0;
  else
    {
      data = menu_item_descriptor_to_widget_value (menubar, 1, deep_p);
#ifdef ENERGIZE
      if (data)
	set_panel_button_sensitivity (s, data);
#endif
    }
  return data;
}

static void
set_screen_menubar (struct screen *s, int deep_p)
{
  widget_value *data;
  Lisp_Object obuf = Fcurrent_buffer ();
  Lisp_Object menubar;
  int last_saved_entry;
  int i;

  if (! SCREEN_IS_X (s))
    return;

  /* evaluate `current-menubar' in the buffer of the selected window of
     the screen in question.
     */
  Fset_buffer (XWINDOW (s->selected_window)->buffer);
  menubar = Fsymbol_value (Qcurrent_menubar);
  Fset_buffer (obuf);

  data = compute_menubar_data (s, menubar, deep_p);

  if (NILP (s->menubar_data))
    s->menubar_data = Fmake_vector (MENUBAR_DATA_SIZE, Qnil);

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
      lw_modify_all_widgets (id, data, deep_p ? True : False);
    else
      {
	Widget parent = s->display.x->column_widget;
	Widget edit = s->display.x->edit_widget;

	/* It's the first time we map the menubar so compute its
	   contents completely once.  This makes sure that the menubar
	   components are created with the right type. */
	if (!deep_p)
	  {
	    free_widget_value (data);
	    data = compute_menubar_data (s, menubar, 1);
	  }

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
      }
    UNBLOCK_INPUT;
  }
  if (data) free_widget_value (data);
  SCREEN_MENUBAR_DATA (s)->menubar_contents_up_to_date = deep_p ? Qt : Qnil;
  SCREEN_MENUBAR_DATA (s)->last_menubar_buffer =
    XWINDOW (s->selected_window)->buffer;
  menubar_has_changed = 0;

  last_saved_entry = gcpro_menu_callbacks (s, menubar, 0);
  if (VECTORP (SCREEN_MENUBAR_DATA (s)->all_callbacks))
    for (i = last_saved_entry;
	 i < XVECTOR (SCREEN_MENUBAR_DATA (s)->all_callbacks)->size;
	 i++)
      XVECTOR (SCREEN_MENUBAR_DATA (s)->all_callbacks)->contents [i] = Qnil;
}

static void
popup_selection_callback (widget, id, client_data)
     Widget widget;
     BITS32 id;
     XtPointer client_data;
{
  menubar_selection_callback (widget, id, client_data);
}

static void
popup_down_callback (widget, id, client_data)
     Widget widget;
     BITS32 id;
     XtPointer client_data;
{
  popup_menu_up_p = 0;
}


DEFUN ("popup-menu", Fpopup_menu, Spopup_menu, 1, 1, 0,
       "Pop up the given menu.\n\
A menu is a list of menu items, strings, and submenus.\n\
The first element of a menu must be a string, which is the name of the\n\
 menu.  This is the string that will be displayed in the parent menu, if\n\
 any.  For toplevel menus, it is ignored.  This string is not displayed\n\
 in the menu itself.\n\
A menu item is a vector of three elements:\n\
 the name of the menu item (a string);\n\
 the `callback' of that item;\n\
 and whether this item is active (selectable.)\n\
If the `callback' of a menu item is a symbol, then it must name a\n\
 command.  It will be invoked with `call-interactively'. If it is\n\
 a list, then it is evaluated with `eval'.\n\
If an element of a menu is a string, then that string will be\n\
 presented in the menu as unselectable text.\n\
If an element of a menu is a string consisting solely of hyphens,\n\
 then that item will be presented as a solid horizontal line.\n\
If an element of a menu is a list, it is treated as a submenu.\n\
 The name of that submenu (the first element in the list) will be\n\
 used as the name of the item representing this menu on the parent.\n\
\n\
More precisely:\n\
\n\
   form		:=  <something to pass to `eval'>\n\
   command	:=  <a symbol or string, to pass to `call-interactively'>\n\
   callback 	:=  command | form\n\
   active-p	:=  <t or nil, whether this thing is selectable>\n\
   text		:=  <a string, non selectable>\n\
   menu-item	:=  '['  string callback active-p  ']'\n\
   name		:=  <string>\n\
   menu		:=  '(' name [ menu-item | menu | text ]+ ')'")
     (menu_desc)
     Lisp_Object menu_desc;
{
  static int menu_id;

  struct screen *s = selected_screen;
  char *name = " unnamed popup ";
  widget_value *data;
  Widget parent, menu;
  int id;

  if (!SCREEN_IS_X (s)) error ("not an X screen");
  if (SYMBOLP (menu_desc))
    {
      name = (char *) XSYMBOL (menu_desc)->name->data;
      menu_desc = Fsymbol_value (menu_desc);
    }
  CHECK_CONS (menu_desc, 0);
  CHECK_STRING (XCONS (menu_desc)->car, 0);
  data = menu_item_descriptor_to_widget_value (menu_desc, 0, 1);

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
  if (SYMBOLP (dbox_desc))
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
extern int debuggerpanel_sheet;
#endif

void
update_screen_menubars ()
{
  struct screen* s;
  Lisp_Object tail;
  
  for (tail = Vscreen_list; CONSP (tail); tail = XCONS (tail)->cdr)
    {
      Lisp_Object screen = XCONS (tail)->car;
      if (!SCREENP (screen))
	continue;
      s = XSCREEN (screen);
      
      if (!SCREEN_IS_X (s))
	continue;
      
      /* If the menubar_has_changed flag was set, or the displayed buffer
	 has changed we have to update the menubar. */
      if (menubar_has_changed
	  || !VECTORP (s->menubar_data)
	  || (SCREEN_MENUBAR_DATA (s)->last_menubar_buffer !=
	      XWINDOW (s->selected_window)->buffer))
	if (!MINI_WINDOW_P (XWINDOW (s->selected_window)))
	  set_screen_menubar (s, 0);
    }
}

static void
update_one_screen_psheets (screen)
     Lisp_Object screen;
{
  struct screen* s = XSCREEN (screen);
  struct x_display *x = s->display.x;
  struct window *window = XWINDOW (s->selected_window);
  
  int just_created = 0;
  int i;
  
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
  int menubar_changed;
  
  menubar_changed = (x->menubar_widget
		     && !XtIsManaged (x->menubar_widget));

#ifdef ENERGIZE
  x->current_psheets = x->desired_psheets;
  x->current_psheet_count = x->desired_psheet_count;
  x->current_psheet_buffer = x->desired_psheet_buffer;
#endif

  if (! (menubar_changed
#ifdef ENERGIZE
	 || psheets_changed || debuggerpanel_changed
#endif
	 ))
    return;

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
	  w = lw_get_widget (sheet, x->column_widget, 0);
	  if (psheets_changed && w)
	    {
	      notify_that_sheet_has_been_hidden (sheet);
	      XtVaSetValues (w, XtNmappedWhenManaged, 0, 0);
	      XtUnmanageChild (w);
	      XtUnmapWidget (w);
	    }
	}
    }

  /* remove debugger panel if present */
  if (current_debuggerpanel_exposed_p && debuggerpanel_sheet &&
      (menubar_changed || debuggerpanel_changed))
    {
      Widget w;
      int sheet = debuggerpanel_sheet;
      w = lw_get_widget (sheet, x->column_widget, 0);
      if (!desired_debuggerpanel_exposed_p && w)
	{
	  notify_that_sheet_has_been_hidden (sheet);
	  XtVaSetValues (w, XtNmappedWhenManaged, 0, 0);
	  XtUnmanageChild (w);
	  XtUnmapWidget (w);
	}
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
      w = lw_make_widget (debuggerpanel_sheet, x->column_widget, 0);
      fix_pane_constraints (w);
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
      w = lw_make_widget (sheet, x->column_widget, 0);
      fix_pane_constraints (w);
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
update_psheets ()
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
      if (!SCREENP (screen))
	continue;
      s = XSCREEN (screen);
      w = XWINDOW (s->selected_window);
      buf = XBUFFER (w->buffer);

      if (!SCREEN_IS_X (s)
	  || MINI_WINDOW_P (w)
	  || EQ (screen, Vglobal_minibuffer_screen))
	continue;

      update_one_screen_psheets (screen);
    }
#ifdef ENERGIZE
  current_debuggerpanel_exposed_p = desired_debuggerpanel_exposed_p;
#endif
  
  menubar_disabled = 0;
}



void
syms_of_menubar ()
{
  defsubr (&Sset_menubar_dirty_flag);
  defsubr (&Spopup_menu);
  defsubr (&Spopup_menu_up_p);
#if 0
  defsubr (&Spopup_dialog_box);
#endif

/*
 *
 *  This DEFVAR_LISP is just for the benefit of make-docfile.  there is no
 *  C variable Vcurrent_menubar - all C code must access the menubar via
 *  Qcurrent_menubar because it can be buffer-local.
 *

  DEFVAR_LISP ("current-menubar", &Vcurrent_menubar,
   "The current menubar.  This may be buffer-local.\n\
\n\
When the menubar is changed the function set-menubar-dirty-flag has to be\n\
called for the menubar to be updated on the screen.  See set-menubar\n\
and set-buffer-menubar.\n\
\n\
A menubar is a list of menus and menu-items.\n\
A menu is a list of menu items, strings, and submenus.\n\
The first element of a menu must be a string, which is the name of the\n\
 menu.  This is the string that will be displayed in the menubar, or in\n\
 the parent menu.  This string is not displayed in the menu itself.\n\
A menu item is a vector of three elements:\n\
 the name of the menu item (a string);\n\
 the `callback' of that item;\n\
 and whether this item is active (selectable.)\n\
If the `callback' of a menu item is a symbol, then it must name a\n\
 command.  It will be invoked with `call-interactively'. If it is\n\
 a list, then it is evaluated with `eval'.\n\
If an element of a menu (or menubar) is a string, then that string will\n\
 be presented in the menu (or menubar) as unselectable text.\n\
If an element of a menu is a string consisting solely of hyphens,\n\
 then that item will be presented as a solid horizontal line.\n\
If an element of a menu is a list, it is treated as a submenu.\n\
 The name of that submenu (the first element in the list) will be\n\
 used as the name of the item representing this menu on the parent.\n\
If an element of a menubar is `nil', then it is used to represent the\n\
 division between the set of menubar-items which are flushleft and those\n\
 which are flushright.  (Note: this isn't completely implemented yet.)\n\
\n\
After the menubar is clicked upon, but before any menus are popped up,\n\
 the functions on the activate-menubar-hook are invoked to make changes to\n\
 the menus and menubar.  This is intended to implement lazy alteration of\n\
 the sensitivity of menu items.\n\
\n\
The syntax, More precisely:\n\
\n\
   form		:=  <something to pass to `eval'>\n\
   command	:=  <a symbol or string, to pass to `call-interactively'>\n\
   callback 	:=  command | form\n\
   active-p	:=  <t or nil, whether this thing is selectable>\n\
   text		:=  <a string, non selectable>\n\
   menu-item	:=  '['  string callback active-p  ']'\n\
   name		:=  <string>\n\
   menu		:=  '(' name [ menu-item | menu | text ]+ ')'\n\
   partition	:=  'nil'\n\
   menubar	:=  '(' [ menu-item | menu | text ]* [ partition ]\n\
		        [ menu-item | menu | text ]*\n\
		     ')'");

  */

  Qcurrent_menubar = intern ("current-menubar");
  staticpro (&Qcurrent_menubar);
  Fset (Qcurrent_menubar, Qnil);

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
