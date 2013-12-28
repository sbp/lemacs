/* The lwlib interface to Open-Look widgets.
   Copyright (C) 1992, 1993, 1994 Lucid, Inc.

This file is part of the Lucid Widget Library.

The Lucid Widget Library is free software; you can redistribute it and/or 
modify it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

The Lucid Widget Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of 
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "lwlib-Xol.h"
#include <X11/StringDefs.h>
#include <X11/IntrinsicP.h>
#include <X11/CompositeP.h>
#include <X11/Shell.h>
#include <Xol/Menu.h>
#include <Xol/OpenLook.h>
#include <Xol/MenuButton.h>
#include <Xol/OblongButt.h>
#include <Xol/ControlAre.h>
#include <Xol/OpenLookP.h>      /* so ControlArP.h can be included */
#include <Xol/ControlArP.h>
#include <Xol/Stub.h>
#include <Xol/StaticText.h>
#include "lwlib-Xol-mb.h"

extern int olit_menu_up_flag;

struct internal_client_data
{
  widget_instance *instance;
  Widget menushell;
  Widget menupane;
};

void
place_menu (Widget menu, Widget emanate, Cardinal item_index, OlDefine state,
	    Position *mx, Position *my, Position *px, Position *py)
{
  Widget parent;
  Position pos_x, pos_y;
  Dimension button_w, button_h;
  int display_width, display_height;

  display_height = XDisplayHeight (XtDisplay(menu), 0);
  display_width = XDisplayWidth (XtDisplay(menu), 0);


  parent = menu->core.parent;
  pos_x = pos_y = 0;
  button_w = parent->core.width + 1;
  button_h = parent->core.height + 1;

  while (parent)
    {
      pos_x += parent->core.x;
      pos_y += parent->core.y;
      parent = parent->core.parent;
    }

  pos_x += ((Position) button_w / (Position) 2);
  pos_y += button_h;
  pos_x -= ((Position) (menu->core.width + 1) / (Position) 2);

  if (pos_x < 0)
    pos_x = 0;
  else if ((Position)(pos_x + menu->core.width) > (Position)display_width)
    pos_x = display_width - menu->core.width;

  if (pos_y < 0)
    pos_y = 0;
  else if ((Position)(pos_y + menu->core.height) >
	   (Position)display_height)
    pos_y = display_height - menu->core.height;

  *mx = pos_x;
  *my = pos_y;
}

void
ActivateDefaultOption (Widget w)
{
  XButtonPressedEvent dummy_p, dummy_r;

  dummy_p.type = ButtonPress;
  dummy_p.serial = 0;
  dummy_p.send_event = 0;
  dummy_p.display = XtDisplay(w);
  dummy_p.window = XtWindow (w);
  dummy_p.time = CurrentTime;
  dummy_p.button = 1;

  XQueryPointer (dummy_p.display, dummy_p.window, &dummy_p.root,
		 &dummy_p.subwindow, &dummy_p.x_root, &dummy_p.y_root,
		 &dummy_p.x, &dummy_p.y, &dummy_p.state);

  dummy_r.type = ButtonRelease;
  dummy_r.serial = 0;
  dummy_r.send_event = 0;
  dummy_r.display = XtDisplay(w);
  dummy_r.window = XtWindow (w);
  dummy_r.time = CurrentTime;
  dummy_r.button = 1;

  XQueryPointer (dummy_r.display, dummy_r.window, &dummy_r.root,
		 &dummy_r.subwindow, &dummy_r.x_root, &dummy_r.y_root,
		 &dummy_r.x, &dummy_r.y, &dummy_r.state);

  XtDispatchEvent ((XEvent *) &dummy_p);
  XtDispatchEvent ((XEvent *) &dummy_r);
}

Widget
FindDefaultWidget (Widget w)
{
  int cnt = 0;
  Boolean default_item;
  ControlAreaWidget wc = (ControlAreaWidget) w;

  while (cnt < (int) wc->composite.num_children)
    {
      XtVaGetValues (wc->composite.children[cnt],
		     XtNdefault, &default_item, 0);
      if (default_item)
	return wc->composite.children[cnt];
      else
	cnt++;
    }

  return ((Widget) NULL);
}

/* forward declarations */
static void
update_menu_widget (widget_instance* instance, Widget widget,
		    widget_value* val);

/* Menu callbacks */
static void
pre_hook (Widget w, caddr_t client_data, caddr_t call_data)
{
  OlVirtualEvent ve = (OlVirtualEvent)call_data;
  widget_instance* instance =
    ((struct internal_client_data *)client_data)->instance;
  Widget menushell = ((struct internal_client_data *)client_data)->menushell;
  Widget menupane = ((struct internal_client_data *)client_data)->menupane;
  Widget default_widget;
  String current_label;

  if (w->core.being_destroyed)
    return;

  if (XtParent (w) == instance->widget)
    {
      switch (ve->xevent->type)
	{
	case ButtonPress:
	  if (instance->info->pre_activate_cb)
	    instance->info->pre_activate_cb (instance->widget,
					     instance->info->id, NULL);
	  if (menushell && ve->xevent->xbutton.button == Button3)
	    OlMenuPopup ((Widget)menushell, NULL, NULL, OL_STAYUP_MENU, FALSE,
			 0, 0, (OlMenuPositionProc) place_menu);
	  if (menushell && ve->xevent->xbutton.button == Button1)
	    {
	      default_widget = FindDefaultWidget (menupane);
	      if (default_widget)
		XtVaSetValues (w, XtNlabel, default_widget->core.name, 0);
	      else
		XtVaSetValues (w, XtNlabel, "", 0);
	    }
	  break;
	case ButtonRelease:
	  if (menushell && ve->xevent->xbutton.button == Button3)
	    OlMenuPopdown (menushell, False);
	  if (menushell && ve->xevent->xbutton.button == Button1)
	    {
	      XtVaGetValues (w, XtNlabel, &current_label, 0);
	      if (strcmp (current_label, menushell->core.name))
		XtVaSetValues (w, XtNlabel, menushell->core.name, 0);

	      default_widget = FindDefaultWidget (menupane);
	      if (default_widget)
		if (!XtIsRealized (menushell))
		  XtRealizeWidget (menushell);
	      ActivateDefaultOption (default_widget);
	    }
	  break;
	case EnterNotify:
	  if (ve->xevent->xcrossing.state & Button1MotionMask)
	    {
	      default_widget = FindDefaultWidget (menupane);
	      if (default_widget)
		XtVaSetValues (w, XtNlabel, default_widget->core.name, 0);
	      else
		XtVaSetValues (w, XtNlabel, "", 0);
	    }
	  break;
	case LeaveNotify:
	  XtVaGetValues (w, XtNlabel, &current_label, 0);
	  if (strcmp (current_label, menushell->core.name))
	    XtVaSetValues (w, XtNlabel, menushell->core.name, 0);
	  break;
	default:
	  break;
	}
    }
}

#if 0 /* Unused */
static void
post_hook (Widget w, caddr_t client_data, caddr_t call_data)
{
  widget_instance* instance = (widget_instance*)client_data;
  
  if (w->core.being_destroyed)
    return;
  
  if (instance->info->post_activate_cb)
    instance->info->post_activate_cb (w, instance->info->id, NULL);
}
#endif

static void
pick_hook (Widget w, caddr_t client_data, caddr_t call_data)
{
  OlVirtualEvent ve = (OlVirtualEvent)call_data;
  widget_instance* instance = 0;
  widget_value* val = (widget_value*)client_data;

  if (w->core.being_destroyed)
    return;

  if (val == NULL)
    {
      if (ve->xevent->type == ButtonRelease)
	if (ve->xevent->xbutton.button == Button3)
	  ActivateDefaultOption (w);
    }
  else
    {
      XtVaGetValues (w, XtNuserData, &instance, 0);

      if (!instance)
	return;

      if (instance->info->selection_cb && val && val->enabled
	  && !val->contents)
	instance->info->selection_cb (w, instance->info->id, val->call_data);
    }
}

static void
popup_hook (Widget w, caddr_t client_data, caddr_t call_data)
{
  olit_menu_up_flag = 1;
}

static void
popdown_hook (Widget w, caddr_t client_data, caddr_t call_data)
{
  olit_menu_up_flag = 0;
}

/* creation functions */
static Widget
xol_create_menubar (widget_instance* instance)
{
  Widget widget =
    XtVaCreateWidget (instance->info->name, lwMenubarWidgetClass,
		      instance->parent, 0);

  return widget;
}

static Widget
xol_create_popup_menu (widget_instance* instance)
{
  Widget popup_shell =
    XtVaCreatePopupShell (instance->info->name, menuShellWidgetClass,
			  instance->parent, XtNmenuAugment, FALSE, 0);

  OlAddCallback (popup_shell, (String) XtNpopupCallback,
		 (XtCallbackProc) popup_hook, (XtPointer) NULL);
  OlAddCallback (popup_shell, (String) XtNpopdownCallback,
		 (XtCallbackProc) popdown_hook, (XtPointer) NULL);

  return popup_shell;
}

widget_creation_entry 
xol_creation_table [] =
{
  {"menubar", xol_create_menubar},
  {"popup", xol_create_popup_menu},
  {NULL, NULL}
};

/*
   I guess this is in here as a marker.  However, since USE_OLIT isn't
   defined in here this is causing an identifier redeclared error.
   Since there aren't any OLIT dialog boxes at the moment, just zap it.

#ifndef USE_OLIT
Widget
xol_create_dialog (widget_instance* instance)
{
  return NULL;
}
#endif

*/
 
Boolean
lw_olit_widget_p (Widget widget)
{
  return True;
}

/* update functions */
static void
destroy_all_children (Widget widget)
{
  Widget* children;
  unsigned int number;
  int i;

  children = (Widget *) XtCompositeChildren (widget, &number);
  if (children)
    {
      /* Unmanage all children and destroy them.  They will only be 
       * really destroyed when we get out of DispatchEvent. */
      for (i = 0; i < number; i++)
	{
	  Widget child = children [i];
	  if (!child->core.being_destroyed)
	    {
	      XtUnmanageChild (child);
	      XtDestroyWidget (child);
	    }
	}
      XtFree ((char *) children);
    }
}

static Boolean
all_dashes_p (char* s)
{
  char* t;
  for (t = s; *t; t++)
    if (*t != '-')
      return False;
  return True;
}

static void
make_menu_in_widget (widget_instance* instance, Widget widget,
		     widget_value* val, int initial)
{
  widget_value* cur;
  Widget button;
  Arg al [256];
  int ac;
  int first;
  if (initial)
    first = 0;
  else
    first = 1;

  for (cur = val; cur; cur = cur->next)
    {    
      ac = 0;
      XtSetArg (al [ac], XtNsensitive, cur->enabled); ac++;
      XtSetArg (al [ac], XtNuserData, instance); ac++;
      XtSetArg (al [ac], XtNacceleratorText, cur->key); ac++;
      
      if (all_dashes_p (cur->name))
	{
	  /* no separator in OpenLook just make some space. */
	  XtSetArg (al [ac], XtNheight, 5); ac++;
	  XtSetArg (al [ac], XtNwidth, 5); ac++;
	  button = XtCreateWidget (cur->name, stubWidgetClass, widget, al, ac);
	}
      else if (!cur->contents)
	{
	  if (!cur->call_data)
	    button =
	      XtCreateManagedWidget (cur->name, staticTextWidgetClass, widget,
				     al, ac);
	  else
	    {
	      if (first && cur->enabled)
		{
		  XtSetArg (al [ac], XtNdefault, True); ac++;
		  first = 0;
		}
		
	      button =
		XtCreateManagedWidget (cur->name, oblongButtonWidgetClass,
				       widget, al, ac);
	      OlAddCallback (button, (String) XtNselect,
			     (XtCallbackProc) pick_hook, (XtPointer) cur);
	    }
	}
      else
	{
	  Widget menu = NULL;
	  Widget menu_pane = NULL;
	  struct internal_client_data *client_data;

	  client_data = (struct internal_client_data *)
	    XtMalloc (sizeof (struct internal_client_data));

	  XtSetArg (al [ac], XtNscale, 10); ac++;
	  XtSetArg (al [ac], XtNpushpin, OL_OUT); ac++;

	  if (initial)
	    {
	      XtSetArg (al [ac], XtNmenuAugment, FALSE); ac++;

	      button =
		XtCreateManagedWidget (cur->name, oblongButtonWidgetClass,
				       widget, al, ac);
	      menu =
		XtCreatePopupShell (cur->name, menuShellWidgetClass, button,
				    al, ac);
	      XtVaGetValues (menu, XtNmenuPane, &menu_pane, NULL);
	    }
	  else
	    {
	      if (first && cur->enabled)
		{
		  XtSetArg (al [ac], XtNdefault, True); ac++;
		  first = 0;
		}

	      button =
		XtCreateManagedWidget (cur->name, menuButtonWidgetClass,
				       widget, al, ac);
	      XtVaGetValues (button, XtNmenuPane, &menu_pane, NULL);
	    }

	  if (!menu_pane)
	    abort ();
	  make_menu_in_widget (instance, menu_pane, cur->contents, 0);

	  client_data->instance = instance;
	  if (initial)
	    {
	      client_data->menushell = menu;
	      client_data->menupane = menu_pane;
	      XtVaSetValues (button, XtNrecomputeSize, FALSE, 0);
	    }
	  else
	    client_data->menushell = NULL;

	  if (initial)
	    {
	      OlAddCallback (button, (String) XtNconsumeEvent,
			     (XtCallbackProc) pre_hook,
			     (XtPointer) client_data);
	      OlAddCallback (menu, (String) XtNpopupCallback,
			     (XtCallbackProc) popup_hook,
			     (XtPointer) instance);
	      OlAddCallback (menu, (String) XtNpopdownCallback,
			     (XtCallbackProc) popdown_hook,
			     (XtPointer) instance);
	    }
	  else
	    {
	      OlAddCallback (button, (String) XtNconsumeEvent,
			     (XtCallbackProc) pick_hook, (XtPointer) NULL);
	    }
	}
    }
}

static void
update_one_menu_entry (widget_instance* instance, Widget widget,
		       widget_value* val)
{
  Widget menu;
  widget_value* contents;

  if (val->change == NO_CHANGE)
    return;

  /* update the sensitivity */
  XtVaSetValues (widget, XtNsensitive, val->enabled, 0);

  /* update the pulldown/pullaside as needed */
  menu = NULL;
  XtVaGetValues (widget, XtNmenuPane, &menu, 0);
  contents = val->contents;

  if (!menu)
    {
      if (contents)
	{
	  /* in OLIT this woudl have to be a structural change on the
	     button. */
	  abort ();
	}
    }
  else if (!contents)
    {
      /* in OLIT this woudl have to be a structural change on the button. */
      abort ();
    }
  else if (contents->change != NO_CHANGE)
    update_menu_widget (instance, menu, val);
}

static void
update_menu_widget (widget_instance* instance, Widget widget,
		    widget_value* val)

{
  if (val->change == STRUCTURAL_CHANGE
      || val->contents->change == STRUCTURAL_CHANGE)
    {
      destroy_all_children (widget);

      if (!strcmp (widget->core.widget_class->core_class.class_name,
		   "Menubar"))
	make_menu_in_widget (instance, widget, val->contents, 1);
      else
	make_menu_in_widget (instance, widget, val->contents, 0);
    }
  else
    {
      /* Update all the buttons of the composite widget in order. */
      Widget* children;
      unsigned int num_children;
      int i;
      widget_value* cur = 0;
      
      children = (Widget *) XtCompositeChildren (widget, &num_children);
      if (children)
	{
	  for (i = 0, cur = val->contents; i < num_children; i++)
	    {
	      if (!cur)
		abort ();
	      if (children [i]->core.being_destroyed
		  || strcmp (XtName (children [i]), cur->name))
		continue;
	      if (cur->change >= VISIBLE_CHANGE)
		update_one_menu_entry (instance, children [i], cur);
	      cur = cur->next;
	    }
	  XtFree ((char *) children);
	}
      if (cur)
	abort ();
    }
}

void
xol_update_one_widget (widget_instance* instance, Widget widget,
		       widget_value* val, Boolean deep_p)
{
  Widget menu = widget;

  if (XtIsShell (widget))
    XtVaGetValues (widget, XtNmenuPane, &menu, 0);

  update_menu_widget (instance, menu, val);
}

void
xol_update_one_value (widget_instance* instance, Widget widget,
		      widget_value* val)
{
  return;
}

void
xol_pop_instance (widget_instance* instance, Boolean up)
{
}

void
xol_popup_menu (Widget widget)
{
  OlMenuPopup (widget, NULL, NULL, OL_STAYUP_MENU, FALSE,
	       0, 0, (OlMenuPositionProc) NULL);
}

/* Destruction of instances */
void
xol_destroy_instance (widget_instance* instance)
{
/*  XtDestroyWidget (instance->widget); */
}
