/* The lwlib interface to Motif widgets.
   Copyright (C) 1992 Lucid, Inc.

This file is part of the Lucid Widget Library.

The Lucid Widget Library is free software; you can redistribute it and/or 
modify it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

The Lucid Widget Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of 
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


static char 
rcs_id [] = "$Header: lwlib-Xm.c,v 100.6 92/05/12 20:10:19 jwz Exp $";

#include "lwlib-Xm.h"
#include "lwlib-utils.h"

#include <X11/StringDefs.h>
#include <X11/IntrinsicP.h>
#include <X11/CoreP.h>
#include <X11/CompositeP.h>

#include <Xm/BulletinB.h>
#include <Xm/CascadeB.h>
#include <Xm/DrawingA.h>
#include <Xm/FileSB.h>
#include <Xm/Label.h>
#include <Xm/List.h>
#include <Xm/MenuShell.h>
#include <Xm/MessageB.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/SelectioB.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
#include <Xm/RowColumn.h>
#include <Xm/ScrolledW.h>
#include <Xm/Separator.h>
#include <Xm/DialogS.h>
#include <Xm/Form.h>

static void
xm_pull_down_callback (Widget widget, XtPointer closure, XtPointer call_data);
static void
xm_generic_callback (Widget widget, XtPointer closure, XtPointer call_data);
static void
xm_pop_down_callback (Widget widget, XtPointer closure, XtPointer call_data);


static void
xm_update_menu (widget_instance* instance, Widget widget, widget_value* val);

/* motif utility functions */
Boolean
lw_motif_widget_p (Widget widget)
{
  return XmIsPrimitive (widget) || XmIsManager (widget) || XmIsGadget (widget);
}

static XmString
resource_motif_string (Widget widget, char* name)
{
  XtResource resource;
  XmString result = 0;
  
  resource.resource_name = name;
  resource.resource_class = XmCXmString;
  resource.resource_type = XmRXmString;
  resource.resource_size = sizeof (XmString);
  resource.resource_offset = 0;
  resource.default_type = XtRImmediate;
  resource.default_addr = 0;

  XtGetSubresources (widget, (XtPointer)&result, "string", "String",
		     &resource, 1, NULL, 0);
  return result;
}

static void
destroy_all_children (Widget widget)
{
  Widget* children;
  unsigned int number;
  int i;

  children = XtCompositeChildren (widget, &number);
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
      XtFree (children);
    }
}

/* update the label of anything subclass of a label */
static void
xm_update_label (widget_instance* instance, Widget widget, widget_value* val)
{
  XmString string = 0;
  XmString key_string = 0;
  Arg al [256];
  int ac;
  
  ac = 0;

  if (val->value)
    {
      string = resource_motif_string (widget, val->value);

      if (string)
	{
	  XtSetArg (al [ac], XmNlabelString, string); ac++;
	  XtSetArg (al [ac], XmNlabelType, XmSTRING); ac++;
	}
    }
  
  if (val->key)
    {
      key_string = XmStringCreateLtoR (val->key, XmSTRING_DEFAULT_CHARSET);

      if (key_string)
	{
	  XtSetArg (al [ac], XmNacceleratorText, key_string); ac++;
	}
    }

  if (ac)
    XtSetValues (widget, al, ac);
  
  if (string)
    XmStringFree (string);

  if (key_string)
    XmStringFree (key_string);
}

/* update of buttons */
static void
xm_update_pushbutton (widget_instance* instance, Widget widget,
		      widget_value* val)
{
  XtRemoveAllCallbacks (widget, XmNactivateCallback);
  XtAddCallback (widget, XmNactivateCallback, xm_generic_callback, instance);
}

static void
xm_update_cascadebutton (widget_instance* instance, Widget widget,
			 widget_value* val)
{
  /* Should also rebuild the menu by calling ...update_menu... */
  XtRemoveAllCallbacks (widget, XmNcascadingCallback);
  XtAddCallback (widget, XmNcascadingCallback, xm_pull_down_callback,
		 instance);
}

/* update toggle and radiobox */
static void
xm_update_toggle (widget_instance* instance, Widget widget, widget_value* val)
{
  XtVaSetValues (widget, XmNset, val->selected, 0);
}

static void
xm_update_radiobox (widget_instance* instance, Widget widget,
		    widget_value* val)
{
  Widget toggle;
  widget_value* cur;

  /* update the callback */
  XtRemoveAllCallbacks (widget, XmNentryCallback);
  XtAddCallback (widget, XmNentryCallback, xm_generic_callback, instance);

  /* STRUCTURAL_CHANGE is not supported for radiobox */
  for (cur = val; cur; cur = cur->next)
    {
      toggle = XtNameToWidget (widget, cur->value);
      if (toggle)
	XtVaSetValues (toggle,
		       XmNset, cur->selected,
		       XmNsensitive, cur->enabled,
		       0);
    }
}

/* update a popup menu, pulldown menu or a menubar */
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
		     widget_value* val)
{
  Widget* children = 0;
  int num_children;
  int child_index;
  widget_value* cur;
  Widget button = 0;
  Widget menu;
  Arg al [256];
  int ac;
  Boolean menubar_p;

  /* Allocate the children array */
  for (num_children = 0, cur = val; cur; num_children++, cur = cur->next);
  children = (Widget*)XtMalloc (num_children * sizeof (Widget));

  /* tricky way to know if this RowColumn is a menubar or a pulldown... */
  menubar_p = False;
  XtSetArg (al[0], XmNisHomogeneous, &menubar_p);
  XtGetValues (widget, al, 1);

  /* add the unmap callback for popups and pulldowns */
  /*** this sounds bogus ***/
  if (!menubar_p)
    XtAddCallback (XtParent (widget), XmNpopdownCallback,
		   xm_pop_down_callback, (XtPointer)instance);

  for (child_index = 0, cur = val; cur; child_index++, cur = cur->next)
    {    
      ac = 0;
      XtSetArg (al [ac], XmNsensitive, cur->enabled); ac++;
      XtSetArg (al [ac], XmNalignment, 0); ac++;
      XtSetArg (al [ac], XmNuserData, cur->call_data); ac++;
      
      if (all_dashes_p (cur->name))
	{
	  button = XmCreateSeparator (widget, cur->name, NULL, 0);
	}
      else if (!cur->contents)
	{
	  if (menubar_p)
	    button = XmCreateCascadeButton (widget, cur->name, al, ac);
	  else if (!cur->call_data)
	    button = XmCreateLabel (widget, cur->name, al, ac);
	  else
	    button = XmCreatePushButtonGadget (widget, cur->name, al, ac);

	  xm_update_label (instance, button, cur);

	  /* don't add a callback to a simple label */
	  if (cur->call_data)
	    XtAddCallback (button, XmNactivateCallback, xm_generic_callback,
			   (XtPointer)instance);
	}
      else
	{
	  menu = XmCreatePulldownMenu (widget, "pulldown", NULL, 0);
	  make_menu_in_widget (instance, menu, cur->contents);
	  XtSetArg (al [ac], XmNsubMenuId, menu); ac++;
	  button = XmCreateCascadeButton (widget, cur->name, al, ac);

	  xm_update_label (instance, button, cur);

	  XtAddCallback (button, XmNcascadingCallback, xm_pull_down_callback,
			 (XtPointer)instance);
	}

      children [child_index] = button;
    }

  XtManageChildren (children, num_children);

  /* Last entry is the help button.  Has to be done after managing
   * the buttons otherwise the menubar is only 4 pixels high... */
  if (button)
    {
      ac = 0;
      XtSetArg (al [ac], XmNmenuHelpWidget, button); ac++;
      XtSetValues (widget, al, ac);
    }

  XtFree (children);
}

static void
update_one_menu_entry (widget_instance* instance, Widget widget,
		       widget_value* val)
{
  Arg al [256];
  int ac;
  Widget menu;
  widget_value* contents;

  if (val->change == NO_CHANGE)
    return;

  /* update the sensitivity and userdata */
  /* Common to all widget types */
  XtVaSetValues (widget,
		 XmNsensitive, val->enabled,
		 XmNuserData, val->call_data,
		 0);

  /* update the menu button as a label. */
  if (val->change >= VISIBLE_CHANGE)
    xm_update_label (instance, widget, val);

  /* update the pulldown/pullaside as needed */
  ac = 0;
  menu = NULL;
  XtSetArg (al [ac], XmNsubMenuId, &menu); ac++;
  XtGetValues (widget, al, ac);
  
  contents = val->contents;

  if (!menu)
    {
      if (contents)
	{
	  menu = XmCreatePulldownMenu (widget, "pulldown", NULL, 0);
	  make_menu_in_widget (instance, menu, contents);
	  ac = 0;
	  XtSetArg (al [ac], XmNsubMenuId, menu); ac++;
	  XtSetValues (widget, al, ac);
	}
    }
  else if (!contents)
    {
      ac = 0;
      XtSetArg (al [ac], XmNsubMenuId, NULL); ac++;
      XtSetValues (widget, al, ac);
      XtDestroyWidget (menu);
    }
  else if (contents->change != NO_CHANGE)
    xm_update_menu (instance, menu, val);
}

static void
xm_update_menu (widget_instance* instance, Widget widget, widget_value* val)
{
  /* Widget is a RowColumn widget whose contents have to be updated
   * to reflect the list of items in val->contents */
  if (val->contents->change == STRUCTURAL_CHANGE)
    {
      destroy_all_children (widget);
      make_menu_in_widget (instance, widget, val->contents);
    }
  else
    {
      /* Update all the buttons of the RowColumn in order. */
      Widget* children;
      unsigned int num_children;
      int i;
      widget_value* cur;

      children = XtCompositeChildren (widget, &num_children);
      if (children)
	{
	  for (i = 0, cur = val->contents; i < num_children; i++)
	    {
	      if (!cur)
		abort ();
	      if (children [i]->core.being_destroyed
		  || strcmp (XtName (children [i]), cur->name))
		continue;
	      update_one_menu_entry (instance, children [i], cur);
	      cur = cur->next;
	    }
	  XtFree (children);
	}
      if (cur)
	abort ();
    }
}


/* update text widgets */

static void
xm_update_text (widget_instance* instance, Widget widget, widget_value* val)
{
  XmTextSetString (widget, val->value);
  XtRemoveAllCallbacks (widget, XmNactivateCallback);
  XtAddCallback (widget, XmNactivateCallback, xm_generic_callback, instance);
}

static void
xm_update_text_field (widget_instance* instance, Widget widget,
		      widget_value* val)
{
  XmTextFieldSetString (widget, val->value);
  XtRemoveAllCallbacks (widget, XmNactivateCallback);
  XtAddCallback (widget, XmNactivateCallback, xm_generic_callback, instance);
}


/* update a motif widget */

void
xm_update_one_widget (widget_instance* instance, Widget widget,
		      widget_value* val)
{
  WidgetClass class;
  
  /* Common to all widget types */
  XtVaSetValues (widget,
		 XmNsensitive, val->enabled,
		 XmNuserData, val->call_data,
		 0);
  
  /* Common to all label like widgets */
  if (XtIsSubclass (widget, xmLabelWidgetClass))
    xm_update_label (instance, widget, val);
  
  class = XtClass (widget);
  /* Class specific things */
  if (class == xmPushButtonWidgetClass)
    {
      xm_update_pushbutton (instance, widget, val);
    }
  else if (class == xmCascadeButtonWidgetClass)
    {
      xm_update_cascadebutton (instance, widget, val);
    }
  else if (class == xmToggleButtonWidgetClass
	   || class == xmToggleButtonGadgetClass)
    {
      xm_update_toggle (instance, widget, val);
    }
  else if (class == xmRowColumnWidgetClass)
    {
      Boolean radiobox = 0;
      int ac = 0;
      Arg al [1];
      
      XtSetArg (al [ac], XmNradioBehavior, &radiobox); ac++;
      XtGetValues (widget, al, ac);
      
      if (radiobox)
	xm_update_radiobox (instance, widget, val);
      else
	xm_update_menu (instance, widget, val);
    }
  else if (class == xmTextWidgetClass)
    {
      xm_update_text (instance, widget, val);
    }
  else if (class == xmTextFieldWidgetClass)
    {
      xm_update_text_field (instance, widget, val);
    }
}

/* getting the value back */
void
xm_update_one_value (widget_instance* instance, Widget widget,
		     widget_value* val)
{
  WidgetClass class = XtClass (widget);

  if (class == xmToggleButtonWidgetClass || class == xmToggleButtonGadgetClass)
    {
      XtVaGetValues (widget, XmNset, &val->selected, 0);
    }
  else if (class == xmTextWidgetClass)
    {
      if (val->value)
	free (val->value);
      val->value = XmTextGetString (widget);
    }
  else if (class == xmTextFieldWidgetClass)
    {
      if (val->value)
	free (val->value);
      val->value = XmTextFieldGetString (widget);
    }
}


/* creation functions */

Widget
xm_create_menubar (widget_instance* instance)
{
  return XmCreateMenuBar (instance->parent, instance->info->name, NULL, 0);
}

static void
remove_grabs (Widget shell, XtPointer closure, XtPointer call_data)
{
  Widget menu = (Widget)closure;
  XmRemoveFromPostFromList (menu, XtParent (XtParent (menu)));
}

Widget
xm_create_popup_menu (widget_instance* instance)
{
  Widget parent = instance->parent;
  Window parent_window = parent->core.window;
  Widget result;

  /* sets the parent window to 0 to fool Motif into not generating a grab */
  parent->core.window = 0;
  result = XmCreatePopupMenu (parent, instance->info->name, NULL, 0);
  XtAddCallback (XtParent (result), XmNpopdownCallback, remove_grabs,
		 (XtPointer)result);
  parent->core.window = parent_window;
  return result;
}

/* Table of functions to create widgets */
widget_creation_entry
xm_creation_table [] = 
{
  {"menubar", xm_create_menubar},
  {"popup", xm_create_popup_menu},
  {NULL, NULL}
};

/* popup utility */
void
xm_popup_menu (Widget widget)
{
  XButtonPressedEvent dummy;
  XEvent* event;

  dummy.type = ButtonPress;
  dummy.serial = 0;
  dummy.send_event = 0;
  dummy.display = XtDisplay (widget);
  dummy.window = XtWindow (XtParent (widget));
  dummy.time = 0;
  dummy.button = 0;
  XQueryPointer (dummy.display, dummy.window, &dummy.root,
		 &dummy.subwindow, &dummy.x_root, &dummy.y_root,
		 &dummy.x, &dummy.y, &dummy.state);
  event = (XEvent *) &dummy;

  if (event->type == ButtonPress || event->type == ButtonRelease)
    {
      /* This is so totally ridiculous: there's NO WAY to tell Motif
	 that *any* button can select a menu item.  Only one button
	 can have that honor.
       */
      char *trans = 0;
      if      (event->xbutton.state & Button5Mask) trans = "<Btn5Down>";
      else if (event->xbutton.state & Button4Mask) trans = "<Btn4Down>";
      else if (event->xbutton.state & Button3Mask) trans = "<Btn3Down>";
      else if (event->xbutton.state & Button2Mask) trans = "<Btn2Down>";
      else if (event->xbutton.state & Button1Mask) trans = "<Btn1Down>";
      if (trans) XtVaSetValues (widget, XmNmenuPost, trans, 0);
      XmMenuPosition (widget, (XButtonPressedEvent *) event);
    }
  XtManageChild (widget);
}



/* motif callback */ 

enum do_call_type { pre_activate, selection, no_selection, post_activate };

static void
do_call (Widget widget, XtPointer closure, enum do_call_type type)
{
  Arg al [256];
  int ac;
  XtPointer user_data;
  widget_instance* instance = (widget_instance*)closure;

  if (!instance)
    return;
  if (widget->core.being_destroyed)
    return;

  ac = 0;
  user_data = NULL;
  XtSetArg (al [ac], XmNuserData, &user_data); ac++;
  XtGetValues (widget, al, ac);
  switch (type)
    {
    case pre_activate:
      if (instance->info->pre_activate_cb)
	instance->info->pre_activate_cb (instance->widget, user_data);
      break;
    case selection:
      if (instance->info->selection_cb)
	instance->info->selection_cb (instance->widget, user_data);
      break;
    case no_selection:
      if (instance->info->selection_cb)
	instance->info->selection_cb (instance->widget, (XtPointer) -1);
      break;
    case post_activate:
      if (instance->info->post_activate_cb)
	instance->info->post_activate_cb (instance->widget, user_data);
      break;
    default:
      exit (-69);
    }
}

static void
xm_generic_callback (Widget widget, XtPointer closure, XtPointer call_data)
{
  do_call (widget, closure, selection);
}

static void
xm_pull_down_callback (Widget widget, XtPointer closure, XtPointer call_data)
{
  do_call (widget, closure, pre_activate);
}

static void
xm_pop_down_callback (Widget widget, XtPointer closure, XtPointer call_data)
{
  do_call (widget, closure, post_activate);
}


/* set the keyboard focus */
void
xm_set_keyboard_focus (Widget parent, Widget w)
{
  XmProcessTraversal (w, 0);
  XtSetKeyboardFocus (parent, w);
}
