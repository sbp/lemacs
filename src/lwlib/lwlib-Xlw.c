/* The lwlib interface to "xlwmenu" menus.
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

#include "lwlib-Xlw.h"
#include <X11/StringDefs.h>
#include <X11/IntrinsicP.h>
#include <X11/CompositeP.h>
#include <X11/Shell.h>
#include "xlwmenu.h"

/* Menu callbacks */
static void
pre_hook (Widget w, caddr_t client_data, caddr_t call_data)
{
  XlwMenuWidget bar = (XlwMenuWidget)w;
  widget_instance* instance = (widget_instance*)client_data;

  if (w->core.being_destroyed)
    return;

  if (instance->info->pre_activate_cb)
    instance->info->pre_activate_cb (w, NULL);
}

static void
pick_hook (Widget w, caddr_t client_data, caddr_t call_data)
{
  XlwMenuWidget bar = (XlwMenuWidget)w;
  widget_instance* instance = (widget_instance*)client_data;
  widget_value* val = (widget_value*)call_data;

  if (w->core.being_destroyed)
    return;

  if (instance->info->selection_cb && val && val->enabled
      && !val->contents)
    instance->info->selection_cb (w, val->call_data);

  if (instance->info->post_activate_cb)
    instance->info->post_activate_cb (w, NULL);
}

/* creation functions */
static Widget
xlw_create_menubar (widget_instance* instance)
{
  Widget widget =
    XtVaCreateWidget (instance->info->name, xlwMenuWidgetClass,
		      instance->parent,
		      XtNmenu, instance->info->val,
		      0);
  XtAddCallback (widget, XtNopen, pre_hook, (XtPointer)instance);
  XtAddCallback (widget, XtNselect, pick_hook, (XtPointer)instance);
  return widget;
}

static Widget
xlw_create_popup_menu (widget_instance* instance)
{
  Widget popup_shell =
    XtCreatePopupShell (instance->info->name, overrideShellWidgetClass,
			instance->parent, NULL, 0);
  
  Widget widget = 
    XtVaCreateManagedWidget ("popup", xlwMenuWidgetClass,
			     popup_shell,
			     XtNmenu, instance->info->val,
			     XtNhorizontal, False,
			     0);

  XtAddCallback (widget, XtNselect, pick_hook, (XtPointer)instance);

  return popup_shell;
}

widget_creation_entry 
xlw_creation_table [] =
{
  {"menubar", xlw_create_menubar},
  {"popup", xlw_create_popup_menu},
  {NULL, NULL}
};

Boolean
lw_lucid_widget_p (Widget widget)
{
  return True;
}

void
xlw_update_one_widget (widget_instance* instance, Widget widget,
		       widget_value* val)
{
  XlwMenuWidget mw;

  if (XtIsShell (widget))
    mw = (XlwMenuWidget)((CompositeWidget)widget)->composite.children [0];
  else
    mw = (XlwMenuWidget)widget;
  XtVaSetValues (widget, XtNmenu, val, 0);
}

void
xlw_update_one_value (widget_instance* instance, Widget widget,
		      widget_value* val)
{
  return;
}

void
xlw_popup_menu (Widget widget)
{
  XButtonPressedEvent dummy;
  XlwMenuWidget mw;

  if (!XtIsShell (widget))
    return;

  mw = (XlwMenuWidget)((CompositeWidget)widget)->composite.children [0];

  dummy.type = ButtonPress;
  dummy.serial = 0;
  dummy.send_event = 0;
  dummy.display = XtDisplay (widget);
  dummy.window = XtWindow (XtParent (widget));
  dummy.time = CurrentTime;
  dummy.button = 0;
  XQueryPointer (dummy.display, dummy.window, &dummy.root,
		 &dummy.subwindow, &dummy.x_root, &dummy.y_root,
		 &dummy.x, &dummy.y, &dummy.state);

  pop_up_menu (mw, &dummy);
}

