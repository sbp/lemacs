/* Drag-and-drop support under Motif.
   Copyright (C) 1993 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

/* Written by Ben Wing, September 1993. */

#include "config.h"
#include "intl.h"

Lisp_Object Qdrag_and_drop_error;

#ifdef TEMP_NOT_DEFINED

DEFUN ("xm-create-drag-icon", Fxm_create_drag_icon, Sxm_create_drag_icon,
       3, 3, 0, "Create a DragIcon widget."
)
     (screen, name, values)
     Lisp_Object screen, name, values;
{
  Widget w;
  struct screen *s;

  CHECK_SCREEN (screen, 0);
  s = XSCREEN (screen);
  CHECK_STRING (name, 0);
  CHECK_LIST (values, 0);
  
  values_to_xt_arglist (values, 1, xxxx_class_XmDragIcon, access_create);
  BLOCK_INPUT;
  w = XmCreateDragIcon (s->display.x->edit_widget,
			XSTRING (name)->data,
			xtf_argcur,
			xtf_arglist);
  UNBLOCK_INPUT;
  return make_widget (w, xxxx_class_XmDragIcon, Qnil, 1);
}

Lisp_Object make_drag_context (Widget w)
{
  Lisp_Object obj = make_widget (w, xxxx_class_XmDragContext, Qnil, 9);
  struct Lisp_Widget *wid = XWIDGET (obj);

  wid->destroyable = 0;
  return obj;
}

static void widget_destroy_callback (Widget w, XtPointer client_data,
					XtPointer call_data)
{
  Lisp_Object obj;

  obj = find_widget (w);
  if (NILP (obj))
    abort ();
  zero_widget (XWIDGET (obj));
}

DEFUN ("xm-drag-start", Fxm_drag_start, Sxm_drag_start, 3, 3, 0,
       "Initiate a drag-and-drop transaction.\n\
")
     (screen, event, values)
     Lisp_Object screen, event, values;
{
  struct screen *s;
  XEvent xev;
  Widget w;

  CHECK_SCREEN (screen, 0);
  s = XSCREEN (screen);
  CHECK_EVENT (event, 0);
  CHECK_LIST (values, 0);

  if (event->event_type != button_press_event)
    signal_error (Qdrag_and_drop_error, list2
		  (build_string
		   (GETTEXT ("Must be button-press event")),
		   event));
		   
  emacs_event_to_x_event (event, &xev);
  values_to_xt_arglist (values, 1, xxxx_class_XmDragContext, access_create);
  BLOCK_INPUT;
  w = XmDragStart (s->display.x->edit_widget, &xev, arglist, argcount);
  XtAddCallback (w, XtNdestroyCallback, widget_destroy_callback,
		 (XtPointer) 0);
  UNBLOCK_INPUT;
    
  return make_drag_context (w);
}

DEFUN ("xm-drag-cancel", Fxm_drag_cancel, Sxm_drag_cancel, 1, 1, 0,
       "Terminate a drag-and-drop transaction.\n\
")
     (drag_context)
     Lisp_Object drag_context;
{
  struct Lisp_Widget *w;

  check_valid_widget_type (drag_context, xxxx_class_XmDragContext);
  w = XWIDGET (drag_context);
  BLOCK_INPUT;
  XmDragCancel (w->widget);
  UNBLOCK_INPUT;
  return Qnil;
}

#if 0

/* The stacking-order and multiple-update functions are useful when
   one shell widget has lots of drop sites, which is not the case for
   us for the moment: There is currently one drop site per Emacs screen. */

DEFUN ("xm-drop-site-configure-stacking-order",
       Fxm_drop_site_configure_stacking_order,
       Sxm_drop_site_configure_stacking_order,
       3, 3, 0, "")
{
}

DEFUN ("xm-drop-site-end-update",
       Fxm_drop_site_end_update,
       Sxm_drop_site_end_update,
       3, 3, 0, "")
{
}

DEFUN ("xm-drop-site-query-stacking-order",
       Fxm_drop_site_query_stacking_order,
       Sxm_drop_site_query_stacking_order,
       3, 3, 0, "")
{
}

DEFUN ("xm-drop-site-start-update",
       Fxm_drop_site_start_update,
       Sxm_drop_site_start_update,
       3, 3, 0, "")
{
}

#endif

/* Motif keeps track of one virtual XmDropSite widget per drop site (there
   is at most one drop site per real widget, and thus that widget serves
   to identify the drop site).  "Virtual" means that Motif does not actually
   create such a widget, but acts like it does and provides virtual
   create/set/get/destroy functions in the form of XmDropSiteRegister(),
   XmDropSiteUpdate(), XmDropSiteRetrieve(), and XmDropSiteUnregister().
   These functions use the real drop site widget to identify the virual
   XmDropSite widget.

   We just pretend that these virtual widgets are real and use the real
   drop site widget as the identifier, placing a mark in the struct
   Lisp_Widget indicating that the widget is virtual.  This causes no
   problems in the widget hash table because the real widget is always
   an EmacsScreen widget and such widgets are never directly visible on
   the E-Lisp level. */

DEFUN ("xm-drop-site-register",
       Fxm_drop_site_register,
       Sxm_drop_site_register,
       2, 2, 0, "Register SCREEN as a drop site for Motif drag-and-drop.\n\
Returns an xm-drop-site widget whose initial resource values are specified\n\
by VALUES, an alist of (NAME . VALUE) resource specifications.")
     (screen, values)
     Lisp_Object screen, values;
{
  Widget w;

  CHECK_SCREEN (screen, 0);
  CHECK_LIST (values, 0);

  w = XSCREEN (screen)->x.display->edit_widget;
  if (!NILP (find_widget (w)))
    signal_error (Qdrag_and_drop_error, list2
		  (build_string
		   (GETTEXT
		    ("Already registered as drop site")),
		   screen));

  values_to_xt_arglist (values, 1, xxxx_class_XmDropSite, access_create);
  BLOCK_INPUT;
  XmDropSiteRegister (w, xtf_arglist, xtf_argcur);
  UNBLOCK_INPUT;
  {
    Lisp_Object obj = make_widget (w, xxxx_class_XmDropSite, Qnil, 1);
    XWIDGET (obj)->destroyable = 0;
    return obj;
  }
}

DEFUN ("xm-drop-site-retrieve",
       Fxm_drop_site_retrieve,
       Sxm_drop_site_retrieve,
       2, 2, 0, "Retrieve the resources of a virtual `xm-drop-site' widget.\n\
This function works like `xt-get-values'.  Widgets of class `xm-drop-site'\n\
do not actually exist, but Motif acts as if there is such a widget\n\
associated with each valid drop site (there is at most one drop site per\n\
Emacs screen).  SCREEN-OR-DC identifies the virtual widget and can either\n\
be an Emacs screen (on the receiver side) or a widget of class\n\
`xm-drag-context' (on the initiator side).  In the latter case, the\n\
resources `drag-proc' and `drop-proc' cannot be accessed.")
     (screen_or_dc, values)
     Lisp_Object screen_or_dc, values;
{
  Widget w;

  if (WIDGETP (screen_or_dc)) {
    check_valid_widget_type (screen_or_dc, xxxx_class_XmDragContext);
    w = XWIDGET (screen_or_dc)->widget;
  /* #### prevent access to drag-proc and drop-proc if drag-context? */
  } else {
    CHECK_SCREEN (screen_or_dc, 0);
    w = XSCREEN (screen_or_dc)->display.x->edit_widget;
  }
  xt_values_to_arglist (values, 0, xxxx_class_XmDropSite, access_get);
  {
    XtArgVal *aux = (XtArgVal *) alloca (xtf_argcur * sizeof (*aux));
    int i;

    for (i=0; i<xtf_argcur; i++)
      xtf_arglist[i].value = (XtArgVal) (aux+i);
    BLOCK_INPUT;
    XmDropSiteRetrieve (w, xtf_arglist, xtf_argcur);
    UNBLOCK_INPUT;
    for (i=0; i<xtf_argcur; i++)
      xtf_arglist[i].value = aux[i];
  }
  return arglist_to_xt_values (xxxx_class_XmDropSite, values);
}

DEFUN ("xm-drop-site-update",
       Fxm_drop_site_update,
       Sxm_drop_site_update,
       2, 2, 0, "Set the resources of a virtual `xm-drop-site' widget.\n\
This function works like `xt-set-values'.  Widgets of class `xm-drop-site'\n\
do not actually exist, but Motif acts as if there is such a widget\n\
associated with each valid drop site (there is at most one drop site per\n\
Emacs screen).  SCREEN-OR-DC identifies the virtual widget and can either\n\
be an Emacs screen (on the receiver side) or a widget of class\n\
`xm-drag-context' (on the initiator side).  In the latter case, the\n\
resources `drag-proc' and `drop-proc' cannot be accessed.")
     (screen_or_dc, values)
     Lisp_Object screen_or_dc, values;
{
  Widget w;

  if (WIDGETP (screen_or_dc)) {
    check_valid_widget_type (screen_or_dc, xxxx_class_XmDragContext);
    w = XWIDGET (screen_or_dc)->widget;
  /* #### prevent access to drag-proc and drop-proc if drag-context? */
  } else {
    CHECK_SCREEN (screen_or_dc, 0);
    w = XSCREEN (screen_or_dc)->display.x->edit_widget;
  }
  xt_values_to_arglist (values, 1, xxxx_class_XmDropSite, access_set);
  BLOCK_INPUT;
  XmDropSiteUpdate (w, xtf_arglist, xtf_argcur);
  UNBLOCK_INPUT;

  return Qnil;
}

DEFUN ("xm-drop-site-unregister",
       Fxm_drop_site_unregister,
       Sxm_drop_site_unregister,
       1, 1, 0, "Unregister SCREEN as a valid drop site for Motif drag-and-drop.")
     (screen)
     Lisp_Object screen;
{
  Widget w;
  Lisp_Object obj;

  CHECK_SCREEN (screen, 0);

  w = XSCREEN (screen)->x.display->edit_widget;
  obj = find_widget (w);
  if (NILP (w))
    signal_error (Qdrag_and_drop_error, list2
		  (build_string
		   (GETTEXT
		    ("Not registered as drop site")),
		   screen));
  BLOCK_INPUT;
  XmDropSiteUnregister (w);
  UNBLOCK_INPUT;
  zero_widget (XWIDGET (obj));

  return Qnil;
}

DEFUN ("xm-drop-transfer-add",
       Fxm_drop_transfer_add,
       Sxm_drop_transfer_add,
       2, 2, 0, "")
     (drop_transfer, transfers)
     Lisp_Object drop_transfer, transfers;
{
  check_valid_widget_type (drop_transfer, xxxx_class_XmDropTransfer);
  CHECK_LIST (transfers, 0);

  /* punt for now */

  return Qnil;
}

DEFUN ("xm-drop-transfer-start",
       Fxm_drop_transfer_start,
       Sxm_drop_transfer_start,
       2, 2, 0, "")
     (drag_context, values)
      Lisp_Object drag_context, values;
     
{
  Widget w;

  CHECK_SCREEN (screen, 0);
  CHECK_LIST (values, 0);

  check_valid_widget_type (drag_context, xxxx_class_XmDragContext);
  values_to_xt_arglist (values, 1, xxxx_class_XmDropTransfer, access_create);
  BLOCK_INPUT;
  w = XmDropTransferStart (drag_context, xtf_arglist,_argcur);
  XtAddCallback (w, XtNdestroyCallback, widget_destroy_callback,
		 (XtPointer) 0);
  UNBLOCK_INPUT;
  return make_widget (w, xxxx_class_XmDropTransfer, Qnil, 0);
}

DEFUN ("xm-get-drag-context",
       Fxm_get_drag_context,
       Sxm_get_drag_context,
       2, 2, 0, "")
     (screen, timestamp)
     Lisp_Object screen, timestamp;
     
{
  Widget w;
  Lisp_Object obj;

  CHECK_SCREEN (screen, 0);
  CHECK_FIXNUM (timestamp, 0);
  BLOCK_INPUT;
  w = XmGetDragContext (XSCREEN (screen)->x.display->edit_widget,
			XINT (timestamp));
  UNBLOCK_INPUT;
  if (!w)
    signal_error (Qdrag_and_drop_error, list3
		  (build_string
		   (GETTEXT
		    ("No such drag context for screen and timestamp")),
		   screen,
		   timestamp));
  obj = find_widget (w);
  if (NILP (obj))
    return make_drag_context (w);
  else
    return obj;
}

DEFUN ("xm-get-xm-display",
       Fxm_get_xm_display,
       Sxm_get_xm_display,
       1, 1, 0, "Return the XmDisplay widget for SCREEN.\n\
There is one XmDisplay widget per physical X \"display\".")
     (screen)
     Lisp_Object screen;
{
  Widget w;
  Lisp_Object obj;

  CHECK_SCREEN (screen, 0);
  BLOCK_INPUT;
  w = XmGetXmDisplay (XtDisplay (XSCREEN (screen)->x.display->edit_widget));
  UNBLOCK_INPUT;
  obj = find_widget (w);
  if (NILP (obj))
    return make_widget (w, xxxx_class_XmDisplay, Qnil, 0);
  else
    return obj;
}

DEFUN ("xm-get-xm-screen",
       Fxm_get_xm_screen,
       Sxm_get_xm_screen,
       1, 1, 0, "Return the XmScreen widget for SCREEN.\n\
There is one XmScreen widget per physical X \"screen\".")
     (screen)
     Lisp_Object screen;
{
  CHECK_SCREEN (screen, 0);
  BLOCK_INPUT;
  w = XmGetXmScreen (XtScreen (XSCREEN (screen)->x.display->edit_widget));
  UNBLOCK_INPUT;
  obj = find_widget (w);
  if (NILP (obj))
    return make_widget (w, xxxx_class_XmScreen, Qnil, 0);
  else
    return obj;
}

DEFUN ("xm-targets-are-compatible",
       Fxm_targets_are_compatible,
       Sxm_targets_are_compatible,,
       3, 3, 0, "")
     (screen, export_targets, import_targets)
     Lisp_Object screen, export_targets, import_targets;
{
  CHECK_SCREEN (screen, 0);
  CHECK_LIST (export_targets, 0);
  CHECK_LIST (import_targets, 0);
  /* punt for now */
  return Qnil;
}

/**********************************************************************/
/*              SPECIAL HANDLERS FOR SPECIFIC WIDGET CLASSES          */
/**********************************************************************/

/* The special handler is called after widget creation and xt-get-values
   and xt-set-values, and handles any special cases. */

static
Lisp_Object find_pixmap (Lisp_Object values, int assq_p)
{
  Lisp_Object obj = (assq_p ? Fassq : Fmemq) (values, Qpixmap);

  return obj;
}


void Special_XmDragIcon (Lisp_Object wid,
			 Lisp_Object *values,
			 xtf_access access)
{
  Lisp_Object obj;
  Widget w = XWIDGET (wid)->widget;

  if (!NILP (obj = find_pixmap (*values, access_get ? 0 : 1))) {
    struct Lisp_Pixmap *p = XPIXMAP (obj);
    
    if (!PIXMAPP (obj))
      abort();

    if (access == access_set || access == access_create) {
      xtf_add_arg ("width", p->width);
      xtf_add_arg ("height", p->height);
      xtf_add_arg ("depth", p->depth);
      xtf_add_arg ("mask", p->mask);
      xtf_add_arg ("hotX", p->x);
      xtf_add_arg ("hotY", p->y);
    } else {
      struct Lisp_Pixmap *p = XPIXMAP (obj);
      BLOCK_INPUT;
      XtVaGetValues (w,
		     "width", &p->width,
		     "height", &p->height,
		     "depth", &p->depth,
		     "mask", &p->mask,
		     "hotX", &p->x,
		     "hotY", &p->y);
      p->mask_delete_p = 0;
      UNBLOCK_INPUT;
    }
  }

}

    
void Special_XmDropTransfer (Lisp_Object wid,
			     Lisp_Object *values,
			     xtf_access access)
{
}

syms_of_dragndrop()
{

  defsubr (&Sxm_drag_start);
  defsubr (&Sxm_drag_cancel);
  defsubr (&Sxm_drop_site_register);
  defsubr (&Sxm_drop_site_unregister);
  defsubr (&Sxm_drop_site_retrieve);
  defsubr (&Sxm_drop_site_update);
  defsubr (&Sxm_drop_transfer_add);
  defsubr (&Sxm_drop_transfer_start);
  defsubr (&Sxm_get_drag_context);
  defsubr (&Sxm_get_xm_display);
  defsubr (&Sxm_get_xm_screen);
  defsubr (&Sxm_targets_are_compatible);

  defsymbol (&Qdrag_and_drop_error, "drag-and-drop-error");

  pure_put (Qdrag_and_drop_error, Qerror_conditions,
	    list2 (Qdrag_and_drop_error, Qerror));
  pure_put (Qdrag_and_drop_error, Qerror_message,
	    build_string (DEFER_GETTEXT ("Drag-and-drop error")));
}

#else /* TEMP_NOT_DEFINED */

syms_of_dragndrop()
{
}

#endif
