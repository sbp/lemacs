/* Emacs client widget.
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

#ifndef EXTERNAL_WIDGET
ERROR!  This ought not be getting compiled if EXTERNAL_WIDGET is undefined
#endif

#include "intl.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <Xm/XmP.h>
#include <Xm/PrimitiveP.h>
#include <X11/keysym.h>
/* 
#include <X11/StringDefs.h>
#include "xintrinsicp.h"

*/
#include "EmacsWidgetP.h"

#ifdef TOOLTALK
#include <tt_c.h>
#endif

#ifdef DEBUG_WIDGET
int debug_widget;
#endif

/* This is the client widget, used to communicate with an EmacsShell
   widget. */

#define EMACS_ME emacs_client_send
#define EMACS_YOU emacs_shell_send

#define DECL_WIN(w) Window win = XtWindow((Widget)(w))
#define TIMEOUT(w) ((w)->emacsClient.shell_timeout)

#define WIDGET_TYPE EmacsClientWidget

#include "commoncom.c"

static void emacsClientInitialize ();
static void emacsClientRealize (Widget widget, XtValueMask *mask,
		    XSetWindowAttributes *attrs);
static void Destroy (Widget w);
static void EventHandler();
static void MaskableEventHandler();
static XtGeometryResult QueryGeometry(Widget, XtWidgetGeometry *,
				      XtWidgetGeometry *);
static void EmacsClientFocusIn (Widget, XEvent *, String *, Cardinal *);
static void EmacsClientFocusOut (Widget, XEvent *, String *, Cardinal *);
static void EmacsClientEnter (Widget, XEvent *, String *, Cardinal *);
static void EmacsClientLeave (Widget, XEvent *, String *, Cardinal *);

static int my_error_handler(Display *display, XErrorEvent *xev);
static int (*error_old_handler)(Display *, XErrorEvent *);

static XtResource resources[] = {
#define offset(field) XtOffset(EmacsClientWidget, emacsClient.field)
  { XtNshellTimeout, XtCShellTimeout, XtRInt, sizeof(int),
      offset(shell_timeout), XtRImmediate,(XtPointer)DEFAULT_WM_TIMEOUT},
  { XtNdeadShell, XtCDeadShell, XtRBoolean, sizeof(Boolean),
      offset(dead_shell), XtRImmediate, (XtPointer)False},
  { XmNnavigationType, XmCNavigationType, XmRNavigationType,
      sizeof(XmNavigationType), XtOffset(EmacsClientWidget,
      primitive.navigation_type), XtRImmediate,
      (XtPointer)XmTAB_GROUP},
  { XtNemacsProcID, XtCEmacsProcID, XtRString, sizeof(String),
      offset(emacs_procid), XtRImmediate, (XtPointer)NULL},
  { XtNshellReadyCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
      offset(shell_ready_callback), XtRImmediate, (XtPointer)NULL},
  { XtNshellName, XtCShellName, XtRString, sizeof(String),
      offset(shell_name), XtRImmediate, (XtPointer)NULL},
  { XtNuseToolTalk, XtCUseToolTalk, XtRBoolean, sizeof(Boolean),
      offset(use_tooltalk), XtRImmediate, (XtPointer)False}
};

static XtActionsRec actions[] = {
  {"focusIn",	EmacsClientFocusIn},
  {"focusOut",	EmacsClientFocusOut},
  {"enter",	EmacsClientEnter},
  {"leave",	EmacsClientLeave},
};

EmacsClientClassRec emacsClientClassRec = {
    { /*
       *	core_class fields
       */
    /* superclass	  */	(WidgetClass) &xmPrimitiveClassRec,
    /* class_name	  */	"EmacsClient",
    /* size		  */	sizeof(EmacsClientRec),
    /* Class Initializer  */	NULL,
    /* class_part_initialize*/	NULL, /* XtInheritClassPartInitialize, */
    /* Class init'ed ?	  */	FALSE,
    /* initialize	  */	emacsClientInitialize,
    /* initialize_notify  */	NULL,
    /* realize		  */	emacsClientRealize, 
    /* actions		  */	actions,
    /* num_actions	  */	XtNumber (actions),
    /* resources	  */	resources,
    /* resource_count	  */	XtNumber (resources),
    /* xrm_class	  */	NULLQUARK,
    /* compress_motion	  */	FALSE,
    /* compress_exposure  */	TRUE,
    /* compress_enterleave*/	FALSE,
    /* visible_interest	  */	TRUE,
    /* destroy		  */	Destroy, /* XtInheritDestroy, */
    /* resize		  */	XtInheritResize,
    /* expose		  */	NULL,
    /* set_values	  */	NULL, /* XtInheritSetValues, */
    /* set_values_hook	  */	NULL,			
    /* set_values_almost  */	XtInheritSetValuesAlmost,  
    /* get_values_hook	  */	NULL,			
    /* accept_focus	  */	NULL,
    /* intrinsics version */	XtVersion,
    /* callback offsets	  */	NULL,
    /* tm_table		  */	"", /* MUST NOT BE NULL or
                                       XtInheritTranslations in Motif!!!!!
				       Otherwise keyboard focus translations
				       will not work. */
    /* query_geometry	  */	QueryGeometry,
    /* display_accelerator*/	NULL,
    /* extension	  */	NULL
  },
  {
    _XtInherit,              /* Primitive border_highlight */
    _XtInherit,              /* Primitive border_unhighlight */
    XtInheritTranslations,   /* translations */
    NULL,                    /* arm_and_activate */
    NULL,                    /* get resources */
    0,                       /* num get_resources */
    NULL,                    /* extension */
  },
  {
    0
  }
};

WidgetClass emacsClientWidgetClass = (WidgetClass) &emacsClientClassRec;

static void emacsClientInitialize (req, new, args, num_args)
     Widget req, new;
     ArgList args;
     Cardinal *num_args;
{
  EmacsClientWidget ecw = (EmacsClientWidget) new;
  static int error_handler_added = 0;

#ifdef DEBUG_WIDGET
  {
    char *val = getenv ("ERA_DEBUG_WIDGET");
    if (val) {
      debug_widget = atoi (val);
      if (!debug_widget)
	debug_widget = -1;
    }
  }
#endif

  /* yes I know this is horrible.  However, the XmPrimitive class adds
     the Tab translation in its initialization routine, so we have to
     override it here.  This is all the fault of Xt, which doesn't
     provide a proper inheritance mechanism for translations.

     -- BPW

  */
    
  XtOverrideTranslations(new, XtParseTranslationTable("None<Key>Tab:\n"
						      "<FocusIn>:focusIn()\n"
						      "<FocusOut>:focusOut()\n"
						      "<Enter>:enter()\n"
						      "<Leave>:leave()\n"));
  

  common_initialize_atoms(XtDisplay(req));
  XtAddEventHandler(new, NULL, TRUE, EventHandler, (XtPointer) NULL);
#ifdef DEBUG_WIDGET
  if (debug_widget)
    printf("event handler added for window %d\n", XtWindow(new));
#endif
  ecw->emacsClient.shell_ready = False;
  ecw->emacsClient.has_focus = False;

  if (!error_handler_added) {
    error_handler_added = 1;
    error_old_handler = XSetErrorHandler(my_error_handler);
#ifdef DEBUG_WIDGET
    if (debug_widget)
      printf("adding error handler\n");
#endif
  }

#if 0
  XtAddEventHandler(new, KeyPressMask, FALSE, MaskableEventHandler,
		    (XtPointer) NULL);
#endif
}


#ifdef TOOLTALK
static Tt_callback_action
tt_callback(Tt_message m, Tt_pattern p)
{
  EmacsClientWidget ecw = (EmacsClientWidget)tt_message_user(m, 0);
  
  switch (tt_message_state(m)) {
  case TT_FAILED:
    /* handle errors here */
    break;
  case TT_HANDLED:
    ecw->emacsClient.shell_name = tt_message_arg_val(m, 2);
    XtCallCallbackList((Widget)ecw, ecw->emacsClient.shell_ready_callback, NULL);
    break;
  }
  
  tt_message_destroy(m);
  return TT_CALLBACK_PROCESSED;
}
#endif


static void emacsClientRealize(Widget w, XtValueMask *vm, XSetWindowAttributes *attrs)
{

  EmacsClientWidget ecw = (EmacsClientWidget)w;
  
  (*xmPrimitiveWidgetClass->core_class.realize)(w, vm, attrs);

#ifdef TOOLTALK
  if (ecw->emacsClient.use_tooltalk)  {
    Tt_message m = tt_message_create();

    tt_message_op_set(m, "emacs-make-client-screen");
    tt_message_scope_set(m, TT_SESSION);
    tt_message_class_set(m, TT_REQUEST);
    tt_message_arg_add(m, TT_IN, "string", XtName(w));
    tt_message_iarg_add(m, TT_IN, "int", XtWindow(w));
    tt_message_arg_add(m, TT_OUT, "string", NULL); 
    tt_message_user_set(m, 0, (void *)w);
    tt_message_callback_add(m, tt_callback);
    if (ecw->emacsClient.emacs_procid) {
      tt_message_address_set(m, TT_HANDLER);
      tt_message_handler_set(m, ecw->emacsClient.emacs_procid);
    }
    else
      tt_message_address_set(m, TT_PROCEDURE);
    tt_message_send(m);
  }
#endif  
}


/***********************************************************************/

/* window-to-widget list. */

struct ww_list {
  Window win;
  Widget wid;
  struct ww_list *next;
};

struct ww_list ww_list[1];

static int add_ww(Window win, Widget wid)
{
  struct ww_list *ww = (struct ww_list *) malloc(sizeof(struct
							ww_list));
  if (!ww)
    return 0;
  ww->win = win;
  ww->wid = wid;
  ww->next = ww_list->next;
  ww_list->next = ww;
  return 1;
}

static Widget remove_ww(Window win)
{
  struct ww_list *w1, *w2;
  Widget wid = 0;
  
  for (w1=ww_list, w2=w1->next; w2; w1=w2, w2=w2->next)
    if (w2->win == win) {
      w1->next = w2->next;
      wid = w2->wid;
      free(w2);
      break;
    }
  return wid;
}

/***********************************************************************/

/* stolen outright from Intrinsic.c */

static void ComputeWindowAttributes(widget,value_mask,values)
     Widget		 widget;
     XtValueMask		 *value_mask;
     XSetWindowAttributes *values;
{
  *value_mask = CWEventMask | CWColormap;
  (*values).event_mask = XtBuildEventMask(widget);
  (*values).colormap = widget->core.colormap;
  if (widget->core.background_pixmap != XtUnspecifiedPixmap) {
    *value_mask |= CWBackPixmap;
    (*values).background_pixmap = widget->core.background_pixmap;
  } else {
    *value_mask |= CWBackPixel;
    (*values).background_pixel = widget->core.background_pixel;
  }
  if (widget->core.border_pixmap != XtUnspecifiedPixmap) {
    *value_mask |= CWBorderPixmap;
    (*values).border_pixmap = widget->core.border_pixmap;
  } else {
    *value_mask |= CWBorderPixel;
    (*values).border_pixel = widget->core.border_pixel;
  }
  if (widget->core.widget_class->core_class.expose == (XtExposeProc) NULL) {
    /* Try to avoid redisplay upon resize by making bit_gravity the same
       as the default win_gravity */
    *value_mask |= CWBitGravity;
    (*values).bit_gravity = NorthWestGravity;
  }
} /* ComputeWindowAttributes */

static void end_connection(EmacsClientWidget w)
{
  XSetWindowAttributes xswa;
  XtValueMask mask;
  Widget wid = (Widget) w;
  
  w->emacsClient.shell_ready = False;
  XtRemoveEventHandler(wid, w->emacsClient.event_mask,
		       FALSE, MaskableEventHandler, (XtPointer) NULL);
  ComputeWindowAttributes(wid, &mask, &xswa);
  XChangeWindowAttributes(XtDisplay(wid), XtWindow(wid), mask, &xswa);
  XClearArea(XtDisplay(wid), XtWindow(wid), 0, 0, 0, 0, True);
}

static int error_occurred;

static int my_error_handler(Display *display, XErrorEvent *xev)
{
  Widget wid;
  
#ifdef DEBUG_WIDGET
  if (debug_widget)
    printf("my error handler called\n");
#endif
  
  if (xev->error_code != BadWindow)
    goto call_old;
#ifdef DEBUG_WIDGET
  if (debug_widget)
    printf("bad window, window = %d\n", xev->resourceid);
#endif
  wid = remove_ww(xev->resourceid);
  if (wid) {
    end_connection((EmacsClientWidget) wid);
#ifdef DEBUG_WIDGET
    if (debug_widget)
      printf("removing handler for window %d\n", xev->resourceid);
#endif
    return 0;
  }
  
 call_old:
#ifdef DEBUG_WIDGET
  if (debug_widget)
    printf("calling old\n");
#endif
  return error_old_handler(display, xev);
}

static void MaskableEventHandler(wid, closure, event, continue_to_dispatch)
     Widget wid;
     XtPointer closure;	/* unused */
     XEvent *event;
     Boolean *continue_to_dispatch; /* unused */
{
  register EmacsClientWidget w = (EmacsClientWidget) wid;
  
#ifdef DEBUG_WIDGET
  if (debug_widget)
    if(w->core.window != event->xany.window)
      printf("wrong window!\n");
#endif
  
#if 0
  {
    Display *display = XtDisplay(wid);
    XGCValues values;
    char *string;
    GC gc;
    
    values.foreground = w->primitive.foreground;
    gc = XtGetGC(wid, GCForeground, &values);
    string = XKeysymToString(XKeycodeToKeysym(display,
					      event->xkey.keycode, 0));
    XClearArea(display, XtWindow(wid), 0, 0, 100, 100, True);
    XDrawString(display, XtWindow(wid), gc, 50, 50, string,
		strlen(string));
  }
#endif
  
  if (w->emacsClient.shell_ready) {
#ifdef DEBUG_WIDGET
    if (debug_widget)
      printf("sending event of type %d to window %d\n", event->type,
	     w->emacsClient.event_window);
#endif
    if (event->type == KeyPress || event->type == KeyRelease ||
	event->type == ButtonPress || event->type == ButtonRelease ||
	event->type == MotionNotify)
      event->xkey.subwindow = 0;
    /* hackkkkkkkkkkkkkk!  Suppress CTRL-TAB, SHIFT-TAB, etc. so that
       Emacs doesn't attempt to interpret focus-change keystrokes. */
    if (event->type == KeyPress && XLookupKeysym (event, 0) == XK_Tab &&
	(event->xkey.state & ControlMask ||
	 event->xkey.state & ShiftMask))
      return;
    {
      event->xany.window = w->core.window;
      XSendEvent(XtDisplay(wid), w->emacsClient.event_window, FALSE, 0, event);
      XSync(XtDisplay(wid), 0); /* make sure that any BadWindow errors
				   (meaning the server died) get handled
				   before XSendEvent is called again. */
      
    }
  }
}

static void EventHandler(wid, closure, event, continue_to_dispatch)
     Widget wid;
     XtPointer closure;	/* unused */
     XEvent *event;
     Boolean *continue_to_dispatch; /* unused */
{
  register EmacsClientWidget w = (EmacsClientWidget) wid;
  
#ifdef DEBUG_WIDGET
  if (debug_widget)
    printf("received event\n");
#endif
  
  if(w->core.window != event->xany.window) {
    XtAppErrorMsg(XtWidgetToApplicationContext(wid),
		  "invalidWindow","eventHandler",XtCXtToolkitError,
		  "Event with wrong window",
		  (String *)NULL, (Cardinal *)NULL);
    return;
  }
  
#ifdef DEBUG_WIDGET
  if (debug_widget)
    if (event->type == ClientMessage)
      printf("received notify (%d, %d, %d, %d, %d) on window %x\n",
	     event->xclient.data.l[0], event->xclient.data.l[1],
	     event->xclient.data.l[2], event->xclient.data.l[3],
	     event->xclient.data.l[4], event->xany.window);
#endif
  
  if (event->type == ClientMessage &&
      event->xclient.message_type == a_EMACS_NOTIFY &&
      event->xclient.data.l[0] == EMACS_YOU)
    switch (event->xclient.data.l[1]) {
      
    case emacs_notify_qg:
      /* shell is alive again. */
#ifdef DEBUG_WIDGET
      if (debug_widget)
	printf ("client received emacs_notify_qg\n");
#endif
      w->emacsClient.dead_shell = False;
      break;
      
    case emacs_notify_gm: {
      XtWidgetGeometry xwg, xwg_return;
      XtGeometryResult result;
      
      get_geometry_value(w, a_EMACS_GEOMETRY_MANAGER, &xwg);
      result = XtMakeGeometryRequest(wid, &xwg, &xwg_return);
#ifdef DEBUG_WIDGET
      if (debug_widget) {
	printf ("client received emacs_notify_gm\n");
	print_geometry_result (result);
	if (result == XtGeometryAlmost)
	  print_geometry_structure (&xwg_return);
      }
#endif
      send_geometry_value(w, a_EMACS_GEOMETRY_MANAGER, emacs_notify_gm,
			  result == XtGeometryAlmost ? &xwg_return :
			  NULL, result);
      break;
    }
      
    case emacs_notify_init:
      w->emacsClient.shell_ready = True;
      w->emacsClient.event_window = event->xclient.data.l[2];
      w->emacsClient.event_mask = event->xclient.data.l[3];
      add_ww(w->emacsClient.event_window, (Widget) w);
#ifdef DEBUG_WIDGET
      if (debug_widget)
	printf("adding maskable event handler\n");
#endif
      XtAddEventHandler(wid, w->emacsClient.event_mask,
			FALSE, MaskableEventHandler, (XtPointer) NULL);
      break;
      
    case emacs_notify_end:
      end_connection(w);
      remove_ww(w->emacsClient.event_window);
      break;
      
    case emacs_notify_set_focus:
      XmProcessTraversal(wid, XmTRAVERSE_CURRENT);
      break;
      
    }
}

static void Destroy(wid)
     Widget wid;
{
  EmacsClientWidget w = (EmacsClientWidget)wid;
  
  send_notify_0(w, emacs_notify_end);
}

static XtGeometryResult QueryGeometry(gw, request, reply)
     Widget gw;
     XtWidgetGeometry *request, *reply;
{
  register EmacsClientWidget w = (EmacsClientWidget)gw;
  XEvent event;
  unsigned long request_num;
  Display *display = XtDisplay(gw);
  XtWidgetGeometry req = *request; /* don't modify caller's structure */
  
#ifdef DEBUG_WIDGET
  if (debug_widget) {
    printf ("client's query geometry called\n");
    print_geometry_structure (request);
  }
#endif
  
  if (!XtIsRealized((Widget)w) || !w->emacsClient.shell_ready)
    return XtGeometryYes;
  
  if (w->emacsClient.dead_shell == TRUE)
    /* The shell is sick. */
    return XtGeometryNo;
  
  req.sibling = None;
  req.request_mode &= ~CWSibling;
  request_num = NextRequest(display);
  send_geometry_value(w, a_EMACS_QUERY_GEOMETRY, emacs_notify_qg, &req, 0);
  
#ifdef DEBUG_WIDGET
  if (debug_widget)
    printf ("sent geometry to server\n");
#endif
  if (wait_for_response(w, &event, request_num, emacs_notify_qg)) {
    XtGeometryResult result = (XtGeometryResult) event.xclient.data.l[0];
#ifdef DEBUG_WIDGET
    if (debug_widget)
      print_geometry_result (result);
#endif
    if (result == XtGeometryAlmost) {
      get_geometry_value(w, a_EMACS_QUERY_GEOMETRY, reply);
#ifdef DEBUG_WIDGET
      if (debug_widget)
	print_geometry_structure (reply);
#endif
    }
    return result;
  } else {
#ifdef DEBUG_WIDGET
    if (debug_widget)
      printf ("no reply from server\n");
#endif
    w->emacsClient.dead_shell = TRUE; /* timed out; must be broken */
    return XtGeometryNo;
  }
}

static void EmacsClientFocusIn (Widget w, XEvent *event, String *params,
				Cardinal *num_params)
{
  EmacsClientWidget ecw = (EmacsClientWidget) w;
  
  if (event->xfocus.send_event && !ecw->emacsClient.has_focus) {
    ecw->emacsClient.has_focus = True;
    send_notify_0 (ecw, emacs_notify_focus_in);
  }
  _XmPrimitiveFocusIn (w, event, params, num_params);
}

static void EmacsClientFocusOut (Widget w, XEvent *event, String *params,
				 Cardinal *num_params)
{
  EmacsClientWidget ecw = (EmacsClientWidget) w;
  
  if (event->xfocus.send_event && ecw->emacsClient.has_focus) {
    ecw->emacsClient.has_focus = False;
    send_notify_0 (ecw, emacs_notify_focus_out);
  }
  _XmPrimitiveFocusOut (w, event, params, num_params);
}

static void EmacsClientEnter (Widget w, XEvent *event, String *params,
			      Cardinal *num_params)
{
  EmacsClientWidget ecw = (EmacsClientWidget) w;
  
  if (_XmGetFocusPolicy (w) != XmEXPLICIT && !ecw->emacsClient.has_focus &&
      event->xcrossing.focus && event->xcrossing.detail != NotifyInferior) {
    ecw->emacsClient.has_focus = True;
    send_notify_0 (ecw, emacs_notify_focus_in);
  }
  _XmPrimitiveEnter (w, event, params, num_params);
}

static void EmacsClientLeave (Widget w, XEvent *event, String *params,
			      Cardinal *num_params)
{
  EmacsClientWidget ecw = (EmacsClientWidget) w;
  
  if (_XmGetFocusPolicy (w) != XmEXPLICIT && ecw->emacsClient.has_focus &&
      event->xcrossing.focus && event->xcrossing.detail != NotifyInferior) {
    ecw->emacsClient.has_focus = False;
    send_notify_0 (ecw, emacs_notify_focus_out);
  }
  _XmPrimitiveLeave (w, event, params, num_params);
}

