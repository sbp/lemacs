/* Emacs shell widget.
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

/* Completely rewritten by Ben Wing, September 1993. */

/* This is a special Shell that is designed to use an externally-
   provided window created by someone else (possibly another process).
   That other window should have an associated widget of class
   EmacsClient.  The two widgets communicate with each other using
   ClientMessage events and properties on the external window.

   Ideally this feature should be independent of Emacs.  Unfortunately
   there are lots and lots of specifics that need to be dealt with
   for this to work properly, and some of them can't conveniently
   be handled within the widget's methods.  Some day the code may
   be rewritten so that the embedded-widget feature can be used by
   any application, with appropriate entry points that are called
   at specific points within the application.

   This feature is similar to the OLE (Object Linking & Embedding)
   feature provided by MS Windows.
 */

#include "config.h"

#ifndef EXTERNAL_WIDGET
ERROR!  This ought not be getting compiled if EXTERNAL_WIDGET is undefined
#endif

#include <stdio.h>
#include <string.h>
#include <X11/StringDefs.h>
#include "xintrinsicp.h"
#include <X11/Shell.h>
#include <X11/ShellP.h>
#include <X11/Vendor.h>
#include <X11/VendorP.h>
#include "EmacsShellP.h"

/* Communication between this shell and the client widget:

   Communication is through ClientMessage events with message_type
   EMACS_NOTIFY and format 32.  Both the shell and the client widget
   communicate with each other by sending the message to the same
   window (the "external window" below), and the data.l[0] value is
   used to determine who sent the message.

   The data is formatted as follows:

   data.l[0] = who sent this message: emacs_shell_send (0) or
               emacs_client_send (1)
   data.l[1] = message type (see enum en_emacs_notify below)
   data.l[2-4] = data associated with this message

   EventHandler() handles messages from the other side.

   send_notify_3() sends a message to the other side.  Macros
      send_notify_0(), send_notify_1(), and send_notify_2() call
      this function with fewer data arguments.

   send_geometry_value() is used when an XtWidgetGeometry structure
      needs to be sent.  This is too much data to fit into a
      ClientMessage, so the data is stored in a property and then
      send_notify_*() is called.

   get_geometry_value() receives an XtWidgetGeometry structure from a
      property.

   _wait_for_response() is used when a response to a sent message
      is expected.  It looks for a matching event within a
      particular timeout.

   The particular message types are as follows:

1) emacs_notify_init (event_window, event_mask)

   This is sent from the shell to the client after the shell realizes
   its EmacsScreen widget on the client's "external window".  This
   tells the client that it should start passing along events of the
   types specified in event_mask.  event_window specifies the window
   of the EmacsScreen widget, which is a child of the client's
   external window.

2) emacs_notify_end ()

   This is sent from the shell to the client when the shell's
   EmacsScreen widget is destroyed, and tells the client to stop
   passing events along.

3) emacs_notify_qg (result)

   This is sent from the client to the shell when a QueryGeometry
   request is received on the client.  The XtWidgetGeometry structure
   specified in the QueryGeometry request is passed on in the
   EMACS_QUERY_GEOMETRY property (of type EMACS_WIDGET_GEOMETRY) on the
   external window.  result is unused.

   In response, the shell passes the QueryGeometry request down the
   widget tree, and when a response is received, sends a message of
   type emacs_notify_qg back to the client, with result specifying the
   GeometryResult value.  If this value is XtGeometryAlmost, the
   returned XtWidgetGeometry structure is stored into the same property
   as above. [BPW is there a possible race condition here?]

4) emacs_notify_gm (result)

   A very similar procedure to that for emacs_notify_qg is followed
   when the shell's RootGeometryManager method is called, indicating
   that a child widget wishes to change the shell's geometry.  The
   XtWidgetGeometry structure is stored in the EMACS_GEOMETRY_MANAGER
   property.
  
*/

#ifdef DEBUG_WIDGET
extern int debug_widget;
#endif

#define EMACS_ME emacs_shell_send
#define EMACS_YOU emacs_client_send

#define DECL_WIN(w) Window win = (w)->emacsShell.external_window
#define TIMEOUT(w) ((w)->emacsShell.client_timeout)

#define WIDGET_TYPE EmacsShellWidget

#include "commoncom.c"

static void EmacsShellInitialize (Widget req, Widget new, ArgList args,
				  Cardinal *num_args);
static void EmacsShellRealize (Widget wid, Mask *vmask, XSetWindowAttributes
			       *attr);
static void EmacsShellDestroy (Widget w);
static XtGeometryResult EmacsShellRootGeometryManager(Widget gw,
  XtWidgetGeometry *request, XtWidgetGeometry *reply);
static void EventHandler(Widget wid, XtPointer closure, XEvent *event,
			 Boolean *continue_to_dispatch);

#ifndef DEFAULT_WM_TIMEOUT
# define DEFAULT_WM_TIMEOUT 5000
#endif

void EmacsShellUnrealize(Widget w);

static XtResource resources[] = {
#define offset(field) XtOffset(EmacsShellWidget, emacsShell.field)
  {XtNwindow, XtCWindow, XtRWindow, sizeof (Window),
     offset (external_window), XtRImmediate, (XtPointer)0},
  { XtNclientTimeout, XtCClientTimeout, XtRInt, sizeof(int),
      offset(client_timeout), XtRImmediate,(XtPointer)DEFAULT_WM_TIMEOUT},
  { XtNdeadClient, XtCDeadClient, XtRBoolean, sizeof(Boolean),
      offset(dead_client), XtRImmediate, (XtPointer)False},
};


static ShellClassExtensionRec shellClassExtRec = {
    NULL,
    NULLQUARK,
    XtShellExtensionVersion,
    sizeof(ShellClassExtensionRec),
    EmacsShellRootGeometryManager
};

EmacsShellClassRec emacsShellClassRec = {
    { /*
       *	core_class fields
       */
    /* superclass	  */	(WidgetClass) &shellClassRec,
    /* class_name	  */	"EmacsShell",
    /* size		  */	sizeof(EmacsShellRec),
    /* Class Initializer  */	NULL,
    /* class_part_initialize*/	NULL, /* XtInheritClassPartInitialize, */
    /* Class init'ed ?	  */	FALSE,
    /* initialize	  */	EmacsShellInitialize,
    /* initialize_notify  */	NULL,
    /* realize		  */	EmacsShellRealize,
    /* actions		  */	NULL,
    /* num_actions	  */	0,
    /* resources	  */	resources,
    /* resource_count	  */	XtNumber (resources),
    /* xrm_class	  */	NULLQUARK,
    /* compress_motion	  */	FALSE,
    /* compress_exposure  */	TRUE,
    /* compress_enterleave*/	FALSE,
    /* visible_interest	  */	TRUE,
    /* destroy		  */	EmacsShellDestroy, /* XtInheritDestroy, */
    /* resize		  */	XtInheritResize,
    /* expose		  */	NULL,
    /* set_values	  */	NULL, /* XtInheritSetValues, */
    /* set_values_hook	  */	NULL,			
    /* set_values_almost  */	XtInheritSetValuesAlmost,  
    /* get_values_hook	  */	NULL,			
    /* accept_focus	  */	NULL,
    /* intrinsics version */	XtVersion,
    /* callback offsets	  */	NULL,
    /* tm_table		  */	NULL,
    /* query_geometry	  */	NULL,
    /* display_accelerator*/	NULL,
    /* extension	  */	NULL
  },{ /* Composite */
    /* geometry_manager	  */	XtInheritGeometryManager,
    /* change_managed	  */	XtInheritChangeManaged,
    /* insert_child	  */	XtInheritInsertChild,
    /* delete_child	  */	XtInheritDeleteChild,
    /* extension	  */	NULL
  },{ /* Shell */
    /* extension	  */	(XtPointer)&shellClassExtRec
  }
};

WidgetClass emacsShellWidgetClass = (WidgetClass) &emacsShellClassRec;

static void EmacsShellInitialize (Widget req, Widget new, ArgList args,
				  Cardinal *num_args)
{
  XtAddEventHandler(new, NULL,
		    TRUE, EventHandler, (XtPointer) NULL);
  common_initialize_atoms(XtDisplay(req));
}

static Widget find_managed_child(CompositeWidget w)
{
  int i;
  Widget *childP = w->composite.children;

  for (i = w->composite.num_children; i; i--, childP++)
    if (XtIsWidget(*childP) && XtIsManaged(*childP))
      return *childP;
  return NULL;
}

#ifndef XtCXtToolkitError
# define XtCXtToolkitError "XtToolkitError"
#endif

static void EventHandler(wid, closure, event, continue_to_dispatch)
     Widget wid;
     XtPointer closure;	/* unused */
     XEvent *event;
     Boolean *continue_to_dispatch; /* unused */
{
  register EmacsShellWidget w = (EmacsShellWidget) wid;

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
      event->xclient.data.l[0] == EMACS_YOU &&
      event->xclient.message_type == a_EMACS_NOTIFY)
    switch (event->xclient.data.l[1]) {

    case emacs_notify_gm:
      /* client is alive again. */
#ifdef DEBUG_WIDGET
      if (debug_widget)
	printf ("server received emacs_notify_gm\n");
#endif
      w->emacsShell.dead_client = False;
      break;

    case emacs_notify_qg: {
      XtWidgetGeometry xwg, xwg_return;
      XtGeometryResult result;
      Widget child = find_managed_child((CompositeWidget) w);

      if (child) {
	get_geometry_value(w, a_EMACS_QUERY_GEOMETRY, &xwg);
	result = XtQueryGeometry(child, &xwg, &xwg_return);
      } else
	result = XtGeometryYes;
#ifdef DEBUG_WIDGET
      if (debug_widget) {
	printf ("server received emacs_notify_qg\n");
	print_geometry_result (result);
	if (result == XtGeometryAlmost)
	  print_geometry_structure (&xwg_return);
      }
#endif
      send_geometry_value(w, a_EMACS_QUERY_GEOMETRY, emacs_notify_qg,
			  result == XtGeometryAlmost ? &xwg_return :
			  NULL, result);
      break;
    }

    case emacs_notify_focus_in: {
      XFocusChangeEvent event;
      
      event.type = FocusIn;
      event.serial = LastKnownRequestProcessed (XtDisplay (wid));
      event.send_event = True;
      event.display = XtDisplay (wid);
      event.window = XtWindow (wid);
      event.mode = NotifyNormal;
      event.detail = NotifyAncestor;
      /* XtDispatchEvent ((XEvent *) &event); */
      emacs_Xt_focus_event_handler ((XEvent *) &event, 0);
      break;
    }
      
    case emacs_notify_focus_out: {
      XFocusChangeEvent event;
      
      event.type = FocusOut;
      event.serial = LastKnownRequestProcessed (XtDisplay (wid));
      event.send_event = True;
      event.display = XtDisplay (wid);
      event.window = XtWindow (wid);
      event.mode = NotifyNormal;
      event.detail = NotifyAncestor;
      /* XtDispatchEvent ((XEvent *) &event); */
      emacs_Xt_focus_event_handler ((XEvent *) &event, 0);
      break;
    }

    case emacs_notify_end:
      /* screen should be destroyed. */
      break;
    }
}

/* Lifted almost entirely from GetGeometry() in Shell.c
 */
static void GetGeometry(W, child)
    Widget W, child;
{
    register EmacsShellWidget w = (EmacsShellWidget)W;
    int x, y, win_gravity = -1, flag;
    XSizeHints hints;
    DECL_WIN(w);
    
    {
      Window dummy_root;
      unsigned int dummy_bd_width, dummy_depth, width, height;
      
      /* determine the existing size of the window. */
      XGetGeometry(XtDisplay(W), win, &dummy_root, &x, &y, &width,
		   &height, &dummy_bd_width, &dummy_depth);
      w->core.width = width;
      w->core.height = height;
    }

    if(w->shell.geometry != NULL) {
	char def_geom[64];
	int width, height;

	x = w->core.x;
	y = w->core.y;
	width = w->core.width;
	height = w->core.height;
	hints.flags = 0;

	sprintf( def_geom, "%dx%d+%d+%d", width, height, x, y );
	flag = XWMGeometry( XtDisplay(W),
			    XScreenNumberOfScreen(XtScreen(W)),
			    w->shell.geometry, def_geom,
			    (unsigned int)w->core.border_width,
			    &hints, &x, &y, &width, &height,
			    &win_gravity
			   );
	if (flag) {
	    if (flag & XValue) w->core.x = (Position)x;
	    if (flag & YValue) w->core.y = (Position)y;
	    if (flag & WidthValue) w->core.width = (Dimension)width;
	    if (flag & HeightValue) w->core.height = (Dimension)height;
	}
	else {
	    String params[2];
	    Cardinal num_params = 2;
	    params[0] = XtName(W);
	    params[1] = w->shell.geometry;
	    XtAppWarningMsg(XtWidgetToApplicationContext(W),
       "badGeometry", "shellRealize", XtCXtToolkitError,
       "Shell widget \"%s\" has an invalid geometry specification: \"%s\"",
			    params, &num_params);
	}
    }
    else
	flag = 0;

    w->shell.client_specified |= _XtShellGeometryParsed;
}

/* Lifted almost entirely from Realize() in Shell.c
 */
static void EmacsShellRealize (Widget wid, Mask *vmask, XSetWindowAttributes *attr)
{
	EmacsShellWidget w = (EmacsShellWidget) wid;
        Mask mask = *vmask;
	DECL_WIN(w);

	if (!win) {
	  Cardinal count = 1;
	  XtErrorMsg("invalidWindow","shellRealize", XtCXtToolkitError,
		     "No external window specified for EmacsShell widget %s",
		     &wid->core.name, &count);
	}

	if (! (w->shell.client_specified & _XtShellGeometryParsed)) {
	    /* we'll get here only if there was no child the first
	       time we were realized.  If the shell was Unrealized
	       and then re-Realized, we probably don't want to
	       re-evaluate the defaults anyway.
	     */
	    GetGeometry(wid, (Widget)NULL);
	}
	else if (w->core.background_pixmap == XtUnspecifiedPixmap) {
	    /* I attempt to inherit my child's background to avoid screen flash
	     * if there is latency between when I get resized and when my child
	     * is resized.  Background=None is not satisfactory, as I want the
	     * user to get immediate feedback on the new dimensions (most
	     * particularly in the case of a non-reparenting wm).  It is
	     * especially important to have the server clear any old cruft
	     * from the display when I am resized larger.
	     */
	    register Widget *childP = w->composite.children;
	    int i;
	    for (i = w->composite.num_children; i; i--, childP++) {
		if (XtIsWidget(*childP) && XtIsManaged(*childP)) {
		    if ((*childP)->core.background_pixmap
			    != XtUnspecifiedPixmap) {
			mask &= ~(CWBackPixel);
			mask |= CWBackPixmap;
			attr->background_pixmap =
			    w->core.background_pixmap =
				(*childP)->core.background_pixmap;
		    } else {
			attr->background_pixel = 
			    w->core.background_pixel = 
				(*childP)->core.background_pixel;
		    }
		    break;
		}
	    }
	}

	if(w->shell.save_under) {
		mask |= CWSaveUnder;
		attr->save_under = TRUE;
	}
	if(w->shell.override_redirect) {
		mask |= CWOverrideRedirect;
		attr->override_redirect = TRUE;
	}
	if (wid->core.width == 0 || wid->core.height == 0) {
	    Cardinal count = 1;
	    XtErrorMsg("invalidDimension", "shellRealize", XtCXtToolkitError,
		       "Shell widget %s has zero width and/or height",
		       &wid->core.name, &count);
	}
	wid->core.window = win;
#ifdef DEBUG_WIDGET
	if (debug_widget) {
	  printf("event_mask: %x\n", attr->event_mask);
	  printf("select mask: %x\n", mask);
	}
#endif
	XChangeWindowAttributes(XtDisplay(wid), wid->core.window,
				mask, attr);

}

static void EmacsShellDestroy(wid)
	Widget wid;
{
  EmacsShellWidget w = (EmacsShellWidget)wid;

  if (XtIsRealized(wid))
    EmacsShellUnrealize(wid);

  send_notify_0(w, emacs_notify_end);
}

/* Based on RootGeometryManager() in Shell.c */

static XtGeometryResult EmacsShellRootGeometryManager(gw, request, reply)
    Widget gw;
    XtWidgetGeometry *request, *reply;
{
    register EmacsShellWidget w = (EmacsShellWidget)gw;
    unsigned int mask = request->request_mode;
    XEvent event;
    int oldx, oldy, oldwidth, oldheight, oldborder_width;
    unsigned long request_num;
    XtWidgetGeometry req = *request; /* don't modify caller's structure */

#ifdef DEBUG_WIDGET
    if (debug_widget) {
      printf ("server RootGeometryManager called\n");
      print_geometry_structure (request);
    }
#endif
    oldx = w->core.x;
    oldy = w->core.y;
    oldwidth = w->core.width;
    oldheight = w->core.height;
    oldborder_width = w->core.border_width;

#define PutBackGeometry() \
	{ w->core.x = oldx; \
	  w->core.y = oldy; \
	  w->core.width = oldwidth; \
	  w->core.height = oldheight; \
	  w->core.border_width = oldborder_width; }

    if (mask & CWX) {
      if (w->core.x == request->x) mask &= ~CWX;
      else
	w->core.x = request->x;
    }
    if (mask & CWY) {
      if (w->core.y == request->y) mask &= ~CWY;
      else w->core.y = request->y;
    }
    if (mask & CWBorderWidth) {
      if (w->core.border_width == request->border_width)
	      mask &= ~CWBorderWidth;
      else w->core.border_width = request->border_width;
    }
    if (mask & CWWidth) {
      if (w->core.width == request->width) mask &= ~CWWidth;
      else w->core.width = request->width;
    }
    if (mask & CWHeight) {
      if (w->core.height == request->height) mask &= ~CWHeight;
      else w->core.height = request->height;
    }

    if (!XtIsRealized((Widget)w)) return XtGeometryYes;

    req.sibling = None;
    req.request_mode = mask & ~CWSibling;
    request_num = NextRequest(XtDisplay(w));
    send_geometry_value(w, a_EMACS_GEOMETRY_MANAGER, emacs_notify_gm, &req, 0);
#ifdef DEBUG_WIDGET
    if (debug_widget)
      printf ("sent geometry to client\n");
#endif

    if (w->emacsShell.dead_client == TRUE) {
      /* The client is sick.  Refuse the request.
       * If the client recovers and decides to honor the
       * request, it will be handled by Shell's EventHandler().
       */
      PutBackGeometry();
      return XtGeometryNo;
    }

    if (wait_for_response(w, &event, request_num, emacs_notify_gm)) {
      XtGeometryResult result = (XtGeometryResult) event.xclient.data.l[2];

#ifdef DEBUG_WIDGET
      if (debug_widget)
	print_geometry_result (result);
#endif
      if (result != XtGeometryYes)
	PutBackGeometry();
      if (result == XtGeometryAlmost) {
	get_geometry_value(w, a_EMACS_GEOMETRY_MANAGER, reply);
#ifdef DEBUG_WIDGET
	if (debug_widget)
	  print_geometry_structure (reply);
#endif
      }
      return result;
    } else {
#ifdef DEBUG_WIDGET
      if (debug_widget)
	printf ("no reply from client\n");
#endif
      w->emacsShell.dead_client = TRUE; /* timed out; must be broken */
      PutBackGeometry();
      return XtGeometryNo;
    }
#undef PutBackGeometry
}

/* external entry points */

void EmacsShellReady(Widget w, Window win, long event_mask)
{
  EmacsShellWidget ew = (EmacsShellWidget) w;

  send_notify_2(ew, emacs_notify_init, (long) win, event_mask);
}

void EmacsShellSetFocus(Widget wid)
{
  EmacsShellWidget w = (EmacsShellWidget)wid;

  send_notify_0(w, emacs_notify_set_focus);
}

extern void _XtUnregisterWindow (Window, Widget);

void EmacsShellUnrealize(Widget w)
{
  _XtUnregisterWindow(w->core.window, w);
  w->core.window = 0;
}
