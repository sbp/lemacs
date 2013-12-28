/* Common communications code between client and shell widgets */

#ifndef EXTERNAL_WIDGET
ERROR!  This ought not be getting included if EXTERNAL_WIDGET is undefined
#endif

#ifndef XtCXtToolkitError
#define XtCXtToolkitError "XtToolkitError"
#endif

#ifndef DEFAULT_WM_TIMEOUT
#define DEFAULT_WM_TIMEOUT 5000
#endif

#define emacs_shell_send 0
#define emacs_client_send 1

#define send_notify_0(w, type) send_notify_3(w, type, 0, 0, 0)
#define send_notify_1(w, type, data0) send_notify_3(w, type, data0, 0, 0)
#define send_notify_2(w, type, data0, data1) send_notify_3(w, type, data0, data1, 0)

typedef enum {
  emacs_notify_init,
  emacs_notify_end,
  emacs_notify_qg,
  emacs_notify_gm,
  emacs_notify_set_focus,
  emacs_notify_focus_in,
  emacs_notify_focus_out
} en_emacs_notify;

static void send_notify_3(WIDGET_TYPE w, en_emacs_notify type, long data0,
			  long data1, long data2);
static void send_geometry_value(WIDGET_TYPE w, Atom property,
				en_emacs_notify type, XtWidgetGeometry *xwg,
				long data0);
static void get_geometry_value(WIDGET_TYPE w, Atom property,
			       XtWidgetGeometry *xwg);


static int atoms_initialized;
Atom a_EMACS_QUERY_GEOMETRY, a_EMACS_GEOMETRY_MANAGER, a_EMACS_WIDGET_GEOMETRY,
     a_EMACS_NOTIFY;

#ifdef DEBUG_WIDGET

static int geom_masks[] = {
  CWX, CWY, CWWidth, CWHeight, CWBorderWidth, CWSibling, CWStackMode,
  XtCWQueryOnly };
static char *geom_mask_strings[] = {
  "CWX", "CWY", "CWWidth", "CWHeight", "CWBorderWidth",
  "CWSibling", "CWStackMode", "XtCWQueryOnly" };
static int stack_modes[] = {
  Below, TopIf, BottomIf, Opposite, XtSMDontChange };
static char *stack_mode_strings[] = {
  "Below", "TopIf", "BottomIf", "Opposite", "XtSMDontChange" };

static void print_geometry_structure(XtWidgetGeometry *xwg)
{
  int num = sizeof(geom_masks)/sizeof(int);
  int i;
  
  printf ("  masks:");
  for (i=0; i<num; i++)
    if (xwg->request_mode & geom_masks[i])
      printf (" %s", geom_mask_strings[i]);
  printf ("\n");
  printf ("  x:%d y:%d\n", xwg->x, xwg->y);
  printf ("  width:%d height:%d border_width:%d\n", xwg->width,
	  xwg->height, xwg->border_width);
  printf ("  sibling: %x\n", xwg->sibling);
  printf ("  stack_mode: ");
  for (i=0, num=sizeof(stack_modes)/sizeof(int); i<num; i++)
    if (xwg->stack_mode == stack_modes[i]) {
      printf ("%s", stack_mode_strings[i]);
      break;
    }
  printf ("\n");
}

static void print_geometry_result (XtGeometryResult res)
{
  printf ("result: %s\n",
	  res == XtGeometryYes ? "XtGeometryYes" :
	  res == XtGeometryNo ? "XtGeometryNo" :
	  res == XtGeometryAlmost ? "XtGeometryAlmost" :
	  res == XtGeometryDone ? "XtGeometryDone" :
	  "unknown");
}

#endif

static void common_initialize_atoms(Display *display)
{
  if (!atoms_initialized) {
    a_EMACS_QUERY_GEOMETRY =
      XInternAtom(display, "EMACS_QUERY_GEOMETRY", FALSE);
    a_EMACS_GEOMETRY_MANAGER =
      XInternAtom(display, "EMACS_GEOMETRY_MANAGER", FALSE);
    a_EMACS_WIDGET_GEOMETRY =
      XInternAtom(display, "EMACS_WIDGET_GEOMETRY", FALSE);
    a_EMACS_NOTIFY =
      XInternAtom(display, "EMACS_NOTIFY", FALSE);
    atoms_initialized = 1;
  }

}
/* send a notification to the client-side widget. */

static void send_notify_3(WIDGET_TYPE w, en_emacs_notify type, long data0,
			  long data1, long data2)
{
  XClientMessageEvent xev;
  DECL_WIN(w);
  Display *display = XtDisplay((Widget)w);
  
  xev.type = ClientMessage;
  xev.message_type = a_EMACS_NOTIFY;
  xev.format = 32;
  xev.display = display;
  xev.window = win;
  xev.data.l[0] = EMACS_ME;
  xev.data.l[1] = type;
  xev.data.l[2] = data0;
  xev.data.l[3] = data1;
  xev.data.l[4] = data2;

  /* UGGGHHHH!  All I want to do is ensure that the ClientMessage gets
     received.  Unfortunately X doesn't provide any simple way to do
     that but instead has this event_mask bogosity in XSendEvent. */

#if EMACS_ME == emacs_shell_send
  XSendEvent(display, win, False, 0, (XEvent *) &xev);
#else
  XSendEvent(display, win, False, StructureNotifyMask, (XEvent *) &xev);
#endif
#ifdef DEBUG_WIDGET
  if (debug_widget)
    printf("sent notify (%d, %d, %d, %d, %d) to window %x\n",
	   xev.data.l[0], xev.data.l[1], xev.data.l[2], xev.data.l[3],
	   xev.data.l[4], win);
#endif
}

/* put a geometry specification in the specified property on the window
   of the specified widget, and send a notification message to tell the
   client-side widget about this. */

static void send_geometry_value(WIDGET_TYPE w, Atom property,
				en_emacs_notify type, XtWidgetGeometry *xwg,
				long data0)
{
  DECL_WIN(w);

  if (xwg != NULL)
    XChangeProperty(XtDisplay((Widget)w), win, property,
		    a_EMACS_WIDGET_GEOMETRY, 32, PropModeReplace,
		    (unsigned char *) xwg, sizeof(*xwg)/sizeof(int));
  send_notify_1(w, type, data0);
}

/* get the geometry specification stored in the specified property of the
   specified widget's window. */

static void get_geometry_value(WIDGET_TYPE w, Atom property,
			       XtWidgetGeometry *xwg)
{
  Atom dummy;
  int format;
  unsigned long nitems, bytes_after;
  unsigned char *prop;
  DECL_WIN(w);
  
  if (XGetWindowProperty(XtDisplay((Widget)w), win, property, 0,
			 sizeof(*xwg)/4, False, a_EMACS_WIDGET_GEOMETRY,
			 &dummy, &format, &nitems, &bytes_after,
			 &prop) != Success
      || !format || !nitems)
    XtAppErrorMsg(XtWidgetToApplicationContext((Widget)w),
		  "invalidProperty","get_geometry_value",XtCXtToolkitError,
		  "Unable to retrieve property for widget-geometry",
		  (String *)NULL, (Cardinal *)NULL);
  if (format != 8*sizeof(int) || bytes_after) {
    XFree((char *) prop);
    XtAppErrorMsg(XtWidgetToApplicationContext((Widget)w),
		  "invalidProperty","get_geometry_value",XtCXtToolkitError,
		  "Unable to retrieve property for widget-geometry",
		  (String *)NULL, (Cardinal *)NULL);
  }
  *xwg = * (XtWidgetGeometry *) prop;
}

typedef struct {
	Widget w;
	unsigned long request_num;
	en_emacs_notify type;
} QueryStruct;

/* check if an event is of the sort we're looking for */

static Bool isMine(dpy, event, arg)
	Display *dpy;
	register XEvent  *event;
	char *arg;
{
	QueryStruct *q = (QueryStruct *) arg;
	register Widget w = q->w;
	
	if ( (dpy != XtDisplay(w)) || (event->xany.window != XtWindow(w)) ) {
	    return FALSE;
	}
	if (event->xany.serial >= q->request_num) {
	  if (event->type == ClientMessage &&
	      event->xclient.message_type == a_EMACS_NOTIFY &&
	      event->xclient.data.l[0] == EMACS_YOU &&
	      event->xclient.data.l[1] == q->type)
	    return TRUE;
	}
	return FALSE;
}

/* wait for a ClientMessage of the specified type from the other widget, or
   time-out.  isMine() determines whether an event matches.  Culled from
   Shell.c. */

static Bool wait_for_response(w, event, request_num, type)
     WIDGET_TYPE	w;
     XEvent		*event;
     unsigned long	request_num;
     en_emacs_notify    type;
{
	XtAppContext app = XtWidgetToApplicationContext((Widget) w);
	QueryStruct q;
	unsigned long timeout;

	timeout = TIMEOUT(w);

	XFlush(XtDisplay(w));
	q.w = (Widget) w;
	q.request_num = request_num;
	q.type = type;
	
	for(;;) {
 	    /*
 	     * look for match event
 	     */
	    if (XCheckIfEvent( XtDisplay(w), event, isMine, (char*)&q))
	        return TRUE;
	    if (_XtwaitForSomething(TRUE, TRUE, FALSE, TRUE, &timeout, app)
		!= -1) continue;
	    if (timeout == 0)
		return FALSE;
	}
}
