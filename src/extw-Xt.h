#ifndef _EXTW_XT_H_
#define _EXTW_XT_H_

#include "extw-Xlib.h"

#ifndef XtCXtToolkitError
#define XtCXtToolkitError "XtToolkitError"
#endif

#ifndef DEFAULT_WM_TIMEOUT
#define DEFAULT_WM_TIMEOUT 5000
#endif

void extw_send_geometry_value(Display *display, Window win, Atom property,
			      en_extw_notify type, XtWidgetGeometry *xwg,
			      long data0);
void extw_get_geometry_value(Display *display, Window win, Atom property,
			     XtWidgetGeometry *xwg);
Bool extw_wait_for_response(Widget w, XEvent *event, unsigned long request_num,
			    en_extw_notify type, unsigned long timeout);


#endif /* _EXTW_XT_H_ */
