
#include <stdio.h>
#include <X11/StringDefs.h>
#include <X11/IntrinsicP.h>
#include <X11/Shell.h>
#include <X11/ShellP.h>
#include <X11/Vendor.h>
#include <X11/VendorP.h>
#include "EmacsShellP.h"

#ifndef XtCXtToolkitError
#define XtCXtToolkitError "XtToolkitError"
#endif

static void emacsShellInitialize ();
static void emacsShellRealize ();
static void EvaluateWMHints ();
static void ComputeWMSizeHints();
static void ComputeWMSizeHints ();
static void Destroy (Widget w);
#define BIGSIZE ((Dimension)32767)  /* from Shell.c */

static XtResource resources[] = {
#define offset(field) XtOffset(EmacsShellWidget, emacsShell.field)
  {XtNwindow, XtCWindow, XtRString, sizeof (char*),
     offset (external_window), XtRImmediate, (XtPointer)0},
};

EmacsShellClassRec emacsShellClassRec = {
    { /*
       *	core_class fields
       */
    /* superclass	  */	(WidgetClass) &applicationShellClassRec,
    /* class_name	  */	"EmacsShell",
    /* size		  */	sizeof(EmacsShellRec),
    /* Class Initializer  */	NULL,
    /* class_part_initialize*/	NULL, /* XtInheritClassPartInitialize, */
    /* Class init'ed ?	  */	FALSE,
    /* initialize	  */	emacsShellInitialize,
    /* initialize_notify  */	NULL,
    /* realize		  */	emacsShellRealize,
    /* actions		  */	NULL,
    /* num_actions	  */	0,
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
    /* extension	  */	NULL /* (XtPointer)&shellClassExtRec */
  }
};

WidgetClass emacsShellWidgetClass = (WidgetClass) &emacsShellClassRec;

static void emacsShellInitialize (req, new, args, num_args)
     Widget req, new;
     ArgList args;
     Cardinal *num_args;
{
  EmacsShellWidget w = (EmacsShellWidget) new;
/*
  XtAddEventHandler(new, NULL,
		    TRUE, emacs_Xt_event_handler, (XtPointer) NULL);
 */
}

/* Lifted without alteration from Shell.c
 */
static void EvaluateSizeHints(w)
    WMShellWidget w;
{
	struct _OldXSizeHints *sizep = &w->wm.size_hints;

	sizep->x = w->core.x;
	sizep->y = w->core.y;
	sizep->width = w->core.width;
	sizep->height = w->core.height;

	if (sizep->flags & USSize) {
	    if (sizep->flags & PSize) sizep->flags &= ~PSize;
	} else
	    sizep->flags |= PSize;

	if (sizep->flags & USPosition) {
	    if (sizep->flags & PPosition) sizep->flags &= ~PPosition;
	} else if (w->shell.client_specified & _XtShellPPositionOK)
	    sizep->flags |= PPosition;

	if (sizep->min_aspect.x != XtUnspecifiedShellInt
	    || sizep->min_aspect.y != XtUnspecifiedShellInt
	    || sizep->max_aspect.x != XtUnspecifiedShellInt
	    || sizep->max_aspect.y != XtUnspecifiedShellInt) {
	    sizep->flags |= PAspect;
	}
	if(w->wm.base_width != XtUnspecifiedShellInt
	   || w->wm.base_height != XtUnspecifiedShellInt) {
	    sizep->flags |= PBaseSize;
	    if (w->wm.base_width == XtUnspecifiedShellInt)
		w->wm.base_width = 0;
	    if (w->wm.base_height == XtUnspecifiedShellInt)
		w->wm.base_height = 0;
	}
	if (sizep->width_inc != XtUnspecifiedShellInt
	    || sizep->height_inc != XtUnspecifiedShellInt) {
	    if (sizep->width_inc < 1) sizep->width_inc = 1;
	    if (sizep->height_inc < 1) sizep->height_inc = 1;
	    sizep->flags |= PResizeInc;
	}
	if (sizep->max_width != XtUnspecifiedShellInt
	    || sizep->max_height != XtUnspecifiedShellInt) {
	    sizep->flags |= PMaxSize;
	    if (sizep->max_width == XtUnspecifiedShellInt)
		sizep->max_width = BIGSIZE;
	    if (sizep->max_height == XtUnspecifiedShellInt)
		sizep->max_height = BIGSIZE;
	}
	if(sizep->min_width != XtUnspecifiedShellInt
	   || sizep->min_height != XtUnspecifiedShellInt) {
	    sizep->flags |= PMinSize;
	    if (sizep->min_width == XtUnspecifiedShellInt)
		sizep->min_width = 1;
	    if (sizep->min_height == XtUnspecifiedShellInt)
		sizep->min_height = 1;
	}
}


/* Lifted without alteration from Shell.c
 */
static void GetGeometry(W, child)
    Widget W, child;
{
    register ShellWidget w = (ShellWidget)W;
    Boolean is_wmshell = XtIsWMShell(W);
    int x, y, width, height, win_gravity = -1, flag;
    XSizeHints hints;

    if (child != NULL) {
	/* we default to our child's size */
	if (is_wmshell && (w->core.width == 0 || w->core.height == 0))
	    ((WMShellWidget)W)->wm.size_hints.flags |= PSize;
	if (w->core.width == 0)	    w->core.width = child->core.width;
	if (w->core.height == 0)    w->core.height = child->core.height;
    }
    if(w->shell.geometry != NULL) {
	char def_geom[64];
	x = w->core.x;
	y = w->core.y;
	width = w->core.width;
	height = w->core.height;
	if (is_wmshell) {
	    WMShellPart* wm = &((WMShellWidget)w)->wm;
	    EvaluateSizeHints((WMShellWidget)w);
	    bcopy((char*)&wm->size_hints, (char*)&hints,
		  sizeof(struct _OldXSizeHints));
	    hints.win_gravity = wm->win_gravity;
	    if (wm->size_hints.flags & PBaseSize) {
		width -= wm->base_width;
		height -= wm->base_height;
		hints.base_width = wm->base_width;
		hints.base_height = wm->base_height;
	    }
	    else if (wm->size_hints.flags & PMinSize) {
		width -= wm->size_hints.min_width;
		height -= wm->size_hints.min_height;
	    }
	    if (wm->size_hints.flags & PResizeInc) {
		width /= wm->size_hints.width_inc;
		height /= wm->size_hints.height_inc;
	    }
	}
	else hints.flags = 0;

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

    if (is_wmshell) {
	WMShellWidget wmshell = (WMShellWidget) w;
	if (wmshell->wm.win_gravity == XtUnspecifiedShellInt) {
	    if (win_gravity != -1)
		wmshell->wm.win_gravity = win_gravity;
	    else
		wmshell->wm.win_gravity = NorthWestGravity;
	}
	wmshell->wm.size_hints.flags |= PWinGravity;
	if ((flag & (XValue|YValue)) == (XValue|YValue))
	    wmshell->wm.size_hints.flags |= USPosition;
	if ((flag & (WidthValue|HeightValue)) == (WidthValue|HeightValue))
	    wmshell->wm.size_hints.flags |= USSize;
    }
    w->shell.client_specified |= _XtShellGeometryParsed;
}


/* Lifted almost entirely from Realize() in Shell.c
 */
static void _popup_set_prop();
static void emacsShellRealize (wid, vmask, attr)
	Widget wid;
	Mask *vmask;
	XSetWindowAttributes *attr;
{
	EmacsShellWidget w = (EmacsShellWidget) wid;
        Mask mask = *vmask;

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

	if (w->emacsShell.external_window != 0) {
	   wid->core.window = w->emacsShell.external_window;
	   XChangeWindowAttributes(XtDisplay(wid), wid->core.window,
				   mask, attr);
       } else {
	  wid->core.window = XCreateWindow(XtDisplay(wid),
	        wid->core.screen->root, (int)wid->core.x, (int)wid->core.y,
		(unsigned int)wid->core.width, (unsigned int)wid->core.height,
		(unsigned int)wid->core.border_width, (int) wid->core.depth,
		(unsigned int) InputOutput, w->shell.visual,
		mask, attr);
       }
	_popup_set_prop(w);
}


/* Lifted without alteration from Shell.c
 */
static void EvaluateWMHints(w)
    WMShellWidget w;
{
	XWMHints *hintp = &w->wm.wm_hints;

	hintp->flags = StateHint | InputHint;

	if (XtIsTopLevelShell((Widget)w)
	    && ((TopLevelShellWidget)w)->topLevel.iconic) {
	    hintp->initial_state = IconicState;
	}
	if (hintp->icon_x == XtUnspecifiedShellInt)
	    hintp->icon_x = -1;
	else
	    hintp->flags |= IconPositionHint;

	if (hintp->icon_y == XtUnspecifiedShellInt)
	    hintp->icon_y = -1;
	else
	    hintp->flags |= IconPositionHint;

	if (hintp->icon_pixmap) hintp->flags |= IconPixmapHint;
	if (hintp->icon_mask)   hintp->flags |= IconMaskHint;
	if (hintp->icon_window) hintp->flags |= IconWindowHint;

	if (hintp->window_group == XtUnspecifiedWindow) {
	    if(w->core.parent) {
		Widget p;
		for (p = w->core.parent; p->core.parent; p = p->core.parent);
		if (XtIsRealized(p)) {
		    hintp->window_group = XtWindow(p);
		    hintp->flags |=  WindowGroupHint;
		}
	    }
	} else if (hintp->window_group != XtUnspecifiedWindowGroup)
	    hintp->flags |=  WindowGroupHint;
}


/* Lifted without alteration from Shell.c
 */
static void _popup_set_prop(w)
	ShellWidget w;
{
	Widget p;
	WMShellWidget wmshell = (WMShellWidget) w;
	TopLevelShellWidget tlshell = (TopLevelShellWidget) w;
	ApplicationShellWidget appshell = (ApplicationShellWidget) w;
	XTextProperty icon_name;
	XTextProperty window_name;
	char **argv;
	int argc;
	XSizeHints *size_hints;
	Window window_group;
	XClassHint classhint;

	if (!XtIsWMShell((Widget)w) || w->shell.override_redirect) return;

	if ((size_hints = XAllocSizeHints()) == NULL)
	    _XtAllocError("XAllocSizeHints");

	window_name.value = (unsigned char*)wmshell->wm.title;
	window_name.encoding = wmshell->wm.title_encoding;
	window_name.format = 8;
	window_name.nitems = strlen((char *) window_name.value) + 1;

	if (XtIsTopLevelShell((Widget)w)) {
	    icon_name.value = (unsigned char*)tlshell->topLevel.icon_name;
	    icon_name.encoding = tlshell->topLevel.icon_name_encoding;
	    icon_name.format = 8;
	    icon_name.nitems = strlen((char *) icon_name.value) + 1;
	}

	EvaluateWMHints(wmshell);
	EvaluateSizeHints(wmshell);
	ComputeWMSizeHints(wmshell, size_hints);

	if (wmshell->wm.transient
	    && !XtIsTransientShell((Widget)w)
	    && (window_group = wmshell->wm.wm_hints.window_group)
	       != XtUnspecifiedWindowGroup) {

	    XSetTransientForHint(XtDisplay((Widget)w),
				 XtWindow((Widget)w),
				 window_group
				 );
	}

	classhint.res_name = w->core.name;
	/* For the class, look up to the top of the tree */
	for (p = (Widget)w; p->core.parent != NULL; p = p->core.parent);
	if (XtIsApplicationShell(p)) {
	    classhint.res_class =
		((ApplicationShellWidget)p)->application.class;
	} else classhint.res_class = XtClass(p)->core_class.class_name;

	if (XtIsApplicationShell((Widget)w)
	    && (argc = appshell->application.argc) != -1)
	    argv = (char**)appshell->application.argv;
	else {
	    argv = NULL;
	    argc = 0;
	}

	XSetWMProperties(XtDisplay((Widget)w), XtWindow((Widget)w),
			 &window_name,
			 (XtIsTopLevelShell((Widget)w)) ? &icon_name : NULL,
			 argv, argc,
			 size_hints,
			 &wmshell->wm.wm_hints,
			 &classhint);
	XFree((char*)size_hints);
}

/* Lifted without alteration from Shell.c
 */
static void ComputeWMSizeHints(w, hints)
    WMShellWidget w;
    XSizeHints *hints;
{
    register long flags;
    hints->flags = flags = w->wm.size_hints.flags;
#define copy(field) hints->field = w->wm.size_hints.field
    if (flags & (USPosition | PPosition)) {
	copy(x);
	copy(y);
    }
    if (flags & (USSize | PSize)) {
	copy(width);
	copy(height);
    }
    if (flags & PMinSize) {
	copy(min_width);
	copy(min_height);
    }
    if (flags & PMaxSize) {
	copy(max_width);
	copy(max_height);
    }
    if (flags & PResizeInc) {
	copy(width_inc);
	copy(height_inc);
    }
    if (flags & PAspect) {
	copy(min_aspect.x);
	copy(min_aspect.y);
	copy(max_aspect.x);
	copy(max_aspect.y);
    }
#undef copy
#define copy(field) hints->field = w->wm.field
    if (flags & PBaseSize) {
	copy(base_width);
	copy(base_height);
    }
    if (flags & PWinGravity)
	copy(win_gravity);
#undef copy
}

static void Destroy(wid)
	Widget wid;
{
  EmacsShellWidget w = (EmacsShellWidget)wid;

  if (XtIsRealized((Widget) w) &&
      w->core.window == w->emacsShell.external_window)
    {
      _XtUnregisterWindow(w->core.window, w);
      wid->core.window = 0;
    }
}

