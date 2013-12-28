/* Emacs shell widget.
   Copyright (C) 1994 Sun Microsystems, Inc.

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

Written by Ben Wing, May, 1994.

*/

/*
   It is rather unfortunate that we have to do this.  Blame those
   short-sighted people who designed the monstrosities known as
   Xt and ICCCM.
*/

/*
   This widget is not actually Emacs-specific; perhaps there could
   be a better name than "EmacsShell".  What it does is work around
   a limitation in Xt in correctly dealing with the window-manager
   size hints with applications that
   
   (a) dynamically change their window size
   (b) have a cell size (width-inc and height-inc) other than 1

   and

   (c) cannot predict in advance exactly what size their shell will be
       (This is the more common situation, when you have a number
       of widgets, each with their own size ideas)

   This widget assumes that your program contains a fixed "base size"
   plus some number of cells (e.g. character cells).  The WMShell
   resources "widthInc" and "heightInc" specify the size of a
   character cell, and the window manager will report the app's
   size in cells rather than in pixels.

   If you use this widget, do not use the WMShell resources
   "baseWidth", "baseHeight", "minWidth", or "minHeight".
   Instead, use "widthCells" and "heightCells" to specify the
   current size in cells (you must keep this up-to-date),
   and "minWidthCells" and "minHeightCells" to specify the
   minimum size in cells.
   
   Every time that the program issues a size command, the
   "baseWidth", "baseHeight", "minWidth", and "minHeight" fields
   of the WM_NORMAL_HINTS property will be updated to stay in
   line with the resource values specified above.  The calculations
   are done once the desired shell size is known but before the
   window-manager size-change request is issued. (We must do it
   at this time because before then we don't know what size we
   will request, and after the request the deed has already
   been done.)

   After you change the "baseWidth", "baseHeight", "minWidth",
   or "minHeight" resources, you need to call
   EmacsShellUpdateSizeHints() to manually update the size
   hints, except in the following two circumstances:

   (a) you are about to make a geometry request.
   (b) you are changing only "baseWidth" and "baseHeight"
       from within a resize procedure.  (In this case,
       the size hints are already correct.)

*/

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <X11/StringDefs.h>
#include "xintrinsicp.h"
#include <X11/Shell.h>
#include <X11/ShellP.h>
#include <X11/Vendor.h>
#include <X11/VendorP.h>
#include "EmacsShellP.h"

typedef struct {
    XtPointer           next_extension;           
    XrmQuark            record_type;             
    long                version;
    Cardinal            record_size;
} GenericClassExtRec;

static XtGeometryResult EmacsShellRootGeometryManager (Widget gw,
  XtWidgetGeometry *request, XtWidgetGeometry *reply);
static void EmacsShellChangeManaged (Widget w);

/* snarfed from Shell.c */
#define BIGSIZE ((Dimension)32767)
 
static XtResource resources[] = {
#define offset(field) XtOffset(EmacsShellWidget, emacs_shell.field)
#define coreoffset(field) XtOffset(EmacsShellWidget, core.field)
#ifdef LWLIB_USES_MOTIF
  /* *** BOGOSITY^10! *** The Motif VendorShell fucks around with
     the default values for X and Y, for no obvious reason.  This
     causes Shell to indicate that the defaults of (0,0) were
     program-specified, instead of letting the WM do what it wants. */
  {XtNx, XtCPosition, XtRPosition, sizeof(Position),
     coreoffset (x), XtRImmediate, (XtPointer)BIGSIZE},
  {XtNy, XtCPosition, XtRPosition, sizeof(Position),
     coreoffset (y), XtRImmediate, (XtPointer)BIGSIZE},
#endif
  { XtNwidthCells, XtCWidthCells, XtRInt, sizeof(int),
      offset (width_cells), XtRImmediate, (XtPointer)0},
  { XtNheightCells, XtCHeightCells, XtRInt, sizeof(int),
      offset (height_cells), XtRImmediate, (XtPointer)0},
  { XtNminWidthCells, XtCMinWidthCells, XtRInt, sizeof(int),
      offset (min_width_cells), XtRImmediate, (XtPointer)0},
  { XtNminHeightCells, XtCMinHeightCells, XtRInt, sizeof(int),
      offset (min_height_cells), XtRImmediate, (XtPointer)0},
};

static CompositeClassExtensionRec compositeClassExtRec = {
    NULL,
    NULLQUARK,
    XtCompositeExtensionVersion,
    sizeof(CompositeClassExtensionRec),
    TRUE,
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
    /* superclass	  */	(WidgetClass) &topLevelShellClassRec,
    /* class_name	  */	"EmacsShell",
    /* size		  */	sizeof(EmacsShellRec),
    /* Class Initializer  */	NULL,
    /* class_part_initialize*/	NULL, /* XtInheritClassPartInitialize, */
    /* Class init'ed ?	  */	FALSE,
    /* initialize	  */	NULL,
    /* initialize_notify  */	NULL,
    /* realize		  */	XtInheritRealize,
    /* actions		  */	NULL,
    /* num_actions	  */	0,
    /* resources	  */	resources,
    /* resource_count	  */	XtNumber (resources),
    /* xrm_class	  */	NULLQUARK,
    /* compress_motion	  */	FALSE,
    /* compress_exposure  */	TRUE,
    /* compress_enterleave*/	FALSE,
    /* visible_interest	  */	TRUE,
    /* destroy		  */	NULL,
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
    /* change_managed	  */	EmacsShellChangeManaged,
    /* insert_child	  */	XtInheritInsertChild,
    /* delete_child	  */	XtInheritDeleteChild,
    /* extension	  */	(XtPointer)&compositeClassExtRec
  },{ /* Shell */
    /* extension	  */	(XtPointer)&shellClassExtRec
  },{ /* WMShell */
    /* extension	  */	NULL
  },{ /* VendorShell */
    /* extension	  */	NULL
  },{ /* TopLevelShell */
    /* extension	  */	NULL
  },{ /* EmacsShell */
    0
  }
};

WidgetClass emacsShellWidgetClass = (WidgetClass) &emacsShellClassRec;

static void update_size_hints_internal (EmacsShellWidget w,
					int width, int height)
{
  int base_width, base_height;
  int cell_width, cell_height;

  /* time to update them thar size hints */
  cell_width = w->wm.size_hints.width_inc;
  cell_height = w->wm.size_hints.height_inc;
  base_width = width - cell_width * w->emacs_shell.width_cells;
  base_height = height - cell_height * w->emacs_shell.height_cells;
#ifdef DEBUG_GEOMETRY_MANAGEMENT
  /* Very useful info when debugging geometry management problems.
     When it's guaranteed that no more such problems exist, take
     this stuff out. */
  printf("update_size_hints_internal:\n");
  printf("  actual pixel size: %d %d\n", width, height);
  printf("  cell size in pixels: %d %d\n", cell_width, cell_height);
  printf("  text area size in cells: %d %d\n", w->emacs_shell.width_cells,
	 w->emacs_shell.height_cells);
  printf("  base size set to: %d %d\n", base_width, base_height);
  fflush (stdout);
#endif
  XtVaSetValues((Widget) w,
		XtNbaseWidth, base_width,
		XtNbaseHeight, base_height,
		XtNminWidth, base_width +
		cell_width * w->emacs_shell.min_width_cells,
		XtNminHeight, base_height +
		cell_height * w->emacs_shell.min_height_cells,
		NULL);
}

#if 0 /* Not currently used */

/* The root_geometry_manager() method in Shell.c is fucked up with regard
   to the user-specified-position vs. program-specified-position and
   user-specified-size vs. program-specified-size flag. (It always
   sets program-specified whenever the program requests a change
   in its size or position, even when this came from direct user
   request.) So we provide external entry points to fix this after
   the program requested a size or position change.  If it turns
   out that the user-specified-position flag needs to be set at the
   *same* time that the geometry change request is made, then we
   will have to duplicate the entire root_geometry_manager() method;
   but I don't think there are any WM's that require this. */

/* junk stolen from IntrinsicI.h */

extern void _XtAllocError(
#if NeedFunctionPrototypes
    String	/* alloc_type */
#endif
);

/* junk ungraciously copied from Shell.c */

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

static void _SetWMSizeHints(w)
    WMShellWidget w;
{
    XSizeHints *size_hints = XAllocSizeHints();

    if (size_hints == NULL) _XtAllocError("XAllocSizeHints");
    ComputeWMSizeHints(w, size_hints);
    XSetWMNormalHints(XtDisplay((Widget)w), XtWindow((Widget)w), size_hints);
    XFree((char*)size_hints);
}

/* end of junk ungraciously copied from Shell.c */

#endif /* 0 */

static XtGeometryResult SuperClassRootGeometryManager (gw, request, reply)
    Widget gw;
    XtWidgetGeometry *request, *reply;
{
  ShellWidgetClass swc = (ShellWidgetClass) topLevelShellWidgetClass;
  ShellClassExtensionRec *scer;
  GenericClassExtRec *gcer;

  /* find the shell extension record that specifies the
     root geometry manager method */
  for (gcer = (GenericClassExtRec *) swc->shell_class.extension;
       gcer;
       gcer = (GenericClassExtRec *) gcer->next_extension)
    {
      if (gcer->record_type == NULLQUARK)
	break;
    }

  if (!gcer)
    abort ();

  /* call it to actually make the geometry request */
  scer = (ShellClassExtensionRec *) gcer;
  return (scer->root_geometry_manager)(gw, request, reply);
}

static XtGeometryResult EmacsShellRootGeometryManager (gw, request, reply)
    Widget gw;
    XtWidgetGeometry *request, *reply;
{
  EmacsShellWidget w = (EmacsShellWidget) gw;
  /* OK since this file is not dumped */
  static int reentrant = 0;
  XtGeometryResult result;

  if (reentrant)
    abort ();
  reentrant++;

#ifdef DEBUG_GEOMETRY_MANAGEMENT
  printf ("root_geometry_manager:\n");
  printf ("  current shell size: %d %d\n", w->core.width, w->core.height);
  if (request->request_mode & CWWidth)
    printf ("width requested;");
  if (request->request_mode & CWHeight)
    printf ("height requested;");
  printf ("\n");
  printf ("  requested shell size: %d %d\n", request->width, request->height);
#endif
  /* update the size hints */
  update_size_hints_internal (w,
			      request->request_mode & CWWidth ?
			      request->width : w->core.width,
			      request->request_mode & CWHeight ?
			      request->height : w->core.height);

  result = SuperClassRootGeometryManager (gw, request, reply);

#ifdef DEBUG_GEOMETRY_MANAGEMENT
  printf ("  result: %s\n",
	  result == XtGeometryYes ? "XtGeometryYes" :
	  result == XtGeometryNo ? "XtGeometryNo" :
	  result == XtGeometryAlmost ? "XtGeometryAlmost" :
	  "XtGeometryDone");
  if (reply->request_mode & CWWidth)
    printf ("width returned;");
  if (reply->request_mode & CWHeight)
    printf ("height returned;");
  printf ("\n");
  printf ("  resulting shell size: %d %d\n", reply->width, reply->height);
  printf ("----------\n");
  fflush (stdout);
#endif
  reentrant--;
  return result;
}

static void
EmacsShellChangeManaged (Widget wid)
{
  EmacsShellWidget w = (EmacsShellWidget) wid;

  /* If not realized, then we're being called from XtRealizeWidget().
     RootGeometryManager() has not yet been called, and thus our
     base size is incorrect.  We need to set it now or the Shell
     will mess up geometry specifications with negative positional
     offsets. */
  if (!XtIsRealized (wid))
    {
      Widget child = NULL;
      int i;
      
      /* the managed child indicates what our size is */
      for (i = 0; i < w->composite.num_children; i++) {
	if (XtIsManaged(w->composite.children[i])) {
	  child = w->composite.children[i];
	  break;
	}
      }
      
      update_size_hints_internal (w, child->core.width, child->core.height);
    }

  /* call the real ChangeManaged */
  (((ShellWidgetClass) topLevelShellWidgetClass)->
   composite_class.change_managed)(wid);
}
    

/******************* external entry points *********************/

void
EmacsShellUpdateSizeHints (Widget gw)
{
  EmacsShellWidget w = (EmacsShellWidget) gw;
  update_size_hints_internal (w, w->core.width, w->core.height);
}

#if 0 /* Not currently used */

void
EmacsShellSetSizeUserSpecified (Widget gw)
{
  EmacsShellWidget w = (EmacsShellWidget) gw;
  w->wm.size_hints.flags |= USSize;
  w->wm.size_hints.flags &= ~PSize;
  if (!w->shell.override_redirect && XtIsRealized (gw))
    _SetWMSizeHints ((WMShellWidget)gw);
}

void
EmacsShellSetPositionUserSpecified (Widget gw)
{
  EmacsShellWidget w = (EmacsShellWidget) gw;
  w->wm.size_hints.flags |= USPosition;
  w->wm.size_hints.flags &= ~PPosition;
  if (!w->shell.override_redirect && XtIsRealized (gw))
    _SetWMSizeHints ((WMShellWidget)gw);
}

#endif /* 0 */

void
EmacsShellSmashIconicHint (Widget shell, int iconic_p)
{
  /* See comment in xfns.c about this */
  WMShellWidget wmshell;
  int old, new;
  if (! XtIsSubclass (shell, wmShellWidgetClass)) abort ();
  wmshell = (WMShellWidget) shell;
  old = (wmshell->wm.wm_hints.flags & StateHint
	 ? wmshell->wm.wm_hints.initial_state
	 : NormalState);
  new = (iconic_p ? IconicState : NormalState);
  wmshell->wm.wm_hints.flags |= StateHint;
  wmshell->wm.wm_hints.initial_state = new;
}
