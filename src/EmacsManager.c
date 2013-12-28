/* Emacs manager widget.
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

#include "config.h"

#include <X11/StringDefs.h>
#include "EmacsManagerP.h"
#ifdef HAVE_XMU
#include <X11/Xmu/CharSet.h>
#else
#include <stdio.h>
#include "xmu.h"
#endif

/* For I, Emacs, am a kind god.  Unlike the goddess Athena and the
   Titan Motif, I require no ritual sacrifices to placate the lesser
   daemons of geometry management. */

static XtResource resources[] = {
#define offset(field) XtOffset(EmacsManagerWidget, emacs_manager.field)
  { XtNtextArea, XtCTextArea, XtRWidget, sizeof(Widget),
      offset(text_area), XtRImmediate, (XtPointer) 0 },
  { XtNtopAreaWidgets, XtCTopAreaWidgets, XtRWidgetList, sizeof(WidgetList),
      offset(top_area_widgets), XtRImmediate, (XtPointer) 0 },
  { XtNnumTopAreaWidgets, XtCNumTopAreaWidgets, XtRCardinal, sizeof(Cardinal),
      offset(num_top_area_widgets), XtRImmediate, (XtPointer) 0 },
  { XtNscrollbarWidget, XtCScrollbarWidget, XtRWidget, sizeof(Widget),
      offset(scrollbar_widget), XtRImmediate, (XtPointer) 0 },
  { XtNscrollBarPlacement, XtCScrollBarPlacement, XtRScrollBarPlacement,
      sizeof(unsigned char), offset(scrollbar_placement), XtRImmediate,
#ifdef LWLIB_USES_MOTIF
      (XtPointer) XtBOTTOM_RIGHT
#else
      (XtPointer) XtBOTTOM_LEFT
#endif
  },
  { XtNpreserveSameSize, XtCPreserveSameSize, XtRBoolean, sizeof(Boolean),
      offset(preserve_same_size), XtRImmediate, (XtPointer) False },
  { XtNrefigureMode, XtCRefigureMode, XtRBoolean, sizeof(Boolean),
      offset(refigure_mode), XtRImmediate, (XtPointer) True },
};

/****************************************************************
 *
 * Full class record constant
 *
 ****************************************************************/

static XtGeometryResult QueryGeometry (Widget wid,
				       XtWidgetGeometry *request,
				       XtWidgetGeometry *reply);
static void Resize (Widget w);
static XtGeometryResult GeometryManager (Widget w, XtWidgetGeometry *request,
					 XtWidgetGeometry *reply);
static void ChangeManaged (Widget w);
static void Realize (Widget w, Mask *valueMask,
		     XSetWindowAttributes *attributes);
static void ClassInitialize (void);

EmacsManagerClassRec emacsManagerClassRec = {
  {
/* core_class fields      */
#ifdef LWLIB_USES_MOTIF
    /* superclass         */    (WidgetClass) &xmManagerClassRec,
#else
    /* superclass         */    (WidgetClass) &compositeClassRec,
#endif
    /* class_name         */    "EmacsManager",
    /* widget_size        */    sizeof(EmacsManagerRec),
    /* class_initialize   */    ClassInitialize,
    /* class_part_init    */	NULL,
    /* class_inited       */	FALSE,
    /* initialize         */    NULL,
    /* initialize_hook    */	NULL,
    /* realize            */    Realize,
    /* actions            */    NULL,
    /* num_actions	  */	0,
    /* resources          */    resources,
    /* num_resources      */    XtNumber(resources),
    /* xrm_class          */    NULLQUARK,
    /* compress_motion	  */	TRUE,
    /* compress_exposure  */	TRUE,
    /* compress_enterleave*/	TRUE,
    /* visible_interest   */    FALSE,
    /* destroy            */    NULL,
    /* resize             */    Resize,
    /* expose             */    NULL,
    /* set_values         */    NULL,
    /* set_values_hook    */	NULL,
    /* set_values_almost  */    XtInheritSetValuesAlmost,
    /* get_values_hook    */	NULL,
    /* accept_focus       */    NULL,
    /* version            */	XtVersion,
    /* callback_private   */    NULL,
    /* tm_table           */    XtInheritTranslations,
    /* query_geometry     */	QueryGeometry,
    /* display_accelerator*/	XtInheritDisplayAccelerator,
    /* extension          */	NULL
  },
  {
/* composite_class fields */
    /* geometry_manager   */    GeometryManager,
    /* change_managed     */    ChangeManaged,
    /* insert_child	  */	XtInheritInsertChild,
    /* delete_child	  */	XtInheritDeleteChild,
    /* extension          */	NULL
  },
#ifdef LWLIB_USES_MOTIF
  {
  /* constraint_class fields */
      NULL,                                     /* resource list        */   
      0,                                        /* num resources        */   
      0,                                        /* constraint size      */   
      (XtInitProc)NULL,                         /* init proc            */   
      (XtWidgetProc)NULL,                       /* destroy proc         */   
      (XtSetValuesFunc)NULL,                    /* set values proc      */   
      NULL,                                     /* extension            */
  },
  {
/* manager_class fields */
    XtInheritTranslations,			/* translations           */
    NULL,					/* syn_resources      	  */
    0,						/* num_syn_resources 	  */
    NULL,					/* syn_cont_resources     */
    0,						/* num_syn_cont_resources */
    XmInheritParentProcess,			/* parent_process         */
    NULL,					/* extension           	  */
  },
#endif
  {
/* emacs_manager_class fields */
    /* empty		  */	0,
  }
};

WidgetClass emacsManagerWidgetClass = (WidgetClass)&emacsManagerClassRec;

/* Structure containing info about the widgets that we manage */

struct em_widget_info
{
  WidgetList top;
  Cardinal numtop;
  Dimension topbreadth;

  Widget sb;
  Dimension sbw;
  Dimension sbbord;
  Dimension sbgirth;

  Widget text;
  Dimension textbord;
};

/* Fill a struct em_widget_info */

static void
get_widget_info (EmacsManagerWidget w, struct em_widget_info *x)
{
  Cardinal i;

  /* top area (menubar and psheets) */
  x->top = w->emacs_manager.top_area_widgets;
  x->numtop = w->emacs_manager.num_top_area_widgets;

  /* compute height of all top-area widgets */
  for (i=0, x->topbreadth = 0; i<x->numtop; i++)
    {
      Widget wid = x->top[i];
      if (wid && XtIsManaged (wid))
	x->topbreadth += wid->core.height + 2*wid->core.border_width;
    }

  /* scrollbar */
  x->sb = w->emacs_manager.scrollbar_widget;
  if (!x->sb || !XtIsManaged (x->sb))
    {
      x->sb = 0;
      x->sbw = 0;
      x->sbbord = 0;
    }
  else
    {
      x->sbbord = x->sb->core.border_width;
      x->sbw = x->sb->core.width;
    }
  x->sbgirth = x->sbw + 2*x->sbbord;

  /* text area */
  x->text = w->emacs_manager.text_area;
  if (!x->text || !XtIsManaged (x->text))
    {
      x->text = 0;
      x->textbord = 0;
    }
  else
    x->textbord = x->text->core.border_width;
}

/* resize my child widgets to fit into (width, height) */

static void
do_layout (EmacsManagerWidget w, Dimension width, Dimension height)
{
  struct em_widget_info x;
  Position text_x = 0, text_y = 0;
  Cardinal i;
  
  get_widget_info (w, &x);

  /* first the menubar and psheets ... */
  for (i=0; i<x.numtop; i++)
    {
      Widget wid = x.top[i];
      if (wid && XtIsManaged (wid))
	{
	  Dimension bord = wid->core.border_width;
	  XtConfigureWidget (wid, 0, text_y,
			     width - 2*bord, wid->core.height,
			     bord);
	  text_y += wid->core.height + 2*bord;
	}
    }
    
  /* then the scrollbar ... */
  if (x.sb)
    {
      Position sbx = 0;

      if (w->emacs_manager.scrollbar_placement & EM_LEFT)
	text_x = x.sbgirth;
      else
	sbx = width - x.sbgirth;
      XtConfigureWidget (x.sb, sbx, text_y,
			 x.sbw, height - text_y - 2*x.sbbord, 
			 x.sbbord);
    }

  /* finally the text area */
  if (x.text)
    XtConfigureWidget (x.text, text_x, text_y,
		       width - x.sbgirth - 2*x.textbord,
		       height - text_y - 2*x.textbord,
		       x.textbord);
}

/* What is my preferred size?  A suggested size may be given. */

static XtGeometryResult
QueryGeometry (Widget wid, XtWidgetGeometry *request, XtWidgetGeometry *reply)
{
  EmacsManagerWidget w = (EmacsManagerWidget) wid;
  XtWidgetGeometry req, repl;
  int mask = request->request_mode & (CWWidth | CWHeight);
  struct em_widget_info x;

  get_widget_info (w, &x);

  if (!x.text)
    return XtGeometryYes;

  /* strip away menubar and scrollbar from suggested size, and ask
     the text widget what size it wants to be */
  req.request_mode = mask;
  if (mask & CWWidth)
    req.width = request->width - x.sbgirth -2*x.textbord;
  if (mask & CWHeight)
    req.height = request->height - x.topbreadth - 2*x.textbord;
  XtQueryGeometry (x.text, &req, &repl);

  /* Now add the menubar and scrollbar back again */
  reply->width = repl.width + x.sbgirth + 2*x.textbord;
  reply->height = repl.height + x.topbreadth + 2*x.textbord;
  reply->request_mode = CWWidth | CWHeight;

  if (((mask & CWWidth) && (request->width != reply->width))
      || ((mask & CWHeight) && (request->height != reply->height)))
    return XtGeometryAlmost;
  return XtGeometryYes;
}

static void
Resize (Widget w)
{
  do_layout ((EmacsManagerWidget) w, w->core.width, w->core.height);
}


/* Attempt to resize ourself to fit our widgets.  Return value is
   XtGeometryYes (we have been resized), XtGeometryNo (sorry!),
   or XtGeometryAlmost (an amended suggestion is returned, but
   we weren't actually resized). */
static XtGeometryResult
self_resize (EmacsManagerWidget emw, Dimension *width, Dimension *height)
{
  Dimension proposed_width, proposed_height;
  XtWidgetGeometry req, repl;

  /* find out how big we'd like to be ... */

  if (emw->emacs_manager.preserve_same_size)
    {
      req.request_mode = CWWidth | CWHeight;
      req.width = emw->core.width;
      req.height = emw->core.height;
    }
  else
    req.request_mode = 0;
  XtQueryGeometry ((Widget) emw, &req, &repl);
  proposed_width = repl.width;
  proposed_height = repl.height;

  /* do nothing if we're already that size */
  if (emw->core.width == proposed_width &&
      emw->core.height == proposed_height)
    return XtGeometryYes;

  return XtMakeResizeRequest ((Widget) emw,
			      proposed_width, proposed_height,
			      width, height);
}

static XtGeometryResult
GeometryManager (Widget w, XtWidgetGeometry *request, XtWidgetGeometry *reply)
{
  Dimension width, height, borderWidth;
  EmacsManagerWidget emw;
  XtGeometryResult res;

  emw = (EmacsManagerWidget) w->core.parent;
  
  /* position request always denied */
  if ((request->request_mode & CWX && request->x != w->core.x) ||
      (request->request_mode & CWY && request->y != w->core.y))
    return XtGeometryNo;

  /* Size request? */
  if (request->request_mode & (CWWidth | CWHeight | CWBorderWidth))
    {
      /* Save current size and set to new size */
      width = w->core.width;
      height = w->core.height;
      borderWidth = w->core.border_width;

      if (request->request_mode & CWWidth)
	w->core.width = request->width;
      if (request->request_mode & CWHeight)
	w->core.height = request->height;
      if (request->request_mode & CWBorderWidth)
	w->core.border_width = request->border_width;

      /* Attempt to resize ourself to fit */
      res = self_resize (emw, &reply->width, &reply->height);

      /* If yes, then we were resized and need to fix up
         our child widgets */
      if (res == XtGeometryYes)
	{
	  Resize (w->core.parent);
	}
      else
	{
	  /* Cannot satisfy request, change back to original geometry */
	  w->core.width = width;
	  w->core.height = height;
	  w->core.border_width = borderWidth;
	}
      return res;
    }
  else
    /* Any other requests are OK */
    return XtGeometryYes;
}

static void
ChangeManaged (Widget w)
{
  EmacsManagerWidget emw = (EmacsManagerWidget) w;
  Dimension width, height;

  if (!emw->emacs_manager.refigure_mode)
    return;

  /* try to resize ourself to fit.  If an alternative suggestion
     was proposed, go ahead and accept that. */
  if (self_resize (emw, &width, &height) == XtGeometryAlmost)
    XtMakeResizeRequest (w, width, height, NULL, NULL);

  /* Now lay out our child widgets. */
  Resize (w);
}

static void
Realize (Widget w, Mask *valueMask, XSetWindowAttributes *attributes)
{
  attributes->bit_gravity = NorthWestGravity;
  *valueMask |= CWBitGravity;
  
  XtCreateWindow(w, (unsigned) InputOutput, (Visual *) CopyFromParent,
		 *valueMask, attributes);
}

/**** string-to-scrollbar-placement converter: modelled after edge-type
      converter in Xaw/Form.c ****/

/* #### server-dependent - these need to be per-display some day */
static XrmQuark	XtQTopLeft, XtQBottomLeft, XtQTopRight, XtQBottomRight;

#define	done(address, type) \
{ toVal->size = sizeof(type); \
    toVal->addr = (caddr_t) address; \
    return; \
}

/* ARGSUSED */
static void
_CvtStringToScrollBarPlacement (args, num_args, fromVal, toVal)
     XrmValuePtr args;		/* unused */
     Cardinal *num_args;	/* unused */
     XrmValuePtr fromVal;
     XrmValuePtr toVal;
{
  unsigned char scrollbar_placement;
  XrmQuark q;
  char lowerName[1000];
  
  XmuCopyISOLatin1Lowered (lowerName, (char*)fromVal->addr);
  q = XrmStringToQuark(lowerName);
  if (q == XtQTopLeft) {
    scrollbar_placement = XtTOP_LEFT;
    done (&scrollbar_placement, unsigned char);
  }
  if (q == XtQBottomLeft) {
    scrollbar_placement = XtBOTTOM_LEFT;
    done (&scrollbar_placement, unsigned char);
  }
  if (q == XtQTopRight) {
    scrollbar_placement = XtTOP_RIGHT;
    done (&scrollbar_placement, unsigned char);
  }
  if (q == XtQBottomRight) {
    scrollbar_placement = XtBOTTOM_RIGHT;
    done (&scrollbar_placement, unsigned char);
  }
  XtStringConversionWarning(fromVal->addr, "scrollBarPlacement");
  toVal->addr = NULL;
  toVal->size = 0;
}

static void
ClassInitialize (void)
{
  XtQTopLeft     = XrmStringToQuark ("top_left");
  XtQBottomLeft  = XrmStringToQuark ("bottom_left");
  XtQTopRight    = XrmStringToQuark ("top_right");
  XtQBottomRight = XrmStringToQuark ("bottom_right");
  
  XtAddConverter (XtRString, XtRScrollBarPlacement,
		  _CvtStringToScrollBarPlacement, NULL, 0 );
}

void
EmacsManagerForceRefigure (Widget w)
{
  ChangeManaged (w);
}
