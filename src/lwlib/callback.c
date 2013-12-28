/* Defines a function to determine whether a widget uses a callback.
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

/* 
 *   The function XtHasCallbackProcedure() tells you whether a given Widget
 *   has a given procedure on the named callback list.   This file copies
 *   code and data structures from the X11r4 and X11r5 Xt source, and is thus
 *   a portability problem.  It also requires data structures defined in
 *   CallbackI.h, which is a non-exported Xt header file, so you can't
 *   compile this file unless you have the Xt sources online.
 */

#if (defined(THIS_IS_X11R4) && defined(THIS_IS_X11R5))
#error only one of THIS_IS_X11R4 and THIS_IS_X11R5 may be defined.
#endif

#if (! (defined(THIS_IS_X11R4) || defined(THIS_IS_X11R5)))
#error one of THIS_IS_X11R4 and THIS_IS_X11R5 must be defined.
#endif

#ifdef THIS_IS_X11R4

/* #### everything on this page was copied from the X11r4 lib/Xt/Callback.c.
 */

#include <X11/IntrinsicP.h>
#include <ResourceI.h>
#include <CallbackI.h>

typedef struct _CallbackRec {
    CallbackList  next;
    Widget	    widget;
    XtCallbackProc  callback;
    XtPointer	    closure;
} CallbackRec;

struct _CallbackStruct {
    XtCallbackList	external_form;
    int			array_size;
    CallbackList	internal_form;
};

static CallbackStruct **FetchCallbackStruct(widget, name)
    Widget widget;
    String name;
{
    register _XtOffsetList  offsets;
    register XrmQuark       quark;

    quark = StringToQuark(name);
    for (offsets = (_XtOffsetList)widget->core.widget_class->
				 core_class.callback_private;
	 offsets != NULL;
	 offsets = offsets->next) {
	if (quark == offsets->name) {
	    return (CallbackStruct**)((char *) widget - offsets->offset - 1);
	}
    }
    return NULL;
}


static CallbackList *FetchCallbackList (widget, name, create)
    Widget  widget;
    String  name;
    Boolean create;		/* if False, may return a empty list */
{
    register CallbackStruct **callbacks = FetchCallbackStruct(widget, name);
    if (callbacks == NULL) return NULL;
    if (*callbacks == NULL) {
	if (!create) {
	    static CallbackList emptyList = NULL;
	    return &emptyList;
	}
	*callbacks = XtNew(CallbackStruct);
	(*callbacks)->external_form = NULL;
	(*callbacks)->array_size = 0;
	(*callbacks)->internal_form = NULL;
    }
    return _XtCallbackList(*callbacks);
} /* FetchCallbackList */

#endif /* THIS_IS_X11R4 */


#ifdef THIS_IS_X11R5

/* #### everything on this page was copied from the X11r5 lib/Xt/Callback.c.
 */

#include <IntrinsicI.h>
/* #include <ResourceI.h> */

#define ToList(p) ((XtCallbackList) ((p)+1))

static InternalCallbackList* FetchInternalList(widget, name)
    Widget	widget;
    String	name;
{
    register XrmQuark		quark;
    register int 		n;
    register CallbackTable	offsets;

    quark = StringToQuark(name);
    offsets = (CallbackTable) 
	widget->core.widget_class->core_class.callback_private;

    for (n = (int) *(offsets++); --n >= 0; offsets++)
	if (quark == (*offsets)->xrm_name)
	    return (InternalCallbackList *) 
		((char *) widget - (*offsets)->xrm_offset - 1);
    return NULL;
}

#endif /* THIS_IS_X11R5 */


/* this function is new */

Boolean
XtHasCallbackProcedure (Widget widget, String name, XtCallbackProc callback)
{
#ifdef THIS_IS_X11R4
  CallbackList *callbacks = FetchCallbackList (widget, name, False);
  CallbackList cl;
#else /* R5 */
  InternalCallbackList *callbacks = FetchInternalList (widget, name);
  InternalCallbackList icl;
  XtCallbackList cl;
  int i;
#endif

  if (callbacks == NULL)
    {
      XtAppWarningMsg (XtWidgetToApplicationContext (widget),
		       "invalidCallbackList", "XtHasCallbackProcedure",
		       "XtToolkitError",
		       "Cannot find callback list in XtHasCallbackProcedure",
		       (String *)NULL, (Cardinal *)NULL);
      return False;
    }

#ifdef THIS_IS_X11R4

  for (cl = *callbacks; cl != NULL; (cl = *(callbacks = &cl->next)))
    if ((cl->widget == widget) && (cl->callback == callback))
      return True;

#else /* R5 */

  icl = *callbacks;
  cl = ToList (icl);
  for (i = icl->count; --i >= 0; cl++)
    if (cl->callback == callback)
      return True;

#endif
  return False;
}
