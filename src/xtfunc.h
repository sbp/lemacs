/* Interface to Xt widgets.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Written by Ben Wing, September 1993. */

#ifndef _EMACS_XTFUNC_H_
#define _EMACS_XTFUNC_H_

#include "hash.h"

/****************************** TYPES ******************************/

struct Lisp_Widget;
struct _xtf_type;

typedef XtArgVal (*L2C)(Lisp_Object, struct _xtf_type *,
			struct Lisp_Widget *, int, char *);
typedef Lisp_Object (*C2L)(XtArgVal, struct _xtf_type *,
			   struct Lisp_Widget *, int, char *);
typedef void (*STAGE2)(Lisp_Object, struct _xtf_type *,
		       struct Lisp_Widget *, int, char *);

/* information associated with a type class */

typedef struct
{
  L2C L2C;
  C2L C2L;
  STAGE2 stage2;
  char *name;
  struct {
    int needs_slot : 1;
  } flags;
} xtf_type_class;

/* information associated with a particular type */

typedef struct _xtf_type
{
  xtf_type_class *cl;
  XtArgVal data;
} xtf_type;

/****************************** RESOURCES ******************************/

/* possible access flags for a resource */

typedef enum
{
  access_create,
  access_get,
  access_set
} xtf_access;

/* information pertaining to a particular resource in a widget class.

   LISP_NAME and LISP_CLASS are Lisp symbols specifying this resource's
   name and class.

   C_NAME and C_CLASS are strings specifying this resource's name and
   class.

   ACCESS specifies the ways in which this resource can be accessed.

   DATA is additional data associated with this resource.  E.g. if this
   resource is a list, this specifies the associated resource giving
   the number of elements in the list; if this resource is a pixmap,
   this specifies the associated resources giving the width, height,
   mask, etc. of the pixmap.
*/

typedef struct
{
  Lisp_Object lisp_name, lisp_class;
  char *c_name, *c_class;
  xtf_type *type;
  int access;
  XtArgVal data;
} xtf_resource_spec;

/****************************** WIDGET CLASSES ******************************/

#define XWIDGET_CLASS(a) ((struct Lisp_Widget_Class *) XPNTR(a))
#define WIDGET_CLASSP(x) RECORD_TYPEP ((x), lrecord_widget_class)
#define CHECK_WIDGET_CLASS(x, i) \
  do { if (!WIDGET_CLASSP ((x))) x = wrong_type_argument (Qwidget_classp, (x)); } while (0)
extern CONST struct lrecord_implementation lrecord_widget_class[];
extern Lisp_Object Fwidget_classp (Lisp_Object obj);
extern Lisp_Object Fwidget_class_name (Lisp_Object obj);
extern Lisp_Object Qwidget_classp;

/* a Lisp_Widget_Class is an object defining a widget class, specifying
   the class's name, parent class, and resources, each of which is specified
   by an xtf_resource_spec. */

struct Lisp_Widget_Class
{
  struct lcrecord_header header;
  Lisp_Object superclass;
  Lisp_Object lisp_name;
  WidgetClass class;
  int num;
  xtf_resource_spec *list;
  
  /* if this class has a special func, then it is called:

     1) right before the call to XtSetValues() in xt-set-values.  VALUES
        contains the alist passed to xt-set-values and the equivalent
	specifications in C are in the global arglist.  ACCESS == access_set.

     2) After the call to XtGetValues() in xt-get-values, and after the
        returned resources have been converted into Lisp.  VALUES contains
	the alist that will be returned to the caller and the equivalent
	specifications in C are in the global arglist.  ACCESS == access_get.

     Note that in all cases, a pointer to the alist is passed so that the
     head of the alist can be changed if necessary.
  */
  void (*special_func)(Lisp_Object widget, Lisp_Object *values,
		       xtf_access access);

  /* if this class has a virtual func for set and/or get, then it is called
     in place of XtGetValues() or XtSetValues(). */
  void (*virtual_set)(Widget object, ArgList args, Cardinal num_args);
  void (*virtual_get)(Widget object, ArgList args, Cardinal num_args);
};

/****************************** WIDGETS ******************************/

#define XWIDGET(a) ((struct Lisp_Widget *) XPNTR(a))
#define WIDGETP(x) RECORD_TYPEP ((x), lrecord_widget)
#define CHECK_WIDGET(x, i) \
  do { if (!WIDGETP ((x))) x = wrong_type_argument (Qwidgetp, (x)); } while (0)
extern CONST struct lrecord_implementation lrecord_widget[];
extern Lisp_Object Fwidgetp (Lisp_Object obj);
extern Lisp_Object Fwidget_name (Lisp_Object obj);
extern Lisp_Object Qwidgetp;

#define WIDGET_LIVE_P(w) ((w)->widget != 0)

struct Lisp_Widget {
  struct lcrecord_header header;

  Lisp_Object class;
  Widget widget;
  Lisp_Object parent;

  /* Both of the hash tables below are keyed with strings. */

  /* Lisp objects that must be protected against garbage collection.
     This is used for widgets, pixmaps, pixels, opaque data, etc. that
     are stored in resources.  If the corresponding Lisp_Object is
     destroyed, then the C object it refers to will be destroyed,
     which is a Bad thing. */
  c_hashtable lisp_objs;

  /* Additional private data that is not Lisp_Objects */
  c_hashtable private;

  /* whether we should destroy the C widget when the Lisp_Widget is
     destroyed */
  char destroyable;
};

/****************************** MISCELLANEOUS ******************************/

Arg *xtf_arglist;
Cardinal xtf_argcur;

extern Lisp_Object Qxt_error;

extern Lisp_Object Qparent_relative;
extern Lisp_Object Qxt_unspecified_pixmap;

#endif /* _EMACS_XTFUNC_H_ */
