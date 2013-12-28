/* Xt Interface Functions.
   Copyright (C) 1993, 1994 Sun Microsystems, Inc.

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


#define DEBUG_XTFUNC

#ifdef DEBUG_XTFUNC
#define XmZERO 0
#define XmEINS 1
#define XmZWEI 2
#define XmVIER 5
#define XmSIEBEN 7
#define XmACHT 8
#define XmELF 11
#define XmNEUNZEHN 19
#define XmDREIUNDACHTZIG 83
#define XmVINGT_ET_UN 21
#endif

#include <limits.h>

#include "config.h"
#include "lisp.h"
#include "intl.h"

#include "xintrinsic.h"
#include <X11/StringDefs.h>
#include <assert.h>

#include "xterm.h"
#include "screen.h"
#include "events.h"
#include "xobjs.h"
#include "xtfunc.h"
#include "opaque.h"

#define CAR(x) (XCONS(x)->car)
#define CDR(x) (XCONS(x)->cdr)
#define TO_TYPE(lvalue, type) (* (type *) &(lvalue))

#define MAX_LISP_INT ((1L << (VALBITS - 1)) - 1)
#define MIN_LISP_INT (-1L << (VALBITS - 1))

#define DEFINE_L2C(t) static \
XtArgVal L2C_ ## t (Lisp_Object obj, xtf_type *type, \
		    struct Lisp_Widget *w, int ind, char *slot)
#define DEFINE_C2L(t) static \
Lisp_Object C2L_ ## t (XtArgVal obj, xtf_type *type, \
		       struct Lisp_Widget *w, int ind, char *slot)
#define DEFINE_STAGE2(t) static \
void stage2_ ## t (Lisp_Object obj, xtf_type *type, \
		   struct Lisp_Widget *w, int ind, char *slot)

/* let it be known that there is some bogosity in the declarations
   of types in the hash.c functions.  The value stored into a hash
   cell should probably be a long instead of a void *.  And certainly,
   it should NOT be a const void *.  The third argument to gethash()
   should be void **, not const void **.  The latter would apply
   only if the hash functions look at what the void * value points to
   and possibly that stuff elsewhere.  "const void **" implies that
   the caller is not allowed to change the data that is pointed to.
*/
     
#define STANDARD_STAGE2 do \
{ \
  Lisp_Object obj; \
\
  if (!get_lisp_object (w, slot, &obj)) \
    abort (); \
  slot[0] = 'B'; \
  add_lisp_object (w, slot, obj); \
} while (0)

#define writeit(x) (write_string_1 ((x), -1, printcharfun))
#define printobj(x) (print_internal ((x), printcharfun, 0))

Lisp_Object Qxt_error;

Lisp_Object Qparent_relative;
Lisp_Object Qxt_unspecified_pixmap;

/**********************************************************************/
/*                       GENERAL FUNCTIONS                            */
/**********************************************************************/

/* hash a string.  If strcmp (a, b) == 0, then save_string (a) ==
   save_string (b).  The return value from save_string () is a char *
   pointing to a copy of the string, which has indefinite scope. */

static c_hashtable xtf_string_table;
static char *xtf_string_array;
static int xtf_string_array_size, xtf_string_array_ind;

static char *
save_string (char *s)
{
  char *ret;
  if (!gethash (s, xtf_string_table, (CONST void **) &ret)) {
    int len = 1 + strlen (s);
    while (len > xtf_string_array_size - xtf_string_array_ind) {
      int newsize = 2 * xtf_string_array_size;
      xtf_string_array = realloc (xtf_string_array, newsize);
      xtf_string_array_size = newsize;
    }
    strcpy (ret = xtf_string_array + xtf_string_array_ind, s);
    xtf_string_array_ind += len;
    puthash (ret, ret, xtf_string_table);
  }
  return ret;
}

static void add_lisp_object (struct Lisp_Widget *w, char *slot,
			     Lisp_Object obj)
{
  puthash (save_string (slot), LISP_TO_VOID (obj), w->lisp_objs);
}

static void add_misc_data (struct Lisp_Widget *w, char *slot, void *data)
{
  puthash (save_string (slot), (void *) data, w->private);
}

static void *get_lisp_object (struct Lisp_Widget *w, char *slot,
			      Lisp_Object *obj)
{
  void *val, *retval;

  retval = (void *) gethash (save_string (slot), w->lisp_objs,
			     (CONST void **) &val);
  if (retval)
    VOID_TO_LISP (*obj, val);
  return retval;
}

static void *get_misc_data (struct Lisp_Widget *w, char *slot, void **data)
{
  return (void *) gethash (save_string (slot), w->private,
			   (CONST void **) data);
}

/**********************************************************************/
/*                         WIDGET OBJECTS                             */
/**********************************************************************/

Lisp_Object Qwidgetp;
static Lisp_Object mark_widget (Lisp_Object, void (*) (Lisp_Object));
static void print_widget (Lisp_Object, Lisp_Object, int);
static void finalize_widget (void *, int);
static int sizeof_widget (void *header) { return sizeof(struct Lisp_Widget); }
static int widget_equal (Lisp_Object, Lisp_Object, int depth);
DEFINE_LRECORD_IMPLEMENTATION (lrecord_widget,
			       mark_widget, print_widget, finalize_widget,
			       sizeof_widget, widget_equal);

/* We use a hash table to keep track of the correspondences between
   Widgets and their struct Lisp_Widget container structures.  An entry
   is added when make_widget() is called and removed when finalize_widget()
   is called, so everything stays in sync.  Thus, for any widget there
   is at most one Lisp_Widget structure corresponding to it. */

static c_hashtable xtf_widget_table;

Lisp_Object find_widget (Widget widget)
{
  Lisp_Object obj;

  if (gethash (widget, xtf_widget_table, (CONST void **) &obj))
    return obj;
  else
    return Qnil;
}

void zero_widget (struct Lisp_Widget *w)
{
  remhash (w->widget, xtf_widget_table);
  w->widget = 0;
}

static void
mark_widget_fn (CONST void* key, void* contents, void* arg)
{
  Lisp_Object obj;

  VOID_TO_LISP (obj, contents);
  /* C syntax is HIDEOUS! */
  ((void (*)(Lisp_Object))arg)(obj);
}

static Lisp_Object
mark_widget (Lisp_Object obj, void (*markobj) (Lisp_Object))
{
  struct Lisp_Widget *w = XWIDGET (obj);
  int i;

  ((markobj) (w->class));
  maphash (mark_widget_fn, w->lisp_objs, (void *) markobj);
  return w->parent;
}

static void
print_widget (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  char buf[100];
  struct Lisp_Widget *p = XWIDGET (obj);

  if (print_readably)
    error (GETTEXT ("printing unreadable object #<widget 0x%x>"),
	   p->header.uid);

  sprintf (buf, "#<widget %x \"", p->widget);
  writeit (buf);
  writeit (XtName (p->widget));
  writeit ("\" class:");
  printobj (Fsymbol_name (Fwidget_class_name (p->class)));
  if (WIDGETP (p->parent)) {
    sprintf (buf, " parent:%x>", XWIDGET (p->parent)->widget);
    writeit (buf);
  } else
    writeit (" parent:none>");
}

static void
finalize_widget (void *header, int for_disksave)
{
  struct Lisp_Widget *p = (struct Lisp_Widget *) header;
  if (for_disksave) finalose (p);
  BLOCK_INPUT;
  if (p->widget) {
    if (p->destroyable)
      XtDestroyWidget (p->widget);
    zero_widget (p);
  }
  free_hashtable (p->lisp_objs);
  free_hashtable (p->private);
}

static int
widget_equal (Lisp_Object o1, Lisp_Object o2, int depth)
{
  return XWIDGET (o1)->widget == XWIDGET (o2)->widget;
}

Lisp_Object make_widget (Widget widget,
			 Lisp_Object class,
			 Lisp_Object parent)
{
  struct Lisp_Widget *w = alloc_lcrecord (sizeof (struct Lisp_Widget),
					  lrecord_widget);
  Lisp_Object val;

  w->widget = widget;
  w->class = class;
  w->parent = parent;
  
  w->destroyable = 1;

  /* 25 is an arbitrary number that should be sufficient in most cases.
     If more objects than this get stored, then the hash table will
     be automatically grown, so not to worry. */
  w->lisp_objs = make_hashtable (25);
  w->private = make_hashtable (25);

  XSETR (val, Lisp_Widget, w);
  puthash (widget, LISP_TO_VOID (val), xtf_widget_table);
  return val;
 }

DEFUN ("widgetp", Fwidgetp, Swidgetp, 1, 1, 0,
       "Whether OBJ is a widget.")
  (obj)
  Lisp_Object obj;
{
  return (WIDGETP (obj) ? Qt : Qnil);
}

DEFUN ("live-widgetp", Flive_widgetp, Slive_widgetp, 1, 1, 0,
       "Whether OBJ is a non-deleted widget.")
     (obj)
     Lisp_Object obj;
{
  return (WIDGETP (obj) && WIDGET_LIVE_P (XWIDGET(obj)) ? Qt : Qnil);
}

DEFUN ("widget-name", Fwidget_name, Swidget_name, 1, 1, 0,
       "Return a string representing the name of WIDGET.")
     (widget)
     Lisp_Object widget;
{
  CHECK_WIDGET (widget, 0);
  return build_string (XtName (XWIDGET (widget)->widget));
}

DEFUN ("widget-widget-class", Fwidget_widget_class,
       Swidget_widget_class, 1, 1, 0,
       "Return an object representing WIDGET's class.")
     (widget)
     Lisp_Object widget;
{
  CHECK_WIDGET (widget, 0);
  return XWIDGET (widget)->class;
}

DEFUN ("widget-parent", Fwidget_parent,
       Swidget_parent, 1, 1, 0,
       "Return the parent of WIDGET.")
     (widget)
     Lisp_Object widget;
{
  CHECK_WIDGET (widget, 0);
  return XWIDGET (widget)->parent;
}

/**********************************************************************/
/*                      WIDGET CLASS OBJECTS                          */
/**********************************************************************/

Lisp_Object Qwidget_classp;
static Lisp_Object mark_widget_class (Lisp_Object, void (*) (Lisp_Object));
static void print_widget_class (Lisp_Object, Lisp_Object, int);
static int sizeof_widget_class (void *header)
{ return sizeof(struct Lisp_Widget_Class); }
static int widget_class_equal (Lisp_Object, Lisp_Object, int depth);
DEFINE_LRECORD_IMPLEMENTATION (lrecord_widget_class,
			       mark_widget_class, print_widget_class, 0,
			       sizeof_widget_class, widget_class_equal);

static Lisp_Object
mark_widget_class (Lisp_Object obj, void (*markobj) (Lisp_Object))
{
  struct Lisp_Widget_Class *w = XWIDGET_CLASS (obj);
  int i;

  ((markobj) (w->superclass));
  for (i=0; i<w->num; i++) {
    ((markobj) (w->list[i].lisp_name));
    ((markobj) (w->list[i].lisp_class));
  }
  return w->lisp_name;
}

static void
print_widget_class (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  struct Lisp_Widget_Class *p = XWIDGET_CLASS (obj);

  if (print_readably)
    error (GETTEXT ("printing unreadable object #<widget_class 0x%x>"),
	   p->header.uid);

  writeit ("#<widget_class ");
  printobj (p->lisp_name);
  writeit (">");
}

static int
widget_class_equal (Lisp_Object o1, Lisp_Object o2, int depth)
{
  return EQ (XWIDGET_CLASS (o1)->lisp_name, XWIDGET_CLASS (o2)->lisp_name);
}

Lisp_Object make_widget_class (Lisp_Object superclass,
			       Lisp_Object lisp_name,
			       WidgetClass class,
			       int num,
			       xtf_resource_spec *list)
{
  struct Lisp_Widget_Class *wc = alloc_lcrecord (sizeof
						 (struct Lisp_Widget_Class),
						 lrecord_widget_class);
  Lisp_Object val;

  wc->superclass = superclass;
  wc->lisp_name = lisp_name;
  wc->class = class;
  wc->num = num;
  wc->list = list;

  XSETR (val, Lisp_Widget_Class, wc);
  return val;
}

DEFUN ("widget-classp", Fwidget_classp, Swidget_classp, 1, 1, 0,
       "Whether the given object is a widget.")
  (obj)
  Lisp_Object obj;
{
  return (WIDGET_CLASSP (obj) ? Qt : Qnil);
}

DEFUN ("widget-class-name", Fwidget_class_name, Swidget_class_name,
       1, 1, 0,
       "Returns a symbol representing the name of the given widget class.")
     (widget_class)
     Lisp_Object widget_class;
{
  CHECK_WIDGET_CLASS (widget_class, 0);
  return XWIDGET_CLASS (widget_class)->lisp_name;
}

DEFUN ("widget-class-superclass", Fwidget_class_superclass,
       Swidget_class_superclass, 1, 1, 0,
       "Returns the superclass of the given widget class.")
     (widget_class)
     Lisp_Object widget_class;
{
  CHECK_WIDGET_CLASS (widget_class, 0);
  return XWIDGET_CLASS (widget_class)->superclass;
}

DEFUN ("widget-class-resources", Fwidget_class_resources,
       Swidget_class_resources, 1, 1, 0,
       "Returns a list of resources for the given widget class.\n\
Each entry in the list is a list (NAME TYPE ACCESS) where NAME is\n\
a symbol identifying the resource, TYPE is a symbol identifying the\n\
type, and ACCESS is a list of one or more of the following symbols:\n\
\n\
        c       Resource can be initialized at widget creation time\n\
        g       Resource can be retrieved using xt-get-values\n\
        s       Resource can be set using xt-set-values\n")
     (widget_class)
     Lisp_Object widget_class;
{
  CHECK_WIDGET_CLASS (widget_class, 0);
  /* punt for now */
  return Qnil;
}

/**********************************************************************/
/*                         CONVERSION FUNCTIONS                       */
/**********************************************************************/

/*
   The specific type-conversion functions look like this:

   XtArgVal L2C_type (Lisp_Object obj, xtf_type *type,
                      struct Lisp_Widget *w, int ind, char *slot)
   {
   }

   Lisp_Object C2L_type (XtArgVal obj, xtf_type *type,
                         struct Lisp_Widget *w, int ind, char *slot)
   {
   }

   OBJ is the object to be converted; the converted value is returned
   through the return value.

   TYPE is the type of the object.  Types that are part of a class
   (for which there is one conversion function per class) need this.

   W and IND specify the widget, and the index within this widget,
   of the resource currently being converted.  Note that the type
   of the object being converted may not be the same as the type
   of that resource, due to the possibility of complex types.
   For example, the type of the resource might be "list of atom"
   and this conversion function is for atoms.

   SLOT is typical C bogosity.  It is a string that uniquely
   identifies this particular "slot" in the resource.  For example, if
   the type of the resource is "list of list of pixel" and we are
   converting the second pixel in the third sublist in the resource's
   list, and this is the twelfth resource, then the slot string would
   be ".2.3.12". (Some conversion functions will create additional
   slots by replacing the initial period with a letter.) This is so
   that we can keep track of Lisp_Objects for GC purposes and
   allocated memory for leakage-prevention purposes.

   Note that the slot string will only be constructed if the type
   specifically requests it in its (deftype ...) initialization. */   

XtArgVal lisp_to_c (Lisp_Object obj, xtf_type *type,
		    struct Lisp_Widget *w, int ind, char *prev_slot,
		    int new_ind)
{
  char buf[1000];
  char buf2[50];

  if (!type->cl->L2C)
    signal_error (Qxt_error, list3
		  (build_string
		   (GETTEXT ("Unable to convert Lisp object")),
		   obj,
		   XWIDGET_CLASS (w->class)->list[ind].lisp_name));
  if (type->cl->flags.needs_slot) {
    sprintf (buf, ".%d", new_ind);
    if (prev_slot)
      strcat (buf, prev_slot);
    prev_slot = buf;
  } else
    prev_slot = NULL;
  return type->cl->L2C (obj, type, w, ind, prev_slot);
}

Lisp_Object c_to_lisp (XtArgVal obj, xtf_type *type,
		       struct Lisp_Widget *w, int ind, char *prev_slot,
		       int new_ind)
{
  if (!type->cl->C2L) {
    char buffer[100];
    sprintf(buffer, "%x", obj);
    signal_error (Qxt_error, list3
		  (build_string
		   (GETTEXT ("Unable to convert C object")),
		   build_string (buffer),
		   XWIDGET_CLASS (w->class)->list[ind].lisp_name));
  }
  if (type->cl->flags.needs_slot) {
    char buf[1000];
    char buf2[50];
    
    if (prev_slot) {
      strcpy (buf, prev_slot);
      strcat (buf, ".");
    }
    else
      strcpy (buf, "");
    sprintf (buf2, "%d", new_ind);
    strcat (buf, buf2);
    prev_slot = buf;
  } else
    prev_slot = NULL;
  return type->cl->C2L (obj, type, w, ind, prev_slot);
}

/**********************************************************************/
/*                   FUNCTIONS FOR SPECIFIC TYPES                     */
/**********************************************************************/

static Lisp_Object Qoverflow, Qunderflow;

/********** boolean **********/

DEFINE_L2C (boolean)
{
  if (NILP (obj))
    return (XtArgVal) 0;
  else
    return (XtArgVal) 1;
}

DEFINE_C2L (boolean)
{
  Boolean i = TO_TYPE (obj, Boolean);

  if (i)
    return Qt;
  else
    return Qnil;
}

/********** callback **********/

DEFINE_L2C (callback)
{
  return (XtArgVal) 0;
}

DEFINE_C2L (callback)
{
  return Qnil;
}

/********** enum **********/

typedef struct
{
  Lisp_Object lisp;
  int c;
} xtf_enum_spec;

typedef struct
{
  int num;
  xtf_enum_spec *els;
} xtf_enum;

DEFINE_L2C (enum)
{
  xtf_enum *list = (xtf_enum *) type->data;
  int i;

  for (i=0; i<list->num; i++)
    if (EQ (list->els[i].lisp, obj))
      return (XtArgVal) (list->els[i].c);
  signal_error (Qxt_error, list2
		(build_string
		 (GETTEXT ("Invalid enum value")),
		 obj));
}

DEFINE_C2L (enum)
{
  xtf_enum *list = (xtf_enum *) type->data;
  int i;
  unsigned char c = TO_TYPE (obj, unsigned char);

  for (i=0; i<list->num; i++)
    if (list->els[i].c == c)
      return list->els[i].lisp;
  return Qnil;
}

/********** event **********/

DEFINE_L2C (event)
{
  return (XtArgVal) 0;
}

DEFINE_C2L (event)
{
#if 0
  Lisp_Object event = Fallocate_event ();
  XEvent *xev = (XEvent *) obj;

  x_event_to_emacs_event (XEVENT (event), xev);
  return event;
#endif
  return Qnil;
}

/********** font **********/

DEFINE_L2C (font)
{
  return (XtArgVal) 0;
}

DEFINE_C2L (font)
{
  return Qnil;
}

/********** function **********/

DEFINE_L2C (function)
{
  return (XtArgVal) 0;
}

DEFINE_C2L (function)
{
  return Qnil;
}

/********** int **********/

DEFINE_L2C (int)
{
  CHECK_FIXNUM (obj, 0);
  return (XtArgVal) XINT (obj);
}

DEFINE_C2L (int)
{
  int i;

  i = TO_TYPE (obj, int);
  if (i > MAX_LISP_INT)
    return Qoverflow;
  else if (i < MIN_LISP_INT)
    return Qunderflow;
  else
    return make_number (i);
}

/********** list **********/

typedef struct {
  xtf_type *eltype;
  String resource_num_name;
} xtf_list;

DEFINE_L2C (list)
{
  XtArgVal *new;
  int i, len;
  Lisp_Object lispnew;
  xtf_list *l = (xtf_list *) type->data;
  struct gcpro gcpro1;

  GCPRO1 (lispnew);
  CHECK_LIST (obj, 0);
  len = XINT (Flength (obj)) + (l->resource_num_name ? 0 : 1);
  lispnew = make_opaque (len*sizeof(*new), 0);
  new = (XtArgVal *) opaque_data (lispnew);
  
  for (i=0; CONSP (obj); i++, obj = CDR (obj))
    new[i] = lisp_to_c (CAR (obj), l->eltype, w, ind, slot, i);
  add_lisp_object (w, slot, lispnew);
  add_misc_data (w, slot, (void *) i);
  UNGCPRO;

  return (XtArgVal) new;
}

DEFINE_STAGE2 (list)
{
  STANDARD_STAGE2;

  {
    xtf_list *l = (xtf_list *) type->data;
    if (l->resource_num_name) {
      int num;
      assert (get_misc_data (w, slot, (void **) &num));
      XtVaSetValues (w->widget, l->resource_num_name, num, NULL);
    }
  }
}

DEFINE_C2L (list)
{
  Lisp_Object lisp = Qnil;
  xtf_list *l = (xtf_list *) type->data;
  int num = 0;
  XtArgVal *c = (XtArgVal *) obj;
  int i;
  struct gcpro gcpro1;

  GCPRO1 (lisp);
  if (l->resource_num_name)
    XtVaGetValues (w->widget, l->resource_num_name, &num, NULL);
  for (i=0; num ? i<num : c[i]; i++)
    lisp = Fcons (c_to_lisp (c[i], l->eltype, w, ind, slot, i), lisp);
  lisp = Fnreverse (lisp);
  UNGCPRO;
  return lisp;
}

/********** pixel **********/

DEFINE_L2C (pixel)
{
  CHECK_PIXEL (obj, 0);
  add_lisp_object (w, slot, obj);
  return (XtArgVal) (XPIXEL (obj)->color.pixel);
}

DEFINE_STAGE2 (pixel)
{
  STANDARD_STAGE2;
}

DEFINE_C2L (pixel)
{
  struct screen *s;
  Lisp_Object scr;

  /* It's correct to make a new pixel using make_pixel_internal()
     and leave the hash table alone.  Trust me. */

  s = get_screen_on_screen (XtScreen (w->widget));
  XSETR (scr, Lisp_Screen, s);
  return make_pixel_internal (TO_TYPE (obj, unsigned long), scr);
}

/********** pixmap **********/

DEFINE_L2C (pixmap)
{
  if (NILP (obj))
    return (XtArgVal) 0;
  if (EQ (obj, Qxt_unspecified_pixmap))
    return (XtArgVal) XtUnspecifiedPixmap;
  if (EQ (obj, Qparent_relative))
    return (XtArgVal) ParentRelative;

  CHECK_PIXMAP (obj, 0);
  add_lisp_object (w, slot, obj);
  return (XtArgVal) (XPIXMAP (obj)->pixmap);
}

DEFINE_STAGE2 (pixmap)
{
  STANDARD_STAGE2;
}

DEFINE_C2L (pixmap)
{
  struct screen *s;
  Lisp_Object scr;
  Pixmap p = TO_TYPE (obj, Pixmap);

  /* It's correct to make a new pixmap using make_pixmap_internal()
     and leave the hash table alone.  Trust me. */

  if (!p)
    return Qnil;
  if (p == ParentRelative)
    return Qparent_relative;
  if (p == XtUnspecifiedPixmap)
    return Qxt_unspecified_pixmap;
  s = get_screen_on_screen (XtScreen (w->widget));
  XSETR (scr, Lisp_Screen, s);
  return make_pixmap_internal (p, scr);
}

/********** screen **********/

DEFINE_L2C (screen)
{
  CHECK_SCREEN (obj, 0);
  return (XtArgVal) XtWindow (XSCREEN (obj)->display.x->edit_widget);
}

DEFINE_C2L (screen)
{
  Lisp_Object scr;
  struct screen *s;

  s = x_any_window_to_screen (TO_TYPE (obj, Window));
  if (s) {
    XSETR (scr, Lisp_Screen, s);
    return scr;
  } else
    return Qnil;
}

/********** set **********/

typedef struct
{
  Lisp_Object lisp;
  int c;
  int mask;
} xtf_set_spec;

typedef struct
{
  int num;
  xtf_set_spec *els;
} xtf_set;

DEFINE_L2C (set)
{
  xtf_set *l = (xtf_set *) type->data;
  unsigned char val = 0;
  int i;

  CHECK_LIST (obj, 0);
  for (i=0; i<l->num; i++)
    if (!NILP (Fmemq (l->els[i].lisp, obj)))
      val |= l->els[i].c;

  return (XtArgVal) val;
}

DEFINE_C2L (set)
{
  xtf_set *l = (xtf_set *) type->data;
  int i;
  unsigned char c = TO_TYPE (obj, unsigned char);
  Lisp_Object val = Qnil;

  for (i=0; i<l->num; i++)
    if (c & l->els[i].mask == l->els[i].c)
      val = Fcons (l->els[i].lisp, val);
  return val;
}

/********** short **********/

DEFINE_L2C (short)
{
  int x;

  CHECK_FIXNUM (obj, 0);
  x = XINT (obj);
  if (x > SHRT_MAX || x < SHRT_MIN)
    signal_error (Qxt_error, list3
		  (build_string
		   (GETTEXT ("Value out of range")),
		   build_string ("short"),
		   obj));
  return x;
}

DEFINE_C2L (short)
{
  return make_number (TO_TYPE (obj, short));
}

/********** string **********/

DEFINE_L2C (string)
{
  CHECK_STRING (obj, 0);
  /* #### is this GC safe? */
  return (XtArgVal) (XSTRING (obj)->data);
}

DEFINE_C2L (string)
{
  char *c = (char *) obj;
  /* #### think about this return value */
  return c ? build_string (c) : Qnil;
}

/********** unsigned int **********/

DEFINE_L2C (unsigned_int)
{
  CHECK_NATNUM (obj, 0);
  return (XtArgVal) XFASTINT (obj);
}

DEFINE_C2L (unsigned_int)
{
  Lisp_Object l;
  unsigned int i, j;

  i = TO_TYPE (obj, unsigned int);
  if (i > MAX_LISP_INT)
    return Qoverflow;
  else
    return make_number (i);
}

/********** unsigned short **********/

DEFINE_L2C (unsigned_short)
{
  int x;

  CHECK_FIXNUM (obj, 0);
  x = XINT (obj);
  if (x > USHRT_MAX || x < 0)
    signal_error (Qxt_error, list3
		  (build_string
		   (GETTEXT ("Value out of range")),
		   build_string ("unsigned short"),
		   obj));
  return (XtArgVal) x;
}

DEFINE_C2L (unsigned_short)
{
  return make_number (TO_TYPE (obj, unsigned short));
}

/********** widget **********/

DEFINE_L2C (widget)
{
  return (XtArgVal) 0;
}

DEFINE_C2L (widget)
{
  return Qnil;
}

/**********************************************************************/
/*                             THE ARGLIST                            */
/**********************************************************************/

/* For the moment, all functions requiring an Arg array use the global
   one sitting below.  It's not too pretty, but it works.  Various
   functions are provided to manipulate this array. */

#define XTF_ARGLIST_INC 10

Arg *xtf_arglist;
Cardinal xtf_argcur;
static Cardinal xtf_argmax;

void xtf_clear_arglist (void)
{
  xtf_argcur = 0;
}

void xtf_set_arg (int i, String name, XtArgVal value)
{
  xtf_arglist[i].name = name;
  xtf_arglist[i].value = value;
}

String xtf_get_arg_name (int i)
{
  return xtf_arglist[i].name;
}

XtArgVal xtf_get_arg_value (int i)
{
  return xtf_arglist[i].value;
}

void xtf_add_arg (String name, XtArgVal value)
{
  if (xtf_argcur == xtf_argmax) {
    xtf_arglist = (Arg *) xrealloc (xtf_arglist, sizeof(Arg) *
				    (xtf_argmax + XTF_ARGLIST_INC));
    xtf_argmax += XTF_ARGLIST_INC;
  }
  xtf_set_arg (xtf_argcur, name, value);
  xtf_argcur++;
}

int xtf_find_arg (String name)
{
  int i;

  for (i=0; i<xtf_argcur; i++)
    if (!strcmp(xtf_arglist[i].name, name))
      return i;
  return -1;
}

/**********************************************************************/
/*                      SET-VALUES, GET-VALUES                        */
/**********************************************************************/

#include "xtfunc-def.h"

/* set-values, get-values */

/*
   Determine if `resource' is a resource or resource class in widget class
   `wc'.  If so, return the index of the corresponding xtf_resource_spec
   structure through `index_out'.

   Return value =
   
   0: resource not recognized
   1: simple resource
   2: resource class
*/

int resource_in_resource_list (Lisp_Object resource,
			       struct Lisp_Widget_Class *wc,
			       int *index_out)
{
  int i;

  for (i=0; i<wc->num; i++) {
    xtf_resource_spec *r = &wc->list[i];
    *index_out = i;
    if (EQ (r->lisp_name, resource))
      return 1;
    else if (EQ (r->lisp_class, resource))
      return 2;
  }
  return 0;
}    

/*
   Convert `values' (if type == 1: an alist of (RESOURCE . VALUE)
   pairs, giving resource specifications for widget class `class'; if
   type == 0, a list of RESOURCE symbols for `class') into standard Xt
   arglist/argcount format.  `access' specifies the desired access for
   the resource; an error will be issued if any resource does not
   allow this form of access.  If type == 1, the values specified are
   converted into C form by calling the proper converters.  If type ==
   0, 0 is stored in the `value' field of each Arg.  The results are
   stored in the global arglist. */

void values_to_xt_arglist (Lisp_Object values,
			   int type,
			   struct Lisp_Widget *w,
			   xtf_access access)
{
  struct Lisp_Widget_Class *wc = XWIDGET_CLASS (w->class);

  xtf_clear_arglist();

  for (; CONSP (values); values = CDR (values)) {
    Lisp_Object car = CAR (values);
    Lisp_Object resource;
    int rtype, ind;
    xtf_resource_spec *r;

    if (type) {
      if (! CONSP (car))
	continue;
      resource = CAR (car);
    } else
      resource = car;
    CHECK_SYMBOL (resource, 0);
    rtype = resource_in_resource_list (resource, wc, &ind);
    if (!rtype) {
      Lisp_Object class;

      XSETR (class, Lisp_Widget_Class, wc);
      signal_error (Qxt_error, list3
		    (build_string
		     (GETTEXT ("Invalid resource for widget class")),
		     resource,
		     class));
    }
    r = &wc->list[ind];
    if (! (r->access & access))
      signal_error (Qxt_error, list3
		    (build_string
		     (GETTEXT ("Invalid access for resource")),
		     build_string
		     (access == access_create ? GETTEXT ("create") :
		      access == access_get ? GETTEXT ("get") :
		      GETTEXT ("set")),
		     resource));
    if (rtype == 2 && access == access_get)
      signal_error (Qxt_error, list2
		    (build_string
		     (GETTEXT ("Resource class not allowed in xt-get-values")),
		     resource));
    xtf_add_arg (rtype == 1 ? r->c_name : r->c_class,
		 type ? lisp_to_c (CDR (car), r->type, w, ind, 0, ind) : 0);
  }
}

/*
   Convert the global arglist into an alist of (RESOURCE . VALUE)
   pairs, giving resource specifications for widget class `class'
   (see `values_to_xt_arglist').  C-to-lisp conversion functions
   are called as necessary.  `values' should be the original list
   of resource symbols passed to xt-get-values.
*/

Lisp_Object xt_arglist_to_values (struct Lisp_Widget *w,
				  Lisp_Object values)
{
  int i;
  Lisp_Object values_out = Qnil;
  struct Lisp_Widget_Class *wc = XWIDGET_CLASS (w->class);

  for (i=0; i<xtf_argcur; i++, values = CDR (values)) {
    Lisp_Object resource;
    int rtype, ind;

    if (!CONSP (values))
      abort ();
    resource = CAR (values);
    rtype = resource_in_resource_list (resource, wc, &ind);
    if (rtype != 1)
      abort ();
    values_out = Fcons (Fcons
			(resource,
			 c_to_lisp (xtf_arglist[i].value, wc->list[ind].type,
				    w, ind, 0, ind)),
			values_out);
  }
  values_out = Fnreverse (values_out);
  return values_out;
}

/* The whole stage2 bogosity is to deal with the possibility that the
   user QUITs or errors somewhere in the midst of converting Lisp
   values to C values, before XtSetValues() is called.  During
   conversion, we sometimes need to record new information related to
   the value being converted into a permanent structure (e.g. the size
   of the list being converted, a Lisp_Object to be protected against
   GC).  But we can't unrecord the old values until after
   XtSetValues() is called -- if the user quits and XtSetValues() is
   not called, things must remain the way they were.

   *** The stage2 operations better not quit or error!!! ***
 */

void call_stage2 (Lisp_Object values,
		  struct Lisp_Widget *w)
{
  struct Lisp_Widget_Class *wc = XWIDGET_CLASS (w->class);
  char buf[1000];
  char buf2[50];
  
  for (; CONSP (values); values = CDR (values)) {
    Lisp_Object car = CAR (values);
    int ind;
    xtf_resource_spec *r;
    
    if (! CONSP (car))
      continue;
    resource_in_resource_list (CAR (car), wc, &ind);
    r = &wc->list[ind];
    if (r->type->cl->stage2) {
      char *slot;
      if (r->type->cl->flags.needs_slot) {
	sprintf (buf, ".%d", ind);
	slot = buf;
      } else
	slot = NULL;
      r->type->cl->stage2 (CDR (car), r->type, w, ind, slot);
    }
  }
}

void check_live_widget (struct Lisp_Widget *w)
{
  if (!WIDGET_LIVE_P (w)) {
    Lisp_Object obj;
    XSETR (obj, Lisp_Widget, w);
    signal_error (Qxt_error, list2
		  (build_string
		   (GETTEXT ("Attempt to use dead widget")),
		   obj));
  }
}

/* make sure that OBJ is a live widget and is of the correct class */

void check_valid_widget_type (Lisp_Object obj, Lisp_Object class)
{
  struct Lisp_Widget *w;

  CHECK_WIDGET (obj, 0);
  w = XWIDGET (obj);
  check_live_widget (w);
  if (!EQ (class, w->class))
    signal_error (Qxt_error, list3
		  (build_string
		   (GETTEXT ("Incorrect widget class")),
		   class,
		   obj));
}

DEFUN ("xt-set-values", Fxt_set_values,
       Sxt_set_values, 2, 2, 0,
       "Set the resources of WIDGET according to VALUES.\n\
VALUES is an alist of (NAME . VALUE) pairs, specifying the desired value\n\
of the resource identified by NAME (a symbol).")
     (widget, values)
     Lisp_Object widget, values;
{
  struct Lisp_Widget *w;
  
  CHECK_WIDGET (widget, 0);
  CHECK_LIST (values, 0);

  w = XWIDGET (widget);
  check_live_widget (w);
  values_to_xt_arglist (values, 1, w, access_set);
  if (XWIDGET_CLASS (w->class)->special_func)
    XWIDGET_CLASS (w->class)->special_func (widget, &values, access_set);
  BLOCK_INPUT;
  XtSetValues (w->widget, xtf_arglist, xtf_argcur);
  UNBLOCK_INPUT;
  call_stage2 (values, w);

  return Qnil;
}

DEFUN ("xt-get-values", Fxt_get_values,
       Sxt_get_values, 2, 2, "",
       "Retrieve the resources for WIDGET that are specified in VALUES.\n\
VALUES is a list of symbols, each specifying a resource for which the\n\
current value is desired.  An alist of (NAME . VALUE) pairs is returned,\n\
one for each symbol specified in VALUES.")
     (widget, values)
     Lisp_Object widget, values;
{
  struct Lisp_Widget *w;
  static XtArgVal *aux;
  static Cardinal auxmax;
  int i;

  CHECK_WIDGET (widget, 0);
  CHECK_LIST (values, 0);

  w = XWIDGET (widget);
  check_live_widget (w);
  values_to_xt_arglist (values, 0, w, access_get);

  /* XtGetValues() expects that a pointer is placed in the `value' field
     of Arg, and stores the actual value of the resource into the place
     pointed to by the pointer.  This is a little inconvenient for us.
     We allocate an array of XtArgVals, point the global arglist there,
     then undo this after the call.
  */

  if (xtf_argcur > auxmax) {
    aux = (XtArgVal *) xrealloc (aux, xtf_argcur * sizeof(*aux));
    auxmax = xtf_argcur;
  }
  for (i=0; i<xtf_argcur; i++) {
    aux[i] = 0;
    xtf_arglist[i].value = (XtArgVal) (aux+i);
  }
  BLOCK_INPUT;
  XtGetValues (w->widget, xtf_arglist, xtf_argcur);
  UNBLOCK_INPUT;
  for (i=0; i<xtf_argcur; i++)
    xtf_arglist[i].value = aux[i];

  {
    Lisp_Object values_out;

    values_out = xt_arglist_to_values (w, values);
    if (XWIDGET_CLASS (w->class)->special_func)
      XWIDGET_CLASS (w->class)->special_func (widget,
					      &values_out, access_get);
    return values_out;
  }  
}

#ifdef DEBUG_XTFUNC

DEFUN ("make-core", Fmake_core,
       Smake_core, 0, 0, "",
       "Temporary debugging function.  Make a Core widget.")
     ()
{
  char buffer[100];
  static int off = 0;
  Widget w;
  Lisp_Object class;

  sprintf (buffer, "foofoo%d", off++);
  w = XtVaCreateWidget (buffer, coreWidgetClass,
			selected_screen->display.x->edit_widget,
			XtNwidth, 500, XtNheight, 300, XtNx, 10, XtNy, 66,
			NULL);
  XSETR (class, Lisp_Widget_Class, xxxx_class_Core);
  return make_widget (w, class, Qnil);
}

static void printmapfn (CONST void* key, void* contents, void* arg)
{
    Lisp_Object obj;

    printf ("slot: %s\n", key);
    VOID_TO_LISP (obj, contents);
    debug_print (obj);
}

void printhash (c_hashtable h)
{
    maphash (printmapfn, h, 0);
}

#endif

void
init_xtfunc()
{
  xtf_widget_table = make_hashtable (100);
  xtf_string_table = make_strings_hashtable (500);
  xtf_string_array = (char *) malloc (32);
  xtf_string_array_size = 32;
}

void
syms_of_xtfunc()
{
  Lisp_Object classobj;

  defsymbol (&Qwidgetp, "widgetp");
  defsubr (&Swidgetp);
  defsubr (&Slive_widgetp);
  defsubr (&Swidget_name);
  defsubr (&Swidget_widget_class);
  defsubr (&Swidget_parent);  

  defsymbol (&Qwidget_classp, "widget-classp");
  defsubr (&Swidget_classp);
  defsubr (&Swidget_class_name);
  defsubr (&Swidget_class_superclass);
  defsubr (&Swidget_class_resources);
  
  defsubr (&Sxt_set_values);
  defsubr (&Sxt_get_values);

#ifdef DEBUG_XTFUNC
  defsubr (&Smake_core);
#endif

  defsymbol (&Qoverflow, "overflow");
  defsymbol (&Qunderflow, "underflow");

  defsymbol (&Qxt_error, "xt-error");

  defsymbol (&Qparent_relative, "parent-relative");
  defsymbol (&Qxt_unspecified_pixmap, "xt-unspecified-pixmap");


#include "xtfunc-sym.h"

  pure_put (Qxt_error, Qerror_conditions,
	    list2 (Qxt_error, Qerror));
  pure_put (Qxt_error, Qerror_message,
	    build_string (DEFER_GETTEXT ("Xt error")));
}

