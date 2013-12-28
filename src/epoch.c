/* Epoch functionality.
   Copyright (C) 1985-1994 Free Software Foundation.

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

#include "config.h"
#include "lisp.h"
#include "xterm.h"
#include "dispextern.h"
#include "screen.h"
#include "xobjs.h"
#include "events.h"

Lisp_Object
make_xresource (XID xid, Atom type)
{
  struct Lisp_X_Resource *xr =
    alloc_lcrecord (sizeof (struct Lisp_X_Resource), lrecord_x_resource);
  Lisp_Object val;

  xr->xid = xid;
  xr->type = type;
  XSETR (val, Lisp_X_Resource, xr);

  return val;
}

/*
 * Epoch equivalent:  epoch::intern-atom
 */
DEFUN ("x-intern-atom", Fx_intern_atom, Sx_intern_atom, 1, 1, 0,
 "Convert a STRING or SYMBOL into an atom and return as an XRESOURCE.")
     (name)
     Lisp_Object name;
{
  Atom atom;
  char *data;

  if (SYMBOLP (name))
    data = (char *) XSYMBOL (name)->name->data;
  else
    {
      CHECK_STRING (name, 0);
      data = (char *) XSTRING (name)->data;
    }

  BLOCK_INPUT;
  atom = XInternAtom (x_current_display, data, False);
  UNBLOCK_INPUT;

  return make_xresource (atom, XA_ATOM);
}

/*
 * Epoch equivalent:  epoch::unintern-atom
 */
DEFUN ("x-atom-name", Fx_atom_name, Sx_atom_name, 1, 1, 0,
  "Return the name of an X atom resource as a string.")
     (atom)
     Lisp_Object atom;
{
  char *atom_name;
  Lisp_Object val;

  CHECK_XRESOURCE (atom, 0);
  if (XXRESOURCE (atom)->type != XA_ATOM)
    error ("Resource is not an atom");

  BLOCK_INPUT;
  atom_name = XGetAtomName (x_current_display, XXRESOURCE (atom)->xid);
  UNBLOCK_INPUT;

  if (atom_name)
    {
      val = build_string (atom_name);
      xfree (atom_name);
    }
  else
    val = Qnil;

  return val;
}

/*
 * Epoch equivalent:  epoch::string-to-resource
 */
DEFUN ("x-string-to-x-resource", Fx_string_to_x_resource,
       Sx_string_to_x_resource, 2, 3, 0,
  "Convert a numeric STRING to an XRESOURCE.\n\
STRING is assumed to represent a 32-bit numer value. XRESOURCE must be\n\
an X atom.  Optional BASE argument should be a number between 2 and 36,\n\
specifying the base for converting STRING.")
     (string, type, base)
     Lisp_Object string, type, base;
{
  XID xid;
  struct Lisp_X_Resource *xr;
  char *ptr;
  int b;

  CHECK_STRING (string, 0);
  CHECK_XRESOURCE (type, 0);

  if (EQ (base, Qnil))
    b = 0;
  else
    {
      CHECK_FIXNUM (base, 0);
      b = XUINT(base);
      if (b < 2 || b > 36) args_out_of_range_3 (base, make_number (2),
						make_number (36));
    }

  if (XXRESOURCE (type)->type != XA_ATOM)
    error ("Resource must be an atom");
  xr = XXRESOURCE (type);

  xid = (XID) strtol ((CONST char*) XSTRING (string)->data, &ptr, b);

  return ((ptr == (char *) XSTRING (string)->data)
	  ? Qnil
	  : make_xresource (xid, xr->xid));
}

/*
 * Epoch equivalent:  epoch::resource-to-type
 */
DEFUN ("x-resource-to-type", Fx_resource_to_type, Sx_resource_to_type, 1, 1, 0,
  "Return an x-resource of type ATOM whose value is the type of the argument")
     (resource)
     Lisp_Object resource;
{
  struct Lisp_X_Resource *xr;

  CHECK_XRESOURCE (resource, 0);
  xr = XXRESOURCE (resource);

  return make_xresource (xr->type, XA_ATOM);
}

/* internal crap stolen from Epoch */
static char LongToStringBuffer[33]; /* can't have statics inside functions! */
char *
long_to_string (unsigned long n, unsigned int base)
{
  char *digit = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  char *s = LongToStringBuffer + 32; /* at most 33 characters in binary */

  *s = 0;			/* terminate */
  while (n)			/* something there */
    {
    *--s = digit[n % base];		/* store bottom digit */
    n /= base;			/* shift right */
    }
  if (*s == 0) *--s = '0';		/* in case nothing was put in string */
  return s;
}

/*
 * Epoch equivalent:  epoch::resource-to-string
 */
DEFUN ("x-resource-to-string", Fx_resource_to_string, Sx_resource_to_string,
       1, 2, 0,
  "Convert the xid of RESOURCE to a numeric string.\n\
Optional BASE specifies the base for the conversion (2..36 inclusive)")
     (resource, base)
     Lisp_Object resource, base;
{
  int cbase = 10;

  CHECK_XRESOURCE (resource, 0);
  if (!NILP (base))
    {
      CHECK_FIXNUM (base, 0);
      cbase = XUINT (base);
      if ((cbase < 2) || (cbase > 36))
	args_out_of_range_3 (base, make_number (2), make_number (36));
    }

  return build_string (long_to_string (XXRESOURCE (resource)->xid, cbase));
}

/*
 * Epoch equivalent:  epoch::xid-of-screen
 *
 * This differs from x-window-id in xfns.c in that its return value is an
 * x-resource rather than a string.
 */
DEFUN ("x-id-of-screen", Fx_id_of_screen, Sx_id_of_screen, 0, 1, 0,
  "Return ID of SCREEN as an x-resource, or nil on error.")
     (screen)
     Lisp_Object screen;
{
  if (NILP (screen))
    screen = Fselected_screen();

  if (NILP (screen))
    return Qnil;

  return make_xresource (XtWindow (XSCREEN (screen)->display.x->widget),
			 XA_WINDOW);
}

/*
 * Epoch equivalent:  epoch::query-tree
*/
DEFUN ("x-query-tree", Fx_query_tree, Sx_query_tree, 0, 1, 0,
  "Return the portion of the window tree adjacent to SCREEN.\n\
Return value is the list ( ROOT PARENT . CHILDREN ).  The SCREEN arg\n\
can either be a screen object or an x-resource of type window.")
     (screen)
     Lisp_Object screen;
{
  Window win;
  Window root, parent, *children;
  unsigned int count;
  int retval;
  Lisp_Object val;

  if (XRESOURCEP (screen))
    {
      if (XXRESOURCE (screen)->type != XA_WINDOW)
	error ("Screen resource must be of type WINDOW");
      win = XXRESOURCE (screen)->xid;
    }
  else
    {
      if (NILP (screen))
	screen = Fselected_screen();
      if (NILP (screen))
	return Qnil;
      win = XXRESOURCE (Fx_id_of_screen (screen))->xid;
    }

  BLOCK_INPUT;
  retval =
    XQueryTree (x_current_display, win, &root, &parent, &children, &count);
  UNBLOCK_INPUT;

  /* Thank you, X-Consortium. XQueryTree doesn't return Success like everyone
   * else, it returns 1. (Success is defined to be 0 in the standard header
   * files)
   */
  if (!retval) return Qnil;

  val = Qnil;
  while (count)
    val = Fcons (make_xresource (children[--count], XA_WINDOW), val);

  xfree (children);

  return Fcons (make_xresource (root, XA_WINDOW),
		Fcons ((parent
			? make_xresource (parent, XA_WINDOW)
			: Qnil),
		       val));
}

/* more internal crap stolen from Epoch */

static void
verify_vector_has_consistent_type (Lisp_Object vector)
{
  int i;			/* vector index */
  XID rtype;			/* Xresource type (if vector of Xresources) */
  int length;			/* vector length */
  struct Lisp_Vector *v = XVECTOR (vector);
  Lisp_Object *element;
  Lisp_Object sample;
  Lisp_Object type_obj;		/* base type of vector elements */

  sample = v->contents[0];
  type_obj = sample;
  if (XRESOURCEP (sample))
    rtype = XXRESOURCE (sample)->type;
  length = v->size;
  element = v->contents;

  for (i = 1; i < length; ++i, ++element)
    {
      QUIT;
      if ((XTYPE (*element) != XTYPE (type_obj))
	  || (LRECORDP (type_obj) &&
	      (XRECORD_LHEADER (*element)->implementation !=
	       XRECORD_LHEADER (type_obj)->implementation))
	  || (XRESOURCEP (type_obj) && rtype != XXRESOURCE(*element)->type))
	error("Vector has inconsistent types");
    }
}

static void
verify_list_has_consistent_type (Lisp_Object list)
{
  Lisp_Object type_obj;
  XID rtype;			/* Xresource type (if vector of Xresources) */
  Lisp_Object temp = Fcar(list);

  type_obj = temp;
  if (XRESOURCEP(temp))
    rtype = XXRESOURCE(temp)->type;
  list = Fcdr(list);

  for ( ; !NILP(list) ; list = Fcdr(list))
    {
      QUIT;
      temp = Fcar(list);
      if ((XTYPE (temp) != XTYPE (type_obj))
	  || (LRECORDP (type_obj) &&
	      (XRECORD_LHEADER (temp)->implementation !=
	       XRECORD_LHEADER (type_obj)->implementation))
	  || (XRESOURCEP (type_obj) && rtype != XXRESOURCE(temp)->type))
	error("List has inconsistent types");
    }
}

#define BYTESIZE 8
/* 16 bit types */
typedef short int int16;
typedef short unsigned int uint16;

/* the Calculate functions return allocated memory that must be free'd.
   I tried to use alloca, but that fails. Sigh.
*/
void *
calculate_vector_property (Lisp_Object vector, unsigned long *count,
			   Atom *type, int *format)
{
  int length;
  unsigned int size,tsize;
  int i;
  struct Lisp_Vector *v;
  void *addr;

  v = XVECTOR(vector);
  *count = length = v->size;

  switch (XTYPE(v->contents[0]))
    {
    case Lisp_Int:
      *type = XA_INTEGER;
      if (*format != 8 && *format != 16) *format = 32;
      size = *format * length;
      addr = (void *) xmalloc(size);
      for ( i = 0 ; i < length ; ++i )
	switch (*format)
	  {
	  case 32 : ((int *)addr)[i] = XINT(v->contents[i]); break;
	  case 16 : ((int16 *)addr)[i] = XINT(v->contents[i]); break;
	  case 8 : ((char *)addr)[i] = XINT(v->contents[i]); break;
	  }
      break;

    case Lisp_Record:
      if (XRESOURCEP (v->contents[0]))
	{
	  size = BYTESIZE * sizeof(XID) * length;
	  *format = BYTESIZE * sizeof(XID);
	  *type = XXRESOURCE(v->contents[0])->type;
	  addr = (void *) xmalloc(size);
	  for ( i = 0 ; i < length ; ++i )
	    ( (XID *) addr) [i] = XXRESOURCE(v->contents[i])->xid;
	}
      break;

    case Lisp_String:
      *format = BYTESIZE * sizeof(char);
      *type = XA_STRING;
      for ( i=0, size=0 ; i < length ; ++i )
	size += XSTRING(v->contents[i])->size + 1; /* include null */
      addr = (void *) xmalloc(size);
      *count = size;
      for ( i = 0 , size = 0 ; i < length ; ++i )
	{
	  tsize = XSTRING(v->contents[i])->size + 1;
	  memmove (((char *) addr), XSTRING (v->contents[i])->data, tsize);
	  size += tsize;
	}
      break;

    default:
      error("Invalid type for conversion");
    }
  return addr;
}

void *
calculate_list_property (Lisp_Object list, unsigned long *count,
			 Atom *type, int *format)
{
  int length;
  unsigned int size, tsize;
  int i;
  Lisp_Object tlist,temp;
  void *addr;

  *count = length = XINT (Flength(list));

  switch (XTYPE(Fcar(list)))
    {
    case Lisp_Int:
      *type = XA_INTEGER;
      if (*format != 8 && *format != 16) *format = 32;
      size = *format * length;
      addr = (void *) xmalloc(size);
      for ( i = 0 ; i < length ; ++i, list = Fcdr(list))
	switch (*format)
	  {
	  case 32 : ((int *)addr)[i] = XINT(Fcar(list)); break;
	  case 16 : ((int16 *)addr)[i] = XINT(Fcar(list)); break;
	  case 8 : ((char *)addr)[i] = XINT(Fcar(list)); break;
	  }
      break;

    case Lisp_Record:
      if (XRESOURCEP (Fcar (list)))
	{
	  size = BYTESIZE * sizeof(XID) * length;
	  *format = BYTESIZE * sizeof(XID);
	  *type = XXRESOURCE(Fcar(list))->type;
	  addr = (void *) xmalloc(size);
	  for ( i = 0 ; i < length ; ++i, list = Fcdr(list))
	    ((XID *)addr)[i] = XXRESOURCE(Fcar(list))->xid;
	}
      break;

    case Lisp_String:
      *format = BYTESIZE * sizeof(char);
      *type = XA_STRING;
      for ( i=0, size=0 , tlist=list ; i < length ; ++i, tlist = Fcdr(tlist) )
	size += XSTRING(Fcar(tlist))->size + 1; /* include null */
      addr = (void *) xmalloc(size);
      *count = size;
      for ( i=0, size=0, tlist=list ; i < length  ; ++i , tlist = Fcdr(tlist) )
	{
	  temp = Fcar(tlist);
	  tsize = XSTRING(temp)->size + 1;
	  memmove (((char *) addr), XSTRING (temp)->data, tsize);
	  size += tsize;
	}
      break;

    default:
      error("Invalid type for conversion");
    }
  return addr;
}

/* Returns whether the conversion was successful or not */
static int
convert_elisp_to_x (Lisp_Object value, void **addr, unsigned long *count,
		    Atom *type, int *format, int *free_storage)
{
  if (VECTORP(value))
    verify_vector_has_consistent_type (value);
  else if (CONSP(value))
    verify_list_has_consistent_type (value);

  *free_storage = 0;
  switch (XTYPE(value))
    {
    case Lisp_String:
      *format = BYTESIZE;
      *type = XA_STRING;
      *count = strlen((CONST char *) XSTRING(value)->data)+1;
      *addr = (void *) XSTRING(value)->data;
      break;

    case Lisp_Int:
      *type = XA_INTEGER;
      *count = 1;
      *free_storage = 1;
      *addr = (void *) xmalloc(sizeof(int));
      /* This is ugly -
       * we have to deal with the possibility of different formats
       */
      switch (*format)
	{
	default :
	case 32 : *format = 32; *((int *)(*addr)) = XINT(value); break;
	case 16 : *((int16 *)(*addr)) = XINT(value); break;
	case 8 :  *((char *)(*addr)) = XINT(value); break;
	}
      break;

    case Lisp_Record:
      if (XRESOURCEP (value))
	{
	  *format = sizeof(XID) * BYTESIZE;
	  *type = XXRESOURCE(value)->type;
	  *count = 1;
	  *addr = (void *) & (XXRESOURCE(value)->xid);
	}
      break;

    case Lisp_Cons:
      *addr = calculate_list_property (value,count,type,format);
      *free_storage = 1;	/* above allocates storage */
      break;

    case Lisp_Vector:
      *addr = calculate_vector_property (value,count,type,format);
      *free_storage = 1;	/* above allocates storage */
      break;

    default :
      error("Improper type for conversion");
    }

  return 1;
}

static Lisp_Object
format_size_hints (XSizeHints *hints)
{
  Lisp_Object result;
  struct Lisp_Vector *v;

  result = Fmake_vector (make_number(6), Qnil);
  v = XVECTOR(result);

  /* ugly but straightforward - just step through the members and flags
   * and stick in the ones that are there
   */
  if (hints->flags & (PPosition|USPosition))
    v->contents[0] = Fcons(make_number(hints->x),make_number(hints->y));
  if (hints->flags & (PSize|USSize))
    v->contents[1] = Fcons(make_number(hints->width),
			   make_number(hints->height));
  if (hints->flags & PMinSize)
    v->contents[2] = Fcons(make_number(hints->min_width),
			   make_number(hints->min_height));
  if (hints->flags & PMaxSize)
    v->contents[3] = Fcons(make_number(hints->max_width),
			   make_number(hints->max_height));
  if (hints->flags & PResizeInc)
        v->contents[4] = Fcons(make_number(hints->width_inc),
                               make_number(hints->height_inc));
  if (hints->flags & PAspect)
    v->contents[5] = Fcons(make_number(hints->min_aspect.x),
			   Fcons(make_number(hints->min_aspect.y),
				 Fcons(make_number(hints->max_aspect.x),
				       make_number(hints->max_aspect.y))));

  return result;
}

static Lisp_Object
format_string_property (char *buffer, unsigned long count)
{
  Lisp_Object value = Qnil;		/* data */
  Lisp_Object temp;			/* temp value holder */
  int len;				/* length of current string */
  char *strend;

  while (count)
    {
      strend = memchr (buffer, 0, (int) count);
      len = strend ? strend - buffer : count;
      if (len)
	{
	  temp = make_string (buffer, len);
	  value = Fcons (temp, value);
	}
      buffer = strend + 1;	/* skip null, or leaving loop if no null */
      count -= len + !!strend;
    }

  return (NILP (Fcdr (value))
	  ? Fcar (value)
	  : Fnreverse (value));
}

static Lisp_Object
format_integer_32_property (long *buff, unsigned long count)
{
  Lisp_Object value = Qnil;	/* return value */
  while (count)
    value = Fcons (make_number(buff[--count]), value);

  return (NILP (Fcdr(value))
	  ? Fcar (value)
	  : value);
}

static Lisp_Object
format_integer_16_property (int16 *buff, unsigned long count)
{
  Lisp_Object value = Qnil;	/* return value */

  while (count)
    value = Fcons (make_number(buff[--count]), value);

  return (NILP (Fcdr (value))
	  ? Fcar (value)
	  : value);
}

static Lisp_Object
format_integer_8_property (char *buff, unsigned long count)
{
  Lisp_Object value = Qnil;	/* return value */

  while (count)
    value = Fcons (make_number(buff[--count]), value);

  return (NILP (Fcdr (value))
	  ? Fcar (value)
	  : value);
}

static Lisp_Object
format_integer_property (void *buff, unsigned long count, int format)
{
  switch (format)
    {
    case 8:
      return format_integer_8_property ((char *) buff, count);
      break;
    case 16:
      return format_integer_16_property ((int16 *) buff, count);
      break;
    case 32:
      return format_integer_32_property ((long *) buff, count);
      break;
    default:
      return Qnil;
    }
}

static Lisp_Object
format_cardinal_32_property (unsigned long *buff, unsigned long count)
{
  Lisp_Object value = Qnil;	/* return value */

  while (count)
    value = Fcons (make_number(buff[--count]), value);

  return (NILP (Fcdr(value))
	  ? Fcar (value)
	  : value);
}

static Lisp_Object
format_cardinal_16_property (uint16 *buff, unsigned long count)
{
  Lisp_Object value = Qnil;	/* return value */

  while (count)
    value = Fcons (make_number(buff[--count]), value);

  return (NILP (Fcdr (value))
	  ? Fcar (value)
	  : value);
}

static Lisp_Object
format_cardinal_8_property (unsigned char *buff, unsigned long count)
{
  Lisp_Object value = Qnil;	/* return value */

  while (count)
    value = Fcons (make_number(buff[--count]), value);

  return (NILP (Fcdr (value))
	  ? Fcar (value)
	  : value);
}

static Lisp_Object
format_cardinal_property (void *buff, unsigned long count, int format)
{
  switch (format)
    {
    case 8:
      return format_cardinal_8_property ((unsigned char *) buff, count);
      break;
    case 16:
      return format_cardinal_16_property ((uint16 *) buff, count);
      break;
    case 32:
      return format_cardinal_32_property ((unsigned long *) buff, count);
    default:
      return Qnil;
    }
}

static Lisp_Object
format_unknown_property (void *buff, unsigned long count, Atom type,
			 int format)
{
  Lisp_Object value = Qnil;	/* return value */

  switch (format)
    {
    case 32:
      {
	XID *xid = (XID *) buff;
	int non_zero = 0;
	while (count--)
	  if (non_zero || xid[count])
	    {
	      value = Fcons (make_xresource (xid[count],type), value);
	      non_zero = 1;
	    }
      }
      break;
    }

  return (NILP (Fcdr(value))
	  ? Fcar (value)
	  : value);
}

static Lisp_Object
convert_x_to_elisp (void *buffer, unsigned long count, Atom type, int format)
{
  Lisp_Object value = Qnil;

  switch (type)
    {
    case None:
      value = Qnil;
      break;
    case XA_STRING:
      value = format_string_property (buffer, count);
      break;
    case XA_INTEGER:
      value = format_integer_property ((long *) buffer, count, format);
      break;
    case XA_CARDINAL:
      value = format_cardinal_property ((unsigned long *) buffer,
					count, format);
      break;
    case XA_WM_SIZE_HINTS:
      value = format_size_hints ((XSizeHints *) buffer);
      break;
    default:
      value = format_unknown_property ((void *) buffer, count, type, format);
      break;
    }

  return value;
}

/* get a property given its atom, display, and window */
Lisp_Object
static raw_get_property (Display *dpy, Window win, Atom prop)
{
  Lisp_Object value = Qnil;
  Atom actual_type;
  int actual_format;
  unsigned char *buffer;
  unsigned long count, remaining;
  int zret;

  BLOCK_INPUT;
  zret = XGetWindowProperty (dpy, win, prop,
			     0L, 1024L, False, AnyPropertyType,
			     &actual_type, &actual_format,
			     &count, &remaining, &buffer);

  /* If remaining is set, then there's more of the property to get.
     Let's just do the whole read again, this time with enough space
     to get it all. */
  if (zret == Success && remaining > 0)
    {
      xfree (buffer);
      zret = XGetWindowProperty (dpy, win, prop,
				 0L, 1024L + ((remaining + 3) / 4),
				 False, AnyPropertyType,
				 &actual_type, &actual_format,
				 &count, &remaining, &buffer);
    }
  UNBLOCK_INPUT;

  if (zret != Success)
    return Qnil;		/* failed */

  value = convert_x_to_elisp (buffer, count, actual_type, actual_format);

  xfree (buffer);
  return value;
}

/*
 * Epoch equivalent:  epoch::get-property
 */
DEFUN ("x-get-property", Fx_get_property, Sx_get_property, 1, 2, 0,
       "Retrieve the X window property for a screen. Arguments are\n\
PROPERTY: must be a string or an X-resource of type ATOM.\n\
SCREEN: (optional) If present, must be a screen object, a screen id, or\n\
and X-resource of type WINDOW. Defaults to the current screen.\n\
Returns the value of the property, or nil if the property couldn't\n\
be retrieved.")
     (name, screen)
     Lisp_Object name, screen;
{
  Atom prop = None;
  Display *dpy = x_current_display;
  Window win;
  
  if (XRESOURCEP (screen))
    {
      if (XXRESOURCE (screen)->type != XA_WINDOW)
	error("Screen resource must be of type WINDOW");
      win = XXRESOURCE(screen)->xid;
    }
  else
    {
      if (NILP (screen))
	screen = Fselected_screen();

      if (NILP (screen))
	return Qnil;

      /* We can't use Fx_id_of_screen because it returns the xid of
	 the shell widget.  But the property change has to take place
	 on the edit widget in order for a PropertyNotify event to
	 be generated */
      win = XtWindow (XSCREEN (screen)->display.x->edit_widget);
#if 0
      win = XXRESOURCE (Fx_id_of_screen (screen))->xid;
#endif
    }

  if (XTYPE(name) == Lisp_String)
    {
      BLOCK_INPUT;
      prop = XInternAtom (dpy, XSTRING(name)->data, True);
      UNBLOCK_INPUT;
    }
  else if (XRESOURCEP(name))
    {
      if (XXRESOURCE(name)->type != XA_ATOM)
	error ("Property must be an ATOM X-resource");
      prop = XXRESOURCE(name)->xid;
    }
  else
    error ("Property must be a string or X-resource ATOM");

  if (prop == None)
    return Qnil;

  /* now we have the atom, let's ask for the property! */
  return raw_get_property (dpy,win,prop);
}

static Lisp_Object
raw_set_property (Display *dpy, Window win, Atom prop, Lisp_Object value)
{
  Atom actual_type;		/* X type of items */
  int actual_format;		/* size of data items (8,16,32) */
  unsigned long count;		/* Number of data items */
  void* addr;			/* address of data item array */
  int zret;			/* X call return value */
  int free_storage;		/* set if addr points at non-malloc'd store */

  actual_format = 0;		/* don't force a particular format */
  convert_elisp_to_x (value, &addr, &count, &actual_type, &actual_format,
		      &free_storage);

  BLOCK_INPUT;
  zret = XChangeProperty (dpy, win, prop, actual_type, actual_format,
			  PropModeReplace, (char *) addr, count);
  XFlush (dpy);
  UNBLOCK_INPUT;

  if (free_storage)
    xfree (addr);

  return value;
}

DEFUN ("x-set-property", Fx_set_property, Sx_set_property, 2, 3, 0,
      "Set a named property for a screen. The first argument (required)\n\
is the name of the property. The second is the value to set the propery\n\
to. The third (optional) is the screen, default is\n\
the current screen.")
     (name, value, screen)
     Lisp_Object name, value, screen;
{
  Atom prop = None;		/* name of the property */
  Window win;			/* window to put property on */
  Display *dpy = x_current_display;		/* display for window */

  if (XRESOURCEP (screen))
    {
      if (XXRESOURCE(screen)->type != XA_WINDOW)
	error ("Screen resource must be of type WINDOW");
      win = XXRESOURCE(screen)->xid;
    }
  else
    {
      if (NILP (screen))
	screen = Fselected_screen();

      if (NILP (screen))
	return Qnil;

      /* We can't use Fx_id_of_screen because it returns the xid of
	 the shell widget.  But the property change has to take place
	 on the edit widget in order for a PropertyNotify event to
	 be generated */
      win = XtWindow (XSCREEN (screen)->display.x->edit_widget);
#if 0
      win = XXRESOURCE (Fx_id_of_screen (screen))->xid;
#endif
    }

  /* parse the atom name, either a string or an actual atom */
  if (XTYPE(name) == Lisp_String)
    {
      BLOCK_INPUT;
      prop = XInternAtom (dpy, XSTRING(name)->data, False);
      UNBLOCK_INPUT;
    }
  else if (XRESOURCEP(name))
    {
      if (XXRESOURCE(name)->type != XA_ATOM)
	error("Property must be an X-resource ATOM");
      prop = XXRESOURCE(name)->xid;
    }
  else
    error("Property must be a string or X-resource ATOM");

  if (prop == None)
    return Qnil;

  /* that's it. Now set it */
  return raw_set_property (dpy, win, prop, value);
}

/*
 * Epoch equivalent:  epoch::send-client-message
 */
DEFUN ("x-send-client-message", Fx_send_client_message, Sx_send_client_message,
       1, 5, 0,
  "Send a client message to DEST, marking it as being from SOURCE.\n\
The message is DATA of TYPE with FORMAT.  If TYPE and FORMAT are omitted,\n\
they are deduced from DATA.  If SOURCE is nil, the current screen is used.")
     (dest, source, data, type, format)
     Lisp_Object dest, source, data, type, format;
{
  int actual_format = 0;
  Atom actual_type;
  unsigned long count;
  void *addr;
  int free_storage;
  XEvent ev;
  struct Lisp_X_Resource *xr;
  Lisp_Object result;

  /* find our destination first */
  if (XRESOURCEP (dest))
    {
      if (XXRESOURCE (dest)->type == XA_WINDOW)
	xr = XXRESOURCE (dest);
      else
	error ("Argument must be a screen or x-window-resource");
    }
  else
    {
      if (NILP (dest))
	dest = Fselected_screen();
      if (NILP (dest))
	return Qnil;
      else if (!SCREENP (dest))
	error ("Argument must be a screen or x-window-resource");
      else
	xr = XXRESOURCE (Fx_id_of_screen (dest));
    }

  /* find our source - all we need from this is the window id */
  if (XRESOURCEP (source))
    {
      if (XXRESOURCE (source)->type != XA_WINDOW)
	error("X-resource must be a WINDOW");
      ev.xclient.window = XXRESOURCE(source)->xid;
    }
  else
    {
      if (NILP (source))
	source = Fselected_screen();

      CHECK_SCREEN (source, 0);
      ev.xclient.window = XXRESOURCE (Fx_id_of_screen (source))->xid;
    }

  /* check format before data, because it can cause the data format to vary */
  if (!NILP (format))
    {
      CHECK_NUMBER(format, 0);
      actual_format = XINT (format);
      if (actual_format != 8 && actual_format != 16 && actual_format != 32)
	error ("Format must be 8, 16, or 32, or nil");
    }

  /* clear out any cruft */
  memset ((char *) &ev.xclient.data, 0, 20);

  /* look for the data */
  if (!NILP (data))
    {
      convert_elisp_to_x (data, &addr, &count, &actual_type, &actual_format,
			  &free_storage);
      if ((count * actual_format) > 20*8)
	{
	  if (free_storage)
	    xfree(addr);
	  error("Data is too big to fit in a client message");
	}
      memmove (&ev.xclient.data, (char *)addr, count * (actual_format/8));
      if (free_storage)
	xfree(addr);
    }

  if (!NILP (type))
    {
      CHECK_XRESOURCE (type,0);
      if (XXRESOURCE(type)->type != XA_ATOM)
        error("Resource for message type must be an atom");
      actual_type = XXRESOURCE(type)->xid;
    }
      
  ev.xany.type = ClientMessage;
  ev.xclient.message_type = actual_type;
  ev.xclient.format = actual_format;
  /* There's no better way to set the mask than to hard code the correct
   * width bit pattern. 1L<<24 == OwnerGrabButtonMask, is the largest
   * This is the word from the X-consortium.
   */
  result = (XSendEvent (x_current_display, xr->xid, False, (1L<<25)-1L,&ev)
	    ? Qt
	    : Qnil);
  XFlush (x_current_display);
  return result;
}

/*
 * These duplicate the needed functionality from the Epoch event handler.
 */
static Lisp_Object
read_client_message (XClientMessageEvent *cm)
{
  Lisp_Object result;

  result = Fcons (make_xresource (cm->message_type, XA_ATOM),
		  Fcons (make_xresource (cm->window, XA_WINDOW),
			 convert_x_to_elisp ((void *) cm->data.b,
					     (20*8)/cm->format,
					     cm->message_type,
					     cm->format)));

  return result;
}

static Lisp_Object
read_property_event (XPropertyEvent *pe, Lisp_Object screen)
{
  Lisp_Object result, value;
  struct screen *s = XSCREEN (screen);
  Display *dpy = XtDisplay (s->display.x->widget);
  char *atom_name;

  BLOCK_INPUT;
  atom_name = XGetAtomName (dpy, pe->atom);
  UNBLOCK_INPUT;

  /* didn't get a name, blow this one off */
  if (atom_name == (char *) 0)
    return Qnil;

  /* We can't use Fx_id_of_screen because it returns the xid of
     the shell widget.  But the property change has to take place
     on the edit widget in order for a PropertyNotify event to
     be generated */
  value = raw_get_property (dpy, XtWindow (s->display.x->edit_widget),
			    pe->atom);
  result = Fcons (build_string (atom_name), value);

  xfree (atom_name);

  return result;
}

void
dispatch_epoch_event (struct Lisp_Event *emacs_event, Lisp_Object type)
{
  XEvent *event;
  struct Lisp_Vector *evp;
  struct screen *s;

  switch (emacs_event->event_type)
    {
    case eval_event:
      if ((!EQ (type, Qx_map) && !EQ (type, Qx_unmap))
	  || NILP (emacs_event->event.eval.object))
	{
	  Vepoch_event = Qnil;
	  emacs_event->epoch_event = Qnil;
	  return;
	}
      s = XSCREEN (emacs_event->event.eval.object);
      break;
    case magic_event:
      event = (XEvent *) &emacs_event->event.magic.underlying_event;
      BLOCK_INPUT;
      s = x_any_window_to_screen (event->xany.window);
      UNBLOCK_INPUT;
      if (!s)
	{
	  Vepoch_event = Qnil;
	  emacs_event->epoch_event = Qnil;
	  return;
	}
      break;
    default:
      /* Someone added a new type of event to be handled and didn't do
	 a complete job. */
      Vepoch_event = Qnil;
      emacs_event->epoch_event = Qnil;
      return;
    }

  if (XTYPE (Vepoch_event) != Lisp_Vector || XVECTOR (Vepoch_event)->size < 3)
    Vepoch_event = Fmake_vector (make_number (3), Qnil);
  evp = XVECTOR (Vepoch_event);

  XSETR (evp->contents[2], Lisp_Screen, s);

  if (EQ (type, Qx_property_change))
    {
      evp->contents[0] = Qx_property_change;
      evp->contents[1] =
	read_property_event (&event->xproperty, evp->contents[2]);
    }
  else if (EQ (type, Qx_client_message))
    {
      evp->contents[0] = Qx_client_message;
      evp->contents[1] = read_client_message (&event->xclient);
    }
  else if (EQ (type, Qx_map))
    {
      evp->contents[0] = Qx_map;
      evp->contents[1] = Qt;
    }
  else if (EQ (type, Qx_unmap))
    {
      evp->contents[0] = Qx_unmap;
      evp->contents[1] = Qnil;
    }
  else
    {
      Vepoch_event = Qnil;
    }

  if (NILP (Vepoch_event))
    return;
  if (NILP (Vepoch_event_handler))
    return;

  Ffuncall (1, &Vepoch_event_handler);

  Vepoch_event = Qnil;
  emacs_event->epoch_event = Qnil;
  return;
}

void
syms_of_epoch (void)
{
  defsubr (&Sx_intern_atom);
  defsubr (&Sx_atom_name);
  defsubr (&Sx_string_to_x_resource);
  defsubr (&Sx_resource_to_type);
  defsubr (&Sx_resource_to_string);
  defsubr (&Sx_id_of_screen);
  defsubr (&Sx_query_tree);
  defsubr (&Sx_get_property);
  defsubr (&Sx_set_property);
  defsubr (&Sx_send_client_message);
}
