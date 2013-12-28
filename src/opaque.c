/* Opaque Lisp objects.
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

Written by Ben Wing, October 1993.

*/

/* "Opaque" is just about the most minimal Lisp type possible and is
   used internally to hold keep track of allocated memory so it gets
   GC'd properly.  As its name implies, its contents are inaccessible
   to the Lisp programmer.  Once created in C, opaque objects cannot
   be resized.
 */

#include "config.h"
#include "intl.h"
#include "opaque.h"

#include <stdio.h>

#define writeit(x) (write_string_1 ((x), -1, printcharfun))

/**********************************************************************/
/*                          OPAQUE OBJECTS                            */
/**********************************************************************/

Lisp_Object Qopaquep;
static Lisp_Object mark_opaque (Lisp_Object, void (*) (Lisp_Object));
static void print_opaque (Lisp_Object, Lisp_Object, int);
static unsigned int sizeof_opaque (CONST void *header);
static int opaque_equal (Lisp_Object, Lisp_Object, int depth);
DEFINE_LRECORD_SEQUENCE_IMPLEMENTATION ("opaque", lrecord_opaque,
					mark_opaque, print_opaque, 0,
					opaque_equal, sizeof_opaque);

static Lisp_Object
mark_opaque (Lisp_Object obj, void (*markobj) (Lisp_Object))
{
  return Qnil;
}

static void
print_opaque (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  struct Lisp_Opaque *p = XOPAQUE (obj);
  char buf[200];

  if (print_readably)
    error (GETTEXT ("printing unreadable object #<opaque 0x%x>"), (long) p);

  sprintf (buf, "#<opaque 0x%x>", (long) p);
  writeit (buf);
}

static unsigned int
sizeof_opaque (CONST void *header)
{
  struct Lisp_Opaque *p = (struct Lisp_Opaque *) header;
  return sizeof (*p) + p->size - 1;
}

static int
opaque_equal (Lisp_Object o1, Lisp_Object o2, int depth)
{
  /* Lisp programmer has no business knowing more than this! */
  return XOPAQUE (o1) == XOPAQUE (o2);
}

Lisp_Object make_opaque (int size,
			 void *data)
{
  struct Lisp_Opaque *p = alloc_lcrecord (sizeof (*p) + size - 1,
					  lrecord_opaque);
  Lisp_Object val;

  p->size = size;
  if (data)
    memcpy (p->data, data, size);
  else
    memset (p->data, 0, size);
  XSETR (val, Lisp_Opaque, p);
  return val;
}

DEFUN ("opaquep", Fopaquep, Sopaquep, 1, 1, 0,
       "Whether the given object is an opaque object.")
  (obj)
  Lisp_Object obj;
{
  return (OPAQUEP (obj) ? Qt : Qnil);
}

void
syms_of_opaque()
{
  defsymbol (&Qopaquep, "opaquep");
  defsubr (&Sopaquep);
}
