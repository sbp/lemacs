/* Opaque Lisp objects.
   Copyright (C) 1993 Sun Microsystems, Inc.

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

#ifndef _EMACS_OPAQUE_H_
#define _EMACS_OPAQUE_H_

#define XOPAQUE(a) ((struct Lisp_Opaque *) XPNTR(a))
#define OPAQUEP(x) RECORD_TYPEP ((x), lrecord_opaque)
#define CHECK_OPAQUE(x, i) \
  do { if (!OPAQUEP ((x))) x = wrong_type_argument (Qopaquep, (x)); } while (0)
extern CONST struct lrecord_implementation lrecord_opaque[];
extern Lisp_Object make_opaque (int size, void *data);
extern Lisp_Object Fopaquep (Lisp_Object obj);
extern Lisp_Object Qopaquep;

struct Lisp_Opaque {
  struct lcrecord_header header;
  int size;
  char data[1];
};

#define opaque_size(op) (XOPAQUE (op)->size)
#define opaque_data(op) ((void *) (XOPAQUE (op)->data))

#endif /* _EMACS_OPAQUE_H_ */
