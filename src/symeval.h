/* Definitions of symbol-value forwarding for GNU Emacs Lisp interpreter.
   Copyright (C) 1985, 1986, 1987, 1992, 1993 Free Software Foundation, Inc.

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

/* Fsymbol_value checks whether XSYMBOL (sym)->value is one of these,
 *  and does weird magic stuff if so */

#ifndef _EMACS_SYMEVAL_H_
#define _EMACS_SYMEVAL_H_

struct symbol_value_magic
{
  struct lcrecord_header lcheader;
  enum
    {
      fixnum_forward,           /* Forward C "int" */
      boolean_forward,          /* Forward C boolean ("int") */
      object_forward,           /* Forward C Lisp_Object */
      default_buffer_forward,   /* Forward Lisp_Object into Vbuffer_defaults */
      current_buffer_forward,   /* Forward Lisp_Object into current_buffer */
      /*some_buffer_forward,       Either current_buffer or buffer_defaults */
      buffer_local,             /* make-local-variable */
      some_buffer_local,        /* make-variable-buffer-local */
      unbound_marker            /* Only Qunbound actually has this tag */
    } type;
};
/* #define SYMBOL_VALUE_MAGIC_P(v) (XTYPE(v) == Lisp_Symbol_Value_Magic) */
#define SYMBOL_VALUE_MAGIC_P(x) \
  (LRECORDP((x)) \
   && (XRECORD_LHEADER ((x))->implementation->printer \
       == print_symbol_value_magic))
extern void print_symbol_value_magic (Lisp_Object, Lisp_Object, int);
extern const struct lrecord_implementation lrecord_symbol_value_forward[];
extern const struct lrecord_implementation lrecord_symbol_value_buffer_local[];


struct symbol_value_forward
{
  struct symbol_value_magic magic;
  /* void *forward; -- use magic.lcheader.next instead */
};
#define XSYMBOL_VALUE_FORWARD(v) \
	((const struct symbol_value_forward *) XPNTR(v))
#define symbol_value_forward_forward(m) ((void *)((m)->magic.lcheader.next))

struct symbol_value_buffer_local
{
  struct symbol_value_magic magic;
  /* Used in a symbol value cell when the symbol's value is per-buffer.
   * The actual contents are a cons cell which starts a list like this:
   * 
   * CURRENT-BUFFER is the last buffer for which this symbol's value was
   * made up to date.
   * 
   * CURRENT-ALIST-ELEMENT is a pointer to an element of BUFFER's
   * buffer_local_var_alist, that being the element whose car
   * is this variable.
   * Or it can be NIL if BUFFER does not have an element in its
   *  alist for this variable (that is, if BUFFER sees the default
   *  value of this variable).
   * 
   * If we want to examine or set the value and BUFFER is current,
   * we just examine or set REALVALUE.
   * If BUFFER is not current, we store the current REALVALUE value into
   * CURRENT-ALIST-ELEMENT, then find the appropriate alist element for
   * the buffer now current and set up CURRENT-ALIST-ELEMENT.
   * Then we set REALVALUE out of that element, and store into BUFFER.
   * 
   * If we are setting the variable and the current buffer does not have
   * an alist entry for this variable, an alist entry is created.
   * 
   * Note that CURRENT-VALUE (but not DEFAULT-VALUE) can be a
   * forwarding pointer.  Each time it is examined or set, 
   * forwarding must be done.
   */
  Lisp_Object default_value;
  Lisp_Object current_value;
  Lisp_Object current_buffer;
  Lisp_Object current_alist_element;
};
#define XSYMBOL_VALUE_BUFFER_LOCAL(v) \
	((struct symbol_value_buffer_local *)XPNTR(v))

#endif /* _EMACS_SYMEVAL_H_ */
