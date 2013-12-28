/* "intern" and friends -- moved here from lread.c
   Copyright (C) 1985, 1986, 1987, 1988, 1989, 1992, 1993
   Free Software Foundation, Inc.

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
#include "symeval.h"
#include "buffer.h"             /* for Vbuffer_defaults */
#include <stdio.h>		/* for sprintf */


#ifdef LRECORD_SYMBOL

static Lisp_Object mark_symbol (Lisp_Object, void (*) (Lisp_Object));
extern void print_symbol (Lisp_Object, Lisp_Object, int);
static int sizeof_symbol (void *h) { return (sizeof (struct Lisp_Symbol)); }
DEFINE_LRECORD_IMPLEMENTATION (lrecord_symbol,
                               mark_symbol, print_symbol, 
                               0, sizeof_symbol, 0);

static Lisp_Object
mark_symbol (Lisp_Object obj, void (*markobj) (Lisp_Object))
{
  struct Lisp_Symbol *sym = XSYMBOL (obj);
  Lisp_Object pname;

  ((markobj) (sym->value));
  ((markobj) (sym->function));
  XSET (pname, Lisp_String, sym->name);
  ((markobj) (pname));
  if (!symbol_next (sym))
    return (sym->plist);
  else
  {
    ((markobj) (sym->plist));
    /* Mark the rest of the symbols in the obarray hash-chain */
    sym = symbol_next (sym);
    XSETSYMBOL (obj, sym);
    return (obj);
  }
}

#endif /* LRECORD_SYMBOL */


/**********************************************************************/
/* Intern                                                             */
/**********************************************************************/

Lisp_Object Vobarray;

static Lisp_Object initial_obarray;

static Lisp_Object
check_obarray (Lisp_Object obarray)
{
  while (!VECTORP (obarray) || vector_length (XVECTOR (obarray)) == 0)
    {
      /* If Vobarray is now invalid, force it to be valid.  */
      if (EQ (Vobarray, obarray)) Vobarray = initial_obarray;

      obarray = wrong_type_argument (Qvectorp, obarray);
    }
  return obarray;
}

Lisp_Object
intern (const char *str)
{
  Lisp_Object tem;
  int len = strlen (str);
  Lisp_Object obarray = Vobarray;
  if (!VECTORP (obarray) || vector_length (XVECTOR (obarray)) == 0)
    obarray = check_obarray (obarray);
  tem = oblookup (obarray, (const unsigned char *) str, len);
  if (SYMBOLP (tem))
    return tem;
  return Fintern (((purify_flag)
		   ? make_pure_pname (str, len, 0)
		   : make_string (str, len)),
		  obarray);
}

DEFUN ("intern", Fintern, Sintern, 1, 2, 0,
  "Return the canonical symbol whose name is STRING.\n\
If there is none, one is created by this function and returned.\n\
A second optional argument specifies the obarray to use;\n\
it defaults to the value of `obarray'.")
  (str, obarray)
     Lisp_Object str, obarray;
{
  Lisp_Object sym, *ptr;
  int len;

  if (NILP (obarray)) obarray = Vobarray;
  obarray = check_obarray (obarray);

  CHECK_STRING (str, 0);

  len = string_length (XSTRING (str));
  sym = oblookup (obarray, XSTRING (str)->data, len);
  if (!FIXNUMP (sym))
    /* Found it */
    return sym;

  ptr = &XVECTOR (obarray)->contents[XINT (sym)];

  if (purify_flag && ! purified (str))
    str = make_pure_pname ((char *) XSTRING (str)->data, len, 0);
  sym = Fmake_symbol (str);

  if (SYMBOLP (*ptr))
    symbol_next (XSYMBOL (sym)) = XSYMBOL (*ptr);
  else
    symbol_next (XSYMBOL (sym)) = 0;
  *ptr = sym;
  return sym;
}

DEFUN ("intern-soft", Fintern_soft, Sintern_soft, 1, 2, 0,
  "Return the canonical symbol whose name is STRING, or nil if none exists.\n\
A second optional argument specifies the obarray to use;\n\
it defaults to the value of `obarray'.")
  (str, obarray)
     Lisp_Object str, obarray;
{
  register Lisp_Object tem;

  if (NILP (obarray)) obarray = Vobarray;
  obarray = check_obarray (obarray);

  CHECK_STRING (str, 0);

  tem = oblookup (obarray, XSTRING (str)->data, string_length (XSTRING (str)));
  if (!FIXNUMP (tem))
    return tem;
  return Qnil;
}

Lisp_Object
oblookup (Lisp_Object obarray, const unsigned char *ptr, int size)
{
  int hash, obsize;
  struct Lisp_Symbol *tail;
  Lisp_Object bucket;

  while (!VECTORP (obarray) ||
         (obsize = vector_length (XVECTOR (obarray))) == 0)
    {
      obarray = check_obarray (obarray);
    }
  /* Combining next two lines breaks VMS C 2.3.  */
  hash = hash_string ((const unsigned char *) ptr, size);
  hash %= obsize;
  bucket = XVECTOR (obarray)->contents[hash];
  if (XINT (bucket) == 0)
    ;
  else if (!SYMBOLP (bucket))
    error ("Bad data in guts of obarray"); /* Like CADR error message */
  else
    for (tail = XSYMBOL (bucket); ;)
      {
	if (tail->name->size == size &&
	    !memcmp (tail->name->data, ptr, size))
          {
            XSETSYMBOL (bucket, tail);
            return (bucket);
          }
        tail = symbol_next (tail);
        if (!tail)
	  break;
      }
  return (make_number (hash));
}

int
hash_string (const unsigned char *ptr, int len)
{
  register const unsigned char *p = ptr;
  register const unsigned char *end = p + len;
  register unsigned char c;
  register int hash = 0;

  while (p != end)
    {
      c = *p++;
      if (c >= 0140) c -= 40;
      hash = ((hash<<3) + (hash>>28) + c);
    }
  return hash & 07777777777;
}

void
map_obarray (Lisp_Object obarray,
             void (*fn) (Lisp_Object sym, Lisp_Object arg),
             Lisp_Object arg)
{
  int i;
  Lisp_Object tail;
  CHECK_VECTOR (obarray, 1);
  for (i = vector_length (XVECTOR (obarray)) - 1; i >= 0; i--)
    {
      tail = XVECTOR (obarray)->contents[i];
      if (SYMBOLP (tail))
	while (1)
	  {
            struct Lisp_Symbol *next;
	    (*fn) (tail, arg);
            next = symbol_next (XSYMBOL (tail));
	    if (!next)
	      break;
	    XSETSYMBOL (tail, next);
	  }
    }
}

static void
mapatoms_1 (Lisp_Object sym, Lisp_Object function)
{
  call1 (function, sym);
}

DEFUN ("mapatoms", Fmapatoms, Smapatoms, 1, 2, 0,
  "Call FUNCTION on every symbol in OBARRAY.\n\
OBARRAY defaults to the value of `obarray'.")
  (function, obarray)
     Lisp_Object function, obarray;
{
  if (NILP (obarray))
    obarray = Vobarray;
  obarray = check_obarray (obarray);

  map_obarray (obarray, mapatoms_1, function);
  return Qnil;
}


/**********************************************************************/
/* Apropos                                                            */
/**********************************************************************/

static void
apropos_accum (Lisp_Object symbol, Lisp_Object arg)
{
  register Lisp_Object tem;
  Lisp_Object string = XCONS (arg)->car;
  Lisp_Object predicate = XCONS (XCONS (arg)->cdr)->car;
  Lisp_Object *accumulation = &(XCONS (XCONS (arg)->cdr)->cdr);

  tem = Fstring_match (string, Fsymbol_name (symbol), Qnil);
  if (!NILP (tem) && !NILP (predicate))
    tem = call1 (predicate, symbol);
  if (!NILP (tem))
    *accumulation = Fcons (symbol, *accumulation);
}

DEFUN ("apropos-internal", Fapropos_internal, Sapropos_internal, 1, 2, 0, 
  "Show all symbols whose names contain match for REGEXP.\n\
If optional 2nd arg PRED is non-nil, (funcall PRED SYM) is done\n\
for each symbol and a symbol is mentioned only if that returns non-nil.\n\
Return list of symbols found.")
  (string, pred)
     Lisp_Object string, pred;
{
  struct gcpro gcpro1;
  Lisp_Object accumulation;

  CHECK_STRING (string, 0);
  accumulation = Fcons (string, Fcons (pred, Qnil));
  GCPRO1 (accumulation);
  map_obarray (Vobarray, apropos_accum, accumulation);
  accumulation = Fsort (Fcdr (Fcdr (accumulation)), Qstring_lessp);
  UNGCPRO;
  return (accumulation);
}


/* Extract and set components of symbols */

static Lisp_Object swap_in_symval_forwarding
			(Lisp_Object sym, struct symbol_value_buffer_local *);
#define XSYMBOL_VALUE_MAGIC_TYPE(v) \
	(((struct symbol_value_magic *) XPNTR(v))->type)


DEFUN ("boundp", Fboundp, Sboundp, 1, 1, 0, "T if SYMBOL's value is not void.")
  (sym)
     register Lisp_Object sym;
{
  Lisp_Object valcontents;
  CHECK_SYMBOL (sym, 0);

  valcontents = XSYMBOL (sym)->value;

  if (SYMBOL_VALUE_MAGIC_P (valcontents))
  {
    switch (XSYMBOL_VALUE_MAGIC_TYPE (valcontents))
      {
      case unbound_marker:
	return (Qnil);
      case buffer_local:
      case some_buffer_local:
	{
	  struct symbol_value_buffer_local *bfwd
	    = XSYMBOL_VALUE_BUFFER_LOCAL (valcontents);
	  valcontents = swap_in_symval_forwarding (sym, bfwd);
	  break;
	}
      case fixnum_forward:
      case boolean_forward:
      case object_forward:
      case default_buffer_forward:
      case current_buffer_forward:
        return (Qt);
      }
  }
  return (EQ (valcontents, Qunbound) ? Qnil : Qt);
}

DEFUN ("globally-boundp", Fglobally_boundp, Sglobally_boundp, 1, 1, 0,
       "T if SYMBOL has a global (non-bound) value.\n\
This is for the byte-compiler; you really shouldn't be using this.")
  (sym)
     register Lisp_Object sym;
{
  CHECK_SYMBOL (sym, 0);
  return (EQ (top_level_value (sym), Qunbound) ? Qnil : Qt);
}

DEFUN ("fboundp", Ffboundp, Sfboundp, 1, 1, 0,
       "T if SYMBOL's function definition is not void.")
  (sym)
     register Lisp_Object sym;
{
  CHECK_SYMBOL (sym, 0);
  return ((EQ (XSYMBOL (sym)->function, Qunbound)) ? Qnil : Qt);
}

DEFUN ("makunbound", Fmakunbound, Smakunbound, 1, 1, 0,
       "Make SYMBOL's value be void.")
  (sym)
     register Lisp_Object sym;
{
  CHECK_SYMBOL (sym, 0);
  if (NILP (sym) || EQ (sym, Qt))
    return Fsignal (Qsetting_constant, list1 (sym));
  Fset (sym, Qunbound);
  return sym;
}

DEFUN ("fmakunbound", Ffmakunbound, Sfmakunbound, 1, 1, 0,
       "Make SYMBOL's function definition be void.")
  (sym)
     register Lisp_Object sym;
{
  CHECK_SYMBOL (sym, 0);
  XSYMBOL (sym)->function = Qunbound;
  return sym;
}

DEFUN ("symbol-function", Fsymbol_function, Ssymbol_function, 1, 1, 0,
  "Return SYMBOL's function definition.  Error if that is void.")
  (symbol)
     register Lisp_Object symbol;
{
  CHECK_SYMBOL (symbol, 0);
  if (EQ (XSYMBOL (symbol)->function, Qunbound))
    return Fsignal (Qvoid_function, list1 (symbol));
  return XSYMBOL (symbol)->function;
}

DEFUN ("symbol-plist", Fsymbol_plist, Ssymbol_plist, 1, 1, 0,
       "Return SYMBOL's property list.")
  (sym)
     register Lisp_Object sym;
{
  CHECK_SYMBOL (sym, 0);
  return XSYMBOL (sym)->plist;
}

DEFUN ("symbol-name", Fsymbol_name, Ssymbol_name, 1, 1, 0,
       "Return SYMBOL's name, a string.")
  (sym)
     register Lisp_Object sym;
{
  Lisp_Object name;

  CHECK_SYMBOL (sym, 0);
  XSET (name, Lisp_String, XSYMBOL (sym)->name);
  return name;
}

DEFUN ("fset", Ffset, Sfset, 2, 2, 0,
  "Set SYMBOL's function definition to NEWVAL, and return NEWVAL.")
  (sym, newdef)
     register Lisp_Object sym, newdef;
{
  CHECK_SYMBOL (sym, 0);
  if (!NILP (Vautoload_queue) && !EQ (XSYMBOL (sym)->function, Qunbound))
    Vautoload_queue = Fcons (Fcons (sym, XSYMBOL (sym)->function),
			     Vautoload_queue);
  XSYMBOL (sym)->function = newdef;
  return newdef;
}

/* FSFmacs */
DEFUN ("define-function", Fdefine_function, Sdefine_function, 2, 2, 0,
  "Set SYMBOL's function definition to NEWVAL, and return NEWVAL.\n\
Associates the function with the current load file, if any.")
  (sym, newdef)
     register Lisp_Object sym, newdef;
{
  CHECK_SYMBOL (sym, 0);
  Ffset (sym, newdef);
  LOADHIST_ATTACH (sym);
  return newdef;
}


DEFUN ("setplist", Fsetplist, Ssetplist, 2, 2, 0,
  "Set SYMBOL's property list to NEWVAL, and return NEWVAL.")
  (sym, newplist)
     register Lisp_Object sym, newplist;
{
  CHECK_SYMBOL (sym, 0);
  XSYMBOL (sym)->plist = newplist;
  return newplist;
}


/**********************************************************************/
/* symbol-value                                                       */
/**********************************************************************/

static Lisp_Object mark_symbol_value_buffer_local (Lisp_Object,
						   void (*) (Lisp_Object));

DEFINE_LRECORD_IMPLEMENTATION (lrecord_symbol_value_forward, 0,
                               print_symbol_value_magic,
                               0, 0, 0);

DEFINE_LRECORD_IMPLEMENTATION (lrecord_symbol_value_buffer_local, 
                               mark_symbol_value_buffer_local,
                               print_symbol_value_magic, 
                               0, 0, 0);

static Lisp_Object
mark_symbol_value_buffer_local (Lisp_Object obj,
				void (*markobj) (Lisp_Object))
{
  switch (XSYMBOL_VALUE_MAGIC_TYPE (obj))
    {
    case fixnum_forward:
    case boolean_forward:
    case object_forward:
    case default_buffer_forward:
    case unbound_marker:
      abort ();
    case current_buffer_forward:
    case buffer_local:
    case some_buffer_local:
      {
	struct symbol_value_buffer_local *bfwd
	  = XSYMBOL_VALUE_BUFFER_LOCAL (obj);
	((markobj) (bfwd->default_value));
	((markobj) (bfwd->current_value));
	((markobj) (bfwd->current_buffer));
	return (bfwd->current_alist_element);
      }
    default:
      abort ();
    }
}

/* Should never, ever be called. (except by an external debugger) */
void
print_symbol_value_magic (Lisp_Object obj, 
                          Lisp_Object printcharfun, int escapeflag)
{
  char buf[40];
  sprintf (buf, "#<INTERNAL EMACS BUG (symfwd %d) 0x%x>",
           (LISP_WORD_TYPE) XSYMBOL_VALUE_MAGIC_TYPE (obj), (LISP_WORD_TYPE) XPNTR (obj));
  write_string_1 (buf, -1, printcharfun);
}



/* Getting and setting values of symbols */

/* Given the raw contents of a symbol value cell,
   return the Lisp value of the symbol.
   This does not handle buffer-local variables; use
   swap_in_symval_forwarding for that.  */

static Lisp_Object
do_symval_forwarding (Lisp_Object valcontents, struct buffer *buffer)
{
  const struct symbol_value_forward *fwd;

  if (!SYMBOL_VALUE_MAGIC_P (valcontents))
    return (valcontents);

  fwd = XSYMBOL_VALUE_FORWARD (valcontents);
  switch (fwd->magic.type)
    {
    case fixnum_forward:
      return (make_number (*((int *)symbol_value_forward_forward (fwd))));

    case boolean_forward:
      {
	if (*((int *)symbol_value_forward_forward (fwd)))
	  return (Qt);
	else
	  return (Qnil);
      }

    case object_forward:
      return (*((Lisp_Object *)symbol_value_forward_forward (fwd)));

    case default_buffer_forward:
      return (*((Lisp_Object *)((char *) XBUFFER (Vbuffer_defaults)
				+ ((char *)symbol_value_forward_forward (fwd)
				   - (char *)&buffer_local_flags))));


    case current_buffer_forward:
      return (*((Lisp_Object *)((char *)buffer
				+ ((char *)symbol_value_forward_forward (fwd)
				   - (char *)&buffer_local_flags))));

    case unbound_marker:
      return (valcontents);

    case buffer_local:
    case some_buffer_local:
    default:
      abort ();
    }
}

/* Store NEWVAL into SYM, where VALCONTENTS is found in the value cell
   of SYM.  If SYM is buffer-local, VALCONTENTS should be the
   buffer-independent contents of the value cell: forwarded just one
   step past the buffer-localness.  */

void
store_symval_forwarding (sym, ovalue, newval)
     Lisp_Object sym;
     register Lisp_Object ovalue, newval;
{
  if (!SYMBOL_VALUE_MAGIC_P (ovalue))
    {
      ovalue = XSYMBOL (sym)->value;
    
      if (!SYMBOL_VALUE_MAGIC_P (ovalue))
	{
	  XSYMBOL (sym)->value = newval;
	}

      else switch (XSYMBOL_VALUE_MAGIC_TYPE (ovalue))
	{
	case buffer_local:
	case some_buffer_local:
	  XSYMBOL_VALUE_BUFFER_LOCAL (ovalue)->current_value = newval;
	  break;
	default:
	  XSYMBOL (sym)->value = newval;
	  break;
	}
      return;
    }

  else switch (XSYMBOL_VALUE_MAGIC_TYPE (ovalue))
    {
    case unbound_marker:
      XSYMBOL (sym)->value = newval;
      return;

    case fixnum_forward:
      {
	const struct symbol_value_forward *fwd
	  = XSYMBOL_VALUE_FORWARD (ovalue);
	CHECK_FIXNUM (newval, 1);
	*((int *)symbol_value_forward_forward (fwd)) = XINT (newval);
	return;
      }

    case boolean_forward:
      {
	const struct symbol_value_forward *fwd
	  = XSYMBOL_VALUE_FORWARD (ovalue);
	*((int *)symbol_value_forward_forward (fwd))
	  = ((NILP (newval)) ? 0 : 1);
	return;
      }

    case object_forward:
      {
	const struct symbol_value_forward *fwd
	  = XSYMBOL_VALUE_FORWARD (ovalue);
	*((Lisp_Object *)symbol_value_forward_forward (fwd)) = newval;
	return;
      }

    case default_buffer_forward:
      {
	const struct symbol_value_forward *fwd
	  = XSYMBOL_VALUE_FORWARD (ovalue);
	*((Lisp_Object *)((char *) XBUFFER (Vbuffer_defaults)
			  + ((char *)symbol_value_forward_forward (fwd)
			     - (char *)&buffer_local_flags))) 
	  = newval;
	return;
      }

    case current_buffer_forward:
      {
	const struct symbol_value_forward *fwd
	  = XSYMBOL_VALUE_FORWARD (ovalue);
	*((Lisp_Object *)((char *)current_buffer
			  + ((char *)symbol_value_forward_forward (fwd)
			     - (char *)&buffer_local_flags))) 
	  = newval;
	return;
      }

    default:
      abort ();
    }
}

/* Set up the buffer-local symbol SYM for validity in the current
   buffer.  VALCONTENTS is the contents of its value cell.
   Return the value forwarded one step past the buffer-local indicator.  */

static Lisp_Object
swap_in_symval_forwarding (Lisp_Object sym, 
                           struct symbol_value_buffer_local *bfwd)
{
  /* If the current buffer is not BUFFER, 
     we store the current CURRENT-VALUE value into CURRENT-ALIST-ELEMENT,
     then find the appropriate alist element for the buffer now current
     and set up CURRENT-ALIST-ELEMENT.
     Then we set CURRENT-VALUE out of that element, and store into BUFFER.
     Note that CURRENT-VALUE can be a forwarding pointer. */

  register Lisp_Object tem;

  if (current_buffer != XBUFFER (bfwd->current_buffer))
    {
      Lisp_Object old_current
	= do_symval_forwarding (bfwd->current_value, current_buffer);

      if (NILP (bfwd->current_alist_element))
	/* current_value may be updated more recently than default_value */
	bfwd->default_value = old_current;
      else
	Fsetcdr (bfwd->current_alist_element, old_current);

      tem = assq_no_quit (sym, current_buffer->local_var_alist);
      if (NILP (tem))
	{
	  bfwd->current_alist_element = Qnil;
	  tem = bfwd->default_value;
	}
      else
	{
	  bfwd->current_alist_element = tem;
	  tem = Fcdr (tem);
	}
      bfwd->current_buffer = Fcurrent_buffer ();
      store_symval_forwarding (sym,
			       bfwd->current_value, 
			       tem);
    }
  return (bfwd->current_value);
}



void
decache_buffer_local_variables (struct buffer *buf)
{
  Lisp_Object alist;
  for (alist = current_buffer->local_var_alist;
       !NILP (alist); 
       alist = XCONS (alist)->cdr)
    {
      Lisp_Object sym = XCONS (XCONS (alist)->car)->car;
      struct symbol_value_buffer_local *bfwd;

      if (!SYMBOL_VALUE_MAGIC_P (XSYMBOL (sym)->value))
	abort ();

      bfwd = XSYMBOL_VALUE_BUFFER_LOCAL (XSYMBOL (sym)->value);
      if (bfwd->magic.type != buffer_local
	  && bfwd->magic.type != some_buffer_local)
	abort ();

      /* Need not do anything if some other buffer's binding is now encached.
       */
      if (XBUFFER (bfwd->current_buffer) == current_buffer)
	{
	  /* Symbol is set up for this buffer's old local value.
	     Set it up for the current buffer with the default value.  */

	  /* Store the symbol's current value into the alist entry
	     it is currently set up for.  This is so that, if the
	     local is marked permanent, and we make it local again below,
	     we don't lose the value.  */
	  if (!NILP (bfwd->current_alist_element))
	    Fsetcdr (bfwd->current_alist_element, bfwd->current_value);

	  /* Switch to the symbol's default-value alist entry.  */
	  bfwd->current_alist_element = Qnil;
	  /* Mark it as current for the current buffer.  */
	  bfwd->current_buffer = Fcurrent_buffer ();
	  /* Store the current value into any forwarding in the symbol.  */
	  store_symval_forwarding (sym, 
				   bfwd->current_value,
				   bfwd->default_value);
	}
    }
}

/* Find the value of a symbol, returning Qunbound if it's not bound.
   Note that it must not be possible to quit within this function. */

Lisp_Object
symbol_value_in_buffer (Lisp_Object sym, Lisp_Object buffer)
{
  Lisp_Object valcontents;
  struct buffer *buf;

  CHECK_SYMBOL (sym, 0);
  CHECK_BUFFER (buffer, 1);
  valcontents = XSYMBOL (sym)->value;
  buf = XBUFFER (buffer);

  if (!SYMBOL_VALUE_MAGIC_P (valcontents))
    return (valcontents);

  switch (XSYMBOL_VALUE_MAGIC_TYPE (valcontents))
    {
    case unbound_marker:	/* Qunbound */
    case fixnum_forward:
    case boolean_forward:
    case object_forward:
    case default_buffer_forward:
    case current_buffer_forward:
      break;
    case buffer_local:
    case some_buffer_local:
      {
	struct symbol_value_buffer_local *bfwd
	  = XSYMBOL_VALUE_BUFFER_LOCAL (valcontents);

        if (buf == XBUFFER (bfwd->current_buffer))
          valcontents = bfwd->current_value;
        else
          {
            valcontents = assq_no_quit (sym, buf->local_var_alist);
            if (NILP (valcontents))
              valcontents = bfwd->default_value;
            else
              valcontents = Fcdr (valcontents);
          }
        break;
      }
      
    default:
      abort ();
    }
  return (do_symval_forwarding (valcontents, buf));
}

DEFUN ("symbol-value-in-buffer", Fsymbol_value_in_buffer, Ssymbol_value_in_buffer, 3, 3, 0,
  "Return the value of SYMBOL in BUFFER, or UNBOUND-VALUE if it has unboud.")
  (symbol, buffer, unbound_value)
     Lisp_Object symbol, buffer, unbound_value;
{
  Lisp_Object value;
  CHECK_SYMBOL (symbol, 0);
  CHECK_BUFFER (buffer, 0);
  value = symbol_value_in_buffer (symbol, buffer);
  if (EQ (value, Qunbound))
    return (unbound_value);
  else
    return (value);
}

Lisp_Object
find_symbol_value (sym)
     Lisp_Object sym;
{
  Lisp_Object valcontents;

  CHECK_SYMBOL (sym, 0);
  valcontents = XSYMBOL (sym)->value;

 retry:
  if (!SYMBOL_VALUE_MAGIC_P (valcontents))
    return (valcontents);

  switch (XSYMBOL_VALUE_MAGIC_TYPE (valcontents))
    {
    case unbound_marker:	/* Qunbound */
      return (valcontents);

    case fixnum_forward:
      {
	const struct symbol_value_forward *fwd
	  = XSYMBOL_VALUE_FORWARD (valcontents);
	return (make_number (*((int *)symbol_value_forward_forward (fwd))));
      }

    case boolean_forward:
      {
	const struct symbol_value_forward *fwd
	  = XSYMBOL_VALUE_FORWARD (valcontents);
	if (*((int *)symbol_value_forward_forward (fwd)))
	  return (Qt);
	else
	  return (Qnil);
      }

    case object_forward:
      {
	const struct symbol_value_forward *fwd
	  = XSYMBOL_VALUE_FORWARD (valcontents);
	return (*((Lisp_Object *) symbol_value_forward_forward (fwd)));
      }

    case default_buffer_forward:
      {
	const struct symbol_value_forward *fwd
	  = XSYMBOL_VALUE_FORWARD (valcontents);
	return (*((Lisp_Object *)((char *) XBUFFER (Vbuffer_defaults)
				  + ((char *)symbol_value_forward_forward (fwd)
				     - (char *)&buffer_local_flags))));
      }

    case current_buffer_forward:
      {
	const struct symbol_value_forward *fwd
	  = XSYMBOL_VALUE_FORWARD (valcontents);
	return (*((Lisp_Object *)((char *)current_buffer
				  + ((char *)symbol_value_forward_forward (fwd)
				     - (char *)&buffer_local_flags))));
      }

    case buffer_local:
    case some_buffer_local:
      {
	struct symbol_value_buffer_local *bfwd
	  = XSYMBOL_VALUE_BUFFER_LOCAL (valcontents);
	valcontents = swap_in_symval_forwarding (sym, bfwd);
	goto retry;
      }
    default:
      abort ();
    }
}

DEFUN ("symbol-value", Fsymbol_value, Ssymbol_value, 1, 1, 0,
  "Return SYMBOL's value.  Error if that is void.")
  (sym)
     Lisp_Object sym;
{
  Lisp_Object val = find_symbol_value (sym);

  if (EQ (val, Qunbound))
    return Fsignal (Qvoid_variable, list1 (sym));
  else
    return val;
}

DEFUN ("set", Fset, Sset, 2, 2, 0,
  "Set SYMBOL's value to NEWVAL, and return NEWVAL.")
  (sym, newval)
     register Lisp_Object sym, newval;
{
  int voide = (EQ (newval, Qunbound));
  register Lisp_Object valcontents;

  CHECK_SYMBOL (sym, 0);
  if (NILP (sym) || EQ (sym, Qt))
    return Fsignal (Qsetting_constant, list2 (sym, newval));
  valcontents = XSYMBOL (sym)->value;

  if (SYMBOL_VALUE_MAGIC_P (valcontents))
    {
      switch (XSYMBOL_VALUE_MAGIC_TYPE (valcontents))
	{
	case fixnum_forward:
	case boolean_forward:
	case object_forward:
	case default_buffer_forward:
          if (voide)
            signal_error (Qerror, 
                          list2 (build_string ("Cannot makunbound"), sym));
	  break;

	case unbound_marker:
	  break;

	case current_buffer_forward:
	  {
	    const struct symbol_value_forward *fwd
	      = XSYMBOL_VALUE_FORWARD (valcontents);
	    register int mask = *((int *) symbol_value_forward_forward (fwd));
	    if (mask > 0)
	      /* Setting this variable makes it buffer-local */
	      current_buffer->local_var_flags |= mask;
	    break;
	  }

	case buffer_local:
	case some_buffer_local:
	  {
	    /* If we want to examine or set the value and BUFFER is current,
	       we just examine or set CURRENT-VALUE. If BUFFER is not current,
	       we store the current CURRENT-VALUE value into CURRENT-ALIST-
	       ELEMENT, then find the appropriate alist element for the buffer
	       now current and set up CURRENT-ALIST-ELEMENT.  Then we set
	       CURRENT-VALUE out of that element, and store into BUFFER.
	       
	       If we are setting the variable and the current buffer does
	       not have an alist entry for this variable, an alist entry is
	       created.
	       
	       Note that CURRENT-VALUE can be a forwarding pointer.  Each time
	       it is examined or set, forwarding must be done.
	     */
	    struct symbol_value_buffer_local *bfwd
	      = XSYMBOL_VALUE_BUFFER_LOCAL (valcontents);
	    int some_buffer_local_p = (bfwd->magic.type == some_buffer_local);
	    /* What value are we caching right now?  */
	    Lisp_Object alist_element = bfwd->current_alist_element;
	    Lisp_Object tem;

	    /* If the current buffer is not the buffer whose binding is
	       currently cached, or if it's a Lisp_Buffer_Local_Value and
	       we're looking at the default value, the cache is invalid; we
	       need to write it out, and find the new CURRENT-ALIST-ELEMENT.
	     */
	    if ((current_buffer == XBUFFER (bfwd->current_buffer))
		&& ((some_buffer_local_p) ? 1 : !NILP (alist_element)))
	      {
		valcontents = bfwd->current_value;
		break;
	      }

	    /* Write out the cached value for the old buffer; copy it
	       back to its alist element.  This works if the current
	       buffer only sees the default value, too.  */
	    tem = do_symval_forwarding (bfwd->current_value, current_buffer);
	    if (NILP (alist_element))
	      bfwd->default_value = tem;
	    else
	      Fsetcdr (alist_element, tem);

	    /* Find the new value for CURRENT-ALIST-ELEMENT.  */
	    tem = Fassq (sym, current_buffer->local_var_alist);
	    if (NILP (tem))
	      {
		/* This buffer still sees the default value.  */

		if (some_buffer_local_p)
		  {
		    /* If the variable is a Lisp_Some_Buffer_Local_Value,
		       indicate that we're seeing the default value.  */
		    tem = Qnil;
		  }
		else
		  {
		    /* If it's a Lisp_Buffer_Local_Value, give this buffer a
		       new assoc for a local value and set
		       CURRENT-ALIST-ELEMENT to point to that.  */
		    tem = Fcons (sym, Fcdr (alist_element));
		    current_buffer->local_var_alist =
		      Fcons (tem, current_buffer->local_var_alist);
		  }
	      }
	    /* Cache the new buffer's assoc in CURRENT-ALIST-ELEMENT.  */
	    bfwd->current_alist_element = tem;
	    /* Set BUFFER, now that CURRENT-ALIST-ELEMENT is accurate.  */
	    bfwd->current_buffer = Fcurrent_buffer ();
	    valcontents = bfwd->current_value;
	    break;
	  }
	default:
	  abort ();
	}
    }
  /* If storing void (making the symbol void), forward only through
     buffer-local indicator, not through Lisp_Objfwd, etc.  */
  if (voide)
    store_symval_forwarding (sym, Qnil, newval);
  else
    store_symval_forwarding (sym, valcontents, newval);

  return (newval);
}


/* Access or set a buffer-local symbol's default value.  */

/* Return the default value of SYM, but don't check for voidness.
   Return Qunbound if it is void.  */

Lisp_Object
default_value (sym)
     Lisp_Object sym;
{
  register Lisp_Object valcontents;

  CHECK_SYMBOL (sym, 0);
  valcontents = XSYMBOL (sym)->value;

  if (!SYMBOL_VALUE_MAGIC_P (valcontents))
    return (valcontents);
    
  switch (XSYMBOL_VALUE_MAGIC_TYPE (valcontents))
    {
    case unbound_marker:
      return valcontents;

    case current_buffer_forward:
      {
	const struct symbol_value_forward *fwd
	  = XSYMBOL_VALUE_FORWARD (valcontents);
	return (*((Lisp_Object *)((char *)XBUFFER (Vbuffer_defaults)
				  + ((char *)symbol_value_forward_forward (fwd)
				     - (char *)&buffer_local_flags))));
      }
    case buffer_local:
    case some_buffer_local:
      {
	struct symbol_value_buffer_local *bfwd = 
	  XSYMBOL_VALUE_BUFFER_LOCAL (valcontents);

	/* Handle user-created local variables.  */
	/* If var is set up for a buffer that lacks a local value for it,
	   the current value is nominally the default value.
	   But the current value slot may be more up to date, since
	   ordinary setq stores just that slot.  So use that.  */
	if (NILP (bfwd->current_alist_element))
	  return (do_symval_forwarding (bfwd->current_value, current_buffer));
	else
	  return (bfwd->default_value);
      }
    case fixnum_forward:
    case boolean_forward:
    case object_forward:
    case default_buffer_forward:
      {
	/* For other variables, get the current value.  */
	return (do_symval_forwarding (valcontents, current_buffer));
      }
    default:
      abort ();
    }
}

DEFUN ("default-boundp", Fdefault_boundp, Sdefault_boundp, 1, 1, 0,
  "Return T if SYMBOL has a non-void default value.\n\
This is the value that is seen in buffers that do not have their own values\n\
for this variable.")
  (sym)
     Lisp_Object sym;
{
  register Lisp_Object value;

  value = default_value (sym);
  return (EQ (value, Qunbound) ? Qnil : Qt);
}

DEFUN ("default-value", Fdefault_value, Sdefault_value, 1, 1, 0,
  "Return SYMBOL's default value.\n\
This is the value that is seen in buffers that do not have their own values\n\
for this variable.  The default value is meaningful for variables with\n\
local bindings in certain buffers.")
  (sym)
     Lisp_Object sym;
{
  register Lisp_Object value;

  value = default_value (sym);
  if (EQ (value, Qunbound))
    return Fsignal (Qvoid_variable, list1 (sym));
  return value;
}

DEFUN ("set-default", Fset_default, Sset_default, 2, 2, 0,
  "Set SYMBOL's default value to VAL.  SYMBOL and VAL are evaluated.\n\
The default value is seen in buffers that do not have their own values\n\
for this variable.")
  (sym, value)
     Lisp_Object sym, value;
{
  register Lisp_Object valcontents;

  CHECK_SYMBOL (sym, 0);
  valcontents = XSYMBOL (sym)->value;

  if (!SYMBOL_VALUE_MAGIC_P (valcontents))
    return Fset (sym, value);

  switch (XSYMBOL_VALUE_MAGIC_TYPE (valcontents))
    {
    case fixnum_forward:
    case boolean_forward:
    case object_forward:
    case default_buffer_forward:
    case unbound_marker:
      return Fset (sym, value);

    case current_buffer_forward:
      {
	/* Handle variables like case-fold-search that have special slots in
	   the buffer.  Make them work apparently like buffer_local variables.
	 */
	register const struct symbol_value_forward *fwd 
	  = XSYMBOL_VALUE_FORWARD (valcontents);
	register int offset = ((char *)symbol_value_forward_forward (fwd) 
			       - (char *)&buffer_local_flags);
	register int mask = *((int *)symbol_value_forward_forward (fwd));

	if (mask > 0)		/* Not always per-buffer */
	  {
	    Lisp_Object tail;

	    *((Lisp_Object *)(offset + (char *) XBUFFER (Vbuffer_defaults)))
	      = value;
        
	    /* Set value in each buffer which hasn't shadowed the default */
	    for (tail = Vbuffer_alist; !NILP (tail); tail = XCONS (tail)->cdr)
	      {
		struct buffer *b = XBUFFER (XCONS (XCONS (tail)->car)->cdr);
		if (!(b->local_var_flags & mask))
		  *((Lisp_Object *)(offset + (char *) b)) = value; 
	      }
	  }
	return (value);
      }

    case buffer_local:
    case some_buffer_local:
      {
	/* Store new value into the DEFAULT-VALUE slot */
	register struct symbol_value_buffer_local *bfwd
	  = XSYMBOL_VALUE_BUFFER_LOCAL (valcontents);

	bfwd->default_value = value;
	/* If current-buffer doesn't shadow default_value,
	 *  we must set the CURRENT-VALUE slot too */
	if (NILP (bfwd->current_alist_element))
	  store_symval_forwarding (sym, bfwd->current_value, value);
	return (value);
      }

    default:
      abort ();
    }
}

DEFUN ("setq-default", Fsetq_default, Ssetq_default, 2, UNEVALLED, 0,
       "(setq-default SYM VAL SYM VAL ...): set each SYM's default value.\n\
VALs are evaluated; SYMs are not.\n\
Default values are seen in buffers that do not have their own values for\n\
the variable.")
  (args)
     Lisp_Object args;
{
  register Lisp_Object args_left;
  register Lisp_Object val, sym;
  struct gcpro gcpro1;

  if (NILP (args))
    return Qnil;

  args_left = args;
  GCPRO1 (args);

  do
    {
      val = Feval (Fcar (Fcdr (args_left)));
      sym = Fcar (args_left);
      Fset_default (sym, val);
      args_left = Fcdr (Fcdr (args_left));
    }
  while (!NILP (args_left));

  UNGCPRO;
  return val;
}

/* Lisp functions for creating and removing buffer-local variables.  */

DEFUN ("make-variable-buffer-local", Fmake_variable_buffer_local, Smake_variable_buffer_local,
  1, 1, "vMake Variable Buffer Local: ",
  "Make VARIABLE have a separate value for each buffer.\n\
At any time, the value for the current buffer is in effect.\n\
There is also a default value which is seen in any buffer which has not yet\n\
set its own value.\n\
Using `set' or `setq' to set the variable causes it to have a separate value\n\
for the current buffer if it was previously using the default value.\n\
The function `default-value' gets the default value and `set-default' sets it.")
  (sym)
     register Lisp_Object sym;
{
  register Lisp_Object valcontents;

  CHECK_SYMBOL (sym, 0);

  if (EQ (sym, Qnil) || EQ (sym, Qt))
    {
    lose:
      signal_error (Qerror, 
                    list2 (build_string ("Symbol may not be buffer-local"),
                           sym));
    }

  valcontents = XSYMBOL (sym)->value;

  if (SYMBOL_VALUE_MAGIC_P (valcontents))
    {
      switch (XSYMBOL_VALUE_MAGIC_TYPE (valcontents))
	{
	case fixnum_forward:
	case boolean_forward:
	case object_forward:
	case unbound_marker:
          break;

	case default_buffer_forward:
          goto lose;

	case current_buffer_forward:
	case buffer_local:
	  /* Already per-each-buffer */
	  return (sym);

	case some_buffer_local:
	  /* Transmogrify */
	  XSYMBOL_VALUE_BUFFER_LOCAL (valcontents)->magic.type = buffer_local;
	  return (sym);
	}
    }
  
  {
    struct symbol_value_buffer_local *bfwd
      = alloc_lcrecord (sizeof (struct symbol_value_buffer_local),
                        lrecord_symbol_value_buffer_local);
    bfwd->magic.type = buffer_local;

    bfwd->default_value = ((EQ (valcontents, Qunbound))
                           ? Qnil 
                           : Fsymbol_value (sym));
    bfwd->current_alist_element = Qnil;
    bfwd->current_buffer = Fcurrent_buffer ();
    bfwd->current_value = ((EQ (valcontents, Qunbound))
			   ? Qnil
			   : valcontents);
    XSETR (XSYMBOL (sym)->value, Lisp_Symbol_Value_Magic, bfwd);
    return (sym);
  }
}

DEFUN ("make-local-variable", Fmake_local_variable, Smake_local_variable,
  1, 1, "vMake Local Variable: ",
  "Make VARIABLE have a separate value in the current buffer.\n\
Other buffers will continue to share a common default value.\n\
See also `make-variable-buffer-local'.\n\
\n\
If the variable is already arranged to become local when set,\n\
this function causes a local value to exist for this buffer,\n\
just as if the variable were set.")
  (sym)
     register Lisp_Object sym;
{
  Lisp_Object valcontents;

  CHECK_SYMBOL (sym, 0);

  if (EQ (sym, Qnil) || EQ (sym, Qt))
    {
    lose:
      signal_error (Qerror, 
                    list2 (build_string ("Symbol may not be buffer-local"),
                           sym));
    }

  valcontents = XSYMBOL (sym)->value;

  if (SYMBOL_VALUE_MAGIC_P (valcontents))
    {
      switch (XSYMBOL_VALUE_MAGIC_TYPE (valcontents))
	{
	case fixnum_forward:
	case boolean_forward:
	case object_forward:
	case unbound_marker:
          break;

        case default_buffer_forward:
          goto lose;

	case buffer_local:
	case current_buffer_forward:
	  {
	    /* Make sure the symbol has a local value in this particular
	       buffer, by setting it to the same value it already has.  */
	    Lisp_Object tem = Fboundp (sym);
	    Fset (sym, (!NILP (tem) ? Fsymbol_value (sym) : Qunbound));
	    return (sym);
	  }

	case some_buffer_local:
	  goto already_some_buffer_local;
	}
    }

  /* Make sure sym is set up to hold per-buffer values */
  {
    struct symbol_value_buffer_local *bfwd
      = alloc_lcrecord (sizeof (struct symbol_value_buffer_local),
                        lrecord_symbol_value_buffer_local);
    bfwd->magic.type = some_buffer_local;

    bfwd->current_value = ((EQ (valcontents, Qunbound))
                           ? Qnil 
                           : valcontents);
    bfwd->current_buffer = Qnil;
    bfwd->current_alist_element = Qnil;
    bfwd->default_value = do_symval_forwarding (valcontents, current_buffer);
    if (EQ (bfwd->default_value, Qunbound))
      /* Can't be allowed to be unbound, for compatibility with exising code */
      bfwd->default_value = Qnil;
    XSETR (valcontents, Lisp_Symbol_Value_Magic, bfwd);
    XSYMBOL (sym)->value = valcontents;
  }

 already_some_buffer_local:
  /* Make sure this buffer has its own value of sym */
  {
    struct symbol_value_buffer_local *bfwd 
      = XSYMBOL_VALUE_BUFFER_LOCAL (valcontents);
    Lisp_Object tem = Fassq (sym, current_buffer->local_var_alist);

    if (NILP (tem))
      {
	current_buffer->local_var_alist
	  = Fcons (Fcons (sym, bfwd->default_value),
		   current_buffer->local_var_alist);

	/* Make sure symbol does not think it is set up for this buffer;
	   force it to look once again for this buffer's value */
	if (current_buffer == XBUFFER (bfwd->current_buffer))
	  bfwd->current_buffer = Qnil;
      }

    /* If the symbol forwards into a C variable, then swap in the
       variable for this buffer immediately.  If C code modifies the
       variable before we swap in, then that new value will clobber the
       default value the next time we swap.  */
    if (SYMBOL_VALUE_MAGIC_P (bfwd->current_value))
      {
	switch (XSYMBOL_VALUE_MAGIC_TYPE (bfwd->current_value))
	  {
	  case fixnum_forward:
	  case boolean_forward:
	  case object_forward:
          case default_buffer_forward:
	    swap_in_symval_forwarding (sym, bfwd);
	    break;

	  case unbound_marker:
	  case current_buffer_forward:
	  case buffer_local:
	  case some_buffer_local:
	    break;
	  }
      }
  }

  return (sym);
}

DEFUN ("kill-local-variable", Fkill_local_variable, Skill_local_variable,
  1, 1, "vKill Local Variable: ",
  "Make VARIABLE no longer have a separate value in the current buffer.\n\
From now on the default value will apply in this buffer.")
  (sym)
     register Lisp_Object sym;
{
  register Lisp_Object valcontents;

  CHECK_SYMBOL (sym, 0);

  valcontents = XSYMBOL (sym)->value;

  if (!SYMBOL_VALUE_MAGIC_P (valcontents))
    return (sym);

  switch (XSYMBOL_VALUE_MAGIC_TYPE (valcontents))
    {
    case fixnum_forward:
    case boolean_forward:
    case object_forward:
    case default_buffer_forward:
    case unbound_marker:
      return (sym);

    case current_buffer_forward:
      {
        const struct symbol_value_forward *fwd
          = XSYMBOL_VALUE_FORWARD (valcontents);
        register int offset = ((char *)symbol_value_forward_forward (fwd) 
                               - (char *)&buffer_local_flags);
        register int mask = *((int *)symbol_value_forward_forward (fwd));

        if (mask > 0)
        {
          *(Lisp_Object *)(offset + (char *) current_buffer)
            = *(Lisp_Object *)(offset + (char *) XBUFFER (Vbuffer_defaults));
          current_buffer->local_var_flags &= ~mask;
        }
        return (sym);
      }

    case buffer_local:
    case some_buffer_local:
      {
        /* Get rid of this buffer's alist element, if any */
        struct symbol_value_buffer_local *bfwd
          = XSYMBOL_VALUE_BUFFER_LOCAL (valcontents);
        Lisp_Object tem = Fassq (sym, current_buffer->local_var_alist);

        if (!NILP (tem))
          current_buffer->local_var_alist 
            = Fdelq (tem, current_buffer->local_var_alist);

        /* Make sure symbol does not think it is set up for this buffer;
           force it to look once again for this buffer's value */
        if (current_buffer == XBUFFER (bfwd->current_buffer))
          bfwd->current_buffer = Qnil;

        /* In case it's a C variable, flush it out. */
        swap_in_symval_forwarding (sym, bfwd);
      }
      return (sym);
    default:
      abort ();
    }
}



/**********************************************************************/
/* Lisp initialisation stuff                                          */
/**********************************************************************/


void
defsymbol (Lisp_Object *location, const char *name)
{
  *location = Fintern (make_pure_pname (name, strlen (name), 1),
                       Qnil);
  staticpro (location);
}


void
defsubr (struct Lisp_Subr *subr)
{
  Lisp_Object sym = intern (subr_name (subr));

  /* Check that nobody spazzed */
  if (subr->max_args != MANY && subr->max_args != UNEVALLED)
  {
    if (subr->max_args > SUBR_MAX_ARGS /* Need to fix eval.c if so */
        || subr->max_args < subr->min_args)
      abort ();
  }
  if (subr->min_args < 0 || subr->min_args > SUBR_MAX_ARGS)
    abort ();

  if (!EQ (XSYMBOL (sym)->function, Qunbound)) abort ();

  XSETR (XSYMBOL (sym)->function, Lisp_Subr, subr);
}

#define OBARRAY_SIZE 509

#ifndef Qzero
Lisp_Object Qzero;
#endif

/* some losing systems can't have static vars at function scope... */
static struct symbol_value_magic guts_of_unbound_marker =
  { { { lrecord_symbol_value_forward }, 0, 69}, unbound_marker };

void
init_symbols ()
{
  Qnil = Fmake_symbol (make_pure_pname ("nil", 3, 1));
  /* Bootstrapping problem: Qnil isn't set when make_pure_pname is
     called the first time. */
  XSYMBOL (Qnil)->name->dup_list = Qnil;
  XSYMBOL (Qnil)->value = Qnil; /* Nihil ex hihil */
  XSYMBOL (Qnil)->plist = Qnil;

#ifndef Qzero
  Qzero = make_number (0);      /* Only used if Lisp_Object is a union type */
#endif

  Vobarray = make_vector (OBARRAY_SIZE, Qzero);
  initial_obarray = Vobarray;
  staticpro (&initial_obarray);
  /* Intern nil in the obarray */
  {
    /* These locals are to kludge around a pyramid compiler bug. */
    int hash;
    Lisp_Object *tem;

    hash = hash_string (XSYMBOL (Qnil)->name->data, 3);
    /* Separate statement here to avoid VAXC bug. */
    hash %= OBARRAY_SIZE;
    tem = &XVECTOR (Vobarray)->contents[hash];
    *tem = Qnil;
  }

  XSETR (Qunbound, Lisp_Symbol_Value_Magic, &guts_of_unbound_marker);
    if ((const void *) XPNTR (Qunbound) != (const void *) &guts_of_unbound_marker)
  {
    /* This might happen on DATA_SEG_BITS machines. */
    /* abort (); */
    /* Can't represent a pointer to constant C data using a Lisp_Object.
       So heap-allocate it. */
    struct symbol_value_magic *urk = xmalloc (sizeof (*urk));
    memcpy (urk, &guts_of_unbound_marker, sizeof (*urk));
    XSETR (Qunbound, Lisp_Symbol_Value_Magic, urk);
  }

  XSYMBOL (Qnil)->function = Qunbound;

  defsymbol (&Qt, "t");
  XSYMBOL (Qt)->value = Qt;     /* Veritas aetera */

  defsymbol (&Qvariable_documentation, "variable-documentation");
}

/* Create and initialise a variable whose value is forwarded to C data */
void
defvar_mumble (const char *namestring, 
               const void *magic, int sizeof_magic)
{
  Lisp_Object kludge;
  Lisp_Object sym = Fintern (make_pure_pname (namestring, strlen (namestring),
                                              1), Qnil);

  /* Check that magic points somewhere we can represent as a Lisp pointer */
  XSETR (kludge, 0, magic);
  if (magic != (const void *) XPNTR (kludge))
  {
    /* This might happen on DATA_SEG_BITS machines. */
    /* abort (); */
    /* Copy it to somewhere which is representable. */
    void *f = xmalloc (sizeof_magic);
    memcpy (f, magic, sizeof_magic);
    XSET (XSYMBOL (sym)->value, Lisp_Record, f);
  }
  else
    XSET (XSYMBOL (sym)->value, Lisp_Record, magic);
}


void
syms_of_symbols ()
{
  defsubr (&Sintern);
  defsubr (&Sintern_soft);
  defsubr (&Smapatoms);
  defsubr (&Sapropos_internal);

  defsubr (&Ssymbol_function);
  defsubr (&Ssymbol_plist);
  defsubr (&Ssymbol_name);
  defsubr (&Smakunbound);
  defsubr (&Sfmakunbound);
  defsubr (&Sboundp);
  defsubr (&Sglobally_boundp);
  defsubr (&Sfboundp);
  defsubr (&Sfset);
  defsubr (&Sdefine_function);
  defsubr (&Ssetplist);
  defsubr (&Ssymbol_value_in_buffer);
  defsubr (&Ssymbol_value);
  defsubr (&Sset);
  defsubr (&Sdefault_boundp);
  defsubr (&Sdefault_value);
  defsubr (&Sset_default);
  defsubr (&Ssetq_default);
  defsubr (&Smake_variable_buffer_local);
  defsubr (&Smake_local_variable);
  defsubr (&Skill_local_variable);

  DEFVAR_LISP ("obarray", &Vobarray,
    "Symbol table for use by `intern' and `read'.\n\
It is a vector whose length ought to be prime for best results.\n\
The vector's contents don't make sense if examined from Lisp programs;\n\
to find all the symbols in an obarray, use `mapatoms'.");

}
