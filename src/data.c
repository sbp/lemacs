/* Primitive operations on Lisp data types for GNU Emacs Lisp interpreter.
   Copyright (C) 1985, 1986, 1988, 1992, 1993, 1994
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

#include <stdio.h>		/* For sprintf */

#include "lisp.h"
#include "intl.h"

#include "syssignal.h"

#include "bytecode.h"

#ifdef LISP_FLOAT_TYPE

/* Work around a problem that happens because math.h on hpux 7
   defines two static variables--which, in Emacs, are not really static,
   because `static' is defined as nothing.  The problem is that they are
   here, in floatfns.c, and in lread.c.
   These macros prevent the name conflict.

   (Is it still necessary to define static to nothing on hpux7?
   Removing that would be the best fix. -jwz)
 */
# if defined (HPUX) && !defined (HPUX8)
#  define _MAXLDBL data_c_maxldbl
#  define _NMAXLDBL data_c_nmaxldbl
# endif

#include <math.h>

#endif /* LISP_FLOAT_TYPE */

Lisp_Object Qnil, Qt, Qquote, Qlambda, Qfunction, Qunbound;
Lisp_Object Qerror_conditions, Qerror_message, Qtop_level;
Lisp_Object Qsignal, Qerror, Qquit, Qwrong_type_argument, Qargs_out_of_range;
Lisp_Object Qvoid_variable, Qvoid_function, Qcyclic_function_indirection;
Lisp_Object Qsetting_constant, Qinvalid_read_syntax;
Lisp_Object Qinvalid_function, Qwrong_number_of_arguments, Qno_catch;
Lisp_Object Qend_of_file;
Lisp_Object Qarith_error, Qrange_error, Qdomain_error;
Lisp_Object Qsingularity_error, Qoverflow_error, Qunderflow_error;
Lisp_Object Qbeginning_of_buffer, Qend_of_buffer, Qbuffer_read_only;
Lisp_Object Qintegerp, Qnatnump, Qsymbolp, Qlistp, Qconsp, Qsubrp;
Lisp_Object Qstringp, Qarrayp, Qsequencep, Qbufferp, Qbytecodep;
Lisp_Object Qchar_or_string_p, Qmarkerp, Qinteger_or_marker_p, Qvectorp;
Lisp_Object Qboundp, Qfboundp;
Lisp_Object Qcdr;

Lisp_Object Qdefault;
Lisp_Object Qignore;

#ifdef LISP_FLOAT_TYPE
Lisp_Object Qfloatp;
#endif
Lisp_Object Qnumberp, Qnumber_or_marker_p;


Lisp_Object
wrong_type_argument (predicate, value)
     register Lisp_Object predicate, value;
{
  register Lisp_Object tem;
  do
    {
#ifdef MOCKLISP_SUPPORT
      if (!EQ (Vmocklisp_arguments, Qt))
	{
	 if (STRINGP (value) &&
	     (EQ (predicate, Qintegerp) ||
	      EQ (predicate, Qinteger_or_marker_p)))
	   return Fstring_to_number (value);
	 if (FIXNUMP (value) && EQ (predicate, Qstringp))
	   return Fnumber_to_string (value);
	}
#endif
      value = Fsignal (Qwrong_type_argument, list2 (predicate, value));
      tem = call1 (predicate, value);
    }
  while (NILP (tem));
  return value;
}

DOESNT_RETURN
pure_write_error ()
{
  error (GETTEXT ("Attempt to modify read-only object"));
}

DOESNT_RETURN
args_out_of_range (a1, a2)
     Lisp_Object a1, a2;
{
  signal_error (Qargs_out_of_range, list2 (a1, a2));
}

DOESNT_RETURN
args_out_of_range_3 (a1, a2, a3)
     Lisp_Object a1, a2, a3;
{
  signal_error (Qargs_out_of_range, list3 (a1, a2, a3));
}

#ifndef make_number
Lisp_Object
make_number (LISP_WORD_TYPE num)
{
  Lisp_Object val;
  XSET (val, Lisp_Int, num);
  return val;
}
#endif /* ! defined (make_number) */

/* On some machines, XINT needs a temporary location.
   Here it is, in case it is needed.  */

LISP_WORD_TYPE sign_extend_temp;

/* On a few machines, XINT can only be done by calling this.  */

int
sign_extend_lisp_int (LISP_WORD_TYPE num)
{
  if (num & (1L << (VALBITS - 1)))
    return num | ((-1L) << VALBITS);
  else
    return num & ((1L << VALBITS) - 1);
}

/* Data type predicates */

DEFUN ("eq", Feq, Seq, 2, 2, 0,
  "T if the two args are the same Lisp object.")
  (obj1, obj2)
     Lisp_Object obj1, obj2;
{
  if (EQ (obj1, obj2))
    return Qt;
  return Qnil;
}

DEFUN ("null", Fnull, Snull, 1, 1, 0, "T if OBJECT is nil.")
  (object)
     Lisp_Object object;
{
  if (NILP (object))
    return Qt;
  return Qnil;
}

DEFUN ("consp", Fconsp, Sconsp, 1, 1, 0, "T if OBJECT is a cons cell.")
  (object)
     Lisp_Object object;
{
  if (CONSP (object))
    return Qt;
  return Qnil;
}

DEFUN ("atom", Fatom, Satom, 1, 1, 0, "T if OBJECT is not a cons cell.  This includes nil.")
  (object)
     Lisp_Object object;
{
  if (CONSP (object))
    return Qnil;
  return Qt;
}

DEFUN ("listp", Flistp, Slistp, 1, 1, 0, "T if OBJECT is a list.  This includes nil.")
  (object)
     Lisp_Object object;
{
  if (CONSP (object) || NILP (object))
    return Qt;
  return Qnil;
}

DEFUN ("nlistp", Fnlistp, Snlistp, 1, 1, 0, "T if OBJECT is not a list.  Lists include nil.")
  (object)
     Lisp_Object object;
{
  if (CONSP (object) || NILP (object))
    return Qnil;
  return Qt;
}

DEFUN ("symbolp", Fsymbolp, Ssymbolp, 1, 1, 0, "T if OBJECT is a symbol.")
  (object)
     Lisp_Object object;
{
  if (SYMBOLP (object))
    return Qt;
  return Qnil;
}

DEFUN ("vectorp", Fvectorp, Svectorp, 1, 1, 0, "T if OBJECT is a vector.")
  (object)
     Lisp_Object object;
{
  if (VECTORP (object))
    return Qt;
  return Qnil;
}

DEFUN ("stringp", Fstringp, Sstringp, 1, 1, 0, "T if OBJECT is a string.")
  (object)
     Lisp_Object object;
{
  if (STRINGP (object))
    return Qt;
  return Qnil;
}

DEFUN ("arrayp", Farrayp, Sarrayp, 1, 1, 0, "T if OBJECT is an array (string or vector).")
  (object)
     Lisp_Object object;
{
  if (VECTORP (object) || STRINGP (object))
    return Qt;
  return Qnil;
}

DEFUN ("sequencep", Fsequencep, Ssequencep, 1, 1, 0,
  "T if OBJECT is a sequence (list or array).")
  (object)
     Lisp_Object object;
{
  if (CONSP (object) || NILP (object) 
      || VECTORP (object) || STRINGP (object))
    return Qt;
  return Qnil;
}

DEFUN ("markerp", Fmarkerp, Smarkerp, 1, 1, 0, "T if OBJECT is a marker (editor pointer).")
  (object)
     Lisp_Object object;
{
  if (MARKERP (object))
    return Qt;
  return Qnil;
}

DEFUN ("subrp", Fsubrp, Ssubrp, 1, 1, 0, "T if OBJECT is a built-in function.")
  (object)
     Lisp_Object object;
{
  if (SUBRP (object))
    return Qt;
  return Qnil;
}

DEFUN ("subr-min-args", Fsubr_min_args, Ssubr_min_args, 1, 1, 0,
   "Returns minimum number of args built-in function SUBR may be called with.")
  (subr)
     Lisp_Object subr;
{
  CHECK_SUBR (subr, 0);
  return make_number (XSUBR (subr)->min_args);
}

DEFUN ("subr-max-args", Fsubr_max_args, Ssubr_max_args, 1, 1, 0,
 "Returns maximum number of args built-in function SUBR may be called with,\n\
or nil if it takes an arbitrary number of arguments (or is a special form.)")
  (subr)
     Lisp_Object subr;
{
  int nargs;
  CHECK_SUBR (subr, 0);
  nargs = XSUBR (subr)->max_args;
  if (nargs == MANY || nargs == UNEVALLED)
    return Qnil;
  else
    return make_number (nargs);
}

DEFUN ("compiled-function-p", Fcompiled_function_p, Scompiled_function_p, 1, 1, 0, 
       "T if OBJECT is a byte-compiled function object.")
  (object)
     Lisp_Object object;
{
  if (COMPILEDP (object))
    return Qt;
  return Qnil;
}

DEFUN ("char-or-string-p", Fchar_or_string_p, Schar_or_string_p, 1, 1, 0, "T if OBJECT is a character (a number) or a string.")
  (object)
     Lisp_Object object;
{
  if (FIXNUMP (object) || STRINGP (object))
    return Qt;
  return Qnil;
}

DEFUN ("integerp", Fintegerp, Sintegerp, 1, 1, 0, "T if OBJECT is a number.")
  (object)
     Lisp_Object object;
{
  if (FIXNUMP (object))
    return Qt;
  return Qnil;
}

DEFUN ("integer-or-marker-p", Finteger_or_marker_p, Sinteger_or_marker_p, 1, 1, 0,
  "T if OBJECT is an integer or a marker (editor pointer).")
  (object)
     Lisp_Object object;
{
  if (FIXNUMP (object) || MARKERP (object))
    return Qt;
  return Qnil;
}

DEFUN ("natnump", Fnatnump, Snatnump, 1, 1, 0, "T if OBJECT is a nonnegative number.")
  (object)
     Lisp_Object object;
{
  if (FIXNUMP (object) && XINT (object) >= 0)
    return Qt;
  return Qnil;
}

DEFUN ("numberp", Fnumberp, Snumberp, 1, 1, 0,
       "T if OBJECT is a number (floating point or integer).")
  (object)
     Lisp_Object object;
{
  if (NUMBERP (object))
    return Qt;
  return Qnil;
}

DEFUN ("number-or-marker-p", Fnumber_or_marker_p,
       Snumber_or_marker_p, 1, 1, 0,
       "T if OBJECT is a number or a marker.")
  (object)
     Lisp_Object object;
{
  if (NUMBERP (object)
      || MARKERP (object))
    return Qt;
  return Qnil;
}

#ifdef LISP_FLOAT_TYPE
DEFUN ("floatp", Ffloatp, Sfloatp, 1, 1, 0,
       "T if OBJECT is a floating point number.")
  (object)
     Lisp_Object object;
{
  if (FLOATP (object))
    return Qt;
  return Qnil;
}
#endif /* LISP_FLOAT_TYPE */


/* Extract and set components of lists */

DEFUN ("car", Fcar, Scar, 1, 1, 0,
  "Return the car of CONSCELL.  If arg is nil, return nil.\n\
Error if arg is not nil and not a cons cell.  See also `car-safe'.")
  (list)
     register Lisp_Object list;
{
  while (1)
    {
      if (CONSP (list))
	return XCONS (list)->car;
      else if (EQ (list, Qnil))
	return Qnil;
      else
	list = wrong_type_argument (Qlistp, list);
    }
}

DEFUN ("car-safe", Fcar_safe, Scar_safe, 1, 1, 0,
  "Return the car of OBJECT if it is a cons cell, or else nil.")
  (object)
     Lisp_Object object;
{
  if (CONSP (object))
    return XCONS (object)->car;
  else
    return Qnil;
}

DEFUN ("cdr", Fcdr, Scdr, 1, 1, 0,
  "Return the cdr of CONSCELL.  If arg is nil, return nil.\n\
Error if arg is not nil and not a cons cell.  See also `cdr-safe'.")

  (list)
     register Lisp_Object list;
{
  while (1)
    {
      if (CONSP (list))
	return XCONS (list)->cdr;
      else if (EQ (list, Qnil))
	return Qnil;
      else
	list = wrong_type_argument (Qlistp, list);
    }
}

DEFUN ("cdr-safe", Fcdr_safe, Scdr_safe, 1, 1, 0,
  "Return the cdr of OBJECT if it is a cons cell, or else  nil.")
  (object)
     Lisp_Object object;
{
  if (CONSP (object))
    return XCONS (object)->cdr;
  else
    return Qnil;
}

DEFUN ("setcar", Fsetcar, Ssetcar, 2, 2, 0,
  "Set the car of CONSCELL to be NEWCAR.  Returns NEWCAR.")
  (cell, newcar)
     register Lisp_Object cell, newcar;
{
  if (!CONSP (cell))
    cell = wrong_type_argument (Qconsp, cell);

  CHECK_IMPURE (cell);
  XCONS (cell)->car = newcar;
  return newcar;
}

DEFUN ("setcdr", Fsetcdr, Ssetcdr, 2, 2, 0,
  "Set the cdr of CONSCELL to be NEWCDR.  Returns NEWCDR.")
  (cell, newcdr)
     register Lisp_Object cell, newcdr;
{
  if (!CONSP (cell))
    cell = wrong_type_argument (Qconsp, cell);

  CHECK_IMPURE (cell);
  XCONS (cell)->cdr = newcdr;
  return newcdr;
}

/* Find the function at the end of a chain of symbol function indirections.  */

/* If OBJECT is a symbol, find the end of its function chain and
   return the value found there.  If OBJECT is not a symbol, just
   return it.  If there is a cycle in the function chain, signal a
   cyclic-function-indirection error.

   This is like Findirect_function, except that it doesn't signal an
   error if the chain ends up unbound.  */
Lisp_Object
indirect_function (Lisp_Object object, int error)
{
  Lisp_Object tortoise = object; 
  Lisp_Object hare = object;

  for (;;)
    {
      if (!SYMBOLP (hare) || EQ (hare, Qunbound))
	break;
      hare = XSYMBOL (hare)->function;
      if (!SYMBOLP (hare) || EQ (hare, Qunbound))
	break;
      hare = XSYMBOL (hare)->function;

      tortoise = XSYMBOL (tortoise)->function;

      if (EQ (hare, tortoise))
	return (Fsignal (Qcyclic_function_indirection, list1 (object)));
    }

  if (EQ (hare, Qunbound) && error)
    return Fsignal (Qvoid_function, list1 (object));
  return hare;
}

DEFUN ("indirect-function", Findirect_function, Sindirect_function, 1, 1, 0,
  "Return the function at the end of OBJECT's function chain.\n\
If OBJECT is a symbol, follow all function indirections and return\n\
the final function binding.\n\
If OBJECT is not a symbol, just return it.\n\
Signal a void-function error if the final symbol is unbound.\n\
Signal a cyclic-function-indirection error if there is a loop in the\n\
function chain of symbols.")
  (object)
    register Lisp_Object object;
{
  return indirect_function (object, 1);
}

/* Extract and set vector and string elements */

DEFUN ("aref", Faref, Saref, 2, 2, 0,
  "Return the element of ARRAY at index INDEX.\n\
ARRAY may be a vector or a string, or a byte-code object.  INDEX starts at 0.")
  (array, idx)
     register Lisp_Object array;
     Lisp_Object idx;
{
  register int idxval;

 retry:
  CHECK_FIXNUM (idx, 1);
  idxval = XINT (idx);
  if (idxval < 0)
    {
    lose:
      args_out_of_range (array, idx);
    }
  if (VECTORP (array))
    {
      if (idxval >= vector_length (XVECTOR (array))) goto lose;
      return XVECTOR (array)->contents[idxval];
    }
  else if (STRINGP (array))
    {
      if (idxval >= string_length (XSTRING (array))) goto lose;
      return (make_number ((unsigned char) XSTRING (array)->data[idxval]));
    }
  else if (COMPILEDP (array))
    {
      /* Weird, gross compatibility kludge */
      return (Felt (array, idx));
    }
  else
    {
      array = wrong_type_argument (Qarrayp, array);
      goto retry;
    }
}

DEFUN ("aset", Faset, Saset, 3, 3, 0,
  "Store into the element of ARRAY at index INDEX the value NEWVAL.\n\
ARRAY may be a vector or a string.  INDEX starts at 0.")
  (array, idx, newelt)
     register Lisp_Object array;
     Lisp_Object idx, newelt;
{
  register int idxval;

  CHECK_FIXNUM (idx, 1);
  if (!VECTORP (array) && !STRINGP (array))
    array = wrong_type_argument (Qarrayp, array);

  idxval = XINT (idx);
  if (idxval < 0)
    {
    lose:
      args_out_of_range (array, idx);
    }
  CHECK_IMPURE (array);

  if (VECTORP (array))
    {
      if (idxval >= vector_length (XVECTOR (array))) goto lose;
      XVECTOR (array)->contents[idxval] = newelt;
    }
  else                          /* string */
    {
      CHECK_FIXNUM (newelt, 2);
      if (idxval >= string_length (XSTRING (array))) goto lose;
      XSTRING (array)->data[idxval] = XINT (newelt);
    }

  return newelt;
}


/* Function objects */

/* The bytecode->doc_and_interactive slot uses the minimal number of conses,
   based on bytecode->flags; it may take any of the following forms:
	doc
	interactive
	domain
	(doc . interactive)
	(doc . domain)
	(interactive . domain)
	(doc . (interactive . domain))
 */

/* Caller must check flags.interactivep first */
Lisp_Object
bytecode_interactive (struct Lisp_Bytecode *b)
{
  if (! b->flags.interactivep)
    abort ();
  else if (b->flags.documentationp && b->flags.domainp)
    return (XCONS (XCONS (b->doc_and_interactive)->cdr)->car);
  else if (b->flags.documentationp)
    return (XCONS (b->doc_and_interactive)->cdr);
  else if (b->flags.domainp)
    return (XCONS (b->doc_and_interactive)->car);
  else
    return (b->doc_and_interactive);
}

/* Caller need not check flags.documentationp first */
Lisp_Object
bytecode_documentation (struct Lisp_Bytecode *b)
{
  if (! b->flags.documentationp)
    return Qnil;
  else if (b->flags.interactivep && b->flags.domainp)
    return (XCONS (b->doc_and_interactive)->car);
  else if (b->flags.interactivep)
    return (XCONS (b->doc_and_interactive)->car);
  else if (b->flags.domainp)
    return (XCONS (b->doc_and_interactive)->car);
  else
    return (b->doc_and_interactive);
}

/* Caller need not check flags.domainp first */
Lisp_Object
bytecode_domain (struct Lisp_Bytecode *b)
{
  if (! b->flags.domainp)
    return Qnil;
  else if (b->flags.documentationp && b->flags.interactivep)
    return (XCONS (XCONS (b->doc_and_interactive)->cdr)->cdr);
  else if (b->flags.documentationp)
    return (XCONS (b->doc_and_interactive)->cdr);
  else if (b->flags.interactivep)
    return (XCONS (b->doc_and_interactive)->cdr);
  else
    return (b->doc_and_interactive);
}

/* used only by Snarf-documentation; there must be doc already. */
void
set_bytecode_documentation (struct Lisp_Bytecode *b, Lisp_Object new)
{
  if (! b->flags.documentationp)
    abort ();
  else if (!(FIXNUMP (new) || STRINGP (new)))
    abort ();
  else if (b->flags.interactivep && b->flags.domainp)
    XCONS (b->doc_and_interactive)->car = new;
  else if (b->flags.interactivep)
    XCONS (b->doc_and_interactive)->car = new;
  else if (b->flags.domainp)
    XCONS (b->doc_and_interactive)->car = new;
  else
    b->doc_and_interactive = new;
}

DEFUN ("compiled-function-instructions", Fcompiled_function_instructions,
       Scompiled_function_instructions, 1, 1, 0,
       "Returns the byte-opcode string of the compiled-function object.")
     (function)
     Lisp_Object function;
{
  CHECK_BYTECODE (function, 0);
  return (XBYTECODE (function)->bytecodes);
}

DEFUN ("compiled-function-constants", Fcompiled_function_constants,
       Scompiled_function_constants, 1, 1, 0,
       "Returns the constants vector of the compiled-function object.")
     (function)
     Lisp_Object function;
{
  CHECK_BYTECODE (function, 0);
  return (XBYTECODE (function)->constants);
}

DEFUN ("compiled-function-stack-depth", Fcompiled_function_stack_depth,
       Scompiled_function_stack_depth, 1, 1, 0,
       "Returns the max stack depth of the compiled-function object.")
     (function)
     Lisp_Object function;
{
  CHECK_BYTECODE (function, 0);
  return (make_number (XBYTECODE (function)->maxdepth));
}

DEFUN ("compiled-function-arglist", Fcompiled_function_arglist,
       Scompiled_function_arglist, 1, 1, 0,
       "Returns the argument list of the compiled-function object.")
     (function)
     Lisp_Object function;
{
  CHECK_BYTECODE (function, 0);
  return (XBYTECODE (function)->arglist);
}

DEFUN ("compiled-function-interactive", Fcompiled_function_interactive,
       Scompiled_function_interactive, 1, 1, 0,
       "Returns the interactive spec of the compiled-function object, or nil.")
     (function)
     Lisp_Object function;
{
  CHECK_BYTECODE (function, 0);
  if (!XBYTECODE (function)->flags.interactivep)
    return Qnil;
  return (list2 (Qinteractive, bytecode_interactive (XBYTECODE (function))));
}

DEFUN ("compiled-function-domain", Fcompiled_function_domain,
       Scompiled_function_domain, 1, 1, 0,
       "Returns the domain of the compiled-function object, or nil.\n\
This is only meaningful if I18N3 was enabled when emacs was compiled.")
     (function)
     Lisp_Object function;
{
  CHECK_BYTECODE (function, 0);
  if (!XBYTECODE (function)->flags.domainp)
    return Qnil;
  return (bytecode_domain (XBYTECODE (function)));
}


/* Arithmetic functions */

enum comparison { equal, notequal, less, grtr, less_or_equal, grtr_or_equal };

static Lisp_Object
arithcompare (Lisp_Object num1, Lisp_Object num2, 
              enum comparison comparison)
{
  double f1, f2;
  int floatp = 0;

  CHECK_NUMBER_COERCE_MARKER (num1, 0);
  CHECK_NUMBER_COERCE_MARKER (num2, 0);

#ifdef LISP_FLOAT_TYPE
  if (FLOATP (num1) || FLOATP (num2))
    {
      floatp = 1;
      f1 = (FLOATP (num1)) ? float_data (XFLOAT (num1)) : XINT (num1);
      f2 = (FLOATP (num2)) ? float_data (XFLOAT (num2)) : XINT (num2);
    }
#endif /* LISP_FLOAT_TYPE */

  switch (comparison)
    {
    case equal:
      if ((floatp) ? (f1 == f2) : (XINT (num1) == XINT (num2)))
	return Qt;
      return Qnil;

    case notequal:
      if ((floatp) ? (f1 != f2) : (XINT (num1) != XINT (num2)))
	return Qt;
      return Qnil;

    case less:
      if ((floatp) ? (f1 < f2) : (XINT (num1) < XINT (num2)))
	return Qt;
      return Qnil;

    case less_or_equal:
      if ((floatp) ? (f1 <= f2) : (XINT (num1) <= XINT (num2)))
	return Qt;
      return Qnil;

    case grtr:
      if ((floatp) ? (f1 > f2) : (XINT (num1) > XINT (num2)))
	return Qt;
      return Qnil;

    case grtr_or_equal:
      if ((floatp) ? (f1 >= f2) : (XINT (num1) >= XINT (num2)))
	return Qt;
      return Qnil;
    }
  abort ();
}

DEFUN ("=", Feqlsign, Seqlsign, 2, 2, 0,
  "T if two args, both numbers or markers, are equal.")
  (num1, num2)
     register Lisp_Object num1, num2;
{
  return arithcompare (num1, num2, equal);
}

DEFUN ("<", Flss, Slss, 2, 2, 0,
  "T if first arg is less than second arg.  Both must be numbers or markers.")
  (num1, num2)
     register Lisp_Object num1, num2;
{
  return arithcompare (num1, num2, less);
}

DEFUN (">", Fgtr, Sgtr, 2, 2, 0,
  "T if first arg is greater than second arg.  Both must be numbers or markers.")
  (num1, num2)
     register Lisp_Object num1, num2;
{
  return arithcompare (num1, num2, grtr);
}

DEFUN ("<=", Fleq, Sleq, 2, 2, 0,
  "T if first arg is less than or equal to second arg.\n\
Both must be numbers or markers.")
  (num1, num2)
     register Lisp_Object num1, num2;
{
  return arithcompare (num1, num2, less_or_equal);
}

DEFUN (">=", Fgeq, Sgeq, 2, 2, 0,
  "T if first arg is greater than or equal to second arg.\n\
Both must be numbers or markers.")
  (num1, num2)
     register Lisp_Object num1, num2;
{
  return arithcompare (num1, num2, grtr_or_equal);
}

DEFUN ("/=", Fneq, Sneq, 2, 2, 0,
  "T if first arg is not equal to second arg.  Both must be numbers or markers.")
  (num1, num2)
     register Lisp_Object num1, num2;
{
  return arithcompare (num1, num2, notequal);
}

DEFUN ("zerop", Fzerop, Szerop, 1, 1, 0, "T if NUMBER is zero.")
  (number)
     Lisp_Object number;
{
  CHECK_NUMBER (number, 0);

#ifdef LISP_FLOAT_TYPE
  if (FLOATP (number))
    {
      if (float_data (XFLOAT (number)) == 0.0)
	return Qt;
      return Qnil;
    }
#endif /* LISP_FLOAT_TYPE */

  if (XINT (number) == 0)
    return Qt;
  return Qnil;
}

/* Convert between 32-bit values and kludgy pairs of lispy 24-bit values.  */

/* >>> do we want this or lisp_to_word/word_to_lisp ??? */
Lisp_Object
long_to_cons (unsigned long i)
{
  unsigned int top = i >> 16;
  unsigned int bot = i & 0xFFFF;
  if (top == 0)
    return make_number (bot);
  if (top == 0xFFFF)
    return Fcons (make_number (-1), make_number (bot));
  return Fcons (make_number (top), make_number (bot));
}

unsigned long
cons_to_long (Lisp_Object c)
{
  Lisp_Object top, bot;
  if (FIXNUMP (c))
    return XINT (c);
  top = XCONS (c)->car;
  bot = XCONS (c)->cdr;
  if (CONSP (bot))
    bot = XCONS (bot)->car;
  return ((XINT (top) << 16) | XINT (bot));
}

/* Convert 32 bit items <-> (<high16> . <low16>) */
/* #### Probably this needs to be more clever on 64 bit machines */

Lisp_Object
word_to_lisp (unsigned int item)
{
  return Fcons (make_number (item >> 16), make_number (item & 0xffff));
}

unsigned int
lisp_to_word (Lisp_Object obj)
{
  Lisp_Object high;
  Lisp_Object low;
  if ((NILP (obj)) || (!CONSP (obj))) return 0;
  
  high = XCONS (obj)->car;
  if (!FIXNUMP (high)) return 0;
  
  low = XCONS (obj)->cdr; 
  if (!FIXNUMP (low)) return 0;
  
  return ((((unsigned int) (((unsigned int) (XINT (high))) << 16))
           | ((unsigned int) XINT ((low)))));
}


DEFUN ("number-to-string", Fnumber_to_string, Snumber_to_string, 1, 1, 0,
  "Convert NUM to a string by printing it in decimal.\n\
Uses a minus sign if negative.\n\
NUM may be an integer or a floating point number.")
  (num)
     Lisp_Object num;
{
  char buffer[20];

  CHECK_NUMBER (num, 0);

#ifdef LISP_FLOAT_TYPE
  if (FLOATP (num))
    {
      char pigbuf[350];	/* see comments in float_to_string */

      float_to_string (pigbuf, float_data (XFLOAT (num)));
      return build_string (pigbuf);      
    }
#endif /* LISP_FLOAT_TYPE */

  sprintf (buffer, "%d", XINT (num));
  return build_string (buffer);
}

DEFUN ("string-to-number", Fstring_to_number, Sstring_to_number, 1, 1, 0,
  "Convert STRING to a number by parsing it as a decimal number.\n\
This parses both integers and floating point numbers.")
  (string)
     Lisp_Object string;
{
  char *p;
  CHECK_STRING (string, 0);

  p = (char *) XSTRING (string)->data;
  /* Skip any whitespace at the front of the number.  Some versions of
     atoi do this anyway, so we might as well make Emacs lisp consistent.  */
  while (*p == ' ' || *p == '\t')
    p++;

#ifdef LISP_FLOAT_TYPE
  if (isfloat_string (p))
    return make_float (atof (p));
#endif /* LISP_FLOAT_TYPE */

  return make_number (atoi (p));
}
  
enum arithop
  { Aadd, Asub, Amult, Adiv, Alogand, Alogior, Alogxor, Amax, Amin };

#ifdef LISP_FLOAT_TYPE
static Lisp_Object float_arith_driver (double accum, int argnum, 
                                       enum arithop code, 
                                       int nargs, Lisp_Object *args);
#endif


static Lisp_Object
arith_driver (code, nargs, args)
     enum arithop code;
     int nargs;
     register Lisp_Object *args;
{
  Lisp_Object val;
  register int argnum;
  register LISP_WORD_TYPE accum;
  register LISP_WORD_TYPE next;

  switch (code)
    {
    case Alogior:
    case Alogxor:
    case Aadd:
    case Asub:
      accum = 0; break;
    case Amult:
      accum = 1; break;
    case Alogand:
      accum = -1; break;
    case Adiv:
    case Amax:
    case Amin:
      accum = 0;
      break;
    default:
      abort ();
    }

  for (argnum = 0; argnum < nargs; argnum++)
    {
      val = args[argnum];    /* using args[argnum] as argument to CHECK_NUMBER_... */
      CHECK_NUMBER_COERCE_MARKER (val, argnum);

#ifdef LISP_FLOAT_TYPE
      if (FLOATP (val)) /* time to do serious math */
	return (float_arith_driver ((double) accum, argnum, code,
				    nargs, args));
#endif /* LISP_FLOAT_TYPE */
      args[argnum] = val;    /* runs into a compiler bug. */
      next = XINT (args[argnum]);
      switch (code)
	{
	case Aadd: accum += next; break;
	case Asub:
	  if (!argnum && nargs != 1)
	    next = - next;
	  accum -= next;
	  break;
	case Amult: accum *= next; break;
	case Adiv:
	  if (!argnum) accum = next;
	  else
	    {
	      if (next == 0)
		Fsignal (Qarith_error, Qnil);
	      accum /= next;
	    }
	  break;
	case Alogand: accum &= next; break;
	case Alogior: accum |= next; break;
	case Alogxor: accum ^= next; break;
	case Amax: if (!argnum || next > accum) accum = next; break;
	case Amin: if (!argnum || next < accum) accum = next; break;
	}
    }

  XSET (val, Lisp_Int, accum);
  return val;
}

#ifdef LISP_FLOAT_TYPE
static Lisp_Object
float_arith_driver (accum, argnum, code, nargs, args)
     double accum;
     register int argnum;
     enum arithop code;
     int nargs;
     register Lisp_Object *args;
{
  register Lisp_Object val;
  double next;
  
  for (; argnum < nargs; argnum++)
    {
      val = args[argnum];    /* using args[argnum] as argument to CHECK_NUMBER_... */
      CHECK_NUMBER_COERCE_MARKER (val, argnum);

      if (FLOATP (val))
	{
	  next = float_data (XFLOAT (val));
	}
      else
	{
	  args[argnum] = val;    /* runs into a compiler bug. */
	  next = XINT (args[argnum]);
	}
      switch (code)
	{
	case Aadd:
	  accum += next;
	  break;
	case Asub:
	  if (!argnum && nargs != 1)
	    next = - next;
	  accum -= next;
	  break;
	case Amult:
	  accum *= next;
	  break;
	case Adiv:
	  if (!argnum)
	    accum = next;
	  else
	    {
	      if (next == 0)
		Fsignal (Qarith_error, Qnil);
	      accum /= next;
	    }
	  break;
	case Alogand:
	case Alogior:
	case Alogxor:
	  return wrong_type_argument (Qinteger_or_marker_p, val);
	case Amax:
	  if (!argnum || next > accum)
	    accum = next;
	  break;
	case Amin:
	  if (!argnum || next < accum)
	    accum = next;
	  break;
	}
    }

  return make_float (accum);
}
#endif /* LISP_FLOAT_TYPE */

DEFUN ("+", Fplus, Splus, 0, MANY, 0,
  "Return sum of any number of arguments, which are numbers or markers.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return arith_driver (Aadd, nargs, args);
}

DEFUN ("-", Fminus, Sminus, 0, MANY, 0,
  "Negate number or subtract numbers or markers.\n\
With one arg, negates it.  With more than one arg,\n\
subtracts all but the first from the first.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return arith_driver (Asub, nargs, args);
}

DEFUN ("*", Ftimes, Stimes, 0, MANY, 0,
  "Returns product of any number of arguments, which are numbers or markers.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return arith_driver (Amult, nargs, args);
}

DEFUN ("/", Fquo, Squo, 2, MANY, 0,
  "Returns first argument divided by all the remaining arguments.\n\
The arguments must be numbers or markers.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return arith_driver (Adiv, nargs, args);
}

DEFUN ("%", Frem, Srem, 2, 2, 0,
  "Returns remainder of first arg divided by second.\n\
Both must be integers or markers.")
  (num1, num2)
     register Lisp_Object num1, num2;
{
  CHECK_FIXNUM_COERCE_MARKER (num1, 0);
  CHECK_FIXNUM_COERCE_MARKER (num2, 1);

  if (XFASTINT (num2) == 0)
    Fsignal (Qarith_error, Qnil);

  return (make_number (XINT (num1) % XINT (num2)));
}

DEFUN ("mod", Fmod, Smod, 2, 2, 0,
  "Returns X modulo Y.\n\
The result falls between zero (inclusive) and Y (exclusive).\n\
Both X and Y must be numbers or markers.\n\
If either argument is a float, a float will be returned.")
  (num1, num2)
     register Lisp_Object num1, num2;
{
  int i1, i2;

  CHECK_NUMBER_COERCE_MARKER (num1, 0);
  CHECK_NUMBER_COERCE_MARKER (num2, 1);

#ifdef LISP_FLOAT_TYPE
  if (FLOATP (num1) || FLOATP (num2))
    {
      double f1, f2;

      f1 = ((FLOATP (num1)) ? float_data (XFLOAT (num1)) : XINT (num1));
      f2 = ((FLOATP (num2)) ? float_data (XFLOAT (num2)) : XINT (num2));
      if (f2 == 0)
	Fsignal (Qarith_error, Qnil);

      /* Note, ANSI *requires* the presence of the fmod() library routine.
         If your system doesn't have it, complain to your vendor, because
         that is a bug. */
#ifdef USE_DREM
      /* drem returns a result in the range [-f2/2,f2/2] instead of
         [0,f2), but the sign fixup below takes care of that. */
      f1 = drem (f1, f2);
#else
      f1 = fmod (f1, f2); /* fmod is ANSI. */
#endif

      /* If the "remainder" comes out with the wrong sign, fix it.  */
      if ((f1 < 0) != (f2 < 0))
	f1 += f2;
      return (make_float (f1));
    }
#else /* not LISP_FLOAT_TYPE */
  CHECK_NUMBER_COERCE_MARKER (num1, 0);
  CHECK_NUMBER_COERCE_MARKER (num2, 1);
#endif /* not LISP_FLOAT_TYPE */

  i1 = XINT (num1);
  i2 = XINT (num2);

  if (i2 == 0)
    Fsignal (Qarith_error, Qnil);
  
  i1 %= i2;

  /* If the "remainder" comes out with the wrong sign, fix it.  */
  if ((i1 < 0) != (i2 < 0))
    i1 += i2;

  return (make_number (i1));
}


DEFUN ("max", Fmax, Smax, 1, MANY, 0,
  "Return largest of all the arguments (which must be numbers or markers).\n\
The value is always a number; markers are converted to numbers.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return arith_driver (Amax, nargs, args);
}

DEFUN ("min", Fmin, Smin, 1, MANY, 0,
  "Return smallest of all the arguments (which must be numbers or markers).\n\
The value is always a number; markers are converted to numbers.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return arith_driver (Amin, nargs, args);
}

DEFUN ("logand", Flogand, Slogand, 0, MANY, 0,
  "Return bitwise-and of all the arguments.\n\
Arguments may be integers, or markers converted to integers.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return arith_driver (Alogand, nargs, args);
}

DEFUN ("logior", Flogior, Slogior, 0, MANY, 0,
  "Return bitwise-or of all the arguments.\n\
Arguments may be integers, or markers converted to integers.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return arith_driver (Alogior, nargs, args);
}

DEFUN ("logxor", Flogxor, Slogxor, 0, MANY, 0,
  "Return bitwise-exclusive-or of all the arguments.\n\
Arguments may be integers, or markers converted to integers.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return arith_driver (Alogxor, nargs, args);
}

DEFUN ("ash", Fash, Sash, 2, 2, 0,
  "Return VALUE with its bits shifted left by COUNT.\n\
If COUNT is negative, shifting is actually to the right.\n\
In this case, the sign bit is duplicated.")
  (value, count)
     Lisp_Object value, count;
{
  CHECK_FIXNUM (value, 0);
  CHECK_FIXNUM (count, 1);

  if (XINT (count) > 0)
    return (make_number (XINT (value) << XFASTINT (count)));
  else
    return (make_number (XINT (value) >> -XINT (count)));
}

DEFUN ("lsh", Flsh, Slsh, 2, 2, 0,
  "Return VALUE with its bits shifted left by COUNT.\n\
If COUNT is negative, shifting is actually to the right.\n\
In this case,  zeros are shifted in on the left.")
  (value, count)
     Lisp_Object value, count;
{
  Lisp_Object val;

  CHECK_FIXNUM (value, 0);
  CHECK_FIXNUM (count, 1);

  if (XINT (count) > 0)
    XSET (val, Lisp_Int, (unsigned) XFASTINT (value) << XFASTINT (count));
  else
    XSET (val, Lisp_Int, (unsigned) XFASTINT (value) >> -XINT (count));
  return val;
}

DEFUN ("1+", Fadd1, Sadd1, 1, 1, 0,
  "Return NUMBER plus one.  NUMBER may be a number or a marker.\n\
Markers are converted to integers.")
  (number)
     Lisp_Object number;
{
  CHECK_NUMBER_COERCE_MARKER (number, 0);

#ifdef LISP_FLOAT_TYPE
  if (FLOATP (number))
    return (make_float (1.0 + float_data (XFLOAT (number))));
#endif /* LISP_FLOAT_TYPE */

  return (make_number (XINT (number) + 1));
}

DEFUN ("1-", Fsub1, Ssub1, 1, 1, 0,
  "Return NUMBER minus one.  NUMBER may be a number or a marker.\n\
Markers are converted to integers.")
  (number)
     Lisp_Object number;
{
  CHECK_NUMBER_COERCE_MARKER (number, 0);

#ifdef LISP_FLOAT_TYPE
  if (FLOATP (number))
    return (make_float (-1.0 + (float_data (XFLOAT (number)))));
#endif /* LISP_FLOAT_TYPE */

  return (make_number (XINT (number) - 1));
}

DEFUN ("lognot", Flognot, Slognot, 1, 1, 0,
  "Return the bitwise complement of NUMBER.  NUMBER must be an integer.")
  (number)
     Lisp_Object number;
{
  CHECK_FIXNUM (number, 0);
  return (make_number (~XINT (number)));
}



void
syms_of_data ()
{
  defsymbol (&Qquote, "quote");
  defsymbol (&Qlambda, "lambda");
  defsymbol (&Qfunction, "function");
  defsymbol (&Qerror_conditions, "error-conditions");
  defsymbol (&Qerror_message, "error-message");
  defsymbol (&Qsignal, "signal");
  defsymbol (&Qtop_level, "top-level");
  defsymbol (&Qdefault, "default");
  defsymbol (&Qignore, "ignore");

  defsymbol (&Qerror, "error");
  defsymbol (&Qquit, "quit");
  defsymbol (&Qwrong_type_argument, "wrong-type-argument");
  defsymbol (&Qargs_out_of_range, "args-out-of-range");
  defsymbol (&Qvoid_function, "void-function");
  defsymbol (&Qcyclic_function_indirection, "cyclic-function-indirection");
  defsymbol (&Qvoid_variable, "void-variable");
  defsymbol (&Qsetting_constant, "setting-constant");
  defsymbol (&Qinvalid_read_syntax, "invalid-read-syntax");

  defsymbol (&Qinvalid_function, "invalid-function");
  defsymbol (&Qwrong_number_of_arguments, "wrong-number-of-arguments");
  defsymbol (&Qno_catch, "no-catch");
  defsymbol (&Qend_of_file, "end-of-file");
  defsymbol (&Qarith_error, "arith-error");
  defsymbol (&Qrange_error, "range-error");
  defsymbol (&Qdomain_error, "domain-error");
  defsymbol (&Qsingularity_error, "singularity-error");
  defsymbol (&Qoverflow_error, "overflow-error");
  defsymbol (&Qunderflow_error, "underflow-error");
  defsymbol (&Qbeginning_of_buffer, "beginning-of-buffer");
  defsymbol (&Qend_of_buffer, "end-of-buffer");
  defsymbol (&Qbuffer_read_only, "buffer-read-only");

  defsymbol (&Qlistp, "listp");
  defsymbol (&Qconsp, "consp");
  defsymbol (&Qsubrp, "subrp");
  defsymbol (&Qsymbolp, "symbolp");
  defsymbol (&Qintegerp, "integerp");
  defsymbol (&Qnatnump, "natnump");
  defsymbol (&Qstringp, "stringp");
  defsymbol (&Qarrayp, "arrayp");
  defsymbol (&Qsequencep, "sequencep");
  defsymbol (&Qbufferp, "bufferp");
  defsymbol (&Qvectorp, "vectorp");
  defsymbol (&Qbytecodep, "bytecodep");
  defsymbol (&Qchar_or_string_p, "char-or-string-p");
  defsymbol (&Qmarkerp, "markerp");
  defsymbol (&Qinteger_or_marker_p, "integer-or-marker-p");
  defsymbol (&Qboundp, "boundp");
  defsymbol (&Qfboundp, "fboundp");

#ifdef LISP_FLOAT_TYPE
  defsymbol (&Qfloatp, "floatp");
#endif /* LISP_FLOAT_TYPE */
  defsymbol (&Qnumberp, "numberp");
  defsymbol (&Qnumber_or_marker_p, "number-or-marker-p");

  defsymbol (&Qcdr, "cdr");

  /* ERROR is used as a signaler for random errors for which nothing
     else is right */

  pure_put (Qerror, Qerror_conditions,
	    list1 (Qerror));
  pure_put (Qerror, Qerror_message,
	    build_string (DEFER_GETTEXT ("error")));

  pure_put (Qquit, Qerror_conditions,
	    list1 (Qquit));
  pure_put (Qquit, Qerror_message,
	    build_string (DEFER_GETTEXT ("Quit")));

  pure_put (Qwrong_type_argument, Qerror_conditions,
	    list2 (Qwrong_type_argument, Qerror));
  pure_put (Qwrong_type_argument, Qerror_message,
	    build_string (DEFER_GETTEXT ("Wrong type argument")));

  pure_put (Qargs_out_of_range, Qerror_conditions,
	    list2 (Qargs_out_of_range, Qerror));
  pure_put (Qargs_out_of_range, Qerror_message,
	    build_string (DEFER_GETTEXT ("Args out of range")));

  pure_put (Qvoid_function, Qerror_conditions,
	    list2 (Qvoid_function, Qerror));
  pure_put (Qvoid_function, Qerror_message,
	    build_string (DEFER_GETTEXT
			  ("Symbol's function definition is void")));

  pure_put (Qcyclic_function_indirection, Qerror_conditions,
	    list2 (Qcyclic_function_indirection, Qerror));
  pure_put (Qcyclic_function_indirection, Qerror_message,
	    build_string (DEFER_GETTEXT ("Symbol's chain of function indirections contains a loop")));

  pure_put (Qvoid_variable, Qerror_conditions,
	    list2 (Qvoid_variable, Qerror));
  pure_put (Qvoid_variable, Qerror_message,
	    build_string (DEFER_GETTEXT
			  ("Symbol's value as variable is void")));

  pure_put (Qsetting_constant, Qerror_conditions,
	    list2 (Qsetting_constant, Qerror));
  pure_put (Qsetting_constant, Qerror_message,
	    build_string (DEFER_GETTEXT ("Attempt to set a constant symbol")));

  pure_put (Qinvalid_read_syntax, Qerror_conditions,
	    list2 (Qinvalid_read_syntax, Qerror));
  pure_put (Qinvalid_read_syntax, Qerror_message,
	    build_string (DEFER_GETTEXT ("Invalid read syntax")));

  pure_put (Qinvalid_function, Qerror_conditions,
	    list2 (Qinvalid_function, Qerror));
  pure_put (Qinvalid_function, Qerror_message,
	    build_string (DEFER_GETTEXT ("Invalid function")));

  pure_put (Qwrong_number_of_arguments, Qerror_conditions,
	    list2 (Qwrong_number_of_arguments, Qerror));
  pure_put (Qwrong_number_of_arguments, Qerror_message,
	    build_string (DEFER_GETTEXT ("Wrong number of arguments")));

  pure_put (Qno_catch, Qerror_conditions,
	    list2 (Qno_catch, Qerror));
  pure_put (Qno_catch, Qerror_message,
	    build_string (DEFER_GETTEXT ("No catch for tag")));

  pure_put (Qend_of_file, Qerror_conditions,
	    list2 (Qend_of_file, Qerror));
  pure_put (Qend_of_file, Qerror_message,
	    build_string (DEFER_GETTEXT ("End of file during parsing")));

  pure_put (Qarith_error, Qerror_conditions,
	    list2 (Qarith_error, Qerror));
  pure_put (Qarith_error, Qerror_message,
	    build_string (DEFER_GETTEXT ("Arithmetic error")));

  pure_put (Qdomain_error, Qerror_conditions,
	    list3 (Qdomain_error, Qarith_error, Qerror));
  pure_put (Qdomain_error, Qerror_message,
	    build_string (DEFER_GETTEXT ("Arithmetic domain error")));

  pure_put (Qrange_error, Qerror_conditions,
	    list3 (Qrange_error, Qarith_error, Qerror));
  pure_put (Qrange_error, Qerror_message,
	    build_string (DEFER_GETTEXT ("Arithmetic range error")));

  pure_put (Qsingularity_error, Qerror_conditions,
	    list4 (Qsingularity_error, Qdomain_error, Qarith_error, Qerror));
  pure_put (Qsingularity_error, Qerror_message,
	    build_string (DEFER_GETTEXT ("Arithmetic singularity error")));

  pure_put (Qoverflow_error, Qerror_conditions,
	    list4 (Qoverflow_error, Qdomain_error, Qarith_error, Qerror));
  pure_put (Qoverflow_error, Qerror_message,
	    build_string (DEFER_GETTEXT ("Arithmetic overflow error")));

  pure_put (Qunderflow_error, Qerror_conditions,
	    list4 (Qunderflow_error, Qdomain_error, Qarith_error, Qerror));
  pure_put (Qunderflow_error, Qerror_message,
	    build_string (DEFER_GETTEXT ("Arithmetic underflow error")));

  pure_put (Qbeginning_of_buffer, Qerror_conditions,
	    list2 (Qbeginning_of_buffer, Qerror));
  pure_put (Qbeginning_of_buffer, Qerror_message,
	    build_string (DEFER_GETTEXT ("Beginning of buffer")));

  pure_put (Qend_of_buffer, Qerror_conditions,
	    list2 (Qend_of_buffer, Qerror));
  pure_put (Qend_of_buffer, Qerror_message,
	    build_string (DEFER_GETTEXT ("End of buffer")));

  pure_put (Qbuffer_read_only, Qerror_conditions,
	    list2 (Qbuffer_read_only, Qerror));
  pure_put (Qbuffer_read_only, Qerror_message,
	    build_string (DEFER_GETTEXT ("Buffer is read-only")));

  defsubr (&Seq);
  defsubr (&Snull);
  defsubr (&Slistp);
  defsubr (&Snlistp);
  defsubr (&Sconsp);
  defsubr (&Satom);
  defsubr (&Sintegerp);
  defsubr (&Sinteger_or_marker_p);
  defsubr (&Snumberp);
  defsubr (&Snumber_or_marker_p);
#ifdef LISP_FLOAT_TYPE
  defsubr (&Sfloatp);
#endif /* LISP_FLOAT_TYPE */
  defsubr (&Snatnump);
  defsubr (&Ssymbolp);
  defsubr (&Sstringp);
  defsubr (&Svectorp);
  defsubr (&Sarrayp);
  defsubr (&Ssequencep);
  defsubr (&Smarkerp);
  defsubr (&Ssubrp);
  defsubr (&Ssubr_min_args);
  defsubr (&Ssubr_max_args);
  defsubr (&Scompiled_function_p);
  defsubr (&Schar_or_string_p);
  defsubr (&Scar);
  defsubr (&Scdr);
  defsubr (&Scar_safe);
  defsubr (&Scdr_safe);
  defsubr (&Ssetcar);
  defsubr (&Ssetcdr);
  defsubr (&Sindirect_function);
  defsubr (&Saref);
  defsubr (&Saset);

  defsubr (&Scompiled_function_instructions);
  defsubr (&Scompiled_function_constants);
  defsubr (&Scompiled_function_stack_depth);
  defsubr (&Scompiled_function_arglist);
  defsubr (&Scompiled_function_interactive);
  defsubr (&Scompiled_function_domain);

  defsubr (&Snumber_to_string);
  defsubr (&Sstring_to_number);
  defsubr (&Seqlsign);
  defsubr (&Slss);
  defsubr (&Sgtr);
  defsubr (&Sleq);
  defsubr (&Sgeq);
  defsubr (&Sneq);
  defsubr (&Szerop);
  defsubr (&Splus);
  defsubr (&Sminus);
  defsubr (&Stimes);
  defsubr (&Squo);
  defsubr (&Srem);
  defsubr (&Smod);
  defsubr (&Smax);
  defsubr (&Smin);
  defsubr (&Slogand);
  defsubr (&Slogior);
  defsubr (&Slogxor);
  defsubr (&Slsh);
  defsubr (&Sash);
  defsubr (&Sadd1);
  defsubr (&Ssub1);
  defsubr (&Slognot);
}

static SIGTYPE
arith_error (signo)
     int signo;
{
#ifdef USG
  /* USG systems forget handlers when they are used;
     must reestablish each time */
  signal (signo, arith_error);
#endif /* USG */
#ifdef VMS
  /* VMS systems are like USG.  */
  signal (signo, arith_error);
#endif /* VMS */
#ifdef BSD4_1
  sigrelse (SIGFPE);
#else /* not BSD4_1 */
  sigsetmask (SIGEMPTYMASK);
#endif /* not BSD4_1 */

  signal_error (Qarith_error, Qnil);
}

void
init_data ()
{
  /* Don't do this if just dumping out.
     We don't want to call `signal' in this case
     so that we don't have trouble with dumping
     signal-delivering routines in an inconsistent state.  */
#ifndef CANNOT_DUMP
  if (!initialized)
    return;
#endif /* CANNOT_DUMP */
  signal (SIGFPE, arith_error);
#ifdef uts
  signal (SIGEMT, arith_error);
#endif /* uts */
}
