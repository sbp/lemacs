/* Random utility Lisp functions.
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


#include "config.h"

/* Note on some machines this defines `vector' as a typedef,
   so make sure we don't use that name in this file.  */
#undef vector
#define vector *****

#include "lisp.h"
#include "commands.h"

#include "buffer.h"
#include "extents.h"
#ifdef MULTI_SCREEN
#include "screen.h"
#endif

#include "events.h"
#include "bytecode.h"

#include <stdarg.h>
#include <stdio.h>              /* because sysdep.h needs it */
#include "sysdep.h"             /* for play_sound junk */
#include "systime.h"

int bell_volume;
Lisp_Object Qstring_lessp;

#define min(a, b) ((a) < (b) ? (a) : (b))
#define max(a, b) ((a) > (b) ? (a) : (b))

DEFUN ("identity", Fidentity, Sidentity, 1, 1, 0,
  "Return the argument unchanged.")
  (arg)
     Lisp_Object arg;
{
  return arg;
}

extern long random ();
extern void srandom ();

DEFUN ("random", Frandom, Srandom, 0, 1, 0,
  "Return a pseudo-random number.\n\
On most systems all integers representable in Lisp are equally likely.\n(\
A lisp integer is a few bits smaller than a C `long'; on most systems,\n\
this means 28 bits.)\n\
With argument N, return random number in interval [0,N).\n\
With argument t, set the random number seed from the current time and pid.")
  (limit)
     Lisp_Object limit;
{
  int val;

  if (EQ (limit, Qt))
    srandom (getpid () + time (0));
  val = random ();
  if (FIXNUMP (limit) && XINT (limit) != 0)
    {
      /* Try to take our random number from the higher bits of VAL,
	 not the lower, since (says Gentzel) the low bits of `random'
	 are less random than the higher ones.  */
      val &= 0xfffffff;		/* Ensure positive.  */
      val >>= 5;
      if (XINT (limit) < 10000)
	val >>= 6;
      val %= XINT (limit);
    }
  return make_number (val);
}

/* Random data-structure functions */

static int
length_with_bytecode_hack (Lisp_Object seq)
{
  if (!COMPILEDP (seq))
    return (XFASTINT (Flength (seq)));
  else
  {
#ifndef LRECORD_BYTECODE
    return (vector_length (XVECTOR (seq)));
#else
    struct Lisp_Bytecode *b = XBYTECODE (seq);
    int docp = b->flags.documentationp;
    int intp = b->flags.interactivep;
    int lesser = min (COMPILED_INTERACTIVE, COMPILED_DOC_STRING);

    if (!intp && !docp)
      return (lesser + 1);
    else if (((lesser == COMPILED_DOC_STRING) ? intp : docp))
      return (max (COMPILED_INTERACTIVE, COMPILED_DOC_STRING) + 1);
    else if (intp)
      return (COMPILED_INTERACTIVE + 1);
    else
      return (COMPILED_DOC_STRING + 1);
  }
#endif /* LRECORD_BYTECODE */
}



DEFUN ("length", Flength, Slength, 1, 1, 0,
  "Return the length of vector, list or string SEQUENCE.\n\
A byte-code function object is also allowed.")
  (obj)
     register Lisp_Object obj;
{
  register Lisp_Object tail;
  register int i;

 retry:
  if (STRINGP (obj))
    return (make_number (string_length (XSTRING (obj))));
  else if (VECTORP (obj))
    return (make_number (vector_length (XVECTOR (obj))));
  else if (CONSP (obj))
    {
      for (i = 0, tail = obj; !NILP(tail); i++)
	{
	  QUIT;
	  tail = Fcdr (tail);
	}

      return (make_number (i));
    }
  else if (NILP (obj))
    {
      return (Qzero);
    }
#if 0 /* I don't see any need to make this "work" */
  /* revolting "concat" callers use "length_with_bytecode_hack",
   *  so that bytecomp.el (which uses "(append bytcode nil)"
   *  "works". */
  else if (COMPILED (obj))
#ifndef LRECORD_BYTECODE
    return (make_number (vector_length (XVECTOR (obj))));
#else
#endif /* LRECORD_BYTECODE */
#endif /* 0 */
  else
    {
      obj = wrong_type_argument (Qsequencep, obj);
      goto retry;
    }
}

DEFUN ("string-equal", Fstring_equal, Sstring_equal, 2, 2, 0,
  "T if two strings have identical contents.\n\
Case is significant.\n\
Symbols are also allowed; their print names are used instead.")
  (s1, s2)
     Lisp_Object s1, s2;
{
  int len;

  if (SYMBOLP (s1))
    XSET (s1, Lisp_String, XSYMBOL (s1)->name);
  if (SYMBOLP (s2))
    XSET (s2, Lisp_String, XSYMBOL (s2)->name);
  CHECK_STRING (s1, 0);
  CHECK_STRING (s2, 1);

  len = string_length (XSTRING (s1));
  if (len != string_length (XSTRING (s2)) ||
      memcmp (XSTRING (s1)->data, XSTRING (s2)->data, len))
    return Qnil;
  return Qt;
}

DEFUN ("string-lessp", Fstring_lessp, Sstring_lessp, 2, 2, 0,
  "T if first arg string is less than second in lexicographic order.\n\
Case is significant.\n\
Symbols are also allowed; their print names are used instead.")
  (s1, s2)
     Lisp_Object s1, s2;
{
  register int i;
  register unsigned char *p1, *p2;
  register int end;
  int len2;

  if (SYMBOLP (s1))
    XSET (s1, Lisp_String, XSYMBOL (s1)->name);
  if (SYMBOLP (s2))
    XSET (s2, Lisp_String, XSYMBOL (s2)->name);
  CHECK_STRING (s1, 0);
  CHECK_STRING (s2, 1);

  p1 = XSTRING (s1)->data;
  p2 = XSTRING (s2)->data;
  end = string_length (XSTRING (s1));
  len2 = string_length (XSTRING (s2));
  if (end > len2)
    end = len2;

  for (i = 0; i < end; i++)
    {
      if (p1[i] != p2[i])
	return p1[i] < p2[i] ? Qt : Qnil;
    }
  return ((i < len2) ? Qt : Qnil);
}

static Lisp_Object concat (int nargs, Lisp_Object *args,
                           enum Lisp_Type target_type, 
                           int last_special);

Lisp_Object
concat2 (s1, s2)
     Lisp_Object s1, s2;
{
  Lisp_Object args[2];
  args[0] = s1;
  args[1] = s2;
  return concat (2, args, Lisp_String, 0);
}

DEFUN ("append", Fappend, Sappend, 0, MANY, 0,
  "Concatenate all the arguments and make the result a list.\n\
The result is a list whose elements are the elements of all the arguments.\n\
Each argument may be a list, vector or string.\n\
The last argument is not copied, just used as the tail of the new list.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return concat (nargs, args, Lisp_Cons, 1);
}

DEFUN ("concat", Fconcat, Sconcat, 0, MANY, 0,
  "Concatenate all the arguments and make the result a string.\n\
The result is a string whose elements are the elements of all the arguments.\n\
Each argument may be a string, a list of numbers, or a vector of numbers.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return concat (nargs, args, Lisp_String, 0);
}

DEFUN ("vconcat", Fvconcat, Svconcat, 0, MANY, 0,
  "Concatenate all the arguments and make the result a vector.\n\
The result is a vector whose elements are the elements of all the arguments.\n\
Each argument may be a list, vector or string.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return concat (nargs, args, Lisp_Vector, 0);
}

DEFUN ("copy-sequence", Fcopy_sequence, Scopy_sequence, 1, 1, 0,
  "Return a copy of a list, vector or string.\n\
The elements of a list or vector are not copied; they are shared\n\
with the original.")
  (arg)
     Lisp_Object arg;
{
  if (NILP (arg)) return arg;
  if (!CONSP (arg) && !VECTORP (arg) && !STRINGP (arg))
    arg = wrong_type_argument (Qsequencep, arg);
  /* We handle conses seperately because concat() is big and hairy and
     doesn't handle (copy-sequence '(a b . c)) and it's easier to redo this
     than to fix concat() without worrying about breaking other things.
   */
  if (CONSP (arg))
    {
      Lisp_Object rest = arg;
      Lisp_Object head, tail;
      tail = Qnil;
      while (CONSP (rest))
	{
	  Lisp_Object new = Fcons (XCONS (rest)->car, XCONS (rest)->cdr);
	  if (NILP (tail))
	    head = tail = new;
	  else
	    XCONS (tail)->cdr = new, tail = new;
	  rest = XCONS (rest)->cdr;
	  QUIT;
	}
      if (!NILP (tail))
	XCONS (tail)->cdr = rest;
      return head;
    }
  else
    return concat (1, &arg, ((CONSP (arg)) ? Lisp_Cons : XTYPE (arg)), 0);
}

static Lisp_Object
concat (nargs, args, target_type, last_special)
     int nargs;
     Lisp_Object *args;
     enum Lisp_Type target_type;
     int last_special;
{
  Lisp_Object val;
  register Lisp_Object tail = Qnil;
  int toindex;
  register int argnum;
  Lisp_Object last_tail;
  Lisp_Object prev;
  struct merge_replicas_struct *args_mr = 0;

  if (target_type == Lisp_String)
    {
      int size = nargs * sizeof (struct merge_replicas_struct);
      args_mr = (struct merge_replicas_struct *) alloca (size);
    }

  /* In append, the last arg isn't treated like the others */
  if (last_special && nargs > 0)
    {
      nargs--;
      last_tail = args[nargs];
    }
  else
    last_tail = Qnil;

  /* Check and coerce the arguments. */
  for (argnum = 0; argnum < nargs; argnum++)
    {
      Lisp_Object seq = args[argnum];
      if (CONSP (seq) || NILP (seq))
        ;
      else if (VECTORP (seq) || STRINGP (seq))
        ;
      else if (COMPILEDP (seq))
        /* Urk!  We allow this, for "compatibility"... */
        ;
      else if (FIXNUMP (seq))
        /* This is too revolting to think about. */
        args[argnum] = Fnumber_to_string (seq);
      else
        args[argnum] = wrong_type_argument (Qsequencep, seq);
      
      if (args_mr)
        {
          if (STRINGP (seq))
            args_mr[argnum].dup_list = XSTRING (seq)->dup_list;
          else
            args_mr[argnum].dup_list = Qnil;
        }
    }

  {
    int total_length;

    for (argnum = 0, total_length = 0; argnum < nargs; argnum++)
      {
        int thislen = length_with_bytecode_hack (args[argnum]);
        if (args_mr)
        {
          args_mr[argnum].entry_offset = total_length;
          args_mr[argnum].entry_length = thislen;
        }
        total_length += thislen;
      }

    switch (target_type)
      {
      case Lisp_Cons:
        if (total_length == 0)
          /* In append, if all but last arg are nil, return last arg */
          return (last_tail);
        val = Fmake_list (make_number (total_length), Qnil);
        break;
      case Lisp_Vector:
        val = make_vector (total_length, Qnil);
        break;
      case Lisp_String:
        val = Fmake_string (make_number (total_length), Qzero);
        XSTRING (val)->dup_list = merge_replicas (nargs, args_mr);
        break;
      default:
        abort ();
      }
  }


  if (CONSP (val))
    tail = val, toindex = -1;	/* -1 in toindex is flag we are
				    making a list */
  else
    toindex = 0;

  prev = Qnil;

  for (argnum = 0; argnum < nargs; argnum++)
    {
      int thisleni;
      int thisindex = 0;
      Lisp_Object seq = args[argnum];

      if (!CONSP (seq))
	{
	  thisleni = length_with_bytecode_hack (seq);
	}

      while (1)
	{
	  register Lisp_Object elt;

	  /* We've come to the end of this arg, so exit. */
	  if (NILP (seq))
	    break;

	  /* Fetch next element of `seq' arg into `elt' */
	  if (CONSP (seq))
            {
              elt = Fcar (seq);
              seq = Fcdr (seq);
            }
	  else
	    {
	      if (thisindex >= thisleni)
		break;

	      if (STRINGP (seq))
                elt = make_number (XSTRING (seq)->data[thisindex]);
	      else if (VECTORP (seq))
                elt = XVECTOR (seq)->contents[thisindex];
              else
		elt = Felt (seq, make_number (thisindex));
              thisindex++;
	    }

	  /* Store into result */
	  if (toindex < 0)
	    {
	      /* toindex negative means we are making a list */
	      XCONS (tail)->car = elt;
	      prev = tail;
	      tail = XCONS (tail)->cdr;
	    }
	  else if (VECTORP (val))
	    XVECTOR (val)->contents[toindex++] = elt;
	  else
	    {
	      while (!FIXNUMP (elt))
		elt = wrong_type_argument (Qintegerp, elt);

	      {
#ifdef MASSC_REGISTER_BUG
		/* Even removing all "register"s doesn't disable this bug!
		   Nothing simpler than this seems to work. */
		unsigned char *p = & XSTRING (val)->data[toindex++];
		*p = XINT (elt);
#else
		XSTRING (val)->data[toindex++] = XINT (elt);
#endif
	      }
	    }
	}
    }
  if (!NILP (prev))
    XCONS (prev)->cdr = last_tail;

  return val;  
}

DEFUN ("copy-alist", Fcopy_alist, Scopy_alist, 1, 1, 0,
  "Return a copy of ALIST.\n\
This is an alist which represents the same mapping from objects to objects,\n\
but does not share the alist structure with ALIST.\n\
The objects mapped (cars and cdrs of elements of the alist)\n\
are shared, however.\n\
Elements of ALIST that are not conses are also shared.")
  (alist)
     Lisp_Object alist;
{
  register Lisp_Object tem;

  CHECK_LIST (alist, 0);
  if (NILP (alist))
    return alist;
  alist = concat (1, &alist, Lisp_Cons, 0);
  for (tem = alist; CONSP (tem); tem = XCONS (tem)->cdr)
    {
      register Lisp_Object car;
      car = XCONS (tem)->car;

      if (CONSP (car))
	XCONS (tem)->car = Fcons (XCONS (car)->car, XCONS (car)->cdr);
    }
  return alist;
}

DEFUN ("copy-tree", Fcopy_tree, Scopy_tree, 1, 2, 0,
  "Return a copy of a list and substructures.\n\
The argument is copied, and any lists contained within it are copied\n\
recursively.  Circularities and shared substructures are not preserved.\n\
Second arg VECP causes vectors to be copied, too.  Strings are not copied.")
   (arg, vecp)
     Lisp_Object arg, vecp;
{
  if (CONSP (arg))
    {
      Lisp_Object rest;
      rest = arg = Fcopy_sequence (arg);
      while (CONSP (rest))
	{
	  Lisp_Object elt = XCONS (rest)->car;
	  QUIT;
	  if (CONSP (elt) || VECTORP (elt))
	    XCONS (rest)->car = Fcopy_tree (elt, vecp);
	  if (VECTORP (XCONS (rest)->cdr)) /* hack for (a b . [c d]) */
	    XCONS (rest)->cdr = Fcopy_tree (XCONS (rest)->cdr, vecp);
	  rest = XCONS (rest)->cdr;
	}
    }
  else if (VECTORP (arg) && ! NILP (vecp))
    {
      int i = vector_length (XVECTOR (arg));
      int j;
      arg = Fcopy_sequence (arg);
      for (j = 0; j < i; j++)
	{
	  Lisp_Object elt = XVECTOR (arg)->contents [j];
	  QUIT;
	  if (CONSP (elt) || VECTORP (elt))
	    XVECTOR (arg)->contents [j] = Fcopy_tree (elt, vecp);
	}
    }
  return arg;
}

DEFUN ("substring", Fsubstring, Ssubstring, 2, 3, 0,
  "Return a substring of STRING, starting at index FROM and ending before TO.\n\
TO may be nil or omitted; then the substring runs to the end of STRING.\n\
If FROM or TO is negative, it counts from the end.")
  (string, from, to)
     Lisp_Object string;
     register Lisp_Object from, to;
{
  int len;

  CHECK_STRING (string, 0);
  CHECK_FIXNUM (from, 1);
  if (NILP (to))
    to = Flength (string);
  else
    CHECK_FIXNUM (to, 2);

  len = string_length (XSTRING (string));
  if (XINT (from) < 0)
    XSETINT (from, XINT (from) + len);
  if (XINT (to) < 0)
    XSETINT (to, XINT (to) + len);
  if (!(0 <= XINT (from) && XINT (from) <= XINT (to)
        && XINT (to) <= len))
    args_out_of_range_3 (string, from, to);

  return make_string ((char *) XSTRING (string)->data + XINT (from),
		      XINT (to) - XINT (from));
}

DEFUN ("nthcdr", Fnthcdr, Snthcdr, 2, 2, 0,
  "Take cdr N times on LIST, returns the result.")
  (n, list)
     Lisp_Object n;
     register Lisp_Object list;
{
  register int i, num;
  CHECK_FIXNUM (n, 0);
  num = XINT (n);
  for (i = 0; i < num && !NILP (list); i++)
    {
      QUIT;
      list = Fcdr (list);
    }
  return list;
}

DEFUN ("nth", Fnth, Snth, 2, 2, 0,
  "Return the Nth element of LIST.\n\
N counts from zero.  If LIST is not that long, nil is returned.")
  (n, list)
     Lisp_Object n, list;
{
  return Fcar (Fnthcdr (n, list));
}

DEFUN ("elt", Felt, Selt, 2, 2, 0,
  "Return element of SEQUENCE at index N.")
  (seq, n)
     register Lisp_Object seq, n;
{
 retry:
  CHECK_FIXNUM (n, 0);
  if (CONSP (seq) || NILP (seq))
    {
      Lisp_Object tem = Fnthcdr (n, seq);
      if (CONSP (tem))
	return (XCONS (tem)->car);
      else
	return Qnil;
    }
  else if (STRINGP (seq)
           || VECTORP (seq))
    return (Faref (seq, n));
  else if (COMPILEDP (seq))
    {
      int idx = XINT (n);
      if (idx < 0)
        {
        lose:
          args_out_of_range (seq, n);
        }
#ifndef LRECORD_BYTECODE
      if (idx >= vector_length (XVECTOR (seq))) goto lose;
      return (XVECTOR (seq)->contents[idx]);
#else /* LRECORD_BYTECODE */
      {
        struct Lisp_Bytecode *b = XBYTECODE (seq);
        int docp = b->flags.documentationp;
        int intp = b->flags.interactivep;
        switch (idx)
          {
          case COMPILED_ARGLIST:
            return (b->arglist);
          case COMPILED_BYTECODE:
            return (b->bytecodes);
          case COMPILED_CONSTANTS:
            return (b->constants);
          case COMPILED_STACK_DEPTH:
            return (make_number (b->maxdepth));
          case COMPILED_DOC_STRING:
            {
              if (!docp) 
                return Qnil;
              if (!intp)
                return (b->doc_and_interactive);
              else
                return (XCONS (b->doc_and_interactive)->car);
            }
          case COMPILED_INTERACTIVE:
            {
              if (!intp)
		/* if we return nil, can't tell interactive with no args
		   from noninteractive. */
                goto lose;
              if (!docp)
                return (b->doc_and_interactive);
              else
                return (XCONS (b->doc_and_interactive)->cdr);
            }
          default:
            goto lose;
          }
      }
#endif /* LRECORD_BYTECODE */
    }
  else
    {
      seq = wrong_type_argument (Qsequencep, seq);
      goto retry;
    }
}

DEFUN ("member", Fmember, Smember, 2, 2, 0,
  "Return non-nil if ELT is an element of LIST.  Comparison done with EQUAL.\n\
The value is actually the tail of LIST whose car is ELT.")
  (elt, list)
     register Lisp_Object elt;
     Lisp_Object list;
{
  register Lisp_Object tail;
  for (tail = list; !NILP (tail); tail = Fcdr (tail))
    {
      register Lisp_Object tem;
      tem = Fcar (tail);
      if (! NILP (Fequal (elt, tem)))
	return tail;
      QUIT;
    }
  return Qnil;
}

DEFUN ("memq", Fmemq, Smemq, 2, 2, 0,
  "Return non-nil if ELT is an element of LIST.  Comparison done with EQ.\n\
The value is actually the tail of LIST whose car is ELT.")
  (elt, list)
     register Lisp_Object elt;
     Lisp_Object list;
{
  register Lisp_Object tail;
  for (tail = list; !NILP (tail); tail = Fcdr (tail))
    {
      register Lisp_Object tem;
      tem = Fcar (tail);
      if (EQ (elt, tem)) return tail;
      QUIT;
    }
  return Qnil;
}

Lisp_Object
memq_no_quit (elt, list)
     register Lisp_Object elt;
     Lisp_Object list;
{
  register Lisp_Object tail;
  for (tail = list; CONSP (tail); tail = XCONS (tail)->cdr)
    {
      register Lisp_Object tem;
      tem = XCONS (tail)->car;
      if (EQ (elt, tem)) return tail;
    }
  return Qnil;
}

DEFUN ("assq", Fassq, Sassq, 2, 2, 0,
  "Return non-nil if ELT is `eq' to the car of an element of LIST.\n\
The value is actually the element of LIST whose car is ELT.\n\
Elements of LIST that are not conses are ignored.")
  (key, list)
     register Lisp_Object key;
     Lisp_Object list;
{
  register Lisp_Object tail;
  for (tail = list; !NILP (tail); tail = Fcdr (tail))
    {
      register Lisp_Object elt, tem;
      elt = Fcar (tail);
      if (!CONSP (elt)) continue;
      tem = Fcar (elt);
      if (EQ (key, tem)) return elt;
      QUIT;
    }
  return Qnil;
}

/* Like Fassq but never report an error and do not allow quits.
   Use only on lists known never to be circular.  */

Lisp_Object
assq_no_quit (key, list)
     register Lisp_Object key;
     Lisp_Object list;
{
  register Lisp_Object tail;
  for (tail = list; CONSP (tail); tail = Fcdr (tail))
    {
      register Lisp_Object elt, tem;
      elt = Fcar (tail);
      if (!CONSP (elt)) continue;
      tem = Fcar (elt);
      if (EQ (key, tem)) return elt;
    }
  return Qnil;
}

DEFUN ("assoc", Fassoc, Sassoc, 2, 2, 0,
  "Return non-nil if ELT is `equal' to the car of an element of LIST.\n\
The value is actually the element of LIST whose car is ELT.")
  (key, list)
     register Lisp_Object key;
     Lisp_Object list;
{
  register Lisp_Object tail;
  for (tail = list; !NILP (tail); tail = Fcdr (tail))
    {
      register Lisp_Object elt, tem;
      elt = Fcar (tail);
      if (!CONSP (elt)) continue;
      tem = Fequal (Fcar (elt), key);
      if (!NILP (tem)) return elt;
      QUIT;
    }
  return Qnil;
}

Lisp_Object
assoc_no_quit (key, list)
     register Lisp_Object key;
     Lisp_Object list;
{
  int speccount = specpdl_depth ();
  specbind (Qinhibit_quit, Qt);
  return (unbind_to (speccount, Fassoc (key, list)));
}

DEFUN ("rassq", Frassq, Srassq, 2, 2, 0,
  "Return non-nil if ELT is `eq' to the cdr of an element of LIST.\n\
The value is actually the element of LIST whose cdr is ELT.")
  (key, list)
     register Lisp_Object key;
     Lisp_Object list;
{
  register Lisp_Object tail;
  for (tail = list; !NILP (tail); tail = Fcdr (tail))
    {
      register Lisp_Object elt, tem;
      elt = Fcar (tail);
      if (!CONSP (elt)) continue;
      tem = Fcdr (elt);
      if (EQ (key, tem)) return elt;
      QUIT;
    }
  return Qnil;
}

Lisp_Object
rassq_no_quit (key, list)
     register Lisp_Object key;
     Lisp_Object list;
{
  register Lisp_Object tail;
  for (tail = list; CONSP (tail); tail = XCONS (tail)->cdr)
    {
      register Lisp_Object elt, tem;
      elt = XCONS (tail)->car;
      if (!CONSP (elt)) continue;
      tem = XCONS (elt)->cdr;
      if (EQ (key, tem)) return elt;
    }
  return Qnil;
}


DEFUN ("delq", Fdelq, Sdelq, 2, 2, 0,
  "Delete by side effect any occurrences of ELT as a member of LIST.\n\
The modified LIST is returned.  Comparison is done with `eq'.\n\
If the first member of LIST is ELT, there is no way to remove it by side effect;\n\
therefore, write `(setq foo (delq element foo))'\n\
to be sure of changing the value of `foo'.")
  (elt, list)
     register Lisp_Object elt;
     Lisp_Object list;
{
  register Lisp_Object tail, prev;
  register Lisp_Object tem;

  tail = list;
  prev = Qnil;
  while (!NILP (tail))
    {
      tem = Fcar (tail);
      if (EQ (elt, tem))
	{
	  if (NILP (prev))
	    list = Fcdr (tail);
	  else
	    Fsetcdr (prev, Fcdr (tail));
	}
      else
	prev = tail;
      tail = Fcdr (tail);
      QUIT;
    }
  return list;
}


Lisp_Object
delq_no_quit (elt, list)		/* no quit, no errors; be careful */
     register Lisp_Object elt, list;
{
  register Lisp_Object tail, prev;
  register Lisp_Object tem;

  tail = list;
  prev = Qnil;
  while (CONSP (tail))
    {
      tem = XCONS (tail)->car;
      if (EQ (elt, tem))
	{
	  if (NILP (prev))
	    list = XCONS (tail)->cdr;
	  else
	    XCONS (prev)->cdr = XCONS (tail)->cdr;
	}
      else
	prev = tail;
      tail = XCONS (tail)->cdr;
    }
  return list;
}

DEFUN ("delete", Fdelete, Sdelete, 2, 2, 0,
  "Delete by side effect any occurrences of ELT as a member of LIST.\n\
The modified LIST is returned.  Comparison is done with `equal'.\n\
If the first member of LIST is ELT, there is no way to remove it by\n\
side effect; therefore, write `(setq foo (delete element foo))'\n\
to be sure of changing the value of `foo'.")
  (elt, list)
     register Lisp_Object elt;
     Lisp_Object list;
{
  Lisp_Object tail, prev;

  tail = list;
  prev = Qnil;
  while (!NILP (tail))
    {
      if (! NILP (Fequal (elt, Fcar (tail))))
	{
	  if (NILP (prev))
	    list = Fcdr (tail);
	  else
	    Fsetcdr (prev, Fcdr (tail));
	}
      else
	prev = tail;
      tail = Fcdr (tail);
      QUIT;
    }
  return list;
}

DEFUN ("nreverse", Fnreverse, Snreverse, 1, 1, 0,
  "Reverse LIST by modifying cdr pointers.\n\
Returns the beginning of the reversed list.")
  (list)
     Lisp_Object list;
{
  register Lisp_Object prev, tail, next;

  prev = Qnil;
  tail = list;
  while (!NILP (tail))
    {
      QUIT;
      next = Fcdr (tail);
      Fsetcdr (tail, prev);
      prev = tail;
      tail = next;
    }
  return prev;
}

DEFUN ("reverse", Freverse, Sreverse, 1, 1, 0,
  "Reverse LIST, copying.  Returns the beginning of the reversed list.\n\
See also the function `nreverse', which is used more often.")
  (list)
     Lisp_Object list;
{
  Lisp_Object length;
  register Lisp_Object *vec;
  register Lisp_Object tail;
  register int i;

  length = Flength (list);
  vec = (Lisp_Object *) alloca (XINT (length) * sizeof (Lisp_Object));
  for (i = XINT (length) - 1, tail = list; i >= 0; i--, tail = Fcdr (tail))
    vec[i] = Fcar (tail);

  return Flist (XINT (length), vec);
}

static Lisp_Object list_merge (Lisp_Object org_l1, Lisp_Object org_l2, 
                               Lisp_Object lisp_arg, 
                               int (*pred_fn) (Lisp_Object, Lisp_Object,
                                               Lisp_Object lisp_arg));

Lisp_Object
list_sort (Lisp_Object list,
           Lisp_Object lisp_arg, 
           int (*pred_fn) (Lisp_Object, Lisp_Object,
                           Lisp_Object lisp_arg))
{
  Lisp_Object front, back;
  Lisp_Object len, tem;
  struct gcpro gcpro1, gcpro2, gcpro3;
  int length;

  front = list;
  len = Flength (list);
  length = XINT (len);
  if (length < 2)
    return list;

  XSETINT (len, (length / 2) - 1);
  tem = Fnthcdr (len, list);
  back = Fcdr (tem);
  Fsetcdr (tem, Qnil);

  GCPRO3 (front, back, lisp_arg);
  front = list_sort (front, lisp_arg, pred_fn);
  back = list_sort (back, lisp_arg, pred_fn);
  UNGCPRO;
  return list_merge (front, back, lisp_arg, pred_fn);
}


/* Emacs' GC doesn't actually relocate pointers, so this probably
   isn't strictly necessary */
static Lisp_Object
restore_gc_inhibit (Lisp_Object val)
{
  gc_currently_forbidden = XINT (val);
  return val;
}

void
run_hook_with_args (Lisp_Object hook_var, int nargs, ...)
{
  Lisp_Object rest;
  int i;
  va_list vargs;
  va_start (vargs, hook_var);

  if (NILP (Fboundp (hook_var)))
    rest = Qnil;
  else
    rest = Fsymbol_value (hook_var);
  if (NILP (rest))
    {
      for (i = 0; i < nargs; i++)
        (void) va_arg (vargs, Lisp_Object);
      va_end (vargs);
      return;
    }
  else
    {
      struct gcpro gcpro1, gcpro2;
      Lisp_Object *funcall_args =
	(Lisp_Object *) alloca ((1 + nargs) * sizeof (Lisp_Object));

      for (i = 0; i < nargs; i++)
        funcall_args[i + 1] = va_arg (vargs, Lisp_Object);
      va_end (vargs);

      funcall_args[0] = rest;
      GCPRO2 (rest, *funcall_args);
      gcpro2.nvars = nargs + 1;

      if (SYMBOLP (rest) || EQ (Qlambda, Fcar_safe (rest)))
        Ffuncall (nargs + 1, funcall_args);
      else
        {
          while (!NILP (rest))
            {
              funcall_args[0] = Fcar (rest);
              Ffuncall (nargs + 1, funcall_args);
              rest = Fcdr (rest);
            }
        }
      UNGCPRO;
    }
}



static int
merge_pred_function (Lisp_Object obj1, Lisp_Object obj2, 
                     Lisp_Object pred)
{
  Lisp_Object tmp;

  /* prevents the GC from happening in call2 */
  int speccount = specpdl_depth ();
  record_unwind_protect (restore_gc_inhibit,
                         make_number (gc_currently_forbidden));
  gc_currently_forbidden = 1;
  tmp = call2 (pred, obj1, obj2);
  unbind_to (speccount, Qnil);

  if (NILP (tmp)) 
    return -1;
  else
    return 1;
}

DEFUN ("sort", Fsort, Ssort, 2, 2, 0,
  "Sort LIST, stably, comparing elements using PREDICATE.\n\
Returns the sorted list.  LIST is modified by side effects.\n\
PREDICATE is called with two elements of LIST, and should return T\n\
if the first element is \"less\" than the second.")
  (list, pred)
     Lisp_Object list, pred;
{
  return list_sort (list, pred, merge_pred_function);
}

Lisp_Object
merge (Lisp_Object org_l1, Lisp_Object org_l2, 
       Lisp_Object pred)
{
  return list_merge (org_l1, org_l2, pred, merge_pred_function);
}


static Lisp_Object
list_merge (Lisp_Object org_l1, Lisp_Object org_l2, 
            Lisp_Object lisp_arg, 
            int (*pred_fn) (Lisp_Object, Lisp_Object, Lisp_Object lisp_arg))
{
  Lisp_Object value;
  Lisp_Object tail;
  Lisp_Object tem;
  Lisp_Object l1, l2;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

  l1 = org_l1;
  l2 = org_l2;
  tail = Qnil;
  value = Qnil;

  /* It is sufficient to protect org_l1 and org_l2.
     When l1 and l2 are updated, we copy the new values
     back into the org_ vars.  */
  
  GCPRO4 (org_l1, org_l2, lisp_arg, value);

  while (1)
    {
      if (NILP (l1))
	{
	  UNGCPRO;
	  if (NILP (tail))
	    return l2;
	  Fsetcdr (tail, l2);
	  return value;
	}
      if (NILP (l2))
	{
	  UNGCPRO;
	  if (NILP (tail))
	    return l1;
	  Fsetcdr (tail, l1);
	  return value;
	}

      if (((*pred_fn) (Fcar (l2), Fcar (l1), lisp_arg)) < 0)
	{
	  tem = l1;
	  l1 = Fcdr (l1);
	  org_l1 = l1;
	}
      else
	{
	  tem = l2;
	  l2 = Fcdr (l2);
	  org_l2 = l2;
	}
      if (NILP (tail))
	value = tem;
      else
	Fsetcdr (tail, tem);
      tail = tem;
    }
}


DEFUN ("get", Fget, Sget, 2, 3, 0,
  "Return the value of SYMBOL's PROPNAME property.\n\
This is the last VALUE stored with `(put SYMBOL PROPNAME VALUE)'.\n\
If there is no such property, return optional third arg DEFAULT\n\
  (which defaults to `nil'.)")
  (sym, prop, defalt)           /* Cant spel in C */
     Lisp_Object sym, defalt;
     register Lisp_Object prop;
{
  register Lisp_Object tail;
  for (tail = Fsymbol_plist (sym); !NILP (tail); tail = Fcdr (Fcdr (tail)))
    if (EQ (prop, Fcar (tail)))
      return Fcar (Fcdr (tail));

  return defalt;
}

DEFUN ("put", Fput, Sput, 3, 3, 0,
  "Store SYMBOL's PROPNAME property with value VALUE.\n\
It can be retrieved with `(get SYMBOL PROPNAME)'.")
  (sym, prop, val)
     Lisp_Object sym;
     register Lisp_Object prop;
     Lisp_Object val;
{
  register Lisp_Object tail;
  Lisp_Object head = Fsymbol_plist (sym);

  for (tail = head; !NILP (tail); tail = Fcdr (Fcdr (tail)))
    if (EQ (prop, Fcar (tail)))
      return Fsetcar (Fcdr (tail), val);

  Fsetplist (sym, Fcons (prop, Fcons (val, head)));
  return val;
}

void
pure_put (Lisp_Object sym, Lisp_Object prop, Lisp_Object val)
{
  Fput (sym, prop, Fpurecopy (val));
}

DEFUN ("remprop", Fremprop, Sremprop, 2, 2, 0,
  "Remove from SYMBOL's plist the property PROP and its value.")
  (symbol, property)
     Lisp_Object symbol, property;
{
  register Lisp_Object tail;
  register Lisp_Object obj;
  register Lisp_Object prev;
  register unsigned char changed = 0;

  CHECK_SYMBOL (symbol, 0);
  tail = XSYMBOL (symbol)->plist;

  obj = Fcar (tail);
  while (!NILP (obj) && EQ (property, obj))
    {
      changed = 1;
      tail = Fcdr (Fcdr (tail));
      obj = Fcar (tail);
    }
  XSYMBOL (symbol)->plist = tail;
  
  prev = tail;
  tail = Fcdr (Fcdr (tail));
  while (!NILP (tail))
    {
      obj = Fcar (tail);
      if (EQ (property, obj))
	{
	  changed = 1;
          Fsetcdr (Fcdr (prev), (Fcdr (Fcdr (tail))));
	}
      prev = tail;
      tail = Fcdr (Fcdr (tail));
    }

  return changed ? Qt : Qnil;
}


/* Same as the Common Lisp function GETF.  Never errors,
   returns nil when there is no match. */
Lisp_Object
getf (plist, indicator)
     Lisp_Object plist, indicator;
{
  Lisp_Object tail = plist;

  while (CONSP (tail))
    {
      struct Lisp_Cons *untagged_tail = XCONS (tail);
      
      tail = untagged_tail->cdr;
      if (EQ (untagged_tail->car, indicator))
	{
	  if (CONSP (tail))
	    return XCONS (tail)->car;
	  else
	    return Qnil;
	}
      if (CONSP (tail))
	tail = XCONS (tail)->cdr;
      else
	return Qnil;
    }
  return Qnil;
}

int
internal_equal (Lisp_Object o1, Lisp_Object o2, int depth)
{
  if (depth > 200)
    error ("Stack overflow in equal");
 do_cdr:
  QUIT;
  if (EQ (o1, o2))
    return (1);
#ifdef LISP_FLOAT_TYPE
  /* #### once LRECORD_FLOAT is the only choice, this should go away. */
  else if (NUMBERP (o1) && NUMBERP (o2))
    {
      if (FIXNUMP (o1) && FIXNUMP (o2))
	return (0);
      else if (extract_float (o1) == extract_float (o2))
	return (1);
      else
	return (0);
    }
#endif
  else if (XTYPE (o1) != XTYPE (o2)) 
    return (0);
  else if (CONSP (o1))
    {
      if (!internal_equal (Fcar (o1), Fcar (o2), depth + 1))
        return (0);
      o1 = Fcdr (o1);
      o2 = Fcdr (o2);
      goto do_cdr;
    }

  else if (VECTORP (o1))
    {
      register int index;
      int len = vector_length (XVECTOR (o1));
      if (len != vector_length (XVECTOR (o2)))
	return (0);
      for (index = 0; index < len; index++)
	{
	  Lisp_Object v1, v2;
	  v1 = XVECTOR (o1)->contents [index];
	  v2 = XVECTOR (o2)->contents [index];
	  if (!internal_equal (v1, v2, depth + 1))
            return (0);
	}
      return (1);
    }
  else if (STRINGP (o1))
    {
      int len = string_length (XSTRING (o1));
      if (len != string_length (XSTRING (o2)))
	return (0);
      if (memcmp (XSTRING (o1)->data, XSTRING (o2)->data, len))
	return (0);
      return (1);
    }
  else if (LRECORDP (o1))
    {
      const struct lrecord_implementation
	*imp1 = XRECORD_LHEADER (o1)->implementation,
	*imp2 = XRECORD_LHEADER (o2)->implementation;
      if (imp1 != imp2)
	return (0);
      else if (imp1->equal == 0)
	return (0);
      else
	return ((imp1->equal) (o1, o2, depth));
    }

#ifndef LRECORD_BYTECODE
  else if (COMPILEDP (o1))
    {
      int index;
      int len = vector_length (XVECTOR (o1));
      if (len != vector_length (XVECTOR (o2)))
	return (0);
      for (index = 0; index < len; index++)
	{
	  Lisp_Object v, v1, v2;
	  v1 = XVECTOR (o1)->contents [index];
	  v2 = XVECTOR (o2)->contents [index];
	  if (!internal_equal (v1, v2, depth + 1))
            return (0);
	}
      return (1);
    }
#endif /* !LRECORD_BYTECODE */

  return (0);
}

DEFUN ("equal", Fequal, Sequal, 2, 2, 0,
  "T if two Lisp objects have similar structure and contents.\n\
They must have the same data type.\n\
Conses are compared by comparing the cars and the cdrs.\n\
Vectors and strings are compared element by element.\n\
Numbers are compared by value.  Symbols must match exactly.")
  (o1, o2)
     register Lisp_Object o1, o2;
{
  return ((internal_equal (o1, o2, 0)) ? Qt : Qnil);
}



DEFUN ("fillarray", Ffillarray, Sfillarray, 2, 2, 0,
  "Store each element of ARRAY with ITEM.  ARRAY is a vector or string.")
  (array, item)
     Lisp_Object array, item;
{
 retry:
  if (VECTORP (array))
    {
      register Lisp_Object *p;
      int size;
      int index;
      CHECK_IMPURE (array);
      size = vector_length (XVECTOR (array));
      p = XVECTOR (array)->contents;
      for (index = 0; index < size; index++)
	p[index] = item;
    }
  else if (STRINGP (array))
    {
      register unsigned char *p;
      int size;
      int index;
      int charval;
      CHECK_FIXNUM (item, 1);
      CHECK_IMPURE (array);
      charval = XINT (item);
      size = string_length (XSTRING (array));
      p = XSTRING (array)->data;
      for (index = 0; index < size; index++)
	p[index] = charval;
    }
  else
    {
      array = wrong_type_argument (Qarrayp, array);
      goto retry;
    }
  return array;
}

Lisp_Object
nconc2 (s1, s2)
     Lisp_Object s1, s2;
{
  Lisp_Object args[2];
  args[0] = s1;
  args[1] = s2;
  return Fnconc (2, args);
}

DEFUN ("nconc", Fnconc, Snconc, 0, MANY, 0,
  "Concatenate any number of lists by altering them.\n\
Only the last argument is not altered, and need not be a list.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  register int argnum;
  register Lisp_Object tail, tem, val;

  val = Qnil;

  for (argnum = 0; argnum < nargs; argnum++)
    {
      tem = args[argnum];
      if (NILP (tem)) continue;

      if (NILP (val))
	val = tem;

      if (argnum + 1 == nargs) break;

      if (!CONSP (tem))
	tem = wrong_type_argument (Qlistp, tem);

      while (CONSP (tem))
	{
	  tail = tem;
	  tem = Fcdr (tail);
	  QUIT;
	}

      tem = args[argnum + 1];
      Fsetcdr (tail, tem);
      if (NILP (tem))
	args[argnum + 1] = tail;
    }

  return val;
}

/* This is the guts of all mapping functions.
 Apply fn to each element of seq, one by one,
 storing the results into elements of vals, a C vector of Lisp_Objects.
 leni is the length of vals, which should also be the length of seq. */

static void
mapcar1 (leni, vals, fn, seq)
     int leni;
     Lisp_Object *vals;
     Lisp_Object fn, seq;
{
  register Lisp_Object tail;
  Lisp_Object dummy;
  register int i;
  struct gcpro gcpro1, gcpro2, gcpro3;

  /* Don't let vals contain any garbage when GC happens.  */
  for (i = 0; i < leni; i++)
    vals[i] = Qnil;

  GCPRO3 (dummy, fn, seq);
  gcpro1.var = vals;
  gcpro1.nvars = leni;
  /* We need not explicitly protect `tail' because it is used only on lists, and
    1) lists are not relocated and 2) the list is marked via `seq' so will not be freed */

  if (VECTORP (seq))
    {
      for (i = 0; i < leni; i++)
	{
	  dummy = XVECTOR (seq)->contents[i];
	  vals[i] = call1 (fn, dummy);
	}
    }
  else if (STRINGP (seq))
    {
      for (i = 0; i < leni; i++)
	{
	  vals[i] = call1 (fn, make_number (XSTRING (seq)->data[i]));
	}
    }
  else   /* Must be a list, since Flength did not get an error */
    {
      tail = seq;
      for (i = 0; i < leni; i++)
	{
	  vals[i] = call1 (fn, Fcar (tail));
	  tail = Fcdr (tail);
	}
    }

  UNGCPRO;
}

DEFUN ("mapconcat", Fmapconcat, Smapconcat, 3, 3, 0,
  "Apply FN to each element of SEQ, and concat the results as strings.\n\
In between each pair of results, stick in SEP.\n\
Thus, \" \" as SEP results in spaces between the values return by FN.")
  (fn, seq, sep)
     Lisp_Object fn, seq, sep;
{
  Lisp_Object len;
  register int leni;
  int nargs;
  register Lisp_Object *args;
  register int i;
  struct gcpro gcpro1;

  len = Flength (seq);
  leni = XINT (len);
  nargs = leni + leni - 1;
  if (nargs < 0) return build_string ("");

  args = (Lisp_Object *) alloca (nargs * sizeof (Lisp_Object));

  GCPRO1 (sep);
  mapcar1 (leni, args, fn, seq);
  UNGCPRO;

  for (i = leni - 1; i >= 0; i--)
    args[i + i] = args[i];
      
  for (i = 1; i < nargs; i += 2)
    args[i] = sep;

  return Fconcat (nargs, args);
}

DEFUN ("mapcar", Fmapcar, Smapcar, 2, 2, 0,
  "Apply FUNCTION to each element of SEQUENCE, and make a list of the results.\n\
The result is a list just as long as SEQUENCE.\n\
SEQUENCE may be a list, a vector or a string.")
  (fn, seq)
     Lisp_Object fn, seq;
{
  register Lisp_Object len;
  register int leni;
  register Lisp_Object *args;

  len = Flength (seq);
  leni = XINT (len);
  args = (Lisp_Object *) alloca (leni * sizeof (Lisp_Object));

  mapcar1 (leni, args, fn, seq);

  return Flist (leni, args);
}

DEFUN ("load-average", Fload_average, Sload_average, 0, 0, 0,
  "Return list of 1 minute, 5 minute and 15 minute load averages.\n\
Each of the three load averages is multiplied by 100,\n\
then converted to integer.\n\
\n\
If the 5-minute or 15-minute load averages are not available, return a\n\
shortened list, containing only those averages which are available.\n\
\n\
On most systems, this won't work unless the emacs executable is installed\n\
as setgid kmem (assuming that /dev/kmem is in the group kmem.)")
  ()
{
  double load_ave[10]; /* hey, just in case */
  int loads = getloadavg (load_ave, 3);
  Lisp_Object ret;

  if (loads == -2)
    error ("load-average not implemented for this operating system.");
  else if (loads < 0)
    error ("could not get load-average; check permissions.");

  ret = Qnil;
  while (loads > 0)
    ret = Fcons (make_number ((int) (load_ave[--loads] * 100.0)), ret);

  return ret;
}

Lisp_Object Vfeatures;

DEFUN ("featurep", Ffeaturep, Sfeaturep, 1, 1, 0,
  "Returns t if FEATURE is present in this Emacs.\n\
Use this to conditionalize execution of lisp code based on the presence or\n\
absence of emacs or environment extensions.\n\
Use `provide' to declare that a feature is available.\n\
This function looks at the value of the variable `features'.")
     (feature)
     Lisp_Object feature;
{
  register Lisp_Object tem;
  CHECK_SYMBOL (feature, 0);
  tem = Fmemq (feature, Vfeatures);
  return (NILP (tem)) ? Qnil : Qt;
}

DEFUN ("provide", Fprovide, Sprovide, 1, 1, 0,
  "Announce that FEATURE is a feature of the current Emacs.")
     (feature)
     Lisp_Object feature;
{
  register Lisp_Object tem;
  CHECK_SYMBOL (feature, 0);
  if (!NILP (Vautoload_queue))
    Vautoload_queue = Fcons (Fcons (Vfeatures, Qnil), Vautoload_queue);
  tem = Fmemq (feature, Vfeatures);
  if (NILP (tem))
    Vfeatures = Fcons (feature, Vfeatures);
  LOADHIST_ATTACH (Fcons (Qprovide, feature));
  return feature;
}

DEFUN ("require", Frequire, Srequire, 1, 2, 0,
  "If feature FEATURE is not loaded, load it from FILENAME.\n\
If FEATURE is not a member of the list `features', then the feature\n\
is not loaded; so load the file FILENAME.\n\
If FILENAME is omitted, the printname of FEATURE is used as the file name.")
     (feature, file_name)
     Lisp_Object feature, file_name;
{
  register Lisp_Object tem;
  CHECK_SYMBOL (feature, 0);
  tem = Fmemq (feature, Vfeatures);
  LOADHIST_ATTACH (Fcons (Qrequire, feature));
  if (!NILP (tem))
    return (feature);
  else
    {
      int speccount = specpdl_depth ();

      /* Value saved here is to be restored into Vautoload_queue */
      record_unwind_protect (un_autoload, Vautoload_queue);
      Vautoload_queue = Qt;

      Fload (NILP (file_name) ? Fsymbol_name (feature) : file_name,
	     Qnil, Qt, Qnil);

      tem = Fmemq (feature, Vfeatures);
      if (NILP (tem))
	error ("Required feature %s was not provided",
	       XSYMBOL (feature)->name->data );

      /* Once loading finishes, don't undo it.  */
      Vautoload_queue = Qt;
      return (unbind_to (speccount, feature));
    }
}

/* Sound stuff, by jwz. */

extern void play_sound_file (char *name, int volume);
extern void play_sound_data (unsigned char *data, int length, int volume);

DEFUN ("play-sound-file", Fplay_sound_file, Splay_sound_file,
       1, 2, "fSound file name: ",
 "Play the named sound file on the console speaker at the specified volume\n(\
0-100, default specified by the `bell-volume' variable).\n\
The sound file must be in the Sun/NeXT U-LAW format."
       )
     (file, volume)
   Lisp_Object file, volume;
{
#ifdef USE_SOUND
  int vol;
  CHECK_STRING (file, 0);
  if (NILP (volume))
    vol = bell_volume;
  else
    {
      CHECK_FIXNUM (volume, 0);
      vol = XINT (volume);
    }

  file = Fexpand_file_name (file, Qnil);
  if (NILP (Ffile_readable_p (file)))
    if (NILP (Ffile_exists_p (file)))
      error ("file does not exist.");
    else
      error ("file is unreadable.");

  /* The sound code doesn't like getting SIGIO interrupts.  Unix sucks! */
  if (interrupt_input) unrequest_sigio ();
  play_sound_file ((char *) XSTRING (file)->data, vol);
  if (interrupt_input) request_sigio ();
  QUIT;

#endif /* USE_SOUND */

  return Qnil;
}

Lisp_Object Vsound_alist;

#ifdef USE_SOUND
int not_on_console; /*set at startup to determine whether we can play sounds*/
#endif

void (*beep_hook) (int vol);

DEFUN ("play-sound", Fplay_sound, Splay_sound, 1, 2, 0,
       "Play a sound of the provided type.\n\
See the variable sound-alist.")
     (sound, volume)
     Lisp_Object sound;
     Lisp_Object volume;
{
  int looking_for_default = 0;
  int vol;

 TRY_AGAIN:
    while (!NILP (sound) && SYMBOLP (sound) && !EQ (sound, Qt))
      {
	sound = Fcdr (Fassq (sound, Vsound_alist));
	/* allow (name foo) as well as (name . foo) */
	if (!CONSP (sound))
	  ;
	else if (NILP (Fcdr (sound)))
	  {
	    sound = Fcar (sound);
	  }
	else if (FIXNUMP (Fcar (Fcdr (sound))) &&
		 NILP (Fcdr (Fcdr (sound))))
	  {
	    if (NILP (volume)) volume = Fcar (Fcdr (sound));
	    sound = Fcar (sound);
	  }
	else if (FIXNUMP (Fcar (sound)) &&
		 NILP (Fcdr (Fcdr (sound))))
	  {
	    if (NILP (volume)) volume = Fcar (sound);
	    sound = Fcar (Fcdr (sound));
	  }
      }

  if (NILP (sound) && !looking_for_default)
    {
      looking_for_default = 1;
      sound = Qdefault;
      goto TRY_AGAIN;
    }

  if (FIXNUMP (volume))
    vol = XINT (volume);
  else
    vol = bell_volume;
  
#ifdef USE_SOUND
  if (not_on_console) sound = Qt;

  if (!STRINGP (sound))
    {
      if (beep_hook) (*beep_hook) (vol);
    }
  else
    {
      /* The sound code doesn't like getting SIGIO interrupts.  Unix sucks! */
      if (interrupt_input) unrequest_sigio ();
      play_sound_data (XSTRING (sound)->data, string_length (XSTRING (sound)),
                       vol);
      if (interrupt_input) request_sigio ();
      QUIT;
    }
#else  /* ! USE_SOUND */
  if (beep_hook) (*beep_hook) (vol);
#endif  /* ! USE_SOUND */

  return Qnil;
}


Lisp_Object Qyes_or_no_p;

void
syms_of_fns ()
{
  defsymbol (&Qstring_lessp, "string-lessp");
  defsymbol (&Qyes_or_no_p, "yes-or-no-p");

  DEFVAR_LISP ("features", &Vfeatures,
    "A list of symbols which are the features of the executing emacs.\n\
Used by `featurep' and `require', and altered by `provide'.");
  Vfeatures = Qnil;

  defsubr (&Sidentity);
  defsubr (&Srandom);
  defsubr (&Slength);
  defsubr (&Sstring_equal);
  defsubr (&Sstring_lessp);
  defsubr (&Sappend);
  defsubr (&Sconcat);
  defsubr (&Svconcat);
  defsubr (&Scopy_sequence);
  defsubr (&Scopy_alist);
  defsubr (&Scopy_tree);
  defsubr (&Ssubstring);
  defsubr (&Snthcdr);
  defsubr (&Snth);
  defsubr (&Selt);
  defsubr (&Smember);
  defsubr (&Smemq);
  defsubr (&Sassq);
  defsubr (&Sassoc);
  defsubr (&Srassq);
  defsubr (&Sdelq);
  defsubr (&Sdelete);
  defsubr (&Snreverse);
  defsubr (&Sreverse);
  defsubr (&Ssort);
  defsubr (&Sget);
  defsubr (&Sput);
  defsubr (&Sremprop);
  defsubr (&Sequal);
  defsubr (&Sfillarray);
  defsubr (&Snconc);
  defsubr (&Smapcar);
  defsubr (&Smapconcat);
  defsubr (&Sload_average);
  defsubr (&Sfeaturep);
  defsubr (&Srequire);
  defsubr (&Sprovide);

  DEFVAR_INT ("bell-volume", &bell_volume, "*How loud to be, from 0 to 100.");
  bell_volume = 50;

  DEFVAR_LISP ("sound-alist", &Vsound_alist,
    "An alist associating symbols with strings of audio-data.\n\
When `beep' or `ding' is called with one of the symbols, the associated\n\
sound data will be played instead of the standard beep.  This only works\n\
if you are running emacs on the console of a Sun SparcStation, SGI machine,\n\
or HP9000s700.\n\
\n\
Elements of this list should be of one of the following forms:\n\
\n\
   ( symbol . string-or-symbol )\n\
   ( symbol integer string-or-symbol )\n\
\n\
If the `string-or-symbol' is a string, then it should contain raw sound data,\n\
the contents of a `.au' file.  If it is a symbol, then that means that this\n\
element is an alias for some other element, and the sound-player will look\n\
for that next.  If the integer is provided, it is the volume at which the\n\
sound should be played, from 0 to 100.  \n\
\n\
If an element of this alist begins with the symbol `default', then that sound\n\
will be used when no other sound is appropriate.\n\
\n\
The symbol `t' in place of a sound-string means to use the default X beep.\n\
In this way, you can define beep-types to have different volumes even when\n\
not running on the console.\n\
\n\
You should probably add things to this list by calling the function\n\
load-sound-file.\n\
\n\
The following beep-types are used by emacs itself:\n\
\n\
    auto-save-error	when an auto-save does not succeed\n\
    command-error	when the emacs command loop catches an error\n\
    undefined-key	when you type a key that is undefined\n\
    undefined-click	when you use an undefined mouse-click combination\n\
    no-completion	during completing-read\n\
    y-or-n-p		when you type something other than 'y' or 'n'\n\
    yes-or-no-p  	when you type something other than 'yes' or 'no'\n\
\n\
Other lisp packages may use other beep types, but these are the ones that\n\
the C kernel of emacs uses.");
  Vsound_alist = Qnil;
  defsubr (&Splay_sound_file);
  defsubr (&Splay_sound);
#ifdef USE_SOUND
  not_on_console = 0;	/* set by X startup code */
#endif
  beep_hook = 0	;	/* set by X startup code */
}
