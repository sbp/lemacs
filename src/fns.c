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

#ifdef LOAD_AVE_TYPE
#ifdef BSD
/* It appears param.h defines BSD and BSD4_3 in 4.3
   and is not considerate enough to avoid bombing out
   if they are already defined.  */
#undef BSD
#ifdef BSD4_3
#undef BSD4_3
#define XBSD4_3 /* XBSD4_3 says BSD4_3 is supposed to be defined.  */
#endif
#include <sys/param.h>
/* Now if BSD or BSD4_3 was defined and is no longer,
   define it again.  */
#ifndef BSD
#define BSD
#endif
#ifdef XBSD4_3
#ifndef BSD4_3
#define BSD4_3
#endif
#endif /* XBSD4_3 */
#endif /* BSD */
#ifndef VMS
#ifndef NLIST_STRUCT
#include <a.out.h> 
#else /* NLIST_STRUCT */
#include <nlist.h>
#endif /* NLIST_STRUCT */
#endif /* not VMS */
#endif /* LOAD_AVE_TYPE */

#ifdef HAVE_HPUX_PSTAT
#include <sys/pstat.h>
#endif /* HAVE_HPUX_PSTAT */

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

/* Convert 32 bit items <-> (<high16> . <low16>) */

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
  if ((obj == Qnil) || (!CONSP (obj))) return 0;
  
  high = XCONS (obj)->car;
  if (!FIXNUMP (high)) return 0;
  
  low = XCONS (obj)->cdr; 
  if (!FIXNUMP (low)) return 0;
  
  return (XUINT (high) << 16) | (XUINT (low));
}

#ifdef NEED_STRDUP
char *
strdup (s)
     char *s;
{
    char *result = (char *) xmalloc (strlen (s) + 1);
    if (result == (char *) 0)
      return (char *) 0;
    strcpy (result, s);
    return result;
}
#endif


/* Lucid sound change */
Lisp_Object Vbell_volume;

Lisp_Object Qstring_lessp;
Lisp_Object Qyes_or_no_p;

DEFUN ("identity", Fidentity, Sidentity, 1, 1, 0,
  "Return the argument unchanged.")
  (arg)
     Lisp_Object arg;
{
  return arg;
}

extern int random ();
extern void srandom ();

DEFUN ("random", Frandom, Srandom, 0, 1, 0,
  "Return a pseudo-random number.\n\
On most systems all integers representable in Lisp are equally likely.\n\
  This is 24 bits' worth.\n\
With argument N, return random number in interval [0,N).\n\
With argument t, set the random number seed from the current time and pid.")
  (arg)
     Lisp_Object arg;
{
  int val;

  if (EQ (arg, Qt))
    srandom (getpid () + time (0));
  val = random ();
  if (FIXNUMP (arg) && XINT (arg) != 0)
    {
      /* Try to take our random number from the higher bits of VAL,
	 not the lower, since (says Gentzel) the low bits of `random'
	 are less random than the higher ones.  */
      val &= 0xfffffff;		/* Ensure positive.  */
      val >>= 5;
      if (XINT (arg) < 10000)
	val >>= 6;
      val %= XINT (arg);
    }
  return make_number (val);
}

/* Random data-structure functions */

DEFUN ("length", Flength, Slength, 1, 1, 0,
  "Return the length of vector, list or string SEQUENCE.\n\
A byte-code function object is also allowed.")
  (obj)
     register Lisp_Object obj;
{
  register Lisp_Object tail, val;
  register int i;

 retry:
  if (VECTORP (obj) || STRINGP (obj)
      || COMPILEDP (obj))
    return Farray_length (obj);
  else if (CONSP (obj))
    {
      for (i = 0, tail = obj; !NILP(tail); i++)
	{
	  QUIT;
	  tail = Fcdr (tail);
	}

      XFASTINT (val) = i;
      return val;
    }
  else if (NILP(obj))
    {
      XFASTINT (val) = 0;
      return val;
    }
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
     register Lisp_Object s1, s2;
{
  if (SYMBOLP (s1))
    XSET (s1, Lisp_String, XSYMBOL (s1)->name);
  if (SYMBOLP (s2))
    XSET (s2, Lisp_String, XSYMBOL (s2)->name);
  CHECK_STRING (s1, 0);
  CHECK_STRING (s2, 1);

  if (XSTRING (s1)->size != XSTRING (s2)->size ||
      memcmp (XSTRING (s1)->data, XSTRING (s2)->data, XSTRING (s1)->size))
    return Qnil;
  return Qt;
}

DEFUN ("string-lessp", Fstring_lessp, Sstring_lessp, 2, 2, 0,
  "T if first arg string is less than second in lexicographic order.\n\
Case is significant.\n\
Symbols are also allowed; their print names are used instead.")
  (s1, s2)
     register Lisp_Object s1, s2;
{
  register int i;
  register unsigned char *p1, *p2;
  register int end;

  if (SYMBOLP (s1))
    XSET (s1, Lisp_String, XSYMBOL (s1)->name);
  if (SYMBOLP (s2))
    XSET (s2, Lisp_String, XSYMBOL (s2)->name);
  CHECK_STRING (s1, 0);
  CHECK_STRING (s2, 1);

  p1 = XSTRING (s1)->data;
  p2 = XSTRING (s2)->data;
  end = XSTRING (s1)->size;
  if (end > XSTRING (s2)->size)
    end = XSTRING (s2)->size;

  for (i = 0; i < end; i++)
    {
      if (p1[i] != p2[i])
	return p1[i] < p2[i] ? Qt : Qnil;
    }
  return i < XSTRING (s2)->size ? Qt : Qnil;
}

static Lisp_Object concat ();

/* ARGSUSED */
Lisp_Object
concat2 (s1, s2)
     Lisp_Object s1, s2;
{
#ifdef NO_ARG_ARRAY
  Lisp_Object args[2];
  args[0] = s1;
  args[1] = s2;
  return concat (2, args, Lisp_String, 0);
#else
  return concat (2, &s1, Lisp_String, 0);
#endif /* NO_ARG_ARRAY */
}

DEFUN ("append", Fappend, Sappend, 0, MANY, 0,
  "Concatenate all the arguments and make the result a list.\n\
The result is a list whose elements are the elements of all the arguments.\n\
Each argument may be a list, vector or string.\n\
The last argument is not copied if it is a list.")
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
    return concat (1, &arg, XTYPE (arg), 0);
}

static Lisp_Object
concat (nargs, args, target_type, last_special)
     int nargs;
     Lisp_Object *args;
     enum Lisp_Type target_type;
     int last_special;
{
  Lisp_Object val;
  Lisp_Object len;
  register Lisp_Object tail;
  register Lisp_Object this;
  int toindex;
  register int leni;
  register int argnum;
  Lisp_Object last_tail;
  Lisp_Object prev;
  int mr_structs_size = nargs * sizeof (struct merge_replicas_struct);
  struct merge_replicas_struct *args_mr_structs = 
    (struct merge_replicas_struct *) alloca (mr_structs_size);

  memset ((char *) args_mr_structs, 0, mr_structs_size);

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
      this = args[argnum];
      if (!(CONSP (this) || NILP (this)
	    || VECTORP (this) || STRINGP (this)
	    || COMPILEDP (this)))
	{
	  if (FIXNUMP (this))
            args[argnum] = Fint_to_string (this);
	  else
	    args[argnum] = wrong_type_argument (Qsequencep, this);
	}
      
      if (STRINGP (this))
        args_mr_structs[argnum].dup_list = XSTRING (this)->dup_list;
      else
        args_mr_structs[argnum].dup_list = Qnil;
    }

  for (argnum = 0, leni = 0; argnum < nargs; argnum++)
    {
      this = args[argnum];
      len = Flength (this);
      args_mr_structs[argnum].entry_offset = leni;
      args_mr_structs[argnum].entry_length = XFASTINT (len);
      leni += XFASTINT (len);
    }
  XFASTINT (len) = leni;

  if (target_type == Lisp_Cons)
    val = Fmake_list (len, Qnil);
  else if (target_type == Lisp_Vector)
    val = Fmake_vector (len, Qnil);
  else
    {
      val = Fmake_string (len, len);
      XSTRING(val)->dup_list = merge_replicas (nargs, args_mr_structs);
    }

  /* In append, if all but last arg are nil, return last arg */
  if (target_type == Lisp_Cons && EQ (val, Qnil))
    return last_tail;

  if (CONSP (val))
    tail = val, toindex = -1;	/* -1 in toindex is flag we are
				    making a list */
  else
    toindex = 0;

  prev = Qnil;

  for (argnum = 0; argnum < nargs; argnum++)
    {
      Lisp_Object thislen;
      int thisleni;
      register int thisindex = 0;

      this = args[argnum];
      if (!CONSP (this))
	{
	  thislen = Flength (this);
	  thisleni = XINT (thislen);
	}

      while (1)
	{
	  register Lisp_Object elt;

	  /* We've come to the end of this arg, so exit. */
	  if (NILP (this))
	    break;

	  /* Fetch next element of `this' arg into `elt' */
	  if (CONSP (this))
	    elt = Fcar (this), this = Fcdr (this);
	  else
	    {
	      if (thisindex >= thisleni)
		break;

	      if (STRINGP (this))
		XFASTINT (elt) = XSTRING (this)->data[thisindex++];
	      else
		elt = XVECTOR (this)->contents[thisindex++];
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
are shared, however.")
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


DEFUN ("copy-tree", Fcopy_tree, Scopy_tree, 1, 1, 0,
  "Return a copy of a list or vector, and substructures.\n\
The argument is copied, and any lists or vectors contained within it\n\
are copied recursively.  Circularities and shared substructures are\n\
not preserved.  Strings are not copied.")
   (arg)
     Lisp_Object arg;
{
  arg = Fcopy_sequence (arg);
  if (CONSP (arg))
    {
      Lisp_Object rest = arg;
      while (CONSP (rest))
	{
	  Lisp_Object elt = XCONS (rest)->car;
	  QUIT;
	  if (CONSP (elt) || VECTORP (elt))
	    XCONS (rest)->car = Fcopy_tree (elt);
	  if (VECTORP (XCONS (rest)->cdr)) /* hack for (a b . [c d]) */
	    XCONS (rest)->cdr = Fcopy_tree (XCONS (rest)->cdr);
	  rest = XCONS (rest)->cdr;
	}
    }
  else if (VECTORP (arg))
    {
      int i = XVECTOR (arg)->size;
      int j;
      for (j = 0; j < i; j++)
	{
	  Lisp_Object elt = XVECTOR (arg)->contents [j];
	  QUIT;
	  if (CONSP (elt) || VECTORP (elt))
	    XVECTOR (arg)->contents [j] = Fcopy_tree (elt);
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
  CHECK_STRING (string, 0);
  CHECK_FIXNUM (from, 1);
  if (NILP (to))
    to = Flength (string);
  else
    CHECK_FIXNUM (to, 2);

  if (XINT (from) < 0)
    XSETINT (from, XINT (from) + XSTRING (string)->size);
  if (XINT (to) < 0)
    XSETINT (to, XINT (to) + XSTRING (string)->size);
  if (!(0 <= XINT (from) && XINT (from) <= XINT (to)
        && XINT (to) <= XSTRING (string)->size))
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
  for (i = 0; i < num && ! NILP (list); i++)
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
  CHECK_FIXNUM (n, 0);
  while (1)
    {
      if (CONSP (seq) || NILP (seq))
	return Fcar (Fnthcdr (n, seq));
      else if (STRINGP (seq) ||
	       VECTORP (seq))
	return Faref (seq, n);
      else
	seq = wrong_type_argument (Qsequencep, seq);
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
  Lisp_Object result;
  Lisp_Object oinhibit = Vinhibit_quit;
  Vinhibit_quit = Qt;
  result = Fassoc (key, list);
  Vinhibit_quit = oinhibit;
  return result;
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
If the first member of LIST is ELT, there is no way to remove it by side effect;\n\
therefore, write `(setq foo (delete element foo))'\n\
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
      tem = Fequal (elt, tem);
      if (!NILP (tem))
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

  if (NILP (list)) return list;
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

static Lisp_Object list_merge ();

Lisp_Object list_sort (list, lisp_arg, pred_fn)
     Lisp_Object list, lisp_arg;
     int (*pred_fn)();
{
  Lisp_Object front, back;
  Lisp_Object len, tem;
  struct gcpro gcpro1, gcpro2;
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

  GCPRO2 (front, back);
  front = list_sort (front, lisp_arg, pred_fn);
  back = list_sort (back, lisp_arg, pred_fn);
  UNGCPRO;
  return list_merge (front, back, lisp_arg, pred_fn);
}

extern Lisp_Object Qgc_currently_forbidden;

/* #### this is stupid and should be expunged */
Lisp_Object
safe_funcall_hook (Lisp_Object hook, int nargs, Lisp_Object arg1,
		   Lisp_Object arg2, Lisp_Object arg3)
{
  Lisp_Object result = Qnil;
  int count = specpdl_depth;
  specbind (Qgc_currently_forbidden, 1);

  if (!NILP (hook))
    {
      switch (nargs)
	{
	case 0: result = call0 (hook); break;
	case 1: result = call1 (hook, arg1); break;
	case 2: result = call2 (hook, arg1, arg2); break;
	case 3: result = call3 (hook, arg1, arg2, arg3); break;
	}
    }      
  return unbind_to (count, result);
}

void
run_hooks_with_args (hook_var, args, nargs)
     Lisp_Object hook_var, *args;
     int nargs;
{
  Lisp_Object rest;
  struct gcpro gcpro1;
  if (NILP (Fboundp (hook_var)))
    return;
  rest = Fsymbol_value (hook_var);
  if (NILP (rest))
    return;
  GCPRO1 (rest);
  if (SYMBOLP (rest) || EQ (Qlambda, Fcar (rest)))
    rest = list1 (rest);
  while (!NILP (rest))
    {
      switch (nargs)
	{
	case 0: call0 (Fcar (rest)); break;
	case 1: call1 (Fcar (rest), args[1]); break;
	case 2: call2 (Fcar (rest), args[1], args[2]); break;
	case 3: call3 (Fcar (rest), args[1], args[2], args[3]); break;
	case 4: call4 (Fcar (rest), args[1], args[2], args[3], args[4]); break;
	  /* if we ever want more, we'll add the clauses... */
	default: error ("run_hooks_with_args called with too many args");
	}
      rest = Fcdr (rest);
    }
  UNGCPRO;
}


/* feel free to write the others as needed... */
void
run_hooks_1_arg (hook_var, arg)
     Lisp_Object hook_var, arg;
{
  run_hooks_with_args (hook_var, &arg, 1);
}


static Lisp_Object 
merge_pred_function (obj1, obj2, pred)
     Lisp_Object obj1, obj2, pred;
{
  Lisp_Object tmp = Qnil;

  /* prevents the GC from happening in call2 */
  int count = specpdl_depth;
  specbind (Qgc_currently_forbidden, 1);
  tmp = call2 (pred, obj1, obj2);
  unbind_to (count, Qnil);

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
merge (org_l1, org_l2, pred)
     Lisp_Object org_l1, org_l2, pred;
{
  return list_merge (org_l1, org_l2, pred, merge_pred_function);
}


static Lisp_Object
list_merge (org_l1, org_l2, lisp_arg, pred_fn)
     Lisp_Object org_l1, org_l2, lisp_arg;
     int (*pred_fn)();
{
  Lisp_Object value;
  register Lisp_Object tail;
  Lisp_Object tem;
  register Lisp_Object l1, l2;
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

      if ((*pred_fn)(Fcar (l2), Fcar (l1), lisp_arg) < 0)
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


DEFUN ("get", Fget, Sget, 2, 2, 0,
  "Return the value of SYMBOL's PROPNAME property.\n\
This is the last VALUE stored with `(put SYMBOL PROPNAME VALUE)'.")
  (sym, prop)
     Lisp_Object sym;
     register Lisp_Object prop;
{
  register Lisp_Object tail;

  for (tail = Fsymbol_plist (sym); !NILP (tail); tail = Fcdr (Fcdr (tail)))
    if (EQ (prop, Fcar (tail)))
      return Fcar (Fcdr (tail));

  return Qnil;
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

extern Lisp_Object event_equal (Lisp_Object, Lisp_Object);

DEFUN ("equal", Fequal, Sequal, 2, 2, 0,
  "T if two Lisp objects have similar structure and contents.\n\
They must have the same data type.\n\
Conses are compared by comparing the cars and the cdrs.\n\
Vectors and strings are compared element by element.\n\
Numbers are compared by value.  Symbols must match exactly.")
  (o1, o2)
     register Lisp_Object o1, o2;
{
do_cdr:
  QUIT;
  if (XTYPE (o1) != XTYPE (o2)) return Qnil;
  if (XINT (o1) == XINT (o2)) return Qt;
  if (CONSP (o1))
    {
      Lisp_Object v1;
      v1 = Fequal (Fcar (o1), Fcar (o2));
      if (NILP (v1))
	return v1;
      o1 = Fcdr (o1), o2 = Fcdr (o2);
      goto do_cdr;
    }
  if (MARKERP (o1))
    {
      return (XMARKER (o1)->buffer == XMARKER (o2)->buffer
	      && XMARKER (o1)->bufpos == XMARKER (o2)->bufpos)
	? Qt : Qnil;
    }
  if (VECTORP (o1))
    {
      register int index;
      if (XVECTOR (o1)->size != XVECTOR (o2)->size)
	return Qnil;
      for (index = 0; index < XVECTOR (o1)->size; index++)
	{
	  Lisp_Object v, v1, v2;
	  v1 = XVECTOR (o1)->contents [index];
	  v2 = XVECTOR (o2)->contents [index];
	  v = Fequal (v1, v2);
	  if (NILP (v)) return v;
	}
      return Qt;
    }
  if (STRINGP (o1))
    {
      if (XSTRING (o1)->size != XSTRING (o2)->size)
	return Qnil;
      if (memcmp (XSTRING (o1)->data, XSTRING (o2)->data, XSTRING (o1)->size))
	return Qnil;
      return Qt;
    }
  if (EVENTP (o1))
    return event_equal (o1, o2);
#ifdef LISP_FLOAT_TYPE
  if (FLOATP (o1))
    return (XFLOAT (o1)->data == XFLOAT (o2)->data) ? Qt : Qnil;
#endif

  return Qnil;
}

DEFUN ("fillarray", Ffillarray, Sfillarray, 2, 2, 0,
  "Store each element of ARRAY with ITEM.  ARRAY is a vector or string.")
  (array, item)
     Lisp_Object array, item;
{
  register int size, index, charval;
 retry:
  if (VECTORP (array))
    {
      register Lisp_Object *p = XVECTOR (array)->contents;
      size = XVECTOR (array)->size;
      for (index = 0; index < size; index++)
	p[index] = item;
    }
  else if (STRINGP (array))
    {
      register unsigned char *p = XSTRING (array)->data;
      CHECK_FIXNUM (item, 1);
      charval = XINT (item);
      size = XSTRING (array)->size;
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

/* ARGSUSED */
Lisp_Object
nconc2 (s1, s2)
     Lisp_Object s1, s2;
{
#ifdef NO_ARG_ARRAY
  Lisp_Object args[2];
  args[0] = s1;
  args[1] = s2;
  return Fnconc (2, args);
#else
  return Fnconc (2, &s1);
#endif /* NO_ARG_ARRAY */
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
	  XFASTINT (dummy) = XSTRING (seq)->data[i];
	  vals[i] = call1 (fn, dummy);
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
  leni = XFASTINT (len);
  args = (Lisp_Object *) alloca (leni * sizeof (Lisp_Object));

  mapcar1 (leni, args, fn, seq);

  return Flist (leni, args);
}

/* Avoid static vars inside a function since in HPUX they dump as pure.  */
static int ldav_initialized;
static int ldav_channel;
#ifdef LOAD_AVE_TYPE
#ifndef VMS
static struct nlist ldav_nl[2];
#endif /* VMS */
#endif /* LOAD_AVE_TYPE */

#define channel ldav_channel
#define initialized ldav_initialized
#define nl ldav_nl

DEFUN ("load-average", Fload_average, Sload_average, 0, 0, 0,
  "Return list of 1 minute, 5 minute and 15 minute load averages.\n\
Each of the three load averages is multiplied by 100,\n\
then converted to integer.\n\
\n\
This won't work unless the emacs executable is setgid kmem\n(\
assuming that /dev/kmem is in the group kmem.)")
  ()
{
#ifndef LOAD_AVE_TYPE
  error ("load-average not implemented for this operating system");

#else /* LOAD_AVE_TYPE defined */

  LOAD_AVE_TYPE load_ave[3];
#ifdef HAVE_HPUX_PSTAT
  struct pst_dynamic pst_dyn;
#endif /* HAVE HPUX_PSTAT */
#ifdef VMS
#ifndef eunice
#include <iodef.h>
#include <descrip.h>
#else
#include <vms/iodef.h>
  struct {int dsc$w_length; char *dsc$a_pointer;} descriptor;
#endif /* eunice */
#endif /* VMS */

  /* If this fails for any reason, we can return (0 0 0) */
  load_ave[0] = 0.0; load_ave[1] = 0.0; load_ave[2] = 0.0;

#ifdef VMS
  /*
   *	VMS specific code -- read from the Load Ave driver
   */

  /*
   *	Ensure that there is a channel open to the load ave device
   */
  if (initialized == 0)
    {
      /* Attempt to open the channel */
#ifdef eunice
      descriptor.size = 18;
      descriptor.ptr  = "$$VMS_LOAD_AVERAGE";
#else
      $DESCRIPTOR(descriptor, "LAV0:");
#endif
      if (sys$assign (&descriptor, &channel, 0, 0) & 1)
	initialized = 1;
    }
  /*
   *	Read the load average vector
   */
  if (initialized)
    {
      if (!(sys$qiow (0, channel, IO$_READVBLK, 0, 0, 0,
		     load_ave, 12, 0, 0, 0, 0)
	    & 1))
	{
	  sys$dassgn (channel);
	  initialized = 0;
	}
    }
#else  /* not VMS */

#ifdef HAVE_HPUX_PSTAT
    pstat(PSTAT_DYNAMIC,(union pstun)&pst_dyn,sizeof(pst_dyn),0,0);
    load_ave[0] = pst_dyn.psd_avg_1_min;
    load_ave[1] = pst_dyn.psd_avg_5_min;
    load_ave[2] = pst_dyn.psd_avg_15_min;
#else /* HAVE_HPUX_PSTAT */
  /*
   *	4.2BSD UNIX-specific code -- read _avenrun from /dev/kmem
   */

  /*
   *	Make sure we have the address of _avenrun
   */
  if (nl[0].n_value == 0)
    {
      /*
       *	Get the address of _avenrun
       */
#ifndef NLIST_STRUCT
      strcpy (nl[0].n_name, LDAV_SYMBOL);
      nl[1].n_zeroes = 0;
#else /* NLIST_STRUCT */
#ifdef convex
      nl[0].n_un.n_name = LDAV_SYMBOL;
      nl[1].n_un.n_name = 0;
#else /* not convex */
#ifdef NEXT_KERNEL_FILE
      nl[0].n_un.n_name = LDAV_SYMBOL;
      nl[1].n_un.n_name = 0;
#else
      nl[0].n_name = LDAV_SYMBOL;
      nl[1].n_name = 0;
#endif /* not NEXT_KERNEL_FILE */
#endif /* not convex */
#endif /* NLIST_STRUCT */

#ifdef NEXT_KERNEL_FILE
      nlist (NEXT_KERNEL_FILE, nl);
#else
      nlist (KERNEL_FILE, nl);
#endif      /* NEXT_KERNEL_FILE */

#ifdef FIXUP_KERNEL_SYMBOL_ADDR
      FIXUP_KERNEL_SYMBOL_ADDR (nl);
#endif /* FIXUP_KERNEL_SYMBOL_ADDR */
    }
  /*
   *	Make sure we have /dev/kmem open
   */
  if (initialized == 0)
    {
      /*
       *	Open /dev/kmem
       */
      channel = open ("/dev/kmem", 0);
      if (channel >= 0) initialized = 1;
    }
  /*
   *	If we can, get the load ave values
   */
  if ((nl[0].n_value != 0) && (initialized != 0))
    {
      /*
       *	Seek to the correct address
       */
      lseek (channel, (long) nl[0].n_value, 0);
      if (read (channel, (char *) load_ave, sizeof load_ave)
	  != sizeof(load_ave))
	{
	  close (channel);
	  initialized = 0;
	}
    }
#endif /* not HAVE_HPUX_PSTAT */
#endif /* not VMS */

  /*
   *	Return the list of load average values
   */
  return Fcons (make_number (LOAD_AVE_CVT (load_ave[0])),
		Fcons (make_number (LOAD_AVE_CVT (load_ave[1])),
		       Fcons (make_number (LOAD_AVE_CVT (load_ave[2])),
			      Qnil)));
#endif /* LOAD_AVE_TYPE */
}

#undef channel
#undef initialized
#undef nl

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
  if (NILP (tem))
    {
      Fload (NILP (file_name) ? Fsymbol_name (feature) : file_name,
	     Qnil, Qt, Qnil);
      tem = Fmemq (feature, Vfeatures);
      if (NILP (tem))
	error ("Required feature %s was not provided",
	       XSYMBOL (feature)->name->data );
    }
  return feature;
}

/* Sound stuff, by jwz. */

#ifdef USE_SOUND
extern void play_sound_file (char *name, int volume);
extern void play_sound_data (unsigned char *data, int length, int volume);

extern int interrupt_input;
extern void request_sigio (void);
extern void unrequest_sigio (void);
#endif /* USE_SOUND */

DEFUN ("play-sound-file", Fplay_sound_file, Splay_sound_file,
       1, 2, "fSound file name: ",
 "Play the named sound file on the console speaker at the specified volume\n(\
0-100, default specified by the `bell-volume' variable).\n\
The sound file must be in the Sun/NeXT U-LAW format."
       )
     (file, vol)
	Lisp_Object file, vol;
{
#ifdef USE_SOUND

  CHECK_STRING (file, 0);
  if (NILP (vol)) vol = Vbell_volume;
  CHECK_FIXNUM (vol, 0);

  file = Fexpand_file_name (file, Qnil);
  if (Qnil == Ffile_readable_p (file))
    if (Qnil == Ffile_exists_p (file))
      error ("file does not exist.");
    else
      error ("file is unreadable.");

  /* The sound code doesn't like getting SIGIO interrupts.  Unix sucks! */
  if (interrupt_input) unrequest_sigio ();
  play_sound_file ((char *) XSTRING(file)->data, XINT(vol));
  if (interrupt_input) request_sigio ();
  QUIT;

#endif /* USE_SOUND */

  return Qnil;
}

Lisp_Object Vsound_alist;

#ifdef USE_SOUND
int not_on_console; /*set at startup to determine whether we can play sounds*/
#endif

void (*beep_hook) ();

DEFUN ("play-sound", Fplay_sound, Splay_sound, 1, 2, 0,
       "Play a sound of the provided type.\n\
See the variable sound-alist.")
     (sound, volume)
     Lisp_Object sound;
     Lisp_Object volume;
{
  int looking_for_default = 0;

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
      sound = intern ("default");
      goto TRY_AGAIN;
    }

  if (!FIXNUMP (volume))
    volume = Vbell_volume;
  
#ifdef USE_SOUND
  if (not_on_console) sound = Qt;

  if (!STRINGP (sound))
    {
      if (beep_hook) (*beep_hook) (XINT (volume));
    }
  else
    {
      /* The sound code doesn't like getting SIGIO interrupts.  Unix sucks! */
      if (interrupt_input) unrequest_sigio ();
      play_sound_data (XSTRING (sound)->data, XSTRING (sound)->size,
		       XINT (volume));
      if (interrupt_input) request_sigio ();
      QUIT;
    }
#else  /* ! USE_SOUND */
  if (beep_hook) (*beep_hook) (XINT (volume));
#endif  /* ! USE_SOUND */

  return Qnil;
}


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

  /* Lucid sound change */
  DEFVAR_LISP ("bell-volume", &Vbell_volume, "How loud to be, from 0 to 100.");
  Vbell_volume = make_number (50);

  DEFVAR_LISP ("sound-alist", &Vsound_alist,
    "An alist associating symbols with strings of audio-data.\n\
When `beep' or `ding' is called with one of the symbols, the associated\n\
sound data will be played instead of the standard beep.  This only works\n\
if you are logged in on the console of a Sun SparcStation or SGI machine.\n\
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
not running on the console of a Sun4.\n\
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
