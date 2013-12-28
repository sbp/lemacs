/* Fundamental definitions for GNU Emacs Lisp interpreter.
   Copyright (C) 1985-1993 Free Software Foundation, Inc.

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

#if __STDC__

/* I don't know how correct this attempt to get more prototypes is... */
# if defined(sun) && defined(_POSIX_SOURCE)
#  undef _POSIX_SOURCE
# endif

# if defined(__lucid) && !defined(__STDC_EXTENDED__)
#  define __STDC_EXTENDED__ 1
# endif

# include <stdlib.h>
# include <unistd.h>
# include <string.h>

# ifdef __lucid
#  include <sysent.h>
# endif

#ifndef index
extern char *index (const char *, char);
#endif

#ifdef NeXT
typedef int pid_t;
#endif

#endif /* __STDC__ */

/* Define the fundamental Lisp data structures */

/* This is the set of Lisp data types */

enum Lisp_Type
  {
    /* Integer.  XINT(obj) is the integer value. */
    Lisp_Int,

    /* Symbol.  XSYMBOL (object) points to a struct Lisp_Symbol. */
    Lisp_Symbol,

    /* Marker (buffer ptr).  XMARKER(object) points to a struct Lisp_Marker. */
    Lisp_Marker,

    /* String.  XSTRING (object) points to a struct Lisp_String.
       The length of the string, and its contents, are stored therein. */
    Lisp_String,

    /* Vector of Lisp objects.  XVECTOR(object) points to a struct Lisp_Vector.
       The length of the vector, and its contents, are stored therein. */
    Lisp_Vector,

    /* Cons.  XCONS (object) points to a struct Lisp_Cons. */
    Lisp_Cons,

    /* Byte-compiled function.  A vector of 4 to 6 elements which are the
       arglist, bytecode-string, constant vector, stack size,
       (optional) doc string, and (optional) interactive spec.  */
    Lisp_Compiled,

    /* Editor buffer.  XBUFFER(obj) points to a struct buffer.  */
    Lisp_Buffer,

    /* Built-in function.  XSUBR(obj) points to a struct Lisp_Subr
       which describes how to call the function, and its documentation,
       as well as pointing to the code. */
    Lisp_Subr,

    /* Internal value return by subroutines of read.
       The user never sees this data type.
       Its value is just a number. */
    Lisp_Internal,

    /* Forwarding pointer to an int variable.
       This is allowed only in the value cell of a symbol,
       and it means that the symbol's value really lives in the
       specified int variable.
       XINTPTR(obj) points to the int variable. */
    Lisp_Intfwd,

    /* Boolean forwarding pointer to an int variable.
       This is like Lisp_Intfwd except that the ostensible
       "value" of the symbol is t if the int variable is nonzero,
       nil if it is zero.  XINTPTR(obj) points to the int variable. */
    Lisp_Boolfwd,

    /* Object describing a connection to a subprocess.
       It points to storage of type  struct Lisp_Process  */
    Lisp_Process,

    /* Forwarding pointer to a Lisp_Object variable.
       This is allowed only in the value cell of a symbol,
       and it means that the symbol's value really lives in the
       specified variable.
       XOBJFWD(obj) points to the Lisp_Object variable. */
    Lisp_Objfwd,

    /* Pointer to a vector-like object describing a display screen
       on which Emacs can display a window hierarchy.  */
    Lisp_Screen,

    /* Used when a FILE * value needs to be passed
       in an argument of type Lisp_Object.
       You must do *(FILE **) XPNTR(obj) to get the value.
       The user will never see this data type. */
    Lisp_Internal_Stream,

    /* Used in a symbol value cell when the symbol's value is per-buffer.
        The actual contents are a cons cell which starts a list like this:
        (REALVALUE BUFFER CURRENT-ALIST-ELEMENT . DEFAULT-VALUE)).

	BUFFER is the last buffer for which this symbol's value was
	made up to date.

        CURRENT-ALIST-ELEMENT is a pointer to an element of BUFFER's
	b_local_var_alist, that being the element whose car is this variable.
        Or it can be a pointer to the (CURRENT-ALIST-ELEMENT . DEFAULT-VALUE), if BUFFER
	does not have an element in its alist for this variable
	(that is, if BUFFER sees the default value of this variable).

	If we want to examine or set the value and BUFFER is current,
	we just examine or set REALVALUE.
	If BUFFER is not current, we store the current REALVALUE value into
	CURRENT-ALIST-ELEMENT, then find the appropriate alist element for
	the buffer now current and set up CURRENT-ALIST-ELEMENT.
	Then we set REALVALUE out of that element, and store into BUFFER.

	If we are setting the variable and the current buffer does not have
	an alist entry for this variable, an alist entry is created.

	Note that REALVALUE can be a forwarding pointer.
	Each time it is examined or set, forwarding must be done.  */
    Lisp_Buffer_Local_Value,

    /* Like Lisp_Buffer_Local_Value with one difference:
	merely setting the variable while some buffer is current
	does not cause that buffer to have its own local value of this variable.
	Only make-local-variable does that.  */
    Lisp_Some_Buffer_Local_Value,


    /* Like Lisp_Objfwd except that value lives in a slot
       in the current buffer.  Value is byte index of slot within buffer */
    Lisp_Buffer_Objfwd,

    /* In symbol value cell, means var is unbound.
       In symbol function cell, means function name is undefined. */
    Lisp_Void,

    /* Window used for Emacs display.
       Data inside looks like a Lisp_Vector.  */
    Lisp_Window,

    /* Used by save,set,restore-window-configuration */
    Lisp_Window_Configuration,

    /* An active region in buffer or string.  XEXTENT (obj) is
       typedef EXTENT. */
    Lisp_Extent,

    Lisp_Extent_Data,

    Lisp_Extent_Replica,

#ifdef LISP_FLOAT_TYPE
    Lisp_Float,
#endif /* LISP_FLOAT_TYPE */

    Lisp_Event,
    Lisp_Keymap
  };

#ifndef NO_UNION_TYPE

#ifndef BIG_ENDIAN

/* Definition of Lisp_Object for little-endian machines.  */

typedef
union Lisp_Object
  {
    /* Used for comparing two Lisp_Objects;
       also, positive integers can be accessed fast this way. */
    int i;

    struct
      {
	int val: 24;
	char type;
      } s;
    struct
      {
	unsigned int val: 24;
	char type;
      } u;
    struct
      {
	unsigned int val: 24;
	enum Lisp_Type type: 7;
	/* The markbit is not really part of the value of a Lisp_Object,
	   and is always zero except during garbage collection.  */
	unsigned int markbit: 1;
      } gu;
  }
Lisp_Object;

#else /* If BIG_ENDIAN */

typedef
union Lisp_Object
  {
    /* Used for comparing two Lisp_Objects;
       also, positive integers can be accessed fast this way. */
    int i;

    struct
      {
	char type;
	int val: 24;
      } s;
    struct
      {
	char type;
	unsigned int val: 24;
      } u;
    struct
      {
	/* The markbit is not really part of the value of a Lisp_Object,
	   and is always zero except during garbage collection.  */
	unsigned int markbit: 1;
	enum Lisp_Type type: 7;
	unsigned int val: 24;
      } gu;
  }
Lisp_Object;

#endif /* BIG_ENDIAN */

#endif /* NO_UNION_TYPE */


/* If union type is not wanted, define Lisp_Object as just a number
   and define the macros below to extract fields by shifting */

#ifdef NO_UNION_TYPE

#define Lisp_Object int

/* These values are overridden by the m- file on some machines.  */
#ifndef VALBITS
#define VALBITS 24
#endif

#ifndef GCTYPEBITS
#define GCTYPEBITS 7
#endif

#ifndef VALMASK
#define VALMASK ((1<<VALBITS) - 1)
#endif
#define GCTYPEMASK ((1<<GCTYPEBITS) - 1)
#define MARKBIT (1 << (VALBITS + GCTYPEBITS))

#endif /* NO_UNION_TYPE */

/* These macros extract various sorts of values from a Lisp_Object.
 For example, if tem is a Lisp_Object whose type is Lisp_Cons,
 XCONS (tem) is the struct Lisp_Cons * pointing to the memory for that cons. */

#ifdef NO_UNION_TYPE

/* One need to override this if there must be high bits set in data space
   (doing the result of the below & ((1 << (GCTYPE + 1)) - 1) would work
    on all machines, but would penalise machines which don't need it)
 */
#ifndef XTYPE
#define XTYPE(a) ((enum Lisp_Type) ((a) >> VALBITS))
#endif

#ifndef XSETTYPE
#define XSETTYPE(a, b) ((a)  =  XUINT (a) | ((int)(b) << VALBITS))
#endif

/* Use XFASTINT for fast retrieval and storage of integers known
  to be positive.  This takes advantage of the fact that Lisp_Int is 0.  */
#define XFASTINT(a) (a)

/* Extract the value of a Lisp_Object as a signed integer.  */

#ifndef XINT   /* Some machines need to do this differently.  */
#define XINT(a) (((a) << (INTBITS-VALBITS)) >> (INTBITS-VALBITS))
#endif

/* Extract the value as an unsigned integer.  This is a basis
   for extracting it as a pointer to a structure in storage.  */

#ifndef XUINT
#define XUINT(a) ((a) & VALMASK)
#endif

#ifndef XPNTR
#ifdef HAVE_SHM
/* In this representation, data is found in two widely separated segments.  */
extern int pure_size;
#define XPNTR(a) \
  (XUINT (a) | (XUINT (a) > pure_size ? DATA_SEG_BITS : PURE_SEG_BITS))
#else /* not HAVE_SHM */
#ifdef DATA_SEG_BITS
/* This case is used for the rt-pc.
   In the diffs I was given, it checked for ptr = 0
   and did not adjust it in that case.
   But I don't think that zero should ever be found
   in a Lisp object whose data type says it points to something.  */
#define XPNTR(a) (XUINT (a) | DATA_SEG_BITS)
#else
#define XPNTR(a) XUINT (a)
#endif
#endif /* not HAVE_SHM */
#endif /* no XPNTR */

#ifndef XSETINT
#if 0 /* This is the nominal def, but it breaks things. */
#define XSETINT(a, b)  XSET ((a), Lisp_Int, (b))
#endif
#define XSETINT(a, b)  ((a) = ((a) & ~VALMASK) |  ((b) & VALMASK))
#endif

#ifndef XSET
#define XSET(var, type, ptr) \
   ((var) = ((int)(type) << VALBITS) + ((int) (ptr) & VALMASK))
#endif

/* During garbage collection, XGCTYPE must be used for extracting types
 so that the mark bit is ignored.  XMARKBIT accesses the markbit.
 Markbits are used only in particular slots of particular structure types.
 Other markbits are always zero.
 Outside of garbage collection, all mark bits are always zero.  */

#ifndef XGCTYPE
#define XGCTYPE(a) ((enum Lisp_Type) (((a) >> VALBITS) & GCTYPEMASK))
#endif

#if VALBITS + GCTYPEBITS == INTBITS - 1
/* Make XMARKBIT faster if mark bit is sign bit.  */
#ifndef XMARKBIT
#define XMARKBIT(a) ((a) < 0)
#endif
#endif /* markbit is sign bit */

#ifndef XMARKBIT
#define XMARKBIT(a) ((a) & MARKBIT)
#endif

#ifndef XSETMARKBIT
#define XSETMARKBIT(a,b) ((a) = ((a) & ~MARKBIT) | ((b) ? MARKBIT : 0))
#endif

#ifndef XMARK
#define XMARK(a) ((a) |= MARKBIT)
#endif

#ifndef XUNMARK
#define XUNMARK(a) ((a) &= ~MARKBIT)
#endif

#endif /* NO_UNION_TYPE */

#ifndef NO_UNION_TYPE

#define XTYPE(a) ((enum Lisp_Type) (a).u.type)
#define XSETTYPE(a, b) ((a).u.type = (char) (b))

/* Use XFASTINT for fast retrieval and storage of integers known
  to be positive.  This takes advantage of the fact that Lisp_Int is 0.  */
#define XFASTINT(a) ((a).i)

#ifdef EXPLICIT_SIGN_EXTEND
/* Make sure we sign-extend; compilers have been known to fail to do so.  */
#define XINT(a) (((a).i << (32-VALBITS)) >> (32-VALBITS))
#else
#define XINT(a) ((a).s.val)
#endif /* EXPLICIT_SIGN_EXTEND */

#define XUINT(a) ((a).u.val)
#define XPNTR(a) ((a).u.val)
#define XSETINT(a, b) ((a).s.val = (int) (b))

#define XSET(var, vartype, ptr) \
   (((var).s.type = ((char) (vartype))), ((var).s.val = ((int) (ptr))))

/* During garbage collection, XGCTYPE must be used for extracting types
 so that the mark bit is ignored.  XMARKBIT access the markbit.
 Markbits are used only in particular slots of particular structure types.
 Other markbits are always zero.
 Outside of garbage collection, all mark bits are always zero.  */

#define XGCTYPE(a) ((a).gu.type)
#define XMARKBIT(a) ((a).gu.markbit)
#define XSETMARKBIT(a,b) (XMARKBIT(a) = (b))
#define XMARK(a) (XMARKBIT(a) = 1)
#define XUNMARK(a) (XMARKBIT(a) = 0)

#endif /* NO_UNION_TYPE */


#define XCONS(a) ((struct Lisp_Cons *) XPNTR(a))
#define XBUFFER(a) ((struct buffer *) XPNTR(a))
#define XVECTOR(a) ((struct Lisp_Vector *) XPNTR(a))
#define XSUBR(a) ((struct Lisp_Subr *) XPNTR(a))
#define XSTRING(a) ((struct Lisp_String *) XPNTR(a))
#define XSYMBOL(a) ((struct Lisp_Symbol *) XPNTR(a))
#define XMARKER(a) ((struct Lisp_Marker *) XPNTR(a))
#define XOBJFWD(a) ((Lisp_Object *) XPNTR(a))
#define XINTPTR(a) ((int *) XPNTR(a))
#define XWINDOW(a) ((struct window *) XPNTR(a))
#define XEXTENT(a) ((EXTENT) XPNTR(a))
#define XEXTENT_REPLICA(a) ((EXTENT_REPLICA) XPNTR(a))
#define XPROCESS(a) ((struct Lisp_Process *) XPNTR(a))
#ifdef LISP_FLOAT_TYPE
#define XFLOAT(a) ((struct Lisp_Float *) XPNTR(a))
#else
#define XFLOAT(a) --- error!  No float support. ---
#endif
#define XEVENT(a) ((struct Lisp_Event *) XPNTR(a))
#define XKEYMAP(a) ((struct Lisp_Keymap *) XPNTR(a))


/* In a cons, the markbit of the car is the gc mark bit */

struct Lisp_Cons
  {
    Lisp_Object car, cdr;
  };

/* In a string or vector, the sign bit of the `size' is the gc mark bit */

struct Lisp_String
  {
    int size;
    Lisp_Object dup_list;
    unsigned char *data;
  };

struct Lisp_Vector
  {
    int size;
    struct Lisp_Vector *next;
    Lisp_Object contents[1];
  };

/* In a symbol, the markbit of the plist is used as the gc mark bit */

struct Lisp_Symbol
  {
    struct Lisp_String *name;
    Lisp_Object value;
    Lisp_Object function;
    Lisp_Object plist;
    struct Lisp_Symbol *next;	/* -> next symbol in this obarray bucket */
  };
/* next symbol in this obarray bucket */
#define symbol_next(s) ((s)->next)

struct Lisp_Subr
  {
    Lisp_Object (*function) ();
    short min_args, max_args;
    char *symbol_name;
    char *prompt;
    char *doc;
  };
#define subr_function(subr) (subr)->function
#define subr_name(subr) (subr)->symbol_name

/* In a marker, the markbit of the chain field is used as the gc mark bit */

struct Lisp_Marker
  {
    struct buffer *buffer;
    Lisp_Object chain;
    int bufpos;
#if 0
    int modified;
#endif
  };

#ifdef LISP_FLOAT_TYPE
struct Lisp_Float
  {
    Lisp_Object type;		/* essentially used for mark-bit 
				   and chaining when on free-list */
    double data;  
  };
#endif /* LISP_FLOAT_TYPE */

/* These structures are defined elsewhere, but assert that they name global
   structures so that we can use pointers to them in prototypes. */
struct Lisp_Event;
struct Lisp_Process;


/* A character, declared with the following typedef, is a member
   of some character set associated with the current buffer. */
typedef unsigned char UCHAR;

/* A UCHAR is displayed on a given terminal by means of a
   sequence of one or more GLYPHs.
   A GLYPH is something that takes
   up exactly one display position on the screen.  */
typedef unsigned short GLYPH;

/* Meanings of slots in a Lisp_Compiled:  */

#define COMPILED_ARGLIST 0
#define COMPILED_BYTECODE 1
#define COMPILED_CONSTANTS 2
#define COMPILED_STACK_DEPTH 3
#define COMPILED_DOC_STRING 4
#define COMPILED_INTERACTIVE 5

/* Data type checking */

#define NILP(x)  (XFASTINT (x) == XFASTINT (Qnil))

#define CONSP(x) (XTYPE ((x)) == Lisp_Cons)
#define SYMBOLP(x) (XTYPE ((x)) == Lisp_Symbol)
#define FIXNUMP(x) (XTYPE ((x)) == Lisp_Int)
#define MARKERP(x) (XTYPE ((x)) == Lisp_Marker)
#define STRINGP(x) (XTYPE ((x)) == Lisp_String)
#define VECTORP(x) (XTYPE ((x)) == Lisp_Vector)
#define SUBRP(x) (XTYPE ((x)) == Lisp_Subr)
#define PROCESSP(x) (XTYPE ((x)) == Lisp_Process)
#define BUFFERP(x) (XTYPE ((x)) == Lisp_Buffer)
#define WINDOWP(x) (XTYPE ((x)) == Lisp_Window)
#define SCREENP(x) (XTYPE ((x)) == Lisp_Screen)
#define KEYMAPP(x) (XTYPE ((x)) == Lisp_Keymap)
#define COMPILEDP(x) (XTYPE ((x)) == Lisp_Compiled)
#define EVENTP(x) (XTYPE ((x)) == Lisp_Event)
#define EXTENTP(x) (XTYPE ((x)) == Lisp_Extent)
#define EXTENT_REPLICA_P(x) (XTYPE ((x)) == Lisp_Extent_Replica)

#ifdef LISP_FLOAT_TYPE
#define FLOATP(x) (XTYPE ((x)) == Lisp_Float)
#define NUMBERP(x) (FIXNUMP (x) || FLOATP (x))
#else
#define FLOATP(x) --- error! no float support. ---
#define NUMBERP(x) (FIXNUMP (x))
#endif

#define EQ(x, y) (XFASTINT (x) == XFASTINT (y))

#define CHECK_LIST(x, i) \
  { if ((!CONSP ((x))) && !NILP (x)) x = wrong_type_argument (Qlistp, (x)); }

#define CHECK_STRING(x, i) \
  { if (!STRINGP ((x))) x = wrong_type_argument (Qstringp, (x)); }

#define CHECK_CONS(x, i) \
  { if (!CONSP ((x))) x = wrong_type_argument (Qconsp, (x)); }

#define CHECK_SYMBOL(x, i) \
  { if (!SYMBOLP ((x))) x = wrong_type_argument (Qsymbolp, (x)); }

#define CHECK_VECTOR(x, i) \
  { if (!VECTORP ((x))) x = wrong_type_argument (Qvectorp, (x)); }

#define CHECK_BUFFER(x, i) \
  { if (!BUFFERP ((x))) x = wrong_type_argument (Qbufferp, (x)); }

#define CHECK_WINDOW(x, i) \
  { if (!WINDOWP ((x))) x = wrong_type_argument (Qwindowp, (x)); }

#define CHECK_EXTENT(x, i) \
  { if (!EXTENTP ((x))) x = wrong_type_argument (Qextentp, (x)); }

#define CHECK_EXTENT_REPLICA(x, i) \
  { if (!EXTENT_REPLICA_P ((x))) \
      x = wrong_type_argument (Qextent_replica_p, (x)); }

#define CHECK_PROCESS(x, i) \
  { if (!PROCESSP ((x))) x = wrong_type_argument (Qprocessp, (x)); }

#define CHECK_EVENT(x, i) \
  { if (!EVENTP ((x))) x = wrong_type_argument (Qeventp, (x)); }

#define CHECK_KEYMAP(x, i) \
  { if (!KEYMAPP ((x))) x = wrong_type_argument (Qkeymapp, (x)); }

#define CHECK_FIXNUM(x, i) \
  { if (!FIXNUMP ((x))) x = wrong_type_argument (Qintegerp, (x)); }

#define CHECK_NUMBER(x, i)  \
  { if (!NUMBERP (x)) x = wrong_type_argument (Qnumberp, (x)); }

#define CHECK_NATNUM(x, i) \
  { if (!FIXNUMP ((x)) || XINT ((x)) < 0) \
      x = wrong_type_argument (Qnatnump, (x)); }

#define CHECK_FIXNUM_COERCE_MARKER(x, i) \
  { if (MARKERP ((x))) XFASTINT (x) = marker_position (x); \
    else if (!FIXNUMP ((x))) \
      x = wrong_type_argument (Qinteger_or_marker_p, (x)); }

#define CHECK_NUMBER_COERCE_MARKER(x, i) \
  { if (MARKERP ((x))) XFASTINT (x) = marker_position (x); \
    else if (!NUMBERP ((x))) \
      x = wrong_type_argument (Qnumber_or_marker_p, (x)); }

/* The second check is looking for GCed markers still in use */
#define CHECK_MARKER(x, i) \
  { if (!MARKERP ((x))) x = wrong_type_argument (Qmarkerp, (x)); \
    if (FIXNUMP (XMARKER ((x))->chain)) abort (); }

#ifdef LISP_FLOAT_TYPE

#ifndef DBL_DIG
#define DBL_DIG 16
#endif

#define CHECK_FLOAT(x, i) \
{ if (!FLOATP (x)) x = wrong_type_argument (Qfloatp, (x)); }

#define XFLOATINT(n) extract_float((n))

#else  /* Not LISP_FLOAT_TYPE */

#define CHECK_FLOAT(x,i) --- error! no float support. ---

#define XFLOATINT(n) XINT((n))

#endif /* LISP_FLOAT_TYPE */

#ifdef VIRT_ADDR_VARIES

/* For machines like APOLLO where text and data can go anywhere
   in virtual memory.  */
#define CHECK_IMPURE(obj) \
  { extern int pure[]; \
    if ((PNTR_COMPARISON_TYPE) XPNTR (obj) < (PNTR_COMPARISON_TYPE) ((char *) pure + PURESIZE) \
	&& (PNTR_COMPARISON_TYPE) XPNTR (obj) >= (PNTR_COMPARISON_TYPE) pure) \
      pure_write_error (); }

#else /* not VIRT_ADDR_VARIES */
#ifdef PNTR_COMPARISON_TYPE

/* when PNTR_COMPARISON_TYPE is not the default (unsigned int) */
#define CHECK_IMPURE(obj) \
  { extern char my_edata; \
    if ((PNTR_COMPARISON_TYPE) XPNTR (obj) < (PNTR_COMPARISON_TYPE) &my_edata) \
      pure_write_error (); }

#else /* not VIRT_ADDRESS_VARIES, not PNTR_COMPARISON_TYPE */

#define CHECK_IMPURE(obj) \
  { extern char my_edata; \
    if (XPNTR (obj) < (unsigned int) &my_edata) \
      pure_write_error (); }

#endif /* PNTR_COMPARISON_TYPE */
#endif /* VIRT_ADDRESS_VARIES */

/* Cast pointers to this type to compare them.  Some machines want int.  */
#ifndef PNTR_COMPARISON_TYPE
#define PNTR_COMPARISON_TYPE unsigned int
#endif

/* Define a built-in function for calling from Lisp.
 `lname' should be the name to give the function in Lisp,
    as a null-terminated C string.
 `fnname' should be the name of the function in C.
    By convention, it starts with F.
 `sname' should be the name for the C constant structure
    that records information on this function for internal use.
    By convention, it should be the same as `fnname' but with S instead of F.
    It's too bad that C macros can't compute this from `fnname'.
 `minargs' should be a number, the minimum number of arguments allowed.
 `maxargs' should be a number, the maximum number of arguments allowed,
    or else MANY or UNEVALLED.
    MANY means pass a vector of evaluated arguments,
	 in the form of an integer number-of-arguments
	 followed by the address of a vector of Lisp_Objects
	 which contains the argument values.
    UNEVALLED means pass the list of unevaluated arguments
 `prompt' says how to read arguments for an interactive call.
    This can be zero or a C string.
    Zero means that interactive calls are not allowed.
    A string is interpreted in a hairy way:
     it should contain one line for each argument to be read, terminated by \n.
     The first character of the line controls the type of parsing:
       s  --  read a string.
       S  --  read a symbol.
       k  --  read a key sequence and return it as a string.
       a  --  read a function name (symbol) with completion.
       C  --  read a command name (symbol) with completion.
       v  --  read a variable name (symbol) with completion.
       b  --  read a buffer name (a string) with completion.
       B  --  buffer name, may be existing buffer or may not be.
       f  --  read a file name, file must exist.
       F  --  read a file name, file need not exist.
       n  --  read a number.
       c  --  read a character and return it as a number.
       p  --  use the numeric value of the prefix argument.
       P  --  use raw value of prefix - can be nil, -, (NUMBER) or NUMBER.
       x  --  read a Lisp object from the minibuffer.
       X  --  read a Lisp form from the minibuffer and use its value.
    A null string means call interactively with no arguments.
 `doc' is documentation for the user.
*/

#define DEFUN(lname, fnname, sname, minargs, maxargs, prompt, doc) \
  Lisp_Object fnname (); \
  struct Lisp_Subr sname = {fnname, minargs, maxargs, lname, prompt, 0}; \
  Lisp_Object fnname

/* defsubr (Sname);
 is how we define the symbol for function `name' at start-up time. */
extern void defsubr (struct Lisp_Subr *);

#define MANY -2
#define UNEVALLED -1
#define SUBR_MAX_ARGS 7

extern void defvar_lisp (const char *namestring,
                         Lisp_Object *address, const char *doc);
extern void defvar_lisp_nopro (const char *namestring,
                               Lisp_Object *address, const char *doc);
extern void defvar_bool (const char *namestring,
                         int *address, const char *doc);
extern void defvar_int (const char *namestring,
                        int *address, const char *doc);

/* Macros we use to define forwarded Lisp variables.
   These are used in the syms_of_FILENAME functions.  */

#define DEFVARLISP(lname, vname, doc) defvar_lisp (lname, vname, doc)
#define DEFVARBOOL(lname, vname, doc) defvar_bool (lname, vname, doc)
#define DEFVARINT(lname, vname, doc) defvar_int (lname, vname, doc)

#define DEFVAR_LISP(lname, vname, doc) defvar_lisp (lname, vname, doc)
#define DEFVAR_LISP_NOPRO(lname, vname, doc) \
 defvar_lisp_nopro (lname, vname, doc)
#define DEFVAR_BOOL(lname, vname, doc) defvar_bool (lname, vname, doc)
#define DEFVAR_INT(lname, vname, doc) defvar_int (lname, vname, doc)

/* Structure for recording Lisp call stack for backtrace purposes */

struct specbinding
  {
    Lisp_Object symbol, old_value;
    Lisp_Object (*func) ();
    Lisp_Object unused;		/* Dividing by 16 is faster than by 12 */
  };

extern struct specbinding *specpdl;
extern struct specbinding *specpdl_ptr;
extern int specpdl_depth;

struct handler
  {
    Lisp_Object handlers;
    Lisp_Object handler_arg;
    struct catchtag *tag;
    struct handler *next;
  };

extern struct handler *handlerlist;

/* Check quit-flag and quit if it is non-nil. */

#define QUIT \
  if (!NILP (Vquit_flag) && NILP (Vinhibit_quit)) \
    { Vquit_flag = Qnil; Fsignal (Qquit, Qnil); }

/* Nonzero if ought to quit now.  */

#define QUITP (!NILP (Vquit_flag) && NILP (Vinhibit_quit))

/* 1 if CH is upper case.  */

#define UPPERCASEP(CH) (XSTRING (current_buffer->downcase_table)->data[CH] != (CH))

/* 1 if CH is lower case.  */

#define LOWERCASEP(CH) \
  (!UPPERCASEP (CH) && XSTRING (current_buffer->upcase_table)->data[CH] != (CH))

/* 1 if CH is neither upper nor lower case.  */

#define NOCASEP(CH) (XSTRING (current_buffer->upcase_table)->data[CH] == (CH))

/* Upcase a character, or make no change if that cannot be done.  */

#define UPCASE(CH) (XSTRING (current_buffer->downcase_table)->data[CH] == (CH) \
		    ? UPCASE1 (CH) : (CH))

/* Upcase a character known to be not upper case.  */

#define UPCASE1(CH) (XSTRING (current_buffer->upcase_table)->data[CH])

/* Downcase a character, or make no change if that cannot be done. */

#define DOWNCASE(CH) (XSTRING (current_buffer->downcase_table)->data[CH])

/* Current buffer's map from characters to lower-case characters.  */

#define DOWNCASE_TABLE XSTRING (current_buffer->downcase_table)->data

/* Table mapping each char to the next char with the same lowercase version.
   This mapping is a no-op only for characters that don't have case.  */
#define UPCASE_TABLE XSTRING (current_buffer->upcase_table)->data

extern Lisp_Object Vascii_downcase_table, Vascii_upcase_table;

/* number of bytes of structure consed since last GC */

extern int consing_since_gc;

/* threshold for doing another gc */

extern int gc_cons_threshold;

/* Structure for recording stack slots that need marking */

/* This is a chain of structures, each of which points at a Lisp_Object variable
 whose value should be marked in garbage collection.
 Normally every link of the chain is an automatic variable of a function,
 and its `val' points to some argument or local variable of the function.
 On exit to the function, the chain is set back to the value it had on entry.
 This way, no link remains in the chain when the stack frame containing the link disappears.

 Every function that can call Feval must protect in this fashion all
 Lisp_Object variables whose contents will be used again. */

extern struct gcpro *gcprolist;

struct gcpro
  {
    struct gcpro *next;
    Lisp_Object *var;		/* Address of first protected variable */
    int nvars;			/* Number of consecutive protected variables */
  };

#ifdef DEBUG_GCPRO

extern void debug_gcpro1(), debug_gcpro2(), debug_gcpro3(), debug_gcpro4(),
  debug_ungcpro();

#define GCPRO1(v) \
 debug_gcpro1 (__FILE__, __LINE__,&gcpro1,&v)
#define GCPRO2(v1,v2) \
 debug_gcpro2 (__FILE__, __LINE__,&gcpro1,&gcpro2,&v1,&v2)
#define GCPRO3(v1,v2,v3) \
 debug_gcpro3 (__FILE__, __LINE__,&gcpro1,&gcpro2,&gcpro3,&v1,&v2,&v3)
#define GCPRO4(v1,v2,v3,v4) \
 debug_gcpro4 (__FILE__, __LINE__,&gcpro1,&gcpro2,&gcpro3,&gcpro4,\
	       &v1,&v2,&v3,&v4)
#define UNGCPRO \
 debug_ungcpro(__FILE__, __LINE__,&gcpro1)

#else /* ! DEBUG_GCPRO */

#define GCPRO1(varname) \
 {gcpro1.next = gcprolist; gcpro1.var = &varname; gcpro1.nvars = 1; \
  gcprolist = &gcpro1; }

#define GCPRO2(varname1, varname2) \
 {gcpro1.next = gcprolist; gcpro1.var = &varname1; gcpro1.nvars = 1; \
  gcpro2.next = &gcpro1; gcpro2.var = &varname2; gcpro2.nvars = 1; \
  gcprolist = &gcpro2; }

#define GCPRO3(varname1, varname2, varname3) \
 {gcpro1.next = gcprolist; gcpro1.var = &varname1; gcpro1.nvars = 1; \
  gcpro2.next = &gcpro1; gcpro2.var = &varname2; gcpro2.nvars = 1; \
  gcpro3.next = &gcpro2; gcpro3.var = &varname3; gcpro3.nvars = 1; \
  gcprolist = &gcpro3; }

#define GCPRO4(varname1, varname2, varname3, varname4) \
 {gcpro1.next = gcprolist; gcpro1.var = &varname1; gcpro1.nvars = 1; \
  gcpro2.next = &gcpro1; gcpro2.var = &varname2; gcpro2.nvars = 1; \
  gcpro3.next = &gcpro2; gcpro3.var = &varname3; gcpro3.nvars = 1; \
  gcpro4.next = &gcpro3; gcpro4.var = &varname4; gcpro4.nvars = 1; \
  gcprolist = &gcpro4; }

#define UNGCPRO (gcprolist = gcpro1.next)

#endif /* ! DEBUG_GCPRO */

/* Evaluate expr, UNGCPRO, and then return the value of expr.  */
#define RETURN_UNGCPRO(expr)		 \
  do					 \
    {					 \
      Lisp_Object ret_ungc_val = (expr); \
      UNGCPRO;				 \
      return ret_ungc_val;		 \
    }					 \
  while (0)

/* Call staticpro (&var) to protect static variable `var'. */

extern void staticpro (Lisp_Object *);

/* Nonzero means Emacs has already been initialized.
   Used during startup to detect startup of dumped Emacs.  */
extern int initialized;

extern int immediate_quit;	    /* Nonzero means ^G can quit instantly */


/* Defined in eval.c */
extern Lisp_Object Qautoload, Qexit, Qinteractive, Qcommandp, Qdefun, Qmacro;
extern Lisp_Object Vinhibit_quit, Vquit_flag, Qinhibit_quit;
extern Lisp_Object Vmocklisp_arguments, Qmocklisp, Qmocklisp_arguments;
extern Lisp_Object Vautoload_queue;
extern Lisp_Object Vrun_hooks;
extern Lisp_Object Fuser_variable_p (Lisp_Object);
extern Lisp_Object Finteractive_p (void);
extern void signal_error (Lisp_Object sig, Lisp_Object data);
extern Lisp_Object Fprogn (Lisp_Object args);
extern Lisp_Object Fcommandp (Lisp_Object obj);
extern Lisp_Object Feval (Lisp_Object form);
extern Lisp_Object Fapply (int nargs, Lisp_Object *args);
extern Lisp_Object Ffuncall (int nargs, Lisp_Object *args);
extern Lisp_Object Fbacktrace (Lisp_Object stream);
extern Lisp_Object apply1 (Lisp_Object fn, Lisp_Object args);
extern Lisp_Object call0 (Lisp_Object fn);
extern Lisp_Object call1 (Lisp_Object fn, Lisp_Object arg0);
extern Lisp_Object call2 (Lisp_Object fn, Lisp_Object a0, Lisp_Object a1);
extern Lisp_Object call3 (Lisp_Object fn,
                          Lisp_Object a0, Lisp_Object a1, Lisp_Object a2);
extern Lisp_Object call4 (Lisp_Object fn,
                          Lisp_Object a0, Lisp_Object a1, Lisp_Object a2,
                          Lisp_Object a3);
extern Lisp_Object call5 (Lisp_Object fn,
                          Lisp_Object a0, Lisp_Object arg1, Lisp_Object a2,
                          Lisp_Object a3, Lisp_Object a4);
extern Lisp_Object Fsignal (Lisp_Object signame, Lisp_Object data);
/* C Code should be using internal_catch, record_unwind_p, condition_case_1 */
/* extern Lisp_Object Fcatch (Lisp_Object args); */
/* extern Lisp_Object Funwind_protect (Lisp_Object args); */
/* extern Lisp_Object Fcondition_case (Lisp_Object args); */
extern Lisp_Object Fthrow (Lisp_Object tag, Lisp_Object val);
extern Lisp_Object internal_catch (Lisp_Object tag, 
                                   Lisp_Object (*func) (Lisp_Object arg),
                                   Lisp_Object arg);
extern Lisp_Object condition_case_1 (Lisp_Object handlers,
                                     Lisp_Object (*bfun) (Lisp_Object barg),
                                     Lisp_Object barg,
                                     Lisp_Object (*hfun) (Lisp_Object val,
                                                          Lisp_Object harg),
                                     Lisp_Object harg);
extern Lisp_Object Fcondition_case_3 (Lisp_Object bodyform, 
                                      Lisp_Object var, 
                                      Lisp_Object handlers);
extern Lisp_Object unbind_to (int n, Lisp_Object val);
extern void specbind (Lisp_Object symbol, Lisp_Object value);
extern void record_unwind_protect (Lisp_Object (*function) (Lisp_Object),
                                   Lisp_Object arg);
extern void do_autoload (Lisp_Object fundef, Lisp_Object funname);
extern void error ();

#include "emacsfns.h"


#ifdef MAINTAIN_ENVIRONMENT
extern char *egetenv (const char *);
extern char *getenv (const char *);
#else
#define egetenv getenv
#endif
