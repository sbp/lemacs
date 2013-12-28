/* Fundamental definitions for GNU Emacs Lisp interpreter.
   Copyright (C) 1985, 1986, 1987, 1992, 1993, 1994
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

#ifndef _EMACS_LISP_H_
#define _EMACS_LISP_H_

#ifdef emacs	/* some things other than emacs want the structs */

#include <stdlib.h>
#include <unistd.h>
#include <string.h>            /* primarily for memcpy, etc */

#ifdef __lucid
# include <sysent.h>
#endif

/* Emacs needs to use its own definitions of certain system calls on some
   systems (like SunOS 4.1, where the read system call is interruptable but
   emacs expects it not to be.  This used to be done by having the appropriate
   "s" file do `#define read sys_read' and having there be a definition of the
   sys_read function in sysdep.c, but doing it this way can cause conflicts if
   the prototype of sys_read doesn't exactly match the prototype of read from
   the system header files, because read will be defined before the system
   header files are included.  So instead, the "s" files define `emacs_read'
   as `sys_read', and if `emacs_read' is not defined by the time lisp.h is
   included, then we define it to be simply `read'.  All calls to read, write,
   open, and close in the emacs source are really calls to the emacs_ version
   instead.

   Possibly the INTERRUPTIBLE_IO flag could be used for this instead?  Then the
   s and m files would only need to do one define (INTERRUPTIBLE_IO) instead of
   four or five (INTERRUPTIBLE_IO, emacs_read, emacs_write, emacs_open, and
   maybe emacs_close).

   Possibly we should do this with fwrite/emacs_fwrite as well, but it looks
   like VMS is the only system that needs to encapsulate fwrite.
 */
#ifndef emacs_read
#define emacs_read read
#endif
#ifndef emacs_write
#define emacs_write write
#endif
#ifndef emacs_open
#define emacs_open open
#endif
#ifndef emacs_close
#define emacs_close close
#endif

#endif /* emacs */

/* generally useful */
#define countof(x) (sizeof((x))/sizeof((x)[0]))

/* We assume an ANSI C compiler and libraries and memcpy, memset, memcmp */
/*  (This definition is here because system header file macros may want
 *   to call bzero (eg FD_ZERO) */
#ifndef bzero
#define bzero(m, l) memset ((m), 0, (l))
#endif

#ifndef DOESNT_RETURN
# ifdef __GNUC__
#  define DOESNT_RETURN void volatile /* eg extern DOESNT_RETURN abort (); */
# else
#  define DOESNT_RETURN void
# endif
#endif

#ifndef ALIGNOF
# if defined (__GNUC__) && (__GNUC__ >= 2)
#  define ALIGNOF(x) __alignof(x)
# else
#  define ALIGNOF(x) sizeof(x)
# endif
#endif

/* These values are overridden by the m- file on some machines.  */
#ifndef GCTYPEBITS
# define GCTYPEBITS 3L
#endif

#ifndef VALBITS
# define VALBITS ((LONGBITS)-((GCTYPEBITS)+1L))
#endif


/* There's not any particular reason not to use lrecords for these; some
   objects get slightly larger, but we get 3 bit tags instead of 4.
 */
#define LRECORD_SYMBOL


/* Define the fundamental Lisp data structures */

/* This is the set of Lisp data types */

enum Lisp_Type
  {
    /* Integer.  XINT(obj) is the integer value. */
    Lisp_Int                    /* 0  DTP-FIXNUM */

    /* XRECORD_LHEADER (object) points to a struct lrecord_header
       lheader->implementation determines the type (and GC behaviour)
       of the object. */
    ,Lisp_Record                /* 1  DTP-OTHER-POINTER */

    /* Cons.  XCONS (object) points to a struct Lisp_Cons. */
    ,Lisp_Cons                  /* 2  DTP-LIST */

/* LRECORD_STRING is NYI */
    /* String.  XSTRING (object) points to a struct Lisp_String.
       The length of the string, and its contents, are stored therein. */
    ,Lisp_String                /* 3  DTP-STRING */

#ifndef LRECORD_VECTOR
    /* Vector of Lisp objects.  XVECTOR(object) points to a struct Lisp_Vector.
       The length of the vector, and its contents, are stored therein. */
    ,Lisp_Vector                /* 4  DTP-SIMPLE-ARRAY */
#endif

#ifndef LRECORD_SYMBOL
    ,Lisp_Symbol
#endif /* !LRECORD_SYMBOL */
  };

#define POINTER_TYPE_P(type) ((type) != Lisp_Int)

/* This should be the underlying type intowhich a Lisp_Object must fit.
   In a strict ANSI world, this must be `int', since ANSI says you can't
   use bitfields on any type other than `int'.  However, on a machine
   where `int' and `long' are not the same size, this should be the
   longer of the two.  (This also must be something intowhich a pointer
   to an arbitrary object will fit, modulo any DATA_SEG_BITS cruft.)
 */
#if (LONGBITS > INTBITS)
# define LISP_WORD_TYPE long
#else
# define LISP_WORD_TYPE int
#endif

#ifdef NO_UNION_TYPE
# include "lisp-disunion.h"
#else /* !NO_UNION_TYPE */
# include "lisp-union.h"
#endif /* !NO_UNION_TYPE */


#define XCONS(a) ((struct Lisp_Cons *) XPNTR(a))
#define XBUFFER(a) ((struct buffer *) XPNTR(a))
#define XVECTOR(a) ((struct Lisp_Vector *) XPNTR(a))
#define XSUBR(a) ((struct Lisp_Subr *) XPNTR(a))
#define XSTRING(a) ((struct Lisp_String *) XPNTR(a))
#define XSYMBOL(a) ((struct Lisp_Symbol *) XPNTR(a))
#define XMARKER(a) ((struct Lisp_Marker *) XPNTR(a))
#ifdef LISP_FLOAT_TYPE
# define XFLOAT(a) ((struct Lisp_Float *) XPNTR(a))
#else
# define XFLOAT(a) --- error!  No float support. ---
#endif


#include "lrecord.h"

/* In a cons, the markbit of the car is the gc mark bit */

struct Lisp_Cons
  {
    Lisp_Object car, cdr;
  };

/* In a string or vector, the sign bit of the `size' is the gc mark bit */

struct Lisp_String
  {
    long size;
    unsigned char *data;
    Lisp_Object dup_list;
  };
#define string_length(s) ((s)->size)
#define string_dups(s) ((s)->dup_list)

struct Lisp_Vector
  {
#ifdef LRECORD_VECTOR
    struct lrecord_header lheader;
#endif
    long size;
    /* next is now chained through v->contents[size], terminated by Qzero.
     * This means that pure vectors don't need a "next" */
    /* struct Lisp_Vector *next; */
    Lisp_Object contents[1];
  };
#define vector_length(v) ((v)->size)
#define vector_next(v) ((v)->contents[(v)->size])
#ifndef LRECORD_VECTOR
# define XSETVECTOR(v,p) XSET ((v), Lisp_Vector, (p))
#else
# define XSETVECTOR(v,p) XSETR ((v), Lisp_Vector, (p))
#endif /* LRECORD_VECTOR */

/* In a symbol, the markbit of the plist is used as the gc mark bit */
struct Lisp_Symbol
  {
#ifdef LRECORD_SYMBOL
    struct lrecord_header lheader;
#endif
    /* next symbol in this obarray bucket */
    struct Lisp_Symbol *next;
    struct Lisp_String *name;
    Lisp_Object value;
    Lisp_Object function;
    Lisp_Object plist;
  };
#define symbol_next(s) ((s)->next)

#ifndef LRECORD_SYMBOL
# define XSETSYMBOL(s, p) XSET ((s), Lisp_Symbol, (p))
#else
# define XSETSYMBOL(s, p) XSETR ((s), Lisp_Symbol, (p))
#endif /* LRECORD_SYMBOL */



struct Lisp_Subr
  {
    struct lrecord_header lheader;
    short min_args, max_args;
    CONST char *prompt;
    CONST char *doc;
    CONST char *name;
    Lisp_Object (*subr_fn) ();
  };
#define subr_function(subr) (subr)->subr_fn
#define subr_name(subr) (subr)->name

struct Lisp_Marker
  {
    struct lrecord_header lheader;
    struct Lisp_Marker *next;
    struct buffer *buffer;
    long bufpos;
  };
#define marker_next(m) ((m)->next)

#ifdef LISP_FLOAT_TYPE
struct Lisp_Float
  {
    struct lrecord_header lheader;
    union { double d; struct Lisp_Float *next; } data;
# define float_next(f) ((f)->data.next)
# define float_data(f) ((f)->data.d)
# define XSETFLOAT(s, p) XSETR ((s), Lisp_Float, (p))
  };
#endif /* LISP_FLOAT_TYPE */

#ifdef emacs /* gdb doesn't like this */

/* A character, declared with the following typedef, is a member
   of some character set associated with the current buffer. */
typedef unsigned char UCHAR;

#ifdef I18N4
#include <stdlib.h>
#endif

/* A GLYPH is an index into the `glyph_to_pixmaps_table'. */
typedef unsigned short GLYPH;

#endif /* emacs */


/* Data type checking */

#define NILP(x)  (EQ ((x), Qnil))
#define CONSP(x) (XTYPE ((x)) == Lisp_Cons)
#define FIXNUMP(x) (XTYPE ((x)) == Lisp_Int)

#ifdef LRECORD_SYMBOL
# define SYMBOLP(x) (RECORD_TYPEP ((x), lrecord_symbol))
extern CONST struct lrecord_implementation lrecord_symbol[];
#else /* !LRECORD_SYMBOL */
# define SYMBOLP(x) (XTYPE ((x)) == Lisp_Symbol)
#endif /* !LRECORD_SYMBOL */

#ifdef LRECORD_STRING
# define STRINGP(x) (RECORD_TYPEP ((x), lrecord_string))
extern CONST struct lrecord_implementation lrecord_string[];
#else /* !LRECORD_STRING */
#define STRINGP(x) (XTYPE ((x)) == Lisp_String)
#endif /* !LRECORD_STRING */

#ifdef LRECORD_VECTOR
# define VECTORP(x) (RECORD_TYPEP ((x), lrecord_vector))
extern CONST struct lrecord_implementation lrecord_vector[];
#else /* !LRECORD_VECTOR */
# define VECTORP(x) (XTYPE ((x)) == Lisp_Vector)
#endif /* !LRECORD_VECTOR */

#define FLOATP(x) (RECORD_TYPEP ((x), lrecord_float))
extern CONST struct lrecord_implementation lrecord_float[];

#define COMPILEDP(x) (RECORD_TYPEP ((x), lrecord_bytecode))
extern CONST struct lrecord_implementation lrecord_bytecode[];

#define SUBRP(x) (RECORD_TYPEP((x), lrecord_subr))
extern CONST struct lrecord_implementation lrecord_subr[];
#define MARKERP(x) (RECORD_TYPEP((x), lrecord_marker))
extern CONST struct lrecord_implementation lrecord_marker[];
#ifdef LISP_FLOAT_TYPE
# define NUMBERP(x) (FIXNUMP (x) || FLOATP (x))
#else
# define NUMBERP(x) (FIXNUMP (x))
#endif

#ifdef emacs

#define CHECK_LIST(x, i) \
  do { if ((!CONSP ((x))) && !NILP (x)) x = wrong_type_argument (Qlistp, (x)); } while (0)

#define CHECK_STRING(x, i) \
  do { if (!STRINGP ((x))) x = wrong_type_argument (Qstringp, (x)); } while (0)

#define CHECK_CONS(x, i) \
  do { if (!CONSP ((x))) x = wrong_type_argument (Qconsp, (x)); } while (0)

#define CHECK_SYMBOL(x, i) \
  do { if (!SYMBOLP ((x))) x = wrong_type_argument (Qsymbolp, (x)); } while (0)

#define CHECK_VECTOR(x, i) \
  do { if (!VECTORP ((x))) x = wrong_type_argument (Qvectorp, (x)); } while(0)

#define CHECK_FIXNUM(x, i) \
  do { if (!FIXNUMP ((x))) x = wrong_type_argument (Qintegerp, (x)); } while(0)

#define CHECK_NATNUM(x, i) \
  do { if (!FIXNUMP ((x)) || XINT ((x)) < 0) \
      x = wrong_type_argument (Qnatnump, (x)); } while (0)

/* The second check was looking for GCed markers still in use */
#define CHECK_MARKER(x, i) \
  do { if (!MARKERP ((x))) x = wrong_type_argument (Qmarkerp, (x)); } while (0)
/* if (FIXNUMP (XMARKER ((x))->lheader.next.v)) abort (); */


#define CHECK_FIXNUM_COERCE_MARKER(x, i) \
  do { if (FIXNUMP ((x))) \
         ; \
       else if (MARKERP ((x))) \
         (x) = make_number (marker_position ((x))); \
       else \
         (x) = wrong_type_argument (Qinteger_or_marker_p, ((x))); } while (0)

#define CHECK_SUBR(x, i) \
 do { if (!SUBRP ((x))) (x) = wrong_type_argument (Qsubrp, ((x))); } while (0)

#ifdef LISP_FLOAT_TYPE

#ifndef DBL_DIG
# define DBL_DIG 16
#endif

#define XFLOATINT(n) extract_float((n))

#define CHECK_FLOAT(x, i) \
  do { if (!FLOATP (x)) (x) = wrong_type_argument (Qfloatp, ((x))); } while (0)

#define CHECK_NUMBER(x, i)  \
  do { if ( !FIXNUMP ((x)) && !FLOATP ((x))) \
       (x) = wrong_type_argument (Qnumberp, ((x))); } while (0)

#define CHECK_NUMBER_COERCE_MARKER(x, i) \
  do { if (FIXNUMP ((x)) || FLOATP ((x))) \
         ; \
       else if (MARKERP ((x))) \
         (x) = make_number (marker_position ((x))); \
       else  \
         (x) = wrong_type_argument (Qnumber_or_marker_p, ((x))); } while (0)

#else  /* not LISP_FLOAT_TYPE */

#define CHECK_NUMBER CHECK_FIXNUM

#define CHECK_NUMBER_COERCE_MARKER CHECK_FIXNUM_COERCE_MARKER

#define XFLOATINT(n) XINT((n))

#define CHECK_FLOAT(x,i) --- error! no float support. ---

#endif /* LISP_FLOAT_TYPE */



#define CHECK_IMPURE(obj) \
  do { if (purified (obj)) pure_write_error (); } while (0)

/* Cast pointers to this type to compare them.  Some machines want int.  */
#ifndef PNTR_COMPARISON_TYPE
# define PNTR_COMPARISON_TYPE unsigned int
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

#define SUBR_MAX_ARGS 7
#define MANY -2
#define UNEVALLED -1

/* Can't be const, because then subr->doc is read-only and
 *  FSnarf_documentation chokes */
#define DEFUN(lname, fnname, sname, minargs, maxargs, prompt, doc) \
  Lisp_Object fnname DEFUN_ARGS_ ## maxargs ; /* See below */ \
  static struct Lisp_Subr sname \
     = { { lrecord_subr }, minargs, maxargs, prompt, 0, lname, fnname }; \
  Lisp_Object fnname

/* Scary ANSI C preprocessor hackery by Felix Lee <flee@guardian.cse.psu.edu>
   to get DEFUN to declare a prototype that matches maxargs, so that the
   compiler can complain if the "real" arglist doesn't match.  Clever hack
   or repulsive kludge?  You be the judge.
 */
#define DEFUN_ARGS_UNEVALLED (Lisp_Object)
#define DEFUN_ARGS_MANY (int, Lisp_Object *)
#define DEFUN_ARGS_0 (void)
#define DEFUN_ARGS_1 (Lisp_Object)
#define DEFUN_ARGS_2 (Lisp_Object, Lisp_Object)
#define DEFUN_ARGS_3 (Lisp_Object, Lisp_Object, Lisp_Object)
#define DEFUN_ARGS_4 (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object)
#define DEFUN_ARGS_5 (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, \
		      Lisp_Object)
#define DEFUN_ARGS_6 (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, \
		      Lisp_Object, Lisp_Object)
#define DEFUN_ARGS_7 (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, \
		      Lisp_Object, Lisp_Object, Lisp_Object)

/* defsubr (Sname);
 is how we define the symbol for function `name' at start-up time. */
extern void defsubr (struct Lisp_Subr *);

extern void defsymbol (Lisp_Object *location, CONST char *name);

struct symbol_value_forward;

/* Macros we use to define forwarded Lisp variables.
   These are used in the syms_of_FILENAME functions.  */

/* Sigh, need to include this for const structure initialisations below.
 *   C sucks, yet again */
#include "symeval.h"

extern void defvar_mumble (CONST char *names,
                           CONST void *magic, int sizeof_magic);

#define DEFVARLISP(lname, c_location, doc) \
 do { static CONST struct symbol_value_forward I_hate_C \
       = { { { { lrecord_symbol_value_forward }, (void *) (c_location) }, \
             object_forward } }; \
      defvar_mumble ((lname), &I_hate_C, sizeof (I_hate_C)); \
      staticpro ((c_location)); \
 } while (0)
#define DEFVARINT(lname, c_location, doc) \
 do { static CONST struct symbol_value_forward I_hate_C \
       = { { { {lrecord_symbol_value_forward}, (void *) (c_location) }, \
             fixnum_forward } }; \
      defvar_mumble ((lname), (&I_hate_C), (sizeof (I_hate_C))); \
 } while (0)
#define DEFVARBOOL(lname, c_location, doc) \
 do { static CONST struct symbol_value_forward I_hate_C \
       = { { { {lrecord_symbol_value_forward}, (void *) (c_location) }, \
             boolean_forward } }; \
      defvar_mumble ((lname), &I_hate_C, sizeof (I_hate_C)); \
 } while (0)

/* These discard their DOC arg because it is snarfed by make-docfile
 *  and stored in an external file. */
#define DEFVAR_LISP(lname, c_location, doc) DEFVARLISP (lname, c_location, 0)
#define DEFVAR_BOOL(lname, c_location, doc) DEFVARBOOL (lname, c_location, 0)
#define DEFVAR_INT(lname, c_location, doc) DEFVARINT (lname, c_location, 0)


/* Depth of special binding/unwind-protect stack.  Use as arg to unbind_to */
extern int specpdl_depth (void);

extern int sigio_happened;
extern int check_sigio (void);

extern void signal_quit (void);

/* Nonzero if ought to quit now.  */
#define QUITP ((sigio_happened ? check_sigio() : 0), \
	       (!NILP (Vquit_flag) && NILP (Vinhibit_quit)))

/* Check quit-flag and quit if it is non-nil. */
#define QUIT \
  do { if (QUITP) signal_quit (); } while (0)


/* 1 if CH is upper case.  */
#ifdef I18N4
#include "iso-wide.h"
#endif

#ifdef I18N4
#define UPPERCASEP(CH)						\
  (IN_TABLE_DOMAIN (CH)						\
   && XSTRING (current_buffer->downcase_table)->		\
	data[WIDE_TO_BYTE (CH)] != (WIDE_TO_BYTE (CH)))
#else
#define UPPERCASEP(CH) \
  (XSTRING (current_buffer->downcase_table)->data[CH] != (CH))
#endif

/* 1 if CH is lower case.  */

#ifdef I18N4
#define LOWERCASEP(CH)						\
  (!UPPERCASEP (CH) &&						\
   IN_TABLE_DOMAIN (CH)						\
       && XSTRING (current_buffer->upcase_table)->		\
	data[WIDE_TO_BYTE (CH)] != (WIDE_TO_BYTE (CH)))
#else
#define LOWERCASEP(CH) \
  (!UPPERCASEP (CH) \
   && XSTRING (current_buffer->upcase_table)->data[CH] != (CH))
#endif

/* 1 if CH is neither upper nor lower case.  */

#ifdef I18N4
#define NOCASEP(CH)						\
  (!IN_TABLE_DOMAIN (CH)					\
   || XSTRING (current_buffer->upcase_table)->			\
	data[WIDE_TO_BYTE (CH)] == (WIDE_TO_BYTE (CH)))
#else
#define NOCASEP(CH) (XSTRING (current_buffer->upcase_table)->data[CH] == (CH))
#endif

/* Upcase a character, or make no change if that cannot be done.  */

#ifdef I18N4
#define UPCASE(CH)						\
  ((IN_TABLE_DOMAIN (CH)					\
    && XSTRING (current_buffer->downcase_table)->		\
	data[WIDE_TO_BYTE (CH)] == (WIDE_TO_BYTE (CH)))		\
   ? UPCASE1 (CH) : (CH))
#else
#define UPCASE(CH) \
  (XSTRING (current_buffer->downcase_table)->data[CH] == (CH) \
   ? UPCASE1 (CH) : (CH))
#endif

/* Upcase a character known to be not upper case.  */

#ifdef I18N4
#define UPCASE1(CH)						\
  (IN_TABLE_DOMAIN (CH)						\
   ? BYTE_TO_WIDE (XSTRING (current_buffer->upcase_table)->	\
	data[WIDE_TO_BYTE (CH)])				\
   : (CH))
#else
#define UPCASE1(CH) (XSTRING (current_buffer->upcase_table)->data[CH])
#endif

/* Downcase a character, or make no change if that cannot be done. */

#ifdef I18N4
#define DOWNCASE(CH)						\
  (IN_TABLE_DOMAIN (CH)						\
   ? BYTE_TO_WIDE (XSTRING (current_buffer->downcase_table)->	\
	data[WIDE_TO_BYTE (CH)])				\
   : (CH))
#else
#define DOWNCASE(CH) (XSTRING (current_buffer->downcase_table)->data[CH])
#endif

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

/* This is a chain of structures, each of which points at a Lisp_Object
   variable whose value should be marked in garbage collection.
   Normally every link of the chain is an automatic variable of a function,
   and its `val' points to some argument or local variable of the function.
   On exit to the function, the chain is set back to the value it had on
   entry.  This way, no link remains in the chain when the stack frame
   containing the link disappears. 

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

/* Nonzero means ^G can quit instantly.
   Setting this means that the SIGIO handler can do a longjmp().
   You'd better be really, really sure you know this is safe.
 */
extern int immediate_quit;

#if 0 /* gcc 2.5.[34] apparently doesn't like this */
#ifdef __GNUC__
extern DOESNT_RETURN exit (int);
extern DOESNT_RETURN abort (void);
#endif
#endif

#ifdef USE_ASSERTIONS
/* Highly dubious kludge */
/*   (thanks, Jamie, I feel better now -- bpw) */
extern void assert_failed (char *, int, char *);
# define abort() (assert_failed (__FILE__, __LINE__, 0))
#endif

#include "emacsfns.h"

#endif /* emacs */

#endif /* _EMACS_LISP_H_ */
