/* Fundamental definitions for GNU Emacs Lisp interpreter.
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

/* If union type is not wanted, define Lisp_Object as just a number
   and define the macros below to extract fields by shifting */

#define Qzero 0

/* #define Lisp_Object int */
typedef LISP_WORD_TYPE Lisp_Object;

#ifndef VALMASK
# define VALMASK ((1L << (VALBITS)) - 1L)
#endif
#define GCTYPEMASK ((1L << (GCTYPEBITS)) - 1L)
#define MARKBIT (1L << ((VALBITS) + (GCTYPEBITS)))


/* These macros extract various sorts of values from a Lisp_Object.
 For example, if tem is a Lisp_Object whose type is Lisp_Cons,
 XCONS (tem) is the struct Lisp_Cons * pointing to the memory for that cons. */

/* One need to override this if there must be high bits set in data space
   (doing the result of the below & ((1 << (GCTYPE + 1)) - 1) would work
    on all machines, but would penalise machines which don't need it)
 */
#ifndef XTYPE
# define XTYPE(a) ((enum Lisp_Type) ((a) >> VALBITS))
#endif

#ifndef XSETTYPE
# define XSETTYPE(a,b) ((a)  =  XUINT (a) | ((LISP_WORD_TYPE)(b) << VALBITS))
#endif

#define EQ(x,y) ((x) == (y))

/* Use XFASTINT for fast retrieval and storage of integers known
  to be positive.  This takes advantage of the fact that Lisp_Int is 0.  */
#define XFASTINT(a) (a)

/* Extract the value of a Lisp_Object as a signed integer.  */

#ifndef XINT   /* Some machines need to do this differently.  */
# define XINT(a) (((a) << (LONGBITS-VALBITS)) >> (LONGBITS-VALBITS))
#endif

/* Extract the value as an unsigned integer.  This is a basis
   for extracting it as a pointer to a structure in storage.  */

#ifndef XUINT
# define XUINT(a) ((a) & VALMASK)
#endif

#ifndef XPNTR
# ifdef HAVE_SHM
/* In this representation, data is found in two widely separated segments.  */
extern int pure_size;
#  define XPNTR(a) \
  (XUINT (a) | (XUINT (a) > pure_size ? DATA_SEG_BITS : PURE_SEG_BITS))
# else /* not HAVE_SHM */
#  ifdef DATA_SEG_BITS
/* This case is used for the rt-pc.
   In the diffs I was given, it checked for ptr = 0
   and did not adjust it in that case.
   But I don't think that zero should ever be found
   in a Lisp object whose data type says it points to something.
 */
#   define XPNTR(a) (XUINT (a) | DATA_SEG_BITS)
#  else
#   define XPNTR(a) XUINT (a)
#  endif
# endif /* not HAVE_SHM */
#endif /* no XPNTR */

#ifndef XSETINT
# if 1 /* Back in the dark ages, this def "broke things" */
#  define XSETINT(a,b) do { XSET ((a), Lisp_Int, (b)); } while (0)
# else /* alternate def to work around some putative bug with the above */
#  define XSETINT(a,b) do { (a) = (((a) & ~VALMASK) | ((b) & VALMASK)); \
			  } while (0)
# endif
#endif /* !XSETINT */

#ifndef XSET
# define XSET(var,type,ptr) \
   do { (var) = (((LISP_WORD_TYPE) (type) << VALBITS) \
                 + ((LISP_WORD_TYPE) (ptr) & VALMASK)); \
      } while(0)
#endif

/* During garbage collection, XGCTYPE must be used for extracting types
 so that the mark bit is ignored.  XMARKBIT accesses the markbit.
 Markbits are used only in particular slots of particular structure types.
 Other markbits are always zero.
 Outside of garbage collection, all mark bits are always zero.  */

#ifndef XGCTYPE
# define XGCTYPE(a) ((enum Lisp_Type) (((a) >> VALBITS) & GCTYPEMASK))
#endif

#if ((VALBITS) + (GCTYPEBITS)) == ((LONGBITS) - 1L)
/* Make XMARKBIT faster if mark bit is sign bit.  */
# ifndef XMARKBIT
#  define XMARKBIT(a) ((a) < 0L)
# endif
#endif /* markbit is sign bit */

#ifndef XMARKBIT
# define XMARKBIT(a) ((a) & (MARKBIT))
#endif

#ifndef XMARK
# define XMARK(a) do { ((a) |= (MARKBIT)); } while (0)
#endif

#ifndef XUNMARK
# define XUNMARK(a) do { ((a) &= (~(MARKBIT))); } while (0)
#endif

/* Use this for turning a (void *) into a Lisp_Object, as when the
  Lisp_Object is passed into a toolkit callback function */
#define VOID_TO_LISP(larg,varg) \
  do { ((larg) = ((Lisp_Object) (varg))); } while (0)
#define CVOID_TO_LISP VOID_TO_LISP

/* Use this for turning a Lisp_Object into a  (void *), as when the
   Lisp_Object is passed into a toolkit callback function */
#define LISP_TO_VOID(larg) ((void *) (larg))
#define LISP_TO_CVOID(varg) ((const void *) (larg))
