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

#if (!!defined (BIG_ENDIAN) != !!defined (LOWTAGS))

/* Big-endian lowtags, little-endian hightags */
typedef
union Lisp_Object
  {
    struct
      {
	unsigned LISP_WORD_TYPE type_mark: GCTYPEBITS + 1;
	signed LISP_WORD_TYPE val: VALBITS;
      } s;
    struct
      {
#ifdef __GNUC__ /* Non-ANSI extension */
        enum Lisp_Type type: GCTYPEBITS;
#else
	unsigned LISP_WORD_TYPE type: GCTYPEBITS;
#endif /* __GNUC__ */
	/* The markbit is not really part of the value of a Lisp_Object,
	   and is always zero except during garbage collection.  */
	unsigned LISP_WORD_TYPE markbit: 1;
	unsigned LISP_WORD_TYPE val: VALBITS;
      } gu;
    void *v;
    const void *cv;             /* C wanks */
  }
Lisp_Object;

#else /* If BIG_ENDIAN, or little-endian hightags */

/* Big-endian hightags, big-endian lowtags */
typedef
union Lisp_Object
  {
    struct
      {
	signed LISP_WORD_TYPE val: VALBITS;
	unsigned LISP_WORD_TYPE mark_type: GCTYPEBITS + 1;
      } s;
    struct
      {
	unsigned LISP_WORD_TYPE val: VALBITS;
#ifdef __GNUC__ /* Non-ANSI extension */
        enum Lisp_Type type: GCTYPEBITS;
#else
	unsigned LISP_WORD_TYPE type: GCTYPEBITS;
#endif /* __GNUC__ */
	/* The markbit is not really part of the value of a Lisp_Object,
	   and is always zero except during garbage collection.  */
	unsigned LISP_WORD_TYPE markbit: 1;
      } gu;
    void *v;
    const void *cv;             /* C sucks */
  }
Lisp_Object;

#endif /* BIG/LITTLE_ENDIAN vs HIGH/LOWTAGS */


#ifndef XMAKE_LISP
#ifdef __GNUC__
/* Use GCC's struct initializers feature */
#define XMAKE_LISP(vartype,ptr) \
   ((union Lisp_Object) { gu: { markbit: 0, \
                                type: (vartype), \
                                val: ((unsigned LISP_WORD_TYPE) ptr) } })
#endif /* __GNUC__ */
#endif /* !XMAKE_LISP */


#ifdef XMAKE_LISP
#define Qzero (XMAKE_LISP (Lisp_Int, 0))
#define make_number(a) (XMAKE_LISP (Lisp_Int, (a)))
#else
extern Lisp_Object Qzero;
#endif


#define EQ(x,y) ((x).v == (y).v)

#define XTYPE(a) ((enum Lisp_Type) (a).gu.type)
#define XSETTYPE(a,b) ((a).gu.type = (b))
#define XGCTYPE(a) XTYPE((a))

/* >>>??? #ifdef EXPLICIT_SIGN_EXTEND ??? */
#define XINT(a) ((a).s.val)
/* The + 0 is to prevent XFASTINT being used on the LHS of an assignment */
#define XFASTINT(a) ((a).gu.val + 0)

#define XUINT(a) ((a).gu.val)
#define XPNTR(a) ((void *) ((a).gu.val)) /* >>> DATA_SEG_BITS, HAVE_SHM >>> */
#define XSETINT(a,b) do { ((a) = make_number (b)); } while (0)

#ifdef XMAKE_LISP
#define XSET(var,vartype,ptr) \
  do { ((var) = XMAKE_LISP ((vartype), (ptr))); } while (0)
#else
/* This is haired up to avoid evaluating var twice...
   This is necessary only in the "union" version.
   The "int" version has never done double evaluation.
 */
#define XSET(var,vartype,ptr)					\
   do {								\
	 Lisp_Object *tmp_xset_var = &((var));			\
	 (*tmp_xset_var).gu.markbit = 0;			\
	 (*tmp_xset_var).gu.type = ((vartype));			\
	 (*tmp_xset_var).s.val = ((LISP_WORD_TYPE) (ptr));	\
      } while (0)
#endif /* undefined XMAKE_LISP */

/* During garbage collection, XGCTYPE must be used for extracting types
 so that the mark bit is ignored.  XMARKBIT access the markbit.
 Markbits are used only in particular slots of particular structure types.
 Other markbits are always zero.
 Outside of garbage collection, all mark bits are always zero.  */


#define XMARKBIT(a) ((a).gu.markbit)
#define XMARK(a) do { XMARKBIT(a) = 1; } while (0)
#define XUNMARK(a) do { XMARKBIT(a) = 0; } while (0)

/* Use this for turning a (void *) into a Lisp_Object, as when the
  Lisp_Object is passed into a toolkit callback function */
#define VOID_TO_LISP(larg,varg) \
  do { ((larg).v = (void *) (varg)); } while (0)
#define CVOID_TO_LISP(larg,varg) \
  do { ((larg).cv = (const void *) (varg)); } while (0)

/* Use this for turning a Lisp_Object into a  (void *), as when the
  Lisp_Object is passed into a toolkit callback function */
#define LISP_TO_VOID(larg) ((larg).v)
#define LISP_TO_CVOID(larg) ((larg).cv)

