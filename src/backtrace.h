/* The lisp stack.
   Copyright (C) 1985, 1986, 1987, 1992 Free Software Foundation, Inc.

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

#include <setjmp.h>
       
/* These definitions are used variously definition is used in alloc.c 
   and keyboard.c, and it is reported that putting some of then in
   lisp.h makes cc bomb out. */

struct backtrace
  {
    struct backtrace *next;
    Lisp_Object *function;
    Lisp_Object *args;		/* Points to vector of args. */
    int nargs;			/* Length of vector.
				   If nargs is UNEVALLED, args points to
				   slot holding list of unevalled args */
#ifdef EMACS_BTL
    /* The value of a Lisp integer that specifies the symbol being
       "invoked" by this node in the backtrace, or 0 if the backtrace
       doesn't correspond to a such an invocation */
    int id_number;
#endif
    char evalargs;
    /* Nonzero means call value of debugger when done with this operation. */
    char debug_on_exit;
  };


struct catchtag
  {
    Lisp_Object tag;
    Lisp_Object val;
    struct catchtag *next;
    struct gcpro *gcpro;
    jmp_buf jmp;
    struct backtrace *backlist;
    struct handler *handlerlist;
    int lisp_eval_depth;
    int pdlcount;
  };

extern struct catchtag *catchlist;
extern struct backtrace *backtrace_list;

