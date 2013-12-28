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

#ifndef _EMACS_BYTECODE_H_
#define _EMACS_BYTECODE_H_

/* Meanings of slots in a Lisp_Compiled.
   This is kludgily used by `elt' and `concat' to pretend that bytecode
   objects are really sequences and should be eliminated eventually...
 */
#define COMPILED_ARGLIST 0
#define COMPILED_BYTECODE 1
#define COMPILED_CONSTANTS 2
#define COMPILED_STACK_DEPTH 3
#define COMPILED_DOC_STRING 4
#define COMPILED_INTERACTIVE 5
#define COMPILED_DOMAIN 6

struct Lisp_Bytecode
  {
    struct lrecord_header lheader;
    unsigned short maxdepth;
    struct 
      {
        unsigned int documentationp: 1;
        unsigned int interactivep: 1;
	/* Only used if I18N3, but always defined for simplicity. */
	unsigned int domainp: 1;
      } flags;
    Lisp_Object bytecodes;
    Lisp_Object constants;
    Lisp_Object arglist;
    /* This uses the minimal number of conses; see accessors in data.c. */
    Lisp_Object doc_and_interactive;
  };

extern Lisp_Object bytecode_documentation (struct Lisp_Bytecode *b);
extern Lisp_Object bytecode_interactive (struct Lisp_Bytecode *b);
extern Lisp_Object bytecode_domain (struct Lisp_Bytecode *b);
extern void set_bytecode_documentation (struct Lisp_Bytecode *b,
					Lisp_Object);

#define XBYTECODE(a) ((struct Lisp_Bytecode *) XPNTR(a))

#define CHECK_BYTECODE(x,i) \
  do { if (!COMPILEDP ((x))) x = wrong_type_argument (Qbytecodep, (x)); } \
  while (0)

/* total 1765 internal 101 doc-and-int 775 doc-only 389 int-only 42 neither 559
 no doc slot, no int slot
    overhead                        : (* 1765 0) =    0
    doc-and-int (args . (doc . int)): (*  775 4) = 3100
    doc-only    (args . doc)        : (*  389 2) =  778
    int-only    (args . int)        : (*   42 2) =   84
    neither     args                : (*  559 0) =    0 = 3962
 combined
    overhead                        : (* 1765 1) = 1765
    doc-and-int (doc . int)         : (*  775 2) = 1550 
    doc-only    doc                 : (*  389 0) =    0
    int-only    int                 : (*   42 0) =    0 
    neither     -                   : (*  559 0) =    0 = 3315
 both
    overhead                        : (* 1765 2) = 3530
    doc-and-int -                   : (*  775 0) =    0
    doc-only    -                   : (*  389 0) =    0
    int-only    -                   : (*   42 0) =    0 
    neither     -                   : (*  559 0)  =   0 = 3530
*/

#endif /* _EMACS_BYTECODE_H_ */

