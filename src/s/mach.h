/* Definitions file for GNU Emacs running on Mach (BSD 4.3)
   Copyright (C) 1985, 1986 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* This has only been tested in the Carnegie Mellon University
 * Computer Science Department Mach environment.  If you use it
 * elsewhere, please let me (dkindred@cmu.edu) know whether it
 * works. */

#include "bsd4-3.h"

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#undef SYSTEM_TYPE
#define SYSTEM_TYPE "mach"

/* Define this macro if system defines a type `union wait'.  */

#define HAVE_UNION_WAIT

/* Don't send signals to subprocesses by "typing" special chars at them. */
#undef SIGNALS_VIA_CHARACTERS

/* We don't have a built-in realpath() function */
#define NEED_REALPATH

/* We have an rint() function (this should probably go in bsd4-3.h) */
#define HAVE_RINT

#ifndef THIS_IS_YMAKEFILE
typedef int pid_t;
#endif THIS_IS_YMAKEFILE
