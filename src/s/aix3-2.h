/* Definitions file for GNU Emacs running on IBM AIX version 3.2
   Copyright (C) 1985, 1986, 1990, 1992, 1993 Free Software Foundation, Inc.

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

#include "aix3-1.h"

#ifndef __GNUC__
#define C_SWITCH_SYSTEM -ma
#endif

/* For AIX 3.2.2 and 3.2.3: */
/* #define LD_SWITCH_MACHINE -Wl,-bnso,-bnodelcsect,-bI:/lib/syscalls.exp */

/* For AIX 3.2.4: */
#define LD_SWITCH_MACHINE -Wl,-bnso,-bnodelcsect,-bI:/lib/syscalls.exp,-bI:/usr/bin/X11/smt.exp

#define HAVE_ALLOCA
#define HAVE_CLOSEDIR
#define INTERRUPTIBLE_CLOSE
#define NEED_REALPATH
#define HAVE_SETPRIORITY
#undef HAVE_TIMEVAL
#undef SYSTEM_MALLOC
#undef index
#undef rindex

#pragma alloca
