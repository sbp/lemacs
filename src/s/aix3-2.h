/* Definitions file for GNU Emacs running on IBM AIX version 3.2
   Copyright (C) 1985, 1986, 1990, 1992, 1993, 1994
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

#include "aix3-1.h"

#ifndef __GNUC__
#define C_SWITCH_SYSTEM -ma
#endif

/* For X11R4 or MIT X11R5: */
#define LD_SWITCH_MACHINE -Wl,-bnso,-bnodelcsect,-bI:/lib/syscalls.exp

/* For X11R5 from AIX 3.2.4: */
/* #define LD_SWITCH_MACHINE -Wl,-bnso,-bnodelcsect,-bI:/lib/syscalls.exp,-bI:/usr/bin/X11/smt.exp */
  
/* For X11R5 from AIX 3.2.5: */
/* #define LD_SWITCH_MACHINE -Wl,-bnso,-bnodelcsect,-bI:/lib/syscalls.exp,-bI:/usr/lpp/X11/bin/smt.exp */

#define HAVE_ALLOCA
#define HAVE_CLOSEDIR
#define INTERRUPTIBLE_CLOSE
#define NEED_REALPATH
#define HAVE_SETPRIORITY

/* sperber@midi.informatik.uni-tuebingen.de says define these; 5-dec-93 */
#define HAVE_TIMEVAL
#define POSIX_SIGNALS
#define LIB_INTL

#define I18N2

#define TIME_WITH_SYS_TIME
#undef SYSTEM_MALLOC
#undef index
#undef rindex

/* jwz: not everybody has trouble with this, but those who do can't explain
   why yet, so this should probably be off by default. */
#define NO_REL_ALLOC

/* jwz: dkeller@vnet.IBM.COM says take this out;
   the -ma compiler option does the same thing */
/* #pragma alloca */

