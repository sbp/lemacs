/* Definitions file for GNU Emacs running on Solaris2.
   Copyright (C) 1992-1993 Free Software Foundation, Inc.

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

/* 
 * For Solaris2 by John Furlani, Sun Microsystems Computer Corporation 
 *            <john.furlani@East.Sun.COM> 
 */

/* Use basic SVR4 config for starters */
#include "s-usg5-4.h"

/* For Solaris2 specific stuff*/
#define SOLARIS2

/* Solaris2 is not BSD */
#undef BSD

/* Solaris2 is cool with subprocesses and TIOCGETC */
#define subprocesses
#undef	BROKEN_TIOCGETC

/* We've got sockets, so we'll use them */
#define HAVE_SOCKETS

/* vfork() is also provided for now, although it "will be eliminated
   in a future release" */
#define HAVE_VFORK

/* Solaris2 can use SIGPOLL as SIGIO  */
#define SIGIO SIGPOLL

#define HAVE_SYSCONF

/* Flags for OpenWindows */
#if 1
#undef LD_SWITCH_SYSTEM
#define LD_SWITCH_SYSTEM -L/usr/openwin/lib -R/usr/openwin/lib
#undef C_SWITCH_SYSTEM
#define C_SWITCH_SYSTEM -I/usr/openwin/include
#endif 

/* Don't need any of the start files */
#undef START_FILES
#define START_FILES

#undef LIB_STANDARD
#define LIB_STANDARD -lkvm -lsocket -lnsl -lelf -lintl -ldl

/* DATA_SEG_BITS not needed */
#undef DATA_SEG_BITS

/* Use cc to link */
#define LD_CMD cc

/* TIOCSIGSEND is ok for Solaris2 */
#undef TIOCSIGSEND

/* Xos.h doesn't define __TIMEVAL__ correctly */
#define __TIMEVAL__

#define FLOAT_CATCH_SIGILL
