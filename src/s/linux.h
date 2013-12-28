/* Definitions file for GNU Emacs running on Linus Torvald's Linux
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

/* derived from a config.h for Emacs 18.59 by Rick Sladkey
   by Chipsy Sperber, sperber@informatik.uni-tuebingen.de
   for Lemacs 19.9, gcc 2.5.7, libc 4.5.8, ld.so 1.3 */

#define USG				/* System III, System V, etc */

#define USG5

#define LINUX

#define SYSTEM_TYPE "linux"		/* all the best software is free */

#define FIRST_PTY_LETTER 'p'

#define HAVE_TERMIOS
#define HAVE_TIMEVAL
#define HAVE_SELECT
#define HAVE_PTYS
#define HAVE_SOCKETS

#define subprocesses

#define emacs_read sys_read
#define emacs_write sys_write
#define emacs_open sys_open
#define emacs_close sys_close

#define INTERRUPTIBLE_OPEN
#define INTERRUPTIBLE_CLOSE
#define INTERRUPTIBLE_IO

#define HAVE_TCATTR
#define HAVE_SETSID
#define HAVE_DUP2
#define HAVE_GETTIMEOFDAY
#define HAVE_RENAME
#define HAVE_RANDOM
#define HAVE_CLOSEDIR
#define HAVE_GETPAGESIZE
#define HAVE_VFORK
#define HAVE_SYS_SIGLIST
#define HAVE_SIGLIST
#define HAVE_GETWD
#define	POSIX_SIGNALS
#define I18N2

#define BSTRING
#define NO_SIOCTL_H
#define SYSV_SYSTEM_DIR
#define HAVE_SYS_TIME_H
#define USG_SYS_TIME
#define HAVE_TZNAME
#define SIGNALS_VIA_CHARACTERS	/* cannot do TIOCGPGRP on a pty master */

/* misc. kludges for linux */

#define MAXNAMLEN NAME_MAX	/* POSIX */

#define SIGSYS SIGSEGV		/* no such thing with linux */

#define VSWTCH VSWTC		/* mis-spelling in termios.h? */
#define CDEL '\0'		/* missing termio-ism */

#define C_COMPILER gcc
#define C_DEBUG_SWITCH -g
#define C_SWITCH_SYSTEM -O6
#define START_FILES pre-crt0.o /usr/lib/crt0.o
#define LIBS_DEBUG
#define LIBS_TERMCAP -ltermcap
#define LIBS_SYSTEM -lc
#define	LIB_STANDARD -lc
