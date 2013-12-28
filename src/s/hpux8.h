/* Definitions file for GNU Emacs running on HP-UX 8.0 (only).
   Copyright (C) 1992, 1993 Free Software Foundation, Inc.
  
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

#include "hpux.h"

#undef HPUX_PRE_8_0

/*
 * Ugly, nasty kludge to prevent X11R4 Xos.h from redefining struct timeval
 * and struct timezone.
 */
#define __TIMEVAL__

/* kludge no longer necessary (at least on 9000/300 with hpux 8.0) */
#undef static

#ifdef __GNUC__
#define HAVE_ALLOCA
#endif

/* HPUX 8 has TERMIOS and POSIX signals */
#undef  HAVE_TERMIO
#define HAVE_TERMIOS
#define POSIX_SIGNALS

#define HAVE_WAIT_HEADER
#define WAITTYPE int
#define WRETCODE(X) WEXITSTATUS(X)

/* If you use X11R4 you must define this.  If you use
   X11R5 you must comment this out */
/* #define HAVE_RANDOM */

/* include file locations for HPUX 8 (X11R4) */
#ifdef __GNUC__
#define C_SWITCH_SYSTEM -I/usr/include/X11R4 -I/usr/include/Motif1.1
#else
#define C_SWITCH_SYSTEM -D_HPUX_SOURCE  -I/usr/include/X11R4 -I/usr/include/Motif1.1
#endif

#ifndef LD_SWITCH_SYSTEM
# ifdef __GNUC__ 
#  if defined(__HPUX_ASM__) || !defined(__hp9000s300)
#   define LD_SWITCH_SYSTEM -Xlinker -a -Xlinker archive -L/usr/lib/X11R4 -L/usr/lib/Motif1.1
#  else
#   define LD_SWITCH_SYSTEM -L/usr/lib/X11R4 -L/usr/lib/Motif1.1
#  endif /* HPUX_ASM */
# else
#  define LD_SWITCH_SYSTEM -a archive -L/usr/lib/X11R4 -L/usr/lib/Motif1.1
# endif /* GNUC */
#endif /* LD_SWITCH_SYSTEM */
