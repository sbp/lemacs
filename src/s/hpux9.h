/* Definitions file for GNU Emacs running on HP-UX 9.0
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
#undef HAVE_RANDOM                   /* not any more ... */

/*
 * HP-UX 9.0 comes with X11R5, so we must use the appropriate headers and
 * libraries.
 */

/* kludge no longer necessary (I assume, since it wasn't in 8.0) */
#undef static

#ifdef __GNUC__
#define HAVE_ALLOCA
#endif

/* HPUX 9 has TERMIOS and POSIX signals */
#undef  HAVE_TERMIO
#define HAVE_TERMIOS
#define POSIX_SIGNALS
#define I18N2

/* include file locations for HPUX 9 (X11R5) */
#ifdef __GNUC__
#define C_SWITCH_SYSTEM -I/usr/include/X11R5 -I/usr/include/Motif1.2
#else
/* #define C_SWITCH_SYSTEM -D_HPUX_SOURCE -I/usr/include/X11R5 -I/usr/include/Motif1.2 */
#define C_SWITCH_SYSTEM -Ae -I/usr/include/X11R5 -I/usr/include/Motif1.2
#endif

/* Don't use shared libraries.  unexec doesn't handle them.  */
#ifndef LD_SWITCH_SYSTEM
# ifdef __GNUC__ 
#   define LD_SWITCH_SYSTEM -Xlinker -a -Xlinker archive -L/usr/lib/X11R5 -L/usr/lib/Motif1.2
# else
#  define LD_SWITCH_SYSTEM -a archive -L/usr/lib/X11R5 -L/usr/lib/Motif1.2
# endif /* GNUC */
#endif /* LD_SWITCH_SYSTEM */
