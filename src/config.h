/* GNU Emacs site configuration template file.
   Copyright (C) 1986, 1988, 1992 Free Software Foundation, Inc.

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


/* Allow Emacses larger than 16 megabytes.  */

#ifndef VALBITS
#define VALBITS 26
#define GCTYPEBITS 5
#endif

/* Include here a s- file that describes the system type you are using.
   See the file ../etc/MACHINES for a list of systems and the names of
   the s- files to use for them.  See s-template.h for documentation on 
   writing s- files.
 */
#include "s/s-sunos4.h"

/* Include here a m- file that describes the machine and system you use.
   See the file ../etc/MACHINES for a list of machines and the names of 
   the m- files to use for them.   See m-template.h for info on what m- 
   files should define.
 */
#include "m/m-sparc.h"

/* Load in the conversion definitions if this system
   needs them and the source file being compiled has not
   said to inhibit this.  There should be no need for you
   to alter these lines.  */

#ifdef SHORTNAMES
#ifndef NO_SHORTNAMES
#include "../shortnames/remap.h"
#endif /* not NO_SHORTNAMES */
#endif /* SHORTNAMES */

/* Define HAVE_X_WINDOWS if you want to use the X window system.  */
#define HAVE_X_WINDOWS

#ifdef HAVE_X_WINDOWS
#ifndef MULTI_SCREEN
#define MULTI_SCREEN
#endif
#endif

/* Define `subprocesses' if you want to have code for asynchronous
   subprocesses (as used in M-x compile and M-x shell).  These do not
   work for some USG systems yet; for the ones where they work, the
   s-*.h file defines this flag.  */

#ifndef VMS
#ifndef USG
#define subprocesses
#endif
#endif

/* Define USER_FULL_NAME to return a string
   that is the user's full name.
   It can assume that the variable `pw'
   points to the password file entry for this user.

   At some sites, the pw_gecos field contains
   the user's full name.  If neither this nor any other
   field contains the right thing, use pw_name,
   giving the user's login name, since that is better than nothing.  */

#define USER_FULL_NAME pw->pw_gecos

/* Define AMPERSAND_FULL_NAME if you use the convention
   that & in the full name stands for the login id.  */

/* #define AMPERSAND_FULL_NAME */

/* Define HIGHPRI as a negative number if you want Emacs to run at a higher
   than normal priority.  For this to take effect, you must install it as
   setuid root. */

/* #define HIGHPRI */

/* support `getenv' and `setenv' in Emacs (unix only) */
#define MAINTAIN_ENVIRONMENT

/* Define LISP_FLOAT_TYPE if you want emacs to support floating-point
   numbers. */

#define LISP_FLOAT_TYPE

/* Define GNU_MALLOC if you want to use the *new* GNU memory allocator. */

#ifdef SYSTEM_MALLOC
#undef SYSTEM_MALLOC
#endif
#define GNU_MALLOC

/* Define REL_ALLOC if you want to use the relocating allocator for
   buffer space.  (There are too many problems with this right now.) */

/* #define REL_ALLOC */

/* Non-Lucid sites can't compile with Energize support yet */
/* #define ENERGIZE */

/* You should define LWLIB_HAS_EXTENSIONS if and only if the lwlib Imakefile
   had INCLUDE_EXTENSIONS defined.  This makes it possible to link arbitrary
   other widgets into the Emacs frame; Energize requires this.  The only
   reason you would not want this defined is if you couldn't get lwlib to
   compile this way because you don't have read-access to the X source tree.
   See also LWLIB_USES_MOTIF and LWLIB_USES_OLIT, below.
 */
#ifdef ENERGIZE
#define LWLIB_HAS_EXTENSIONS
#endif


/* These are magic */
/* #define EMACS_BTL */
/* #define FREE_CHECKING */

/* Until we figure out a better way, this is so you can run an X-based Emacs
   from a menu entry or from a shell buffer. */
#define INVISIBLE_TERMINAL_KLUDGE

/* Sound support */
#ifdef sparc
#define USE_SPARC_SOUND
#endif

/* Compile in support for running emacs directly from temacs (useful for
   debugging emacs) */
#define RUNNABLE_TEMACS

/* Support for truenames */
#define HAVE_REALPATH

#ifdef THIS_IS_YMAKEFILE

/* Define USE_GCC to compile with GCC.
   Define USE_LCC to compile with Lucid C.
   Otherwise, "cc" will be used.
   You -must- use an ANSI C compiler.
 */
#define USE_GCC
/* #define USE_LCC */

/* If you have defined USE_MOTIF in the lwlib Imakefile, then you must define
   LWLIB_USES_MOTIF here.  Similarly, if you have defined USE_OLIT in the
   lwlib Imakefile, you must define LWLIB_USES_OLIT here.  This is because
   emacs must be linked with the Motif or OpenLook libraries if lwlib has
   been configured to use them.  (Note: you cannot define both.)
 */
/* #define LWLIB_USES_MOTIF */
/* #define LWLIB_USES_OLIT */


/* Define LD_SWITCH_SITE to contain any special flags your loader may
   need.  For instance, if you've defined HAVE_X_WINDOWS above and your
   X libraries aren't in a place that your loader can find on its own,
   you might want to add "-L/..." or something similar.  */

/* #define LD_SWITCH_SITE -L/x11r4/usr.`arch`/lib */

/* Define C_SWITCH_SITE to contain any special flags your compiler may
   need.  For instance, if you've defined HAVE_X_WINDOWS above and your
   X include files aren't in a place that your compiler can find on its
   own, you might want to add "-I/..." or something similar.  */

/* #define C_SWITCH_SITE -I/cadillacgnu/gcc-include -I/x11r4/usr/include */

#ifdef USE_GCC
/* Depending on how GCC is installed, you may need to add the gcc library
   here.  This could also go in LD_SWITCH_SITE.  */

/* #define LIB_GCC /cadillacgnu/lib/sun4/gcc-gnulib */

#endif /* USE_GCC */


/* If you are using SunOS 4.1 and X11r5, then you need this patch.
   There is a stupid bug in the SunOS libc.a: two functions which X11r5
   uses, mbstowcs() and wcstombs(), are unusable when programs are
   statically linked (as Emacs must be) because the static version of
   libc.a contains the *dynamic* versions of these functions.  These
   functions don't seem to be called when Emacs is running, so it's 
   enough to define stubs for them.
 */
#ifdef sparc
#define OBJECTS_SYSTEM sunOS-fix.o
#endif


#endif /* THIS_IS_YMAKEFILE */
