/* GNU Emacs site configuration template file.
   Copyright (C) 1986, 1991, 1992, 1993, 1994 Free Software Foundation, Inc.

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

#ifndef _CONFIG_H_
#define _CONFIG_H_

#ifndef HAVE_CONFIG_H
# define HAVE_CONFIG_H
#endif

#define LOWTAGS


/* Define USE_GCC to compile with GCC.
   Define USE_LCC to compile with Lucid C.
   Otherwise, "cc" will be used.
   You -must- use an ANSI C compiler.
   This has to come before include the m- file.
 */

#define USE_GCC
/* #define USE_LCC */

/* Include here a s- file that describes the system type you are using.
   See the file ../etc/MACHINES for a list of systems and the names of
   the s- files to use for them.  See s-template.h for documentation on 
   writing s- files.
 */
#include "s/sunos4-1.h"

/* Include here a m- file that describes the machine and system you use.
   See the file ../etc/MACHINES for a list of machines and the names of 
   the m- files to use for them.   See m-template.h for info on what m- 
   files should define.
 */
#include "m/sparc.h"


/* The ANSI C `const' declaration is a portability problem, because many 
   vendors are inconsistent in their use of it in the system header files.
   This flag causes emacs to not use `const' internally.  You can comment
   out this line to re-enable it (a good idea if you are developing new 
   emacs code) but in general doing so doesn't buy you much, and could 
   cause unnecessary compilation errors.
*/
#define CONST_IS_LOSING

/* Define HAVE_X_WINDOWS if you want to use the X window system.  */
#define HAVE_X_WINDOWS

#ifdef HAVE_X_WINDOWS
#ifndef MULTI_SCREEN
#define MULTI_SCREEN
#endif
#endif

/* Define HAVE_XPM if you have the `xpm' library and want emacs to use it. */
#define HAVE_XPM

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

/* Define LISP_FLOAT_TYPE if you want emacs to support floating-point numbers.
 */
#define LISP_FLOAT_TYPE

/* The SunOS 4.1 version of localtime() allocates 8 bytes and writes to 9,
   which causes predictably bad problems unless you use the SunOS version
   of malloc, which doesn't mind.  This turns on a workaround in gmalloc.c.
   This is allegedly fixed by Sun patch 100267-04.  Its official designation
   is "1038500: localtime or tzsetwall corrupts malloc space."
 */
#if !defined(SYSTEM_MALLOC) && defined(sparc) && !defined(__svr4__)
# define SUNOS_LOCALTIME_BUG
#endif

/* Define NEED_STRDUP if your system doesn't have a strdup() function.
   Define NEED_REALPATH if your system does not include a realpath() function.
   These flags really should be in the appropriate s- file, so if you need 
   to do this, let us know and we'll put them there in the next release.
 */
/* #define NEED_STRDUP */
/* #define NEED_REALPATH */

/* Define ENERGIZE to compile with support for the Energize Programming System.
   If you do this, don't forget to define ENERGIZE in lwlib/Imakefile as well.
   You will need to set your C_SWITCH_SITE and LD_SWITCH_SITE to point at the
   Energize connection library (libconn.a) and associated header files.
 */
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

/* Sun SparStations, SGI machines, and HP9000s700s have support for playing
   different sound files as beeps.  If you are on a SparcStation but do not 
   have the sound option installed for some reason, then undefine USE_SOUND.
   (It's usually found in /usr/demo/SOUND/ on SunOS 4 and Solaris systems; 
   on Solaris, you may need to install the "SUNWaudmo" package.)
 */
/* #undef USE_SOUND */


/* Compile in support for running emacs directly from temacs (useful for
   debugging emacs) */
#define RUNNABLE_TEMACS

/* Define TOOLTALK if your site supports the ToolTalk library.
   WARNING, this code is under construction.
 */
/* #define TOOLTALK */

/* Define SPARCWORKS if your site supports SPARCworks 3.0
   which contains ToolTalk hooks for communication with the editor.
   WARNING, this code is under construction.
 */
#if defined(sparc) && defined(USG) && defined(TOOLTALK)
# define SPARCWORKS
#endif


/* Define this if you want level 2 internationalization compliance
   (localized collation and formatting).  Generally this should be
   defined, unless your system doesn't have the strcoll() and 
   setlocale() library routines.  This really should be defined in
   the appropriate s/ or m/ file.
 */
/* #define I18N2 */

/* Define this if you want level 3 internationalization compliance
   (localized messaging).  This will cause a small runtime performance
   penalty, as the strings are read from the message catalog(s).
   For this you need the gettext() and dgetext() library routines.
   WARNING, this code is under construction.
 */
/* #define I18N3 */

/* Define this if you want level 4 internationalization compliance
   (multi-byte character support).  This has a large cost in terms of
   runtime speed and memory usage, as the internal representation of
   characters will be 32 bits instead of 8.
   WARNING, this code is under construction.
 */
/* #define I18N4 */


/* If you have defined USE_MOTIF in the lwlib Imakefile, then you must define
   LWLIB_USES_MOTIF here.  Similarly, if you have defined USE_OLIT in the
   lwlib Imakefile, you must define LWLIB_USES_OLIT here.  This is because
   emacs must be linked with the Motif or OpenLook libraries if lwlib has
   been configured to use them.  (Note: you cannot define both.)

   See the comments in lwlib/Imakefile for more info.
 */
#define LWLIB_USES_MOTIF
/* #define LWLIB_USES_OLIT */

/* Energize requires Motif. */
#if defined(ENERGIZE) && !defined(LWLIB_USES_MOTIF)
# define LWLIB_USES_MOTIF
# undef LWLIB_USES_OLIT
#endif


/* Define `subprocesses' if you want to have code for asynchronous
   subprocesses (as used in M-x compile and M-x shell).  These do not
   work for some USG systems yet; for the ones where they work, the
   s- file defines this flag.  */

#ifndef VMS
#ifndef USG
#define subprocesses
#endif
#endif

/* Define the return type of signal handlers if the s-xxx file
   did not already do so.  */
#ifndef SIGTYPE
#define SIGTYPE void
#define SIGRETURN return
#endif

/* Define REL_ALLOC if you want to use the relocating allocator for
   buffer space.
   This is desirable because it means that when you kill buffers, the space
   will be returned to the OS (the emacs process size will shrink) but 
   there are problems with this on some OSes and with some compilers. 
   This requires GNU malloc, that is, that SYSTEM_MALLOC not be defined.
 */
#ifndef NO_REL_ALLOC
# define REL_ALLOC
#endif

/* Some s- files may define SYSTEM_MALLOC, in which case make sure
   we don't use REL_ALLOC. */
#ifdef SYSTEM_MALLOC
# undef REL_ALLOC
#endif

#ifdef THIS_IS_YMAKEFILE

/* Define LD_SWITCH_SITE to contain any special flags your loader may
   need.  For instance, if you've defined HAVE_X_WINDOWS above and your
   X libraries aren't in a place that your loader can find on its own,
   you might want to add "-L/..." or something similar.  */

#define LD_SWITCH_SITE -L/local/lib/X11/lib

/* Define C_SWITCH_SITE to contain any special flags your compiler may
   need.  For instance, if you've defined HAVE_X_WINDOWS above and your
   X include files aren't in a place that your compiler can find on its
   own, you might want to add "-I/..." or something similar.  */

#define C_SWITCH_SITE -I/local/lib/X11/include

#ifdef USE_GCC
/* Depending on how GCC is installed, you may need to add the gcc library
   here.  This could also go in LD_SWITCH_SITE.  If you get errors about
   __fixunsdfsi or__main being undefined, you probably need to do this. */

#define LIB_GCC /local/lib/gcc/lib/libgcc.a

#endif /* USE_GCC */

#ifdef USE_LCC
# undef C_OPTIMIZE_SWITCH
# define C_OPTIMIZE_SWITCH -O4 -Oi -G
#endif

/* If you are using SunOS 4.1.1 and X11r5, then you need this patch.
   There is a stupid bug in the SunOS libc.a: two functions which X11r5
   uses, mbstowcs() and wcstombs(), are unusable when programs are
   statically linked (as Emacs must be) because the static version of
   libc.a contains the *dynamic* versions of these functions.  These
   functions don't seem to be called when Emacs is running, so it's 
   enough to define stubs for them.

   This appears to be fixed in SunOS 4.1.2.

   Also, SunOS 4.1.1 contains buggy versions of strcmp and strcpy that
   sometimes reference memory past the end of the string, which can segv.
   I don't know whether this is has been fixed as of 4.1.2 or 4.1.3.
 */
#if defined(sparc) && !defined(USG)
#define OBJECTS_SYSTEM sunOS-fix.o strcmp.o strcpy.o
#endif

#endif /* THIS_IS_YMAKEFILE */


#ifndef THIS_IS_YMAKEFILE
  /* To eliminate use of `const' in the emacs sources,
     do `#define CONST_IS_LOSING' */
# undef CONST
# ifdef CONST_IS_LOSING
#  define CONST
# else
#  define CONST const
# endif /* CONST */
#endif /* THIS_IS_YMAKEFILE */


#endif /* _CONFIG_H_ */
