/* Definitions file for GNU Emacs running on IBM AIX version 3.1
   Copyright (C) 1985, 1986 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU Emacs General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU Emacs, but only under the conditions described in the
GNU Emacs General Public License.   A copy of this license is
supposed to have been given to you along with GNU Emacs so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  */


/*
 *	Define symbols to identify the version of Unix this is.
 *	Define all the symbols that apply correctly.
 */

#define USG				/* System III, System V, etc */
#define USG5

/*      Specify IBM AIX version of system */

#ifndef AIX
#define AIX
#endif

/*      turn off c prototypes */
#ifndef _NO_PROTO
#define _NO_PROTO
#endif

/*      This symbol should be defined on AIX Version 3  ??????? */
#ifndef _AIX
#define _AIX
#endif

/*      Specify "_BSD" to invoke Berkeley compatibility in header files */
/*#ifndef _BSD
#define _BSD
#endif
*/

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "aix-v3"


/* nomultiplejobs should be defined if your system's shell
 does not have "job control" (the ability to stop a program,
 run some other program, then continue the first one).  */

/* #define NOMULTIPLEJOBS */

/* Default is to set interrupt_input to 0: don't do input buffering within Emacs */

/* #define INTERRUPT_INPUT */

/* In AIX, you allocate a pty by opening /dev/ptc to get the master side.
   To get the name of the slave side, you just ttyname() the master side.  */

#define PTY_ITERATION for (c = 0; !c ; c++)
#define PTY_NAME_SPRINTF strcpy (pty_name, "/dev/ptc");
#define PTY_TTY_NAME_SPRINTF strcpy (pty_name, ttyname (fd));

/*
 *	Define HAVE_TERMIO if the system provides sysV-style ioctls
 *	for terminal control.
 */

#define HAVE_TERMIO

/*
 *	Define HAVE_TIMEVAL if the system supports the BSD style clock values.
 *	Look in <sys/time.h> for a timeval structure.
 */

#define HAVE_TIMEVAL
 
/*
 *	Define HAVE_SELECT if the system supports the `select' system call.
 */

#define HAVE_SELECT 

/*
 *	Define HAVE_PTYS if the system supports pty devices.
 */

#define HAVE_PTYS

/* Define HAVE_SOCKETS if system supports 4.2-compatible sockets.  */

#define HAVE_SOCKETS

/*
 *	Define NONSYSTEM_DIR_LIBRARY to make Emacs emulate
 *      The 4.2 opendir, etc., library functions.
 */

/* #define NONSYSTEM_DIR_LIBRARY */

/*
 * 	Define SYSV_SYSTEM_DIR to use the V.3 getdents/readir
 *	library functions.  Almost, but not quite the same as
 *	the 4.2 functions
 */

#define SYSV_SYSTEM_DIR

/* Define this symbol if your system has the functions bcopy, etc. */

#define BSTRING

/* subprocesses should be defined if you want to
 have code for asynchronous subprocesses
 (as used in M-x compile and M-x shell).
 This is supposed to work now on system V release 2.  */

#define subprocesses

/* If your system uses COFF (Common Object File Format) then define the
   preprocessor symbol "COFF". */

/* #define COFF */

/* define MAIL_USE_FLOCK if the mailer uses flock
   to interlock access to /usr/spool/mail/$USER.
   The alternative is that a lock file named
   /usr/spool/mail/$USER.lock.  */

/* #define MAIL_USE_FLOCK */

/* Define CLASH_DETECTION if you want lock files to be written
   so that Emacs can tell instantly when you try to modify
   a file that someone else has modified in his Emacs.  */

/* #define CLASH_DETECTION */

/* Define SHORTNAMES if the C compiler can distinguish only
   short names.  It means that the stuff in ../shortnames
   must be run to convert the long names to short ones.  */

/* #define SHORTNAMES */

/* We do NOT use the Berkeley (and usg5.2.2) interface to nlist.  */

/* #define NLIST_STRUCT */

/* The file containing the kernel's symbol table is called /unix.  */

#define KERNEL_FILE "/unix"

/* The symbol in the kernel where the load average is found
   is named avenrun.  */

#define LDAV_SYMBOL "avenrun"

/* Special itemss needed to make Emacs run on this system.  */

#if 0 /* jwz: dkeller@vnet.IBM.COM says take this out; it's in syssignal.h */
/*
 *	Make the sigsetmask function go away.  Don't know what the
 *	ramifications of this are, but doesn't seem possible to
 *	emulate it properly anyway at this point.
 */

#define sigsetmask(mask)	/* Null expansion */
#endif /* 0 */

/* setjmp and longjmp can safely replace _setjmp and _longjmp,
   but they will run slower.  */

#define _setjmp setjmp
#define _longjmp longjmp

/* On USG systems the system calls are interruptable by signals
 that the user program has elected to catch.  Thus the system call
 must be retried in these cases.  All calls to read, write, and open
 in emacs are really calls to emacs_read, etc.  We define emacs_read
 to be sys_read (which is defined in sysdep.c for this system.)  If
 these were not defined, they would be defined to be open, etc.
 We can't just "#define open sys_open" because of prototype problems.
 */

#define emacs_read sys_read
#define emacs_open sys_open
#define emacs_write sys_write

#define INTERRUPTIBLE_OPEN
#define INTERRUPTIBLE_IO

/* On USG systems these have different names */

#define index strchr
#define rindex strrchr

/* USG systems tend to put everything declared static
   into the initialized data area, which becomes pure after dumping Emacs.
   Foil this.  Emacs carefully avoids static vars inside functions.  */

#undef static

/* Compiler bug bites on many systems when default ADDR_CORRECT is used.  */

/* #define ADDR_CORRECT(x) (x) */

#define LINKER cc

/* Prevent -lg from being used for debugging.  Not needed.  */

#define LIBS_DEBUG

/* No need to specify -lc when linking.  */

#define LIB_STANDARD

/* Use terminfo instead of termcap.  */

#define TERMINFO

/* The following definition seems to be needed in AIX version 3.1.6.8.
   It may not have been needed in certain earlier versions.  */
#define HAVE_TCATTR

#define SYSTEM_MALLOC

/* Use the gethostname system call.  */
#define HAVE_GETHOSTNAME

#define HAVE_CLOSEDIR

/* Apparently the AIX 3.2 C compiler has a buggy interpretation of const. */
#ifndef __GNUC__
#define const
#endif
