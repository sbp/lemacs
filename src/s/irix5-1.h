/* by Hayden Schultz <haydens@ll.mit.edu> for lemacs */

#ifndef IRIX5_1_H
#define IRIX5_1_H

#include "usg5-4.h"

#define IRIX5
#define IRIX
#define IRIS

#define SETPGRP_RELEASES_CTTY

#ifdef LIBS_SYSTEM
#undef LIBS_SYSTEM
#endif

#ifdef LIB_STANDARD
#undef LIB_STANDARD
#endif

#ifdef SYSTEM_TYPE
#undef SYSTEM_TYPE
#endif
#define SYSTEM_TYPE "irix"

#ifdef SETUP_SLAVE_PTY
#undef SETUP_SLAVE_PTY
#endif

/* thomas@mathematik.uni-bremen.de says this is needed.  */
/* Make process_send_signal work by "typing" a signal character on the pty.  */
#define SIGNALS_VIA_CHARACTERS

/* Define HAVE_ALLOCA to say that the system provides a properly
   working alloca function and it should be used. */
#define HAVE_ALLOCA
#undef C_ALLOCA

#ifndef NOT_C_CODE
#ifndef __GNUC__
#include <alloca.h>
#endif
#endif

/* SGI has all the fancy wait stuff, but we can't include sys/wait.h
   because it defines BIG_ENDIAN and LITTLE_ENDIAN (ugh!.)  Instead
   we'll just define WNOHANG right here.
   (An implicit decl is good enough for wait3.)  */

/* #define WNOHANG		0x1 */

/* No need to use sprintf to get the tty name--we get that from _getpty.  */
#ifdef PTY_TTY_NAME_SPRINTF
#undef PTY_TTY_NAME_SPRINTF
#endif
#define PTY_TTY_NAME_SPRINTF
/* No need to get the pty name at all.  */
#ifdef PTY_NAME_SPRINTF
#undef PTY_NAME_SPRINTF
#endif
#define PTY_NAME_SPRINTF
#ifdef emacs
char *_getpty();
#endif
/* We need only try once to open a pty.  */
#define PTY_ITERATION
/* Here is how to do it.  */
/* It is necessary to prevent SIGCHLD signals within _getpty.
   So we block them. But since all of Emacs uses classic SYSV signal()
   signals, there is no reliable way to do this (unlike BSD sighold or
   POSIX sigaction).  On Irix 5.* systems, the implementation of
   sigaction is as close as you can get to a universal. */
#define PTY_OPEN					    \
{							    \
  struct sigaction ocstat, cstat;			    \
  char * name;						    \
  sigemptyset(&cstat.sa_mask);				    \
  cstat.sa_handler = SIG_DFL;				    \
  cstat.sa_flags = 0;					    \
  sigaction(SIGCLD, &cstat, &ocstat);			    \
  name = _getpty (&fd, O_RDWR | O_NDELAY, 0600, 0);	    \
  sigaction(SIGCLD, &ocstat, (struct sigaction *)0);	    \
  if (name == 0)					    \
    return -1;						    \
  if (fd < 0)						    \
    return -1;						    \
  if (fstat (fd, &stb) < 0)				    \
    return -1;						    \
  strcpy (pty_name, name);				    \
}

/* jpff@maths.bath.ac.uk reports `struct exception' is not defined
   on this system, so inhibit use of matherr.  */
#define NO_MATHERR

/* Info from simon@lia.di.epfl.ch (Simon Leinen) suggests this is needed.  */
#define GETPGRP_NO_ARG

/* Ulimit(UL_GMEMLIM) is busted...  */
#define ULIMIT_BREAK_VALUE 0x14000000

#define POSIX_SIGNALS
#define I18N2

#define LD_SWITCH_SYSTEM -elf "-_SYSTYPE_SVR4" -require_dynamic_link _rld_new_interface -no_unresolved -Wx,-G 0 -L. -L./lwlib -g0 -call_shared -transitive_link

/* By Tor Arntsen <tor@spacetec.no> for lemacs.
   With the following kludge the above LD_SWITCH_SYSTEM will still work just 
   fine even with USE_GCC, and additional tweaking of config.h or ymakefile 
   is avoided. */
#ifdef NOT_C_CODE
# ifdef USE_GCC
#  undef LD_CMD
#  undef LIB_GCC
#  define LD_CMD ld
#  define LIB_GCC `gcc --print`
#  endif
#endif

#endif /* IRIX5_1_H */
