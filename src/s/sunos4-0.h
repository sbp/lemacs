/* For building Lucid Emacs under SunOS 4.1.* with static libraries. */

#ifndef _S_SUNOS4_H_
#define _S_SUNOS4_H_

#include "bsd4-2.h"

#ifndef HAVE_SYS_TIME_H
#define HAVE_SYS_TIME_H
#endif

#if 0  /* This may have been needed for an earlier version of Sun OS 4.
	  It seems to cause warnings in 4.0.3 and 4.1.  */
#define O_NDELAY        FNDELAY /* Non-blocking I/O (4.2 style) */
#endif

#ifdef THIS_IS_YMAKEFILE
  /* The new sunOS unexec eliminates the need for a custom crt0.o, so we
     can just let the compiler invoke the linker and don't have to guess
     what options it might have passed it. */
# define ORDINARY_LINK
# define START_FILES
# define LD_CMD $(CC)
# ifndef LD_SWITCH_SYSTEM
#  ifdef USE_GCC
/* of course gcc has to take different args than the rest of the universe */
#   define LD_SWITCH_SYSTEM -static
#  else
#   define LD_SWITCH_SYSTEM -Bstatic
#  endif
# endif
# define UNEXEC unexsunos4.o
#endif

#define RUN_TIME_REMAP

/* SunOS provides strcoll() and setlocale(). */
#define I18N2

/* these don't matter, but we have to define something to keep
   sysdep.c from introducing bogus symbols */
#define TEXT_START 0
#define DATA_START 0

/* lemacs change -- Sun CC needs this to default to ANSI */
#if __SUNPRO_C
#define C_SWITCH_SYSTEM -Xa
#endif

#ifndef THIS_IS_YMAKEFILE
#ifdef __STDC__
/* Sun's headers are categorically losing.
   Mly uses broken-sun.h to get the protos for this, but lcc provides all
   of the prototypes for the ANSI routines.  So I'm just going to put the
   protos of the non-ANSI routines that we use here (I guess that would
   be things that are Posix but not ANSI?)  You're in a maze of twisty
   little standards, all alike...
 */
extern int setpgrp ();
extern char *strdup ();
extern char *ttyname (int);
extern void tzsetwall (void);
extern int getpagesize (void);
#endif /* __STDC__ */

#define LOCALTIME_CACHE

# ifdef __GNUC__
  /* gcc has the bug that it claims to conform the the ANSI C standard
     (which is what setting __STDC__ to 1 means) but does not necessarily
     provide all of the library routines which the standard requires of a
     conforming compiler.  Such as memmove.  The other Sun ANSI compilers
     (Sun's acc and Lucid's lcc) do not have this bug. */
#  define memmove(to, from, size) bcopy ((from), (to), (size))
# endif /* __GNUC__ */

#endif /* !THIS_IS_YMAKEFILE */

#endif /* _S_SUNOS4_H_ */
