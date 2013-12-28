#include "s-irix3-3.h"

#define USG5_3

/* Maybe these are also in older IRIX versions */
#undef BROKEN_UNAME
#define HAVE_RANDOM
#define HAVE_GETWD
#define HAVE_RENAME
#define HAVE_SETPRIORITY
#undef HAVE_SYSVIPC
#undef HAVE_VFORK
#undef IRIS_UTIME
#define HAVE_DUP2
#define HAVE_GETTIMEOFDAY
#define HAVE_SIGLIST

/* Define HAVE_ALLOCA to say that the system provides a properly
   working alloca function and it should be used. */
#define HAVE_ALLOCA
#ifdef emacs
#ifndef THIS_IS_YMAKEFILE
#include <alloca.h>
#include <unistd.h>
#endif
#endif

#define C_SWITCH_MACHINE -ansi -D__EXTENSIONS__ -D_POSIX_SOURCE -Dmips=__mips

/* SGI has all the fancy wait stuff, but sys/wait.h defines both
   BIG_ENDIAN and LITTLE_ENDIAN, so we have to do some undefining.
   We know that m/m-iris4d.h is included after this file and that
   it defines BIG_ENDIAN.  */

#ifdef emacs
#include <sys/wait.h>
#undef LITTLE_ENDIAN
#undef BIG_ENDIAN
#define HAVE_WAIT_HEADER
#define WAITTYPE union wait
#define WRETCODE(x)	WEXITSTATUS(x)
#define WCOREDUMP(x)	((x).w_coredump)
#endif

/* No need to use sprintf to get the tty name--we get that from _getpty.  */
#define PTY_TTY_NAME_SPRINTF
/* No need to get the pty name at all.  */
#define PTY_NAME_SPRINTF
/* We need only try once to open a pty.  */
#define PTY_ITERATION
/* Here is how to do it.  */
/* It is necessary to prevent SIGCHLD signals within _getpty.
   So we block them.  */
#define PTY_OPEN						\
{								\
  int   mask =  sigblock(sigmask(SIGCHLD));                     \
  char *name = _getpty (&fd, O_RDWR | O_NDELAY, 0600, 0);	\
                                                                \
  sigsetmask(mask);                                             \
                                                                \
  if (name == 0)						\
    return -1;							\
  if (fd < 0)							\
    return -1;							\
  if (fstat (fd, &stb) < 0)					\
    return -1;							\
  strcpy (pty_name, name);					\
}

/* IRIX 4.0 has sound support. */
#define USE_SOUND

#define FLOAT_CATCH_SIGILL
