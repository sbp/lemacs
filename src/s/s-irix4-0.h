#include "s-irix3-3.h"

#define USG5_3

/* Maybe these are also in older IRIX versions */
#undef BROKEN_UNAME
#define HAVE_RANDOM
#define HAVE_GETWD
#define HAVE_RENAME
#define HAVE_SETPRIORITY
#undef HAVE_VFORK
#undef IRIS_UTIME
#define HAVE_DUP2
#define HAVE_GETTIMEOFDAY
#define HAVE_DREM

/* Define HAVE_ALLOCA to say that the system provides a properly
   working alloca function and it should be used. */
#define HAVE_ALLOCA
#include <alloca.h>

/* use K&R C */
#define C_SWITCH_MACHINE -cckr

#if 0 /* This has been suggested, but does not currently work..  */
/* No need to use sprintf to get the tty name--we get that from _getpty.  */
#define PTY_TTY_NAME_SPRINTF
/* No need to get the pty name at all.  */
#define PTY_NAME_SPRINTF
/* We need only try once to open a pty.  */
#define PTY_ITERATION
/* Here is how to do it.  */
/* ??? People say it is necessary to prevent SIGCHLD signals within _getpty.
   The right way is probably to block them.  Someone should try it.  */
#define PTY_OPEN						\
{								\
  char *name = _getpty (&fd, O_RDWR | O_NDELAY, 0600, 0);	\
  if (name == 0)						\
    return -1;							\
  if (fd < 0)							\
    return -1;							\
  if (fstat (fd, &stb) < 0)					\
    return -1;							\
  strcpy (pty_name, name);					\
}
#endif /* 0 */

/* IRIX 4.0 has sound support. */
#define USE_SOUND
