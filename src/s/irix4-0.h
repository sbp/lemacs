#include "irix3-3.h"

#define USG5_3

/* Define HAVE_ALLOCA to say that the system provides a properly
   working alloca function and it should be used. */
#define HAVE_ALLOCA
#undef C_ALLOCA
#define alloca __builtin_alloca

/* SGI has all the fancy wait stuff, but we can't include sys/wait.h
   because it defines BIG_ENDIAN and LITTLE_ENDIAN (ugh!.)  Instead
   we'll just define WNOHANG right here.
   (An implicit decl is good enough for wait3.)  */

#define WNOHANG		0x1

/* No need to use sprintf to get the tty name--we get that from _getpty.  */
#undef PTY_TTY_NAME_SPRINTF
#define PTY_TTY_NAME_SPRINTF
/* No need to get the pty name at all.  */
#define PTY_NAME_SPRINTF
#ifdef emacs
char *_get_pty();
#endif
/* We need only try once to open a pty.  */
#define PTY_ITERATION
/* Here is how to do it.  */
/* It is necessary to prevent SIGCHLD signals within _getpty.
   So we block them. */
#define PTY_OPEN						\
{								\
  int mask = sigblock (sigmask (SIGCHLD));			\
  char *name = _getpty (&fd, O_RDWR | O_NDELAY, 0600, 0);	\
  sigsetmask(mask);						\
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

#define HAVE_GETTIMEOFDAY
#define HAVE_SYS_TIME_H
#undef IRIS_UTIME
#define IRIX_CONST_ENUM_BUG
#define INTERRUPTIBLE_CLOSE
