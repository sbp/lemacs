#include "usg5-4-2.h"

#ifdef BSTRING
ERROR! SunOS 5.x does not have BSTRING.
#endif

#if !defined(__GNUC__) && !defined(__lucid)
  /* The Sun compiler (acc) is apparently broken w.r.t. unions. */
# define NO_UNION_TYPE
#endif

#define POSIX

#define HAVE_TERMIOS

#define HAVE_VFORK

#undef HAVE_GETHOSTNAME

#define HAVE_RINT

#if !defined(sun)
#define sun
#endif /* sun */

#define HAVE_UNISTD_H

#define HAVE_UTIMBUF

/* Sun4s running SunOS 4+ usually have sound support. */
#define USE_SOUND

/* SunOS provides strcoll() and setlocale(). */
#define I18N2

#ifdef THIS_IS_YMAKEFILE

# define LD_SWITCH_SYSTEM -L/usr/openwin/lib		\
			  -L/opt/SUNWmotif/lib		\
			  -L/pkg/src/X11/xpm-3.3/lib	\
			  -R/usr/openwin/lib:/opt/SUNWmotif/lib

# define C_SWITCH_SYSTEM -I/usr/openwin/include		\
			 -I/opt/SUNWmotif/include	\
			 -I/pkg/src/X11/xpm-3.3/lib

# define LIB_INTL -L/usr/openwin/lib -lintl -lw
# define LIBS_TERMCAP -ltermlib -lcurses
# define LIBS_DEBUG
# undef LIBS_SYSTEM
# define LIBS_SYSTEM -lsocket -lnsl -lelf -lgen
# define START_FILES
# define LD_CMD $(CC)

#endif
