/* system description file for hpux version 8.
   This contains changes that were suggested "for the hp700".
   They were not needed for the 800.
   Our conjecture that they are needed for hpux version 8,
   which is what runs on the 700.  */

#include "hpux.h"

#define HPUX8

/* lemacs change -- suggested by hamish@bnr.ca */
#undef static
#undef HPUX_PRE_8_0

/* lemacs change -- Ugly, nasty kludge to prevent X11R4 Xos.h from
   redefining struct timeval and struct timezone. */
#define __TIMEVAL__

/* lemacs change -- changed LIBX11_SYSTEM and C_SWITCH_X_SYSTEM */
#define C_SWITCH_X_SYSTEM -I/usr/include/X11R4 -I/usr/include/Motif1.1
#define LD_SWITCH_X_SYSTEM -L/usr/lib/X11R4 -L/usr/lib/Motif1.1

/* No need to specify roundabout way of linking temacs.  */
#define ORDINARY_LINK

#ifdef __GNUC__
/* lemacs change */
#ifndef HPUX_USE_SHLIBS
#define LD_SWITCH_SYSTEM -Xlinker -a -Xlinker archive
#else
#define LD_SWITCH_SYSTEM
#endif
#else
/* lemacs change */
#if !defined (LD_SWITCH_SYSTEM)
#if !defined (HPUX_USE_SHLIBS)
#define LD_SWITCH_SYSTEM -Wl,-a,archive
#endif
#endif
#endif

/* lemacs change */
#ifndef __GNUC__
#define C_SWITCH_SYSTEM -Aa -D_HPUX_SOURCE
#endif

#if 0 /* This should no longer be necessary now that
	 C_SWITCH_... are passed down when compiling oldXMenu.  */
/* Specify compiler options for compiling oldXMenu.  */
#define OLDXMENU_OPTIONS CFLAGS="-I/usr/include/X11R5 -I/usr/include/X11R4"
#endif

/* Some hpux 8 machines seem to have TIOCGWINSZ,
   and none have sioctl.h, so might as well define this.  */
#define NO_SIOCTL_H

#if 0 /* autoconf should be detecting the presence or absence of 
	 random and srandom now.  */
/* If you use X11R4 you must define this.  If you use
   X11R5 you must comment this out */
/* #define HAVE_RANDOM */
#define random foo_random
#define srandom foo_srandom
#endif

#if 0  /* This seems to be spurious.  */
/* "X11R5" on hpux8 doesn't have this function, which is supposed to exist
   in X11R5.  Maybe things will work if we just don't call it.  */
#define NO_XRM_SET_DATABASE
#endif

/* Enable a special hack in XTread_socket.  */
/* lemacs change:  we don't use this. */
#if 0
#define X_IO_BUG
#endif

/* lemacs change */
#undef HAVE_TERMIO
#define HAVE_TERMIOS
#define POSIX_SIGNALS
#define I18N2
#define HAVE_WAIT_HEADER
#define WAITTYPE int
#define WRETCODE(x) WEXITSTATUS(x)
