#include "sunos4-0.h"

/* 4.1.1 makes these system calls interruptible.  */

#define emacs_read sys_read
#define emacs_write sys_write
#define emacs_open sys_open
#define emacs_close sys_close

#define INTERRUPTIBLE_OPEN
#define INTERRUPTIBLE_CLOSE
#define INTERRUPTIBLE_IO

/* Cause the compilation of oldxmenu to use the right -I option.  */
#define OLDXMENU_OPTIONS CFLAGS=C_SWITCH_SYSTEM

/* Some systems do not run the Network Information Service, but have
   modified the shared C library to include resolver support without
   also changing the C archive library (/usr/lib/libc.a).  If we can't
   detect the presence of res_init, use -lresolv to supplement libc.a.
   The #ifdef HAVE_GETHOSTNAME is to prevent configure from
   setting libsrc_libs to -lresolv in lib-src/Makefile.  configure
   includes this file without defining any of the HAVE_* macros.  */
#ifdef HAVE_GETHOSTNAME
/* lemacs change: -lresolve should be added only if we have RES_INIT,
   not if we don't */
#ifdef HAVE_RES_INIT
#define LIBS_SYSTEM -lresolv
#endif
#endif

#if 0 /* Not necessary, since SYSTEM_MALLOC is defined in sunos4-0.h.  */
/* Tell GNU malloc to compensate for a bug in localtime.  */
#define SUNOS_LOCALTIME_BUG
#endif
