#define OSF1

#ifndef THIS_IS_YMAKEFILE
#include <sys/param.h>
#endif

#include "bsd4-3.h"

/* Identify OSF1 for the m- files. */



/* Define _BSD to tell the include files we're running under
   the BSD universe and not the SYSV universe.  */

#define HAVE_SETSID
#define HAVE_SYS_TIME_H
#define HAVE_DUP2
#ifdef USE_GCC
#define C_SWITCH_SYSTEM	-D_BSD
#else
#define C_SWITCH_SYSTEM       -D_BSD -std
#define LD_SWITCH_SYSTEM
#endif
#define LIBS_SYSTEM	-lbsd
#define sys_close close
#define NEED_REALPATH
#define SYSTEM_MALLOC
#define SYSV_SYSTEM_DIR
#define GETPGRP_NO_ARG
#define HAVE_RINT

/* This to get rid of the -X that ymakefile inserts */
#undef LD_SWITCH_SYSTEM
#define LD_SWITCH_SYSTEM
