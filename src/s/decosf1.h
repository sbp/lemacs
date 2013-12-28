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
#define C_SWITCH_SYSTEM	-D_BSD
#define LIBS_SYSTEM	-lbsd
#define sys_close close
#define NEED_REALPATH
#undef GNU_MALLOC
#define SYSTEM_MALLOC
#define SYSV_SYSTEM_DIR
