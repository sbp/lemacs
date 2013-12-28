#define OSF1

#ifndef THIS_IS_YMAKEFILE
#include <sys/param.h>
#endif

#include "bsd4-3.h"

/* Identify OSF1 for the m- files. */



/* Define _BSD to tell the include files we're running under
   the BSD universe and not the SYSV universe.  */

#define C_SWITCH_SYSTEM	-std -D_BSD
#define LIBS_SYSTEM	-lbsd
#define sys_close close
#define SYSTEM_MALLOC
#define SYSV_SYSTEM_DIR
#define GETPGRP_NO_ARG

/* This to get rid of the -X that ymakefile inserts */
#undef LD_SWITCH_SYSTEM
#define LD_SWITCH_SYSTEM
