#include "s-bsd4-3.h"

/* Identify OSF1 for the m- files. */

#define OSF1

/* Define _BSD to tell the inlcude files we're running under
   the BSD universe and not the SYSV universe.  */

#define C_SWITCH_SYSTEM	-D_BSD
#define LIBS_SYSTEM	-lbsd
#define LD_SWITCH_SYSTEM	-non_shared

#ifndef SYSV_SYSTEM_DIR
# define SYSV_SYSTEM_DIR
#endif
#ifndef GETPGRP_NO_ARG
# define GETPGRP_NO_ARG
#endif
