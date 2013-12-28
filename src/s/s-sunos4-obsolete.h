#include "s-bsd4-2.h"

#if 0  /* This may have been needed for an earlier version of Sun OS 4.
	  It seems to cause warnings in 4.0.3 and 4.1.  */
#define O_NDELAY        FNDELAY /* Non-blocking I/O (4.2 style) */
#endif

#ifdef __GNUC__
#define LD_SWITCH_SYSTEM -e __start -static
#else
#define LD_SWITCH_SYSTEM -e __start -Bstatic
#endif

/* Sun4s running SunOS 4+ usually have sound support. */
#define USE_SOUND
