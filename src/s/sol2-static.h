#include "sol2.h"

/* Force static linking */
/* Here is how to find X Windows.  The -R option says where
   to find X windows at run time.  */
#undef LD_SWITCH_SYSTEM
#ifndef __GNUC__
#define LD_SWITCH_SYSTEM -R/usr/openwin/lib -Bstatic
#else /* GCC */
/* jwz: note that we need "-Xlinker -Bstatic" and not just "-static" */
#define LD_SWITCH_SYSTEM -Xlinker -R/usr/openwin/lib -Xlinker -Bstatic
#endif /* GCC */

/* static linking and Solaris don't mix real well */
#undef LIB_STANDARD
#define LIB_STANDARD -lw -lintl -lc -Bdynamic -ldl
