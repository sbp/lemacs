#include "decosf1-2.h"

/* Supposedly gmalloc and rel_alloc will work now
   (grunwald@foobar.cs.colorado.edu) */
#undef SYSTEM_MALLOC
#undef NO_REMAP

#define _NO_MALLOC_WARNING_

#ifdef THIS_IS_YMAKEFILE
/* This to get rid of the -X that Makefile inserts
 * and force dynamic linking and optimization during link time.
 * The ifndef is needed to avoid screwups during configure
 */
#undef LD_SWITCH_SYSTEM
#define LD_SWITCH_SYSTEM -call_shared
#endif
