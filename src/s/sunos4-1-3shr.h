/* For building Lucid Emacs under SunOS 4.1.* with dynamic libraries. */

#ifdef THIS_IS_YMAKEFILE
# ifdef USE_GCC
  /* of course gcc has to take different args than the rest of the universe */
#  define LD_SWITCH_SYSTEM -dynamic
# else
#  define LD_SWITCH_SYSTEM -Bdynamic
# endif
#endif

#include "sunos4-1-3.h"
