/* For building Lucid Emacs under SunOS 4.1.* with dynamic libraries. */

#ifdef THIS_IS_YMAKEFILE
#define LD_SWITCH_SYSTEM -Bdynamic
#endif

#include "s-sunos4.h"
