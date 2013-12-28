/* For building Lucid Emacs under HPUX 8.0 with dynamic libraries. */

/* Don't tell the linker to link statically */
#ifdef THIS_IS_YMAKEFILE
/* now done in hpux8.h */
/* #define LD_SWITCH_SYSTEM -L/usr/lib/X11R4 -L/usr/lib/Motif1.1 */
#ifdef __GNUC__
#define LIB_STANDARD
#endif
#endif /* THIS IS YMAKEFILE */

/* get call to brk() when rerunning lemacs */
#define RUN_TIME_REMAP

#define HPUX_USE_SHLIBS

#include "hpux8.h"
