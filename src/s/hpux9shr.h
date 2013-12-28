/* For building Lucid Emacs under HPUX 9.0 with dynamic libraries. */

/* Only support for hp9000s300 currently */
#if !defined(__hp9000s300)
/* #ifndef USE_GCC */
#define HPUX_USE_SHLIBS
/* #endif */
#endif /* !hp9000s300 */

/* Don't tell the linker to link statically */
#ifdef THIS_IS_YMAKEFILE
#define START_FILES
#define LINKER $(CC)
/* now done in hpux8.h */
/* #define LD_SWITCH_SYSTEM -L/usr/lib/X11R5 -L/usr/lib/Motif1.2 */
#endif /* THIS IS YMAKEFILE */

/* get call to brk() when rerunning lemacs */
/* #ifndef USE_GCC */
#define RUN_TIME_REMAP
/* #endif */

#include "hpux9.h"
