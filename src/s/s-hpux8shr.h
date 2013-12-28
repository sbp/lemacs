/* For building Lucid Emacs under HPUX 8.0 with dynamic libraries. */

/* Only support for hp9000s300 currently */
#if !defined(__hp9000s300)
#error Dynamic linking only supported for HP9000S300
#endif /* !hp9000s300 */

/* Don't tell the linker to link statically */
#ifdef THIS_IS_YMAKEFILE
#define LD_SWITCH_SYSTEM -L/usr/lib/X11R5
#endif /* THIS IS YMAKEFILE */

/* get call to brk() when rerunning xemacs */
#define RUN_TIME_REMAP

#include "s-hpux8.h"
