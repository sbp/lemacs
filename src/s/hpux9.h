/* System description file for hpux version 9.  */

#include "hpux8.h"

#define HPUX9

/* lemacs change */
#undef C_SWITCH_X_SYSTEM
#define C_SWITCH_X_SYSTEM -I/usr/include/X11R5 -I/usr/include/Motif1.2
#undef LD_SWITCH_X_SYSTEM
#define LD_SWITCH_X_SYSTEM -L/usr/lib/X11R5 -L/usr/lib/Motif1.2

/* If Emacs doesn't seem to work when built to use GNU malloc, you
   probably need to get the latest patches to the HP/UX compiler.
   See `etc/MACHINES' for more information.  */
#if 0
#define SYSTEM_MALLOC 1
#undef GNU_MALLOC
#undef REL_ALLOC
#endif
