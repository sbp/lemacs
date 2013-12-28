#include "m-mips.h"
#undef LIBS_MACHINE

#define LIBS_MACHINE -lmld

#define COFF
#undef LD_SWITCH_MACHINE
#define LD_SWITCH_MACHINE -x -D 800000

/* more pure Lisp code area when risc machine.  */
#define RISC

#if 0
#define PURESIZE 142000		/* (122000 + MORE) MORE=20000 */
#endif /* 0 */

/* #define C_OPTIMIZE_SWITCH -O2 */
#define C_OPTIMIZE_SWITCH -O

#define C_DEBUG_SWITCH -g3

/* Define BIG_ENDIAN iff lowest-numbered byte in a word
   is the most significant byte.  */

/* done in endian.h: #define BIG_ENDIAN */
/* so need to undef to avoid the confilict in wait.h */
#undef BIG_ENDIAN */

#undef TERMINFO

#define NO_REMAINDER
