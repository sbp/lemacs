#include "m-mips.h"

#undef BIG_ENDIAN
#define LIBS_DEBUG

/* Supposedly the following will overcome a kernel bug.  */
#undef LD_SWITCH_MACHINE
#undef DATA_START
#define DATA_START 0x10000000
#define DATA_SEG_BITS 0x10000000

#define BROKEN_O_NONBLOCK
#undef TERMINFO

#define NEED_STRDUP
#define NEED_REALPATH
