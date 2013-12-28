#include "usg5-4-2.h"

/* Strange and varied things occur under Solaris if Lisp_Object's are unions */
/* #define NO_UNION_TYPE */

#define POSIX

#define HAVE_VFORK

/* Here is how to find X Windows.  */
#define LD_SWITCH_SYSTEM -L$(OPENWINHOME)/lib -R$(OPENWINHOME)/lib
#define C_SWITCH_SYSTEM -I$(OPENWINHOME)/include

/* Sun4s running SunOS 4+ usually have sound support. */
#define USE_SOUND

#define LIBS_DEBUG
