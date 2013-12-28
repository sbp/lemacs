/* For building Lucid Emacs under SunOS 4.1.* with static libraries. */

#ifndef _S_SUNOS4_H_
#define _S_SUNOS4_H_

#include "s-bsd4-2.h"

#ifdef THIS_IS_YMAKEFILE
  /* The new sunOS unexec eliminates the need for a custom crt0.o, so we
     can just let the compiler invoke the linker and don't have to guess
     what options it might have passed it. */
# define START_FILES
# define LD_CMD $(CC)
# ifndef LD_SWITCH_SYSTEM
#  define LD_SWITCH_SYSTEM -Bstatic
# endif
# define UNEXEC unexsunos4.o
#endif

/* Sun4s running SunOS 4+ usually have sound support. */
#define USE_SOUND

#define RUN_TIME_REMAP

/* these don't matter, but we have to define something to keep
   sysdep.c from introducing bogus symbols */
#define TEXT_START 0
#define DATA_START 0

#endif /* _S_SUNOS4_H_ */
