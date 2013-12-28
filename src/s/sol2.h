#include "usg5-4-2.h"		/* lemacs change from 5-4 to 5-4-2 */

/* eggert@twinsun.com said these work in Solaris.
   Perhaps they work in all kinds of SVR4, but this is more conservative.  */
#undef BROKEN_TIOCGETC
#undef BROKEN_TIOCGWINSZ

/* This triggers a conditional in xfaces.c.  */
#define XOS_NEEDS_TIME_H

#define POSIX

/* Here is how to find X Windows.  The -R option says where
   to find X windows at run time.  */
#ifndef __GNUC__
#define LD_SWITCH_SYSTEM -R/usr/openwin/lib
#else /* GCC */
#define LD_SWITCH_SYSTEM -Xlinker -R/usr/openwin/lib
#endif /* GCC */

/* lemacs change -- Sun CC needs this to default to ANSI */
#ifdef __SUNPRO_C
#define C_SWITCH_SYSTEM -Xa
#ifndef NOT_C_CODE
/* prototype left out out include files */
int gethostname (char *, int);
#endif
#endif /* __SUNPRO_C */

/* lemacs change -- removed flags to force K & R compilation */

/* Karl Berry writes:
If you have the misfortune to be running Solaris 2.1, you may have
noticed that the access system call does not check the readonlyness of
the filesystem the path refers to.  This is a bug, according to
access(2), but in the meantime, some of us need the right behavior.  */

/* Well, we released Emacs with this change, and fixed a typo, but
   people keep saying that it doesn't work, and that the patch is easy
   to install.  Patch number is 100947-02.  */
#undef SOLARIS_BROKEN_ACCESS

/*
 * lemacs change -- some Motif packages need -lgen to get regex and regcmp
 */

#undef LIBS_SYSTEM
#define LIBS_SYSTEM -lsocket -lnsl -lelf -lgen


/* #### lemacs change: until we've gotten the Energize builds converted
   over to use configure instead of ymakefile, we still need this.
 */
#ifdef THIS_IS_YMAKEFILE

# define LIB_INTL -L/usr/openwin/lib -lintl -lw
# define LIBS_TERMCAP -ltermlib 
# define LIBS_DEBUG
# undef LIBS_SYSTEM
# define LIBS_SYSTEM -lsocket -lnsl -lintl -lelf -lgen
# define START_FILES
# define LD_CMD $(CC)

#endif

#define SYSTEM_MALLOC
