/* xmmanagerp.h

   This file just includes the Motif header file ManagerP.h, but does
   the necessary magic to do this properly.
*/

/* Motif attempts to use old '/ * * /' method of pasting tokens together
   unless __STDC__ > 0.  Bad idea, because the SunPro C compiler defines
   __STDC__ to 0 in "lenient ANSI mode" (which is what you need to
   compile Emacs in).  Unfortunately, some compilers don't let you mess
   around with __STDC__, so ... */

#if defined(__SUNPRO_C) && (__STDC__ == 0)
# undef __STDC__
# define __STDC__ 1
# define __STDC__CHANGED__
#endif

/* ManagerP.h doesn't exist in old versions of Motif; the stuff is
   in XmP.h instead */

#include <Xm/Xm.h>	/* to get XmVersion */
#if (XmVersion >= 1002)
# include <Xm/ManagerP.h>
#else
# include <Xm/XmP.h>
#endif

#ifdef __STDC__CHANGED__
# undef __STDC__
# define __STDC__ 0
# undef __STDC__CHANGED__
#endif
