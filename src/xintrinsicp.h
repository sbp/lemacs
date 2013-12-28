#undef CONST                    /* X11R4 header thinks it can define CONST */

#include <X11/Intrinsic.h>
#include <X11/IntrinsicP.h>

#ifdef CONST_IS_LOSING
# define CONST
#else
# define CONST const
#endif
