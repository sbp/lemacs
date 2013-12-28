#ifdef BSD
#ifndef BSD4_1
#define HAVE_GETPAGESIZE
#endif
#endif

#ifdef __hpux
#include <sys/types.h>
static size_t getpagesize() { return( 4096 ); }
#define HAVE_GETPAGESIZE
#endif
  
#ifndef HAVE_GETPAGESIZE

#include <sys/param.h>

#ifdef EXEC_PAGESIZE
#define getpagesize() EXEC_PAGESIZE
#else
#ifdef NBPG
#define getpagesize() NBPG * CLSIZE
#ifndef CLSIZE
#define CLSIZE 1
#endif /* no CLSIZE */
#else /* no NBPG */
#define getpagesize() NBPC
#endif /* no NBPG */
#endif /* no EXEC_PAGESIZE */

#endif /* not HAVE_GETPAGESIZE */

