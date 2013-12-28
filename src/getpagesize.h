/* lemacs change */
#if 0
#ifdef BSD
#ifndef BSD4_1
#define HAVE_GETPAGESIZE
#endif
#endif
#endif

#if 0
#ifdef __hpux
#include <sys/types.h>
static size_t getpagesize() { return( 4096 ); }
#define HAVE_GETPAGESIZE
#endif
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
#if defined (sparc) && defined (USG)
#include <unistd.h>
#define getpagesize() PAGESIZE
#else /* not Solaris 2 */
#define getpagesize() NBPC
#endif /* not Solaris 2 */
#endif /* no NBPG */
#endif /* no EXEC_PAGESIZE */

#endif /* not HAVE_GETPAGESIZE */

