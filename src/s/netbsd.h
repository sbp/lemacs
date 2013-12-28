/* s/ file for netbsd system.  */

/* Get most of the stuff from bsd4.3 */
#include "bsd4-3.h"

/* For mem-limits.h.  */
#define BSD4_2

#undef SYSTEM_TYPE
#define SYSTEM_TYPE "netbsd"

#undef KERNEL_FILE
#undef LDAV_SYMBOL
#define HAVE_GETLOADAVG

#define HAVE_UNION_WAIT

#define SIGNALS_VIA_CHARACTERS

#define PENDING_OUTPUT_COUNT(FILE) ((FILE)->_p - (FILE)->_bf._base)

/* netbsd uses OXTABS instead of the expected TAB3.  */
#define TAB3 OXTABS

#define A_TEXT_OFFSET(x) (sizeof (struct exec))
#define A_TEXT_SEEK(hdr) (N_TXTOFF(hdr) + A_TEXT_OFFSET(hdr))

#define HAVE_TERMIOS
#define NO_TERMIO

#define LIBS_DEBUG
/* -lutil is not needed for NetBSD >0.9.  */
#define LIBS_SYSTEM -lutil
#define LIBS_TERMCAP -ltermcap

#define NEED_ERRNO
#define SYSV_SYSTEM_DIR

/* Netbsd has POSIX-style pgrp behavior.  */
#undef BSD_PGRPS

/* These definitions should work for either dynamic or static linking,
   whichever is the default for `cc -nostdlib'.  */
/* lemacs change: from jrg@doc.ic.ac.uk */
#if 0	/* not in NetBSD 0.9 */
#define HAVE_TEXT_START		/* No need to define `start_of_text'.  */
#define START_FILES pre-crt0.o /usr/lib/crt0.o
#define UNEXEC unexsunos4.o
#define RUN_TIME_REMAP
#else
#define START_FILES crt0.o
#endif

/* Try to make this work for both 0.9 and >0.9.  */
#ifndef N_TRELOFF
#define N_PAGSIZ(x) __LDPGSZ
#define N_BSSADDR(x) (N_ALIGN(x, N_DATADDR(x)+x.a_data))
#define N_TRELOFF(x) N_RELOFF(x)
#endif

/* Reread the time zone on startup.  */
#define LOCALTIME_CACHE

#define HAVE_WAIT_HEADER

/* lemacs change -- jrg@doc.ic.ac.uk */
#define GETPGRP_NO_ARG 1
