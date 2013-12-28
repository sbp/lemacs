/* s/ file for netbsd system.  */

/* jrg@doc.ic.ac.uk 23 Nov 1993 - 
/* hacked for Lucid 19.8 from netbsd.h for gnu emacs19-21
   works on NetBSD 0.9 */

/* Get most of the stuff from bsd4.3 */
#include "bsd4-3.h"

#undef SYSTEM_TYPE
#define SYSTEM_TYPE "netbsd"

#undef KERNEL_FILE
#define KERNEL_FILE "/netbsd"

#undef LDAV_SYMBOL
#define LDAV_SYMBOL "_averunnable"

#define SIGNALS_VIA_CHARACTERS

#define PENDING_OUTPUT_COUNT(FILE) ((FILE)->_p - (FILE)->_bf._base)

#define A_TEXT_OFFSET(x) (sizeof (struct exec))
#define A_TEXT_SEEK(hdr) (N_TXTOFF(hdr) + A_TEXT_OFFSET(hdr))

#define HAVE_SETSID

/* defines for comments in config.h */
/* no realpath() function in NetBSD */
#define NEED_REALPATH
/* link using libraries in /usr/X386/lib */
#define LD_SWITCH_SITE -L/usr/X386/lib
/* X include files in /usr/X386/include */
#define C_SWITCH_SITE -I/usr/X386/include
/* inhibit debug code in libraries */
#define LIBS_DEBUG 
/* inhibit debug code in objects */
#define C_DEBUG_SWITCH
#define LIBS_SYSTEM -lutil 

/* c_debug_switch replaced with c_optimize_switch in ymakefile */
#define C_OPTIMIZE_SWITCH -O6

#define HAVE_GETLOADAVG

/* ZMAGIC and QMAGIC require page alignments */
#define SECTION_ALIGNMENT (__LDPGSZ-1)
#define ZQMAGIC(hdr) ( (N_GETMAGIC(hdr) == ZMAGIC) || \
            (N_GETMAGIC(hdr) == QMAGIC) )

