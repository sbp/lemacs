#include "sunos4-0.h"

/* 4.1.1 makes these system calls interruptable.  */

#define emacs_read sys_read
#define emacs_write sys_write
#define emacs_open sys_open
#define emacs_close sys_close

#define INTERRUPTIBLE_OPEN
#define INTERRUPTIBLE_CLOSE
#define INTERRUPTIBLE_IO

# define LIB_INTL /*-L/usr/openwin/lib -lI18N*/
