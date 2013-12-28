#include "s-bsd4-2.h"

/* Say that the text segment of a.out includes the header;
   the header actually occupies the first few bytes of the text segment
   and is counted in hdr.a_text.  */

#define O_NDELAY        FNDELAY /* Non-blocking I/O (4.2 style) */

/* This is supposed to avoid a mysterious bug.  */

#define SYSTEM_MALLOC

/*  Misleading!  Actually gets loaded after crt0.o */
#define START_FILES pre-crt0.o

/*
 *  Kludge!  can't get at symbol "start" in std crt0.o
 *  Who the #$%&* decided to remove the __ characters!
 *  Someone needs to fix this in sysdep.c  with an #ifdef BROKEN_START in
 * sysdep.c.  We do not use this address so any value should do really.  Still
 *  may need it in the future?
 */
#define BROKEN_START
#define TEXT_START 0x2020

#define UNEXEC	unexsunos4.o
#define RUN_TIME_REMAP
#define LD_CMD cc
