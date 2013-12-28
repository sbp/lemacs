/*
 * Timer program for GNU Emacs timer implementation.
 * Copyright (C) 1988 Kyle E. Jones
 *
 * Verbatim copies of this file may be freely redistributed.
 *
 * Modified versions of this file may be redistributed provided that this
 * notice remains unchanged, the file contains prominent notice of
 * author and time of modifications, and redistribution of the file
 * is not further restricted in any way.
 *
 * This file is distributed `as is', without warranties of any kind.
 */

/*
 * #define USG if this is a System V system.
 */

#include <stdio.h>
#include <signal.h>

#define boolean char
#define TRUE 1
#define FALSE 0

boolean signaled = FALSE;

wakeup()
{
#ifdef USG
    (void) signal(SIGINT, wakeup);
    (void) signal(SIGALRM, wakeup);
#endif
    signaled = TRUE;
}

main()
{
    unsigned sleeptime;
    long time(), lastwakeup, now;
    char timebuf[20];

    (void) signal(SIGINT, wakeup);
    (void) signal(SIGALRM, wakeup);

    (void) time(&lastwakeup);

    /*
     * 1. read the number of seconds to sleep frmo stdin.
     * 2. sleep until a SIGALRM or SIGINT arrives.
     * 3. report the number of seconds actually slept to stdout.
     * 4. repeat...
     */
    while (1) {
	/* read the number of seconds we should sleep */
	(void) gets(timebuf);
	sleeptime = atoi(timebuf);
	(void) alarm(sleeptime);
	/* sleep if no signal received since last wakeup */
	if (! signaled)
	  (void) pause();
	signaled = FALSE;
	/* report the number of seconds we actually slept */
	(void) time(&now);
	(void) sprintf(timebuf, "%d", now - lastwakeup);
	(void) fputs(timebuf, stdout);
	(void) fflush(stdout);
	lastwakeup = now;
    }
}
