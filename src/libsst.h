/* libsst.h - include file for SPARC sound tools library
**
** Copyright (C) 1989 by Jef Poskanzer.
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software is provided "as is" without express or
** implied warranty.
*/

#include <sys/ioctl.h>
#ifndef SUNOS4_0_3
#define AUDIO_4_0_3_COMPAT
#define AUDIO_CHIP
#include <sbusdev/audio_79C30.h>
#include <multimedia/libaudio.h>
#include <multimedia/audio_device.h>
#else
#include <sbusdev/audioreg.h>
#endif
#include <sun/audioio.h>

#define SAMPLES_PER_SECOND 8192

int sst_open( );
void sst_close( /* int fd */ );

void sst_set_ger( /* int fd, value */ );
void sst_set_gr( /* int fd, value */ );
void sst_set_gx( /* int fd, value */ );

void sst_tones( /* int fd, dhz1, dhz2, thz, rhz, usec */ );
void sst_dtmf( /* int fd, char *dial, int usecper, usecpause */ );

#ifdef emacs
extern char *sys_errlist[];
extern int errno, sys_nerr;
# define perror(string) \
    message("audio: %s, %s", string, \
	    (errno < sys_nerr) ? sys_errlist[errno] : \
	    "unknown")
# define warn(str) message ("audio: %s", (str))
#else /* !emacs */
# define warn(str) fprintf (stderr, "%s\n", (str))
#endif /* emacs */
