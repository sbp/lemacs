/* play.c - play a sound file on the speaker
 **
 ** Copyright (C) 1989 by Jef Poskanzer.
 **
 ** Modified 24-May-91 by Jamie Zawinski (for emacs.)
 **
 ** Permission to use, copy, modify, and distribute this software and its
 ** documentation for any purpose and without fee is hereby granted, provided
 ** that the above copyright notice appear in all copies and that both that
 ** copyright notice and this permission notice appear in supporting
 ** documentation.  This software is provided "as is" without express or
 ** implied warranty.
 */

#include <stdio.h>
#include <fcntl.h>
#include <sys/file.h>
#include <sys/signal.h>
#include "libsst.h"

#define MYBUF 256

int sst_fd;

void (*sighup_handler)();
void (*sigint_handler)();

static void sighandler ();

void
play_sound_file (sound_file, volume)
    char *sound_file;
    int volume;
{
    int rrtn, wrtn;
    unsigned char buf[MYBUF];
    int file_fd;
    
    sst_fd = sst_open(volume,0);
    if ( sst_fd < 0 ) return;
    sighup_handler = signal( SIGHUP, sighandler );
    sigint_handler = signal( SIGINT, sighandler );
    
    file_fd = open( sound_file, O_RDONLY );
    if ( file_fd < 0 )
    {
	perror( sound_file );
	goto end_of_play;
    }
#ifndef SUNOS4_0_3
    {
    int err;
    Audio_hdr Dev_hdr;
    Audio_hdr File_hdr;

    err = audio_get_play_config( sst_fd, &Dev_hdr );

    if ( err != AUDIO_SUCCESS )
    {
	perror( "Not a valid audio device" );
	goto end_of_play;
    }

    err = audio_read_filehdr( file_fd, &File_hdr, (char *) NULL, 0 );
    if ( err != AUDIO_SUCCESS )
    {
	perror( "Not a valid audio file" );
	goto end_of_play;
    }

    /* Shamelessly pilfered from /usr/demo/SOUND/play.c */
    if ( audio_cmp_hdr(&Dev_hdr, &File_hdr) != 0 ) {
	if ( audio_drain( sst_fd, FALSE) != AUDIO_SUCCESS) {
	    perror( "draining audio device" );
	    goto end_of_play;
	}
    /* Fancy code to reconfigure audio device for given file header omitted */
    }
    }
#endif
    
    for ( ; ; )
    {
	rrtn = read( file_fd, buf, MYBUF );
	if ( rrtn < 0 )
	{
	    perror( "read" );
	    goto end_of_play;
	}
	if ( rrtn == 0 )
	    break;
	
	for ( ; ; )
	{
	    wrtn = write( sst_fd, buf, rrtn );
	    if ( wrtn < 0 )
	    {
		perror( "write" );
		goto end_of_play;
	    }
	    if ( wrtn != 0 )
		break;
	    usleep( 1000 );
	}
	if ( wrtn != rrtn )
	{
	  char buf[255];
	  sprintf (buf, "play: rrtn = %d, wrtn = %d", rrtn, wrtn);
	  warn (buf);
	  goto end_of_play;
	}
    }
    
    if (file_fd > 0)
	close(file_fd);
#ifndef SUNOS4_0_3
    if ( audio_drain( sst_fd, FALSE) != AUDIO_SUCCESS) {
	perror( "draining audio device" );
	goto end_of_play;
    }
#endif

 end_of_play:

    sst_close( sst_fd );
    (void) signal( SIGHUP, sighup_handler );
    (void) signal( SIGINT, sigint_handler );
}


void
play_sound_data (data, length, volume)
    unsigned char *data;
    int length, volume;
{
    int rrtn, wrtn, start = 0;
    int ilen;

    if (!length) return;

    sst_fd = sst_open(volume,0);
    if ( sst_fd < 0 ) return;

    sighup_handler = signal( SIGHUP, sighandler );
    sigint_handler = signal( SIGINT, sighandler );
    
#ifndef SUNOS4_0_3
    {
    int err;
    Audio_hdr Dev_hdr;
    Audio_hdr File_hdr;

    err = audio_get_play_config( sst_fd, &Dev_hdr );

    if ( err != AUDIO_SUCCESS )
    {
	perror( "Not a valid audio device" );
	goto end_of_play;
    }

    if (strncmp (".snd\0", data, 4)) {
      perror ("Not valid audio data (bad magic number)");
      goto end_of_play;
    }
    if (length <= sizeof (Audio_hdr)) {
      perror( "Not valid audio data (too short)" );
      goto end_of_play;
    }
    
    err = audio_decode_filehdr (data, &File_hdr, &ilen);
    if ( err != AUDIO_SUCCESS )
    {
	perror( "Not a valid audio file" );
	goto end_of_play;
    }

    data   += (ilen<<2);
    length -= (ilen<<2);
    if (length <= 1) return;

    /* Shamelessly pilfered from /usr/demo/SOUND/play.c */
    if ( audio_cmp_hdr(&Dev_hdr, &File_hdr) != 0 ) {
	if ( audio_drain( sst_fd, FALSE) != AUDIO_SUCCESS) {
	    perror( "draining audio device" );
	    goto end_of_play;
	}
    /* Fancy code to reconfigure audio device for given file header omitted */
    }
    }
#endif
    
    for ( ; ; )
    {
      wrtn = write( sst_fd, data+start, length-start );
      if ( wrtn < 0 )
	{
	  perror( "write" );
	  goto end_of_play;
	}
      if ( wrtn != 0 ) {
	start += wrtn;
	break;
      }
      usleep( 1000 );
    }
    if ( wrtn != length )
      {
	char buf [255];
	sprintf( buf, "play: rrtn = %d, wrtn = %d", length, wrtn );
	warn (buf);
	goto end_of_play;
      }
    
#ifndef SUNOS4_0_3
    if ( audio_drain( sst_fd, FALSE) != AUDIO_SUCCESS) {
	perror( "draining audio device" );
	goto end_of_play;
    }
#endif

  end_of_play:

    sst_close( sst_fd );
    (void) signal( SIGHUP, sighup_handler );
    (void) signal( SIGINT, sigint_handler );
}


static void
sighandler(sig, code, scp, addr)
    int sig, code;
    struct sigcontext *scp;
    char *addr;
{
    sst_close( sst_fd );
    if (sig == SIGHUP && sighup_handler)
	(*sighup_handler)(sig, code, scp, addr);
    else if (sigint_handler)
	(*sigint_handler)(sig, code, scp, addr);
    exit( 1 );
}
