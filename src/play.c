/* play.c - play a sound file on the speaker
 **
 ** Copyright (C) 1989 by Jef Poskanzer.
 **
 ** Modified 24-May-91 by Jamie Zawinski (for Lucid Emacs.)
 ** Modified 17-Dec-92 by Jamie Zawinski (largely rewritten for SunOS 4.1.3.)
 **
 ** Permission to use, copy, modify, and distribute this software and its
 ** documentation for any purpose and without fee is hereby granted, provided
 ** that the above copyright notice appear in all copies and that both that
 ** copyright notice and this permission notice appear in supporting
 ** documentation.  This software is provided "as is" without express or
 ** implied warranty.
 */

#if __STDC__
#include <stdlib.h>
#include <unistd.h>
#endif

#include <stdio.h>
#include <string.h>
#include <sys/signal.h>
#include <sys/fcntl.h>
#include <sys/file.h>

#include <multimedia/libaudio.h>
#include <multimedia/audio_device.h>

#if __STDC__				/* warning suppression */
extern int audio__setplayhdr();
extern int audio__setgain();
extern int audio__flush();
extern int audio_decode_filehdr();
extern int audio_read_filehdr();
extern int audio_cmp_hdr();
extern int audio_enc_to_str();
extern int audio_drain();
extern void usleep();
#endif


#ifdef emacs
extern char *sys_errlist[];
extern int errno, sys_nerr;
extern void message ();
# define perror(string) \
    message("audio: %s, %s ", string, \
	    (errno < sys_nerr) ? sys_errlist[errno] : \
	    "unknown")
# define warn(str) message ("audio: %s ", (str))
#else /* !emacs */
# define warn(str) fprintf (stderr, "%s\n", (str))
#endif /* emacs */

static void (*sighup_handler) ();
static void (*sigint_handler) ();
static void sighandler ();

static int audio_fd;

#define audio_open()	open ("/dev/audio", (O_WRONLY | O_NDELAY))

static int reset_volume_p, reset_device_p;
static double old_volume;
static Audio_hdr dev_hdr;

static int
init_device (volume, data, fd, header_length)
     int volume;
     unsigned char *data;
     int fd;
     int *header_length;
{
#ifdef SUNOS4_0_3
  if (header_length) *header_length = 0;
  return 0;
#else
  Audio_hdr file_hdr;
  
  reset_volume_p = 0;
  reset_device_p = 0;

  if (data && fd) abort (); /* one or the other */

  if (AUDIO_SUCCESS != audio_get_play_config (audio_fd, &dev_hdr))
    {
      perror ("Not a valid audio device");
      return 1;
    }
  
  if (AUDIO_SUCCESS != (data
			? audio_decode_filehdr (data, &file_hdr, header_length)
			: audio_read_filehdr (fd, &file_hdr, 0, 0)))
    {
      if (data)
	perror ("invalid audio data");
      else
	perror ("invalid audio file");
      return 1;
    }
  
  audio_flush_play (audio_fd);

  if (0 != audio_cmp_hdr (&dev_hdr, &file_hdr))
    {
      Audio_hdr new_hdr;
      new_hdr = file_hdr;
      reset_device_p = 1;
      if (AUDIO_SUCCESS != audio_set_play_config (audio_fd, &new_hdr))
	{
	  char buf1 [100], buf2 [100], buf3 [250];
	  audio_enc_to_str (&file_hdr, buf1);
	  audio_enc_to_str (&new_hdr, buf2);
	  sprintf (buf3, "wanted %s, got %s", buf1, buf2);
	  warn (buf3);
	  return 1;
	}
    }

  if (volume < 0 || volume > 100)
    {
      char buf [255];
      sprintf (buf, "volume must be between 0 and 100 (not %d)", volume);
      warn (buf);
      return 1;
    }
  {
    /* set the volume; scale it to 0.0 - 1.0 */
    double V = (volume / 100.0);
    audio_get_play_gain (audio_fd, &old_volume);
    reset_volume_p = 1;
    audio_set_play_gain (audio_fd, &V);
  }

  return 0;
#endif
}


static void
reset_device (wait_p)
     int wait_p;
{
  if (wait_p)
    audio_drain (audio_fd, 1);
  else
    audio_flush_play (audio_fd);
  if (reset_device_p)
    audio_set_play_config (audio_fd, &dev_hdr);
  if (reset_volume_p)
    audio_set_play_gain (audio_fd, &old_volume);
}


void
play_sound_file (sound_file, volume)
    char *sound_file;
    int volume;
{
  int rrtn, wrtn;
  unsigned char buf [255];
  int file_fd;
  
  audio_fd = audio_open ();

  if (audio_fd < 0)
    {
      perror ("open /dev/audio");
      return;
    }

  sighup_handler = signal (SIGHUP, sighandler);
  sigint_handler = signal (SIGINT, sighandler);
  
  file_fd = open (sound_file, O_RDONLY);
  if (file_fd < 0)
    {
      perror (sound_file);
      goto END_OF_PLAY;
    }

  if (init_device (volume, 0, file_fd, 0))
    goto END_OF_PLAY;
    
  while (1)
    {
      rrtn = read (file_fd, (char *) buf, sizeof (buf));
      if (rrtn < 0)
	{
	  perror ("read");
	  goto END_OF_PLAY;
	}
      if (rrtn == 0)
	break;
	
      while (1)
	{
	  wrtn = write (audio_fd, (char *) buf, rrtn);
	  if (wrtn < 0)
	    {
	      perror ("write");
	      goto END_OF_PLAY;
	    }
	  if (wrtn != 0)
	    break;

	  if (AUDIO_ERR_INTERRUPTED == audio_drain (audio_fd, 1))
	    goto END_OF_PLAY;
	}
      if (wrtn != rrtn)
	{
	  char buf [255];
	  sprintf (buf, "play: rrtn = %d, wrtn = %d", rrtn, wrtn);
	  warn (buf);
	  goto END_OF_PLAY;
	}
    }
  
 END_OF_PLAY:

  if (file_fd > 0)
    close (file_fd);

  if (audio_fd > 0)
    {
      reset_device (1);
      close (audio_fd);
    }

  (void) signal (SIGHUP, sighup_handler);
  (void) signal (SIGINT, sigint_handler);
}


void
play_sound_data (data, length, volume)
     unsigned char *data;
     int length, volume;
{
  int wrtn, start = 0;
  int ilen;

  audio_fd = -1;

  if (length == 0) return;

  /* this is just to get a better error message */
  if (strncmp (".snd\0", (char *) data, 4))
    {
      warn ("Not valid audio data (bad magic number)");
      goto END_OF_PLAY;
    }
  if (length <= sizeof (Audio_hdr))
    {
      warn ("Not valid audio data (too short)");
      goto END_OF_PLAY;
    }

  audio_fd = audio_open ();
  if (audio_fd < 0)
    {
      perror ("open /dev/audio");
      return;
    }

  sighup_handler = signal (SIGHUP, sighandler);
  sigint_handler = signal (SIGINT, sighandler);
    
  if (init_device (volume, data, 0, &ilen))
    goto END_OF_PLAY;
      
  data   += (ilen<<2);
  length -= (ilen<<2);
  if (length <= 1)
    goto END_OF_PLAY;
    
  while (1)
    {
      wrtn = write (audio_fd, (char *) (data+start), length-start);
      if (wrtn < 0)
	{
	  perror ("write");
	  goto END_OF_PLAY;
	}
      if (wrtn != 0)
	{
	  start += wrtn;
	  break;
	}
      if (AUDIO_ERR_INTERRUPTED == audio_drain (audio_fd, 1))
	goto END_OF_PLAY;
    }
  if (wrtn != length)
    {
      char buf [255];
      sprintf (buf, "play: rrtn = %d, wrtn = %d", length, wrtn);
      warn (buf);
      goto END_OF_PLAY;
    }
  
 END_OF_PLAY:

  if (audio_fd > 0)
    {
      reset_device (1);
      close (audio_fd);
    }

  (void) signal (SIGHUP, sighup_handler);
  (void) signal (SIGINT, sigint_handler);
}


static void
sighandler (sig, code, scp, addr)
     int sig, code;
     struct sigcontext *scp;
     char *addr;
{
  if (audio_fd > 0)
    {
      reset_device (0);
      close (audio_fd);
    }
  if (sig == SIGHUP && sighup_handler)
    sighup_handler (sig, code, scp, addr);
  else if (sig == SIGINT && sigint_handler)
    sigint_handler (sig, code, scp, addr);
  else
    exit (1);
}
