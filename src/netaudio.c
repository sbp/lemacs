/* netaudio.c --- Lucid Emacs support for the NetAudio server.
 *
 * Author: Richard Caley <R.Caley@ed.ac.uk>
 *
 * Copyright 1994 Free Software Foundation, Inc.
 * Copyright 1993 Network Computing Devices, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and
 * its documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name Network Computing Devices, Inc. not be
 * used in advertising or publicity pertaining to distribution of this 
 * software without specific, written prior permission.
 * 
 * THIS SOFTWARE IS PROVIDED 'AS-IS'.  NETWORK COMPUTING DEVICES, INC.,
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 * LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL NETWORK
 * COMPUTING DEVICES, INC., BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING
 * SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA,
 * OR PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 * WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

/* There are four compile-time options.
 *
 * XTOOLKIT	This will be part of an Xt program.
 * 
 * XTEVENTS	The playing will be supervised asynchronously by the Xt event
 *		loop.  If not set, playing will be completed within the call
 *		to play_file etc. (which is appropriate for Emacs's use.)
 *
 * ROBUST_PLAY	Causes errors in netaudio to be caught.  This means that the
 *		program will attempt not to die if the netaudio server does.
 *
 * CACHE_SOUNDS	Causes the sounds to be cached in buckets in the netaudio
 *		server.  They are named by their comment field, or if that is
 *		empty by the filename, or for play_sound_data by a name made up
 *		from the sample itself.
 */

#if __STDC__ || defined(STDC_HEADERS)

#include <stdlib.h>
#include <unistd.h>
#endif

#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <errno.h>

#include <audio/audiolib.h>
#include <audio/soundlib.h>
#include <audio/snd.h>
#include <audio/fileutil.h>

#ifdef emacs
# define XTOOLKIT
# undef XTEVENTS
# define ROBUST_PLAY
# define CACHE_SOUNDS
extern char *sys_errlist[];
extern int errno, sys_nerr;
extern void message ();
# define perror(string) \
    message("netaudio: %s, %s ", string, strerror (errno))
# define warn(str) message ("audio: %s ", (str))
# define play_sound_file netaudio_play_sound_file
# define play_sound_data netaudio_play_sound_data
# define init_play netaudio_init_play
# define close_down_play netaudio_close_down_play
#else /* !emacs */
# define warn(str) fprintf (stderr, "%s\n", (str))
#endif /* emacs */

#ifdef XTOOLKIT
#    include <X11/Intrinsic.h>
#    include <audio/Xtutil.h>
#endif

#if defined(XTOOLKIT) && defined(ROBUST_PLAY)
    AuBool CatchIoErrorAndJump(AuServer *aud);
    AuBool CatchErrorAndJump(AuServer *aud, AuErrorEvent *event);
    void sigpipe_handle(void);

#   include <setjmp.h>

    jmp_buf ErrorJump;
#endif

static AuServer       *aud;
#ifdef XTOOLKIT
    static Display *aud_server;
    static XtInputId input_id;
#else
    static char *aud_server;
#endif

#ifdef XTOOLKIT
char *
init_play(display)
    Display *display;
#else
char *
init_play(server)
    char *server;
#endif
{
char *message;

#ifdef XTOOLKIT
char * server=DisplayString(display);
XtAppContext app_context= XtDisplayToApplicationContext(display);

aud_server = display;
#else

aud_server = server;
#endif

if (!(aud = AuOpenServer(server, 0, NULL, 0, NULL, &message)))
    if (message==NULL)
	return "Can't connect to audio server";
    else
	return message;

#ifdef XTEVENTS
input_id = AuXtAppAddAudioHandler(app_context, aud); 
#endif

#ifdef ROBUST_PLAY
     aud->funcs.ioerror_handler = CatchIoErrorAndJump;
     aud->funcs.error_handler = CatchErrorAndJump;
#endif

#ifdef CACHE_SOUNDS
 AuSetCloseDownMode(aud, AuCloseDownRetainPermanent, NULL);
#endif


return NULL;
}

void
close_down_play()

{
AuCloseServer(aud);
}

#ifndef XTEVENTS

static int done;

static void
doneCB(aud, handler, ev, data)
AuServer       *aud;
AuEvent        *ev;
AuEventHandlerRec *handler;
AuPointer       data;
{
    AuBool         *done = (AuBool *) data;

    *done = AuTrue;
}
#endif

#ifdef CACHE_SOUNDS

void
do_caching_play(s, volume, buf)

Sound s;
int volume;
unsigned char *buf;

{
AuBucketAttributes *list, b;
AuBucketID      id;
int n;

AuSetString(AuBucketDescription(&b),
	    AuStringLatin1, strlen(SoundComment(s)), SoundComment(s));

list = AuListBuckets(aud, AuCompCommonDescriptionMask, &b, &n, NULL);

if (list == NULL)
    {
    unsigned char *my_buf;

    if (buf==NULL)
	{
	if ((my_buf=malloc(SoundNumBytes(s)))==NULL)
	    {
	    return;
	    }

	if (SoundReadFile(my_buf, SoundNumBytes(s), s) != SoundNumBytes(s))
	    {
	    free(my_buf);
	    return;
	    }
	}
    else
	my_buf=buf;

    id = AuSoundCreateBucketFromData(aud, 
				     s,
				     my_buf,
				     AuAccessAllMasks, 
				     NULL,
				     NULL);
    if (buf == NULL)
	free(my_buf);
    }
else /* found cached sound */
    {
    id = AuBucketIdentifier(list);
    AuFreeBucketAttributes(aud, n, list);
    }

AuSoundPlayFromBucket(aud, 
		      id, 
		      AuNone,
		      AuFixedPointFromFraction(volume, 100), 
#ifdef XTEVENTS
		      NULL, NULL,
#else
		      doneCB, (AuPointer) &done,
#endif
		      1,
		      NULL, NULL,
		      NULL, NULL);

}
#endif /* CACHE_SOUNDS */

void
play_sound_file (sound_file, volume)
    char *sound_file;
    int volume;
{
AuEvent         ev;
void (*old_sigpipe)();

#ifdef ROBUST_PLAY
  old_sigpipe=signal(SIGPIPE, sigpipe_handle);
  if (setjmp(ErrorJump)!=0)
      {
      signal(SIGPIPE, old_sigpipe);
      return;
      }
#endif

if (aud==NULL)
    if (aud_server != NULL)
	{
	char *m;
	/* attempt to reconect */
	if ((m=init_play(aud_server))!= NULL)
	    {
#ifdef XTOOLKIT
	    XBell(aud_server, volume);
#else
	    warn("BEEP");
#endif
	    return;
	    }
	}
    else
	{
	warn("Attempt to play with no audio init\n");
	return;
	}

#ifndef XTEVENTS
done=0;
#endif

#ifndef CACHE_SOUNDS
    AuSoundPlayFromFile(aud,
			sound_file,
			AuNone,
			AuFixedPointFromFraction(volume,100),
#ifdef XTEVENTS
			NULL, NULL,
#else
			doneCB, (AuPointer) &done,
#endif
			NULL,
			NULL,
			NULL,
			NULL);
#else
    /* Cache the sounds in buckets on the server */

    {
    Sound s;

    if ((s = SoundOpenFileForReading(sound_file))==NULL)
	{
#ifdef ROBUST_PLAY
	signal(SIGPIPE, old_sigpipe);
#endif
	return;
	}

    if (SoundComment(s) == NULL || SoundComment(s)[0] == '\0')
	{
	SoundComment(s)=FileCommentFromFilename(sound_file);
	}

    do_caching_play(s, volume, NULL);

    SoundCloseFile(s);

    }
#endif /* CACHE_SOUNDS */

#ifndef XTEVENTS
   while (!done)
    {
	AuNextEvent(aud, AuTrue, &ev);
	AuDispatchEvent(aud, &ev);
    }
#endif

#ifdef ROBUST_PLAY
	signal(SIGPIPE, old_sigpipe);
#endif
}

 /********************************************************************\
 *                                                                    *
 * This code is here because the netaudio Sound library doesn't       *
 * support playing from a file buffered in memory.                    *
 *                                                                    *
 \********************************************************************/

/* Create a name from the sound. */

static char *
NameFromData(buf, len)

unsigned char *buf;
int len;

{
unsigned char name[9];
int i;
char *s;

buf+=len/2;
len -= len/2;

i=0;
while (i<8 && len >0)
    {
    while (*buf == 0 && len>0)
	{
	buf++;
	len--;
	}
    name[i]= *buf;
    i++;
    buf++;
    len--;
    }

name[i]='\0';

if (i==8)
    {
    strcpy(s=malloc(10), name);
    }
else 
    {
    strcpy(s=malloc(15), "short sound");
    }

return s;
}

/* Code to do a pseudo-open on a data buffer. Only for snd files at the
   moment. 
 */

static SndInfo *
SndOpenDataForReading(data, length)

const char *data;
int length;

{
SndInfo        *si;
int             size;

int i;

if (!(si = (SndInfo *) malloc(sizeof(SndInfo))))
    return NULL;

si->comment = NULL;
si->writing = 0;

memcpy(&si->h, data, sizeof(SndHeader));

if (LITTLE_ENDIAN)
    {
    char            n;
    
    swapl(&si->h.magic, n);
    swapl(&si->h.dataOffset, n);
    swapl(&si->h.dataSize, n);
    swapl(&si->h.format, n);
    swapl(&si->h.sampleRate, n);
    swapl(&si->h.tracks, n);
    }

if (si->h.magic != SND_MAGIC_NUM)
    {
    free(si);
    return NULL;
    }

size = si->h.dataOffset - sizeof(SndHeader);

if (size)
    {
    if (!(si->comment = (char *) malloc(size + 1)))
	{
	free(si);
	return NULL;
	}

    memcpy(si->comment,  data+sizeof(SndHeader), size);

    *(si->comment + size) = 0;
    }
else
    si->comment = NameFromData(data+si->h.dataOffset, length-si->h.dataOffset);

si->h.dataSize = length-si->h.dataOffset;

si->fp=NULL;

return si;
}

static Sound
SoundOpenDataForReading(data, length)

unsigned char *data;
int length;

{
Sound s;

if (!(s = (Sound) malloc(sizeof(SoundRec))))
    return NULL;

if ((s->formatInfo = SndOpenDataForReading(data, length))==NULL)
    {
    free(s);
    return NULL;
    }
    

if (!(SoundFileInfo[SoundFileFormatSnd].toSound) (s))
    {
    free(s);
    SndCloseFile(s->formatInfo);
    return NULL;
    }

return s;
}

void
play_sound_data (data, length, volume)
     unsigned char *data;
     int length, volume;
{
AuEvent         ev;
Sound s;
int offset;
void (*old_sigpipe)();

#ifdef ROBUST_PLAY
  old_sigpipe=signal(SIGPIPE, sigpipe_handle);
  if (setjmp(ErrorJump)!=0)
      {
      signal(SIGPIPE, old_sigpipe);
      return;
      }
#endif


if (aud==NULL)
    if (aud_server != NULL)
	{
	char *m;
	/* attempt to reconect */
	if ((m=init_play(aud_server))!= NULL)
	    {
#ifdef XTOOLKIT
	    XBell(aud_server, volume);
#else
	    warn("BEEP\n");
#endif
	    return;
	    }
	}
    else
	{
	warn("Attempt to play with no audio init\n");
	return;
	}

if ((s=SoundOpenDataForReading(data, length))==NULL)
    {
    warn("unknown sound type");
    return;
    }

if (SoundFileFormat(s) == SoundFileFormatSnd)
    {
    /* hack, hack */
    offset = ((SndInfo *)(s->formatInfo))->h.dataOffset;
    }
else
    {
    warn("only understand snd files at the moment");
    SoundCloseFile(s);
    return;
    }

#ifndef XTEVENTS
done=0;
#endif

#ifndef CACHE_SOUNDS
    AuSoundPlayFromData(aud,
			s,
			data+offset,
			AuNone,
			AuFixedPointFromFraction(volume,100),
#ifndef XTEVENTS
			doneCB, (AuPointer) &done,
#else
			NULL, NULL,
#endif
			NULL,
			NULL,
			NULL,
			NULL);
#else
    /* Cache the sounds in buckets on the server */

    {

    if (SoundComment(s)==NULL || SoundComment(s)[0] == '\0')
	{
	SoundComment(s)=NameFromData(data+offset, length-offset);
	}

    do_caching_play(s, volume, data+offset);
    }
#endif /* CACHE_SOUNDS */


#ifndef XTEVENTS
   while (!done)
    {
	AuNextEvent(aud, AuTrue, &ev);
	AuDispatchEvent(aud, &ev);
    }
#endif

SoundCloseFile(s); 

#ifdef ROBUST_PLAY
    signal(SIGPIPE, old_sigpipe);
#endif

}

#if defined(ROBUST_PLAY)

 /********************************************************************\
 *                                                                    *
 * Code to protect the client from server shutdowns.                  *
 *                                                                    *
 * This is unbelievably horrible.                                     *
 *                                                                    *
 \********************************************************************/

AuBool
CatchIoErrorAndJump(AuServer *old_aud)

{

warn("Netaudio connection broken"); 


#ifdef XTEVENTS
#ifdef XTOOLKIT
    {
    XtAppContext app_context= XtDisplayToApplicationContext(aud_server);
    AuXtAppRemoveAudioHandler(aud, input_id); 
    }
#endif

if(aud)
    AuCloseServer(aud);
aud=NULL;

/*
longjmp(AuPanicJump, 1);
*/
error("Netaudio connection broken"); 

#else /* XTEVENTS */

  if (aud)
      AuCloseServer(aud);
  aud=NULL;
  longjmp(ErrorJump, 1);
 
#endif /* XTEVENTS */

}

void
sigpipe_handle()

{
CatchIoErrorAndJump(NULL);
}

AuBool
CatchErrorAndJump(AuServer *old_aud,
		  AuErrorEvent *event)
{
CatchIoErrorAndJump(old_aud);
}

#endif /* ROBUST_PLAY */
