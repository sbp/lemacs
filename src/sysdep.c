/* Interfaces to system-dependent kernel and library entries.
   Copyright (C) 1985, 1986, 1987, 1988, 1992, 1993 
   Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "config.h"
#include "lisp.h"

#include <stdio.h>		/* For sprintf */
#include <signal.h>
#include <setjmp.h>

#include "sysdep.h"
#include "blockio.h"

#include "dispmisc.h"

#define min(x,y) ((x) > (y) ? (y) : (x))

/* >>> kludges: need `configure' >>>> */
#define HAVE_MKDIR
#define HAVE_RMDIR
#ifndef USG
# define HAVE_VFORK
#endif
/* >>>> end kludges >>>> */

/* In this file, open, read and write refer to the system calls,
   not our sugared interfaces  sys_open, sys_read and sys_write.
 */
#undef emacs_open
#undef emacs_close
#undef emacs_read
#undef emacs_write
#define emacs_open  -- dont use emacs_open in sysdep.c --
#define emacs_close -- dont use emacs_close in sysdep.c --
#define emacs_read  -- dont use emacs_read in sysdep.c --
#define emacs_write -- dont use emacs_write in sysdep.c --

/* Does anyone other than VMS need this? */
#ifndef fwrite
#define sys_fwrite fwrite
#else
#undef fwrite
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

#ifdef VMS
#include <rms.h>
#include <ttdef.h>
#include <tt2def.h>
#include <iodef.h>
#include <ssdef.h>
#include <descrip.h>
#include <fibdef.h>
#include <atrdef.h>
#include <ctype.h>
#include <string.h>
#ifdef __GNUC__
#include <sys/file.h>
#else
#include <file.h>
#endif
#undef F_SETFL
#ifndef RAB$C_BID
#include <rab.h>
#endif
#define	MAXIOSIZE ( 32 * PAGESIZE )	/* Don't I/O more than 32 blocks at a time */
#endif /* VMS */

#ifndef BSD4_1
#ifdef BSD /* this is done this way to avoid defined (BSD) || defined (USG)
	      because the vms compiler doesn't grok `defined' */
#include <fcntl.h>
#endif /* BSD */
#ifdef USG5_4
#undef FASYNC
#include <sys/file.h>
#include <sys/filio.h>
#endif /* USG5_4 */
#ifdef USG
#ifndef USG5
#include <fcntl.h>
#endif /* not USG5 */
#endif /* USG */
#endif /* not 4.1 bsd */

#ifdef BROKEN_FASYNC
/* On some systems (DGUX comes to mind real fast) FASYNC causes
   background writes to the terminal to stop all processes in the
   process group when invoked under the csh (and probably any shell
   with job control). This stops Emacs dead in its tracks when coming
   up under X11. */
#undef FASYNC
#endif

#include <sys/ioctl.h>
#include "systty.h"
#include "syswait.h"

#ifdef BROKEN_TIOCGWINSZ
#undef TIOCGWINSZ
#endif

#ifdef USG
#include <sys/utsname.h>
#include <string.h>
#ifndef MEMORY_IN_STRING_H
#include <memory.h>
#endif
#ifdef TIOCGWINSZ
#ifdef NEED_SIOCTL
#include <sys/sioctl.h>
#endif
#ifdef NEED_PTEM_H
#include <sys/stream.h>
#include <sys/ptem.h>
#endif
#endif /* TIOCGWINSZ */
#endif /* USG */

#include "dispextern.h"
#include "screen.h"
#include "window.h"
#include "termhooks.h"
#include "termchar.h"
#include "termopts.h"
#include "process.h"

#ifdef NONSYSTEM_DIR_LIBRARY
#include "ndir.h"
#endif /* NONSYSTEM_DIR_LIBRARY */

#include "syssignal.h"
#include "systime.h"
#ifdef HPUX
#include <utime.h>
#endif

static int baud_convert[] =
#ifdef BAUD_CONVERT
  BAUD_CONVERT;
#else
  {
    0, 50, 75, 110, 135, 150, 200, 300, 600, 1200,
    1800, 2400, 4800, 9600, 19200, 38400
  };
#endif

#ifdef HAVE_TERMIOS
extern speed_t ospeed;
#else
extern short ospeed;
#endif

/* The file descriptor for Emacs's input terminal.
   Under Unix, this is always left zero;
   under VMS, we place the input channel number here.
   This allows us to write more code that works for both VMS and Unix.  */
static int input_fd;

#ifdef VMS
static struct iosb
{
  short status;
  short offset;
  short termlen;
  short term;
} input_iosb;

static void kbd_input_ast (void);

static int vms_waiting_for_ast;
static int vms_stop_input;
static int vms_input_ef = 0;
static int vms_timer_ef = 0;
static int vms_process_ef = 0;
static int vms_input_eflist;
static int vms_timer_eflist;

static $DESCRIPTOR (vms_input_dsc, "TT");
static int vms_terminator_mask[2] = { 0, 0 };

struct vms_sensemode {
  short status;
  unsigned char xmit_baud;
  unsigned char rcv_baud;
  unsigned char crfill;
  unsigned char lffill;
  unsigned char parity;
  unsigned char unused;
  char class;
  char type;
  short scr_wid;
  unsigned long tt_char : 24, scr_len : 8;
  unsigned long tt2_char;
};
#endif /* VMS */

void
discard_tty_input ()
{
  struct emacs_tty buf;

  if (noninteractive)
    return;

  /* Discarding input is not safe when the input could contain
     replies from the X server.  So don't do it.  */
  if (read_socket_hook)
    return;

#ifdef VMS
  end_kbd_input ();
  SYS$QIOW (0, input_fd, IO$_READVBLK|IO$M_PURGE, vms_input_iosb, 0, 0,
	    &buf.main, 0, 0, vms_terminator_mask, 0, 0);
  queue_vms_kbd_input ();
#else /* not VMS */
#ifdef APOLLO
  {
    int zero = 0;
    ioctl (0, TIOCFLUSH, &zero);
  }
#else /* not Apollo */
  EMACS_GET_TTY (input_fd, &buf);
  EMACS_SET_TTY (input_fd, &buf, 0);
#endif /* not Apollo */
#endif /* not VMS */
}

#ifdef SIGTSTP

void
stuff_char (int c)
{
/* Should perhaps error if in batch mode */
#ifdef TIOCSTI
  ioctl (0, TIOCSTI, &c);
#else /* no TIOCSTI */
  error ("Cannot stuff terminal input characters in this version of Unix.");
#endif /* no TIOCSTI */
}

#endif /* SIGTSTP */

void
init_baud_rate ()
{
  if (noninteractive)
    ospeed = 0;
  else
    {
#ifdef VMS
      struct vms_sensemode sg;

      SYS$QIOW (0, input_fd, IO$_SENSEMODE, &sg, 0, 0,
		&sg.class, 12, 0, 0, 0, 0 );
      ospeed = sg.xmit_baud;
#else /* not VMS */
#ifdef HAVE_TERMIOS
      struct termios sg;

      sg.c_cflag = (sg.c_cflag & ~CBAUD) | B9600;
      tcgetattr (0, &sg);
      ospeed = cfgetospeed (&sg);
#else /* neither VMS nor TERMIOS */
#ifdef HAVE_TERMIO
      struct termio sg;

      sg.c_cflag = (sg.c_cflag & ~CBAUD) | B9600;
#ifdef HAVE_TCATTR
      tcgetattr (0, &sg);
#else
      ioctl (input_fd, TCGETA, &sg);
#endif
      ospeed = sg.c_cflag & CBAUD;
#else /* neither VMS nor TERMIOS nor TERMIO */
      struct sgttyb sg;
      
      sg.sg_ospeed = B9600;
      if (ioctl (0, TIOCGETP, &sg) < 0)
	abort ();
      ospeed = sg.sg_ospeed;
#endif /* not HAVE_TERMIO */
#endif /* not HAVE_TERMIOS */
#endif /* not VMS */
    }
  
  baud_rate = (ospeed < sizeof baud_convert / sizeof baud_convert[0]
	       ? baud_convert[ospeed] : 9600);
  if (baud_rate == 0)
    baud_rate = 1200;
}

/*ARGSUSED*/
void
set_exclusive_use (int fd)
{
#ifdef FIOCLEX
  ioctl (fd, FIOCLEX, 0);
#endif
  /* Ok to do nothing if this feature does not exist */
}

#ifndef subprocesses

#ifdef BSD
void
wait_without_blocking ()
{
  wait3 (0, WNOHANG | WUNTRACED, 0);
  synch_process_alive = 0;
}
#endif /* BSD */

#endif /* not subprocesses */

int wait_debugging;   /* Set nonzero to make following function work under dbx
		         (at least for bsd).  */

#if 0 /* Unused */
SIGTYPE
wait_for_termination_signal (int ignore)
{
  SIGRETURN;
}
#endif

/* Wait for subprocess with process id `pid' to terminate and
   make sure it will get eliminated (not remain forever as a zombie) */

void
wait_for_termination (int pid)
{
  while (1)
    {
#ifdef subprocesses
#ifdef VMS
      int status;

      status = SYS$FORCEX (&pid, 0, 0);
      break;
#else /* not VMS */

#if defined (BSD) || (defined (HPUX) && !defined (HPUX_5))
      /* Note that kill returns -1 even if the process is just a zombie now.
	 But inevitably a SIGCHLD interrupt should be generated
	 and child_sig will do wait3 and make the process go away. */
      /* There is some indication that there is a bug involved with
	 termination of subprocesses, perhaps involving a kernel bug too,
	 but no idea what it is.  Just as a hunch we signal SIGCHLD to see
	 if that causes the problem to go away or get worse.  */
      sigsetmask (sigmask (SIGCHLD));
      if (0 > kill (pid, 0))
        {
	  sigsetmask (SIGEMPTYMASK);
	  kill (getpid (), SIGCHLD);
	  break;
	}
      if (wait_debugging)
	sleep (1);
      else
	sigpause (SIGEMPTYMASK);
#else /* not BSD, and not HPUX version >= 6 */
#if defined (UNIPLUS)
      if (0 > kill (pid, 0))
	break;
      wait (0);
#else /* neither BSD nor UNIPLUS: random sysV */
#ifdef POSIX_SIGNALS            /* would this work for LINUX as well? */
      sigblock (sigmask (SIGCHLD));
      if (0 > kill (pid, 0))
	{
	  sigunblock (sigmask (SIGCHLD));
	  break;
	}
      sigpause (SIGEMPTYMASK);
#else /* not POSIX_SIGNALS */
#ifdef HAVE_SYSV_SIGPAUSE
      sighold (SIGCHLD);
      if (0 > kill (pid, 0))
	{
	  sigrelse (SIGCHLD);
	  break;
	}
#ifdef SOLARIS2
#undef sigpause
#endif
      sigpause (SIGCHLD);
#else /* not HAVE_SYSV_SIGPAUSE */
      if (0 > kill (pid, 0))
	break;
      /* Using sleep instead of pause avoids timing error.
	 If the inferior dies just before the sleep,
	 we lose just one second.  */
      sleep (1);
#endif /* not HAVE_SYSV_SIGPAUSE */
#endif /* not POSIX_SIGNALS */
#endif /* not UNIPLUS */
#endif /* not BSD, and not HPUX version >= 6 */
#endif /* not VMS */
#else /* not subprocesses */
#ifndef BSD4_1
      if (kill (pid, 0) < 0)
	break;
      wait (0);
#else /* BSD4_1 */
      int status;
      status = wait (0);
      if (status == pid || status == -1)
	break;
#endif /* BSD4_1 */
#endif /* not subprocesses */
    }
}


#ifdef subprocesses

/*
 *	flush any pending output
 *      (may flush input as well; it does not matter the way we use it)
 */
 
void
flush_pending_output (channel)
     int channel;
{
#ifdef HAVE_TERMIOS
  /* If we try this, we get hit with SIGTTIN, because
     the child's tty belongs to the child's pgrp. */
#else
#ifdef TCFLSH
  ioctl (channel, TCFLSH, 1);
#else
#ifdef TIOCFLUSH
  int zero = 0;
  /* 3rd arg should be ignored
     but some 4.2 kernels actually want the address of an int
     and nonzero means something different.  */
  ioctl (channel, TIOCFLUSH, &zero);
#endif
#endif
#endif
}

#ifndef VMS
/*  Set up the terminal at the other end of a pseudo-terminal that
    we will be controlling an inferior through.
    It should not echo or do line-editing, since that is done
    in Emacs.  No padding needed for insertion into an Emacs buffer.  */

void
child_setup_tty (out)
     int out;
{
  struct emacs_tty s;

  EMACS_GET_TTY (out, &s);

#if defined (HAVE_TERMIO) || defined (HAVE_TERMIOS)
  s.main.c_oflag |= OPOST;	/* Enable output postprocessing */
  s.main.c_oflag &= ~ONLCR;	/* Disable map of NL to CR-NL on output */
  s.main.c_oflag &= ~(NLDLY|CRDLY|TABDLY|BSDLY|VTDLY|FFDLY);
  				/* No output delays */
  s.main.c_lflag &= ~ECHO;	/* Disable echo */
  s.main.c_lflag |= ISIG;	/* Enable signals */
  s.main.c_iflag &= ~IUCLC;	/* Disable map of upper case to lower on
				   input */
  s.main.c_oflag &= ~OLCUC;	/* Disable map of lower case to upper on
				   output */
#if 0
  /* Said to be unnecessary:  */
  s.main.c_cc[VMIN] = 1;	/* minimum number of characters to accept  */
  s.main.c_cc[VTIME] = 0;	/* wait forever for at least 1 character  */
#endif

  s.main.c_lflag |= ICANON;	/* Enable erase/kill and eof processing */
  s.main.c_cc[VEOF] = 04;	/* insure that EOF is Control-D */
  s.main.c_cc[VERASE] = 0377;	/* disable erase processing */
  s.main.c_cc[VKILL] = 0377;	/* disable kill processing */

#ifdef HPUX
  s.main.c_cflag = (s.main.c_cflag & ~CBAUD) | B9600; /* baud rate sanity */
#endif /* HPUX */

#ifdef AIX
/* AIX enhanced edit loses NULs, so disable it */
#ifndef IBMR2AIX
  s.main.c_line = 0;
  s.main.c_iflag &= ~ASCEDIT;
#endif
  /* Also, PTY overloads NUL and BREAK.
     don't ignore break, but don't signal either, so it looks like NUL.  */
  s.main.c_iflag &= ~IGNBRK;
  s.main.c_iflag &= ~BRKINT;
  /* QUIT and INTR work better as signals, so disable character forms */
  s.main.c_cc[VINTR] = 0377;
#ifdef SIGNALS_VIA_CHARACTERS
  /* the QUIT and INTR character are used in process_send_signal
     so set them here to something useful.  */
  if (s.main.c_cc[VQUIT] == 0377)
    s.main.c_cc[VQUIT] = '\\'&037;	/* Control-\ */
  if (s.main.c_cc[VINTR] == 0377)
    s.main.c_cc[VINTR] = 'C'&037;	/* Control-C */
#else /* no TIOCGPGRP or no TIOCGLTC or no TIOCGETC */
  /* QUIT and INTR work better as signals, so disable character forms */
  s.main.c_cc[VQUIT] = 0377;
  s.main.c_cc[VINTR] = 0377;
  s.main.c_lflag &= ~ISIG;
#endif /* no TIOCGPGRP or no TIOCGLTC or no TIOCGETC */
  s.main.c_cc[VEOL] = 0377;
  s.main.c_cflag = (s.main.c_cflag & ~CBAUD) | B9600; /* baud rate sanity */
#endif /* AIX */

#else /* not HAVE_TERMIO */

  s.main.sg_flags &= ~(ECHO | CRMOD | ANYP | ALLDELAY | RAW | LCASE
		       | CBREAK | TANDEM);
  s.main.sg_erase = 0377;
  s.main.sg_kill = 0377;

#endif /* not HAVE_TERMIO */

  EMACS_SET_TTY (out, &s, 0);

#ifdef BSD4_1
  if (interrupt_input)
    reset_sigio ();
#endif /* BSD4_1 */
#ifdef RTU
  {
    int zero = 0;
    ioctl (out, FIOASYNC, &zero);
  }
#endif /* RTU */
}
#endif /* not VMS */

#endif /* subprocesses */

/*ARGSUSED*/
void
setpgrp_of_tty (pid)
     int pid;
{
  EMACS_SET_TTY_PGRP (input_fd, &pid);
}

#ifndef VMS
#ifndef SIGTSTP
#ifndef USG_JOBCTRL

/* Record a signal code and the handler for it.  */
struct save_signal
{
  int code;
  SIGTYPE (*handler) ();
};

static void
save_signal_handlers (saved_handlers)
     struct save_signal *saved_handlers;
{
  while (saved_handlers->code)
    {
      saved_handlers->handler
	= (SIGTYPE (*) ()) signal (saved_handlers->code, SIG_IGN);
      saved_handlers++;
    }
}

static void
restore_signal_handlers (saved_handlers)
     struct save_signal *saved_handlers;
{
  while (saved_handlers->code)
    {
      signal (saved_handlers->code, saved_handlers->handler);
      saved_handlers++;
    }
}
#endif /* no USG_JOBCTRL */
#endif /* no SIGTSTP */
#endif /* not VMS */


/* Suspend the Emacs process; give terminal to its superior.  */

void
sys_suspend ()
{
#ifdef VMS
  /* "Foster" parentage allows emacs to return to a subprocess that attached
     to the current emacs as a cheaper than starting a whole new process.  This
     is set up by KEPTEDITOR.COM.  */
  unsigned long parent_id, foster_parent_id;
  char *fpid_string;

  fpid_string = getenv ("EMACS_PARENT_PID");
  if (fpid_string != NULL)
    {
      sscanf (fpid_string, "%x", &foster_parent_id);
      if (foster_parent_id != 0)
	parent_id = foster_parent_id;
      else
	parent_id = getppid ();
    }
  else
    parent_id = getppid ();

  xfree (fpid_string);		/* On VMS, this was malloc'd */

  if (parent_id && parent_id != 0xffffffff)
    {
      SIGTYPE (*oldsig)() = (int) signal (SIGINT, SIG_IGN);
      int status = LIB$ATTACH (&parent_id) & 1;
      signal (SIGINT, oldsig);
      return status;
    }
  else
    {
      struct {
	int	l;
	char	*a;
      } d_prompt;
      d_prompt.l = sizeof ("Emacs: ");		/* Our special prompt */
      d_prompt.a = "Emacs: ";			/* Just a reminder */
      LIB$SPAWN (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &d_prompt, 0);
      return 1;
    }
  return -1;
#else /* not VMS */
#ifdef SIGTSTP

  {
#ifdef USG
    int pgrp = getpgrp ();
#else
#ifdef OSF1
    int pgrp = getpgrp ();
#else
    int pgrp = getpgrp (0);
#endif
#endif
    EMACS_KILLPG (pgrp, SIGTSTP);
  }

#else /* No SIGTSTP */
#ifdef USG_JOBCTRL /* If you don't know what this is don't mess with it */
  ptrace (0, 0, 0, 0);		/* set for ptrace - caught by csh */
  kill (getpid (), SIGQUIT);

#else /* No SIGTSTP or USG_JOBCTRL */

/* On a system where suspending is not implemented,
   instead fork a subshell and let it talk directly to the terminal
   while we wait.  */
  int pid = fork ();
  struct save_signal saved_handlers[5];

  saved_handlers[0].code = SIGINT;
  saved_handlers[1].code = SIGQUIT;
  saved_handlers[2].code = SIGTERM;
#ifdef SIGIO
  saved_handlers[3].code = SIGIO;
  saved_handlers[4].code = 0;
#else
  saved_handlers[3].code = 0;
#endif

  if (pid == -1)
    error ("Can't spawn subshell");
  if (pid == 0)
    {
      char *sh;

      sh = (char *) egetenv ("SHELL");

      if (sh == 0)
	sh = "sh";

      /* Use our buffer's default directory for the subshell.  */
      {
        Lisp_Object dir;
        unsigned char *str;
        int len;

        /* mentioning current_buffer->buffer would mean including buffer.h,
           which somehow wedges the hp compiler.  So instead... */

        if (NILP (Fboundp (Qdefault_directory)))
          goto xyzzy;
        dir = Fsymbol_value (Qdefault_directory);
        if (!STRINGP (dir))
          goto xyzzy;

        str = (unsigned char *) alloca (XSTRING (dir)->size + 2);
        len = XSTRING (dir)->size;
        memcpy (str, XSTRING (dir)->data, len);
        /* >>>> Unix specific */
        if (str[len - 1] != '/') str[len++] = '/';
        str[len] = 0;

        chdir (str);
      }

    xyzzy:
      close_process_descs ();	/* Close Emacs's pipes/ptys */
#ifdef PRIO_PROCESS
      if (emacs_priority != 0)
        nice (-emacs_priority); /* Give the new shell the default priority */ 
#endif
      execlp (sh, sh, 0);
      write (1, "Can't execute subshell", 22);
      _exit (1);
    }

  save_signal_handlers (saved_handlers);
  synch_process_alive = 1;
  wait_for_termination (pid);
  restore_signal_handlers (saved_handlers);

#endif /* no USG_JOBCTRL */
#endif /* no SIGTSTP */
#endif /* not VMS */
}


#ifdef F_SETFL

#ifdef HAVE_X_WINDOWS
extern int x_file_descriptor;
#endif

#ifdef FASYNC
static int old_fcntl_flags;
#endif

void
init_sigio ()
{
#ifdef FASYNC
#ifdef HAVE_X_WINDOWS
  if (x_file_descriptor)
    old_fcntl_flags = fcntl (x_file_descriptor, F_GETFL, 0) & ~FASYNC;
  else
#else
    old_fcntl_flags = fcntl (0, F_GETFL, 0) & ~FASYNC;
#endif
#endif
  request_sigio ();
}

void
reset_sigio ()
{
  unrequest_sigio ();
}

#ifdef FASYNC		/* F_SETFL does not imply existence of FASYNC */

void
request_sigio ()
{
#ifdef SIGWINCH
  sigunblock (sigmask (SIGWINCH));
#endif
#ifdef HAVE_X_WINDOWS
  if (x_file_descriptor)
    fcntl (x_file_descriptor, F_SETFL, old_fcntl_flags | FASYNC);
  else
#else
    fcntl (0, F_SETFL, old_fcntl_flags | FASYNC);
#endif
  interrupts_deferred = 0;
}

void
unrequest_sigio ()
{
#ifdef SIGWINCH
  sigblock (sigmask (SIGWINCH));
#endif
#ifdef HAVE_X_WINDOWS
  if (x_file_descriptor)
    fcntl (x_file_descriptor, F_SETFL, old_fcntl_flags);
  else
#else
    fcntl (0, F_SETFL, old_fcntl_flags);
#endif
  interrupts_deferred = 1;
}

#else /* no FASYNC */
#if defined(STRIDE) || defined(HPUX)	/* Stride doesn't have FASYNC - use FIOASYNC */

void
request_sigio ()
{
  int on = 1;

#ifdef HAVE_X_WINDOWS
  if (x_file_descriptor)
    ioctl (x_file_descriptor, FIOASYNC, &on);
  else
#endif /*  HAVE_X_WINDOWS */
#ifdef HPUX                     /* >>>>???? */
    ioctl (0, FIOSSAIOSTAT, &on);
#else /* !HPUX */
    ioctl (0, FIOASYNC, &on);
#endif /* !HPUX */

  interrupts_deferred = 0;
}

void
unrequest_sigio ()
{
  int off = 0;

#ifdef HAVE_X_WINDOWS
  if (x_file_descriptor)
    ioctl (x_file_descriptor, FIOASYNC, &off);
  else
#endif /*  HAVE_X_WINDOWS */
#ifdef HPUX
    ioctl (0, FIOSSAIOSTAT, &off); /* >>> ???? */
#else /* !HPUX */
    ioctl (0, FIOASYNC, &off);
#endif /* !HPUX */

  interrupts_deferred = 1;
}

#else /* not FASYNC, not STRIDE or HPUX */
 
 
#endif /* STRIDE */
#endif /* FASYNC */
#endif /* F_SETFL */

/* Saving and restoring the process group of Emacs's terminal.  */

#ifdef BSD

/* The process group of which Emacs was a member when it initially
   started.

   If Emacs was in its own process group (i.e. inherited_pgroup ==
   getpid ()), then we know we're running under a shell with job
   control (Emacs would never be run as part of a pipeline).
   Everything is fine.

   If Emacs was not in its own process group, then we know we're
   running under a shell (or a caller) that doesn't know how to
   separate itself from Emacs (like sh).  Emacs must be in its own
   process group in order to receive SIGIO correctly.  In this
   situation, we put ourselves in our own pgroup, forcibly set the
   tty's pgroup to our pgroup, and make sure to restore and reinstate
   the tty's pgroup just like any other terminal setting.  If
   inherited_group was not the tty's pgroup, then we'll get a
   SIGTTmumble when we try to change the tty's pgroup, and a CONT if
   it goes foreground in the future, which is what should happen.  */
int inherited_pgroup;

/* Split off the foreground process group to Emacs alone.
   When we are in the foreground, but not started in our own process
   group, redirect the TTY to point to our own process group.  We need
   to be in our own process group to receive SIGIO properly.  */
void
narrow_foreground_group ()
{
  int me = getpid ();

  setpgrp (0, inherited_pgroup);
  if (inherited_pgroup != me)
    EMACS_SET_TTY_PGRP (0, &me);
  setpgrp (0, me);
}

/* Set the tty to our original foreground group.  */
void
widen_foreground_group ()
{
  if (inherited_pgroup != getpid ())
    EMACS_SET_TTY_PGRP (0, &inherited_pgroup);
  setpgrp (0, inherited_pgroup);
}

#endif

/* Getting and setting emacs_tty structures.  */

/* Set *TC to the parameters associated with the terminal FD.
   Return zero if all's well, or -1 if we ran into an error we
   couldn't deal with.  */
int
emacs_get_tty (int fd, struct emacs_tty *settings)
{
  /* Retrieve the primary parameters - baud rate, character size, etcetera.  */
#ifdef HAVE_TCATTR
  /* We have those nifty POSIX tcmumbleattr functions.  */
  if (tcgetattr (fd, &settings->main) < 0)
    return -1;

#else
#ifdef HAVE_TERMIO
  /* The SYSV-style interface?  */
  if (ioctl (fd, TCGETA, &settings->main) < 0)
    return -1;

#else
#ifdef VMS
  /* Vehemently Monstrous System?  :-)  */
  if (! (SYS$QIOW (0, fd, IO$_SENSEMODE, settings, 0, 0,
		   &settings->main.class, 12, 0, 0, 0, 0)
	 & 1))
    return -1;

#else
  /* I give up - I hope you have the BSD ioctls.  */
  if (ioctl (fd, TIOCGETP, &settings->main) < 0)
    return -1;

#endif
#endif
#endif

  /* Suivant - Do we have to get struct ltchars data?  */
#ifdef HAVE_LTCHARS
  if (ioctl (fd, TIOCGLTC, &settings->ltchars) < 0)
    return -1;
#endif

  /* How about a struct tchars and a wordful of lmode bits?  */
#ifdef HAVE_TCHARS
  if (ioctl (fd, TIOCGETC, &settings->tchars) < 0
      || ioctl (fd, TIOCLGET, &settings->lmode) < 0)
    return -1;
#endif

  /* We have survived the tempest.  */
  return 0;
}


/* Set the parameters of the tty on FD according to the contents of
   *SETTINGS.  If WAITP is non-zero, we wait for all queued output to
   be written before making the change; otherwise, we forget any
   queued input and make the change immediately.
   Return 0 if all went well, and -1 if anything failed.  */
int
emacs_set_tty (int fd, struct emacs_tty *settings, int waitp)
{
  /* Set the primary parameters - baud rate, character size, etcetera.  */
#ifdef HAVE_TCATTR
  int i;
  /* We have those nifty POSIX tcmumbleattr functions.
     William J. Smith <wjs@wiis.wang.com> writes:
     "POSIX 1003.1 defines tcsetattr() to return success if it was
     able to perform any of the requested actions, even if some
     of the requested actions could not be performed.
     We must read settings back to ensure tty setup properly.
     AIX requires this to keep tty from hanging occasionally."  */
  /* This makes sure that we don't loop indefinitely in here.  */
  for (i = 0 ; i < 10 ; i++)
    if (tcsetattr (fd, waitp ? TCSAFLUSH : TCSADRAIN, &settings->main) < 0)
      {
	if (errno == EINTR)
	  continue;
	else
	  return -1;
      }
    else
      {
	struct termios new;

	/* Get the current settings, and see if they're what we asked for.  */
	tcgetattr (fd, &new);
	/* We cannot use memcmp on the whole structure here because under
	 * aix386 the termios structure has some reserved field that may
	 * not be filled in.
	 */
	if (   new.c_iflag == settings->main.c_iflag
	    && new.c_oflag == settings->main.c_oflag
	    && new.c_cflag == settings->main.c_cflag
	    && new.c_lflag == settings->main.c_lflag
	    && memcmp(new.c_cc, settings->main.c_cc, NCCS) == 0)
	  break;
	else
	  continue;
      }

#else
#ifdef HAVE_TERMIO
  /* The SYSV-style interface?  */
  if (ioctl (fd, waitp ? TCSETAW : TCSETAF, &settings->main) < 0)
    return -1;

#else
#ifdef VMS
  /* Vehemently Monstrous System?  :-)  */
  if (! (SYS$QIOW (0, fd, IO$_SETMODE, &input_iosb, 0, 0,
		   &settings->main.class, 12, 0, 0, 0, 0)
	 & 1))
    return -1;

#else
  /* I give up - I hope you have the BSD ioctls.  */
  if (ioctl (fd, (waitp) ? TIOCSETP : TIOCSETN, &settings->main) < 0)
    return -1;

#endif
#endif
#endif

  /* Suivant - Do we have to get struct ltchars data?  */
#ifdef HAVE_LTCHARS
  if (ioctl (fd, TIOCSLTC, &settings->ltchars) < 0)
    return -1;
#endif

  /* How about a struct tchars and a wordful of lmode bits?  */
#ifdef HAVE_TCHARS
  if (ioctl (fd, TIOCSETC, &settings->tchars) < 0
      || ioctl (fd, TIOCLSET, &settings->lmode) < 0)
    return -1;
#endif
  
  /* We have survived the tempest.  */
  return 0;
}


/* The initial tty mode bits */
static struct emacs_tty old_tty;

static int term_initted;        /* 1 if outer tty status has been recorded */

#ifdef BSD4_1
/* BSD 4.1 needs to keep track of the lmode bits in order to start
   sigio.  */
int lmode;
#endif

#ifndef F_SETOWN_BUG
#if defined(F_SETOWN) || defined(HPUX)
int old_fcntl_owner;
#endif /* F_SETOWN || HPUX */
#endif /* F_SETOWN_BUG */

/* This may also be defined in stdio,
   but if so, this does no harm,
   and using the same name avoids wasting the other one's space.  */

#if ((defined(USG) || defined(DGUX)) && !defined(__STDC__))
char _sobuf[BUFSIZ+8];
#elif defined(sun) && defined(USG)
extern unsigned char _sobuf[BUFSIZ+8];
#else
char _sobuf[BUFSIZ];
#endif
 
#ifdef TIOCGLTC /* HAVE_LTCHARS */
static struct ltchars new_ltchars = {-1,-1,-1,-1,-1,-1};
#endif
#ifdef TIOCGETC /* HAVE_TCHARS */
static struct tchars new_tchars = {-1,-1,-1,-1,-1,-1};
#endif 

void
init_sys_modes ()
{
  struct emacs_tty tty;

#ifdef VMS
#if 0
  static int oob_chars[2] = {0, 1 << 7}; /* catch C-g's */
  extern int (*interrupt_signal) ();
#endif
#endif

  if (noninteractive)
    return;

#ifdef VMS
  if (!vms_input_ef)
    vms_input_ef = get_kbd_event_flag ();
    /* LIB$GET_EF (&vms_input_ef); */
  SYS$CLREF (vms_input_ef);
  vms_waiting_for_ast = 0;
  if (!vms_timer_ef)
    vms_timer_ef = get_timer_event_flag ();
    /* LIB$GET_EF (&vms_timer_ef); */
  SYS$CLREF (vms_timer_ef);
#if 0
  if (!vms_process_ef)
    {
      LIB$GET_EF (&vms_process_ef);
      SYS$CLREF (vms_process_ef);
    }
  if (vms_input_ef / 32 != vms_process_ef / 32)
    croak ("Input and process event flags in different clusters.");
#endif
  if (vms_input_ef / 32 != vms_timer_ef / 32)
    croak ("Input and timer event flags in different clusters.");
#if 0
  vms_input_eflist = ((unsigned) 1 << (vms_input_ef % 32)) |
    ((unsigned) 1 << (vms_process_ef % 32));
#endif
  vms_timer_eflist = ((unsigned) 1 << (vms_input_ef % 32)) |
    ((unsigned) 1 << (vms_timer_ef % 32));
#ifndef VMS4_4
  sys_access_reinit ();
#endif
#endif /* not VMS */

#ifdef BSD
  if (! read_socket_hook && EQ (Vwindow_system, Qnil))
    narrow_foreground_group ();
#endif

  EMACS_GET_TTY (input_fd, &old_tty);

  if (!read_socket_hook && EQ (Vwindow_system, Qnil))
    {
      tty = old_tty;

#if defined (HAVE_TERMIO) || defined (HAVE_TERMIOS)
      tty.main.c_iflag |= (IGNBRK);	/* Ignore break condition */
      tty.main.c_iflag &= ~ICRNL;	/* Disable map of CR to NL on input */
#ifdef ISTRIP
      tty.main.c_iflag &= ~ISTRIP;	/* don't strip 8th bit on input */
#endif
      tty.main.c_lflag &= ~ECHO;	/* Disable echo */
      tty.main.c_lflag &= ~ICANON;	/* Disable erase/kill processing */
#ifdef IEXTEN
      tty.main.c_iflag &= ~IEXTEN;	/* Disable other editing characters.  */
#endif
      tty.main.c_lflag |= ISIG;	/* Enable signals */
      if (flow_control)
	{
	  tty.main.c_iflag |= IXON;	/* Enable start/stop output control */
#ifdef IXANY
	  tty.main.c_iflag &= ~IXANY;
#endif /* IXANY */
	}
      else
	tty.main.c_iflag &= ~IXON;	/* Disable start/stop output control */
      tty.main.c_oflag &= ~ONLCR;	/* Disable map of NL to CR-NL 
					   on output */
      tty.main.c_oflag &= ~TAB3;	/* Disable tab expansion */
#ifdef CS8
      if (meta_key)
	{
	  tty.main.c_cflag |= CS8;	/* allow 8th bit on input */
	  tty.main.c_cflag &= ~PARENB;/* Don't check parity */
	}
#endif
      tty.main.c_cc[VINTR] = interrupt_char; /* C-g (usually) gives SIGINT */
      /* Set up C-g for both SIGQUIT and SIGINT.
	 We don't know which we will get, but we handle both alike
	 so which one it really gives us does not matter.  */
      tty.main.c_cc[VQUIT] = interrupt_char;
      tty.main.c_cc[VMIN] = 1;	/* Input should wait for at least 1 char */
      tty.main.c_cc[VTIME] = 0;	/* no matter how long that takes.  */
#ifdef VSWTCH
      tty.main.c_cc[VSWTCH] = CDISABLE;	/* Turn off shell layering use
					   of C-z */
#endif /* VSWTCH */
#if defined (mips) || defined (HAVE_TCATTR)
#ifdef VSUSP
      tty.main.c_cc[VSUSP] = CDISABLE;	/* Turn off mips handling of C-z.  */
#endif /* VSUSP */
#ifdef V_DSUSP
      tty.main.c_cc[V_DSUSP] = CDISABLE; /* Turn off mips handling of C-y.  */
#endif /* V_DSUSP */
#ifdef VDSUSP /* Some systems have VDSUSP, some have V_DSUSP.  */
      tty.main.c_cc[VDSUSP] = CDISABLE;
#endif /* VDSUSP */
#ifdef VLNEXT
      tty.main.c_cc[VLNEXT] = CDISABLE;
#endif /* VLNEXT */
#ifdef VREPRINT
      tty.main.c_cc[VREPRINT] = CDISABLE;
#endif /* VREPRINT */
#ifdef VWERASE
      tty.main.c_cc[VWERASE] = CDISABLE;
#endif /* VWERASE */
#ifdef VDISCARD
      tty.main.c_cc[VDISCARD] = CDISABLE;
#endif /* VDISCARD */
#endif /* mips or HAVE_TCATTR */
#ifdef AIX
#ifndef IBMR2AIX
      /* AIX enhanced edit loses NULs, so disable it */
      tty.main.c_line = 0;
      tty.main.c_iflag &= ~ASCEDIT;
#else
      tty.main.c_cc[VSTRT] = 255;
      tty.main.c_cc[VSTOP] = 255;
      tty.main.c_cc[VSUSP] = 255;
      tty.main.c_cc[VDSUSP] = 255;
#endif /* IBMR2AIX */
      /* Also, PTY overloads NUL and BREAK.
	 don't ignore break, but don't signal either, so it looks like NUL.
	 This really serves a purpose only if running in an XTERM window
	 or via TELNET or the like, but does no harm elsewhere.  */
      tty.main.c_iflag &= ~IGNBRK;
      tty.main.c_iflag &= ~BRKINT;
#endif
#else /* if not HAVE_TERMIO */
#ifdef VMS
      tty.main.tt_char |= TT$M_NOECHO;
      if (meta_key)
	tty.main.tt_char |= TT$M_EIGHTBIT;
      if (flow_control)
	tty.main.tt_char |= TT$M_TTSYNC;
      else
	tty.main.tt_char &= ~TT$M_TTSYNC;
      tty.main.tt2_char |= TT2$M_PASTHRU | TT2$M_XON;
#else /* not VMS (BSD, that is) */
      tty.main.sg_flags &= ~(ECHO | CRMOD | XTABS);
      if (meta_key)
	tty.main.sg_flags |= ANYP;
      tty.main.sg_flags |= interrupt_input ? RAW : CBREAK;
#endif /* not VMS (BSD, that is) */
#endif /* not HAVE_TERMIO */

      /* If going to use CBREAK mode, we must request C-g to interrupt
	 and turn off start and stop chars, etc.  If not going to use
	 CBREAK mode, do this anyway so as to turn off local flow
	 control for user coming over network on 4.2; in this case,
	 only t_stopc and t_startc really matter.  */
#ifndef HAVE_TERMIO
#ifdef HAVE_TCHARS
      /* Note: if not using CBREAK mode, it makes no difference how we
	 set this */
      tty.tchars = new_tchars;
      tty.tchars.t_intrc = interrupt_char;
      if (flow_control)
	{
	  tty.tchars.t_startc = '\021';
	  tty.tchars.t_stopc = '\023';
	}

/* LPASS8 is new in 4.3, and makes cbreak mode provide all 8 bits.  */
#ifndef LPASS8
#define LPASS8 0
#endif

#ifdef BSD4_1
#define LNOFLSH 0100000
#endif

      tty.lmode = LDECCTQ | LLITOUT | LPASS8 | LNOFLSH | old_tty.lmode;
      
#ifdef ultrix
      /* Under Ultrix 4.2a, leaving this out doesn't seem to hurt
	 anything, and leaving it in breaks the meta key.  Go figure.  */
      tty.lmode &= ~LLITOUT;
#endif
      
#ifdef BSD4_1
      lmode = tty.lmode;
#endif

#endif /* HAVE_TCHARS */
#endif /* not HAVE_TERMIO */

#ifdef HAVE_LTCHARS
      tty.ltchars = new_ltchars;
#endif /* HAVE_LTCHARS */

      EMACS_SET_TTY (input_fd, &tty, 0);

      /* This code added to insure that, if flow-control is not to be used,
	 we have an unlocked terminal at the start. */

#ifdef TCXONC
      if (!flow_control) ioctl (0, TCXONC, 1);
#endif
#ifndef APOLLO
#ifdef TIOCSTART
      if (!flow_control) ioctl (0, TIOCSTART, 0);
#endif
#endif

#ifdef AIX
      hft_init ();
#ifdef IBMR2AIX
      {
	/* IBM's HFT device usually thinks a ^J should be LF/CR.  We need it
	   to be only LF.  This is the way that is done. */
	struct termio tty;

	if (ioctl (1, HFTGETID, &tty) != -1)
	  write (1, "\033[20l", 5);
      }
#endif
#endif

#ifdef VMS
/*  Appears to do nothing when in PASTHRU mode.
      SYS$QIOW (0, input_fd, IO$_SETMODE|IO$M_OUTBAND, 0, 0, 0,
		interrupt_signal, oob_chars, 0, 0, 0, 0);
*/
      queue_vms_kbd_input ();
#endif /* VMS */

#ifdef HPUX
      if (interrupt_input)
	{
	  int owner = getpid ();
	  ioctl (0, FIOGSAIOOWN, &old_fcntl_owner);
	  ioctl (0, FIOSSAIOOWN, &owner);
	  init_sigio ();
	}
#else /* !HPUX */

#ifdef F_SETFL
#ifndef F_SETOWN_BUG
#ifdef F_GETOWN		/* F_SETFL does not imply existence of F_GETOWN */
      if (interrupt_input)
	{
	  old_fcntl_owner = fcntl (0, F_GETOWN, 0);
	  fcntl (0, F_SETOWN, getpid ());
	  init_sigio ();
	}
#endif /* F_GETOWN */
#endif /* F_SETFL */
#endif /* !F_SETOWN_BUG */

#endif /* !HPUX */

#ifdef BSD4_1
      if (interrupt_input)
	init_sigio ();
#endif
    }

#ifdef VMS  /* VMS sometimes has this symbol but lacks setvbuf.  */
#undef _IOFBF
#endif
#ifdef _IOFBF
  /* This symbol is defined on recent USG systems.
     Someone says without this call USG won't really buffer the file
     even with a call to setbuf. */
  setvbuf (stdout, _sobuf, _IOFBF, sizeof _sobuf);
#else
  setbuf (stdout, _sobuf);
#endif
  set_terminal_modes ();
  if (term_initted && no_redraw_on_reenter)
    {
      if (display_completed)
	direct_output_forward_char (0);
    }
  else
    {
      screen_garbaged = 1;
#ifdef MULTI_SCREEN
      if (SCREENP (Vterminal_screen))
	SCREEN_GARBAGED_P (XSCREEN (Vterminal_screen)) = 1;
#endif
    }

  term_initted = 1;
}

/* Return nonzero if safe to use tabs in output.
   At the time this is called, init_sys_modes has not been done yet.  */
   
int
tabs_safe_p ()
{
  struct emacs_tty tty;

  EMACS_GET_TTY (input_fd, &tty);
  return EMACS_TTY_TABS_OK (&tty);
}

/* Get terminal size from system.
   Store number of lines into *heightp and width into *widthp.
   If zero or a negative number is stored, the value is not valid.  */

void
get_screen_size (widthp, heightp)
     int *widthp, *heightp;
{

#ifdef TIOCGWINSZ

  /* BSD-style.  */
  struct winsize size;

  if (ioctl (input_fd, TIOCGWINSZ, &size) == -1)
    *widthp = *heightp = 0;
  else
    {
      *widthp = size.ws_col;
      *heightp = size.ws_row;
    }

#else
#ifdef TIOCGSIZE

  /* SunOS - style.  */
  struct ttysize size;  

  if (ioctl (input_fd, TIOCGSIZE, &size) == -1)
    *widthp = *heightp = 0;
  else
    {
      *widthp = size.ts_cols;
      *heightp = size.ts_lines;
    }

#else
#ifdef VMS

  struct vms_sensemode tty;
  
  SYS$QIOW (0, input_fd, IO$_SENSEMODE, &tty, 0, 0,
	    &tty.class, 12, 0, 0, 0, 0);
  *widthp = tty.scr_wid;
  *heightp = tty.scr_len;

#else /* system doesn't know size */

  *widthp = 0;
  *heightp = 0;

#endif /* not VMS */
#endif /* not SunOS-style */
#endif /* not BSD-style */
}


/* Prepare the terminal for exiting Emacs; move the cursor to the
   bottom of the screen, turn off interrupt-driven I/O, etc.  */
void
reset_sys_modes ()
{
  if (noninteractive)
    {
      fflush (stdout);
      return;
    }
  if (!term_initted)
    return;
  if (read_socket_hook || !EQ (Vwindow_system, Qnil))
    return;
  /* >>>>> current_glyphs test is a gross temporary kludge
   * >>>>> whilst lemacs continues to not work with termcap screens.
   * >>>>> (Can get here if run temacs -l loadup and kill emacs
   * >>>>>  before calling initialize-first-screen */
  if (selected_screen->cur_line)
  {
    selected_screen->new_cur_line = selected_screen->new_cur_w->lines;
    selected_screen->new_cur_char = selected_screen->new_cur_line->body;
    selected_screen->cursor_x = selected_screen->cursor_y = 0;
    cursor_to (selected_screen->new_cur_line,
	       selected_screen->new_cur_char,
	       selected_screen->cursor_y,
	       selected_screen->cursor_x,
	       selected_screen->new_cur_w, selected_screen);	/* WRONG */
    /*  clear_end_of_line (selected_screen->width); */
    /* clear_end_of_line may move the cursor */
    cursor_to (selected_screen->new_cur_line,
	       selected_screen->new_cur_char,
	       selected_screen->cursor_y,
	       selected_screen->cursor_x,
	       selected_screen->new_cur_w, selected_screen);	/* WRONG */
    /* Output raw CR so kernel can track the cursor hpos.  */
  }
#ifdef IBMR2AIX
  {
    /* HFT devices normally use ^J as a LF/CR.  We forced it to 
       do the LF only.  Now, we need to reset it. */
    struct termio tty;

    if (ioctl (1, HFTGETID, &tty) != -1)
      write (1, "\033[20h", 5);
  }
#endif

  reset_terminal_modes ();
  fflush (stdout);
#ifdef BSD
#ifndef BSD4_1
  /* Avoid possible loss of output when changing terminal modes.  */
  fsync (fileno (stdout));
#endif
#endif

#ifdef HPUX
  if (interrupt_input)
    {
      ioctl (0, FIOSSAIOOWN, &old_fcntl_owner);
      reset_sigio ();
    }
#else /* !HPUX */
#ifdef F_SETFL
#ifndef F_SETOWN_BUG
#ifdef F_SETOWN		/* F_SETFL does not imply existence of F_SETOWN */
  if (interrupt_input)
    {
      reset_sigio ();
      fcntl (0, F_SETOWN, old_fcntl_owner);
    }
#endif /* F_SETOWN */
#endif /* F_SETFL */
#endif /* !F_SETOWN_BUG */
#endif /* !HPUX */

#ifdef BSD4_1
  if (interrupt_input)
    reset_sigio ();
#endif /* BSD4_1 */

  while (EMACS_SET_TTY (input_fd, &old_tty, 0) < 0 && errno == EINTR)
    ;

#ifdef AIX
  hft_reset ();
#endif

#ifdef BSD
  widen_foreground_group ();
#endif
}

#ifdef HAVE_PTYS

/* Set up the proper status flags for use of a pty.  */

void
setup_pty (fd)
     int fd;
{
  /* I'm told that TOICREMOTE does not mean control chars
     "can't be sent" but rather that they don't have
     input-editing or signaling effects.
     That should be good, because we have other ways
     to do those things in Emacs.
     However, telnet mode seems not to work on 4.2.
     So TIOCREMOTE is turned off now. */

  /* Under hp-ux, if TIOCREMOTE is turned on, some calls
     will hang.  In particular, the "timeout" feature (which
     causes a read to return if there is no data available)
     does this.  Also it is known that telnet mode will hang
     in such a way that Emacs must be stopped (perhaps this
     is the same problem).
     
     If TIOCREMOTE is turned off, then there is a bug in
     hp-ux which sometimes loses data.  Apparently the
     code which blocks the master process when the internal
     buffer fills up does not work.  Other than this,
     though, everything else seems to work fine.
     
     Since the latter lossage is more benign, we may as well
     lose that way.  -- cph */
#ifdef FIONBIO
#ifdef SYSV_PTYS
  {
    int on = 1;
    ioctl (fd, FIONBIO, &on);
  }
#endif
#endif
#ifdef IBMRTAIX
  /* On AIX, the parent gets SIGHUP when a pty attached child dies.  So, we */
  /* ignore SIGHUP once we've started a child on a pty.  Note that this may */
  /* cause EMACS not to die when it should, i.e., when its own controlling  */
  /* tty goes away.  I've complained to the AIX developers, and they may    */
  /* change this behavior, but I'm not going to hold my breath.             */
  signal (SIGHUP, SIG_IGN);
#endif
}
#endif /* HAVE_PTYS */

#ifdef VMS

/* Assigning an input channel is done at the start of Emacs execution.
   This is called each time Emacs is resumed, also, but does nothing
   because input_chain is no longer zero.  */

void
init_vms_input ()
{
  int status;
  
  if (input_fd == 0)
    {
      status = SYS$ASSIGN (&vms_input_dsc, &input_fd, 0, 0);
      if (! (status & 1))
	LIB$STOP (status);
    }
}

/* Deassigning the input channel is done before exiting.  */

void
stop_vms_input ()
{
  return SYS$DASSGN (input_fd);
}

static short vms_input_buffer;

/* Request reading one character into the keyboard buffer.
   This is done as soon as the buffer becomes empty.  */

static void
queue_vms_kbd_input ()
{
  int status;
  vms_waiting_for_ast = 0;
  vms_stop_input = 0;
  status = SYS$QIO (0, input_fd, IO$_READVBLK,
		    &vms_input_iosb, vms_kbd_input_ast, 1,
		    &vms_input_buffer, 1, 0, vms_terminator_mask, 0, 0);
}

static int vms_input_count;

/* Ast routine that is called when keyboard input comes in
   in accord with the SYS$QIO above.  */

static void
vms_kbd_input_ast ()
{
  register int c = -1;
  int old_errno = errno;
  extern EMACS_TIME *input_available_clear_time;

  if (vms_waiting_for_ast)
    SYS$SETEF (vms_input_ef);
  vms_waiting_for_ast = 0;
  vms_input_count++;
#ifdef ASTDEBUG
  if (vms_input_count == 25)
    exit (1);
  printf ("Ast # %d,", vms_input_count);
  printf (" iosb = %x, %x, %x, %x",
	  vms_input_iosb.offset, vms_input_iosb.status, 
          vms_input_iosb.termlen, vms_input_iosb.term);
#endif
  if (vms_input_iosb.offset)
    {
      c = vms_input_buffer;
#ifdef ASTDEBUG
      printf (", char = 0%o", c);
#endif
    }
#ifdef ASTDEBUG
  printf ("\n");
  fflush (stdout);
  sleep (1);
#endif
  if (! vms_stop_input)
    queue_vms_kbd_input ();
  if (c >= 0)
    kbd_buffer_store_char (c);

  if (input_available_clear_time)
    EMACS_SET_SECS_USECS (*input_available_clear_time, 0, 0);
  errno = old_errno;
}

#if 0 /* Unused */
/* Wait until there is something in kbd_buffer.  */

void
vms_wait_for_kbd_input ()
{
  extern int have_process_input, process_exited;

  /* If already something, avoid doing system calls.  */
  if (detect_input_pending ())
    {
      return;
    }
  /* Clear a flag, and tell ast routine above to set it.  */
  SYS$CLREF (vms_input_ef);
  vms_waiting_for_ast = 1;
  /* Check for timing error: ast happened while we were doing that.  */
  if (!detect_input_pending ())
    {
      /* No timing error: wait for flag to be set.  */
      set_waiting_for_input (0);
      SYS$WFLOR (vms_input_ef, vms_input_eflist);
      clear_waiting_for_input (0);
      if (!detect_input_pending ())
	/* Check for subprocess input availability */
	{
	  int dsp = have_process_input || process_exited;

	  SYS$CLREF (vms_process_ef);
	  if (have_process_input)
	    process_command_input ();
	  if (process_exited)
	    process_exit ();
	  if (dsp)
	    {
	      redraw_mode_line++;
	      redisplay_preserving_echo_area ();
	    }
	}
    }
  vms_waiting_for_ast = 0;
}
#endif

/* Get rid of any pending QIO, when we are about to suspend
   or when we want to throw away pending input.
   We wait for a positive sign that the AST routine has run
   and therefore there is no I/O request queued when we return.
   SYS$SETAST is used to avoid a timing error.  */

static void
vms_end_kbd_input ()
{
#ifdef ASTDEBUG
  printf ("At end_kbd_input.\n");
  fflush (stdout);
  sleep (1);
#endif
  if (LIB$AST_IN_PROG ())  /* Don't wait if suspending from kbd_buffer_store_char! */
    {
      SYS$CANCEL (input_fd);
      return;
    }

  SYS$SETAST (0);
  /* Clear a flag, and tell ast routine above to set it.  */
  SYS$CLREF (vms_input_ef);
  vms_waiting_for_ast = 1;
  vms_stop_input = 1;
  SYS$CANCEL (input_fd);
  SYS$SETAST (1);
  SYS$WAITFR (vms_input_ef);
  vms_waiting_for_ast = 0;
}

#if 0 /* Unused */
/* Wait for either input available or time interval expiry.  */

void
vms_input_wait_timeout (timeval)
     int timeval;		/* Time to wait, in seconds */
{
  int time [2];
  static int zero = 0;
  static int large = -10000000;

  LIB$EMUL (&timeval, &large, &zero, time); 	  /* Convert to VMS format */

  /* If already something, avoid doing system calls.  */
  if (detect_input_pending ())
    {
      return;
    }
  /* Clear a flag, and tell ast routine above to set it.  */
  SYS$CLREF (vms_input_ef);
  vms_waiting_for_ast = 1;
  /* Check for timing error: ast happened while we were doing that.  */
  if (!detect_input_pending ())
    {
      /* No timing error: wait for flag to be set.  */
      SYS$CANTIM (1, 0);
      if (SYS$SETIMR (vms_timer_ef, time, 0, 1) & 1) /* Set timer */
	SYS$WFLOR (vms_timer_ef, vms_timer_eflist);  /* Wait for timer expiry or input */
    }
  vms_waiting_for_ast = 0;
}
#endif /* 0 */


#if 0 /* Apparently unused */
/* The standard `sleep' routine works some other way
   and it stops working if you have ever quit out of it.
   This one continues to work.  */

void
sys_sleep (timeval)
     int timeval;
{
  int time [2];
  static int zero = 0;
  static int large = -10000000;

  LIB$EMUL (&timeval, &large, &zero, time); 	  /* Convert to VMS format */

  SYS$CANTIM (1, 0);
  if (SYS$SETIMR (vms_timer_ef, time, 0, 1) & 1) /* Set timer */
    SYS$WAITFR (vms_timer_ef);	  /* Wait for timer expiry only */
}
#endif /* 0 */

#endif /* VMS */

/* Note that VMS compiler won't accept defined (CANNOT_DUMP).  */
#ifndef CANNOT_DUMP
#define NEED_STARTS
#endif

#ifndef SYSTEM_MALLOC
#ifndef NEED_STARTS
#define NEED_STARTS
#endif
#endif

#ifdef NEED_STARTS
/* Some systems that cannot dump also cannot implement these.  */

/*
 *	Return the address of the start of the text segment prior to
 *	doing an unexec.  After unexec the return value is undefined.
 *	See crt0.c for further explanation and _start.
 *
 */

#ifndef CANNOT_UNEXEC
char *
start_of_text ()
{
#ifdef TEXT_START
  return ((char *) TEXT_START);
#else
#ifdef GOULD
  extern csrt ();
  return ((char *) csrt);
#else /* not GOULD */
  extern int _start ();
  return ((char *) _start);
#endif /* GOULD */
#endif /* TEXT_START */
}
#endif /* not CANNOT_UNEXEC */

/*
 *	Return the address of the start of the data segment prior to
 *	doing an unexec.  After unexec the return value is undefined.
 *	See crt0.c for further information and definition of data_start.
 *
 *	Apparently, on BSD systems this is etext at startup.  On
 *	USG systems (swapping) this is highly mmu dependent and
 *	is also dependent on whether or not the program is running
 *	with shared text.  Generally there is a (possibly large)
 *	gap between end of text and start of data with shared text.
 *
 *	On Uniplus+ systems with shared text, data starts at a
 *	fixed address.  Each port (from a given oem) is generally
 *	different, and the specific value of the start of data can
 *	be obtained via the UniPlus+ specific "uvar" system call,
 *	however the method outlined in crt0.c seems to be more portable.
 *
 *	Probably what will have to happen when a USG unexec is available,
 *	at least on UniPlus, is temacs will have to be made unshared so
 *	that text and data are contiguous.  Then once loadup is complete,
 *	unexec will produce a shared executable where the data can be
 *	at the normal shared text boundry and the startofdata variable
 *	will be patched by unexec to the correct value.
 *
 */
 
void *
start_of_data ()
{
#ifdef DATA_START
  return ((char *) DATA_START);
#else
#ifdef ORDINARY_LINK
  /*
   * This is a hack.  Since we're not linking crt0.c or pre_crt0.c,
   * data_start isn't defined.  We take the address of environ, which
   * is known to live at or near the start of the system crt0.c, and
   * we don't sweat the handful of bytes that might lose.
   */
  extern char **environ;

  return((char *) &environ);
#else
  extern int data_start;
  return ((char *) &data_start);
#endif /* ORDINARY_LINK */
#endif /* DATA_START */
}
#endif /* NEED_STARTS (not CANNOT_DUMP or not SYSTEM_MALLOC) */

#ifndef CANNOT_DUMP
/* Some systems that cannot dump also cannot implement these.  */

/*
 *	Return the address of the end of the text segment prior to
 *	doing an unexec.  After unexec the return value is undefined.
 */
 
char *
end_of_text ()
{
#ifdef TEXT_END
  return ((char *) TEXT_END);
#else
  extern int etext;
  return ((char *) &etext);
#endif
}
 
/*
 *	Return the address of the end of the data segment prior to
 *	doing an unexec.  After unexec the return value is undefined.
 */

char *
end_of_data ()
{
#ifdef DATA_END
  return ((char *) DATA_END);
#else
  extern int edata;
  return ((char *) &edata);
#endif
}

#endif /* not CANNOT_DUMP */

/* Get_system_name returns as its value
 a string for the Lisp function system-name to return. */

#ifdef BSD4_1
#include <whoami.h>
#endif

/* Can't have this within the function since `static' is #defined to
   nothing for some USG systems.  */
#ifdef USG
#ifdef HAVE_GETHOSTNAME
static char get_system_name_name[256];
#else /* not HAVE_GETHOSTNAME */
static struct utsname get_system_name_name;
#endif /* not HAVE_GETHOSTNAME */
#endif /* USG */

#ifndef BSD4_1
#ifndef USG
#ifndef VMS
#ifdef HAVE_SOCKETS
#include <sys/socket.h>
#include <netdb.h>
#endif /* HAVE_SOCKETS */
#endif /* not VMS */
#endif /* not USG */
#endif /* not BSD4_1 */

char *
get_system_name ()
{
#ifdef USG
#ifdef HAVE_GETHOSTNAME
  gethostname (get_system_name_name, sizeof (get_system_name_name));
  return get_system_name_name;
#else /* not HAVE_GETHOSTNAME */
  uname (&get_system_name_name);
  return (get_system_name_name.nodename);
#endif /* not HAVE_GETHOSTNAME */
#else /* Not USG */
#ifdef BSD4_1
  return sysname;
#else /* not USG, not 4.1 */
  static char system_name_saved[32];
#ifdef VMS
  char *sp;
  if ((sp = egetenv ("SYS$NODE")) == 0)
    sp = "vax-vms";
  else
    {
      char *end;

      if ((end = index (sp, ':')) != 0)
	*end = '\0';
    }
  strcpy (system_name_saved, sp);
#else /* not VMS */
  if (gethostname (system_name_saved, sizeof (system_name_saved)) != 0)
    {
      sprintf (system_name_saved, "gethostname: %s", sys_errlist[errno]);
      return (system_name_saved);
    }
#ifdef HAVE_SOCKETS
  /* Turn the hostname into the official, fully-qualified hostname.
     Don't do this if we're going to dump; this can confuse system
     libraries on some machines and make the dumped emacs core dump. */
#ifndef CANNOT_DUMP
  if (initialized)
#endif /* not CANNOT_DUMP */
    {
      struct hostent *hp;
      hp = gethostbyname (system_name_saved);
      if (hp && strlen (hp->h_name) < sizeof(system_name_saved))
	strcpy (system_name_saved, hp->h_name);
    }
#endif /* HAVE_SOCKETS */
#endif /* not VMS */
  return system_name_saved;
#endif /* not USG, not 4.1 */
#endif /* not USG */
}

#ifdef VMS
#ifndef HAVE_GETHOSTNAME
void gethostname(buf, len)
    char *buf;
    int len;
{
    char *s;
    s = getenv ("SYS$NODE");
    if (s == NULL)
        buf[0] = '\0';
    else {
        strncpy (buf, s, len - 2);
        buf[len - 1] = '\0';
    } /* else */
} /* static void gethostname */
#endif /* ! HAVE_GETHOSTNAME */
#endif /* VMS */


#ifndef VMS
#ifndef HAVE_SELECT

#ifdef HAVE_X_WINDOWS
/* Cause explanatory error message at compile time,
   since the select emulation is not good enough for X.  */
int *x = &x_windows_lose_if_no_select_system_call;
#endif

/* Emulate as much as select as is possible under 4.1 and needed by Gnu Emacs
 * Only checks read descriptors.
 */
/* How long to wait between checking fds in select */
#define SELECT_PAUSE 1
int select_alarmed;

/* For longjmp'ing back to read_input_waiting.  */

static jmp_buf read_alarm_throw;

/* Nonzero if the alarm signal should throw back to read_input_waiting.
   The read_socket_hook function sets this to 1 while it is waiting.  */

static int read_alarm_should_throw;

static SIGTYPE
select_alarm ()
{
  select_alarmed = 1;
#ifdef BSD4_1
  sigrelse (SIGALRM);
#else /* not BSD4_1 */
  signal (SIGALRM, SIG_IGN);
#endif /* not BSD4_1 */
  if (read_alarm_should_throw)
    longjmp (read_alarm_throw, 1);
}

/* Only rfds are checked and timeout must point somewhere */
int
select (nfds, rfds, wfds, efds, timeout)
     int nfds;
     int *rfds, *wfds, *efds, *timeout;
{
  int ravail = 0, orfds = 0, old_alarm;
  int timeoutval = timeout ? *timeout : 100000;
  int *local_timeout = &timeoutval;
  extern int proc_buffered_char[];
#ifndef subprocesses
  int process_tick = 0, update_tick = 0;
#else
  extern int process_tick, update_tick;
#endif
  SIGTYPE (*old_trap) ();
  unsigned char buf;

  if (rfds)
    {
      orfds = *rfds;
      *rfds = 0;
    }
  if (wfds)
    *wfds = 0;
  if (efds)
    *efds = 0;

  /* If we are looking only for the terminal, with no timeout,
     just read it and wait -- that's more efficient.  */
  if (orfds == 1 && *local_timeout == 100000 && process_tick == update_tick)
    {
      if (! detect_input_pending ())
	read_input_waiting ();
      *rfds = 1;
      return 1;
    }

  /* Once a second, till the timer expires, check all the flagged read
   * descriptors to see if any input is available.  If there is some then
   * set the corresponding bit in the return copy of rfds.
   */ 
  while (1)
    {
      register int to_check, bit, fd;

      if (rfds)
	{
	  for (to_check = nfds, bit = 1, fd = 0; --to_check >= 0; bit <<= 1, fd++)
	    {
	      if (orfds & bit)
		{
		  int avail = 0, status = 0;

		  if (bit == 1)
		    avail = detect_input_pending (); /* Special keyboard handler */
		  else
		    {
#ifdef FIONREAD
		      status = ioctl (fd, FIONREAD, &avail);
#else /* no FIONREAD */
		      /* Hoping it will return -1 if nothing available
			 or 0 if all 0 chars requested are read.  */
		      if (proc_buffered_char[fd] >= 0)
			avail = 1;
		      else
			{
			  avail = read (fd, &buf, 1);
			  if (avail > 0)
			    proc_buffered_char[fd] = buf;
			}
#endif /* no FIONREAD */
		    }
		  if (status >= 0 && avail > 0)
		    {
		      (*rfds) |= bit;
		      ravail++;
		    }
		}
	    }
	}
      if (*local_timeout == 0 || ravail != 0 || process_tick != update_tick)
	break;
      old_alarm = alarm (0);
      old_trap = signal (SIGALRM, select_alarm);
      select_alarmed = 0;
      alarm (SELECT_PAUSE);
      /* Wait for a SIGALRM (or maybe a SIGTINT) */
      while (select_alarmed == 0 && *local_timeout != 0
	     && process_tick == update_tick)
	{
	  /* If we are interested in terminal input,
	     wait by reading the terminal.
	     That makes instant wakeup for terminal input at least.  */
	  if (orfds & 1)
	    {
	      read_input_waiting ();
	      if (detect_input_pending ())
		select_alarmed = 1;
	    }
	  else
	    pause ();
	}
      (*local_timeout) -= SELECT_PAUSE;
      /* Reset the old alarm if there was one */
      alarm (0);
      signal (SIGALRM, old_trap);
      if (old_alarm != 0)
	{
	  /* Reset or forge an interrupt for the original handler. */
	  old_alarm -= SELECT_PAUSE;
	  if (old_alarm <= 0)
	    kill (getpid (), SIGALRM); /* Fake an alarm with the orig' handler */
	  else
	    alarm (old_alarm);
	}
      if (*local_timeout == 0)  /* Stop on timer being cleared */
	break;
    }
  return ravail;
}

/* Read keyboard input into the standard buffer,
   waiting for at least one character.  */

/* Make all keyboard buffers much bigger when using X windows.  */
#ifdef HAVE_X_WINDOWS
#define BUFFER_SIZE_FACTOR 16
#else
#define BUFFER_SIZE_FACTOR 1
#endif

void
read_input_waiting ()
{
  char buf[256 * BUFFER_SIZE_FACTOR];
  int nread;

  if (read_socket_hook)
    {
      read_alarm_should_throw = 0;
      if (! setjmp (read_alarm_throw))
	nread = (*read_socket_hook) (0, buf, 256 * BUFFER_SIZE_FACTOR, 1, 0);
      else
	nread = -1;
    }
  else
    nread = read (fileno (stdin), buf, 1);

  /* Scan the chars for C-g and store them in kbd_buffer.  */
  for (i = 0; i < nread; i++)
    {
      kbd_buffer_store_char (buf[i]);
      /* Don't look at input that follows a C-g too closely.
	 This reduces lossage due to autorepeat on C-g.  */
      if (buf[i] == interrupt_char)
	break;
    }
}

#endif /* not HAVE_SELECT */
#endif /* not VMS */

#ifdef BSD4_1
/*
 * Partially emulate 4.2 open call.
 * open is defined as this in 4.1.
 *
 * - added by Michael Bloom @ Citicorp/TTI
 *
 */

int
sys_open (path, oflag, mode)
     char *path;
     int oflag, mode;
{
  if (oflag & O_CREAT) 
    return creat (path, mode);
  else
    return open (path, oflag);
}

void
init_sigio ()
{
  if (noninteractive)
    return;
  lmode = LINTRUP | lmode;
  ioctl (0, TIOCLSET, &lmode);
}

void
reset_sigio ()
{
  if (noninteractive)
    return;
  lmode = ~LINTRUP & lmode;
  ioctl (0, TIOCLSET, &lmode);
}

void
request_sigio ()
{
  sigrelse (SIGTINT);

  interrupts_deferred = 0;
}

void
unrequest_sigio ()
{
  sighold (SIGTINT);

  interrupts_deferred = 1;
}

/* still inside #ifdef BSD4_1 */
#ifdef subprocesses

int sigheld; /* Mask of held signals */

sigholdx (signum)
     int signum;
{
  sigheld |= sigbit (signum);
  sighold (signum);
}

sigisheld (signum)
     int signum;
{
  sigheld |= sigbit (signum);
}

sigunhold (signum)
     int signum;
{
  sigheld &= ~sigbit (signum);
  sigrelse (signum);
}

sigfree ()    /* Free all held signals */
{
  int i;
  for (i = 0; i < NSIG; i++)
    if (sigheld & sigbit (i))
      sigrelse (i);
  sigheld = 0;
}

sigbit (i)
{
  return 1 << (i - 1);
}
#endif /* subprocesses */
#endif /* BSD4_1 */

/* POSIX signals support - DJB */
/* Anyone with POSIX signals should have ANSI C declarations */

#ifdef POSIX_SIGNALS

sigset_t old_mask, empty_mask, full_mask, temp_mask;
static struct sigaction new_action, old_action;

void
init_signals ()
{
  sigemptyset (&empty_mask);
  sigfillset (&full_mask);
}

signal_handler_t
sys_signal (int signal_number, signal_handler_t action)
{
#ifdef DGUX
  /* This gets us restartable system calls for efficiency.
     The "else" code will works as well. */
  return (berk_signal (signal_number, action));
#else
  sigemptyset (&new_action.sa_mask);
  new_action.sa_handler = action;
  new_action.sa_flags = 0;
  sigaction (signal_number, &new_action, &old_action);
  return (old_action.sa_handler);
#endif /* DGUX */
}

#ifndef __GNUC__
/* If we're compiling with GCC, we don't need this function, since it
   can be written as a macro.  */
sigset_t
sys_sigmask (int sig)
{
  sigset_t mask;
  sigemptyset (&mask);
  sigaddset (&mask, sig);
  return mask;
}
#endif

int
sys_sigpause (sigset_t new_mask)
{
  /* pause emulating berk sigpause... */
  sigsuspend (&new_mask);
  return (EINTR);
}

/* I'd like to have these guys return pointers to the mask storage in here,
   but there'd be trouble if the code was saving multiple masks.  I'll be
   safe and pass the structure.  It normally won't be more than 2 bytes
   anyhow. - DJB */

sigset_t
sys_sigblock (sigset_t new_mask)
{
  sigset_t old_mask;
  sigprocmask (SIG_BLOCK, &new_mask, &old_mask);
  return (old_mask);
}

sigset_t
sys_sigunblock (sigset_t new_mask)
{
  sigset_t old_mask;
  sigprocmask (SIG_UNBLOCK, &new_mask, &old_mask);
  return (old_mask);
}

sigset_t
sys_sigsetmask (sigset_t new_mask)
{
  sigset_t old_mask;
  sigprocmask (SIG_SETMASK, &new_mask, &old_mask);
  return (old_mask);
}

#endif /* POSIX_SIGNALS */

#ifndef HAVE_RANDOM
#ifdef USG
/*
 *	The BSD random returns numbers in the range of
 *	0 to 2e31 - 1.  The USG rand returns numbers in the
 *	range of 0 to 2e15 - 1.  This is probably not significant
 *	in this usage.
 */

long
random ()
{
  /* Arrange to return a range centered on zero.  */
  return (rand () << 15) + rand () - (1 << 29);
}

void
srandom (arg)
     int arg;
{
  srand (arg);
}

#endif /* USG */

#ifdef BSD4_1
long
random ()
{
  /* Arrange to return a range centered on zero.  */
  return (rand () << 15) + rand () - (1 << 29);
}

void
srandom (arg)
     int arg;
{
  srand (arg);
}
#endif /* BSD4_1 */
#endif /* HAVE_RANDOM */

#ifdef WRONG_NAME_INSQUE

void
insque (q,p)
     caddr_t q,p;
{
  _insque (q,p);
}

#endif

#ifdef VMS

#ifdef getenv
/* If any place else asks for the TERM variable,
   allow it to be overridden with the EMACS_TERM variable
   before attempting to translate the logical name TERM.  As a last
   resort, ask for VAX C's special idea of the TERM variable.  */
#undef getenv
char *
sys_getenv (name)
     char *name;
{
  register char *val;
  static char buf[256];
  static struct dsc$descriptor_s equiv
    = {sizeof (buf), DSC$K_DTYPE_T, DSC$K_CLASS_S, buf};
  static struct dsc$descriptor_s d_name
    = {0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0};
  short eqlen;

  if (!strcmp (name, "TERM"))
    {
      val = (char *) getenv ("EMACS_TERM");
      if (val)
	return val;
    }

  d_name.dsc$w_length = strlen (name);
  d_name.dsc$a_pointer = name;
  if (LIB$SYS_TRNLOG (&d_name, &eqlen, &equiv) == 1)
    {
      char *str = (char *) xmalloc (eqlen + 1);
      memcpy (str, buf, eqlen);
      str[eqlen] = '\0';
      /* This is a storage leak, but a pain to fix.  With luck,
	 no one will ever notice.  */
      return str;
    }
  return (char *) getenv (name);
}
#endif /* getenv */

#ifdef abort
/* Since VMS doesn't believe in core dumps, the only way to debug this beast is
   to force a call on the debugger from within the image. */
#undef abort
sys_abort ()
{
  reset_sys_modes ();
  LIB$SIGNAL (SS$_DEBUG);
}
#endif /* abort */
#endif /* VMS */

#ifdef VMS
#ifdef LINK_CRTL_SHARE
#ifdef SHAREABLE_LIB_BUG
/* Variables declared noshare and initialized in sharable libraries
   cannot be shared.  The VMS linker incorrectly forces you to use a private
   version which is uninitialized... If not for this "feature", we
   could use the C library definition of sys_nerr and sys_errlist. */
int sys_nerr = 35;
const char *sys_errlist[] =
  {
    "error 0",
    "not owner",
    "no such file or directory",
    "no such process",
    "interrupted system call",
    "i/o error",
    "no such device or address",
    "argument list too long",
    "exec format error",
    "bad file number",
    "no child process",
    "no more processes",
    "not enough memory",
    "permission denied",
    "bad address",
    "block device required",
    "mount devices busy",
    "file exists",
    "cross-device link",
    "no such device",
    "not a directory",
    "is a directory",
    "invalid argument",
    "file table overflow",
    "too many open files",
    "not a typewriter",
    "text file busy",
    "file too big",
    "no space left on device",
    "illegal seek",
    "read-only file system",
    "too many links",
    "broken pipe",
    "math argument",
    "result too large",
    "I/O stream empty",
    "vax/vms specific error code nontranslatable error"
  };
#endif /* SHAREABLE_LIB_BUG */
#endif /* LINK_CRTL_SHARE */
#endif /* VMS */

#ifdef INTERRUPTIBLE_OPEN

int
/* VARARGS 2 */
sys_open (path, oflag, mode)
     const char *path;
     int oflag, mode;
{
  register int rtnval;
  
  while ((rtnval = open (path, oflag, mode)) == -1
	 && (errno == EINTR));
  return (rtnval);
}

#endif /* INTERRUPTIBLE_OPEN */

#ifdef INTERRUPTIBLE_CLOSE

int
sys_close (fd)
     int fd;
{
  register int rtnval;

  while ((rtnval = close (fd)) == -1
	 && (errno == EINTR))
    ;
  return rtnval;
}

#endif /* INTERRUPTIBLE_CLOSE */

#ifdef INTERRUPTIBLE_IO

int
sys_read (int fildes, void *buf, unsigned int nbyte)
{
  register int rtnval;
  
  while ((rtnval = read (fildes, buf, nbyte)) == -1
	 && (errno == EINTR))
    ;
  return (rtnval);
}

int
sys_write (int fildes, const void *buf, unsigned int nbyte)
{
  register int rtnval;

  while ((rtnval = write (fildes, buf, nbyte)) == -1
	 && (errno == EINTR))
    ;
  return (rtnval);
}

#endif /* INTERRUPTIBLE_IO */

#ifndef HAVE_VFORK

/*
 *	Substitute fork for vfork on USG flavors.
 */

pid_t
vfork ()
{
  return (fork ());
}

#endif /* not HAVE_VFORK */


#ifdef USG
/*
 *	All of the following are for USG.
 *
 *	On USG systems the system calls are INTERRUPTIBLE by signals
 *	that the user program has elected to catch.  Thus the system call
 *	must be retried in these cases.  To handle this without massive
 *	changes in the source code, we remap the standard system call names
 *	to names for our own functions in sysdep.c that do the system call
 *	with retries.  Actually, for portability reasons, it is good
 *	programming practice, as this example shows, to limit all actual
 *	system calls to a single occurrence in the source.  Sure, this
 *	adds an extra level of function call overhead but it is almost
 *	always negligible.   Fred Fish, Unisoft Systems Inc.
 */

#ifndef HAVE_SYS_SIGLIST

const char *sys_siglist[NSIG + 1] =
{
#ifdef AIX
/* AIX has changed the signals a bit */
  "bogus signal",			/* 0 */
  "hangup",				/* 1  SIGHUP */
  "interrupt",				/* 2  SIGINT */
  "quit",				/* 3  SIGQUIT */
  "illegal instruction",		/* 4  SIGILL */
  "trace trap",				/* 5  SIGTRAP */
  "IOT instruction",			/* 6  SIGIOT */
  "crash likely",			/* 7  SIGDANGER */
  "floating point exception",		/* 8  SIGFPE */
  "kill",				/* 9  SIGKILL */
  "bus error",				/* 10 SIGBUS */
  "segmentation violation",		/* 11 SIGSEGV */
  "bad argument to system call",	/* 12 SIGSYS */
  "write on a pipe with no one to read it", /* 13 SIGPIPE */
  "alarm clock",			/* 14 SIGALRM */
  "software termination signum",	/* 15 SIGTERM */
  "user defined signal 1",		/* 16 SIGUSR1 */
  "user defined signal 2",		/* 17 SIGUSR2 */
  "death of a child",			/* 18 SIGCLD */
  "power-fail restart",			/* 19 SIGPWR */
  "bogus signal",			/* 20 */
  "bogus signal",			/* 21 */
  "bogus signal",			/* 22 */
  "bogus signal",			/* 23 */
  "bogus signal",			/* 24 */
  "LAN I/O interrupt",			/* 25 SIGAIO */
  "PTY I/O interrupt",			/* 26 SIGPTY */
  "I/O intervention required",		/* 27 SIGIOINT */
  "HFT grant",				/* 28 SIGGRANT */
  "HFT retract",			/* 29 SIGRETRACT */
  "HFT sound done",			/* 30 SIGSOUND */
  "HFT input ready",			/* 31 SIGMSG */
#else /* not AIX */
  "bogus signal",			/* 0 */
  "hangup",				/* 1  SIGHUP */
  "interrupt",				/* 2  SIGINT */
  "quit",				/* 3  SIGQUIT */
  "illegal instruction",		/* 4  SIGILL */
  "trace trap",				/* 5  SIGTRAP */
  "IOT instruction",			/* 6  SIGIOT */
  "EMT instruction",			/* 7  SIGEMT */
  "floating point exception",		/* 8  SIGFPE */
  "kill",				/* 9  SIGKILL */
  "bus error",				/* 10 SIGBUS */
  "segmentation violation",		/* 11 SIGSEGV */
  "bad argument to system call",	/* 12 SIGSYS */
  "write on a pipe with no one to read it", /* 13 SIGPIPE */
  "alarm clock",			/* 14 SIGALRM */
  "software termination signum",	/* 15 SIGTERM */
  "user defined signal 1",		/* 16 SIGUSR1 */
  "user defined signal 2",		/* 17 SIGUSR2 */
  "death of a child",			/* 18 SIGCLD */
  "power-fail restart",			/* 19 SIGPWR */
#endif /* not AIX */
  0
};
#endif /* not HAVE_SYS_SIGLIST */

/*
 *	Warning, this function may not duplicate 4.2 action properly
 *	under error conditions.
 */

#ifndef MAXPATHLEN
/* In 4.1, param.h fails to define this.  */
#define MAXPATHLEN 1024
#endif

#ifndef HAVE_GETWD

char *
getwd (pathname)
     char *pathname;
{
  char *npath, *spath;
  extern char *getcwd ();

  BLOCK_INPUT;			/* getcwd uses malloc */
  spath = npath = getcwd ((char *) 0, MAXPATHLEN);
  /* On Altos 3068, getcwd can return @hostname/dir, so discard
     up to first slash.  Should be harmless on other systems.  */
  if (npath) { /* added this test to prevent deref of NULL ptr below--DAE */
    while (*npath && *npath != '/')
      npath++;
    strcpy (pathname, npath);
    xfree (spath);			/* getcwd uses malloc */
  }
  UNBLOCK_INPUT;
  return pathname;
}

#endif /* HAVE_GETWD */

/*
 *	Emulate rename using unlink/link.  Note that this is
 *	only partially correct.  Also, doesn't enforce restriction
 *	that files be of same type (regular->regular, dir->dir, etc).
 */

#ifndef HAVE_RENAME

int
rename (from, to)
     const char *from;
     const char *to;
{
  if (access (from, 0) == 0)
    {
      unlink (to);
      if (link (from, to) == 0)
	if (unlink (from) == 0)
	  return (0);
    }
  return (-1);
}

#endif

#ifdef MISSING_UTIMES

/* HPUX (among others) sets HAVE_TIMEVAL but does not implement utimes.  */

#ifdef HPUX
int utimes(const char *path, const EMACS_TIME *tvp)
{
    struct utimbuf buf;

    buf.actime = EMACS_SECS (tvp[0]);
    buf.modtime = EMACS_SECS (tvp[1]);
    return utime(path, &buf);
}
#else /* !HPUX */
int
utimes ()
{
    errno = EINVAL;
    return -1;
}
#endif /* !HPUX */
#endif /* MISSING_UTIMES */

#ifdef IRIS_UTIME

/* The IRIS (3.5) has timevals, but uses sys V utime, and doesn't have the
   utimbuf structure defined anywhere but in the man page. */

struct utimbuf
 {
   long actime;
   long modtime;
 };

int
utimes (name, tvp)
     const char *name;
     const struct timeval tvp[];
{
  struct utimbuf utb;
  utb.actime  = tvp[0].tv_sec;
  utb.modtime = tvp[1].tv_sec;
  utime (name, &utb);
}
#endif /* IRIS_UTIME */


#ifdef HPUX
#ifndef HAVE_PERROR

/* HPUX curses library references perror, but as far as we know
   it won't be called.  Anyway this definition will do for now.  */

perror ()
{
}

#endif /* not HAVE_PERROR */
#endif /* HPUX */

#ifndef HAVE_DUP2

/*
 *	Emulate BSD dup2.  First close newd if it already exists.
 *	Then, attempt to dup oldd.  If not successful, call dup2 recursively
 *	until we are, then close the unsuccessful ones.
 */

int
dup2 (oldd, newd)
     int oldd;
     int newd;
{
  register int fd, ret;
  
  sys_close (newd);

#ifdef F_DUPFD
  fd = fcntl (oldd, F_DUPFD, newd);
  if (fd != newd)
    error ("can't dup2 (%i,%i) : %s", oldd, newd, sys_errlist[errno]);
#else
  fd = dup (old);
  if (fd == -1)
    return -1;
  if (fd == new)
    return new;
  ret = dup2 (old,new);
  sys_close (fd);
  return ret;
#endif /*  F_DUPFD */
}

#endif /* not HAVE_DUP2 */

/*
 *	Gettimeofday.  Simulate as much as possible.  Only accurate
 *	to nearest second.  Emacs doesn't use tzp so ignore it for now.
 *	Only needed when subprocesses are defined.
 */

#ifdef subprocesses
#ifndef VMS
#ifndef HAVE_GETTIMEOFDAY
#ifdef HAVE_TIMEVAL
 
/* ARGSUSED */
int
gettimeofday (tp, tzp)
     struct timeval *tp;
     struct timezone *tzp;
{
  extern long time ();

  tp->tv_sec = time ((long *)0);    
  tp->tv_usec = 0;
  if (tzp != 0)
    tzp->tz_minuteswest = -1;
  return (0);
}
 
#endif /* HAVE_TIMEVAL */
#endif /* !HAVE_GETTIMEOFDAY */
#endif /* !VMS */
#endif /* subprocess && !HAVE_GETTIMEOFDAY && HAVE_TIMEVAL && !VMS */
  

#endif /* USG */

#ifdef DGUX

const char *sys_siglist[NSIG + 1] =
{
  "null signal",			 /*  0 SIGNULL   */
  "hangup",				 /*  1 SIGHUP    */
  "interrupt",		       		 /*  2 SIGINT    */
  "quit",				 /*  3 SIGQUIT   */
  "illegal instruction",		 /*  4 SIGILL    */
  "trace trap",				 /*  5 SIGTRAP   */
  "abort termination",			 /*  6 SIGABRT   */
  "SIGEMT",				 /*  7 SIGEMT    */
  "floating point exception",		 /*  8 SIGFPE    */
  "kill",				 /*  9 SIGKILL   */
  "bus error",				 /* 10 SIGBUS    */
  "segmentation violation",		 /* 11 SIGSEGV   */
  "bad argument to system call",	 /* 12 SIGSYS    */
  "write on a pipe with no reader",	 /* 13 SIGPIPE   */
  "alarm clock",			 /* 14 SIGALRM   */
  "software termination signal",	 /* 15 SIGTERM   */
  "user defined signal 1",		 /* 16 SIGUSR1   */
  "user defined signal 2",		 /* 17 SIGUSR2   */
  "child stopped or terminated",	 /* 18 SIGCLD    */
  "power-fail restart",			 /* 19 SIGPWR    */
  "window size changed",		 /* 20 SIGWINCH  */
  "undefined",				 /* 21           */
  "pollable event occurred",		 /* 22 SIGPOLL   */
  "sendable stop signal not from tty",	 /* 23 SIGSTOP   */
  "stop signal from tty",		 /* 24 SIGSTP    */
  "continue a stopped process",		 /* 25 SIGCONT   */
  "attempted background tty read",	 /* 26 SIGTTIN   */
  "attempted background tty write",	 /* 27 SIGTTOU   */
  "undefined",				 /* 28           */
  "undefined",				 /* 29           */
  "undefined",				 /* 30           */
  "undefined",				 /* 31           */
  "undefined",				 /* 32           */
  "socket (TCP/IP) urgent data arrival", /* 33 SIGURG    */
  "I/O is possible",			 /* 34 SIGIO     */
  "exceeded cpu time limit",		 /* 35 SIGXCPU   */
  "exceeded file size limit",		 /* 36 SIGXFSZ   */
  "virtual time alarm",			 /* 37 SIGVTALRM */
  "profiling time alarm",		 /* 38 SIGPROF   */
  "undefined",				 /* 39           */
  "file record locks revoked",		 /* 40 SIGLOST   */
  "undefined",				 /* 41           */
  "undefined",				 /* 42           */
  "undefined",				 /* 43           */
  "undefined",				 /* 44           */
  "undefined",				 /* 45           */
  "undefined",				 /* 46           */
  "undefined",				 /* 47           */
  "undefined",				 /* 48           */
  "undefined",				 /* 49           */
  "undefined",				 /* 50           */
  "undefined",				 /* 51           */
  "undefined",				 /* 52           */
  "undefined",				 /* 53           */
  "undefined",				 /* 54           */
  "undefined",				 /* 55           */
  "undefined",				 /* 56           */
  "undefined",				 /* 57           */
  "undefined",				 /* 58           */
  "undefined",				 /* 59           */
  "undefined",				 /* 60           */
  "undefined",				 /* 61           */
  "undefined",				 /* 62           */
  "undefined",				 /* 63           */
  "notification message in mess. queue", /* 64 SIGDGNOTIFY */
  0
};

#endif /* DGUX */

#ifdef BSD4_1
const char *sys_siglist[] =
  {
    "bum signal!!",
    "hangup",
    "interrupt",
    "quit",
    "illegal instruction",
    "trace trap",
    "iot instruction",
    "emt instruction",
    "floating point exception",
    "kill",
    "bus error",
    "segmentation violation",
    "bad argument to system call",
    "write on a pipe with no one to read it",
    "alarm clock",
    "software termination signal from kill",
    "status signal",
    "sendable stop signal not from tty",
    "stop signal from tty",
    "continue a stopped process",
    "child status has changed",
    "background read attempted from control tty",
    "background write attempted from control tty",
    "input record available at control tty",
    "exceeded CPU time limit",
    "exceeded file size limit",
    0
};
#endif /* BSD4_1 */


/* Directory routines for systems that don't have them. */

#ifdef SYSV_SYSTEM_DIR

#include <dirent.h>

#ifndef HAVE_CLOSEDIR
int
closedir (dirp)
     register DIR *dirp;        /* stream from opendir */
{
  sys_close (dirp->dd_fd);
  /* Some systems (like Solaris) allocate the buffer and the DIR all
     in one block.  Why in the world are we freeing this ourselves
     anyway?  */
#if ! (defined (sun) && defined (USG5_4))
  xfree ((char *) dirp->dd_buf); /* directory block defined in <dirent.h> */
#endif
  xfree ((char *) dirp);
  return (0);
}
#endif /* not HAVE_CLOSEDIR */
#endif /* SYSV_SYSTEM_DIR */

#ifdef NONSYSTEM_DIR_LIBRARY

DIR *
opendir (filename)
     const char *filename;	/* name of directory */
{
  register DIR *dirp;		/* -> malloc'ed storage */
  register int fd;		/* file descriptor for read */
  struct stat sbuf;		/* result of fstat */

  fd = sys_open (filename, 0);
  if (fd < 0)
    return 0;

  BLOCK_INPUT;
  if (fstat (fd, &sbuf) < 0
      || (sbuf.st_mode & S_IFMT) != S_IFDIR
      || (dirp = (DIR *) malloc (sizeof (DIR))) == 0)
    {
      sys_close (fd);
      UNBLOCK_INPUT;
      return 0;		/* bad luck today */
    }
  UNBLOCK_INPUT;

  dirp->dd_fd = fd;
  dirp->dd_loc = dirp->dd_size = 0;	/* refill needed */

  return dirp;
}

void
closedir (dirp)
     register DIR *dirp;		/* stream from opendir */
{
  sys_close (dirp->dd_fd);
  xfree (dirp);
}


#ifndef VMS
#define DIRSIZ	14
struct olddir
  {
    ino_t od_ino; 		/* inode */
    char od_name[DIRSIZ];	/* filename */
  };
#endif /* not VMS */

static struct direct dir_static; /* simulated directory contents */

/* ARGUSED */
struct direct *
readdir (dirp)
     register DIR *dirp;	/* stream from opendir */
{
#ifndef VMS
  register struct olddir *dp;	/* -> directory data */
#else /* VMS */
  register struct dir$_name *dp; /* -> directory data */
  register struct dir$_version *dv; /* -> version data */
#endif /* VMS */

  for (; ;)
    {
      if (dirp->dd_loc >= dirp->dd_size)
	dirp->dd_loc = dirp->dd_size = 0;

      if (dirp->dd_size == 0 	/* refill buffer */
	  && (dirp->dd_size = sys_read (dirp->dd_fd, dirp->dd_buf, DIRBLKSIZ)) <= 0)
	return 0;

#ifndef VMS
      dp = (struct olddir *) &dirp->dd_buf[dirp->dd_loc];
      dirp->dd_loc += sizeof (struct olddir);

      if (dp->od_ino != 0)	/* not deleted entry */
	{
	  dir_static.d_ino = dp->od_ino;
	  strncpy (dir_static.d_name, dp->od_name, DIRSIZ);
	  dir_static.d_name[DIRSIZ] = '\0';
	  dir_static.d_namlen = strlen (dir_static.d_name);
	  dir_static.d_reclen = sizeof (struct direct)
	    - MAXNAMLEN + 3
	      + dir_static.d_namlen - dir_static.d_namlen % 4;
	  return &dir_static;	/* -> simulated structure */
	}
#else /* VMS */
      dp = (struct dir$_name *) dirp->dd_buf;
      if (dirp->dd_loc == 0)
	dirp->dd_loc = (dp->dir$b_namecount&1) ? dp->dir$b_namecount + 1
	  : dp->dir$b_namecount;
      dv = (struct dir$_version *)&dp->dir$t_name[dirp->dd_loc];
      dir_static.d_ino = dv->dir$w_fid_num;
      dir_static.d_namlen = dp->dir$b_namecount;
      dir_static.d_reclen = sizeof (struct direct)
	- MAXNAMLEN + 3
	  + dir_static.d_namlen - dir_static.d_namlen % 4;
      strncpy (dir_static.d_name, dp->dir$t_name, dp->dir$b_namecount);
      dir_static.d_name[dir_static.d_namlen] = '\0';
      dirp->dd_loc = dirp->dd_size; /* only one record at a time */
      return &dir_static;
#endif /* VMS */
    }
}

#ifdef VMS
/* readdirver is just like readdir except it returns all versions of a file
   as separate entries.  */

/* ARGUSED */
struct direct *
readdirver (dirp)
     register DIR *dirp;	/* stream from opendir */
{
  register struct dir$_name *dp; /* -> directory data */
  register struct dir$_version *dv; /* -> version data */

  if (dirp->dd_loc >= dirp->dd_size - sizeof (struct dir$_name))
    dirp->dd_loc = dirp->dd_size = 0;

  if (dirp->dd_size == 0 	/* refill buffer */
      && (dirp->dd_size = sys_read (dirp->dd_fd, dirp->dd_buf, DIRBLKSIZ)) <= 0)
    return 0;

  dp = (struct dir$_name *) dirp->dd_buf;
  if (dirp->dd_loc == 0)
    dirp->dd_loc = (dp->dir$b_namecount & 1) ? dp->dir$b_namecount + 1
		   : dp->dir$b_namecount;
  dv = (struct dir$_version *) &dp->dir$t_name[dirp->dd_loc];
  strncpy (dir_static.d_name, dp->dir$t_name, dp->dir$b_namecount);
  sprintf (&dir_static.d_name[dp->dir$b_namecount], ";%d", dv->dir$w_version);
  dir_static.d_namlen = strlen (dir_static.d_name);
  dir_static.d_ino = dv->dir$w_fid_num;
  dir_static.d_reclen = sizeof (struct direct) - MAXNAMLEN + 3
			+ dir_static.d_namlen - dir_static.d_namlen % 4;
  dirp->dd_loc = ((char *) (++dv) - dp->dir$t_name);
  return &dir_static;
}

#endif /* VMS */

#endif /* NONSYSTEM_DIR_LIBRARY */


/* mkdir and rmdir functions, for systems which don't have them.  */

#ifndef HAVE_MKDIR
/*
 * Written by Robert Rother, Mariah Corporation, August 1985.
 *
 * If you want it, it's yours.  All I ask in return is that if you
 * figure out how to do this in a Bourne Shell script you send me
 * a copy.
 *					sdcsvax!rmr or rmr@uscd
 *
 * Severely hacked over by John Gilmore to make a 4.2BSD compatible
 * subroutine.	11Mar86; hoptoad!gnu
 *
 * Modified by rmtodd@uokmax 6-28-87 -- when making an already existing dir,
 * subroutine didn't return EEXIST.  It does now.
 */

/*
 * Make a directory.
 */
int
mkdir (dpath, dmode)
     const char *dpath;
     int dmode;
{
  int cpid, status, fd;
  struct stat statbuf;

  if (stat (dpath, &statbuf) == 0)
    {
      errno = EEXIST;		/* Stat worked, so it already exists */
      return -1;
    }

  /* If stat fails for a reason other than non-existence, return error */
  if (errno != ENOENT)
    return -1;

  synch_process_alive = 1;
  switch (cpid = fork ())
    {

    case -1:			/* Error in fork() */
      return (-1);		/* Errno is set already */

    case 0:			/* Child process */
      /*
		 * Cheap hack to set mode of new directory.  Since this
		 * child process is going away anyway, we zap its umask.
		 * FIXME, this won't suffice to set SUID, SGID, etc. on this
		 * directory.  Does anybody care?
		 */
      status = umask (0);	/* Get current umask */
      status = umask (status | (0777 & ~dmode));	/* Set for mkdir */
      fd = sys_open("/dev/null", 2);
      if (fd >= 0)
        {
	  dup2 (fd, 0);
	  dup2 (fd, 1);
	  dup2 (fd, 2);
        }
      execl ("/bin/mkdir", "mkdir", dpath, (char *) 0);
      _exit (-1);		/* Can't exec /bin/mkdir */

    default:			/* Parent process */
      wait_for_termination (cpid);
    }

  if (synch_process_death != 0 || synch_process_retcode != 0)
    {
      errno = EIO;		/* We don't know why, but */
      return -1;		/* /bin/mkdir failed */
    }

  return 0;
}
#endif /* not HAVE_MKDIR */

#ifndef HAVE_RMDIR
int
rmdir (dpath)
     const char *dpath;
{
  int cpid, status, fd;
  struct stat statbuf;

  if (stat (dpath, &statbuf) != 0)
    {
      /* Stat just set errno.  We don't have to */
      return -1;
    }

  synch_process_alive = 1;
  switch (cpid = fork ())
    {

    case -1:			/* Error in fork() */
      return (-1);		/* Errno is set already */

    case 0:			/* Child process */
      fd = sys_open("/dev/null", 2);
      if (fd >= 0)
        {
	  dup2 (fd, 0);
	  dup2 (fd, 1);
	  dup2 (fd, 2);
        }
      execl ("/bin/rmdir", "rmdir", dpath, (char *) 0);
      _exit (-1);		/* Can't exec /bin/mkdir */

    default:			/* Parent process */
      wait_for_termination (cpid);
    }

  if (synch_process_death != 0 || synch_process_retcode != 0)
    {
      errno = EIO;		/* We don't know why, but */
      return -1;		/* /bin/rmdir failed */
    }

  return 0;
}
#endif /* !HAVE_RMDIR */


/* Functions for VMS */
#ifdef VMS
#include "vms-pwd.h"
#include <acldef.h>
#include <chpdef.h>
#include <jpidef.h>

/* Return as a string the VMS error string pertaining to STATUS.
   Reuses the same static buffer each time it is called.  */

char *
vmserrstr (status)
     int status;		/* VMS status code */
{
  int bufadr[2];
  short len;
  static char buf[257];

  bufadr[0] = sizeof buf - 1;
  bufadr[1] = (int) buf;
  if (! (SYS$GETMSG (status, &len, bufadr, 0x1, 0) & 1))
    return "untranslatable VMS error status";
  buf[len] = '\0';
  return buf;
}

#ifdef access
#undef access
  
/* The following is necessary because 'access' emulation by VMS C (2.0) does
 * not work correctly.  (It also doesn't work well in version 2.3.)
 */

#ifdef VMS4_4

#define DESCRIPTOR(name,string) struct dsc$descriptor_s name = \
	{ strlen (string), DSC$K_DTYPE_T, DSC$K_CLASS_S, string }

typedef union {
    struct {
	unsigned short s_buflen;
	unsigned short s_code;
	char *s_bufadr;
	unsigned short *s_retlenadr;
    } s;
    int end;
} item;
#define buflen s.s_buflen
#define code s.s_code
#define bufadr s.s_bufadr
#define retlenadr s.s_retlenadr

#define R_OK 4	/* test for read permission */
#define W_OK 2	/* test for write permission */
#define X_OK 1	/* test for execute (search) permission */
#define F_OK 0	/* test for presence of file */

int
sys_access (path, mode)
     const char *path;
     int mode;
{
  static char *user = NULL;
  char dir_fn[512];

  /* translate possible directory spec into .DIR file name, so brain-dead
   * access can treat the directory like a file.  */
  if (directory_file_name (path, dir_fn))
    path = dir_fn;
  
  if (mode == F_OK)
    return access (path, mode);
  if (user == NULL && (user = (char *) getenv ("USER")) == NULL)
    return -1;
  {
    int stat;
    int flags;
    int acces;
    unsigned short int dummy;
    item itemlst[3];
    static int constant = ACL$C_FILE;
    DESCRIPTOR (path_desc, path);
    DESCRIPTOR (user_desc, user);
 
    flags = 0;
    acces = 0;
    if ((mode & X_OK) && ((stat = access (path, mode)) < 0 || mode == X_OK))
      return stat;
    if (mode & R_OK)
      acces |= CHP$M_READ;
    if (mode & W_OK)
      acces |= CHP$M_WRITE;
    itemlst[0].buflen = sizeof (int);
    itemlst[0].code = CHP$_FLAGS;
    itemlst[0].bufadr = (char *) &flags;
    itemlst[0].retlenadr = &dummy;
    itemlst[1].buflen = sizeof (int);
    itemlst[1].code = CHP$_ACCESS;
    itemlst[1].bufadr = (char *) &acces;
    itemlst[1].retlenadr = &dummy;
    itemlst[2].end = CHP$_END;
    stat = SYS$CHECK_ACCESS (&constant, &path_desc, &user_desc, itemlst);
    return stat == SS$_NORMAL ? 0 : -1;
  }
}

#else /* not VMS4_4 */

#include <prvdef.h>
#define	ACE$M_WRITE	2
#define	ACE$C_KEYID	1

static unsigned short vms_memid, vms_grpid;
static unsigned int vms_uic;

/* Called from init_sys_modes, so it happens not very often
   but at least each time Emacs is loaded.  */
sys_access_reinit ()
{
  vms_uic = 0;
}

int
sys_access (filename, type)
     char * filename;
     int type;
{
  struct FAB fab;
  struct XABPRO xab;
  int status, size, i, typecode, acl_controlled;
  unsigned int *aclptr, *aclend, aclbuf[60];
  union prvdef prvmask;

  /* Get UIC and GRP values for protection checking.  */
  if (vms_uic == 0)
    {
      status = LIB$GETJPI (&JPI$_UIC, 0, 0, &vms_uic, 0, 0);
      if (! (status & 1))
	return -1;
      vms_memid = vms_uic & 0xFFFF;
      vms_grpid = vms_uic >> 16;
    }

  if (type != 2)		/* not checking write access */
    return access (filename, type);

  /* Check write protection. */
    
#define	CHECKPRIV(bit)    (prvmask.bit)
#define	WRITEABLE(field)  (! ((xab.xab$w_pro >> field) & XAB$M_NOWRITE))

  /* Find privilege bits */
  status = SYS$SETPRV (0, 0, 0, prvmask);
  if (! (status & 1))
    error ("Unable to find privileges: %s", vmserrstr (status));
  if (CHECKPRIV (PRV$V_BYPASS))
    return 0;			/* BYPASS enabled */
  fab = cc$rms_fab;
  fab.fab$b_fac = FAB$M_GET;
  fab.fab$l_fna = filename;
  fab.fab$b_fns = strlen (filename);
  fab.fab$l_xab = &xab;
  xab = cc$rms_xabpro;
  xab.xab$l_aclbuf = aclbuf;
  xab.xab$w_aclsiz = sizeof (aclbuf);
  status = SYS$OPEN (&fab, 0, 0);
  if (! (status & 1))
    return -1;
  SYS$CLOSE (&fab, 0, 0);
  /* Check system access */
  if (CHECKPRIV (PRV$V_SYSPRV) && WRITEABLE (XAB$V_SYS))
    return 0;
  /* Check ACL entries, if any */
  acl_controlled = 0;
  if (xab.xab$w_acllen > 0)
    {
      aclptr = aclbuf;
      aclend = &aclbuf[xab.xab$w_acllen / 4];
      while (*aclptr && aclptr < aclend)
	{
	  size = (*aclptr & 0xff) / 4;
	  typecode = (*aclptr >> 8) & 0xff;
	  if (typecode == ACE$C_KEYID)
	    for (i = size - 1; i > 1; i--)
	      if (aclptr[i] == vms_uic)
		{
		  acl_controlled = 1;
		  if (aclptr[1] & ACE$M_WRITE)
		    return 0;	/* Write access through ACL */
		}
	  aclptr = &aclptr[size];
	}
      if (acl_controlled)	/* ACL specified, prohibits write access */
	return -1;
    }
  /* No ACL entries specified, check normal protection */
  if (WRITEABLE (XAB$V_WLD))	/* World writeable */
    return 0;
  if (WRITEABLE (XAB$V_GRP) &&
      (unsigned short) (xab.xab$l_uic >> 16) == vms_grpid)
    return 0;			/* Group writeable */
  if (WRITEABLE (XAB$V_OWN) &&
      (xab.xab$l_uic & 0xFFFF) == vms_memid)
    return 0;			/* Owner writeable */

  return -1;	/* Not writeable */
}
#endif /* not VMS4_4 */
#endif /* access */
  
static char vtbuf[NAM$C_MAXRSS+1];

/* translate a vms file spec to a unix path */
char *
sys_translate_vms (vfile)
     char * vfile;
{
  char * p;
  char * targ;

  if (!vfile)
    return 0;

  targ = vtbuf;

  /* leading device or logical name is a root directory */
  if (p = strchr (vfile, ':'))
    {
      *targ++ = '/';
      while (vfile < p)
	*targ++ = *vfile++;
      vfile++;
      *targ++ = '/';
    }
  p = vfile;
  if (*p == '[' || *p == '<')
    {
      while (*++vfile != *p + 2)
	switch (*vfile)
	  {
	  case '.':
	    if (vfile[-1] == *p)
	      *targ++ = '.';
	    *targ++ = '/';
	    break;

	  case '-':
	    *targ++ = '.';
	    *targ++ = '.';
	    break;
	    
	  default:
	    *targ++ = *vfile;
	    break;
	  }
      vfile++;
      *targ++ = '/';
    }
  while (*vfile)
    *targ++ = *vfile++;

  return vtbuf;
}

static char utbuf[NAM$C_MAXRSS+1];

/* translate a unix path to a VMS file spec */
char *
sys_translate_unix (ufile)
     char * ufile;
{
  int slash_seen = 0;
  char *p;
  char * targ;

  if (!ufile)
    return 0;

  targ = utbuf;

  if (*ufile == '/')
    {
      ufile++;
    }

  while (*ufile)
    {
      switch (*ufile)
	{
	case '/':
	  if (slash_seen)
	    if (index (&ufile[1], '/'))
	      *targ++ = '.';
	    else
	      *targ++ = ']';
	  else
	    {
	      *targ++ = ':';
	      if (index (&ufile[1], '/'))
		*targ++ = '[';
	      slash_seen = 1;
	    }
	  break;

	case '.':
	  if (strncmp (ufile, "./", 2) == 0)
	    {
	      if (!slash_seen)
		{
		  *targ++ = '[';
		  slash_seen = 1;
		}
	      ufile++;		/* skip the dot */
	      if (index (&ufile[1], '/'))
		*targ++ = '.';
	      else
		*targ++ = ']';
	    }
	  else if (strncmp (ufile, "../", 3) == 0)
	    {
	      if (!slash_seen)
		{
		  *targ++ = '[';
		  slash_seen = 1;
		}
	      *targ++ = '-';
	      ufile += 2;	/* skip the dots */
	      if (index (&ufile[1], '/'))
		*targ++ = '.';
	      else
		*targ++ = ']';
	    }
	  else
	    *targ++ = *ufile;
	  break;

	default:
	  *targ++ = *ufile;
	  break;
	}
      ufile++;
    }
  *targ = '\0';
  
  return utbuf;
}

char *
getwd (pathname)
     char *pathname;
{
  char *ptr;
  strcpy (pathname, egetenv ("PATH"));

  ptr = pathname;
  while (*ptr)
    {
      if ('a' <= *ptr && *ptr <= 'z')
	*ptr -= 040;
      ptr++;
    }
  return pathname;
}

int
getppid ()
{
  long item_code = JPI$_OWNER;
  unsigned long parent_id;
  int status;

  if (((status = LIB$GETJPI (&item_code, 0, 0, &parent_id)) & 1) == 0)
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return -1;
    }
  return parent_id;
}

#undef getuid
unsigned int
sys_getuid ()
{
  return (getgid () << 16) | getuid ();
}

int
sys_read (fildes, buf, nbyte)
     int fildes;
     char *buf;
     unsigned int nbyte;
{
  return read (fildes, buf, (nbyte < MAXIOSIZE ? nbyte : MAXIOSIZE));
}

#if 0
int
sys_write (fildes, buf, nbyte)
     int fildes;
     char *buf;
     unsigned int nbyte;
{
  register int nwrote, rtnval = 0;

  while (nbyte > MAXIOSIZE && (nwrote = write (fildes, buf, MAXIOSIZE)) > 0) {
    nbyte -= nwrote;
    buf += nwrote;
    rtnval += nwrote;
  }
  if (nwrote < 0)
    return rtnval ? rtnval : -1;
  if ((nwrote = write (fildes, buf, nbyte)) < 0)
    return rtnval ? rtnval : -1;
  return (rtnval + nwrote);
}
#endif /* 0 */

/*
 *	VAX/VMS VAX C RTL really loses. It insists that records
 *      end with a newline (carriage return) character, and if they
 *	don't it adds one (nice of it isn't it!)
 *
 *	Thus we do this stupidity below.
 */

int
sys_write (fildes, buf, nbytes)
     int fildes;
     char *buf;
     unsigned int nbytes;
{
  register char *p;
  register char *e;
  int sum = 0;
  struct stat st;

  fstat (fildes, &st);
  p = buf;
  while (nbytes > 0)
    {
      int len, retval;

      /* Handle fixed-length files with carriage control.  */
      if (st.st_fab_rfm == FAB$C_FIX
	  && ((st.st_fab_rat & (FAB$M_FTN | FAB$M_CR)) != 0))
	{
	  len = st.st_fab_mrs;
	  retval = write (fildes, p, min (len, nbytes));
	  if (retval != len)
	    return -1;
	  retval++;	/* This skips the implied carriage control */
	}
      else
	{
	  e =  p + min (MAXIOSIZE, nbytes) - 1;
	  while (*e != '\n' && e > p) e--;
	  if (p == e)		/* Ok.. so here we add a newline... sigh. */
	    e = p + min (MAXIOSIZE, nbytes) - 1;
	  len = e + 1 - p;
	  retval = write (fildes, p, len);
	  if (retval != len)
	    return -1;
	}
      p += retval;
      sum += retval;
      nbytes -= retval;
    }
  return sum;
}

/* Create file NEW copying its attributes from file OLD.  If
   OLD is 0 or does not exist, create based on the value of
   vms_stmlf_recfm. */

/* Protection value the file should ultimately have.
   Set by create_copy_attrs, and use by rename_sansversions.  */
static unsigned short int vms_fab_final_pro;

int
creat_copy_attrs (old, new)
     char *old, *new;
{
  struct FAB fab = cc$rms_fab;
  struct XABPRO xabpro;
  char aclbuf[256];	/* Choice of size is arbitrary.  See below. */
  extern int vms_stmlf_recfm;

  if (old)
    {
      fab.fab$b_fac = FAB$M_GET;
      fab.fab$l_fna = old;
      fab.fab$b_fns = strlen (old);
      fab.fab$l_xab = (char *) &xabpro;
      xabpro = cc$rms_xabpro;
      xabpro.xab$l_aclbuf = aclbuf;
      xabpro.xab$w_aclsiz = sizeof aclbuf;
      /* Call $OPEN to fill in the fab & xabpro fields. */
      if (SYS$OPEN (&fab, 0, 0) & 1)
	{
	  SYS$CLOSE (&fab, 0, 0);
	  fab.fab$l_alq = 0;	/* zero the allocation quantity */
	  if (xabpro.xab$w_acllen > 0)
	    {
	      if (xabpro.xab$w_acllen > sizeof aclbuf)
		/* If the acl buffer was too short, redo open with longer one.
		   Wouldn't need to do this if there were some system imposed
		   limit on the size of an ACL, but I can't find any such. */
		{
		  xabpro.xab$l_aclbuf = (char *) alloca (xabpro.xab$w_acllen);
		  xabpro.xab$w_aclsiz = xabpro.xab$w_acllen;
		  if (SYS$OPEN (&fab, 0, 0) & 1)
		    SYS$CLOSE (&fab, 0, 0);
		  else
		    old = 0;
		}
	    }
	  else
	    xabpro.xab$l_aclbuf = 0;
	}
      else
	old = 0;
    }
  fab.fab$l_fna = new;
  fab.fab$b_fns = strlen (new);
  if (!old)
    {
      fab.fab$l_xab = 0;
      fab.fab$b_rfm = vms_stmlf_recfm ? FAB$C_STMLF : FAB$C_VAR;
      fab.fab$b_rat = FAB$M_CR;
    }

  /* Set the file protections such that we will be able to manipulate
     this file.  Once we are done writing and renaming it, we will set
     the protections back.  */
  if (old)
    vms_fab_final_pro = xabpro.xab$w_pro;
  else
    SYS$SETDFPROT (0, &vms_fab_final_pro);
  xabpro.xab$w_pro &= 0xff0f; /* set O:rewd for now. This is set back later. */

  /* Create the new file with either default attrs or attrs copied
     from old file. */
  if (!(SYS$CREATE (&fab, 0, 0) & 1))
    return -1;
  SYS$CLOSE (&fab, 0, 0);
  /* As this is a "replacement" for creat, return a file descriptor
     opened for writing. */
  return open (new, O_WRONLY);
}

#ifdef creat
#undef creat
#include <varargs.h>
#ifdef __GNUC__
#ifndef va_count
#define va_count(X) ((X) = *(((int *) &(va_alist)) - 1))
#endif
#endif

int
sys_creat (va_alist)
     va_dcl
{
  va_list list_incrementer;
  char *name;
  int mode;
  int rfd;			/* related file descriptor */
  int fd;			/* Our new file descriptor */
  int count;
  struct stat st_buf;
  char rfm[12];
  char rat[15];
  char mrs[13];
  char fsz[13];
  extern int vms_stmlf_recfm;

  va_count (count);
  va_start (list_incrementer);
  name = va_arg (list_incrementer, char *);
  mode = va_arg (list_incrementer, int);
  if (count > 2)
    rfd = va_arg (list_incrementer, int);
  va_end (list_incrementer);
  if (count > 2)
    {
      /* Use information from the related file descriptor to set record
	 format of the newly created file. */
      fstat (rfd, &st_buf);
      switch (st_buf.st_fab_rfm)
	{
	case FAB$C_FIX:
	  strcpy (rfm, "rfm = fix");
	  sprintf (mrs, "mrs = %d", st_buf.st_fab_mrs);
	  strcpy (rat, "rat = ");
	  if (st_buf.st_fab_rat & FAB$M_CR)
	    strcat (rat, "cr");
	  else if (st_buf.st_fab_rat & FAB$M_FTN)
	    strcat (rat, "ftn");
	  else if (st_buf.st_fab_rat & FAB$M_PRN)
	    strcat (rat, "prn");
	  if (st_buf.st_fab_rat & FAB$M_BLK)
	    if (st_buf.st_fab_rat & (FAB$M_CR|FAB$M_FTN|FAB$M_PRN))
	      strcat (rat, ", blk");
	    else
	      strcat (rat, "blk");
	  return creat (name, 0, rfm, rat, mrs);

	case FAB$C_VFC:
	  strcpy (rfm, "rfm = vfc");
	  sprintf (fsz, "fsz = %d", st_buf.st_fab_fsz);
	  strcpy (rat, "rat = ");
	  if (st_buf.st_fab_rat & FAB$M_CR)
	    strcat (rat, "cr");
	  else if (st_buf.st_fab_rat & FAB$M_FTN)
	    strcat (rat, "ftn");
	  else if (st_buf.st_fab_rat & FAB$M_PRN)
	    strcat (rat, "prn");
	  if (st_buf.st_fab_rat & FAB$M_BLK)
	    if (st_buf.st_fab_rat & (FAB$M_CR|FAB$M_FTN|FAB$M_PRN))
	      strcat (rat, ", blk");
	    else
	      strcat (rat, "blk");
	  return creat (name, 0, rfm, rat, fsz);

	case FAB$C_STM:
	  strcpy (rfm, "rfm = stm");
	  break;

	case FAB$C_STMCR:
	  strcpy (rfm, "rfm = stmcr");
	  break;

	case FAB$C_STMLF:
	  strcpy (rfm, "rfm = stmlf");
	  break;

	case FAB$C_UDF:
	  strcpy (rfm, "rfm = udf");
	  break;

	case FAB$C_VAR:
	  strcpy (rfm, "rfm = var");
	  break;
	}
      strcpy (rat, "rat = ");
      if (st_buf.st_fab_rat & FAB$M_CR)
	strcat (rat, "cr");
      else if (st_buf.st_fab_rat & FAB$M_FTN)
	strcat (rat, "ftn");
      else if (st_buf.st_fab_rat & FAB$M_PRN)
	strcat (rat, "prn");
      if (st_buf.st_fab_rat & FAB$M_BLK)
	if (st_buf.st_fab_rat & (FAB$M_CR|FAB$M_FTN|FAB$M_PRN))
	  strcat (rat, ", blk");
	else
	  strcat (rat, "blk");
    }
  else
    {
      strcpy (rfm, vms_stmlf_recfm ? "rfm = stmlf" : "rfm=var");
      strcpy (rat, "rat=cr");
    }
  /* Until the VAX C RTL fixes the many bugs with modes, always use
     mode 0 to get the user's default protection. */
  fd = creat (name, 0, rfm, rat);
  if (fd < 0 && errno == EEXIST)
    {
      if (unlink (name) < 0)
	report_file_error ("delete", build_string (name));
      fd = creat (name, 0, rfm, rat);
    }
  return fd;
}
#endif /* creat */

/* fwrite to stdout is S L O W.  Speed it up by using fputc...*/
int
sys_fwrite (ptr, size, num, fp)
     register char * ptr;
     FILE * fp;
{
  register int tot = num * size;

  while (tot--)
    fputc (*ptr++, fp);
  return (num);
}

/*
 * The VMS C library routine creat actually creates a new version of an
 * existing file rather than truncating the old version.  There are times
 * when this is not the desired behavior, for instance, when writing an
 * auto save file (you only want one version), or when you don't have
 * write permission in the directory containing the file (but the file
 * itself is writable).  Hence this routine, which is equivalent to 
 * "close (creat (fn, 0));" on Unix if fn already exists.
 */
int
vms_truncate (fn)
     char *fn;
{
  struct FAB xfab = cc$rms_fab;
  struct RAB xrab = cc$rms_rab;
  int status;

  xfab.fab$l_fop = FAB$M_TEF;	/* free allocated but unused blocks on close */
  xfab.fab$b_fac = FAB$M_TRN | FAB$M_GET; /* allow truncate and get access */
  xfab.fab$b_shr = FAB$M_NIL;	/* allow no sharing - file must be locked */
  xfab.fab$l_fna = fn;
  xfab.fab$b_fns = strlen (fn);
  xfab.fab$l_dna = ";0";	/* default to latest version of the file */
  xfab.fab$b_dns = 2;
  xrab.rab$l_fab = &xfab;

  /* This gibberish opens the file, positions to the first record, and
     deletes all records from there until the end of file. */
  if ((SYS$OPEN (&xfab) & 01) == 01)
    {
      if ((SYS$CONNECT (&xrab) & 01) == 01 &&
	  (SYS$FIND (&xrab) & 01) == 01 &&
	  (SYS$TRUNCATE (&xrab) & 01) == 01)
	status = 0;
      else
	status = -1;
    }
  else
    status = -1;
  SYS$CLOSE (&xfab);
  return status;
}

/* Define this symbol to actually read SYSUAF.DAT.  This requires either
   SYSPRV or a readable SYSUAF.DAT. */

#ifdef READ_SYSUAF
/*
 * getuaf.c
 *
 * Routine to read the VMS User Authorization File and return
 * a specific user's record.
 */

static struct UAF vms_retuaf;

static struct UAF *
get_uaf_name (uname)
     char * uname;
{
  register status;
  struct FAB uaf_fab;
  struct RAB uaf_rab;
  
  uaf_fab = cc$rms_fab;
  uaf_rab = cc$rms_rab;
  /* initialize fab fields */
  uaf_fab.fab$l_fna = "SYS$SYSTEM:SYSUAF.DAT";
  uaf_fab.fab$b_fns = 21;
  uaf_fab.fab$b_fac = FAB$M_GET;
  uaf_fab.fab$b_org = FAB$C_IDX;
  uaf_fab.fab$b_shr = FAB$M_GET|FAB$M_PUT|FAB$M_UPD|FAB$M_DEL;
  /* initialize rab fields */
  uaf_rab.rab$l_fab = &uaf_fab;
  /* open the User Authorization File */
  status = SYS$OPEN (&uaf_fab);
  if (!(status&1))
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return 0;
    }
  status = SYS$CONNECT (&uaf_rab);
  if (!(status&1))
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return 0;
    }
  /* read the requested record - index is in uname */
  uaf_rab.rab$l_kbf = uname;
  uaf_rab.rab$b_ksz = strlen (uname);
  uaf_rab.rab$b_rac = RAB$C_KEY;
  uaf_rab.rab$l_ubf = (char *)&vms_retuaf;
  uaf_rab.rab$w_usz = sizeof vms_retuaf;
  status = SYS$GET (&uaf_rab);
  if (!(status&1))
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return 0;
    }
  /* close the User Authorization File */
  status = SYS$DISCONNECT (&uaf_rab);
  if (!(status&1))
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return 0;
    }
  status = SYS$CLOSE (&uaf_fab);
  if (!(status&1))
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return 0;
    }
  return &vms_retuaf;
}

static struct UAF *
get_uaf_uic (uic)
     unsigned long uic;
{
  register status;
  struct FAB uaf_fab;
  struct RAB uaf_rab;
  
  uaf_fab = cc$rms_fab;
  uaf_rab = cc$rms_rab;
  /* initialize fab fields */
  uaf_fab.fab$l_fna = "SYS$SYSTEM:SYSUAF.DAT";
  uaf_fab.fab$b_fns = 21;
  uaf_fab.fab$b_fac = FAB$M_GET;
  uaf_fab.fab$b_org = FAB$C_IDX;
  uaf_fab.fab$b_shr = FAB$M_GET|FAB$M_PUT|FAB$M_UPD|FAB$M_DEL;
  /* initialize rab fields */
  uaf_rab.rab$l_fab = &uaf_fab;
  /* open the User Authorization File */
  status = SYS$OPEN (&uaf_fab);
  if (!(status&1))
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return 0;
    }
  status = SYS$CONNECT (&uaf_rab);
  if (!(status&1))
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return 0;
    }
  /* read the requested record - index is in uic */
  uaf_rab.rab$b_krf = 1;	/* 1st alternate key */
  uaf_rab.rab$l_kbf = (char *) &uic;
  uaf_rab.rab$b_ksz = sizeof uic;
  uaf_rab.rab$b_rac = RAB$C_KEY;
  uaf_rab.rab$l_ubf = (char *)&vms_retuaf;
  uaf_rab.rab$w_usz = sizeof vms_retuaf;
  status = SYS$GET (&uaf_rab);
  if (!(status&1))
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return 0;
    }
  /* close the User Authorization File */
  status = SYS$DISCONNECT (&uaf_rab);
  if (!(status&1))
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return 0;
    }
  status = SYS$CLOSE (&uaf_fab);
  if (!(status&1))
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return 0;
    }
  return &vms_retuaf;
}

static struct passwd vms_retpw;

static struct passwd *
cnv_uaf_pw (up)
     struct UAF * up;
{
  char * ptr;

  /* copy these out first because if the username is 32 chars, the next
     section will overwrite the first byte of the UIC */
  vms_retpw.pw_uid = up->uaf$w_mem;
  vms_retpw.pw_gid = up->uaf$w_grp;

  /* I suppose this is not the best sytle, to possibly overwrite one
     byte beyond the end of the field, but what the heck... */
  ptr = &up->uaf$t_username[UAF$S_USERNAME];
  while (ptr[-1] == ' ')
    ptr--;
  *ptr = '\0';
  strcpy (vms_retpw.pw_name, up->uaf$t_username);

  /* the rest of these are counted ascii strings */
  strncpy (vms_retpw.pw_gecos, &up->uaf$t_owner[1], up->uaf$t_owner[0]);
  vms_retpw.pw_gecos[up->uaf$t_owner[0]] = '\0';
  strncpy (vms_retpw.pw_dir, &up->uaf$t_defdev[1], up->uaf$t_defdev[0]);
  vms_retpw.pw_dir[up->uaf$t_defdev[0]] = '\0';
  strncat (vms_retpw.pw_dir, &up->uaf$t_defdir[1], up->uaf$t_defdir[0]);
  vms_retpw.pw_dir[up->uaf$t_defdev[0] + up->uaf$t_defdir[0]] = '\0';
  strncpy (vms_retpw.pw_shell, &up->uaf$t_defcli[1], up->uaf$t_defcli[0]);
  vms_retpw.pw_shell[up->uaf$t_defcli[0]] = '\0';

  return &vms_retpw;
}
#else /* not READ_SYSUAF */
static struct passwd vms_retpw;
#endif /* not READ_SYSUAF */

struct passwd *
getpwnam (name)
     char * name;
{
#ifdef READ_SYSUAF
  struct UAF *up;
#else
  char * user;
  char * dir;
  unsigned char * full;
#endif /* READ_SYSUAF */
  char *ptr = name;

  while (*ptr)
    {
      if ('a' <= *ptr && *ptr <= 'z')
	*ptr -= 040;
      ptr++;
    }
#ifdef READ_SYSUAF
  if (!(up = get_uaf_name (name)))
    return 0;
  return cnv_uaf_pw (up);
#else
  if (strcmp (name, getenv ("USER")) == 0)
    {
      vms_retpw.pw_uid = getuid ();
      vms_retpw.pw_gid = getgid ();
      strcpy (vms_retpw.pw_name, name);
      if (full = egetenv ("FULLNAME"))
	strcpy (vms_retpw.pw_gecos, full);
      else
	*vms_retpw.pw_gecos = '\0';
      strcpy (vms_retpw.pw_dir, egetenv ("HOME"));
      *vms_retpw.pw_shell = '\0';
      return &vms_retpw;
    }
  else
    return 0;
#endif /* not READ_SYSUAF */
}

struct passwd *
getpwuid (uid)
     unsigned long uid;
{
#ifdef READ_SYSUAF
  struct UAF * up;

  if (!(up = get_uaf_uic (uid)))
    return 0;
  return cnv_uaf_pw (up);
#else
  if (uid == sys_getuid ())
    return getpwnam (egetenv ("USER"));
  else
    return 0;
#endif /* not READ_SYSUAF */
}

/* return total address space available to the current process.  This is
   the sum of the current p0 size, p1 size and free page table entries
   available. */
int
vlimit ()
{
  int item_code;
  unsigned long free_pages;
  unsigned long frep0va;
  unsigned long frep1va;
  register status;

  item_code = JPI$_FREPTECNT;
  if (((status = LIB$GETJPI (&item_code, 0, 0, &free_pages)) & 1) == 0)
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return -1;
    }
  free_pages *= 512;

  item_code = JPI$_FREP0VA;
  if (((status = LIB$GETJPI (&item_code, 0, 0, &frep0va)) & 1) == 0)
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return -1;
    }
  item_code = JPI$_FREP1VA;
  if (((status = LIB$GETJPI (&item_code, 0, 0, &frep1va)) & 1) == 0)
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return -1;
    }

  return free_pages + frep0va + (0x7fffffff - frep1va);
}

int
define_logical_name (varname, string)
     char *varname;
     char *string;
{
  struct dsc$descriptor_s strdsc =
    {strlen (string), DSC$K_DTYPE_T, DSC$K_CLASS_S, string};
  struct dsc$descriptor_s envdsc =
    {strlen (varname), DSC$K_DTYPE_T, DSC$K_CLASS_S, varname};
  struct dsc$descriptor_s lnmdsc =
    {7, DSC$K_DTYPE_T, DSC$K_CLASS_S, "LNM$JOB"};

  return LIB$SET_LOGICAL (&envdsc, &strdsc, &lnmdsc, 0, 0);
}

int
delete_logical_name (varname)
     char *varname;
{
  struct dsc$descriptor_s envdsc =
    {strlen (varname), DSC$K_DTYPE_T, DSC$K_CLASS_S, varname};
  struct dsc$descriptor_s lnmdsc =
    {7, DSC$K_DTYPE_T, DSC$K_CLASS_S, "LNM$JOB"};

  return LIB$DELETE_LOGICAL (&envdsc, &lnmdsc);
}

execvp ()
{
  error ("execvp system call not implemented");
}

int
rename (from, to)
     char *from, *to;
{
  int status;
  struct FAB from_fab = cc$rms_fab, to_fab = cc$rms_fab;
  struct NAM from_nam = cc$rms_nam, to_nam = cc$rms_nam;
  char from_esn[NAM$C_MAXRSS];
  char to_esn[NAM$C_MAXRSS];

  from_fab.fab$l_fna = from;
  from_fab.fab$b_fns = strlen (from);
  from_fab.fab$l_nam = &from_nam;
  from_fab.fab$l_fop = FAB$M_NAM;

  from_nam.nam$l_esa = from_esn;
  from_nam.nam$b_ess = sizeof from_esn;

  to_fab.fab$l_fna = to;
  to_fab.fab$b_fns = strlen (to);
  to_fab.fab$l_nam = &to_nam;
  to_fab.fab$l_fop = FAB$M_NAM;

  to_nam.nam$l_esa = to_esn;
  to_nam.nam$b_ess = sizeof to_esn;

  status = SYS$RENAME (&from_fab, 0, 0, &to_fab);

  if (status & 1)
    return 0;
  else
    {
      if (status == RMS$_DEV)
	errno = EXDEV;
      else
	errno = EVMSERR;
      vaxc$errno = status;
      return -1;
    }
}

/* This function renames a file like `rename', but it strips
   the version number from the "to" filename, such that the "to" file is
   will always be a new version.  It also sets the file protection once it is
   finished.  The protection that we will use is stored in vms_fab_final_pro,
   and was set when we did a creat_copy_attrs to create the file that we
   are renaming.

   We could use the chmod function, but Eunichs uses 3 bits per user category
   to describe the protection, and VMS uses 4 (write and delete are separate
   bits).  To maintain portability, the VMS implementation of `chmod' wires
   the W and D bits together.  */

 
static char vms_file_written[NAM$C_MAXRSS];

int
rename_sans_version (from,to)
     char *from, *to;
{
  short int chan;
  int stat;
  short int iosb[4];
  int status;
  struct fibdef fib;
  struct FAB to_fab = cc$rms_fab;
  struct NAM to_nam = cc$rms_nam;
  struct dsc$descriptor fib_d ={sizeof (fib),0,0,(char*) &fib};
  struct dsc$descriptor fib_attr[2]
    = {{sizeof (vms_fab_final_pro),ATR$C_FPRO,0,(char*) &vms_fab_final_pro},{0,0,0,0}};
  char to_esn[NAM$C_MAXRSS];

  $DESCRIPTOR (disk,to_esn);

  memset (&fib, 0, sizeof (fib));

  to_fab.fab$l_fna = to;
  to_fab.fab$b_fns = strlen (to);
  to_fab.fab$l_nam = &to_nam;
  to_fab.fab$l_fop = FAB$M_NAM;

  to_nam.nam$l_esa = to_esn;
  to_nam.nam$b_ess = sizeof to_esn;

  status = SYS$PARSE (&to_fab, 0, 0); /* figure out the full file name */

  if (to_nam.nam$l_fnb && NAM$M_EXP_VER)
    *(to_nam.nam$l_ver) = '\0';

  stat = rename (from, to_esn);
  if (stat < 0)
    return stat;

  strcpy (vms_file_written, to_esn);

  to_fab.fab$l_fna = vms_file_written; /* this points to the versionless name */
  to_fab.fab$b_fns = strlen (vms_file_written);

  /* Now set the file protection to the correct value */
  SYS$OPEN (&to_fab, 0, 0);	/* This fills in the nam$w_fid fields */

  /* Copy these fields into the fib */
  fib.fib$r_fid_overlay.fib$w_fid[0] = to_nam.nam$w_fid[0];
  fib.fib$r_fid_overlay.fib$w_fid[1] = to_nam.nam$w_fid[1];
  fib.fib$r_fid_overlay.fib$w_fid[2] = to_nam.nam$w_fid[2];

  SYS$CLOSE (&to_fab, 0, 0);

  stat = SYS$ASSIGN (&disk, &chan, 0, 0); /* open a channel to the disk */
  if (!stat)
    LIB$SIGNAL (stat);
  stat = SYS$QIOW (0, chan, IO$_MODIFY, iosb, 0, 0, &fib_d,
		   0, 0, 0, &fib_attr, 0);
  if (!stat)
    LIB$SIGNAL (stat);
  stat = SYS$DASSGN (chan);
  if (!stat)
    LIB$SIGNAL (stat);
  strcpy (vms_file_written, to_esn); /* We will write this to the terminal*/
  return 0;
}

int
link (file, new)
     char * file, * new;
{
  register status;
  struct FAB fab;
  struct NAM nam;
  unsigned short fid[3];
  char esa[NAM$C_MAXRSS];

  fab = cc$rms_fab;
  fab.fab$l_fop = FAB$M_OFP;
  fab.fab$l_fna = file;
  fab.fab$b_fns = strlen (file);
  fab.fab$l_nam = &nam;

  nam = cc$rms_nam;
  nam.nam$l_esa = esa;
  nam.nam$b_ess = NAM$C_MAXRSS;

  status = SYS$PARSE (&fab);
  if ((status & 1) == 0)
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return -1;
    }
  status = SYS$SEARCH (&fab);
  if ((status & 1) == 0)
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return -1;
    }

  fid[0] = nam.nam$w_fid[0];
  fid[1] = nam.nam$w_fid[1];
  fid[2] = nam.nam$w_fid[2];

  fab.fab$l_fna = new;
  fab.fab$b_fns = strlen (new);

  status = SYS$PARSE (&fab);
  if ((status & 1) == 0)
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return -1;
    }

  nam.nam$w_fid[0] = fid[0];
  nam.nam$w_fid[1] = fid[1];
  nam.nam$w_fid[2] = fid[2];

  nam.nam$l_esa = nam.nam$l_name;
  nam.nam$b_esl = nam.nam$b_name + nam.nam$b_type + nam.nam$b_ver;

  status = SYS$ENTER (&fab);
  if ((status & 1) == 0)
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return -1;
    }

  return 0;
}

long
random ()
{
  /* Arrange to return a range centered on zero.  */
  return rand () - (1 << 30);
}

void
srandom (int seed)
{
  srand (seed);
}
#endif /* VMS */

#ifdef AIX

/* Called from init_sys_modes.  */
void
hft_init ()
{
  int junk;

  /* If we're not on an HFT we shouldn't do any of this.  We determine
     if we are on an HFT by trying to get an HFT error code.  If this
     call fails, we're not on an HFT. */ 
#ifdef IBMR2AIX
  if (ioctl (0, HFQERROR, &junk) < 0)
    return;
#else /* not IBMR2AIX */
  if (ioctl (0, HFQEIO, 0) < 0)
    return;
#endif /* not IBMR2AIX */

  /* On AIX the default hft keyboard mapping uses backspace rather than delete
     as the rubout key's ASCII code.  Here this is changed.  The bug is that
     there's no way to determine the old mapping, so in reset_sys_modes
     we need to assume that the normal map had been present.  Of course, this
     code also doesn't help if on a terminal emulator which doesn't understand
     HFT VTD's. */
  {
    struct hfbuf buf;
    struct hfkeymap keymap;

    buf.hf_bufp = (char *)&keymap;
    buf.hf_buflen = sizeof (keymap);
    keymap.hf_nkeys = 2;
    keymap.hfkey[0].hf_kpos = 15;
    keymap.hfkey[0].hf_kstate = HFMAPCHAR | HFSHFNONE;
#ifdef IBMR2AIX
    keymap.hfkey[0].hf_keyidh = '<';
#else /* not IBMR2AIX */
    keymap.hfkey[0].hf_page = '<';
#endif /* not IBMR2AIX */
    keymap.hfkey[0].hf_char = 127;
    keymap.hfkey[1].hf_kpos = 15;
    keymap.hfkey[1].hf_kstate = HFMAPCHAR | HFSHFSHFT;
#ifdef IBMR2AIX
    keymap.hfkey[1].hf_keyidh = '<';
#else /* not IBMR2AIX */
    keymap.hfkey[1].hf_page = '<';
#endif /* not IBMR2AIX */
    keymap.hfkey[1].hf_char = 127;
    hftctl (0, HFSKBD, &buf);
  }
  /* The HFT system on AIX doesn't optimize for scrolling, so it's really ugly
     at times. */
  line_ins_del_ok = char_ins_del_ok = 0;
}

/* Reset the rubout key to backspace. */

void
hft_reset ()
{
  struct hfbuf buf;
  struct hfkeymap keymap;
  int junk;

#ifdef IBMR2AIX
  if (ioctl (0, HFQERROR, &junk) < 0)
    return;
#else /* not IBMR2AIX */
  if (ioctl (0, HFQEIO, 0) < 0)
    return;
#endif /* not IBMR2AIX */

  buf.hf_bufp = (char *)&keymap;
  buf.hf_buflen = sizeof (keymap);
  keymap.hf_nkeys = 2;
  keymap.hfkey[0].hf_kpos = 15;
  keymap.hfkey[0].hf_kstate = HFMAPCHAR | HFSHFNONE;
#ifdef IBMR2AIX
  keymap.hfkey[0].hf_keyidh = '<';
#else /* not IBMR2AIX */
  keymap.hfkey[0].hf_page = '<';
#endif /* not IBMR2AIX */
  keymap.hfkey[0].hf_char = 8;
  keymap.hfkey[1].hf_kpos = 15;
  keymap.hfkey[1].hf_kstate = HFMAPCHAR | HFSHFSHFT;
#ifdef IBMR2AIX
  keymap.hfkey[1].hf_keyidh = '<';
#else /* not IBMR2AIX */
  keymap.hfkey[1].hf_page = '<';
#endif /* not IBMR2AIX */
  keymap.hfkey[1].hf_char = 8;
  hftctl (0, HFSKBD, &buf);
}

#endif /* AIX */
