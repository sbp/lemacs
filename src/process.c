/* Asynchronous subprocess control for GNU Emacs.
   Copyright (C) 1985, 1986, 1987, 1988, 1992, 1993, 1994
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

/* #include <signal.h>  use "syssignal.h" instead -jwz */

#if defined(sun) && !defined(USG) && !defined(MACH)
# include <vfork.h>
#endif

#ifdef VMS
/* Prevent the file from being totally empty.  */
static dummy () {}
#endif

#ifdef subprocesses
/* The entire file is within this conditional */

#include <stdio.h>
#include <errno.h>
#include <setjmp.h>
#include <sys/types.h>		/* some typedefs are used in sys/file.h */
#include <sys/file.h>
#include <sys/stat.h>

#ifdef HAVE_SOCKETS	/* TCP connection support, if kernel can do it */
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#endif /* HAVE_SOCKETS */

/* TERM is a poor-man's SLIP, used on Linux.  */
#ifdef TERM
# include <client.h>
#endif

/* DGUX inet_addr returns a 'struct in_addr'. */
#ifdef DGUX
#define IN_ADDR struct in_addr
#define NUMERIC_ADDR_ERROR (numeric_addr.s_addr == -1)
#else
#define IN_ADDR unsigned long
#define NUMERIC_ADDR_ERROR (numeric_addr == -1)
#endif

#if defined(BSD) || defined(STRIDE)
#include <sys/ioctl.h>
#if !defined (O_NDELAY) && defined (HAVE_PTYS) && !defined(USG5)
#include <fcntl.h>
#endif /* HAVE_PTYS and no O_NDELAY */
#endif /* BSD or STRIDE */

#ifdef NEED_BSDTTY
#include <bsdtty.h> /*>>> <sys/bsdtty.h> ?? */
#endif

#ifdef IRIS
#include <sys/sysmacros.h>	/* for "minor" */
#endif /* not IRIS */

#include "systime.h"
#include "systty.h"

#include "lisp.h"
#include "intl.h"
#include "window.h"
#include "buffer.h"
#include "insdel.h"
#include "process.h"
#include "termhooks.h"
#include "termopts.h"
#include "commands.h"

#include "events.h"

/* a process object is a network connection when its pid field a cons
   (name of name of port we are connected to . foreign host name) */


/* Valid values of process->status_symbol */
Lisp_Object Qrun, Qstop; /* Qexit from eval.c, Qsignal from data.c. */
/* Qrun => Qopen, Qexit => Qclosed for "network connection" processes */
Lisp_Object Qopen, Qclosed;

/* Define first descriptor number available for subprocesses.  */
#ifdef VMS
#define FIRST_PROC_DESC 1
#else /* Not VMS */
#define FIRST_PROC_DESC 3
#endif

/* Define SIGCHLD as an alias for SIGCLD.  There are many conditionals
   testing SIGCHLD.  */

#if !defined (SIGCHLD) && defined (SIGCLD)
#define SIGCHLD SIGCLD
#endif /* SIGCLD */

#include "syssignal.h"

#include "sysdep.h"

#include "syswait.h"

/* Define the structure that the wait system call stores.
   On many systems, there is a structure defined for this.
   But on vanilla-ish USG systems there is not.  */

#ifdef VMS
#define sys_siglist sys_errlist
#endif

/* t means use pty, nil means use a pipe,
   maybe other values to come.  */
static Lisp_Object Vprocess_connection_type;

#ifdef SKTPAIR
#ifndef HAVE_SOCKETS
#include <sys/socket.h>
#endif
#endif /* SKTPAIR */

#ifndef NSIG
#define NSIG (SIGUSR2+1) /* guess how many elements are in sys_siglist... */
#endif

/* Number of events of change of status of a process.  */
static int process_tick;

/* Number of events for which the user or sentinel has been notified.  */
static int update_tick;

#ifdef FD_SET
/* We could get this from param.h, but better not to depend on finding that.
   And better not to risk that it might define other symbols used in this
   file.  */
#ifdef FD_SETSIZE
#define MAXDESC FD_SETSIZE
#else
#define MAXDESC 64
#endif
#define SELECT_TYPE fd_set
#else /* no FD_SET */
#define MAXDESC 32
#define SELECT_TYPE int

/* Define the macros to access a single-int bitmap of descriptors.  */
#define FD_SET(n, p) (*(p) |= (1 << (n)))
#define FD_CLR(n, p) (*(p) &= ~(1 << (n)))
#define FD_ISSET(n, p) (*(p) & (1 << (n)))
#define FD_ZERO(p) (*(p) = 0)
#endif /* no FD_SET */

/* Mask of bits indicating the descriptors that we wait for input on */
static SELECT_TYPE input_wait_mask;

/* Nonzero means delete a process right away if it exits.  */
int delete_exited_processes;

/* Indexed by descriptor, gives the process (if any) for that descriptor */
static Lisp_Object chan_process[MAXDESC];

/* List of process objects. */
Lisp_Object Vprocess_list;

Lisp_Object Qprocessp;

/* Buffered-ahead input char from process, indexed by channel.
   -1 means empty (no char is buffered).
   Used on sys V where the only way to tell if there is any
   output from the process is to read at least one char.
   Always -1 on systems that support FIONREAD.  */
static int proc_buffered_char[MAXDESC];

#ifdef BROKEN_O_NONBLOCK
#undef O_NONBLOCK
#endif

/*
 * Structure records pertinent information about open channels.
 * There is one channel associated with each process.
 */

struct Lisp_Process
  {
    struct lcrecord_header header;
    /* Name of this process */
    Lisp_Object name;
    /* List of command arguments that this process was run with */
    Lisp_Object command;
    /* (funcall FILTER PROC STRING)  (if FILTER is non-nil)
       to dispose of a bunch of chars from the process all at once */
    Lisp_Object filter;
    /* (funcall SENTINEL PROCESS) when process state changes */
    Lisp_Object sentinel;
    /* Buffer that output is going to */
    Lisp_Object buffer;
    /* Marker set to end of last buffer-inserted output from this process */
    Lisp_Object mark;
    /* Lisp_Int of subprocess' PID, or a cons of
       service/host if this is really a network connection */
    Lisp_Object pid;

    /* Symbol indicating status of process.
       This may be a symbol: run, stop, exit, signal */
    Lisp_Object status_symbol;

    /* Non-0 if this is really a command channel.  Only used by the
       ToolTalk stuff. */
    int command_channel_p;

    /* Exit code if process has terminated,
       signal which stopped/interrupted process
       or 0 if process is running */
    int exit_code;
    /* Number of this process */
    /* Non-false if process has exited and "dumped core" on its way down */
    char core_dumped;
    /* Descriptor by which we read from this process.  -1 for dead process */
    int infd;
    /* Descriptor by which we write to this process. -1 for dead process */
    int outfd;
    /* Descriptor for the tty which this process is using.
       -1 if we didn't record it (on some systems, there's no need).  */
    int subtty;
    /* Non-false if communicating through a pty.  */
    char pty_flag;
    /* This next field is only actually used #ifdef ENERGIZE */
    /* if this flag is not NIL, then filter will do the read on the
       channel, rather than having a call to make_string.
       This only works if the filter is a subr. */
    char filter_does_read;
    /* Non-nil means kill silently if Emacs is exited.  */
    char kill_without_query;
    /* Event-count of last event in which this process changed status.  */
    int tick;
    /* Event-count of last such event reported.  */
    int update_tick;
  };

static Lisp_Object mark_process (Lisp_Object, void (*) (Lisp_Object));
static void print_process (Lisp_Object, Lisp_Object, int);
static void finalize_process (void *, int);
static int process_sizeof (void *h) { return sizeof (struct Lisp_Process); }
DEFINE_LRECORD_IMPLEMENTATION (lrecord_process,
                               mark_process, print_process, finalize_process,
                               process_sizeof, 0);

static Lisp_Object
mark_process (Lisp_Object obj, void (*markobj) (Lisp_Object))
{
  struct Lisp_Process *proc = XPROCESS (obj);
  ((markobj) (proc->name));
  ((markobj) (proc->command));
  ((markobj) (proc->filter));
  ((markobj) (proc->sentinel));
  ((markobj) (proc->buffer));
  ((markobj) (proc->mark));
  ((markobj) (proc->pid));
  return (proc->status_symbol);
}

static void
print_process (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  struct Lisp_Process *proc = XPROCESS (obj);
  
  if (print_readably)
    error (GETTEXT ("printing unreadable object #<process %s>"),
           XSTRING (proc->name)->data);
      
  if (!escapeflag)
    {
      print_internal (proc->name, printcharfun, 0);
    }
  else
    {
      int netp = network_connection_p (obj);
      write_string_1 (((netp) ? GETTEXT ("#<network connection ") :
		       GETTEXT ("#<process ")),
		      -1, printcharfun);
      print_internal (proc->name, printcharfun, 1);
      write_string_1 (((netp) ? " " : " pid "), -1, printcharfun);
      print_internal (proc->pid, printcharfun, 1);
      write_string_1 (" state:", -1, printcharfun);
      print_internal (proc->status_symbol, printcharfun, 1);
      write_string_1 (">", 1, printcharfun);
    }
}

extern void debug_process_finalization (struct Lisp_Process *p);

static void
finalize_process (void *header, int for_disksave)
{
  struct Lisp_Process *p = (struct Lisp_Process *) header;
  if (for_disksave) return; /* hmm, what would this do anyway? */
  debug_process_finalization (p);
}


/* Compute the Lisp form of the process status from
   the numeric status that was returned by `wait'.  */

static void
update_status_from_wait_code (struct Lisp_Process *p, WAITTYPE *w_fmh)
{
  /* C compiler lossage when attempting to pass w directly */
  WAITTYPE w = *w_fmh;

  if (WIFSTOPPED (w))
    {
      p->status_symbol = Qstop;
      p->exit_code = WSTOPSIG (w);
      p->core_dumped = 0;
    }
  else if (WIFEXITED (w))
    {
      p->status_symbol = Qexit;
      p->exit_code = WRETCODE (w);
      p->core_dumped = ((WCOREDUMP (w)) ? 1 : 0);
    }
  else if (WIFSIGNALED (w))
    {
      p->status_symbol = Qsignal;
      p->exit_code = (int) WTERMSIG (w);
      p->core_dumped = ((WCOREDUMP (w)) ? 1 : 0);
    }
  else
    {
      p->status_symbol = Qrun;
      p->exit_code = 0;
    }
}

void
update_process_status (Lisp_Object p,
		       Lisp_Object status_symbol,
		       int exit_code,
		       int core_dumped)
{
  XPROCESS (p)->tick = ++process_tick;
  XPROCESS (p)->status_symbol = status_symbol;
  XPROCESS (p)->exit_code = exit_code;
  XPROCESS (p)->core_dumped = core_dumped;
}

/* Return a string describing a process status list.  */

static Lisp_Object 
status_message (p)
     struct Lisp_Process *p;
{
  Lisp_Object symbol = p->status_symbol;
  int code = p->exit_code;
  int coredump = p->core_dumped;
  Lisp_Object string, string2;

  if (EQ (symbol, Qsignal) || EQ (symbol, Qstop))
    {
      string = build_string (code < NSIG ? sys_siglist[code] :
			     GETTEXT ("unknown"));
      string2 = build_string (coredump ? GETTEXT (" (core dumped)\n") : "\n");
      XSTRING (string)->data[0] = DOWNCASE (XSTRING (string)->data[0]);
      return concat2 (string, string2);
    }
  else if (EQ (symbol, Qexit))
    {
      if (code == 0)
	return build_string (GETTEXT ("finished\n"));
      string = Fnumber_to_string (make_number (code));
      string2 = build_string (coredump ? GETTEXT (" (core dumped)\n") : "\n");
      return concat2 (build_string (GETTEXT ("exited abnormally with code ")),
		      concat2 (string, string2));
    }
  else
    return Fcopy_sequence (Fsymbol_name (symbol));
}

#ifdef HAVE_SOCKETS
int
network_connection_p (Lisp_Object process)
{
  return (XGCTYPE (XPROCESS (process)->pid) == Lisp_Cons);
}
#endif


#ifdef HAVE_PTYS

/* Open an available pty, returning a file descriptor.
   Return -1 on failure.
   The file name of the terminal corresponding to the pty
   is left in the variable pty_name.  */

char pty_name[24];

int
allocate_pty ()
{
  struct stat stb;
  register c, i;
  int fd;

  /* Some systems name their pseudoterminals so that there are gaps in
     the usual sequence - for example, on HP9000/S700 systems, there
     are no pseudoterminals with names ending in 'f'.  So we wait for
     three failures in a row before deciding that we've reached the
     end of the ptys.  */
  int failed_count = 0;

#ifdef PTY_ITERATION
  PTY_ITERATION
#else
  for (c = FIRST_PTY_LETTER; c <= 'z'; c++)
    for (i = 0; i < 16; i++)
#endif
      {
#ifdef PTY_NAME_SPRINTF
	PTY_NAME_SPRINTF
#else
	sprintf (pty_name, "/dev/pty%c%x", c, i);
#endif /* no PTY_NAME_SPRINTF */

#ifdef PTY_OPEN
	PTY_OPEN;
#else /* no PTY_OPEN */
#ifdef IRIS
	/* Unusual IRIS code */
 	*ptyv = emacs_open ("/dev/ptc", O_RDWR | O_NDELAY, 0);
 	if (fd < 0)
 	  return -1;
	if (fstat (fd, &stb) < 0)
	  return -1;
#else /* not IRIS */
	if (stat (pty_name, &stb) < 0)
	  {
	    failed_count++;
	    if (failed_count >= 3)
	      return -1;
	  }
	else
	  failed_count = 0;
#ifdef O_NONBLOCK
	fd = emacs_open (pty_name, O_RDWR | O_NONBLOCK, 0);
#else
	fd = emacs_open (pty_name, O_RDWR | O_NDELAY, 0);
#endif
#endif /* not IRIS */
#endif /* no PTY_OPEN */

	if (fd >= 0)
	  {
	    /* check to make certain that both sides are available
	       this avoids a nasty yet stupid bug in rlogins */
#ifdef PTY_TTY_NAME_SPRINTF
	    PTY_TTY_NAME_SPRINTF
#else
            sprintf (pty_name, "/dev/tty%c%x", c, i);
#endif /* no PTY_TTY_NAME_SPRINTF */
#ifndef UNIPLUS
	    if (access (pty_name, 6) != 0)
	      {
		emacs_close (fd);
#if !defined(IRIS) && !defined(__sgi)
		continue;
#else
		return -1;
#endif /* IRIS */
	      }
#endif /* not UNIPLUS */
	    setup_pty (fd);
	    return fd;
	  }
      }
  return -1;
}
#endif /* HAVE_PTYS */

static Lisp_Object
make_process_internal (Lisp_Object name)
{
  Lisp_Object val, name1;
  register int i;
  register struct Lisp_Process *p
    = alloc_lcrecord (sizeof (struct Lisp_Process), lrecord_process);

  /* If name is already in use, modify it until it is unused.  */
  name1 = name;
  for (i = 1; ; i++)
    {
      char suffix[10];
      Lisp_Object tem = Fget_process (name1);
      if (NILP (tem)) 
        break;
      sprintf (suffix, "<%d>", i);
      name1 = concat2 (name, build_string (suffix));
    }
  name = name1;
  p->name = name;

  p->command = Qnil;
  p->filter = Qnil;
  p->sentinel = Qnil;
  p->buffer = Qnil;
  p->mark = Fmake_marker ();
  p->pid = Qt; /* must not be Qnil or a race condition may occur if
		  a SIGCHLD occurs immediately after this function
		  returns */
  p->status_symbol = Qrun;
  p->command_channel_p = 0;
  p->exit_code = 0;
  p->core_dumped = 0;
  p->infd = -1;
  p->outfd = -1;
  p->subtty = 0;
  p->pty_flag = 0;
  p->filter_does_read = 0;
  p->kill_without_query = 0;
  p->tick = 0;
  p->update_tick = 0;

  XSETR (val, Lisp_Process, p);

  Vprocess_list = Fcons (val, Vprocess_list);
  return (val);
}

static void
remove_process (proc)
     register Lisp_Object proc;
{
  Vprocess_list = delq_no_quit (proc, Vprocess_list);
  Fset_marker (XPROCESS (proc)->mark, Qnil, Qnil);

  deactivate_process (proc);
}

DEFUN ("processp", Fprocessp, Sprocessp, 1, 1, 0,
  "Return t if OBJECT is a process.")
  (obj)
     Lisp_Object obj;
{
  return ((PROCESSP (obj)) ? Qt : Qnil);
}

DEFUN ("process-list", Fprocess_list, Sprocess_list, 0, 0, 0,
  "Return a list of all processes.")
  ()
{
  return Fcopy_sequence (Vprocess_list);
}

DEFUN ("get-process", Fget_process, Sget_process, 1, 1, 0,
  "Return the process named NAME, or nil if there is none.")
  (name)
     register Lisp_Object name;
{
  register Lisp_Object tail;

  if (PROCESSP (name))
    return (name);

  if (!gc_in_progress)
    /* this only gets called during GC when emacs is going away as a result
       of a signal or crash. */
    CHECK_STRING (name, 0);

  for (tail = Vprocess_list; CONSP (tail); tail = XCONS (tail)->cdr)
    {
      Lisp_Object proc = XCONS (tail)->car;
      QUIT;
      if (!NILP (Fequal (name, XPROCESS (proc)->name)))
        return (XCONS (tail)->car);
    }
  return Qnil;
}

#ifdef ENERGIZE
extern Lisp_Object energize_get_buffer_process (Lisp_Object);
extern Lisp_Object Fenergize_user_input_buffer_mark (Lisp_Object);
extern Lisp_Object Venergize_process;
#endif

DEFUN ("get-buffer-process", Fget_buffer_process, Sget_buffer_process, 1, 1, 0,
  "Return the (or, a) process associated with BUFFER.\n\
BUFFER may be a buffer or the name of one.")
  (name)
     register Lisp_Object name;
{
  register Lisp_Object buf, tail, proc;

  if (NILP (name)) return Qnil;
  buf = Fget_buffer (name);
  if (NILP (buf)) return Qnil;

#ifdef ENERGIZE
  {
    Lisp_Object p = energize_get_buffer_process (buf);
    if (!NILP (p)) return p;
  }
#endif

  for (tail = Vprocess_list; CONSP (tail); tail = XCONS (tail)->cdr)
    {
      QUIT;
      proc = XCONS (tail)->car;
      if (PROCESSP (proc) && EQ (XPROCESS (proc)->buffer, buf))
	return proc;
    }
  return Qnil;
}

/* This is how commands for the user decode process arguments.  It
   accepts a process, a process name, a buffer, a buffer name, or nil.
   Buffers denote the first process in the buffer, and nil denotes the
   current buffer.  */

Lisp_Object
get_process (name)
     register Lisp_Object name;
{
  register Lisp_Object proc;
  if (NILP (name))
    proc = Fget_buffer_process (Fcurrent_buffer ());
  else
    {
      proc = Fget_process (name);
      if (NILP (proc))
	proc = Fget_buffer_process (Fget_buffer (name));
    }

  if (!NILP (proc))
    return proc;

  if (NILP (name))
    error (GETTEXT ("Current buffer has no process"));
  else
    error (GETTEXT ("Process %s does not exist"), XSTRING (name)->data);
  /* NOTREACHED */
  return Qnil; /* warning suppression */
}

DEFUN ("delete-process", Fdelete_process, Sdelete_process, 1, 1, 0,
  "Delete PROCESS: kill it and forget about it immediately.\n\
PROCESS may be a process or the name of one, or a buffer name.")
  (proc)
     register Lisp_Object proc;
{
  struct Lisp_Process *p;
  proc = get_process (proc);
  p = XPROCESS (proc);
  if (network_connection_p (proc))
    {
      p->status_symbol = Qexit;
      p->exit_code = 0;
      p->core_dumped = 0;
      p->tick = ++process_tick;
    }
  else if (p->infd >= 0)
    {
      Fkill_process (proc, Qnil);
      /* Do this now, since remove_process will make sigchld_handler do nothing.  */
      p->status_symbol = Qsignal;
      p->exit_code = SIGKILL;
      p->core_dumped = 0;
      p->tick = ++process_tick;
      status_notify ();
    }
  remove_process (proc);
  return Qnil;
}

DEFUN ("process-status", Fprocess_status, Sprocess_status, 1, 1, 0,
  "Return the status of PROCESS: a symbol, one of these:\n\
run  -- for a process that is running.\n\
stop -- for a process stopped but continuable.\n\
exit -- for a process that has exited.\n\
signal -- for a process that has got a fatal signal.\n\
open -- for a network stream connection that is open.\n\
closed -- for a network stream connection that is closed.\n\
nil -- if arg is a process name and no such process exists.")
  (proc)
     register Lisp_Object proc;
{
  Lisp_Object status;

  proc = Fget_process (proc);
  if (NILP (proc))
    return (Qnil);

  status = XPROCESS (proc)->status_symbol;
  if (network_connection_p (proc))
    {
      if (EQ (status, Qrun))
	status = Qopen;
      else if (EQ (status, Qexit))
	status = Qclosed;
    }
  return (status);
}

DEFUN ("process-exit-status", Fprocess_exit_status, Sprocess_exit_status,
       1, 1, 0,
  "Return the exit status of PROCESS or the signal number that killed it.\n\
If PROCESS has not yet exited or died, return 0.")
  (proc)
     register Lisp_Object proc;
{
  CHECK_PROCESS (proc, 0);
  return (make_number (XPROCESS (proc)->exit_code));
}

DEFUN ("process-id", Fprocess_id, Sprocess_id, 1, 1, 0,
  "Return the process id of PROCESS.\n\
This is the pid of the Unix process which PROCESS uses or talks to.\n\
For a network connection, this value is a cons of\n\
 (foreign-network-port . foreign-host-name).")
  (proc)
     register Lisp_Object proc;
{
  Lisp_Object pid;
  CHECK_PROCESS (proc, 0);

  pid = XPROCESS (proc)->pid;
  if (network_connection_p (proc))
    /* return (Qnil); */
    return (Fcons (Fcar (pid), Fcdr (pid)));
  else
    return (pid);
}

DEFUN ("process-name", Fprocess_name, Sprocess_name, 1, 1, 0,
  "Return the name of PROCESS, as a string.\n\
This is the name of the program invoked in PROCESS,\n\
possibly modified to make it unique among process names.")
  (proc)
     register Lisp_Object proc;
{
  CHECK_PROCESS (proc, 0);
  return XPROCESS (proc)->name;
}

DEFUN ("process-command", Fprocess_command, Sprocess_command, 1, 1, 0,
  "Return the command that was executed to start PROCESS.\n\
This is a list of strings, the first string being the program executed\n\
and the rest of the strings being the arguments given to it.")
  (proc)
     register Lisp_Object proc;
{
  CHECK_PROCESS (proc, 0);
  return XPROCESS (proc)->command;
}

DEFUN ("set-process-buffer", Fset_process_buffer, Sset_process_buffer,
  2, 2, 0,
  "Set buffer associated with PROCESS to BUFFER (a buffer, or nil).")
  (proc, buffer)
     register Lisp_Object proc, buffer;
{
  CHECK_PROCESS (proc, 0);
  if (!NILP (buffer))
    CHECK_BUFFER (buffer, 1);
  XPROCESS (proc)->buffer = buffer;
  return buffer;
}

DEFUN ("process-buffer", Fprocess_buffer, Sprocess_buffer,
  1, 1, 0,
  "Return the buffer PROCESS is associated with.\n\
Output from PROCESS is inserted in this buffer\n\
unless PROCESS has a filter.")
  (proc)
     register Lisp_Object proc;
{
  CHECK_PROCESS (proc, 0);
  return XPROCESS (proc)->buffer;
}

DEFUN ("process-mark", Fprocess_mark, Sprocess_mark,
  1, 1, 0,
  "Return the marker for the end of the last output from PROCESS.")
  (proc)
     register Lisp_Object proc;
{
  CHECK_PROCESS (proc, 0);
#ifdef ENERGIZE
  if (EQ (proc, Venergize_process)) /* per buffer rather than per process */
    return Fenergize_user_input_buffer_mark (Qnil); /* ## current_buffer ok? */
#endif
  return XPROCESS (proc)->mark;
}

void
set_process_filter (Lisp_Object proc, Lisp_Object filter, int filter_does_read)
{
  CHECK_PROCESS (proc, 0);
  if (EQ (filter, Qt))
    FD_CLR (XPROCESS (proc)->infd, &input_wait_mask);
  else if (EQ (XPROCESS (proc)->filter, Qt))
    FD_SET (XPROCESS (proc)->infd, &input_wait_mask);
  XPROCESS (proc)->filter = filter;
  XPROCESS (proc)->filter_does_read = filter_does_read;
}

DEFUN ("set-process-filter", Fset_process_filter, Sset_process_filter,
  2, 2, 0,
  "Give PROCESS the filter function FILTER; nil means no filter.\n\
t means stop accepting output from the process.\n\
When a process has a filter, each time it does output\n\
the entire string of output is passed to the filter.\n\
The filter gets two arguments: the process and the string of output.\n\
If the process has a filter, its buffer is not used for output.")
  (proc, filter)
     register Lisp_Object proc, filter;
{
  set_process_filter (proc, filter, 0);
  return filter;
}

DEFUN ("process-filter", Fprocess_filter, Sprocess_filter,
  1, 1, 0,
  "Returns the filter function of PROCESS; nil if none.\n\
See `set-process-filter' for more info on filter functions.")
  (proc)
     register Lisp_Object proc;
{
  CHECK_PROCESS (proc, 0);
  return XPROCESS (proc)->filter;
}

DEFUN ("set-process-sentinel", Fset_process_sentinel, Sset_process_sentinel,
  2, 2, 0,
  "Give PROCESS the sentinel SENTINEL; nil for none.\n\
The sentinel is called as a function when the process changes state.\n\
It gets two arguments: the process, and a string describing the change.")
  (proc, sentinel)
     register Lisp_Object proc, sentinel;
{
  CHECK_PROCESS (proc, 0);
  XPROCESS (proc)->sentinel = sentinel;
  return sentinel;
}

DEFUN ("process-sentinel", Fprocess_sentinel, Sprocess_sentinel,
  1, 1, 0,
  "Return the sentinel of PROCESS; nil if none.\n\
See `set-process-sentinel' for more info on sentinels.")
  (proc)
     register Lisp_Object proc;
{
  CHECK_PROCESS (proc, 0);
  return XPROCESS (proc)->sentinel;
}

DEFUN ("process-kill-without-query", Fprocess_kill_without_query,
  Sprocess_kill_without_query, 1, 2, 0,
  "Say no query needed if PROCESS is running when Emacs is exited.\n\
Optional second argument if non-nil says to require a query.\n\
Value is t if a query was formerly required.")
  (proc, require_query_p)
     register Lisp_Object proc, require_query_p;
{
  int tem;

  CHECK_PROCESS (proc, 0);
  tem = XPROCESS (proc)->kill_without_query;
  XPROCESS (proc)->kill_without_query = NILP (require_query_p);

  return (tem ? Qnil : Qt);
}

DEFUN ("process-kill-without-query-p", Fprocess_kill_without_query_p,
  Sprocess_kill_without_query_p, 1, 1, 0,
  "Whether PROC will be killed without query if running when emacs is exited.")
  (proc)
     register Lisp_Object proc;
{
  CHECK_PROCESS (proc, 0);
  return (XPROCESS (proc)->kill_without_query ? Qt : Qnil);
}



/* Starting asynchronous inferior processes.  */

#ifndef VMS
static void create_process (Lisp_Object process, 
                            char **new_argv, CONST char *current_dir);
#endif

static Lisp_Object start_process_unwind (Lisp_Object);

DEFUN ("start-process", Fstart_process, Sstart_process, 3, MANY, 0,
  "Start a program in a subprocess.  Return the process object for it.\n\
Args are NAME BUFFER PROGRAM &rest PROGRAM-ARGS\n\
NAME is name for process.  It is modified if necessary to make it unique.\n\
BUFFER is the buffer or (buffer-name) to associate with the process.\n\
 Process output goes at end of that buffer, unless you specify\n\
 an output stream or filter function to handle the output.\n\
 BUFFER may be also nil, meaning that this process is not associated\n\
 with any buffer\n\
Third arg is program file name.  It is searched for as in the shell.\n\
Remaining arguments are strings to give program as arguments.")
  (nargs, args)
     int nargs;
     register Lisp_Object *args;
{
  Lisp_Object buffer, name, program, proc, current_dir;
  Lisp_Object tem;
  int speccount = specpdl_depth ();
#ifdef VMS
  char *new_argv;
  int len;
#else
  char **new_argv;
#endif
  int i;

  buffer = args[1];
  if (!NILP (buffer))
    buffer = Fget_buffer_create (buffer);

  CHECK_STRING (args[0], 0);    /* name */
  CHECK_STRING (args[2], 2);    /* program */

  /* Make sure that the child will be able to chdir to the current
     buffer's current directory, or its unhandled equivalent.  We
     can't just have the child check for an error when it does the
     chdir, since it's in a vfork.

     We have to GCPRO around this because Fexpand_file_name and
     Funhandled_file_name_directory might call a file name handling
     function.  The argument list is protected by the caller, so all
     we really have to worry about is buffer.  */
  {
    struct gcpro gcpro1, gcpro2; /* Caller gc-protects args[] */

    current_dir = current_buffer->directory;

    GCPRO2 (buffer, current_dir);

    current_dir = 
      expand_and_dir_to_file
	(Funhandled_file_name_directory (current_dir), Qnil);
#if 0	/* This loser breaks ange-ftp */
    if (NILP (Ffile_accessible_directory_p (current_dir)))
      report_file_error (GETTEXT ("Setting current directory"),
			 list1 (current_buffer->directory));
#endif /* 0 */

    UNGCPRO;
  }

  name = args[0];
  program = args[2];

#ifdef VMS
  /* Make a one member argv with all args concatenated
     together separated by a blank.  */
  len = string_length (XSTRING (program)) + 2;
  for (i = 3; i < nargs; i++)
    {
      tem = args[i];
      CHECK_STRING (tem, i);
      len += string_length (XSTRING (tem)) + 1;	/* count the blank */
    }
  new_argv = (char *) alloca (len);
  strcpy (new_argv, XSTRING (program)->data);
  for (i = 3; i < nargs; i++)
    {
      tem = args[i];
      CHECK_STRING (tem, i);
      strcat (new_argv, " ");
      strcat (new_argv, XSTRING (tem)->data);
    }
  /* Need to add code here to check for program existence on VMS */

#else /* not VMS */
  new_argv = (char **) alloca ((nargs - 1) * sizeof (char *));

  new_argv[0] = (char *) XSTRING (program)->data;

  /* If program file name is not absolute, search our path for it */
  if (new_argv[0][0] != '/') /* >>>> !complete_filename_p */
    {
      tem = Qnil;
      locate_file (Vexec_path, program, EXEC_SUFFIXES, &tem, X_OK);
      if (NILP (tem))
	report_file_error (GETTEXT ("Searching for program"), list1 (program));
      new_argv[0] = (char *) XSTRING (tem)->data;
    }

  for (i = 3; i < nargs; i++)
    {
      tem = args[i];
      CHECK_STRING (tem, i);
      new_argv[i - 2] = (char *) XSTRING (tem)->data;
    }
  new_argv[i - 2] = 0;

#endif /* not VMS */

  proc = make_process_internal (name);

  XPROCESS (proc)->buffer = buffer;
  XPROCESS (proc)->command = Flist (nargs - 2, args + 2);

  /* If an error occurs and we can't start the process, we want to
     remove it from the process list.  This means that each error
     check in create_process doesn't need to call remove_process
     itself; it's all taken care of here.  */
  record_unwind_protect (start_process_unwind, proc);

  create_process (proc, new_argv, (char *) XSTRING (current_dir)->data);

  return unbind_to (speccount, proc);
}

/* This function is the unwind_protect form for Fstart_process.  If
   PROC doesn't have its pid set, then we know someone has signalled
   an error and the process wasn't started successfully, so we should
   remove it from the process list.  */
static Lisp_Object
start_process_unwind (Lisp_Object proc)
{
  if (!PROCESSP (proc))
    abort ();

  /* Was PROC started successfully?  */
  if (EQ (XPROCESS (proc)->pid, Qnil))
    remove_process (proc);
  return Qnil;
}

static SIGTYPE
create_process_1 (signo)
     int signo;
{
#ifdef USG
  /* USG systems forget handlers when they are used;
     must reestablish each time */
  signal (signo, create_process_1);
#endif /* USG */
  SIGRETURN;
}

#if 0  /* This doesn't work; see the note before sigchld_handler.  */
#ifdef USG
#ifdef SIGCHLD
/* Mimic blocking of signals on system V, which doesn't really have it.  */

/* Nonzero means we got a SIGCHLD when it was supposed to be blocked.  */
int sigchld_deferred;

static SIGTYPE
create_process_sigchld ()
{
  signal (SIGCHLD, create_process_sigchld);

  sigchld_deferred = 1;
}
#endif
#endif
#endif

#ifndef VMS /* VMS version of this function is in vmsproc.c.  */

static void
create_process (Lisp_Object process, 
                char **new_argv, CONST char *current_dir)
{
  int pid, inchannel, outchannel, forkin, forkout;
  int sv[2];
#if 0 /* ifdef SIGCHLD */
  SIGTYPE (*sigchld)();
#endif
  int pty_flag = 0;
  char **env;
  struct Lisp_Process *p = XPROCESS (process);

  env = environ;

  inchannel = outchannel = -1;

#ifdef HAVE_PTYS
  if (EQ (Vprocess_connection_type, Qt))

#ifdef OSF1
/* use osf pty support for more pty channels */
    openpty(&outchannel, &inchannel, pty_name, 0, 0);
    close(inchannel);
    setup_pty(outchannel);
    inchannel = outchannel;
#else /* OSF1 */
    outchannel = inchannel = allocate_pty ();
#endif /* OSF1 */

  if (inchannel >= 0)
    {
#ifndef USG
      /* On USG systems it does not work to open the pty's tty here
	       and then close and reopen it in the child.  */
#ifdef O_NOCTTY
      /* Don't let this terminal become our controlling terminal
	 (in case we don't have one).  */
      forkout = forkin = emacs_open (pty_name, O_RDWR | O_NOCTTY, 0);
#else
      forkout = forkin = emacs_open (pty_name, O_RDWR, 0);
#endif
      if (forkin < 0)
	report_file_error (GETTEXT ("Opening pty"), Qnil);
#else
      forkin = forkout = -1;
#endif /* not USG */
      pty_flag = 1;
    }
  else
#endif /* HAVE_PTYS */
#ifdef SKTPAIR
    {
      if (socketpair (AF_UNIX, SOCK_STREAM, 0, sv) < 0)
	report_file_error (GETTEXT ("Opening socketpair"), Qnil);
      outchannel = inchannel = sv[0];
      forkout = forkin = sv[1];
    }
#else /* not SKTPAIR */
    {
      int temp;
      temp = pipe (sv);
      if (temp < 0) goto io_failure;
      inchannel = sv[0];
      forkout = sv[1];
      temp = pipe (sv);
      if (temp < 0) goto io_failure;
      outchannel = sv[1];
      forkin = sv[0];
    }
#endif /* not SKTPAIR */

#if 0
  /* Replaced by close_process_descs */
  set_exclusive_use (inchannel);
  set_exclusive_use (outchannel);
#endif

/* Stride people say it's a mystery why this is needed
   as well as the O_NDELAY, but that it fails without this.  */
#if defined (STRIDE) || (defined (pfa) && defined (HAVE_PTYS))
  {
    int one = 1;
    ioctl (inchannel, FIONBIO, &one);
  }
#endif

#ifdef O_NONBLOCK
  fcntl (inchannel, F_SETFL, O_NONBLOCK);
#else
#ifdef O_NDELAY
  fcntl (inchannel, F_SETFL, O_NDELAY);
#endif
#endif

  /* Record this as an active process, with its channels.
     As a result, child_setup will close Emacs's side of the pipes.  */
  chan_process[inchannel] = process;
  p->infd = inchannel;
  p->outfd = outchannel;
  /* Record the tty descriptor used in the subprocess.  */
  p->subtty = forkin;
  p->pty_flag = pty_flag;
  p->status_symbol = Qrun;
  p->exit_code = 0;

  /* Delay interrupts until we have a chance to store
     the new fork's pid in its process structure */
#ifdef SIGCHLD
#ifdef BSD4_1
  sighold (SIGCHLD);
#else /* not BSD4_1 */
#if defined (BSD) || defined (UNIPLUS) || defined (HPUX)
  sigsetmask (sigmask (SIGCHLD));
#else /* ordinary USG */
#if 0
  sigchld_deferred = 0;
  sigchld = signal (SIGCHLD, create_process_sigchld);
#endif
#endif /* ordinary USG */
#endif /* not BSD4_1 */
#endif /* SIGCHLD */

  /* Until we store the proper pid, enable sigchld_handler
     to recognize an unknown pid as standing for this process.
     It is very important not to let this `marker' value persist
     in the table after this function has returned; if it does
     it might cause call-process to hang and subsequent asynchronous
     processes to get their return values scrambled.  */
  p->pid = Qnil;
  /* Turn on the bit for our input from this process now,
     so that even if the process terminates very soon,
     we can clear the bit properly on termination.
     If fork fails, remove_process will clear the bit.  */
  FD_SET (inchannel, &input_wait_mask);

  {
#ifdef EMACS_BTL
    /* when performance monitoring is on, turn it off before the vfork(),
       as the child has no handler for the signal -- when back in the
       parent process, turn it back on if it was really on when you "turned
       it off" */
    extern int cadillac_stop_logging ();	/* #### rename me */
    extern int cadillac_start_logging ();	/* #### rename me */
    int logging_on = 0;
#endif
    /* child_setup must clobber environ on systems with true vfork.
       Protect it from permanent change.  */
    char **save_environ = environ;

#ifdef EMACS_BTL
    logging_on = cadillac_stop_logging ();	/* #### rename me */
#endif

    pid = vfork ();
    if (pid == 0)
      {
	int xforkin = forkin;
	int xforkout = forkout;

#if 0 /* This was probably a mistake--it duplicates code later on,
	 but fails to handle all the cases.  */
	/* Make sure SIGCHLD is not blocked in the child.  */
	sigsetmask (SIGEMPTYMASK);
#endif

	/* Make the pty be the controlling terminal of the process.  */
#ifdef HAVE_PTYS
	/* First, disconnect its current controlling terminal.  */
#ifdef HAVE_SETSID
	setsid ();
#ifdef TIOCSCTTY
	/* Make the pty's terminal the controlling terminal.  */
	if (pty_flag)
	  {
	    /* We ignore the return value
	       because faith@cs.unc.edu says that is necessary on Linux.  */
	    ioctl (xforkin, TIOCSCTTY, 0);
	  }
#endif
#else /* not HAVE_SETSID */
#if defined (USG) && !defined (IRIX)
	/* It's very important to call setpgrp() here and no time
	   afterwards.  Otherwise, we lose our controlling tty which
	   is set when we open the pty. */
	setpgrp ();
#endif /* USG */
#endif /* not HAVE_SETSID */
#ifdef TIOCNOTTY
	/* In 4.3BSD, the TIOCSPGRP bug has been fixed, and now you
	   can do TIOCSPGRP only to the process's controlling tty.  */
	if (pty_flag)
	  {
	    /* I wonder: would just ioctl (0, TIOCNOTTY, 0) work here? 
	       I can't test it since I don't have 4.3.  */
	    int j = emacs_open ("/dev/tty", O_RDWR, 0);
	    ioctl (j, TIOCNOTTY, 0);
	    emacs_close (j);
#ifndef USG
	    /* In order to get a controlling terminal on some versions
	       of BSD, it is necessary to put the process in pgrp 0
	       before it opens the terminal.  */
	    setpgrp (0, 0);
#endif
	  }
#endif /* TIOCNOTTY */

#if !defined (RTU) && !defined (UNIPLUS)
/*** There is a suggestion that this ought to be a
     conditional on TIOCSPGRP.  */
	/* Now close the pty (if we had it open) and reopen it.
	   This makes the pty the controlling terminal of the subprocess.  */
	if (pty_flag)
	  {
	    /* I wonder if close (open (pty_name, ...)) would work?  */
	    if (xforkin >= 0)
	      emacs_close (xforkin);
	    xforkout = xforkin = emacs_open (pty_name, O_RDWR, 0);

	    if (xforkin < 0)
	      abort ();
	  }
#endif /* not UNIPLUS and not RTU */
#ifdef SETUP_SLAVE_PTY
	SETUP_SLAVE_PTY;
#endif /* SETUP_SLAVE_PTY */
#ifdef AIX
	/* On AIX, we've disabled SIGHUP above once we start a child on a pty.
	   Now reenable it in the child, so it will die when we want it to.  */
	if (pty_flag)
	  signal (SIGHUP, SIG_DFL);
#endif
#endif /* HAVE_PTYS */

#ifdef SIGCHLD
#ifdef BSD4_1
	sigrelse (SIGCHLD);
#else /* not BSD4_1 */
#if defined (BSD) || defined (UNIPLUS) || defined (HPUX)
	sigsetmask (SIGEMPTYMASK);
#else /* ordinary USG */
#if 0
	signal (SIGCHLD, sigchld);
#endif
#endif /* ordinary USG */
#endif /* not BSD4_1 */
#endif /* SIGCHLD */

	child_setup_tty (xforkout);
	child_setup (xforkin, xforkout, xforkout,
                     new_argv, env, 1, current_dir);
      }
#ifdef EMACS_BTL
    else if (logging_on)
      cadillac_start_logging ();	/* #### rename me */
#endif

    environ = save_environ;
  }

  if (pid < 0)
    {
      report_file_error (GETTEXT ("Doing vfork"), Qnil);
    }

  p->pid = make_number (pid);

  /* If the subfork execv fails, and it exits,
     this close hangs.  I don't know why.
     So have an interrupt jar it loose.  */
  stop_polling ();
  signal (SIGALRM, create_process_1);
  alarm (1);
#ifdef SYSV4_PTYS
  /* OK to close only if it's not a pty.  Otherwise we need to leave
     it open for ioctl to get pgrp when signals are sent, or to send
     the interrupt characters through if that's how we're signalling
     subprocesses.  Alternately if you are concerned about running out
     of file descriptors, you could just save the tty name and open
     just to do the ioctl.  */
  if (!p->pty_flag)
#endif
    {
      p->subtty = -1;
      if (forkin >= 0)
        emacs_close (forkin);
    }
  alarm (0);
  start_polling ();
  if (forkin != forkout && forkout >= 0)
    emacs_close (forkout);

  /* do this before re-enabling SIGCHLD */
  event_stream->select_process_cb (XPROCESS (process));

#ifdef SIGCHLD
#ifdef BSD4_1
  sigrelse (SIGCHLD);
#else /* not BSD4_1 */
#if defined (BSD) || defined (UNIPLUS) || defined (HPUX)
  sigsetmask (SIGEMPTYMASK);
#else
#ifdef POSIX_SIGNALS	/* Lemacs change for Linux by Raymond L. Toy
	<toy@alydar.crd.ge.com>: "For USG systems, nothing is done about
	catching SIGCHLD because of the #if 0.  I added another (!) #ifdef 
	for POSIX_SIGNALS to do a sigsetmask(SIGEMPTYMASK).  This may only
	be applicable for linux." */
  sigsetmask (SIGEMPTYMASK);
#else /* ordinary USG */
#if 0
  signal (SIGCHLD, sigchld_handler);
  /* Now really handle any of these signals
     that came in during this function.  */
  if (sigchld_deferred)
    kill (getpid (), SIGCHLD);
#endif
#endif /* ordinary USG */
#endif /* POSIX_SIGNALS */
#endif /* not BSD4_1 */
#endif /* SIGCHLD */

  return;

io_failure:
  {
    int temp = errno;
    emacs_close (forkin);
    emacs_close (forkout);
    emacs_close (inchannel);
    emacs_close (outchannel);
    errno = temp;
    report_file_error (GETTEXT ("Opening pty or pipe"), Qnil);
  }
}
#endif /* not VMS */


/* connect to an existing file descriptor.  This is very similar to
   open-network-stream except that it assumes that the connection has
   already been initialized.  It is currently used for ToolTalk
   communication. */
/* #### Is it really sensible to expose this to lisp?  Seems like false
   generality, since special-purpose C code must be involved.  -jwz */

DEFUN ("connect-to-file-descriptor", Fconnect_to_file_descriptor,
       Sconnect_to_file_descriptor, 4, 4, 0,
  "Connect to an existing file descriptor.\n\
Returns a subprocess-object to represent the connection.\n\
Input and output work as for subprocesses; `delete-process' closes it.\n\
Args are NAME BUFFER INFD OUTFD.\n\
NAME is name for process.  It is modified if necessary to make it unique.\n\
BUFFER is the buffer (or buffer-name) to associate with the process.\n\
 Process output goes at end of that buffer, unless you specify\n\
 an output stream or filter function to handle the output.\n\
 BUFFER may be also nil, meaning that this process is not associated\n\
 with any buffer\n\
INFD and OUTFD specify the file descriptors to use for input and\n\
 output, respectively.")
   (name, buffer, infd, outfd)
      Lisp_Object name, buffer, infd, outfd;
{
  Lisp_Object proc;
  int inch;

  CHECK_STRING (name, 0);
  CHECK_FIXNUM (infd, 0);
  CHECK_FIXNUM (outfd, 0);

  inch = XINT(infd);
  if (!NILP (chan_process[inch]))
    error (GETTEXT ("There is already a process connected to fd %d"), inch);
  if (!NILP (buffer))
    buffer = Fget_buffer_create (buffer);
  proc = make_process_internal (name);

  chan_process[inch] = proc;

  XPROCESS (proc)->pid = Fcons (infd, name);
  XPROCESS (proc)->buffer = buffer;
  XPROCESS (proc)->infd = inch;
  XPROCESS (proc)->outfd = XINT(outfd);
  XPROCESS (proc)->command_channel_p = 1;

  FD_SET (inch, &input_wait_mask);

  event_stream->select_process_cb (XPROCESS (proc));

  return proc;
}

/* This is used for ToolTalk.  It is necessary because Lisp_Process is no
   longer accessible outside of this file.  Actually, the variable it used
   was purged, but I added it back in for now.  --Chuck
   */

int
connected_via_file_desc (struct Lisp_Process *p)
{
  return p->command_channel_p;
}


#ifdef HAVE_SOCKETS

/* open a TCP network connection to a given HOST/SERVICE.  Treated
   exactly like a normal process when reading and writing.  Only
   differences are in status display and process deletion.  A network
   connection has no PID; you cannot signal it.  All you can do is
   deactivate and close it via delete-process */

DEFUN ("open-network-stream", Fopen_network_stream, Sopen_network_stream, 
       4, 4, 0, 
  "Open a TCP connection for a service to a host.\n\
Returns a subprocess-object to represent the connection.\n\
Input and output work as for subprocesses; `delete-process' closes it.\n\
Args are NAME BUFFER HOST SERVICE.\n\
NAME is name for process.  It is modified if necessary to make it unique.\n\
BUFFER is the buffer (or buffer-name) to associate with the process.\n\
 Process output goes at end of that buffer, unless you specify\n\
 an output stream or filter function to handle the output.\n\
 BUFFER may be also nil, meaning that this process is not associated\n\
 with any buffer\n\
Third arg is name of the host to connect to, or its IP address.\n\
Fourth arg SERVICE is name of the service desired, or an integer\n\
 specifying a port number to connect to.")
   (name, buffer, host, service)
      Lisp_Object name, buffer, host, service;
{
  Lisp_Object proc;
  struct sockaddr_in address;
  struct servent *svc_info;
  struct hostent *host_info_ptr, host_info;
  char *(addr_list[2]);
  IN_ADDR numeric_addr;
  int s, outch, inch;
  int port;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

  GCPRO4 (name, buffer, host, service);
  CHECK_STRING (name, 0);
  CHECK_STRING (host, 0);
  if (FIXNUMP (service))
    port = htons ((unsigned short) XINT (service));
  else
    {
      CHECK_STRING (service, 0);
      svc_info = getservbyname ((char *) XSTRING (service)->data, "tcp");
      if (svc_info == 0)
	error (GETTEXT ("Unknown service \"%s\""), XSTRING (service)->data);
      port = svc_info->s_port;
    }

#ifndef TERM
  host_info_ptr = gethostbyname ((char *) XSTRING (host)->data);
  if (host_info_ptr == 0)
    /* Attempt to interpret host as numeric inet address */
    {
      numeric_addr = inet_addr ((char *) XSTRING (host)->data);
      if (NUMERIC_ADDR_ERROR)
	error (GETTEXT ("Unknown host \"%s\""), XSTRING (host)->data);

      host_info_ptr = &host_info;
      host_info.h_name = 0;
      host_info.h_aliases = 0;
      host_info.h_addrtype = AF_INET;
#ifdef h_addr
      /* Older machines have only one address slot called h_addr.
	 Newer machines have h_addr_list, but #define h_addr to
	 be its first element.  */
      host_info.h_addr_list = &(addr_list[0]);
#endif
      host_info.h_addr = (char*)(&numeric_addr);
      addr_list[1] = 0;
      host_info.h_length = strlen (addr_list[0]);
    }

  memset (&address, 0, sizeof (address));
  memcpy (&address.sin_addr, host_info_ptr->h_addr, host_info_ptr->h_length);
  address.sin_family = host_info_ptr->h_addrtype;
  address.sin_port = port;

  s = socket (host_info_ptr->h_addrtype, SOCK_STREAM, 0);
  if (s < 0) 
    report_file_error (GETTEXT ("error creating socket"), list1 (name));

  /* jwz: this next bit disappeared at some point and was restored at the
     suggestion of jrs@world.std.com (Rick Sladkey). */
  /* Kernel bugs (on Ultrix at least) cause lossage (not just EINTR)
     when connect is interrupted.  So let's not let it get interrupted.  */
  if (interrupt_input)
    unrequest_sigio ();
  stop_polling ();

 loop:
  if (connect (s, (struct sockaddr *) &address, sizeof (address)) == -1)
    {
      int xerrno = errno;
      if (errno == EINTR)
	goto loop;
      emacs_close (s);

      /* jwz: from jrs@world.std.com (Rick Sladkey), see above. */
      if (interrupt_input)
	request_sigio ();
      start_polling ();

      errno = xerrno;
      report_file_error (GETTEXT ("connection failed"), list2 (host, name));
    }

  /* jwz: from jrs@world.std.com (Rick Sladkey), see above. */
  if (interrupt_input)
    request_sigio ();
  start_polling ();

#else /* TERM */
  s = connect_server (0);
  if (s < 0)
    report_file_error ("error creating socket", Fcons (name, Qnil));
  send_command (s, C_PORT, 0, "%s:%d", XSTRING (host)->data, ntohs (port));
  send_command (s, C_DUMB, 1, 0);
#endif /* TERM */

  inch = s;
  outch = dup (s);
  if (outch < 0)
    {
      emacs_close (s); /* this used to be leaked; from Kyle Jones */
      report_file_error (GETTEXT ("error duplicating socket"), list1 (name));
    }

  if (!NILP (buffer))
    buffer = Fget_buffer_create (buffer);
  proc = make_process_internal (name);

  chan_process[inch] = proc;

#ifdef O_NONBLOCK
  fcntl (inch, F_SETFL, O_NONBLOCK);
#else
#ifdef O_NDELAY
  fcntl (inch, F_SETFL, O_NDELAY);
#endif
#endif

#ifdef AIX
  /* Apparently need this for non-blocking reads on sockets.  It seems that
     O_NONBLOCK applies only to FIFOs?  From lowry@watson.ibm.com (Andy Lowry).
   */
  {
    int one = 1;
    ioctl (inch, FIONBIO, &one);
  }
#endif /* AIX */

  XPROCESS (proc)->pid = Fcons (service, host);
  XPROCESS (proc)->buffer = buffer;
  XPROCESS (proc)->infd = s;
  XPROCESS (proc)->outfd = outch;
  XPROCESS (proc)->command_channel_p = 0;
  FD_SET (inch, &input_wait_mask);

  if (!event_stream && noninteractive)
    fatal (GETTEXT ("this version of emacs only works under X for now;\n\
can't open network connections in -batch mode."));

  event_stream->select_process_cb (XPROCESS (proc));

  UNGCPRO;
  return proc;
}
#endif	/* HAVE_SOCKETS */

void
deactivate_process (Lisp_Object proc)
{
  register int inchannel, outchannel;
  register struct Lisp_Process *p = XPROCESS (proc);

  inchannel = p->infd;
  outchannel = p->outfd;

  if (inchannel >= 0)
    {
      /* Beware SIGCHLD hereabouts. */
      flush_pending_output (inchannel);
#ifdef VMS
      {
	VMS_PROC_STUFF *get_vms_process_pointer (), *vs;
	if (outchannel >= 0) 
          sys$dassgn (outchannel);
	vs = get_vms_process_pointer (XINT (p->pid));
	if (vs)
	  give_back_vms_process_stuff (vs);
      }
#else
      emacs_close (inchannel);
      if (outchannel >= 0 && outchannel != inchannel)
 	emacs_close (outchannel);
#ifdef SYSV4_PTYS
      if (p->subtty != -1)
        emacs_close (p->subtty);
#endif
#endif
      /* Must call this before setting the file descriptors to 0 */
      event_stream->unselect_process_cb (p);

      p->infd = -1;
      p->outfd = -1;
      chan_process[inchannel] = Qnil;
      FD_CLR (inchannel, &input_wait_mask);
    }
}

void
get_process_file_descriptors (struct Lisp_Process *p, int *infd, int *outfd)
{
  if (! p) abort ();
  if ((p->infd  < -1) || (p->infd  > ((int) (MAXDESC)))) abort ();
  if ((p->outfd < -1) || (p->outfd > ((int) (MAXDESC)))) abort ();
  *infd = p->infd;
  *outfd = p->outfd;
}

/* Close all descriptors currently in use for communication
   with subprocess.  This is used in a newly-forked subprocess
   to get rid of irrelevant descriptors.  */

void
close_process_descs ()
{
  int i;
  for (i = 0; i < MAXDESC; i++)
    {
      Lisp_Object process;
      process = chan_process[i];
      if (!NILP (process))
	{
	  int in = XPROCESS (process)->infd;
	  int out = XPROCESS (process)->outfd;
	  if (in >= 0)
	    emacs_close (in);
	  if (out >= 0 && out != in)
	    emacs_close (out);
	}
    }
}

/*  (Faccept_process_output is now in event-stream.c) */

/* This variable is different from waiting_for_input in keyboard.c.
   It is used to communicate to a lisp process-filter/sentinel (via the
   function Fwaiting_for_user_input_p below) whether emacs was waiting
   for user-input when that process-filter was called.
   waiting_for_input cannot be used as that is by definition 0 when
   lisp code is being evalled */
static int waiting_for_user_input_p;

/* Read pending output from the process channel,
   starting with our buffered-ahead character if we have one.
   Yield number of characters read.

   This function reads at most 1024 characters.
   If you want to read all available subprocess output,
   you must call it repeatedly until it returns zero.  */

int
read_process_output (Lisp_Object proc, int channel)
{
  register int nchars;
#ifdef VMS
  char *chars;
#else
  char chars[1024];
#endif
  register Lisp_Object outstream;
  register struct Lisp_Process *p = XPROCESS (proc);

  if (!NILP (p->filter) && (p->filter_does_read))
    {
      Lisp_Object filter_result = call2 (p->filter, proc, Qnil);
      CHECK_FIXNUM (filter_result, 0);
      return XINT (filter_result);
    }

#ifdef VMS
  VMS_PROC_STUFF *vs, *get_vms_process_pointer();

  vs = get_vms_process_pointer (XINT (p->pid));
  if (vs)
    {
      if (!vs->iosb[0])
	return(0);		/* Really weird if it does this */
      if (!(vs->iosb[0] & 1))
	return -1;		/* I/O error */
    }
  else
    error (GETTEXT ("Could not get VMS process pointer"));
  chars = vs->inputBuffer;
  nchars = clean_vms_buffer (chars, vs->iosb[1]);
  if (nchars <= 0)
    {
      start_vms_process_read (vs); /* Crank up the next read on the process */
      return 1;			/* Nothing worth printing, say we got 1 */
    }
#else /* not VMS */

  if (proc_buffered_char[channel] < 0)
    nchars = emacs_read (channel, chars, sizeof (chars));
  else
    {
      chars[0] = proc_buffered_char[channel];
      proc_buffered_char[channel] = -1;
      nchars = emacs_read (channel, chars + 1, sizeof (chars) - 1);
      if (nchars < 0)
	nchars = 1;
      else
	nchars = nchars + 1;
    }
#endif /* not VMS */

  if (nchars <= 0) return nchars;

  outstream = p->filter;
  if (!NILP (outstream))
    {
      /* We inhibit quit here instead of just catching it so that 
	 hitting ^G when a filter happens to be running won't screw
	 it up.  */
      int speccount = specpdl_depth ();
#if 0 /* RMSmacs */
      Lisp_Object odeactivate = Vdeactivate_mark;
#endif
      specbind (Qinhibit_quit, Qt);
      call2 (outstream, proc, make_string (chars, nchars));
#if 0
      /* Handling the process output should not deactivate the mark.  */
      Vdeactivate_mark = odeactivate;
#endif

#ifdef VMS
      start_vms_process_read (vs);
#endif
      unbind_to (speccount, Qnil);
      return (nchars);
    }

  /* If no filter, write into buffer if it isn't dead.  */
  if (!NILP (p->buffer) && !NILP (XBUFFER (p->buffer)->name))
    {
      Lisp_Object old_buffer = Fcurrent_buffer ();
      Lisp_Object old_read_only = Qnil;
      int old_point;
      int old_begv;
      int old_zv;
#if 0 /* RMSmacs */
      Lisp_Object odeactivate = Vdeactivate_mark;
#endif
      struct gcpro gcpro1, gcpro2, gcpro3;

      GCPRO3 (proc, old_buffer, old_read_only /*, odeactivate */);

      Fset_buffer (p->buffer);
      /* From here, use "XPROCESS (proc)" rather than "p", in a pathetic
       *  attempt to believe that Emacs would work with a relocating GC */
      old_point = PT;
      old_begv = BEGV;
      old_zv = ZV;
      old_read_only = current_buffer->read_only;
      current_buffer->read_only = Qnil;

      /* Insert new output into buffer
	 at the current end-of-output marker,
	 thus preserving logical ordering of input and output.  */
      if (XMARKER (XPROCESS (proc)->mark)->buffer)
	SET_PT (marker_position (XPROCESS (proc)->mark));
      else
	SET_PT (old_zv);

      /* If the output marker is outside of the visible region, save
	 the restriction and widen.  */
      if (! (BEGV <= PT && PT <= ZV))
	Fwiden ();

      /* Make sure opoint floats ahead of any new text, just as point
	 would.  */
      if (PT <= old_point)
	old_point += nchars;

      /* Insert after old_begv, but before old_zv.  */
      if (PT < old_begv)
	old_begv += nchars;
      if (PT <= old_zv)
	old_zv += nchars;

#if 0
      /* This screws up intial display of the window.  jla */

      /* Insert before markers in case we are inserting where
	 the buffer's mark is, and the user's next command is Meta-y.  */
      insert_before_markers (chars, nchars);
#else
      insert_raw_string (chars, nchars);
#endif

      Fset_marker (XPROCESS (proc)->mark, 
                   make_number (PT), 
                   XPROCESS (proc)->buffer);

      redraw_mode_line++;

      /* If the restriction isn't what it should be, set it.  */
      if (old_begv != BEGV || old_zv != ZV)
	Fnarrow_to_region (make_number (old_begv), make_number (old_zv));

#if 0 /* RMSmacs */
      /* Handling the process output should not deactivate the mark.  */
      Vdeactivate_mark = odeactivate;
#endif
      current_buffer->read_only = old_read_only;
      SET_PT (old_point);
      Fset_buffer (old_buffer);

      UNGCPRO;
    }
#ifdef VMS
  start_vms_process_read (vs);
#endif
  return (nchars);
}

DEFUN ("waiting-for-user-input-p", Fwaiting_for_user_input_p, Swaiting_for_user_input_p,
       0, 0, 0,
  "Returns non-NIL if emacs is waiting for input from the user.\n\
This is intended for use by asynchronous process output filters and sentinels.")
       ()
{
  return ((waiting_for_user_input_p) ? Qt : Qnil);
}

/* Sending data to subprocess */

static jmp_buf send_process_frame;

SIGTYPE
send_process_trap (int ignore)
{
#ifdef BSD4_1
  sigrelse (SIGPIPE);
  sigrelse (SIGALRM);
#endif /* BSD4_1 */
  longjmp (send_process_frame, 1);
}

static void
send_process (proc, buf, len)
     Lisp_Object proc;
     char *buf;
     int len;
{
  /* Don't use register vars; longjmp can lose them.  */
  int rv;
  unsigned char *procname = XSTRING (XPROCESS (proc)->name)->data;


#ifdef VMS
  struct Lisp_Process *p = XPROCESS (proc);
  VMS_PROC_STUFF *vs, *get_vms_process_pointer();
#endif /* VMS */

  if (! EQ (XPROCESS (proc)->status_symbol, Qrun))
    error (GETTEXT ("Process %s not running"), procname);

#ifdef VMS
  vs = get_vms_process_pointer (XINT (p->pid));
  if (vs == 0)
    error (GETTEXT ("Could not find this process: %x"), XINT (p->pid));
  else if (write_to_vms_process (vs, buf, len))
    ;
#else
  if (!setjmp (send_process_frame))
    {
      while (len > 0)
	{
	  int this_len = len;
	  SIGTYPE (*old_sigpipe)();

	  /* Don't send more than 500 bytes at a time.  */
	  if (this_len > 500)
	    this_len = 500;
	  old_sigpipe = (SIGTYPE (*) ()) signal (SIGPIPE, send_process_trap);
	  rv = emacs_write (XPROCESS (proc)->outfd, buf, this_len);
	  signal (SIGPIPE, old_sigpipe);
	  if (rv < 0)
	    {
	      if (0
#ifdef EWOULDBLOCK
		  || errno == EWOULDBLOCK
#endif
#ifdef EAGAIN
		  || errno == EAGAIN
#endif
		  )
		{
		  /* It would be nice to accept process output here,
		     but that is difficult.  For example, it could
		     garbage what we are sending if that is from a buffer.  */
		  immediate_quit = 1;
		  QUIT;
		  sleep (1);
		  immediate_quit = 0;
		  continue;
		}
	      report_file_error (GETTEXT ("writing to process"), list1 (proc));
	    }
	  buf += rv;
	  len -= rv;
	  /* Allow input from processes between bursts of sending.
	     Otherwise things may get stopped up.  */
	  if (len > 0)
	    Faccept_process_output (Qnil, Qnil, Qnil);
	}
    }
#endif
  else
    {
      XPROCESS (proc)->status_symbol = Qexit;
      XPROCESS (proc)->exit_code = 256; /* >>> SIGPIPE ??? */
      XPROCESS (proc)->core_dumped = 0;
      XPROCESS (proc)->tick = ++process_tick;
      deactivate_process (proc);
#ifdef VMS
      error (GETTEXT ("Error writing to process %s; closed it"), procname);
#else
      error (GETTEXT ("SIGPIPE raised on process %s; closed it"), procname);
#endif
    }
}

DEFUN ("process-send-region", Fprocess_send_region, Sprocess_send_region,
  3, 3, 0,
  "Send current contents of region as input to PROCESS.\n\
PROCESS may be a process name or an actual process.\n\
Called from program, takes three arguments, PROCESS, START and END.\n\
If the region is more than 500 characters long,\n\
it is sent in several bunches.  This may happen even for shorter regions.\n\
Output from processes can arrive in between bunches.")
  (process, start, end)
     Lisp_Object process, start, end;
{
  Lisp_Object proc;
  int start1;

  proc = get_process (process);
  validate_region (&start, &end);

  if (XINT (start) < GPT && XINT (end) > GPT)
    move_gap (current_buffer, XINT (start));

  start1 = XINT (start);
  send_process (proc, (char *) CHAR_ADDRESS (start1),
		XINT (end) - XINT (start));

  return Qnil;
}

DEFUN ("process-send-string", Fprocess_send_string, Sprocess_send_string,
  2, 2, 0,
  "Send PROCESS the contents of STRING as input.\n\
PROCESS may be a process name or an actual process.\n\
If STRING is more than 500 characters long,\n\
it is sent in several bunches.  This may happen even for shorter strings.\n\
Output from processes can arrive in between bunches.")
  (process, string)
     Lisp_Object process, string;
{
  Lisp_Object proc;
  CHECK_STRING (string, 1);
  proc = get_process (process);
  send_process (proc, 
                (char *) XSTRING (string)->data,
                string_length (XSTRING (string)));
  return Qnil;
}

/* send a signal number SIGNO to PROCESS.
   CURRENT_GROUP means send to the process group that currently owns
   the terminal being used to communicate with PROCESS.
   This is used for various commands in shell mode.
   If NOMSG is zero, insert signal-announcements into process's buffers
   right away.

   If we can, we try to signal PROCESS by sending control characters
   down the pipe.  This allows us to signal inferiors who have changed
   their uid, for which killpg would return an EPERM error.  */

static void
process_send_signal (process0, signo, current_group, nomsg)
     Lisp_Object process0;
     int signo;
     Lisp_Object current_group;
     int nomsg;
{
  Lisp_Object proc = get_process (process0);
  register struct Lisp_Process *p = XPROCESS (proc);
  int gid;
  int no_pgrp = 0;

  if (network_connection_p (proc))
    error (GETTEXT ("Network connection %s is not a subprocess"),
	   XSTRING (p->name)->data);
  if (p->infd < 0)
    error (GETTEXT ("Process %s is not active"),
	   XSTRING (p->name)->data);

  if (!p->pty_flag)
    current_group = Qnil;

  /* If we are using pgrps, get a pgrp number and make it negative.  */
  if (!NILP (current_group))
    {
#ifdef SIGNALS_VIA_CHARACTERS
      /* If possible, send signals to the entire pgrp
	 by sending an input character to it.  */

      /* TERMIOS is the latest and bestest, and seems most likely to
         work.  If the system has it, use it.  */
#ifdef HAVE_TERMIOS
      struct termios t;

      switch (signo)
	{
	case SIGINT:
	  tcgetattr (p->infd, &t);
	  send_process (proc, &t.c_cc[VINTR], 1);
	  return;

	case SIGQUIT:
	  tcgetattr (p->infd, &t);
  	  send_process (proc, &t.c_cc[VQUIT], 1);
  	  return;

  	case SIGTSTP:
	  tcgetattr (p->infd, &t);
#if defined (VSWTCH) && !defined (IRIX5)
  	  send_process (proc, &t.c_cc[VSWTCH], 1);
#else
	  send_process (proc, &t.c_cc[VSUSP], 1);
#endif
  	  return;
	}

#else /* ! HAVE_TERMIOS */

      /* On Berkeley descendants, the following IOCTL's retrieve the
	 current control characters.  */
#if defined (TIOCGLTC) && defined (TIOCGETC)
      struct tchars c;
      struct ltchars lc;

      switch (signo)
	{
	case SIGINT:
	  ioctl (p->infd, TIOCGETC, &c);
	  send_process (proc, &c.t_intrc, 1);
	  return;
	case SIGQUIT:
	  ioctl (p->infd, TIOCGETC, &c);
	  send_process (proc, &c.t_quitc, 1);
	  return;
#ifdef SIGTSTP
	case SIGTSTP:
	  ioctl (p->infd, TIOCGLTC, &lc);
	  send_process (proc, &lc.t_suspc, 1);
	  return;
#endif /* ! defined (SIGTSTP) */
	}

#else /* ! defined (TIOCGLTC) && defined (TIOCGETC) */

      /* On SYSV descendants, the TCGETA ioctl retrieves the current control
	 characters.  */
#ifdef TCGETA
      struct termio t;
      switch (signo)
	{
	case SIGINT:
	  ioctl (p->infd, TCGETA, &t);
	  send_process (proc, &t.c_cc[VINTR], 1);
	  return;
	case SIGQUIT:
	  ioctl (p->infd, TCGETA, &t);
	  send_process (proc, &t.c_cc[VQUIT], 1);
	  return;
#ifdef SIGTSTP
	case SIGTSTP:
	  ioctl (p->infd, TCGETA, &t);
	  send_process (proc, &t.c_cc[VSWTCH], 1);
	  return;
#endif /* ! defined (SIGTSTP) */
	}
#else /* ! defined (TCGETA) */
ERROR! Using SIGNALS_VIA_CHARACTERS, but not (TIOCGLTC && TIOCGETC) || TCGETA
      /* If your system configuration files define SIGNALS_VIA_CHARACTERS,
	 you'd better be using one of the alternatives above!  */
#endif /* ! defined (TCGETA) */
#endif /* ! defined (TIOCGLTC) && defined (TIOCGETC) */
#endif /* ! defined HAVE_TERMIOS */
#endif /* ! defined (SIGNALS_VIA_CHARACTERS) */

#ifdef TIOCGPGRP 
      /* Get the pgrp using the tty itself, if we have that.
	 Otherwise, use the pty to get the pgrp.
	 On pfa systems, saka@pfu.fujitsu.co.JP writes:
	 "TIOCGPGRP symbol defined in sys/ioctl.h at E50.
	 But, TIOCGPGRP does not work on E50 ;-P works fine on E60"
	 His patch indicates that if TIOCGPGRP returns an error, then
	 we should just assume that p->pid is also the process group id.  */
      {
	int err;

	if (p->subtty != -1)
	  err = ioctl (p->subtty, TIOCGPGRP, &gid);
	else
	  err = ioctl (p->infd, TIOCGPGRP, &gid);

#ifdef pfa
	if (err == -1)
	  gid = - XINT (p->pid);
#endif /* ! defined (pfa) */
      }
      if (gid == -1)
	no_pgrp = 1;
      else
	gid = - gid;
#else /* ! defined (TIOCGPGRP ) */
      /* Can't select pgrps on this system, so we know that
	 the child itself heads the pgrp.  */
      gid = - XINT (p->pid);
#endif /* ! defined (TIOCGPGRP ) */
    }
  else
    gid = - XINT (p->pid);

  switch (signo)
    {
#ifdef SIGCONT
    case SIGCONT:
      p->status_symbol = Qrun;
      p->exit_code = 0;
      p->tick = ++process_tick;
      if (!nomsg)
	status_notify ();
      break;
#endif /* ! defined (SIGCONT) */
    case SIGINT:
#ifdef VMS
      send_process (proc, "\003", 1);	/* ^C */
      goto whoosh;
#endif
    case SIGQUIT:
#ifdef VMS
      send_process (proc, "\031", 1);	/* ^Y */
      goto whoosh;
#endif
    case SIGKILL:
#ifdef VMS
      sys$forcex (&(XFASTINT (p->pid)), 0, 1);
      whoosh:
#endif
      flush_pending_output (p->infd);
      break;
    }

  /* If we don't have process groups, send the signal to the immediate
     subprocess.  That isn't really right, but it's better than any
     obvious alternative.  */
  if (no_pgrp)
    {
      kill (XINT (p->pid), signo);
      return;
    }

  /* gid may be a pid, or minus a pgrp's number */
#ifdef TIOCSIGSEND
  if (!NILP (current_group))
    ioctl (p->infd, TIOCSIGSEND, signo);
  else
    {
      gid = - XINT (p->pid);
      kill (gid, signo);
    }
#else /* ! defined (TIOCSIGSEND) */
  EMACS_KILLPG (-gid, signo);
#endif /* ! defined (TIOCSIGSEND) */
}

DEFUN ("interrupt-process", Finterrupt_process, Sinterrupt_process, 0, 2, 0,
  "Interrupt process PROCESS.  May be process or name of one.\n\
Nil or no arg means current buffer's process.\n\
Second arg CURRENT-GROUP non-nil means send signal to\n\
the current process-group of the process's controlling terminal\n\
rather than to the process's own process group.\n\
If the process is a shell, this means interrupt current subjob\n\
rather than the shell.")
  (process, current_group)
     Lisp_Object process, current_group;
{
  process_send_signal (process, SIGINT, current_group, 0);
  return process;
}

DEFUN ("kill-process", Fkill_process, Skill_process, 0, 2, 0,
  "Kill process PROCESS.  May be process or name of one.\n\
See function `interrupt-process' for more details on usage.")
  (process, current_group)
     Lisp_Object process, current_group;
{
  process_send_signal (process, SIGKILL, current_group, 0);
  return process;
}

DEFUN ("quit-process", Fquit_process, Squit_process, 0, 2, 0,
  "Send QUIT signal to process PROCESS.  May be process or name of one.\n\
See function `interrupt-process' for more details on usage.")
  (process, current_group)
     Lisp_Object process, current_group;
{
  process_send_signal (process, SIGQUIT, current_group, 0);
  return process;
}

DEFUN ("stop-process", Fstop_process, Sstop_process, 0, 2, 0,
  "Stop process PROCESS.  May be process or name of one.\n\
See function `interrupt-process' for more details on usage.")
  (process, current_group)
     Lisp_Object process, current_group;
{
#ifndef SIGTSTP
  error (GETTEXT ("no SIGTSTP support"));
#else
  process_send_signal (process, SIGTSTP, current_group, 0);
#endif
  return process;
}

DEFUN ("continue-process", Fcontinue_process, Scontinue_process, 0, 2, 0,
  "Continue process PROCESS.  May be process or name of one.\n\
See function `interrupt-process' for more details on usage.")
  (process, current_group)
     Lisp_Object process, current_group;
{
#ifdef SIGCONT
    process_send_signal (process, SIGCONT, current_group, 0);
#else
    error (GETTEXT ("no SIGCONT support"));
#endif
  return process;
}

DEFUN ("signal-process", Fsignal_process, Ssignal_process,
  2, 2, "nProcess number: \nnSignal code: ",
  "Send the process with number PID the signal with code CODE.\n\
Both PID and CODE are integers.")
  (pid, sig)
     Lisp_Object pid, sig;
{
  CHECK_FIXNUM (pid, 0);
  CHECK_FIXNUM (sig, 1);
  return make_number (kill (XINT (pid), XINT (sig)));
}

DEFUN ("process-send-eof", Fprocess_send_eof, Sprocess_send_eof, 0, 1, 0,
  "Make PROCESS see end-of-file in its input.\n\
Eof comes after any text already sent to it.\n\
nil or no arg means current buffer's process.")
  (process)
     Lisp_Object process;
{
  Lisp_Object proc;

  proc = get_process (process);

  /* Make sure the process is really alive.  */
  if (! EQ (XPROCESS (proc)->status_symbol, Qrun))
    error (GETTEXT ("Process %s not running"),
	   XSTRING (XPROCESS (proc)->name)->data);

  /* Sending a zero-length record is supposed to mean eof
     when TIOCREMOTE is turned on.  */
#ifdef DID_REMOTE /* >>> Nothing defines this, nothing else uses this.  What does "DID_REMOTE" mean? */
  {
    char buf[1];
    emacs_write (XPROCESS (proc)->outfd, buf, 0);
  }
#else /* did not do TOICREMOTE */
#ifdef VMS
  send_process (proc, "\032", 1); 	/* ^z */
#else
  if (XPROCESS (proc)->pty_flag)
    send_process (proc, "\004", 1);
  else
    {
      emacs_close (XPROCESS (proc)->outfd);
      XPROCESS (proc)->outfd = emacs_open (NULL_DEVICE, O_WRONLY, 0);
    }
#endif /* !VMS */
#endif /* did not do TOICREMOTE */
  return process;
}

/* Kill all processes associated with `buffer'.
 If `buffer' is nil, kill all processes  */

void
kill_buffer_processes (buffer)
     Lisp_Object buffer;
{
  Lisp_Object tail, proc;

  for (tail = Vprocess_list; XGCTYPE (tail) == Lisp_Cons;
       tail = XCONS (tail)->cdr)
    {
      proc = XCONS (tail)->car;
      if (XGCTYPE (proc) == Lisp_Record
          && gc_record_type_p (proc, lrecord_process)
	  && (NILP (buffer) || EQ (XPROCESS (proc)->buffer, buffer)))
	{
	  if (network_connection_p (proc))
	    deactivate_process (proc);
	  else if (XPROCESS (proc)->infd >= 0)
	    process_send_signal (proc, SIGHUP, Qnil, 1);
	}
    }
}

int
count_active_processes ()
{
  register Lisp_Object tail;
  register int count = 0;

  for (tail = Vprocess_list; CONSP (tail); tail = XCONS (tail)->cdr)
    {
      Lisp_Object status = XPROCESS (XCONS (tail)->car)->status_symbol;
      if ((EQ (status, Qrun) || EQ (status, Qstop)))
	count++;
    }

  return count;
}

/* On receipt of a signal that a child status has changed,
 loop asking about children with changed statuses until
 the system says there are no more.
   All we do is change the status;
 we do not run sentinels or print notifications.
 That is saved for the next time keyboard input is done,
 in order to avoid timing errors.  */

/** WARNING: this can be called during garbage collection.
 Therefore, it must not be fooled by the presence of mark bits in
 Lisp objects.  */

/** USG WARNING:  Although it is not obvious from the documentation
 in signal(2), on a USG system the SIGCLD handler MUST NOT call
 signal() before executing at least one wait(), otherwise the handler
 will be called again, resulting in an infinite loop.  The relevant
 portion of the documentation reads "SIGCLD signals will be queued
 and the signal-catching function will be continually reentered until
 the queue is empty".  Invoking signal() causes the kernel to reexamine
 the SIGCLD queue.   Fred Fish, UniSoft Systems Inc. */

SIGTYPE
sigchld_handler (signo)
     int signo;
{
  int old_errno = errno;

#ifdef BSD4_1
  extern int sigheld;
  sigheld |= sigbit (SIGCHLD);
#endif

  while (1)
    {
      register int pid;
      WAITTYPE w;
      register struct Lisp_Process *p;

#ifdef WNOHANG
#ifndef WUNTRACED
#define WUNTRACED 0
#endif /* no WUNTRACED */
      /* Keep trying to get a status until we get a definitive result.  */
      do 
	{
	  errno = 0;
	  pid = wait3 (&w, WNOHANG | WUNTRACED, 0);
	}
      while (pid <= 0 && errno == EINTR);

      if (pid <= 0)
	{
	  /* A real failure.  We have done all our job, so return.  */

	  /* USG systems forget handlers when they are used;
	     must reestablish each time */
#ifdef USG
	  signal (signo, sigchld_handler);   /* WARNING - must come after wait3() */
#endif
#ifdef  BSD4_1
	  sigheld &= ~sigbit (SIGCHLD);
	  sigrelse (SIGCHLD);
#endif
	  errno = old_errno;
	  SIGRETURN;
	}
#else
      pid = wait (&w);
#endif /* no WNOHANG */

      /* Find the process that signaled us, and record its status.  */

      p = 0;
      {
        Lisp_Object tail;
        for (tail = Vprocess_list; 
             XGCTYPE (tail) == Lisp_Cons;
             tail = XCONS (tail)->cdr)
	  {
	    Lisp_Object proc = XCONS (tail)->car;
	    p = XPROCESS (proc);
	    if (EQ (p->pid, make_number (pid)))
	      break;
	    p = 0;
	  }
      }

      /* Look for an asynchronous process whose pid hasn't been filled
	 in yet.  */
      if (!p)
	{
	  Lisp_Object tail;
	  for (tail = Vprocess_list; 
	       XGCTYPE (tail) == Lisp_Cons;
	       tail = XCONS (tail)->cdr)
	    {
	      Lisp_Object proc = XCONS (tail)->car;
	      p = XPROCESS (proc);
	      if (EQ (p->pid, Qnil))
		break;
	      p = 0;
	    }
	}

      if (p)
	{
	  /* Change the status of the process that was found.  */
	  p->tick = ++process_tick;
	  update_status_from_wait_code (p, &w);
	  
          /* If process has terminated, stop waiting for its output.  */
	  if (WIFSIGNALED (w) || WIFEXITED (w))
	    {
	      if (p->infd >= 0)
		{
		  FD_CLR (p->infd, &input_wait_mask);
		  /* We can't just call event_stream->unselect_process_cb (p)
		     here, because that calls XtRemoveInput, which is not
		     necessarily reentrant, so we can't call this at interrupt
		     level.
		   */
		}
	    }
	}
      else
	{
	  /* There was no asynchronous process found for that id.  Check
	     if we have a synchronous process.  */
	  synch_process_alive = 0;

	  /* Report the status of the synchronous process.  */
	  if (WIFEXITED (w))
	    synch_process_retcode = WRETCODE (w);
	  else if (WIFSIGNALED (w))
	    synch_process_death = sys_siglist[WTERMSIG (w)];
	}

      /* On some systems, we must return right away.
	 If any more processes want to signal us, we will
	 get another signal.
	 Otherwise (on systems that have WNOHANG), loop around
	 to use up all the processes that have something to tell us.  */
#if defined (USG) && ! (defined (HPUX) && defined (WNOHANG))
#ifdef USG
      signal (signo, sigchld_handler);
#endif
      errno = old_errno;
      SIGRETURN;
#endif /* USG, but not HPUX with WNOHANG */
    }
}


static Lisp_Object
exec_sentinel_unwind (Lisp_Object datum)
{
  struct Lisp_Cons *d = XCONS (datum);
  XPROCESS (d->car)->sentinel = d->cdr;
  free_cons (d);
  return Qnil;
}

static void
exec_sentinel (proc, reason)
     Lisp_Object proc, reason;
{
  Lisp_Object sentinel;
  register struct Lisp_Process *p = XPROCESS (proc);
  int speccount = specpdl_depth ();

  sentinel = p->sentinel;
  if (NILP (sentinel))
    return;

  /* Zilch the sentinel while it's running, to avoid recursive invocations;
     assure that it gets restored no matter how the sentinel exits.  */
  p->sentinel = Qnil;
  record_unwind_protect (exec_sentinel_unwind, Fcons (proc, sentinel));
  /* Inhibit quit so that random quits don't screw up a running filter.  */
  specbind (Qinhibit_quit, Qt);
  call2 (sentinel, proc, reason);
  unbind_to (speccount, Qnil);
}

/* Report all recent events of a change in process status
   (either run the sentinel or output a message).
   This is done while Emacs is waiting for keyboard input.  */

void
status_notify ()
{
  Lisp_Object tail = Qnil;
  Lisp_Object symbol = Qnil;
  Lisp_Object msg = Qnil;
  struct gcpro gcpro1, gcpro2, gcpro3;

  if (update_tick == process_tick)
    return;

  /* We need to gcpro tail; if read_process_output calls a filter
     which deletes a process and removes the cons to which tail points
     from Vprocess_alist, and then causes a GC, tail is an unprotected
     reference.  */
  GCPRO3 (tail, symbol, msg);

  for (tail = Vprocess_list; CONSP (tail); tail = XCONS (tail)->cdr)
    {
      Lisp_Object proc = XCONS (tail)->car;
      register struct Lisp_Process *p = XPROCESS (proc);

      if (p->tick != p->update_tick)
	{
	  p->update_tick = p->tick;

	  /* If process is still active, read any output that remains.  */
          while (!EQ (p->filter, Qt)
		 && p->infd >= 0
		 && read_process_output (proc, p->infd) > 0)
            ;

	  /* Get the text to use for the message.  */
	  msg = status_message (p);

	  /* If process is terminated, deactivate it or delete it.  */
	  symbol = p->status_symbol;

	  if (EQ (symbol, Qsignal) 
              || EQ (symbol, Qexit))
	    {
	      if (delete_exited_processes)
		remove_process (proc);
	      else
		deactivate_process (proc);
	    }

	  /* Now output the message suitably.  */
	  if (!NILP (p->sentinel))
	    exec_sentinel (proc, msg);
	  /* Don't bother with a message in the buffer
	     when a process becomes runnable.  */
	  else if (!EQ (symbol, Qrun) && !NILP (p->buffer))
	    {
	      Lisp_Object old_read_only = Qnil;
	      Lisp_Object old = Fcurrent_buffer ();
	      int opoint;
              struct gcpro gcpro1, gcpro2;

	      /* Avoid error if buffer is deleted
		 (probably that's why the process is dead, too) */
	      if (NILP (XBUFFER (p->buffer)->name))
		continue;

              GCPRO2 (old, old_read_only);
	      Fset_buffer (p->buffer);
	      opoint = PT;
	      /* Insert new output into buffer
		 at the current end-of-output marker,
		 thus preserving logical ordering of input and output.  */
	      if (XMARKER (p->mark)->buffer)
		SET_PT (marker_position (p->mark));
	      else
		SET_PT (ZV);
	      if (PT <= opoint)
		opoint += (string_length (XSTRING (msg))
                           + string_length (XSTRING (p->name))
                           + 10);

	      old_read_only = current_buffer->read_only;
	      current_buffer->read_only = Qnil;
	      insert_string (GETTEXT ("\nProcess "));
	      Finsert (1, &p->name);
	      insert_string (" ");
	      Finsert (1, &msg);
	      current_buffer->read_only = old_read_only;
	      Fset_marker (p->mark, make_number (PT), p->buffer);

	      SET_PT (opoint);
	      Fset_buffer (old);
              UNGCPRO;
	    }
	}
    } /* end for */

  redraw_mode_line++;  /* in case buffers use %s in mode-line-format */
  redisplay_preserving_echo_area ();

  update_tick = process_tick;

  UNGCPRO;
}

void
init_process ()
{
  register int i;

#ifdef SIGCHLD
#ifndef CANNOT_DUMP
  if (! noninteractive || initialized)
#endif
    signal (SIGCHLD, sigchld_handler);
#endif

  FD_ZERO (&input_wait_mask);
  FD_SET (0, &input_wait_mask);
  Vprocess_list = Qnil;
  for (i = 0; i < MAXDESC; i++)
    {
      chan_process[i] = Qnil;
      proc_buffered_char[i] = -1;
    }
}
/* 
 * DEFUN ("process-connection",Fprocess_connection,Sprocess_connection,0,1,0,
 *  "Return the connection type of `PROCESS'.  This can be nil (pipe),\n\
 * t or pty (pty) or stream (socket connection).")
 *   (process)
 *      Lisp_Object process;
 * {
 *   return XPROCESS (process)->type;
 * }
 */

void
syms_of_process ()
{
  defsymbol (&Qprocessp, "processp");
  defsymbol (&Qrun, "run");
  defsymbol (&Qstop, "stop");
  defsymbol (&Qsignal, "signal");
  /* Qexit is already defined by syms_of_eval
   * defsymbol (&Qexit, "exit"); 
   */
  defsymbol (&Qopen, "open");
  defsymbol (&Qclosed, "closed");

  staticpro (&Vprocess_list);

  DEFVAR_BOOL ("delete-exited-processes", &delete_exited_processes,
    "*Non-nil means delete processes immediately when they exit.\n\
nil means don't delete them until `list-processes' is run.");

  delete_exited_processes = 1;

  DEFVAR_LISP ("process-connection-type", &Vprocess_connection_type,
    "Control type of device used to communicate with subprocesses.\n\
Values are nil to use a pipe, and t or 'pty for a pty.  Note that if\n\
pty's are not available, this variable will be ignored. The value takes\n\
effect when `start-process' is called.");
  Vprocess_connection_type = Qt;

  defsubr (&Sprocessp);
  defsubr (&Sget_process);
  defsubr (&Sget_buffer_process);
  defsubr (&Sdelete_process);
  defsubr (&Sprocess_status);
  defsubr (&Sprocess_exit_status);
  defsubr (&Sprocess_id);
  defsubr (&Sprocess_name);
  defsubr (&Sprocess_command);
  defsubr (&Sset_process_buffer);
  defsubr (&Sprocess_buffer);
  defsubr (&Sprocess_mark);
  defsubr (&Sset_process_filter);
  defsubr (&Sprocess_filter);
  defsubr (&Sset_process_sentinel);
  defsubr (&Sprocess_sentinel);
  defsubr (&Sprocess_kill_without_query);
  defsubr (&Sprocess_kill_without_query_p);
  defsubr (&Sprocess_list);
  defsubr (&Sstart_process);
#ifdef HAVE_SOCKETS
  defsubr (&Sopen_network_stream);
#endif /* HAVE_SOCKETS */
  defsubr (&Sconnect_to_file_descriptor);
  defsubr (&Sprocess_send_region);
  defsubr (&Sprocess_send_string);
  defsubr (&Sinterrupt_process);
  defsubr (&Skill_process);
  defsubr (&Squit_process);
  defsubr (&Sstop_process);
  defsubr (&Scontinue_process);
  defsubr (&Sprocess_send_eof);
  defsubr (&Ssignal_process);
  defsubr (&Swaiting_for_user_input_p);
/*  defsubr (&Sprocess_connection); */
}

#endif /* subprocesses */
