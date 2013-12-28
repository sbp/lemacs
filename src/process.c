/* Asynchronous subprocess control for GNU Emacs.
   Copyright (C) 1992 Free Software Foundation, Inc.

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

/* #### None of the select() stuff (FD_SET, FD_CLR, etc) in this file is
   actually used any more because of the new input model, so it should be
   expunged.
 */

#include <signal.h>

#include "config.h"

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

#if defined(BSD) || defined(STRIDE)
#include <sys/ioctl.h>
#if !defined (O_NDELAY) && defined (HAVE_PTYS)
#include <fcntl.h>
#endif /* HAVE_PTYS and no O_NDELAY */
#endif /* BSD or STRIDE */
#ifdef USG
#ifndef NO_TERMIO
#include <termio.h>
#endif
#include <fcntl.h>
#endif /* USG */

#ifdef NEED_BSDTTY
#include <sys/bsdtty.h>
#endif

#ifdef NEED_TERMIOS
#include <sys/termios.h>
#endif

#ifdef TRITON88			/* To make emacs send C-c correctly in shell */
#define TIOCGPGRP FIOGETOWN
#endif

#ifdef HPUX
#undef TIOCGPGRP
#endif

/* Include time.h or sys/time.h or both.  */
#include "gettime.h"

#if defined (HPUX) && defined (HAVE_PTYS)
#include <sys/ptyio.h>
#endif
  
#ifdef AIX
#include <sys/pty.h>
#include <unistd.h>
#endif /* AIX */

#ifdef SYSV_PTYS
#include <sys/tty.h>
#ifdef titan
#include <sys/ttyhw.h>
#include <sys/stream.h>
#endif
#include <sys/pty.h>
#endif

#ifdef XENIX
#undef TIOCGETC  /* Avoid confusing some conditionals that test this.  */
#endif

#ifdef BROKEN_TIOCGETC
#undef TIOCGETC
#endif

#ifdef BROKEN_O_NONBLOCK
#undef O_NONBLOCK
#endif

#include "lisp.h"
#include "window.h"
#include "buffer.h"
#include "process.h"
#include "termhooks.h"
#include "termopts.h"
#include "commands.h"

#include "events.h"
extern struct event_stream *event_stream;

extern void child_setup ();

Lisp_Object Qrun, Qstop, Qsignal, Qopen, Qclosed;
extern Lisp_Object Qexit;

/* a process object is a network connection when its childp field is neither
   Qt nor Qnil but is instead a string (name of foreign host we
   are connected to + name of port we are connected to) */

#ifdef HAVE_SOCKETS
static Lisp_Object stream_process;

#define NETCONN_P(p) (XGCTYPE (XPROCESS (p)->childp) == Lisp_String)
#else
#define NETCONN_P(p) 0
#endif /* HAVE_SOCKETS */

/* Define SIGCHLD as an alias for SIGCLD.  There are many conditionals
   testing SIGCHLD.  */

#if !defined (SIGCHLD) && defined (SIGCLD)
#define SIGCHLD SIGCLD
#endif /* SIGCLD */

#include "emacssignal.h"

/* Define the structure that the wait system call stores.
   On many systems, there is a structure defined for this.
   But on vanilla-ish USG systems there is not.  */

#ifndef WAITTYPE
#if !defined (BSD) && !defined (UNIPLUS) && !defined (STRIDE) && !(defined (HPUX) && !defined (NOMULTIPLEJOBS)) && !defined (HAVE_WAIT_HEADER)
#define WAITTYPE int
#define WIFSTOPPED(w) ((w&0377) == 0177)
#define WIFSIGNALED(w) ((w&0377) != 0177 && (w&~0377) == 0)
#define WIFEXITED(w) ((w&0377) == 0)
#define WRETCODE(w) (w >> 8)
#define WSTOPSIG(w) (w >> 8)
#define WCOREDUMP(w) ((w&0200) != 0)
#define WTERMSIG(w) (w & 0377)
#else
#ifdef BSD4_1
#include <wait.h>
#else
#include <sys/wait.h>
#endif /* not BSD 4.1 */

#define WAITTYPE union wait
#define WRETCODE(w) w.w_retcode
#define WCOREDUMP(w) w.w_coredump

#ifdef HPUX
/* HPUX version 7 has broken definitions of these.  */
#undef WTERMSIG
#undef WSTOPSIG
#undef WIFSTOPPED
#undef WIFSIGNALED
#undef WIFEXITED
#endif

#ifndef WTERMSIG
#define WTERMSIG(w) w.w_termsig
#endif
#ifndef WSTOPSIG
#define WSTOPSIG(w) w.w_stopsig
#endif
#ifndef WIFSTOPPED
#define WIFSTOPPED(w) (WTERMSIG (w) == 0177)
#endif
#ifndef WIFSIGNALED
#define WIFSIGNALED(w) (WTERMSIG (w) != 0177 && (WSTOPSIG (w)) == 0)
#endif
#ifndef WIFEXITED
#define WIFEXITED(w) (WTERMSIG (w) == 0)
#endif
#endif /* BSD or UNIPLUS or STRIDE */
#endif /* no WAITTYPE */

extern errno;
extern sys_nerr;
extern char *sys_errlist[];

#ifndef BSD4_1
extern char *sys_siglist[];
#else
char *sys_siglist[] =
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
    "exceeded file size limit"
    };
#endif

#ifdef vipc

#include "vipc.h"
extern int comm_server;
extern int net_listen_address;
#endif /* vipc */

/* Communicate exit status of synch process to callproc.c.  */
extern int synch_process_retcode;
extern char *synch_process_death;

/* t means use pty, nil means use a pipe,
   maybe other values to come.  */
Lisp_Object Vprocess_connection_type;

#ifdef SKTPAIR
#ifndef HAVE_SOCKETS
#include <sys/socket.h>
#endif
#endif /* SKTPAIR */

/* Number of events of change of status of a process.  */
int process_tick;

/* Number of events for which the user or sentinel has been notified.  */
int update_tick;

int delete_exited_processes;

#ifdef FD_SET
/* We could get this from param.h, but better not to depend on finding that.
   And better not to risk that it might define other symbols used in this
   file.  */
#define MAXDESC 64
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

SELECT_TYPE input_wait_mask;

/* Indexed by descriptor, gives the process (if any) for that descriptor */
Lisp_Object chan_process[MAXDESC];

/* Alist of elements (NAME . PROCESS) */
Lisp_Object Vprocess_alist;

Lisp_Object Qprocessp;

Lisp_Object get_process ();

/* Buffered-ahead input char from process, indexed by channel.
   -1 means empty (no char is buffered).
   Used on sys V where the only way to tell if there is any
   output from the process is to read at least one char.
   Always -1 on systems that support FIONREAD.  */

int proc_buffered_char[MAXDESC];

/* These variables hold the filter about to be run, and its args,
   between read_process_output and run_filter.
   Also used in exec_sentinel for sentinels.  */
Lisp_Object this_filter;
Lisp_Object filter_process, filter_string;

/* Compute the Lisp form of the process status, p->status, from
   the numeric status that was returned by `wait'.  */

update_status (p)
     struct Lisp_Process *p;
{
  union { int i; WAITTYPE wt; } u;
  u.i = XFASTINT (p->raw_status_low) + (XFASTINT (p->raw_status_high) << 16);
  p->status = status_convert (u.wt);
  p->raw_status_low = Qnil;
  p->raw_status_high = Qnil;
}

/*  Convert a process status word in Unix format to 
    the list that we use internally.  */

Lisp_Object
status_convert (w)
     WAITTYPE w;
{
  if (WIFSTOPPED (w))
    return Fcons (Qstop, Fcons (make_number (WSTOPSIG (w)), Qnil));
  else if (WIFEXITED (w))
    return Fcons (Qexit, Fcons (make_number (WRETCODE (w)),
				WCOREDUMP (w) ? Qt : Qnil));
  else if (WIFSIGNALED (w))
    return Fcons (Qsignal, Fcons (make_number ((int)WTERMSIG (w)),
				  WCOREDUMP (w) ? Qt : Qnil));
  else
    return Qrun;
}

/* Given a status-list, extract the three pieces of information
   and store them individually through the three pointers.  */

void
decode_status (l, symbol, code, coredump)
     Lisp_Object l;
     Lisp_Object *symbol;
     int *code;
     int *coredump;
{
  Lisp_Object tem;

  if (SYMBOLP (l))
    {
      *symbol = l;
      *code = 0;
      *coredump = 0;
    }
  else
    {
      *symbol = XCONS (l)->car;
      tem = XCONS (l)->cdr;
      *code = XFASTINT (XCONS (tem)->car);
      tem = XFASTINT (XCONS (tem)->cdr);
      *coredump = !NILP (tem);
    }
}

/* Return a string describing a process status list.  */

Lisp_Object 
status_message (status)
     Lisp_Object status;
{
  Lisp_Object symbol;
  int code, coredump;
  Lisp_Object string, string2;

  decode_status (status, &symbol, &code, &coredump);

  if (EQ (symbol, Qsignal) || EQ (symbol, Qstop))
    {
      string = build_string (code < NSIG ? sys_siglist[code] : "unknown");
      string2 = build_string (coredump ? " (core dumped)\n" : "\n");
      XSTRING (string)->data[0] = DOWNCASE (XSTRING (string)->data[0]);
      return concat2 (string, string2);
    }
  else if (EQ (symbol, Qexit))
    {
      if (code == 0)
	return build_string ("finished\n");
      string = Fint_to_string (make_number (code));
      string2 = build_string (coredump ? " (core dumped)\n" : "\n");
      return concat2 (build_string ("exited abnormally with code "),
		      concat2 (string, string2));
    }
  else
    return Fcopy_sequence (Fsymbol_name (symbol));
}

#ifdef HAVE_PTYS
static pty_process;

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
#ifdef HPUX
	sprintf (pty_name, "/dev/ptym/pty%c%x", c, i);
#else
#ifdef RTU
	sprintf (pty_name, "/dev/pty%x", i);
#else
	sprintf (pty_name, "/dev/pty%c%x", c, i);
#endif /* not RTU */
#endif /* not HPUX */
#endif /* no PTY_NAME_SPRINTF */

#ifdef PTY_OPEN
	PTY_OPEN;
#else /* no PTY_OPEN */
#ifndef IRIS
	if (stat (pty_name, &stb) < 0)
	  return -1;
#ifdef O_NONBLOCK
	fd = open (pty_name, O_RDWR | O_NONBLOCK, 0);
#else
	fd = open (pty_name, O_RDWR | O_NDELAY, 0);
#endif
#else /* Unusual IRIS code */
 	fd = open ("/dev/ptc", O_RDWR | O_NDELAY, 0);
 	if (fd < 0)
 	  return -1;
	if (fstat (fd, &stb) < 0)
	  return -1;
#endif /* IRIS */
#endif /* no PTY_OPEN */

	if (fd >= 0)
	  {
	    /* check to make certain that both sides are available
	       this avoids a nasty yet stupid bug in rlogins */
#ifdef PTY_TTY_NAME_SPRINTF
	    PTY_TTY_NAME_SPRINTF
#else
	    /* TODO: In version 19, make these special cases use the macro above.  */
#ifdef HPUX
            sprintf (pty_name, "/dev/pty/tty%c%x", c, i);
#else
#ifdef RTU
            sprintf (pty_name, "/dev/ttyp%x", i);
#else
#ifdef IRIS
 	    sprintf (pty_name, "/dev/ttyq%d", minor (stb.st_rdev));
#else
            sprintf (pty_name, "/dev/tty%c%x", c, i);
#endif /* not IRIS */
#endif /* not RTU */
#endif /* not HPUX */
#endif /* no PTY_TTY_NAME_SPRINTF */
#ifndef UNIPLUS
	    if (access (pty_name, 6) != 0)
	      {
		close (fd);
#ifndef IRIS
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

Lisp_Object
make_process (name)
     Lisp_Object name;
{
  register Lisp_Object val, tem, name1;
  register struct Lisp_Process *p;
  char suffix[10];
  register int i;

  /* size of process structure includes the vector header,
     so deduct for that.  But struct Lisp_Vector includes the first
     element, thus deducts too much, so add it back.  */
  val = Fmake_vector (make_number ((sizeof (struct Lisp_Process)
				    - sizeof (struct Lisp_Vector)
				    + sizeof (Lisp_Object))
				   / sizeof (Lisp_Object)),
		      Qnil);
  XSETTYPE (val, Lisp_Process);

  p = XPROCESS (val);
  XFASTINT (p->infd) = 0;
  XFASTINT (p->outfd) = 0;
  XFASTINT (p->pid) = 0;
  XFASTINT (p->tick) = 0;
  XFASTINT (p->update_tick) = 0;
  p->raw_status_low = Qnil;
  p->raw_status_high = Qnil;
  p->status = Qrun;
  p->mark = Fmake_marker ();

  /* If name is already in use, modify it until it is unused.  */

  name1 = name;
  for (i = 1; ; i++)
    {
      tem = Fget_process (name1);
      if (NILP (tem)) break;
      sprintf (suffix, "<%d>", i);
      name1 = concat2 (name, build_string (suffix));
    }
  name = name1;
  p->name = name;
  Vprocess_alist = Fcons (Fcons (name, val), Vprocess_alist);
  return val;
}

remove_process (proc)
     register Lisp_Object proc;
{
  register Lisp_Object pair;

  pair = Frassq (proc, Vprocess_alist);
  Vprocess_alist = Fdelq (pair, Vprocess_alist);
  Fset_marker (XPROCESS (proc)->mark, Qnil, Qnil);

  deactivate_process (proc);
}

DEFUN ("processp", Fprocessp, Sprocessp, 1, 1, 0,
  "Return t if OBJECT is a process.")
  (obj)
     Lisp_Object obj;
{
  return PROCESSP (obj) ? Qt : Qnil;
}

DEFUN ("get-process", Fget_process, Sget_process, 1, 1, 0,
  "Return the process named NAME, or nil if there is none.")
  (name)
     register Lisp_Object name;
{
  if (PROCESSP (name))
    return name;
  CHECK_STRING (name, 0);
  return Fcdr (Fassoc (name, Vprocess_alist));
}

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

  for (tail = Vprocess_alist; !NILP (tail); tail = Fcdr (tail))
    {
      proc = Fcdr (Fcar (tail));
      if (PROCESSP (proc) && EQ (XPROCESS (proc)->buffer, buf))
	return proc;
    }
  return Qnil;
}

/* This is how commands for the user decode process arguments */

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
    error ("Current buffer has no process");
  else
    error ("Process %s does not exist", XSTRING (name)->data);
  /* NOTREACHED */
}

DEFUN ("delete-process", Fdelete_process, Sdelete_process, 1, 1, 0,
  "Delete PROCESS: kill it and forget about it immediately.\n\
PROCESS may be a process or the name of one, or a buffer name.")
  (proc)
     register Lisp_Object proc;
{
  proc = get_process (proc);
  XPROCESS (proc)->raw_status_low = Qnil;
  XPROCESS (proc)->raw_status_high = Qnil;
  if (NETCONN_P (proc))
    {
      XPROCESS (proc)->status = Fcons (Qexit, Fcons (make_number (0), Qnil));
      XSETINT (XPROCESS (proc)->tick, ++process_tick);
    }
  else if (XFASTINT (XPROCESS (proc)->infd))
    {
      Fkill_process (proc, Qnil);
      /* Do this now, since remove_process will make sigchld_handler do nothing.  */
      XPROCESS (proc)->status
	= Fcons (Qsignal, Fcons (make_number (SIGKILL), Qnil));
      XSETINT (XPROCESS (proc)->tick, ++process_tick);
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
/* command -- for a command channel opened to Emacs by another process.\n\
   external -- for an i/o channel opened to Emacs by another process.\n\  */
  (proc)
     register Lisp_Object proc;
{
  register struct Lisp_Process *p;
  register Lisp_Object status;
  proc = Fget_process (proc);
  if (NILP (proc))
    return proc;
  p = XPROCESS (proc);
  if (!NILP (p->raw_status_low))
    update_status (p);
  status = p->status;
  if (CONSP (status))
    status = XCONS (status)->car;
  if (NETCONN_P (proc))
    {
      if (EQ (status, Qrun))
	status = Qopen;
      else if (EQ (status, Qexit))
	status = Qclosed;
    }
  return status;
}

DEFUN ("process-exit-status", Fprocess_exit_status, Sprocess_exit_status,
       1, 1, 0,
  "Return the exit status of PROCESS or the signal number that killed it.\n\
If PROCESS has not yet exited or died, return 0.\n\
If PROCESS is a net connection that was closed remotely, return 256.")
  (proc)
     register Lisp_Object proc;
{
  CHECK_PROCESS (proc, 0);
  if (!NILP (XPROCESS (proc)->raw_status_low))
    update_status (XPROCESS (proc));
  if (CONSP (XPROCESS (proc)->status))
    return XCONS (XCONS (XPROCESS (proc)->status)->cdr)->car;
  return make_number (0);
}

DEFUN ("process-id", Fprocess_id, Sprocess_id, 1, 1, 0,
  "Return the process id of PROCESS.\n\
This is the pid of the Unix process which PROCESS uses or talks to.\n\
For a network connection, this value is nil.")
  (proc)
     register Lisp_Object proc;
{
  CHECK_PROCESS (proc, 0);
  return XPROCESS (proc)->pid;
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
and the rest of the strings being the arguments given to it.\n\
For a non-child channel, this is nil.")
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
  return XPROCESS (proc)->mark;
}

DEFUN ("set-process-filter", Fset_process_filter, Sset_process_filter,
  2, 2, 0,
  "Give PROCESS the filter function FILTER; nil means no filter.\n\
When a process has a filter, each time it does output\n\
the entire string of output is passed to the filter.\n\
The filter gets two arguments: the process and the string of output.\n\
If the process has a filter, its buffer is not used for output.")
  (proc, filter)
     register Lisp_Object proc, filter;
{
  CHECK_PROCESS (proc, 0);
  XPROCESS (proc)->filter = filter;
#ifdef ENERGIZE
  XPROCESS (proc)->filter_does_read = Qnil;
#endif
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
  (proc, value)
     register Lisp_Object proc, value;
{
  Lisp_Object tem;

  CHECK_PROCESS (proc, 0);
  tem = XPROCESS (proc)->kill_without_query;
  XPROCESS (proc)->kill_without_query = Fnull (value);

  return Fnull (tem);
}

DEFUN ("process-kill-without-query-p", Fprocess_kill_without_query_p,
  Sprocess_kill_without_query_p, 1, 1, 0,
  "Return t or nil, depending on whether or not PROCESS will be killed\n\
without query.")
  (proc)
     register Lisp_Object proc;
{
  CHECK_PROCESS (proc, 0);
  return XPROCESS (proc)->kill_without_query;
}

Lisp_Object
list_processes_1 ()
{
  register Lisp_Object tail, tem;
  Lisp_Object proc, minspace, tem1;
  register struct buffer *old = current_buffer;
  register struct Lisp_Process *p;
  register int state;
  char tembuf[80];

  XFASTINT (minspace) = 1;

  internal_set_buffer (XBUFFER (Vstandard_output));
  Fbuffer_disable_undo (Vstandard_output);

  current_buffer->truncate_lines = Qt;

  write_string ("\
Proc         Status   Buffer         Command\n\
----         ------   ------         -------\n", -1);

  for (tail = Vprocess_alist; !NILP (tail); tail = Fcdr (tail))
    {
      Lisp_Object symbol;

      proc = Fcdr (Fcar (tail));
      p = XPROCESS (proc);
      if (NILP (p->childp))
	continue;

      Finsert (1, &p->name);
      Findent_to (make_number (13), minspace);

      if (!NILP (p->raw_status_low))
	update_status (p);
      symbol = p->status;
      if (CONSP (p->status))
	symbol = XCONS (p->status)->car;

      
      if (EQ (symbol, Qsignal))
	{
	  Lisp_Object tem;
	  tem = Fcar (Fcdr (p->status));
	  if (XINT (tem) < NSIG)
	    write_string (sys_siglist [XINT (tem)], -1);
	  else
	    Fprinc (symbol, Qnil);
	}
      else if (NETCONN_P (proc))
	{
	  if (EQ (symbol, Qrun))
	    write_string ("open", -1);
	  else if (EQ (symbol, Qexit))
	    write_string ("closed", -1);
	  else
	    Fprinc (symbol, Qnil);
	}
      else
	Fprinc (symbol, Qnil);

      if (EQ (symbol, Qexit))
	{
	  Lisp_Object tem;
	  tem = Fcar (Fcdr (p->status));
	  if (XFASTINT (tem))
	    {
	      sprintf (tembuf, " %d", XFASTINT (tem));
	      write_string (tembuf, -1);
	    }
	}

      if (EQ (symbol, Qsignal) || EQ (symbol, Qexit))
	remove_process (proc);

      Findent_to (make_number (22), minspace);
      if (NILP (p->buffer))
	insert_string ("(none)");
      else if (NILP (XBUFFER (p->buffer)->name))
	insert_string ("(Killed)");
      else
	Finsert (1, &XBUFFER (p->buffer)->name);

      Findent_to (make_number (37), minspace);

      if (NETCONN_P (proc))
        {
	  sprintf (tembuf, "(network stream connection to %s)\n",
		   XSTRING (p->childp)->data);
	  insert_string (tembuf);
        }
      else 
	{
	  tem = p->command;
	  while (1)
	    {
	      tem1 = Fcar (tem);
	      Finsert (1, &tem1);
	      tem = Fcdr (tem);
	      if (NILP (tem))
		break;
	      insert_string (" ");
	    }
	  insert_string ("\n");
       }
    }
  return Qnil;
}

DEFUN ("list-processes", Flist_processes, Slist_processes, 0, 0, "",
  "Display a list of all processes.\n\
\(Any processes listed as Exited or Signaled are actually eliminated\n\
after the listing is made.)")
  ()
{
  internal_with_output_to_temp_buffer ("*Process List*",
				       list_processes_1, Qnil, Qt);
  return Qnil;
}

DEFUN ("process-list", Fprocess_list, Sprocess_list, 0, 0, 0,
  "Return a list of all processes.")
  ()
{
  return Fmapcar (Qcdr, Vprocess_alist);
}

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
  Lisp_Object buffer, name, program, proc, tem;
  register unsigned char **new_argv;
  register int i;

  buffer = args[1];
  if (!NILP (buffer))
    buffer = Fget_buffer_create (buffer);

  name = args[0];
  CHECK_STRING (name, 0);

  program = args[2];

  CHECK_STRING (program, 2);

  new_argv = (unsigned char **) alloca ((nargs - 1) * sizeof (char *));

  for (i = 3; i < nargs; i++)
    {
      tem = args[i];
      CHECK_STRING (tem, i);
      new_argv[i - 2] = XSTRING (tem)->data;
    }
  new_argv[i - 2] = 0;
  new_argv[0] = XSTRING (program)->data;

  /* If program file name is not absolute, search our path for it */
  if (new_argv[0][0] != '/')
    {
      tem = Qnil;
      openp (Vexec_path, program, "", &tem, 1);
      if (NILP (tem))
	report_file_error ("Searching for program", Fcons (program, Qnil));
      new_argv[0] = XSTRING (tem)->data;
    }

  proc = make_process (name);

  XPROCESS (proc)->childp = Qt;
  XPROCESS (proc)->command_channel_p = Qnil;
  XPROCESS (proc)->buffer = buffer;
  XPROCESS (proc)->sentinel = Qnil;
  XPROCESS (proc)->filter = Qnil;
#ifdef ENERGIZE
  XPROCESS (proc)->filter_does_read = Qnil;
#endif
  XPROCESS (proc)->command = Flist (nargs - 2, args + 2);

  create_process (proc, new_argv);

  return proc;
}

void
create_process_1 (signo)
     int signo;
{
#ifdef USG
  /* USG systems forget handlers when they are used;
     must reestablish each time */
  signal (signo, create_process_1);
#endif /* USG */
}

#if 0  /* This doesn't work; see the note before sigchld_handler.  */
#ifdef USG
#ifdef SIGCHLD
/* Mimic blocking of signals on system V, which doesn't really have it.  */

/* Nonzero means we got a SIGCHLD when it was supposed to be blocked.  */
int sigchld_deferred;

create_process_sigchld ()
{
  signal (SIGCHLD, create_process_sigchld);

  sigchld_deferred = 1;
}
#endif
#endif
#endif

create_process (process, new_argv)
     Lisp_Object process;
     char **new_argv;
{
  int pid, inchannel, outchannel, forkin, forkout;
  int sv[2];
#ifdef SIGCHLD
  int (*sigchld)();
#endif
  int pty_flag = 0;
  char **env;
  extern char **environ;

#ifdef MAINTAIN_ENVIRONMENT
  env = (char **) alloca (size_of_current_environ ());
  get_current_environ (env);
#else
  env = environ;
#endif /* MAINTAIN_ENVIRONMENT */

  inchannel = outchannel = -1;

#ifdef HAVE_PTYS
  if (EQ (Vprocess_connection_type, Qt))
    outchannel = inchannel = allocate_pty ();

  if (inchannel >= 0)
    {
#ifndef USG
      /* On USG systems it does not work to open the pty's tty here
	       and then close and reopen it in the child.  */
#ifdef O_NOCTTY
      /* Don't let this terminal become our controlling terminal
	 (in case we don't have one).  */
      forkout = forkin = open (pty_name, O_RDWR | O_NOCTTY, 0);
#else
      forkout = forkin = open (pty_name, O_RDWR, 0);
#endif
      if (forkin < 0)
	report_file_error ("Opening pty", Qnil);
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
	report_file_error ("Opening socketpair", Qnil);
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
#ifdef STRIDE
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

  XFASTINT (XPROCESS (process)->infd) = inchannel;
  XFASTINT (XPROCESS (process)->outfd) = outchannel;
  /* Record the tty descriptor used in the subprocess.  */
#ifdef SYSV4_PTYS
  /* On system V.4, if using a pty, we need to keep a descriptor
     for the tty that the inferior uses, in order to get the pgrp.
     If this uses too many descriptors, we could instead save the tty name
     and reopen it to send signals.  */
  if (pty_flag)
    {
      int temp = dup (forkin);
      if (temp < 0) goto io_failure;
      XFASTINT (XPROCESS (process)->subtty) = temp;
    }
  else
#endif
    XPROCESS (process)->subtty = Qnil;
  XPROCESS (process)->pty_flag = (pty_flag ? Qt : Qnil);
  XPROCESS (process)->status = Qrun;
  /* Record this as an active process, with its channels.
     As a result, child_setup will close Emacs's side of the pipes.  */
  chan_process[inchannel] = process;

  /* Delay interrupts until we have a chance to store
     the new fork's pid in its process structure */
#ifdef SIGCHLD
#ifdef BSD4_1
  sighold (SIGCHLD);
#else /* not BSD4_1 */
#ifdef HPUX
  sigsetmask (sigmask (SIGCHLD));
#else /* not HPUX */
#if defined (BSD) || defined (UNIPLUS)
  sigsetmask (sigmask (SIGCHLD));
#else /* ordinary USG */
#if 0
  sigchld_deferred = 0;
  sigchld = (int (*)()) signal (SIGCHLD, create_process_sigchld);
#endif
#endif /* ordinary USG */
#endif /* not HPUX */
#endif /* not BSD4_1 */
#endif /* SIGCHLD */

  /* Until we store the proper pid, enable sigchld_handler
     to recognize an unknown pid as standing for this process.
     It is very important not to let this marker value persist
     in the table outside of this function; if it does it might
     cause call-process to hang and subsequent asynchronous
     processes to get their return values scrambled.  */
  XSETINT (XPROCESS (process)->pid, -1);
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
#else /* not HAVE_SETSID */
#ifdef USG
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
	    int j = open ("/dev/tty", O_RDWR, 0);
	    ioctl (j, TIOCNOTTY, 0);
	    close (j);
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
	      close (xforkin);
	    xforkout = xforkin = open (pty_name, O_RDWR, 0);

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
	child_setup (xforkin, xforkout, xforkout, new_argv, env);
      }
#ifdef EMACS_BTL
    else if (logging_on)
      cadillac_start_logging ();	/* #### rename me */
#endif

    environ = save_environ;
  }

  if (pid < 0)
    {
      remove_process (process);
      report_file_error ("Doing vfork", Qnil);
    }

  XFASTINT (XPROCESS (process)->pid) = pid;

  /* If the subfork execv fails, and it exits,
     this close hangs.  I don't know why.
     So have an interrupt jar it loose.  */
  stop_polling ();
  signal (SIGALRM, create_process_1);
  alarm (1);
  if (forkin >= 0)
    close (forkin);
  alarm (0);
  start_polling ();
  if (forkin != forkout && forkout >= 0)
    close (forkout);

#ifdef SIGCHLD
#ifdef BSD4_1
  sigrelse (SIGCHLD);
#else /* not BSD4_1 */
#if defined (BSD) || defined (UNIPLUS) || defined (HPUX)
  sigsetmask (SIGEMPTYMASK);
#else /* ordinary USG */
#if 0
  signal (SIGCHLD, sigchld);
  /* Now really handle any of these signals
     that came in during this function.  */
  if (sigchld_deferred)
    kill (getpid (), SIGCHLD);
#endif
#endif /* ordinary USG */
#endif /* not BSD4_1 */
#endif /* SIGCHLD */

  event_stream->select_process_cb (XPROCESS (process));
  return;

io_failure:
  {
    int temp = errno;
    close (forkin);
    close (forkout);
    close (inchannel);
    close (outchannel);
    errno = temp;
    report_file_error ("Opening pty or pipe", Qnil);
  }
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
Third arg is name of the host to connect to.\n\
Fourth arg SERVICE is name of the service desired, or an integer\n\
 specifying a port number to connect to.")
   (name, buffer, host, service)
      Lisp_Object name, buffer, host, service;
{
  Lisp_Object proc;
  register int i;
  struct sockaddr_in address;
  struct servent *svc_info;
  struct hostent *host_info_ptr, host_info;
  char *(addr_list[2]);
  unsigned long numeric_addr;
  int s, outch, inch;
  char errstring[80];
  int port;
  struct hostent host_info_fixed;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

  GCPRO4 (name, buffer, host, service);
  CHECK_STRING (name, 0);
  CHECK_STRING (host, 0);
  if (FIXNUMP (service))
    port = htons ((unsigned short) XINT (service));
  else
    {
      CHECK_STRING (service, 0);
      svc_info = getservbyname ((char *)XSTRING (service)->data, "tcp");
      if (svc_info == 0)
	error ("Unknown service \"%s\"", XSTRING (service)->data);
      port = svc_info->s_port;
    }

  host_info_ptr = gethostbyname ((char *)XSTRING (host)->data);
  if (host_info_ptr == 0)
    /* Attempt to interpret host as numeric inet address */
    {
      numeric_addr = inet_addr ((char *)XSTRING (host)->data);
      if (numeric_addr == -1)
	error ("Unknown host \"%s\"", XSTRING (host)->data);

      host_info_ptr = &host_info;
      host_info.h_name = 0;
      host_info.h_aliases = 0;
      host_info.h_addrtype = AF_INET;
      host_info.h_addr_list  =  &(addr_list[0]);
      addr_list[0] = (char*)(&numeric_addr);
      addr_list[1] = 0;
      host_info.h_length = strlen (addr_list[0]);
    }

  bzero (&address, sizeof address);
  bcopy (host_info_ptr->h_addr, (char *) &address.sin_addr,
	 host_info_ptr->h_length);
  address.sin_family = host_info_ptr->h_addrtype;
  address.sin_port = port;

  s = socket (host_info_ptr->h_addrtype, SOCK_STREAM, 0);
  if (s < 0) 
    report_file_error ("error creating socket", Fcons (name, Qnil));

  /* Kernel bugs (on Ultrix at least) cause lossage (not just EINTR)
     when connect is interrupted.  So let's not let it get interrupted.  */
  if (interrupt_input)
    unrequest_sigio ();
  stop_polling ();

 loop:
  if (connect (s, (struct sockaddr *)&address, sizeof address) == -1)
    {
      int xerrno = errno;
      if (errno == EINTR)
	goto loop;
      close (s);
      errno = xerrno;
      report_file_error ("connection failed",
			 Fcons (host, Fcons (name, Qnil)));
    }

  if (interrupt_input)
    request_sigio ();
  start_polling ();

  inch = s;
  outch = dup (s);
  if (outch < 0) 
    report_file_error ("error duplicating socket", Fcons (name, Qnil));

  if (!NILP (buffer))
    buffer = Fget_buffer_create (buffer);
  proc = make_process (name);

  chan_process[inch] = proc;

#ifdef O_NONBLOCK
  fcntl (inch, F_SETFL, O_NONBLOCK);
#else
#ifdef O_NDELAY
  fcntl (inch, F_SETFL, O_NDELAY);
#endif
#endif

  XPROCESS (proc)->childp = host;
  XPROCESS (proc)->command_channel_p = Qnil;
  XPROCESS (proc)->buffer = buffer;
  XPROCESS (proc)->sentinel = Qnil;
  XPROCESS (proc)->filter = Qnil;
#ifdef ENERGIZE
  XPROCESS (proc)->filter_does_read = Qnil;
#endif
  XPROCESS (proc)->command = Qnil;
  XPROCESS (proc)->pid = Qnil;
  XPROCESS (proc)->kill_without_query = Qt;
  XFASTINT (XPROCESS (proc)->infd) = s;
  XFASTINT (XPROCESS (proc)->outfd) = outch;
  XPROCESS (proc)->status = Qrun;
  FD_SET (inch, &input_wait_mask);

  event_stream->select_process_cb (XPROCESS (proc));

  UNGCPRO;
  return proc;
}
#endif	/* HAVE_SOCKETS */

deactivate_process (proc)
     Lisp_Object proc;
{
  register int inchannel, outchannel;
  register struct Lisp_Process *p = XPROCESS (proc);

  inchannel = XFASTINT (p->infd);
  outchannel = XFASTINT (p->outfd);

  if (inchannel)
    {
      /* Beware SIGCHLD hereabouts. */
      flush_pending_output (inchannel);
      close (inchannel);
      if (outchannel && outchannel != inchannel)
 	close (outchannel);

      /* Must call this before setting the file descriptors to 0 */
      event_stream->unselect_process_cb (p);

      XFASTINT (p->infd) = 0;
      XFASTINT (p->outfd) = 0;
      chan_process[inchannel] = Qnil;
      FD_CLR (inchannel, &input_wait_mask);
    }
}

/* Close all descriptors currently in use for communication
   with subprocess.  This is used in a newly-forked subprocess
   to get rid of irrelevant descriptors.  */

close_process_descs ()
{
  int i;
  for (i = 0; i < MAXDESC; i++)
    {
      Lisp_Object process;
      process = chan_process[i];
      if (!NILP (process))
	{
	  int in = XFASTINT (XPROCESS (process)->infd);
	  int out = XFASTINT (XPROCESS (process)->outfd);

	  if (in != 0)
	    close (in);
	  if (out != 0 && out != in)
	    close (out);
	  if (!NILP (XPROCESS (process)->subtty))
	    close (XFASTINT (XPROCESS (process)->subtty));
	}
    }
}


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

read_process_output (proc, channel)
     Lisp_Object proc;
     register int channel;
{
  register int nchars;
  char chars[1024];
  register Lisp_Object outstream;
  register struct buffer *old = current_buffer;
  register struct Lisp_Process *p = XPROCESS (proc);
  register int opoint;

#ifdef ENERGIZE
  if (!NILP (p->filter) && !NILP (p->filter_does_read))
    {
      int filter_result = call2 (p->filter, proc, Qnil);
      CHECK_FIXNUM (filter_result, 0);
      return XINT (filter_result);
    }
#endif
  if (proc_buffered_char[channel] < 0)
    nchars = read (channel, chars, sizeof chars);
  else
    {
      chars[0] = proc_buffered_char[channel];
      proc_buffered_char[channel] = -1;
      nchars = read (channel, chars + 1, sizeof chars - 1);
      if (nchars < 0)
	nchars = 1;
      else
	nchars = nchars + 1;
    }

  if (nchars <= 0) return nchars;

  outstream = p->filter;
  if (!NILP (outstream))
    {
      int count = specpdl_ptr - specpdl;
      specbind (Qinhibit_quit, Qt);
      this_filter = outstream;
      filter_process = proc;
      filter_string = make_string (chars, nchars);
      call2 (this_filter, filter_process, filter_string);
      /*   internal_condition_case (run_filter, Qerror, Fidentity);  */
      unbind_to (count);
      return nchars;
    }

  /* If no filter, write into buffer if it isn't dead.  */
  if (!NILP (p->buffer) && !NILP (XBUFFER (p->buffer)->name))
    {
      Lisp_Object tem;

      Fset_buffer (p->buffer);
      opoint = point;

      /* Insert new output into buffer
	 at the current end-of-output marker,
	 thus preserving logical ordering of input and output.  */
      if (XMARKER (p->mark)->buffer)
	SET_PT (marker_position (p->mark));
      else
	SET_PT (ZV);
      if (point <= opoint)
	opoint += nchars;

      tem = current_buffer->read_only;
      current_buffer->read_only = Qnil;
#if 0
      /* This screws up intial display of the window.  jla */

      /* Insert before markers in case we are inserting where
	 the buffer's mark is, and the user's next command is Meta-y.  */
      insert_before_markers (chars, nchars);
#endif
      insert_raw_string (chars, nchars);
      current_buffer->read_only = tem;
      Fset_marker (p->mark, make_number (point), p->buffer);
      redraw_mode_line++;

      SET_PT (opoint);
      internal_set_buffer (old);
    }
  return nchars;
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

jmp_buf send_process_frame;

void
send_process_trap (sig)
     int sig;
{
#ifdef BSD4_1
  sigrelse (SIGPIPE);
  sigrelse (SIGALRM);
#endif /* BSD4_1 */
  longjmp (send_process_frame, 1);
}

send_process (proc, buf, len)
     Lisp_Object proc;
     char *buf;
     int len;
{
  /* Don't use register vars; longjmp can lose them.  */
  int rv;
  unsigned char *procname = XSTRING (XPROCESS (proc)->name)->data;

  if (! NILP (XPROCESS (proc)->raw_status_low))
    update_status (XPROCESS (proc));
  if (! EQ (XPROCESS (proc)->status, Qrun))
    error ("Process %s not running", procname);

  if (!setjmp (send_process_frame))
    while (len > 0)
      {
	signal (SIGPIPE, send_process_trap);
	rv = write (XFASTINT (XPROCESS (proc)->outfd), buf, len);
	signal (SIGPIPE, SIG_DFL);
	if (rv < 0)
	  {
#if defined(O_NDELAY) || defined(O_NONBLOCK)
	    if (errno == EAGAIN)
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
#endif
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
	    report_file_error ("writing to process", Fcons (proc, Qnil));
	  }
	buf += rv;
	len -= rv;
      }
  else
    {
      XPROCESS (proc)->raw_status_low = Qnil;
      XPROCESS (proc)->raw_status_high = Qnil;
      XPROCESS (proc)->status = Fcons (Qexit, Fcons (make_number (256), Qnil));
      XSETINT (XPROCESS (proc)->tick, ++process_tick);
      deactivate_process (proc);
      error ("SIGPIPE raised on process %s; closed it", procname);
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
    move_gap (start);

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
  send_process (proc, (char *) XSTRING (string)->data, XSTRING (string)->size);
  return Qnil;
}

/* send a signal number SIGNO to PROCESS.
   CURRENT_GROUP means send to the process group that currently owns
   the terminal being used to communicate with PROCESS.
   This is used for various commands in shell mode.
   If NOMSG is zero, insert signal-announcements into process's buffers
   right away.  */

process_send_signal (process, signo, current_group, nomsg)
     Lisp_Object process;
     int signo;
     Lisp_Object current_group;
     int nomsg;
{
  Lisp_Object proc;
  register struct Lisp_Process *p;
  int gid;
  int no_pgrp = 0;

  proc = get_process (process);
  p = XPROCESS (proc);

  if (!EQ (p->childp, Qt))
    error ("Process %s is not a subprocess",
	   XSTRING (p->name)->data);
  if (!XFASTINT (p->infd))
    error ("Process %s is not active",
	   XSTRING (p->name)->data);

  if (NILP (p->pty_flag))
    current_group = Qnil;

#ifdef TIOCGPGRP		/* Not sure about this! (fnf) */
  /* If we are using pgrps, get a pgrp number and make it negative.  */
  if (!NILP (current_group))
    {
      /* If possible, send signals to the entire pgrp
	 by sending an input character to it.  */
#if defined (TIOCGLTC) && defined (TIOCGETC)
      struct tchars c;
      struct ltchars lc;

      switch (signo)
	{
	case SIGINT:
	  ioctl (XFASTINT (p->infd), TIOCGETC, &c);
	  send_process (proc, &c.t_intrc, 1);
	  return Qnil;
	case SIGQUIT:
	  ioctl (XFASTINT (p->infd), TIOCGETC, &c);
	  send_process (proc, &c.t_quitc, 1);
	  return Qnil;
	case SIGTSTP:
	  ioctl (XFASTINT (p->infd), TIOCGLTC, &lc);
	  send_process (proc, &lc.t_suspc, 1);
	  return Qnil;
	}
#endif /* have TIOCGLTC and have TIOCGETC */
      /* It is possible that the following code would work
	 on other kinds of USG systems, not just on the IRIS.
	 This should be tried in Emacs 19.  */
#if defined (IRIS) && defined (HAVE_SETSID) /* Check for Irix, not older
					       systems.  */
      struct termio t;
      switch (signo)
	{
	case SIGINT:
	  ioctl (XFASTINT (p->infd), TCGETA, &t);
	  send_process (proc, &t.c_cc[VINTR], 1);
	  return Qnil;
	case SIGQUIT:
	  ioctl (XFASTINT (p->infd), TCGETA, &t);
	  send_process (proc, &t.c_cc[VQUIT], 1);
	  return Qnil;
	case SIGTSTP:
	  ioctl (XFASTINT (p->infd), TCGETA, &t);
	  send_process (proc, &t.c_cc[VSWTCH], 1);
	  return Qnil;
	}
#endif /* IRIS and HAVE_SETSID */

      /* Get the pgrp using the tty itself, if we have that.
	 Otherwise, use the pty to get the pgrp.  */
      if (!NILP (p->subtty))
	ioctl (XFASTINT (p->subtty), TIOCGPGRP, &gid);
      else
	ioctl (XFASTINT (p->infd), TIOCGPGRP, &gid);
      if (gid == -1)
	no_pgrp = 1;
      else
	gid = - gid;
    }
  else
    gid = - XFASTINT (p->pid);
#else /* not using pgrps */
  /* Can't select pgrps on this system, so we know that
     the child itself heads the pgrp.  */
  gid = - XFASTINT (p->pid);
#endif /* not using pgrps */

  switch (signo)
    {
#ifdef SIGCONT
    case SIGCONT:
      p->raw_status_low = Qnil;
      p->raw_status_high = Qnil;
      p->status = Qrun;
      XSETINT (p->tick, ++process_tick);
      if (!nomsg)
	status_notify ();
      break;
#endif
    case SIGINT:
    case SIGQUIT:
    case SIGKILL:
      flush_pending_output (XFASTINT (p->infd));
      break;
    }

  /* If we don't have process groups, send the signal to the immediate subprocess.
     That isn't really right, but it's better than any obvious alternative.  */
  if (no_pgrp)
    {
      kill (XFASTINT (p->pid), signo);
      return;
    }

  /* gid may be a pid, or minus a pgrp's number */
#ifdef TIOCSIGSEND
  if (!NILP (current_group))
    ioctl (XFASTINT (p->infd), TIOCSIGSEND, signo);
  else
    {
      gid = - XFASTINT (p->pid);
      kill (gid, signo);
    }
#else /* no TIOCSIGSEND */
#ifdef BSD
  /* On bsd, [man says] kill does not accept a negative number to kill a pgrp.
     Must do that differently.  */
  killpg (-gid, signo);
#else /* Not BSD.  */
  kill (gid, signo);
#endif /* Not BSD.  */
#endif /* no TIOCSIGSEND */
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
  error ("no SIGTSTP support");
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
    error ("no SIGCONT support");
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
  /* Sending a zero-length record is supposed to mean eof
     when TIOCREMOTE is turned on.  */
#ifdef DID_REMOTE
  {
    char buf[1];
    write (XFASTINT (XPROCESS (proc)->outfd), buf, 0);
  }
#else /* did not do TOICREMOTE */
  if (!NILP (XPROCESS (proc)->pty_flag))
    send_process (proc, "\004", 1);
  else
    {
      close (XPROCESS (proc)->outfd);
      XFASTINT (XPROCESS (proc)->outfd) = open ("/dev/NILP", O_WRONLY);
    }

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

  for (tail = Vprocess_alist; XGCTYPE (tail) == Lisp_Cons;
       tail = XCONS (tail)->cdr)
    {
      proc = XCONS (XCONS (tail)->car)->cdr;
      if (XGCTYPE (proc) == Lisp_Process
	  && (NILP (buffer) || EQ (XPROCESS (proc)->buffer, buffer)))
	{
	  if (NETCONN_P (proc))
	    deactivate_process (proc);
	  else if (XFASTINT (XPROCESS (proc)->infd))
	    process_send_signal (proc, SIGHUP, Qnil, 1);
	}
    }
}

int
count_active_processes ()
{
  register Lisp_Object tail;
  register int count = 0;

  for (tail = Vprocess_alist; !NILP (tail); tail = Fcdr (tail))
    {
      Lisp_Object status;

      status = XPROCESS (Fcdr (Fcar (tail)))->status;
      if (SYMBOLP (status)
	  && (EQ (status, Qrun) || EQ (status, Qstop) || EQ (status, Qopen)))
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

void
sigchld_handler (signo)
     int signo;
{
  int old_errno = errno;
  Lisp_Object proc;
  register struct Lisp_Process *p;

#ifdef BSD4_1
  extern int synch_process_pid;
  extern int sigheld;
  sigheld |= sigbit (SIGCHLD);
#endif

  while (1)
    {
      register int pid;
      WAITTYPE w;
      Lisp_Object tail;

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
	  return;
	}
#else
      pid = wait (&w);
#endif /* no WNOHANG */

#ifdef BSD4_1
      if (synch_process_pid == pid)
	synch_process_pid = 0;         /* Zero it to show process has died. */
#endif

      /* Find the process that signaled us, and record its status.  */

      p = 0;
      for (tail = Vprocess_alist; XSYMBOL (tail) != XSYMBOL (Qnil); tail = XCONS (tail)->cdr)
	{
	  proc = XCONS (XCONS (tail)->car)->cdr;
	  p = XPROCESS (proc);
	  if (EQ (p->childp, Qt) && XFASTINT (p->pid) == pid)
	    break;
	  p = 0;
	}

      /* Look for an asynchronous process whose pid hasn't been filled
	 in yet.  */
      if (p == 0)
	for (tail = Vprocess_alist; XSYMBOL (tail) != XSYMBOL (Qnil); tail = XCONS (tail)->cdr)
	  {
	    proc = XCONS (XCONS (tail)->car)->cdr;
	    p = XPROCESS (proc);
	    if (XINT (p->pid) == -1)
	      break;
	    p = 0;
	  }

      /* Change the status of the process that was found.  */

      if (p != 0)
	{
	  union { int i; WAITTYPE wt; } u;

	  XSETINT (p->tick, ++process_tick);
	  u.wt = w;
	  XFASTINT (p->raw_status_low) = u.i & 0xffff;
	  XFASTINT (p->raw_status_high) = u.i >> 16;

	  /* If process has terminated, stop waiting for its output.  */
	  if (WIFSIGNALED (w) || WIFEXITED (w))
	    if (p->infd) {
	      FD_CLR (p->infd, &input_wait_mask);
	      /* We can't just call event_stream->unselect_process_cb (p)
		 here, because that calls XtRemoveInput, which is not
		 necessarily reentrant, so we can't call this at interrupt
		 level.
	       */
	    }
	}

	/* There was no asynchronous process found for that id.  Check
	   if we have a synchronous process.  */
      else
	{
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
      return;
#endif /* USG, but not HPUX with WNOHANG */
    }
}

/* Report all recent events of a change in process status
   (either run the sentinel or output a message).
   This is done while Emacs is waiting for keyboard input.  */

status_notify ()
{
  register Lisp_Object proc, buffer;
  Lisp_Object tail = Qnil;
  Lisp_Object msg = Qnil;
  struct gcpro gcpro1, gcpro2;

  /* We need to gcpro tail; if read_process_output calls a filter
     which deletes a process and removes the cons to which tail points
     from Vprocess_alist, tail becomes an unprotected reference.  */
  GCPRO2 (tail, msg);

  for (tail = Vprocess_alist; !NILP (tail); tail = Fcdr (tail))
    {
      Lisp_Object symbol;
      register struct Lisp_Process *p;

      proc = Fcdr (Fcar (tail));
      p = XPROCESS (proc);

      if (XINT (p->tick) != XINT (p->update_tick))
	{
	  XSETINT (p->update_tick, XINT (p->tick));

	  /* If process is still active, read any output that remains.  */
	  if (XFASTINT (p->infd))
	    {
	      int inchannel = p->infd;

	      /* Must call this before setting the file descriptors to 0 */
	      event_stream->unselect_process_cb (p);
	      while (read_process_output (proc, inchannel) > 0);
	      close (XFASTINT (p->infd));
	      if (!EQ (p->infd, p->outfd))
		close (XFASTINT (p->outfd));
	      XFASTINT (p->infd) = 0;
	      XFASTINT (p->outfd) = 0;
	      chan_process[inchannel] = Qnil;
	      FD_CLR (inchannel, &input_wait_mask);
	    }

	  buffer = p->buffer;

	  /* Get the text to use for the message.  */
	  if (!NILP (p->raw_status_low))
	    update_status (p);
	  msg = status_message (p->status);

	  /* If process is terminated, deactivate it or delete it.  */
	  symbol = p->status;
	  if (CONSP (p->status))
	    symbol = XCONS (p->status)->car;

	  if (EQ (symbol, Qsignal) || EQ (symbol, Qexit)
	      || EQ (symbol, Qclosed))
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
	  else if (!EQ (symbol, Qrun) && !NILP (buffer))
	    {
	      Lisp_Object ro = XBUFFER (buffer)->read_only;
	      Lisp_Object tem;
	      struct buffer *old = current_buffer;
	      int opoint;

	      /* Avoid error if buffer is deleted
		 (probably that's why the process is dead, too) */
	      if (NILP (XBUFFER (buffer)->name))
		continue;
	      Fset_buffer (buffer);
	      opoint = point;
	      /* Insert new output into buffer
		 at the current end-of-output marker,
		 thus preserving logical ordering of input and output.  */
	      if (XMARKER (p->mark)->buffer)
		SET_PT (marker_position (p->mark));
	      else
		SET_PT (ZV);
	      if (point <= opoint)
		opoint += XSTRING (msg)->size + XSTRING (p->name)->size + 10;

	      tem = current_buffer->read_only;
	      current_buffer->read_only = Qnil;
	      insert_string ("\nProcess ");
	      Finsert (1, &p->name);
	      insert_string (" ");
	      Finsert (1, &msg);
	      current_buffer->read_only = tem;
	      Fset_marker (p->mark, make_number (point), p->buffer);

	      SET_PT (opoint);
	      internal_set_buffer (old);
	    }
	}
    } /* end for */

  redraw_mode_line++;  /* in case buffers use %s in mode-line-format */
  redisplay_preserving_echo_area ();

  update_tick = process_tick;

  UNGCPRO;
}

void
maybe_status_notify ()
{
  if (update_tick != process_tick) status_notify ();
}

exec_sentinel (proc, reason)
     Lisp_Object proc, reason;
{
  Lisp_Object sentinel;
  register struct Lisp_Process *p = XPROCESS (proc);
  int count = specpdl_ptr - specpdl;

  sentinel = p->sentinel;
  if (NILP (sentinel))
    return;

  p->sentinel = Qnil;
  specbind (Qinhibit_quit, Qt);
  this_filter = sentinel;
  filter_process = proc;
  filter_string = reason;
  call2 (this_filter, filter_process, filter_string);
/*   internal_condition_case (run_filter, Qerror, Fidentity);  */
  unbind_to (count);
  p->sentinel = sentinel;
}

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
  Vprocess_alist = Qnil;
  for (i = 0; i < MAXDESC; i++)
    {
      chan_process[i] = Qnil;
      proc_buffered_char[i] = -1;
    }
}
#if 0
DEFUN ("process-connection", Fprocess_connection, Sprocess_connection, 0, 1, 0,
 "Return the connection type of `PROCESS'.  This can be nil (pipe),\n\
t or pty (pty) or stream (socket connection).")
  (process)
     Lisp_Object process;
{
  return XPROCESS (process)->type;
}
#endif
syms_of_process ()
{
#ifdef HAVE_PTYS
  pty_process = intern ("pty");
#endif
#ifdef HAVE_SOCKETS
  stream_process = intern ("stream");
#endif
  Qprocessp = intern ("processp");
  staticpro (&Qprocessp);
  Qrun = intern ("run");
  staticpro (&Qrun);
  Qstop = intern ("stop");
  staticpro (&Qstop);
  Qsignal = intern ("signal");
  staticpro (&Qsignal);

  /* Qexit is already staticpro'd by syms_of_eval; don't staticpro it
     here again.

     Qexit = intern ("exit");
     staticpro (&Qexit); */

  Qopen = intern ("open");
  staticpro (&Qopen);
  Qclosed = intern ("closed");
  staticpro (&Qclosed);

  staticpro (&Vprocess_alist);

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
  defsubr (&Slist_processes);
  defsubr (&Sprocess_list);
  defsubr (&Sstart_process);
#ifdef HAVE_SOCKETS
  defsubr (&Sopen_network_stream);
#endif /* HAVE_SOCKETS */
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
