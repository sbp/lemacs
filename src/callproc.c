/* Synchronous subprocess invocation for GNU Emacs.
   Copyright (C) 1985-1993 Free Software Foundation, Inc.

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

#include <stdio.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/resource.h>

#include <sys/types.h>
#define PRIO_PROCESS 0
#include <sys/file.h>
#ifdef USG5
#include <fcntl.h>
#endif

#ifndef O_RDONLY
#define O_RDONLY 0
#endif

#ifndef O_WRONLY
#define O_WRONLY 1
#endif

#if defined(sun) && !defined(USG)
# include <vfork.h>
#endif

#include "commands.h"
#include "buffer.h"
#include "paths.h"
#include "process.h"
#include "insdel.h"

#define max(a, b) ((a) > (b) ? (a) : (b))

Lisp_Object Vexec_path, Vexec_directory;

Lisp_Object Vshell_file_name;

#ifndef MAINTAIN_ENVIRONMENT
/* List of strings to append to front of environment of
   all subprocesses when they are started.  */

Lisp_Object Vprocess_environment;
#endif

#ifdef BSD4_1
/* Set nonzero when a synchronous subprocess is made,
   and set to zero again when it is observed to die.
   We wait for this to be zero in order to wait for termination.  */
int synch_process_pid;
#endif /* BSD4_1 */

/* True iff we are about to fork off a synchronous process or if we
   are waiting for it.  */
/* int synch_process_alive; */

/* Nonzero => this is a string explaining death of synchronous subprocess.  */
char *synch_process_death;

/* Exit code of synchronous subprocess if positive,
   minus the signal number if negative.  */
int synch_process_retcode;

void child_setup ();


static Lisp_Object
call_process_cleanup (fdpid)
     Lisp_Object fdpid;
{
  register Lisp_Object fd, pid;
  fd = Fcar (fdpid);
  pid = Fcdr (fdpid);
  close (XFASTINT (fd));
  kill (XFASTINT (pid), SIGKILL);
  return Qnil;
}

extern int errno;
extern char *sys_errlist[];
extern int sys_nerr;

static Lisp_Object fork_error;

static void
report_fork_error (string, data)
     char *string;
     Lisp_Object data;
{
  Lisp_Object errstring;

  if (errno >= 0 && errno < sys_nerr)
    errstring = build_string (sys_errlist[errno]);
  else
    errstring = build_string ("undocumented error code");

  /* System error messages are capitalized.  Downcase the initial. */
  XSTRING (errstring)->data[0] = DOWNCASE (XSTRING (errstring)->data[0]);

  fork_error = Fcons (build_string (string), Fcons (errstring, data));

  /* terminate this branch of the fork, without closing stdin/out/etc. */
  _exit (0);
}

#ifdef VMS
#ifdef __GNUC__
#define	environ $$PsectAttributes_NOSHR$$environ
extern char **environ;
#else
extern noshare char **environ;
#endif
#else
extern char **environ;
#endif

extern void wait_for_termination (int);

DEFUN ("call-process", Fcall_process, Scall_process, 1, MANY, 0,
  "Call PROGRAM synchronously in separate process.\n\
The program's input comes from file INFILE (nil means `/dev/null').\n\
Insert output in BUFFER before point; t means current buffer;\n\
 nil for BUFFER means discard it; 0 means discard and don't wait.\n\
Fourth arg DISPLAY non-nil means redisplay buffer as output is inserted.\n\
Remaining arguments are strings passed as command arguments to PROGRAM.\n\
If BUFFER is nil or 0, returns immediately with value nil.\n\
Otherwise waits for PROGRAM to terminate\n\
and returns a numeric exit status or a signal name as a string.\n\
If you quit, the process is killed with SIGKILL.")
  (nargs, args)
     int nargs;
     register Lisp_Object *args;
{
  Lisp_Object display, buffer, path;
  int fd[2];
  int filefd;
  register int pid;
  char buf[1024];
  int count = specpdl_depth;
  register unsigned char **new_argv
    = (unsigned char **) alloca ((max (2, nargs - 2)) * sizeof (char *));
  struct buffer *old = current_buffer;

  CHECK_STRING (args[0], 0);

  if (nargs <= 1 || NILP (args[1]))
#ifdef VMS
    args[1] = build_string ("NLA0:");
#else
    args[1] = build_string ("/dev/null");
#endif /* not VMS */
  else
    args[1] = Fexpand_file_name (args[1], current_buffer->directory);

  CHECK_STRING (args[1], 1);

  {
    register Lisp_Object tem;
    buffer = tem = args[2];
    if (nargs <= 2)
      buffer = Qnil;
    else if (!(EQ (tem, Qnil) || EQ (tem, Qt)
	       || XFASTINT (tem) == 0))
      {
	buffer = Fget_buffer (tem);
	CHECK_BUFFER (buffer, 2);
      }
  }

  display = nargs > 3 ? args[3] : Qnil;

  {
    register int i;
    for (i = 4; i < nargs; i++)
      {
	CHECK_STRING (args[i], i);
	new_argv[i - 3] = XSTRING (args[i])->data;
      }
    /* Program name is first command arg */
    new_argv[0] = XSTRING (args[0])->data;
    new_argv[i - 3] = 0;
  }

  filefd = open ((char *)XSTRING (args[1])->data, O_RDONLY, 0);
  if (filefd < 0)
    {
      report_file_error ("Opening process input file", Fcons (args[1], Qnil));
    }
  /* Search for program; barf if not found.  */
  locate_file (Vexec_path, args[0], "", &path, X_OK);
  if (NILP (path))
    {
      close (filefd);
      report_file_error ("Searching for program", Fcons (args[0], Qnil));
    }
  new_argv[0] = XSTRING (path)->data;

  if (FIXNUMP (buffer))
#ifdef VMS
    fd[1] = open ("NLA0:", 0), fd[0] = -1;
#else
    fd[1] = open ("/dev/null", O_WRONLY), fd[0] = -1;
#endif /* not VMS */
  else
    {
      pipe (fd);
#if 0
      /* Replaced by close_process_descs */
      set_exclusive_use (fd[0]);
#endif
    }

  synch_process_death = 0;
  synch_process_retcode = 0;

  {
    /* child_setup must clobber environ in systems with true vfork.
       Protect it from permanent change.  */
    register char **save_environ = environ;
    register int fd1 = fd[1];
    char **env;
#ifdef EMACS_BTL
    /* when performance monitoring is on, turn it off before the vfork(),
       as the child has no handler for the signal -- when back in the
       parent process, turn it back on if it was really on when you "turned
       it off" */
    extern int cadillac_stop_logging ();
    extern int cadillac_start_logging ();
    int logging_on = 0;
#endif

#ifdef MAINTAIN_ENVIRONMENT
    env = (char **) alloca (size_of_current_environ ());
    get_current_environ (env);
#else
    env = environ;
#endif /* MAINTAIN_ENVIRONMENT */

#ifdef EMACS_BTL
    logging_on = cadillac_stop_logging ();
#endif

    fork_error = Qnil;
    pid = vfork ();
#ifdef BSD4_1
    /* cause SIGCHLD interrupts to look for this pid. */
    synch_process_pid = pid;
#endif /* BSD4_1 */

    if (pid == 0)
      {
	if (fd[0] >= 0)
	  close (fd[0]);
#ifdef USG
#ifdef HAVE_PTYS
	setpgrp ();
#endif
#endif /* USG */
	child_setup (filefd, fd1, fd1, new_argv, env);
      }
#ifdef EMACS_BTL
    else if (logging_on)
      cadillac_start_logging ();
#endif

    environ = save_environ;

    close (filefd);
    close (fd1);
  }

  if (!NILP (fork_error))
    while (1) Fsignal (Qfile_error, fork_error);

  if (pid < 0)
    {
      close (fd[0]);
      report_file_error ("Doing vfork", Qnil);
    }

  if (FIXNUMP (buffer))
    {
#ifndef subprocesses
      wait_without_blocking ();
#endif /* subprocesses */
      return Qnil;
    }

  record_unwind_protect (call_process_cleanup,
			 Fcons (make_number (fd[0]), make_number (pid)));


  if (BUFFERP (buffer))
    Fset_buffer (buffer);

  immediate_quit = 1;
  QUIT;

  {
    register int nread;

    while ((nread = read (fd[0], buf, sizeof buf)) > 0)
      {
	immediate_quit = 0;
	if (!NILP (buffer))
	  insert_raw_string (buf, nread);
	if (!NILP (display) && INTERACTIVE)
	  redisplay_preserving_echo_area ();
	immediate_quit = 1;
	QUIT;
      }
  }

  /* Wait for it to terminate, unless it already has.  */
  wait_for_termination (pid);

  immediate_quit = 0;

  internal_set_buffer (old);

  unbind_to (count, Qnil);

  if (synch_process_death)
    return build_string (synch_process_death);
  return make_number (synch_process_retcode);
}

static Lisp_Object
delete_temp_file (name)
     Lisp_Object name;
{
  unlink ((char *) XSTRING (name)->data);
  return (Qnil);
}

DEFUN ("call-process-region", Fcall_process_region, Scall_process_region,
  3, MANY, 0,
  "Send text from START to END to a synchronous process running PROGRAM.\n\
Delete the text if fourth arg DELETE is non-nil.\n\
Insert output in BUFFER before point; t means current buffer;\n\
 nil for BUFFER means discard it; 0 means discard and don't wait.\n\
Sixth arg DISPLAY non-nil means redisplay buffer as output is inserted.\n\
Remaining args are passed to PROGRAM at startup as command args.\n\
If BUFFER is nil, returns immediately with value nil.\n\
Otherwise waits for PROGRAM to terminate\n\
and returns a numeric exit status or a signal name as a string.\n\
If you quit, the process is killed with SIGKILL.")
  (nargs, args)
     int nargs;
     register Lisp_Object *args;
{
  register Lisp_Object filename_string, start, end;
  char tempfile[20];
  int count = specpdl_depth;
  Lisp_Object result;

  strcpy (tempfile, "/tmp/emacsXXXXXX");
  mktemp (tempfile);

  filename_string = build_string (tempfile);
  start = args[0];
  end = args[1];
  Fwrite_region (start, end, filename_string, Qnil, Qlambda);
  record_unwind_protect (delete_temp_file, filename_string);

  if (!NILP (args[3]))
    Fdelete_region (start, end);

  args[3] = filename_string;
  result = Fcall_process (nargs - 2, args + 2);
  return unbind_to (count, result);
}

/* This is the last thing run in a newly forked inferior
   either synchronous or asynchronous.
   Copy descriptors IN, OUT and ERR as descriptors 0, 1 and 2.
   Initialize inferior's priority, pgrp, connected dir and environment.
   then exec another program based on new_argv.

   This function may change environ for the superior process.
   Therefore, the superior process must save and restore the value
   of environ around the vfork and the call to this function.

   ENV is the environment for the subprocess. */

extern void close_process_descs (void);
extern void setpgrp_of_tty (int);

extern pid_t setpgrp ();

void
child_setup (in, out, err, new_argv, env)
     int in, out, err;
     register char **new_argv;
     char **env;
{
  register int pid = getpid();

  setpriority (PRIO_PROCESS, pid, 0);

#ifdef subprocesses
  /* Close Emacs's descriptors that this process should not have.  */
  close_process_descs ();
#endif

  /* Note that use of alloca is always safe here.  It's obvious for systems
     that do not have true vfork or that have true (stack) alloca.
     If using vfork and C_ALLOCA it is safe because that changes
     the superior's static variables as if the superior had done alloca
     and will be cleaned up in the usual way.  */

  if (STRINGP (current_buffer->directory))
    {
      register unsigned char *temp;
      register int i;

      i = XSTRING (current_buffer->directory)->size;
      temp = (unsigned char *) alloca (i + 2);
      memcpy (temp, XSTRING (current_buffer->directory)->data, i);
      if (temp[i - 1] != '/') temp[i++] = '/';
      temp[i] = 0;
      /* Switch to that directory, and report any error.  */

#if 0
 /* don't report the chdir error, or ange-ftp.el doesn't work. */
      if (chdir (temp) < 0)
	report_fork_error ("In chdir",
			   Fcons (current_buffer->directory, Qnil));
#else
      chdir ((char *) temp);
#endif
    }

#ifndef MAINTAIN_ENVIRONMENT
  /* Set `env' to a vector of the strings in Vprocess_environment.  */
  {
    register Lisp_Object tem;
    register char **new_env;
    register int new_length;

    new_length = 0;
    for (tem = Vprocess_environment;
	 (CONSP (tem)
	  && STRINGP (XCONS (tem)->car));
	 tem = XCONS (tem)->cdr)
      new_length++;

    /* new_length + 1 to include terminating 0 */
    env = new_env = (char **) alloca ((new_length + 1) * sizeof (char *));

    /* Copy the env strings into new_env.  */
    for (tem = Vprocess_environment;
	 (CONSP (tem)
	  && STRINGP (XCONS (tem)->car));
	 tem = XCONS (tem)->cdr)
      *new_env++ = (char *) XSTRING (XCONS (tem)->car)->data;
    *new_env = 0;
  }
#endif /* Not MAINTAIN_ENVIRONMENT */

  close (0);
  close (1);
  close (2);

  dup2 (in, 0);
  dup2 (out, 1);
  dup2 (err, 2);
  close (in);
  close (out);
  close (err);

#ifdef USG
#ifndef HAVE_PTYS
  setpgrp ();			/* No arguments but equivalent in this case */
#endif
#else
  setpgrp (pid, pid);
#endif /* USG */
  setpgrp_of_tty (pid);

#ifdef vipc
  something missing here;
#endif /* vipc */

  /* execvp does not accept an environment arg so the only way
     to pass this environment is to set environ.  Our caller
     is responsible for restoring the ambient value of environ.  */
  environ = env;
  execvp (new_argv[0], new_argv);

  write (1, "Couldn't exec the program ", 26);
  write (1, new_argv[0], strlen (new_argv[0]));
  _exit (1);
}

void
init_callproc ()
{
  register char * sh;
  Lisp_Object execdir;

#ifdef PATH_EXEC
  /* Turn PATH_EXEC into a path.  `==' is just a string which we know
     will not be the name of an environment variable.  */
  Vexec_path = decode_env_path ("==", PATH_EXEC);
#else
  Vexec_path = Qnil;
#endif
  if (NILP (Vexec_path))
    {
      Vexec_directory = Qnil;
      execdir = Qnil;
    }
  else
    {
      Vexec_directory = Ffile_name_as_directory (Fcar (Vexec_path));
      execdir = Fdirectory_file_name (Vexec_directory);
    }
  Vexec_path = nconc2 (decode_env_path ("PATH", ""), Vexec_path);

  if (!NILP (execdir) && access ((char *)XSTRING (execdir)->data, 0) < 0)
    {
      printf ("Warning: executable/documentation dir (%s) does not exist.\n",
	      XSTRING (Vexec_directory)->data);
      sleep (2);
    }

  sh = (char *) egetenv ("SHELL");
  Vshell_file_name = build_string (sh ? sh : "/bin/sh");

#ifndef MAINTAIN_ENVIRONMENT
  /* The equivalent of this operation was done
     in init_environ in environ.c if MAINTAIN_ENVIRONMENT */
  Vprocess_environment = Qnil;
#ifndef CANNOT_DUMP
  if (initialized)
#endif
    {
      char **envp;
      for (envp = environ; *envp; envp++)
	Vprocess_environment = Fcons (build_string (*envp),
				      Vprocess_environment);
    }
#endif /* MAINTAIN_ENVIRONMENT */
}

void
syms_of_callproc ()
{
  DEFVAR_LISP ("shell-file-name", &Vshell_file_name,
    "*File name to load inferior shells from.\n\
Initialized from the SHELL environment variable.");

  DEFVAR_LISP ("exec-path", &Vexec_path,
    "*List of directories to search programs to run in subprocesses.\n\
Each element is a string (directory name) or nil (try default directory).");

  DEFVAR_LISP ("exec-directory", &Vexec_directory,
    "Directory that holds programs that come with GNU Emacs,\n\
intended for Emacs to invoke.");

#ifndef MAINTAIN_ENVIRONMENT
  DEFVAR_LISP ("process-environment", &Vprocess_environment,
    "List of strings to append to environment of subprocesses that are started.\n\
Each string should have the format ENVVARNAME=VALUE.");
#endif

  defsubr (&Scall_process);
  defsubr (&Scall_process_region);
}
