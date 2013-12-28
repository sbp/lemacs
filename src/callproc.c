/* Synchronous subprocess invocation for GNU Emacs.
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

#include <stdio.h>		/* For sprintf */
#if defined(IRIX)
#include <errno.h>	/* From gparker@sni-usa.com for IRIX 4.0.5E */
#endif
#include <sys/errno.h>
#include <sys/time.h>
#include <sys/resource.h>

/* Define SIGCHLD as an alias for SIGCLD.  */

#if !defined (SIGCHLD) && defined (SIGCLD)
#define SIGCHLD SIGCLD
#endif /* SIGCLD */

#include <sys/types.h>
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

#if defined(sun) && !defined(USG) && !defined(MACH)
# include <vfork.h>
#endif

#include "lisp.h"
#include "intl.h"
#include "commands.h"
#include "buffer.h"
#include "paths.h"
#include "process.h"
#include "syssignal.h"
#include "insdel.h"
#include "dispmisc.h"

#include "sysdep.h"

#define max(a, b) ((a) > (b) ? (a) : (b))

Lisp_Object Vexec_path, Vexec_directory, Vdata_directory;

Lisp_Object Vshell_file_name;

/* The environment to pass to all subprocesses when they are started.
   This is in the semi-bogus format of ("VAR=VAL" "VAR2=VAL2" ... )
 */
Lisp_Object Vprocess_environment;

/* True iff we are about to fork off a synchronous process or if we
   are waiting for it.  */
int synch_process_alive;

/* Nonzero => this is a string explaining death of synchronous subprocess.  */
CONST char *synch_process_death;

/* If synch_process_death is zero,
   this is exit code of synchronous subprocess.  */
int synch_process_retcode;

#ifndef VMS  /* VMS version is in vmsproc.c.  */

static Lisp_Object
call_process_kill (fdpid)
     Lisp_Object fdpid;
{
  Lisp_Object fd = Fcar (fdpid);
  Lisp_Object pid = Fcdr (fdpid);

  if (!NILP (fd))
    emacs_close (XINT (fd));

  if (!NILP (pid))
    EMACS_KILLPG (XINT (pid), SIGKILL);
  
  synch_process_alive = 0;
  return Qnil;
}

static Lisp_Object
call_process_cleanup (fdpid)
     Lisp_Object fdpid;
{
  int fd = XINT (Fcar (fdpid));
  int pid = XINT (Fcdr (fdpid));

  if (EMACS_KILLPG (pid, SIGINT) == 0)
  {
    int speccount = specpdl_depth ();

    record_unwind_protect (call_process_kill, fdpid);
    /*>>>>>>> "c-G" -- need non-consing Single-key-description */
    message (GETTEXT ("Waiting for process to die...(type C-g again to kill it instantly)"));

    immediate_quit = 1;
    QUIT;
    wait_for_termination (pid);
    immediate_quit = 0;
    /* "Discard" the unwind protect.  */
    XCONS (fdpid)->car = Qnil;
    XCONS (fdpid)->cdr = Qnil;
    unbind_to (speccount, Qnil);

    message (GETTEXT ("Waiting for process to die... done"));
  }
  synch_process_alive = 0;
  emacs_close (fd);
  return Qnil;
}

static Lisp_Object fork_error;
#if 0 /* UNUSED */
static void
report_fork_error (string, data)
     char *string;
     Lisp_Object data;
{
  Lisp_Object errstring;

  if (errno >= 0 && errno < sys_nerr)
    errstring = build_string (sys_errlist[errno]);
  else
    errstring = build_string (GETTEXT ("undocumented error code"));

  /* System error messages are capitalized.  Downcase the initial. */
  XSTRING (errstring)->data[0] = DOWNCASE (XSTRING (errstring)->data[0]);

  fork_error = Fcons (build_string (string), Fcons (errstring, data));

  /* terminate this branch of the fork, without closing stdin/out/etc. */
  _exit (1);
}
#endif /* unused */


DEFUN ("call-process", Fcall_process, Scall_process, 1, MANY, 0,
  "Call PROGRAM synchronously in separate process.\n\
The program's input comes from file INFILE (nil means `/dev/null').\n\
Insert output in BUFFER before point; t means current buffer;\n\
 nil for BUFFER means discard it; 0 means discard and don't wait.\n\
Fourth arg DISPLAY non-nil means redisplay buffer as output is inserted.\n\
Remaining arguments are strings passed as command arguments to PROGRAM.\n\
If BUFFER is nil or 0, returns immediately with value nil.\n\
Otherwise waits for PROGRAM to terminate\n\
and returns a numeric exit status or a signal description string.\n\
If you quit, the process is killed with SIGINT, or SIGKILL if you quit again.")
  (nargs, args)
     int nargs;
     register Lisp_Object *args;
{
  Lisp_Object infile, buffer, current_dir, display, path;
  int fd[2];
  int filefd;
  register int pid;
  char buf[1024];
  int speccount = specpdl_depth ();
  register char **new_argv
    = (char **) alloca ((max (2, nargs - 2)) * sizeof (char *));
#if 0
  int mask;
#endif

  CHECK_STRING (args[0], 0);

  /* Do this before building new_argv because GC in Lisp code
   *  called by various filename-hacking routines might relocate strings */
  locate_file (Vexec_path, args[0], EXEC_SUFFIXES, &path, X_OK);

  /* Make sure that the child will be able to chdir to the current
     buffer's current directory, or its unhandled equivalent.  We
     can't just have the child check for an error when it does the
     chdir, since it's in a vfork. */
  {
    struct gcpro gcpro1, gcpro2;
    /* Do this test before building new_argv because GC in Lisp code 
     *  called by various filename-hacking routines might relocate strings */
    /* Make sure that the child will be able to chdir to the current
       buffer's current directory.  We can't just have the child check
       for an error when it does the chdir, since it's in a vfork.  */

    GCPRO2 (current_dir, path);   /* Caller gcprotects args[] */
    current_dir = current_buffer->directory;
    current_dir = expand_and_dir_to_file
      (Funhandled_file_name_directory (current_dir), Qnil);
#if 0
  /* I don't know how RMS intends this crock of shit to work, but it
     breaks everything in the presence of ange-ftp-visited files, so
     fuck it. */
    if (NILP (Ffile_accessible_directory_p (current_dir)))
      report_file_error (GETTEXT ("Setting current directory"),
                         Fcons (current_buffer->directory, Qnil));
#endif /* 0 */
    UNGCPRO;
  }

  if (nargs >= 2 && ! NILP (args[1]))
    {
      infile = Fexpand_file_name (args[1], current_buffer->directory);
      CHECK_STRING (infile, 1);
    }
  else
    infile = build_string (NULL_DEVICE);

  if (nargs >= 3)
    {
      register Lisp_Object tem;

      buffer = tem = args[2];
      if (!(EQ (tem, Qnil)
	    || EQ (tem, Qt)
	    || EQ (tem, Qzero)))
	{
	  buffer = Fget_buffer (tem);
	  CHECK_BUFFER (buffer, 2);
	}
    }
  else 
    buffer = Qnil;

  display = ((nargs >= 4) ? args[3] : Qnil);

  /* From here we assume we won't GC (unless an error is signalled.) */
  {
    register int i;
    for (i = 4; i < nargs; i++)
      {
	CHECK_STRING (args[i], i);
	new_argv[i - 3] = (char *) XSTRING (args[i])->data;
      }
    /* Program name is first command arg */
    new_argv[0] = (char *) XSTRING (args[0])->data;
    new_argv[i - 3] = 0;
  }

  filefd = emacs_open ((char *)XSTRING (infile)->data, O_RDONLY, 0);
  if (filefd < 0)
    {
      report_file_error (GETTEXT ("Opening process input file"),
			 Fcons (infile, Qnil));
    }

  if (NILP (path))
    {
      emacs_close (filefd);
      report_file_error (GETTEXT ("Searching for program"),
			 Fcons (args[0], Qnil));
    }
  new_argv[0] = (char *) XSTRING (path)->data;

  if (FIXNUMP (buffer))
    {
      fd[1] = emacs_open (NULL_DEVICE, O_WRONLY, 0);
      fd[0] = -1;
    }
  else
    {
      pipe (fd);
#if 0
      /* Replaced by close_process_descs */
      set_exclusive_use (fd[0]);
#endif
    }

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

    env = environ;

#if 0  /* Some systems don't have sigblock.  */
    mask = sigblock (sigmask (SIGCHLD));
#endif

    /* Record that we're about to create a synchronous process.  */
    synch_process_alive = 1;

#ifdef EMACS_BTL
    logging_on = cadillac_stop_logging ();
#endif

    /* These vars record information from process termination.
       Clear them now before process can possibly terminate,
       to avoid timing error if process terminates soon.  */
    synch_process_death = 0;
    synch_process_retcode = 0;

    fork_error = Qnil;
    pid = vfork ();

    if (pid == 0)
      {
	if (fd[0] >= 0)
	  emacs_close (fd[0]);
#ifdef USG
	setpgrp ();
#else
        setpgrp (pid, pid);
#endif /* USG */
	child_setup (filefd, fd1, fd1, new_argv, env, 0, 
                     (char *) XSTRING (current_dir)->data);
      }
#ifdef EMACS_BTL
    else if (logging_on)
      cadillac_start_logging ();
#endif

#if 0
    /* Tell SIGCHLD handler to look for this pid.  */
    synch_process_pid = pid;
    /* Now let SIGCHLD come through.  */
    sigsetmask (mask);
#endif

    environ = save_environ;

    emacs_close (filefd);
    emacs_close (fd1);
  }

  if (!NILP (fork_error))
    signal_error (Qfile_error, fork_error);

  if (pid < 0)
    {
      emacs_close (fd[0]);
      report_file_error (GETTEXT ("Doing vfork"), Qnil);
    }

  if (FIXNUMP (buffer))
    {
#ifndef subprocesses
      /* If Emacs has been built with asynchronous subprocess support,
	 we don't need to do this, I think because it will then have
	 the facilities for handling SIGCHLD.  */
      wait_without_blocking ();
#endif /* subprocesses */
      return Qnil;
    }

  {
    Lisp_Object old = Fcurrent_buffer ();
    struct gcpro gcpro1;
    register int nread;

    GCPRO1 (old);

    record_unwind_protect (call_process_cleanup,
			   Fcons (make_number (fd[0]), make_number (pid)));

    if (BUFFERP (buffer))
      Fset_buffer (buffer);

    immediate_quit = 1;
    QUIT;

    while ((nread = emacs_read (fd[0], buf, sizeof buf)) > 0)
      {
	immediate_quit = 0;
	if (!NILP (buffer))
	  insert_raw_string (buf, nread);
	if (!NILP (display) && INTERACTIVE)
	  redisplay_preserving_echo_area ();
	immediate_quit = 1;
	QUIT;
      }

    /* Wait for it to terminate, unless it already has.  */
    wait_for_termination (pid);

    immediate_quit = 0;

    Fset_buffer (old);
    UNGCPRO;
    unbind_to (speccount, Qnil);

    if (synch_process_death)
      return build_string (synch_process_death);
    return make_number (synch_process_retcode);
  }
}

#endif /* VMS */

#ifndef VMS /* VMS version is in vmsproc.c.  */

/* This is the last thing run in a newly forked inferior
   either synchronous or asynchronous.
   Copy descriptors IN, OUT and ERR as descriptors 0, 1 and 2.
   Initialize inferior's priority, pgrp, connected dir and environment.
   then exec another program based on new_argv.

   This function may change environ for the superior process.
   Therefore, the superior process must save and restore the value
   of environ around the vfork and the call to this function.

   ENV is the environment for the subprocess.

   SET_PGRP is nonzero if we should put the subprocess into a separate
   process group.  

   CURRENT_DIR is an elisp string giving the path of the current
   directory the subprocess should have.  Since we can't really signal
   a decent error from within the child, this should be verified as an
   executable directory by the parent.  */

extern int emacs_priority;
static int relocate_fd (int fd, int min);

void
child_setup (in, out, err, new_argv, env, set_pgrp, current_dir)
     int in, out, err;
     char **new_argv;
     char **env;
     int set_pgrp;
     CONST char *current_dir;
{
  register int pid = getpid ();
  register char *pwd;

  if (emacs_priority != 0)
    nice (- emacs_priority);

  /* Close Emacs's descriptors that this process should not have.  */
  close_process_descs ();

  /* Note that use of alloca is always safe here.  It's obvious for systems
     that do not have true vfork or that have true (stack) alloca.
     If using vfork and C_ALLOCA it is safe because that changes
     the superior's static variables as if the superior had done alloca
     and will be cleaned up in the usual way.  */
  {
    register int i;

    i = strlen (current_dir);
    pwd = (char *) alloca (i + 6);
    memcpy (pwd, "PWD=", 4);
    memcpy (pwd + 4, current_dir, i);
    i += 4;
    if (pwd[i - 1] != '/')
      pwd[i++] = '/';
    pwd[i] = 0;

    /* We can't signal an Elisp error here; we're in a vfork.  Since
       the callers check the current directory before forking, this
       should only return an error if the directory's permissions
       are changed between the check and this chdir, but we should
       at least check.  */
    if (chdir (pwd + 4) < 0)
    {
#if 0 /* >>>> Check this -- perhaps ange-ftp now works? */
      exit (errno);
#endif
      /* Don't report the chdir error, or ange-ftp.el doesn't work. */
      pwd = 0;
    }
    else
    {
      /* Strip trailing "/".  Cretinous *[]&@$#^%@#$% Un*x */
      while (i > 5 && pwd[--i] == '/')
        pwd[i] = 0;
    }
  }

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

    /* new_length + 1 to include terminating 0.  */
    env = new_env = (char **) alloca ((new_length + 1) * sizeof (char *));

    /* Copy the Vprocess_environment strings into new_env.  */
    for (tem = Vprocess_environment;
	 (CONSP (tem)
	  && STRINGP (XCONS (tem)->car));
	 tem = XCONS (tem)->cdr)
    {
      char **ep = env;
      char *string = (char *) XSTRING (XCONS (tem)->car)->data;
      /* See if this string duplicates any string already in the env.
	 If so, don't put it in.
	 When an env var has multiple definitions,
	 we keep the definition that comes first in process-environment.  */
      for (; ep != new_env; ep++)
	{
	  char *p = *ep, *q = string;
	  while (1)
	    {
	      if (*q == 0)
		/* The string is malformed; might as well drop it.  */
		goto duplicate;
	      if (*q != *p)
		break;
	      if (*q == '=')
		goto duplicate;
	      p++, q++;
	    }
	}
      if (pwd && !strncmp ("PWD=", string, 4))
	{
	  *new_env++ = pwd;
	  pwd = 0;
	}
      else
        *new_env++ = string;
    duplicate: ;
    }
    *new_env = 0;
  }

  /* Make sure that in, out, and err are not actually already in
     descriptors zero, one, or two; this could happen if Emacs is
     started with its standard in, our, or error closed, as might
     happen under X.  */
  in = relocate_fd (in, 3);
  out = relocate_fd (out, 3);
  err = relocate_fd (err, 3);

  emacs_close (0);
  emacs_close (1);
  emacs_close (2);

  dup2 (in, 0);
  dup2 (out, 1);
  dup2 (err, 2);
  emacs_close (in);
  emacs_close (out);
  emacs_close (err);

#ifndef IRIX
# ifdef USG
#  ifndef SETPGRP_RELEASES_CTTY
  setpgrp ();			/* No arguments but equivalent in this case */
#  endif
#  else
  setpgrp (pid, pid);
#  endif /* USG */
# endif /* IRIX */
  setpgrp_of_tty (pid);

#ifdef vipc
  something missing here;
#endif /* vipc */

  /* execvp does not accept an environment arg so the only way
     to pass this environment is to set environ.  Our caller
     is responsible for restoring the ambient value of environ.  */
  environ = env;
  execvp (new_argv[0], new_argv);

  {
    char buf[255];
    sprintf (buf, GETTEXT ("Couldn't exec the program %s"), new_argv[0]);
    emacs_write (1, buf, strlen (buf));
  }
  _exit (1);
}

/* Move the file descriptor FD so that its number is not less than MIN.
   If the file descriptor is moved at all, the original is freed.  */
static int
relocate_fd (int fd, int min)
{
  if (fd >= min)
    return fd;
  else
    {
      int new = dup (fd);
      if (new == -1)
	{
	  CONST char *message1 = GETTEXT ("Error while setting up child: ");
	  CONST char *message2 = "\n";
	  emacs_write (2, message1, strlen (message1));
	  emacs_write (2, sys_errlist[errno], strlen (sys_errlist[errno]));
	  emacs_write (2, message2, strlen (message2));
	  _exit (1);
	}
      /* Note that we hold the original FD open while we recurse,
	 to guarantee we'll get a new FD if we need it.  */
      new = relocate_fd (new, min);
      emacs_close (fd);
      return new;
    }
}

static int
getenv_internal (CONST char *var,
		 int varlen,
		 char **value,
		 int *valuelen)
{
  Lisp_Object scan;

  for (scan = Vprocess_environment; CONSP (scan); scan = XCONS (scan)->cdr)
    {
      Lisp_Object entry = XCONS (scan)->car;
      
      if (XTYPE (entry) == Lisp_String
	  && XSTRING (entry)->size > varlen
	  && XSTRING (entry)->data[varlen] == '='
	  && ! memcmp (XSTRING (entry)->data, var, varlen))
	{
	  *value    = (char *) XSTRING (entry)->data + (varlen + 1);
	  *valuelen = XSTRING (entry)->size - (varlen + 1);
	  return 1;
	}
    }

  return 0;
}

DEFUN ("getenv", Fgetenv, Sgetenv, 1, 2, "sEnvironment variable: \np",
  "Return the value of environment variable VAR, as a string.\n\
VAR is a string, the name of the variable.\n\
When invoked interactively, prints the value in the echo area.")
     (var, interactivep)
     Lisp_Object var, interactivep;
{
  char *value;
  int valuelen;
  Lisp_Object v = Qnil;
  CHECK_STRING (var, 0);
  if (getenv_internal ((char *) XSTRING (var)->data,
		       XSTRING (var)->size,
		       &value, &valuelen))
    v = make_string (value, valuelen);
  if (!NILP (interactivep))
    {
      if (NILP (v))
	message (GETTEXT ("%s not defined in environment"),
		 XSTRING (var)->data);
      else
	message ("\"%s\"", value);
    }
  return v;
}

/* A version of getenv that consults process_environment, easily
   callable from C.  */
char *
egetenv (var)
     CONST char *var;
{
  char *value;
  int valuelen;

  if (getenv_internal (var, strlen (var), &value, &valuelen))
    return value;
  else
    return 0;
}

#endif /* not VMS */

void
init_callproc ()
{
  register char * sh;
  Lisp_Object tempdir;

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

  {
    char *data_dir = egetenv ("EMACSDATA");
    
#ifdef PATH_DATA
    if (!data_dir)
      data_dir = PATH_DATA;
#endif
    
    if (data_dir)
      Vdata_directory = Ffile_name_as_directory (build_string (data_dir));
    else
      Vdata_directory = Qnil;
  }

  /* Check the EMACSPATH environment variable, defaulting to the
     PATH_EXEC path from paths.h.  */
  Vexec_path = decode_env_path ("EMACSPATH",
#ifdef PATH_EXEC
                                PATH_EXEC
#else
                                0
#endif
                                );
  if (NILP (Vexec_path))
    Vexec_directory = Qnil;
  else
    Vexec_directory = Ffile_name_as_directory (Fcar (Vexec_path));

  if (initialized)
    Vexec_path = nconc2 (decode_env_path ("PATH", 0),
                         Vexec_path);

  if (!NILP (Vexec_directory))
  {
    tempdir = Fdirectory_file_name (Vexec_directory);
    if (access ((char *) XSTRING (tempdir)->data, 0) < 0)
    {
      fprintf (stderr,
               GETTEXT ("Warning: machine-dependent data dir (%s) does not exist.\n"),
               XSTRING (Vexec_directory)->data);
      sleep (2);
    }
  }

  if (!NILP (Vdata_directory))
  {
    tempdir = Fdirectory_file_name (Vdata_directory);
    if (access ((char *) XSTRING (tempdir)->data, 0) < 0)
    {
      fprintf (stderr,
               GETTEXT ("Warning: machine-independent data dir (%s) does not exist.\n"),
               XSTRING (Vdata_directory)->data);
      sleep (2);
    }
  }

#ifdef VMS
  Vshell_file_name = build_string ("*dcl*");
#else
  sh = (char *) egetenv ("SHELL");
  Vshell_file_name = build_string (sh ? sh : "/bin/sh");
#endif
}

void
set_process_environment ()
{
  register char **envp;

  Vprocess_environment = Qnil;
#ifndef CANNOT_DUMP
  if (initialized)
#endif
    for (envp = environ; *envp; envp++)
      Vprocess_environment = Fcons (build_string (*envp),
				    Vprocess_environment);
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
    "Directory of architecture-dependent files that come with GNU Emacs,\n\
especially executable programs intended for Emacs to invoke.");

  DEFVAR_LISP ("data-directory", &Vdata_directory,
    "Directory of architecture-independent files that come with GNU Emacs,\n\
intended for Emacs to use.");

  DEFVAR_LISP ("process-environment", &Vprocess_environment,
    "List of environment variables for subprocesses to inherit.\n\
Each element should be a string of the form ENVVARNAME=VALUE.\n\
The environment which Emacs inherits is placed in this variable\n\
when Emacs starts.");

#ifndef VMS
  defsubr (&Scall_process);
  defsubr (&Sgetenv);
#endif
}
