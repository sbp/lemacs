/* Fully extensible Emacs, running on Unix, intended for GNU.
   Copyright (C) 1985, 1986, 1987, 1992, 1993, 1994
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
#include "intl.h"

#include <errno.h>
#include <stdarg.h>

#include <stdio.h>

#if defined (I18N2) || defined (I18N3) || defined (I18N4)
#include <locale.h>
#endif

#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>

#ifdef TOOLTALK
#include <tt_c.h>
#endif

#ifdef VMS
#include <ssdef.h>
#endif

#ifdef BSD
#include <sys/ioctl.h>
#endif

#ifdef APOLLO
#ifndef APOLLO_SR10
#include <default_acl.h>
#endif
#endif

#include "commands.h"
#include "process.h"
#include "systty.h"
#include "syssignal.h"
#include "sysdep.h"


#ifndef O_RDWR
#define O_RDWR 2
#endif

extern void memory_warnings ();

/* Command line args from shell, as list of strings */
Lisp_Object Vcommand_line_args;

/* Set nonzero after Emacs has started up the first time.
  Prevents reinitialization of the Lisp world and keymaps
  on subsequent starts.  */
int initialized;

/* Variable whose value is symbol giving operating system type. */
Lisp_Object Vsystem_type;

/* Variable whose value is string giving configuration built for.  */
Lisp_Object Vsystem_configuration;
  
/* Variable holding the name used to invoke emacs, and the full path
   used to get to the actual exec file. */
Lisp_Object Vinvocation_name;
Lisp_Object Vexecution_path;

#if 0 /* RMSmacs */
/* The directory name in which to find subdirs such as lisp and etc.
   nil means get them only from PATH_LOADSEARCH.  */
Lisp_Object Vinstallation_directory;
#endif

/* If non-zero, emacs should not attempt to use an window-specific code,
   but instead should use the virtual terminal under which it was started */
int inhibit_window_system;

/* If nonzero, set Emacs to run at this priority.  This is also used
   in child_setup and sys_suspend to make sure subshells run at normal
   priority. */
int emacs_priority;

/* An address near the bottom of the stack.
   Tells GC how to save a copy of the stack.  */
char *stack_bottom;

#ifdef USG_SHARED_LIBRARIES
/* If nonzero, this is the place to put the end of the writable segment
   at startup.  */

unsigned int bss_end = 0;
#endif

/* Nonzero means running Emacs without interactive terminal.  */

int noninteractive;

/* Value of Lisp variable `noninteractive'.
   Normally same as C variable `noninteractive'
   but nothing terrible happens if user sets this one.  */

int noninteractive1;

extern int always_gc;           /* hack */
extern void syms_of_abbrev (void);
extern void init_alloc (void), syms_of_alloc (void), init_alloc_once (void);
extern void init_buffer (void), syms_of_buffer (void), init_buffer_once (void);
extern void init_insdel (void);
extern void syms_of_bytecode (void);
extern void syms_of_callint (void), init_callproc (void);
extern void syms_of_callproc (void);
extern void syms_of_casefiddle (void);
extern void syms_of_casetab (void), init_casetab_once (void);
extern void syms_of_cmds (void);
extern void init_data (void), syms_of_data (void);
extern void syms_of_dired (void);
extern void init_display (void), syms_of_display (void);
extern void syms_of_doc (void);
extern void init_editfns (void), syms_of_editfns (void);
extern void syms_of_elhash (void);
extern void syms_of_emacs (void);
extern void init_environ (void), syms_of_environ (void);
extern void init_eval (void), syms_of_eval (void), init_eval_once (void);
extern void syms_of_event_alloc (void);
extern void init_event_stream (void), syms_of_event_stream (void);
extern void syms_of_events (void);
extern void syms_of_extents (void);
extern void syms_of_faces (void);
extern void syms_of_fileio (void);
extern void init_filelock (void), syms_of_filelock (void);
extern void init_floatfns (void), syms_of_floatfns (void);
extern void syms_of_fns (void);
extern void syms_of_font_lock (void);
extern void syms_of_intl (void);
extern void syms_of_indent (void);
extern void init_keyboard (void), syms_of_keyboard (void);
extern void syms_of_keymap (void);
extern void init_lread (void), syms_of_lread (void);
extern void syms_of_symbols (void), init_symbols (void);
extern void init_macros (void), syms_of_macros (void);
extern void syms_of_marker (void);
extern void syms_of_menubar (void);
extern void syms_of_minibuf (void), init_minibuf_once (void);
extern void syms_of_mocklisp (void);
extern void keys_of_keymap (void);
extern void syms_of_print (void);
extern void init_process (void), syms_of_process (void);
extern void syms_of_screen (void);
extern void syms_of_search (void);
#ifdef POSIX_SIGNALS
extern void init_signals (void);
#endif
extern void init_sunpro (void), syms_of_sunpro (void);
extern void syms_of_tooltalk (void);
extern void syms_of_syntax (void), init_syntax_once (void);
extern void init_sys_modes (void);
extern void syms_of_undo (void);
extern void syms_of_window (void), init_window_once (void);
extern void init_xdisp (void), syms_of_xdisp (void);
extern void syms_of_xfns (void), syms_of_xobjs (void);
extern void syms_of_xselect (void);
#ifdef EPOCH
extern void syms_of_epoch (void);
#endif
#ifdef EMACS_BTL
extern void syms_of_btl (void);
#endif
#ifdef ENERGIZE
extern void syms_of_editorside (void);
#endif
#ifdef DRAGNDROP
extern void syms_of_opaque (void);
extern void init_xtfunc (void), syms_of_xtfunc (void);
extern void syms_of_dragndrop (void);
#endif

Lisp_Object Qkill_emacs_hook;
Lisp_Object Qx;                 /* 'x */
Lisp_Object Qsave_buffers_kill_emacs;


/* Signal code for the fatal signal that was received */
static int fatal_error_code;

/* Nonzero if handling a fatal error already */
static int fatal_error_in_progress;

static void shut_down_emacs (int sig, int no_x, Lisp_Object stuff);

/* Handle bus errors, illegal instruction, etc. */
SIGTYPE
fatal_error_signal (sig)
     int sig;
{
  fatal_error_code = sig;
  signal (sig, SIG_DFL);

  /* If fatal error occurs in code below, avoid infinite recursion.  */
  if (! fatal_error_in_progress)
    {
      fatal_error_in_progress = 1;

      shut_down_emacs (sig, 0, Qnil);
    }
#ifdef VMS
  LIB$STOP (SS$_ABORT);
#else
  /* Signal the same code; this time it will really be fatal.
     Remember that since we're in a signal handler, the signal we're
     going to send is probably blocked, so we have to unblock it if we
     want to really receive it.  */
#ifndef MSDOS
  sigunblock (sigmask (fatal_error_code));
#endif /* !MSDOS */
  kill (getpid (), fatal_error_code);
#endif /* not VMS */
  SIGRETURN;
}

/* dump an error message; called like printf */

DOESNT_RETURN
error (CONST char *fmt, ...)
{
  char buf[200];
  va_list args;
  va_start (args, fmt);

  vsprintf (buf, fmt, args);

  va_end (args);

  while (1)
    Fsignal (Qerror, list1 (build_string (buf)));
}

DOESNT_RETURN
fatal (CONST char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);

  fprintf (stderr, "emacs: ");
  vfprintf (stderr, fmt, args);
  fprintf (stderr, "\n");

  va_end (args);
  fflush (stderr);
  exit (1);
}


#ifdef SIGDANGER

/* Handler for SIGDANGER.  */
SIGTYPE
memory_warning_signal (int sig)
{
  signal (sig, memory_warning_signal);

  malloc_warning (GETTEXT ("Operating system warns that virtual memory is running low.\n"));
}
#endif

/* Code for dealing with Lisp access to the Unix command line */

static void
init_cmdargs (argc, argv, skip_args)
     int argc;
     char **argv;
     int skip_args;
{
  register int i;

  Vcommand_line_args = Qnil;

  for (i = argc - 1; i >= 0; i--)
    {
      if (i == 0 || i > skip_args)
	Vcommand_line_args
	  = Fcons (build_string (argv[i]), Vcommand_line_args);
    }
}

DEFUN ("invocation-name", Finvocation_name, Sinvocation_name, 0, 0, 0,
  "Return the program name that was used to run Emacs.\n\
Any directory names are omitted.")
  ()
{
  return Fcopy_sequence (Vinvocation_name);
}


#ifdef RUN_TIME_REMAP
extern int run_time_remap (char *);
#endif

static DOESNT_RETURN
main_1 (int argc, char **argv, char **envp)
{
  char stack_bottom_variable;
  int skip_args = 0;
  Lisp_Object load_me;

#ifdef FREE_CHECKING
  init_free_hook ();
#endif

  /* Map in shared memory, if we are using that.  */
#ifdef HAVE_SHM
  if (argc > 1 && !strcmp (argv[1], "-nl"))
    {
      map_in_data (0);
      /* The shared memory was just restored, which clobbered this.  */
      skip_args = 1;
    }
  else
    {
      map_in_data (1);
      /* The shared memory was just restored, which clobbered this.  */
      skip_args = 0;
    }
#endif

#ifdef VMS
  /* If -map specified, map the data file in */
  if (argc > 2 && ! strcmp (argv[1], "-map"))
    {
      skip_args = 2;
      mapin_data (argv[2]);
    }

#ifdef LINK_CRTL_SHARE
#ifdef SHAREABLE_LIB_BUG
  /* Bletcherous shared libraries! */
  if (!stdin)
    stdin = fdopen (0, "r");
  if (!stdout)
    stdout = fdopen (1, "w");
  if (!stderr)
    stderr = fdopen (2, "w");
  if (!environ)
    environ = envp;
#endif /* SHAREABLE_LIB_BUG */
#endif /* LINK_CRTL_SHARE */
#endif /* VMS */

  /* Record (approximately) where the stack begins.  */
  stack_bottom = &stack_bottom_variable;

#ifdef RUN_TIME_REMAP
  if (initialized)
    run_time_remap (argv[0]);
#endif

#ifdef USG_SHARED_LIBRARIES
  if (bss_end)
    brk ((void *) bss_end);
#endif

  clearerr (stdin);

#ifdef BSD_PGRPS
  if (initialized)
  {
    inherited_pgroup = EMACS_GETPGRP (0);
#  if	defined(__osf__) /* lemacs: <grunwald@foobar.cs.colorado.edu> */
    setpgrp (0, getpid ());
#  else
    setpgid (0, getpid ());
#  endif /* __osf__ */
  }
#else
#if defined (USG5) && defined (INTERRUPT_INPUT)
  setpgrp ();
#endif
#endif /* BSD_PGRPS */

#ifdef APOLLO
#ifndef APOLLO_SR10
  /* If USE_DOMAIN_ACLS environment variable exists,
     use ACLs rather than UNIX modes. */
  if (egetenv ("USE_DOMAIN_ACLS"))
    default_acl (USE_DEFACL);
#endif
#endif /* APOLLO */

#ifndef SYSTEM_MALLOC
  if (! initialized)
    {
      /* Arrange to get warning messages as memory fills up.  */
      memory_warnings (0, malloc_warning);

      /* Arrange to disable interrupt input while malloc and friends are
	 running.  */
      uninterrupt_malloc ();
    }
#endif	/* not SYSTEM_MALLOC */

#ifdef MSDOS
  /* We do all file input/output as binary files.  When we need to translate
     newlines, we do that manually.  */
  _fmode = O_BINARY;
  (stdin)->_flag &= ~_IOTEXT;
  (stdout)->_flag &= ~_IOTEXT;
  (stderr)->_flag &= ~_IOTEXT;
#endif /* MSDOS */

#ifdef PRIO_PROCESS
  if (emacs_priority != 0)
    nice (emacs_priority);
  setuid (getuid ());
#endif /* PRIO_PROCESS */

#ifdef BSD_PGRPS
  /* lemacs: BSD -> BSD_PGRPS by <grunwald@foobar.cs.colorado.edu> */
  /* interrupt_input has trouble if we aren't in a separate process group.  */
#  if	defined(__osf__) /* lemacs: <grunwald@foobar.cs.colorado.edu> */
  setpgrp (getpid (), getpid ());
#  else
  setpgid (getpid (), getpid ());
#  endif /* __osf__ */
#endif

  inhibit_window_system = 0;

  /* Handle the -t switch, which specifies filename to use as terminal */
  if (skip_args + 2 < argc && !strcmp (argv[skip_args + 1], "-t"))
    {
      int result;

      skip_args += 2;
      emacs_close (0);
      emacs_close (1);
      result = emacs_open (argv[skip_args], O_RDWR, 2 );
      if (result < 0)
	{
	  fatal ("%s: %s", argv[skip_args], strerror (errno));
	}
      dup (0);
      if (! isatty (0))
	fatal ("%s: not a tty", argv[skip_args]);

      fprintf (stderr, "Using %s", ttyname (0));
#if 0
      fprintf (stderr, "Using %s", argv[skip_args]);
#endif
#ifdef HAVE_X_WINDOWS
      inhibit_window_system = 1;	/* -t => -nw */
#endif
    }

  if (skip_args + 1 < argc
      && (!strcmp (argv[skip_args + 1], "-nw")))
    {
      skip_args += 1;
      inhibit_window_system = 1;
    }

  /* Handle the -batch switch, which means don't do interactive display.  */
  noninteractive = 0;
  if (skip_args + 1 < argc &&
      (!strcmp (argv[skip_args + 1], "-batch") ||
       !strcmp (argv[skip_args + 1], "--batch")))
    {
      skip_args += 1;
      noninteractive = 1;
    }

  /* Partially handle the -version and -help switches: they imply -batch,
     but are not removed from the list.
   */
  if (skip_args + 1 < argc &&
      (!strcmp (argv[skip_args + 1], "-version") ||
       !strcmp (argv[skip_args + 1], "--version") ||
       !strcmp (argv[skip_args + 1], "-help") ||
       !strcmp (argv[skip_args + 1], "--help")))
    noninteractive = 1;

#ifdef POSIX_SIGNALS
  init_signals ();
#endif

  if (
#ifndef CANNOT_DUMP
      ! noninteractive || initialized
#else
      1
#endif
      )
    {
      /* Don't catch these signals in batch mode if not initialized.
	 On some machines, this sets static data that would make
	 signal fail to work right when the dumped Emacs is run.  */
      signal (SIGHUP, fatal_error_signal);
      signal (SIGQUIT, fatal_error_signal);
      signal (SIGILL, fatal_error_signal);
      signal (SIGTRAP, fatal_error_signal);
#ifdef SIGIOT
      /* This is missing on some systems - OS/2, for example.  */
      signal (SIGIOT, fatal_error_signal);
#endif
#ifdef SIGEMT
      signal (SIGEMT, fatal_error_signal);
#endif
      signal (SIGFPE, fatal_error_signal);
#ifdef SIGBUS
      signal (SIGBUS, fatal_error_signal);
#endif
      signal (SIGSEGV, fatal_error_signal);
#ifdef SIGSYS
      signal (SIGSYS, fatal_error_signal);
#endif
      signal (SIGTERM, fatal_error_signal);
#ifdef SIGXCPU
      signal (SIGXCPU, fatal_error_signal);
#endif
#ifdef SIGXFSZ
      signal (SIGXFSZ, fatal_error_signal);
#endif /* SIGXFSZ */

#ifdef SIGDANGER
      /* This just means available memory is getting low.  */
      signal (SIGDANGER, memory_warning_signal);
#endif

#ifdef AIX
/* 20 is SIGCHLD, 21 is SIGTTIN, 22 is SIGTTOU.  */
      signal (SIGXCPU, fatal_error_signal);
#ifndef _I386
      signal (SIGIOINT, fatal_error_signal);
#endif
      signal (SIGGRANT, fatal_error_signal);
      signal (SIGRETRACT, fatal_error_signal);
      signal (SIGSOUND, fatal_error_signal);
      signal (SIGMSG, fatal_error_signal);
#endif /* AIX */
    }

  noninteractive1 = noninteractive;

/* Perform basic initializations (not merely interning symbols) */

  if (!initialized)
    {
      init_alloc_once ();
      init_symbols ();
      init_eval_once ();
      init_casetab_once ();
      init_syntax_once ();	/* Create standard syntax table. */
      init_buffer_once ();	/* Create buffer table and some buffers.
                                   Must be done after standard_syntax_table
                                   is created. */
      init_minibuf_once ();	/* Create list of minibuffers.
				   Must precede init_window_once */
      init_window_once ();	/* Init the window system */
    }

  init_alloc ();
  init_eval ();

  if (always_gc)                /* purification debugging hack */
    garbage_collect_1 ();

  init_callproc ();	/* Must be called before egetenv(). */
  init_data ();
  init_lread ();

#ifdef MSDOS
  /* Call early 'cause init_environment needs it.  */
  init_dosfns ();
  /* Set defaults for several environment variables.  */
  if (initialized) init_environment (argc, argv, skip_args);
#endif

#if defined (I18N2) || defined (I18N3) || defined (I18N4)
  setlocale (LC_ALL, "");
#endif

#ifdef I18N3
  textdomain ("emacs");
#endif

  init_cmdargs (argc, argv, skip_args);	/* Create list Vcommand_line_args */
  init_buffer ();	/* Init default directory of main buffer */
  init_insdel ();
  if (!noninteractive)
    {
#ifdef VMS
      init_vms_input ();/* init_display calls get_frame_size, that needs this */
#endif /* VMS */
      /* Determine terminal type.  init_sys_modes uses results */
      init_display ();
    }
  init_keyboard ();	/* This too must precede init_sys_modes */
#ifdef VMS
  init_vmsproc ();	/* And this too. */
  init_vmsfns ();
#endif /* VMS */
  init_sys_modes ();	/* Init system terminal modes (RAW or CBREAK, etc.) */
  init_xdisp ();
  init_macros ();
  init_editfns ();
#ifdef LISP_FLOAT_TYPE
  init_floatfns ();
#endif
  init_process ();
#ifdef CLASH_DETECTION
  init_filelock ();
#endif /* CLASH_DETECTION */
#ifdef DRAGNDROP
  init_xtfunc ();
#endif
#ifdef SUNPRO
  init_sunpro ();
#endif

/* Intern the names of all standard functions and variables; define standard keys */

  if (!initialized)
    {
      /* The basic levels of Lisp must come first */
      /* And data must come first of all
	 for the sake of symbols like error-message */
      syms_of_data ();
      syms_of_alloc ();
      syms_of_symbols ();
      syms_of_lread ();
      syms_of_print ();
      syms_of_eval ();
      syms_of_fns ();
      syms_of_floatfns ();
      syms_of_elhash (); /* This has to be before any keymaps are made */
      syms_of_abbrev ();
      syms_of_buffer ();
      syms_of_bytecode ();
      syms_of_callint ();
      syms_of_casefiddle ();
      syms_of_casetab ();
      syms_of_callproc ();
      syms_of_cmds ();
#ifndef NO_DIR_LIBRARY
      syms_of_dired ();
#endif /* not NO_DIR_LIBRARY */
      syms_of_display ();
      syms_of_doc ();
      syms_of_editfns ();
      syms_of_emacs ();
      syms_of_fileio ();
#ifdef CLASH_DETECTION
      syms_of_filelock ();
#endif /* CLASH_DETECTION */
      syms_of_intl ();
      syms_of_indent ();
      syms_of_keyboard ();
      syms_of_keymap ();
      syms_of_macros ();
      syms_of_marker ();
      syms_of_minibuf ();
#ifdef MOCKLISP_SUPPORT
      syms_of_mocklisp ();
#endif
#ifdef subprocesses
      syms_of_process ();
#endif /* subprocesses */
      syms_of_search ();
#ifdef MULTI_SCREEN
      syms_of_screen ();
#endif
      syms_of_extents ();
      syms_of_syntax ();
      syms_of_undo ();
      syms_of_window ();
      syms_of_xdisp ();
#ifdef HAVE_X_WINDOWS
      syms_of_xfns ();
      syms_of_xobjs ();
      syms_of_xselect ();
#ifdef EPOCH
      syms_of_epoch ();
#endif
      syms_of_menubar ();
#endif /* HAVE_X_WINDOWS */
      syms_of_faces ();
      syms_of_events ();
      syms_of_event_alloc ();
      syms_of_event_stream ();
      syms_of_font_lock ();

#ifdef SYMS_SYSTEM
      SYMS_SYSTEM;
#endif

#ifdef SYMS_MACHINE
      SYMS_MACHINE;
#endif

#ifdef EMACS_BTL
      syms_of_btl (); 
#endif

#ifdef ENERGIZE
      syms_of_editorside ();
#endif

#ifdef FREE_CHECKING
      syms_of_free_hook();
#endif

#ifdef TOOLTALK
      syms_of_tooltalk ();
#endif

#ifdef SUNPRO
      syms_of_sunpro ();
#endif

#ifdef DRAGNDROP
      syms_of_opaque ();
      syms_of_xtfun ();
      syms_of_dragndrop ();
#endif

      keys_of_keymap ();
    }

  /* try to get the actually pathname of the exec file we are running */
  {
    Vinvocation_name = Fcar (Vcommand_line_args);
    Vexecution_path = Vinvocation_name;

    if (!NILP (Ffile_name_directory (Vinvocation_name)))
      /* invocation-name includes a directory component -- presumably it
         is relative to cwd, not $PATH */
      Vexecution_path = Fexpand_file_name (Vinvocation_name, Qnil);
    else
      locate_file (decode_env_path ("PATH", 0),
		   Vinvocation_name, EXEC_SUFFIXES,
		   &Vexecution_path, X_OK);

    if (NILP (Vexecution_path))
      Vexecution_path = Vinvocation_name;

    Vinvocation_name = Ffile_name_nondirectory (Vexecution_path);
  }

#if defined (sun) || defined (LOCALTIME_CACHE)
  /* sun's localtime() has a bug.  it caches the value of the time
     zone rather than looking it up every time.  Since localtime() is
     called to bolt the undumping time into the undumped emacs, this
     results in localtime() ignoring the TZ environment variable.
     This flushes the new TZ value into localtime(). */
  tzset();
#endif /* sun  || LOCALTIME_CACHE */

  load_me = Qnil;
  if (!initialized)
    {
      /* Handle -l loadup-and-dump, args passed by Makefile. */
      if (argc > 2 + skip_args && !strcmp (argv[1 + skip_args], "-l"))
        load_me = build_string (argv[2 + skip_args]);
#ifdef CANNOT_DUMP
      /* Unless next switch is -nl, load "loadup.el" first thing.  */
      if (!(argc > 1 + skip_args && !strcmp (argv[1 + skip_args], "-nl")))
	load_me = build_string ("loadup.el");
#endif /* CANNOT_DUMP */
    }

  initialized = 1;

  /* This never returns.  */
  initial_command_loop (load_me);
  /* NOTREACHED */
}

#define RUNNABLE_TEMACS

#ifdef RUNNABLE_TEMACS

#include <setjmp.h>
static jmp_buf run_temacs_catch;

static int run_temacs_argc;
static char **run_temacs_argv;
static char *run_temacs_args;
static int run_temacs_argv_size;
static int run_temacs_args_size;

extern int gc_in_progress;
extern int waiting_for_input;

#define DO_REALLOC(basevar, sizevar, needed_size, type) \
do {\
  while ((sizevar) < (needed_size)) {\
    int newsize = 2*(sizevar);\
    if (newsize < 32)\
      newsize = 32;\
    (basevar) = (type *) xrealloc (basevar, (newsize)*sizeof(type));\
    (sizevar) = newsize;\
  }\
} while (0)

DEFUN ("run-emacs-from-temacs",
       Frun_emacs_from_temacs, Srun_emacs_from_temacs, 0, MANY, 0,
  "Do not call this.  It will reinitialize your Emacs.  You'll be sorry.")
/* If this function is called from startup.el, it will be possible to run
   temacs as an editor, instead of having to dump an emacs and then run that
   (when debugging emacs itself, this can be much faster.)
   This will \"restart\" emacs with the specified command-line arguments.
 */
    (nargs, args)
    int nargs;
    Lisp_Object *args;
{
  int ac;
  int namesize;
  int total_len;

  if (gc_in_progress) abort ();

  if (run_temacs_argc < 0)
    error ("I've lost my temacs-hood.");

  namesize = string_length (XSTRING (Vexecution_path)) + 1;

  for (ac = 0, total_len = namesize; ac < nargs; ac++)
    {
      int s;
      CHECK_STRING (args[ac], ac);
      s = string_length (XSTRING (args[ac])) + 1;
      total_len += s;
    }
  DO_REALLOC (run_temacs_args, run_temacs_args_size, total_len, char);
  DO_REALLOC (run_temacs_argv, run_temacs_argv_size, nargs+1, char *);

  memcpy (run_temacs_args, (char *) XSTRING (Vexecution_path)->data,
	  namesize);
  run_temacs_argv [0] = run_temacs_args;
  for (ac = 0; ac < nargs; ac++)
    {
      int s;
      s = string_length (XSTRING (args[ac])) + 1;
      memcpy (run_temacs_args + namesize, XSTRING (args[ac])->data, s);
      run_temacs_argv [ac + 1] = run_temacs_args + namesize;
      namesize += s;
    }
  run_temacs_argv [nargs + 1] = 0;
  unbind_to (0, Qnil); /* this closes loadup.el */
  waiting_for_input = 0;
  purify_flag = 0;
  run_temacs_argc = nargs + 1;
  report_pure_usage (1, 0);
  _longjmp (run_temacs_catch, 1);
  return Qnil; /* not reached; warning suppression */
}

#endif /* RUNNABLE_TEMACS */

/* ARGSUSED */
DOESNT_RETURN
main (argc, argv, envp)
     int argc;
     char **argv;
     char **envp;
{
  if (sizeof (Lisp_Object) != sizeof (void *))
    abort (); /* Lisp_Object must fit in a word; check VALBITS and GCTYPEBITS */
#ifdef RUNNABLE_TEMACS
  if (!initialized)
  {
    run_temacs_argc = 0;
    if (! _setjmp (run_temacs_catch))
      main_1 (argc, argv, envp);
    /* run-emacs-from-temacs called */
    argc = run_temacs_argc;
    run_temacs_argc = 0;
    argv = run_temacs_argv;
    envp = environ;
  }
  run_temacs_argc = -1;
#endif /* RUNNABLE_TEMACS */
  main_1 (argc, argv, envp);
}


#ifdef USE_ASSERTIONS
/* This highly dubious kludge redefines `abort' to call `assert (0)' instead,
   with the (single) benefit of printing the line number when emacs crashes.

   This cannot be redefined to bring up a dialog box, or ignore the error, 
   or anything other than dump a core file and exit, because by the time this
   has gotten called, emacs is in a dangerously unknown state.  If we abort(),
   then we will (try our best to) autosave all modified buffers, and then exit.
   If we do anything here, we may get a SEGV instead of an abort, in which case
   the autosave won't happen and the user can lose data.
 */

void assert_failed (char *file, int line, char *expr)
{
  fprintf (stderr, "Fatal error: assertion failed, file %s, line %d\n",
	   file, line);
#undef abort	/* avoid infinite #define loop... */
  abort ();
}
#endif /* USE_ASSERTIONS */


DEFUN ("kill-emacs", Fkill_emacs, Skill_emacs, 0, 1, "P",
  "Exit the Emacs job and kill it.  Ask for confirmation, without argument.\n\
If ARG is an integer, return ARG as the exit program code.\n\
If ARG is a  string, stuff it as keyboard input.\n\n\
The value of `kill-emacs-hook', if not void,\n\
is a list of functions (of no args),\n\
all of which are called before Emacs is actually killed.")
  (arg)
     Lisp_Object arg;
{
  struct gcpro gcpro1;

  GCPRO1 (arg);

  if (feof (stdin))
    arg = Qt;

  if (!NILP (Vrun_hooks) && !noninteractive)
    call1 (Vrun_hooks, Qkill_emacs_hook);

  UNGCPRO;

/* Is it really necessary to do this deassign
   when we are going to exit anyway?  */
/* #ifdef VMS
  stop_vms_input ();
 #endif  */

  shut_down_emacs (0, 0, ((STRINGP (arg)) ? arg : Qnil));

  exit ((FIXNUMP (arg)) ? XINT (arg)
#ifdef VMS
	: 1
#else
	: 0
#endif
	);
  /* NOTREACHED */
  return Qnil; /* I'm sick of the compiler warning */
}

/* Perform an orderly shutdown of Emacs.  Autosave any modified
   buffers, kill any child processes, clean up the terminal modes (if
   we're in the foreground), and other stuff like that.  Don't perform
   any redisplay; this may be called when Emacs is shutting down in
   the background, or after its X connection has died.

   If SIG is a signal number, print a message for it.

   This is called by fatal signal handlers, X protocol error handlers,
   and Fkill_emacs.  */
static void
shut_down_emacs (int sig, int no_x, Lisp_Object stuff)
{
  /* Prevent running of hooks from now on.  */
  Vrun_hooks = Qnil;

  /* If we are controlling the terminal, reset terminal modes */
#ifdef EMACS_HAVE_TTY_PGRP
  {
    int pgrp = EMACS_GETPGRP (0);
    int tpgrp;

    if (EMACS_GET_TTY_PGRP (0, &tpgrp) != -1
	&& tpgrp == pgrp)
    {
	fflush (stdout);
	reset_sys_modes ();
	if (sig && sig != SIGTERM)
	  fprintf (stderr, "Fatal error (%d).\n", sig);
      }
  }
#else
  fflush (stdout);
  reset_sys_modes ();
#endif

  stuff_buffered_input (stuff);

  Fdo_auto_save (Qt, Qnil);	/* do this before anything hazardous */
  kill_buffer_processes (Qnil);

#ifdef CLASH_DETECTION
  unlock_all_files ();
#endif

#ifdef TOOLTALK
  tt_session_quit (tt_default_session());
  tt_close();
#endif

#ifdef VMS
  kill_vms_processes ();
#endif

#ifdef HAVE_X_WINDOWS
  if (!noninteractive && EQ (Vwindow_system, Qx) && ! no_x)
    Fx_close_current_connection ();
#endif /* HAVE_X_WINDOWS */

#ifdef SIGIO
  /* There is a tendency for a SIGIO signal to arrive within exit,
     and cause a SIGHUP because the input descriptor is already closed.  */
  unrequest_sigio ();
  signal (SIGIO, SIG_IGN);
#endif
}


#ifndef CANNOT_DUMP
/* Nothing like this can be implemented on an Apollo.
   What a loss!  */

#ifdef HAVE_SHM

DEFUN ("dump-emacs-data", Fdump_emacs_data, Sdump_emacs_data, 1, 1, 0,
  "Dump current state of Emacs into data file FILENAME.\n\
This function exists on systems that use HAVE_SHM.")
  (intoname)
     Lisp_Object intoname;
{
  extern int my_edata;
  Lisp_Object tem;

  CHECK_STRING (intoname, 0);
  intoname = Fexpand_file_name (intoname, Qnil);

  tem = Vpurify_flag;
  Vpurify_flag = Qnil;

  fflush (stderr);
  fflush (stdout);

  disksave_object_finalisation ();
  release_breathing_space ();

  /* Tell malloc where start of impure now is */
  /* Also arrange for warnings when nearly out of space.  */
#ifndef SYSTEM_MALLOC
  memory_warnings (&my_edata, malloc_warning);
#endif
  map_out_data (XSTRING (intoname)->data);

  Vpurify_flag = tem;

  return Qnil;
}

#else /* not HAVE_SHM */

DEFUN ("dump-emacs", Fdump_emacs, Sdump_emacs, 2, 2, 0,
  "Dump current state of Emacs into executable file FILENAME.\n\
Take symbols from SYMFILE (presumably the file you executed to run Emacs).\n\
This is used in the file `loadup.el' when building Emacs.\n\
\n\
Remember to set `command-line-processed' to nil before dumping\n\
if you want the dumped Emacs to process its command line\n\
and announce itself normally when it is run.")
  (intoname, symname)
     Lisp_Object intoname, symname;
{
  unsigned char *a_name = 0;
  extern char my_edata; /* this is char in lastfile.c, not int! */
  struct gcpro gcpro1, gcpro2;
  int tem;

  GCPRO2 (intoname, symname);

#ifdef FREE_CHECKING
  Freally_free (Qnil);

  /* When we're dumping, we can't use the debugging free() */

  disable_free_hook ();
#endif

  CHECK_STRING (intoname, 0);
  intoname = Fexpand_file_name (intoname, Qnil);
  if (!NILP (symname))
    {
      CHECK_STRING (symname, 0);
      if (string_length (XSTRING (symname)) > 0)
	{
	  symname = Fexpand_file_name (symname, Qnil);
	  a_name = XSTRING (symname)->data;
	}
    }

  tem = purify_flag;
  purify_flag = 0;

  report_pure_usage (1, 1);

  fflush (stderr);
  fflush (stdout);

  disksave_object_finalisation ();
  release_breathing_space ();

  UNGCPRO;

#ifdef VMS
  mapout_data (XSTRING (intoname)->data);
#else
  /* Tell malloc where start of impure now is */
  /* Also arrange for warnings when nearly out of space.  */
#ifndef SYSTEM_MALLOC
  memory_warnings (&my_edata, malloc_warning);
#endif
  unexec ((char *) XSTRING (intoname)->data,
	  (char *) a_name,
	  (unsigned int) &my_edata,
	  0, 0);
#endif /* not VMS */

  purify_flag = tem;

  return Qnil;
}

#endif /* not HAVE_SHM */

#endif /* not CANNOT_DUMP */

#ifndef SEPCHAR
#define SEPCHAR ':'
#endif

Lisp_Object
decode_env_path (CONST char *evarname, CONST char *defalt)
{
  register CONST char *path = 0;
  register CONST char *p;
  Lisp_Object lpath = Qnil;

  if (evarname)
    path = (char *) egetenv (evarname);
  if (!path)
    path = defalt;
  if (!path)
    return (Qnil);

  while (1)
    {
      p = strchr (path, SEPCHAR);
      if (!p) p = path + strlen (path);
      lpath = Fcons (((p != path) ? make_string (path, p - path) : Qnil),
		     lpath);
      if (*p)
	path = p + 1;
      else
	break;
    }
  return Fnreverse (lpath);
}

DEFUN ("noninteractive", Fnoninteractive, Snoninteractive, 0, 0, 0,
  "Non-nil return value means Emacs is running without interactive terminal.")
  ()
{
  return ((noninteractive) ? Qt : Qnil);
}

void
syms_of_emacs ()
{
#ifndef CANNOT_DUMP
#ifdef HAVE_SHM
  defsubr (&Sdump_emacs_data);
#else
  defsubr (&Sdump_emacs);
#endif
#endif /* !CANNOT_DUMP */

#ifdef RUNNABLE_TEMACS
  defsubr (&Srun_emacs_from_temacs);
#endif

  defsubr (&Sinvocation_name);
  defsubr (&Skill_emacs);
  defsubr (&Snoninteractive);

  defsymbol (&Qkill_emacs_hook, "kill-emacs-hook");
  defsymbol (&Qsave_buffers_kill_emacs, "save-buffers-kill-emacs");
  defsymbol (&Qx, "x");

  DEFVAR_LISP ("command-line-args", &Vcommand_line_args,
    "Args passed by shell to Emacs, as a list of strings.");

  DEFVAR_LISP ("invocation-name", &Vinvocation_name,
    "Name of file used to invoke editing session.\n\
This is the same as `(file-name-nondirectory execution-path)'.");

  /* Poor name */
  DEFVAR_LISP ("execution-path", &Vexecution_path,
    "Pathname of executable emacs program now running.");

  DEFVAR_LISP ("system-type", &Vsystem_type,
    "Value is symbol indicating type of operating system you are using.");
  Vsystem_type = intern (SYSTEM_TYPE);

#ifndef CONFIGURATION
# define CONFIGURATION "UNKNOWN"
#endif
  DEFVAR_LISP ("system-configuration", &Vsystem_configuration,
    "Value is string indicating configuration Emacs was built for.");
  Vsystem_configuration = Fpurecopy (build_string (CONFIGURATION));

  DEFVAR_BOOL ("noninteractive", &noninteractive1,
    "Non-nil means Emacs is running without interactive terminal.");

  DEFVAR_INT ("emacs-priority", &emacs_priority,
    "Priority for Emacs to run at.\n\
This value is effective only if set before Emacs is dumped,\n\
and only if the Emacs executable is installed with setuid to permit\n\
it to change priority.  (Emacs sets its uid back to the real uid.)");
  emacs_priority = 0;
}
