/* Fully extensible Emacs, running on Unix, intended for GNU.
   Copyright (C) 1992, 1993 Free Software Foundation, Inc.

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

#include <signal.h>
#include <sys/signal.h>
#include <errno.h>

#include <stdio.h>

#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>

#ifdef VMS
#include <ssdef.h>
#endif

#ifdef USG5
#include <fcntl.h>
#endif

#ifdef BSD
#include <sys/ioctl.h>
#endif

#ifdef APOLLO
#ifndef APOLLO_SR10
#include <default_acl.h>
#endif
#endif

#ifdef RUNNABLE_TEMACS
#include <setjmp.h>
#endif

#include "commands.h"

#ifndef O_RDWR
#define O_RDWR 2
#endif

#define PRIO_PROCESS 0

/* Command line args from shell, as list of strings */
Lisp_Object Vcommand_line_args;

/* Set nonzero after Emacs has started up the first time.
  Prevents reinitialization of the Lisp world and keymaps
  on subsequent starts.  */
int initialized;

/* Variable whose value is symbol giving operating system type */
Lisp_Object Vsystem_type;

extern Lisp_Object Vtop_level;

/* Variable holding the name used to invoke emacs, and the full path
   used to get to the actual exec file. */
Lisp_Object Vinvocation_name, Vexecution_path;

/* If non-zero, emacs should not attempt to use an window-specific code,
   but instead should use the virtual terminal under which it was started */
int inhibit_window_system;

/* An address near the bottom of the stack.
   Tells GC how to save a copy of the stack.  */
char *stack_bottom;

#ifdef HAVE_X_WINDOWS
extern Lisp_Object Vwindow_system;
#endif /* HAVE_X_WINDOWS */

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

/* Signal code for the fatal signal that was received */
static int fatal_error_code;

/* Nonzero if handling a fatal error already */
static int fatal_error_in_progress;

extern void reset_sys_modes (void);

extern void report_pure_usage (void);

#ifdef subprocesses
extern void kill_buffer_processes (Lisp_Object buffer);
#endif

/* Handle bus errors, illegal instruction, etc. */
SIGTYPE
fatal_error_signal (sig)
     int sig;
{
#ifdef BSD
  int tpgrp;
#endif /* BSD */

  fatal_error_code = sig;
  signal (sig, SIG_DFL);

  /* If fatal error occurs in code below, avoid infinite recursion.  */
  if (fatal_error_in_progress)
    kill (getpid (), fatal_error_code);

  fatal_error_in_progress = 1;

  /* If we are controlling the terminal, reset terminal modes */
#ifdef BSD
  if (ioctl(0, TIOCGPGRP, &tpgrp) == 0
      && tpgrp == getpgrp (0))
#endif /* BSD */
    {
      reset_sys_modes ();
      if (sig != SIGTERM)
	fprintf (stderr, "Fatal error (%d).", sig);
    }

  /* Clean up */
#ifdef subprocesses
  kill_buffer_processes (Qnil);
#endif
  Fdo_auto_save (Qt);

#ifdef CLASH_DETECTION
  unlock_all_files ();
#endif /* CLASH_DETECTION */

#ifdef VMS
  kill_vms_processes ();
  LIB$STOP (SS$_ABORT);
#else
  /* Signal the same code; this time it will really be fatal.  */
  kill (getpid (), fatal_error_code);
#endif /* not VMS */
}

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

/* DOT_FOUND_IN_SEARCH becomes non-zero when find_user_command ()
   encounters a `.' as the directory pathname while scanning the
   list of possible pathnames; i.e., if `.' comes before the directory
   containing the file of interest. */
static int dot_found_in_search;

#define savestring(x) ((char *)(strcpy ((char *)xmalloc (1 + strlen (x)), (x))))

#define u_mode_bits(x) (((x) & 0000700) >> 6)
#define g_mode_bits(x) (((x) & 0000070) >> 3)
#define o_mode_bits(x) (((x) & 0000007) >> 0)
#define X_BIT(x) (x & 1)

/* Non-zero if the last call to executable_file () found
   the file, but stated that it wasn't executable. */
static int file_exists_p;

static int
group_member (gid)
     int gid;
{
  return ((gid == getgid ()) || (gid == getegid ()));
}

/* Return non-zero if FILE is an executable file, otherwise 0.
   Note that this function is the definition of what an
   executable file is; do not change this unless YOU know
   what an executable file is. */
static int
executable_file (file)
     char *file;
{
  struct stat finfo;
  static int user_id = -1;

  /* If the file doesn't exist, or is a directory, then we are
     not interested. */
  file_exists_p = !stat (file, &finfo);

#ifndef S_ISDIR
#define S_ISDIR(mode) (((mode) & S_IFMT) == S_IFDIR)
#endif

  if (!file_exists_p || S_ISDIR (finfo.st_mode))
    return (0);

  /* By definition, the only other criteria is that the file has
     an execute bit set that we can use. */
  if (user_id == -1)
    user_id = geteuid ();

  /* Root only requires execute permission for any of owner, group or
     others to be able to exec a file. */
  if (user_id == 0)
    {
      int bits;

      bits = (u_mode_bits (finfo.st_mode) |
	      g_mode_bits (finfo.st_mode) |
	      o_mode_bits (finfo.st_mode));

      return (X_BIT (bits));
    }

  /* If we are the owner of the file, the owner execute bit applies. */
  if (user_id == finfo.st_uid)
    return (X_BIT (u_mode_bits (finfo.st_mode)));

  /* If we are in the owning group, the group permissions apply. */
  if (group_member (finfo.st_gid))
    return (X_BIT (g_mode_bits (finfo.st_mode)));

  /* If `others' have execute permission to the file, then so do we,
     since we are also `others'. */
  return (X_BIT (o_mode_bits (finfo.st_mode)));
}

/* Return 1 if PATH1 and PATH2 are the same file.  This is kind of
   expensive.   If non-NULL STP1 and STP2 point to stat structures
   corresponding to PATH1 and PATH2, respectively. */
static int
same_file (path1, path2, stp1, stp2)
     char *path1, *path2;
     struct stat *stp1, *stp2;
{
  struct stat st1, st2;

  if (stp1 == 0)
    {
      if (stat (path1, &st1) != 0)
	return (0);
      stp1 = &st1;
    }

  if (stp2 == 0)
    {
      if (stat (path2, &st2) != 0)
	return (0);
      stp2 = &st2;
    }

  return ((stp1->st_dev == stp2->st_dev) && (stp1->st_ino == stp2->st_ino));
}

/* Return 1 if STRING is an absolute program name; it is absolute if it
   contains any slashes.  This is used to decide whether or not to look
   up through $PATH. */
static int
absolute_program (string)
     char *string;
{
  return ((char *)strchr (string, '/') != 0);
}

/* Given a string containing units of information separated by colons,
   return the next one pointed to by INDEX, or NULL if there are no more.
   Advance INDEX to the character after the colon. */
static char *
extract_colon_unit (string, index)
     char *string;
     int *index;
{
  int i, start;

  i = *index;

  if (!string || (i >= strlen (string)))
    return 0;

  /*
   * Each call to this routine leaves the index pointing at a colon if there
   * is more to the path.  If i is > 0, then increment past the `:'.  (If i
   * is 0, then the path has a leading colon.  If this is not done, the
   * second call to this routine will always return NULL, which will be
   * translated to  `.', even if `.' is not in the path.  Trailing colons
   * are handled OK by the `else' part of the if statement; it returns a null
   * string for the last component of a path with a trailing colon, and the
   * routines that call this will translate that to `.'.
   */

  if (i && string[i] == ':')
    i++;

  start = i;

  while (string[i] && string[i] != ':') i++;

  *index = i;

  if (i == start)
    {
      if (!string[i])
	return 0;

      (*index)++;

      return (savestring (""));
    }
  else
    {
      char *value;

      value = (char *)xmalloc (1 + (i - start));
      strncpy (value, &string[start], (i - start));
      value [i - start] = '\0';

      return (value);
    }
}

static char *
find_executable_name ()
{
  extern char *getenv ();
  struct Lisp_String *lname = XSTRING (Fcar (Vcommand_line_args));
  int name_len = lname->size;
  char *path_list = getenv ("PATH");
  int must_be_executable = 1;
  extern int file_exists_p;
  char *full_path;
  char *path;
  int path_index = 0;
  struct stat dot_stat_buf;
  char *name = (char *) alloca (name_len + 1);
  memcpy (name, lname->data, name_len);
  name[name_len] = 0;

  /* We haven't started looking, so we certainly haven't seen
     a `.' as the directory path yet. */
  dot_found_in_search = 0;

  if (absolute_program (name))
    {
      full_path = (char *)xmalloc (1 + name_len);
      strcpy (full_path, name);

      if (executable_file (full_path) || file_exists_p)
	{
	  return (full_path);
	}
      else
	{
	  xfree (full_path);
	  return 0;
	}
    }

  stat (".", &dot_stat_buf);   /* should set in get_working_directory */

  while (path_list && path_list[path_index])
    {
      path = extract_colon_unit (path_list, &path_index);
      if (!path || !*path)
	{
	  if (path)
	    xfree (path);
	  path = savestring ("."); /* by definition. */
	}

      /* Remember the location of "." in the path, in all its forms (as long as
	 they begin with a `.', e.g. `./.') */
      if ((*path == '.') && same_file (".", path, &dot_stat_buf, (struct stat *) 0))
	dot_found_in_search = 1;

      full_path = (char *)xmalloc (2 + strlen (path) + name_len);
      sprintf (full_path, "%s/%s", path, name);
      xfree (path);

      if (executable_file (full_path) ||
	  (!must_be_executable && file_exists_p))
	{
	  return (full_path);
	}
      else
	xfree (full_path);
    }

  return 0;
}

#ifdef NeXT
int malloc_cookie;
#endif

extern void init_alloc_once (void);
extern void init_obarray (void);
extern void init_eval_once (void);
extern void init_syntax_once (void);
extern void init_casetab_once (void);
extern void init_buffer_once (void);
extern void init_minibuf_once (void);
extern void init_window_once (void);

extern void init_alloc (void);
#ifdef MAINTAIN_ENVIRONMENT
extern void init_environ (void);
#endif
extern void init_eval (void);
extern void init_data (void);
extern void init_lread (void);
extern void init_cmdargs ();
extern void init_buffer (void);
#ifdef VMS
extern void init_vms_input (void);
#endif /* VMS */
extern void init_display (void);
extern void init_keyboard (void);
extern void init_callproc (void);
#ifdef VMS
extern void init_vmsproc (void);
#endif /* VMS */
extern void init_sys_modes (void);
extern void init_xdisp (void);
extern void init_macros (void);
extern void init_editfns (void);
#ifdef LISP_FLOAT_TYPE
extern void init_floatfns (void);
#endif
#ifdef VMS
extern void init_vmsfns (void);
#endif /* VMS */
#ifdef subprocesses
extern void init_process (void);
#endif /* subprocesses */

extern void syms_of_data (void);
extern void syms_of_alloc (void);
#ifdef MAINTAIN_ENVIRONMENT
extern void syms_of_environ (void);
#endif /* MAINTAIN_ENVIRONMENT */
extern void syms_of_lread (void);
extern void syms_of_print (void);
extern void syms_of_eval (void);
extern void syms_of_fns (void);
#ifdef LISP_FLOAT_TYPE
extern void syms_of_floatfns (void);
#endif
extern void syms_of_elhash (void);
extern void syms_of_abbrev (void);
extern void syms_of_buffer (void);
extern void syms_of_bytecode (void);
extern void syms_of_callint (void);
extern void syms_of_casefiddle (void);
extern void syms_of_casetab (void);
extern void syms_of_callproc (void);
extern void syms_of_cmds (void);
#ifndef NO_DIR_LIBRARY
extern void syms_of_dired (void);
#endif /* not NO_DIR_LIBRARY */
extern void syms_of_display (void);
extern void syms_of_doc (void);
extern void syms_of_editfns (void);
extern void syms_of_fileio (void);
#ifdef CLASH_DETECTION
extern void syms_of_filelock (void);
#endif /* CLASH_DETECTION */
extern void syms_of_indent (void);
extern void syms_of_keyboard (void);
extern void syms_of_keymap (void);
extern void syms_of_macros (void);
extern void syms_of_marker (void);
extern void syms_of_minibuf (void);
extern void syms_of_mocklisp (void);
#ifdef subprocesses
extern void syms_of_process (void);
#endif /* subprocesses */
extern void syms_of_search (void);
#ifdef MULTI_SCREEN
extern void syms_of_screen (void);
#endif
extern void syms_of_extents (void);
extern void syms_of_syntax (void);
extern void syms_of_undo (void);
#ifdef VMS
extern void syms_of_vmsproc (void);
#endif /* VMS */
extern void syms_of_window (void);
extern void syms_of_xdisp (void);
#ifdef HAVE_X_WINDOWS
extern void syms_of_xfns (void);
extern void syms_of_xselect (void);
extern void syms_of_menubar (void);
#endif /* HAVE_X_WINDOWS */
extern void syms_of_faces (void);
extern void syms_of_events (void);
extern void syms_of_event_alloc (void);
extern void syms_of_event_stream (void);
extern void syms_of_font_lock (void);
#ifdef ENERGIZE
extern void syms_of_editorside (void);
#endif
#ifdef EMACS_BTL
extern void syms_of_cadillac_btl_emacs (void);
#endif
extern void keys_of_keymap (void);

static void syms_of_emacs (void);

#ifndef SYSTEM_MALLOC
extern void malloc_init ();
#endif

extern void malloc_warning ();

#ifdef BSD
extern pid_t setpgrp ();
#endif

#ifdef RUN_TIME_REMAP
extern int run_time_remap ();
#endif

/* ARGSUSED */
#ifdef RUNNABLE_TEMACS
static int main_1 (argc, argv, envp)
#else
int main (argc, argv, envp)
#endif
     int argc;
     char **argv;
     char **envp;
{
  char stack_bottom_variable;
  int skip_args = 0;
  extern int errno;
  extern sys_nerr;
  extern char *sys_errlist[];

#ifdef NeXT
  /* this helps out unexNeXT.c */
  if (initialized)
    if (malloc_jumpstart (malloc_cookie) != 0)
      printf ("malloc jumpstart failed!\n");
#endif

#ifdef FREE_CHECKING
  init_free_hook ();
#endif

#ifdef VMS
#ifdef LINK_CRTL_SHARE
#ifdef SHAREABLE_LIB_BUG
  extern noshare char **environ;
#endif /* SHAREABLE_LIB_BUG */
#endif /* LINK_CRTL_SHARE */

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
    brk (bss_end);
#endif

  clearerr (stdin);
#ifdef BSD
  setpgrp (0, getpid ());
#endif

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
    malloc_init (0, malloc_warning);
#endif	/* not SYSTEM_MALLOC */

#ifdef HIGHPRI
  setpriority (PRIO_PROCESS, getpid (), HIGHPRI);
  setuid (getuid ());
#endif /* HIGHPRI */

#ifdef BSD
  /* interrupt_input has trouble if we aren't in a separate process group.  */
  setpgrp (getpid (), getpid ());
#endif

  inhibit_window_system = 0;

/* Handle the -t switch, which specifies filename to use as terminal */
  if (skip_args + 2 < argc && !strcmp (argv[skip_args + 1], "-t"))
    {
      int result;
      char *name, *ttyname ();

      skip_args += 2;
      close (0);
      close (1);
      result = open (argv[skip_args], O_RDWR, 2 );
      if (result < 0)
	{
	  char *errstring;

	  if (errno >= 0 && errno < sys_nerr)
	    errstring = sys_errlist[errno];
	  else
	    errstring = "undocumented error code";
	  fprintf (stderr, "emacs: %s: %s\n", argv[skip_args], errstring);
	  exit (1);
	}
      dup (0);
      if (! isatty (0))
	{
	  fprintf (stderr, "emacs: %s: not a tty\n", argv[skip_args]);
	  exit (1);
	}

      name = ttyname (0);
      fprintf (stderr, "Using %s\n", name);
#if 0
      fprintf (stderr, "Using %s\n", argv[skip_args]);
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
  if (skip_args + 1 < argc && !strcmp (argv[skip_args + 1], "-batch"))
    {
      skip_args += 1;
      noninteractive = 1;
    }

  /* Partially handle the -version and -help switches: they imply -batch,
     but are not removed from the list.
   */
  if (skip_args + 1 < argc &&
      (!strcmp (argv[skip_args + 1], "-version") ||
       !strcmp (argv[skip_args + 1], "-help")))
    noninteractive = 1;

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
      signal (SIGIOT, fatal_error_signal);
#ifdef SIGEMT
      signal (SIGEMT, fatal_error_signal);
#endif
      signal (SIGFPE, fatal_error_signal);
      signal (SIGBUS, fatal_error_signal);
      signal (SIGSEGV, fatal_error_signal);
      signal (SIGSYS, fatal_error_signal);
      signal (SIGTERM, fatal_error_signal);
#ifdef SIGXCPU
      signal (SIGXCPU, fatal_error_signal);
#endif
#ifdef SIGXFSZ
      signal (SIGXFSZ, fatal_error_signal);
#endif /* SIGXFSZ */

#ifdef IBMAIX
      signal (SIGDANGER, fatal_error_signal);
      signal (20, fatal_error_signal);
      signal (21, fatal_error_signal);
      signal (22, fatal_error_signal);
      signal (23, fatal_error_signal);
      signal (24, fatal_error_signal);
      signal (SIGAIO, fatal_error_signal);
      signal (SIGPTY, fatal_error_signal);
      signal (SIGIOINT, fatal_error_signal);
      signal (SIGGRANT, fatal_error_signal);
      signal (SIGRETRACT, fatal_error_signal);
      signal (SIGSOUND, fatal_error_signal);
      signal (SIGMSG, fatal_error_signal);
#endif
    }

  noninteractive1 = noninteractive;

/* Perform basic initializations (not merely interning symbols) */

  if (!initialized)
    {
      init_alloc_once ();
      init_obarray ();
      init_eval_once ();
      init_syntax_once ();	/* Create standard syntax table.
				   Must be done before init_buffer */
      init_casetab_once ();
      init_buffer_once ();	/* Create buffer table and some buffers */
      init_minibuf_once ();	/* Create list of minibuffers.
				   Must precede init_window_once */
      init_window_once ();	/* Init the window system */
    }

  init_alloc ();
#ifdef MAINTAIN_ENVIRONMENT
  init_environ ();
#endif
  init_eval ();
  init_data ();
  init_lread ();

  init_cmdargs (argc, argv, skip_args);	/* Create list Vcommand_line_args */
  init_buffer ();	/* Init default directory of main buffer */
  if (!noninteractive)
    {
#ifdef VMS
      init_vms_input ();/* init_display calls get_screen_size, that needs this */
#endif /* VMS */
      init_display ();	/* Determine terminal type.  init_sys_modes uses results */
    }
  init_keyboard ();	/* This too must precede init_sys_modes */
  init_callproc ();	/* And this too. */
#ifdef VMS
  init_vmsproc ();	/* And this too. */
#endif /* VMS */
  init_sys_modes ();	/* Init system terminal modes (RAW or CBREAK, etc.) */
  init_xdisp ();
  init_macros ();
  init_editfns ();
#ifdef LISP_FLOAT_TYPE
  init_floatfns ();
#endif
#ifdef VMS
  init_vmsfns ();
#endif /* VMS */
#ifdef subprocesses
  init_process ();
#endif /* subprocesses */

/* Intern the names of all standard functions and variables; define standard keys */

  if (!initialized)
    {
      /* The basic levels of Lisp must come first */
      /* And data must come first of all
	 for the sake of symbols like error-message */
      syms_of_data ();
      syms_of_alloc ();
#ifdef MAINTAIN_ENVIRONMENT
      syms_of_environ ();
#endif /* MAINTAIN_ENVIRONMENT */
      syms_of_lread ();
      syms_of_print ();
      syms_of_eval ();
      syms_of_fns ();
#ifdef LISP_FLOAT_TYPE
      syms_of_floatfns ();
#endif

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
      syms_of_indent ();
      syms_of_keyboard ();
      syms_of_keymap ();
      syms_of_macros ();
      syms_of_marker ();
      syms_of_minibuf ();
      syms_of_mocklisp ();
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
#ifdef VMS
      syms_of_vmsproc ();
#endif /* VMS */
      syms_of_window ();
      syms_of_xdisp ();
#ifdef HAVE_X_WINDOWS
      syms_of_xfns ();
      syms_of_xselect ();
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
      syms_of_cadillac_btl_emacs ();  /* #### rename me */
#endif

#ifdef ENERGIZE
      syms_of_editorside ();
#endif

#ifdef FREE_CHECKING
      syms_of_free_hook();
#endif
      keys_of_keymap ();
    }

  if (!initialized)
    {
      /* Handle -l loadup-and-dump, args passed by Makefile. */
      if (argc > 2 + skip_args && !strcmp (argv[1 + skip_args], "-l"))
	Vtop_level = Fcons (intern ("load"),
			    Fcons (build_string (argv[2 + skip_args]), Qnil));
#ifdef CANNOT_DUMP
      /* Unless next switch is -nl, load "loadup.el" first thing.  */
      if (!(argc > 1 + skip_args && !strcmp (argv[1 + skip_args], "-nl")))
	Vtop_level = Fcons (intern ("load"),
			    Fcons (build_string ("loadup.el"), Qnil));
#endif /* CANNOT_DUMP */
    }

  initialized = 1;

  /* try really hard to get the actually pathname of the exec file we
     are running */
  {
    Lisp_Object full_program_name;
    char *progname = find_executable_name ();
    full_program_name = (progname
			 ? Fexpand_file_name (build_string (progname), Qnil)
			 : Fcar (Vcommand_line_args));
    xfree (progname);

    Vexecution_path = full_program_name;
    Vinvocation_name = Ffile_name_nondirectory (full_program_name);
  }

#ifdef HAVE_TZSETWALL
  {
    /* fix for bug where emacs always thinks that it is in the
       timezone wherein it was dumped
     */
    extern void tzsetwall ();
    tzsetwall ();
  }
#endif

  /* Enter editor command loop.  This never returns.  */
  Frecursive_edit ();
  /* NOTREACHED */
  return -1;
}

#ifdef RUNNABLE_TEMACS

static jmp_buf run_temacs_catch;
static char *run_temacs_argv[100];
static char run_temacs_args[1000];
static int run_temacs_argc; /* -1 if dump_emacs or run_emacs_from_temacs */

extern char **environ;

/* ARGSUSED */
int
main (argc, argv, envp)
     int argc;
     char **argv;
     char **envp;
{
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
  return main_1 (argc, argv, envp);
}

DEFUN ("run-emacs-from-temacs",
       Frun_emacs_from_temacs, Srun_emacs_from_temacs, 0, MANY, 0,
  "Do not call this.  It will reinitialize your Emacs.  You'll be sorry.")
/* If this function is called from startup.el, it will be possible to run
   temacs as an editor, instead of having to dump an emacs and then run that
   (when debugging emacs itself, this can be much faster.)
   This will "restart" emacs with the specified command-line arguments.
 */
  (int nargs, Lisp_Object *args)
{
  extern char **environ;
  extern int gc_in_progress, waiting_for_input;
  int ac;
  int namesize;

  if (gc_in_progress) abort ();

  if (run_temacs_argc < 0)
    error ("I've lost my temacs-hood.");

  namesize = XSTRING (Vexecution_path)->size + 1;
  if (namesize >= sizeof (run_temacs_args))
    error ("execution-path string is too long");
  if (nargs >= (sizeof (run_temacs_argv) / sizeof (run_temacs_argv [0])))
    error ("too many args");
  memcpy (run_temacs_args, (char *) XSTRING (Vexecution_path)->data, namesize);
  run_temacs_argv [0] = run_temacs_args;
  for (ac = 0; ac < nargs; ac++)
    {
      int s;
      CHECK_STRING (args[ac], ac);
      s = XSTRING (args[ac])->size + 1;
      if (s + namesize > sizeof (run_temacs_args))
        error ("lose lose");
      memcpy (run_temacs_args + namesize, XSTRING (args[ac])->data, s);
      run_temacs_argv [ac + 1] = run_temacs_args + namesize;
      namesize += s;
    }
  run_temacs_argv [nargs + 1] = 0;
  unbind_to (0, Qnil); /* this closes loadup.el */
  waiting_for_input = 0;
  Vpurify_flag = Qnil;
  run_temacs_argc = nargs + 1;
  report_pure_usage ();
  _longjmp (run_temacs_catch, 1);
  return Qnil; /* not reached; warning suppression */
}

#endif /* RUNNABLE_TEMACS */


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
    call1 (Vrun_hooks, intern ("kill-emacs-hook"));

#ifdef subprocesses
  kill_buffer_processes (Qnil);
#endif /* subprocesses */

#ifdef VMS
  kill_vms_processes ();
#endif /* VMS */

  Fdo_auto_save (Qt);

#ifdef CLASH_DETECTION
  unlock_all_files ();
#endif /* CLASH_DETECTION */

  fflush (stdout);
  reset_sys_modes ();

#ifdef HAVE_X_WINDOWS
  if (!noninteractive && EQ (Vwindow_system, intern ("x")))
    Fx_close_current_connection ();
#endif /* HAVE_X_WINDOWS */

  UNGCPRO;

/* Is it really necessary to do this deassign
   when we are going to exit anyway?  */
/* #ifdef VMS
  stop_vms_input ();
 #endif  */
  stuff_buffered_input (arg);
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

#ifndef CANNOT_DUMP
/* Nothing like this can be implemented on an Apollo.
   What a loss!  */

DEFUN ("dump-emacs", Fdump_emacs, Sdump_emacs, 2, 2, 0,
  "Dump current state of Emacs into executable file FILENAME.\n\
Take symbols from SYMFILE (presumably the file you executed to run Emacs).\n\
This is used in the file `loadup.el' when building Emacs.\n\
\n\
Bind `command-line-processed' to nil before dumping,\n\
if you want the dumped Emacs to process its command line\n\
and announce itself normally when it is run.")
  (intoname, symname)
     Lisp_Object intoname, symname;
{
  register unsigned char *a_name = 0;
  extern char my_edata;
  Lisp_Object tem;

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
      if (XSTRING (symname)->size)
	{
	  symname = Fexpand_file_name (symname, Qnil);
	  a_name = XSTRING (symname)->data;
	}
    }

  tem = Vpurify_flag;
  Vpurify_flag = Qnil;

  report_pure_usage ();
  fflush (stdout);
#ifdef VMS
  mapout_data (XSTRING (intoname)->data);
#else
  /* Tell malloc where start of impure now is */
  /* Also arrange for warnings when nearly out of space.  */
#ifndef SYSTEM_MALLOC
  malloc_init (&my_edata, malloc_warning);
#endif
  unexec (XSTRING (intoname)->data, a_name, &my_edata, 0, 0);
#endif /* not VMS */

  Vpurify_flag = tem;

  return Qnil;
}

#endif /* not CANNOT_DUMP */

#ifdef VMS
#define SEPCHAR ','
#else
#define SEPCHAR ':'
#endif

Lisp_Object
decode_env_path (evarname, defalt)
     const char *evarname, *defalt;
{
  register const char *path;
  register const char *p;

  Lisp_Object lpath;

  path = (char *) egetenv (evarname);
  if (!path)
    path = defalt;
  lpath = Qnil;
  while (1)
    {
      p = strchr (path, SEPCHAR);
      if (!p) p = path + strlen (path);
      lpath = Fcons (p - path ? make_string (path, p - path) : Qnil,
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

static void
syms_of_emacs ()
{
#ifndef CANNOT_DUMP
  defsubr (&Sdump_emacs);
#endif /* not CANNOT_DUMP */

#ifdef RUNNABLE_TEMACS
  defsubr (&Srun_emacs_from_temacs);
#endif

  defsubr (&Skill_emacs);
  defsubr (&Snoninteractive);

  DEFVAR_LISP ("command-line-args", &Vcommand_line_args,
    "Args passed by shell to Emacs, as a list of strings.");

  DEFVAR_LISP ("invocation-name", &Vinvocation_name,
    "Name of file used to invoke editing session.\n\
This is the same as `(file-name-nondirectory execution-path)'.");

  DEFVAR_LISP ("execution-path", &Vexecution_path,
    "Pathname of executable emacs program now running.");

  DEFVAR_LISP ("system-type", &Vsystem_type,
    "Value is symbol indicating type of operating system you are using.");
  Vsystem_type = intern (SYSTEM_TYPE);

  DEFVAR_BOOL ("noninteractive", &noninteractive1,
    "Non-nil means Emacs is running without interactive terminal.");
}
