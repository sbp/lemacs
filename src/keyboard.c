/* Keyboard input; editor command loop.
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

/* #include <signal.h>  use "syssignal.h" instead -jwz */

#include "config.h"
#include "intl.h"
#include <stdio.h>

#include "termchar.h"
#include "termopts.h"
#include "lisp.h"
#include "termhooks.h"
#include "macros.h"
#include "buffer.h"
#include "screen.h"
#include "window.h"
#include "commands.h"
#include "disptab.h"
#include "events.h"

#include "backtrace.h"          /* for command-execute's pleasure */

#include <setjmp.h>
#include <errno.h>

#ifndef VMS
#include <sys/ioctl.h>
#endif

#include "syssignal.h"
#include "systty.h"
#include "systime.h"

#include "sysdep.h"

#include "blockio.h"

/* Non-nil disable property on a command means
   do not execute it; call disabled-command-hook's value instead. */
Lisp_Object Qdisabled, Vdisabled_command_hook;

/* True while doing kbd input */
int waiting_for_input;

/* Nonzero means C-G should cause immediate error-signal. */
int immediate_quit;

/* Character to recognize as the help char.  */
Lisp_Object help_char;

/* Character to set Vquit_flag. */
int interrupt_char;

/* Form to execute when help char is typed.  */
Lisp_Object Vhelp_form;

Lisp_Object Vpre_command_hook, Vpost_command_hook;
Lisp_Object Qpre_command_hook, Qpost_command_hook;

Lisp_Object Qsuspend_hook;
Lisp_Object Qsuspend_resume_hook;


/* Current depth in recursive edits.  */
int command_loop_level;

/* Total number of times command_loop has read a key sequence.  */
int num_input_keys;

/* Last keyboard or mouse input event read as a command. */
Lisp_Object Vlast_command_event;

/* The nearest ASCII equivalent of the above. */
Lisp_Object Vlast_command_char;

/* Last keyboard or mouse event read for any purpose. */
Lisp_Object Vlast_input_event;

/* The nearest ASCII equivalent of the above. */
Lisp_Object Vlast_input_char;

/* If not Qnil, an event object to be read as the next command input */
Lisp_Object Vunread_command_event;
Lisp_Object Qunread_command_event;

/* Char to use as prefix when a meta character is typed in.
 This is bound on entry to minibuffer in case Esc is changed there.  */

/* Previous command, represented by a Lisp object.
   Does not include prefix commands and arg setting commands */
Lisp_Object Vlast_command;

/* If a command sets this, the value goes into
   previous-command for the next command. */
Lisp_Object Vthis_command;

/* A (16bit . 16bit) representation of the time of the last-command-event.
 */
Lisp_Object Vlast_input_time;


#ifndef LISP_COMMAND_LOOP
/* Form to evaluate (if non-nil) when Emacs is started */
Lisp_Object Vtop_level;
#else
/* Function to call to evaluate to read and process events */
Lisp_Object Vcommand_loop;
#endif /* LISP_COMMAND_LOOP */

/* The error handler */
Lisp_Object Qcommand_error;


/* User-supplied string to translate input characters through */
Lisp_Object Vkeyboard_translate_table;

/* File in which we write all commands we read */
/* #### there is exactly zero chance that this works right now */
static FILE *dribble;

/* #### this should be a property of the tty event_stream */
/* Nonzero if should obey 0200 bit in input chars as "Meta" */
int meta_key;

#if 0
/* Address (if not 0) of word to zero out
 if a SIGIO interrupt happens */
/* #### whatever this is, I'm sure it doesn't work */
static long *input_available_clear_word;
#endif

/* Set to 1 when a SIGIO interrupt is processed.  The QUIT macro may look
   at this. */
int sigio_happened;

/* Nonzero means use SIGIO interrupts; zero means use CBREAK mode.
   Default is 1 if INTERRUPT_INPUT is defined.  */
int interrupt_input;

SIGTYPE interrupt_signal ();

/* Nonzero while interrupts are temporarily deferred during redisplay.  */
int interrupts_deferred;

/* nonzero means use ^S/^Q for flow control.  */
/* #### should be a property of tty event_stream */
int flow_control;

/* Allow m- file to inhibit use of FIONREAD.  */
#ifdef BROKEN_FIONREAD
#undef FIONREAD
#endif

/* We are unable to use interrupts if FIONREAD is not available,
   so flush SIGIO so we won't try. */
#ifndef FIONREAD
#ifdef SIGIO
#undef SIGIO
#endif
#endif

/* Function for init_keyboard to call with no args (if nonzero).  */
void (*keyboard_init_hook) (void);


#define	min(a,b)	((a)<(b)?(a):(b))
#define	max(a,b)	((a)>(b)?(a):(b))

Lisp_Object
load1 (Lisp_Object name)
{
  Fload (name, Qnil, Qt, Qnil);
  return (Qnil);
}


/**********************************************************************/
/* Command-loop (in C)                                                */
/**********************************************************************/

static Lisp_Object
command_loop_1 (Lisp_Object dummy)
{
  Vprefix_arg = Qnil;
  return (Fcommand_loop_1 ());
}

#ifndef LISP_COMMAND_LOOP

static Lisp_Object command_loop_2 (Lisp_Object dummy);
static Lisp_Object top_level_1 (Lisp_Object dummy);

static Lisp_Object
recursive_edit_unwind (Lisp_Object buffer)
{
  if (!NILP (buffer))
    Fset_buffer (buffer);

  command_loop_level--;
  redraw_mode_line = 1;

  return Qnil;
}

DEFUN ("recursive-edit", Frecursive_edit, Srecursive_edit, 0, 0, "",
  "Invoke the editor command loop recursively.\n\
To get out of the recursive edit, a command can do `(throw 'exit nil)';\n\
that tells this function to return.\n\
Alternately, `(throw 'exit t)' makes this function signal an error.\n\
This function is called by the editor initialization to begin editing.")
  ()
{
  Lisp_Object val;
  int speccount = specpdl_depth ();

  command_loop_level++;
  redraw_mode_line = 1;

  record_unwind_protect (recursive_edit_unwind,
			 ((current_buffer
                           != XBUFFER (XWINDOW (selected_window)->buffer))
                          ? Fcurrent_buffer ()
                          : Qnil));

  specbind (Qstandard_output, Qt);
  specbind (Qstandard_input, Qt);

  val = internal_catch (Qexit, command_loop_2, Qnil, 0);

  if (EQ (val, Qt))
    /* Turn abort-recursive-edit into a quit. */
    Fsignal (Qquit, Qnil);

  return unbind_to (speccount, Qnil);
}

Lisp_Object
call_command_loop (Lisp_Object catch_errors)
{
  if (NILP (catch_errors))
    return (command_loop_1 (Qnil));
  else
    return (command_loop_2 (Qnil));
}


DOESNT_RETURN
initial_command_loop (Lisp_Object load_me)
{
  if (!NILP (load_me))
    Vtop_level = list2 (Qload, load_me);
  for (;;)
  {
    command_loop_level = 0;
    redraw_mode_line = 1;

    internal_catch (Qtop_level, top_level_1, Qnil, 0);
    internal_catch (Qtop_level, command_loop_2, Qnil, 0);

    /* End of file in -batch run causes exit here.  */
    if (noninteractive)
      Fkill_emacs (Qt);
  }
}



static Lisp_Object
cmd_error (Lisp_Object data, Lisp_Object dummy)
{
  int speccount = specpdl_depth ();

  Vquit_flag = Qnil;

  if (!NILP (Ffboundp (Qcommand_error)))
    return call1 (Qcommand_error, data);

  Fding (Qnil, Qnil);
  Fzmacs_deactivate_region ();
  Fdiscard_input ();
  specbind (Qinhibit_quit, Qt);
  Vstandard_output = Qt;
  Vstandard_input = Qt;
  Vexecuting_macro = Qnil;
  echo_area_glyphs = 0;
  data = Fprin1_to_string (data, Qnil);
  message (GETTEXT ("Error: %s"), XSTRING (data)->data);
  /* Return non-nil value to say command loop shouldn't exit. */
  return (unbind_to (speccount, Qt));
}

/* Here we catch errors in execution of commands within the
   editing loop, and reenter the editing loop.
   When there is an error, cmd_error runs and returns a non-nil
   value to us.  A value of nil means that cmd_loop_1 itself
   returned due to end of file (or end of kbd macro).  */

static Lisp_Object
command_loop_2 (Lisp_Object dummy)
{
  for (;;)
    {
      if (NILP (condition_case_1 (Qerror,
				  command_loop_1, Qnil,
				  cmd_error, Qnil)))
        return (Qnil);
    }
}

static Lisp_Object
top_level_1 (Lisp_Object dummy)
{
  /* On entry to the outer level, run the startup file */
  if (!NILP (Vtop_level))
    condition_case_1 (Qerror, Feval, Vtop_level, cmd_error, Qnil);
#if 1
  else
    {
      fprintf (stderr, GETTEXT ("\ntemacs can only be run in -batch mode.\n"));
      noninteractive = 1; /* prevent things under kill-emacs from blowing up */
      Fkill_emacs (make_number (-1));
    }
#else
  else if (purify_flag)
    message (GETTEXT ("Bare impure Emacs (standard Lisp code not loaded)"));
  else
    message (GETTEXT ("Bare Emacs (standard Lisp code not loaded)"));
#endif

  return Qnil;
}

#endif /* !LISP_COMMAND_LOOP */

/**********************************************************************/
/* Alternate command-loop (largely in Lisp)                           */
/**********************************************************************/

#ifdef LISP_COMMAND_LOOP

/* emergency backups for cold-load-stream use */
static Lisp_Object
cold_load_command_error (Lisp_Object datum, Lisp_Object ignored)
{
  int speccount = specpdl_depth ();

  Vquit_flag = Qnil;
  Fding (Qnil, Qnil);
  Fzmacs_deactivate_region ();
  Fdiscard_input ();
  specbind (Qinhibit_quit, Qt);
  Vstandard_output = Qt;
  Vstandard_input = Qt;
  Vexecuting_macro = Qnil;
  echo_area_glyphs = 0;
  datum = Fprin1_to_string (datum, Qnil);
  message (GETTEXT ("Error: %s"), XSTRING (datum)->data);
  Vquit_flag = Qnil;
  return (unbind_to (speccount, Qt));
}


static Lisp_Object
cold_load_command_loop (Lisp_Object dummy)
{
  return (condition_case_1 (Qt,
                            command_loop_1, Qnil,
                            cold_load_command_error, Qnil));
}

/* This nonsense is the only way to avoid retval warnings here.  FMH! */
static void
call_command_loop_kludge (Lisp_Object catch_errors)
{
  reset_this_command_keys (Qnil);

 loop:
  for (;;)
  {
    if (NILP (Vcommand_loop))
      break;
    call1 (Vcommand_loop, catch_errors);
  }

  /* This isn't a "correct" definition, but you're pretty hosed if
     you broke "command-loop" anyway */
  Vprefix_arg = Qnil;
  if (NILP (catch_errors))
    Fcommand_loop_1 ();
  else 
    internal_catch (Qtop_level,
                    cold_load_command_loop, Qnil, 0);
  goto loop;
}

Lisp_Object
call_command_loop (Lisp_Object catch_errors)
{
  call_command_loop_kludge (catch_errors);
  return Qnil;
}

static Lisp_Object
initial_error_handler (Lisp_Object datum, Lisp_Object ignored)
{
  Vcommand_loop =  Qnil;
  Fding (Qnil, Qnil);

  if (CONSP (datum) && EQ (XCONS (datum)->car, Qquit))
    /* Don't bother with the message */
    return (Qt);
      
  message (GETTEXT ("Error in command-loop!!"));
  Fset (intern ("last-error"), datum); /* >>> Better/different name? */
  Fsit_for (make_number (2), Qnil);
  cold_load_command_error (datum, Qnil);
  return (Qt);
}


DOESNT_RETURN
initial_command_loop (Lisp_Object load_me)
{
  if (!NILP (load_me))
    {
      if (!NILP (condition_case_1 (Qt, load1, load_me,
                                   initial_error_handler, Qnil)))
	Fkill_emacs (make_number (-1));
    }

  for (;;)
    {
      command_loop_level = 0;
      redraw_mode_line = 1;

      condition_case_1 (Qt,
			call_command_loop, Qtop_level,
			initial_error_handler, Qnil);
    }
}

#endif /* LISP_COMMAND_LOOP */


/**********************************************************************/
/* Guts of command-loop                                               */
/**********************************************************************/

/* This is the actual command reading loop,
   sans error-handling encapsulation */

DEFUN ("command-loop-1", Fcommand_loop_1, Scommand_loop_1, 0, 0, 0,
  "Invoke the internals of the canonical editor command loop.\n\
Don't call this unless you know what you're doing.")
  ()
{
  Lisp_Object event = Fallocate_event ();
  Lisp_Object old_loop = Qnil;
  struct gcpro gcpro1, gcpro2;
  GCPRO2 (event, old_loop);

  waiting_for_input = 0;
  /* cancel_echoing (); */
  /* This magically makes single character keyboard macros work just
     like the real thing.  This is slightly bogus, but it's in here for
     compatibility with Emacs 18.  It's not even clear what the "right
     thing" is. */
  if (!(((STRINGP (Vexecuting_macro) || VECTORP (Vexecuting_macro))
         && XINT (Flength (Vexecuting_macro)) == 1)))
    Vlast_command = Qt;

#ifndef LISP_COMMAND_LOOP
  while (1)
#else
  old_loop = Vcommand_loop;
  while (EQ (Vcommand_loop, old_loop))
#endif /* LISP_COMMAND_LOOP */
    {
      /* Make sure the current window's buffer is selected.  */
      if (XBUFFER (XWINDOW (selected_window)->buffer) != current_buffer)
	set_buffer_internal (XBUFFER (XWINDOW (selected_window)->buffer));

#if 0 /* NYI */
      /* Display any malloc warning that just came out.  Use while because
	 displaying one warning can cause another.  */
      while (pending_malloc_warning)
	display_malloc_warning ();
#endif

      /* If ^G was typed before we got here (that is, before emacs was
	 idle and waiting for input) then we treat that as an interrupt. */
      QUIT;

      /* If minibuffer on and echo area in use, wait 2 sec and redraw
	 minibuffer.  Treat a ^G here as a command, not an interrupt.
       */
      if (minibuf_level > 0 && echo_area_glyphs)
	{
	  Fsit_for (make_number (2), Qnil);
	  echo_area_glyphs = 0;
	}

      /* Shortcut not applicable or found a prefix key.
	 Take full precautions and read key sequence the hard way.  */
#ifdef C_ALLOCA
      alloca (0);		/* Cause a garbage collection now */
				/* Since we can free the most stuff here.  */
#endif /* C_ALLOCA */

      Fnext_event_1 (event, Qnil);
      /* If ^G was typed while emacs was reading input from the user, then
	 it is treated as just another key.  This is strange, but it is
	 what emacs 18 did. */
      Vquit_flag = Qnil;
      Fdispatch_event (event);
    }
  UNGCPRO; 
  return Qnil;
}



/**********************************************************************/
/* command-exectute                                                   */
/**********************************************************************/


DEFUN ("command-execute", Fcommand_execute, Scommand_execute, 1, 2, 0,
 "Execute CMD as an editor command.\n\
CMD must be a symbol that satisfies the `commandp' predicate.\n\
Optional second arg RECORD-FLAG non-nil\n\
means unconditionally put this command in `command-history'.\n\
Otherwise, that is done only if an arg is read using the minibuffer.")
     (cmd, record)
     Lisp_Object cmd, record;
{
  Lisp_Object prefixarg;
  Lisp_Object final = cmd;
  struct backtrace backtrace;

  prefixarg = Vprefix_arg;
  Vprefix_arg = Qnil;
  Vcurrent_prefix_arg = prefixarg;

  if (SYMBOLP (cmd) && !NILP (Fget (cmd, Qdisabled, Qnil)))
    {
	return call1 (Vrun_hooks, Vdisabled_command_hook);
    }

  for (;;)
	{
      final = indirect_function (cmd, 1);
      if (CONSP (final) && EQ (Fcar (final), Qautoload))
	do_autoload (final, cmd);
      else
	break;
    }

  if (CONSP (final) || SUBRP (final) || COMPILEDP (final))
    {
#ifdef EMACS_BTL
      backtrace.id_number = 0;
#endif
      backtrace.next = backtrace_list;
      backtrace_list = &backtrace;
      backtrace.function = &Qcall_interactively;
      backtrace.args = &cmd;
      backtrace.nargs = 1;
      backtrace.evalargs = 0;
      backtrace.pdlcount = specpdl_depth ();
      backtrace.debug_on_exit = 0;

      final = Fcall_interactively (cmd, record);

      backtrace_list = backtrace.next;
      return (final);
    }
  else if (STRINGP (final) || VECTORP (final))
    {
      return Fexecute_kbd_macro (final, prefixarg);
    }
  else
    {
      Fsignal (Qwrong_type_argument,
	       Fcons (Qcommandp,
		      ((EQ (cmd, final))
                       ? list1 (cmd)
                       : list2 (cmd, final))));
      return Qnil;
    }
}


/* Number of seconds between polling for input.  */
int polling_period;

#ifdef POLL_FOR_INPUT
int polling_for_input;

/* Nonzero means polling for input is temporarily suppresed.  */
int poll_suppress_count;

/* Handle an alarm once each second and read pending input
   so as to handle a C-g if it comces in.  */

SIGTYPE
input_poll_signal ()
{
  int junk;

  if (interrupt_input_blocked == 0)
    if (!waiting_for_input)
      read_avail_input (&junk);
  signal (SIGALRM, input_poll_signal);
  alarm (polling_period);
}

#endif

/* Begin signals to poll for input, if they are appropriate.
   This function is called unconditionally from various places.  */

void
start_polling ()
{
#ifdef POLL_FOR_INPUT
  if (read_socket_hook)
    {
      poll_suppress_count--;
      if (poll_suppress_count == 0)
	{
	  signal (SIGALRM, input_poll_signal);
	  polling_for_input = 1;
	  alarm (polling_period);
	}
    }
#endif
}

/* Turn off polling.  */

void
stop_polling ()
{
#ifdef POLL_FOR_INPUT
  if (read_socket_hook)
    {
      if (poll_suppress_count == 0)
	{
	  polling_for_input = 0;
	  alarm (0);
	}
      poll_suppress_count++;
    }
#endif
}


/* Set this for debugging, to have a way to get out */
int stop_character;

#ifdef HAVE_X_WINDOWS
/* no point in including X headers, we just want to know if it's non-zero. */
extern struct _XDisplay* x_current_display;
#endif

/* Interface to read_avail_input, blocking SIGIO if necessary.  */


#ifdef SIGIO   /* for entire page */

/* Note SIGIO has been undef'd if FIONREAD is missing.  */

void
input_available_signal (signo)
     int signo;
{
  /* Must preserve main program's value of errno.  */
  int old_errno = errno;

  sigio_happened = 1;

#ifdef USG
  /* USG systems forget handlers when they are used;
     must reestablish each time */
  signal (signo, input_available_signal);
#endif /* USG */

#ifdef BSD4_1
  sigisheld (SIGIO);
#endif

#ifdef BSD4_1
  select_alarmed = 1;  /* Force the select emulator back to life */
  sigfree ();
#endif
  errno = old_errno;
}
#endif /* SIGIO */


DEFUN ("recent-keys", Frecent_keys, Srecent_keys, 0, 0, 0,
  "Return vector of last 100 keyboard or mouse button events read.\n\
This copies 100 event objects and a vector; it is safe to keep and modify\n\
them.")
  ()
{
  struct gcpro gcpro1;
  Lisp_Object val = make_vector (100, Qnil);
  Lisp_Object *vec = XVECTOR (val)->contents;
  Lisp_Object *vec2 = XVECTOR (recent_keys_ring)->contents;
  int i = 0, j = recent_keys_ring_index;
  GCPRO1 (val);
  while (i < 100) {
    vec [i] = vec2 [j];
    if (NILP (vec [i]))
      vec [i] = Fallocate_event ();
    else
      vec [i] = Fcopy_event (vec [i], Qnil);
    if (++j >= 100) j = 0;
    i++;
  }
  UNGCPRO;
  return val;
}

DEFUN ("open-dribble-file", Fopen_dribble_file, Sopen_dribble_file, 1, 1,
  "FOpen dribble file: ",
  "Start writing all keyboard characters to FILE.")
  (file)
     Lisp_Object file;
{
  if (dribble)
    fclose (dribble);
  dribble = 0;
  if (!NILP (file))
    {
      file = Fexpand_file_name (file, Qnil);
      dribble = fopen ((char *) XSTRING (file)->data, "w");
    }
  return Qnil;
}


static Lisp_Object
unwind_init_sys_modes (Lisp_Object dummy)
{
  init_sys_modes ();
  return (dummy);
}

DEFUN ("suspend-emacs", Fsuspend_emacs, Ssuspend_emacs, 0, 1, "",
  "Stop Emacs and return to superior process.  You can resume later.\n\
On systems that don't have job control, run a subshell instead.\n\n\
If optional arg STUFFSTRING is non-nil, its characters are stuffed\n\
to be read as terminal input by Emacs's superior shell.\n\
Before suspending, if `suspend-hook' is bound and value is non-nil\n\
call the value as a function of no args.  Don't suspend if it returns non-nil.\n\
Otherwise, suspend normally and after resumption call\n\
`suspend-resume-hook' if that is bound and non-nil.\n\
\n\
Some operating systems cannot stop the Emacs process and resume it later.\n\
On such systems, Emacs will start a subshell and wait for it to exit.")
  (stuffstring)
     Lisp_Object stuffstring;
{
  register Lisp_Object tem;
  int speccount = specpdl_depth ();
  struct gcpro gcpro1;

  if (!NILP (stuffstring))
    CHECK_STRING (stuffstring, 0);
  GCPRO1 (stuffstring);

  /* Call value of suspend-hook
     if it is bound and value is non-nil.  */
  if (!NILP (Vrun_hooks))
    {
      tem = call1 (Vrun_hooks, Qsuspend_hook);
      if (!EQ (tem, Qnil)) return Qnil;
    }

  reset_sys_modes ();
  /* sys_suspend can get an error if it tries to fork a subshell
     and the system resources aren't available for that.  */
  record_unwind_protect (unwind_init_sys_modes, Qnil);
  stuff_buffered_input (stuffstring);
  sys_suspend ();
  unbind_to (speccount, Qnil);

  /* Call value of suspend-resume-hook
     if it is bound and value is non-nil.  */
  if (!NILP (Vrun_hooks))
    call1 (Vrun_hooks, Qsuspend_resume_hook);
  
  UNGCPRO;
  return Qnil;
}

/* If STUFFSTRING is a string, stuff its contents as pending terminal input.
   Then in any case stuff anthing Emacs has read ahead and not used.  */

void
stuff_buffered_input (stuffstring)
     Lisp_Object stuffstring;
{
/* stuff_char works only in BSD, versions 4.2 and up.  */
#ifdef BSD
#ifndef BSD4_1
  register unsigned char *p;

  if (STRINGP (stuffstring))
    {
      register int count;

      p = XSTRING (stuffstring)->data;
      count = XSTRING (stuffstring)->size;
      while (count-- > 0)
	stuff_char (*p++);
      stuff_char ('\n');
    }
  /* Anything we have read ahead, put back for the shell to read.  */
# if 0 /* oh, who cares about this silliness */
  while (kbd_fetch_ptr != kbd_store_ptr)
    {
      if (kbd_fetch_ptr == kbd_buffer + KBD_BUFFER_SIZE)
	kbd_fetch_ptr = kbd_buffer;
      stuff_char (*kbd_fetch_ptr++);
    }
# endif
#endif
#endif /* BSD and not BSD4_1 */
}

#if 0 /* Unused in Lucid Emacs */
void
set_waiting_for_input (word_to_clear)
     long *word_to_clear;
{
  input_available_clear_word = word_to_clear;

  /* Tell interrupt_signal to throw back to read_char,  */
  waiting_for_input = 1;

  /* If interrupt_signal was called before and buffered a C-g,
     make it run again now, to avoid timing error. */
  detect_input_pending ();
  QUIT;

}

void
clear_waiting_for_input ()
{
  /* Tell interrupt_signal not to throw back to read_char,  */
  waiting_for_input = 0;
  input_available_clear_word = 0;
}
#endif /* Unused */

/* This routine is called at interrupt level in response to C-G.
 If interrupt_input, this is the handler for SIGINT.
 Otherwise, it is called from Fnext_event in handling SIGIO or SIGTINT.

 Otherwise it sets the Lisp variable  quit-flag  not-nil.
 This causes  eval  to throw, when it gets a chance.
 If  quit-flag  is already non-nil, it stops the job right away.  */

SIGTYPE
interrupt_signal (dummy)
     int dummy;
{
  char c;
  /* Must preserve main program's value of errno.  */
  int old_errno = errno;

#ifdef USG
  /* USG systems forget handlers when they are used;
     must reestablish each time */
  signal (SIGINT, interrupt_signal);
  signal (SIGQUIT, interrupt_signal);
#endif /* USG */

  if (!NILP (Vquit_flag) && SCREEN_IS_TERMCAP (selected_screen))
    {
      fflush (stdout);
      reset_sys_modes ();
      sigfree ();
#ifdef SIGTSTP			/* Support possible in later USG versions */
/*
 * On systems which can suspend the current process and return to the original
 * shell, this command causes the user to end up back at the shell.
 * The "Auto-save" and "Abort" questions are not asked until
 * the user elects to return to emacs, at which point he can save the current
 * job and either dump core or continue.
 */
      sys_suspend ();
#else
#ifdef VMS
      if (sys_suspend () == -1)
	{
	  fprintf (stdout, GETTEXT ("Not running as a subprocess;\n"));
	  fprintf (stdout, GETTEXT ("you can continue or abort.\n"));
	}
#else /* not VMS */
      /* Perhaps should really fork an inferior shell?
	 But that would not provide any way to get back
	 to the original shell, ever.  */
      fprintf (stdout, GETTEXT ("No support for stopping a process on this operating system;\n"));
      fprintf (stdout, GETTEXT ("you can continue or abort.\n"));
#endif /* not VMS */
#endif /* not SIGTSTP */
      fprintf (stdout, GETTEXT ("Auto-save? (y or n) "));
      fflush (stdout);
      if (((c = getc (stdin)) & ~040) == 'Y')
	Fdo_auto_save (Qnil, Qnil);
      while (c != '\n')
        c = getc (stdin);
#ifdef VMS
      fprintf (stdout, GETTEXT ("Abort (and enter debugger)? (y or n) "));
#else /* not VMS */
      fprintf (stdout, GETTEXT ("Abort (and dump core)? (y or n) "));
#endif /* not VMS */
      fflush (stdout);
      if (((c = getc (stdin)) & ~040) == 'Y')
	abort ();
      while (c != '\n')
        c = getc (stdin);
      fprintf (stdout, GETTEXT ("Continuing...\n"));
      fflush (stdout);
      init_sys_modes ();
    }
  else
    {
      /* If executing a function that wants to be interrupted out of
	     and the user has not deferred quitting by binding `inhibit-quit'
	     then quit right away.  */
      if (immediate_quit && NILP (Vinhibit_quit))
	{
	  immediate_quit = 0;
          sigfree ();
	  Fsignal (Qquit, Qnil);
	}
      else
	/* Else request quit when it's safe */
	Vquit_flag = Qt;
    }
  errno = old_errno;
  SIGRETURN;
}


DEFUN ("set-input-mode", Fset_input_mode, Sset_input_mode, 3, 3, 0,
  "Set mode of reading keyboard input.\n\
First arg non-nil means use input interrupts; nil means use CBREAK mode.\n\
Second arg non-nil means use ^S/^Q flow control for output to terminal\n\
 (no effect except in CBREAK mode).\n\
Third arg non-nil means accept 8-bit input (for a Meta key).\n\
 Otherwise, the top bit is ignored, on the assumption it is parity.")
  (interrupt, flow, meta)
     Lisp_Object interrupt, flow, meta;
{
  reset_sys_modes ();
#ifdef SIGIO
/* Note SIGIO has been undef'd if FIONREAD is missing.  */
#ifdef NO_SOCK_SIGIO
  if (read_socket_hook)
    interrupt_input = 0;	/* No interrupts if reading from a socket.  */
  else
#endif /* NO_SOCK_SIGIO */
    interrupt_input = !NILP (interrupt);
#else /* not SIGIO */
  interrupt_input = 0;
#endif /* not SIGIO */
  flow_control = !NILP (flow);
  meta_key = !NILP (meta);
  init_sys_modes ();
  return Qnil;
}

/* This may actually need to do something goofy once ttys work again */
DEFUN ("set-interrupt-character", Fset_interrupt_character,
       Sset_interrupt_character, 1, 1, 0,
  "Change the interrupt character.  Arg is an ASCII code or nil.\n\
Among other system-dependent things, this changes the value of the\n\
variable `interrupt-char'.")
  (new_interrupt_char)
	Lisp_Object new_interrupt_char;
{
  int c;
  if (NILP (new_interrupt_char))
    c = -1;
  else
    {
      CHECK_FIXNUM (new_interrupt_char, 0);
      c = XINT (new_interrupt_char);
      if (c < -1 || c > 127)
	error (GETTEXT ("interrupt character must be an ASCII code, or nil or -1"));
    }

  interrupt_char = c;
  return make_number (interrupt_char);
}


void
init_keyboard ()
{
  /* This is correct before outermost invocation of the editor loop */
  immediate_quit = 0;

  if (!noninteractive)
    {
      signal (SIGINT, interrupt_signal);
#ifdef HAVE_TERMIO
      /* On  systems with TERMIO, C-g is set up for both SIGINT and SIGQUIT
	 and we can't tell which one it will give us.  */
      signal (SIGQUIT, interrupt_signal);
#endif /* HAVE_TERMIO */
/* Note SIGIO has been undef'd if FIONREAD is missing.  */
#ifdef SIGIO
      signal (SIGIO, input_available_signal);
#endif /* SIGIO */
    }

/* Use interrupt input by default, if it works and noninterrupt input
   has deficiencies.  */

#ifdef INTERRUPT_INPUT
  interrupt_input = 1;
#else
  interrupt_input = 0;
#endif

  sigfree ();
  dribble = 0;

  if (keyboard_init_hook)
    (*keyboard_init_hook) ();
}

void
syms_of_keyboard ()
{
  sigio_happened = 0;

  defsymbol (&Qdisabled, "disabled");
  defsymbol (&Qcommand_error, "command-error");
  defsymbol (&Qtop_level, "top-level");
  defsymbol (&Qsuspend_hook, "suspend-hook");
  defsymbol (&Qsuspend_resume_hook, "suspend-resume-hook");
  defsymbol (&Qunread_command_event, "unread-command-event");

#ifndef LISP_COMMAND_LOOP
  defsubr (&Srecursive_edit);
#endif
  defsubr (&Scommand_loop_1);
  defsubr (&Scommand_execute);

  defsubr (&Srecent_keys);
  defsubr (&Ssuspend_emacs);
  defsubr (&Sopen_dribble_file);
  defsubr (&Sset_input_mode);
  defsubr (&Sset_interrupt_character);

  DEFVAR_INT ("command-loop-level", &command_loop_level,
    "Number of recursive edits in progress.");
  command_loop_level = 0;

  DEFVAR_LISP ("disabled-command-hook", &Vdisabled_command_hook,
    "Value is called instead of any command that is disabled,\n\
i.e. has a non-nil `disabled' property.");
  Vdisabled_command_hook = intern ("disabled-command-hook");

  DEFVAR_LISP ("last-command-event", &Vlast_command_event,
    "Last keyboard or mouse button event that was part of a command.  This\n\
variable is off limits: you may not set its value or modify the event that\n\
is its value, as it is destructively modified by `read-key-sequence'.  If\n\
you want to keep a pointer to this value, you must use `copy-event'.");
  Vlast_command_event = Qnil;

  DEFVAR_LISP ("last-command-char", &Vlast_command_char,
    "If the value of `last-command-event' is a keyboard event, then\n\
this is the nearest ASCII equivalent to it.  This the the value that\n\
`self-insert-command' will put in the buffer.  Remember that there is\n\
NOT a 1:1 mapping between keyboard events and ASCII characters: the set\n\
of keyboard events is much larger, so writing code that examines this\n\
variable to determine what key has been typed is bad practice, unless\n\
you are certain that it will be one of a small set of characters.");
  Vlast_command_char = Qnil;

  DEFVAR_LISP ("last-input-event", &Vlast_input_event,
    "Last keyboard or mouse button event recieved.  This variable is off\n\
limits: you may not set its value or modify the event that is its value, as\n\
it is destructively modified by `next-event'.  If you want to keep a pointer\n\
to this value, you must use `copy-event'.");
  Vlast_input_event = Qnil;

  DEFVAR_LISP ("last-input-char", &Vlast_input_char,
    "If the value of `last-input-event' is a keyboard event, then\n\
this is the nearest ASCII equivalent to it.  Remember that there is\n\
NOT a 1:1 mapping between keyboard events and ASCII characters: the set\n\
of keyboard events is much larger, so writing code that examines this\n\
variable to determine what key has been typed is bad practice, unless\n\
you are certain that it will be one of a small set of characters.");
  Vlast_input_char = Qnil;

  DEFVAR_LISP ("last-input-time", &Vlast_input_time,
    "The time (in seconds since Jan 1, 1970) of the last-command-event,\n\
represented as a cons of two 16-bit integers.  This is destructively\n\
modified, so copy it if you want to keep it.");
  Vlast_input_time = Qnil;

  DEFVAR_LISP ("unread-command-event", &Vunread_command_event,
    "Set this to an event object to simulate the reciept of an event from\n\
the user.  Normally this is nil.");
  Vunread_command_event = Qnil;

  DEFVAR_LISP ("last-command", &Vlast_command,
  "The last command executed.  Normally a symbol with a function definition,\n\
but can be whatever was found in the keymap, or whatever the variable\n\
`this-command' was set to by that command.");
  Vlast_command = Qnil;

  DEFVAR_LISP ("this-command", &Vthis_command,
    "The command now being executed.\n\
The command can set this variable; whatever is put here\n\
will be in `last-command' during the following command.");
  Vthis_command = Qnil;

  DEFVAR_LISP ("help-char", &help_char,
    "Character to recognize as meaning Help.\n\
When it is read, do `(eval help-form)', and display result if it's a string.\n\
If the value of `help-form' is nil, this char can be read normally.");
  XSET (help_char, Lisp_Int, 8); /* C-h */

  DEFVAR_INT ("interrupt-char", &interrupt_char,
    "Character which interrupts emacs.\n\
Do not setq this variable: use the function `set-interrupt-character' instead.\n\
Depending on the system you are on, this may need to do magic like changing\n\
interrupt handlers.");
  interrupt_char = 7; 		/* C-g */

  DEFVAR_LISP ("help-form", &Vhelp_form,
    "Form to execute when character help-char is read.\n\
If the form returns a string, that string is displayed.\n\
If `help-form' is nil, the help char is not recognized.");
  Vhelp_form = Qnil;

#ifndef LISP_COMMAND_LOOP
  DEFVAR_LISP ("top-level", &Vtop_level,
    "Form to evaluate when Emacs starts up.\n\
Useful to set before you dump a modified Emacs.");
  Vtop_level = Qnil;
#else
  DEFVAR_LISP ("command-loop", &Vcommand_loop,
    "Function or one argument to call to read and process keyboard commands.\n\
The passed argument specifies whether or not to handle errors.");
  Vcommand_loop = Qnil;
#endif /* LISP_COMMAND_LOOP */

  DEFVAR_LISP ("pre-command-hook", &Vpre_command_hook,
     "Function or functions to run before every command.\n\
This may examine the `this-command' variable to find out what command\n\
is about to be run, or may change it to cause a different command to run.\n\
Function on this hook must be careful to avoid signalling errors!");
  Vpre_command_hook = Qnil;
  defsymbol (&Qpre_command_hook, "pre-command-hook");

  DEFVAR_LISP ("post-command-hook", &Vpost_command_hook,
     "Function or functions to run after every command.\n\
This may examine the `this-command' variable to find out what command\n\
was just executed.");
  Vpost_command_hook = Qnil;
  defsymbol (&Qpost_command_hook, "post-command-hook");

  DEFVAR_LISP ("keyboard-translate-table", &Vkeyboard_translate_table,
    "String used as translate table for keyboard input, or nil.\n\
Each character is looked up in this string and the contents used instead.\n\
If string is of length N, character codes N and up are untranslated.\n\
This is the right thing to use only if you are on a dumb tty, as it cannot\n\
handle input which cannot be represented as ASCII.  If you are running emacs\n\
under X, you should do the translations with the `xmodmap' program instead.");
  Vkeyboard_translate_table = Qnil;
}
