/* The portable interface to event_streams.
   Copyright (C) 1991, 1992, 1993, 1994 Free Software Foundation, Inc.

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

/*
 *	DANGER!!
 *
 *	If you ever change ANYTHING in this file, you MUST run the
 *	testcases at the end to make sure that you haven't changed
 *	the semantics of recent-keys, last-input-char, or keyboard
 *	macros.  You'd be surprised how easy it is to break this.
 *
 */

#include "config.h"
#include "intl.h"

#include <stdio.h>              /* for "graceful exit" kludge */

#include "lisp.h"
#include "buffer.h"
#include "window.h"
#include "process.h"
#include "events.h"
#include "keymap.h"
#include "commands.h"
#include "macros.h"		/* for defining_keyboard_macro */

#include "systime.h"		/* to set Vlast_input_time */

/* The number of keystrokes between auto-saves. */
static int auto_save_interval;

Lisp_Object Qundefined; /* The symbol undefined; good a place as any... */
Lisp_Object Qundefined_keystroke_sequence;

Lisp_Object Qcommand_execute;

static void pre_command_hook (void);
static void post_command_hook (int old_region_p);


/* The callback routines for the window system or terminal driver */
struct event_stream *event_stream;

/* This structure is what we use to excapsulate the state of a command sequence
   being composed; key events are executed by adding themselves to the command
   builder; if the command builder is then complete (does not still represent
   a prefix key sequence) it executes the corresponding command.
 */
struct command_builder {
  /* Qnil, or a Lisp_Event representing the first event event read
   *  after the last command completed.  Threaded. */
  /* >>>> NYI */
  Lisp_Object prefix_events;
  /* Qnil, or a Lisp_Event representing event in the current 
   *  keymap-lookup sequence.  Subsequent events are threaded via
   *  the event's next slot */
  Lisp_Object current_events;
  /* Last elt of above  */
  Lisp_Object most_current_event;

  char *echo_buf;
  int echo_buf_length;          /* size of echo_buf */
  int echo_buf_index;           /* index into echo_buf */
  int echo_previous_index;      /* for disgusting ESC => meta kludge */
};

static struct command_builder *the_command_builder;

static void echo_char_event (struct command_builder *, Lisp_Object event);
static void maybe_echo_keys (struct command_builder *);

/* This structure is basically a typeahead queue: things like
   wait-reading-process-output will delay the execution of
   keyboard and mouse events by pushing them here.

   Chained through event_next()
   command_event_queue_tail is a pointer to the last-added element.
 */
static Lisp_Object command_event_queue;
static struct Lisp_Event *command_event_queue_tail;



/* The number of keystrokes since the last auto-save. */
static int keystrokes_since_auto_save;

#define	max(a,b) ((a)>(b)?(a):(b))


/*
 * character prompting
 */

static void
echo_char_event (struct command_builder *command_builder,
                 Lisp_Object event)
{
  char buf[255];
  int index = command_builder->echo_buf_index;
  char *e;
  int len;

  command_builder->echo_previous_index = index;
  format_event_object (buf, XEVENT (event), 1);
  len = strlen (buf);
  
  if (len + index + 5 > command_builder->echo_buf_length)
    return;
  e = command_builder->echo_buf + index;
  memcpy (e, buf, len);
  e += len;

  e[0] = ' ';
  e[1] = '-';
  e[2] = ' ';
  e[3] = 0;

  command_builder->echo_buf_index = index + len + 1;
}

static int echo_keystrokes;

static void
maybe_echo_keys (struct command_builder *command_builder)
{
  /* Message turns off echoing unless more keystrokes turn it on again. */
  if (echo_area_glyphs && *echo_area_glyphs &&
      echo_area_glyphs != command_builder->echo_buf)
    return;

  if (minibuf_level == 0 
      && echo_keystrokes > 0 
      && !NILP (Fsit_for (make_number (echo_keystrokes), Qnil)))
  {
    /* This is like calling message(), but trickier */
    echo_area_glyphs = command_builder->echo_buf;
  }
}



#if 0
static void
maybe_kbd_translate (event)
     Lisp_Object event;
{
  int c;
  if (!STRINGP (Vkeyboard_translate_table)) return;
  c = event_to_character (XEVENT (event), 0);
  if (c == -1) return;
  if (XSTRING (Vkeyboard_translate_table)->size <= c) return;
  c = XSTRING (Vkeyboard_translate_table)->data[c];
  Fcharacter_to_event (make_number (c), event);
}
#endif

void
record_auto_save ()
{
  keystrokes_since_auto_save = 0;
}
  

static void
maybe_do_auto_save ()
{
  keystrokes_since_auto_save++;
  if (auto_save_interval > 0 &&
      keystrokes_since_auto_save > max (auto_save_interval, 20) &&
      !detect_input_pending ())
    {
      Fdo_auto_save (Qnil, Qnil);
      record_auto_save ();
    }
}


static Lisp_Object
print_help (object)
     Lisp_Object object;
{
  Fprinc (object, Qnil);
  return Qnil;
}


static void
execute_help_form (struct command_builder *command_builder,
                   Lisp_Object event)
{
  Lisp_Object tem0;
  int speccount = specpdl_depth ();
  int kludge = command_builder->echo_previous_index;
  Lisp_Object echo = make_string (command_builder->echo_buf,
                                  command_builder->echo_buf_index);
  struct gcpro gcpro1;
  GCPRO1 (echo);

  record_unwind_protect (Fset_window_configuration,
			 Fcurrent_window_configuration (Qnil));
  command_builder->echo_buf_index = 0;
  command_builder->echo_previous_index = 0;
  command_builder->echo_buf[0] = 0;

  tem0 = Feval (Vhelp_form);
  if (STRINGP (tem0))
    internal_with_output_to_temp_buffer (GETTEXT ("*Help*"),
					 print_help, tem0, Qnil);

  Fnext_command_event (event);

  /* Remove the help from the screen */
  unbind_to (speccount, Qnil);
  redisplay ();
  if (event_to_character (XEVENT (event), 0, 0, 0) == ' ')
    {
      command_builder->echo_buf_index = 0;
      command_builder->echo_previous_index = 0;
      command_builder->echo_buf[0] = 0;
      Fnext_command_event (event);
    }
  command_builder->echo_buf_index = XSTRING (echo)->size;
  command_builder->echo_previous_index = kludge;
  memcpy (command_builder->echo_buf,
          XSTRING (echo)->data, XSTRING (echo)->size);
  UNGCPRO;
}


int
detect_input_pending ()
{
  /* Always call the event_pending_p hook even if there's an unread
     character, because that might do some needed ^G detection (on
     systems without SIGIO, for example).
   */
  if (event_stream && event_stream->event_pending_p (1))
    return 1;
  if (!NILP (Vunread_command_event))
    return 1;
  if (!NILP (command_event_queue))
    {
      struct Lisp_Event *e;
      for (e = XEVENT (command_event_queue);
           e;
           e = event_next (e))
      {
        switch (e->event_type)
	  {
	  case eval_event:
	    break;
	  default:
	    return (1);
	  }
      }
    }
  return 0;
}

int 
check_sigio ()		/* called from QUIT; */
{
  if (sigio_happened)
    {
      sigio_happened = 0;
      event_stream->quit_p_cb ();
      return 1;
    }
  else
    return 0;
}

DEFUN ("input-pending-p", Finput_pending_p, Sinput_pending_p, 0, 0, 0,
  "T if command input is currently available with no waiting.\n\
Actually, the value is nil only if we can be sure that no input is available.")
  ()
{
  return (detect_input_pending() ? Qt : Qnil);
}

/* Add an event to the back of the queue: it will be the next event read after
   all pending events.   This only works on keyboard, mouse-click, menu, and
   eval events.
 */
void
enqueue_command_event (event)
     Lisp_Object event;
{
  struct Lisp_Event *e = XEVENT (event);
  if (event_next (e))
    abort ();
  if (command_event_queue_tail
      && command_event_queue_tail == e)
    abort();

  if (command_event_queue_tail)
    set_event_next (command_event_queue_tail, e);
  else
    command_event_queue = event;
  command_event_queue_tail = e;
  
  if (e == event_next (e))
    abort ();
}


DEFUN ("enqueue-eval-event", Fenqueue_command_event, Senqueue_command_event,
       2, 2, 0, 
       "Add an eval event to the back of the queue.\n\
The eval-event will be the next event read after all pending events.")
  (function, object)
Lisp_Object function, object;
{
  Lisp_Object event;

  event = Fallocate_event ();

  XEVENT (event)->event_type = eval_event;
  XEVENT (event)->event.eval.function = function;
  XEVENT (event)->event.eval.object = object;
  enqueue_command_event (event);

  return event;
}


/* the number of keyboard characters read.  callint.c wants this. 
 */
int num_input_chars;

void
lose_without_x ()
{
  fputs (GETTEXT ("\n\n\
emacs: this version of emacs only works under X for now;\n\
check that your $DISPLAY environment variable is properly set.\n"),
	 stderr);
  Fkill_emacs (make_number (69));
}

static void
next_event_internal (Lisp_Object target_event, int allow_queued)
{
  Lisp_Object event;

  if (event_next (XEVENT (target_event)))
    abort ();

  if (allow_queued && !NILP (command_event_queue))
    {
      struct Lisp_Event *e = XEVENT (command_event_queue);
      XSETR (event, Lisp_Event, e);
      if (!event_next (e))
      {
        command_event_queue_tail = 0;
        command_event_queue = Qnil;
      }
      else
      {
        XSETR (command_event_queue, Lisp_Event, event_next (e));
      }
      set_event_next (e, 0);
    }
  else
    {
      struct Lisp_Event *e;

      /* #### Temporary hack to make emacs exit "gracefully" when it tries
	 to run in tty-mode for some reason.  This is the latest time we
	 can make this check, because we're about to deref event_stream.
       */
      if (! event_stream) lose_without_x ();

      /* The command_event_queue was empty.  Wait for an event. */
      event = Fallocate_event ();
      e = XEVENT (event);
      event_stream->next_event_cb (e);
      if (e->event_type == key_press_event &&
	  event_to_character (e, 0, 0, 0) == interrupt_char)
	interrupt_signal (0);
    }

  Fcopy_event (event, target_event);
  Fdeallocate_event (event);
  return;
}

static void push_this_command_keys (Lisp_Object event);
static void push_recent_keys (Lisp_Object event);


DEFUN ("next-event", Fnext_event_1, Snext_event, 0, 2, 0,
 "Returns the next available event from the window system or terminal driver.\n\
Pass this object to dispatch-event to handle it.  See also the function\n\
next-command-event, which is often more appropriate.\n\
If an event object is supplied, it is filled in and returned, otherwise a\n\
new event object will be created.")
     (event, prompt)
     Lisp_Object event, prompt;
{
  struct gcpro gcpro1;
  int store_this_key = 0;

  /* I think this is necessary because we store new objects into this var. */
  GCPRO1 (event);

  if (NILP (event))
    event = Fallocate_event ();
  else
    CHECK_EVENT (event, 0);

  if (XEVENT (event)->event_type == dead_event)
    error (GETTEXT ("next-event called with a deallocated event!"));

  if (!NILP (prompt))
    {
      int len;
      CHECK_STRING (prompt, 1);

      len = XSTRING (prompt)->size;
      if (the_command_builder->echo_buf_length < len)
	len = the_command_builder->echo_buf_length - 1;
      memcpy (the_command_builder->echo_buf, XSTRING (prompt)->data, len);
      the_command_builder->echo_buf[len] = 0;
      the_command_builder->echo_buf_index = len;
      echo_area_glyphs = the_command_builder->echo_buf;
      /* redundant: maybe_echo_keys (the_command_builder); */
    }

  if (!noninteractive && !detect_input_pending () && NILP (Vexecuting_macro))
    redisplay ();

  /* If there is an unread-command-event, simply return it.
     But do some error checking to make sure the user hasn't put something
     in the unread-command-event that they shouldn't have.
     This does not update this-command-keys and recent-keys.
   */
  if (!NILP (Vunread_command_event))
    {
      Lisp_Object e = Vunread_command_event;
      Vunread_command_event = Qnil;

      if (!EVENTP (e))
	{
	bogus_unread_event:
	  signal_error (Qwrong_type_argument,
			list3 (Qeventp, e, intern ("unread-command-event")));
	}
      switch (XEVENT (e)->event_type)
	{
	case key_press_event:
	case button_press_event:
	case button_release_event:
	case menu_event:
	  if (!EQ (e, event))
	    Fcopy_event (e, event);
	  break;
	default:
	  goto bogus_unread_event;
	}
    }
  /* If we're executing a keyboard macro, take the next event from that,
     and update this-command-keys and recent-keys.
     Note that the unread-command-event takes precedence over kbd macros.
   */
  else if (!NILP (Vexecuting_macro))
    {
      pop_kbd_macro_event (event);  /* This throws past us at end-of-macro. */
      store_this_key = 1;
    }
  /* Otherwise, read a real event, possibly from the command_event_queue,
     and update this-command-keys and recent-keys.
   */
  else
    {
      next_event_internal (event, 1);
      store_this_key = 1;
    }

  status_notify ();             /* Notice process change */

#ifdef C_ALLOCA
      alloca (0);		/* Cause a garbage collection now */
				/* Since we can free the most stuff here.  */
#endif /* C_ALLOCA */

  switch (XEVENT (event)->event_type)
    {
    case key_press_event:	/* any key input can trigger autosave */
      maybe_do_auto_save ();
      num_input_chars++;
      /* fall through */
    case button_press_event:	/* key or mouse input can trigger prompting */
      if (store_this_key)
	echo_char_event (the_command_builder, event);
      /* fall through */
    case button_release_event:
    case menu_event:

      /* Store the last-input-event.  The semantics of this is that it is
	 the thing most recently returned by next-command-event.  It need
	 not have come from the keyboard or a keyboard macro, it may have
	 come from unread-command-event.  It's always a command-event, (a
	 key, click, or menu selection) never a motion or process event.
       */
      if (!EVENTP (Vlast_input_event))
	Vlast_input_event = Fallocate_event ();
      if (XEVENT (Vlast_input_event)->event_type == dead_event)
	{
	  Vlast_input_event = Fallocate_event ();
	  error (GETTEXT ("Someone deallocated last-input-event!"));
	}
      if (! EQ (event, Vlast_input_event))
	Fcopy_event (event, Vlast_input_event);
      
      /* last-input-char and last-input-time are derived from last-input-event.
	 Note that last-input-char will never have its high-bit set, in an
	 effort to sidestep the ambiguity between M-x and oslash.
       */
      Vlast_input_char = Fevent_to_character (Vlast_input_event,
					      Qnil, Qnil, Qnil);
      {
	EMACS_TIME t;
	EMACS_GET_TIME (t);
	if (!CONSP (Vlast_input_time))
	  Vlast_input_time = Fcons (Qnil, Qnil);
	XCONS (Vlast_input_time)->car
	  = make_number ((EMACS_SECS (t) >> 16) & 0xffff);
	XCONS (Vlast_input_time)->cdr
	  = make_number ((EMACS_SECS (t) >> 0)  & 0xffff);
      }

      /* If this key came from the keyboard or from a keyboard macro, then
	 it goes into the recent-keys and this-command-keys vectors.
	 If this key came from the keyboard, and we're defining a keyboard
	 macro, then it goes into the macro.
       */
      if (store_this_key)
	{
          push_this_command_keys (event);
          push_recent_keys (event);
	  if (defining_kbd_macro && NILP (Vexecuting_macro))
	    {
	      if (!EVENTP (the_command_builder->current_events))
		finalize_kbd_macro_chars ();
	      store_kbd_macro_event (event);
	    }
	}
    default:
      break;
    }

  /* If this is the help char and there is a help form, then execute the
     help form and swallow this character.  This is the only place where
     calling Fnext_event() can cause arbitrary lisp code to run.  Note
     that execute_help_form() calls Fnext_command_event(), which calls
     this function, as well as Fdispatch_event.
   */
  if (!NILP (Vhelp_form)
      && XEVENT (event)->event_type == key_press_event
      /* >>> Fix this */
      && EQ (help_char,
	     make_number (event_to_character (XEVENT (event), 0, 0, 0))))
    execute_help_form (the_command_builder, event);

  UNGCPRO;
  return event;
}


DEFUN ("next-command-event", Fnext_command_event, Snext_command_event, 0, 1, 0,
"Returns the next available \"user\" event from the window system or terminal\n\
driver.  Pass this object to dispatch-event to handle it.  If an event object\n\
is supplied, it is filled in and returned, otherwise a new event object will\n\
be created.\n\
\n\
The event returned will be a keyboard, mouse press, or mouse release event.\n\
If there are non-command events available (mouse motion, sub-process output,\n\
etc) then these will be executed (with `dispatch-event') and discarded.  This\n\
function is provided as a convenience; it is equivalent to the lisp code\n\
\n\
	(while (progn\n\
		 (next-event event)\n\
	         (not (or (key-press-event-p event)\n\
	                  (button-press-event-p event)\n\
	                  (button-release-event-p event)\n\
	                  (menu-event-p event))))\n\
	   (dispatch-event event))\n")
     (event)
    Lisp_Object event;
{
  struct gcpro gcpro1;
  /* I think this is necessary because we store new objects into this var. */
  GCPRO1 (event);
  maybe_echo_keys (the_command_builder);  /* ## This sucks bigtime */
  while (1)
    {
      event = Fnext_event_1 (event, Qnil);
      switch (XEVENT (event)->event_type)
	{
	case key_press_event:
	case button_press_event:
	case button_release_event:
	case menu_event:
	  goto done;
	default:
	  Fdispatch_event (event);
	}
    }
 done:
  UNGCPRO;
  return event;
}


static void
nuke_current_events (struct command_builder *command_builder)
{
  Lisp_Object event = command_builder->current_events;
  command_builder->current_events = Qnil;
  command_builder->most_current_event = Qnil;
  if (EVENTP (event))
    {
      for (;;)
	{
	  struct Lisp_Event *e = event_next (XEVENT (event));
	  Fdeallocate_event (event);
	  if (e == 0)
	    break;
	  XSETR (event, Lisp_Event, e);
	}
    }
}


DEFUN ("discard-input", Fdiscard_input, Sdiscard_input, 0, 0, 0,
  "Discard the contents of the terminal input buffer.\n\
Also cancel any kbd macro being defined.")
  ()
{
  /* This throws away user-input on the queue, but doesn't process any
     events.  Calling Fdispatch_event() here leads to a race condition.
   */
  Lisp_Object event, event2;
  struct Lisp_Event *e, *e2, *head, *tail;
  Lisp_Object oiq = Vinhibit_quit;

  Vinhibit_quit = Qt;
  defining_kbd_macro = 0;
  nuke_current_events (the_command_builder);
  event = Fallocate_event ();
  e = XEVENT (event);
  tail = 0;

  while (!NILP (command_event_queue)
         || (event_stream && event_stream->event_pending_p (1)))
    {
      /* This will take stuff off the command_event_queue, or read it
	 from the event_stream, but it will not block.
       */
      next_event_internal (event, 1);

      /* If the event is a user event, ignore it.
       */
      if (e->event_type == key_press_event ||
	  e->event_type == button_press_event ||
	  e->event_type == button_release_event ||
	  e->event_type == menu_event)
	continue;

      /* Otherwise, chain the event onto our list of events not to ignore,
	 and keep reading until the queue is empty.  This does not mean
	 that if a subprocess is generating an infinite amount of output,
	 we will never terminate, because this loop ends as soon as there
	 are no more user events on the command_event_queue or event_stream.
       */
      event2 = Fcopy_event (event, Qnil);
      e2 = XEVENT (event2);
      if (tail)
	set_event_next (tail, e2);
      else
	head = e2;
      tail = e2;
    }

  if (!NILP (command_event_queue) || command_event_queue_tail)
    abort ();

  /* Now tack our chain of events back on to the front of the queue.
     Actually, since the queue is now drained, we can just replace it.
     The effect of this will be that we have deleted all user events
     from the input stream without changing the relative ordering of
     any other events.  (Some events may have been taken from the
     event_stream and added to the command_event_queue, however.)

     At this time, the command_event_queue will contain only eval_events.
   */
  if (tail)
    {
      XSETR (command_event_queue, Lisp_Event, head);
      command_event_queue_tail = tail;
    }

  Fdeallocate_event (event);

  Vinhibit_quit = oiq;
  return Qnil;
}


static unsigned long lisp_number_to_milliseconds (Lisp_Object secs,
						  int allow_0);

/* #### Is (accept-process-output nil 3) supposed to be like (sleep-for 3)?
 */

DEFUN ("accept-process-output", Faccept_process_output, Saccept_process_output,
       0, 3, 0,
  "Allow any pending output from subprocesses to be read by Emacs.\n\
It is read into the process' buffers or given to their filter functions.\n\
Non-nil arg PROCESS means do not return until some output has been received\n\
 from PROCESS.\n\
If the second arg is non-nil, it is the maximum number of seconds to wait:\n\
 this function will return after that much time even if no input has arrived\n\
 from PROCESS.  This argument may be a float, meaning wait some fractional\n\
 part of a second.\n\
If the third arg is non-nil, it is a number of microseconds that is added\n\
 to the first arg.  (This exists only for compatibility.)\n\
Return non-nil ifn we received any output before the timeout expired.")
     (proc, timeout_secs, timeout_msecs)
     register Lisp_Object proc, timeout_secs, timeout_msecs;
{
  struct gcpro gcpro1;
  Lisp_Object event = Qnil;
  int timeout_id = 0;
  Lisp_Object result = Qnil;

  if (!NILP (proc))
    CHECK_PROCESS (proc, 0);

  GCPRO1 (event);

  if (!NILP (proc) && (!NILP (timeout_secs) || !NILP (timeout_msecs)))
    {
      unsigned long msecs = 0;
      if (!NILP (timeout_secs))
	msecs = lisp_number_to_milliseconds (timeout_secs, 1);
      if (!NILP (timeout_msecs))
	{
	  CHECK_NATNUM (timeout_msecs, 0);
	  msecs += XINT (timeout_msecs);
	}
      if (msecs)
	timeout_id = event_stream->generate_wakeup_cb (msecs, 0, Qnil, Qnil);
    }

  event = Fallocate_event ();

  while (!NILP (proc) ||
	 /* Calling detect_input_pending() is the wrong thing here, because
	    that considers the Vunread_command_event and command_event_queue.
	    We don't need to look at the command_event_queue because we are
	    only interested in process events, which don't go on that.  In
	    fact, we can't read from it anyway, because we put stuff on it.

	    Note that event_stream->event_pending_p must be called in such
	    a way that it says whether any events *of any kind* are ready,
	    not just user events, or (accept-process-output nil) will fail
	    to dispatch any process events that may be on the queue.  It is
	    not clear to me that this is important, because the top-level
	    loop will process it, and I don't think that there is ever a
	    time when one calls accept-process-output with a nil argument
	    and really need the processes to be handled.
	  */
	 event_stream->event_pending_p (0))
    {
      QUIT;
      next_event_internal (event, 0);
      switch (XEVENT (event)->event_type)
	{
	case process_event:
	  {
	    if (EQ (XEVENT (event)->event.process.process, proc))
	      {
		proc = Qnil;
		/* RMS's version always returns nil when proc is nil,
		   and only returns t if input ever arrived on proc. */
		result = Qt;
	      }

	    Fdispatch_event (event);

	    /* We must call status_notify here to allow the
	       event_stream->unselect_process_cb to be run if appropriate.
	       Otherwise, dead fds may be selected for, and we will get a
	       continuous stream of process events for them.  Since we don't
	       return until all process events have been flushed, we would
	       get stuck here, processing events on a process whose status
	       was 'exit.  Call this after dispatch-event, or the fds will
	       have been closed before we read the last data from them.
	       It's safe for the filter to signal an error because
	       status_notify() will be called on return to top-level.
	     */
	    status_notify ();
	    break;
	  }
	case timeout_event:
	  {
	    if (XEVENT (event)->event.timeout.id_number == timeout_id)
	      {
		timeout_id = 0;
		proc = Qnil;  /* we're done */
	      }
	    else	      /* a timeout that wasn't one we're waiting for */
	      Fdispatch_event (event);
	    break;
	  }
	case pointer_motion_event:
	case magic_event:
	  Fdispatch_event (event);
	  break;
	default:
	  enqueue_command_event (Fcopy_event (event, Qnil));
	}
    }

  /* If our timeout has not been signalled yet, disable it. */
  if (timeout_id)
    event_stream->disable_wakeup_cb (timeout_id);

  Fdeallocate_event (event);
  UNGCPRO;
  return result;
}


/* sit-for, sleep-for, and timeouts.
 */

static unsigned long
lisp_number_to_milliseconds (Lisp_Object secs, int allow_0)
{
  unsigned long msecs;
#ifdef LISP_FLOAT_TYPE
  double fsecs;
  CHECK_NUMBER (secs, 0);
  fsecs = XFLOATINT (secs);
#else
  long fsecs;
  CHECK_NUMBER (secs, 0);
  fsecs = XINT (secs);
#endif
  msecs = 1000 * fsecs;
  if (fsecs < 0)
    signal_error (Qerror, list2 (build_string (GETTEXT("timeout is negative")),
				 secs));
  if (!allow_0 && fsecs == 0)
    signal_error (Qerror, list2 (build_string
				 (GETTEXT ("timeout is non-positive")),
				 secs));
  if (fsecs >= (((unsigned int) 0xFFFFFFFF) / 1000))
    signal_error (Qerror, list2 (build_string (GETTEXT (
             "timeout would exceed 32 bits when represented in milliseconds")),
			    secs));
  return msecs;
}


DEFUN ("sleep-for", Fsleep_for, Ssleep_for, 1, 1, 0,
  "Pause, without updating display, for ARG seconds.\n\
ARG may be a float, meaning pause for some fractional part of a second.")
  (seconds)
     Lisp_Object seconds;
{
  unsigned long msecs = lisp_number_to_milliseconds (seconds, 1);
  int id;
  Lisp_Object event = Qnil;
  struct gcpro gcpro1;

  if (!event_stream)
    return Qnil;

  GCPRO1 (event);

  id = event_stream->generate_wakeup_cb (msecs, 0, Qnil, Qnil);
  event = Fallocate_event ();
  while (1)
    {
      QUIT;
      /* We're a generator of the command_event_queue, so we can't be a
	 consumer as well.  We don't care about command and eval-events
	 anyway.
       */
      next_event_internal (event, 0);	/* blocks */
      switch (XEVENT (event)->event_type)
	{
	case timeout_event:
	  if (XEVENT (event)->event.timeout.id_number == id)
	    goto DONE;
	  /* else fall through */
	case pointer_motion_event:
	case process_event:
	case magic_event:
	  Fdispatch_event (event);
	  break;
	default:
	  enqueue_command_event (Fcopy_event (event, Qnil));
	}
    }
 DONE:
  Fdeallocate_event (event);
  UNGCPRO;
  return Qnil;
}


DEFUN ("sit-for", Fsit_for, Ssit_for, 1, 2, 0,
  "Perform redisplay, then wait for ARG seconds or until user input is\n\
available.  ARG may be a float, meaning a fractional part of a second.\n\
Optional second arg non-nil means don't redisplay, just wait for input.\n\
Redisplay is preempted as always if user input arrives, and does not\n\
happen if input is available before it starts.\n\
Value is t if waited the full time with no input arriving.")
  (seconds, nodisp)
     Lisp_Object seconds, nodisp;
{
  unsigned long msecs = lisp_number_to_milliseconds (seconds, 1);
  Lisp_Object event, result;
  struct Lisp_Event *e;
  struct gcpro gcpro1;
  int id;

  /* The unread-command-event counts as pending input */
  if (!NILP (Vunread_command_event))
    return Qnil;

  /* If the command-builder already has user-input on it (not eval events)
     then that means we're done too.
   */
  if (!NILP (command_event_queue))
    {
      for (e = XEVENT (command_event_queue); e; e = event_next (e))
	{
	  if (e->event_type == key_press_event ||
	      e->event_type == button_press_event ||
	      e->event_type == button_release_event ||
	      e->event_type == menu_event)
	    return Qnil;
	}
    }

  /* If we're in a macro, or noninteractive, or early in temacs, then
     don't wait. */
  if (noninteractive || !NILP (Vexecuting_macro) || !event_stream)
    return Qt;

  /* Otherwise, start reading events from the event_stream.
     Do this loop at least once even if (sit-for 0) so that we
     redisplay when no input pending.
   */
  event = Fallocate_event ();
  GCPRO1 (event);

  if (msecs <= 0)
    id = 0;
  else
    id = event_stream->generate_wakeup_cb (msecs, 0, Qnil, Qnil);

  while (1)
    {
      /* If there is no user input pending, then redisplay.
       */
      if (!event_stream->event_pending_p (1) && NILP (nodisp))
	redisplay_preserving_echo_area ();

      /* If we're no longer waiting for a timeout, bug out. */
      if (! id)
	{
	  result = Qt;
	  goto DONE;
	}

      QUIT;
      /* We're a generator of the command_event_queue, so we can't be a
	 consumer as well.  In fact, we know there's nothing on the
	 command_event_queue that we didn't just put there.
       */
      next_event_internal (event, 0);	/* blocks */

      switch (XEVENT (event)->event_type)
	{
	case key_press_event:
	case button_press_event:
	case button_release_event:
	case menu_event:
	  result = Qnil;
	  goto DONE;

	case eval_event:
	  /* eval-events get delayed until later. */
	  enqueue_command_event (Fcopy_event (event, Qnil));
	  break;

	case timeout_event:
	  if (XEVENT (event)->event.timeout.id_number == id)
	    {
	      id = 0;	/* assert that we are no longer waiting for it. */
	      result = Qt;
	      goto DONE;
	    }
	  else		/* a timeout that wasn't the one we're waiting for */
	    Fdispatch_event (event);
	  break;
	  
	case process_event:
	case pointer_motion_event:
	case magic_event:
	  Fdispatch_event (event);
	  break;

	default:
	  abort ();
	}
    }

 DONE:
  /* If our timeout has not been signalled yet, disable it. */
  if (id)
    event_stream->disable_wakeup_cb (id);

  /* Put back the event (if any) that made Fsit_for() exit before the
     timeout.  Note that it is being added to the back of the queue, which
     would be inappropriate if there were any user events on the queue
     already: we would be misordering them.  But we know that there are
     no user-events on the queue, or else we would not have reached this
     point at all.
   */
  if (NILP (result))
    enqueue_command_event (event);
  else
    Fdeallocate_event (event);
  UNGCPRO;
  return result;
}


DEFUN ("add-timeout", Fadd_timeout, Sadd_timeout, 3, 4, 0,
 "SECS is a number of seconds, expressed as an integer or a float.\n\
FUNCTION will be called after that many seconds have elapsed, with one\n\
argument, the given OBJECT.  If the optional RESIGNAL argument is provided,\n\
then after this timeout expires, `add-timeout' will automatically be called\n\
again with RESIGNAL as the first argument.\n\
\n\
This function returns an object which is the `id' of this particular timeout.\n\
You can pass that object to `disable-timeout' to turn off the timeout before\n\
it has been signalled.\n\
\n\
The number of seconds may be expressed as a floating-point number, in which\n\
case some fractional part of a second will be used.  Caveat: the usable\n\
timeout granularity will vary from system to system.\n\
\n\
Adding a timeout causes a timeout event to be returned by `next-event', and\n\
the function will be invoked by `dispatch-event,' so if emacs is in a tight\n\
loop, the function will not be invoked until the next call to sit-for or\n\
until the return to top-level (the same is true of process filters.)\n\
\n\
WARNING: if you are thinking of calling add-timeout from inside of a\n\
callback function as a way of resignalling a timeout, think again.  There\n\
is a race condition.  That's why the RESIGNAL argument exists.")
     (secs, function, object, resignal)
     Lisp_Object secs, function, object, resignal;
{
  unsigned long msecs = lisp_number_to_milliseconds (secs, 0);
  unsigned long msecs2 = (NILP (resignal) ? 0 :
			  lisp_number_to_milliseconds (resignal, 0));
  int id;
  Lisp_Object lid;
  if (noninteractive) error (GETTEXT ("can't add timeouts in batch mode"));
  if (! event_stream) abort ();
  id = event_stream->generate_wakeup_cb (msecs, msecs2, function, object);
  lid = make_number (id);
  if (id != XINT (lid)) abort ();
  return lid;
}


DEFUN ("disable-timeout", Fdisable_timeout, Sdisable_timeout, 1, 1, 0,
 "Given a timeout id number as returned by `add-timeout', this function\n\
will cause that timeout to not be signalled if it hasn't been already.")
     (id)
     Lisp_Object id;
{
  CHECK_FIXNUM (id, 0);
  if (noninteractive) error (GETTEXT ("can't use timeouts in batch mode"));
  if (! event_stream) abort ();
  event_stream->disable_wakeup_cb (XINT (id));
  return Qnil;
}


/* This handy little function is used by xselect.c and editorside.c to
   wait for replies from processes that aren't really processes (that is,
   the X server and the Energize server.)
 */
void
wait_delaying_user_input (int (*predicate) (void *arg), void *predicate_arg)
{
  Lisp_Object event = Fallocate_event ();
  struct gcpro gcpro1;
  GCPRO1 (event);

  while (!(*predicate) (predicate_arg))
    {
      QUIT;
      /* We're a generator of the command_event_queue, so we can't be a
	 consumer as well.  Also, we have no reason to consult the
	 command_event_queue; there are only user and eval-events there,
	 and we'd just have to put them back anyway.
       */
      next_event_internal (event, 0);
      switch (XEVENT (event)->event_type)
	{
	case key_press_event:
	case button_press_event:
	case button_release_event:
	case menu_event:
	case eval_event:
	  enqueue_command_event (Fcopy_event (event, Qnil));
	  break;
	default:
	  Fdispatch_event (event);
	}
    }
  UNGCPRO;
}



static Lisp_Object compose_command (struct command_builder *command_builder,
                                    Lisp_Object event, 
                                    int execute_p);

static void dispatch_command_event_internal (struct command_builder *,
                                             Lisp_Object event,
                                             Lisp_Object leaf);


/* If execute_p is true, this does what you expect: execute the event.
   But if events are keyboard or mouse events, they are executed by passing 
   them to the command builder, which doesn't actually run a command until
   a complete key sequence has been read.  So, if the execute_p arg is
   false, this returns the complete command sequence without executing it,
   once it has read one, and returns nil before then.
   This function is the guts of Fread_key_sequence and Fdispatch_event.
 */
static Lisp_Object 
dispatch_event_internal (struct command_builder *command_builder,
                         Lisp_Object event, 
                         int execute_p)
{
  switch (XEVENT (event)->event_type) 
  {
  case key_press_event:
  case button_press_event:
  case button_release_event:
    return (compose_command (command_builder, event, execute_p));

#ifdef I18N4
  case wchar_event:
    insert_wide_char (XEVENT(event)->event.wchar.data);
    return Qnil;
#endif

  case menu_event:
    {
      if (execute_p)
	{
	  dispatch_command_event_internal (command_builder, event, Qnil);
	  return (Qnil);
	}
      else
	{
	  /* If a menu item is selected while in the midst of
	     read-key-sequence, return that event, so that describe-key
	     works for menu items as well.
	     */
	  nuke_current_events (command_builder);
	  return (compose_command (command_builder, event, 0));
	}
    }
    
  case eval_event:
    {
      dispatch_command_event_internal (command_builder, event, Qnil);
      return (Qnil);
    }

  case pointer_motion_event:
    {
      if (!NILP (Vmouse_motion_handler))
        call1 (Vmouse_motion_handler, event);
      return (Qnil);
    }

  case process_event:
    {
      Lisp_Object p = XEVENT (event)->event.process.process;
      int infd, outfd;
      if (!PROCESSP (p)) abort ();
      get_process_file_descriptors (XPROCESS (p), &infd, &outfd);
      if (infd >= 0)
	{
	  if ((read_process_output (p, infd)) == 0)
	    {
	      /*
		 HACK.  Whatever command_channel_p is supposed to be,
		 it's now used to indicate whether this connection was
		 created with connect-to-file-descriptor.  When
		 connected to ToolTalk, it's not possible to reliably
		 determine whether there is a message waiting for
		 ToolTalk to receive.  ToolTalk expects to have
		 tt_message_receive() called exactly once every time
		 the file descriptor becomes active, so the filter
		 function forces this by returning 0.  Emacs must not
		 interpret this as EOF.

		 command_channel_p no longer exists as such so it has
		 been replaced with the function connected_via_file_desc.
		 */

	      if (network_connection_p (p) &&
		  !connected_via_file_desc(XPROCESS (p)))
		{
		  /* Deactivate network connection */
		  Lisp_Object status = Fprocess_status (p);
		  if (EQ (status, Qopen)
		      /* In case somebody changes the theory of whether to
			 return open as opposed to run for network connection
			 "processes"... */
		      || EQ (status, Qrun))
		    update_process_status (p, Qexit, 256, 0);
		  deactivate_process (p);
		}
	    }
          else
	    {
	      while (read_process_output (p, infd) > 0)
		;
	    }
	}
      return (Qnil);
    }
  case timeout_event:
    {
      struct Lisp_Event *e = XEVENT (event);
      if (!NILP (e->event.timeout.function))
        call1 (e->event.timeout.function, e->event.timeout.object);
      return (Qnil);
    }

  case magic_event:
    {
      event_stream->handle_magic_event_cb (XEVENT (event));
      return (Qnil);
    }

  default:
    abort();
  }
}


/* Compare the current state of the command builder against the local and
   global keymaps; if there is no match, try again, case-insensitively.
   The binding found (if any) is returned.
   It may be a command or a keymap if we're not done yet.
 */
static Lisp_Object
command_builder_find_leaf (struct command_builder *command_builder,
                           int allow_menu_events_p)
{
  Lisp_Object event0 = command_builder->current_events;
  Lisp_Object tem;
  struct Lisp_Event *terminal;

  if (NILP (event0))
    return (Qnil);

  if (!EVENTP (event0))
    abort ();

  if (allow_menu_events_p 
      && (event_next (XEVENT (event0)) == 0)
      && (XEVENT (event0)->event_type == menu_event))
    {
      Lisp_Object fn = XEVENT (event0)->event.eval.function;
      Lisp_Object arg = XEVENT (event0)->event.eval.object;
      return (list2 (fn, arg));
    }

  tem = event_binding (event0);
  if (!NILP (tem))
    return (tem);

  /* If we didn't find a binding, and the last event in the sequence is
     a shifted character, then try again with the lowercase version.
   */
  for (terminal = XEVENT (event0);
       event_next (terminal);
       terminal = event_next (terminal))
    ;

  if (terminal->event_type != key_press_event)
    return (Qnil);

  /* This behaviour should be under control of some variable.
     (Richard Mly hates it.) */
  if ((terminal->event.key.modifiers & MOD_SHIFT) ||
      (FIXNUMP (terminal->event.key.key)
       && XINT (terminal->event.key.key) >= 'A'
       && XINT (terminal->event.key.key) <= 'Z'))
      {
	struct Lisp_Event terminal_copy = *terminal;

	if (terminal->event.key.modifiers & MOD_SHIFT)
	  terminal->event.key.modifiers &= (~ MOD_SHIFT);
	else
	  terminal->event.key.key = make_number (XINT (terminal->event.key.key)
						 + 'a'-'A');

	tem = command_builder_find_leaf (command_builder, 
					  allow_menu_events_p);
	if (!NILP (tem))
	  return (tem);
	/* If there was no match with the lower-case version either, then
	   put back the upper-case event for the error message. */
	*terminal = terminal_copy;
      }
  return (Qnil);
}


/* Every time a command-event (a key, button, or menu selection) is read by
   Fnext_event(), it is stored in the recent_keys_ring, in Vlast_input_event,
   and in Vthis_command_keys.  (Eval-events are not stored there.)

   Every time a command is invoked, Vlast_command_event is set to the last
   event in the sequence.

   This means that Vthis_command_keys is really about "input read since the
   last command was executed" rather than about "what keys invoked this
   command."  This is a little counterintuitive, but that's the way it 
   has always worked.

   As an extra kink, the function read-key-sequence resets/updates the
   last-command-event and this-command-keys.  It doesn't append to the
   command-keys as read-char does.  Such are the pitfalls of having to
   maintain compatibility with a program for which the only specification
   is the code itself.

   (We could implement recent_keys_ring and Vthis_command_keys as the same
   data structure.)
 */

#define RECENT_KEYS_SIZE 100
Lisp_Object recent_keys_ring;
int recent_keys_ring_index;

/* An event (actually an event chain linked through event_next) of Qnil.
   This is stored reversed, with the most recent (copied) event as the
   head of the chain. */
static Lisp_Object Vthis_command_keys;

/* Vthis_command_keys having value Qnil means that the next time
   push_this_command_keys is called, it should start over.
   The times at which the the command-keys are reset
   (instead of merely being augumented) are pretty conterintuitive.
 */
Lisp_Object
reset_this_command_keys (Lisp_Object dummy)
{
  Lisp_Object e = Vthis_command_keys;

  if (NILP (e))
    return (Qnil);

  for (;;)
    {
      struct Lisp_Event *n = event_next (XEVENT (e));

      Fdeallocate_event (e);
      if (!n)
	{
	  Vthis_command_keys = Qnil;
	  return (Qnil);
	}
      XSETR (e, Lisp_Event, n);
    }
}

static void
push_this_command_keys (Lisp_Object event)
{
  Lisp_Object new = Fallocate_event ();

  Fcopy_event (event, new);
  set_event_next (XEVENT (new),
		  ((!NILP (Vthis_command_keys))
		   ? XEVENT (Vthis_command_keys)
		   : 0));
  Vthis_command_keys = new;
}

static void
push_recent_keys (Lisp_Object event)
{
  Lisp_Object e
    = XVECTOR (recent_keys_ring)->contents [recent_keys_ring_index];

  if (NILP (e))
    {
      e = Fallocate_event ();
      XVECTOR (recent_keys_ring)->contents [recent_keys_ring_index] = e;
    }
  Fcopy_event (event, e);
  if (++recent_keys_ring_index == XVECTOR (recent_keys_ring)->size)
    recent_keys_ring_index = 0;
}


/* Add the given event to the command builder, enlarging the vector 
   first if necessary.

   Extra hack: this also updates the recent_keys_ring and Vthis_command_keys
   vectors to translate "ESC x" to "M-x" (for any "x" of course.)
 */
static void
command_builder_push (struct command_builder *command_builder,
                      Lisp_Object event)
{
  Lisp_Object recent = command_builder->most_current_event;

  if (EVENTP (recent)
      && meta_prefix_char != -1 
      && event_to_character (XEVENT (recent), 0, 0, 0) == meta_prefix_char)
    {
      struct Lisp_Event *e;
      /* When we see a sequence like "ESC x", pretend we really saw "M-x".
         DoubleThink the recent-keys and this-command-keys as well. */

      /* Modify the previous most-recently-pushed event on the command
         builder to be a copy of this one with the meta-bit set instead of
         pushing a new event.
       */
      Fcopy_event (event, recent);
      e = XEVENT (recent);
      if (e->event_type == key_press_event)
	e->event.key.modifiers |= MOD_META;
      else if (e->event_type == button_press_event 
	       || e->event_type == button_release_event)
	e->event.button.modifiers |= MOD_META;
      else
	abort ();

      /* regenerate the final echo-glyph */
      command_builder->echo_buf_index = command_builder->echo_previous_index;
      echo_char_event (command_builder, recent);
      return;
    }

  event = Fcopy_event (event, Fallocate_event ());

  if (EVENTP (recent))
    set_event_next (XEVENT (recent), XEVENT (event));
  else
    command_builder->current_events = event;

  command_builder->most_current_event = event;
}

/* Self-insert-command is magic in that it doesn't always push an undo-
   boundary: up to 20 consecutive self-inserts can happen before an undo-
   boundary is pushed.  This variable is that counter.
 */
static int self_insert_countdown;

static Lisp_Object
current_events_into_vector (struct command_builder *command_builder)
{
  Lisp_Object vector;
  struct Lisp_Event *e;
  int n;

  if (!EVENTP (command_builder->current_events))
    abort ();
  for (e = XEVENT (command_builder->current_events), n = 0;
       e;
       e = event_next (e), n++)
    ;
  /* Copy the vector and the events in it. */
  /*  No need to copy the events, since they're already copies, and
      nobody other than the command-builder has pointers to them */
  vector = make_vector (n, Qnil);
  for (e = XEVENT (command_builder->current_events), n = 0;
       e;
       e = event_next (e), n++)
    XSETR (XVECTOR (vector)->contents[n], Lisp_Event, e);
  command_builder->current_events = Qnil;
  command_builder->most_current_event = Qnil;
  return (vector);
}


/* Add the given event to the command builder, and if that results in 
   a complete key sequence, either execute the corresponding command, or
   return the key sequence.
 */
static Lisp_Object
compose_command (struct command_builder *command_builder,
                 Lisp_Object event,
                 int execute_p)
{
  Lisp_Object leaf;

  /* #### This is done by dispatch_command_event_internal.  We can't
     do it twice, or both eag and peg get zeroed, and the echo area
     doesn't actually get erased. */
  clear_message (0);
  command_builder_push (command_builder, event);
  leaf = command_builder_find_leaf (command_builder, !execute_p);

  /* The suppress-keymap function binds keys to 'undefined - special-case
     that here, so that being bound to that has the same error-behavior as
     not being defined at all.
   */
  if (EQ (leaf, Qundefined))
    leaf = Qnil;

  /* If the leaf is a keymap, we're not done yet.  Return nil.
     Note that Fkeymapp() may autoload the keymap.
     (Need to gcpro leaf?  get_keymap() seems to do it.)
   */
  else if (!NILP (Fkeymapp (leaf)))
    {
      Lisp_Object prompt = Fkeymap_prompt (leaf, Qt);
      if (STRINGP (prompt))
	{
	  int index = command_builder->echo_buf_index;
	  int len = XSTRING (prompt)->size;
	  char *echo;

	  if (len + index + 1 <= command_builder->echo_buf_length)
	    {
	      echo = command_builder->echo_buf + index;
	      memcpy (echo, XSTRING (prompt)->data, len);
	      echo[len] = 0;
	    }
	}
      maybe_echo_keys (command_builder);
      return Qnil;
    }

  /* Now we've got a complete key sequence.  Execute it or return a vector
     of the events, depending on the value of execute_p.
   */
  if (!execute_p) 
    {
      return (current_events_into_vector (command_builder));
    }
  if (NILP (leaf))
    {
      /* At this point, we know that the sequence is not bound to a command.
	 Normally, we beep and print a message informing the user of this.
	 But we do not beep or print a message when:
	 
	 o  the last event in this sequence is a mouse-up event; or
	 o  the last event in this sequence is a mouse-down event and
	    there is a binding for the mouse-up version.
	 
	 That is, if the sequence ``C-x button1'' is typed, and is not bound
	 to a command, but the sequence ``C-x button1up'' is bound to a
	 command, we do not complain about the ``C-x button1'' sequence.  If
	 neither ``C-x button1'' nor ``C-x button1up'' is bound to a command,
	 then we complain about the ``C-x button1'' sequence, but later will
	 *not* complain about the ``C-x button1up'' sequence, which would be
	 redundant.
	 
	 This is pretty hairy, but I think it's the most intuitive behavior.
       */
      struct Lisp_Event *terminal
	= XEVENT (command_builder->most_current_event);

      if (terminal->event_type == button_press_event)
	{
	  int suppress_bitching;
	  Lisp_Object leaf2;
	  /* Temporarily pretend the last event was an "up" instead of a
	     "down", and look up its binding. */
	  terminal->event_type = button_release_event;
	  leaf2 = command_builder_find_leaf (command_builder, 0);
	  /* If the "up" version is bound, don't complain. */
	  suppress_bitching = !NILP (leaf2);
	  /* Undo the temporary changes we just made. */
	  terminal->event_type = button_press_event;
	  if (suppress_bitching)
	    {
	      /* Pretend this press was not seen (treat it as a prefix) */
	      struct Lisp_Event *e;
	      for (e = XEVENT (command_builder->current_events);
		   event_next (e) != terminal;
		   e = event_next (e))
		;
	      Fdeallocate_event (command_builder->most_current_event);
	      set_event_next (e, 0);
	      XSETR (command_builder->most_current_event, Lisp_Event, e);
	      maybe_echo_keys (command_builder);
	      return Qnil;
	    }
	}

      /* Complain that the typed sequence is not defined, if this is the
	 kind of sequence that warrants a complaint.
       */
      command_builder->echo_buf_index = 0;
      command_builder->echo_previous_index = 0;
      defining_kbd_macro = 0;
      Vprefix_arg = Qnil;
      /* Don't complain about undefined button-release events */
      if (terminal->event_type != button_release_event) 
	{
	  Lisp_Object keys = current_events_into_vector (command_builder);

	  /* Reset the command builder for reading the next sequence. */
	  nuke_current_events (command_builder);

	  /* Run the pre-command-hook before barfing about an undefined key. */
	  Vthis_command = Qnil;
	  pre_command_hook ();
	  /* Beep (but don't signal).  The post-command-hook doesn't run,
	     just as it wouldn't run if we had executed a real command which
	     signalled an error.  But maybe it should in this case.
	   */
#if 1
	  Fsignal (Qundefined_keystroke_sequence, list1 (keys));
#else
	  message (GETTEXT ("%s not defined."), /* doo dah, doo dah */
		   command_builder->echo_buf);
	  bitch_at_user (intern ("undefined-key"));
#endif
	}
      /* Reset the command builder for reading the next sequence. */
      nuke_current_events (command_builder);
      return Qnil;
    }
  dispatch_command_event_internal (command_builder, event, leaf);
  return Qnil;
}

static void store_last_command_event (struct command_builder *command_builder,
                                      Lisp_Object event);


static Lisp_Object
dispatch_command_event_unwind (Lisp_Object datum)
{
  if (!CONSP (datum))
    abort ();
  if (!NILP (XCONS (datum)->car))
    reset_this_command_keys (Qnil);
  free_cons (XCONS (datum));
  return (Qzero);
}


static void
dispatch_command_event_internal (struct command_builder *command_builder,
                                 Lisp_Object event,
                                 Lisp_Object leaf)
{
  enum emacs_event_type event_type = XEVENT (event)->event_type;

  Vthis_command = leaf;

  /* Don't push an undo boundary if the command set the prefix arg, or if we
     are executing a keyboard macro, or if in the minibuffer.  If the command
     we are about to execute is self-insert, it's tricky: up to 20 consecutive
     self-inserts may be done without an undo boundary.  This counter is reset
     as soon as a command other than self-insert-command is executed.
   */
  if (! EQ (leaf, Qself_insert_command))
    self_insert_countdown = 0;
  if (NILP (Vprefix_arg) &&
      NILP (Vexecuting_macro) &&
      !EQ (minibuf_window, selected_window) &&
      self_insert_countdown == 0)
    Fundo_boundary ();

  if (EQ (leaf, Qself_insert_command))
    if (--self_insert_countdown < 0)
      self_insert_countdown = 20;

  /* If we had been echoing keys, echo the last one (without the trailing
     dash) and redisplay before executing the command.
   */
  if (command_builder->echo_buf == echo_area_glyphs
      && event_type != eval_event)
    {
      command_builder->echo_buf[command_builder->echo_buf_index] = 0;
      maybe_echo_keys (command_builder);
      Fsit_for (Qzero, Qt);
    }

  if (event_type == key_press_event)
    Vcurrent_mouse_event = Qnil;
  else
    Vcurrent_mouse_event = event;

  if (event_type != eval_event)
    store_last_command_event (command_builder, event);

  {
    int old_region_p = zmacs_region_active_p;
    int old_kbd_macro = kbd_macro_end;
    int speccount = specpdl_depth ();
    int old_echo_buf_index = command_builder->echo_buf_index;
    int old_previous_index = command_builder->echo_previous_index;
    Lisp_Object locative = Fcons (Qt, Qnil);

    /* We're executing a new command, so the old value of this is irrelevant. */
    zmacs_region_stays = 0;

    /* Now we actually execute the command.
       If the command completes abnormally (signals an error, or does
       a throw past us) then we want Vthis_command_keys to get set to Qnil.
       Otherwise, we want it unchanged.
     */
    record_unwind_protect (dispatch_command_event_unwind, locative);
    command_builder->echo_buf_index = 0;
    command_builder->echo_previous_index = 0;

    if (event_type != eval_event)
      {
	/* Don't immediately display.
	 * Echo-area will be cleared for real if necessary when next
	 * redisplay happens 
	 */
	clear_message (0);      /* clear minibuffer echo-area */
	pre_command_hook ();
      }

    if (event_type == menu_event || event_type == eval_event)
      call1 (XEVENT (event)->event.eval.function, 
             XEVENT (event)->event.eval.object);
    else
#if 0
      call2 (Qcommand_execute, Vthis_command, Qnil);
#else
      Fcommand_execute (Vthis_command, Qnil);
#endif

    /* We completed normally -- don't do reset in unwind-protect */
    XCONS (locative)->car = Qnil;
    unbind_to (speccount, Qnil);

    if (event_type == eval_event)
      return;

    post_command_hook (old_region_p);

    if (!NILP (Vprefix_arg))
      {
	/* Commands that set the prefix arg don't update last-command, don't
	   reset the echoing state, and don't go into keyboard macros unless
	   followed by another command.
	 */
	command_builder->echo_buf_index = old_echo_buf_index;
	command_builder->echo_previous_index = old_previous_index;

	maybe_echo_keys (command_builder);

	/* If we're recording a keyboard macro, and the last command
	   executed set a prefix argument, then decrement the pointer to
	   the "last character really in the macro" to be just before this
	   command.  This is so that the ^U in "^U ^X )" doesn't go onto
	   the end of macro.
	 */
	if (defining_kbd_macro)
	  kbd_macro_end = old_kbd_macro;
      }
    else
      {
	/* Clear command-key echo, */
	/*   but if the command printed a message, don't lose it. */
	if (echo_area_glyphs == command_builder->echo_buf)
	  /* Don't immediately display.  Leads to too much flashing
	     (eg in isearch, where message is cleared and redisplayed
	     after every keystroke.)
	     Echo-area will be cleared for real if necessary when next
	     redisplay happens 
	   */
	  clear_message (0);      /* clear minibuffer echo-area */

	/* Start a new command next time */
	reset_this_command_keys (Qnil);
	Vlast_command = Vthis_command;
      }
  }
}


static void
store_last_command_event (struct command_builder *command_builder,
                          Lisp_Object event)
{
  nuke_current_events (command_builder);

  /* Store the last-command-event.  The semantics of this is that it is
     the last event most recently involved in command-lookup.
     */
  if (!EVENTP (Vlast_command_event))
    Vlast_command_event = Fallocate_event ();
  if (XEVENT (Vlast_command_event)->event_type == dead_event)
    {
      Vlast_command_event = Fallocate_event ();
      error (GETTEXT ("Someone deallocated the last-command-event!"));
    }

  if (! EQ (event, Vlast_command_event))
    Fcopy_event (event, Vlast_command_event);

  /* Note that last-command-char will never have its high-bit set, in
     an effort to sidestep the ambiguity between M-x and oslash.
   */
  Vlast_command_char = Fevent_to_character (Vlast_command_event,
					    Qnil, Qnil, Qnil);
}

static void
pre_command_hook ()
{
  if (!NILP (Vrun_hooks))
    call1 (Vrun_hooks, Qpre_command_hook);
}

static void
post_command_hook (old_region_p)
     int old_region_p;
{
  /* Turn off region highlighting unless this command requested that
     it be left on, or we're in the minibuffer.  We don't turn it off
     when we're in the minibuffer so that things like M-x write-region
     still work!

     This could be done via a function on the post-command-hook, but
     we don't want the user to accidentally remove it.
   */
  if (! zmacs_region_stays &&
      !EQ (minibuf_window, selected_window) &&
      (! zmacs_region_active_p ||
       ! (zmacs_region_active_p > old_region_p)))
    Fzmacs_deactivate_region ();
  else
    zmacs_update_region ();

  if (!NILP (Vrun_hooks))
    call1 (Vrun_hooks, Qpost_command_hook);

#if 0 /* RMSmacs */
  if (!NILP (current_buffer->mark_active))
    {
      if (!NILP (Vdeactivate_mark) && !NILP (Vtransient_mark_mode))
        {
          current_buffer->mark_active = Qnil;
          call1 (Vrun_hooks, intern ("deactivate-mark-hook"));
        }
      else if (current_buffer != prev_buffer || MODIFF != prev_modiff)
        call1 (Vrun_hooks, intern ("activate-mark-hook"));
    }
#endif /* RMSmacs */
}




DEFUN ("dispatch-event", Fdispatch_event, Sdispatch_event, 1, 1, 0,
  "Given an event object returned by next-event, execute it.")
     (event)
     Lisp_Object event;
{
  CHECK_EVENT (event, 0);
  if (XEVENT (event)->event_type == dead_event)
    error (GETTEXT ("dispatch-event called on a deallocated event!"));
  dispatch_event_internal (the_command_builder, event, 1);
  return Qnil;
}

DEFUN ("read-key-sequence", Fread_key_sequence, Sread_key_sequence, 1, 1, 0,
  "Read a sequence of keystrokes or mouse clicks and return a vector of the\n\
event objects read.  The vector and the event objects it contains are\n\
freshly created (and will not be side-effected by subsequent calls\n\
to this function.)\n\
\n\
The sequence read is sufficient to specify a non-prefix command starting\n\
from the current local and global keymaps.  A C-g typed while in this\n\
function is treated like any other character, and `quit-flag' is not set.\n\
\n\
First arg PROMPT is a prompt string.  If nil, do not prompt specially.\n\
\n\
If the user selects a menu item while we are prompting for a key-sequence,\n\
the returned value will be a vector of a single menu-selection event.\n\
An error will be signalled if you pass this value to `lookup-key' or a\n\
related function.")
  (prompt)
     Lisp_Object prompt;
{
  Lisp_Object result;
  Lisp_Object event = Fallocate_event ();
  int speccount = specpdl_depth ();
  struct gcpro gcpro1;
  GCPRO1 (event);

  if (!NILP (prompt))
    CHECK_STRING (prompt, 0);
  /* else prompt = Fkeymap_prompt (current_buffer->keymap); */
  QUIT;

  reset_this_command_keys (Qnil);

  specbind (Qinhibit_quit, Qt);
  result = dispatch_event_internal (the_command_builder,
                                    Fnext_event_1 (event, prompt), 0);
  while (NILP (result))
    result = dispatch_event_internal (the_command_builder,
                                      Fnext_event_1 (event, Qnil), 0);

  Vquit_flag = Qnil;  /* In case we read a ^G */
  Fdeallocate_event (event);
  UNGCPRO;
  return (unbind_to (speccount, result));
}

DEFUN ("this-command-keys", Fthis_command_keys, Sthis_command_keys, 0, 0, 0,
  "Returns a vector of the keyboard or mouse button events that were used\n\
to invoke this command.  This copies the vector and the events; it is safe\n\
to keep and modify them.")
   ()
{
  struct Lisp_Event *e;
  Lisp_Object result;
  int len;

  if (NILP (Vthis_command_keys))
    return (make_vector (0, Qnil));
  
  for (e = XEVENT (Vthis_command_keys), len = 0;
       e;
       e = event_next (e), len++)
    ;

  /* Vthis_command_keys is threaded in reverse-chronological order */
  result = make_vector (len, Qnil);
  for (e = XEVENT (Vthis_command_keys);
       e;
       e = event_next (e), len--)
  {
    Lisp_Object tem;
    XSETR (tem, Lisp_Event, e);
    XVECTOR (result)->contents[len - 1] = Fcopy_event (tem, Qnil);
  }
  return (result);
}


void
syms_of_event_stream ()
{
  the_command_builder
    = (struct command_builder *) xmalloc (sizeof (struct command_builder));
  the_command_builder->current_events = Qnil;
  the_command_builder->most_current_event = Qnil;
  the_command_builder->prefix_events = Qnil;
  the_command_builder->echo_buf_length = 300; /* >>> Kludge */
  the_command_builder->echo_buf = (char *) xmalloc (300);
  the_command_builder->echo_buf_index = 0;
  the_command_builder->echo_previous_index = 0;
  the_command_builder->echo_buf[0] = 0;

  staticpro (&the_command_builder->current_events);
  staticpro (&the_command_builder->prefix_events);

  defsymbol (&Qundefined_keystroke_sequence, "undefined-keystroke-sequence");
  pure_put (Qundefined_keystroke_sequence, Qerror_conditions,
            list2 (Qundefined_keystroke_sequence, Qerror));
  pure_put (Qundefined_keystroke_sequence, Qerror_message,
            build_string (DEFER_GETTEXT ("Undefined keystroke sequence")));

  recent_keys_ring_index = 0;
  recent_keys_ring = make_vector (RECENT_KEYS_SIZE, Qnil);
  staticpro (&recent_keys_ring);

  Vthis_command_keys = Qnil;
  staticpro (&Vthis_command_keys);

  num_input_chars = 0;
  self_insert_countdown = 0;
 
  defsymbol (&Qundefined, "undefined");
  defsymbol (&Qcommand_execute, "command-execute");

  command_event_queue = Qnil;
  staticpro (&command_event_queue);

  defsubr (&Sinput_pending_p);
  defsubr (&Senqueue_command_event);
  defsubr (&Snext_event);
  defsubr (&Snext_command_event);
  defsubr (&Sdiscard_input);
  defsubr (&Ssit_for);
  defsubr (&Ssleep_for);
  defsubr (&Saccept_process_output);
  defsubr (&Sadd_timeout);
  defsubr (&Sdisable_timeout);
  defsubr (&Sdispatch_event);
  defsubr (&Sread_key_sequence);
  defsubr (&Sthis_command_keys);

  DEFVAR_INT ("echo-keystrokes", &echo_keystrokes,
 "*Nonzero means echo unfinished commands after this many seconds of pause.");
  echo_keystrokes = 1;

  DEFVAR_INT ("auto-save-interval", &auto_save_interval,
    "*Number of keyboard input characters between auto-saves.\n\
Zero means disable autosaving due to number of characters typed.\n\
See also the variable `auto-save-timeout'.");
  auto_save_interval = 300;
}

/*
useful testcases for v18/v19 compatibility:

(defun foo ()
 (interactive)
 (setq unread-command-event (character-to-event ?A (allocate-event)))
 (setq x (list (read-char)
;	  (read-key-sequence "") ; try it with and without this
	  last-command-char last-input-char
	  (recent-keys) (this-command-keys))))
(global-set-key "\^Q" 'foo)
;use ^Q and ^U^U^Q and ^U^U^U^G^Q
;the evi-mode command "4dlj.j.j.j.j.j." is also a good testcase (gag)

;(setq x (list (read-char) quit-flag))^J^G
;x should get set to (7 t), but no result should be printed.

;also do this: make two screens, one viewing "*scratch*", the other "foo".
;in *scratch*, type (sit-for 20)^J
;wait a couple of seconds, move cursor to foo, type "a"
;a should be inserted in foo.  Cursor highlighting should not change in
;the meantime.

;do it with sleep-for.  move cursor into foo, then back into *scratch*
;before typing.

;make sure ^G aborts both sit-for and sleep-for.

;do it all in both v18 and v19 and make sure all results are the same.
;all of these cases matter a lot, but some in quite subtle ways.
*/
