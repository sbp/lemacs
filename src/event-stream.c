/* The portable interface to event_streams.
   Copyright (C) 1991, 1992, 1993 Free Software Foundation, Inc.

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
#include "lisp.h"
#include "buffer.h"
#include "window.h"
#include "process.h"
#include "events.h"

/* The number of keystrokes between auto-saves. */
static int auto_save_interval;

extern Lisp_Object Qself_insert_command, Qprocessp;

extern int interrupt_char, help_char, meta_prefix_char;

Lisp_Object Qundefined; /* The symbol undefined; good a place as any... */

extern Lisp_Object Vmouse_motion_handler;

extern Lisp_Object Qpre_command_hook, Qpost_command_hook;

static void echo_char_event (), echo_prompt (), maybe_echo_keys ();
void cancel_echoing ();


/* The callback routines for the window system or terminal driver */
struct event_stream *event_stream;

/* This structure is what we use to excapsulate the state of a command sequence
   being composed; key events are executed by adding themselves to the command
   builder; if the command builder is then complete (does not still represent
   a prefix key sequence) it executes the corresponding command.
 */
static struct command_builder {
  Lisp_Object events;		/* Vector of the events being accumulated */
  int event_count;		/* How many elts of the vector are in use */
  Lisp_Object leaf;		/* Terminal node of what we've got so far */
  int echo_keys;		/* Whether minibuffer echoing is active */
  char *echobuf, *echoptr;	/* Minibuffer string, and fill pointer */
} *command_builder;

/* This structure is basically a typeahead queue: things like wait-reading-
   process-output will delay the execution of keyboard and mouse events by
   pushing them here.  I'd like this to be private to event-stream.c, but
   alloc.c needs to know about it to mark the events on it during GC.
 */
struct command_event_queue *command_event_queue;


extern Lisp_Object Vkeyboard_translate_table;
extern Lisp_Object Vthis_command, Vlast_command;
extern Lisp_Object minibuf_window, selected_window;

/* The number of keystrokes since the last auto-save. */
static int keystrokes;



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

#define	max(a,b) ((a)>(b)?(a):(b))

static void
maybe_do_auto_save ()
{
  keystrokes++;
  if (auto_save_interval > 0 &&
      keystrokes > max (auto_save_interval, 20) &&
      !detect_input_pending ())
    {
      keystrokes = 0;
      Fdo_auto_save (Qnil);
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
execute_help_form (event)
     Lisp_Object event;
{
  Lisp_Object tem0;
  int count = specpdl_ptr - specpdl;
  record_unwind_protect (Fset_window_configuration,
			 Fcurrent_window_configuration ());
  tem0 = Feval (Vhelp_form);
  if (STRINGP (tem0))
    internal_with_output_to_temp_buffer ("*Help*", print_help, tem0, Qnil);
  cancel_echoing ();
  Fnext_command_event (event);
  /* Remove the help from the screen */
  unbind_to (count, Qnil);
  redisplay ();
  if (event_to_character (XEVENT (event), 0) == ' ')
    {
      cancel_echoing ();
      Fnext_command_event (event);
    }
}

extern Lisp_Object Vunread_command_event;
extern Lisp_Object Vlast_command_event, Vlast_input_event;
/* These two for compatibility; they are V... because they can be nil. */
extern Lisp_Object Vlast_command_char, Vlast_input_char;
extern Lisp_Object Vlast_input_time;

extern Lisp_Object Vexecuting_macro;
extern int defining_kbd_macro;
extern void store_kbd_macro_event ();
extern void pop_kbd_macro_event ();

int
detect_input_pending ()
{
  /* Always call the event_pending_p hook even if there's an unread
     character, because that might do some needed ^G detection (on
     systems without SIGIO, for example).
   */
  if (event_stream && event_stream->event_pending_p (1)) return 1;
  if (!NILP (Vunread_command_event)) return 1;
  if (command_event_queue->head)
    {
      struct Lisp_Event *e;
      for (e = command_event_queue->head; e; e = e->next)
	if (e->event_type != eval_event)
	  return 1;
    }
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
  if (command_event_queue->tail &&
      command_event_queue->tail == XEVENT (event))
    abort();

  if (command_event_queue->tail)
    command_event_queue->tail->next = XEVENT (event);
  else
    command_event_queue->head = XEVENT (event);
  command_event_queue->tail = XEVENT (event);
  
  if (XEVENT (event) == XEVENT (event)->next)
    abort ();
}

DEFUN ("enqueue-eval-event", Fenqueue_command_event, Senqueue_command_event,
       2, 2, 0, 
       "Add an eval event to the back of the queue.\n\
(enqueue-eval-event <function> <object>)\n\
It will be the next event read after all pending events.")
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

static void store_recent_key ();

static void
next_event_internal (target_event, allow_queued)
     Lisp_Object target_event;
     int allow_queued;
{
  Lisp_Object event;
  struct Lisp_Event *e;

  if (XEVENT (target_event)->next)
    abort ();

  if (allow_queued && (e = command_event_queue->head))
    {
      XSET (event, Lisp_Event, e);
      command_event_queue->head = e->next;
      if (! e->next)
	command_event_queue->tail = 0;
      e->next = 0;
    }
  else
    {
      /* #### Temporary hack to make emacs exit "gracefully" when it tries
	 to run in tty-mode for some reason.  This is the latest time we
	 can make this check, because we're about to deref event_stream.
       */
      if (! event_stream)
	{
	  extern Display *x_current_display;
	  Fsend_string_to_terminal (build_string ("\n\n\
This version of emacs only runs under X Windows (for now).\n\
Check that your $DISPLAY environment variable is properly set.\n"));
	  if (!x_current_display) Vwindow_system = Qnil;
	  Fkill_emacs (make_number (69));
	}

      /* The command_event_queue was empty.  Wait for an event.
       */
      event = Fallocate_event ();
      e = XEVENT (event);
      event_stream->next_event_cb (e);
      if (e->event_type == key_press_event &&
	  event_to_character (e, 0) == interrupt_char)
	interrupt_signal (0);
    }

  Fcopy_event (event, target_event);
  Fdeallocate_event (event);
  return;
}


void maybe_status_notify (void);
void update_status (struct Lisp_Process *);
void finalize_kbd_macro_chars (void);
void deactivate_process (Lisp_Object);
int read_process_output (Lisp_Object, int);


DEFUN ("next-event", Fnext_event, Snext_event, 1, 1, 0,
  "Given an event structure, fills it in with the next event available\n\
from the window system or terminal driver.  Pass this object to\n\
dispatch-event to handle it.  See also the function next-command-event,\n\
which is often more appropriate.")
     (event)
     Lisp_Object event;
{
  int store_this_key = 0;

  CHECK_EVENT (event, 0);
  if (XEVENT (event)->event_type == dead_event)
    error ("next-event called with a deallocated event!");

  if (! detect_input_pending () && NILP (Vexecuting_macro))
    redisplay ();

  /* If there is an unread-command-event, simply return it.
     But do some error checking to make sure the user hasn't put something
     in the unread-command-event that they shouldn't have.
     This does not update this-command-keys and recent-keys.
   */
  if (!NILP (Vunread_command_event))
    {
      if (!EVENTP (Vunread_command_event) ||
	  (XEVENT (Vunread_command_event)->event_type != key_press_event &&
	   XEVENT (Vunread_command_event)->event_type != button_press_event &&
	   XEVENT (Vunread_command_event)->event_type != button_release_event&&
	   XEVENT (Vunread_command_event)->event_type != menu_event))
	{
	  Lisp_Object bogus = Vunread_command_event;
	  Vunread_command_event = Qnil;
	  while (1)
	    Fsignal (Qwrong_type_argument,
		     Fcons (Qeventp,
			    Fcons (bogus,
				   Fcons (intern ("unread-command-event"),
					  Qnil))));
	}
      if (!EQ (Vunread_command_event, event))
	Fcopy_event (Vunread_command_event, event);
      Vunread_command_event = Qnil;
    }

  /* If we're executing a keyboard macro, take the next event from that,
     and update this-command-keys and recent-keys.
     Note that the unread-command-event takes prescedence over kbd macros.
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

  maybe_status_notify ();	/* Notice process change */

  switch (XEVENT (event)->event_type)
    {
    case key_press_event:	/* any key input can trigger autosave */
      maybe_do_auto_save ();
      num_input_chars++;
      /* fall through */
    case button_press_event:	/* key or mouse input can trigger prompting */
      if (store_this_key)
	echo_char_event (XEVENT (event));
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
	  error ("Someone deallocated the last-input-event!");
	}
      if (! EQ (event, Vlast_input_event))
	Fcopy_event (event, Vlast_input_event);
      
      /* last-input-char and last-input-time are derived from last-input-event.
       */
      Vlast_input_char = Fevent_to_character (Vlast_input_event, Qnil);
      Vlast_input_time = Fcurrent_time_seconds (Vlast_input_time);

      /* If this key came from the keyboard or from a keyboard macro, then
	 it goes into the recent-keys and this-command-keys vectors.
	 If this key came from the keyboard, and we're defining a keyboard
	 macro, then it goes into the macro.
       */
      if (store_this_key)
	{
	  store_recent_key (event);
	  if (defining_kbd_macro && NILP (Vexecuting_macro))
	    {
	      if (command_builder->event_count == 0)
		finalize_kbd_macro_chars ();
	      store_kbd_macro_event (event);
	    }
	}
    default:
      ;
    }

  /* If this is the help char and there is a help form, then execute the
     help form and swallow this character.  This is the only place where
     calling Fnext_event() can cause arbitrary lisp code to run.  Note
     that execute_help_form() calls Fnext_command_event(), which calls
     this function, as well as Fdispatch_event.
   */
  if (!NILP (Vhelp_form) &&
      XEVENT (event)->event_type == key_press_event &&
      help_char == event_to_character (XEVENT (event), 0))
    execute_help_form (event);

  return event;
}


DEFUN ("next-command-event", Fnext_command_event, Snext_command_event, 1, 1, 0,
  "Given an event structure, fills it in with the next keyboard, mouse\n\
press, or mouse release event available from the user.  If there are\n\
non-command events available (mouse motion, sub-process output, etc) then\n\
these will be executed (with `dispatch-event') and discarded.  This \n\
function is provided as a convenience; it is equivalent to the elisp code\n\
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
  maybe_echo_keys ();  /* ## This sucks bigtime */
  while (1) {
    Fnext_event (event);
    switch (XEVENT (event)->event_type) {
    case key_press_event:
    case button_press_event:
    case button_release_event:
    case menu_event:
      return event;
    default:
      Fdispatch_event (event);
    }
  }
}


DEFUN ("read-char", Fread_char, Sread_char, 0, 0, 0,
  "Read a character from the command input (keyboard or macro).\n\
If a mouse click is detected, an error is signalled.  The character typed\n\
is returned as an ASCII value.  This is most likely the wrong thing for you\n\
to be using: consider using the `next-command-event' function instead.")
  ()
{
  register int val = -1;
  Lisp_Object event = Fallocate_event ();
  struct gcpro gcpro1;
  GCPRO1 (event);

  Fnext_command_event (event);
  switch (XEVENT (event)->event_type) {
  case key_press_event:
  case button_press_event:
  case button_release_event:
  case menu_event:
    if (XEVENT (event)->event_type == key_press_event)
      val = event_to_character (XEVENT (event), 0);
    if (val == -1)
      Fsignal (Qerror,
	       Fcons(build_string ("key read has no ASCII equivalent"),
		     Fcons (event, Qnil)));
    Fdeallocate_event (event);
    UNGCPRO;
    return (make_number (val));
  default:
    abort ();
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

  defining_kbd_macro = 0;
  command_builder->event_count = 0;
  command_builder->leaf = Qnil;
  Vinhibit_quit = Qt;

  event = Fallocate_event ();
  e = XEVENT (event);
  tail = 0;

  while (command_event_queue->head ||
	 (event_stream && event_stream->event_pending_p (1)))
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
	tail->next = e2;
      else
	head = e2;
      tail = e2;
    }

  if (command_event_queue->head || command_event_queue->tail)
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
      command_event_queue->head = head;
      command_event_queue->tail = tail;
    }

  Fdeallocate_event (event);

  Vinhibit_quit = oiq;
  return Qnil;
}


DEFUN ("accept-process-output", Faccept_process_output, Saccept_process_output,
  0, 1, 0,
  "Allow any pending output from subprocesses to be read by Emacs.\n\
It is read into the process' buffers or given to their filter functions.\n\
Non-nil arg PROCESS means do not return until some output has been received\n\
from PROCESS.")
  (proc)
     register Lisp_Object proc;
{
  Lisp_Object event = 0;
  struct gcpro gcpro1;
  GCPRO1 (event);

  if (!NILP (proc)) CHECK_PROCESS (proc, 0);
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
	      proc = Qnil;
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
	    maybe_status_notify ();
	    break;
	  }
	case timeout_event:
	case pointer_motion_event:
	case magic_event:
	  Fdispatch_event (event);
	  break;
	default:
	  enqueue_command_event (Fcopy_event (event, Qnil));
	}
    }
  Fdeallocate_event (event);
  UNGCPRO;
  return Qnil;
}


/* sit-for, sleep-for, and timeouts.
 */

static unsigned long
lisp_number_to_milliseconds (secs, allow_0)
     Lisp_Object secs;
     int allow_0;
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
    return Fsignal (Qerror, Fcons (build_string ("timeout is negative"),
				   Fcons (secs, Qnil)));
  if (!allow_0 && fsecs == 0)
    return Fsignal (Qerror, Fcons (build_string ("timeout is non-positive"),
				   Fcons (secs, Qnil)));
  if (fsecs >= (((unsigned int) 0xFFFFFFFF) / 1000))
    return Fsignal (Qerror, Fcons (build_string (
             "timeout would exceed 32 bits when represented in milliseconds"),
			    Fcons (secs, Qnil)));
  return msecs;
}


DEFUN ("sleep-for", Fsleep_for, Ssleep_for, 1, 1, 0,
  "Pause, without updating display, for ARG seconds.\n\
ARG may be a float, meaning pause for some fractional part of a second.")
  (n)
     Lisp_Object n;
{
  unsigned int msecs = lisp_number_to_milliseconds (n, 1);
  int id;
  Lisp_Object event = 0;
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
  (n, nodisp)
     Lisp_Object n, nodisp;
{
  unsigned long msecs = lisp_number_to_milliseconds (n, 1);
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
  for (e = command_event_queue->head; e; e = e->next)
    if (e->event_type == key_press_event ||
	e->event_type == button_press_event ||
	e->event_type == button_release_event ||
	e->event_type == menu_event)
      return Qnil;

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


#ifdef LISP_FLOAT_TYPE
DEFUN ("sleep-for-millisecs", Fsleep_for_millisecs, Ssleep_for_millisecs,
  1, 1, 0,
  "Pause, without updating display, for ARG milliseconds.\n\
This function is obsolete; call `sleep-for' with a float instead.")
  (n)
     Lisp_Object n;
{
  Lisp_Object args[2];
  args[0] = Ffloat (n);
  args[1] = make_number (1000);
  return Fsleep_for (Fquo (2, args));
}
#endif


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
  if (noninteractive) error ("can't add timeouts in batch mode");
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
  if (noninteractive) error ("can't use timeouts in batch mode");
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


/* ## This macro copied from process.c; should be in process.h */
#ifdef HAVE_SOCKETS
# define NETCONN_P(p) (XGCTYPE (XPROCESS (p)->childp) == Lisp_String)
#else
# define NETCONN_P(p) 0
#endif

extern Lisp_Object Qrun, Qexit;
extern int process_tick;

static void
deactivate_netconn (p)
     Lisp_Object p;
{
  XSETINT (XPROCESS (p)->tick, ++process_tick);
  if (!NILP (XPROCESS (p)->raw_status_low))
    update_status (XPROCESS (p));
  if (EQ (XPROCESS (p)->status, Qrun))
    XPROCESS (p)->status = Fcons (Qexit, Fcons (make_number (256), Qnil));
  deactivate_process (p);
}

static void dispatch_menu_event ();
static Lisp_Object compose_command ();


/* If execute_p is true, this does what you expect: execute the event.
   But if events are keyboard or mouse events, they are executed by passing 
   them to the command builder, which doesn't actually run a command until
   a complete key sequence has been read.  So, if the execute_p arg is
   false, this returns the complete command sequence without executing it,
   once it has read one, and returns nil before then.
   This function is the guts of Fread_key_sequence and Fdispatch_event.
 */
static Lisp_Object 
dispatch_event_internal (event, execute_p)
     Lisp_Object event;
     int execute_p;
{
  struct Lisp_Event *e;
  Lisp_Object result = Qnil;

  e = XEVENT (event);

  switch (e->event_type) {

  case key_press_event:
  case button_press_event:
  case button_release_event:
    result = compose_command (e, execute_p);
    break;

  case menu_event:
    if (execute_p)
      dispatch_menu_event (e);
    else
      {
	/* If a menu item is selected while in the midst of read-key-sequence,
	   return that event, so that describe-key works for menu items
	   as well.
	 */
	command_builder->event_count = 0;
	command_builder->leaf = Qnil;
	result = compose_command (e, 0);
      }
    break;
    
  case eval_event:
    dispatch_menu_event (e);
    break;

  case pointer_motion_event:
    if (!NILP (Vmouse_motion_handler))
      call1 (Vmouse_motion_handler, event);
    break;

  case process_event:
    {
      Lisp_Object p = e->event.process.process;
      int fd = XFASTINT (XPROCESS (p)->infd);
      int value;
      if (fd)
	{
	  if (NETCONN_P (p) && ! (value = read_process_output (p, fd)))
	    deactivate_netconn (p);
	  while ((value = read_process_output (p, fd)) > 0);
	}
      break;
    }
  case timeout_event:
    if (!NILP (e->event.timeout.function))
      call1 (e->event.timeout.function, e->event.timeout.object);
    break;

  case magic_event:
    event_stream->handle_magic_event_cb (e);
    break;

  default:
    abort();
  }

  return result;
}


/*
 * character prompting
 */

static void
echo_char_event (event)
     struct Lisp_Event *event;
{
  int len = command_builder->echoptr - command_builder->echobuf;
  if (len > 200) return;
  format_event_object (command_builder->echoptr, event, 1);
  command_builder->echoptr += strlen (command_builder->echoptr);
  if (len == 0 && help_char == event_to_character (event, 0))
    {
      strcpy (command_builder->echoptr, " (Type ? for further options)");
      command_builder->echoptr += strlen (command_builder->echoptr);
    }
  command_builder->echoptr [0] = ' ';
  command_builder->echoptr [1] = '-';
  command_builder->echoptr [2] = ' ';
  command_builder->echoptr [3] = 0;
  command_builder->echoptr++;
}

static void
echo_prompt (str)
     char *str;
{
  int len = strlen (str);
  if (len > 200) return;
  memcpy (command_builder->echobuf, str, len+1);
  command_builder->echoptr = command_builder->echobuf + len;
  command_builder->echo_keys = 1;
  maybe_echo_keys ();
}

extern int minibuf_level;
static int echo_keystrokes;

static void
maybe_echo_keys ()
{
  /* Message turns off echoing unless more keystrokes turn it on again. */
  if (echo_area_glyphs && *echo_area_glyphs &&
      echo_area_glyphs != command_builder->echobuf) {
    command_builder->echo_keys = 0;
    return;
  }

  if (command_builder->echo_keys ||
      (minibuf_level == 0 &&
       echo_keystrokes > 0 &&
       ! NILP (Fsit_for (make_number (echo_keystrokes), Qt))))
    {
      command_builder->echo_keys = 1;
      /* This is like calling message(), but trickier */
      echo_area_glyphs = command_builder->echobuf;
    }
}

extern char* echo_area_glyphs;

void
cancel_echoing ()
{
  command_builder->echoptr = command_builder->echobuf;
  if (command_builder->echo_keys) {
    command_builder->echo_keys = 0;
    echo_area_glyphs = 0;
  }
}


/* Compare the current state of the command builder against the local and
   global keymaps; if there is no match, try again, case-insensitively.
   The binding found (if any) is stored in the CB's `leaf' slot.
   It may be a command or a keymap if we're not done yet.
 */
static Lisp_Object find_leaf_unwind ();

static void
command_builder_find_leaf (allow_menu_events_p)
     int allow_menu_events_p;
{
  struct gcpro gcpro1;
  int size = XVECTOR (command_builder->events)->size;
  int fill_pointer = command_builder->event_count;
  Lisp_Object event1;
  int count = specpdl_ptr - specpdl;

  /* This is an utterly sleazy way of simulating fill pointers...
   */
  GCPRO1 (XVECTOR (command_builder->events)->contents [fill_pointer]);
  gcpro1.nvars = size - fill_pointer;
  record_unwind_protect (find_leaf_unwind, size);
  XVECTOR (command_builder->events)->size = fill_pointer;
  event1 = XVECTOR (command_builder->events)->contents [0];
  if (allow_menu_events_p &&
      fill_pointer == 1 &&
      XEVENT (event1)->event_type == menu_event)
    {
      Lisp_Object fn = XEVENT (event1)->event.eval.function;
      Lisp_Object arg = XEVENT (event1)->event.eval.object;
      command_builder->leaf = Fcons (fn, Fcons (arg, Qnil));
    }
  else
    command_builder->leaf = Fkey_binding (command_builder->events);
  unbind_to (count, Qnil);
  UNGCPRO;

  /* If we didn't find a binding, and the last event in the sequence is
     a shifted character, then try again with the lowercase version.
   */
  if (NILP (command_builder->leaf))
    {
      Lisp_Object terminal = 
	XVECTOR (command_builder->events)->contents [fill_pointer - 1];
      if (XEVENT (terminal)->event_type == key_press_event &&
	  ((XEVENT (terminal)->event.key.key >= 'A' &&
	    XEVENT (terminal)->event.key.key <= 'Z') ||
	   XEVENT (terminal)->event.key.modifiers & MOD_SHIFT))
	{
	  Lisp_Object e2 = Fallocate_event ();
	  GCPRO1 (e2);
	  Fcopy_event (terminal, e2);
	  if (XEVENT (e2)->event.key.modifiers & MOD_SHIFT)
	    XEVENT (e2)->event.key.modifiers &= (~ MOD_SHIFT);
	  else
	    XEVENT (e2)->event.key.key += ('a'-'A');
	  XVECTOR (command_builder->events)->contents [fill_pointer - 1] = e2;
	  command_builder_find_leaf (allow_menu_events_p);
	  /* If there was no match with the lower-case version either, then
	     put back the upper-case event for the error message.
	   */
	  if (NILP (command_builder->leaf))
	    {
	      XVECTOR (command_builder->events)->contents [fill_pointer - 1]
		= terminal;
	      Fdeallocate_event (e2);
	    }
	  UNGCPRO;
	}
    }
      
}

static Lisp_Object
find_leaf_unwind (size)
     int size;
{
  XVECTOR (command_builder->events)->size = size;
  return Qnil;
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

Lisp_Object Vthis_command_keys;
int this_command_keys_count;
int reset_this_command_keys;

/* reset_this_command_keys is a flag which means that the next time
   push_command_keys_vector() is called, it should flush the current
   value of Vthis_command_keys and start over.  The times at which the
   the command-keys are reset (instead of merely being augumented)
   are pretty conterintuitive.
 */

static Lisp_Object
reset_this_command_keys_fn () /* need a function for unwind-protect */
{
  reset_this_command_keys = 1;
  return Qnil;
}

static void
push_command_keys_vector (event)
     Lisp_Object event;
{
  int i;
  int old_size = XVECTOR (Vthis_command_keys)->size;
  int old_fp, new_fp;
  int meta_hack = 0;

  if (reset_this_command_keys)
    {
      reset_this_command_keys = 0;
      this_command_keys_count = 0;
    }

  old_fp = this_command_keys_count;
  new_fp = old_fp + 1;

  if (old_size <= new_fp) {
    Lisp_Object new = Fmake_vector (new_fp + 10, Qnil);
    for (i = 0; i < old_size; i++)
      XVECTOR (new)->contents [i] = XVECTOR (Vthis_command_keys)->contents [i];
    Vthis_command_keys = new;
  }
  for (i = 0; i < new_fp; i++) {
    Lisp_Object event;
    if (NILP (event = XVECTOR (Vthis_command_keys)->contents [i]))
      XVECTOR (Vthis_command_keys)->contents [i] = event = Fallocate_event ();
  }
  Fcopy_event (event, XVECTOR (Vthis_command_keys)->contents [new_fp-1]);

  if (meta_hack)
    XEVENT (XVECTOR (Vthis_command_keys)->contents [new_fp-1])
      ->event.key.modifiers |= MOD_META;

  this_command_keys_count = new_fp;
}


static void
push_recent_keys_vector (event)
     Lisp_Object event;
{
  if (NILP (XVECTOR (recent_keys_ring)->contents [recent_keys_ring_index]))
    XVECTOR (recent_keys_ring)->contents [recent_keys_ring_index] =
      Fallocate_event ();
  Fcopy_event (event,
	       XVECTOR (recent_keys_ring)->contents [recent_keys_ring_index]);
  if (++recent_keys_ring_index == RECENT_KEYS_SIZE)
    recent_keys_ring_index = 0;
}


static void
store_recent_key (event)
     Lisp_Object event;
{
  push_command_keys_vector (event);
  push_recent_keys_vector (event);
}


static void command_builder_push_meta_hack ();

/* Add the given event to the command builder, enlarging the vector 
   first if necessary.

   Extra hack: this also updates the recent_keys_ring and Vthis_command_keys
   vectors to translate "ESC x" to "M-x" (for any "x" of course.)
 */
static void
command_builder_push (e)
     struct Lisp_Event *e;
{
  Lisp_Object event, event2;
  struct Lisp_Vector *vec = XVECTOR (command_builder->events);
  Lisp_Object prev = Qnil;

  if (command_builder->event_count > 0)
    prev = vec->contents [command_builder->event_count - 1];
      
  if (!NILP (prev) && meta_prefix_char != -1 &&
      event_to_character (XEVENT (prev), 0) == meta_prefix_char)
    {
      command_builder_push_meta_hack (e);
      return;
    }

  /* Update the command-builder vector
   */
  if (command_builder->event_count >= vec->size - 1) {
    int i;
    int old_size = command_builder->event_count;
    int new_size = old_size + 50;
    Lisp_Object old_vector = command_builder->events;
    Lisp_Object new_vector = Fmake_vector (new_size, Qnil);
    struct gcpro gcpro1;
    GCPRO1 (new_vector);
    for (i = 0; i < old_size; i++)
      XVECTOR (new_vector)->contents [i] = XVECTOR (old_vector)->contents [i];
    for (; i < new_size; i++)
      XVECTOR (new_vector)->contents [i] = Fallocate_event ();
    command_builder->events = new_vector;
    vec = XVECTOR (new_vector);
    UNGCPRO;
  }
  XSET (event, Lisp_Event, e);
  event2 = vec->contents [command_builder->event_count++];
  Fcopy_event (event, event2);
  XEVENT (event2)->next = (struct Lisp_Event *) -1;  /* safety hack */
}


/* When we see a sequence like "ESC x", pretend we really saw "M-x".
   DoubleThink the recent-keys and this-command-keys as well.
 */
static void
command_builder_push_meta_hack (e)
     struct Lisp_Event *e;
{
  Lisp_Object event;
  struct Lisp_Vector *vec = XVECTOR (command_builder->events);
  Lisp_Object target_event = vec->contents [command_builder->event_count - 1];

  /* Modify the previous most-recently-pushed event on the command
     builder to be a copy of this one with the meta-bit set instead of
     pushing a new event.
   */
  XSET (event, Lisp_Event, e);
  Fcopy_event (event, target_event);
  if (XEVENT (target_event)->event_type == key_press_event)
    XEVENT (target_event)->event.key.modifiers |= MOD_META;
  else if (XEVENT (target_event)->event_type == button_press_event ||
	   XEVENT (target_event)->event_type == button_release_event)
    XEVENT (target_event)->event.button.modifiers |= MOD_META;
  else abort ();

  /* regenerate the echo-glyphs */
  {
    int i;
    command_builder->echoptr = command_builder->echobuf;
    for (i = 0; i < command_builder->event_count; i++)
      echo_char_event (XEVENT (XVECTOR (command_builder->events)
			       ->contents [i]));
  }
}


/* Self-insert-command is magic in that it doesn't always push an undo-
   boundary: up to 20 consecutive self-inserts can happen before an undo-
   boundary is pushed.  This variable is that counter.
 */
static int self_insert_countdown;

static void dispatch_command_event_internal ();

extern Lisp_Object Vcurrent_mouse_event;
extern int zmacs_region_active_p, zmacs_region_stays;
extern void zmacs_update_region (void);

extern int kbd_macro_end, pre_command_kbd_macro_end;

static void pre_command_hook ();
static void post_command_hook ();

/* Add the given event to the command builder, and if that results in 
   a complete key sequence, either execute the corresponding command, or
   return the key sequence.
 */
static Lisp_Object
compose_command (event, execute_p)
     struct Lisp_Event *event;
     int execute_p;
{
  Lisp_Object leaf;
  int count;

  /* for button-release events, kludges abound */
  int button_up_p = (event->event_type == button_release_event);

  command_builder_push (event);
  command_builder_find_leaf (! execute_p);

  leaf = command_builder->leaf;
  count = command_builder->event_count;

  {
    Lisp_Object leaf2 = leaf;

    if (! button_up_p)
      echo_area_glyphs = 0;

    while (SYMBOLP (leaf2) && !NILP (Ffboundp (leaf2)))
      leaf2 = Fsymbol_function (leaf2);
    /* If the leaf is a keymap, we're not done yet.  Return nil.
       We do all this junk with leaf2 instead of leaf so that
       command-execute is called on the actual contents of the
       keymap instead of its function cell.
     */
    if (KEYMAPP (leaf2)) {
      maybe_echo_keys ();
      return Qnil;
    }
  }

  /* The suppress-keymap function binds keys to 'undefined - special-case
     that here, so that being bound to that has the same error-behavior as
     not being defined at all.
   */
  if (EQ (leaf, Qundefined)) leaf = Qnil;

  /* Now we've got a complete key sequence.  Execute it or return a vector
     of the events, depending on the value of execute_p.
   */
  if (execute_p)
    {
      if (NILP (leaf))
	{
	 /* At this point, we know that the sequence is not bound to a
	    command.  Normally, we beep and print a message informing the
	    user of this.  But we do not beep or print a message when:

	    o  the last event in this sequence is a mouse-up event; or
	    o  the last event in this sequence is a mouse-down event and
	       there is a binding for the mouse-up version.

	    That is, if the sequence ``C-x button1'' is typed, and is not
	    bound to a command, but the sequence ``C-x button1up'' is bound
	    to a command, we do not complain about the ``C-x button1''
	    sequence.  If neither ``C-x button1'' nor ``C-x button1up'' is
	    bound to a command, then we complain about the ``C-x button1''
	    sequence, but later will *not* complain about the ``C-x button1up''
	    sequence, which would be redundant.

	    This is pretty hairy, but I think it's the most intuitive behavior.
	  */
	  struct Lisp_Event *terminal_event =
	    /* `terminal-event' is equal but not eq to `event' */
	    XEVENT (XVECTOR (command_builder->events)->contents [count-1]);

	  if (terminal_event->event_type == button_press_event)
	    {
	      int suppress_bitching;
	      /* Temporarily pretend the last event was an "up" instead of a
		 "down", and look up its binding. */
	      terminal_event->event_type = button_release_event;
	      command_builder_find_leaf (0);
	      /* If the "up" version is bound, don't complain. */
	      suppress_bitching = !NILP (command_builder->leaf);
	      /* Undo the temporary changes we just made. */
	      terminal_event->event_type = button_press_event;
	      command_builder->leaf = leaf;
	      if (suppress_bitching)
		{
		  /* Pretend this press was not seen (treat it as a prefix) */
		  command_builder->event_count--;
		  maybe_echo_keys ();
		  return Qnil;
		}
	    }
      
	  /* Complain that the typed sequence is not defined, if this is the
	     kind of sequence that warrants a complaint.
	   */
	  command_builder->echo_keys = 1;
	  if (! button_up_p)
	    {
	      echo_area_glyphs = 0;
	      strcpy (command_builder->echoptr,
		      "not defined."); /* doo dah, doo dah */
	      maybe_echo_keys ();
	      command_builder->echo_keys = 0;
	      /* Run the pre-command-hook before barfing about an
		 undefined key. */
	      Vthis_command = Qnil;
	      pre_command_hook ();
	      /* Beep (but don't signal).  The post-command-hook doesn't run,
		 just as it wouldn't run if we had executed a real command
		 which signalled an error.  But maybe it should in this case.
	       */
	      bitch_at_user (intern ("undefined-key"));

	      cancel_echoing ();
	    }
	  /* Reset the command builder to read the next sequence.
	   */
	  command_builder->event_count = 0;
	  command_builder->leaf = Qnil;
	  defining_kbd_macro = 0;
	  Vprefix_arg = Qnil;
	  return Qnil;
	}
      dispatch_command_event_internal (event, leaf);
      return Qnil;
    }
  else
    {
      /* Copy the vector and the events in it.
       */
      Lisp_Object vector = Fmake_vector (make_number (count), Qnil);
      Lisp_Object *vec = XVECTOR (vector)->contents;
      Lisp_Object *events = XVECTOR (command_builder->events)->contents;
      int i;
      for (i = 0; i < count; i++)
	vec [i] = Fcopy_event (events [i], Qnil);
      command_builder->event_count = 0;
      command_builder->leaf = Qnil;
      return vector;
    }
}


static void store_last_command_event ();
static void
dispatch_command_event_internal (event, leaf)
     struct Lisp_Event *event;
     Lisp_Object leaf;
{
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
  if (command_builder->echo_keys && event->event_type != eval_event)
    {
      command_builder->echoptr[0] = 0;
      maybe_echo_keys ();
      Fsit_for (0, Qt);
    }

  if (event->event_type == key_press_event)
    Vcurrent_mouse_event = Qnil;
  else XSET (Vcurrent_mouse_event, Lisp_Event, event);

  if (event->event_type != eval_event)
    store_last_command_event (event);

  {
    char *old_eag = echo_area_glyphs; /* before command executed */
    int old_rap = zmacs_region_active_p;
    zmacs_region_stays = 0;

    if (defining_kbd_macro)
      pre_command_kbd_macro_end = kbd_macro_end;

    /* Now we actually execute the command.
       If the command completes abnormally (signals an error, or does
       a throw past us) then we want reset_this_command_keys to get set
       to 1.  Otherwise, we want it to be 0.  We do this via the kind
       of hairy unwind-protect here...
     */
    {
      int count = specpdl_ptr - specpdl;
      record_unwind_protect (reset_this_command_keys_fn, 0);
      
      if (event->event_type != eval_event)
	pre_command_hook ();

      if (event->event_type == menu_event ||
	  event->event_type == eval_event)
	call1 (event->event.eval.function, event->event.eval.object);
      else
	Fcommand_execute (Vthis_command, Qnil);
      
      /* the following two lines set it to 1 and then set it back;
	 but remember that the unbinding will happen from elsewhere
	 if an error was actually signalled, so the setting back
	 will only happen if it completed normally.
       */
      unbind_to (count, Qnil);
      reset_this_command_keys = 0;
    }

    if (event->event_type == eval_event)
      return;

    /* If we're recording a keyboard macro, and the last command
       executed set a prefix argument, then decrement the pointer to
       the "last character really in the macro" to be just before this
       command.  This is so that the ^U in "^U ^X )" doesn't go onto
       the end of macro.
     */
    if (defining_kbd_macro && !NILP (Vprefix_arg))
      kbd_macro_end = pre_command_kbd_macro_end;

    post_command_hook (old_rap);

    /* Commands that set the prefix arg don't update last-command, don't
       reset the echoing state, and don't go into keyboard macros unless
       followed by another command.
     */
    if (NILP (Vprefix_arg))
      {
	char *new_eag = echo_area_glyphs;  /* after command executed */
	Vlast_command = Vthis_command;
	cancel_echoing ();  /* clear the char-echoing... */
	reset_this_command_keys = 1;  /* restart this-command-keys */
	/* ...but if the command printed a message, don't lose it. */
	if (old_eag != new_eag) echo_area_glyphs = new_eag;
      }
    else
      {
	maybe_echo_keys ();
      }
  }
}


static void
dispatch_menu_event (e)
     struct Lisp_Event *e;
{     
  dispatch_command_event_internal (e, Qnil);
}


static void
store_last_command_event (event)
     struct Lisp_event *event;
{
  Lisp_Object e;

  command_builder->event_count = 0;
  command_builder->leaf = Qnil;

  /* Store the last-command-event.  The semantics of this is that it is
     the last event most recently involved in command-lookup.
     */
  if (!EVENTP (Vlast_command_event))
    Vlast_command_event = Fallocate_event ();
  if (XEVENT (Vlast_command_event)->event_type == dead_event)
    {
      Vlast_command_event = Fallocate_event ();
      error ("Someone deallocated the last-command-event!");
    }

  XSET (e, Lisp_Event, event);
  if (! EQ (e, Vlast_command_event))
    Fcopy_event (e, Vlast_command_event);
  Vlast_command_char = Fevent_to_character (Vlast_command_event, Qnil);
}

static void
pre_command_hook ()
{
  if (!NILP (Vrun_hooks))
    call1 (Vrun_hooks, Qpre_command_hook);
}

static void
post_command_hook (old_rap)
     int old_rap;
{
  /* Turn off region hilighting unless this command requested that
     it be left on, or we're in the minibuffer.  We don't turn it off
     when we're in the minibuffer so that things like M-x write-region
     still work!

     This could be done via a function on the post-command-hook, but
     we don't want the user to accidentally remove it.
   */
  if (! zmacs_region_stays &&
      !EQ (minibuf_window, selected_window) &&
      (! zmacs_region_active_p ||
       ! (zmacs_region_active_p > old_rap)))
    Fzmacs_deactivate_region ();
  else
    zmacs_update_region ();

  if (!NILP (Vrun_hooks))
    call1 (Vrun_hooks, Qpost_command_hook);
}



static Lisp_Object dispatch_event_internal ();
static Lisp_Object compose_command ();

DEFUN ("dispatch-event", Fdispatch_event, Sdispatch_event, 1, 1, 0,
  "Given an event object returned by next-event, execute it.")
     (event)
     Lisp_Object event;
{
  CHECK_EVENT (event, 0);
  if (XEVENT (event)->event_type == dead_event)
    error ("dispatch-event called on a deallocated event!");
  dispatch_event_internal (event, 1);
  return Qnil;
}

DEFUN ("read-key-sequence", Fread_key_sequence, Sread_key_sequence, 1, 1, 0,
  "Read a sequence of keystrokes or mouse clicks and return a vector of the\n\
event objects read.  The vector is newly created, but the event objects are\n\
reused: if you want to hold a pointer to them beyond the next call to this\n\
function, you must copy them first.\n\
\n\
The sequence read is sufficient to specify a non-prefix command starting\n\
from the current local and global keymaps.  A C-g typed while in this\n\
function is treated like any other character, and `quit-flag' is not set.\n\
One arg, PROMPT, is a prompt string, or nil meaning do not prompt specially.\n\
\n\
If the user selects a menu item while we are prompting for a key-sequence,\n\
the returned value will be a vector of a single menu-selection event.\n\
An error will be signalled if you pass this value to `lookup-key' or a\n\
related function.")
     (prompt)
     Lisp_Object prompt;
{
  Lisp_Object result = Qnil;
  Lisp_Object event = Fallocate_event ();
  int count = specpdl_ptr - specpdl;
  struct gcpro gcpro1;
  GCPRO1 (event);

  QUIT;
  CHECK_STRING (prompt, 0);
  echo_prompt ((char *) XSTRING (prompt)->data);

  specbind (Qinhibit_quit, Qt);
  reset_this_command_keys = 1; /* this is stupid, but that's how v18 works */
  while (NILP (result))
    result = dispatch_event_internal (Fnext_event (event), 0);
  Vquit_flag = Qnil;  /* In case we read a ^G */
  Fdeallocate_event (event);
  unbind_to (count, Qnil);
  UNGCPRO;
  return result;
}


void
syms_of_event_stream ()
{
  command_builder = (struct command_builder *)
    xmalloc (sizeof (struct command_builder));
  command_builder->event_count = 0;
  command_builder->leaf = Qnil;
  command_builder->echobuf = (char *) xmalloc (300);
  command_builder->echoptr = command_builder->echobuf;
  command_builder->echoptr[0] = 0;
  command_builder->echo_keys = 0;
  command_builder->events = Fmake_vector (0, Qnil);
  staticpro (&command_builder->events);

  command_event_queue = (struct command_event_queue *)
    xmalloc (sizeof (struct command_event_queue));
  command_event_queue->head = 0;
  command_event_queue->tail = 0;

  recent_keys_ring_index = 0;
  recent_keys_ring = Fmake_vector (RECENT_KEYS_SIZE, Qnil);
  staticpro (&recent_keys_ring);

  this_command_keys_count = 0;
  Vthis_command_keys = Fmake_vector (40, Qnil);
  staticpro (&Vthis_command_keys);

  num_input_chars = 0;
  self_insert_countdown = 0;
 
  Qundefined = intern ("undefined");
  staticpro (&Qundefined);

  defsubr (&Sinput_pending_p);
  defsubr (&Senqueue_command_event);
  defsubr (&Snext_event);
  defsubr (&Snext_command_event);
  defsubr (&Sread_char);
  defsubr (&Sdiscard_input);
  defsubr (&Ssit_for);
  defsubr (&Ssleep_for);
#ifdef LISP_FLOAT_TYPE
  defsubr (&Ssleep_for_millisecs);
#endif
  defsubr (&Saccept_process_output);
  defsubr (&Sadd_timeout);
  defsubr (&Sdisable_timeout);
  defsubr (&Sdispatch_event);
  defsubr (&Sread_key_sequence);

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
