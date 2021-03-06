/* Definitions for the new event model;
   created 16-jul-91 by Jamie Zawinski
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

#ifndef _EMACS_EVENTS_H_
#define _EMACS_EVENTS_H_

/* There is one object, called an event_stream.  This object contains 
   callback functions for doing the window-system dependent operations that
   emacs requires.

   If emacs is compiled with support for X11 and the X Toolkit, then this
   event_stream structure will contain functions that can cope with input
   on emacs windows on multiple displays, as well as input from dumb tty
   screens.  

   If it is desired to have emacs able to open screens on the displays of
   multiple heterogeneous machines, X11 and SunView, or X11 and NeXT, for
   example, then it will be necessary to construct an event_stream structure
   that can cope with the given types.  Currently, the only implemented
   event_streams are for dumb-ttys, and for X11 plus dumb-ttys.
   
   To implement this for one window system is relatively clean and simple.  
   To implement this for multiple window systems is hard and hairy, so we 
   are punting for now.

  The slots of the event_stream structure:

 next_event_cb		A function which fills in an emacs_event struture
			with the next event available.  If there is no event
			available, then this should block.

 event_pending_cb	A function which says whether there are events to be
			read.  If called with an argument of 0, then this
			should say whether calling the next_event_cb will
			block.  If called with an argument of 1, then this
			should say whether there are user-generated events
			pending (that is, keypresses or mouse-clicks.)  This
			is used for redisplay optimization, among other 
			things.  On dumb ttys, these two results are the 
			same, but under a window system, they are not.  

 handle_magic_event_cb	Emacs calls this with an event structure which
  			contains window-system dependent information that
			emacs doesn't need to know about, but which must
			happen in order.  If the next_event_cb never returns
			an event of type "magic", this will never be used.

 generate_wakeup_cb	Called with an int, the number of milliseconds after
  			which a wakeup event should be generated; the number
			of milliseconds after which this timeout should *next*
			occur (or 0 if it's a one-shot); a function to call
			when the wakeup occurs; and some other object which
			that function will be called with.  This callback
			should return an int id-number which uniquely
			identifies this wakeup, and the "resignalled" wakeups.
			If an implementation doesn't have millisecond
			granularity, it should round up to the closest value
			it can deal with.

 disable_wakeup_cb	Called with an int, the id number of a wakeup to 
 			discard.  This id number must have been returned by
			the generate_wakeup_cb.  If the given wakeup has
			already expired, this should do nothing.

 select_process_cb	These callbacks tell the underlying implementation to
 unselect_process_cb	add or remove a file descriptor from the list of fds
  			which are polled for inferior-process input.  When
			input becomes available on the given tty, an event of
			type "process" should be generated.

 select_tty_cb		These callbacks tell the underlying implementation 
 unselect_tty_cb	to add or remove a file descriptor from the list of
  			fds which are polled for user-input.  Each character
			read from the fd generates a keypress event.  This
			is dumb-tty-type input; anything more complicated
			than that needs to be handled differently.  
			(Handwave, handwave.)

			Possibly we should implement these in terms of 
			select_process_cb.

 quitp_cb		A handler function called from the `QUIT' macro which
			should check whether the interrupt character has been
			typed.  On systems with SIGIO, this will not be called
			unless the `sigio_happened' flag is true (it is set
			from the SIGIO handler.)

 Emacs has its own event structures, which are distinct from the event
 structures used by X or any other window system.  It is the job of the
 event_stream layer to translate to this format.

 key_press_event	
    event_channel	A token representing which keyboard generated it.
			For this kind of event, this is a screen object.
			(This is for eventual support of multiple displays.)
    timestamp		When it happened (#### in server-time or emacs-time?)
    key			What keysym this is; an integer or a symbol.
			If this is an integer, it will be in the printing
			ASCII range: >32 and <127.
    modifiers		Bucky-bits on that key: control, meta, etc.
			For most keys, Shift is not a bit; that is implicit
			in the keyboard layout.

 wchar_event (I18N4 only)
    event_channel	A token representing which keyboard generated it.
    timestamp		When the input method composed its input.
    data		A single wide character.

 button_press_event
 button_release_event
    event_channel	A token representing which mouse generated it.
			For this kind of event, this is a screen object.
    timestamp		When it happened
    button		What button went down or up.
    modifiers		Bucky-bits on that button: shift, control, meta, etc.
    x, y		Where it was at the button-state-change (in pixels).

 pointer_motion_event
    event_channel	A token representing which mouse generated it.
			For this kind of event, this is a screen object.
    timestamp		When it happened
    x, y		Where it was after it moved (in pixels).
    modifiers		Bucky-bits down when the motion was detected.
			(Possibly not all window systems will provide this?)

 process_event
    timestamp		When it happened
    process		the emacs "process" object in question

 timeout_event
    timestamp		Now (really, when the timeout was signalled)
    function		The elisp function to call for this timeout.  It is
    			called with one argument, the event.
    object		Some lisp object associated with this timeout, to
    			make it easier to tell them apart.

 eval_event
    timestamp		When it happened.
    function		An elisp function to call with this event object.
    object		Anything.
			This kind of event is used internally; sometimes the
			window system interface would like to inform emacs of
			some user action (such as focusing on another screen)
			but needs that to happen synchronously with the other
			user input, like keypresses.

 menu_event
    timestamp		When it happened.
    function		An elisp function to call with this event object.
    object		Anything.
			This is similar to an eval_event, except that it is
			generated by selections in the menubar.  It is a
			"command" event, like key and mouse presses (and 
			unlike mouse motion, process output, and enter and
			leave window hooks.)  In many ways, eval_events are
			not the same as key- or menu-events.

 magic_event
			No user-serviceable parts within.  This is for things
			like KeymapNotify and ExposeRegion events and so on
			that emacs itself doesn't care about, but which it
			must do something with for proper interaction with
			the window system.

			Magic_events are handled somewhat asynchronously, just
			like subprocess filters.  However, occasionally a 
			magic_event needs to be handled synchronously; in that
			case, the asynchronous handling of the magic_event will
			push an eval_event back onto the queue, which will be 
			handled synchronously later.  This is why eval_events
			exist; I'm not entirely happy with this aspect of
			this event model.
 */


/* The following cruft is to determine whether we have SIGIO... */
/* #### Maybe all this junk isn't necessary any more now that we have
   syssignal.h? */

#include "config.h"

/* #include <signal.h>  use "syssignal.h" instead -jwz */
#include "syssignal.h"

/* Get FIONREAD, if it is available.  */
#ifdef HAVE_TERMIO
# include <termio.h>
# include <fcntl.h>
#else /* not HAVE_TERMIO */
# ifdef HAVE_TERMIOS
#  include <termios.h>
#  include <fcntl.h>
# else /* not HAVE_TERMIOS */
#  ifndef VMS
#   include <sys/ioctl.h>
#  endif /* VMS */
# endif /* not TERMIOS */
#endif /* not TERMIO */

/* UNIPLUS systems may have FIONREAD.  */
#ifdef UNIPLUS
# include <sys.ioctl.h>
#endif

/* Allow m- file to inhibit use of FIONREAD.  */
#ifdef BROKEN_FIONREAD
# undef FIONREAD
#endif

#ifdef BSD4_1
# define SIGIO SIGTINT
#endif
#ifndef FIONREAD
# ifdef SIGIO
#  undef SIGIO
# endif
#endif


struct Lisp_Event;
struct Lisp_Process;

struct event_stream {
  int  (*event_pending_p)	(int, int);
  void (*next_event_cb)		(struct Lisp_Event *);
  void (*handle_magic_event_cb)	(struct Lisp_Event *);
  int  (*generate_wakeup_cb)	(unsigned int, unsigned int,
				 Lisp_Object, Lisp_Object);
  void (*disable_wakeup_cb)	(int);
  void (*select_tty_cb)		(int);
  void (*unselect_tty_cb)	(int);
  void (*select_process_cb)	(struct Lisp_Process *);
  void (*unselect_process_cb)	(struct Lisp_Process *);
  void (*quit_p_cb)		(void);
};


extern struct event_stream *event_stream;

typedef enum emacs_event_type {
  empty_event,
  key_press_event,
#ifdef I18N4
  wchar_event,
#endif
  button_press_event,
  button_release_event,
  pointer_motion_event,
  process_event,
  timeout_event,
  magic_event,
  eval_event,
  menu_event,
  dead_event
} emacs_event_type;

#define first_event_type empty_event
#define last_event_type dead_event


struct key_data {
  Lisp_Object       keysym;
  unsigned char     modifiers;
};

#ifdef I18N4
struct wchar_data {
  wchar_t           data;
};
#endif

struct button_data {
  int               button;
  unsigned char     modifiers;
  int               x, y;
};

struct motion_data {
  int               x, y;
  unsigned char     modifiers;
};

struct process_data {
  Lisp_Object       process;
};

struct timeout_data {
  Lisp_Object       function;
  Lisp_Object	    object;
  int		    id_number;
};

struct eval_data {
  Lisp_Object       function;
  Lisp_Object	    object;
};

#if defined(HAVE_X_WINDOWS) && defined(emacs)
# include <X11/Xlib.h>
#endif

#ifndef MAX_UNDERLYING_EVENT_SIZE
# if defined(HAVE_X_WINDOWS) && defined(emacs)
#  define MAX_UNDERLYING_EVENT_SIZE (sizeof (XEvent))
# else
#  define MAX_UNDERLYING_EVENT_SIZE 1
# endif
#endif

struct magic_data {
  char		   underlying_event [MAX_UNDERLYING_EVENT_SIZE];
};

struct Lisp_Event {
  /* header->next (aka event_next ()) is used as follows:
     - For dead events, this is the next dead one.
     - For events on the command_event_queue, the next one on the queue.
     - Otherwise it's 0.
   */
  struct lrecord_header lheader;
  struct Lisp_Event	*next;
  emacs_event_type	event_type;
  Lisp_Object		channel;
  unsigned int		timestamp;
  union
    {
      struct key_data     key;
#ifdef I18N4
    struct wchar_data	wchar;
#endif
      struct button_data  button;
      struct motion_data  motion;
      struct process_data process;
      struct timeout_data timeout;
      struct eval_data    eval;		/* menu_event uses this too */
      struct magic_data   magic;
    } event;
#ifdef EPOCH
  Lisp_Object		epoch_event;
#endif
};

extern CONST struct lrecord_implementation lrecord_event[];

#define XEVENT(a) ((struct Lisp_Event *) XPNTR(a))
#define CHECK_EVENT(x, i) CHECK_RECORD ((x), lrecord_event, Qeventp, (i))
#define EVENTP(x) RECORD_TYPEP ((x), lrecord_event)
#define event_next(a) ((a)->next)
#define set_event_next(a, n) do { ((a)->next = (n)); } while (0)



/* The modifiers emacs knows about; these appear in key and button events.
 */
#define MOD_CONTROL	(1<<0)
#define MOD_META	(1<<1)
#define MOD_SUPER	(1<<2)
#define MOD_HYPER	(1<<3)
#define MOD_SYMBOL	(1<<4)
#define MOD_SHIFT	(1<<5)  /* not used for dual-case characters */

/* Note: under X Windows, MOD_SYMBOL is generated by the Alt key if there are
   both Alt and Meta keys.  If there are no Meta keys, then Alt generates
   MOD_META instead.
 */

#ifdef emacs
/* Maybe this should be trickier */
#define KEYSYM(x) (intern (x))

extern void format_event_object (char *buf, struct Lisp_Event *e, int brief);
extern void character_to_event (unsigned int c, struct Lisp_Event *event);


/* True is this is a non-internal event
   (keyboard press, menu, mouse button) */
extern int command_event_p (struct Lisp_Event *event);

extern Lisp_Object Fenqueue_command_event (Lisp_Object function,
					   Lisp_Object object);

#ifdef EPOCH
extern Lisp_Object Qx_property_change, Qx_client_message, Qx_map, Qx_unmap;
extern Lisp_Object Vepoch_event, Vepoch_event_handler;

extern void dispatch_epoch_event (struct Lisp_Event *event, Lisp_Object type);
#endif

#endif /* emacs */

#endif /* _EMACS_EVENTS_H_ */
