/* Event allocation and memory management.
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

#include "config.h"
#include "intl.h"
#include "lisp.h"
#include "buffer.h"
#include "window.h"
#include "screen.h"
#include "process.h"
#include "events.h"

/* Where old events go when they are explicitly deallocated.
   The event chain here is cut loose before GC, so these will be freed
   eventually.
 */
static struct Lisp_Event *event_resource;


/* >>> Ad-hoc hack.  Should be part of define_lrecord_implementation */
void
clear_event_resource (void)
{
  event_resource = 0;
}


static Lisp_Object mark_event (Lisp_Object, void (*) (Lisp_Object));
extern void print_event (Lisp_Object, Lisp_Object, int);
static int sizeof_event (void *h) { return (sizeof (struct Lisp_Event)); }
static int event_equal (Lisp_Object, Lisp_Object, int);
DEFINE_LRECORD_IMPLEMENTATION (lrecord_event,
                               mark_event, print_event, 
                               0, sizeof_event, event_equal);

/* Make sure we lose quickly if we try to use this event */
static void
deinitialize_event (struct Lisp_Event *event)
{
  int i;

  for (i = 0; i < ((sizeof (struct Lisp_Event)) / sizeof (int)); i++)
    ((int *) event) [i] = 0xdeadbeef;
  event->event_type = dead_event;
  event->lheader.implementation = lrecord_event;
  event_next (event) = 0;
}

static Lisp_Object
mark_event (Lisp_Object obj, void (*markobj) (Lisp_Object))
{
  struct Lisp_Event *event = XEVENT (obj);

  switch (event->event_type)
    {
    case key_press_event:
      ((markobj) (event->event.key.key));
      break;
    case process_event:
      ((markobj) (event->event.process.process));
      break;
    case timeout_event:
      ((markobj) (event->event.timeout.function));
      ((markobj) (event->event.timeout.object));
      break;
    case eval_event:
    case menu_event:
      ((markobj) (event->event.eval.function));
      ((markobj) (event->event.eval.object));
      break;
    case button_press_event:
    case button_release_event:
    case pointer_motion_event:
    case magic_event:
    case empty_event:
    case dead_event:
      break;
    default:
      abort ();
    }
  event = event_next (event);
  if (!event)
    return (Qnil);
  XSETR (obj, Lisp_Event, event);
  return (obj);
}
  
static int
event_equal (Lisp_Object o1, Lisp_Object o2, int depth)
{
  if (XEVENT (o1)->event_type != XEVENT (o2)->event_type) return 0;
  if (!EQ (XEVENT (o1)->channel, XEVENT (o2)->channel)) return 0;
/*  if (XEVENT (o1)->timestamp != XEVENT (o2)->timestamp) return 0; */
  switch (XEVENT (o1)->event_type)
    {
    case process_event:
      return (EQ (XEVENT (o1)->event.process.process,
		  XEVENT (o2)->event.process.process));
    
    case timeout_event:
      if (NILP (Fequal (XEVENT (o1)->event.timeout.function,
			XEVENT (o2)->event.timeout.function)))
	return 0;
      if (NILP (Fequal (XEVENT (o1)->event.timeout.object,
			XEVENT (o2)->event.timeout.object)))
	return 0;
      return 1;
    
    case key_press_event:
      return ((EQ (XEVENT (o1)->event.key.key, XEVENT (o2)->event.key.key)
               && (XEVENT (o1)->event.key.modifiers
                   == XEVENT (o2)->event.key.modifiers)));

    case button_press_event:
    case button_release_event:
      return (((XEVENT (o1)->event.button.button
                == XEVENT (o2)->event.button.button)
               && (XEVENT (o1)->event.button.modifiers
                   == XEVENT (o2)->event.button.modifiers)));

#ifdef I18N4
    case wchar_event:
      return (XEVENT (o1)->event.wchar.data == XEVENT (o2)->event.wchar.data);
#endif

    case pointer_motion_event:
      return ((XEVENT (o1)->event.motion.x == XEVENT (o2)->event.motion.x
               && XEVENT (o1)->event.motion.y == XEVENT (o2)->event.motion.y));

    case menu_event:
    case eval_event:
      if (NILP (Fequal (XEVENT (o1)->event.eval.function,
			XEVENT (o2)->event.eval.function)))
	return 0;
      if (NILP (Fequal (XEVENT (o1)->event.eval.object,
			XEVENT (o2)->event.eval.object)))
	return 0;
      return 1;
    case magic_event:
      return (!memcmp ((char*) &(XEVENT (o1)->event.magic),
                       (char*) &(XEVENT (o2)->event.magic),
                       sizeof (struct magic_data)));

    case empty_event:      /* Empty and deallocated events are equal. */
    case dead_event:
      return 1;

    default:
      error ("unknown event type");
      return 0;                 /* not reached; warning suppression */
    }
}


DEFUN ("allocate-event", Fallocate_event, Sallocate_event, 0, 0, 0,
  "Returns an empty event structure.\n\
WARNING, the event object returned may be a reused one; see the function\n\
`deallocate-event'.")
    ()
{
  struct Lisp_Event *e;
  Lisp_Object event;
  if (event_resource)
  {
    e = event_resource;
    event_resource = event_next (e);
    XSETR (event, Lisp_Event, e);
  }
  else
  {
    event = make_event ();
    e = XEVENT (event);
  }
  deinitialize_event (e);
  e->event_type = empty_event;
  set_event_next (e, 0);
  e->timestamp = 0;
  e->channel = Qnil;
  return event;
}

DEFUN ("deallocate-event", Fdeallocate_event, Sdeallocate_event, 1, 1, 0,
  "Allow the given event structure to be reused.  You MUST NOT use this \n\
event object after calling this function with it.  You will lose.\n\
It is not necessary to call this function, as event objects are garbage-\n\
collected like all other objects; however, it may be more efficient to\n\
explicitly deallocate events when you are sure that that is safe.")
    (event)
    Lisp_Object event;
{
  struct Lisp_Event *e;
  CHECK_EVENT (event, 0);

  e = XEVENT (event);
  if (e->event_type == dead_event)
    error (GETTEXT ("this event is already deallocated!"));

  if (e->event_type < first_event_type || e->event_type > last_event_type)
    abort ();

#if 0
  {  
    int i;
    extern Lisp_Object Vlast_command_event;
    extern Lisp_Object Vlast_input_event, Vunread_command_event;
    extern Lisp_Object Vthis_command_keys, Vrecent_keys_ring;

    if (EQ (event, Vlast_command_event))
      abort ();
    if (EQ (event, Vlast_input_event))
      abort ();
    if (EQ (event, Vunread_command_event))
      abort ();
    for (i = 0; i < XVECTOR (Vthis_command_keys)->size; i++)
      if (EQ (event, XVECTOR (Vthis_command_keys)->contents [i]))
	abort ();
    for (i = 0; i < XVECTOR (Vrecent_keys_ring)->size; i++)
      if (EQ (event, XVECTOR (Vrecent_keys_ring)->contents [i]))
	abort ();
  }
#endif /* 0 */

  if (e == event_resource)
    abort ();
  deinitialize_event (e);
#ifndef ALLOC_NO_POOLS
  set_event_next (e, event_resource);
  event_resource = e;
#endif
  return Qnil;
}


DEFUN ("copy-event", Fcopy_event, Scopy_event, 1, 2, 0,
  "Make a copy of the given event object.  If a second argument is given,\n\
the first event is copied into the second and the second is returned.\n\
If the second argument is not supplied (or is nil) then a new event will\n\
be made as with `allocate-event.'  See also the function `deallocate-event'.")
     (event1, event2)
     Lisp_Object event1, event2;
{
  struct Lisp_Event *e1, *e2;

  CHECK_EVENT (event1, 0);
  if (NILP (event2))
    event2 = Fallocate_event ();
  else CHECK_EVENT (event2, 0);
  if (EQ (event1, event2))
    return Fsignal (Qerror,
                    list3 (build_string
                           (GETTEXT ("copy-event called with `eq' events")),
                           event1, event2));
  e1 = XEVENT (event1);
  e2 = XEVENT (event2);

  if ((e1->event_type < first_event_type)
      || (e1->event_type > last_event_type)
      || (e2->event_type < first_event_type)
      || (e2->event_type > last_event_type))
    abort ();
  if ((e1->event_type == dead_event) ||
      (e2->event_type == dead_event))
    return Fsignal (Qerror,
		    list3 (build_string (GETTEXT
			("copy-event called with a deallocated event!")),
			   event1,
                           event2));
  {
    struct Lisp_Event *save_next = event_next (e2);

    *e2 = *e1;
    set_event_next (e2, save_next);
    return (event2);
  }
}


void
syms_of_event_alloc ()
{
  event_resource = 0;
  defsubr (&Sallocate_event);
  defsubr (&Sdeallocate_event);
  defsubr (&Scopy_event);
}
