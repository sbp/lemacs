/* Event allocation and memory management.
   Copyright (C) 1991, 1992 Free Software Foundation, Inc.

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
#include "buffer.h"
#include "window.h"
#include "screen.h"
#include "process.h"
#include "events.h"

/* Where old events go when they are explicitly deallocated.
   The event chain here is cut loose before GC, so these will be freed
   eventually.
 */
static struct Lisp_Event *event_free_list;


/* Events are allocated similarly to strings.  All events are embedded in
   a linked list of event-block structures which are each 1k long; after the
   mark pass of the garbage collector, all event-block structures which 
   contain no marked events are freed.  If even one marked event remains,
   it is not freed, and the unmarked events are chained onto event_free_list.
   New events are added to the end of the last-allocated event block.  When
   it fills up, a new event-block is allocated and added to the end.

   In the case where there are no more pointers to a sequence of events which
   are adjascent to the end of the most recently allocated event-block, we
   decrement the fill pointer of that block, so that the space is reused
   immediately.
 */

#define EVENT_BLOCK_SIZE \
  ((1020 - sizeof (struct event_block *)) / sizeof (struct Lisp_Event))

struct event_block {
  struct Lisp_Event events [EVENT_BLOCK_SIZE];
  struct event_block *next;
};

static struct event_block *event_blocks;

/* Make sure we lose quickly if we try to use this event */
static void
deinitialize_event (event)
     struct Lisp_Event *event;
{
  int i;

  for (i = 0; i < ((sizeof (struct Lisp_Event)) / sizeof (int)); i++)
    ((int *) event) [i] = 0xdeadbeef;
  event->event_type = dead_event;
}

static void
get_more_events ()
{
  int i;
  struct event_block *new  = (struct event_block *)
    xmalloc (sizeof (struct event_block));

  /* Deinitialize events and put them on the free list */
  for (i = 0; i < EVENT_BLOCK_SIZE; i++)
    {
      deinitialize_event (&new->events[i]);
      new->events[i].next = event_free_list;
      event_free_list = &new->events[i];
    }
  new->next = event_blocks;
  event_blocks = new;
}


void
free_unmarked_events ()			/* called from gc_sweep() */
{
  struct event_block *prev = 0;
  struct event_block *current = event_blocks;
  while (current)
    {
      int i;
      int survivors = 0;
      /* Save the old free list in case we free the entire block */
      struct Lisp_Event *old_free_list = event_free_list;
      
      for (i = 0; i < EVENT_BLOCK_SIZE; i++)
	{
	  if (XMARKBIT ((int) (current->events [i].event_type)))
	    {
	      XUNMARK (current->events [i].event_type);
	      survivors = 1;
	    }
	  else
	    {
	      deinitialize_event (&current->events[i]);
	      current->events[i].next = event_free_list;
	      event_free_list = &current->events[i];
	    }
	}
      if (! survivors)
	{
	  struct event_block *tmp = current;
	  current = current->next;
	  if (prev)
	    prev->next = current;
	  /* Restore the free list before we started this block */
	  event_free_list = old_free_list;
	  if (tmp == event_blocks)
	    event_blocks = current;
	  xfree (tmp);
	} else {
	  prev = current;
	  current = current->next;
	}
    }
}

void
prepare_to_gc_events ()		/* called by Fgarbage_collect() */
{
  /* Flush the list of deallocated events.  No pointers to these should
     still exist, but even if they do, they will be marked normally.
   */
#if 0
  /* There's actually no need to go down the list and clear the next slots */
  struct Lisp_Event *prev = 0, *event = event_free_list;
  while (event) {
    if (prev)
      prev->next = 0;
    prev = event;
    event = event->next;
    
  }
#endif
  /* All we need to do is set the free list to 0 */
  event_free_list = 0;
}


DEFUN ("allocate-event", Fallocate_event, Sallocate_event, 0, 0, 0,
  "Returns an empty event structure.\n\
WARNING, the event object returned may be a reused one; see the function\n\
`deallocate-event'.")
    ()
{
  Lisp_Object event;
  if (! event_free_list)
    get_more_events ();
  XSET (event, Lisp_Event, (struct Lisp_Event *) event_free_list);
  event_free_list = event_free_list->next;
  XEVENT (event)->event_type = empty_event;
  XEVENT (event)->next = 0;
  XEVENT (event)->timestamp = 0;
  XEVENT (event)->channel = Qnil;
  return event;
}

extern Lisp_Object Vlast_command_event;
extern Lisp_Object Vlast_input_event, Vunread_command_event;
extern Lisp_Object Vthis_command_keys, Vrecent_keys_ring;

DEFUN ("deallocate-event", Fdeallocate_event, Sdeallocate_event, 1, 1, 0,
  "Allow the given event structure to be reused.  You MUST NOT use this \n\
event object after calling this function with it.  You will lose.\n\
It is not necessary to call this function, as event objects are garbage-\n\
collected like all other objects; however, it may be more efficient to\n\
explicitly deallocate events when you are sure that that is safe.")
    (event)
    Lisp_Object event;
{
  CHECK_EVENT (event, 0);
  if (XEVENT (event)->event_type == dead_event)
    error ("this event is already deallocated!");

  if (XEVENT (event)->event_type < first_event_type
      || XEVENT (event)->event_type > last_event_type)
    abort();

#if 0
  if (EQ (event, Vlast_command_event))
    abort ();
  if (EQ (event, Vlast_input_event))
    abort ();
  if (EQ (event, Vunread_command_event))
    abort ();
  {
    int i;
    for (i = 0; i < XVECTOR (Vthis_command_keys)->size; i++)
      if (EQ (event, XVECTOR (Vthis_command_keys)->contents [i]))
	abort ();
    for (i = 0; i < XVECTOR (Vrecent_keys_ring)->size; i++)
      if (EQ (event, XVECTOR (Vrecent_keys_ring)->contents [i]))
	abort ();
  }
#endif

  if (XEVENT (event) == event_free_list)
    abort ();
  deinitialize_event (XEVENT (event));
  XEVENT (event)->next = event_free_list;
  event_free_list = XEVENT (event);
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
  struct Lisp_Event *save_next;
  CHECK_EVENT (event1, 0);
  if (NILP (event2))
    event2 = Fallocate_event ();
  else CHECK_EVENT (event2, 0);
  if (EQ (event1, event2))
    return Fsignal (Qerror, Fcons (build_string ("those events are eq."),
				   Fcons (event1, Qnil)));
  if ((XEVENT (event1)->event_type < first_event_type)
      || (XEVENT (event1)->event_type > last_event_type)
      || (XEVENT (event2)->event_type < first_event_type)
      || (XEVENT (event2)->event_type > last_event_type))
    abort ();
  if ((XEVENT (event1)->event_type == dead_event) ||
      (XEVENT (event2)->event_type == dead_event))
    return Fsignal (Qerror,
		    Fcons (build_string
			   ("copy-event called with a deallocated event!"),
			   Fcons (event1, Fcons (event2, Qnil))));
  save_next = XEVENT (event2)->next;
  *XEVENT (event2) = *XEVENT (event1);
  XEVENT (event2)->next = save_next;
  return event2;
}


void
syms_of_event_alloc ()
{
  event_free_list = 0;
  defsubr (&Sallocate_event);
  defsubr (&Sdeallocate_event);
  defsubr (&Scopy_event);
}
