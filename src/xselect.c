/* X Selection processing for emacs
   Copyright (C) 1990 Free Software Foundation.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Rewritten by jwz */

#include "config.h"
#include "lisp.h"
#include "xterm.h"	/* for all of the X includes */
#include "dispextern.h"	/* screen.h seems to want this */
#include "screen.h"	/* Need this to get the X window of selected_screen */

#define CUT_BUFFER_SUPPORT

Atom Xatom_CLIPBOARD, Xatom_TIMESTAMP, Xatom_TEXT, Xatom_DELETE,
  Xatom_MULTIPLE, Xatom_INCR, Xatom_EMACS_TMP, Xatom_TARGETS, Xatom_NULL,
  Xatom_ATOM_PAIR;

Lisp_Object QPRIMARY, QSECONDARY, QSTRING, QINTEGER, QCLIPBOARD, QTIMESTAMP,
  QTEXT, QDELETE, QMULTIPLE, QINCR, QEMACS_TMP, QTARGETS, QATOM, QNULL,
  QATOM_PAIR;

#ifdef CUT_BUFFER_SUPPORT
Lisp_Object QCUT_BUFFER0, QCUT_BUFFER1, QCUT_BUFFER2, QCUT_BUFFER3,
  QCUT_BUFFER4, QCUT_BUFFER5, QCUT_BUFFER6, QCUT_BUFFER7;
#endif

Lisp_Object Vx_lost_selection_hooks;
Lisp_Object Vx_sent_selection_hooks;

/* If this is a smaller number than the max-request-size of the display,
   emacs will use INCR selection transfer when the selection is larger
   than this.  The max-request-size is usually around 64k, so if you want
   emacs to use incremental selection transfers when the selection is 
   smaller than that, set this.  I added this mostly for debugging the
   incremental transfer stuff, but it might improve server performance.
 */
#define MAX_SELECTION_QUANTUM 0xFFFFFF

#define SELECTION_QUANTUM(dpy) ((XMaxRequestSize (dpy) << 2) - 100)


/* The time of the last-read mouse or keyboard event. 
   For selection purposes, we use this as a sleazy way of knowing what the
   current time is in server-time.  This assumes that the most recently read
   mouse or keyboard event has something to do with the assertion of the
   selection, which is probably true.
 */
extern Time mouse_timestamp;


/* This is an association list whose elements are of the form
     ( selection-name selection-value selection-timestamp )
   selection-name is a lisp symbol, whose name is the name of an X Atom.
   selection-value is the value that emacs owns for that selection.
     It may be any kind of Lisp object.
   selection-timestamp is the time at which emacs began owning this selection,
     as a cons of two 16-bit numbers (making a 32 bit time.)
   If there is an entry in this alist, then it can be assumed that emacs owns
    that selection.
   The only (eq) parts of this list that are visible from elisp are the
    selection-values.
 */
Lisp_Object Vselection_alist;

/* This is an alist whose CARs are selection-types (whose names are the same
   as the names of X Atoms) and whose CDRs are the names of Lisp functions to
   call to convert the given Emacs selection value to a string representing 
   the given selection type.  This is for elisp-level extension of the emacs
   selection handling.
 */
Lisp_Object Vselection_converter_alist;



/* Utility functions */

static void lisp_data_to_selection_data ();
static Lisp_Object selection_data_to_lisp_data ();
static Lisp_Object x_get_window_property_as_lisp_data ();

static int expect_property_change ();
static void wait_for_property_change ();
static void unexpect_property_change ();
static int waiting_for_other_props_on_window ();

/* This converts a Lisp symbol to a server Atom, avoiding a server 
   roundtrip whenever possible.
 */
static Atom
symbol_to_x_atom (display, sym)
     Display *display;
     Lisp_Object sym;
{
  Atom val;
  if (NILP (sym))	    return 0;
  if (EQ (sym, QPRIMARY))   return XA_PRIMARY;
  if (EQ (sym, QSECONDARY)) return XA_SECONDARY;
  if (EQ (sym, QSTRING))    return XA_STRING;
  if (EQ (sym, QINTEGER))   return XA_INTEGER;
  if (EQ (sym, QATOM))	    return XA_ATOM;
  if (EQ (sym, QCLIPBOARD)) return Xatom_CLIPBOARD;
  if (EQ (sym, QTIMESTAMP)) return Xatom_TIMESTAMP;
  if (EQ (sym, QTEXT))	    return Xatom_TEXT;
  if (EQ (sym, QDELETE))    return Xatom_DELETE;
  if (EQ (sym, QMULTIPLE))  return Xatom_MULTIPLE;
  if (EQ (sym, QINCR))	    return Xatom_INCR;
  if (EQ (sym, QEMACS_TMP)) return Xatom_EMACS_TMP;
  if (EQ (sym, QTARGETS))   return Xatom_TARGETS;
  if (EQ (sym, QNULL))	    return Xatom_NULL;
#ifdef CUT_BUFFER_SUPPORT
  if (EQ (sym, QCUT_BUFFER0)) return XA_CUT_BUFFER0;
  if (EQ (sym, QCUT_BUFFER1)) return XA_CUT_BUFFER1;
  if (EQ (sym, QCUT_BUFFER2)) return XA_CUT_BUFFER2;
  if (EQ (sym, QCUT_BUFFER3)) return XA_CUT_BUFFER3;
  if (EQ (sym, QCUT_BUFFER4)) return XA_CUT_BUFFER4;
  if (EQ (sym, QCUT_BUFFER5)) return XA_CUT_BUFFER5;
  if (EQ (sym, QCUT_BUFFER6)) return XA_CUT_BUFFER6;
  if (EQ (sym, QCUT_BUFFER7)) return XA_CUT_BUFFER7;
#endif
  if (XTYPE (sym) != Lisp_Symbol) abort ();

#if 0
  fprintf (stderr, " XInternAtom %s\n", (char *) XSYMBOL (sym)->name->data);
#endif
  BLOCK_INPUT;
  val = XInternAtom (display, (char *) XSYMBOL (sym)->name->data, False);
  UNBLOCK_INPUT;
  return val;
}


/* This converts a server Atom to a Lisp symbol, avoiding server roundtrips
   and calls to intern whenever possible.
 */
static Lisp_Object
x_atom_to_symbol (display, atom)
     Display *display;
     Atom atom;
{
  char *str;
  Lisp_Object val;
  if (! atom) return Qnil;
  if (atom == XA_PRIMARY)      return QPRIMARY;
  if (atom == XA_SECONDARY)    return QSECONDARY;
  if (atom == XA_STRING)       return QSTRING;
  if (atom == XA_INTEGER)      return QINTEGER;
  if (atom == XA_ATOM)	       return QATOM;
  if (atom == Xatom_CLIPBOARD) return QCLIPBOARD;
  if (atom == Xatom_TIMESTAMP) return QTIMESTAMP;
  if (atom == Xatom_TEXT)      return QTEXT;
  if (atom == Xatom_DELETE)    return QDELETE;
  if (atom == Xatom_MULTIPLE)  return QMULTIPLE;
  if (atom == Xatom_INCR)      return QINCR;
  if (atom == Xatom_EMACS_TMP) return QEMACS_TMP;
  if (atom == Xatom_TARGETS)   return QTARGETS;
  if (atom == Xatom_NULL)      return QNULL;
#ifdef CUT_BUFFER_SUPPORT
  if (atom == XA_CUT_BUFFER0) return QCUT_BUFFER0;
  if (atom == XA_CUT_BUFFER1) return QCUT_BUFFER1;
  if (atom == XA_CUT_BUFFER2) return QCUT_BUFFER2;
  if (atom == XA_CUT_BUFFER3) return QCUT_BUFFER3;
  if (atom == XA_CUT_BUFFER4) return QCUT_BUFFER4;
  if (atom == XA_CUT_BUFFER5) return QCUT_BUFFER5;
  if (atom == XA_CUT_BUFFER6) return QCUT_BUFFER6;
  if (atom == XA_CUT_BUFFER7) return QCUT_BUFFER7;
#endif

  BLOCK_INPUT;
  str = XGetAtomName (display, atom);
  UNBLOCK_INPUT;
#if 0
  fprintf (stderr, " XGetAtomName --> %s\n", str);
#endif
  if (! str) return Qnil;
  val = intern (str);
  XFree (str);
  return val;
}


static Lisp_Object
long_to_cons (i)
     unsigned long i;
{
  unsigned int top = i >> 16;
  unsigned int bot = i & 0xFFFF;
  if (top == 0) return make_number (bot);
  if (top == 0xFFFF) return Fcons (make_number (-1), make_number (bot));
  return Fcons (make_number (top), make_number (bot));
}

static unsigned long
cons_to_long (c)
     Lisp_Object c;
{
  int top, bot;
  if (XTYPE (c) == Lisp_Int) return XINT (c);
  top = XCONS (c)->car;
  bot = XCONS (c)->cdr;
  if (XTYPE (bot) == Lisp_Cons) bot = XCONS (bot)->car;
  return ((XINT (top) << 16) | XINT (bot));
}



/* Do protocol to assert ourself as a selection owner.
   Update the Vselection_alist so that we can reply to later requests for 
   our selection.
 */
static void
x_own_selection (selection_name, selection_value)
     Lisp_Object selection_name, selection_value;
{
  Display *display = x_current_display;
  Window selecting_window = XtWindow (selected_screen->display.x->edit_widget);
  Time time = mouse_timestamp;
  Atom selection_atom;

  CHECK_SYMBOL (selection_name, 0);
  selection_atom = symbol_to_x_atom (display, selection_name);

  BLOCK_INPUT;
  XSetSelectionOwner (display, selection_atom, selecting_window, time);
  UNBLOCK_INPUT;

  /* Now update the local cache */
  {
    Lisp_Object selection_time = long_to_cons ((unsigned long) time);
    Lisp_Object selection_data = Fcons (selection_name,
					Fcons (selection_value,
					       Fcons (selection_time, Qnil)));
    Lisp_Object prev_value = assq_no_quit (selection_name, Vselection_alist);
    Vselection_alist = Fcons (selection_data, Vselection_alist);

    /* If we already owned the selection, remove the old selection data.
       Perhaps we should destructively modify it instead.
       Don't use Fdelq() as that may QUIT;.
     */
    if (!NILP (prev_value))
      {
	Lisp_Object rest;	/* we know it's not the CAR, so it's easy. */
	for (rest = Vselection_alist; !NILP (rest); rest = Fcdr (rest))
	  if (EQ (prev_value, Fcar (XCONS (rest)->cdr)))
	    {
	      XCONS (rest)->cdr = Fcdr (XCONS (rest)->cdr);
	      break;
	    }
      }
  }
}


/* Given a selection-name and desired type, this looks up our local copy of
   the selection value and converts it to the type.  It returns nil or a
   string.  This calls random elisp code, and may signal or gc.
 */
static Lisp_Object
x_get_local_selection (selection_symbol, target_type)
     Lisp_Object selection_symbol, target_type;
{
  Lisp_Object local_value = assq_no_quit (selection_symbol, Vselection_alist);
  Lisp_Object handler_fn, value, type, check;

  if (NILP (local_value)) return Qnil;

  /* TIMESTAMP and MULTIPLE are special cases 'cause that's easiest. */
  if (EQ (target_type, QTIMESTAMP))
    {
      handler_fn = Qnil;
      value = XCONS (XCONS (XCONS (local_value)->cdr)->cdr)->car;
    }
#if 0
  else if (EQ (target_type, QDELETE))
    {
      handler_fn = Qnil;
      Fx_disown_selection_internal
	(selection_symbol,
	 XCONS (XCONS (XCONS (local_value)->cdr)->cdr)->car);
      value = QNULL;
    }
#endif

#if 0 /* #### MULTIPLE doesn't work yet */
  else if (XTYPE (target_type) == Lisp_Cons &&
	   XCONS (target_type)->car == QMULTIPLE)
    {
      Lisp_Object pairs = XCONS (target_type)->cdr;
      int size = XVECTOR (pairs)->size;
      int i;
      /* If the target is MULTIPLE, then target_type looks like
	  (MULTIPLE . [[SELECTION1 TARGET1] [SELECTION2 TARGET2] ... ])
	 We modify the second element of each pair in the vector and
	 return it as [[SELECTION1 <value1>] [SELECTION2 <value2>] ... ]
       */
      for (i = 0; i < size; i++)
	{
	  Lisp_Object pair = XVECTOR (pairs)->contents [i];
	  XVECTOR (pair)->contents [1] =
	    x_get_local_selection (XVECTOR (pair)->contents [0],
				   XVECTOR (pair)->contents [1]);
	}
      return pairs;
    }
#endif
  else
    {
      CHECK_SYMBOL (target_type, 0);
      handler_fn = Fcdr (Fassq (target_type, Vselection_converter_alist));
      if (NILP (handler_fn)) return Qnil;
      value = call3 (handler_fn,
		     selection_symbol, target_type,
		     XCONS (XCONS (local_value)->cdr)->car);
    }

  check = value;
  if (XTYPE (value) == Lisp_Cons &&
      XTYPE (XCONS (value)->car) == Lisp_Symbol)
    type = XCONS (value)->car,
    check = XCONS (value)->cdr;
  
  if (XTYPE (check) == Lisp_String ||
      XTYPE (check) == Lisp_Vector ||
      XTYPE (check) == Lisp_Symbol ||
      XTYPE (check) == Lisp_Int ||
      NILP (value))
    return value;
  else if (XTYPE (check) == Lisp_Cons &&
	   XTYPE (XCONS (check)->car) == Lisp_Int &&
	   (XTYPE (XCONS (check)->cdr) == Lisp_Int ||
	    (XTYPE (XCONS (check)->cdr) == Lisp_Cons &&
	     XTYPE (XCONS (XCONS (check)->cdr)->car) == Lisp_Int &&
	     NILP (XCONS (XCONS (check)->cdr)->cdr))))
    return value;
  else
    Fsignal (Qerror,
	     Fcons (build_string ("unrecognised selection-conversion type"),
		    Fcons (handler_fn, Fcons (value, Qnil))));
}



/* Send a SelectionNotify event to the requestor with property=None, meaning
   we were unable to do what they wanted.
 */
static void
x_decline_selection_request (event)
     XSelectionRequestEvent *event;
{
  XSelectionEvent reply;
  reply.type = SelectionNotify;
  reply.display = event->display;
  reply.requestor = event->requestor;
  reply.selection = event->selection;
  reply.time = event->time;
  reply.target = event->target;
  reply.property = None;

  BLOCK_INPUT;
  (void) XSendEvent (reply.display, reply.requestor, False, 0L,
		     (XEvent *) &reply);
  UNBLOCK_INPUT;
}


/* Used as an unwind-protect clause so that, if a selection-converter signals
   an error, we tell the requestor that we were unable to do what they wanted
   before we throw to top-level or go into the debugger or whatever.
 */
static Lisp_Object
x_selection_request_lisp_error (closure)
     Lisp_Object closure;
{
  /* Sleazy! */
  XSelectionRequestEvent *event = (XSelectionRequestEvent *) closure;
  
  if (event->type == 0) /* we set this to mean "completed normally" */
    return Qnil;
  x_decline_selection_request (event);
  return Qnil;
}


/* Convert our selection to the requested type, and put that data where the
   requestor wants it.  Then tell them whether we've succeeded.
 */
static void
x_reply_selection_request (event, format, data, size, type)
     XSelectionRequestEvent *event;
     int format, size;
     unsigned char *data;
     Atom type;
{
  XSelectionEvent reply;
  Display *display = event->display;
  Window window = event->requestor;
  int bytes_remaining;
  int format_bytes = format/8;
  int max_bytes = SELECTION_QUANTUM (display);
  if (max_bytes > MAX_SELECTION_QUANTUM) max_bytes = MAX_SELECTION_QUANTUM;

  reply.type = SelectionNotify;
  reply.display = display;
  reply.requestor = window;
  reply.selection = event->selection;
  reply.time = event->time;
  reply.target = event->target;
  reply.property = (event->property == None ? event->target : event->property);

  /* #### XChangeProperty can generate BadAlloc, and we must handle it! */

  BLOCK_INPUT;
  /* Store the data on the requested property.
     If the selection is large, only store the first N bytes of it.
   */
  bytes_remaining = size * format_bytes;
  if (bytes_remaining <= max_bytes)
    {
      /* Send all the data at once, with minimal handshaking. */
#if 0
      fprintf(stderr,"\nStoring all %d\n", bytes_remaining);
#endif
      XChangeProperty (display, window, reply.property, type, format,
		       PropModeReplace, data, size);
      /* At this point, the selection was successfully stored; ack it. */
      (void) XSendEvent (display, window, False, 0L, (XEvent *) &reply);
    }
  else
    {
      /* Send an INCR selection. */
      int i;
      int prop_id;

      if (x_window_to_screen (window)) /* #### debug */
	error ("attempt to transfer an INCR to ourself!");
#if 0
      fprintf(stderr, "\nINCR %d\n", bytes_remaining);
#endif
      prop_id = expect_property_change (display, window, reply.property,
					PropertyDelete);

      XChangeProperty (display, window, reply.property, Xatom_INCR,
		       32, PropModeReplace, (unsigned char *)
		       &bytes_remaining, 1);
      XSelectInput (display, window, PropertyChangeMask);
      /* Tell 'em the INCR data is there... */
      (void) XSendEvent (display, window, False, 0L, (XEvent *) &reply);

      /* First, wait for the requestor to ack by deleting the property.
	 This can run random lisp code (process handlers) or signal.
       */
      wait_for_property_change (prop_id);

      while (bytes_remaining)
	{
	  int i = ((bytes_remaining < max_bytes)
		   ? bytes_remaining
		   : max_bytes);
	  prop_id = expect_property_change (display, window, reply.property,
					    PropertyDelete);
#if 0
	  fprintf (stderr,"  INCR adding %d\n", i);
#endif
	  /* Append the next chunk of data to the property. */
	  XChangeProperty (display, window, reply.property, type, format,
			   PropModeAppend, data, i / format_bytes);
	  bytes_remaining -= i;
	  data += i;

	  /* Now wait for the requestor to ack this chunk by deleting the
	     property.	 This can run random lisp code or signal.
	   */
	  wait_for_property_change (prop_id);
	}
      /* Now write a zero-length chunk to the property to tell the requestor
	 that we're done. */
#if 0
      fprintf (stderr,"  INCR done\n");
#endif
      if (! waiting_for_other_props_on_window (display, window))
	XSelectInput (display, window, 0L);

      XChangeProperty (display, window, reply.property, type, format,
		       PropModeReplace, data, 0);
    }
  UNBLOCK_INPUT;
}



/* Called from the event-loop in response to a SelectionRequest event.
 */
void
x_handle_selection_request (event)
     XSelectionRequestEvent *event;
{
  struct gcpro gcpro1, gcpro2, gcpro3;
  XSelectionEvent reply;
  Lisp_Object local_selection_data = Qnil;
  Lisp_Object selection_symbol;
  Lisp_Object target_symbol = Qnil;
  Lisp_Object converted_selection = Qnil;
  Time local_selection_time;
  Lisp_Object successful_p = Qnil;
  int count;

  GCPRO3 (local_selection_data, converted_selection, target_symbol);

  reply.type = SelectionNotify;		/* Construct the reply event */
  reply.display = event->display;
  reply.requestor = event->requestor;
  reply.selection = event->selection;
  reply.time = event->time;
  reply.target = event->target;
  reply.property = (event->property == None ? event->target : event->property);

  selection_symbol = x_atom_to_symbol (reply.display, event->selection);

  local_selection_data = assq_no_quit (selection_symbol, Vselection_alist);
  
#if 0
# define CDR(x) (XCONS(x)->cdr)
# define CAR(x) (XCONS(x)->car)
  /* This list isn't user-visible, so it can't "go bad." */
  if (XTYPE (local_selection_data) != Lisp_Cons) abort ();
  if (XTYPE (CDR (local_selection_data)) != Lisp_Cons) abort ();
  if (XTYPE (CDR (CDR (local_selection_data))) != Lisp_Cons) abort ();
  if (!NILP (CDR (CDR (CDR (local_selection_data))))) abort ();
  if (XTYPE (CAR (CDR (CDR (local_selection_data)))) != Lisp_Cons) abort ();
  if (XTYPE (CAR (CAR (CDR (CDR (local_selection_data))))) != Lisp_Int) abort ();
  if (XTYPE (CDR (CAR (CDR (CDR (local_selection_data))))) != Lisp_Int) abort ();
# undef CAR
# undef CDR
#endif

  if (NILP (local_selection_data))
    {
      /* Someone asked for the selection, but we don't have it any more.
       */
      x_decline_selection_request (event);
      goto DONE;
    }

  local_selection_time = (Time)
    cons_to_long (XCONS (XCONS (XCONS (local_selection_data)->cdr)->cdr)->car);

  if (event->time != CurrentTime &&
      local_selection_time > event->time)
    {
      /* Someone asked for the selection, and we have one, but not the one
	 they're looking for.
       */
      x_decline_selection_request (event);
      goto DONE;
    }

  count = specpdl_ptr - specpdl;
  record_unwind_protect (x_selection_request_lisp_error, event);
  target_symbol = x_atom_to_symbol (reply.display, event->target);

#if 0 /* #### MULTIPLE doesn't work yet */
  if (EQ (target_symbol, QMULTIPLE))
    target_symbol = fetch_multiple_target (event);
#endif
  
  /* Convert lisp objects back into binary data */
  
  converted_selection =
    x_get_local_selection (selection_symbol, target_symbol);
  
  if (! NILP (converted_selection))
    {
      unsigned char *data;
      unsigned int size;
      int format;
      Atom type;
      lisp_data_to_selection_data (reply.display, converted_selection,
				   &data, &type, &size, &format);
      
      x_reply_selection_request (event, format, data, size, type);
      successful_p = Qt;
      /* Tell x_selection_request_lisp_error() it's cool. */
      event->type = 0;
      free (data);
    }
  unbind_to (count);

 DONE:

  UNGCPRO;

  /* Let random lisp code notice that the selection has been asked for.
   */
  {
    Lisp_Object rest;
    Lisp_Object val = Vx_sent_selection_hooks;
    if (!EQ (val, Qunbound) && !NILP (val))
      {
	if (XTYPE (val) == Lisp_Cons && !EQ (XCONS (val)->car, Qlambda))
	  for (rest = val; !NILP (rest); rest = Fcdr (rest))
	    call3 (Fcar(rest), selection_symbol, target_symbol, successful_p);
	else
	  call3 (val, selection_symbol, target_symbol, successful_p);
      }
  }
}


/* Called from the event-loop in response to a SelectionClear event.
 */
void
x_handle_selection_clear (event)
     XSelectionClearEvent *event;
{
  Display *display = event->display;
  Atom selection = event->selection;
  Time changed_owner_time = event->time;
  
  Lisp_Object selection_symbol, local_selection_data;
  Time local_selection_time;

  selection_symbol = x_atom_to_symbol (display, selection);

  local_selection_data = assq_no_quit (selection_symbol, Vselection_alist);

  /* Well, we already believe that we don't own it, so that's just fine. */
  if (NILP (local_selection_data)) return;

  local_selection_time = (Time)
    cons_to_long (XCONS (XCONS (XCONS (local_selection_data)->cdr)->cdr)->car);

  /* This SelectionClear is for a selection that we no longer own, so we can
     disregard it.  (That is, we have reasserted the selection since this
     request was generated.)
   */
  if (changed_owner_time != CurrentTime &&
      local_selection_time > changed_owner_time)
    return;

  /* Otherwise, we're really honest and truly being told to drop it.
     Don't use Fdelq() as that may QUIT;.
   */
  if (EQ (local_selection_data, Fcar (Vselection_alist)))
    Vselection_alist = Fcdr (Vselection_alist);
  else
    {
      Lisp_Object rest;
      for (rest = Vselection_alist; !NILP (rest); rest = Fcdr (rest))
	if (EQ (local_selection_data, Fcar (XCONS (rest)->cdr)))
	  {
	    XCONS (rest)->cdr = Fcdr (XCONS (rest)->cdr);
	    break;
	  }
    }

  /* Let random lisp code notice that the selection has been stolen.
   */
  {
    Lisp_Object rest;
    Lisp_Object val = Vx_lost_selection_hooks;
    if (!EQ (val, Qunbound) && !NILP (val))
      {
	if (XTYPE (val) == Lisp_Cons && !EQ (XCONS (val)->car, Qlambda))
	  for (rest = val; !NILP (rest); rest = Fcdr (rest))
	    call1 (Fcar (rest), selection_symbol);
	else
	  call1 (val, selection_symbol);
      }
  }
}


/* This stuff is so that INCR selections are reentrant (that is, so we can
   be servicing multiple INCR selection requests simultaniously.)  I haven't
   actually tested that yet.
 */

static int prop_location_tick;

static struct prop_location {
  int tick;
  Display *display;
  Window window;
  Atom property;
  int desired_state;
  struct prop_location *next;
} *for_whom_the_bell_tolls;


static int
property_deleted_p (tick)
     int tick;
{
  struct prop_location *rest = for_whom_the_bell_tolls;
  while (rest)
    if (rest->tick == tick)
      return 0;
    else
      rest = rest->next;
  return 1;
}

static int
waiting_for_other_props_on_window (display, window)
     Display *display;
     Window window;
{
  struct prop_location *rest = for_whom_the_bell_tolls;
  while (rest)
    if (rest->display == display && rest->window == window)
      return 1;
    else
      rest = rest->next;
  return 0;
}


static int
expect_property_change (display, window, property, state)
     Display *display;
     Window window;
     Lisp_Object property;
     int state;
{
  struct prop_location *pl = (struct prop_location *)
    malloc (sizeof (struct prop_location));
  pl->tick = ++prop_location_tick;
  pl->display = display;
  pl->window = window;
  pl->property = property;
  pl->desired_state = state;
  pl->next = for_whom_the_bell_tolls;
  for_whom_the_bell_tolls = pl;
  return pl->tick;
}

static void
unexpect_property_change (tick)
     int tick;
{
  struct prop_location *prev = 0, *rest = for_whom_the_bell_tolls;
  while (rest)
    {
      if (rest->tick == tick)
	{
	  if (prev)
	    prev->next = rest->next;
	  else
	    for_whom_the_bell_tolls = rest->next;
	  free (rest);
	  return;
	}
      prev = rest;
      rest = rest->next;
    }
}

static void
wait_for_property_change (tick)
     int tick;
{
  wait_delaying_user_input (property_deleted_p, tick);
}


/* Called from the event-loop in response to a PropertyNotify event.
 */
void
x_handle_property_notify (event)
     XPropertyEvent *event;
{
  struct prop_location *prev = 0, *rest = for_whom_the_bell_tolls;
  while (rest)
    {
      if (rest->property == event->atom &&
	  rest->window == event->window &&
	  rest->display == event->display &&
	  rest->desired_state == event->state)
	{
#if 0
	  fprintf (stderr, "Saw expected prop-%s on %s\n",
		   (event->state == PropertyDelete ? "delete" : "change"),
		   (char *) XSYMBOL (x_atom_to_symbol (event->display,
						       event->atom))
		   ->name->data);
#endif
	  if (prev)
	    prev->next = rest->next;
	  else
	    for_whom_the_bell_tolls = rest->next;
	  free (rest);
	  return;
	}
      prev = rest;
      rest = rest->next;
    }
#if 0
  fprintf (stderr, "Saw UNexpected prop-%s on %s\n",
	   (event->state == PropertyDelete ? "delete" : "change"),
	   (char *) XSYMBOL (x_atom_to_symbol (event->display, event->atom))
	   ->name->data);
#endif
}



#if 0 /* #### MULTIPLE doesn't work yet */

static Lisp_Object
fetch_multiple_target (event)
     XSelectionRequestEvent *event;
{
  Display *display = event->display;
  Window window = event->requestor;
  Atom target = event->target;
  Atom selection_atom = event->selection;
  int result;

  return
    Fcons (QMULTIPLE,
	   x_get_window_property_as_lisp_data (display, window, target,
					       QMULTIPLE, selection_atom));
}

static Lisp_Object
copy_multiple_data (obj)
     Lisp_Object obj;
{
  Lisp_Object vec;
  int i;
  int size;
  if (XTYPE (obj) == Lisp_Cons)
    return Fcons (XCONS (obj)->car, copy_multiple_data (XCONS (obj)->cdr));
    
  CHECK_VECTOR (obj, 0);
  vec = Fmake_vector (size = XVECTOR (obj)->size, Qnil);
  for (i = 0; i < size; i++)
    {
      Lisp_Object vec2 = XVECTOR (obj)->contents [i];
      CHECK_VECTOR (vec2, 0);
      if (XVECTOR (vec2)->size != 2)
	Fsignal (Qerror, Fcons (build_string ("vectors must be of length 2"),
				Fcons (vec2, Qnil)));
      XVECTOR (vec)->contents [i] = Fmake_vector (2, Qnil);
      XVECTOR (XVECTOR (vec)->contents [i])->contents [0] =
	XVECTOR (vec2)->contents [0];
      XVECTOR (XVECTOR (vec)->contents [i])->contents [1] =
	XVECTOR (vec2)->contents [1];
    }
  return vec;
}

#endif


static int reading_selection_reply;
static Atom reading_which_selection;

static int
selection_reply_done (ignore)
     char *ignore;
{
  return !reading_selection_reply;
}

/* Do protocol to read selection-data from the server.
   Converts this to lisp data and returns it.
 */
static Lisp_Object
x_get_foreign_selection (selection_symbol, target_type)
     Lisp_Object selection_symbol, target_type;
{
  Display *display = x_current_display;
  Window requestor_window = XtWindow (selected_screen->display.x->edit_widget);
  Time requestor_time = mouse_timestamp;
  Atom target_property = Xatom_EMACS_TMP;
  Atom selection_atom = symbol_to_x_atom (display, selection_symbol);
  Atom type_atom;

  if (XTYPE (target_type) == Lisp_Cons)
    type_atom = symbol_to_x_atom (display, XCONS (target_type)->car);
  else
    type_atom = symbol_to_x_atom (display, target_type);

  BLOCK_INPUT;
  XConvertSelection (display, selection_atom, type_atom, target_property,
		     requestor_window, requestor_time);
  UNBLOCK_INPUT;

  /* Block until the reply has been read. */
  reading_selection_reply = (int) requestor_window;
  reading_which_selection = selection_atom;

  wait_delaying_user_input (selection_reply_done, 0);
  /* If the above returns (not ^Ged) then the selection is waiting for us
     on the requested property.
   */
  return
    x_get_window_property_as_lisp_data (display, requestor_window,
					target_property, target_type,
					selection_atom);
}


static void
x_get_window_property (display, window, property, data_ret, bytes_ret,
		       actual_type_ret, actual_format_ret, actual_size_ret,
		       delete_p)
     Display *display;
     Window window;
     Atom property;
     unsigned char **data_ret;
     int *bytes_ret;
     Atom *actual_type_ret;
     int *actual_format_ret;
     unsigned long *actual_size_ret;
     int delete_p;
{
  int remaining;
  int total_size;
  unsigned long bytes_remaining;
  int offset = 0;
  unsigned char *tmp_data = 0;
  int result;
  int buffer_size = SELECTION_QUANTUM (display);
  if (buffer_size > MAX_SELECTION_QUANTUM) buffer_size = MAX_SELECTION_QUANTUM;
  
  BLOCK_INPUT;
  /* First probe the thing to find out how big it is. */
  result = XGetWindowProperty (display, window, property,
			       0, 0, False, AnyPropertyType,
			       actual_type_ret, actual_format_ret,
			       actual_size_ret,
			       &bytes_remaining, &tmp_data);
  UNBLOCK_INPUT;
  if (result != Success)
    {
      *data_ret = 0;
      *bytes_ret = 0;
      return;
    }
  XFree ((char *) tmp_data);
  
  if (*actual_type_ret == None || *actual_format_ret == 0)
    {
      if (delete_p) XDeleteProperty (display, window, property);
      return;
    }

  total_size = bytes_remaining + 1;
  *data_ret = (unsigned char *) malloc (total_size);
  
  /* Now read, until weve gotten it all. */
  BLOCK_INPUT;
  while (bytes_remaining)
    {
#if 0
      int last = bytes_remaining;
#endif
      result =
	XGetWindowProperty (display, window, property,
			    offset/4, buffer_size/4,
			    (delete_p ? True : False),
			    AnyPropertyType,
			    actual_type_ret, actual_format_ret,
			    actual_size_ret, &bytes_remaining, &tmp_data);
#if 0
      fprintf (stderr, "<< read %d\n", last-bytes_remaining);
#endif
      /* If this doesn't return Success at this point, it means that
	 some clod deleted the selection while we were in the midst of
	 reading it.  Deal with that, I guess....
       */
      if (result != Success) break;
      *actual_size_ret *= *actual_format_ret / 8;
      bcopy (tmp_data, (*data_ret) + offset, *actual_size_ret);
      offset += *actual_size_ret;
      XFree ((char *) tmp_data);
    }
  UNBLOCK_INPUT;
  *bytes_ret = offset;
}


static void
receive_incremental_selection (display, window, property, target_type,
			       min_size_bytes, data_ret, size_bytes_ret,
			       type_ret, format_ret, size_ret)
     Display *display;
     Window window;
     Atom property;
     Lisp_Object target_type; /* for error messages only */
     unsigned int min_size_bytes;
     unsigned char **data_ret;
     int *size_bytes_ret;
     Atom *type_ret;
     unsigned long *size_ret;
     int *format_ret;
{
  int offset = 0;
  int prop_id;
  *size_bytes_ret = min_size_bytes;
  *data_ret = (unsigned char *) malloc (*size_bytes_ret);
#if 0
  fprintf (stderr, "\nread INCR %d\n", min_size_bytes);
#endif
  /* At this point, we have read an INCR property, and deleted it (which
     is how we ack its receipt: the sending window will be selecting
     PropertyNotify events on our window to notice this.)

     Now, we must loop, waiting for the sending window to put a value on
     that property, then reading the property, then deleting it to ack.
     We are done when the sender places a property of length 0.
   */
  prop_id = expect_property_change (display, window, property,
				    PropertyNewValue);
  while (1)
    {
      unsigned char *tmp_data;
      int tmp_size_bytes;
      wait_for_property_change (prop_id);
      /* expect it again immediately, because x_get_window_property may
	 .. no it wont, I dont get it.
	 .. Ok, I get it now, the Xt code that implements INCR is broken.
       */
      prop_id = expect_property_change (display, window, property,
					PropertyNewValue);
      x_get_window_property (display, window, property,
			     &tmp_data, &tmp_size_bytes,
			     type_ret, format_ret, size_ret, 1);

      if (tmp_size_bytes == 0) /* we're done */
	{
#if 0
	  fprintf (stderr, "  read INCR done\n");
#endif
	  unexpect_property_change (prop_id);
	  if (tmp_data) free (tmp_data);
	  break;
	}
#if 0
      fprintf (stderr, "  read INCR %d\n", tmp_size_bytes);
#endif
      if (*size_bytes_ret < offset + tmp_size_bytes)
	{
#if 0
	  fprintf (stderr, "  read INCR realloc %d -> %d\n",
		   *size_bytes_ret, offset + tmp_size_bytes);
#endif
	  *size_bytes_ret = offset + tmp_size_bytes;
	  *data_ret = (unsigned char *) realloc (*data_ret, *size_bytes_ret);
	}
      bcopy (tmp_data, (*data_ret) + offset, tmp_size_bytes);
      offset += tmp_size_bytes;
      free (tmp_data);
    }
}


static Lisp_Object
x_get_window_property_as_lisp_data (display, window, property, target_type,
				    selection_atom)
     Display *display;
     Window window;
     Atom property;
     Lisp_Object target_type;	/* for error messages only */
     Atom selection_atom;	/* for error messages only */
{
  Atom actual_type;
  int actual_format;
  unsigned long actual_size;
  unsigned char *data = 0;
  int bytes = 0;
  Lisp_Object val;

  x_get_window_property (display, window, property, &data, &bytes,
			 &actual_type, &actual_format, &actual_size, 1);
  if (! data)
    {
      int there_is_a_selection_owner;
      BLOCK_INPUT;
      there_is_a_selection_owner =
	XGetSelectionOwner (display, selection_atom);
      UNBLOCK_INPUT;
      while (1) /* don't let debugger return here */
	Fsignal (Qerror,
		 there_is_a_selection_owner ?
		 Fcons (build_string ("selection owner couldn't convert"),
			actual_type
			? Fcons (target_type,
				 Fcons (x_atom_to_symbol (display, actual_type),
					Qnil))
			: Fcons (target_type, Qnil))
		 : Fcons (build_string ("no selection"),
			  Fcons (x_atom_to_symbol (display, selection_atom),
				 Qnil)));
    }
  
  if (actual_type == Xatom_INCR)
    {
      /* Ok, that data wasnt *the* data, it was just the beginning. */

      unsigned int min_size_bytes = * ((unsigned int *) data);
      XFree ((char *) data);
      receive_incremental_selection (display, window, property, target_type,
				     min_size_bytes, &data, &bytes,
				     &actual_type, &actual_format,
				     &actual_size);
    }

  /* It's been read.  Now convert it to a lisp object in some semi-rational
     manner.
   */
  val = selection_data_to_lisp_data (display, data, bytes,
				     actual_type, actual_format);
  
  free ((char *) data);
  return val;
}

/* These functions convert from the selection data read from the server into
   something that we can use from elisp, and vice versa.

	Type:	Format:	Size:		Elisp Type:
	-----	-------	-----		-----------
	*	8	*		String
	ATOM	32	1		Symbol
	ATOM	32	> 1		Vector of Symbols
	*	16	1		Integer
	*	16	> 1		Vector of Integers
	*	32	1		if <=16 bits: Integer
					if > 16 bits: Cons of top16, bot16
	*	32	> 1		Vector of the above

   When converting a Lisp number to C, it is assumed to be of format 16 if
   it is an integer, and of format 32 if it is a cons of two integers.

   When converting a vector of numbers from Elisp to C, it is assumed to be
   of format 16 if every element in the vector is an integer, and is assumed
   to be of format 32 if any element is a cons of two integers.

   When converting an object to C, it may be of the form (SYMBOL . <data>)
   where SYMBOL is what we should claim that the type is.  Format and
   representation are as above.
 */


static Lisp_Object
selection_data_to_lisp_data (display, data, size, type, format)
     Display *display;
     unsigned char *data;
     Atom type;
     int size, format;
{

  if (type == Xatom_NULL)
    return QNULL;

  /* Convert any 8-bit data to a string, for compactness. */
  else if (format == 8)
    return make_string (data, size);

  /* Convert a single atom to a Lisp_Symbol.  Convert a set of atoms to
     a vector of symbols.
   */
  else if (type == XA_ATOM)
    {
      int i;
      if (size == sizeof (Atom))
	return x_atom_to_symbol (display, *((Atom *) data));
      else
	{
	  Lisp_Object v = Fmake_vector (size / sizeof (Atom), 0);
	  for (i = 0; i < size / sizeof (Atom); i++)
	    Faset (v, i, x_atom_to_symbol (display, ((Atom *) data) [i]));
	  return v;
	}
    }

  /* Convert a single 16 or small 32 bit number to a Lisp_Int.
     If the number is > 16 bits, convert it to a cons of integers,
     16 bits in each half.
   */
  else if (format == 32 && size == sizeof (long))
    return long_to_cons (((unsigned long *) data) [0]);
  else if (format == 16 && size == sizeof (short))
    return make_number ((int) (((unsigned short *) data) [0]));

  /* Convert any other kind of data to a vector of numbers, represented
     as above (as an integer, or a cons of two 16 bit integers.)
   */
  else if (format == 16)
    {
      int i;
      Lisp_Object v = Fmake_vector (size / 4, 0);
      for (i = 0; i < size / 4; i++)
	{
	  int j = (int) ((unsigned short *) data) [i];
	  Faset (v, i, make_number (j));
	}
      return v;
    }
  else
    {
      int i;
      Lisp_Object v = Fmake_vector (size / 4, 0);
      for (i = 0; i < size / 4; i++)
	{
	  unsigned long j = ((unsigned long *) data) [i];
	  Faset (v, i, long_to_cons (j));
	}
      return v;
    }
}


static void
lisp_data_to_selection_data (display, obj,
			     data_ret, type_ret, size_ret, format_ret)
     Display *display;
     Lisp_Object obj;
     unsigned char **data_ret;
     Atom *type_ret;
     unsigned int *size_ret;
     int *format_ret;
{
  Lisp_Object type = Qnil;
  if (XTYPE (obj) == Lisp_Cons && XTYPE (XCONS (obj)->car) == Lisp_Symbol)
    {
      type = XCONS (obj)->car;
      obj = XCONS (obj)->cdr;
      if (XTYPE (obj) == Lisp_Cons && NILP (XCONS (obj)->cdr))
	obj = XCONS (obj)->car;
    }

  if (EQ (obj, QNULL) || (EQ (type, QNULL)))
    {				/* This is not the same as declining */
      *format_ret = 32;
      *size_ret = 0;
      *data_ret = 0;
      type = QNULL;
    }
  else if (XTYPE (obj) == Lisp_String)
    {
      *format_ret = 8;
      *size_ret = XSTRING (obj)->size;
      *data_ret = (unsigned char *) malloc (*size_ret);
      bcopy ((char *) XSTRING (obj)->data, *data_ret, *size_ret);
      if (NILP (type)) type = QSTRING;
    }
  else if (XTYPE (obj) == Lisp_Symbol)
    {
      *format_ret = 32;
      *size_ret = 1;
      *data_ret = (unsigned char *) malloc (sizeof (Atom) + 1);
      (*data_ret) [sizeof (Atom)] = 0;
      (*(Atom **) data_ret) [0] = symbol_to_x_atom (display, obj);
      if (NILP (type)) type = QATOM;
    }
  else if (XTYPE (obj) == Lisp_Int &&
	   XINT (obj) < 0xFFFF &&
	   XINT (obj) > -0xFFFF)
    {
      *format_ret = 16;
      *size_ret = 1;
      *data_ret = (unsigned char *) malloc (sizeof (short) + 1);
      (*data_ret) [sizeof (short)] = 0;
      (*(short **) data_ret) [0] = (short) XINT (obj);
      if (NILP (type)) type = QINTEGER;
    }
  else if (XTYPE (obj) == Lisp_Int || XTYPE (obj) == Lisp_Cons)
    {
      *format_ret = 32;
      *size_ret = 1;
      *data_ret = (unsigned char *) malloc (sizeof (long) + 1);
      (*data_ret) [sizeof (long)] = 0;
      (*(unsigned long **) data_ret) [0] = cons_to_long (obj);
      if (NILP (type)) type = QINTEGER;
    }
  else if (XTYPE (obj) == Lisp_Vector)
    {
      /* Lisp_Vectors may represent a set of ATOMs;
	 a set of 16 or 32 bit INTEGERs;
	 or a set of ATOM_PAIRs (represented as [[A1 A2] [A3 A4] ...]
       */
      int i;

      if (XTYPE (XVECTOR (obj)->contents [0]) == Lisp_Symbol)
	/* This vector is an ATOM set */
	{
	  if (NILP (type)) type = QATOM;
	  *size_ret = XVECTOR (obj)->size;
	  *format_ret = 32;
	  *data_ret = (unsigned char *) malloc ((*size_ret) * sizeof (Atom));
	  for (i = 0; i < *size_ret; i++)
	    if (XTYPE (XVECTOR (obj)->contents [i]) == Lisp_Symbol)
	      (*(Atom **) data_ret) [i] =
		symbol_to_x_atom (display, XVECTOR (obj)->contents [i]);
	    else
	      Fsignal (Qerror, /* Qselection_error */
		       Fcons (build_string
		   ("all elements of the vector must be of the same type"),
			      Fcons (obj, Qnil)));
	}
#if 0 /* #### MULTIPLE doesn't work yet */
      else if (XTYPE (XVECTOR (obj)->contents [0]) == Lisp_Vector)
	/* This vector is an ATOM_PAIR set */
	{
	  if (NILP (type)) type = QATOM_PAIR;
	  *size_ret = XVECTOR (obj)->size;
	  *format_ret = 32;
	  *data_ret = (unsigned char *)
	    malloc ((*size_ret) * sizeof (Atom) * 2);
	  for (i = 0; i < *size_ret; i++)
	    if (XTYPE (XVECTOR (obj)->contents [i]) == Lisp_Vector)
	      {
		Lisp_Object pair = XVECTOR (obj)->contents [i];
		if (XVECTOR (pair)->size != 2)
		  Fsignal (Qerror,
			   Fcons (build_string 
       ("elements of the vector must be vectors of exactly two elements"),
				  Fcons (pair, Qnil)));
		
		(*(Atom **) data_ret) [i * 2] =
		  symbol_to_x_atom (display, XVECTOR (pair)->contents [0]);
		(*(Atom **) data_ret) [(i * 2) + 1] =
		  symbol_to_x_atom (display, XVECTOR (pair)->contents [1]);
	      }
	    else
	      Fsignal (Qerror,
		       Fcons (build_string
		   ("all elements of the vector must be of the same type"),
			      Fcons (obj, Qnil)));
	  
	}
#endif
      else
	/* This vector is an INTEGER set, or something like it */
	{
	  *size_ret = XVECTOR (obj)->size;
	  if (NILP (type)) type = QINTEGER;
	  *format_ret = 16;
	  for (i = 0; i < *size_ret; i++)
	    if (XTYPE (XVECTOR (obj)->contents [i]) == Lisp_Cons)
	      *format_ret = 32;
	    else if (XTYPE (XVECTOR (obj)->contents [i]) != Lisp_Int)
	      Fsignal (Qerror, /* Qselection_error */
		       Fcons (build_string
	("all elements of the vector must be integers or conses of integers"),
			      Fcons (obj, Qnil)));

	  *data_ret = (unsigned char *) malloc (*size_ret * (*format_ret/8));
	  for (i = 0; i < *size_ret; i++)
	    if (*format_ret == 32)
	      (*((unsigned long **) data_ret)) [i] =
		cons_to_long (XVECTOR (obj)->contents [i]);
	    else
	      (*((unsigned short **) data_ret)) [i] =
		(unsigned short) cons_to_long (XVECTOR (obj)->contents [i]);
	}
    }
  else
    Fsignal (Qerror, /* Qselection_error */
	     Fcons (build_string ("unrecognised selection data"),
		    Fcons (obj, Qnil)));

  *type_ret = symbol_to_x_atom (display, type);
}

static Lisp_Object
clean_local_selection_data (obj)
     Lisp_Object obj;
{
  if (XTYPE (obj) == Lisp_Cons &&
      XTYPE (XCONS (obj)->car) == Lisp_Int &&
      XTYPE (XCONS (obj)->cdr) == Lisp_Cons &&
      XTYPE (XCONS (XCONS (obj)->cdr)->car) == Lisp_Int &&
      NILP (XCONS (XCONS (obj)->cdr)->cdr))
    obj = Fcons (XCONS (obj)->car, XCONS (obj)->cdr);

  if (XTYPE (obj) == Lisp_Cons &&
      XTYPE (XCONS (obj)->car) == Lisp_Int &&
      XTYPE (XCONS (obj)->cdr) == Lisp_Int)
    {
      if (XINT (XCONS (obj)->car) == 0)
	return XCONS (obj)->cdr;
      if (XINT (XCONS (obj)->car) == -1)
	return make_number (- XINT (XCONS (obj)->cdr));
    }
  if (XTYPE (obj) == Lisp_Vector)
    {
      int i;
      int size = XVECTOR (obj)->size;
      Lisp_Object copy;
      if (size == 1)
	return clean_local_selection_data (XVECTOR (obj)->contents [0]);
      copy = Fmake_vector (size, Qnil);
      for (i = 0; i < size; i++)
	XVECTOR (copy)->contents [i] =
	  clean_local_selection_data (XVECTOR (obj)->contents [i]);
      return copy;
    }
  return obj;
}


/* Called from the event loop to handle SelectionNotify events.
   I don't think this needs to be reentrant.
 */
void
x_handle_selection_notify (event)
     XSelectionEvent *event;
{
  if (! reading_selection_reply)
    {
      message ("received an unexpected SelectionNotify event");
      return;
    }
  if (event->requestor != reading_selection_reply)
    {
      message ("received a SelectionNotify event for the wrong window");
      return;
    }
  if (event->selection != reading_which_selection)
    {
      message ("received the wrong selection type in SelectionNotify!");
      return;
    }

  reading_selection_reply = 0; /* we're done now. */
}


DEFUN ("x-own-selection-internal",
       Fx_own_selection_internal, Sx_own_selection_internal,
  2, 2, 0,
  "Assert an X selection of the given TYPE with the given VALUE.\n\
TYPE is a symbol, typically PRIMARY, SECONDARY, or CLIPBOARD.\n\
VALUE is typically a string, or a cons of two markers, but may be\n\
anything that the functions on selection-converter-alist know about.")
  (selection_name, selection_value)
     Lisp_Object selection_name, selection_value;
{
  CHECK_SYMBOL (selection_name, 0);
  if (NILP (selection_value)) error ("selection-value may not be nil.");
  x_own_selection (selection_name, selection_value);
  return selection_value;
}


/* Request the selection value from the owner.  If we are the owner,
   simply return our selection value.  If we are not the owner, this
   will block until all of the data has arrived.
 */
DEFUN ("x-get-selection-internal",
       Fx_get_selection_internal, Sx_get_selection_internal, 2, 2, 0,
  "Return text selected from some X window.\n\
SELECTION is a symbol, typically PRIMARY, SECONDARY, or CLIPBOARD.\n\
TYPE is the type of data desired, typically STRING.")
  (selection_symbol, target_type)
     Lisp_Object selection_symbol, target_type;
{
  Lisp_Object val = Qnil;
  struct gcpro gcpro1, gcpro2;
  GCPRO2 (target_type, val); /* we store newly consed data into these */
  CHECK_SYMBOL (selection_symbol, 0);

#if 0 /* #### MULTIPLE doesn't work yet */
  if (XTYPE (target_type) == Lisp_Cons &&
      XCONS (target_type)->car == QMULTIPLE)
    {
      CHECK_VECTOR (XCONS (target_type)->cdr, 0);
      /* So we don't destructively modify this... */
      target_type = copy_multiple_data (target_type);
    }
  else
#endif
    CHECK_SYMBOL (target_type, 0);

  val = x_get_local_selection (selection_symbol, target_type);

  if (NILP (val))
    {
      val = x_get_foreign_selection (selection_symbol, target_type);
      goto DONE;
    }

  if (XTYPE (val) == Lisp_Cons &&
      XTYPE (XCONS (val)->car) == Lisp_Symbol)
    {
      val = XCONS (val)->cdr;
      if (XTYPE (val) == Lisp_Cons && NILP (XCONS (val)->cdr))
	val = XCONS (val)->car;
    }
  val = clean_local_selection_data (val);
 DONE:
  UNGCPRO;
  return val;
}

DEFUN ("x-disown-selection-internal",
       Fx_disown_selection_internal, Sx_disown_selection_internal, 1, 2, 0,
 "If we own the named selection, then disown it (make there be no selection).")
     (selection, time)
     Lisp_Object selection;
     Lisp_Object time;
{
  Display *display = x_current_display;
  Time timestamp;
  Atom selection_atom;
  XSelectionClearEvent event;

  CHECK_SYMBOL (selection, 0);
  if (NILP (time))
    timestamp = mouse_timestamp;
  else
    timestamp = cons_to_long (time);

  if (NILP (assq_no_quit (selection, Vselection_alist)))
    return Qnil;  /* Don't disown the selection when we're not the owner. */

  selection_atom = symbol_to_x_atom (display, selection);

  BLOCK_INPUT;
  XSetSelectionOwner (display, selection_atom, None, timestamp);
  UNBLOCK_INPUT;

  /* It doesn't seem to be guarenteed that a SelectionClear event will be
     generated for a window which owns the selection when that window sets
     the selection owner to None.  The NCD server does, the MIT Sun4 server
     doesn't.  So we synthesize one; this means we might get two, but
     that's ok, because the second one won't have any effect.
   */
  event.display = display;
  event.selection = selection_atom;
  event.time = timestamp;
  x_handle_selection_clear (&event);

  return Qt;
}


DEFUN ("x-selection-owner-p",
       Fx_selection_owner_p, Sx_selection_owner_p, 0, 1, 0,
       "Whether the current emacs process owns the given X Selection.\n\
The arg should be the name of the selection in question, typically one of\n\
the symbols PRIMARY, SECONDARY, or CLIPBOARD.  (For convenience, the symbol\n\
nil is the same as PRIMARY, and t is the same as SECONDARY.)")
     (selection)
     Lisp_Object selection;
{
  CHECK_SYMBOL (selection, 0);
  if (EQ (selection, Qnil)) selection = QPRIMARY;
  if (EQ (selection, Qt)) selection = QSECONDARY;
  
  if (NILP (Fassq (selection, Vselection_alist)))
    return Qnil;
  return Qt;
}

DEFUN ("x-selection-exists-p",
       Fx_selection_exists_p, Sx_selection_exists_p, 0, 1, 0,
       "Whether there is an owner for the given X Selection.\n\
The arg should be the name of the selection in question, typically one of\n\
the symbols PRIMARY, SECONDARY, or CLIPBOARD.  (For convenience, the symbol\n\
nil is the same as PRIMARY, and t is the same as SECONDARY.)")
     (selection)
     Lisp_Object selection;
{
  Window owner;
  Display *dpy = x_current_display;
  CHECK_SYMBOL (selection, 0);
  if (!NILP (Fx_selection_owner_p (selection)))
    return Qt;
  BLOCK_INPUT;
  owner = XGetSelectionOwner (dpy, symbol_to_x_atom (dpy, selection));
  UNBLOCK_INPUT;
  return (owner ? Qt : Qnil);
}


#ifdef CUT_BUFFER_SUPPORT

static int cut_buffers_initialized; /* Whether we're sure they all exist */

/* Ensure that all 8 cut buffers exist.  ICCCM says we gotta... */
static void
initialize_cut_buffers (display, window)
     Display *display;
     Window window;
{
  unsigned char *data = (unsigned char *) "";
  BLOCK_INPUT;
#define FROB(atom) XChangeProperty (display, window, atom, XA_STRING, 8, \
				    PropModeAppend, data, 0)
  FROB (XA_CUT_BUFFER0);
  FROB (XA_CUT_BUFFER1);
  FROB (XA_CUT_BUFFER2);
  FROB (XA_CUT_BUFFER3);
  FROB (XA_CUT_BUFFER4);
  FROB (XA_CUT_BUFFER5);
  FROB (XA_CUT_BUFFER6);
  FROB (XA_CUT_BUFFER7);
#undef FROB
  UNBLOCK_INPUT;
  cut_buffers_initialized = 1;
}


#define CHECK_CUTBUFFER(symbol,n)					\
  { CHECK_SYMBOL((symbol),(n));						\
    if (!EQ((symbol),QCUT_BUFFER0) && !EQ((symbol),QCUT_BUFFER1) &&	\
	!EQ((symbol),QCUT_BUFFER2) && !EQ((symbol),QCUT_BUFFER3) &&	\
	!EQ((symbol),QCUT_BUFFER4) && !EQ((symbol),QCUT_BUFFER5) &&	\
	!EQ((symbol),QCUT_BUFFER6) && !EQ((symbol),QCUT_BUFFER7))	\
      Fsignal(Qerror, Fcons (build_string ("doesn't name a cutbuffer"),	\
			     Fcons ((symbol), Qnil)));			\
  }

DEFUN ("x-get-cutbuffer-internal", Fx_get_cutbuffer_internal,
       Sx_get_cutbuffer_internal, 1, 1, 0,
       "Returns the value of the named cutbuffer (typically CUT_BUFFER0).")
     (buffer)
     Lisp_Object buffer;
{
  Display *display = x_current_display;
  Window window = RootWindow (display, 0); /* Cutbuffers are on screen 0 */
  Atom buffer_atom;
  unsigned char *data;
  int bytes;
  Atom type;
  int format;
  unsigned long size;
  Lisp_Object ret;

  CHECK_CUTBUFFER (buffer, 0);
  buffer_atom = symbol_to_x_atom (display, buffer);

  x_get_window_property (display, window, buffer_atom, &data, &bytes,
			 &type, &format, &size, 0);
  if (!data) return Qnil;
  
  if (format != 8 || type != XA_STRING)
    Fsignal (Qerror,
	     Fcons (build_string ("cut buffer doesn't contain 8-bit data"),
		    Fcons (x_atom_to_symbol (display, type),
			   Fcons (make_number (format), Qnil))));

  ret = (bytes ? make_string (data, bytes) : Qnil);
  free (data);
  return ret;
}


DEFUN ("x-store-cutbuffer-internal", Fx_store_cutbuffer_internal,
       Sx_store_cutbuffer_internal, 2, 2, 0,
       "Sets the value of the named cutbuffer (typically CUT_BUFFER0).")
     (buffer, string)
     Lisp_Object buffer, string;
{
  Display *display = x_current_display;
  Window window = RootWindow (display, 0); /* Cutbuffers are on screen 0 */
  Atom buffer_atom;
  unsigned char *data;
  int bytes;
  int bytes_remaining;
  int max_bytes = SELECTION_QUANTUM (display);
  if (max_bytes > MAX_SELECTION_QUANTUM) max_bytes = MAX_SELECTION_QUANTUM;

  CHECK_CUTBUFFER (buffer, 0);
  CHECK_STRING (string, 0);
  buffer_atom = symbol_to_x_atom (display, buffer);
  data = (unsigned char *) XSTRING (string)->data;
  bytes = XSTRING (string)->size;
  bytes_remaining = bytes;

  if (! cut_buffers_initialized) initialize_cut_buffers (display, window);

  BLOCK_INPUT;
  while (bytes_remaining)
    {
      int chunk = (bytes_remaining < max_bytes
		   ? bytes_remaining : max_bytes);
      XChangeProperty (display, window, buffer_atom, XA_STRING, 8,
		       (bytes_remaining == bytes
			? PropModeReplace
			: PropModeAppend),
		       data, chunk);
      data += chunk;
      bytes_remaining -= chunk;
    }
  UNBLOCK_INPUT;
  return string;
}


DEFUN ("x-rotate-cutbuffers-internal", Fx_rotate_cutbuffers_internal,
       Sx_rotate_cutbuffers_internal, 1, 1, 0,
       "Rotate the values of the cutbuffers by the given number of steps;\n\
positive means move values forward, negative means backward.")
     (n)
     Lisp_Object n;
{
  Display *display = x_current_display;
  Window window = RootWindow (display, 0); /* Cutbuffers are on screen 0 */
  Atom props [8];

  CHECK_NUMBER (n, 0);
  if (XINT (n) == 0) return n;
  if (! cut_buffers_initialized) initialize_cut_buffers (display, window);
  props[0] = XA_CUT_BUFFER0;
  props[1] = XA_CUT_BUFFER1;
  props[2] = XA_CUT_BUFFER2;
  props[3] = XA_CUT_BUFFER3;
  props[4] = XA_CUT_BUFFER4;
  props[5] = XA_CUT_BUFFER5;
  props[6] = XA_CUT_BUFFER6;
  props[7] = XA_CUT_BUFFER7;
  BLOCK_INPUT;
  XRotateWindowProperties (display, window, props, 8, XINT (n));
  UNBLOCK_INPUT;
  return n;
}

#endif


void
syms_of_xselect ()
{
  defsubr (&Sx_get_selection_internal);
  defsubr (&Sx_own_selection_internal);
  defsubr (&Sx_disown_selection_internal);
  defsubr (&Sx_selection_owner_p);
  defsubr (&Sx_selection_exists_p);

#ifdef CUT_BUFFER_SUPPORT
  defsubr (&Sx_get_cutbuffer_internal);
  defsubr (&Sx_store_cutbuffer_internal);
  defsubr (&Sx_rotate_cutbuffers_internal);
  cut_buffers_initialized = 0;
#endif

  reading_selection_reply = 0;
  reading_which_selection = 0;
  for_whom_the_bell_tolls = 0;
  prop_location_tick = 0;

  Vselection_alist = Qnil;
  staticpro (&Vselection_alist);

  DEFVAR_LISP ("selection-converter-alist", &Vselection_converter_alist,
  "An alist associating selection-types (such as STRING and TIMESTAMP) with\n\
functions.  These functions will be called with three args: the name of the\n\
selection (typically PRIMARY, SECONDARY, or CLIPBOARD); a desired type to\n\
which the selection should be converted; and the local selection value\n\
(whatever had been passed to `x-own-selection').  These functions should\n\
return the value to send to the X server (typically a string).  A return\n\
value of nil means that the conversion could not be done.  A return value\n\
which is the symbol NULL means that a side-effect was executed, and there\n\
is no meaningful return value.");
  Vselection_converter_alist = Qnil;

  DEFVAR_LISP ("x-lost-selection-hooks", &Vx_lost_selection_hooks,
  "A function or functions to be called after the X server has notified us\n\
that we have lost the selection.  The function(s) will be called with one\n\
argument, a symbol naming the selection (typically PRIMARY, SECONDARY, or\n\
CLIPBOARD.)");
  Vx_lost_selection_hooks = Qunbound;

  DEFVAR_LISP ("x-sent-selection-hooks", &Vx_sent_selection_hooks,
  "A function or functions to be called after we have responded to some\n\
other client's request for the value of a selection that we own.  The\n\
function(s) will be called with four arguments:\n\
  - the name of the selection (typically PRIMARY, SECONDARY, or CLIPBOARD);\n\
  - the name of the selection-type which we were requested to convert the\n\
    selection into before sending (for example, STRING or LENGTH);\n\
  - and whether we successfully transmitted the selection.\n\
We might have failed (and declined the request) for any number of reasons,\n\
including being asked for a selection that we no longer own, or being asked\n\
to convert into a type that we don't know about or that is inappropriate.\n\
This hook doesn't let you change the behavior of emacs's selection replies,\n\
it merely informs you that they have happened.");
  Vx_sent_selection_hooks = Qunbound;

  QPRIMARY   = intern ("PRIMARY");	staticpro (&QPRIMARY);
  QSECONDARY = intern ("SECONDARY");	staticpro (&QSECONDARY);
  QSTRING    = intern ("STRING");	staticpro (&QSTRING);
  QINTEGER   = intern ("INTEGER");	staticpro (&QINTEGER);
  QCLIPBOARD = intern ("CLIPBOARD");	staticpro (&QCLIPBOARD);
  QTIMESTAMP = intern ("TIMESTAMP");	staticpro (&QTIMESTAMP);
  QTEXT      = intern ("TEXT"); 	staticpro (&QTEXT);
  QTIMESTAMP = intern ("TIMESTAMP");	staticpro (&QTIMESTAMP);
  QDELETE    = intern ("DELETE");	staticpro (&QDELETE);
  QMULTIPLE  = intern ("MULTIPLE");	staticpro (&QMULTIPLE);
  QINCR      = intern ("INCR");		staticpro (&QINCR);
  QEMACS_TMP = intern ("_EMACS_TMP_");	staticpro (&QEMACS_TMP);
  QTARGETS   = intern ("TARGETS");	staticpro (&QTARGETS);
  QATOM	     = intern ("ATOM");		staticpro (&QATOM);
  QATOM_PAIR = intern ("ATOM_PAIR");	staticpro (&QATOM_PAIR);
  QNULL	     = intern ("NULL");		staticpro (&QNULL);

#ifdef CUT_BUFFER_SUPPORT
  QCUT_BUFFER0 = intern ("CUT_BUFFER0"); staticpro (&QCUT_BUFFER0);
  QCUT_BUFFER1 = intern ("CUT_BUFFER1"); staticpro (&QCUT_BUFFER1);
  QCUT_BUFFER2 = intern ("CUT_BUFFER2"); staticpro (&QCUT_BUFFER2);
  QCUT_BUFFER3 = intern ("CUT_BUFFER3"); staticpro (&QCUT_BUFFER3);
  QCUT_BUFFER4 = intern ("CUT_BUFFER4"); staticpro (&QCUT_BUFFER4);
  QCUT_BUFFER5 = intern ("CUT_BUFFER5"); staticpro (&QCUT_BUFFER5);
  QCUT_BUFFER6 = intern ("CUT_BUFFER6"); staticpro (&QCUT_BUFFER6);
  QCUT_BUFFER7 = intern ("CUT_BUFFER7"); staticpro (&QCUT_BUFFER7);
#endif

}

void
Xatoms_of_xselect ()
{
#define ATOM(x) XInternAtom(x_current_display, (x), False)

  BLOCK_INPUT;
  /* Non-predefined atoms that we might end up using a lot */
  Xatom_CLIPBOARD =	ATOM("CLIPBOARD");
  Xatom_TIMESTAMP =	ATOM("TIMESTAMP");
  Xatom_TEXT =		ATOM("TEXT");
  Xatom_DELETE =	ATOM("DELETE");
  Xatom_MULTIPLE =	ATOM("MULTIPLE");
  Xatom_INCR =		ATOM("INCR");
  Xatom_EMACS_TMP =	ATOM("_EMACS_TMP_");
  Xatom_TARGETS =	ATOM("TARGETS");
  Xatom_NULL =		ATOM("NULL");
  Xatom_ATOM_PAIR =	ATOM("ATOM_PAIR");
  UNBLOCK_INPUT;
}
