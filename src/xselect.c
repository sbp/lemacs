/* X Selection processing for emacs
   Copyright (C) 1990, 1991, 1992, 1993, 1994 Free Software Foundation.

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

/* Rewritten by jwz */

#include "config.h"
#include "lisp.h"
#include "xterm.h"	/* for all of the X includes */
#include "dispextern.h"	/* screen.h seems to want this */
#include "screen.h"	/* Need this to get the X window of selected_screen */
#include "xobjs.h"

#ifdef LWLIB_USES_MOTIF
# define MOTIF_CLIPBOARDS
#endif

#ifdef MOTIF_CLIPBOARDS
# include <Xm/CutPaste.h>
static void hack_motif_clipboard_selection ();
#endif

#define CUT_BUFFER_SUPPORT

static Atom Xatom_CLIPBOARD, Xatom_TIMESTAMP, Xatom_TEXT, Xatom_DELETE,
  Xatom_MULTIPLE, Xatom_INCR, Xatom_EMACS_TMP, Xatom_TARGETS, Xatom_NULL,
  Xatom_ATOM_PAIR;

Lisp_Object QPRIMARY, QSECONDARY, QSTRING, QINTEGER, QCLIPBOARD, QTIMESTAMP,
  QTEXT, QDELETE, QMULTIPLE, QINCR, QEMACS_TMP, QTARGETS, QATOM, QNULL,
  QATOM_PAIR;

#ifdef EPOCH
Lisp_Object QARC, QBITMAP, QCARDINAL, QCURSOR, QDRAWABLE, QFONT, QINTEGER,
  QPIXMAP, QPOINT, QRECTANGLE, QWINDOW, QWM_HINTS, QWM_SIZE_HINTS;
#endif /* EPOCH */

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

/* If the selection owner takes too long to reply to a selection request,
   we give up on it.  This is in seconds (0 = no timeout.)
 */
int x_selection_timeout;


/* Utility functions */

static void lisp_data_to_selection_data (Display *,
					 Lisp_Object obj,
					 unsigned char **data_ret,
					 Atom *type_ret,
					 unsigned int *size_ret,
					 int *format_ret);
static Lisp_Object selection_data_to_lisp_data (Display *,
						unsigned char *data,
						int size,
						Atom type,
						int format);
static Lisp_Object x_get_window_property_as_lisp_data (Display *,
						       Window,
						       Atom property,
						       Lisp_Object target_type,
						       Atom selection_atom);

static int expect_property_change (Display *, Window, Atom prop, int state);
static void wait_for_property_change (int);
static void unexpect_property_change (int);
static int waiting_for_other_props_on_window (Display *, Window);

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
#ifdef EPOCH
  if (EQ (sym, QARC))       return XA_ARC;
  if (EQ (sym, QBITMAP))    return XA_BITMAP;
  if (EQ (sym, QCARDINAL))  return XA_CARDINAL;
  if (EQ (sym, QCURSOR))    return XA_CURSOR;
  if (EQ (sym, QDRAWABLE))  return XA_DRAWABLE;
  if (EQ (sym, QFONT))      return XA_FONT;
  if (EQ (sym, QINTEGER))   return XA_INTEGER;
  if (EQ (sym, QPIXMAP))    return XA_PIXMAP;
  if (EQ (sym, QPOINT))     return XA_POINT;
  if (EQ (sym, QRECTANGLE)) return XA_RECTANGLE;
  if (EQ (sym, QWINDOW))    return XA_WINDOW;
  if (EQ (sym, QWM_HINTS))  return XA_WM_HINTS;
  if (EQ (sym, QWM_SIZE_HINTS)) return XA_WM_SIZE_HINTS;
#endif /* EPOCH */

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
  if (!SYMBOLP (sym)) abort ();

  BLOCK_INPUT;
  val = XInternAtom (display, (char *) XSYMBOL (sym)->name->data, False);
  UNBLOCK_INPUT;
  return val;
}


/* This converts a server Atom to a Lisp symbol, avoiding server roundtrips
   and calls to intern whenever possible.
 */
Lisp_Object
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
#ifdef EPOCH
  if (atom == XA_ARC)          return QARC;
  if (atom == XA_BITMAP)       return QBITMAP;
  if (atom == XA_CARDINAL)     return QCARDINAL;
  if (atom == XA_CURSOR)       return QCURSOR;
  if (atom == XA_DRAWABLE)     return QDRAWABLE;
  if (atom == XA_FONT)         return QFONT;
  if (atom == XA_INTEGER)      return QINTEGER;
  if (atom == XA_PIXMAP)       return QPIXMAP;
  if (atom == XA_POINT)        return QPOINT;
  if (atom == XA_RECTANGLE)    return QRECTANGLE;
  if (atom == XA_WINDOW)       return QWINDOW;
  if (atom == XA_WM_HINTS)     return QWM_HINTS;
  if (atom == XA_WM_SIZE_HINTS) return QWM_SIZE_HINTS;
#endif /* EPOCH */
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
  if (! str) return Qnil;
  val = intern (str);
  BLOCK_INPUT;
  XFree (str);
  UNBLOCK_INPUT;
  return val;
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
#ifdef MOTIF_CLIPBOARDS
    hack_motif_clipboard_selection (selection_atom, selection_value,
				    time, display, selecting_window,
				    !NILP (prev_value));
#endif
  }
}


#ifdef MOTIF_CLIPBOARDS /* Bend over baby.  Take it and like it. */

# ifdef MOTIF_INCREMENTAL_CLIPBOARDS_WORK
static void motif_clipboard_cb ();
# endif

static void
hack_motif_clipboard_selection (Atom selection_atom,
				Lisp_Object selection_value,
				Time time,
				Display *display,
				Window selecting_window,
				Bool owned_p)
{
  /* Those Motif wankers can't be bothered to follow the ICCCM, and do
     their own non-Xlib non-Xt clipboard processing.  So we have to do
     this so that linked-in Motif widgets don't get themselves wedged.
   */
  if (selection_atom == Xatom_CLIPBOARD
      && STRINGP (selection_value)

      /* If we already own the clipboard, don't own it again in the Motif
	 way.  This might lose in some subtle way, since the timestamp won't
	 be current, but owning the selection on the Motif way does a
	 SHITLOAD of X protocol, and it makes killing text be incredibly
	 slow when using an X terminal.  ARRRRGGGHHH!!!!
       */
      /* No, this is no good, because then Motif text fields don't bother
	 to look up the new value, and you can't Copy from a buffer, Paste
	 into a text field, then Copy something else from the buffer and
	 paste it intot he text field -- it pastes the first thing again. */
/*      && !owned_p */
      )
    {
#ifdef MOTIF_INCREMENTAL_CLIPBOARDS_WORK
      Widget widget = selected_screen->display.x->edit_widget;
#endif
      long itemid;
#if XmVersion >= 1002
      long dataid;
#else
      int dataid;	/* 1.2 wants long, but 1.1.5 wants int... */
#endif
      XmString fmh;
      BLOCK_INPUT;
      fmh = XmStringCreateLtoR ("Clipboard", XmSTRING_DEFAULT_CHARSET);
      while (ClipboardSuccess !=
	     XmClipboardStartCopy (display, selecting_window, fmh, time,
#ifdef MOTIF_INCREMENTAL_CLIPBOARDS_WORK
				   widget, motif_clipboard_cb,
#else
				   0, NULL,
#endif
				   &itemid))
	;
      XmStringFree (fmh);
      while (ClipboardSuccess !=
	     XmClipboardCopy (display, selecting_window, itemid, "STRING",
#ifdef MOTIF_INCREMENTAL_CLIPBOARDS_WORK
			      /* O'Reilly examples say size can be 0, 
				 but this clearly is not the case. */
			      0, XSTRING (selection_value)->size + 1,
			      (int) selecting_window, /* private id */
#else /* !MOTIF_INCREMENTAL_CLIPBOARDS_WORK */
			      (char *) XSTRING (selection_value)->data,
			      XSTRING (selection_value)->size + 1,
			      0,
#endif /* !MOTIF_INCREMENTAL_CLIPBOARDS_WORK */
			      &dataid))
	;
      while (ClipboardSuccess !=
	     XmClipboardEndCopy (display, selecting_window, itemid))
	;
      UNBLOCK_INPUT;
    }
}

# ifdef MOTIF_INCREMENTAL_CLIPBOARDS_WORK
/* I tried to treat the clipboard like a real selection, and not send
   the data until it was requested, but it looks like that just doesn't
   work at all unless the selection owner and requestor are in different
   processes.  From reading the Motif source, it looks like they never
   even considered having two widgets in the same application transfer
   data between each other using "by-name" clipboard values.  What a
   bunch of fuckups.
 */
static void
motif_clipboard_cb (Widget widget, int *data_id, int *private_id, int *reason)
{
  switch (*reason)
    {
    case XmCR_CLIPBOARD_DATA_REQUEST:
      {
	Display *dpy = XtDisplay (widget);
	Window window = (Window) *private_id;
	Lisp_Object selection = assq_no_quit (QCLIPBOARD, Vselection_alist);
	if (NILP (selection)) abort ();
	selection = XCONS (selection)->cdr;
	if (!STRINGP (selection)) abort ();
	XmClipboardCopyByName (dpy, window, *data_id,
			       (char *) XSTRING (selection)->data,
			       XSTRING (selection)->size + 1,
			       0);
      }
      break;
    case XmCR_CLIPBOARD_DATA_DELETE:
    default:
      /* don't need to free anything */
      break;
    }
}
# endif /* MOTIF_INCREMENTAL_CLIPBOARDS_WORK */
#endif /* MOTIF_CLIPBOARDS */


/* Given a selection-name and desired type, this looks up our local copy of
   the selection value and converts it to the type.  It returns nil or a
   string.  This calls random elisp code, and may signal or gc.
 */
static Lisp_Object
x_get_local_selection (selection_symbol, target_type)
     Lisp_Object selection_symbol, target_type;
{
  Lisp_Object local_value = assq_no_quit (selection_symbol, Vselection_alist);
  Lisp_Object handler_fn, value, check;

  if (NILP (local_value)) return Qnil;

  /* TIMESTAMP and MULTIPLE are special cases 'cause that's easiest. */
  if (EQ (target_type, QTIMESTAMP))
    {
      handler_fn = Qnil;
      value = XCONS (XCONS (XCONS (local_value)->cdr)->cdr)->car;
    }

#if 0 /* #### MULTIPLE doesn't work yet */
  else if (CONSP (target_type) &&
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

  /* This lets the selection function to return (TYPE . VALUE).  For example,
     when the selected type is LINE_NUMBER, the returned type is SPAN, not
     INTEGER.
   */
  check = value;
  if (CONSP (value) && SYMBOLP (XCONS (value)->car))
    check = XCONS (value)->cdr;
  
  /* Strings, vectors, and symbols are converted to selection data format in
     the obvious way.  Integers are converted to 16 bit quantities if they're
     small enough, otherwise 32 bits are used.
   */
  if (STRINGP (check) ||
      VECTORP (check) ||
      SYMBOLP (check) ||
      FIXNUMP (check) ||
      NILP (value))
    return value;

  /* (N . M) or (N M) get turned into a 32 bit quantity.  So if you want to
     always return a small quantity as 32 bits, your converter routine needs
     to return a cons.
   */
  else if (CONSP (check) &&
	   FIXNUMP (XCONS (check)->car) &&
	   (FIXNUMP (XCONS (check)->cdr) ||
	    (CONSP (XCONS (check)->cdr) &&
	     FIXNUMP (XCONS (XCONS (check)->cdr)->car) &&
	     NILP (XCONS (XCONS (check)->cdr)->cdr))))
    return value;
  /* Otherwise the lisp converter function returned something unrecognised. 
   */
  else
    signal_error (Qerror,
                  list3 (build_string
			 ("unrecognised selection-conversion type"),
                         handler_fn,
                         value));
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
  XFlush (reply.display);
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
  /* #### Sleazy!  Assumes the X event pointer fits in a Lisp_Int. */
  XSelectionRequestEvent *event = (XSelectionRequestEvent *) XPNTR (closure);
  
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
      XFlush (display);
    }
  else
    {
      /* Send an INCR selection. */
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
      XFlush (display);

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
  Lisp_Object tmp;
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
  if (!CONSP (local_selection_data)) abort ();
  if (!CONSP (CDR (local_selection_data))) abort ();
  if (!CONSP (CDR (CDR (local_selection_data)))) abort ();
  if (!NILP (CDR (CDR (CDR (local_selection_data))))) abort ();
  if (!CONSP (CAR (CDR (CDR (local_selection_data))))) abort ();
  if (!FIXNUMP (CAR (CAR (CDR (CDR (local_selection_data)))))) abort ();
  if (!FIXNUMP (CDR (CAR (CDR (CDR (local_selection_data)))))) abort ();
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

  count = specpdl_depth ();
  /* #### Sleazy!  Assumes the X event pointer fits in a Lisp_Int. */
  XSET (tmp, Lisp_Int, (LISP_WORD_TYPE) event);
  record_unwind_protect (x_selection_request_lisp_error, tmp);
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
      xfree (data);
    }
  unbind_to (count, Qnil);

 DONE:

  UNGCPRO;

  /* Let random lisp code notice that the selection has been asked for.
   */
  {
    Lisp_Object rest;
    Lisp_Object val = Vx_sent_selection_hooks;
    if (!EQ (val, Qunbound) && !NILP (val))
      {
	if (CONSP (val) && !EQ (XCONS (val)->car, Qlambda))
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
	if (CONSP (val) && !EQ (XCONS (val)->car, Qlambda))
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
property_deleted_p (void *tick)
{
  struct prop_location *rest = for_whom_the_bell_tolls;
  while (rest)
    if (rest->tick == (int) tick)
      return 0;
    else
      rest = rest->next;
  return 1;
}

static int
waiting_for_other_props_on_window (Display *display, Window window)
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
expect_property_change (Display *display, Window window,
			Atom property, int state)
{
  struct prop_location *pl = (struct prop_location *)
    xmalloc (sizeof (struct prop_location));
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
unexpect_property_change (int tick)
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
	  xfree (rest);
	  return;
	}
      prev = rest;
      rest = rest->next;
    }
}

static void
wait_for_property_change (int tick)
{
  wait_delaying_user_input (property_deleted_p, (void *) tick);
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
	  xfree (rest);
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
  if (CONSP (obj))
    return Fcons (XCONS (obj)->car, copy_multiple_data (XCONS (obj)->cdr));
    
  CHECK_VECTOR (obj, 0);
  size = XVECTOR (obj)->size;
  vec = make_vector (size, Qnil);
  for (i = 0; i < size; i++)
    {
      Lisp_Object vec2 = XVECTOR (obj)->contents [i];
      CHECK_VECTOR (vec2, 0);
      if (XVECTOR (vec2)->size != 2)
	signal_error (Qerror, list2 (build_string
				     ("vectors must be of length 2"),
                                     vec2));
      XVECTOR (vec)->contents [i] = make_vector (2, Qnil);
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
static int selection_reply_timed_out;

static int
selection_reply_done (ignore)
     void *ignore;
{
  return !reading_selection_reply;
}

static Lisp_Object Qx_selection_reply_timeout_internal;

DEFUN ("x-selection-reply-timeout-internal",
       Fx_selection_reply_timeout_internal,
       Sx_selection_reply_timeout_internal, 1, 1, 0, "")
     (arg)
     Lisp_Object arg;
{
  selection_reply_timed_out = 1;
  reading_selection_reply = 0;
  return Qnil;
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
  int speccount;

  if (CONSP (target_type))
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
  selection_reply_timed_out = 0;

  speccount = specpdl_depth ();

  /* add a timeout handler */
  if (x_selection_timeout > 0)
    {
      Lisp_Object id = Fadd_timeout (make_number (x_selection_timeout),
				     Qx_selection_reply_timeout_internal,
				     Qnil, Qnil);
      record_unwind_protect (Fdisable_timeout, id);
    }

  /* This is ^Gable */
  wait_delaying_user_input (selection_reply_done, 0);

  if (selection_reply_timed_out)
    error ("timed out waiting for reply from selection owner");

  unbind_to (speccount, Qnil);

  /* otherwise, the selection is waiting for us on the requested property. */
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
  BLOCK_INPUT;
  XFree ((char *) tmp_data);
  UNBLOCK_INPUT;
  
  if (*actual_type_ret == None || *actual_format_ret == 0)
    {
      if (delete_p) XDeleteProperty (display, window, property);
      *data_ret = 0;
      *bytes_ret = 0;
      return;
    }

  total_size = bytes_remaining + 1;
  *data_ret = (unsigned char *) xmalloc (total_size);
  
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
      memcpy ((*data_ret) + offset, tmp_data, *actual_size_ret);
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
  *data_ret = (unsigned char *) xmalloc (*size_bytes_ret);
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
	  if (tmp_data) xfree (tmp_data);
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
	  *data_ret = (unsigned char *) xrealloc (*data_ret, *size_bytes_ret);
	}
      memcpy ((*data_ret) + offset, tmp_data, tmp_size_bytes);
      offset += tmp_size_bytes;
      xfree (tmp_data);
    }
}


static Lisp_Object
x_get_window_property_as_lisp_data (Display *display,
				    Window window,
				    Atom property,
				    /* next two for error messages only */
				    Lisp_Object target_type,
				    Atom selection_atom)
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
      signal_error (Qerror,
        (there_is_a_selection_owner
	 ? Fcons (build_string ("selection owner couldn't convert"),
		  (actual_type
		   ? list2 (target_type,
			    x_atom_to_symbol (display, actual_type))
		   : list1 (target_type)))
	 : list2 (build_string ("no selection"),
		  x_atom_to_symbol (display, selection_atom))));
    }
  
  if (actual_type == Xatom_INCR)
    {
      /* Ok, that data wasnt *the* data, it was just the beginning. */

      unsigned int min_size_bytes = * ((unsigned int *) data);
      BLOCK_INPUT;
      XFree ((char *) data);
      UNBLOCK_INPUT;
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
  
  xfree (data);
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
selection_data_to_lisp_data (Display *display,
			     unsigned char *data,
			     int size,
			     Atom type,
			     int format)
{

  if (type == Xatom_NULL)
    return QNULL;

  /* Convert any 8-bit data to a string, for compactness. */
  else if (format == 8)
    return make_string ((char *) data, size);

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
	  Lisp_Object v = Fmake_vector (make_number (size / sizeof (Atom)),
					Qzero);
	  for (i = 0; i < size / sizeof (Atom); i++)
	    Faset (v, make_number (i),
		   x_atom_to_symbol (display, ((Atom *) data) [i]));
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

     #### Perhaps we should return the actual type to lisp as well.

	(x-get-selection-internal 'PRIMARY 'LINE_NUMBER)
	==> [4 4]

     and perhaps it should be

	(x-get-selection-internal 'PRIMARY 'LINE_NUMBER)
	==> (SPAN . [4 4])

     Right now the fact that the return type was SPAN is discarded before
     lisp code gets to see it.
   */
  else if (format == 16)
    {
      int i;
      Lisp_Object v = make_vector (size / 4, Qzero);
      for (i = 0; i < size / 4; i++)
	{
	  int j = (int) ((unsigned short *) data) [i];
	  Faset (v, make_number (i), make_number (j));
	}
      return v;
    }
  else
    {
      int i;
      Lisp_Object v = make_vector (size / 4, Qzero);
      for (i = 0; i < size / 4; i++)
	{
	  unsigned long j = ((unsigned long *) data) [i];
	  Faset (v, make_number (i), long_to_cons (j));
	}
      return v;
    }
}


static void
lisp_data_to_selection_data (Display *display,
			     Lisp_Object obj,
			     unsigned char **data_ret,
			     Atom *type_ret,
			     unsigned int *size_ret,
			     int *format_ret)
{
  Lisp_Object type = Qnil;
  if (CONSP (obj) && SYMBOLP (XCONS (obj)->car))
    {
      type = XCONS (obj)->car;
      obj = XCONS (obj)->cdr;
      if (CONSP (obj) && NILP (XCONS (obj)->cdr))
	obj = XCONS (obj)->car;
    }

  if (EQ (obj, QNULL) || (EQ (type, QNULL)))
    {				/* This is not the same as declining */
      *format_ret = 32;
      *size_ret = 0;
      *data_ret = 0;
      type = QNULL;
    }
  else if (STRINGP (obj))
    {
      *format_ret = 8;
      *size_ret = XSTRING (obj)->size;
      *data_ret = (unsigned char *) xmalloc (*size_ret);
      memcpy (*data_ret, (char *) XSTRING (obj)->data, *size_ret);
      if (NILP (type)) type = QSTRING;
    }
  else if (SYMBOLP (obj))
    {
      *format_ret = 32;
      *size_ret = 1;
      *data_ret = (unsigned char *) xmalloc (sizeof (Atom) + 1);
      (*data_ret) [sizeof (Atom)] = 0;
      (*(Atom **) data_ret) [0] = symbol_to_x_atom (display, obj);
      if (NILP (type)) type = QATOM;
    }
  else if (FIXNUMP (obj) &&
	   XINT (obj) < 0xFFFF &&
	   XINT (obj) > -0xFFFF)
    {
      *format_ret = 16;
      *size_ret = 1;
      *data_ret = (unsigned char *) xmalloc (sizeof (short) + 1);
      (*data_ret) [sizeof (short)] = 0;
      (*(short **) data_ret) [0] = (short) XINT (obj);
      if (NILP (type)) type = QINTEGER;
    }
  else if (FIXNUMP (obj) || CONSP (obj))
    {
      *format_ret = 32;
      *size_ret = 1;
      *data_ret = (unsigned char *) xmalloc (sizeof (long) + 1);
      (*data_ret) [sizeof (long)] = 0;
      (*(unsigned long **) data_ret) [0] = cons_to_long (obj);
      if (NILP (type)) type = QINTEGER;
    }
  else if (VECTORP (obj))
    {
      /* Lisp_Vectors may represent a set of ATOMs;
	 a set of 16 or 32 bit INTEGERs;
	 or a set of ATOM_PAIRs (represented as [[A1 A2] [A3 A4] ...]
       */
      int i;

      if (SYMBOLP (XVECTOR (obj)->contents [0]))
	/* This vector is an ATOM set */
	{
	  if (NILP (type)) type = QATOM;
	  *size_ret = XVECTOR (obj)->size;
	  *format_ret = 32;
	  *data_ret = (unsigned char *) xmalloc ((*size_ret) * sizeof (Atom));
	  for (i = 0; i < *size_ret; i++)
	    if (SYMBOLP (XVECTOR (obj)->contents [i]))
	      (*(Atom **) data_ret) [i] =
		symbol_to_x_atom (display, XVECTOR (obj)->contents [i]);
	    else
              signal_error (Qerror, /* Qselection_error */
                            list2 (build_string
		   ("all elements of the vector must be of the same type"),
                                   obj));
	}
#if 0 /* #### MULTIPLE doesn't work yet */
      else if (VECTORP (XVECTOR (obj)->contents [0]))
	/* This vector is an ATOM_PAIR set */
	{
	  if (NILP (type)) type = QATOM_PAIR;
	  *size_ret = XVECTOR (obj)->size;
	  *format_ret = 32;
	  *data_ret = (unsigned char *)
	    xmalloc ((*size_ret) * sizeof (Atom) * 2);
	  for (i = 0; i < *size_ret; i++)
	    if (VECTORP (XVECTOR (obj)->contents [i]))
	      {
		Lisp_Object pair = XVECTOR (obj)->contents [i];
		if (XVECTOR (pair)->size != 2)
		  signal_error (Qerror,
                                list2 (build_string 
       ("elements of the vector must be vectors of exactly two elements"),
				  pair));
		
		(*(Atom **) data_ret) [i * 2] =
		  symbol_to_x_atom (display, XVECTOR (pair)->contents [0]);
		(*(Atom **) data_ret) [(i * 2) + 1] =
		  symbol_to_x_atom (display, XVECTOR (pair)->contents [1]);
	      }
	    else
	      signal_error (Qerror,
                            list2 (build_string
		   ("all elements of the vector must be of the same type"),
                                   obj));
	}
#endif
      else
	/* This vector is an INTEGER set, or something like it */
	{
	  *size_ret = XVECTOR (obj)->size;
	  if (NILP (type)) type = QINTEGER;
	  *format_ret = 16;
	  for (i = 0; i < *size_ret; i++)
	    if (CONSP (XVECTOR (obj)->contents [i]))
	      *format_ret = 32;
	    else if (!FIXNUMP (XVECTOR (obj)->contents [i]))
	      signal_error (Qerror, /* Qselection_error */
                            list2 (build_string
	("all elements of the vector must be integers or conses of integers"),
                                   obj));

	  *data_ret = (unsigned char *) xmalloc (*size_ret * (*format_ret/8));
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
    signal_error (Qerror, /* Qselection_error */
                  list2 (build_string ("unrecognised selection data"),
                         obj));

  *type_ret = symbol_to_x_atom (display, type);
}

static Lisp_Object
clean_local_selection_data (obj)
     Lisp_Object obj;
{
  if (CONSP (obj) &&
      FIXNUMP (XCONS (obj)->car) &&
      CONSP (XCONS (obj)->cdr) &&
      FIXNUMP (XCONS (XCONS (obj)->cdr)->car) &&
      NILP (XCONS (XCONS (obj)->cdr)->cdr))
    obj = Fcons (XCONS (obj)->car, XCONS (obj)->cdr);

  if (CONSP (obj) &&
      FIXNUMP (XCONS (obj)->car) &&
      FIXNUMP (XCONS (obj)->cdr))
    {
      if (XINT (XCONS (obj)->car) == 0)
	return XCONS (obj)->cdr;
      if (XINT (XCONS (obj)->car) == -1)
	return make_number (- XINT (XCONS (obj)->cdr));
    }
  if (VECTORP (obj))
    {
      int i;
      int size = XVECTOR (obj)->size;
      Lisp_Object copy;
      if (size == 1)
	return clean_local_selection_data (XVECTOR (obj)->contents [0]);
      copy = make_vector (size, Qnil);
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
  if (CONSP (target_type) &&
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

  if (CONSP (val) &&
      SYMBOLP (XCONS (val)->car))
    {
      val = XCONS (val)->cdr;
      if (CONSP (val) && NILP (XCONS (val)->cdr))
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
      signal_error (Qerror, list2 (build_string ("doesn't name a cutbuffer"), \
                                   (symbol))); \
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
    signal_simple_error_2 ("cut buffer doesn't contain 8-bit data",
			   x_atom_to_symbol (display, type),
			   make_number (format));

  ret = (bytes ? make_string ((char *) data, bytes) : Qnil);
  xfree (data);
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

  CHECK_FIXNUM (n, 0);
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
  selection_reply_timed_out = 0;
  for_whom_the_bell_tolls = 0;
  prop_location_tick = 0;

  Vselection_alist = Qnil;
  staticpro (&Vselection_alist);

  DEFVAR_LISP ("selection-converter-alist", &Vselection_converter_alist,
  "An alist associating selection-types (such as STRING and TIMESTAMP) with\n\
functions.  These functions will be called with three args: the name of the\n\
selection (typically PRIMARY, SECONDARY, or CLIPBOARD); a desired type to\n\
which the selection should be converted; and the local selection value\n(\
whatever had been passed to `x-own-selection').  These functions should\n\
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

  DEFVAR_INT ("x-selection-timeout", &x_selection_timeout,
   "If the selection owner doesn't reply in this many seconds, we give up.\n\
A value of 0 means wait as long as necessary.  This is initialized from the\n\
\"*selectionTimeout\" resource (which is expressed in milliseconds).");
  x_selection_timeout = 0;

  /* Unfortunately, timeout handlers must be lisp functions. */
  defsymbol (&Qx_selection_reply_timeout_internal,
             "x-selection-reply-timeout-internal");
  defsubr (&Sx_selection_reply_timeout_internal);

  defsymbol (&QPRIMARY, "PRIMARY");
  defsymbol (&QSECONDARY, "SECONDARY");
  defsymbol (&QSTRING, "STRING");
  defsymbol (&QINTEGER, "INTEGER");
  defsymbol (&QCLIPBOARD, "CLIPBOARD");
  defsymbol (&QTIMESTAMP, "TIMESTAMP");
  defsymbol (&QTEXT, "TEXT");
  defsymbol (&QTIMESTAMP, "TIMESTAMP");
  defsymbol (&QDELETE, "DELETE");
  defsymbol (&QMULTIPLE, "MULTIPLE");
  defsymbol (&QINCR, "INCR");
  defsymbol (&QEMACS_TMP, "_EMACS_TMP_");
  defsymbol (&QTARGETS, "TARGETS");
  defsymbol (&QATOM, "ATOM");
  defsymbol (&QATOM_PAIR, "ATOM_PAIR");
  defsymbol (&QNULL, "NULL");

#ifdef EPOCH
  defsymbol (&QARC, "ARC");
  defsymbol (&QBITMAP, "BITMAP");
  defsymbol (&QCARDINAL, "CARDINAL");
  defsymbol (&QCURSOR, "CURSOR");
  defsymbol (&QDRAWABLE, "DRAWABLE");
  defsymbol (&QFONT, "FONT");
  defsymbol (&QINTEGER, "INTEGER");
  defsymbol (&QPIXMAP, "PIXMAP");
  defsymbol (&QPOINT, "POINT");
  defsymbol (&QRECTANGLE, "RECTANGLE");
  defsymbol (&QWINDOW, "WINDOW");
  defsymbol (&QWM_HINTS, "WM_HINTS");
  defsymbol (&QWM_SIZE_HINTS, "WM_SIZE_HINTS");
#endif /* EPOCH */
  
#ifdef CUT_BUFFER_SUPPORT
  defsymbol (&QCUT_BUFFER0, "CUT_BUFFER0");
  defsymbol (&QCUT_BUFFER1, "CUT_BUFFER1");
  defsymbol (&QCUT_BUFFER2, "CUT_BUFFER2");
  defsymbol (&QCUT_BUFFER3, "CUT_BUFFER3");
  defsymbol (&QCUT_BUFFER4, "CUT_BUFFER4");
  defsymbol (&QCUT_BUFFER5, "CUT_BUFFER5");
  defsymbol (&QCUT_BUFFER6, "CUT_BUFFER6");
  defsymbol (&QCUT_BUFFER7, "CUT_BUFFER7");
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
