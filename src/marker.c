/* Markers: examining, setting and killing.
   Copyright (C) 1985, 1992, 1993 Free Software Foundation, Inc.

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

#include <stdio.h>              /* for sprintf */

static Lisp_Object mark_marker (Lisp_Object, void (*) (Lisp_Object));
static void print_marker (Lisp_Object, Lisp_Object, int);
static int sizeof_marker (void *h) { return (sizeof (struct Lisp_Marker)); }
static int marker_equal (Lisp_Object, Lisp_Object, int);
DEFINE_LRECORD_IMPLEMENTATION (lrecord_marker,
                               mark_marker, print_marker, 
                               0, sizeof_marker, marker_equal);

static Lisp_Object
mark_marker (Lisp_Object obj, void (*markobj) (Lisp_Object))
{
  struct Lisp_Marker *marker = XMARKER (obj);
  Lisp_Object buf;
  /* DO NOT mark through the marker's chain.
     The buffer's markers chain does not preserve markers from gc;
     Instead, markers are removed from the chain when they are freed
     by gc.
   */
  if (!marker->buffer)
    return (Qnil);

  XSETR (buf, Lisp_Buffer, marker->buffer);
  return (buf);
}

static void
print_marker (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  if (print_readably)
    error (GETTEXT ("printing unreadable object #<marker>"));
      
  write_string_1 (GETTEXT ("#<marker "), -1, printcharfun);
  if (!(XMARKER (obj)->buffer))
    write_string_1 (GETTEXT ("in no buffer"), -1, printcharfun);
  else
    {
      char buf[20];
      sprintf (buf, "at %d", marker_position (obj));
      write_string_1 (buf, -1, printcharfun);
      write_string_1 (" in ", -1, printcharfun);
      print_internal (XMARKER (obj)->buffer->name, printcharfun, 0);
    }
  write_string_1 (">", -1, printcharfun);
}

static int
marker_equal (Lisp_Object o1, Lisp_Object o2, int depth)
{
  struct buffer *b1 = XMARKER (o1)->buffer;
  if (b1 != XMARKER (o2)->buffer)
    return (0);
  else if (!b1)
    /* All markers pointing nowhere are equal */
    return (1);
  else
    return ((XMARKER (o1)->bufpos == XMARKER (o2)->bufpos));
}


/* Operations on markers. */

DEFUN ("marker-buffer", Fmarker_buffer, Smarker_buffer, 1, 1, 0,
  "Return the buffer that MARKER points into, or nil if none.\n\
Returns nil if MARKER points into a dead buffer.")
  (marker)
     register Lisp_Object marker;
{
  Lisp_Object buf;
  CHECK_MARKER (marker, 0);
  if (XMARKER (marker)->buffer)
    {
      XSETR (buf, Lisp_Buffer, XMARKER (marker)->buffer);
      /* Return marker's buffer only if it is not dead.  */
      if (!NILP (XBUFFER (buf)->name))
	return buf;
    }
  return Qnil;
}

DEFUN ("marker-position", Fmarker_position, Smarker_position, 1, 1, 0,
  "Return the position MARKER points at, as a character number.\n\
Returns `nil' if marker doesn't point anywhere.")
  (marker)
     Lisp_Object marker;
{
  register int i;
  register struct buffer *buf;

  CHECK_MARKER (marker, 0);
  if (XMARKER (marker)->buffer)
    {
      buf = XMARKER (marker)->buffer;
      i = XMARKER (marker)->bufpos;

      if (i > BUF_GPT (buf) + BUF_GAP_SIZE (buf))
	i -= BUF_GAP_SIZE (buf);
      else if (i > BUF_GPT (buf))
	i = BUF_GPT (buf);

      if (i < BUF_BEG (buf) || i > BUF_Z (buf))
	abort ();

      return (make_number (i));
    }
  return Qnil;
}

#define marker_error(marker,message) \
   signal_error (Qerror, list2 (build_string ((message)), ((marker))))


static Lisp_Object
set_marker_internal (marker, pos, buffer, restricted_p)
     Lisp_Object marker, pos, buffer;
     int restricted_p;
{
  register int charno;
  register struct buffer *b;
  register struct Lisp_Marker *m;
  register int point_p;

  CHECK_MARKER (marker, 0);

  point_p = POINT_MARKER_P (marker);

  /* If position is nil or a marker that points nowhere,
     make this marker point nowhere.  */
  if (NILP (pos) ||
      (MARKERP (pos) && !XMARKER (pos)->buffer))
    {
      if (point_p)
	marker_error (marker,
		      GETTEXT ("can't make point-marker point nowhere"));
      if (XMARKER (marker)->buffer)
	unchain_marker (marker);
      return marker;
    }

  CHECK_FIXNUM_COERCE_MARKER (pos, 1);
  if (NILP (buffer))
    b = current_buffer;
  else
    {
      CHECK_BUFFER (buffer, 1);
      b = XBUFFER (buffer);
      /* If buffer is dead, set marker to point nowhere.  */
      if (EQ (b->name, Qnil))
	{
	  if (point_p)
	    marker_error (marker, GETTEXT
			  ("can't move point-marker in a killed buffer"));
	  if (XMARKER (marker)->buffer)
	    unchain_marker (marker);
	  return marker;
	}
    }

  charno = XINT (pos);
  m = XMARKER (marker);

  if (restricted_p)
    {
      if (charno < BUF_BEGV (b)) charno = BUF_BEGV (b);
      if (charno > BUF_ZV (b)) charno = BUF_ZV (b);
    }
  else
    {
      if (charno < BUF_BEG (b)) charno = BUF_BEG (b);
      if (charno > BUF_Z (b)) charno = BUF_Z (b);
    }

  if (point_p)
    {
#ifdef moving_point_by_moving_its_marker_is_a_feature
      if (XMARKER (marker)->buffer == current_buffer)
	SET_PT (charno);	/* this will move the marker */
      else
	{
	  int speccount = specpdl_depth ();
	  record_unwind_protect (Fset_buffer, Fcurrent_buffer ());
	  set_buffer_internal (b);
	  SET_PT (charno);	/* this will move the marker */
	  unbind_to (count, Qnil);
	}
#else  /* It's not a feature, so it must be a bug */
      marker_error (marker,
		    GETTEXT ("DEBUG: attempt to move point via point-marker"));
#endif
    }
  else
    {
      if (charno > BUF_GPT (b)) charno += BUF_GAP_SIZE (b);
      m->bufpos = charno;
    }

  if (m->buffer != b)
    {
      if (point_p)
	marker_error (marker, GETTEXT ("can't change buffer of point-marker"));
      if (m->buffer != 0)
	unchain_marker (marker);
      marker_next (m) = b->markers;
      b->markers = m;
      m->buffer = b;
    }
  
  return marker;
}


DEFUN ("set-marker", Fset_marker, Sset_marker, 2, 3, 0,
  "Position MARKER before character number NUMBER in BUFFER.\n\
BUFFER defaults to the current buffer.\n\
If NUMBER is nil, makes marker point nowhere.\n\
Then it no longer slows down editing in any buffer.\n\
If this marker was returned by (point-marker t), then changing its position\n\
moves point.  You cannot change its buffer or make it point nowhere.\n\
Returns MARKER.")
  (marker, pos, buffer)
     Lisp_Object marker, pos, buffer;
{
  return set_marker_internal (marker, pos, buffer, 0);
}


/* This version of Fset_marker won't let the position
   be outside the visible part.  */
Lisp_Object 
set_marker_restricted (marker, pos, buffer)
     Lisp_Object marker, pos, buffer;
{
  return set_marker_internal (marker, pos, buffer, 1);
}


/* This is called during garbage collection,
   so we must be careful to ignore and preserve mark bits,
   including those in chain fields of markers.  */

void
unchain_marker (Lisp_Object m)
{
  register struct Lisp_Marker *marker = XMARKER (m);
  register struct buffer *b = marker->buffer;
  register struct Lisp_Marker *chain, *prev, *next;

  if (b == 0)
    return;

  if (EQ (b->name, Qnil))       /* killed buffer */
    abort ();

  for (chain = b->markers, prev = 0; chain; chain = next)
    {
      next = marker_next (chain);

      if (marker == chain)
	{
	  if (!prev)
	    {
	      b->markers = next;
	      /* Deleting first marker from the buffer's chain.
		 Crash if new first marker in chain does not say
		 it belongs to this buffer.  */
	      if (next != 0 && b != next->buffer)
		abort ();
	    }
	  else
	    {
              marker_next (prev) = next;
	    }
	  break;
	}
      else
	prev = chain;
    }

  if (marker == XMARKER (b->point_marker))
    abort ();

  marker->buffer = 0;
}

int
marker_position (marker)
     Lisp_Object marker;
{
  register struct Lisp_Marker *m = XMARKER (marker);
  register struct buffer *buf = m->buffer;
  register int i = m->bufpos;

  if (!buf)
    error (GETTEXT ("Marker does not point anywhere"));

  if (i > BUF_GPT (buf) + BUF_GAP_SIZE (buf))
    i -= BUF_GAP_SIZE (buf);
  else if (i > BUF_GPT (buf))
    i = BUF_GPT (buf);

  if (i < BUF_BEG (buf) || i > BUF_Z (buf))
    abort ();

  return i;
}

DEFUN ("copy-marker", Fcopy_marker, Scopy_marker, 1, 1, 0,
  "Return a new marker pointing at the same place as MARKER.\n\
If argument is a number, makes a new marker pointing\n\
at that position in the current buffer.")
  (marker)
     register Lisp_Object marker;
{
  register Lisp_Object new;

  while (1)
    {
      if (FIXNUMP (marker)
	  || MARKERP (marker))
	{
 	  Lisp_Object buffer = (MARKERP (marker) ? Fmarker_buffer (marker)
				: Qnil);
	  new = Fmake_marker ();
	  Fset_marker (new, marker, buffer);
	  return new;
	}
      else
	marker = wrong_type_argument (Qinteger_or_marker_p, marker);
    }
}

void
syms_of_marker ()
{
  defsubr (&Smarker_position);
  defsubr (&Smarker_buffer);
  defsubr (&Sset_marker);
  defsubr (&Scopy_marker);
}
