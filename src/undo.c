/* undo handling for GNU Emacs.
   Copyright (C) 1990, 1992, 1993 Free Software Foundation, Inc.

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
#include "extents.h"

#define last_point_position PT /* >>> RMSmacs NYI */
#define last_point_position_buffer (Fcurrent_buffer ()) /* >>> RMSmacs NYI */


/* Extent code needs to know about undo because the behavior of insert()
   with regard to extents varies depending on whether we are inside
   and undo or not. */
int inside_undo;

/* Last buffer for which undo information was recorded.  */
static Lisp_Object last_undo_buffer;

Lisp_Object Qinhibit_read_only;

/* The first time a command records something for undo.
   it also allocates the undo-boundary object
   which will be added to the list at the end of the command.
   This ensures we can't run out of space while trying to make
   an undo-boundary.  */
Lisp_Object pending_boundary;

static void
undo_boundary (struct buffer *b)
{
  Lisp_Object tem = Fcar (b->undo_list);
  if (!NILP (tem))
    {
      /* One way or another, cons nil onto the front of the undo list.  */
      if (CONSP (pending_boundary))
	{
	  /* If we have preallocated the cons cell to use here,
	     use that one.  */
	  XCONS (pending_boundary)->cdr = b->undo_list;
	  b->undo_list = pending_boundary;
	  pending_boundary = Qnil;
	}
      else
	b->undo_list = Fcons (Qnil, b->undo_list);
    }
}


static int
undo_prelude (struct buffer *b, int hack_pending_boundary)
{
  if (EQ (b->undo_list, Qt))
    return (0);

  if (b != XBUFFER (last_undo_buffer))
  {
    undo_boundary (b);
    XSETR (last_undo_buffer, Lisp_Buffer, b);
  }
  
  /* Allocate a cons cell to be the undo boundary after this command.  */
  if (hack_pending_boundary && NILP (pending_boundary))
    pending_boundary = Fcons (Qnil, Qnil);

  if (BUF_MODIFF (b) <= b->save_modified)
  {
    /* Record that an unmodified buffer is about to be changed.
       Record the file modification date so that when undoing this entry
       we can tell whether it is obsolete because the file was saved again.  */
    b->undo_list
      = Fcons (Fcons (Qt,
                      Fcons (make_number ((b->modtime >> 16) & 0xffff),
                             make_number (b->modtime & 0xffff))),
               b->undo_list);
  }
  return (1);
}



static Lisp_Object
restore_inside_undo (Lisp_Object val)
{
  inside_undo = XINT (val);
  return val;
}


/* Record an insertion that just happened or is about to happen,
   for LENGTH characters at position BEG.
   (It is possible to record an insertion before or after the fact
   because we don't need to record the contents.)  */

void
record_insert (beg, length)
     int beg, length;
{
  struct buffer *b = current_buffer;
  if (!undo_prelude (b, 1))
    return;

  /* If this is following another insertion and consecutive with it
     in the buffer, combine the two.  */
  if (CONSP (b->undo_list))
    {
      Lisp_Object elt;
      elt = XCONS (b->undo_list)->car;
      if (CONSP (elt)
	  && FIXNUMP (XCONS (elt)->car)
	  && FIXNUMP (XCONS (elt)->cdr)
	  && XINT (XCONS (elt)->cdr) == beg)
	{
	  XCONS (elt)->cdr = make_number (beg + length);
	  return;
	}
    }

  b->undo_list = Fcons (Fcons (make_number (beg), 
                               make_number (beg + length)),
                        b->undo_list);
}

/* Record that a deletion is about to take place,
   for LENGTH characters at location BEG.  */

void
record_delete (int beg, int length)
{
  Lisp_Object sbeg;
  struct buffer *b = current_buffer;
  int at_boundary;

  if (!undo_prelude (b, 1))
    return;

  at_boundary = (CONSP (current_buffer->undo_list)
		 && NILP (XCONS (current_buffer->undo_list)->car));

  if (BUF_PT (b) == beg + length)
    sbeg = make_number (-beg);
  else
    sbeg = make_number (beg);

  /* If we are just after an undo boundary, and 
     point wasn't at start of deleted range, record where it was.  */
  if (at_boundary
      && last_point_position != XFASTINT (sbeg)
      && current_buffer == XBUFFER (last_point_position_buffer))
    b->undo_list = Fcons (make_number (last_point_position), b->undo_list);

  b->undo_list = Fcons (Fcons (make_string_from_buffer (b, beg, length),
                               sbeg),
                        b->undo_list);
}

/* Record that a replacement is about to take place,
   for LENGTH characters at location BEG.
   The replacement does not change the number of characters.  */

void
record_change (int beg, int length)
{
  record_delete (beg, length);
  record_insert (beg, length);
}

/* Record that an EXTENT is about to be attached or detached in its buffer.
   This works much like a deletion or insertion, except that there's no string.
   The tricky part is that the buffer we operate on comes from EXTENT.
   Most extent changes happen as a side effect of string insertion and
   deletion; this call is solely for Fdetach_extent() and Finsert_extent().
   */
void
record_extent (Lisp_Object extent, int attached)
{
  Lisp_Object buffer = Fextent_buffer (extent);
  struct buffer *b = XBUFFER (buffer);
  Lisp_Object token;

  if (!undo_prelude (b, 1))
    return;

  if (attached)
    token = extent;
  else
    {
      Lisp_Object xbeg = Fextent_start_position (extent);
      Lisp_Object xend = Fextent_end_position (extent);
      XSETEXTENT (token,
                  make_extent_replica (extent, XINT (xbeg), XINT (xend)));
    }
  b->undo_list = Fcons (token, b->undo_list);
}

#if 0 /* RMSmacs */
/* Record a change in property PROP (whose old value was VAL)
   for LENGTH characters starting at position BEG in BUFFER.  */

record_property_change (int beg, int length,
                        Lisp_Object prop, Lisp_Object value,
                        Lisp_Object buffer)
{
  Lisp_Object lbeg, lend, entry;
  struct buffer *b = XBUFFER (buffer);

  if (!undo_prelude (b, 1))
    return;

  lbeg = make_number (beg);
  lend = make_number (beg + length);
  entry = Fcons (Qnil, Fcons (prop, Fcons (value, Fcons (lbeg, lend))));
  b->undo_list = Fcons (entry, b->undo_list);
}
#endif /* RMSmacs */


DEFUN ("undo-boundary", Fundo_boundary, Sundo_boundary, 0, 0, 0,
  "Mark a boundary between units of undo.\n\
An undo command will stop at this point,\n\
but another undo command will undo to the previous boundary.")
  ()
{
  if (EQ (current_buffer->undo_list, Qt))
    return Qnil;
  undo_boundary (current_buffer);
  return Qnil;
}

/* At garbage collection time, make an undo list shorter at the end,
   returning the truncated list.
   MINSIZE and MAXSIZE are the limits on size allowed, as described below.
   In practice, these are the values of undo-threshold and
   undo-high-threshold.  */

Lisp_Object
truncate_undo_list (list, minsize, maxsize)
     Lisp_Object list;
     int minsize, maxsize;
{
  Lisp_Object prev, next, last_boundary;
  int size_so_far = 0;

  if (!(minsize > 0 || maxsize > 0))
    return list;

  prev = Qnil;
  next = list;
  last_boundary = Qnil;

  if (!CONSP (list))
    return (list);

  /* Always preserve at least the most recent undo record.
     If the first element is an undo boundary, skip past it. */
  if (CONSP (next)
      && NILP (XCONS (next)->car))
    {
      /* Add in the space occupied by this element and its chain link.  */
      size_so_far += sizeof (struct Lisp_Cons);

      /* Advance to next element.  */
      prev = next;
      next = XCONS (next)->cdr;
    }
  while (CONSP (next)
	 && !NILP (XCONS (next)->car))
    {
      Lisp_Object elt;
      elt = XCONS (next)->car;

      /* Add in the space occupied by this element and its chain link.  */
      size_so_far += sizeof (struct Lisp_Cons);
      if (CONSP (elt))
	{
	  size_so_far += sizeof (struct Lisp_Cons);
	  if (STRINGP (XCONS (elt)->car))
	    size_so_far += (sizeof (struct Lisp_String) - 1
			    + XSTRING (XCONS (elt)->car)->size);
	}

      /* Advance to next element.  */
      prev = next;
      next = XCONS (next)->cdr;
    }
  if (CONSP (next))
    last_boundary = prev;

  while (CONSP (next))
    {
      Lisp_Object elt;
      elt = XCONS (next)->car;

      /* When we get to a boundary, decide whether to truncate
	 either before or after it.  The lower threshold, MINSIZE,
	 tells us to truncate after it.  If its size pushes past
	 the higher threshold MAXSIZE as well, we truncate before it.  */
      if (NILP (elt))
	{
	  if (size_so_far > maxsize && maxsize > 0)
	    break;
	  last_boundary = prev;
	  if (size_so_far > minsize && minsize > 0)
	    break;
	}

      /* Add in the space occupied by this element and its chain link.  */
      size_so_far += sizeof (struct Lisp_Cons);
      if (CONSP (elt))
	{
	  size_so_far += sizeof (struct Lisp_Cons);
	  if (STRINGP (XCONS (elt)->car))
	    size_so_far += (sizeof (struct Lisp_String) - 1
                            + XSTRING (XCONS (elt)->car)->size);
	}

      /* Advance to next element.  */
      prev = next;
      next = XCONS (next)->cdr;
    }

  /* If we scanned the whole list, it is short enough; don't change it.  */
  if (NILP (next))
    return list;

  /* Truncate at the boundary where we decided to truncate.  */
  if (!NILP (last_boundary))
    {
      XCONS (last_boundary)->cdr = Qnil;
      return list;
    }
  else
    return Qnil;
}

DEFUN ("primitive-undo", Fprimitive_undo, Sprimitive_undo, 2, 2, 0,
  "Undo COUNT records from the front of the list LIST.\n\
Return what remains of the list.")
  (count, list)
     Lisp_Object count, list;
{
  int arg = XINT (count);
  int speccount = specpdl_depth ();

  record_unwind_protect (restore_inside_undo, make_number (inside_undo));
  inside_undo = 1;

#if 0  /* This is a good feature, but would make undo-start
	  unable to do what is expected.  */
  Lisp_Object tem;

  /* If the head of the list is a boundary, it is the boundary
     preceding this command.  Get rid of it and don't count it.  */
  tem = Fcar (list);
  if (NILP (tem))
    list = Fcdr (list);
#endif

  /* Don't let read-only properties interfere with undo.  */
  if (NILP (current_buffer->read_only))
    specbind (Qinhibit_read_only, Qt);

  while (arg > 0)
    {
      while (1)
	{
	  Lisp_Object next = Qnil;
          if (NILP (list))
            break;
          else if (!CONSP (list))
            goto rotten;
	  next = XCONS (list)->car;
	  list = XCONS (list)->cdr;
	  /* Exit inner loop at undo boundary.  */
	  if (NILP (next))
	    break;
	  /* Handle an integer by setting point to that value.  */
	  else if (FIXNUMP (next))
	    SET_PT (clip_to_bounds (BEGV, XINT (next), ZV));
	  else if (CONSP (next))
	    {
	      Lisp_Object car = XCONS (next)->car;
              Lisp_Object cdr = XCONS (next)->cdr;

              if (EQ (car, Qt))
		{
		  /* Element (t high . low) records previous modtime.  */
		  Lisp_Object high, low;
		  int mod_time;
		  if (!CONSP (cdr)) goto rotten;
		  high = XCONS (cdr)->car;
		  low = XCONS (cdr)->cdr;
		  if (!FIXNUMP (high) || !FIXNUMP (low)) goto rotten;
		  mod_time = (XFASTINT (high) << 16) + XFASTINT (low);
		  /* If this records an obsolete save
		     (not matching the actual disk file)
		     then don't mark unmodified.  */
		  if (mod_time != current_buffer->modtime)
		    break;
#ifdef CLASH_DETECTION
		  Funlock_buffer ();
#endif /* CLASH_DETECTION */
		  Fset_buffer_modified_p (Qnil);
		}
#if 0 /* RMSmacs */
	      else if (EQ (car, Qnil))
		{
		  /* Element (nil prop val beg . end) is property change.  */
		  Lisp_Object beg, end, prop, val;

		  prop = Fcar (cdr);
		  cdr = Fcdr (cdr);
		  val = Fcar (cdr);
		  cdr = Fcdr (cdr);
		  beg = Fcar (cdr);
		  end = Fcdr (cdr);

		  Fput_text_property (beg, end, prop, val, Qnil);
		}
#endif /* RMSmacs */
	      else if (FIXNUMP (car) && FIXNUMP (cdr))
		{
		  /* Element (BEG . END) means range was inserted.  */

		  if (XINT (car) < BEGV
		      || XINT (cdr) > ZV)
		    error (GETTEXT ("Changes to be undone are outside visible portion of buffer"));
		  /* Set point first thing, so that undoing this undo
		     does not send point back to where it is now.  */
		  Fgoto_char (car);
		  Fdelete_region (car, cdr);
		}
	      else if (STRINGP (car) && FIXNUMP (cdr))
		{
		  /* Element (STRING . POS) means STRING was deleted.  */
		  Lisp_Object membuf = car;
		  int pos = XINT (cdr);

		  if (pos < 0)
		    {
		      if (-pos < BEGV || -pos > ZV)
			error (GETTEXT ("Changes to be undone are outside visible portion of buffer"));
		      SET_PT (-pos);
		      Finsert (1, &membuf);
		    }
		  else
		    {
		      if (pos < BEGV || pos > ZV)
			error (GETTEXT ("Changes to be undone are outside visible portion of buffer"));
		      SET_PT (pos);

		      /* Insert before markers so that if the mark is
			 currently on the boundary of this deletion, it
			 ends up on the other side of the now-undeleted
			 text from point.  Since undo doesn't even keep
			 track of the mark, this isn't really necessary,
			 but it may lead to better behavior in certain
			 situations.
			 
			 I'm doubtful that this is safe; you could mess
			 up the process-output mark in shell buffers, so
			 until I hear a compelling reason for this change,
			 I'm leaving it out. -jwz
			 */
		      /* Finsert_before_markers (1, &membuf); */
		      Finsert (1, &membuf);
		      SET_PT (pos);
		    }
		}
	      else
		{
		  goto rotten;
		}
	    }
	  else if (EXTENTP (next))
	    Fdetach_extent (next);
	  else if (EXTENT_REPLICA_P (next))
	    {
	       Lisp_Object extent_obj, start, end;

	       extent_obj = Fextent_replica_extent (next);
	       start = Fextent_replica_start (next);
	       end = Fextent_replica_end (next);

	       Fset_extent_endpoints (extent_obj, start, end);
	     }
          else
	    {
	    rotten:
	      Fsignal (Qerror,
		       list2 (build_string
			      (GETTEXT ("Something rotten in the state of undo:")),
			      next));
	    }
        }
      arg--;
    }
  
  return unbind_to (speccount, list);
}

void
syms_of_undo ()
{
  inside_undo = 0;
  defsymbol (&Qinhibit_read_only, "inhibit-read-only");
  pending_boundary = Qnil;
  staticpro (&pending_boundary);

  defsubr (&Sprimitive_undo);
  defsubr (&Sundo_boundary);
}
