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

/* Extent code needs to know about undo because the behavior of insert()
   with regard to extents varies depending on whether we are inside
   and undo or not. */
int inside_undo;

/* Last buffer for which undo information was recorded.  */
static Lisp_Object last_undo_buffer;

Lisp_Object Qinhibit_read_only;


/* Record that an unmodified buffer is about to be changed.
   Record the file modification date so that when undoing this entry
   we can tell whether it is obsolete because the file was saved again.  */

static void
record_first_change ()
{
  Lisp_Object high, low;
  high = make_number ((current_buffer->modtime >> 16) & 0xffff);
  low = make_number (current_buffer->modtime & 0xffff);
  current_buffer->undo_list = Fcons (Fcons (Qt, Fcons (high, low)),
                                     current_buffer->undo_list);
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
  int noundo = EQ (current_buffer->undo_list, Qt);

  if (current_buffer == XBUFFER (last_undo_buffer))
    {
      if (noundo) return;
    }
  else
    {
      last_undo_buffer = Fcurrent_buffer ();
      if (noundo) return;
      Fundo_boundary ();
    }

  if (MODIFF <= current_buffer->save_modified)
    record_first_change ();

  /* If this is following another insertion and consecutive with it
     in the buffer, combine the two.  */
  if (CONSP (current_buffer->undo_list))
    {
      Lisp_Object elt;
      elt = XCONS (current_buffer->undo_list)->car;
      if (CONSP (elt)
	  && FIXNUMP (XCONS (elt)->car)
	  && FIXNUMP (XCONS (elt)->cdr)
	  && XINT (XCONS (elt)->cdr) == beg)
	{
	  XCONS (elt)->cdr = make_number (beg + length);
	  return;
	}
    }

  current_buffer->undo_list = Fcons (Fcons (make_number (beg), 
                                            make_number (beg + length)),
                                     current_buffer->undo_list);
}

/* Record that a deletion is about to take place,
   for LENGTH characters at location BEG.  */

void
record_delete (int beg, int length)
{
  Lisp_Object sbeg;
  int noundo = EQ (current_buffer->undo_list, Qt);

  if (current_buffer == XBUFFER (last_undo_buffer))
    {
      if (noundo) return;
    }
  else
    {
      last_undo_buffer = Fcurrent_buffer ();
      if (noundo) return;
      Fundo_boundary ();
    }

  if (MODIFF <= current_buffer->save_modified)
    record_first_change ();

  if (PT == beg + length)
    sbeg = make_number (-beg);
  else
    sbeg = make_number (beg);

  {
/*
 * record_delete is called from many places other than undo so why this was
 * being marked as being inside_undo is beyond me.  It had the effect of
 * overriding the duplicable flag on extents making all extents appear
 * to be duplicable.
 */
/*
 *  int speccount = specpdl_depth ();
 *  record_unwind_protect (restore_inside_undo, make_number (inside_undo));
 *  inside_undo = 1;
 */
    /* If point isn't at start of deleted range, record where it is.  */
    if (PT != XFASTINT (sbeg))
      current_buffer->undo_list
        = Fcons (make_number (PT), current_buffer->undo_list);

    current_buffer->undo_list
      = Fcons (Fcons (make_string_from_buffer (current_buffer, beg, length),
		      sbeg),
               current_buffer->undo_list);

/*  unbind_to (speccount, Qnil); */
  }
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
  Lisp_Object token;

  if (XBUFFER (buffer) != current_buffer)
    {
      /* Temporarily switch buffers. */
      Lisp_Object current = Fcurrent_buffer ();
      Fset_buffer (buffer);
      record_extent (extent, attached);
      Fset_buffer (current);
      return;
    }

  if (current_buffer != XBUFFER (last_undo_buffer))
    Fundo_boundary ();
  XSETR (last_undo_buffer, Lisp_Buffer, current_buffer);

  if (EQ (current_buffer->undo_list, Qt))
    return;

#if 0 /* ?? */
  if (MODIFF <= current_buffer->save_modified)
    record_first_change ();
#endif

  if (attached)
    token = extent;
  else
    {
      Lisp_Object xbeg, xend;
      xbeg = Fextent_start_position (extent);
      xend = Fextent_end_position (extent);
      XSETEXTENT (token,
		  make_extent_replica (extent, XINT (xbeg), XINT (xend)));
    }

  current_buffer->undo_list = Fcons (token, current_buffer->undo_list);
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

#if 0 /* RMSmacs */
/* Record a change in property PROP (whose old value was VAL)
   for LENGTH characters starting at position BEG in BUFFER.  */

record_property_change (beg, length, prop, value, buffer)
     int beg, length;
     Lisp_Object prop, value, buffer;
{
  Lisp_Object lbeg, lend, entry;
  struct buffer *obuf = current_buffer;
  int boundary = 0;

  if (EQ (current_buffer->undo_list, Qt))
    return;

  if (!EQ (buffer, last_undo_buffer))
    boundary = 1;
  last_undo_buffer = buffer;

  /* Switch temporarily to the buffer that was changed.  */
  current_buffer = XBUFFER (buffer);

  if (boundary)
    Fundo_boundary ();

  if (MODIFF <= current_buffer->save_modified)
    record_first_change ();

  XSET (lbeg, Lisp_Int, beg);
  XSET (lend, Lisp_Int, beg + length);
  entry = Fcons (Qnil, Fcons (prop, Fcons (value, Fcons (lbeg, lend))));
  current_buffer->undo_list = Fcons (entry, current_buffer->undo_list);

  current_buffer = obuf;
}
#endif /* RMSmacs */


DEFUN ("undo-boundary", Fundo_boundary, Sundo_boundary, 0, 0, 0,
  "Mark a boundary between units of undo.\n\
An undo command will stop at this point,\n\
but another undo command will undo to the previous boundary.")
  ()
{
  Lisp_Object tem;
  if (EQ (current_buffer->undo_list, Qt))
    return Qnil;
  tem = Fcar (current_buffer->undo_list);
  if (!NILP (tem))
    current_buffer->undo_list = Fcons (Qnil, current_buffer->undo_list);
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
  defsubr (&Sprimitive_undo);
  defsubr (&Sundo_boundary);
}
