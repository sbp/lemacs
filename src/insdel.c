/* Buffer insertion/deletion and gap motion for GNU Emacs.
   Copyright (C) 1985, 1986, 1991, 1992, 1993 Free Software Foundation, Inc.

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
#include "extents.h"
#include "window.h"
#include "insdel.h"


/* Routines for dealing with the gap. */

/* Add `amount' to the position of every marker in the current buffer
   whose current position is between `from' (exclusive) and `to' (inclusive).
   Also, any markers past the outside of that range, in the direction
   of adjustment, are first moved back to the near end of the range
   and then adjusted by `amount'.  */

static void
adjust_markers (from, to, amount)
     int from, to, amount;
{
  Lisp_Object marker;
  struct Lisp_Marker *m;
  int mpos;

  marker = current_buffer->markers;

  while (!NILP (marker))
    {
      m = XMARKER (marker);
      mpos = m->bufpos;
      if (amount > 0)
	{
	  if (mpos > to && mpos < to + amount)
	    mpos = to + amount;
	}
      else
	{
	  if (mpos > from + amount && mpos <= from)
	    mpos = from + amount;
	}
      if (mpos > from && mpos <= to)
	mpos += amount;
      m->bufpos = mpos;
      marker = m->chain;
    }
}

/* Move the gap to POS, which is less than the current GPT.   If NEWGAP
   is nonzero, then don't update beg_unchanged and end_unchanged.  */

static void
gap_left (pos, newgap)
     register int pos;
     int newgap;
{
  register unsigned char *to, *from;
  register int i;
  int new_s1;
  pos--;

  if (!newgap)
    {
      if (unchanged_modified == MODIFF)
	{
	  beg_unchanged = pos;
	  end_unchanged = Z - pos - 1;
	}
      else
	{
	  if (Z - GPT < end_unchanged)
	    end_unchanged = Z - GPT;
	  if (pos < beg_unchanged)
	    beg_unchanged = pos;
	}
    }

  i = GPT;
  to = GAP_END_ADDR;
  from = GPT_ADDR;
  new_s1 = GPT - BEG;

  /* Now copy the characters.  To move the gap down,
     copy characters up.  */

  while (1)
    {
      /* I gets number of characters left to copy.  */
      i = new_s1 - pos;
      if (i == 0)
	break;
      /* If a quit is requested, stop copying now.
	 Change POS to be where we have actually moved the gap to.  */
      if (QUITP)
	{
	  pos = new_s1;
	  break;
	}
      /* Move at most 32000 chars before checking again for a quit.  */
      if (i > 32000)
	i = 32000;
      new_s1 -= i;
      while (--i >= 0)
	*--to = *--from;
    }

  /* Adjust markers, and buffer data structure, to put the gap at POS.
     POS is where the loop above stopped, which may be what was specified
     or may be where a quit was detected.  */
  adjust_markers (pos + 1, GPT, GAP_SIZE);
  adjust_extents (pos + 1, GPT, GAP_SIZE, current_buffer);
  GPT = pos + 1;
  QUIT;
}

static void
gap_right (pos)
     register int pos;
{
  register unsigned char *to, *from;
  register int i;
  int new_s1;
  pos--;

  if (unchanged_modified == MODIFF)
    {
      beg_unchanged = pos;
      end_unchanged = Z - pos - 1;
    }
  else
    {
      if (Z - pos - 1 < end_unchanged)
	end_unchanged = Z - pos - 1;
      if (GPT - BEG < beg_unchanged)
	beg_unchanged = GPT - BEG;
    }

  i = GPT;
  from = GAP_END_ADDR;
  to = GPT_ADDR;
  new_s1 = GPT - 1;

  /* Now copy the characters.  To move the gap up,
     copy characters down.  */

  while (1)
    {
      /* I gets number of characters left to copy.  */
      i = pos - new_s1;
      if (i == 0)
	break;
      /* If a quit is requested, stop copying now.
	 Change POS to be where we have actually moved the gap to.  */
      if (QUITP)
	{
	  pos = new_s1;
	  break;
	}
      /* Move at most 32000 chars before checking again for a quit.  */
      if (i > 32000)
	i = 32000;
      new_s1 += i;
      while (--i >= 0)
	*to++ = *from++;
    }

  adjust_markers (GPT + GAP_SIZE, pos + 1 + GAP_SIZE, - GAP_SIZE);
  adjust_extents (GPT + GAP_SIZE, pos + 1 + GAP_SIZE, - GAP_SIZE,
		  current_buffer);
  GPT = pos + 1;
  QUIT;
}

/* Move gap to position `pos'.
   Note that this can quit!  */

void
move_gap (pos)
     int pos;
{
  if (pos < GPT)
    gap_left (pos, 0);
  else if (pos > GPT)
    gap_right (pos);
}

/* Make the gap INCREMENT characters longer.  */

void
make_gap (increment)
     int increment;
{
  unsigned char *result;
  Lisp_Object tem;
  int real_gap_loc;
  int old_gap_size;

  /* If we have to get more space, get enough to last a while.  We use
     a geometric progession that saves on realloc space. */
  increment += 2000 + (Z - BEG) * .1;

  result = BUFFER_REALLOC (BEG_ADDR, (Z - BEG + GAP_SIZE + increment));
  if (result == 0)
    memory_full ();
  BEG_ADDR = result;

  /* Prevent quitting in move_gap.  */
  tem = Vinhibit_quit;
  Vinhibit_quit = Qt;

  real_gap_loc = GPT;
  old_gap_size = GAP_SIZE;

  /* Call the newly allocated space a gap at the end of the whole space.  */
  GPT = Z + GAP_SIZE;
  GAP_SIZE = increment;

  /* Move the new gap down to be consecutive with the end of the old one.
     This adjusts the markers properly too.  */
  gap_left (real_gap_loc + old_gap_size, 1);

  /* Now combine the two into one large gap.  */
  GAP_SIZE += old_gap_size;
  GPT = real_gap_loc;

  Vinhibit_quit = tem;
}

void signal_before_change (int, int);

/* Check that it is okay to modify the buffer between START and END.
   Run the before-change-function, if any.  */

void
prepare_to_modify_buffer (start, end)
     int start, end;
{
  if (!NILP (current_buffer->read_only))
    Fbarf_if_buffer_read_only ();

  verify_extent_modification (current_buffer, start, end);

#ifdef ENERGIZE
  /* If buffer is unmodified, run a special hook for that case.  */
  if (current_buffer->save_modified >= MODIFF
      && !NILP (Vfirst_change_function))
    {
      safe_funcall_hook (Vfirst_change_function, 0, 0, 0, 0);
    }
#endif

#ifdef CLASH_DETECTION
  if (!NILP (current_buffer->filename)
      && current_buffer->save_modified >= MODIFF)
    lock_file (current_buffer->filename);
#else
  /* At least warn if this file has changed on disk since it was visited.  */
  if (!NILP (current_buffer->filename)
      && current_buffer->save_modified >= MODIFF
      && NILP (Fverify_visited_file_modtime (Fcurrent_buffer ()))
      && !NILP (Ffile_exists_p (current_buffer->filename)))
    call1 (intern ("ask-user-about-supersession-threat"),
	   current_buffer->filename);
#endif /* not CLASH_DETECTION */

  signal_before_change (start, end);
}

void
modify_region (start, end)
     int start, end;
{
  prepare_to_modify_buffer (start, end);

  if (start - 1 < beg_unchanged || unchanged_modified == MODIFF)
    beg_unchanged = start - 1;
  if (Z - end < end_unchanged
      || unchanged_modified == MODIFF)
    end_unchanged = Z - end;
  MODIFF++;
}

static Lisp_Object
before_change_function_restore (value)
     Lisp_Object value;
{
  Vbefore_change_function = value;
  return Qnil;
}

static Lisp_Object
after_change_function_restore (value)
     Lisp_Object value;
{
  Vafter_change_function = value;
  return Qnil;
}

/* Signal a change to the buffer immediatly before it happens.
   START and END are the bounds of the text to be changed,
   as Lisp objects.  */

void
signal_before_change (start, end)
     int start, end;
{
  /* Now in any case run the before-change-function if any.  */
  if (!NILP (Vbefore_change_function))
    {
      int count = specpdl_depth;
      Lisp_Object function, s, e;

      function = Vbefore_change_function;
      record_unwind_protect (after_change_function_restore,
			     Vafter_change_function);
      record_unwind_protect (before_change_function_restore,
			     Vbefore_change_function);
      Vafter_change_function = Qnil;
      Vbefore_change_function = Qnil;

      XSET (s, Lisp_Int, start);
      XSET (e, Lisp_Int, end);
      safe_funcall_hook (function, 2, s, e, 0);
      unbind_to (count, Qnil);
    }
}

/* Signal a change immediatly after it happens.
   POS is the address of the start of the changed text.
   LENDEL is the number of characters of the text before the change.
   (Not the whole buffer; just the part that was changed.)
   LENINS is the number of characters in the changed text.  */

void
signal_after_change (pos, lendel, lenins)
     int pos, lendel, lenins;
{
  if (!NILP (Vafter_change_function))
    {
      int count = specpdl_depth;
      Lisp_Object function;
      function = Vafter_change_function;

      record_unwind_protect (after_change_function_restore,
			     Vafter_change_function);
      record_unwind_protect (before_change_function_restore,
			     Vbefore_change_function);
      Vafter_change_function = Qnil;
      Vbefore_change_function = Qnil;

      safe_funcall_hook (function, 3, make_number (pos),
			 make_number (pos + lenins), make_number (lendel));
      unbind_to (count, Qnil);
    }
}

/* Insertion of strings. */

void
insert_relocatable_raw_string (string, length, obj)
     Lisp_Object obj;
     const char *string;
     int length;
{
  struct gcpro gcpro1;
  Lisp_Object dup_list = 
    (STRINGP (obj))?(XSTRING(obj)->dup_list):Qnil;
  Lisp_Object temp;
  int opoint = point;

  if (length < 1)
    return;

  GCPRO1 (obj);

  /* Make sure that point-max won't exceed the size of an emacs int. */
  XSET (temp, Lisp_Int, length + Z);
  if (length + Z != XINT (temp))
    error ("maximum buffer size exceeded");

  prepare_to_modify_buffer (opoint, opoint);

  if (opoint != GPT)
    move_gap (opoint);
  if (GAP_SIZE < length)
    make_gap (length - GAP_SIZE);

  record_insert (opoint, length);
  MODIFF++;

  /* string may have been relocated up to this point */
  if (STRINGP (obj))
    string = (char *) XSTRING (obj) -> data;

  memcpy (GPT_ADDR, string, length);

  process_extents_for_insertion (opoint, length, current_buffer);

  GAP_SIZE -= length;
  GPT += length;
  ZV += length;
  Z += length;
  SET_PT (opoint + length);

  splice_in_extent_replicas (opoint, length, dup_list, current_buffer);
  signal_after_change (opoint, 0, length);

  UNGCPRO;
}

void
insert_from_string (obj, pos, length)
     Lisp_Object obj;
     int pos, length;
{
  unsigned char *string = XSTRING (obj)->data;
  struct gcpro gcpro1;
  Lisp_Object dup_list = 
    (STRINGP (obj)) ? (XSTRING (obj)->dup_list) : Qnil;
  Lisp_Object temp;
  int opoint = point;

  (void)pos;			/* pos is always 0 */

  if (length < 1)
    return;

  GCPRO1 (obj);

  /* Make sure that point-max won't exceed the size of an emacs int. */
  XSET (temp, Lisp_Int, length + Z);
  if (length + Z != XINT (temp))
    error ("maximum buffer size exceeded");

  prepare_to_modify_buffer (opoint, opoint);

  if (opoint != GPT)
    move_gap (opoint);
  if (GAP_SIZE < length)
    make_gap (length - GAP_SIZE);

  record_insert (opoint, length);
  MODIFF++;

  /* string may have been relocated up to this point */
  if (STRINGP (obj))
    string = XSTRING (obj) -> data;

  memcpy (GPT_ADDR, string, length);

  process_extents_for_insertion (opoint, length, current_buffer);

  GAP_SIZE -= length;
  GPT += length;
  ZV += length;
  Z += length;
  SET_PT (opoint + length);

  splice_in_extent_replicas (opoint, length, dup_list, current_buffer);
  signal_after_change (opoint, 0, length);

  UNGCPRO;
}

/* Insert a raw string of specified length before point */

void
insert_raw_string (string, length)
     const char *string;
     int length;
{
  insert_relocatable_raw_string (string, length, 0);
  return;
}

void
insert (string, length)
     const char *string;
     int length;
{
  insert_relocatable_raw_string (string, length, 0);
  return;
}

/* Insert the null-terminated string S before point */

void
insert_string (s)
     const char *s;
{
  insert_raw_string (s, strlen (s));
}

/* Insert the character C before point */

void
insert_char (char c)
{
  insert_raw_string (&c, 1);
}

/* Like `insert_raw_string' except that all markers pointing at the place where
   the insertion happens are adjusted to point after it.  */

void
insert_before_markers (string, length)
     const char *string;
     register int length;
{
  register int opoint = point;
  insert_raw_string (string, length);
  adjust_markers (opoint - 1, opoint, length);
}

void
insert_from_string_before_markers (string, pos, length)
     Lisp_Object string;
     int pos, length;
{
  register int opoint = point;
  insert_from_string (string, pos, length);
  adjust_markers (opoint - 1, opoint, length);
}


/* This section deals with fat strings, i.e., those with extents. */

/* Insert the string which begins at INDEX in buffer B into
   the current buffer at point. */

void
insert_buffer_string (b, index, length)
     struct buffer *b;
     int index, length;
{
  struct gcpro gcpro1;
  Lisp_Object str = make_string_from_buffer (b, index, length);
  GCPRO1 (str);
  insert_from_string (str, 0, XSTRING (str)->size);
  UNGCPRO;
}

/* Delete characters in current buffer
   from FROM up to (but not including) TO.  */

#ifdef ENERGIZE
extern int inside_parse_buffer; /* total kludge */
#endif

void
del_range (from, to)
     register int from, to;
{
  int numdel;

  /* Make args be valid */
  if (from < BEGV)
    from = BEGV;
  if (to > ZV)
    to = ZV;

  if ((numdel = to - from) <= 0)
    return;

  /* Make sure the gap is somewhere in or next to what we are deleting.  */
  if (from > GPT)
    gap_right (from);
  if (to < GPT)
    gap_left (to, 0);

  prepare_to_modify_buffer (from, to);

  /* Relocate point appropriately relative to the deleted chars. */
  if (from < point)
    {
      if (point < to)
	SET_PT (from);
      else
	SET_PT (point - numdel);
    }

#ifdef ENERGIZE
  if (!inside_parse_buffer)
#endif
    record_delete (from, numdel);

  MODIFF++;

  /* Relocate all markers pointing into the new, larger gap
     to point at the end of the text before the gap.  */
  adjust_markers (to + GAP_SIZE, to + GAP_SIZE, - numdel - GAP_SIZE);

  /* this must come AFTER record_delete(), so that the appropriate extents
     will be present to be recorded, and BEFORE the gap size of increased,
     as otherwise we will be confused about where the extents end. */
  /* Passing to + GAP_SIZE ensures that the extent update code leaves no
     extent endpoints hanging inside the gap.
     --Matthieu */
  process_extents_for_deletion (from, to, from, to + GAP_SIZE,
				current_buffer);

  GAP_SIZE += numdel;
  ZV -= numdel;
  Z -= numdel;
  GPT = from;

  if (GPT - BEG < beg_unchanged)
    beg_unchanged = GPT - BEG;
  if (Z - GPT < end_unchanged)
    end_unchanged = Z - GPT;

  /* Re-synchronize the point-marker with point.  This normally isn't
     necessary, because SET_PT does it, but because we need to set point
     before frobbing markers, above, the point-marker gets frobbed twice.
     */
  {
    int p = point;
    if (p > GPT) p += GAP_SIZE;
    XMARKER (current_buffer->point_marker)->bufpos = p;
  }

  signal_after_change (from, numdel, 0);
}
