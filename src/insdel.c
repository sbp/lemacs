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
#include "intl.h"
#include "lisp.h"
#include "buffer.h"
#include "insdel.h"
#include "extents.h"
#include "window.h"


/* Routines for dealing with the gap. */

/* Add `amount' to the position of every marker in the current buffer
   whose current position is between `from' (exclusive) and `to' (inclusive).
   Also, any markers past the outside of that interval, in the direction
   of adjustment, are first moved back to the near end of the interval
   and then adjusted by `amount'.  */

static void
adjust_markers (struct buffer *buf, int adjust_extents_as_well_p,
                int from, int to, int amount)
{
  struct Lisp_Marker *m;

  for (m = buf->markers; m; m = marker_next (m))
    {
      int mpos = m->bufpos;
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
    }
  if (adjust_extents_as_well_p)
    adjust_extents (from, to, amount, buf);
}

/* Move the gap to POS, which is less than the current GPT.   If NEWGAP
   is nonzero, then don't update beg_unchanged and end_unchanged.  */

static void
gap_left (struct buffer *buf, int pos, int newgap)
{
#ifdef I18N4
  register wchar_t *to, *from;
#else
  register unsigned char *to, *from;
#endif
  register int i;
  int new_s1;

  pos--;

  if (newgap == 0 && buf == current_buffer)
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

  i = BUF_GPT (buf);
  from = BUF_BEG_ADDR (buf) + BUF_GPT (buf) - 1;
  to = from + BUF_GAP_SIZE (buf);
  new_s1 = BUF_GPT (buf) - BUF_BEG (buf);

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
#ifdef GAP_USE_BCOPY
      if (i >= 128
	  /* bcopy is safe if the two areas of memory do not overlap
	     or on systems where bcopy is always safe for moving upward.  */
	  && (BCOPY_UPWARD_SAFE
	      || to - from >= 128))
	{
	  /* If overlap is not safe, avoid it by not moving too many
	     characters at once.  */
	  if (!BCOPY_UPWARD_SAFE && i > to - from)
	    i = to - from;
	  new_s1 -= i;
	  from -= i, to -= i;
	  memcpy (to, from, i);
	}
      else
#endif
	{
	  new_s1 -= i;
	  while (--i >= 0)
	    *--to = *--from;
	}
    }

  /* Adjust markers, and buffer data structure, to put the gap at POS.
     POS is where the loop above stopped, which may be what was specified
     or may be where a quit was detected.  */
  adjust_markers (buf, 1, 
                  pos + 1, BUF_GPT (buf), BUF_GAP_SIZE (buf));
  BUF_GPT (buf) = pos + 1;
  QUIT;
}

static void
gap_right (struct buffer *buf, int pos)
{
#ifdef I18N4
  register wchar_t *to, *from;
#else
  register unsigned char *to, *from;
#endif
  register int i;
  int new_s1;

  pos--;

  if (buf == current_buffer)
    {
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
    }

  i = BUF_GPT (buf);
  to = BUF_BEG_ADDR (buf) + BUF_GPT (buf) - 1;
  from = to + BUF_GAP_SIZE (buf);
  new_s1 = BUF_GPT (buf) - 1;

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
#ifdef GAP_USE_BCOPY
      if (i >= 128
	  /* bcopy is safe if the two areas of memory do not overlap
	     or on systems where bcopy is always safe for moving downward. */
	  && (BCOPY_DOWNWARD_SAFE
	      || from - to >= 128))
	{
	  /* If overlap is not safe, avoid it by not moving too many
	     characters at once.  */
	  if (!BCOPY_DOWNWARD_SAFE && i > from - to)
	    i = from - to;
	  new_s1 += i;
	  memcpy (to, from, i);
	  from += i, to += i;
	}
      else
#endif
	{
	  new_s1 += i;
	  while (--i >= 0)
	    *to++ = *from++;
	}
    }

  {
    int gsize = BUF_GAP_SIZE (buf);
    adjust_markers (buf, 1,
                    BUF_GPT (buf) + gsize,
                    pos + 1 + gsize,
                    - gsize);
    BUF_GPT (buf) = pos + 1;
  }
  QUIT;
}

/* Move gap to position `pos'.
   Note that this can quit!  */

void
move_gap (struct buffer *buf, int pos)
{
  if (pos < BUF_GPT (buf))
    gap_left (buf, pos, 0);
  else if (pos > BUF_GPT (buf))
    gap_right (buf, pos);
}

/* Make the gap INCREMENT characters longer.  */

void
make_gap (increment)
     int increment;
{
#ifdef I18N4
  wchar_t *result;
#else
  unsigned char *result;
#endif
  Lisp_Object tem;
  int real_gap_loc;
  int old_gap_size;

  /* If we have to get more space, get enough to last a while.  We use
     a geometric progession that saves on realloc space. */
  increment += 2000 + ((Z - BEG) / 8);

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
  gap_left (current_buffer, real_gap_loc + old_gap_size, 1);

  /* Now combine the two into one large gap.  */
  GAP_SIZE += old_gap_size;
  GPT = real_gap_loc;

  Vinhibit_quit = tem;
}

/* Check that it is okay to modify the buffer between START and END.
   Run the before-change-function, if any.  */

static void signal_before_change (int start, int end);

void
prepare_to_modify_buffer (start, end)
     int start, end;
{
  if (!NILP (current_buffer->read_only))
    Fbarf_if_buffer_read_only ();

  verify_extent_modification (current_buffer, start, end);

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

/* Call this if you're about to change the region of BUFFER from START
   to END.  This checks the read-only properties of the region, calls
   the necessary modification hooks, and warns the next redisplay that
   it should pay attention to that area.  */
void
modify_region (struct buffer *buf, int start, int end)
{
  Lisp_Object old = Qnil;
  struct gcpro gcpro1;

  if (buf != current_buffer)
  {
    set_buffer_internal (buf);
    old = Fcurrent_buffer ();
  }

  GCPRO1 (old);

  prepare_to_modify_buffer (start, end);

  if (start - 1 < beg_unchanged || unchanged_modified == MODIFF)
    beg_unchanged = start - 1;
  if (Z - end < end_unchanged
      || unchanged_modified == MODIFF)
    end_unchanged = Z - end;
  /* MODIFF++; -- should be done by callers (insert, delete range)
     else record_first_change isn't called */

  if (!NILP (old))
    set_buffer_internal (XBUFFER (old));

  UNGCPRO;
}

static int inside_change_hook;

static Lisp_Object
change_function_restore (Lisp_Object ignored)
{
  inside_change_hook = 0;
  return (ignored);
}

static int in_first_change;

static Lisp_Object
first_change_hook_restore (Lisp_Object value)
{
  in_first_change = 0;
  return Qnil;
}

/* Signal an initial modification to the buffer.  */

static void
signal_first_change (void)
{
  if (!NILP (Vfirst_change_hook) && !in_first_change)
    {
      int speccount = specpdl_depth ();

      record_unwind_protect (first_change_hook_restore, Qnil);
      gc_currently_forbidden = 1;
      in_first_change = 1;

      if (!NILP (Vrun_hooks))
	call1 (Vrun_hooks, Qfirst_change_hook);
      unbind_to (speccount, Qnil);
      gc_currently_forbidden = 0;
    }
}

/* Signal a change to the buffer immediatly before it happens.
   START and END are the bounds of the text to be changed,
   as Lisp objects.  */

static void
signal_before_change (int start, int end)
{
  /* If buffer is unmodified, run a special hook for that case.  */
  if (current_buffer->save_modified >= MODIFF
      && !NILP (Vfirst_change_hook)
      && !NILP (Vrun_hooks))
    signal_first_change();

  /* Now in any case run the before-change-function if any.  */
  if (!NILP (Vbefore_change_function) && !inside_change_hook)
    {
      int speccount = specpdl_depth ();
      record_unwind_protect (change_function_restore, Qzero);
      inside_change_hook = 1;
      run_hook_with_args (Qbefore_change_function,
			  2, make_number (start), make_number (end));
      unbind_to (speccount, Qnil);
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
  if (!NILP (Vafter_change_function) && !inside_change_hook)
    {
      int speccount = specpdl_depth ();
      record_unwind_protect (change_function_restore, Qzero);
      inside_change_hook = 1;
      run_hook_with_args (Qafter_change_function, 3, 
                          make_number (pos), 
                          make_number (pos + lenins),
                          make_number (lendel));
      unbind_to (speccount, Qnil);
    }
}

/* Insertion of strings. */

#ifdef I18N4
/* The only reason for the obj argument is to deal with extents. */
void
insert_wide_string (CONST wchar_t *string, int length, Lisp_Object obj)
{
  struct gcpro gcpro1;
  Lisp_Object dup_list = Qnil;
  Lisp_Object temp;
  int opoint = PT;

  if (STRINGP (obj))
    {
      dup_list = string_dups (XSTRING (obj));
    }

  if (length < 1)
    return;

  /* Make sure that point-max won't exceed the size of an emacs int. */
  XSET (temp, Lisp_Int, length + Z);
  if (length + Z != XINT (temp))
    error (GETTEXT ("maximum buffer size exceeded"));

  GCPRO1 (dup_list);

  prepare_to_modify_buffer (opoint, opoint);

  if (opoint != GPT)
    move_gap (current_buffer, opoint);
  if (GAP_SIZE < length)
    make_gap (length - GAP_SIZE);

  record_insert (opoint, length);
  MODIFF++;

  memcpy (GPT_ADDR, string, length * sizeof (wchar_t));

  process_extents_for_insertion (opoint, length, current_buffer);

  GAP_SIZE -= length;
  GPT += length;
  ZV += length;
  Z += length;
  SET_PT (opoint + length);

  splice_in_extent_replicas (opoint, length, 0, dup_list, current_buffer);
  signal_after_change (opoint, 0, length);

  UNGCPRO;
}

void
insert_relocatable_raw_string (CONST char *string, int length, Lisp_Object obj)
{
  struct gcpro gcpro1;
  int offset = -1;

  if (STRINGP (obj))
    {
      offset = ((string)
		? ((CONST unsigned char *) string - XSTRING (obj)->data)
		: 0);
      if (length < 0) length = XSTRING (obj)->size;
    }

  GCPRO1 (obj);

  /* string may have been relocated up to this point */
  if (STRINGP (obj))
    string = (char *) XSTRING (obj)->data + offset;

  length = mb_substring_to_wc (string, length, &mb_buf, &wc_buf);

  insert_wide_string (wc_buf.data, length, obj);

  UNGCPRO;
}
#else /* not I18N4 */
void
insert_relocatable_raw_string (CONST char *string, int length, Lisp_Object obj)
{
  struct gcpro gcpro1, gcpro2;
  Lisp_Object dup_list = Qnil;
  int offset = -1;
  Lisp_Object temp;
  int opoint = PT;

  if (STRINGP (obj))
  {
    dup_list = string_dups (XSTRING (obj));
    offset = ((string)
              ? ((CONST unsigned char *) string - XSTRING (obj)->data)
              : 0);
    if (length < 0) length = XSTRING (obj)->size;
  }

  if (length < 1)
    return;

  /* Make sure that point-max won't exceed the size of an emacs int. */
  XSET (temp, Lisp_Int, length + Z);
  if (length + Z != XINT (temp))
    error (GETTEXT ("maximum buffer size exceeded"));

  GCPRO2 (obj, dup_list);

  prepare_to_modify_buffer (opoint, opoint);

  if (opoint != GPT)
    move_gap (current_buffer, opoint);
  if (GAP_SIZE < length)
    make_gap (length - GAP_SIZE);

  record_insert (opoint, length);
  MODIFF++;

  /* string may have been relocated up to this point */
  if (STRINGP (obj))
    string = (char *) XSTRING (obj)->data + offset;

  memcpy (GPT_ADDR, string, length);

  GAP_SIZE -= length;
  GPT += length;
  ZV += length;
  Z += length;

  process_extents_for_insertion (opoint, length, current_buffer);

  SET_PT (opoint + length);

  splice_in_extent_replicas (opoint, length, 0, dup_list, current_buffer);
  signal_after_change (opoint, 0, length);

  UNGCPRO;
}
#endif /* I18N4 */

/* Insert the null-terminated string S before point */

void
insert_string (CONST char *s)
{
  insert_relocatable_raw_string (s, strlen (s), Qzero);
}

void
insert_char (int ch)
{
  char c = ch;
  insert_relocatable_raw_string (&c, 1, Qzero);
}

/* Like `insert_relocatable_raw_string' except that all markers pointing
   at the place where the insertion happens are adjusted to point after it. */

void
insert_before_markers (CONST char *string, int length, 
                       Lisp_Object obj)
{
  register int opoint = PT;
  if (length < 0 && STRINGP (obj))
    length = XSTRING (obj)->size;
  if (length < 1)
    return;
  insert_relocatable_raw_string (string, length, obj);
  adjust_markers (current_buffer, 0,
                  opoint - 1, opoint, length);
}

#ifdef I18N4
/* Insert the wide character WC before point */
void
insert_wide_char (wchar_t wc)
{
  insert_wide_string (&wc, 1, Qnil);
}
#endif

/* Insert the string which begins at INDEX in buffer B into
   the current buffer at point. */

void
insert_buffer_string (struct buffer *b, int index, int length)
{
  insert_relocatable_raw_string (0, -1, 
                                 make_string_from_buffer (b, index, length));
}


/* Delete characters in current buffer
   from FROM up to (but not including) TO.  */

#ifdef ENERGIZE
extern int inside_parse_buffer; /* total kludge */
#endif

void
del_range (int from, int to)
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
  if (to < GPT)
    gap_left (current_buffer, to, 0);
  if (from > GPT)
    gap_right (current_buffer, from);

  prepare_to_modify_buffer (from, to);

#ifdef ENERGIZE
  if (!inside_parse_buffer)
#endif
    record_delete (from, numdel);
  MODIFF++;

  /* Relocate point as if it were a marker.  */
  if (from < PT)
    {
      if (PT < to)
	SET_PT (from);
      else
	SET_PT (PT - numdel);
    }

  /* Relocate all markers pointing into the new, larger gap
     to point at the end of the text before the gap.  */
  adjust_markers (current_buffer, 0,
                  (to + GAP_SIZE),
                  (to + GAP_SIZE), 
                  (- numdel - GAP_SIZE));

  /* this must come AFTER record_delete(), so that the appropriate extents
     will be present to be recorded, and BEFORE the gap size is increased,
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
    int p = PT;
    if (p > GPT) p += GAP_SIZE;
    XMARKER (current_buffer->point_marker)->bufpos = p;
  }

  signal_after_change (from, numdel, 0);
}


void
init_insdel ()
{
  inside_change_hook = 0;
}
