/* Buffer insertion/deletion and gap motion for GNU Emacs.
   Copyright (C) 1985, 1986, 1991, 1992 Free Software Foundation, Inc.

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

/* Move gap to position `pos'.   Note that this can quit!  */
extern void move_gap (int pos);

/* Make the gap INCREMENT characters longer.  */
extern void make_gap (int increment);

/* Check that it is okay to modify the buffer between START and END.
   Run the before-change-function, if any.  */
extern void prepare_to_modify_buffer (int start, int end);

extern void modify_region (int start, int end);

/* Signal a change immediatly after it happens. */
extern void signal_after_change (int pos, int lendel, int lenins);

extern void insert_relocatable_raw_string (const char *string,
                                           int length, 
                                           Lisp_Object obj);

extern void insert_from_string (Lisp_Object obj, int pos, int length);

/* Insert a raw string of specified length before point */
extern void insert_raw_string (const char *string, int length);

extern void insert (const char *string, int length);

/* Insert the null-terminated string S before point */
extern void insert_string (const char *s);

/* Insert the character C before point */
/*   insert_relocatable_raw_string (&ch, 1, 0) */
extern void insert_char (char c);

/* Like `insert_raw_string' except that all markers pointing
   at the place where the insertion happens are adjusted to point after it. */
extern void insert_before_markers (const char *string, int length);

extern void insert_from_string_before_markers (Lisp_Object string,
                                               int pos, int length);

/* Insert the string which begins at INDEX in buffer B into
   the current buffer at point. */
extern void insert_buffer_string (struct buffer *b, int index, int length);

/* Delete characters in current buffer
   from FROM up to (but not including) TO.  */
extern void del_range (int from, int to);
