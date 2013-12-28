/* Definitions of marked slots in buffers
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

/* In the declaration of the buffer structure, this file is included
   after defining MARKED_SLOT(x) to be Lisp_Object x; i.e. just a slot
   definition.  In the garbage collector this file is included after
   defining MARKED_SLOT(x) to be mark_object(buffer->x). */

    /* Active regions in this buffer. */
    MARKED_SLOT (extents);

    /* the name of this buffer */
    MARKED_SLOT (name);

    /* the name of the file associated with this buffer */
    MARKED_SLOT (filename);

    /* the truename of the filename (via the realpath() system call) */
    MARKED_SLOT (truename);

    /* Dir for expanding relative pathnames */
    MARKED_SLOT (directory);

    /* true iff this buffer has been been backed
       up (if you write to its associated file
       and it hasn't been backed up, then a
       backup will be made) */
    /* #### This isn't really used by the C code, so could be deleted.  */
    MARKED_SLOT (backed_up);

    /* Length of file when last read or saved. */
    MARKED_SLOT (save_length);

    /* file name used for auto-saving this buffer */
    MARKED_SLOT (auto_save_file_name);

    /* Non-nil if buffer read-only */
    MARKED_SLOT (read_only);

    /* "The mark"; no longer allowed to be nil */
    MARKED_SLOT (mark);

    /* Alist of elements (SYMBOL . VALUE-IN-THIS-BUFFER)
       for all per-buffer variables of this buffer.  */
    MARKED_SLOT (local_var_alist);

    /* Symbol naming major mode (eg lisp-mode) */
    MARKED_SLOT (major_mode);

    /* Pretty name of major mode (eg "Lisp") */
    MARKED_SLOT (mode_name);

    /* Format string for mode line */
    MARKED_SLOT (mode_line_format);

    /* Keys that are bound local to this buffer */
    MARKED_SLOT (keymap);

    /* This buffer's local abbrev table */
    MARKED_SLOT (abbrev_table);
    /* This buffer's syntax table. */
    MARKED_SLOT (syntax_table);

    /* Values of several buffer-local variables.

       tab-width is buffer-local so that redisplay can find it
       in buffers that are not current */
    MARKED_SLOT (case_fold_search);
    MARKED_SLOT (tab_width);
    MARKED_SLOT (fill_column);
    MARKED_SLOT (left_margin);

    /* Function to call when insert space past fill column */
    MARKED_SLOT (auto_fill_function);

    /* String of length 256 mapping each char to its lower-case version.  */
    MARKED_SLOT (downcase_table);
    /* String of length 256 mapping each char to its upper-case version.  */
    MARKED_SLOT (upcase_table);

    /* Translate table for case-folding search.  */
    MARKED_SLOT (case_canon_table);
    /* Inverse translate (equivalence class) table for case-folding search. */
    MARKED_SLOT (case_eqv_table);

    /* Non-nil means do not display continuation lines */
    MARKED_SLOT (truncate_lines);
    /* Non-nil means display ctl chars with uparrow */
    MARKED_SLOT (ctl_arrow);
    /* Non-nil means do selective display;
       See doc string in syms_of_buffer (buffer.c) for details.  */
    MARKED_SLOT (selective_display);
    /* Non-nil means show ... at end of line followed by invisible lines.  */
    MARKED_SLOT (selective_display_ellipses);
    /* Alist of (FUNCTION . STRING) for each minor mode enabled in buffer. */
    MARKED_SLOT (minor_modes);
    /* t if "self-insertion" should overwrite */
    MARKED_SLOT (overwrite_mode);
    /* non-nil means abbrev mode is on.  Expand abbrevs automatically. */
    MARKED_SLOT (abbrev_mode);

    /* Display table to use for text in this buffer. */
    MARKED_SLOT (display_table);

    /* Changes in the buffer are recorded here for undo.
       t means don't record anything.  */
    MARKED_SLOT (undo_list);

    /* A redundant copy of text.pt, in the form of a marker.  Every time one
       is updated, so is the other.
     */
    MARKED_SLOT (point_marker);

    /* If dedicated_screen is non-nil, display_buffer tries to use it instead
       of the current screen */
    MARKED_SLOT (dedicated_screen);

    /* Widths of the annotation margins in characters (em's). */
    MARKED_SLOT (left_outside_margin_width);
    MARKED_SLOT (right_outside_margin_width);
    MARKED_SLOT (use_left_overflow);
