/* Hooks by which low level terminal operations
   can be made to call other routines.
   Copyright (C) 1985-1993 Free Software Foundation, Inc.

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

#ifndef _EMACS_TERMHOOKS_H_
#define _EMACS_TERMHOOKS_H_

extern void (*cursor_to_hook) ();
extern void (*raw_cursor_to_hook) ();

extern void (*clear_to_end_hook) ();
extern void (*clear_screen_hook) ();
extern void (*clear_end_of_line_hook) ();

extern void (*ins_del_lines_hook) ();

extern void (*change_line_highlight_hook) ();
extern void (*reassert_line_highlight_hook) ();

extern void (*insert_glyphs_hook) ();
extern void (*write_glyphs_hook) ();
extern void (*delete_glyphs_hook) ();

extern void (*ring_bell_hook) ();

extern void (*reset_terminal_modes_hook) ();
extern void (*set_terminal_modes_hook) ();
extern void (*update_begin_hook) ();
extern void (*update_end_hook) ();
extern void (*set_terminal_window_hook) ();

extern Lisp_Object (*read_socket_hook) ();

/* If nonzero, send all terminal output characters to this stream also.  */

extern FILE *termscript;

#endif /* _EMACS_TERMHOOKS_H_ */
