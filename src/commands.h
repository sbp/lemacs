/* Definitions needed by most editing commands.
   Copyright (C) 1985 Free Software Foundation, Inc.

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


#define Ctl(c) ((c)&037)

/* Define the names of keymaps, just so people can refer to
   them in calls to initial_define_key */

extern struct Lisp_Keymap *global_map;
				/* default global key bindings */

extern struct Lisp_Keymap *meta_map;
				/* The keymap used for globally bound
				   ESC-prefixed default commands */

extern struct Lisp_Keymap *control_x_map;
				/* The keymap used for globally bound
				   C-x-prefixed default commands */

extern Lisp_Object Vminibuffer_local_map;
				/* The keymap used by the minibuf for
				   local bindings when spaces are allowed
				   in the minibuf */

extern Lisp_Object Vminibuffer_local_ns_map;
				/* The keymap used by the minibuf for
				   local bindings when spaces are not
				   encouraged in the minibuf */

/* keymap used for minibuffers when doing completion */
extern Lisp_Object Vminibuffer_local_completion_map;

/* keymap used for minibuffers when doing completion and require a match */
extern Lisp_Object Vminibuffer_local_must_match_map;

/* Previous command symbol found here for comparison */
extern Lisp_Object last_command;

extern int immediate_quit;	    /* Nonzero means ^G can quit instantly */

extern Lisp_Object Vexecuting_macro;

/* Nonzero if input is coming from the keyboard */

#define INTERACTIVE (NILP (Vexecuting_macro) && !noninteractive)

/* Set this nonzero to force reconsideration of mode line. */

extern int redraw_mode_line;

/* Nonzero means reading single-character input with prompt
   so put cursor on minibuffer after the prompt.  */

extern int cursor_in_echo_area;
