/* Definitions needed by most editing commands.
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

#ifndef _EMACS_COMMANDS_H_
#define _EMACS_COMMANDS_H_

/* Previous command symbol found here for comparison */
extern Lisp_Object last_command;

extern int immediate_quit;	    /* Nonzero means ^G can quit instantly */

extern Lisp_Object Vexecuting_macro;

extern Lisp_Object Qpre_command_hook, Qpost_command_hook;

extern Lisp_Object recent_keys_ring;
extern int recent_keys_ring_index;
  
extern Lisp_Object Qpre_command_hook, Qpost_command_hook;

extern Lisp_Object recent_keys_ring;
extern int recent_keys_ring_index;

/* Nonzero if input is coming from the keyboard */
#define INTERACTIVE (NILP (Vexecuting_macro) && !noninteractive)

/* Set this nonzero to force reconsideration of mode line. */
extern int redraw_mode_line;

/* Nonzero means reading single-character input with prompt
   so put cursor on minibuffer after the prompt.  */
extern int cursor_in_echo_area;

extern Lisp_Object Fcommand_execute (Lisp_Object cmd, Lisp_Object record);
extern Lisp_Object Fcommand_loop_1 (void);
extern Lisp_Object call_command_loop (Lisp_Object catch_errors);
/* #ifndef LISP_COMMAND_LOOP */
extern Lisp_Object Vtop_level;
/* #else */
extern Lisp_Object Vcommand_loop;
/* #endif */
extern DOESNT_RETURN initial_command_loop (Lisp_Object);

extern Lisp_Object Vkeyboard_translate_table;
extern Lisp_Object Vthis_command;
extern Lisp_Object Vlast_command;
extern Lisp_Object Vunread_command_event;
extern Lisp_Object Vlast_command_event;
extern Lisp_Object Vlast_input_event;
/* These two for compatibility; they are V... because they can be nil. */
extern Lisp_Object Vlast_command_char;
extern Lisp_Object Vlast_input_char;
extern Lisp_Object Vlast_input_time;
extern Lisp_Object Vcurrent_mouse_event;

extern int zmacs_regions;
extern int zmacs_region_active_p;
extern int zmacs_region_stays;
extern void zmacs_update_region (void);

#endif /* _EMACS_COMMANDS_H_ */
