/* Keyboard macros.
   Copyright (C) 1985, 1986, 1992 Free Software Foundation, Inc.

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

/* A keyboard macro is a string of ASCII characters, or a vector of event
   objects.  Only key-press, mouse-press, mouse-release, and menu-selection
   events ever get into a keyboard macro.

   When interactively defining a keyboard macro, it will always be a vector
   of events; strings may be executed for backwards compatibility.
 */

#include "config.h"
#include "lisp.h"
#include "events.h"
#include "macros.h"
#include "commands.h"
#include "buffer.h"
#include "window.h"

Lisp_Object Qexecute_kbd_macro;

int defining_kbd_macro;

/* This is a lisp vector, which contains the events of the keyboard macro
   currently being read.  It is reallocated when the macro gets too large.
 */
static Lisp_Object kbd_macro_builder;
int kbd_macro_end;  /* index into the above vector where new events go */
int kbd_macro_ptr;  /* index into the above vector which is for real */

int pre_command_kbd_macro_end;	/* used for backing up kbd_macro_end */


Lisp_Object Vlast_kbd_macro;

/* The current macro and our position in it.  When executing nested kbd
   macros, previous values for these are wound through the execution stack
   with unwind-protect.
 */
Lisp_Object Vexecuting_macro;
int executing_macro_index;

Lisp_Object Fexecute_kbd_macro ();

DEFUN ("start-kbd-macro", Fstart_kbd_macro, Sstart_kbd_macro, 1, 1, "P",
  "Record subsequent keyboard and menu input, defining a keyboard macro.\n\
The commands are recorded even as they are executed.\n\
Use \\[end-kbd-macro] to finish recording and make the macro available.\n\
Use \\[name-last-kbd-macro] to give it a permanent name.\n\
Non-nil arg (prefix arg) means append to last macro defined;\n\
 This begins by re-executing that macro as if you typed it again.")
  (append)
     Lisp_Object append;
{
  if (defining_kbd_macro)
      error ("Already defining kbd macro!");

  redraw_mode_line++;
  if (NILP (append))
    {
      kbd_macro_ptr = 0;
      kbd_macro_end = 0;
      pre_command_kbd_macro_end = 0;
      message ("Defining kbd macro...");
    }
  else
    {
      message ("Appending to kbd macro...");
      kbd_macro_ptr = kbd_macro_end;
      Fexecute_kbd_macro (Vlast_kbd_macro, make_number (1));
    }
  defining_kbd_macro++;

  return Qnil;
}

DEFUN ("end-kbd-macro", Fend_kbd_macro, Send_kbd_macro, 0, 1, "p",
  "Finish defining a keyboard macro.\n\
The definition was started by \\[start-kbd-macro].\n\
The macro is now available for use via \\[call-last-kbd-macro],\n\
or it can be given a name with \\[name-last-kbd-macro] and then invoked\n\
under that name.\n\
\n\
With numeric arg, repeat macro now that many times,\n\
counting the definition just completed as the first repetition.\n\
An argument of zero means repeat until error.")
  (arg)
     Lisp_Object arg;
{
  if (!defining_kbd_macro)
      error ("Not defining kbd macro.");

  if (NILP (arg))
    XFASTINT (arg) = 1;
  else
    CHECK_FIXNUM (arg, 0);

  if (defining_kbd_macro)
    {
      int i;
      int size = kbd_macro_end;
      Vlast_kbd_macro = Fmake_vector (size, Qnil);
      for (i = 0; i < kbd_macro_end; i++)
	XVECTOR (Vlast_kbd_macro)->contents [i] =
	  XVECTOR (kbd_macro_builder)->contents [i];
      defining_kbd_macro = 0;
      redraw_mode_line++;
      message ("Keyboard macro defined");
    }

  if (XFASTINT (arg) == 0)
    Fexecute_kbd_macro (Vlast_kbd_macro, arg);
  else
    {
      XFASTINT (arg)--;
      if (XFASTINT (arg) > 0)
	Fexecute_kbd_macro (Vlast_kbd_macro, arg);
    }
  return Qnil;
}


/* Store event into kbd macro being defined
 */
void
store_kbd_macro_event (event)
     Lisp_Object event;
{
  if (kbd_macro_ptr == XVECTOR (kbd_macro_builder)->size)
    {
      int i;
      int old_size = XVECTOR (kbd_macro_builder)->size;
      int new_size = old_size * 2;
      Lisp_Object new = Fmake_vector (new_size, Qnil);
      for (i = 0; i < old_size; i++)
	XVECTOR (new)->contents [i] =
	  XVECTOR (kbd_macro_builder)->contents [i];
      kbd_macro_builder = new;
    }
  XVECTOR (kbd_macro_builder)->contents [kbd_macro_ptr++] =
    Fcopy_event (event, Qnil);
}


extern void key_desc_list_to_event (); /* from keymap.c */

/* Extract the next kbd-macro element into the given event.
   If we're done, throws to the catch in Fexecute_kbd_macro().
 */
void
pop_kbd_macro_event (event)
     Lisp_Object event;
{
  if (NILP (Vexecuting_macro)) abort ();

  switch (XTYPE (Vexecuting_macro)) {
  case Lisp_String:
    if (XSTRING (Vexecuting_macro)->size > executing_macro_index)
      {
	unsigned int c = 
          (unsigned char)
            (XSTRING (Vexecuting_macro)->data [executing_macro_index++]);
        Lisp_Object c_as_int;
        XSET (c_as_int, Lisp_Int, c);
	Fcharacter_to_event (c_as_int, event);
	return;
      }
    break;
  case Lisp_Vector:
    if (XVECTOR (Vexecuting_macro)->size > executing_macro_index)
      {
	Lisp_Object macro_event =
	  XVECTOR (Vexecuting_macro)->contents [executing_macro_index++];
	switch (XTYPE (macro_event)) {
	case Lisp_Int:
	  character_to_event (XINT (macro_event), XEVENT (event));
	  break;
	case Lisp_Cons:
	case Lisp_Symbol:
	  key_desc_list_to_event (macro_event, event, 1);
	  break;
	default:
	  Fcopy_event (macro_event, event);
	}
	return;
      }
    break;
  }

  Fthrow (Qexecute_kbd_macro, Qt);
}


/* Declare that all chars stored so far in the kbd macro being defined
 really belong to it.  This is done in between editor commands.  */

extern Lisp_Object unread_command_event;

void
finalize_kbd_macro_chars ()
{
  kbd_macro_end = kbd_macro_ptr;
}

DEFUN ("call-last-kbd-macro", Fcall_last_kbd_macro, Scall_last_kbd_macro,
  0, 1, "p",
  "Call the last keyboard macro that you defined with \\[start-kbd-macro].\n\
\n\
A prefix argument serves as a repeat count.  Zero means repeat until error.\n\
\n\
To make a macro permanent so you can call it even after\n\
defining others, use \\[name-last-kbd-macro].")
  (prefix)
     Lisp_Object prefix;
{
  if (defining_kbd_macro)
    error ("Can't execute anonymous macro while defining one");
  else if (NILP (Vlast_kbd_macro))
    error ("No kbd macro has been defined");
  else
    Fexecute_kbd_macro (Vlast_kbd_macro, prefix);
  return Qnil;
}

static Lisp_Object
pop_kbd_macro (info)
     Lisp_Object info;
{
  Lisp_Object tem;
  Vexecuting_macro = Fcar (info);
  tem = Fcdr (info);
  executing_macro_index = XINT (tem);
  return Qnil;
}

extern Lisp_Object command_loop_1 ();
extern int reset_this_command_keys;

DEFUN ("execute-kbd-macro", Fexecute_kbd_macro, Sexecute_kbd_macro, 1, 2, 0,
  "Execute MACRO as string of editor command characters.\n\
If MACRO is a symbol, its function definition is used.\n\
COUNT is a repeat count, or nil for once, or 0 for infinite loop.")
  (macro, prefixarg)
     Lisp_Object macro, prefixarg;
{
  Lisp_Object final;
  Lisp_Object tem;
  int count = specpdl_ptr - specpdl;
  int repeat = 1;
  struct gcpro gcpro1;

  if (!NILP (prefixarg))
    prefixarg = Fprefix_numeric_value (prefixarg),
    repeat = XINT (prefixarg);

  final = macro;
  while (SYMBOLP (final) && !EQ (final, Qunbound))
    final = XSYMBOL (final)->function;
  if (!STRINGP (final) && !VECTORP (final))
    CHECK_STRING (final, 0);

  XFASTINT (tem) = executing_macro_index;
  tem = Fcons (Vexecuting_macro, tem);
  record_unwind_protect (pop_kbd_macro, tem);

  GCPRO1 (final);
  do
    {
      Vexecuting_macro = final;
      executing_macro_index = 0;
      reset_this_command_keys = 1;
      internal_catch (Qexecute_kbd_macro, command_loop_1, Qnil);
    }
  while (--repeat &&
	 (STRINGP (Vexecuting_macro) ||
	  VECTORP (Vexecuting_macro)));

  UNGCPRO;
  unbind_to (count);

  return Qnil;
}


init_macros ()
{
  Vlast_kbd_macro = Qnil;
  defining_kbd_macro = 0;
  reset_this_command_keys = 1;

  Vexecuting_macro = Qnil;
}

syms_of_macros ()
{
  kbd_macro_builder = Fmake_vector (100, Qnil);
  kbd_macro_end = kbd_macro_ptr = pre_command_kbd_macro_end = 0;
  staticpro (&kbd_macro_builder);

  defsubr (&Sstart_kbd_macro);
  defsubr (&Send_kbd_macro);
  defsubr (&Scall_last_kbd_macro);
  defsubr (&Sexecute_kbd_macro);
  Qexecute_kbd_macro = intern ("execute-kbd-macro");
  staticpro (&Qexecute_kbd_macro);

  DEFVAR_BOOL ("defining-kbd-macro", &defining_kbd_macro,
    "Non-nil while a keyboard macro is being defined.  Don't set this!");

  DEFVAR_LISP ("executing-macro", &Vexecuting_macro,
    "Currently executing keyboard macro (a vector of events);\n\
nil if none executing.");

  DEFVAR_LISP ("executing-kbd-macro", &Vexecuting_macro,
    "Currently executing keyboard macro (a vector of events);\n\
nil if none executing.");

  DEFVAR_LISP ("last-kbd-macro", &Vlast_kbd_macro,
    "Last kbd macro defined, as a vector of events; nil if none defined.");
}

keys_of_macros ()
{
  initial_define_key (control_x_map, ('e'), "call-last-kbd-macro");
  initial_define_key (control_x_map, ('('), "start-kbd-macro");
  initial_define_key (control_x_map, (')'), "end-kbd-macro");
}
