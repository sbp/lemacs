/* Primitives for word-abbrev mode.
   Copyright (C) 1985, 1986, 1992, 1993 Free Software Foundation, Inc.

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
#include <stdio.h>
#include "lisp.h"
#include "commands.h"
#include "buffer.h"
#include "window.h"
#include "insdel.h"

/* An abbrev table is an obarray.
   Each defined abbrev is represented by a symbol in that obarray
   whose print name is the abbreviation.
   The symbol's value is a string which is the expansion.
   If its function definition is non-nil, it is called
   after the expansion is done.
   The plist slot of the abbrev symbol is its usage count. */

/* The table of global abbrevs.  These are in effect
   in any buffer in which abbrev mode is turned on. */
Lisp_Object Vglobal_abbrev_table;

/* The local abbrev table used by default (in Fundamental Mode buffers) */
Lisp_Object Vfundamental_mode_abbrev_table;

int abbrev_all_caps;

/* Non-nil => use this location as the start of abbrev to expand
 (rather than taking the word before point as the abbrev) */
Lisp_Object Vabbrev_start_location;

/* Buffer that Vabbrev_start_location applies to */
Lisp_Object Vabbrev_start_location_buffer;

/* The symbol representing the abbrev most recently expanded */
Lisp_Object Vlast_abbrev;

/* A string for the actual text of the abbrev most recently expanded.
   This has more info than Vlast_abbrev since case is significant.  */
Lisp_Object Vlast_abbrev_text;

/* Character address of start of last abbrev expanded */
int last_abbrev_point;

/* Hook to run before expanding any abbrev.  */
Lisp_Object Vpre_abbrev_expand_hook, Qpre_abbrev_expand_hook;


/* Expand the word before point, if it is an abbrev.
   Returns Qt if an expansion is done. */

DEFUN ("expand-abbrev", Fexpand_abbrev, Sexpand_abbrev, 0, 0, "",
  "Expand the abbrev before point, if there is an abbrev there.\n\
Effective when explicitly called even when `abbrev-mode' is nil.\n\
Returns t if expansion took place.")
  ()
{
  register char *buffer, *p;
  register int wordstart, wordend, idx;
  int whitecnt;
  int uccount = 0, lccount = 0;
  register Lisp_Object sym;
  Lisp_Object expansion, hook, tem, value;
  int oldmodiff = MODIFF;

  if (!NILP (Vrun_hooks))
    call1 (Vrun_hooks, Qpre_abbrev_expand_hook);
  /* If the hook changes the buffer, treat that as having "done an
     expansion".  */
  value = (MODIFF != oldmodiff ? Qt : Qnil);

  if (XBUFFER (Vabbrev_start_location_buffer) != current_buffer)
    Vabbrev_start_location = Qnil;
  if (!NILP (Vabbrev_start_location))
    {
      tem = Vabbrev_start_location;
      CHECK_FIXNUM_COERCE_MARKER (tem, 0);
      wordstart = XINT (tem);
      Vabbrev_start_location = Qnil;
      if (CHAR_AT (wordstart) == '-')
	del_range (wordstart, wordstart + 1);
    }
  else
    wordstart = scan_words (point, -1);

  if (!wordstart)
    return value;

  wordend = scan_words (wordstart, 1);
  if (!wordend)
    return value;

  if (wordend > point)
    wordend = point;
  whitecnt = point - wordend;
  if (wordend <= wordstart)
    return value;

  p = buffer = (char *) alloca (wordend - wordstart);

  for (idx = wordstart; idx < wordend; idx++)
    {
      register int c = CHAR_AT (idx);
      if (UPPERCASEP (c))
	c = DOWNCASE (c), uccount++;
      else if (! NOCASEP (c))
	lccount++;
      *p++ = c;
    }

  if (VECTORP (current_buffer->abbrev_table))
    sym = oblookup (current_buffer->abbrev_table,
		    (unsigned char *) buffer,
		    p - buffer);
  else
    sym = Qzero;
  if (FIXNUMP (sym) || NILP (XSYMBOL (sym)->value))
    sym = oblookup (Vglobal_abbrev_table,
		    (unsigned char *) buffer,
		    p - buffer);
  if (FIXNUMP (sym) || NILP (XSYMBOL (sym)->value))
    return value;

  if (INTERACTIVE && !EQ (minibuf_window, selected_window))
    {
      SET_PT (wordend);
      Fundo_boundary ();
    }
  SET_PT (wordstart);
  Vlast_abbrev_text
    = Fbuffer_substring (make_number (wordstart), make_number (wordend));
  del_range (wordstart, wordend);

  /* Now sym is the abbrev symbol. */
  Vlast_abbrev = sym;
  last_abbrev_point = wordstart;

  if (FIXNUMP (XSYMBOL (sym)->plist))
    XSETINT (XSYMBOL (sym)->plist,
	     XINT (XSYMBOL (sym)->plist) + 1);	/* Increment use count */

  expansion = XSYMBOL (sym)->value;
  insert_from_string (expansion, -1);
  SET_PT (point + whitecnt);

  if (uccount && !lccount)
    {
      /* Abbrev was all caps */
      /* If expansion is multiple words, normally capitalize each word */
      /* This used to be if (!... && ... >= ...) Fcapitalize; else Fupcase
	 but Megatest 68000 compiler can't handle that */
      if (!abbrev_all_caps)
	if (scan_words (point, -1) > scan_words (wordstart, 1))
	  {
	    upcase_initials_region (make_number (wordstart),
				    make_number (point));
	    goto caped;
	  }
      /* If expansion is one word, or if user says so, upcase it all. */
      Fupcase_region (make_number (wordstart), make_number (point));
    caped: ;
    }
  else if (uccount)
    {
      /* Abbrev included some caps.  Cap first initial of expansion */
      int old_zv = ZV;
      int old_pt = point;

      /* Don't let Fcapitalize_word operate on text after point.  */
      ZV = point;
      SET_PT (wordstart);
      Fcapitalize_word (make_number (1));

      SET_PT (old_pt);
      ZV = old_zv;
    }

  hook = XSYMBOL (sym)->function;
  if (!NILP (hook))
    call0 (hook);

  return Qt;
}

void
syms_of_abbrev ()
{
  DEFVAR_LISP ("global-abbrev-table", &Vglobal_abbrev_table,
    "The abbrev table whose abbrevs affect all buffers.\n\
Each buffer may also have a local abbrev table.\n\
If it does, the local table overrides the global one\n\
for any particular abbrev defined in both.");
  Vglobal_abbrev_table = Qnil;  /* setup by Lisp code */
  Vfundamental_mode_abbrev_table = Qnil;/* setup by Lisp code */
  current_buffer->abbrev_table = Vfundamental_mode_abbrev_table;

  DEFVAR_LISP ("fundamental-mode-abbrev-table",
	       &Vfundamental_mode_abbrev_table,
    "The abbrev table of mode-specific abbrevs for Fundamental Mode.");

  DEFVAR_LISP ("last-abbrev", &Vlast_abbrev,
    "The abbrev-symbol of the last abbrev expanded.\n\
See the function `abbrev-symbol'.");

  DEFVAR_LISP ("last-abbrev-text", &Vlast_abbrev_text,
    "The exact text of the last abbrev expanded.\n\
nil if the abbrev has already been unexpanded.");

  DEFVAR_INT ("last-abbrev-location", &last_abbrev_point,
    "The location of the start of the last abbrev expanded.");

  Vlast_abbrev = Qnil;
  Vlast_abbrev_text = Qnil;
  last_abbrev_point = 0;

  DEFVAR_LISP ("abbrev-start-location", &Vabbrev_start_location,
    "Buffer position for `expand-abbrev' to use as the start of the abbrev.\n\
nil means use the word before point as the abbrev.\n\
Calling `expand-abbrev' sets this to nil.");
  Vabbrev_start_location = Qnil;

  DEFVAR_LISP ("abbrev-start-location-buffer", &Vabbrev_start_location_buffer,
    "Buffer that `abbrev-start-location' has been set for.\n\
Trying to expand an abbrev in any other buffer clears `abbrev-start-location'.");
  Vabbrev_start_location_buffer = Qnil;

  DEFVAR_BOOL ("abbrev-all-caps", &abbrev_all_caps,
    "*Set non-nil means expand multi-word abbrevs all caps if abbrev was so.");
  abbrev_all_caps = 0;

  DEFVAR_LISP ("pre-abbrev-expand-hook", &Vpre_abbrev_expand_hook,
    "Function or functions to be called before abbrev expansion is done.\n\
This is the first thing that `expand-abbrev' does, and so this may change\n\
the current abbrev table before abbrev lookup happens.");
  Vpre_abbrev_expand_hook = Qnil;
  defsymbol (&Qpre_abbrev_expand_hook, "pre-abbrev-expand-hook");

  defsubr (&Sexpand_abbrev);
}
