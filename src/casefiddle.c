/* GNU Emacs case conversion functions.
   Copyright (C) 1985, 1992, 1993 Free Software Foundation, Inc.

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
#include "insdel.h"
#include "commands.h"
#include "syntax.h"
#include "intl.h"

enum case_action {CASE_UP, CASE_DOWN, CASE_CAPITALIZE, CASE_CAPITALIZE_UP};

static Lisp_Object
casify_object (flag, obj)
     enum case_action flag;
     Lisp_Object obj;
{
  register int i, c, len;
  register int inword = flag == CASE_DOWN;
  Lisp_Object syntax_table = current_buffer->syntax_table;

  while (1)
    {
      if (FIXNUMP (obj))
	{
	  c = XINT (obj);
	  if (c >= 0 && c <= 0400)
	    {
	      if (inword)
		obj = make_number (DOWNCASE (c));
	      else if (!UPPERCASEP (c))
		obj = make_number (UPCASE1 (c));
	    }
	  return obj;
	}
      if (STRINGP (obj))
	{
	  obj = Fcopy_sequence (obj);
	  len = XSTRING (obj)->size;
	  for (i = 0; i < len; i++)
	    {
	      c = XSTRING (obj)->data[i];
	      if (inword)
		c = DOWNCASE (c);
	      else if (!UPPERCASEP (c))
		c = UPCASE1 (c);
	      XSTRING (obj)->data[i] = c;
	      if (flag == CASE_CAPITALIZE)
		inword = SYNTAX (syntax_table, c) == Sword;
	    }
	  return obj;
	}
      obj = wrong_type_argument (Qchar_or_string_p, obj);
    }
}

DEFUN ("upcase", Fupcase, Supcase, 1, 1, 0,
  "Convert argument to upper case and return that.\n\
The argument may be a character or string.  The result has the same type.\n\
The argument object is not altered.  See also `capitalize'.")
  (obj)
     Lisp_Object obj;
{
  return casify_object (CASE_UP, obj);
}

DEFUN ("downcase", Fdowncase, Sdowncase, 1, 1, 0,
  "Convert argument to lower case and return that.\n\
The argument may be a character or string.  The result has the same type.\n\
The argument object is not altered.")
  (obj)
     Lisp_Object obj;
{
  return casify_object (CASE_DOWN, obj);
}

DEFUN ("capitalize", Fcapitalize, Scapitalize, 1, 1, 0,
  "Convert argument to capitalized form and return that.\n\
This means that each word's first character is upper case\n\
and the rest is lower case.\n\
The argument may be a character or string.  The result has the same type.\n\
The argument object is not altered.")
  (obj)
     Lisp_Object obj;
{
  return casify_object (CASE_CAPITALIZE, obj);
}

/* flag is CASE_UP, CASE_DOWN or CASE_CAPITALIZE or CASE_CAPITALIZE_UP.
   b and e specify range of buffer to operate on. */

static void
casify_region (flag, b, e)
     enum case_action flag;
     Lisp_Object b, e;
{
  register int i;
#ifdef I18N4
  register wchar_t c;
#else
  register int c;
#endif
  register int inword = flag == CASE_DOWN;
  Lisp_Object syntax_table = current_buffer->syntax_table;

  if (EQ (b, e))
    /* Not modifying because nothing marked */
    return;

  validate_region (&b, &e);
  modify_region (current_buffer, XINT (b), XINT (e));
  record_change (XINT (b), XINT (e) - XINT (b));
  MODIFF++;

  for (i = XINT (b); i < XINT (e); i++)
    {
      c = FETCH_CHAR (i);
      if (inword && flag != CASE_CAPITALIZE_UP)
	c = DOWNCASE (c);
      else if (!UPPERCASEP (c)
	       && (!inword || flag != CASE_CAPITALIZE_UP))
	c = UPCASE1 (c);
      *(CHAR_ADDRESS (i)) = c;
      if ((int) flag >= (int) CASE_CAPITALIZE)
	inword = SYNTAX (syntax_table, c) == Sword;
    }

  signal_after_change (XINT (b),
		       XINT (e) - XINT (b), 
		       XINT (e) - XINT (b));
}

DEFUN ("upcase-region", Fupcase_region, Supcase_region, 2, 2, "r",
  "Convert the region to upper case.  In programs, wants two arguments.\n\
These arguments specify the starting and ending character numbers of\n\
the region to operate on.  When used as a command, the text between\n\
point and the mark is operated on.\n\
See also `capitalize-region'.")
  (b, e)
     Lisp_Object b, e;
{
  casify_region (CASE_UP, b, e);
  return Qnil;
}

DEFUN ("downcase-region", Fdowncase_region, Sdowncase_region, 2, 2, "r",
  "Convert the region to lower case.  In programs, wants two arguments.\n\
These arguments specify the starting and ending character numbers of\n\
the region to operate on.  When used as a command, the text between\n\
point and the mark is operated on.")
  (b, e)
     Lisp_Object b, e;
{
  casify_region (CASE_DOWN, b, e);
  return Qnil;
}

DEFUN ("capitalize-region", Fcapitalize_region, Scapitalize_region, 2, 2, "r",
  "Convert the region to capitalized form.\n\
Capitalized form means each word's first character is upper case\n\
and the rest of it is lower case.\n\
In programs, give two arguments, the starting and ending\n\
character positions to operate on.")
  (b, e)
     Lisp_Object b, e;
{
  casify_region (CASE_CAPITALIZE, b, e);
  return Qnil;
}

/* Like Fcapitalize but change only the initials.  */

Lisp_Object
upcase_initials_region (b, e)
     Lisp_Object b, e;
{
  casify_region (CASE_CAPITALIZE_UP, b, e);
  return Qnil;
}

static Lisp_Object
operate_on_word (arg)
     Lisp_Object arg;
{
  int end, farend;

  CHECK_FIXNUM (arg, 0);
  farend = scan_words (PT, XINT (arg));
  if (!farend)
    farend = XINT (arg) > 0 ? ZV : BEGV;

  end = ((PT > farend) ? PT : farend);
  SET_PT (end);
  return (make_number (farend));
}

DEFUN ("upcase-word", Fupcase_word, Supcase_word, 1, 1, "p",
  "Convert following word (or ARG words) to upper case, moving over.\n\
With negative argument, convert previous words but do not move.\n\
See also `capitalize-word'.")
  (arg)
     Lisp_Object arg;
{
  Lisp_Object opoint;

  opoint = make_number (PT);
  casify_region (CASE_UP, opoint, operate_on_word (arg));
  return Qnil;
}

DEFUN ("downcase-word", Fdowncase_word, Sdowncase_word, 1, 1, "p",
  "Convert following word (or ARG words) to lower case, moving over.\n\
With negative argument, convert previous words but do not move.")
  (arg)
     Lisp_Object arg;
{
  Lisp_Object opoint;
  opoint = make_number (PT);
  casify_region (CASE_DOWN, opoint, operate_on_word (arg));
  return Qnil;
}

DEFUN ("capitalize-word", Fcapitalize_word, Scapitalize_word, 1, 1, "p",
  "Capitalize the following word (or ARG words), moving over.\n\
This gives the word(s) a first character in upper case\n\
and the rest lower case.\n\
With negative argument, capitalize previous words but do not move.")
  (arg)
     Lisp_Object arg;
{
  Lisp_Object opoint;
  opoint = make_number (PT);
  casify_region (CASE_CAPITALIZE, opoint, operate_on_word (arg));
  return Qnil;
}

void
syms_of_casefiddle ()
{
  defsubr (&Supcase);
  defsubr (&Sdowncase);
  defsubr (&Scapitalize);
  defsubr (&Supcase_region);
  defsubr (&Sdowncase_region);
  defsubr (&Scapitalize_region);
  defsubr (&Supcase_word);
  defsubr (&Sdowncase_word);
  defsubr (&Scapitalize_word);
}
