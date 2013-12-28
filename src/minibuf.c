/* Minibuffer input and completion.
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


#include <stdio.h>
#include "config.h"

#include "lisp.h"
#include "commands.h"
#include "buffer.h"
#include "screen.h"
#include "window.h"
#include "syntax.h"

#define min(a, b) ((a) < (b) ? (a) : (b))

/* Depth in minibuffer invocations.  */
int minibuf_level;

/* help-form is bound to this while in the minibuffer.  */
Lisp_Object Vminibuffer_help_form;

/* Nonzero means completion ignores case.  */
int completion_ignore_case;

/* Width in columns of current minibuffer prompt.  */
extern int minibuf_prompt_width;

/* Width in pixels of current minibuffer prompt.  */
extern int minibuf_prompt_pix_width;

#ifdef MULTI_SCREEN
/* When the global-minibuffer-screen is not used, this is the screen
   where the minbuffer is active, and thus where certain windows
   (completions, etc.) should appear. */
struct screen *active_screen;

extern Lisp_Object Vglobal_minibuffer_screen;
#endif

/* Actual minibuffer invocation. */

DEFUN ("minibuffer-depth", Fminibuffer_depth, Sminibuffer_depth, 0, 0, 0,
  "Return current depth of activations of minibuffer, a nonnegative integer.")
  ()
{
  return make_number (minibuf_level);
}

/* The default buffer to use as the window-buffer of minibuffer windows */
/*  Note there is special code in kill-buffer to make this unkillable */
Lisp_Object Vminibuffer_zero;




static Lisp_Object
read_minibuffer_internal_unwind (Lisp_Object unwind_data)
{
  windows_or_buffers_changed++;
  XFASTINT (XWINDOW (minibuf_window)->last_modified) = 0;
  XFASTINT (XWINDOW (minibuf_window)->last_facechange) = 0;
  Vminibuf_prompt = Felt (unwind_data, make_number (0));
  minibuf_prompt_width = XFASTINT (Felt (unwind_data, make_number (1)));
  minibuf_prompt_pix_width = XFASTINT (Felt (unwind_data, make_number (2)));
  minibuf_level = XFASTINT (Felt (unwind_data, make_number (3)));
  while (CONSP (unwind_data))
  {
    Lisp_Object victim = unwind_data;
    unwind_data = XCONS (unwind_data)->cdr;
    free_cons (XCONS (victim));
  }
  return Qnil;
}

extern Lisp_Object command_loop_2 (Lisp_Object);

DEFUN ("read-minibuffer-internal", 
       Fread_minibuffer_internal, Sread_minibuffer_internal, 
       1, 1, 0,
       "Lowest-level interface to minibuffers.  Don't call this.")
  (prompt)
     Lisp_Object prompt;
{
  int speccount = specpdl_depth;
  Lisp_Object val;

  CHECK_STRING (prompt, 0);

  record_unwind_protect (read_minibuffer_internal_unwind,
                         list4 (Vminibuf_prompt,
                                make_number (minibuf_prompt_width),
                                make_number (minibuf_prompt_pix_width),
                                make_number (minibuf_level)));
  Vminibuf_prompt = prompt;
  minibuf_level++;

#ifdef MULTI_SCREEN
  if (SCREENP (Vglobal_minibuffer_screen))
    active_screen = selected_screen;
#endif

  echo_area_glyphs = 0;

  val = command_loop_2 (Qnil);

  /* If cursor is on the minibuffer line,
     show the user we have exited by putting it in column 0.  */
  if ((SCREEN_CURSOR_Y (selected_screen)
       >= XFASTINT (XWINDOW (minibuf_window)->top))
      && !noninteractive)
    {
      SCREEN_CURSOR_X (selected_screen) = 0;
      update_screen (selected_screen, 1, 1);
    }
#ifdef MULTI_SCREEN
  if (active_screen)
    active_screen = (struct screen *) 0;
#endif

  return (unbind_to (speccount, val));
}




/* Completion hair */

/* Compare exactly LEN chars of strings at S1 and S2,
   ignoring case if appropriate.
   Return -1 if strings match,
   else number of chars that match at the beginning.  */

int
scmp (s1, s2, len)
     register const char *s1, *s2;
     int len;
{
  register int l = len;

  if (completion_ignore_case)
    {
      while (l && DOWNCASE (*s1++) == DOWNCASE (*s2++))
	l--;
    }
  else
    {
      while (l && *s1++ == *s2++)
	l--;
    }
  if (l == 0)
    return -1;
  else return len - l;
}

DEFUN ("try-completion", Ftry_completion, Stry_completion, 2, 3, 0,
  "Return common substring of all completions of STRING in ALIST.\n\
Each car of each element of ALIST is tested to see if it begins with STRING.\n\
All that match are compared together; the longest initial sequence\n\
common to all matches is returned as a string.\n\
If there is no match at all, nil is returned.\n\
For an exact match, t is returned.\n\
\n\
ALIST can be an obarray instead of an alist.\n\
Then the print names of all symbols in the obarray are the possible matches.\n\
\n\
ALIST can also be a function to do the completion itself.\n\
It receives three arguments: the values STRING, PREDICATE and nil.\n\
Whatever it returns becomes the value of `try-completion'.\n\
\n\
If optional third argument PREDICATE is non-nil,\n\
it is used to test each possible match.\n\
The match is a candidate only if PREDICATE returns non-nil.\n\
The argument given to PREDICATE is the alist element or the symbol from the obarray.")
  (string, alist, pred)
     Lisp_Object string, alist, pred;
{
  Lisp_Object bestmatch, tail, elt, eltstring;
  int bestmatchsize;
  int compare, matchsize;
  int list;
  int index, obsize;
  int matchcount = 0;
  Lisp_Object bucket, zero, end, tem;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

  CHECK_STRING (string, 0);

  if (CONSP (alist))
  {
    tem = XCONS (alist)->car;
    if (SYMBOLP (tem))          /* lambda, autoload, etc.  Emacs-lisp sucks */
      return call3 (alist, string, pred, Qnil);
    else
      list = 1;
  }
  else if (VECTORP (alist))
    list = 0;
  else if (NILP (alist))
    list = 1;
  else
    return call3 (alist, string, pred, Qnil);

  bestmatch = Qnil;

  /* If ALIST is not a list, set TAIL just for gc pro.  */
  tail = alist;
  if (! list)
    {
      index = 0;
      obsize = XVECTOR (alist)->size;
      bucket = XVECTOR (alist)->contents[index];
    }

  while (1)
    {
      /* Get the next element of the alist or obarray. */
      /* Exit the loop if the elements are all used up. */
      /* elt gets the alist element or symbol.
	 eltstring gets the name to check as a completion. */

      if (list)
	{
	  if (NILP (tail))
	    break;
	  elt = Fcar (tail);
	  eltstring = Fcar (elt);
	  tail = Fcdr (tail);
	}
      else
	{
	  if (XFASTINT (bucket) != 0)
	    {
              struct Lisp_Symbol *next = symbol_next (XSYMBOL (bucket));
	      elt = bucket;
	      eltstring = Fsymbol_name (elt);
              if (next)
		XSET (bucket, Lisp_Symbol, next);
	      else
		XFASTINT (bucket) = 0;
	    }
	  else if (++index >= obsize)
	    break;
	  else
	    {
	      bucket = XVECTOR (alist)->contents[index];
	      continue;
	    }
	}

      /* Is this element a possible completion? */

      if (STRINGP (eltstring) &&
	  XSTRING (string)->size <= XSTRING (eltstring)->size &&
	  0 > scmp ((char *) XSTRING (eltstring)->data,
		    (char *) XSTRING (string)->data,
		    XSTRING (string)->size))
	{
	  /* Yes. */
	  /* Ignore this element if there is a predicate
	     and the predicate doesn't like it. */

	  if (!NILP (pred))
	    {
	      if (EQ (pred, Qcommandp))
		tem = Fcommandp (elt);
	      else
		{
		  GCPRO4 (tail, string, eltstring, bestmatch);
		  tem = call1 (pred, elt);
		  UNGCPRO;
		}
	      if (NILP (tem)) continue;
	    }

	  /* Update computation of how much all possible completions match */

	  matchcount++;
	  if (NILP (bestmatch))
	    bestmatch = eltstring, bestmatchsize = XSTRING (eltstring)->size;
	  else
	    {
	      compare = min (bestmatchsize, XSTRING (eltstring)->size);
	      matchsize = scmp ((char *) XSTRING (bestmatch)->data,
				(char *) XSTRING (eltstring)->data,
				compare);
	      if (matchsize < 0)
		matchsize = compare;
	      if (completion_ignore_case)
		{
		  /* If this is an exact match except for case,
		     use it as the best match rather than one that is not an
		     exact match.  This way, we get the case pattern
		     of the actual match.  */
		  if ((matchsize == XSTRING (eltstring)->size
		       && matchsize < XSTRING (bestmatch)->size)
		      ||
		      /* If there is more than one exact match ignoring case,
			 and one of them is exact including case,
			 prefer that one.  */
		      /* If there is no exact match ignoring case,
			 prefer a match that does not change the case
			 of the input.  */
		      ((matchsize == XSTRING (eltstring)->size)
		       ==
		       (matchsize == XSTRING (bestmatch)->size)
		       && !memcmp ((char *) XSTRING (eltstring)->data,
				   (char *) XSTRING (string)->data,
				   XSTRING (string)->size)
		       && memcmp ((char *) XSTRING (bestmatch)->data,
				  (char *) XSTRING (string)->data, 
				  XSTRING (string)->size)))
		    bestmatch = eltstring;
		}
	      bestmatchsize = matchsize;
	    }
	}
    }

  if (NILP (bestmatch))
    return Qnil;		/* No completions found */
  /* If we are ignoring case, and there is no exact match,
     and no additional text was supplied,
     don't change the case of what the user typed.  */
  if (completion_ignore_case && bestmatchsize == XSTRING (string)->size
      && XSTRING (bestmatch)->size > bestmatchsize)
    return string;

  /* Return t if the supplied string is an exact match (counting case);
     it does not require any change to be made.  */
  if (matchcount == 1 && bestmatchsize == XSTRING (string)->size
      && !memcmp (XSTRING (bestmatch)->data, XSTRING (string)->data,
		  bestmatchsize))
    return Qt;

  XFASTINT (zero) = 0;		/* Else extract the part in which */
  XFASTINT (end) = bestmatchsize;	     /* all completions agree */
  return Fsubstring (bestmatch, zero, end);
}


DEFUN ("all-completions", Fall_completions, Sall_completions, 2, 3, 0,
  "Search for partial matches to STRING in ALIST.\n\
Each car of each element of ALIST is tested to see if it begins with STRING.\n\
The value is a list of all the strings from ALIST that match.\n\
ALIST can be an obarray instead of an alist.\n\
Then the print names of all symbols in the obarray are the possible matches.\n\
\n\
ALIST can also be a function to do the completion itself.\n\
It receives three arguments: the values STRING, PREDICATE and t.\n\
Whatever it returns becomes the value of `all-completions'.\n\
\n\
If optional third argument PREDICATE is non-nil,\n\
it is used to test each possible match.\n\
The match is a candidate only if PREDICATE returns non-nil.\n\
The argument given to PREDICATE is the alist element or\n\
the symbol from the obarray.")
  (string, alist, pred)
     Lisp_Object string, alist, pred;
{
  Lisp_Object tail, elt, eltstring;
  Lisp_Object allmatches;
  int list;
  int index, obsize;
  Lisp_Object bucket, tem;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

  CHECK_STRING (string, 0);

  if (CONSP (alist))
  {
    tem = XCONS (alist)->car;
    if (SYMBOLP (tem))          /* lambda, autoload, etc.  Emacs-lisp sucks */
      return call3 (alist, string, pred, Qt);
    else
      list = 1;
  }
  else if (VECTORP (alist))
    list = 0;
  else if (NILP (alist))
    list = 1;
  else
    return call3 (alist, string, pred, Qt);

  allmatches = Qnil;

  /* If ALIST is not a list, set TAIL just for gc pro.  */
  tail = alist;
  if (! list)
    {
      index = 0;
      obsize = XVECTOR (alist)->size;
      bucket = XVECTOR (alist)->contents[index];
    }

  while (1)
    {
      /* Get the next element of the alist or obarray. */
      /* Exit the loop if the elements are all used up. */
      /* elt gets the alist element or symbol.
	 eltstring gets the name to check as a completion. */

      if (list)
	{
	  if (NILP (tail))
	    break;
	  elt = Fcar (tail);
	  eltstring = Fcar (elt);
	  tail = Fcdr (tail);
	}
      else
	{
	  if (XFASTINT (bucket) != 0)
	    {
              struct Lisp_Symbol *next = symbol_next (XSYMBOL (bucket));
	      elt = bucket;
	      eltstring = Fsymbol_name (elt);
              if (next)
		XSET (bucket, Lisp_Symbol, next);
	      else
		XFASTINT (bucket) = 0;
            }
	  else if (++index >= obsize)
	    break;
	  else
	    {
	      bucket = XVECTOR (alist)->contents[index];
	      continue;
	    }
	}

      /* Is this element a possible completion? */

      if (STRINGP (eltstring) &&
	  XSTRING (string)->size <= XSTRING (eltstring)->size &&
	  XSTRING (eltstring)->data[0] != ' ' &&
	  0 > scmp ((char *) XSTRING (eltstring)->data,
		    (char *) XSTRING (string)->data,
		    XSTRING (string)->size))
	{
	  /* Yes. */
	  /* Ignore this element if there is a predicate
	     and the predicate doesn't like it. */

	  if (!NILP (pred))
	    {
	      if (EQ (pred, Qcommandp))
		tem = Fcommandp (elt);
	      else
		{
		  GCPRO4 (tail, eltstring, allmatches, string);
		  tem = call1 (pred, elt);
		  UNGCPRO;
		}
	      if (NILP (tem)) continue;
	    }
	  /* Ok => put it on the list. */
	  allmatches = Fcons (eltstring, allmatches);
	}
    }

  return Fnreverse (allmatches);
}


void
init_minibuf_once ()
{
  Vminibuffer_zero = Fget_buffer_create (build_string (" *Minibuf-0*"));
}

void
syms_of_minibuf ()
{
  minibuf_level = 0;

  DEFVAR_BOOL ("completion-ignore-case", &completion_ignore_case,
    "Non-nil means don't consider case significant in completion.");
  completion_ignore_case = 0;

  defsubr (&Sminibuffer_depth);

  defsubr (&Sread_minibuffer_internal);

  defsubr (&Stry_completion);
  defsubr (&Sall_completions);
}
