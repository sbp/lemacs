/* Minibuffer input and completion.
   Copyright (C) 1985, 1986, 1992, 1993, 1994 Free Software Foundation, Inc.

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
#include "xterm.h"	/* for update_cursor decl */

#include "dispmisc.h"

#define min(a, b) ((a) < (b) ? (a) : (b))

/* Depth in minibuffer invocations.  */
int minibuf_level;

Lisp_Object Qcompletion_ignore_case;

/* Nonzero means completion ignores case.  */
int completion_ignore_case;

/* List of regexps that should restrict possible completions.  */
Lisp_Object Vcompletion_regexp_list;

/* Width in columns of current minibuffer prompt.  */
extern int minibuf_prompt_width;


DEFUN ("minibuffer-depth", Fminibuffer_depth, Sminibuffer_depth, 0, 0, 0,
  "Return current depth of activations of minibuffer, a nonnegative integer.")
  ()
{
  return make_number (minibuf_level);
}

/* The default buffer to use as the window-buffer of minibuffer windows */
/*  Note there is special code in kill-buffer to make this unkillable */
Lisp_Object Vminibuffer_zero;



/* Actual minibuffer invocation. */

static Lisp_Object
read_minibuffer_internal_unwind (Lisp_Object unwind_data)
{
  buffers_changed++;
  XWINDOW (minibuf_window)->last_modified = Qzero;
  XWINDOW (minibuf_window)->last_facechange = Qzero;
  Vminibuf_prompt = Felt (unwind_data, Qzero);
  minibuf_prompt_width = XINT (Felt (unwind_data, make_number (1)));
  minibuf_level = XINT (Felt (unwind_data, make_number (2)));
  while (CONSP (unwind_data))
  {
    Lisp_Object victim = unwind_data;
    unwind_data = XCONS (unwind_data)->cdr;
    free_cons (XCONS (victim));
  }

  /* If cursor is on the minibuffer line,
     show the user we have exited by putting it in column 0.  */
  /* #### The selected_screen->cur_mir check should no longer be needed.
     It is in as a safety check at the moment in order to make sure
     that all of the mirror problems are truly gone. */
  if (selected_screen->cur_mir)
    {
      if (MINI_WINDOW_P (XWINDOW (real_window (selected_screen->cur_mir, 0)))
	  && !noninteractive)
	{
	  /* Geez, I hate the cursor code.  I can't wait for these stupid
	     pointers into the redisplay structs to go away. */
	  if (selected_screen->cur_line && selected_screen->cur_line->body)
	    {
	      selected_screen->cursor_x = 0;
	      selected_screen->new_cur_char = selected_screen->cur_line->body;
	      update_cursor (selected_screen, 0);
	    }
	}
    }

  return Qnil;
}

DEFUN ("read-minibuffer-internal", 
       Fread_minibuffer_internal, Sread_minibuffer_internal, 
       1, 1, 0,
       "Lowest-level interface to minibuffers.  Don't call this.")
  (prompt)
     Lisp_Object prompt;
{
  int speccount = specpdl_depth ();
  Lisp_Object val;

  CHECK_STRING (prompt, 0);

  record_unwind_protect (read_minibuffer_internal_unwind,
                         list3 (Vminibuf_prompt,
                                make_number (minibuf_prompt_width),
                                make_number (minibuf_level)));
  Vminibuf_prompt = prompt;
  minibuf_level++;

  echo_area_glyphs = 0;

  val = call_command_loop (Qt);

  return (unbind_to (speccount, val));
}



/* Completion hair */

/* Compare exactly LEN chars of strings at S1 and S2,
   ignoring case if appropriate.
   Return -1 if strings match,
   else number of chars that match at the beginning.  */

int
scmp (CONST unsigned char *s1, CONST unsigned char *s2, int len)
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


int
regexp_ignore_completion_p (CONST char *completion, int len)
{
  /* Ignore this element if it fails to match all the regexps.  */
  if (!NILP (Vcompletion_regexp_list))
  {
    Lisp_Object regexps;
    for (regexps = Vcompletion_regexp_list;
         CONSP (regexps);
         regexps = XCONS (regexps)->cdr)
    {
      Lisp_Object re = XCONS (regexps)->car;
      if (STRINGP (re)
          && (fast_string_match (re, completion, len) >= 0))
        return (1);
    }
  }
  return (0);
}


/* Callers should GCPRO, since this may call eval */
static int
ignore_completion_p (Lisp_Object completion_string,
                     Lisp_Object pred, Lisp_Object completion)
{
  if (regexp_ignore_completion_p ((char *) XSTRING (completion_string)->data,
                                  string_length (XSTRING (completion_string))))
    return (1);
  
  /* Ignore this element if there is a predicate
     and the predicate doesn't like it. */
  if (!NILP (pred))
  {
    Lisp_Object tem;
    if (EQ (pred, Qcommandp))
      tem = Fcommandp (completion);
    else
      tem = call1 (pred, completion);
    if (NILP (tem))
      return (1);
  }
  return (0);
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
  Lisp_Object bestmatch, tail;
  int bestmatchsize;
  int list;
  int index;
  int obsize = 0;
  int matchcount = 0;
  Lisp_Object bucket;
  int slength, blength;

  CHECK_STRING (string, 0);

  if (CONSP (alist))
  {
    Lisp_Object tem = XCONS (alist)->car;
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
  blength = 0;
  slength = string_length (XSTRING (string));

  /* If ALIST is not a list, set TAIL just for gc pro.  */
  tail = alist;
  if (! list)
    {
      index = 0;
      obsize = vector_length (XVECTOR (alist));
      bucket = XVECTOR (alist)->contents[index];
    }

  while (1)
    {
      /* Get the next element of the alist or obarray. */
      /* Exit the loop if the elements are all used up. */
      /* elt gets the alist element or symbol.
	 eltstring gets the name to check as a completion. */
      Lisp_Object elt;
      Lisp_Object eltstring;

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
	  if (!EQ (bucket, Qzero))
	    {
              struct Lisp_Symbol *next = symbol_next (XSYMBOL (bucket));
	      elt = bucket;
	      eltstring = Fsymbol_name (elt);
              if (next)
		XSETSYMBOL (bucket, next);
	      else
		bucket = Qzero;
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

      if (STRINGP (eltstring))
	{
	  int eltlength = string_length (XSTRING (eltstring));
	  if (slength <= eltlength
	      && (0 > scmp (XSTRING (eltstring)->data,
                            XSTRING (string)->data,
                            slength)))
	    {
              {
                struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
                int loser;
                GCPRO4 (tail, string, eltstring, bestmatch);
                loser = ignore_completion_p (eltstring, pred, elt);
                UNGCPRO;
                if (loser)      /* reject this one */
                  continue;
              }

	      /* Update computation of how much all possible
		 completions match */

	      matchcount++;
	      if (NILP (bestmatch))
		{
		  bestmatch = eltstring;
                  blength = eltlength;
		  bestmatchsize = eltlength;
		}
	      else
		{
		  int compare = min (bestmatchsize, eltlength);
		  int matchsize = scmp (XSTRING (bestmatch)->data,
                                        XSTRING (eltstring)->data,
                                        compare);
		  if (matchsize < 0)
		    matchsize = compare;
		  if (completion_ignore_case)
		    {
		      /* If this is an exact match except for case,
			 use it as the best match rather than one that is not
			 an exact match.  This way, we get the case pattern
			 of the actual match.  */
		      if ((matchsize == eltlength
			   && matchsize < blength)
			  ||
			  /* If there is more than one exact match ignoring
			     case, and one of them is exact including case,
			     prefer that one.  */
			  /* If there is no exact match ignoring case,
			     prefer a match that does not change the case
			     of the input.  */
			  ((matchsize == eltlength)
			   ==
			   (matchsize == blength)
			   && !memcmp ((char *) XSTRING (eltstring)->data,
				       (char *) XSTRING (string)->data,
				       slength)
			   && memcmp ((char *) XSTRING (bestmatch)->data,
				      (char *) XSTRING (string)->data, 
				      slength)))
                      {
			bestmatch = eltstring;
                        blength = eltlength;
                      }
		    }
		  bestmatchsize = matchsize;
		}
	    }
	}
    }

  if (NILP (bestmatch))
    return Qnil;		/* No completions found */
  /* If we are ignoring case, and there is no exact match,
     and no additional text was supplied,
     don't change the case of what the user typed.  */
  if (completion_ignore_case
      && bestmatchsize == slength
      && blength > bestmatchsize)
    return string;

  /* Return t if the supplied string is an exact match (counting case);
     it does not require any change to be made.  */
  if (matchcount == 1
      && bestmatchsize == slength
      && !memcmp (XSTRING (bestmatch)->data, XSTRING (string)->data,
		  bestmatchsize))
    return Qt;

  /* Else extract the part in which all completions agree */
  return Fsubstring (bestmatch, Qzero, make_number (bestmatchsize));
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
  Lisp_Object tail;
  Lisp_Object allmatches;
  int list;
  int index;
  int obsize = 0;
  Lisp_Object bucket;
  int slength;

  CHECK_STRING (string, 0);

  if (CONSP (alist))
  {
    Lisp_Object tem = XCONS (alist)->car;
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
  slength = string_length (XSTRING (string));

  /* If ALIST is not a list, set TAIL just for gc pro.  */
  tail = alist;
  if (! list)
    {
      index = 0;
      obsize = vector_length (XVECTOR (alist));
      bucket = XVECTOR (alist)->contents[index];
    }

  while (1)
    {
      /* Get the next element of the alist or obarray. */
      /* Exit the loop if the elements are all used up. */
      /* elt gets the alist element or symbol.
	 eltstring gets the name to check as a completion. */
      Lisp_Object elt;
      Lisp_Object eltstring;

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
	  if (!EQ (bucket, Qzero))
	    {
              struct Lisp_Symbol *next = symbol_next (XSYMBOL (bucket));
	      elt = bucket;
	      eltstring = Fsymbol_name (elt);
              if (next)
		XSETSYMBOL (bucket, next);
	      else
		bucket = Qzero;
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

      if (STRINGP (eltstring) 
          && (slength <= string_length (XSTRING (eltstring)))
	  /* Reject alternatives that start with space
	     unless the input starts with space.  */
	  && ((XSTRING (string)->size > 0 && XSTRING (string)->data[0] == ' ')
	      || XSTRING (eltstring)->data[0] != ' ')
          && (0 > scmp (XSTRING (eltstring)->data,
                        XSTRING (string)->data,
                        slength)))
	{
	  /* Yes.  Now check whether predicate likes it. */
          struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
          int loser;
          GCPRO4 (tail, eltstring, allmatches, string);
          loser = ignore_completion_p (eltstring, pred, elt);
          UNGCPRO;
          if (!loser)
            /* Ok => put it on the list. */
            allmatches = Fcons (eltstring, allmatches);
        }
    }

  return Fnreverse (allmatches);
}

/* Useless RMSmacs functions */

DEFUN ("minibuffer-prompt", Fminibuffer_prompt, Sminibuffer_prompt, 0, 0, 0,
  "Return the prompt string of the currently-active minibuffer.\n\
If no minibuffer is active, return nil.")
  ()
{
  return (Fcopy_sequence (Vminibuf_prompt));
}

DEFUN ("minibuffer-prompt-width", Fminibuffer_prompt_width,
  Sminibuffer_prompt_width, 0, 0, 0,
  "Return the display width of the minibuffer prompt.")
  ()
{
  return (make_number (minibuf_prompt_width));
}



void
init_minibuf_once ()
{
  Vminibuffer_zero
    = Fget_buffer_create (Fpurecopy (build_string (" *Minibuf-0*")));
}

void
syms_of_minibuf ()
{
  minibuf_level = 0;

  defsymbol (&Qcompletion_ignore_case, "completion-ignore-case");

  DEFVAR_BOOL ("completion-ignore-case", &completion_ignore_case,
    "Non-nil means don't consider case significant in completion.");
  completion_ignore_case = 0;

  /* Worthless doc string */
  DEFVAR_LISP ("completion-regexp-list", &Vcompletion_regexp_list,
    "List of regexps that should restrict possible completions.");
  Vcompletion_regexp_list = Qnil;

  defsubr (&Sminibuffer_depth);
  defsubr (&Sminibuffer_prompt);
  defsubr (&Sminibuffer_prompt_width);

  defsubr (&Sread_minibuffer_internal);

  defsubr (&Stry_completion);
  defsubr (&Sall_completions);
}
