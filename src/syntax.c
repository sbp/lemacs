/* GNU Emacs routines to deal with syntax tables; also word and list parsing.
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


#include "config.h"
#include <ctype.h>
#include "lisp.h"
#include "commands.h"
#include "buffer.h"
#include "syntax.h"
#include "insdel.h"
#include "intl.h"

Lisp_Object Qsyntax_table_p;

int words_include_escapes;

int parse_sexp_ignore_comments;
  
Lisp_Object Vstandard_syntax_table;

/* This is the internal form of the parse state used in parse-partial-sexp.  */

struct lisp_parse_state
  {
    int depth;		/* Depth at end of parsing */
    int instring;	/* -1 if not within string, else desired terminator. */
    int incomment;	/* Nonzero if within a comment at end of parsing */
    int comstyle;	/* comment style a=0, or b=1 */
    int quoted;		/* Nonzero if just after an escape char at end of parsing */
    int thislevelstart;	/* Char number of most recent start-of-expression at current level */
    int prevlevelstart; /* Char number of start of containing expression */
    int location;	/* Char number at which parsing stopped. */
    int mindepth;	/* Minimum depth seen while scanning.  */
    int comstart;	/* Position just after last comment starter.  */
  };

/* These variables are a cache for finding the start of a defun.
   find_start_pos is the place for which the defun start was found.
   find_start_value is the defun start position found for it.
   find_start_buffer is the buffer it was found in.
   find_start_begv is the BEGV value when it was found.
   find_start_modiff is the value of MODIFF when it was found.  */

static int find_start_pos;
static int find_start_value;
static struct buffer *find_start_buffer;
static int find_start_begv;
static int find_start_modiff;

/* Find a defun-start that is the last one before POS (or nearly the last).
   We record what we find, so that another call in the same area
   can return the same value right away.  */

static int
find_defun_start (pos)
     int pos;
{
  int tem;
  int shortage;
  Lisp_Object table = current_buffer->syntax_table;

  /* Use previous finding, if it's valid and applies to this inquiry.  */
  if (current_buffer == find_start_buffer
      /* Reuse the defun-start even if POS is a little farther on.
	 POS might be in the next defun, but that's ok.
	 Our value may not be the best possible, but will still be usable.  */
      && pos <= find_start_pos + 1000
      && pos >= find_start_value
      && BEGV == find_start_begv
      && MODIFF == find_start_modiff)
    return find_start_value;

  /* Back up to start of line.  */
  tem = scan_buffer (current_buffer, '\n', pos, -1, &shortage, 1);

  while (tem > BEGV)
    {
      /* Open-paren at start of line means we found our defun-start.  */
      if (SYNTAX (table, FETCH_CHAR (tem)) == Sopen)
	break;
      /* Move to beg of previous line.  */
      tem = scan_buffer (current_buffer, '\n', tem, -2, &shortage, 1);
    }

  /* Record what we found, for the next try.  */
  find_start_value = tem;
  find_start_buffer = current_buffer;
  find_start_modiff = MODIFF;
  find_start_begv = BEGV;
  find_start_pos = pos;

  return find_start_value;
}

DEFUN ("syntax-table-p", Fsyntax_table_p, Ssyntax_table_p, 1, 1, 0,
  "Return t if ARG is a syntax table.\n\
Any vector of 256 elements will do.")
  (obj)
     Lisp_Object obj;
{
  if (VECTORP (obj) && vector_length (XVECTOR (obj)) == 0400)
    return Qt;
  return Qnil;
}

static Lisp_Object
check_syntax_table (Lisp_Object obj, Lisp_Object def)
{
  if (NILP (obj))
    obj = def;
  while (NILP (Fsyntax_table_p (obj)))
    obj = wrong_type_argument (Qsyntax_table_p, obj);
  return (obj);
}

DEFUN ("syntax-table", Fsyntax_table, Ssyntax_table, 0, 0, 0,
  "Return the current syntax table.\n\
This is the one specified by the current buffer.")
  ()
{
  return current_buffer->syntax_table;
}

DEFUN ("standard-syntax-table", Fstandard_syntax_table,
   Sstandard_syntax_table, 0, 0, 0,
  "Return the standard syntax table.\n\
This is the one used for new buffers.")
  ()
{
  return Vstandard_syntax_table;
}

DEFUN ("copy-syntax-table", Fcopy_syntax_table, Scopy_syntax_table, 0, 1, 0,
  "Construct a new syntax table and return it.\n\
It is a copy of the TABLE, which defaults to the standard syntax table.")
  (table)
     Lisp_Object table;
{
  Lisp_Object val = make_vector (0400, Qzero);

  if (NILP (Vstandard_syntax_table))
    /* Can only be null during initialization */
    return val;

  table = check_syntax_table (table, Vstandard_syntax_table);

  memcpy (XVECTOR (val)->contents, XVECTOR (table)->contents,
	  0400 * sizeof (Lisp_Object));
  return val;
}

DEFUN ("set-syntax-table", Fset_syntax_table, Sset_syntax_table, 1, 1, 0,
  "Select a new syntax table for the current buffer.\n\
One argument, a syntax table.")
  (table)
     Lisp_Object table;
{
  table = check_syntax_table (table, Qnil);
  current_buffer->syntax_table = table;
  /* Indicate that this buffer now has a specified syntax table.  */
  current_buffer->local_var_flags |=
    XFASTINT (buffer_local_flags.syntax_table);
  return table;
}

/* Convert a letter which signifies a syntax code
 into the code it signifies.
 This is used by modify-syntax-entry, and other things. */

CONST unsigned char syntax_spec_code[0400] =
  { 0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    (char) Swhitespace, 0377, (char) Sstring, 0377,
        (char) Smath, 0377, 0377, (char) Squote,
    (char) Sopen, (char) Sclose, 0377, 0377,
	0377, (char) Swhitespace, (char) Spunct, (char) Scharquote,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    0377, 0377, 0377, 0377,
	(char) Scomment, 0377, (char) Sendcomment, 0377,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,   /* @, A, ... */
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, (char) Sword,
    0377, 0377, 0377, 0377, (char) Sescape, 0377, 0377, (char) Ssymbol,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,   /* `, a, ... */
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, (char) Sword,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377
  };

/* Indexed by syntax code, give the letter that describes it. */


CONST unsigned char syntax_code_spec[13] =
  {
    ' ', '.', 'w', '_', '(', ')', '\'', '\"', '$', '\\', '/', '<', '>'
  };

DEFUN ("char-syntax", Fchar_syntax, Schar_syntax, 1, 2, 0,
  "Return the syntax code of CHAR, described by a character.\n\
For example, if CHAR is a word constituent, the character `?w' is returned.\n\
The characters that correspond to various syntax codes\n\
are listed in the documentation of `modify-syntax-entry'.\n\
Optional second argument TABLE default's to the current buffer's\n\
syntax table.")
  (ch, table)
     Lisp_Object ch, table;
{
  CHECK_FIXNUM (ch, 0);
  table = check_syntax_table (table, current_buffer->syntax_table);

  return make_number (syntax_code_spec[(int) SYNTAX (table,
                                                     0xFF & XINT (ch))]);
}


/* Return the position across COUNT words from FROM.
   If that many words cannot be found before the end of the buffer, return 0.
   COUNT negative means scan backward and stop at word beginning.  */

int
scan_words (from, count)
     register int from, count;
{
  register int beg = BEGV;
  register int end = ZV;
  register enum syntaxcode code;
  Lisp_Object table = current_buffer->syntax_table;
  
  immediate_quit = 1;
  QUIT;
  
  while (count > 0)
    {
      while (1)
	{
	  if (from == end)
	    {
	      immediate_quit = 0;
	      return 0;
	    }
	  code = SYNTAX (table, FETCH_CHAR (from));
	  if (words_include_escapes
	      && (code == Sescape || code == Scharquote))
	    break;
	  if (code == Sword)
	    break;
	  from++;
	}
      while (1)
	{
	  if (from == end) break;
	  code = SYNTAX (table, FETCH_CHAR (from));
	  if (!(words_include_escapes
		&& (code == Sescape || code == Scharquote)))
	    if (code != Sword)
	      break;
	  from++;
	}
      count--;
    }
  while (count < 0)
    {
      while (1)
	{
	  if (from == beg)
	    {
	      immediate_quit = 0;
	      return 0;
	    }
	  code = SYNTAX (table, FETCH_CHAR (from - 1));
	  if (words_include_escapes
	      && (code == Sescape || code == Scharquote))
	    break;
	  if (code == Sword)
	    break;
	  from--;
	}
      while (1)
	{
	  if (from == beg) break;
	  code = SYNTAX (table, FETCH_CHAR (from - 1));
	  if (!(words_include_escapes
		&& (code == Sescape || code == Scharquote)))
	    if (code != Sword)
	      break;
	  from--;
	}
      count++;
    }

  immediate_quit = 0;

  return from;
}

DEFUN ("forward-word", Fforward_word, Sforward_word, 1, 1, "_p",
  "Move point forward ARG words (backward if ARG is negative).\n\
Normally returns t.\n\
If an edge of the buffer is reached, point is left there\n\
and nil is returned.")
  (count)
     Lisp_Object count;
{
  int val;
  CHECK_FIXNUM (count, 0);

  if (!(val = scan_words (PT, XINT (count))))
    {
      SET_PT (XINT (count) > 0 ? ZV : BEGV);
      return Qnil;
    }
  SET_PT (val);
  return Qt;
}

static int char_quoted (int);
static void scan_sexps_forward (struct lisp_parse_state *,
				int from, int end, 
				int targetdepth, int stopbefore,
				Lisp_Object oldstate,
				int commentstop);

static int
find_start_of_comment (from, stop, mask)
    int from, stop, mask;
{
  int c;
  enum syntaxcode code;
  Lisp_Object table = current_buffer->syntax_table;

  /* Look back, counting the parity of string-quotes,
     and recording the comment-starters seen.
     When we reach a safe place, assume that's not in a string;
     then step the main scan to the earliest comment-starter seen
     an even number of string quotes away from the safe place.
     
     OFROM[I] is position of the earliest comment-starter seen
     which is I+2X quotes from the comment-end.
     PARITY is current parity of quotes from the comment end.  */
  int parity = 0;
  char my_stringend = 0;
  int string_lossage = 0;
  int comment_end = from;
  int comstart_pos = 0;
  int comstart_parity = 0;
  int styles_match_p = 0;

  /* At beginning of range to scan, we're outside of strings;
     that determines quote parity to the comment-end.  */
  while (from != stop)
    {
      /* Move back and examine a character.  */
      from--;

      c = FETCH_CHAR (from);
      code = SYNTAX (table, c);

      /* is this a 1-char comment end sequence? if so, try
	 to see if style matches previously extracted mask */
      if (code == Sendcomment)
	{
	  styles_match_p = SYNTAX_STYLES_MATCH_1CHAR_P (table, c, mask);
	}

      /* otherwise, is this a 2-char comment end sequence?
	 if so, back up, and see if style matches previously
	 extracted mask */
      else if (from > stop
	       && SYNTAX_END_P (table, FETCH_CHAR (from-1), c))
	{
	  code = Sendcomment;
	  styles_match_p = SYNTAX_STYLES_MATCH_END_P (table,
						      FETCH_CHAR (from-1),
						      c, mask);
	  from--;
	}
			
      /* or are we looking at a 1-char comment start sequence
	 of the style matching mask? */
      else if (code == Scomment
	       && SYNTAX_STYLES_MATCH_1CHAR_P (table, c, mask))
	{
	  styles_match_p = 1;
	}
		    
      /* or possibly, a 2-char comment start sequence */
      else if (from > stop
	       && SYNTAX_STYLES_MATCH_START_P (table, FETCH_CHAR (from-1),
					       c, mask))
	{
	  code = Scomment;
	  from--;
	  styles_match_p = 1;
	}

      /* Ignore escaped characters.  */
      if (char_quoted (from))
	continue;

      /* Track parity of quotes.  */
      if (code == Sstring)
	{
	  parity ^= 1;
	  if (my_stringend == 0)
	    my_stringend = c;
	  /* If we have two kinds of string delimiters.
	     There's no way to grok this scanning backwards.  */
	  else if (my_stringend != c)
	    string_lossage = 1;
	}

      /* Record comment-starters according to that
	 quote-parity to the comment-end.  */
      if (code == Scomment && styles_match_p)
	{
	  comstart_parity = parity;
	  comstart_pos = from;
	}

      /* If we find another earlier comment-ender,
	 any comment-starts earlier than that don't count
	 (because they go with the earlier comment-ender).  */
      if (code == Sendcomment && styles_match_p)
	break;

      /* Assume a defun-start point is outside of strings.  */
      if (code == Sopen
	  && (from == stop || FETCH_CHAR (from - 1) == '\n'))
	break;
    }

  if (comstart_pos == 0)
    from = comment_end;
  /* If the earliest comment starter
     is followed by uniform paired string quotes or none,
     we know it can't be inside a string
     since if it were then the comment ender would be inside one.
     So it does start a comment.  Skip back to it.  */
  else if (comstart_parity == 0 && !string_lossage)
    from = comstart_pos;
  else
    {
      /* We had two kinds of string delimiters mixed up
	 together.  Decode this going forwards.
	 Scan fwd from the previous comment ender
	 to the one in question; this records where we
	 last passed a comment starter.  */

      struct lisp_parse_state state;
      scan_sexps_forward (&state, find_defun_start (comment_end),
			  comment_end - 1, -10000, 0, Qnil, 0);
      if (state.incomment)
	from = state.comstart;
      else
	/* We can't grok this as a comment; scan it normally.  */
	from = comment_end;
    }
  return from;
}

static int
find_end_of_comment (from, stop, mask)
     int from, stop, mask;
{
  int c;
  Lisp_Object table = current_buffer->syntax_table;

  while (1)
    {
      if (from == stop)
	{
	  return -1;
	}
      c = FETCH_CHAR (from);
      if (SYNTAX (table, c) == Sendcomment
	  && SYNTAX_STYLES_MATCH_1CHAR_P (table, c, mask))
	/* we have encountered a comment end of the same style
	   as the comment sequence which began this comment
	   section */
	break;

      from++;
      if (from < stop
	  && SYNTAX_STYLES_MATCH_END_P (table, c, FETCH_CHAR (from), mask))
	/* we have encountered a comment end of the same style
	   as the comment sequence which began this comment
	   section */
	{ from++; break; }
    }
  return from;
}


DEFUN ("forward-comment", Fforward_comment, Sforward_comment, 1, 1, 0,
  "Move forward across up to N comments.  If N is negative, move backward.\n\
Stop scanning if we find something other than a comment or whitespace.\n\
Set point to where scanning stops.\n\
If N comments are found as expected, with nothing except whitespace\n\
between them, return t; otherwise return nil.\n\
Point is set in either case.")
     (n)
     Lisp_Object n;
{
  register int from;
  register int stop;
  register int c;
  register enum syntaxcode code;
  int count;
  Lisp_Object table = current_buffer->syntax_table;

  CHECK_FIXNUM (n, 0);
  count = XINT (n);

  immediate_quit = 1;
  QUIT;

  from = PT;

  while (count > 0)
    {
      stop = ZV;
      while (from < stop)
	{
	  int mask = 0;         /* mask for finding matching comment style */

	  if (char_quoted (from))
	    {
	      from++;
	      continue;
	    }

	  c = FETCH_CHAR (from);
	  code = SYNTAX (table, c);

	  if (code == Scomment)
	    {
	      /* we have encountered a single character comment start
		 sequence, and we are ignoring all text inside comments.
		 we must record the comment style this character begins
		 so that later, only a comment end of the same style actually
		 ends the comment section */
	      mask = SYNTAX_COMMENT_1CHAR_MASK (table, c);
	    }

	  else if (from < stop
		   && SYNTAX_START_P (table, c, FETCH_CHAR (from+1)))
	    {
	      /* we have encountered a 2char comment start sequence and we 
		 are ignoring all text inside comments. we must record
		 the comment style this sequence begins so that later,
		 only a comment end of the same style actually ends
		 the comment section */
	      code = Scomment;
	      mask = SYNTAX_COMMENT_MASK_START (table, c, FETCH_CHAR (from+1));
	      from++;
	    }

	  if (code == Scomment)
	    {
	      int newfrom;

	      newfrom = find_end_of_comment (from, stop, mask);
	      if (newfrom < 0)
		{
		  /* we stopped because from==stop */
		  immediate_quit = 0;
		  SET_PT (stop);
		  return Qnil;
		}
	      from = newfrom;

	      /* We have skipped one comment.  */
	      break;
	    }
	  else if (code != Swhitespace
		   && code != Sendcomment
		   && code != Scomment )
	    {
	      immediate_quit = 0;
	      SET_PT (from);
	      return Qnil;
	    }
	  from++;
	}

      /* End of comment reached */
      count--;
    }

  while (count < 0)
    {
      stop = BEGV;
      while (from > stop)
	{
          int mask = 0;         /* mask for finding matching comment style */

	  from--;
	  if (char_quoted (from))
	    {
	      from--;
	      continue;
	    }
	      
	  c = FETCH_CHAR (from);
	  code = SYNTAX (table, c);

	  if (code == Sendcomment)
	    {
	      /* we have found a single char end comment. we must record
		 the comment style encountered so that later, we can match
		 only the proper comment begin sequence of the same style */
	      mask = SYNTAX_COMMENT_1CHAR_MASK (table, c);
	    }

	  else if (from > stop
		   && SYNTAX_END_P (table, FETCH_CHAR (from - 1), c)
		   && !char_quoted (from - 1))
	    {
	      /* We must record the comment style encountered so that
		 later, we can match only the proper comment begin
		 sequence of the same style.  */
	      code = Sendcomment;
	      mask = SYNTAX_COMMENT_MASK_END (FETCH_CHAR (from - 1), c);
	      from--;
	    }

	  if (code == Sendcomment)
 	    {
 	      from = find_start_of_comment (from, stop, mask);
 	      break;
            }

	  else if (code != Swhitespace
		   && SYNTAX (table, c) != Scomment
		   && SYNTAX (table, c) != Sendcomment)
	    {
	      immediate_quit = 0;
	      SET_PT (from + 1);
	      return Qnil;
	    }
	}

      count++;
    }

  SET_PT (from);
  immediate_quit = 0;
  return Qt;
}


Lisp_Object
scan_lists (from, count, depth, sexpflag)
     register int from;
     int count, depth, sexpflag;
{
  register int stop;
  register int c;
  int quoted;
  int mathexit = 0;
  register enum syntaxcode code;
  int min_depth = depth;    /* Err out if depth gets less than this. */
  Lisp_Object table = current_buffer->syntax_table;

  if (depth > 0) min_depth = 0;

  immediate_quit = 1;
  QUIT;

  while (count > 0)
    {
      stop = ZV;
      while (from < stop)
	{
          int mask = 0;         /* mask for finding matching comment style */

	  c = FETCH_CHAR (from);
	  code = SYNTAX (table, c);
	  from++;

	  /* a 1-char comment start sequence */
	  if (code == Scomment && parse_sexp_ignore_comments)
	    {
	      mask = SYNTAX_COMMENT_1CHAR_MASK (table, c);
	    }

	  /* else, a 2-char comment start sequence? */
	  else if (from < stop
		   && SYNTAX_START_P (table, c, FETCH_CHAR (from))
		   && parse_sexp_ignore_comments)
	    {
	      /* we have encountered a comment start sequence and we 
		 are ignoring all text inside comments. we must record
		 the comment style this sequence begins so that later,
		 only a comment end of the same style actually ends
		 the comment section */
	      code = Scomment;
	      mask = SYNTAX_COMMENT_MASK_START (table, c, FETCH_CHAR (from));
	      from++;
	    }
	  
	  if (SYNTAX_PREFIX (table, c))
	    continue;

	  switch (code)
	    {
	    case Sescape:
	    case Scharquote:
	      if (from == stop) goto lose;
	      from++;
	      /* treat following character as a word constituent */
	    case Sword:
	    case Ssymbol:
	      if (depth || !sexpflag) break;
	      /* This word counts as a sexp; return at end of it. */
	      while (from < stop)
		{
		  switch (SYNTAX (table, FETCH_CHAR (from)))
		    {
		    case Scharquote:
		    case Sescape:
		      from++;
		      if (from == stop) goto lose;
		      break;
		    case Sword:
		    case Ssymbol:
		    case Squote:
		      break;
		    default:
		      goto done;
		    }
		  from++;
		}
	      goto done;

	    case Scomment:
	      if (!parse_sexp_ignore_comments)
		break;
	      {
		int newfrom = find_end_of_comment (from, stop, mask);
		if (newfrom < 0)
		  {
		    /* we stopped because from == stop in search forward */
		    from = stop;
		    goto done;
		  }
		from = newfrom;
	      }
	      break;

	    case Smath:
	      if (!sexpflag)
		break;
	      if (from != stop && c == FETCH_CHAR (from))
		from++;
	      if (mathexit)
		{
		  mathexit = 0;
		  goto close1;
		}
	      mathexit = 1;

	    case Sopen:
	      if (!++depth) goto done;
	      break;

	    case Sclose:
	    close1:
	      if (!--depth) goto done;
	      if (depth < min_depth)
		error ("Containing expression ends prematurely");
	      break;

	    case Sstring:
              {
                int ch = FETCH_CHAR (from - 1);
                unsigned char stringterm = SYNTAX_MATCH (table, ch);
                if (stringterm == 0)
                  stringterm = ch;
              
                while (1)
		{
		  if (from >= stop)
		    goto lose;
		  if (FETCH_CHAR (from) == stringterm)
		    break;
		  switch (SYNTAX (table, FETCH_CHAR (from)))
                  {
                  case Scharquote:
                  case Sescape:
                    from++;
                    break;
                  default:
                    break;
                  }
		  from++;
		}
                from++;
                if (!depth && sexpflag) goto done;
                break;
              }

            default:
              break;
	    }
	}

      /* Reached end of buffer.  Error if within object, return nil if between */
      if (depth) goto lose;

      immediate_quit = 0;
      return Qnil;

      /* End of object reached */
    done:
      count--;
    }

  while (count < 0)
    {
      stop = BEGV;
      while (from > stop)
	{
          int mask = 0;         /* mask for finding matching comment style */

	  from--;
          quoted = char_quoted (from);
	  if (quoted)
	    from--;

	  c = FETCH_CHAR (from);
	  code = SYNTAX (table, c);

	  if (code == Sendcomment && parse_sexp_ignore_comments)
	    {
	      /* we have found a single char end comment. we must record
		 the comment style encountered so that later, we can match
		 only the proper comment begin sequence of the same style */
	      mask = SYNTAX_COMMENT_1CHAR_MASK (table, c);
	    }

	  else if (from > stop
		   && SYNTAX_END_P (table, FETCH_CHAR (from-1), c)
		   && !char_quoted (from - 1)
		   && parse_sexp_ignore_comments)
	    {
	      /* we must record the comment style encountered so that
		 later, we can match only the proper comment begin
		 sequence of the same style */
	      code = Sendcomment;
	      mask = SYNTAX_COMMENT_MASK_END (FETCH_CHAR (from - 1), c);
	      from--;
	    }

	  if (SYNTAX_PREFIX (table, c))
	    continue;

	  switch (((quoted) ? Sword : code))
	    {
	    case Sword:
	    case Ssymbol:
	      if (depth || !sexpflag) break;
	      /* This word counts as a sexp; count object finished after 
		 passing it. */
	      while (from > stop)
		{
		  quoted = char_quoted (from - 1);
		  if (quoted)
		    from--;
		  if (! (quoted
                         || SYNTAX (table, FETCH_CHAR (from - 1)) == Sword
			 || SYNTAX (table, FETCH_CHAR (from - 1)) == Ssymbol
			 || SYNTAX (table, FETCH_CHAR (from - 1)) == Squote))
            	    goto done2;
		  from--;
		}
	      goto done2;

	    case Smath:
	      if (!sexpflag)
		break;
	      if (from != stop && c == FETCH_CHAR (from - 1))
		from--;
	      if (mathexit)
		{
		  mathexit = 0;
		  goto open2;
		}
	      mathexit = 1;

	    case Sclose:
	      if (!++depth) goto done2;
	      break;

	    case Sopen:
	    open2:
	      if (!--depth) goto done2;
	      if (depth < min_depth)
		error ("Containing expression ends prematurely");
	      break;

	    case Sendcomment:
	      if (parse_sexp_ignore_comments)
		from = find_start_of_comment (from, stop, mask);
	      break;

	    case Sstring:
              {
                int ch = FETCH_CHAR (from);
                unsigned char stringterm = SYNTAX_MATCH (table, ch);
                if (stringterm == 0)
                  stringterm = ch;

                while (1)
		{
		  if (from == stop) goto lose;
		  if (!char_quoted (from - 1)
 		      && stringterm == FETCH_CHAR (from - 1))
		    break;
		  from--;
		}
                from--;
                if (!depth && sexpflag) goto done2;
                break;
              }
            }
	}

      /* Reached start of buffer.  Error if within object, return nil if between */
      if (depth) goto lose;

      immediate_quit = 0;
      return Qnil;

    done2:
      count++;
    }


  immediate_quit = 0;
  return (make_number (from));

 lose:
  error ("Unbalanced parentheses");
  /* NOTREACHED */
  return Qnil; /* warning suppression */
}

static int
char_quoted (pos)
     register int pos;
{
  register enum syntaxcode code;
  register int beg = BEGV;
  register int quoted = 0;
  Lisp_Object table = current_buffer->syntax_table;

  while (pos > beg
	 && ((code = SYNTAX (table, FETCH_CHAR (pos - 1))) == Scharquote
	     || code == Sescape))
    pos--, quoted = !quoted;
  return quoted;
}

DEFUN ("scan-lists", Fscan_lists, Sscan_lists, 3, 3, 0,
  "Scan from character number FROM by COUNT lists.\n\
Returns the character number of the position thus found.\n\
\n\
If DEPTH is nonzero, paren depth begins counting from that value,\n\
only places where the depth in parentheses becomes zero\n\
are candidates for stopping; COUNT such places are counted.\n\
Thus, a positive value for DEPTH means go out levels.\n\
\n\
Comments are ignored if `parse-sexp-ignore-comments' is non-nil.\n\
\n\
If the beginning or end of (the accessible part of) the buffer is reached\n\
and the depth is wrong, an error is signaled.\n\
If the depth is right but the count is not used up, nil is returned.")
  (from, count, depth)
     Lisp_Object from, count, depth;
{
  CHECK_FIXNUM (from, 0);
  CHECK_FIXNUM (count, 1);
  CHECK_FIXNUM (depth, 2);

  return scan_lists (XINT (from), XINT (count), XINT (depth), 0);
}

DEFUN ("scan-sexps", Fscan_sexps, Sscan_sexps, 2, 2, 0,
  "Scan from character number FROM by COUNT balanced expressions.\n\
If COUNT is negative, scan backwards.\n\
Returns the character number of the position thus found.\n\
\n\
Comments are ignored if `parse-sexp-ignore-comments' is non-nil.\n\
\n\
If the beginning or end of (the accessible part of) the buffer is reached\n\
in the middle of a parenthetical grouping, an error is signaled.\n\
If the beginning or end is reached between groupings\n\
but before count is used up, nil is returned.")
  (from, count)
     Lisp_Object from, count;
{
  CHECK_FIXNUM (from, 0);
  CHECK_FIXNUM (count, 1);

  return scan_lists (XINT (from), XINT (count), 0, 1);
}

DEFUN ("backward-prefix-chars", Fbackward_prefix_chars, Sbackward_prefix_chars,
  0, 0, 0,
  "Move point backward over any number of chars with prefix syntax.\n\
This includes chars with \"quote\" or \"prefix\" syntax (' or p).")
  ()
{
  int beg = BEGV;
  int pos = PT;
  Lisp_Object table = current_buffer->syntax_table;

  while (pos > beg && !char_quoted (pos - 1)
	 && (SYNTAX (table, FETCH_CHAR (pos - 1)) == Squote
	     || SYNTAX_PREFIX (table, FETCH_CHAR (pos - 1))))
    pos--;

  SET_PT (pos);

  return Qnil;
}

/* Parse forward from FROM to END,
   assuming that FROM has state OLDSTATE (nil means FROM is start of function),
   and return a description of the state of the parse at END.
   If STOPBEFORE is nonzero, stop at the start of an atom.
   If COMMENTSTOP is nonzero, stop at the start of a comment.  */

static void
scan_sexps_forward (struct lisp_parse_state *stateptr,
		    int from, int end, 
		    int targetdepth, int stopbefore,
		    Lisp_Object oldstate,
		    int commentstop)
{
  struct lisp_parse_state state;

  register enum syntaxcode code;
  struct level { int last, prev; };
  struct level levelstart[100];
  register struct level *curlevel = levelstart;
  struct level *endlevel = levelstart + 100;
  register int depth;	/* Paren depth of current scanning location.
			   level - levelstart equals this except
			   when the depth becomes negative.  */
  int mindepth;		/* Lowest DEPTH value seen.  */
  int start_quoted = 0;		/* Nonzero means starting after a char quote */
  Lisp_Object table = current_buffer->syntax_table;
  Lisp_Object tem;
  int mask;				     /* comment mask */

  immediate_quit = 1;
  QUIT;

  if (NILP (oldstate))
    {
      depth = 0;
      state.instring = -1;
      state.incomment = 0;
      state.comstyle = 0;	/* comment style a by default */
      mask = SYNTAX_COMMENT_STYLE_A;
    }
  else
    {
      tem = Fcar (oldstate);    /* elt 0, depth */
      if (!NILP (tem))
	depth = XINT (tem);
      else
	depth = 0;

      oldstate = Fcdr (oldstate);
      oldstate = Fcdr (oldstate);
      oldstate = Fcdr (oldstate);
      tem = Fcar (oldstate);    /* elt 3, instring */
      state.instring = !NILP (tem) ? XINT (tem) : -1;

      oldstate = Fcdr (oldstate); /* elt 4, incomment */
      tem = Fcar (oldstate);
      state.incomment = !NILP (tem);

      oldstate = Fcdr (oldstate);
      tem = Fcar (oldstate);    /* elt 5, follows-quote */
      start_quoted = !NILP (tem);

      /* if the eighth element of the list is nil, we are in comment style
	 a. if it is non-nil, we are in comment style b */
      oldstate = Fcdr (oldstate);
      oldstate = Fcdr (oldstate);
      oldstate = Fcdr (oldstate);
      tem = Fcar (oldstate);    /* elt 8, comment style a */
      state.comstyle = !NILP (tem);
      mask = state.comstyle ? SYNTAX_COMMENT_STYLE_B : SYNTAX_COMMENT_STYLE_A;
    }
  state.quoted = 0;
  mindepth = depth;

  curlevel->prev = -1;
  curlevel->last = -1;

  /* Enter the loop at a place appropriate for initial state. */

  if (state.incomment) goto startincomment;
  if (state.instring >= 0)
    {
      if (start_quoted) goto startquotedinstring;
      goto startinstring;
    }
  if (start_quoted) goto startquoted;

  while (from < end)
    {
      code = SYNTAX (table, FETCH_CHAR (from));
      from++;

      if (code == Scomment)
	{
	  /* record the comment style we have entered so that only the
	     comment-ender sequence (or single char) of the same style
	     actually terminates the comment section. */
	  mask = SYNTAX_COMMENT_1CHAR_MASK (table, FETCH_CHAR (from-1));
	  state.comstyle = (mask == SYNTAX_COMMENT_STYLE_B);
	  state.comstart = from - 1;
	}
      
      else if (from < end &&
	       SYNTAX_START_P (table, FETCH_CHAR (from-1), FETCH_CHAR (from)))
	{
	  /* Record the comment style we have entered so that only
	     the comment-end sequence of the same style actually
	     terminates the comment section.  */
	  code = Scomment;
	  mask = SYNTAX_COMMENT_MASK_START (table,
                                            FETCH_CHAR (from-1),
					    FETCH_CHAR (from));
	  state.comstyle = (mask == SYNTAX_COMMENT_STYLE_B);
	  state.comstart = from-1;
	  from++;
	}

      if (SYNTAX_PREFIX (table, FETCH_CHAR (from - 1)))
	continue;
      switch (code)
	{
	case Sescape:
	case Scharquote:
	  if (stopbefore) goto stop;  /* this arg means stop at sexp start */
	  curlevel->last = from - 1;
	startquoted:
	  if (from == end) goto endquoted;
	  from++;
	  goto symstarted;
	  /* treat following character as a word constituent */
	case Sword:
	case Ssymbol:
	  if (stopbefore) goto stop;  /* this arg means stop at sexp start */
	  curlevel->last = from - 1;
	symstarted:
	  while (from < end)
	    {
	      switch (SYNTAX (table, FETCH_CHAR (from)))
		{
		case Scharquote:
		case Sescape:
		  from++;
		  if (from == end) goto endquoted;
		  break;
		case Sword:
		case Ssymbol:
		case Squote:
		  break;
		default:
		  goto symdone;
		}
	      from++;
	    }
	symdone:
	  curlevel->prev = curlevel->last;
	  break;

	case Scomment:
	  state.incomment = 1;
	startincomment:
	  if (commentstop)
	    goto done;
	  {
	    int newfrom = find_end_of_comment (from, end, mask);
	    if (newfrom < 0)
	      {
		/* we terminated search because from == end */
		from = end;
		goto done;
	      }
	    from = newfrom;
	  }
	  state.incomment = 0;
	  state.comstyle = 0;		     /* reset the comment style */
	  mask = 0;
	  break;

	case Sopen:
	  if (stopbefore) goto stop;  /* this arg means stop at sexp start */
	  depth++;
	  /* curlevel++->last ran into compiler bug on Apollo */
	  curlevel->last = from - 1;
	  if (++curlevel == endlevel)
	    error ("Nesting too deep for parser");
	  curlevel->prev = -1;
	  curlevel->last = -1;
	  if (!--targetdepth) goto done;
	  break;

	case Sclose:
	  depth--;
	  if (depth < mindepth)
	    mindepth = depth;
	  if (curlevel != levelstart)
	    curlevel--;
	  curlevel->prev = curlevel->last;
	  if (!++targetdepth) goto done;
	  break;

	case Sstring:
          {
            int ch;
            if (stopbefore) goto stop; /* this arg means stop at sexp start */
            curlevel->last = from - 1;
            ch = FETCH_CHAR (from - 1);
            state.instring = SYNTAX_MATCH (table, ch);
            if (state.instring == 0) state.instring = ch;
          }
	startinstring:
	  while (1)
	    {
	      if (from >= end) goto done;
	      if (FETCH_CHAR (from) == state.instring) break;
	      switch (SYNTAX (table, FETCH_CHAR (from)))
		{
		case Scharquote:
		case Sescape:
                  {
                    from++;
                  startquotedinstring:
                    if (from >= end) goto endquoted;
                    break;
                  }
                default:
                  break;
		}
	      from++;
	    }
	  state.instring = -1;
	  curlevel->prev = curlevel->last;
	  from++;
	  break;

	case Smath:
	  break;

        case Swhitespace:
        case Spunct:
        case Squote:
        case Sendcomment:
        case Smax:
          break;
	}
    }
  goto done;

 stop:   /* Here if stopping before start of sexp. */
  from--;    /* We have just fetched the char that starts it; */
  goto done; /* but return the position before it. */

 endquoted:
  state.quoted = 1;
 done:
  state.depth = depth;
  state.mindepth = mindepth;
  state.thislevelstart = curlevel->prev;
  state.prevlevelstart
    = (curlevel == levelstart) ? -1 : (curlevel - 1)->last;
  state.location = from;
  immediate_quit = 0;

  *stateptr = state;
}

/* This comment supplies the doc string for parse-partial-sexp,
   for make-docfile to see.  We cannot put this in the real DEFUN
   due to limits in the Unix cpp.

DEFUN ("parse-partial-sexp", Ffoo, Sfoo, 2, 6, 0,
  "Parse Lisp syntax starting at FROM until TO; return status of parse at TO.\n\
Parsing stops at TO or when certain criteria are met;\n\
 point is set to where parsing stops.\n\
If fifth arg STATE is omitted or nil,\n\
 parsing assumes that FROM is the beginning of a function.\n\
Value is a list of eight elements describing final state of parsing:\n\
 0. depth in parens.\n\
 1. character address of start of innermost containing list; nil if none.\n\
 2. character address of start of last complete sexp terminated.\n\
 3. non-nil if inside a string.\n\
    (it is the character that will terminate the string.)\n\
 4. t if inside a comment.\n\
 5. t if following a quote character.\n\
 6. the minimum paren-depth encountered during this scan.\n\
 7. nil if in comment style a, or not in a comment; t if in comment style b\n\
If third arg TARGETDEPTH is non-nil, parsing stops if the depth\n\
in parentheses becomes equal to TARGETDEPTH.\n\
Fourth arg STOPBEFORE non-nil means stop when come to\n\
 any character that starts a sexp.\n\
Fifth arg STATE is an eight-element list like what this function returns.\n\
It is used to initialize the state of the parse.  Its second and third\n\
elements are ignored.\n\
Sixth args COMMENTSTOP non-nil means stop at the start of a comment.")
  (from, to, targetdepth, stopbefore, oldstate, commentstop)
*/

DEFUN ("parse-partial-sexp", Fparse_partial_sexp, Sparse_partial_sexp, 2, 6, 0,
  0 /* See immediately above */)
  (from, to, targetdepth, stopbefore, oldstate, commentstop)
     Lisp_Object from, to, targetdepth, stopbefore, oldstate, commentstop;
{
  struct lisp_parse_state state;
  int target;

  if (!NILP (targetdepth))
    {
      CHECK_FIXNUM (targetdepth, 3);
      target = XINT (targetdepth);
    }
  else
    target = -100000;		/* We won't reach this depth */

  validate_region (&from, &to);
  scan_sexps_forward (&state, XINT (from), XINT (to),
		      target, !NILP (stopbefore), oldstate,
		      !NILP (commentstop));

  SET_PT (state.location);

  {
    /*
     * This junk is necessary because of a bug in SparcWorks cc 2.0.1.  It
     * doesn't handle functions as arguments to other functions very well.
     */
    Lisp_Object retval[8];

    retval[0] = make_number (state.depth);
    retval[1] = ((state.prevlevelstart < 0) ? Qnil : make_number (state.prevlevelstart));
    retval[2] = ((state.thislevelstart < 0) ? Qnil : make_number (state.thislevelstart));
    retval[3] = ((state.instring >= 0) ? make_number (state.instring) : Qnil);
    retval[4] = ((state.incomment) ? Qt : Qnil);
    retval[5] = ((state.quoted) ? Qt : Qnil);
    retval[6] = make_number (state.mindepth);
    retval[7] = ((state.comstyle) ? Qt : Qnil);

    return (Flist (8, retval));
  }
}

void
init_syntax_once ()
{
  struct Lisp_Vector *v;
  int i;

  /* Set this now, so first buffer creation can refer to it. */
  /* Make it nil before calling copy-syntax-table
    so that copy-syntax-table will know not to try to copy from garbage */
  Vstandard_syntax_table = Qnil;
  Vstandard_syntax_table = Fcopy_syntax_table (Qnil);
  staticpro (&Vstandard_syntax_table);

  v = XVECTOR (Vstandard_syntax_table);

  for (i = 'a'; i <= 'z'; i++)
    v->contents[i] = make_number ((int) Sword);
  for (i = 'A'; i <= 'Z'; i++)
    v->contents[i] = make_number ((int) Sword);
  for (i = '0'; i <= '9'; i++)
    v->contents[i] = make_number ((int) Sword);
  v->contents['$'] = make_number ((int) Sword);
  v->contents['%'] = make_number ((int) Sword);

  v->contents['('] = make_number ((int) Sopen + (')' << 8));
  v->contents[')'] = make_number ((int) Sclose + ('(' << 8));
  v->contents['['] = make_number ((int) Sopen + (']' << 8));
  v->contents[']'] = make_number ((int) Sclose + ('[' << 8));
  v->contents['{'] = make_number ((int) Sopen + ('}' << 8));
  v->contents['}'] = make_number ((int) Sclose + ('{' << 8));
  v->contents['"'] = make_number ((int) Sstring);
  v->contents['\\'] = make_number ((int) Sescape);

  {
    CONST char *p;
    for (p = "_-+*/&|<>="; *p; p++)
      v->contents[(int) *p] = make_number ((int) Ssymbol);

    for (p = ".,;:?!#@~^'`"; *p; p++)
      v->contents[(int) *p] = make_number ((int) Spunct);
  }
}

void
syms_of_syntax ()
{
  defsymbol (&Qsyntax_table_p, "syntax-table-p");

  DEFVAR_BOOL ("parse-sexp-ignore-comments", &parse_sexp_ignore_comments,
    "Non-nil means `forward-sexp', etc., should treat comments as whitespace.");

  words_include_escapes = 0;
  DEFVAR_BOOL ("words-include-escapes", &words_include_escapes,
    "Non-nil means `forward-word', etc., should treat escape chars part of words.");

  defsubr (&Ssyntax_table_p);
  defsubr (&Ssyntax_table);
  defsubr (&Sstandard_syntax_table);
  defsubr (&Scopy_syntax_table);
  defsubr (&Sset_syntax_table);
  defsubr (&Schar_syntax);
  /* defsubr (&Smodify_syntax_entry); now in Lisp. */

  defsubr (&Sforward_word);
  defsubr (&Sforward_comment);

  defsubr (&Sscan_lists);
  defsubr (&Sscan_sexps);
  defsubr (&Sbackward_prefix_chars);

  defsubr (&Sparse_partial_sexp);
}
