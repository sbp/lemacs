/* Routines to compute the current syntactic context, for font-lock mode.
   Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.

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

/* This code computes the syntactic context of the current point, that is,
   whether point is within a comment, a string, what have you.  It does
   this by picking a point known to be outside of any syntactic constructs
   and moving forward, examining the syntax of each character.

   Two caches are used: one caches the last point computed, and the other
   caches the last point at the beginning of a line.  This makes there
   be little penalty for moving left-to-right on a line a character at a 
   time; makes starting over on a line be cheap; and makes random-accessing
   within a line relatively cheap.  

   When we move to a different line farther down in the file (but within the
   current top-level form) we simply continue computing forward.  If we move
   backward more than a line, or move beyond the end of the current tlf, or
   switch buffers, then we call `beginning-of-defun' and start over from 
   there.

   The caller must flush the caches when deletions occur; we could probably
   notice that here by using the buffer-mod-time, but we don't right now.
 */

#include "config.h"
#include "intl.h"
#include "lisp.h"
#include "buffer.h"
#include "syntax.h"

Lisp_Object Qcomment;
Lisp_Object Qblock_comment;
Lisp_Object Qbeginning_of_defun;
Lisp_Object Qend_of_defun;      /* not used, but doesn't hurt */
Lisp_Object Vend_of_defun_string;

/* #### KLUDGE KLUDGE KLUDGE */
Lisp_Object Qcplusplus_mode;
Lisp_Object Qcplusplus_beginning_of_defun, Qcplusplus_end_of_defun;

enum syntactic_context {
  context_none, context_string, context_comment, context_block_comment
};

enum block_comment_context {
  ccontext_none, ccontext_start1, ccontext_start2, ccontext_end1
};

enum comment_style {
  comment_style_none, comment_style_a, comment_style_b
};

struct context_cache {
  int start_point;			/* cache location */
  int end_point;			/* next end-of-defun forward */
  struct buffer *buffer;		/* does this need to be staticpro'd? */
  enum syntactic_context context;	/* single-char-syntax state */
  enum block_comment_context ccontext;	/* block-comment state */
  enum comment_style style;		/* which comment group */
  unsigned char scontext;		/* active string delimiter */
  int depth;				/* depth in parens */
  int backslash_p;			/* just read a backslash */
};

static struct context_cache context_cache;
static struct context_cache bol_context_cache;

#define reset_context_cache(cc) memset((cc),0,sizeof (struct context_cache))

static int
beginning_of_defun (int pt)
{
  int opt = PT;
  if (pt == BEGV) return pt;
  SET_PT (pt);
  /* #### COMPLETE and UTTER KLUDGE
     #'beginning-of-defun should run beginning-of-defun-function instead
     of different major modes giving this a different binding.
   */
  if (EQ (current_buffer->major_mode, Qcplusplus_mode))
    call0 (Qcplusplus_beginning_of_defun);
  else
    call0 (Qbeginning_of_defun);
  pt = PT;
  SET_PT (opt);
  return pt;
}

static int
end_of_defun (int pt)
{
  int opt = PT;
  SET_PT (pt);
  /* #### COMPLETE and UTTER KLUDGE
     #'end-of-defun should run end-of-defun-function instead
     of different major modes giving this a different binding.
   */
  if (EQ (current_buffer->major_mode, Qcplusplus_mode))
    call0 (Qcplusplus_end_of_defun);
  else
    /* This is the same thing that end-of-defun does in all modes but C++ */
    Fre_search_forward (Vend_of_defun_string, Qnil, Qlambda, Qnil);
  pt = PT;
  SET_PT (opt);
  return pt;
}


static void
find_context_start (int pt, int end)
{
  int do_bod = (pt > context_cache.end_point ||
		current_buffer != context_cache.buffer);

  if (! (do_bod || pt < context_cache.start_point))
    return;

  if (do_bod || pt < bol_context_cache.start_point)
    {
      /* We must start searching at the beginning of defun.
       */
      pt = beginning_of_defun (pt);
      context_cache.start_point = pt;
      context_cache.buffer = current_buffer;
      context_cache.context = context_none;
      context_cache.ccontext = ccontext_none;
      context_cache.style = comment_style_none;
      context_cache.scontext = '\000';
      context_cache.depth = 0;
      context_cache.backslash_p = ((pt > 1) && (CHAR_AT (pt - 1) == '\\'));
      if (end)
	{
	  if (end < pt) abort ();
	  context_cache.end_point = end;
	}
      else
	context_cache.end_point = end_of_defun (pt);
    }
  else
    {
      /* We can start searching at the beginning of the current line. */
      context_cache = bol_context_cache;
    }
}


/* compatibility with new macros:  4-May-1993 baw */
# define SYNTAX_SINGLE_CHAR_STYLE_A(table, c) \
      (SYNTAX_STYLES_MATCH_1CHAR_P((table), (c), SYNTAX_COMMENT_STYLE_A))
# define SYNTAX_SINGLE_CHAR_STYLE_B(table, c) \
      (SYNTAX_STYLES_MATCH_1CHAR_P((table), (c), SYNTAX_COMMENT_STYLE_B))
# define SYNTAX_START(table, c1, c2) (SYNTAX_START_P((table),(c1),(c2)))
# define SYNTAX_END(table, c1, c2) (SYNTAX_END_P((table),(c1),(c2)))
# define SYNTAX_START_SEQUENCE(table, c1, c2, mask) \
      (SYNTAX_STYLES_MATCH_START_P((table), (c1), (c2), (mask)))
# define SYNTAX_END_SEQUENCE(table, c1, c2, mask) \
      (SYNTAX_STYLES_MATCH_END_P((table), (c1), (c2), (mask)))


# define FIRST_CHAR_START_OF_A_OR_B(table, c) \
      (SYNTAX_COMMENT_BITS ((table), (c)) & SYNTAX_FIRST_CHAR_START)
# define SECOND_CHAR_START_OF_A_OR_B(table, c) \
      (SYNTAX_COMMENT_BITS ((table), (c)) & SYNTAX_SECOND_CHAR_START)
# define FIRST_CHAR_END_OF_A_OR_B(table, c) \
      (SYNTAX_COMMENT_BITS ((table), (c)) & SYNTAX_FIRST_CHAR_END)
# define SECOND_CHAR_END_OF_A_OR_B(table, c) \
      (SYNTAX_COMMENT_BITS ((table), (c)) & SYNTAX_SECOND_CHAR_END)
# define START_STYLE_A_P(table, c1, c2) \
      SYNTAX_START_SEQUENCE ((table), (c1), (c2), SYNTAX_COMMENT_STYLE_A)
# define START_STYLE_B_P(table, c1, c2) \
      SYNTAX_START_SEQUENCE ((table), (c1), (c2), SYNTAX_COMMENT_STYLE_B)
# define END_STYLE_A_P(table, c1, c2) \
      SYNTAX_END_SEQUENCE ((table), (c1), (c2), SYNTAX_COMMENT_STYLE_A)
# define END_STYLE_B_P(table, c1, c2) \
      SYNTAX_END_SEQUENCE ((table), (c1), (c2), SYNTAX_COMMENT_STYLE_B)

# define SYNTAX_START_STYLE(table, c1, c2) \
      (START_STYLE_A_P((table), (c1),(c2)) \
        ? comment_style_a \
        : (START_STYLE_B_P((table), (c1),(c2)) \
            ? comment_style_b : comment_style_none))
# define SYNTAX_END_STYLE(table, c1, c2) \
      (END_STYLE_A_P((table), (c1),(c2)) \
        ? comment_style_a \
        : (END_STYLE_B_P((table), (c1),(c2)) \
            ? comment_style_b : comment_style_none))
# define SINGLE_SYNTAX_STYLE(table, c) \
      (SYNTAX_SINGLE_CHAR_STYLE_A((table), (c)) ? comment_style_a : \
       SYNTAX_SINGLE_CHAR_STYLE_B((table), (c)) ? comment_style_b : \
       comment_style_none)

static void
find_context (int pt, int end)
{
  Lisp_Object syntax_table = current_buffer->syntax_table;
#ifdef I18N4
  wchar_t prev_c, c;
#else
  unsigned char prev_c, c;
#endif
  int target = pt;
  if (end == 0)
    find_context_start (pt, end);
  pt = context_cache.start_point;

  if (pt > BEGV)
    c = CHAR_AT (pt - 1);
  else
    c = '\n'; /* to get bol_context_cache at point-min */

  for (; pt < target; pt++, context_cache.start_point = pt)
    {
      prev_c = c;
      c = CHAR_AT (pt);

      if (prev_c == '\n')
	bol_context_cache = context_cache;

      if (context_cache.backslash_p)
	{
	  context_cache.backslash_p = 0;
	  continue;
	}

      switch (SYNTAX (syntax_table, c))
	{
	case Sescape:
	  context_cache.backslash_p = 1;
	  break;

	case Sopen:
	  if (context_cache.context == context_none)
	    context_cache.depth++;
	  break;

	case Sclose:
	  if (context_cache.context == context_none)
	    context_cache.depth--;
	  break;

	case Scomment:
	  if (context_cache.context == context_none)
	    {
	      context_cache.context = context_comment;
	      context_cache.ccontext = ccontext_none;
	      context_cache.style = SINGLE_SYNTAX_STYLE (syntax_table, c);
	      if (context_cache.style == comment_style_none) abort ();
	    }
	  break;

	case Sendcomment:
	  if (context_cache.style != SINGLE_SYNTAX_STYLE (syntax_table, c))
	    ;
	  else
	       if (context_cache.context == context_comment)
	    {
	      context_cache.context = context_none;
	      context_cache.style = comment_style_none;
	    }
	  else if (context_cache.context == context_block_comment &&
		   (context_cache.ccontext == ccontext_start2 ||
		    context_cache.ccontext == ccontext_end1))
	    {
	      context_cache.context = context_none;
	      context_cache.ccontext = ccontext_none;
	      context_cache.style = comment_style_none;
	    }
	  break;

	case Sstring:
          {
            if (context_cache.context == context_string &&
                context_cache.scontext == c)
	    {
	      context_cache.context = context_none;
	      context_cache.scontext = '\000';
	    }
            else if (context_cache.context == context_none)
	    {
              unsigned char stringterm = SYNTAX_MATCH (syntax_table, c);
              if (stringterm == 0) stringterm = c;
	      context_cache.context = context_string;
	      context_cache.scontext = stringterm;
	      context_cache.ccontext = ccontext_none;
	    }
            break;
          }
	default:
	  ;
	}


      /* That takes care of the characters with manifest syntax.
	 Now we've got to hack multi-char sequences that start
	 and end block comments.
       */
      if (SECOND_CHAR_START_OF_A_OR_B (syntax_table, c) &&
	  context_cache.context == context_none &&
	  context_cache.ccontext == ccontext_start1 &&
	  SYNTAX_START (syntax_table, prev_c, c) /* the two chars match */
	  )
	{
	  context_cache.ccontext = ccontext_start2;
	  context_cache.style = SYNTAX_START_STYLE (syntax_table, prev_c, c);
	  if (context_cache.style == comment_style_none) abort ();
	}
      else if (FIRST_CHAR_START_OF_A_OR_B (syntax_table, c) &&
	       context_cache.context == context_none &&
	       (context_cache.ccontext == ccontext_none ||
		context_cache.ccontext == ccontext_start1))
	{
	  context_cache.ccontext = ccontext_start1;
	  context_cache.style = comment_style_none; /* should be this already*/
	}
      else if (SECOND_CHAR_END_OF_A_OR_B (syntax_table, c) &&
	       context_cache.context == context_block_comment &&
	       context_cache.ccontext == ccontext_end1 &&
	       SYNTAX_END (syntax_table, prev_c, c) && /* the two chars match */
	       context_cache.style == SYNTAX_END_STYLE (syntax_table, prev_c, c)
	       )
	{
	  context_cache.context = context_none;
	  context_cache.ccontext = ccontext_none;
	  context_cache.style = comment_style_none;
	}
      else if (FIRST_CHAR_END_OF_A_OR_B (syntax_table, c) &&
	       context_cache.context == context_block_comment &&
	       (context_cache.style == SYNTAX_END_STYLE (syntax_table, c, CHAR_AT (pt+1))) &&
	       (context_cache.ccontext == ccontext_start2 ||
		context_cache.ccontext == ccontext_end1))
	/* #### is it right to check for end1 here?? */
	{
	  if (context_cache.style == comment_style_none) abort ();
	  context_cache.ccontext = ccontext_end1;
	}

      else if (context_cache.ccontext == ccontext_start1)
	{
	  if (context_cache.context != context_none) abort ();
	  context_cache.ccontext = ccontext_none;
	}
      else if (context_cache.ccontext == ccontext_end1)
	{
	  if (context_cache.context != context_block_comment) abort ();
	  context_cache.context = context_none;
	  context_cache.ccontext = ccontext_start2;
	}

      if (context_cache.ccontext == ccontext_start2 &&
	  context_cache.context == context_none)
	{
	  context_cache.context = context_block_comment;
	  if (context_cache.style == comment_style_none) abort ();
	}
      else if (context_cache.ccontext == ccontext_none &&
	       context_cache.context == context_block_comment)
	{
	  context_cache.context = context_none;
	}
    }
}


DEFUN ("buffer-syntactic-context-flush-cache",
       Fbuffer_syntactic_context_flush_cache,
       Sbuffer_syntactic_context_flush_cache, 0, 0, 0,
       "Flush the cache used by `buffer-syntactic-context-flush-cache'.\n\
Call this when deletions occur.  This is a kludge.")
	()
{
  reset_context_cache (&context_cache);
  reset_context_cache (&bol_context_cache);
  return Qnil;
}


static Lisp_Object
context_to_symbol (enum syntactic_context context)
{
  switch (context)
    {
    case context_none:		return (Qnil);
    case context_string:	return (Qstring);
    case context_comment:	return (Qcomment);
    case context_block_comment:	return (Qblock_comment);
    default: abort ();
    }
}

DEFUN ("buffer-syntactic-context", Fbuffer_syntactic_context,
       Sbuffer_syntactic_context, 0, 0, 0,
       "Returns the syntactic context of the current buffer at point.\n\
The returned value is one of the following symbols:\n\
\n\
	nil		; meaning no special interpretation\n\
	string		; meaning point is within a string\n\
	comment		; meaning point is within a line comment\n\
	block-comment	; meaning point is within a block comment\n\
\n\
See also the function `buffer-syntactic-context-depth', which returns\n\
the current nesting-depth within all parenthesis-syntax delimiters\n\
and the function `syntactically-sectionize', which will map a function\n\
over each syntactic context in a region.\n\
\n\
Warning, this may alter match-data.")
	()
{
  find_context (PT, 0);
  return context_to_symbol (context_cache.context);
}

DEFUN ("buffer-syntactic-context-depth", Fbuffer_syntactic_context_depth,
       Sbuffer_syntactic_context_depth, 0, 0, 0,
   "Returns the depth within all parenthesis-syntax delimiters at point.\n\
Warning, this may alter match-data.")
     ()
{
  find_context (PT, 0);
  return make_number (context_cache.depth);
}


DEFUN ("syntactically-sectionize", Fsyntactically_sectionize,
       Ssyntactically_sectionize, 3, 4, 0,
       "Creates extents for each contiguous syntactic context in the region.\n\
Calls the given function when each extent is created with three arguments:\n\
the extent, a symbol representing the syntactic context, and the current\n\
depth (as returned by the functions `buffer-syntactic-context' and\n\
`buffer-syntactic-context-depth').  If the optional arg `properties' is\n\
provided, it is a plist of properties and values to set on the new extents.\n\
\n\
Warning, this may alter match-data.")
	(start, end, function, properties)
	Lisp_Object start, end, function, properties;
{
  int pt, e, edepth;
  enum syntactic_context this_context;
  Lisp_Object extent = Qnil;
  struct gcpro gcpro1;

  CHECK_LIST (properties, 0);
  CHECK_FIXNUM_COERCE_MARKER (start, 0);
  CHECK_FIXNUM_COERCE_MARKER (end, 0);
  pt = XINT (start);
  e = XINT (end);

  /* Only call find_context_start() once; passing the `end' arg
     to find_context() means that it won't call it each time. */
  find_context_start (pt, e);
  find_context (pt, e);

  GCPRO1 (extent);
  while (pt < e)
    {
      int estart, eend;
      /* skip over "blank" areas, and bug out at end-of-buffer. */
      while (context_cache.context == context_none)
	{
	  pt++;
	  if (pt >= e) goto DONE;
	  find_context (pt, e);
	}
      /* We've found a non-blank area; keep going until we reach its end */
      this_context = context_cache.context;
      estart = pt;

      /* Minor kludge: consider the comment-start character(s) a part of
	 the comment.
       */
      if (this_context == context_block_comment &&
	  context_cache.ccontext == ccontext_start2)
	estart -= 2;
      else if (this_context == context_comment)
	estart -= 1;

      edepth = context_cache.depth;
      while (context_cache.context == this_context && pt < e)
	{
	  pt++;
	  find_context (pt, e);
	}

      eend = pt;

      /* Minor kludge: consider the character which terminated the comment
	 a part of the comment.
       */
      if ((this_context == context_block_comment ||
	   this_context == context_comment)
	  && pt < e)
	eend++;

      if (estart == eend)
	continue;
      /* Now make an extent for it. */
      extent = Fmake_extent (make_number (estart),
			     make_number (eend == e ? e : eend - 1),
			     Fcurrent_buffer ());
      {
	Lisp_Object rest;
	for (rest = properties; !NILP (rest); rest = Fcdr (Fcdr (rest)))
	  Fset_extent_property (extent,
				XCONS (rest)->car,
				Fcar (XCONS (rest)->cdr));
      }
      call3 (function, extent, context_to_symbol (this_context),
	     make_number (edepth));
    }
 DONE:
  UNGCPRO;
  return Qnil;
}

void
syms_of_font_lock ()
{
  defsymbol (&Qcomment, "comment");
  defsymbol (&Qblock_comment, "block-comment");
  defsymbol (&Qbeginning_of_defun, "beginning-of-defun");
  defsymbol (&Qend_of_defun, "end-of-defun");

  Vend_of_defun_string = Fpurecopy (build_string ("\n\\s("));
  staticpro (&Vend_of_defun_string);

  /* #### KLUDGE KLUDGE KLUDGE */
  defsymbol (&Qcplusplus_mode, "c++-mode");
  defsymbol (&Qcplusplus_beginning_of_defun, "c++-beginning-of-defun");
  defsymbol (&Qcplusplus_end_of_defun, "c++-end-of-defun");

  memset (&context_cache, 0, sizeof (context_cache));
  memset (&bol_context_cache, 0, sizeof (bol_context_cache));

  defsubr (&Sbuffer_syntactic_context);
  defsubr (&Sbuffer_syntactic_context_depth);
  defsubr (&Sbuffer_syntactic_context_flush_cache);
  defsubr (&Ssyntactically_sectionize);
}
