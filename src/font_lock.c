/* Routines to compute the current syntactic context, for font-lock mode.
   Copyright (C) 1992-1993 Free Software Foundation, Inc.

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
#include "lisp.h"
#include "buffer.h"
#include "syntax.h"

enum syntactic_context {
  context_none, context_string, context_comment, context_block_comment
};

enum block_comment_context {
  ccontext_none, ccontext_start1, ccontext_start2, ccontext_end1
};

#ifdef NEW_SYNTAX
enum comment_style {
  comment_style_none, comment_style_a, comment_style_b
};
#endif

struct context_cache {
  int start_point;			/* cache location */
  int end_point;			/* next end-of-defun forward */
  struct buffer *buffer;		/* does this need to be staticpro'd? */
  enum syntactic_context context;	/* single-char-syntax state */
  enum block_comment_context ccontext;	/* block-comment state */
#ifdef NEW_SYNTAX
  enum comment_style style;		/* which comment group */
#endif
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
  call0 (intern ("beginning-of-defun"));
  pt = PT;
  SET_PT (opt);
  return pt;
}

extern Lisp_Object Fre_search_forward (Lisp_Object, Lisp_Object,
				       Lisp_Object, Lisp_Object);

static int
end_of_defun (int pt)
{
  int opt = PT;
  SET_PT (pt);
  /* ## superfluous consing */
  Fre_search_forward (build_string ("\n\\s("), Qnil, Qlambda, Qnil);
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
#ifdef NEW_SYNTAX
      context_cache.style = comment_style_none;
#endif
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


#ifdef NEW_SYNTAX
# define FIRST_CHAR_START_OF_A_OR_B(c) \
      (SYNTAX_COMMENT_BITS ((c)) & SYNTAX_FIRST_CHAR_START)
# define SECOND_CHAR_START_OF_A_OR_B(c) \
      (SYNTAX_COMMENT_BITS ((c)) & SYNTAX_SECOND_CHAR_START)
# define FIRST_CHAR_END_OF_A_OR_B(c) \
      (SYNTAX_COMMENT_BITS ((c)) & SYNTAX_FIRST_CHAR_END)
# define SECOND_CHAR_END_OF_A_OR_B(c) \
      (SYNTAX_COMMENT_BITS ((c)) & SYNTAX_SECOND_CHAR_END)
# define START_STYLE_A_P(c1, c2) \
      SYNTAX_START_SEQUENCE ((c1), (c2), SYNTAX_COMMENT_STYLE_A)
# define START_STYLE_B_P(c1, c2) \
      SYNTAX_START_SEQUENCE ((c1), (c2), SYNTAX_COMMENT_STYLE_B)
# define END_STYLE_A_P(c1, c2) \
      SYNTAX_END_SEQUENCE ((c1), (c2), SYNTAX_COMMENT_STYLE_A)
# define END_STYLE_B_P(c1, c2) \
      SYNTAX_END_SEQUENCE ((c1), (c2), SYNTAX_COMMENT_STYLE_B)

# define SYNTAX_START_STYLE(c1, c2) \
      (START_STYLE_A_P((c1),(c2)) ? comment_style_a : \
       (START_STYLE_B_P((c1),(c2)) ? comment_style_b : comment_style_none))
# define SYNTAX_END_STYLE(c1, c2) \
      (END_STYLE_A_P((c1),(c2)) ? comment_style_a : \
       (END_STYLE_B_P((c1),(c2)) ? comment_style_b : comment_style_none))
# define SINGLE_SYNTAX_STYLE(c) \
      (SYNTAX_SINGLE_CHAR_STYLE_A((c)) ? comment_style_a : \
       SYNTAX_SINGLE_CHAR_STYLE_B((c)) ? comment_style_b : \
       comment_style_none)
#endif /* NEW_SYNTAX */

static void
find_context (int pt, int end)
{
  unsigned char prev_c, c;
  int target = pt;
  if (end == 0)
    find_context_start (pt, end);
  pt = context_cache.start_point;

  if (pt > BEGV)
    c = CHAR_AT (pt - 1);
  else
    c = 0;

  if (pt == BEGV || c == '\n')
    bol_context_cache = context_cache;

  for (; pt < target; pt++, context_cache.start_point = pt)
    {
      if (context_cache.backslash_p)
	{
	  context_cache.backslash_p = 0;
	  continue;
	}

      prev_c = c;
      c = CHAR_AT (pt);

      switch (SYNTAX (c))
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
#ifdef NEW_SYNTAX
	      context_cache.style = SINGLE_SYNTAX_STYLE (c);
	      if (context_cache.style == comment_style_none) abort ();
#endif
	    }
	  break;

	case Sendcomment:
#ifdef NEW_SYNTAX
	  if (context_cache.style != SINGLE_SYNTAX_STYLE (c))
	    ;
	  else
#endif
	       if (context_cache.context == context_comment)
	    {
	      context_cache.context = context_none;
#ifdef NEW_SYNTAX
	      context_cache.style = comment_style_none;
#endif
	    }
	  else if (context_cache.context == context_block_comment &&
		   (context_cache.ccontext == ccontext_start2 ||
		    context_cache.ccontext == ccontext_end1))
	    {
	      context_cache.context = context_none;
	      context_cache.ccontext = ccontext_none;
#ifdef NEW_SYNTAX
	      context_cache.style = comment_style_none;
#endif
	    }
	  break;

	case Sstring:
	  if (context_cache.context == context_string &&
	      context_cache.scontext == c)
	    {
	      context_cache.context = context_none;
	      context_cache.scontext = '\000';
	    }
	  else if (context_cache.context == context_none)
	    {
	      context_cache.context = context_string;
	      context_cache.scontext = c;
	      context_cache.ccontext = ccontext_none;
	    }
	  break;
	default:
	  ;
	}


      /* That takes care of the characters with manifest syntax.
	 Now we've got to hack multi-char sequences that start
	 and end block comments.
       */
#ifdef NEW_SYNTAX
      if (SECOND_CHAR_START_OF_A_OR_B (c) &&
	  context_cache.context == context_none &&
	  context_cache.ccontext == ccontext_start1 &&
	  SYNTAX_START (prev_c, c) /* the two chars match */
	  )
	{
	  context_cache.ccontext = ccontext_start2;
	  context_cache.style = SYNTAX_START_STYLE (prev_c, c);
	  if (context_cache.style == comment_style_none) abort ();
	}
      else if (FIRST_CHAR_START_OF_A_OR_B (c) &&
	       context_cache.context == context_none &&
	       (context_cache.ccontext == ccontext_none ||
		context_cache.ccontext == ccontext_start1))
	{
	  context_cache.ccontext = ccontext_start1;
	  context_cache.style = comment_style_none; /* should be this already*/
	}
      else if (SECOND_CHAR_END_OF_A_OR_B (c) &&
	       context_cache.context == context_block_comment &&
	       context_cache.ccontext == ccontext_end1 &&
	       SYNTAX_END (prev_c, c) && /* the two chars match */
	       context_cache.style == SYNTAX_END_STYLE (prev_c, c)
	       )
	{
	  context_cache.context = context_none;
	  context_cache.ccontext = ccontext_none;
	  context_cache.style = comment_style_none;
	}
      else if (FIRST_CHAR_END_OF_A_OR_B (c) &&
	       context_cache.context == context_block_comment &&
	       (context_cache.style == SYNTAX_END_STYLE (c, CHAR_AT (pt+1))) &&
	       (context_cache.ccontext == ccontext_start2 ||
		context_cache.ccontext == ccontext_end1))
	/* #### is it right to check for end1 here?? */
	{
	  if (context_cache.style == comment_style_none) abort ();
	  context_cache.ccontext = ccontext_end1;
	}
#else /* !NEW_SYNTAX */

      if (SYNTAX_COMSTART_SECOND (c) &&
	  context_cache.context == context_none &&
	  context_cache.ccontext == ccontext_start1)
	{
	  context_cache.ccontext = ccontext_start2;
/*	  context_cache.style = SYNTAX_COMMENT_STYLE (c); */
	}
      else if (SYNTAX_COMSTART_FIRST (c) &&
	       context_cache.context == context_none &&
	       (context_cache.ccontext == ccontext_none ||
		context_cache.ccontext == ccontext_start1))
	{
	  context_cache.ccontext = ccontext_start1;
	}
      else if (SYNTAX_COMEND_SECOND (c) &&
	       context_cache.context == context_block_comment &&
	       context_cache.ccontext == ccontext_end1)
	{
	  context_cache.context = context_none;
	  context_cache.ccontext = ccontext_none;
	}
      else if (SYNTAX_COMEND_FIRST (c) &&
/*	       context_cache.style == SYNTAX_COMMENT_STYLE (c) && */
	       context_cache.context == context_block_comment &&
	       (context_cache.ccontext == ccontext_start2 ||
		context_cache.ccontext == ccontext_end1))
	/* #### is it right to check for end1 here?? */
	{
	  context_cache.ccontext = ccontext_end1;
	}
#endif /* NEW_SYNTAX */

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
#ifdef NEW_SYNTAX
	  if (context_cache.style == comment_style_none) abort ();
#endif
	}
      else if (context_cache.ccontext == ccontext_none &&
	       context_cache.context == context_block_comment)
	{
	  context_cache.context = context_none;
	}

      if (prev_c == '\n')
	bol_context_cache = context_cache;
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
    case context_none:		return Qnil;
    /* ## superfluous interning */
    case context_string:	return intern ("string");
    case context_comment:	return intern ("comment");
    case context_block_comment:	return intern ("block-comment");
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
`buffer-syntactic-context-depth').  If the optional arg `extent-data' is\n\
provided, the extent will be created with that in its data slot.\n\
\n\
Warning, this may alter match-data.")
	(start, end, function, extent_data)
	Lisp_Object start, end, function, extent_data;
{
  int pt, estart, edepth;
  enum syntactic_context this_context;
  Lisp_Object extent = Qnil;
  struct gcpro gcpro1;

  CHECK_FIXNUM_COERCE_MARKER (start, 0);
  CHECK_FIXNUM_COERCE_MARKER (end, 0);
  start = XINT (start);
  end = XINT (end);
  pt = start;

  /* Only call find_context_start() once; passing the `end' arg
     to find_context() means that it won't call it each time. */
  find_context_start (pt, end);
  find_context (pt, end);

  GCPRO1 (extent);
  while (pt < end)
    {
      /* skip over "blank" areas, and bug out at end-of-buffer. */
      while (context_cache.context == context_none)
	{
	  pt++;
	  if (pt >= end) goto DONE;
	  find_context (pt, end);
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
      while (context_cache.context == this_context && pt < end)
	{
	  pt++;
	  find_context (pt, end);
	}

      /* Minor kludge: consider the character which terminated the comment
	 a part of the comment.
       */
      if ((this_context == context_block_comment ||
	   this_context == context_comment)
	  && pt < end)
	pt++;

      if (estart == pt)
	continue;
      /* Now make an extent for it. */
      extent = Fmake_extent (estart, pt == end ? end : pt - 1,
			     Fcurrent_buffer ());
      if (!NILP (extent_data))
	Fset_extent_data (extent, extent_data);
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
  memset (&context_cache, 0, sizeof (context_cache));
  memset (&bol_context_cache, 0, sizeof (bol_context_cache));
  defsubr (&Sbuffer_syntactic_context);
  defsubr (&Sbuffer_syntactic_context_depth);
  defsubr (&Sbuffer_syntactic_context_flush_cache);
  defsubr (&Ssyntactically_sectionize);
}
