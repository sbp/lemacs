/* String search routines for GNU Emacs.
   Copyright (C) 1985, 1986, 1987, 1992, 1993, 1994
   Free Software Foundation, Inc.

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
#include "intl.h"
#include "lisp.h"
#include "syntax.h"
#include "buffer.h"
#include "insdel.h"
#include "commands.h"

#include <sys/types.h>
#include "regex.h"

#define max(a, b) ((a) > (b) ? (a) : (b))
#define min(a, b) ((a) < (b) ? (a) : (b))


#ifdef I18N4


/* search_assoc_array -- Search associative array for a pair matching c.
*/
static wchar_int_pair *
search_assoc_array (assoc_array *array, wchar_t c)
{
  register int i;
  register wchar_int_pair *p;

  for (i = 0, p = array->data; i < array->in_use; i++, p++)
    if (p->wchar == c)
      return p;
  return NULL;
}

/* add_assoc_entry -- Add pair to associative array.
*/
static void
set_assoc_entry (assoc_array *array, wchar_t c, int num)
{
  wchar_int_pair *p = search_assoc_array (array, c);

  if (!p) {
    if (ARRAY_FULL (array))
      GROW_ASSOC_ARRAY (array, 32);
    p = &(array->data[array->in_use++]);
    p->wchar = c;
  }
  p->value = num;
}


static int
assoc_array_lookup (assoc_array *array, wchar_t c)
{
  wchar_int_pair *p = search_assoc_array (array, c);

  if (p)
    return p->value;
  else
    return array->default_value;
}

/* empty_set_of_chars -- Make set empty with room to grow.
*/
static void
empty_set_of_chars (set_of_chars *set)
{
  if (!set->data)
    GROW_SET_OF_CHARS (set, 64);
  set->data[0] = '\0';
  set->in_use = 1;
  set->complement = 0;
}


/* add_to_set_of_chars -- Add c to set.  (Doesn't do anything with complement flag.)
*/
static void
add_to_set_of_chars (set_of_chars *set, wchar_t c)
{
  if (IN_SET_OF_CHARS_RAW (set, c))
    return;

  if (!set->data)
    empty_set_of_chars (set);
  if ARRAY_FULL (set)
    GROW_SET_OF_CHARS (set, 32);

  set->data[set->in_use - 1] = c;
  set->data[set->in_use++] = '\0';
}


/* Test c for membership in set.
*/
static int
in_set_of_chars (set_of_chars *set, wchar_t c)
{
  int result;

  if (set->anychar)
    return 1;
  result = (wschr (set->data, c) ? 1 : 0);
  if (set->complement)
    result = !result;
  return result;
}
#endif /* I18N4 */


/* We compile regexps into this buffer and then use it for searching. */

struct re_pattern_buffer searchbuf;

#ifdef I18N4
set_of_chars search_fastmap = EMPTY_SET_OF_CHARS;
#else
static char search_fastmap[0400];
#endif

/* Last regexp we compiled */
static Lisp_Object last_regexp;

/* Every call to re_match, etc., must pass &search_regs as the regs argument
   unless you can show it is unnecessary (i.e., if re_match is certainly going
   to be called again before region-around-match can be called).  

   Since the registers are now dynamically allocated, we need to make
   sure not to refer to the Nth register before checking that it has
   been allocated by checking search_regs.num_regs.

   The regex code keeps track of whether it has allocated the search
   buffer using bits in searchbuf.  This means that whenever you
   compile a new pattern, it completely forgets whether it has
   allocated any registers, and will allocate new registers the next
   time you call a searching or matching function.  Therefore, we need
   to call re_set_registers after compiling a new pattern or after
   setting the match registers, so that the regex functions will be
   able to free or re-allocate it properly.  */
static struct re_registers search_regs;
#ifdef NEW_SYNTAX
# define SEARCH_NREGS(x) (x)->num_regs
#else
# define SEARCH_NREGS(x) RE_NREGS
#endif


/* The buffer in which the last search was performed, or
   Qt if the last search was done in a string;
   Qnil if no searching has been done yet.  */
static Lisp_Object last_thing_searched;

/* error condition signalled when regexp compile_pattern fails */
Lisp_Object Qinvalid_regexp;

static void set_search_regs (int, int);

static void
matcher_overflow ()
{
#if 0 /* This is too much of a compatibility problem. */ /* >>> GAG! */
  error (GETTEXT ("Stack overflow in regexp matcher"));
#endif
}

/* Compile a regexp and signal a Lisp error if anything goes wrong.  */
void
compile_pattern (pattern, bufp, regp, translate)
     Lisp_Object pattern;
     struct re_pattern_buffer *bufp;
     struct re_registers *regp;
     char *translate;
{
  CONST char *val;

  if (EQ (pattern, last_regexp)
      && translate == bufp->translate)
    return;
  last_regexp = Qnil;
  bufp->translate = translate;
#ifdef I18N4
  safe_mbstowcs ((char *) XSTRING (pattern)->data, &wc_buf);
  val = re_compile_pattern (wc_buf.data, wc_buf.in_use, bufp);
#else
  val = re_compile_pattern ((char *) XSTRING (pattern)->data,
			    string_length (XSTRING (pattern)),
			    bufp);
#endif
  if (val)
    {
      signal_error (Qinvalid_regexp, list1 (build_string (val)));
    }
  last_regexp = pattern;
#ifdef NEW_REGEX
  /* Advise the searching functions about the space we have allocated
     for register data.  */
  if (regp)
    re_set_registers (bufp, regp, regp->num_regs, regp->start, regp->end);
#endif
  return;
}

/* Error condition used for failing searches */
Lisp_Object Qsearch_failed;


DEFUN ("looking-at", Flooking_at, Slooking_at, 1, 1, 0,
  "Return t if text after point matches regular expression PAT.\n\
This function modifies the match data that `match-beginning',\n\
`match-end' and `match-data' access; save and restore the match\n\
data if you want to preserve them.")
  (string)
     Lisp_Object string;
{
  Lisp_Object val;
#ifdef I18N4
  wchar_t *p1, *p2;
#else
  unsigned char *p1, *p2;
#endif
  int s1, s2;
  register int i;

  CHECK_STRING (string, 0);
  compile_pattern (string, &searchbuf, &search_regs,
		   (!NILP (current_buffer->case_fold_search)
		    ? (char *) DOWNCASE_TABLE
                    : 0));

  immediate_quit = 1;
  QUIT;			/* Do a pending quit right away, to avoid paradoxical behavior */

  /* Get pointers and sizes of the two strings
     that make up the visible portion of the buffer. */

  p1 = BEGV_ADDR;
  s1 = GPT - BEGV;
  p2 = GAP_END_ADDR;
  s2 = ZV - GPT;
  if (s1 < 0)
    {
      p2 = p1;
      s2 = ZV - BEGV;
      s1 = 0;
    }
  if (s2 < 0)
    {
      s1 = ZV - BEGV;
      s2 = 0;
    }
  
  i = re_match_2 (&searchbuf, (char *) p1, s1, (char *) p2, s2,
		  PT - BEGV, &search_regs,
		  ZV - BEGV);
  if (i == -2)
    matcher_overflow ();

  val = (0 <= i ? Qt : Qnil);
  for (i = 0; i < SEARCH_NREGS (&search_regs); i++)
    if (search_regs.start[i] >= 0)
      {
	search_regs.start[i] += BEGV;
	search_regs.end[i] += BEGV;
      }
  XSETR (last_thing_searched, Lisp_Buffer, current_buffer);
  immediate_quit = 0;
  return val;
}

DEFUN ("string-match", Fstring_match, Sstring_match, 2, 3, 0,
  "Return index of start of first match for REGEXP in STRING, or nil.\n\
If third arg START is non-nil, start search at that index in STRING.\n\
For index of first char beyond the match, do (match-end 0).\n\
`match-end' and `match-beginning' also give indices of substrings\n\
matched by parenthesis constructs in the pattern.")
  (regexp, string, start)
     Lisp_Object regexp, string, start;
{
  int val;
  int s;

  CHECK_STRING (regexp, 0);
  CHECK_STRING (string, 1);

  if (NILP (start))
    s = 0;
  else
    {
      int len = string_length (XSTRING (string));

      CHECK_FIXNUM (start, 2);
      s = XINT (start);
      if (s < 0 && -s <= len)
	s = len - s;
      else if (0 > s || s > len)
	args_out_of_range (string, start);
    }

  compile_pattern (regexp, &searchbuf, &search_regs,
		   (!NILP (current_buffer->case_fold_search)
                    ? (char *) DOWNCASE_TABLE
                    : 0));
  immediate_quit = 1;
  val = re_search (&searchbuf,
                   (char *) XSTRING (string)->data,
                   string_length (XSTRING (string)),
                   s,
                   string_length (XSTRING (string)) - s, 
                   &search_regs);
  immediate_quit = 0;
  last_thing_searched = Qt;
  if (val == -2)
    matcher_overflow ();
  if (val < 0) return Qnil;
  return make_number (val);
}


/* Match REGEXP against STRING, searching all of STRING,
   and return the index of the match, or negative on failure.
   This does not clobber the match data.  */

int
fast_string_match (Lisp_Object regexp, CONST char *string, int len)
{
  int val;

  compile_pattern (regexp, &searchbuf, 0, 0);
  immediate_quit = 1;
  val = re_search (&searchbuf,
                   string, len, 0, len,
		   0);
  immediate_quit = 0;
  return val;
}

/* Search in BUF for COUNT instances of the character TARGET, starting
   at START.  If COUNT is negative, search backwards.

   If we find COUNT instances, set *SHORTAGE to zero, and return the
   position after the COUNTth match.  Note that for reverse motion
   this is not the same as the usual convention for Emacs motion commands.

   If we don't find COUNT instances before reaching the end of the
   buffer (or the beginning, if scanning backwards), set *SHORTAGE to
   the number of TARGETs left unfound, and return the end of the
   buffer we bumped up against.

   If ALLOW_QUIT is non-zero, set immediate_quit.  That's good to do
   except when inside redisplay.  */

int
scan_buffer (buf, target, start, count, shortage, allow_quit)
     struct buffer *buf;
     int *shortage, start;
     register int count, target;
     int allow_quit;
{
  int limit = ((count > 0) ? BUF_ZV(buf) - 1 : BUF_BEGV(buf));
  int direction = ((count > 0) ? 1 : -1);

#ifdef I18N4
  register wchar_t *cursor;
  wchar_t *base;
#else
  register unsigned char *cursor;
  unsigned char *base;
#endif

  register int ceiling;
#ifdef I18N4
  register wchar_t *ceiling_addr;
#else
  register unsigned char *ceiling_addr;
#endif

  if (shortage != 0)
    *shortage = 0;

  immediate_quit = allow_quit;

  if (count > 0)
    while (start != limit + 1)
      {
	ceiling =  BUFFER_CEILING_OF (buf, start);
	ceiling = min (limit, ceiling);
	ceiling_addr = &BUF_CHAR_AT (buf, ceiling) + 1;
	base = (cursor = &BUF_CHAR_AT (buf, start));
	while (1)
	  {
	    while (*cursor != target && ++cursor != ceiling_addr)
	      ;
	    if (cursor != ceiling_addr)
	      {
		if (--count == 0)
		  {
		    immediate_quit = 0;
		    return (start + cursor - base + 1);
		  }
		else
		  if (++cursor == ceiling_addr)
		    break;
	      }
	    else
	      break;
	  }
	start += cursor - base;
      }
  else
    {
      start--;			/* first character we scan */
      while (start > limit - 1)
	{			/* we WILL scan under start */
	  ceiling =  BUFFER_FLOOR_OF (buf, start);
	  ceiling = max (limit, ceiling);
	  ceiling_addr = &BUF_CHAR_AT (buf, ceiling) - 1;
	  base = (cursor = &BUF_CHAR_AT (buf, start));
	  cursor++;
	  while (1)
	    {
	      while (--cursor != ceiling_addr && *cursor != target)
		;
	      if (cursor != ceiling_addr)
		{
		  if (++count == 0)
		    {
		      immediate_quit = 0;
		      return (start + cursor - base + 1);
		    }
		}
	      else
		break;
	    }
	  start += cursor - base;
	}
    }
  immediate_quit = 0;
  if (shortage != 0)
    *shortage = count * direction;
  return (start + ((direction == 1 ? 0 : 1)));
}

int
find_next_newline (buf, from, cnt)
     register struct buffer *buf;
     register int from, cnt;
{
  return (scan_buffer (buf, '\n', from, cnt, (int *) 0, 1));
}

static Lisp_Object
skip_chars (int forwardp, int syntaxp, Lisp_Object string, Lisp_Object lim)
{
  register unsigned char *p, *pend;
  /* jwz: c must be bigger than char, else (skip-chars-forward "\200-\377")
     loops while trying to fill fastmap, as c++ wraps when c == 255. */
  register unsigned int c;
#ifdef I18N4
  static set_of_chars fastmap;
#else
  unsigned char fastmap[0400];
  int negate = 0;
#endif
  register int i;
  Lisp_Object syntax_table = current_buffer->syntax_table;

  CHECK_STRING (string, 0);

  if (NILP (lim))
    XSET (lim, Lisp_Int, forwardp ? ZV : BEGV);
  else
    CHECK_FIXNUM_COERCE_MARKER (lim, 1);

  /* In any case, don't allow scan outside bounds of buffer.  */
  if (XINT (lim) > ZV)
    lim = make_number (ZV);
  if (XINT (lim) < BEGV)
    lim = make_number (BEGV);

  p = XSTRING (string)->data;
  pend = p + string_length (XSTRING (string));
#ifdef I18N4
  empty_set_of_chars (&fastmap);
#else
  memset (fastmap, 0, sizeof (fastmap));
#endif

  if (p != pend && *p == '^')
    {
#ifdef I18N4
      fastmap.complement = True;
#else
      negate = 1;
#endif
      p++;
    }

  /* Find the characters specified and set their elements of fastmap.
     If syntaxp, each character counts as itself.
     Otherwise, handle backslashes and ranges specially  */

  while (p != pend)
    {
      c = *p++;
      if (syntaxp)
#ifdef I18N4
	add_to_set_of_chars (&fastmap, c);
#else
	fastmap[c] = 1;
#endif
      else
	{
	  if (c == '\\')
	    {
	      if (p == pend) break;
	      c = *p++;
	    }
	  if (p != pend && *p == '-')
	    {
	      p++;
	      if (p == pend) break;
	      while (c <= *p)
		{
#ifdef I18N4
		  add_to_set_of_chars (&fastmap, c);
#else
		  fastmap[c] = 1;
#endif
		  c++;
		}
	      p++;
	    }
	  else
#ifdef I18N4
	    add_to_set_of_chars (&fastmap, c);
#else
	    fastmap[c] = 1;
#endif
	}
    }

#ifndef I18N4
  if (syntaxp && fastmap['-'] != 0)
    fastmap[' '] = 1;

  /* If ^ was the first character, complement the fastmap. */

  if (negate)
    for (i = 0; i < sizeof fastmap; i++)
      fastmap[i] ^= 1;
#endif

  {
    int start_point = PT;

    immediate_quit = 1;
    if (syntaxp)
      {

	if (forwardp)
	  {
#ifdef I18N4
	    while (PT < XINT (lim)
		   && in_set_of_chars (&fastmap, (wchar_t) syntax_code_spec
				       [(int) SYNTAX (syntax_table, CHAR_AT (PT))]))
#else
	    while (PT < XINT (lim)
		   && fastmap[(unsigned char)
                              syntax_code_spec[(int) SYNTAX (syntax_table,
                                                             FETCH_CHAR (PT))]])
#endif
	      SET_PT (PT + 1);
	  }
	else
	  {
#ifdef I18N4
	    while (PT > XINT (lim)
		   && in_set_of_chars (&fastmap, (wchar_t) syntax_code_spec
				       [(int) SYNTAX (syntax_table, CHAR_AT (PT))]))
#else
	    while (PT > XINT (lim)
		   && fastmap[(unsigned char) 
                              syntax_code_spec[(int) SYNTAX (syntax_table,
                                                             FETCH_CHAR (PT - 1))]])
#endif
	      SET_PT (PT - 1);
	  }
      }
    else
      {
	if (forwardp)
	  {
#ifdef I18N4
	    while (PT < XINT (lim) && in_set_of_chars (&fastmap, CHAR_AT (PT)))
#else
	    while (PT < XINT (lim) && fastmap[FETCH_CHAR (PT)])
#endif
	      SET_PT (PT + 1);
	  }
	else
	  {
#ifdef I18N4
	    while (PT > XINT (lim) && in_set_of_chars (&fastmap, CHAR_AT(PT - 1)))
#else
	    while (PT > XINT (lim) && fastmap[FETCH_CHAR (PT - 1)])
#endif
	      SET_PT (PT - 1);
	  }
      }
    immediate_quit = 0;
    return make_number (PT - start_point);
  }
}

DEFUN ("skip-chars-forward", Fskip_chars_forward, Sskip_chars_forward, 1, 2, 0,
  "Move point forward, stopping before a char not in CHARS, or at position LIM.\n\
CHARS is like the inside of a `[...]' in a regular expression\n\
except that `]' is never special and `\\' quotes `^', `-' or `\\'.\n\
Thus, with arg \"a-zA-Z\", this skips letters stopping before first nonletter.\n\
With arg \"^a-zA-Z\", skips nonletters stopping before first letter.\n\
Returns the distance traveled, either zero or positive.")
  (string, lim)
     Lisp_Object string, lim;
{
  return skip_chars (1, 0, string, lim);
}

DEFUN ("skip-chars-backward", Fskip_chars_backward, Sskip_chars_backward, 1, 2, 0,
  "Move point backward, stopping after a char not in CHARS, or at position LIM.\n\
See `skip-chars-forward' for details.\n\
Returns the distance traveled, either zero or negative.")
  (chars, lim)
     Lisp_Object chars, lim;
{
  return skip_chars (0, 0, chars, lim);
}


DEFUN ("skip-syntax-forward", Fskip_syntax_forward, Sskip_syntax_forward, 1, 2, 0,
  "Move point forward across chars in specified syntax classes.\n\
SYNTAX is a string of syntax code characters.\n\
Stop before a char whose syntax is not in SYNTAX, or at position LIM.\n\
If SYNTAX starts with ^, skip characters whose syntax is NOT in SYNTAX.\n\
This function returns the distance traveled, either zero or positive.")
  (syntax, lim)
     Lisp_Object syntax, lim;
{
  return skip_chars (1, 1, syntax, lim);
}

DEFUN ("skip-syntax-backward", Fskip_syntax_backward, Sskip_syntax_backward, 1, 2, 0,
  "Move point backward across chars in specified syntax classes.\n\
SYNTAX is a string of syntax code characters.\n\
Stop on reaching a char whose syntax is not in SYNTAX, or at position LIM.\n\
If SYNTAX starts with ^, skip characters whose syntax is NOT in SYNTAX.\n\
This function returns the distance traveled, either zero or negative.")
  (syntax, lim)
     Lisp_Object syntax, lim;
{
  return skip_chars (0, 1, syntax, lim);
}


/* Subroutines of Lisp buffer search functions. */

static int search_buffer (Lisp_Object str, int pos, int lim, int n, int RE, 
                          unsigned char *trt, unsigned char *inverse_trt);

static Lisp_Object
search_command (string, bound, noerror, count, direction, RE)
     Lisp_Object string, bound, noerror, count;
     int direction;
     int RE;
{
  register int np;
  long lim;
  long n = direction;

  if (!NILP (count))
    {
      CHECK_FIXNUM (count, 3);
      n *= XINT (count);
    }

  CHECK_STRING (string, 0);
  if (NILP (bound))
    lim = n > 0 ? ZV : BEGV;
  else
    {
      CHECK_FIXNUM_COERCE_MARKER (bound, 1);
      lim = XINT (bound);
      if (n > 0 ? lim < PT : lim > PT)
	error (GETTEXT ("Invalid search bound (wrong side of point)"));
      if (lim > ZV)
	lim = ZV;
      if (lim < BEGV)
	lim = BEGV;
    }

  np = search_buffer (string, PT, lim, n, RE,
		      (!NILP (current_buffer->case_fold_search)
		       ? XSTRING (current_buffer->case_canon_table)->data 
                       : 0),
		      (!NILP (current_buffer->case_fold_search)
		       ? XSTRING (current_buffer->case_eqv_table)->data 
                       : 0));

  if (np <= 0)
    {
      if (NILP (noerror))
	{
	  Fsignal (Qsearch_failed, list1 (string));
	  return Qnil;
	}
      if (!EQ (noerror, Qt))
	{
	  if (lim < BEGV || lim > ZV)
	    abort ();
	  SET_PT (lim);
	  return Qnil;
#if 0				/* This would be clean, but maybe programs depend on
				   a value of nil here.  */
	  np = lim;
#endif
	}

      else
        return Qnil;
    }

  if (np < BEGV || np > ZV)
    abort ();

  SET_PT (np);

  return make_number (np);
}

/* Search for the n'th occurrence of STRING in the current buffer,
   starting at position POS and stopping at position LIM,
   treating PAT as a literal string if RE is false or as
   a regular expression if RE is true.

   If N is positive, searching is forward and LIM must be greater than POS.
   If N is negative, searching is backward and LIM must be less than POS.

   Returns -x if only N-x occurrences found (x > 0),
   or else the position at the beginning of the Nth occurrence
   (if searching backward) or the end (if searching forward).  */

static int
search_buffer (string, pos, lim, n, RE, trt, inverse_trt)
     Lisp_Object string;
     int pos;
     int lim;
     int n;
     int RE;
     register unsigned char *trt;
     register unsigned char *inverse_trt;
{
#ifdef I18N4
  int len = safe_mbstowcs ((char *) XSTRING (string)->data, &wc_buf);
  wchar_t *base_pat = wc_buf.data;
  /* >>> should use Emacs hash-tables!! */
  static assoc_array BM_tab = EMPTY_ASSOC_ARRAY;
#else
  int len = string_length (XSTRING (string));
  unsigned char *base_pat = XSTRING (string)->data;
  register int *BM_tab;
  int *BM_tab_base;
#endif
  register int direction = ((n > 0) ? 1 : -1);
  register int dirlen;
  int infinity, limit, k, stride_for_teases = 0;
#ifdef I18N4
  register wchar_t *pat;
  register wchar_t *cursor, *p_limit;  
#else
  register unsigned char *pat, *cursor, *p_limit;  
#endif
  register int i, j;
#ifdef I18N4
  wchar_t *p1, *p2;
#else
  unsigned char *p1, *p2;
#endif
  int s1, s2;

  /* Null string is found at starting position.  */
  if (len == 0)
  {
    set_search_regs (pos, 0);
    return (pos);
  }

  /* Searching 0 times means don't move.  */
  if (n == 0)
    return pos;

  if (RE)
    compile_pattern (string, &searchbuf, &search_regs, (char *) trt);
  
  if (RE			/* Here we detect whether the */
				/* generality of an RE search is */
				/* really needed. */
      /* first item is "exact match" */
#ifdef NEW_REGEX
      && *(searchbuf.buffer) == (char) RE_EXACTN_VALUE
#else
      && *(searchbuf.buffer) == exactn
#endif
      && searchbuf.buffer[1] + 2 == searchbuf.used) /*first is ONLY item */
    {
      RE = 0;			/* can do straight (non RE) search */
#ifdef I18N4
      pat = (base_pat = searchbuf.buffer + 2);
#else
      pat = (base_pat = (unsigned char *) searchbuf.buffer + 2);
#endif
				/* trt already applied */
      len = searchbuf.used - 2;
    }
  else if (!RE)
    {
#ifdef I18N4
      pat = (wchar_t *) alloca (len * sizeof (wchar_t));
#else
      pat = (unsigned char *) alloca (len);
#endif

      for (i = len; i--;)		/* Copy the pattern; apply trt */
#ifdef I18N4
	*pat++ = (((int) trt) ? do_translate (trt, *base_pat++) : *base_pat++);
#else
	*pat++ = (((int) trt) ? trt [*base_pat++] : *base_pat++);
#endif
      pat -= len; base_pat = pat;
    }

  if (RE)
    {
      immediate_quit = 1;	/* Quit immediately if user types ^G,
				   because letting this function finish
				   can take too long. */
      QUIT;			/* Do a pending quit right away,
				   to avoid paradoxical behavior */
      /* Get pointers and sizes of the two strings
	 that make up the visible portion of the buffer. */

      p1 = BEGV_ADDR;
      s1 = GPT - BEGV;
      p2 = GAP_END_ADDR;
      s2 = ZV - GPT;
      if (s1 < 0)
	{
	  p2 = p1;
	  s2 = ZV - BEGV;
	  s1 = 0;
	}
      if (s2 < 0)
	{
	  s1 = ZV - BEGV;
	  s2 = 0;
	}
      while (n < 0)
	{
          int val;
#ifdef I18N4
	  val = re_search_2 (&searchbuf, p1, s1, p2, s2,
			     pos - BEGV, lim - pos, &search_regs,
			     /* Don't allow match past current point */
			     pos - BEGV);
#else
	  val = re_search_2 (&searchbuf, (char *) p1, s1, (char *) p2, s2,
                             pos - BEGV, lim - pos, &search_regs,
                             /* Don't allow match past current point */
                             pos - BEGV);
#endif
	  if (val == -2)
	    matcher_overflow ();
	  if (val >= 0)
	    {
	      j = BEGV;
	      for (i = 0; i < SEARCH_NREGS (&search_regs); i++)
		if (search_regs.start[i] >= 0)
		  {
		    search_regs.start[i] += j;
		    search_regs.end[i] += j;
		  }
	      XSETR (last_thing_searched, Lisp_Buffer, current_buffer);
	      /* Set pos to the new position. */
	      pos = search_regs.start[0];
	    }
	  else
	    {
	      immediate_quit = 0;
	      return (n);
	    }
	  n++;
	}
      while (n > 0)
	{
	  int val;
#ifdef I18N4
          val = re_search_2 (&searchbuf, p1, s1, p2, s2,
                             pos - BEGV, lim - pos, &search_regs,
                             lim - BEGV);
#else
          val = re_search_2 (&searchbuf, (char *) p1, s1, (char *) p2, s2,
                             pos - BEGV, lim - pos, &search_regs,
                             lim - BEGV);
#endif
	  if (val == -2)
	    matcher_overflow ();
	  if (val >= 0)
	    {
	      j = BEGV;
	      for (i = 0; i < SEARCH_NREGS (&search_regs); i++)
		if (search_regs.start[i] >= 0)
		  {
		    search_regs.start[i] += j;
		    search_regs.end[i] += j;
		  }
	      XSETR (last_thing_searched, Lisp_Buffer, current_buffer);
	      pos = search_regs.end[0];
	    }
	  else
	    {
	      immediate_quit = 0;
	      return (0 - n);
	    }
	  n--;
	}
      immediate_quit = 0;
      return (pos);
    }
  else				/* non-RE case */
    {
#ifndef I18N4
#ifdef C_ALLOCA
      int BM_tab_space[0400];
      BM_tab = &BM_tab_space[0];
#else
      BM_tab = (int *) alloca (0400 * sizeof (int));
#endif
#endif /* not I18N4 */
      /* The general approach is that we are going to maintain that we know */
      /* the first (closest to the present position, in whatever direction */
      /* we're searching) character that could possibly be the last */
      /* (furthest from present position) character of a valid match.  We */
      /* advance the state of our knowledge by looking at that character */
      /* and seeing whether it indeed matches the last character of the */
      /* pattern.  If it does, we take a closer look.  If it does not, we */
      /* move our pointer (to putative last characters) as far as is */
      /* logically possible.  This amount of movement, which I call a */
      /* stride, will be the length of the pattern if the actual character */
      /* appears nowhere in the pattern, otherwise it will be the distance */
      /* from the last occurrence of that character to the end of the */
      /* pattern. */
      /* As a coding trick, an enormous stride is coded into the table for */
      /* characters that match the last character.  This allows use of only */
      /* a single test, a test for having gone past the end of the */
      /* permissible match region, to test for both possible matches (when */
      /* the stride goes past the end immediately) and failure to */
      /* match (where you get nudged past the end one stride at a time). */ 

      /* Here we make a "mickey mouse" BM table.  The stride of the search */
      /* is determined only by the last character of the putative match. */
      /* If that character does not match, we will stride the proper */
      /* distance to propose a match that superimposes it on the last */
      /* instance of a character that matches it (per trt), or misses */
      /* it entirely if there is none. */  

      dirlen = len * direction;
      infinity = dirlen - (lim + pos + len + len) * direction;
      if (direction < 0)
	pat = (base_pat += len - 1);
#ifdef I18N4
      BM_tab.in_use = 0;
#else
      BM_tab_base = BM_tab;
      BM_tab += 0400;
#endif
      j = dirlen;		/* to get it in a register */
      /* A character that does not appear in the pattern induces a */
      /* stride equal to the pattern length. */
#ifdef I18N4
      BM_tab.default_value = j;
#else
      while (BM_tab_base != BM_tab)
	{
	  *--BM_tab = j;
	  *--BM_tab = j;
	  *--BM_tab = j;
	  *--BM_tab = j;
	}
#endif
      i = 0;
      while (i != infinity)
	{
	  j = pat[i]; i += direction;
	  if (i == dirlen) i = infinity;
	  if ((int) trt)
	    {
#ifdef I18N4
	      k = (j = do_translate (trt, j));
#else
	      k = (j = trt[j]);
#endif
	      if (i == infinity)
#ifdef I18N4
  		stride_for_teases = assoc_array_lookup (&BM_tab, j);
#else
		stride_for_teases = BM_tab[j];
#endif
#ifdef I18N4
	      set_assoc_entry (&BM_tab, j, dirlen - i);
#else
	      BM_tab[j] = dirlen - i;
#endif
	      /* A translation table is accompanied by its inverse -- see */
	      /* comment following downcase_table for details */ 

#ifdef I18N4
	      while ((j = do_translate (inverse_trt, j)) != k)
		set_assoc_entry (&BM_tab, j, dirlen - i);
#else
	      while ((j = inverse_trt[j]) != k)
		BM_tab[j] = dirlen - i;
#endif
	    }
	  else
	    {
	      if (i == infinity)
#ifdef I18N4
		stride_for_teases = assoc_array_lookup (&BM_tab, j);
#else
		stride_for_teases = BM_tab[j];
#endif
#ifdef I18N4
	      set_assoc_entry (&BM_tab, j, dirlen - i);
#else
	      BM_tab[j] = dirlen - i;
#endif
	    }
	  /* stride_for_teases tells how much to stride if we get a */
	  /* match on the far character but are subsequently */
	  /* disappointed, by recording what the stride would have been */
	  /* for that character if the last character had been */
	  /* different. */
	}
      infinity = dirlen - infinity;
      pos += dirlen - ((direction > 0) ? direction : 0);
      /* loop invariant - pos points at where last char (first char if reverse)
	 of pattern would align in a possible match.  */
      while (n != 0)
	{
	  if ((lim - pos - (direction > 0)) * direction < 0)
	    return (n * (0 - direction));
	  /* First we do the part we can by pointers (maybe nothing) */
	  QUIT;
	  pat = base_pat;
	  limit = pos - dirlen + direction;
	  limit = ((direction > 0)
		   ? BUFFER_CEILING_OF (current_buffer, limit)
		   : BUFFER_FLOOR_OF (current_buffer, limit));
	  /* LIMIT is now the last (not beyond-last!) value
	     POS can take on without hitting edge of buffer or the gap.  */
	  limit = ((direction > 0)
		   ? min (lim - 1, min (limit, pos + 20000))
		   : max (lim, max (limit, pos - 20000)));
	  if ((limit - pos) * direction > 20)
	    {
	      p_limit = &FETCH_CHAR (limit);
	      p2 = (cursor = &FETCH_CHAR (pos));
	      /* In this loop, pos + cursor - p2 is the surrogate for pos */
	      while (1)		/* use one cursor setting as long as i can */
		{
		  if (direction > 0) /* worth duplicating */
		    {
		      /* Use signed comparison if appropriate
			 to make cursor+infinity sure to be > p_limit.
			 Assuming that the buffer lies in a range of addresses
			 that are all "positive" (as ints) or all "negative",
			 either kind of comparison will work as long
			 as we don't step by infinity.  So pick the kind
			 that works when we do step by infinity.  */
		      if ((int) (p_limit + infinity) > (int) p_limit)
			while ((int) cursor <= (int) p_limit)
#ifdef I18N4
			  cursor += assoc_array_lookup (&BM_tab, *cursor);
#else
			  cursor += BM_tab[*cursor];
#endif
		      else
			while ((unsigned int) cursor <= (unsigned int) p_limit)
#ifdef I18N4
			  cursor += assoc_array_lookup (&BM_tab, *cursor);
#else
			  cursor += BM_tab[*cursor];
#endif
		    }
		  else
		    {
		      if ((int) (p_limit + infinity) < (int) p_limit)
			while ((int) cursor >= (int) p_limit)
#ifdef I18N4
			  cursor += assoc_array_lookup (&BM_tab, *cursor);
#else
			  cursor += BM_tab[*cursor];
#endif
		      else
			while ((unsigned int) cursor >= (unsigned int) p_limit)
#ifdef I18N4
			  cursor += assoc_array_lookup (&BM_tab, *cursor);
#else
			  cursor += BM_tab[*cursor];
#endif
		    }
/* If you are here, cursor is beyond the end of the searched region. */
 /* This can happen if you match on the far character of the pattern, */
 /* because the "stride" of that character is infinity, a number able */
 /* to throw you well beyond the end of the search.  It can also */
 /* happen if you fail to match within the permitted region and would */
 /* otherwise try a character beyond that region */
		  if ((cursor - p_limit) * direction <= len)
		    break;	/* a small overrun is genuine */
		  cursor -= infinity; /* large overrun = hit */
		  i = dirlen - direction;
		  if ((int) trt)
		    {
		      while ((i -= direction) + direction != 0)
#ifdef I18N4
			if (pat[i] != do_translate (trt, *(cursor -= direction)))
#else
			if (pat[i] != trt[*(cursor -= direction)])
#endif
			  break;
		    }
		  else
		    {
		      while ((i -= direction) + direction != 0)
			if (pat[i] != *(cursor -= direction))
			  break;
		    }
		  cursor += dirlen - i - direction;	/* fix cursor */
		  if (i + direction == 0)
		    {
		      cursor -= direction;

                      set_search_regs (pos + cursor - p2 + ((direction > 0)
							    ? 1 - len : 0),
				       len);

		      if ((n -= direction) != 0)
			cursor += dirlen; /* to resume search */
		      else
			return ((direction > 0)
				? search_regs.end[0] : search_regs.start[0]);
		    }
		  else
		    cursor += stride_for_teases; /* <sigh> we lose -  */
		}
	      pos += cursor - p2;
	    }
	  else
	    /* Now we'll pick up a clump that has to be done the hard */
	    /* way because it covers a discontinuity */
	    {
	      limit = ((direction > 0)
		       ? BUFFER_CEILING_OF (current_buffer, pos - dirlen + 1)
		       : BUFFER_FLOOR_OF (current_buffer, pos - dirlen - 1));
	      limit = ((direction > 0)
		       ? min (limit + len, lim - 1)
		       : max (limit - len, lim));
	      /* LIMIT is now the last value POS can have
		 and still be valid for a possible match.  */
	      while (1)
		{
		  /* This loop can be coded for space rather than */
		  /* speed because it will usually run only once. */
		  /* (the reach is at most len + 21, and typically */
		  /* does not exceed len) */    
		  while ((limit - pos) * direction >= 0)
#ifdef I18N4
		    pos += assoc_array_lookup (&BM_tab, FETCH_CHAR(pos));
#else
		    pos += BM_tab[FETCH_CHAR(pos)];
#endif
		  /* now run the same tests to distinguish going off the */
		  /* end, a match or a phony match. */
		  if ((pos - limit) * direction <= len)
		    break;	/* ran off the end */
		  /* Found what might be a match.
		     Set POS back to last (first if reverse) char pos.  */
		  pos -= infinity;
		  i = dirlen - direction;
		  while ((i -= direction) + direction != 0)
		    {
		      pos -= direction;
		      if (pat[i] != (((int) trt)
#ifdef I18N4
				     ? do_translate (trt, FETCH_CHAR(pos))
#else
				     ? trt[FETCH_CHAR(pos)]
#endif
				     : FETCH_CHAR (pos)))
			break;
		    }
		  /* Above loop has moved POS part or all the way
		     back to the first char pos (last char pos if reverse).
		     Set it once again at the last (first if reverse) char.  */
		  pos += dirlen - i- direction;
		  if (i + direction == 0)
		    {
		      pos -= direction;

		      set_search_regs (pos + ((direction > 0) ? 1 - len : 0),
				       len);

		      if ((n -= direction) != 0)
			pos += dirlen; /* to resume search */
		      else
			return ((direction > 0)
				? search_regs.end[0] : search_regs.start[0]);
		    }
		  else
		    pos += stride_for_teases;
		}
	      }
	  /* We have done one clump.  Can we continue? */
	  if ((lim - pos) * direction < 0)
	    return ((0 - n) * direction);
	}
      return pos;
    }
}

/* Record beginning BEG and end BEG + LEN
   for a match just found in the current buffer.  */

static void
set_search_regs (int beg, int len)
{
#ifdef NEW_REGEX
  /* Make sure we have registers in which to store
     the match position.  */
  if (search_regs.num_regs == 0)
  {
    regoff_t *starts, *ends;

    starts = (regoff_t *) xmalloc (2 * sizeof (regoff_t));
    ends = (regoff_t *) xmalloc (2 * sizeof (regoff_t));
    re_set_registers (&searchbuf,
                      &search_regs,
                      2, starts, ends);
  }
#endif /* NEW_REGEX */

  search_regs.start[0] = beg;
  search_regs.end[0] = beg + len;
  XSETR (last_thing_searched, Lisp_Buffer, current_buffer);
}


/* Given a string of words separated by word delimiters,
  compute a regexp that matches those exact words
  separated by arbitrary punctuation.  */

static Lisp_Object
wordify (string)
     Lisp_Object string;
{
  register unsigned char *p, *o;
  register int i, len, punct_count = 0, word_count = 0;
  Lisp_Object val;
  Lisp_Object syntax_table = current_buffer->syntax_table;

  CHECK_STRING (string, 0);
  p = XSTRING (string)->data;
  len = string_length (XSTRING (string));

  for (i = 0; i < len; i++)
    if (SYNTAX (syntax_table, p[i]) != Sword)
      {
	punct_count++;
	if (i > 0 && SYNTAX (syntax_table, p[i-1]) == Sword)
          word_count++;
      }
  if (SYNTAX (syntax_table, p[len-1]) == Sword)
    word_count++;
  if (!word_count) return build_string ("");

  val = make_string ((char *) p,
		     len - punct_count + 5 * (word_count - 1) + 4);

  o = XSTRING (val)->data;
  *o++ = '\\';
  *o++ = 'b';

  for (i = 0; i < len; i++)
    if (SYNTAX (syntax_table, p[i]) == Sword)
      *o++ = p[i];
    else if (i > 0
             && SYNTAX (syntax_table, p[i-1]) == Sword
             && --word_count)
      {
	*o++ = '\\';
	*o++ = 'W';
	*o++ = '\\';
	*o++ = 'W';
	*o++ = '*';
      }

  *o++ = '\\';
  *o++ = 'b';

  return val;
}

DEFUN ("search-backward", Fsearch_backward, Ssearch_backward, 1, 4,
  "sSearch backward: ",
  "Search backward from point for STRING.\n\
Set point to the beginning of the occurrence found, and return point.\n\
An optional second argument bounds the search; it is a buffer position.\n\
The match found must not extend before that position.\n\
Optional third argument, if t, means if fail just return nil (no error).\n\
 If not nil and not t, position at limit of search and return nil.\n\
Optional fourth argument is repeat count--search for successive occurrences.\n\
See also the functions `match-beginning', `match-end' and `replace-match'.")
  (string, bound, noerror, count)
     Lisp_Object string, bound, noerror, count;
{
  return search_command (string, bound, noerror, count, -1, 0);
}

DEFUN ("search-forward", Fsearch_forward, Ssearch_forward, 1, 4, "sSearch: ",
  "Search forward from point for STRING.\n\
Set point to the end of the occurrence found, and return point.\n\
An optional second argument bounds the search; it is a buffer position.\n\
The match found must not extend after that position.  nil is equivalent\n\
  to (point-max).\n\
Optional third argument, if t, means if fail just return nil (no error).\n\
  If not nil and not t, move to limit of search and return nil.\n\
Optional fourth argument is repeat count--search for successive occurrences.\n\
See also the functions `match-beginning', `match-end' and `replace-match'.")
  (string, bound, noerror, count)
     Lisp_Object string, bound, noerror, count;
{
  return search_command (string, bound, noerror, count, 1, 0);
}

DEFUN ("word-search-backward", Fword_search_backward, Sword_search_backward, 1, 4,
  "sWord search backward: ",
  "Search backward from point for STRING, ignoring differences in punctuation.\n\
Set point to the beginning of the occurrence found, and return point.\n\
An optional second argument bounds the search; it is a buffer position.\n\
The match found must not extend before that position.\n\
Optional third argument, if t, means if fail just return nil (no error).\n\
  If not nil and not t, move to limit of search and return nil.\n\
Optional fourth argument is repeat count--search for successive occurrences.")
  (string, bound, noerror, count)
     Lisp_Object string, bound, noerror, count;
{
  return search_command (wordify (string), bound, noerror, count, -1, 1);
}

DEFUN ("word-search-forward", Fword_search_forward, Sword_search_forward, 1, 4,
  "sWord search: ",
  "Search forward from point for STRING, ignoring differences in punctuation.\n\
Set point to the end of the occurrence found, and return point.\n\
An optional second argument bounds the search; it is a buffer position.\n\
The match found must not extend after that position.\n\
Optional third argument, if t, means if fail just return nil (no error).\n\
  If not nil and not t, move to limit of search and return nil.\n\
Optional fourth argument is repeat count--search for successive occurrences.")
  (string, bound, noerror, count)
     Lisp_Object string, bound, noerror, count;
{
  return search_command (wordify (string), bound, noerror, count, 1, 1);
}

DEFUN ("re-search-backward", Fre_search_backward, Sre_search_backward, 1, 4,
  "sRE search backward: ",
  "Search backward from point for match for regular expression REGEXP.\n\
Set point to the beginning of the match, and return point.\n\
The match found is the one starting last in the buffer\n\
and yet ending before the origin of the search.\n\
An optional second argument bounds the search; it is a buffer position.\n\
The match found must start at or after that position.\n\
Optional third argument, if t, means if fail just return nil (no error).\n\
  If not nil and not t, move to limit of search and return nil.\n\
Optional fourth argument is repeat count--search for successive occurrences.\n\
See also the functions `match-beginning', `match-end' and `replace-match'.")
  (regexp, bound, noerror, count)
     Lisp_Object regexp, bound, noerror, count;
{
  return search_command (regexp, bound, noerror, count, -1, 1);
}

DEFUN ("re-search-forward", Fre_search_forward, Sre_search_forward, 1, 4,
  "sRE search: ",
  "Search forward from point for regular expression REGEXP.\n\
Set point to the end of the occurrence found, and return point.\n\
An optional second argument bounds the search; it is a buffer position.\n\
The match found must not extend after that position.\n\
Optional third argument, if t, means if fail just return nil (no error).\n\
  If not nil and not t, move to limit of search and return nil.\n\
Optional fourth argument is repeat count--search for successive occurrences.\n\
See also the functions `match-beginning', `match-end' and `replace-match'.")
  (regexp, bound, noerror, count)
     Lisp_Object regexp, bound, noerror, count;
{
  return search_command (regexp, bound, noerror, count, 1, 1);
}

DEFUN ("replace-match", Freplace_match, Sreplace_match, 1, 3, 0,
  "Replace text matched by last search with NEWTEXT.\n\
If second arg FIXEDCASE is non-nil, do not alter case of replacement text.\n\
Otherwise convert to all caps or cap initials, like replaced text.\n\
If third arg LITERAL is non-nil, insert NEWTEXT literally.\n\
Otherwise treat `\\' as special:\n\
  `\\&' in NEWTEXT means substitute original matched text.\n\
  `\\N' means substitute what matched the Nth `\\(...\\)'.\n\
       If Nth parens didn't match, substitute nothing.\n\
  `\\\\' means insert one `\\'.\n\
FIXEDCASE and LITERAL are optional arguments.\n\
Leaves point at end of replacement text.")
  (newtext, fixedcase, literal)
     Lisp_Object newtext, fixedcase, literal;
{
  enum { nochange, all_caps, cap_initial } case_action;
  register int pos, last;
  int some_multiletter_word;
  int some_lowercase;
  int some_uppercase_initial;
  register int c, prevc;
  int inslen;
  Lisp_Object syntax_table = current_buffer->syntax_table;

  CHECK_STRING (newtext, 0);

  case_action = nochange;	/* We tried an initialization */
				/* but some C compilers blew it */

#ifdef NEW_REGEX
  if (search_regs.num_regs <= 0)
    error (GETTEXT ("replace-match called before any match found"));
#endif

  if (search_regs.start[0] < BEGV
      || search_regs.start[0] > search_regs.end[0]
      || search_regs.end[0] > ZV)
    args_out_of_range (make_number (search_regs.start[0]),
		       make_number (search_regs.end[0]));

  if (NILP (fixedcase))
    {
      /* Decide how to casify by examining the matched text. */

      last = search_regs.end[0];
      prevc = '\n';
      case_action = all_caps;

      /* some_multiletter_word is set nonzero if any original word
	 is more than one letter long. */
      some_multiletter_word = 0;
      some_lowercase = 0;
      some_uppercase_initial = 0;

      for (pos = search_regs.start[0]; pos < last; pos++)
	{
	  c = FETCH_CHAR (pos);
	  if (LOWERCASEP (c))
	    {
	      /* Cannot be all caps if any original char is lower case */

	      some_lowercase = 1;
	      if (SYNTAX (syntax_table, prevc) != Sword)
		;
	      else
		some_multiletter_word = 1;
	    }
	  else if (!NOCASEP (c))
	    {
	      if (SYNTAX (syntax_table, prevc) != Sword)
		some_uppercase_initial = 1;
	      else
		some_multiletter_word = 1;
	    }

	  prevc = c;
	}

      /* Convert to all caps if the old text is all caps
	 and has at least one multiletter word.  */
      if (! some_lowercase && some_multiletter_word)
	case_action = all_caps;
      /* Capitalize each word, if the old text has a capitalized word.  */
      else if (some_uppercase_initial)
	case_action = cap_initial;
      else
	case_action = nochange;
    }

  /* We insert the replacement text before the old text, and then
     delete the original text.  This means that markers at the
     beginning or end of the original will float to the corresponding
     position in the replacement.  */
  SET_PT (search_regs.start[0]);
  if (!NILP (literal))
    Finsert (1, &newtext);
  else
    {
      struct gcpro gcpro1;
      GCPRO1 (newtext);
      for (pos = 0; pos < string_length (XSTRING (newtext)); pos++)
	{
	  int offset = PT - search_regs.start[0];

	  c = XSTRING (newtext)->data[pos];
	  if (c == '\\')
	    {
	      c = XSTRING (newtext)->data[++pos];
	      if (c == '&')
		Finsert_buffer_substring 
                  (Fcurrent_buffer (),
                   make_number (search_regs.start[0] + offset),
                   make_number (search_regs.end[0] + offset));
	      /* lemacs change: even if SEARCH_NREGS is > 10, be sure only
		 to use the digits as match indicators.  */
	      else if (c >= '1' && c <= '9' &&
		       c <= SEARCH_NREGS (&search_regs) + '0')
		{
		  if (search_regs.start[c - '0'] >= 1)
		    Finsert_buffer_substring
                      (Fcurrent_buffer (),
                       make_number (search_regs.start[c - '0'] + offset),
                       make_number (search_regs.end[c - '0'] + offset));
		}
	      else
		insert_char (c);
	    }
	  else
	    insert_char (c);
	}
      UNGCPRO;
    }

  inslen = PT - (search_regs.start[0]);
  del_range (search_regs.start[0] + inslen, search_regs.end[0] + inslen);

  if (case_action == all_caps)
    Fupcase_region (make_number (PT - inslen), make_number (PT));
  else if (case_action == cap_initial)
    upcase_initials_region (make_number (PT - inslen),
                            make_number (PT));
  return Qnil;
}

static Lisp_Object
match_limit (num, beginningp)
     Lisp_Object num;
     int beginningp;
{
  register int n;

  CHECK_FIXNUM (num, 0);
  n = XINT (num);
  if (n < 0 || n >= SEARCH_NREGS (&search_regs))
    args_out_of_range (num, make_number (SEARCH_NREGS (&search_regs)));
#ifdef NEW_REGEX
  if (search_regs.num_regs <= 0)
    return (Qnil);
#endif
  if (search_regs.start[n] < 0)
    return Qnil;
  return (make_number ((beginningp) ? search_regs.start[n]
		                    : search_regs.end[n]));
}

DEFUN ("match-beginning", Fmatch_beginning, Smatch_beginning, 1, 1, 0,
  "Return position of start of text matched by last regexp search.\n\
NUM, specifies which parenthesized expression in the last regexp.\n\
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.\n\
Zero means the entire text matched by the whole regexp or whole string.")
  (num)
     Lisp_Object num;
{
  return match_limit (num, 1);
}

DEFUN ("match-end", Fmatch_end, Smatch_end, 1, 1, 0,
  "Return position of end of text matched by last regexp search.\n\
NUM specifies which parenthesized expression in the last regexp.\n\
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.\n\
Zero means the entire text matched by the whole regexp or whole string.")
  (num)
     Lisp_Object num;
{
  return match_limit (num, 0);
} 

DEFUN ("match-data", Fmatch_data, Smatch_data, 0, 0, 0,
  "Return a list containing all info on what the last regexp search matched.\n\
Element 2N is `(match-beginning N)'; element 2N + 1 is `(match-end N)'.\n\
All the elements are markers or nil (nil if the Nth pair didn't match)\n\
if the last match was on a buffer; integers or nil if a string was matched.\n\
Use `store-match-data' to reinstate the data in this list.")
  ()
{
  Lisp_Object *data;
  int i, len;

  if (NILP (last_thing_searched))
    error (GETTEXT ("match-data called before any match found"));

  data = (Lisp_Object *) alloca ((2 * SEARCH_NREGS (&search_regs))
				 * sizeof (Lisp_Object));

  len = -1;
  for (i = 0; i < SEARCH_NREGS (&search_regs); i++)
    {
      int start = search_regs.start[i];
      if (start >= 0)
	{
	  if (EQ (last_thing_searched, Qt))
	    {
	      data[2 * i] = make_number (start);
	      data[2 * i + 1] = make_number (search_regs.end[i]);
	    }
	  else if (BUFFERP (last_thing_searched))
	    {
	      data[2 * i] = Fmake_marker ();
	      Fset_marker (data[2 * i],
			   make_number (start),
			   last_thing_searched);
	      data[2 * i + 1] = Fmake_marker ();
	      Fset_marker (data[2 * i + 1],
			   make_number (search_regs.end[i]), 
			   last_thing_searched);
	    }
	  else
	    /* last_thing_searched must always be Qt, a buffer, or Qnil.  */
	    abort ();

	  len = i;
	}
      else
	data[2 * i] = data [2 * i + 1] = Qnil;
    }
  return Flist (2 * len + 2, data);
}


DEFUN ("store-match-data", Fstore_match_data, Sstore_match_data, 1, 1, 0,
  "Set internal data on last search match from elements of LIST.\n\
LIST should have been created by calling `match-data' previously.")
  (list)
     register Lisp_Object list;
{
  register int i;
  register Lisp_Object marker;

  if (!CONSP (list) && !NILP (list))
    list = wrong_type_argument (Qconsp, list);

  /* Unless we find a marker with a buffer in LIST, assume that this 
     match data came from a string.  */
  last_thing_searched = Qt;

#ifdef NEW_REGEX
  /* Allocate registers if they don't already exist.  */
  {
    int length = XINT (Flength (list)) / 2;

    if (length > search_regs.num_regs)
      {
	if (search_regs.num_regs == 0)
	  {
	    search_regs.start
	      = (regoff_t *) xmalloc (length * sizeof (regoff_t));
	    search_regs.end
	      = (regoff_t *) xmalloc (length * sizeof (regoff_t));
	  }
	else
	  {
	    search_regs.start
	      = (regoff_t *) xrealloc (search_regs.start,
				       length * sizeof (regoff_t));
	    search_regs.end
	      = (regoff_t *) xrealloc (search_regs.end,
				       length * sizeof (regoff_t));
	  }

	re_set_registers (&searchbuf, &search_regs, length,
			  search_regs.start, search_regs.end);
      }
  }
#endif /* NEW_REGEX */

  for (i = 0; i < SEARCH_NREGS (&search_regs); i++)
    {
      marker = Fcar (list);
      if (NILP (marker))
	{
	  search_regs.start[i] = -1;
	  list = Fcdr (list);
	}
      else
	{
	  if (MARKERP (marker))
	    {
	      if (XMARKER (marker)->buffer == 0)
		marker = Qzero;
	      else
		XSETR (last_thing_searched, Lisp_Buffer,
                       XMARKER (marker)->buffer);
	    }

	  CHECK_NUMBER_COERCE_MARKER (marker, 0);
	  search_regs.start[i] = XINT (marker);
	  list = Fcdr (list);

	  marker = Fcar (list);
	  if (MARKERP (marker)
	      && XMARKER (marker)->buffer == 0)
	    marker = Qzero;

	  CHECK_NUMBER_COERCE_MARKER (marker, 0);
	  search_regs.end[i] = XINT (marker);
	}
      list = Fcdr (list);
    }

  return Qnil;  
}

/* Quote a string to inactivate reg-expr chars */

DEFUN ("regexp-quote", Fregexp_quote, Sregexp_quote, 1, 1, 0,
  "Return a regexp string which matches exactly STRING and nothing else.")
  (str)
     Lisp_Object str;
{
  register unsigned char *in, *out, *end;
  register unsigned char *temp;

  CHECK_STRING (str, 0);

  temp = (unsigned char *) alloca (string_length (XSTRING (str)) * 2);

  /* Now copy the data into the new string, inserting escapes. */

  in = XSTRING (str)->data;
  end = in + string_length (XSTRING (str));
  out = temp; 

  for (; in != end; in++)
    {
      if (*in == '[' || *in == ']'
	  || *in == '*' || *in == '.' || *in == '\\'
	  || *in == '?' || *in == '+'
	  || *in == '^' || *in == '$')
	*out++ = '\\';
      *out++ = *in;
    }

  return make_string ((char *) temp, out - temp);
}

void
syms_of_search ()
{
  searchbuf.allocated = 100;
#ifdef I18N4
  searchbuf.buffer = (wchar_t *) xmalloc (searchbuf.allocated * sizeof (wchar_t));
  searchbuf.fastmap = &search_fastmap;
#else
#ifdef NEW_REGEX
  searchbuf.buffer = (unsigned char *) xmalloc (searchbuf.allocated);
#else
  searchbuf.buffer = (char *) xmalloc (searchbuf.allocated);
#endif
  searchbuf.fastmap = search_fastmap;
#endif

  defsymbol (&Qsearch_failed, "search-failed");
  defsymbol (&Qinvalid_regexp, "invalid-regexp");

  pure_put (Qsearch_failed, Qerror_conditions,
            list2 (Qsearch_failed, Qerror));
  pure_put (Qsearch_failed, Qerror_message,
            build_string (DEFER_GETTEXT ("Search failed")));

  pure_put (Qinvalid_regexp, Qerror_conditions,
            list2 (Qinvalid_regexp, Qerror));
  pure_put (Qinvalid_regexp, Qerror_message,
            build_string (DEFER_GETTEXT ("Invalid regexp")));

  last_regexp = Qnil;
  staticpro (&last_regexp);

  last_thing_searched = Qnil;
  staticpro (&last_thing_searched);

  defsubr (&Sstring_match);
  defsubr (&Slooking_at);
  defsubr (&Sskip_chars_forward);
  defsubr (&Sskip_chars_backward);
  defsubr (&Sskip_syntax_forward);
  defsubr (&Sskip_syntax_backward);
  defsubr (&Ssearch_forward);
  defsubr (&Ssearch_backward);
  defsubr (&Sword_search_forward);
  defsubr (&Sword_search_backward);
  defsubr (&Sre_search_forward);
  defsubr (&Sre_search_backward);
  defsubr (&Sreplace_match);
  defsubr (&Smatch_beginning);
  defsubr (&Smatch_end);
  defsubr (&Smatch_data);
  defsubr (&Sstore_match_data);
  defsubr (&Sregexp_quote);
}
