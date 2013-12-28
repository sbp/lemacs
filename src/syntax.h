/* Declarations having to do with GNU Emacs syntax tables.
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


extern Lisp_Object Qsyntax_table_p;
extern Lisp_Object Fsyntax_table_p (), Fsyntax_table (), Fset_syntax_table ();

/* The standard syntax table is stored where it will automatically
   be used in all new buffers.  */
#define Vstandard_syntax_table buffer_defaults.syntax_table

/* A syntax table is a Lisp vector of length 0400, whose elements are integers.

The low 8 bits of the integer is a code, as follows:
*/

enum syntaxcode
  {
    Swhitespace, /* for a whitespace character */
    Spunct,	 /* for random punctuation characters */
    Sword,	 /* for a word constituent */
    Ssymbol,	 /* symbol constituent but not word constituent */
    Sopen,	 /* for a beginning delimiter */
    Sclose,      /* for an ending delimiter */
    Squote,	 /* for a prefix character like Lisp ' */
    Sstring,	 /* for a string-grouping character like Lisp " */
    Smath,	 /* for delimiters like $ in Tex. */
    Sescape,	 /* for a character that begins a C-style escape */
    Scharquote,  /* for a character that quotes the following character */
    Scomment,    /* for a comment-starting character */
    Sendcomment, /* for a comment-ending character */
    Smax	 /* Upper bound on codes that are meaningful */
  };

#define SYNTAX(c) \
  ((enum syntaxcode) (XINT (XVECTOR (current_buffer->syntax_table)->contents[c]) & 0377))

/* The next 8 bits of the number is a character,
 the matching delimiter in the case of Sopen or Sclose. */

#define SYNTAX_MATCH(c) \
  ((XINT (XVECTOR (current_buffer->syntax_table)->contents[c]) >> 8) & 0377)

/* The following bits have different meanings depending on whether 
 emacs was compiled with the NEW_SYNTAX flag or not.  If so, then
 syntax parsing has been extended to recognize up to two different
 comment styles in a single mode. The next nine single-bit flags
 have the following meanings:

  1. first of a one or two character comment-start sequence of style a.
  2. first of a one or two character comment-start sequence of style b.
  3. second of a two-character comment-start sequence of style a.
  4. second of a two-character comment-start sequence of style b.
  5. first of a one or two character comment-end sequence of style a.
  6. first of a one or two character comment-end sequence of style b.
  7. second of a two-character comment-end sequence of style a.
  8. second of a two-character comment-end sequence of style b.
  9. This character is a prefix, for backward-prefix-chars.

 Otherwise, If emacs was not compiled with the NEW_SYNTAX flag, only
 the following limited syntax is available. This is compatible with
 old versions of emacs:

  1. This character is the first of a two-character comment-start sequence.
  2. This character is the second of a two-character comment-start sequence.
  3. This character is the first of a two-character comment-end sequence.
  4. This character is the second of a two-character comment-end sequence.
  5. This character is a prefix, for backward-prefix-chars.
  6. Comment style flag (see below).
 Note that any two-character sequence whose first character has flag 1
  and whose second character has flag 2 will be interpreted as a comment start.

 bit 6 is used to discriminate between two different comment styles.
 Languages such as C++ allow two orthogonal syntax start/end pairs and
 some way of pairing comment starts and ends has to be provided. To
 keep backward compatibility, we simply add bit 6 flag which is
 checked only on the second character of a two-char comment start
 sequence, and the first character of a two-char comment end sequence,
 or a Scommentend class character.
 */

#ifndef NEW_SYNTAX /* old way of doing things */

#define SYNTAX_COMSTART_FIRST(c) \
  ((XINT (XVECTOR (current_buffer->syntax_table)->contents[c]) >> 16) & 1)

#define SYNTAX_COMSTART_SECOND(c) \
  ((XINT (XVECTOR (current_buffer->syntax_table)->contents[c]) >> 17) & 1)

#define SYNTAX_COMEND_FIRST(c) \
  ((XINT (XVECTOR (current_buffer->syntax_table)->contents[c]) >> 18) & 1)

#define SYNTAX_COMEND_SECOND(c) \
  ((XINT (XVECTOR (current_buffer->syntax_table)->contents[c]) >> 19) & 1)

#define SYNTAX_PREFIX(c) \
  ((XINT (XVECTOR (current_buffer->syntax_table)->contents[c]) >> 20) & 1)

#else /* NEW_SYNTAX */

#define SYNTAX_COMMENT_BITS(c) \
  ((XINT (XVECTOR (current_buffer->syntax_table)->contents[c]) >> 16) &0xff)

#define SYNTAX_COMMENT_STYLE_A 0xaa
#define SYNTAX_COMMENT_STYLE_B 0x55
#define SYNTAX_FIRST_CHAR_START 0xc0
#define SYNTAX_SECOND_CHAR_START 0x30
#define SYNTAX_FIRST_CHAR_END 0x0c
#define SYNTAX_SECOND_CHAR_END 0x03

#define SYNTAX_FIRST_OF_START_A 0x80
#define SYNTAX_FIRST_OF_START_B 0x40
#define SYNTAX_SECOND_OF_START_A 0x20
#define SYNTAX_SECOND_OF_START_B 0x10
#define SYNTAX_FIRST_OF_END_A 0x08
#define SYNTAX_FIRST_OF_END_B 0x04
#define SYNTAX_SECOND_OF_END_A 0x02
#define SYNTAX_SECOND_OF_END_B 0x01

#define SYNTAX_START_SEQUENCE(a,b,style) \
(((SYNTAX_COMMENT_BITS(a)&style)&SYNTAX_FIRST_CHAR_START) && \
 ((SYNTAX_COMMENT_BITS(b)&style)&SYNTAX_SECOND_CHAR_START))

#define SYNTAX_END_SEQUENCE(a,b,style) \
(((SYNTAX_COMMENT_BITS(a)&style)&SYNTAX_FIRST_CHAR_END) && \
 ((SYNTAX_COMMENT_BITS(b)&style)&SYNTAX_SECOND_CHAR_END))

#define SYNTAX_START(a,b) \
(SYNTAX_START_SEQUENCE(a,b,SYNTAX_COMMENT_STYLE_A) || \
 SYNTAX_START_SEQUENCE(a,b,SYNTAX_COMMENT_STYLE_B))

#define SYNTAX_END(a,b) \
(SYNTAX_END_SEQUENCE(a,b,SYNTAX_COMMENT_STYLE_A) || \
 SYNTAX_END_SEQUENCE(a,b,SYNTAX_COMMENT_STYLE_B))

#define SYNTAX_SINGLE_CHAR_STYLE_A(c) \
(SYNTAX_COMMENT_BITS(c)&SYNTAX_COMMENT_STYLE_A)

#define SYNTAX_SINGLE_CHAR_STYLE_B(c) \
(SYNTAX_COMMENT_BITS(c)&SYNTAX_COMMENT_STYLE_B)

#define SYNTAX_PREFIX(c) \
  ((XINT (XVECTOR (current_buffer->syntax_table)->contents[c]) >> 24) & 1)

#endif /* NEW_SYNTAX */

/* This array, indexed by a character, contains the syntax code which that
 character signifies (as a char).  For example,
 (enum syntaxcode) syntax_spec_code['w'] is Sword. */

extern unsigned char syntax_spec_code[0400];

/* Indexed by syntax code, give the letter that describes it. */

extern unsigned char syntax_code_spec[13];
