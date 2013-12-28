/* Things for GLYPHS and glyph tables.
   Copyright (C) 1990, 1992, 1993 Free Software Foundation, Inc.

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

#ifndef _EMACS_DISPTAB_H_
#define _EMACS_DISPTAB_H_

/* Access the slots of a display-table, according to their purpose.  */

#define DISP_TABLE_SIZE 261
#define DISP_TRUNC_GLYPH(dp) ((dp)->contents[256])
#define DISP_CONTINUE_GLYPH(dp) ((dp)->contents[257])
#define DISP_ESCAPE_GLYPH(dp) ((dp)->contents[258])
#define DISP_CTRL_GLYPH(dp) ((dp)->contents[259])
#define DISP_INVIS_ROPE(dp) ((dp)->contents[260])
#define DISP_CHAR_ROPE(dp, c) ((dp)->contents[c])

extern struct Lisp_Vector *buffer_display_table (struct buffer *);
extern struct Lisp_Vector *window_display_table (struct window *);

/* Display table to use for vectors that don't specify their own.  */
extern Lisp_Object Vstandard_display_table;

/* Vector of GLYPH definitions.  Indexed by GLYPH number,
   the contents are a string which is how to output the GLYPH.  */
extern Lisp_Object Vglyph_table;

/* Return the current length of the GLYPH table,
   or 0 if the table isn't currently valid.  */
#define GLYPH_TABLE_LENGTH  \
  ((VECTORP (Vglyph_table)) ? XVECTOR (Vglyph_table)->size : 0)

/* Return the current base (for indexing) of the GLYPH table,
   or 0 if the table isn't currently valid.  */
#define GLYPH_TABLE_BASE  \
  ((VECTORP (Vglyph_table)) ? XVECTOR (Vglyph_table)->contents : 0)

/* Given BASE and LEN returned by the two previous macros,
   return nonzero if the GLYPH code G should be output as a single
   character with code G.  Return zero if G has a string in the table.  */
#define GLYPH_SIMPLE_P(base,len,g)  \
  ((g) >= (len) || !STRINGP (base[g]))

/* Given BASE and LEN returned by the two previous macros,
   return nonzero if GLYPH code G is aliased to a different code.  */
#define GLYPH_ALIAS_P(base,len,g)  \
  ((g) < (len) && FIXNUMP (base[g]))

/* Assuming that GLYPH_SIMPLE_P (BASE, LEN, G) is 1,
   return the alias for G.  */
#define GLYPH_ALIAS(base, g) XINT (base[g])

/* Assuming that GLYPH_SIMPLE_P (BASE, LEN, G) is 0,
   return the length and the address of the character-sequence
   used for outputting GLYPH G.  */
#define GLYPH_LENGTH(base,g)   XSTRING (base[g])->size
#define GLYPH_STRING(base,g)   XSTRING (base[g])->data

/* GLYPH for a space character.  */

#define TABGLYPH '\t'
#define SPACEGLYPH 040
#define NULL_GLYPH 00
#define INVISIBLE_GLYPH ((GLYPH) '>')

#define GLYPH_FROM_CHAR(c) ((GLYPH) (c))

extern int glyphlen ();
extern void str_to_glyph_cpy ();
extern void str_to_glyph_ncpy ();
extern void glyph_to_str_cpy ();
extern int run_from_glyph_index ();

#endif /* _EMACS_DISPTAB_H_ */
