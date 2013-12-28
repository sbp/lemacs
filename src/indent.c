/* Indentation functions.
   Copyright (C) 1985, 1986, 1987, 1988, 1990 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
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
#include "extents.h"
#include "indent.h"
#include "screen.h"
#include "window.h"
#include "termchar.h"
#include "termopts.h"
#include "disptab.h"
#ifdef HAVE_X_WINDOWS
#include "xterm.h"
#endif

/* Indentation can insert tabs if this is non-zero;
   otherwise always uses spaces */
int indent_tabs_mode;

#define min(a, b) ((a) < (b) ? (a) : (b))
#define max(a, b) ((a) > (b) ? (a) : (b))

#define CR 015

/* Avoid recalculation by remembering things in these variables. */

/* Last value returned by current_column.

   Some things set last_known_column_point to -1
   to mark the memoized value as invalid */
int last_known_column;

/* Last buffer searched by current_column */
struct buffer *last_known_column_buffer;

/* Value of point when current_column was called */
int last_known_column_point;

/* Value of MODIFF when current_column was called */
int last_known_column_modified;

extern int minibuf_prompt_width;

extern int minibuf_prompt_pix_width;

/* Get the display table to use for the current buffer.  */

struct Lisp_Vector *
buffer_display_table (struct buffer* buffer)
{
  Lisp_Object thisbuf;

  thisbuf = buffer->display_table;
  if (XTYPE (thisbuf) == Lisp_Vector
      && XVECTOR (thisbuf)->size == DISP_TABLE_SIZE)
    return XVECTOR (thisbuf);

  if (XTYPE (Vstandard_display_table) == Lisp_Vector
      && XVECTOR (Vstandard_display_table)->size == DISP_TABLE_SIZE)
    return XVECTOR (Vstandard_display_table);

  return 0;
}

static int
list_length (list)
     Lisp_Object list;
{
  int length = 0;

  while (XTYPE (list) == Lisp_Cons)
    {
      length++;
      list = XCONS (list)->cdr;
    }
  return length;
}

int
current_column ()
{
  register struct buffer *buffer = current_buffer;
  struct Lisp_Vector *dp = buffer_display_table (buffer);
  register int col;
  register int tab_seen;
  register int tab_width = XINT (buffer->tab_width);
  register int stop, pos;
  int post_tab;
  int stopchar;
  struct glyphs_from_chars *displayed_glyphs;

  if (buffer == last_known_column_buffer
      && BUF_PT (buffer) == last_known_column_point
      && BUF_MODIFF (buffer) == last_known_column_modified)
    return last_known_column;

  pos = BUF_PT (buffer);
  col = tab_seen = post_tab = 0;

  while (1)
    {
      register int width;

      if (pos == BUF_BEGV (buffer))
	break;

      pos--;
      if (BUF_CHAR_AT (buffer, pos) == '\t')
      {
	if (tab_seen)
	    col = ((col + tab_width) / tab_width) * tab_width;

	  post_tab += col;
	  col = 0;
	  tab_seen = 1;
	}
      else if (BUF_CHAR_AT (buffer, pos) == '\n')
	break;
      else
	{
	  displayed_glyphs = glyphs_from_bufpos (selected_screen, buffer, pos,
						 dp, 0, col, 0, 0, 0);
	  col += (displayed_glyphs->columns
		  - (displayed_glyphs->begin_columns
		     + displayed_glyphs->end_columns));
	}
    }

  if (tab_seen)
    {
      col = ((col + tab_width) / tab_width) * tab_width;
      col += post_tab;
    }

  last_known_column_buffer = buffer;
  last_known_column = col;
  last_known_column_point = BUF_PT (buffer);
  last_known_column_modified = BUF_MODIFF (buffer);

  return col;
}

DEFUN ("current-column", Fcurrent_column, Scurrent_column, 0, 0, 0,
  "Return the horizontal position of point.  Beginning of line is column 0.\n\
This is calculated by adding together the widths of all the displayed\n\
representations of the character between the start of the previous line\n\
and point.  (eg control characters will have a width of 2 or 4, tabs\n\
will have a variable width)\n\
Ignores finite width of screen, which means that this function may return\n\
values greater than (screen-width).\n\
Whether the line is visible (if `selective-display' is t) has no effect;\n\
however, ^M is treated as end of line when `selective-display' is t.")
  ()
{
  Lisp_Object temp;
  XFASTINT (temp) = current_column ();
  return temp;
}

DEFUN ("indent-to", Findent_to, Sindent_to, 1, 2, "NIndent to column: ",
  "Indent from point with tabs and spaces until COLUMN is reached.\n\
Optional second argument MIN says always do at least MIN spaces\n\
even if that goes past COLUMN; by default, MIN is zero.")
  (col, minimum)
     Lisp_Object col, minimum;
{
  int mincol;
  register int fromcol;
  register int tab_width = XINT (current_buffer->tab_width);
  int opoint = 0;
  EXTENT extent = extent_at (point, current_buffer, EF_INVISIBLE);

  CHECK_NUMBER (col, 0);
  if (NILP (minimum))
    XFASTINT (minimum) = 0;
  CHECK_NUMBER (minimum, 1);

  fromcol = current_column ();
  mincol = fromcol + XINT (minimum);
  if (mincol < XINT (col)) mincol = XINT (col);

  if (fromcol == mincol)
    return make_number (mincol);

  if (tab_width <= 0 || tab_width > 20) tab_width = 8;

  
  if (extent)
    {
      int last_visible = last_visible_position (point, current_buffer);
      opoint = point;

      if (last_visible >= BEGV)
	SET_PT (last_visible);
      else 
        error ("Visible portion of buffer not modifiable");
    }

  if (indent_tabs_mode)
    {
      Lisp_Object n;
      XFASTINT (n) = mincol / tab_width - fromcol / tab_width;
      if (XFASTINT (n) != 0)
	{
	  Finsert_char (make_number ('\t'), n);

	  fromcol = (mincol / tab_width) * tab_width;
	}
    }

  XFASTINT (col) = mincol - fromcol;
  Finsert_char (make_number (' '), col);

  last_known_column_buffer = current_buffer;
  last_known_column = mincol;
  last_known_column_point = point;
  last_known_column_modified = MODIFF;

  if (opoint > 0)
    SET_PT (opoint);

  XSETINT (col, mincol);
  return col;
}

int
position_indentation (pos)
     register int pos;
{
  register int col = 0;
  register int c;
  register int end = ZV;
  register int tab_width = XINT (current_buffer->tab_width);

  if (extent_at (pos, current_buffer, EF_INVISIBLE))
    return 0;

  if (tab_width <= 0 || tab_width > 20) tab_width = 8;

  while (pos < end &&
	 (c = CHAR_AT (pos),
	  c == '\t' ? (col += tab_width - col % tab_width)
          : (c == ' ' ? ++col : 0)))
    pos++;

  return col;
}

DEFUN ("current-indentation", Fcurrent_indentation, Scurrent_indentation,
  0, 0, 0,
  "Return the indentation of the current line.\n\
This is the horizontal position of the character\n\
following any initial whitespace.")
  ()
{
  Lisp_Object val;

  XFASTINT (val) = position_indentation (find_next_newline (point, -1));
  return val;
}

DEFUN ("move-to-column", Fmove_to_column, Smove_to_column, 1, 2, 0,
  "Move point to column COLUMN in the current line.\n\
The column of a character is calculated by adding together the widths\n\
as displayed of the previous characters in the line.\n\
This function ignores line-continuation;\n\
there is no upper limit on the column number a character can have\n\
and horizontal scrolling has no effect.\n\n\
If specified column is within a character, point goes after that character.\n\
If it's past end of line, point goes to end of line.\n\n\
A non-nil second (optional) argument FORCE means, if the line\n\
is too short to reach column COLUMN then add spaces/tabs to get there,\n\
and if COLUMN is in the middle of a tab character, change it to spaces.")
  (column, force)
     Lisp_Object column, force;
{
  register int pos;
  register int col = current_column ();
  register int goal;
  register int end;
  register int tab_width = XINT (current_buffer->tab_width);
  register int ctl_arrow = !NILP (current_buffer->ctl_arrow);
  register struct Lisp_Vector *dp = buffer_display_table (current_buffer);

  Lisp_Object val;
  int prev_col;
  int c;

  if (tab_width <= 0 || tab_width > 20) tab_width = 8;
  CHECK_NATNUM (column, 0);
  goal = XINT (column);

 retry:
  pos = point;
  end = ZV;

  /* If we're starting past the desired column,
     back up to beginning of line and scan from there.  */
  if (col > goal)
    {
      pos = find_next_newline (pos, -1);
      col = 0;
    }

  while (col < goal && pos < end)
    {
      c = CHAR_AT (pos);
      if (c == '\n')
	break;
      if (c == '\r' && EQ (current_buffer->selective_display, Qt))
	break;
      pos++;
      if (c == '\t')
	{
	  prev_col = col;
	  col += tab_width;
	  col = col / tab_width * tab_width;
	}
      else if (dp != 0 && XTYPE (DISP_CHAR_ROPE (dp, c)) == Lisp_String)
	col += XSTRING (DISP_CHAR_ROPE (dp, c))->size / sizeof (GLYPH);
      else if (ctl_arrow && (c < 040 || c == 0177))
        col++;
      else if (c < 040 || c >= 0177)
        col += 3;
      else
	col++;
    }

  SET_PT (pos);

  /* If a tab char made us overshoot, change it to spaces
     and scan through it again.  */
  if (!NILP (force) && col > goal && c == '\t' && prev_col < goal)
    {
      del_range (point - 1, point);
      Findent_to (make_number (col - 1), 0);
      insert_char (' ');
      goto retry;
    }

  /* If line ends prematurely, add space to the end.  */
  if (col < goal && !NILP (force))
    Findent_to (make_number (col = goal), 0);

  last_known_column_buffer = current_buffer;
  last_known_column = col;
  last_known_column_point = point;
  last_known_column_modified = MODIFF;

  XFASTINT (val) = col;
  return val;
}

struct position val_compute_motion;

/* Note that `cpos' is CURRENT_VPOS << SHORTBITS + CURRENT_HPOS,
   and that CURRENT_HPOS may be negative.  Use these macros
   to extract the hpos or the vpos from cpos or anything like it. */
#ifndef SHORT_CAST_BUG
#define HPOS(VAR) (short) (VAR)
#else
#define HPOS(VAR) (((VAR) & (1 << (SHORTBITS - 1)) \
		    ? ~((1 << SHORTBITS) - 1) : 0) \
		   | (VAR) & ((1 << SHORTBITS) - 1))
#endif /* SHORT_CAST_BUG */

#define VPOS(VAR) (((VAR) >> SHORTBITS) + (HPOS (VAR) < 0))

#ifdef HAVE_X_WINDOWS
#define CHECK_PIX_WIDTH (SCREEN_IS_TERMCAP (screen) || pix_width < max_width)
#ifdef LINE_INFO_COLUMN
#define RESET_PIX_WIDTH (pix_width = screen->display.x->line_info_column_width)
#else
#define RESET_PIX_WIDTH (pix_width = 0)
#endif
#define INC_PIX_WIDTH (pix_width += displayed_glyphs->pixel_width)
#define PIX_VALUES 1
#define CHECK_PIX_OVERFLOW (SCREEN_IS_X (screen)			\
			    && pix_width >= max_width			\
			    && BUF_CHAR_AT (buffer, pos + 1) != '\n')
#else  /* not X */
#define CHECK_PIX_WIDTH 1
#define RESET_PIX_WIDTH
#define INC_PIX_WIDTH
#define PIX_VALUES 0
#define CHECK_PIX_OVERFLOW 0
#endif /* not X */

#ifdef LINE_INFO_COLUMN
#define INFO_COLUMN_ADJUST(cpos) (cpos)++
#define INFO_COLUMNS(cpos) (HPOS(cpos) - 1)
#else
#define INFO_COLUMN_ADJUST(cpos)
#define INFO_COLUMNS(cpos) (HPOS(cpos))
#endif

/* Marker for where to display an arrow on top of the buffer text.  */
Lisp_Object Voverlay_arrow_position;

/* String to display for the arrow.  */
Lisp_Object Voverlay_arrow_string;

/* New version of compute motion. */

struct position *
compute_motion (window, from, fromvpos, fromhpos, to,
		tovpos, tohpos, width, hscroll, tab_offset, hpos_pix_width,
		column)
     int from, fromvpos, fromhpos, to, tovpos, tohpos, hpos_pix_width, column;
     register int width;
     int hscroll, tab_offset;
     Lisp_Object window;
{
  struct buffer *buffer = XBUFFER (XWINDOW (window)->buffer);
  struct Lisp_Vector *dp = buffer_display_table (buffer);
  SCREEN_PTR screen = XSCREEN (WINDOW_SCREEN (XWINDOW (window)));
  SCREEN_PTR s = screen;
  struct window *w = XWINDOW (window);
#ifdef LINE_INFO_COLUMN
  int cpos = fromhpos + (fromvpos << SHORTBITS) + 1;
#else
  int cpos = fromhpos + (fromvpos << SHORTBITS);
#endif
  register int target = tohpos + (tovpos << SHORTBITS);
  register UCHAR c;
  register int pos;
  int prevcpos, pix_width, nextcpos;
  struct glyphs_from_chars *displayed_glyphs, *newline_glyphs;
  int overlay_arrow_seen = 0;

#ifdef HAVE_X_WINDOWS
  int truncate = hscroll
    || (truncate_partial_width_windows
	&& XFASTINT (w->width) < SCREEN_WIDTH (s))
      || !NILP (current_buffer->truncate_lines);
  register int max_width =
    (SCREEN_IS_X (screen) ?
     MAX_LINE_WIDTH (screen) - EOL_CURSOR_WIDTH
     : 0);
  /* max_truncated_or_continued_width is widest possible line that is
     truncated or continued */
  int max_truncated_or_continued_width =
        (SCREEN_IS_X (screen) ?
	 (truncate
	  ? TRUNCATE_WIDTH (screen)
	  : CONTINUE_WIDTH (screen))
	 : max_width);
  int position_before_continuer = 0;
  int cpos_before_continuer;
#endif

  pix_width = hpos_pix_width;
  pos = from;
  prevcpos = cpos;

  while (1)
    {
      /* There are no glyphs at this position */
      displayed_glyphs = 0;
      if (pos == ZV)
	break;

      /* Deal with the overlay arrow. */
      if (XTYPE (Voverlay_arrow_position) == Lisp_Marker
	  && current_buffer == XMARKER (Voverlay_arrow_position)->buffer
	  && (pos == BEGV
	      || BUF_CHAR_AT (buffer, pos - 1) == '\n')
	  && pos == marker_position (Voverlay_arrow_position)
	  && XTYPE (Voverlay_arrow_string) == Lisp_String
	  && ! overlay_arrow_seen)
	{
	  cpos++;
	  pix_width += x_bitmaps[RARROW_BITMAP].width;
	  overlay_arrow_seen = 1;
	}

      c = BUF_CHAR_AT (buffer, pos);
      /* Increment columns by the number of glyphs at this buffer position,
	 and increment pix_width by the pixel_width of these glyphs. */
      displayed_glyphs = glyphs_from_bufpos (screen, buffer, pos, dp,
					     hscroll, column,
					     tab_offset, 0, 0);
      nextcpos = cpos + displayed_glyphs->columns;
      cpos = nextcpos;
      column += displayed_glyphs->columns - (displayed_glyphs->begin_columns
						 + displayed_glyphs->end_columns);
      INC_PIX_WIDTH;
      
      /* If we've reached the continuer position,
	 take note of the pos and cpos before this position. */

      if (!position_before_continuer
	  && pix_width > max_truncated_or_continued_width)
	    {
	      position_before_continuer = pos - 1;
	      cpos_before_continuer = prevcpos;
	    }

	  /* If we've reached the maximum position, go back to the continuer
	     position and continue on the next line starting with the position
	     immediately following position_before_continuer */
	  if (pix_width > max_truncated_or_continued_width)
	    {
	      /* Exceeded max_width, go to the next line, starting at
		 position_before_continuer. */
	      prevcpos = cpos_before_continuer;
	      pos = position_before_continuer + 1;

	      if (truncate)
		{
		  while (pos < to
			 && BUF_CHAR_AT (buffer, pos) != '\n')
		    pos++;
		  if (BUF_CHAR_AT (buffer, pos) == '\n')
		    {
		      displayed_glyphs = 0;
		      nextcpos = cpos + (1 << SHORTBITS) - HPOS (cpos);
		      nextcpos -= hscroll;
		      INFO_COLUMN_ADJUST (nextcpos);
		      if (hscroll > 0)
			nextcpos++;	/* Count the ! on column 0 */
		      tab_offset = 0;
		      column = 0;
		      RESET_PIX_WIDTH;
		      position_before_continuer = 0;
		    }
		  if (pos < to || BUF_CHAR_AT (buffer, pos) == '\n')
		    continue;
		}
	      else
		{
		  cpos += (1 << SHORTBITS) - HPOS (cpos);
		  cpos -= hscroll;
		  INFO_COLUMN_ADJUST (cpos);
		  if (hscroll > 0)
		    cpos++;	/* Count the ! on column 0 */
		  tab_offset = 0;
		  column = 0;
		  RESET_PIX_WIDTH;
		  position_before_continuer = 0;
		  continue;
		}
	    }
      if (c == '\n')
	{
	  /* If the character is a newline, go to the next display line
	     positioned at the beginning after any extras like the info
	     column and the hscroll ! indicator */
	  displayed_glyphs = 0;	/* Don't adjust cursor for end glyphs on EOL */
	  nextcpos = cpos + (1 << SHORTBITS) - HPOS (cpos);
	  nextcpos -= hscroll;
	  INFO_COLUMN_ADJUST (nextcpos);

	  if (hscroll > 0)
	    nextcpos++;		/* Count the ! on column 0 */
	  tab_offset = 0;
	  RESET_PIX_WIDTH;
	  column = 0;
	  /* continued_from_position = 0; */
	  position_before_continuer = 0;
	}

      if (pos >= to || cpos >= target)
	{
	  /* We've found the buffer position we want, but we need to
	     be sure we aren't in the line wrap zone. */
	  if (position_before_continuer)
	    {
	      /* If we're in the line wrap zone, we must look ahead to
		 see if this position is on this line or the next line.
		 If a line break or end of buffer occurs before max_width,
		 it's on this line, otherwise it's on the next line. */
	      int look_c;
	      int lookahead_pos = pos;
	      int lookahead_pix_width = pix_width;
	      struct glyphs_from_chars *lookahead_glyphs;

	      while (1)
		{
		  lookahead_pos++;
		  if (lookahead_pos > ZV)
		    break;
		  look_c = BUF_CHAR_AT (buffer, lookahead_pos);
		  if (look_c == '\n')
		    /* Found a newline, so we stay on this line */
		    break;
		  lookahead_glyphs = glyphs_from_bufpos (screen, buffer,
							 lookahead_pos, dp,
							 hscroll,
							 INFO_COLUMNS (cpos),
							 tab_offset, 0, 0);
		  lookahead_pix_width += lookahead_glyphs->pixel_width;
		  if (lookahead_pix_width > max_truncated_or_continued_width)
		    {
		      /* Exceeded max_width, go to the next line, starting at
			 position_before_continuer. */
		      prevcpos = cpos_before_continuer;
		      pos = position_before_continuer + 1;

		      if (truncate)
			{
			  while (pos < to
				 && BUF_CHAR_AT (buffer, pos) != '\n')
			    pos++;
			  if (pos >= to)
			    cpos = cpos_before_continuer + 2;
			  /* Prevent adjustment after breaking */
			  displayed_glyphs = 0;
			}
		      else
			{
			  cpos += (1 << SHORTBITS) - HPOS (cpos);
			  cpos -= hscroll;
			  INFO_COLUMN_ADJUST (cpos);
			  if (hscroll > 0)
			    cpos++; /* Count the ! on column 0 */
			  tab_offset = 0;
			  RESET_PIX_WIDTH;
			  column = 0;
			  position_before_continuer = 0;
			}
		      break;
		    }
		}
	      if (pos >= to)
		break;
	    }
	  else
	    {
	      /* We aren't in the line wrap zone, so we're done.
		 prevpos points to the previous position. */
	      break;
	    }
	}
      else
	{
	  prevcpos = (displayed_glyphs
		      ? (cpos - (displayed_glyphs->columns
				 - displayed_glyphs->begin_columns))
		      : cpos);
	  cpos = nextcpos;
	  pos++;
	}
    }

  /* cpos now points to the character following this position, so we must
     subtract all but the begin glyphs to get this position.  If this
     position is at a newline or in the truncation zone, there are no glyphs
     and cpos points to this position and needs no adjustment. */
  if (displayed_glyphs)
    {
      cpos -= (displayed_glyphs->columns
	       - displayed_glyphs->begin_columns);
      pix_width -= displayed_glyphs->pixel_width;
      column -= (displayed_glyphs->columns
		 - (displayed_glyphs->begin_columns + displayed_glyphs->end_columns));
    }

  val_compute_motion.bufpos = pos;
  val_compute_motion.hpos = HPOS (cpos);
  val_compute_motion.vpos = VPOS (cpos);
  val_compute_motion.prevhpos = HPOS (prevcpos);
  val_compute_motion.pixpos = pix_width;
  val_compute_motion.column = column;

  /* Nonzero if have just continued a line */
  val_compute_motion.contin = ((pos != from
				&& (val_compute_motion.vpos != VPOS (prevcpos))
				&& c != '\n')
			       ? (pos - 1)
			       : 0);

  return &val_compute_motion;
}


#undef HPOS
#undef VPOS

DEFUN ("motion", Fmotion, Smotion, 3, 3, 0,
  "Move forward from point by N characters.  Stop if we reach\n\
TOHPOS, TOVPOS first.")
  (n, tohpos, tovpos)
     Lisp_Object n, tohpos, tovpos;
{
  int fromhpos = 0;
  int fromvpos = SCREEN_CURSOR_Y (selected_screen);
  int width = XFASTINT (XWINDOW (selected_window)->width);
  int hscroll = XINT (XWINDOW (selected_window)->hscroll);
  struct position *pos;

  pos = compute_motion (selected_window,
			point, fromvpos, fromhpos, (point + n),
			XINT (tovpos), XINT (tohpos),
			width, hscroll,
			pos_tab_offset (XWINDOW (selected_window), point),
			0, 0);
  SET_PT (pos->bufpos);
  return Fcons (make_number (pos->vpos),
		Fcons (make_number (pos->hpos), Qnil));
}

/* Return the column of position POS in window W's buffer,
   rounded down to a multiple of the internal width of W.
   This is the amount of indentation of position POS
   that is not visible in its horizontal position in the window.  */

int
pos_tab_offset (w, pos)
     struct window *w;
     register int pos;
{
  return 0;
#if 0
  int opoint = point;
  int col;
  int width = XFASTINT (w->width) - 1
    - (XFASTINT (w->width) + XFASTINT (w->left)
       != SCREEN_WIDTH (XSCREEN (w->screen)));
  EXTENT extent = extent_at (pos, current_buffer, EF_INVISIBLE);

  if ((pos == BEGV) || (CHAR_AT (pos - 1) == '\n') || extent)
    return 0;

  SET_PT (pos);
  col = current_column ();
  SET_PT (opoint);
  return col - (col % width);
#endif
}

/* start_hpos is the hpos of the first character of the buffer:
   zero except for the minibuffer window,
   where it is the width of the prompt.  */

struct position val_vmotion;

struct position *
vmotion (from, vtarget, width, hscroll, window)
     register int from, vtarget, width;
     int hscroll;
     Lisp_Object window;
{
  struct position pos;
  /* vpos is cumulative vertical position, changed as from is changed */
  register int vpos = 0;
  register int prevline;
  register int first;
  int lmargin = hscroll > 0 ? 1 - hscroll : 0;
  int selective
    = XTYPE (current_buffer->selective_display) == Lisp_Int
      ? XINT (current_buffer->selective_display)
	: !NILP (current_buffer->selective_display) ? -1 : 0;
  int start_hpos = (EQ (window, minibuf_window) ? minibuf_prompt_width : 0);
  int start_pixpos = (EQ (window, minibuf_window) ? minibuf_prompt_pix_width : 0);
  

 retry:
  if (vtarget > vpos)
    {
      /* Moving downward is simple, but must calculate from beg of line 
	 to determine hpos of starting point */
      if (from > BEGV && CHAR_AT (from - 1) != '\n')
	{
	  prevline = find_next_newline (from, -1);
	  while (selective > 0
		 && prevline > BEGV
		 && position_indentation (prevline) >= selective)
	    prevline = find_next_newline (prevline - 1, -1);
	  pos = *compute_motion (window,
				 prevline, 0,
				 lmargin + (prevline == 1 ? start_hpos : 0),
				 from, 10000, 10000,
				 width, hscroll, 0, start_pixpos, 0);
	}
      else
	{
	  pos.hpos = lmargin + (from == 1 ? start_hpos : 0);
	  pos.pixpos = (from == 1 ? start_pixpos : 0);
	  pos.column = 0;
	  pos.vpos = 0;
	}
      return compute_motion (window,
			     from, vpos, pos.hpos,
			     ZV, vtarget, - (1 << (SHORTBITS - 1)),
			     width, hscroll, pos.vpos * width,
			     pos.pixpos, pos.column);
    }

  /* To move upward, go a line at a time until
     we have gone at least far enough */

  first = 1;

  while ((vpos > vtarget || first) && from > BEGV)
    {
      prevline = from;
      while (1)
	{
	  prevline = find_next_newline (prevline - 1, -1);
	  if (prevline == BEGV
	      || selective <= 0
	      || position_indentation (prevline) < selective)
	    break;
	}
      pos = *compute_motion (window,
			     prevline, 0,
			     lmargin + (prevline == 1 ? start_hpos : 0),
			     from, 10000, 10000,
			     width, hscroll, 0, start_pixpos, 0);
      vpos -= pos.vpos;
      first = 0;
      from = prevline;
    }

  /* If we made exactly the desired vertical distance,
     or if we hit beginning of buffer,
     return point found */
  if (vpos >= vtarget)
    {
      val_vmotion.bufpos = from;
      val_vmotion.vpos = vpos;
      val_vmotion.hpos = lmargin;
      val_vmotion.pixpos = 0;
      val_vmotion.column = 0;
      val_vmotion.contin = 0;
      val_vmotion.prevhpos = 0;
      return &val_vmotion;
    }
  
  /* Otherwise find the correct spot by moving down */
  goto retry;
}

DEFUN ("vertical-motion", Fvertical_motion, Svertical_motion, 1, 2, 0,
  "Move to start of screen line LINES lines down.\n\
If LINES is negative, this is moving up.\n\
Sets point to position found; this may be start of line\n\
 or just the start of a continuation line.\n\
Returns number of lines moved; may be closer to zero than LINES\n\
 if beginning or end of buffer was reached.\n\
Optional second argument is WINDOW to move in.")
  (lines, window)
     Lisp_Object lines, window;
{
  if (NILP (window))
    window = selected_window;
  {
    struct position pos;
    register struct window *w  = XWINDOW (window);
    int width = XFASTINT (w->width) - 1
      - (XFASTINT (w->width) + XFASTINT (w->left)
	 != SCREEN_WIDTH (XSCREEN (w->screen)));

    CHECK_NUMBER (lines, 0);

    pos = *vmotion (point, XINT (lines), width,
		    XINT (w->hscroll), window);

    SET_PT (pos.bufpos);
    return make_number (pos.vpos);
  }
}

syms_of_indent ()
{
  DEFVAR_BOOL ("indent-tabs-mode", &indent_tabs_mode,
    "*Indentation can insert tabs if this is non-nil.\n\
Setting this variable automatically makes it local to the current buffer.");
  indent_tabs_mode = 1;

  defsubr (&Scurrent_indentation);
  defsubr (&Sindent_to);
  defsubr (&Scurrent_column);
  defsubr (&Smove_to_column);
  defsubr (&Svertical_motion);
  defsubr (&Smotion);
}
