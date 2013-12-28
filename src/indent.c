/* Indentation functions.
   Copyright (C) 1985, 1986, 1987, 1988, 1992, 1993 
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
#include "buffer.h"
#include "extents.h"
#include "insdel.h"
#include "indent.h"
#include "screen.h"
#include "window.h"
#include "termchar.h"
#include "termopts.h"
#include "disptab.h"
#include "dispmisc.h"
#include "faces.h"
#ifdef HAVE_X_WINDOWS
#include "xterm.h"
#include "xobjs.h"
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
static int last_known_column;

/* Last buffer searched by current_column */
static struct buffer *last_known_column_buffer;

/* Value of point when current_column was called */
static int last_known_column_point;

/* Value of MODIFF when current_column was called */
static int last_known_column_modified;

/* Get the display table to use for the current buffer.  */

struct Lisp_Vector *
buffer_display_table (struct buffer* buffer)
{
#if 0 /* >>> Unused */
  Lisp_Object thisbuf;

  thisbuf = buffer->display_table;
  if (VECTORP (thisbuf)
      && XVECTOR (thisbuf)->size == DISP_TABLE_SIZE)
    return XVECTOR (thisbuf);

  if (VECTORP (Vstandard_display_table)
      && XVECTOR (Vstandard_display_table)->size == DISP_TABLE_SIZE)
    return XVECTOR (Vstandard_display_table);
#endif

  return 0;
}

#if 0
static int
list_length (list)
     Lisp_Object list;
{
  int length = 0;

  while (CONSP (list))
    {
      length++;
      list = XCONS (list)->cdr;
    }
  return length;
}
#endif

/* Cancel any recorded value of the horizontal position.  */
 
void
invalidate_current_column ()
{
  last_known_column_point = -1;
}

int
current_column ()
{
  register struct buffer *buffer = current_buffer;
  struct Lisp_Vector *dp = buffer_display_table (buffer);
  register int col;
  register int tab_seen;
  register int tab_width = XINT (buffer->tab_width);
  register int pos;
  int post_tab;
  struct glyphs_from_chars *displayed_glyphs;

  if (buffer == last_known_column_buffer
      && BUF_PT (buffer) == last_known_column_point
      && BUF_MODIFF (buffer) == last_known_column_modified)
    return last_known_column;

  pos = BUF_PT (buffer);
  col = tab_seen = post_tab = 0;

  while (1)
    {
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
  return (make_number (current_column ()));
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
  EXTENT extent = extent_at (PT, current_buffer, Qinvisible);

  CHECK_FIXNUM (col, 0);
  if (NILP (minimum))
    minimum = Qzero;
  else
    CHECK_FIXNUM (minimum, 1);

  fromcol = current_column ();
  mincol = fromcol + XINT (minimum);
  if (mincol < XINT (col)) mincol = XINT (col);

  if (fromcol == mincol)
    return make_number (mincol);

  if (tab_width <= 0 || tab_width > 20) tab_width = 8;

  
  if (extent)
    {
      int last_visible = last_visible_position (PT, current_buffer);
      opoint = PT;

      if (last_visible >= BEGV)
	SET_PT (last_visible);
      else 
        error (GETTEXT ("Visible portion of buffer not modifiable"));
    }

  if (indent_tabs_mode)
    {
      int n = mincol / tab_width - fromcol / tab_width;
      if (n != 0)
	{
	  Finsert_char (make_number ('\t'), make_number (n));

	  fromcol = (mincol / tab_width) * tab_width;
	}
    }

  Finsert_char (make_number (' '), make_number (mincol - fromcol));

  last_known_column_buffer = current_buffer;
  last_known_column = mincol;
  last_known_column_point = PT;
  last_known_column_modified = MODIFF;

  if (opoint > 0)
    SET_PT (opoint);

  return (make_number (mincol));
}

int
position_indentation (buf, pos)
     register struct buffer *buf;
     register int pos;
{
  register int col = 0;
  register int c;
  register int end = BUF_ZV(buf);
  register int tab_width = XINT (buf->tab_width);

  if (extent_at (pos, buf, Qinvisible))
    return 0;

  if (tab_width <= 0 || tab_width > 20) tab_width = 8;

  while (pos < end &&
	 (c = BUF_CHAR_AT (buf,pos),
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
  return (make_number 
	  (position_indentation (current_buffer,
				 find_next_newline (current_buffer, PT, -1))));
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
  register struct Lisp_Vector *dp = buffer_display_table (current_buffer);
  struct glyphs_from_chars *displayed_glyphs;

  int prev_col;
  int c;

  if (tab_width <= 0 || tab_width > 20) tab_width = 8;
  CHECK_NATNUM (column, 0);
  goal = XINT (column);

 retry:
  pos = PT;
  end = ZV;

  /* If we're starting past the desired column,
     back up to beginning of line and scan from there.  */
  if (col > goal)
    {
      pos = find_next_newline (current_buffer, pos, -1);
      col = 0;
    }

  while (col < goal && pos < end)
    {
      c = CHAR_AT (pos);
      if (c == '\n')
	break;
      if (c == '\r' && EQ (current_buffer->selective_display, Qt))
	break;
      if (c == '\t')
	{
	  prev_col = col;
	  col += tab_width;
	  col = col / tab_width * tab_width;
	}
      else
	{
	  displayed_glyphs = glyphs_from_bufpos (selected_screen,
						 current_buffer, pos,
						 dp, 0, col, 0, 0, 0);
	  col += (displayed_glyphs->columns
		  - (displayed_glyphs->begin_columns
		     + displayed_glyphs->end_columns));
	}

      pos++;
    }

  SET_PT (pos);

  /* If a tab char made us overshoot, change it to spaces
     and scan through it again.  */
  if (!NILP (force) && col > goal && c == '\t' && prev_col < goal)
    {
      del_range (PT - 1, PT);
      Findent_to (make_number (col - 1), Qzero);
      insert_char (' ');
      goto retry;
    }

  /* If line ends prematurely, add space to the end.  */
  if (col < goal && !NILP (force))
    {
      col = goal;
      Findent_to (make_number (col), Qzero);
    }

  last_known_column_buffer = current_buffer;
  last_known_column = col;
  last_known_column_point = PT;
  last_known_column_modified = MODIFF;

  return (make_number (col));
}

static struct position val_compute_motion;

#define CHECK_NEXT(s) {\
  a[0] = (s);\
  wid = text_width (lfont,a,1);\
  if ((pixpos + wid) > pwidth) {\
    if (truncate) {\
      while (bufpos < to && BUF_CHAR_AT(b,bufpos) != '\n') bufpos++;\
      bufpos--;\
    } else {\
      vpos++; hpos = 0; pixpos = 0;\
      vpixpos += max_line_height;\
      max_line_height = font->height;\
      new_line = 1;\
      tab_offset += window_char_width(w) - 1;\
      if (vpos > tovpos || (vpos == tovpos && hpos >= tohpos))\
        goto foundpos;\
      hpos = 1; pixpos = wid;\
    }\
  } else {\
    hpos++; pixpos += wid;\
  }\
}

struct position *
compute_motion (struct window *w, int from, int fromvpos, int fromhpos,
		int to,int tovpos,int tohpos,int hscroll, int tab_offset)
{
  int hpos = fromhpos;
  int vpos = fromvpos;
  int bufpos;
  int pixpos = 0;
  int vpixpos, max_line_height, new_line;
  int prevpos, prevhpos, prevvpos;
  int savehpos, savevpos, savepos;
#ifdef I18N4
  wchar_t c = 0;
  wchar_t c1;
  int wid;
  wchar_t a[2];
#else
  int c = 0;
  int c1,wid;
  unsigned char a[2];
#endif
  int flag = 0;
  int loop;

  struct screen *s = XSCREEN (w->screen);
  struct buffer *b = XBUFFER (w->buffer);
  int e_start, e_end;
  struct face *cur_face, *e_face;
  struct Lisp_Font *font;
  Lisp_Object lfont;
  struct glyphs_from_chars *display_info;

  int tab_width = XFASTINT (b->tab_width);
  Lisp_Object ctl_arrow = b->ctl_arrow;
  int ctl_p = !NILP (ctl_arrow);
  int printable_min = (FIXNUMP (ctl_arrow)
		       ? XINT (ctl_arrow)
		       : ((EQ (ctl_arrow, Qt) || EQ (ctl_arrow, Qnil))
			  ? 256 : 160));
  int truncate = hscroll || !NILP(b->truncate_lines)
    || (truncate_partial_width_windows && w->pixwidth < PIXW(s));
  int selective = FIXNUMP(b->selective_display)
    ? XINT (b->selective_display)
      : !NILP (b->selective_display) ? -1 : 0;
  int pwidth = w->pixwidth - LEFT_MARGIN (b, s, w) - RIGHT_MARGIN (b, s, w);
  int pheight = (((window_char_height(w) - 1) == tovpos)
		 ? (w->pixheight
		    - (MINI_WINDOW_P (w)
		       ? 0
		       : XFONT (SCREEN_DEFAULT_FONT (s))->height))
		 : 100000);
  int char_width;

  /* don't run off the end of the buffer */
  if (to > BUF_ZV(b))
    to = BUF_ZV(b);

  if (tab_width <= 0 || tab_width > 20) tab_width = 8;

  vpixpos = vpos * XFONT (SCREEN_DEFAULT_FONT (s))->height;

  display_info =
    glyphs_from_bufpos (s,b,from,0,hscroll,0,tab_offset,0,1);

  e_face = display_info->faceptr;
  e_start = display_info->run_pos_lower;
  e_end = display_info->run_pos_upper;

  if (e_face)
    {
      /* Set font according to the extent's font (if any) */
      cur_face = e_face;
      if (!NILP (FACE_FONT (e_face)))
  	lfont = FACE_FONT (e_face);
      else
	lfont = SCREEN_DEFAULT_FONT (s);
    }
  else
    lfont = SCREEN_DEFAULT_FONT (s);

  font = XFONT (lfont);
  /* Skip any textual glyphs. */
  for (loop = 0; loop < 2; loop++)
    {
      int col_cnt;
      Lisp_Object *class;

      col_cnt = loop ? display_info->begin_columns
	: display_info->end_columns;
      class = loop ? display_info->begin_class
	: display_info->end_class;

      if (col_cnt)
	{
	  int i;
	  for (i = 0; i < col_cnt; i++)
	    {
	      struct extent *e = XEXTENT(class[i]);

	      if (EXTENT_GLYPH_LAYOUT_P (e, GL_TEXT))
		pixpos += glyph_width (extent_glyph (e), lfont);
	    }
	}
    }

  max_line_height = font->height;
  new_line = 1;

  if (w == XWINDOW(minibuf_window) && minibuf_prompt_width)
#ifdef I18N4
    {
      safe_mbstowcs ((char *) XSTRING (Vminibuf_prompt)->data, &wc_buf);
      pixpos = text_width(lfont, wc_buf.data, wc_buf.in_use + 1);
    }
#else
    pixpos = text_width(lfont, XSTRING(Vminibuf_prompt)->data,
			XSTRING(Vminibuf_prompt)->size) + 1;
#endif

  /* Adjust right logical border of window to account for
     truncate/continue glyph */
  pwidth -= (truncate
	     ? glyph_width (truncator_glyph, lfont)
	     : glyph_width (continuer_glyph, lfont));

  prevpos = from;
  for (bufpos = from; bufpos < to; bufpos++)
    {
      if (vpos > tovpos || (vpos == tovpos && hpos >= tohpos))
	break;
      if (vpixpos + max_line_height > pheight)
	{
	  flag = 1;
	}

      prevvpos = vpos; prevhpos = hpos;

      if (bufpos == e_start || bufpos == e_end || e_start == -1)
	{
	  display_info =
	    glyphs_from_bufpos (s,b,bufpos,0,hscroll,0,tab_offset,0,1);

	  e_face = display_info->faceptr;
	  e_start = display_info->run_pos_lower;
	  e_end = display_info->run_pos_upper;

	  if (e_start == e_end)
	    e_start = e_end = -1;

	  if (e_face)
	    {
	      /* Set font according to the extent's font (if any) */
	      cur_face = e_face;
	      if (!NILP (FACE_FONT (e_face)))
		lfont = FACE_FONT (e_face);
	      else
		lfont = SCREEN_DEFAULT_FONT (s);
	    }
	  else
	    lfont = SCREEN_DEFAULT_FONT (s);
	  font = XFONT (lfont);
	  if (new_line)
	    max_line_height = font->height;
	  else
	    max_line_height = max ((int) font->height, max_line_height);

	  /* Skip any textual glyphs. */
	  for (loop = 0; loop < 2; loop++)
	    {
	      int col_cnt;
	      Lisp_Object *class;

	      col_cnt = loop ? display_info->begin_columns
		: display_info->end_columns;
	      class = loop ? display_info->begin_class
		: display_info->end_class;

	      if (col_cnt)
		{
		  int i;
		  for (i = 0; i < col_cnt; i++)
		    {
		      struct extent *e = XEXTENT(class[i]);

		      if (EXTENT_GLYPH_LAYOUT_P (e, GL_TEXT))
			pixpos += glyph_width (extent_glyph (e), lfont);
		    }
		}
	    }
	}

      c = BUF_CHAR_AT(b,bufpos);
      a[0] = c;
      char_width = text_width (lfont, a, 1);

      if (c >= printable_min && char_width)
	{
	  CHECK_NEXT(c);
	}
      else if (c == '\t')
	{
	  int i;
	  int inc = tab_width -
	    ((hpos + tab_offset + hscroll - (hscroll > 0)
	      /* Add tab_width here to make sure positive.
		 hpos can be negative after continuation but
		 can't be less than -tab_width. */
	      + tab_width)
	      % tab_width);
	  for (i = 0; i < inc; i++)
	    CHECK_NEXT(' ');
	}
      else if (c == '\n')
	{
	  if (selective > 0 && 
	      position_indentation(b, bufpos + 1) >= selective)
	    {
	      /* Skip invisible lines */
	      do
		{
		  while (++bufpos < to && BUF_CHAR_AT(b,bufpos) != '\n');
		}
	      while (selective > 0
		     && position_indentation(b, bufpos + 1) >= selective);
	      bufpos--;
	      /* Allow for ' ...' */
	      if (!NILP(current_buffer->selective_display_ellipses))
		{
		  int i;
		  CHECK_NEXT(' ');
		  for (i = 0; i < 3; i++)
		    CHECK_NEXT('.');
		}
	    }
	  else
	    {
	      vpos++;
	      prevpos = bufpos;
	      vpixpos += max_line_height;
	      max_line_height = font->height;
	      new_line = 1;
	      pixpos = hpos = 0;
	    }
	  hpos -= hscroll;
	  if (hscroll > 0)
	    {
	      CHECK_NEXT('$');
	    }
	  tab_offset = 0;
	}
      else if (c == CR && selective < 0)
	{
	  while (bufpos < to && BUF_CHAR_AT(b,bufpos) != '\n') bufpos++;
	  bufpos--;
	  /* Allow for ' ...' */
	  if (!NILP(current_buffer->selective_display_ellipses))
	    {
	      int i;
	      CHECK_NEXT(' ');
	      for (i = 0; i < 3; i++)
		CHECK_NEXT('.');
	    }
	}
      else if (char_width && ((c < 040 && ctl_p) || c == 0177))
	{
	  /* Insert control character as a 2-char sequence */
	  CHECK_NEXT('^');
	  if (c == 0177)
	    {
	      CHECK_NEXT ('?');
	    }
#ifdef I18N4
	  else if (!((c) < 040 || (c) == 0177))
	    {
	      CHECK_NEXT(c);
	    }
#endif
	  else
	    {
	      CHECK_NEXT(c ^ 0100);
	    }
	}
      else if (c >= 0200 || c < 040 || !char_width)
	{
	  /* Insert control character as a 4-char sequence */
	  CHECK_NEXT('\\');
	  CHECK_NEXT((c>>6) + '0');
	  CHECK_NEXT((7 & (c>>3)) + '0');
	  CHECK_NEXT((7 & c) + '0');
	}
      else
	{
	  CHECK_NEXT(c);
	}
      new_line = 0;
    }

  savevpos = vpos;
  savehpos = hpos;
  savepos = bufpos;

  /* If at end of buffer/narrowed region, don't fetch next character as
   * it isn't there.  Fake it with last character of buffer.
   */
  c1 = ((bufpos < BUF_ZV(b)) ? BUF_CHAR_AT(b,bufpos) : c);
  if (c1)
    CHECK_NEXT(c1);
  if (vpos != savevpos && savepos < BUF_ZV(b) && (c1 != '\n') && !truncate)
    {
      vpos = savevpos + 1; hpos = 0;
    }
  else
    {
      vpos = savevpos; hpos = savehpos;
    }
  bufpos = savepos;

 foundpos:
  val_compute_motion.bufpos = bufpos;
  val_compute_motion.hpos = hpos;
  val_compute_motion.vpos = vpos;
  val_compute_motion.prevhpos = prevhpos;

  val_compute_motion.contin
    = bufpos != from && (val_compute_motion.vpos != prevvpos) && c != '\n';

  if (flag)
    {
      val_compute_motion.bufpos = prevpos;
/*      vpos--; hpos = 0; bufpos = prevpos; */
    }

  return &val_compute_motion;
}


#undef HPOS
#undef VPOS
#undef CHECK_NEXT


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

DEFUN ("motion", Fmotion, Smotion, 3, 3, 0,
  "Move forward from point by N characters.  Stop if we reach\n\
TOHPOS, TOVPOS first.")
  (n, tohpos, tovpos)
     Lisp_Object n, tohpos, tovpos;
{
  int fromhpos = 0;
  int fromvpos = SCREEN_CURSOR_Y (selected_screen);
  int hscroll = XINT (XWINDOW (selected_window)->hscroll);
  struct position *pos;

  CHECK_FIXNUM (n, 0);
  CHECK_FIXNUM (tohpos, 1);
  CHECK_FIXNUM (tovpos, 2);

  pos = compute_motion (XWINDOW (selected_window),
			PT, fromvpos, fromhpos, (PT + XINT (n)),
			XINT (tovpos), XINT (tohpos), hscroll,
			pos_tab_offset (XWINDOW (selected_window), PT));
  SET_PT (pos->bufpos);
  return Fcons (make_number (pos->vpos),
		Fcons (make_number (pos->hpos), Qnil));
}

/* start_hpos is the hpos of the first character of the buffer:
   zero except for the minibuffer window,
   where it is the width of the prompt.  */

static struct position val_vmotion;

struct position *
vmotion (from, vtarget, hscroll, window)
     register int from, vtarget;
     int hscroll;
     Lisp_Object window;
{
  struct buffer *buf = XBUFFER (XWINDOW (window)->buffer);
  struct position pos;
  /* vpos is cumulative vertical position, changed as from is changed */
  register int vpos = 0;
  register int width = window_char_width (XWINDOW (window));
  register int prevline;
  register int first;
  int lmargin = hscroll > 0 ? 1 - hscroll : 0;
  int selective
    = FIXNUMP (buf->selective_display)
      ? XINT (buf->selective_display)
	: !NILP (buf->selective_display) ? -1 : 0;
  int start_hpos = (EQ (window, minibuf_window) ? minibuf_prompt_width : 0);

 retry:
  if (vtarget > vpos)
    {
      /* Moving downward is simple, but must calculate from beg of line 
	 to determine hpos of starting point */
      if (from > BUF_BEGV(buf) && BUF_CHAR_AT (buf, from - 1) != '\n')
	{
	  prevline = find_next_newline (buf, from, -1);
	  while (selective > 0
		 && prevline > BUF_BEGV(buf)
		 && position_indentation (buf, prevline) >= selective)
	    prevline = find_next_newline (buf, prevline - 1, -1);
	  pos = *compute_motion (XWINDOW (window),
				 prevline, 0,
				 lmargin + (prevline == 1 ? start_hpos : 0),
				 from, 10000, 10000,
				 hscroll, 0);
	}
      else
	{
	  pos.hpos = lmargin + (from == 1 ? start_hpos : 0);
	  pos.vpos = 0;
	}
      return compute_motion (XWINDOW (window),
			     from, vpos, pos.hpos,
			     BUF_ZV(buf), vtarget, - (1 << (SHORTBITS - 1)),
			     hscroll, (pos.vpos * width));
    }

  /* To move upward, go a line at a time until
     we have gone at least far enough */

  first = 1;

  while ((vpos > vtarget || first) && from > BUF_BEGV(buf))
    {
      prevline = from;
      while (1)
	{
	  prevline = find_next_newline (buf, prevline - 1, -1);
	  if (prevline == BUF_BEGV(buf)
	      || selective <= 0
	      || position_indentation (buf, prevline) < selective)
	    break;
	}
      pos = *compute_motion (XWINDOW (window),
			     prevline, 0,
			     lmargin + (prevline == 1 ? start_hpos : 0),
			     from, 10000, 10000,
			     hscroll, 0);
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
  CHECK_WINDOW (window, 0);
  {
    struct position pos;
    register struct window *w  = XWINDOW (window);

    CHECK_FIXNUM (lines, 0);

    pos = *vmotion (BUF_PT (XBUFFER (w->buffer)), XINT (lines),
		    XINT (w->hscroll), window);

    /* Note that the buffer's point is set, not the window's point. */
    SET_BUF_PT (XBUFFER (w->buffer), pos.bufpos);

    return make_number (pos.vpos);
  }
}


void
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
