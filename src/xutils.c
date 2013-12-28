/* Random utilities used by X.
   Copyright (C) 1991 Free Software Foundation, Inc.

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
#include "xterm.h"
#include "dispextern.h"
#include "screen.h"

XFontStruct *
x_new_font (name)
     register Lisp_Object name;
{
  char *fontname = (char *) XSTRING (name)->data;
  XFontStruct *font;

  BLOCK_INPUT;
  font = XLoadQueryFont (x_current_display, fontname);
  UNBLOCK_INPUT;
  if (font == 0)
    error ("Can't load font %s", fontname);

  return font;
}

#define NO_CHAR_INFO(ch) (((ch)->width == 0) &&    \
			  ((ch)->rbearing == 0) && \
			  ((ch)->lbearing == 0))

XCharStruct *
x_char_info (font, ch)
     XFontStruct *font;
     unsigned ch;
{
  if (ch >= font->min_char_or_byte2 && ch <= font->max_char_or_byte2)
    {
      XCharStruct *info = &font->per_char[(ch - font->min_char_or_byte2)];
      
      if (!NO_CHAR_INFO (info))
	return info;
    }
  {
    int default_index = font->default_char;
    int limit_index = font->max_char_or_byte2 - font->min_char_or_byte2;

    /* Lucid fix? */
    if ((default_index >= 0) && (default_index < limit_index))
      return &font->per_char[default_index];
    return &font->per_char[0];
  }
}

void
x_read_mouse_position (s, x, y)
     struct screen *s;
     int *x, *y;
{
  Window w;
  int ix, iy;
  int ibw = s->display.x->internal_border_width;
#ifdef LINE_INFO_COLUMN
  int licw = s->display.x->line_info_column_width;
#endif

  Window root_window;
  int root_x, root_y;
  unsigned int keys_and_buttons;

  BLOCK_INPUT;
  if (XQueryPointer (x_current_display, XtWindow (s->display.x->edit_widget),
       	      &root_window, &w, &root_x, &root_y, &ix, &iy,
       	      &keys_and_buttons) == False)
    {
      UNBLOCK_INPUT;
      error ("Pointer not on same screen as window.");
    }
  UNBLOCK_INPUT;

#ifdef LINE_INFO_COLUMN
  *x = (ix - (ibw + licw)) / FONT_WIDTH (s->display.x->font);
#else
  *x = (ix - ibw) / FONT_WIDTH (s->display.x->font);
#endif
  *y = (iy - ibw) / (s->display.x->text_height + x_interline_space);
}

/* Draw a pixmap specified by IMAGE_DATA of dimensions WIDTH and HEIGHT
   on the screen S at position X, Y. */

void
x_draw_pixmap (s, x, y, image_data, width, height)
     struct screen *s;
     int x, y, width, height;
     char *image_data;
{
#if 0
  Pixmap image;

  image = XCreateBitmapFromData (x_current_display,
				 XtWindow (s->display.x->edit_widget),
				 image_data,
				 width, height);
  XCopyPlane (x_current_display, image, XtWindow (s->display.x->edit_widget),
	      s->display.x->normal_gc, 0, 0, width, height, x, y);
#endif
}

/* Draw a rectangle on the screen with left top corner including
   the character specified by LEFT_CHAR and TOP_CHAR.  The rectangle is
   CHARS by LINES wide and long and is the color of the cursor. */

void
x_rectangle (s, gc, left_char, top_char, chars, lines)
     register struct screen *s;
     GC gc;
     register int top_char, left_char, chars, lines;
{
  int width;
  int height;
  int left = (left_char * FONT_WIDTH (s->display.x->font)
#ifdef LINE_INFO_COLUMN
	      + s->display.x->internal_border_width
	      + s->display.x->line_info_column_width);
#else
		    + s->display.x->internal_border_width);
#endif
  int top = (top_char *  (s->display.x->text_height + x_interline_space)
		   + s->display.x->internal_border_width);

  if (chars < 0)
    width = FONT_WIDTH (s->display.x->font) / 2;
  else
    width = FONT_WIDTH (s->display.x->font) * chars;
  if (lines < 0)
    height = FONT_HEIGHT (s->display.x->font) / 2;
  else
    height = FONT_HEIGHT (s->display.x->font) * lines;

  XDrawRectangle (x_current_display,
		  XtWindow (s->display.x->edit_widget),
		  gc, left, top, width, height);
}
