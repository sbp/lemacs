/* Random utilities used by X.
   Copyright (C) 1991, 1992 Free Software Foundation, Inc.

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
#include "lisp.h"
#include "xterm.h"
#include "dispextern.h"
#include "screen.h"

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
  *x = (ix - (ibw + licw)) / FONT_WIDTH (SCREEN_NORMAL_FACE (s).font);
#else
  *x = (ix - ibw) / FONT_WIDTH (SCREEN_NORMAL_FACE (s).font);
#endif
  *y = (iy - ibw) / (s->display.x->text_height + x_interline_space);
}
