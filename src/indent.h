/* Definitions for interface to indent.c
   Copyright (C) 1985, 1986, 1992, 1993 Free Software Foundation, Inc.

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

#ifndef _EMACS_INDENT_H_
#define _EMACS_INDENT_H_

struct position
  {
    long bufpos;		/* Buffer position */
    int hpos;			/* Horizontal window position */
    int vpos;			/* Vertical window position */
    long prevhpos;		/* Horizontal window position of last char */
    int contin;			/* 0 of not continued, 1 if continued */
    /* hpos in pixels */
    int pixpos;
    /* hpos in buffer (not window) columns */
    int column;
  };

struct position *compute_motion (struct window *w,
                                 int from, int fromvpos, int fromhpos,
                                 int to, int tovpos, int tohpos, 
                                 int hscroll, 
                                 int tab_offset);
struct position *vmotion (int from, 
                          int vtarget,
                          int hscroll,
                          Lisp_Object window);

extern void invalidate_current_column (void);

#endif /* _EMACS_INDENT_H_ */
