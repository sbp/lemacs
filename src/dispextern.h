/* Interface definitions for display code.
   Copyright (C) 1985, 1992, 1993 Free Software Foundation, Inc.

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

#ifndef _EMACS_DISPEXTERN_H_
#define _EMACS_DISPEXTERN_H_

#ifdef emacs
/* Nonzero means don't assume anything about current
   contents of actual terminal screen */
extern int screen_garbaged;

/* Nonzero means last display completed and cursor is really at
   cursX, cursY.  Zero means it was preempted. */
extern int display_completed;

/* non-nil if a buffer has changed since the last time redisplay completed */
extern int buffers_changed;

/* non-nil if any extent has changed since the last time redisplay completed */
extern int extents_changed;

/* non-nil if any face has changed since the last time redisplay completed */
extern int faces_changed;

/* non-nil if any window has changed since the last time redisplay completed */
extern int windows_changed;

#ifdef HAVE_X_WINDOWS
#include <X11/Xlib.h>
#endif

#endif /* emacs */

#define GLYPH_CLASS_VECTOR_SIZE 100

#ifdef emacs

struct glyphs_from_chars
  {
#ifdef I18N4
    wchar_t c;
#else
    UCHAR c;
#endif
    GLYPH *glyphs;
    int columns;
    struct face *faceptr;
    Lisp_Object begin_class[GLYPH_CLASS_VECTOR_SIZE];
    Lisp_Object end_class[GLYPH_CLASS_VECTOR_SIZE];
    short n_nonfont;
    int next_visible;
    UCHAR begin_or_end;		/* 0:none, 1:end, 2:begin, 3:both */
#ifdef HAVE_X_WINDOWS
    int begin_columns;
    int begin_pixel_width;
    int end_columns;
    int end_pixel_width;
    int pixel_width;
    int pixel_height;
#endif
    /* The chars in the buffer being processed that lie between
       [run_pos_lower, run_pos_upper] are promised to be in the same run,
       and to be separated from begin and/or end glyphs, provided they fit
       on the same line. This means that there are NO pixel-maps 
       (sometimes called glyphs in other contexts) or font changes in the 
       display of the chars in this range. It is NOT promised to be a 
       maximal such range. */
    int run_pos_lower;
    int run_pos_upper;
  };

#endif /* emacs */

#ifdef HAVE_X_WINDOWS
struct glyph_dimensions
  {
    int top_left_x;
    int top_left_y;
    int width;
    int height;
    int run;
  };
#endif /* HAVE_X_WINDOWS */

#ifdef emacs

extern int in_display;

extern struct glyph_dimensions *get_glyph_dimensions (void);

struct char_block
{
  struct char_block *next;	/* Next charblock */
  struct char_block *prev;
  /* #### these should be lisp objects eventually */
  struct face *face;		/* Style for display */
  struct extent *e;		/* Used to associate annotations with glyphs. */
  GLYPH glyph;
  short xpos;			/* Start pos (x) */
  short width;			/* Pixel width of character */
#ifdef I18N4
  wchar_t ch;			/* character at this pos */  
#else
  unsigned char ch;		/* character at this pos */  
#endif
  unsigned int char_b	:1;	/* true for characters, false for glyphs */
  unsigned int blank	:1;	/* true if the width should be blanked */
  unsigned int new	:1;
  unsigned int changed	:1;
};

struct line_header
{
  struct line_header *next,*prev; /* Next line */
  short ypos;			/* Baseline pos for line */
  short prevy;			/* Previous y position */
  unsigned short ascent, descent; /* Max ascent,descent for line */
  short chars;			/* Number of chars on this line */
  short lwidth;			/* Line width (pixels) */
  short mwidth;			/* Margin width (pixels) */
  short in_display;
  struct char_block *body;	/* Chars on this line */
  struct char_block *end;	/* Ptr to last char/dummy on this line */
  struct char_block *margin_start;	/* Chars in extra margin space */
  struct char_block *margin_end;	/* Ptr to last char/dummy in margin */
  /* Flags used in redisplay process */
  unsigned int modeline	:1;  
  unsigned int changed	:1;
  unsigned int shifted	:1;
  unsigned int new	:1;
  unsigned int tabs	:1;
  unsigned int subwindow :1;	/* subwindows need special handling */
};

struct redisplay_block
{
  union
    {
      struct line_update
	{
	  struct line_header *l;	/* line containing block */
	  struct char_block *cb;	/* start of block */
	  struct char_block *end;       /* end of block */
	  char clear_line;		/* clear to end of line? */
	} line;
      struct clear_region
	{
	  int top,bottom;		/* start,end of region to clear. */
	} area;
      struct blit_region
	{
	  struct line_header *l1; /* start */
	  struct line_header *l2; /* end */
	  short old_top;
	} blit;
    } block;
  char type;			/* type of redisplay block */  
  char done;
};

#define BODY_LINE 1
#define MARGIN_LINE 4
#define AREA 2
#define BLIT 3

#define BODY	0
#define MARGIN	1

#define BLOCK_DONE(x)		(display_list[x].done)
#define BLOCK_TYPE(x)  		(display_list[x].type)
#define BLOCK_LINE(x)		(display_list[x].block.line.l)
#define BLOCK_LINE_START(x)	(display_list[x].block.line.cb)
#define BLOCK_LINE_END(x)	(display_list[x].block.line.end)
#define BLOCK_LINE_CLEAR(x)	(display_list[x].block.line.clear_line)
#define BLOCK_AREA_TOP(x)	(display_list[x].block.area.top)
#define BLOCK_AREA_BOTTOM(x)	(display_list[x].block.area.bottom)
#define BLOCK_BLIT_START(x)	(display_list[x].block.blit.l1)
#define BLOCK_BLIT_END(x)	(display_list[x].block.blit.l2)
#define BLOCK_BLIT_OLD_TOP(x)	(display_list[x].block.blit.old_top)

struct layout_data
{
  Lisp_Object lfont;
  struct face *face;
  struct char_block *cb;
  struct char_block *prev;
  struct char_block *beginc;
  struct char_block *endc;
  struct char_block *new_cpos;
  int border;
  int pixright;
  int lwidth;
  struct glyphs_from_chars *display_info;
  unsigned short asc, old_asc;
  unsigned short desc, old_desc;
  int text_glyphs_added;
  int text_glyph_width;
  int pos;
};

struct pixel_translation_cache
{
  int valid;
  struct screen *screen;
  unsigned int lowx, highx, x;
  unsigned int lowy, highy, y;
  struct window *w;
  int bufp;
  Lisp_Object class;
  int retval;
};

extern struct pixel_translation_cache t_g_cache;

/* >>> This decl should really live somewhere else. */
extern struct glyphs_from_chars *glyphs_from_bufpos
     (struct screen *screen, struct buffer *buffer, struct window *window,
      int pos, struct Lisp_Vector *dp,
      int hscroll, register int columns,
      int tab_offset, int direction, int only_nonchars);

#endif /* emacs */

#endif /* _EMACS_DISPEXTERN_H_ */
