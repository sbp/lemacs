/* X Communication module for terminals which understand the X protocol.
   Copyright (C) 1989, 1991, 1992, 1993 Free Software Foundation, Inc.

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

#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xos.h>
#include <X11/Xmu/Error.h>
#include <X11/IntrinsicP.h>	/* CoreP.h needs this */
#include <X11/CoreP.h>		/* foul, but we need this to use our own
				   window inside a widget instead of one 
				   that Xt creates... */
#include "EmacsShellP.h"
#include "ScreenWidget.h"

#include "hash.h"

extern XtAppContext Xt_app_con;
extern Widget Xt_app_shell;

#ifdef HAVE_X_WINDOWS

/* On 4.3 this loses if it comes after xterm.h.  */
#include <signal.h>

/* This may include sys/types.h, and that somehow loses
   if this is not done before the other system files.  */
#include "xterm.h"

/* Load sys/types.h if not already loaded.
   In some systems loading it twice is suicidal.  */
#ifndef makedev
#include <sys/types.h>
#endif

#ifdef O_NDELAY
#undef O_NDELAY
#endif
#include <fcntl.h>
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <setjmp.h>
#ifdef BSD
#include <sys/ioctl.h>
#include <strings.h>
#else
#include <sys/termio.h>
#include <string.h>
#endif
#include <sys/stat.h>
#include <sys/param.h>

#include "termhooks.h"
#include "termopts.h"
#include "termchar.h"
#include "buffer.h"
#include "screen.h"
#include "disptab.h"
#include "window.h"
#include "bitmaps.h"

#include "events.h"

extern void emacs_Xt_make_event_stream ();


#define min(a,b) ((a)<(b) ? (a) : (b))
#define max(a,b) ((a)>(b) ? (a) : (b))
#define focus_cursor_p(s) \
	(((s) == selected_screen) && ((s)->display.x->focus_p))


/* Nonzero after BLOCK_INPUT; prevents input events from being
   processed until later.  */
int x_input_blocked;

#if defined (SIGIO) && defined (FIONREAD)
int BLOCK_INPUT_mask;
#endif


/* Stuff for dealing with the title. */
extern Lisp_Object Vinvocation_name;
static char *hostname, *id_name;

/* This is the X connection that we are using.  */
Display *x_current_display;


/* File descriptor of the X connection.  Used in sysdep.c to set the
   X connection in SIGIO mode.  */
int x_file_descriptor;

/* Screen being updated by update_screen.
   
   This is set by XTupdate_begin and looked at by all the
   XT functions.  It is zero while not inside an update.
   In that case, the XT functions assume that `selected_screen'
   is the screen to apply to.  */
static struct screen *updating_screen;

/* During an update, maximum vpos which ins/del of lines
   may affect.  This is  specified by higher levels.

   This is the number of lines, from the top of screen downwards,
   which can participate in insert-line/delete-line operations.

   Effectively it excludes the bottom screen_height - specified_window_size
   lines from those operations.  */
static int update_height;

/* Number of pixels below each line. */
int x_interline_space;

/* Tells if a window manager is present or not. */
extern Lisp_Object Vx_no_window_manager;

/* Timestamp that we requested selection data was made. */
extern Time requestor_time;

/* ID of the window requesting selection data. */
extern Window requestor_window;

/* Nonzero enables some debugging for the X interface code. */
extern int _Xdebug;

extern int errno;

extern Lisp_Object Vglobal_minibuffer_screen;

extern Display *XOpenDisplay ();
extern Window XCreateWindow ();

extern Cursor XCreateCursor ();
extern FONT_TYPE *XOpenFont ();

void
x_lower_screen (struct screen* s);
void
x_make_screen_visible (struct screen *s);

void
x_raise_screen (struct screen* s, int force);

#ifdef LINE_INFO_COLUMN
void x_draw_lineinfo_border(), x_clear_lineinfo_glyph();
#endif

extern struct screen *
x_any_window_to_screen (Window);

static int x_display_cursor ();

/* Remember if the last cursor displayed was a bar or a box */
static Lisp_Object last_bar_cursor;

extern Lisp_Object Vbar_cursor;


static void
erase_screen_cursor_if_needed (s)
     struct screen *s;
{
  if (!s->cursor_erased)
    {
      s->cursor_erased = 1;
      s->cursor_erased =
	x_display_cursor (s, 0);
    }
}

static void
erase_cursor_if_needed ()
{
  erase_screen_cursor_if_needed (updating_screen
				 ? updating_screen
				 : selected_screen);
}

#define UP_TO_EOL ~0

static void
erase_screen_cursor_if_needed_and_in_region (s, x, y, n)
     struct screen *s;
     int x, y, n;
{
  if (!s->cursor_erased && s->phys_cursor_y == y
      && x <= s->phys_cursor_x
      && (n == UP_TO_EOL || s->phys_cursor_x <= x + n))
    {
      s->cursor_erased = 1;
      s->cursor_erased =
	x_display_cursor (s, 0);
    }
}

static void
draw_screen_cursor_if_needed (s)
     struct screen *s;
{
  if (s->cursor_erased)
    {
      s->cursor_erased =
	!x_display_cursor (s, 1);
    }
}

/* These hooks are called by update_screen at the beginning and end
   of a screen update.  We record in `updating_screen' the identity
   of the screen being updated, so that the XT... functions do not
   need to take a screen as argument.  Most of the XT... functions
   should never be called except during an update, the only exceptions
   being XTcursor_to, and XTwrite_glyphs.  */

static void
XTupdate_begin (s)
     struct screen *s;
{	
  updating_screen = s;
  update_height = s->height;

  if (last_bar_cursor != Vbar_cursor)
    s->cursor_erased = x_display_cursor (s, 0);

#ifdef ENERGIZE
  BLOCK_INPUT;
#if 0
 *  xc_begin_note_update (s->display.x->edit_widget);
#endif
  UNBLOCK_INPUT;
#endif
}

extern Lisp_Object text_part_sym;
extern Lisp_Object modeline_part_sym;

enum window_type
{
  scrollbar_window,
  text_window
};

/* Nonzero when emacs is garbage-collecting. */
extern int gc_in_progress;

static void
XTupdate_end (s)
     struct screen *s;
{	
  BLOCK_INPUT;
#ifdef ENERGIZE
#if 0
 *  xc_flush_note_update (s->display.x->edit_widget);
#endif
#endif
  /*  adjust_scrollbars (s);*/

  draw_screen_cursor_if_needed (s);
  updating_screen = 0;

  XFlushQueue ();
  UNBLOCK_INPUT;
}

/* Set the nominal cursor position of the screen:
   where display update commands will take effect.
   This does not affect the place where the cursor-box is displayed.  */

static void
XTcursor_to (row, col)
     int row, col;
{
  struct screen* s = updating_screen ? updating_screen : selected_screen;

  if (s->phys_cursor_x != col || s->phys_cursor_y != row)
    {
      if (updating_screen == 0)
	{
	  erase_screen_cursor_if_needed (s);
	  s->cursor_x = col;
	  s->cursor_y = row;
	  s->phys_cursor_x = col;
	  s->phys_cursor_y = row;
	  draw_screen_cursor_if_needed (s);
	  BLOCK_INPUT;
	  XFlushQueue ();
	  UNBLOCK_INPUT;
	}
      else
	{	
	  erase_screen_cursor_if_needed (s);
	  s->phys_cursor_x = col;
	  s->phys_cursor_y = row;
	}
    }
}



#ifdef ENERGIZE
static void
notify_note_update (Widget parent, int x, int y, int width, int height)
{
#if 0
 *  disable_strict_free_check ();
 *  BLOCK_INPUT;
 *  xc_note_xywh_updated (parent, x, y, width, height);
 *  UNBLOCK_INPUT;
 *  enable_strict_free_check ();
#endif
}
#endif /* ENERGIZE */


static int really_abort;
static void
maybe_abort ()
{
  if (really_abort) abort();
}

int
x_write_glyphs (s, left, top, n, force_gc, box_p)
     struct screen *s;
     int left, top, n;
     GC force_gc;
     int box_p;
{
  Widget widget = s->display.x->edit_widget;
  Window x_win = XtWindow (widget);
  int pix_x = s->current_glyphs->top_left_x[top];
  int pix_y = s->current_glyphs->top_left_y[top];
#if 0
  int line_height = (s->current_glyphs->pix_height[top]);
#else
  int line_height = s->display.x->text_height;
#endif
  int max_ascent = s->current_glyphs->max_ascent[top];
  struct run *face_list = s->current_glyphs->face_list[top];
  int this_run;
  int run_len;
  int this_run_len, this_pix_len;
  XFontStruct *this_font;
  GC drawing_gc;
  GLYPH *gp;
  char *buf;
  char *cp;

  /* don't do anything if nothing needs to be drawn */
  if (n <= 0)
    return 0;

  if (top >= s->height
      || top < 0
      || s->current_glyphs->used[top] == 0
      || s->current_glyphs->enable[top] == 0
      || left >= s->current_glyphs->used[top]
      || n < 0
      || !face_list)
    /* calling abort () here maks emacs19 unusable under Energize.
       I try it in a mode where the bug is plainly ignored by returning 
       instead */
    return 0;

  erase_screen_cursor_if_needed_and_in_region (s, left, top, n);

  buf = (char *) alloca (s->current_glyphs->used[top] + 1);
  cp = buf;

  /* Advance to the correct run. */
  this_run = run_len = 0;
  while (left >= run_len + face_list[this_run].length)
    {
      run_len += face_list[this_run].length;
      pix_x += face_list[this_run].pix_length;
      this_run++;

      /* check for end of runs */
      if (this_run >= s->current_glyphs->nruns [top])
	return 0;
    }

  /* If we're starting at the beginning of the line, don't skip over the
     lineinfo run. */
#if 0
 *  if (this_run > 0 &&
 *      left == 0 &&
 *      /* !force_gc && */
 *      face_list[this_run-1].type == column_glyph &&
 *      pix_x == s->current_glyphs->top_left_x[top])
 *    this_run--;
#endif

  this_run_len = face_list[this_run].length;
  this_pix_len = face_list[this_run].pix_length;
  this_font = face_list[this_run].faceptr->font;
  if (! this_font)
    this_font = SCREEN_NORMAL_FACE (s).font;

  BLOCK_INPUT;

/*
  if (this_run == 0 || this_run == 1) 
    x_clear_lineinfo_glyph (s, top);
*/

  if (force_gc)
    {
      drawing_gc = force_gc;
      XSetFont (x_current_display, drawing_gc, this_font->fid);
    }
  else
    if (! (drawing_gc = face_list[this_run].faceptr->facegc))
      if (face_list[this_run].faceptr->hilited)
	drawing_gc = s->display.x->reverse_gc;
      else
	drawing_gc = s->display.x->normal_gc;

  /* If we're starting in the middle of a run, get to the proper offset. */
  if (face_list[this_run].type == font && left > run_len)
    {
      int len = left - run_len;
      int l = len;

      gp = &s->current_glyphs->glyphs[top][run_len];
      cp = buf;
      while (l--)
	if (*gp == TABGLYPH)
	  {
	    *cp++ = ' ';
	    gp++;
	  }
	else
	  *cp++ = 0377 & *gp++;
      this_run_len -= len;
      pix_x += XTextWidth (this_font, buf, len);
    }

   while (1)
    {
      switch (face_list[this_run].type)
	{
	case font:
	  {
	    int len = min (this_run_len, n);
	    int l = len;
	    
	    gp = &s->current_glyphs->glyphs[top][left];
	    cp = buf;
	    while (l--)
	      if (*gp == TABGLYPH)
		{
		  *cp++ = ' ';
		  gp++;
		}
	      else
		*cp++ = 0377 & *gp++;
	    
	    left += len;
	    n -= len;
	    this_run_len -= len;

	    this_pix_len = XTextWidth (this_font, buf, len);

	    if ((this_font->ascent + this_font->descent) < line_height)
	      XClearArea (x_current_display, x_win, pix_x, pix_y, this_pix_len,
			  line_height, False);

	    if (face_list[this_run].faceptr->back_pixmap &&
		face_list[this_run].faceptr->back_pixmap != (~0))
	      {
		if (force_gc)
		  {
		    if (box_p)
		      {		/* The hollow cursor with a stipple */
			XGCValues values;

			if (! XGetGCValues (x_current_display, drawing_gc,
					    GCBackground | GCForeground,
					    &values))
			  abort ();
			XDrawString (x_current_display, x_win,
					  (face_list[this_run].faceptr->facegc
					   ? face_list[this_run].faceptr->facegc
					   : (face_list[this_run].faceptr->hilited
					      ? s->display.x->reverse_gc
					      : s->display.x->normal_gc)),
					  pix_x, pix_y + max_ascent, buf, len);
			XSetForeground (x_current_display, drawing_gc,
					values.background);
			XDrawRectangle (x_current_display, x_win,
					drawing_gc,
					pix_x, pix_y,
					this_pix_len - 1, line_height - 1);
			XSetForeground (x_current_display, drawing_gc,
					values.foreground);
		      }
		    else
		      {		/* The solid cursor with a stipple */
			XGCValues old_values, new_values;

			if (! XGetGCValues (x_current_display, drawing_gc,
					    GCStipple | GCFillStyle,
					    &old_values))
			  abort ();
			if (!XGetGCValues (x_current_display,
					   face_list[this_run].faceptr->facegc,
					   GCStipple | GCFillStyle,
					   &new_values))
			  abort ();
			XSetFillStyle (x_current_display, drawing_gc,
				       FillOpaqueStippled);
			XSetStipple (x_current_display, drawing_gc,
				     new_values.stipple);
			XFillRectangle (x_current_display, x_win, drawing_gc,
					pix_x, pix_y, this_pix_len,
					line_height);
			XSetStipple (x_current_display, drawing_gc,
				     old_values.stipple);
			XSetFillStyle (x_current_display, drawing_gc,
				       old_values.fill_style);
			XDrawString (x_current_display, x_win,
				     drawing_gc,
				     pix_x, pix_y + max_ascent, buf, len);
		      }
		  }
		else
		  {		/* The non cursor stipple with text */
		    XGCValues old_values;

		    if (! XGetGCValues (x_current_display, drawing_gc,
					GCStipple | GCFillStyle, &old_values))
		      abort ();
		    XSetFillStyle (x_current_display, drawing_gc,
				   FillOpaqueStippled);
		    XFillRectangle (x_current_display, x_win, drawing_gc,
				    pix_x, pix_y, this_pix_len, line_height);
		    XSetFillStyle (x_current_display, drawing_gc,
				   old_values.fill_style);
		    XDrawString (x_current_display, x_win,
				 drawing_gc, pix_x, pix_y + max_ascent,
				 buf, len);
		  }
	      }
	    else
	      {
		if (box_p)
		  {		/* The hollow cursor, no stipple */
		    XGCValues values;

		    if (! XGetGCValues (x_current_display, drawing_gc,
					GCBackground | GCForeground,
					&values))
		      abort ();
		    XDrawImageString (x_current_display, x_win,
				      (face_list[this_run].faceptr->facegc
				       ? face_list[this_run].faceptr->facegc
				       : (face_list[this_run].faceptr->hilited
					  ? s->display.x->reverse_gc
					  : s->display.x->normal_gc)),
				      pix_x, pix_y + max_ascent, buf, len);
		    XSetForeground (x_current_display, drawing_gc,
				    values.background);
		    XDrawRectangle (x_current_display, x_win,
				    drawing_gc,
				    pix_x, pix_y,
				    this_pix_len - 1, line_height - 1);
		    XSetForeground (x_current_display, drawing_gc,
				    values.foreground);
		  }
		else		/* Plain or cursor, no stipple */
		  XDrawImageString (x_current_display, x_win,
				    drawing_gc,
				    pix_x,
				    pix_y + max_ascent, buf, len);
	      }

#if 0	/* first stab at implementing boxing; doesn't work well enough yet */

/*	    if (face_list[this_run].faceptr->boxed)  #### */
	    if (face_list[this_run].faceptr->underline)
	      {
		int box_ascent;
		int box_descent;
		unsigned int uthick;
		if (!XGetFontProperty (this_font, XA_STRIKEOUT_ASCENT,
				       (unsigned long *) &box_ascent))
		  box_ascent = max_ascent;
		if (!XGetFontProperty (this_font, XA_STRIKEOUT_DESCENT,
				       (unsigned long *) &box_descent))
		  box_descent = line_height - max_ascent - 1;
		if (!XGetFontProperty (this_font, XA_UNDERLINE_THICKNESS,
				       (unsigned long *) &uthick))
		  uthick = 1;
		/* top line */
		if (face_list[this_run].faceptr->underline && (1<<1))
		  XFillRectangle (x_current_display, x_win, drawing_gc,
				  pix_x, pix_y + max_ascent - box_ascent,
				  this_pix_len, uthick);
		/* bottom line */
		if (face_list[this_run].faceptr->underline && (1<<2))
		  XFillRectangle (x_current_display, x_win, drawing_gc,
				  pix_x, pix_y + max_ascent + box_descent,
				  this_pix_len, uthick);

		/* left line */
#if 0
		if (this_run == 0 ||
		    face_list[this_run-1].type != font ||
		    ! face_list[this_run-1].faceptr->underline) /* #### */
#else
		if (face_list[this_run].faceptr->underline && (1<<3))
#endif
		  XFillRectangle (x_current_display, x_win, drawing_gc,
				  pix_x, pix_y + max_ascent - box_ascent,
				  uthick, box_ascent + box_descent);
		/* right line */
#if 0
		if (this_run >= s->current_glyphs->nruns [top] ||
		    face_list[this_run+1].type != font ||
		    ! face_list[this_run+1].faceptr->underline) /* #### */
#else
		if (face_list[this_run].faceptr->underline && (1<<4))
#endif
		  XFillRectangle (x_current_display, x_win, drawing_gc,
				  pix_x + this_pix_len,
				  pix_y + max_ascent - box_ascent,
				  uthick, box_ascent + box_descent);
	      }

	    else

#endif /* 0 */

	    if (face_list[this_run].faceptr->underline)
	      {
		int upos;
		unsigned int uthick;
		if (!XGetFontProperty (this_font, XA_UNDERLINE_POSITION,
				       (unsigned long *) &upos))
		  upos = 0;
		if (!XGetFontProperty (this_font, XA_UNDERLINE_THICKNESS,
				       (unsigned long *) &uthick))
		  uthick = 1;
		XFillRectangle (x_current_display, x_win, drawing_gc,
				pix_x, pix_y + max_ascent + upos,
				this_pix_len, uthick);
	      }
	  }
	  break;

#ifdef LINE_INFO_COLUMN
	case column_glyph:
	  {
	    int index = face_list[this_run].lineinfo_glyph_index;
	    int width, height;
	    int bitmap_y_offset = 0;
	    int y = pix_y;
	    int ibw = s->display.x->internal_border_width;
	    left += 1;
	    
	    if (index == -1)
	      {
		/* just clears the lineinfo column */
		x_clear_lineinfo_glyph (s, top);
	      }
	    else
	      {
		width = s->display.x->line_info_column_width-(ibw+1);
		height = line_height;
		
		if (!width || !height) 
                  {
                    maybe_abort ();
		    break;
                  }
		
		/* Center the glyph vertically in the display line. */
		if (height < line_height)
		  y += ((line_height - height) / 2);
		else if (height > line_height)
		  {
		    bitmap_y_offset += ((height - line_height) / 2);
		    height -= (height - line_height);
		  }
		
		XCopyPlane (x_current_display,
			    x_bitmaps[index].image,
			    x_win,
			    s->display.x->normal_gc,
			    0, bitmap_y_offset, width, height,
			    s->display.x->internal_border_width+1, y, 1L);
		x_draw_lineinfo_border (s, top);
	      }
	  }
	  break;
#endif

	case glyph:
	  {
	    GLYPH index = s->current_glyphs->glyphs[top][left];
	    struct x_pixmap *p = glyph_to_x_pixmap (index);
	    int height = p->height;
	    int bitmap_y_offset = 0;
	    int y = pix_y;
#ifdef LINE_INFO_WIDGET
	    int use_lineinfo = (s->display.x->lineinfo_widget &&
				XtIsManaged (s->display.x->lineinfo_widget));
#endif
	    int real_pix_x = pix_x;
	    struct face *face = 0;

	    if (!p->width || !p->height)
	      abort ();

	    if (height < line_height || p->mask)
	      {
		if (face_list[this_run].faceptr->back_pixmap &&
		    face_list[this_run].faceptr->back_pixmap != (~0))
		  {
		    XGCValues old_values;
		    if (! XGetGCValues (x_current_display, drawing_gc,
					GCStipple | GCFillStyle, &old_values))
		      abort ();
		    XSetFillStyle (x_current_display, drawing_gc,
				   FillOpaqueStippled);
		    XFillRectangle (x_current_display, x_win, drawing_gc,
				    pix_x, pix_y, this_pix_len, line_height);
		    XSetFillStyle (x_current_display, drawing_gc,
				   old_values.fill_style);
		  }
		else if (face_list[this_run].faceptr->background != ~0 &&
			 face_list[this_run].faceptr->background !=
			 SCREEN_NORMAL_FACE(s).background)
		  {
		    XGCValues old_values;
		    if (! XGetGCValues (x_current_display, drawing_gc,
					GCForeground, &old_values))
		      abort ();
		    XSetForeground (x_current_display, drawing_gc,
				    face_list[this_run].faceptr->background);
		    XFillRectangle (x_current_display, x_win, drawing_gc,
				    pix_x, pix_y, this_pix_len, line_height);
		    XSetForeground (x_current_display, drawing_gc,
				    old_values.foreground);
		  }
		else
		  XClearArea (x_current_display, x_win,
			      pix_x, pix_y, this_pix_len,
			      line_height, False);
	      }
	    
	    /* Center the glyph vertically in the display line. */
	    bitmap_y_offset = (height - line_height) / 2;
	    if (height > line_height)
	      height = line_height;

	    /* ## warning, assumes x_pixmap->face_id is unsigned short... */
	    if (p->face_id != (unsigned short) ~0)
	      face = s->faces [p->face_id];

	    if (face && face->foreground == ~0 && face->background == ~0)
	      face = 0;

	    if (p->mask)
	      {
		XSetFunction (x_current_display, drawing_gc, GXcopy);
		XSetClipMask (x_current_display, drawing_gc, p->mask);
		XSetClipOrigin (x_current_display, drawing_gc,
				pix_x, y - bitmap_y_offset);
	      }

	    /* depth of 0 means it's a bitmap, not a pixmap, and we should
	       use XCopyPlane (1 = current foreground color, 0 = background)
	       instead of XCopyArea, which means that the bits in the pixmap
	       are actual pixel values, instead of symbolic of fg/bg.
	     */
	    if (p->depth > 0 && p->depth == widget->core.depth)
	      XCopyArea (x_current_display, p->pixmap, x_win, drawing_gc,
			 0, bitmap_y_offset, this_pix_len, height,
			 pix_x, y);
	    else if (! face)
	      XCopyPlane (x_current_display, p->pixmap, x_win, drawing_gc,
			  0, bitmap_y_offset < 0 ? 0 : bitmap_y_offset,
			  this_pix_len, height,
			  pix_x, bitmap_y_offset < 0 ? y - bitmap_y_offset : y,
			  1L);
	    else /* face */
	      {
		XGCValues values, old_values;
		int change_p;
		unsigned long fg = face->foreground;
		unsigned long bg = face->background;

		memset (&old_values, 0xDEADBEEF, sizeof(old_values));
		/* this shouldn't cause a server trip; Xlib caches this. */
		if (! XGetGCValues (x_current_display, drawing_gc,
			      GCForeground | GCBackground, &old_values))
		  abort ();
		/* we don't get here if both fg and bg are unspecified */
		if (fg == ~0) fg = old_values.foreground;
		if (bg == ~0) bg = old_values.background;
		values.foreground = fg;
		values.background = bg;

		change_p = (values.background != old_values.background ||
			    values.foreground != old_values.foreground);

		if (change_p)
		  XChangeGC (x_current_display, drawing_gc,
			     GCForeground | GCBackground,
			     &values);
		XCopyPlane (x_current_display,
			    p->pixmap,
			    x_win, drawing_gc,
			    0, bitmap_y_offset, this_pix_len, height,
			    pix_x, y, 1L);
		if (change_p)
		  XChangeGC (x_current_display, drawing_gc,
			     GCForeground | GCBackground,
			     &old_values);
	      }

	    if (p->mask)
	      XSetClipMask (x_current_display, drawing_gc, None);

	    n--;
	    left++;

	    pix_x = real_pix_x;
	  }
	  break;

	case space:
	  {
	    XGCValues ovalues, values;
	    int height;
	    if (face_list[this_run].faceptr &&
		face_list[this_run].faceptr->font &&
		face_list[this_run].faceptr->font != (XFontStruct *) (~0))
	      height = (face_list[this_run].faceptr->font->ascent +
			face_list[this_run].faceptr->font->descent);
	    else
	      height = line_height;

	    if (! XGetGCValues (x_current_display, drawing_gc,
				GCFillStyle | GCForeground | GCBackground,
				&ovalues))
	      abort ();
	    values.foreground = ovalues.background;
	    values.background = ovalues.foreground;
	    values.fill_style = FillSolid;
	    XChangeGC (x_current_display, drawing_gc,
		       GCFillStyle | GCForeground | GCBackground,
		       &values);
	    XFillRectangle (x_current_display, x_win, drawing_gc,
			    pix_x, pix_y, this_pix_len, height);
	    XChangeGC (x_current_display, drawing_gc,
		       GCFillStyle | GCForeground | GCBackground,
		       &ovalues);
	    n--;
	    left++;
	  }
	  break;

	case window:
	  break;

	default:
          {
            maybe_abort();
	    break;
          }
	}

#ifdef ENERGIZE	    
      notify_note_update (s->display.x->edit_widget, pix_x, pix_y,
			    this_pix_len, line_height);
#endif
      if (n == 0) goto ALL_DONE;

      pix_x += face_list[this_run].pix_length;
      this_run++;

      /* check if finished */
      if (this_run >= s->current_glyphs->nruns [top])
	goto ALL_DONE;

      if (face_list[this_run].type == unused_run) goto ALL_DONE;

      if (! force_gc)
	if (! (drawing_gc = face_list[this_run].faceptr->facegc))
	  if (face_list[this_run].faceptr->hilited)
	    drawing_gc = s->display.x->reverse_gc;
	  else
	    drawing_gc = s->display.x->normal_gc;

      this_run_len = face_list[this_run].length;
      this_pix_len = face_list[this_run].pix_length;
      this_font = face_list[this_run].faceptr->font;
      if (!this_font)
	this_font = SCREEN_NORMAL_FACE (s).font;
    }
 ALL_DONE:
  UNBLOCK_INPUT;
  return 1;
}

void
x_clear_display_line_end (vpos)
     int vpos;
{
  struct screen_glyphs *current_screen
    = SCREEN_CURRENT_GLYPHS (updating_screen);
  int pix_x = (current_screen->top_left_x[vpos] +
	       current_screen->pix_width[vpos]);
  int pix_y = current_screen->top_left_y[vpos];
  int pix_width = (PIXEL_WIDTH (updating_screen)
		   - current_screen->pix_width[vpos]
		   - updating_screen->display.x->internal_border_width);
  int pix_height = current_screen->pix_height[vpos];

  erase_screen_cursor_if_needed_and_in_region (updating_screen, 0,
					       vpos, UP_TO_EOL);

  BLOCK_INPUT;
  XClearArea (x_current_display,
	      XtWindow (updating_screen->display.x->edit_widget),
	      pix_x, pix_y, pix_width, pix_height, False);
#ifdef ENERGIZE	    
  notify_note_update (updating_screen->display.x->edit_widget, pix_x, pix_y,
			pix_width, pix_height);
#endif
  UNBLOCK_INPUT;
}

void
x_clear_display_line (vpos, pix_width)
     int vpos, pix_width;
{
  struct screen_glyphs *current_screen
    = SCREEN_CURRENT_GLYPHS (updating_screen);
  int pix_x = current_screen->top_left_x[vpos];
  int pix_y = current_screen->top_left_y[vpos];
  int pix_height = current_screen->pix_height[vpos];

  /* utter kludge to prevent characters with negative left-bearing from
     leaving turds down the left side of the screen... */
  if (pix_x == updating_screen->display.x->internal_border_width)
    pix_x = 0, pix_width += updating_screen->display.x->internal_border_width;

  erase_screen_cursor_if_needed_and_in_region (updating_screen, 0,
					       vpos, UP_TO_EOL);

  BLOCK_INPUT;
  XClearArea (x_current_display,
	      XtWindow (updating_screen->display.x->edit_widget),
	      pix_x, pix_y, pix_width, pix_height, False);

#ifdef ENERGIZE	    
  notify_note_update (updating_screen->display.x->edit_widget, pix_x, pix_y,
			pix_width, pix_height);
#endif
  UNBLOCK_INPUT;
}

#ifdef LINE_INFO_COLUMN

void x_draw_lineinfo_border (screen, vpos)
     struct screen* screen;
     int vpos;
{
  struct screen_glyphs *current_screen = SCREEN_CURRENT_GLYPHS (screen);
  int ibw = screen->display.x->internal_border_width;
  int pix_x = screen->display.x->internal_border_width;
  int pix_y = current_screen->top_left_y[vpos];
  int pix_width  = screen->display.x->line_info_column_width - ibw;
  int pix_height = current_screen->pix_height[vpos];
  GC gc = screen->display.x->line_info_gc;

  if (pix_width <= 0) return;

  BLOCK_INPUT;
  XDrawLine (x_current_display, XtWindow (screen->display.x->edit_widget), gc,
	     pix_x + pix_width - 1, pix_y,
	     pix_x + pix_width - 1, pix_y + pix_height);
  
  XDrawLine (x_current_display, XtWindow (screen->display.x->edit_widget), gc,
	     pix_x, pix_y,
	     pix_x, pix_y + pix_height);
  
#if 0
  if (vpos == 0)
#else
  if (vpos == 0 || (vpos && !current_screen->bufp[vpos-1]))
#endif
    XDrawLine (x_current_display, XtWindow (screen->display.x->edit_widget), gc,
	       pix_x, pix_y,
	       pix_x + pix_width - 1, pix_y);
#if 0
  if (vpos == screen->height - 1)
#else
  if (vpos == screen->height - 1 || ! current_screen->bufp[vpos+1])
#endif
    XDrawLine (x_current_display, XtWindow (screen->display.x->edit_widget), gc,
	       pix_x, pix_y + pix_height,
	       pix_x + pix_width, pix_y + pix_height);
  UNBLOCK_INPUT;
}

void x_clear_lineinfo_glyph (screen, vpos)
     struct screen* screen;
     int vpos;
{
  struct screen_glyphs *current_screen = SCREEN_CURRENT_GLYPHS (screen);
  int ibw = screen->display.x->internal_border_width;
  int pix_x = screen->display.x->internal_border_width;
  int pix_y = current_screen->top_left_y[vpos];
  int pix_width  = screen->display.x->line_info_column_width - ibw;
  int pix_height = current_screen->pix_height[vpos];

  if (pix_width > 0) {
#if 1
  if (current_screen->bufp[vpos] &&
      !(screen->has_minibuffer &&
	vpos >= screen->height - XWINDOW(screen->minibuffer_window)->height)
      )
    {
#endif
    XGCValues gcv;
    GC gc = screen->display.x->line_info_gc;
    unsigned long ofg;

    BLOCK_INPUT;
    XFillRectangle (x_current_display, XtWindow (screen->display.x->edit_widget),
		    screen->display.x->reverse_gc,
		    pix_x+pix_width, pix_y, ibw, pix_height);

    if (! XGetGCValues (x_current_display, gc, GCForeground|GCBackground,&gcv))
      abort ();
    ofg = gcv.foreground;
    gcv.foreground = gcv.background;
    XChangeGC (x_current_display, gc, GCForeground, &gcv);
    XFillRectangle (x_current_display, XtWindow (screen->display.x->edit_widget),
		    gc, pix_x, pix_y, pix_width, pix_height);
    gcv.foreground = ofg;
    XChangeGC (x_current_display, gc, GCForeground, &gcv);
    x_draw_lineinfo_border (screen, vpos);
    UNBLOCK_INPUT;
#if 1
    }
  else if (screen->has_minibuffer &&
	   vpos >= screen->height -
	   XWINDOW(screen->minibuffer_window)->height)
    {
      GC gc = screen->display.x->reverse_gc;
      BLOCK_INPUT;
      XFillRectangle (x_current_display, XtWindow (screen->display.x->edit_widget),
		      gc, pix_x, pix_y, pix_width+ibw, pix_height);
      UNBLOCK_INPUT;
    }
  else
    {
      BLOCK_INPUT;
      XFillRectangle (x_current_display, XtWindow (screen->display.x->edit_widget),
		      screen->display.x->normal_gc,
		      pix_x, pix_y, pix_width+ibw,
		      pix_height - x_interline_space);
      XFillRectangle (x_current_display, XtWindow (screen->display.x->edit_widget),
		      screen->display.x->reverse_gc,
		      pix_x, pix_y + pix_height - x_interline_space,
		      pix_width+ibw, x_interline_space);
      XFillRectangle (x_current_display, XtWindow (screen->display.x->edit_widget),
		      screen->display.x->reverse_gc,
		      pix_x, pix_y - x_interline_space,
		      pix_width+ibw, x_interline_space);
      XDrawLine (x_current_display, XtWindow (screen->display.x->edit_widget),
		 screen->display.x->normal_gc,
		 pix_x, pix_y - (x_interline_space + 1),
		 pix_x + pix_width - 1, pix_y - (x_interline_space + 1));

      UNBLOCK_INPUT;
    }
#endif
  }
}
#endif



/* Output some text at the nominal screen cursor position,
   advancing the cursor over the text.
   Output LEN glyphs at START.  */

static void
XTwrite_glyphs (hpos, vpos, len)
     int hpos, vpos, len;
{
  struct screen *s;

  if (updating_screen == 0)
    {
      s = selected_screen;
      erase_screen_cursor_if_needed (s);
    }
  else
    s = updating_screen;

  if (hpos != s->cursor_x || vpos != s->cursor_y)
    /* abort ();   fuck this */
    return;

  x_write_glyphs (s, hpos, vpos, len, 0, 0);
  s->cursor_x += len;

  if (updating_screen == 0)
    {
      draw_screen_cursor_if_needed (s);
      s->cursor_x -= len;
      BLOCK_INPUT;
      XFlushQueue ();
      UNBLOCK_INPUT;
    }
}

/* Erase the current text line from the nominal cursor position (inclusive)
   to column FIRST_UNUSED (exclusive).  The idea is that everything
   from FIRST_UNUSED onward is already erased.  */

static void
XTclear_end_of_line (first_unused)
     int first_unused;
{
  struct screen *s = updating_screen;
  int pix_x, pix_y, pix_width, pix_height;
  struct glyph_dimensions *dimensions;

  if (s == 0)
    /* abort ();   fuck this */
    return;

  if (s->cursor_y < 0 || s->cursor_y >= s->height || first_unused <= 0)
    return;

  BLOCK_INPUT;
  pix_height = s->current_glyphs->pix_height[s->cursor_y];
  dimensions = get_glyph_dimensions (s, s->cursor_x, s->cursor_y);
  if (!dimensions)
    goto cancel;

  pix_x = dimensions->top_left_x;
  pix_y = dimensions->top_left_y;

  if (first_unused >= s->width
      || first_unused > s->current_glyphs->used[s->cursor_y])
    {
      pix_width = (PIXEL_WIDTH (s)
		   - s->display.x->internal_border_width
		   - pix_x);
    }
  else
    {
      dimensions = get_glyph_dimensions (s, first_unused - 1, s->cursor_y);
      if (!dimensions)
	goto cancel;

      pix_width = pix_x + dimensions->top_left_x - 1;
    }

  erase_screen_cursor_if_needed (s);
  XClearArea (x_current_display, XtWindow (s->display.x->edit_widget),
	      pix_x, pix_y, pix_width, pix_height, False);
#ifdef ENERGIZE	    
  notify_note_update (s->display.x->edit_widget, pix_x, pix_y,
			pix_width, pix_height);
#endif

 cancel:
  UNBLOCK_INPUT;
}

static void
XTclear_screen ()
{
  struct screen *s;

  if (updating_screen == 0)
    s = selected_screen;
  else
    s = updating_screen;

  s->cursor_x = s->cursor_y = 0;
  s->phys_cursor_x = -1;
  BLOCK_INPUT;
  XClearWindow (x_current_display, XtWindow (s->display.x->edit_widget));
#ifdef ENERGIZE	    
  notify_note_update (s->display.x->edit_widget, 0, 0,
		      s->display.x->pixel_width, s->display.x->pixel_height);
#endif
  UNBLOCK_INPUT;
}

/* briefly swap the foreground and background colors.
 */

extern int select();

static void
XTflash (s)
     struct screen *s;
{
  unsigned long frob;
  struct face *face;
  Display *dpy;
  Window w;

  if (updating_screen != 0)
    return;

  BLOCK_INPUT;
  face = &SCREEN_NORMAL_FACE (s);
  frob = face->foreground ^ face->background;
  dpy = XtDisplay (s->display.x->widget);
  w = XtWindow (s->display.x->edit_widget);
  XSetState (dpy, face->facegc, frob, face->background, GXxor, -1);
  XFillRectangle (dpy, w, face->facegc, 0, 0, s->display.x->widget->core.width,
		  s->display.x->widget->core.height);
  XSync (dpy, False);
  UNBLOCK_INPUT;

  {
    int usecs = 100000;
    struct timeval tv;
    tv.tv_sec  = usecs / 1000000L;
    tv.tv_usec = usecs % 1000000L;
    /* I'm sure someone is going to complain about this... */
    (void) select (0, 0, 0, 0, &tv);
  }

  BLOCK_INPUT;
  XFillRectangle (dpy, w, face->facegc, 0, 0, s->display.x->widget->core.width,
		  s->display.x->widget->core.height);
  XSetState (dpy, face->facegc, face->foreground, face->background,
	     GXcopy, -1);
  XSync (dpy, False);
  UNBLOCK_INPUT;
}

/* Make audible bell.  */

/* X defines volume as from -100 to 100; we use 0 to 100 */
extern Lisp_Object Vbell_volume;

static void
XTsimple_beep (volume)
     int volume;
{
  if (volume < 0) volume = 0;
  else if (volume > 100) volume = 100;
  BLOCK_INPUT;
  XBell (x_current_display, (volume * 2) - 100);
  XFlushQueue ();
  UNBLOCK_INPUT;
}

static void
XTring_bell (sound)
     Lisp_Object sound;
{
  if (visible_bell)
    XTflash (selected_screen);
  else
    Fplay_sound (sound, Qnil);
}

/* Specify how many text lines, from the top of the window,
   should be affected by insert-lines and delete-lines operations.
   This, and those operations, are used only within an update
   that is bounded by calls to XTupdate_begin and XTupdate_end.  */

static void
XTset_terminal_window (n)
     int n;
{
  if (updating_screen == 0)
    /* abort ();   fuck this */
    return;

  if ((n <= 0) || (n > updating_screen->height))
    update_height = updating_screen->height;
  else
    update_height = n;
}

/* Perform an insert-lines operation, inserting N lines
   at a vertical position s->cursor_y.  */

static void
stufflines (n)
     int n;
{
  int width, copy_height, clear_height, from_x, from_y, to_x, to_y;
  struct screen *s = updating_screen;
  struct screen_glyphs *current_glyphs = s->current_glyphs;
  int iborder = s->display.x->internal_border_width;
  from_x = to_x = iborder;
  from_y = current_glyphs->top_left_y[s->cursor_y];
  to_y = current_glyphs->top_left_y[s->cursor_y + n];
  to_y = min (to_y, current_glyphs->top_left_y[update_height]);

  width = PIXEL_WIDTH (s) - (2 * iborder);
  copy_height = current_glyphs->top_left_y[update_height - n] - from_y;
  clear_height = to_y - from_y;

  BLOCK_INPUT;
  if (copy_height > 0)
    {
      XCopyArea (x_current_display, XtWindow (s->display.x->edit_widget),
		 XtWindow (s->display.x->edit_widget), s->display.x->normal_gc,
		 from_x, from_y, width, copy_height, to_x, to_y);
#ifdef ENERGIZE
#if 0
 *      xc_note_parent_scrolled (s->display.x->edit_widget,
 *			       from_x, from_y, width, copy_height, 
 *			       to_x - from_x, to_y - from_y);
#endif
#endif
      XClearArea (x_current_display, XtWindow (s->display.x->edit_widget),
		  from_x, from_y, width, clear_height, False);
#ifdef ENERGIZE
      notify_note_update (s->display.x->edit_widget, from_x, from_y,
			  width, clear_height);

#endif
    }
  UNBLOCK_INPUT;
}

/* Perform a delete-lines operation, deleting N lines
   at a vertical position s->cursor_y.  */

static void
scraplines (n)
     int n;
{
  struct screen *s = updating_screen;
  struct screen_glyphs *current_glyphs = s->current_glyphs;
  int top = s->cursor_y;
  int width, height;
  int iborder = s->display.x->internal_border_width;

  BLOCK_INPUT;
  if ((s->cursor_y + n) >= update_height
      && (update_height >= (s->cursor_y + 1)))
    {
      width = PIXEL_WIDTH (s) - (2 * iborder);
      height = (current_glyphs->top_left_y[update_height]
		+ current_glyphs->pix_height[update_height]
		- current_glyphs->top_left_y[top]);
      XClearArea (x_current_display, XtWindow (s->display.x->edit_widget),
		  current_glyphs->top_left_x[top],
		  current_glyphs->top_left_y[top],
		  width, height, False);
#ifdef ENERGIZE
      notify_note_update (s->display.x->edit_widget, 
			  current_glyphs->top_left_x[top],
			  current_glyphs->top_left_y[top],
			  width, height);
#endif
    }
  else
    {
      int from_x = iborder;
      int from_y = current_glyphs->top_left_y[top + n];
      int to_x = iborder;
      int to_y = current_glyphs->top_left_y[top];

      width = PIXEL_WIDTH (s) - (2 * iborder);
      height = current_glyphs->top_left_y[update_height] - from_y;

      /* Move lines under the deleted area upwards. */
      XCopyArea (x_current_display, XtWindow (s->display.x->edit_widget),
		 XtWindow (s->display.x->edit_widget), s->display.x->normal_gc,
		 from_x, from_y, width, height, to_x, to_y);
#ifdef ENERGIZE
#if 0
 *      xc_note_parent_scrolled (s->display.x->edit_widget,
 *			       from_x, from_y, width, height, 
 *			       to_x - from_x, to_y - from_y);
#endif
#endif
      to_y = to_y + height;
      height = current_glyphs->top_left_y[update_height] - to_y;
      /* Clear any lines below those just moved. */
      XClearArea (x_current_display, XtWindow (s->display.x->edit_widget),
		  from_x, to_y, width, height, False);
#ifdef ENERGIZE	    
      notify_note_update (s->display.x->edit_widget, from_x, to_y,
			  width, height);
#endif

  UNBLOCK_INPUT;
    }
}

/* Perform an insert-lines or delete-lines operation,
   inserting N lines or deleting -N lines at vertical position VPOS.  */

static void
XTins_del_lines (vpos, n)
     int vpos, n;
{
  if (updating_screen == 0)
    /* abort ();   fuck this */
    return;
  if (vpos >= update_height)
    return;

  erase_cursor_if_needed ();

  updating_screen->cursor_x = 0;
  updating_screen->cursor_y = vpos;

  if (n >= 0)
    stufflines (n);
  else
    scraplines (-n);
}

/* Repaint all lines encompassed by an Expose region. */

void
repaint_lines (s, left, top, width, height)
     struct screen *s;
     int left, top, width, height;
{
  struct screen_glyphs *current_screen = SCREEN_CURRENT_GLYPHS (s);
  int this_line;
  int bottom = top + height;
  int line_top;
  int line_bottom;
  int line_used; 

  if (s->garbaged)
    return;

  erase_screen_cursor_if_needed (s);

  for (this_line = 0; this_line < SCREEN_HEIGHT (s); this_line++)
    if (current_screen->enable [this_line])
      {
	line_top = current_screen->top_left_y [this_line];
	line_bottom = line_top + current_screen->pix_height [this_line];
	line_used = current_screen->used [this_line];

	if (line_top > bottom)
	  break;
	if (line_bottom >= top && line_used > 0)
	  x_write_glyphs (s, 0, this_line, line_used, 0, 0);
      }

  draw_screen_cursor_if_needed (s);
}

void
x_screen_redraw_cursor (screen)
     struct screen *screen;
{
  erase_screen_cursor_if_needed (screen);
  draw_screen_cursor_if_needed (screen);
}

/* Make SCREEN the current input screen.  */
void
x_new_selected_screen (screen)
     struct screen *screen;
{
  struct screen* previous_screen = selected_screen;

  if (previous_screen != screen)
    {
      selected_screen = screen;

      if (previous_screen)
	x_screen_redraw_cursor (previous_screen);
      x_screen_redraw_cursor (screen);
    }
}


static char *events[] =
{
   "0: ERROR!",
   "1: REPLY",
   "KeyPress",
   "KeyRelease",
   "ButtonPress",
   "ButtonRelease",
   "MotionNotify",
   "EnterNotify",
   "LeaveNotify",
   "FocusIn",
   "FocusOut",
   "KeymapNotify",
   "Expose",
   "GraphicsExpose",
   "NoExpose",
   "VisibilityNotify",
   "CreateNotify",
   "DestroyNotify",
   "UnmapNotify",
   "MapNotify",
   "MapRequest",
   "ReparentNotify",
   "ConfigureNotify",
   "ConfigureRequest",
   "GravityNotify",
   "ResizeRequest",
   "CirculateNotify",
   "CirculateRequest",
   "PropertyNotify",
   "SelectionClear",
   "SelectionRequest",
   "SelectionNotify",
   "ColormapNotify",
   "ClientMessage",
   "MappingNotify",
   "LASTEvent"
};

char *
x_event_name (event_type)
     int event_type;
{
  if (event_type < 0) return 0;
  if (event_type >= (sizeof (events) / sizeof (char *))) return 0;
  return events [event_type];
}


static void
x_display_bar_cursor (s, on)
     struct screen *s;
     int on;
{
  int x0, x1, y0, y1;

  if (! s->visible || (! on && s->phys_cursor_x < 0))
    return;

  if (s->phys_cursor_x >= 0 &&
      (!on || s->phys_cursor_x != s->cursor_x
       || s->phys_cursor_y != s->cursor_y))
    {
      if (s->phys_cursor_x < s->current_glyphs->used[s->phys_cursor_y])
	{
	  struct glyph_dimensions *dimensions
	    = get_glyph_dimensions (s, s->phys_cursor_x, s->phys_cursor_y);
	  if (!dimensions)
	    return;

	  x0 = dimensions->top_left_x;
	  x1 = x0;
	  y0 = s->current_glyphs->top_left_y[s->phys_cursor_y];
	  y1 = (y0 + s->current_glyphs->pix_height[s->phys_cursor_y]
		- x_interline_space);

	  BLOCK_INPUT;
	  XDrawLine (x_current_display, XtWindow (s->display.x->edit_widget),
		     s->display.x->reverse_gc, x0, y0, x1, y1);
	  UNBLOCK_INPUT;
	  x_write_glyphs (s, s->phys_cursor_x, s->phys_cursor_y, 1, 0, 0);
	}
      else
	{
	  x0 = (s->current_glyphs->top_left_x[s->phys_cursor_y]
		+ s->current_glyphs->pix_width[s->phys_cursor_y]);
	  x1 = x0;
	  y0 = s->current_glyphs->top_left_y[s->phys_cursor_y];
	  y1 = y0 + s->current_glyphs->pix_height[s->phys_cursor_y];

	  BLOCK_INPUT;
	  XDrawLine (x_current_display, XtWindow (s->display.x->edit_widget),
		     s->display.x->reverse_gc, x0, y0, x1, y1);
	  UNBLOCK_INPUT;
	}
      s->phys_cursor_x = -1;
    }

  if (on && focus_cursor_p (s))
    {
      if (s->cursor_x < s->current_glyphs->used[s->cursor_y])
	{
	  struct glyph_dimensions *dimensions
	    = get_glyph_dimensions (s, s->cursor_x, s->cursor_y);
	  if (!dimensions)
	    return;

	  x0 = dimensions->top_left_x;
	  x1 = x0;
	  y0 = s->current_glyphs->top_left_y[s->cursor_y];
	  y1 = y0 + s->current_glyphs->pix_height[s->cursor_y];
	}
      else
	{
	  x0 = (s->current_glyphs->top_left_x[s->cursor_y]
		+ s->current_glyphs->pix_width[s->cursor_y]);
	  x1 = x0;
	  y0 = s->current_glyphs->top_left_y[s->cursor_y];
	  y1 = y0 + s->current_glyphs->pix_height[s->cursor_y];
	}

      BLOCK_INPUT;
      XDrawLine (x_current_display, XtWindow (s->display.x->edit_widget),
		 s->display.x->cursor_gc, x0, y0, x1, y1);
      UNBLOCK_INPUT;
      s->phys_cursor_x = s->cursor_x;
      s->phys_cursor_y = s->cursor_y;
    }
}

/* Draw an empty box at the end of the line. */

static int
x_draw_empty_box (s)
     struct screen *s;
{
  int height, x, y;
  XGCValues values;
  
  BLOCK_INPUT;
  if (! XGetGCValues (x_current_display, s->display.x->cursor_gc,
		      GCBackground | GCForeground, &values))
    abort ();
  height = s->current_glyphs->pix_height[s->phys_cursor_y] - x_interline_space;
  x = s->current_glyphs->top_left_x[s->phys_cursor_y]
    + s->current_glyphs->pix_width[s->phys_cursor_y];
  y = s->current_glyphs->top_left_y[s->phys_cursor_y];

  XClearArea (x_current_display, XtWindow (s->display.x->edit_widget),
	      x, y, EOL_CURSOR_WIDTH, height, False);

  XSetForeground (x_current_display, s->display.x->cursor_gc,
		  values.background);
  XDrawRectangle (x_current_display, XtWindow (s->display.x->edit_widget),
		  s->display.x->cursor_gc,
		  x, y,
		  EOL_CURSOR_WIDTH - 1, height - 1);
  XSetForeground (x_current_display, s->display.x->cursor_gc,
		  values.foreground);
  UNBLOCK_INPUT;
  return 1;
}

/* Draw a square with the cursor gc at the end of the line. */

static int
x_draw_square (s)
     struct screen *s;
{
  int height, x, y;
  XGCValues values;
  
  BLOCK_INPUT;
  if (! XGetGCValues (x_current_display, s->display.x->cursor_gc,
		      GCBackground | GCForeground, &values))
    abort ();
  height = s->current_glyphs->pix_height[s->phys_cursor_y] - x_interline_space;
  x = s->current_glyphs->top_left_x[s->phys_cursor_y]
    + s->current_glyphs->pix_width[s->phys_cursor_y];
  y = s->current_glyphs->top_left_y[s->phys_cursor_y];

  XSetForeground (x_current_display, s->display.x->cursor_gc,
		  values.background);
  XFillRectangle (x_current_display, XtWindow (s->display.x->edit_widget),
		  s->display.x->cursor_gc,
		  x, y,
		  EOL_CURSOR_WIDTH, height);
  XSetForeground (x_current_display, s->display.x->cursor_gc,
		  values.foreground);
  UNBLOCK_INPUT;
  return 1;
}

/* Erase the cursor square at the end of the line. */

static int
x_erase_square (s)
     struct screen *s;
{
  int height, x, y;

  height = s->current_glyphs->pix_height[s->phys_cursor_y];
  x = s->current_glyphs->top_left_x[s->phys_cursor_y]
    + s->current_glyphs->pix_width[s->phys_cursor_y];
  y = s->current_glyphs->top_left_y[s->phys_cursor_y];

  BLOCK_INPUT;
  XClearArea (x_current_display, XtWindow (s->display.x->edit_widget),
	      x, y,
	      /* EOL_CURSOR_WIDTH, */
	      PIXEL_WIDTH (s) - x,
	      height, False);
  UNBLOCK_INPUT;
  return 1;
}

/* Turn the displayed cursor of screen S on or off according to ON.
   If ON is nonzero, where to put the cursor is specified
   by S->cursor_x and S->cursor_y.  Return 1 if it does something,
   0 if it does nothing. */

static int
x_display_box_cursor (s, on)
     struct screen *s;
     int on;
{
  struct screen_glyphs *current_screen;
  int value = 0;

  if (! s->visible)
    return 0;

  if (!on && s->phys_cursor_x < 0)
    return 1;

  current_screen = SCREEN_CURRENT_GLYPHS (s);

  if (! current_screen) return 0;  /* added by jwz */

  if (on)
    {
      if (!s->cursor_erased
 	  && ((s->phys_cursor_x != s->cursor_x)
 	      || (s->phys_cursor_y != s->cursor_y)))
 	x_display_box_cursor (s, 0);
      s->phys_cursor_x = s->cursor_x;
      s->phys_cursor_y = s->cursor_y;
      
      /* Case where cursor is in text. */
      if (current_screen->enable[s->cursor_y])
	{
	  if (s->cursor_x < current_screen->used[s->cursor_y])
	    {
	      return x_write_glyphs (s, s->cursor_x, s->cursor_y,
				     1, s->display.x->cursor_gc,
				     ! focus_cursor_p (s));
	    }
	  else
	    {
	      /* No text here. */
	      if (! focus_cursor_p (s))
		return x_draw_empty_box (s);
	      else
		return x_draw_square (s);
	    }
	} else {
	  return 0;
	}
    }

  /* Turn cursor OFF. */
  if (current_screen->enable[s->phys_cursor_y])
    {
      if (s->phys_cursor_x < current_screen->used[s->phys_cursor_y])
	value = x_write_glyphs (s, s->phys_cursor_x, s->phys_cursor_y, 1, 0, 0);
      else
	value = x_erase_square (s);
    }

  s->phys_cursor_x = -1;
  return value;
}

static int
x_display_cursor (s, on)
     struct screen *s;
     int on;
{
  if (on)
    last_bar_cursor = Vbar_cursor;
  
  if (EQ (last_bar_cursor, Qnil))
    return x_display_box_cursor (s, on);
  else
    {
      x_display_bar_cursor (s, on);
      return 1;
    }
}

static int
x_error_handler (disp, event)
     Display *disp;
     XErrorEvent *event;
{
  /* It would be nice to be able to signal a Lisp error here, but we can't,
     because the guts of Xlib expect this function to either return or exit.
     Also, we can't print things in the minibuffer with Fmessage(), because
     this function isn't allowed to do IO on the display connection.  So all
     we can do is write to stderr.
   */
  XmuPrintDefaultErrorMessage (disp, event, stderr);
  return 0;
}

extern char *sys_errlist[];

/* This is called when the display connection becomes hosed.
   It may not return.
 */
static int
x_IO_error_handler (disp)
     Display *disp;
{
  fprintf (stderr, "\nFatal I/O Error %d (%s) on display connection \"%s\"\n",
	   errno, sys_errlist [errno], DisplayString (disp));
  fprintf (stderr,
      "after %lu requests (%lu known processed) with %d events remaining.\n",
	   NextRequest (disp) - 1, LastKnownRequestProcessed (disp),
	   QLength(disp));
  if (_Xdebug)
    abort ();
  else
    {
      fprintf (stderr, "Autosaving and exiting...\n");
      x_current_display = 0; /* it's dead! */
      Vwindow_system = Qnil; /* let it lie! */
      Fset (intern ("kill-emacs-hook"), Qnil); /* too dangerous */
      Fkill_emacs (make_number (70));
    }
  return 0; /* not reached; suppress warnings */
}



/* Pixmap cache */

c_hashtable x_pixmap_table;

struct x_pixmap builtin_continuer_pixmap;
struct x_pixmap builtin_truncator_pixmap;
struct x_pixmap builtin_rarrow_pixmap;

static int glyph_to_x_pixmaps_size, max_pixmap_id;
struct x_pixmap **glyph_to_x_pixmaps_table;


struct x_pixmap *
glyph_to_x_pixmap (GLYPH g)
{
  struct x_pixmap *p;
  if (g >= max_pixmap_id)
    abort ();
  p = glyph_to_x_pixmaps_table [g];
  if (! p) abort ();
  if (g != p->glyph_id) abort ();
  return p;
}

static void
init_bitmap_tables ()
{
  Display *dpy = x_current_display;
  Window root = DefaultRootWindow (dpy);

  glyph_to_x_pixmaps_size = 50;
  glyph_to_x_pixmaps_table = (struct x_pixmap **)
    xmalloc (glyph_to_x_pixmaps_size * sizeof (glyph_to_x_pixmaps_table[0]));
  max_pixmap_id = 0;

  BLOCK_INPUT;
  builtin_continuer_pixmap.glyph_id = max_pixmap_id++;
  builtin_continuer_pixmap.width = continuer_width;
  builtin_continuer_pixmap.height = continuer_height;
  builtin_continuer_pixmap.depth = 0;
  builtin_continuer_pixmap.pixmap =
    XCreateBitmapFromData (dpy, root, (char *) continuer_bits,
			   continuer_width, continuer_height);
  builtin_continuer_pixmap.mask = builtin_continuer_pixmap.pixmap;
  glyph_to_x_pixmaps_table [builtin_continuer_pixmap.glyph_id] =
    &builtin_continuer_pixmap;

  builtin_truncator_pixmap.glyph_id = max_pixmap_id++;
  builtin_truncator_pixmap.width = truncator_width;
  builtin_truncator_pixmap.height = truncator_height;
  builtin_truncator_pixmap.depth = 0;
  builtin_truncator_pixmap.pixmap =
    XCreateBitmapFromData (dpy, root, (char *) truncator_bits,
			   continuer_width, continuer_height);
  builtin_truncator_pixmap.mask = builtin_truncator_pixmap.pixmap;
  glyph_to_x_pixmaps_table [builtin_truncator_pixmap.glyph_id] =
    &builtin_truncator_pixmap;

  builtin_rarrow_pixmap.glyph_id = max_pixmap_id++;
  builtin_rarrow_pixmap.width = rarrow_width;
  builtin_rarrow_pixmap.height = rarrow_height;
  builtin_rarrow_pixmap.depth = 0;
  builtin_rarrow_pixmap.pixmap =
    XCreateBitmapFromData (dpy, root, (char *) rarrow_bits,
			   rarrow_width, rarrow_height);
  builtin_rarrow_pixmap.mask = builtin_rarrow_pixmap.pixmap;
  glyph_to_x_pixmaps_table [builtin_rarrow_pixmap.glyph_id] =
    &builtin_rarrow_pixmap;
  UNBLOCK_INPUT;

  x_pixmap_table = make_strings_hashtable (20);
}


extern Pixmap load_pixmap_1 (Display *, Window, Lisp_Object,
			     unsigned int *, unsigned int *, unsigned int *,
			     Pixmap *);


struct x_pixmap *
x_get_pixmap (Lisp_Object lisp_name, char *hash_suffix)
{
  Display *dpy = x_current_display;
  char hash_buf [255];
  char *name = (char *) XSTRING (lisp_name)->data;
  struct x_pixmap *value = 0;
  struct x_pixmap dummy;

  strcpy (hash_buf, name);
  if (hash_suffix)
    strcat (hash_buf, hash_suffix);

  if (gethash ((void *) hash_buf, x_pixmap_table, (void *) &value))
    if (value) return value;

  dummy.pixmap = load_pixmap_1 (dpy, DefaultRootWindow (dpy), lisp_name,
				&dummy.width, &dummy.height, &dummy.depth,
				&dummy.mask);
  /* we malloc after load_pixmap_1 has returned because that function
     can signal a lisp error.  We could also use unwind-protect... */
  name = strdup (name);
  value = (struct x_pixmap *) xmalloc (sizeof (struct x_pixmap));
  dummy.face_id = ~0;
  memcpy (value, &dummy, sizeof (struct x_pixmap));
  puthash ((void *) strdup (hash_buf), (void *) value, x_pixmap_table);
  value->glyph_id = max_pixmap_id;
  if (max_pixmap_id >= glyph_to_x_pixmaps_size)
    {
      glyph_to_x_pixmaps_size += 50;
      glyph_to_x_pixmaps_table = (struct x_pixmap **)
	xrealloc (glyph_to_x_pixmaps_table,
		  glyph_to_x_pixmaps_size * sizeof (struct x_pixmap *));
    }
  glyph_to_x_pixmaps_table [max_pixmap_id] = value;
  max_pixmap_id++;
  return value;
}


extern void (*beep_hook) ();

static void
make_argc_argv (argv_list, argc, argv)
     Lisp_Object argv_list;
     int* argc;
     char*** argv;
{
  Lisp_Object next;
  int n = Flength (argv_list);
  int i;
  *argv = (char**)xmalloc (n * sizeof (char*));

  for (i = 0, next = argv_list; i < n; i++, next = XCONS (next)->cdr)
    {
      CHECK_STRING (XCONS (next)->car, 0);
      (*argv) [i] = (char*)(XSTRING (XCONS (next)->car)->data);
    }
  *argc = i;
}

static Lisp_Object
make_arg_list (argc, argv)
     int argc;
     char** argv;
{
  Lisp_Object result;
  int i;

  for (i = 0, result = Qnil; i < argc; i++)
    result = Fcons (build_string (argv [i]), result);
  return Fnreverse (result);
}

static XrmOptionDescRec 
emacs_options[] =
{
  {"-internal-border-width", "*EmacsScreen.internalBorderWidth",
     XrmoptionSepArg, NULL},
  {"-ib",	"*EmacsScreen.internalBorderWidth", XrmoptionSepArg, NULL},
  {"-geometry",	"*EmacsScreen.initialGeometry", XrmoptionSepArg, NULL},
  {"-iconic",	"*EmacsShell.iconic", XrmoptionNoArg, (XtPointer) "on"},
  {"-name",	"*EmacsShell.name", XrmoptionSepArg, (XtPointer) NULL},
  {"-T",	"*EmacsShell.title", XrmoptionSepArg, (XtPointer) NULL},
  {"-wn",	"*EmacsShell.title", XrmoptionSepArg, (XtPointer) NULL},
  {"-title",	"*EmacsShell.title", XrmoptionSepArg, (XtPointer) NULL},
  {"-iconname",	"*EmacsShell.iconName", XrmoptionSepArg, (XtPointer) NULL},
  {"-in",	"*EmacsShell.iconName", XrmoptionSepArg, (XtPointer) NULL},
  {"-mc",	"*pointerColor", XrmoptionSepArg, (XtPointer) NULL},
  {"-cr",	"*cursorColor", XrmoptionSepArg, (XtPointer) NULL}
};


static void sanity_check_geometry_resource (Display *);
extern void x_init_modifier_mapping (Display *);

#ifdef SIGIO
extern void init_sigio (void);
#endif

extern Lisp_Object Vx_emacs_application_class;
extern int x_selection_timeout;

Lisp_Object
x_term_init (argv_list)
     Lisp_Object argv_list;
{
  int argc;
  char** argv;
  char *app_class;
#ifdef F_SETOWN
  extern int old_fcntl_owner;
#endif

  if (STRINGP (Vx_emacs_application_class) &&
      XSTRING (Vx_emacs_application_class)->size > 0)
    app_class = (char *) XSTRING (Vx_emacs_application_class)->data;
  else
    app_class = "Emacs";

  make_argc_argv (argv_list, &argc, &argv);

  Xt_app_shell = XtAppInitialize (&Xt_app_con, app_class,
				  emacs_options, XtNumber(emacs_options),
				  &argc, argv,
				  0, NULL, 0);
  x_current_display = XtDisplay (Xt_app_shell);

  if (x_current_display == 0)
    fatal ("X server not responding; check the DISPLAY environment variable or use \"-d\"\n");
  
  x_file_descriptor = ConnectionNumber (x_current_display);

  sanity_check_geometry_resource (x_current_display);

  /* In event-Xt.c */
  x_init_modifier_mapping (x_current_display);
  
  /* In xselect.c */
  x_selection_timeout = (XtAppGetSelectionTimeout (Xt_app_con) / 1000);

  {
    char tem_name[MAXPATHLEN];
    int l;

    gethostname (tem_name, MAXPATHLEN - 1);
    tem_name[99] = 0;
    if ((l = strlen (tem_name)) < MAXPATHLEN -1)
      {
	hostname = (char *) xmalloc (l+1);
	memcpy (hostname, tem_name, l + 1);
      }
    else
      {
	hostname = (char *) xmalloc (10);
	memcpy (hostname, "Somewhere", 9);
	hostname[10] = 0;
      }
    id_name = (char *) xmalloc (XSTRING (Vinvocation_name)->size +
				strlen (hostname) + 2);
    sprintf (id_name, "%s@%s", XSTRING (Vinvocation_name)->data, hostname);
  }
  
#ifdef F_SETOWN
  old_fcntl_owner = fcntl (x_file_descriptor, F_GETOWN, 0);
#ifdef F_SETOWN_SOCK_NEG
  /* stdin is a socket here */
  fcntl (x_file_descriptor, F_SETOWN, -getpid ());
#else
  fcntl (x_file_descriptor, F_SETOWN, getpid ());
#endif /* F_SETOWN_SOCK_NEG */
#endif /* F_SETOWN */

#ifdef SIGIO
  init_sigio ();
#endif

  /* Must use interrupt input because we cannot otherwise
     arrange for C-g to be noticed immediately.
     We cannot connect it to SIGINT.  */
  Fset_input_mode (Qt, Qnil, Qt);

  clear_screen_hook = XTclear_screen;
  clear_end_of_line_hook = XTclear_end_of_line;
  ins_del_lines_hook = XTins_del_lines;
  ring_bell_hook = XTring_bell;
  beep_hook = XTsimple_beep;
  write_glyphs_hook = XTwrite_glyphs;
  update_begin_hook = XTupdate_begin;
  update_end_hook = XTupdate_end;
  set_terminal_window_hook = XTset_terminal_window;
  read_socket_hook = (int (*)())-1;
  cursor_to_hook = XTcursor_to;

  scroll_region_ok = 1;		/* we'll scroll partial screens */
  char_ins_del_ok = 0;		/* just as fast to write the line */
  line_ins_del_ok = 1;		/* we'll just blt 'em */
  fast_clear_end_of_line = 1;	/* X does this well */
  memory_below_screen = 0;	/* we don't remember what scrolls 
				   off the bottom */
  baud_rate = 19200;
  x_interline_space = 0;

  last_bar_cursor = Qnil;

  init_bitmap_tables ();

  XSetErrorHandler (x_error_handler);
  XSetIOErrorHandler (x_IO_error_handler);

  emacs_Xt_make_event_stream ();

  /* Disable Window Change signals;  they are handled by X events. */
#ifdef SIGWINCH
  signal (SIGWINCH, SIG_IGN);
#endif /* SIGWINCH */

  {
    Lisp_Object L = make_arg_list (argc, argv);
    xfree (argv);
    return L;
  }
}

static void
sanity_check_geometry_resource (Display *dpy)
{
  char *app_name, *app_class, *s;
  char buf1 [255], buf2 [255];
  char *type;
  XrmValue value;
  XtGetApplicationNameAndClass (dpy, &app_name, &app_class);
  strcpy (buf1, app_name);
  strcpy (buf2, app_class);
  for (s = buf1; *s; s++) if (*s == '.') *s = '_';
  strcat (buf1, "._no_._such_._resource_.geometry");
  strcat (buf2, "._no_._such_._resource_.Geometry");
  if (XrmGetResource (XtDatabase (dpy), buf1, buf2, &type, &value) == True)
    {
      fprintf (stderr, "\n\
Apparently \"%s*geometry: %s\" was specified in the resource\n\
database.  Specifying \"*geometry\" will make emacs (and most other X\
 programs)\nmalfunction in obscure ways.  You should always use\
 \".geometry\" instead.\n\n", app_class, (char *) value.addr);
      exit (-1);
    }
}


static void
x_update_wm_hints (widget)
     Widget widget;
{
  XSizeHints hints;
  long supplied = 0;
  BLOCK_INPUT;
  if (XGetWMNormalHints (XtDisplay (widget), XtWindow (widget),
			 &hints, &supplied))
    {
      hints.flags |= USPosition;
      hints.x = widget->core.x;
      hints.y = widget->core.y;
    }
  else
    {
      memset ((char *) &hints, 0, sizeof (hints));
      hints.flags = USPosition;
    }
  XSetWMNormalHints (XtDisplay (widget), XtWindow (widget), &hints);
  UNBLOCK_INPUT;
}


static void
x_calc_absolute_position (s)
     struct screen *s;
{
  Display *dpy = XtDisplay (s->display.x->widget);
  int screen_w = DisplayWidth (dpy, DefaultScreen (dpy));
  int screen_h = DisplayHeight (dpy, DefaultScreen (dpy));
  int shell_w = s->display.x->widget->core.width;
  int shell_h = s->display.x->widget->core.height;
  /* #### We should increment shell_w and shell_h by the size of the
     window-manager decoration.  Is there any easy way to find that?
   */
  if (s->display.x->left_pos < 0)
    s->display.x->left_pos = screen_w - shell_w + s->display.x->left_pos;
  if (s->display.x->top_pos < 0)
    s->display.x->top_pos = screen_h - shell_h + s->display.x->top_pos;
}

void
x_set_offset (s, xoff, yoff)
     struct screen *s;
     int xoff, yoff;
{
  s->display.x->top_pos = yoff;
  s->display.x->left_pos = xoff;
  x_calc_absolute_position (s);
  s->display.x->widget->core.y = s->display.x->top_pos;
  s->display.x->widget->core.x = s->display.x->left_pos;
  BLOCK_INPUT;
  XMoveWindow (XtDisplay (s->display.x->widget),
	       XtWindow (s->display.x->widget),
	       s->display.x->widget->core.x,
	       s->display.x->widget->core.y);
  UNBLOCK_INPUT;
  /* Update the hints so that, if this window is iconified, it will come back
     at the same place.  We can't look at s->visible to determine this because
     it might not be up-to-date yet (the queue might not be processed.)
   */
  x_update_wm_hints (s->display.x->widget);
}

/* Call this to change the size of screen S's x-window. */

extern void EmacsScreenSetCharSize (Widget, int, int);
extern Widget lw_raise_all_pop_up_widgets (void);
extern void lw_set_keyboard_focus (Widget, Widget);

void
x_set_window_size (s, cols, rows)
     struct screen *s;
     int cols, rows;
{
  BLOCK_INPUT;
  EmacsScreenSetCharSize (s->display.x->edit_widget, cols, rows);
  UNBLOCK_INPUT;
}



void
x_set_mouse_position (s, x, y)
     struct screen *s;
     int x, y;
{
  int pix_x, pix_y;
  struct glyph_dimensions *dimensions;

  x_raise_screen (s, 0);

  if (x >= 0)
    dimensions = get_glyph_dimensions (s, s->phys_cursor_x, s->phys_cursor_y);
  if (!dimensions)
    return;

  if (x < 0)
    pix_x = PIXEL_WIDTH (s) / 2;
  else
    pix_x = dimensions->top_left_x + dimensions->width / 2;

  if (y < 0)
    pix_y = PIXEL_HEIGHT (s) / 2;
  else
    pix_y = (s->current_glyphs->top_left_y[y]
	     + s->current_glyphs->pix_height[y] / 2);

  BLOCK_INPUT;
  XWarpPointer (x_current_display, None, XtWindow (s->display.x->edit_widget),
	       0, 0, 0, 0, pix_x, pix_y);
  UNBLOCK_INPUT;
}


static int
should_raise_by_send_event;

static int
handle_raise_error (Display* display, XErrorEvent* event)
{
  should_raise_by_send_event = 1;
  return 0;
}

/* Raise screen S.  */
void
x_raise_screen (s, force)
     struct screen *s;
     int force;
{
  Widget bottom_dialog;
  Window emacs_window;
  XWindowChanges xwc;
  int flags;
  int (*prev_handler) (Display*, XErrorEvent*);

  BLOCK_INPUT;
  if (s->visible || force)
    {
      emacs_window = XtWindow (s->display.x->widget);
#ifdef ENERGIZE
      /* first raises all the dialog boxes, then put emacs just below the 
       * bottom most dialog box */
      bottom_dialog = lw_raise_all_pop_up_widgets ();
      if (bottom_dialog && XtWindow (bottom_dialog))
	{
	  xwc.sibling = XtWindow (bottom_dialog);
	  xwc.stack_mode = Below;
	  flags = CWSibling | CWStackMode;
	}
      else
#endif
	{
	  xwc.stack_mode = Above;
	  flags = CWStackMode;
	}

      /* get ready to handle the error generated by XConfigureWindow */
      XSync (x_current_display, False);
      should_raise_by_send_event = 0;
      prev_handler = XSetErrorHandler (handle_raise_error);
      /* first try the plain configure notify and detect an error */
      XConfigureWindow (x_current_display, emacs_window, flags, &xwc);

      /* try to see if some error is generated */
      XSync (x_current_display, False);
      XSetErrorHandler (prev_handler);
      if (should_raise_by_send_event)
	{
	  XConfigureRequestEvent ev;
	  Window root = RootWindow (x_current_display,
				    DefaultScreen (x_current_display));
	  ev.type = ConfigureRequest;
	  ev.parent = root;
	  ev.window = window;
	  ev.above = XtWindow (bottom_dialog);
	  ev.value_mask = flags;
	  XSendEvent (x_current_display, root, False,
		      SubstructureRedirectMask|SubstructureNotifyMask,
		      (XEvent *)&ev);
	}
    }
  UNBLOCK_INPUT;
}

/* Lower screen S.  */
void
x_lower_screen (s)
     struct screen *s;
{
  BLOCK_INPUT;
  if (s->visible)
    XLowerWindow (x_current_display, XtWindow(s->display.x->widget));
  UNBLOCK_INPUT;
}

/* Change from withdrawn state to mapped state. */
void
x_make_screen_visible (s)
     struct screen *s;
{
  BLOCK_INPUT;
  if (!s->visible)
    XMapRaised (x_current_display, XtWindow (s->display.x->widget));
  else
    x_raise_screen (s, 0);
  UNBLOCK_INPUT;
}

/* Change from mapped state to withdrawn state. */
void
x_make_screen_invisible (s)
     struct screen *s;
{
  if (! s->visible)
    return;

  /* Before unmapping the window, update the WM_SIZE_HINTS property to claim
     that the current position of the window is user-specified, rather than
     program-specified, so that when the window is mapped again, it will be
     placed at the same location, without forcing the user to position it
     by hand again (they have already done that once for this window.)
   */
  x_update_wm_hints (s->display.x->widget);

  BLOCK_INPUT;
  /* New function available with R4 */
  if (!XWithdrawWindow (x_current_display,
			XtWindow(s->display.x->widget),
			DefaultScreen (x_current_display)))
    {
      UNBLOCK_INPUT;
      error ("Can't notify window manager of iconification.");
    }
  
  UNBLOCK_INPUT;
}

/* Change window state from mapped to iconified. */
void
x_iconify_screen (s)
     struct screen *s;
{
  int result;
  BLOCK_INPUT;
  result = XIconifyWindow (x_current_display,
			   XtWindow(s->display.x->widget),
			   DefaultScreen (x_current_display));
  UNBLOCK_INPUT;

  if (!result)
    error ("Can't notify window manager of iconification.");

  s->iconified = 1;
}


extern Time mouse_timestamp;
extern int any_screen_has_focus_p;

/* Sets the X focus to screen s.  Should only be called from
   select_screen.  Will only set focus if any screen was given the focus
   by the WM. */
void
x_focus_screen (s)
     struct screen *s;
{
  XWindowAttributes xwa;
  struct x_display* x;

  if (!SCREEN_IS_X (s))
    return;

  x = s->display.x;

  if (!XtWindow (x->widget))
    return;

  BLOCK_INPUT;
  /* Always set the Xt keyboard focus. */
  lw_set_keyboard_focus (x->widget, x->edit_widget);

  /* Do the ICCCM focus change if we don't have focus already and
     the window is still visible.  The s->visible flag might not be
     up-to-date, because we might not have processed magic events
     recently.  So make a server round-trip to find out whether it's
     really mapped right now.  We grab the server to do this, because
     that's the only way to eliminate the race condition.
   */
  if (!x->focus_p && any_screen_has_focus_p)
    {
      XGrabServer (XtDisplay (x->widget));
      if (XGetWindowAttributes (XtDisplay (x->widget), XtWindow (x->widget),
				&xwa))
	s->visible = xwa.map_state == IsViewable;
      
      if (s->visible)
	{
	  Window focus;
	  int revert_to;
	  XGetInputFocus (XtDisplay (x->widget), &focus, &revert_to);
	  /* Don't explicitly set the focus on this window unless the focus
	     was on some other window (not PointerRoot).  Note that, even when
	     running a point-to-type window manager like *twm, there is always
	     a focus window; the window manager maintains that based on the
	     mouse position.  If you set the "NoTitleFocus" option in these
	     window managers, then the server itself maintains the focus via
	     PointerRoot, and changing that to focus on the window would make
	     the window grab the focus.  Very bad.
	   */
	  if (focus != PointerRoot)
	    {
	      XSetInputFocus (XtDisplay (x->widget), XtWindow (x->widget),
			      RevertToParent, mouse_timestamp);
	      XFlush (XtDisplay (x->widget));
	    }
	}
      XUngrabServer (XtDisplay (x->widget));
      XFlush (XtDisplay (x->widget)); /* hey, I'd like to DEBUG this... */
    }
  UNBLOCK_INPUT;
}

extern Lisp_Object WM_COMMAND_screen; /* in xfns.c */
extern Lisp_Object Vcommand_line_args;

/* Called from xfns.c when screens are created */
void
maybe_store_wm_command (struct screen *s)
{
  if (NILP (WM_COMMAND_screen))
    {
      Widget w = s->display.x->widget;
      int argc;
      char **argv;
      make_argc_argv (Vcommand_line_args, &argc, &argv);
      BLOCK_INPUT;
      XSetCommand (XtDisplay (w), XtWindow (w), argv, argc);
      xfree (argv);
      XSET (WM_COMMAND_screen, Lisp_Screen, s);
      UNBLOCK_INPUT;
    }
}

/* If we're deleting the screen on which the WM_COMMAND property has been
   set, then move that property to another screen so that there is exactly
   one screen that has that property set.
 */
static void
maybe_move_wm_command (struct screen *s)
{
  if (s == XSCREEN (WM_COMMAND_screen))
    {
      Lisp_Object rest = Vscreen_list;
      /* find some random other X screen that is not this one, or give up */
      while (!NILP (rest) &&
	     (s == XSCREEN (XCONS (rest)->car) ||
	      !SCREEN_IS_X (XSCREEN (XCONS (rest)->car))))
	rest = XCONS (rest)->cdr;
      if (NILP (rest)) return;
      s = XSCREEN (XCONS (rest)->car);
      WM_COMMAND_screen = Qnil;
      maybe_store_wm_command (s);
    }
}


/* Destroy the X window of screen S.
   DISPL is the former s->display (since s->display
   has already been nulled out).  */
void
x_destroy_window (s)
     struct screen *s;
{
  maybe_move_wm_command (s);
  BLOCK_INPUT;
  XtDestroyWidget(s->display.x->widget);
  UNBLOCK_INPUT;
  
  xfree (s->display.x);
#if 0
  * Punt on this for a while
 *  /* If we don't own this window (we didn't create it) then we can't destroy
 *     it.
 *     We set the window to be the external_window, so that XtDestroyWidget
 *     won't call XDestroyWindow.  Had we just set the window to 0,
 *     XtDestroyWidget would only have done half the job. 
 *     We also have to check if the parent window belongs to a noteWidget
 *     and then remove us from this note. 
 *     This is among the worse code I ever wrote. --Matthieu.*/
 *  {int destroyed_p = False;
 *   if (displ.x->own_window != True) 
 *     {
 *       Window root;
 *       Window parent;
 *       Window* children;
 *       unsigned int n_children;
 *       Widget parent_widget;
 *       
 */*       displ.x->window_desc = 0; */
 *       displ.x->widget->core.window =
 *	 ((EmacsShellWidget)displ.x->widget)->emacsShell.external_window;
 *       
 *       if (XQueryTree (XtDisplay (displ.x->widget),
 *		       XtWindow (displ.x->widget), 
 *		       &root, &parent, &children, &n_children)){
 *	 parent_widget = XtWindowToWidget (x_current_display, parent);
 *	 if (parent_widget){
 *	   destroyed_p = True;
 *	   set_text_widget ((NoteWidget)parent_widget, 0);
 *	 }
 *       }
 *     }else
 *       /* this will tell the emacs shell to destroy its window */
 *       ((EmacsShellWidget)displ.x->widget)->emacsShell.external_window = 0;
 * }
#endif
}

#endif /* HAVE_X_WINDOWS */
