/* X Communication module for terminals which understand the X protocol.
   Copyright (C) 1989, 1991, 1992 Free Software Foundation, Inc.

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

#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xmu/Error.h>
#include <X11/IntrinsicP.h>	/* CoreP.h needs this */
#include <X11/CoreP.h>		/* foul, but we need this to use our own
				   window inside a widget instead of one 
				   that Xt creates... */
#include "EmacsShellP.h"

#ifdef ENERGIZE	/* Is this necessary? */
#include <notelib.h>
#endif

extern XtAppContext Xt_app_con;
extern Widget Xt_app_shell;

#ifdef HAVE_X_WINDOWS

#include "lisp.h"

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

#if 0			/* It shouldn't be necessary to include time.h here,
			   because we include xterm.h, which includes
			   X11/Intrinsic.h, which includes X11/Xos.h, which
			   includes time.h.  Some bogon systems (like HP)
			   don't protect their header files from double
			   inclusion, so including time.h twice blows up!
			 */

#ifdef NEED_TIME_H
#include <time.h>
#else /* not NEED_TIME_H */
#ifdef HAVE_TIMEVAL
#include <sys/time.h>
#endif /* HAVE_TIMEVAL */
#endif /* not NEED_TIME_H */

#endif /* 0 */


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


/* Nonzero if in redisplay ();  prevents us from calling it recursively */
int in_display;

/* Stuff for dealing with the title. */
extern Lisp_Object Vinvocation_name;
char *hostname, *id_name;

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

/* Symbol returned in input stream to indicate mouse movement. */
Lisp_Object mouse_moved_symbol;

static int curs_x, curs_y;

int x_display_cursor ();

/* Remember if the last cursor displayed was a bar or a box */
Lisp_Object last_bar_cursor;

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

void
XTupdate_begin (s)
     struct screen *s;
{	
  updating_screen = s;
  update_height = s->height;

  if (last_bar_cursor != Vbar_cursor)
    s->cursor_erased = x_display_cursor (s, 0);

#ifdef ENERGIZE
  BLOCK_INPUT;
  xc_begin_note_update (s->display.x->edit_widget);
  UNBLOCK_INPUT;
#endif
}

extern Lisp_Object text_part_sym;
extern Lisp_Object modeline_part_sym;

static Lisp_Object notice_mouse_movement ();

enum window_type
{
  scrollbar_window,
  text_window
};

/* Nonzero when emacs is garbage-collecting. */
extern int gc_in_progress;

void
XTupdate_end (s)
     struct screen *s;
{	
  int drop;

  BLOCK_INPUT;
#ifdef ENERGIZE
  xc_flush_note_update (s->display.x->edit_widget);
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

void
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


static void
reverse_all_attributes (struct screen *s, Pixel foreground, Pixel background)
{
#ifdef ENERGIZE
  int i;
  Pixel fore;
  DisplayContext* display_context = s->display_context;

  attributes reversed_attributes;
  reversed_attributes.font = display_context->current_attributes.font;
  reversed_attributes.foreground = foreground;
  reversed_attributes.background = background;
  reversed_attributes.back_pixmap = 
    display_context->current_attributes.back_pixmap;
  
  BLOCK_INPUT;
  UpdateGCFromAttributes (display_context, &reversed_attributes);
  UNBLOCK_INPUT;
  
  for (i = 0; i < display_context->num_attributes; i++)
    {
      if (display_context->attributes_array [i].resourced)
	{
	  fore = display_context->attributes_array [i].foreground;
	  display_context->attributes_array [i].foreground =
	    display_context->attributes_array [i].background;
	  display_context->attributes_array [i].background = fore;	  
	}
    }
#endif /* ENERGIZE */
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
{
#ifdef ENERGIZE
  DisplayContext* display_context = s->display_context;
#endif
  Window x_win = XtWindow (s->display.x->edit_widget);
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
    this_font = s->display.x->font;

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

			XGetGCValues (x_current_display, drawing_gc,
				      GCBackground | GCForeground,
				      &values);

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

			XGetGCValues (x_current_display, drawing_gc,
				      GCStipple | GCFillStyle, &old_values);
			XGetGCValues (x_current_display,
				      face_list[this_run].faceptr->facegc,
				      GCStipple | GCFillStyle, &new_values);
			XSetFillStyle (x_current_display, drawing_gc,
				       FillOpaqueStippled);
			XSetStipple (x_current_display, drawing_gc,
				     new_values.stipple);
			XFillRectangle (x_current_display, x_win, drawing_gc,
					pix_x, pix_y, this_pix_len, line_height);
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
		    XGCValues old_values, new_values;

		    XGetGCValues (x_current_display, drawing_gc,
				  GCStipple | GCFillStyle, &old_values);
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

		    XGetGCValues (x_current_display, drawing_gc,
				  GCBackground | GCForeground,
				  &values);

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
		
#ifdef ENERGIZE
		if (x_bitmaps[index].cimage)
		  {
		    XGCValues gcv;
		    GC ogc = display_context->gc;
		    unsigned long ofg, obg;
		    
		    BLOCK_INPUT;
		    XGetGCValues (x_current_display, ogc,
				  GCBackground|GCForeground, &gcv);
		    ofg = gcv.foreground;
		    obg = gcv.background;
		    gcv.foreground = s->display.x->line_info_foreground_pixel;
		    gcv.background = s->display.x->line_info_background_pixel;
		    XChangeGC (x_current_display, ogc,
			       GCBackground|GCForeground, &gcv);
		    
		    display_context->widget = s->display.x->edit_widget;
		    display_context->window = XtWindow (s->display.x->edit_widget);
		    display_context->view_id = 1;
		    display_context->x = ibw+1;
		    display_context->y = y;
		    display_context->region = 0;
		    
		    DisplayImage (x_bitmaps[index].cimage, display_context);
		    gcv.foreground = ofg;
		    gcv.background = obg;
		    XChangeGC (x_current_display, ogc,
			       GCBackground|GCForeground, &gcv);
		    
		    UNBLOCK_INPUT;
		  }
		else
#endif /* ENERGIZE */
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
	    int index = s->current_glyphs->glyphs[top][left];
	    int height = x_bitmaps[index].height;
	    int bitmap_y_offset = 0;
	    int y = pix_y;
#ifdef LINE_INFO_WIDGET
	    int use_lineinfo = (s->display.x->lineinfo_widget &&
				XtIsManaged (s->display.x->lineinfo_widget));
#endif
	    int real_pix_x = pix_x;

            if (!x_bitmaps[index].width || !x_bitmaps[index].height)
              {
                maybe_abort ();
		break;
              }

	    /* Center the glyph vertically in the display line. */
	    if (height < line_height)
	      {
		XClearArea (x_current_display, x_win,
			    pix_x, pix_y, this_pix_len,
			    line_height, False);
		y += ((line_height - height) / 2);
	      }
	    else if (height > line_height)
	      {
		bitmap_y_offset += ((height - line_height) / 2);
		height -= (height - line_height);
	      }

	    if (force_gc)
	      {
		XGCValues values, old_values;

		XGetGCValues (x_current_display, drawing_gc,
			      GCForeground | GCBackground,
			      &old_values);
#ifdef ENERGIZE
		if (x_bitmaps[index].cimage)
		  {
		    Pixel old_fore =
		      display_context->current_attributes.foreground;
		    Pixel old_back =
		      display_context->current_attributes.background;
		    reverse_all_attributes (s, old_values.foreground,
					    old_values.background);

#ifdef LINE_INFO_WIDGET
		    if (use_lineinfo) {
		      display_context->widget = s->display.x->lineinfo_widget;
		      display_context->window =
			XtWindow (s->display.x->lineinfo_widget);
		    } else
#endif
		    {
		      display_context->widget = s->display.x->edit_widget;
		      display_context->window = XtWindow (s->display.x->edit_widget);
		    }
		    display_context->view_id = 1;
/* Just always use the display context gc */
/*		    display_context->gc = drawing_gc; */
		    display_context->x = pix_x;
		    display_context->y = y;
		    display_context->region = 0;
		    BLOCK_INPUT;
		    DisplayImage (x_bitmaps[index].cimage, display_context);
		    UNBLOCK_INPUT;

		    reverse_all_attributes (s, old_fore, old_back);

		  } else
#endif /* ENERGIZE */
		    {
		    values.background = old_values.foreground;
		    values.foreground = old_values.background;
		    XChangeGC (x_current_display, drawing_gc,
			       GCForeground | GCBackground,
			       &values);
		    XCopyPlane (x_current_display,
				x_bitmaps[index].image,
				x_win, drawing_gc,
				0, bitmap_y_offset, this_pix_len, height,
				pix_x, y, 1L);
		    XChangeGC (x_current_display, drawing_gc,
			       GCForeground | GCBackground,
			       &old_values);
		  }
	      }
	    else
#ifdef ENERGIZE
	      if (x_bitmaps[index].cimage)
		  {
#ifdef LINE_INFO_WIDGET
		    if (use_lineinfo) {
		      display_context->widget = s->display.x->lineinfo_widget;
		      display_context->window =
			XtWindow (s->display.x->lineinfo_widget);
		    } else
#endif
		    {
		      display_context->widget = s->display.x->edit_widget;
		      display_context->window = XtWindow (s->display.x->edit_widget);
		    }

		    display_context->view_id = 1;
/* Just always use the display_context gc */
/*		    display_context->gc = drawing_gc; */
		    display_context->x = pix_x;
		    display_context->y = y;
		    display_context->region = 0;
		    BLOCK_INPUT;
		    DisplayImage (x_bitmaps[index].cimage, display_context);
		    UNBLOCK_INPUT;
		}
	      else
#endif /* ENERGIZE */
		  XCopyPlane (x_current_display,
			      x_bitmaps[index].image,
			      x_win, drawing_gc,
			      0, bitmap_y_offset, this_pix_len, height,
			      pix_x, y, 1L);
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

	    XGetGCValues (x_current_display, drawing_gc,
			  GCFillStyle | GCForeground | GCBackground,
			  &ovalues);
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
	this_font = s->display.x->font;
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

    XGetGCValues (x_current_display, gc, GCForeground|GCBackground, &gcv);
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

void
XTwrite_glyphs (hpos, vpos, len)
     int hpos, vpos, len;
{
  int temp_length;
  int mask;
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

void
XTclear_end_of_line (first_unused)
     int first_unused;
{
  struct screen *s = updating_screen;
  int pix_x, pix_y, pix_width, pix_height, nruns, i;
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

void
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

/* Paint horzontal bars down the screen for a visible bell.
   Note that this may be way too slow on some machines. */

void
XTflash (s)
     struct screen *s;
{
  struct screen_glyphs *active_screen = SCREEN_CURRENT_GLYPHS (s);
  int i;
  int x, y;

  if (updating_screen != 0)
    return;

  BLOCK_INPUT;
  x = (s->width * FONT_WIDTH (s->display.x->font)) / 4;
  y = (s->height * (s->display.x->text_height + x_interline_space)) / 4;
  XFillRectangle (x_current_display, XtWindow (s->display.x->edit_widget),
		  s->display.x->cursor_gc,
		  x, y, 2 * x, 2 * y);
  XFlushQueue ();
  UNBLOCK_INPUT;
}

/* Make audible bell.  */

/* X defines volume as from -100 to 100; we use 0 to 100 */
extern Lisp_Object Vbell_volume;

void XTsimple_beep (volume)
     int volume;
{
  if (volume < 0) volume = 0;
  else if (volume > 100) volume = 100;
  BLOCK_INPUT;
  XBell (x_current_display, (volume * 2) - 100);
  XFlushQueue ();
  UNBLOCK_INPUT;
}

void
XTring_bell (sound)
     Lisp_Object sound;
{
  if (visible_bell)
    {
      XTflash (selected_screen);
      redraw_screen (selected_screen);
    }
  else
    Fplay_sound (sound, Qnil);
}

/* Specify how many text lines, from the top of the window,
   should be affected by insert-lines and delete-lines operations.
   This, and those operations, are used only within an update
   that is bounded by calls to XTupdate_begin and XTupdate_end.  */

void
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
      xc_note_parent_scrolled (s->display.x->edit_widget,
			       from_x, from_y, width, copy_height, 
			       to_x - from_x, to_y - from_y);
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
      xc_note_parent_scrolled (s->display.x->edit_widget,
			       from_x, from_y, width, height, 
			       to_x - from_x, to_y - from_y);
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

void
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


/* Symbols returned in the input stream to indicate various X events. */
Lisp_Object mapped_screen_symbol;
Lisp_Object unmapped_screen_symbol;
Lisp_Object exited_window_symbol;
Lisp_Object redraw_screen_symbol;

extern Lisp_Object Qeval;

/* This function is invoked by each menu item which Emacs should handle
   directly.  It is passed the widget of the menu item.  It takes the
   name of that widget ("open", "quit", etc) and effectively does
   (funcall (intern (concat "Menubar-" item-name)))
   The appropriate functions should be defined in lisp code.
 */
void emacs_menu_callback (w, client_data, call_data)
     Widget w;
     caddr_t client_data, call_data;
{
  char *name = w->core.name;
  char *prefix = "Menubar-";
  char *string = malloc (255 * sizeof(char));
  Lisp_Object symbol, event;
  
  strcpy (string, prefix);
  strcpy ((string+strlen(prefix)), name);
  symbol = intern (string);
  free (string);
  event = Fallocate_event ();
  XEVENT (event)->event_type = menu_event;
  XEVENT (event)->event.eval.function = Qcall_interactively;
  XEVENT (event)->event.eval.object = symbol;
  enqueue_command_event (event);
}

/* This function is invoked by the Energize menu items that are handled 
   by emacs.
   #### This should be renamed to energize_menu_callback, but right now
   libxdui.a knows it by the old name.
 */
void cadillac_menu_callback (w, client_data, call_data)
     Widget w;
     caddr_t client_data, call_data;
{
  emacs_menu_callback (w, client_data, call_data);
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
  int height, width, x, y;
  XGCValues values;
  
  BLOCK_INPUT;
  XGetGCValues (x_current_display, s->display.x->cursor_gc,
		GCBackground | GCForeground, &values);

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
  int height, width, x, y;
  XGCValues values;
  
  BLOCK_INPUT;
  XGetGCValues (x_current_display, s->display.x->cursor_gc,
		GCBackground | GCForeground, &values);

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
  int height, width, x, y;

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

int
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

int
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
int
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
      Fkill_emacs (make_number (70));
    }
  return 0; /* not reached; suppress warnings */
}


static void
init_input_symbols ()
{
  int i;
  mapped_screen_symbol = intern ("mapped-screen");
  unmapped_screen_symbol = intern ("unmapped-screen");
  exited_window_symbol = intern ("exited-window");
  redraw_screen_symbol = intern ("redraw-screen");
  mouse_moved_symbol = intern ("mouse-moved");
}

/* Initialize communication with the X window server.  */

#ifdef ENERGIZE
static int x_bitmaps_length;
struct x_bitmap *x_bitmaps;
struct x_bitmap *x_stipples;
#else
struct x_bitmap x_bitmaps[8];
struct x_bitmap x_stipples[8];
#endif

#ifdef ENERGIZE
int
reasonable_glyph_index_p (index)
     int index;
{
  if (index < 0 || index >= x_bitmaps_length
      || x_bitmaps[index].image == 0)
    return 0;
  return 1;
}

static void
allocate_x_bitmaps (size)
     int size;
{
  if (size <= 0) size = 128;

  if (x_bitmaps_length >= size) 
    return;
  else if (x_bitmaps_length == 0)
    {
      int byte_size = size * sizeof (struct x_bitmap);
      x_bitmaps = (struct x_bitmap *) xmalloc (byte_size);
      x_stipples = (struct x_bitmap *) xmalloc (byte_size);
      bzero ((char *) x_bitmaps, byte_size);
      bzero ((char *) x_stipples, byte_size);
      x_bitmaps_length = size;
    }
  else
    {
      int byte_size = size * sizeof (struct x_bitmap);
      int byte_offset = x_bitmaps_length * sizeof (struct x_bitmap);
      x_bitmaps = (struct x_bitmap *) xrealloc (x_bitmaps, byte_size);
      x_stipples = (struct x_bitmap *) xrealloc (x_stipples, byte_size);
      bzero (((char *) x_bitmaps) + byte_offset , byte_size - byte_offset);
      bzero (((char *) x_stipples) + byte_offset , byte_size - byte_offset);
      x_bitmaps_length = size;
    }
  return;
}

/* make sure that something is put in this slot in init_bitmap_tables(),
   below -- if the entry is 0, then we will lose BIG */
#define DUMMY_GLYPH_INDEX COMPRESS_BITMAP

int 
possible_energize_glyph_index_p (index)
     int index;
{
  if ((index >= PREDEFINED_BITMAPS_UPPER_BOUND) && (index < x_bitmaps_length))
    return 1;
  else
    return 0;
}

int 
allocate_x_bitmaps_index ()
{
  int i;

  if (x_bitmaps_length <= 0) error ("x_bitmaps vector not allocated");

  for (i = PREDEFINED_BITMAPS_UPPER_BOUND; i < x_bitmaps_length; i++)
    {
      if ((i != DUMMY_GLYPH_INDEX) &&
          (!x_bitmaps[i].image || 
           (x_bitmaps[i].image == x_bitmaps[DUMMY_GLYPH_INDEX].image)))
        return i;
    }

  i = x_bitmaps_length;
  allocate_x_bitmaps (2 * x_bitmaps_length);
  return i;
}
       
void
free_all_x_bitmaps_indices ()
{
  int i;

  for (i = PREDEFINED_BITMAPS_UPPER_BOUND; i < x_bitmaps_length; i++)
    {
      if (i != DUMMY_GLYPH_INDEX)
        x_bitmaps[i] = x_bitmaps[DUMMY_GLYPH_INDEX];
    }
  return;
}

void
free_x_bitmaps_index (index)
     int index;
{
  if (x_bitmaps_length <= 0) error ("x_bitmaps vector not allocated");

  if ((index < 0) || 
      (index == DUMMY_GLYPH_INDEX) || 
      (index >= x_bitmaps_length))
    error ("x_bitmaps[%d]: index %d can't be freed", 
           x_bitmaps_length, index);

  x_bitmaps[index] = x_bitmaps[DUMMY_GLYPH_INDEX];

  return;
}
#endif

static void
init_bitmap_tables ()
{
#ifdef ENERGIZE
  allocate_x_bitmaps (128);
#endif
  
  BLOCK_INPUT;
  x_bitmaps[COMPRESS_BITMAP].image
    = XCreateBitmapFromData (x_current_display,
			     DefaultRootWindow (x_current_display),
			     compress_bits,
			     compress_width, compress_height);
  x_bitmaps[COMPRESS_BITMAP].width = compress_width;
  x_bitmaps[COMPRESS_BITMAP].height = compress_height;

  x_bitmaps[IBEGIN_BITMAP].image
    = XCreateBitmapFromData (x_current_display,
			     DefaultRootWindow (x_current_display),
			     extent_begin_bits,
			     extent_begin_width, extent_begin_height);
  x_bitmaps[IBEGIN_BITMAP].width = extent_begin_width;
  x_bitmaps[IBEGIN_BITMAP].height = extent_begin_height;

  x_bitmaps[IEND_BITMAP].image
    = XCreateBitmapFromData (x_current_display,
			     DefaultRootWindow (x_current_display),
			     extent_end_bits,
			     extent_end_width, extent_end_height);
  x_bitmaps[IEND_BITMAP].width = extent_end_width;
  x_bitmaps[IEND_BITMAP].height = extent_end_height;

  x_bitmaps[CONTINUER_BITMAP].image
    = XCreateBitmapFromData (x_current_display,
			     DefaultRootWindow (x_current_display),
			     continuer_bits,
			     continuer_width, continuer_height);
  x_bitmaps[CONTINUER_BITMAP].width = continuer_width;
  x_bitmaps[CONTINUER_BITMAP].height = continuer_height;

  x_bitmaps[TRUNCATOR_BITMAP].image
    = XCreateBitmapFromData (x_current_display,
			     DefaultRootWindow (x_current_display),
			     truncator_bits,
			     truncator_width, truncator_height);
  x_bitmaps[TRUNCATOR_BITMAP].width = truncator_width;
  x_bitmaps[TRUNCATOR_BITMAP].height = truncator_height;

  x_bitmaps[RARROW_BITMAP].image
    = XCreateBitmapFromData (x_current_display,
			     DefaultRootWindow (x_current_display),
			     rarrow_bits,
			     rarrow_width, rarrow_height);
  x_bitmaps[RARROW_BITMAP].width = rarrow_width;
  x_bitmaps[RARROW_BITMAP].height = rarrow_height;

  x_stipples[SELECTION_STIPPLE].image
    = XCreateBitmapFromData (x_current_display,
			     DefaultRootWindow (x_current_display),
			     selection_bits,
			     selection_width, selection_height);
  x_stipples[SELECTION_STIPPLE].width = selection_width;
  x_stipples[SELECTION_STIPPLE].height = selection_height;

  x_stipples[SECONDARY_SELECTION_STIPPLE].image
    = XCreateBitmapFromData (x_current_display,
			     DefaultRootWindow (x_current_display),
			     secondary_selection_bits,
			     secondary_selection_width,
			     secondary_selection_height);
  x_stipples[SECONDARY_SELECTION_STIPPLE].width = secondary_selection_width;
  x_stipples[SECONDARY_SELECTION_STIPPLE].height = secondary_selection_height;

  x_stipples[OVERLAP_SELECTION_STIPPLE].image
    = XCreateBitmapFromData (x_current_display,
			     DefaultRootWindow (x_current_display),
			     overlap_selection_bits,
			     overlap_selection_width,
			     overlap_selection_height);
  x_stipples[OVERLAP_SELECTION_STIPPLE].width = overlap_selection_width;
  x_stipples[OVERLAP_SELECTION_STIPPLE].height = overlap_selection_height;

  x_stipples[DEFAULT0_STIPPLE].image
    = XCreateBitmapFromData (x_current_display,
			     DefaultRootWindow (x_current_display),
			     default0_stipple_bits,
			     default0_stipple_width, default0_stipple_height);
  x_stipples[DEFAULT0_STIPPLE].width = default0_stipple_width;
  x_stipples[DEFAULT0_STIPPLE].height = default0_stipple_height;

  x_stipples[DEFAULT1_STIPPLE].image
    = XCreateBitmapFromData (x_current_display,
			     DefaultRootWindow (x_current_display),
			     default1_stipple_bits,
			     default1_stipple_width, default1_stipple_height);
  x_stipples[DEFAULT1_STIPPLE].width = default1_stipple_width;
  x_stipples[DEFAULT1_STIPPLE].height = default1_stipple_height;
  UNBLOCK_INPUT;
}

static void
report_broken_pipe (sig)
     int sig;
{
  return;
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


Lisp_Object
x_term_init (argv_list)
     Lisp_Object argv_list;
{
  int argc;
  char** argv;
  int fd;

#ifdef F_SETOWN
  extern int old_fcntl_owner;
#endif
  make_argc_argv (argv_list, &argc, &argv);

  Xt_app_shell = XtAppInitialize (&Xt_app_con, "Emacs",
				  emacs_options, XtNumber(emacs_options),
				  &argc, argv,
				  0, NULL, 0);
  x_current_display = XtDisplay (Xt_app_shell);

  if (x_current_display == 0)
    fatal ("X server not responding; check the DISPLAY environment variable or use \"-d\"\n");
  
  x_file_descriptor = ConnectionNumber (x_current_display);

  /* In event-Xt.c */
  x_init_modifier_mapping (x_current_display);
#ifdef ENERGIZE
  xc_init ();
#endif
  
  {
    char tem_name[MAXPATHLEN];
    int l;

    gethostname (tem_name, MAXPATHLEN - 1);
    tem_name[99] = 0;
    if ((l = strlen (tem_name)) < MAXPATHLEN -1)
      {
	hostname = (char *) xmalloc (l+1);
	bcopy (tem_name, hostname, l + 1);
      }
    else
      {
	hostname = (char *) xmalloc (10);
	bcopy ("Somewhere", hostname, 9);
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

#ifdef ENERGIZE
  x_bitmaps_length = 0;
#endif
  init_bitmap_tables ();
  init_input_symbols ();

  XSetErrorHandler (x_error_handler);
  XSetIOErrorHandler (x_IO_error_handler);

  emacs_Xt_make_event_stream ();

  /* Disable Window Change signals;  they are handled by X events. */
#ifdef SIGWINCH
  signal (SIGWINCH, SIG_IGN);
#endif /* SIGWINCH */

  signal (SIGPIPE, report_broken_pipe);

  return make_arg_list (argc, argv);
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
      bzero ((char *) &hints, sizeof (hints));
      hints.flags = USPosition;
    }
  XSetWMNormalHints (XtDisplay (widget), XtWindow (widget), &hints);
  UNBLOCK_INPUT;
}


x_calc_absolute_position (s)
     struct screen *s;
{
  int screen_w = XINT (x_screen_width);
  int screen_h = XINT (x_screen_height);
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
  Window bottom_dialog;
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
      bottom_dialog = xc_raise_dialogs ();
      if (bottom_dialog)
	{
	  xwc.sibling = bottom_dialog;
	  xwc.stack_mode = Below;
	  flags = CWSibling | CWStackMode;
	}
      else
	{
	  xwc.stack_mode = Above;
	  flags = CWStackMode;
	}
#endif

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
	  ev.above = bottom_dialog;
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
    }
  UNBLOCK_INPUT;
}

/* Destroy the X window of screen S.
   DISPL is the former s->display (since s->display
   has already been nulled out).  */
void
x_destroy_window (s, displ)
     struct screen *s;
     union display displ;
{
  BLOCK_INPUT;
  XtDestroyWidget(displ.x->widget);
  UNBLOCK_INPUT;
  
  free (displ.x);
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
