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
#include <sys/time.h>
#include <X11/Xlib.h>
#include <X11/Xmu/Error.h>
#include <X11/IntrinsicP.h>	/* CoreP.h needs this */
#include <X11/CoreP.h>		/* foul, but we need this to use our own
				   window inside a widget instead of one 
				   that Xt creates... */
#include "EmacsShellP.h"
#include "EmacsScreen.h"
#include "EmacsScreenP.h"	/* to get cursor foreground, sigh */

#include "hash.h"

#include "dispextern.h"
#include "dispmisc.h"
#include "faces.h"

#if 1
#include "xgccache.h"
#endif

extern XtAppContext Xt_app_con;
extern Widget Xt_app_shell;

#ifdef HAVE_X_WINDOWS

/* On 4.3 this loses if it comes after xterm.h.  */
#include <signal.h>

/* This may include sys/types.h, and that somehow loses
   if this is not done before the other system files.  */
#include "xterm.h"
#include "xobjs.h"

#include "lwlib.h"

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
#ifndef HPUX
#include <sys/termio.h>
#endif /* HPUX */
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

#include "sysdep.h"             /* old_fcntl_owner */


#define min(a,b) ((a)<(b) ? (a) : (b))
#define max(a,b) ((a)>(b) ? (a) : (b))
#define focus_cursor_p(s) \
	(((s) == selected_screen) && ((s)->display.x->focus_p))

#define NO_CURSOR	0
#define NORMAL_CURSOR	1
#define THIN_CURSOR	2


/* Nonzero after BLOCK_INPUT; prevents input events from being
   processed until later.  */
int interrupt_input_blocked;

/*
#if defined (SIGIO) && defined (FIONREAD)
int BLOCK_INPUT_mask;
#endif
*/


/* Stuff for dealing with the title. */
static char *hostname;
static char *id_name;

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
extern struct screen *updating_screen;

/* During an update, maximum vpos which ins/del of lines
   may affect.  This is  specified by higher levels.

   This is the number of lines, from the top of screen downwards,
   which can participate in insert-line/delete-line operations.

   Effectively it excludes the bottom screen_height - specified_window_size
   lines from those operations.  */
static int update_height;

/* Number of pixels below each line. */
int x_interline_space;

/* Nonzero enables some debugging for the X interface code. */
extern int _Xdebug;

Lisp_Object Vlucid_logo;

#ifdef LINE_INFO_COLUMN
void x_draw_lineinfo_border(), x_clear_lineinfo_glyph();
#endif

/* Remember if the last cursor displayed was a bar or a box */
/* static Lisp_Object Vlast_bar_cursor; */

extern find_window (struct window *w, Lisp_Object win);

static int CursorToggle (struct screen *s);
static void ClearCursor (struct screen *s);
static void dump_windows (Lisp_Object window, 
                          int top, int left, int rows, int cols);
static void dump_window (Lisp_Object window, 
                         int top, register left, int rows, int cols);
static void PlotTextLine (struct window *w, struct line_header *l,
                          struct char_block *start, struct char_block *end,
                          char clear, int line_type);
static void ShipOutTextBlock (unsigned char *str, int count, 
                              int x, int y, int a, int d,
                              int cursor, struct face *face, 
                              struct window *w);
static void ShipOutGlyphBlock (GLYPH index, int x, int y, int a, int d,
                               int cursor, struct face *face, 
                               struct window *w);
static Bool ShipOutBlankBlock (Bool margin, int width, int x, int y, int a,
			       int d, int cursor, struct face *face, 
                               struct window *w);
static void XTcolor_area (struct screen *s, unsigned long color, int x, int y,
			  int width, int height);
static void XTclear_window_end (struct window *w, int ypos1, int ypos2);
static void XTshift_region (struct window *w, struct line_header *start,
                            struct line_header *end);
static void XTcursor_to (struct line_header *l, struct char_block *cb, 
                         int row, int col, 
                         struct window *win, struct screen *s);
static void InsertChar (struct window *w, struct line_header *l, 
                        struct char_block *new,
                        struct char_block *cb, struct char_block *end, 
                        char clear);
/* Get it past C type-check */
static int XTtext_width (Lisp_Object, const unsigned char *,
			 int);

/* These hooks are called by update_screen at the beginning and end
   of a screen update.  We record in `updating_screen' the identity
   of the screen being updated, so that the XT... functions do not
   need to take a screen as argument.  Most of the XT... functions
   should never be called except during an update, the only exceptions
   being XTcursor_to, and XTwrite_glyphs.  */

void
CXTupdate_begin (w)
     struct window *w;
{	
  SCREEN_PTR s = XSCREEN(w->screen);

  updating_screen = s;
  update_height = SCREEN_HEIGHT(updating_screen);

  BLOCK_INPUT;
  if (!s->cursor_erased && w == XWINDOW(s->selected_window))
    {
      CursorToggle (s);
    }
  UNBLOCK_INPUT;

/*
  if (!EQ (Vlast_bar_cursor, Vbar_cursor))
    s->cursor_erased = x_display_cursor (s, 0);
*/
}

static void
XTupdate_begin (struct screen *s) /* >>> arg ignored */
{
  CXTupdate_begin (XWINDOW (selected_window));
}

enum window_type
{
  scrollbar_window,
  text_window
};

/* Nonzero when emacs is garbage-collecting. */
extern int gc_in_progress;

void
CXTupdate_end (w)
     struct window *w;
{
  SCREEN_PTR s = XSCREEN(w->screen);

  BLOCK_INPUT;

  if (!s->cursor_erased && s == selected_screen &&
      w == XWINDOW(s->selected_window))
    {
      CursorToggle (s);
    }
  updating_screen = 0;

  XFlushQueue ();
  UNBLOCK_INPUT;
}

static void
XTupdate_end (struct screen *s) /* >>> ignores arg */
{
  CXTupdate_end (XWINDOW(selected_window));
}



/*
 * Fastmove_cursor - called from output_for_insert().  Note:  in this case,
 * a character has just been inserted (overwriting past cursor), and cursor
 * needs to be redrawn in its new spot.
 */
void
Fastmove_cursor(struct screen *s)
{
  /* Fudge so we are toggling the cursor BACK into existence. */

  s->cursor_erased = 1;
  s->phys_cursor_x = s->cursor_x;
  BLOCK_INPUT;
  CursorToggle(s);
  XFlushQueue ();
  UNBLOCK_INPUT;
}



/*
 * XTcursor_to moves the cursor to the correct location and checks whether an
 * update is in progress in order to toggle it on.
 */
void
XTcursor_to (struct line_header *l, struct char_block *cb, int row,
	     int col, struct window *win, struct screen *s)
{
  /* #### kludge; this can be called from reset_sys_modes after the X
     display has gone away because of KillClient... */
  if (x_current_display == 0) return;

  BLOCK_INPUT;

  if (updating_screen)
    {
      /* Cursor is already dead.  Now put it in its place. */
      s->cur_w = win;
      s->cur_line = l;
      s->cur_char = cb;
      s->phys_cursor_x = col;
      s->phys_cursor_y = row;       
      if (!s->cursor_erased)
	CursorToggle (s);
      UNBLOCK_INPUT;
      return;
      /* Generally, XTmove_cursor will be invoked
       * when InUpdate with !CursorExists 
       * so that wasteful XFlush is not called
       */
    }
  if (s->cur_w &&
      (s->new_cur_w != s->cur_w &&
       s->cur_w != XWINDOW(s->minibuffer_window)) &&
      find_window(s->cur_w,s->root_window))
    {
      unsigned char a[2];
      struct face *face = s->cur_char->char_b ? &SCREEN_NORMAL_FACE(s)
	: &SCREEN_LEFT_MARGIN_FACE(s);
      a[0] = s->cur_char->ch == 0 ? ' ' : s->cur_char->ch;
      a[1] = 0;
	  
      /* UGLY:  Cursor's previous position may be in another window
       * still being displayed; this will result in a blank or stray
       * cursor left in that other window.  Find the window, and
       * erase the old cursor
       */
      if (s->cur_char->char_b)
	ShipOutTextBlock(a,1,
			 s->cur_char->xpos,s->cur_line->ypos,
			 s->cur_line->ascent,s->cur_line->descent,NO_CURSOR,
			 s->cur_char->face ? s->cur_char->face : face,
			 s->cur_w);
      else 
	ShipOutGlyphBlock(s->cur_char->glyph,
			  s->cur_char->xpos,s->cur_line->ypos,
			  s->cur_line->ascent,s->cur_line->descent,NO_CURSOR,
			  s->cur_char->face ? s->cur_char->face : face,
			  s->cur_w);

      s->cursor_erased = 1;
      s->cur_w = win;
      s->cur_line = l;
      s->cur_char = cb;
      s->phys_cursor_x = col;
      s->phys_cursor_y = row;
      CursorToggle(s);
      UNBLOCK_INPUT;
      return;
    }     
  if ((row == s->phys_cursor_y) && (col == s->phys_cursor_x))
    {
      s->cur_w = win;
      s->cur_line = l;
      s->cur_char = cb;
      if (s->cursor_erased)
	CursorToggle (s);
      XFlushQueue ();
      UNBLOCK_INPUT;
      return;
    }
  /*
   * First remove cursor from where it is
   */
  if (!s->cursor_erased)
    CursorToggle (s);
  /*
   * Now update ptrs to where cursor actually is
   */
  s->cur_w = win;
  s->cur_line = l;
  s->cur_char = cb;  
  s->phys_cursor_x = col;
  s->phys_cursor_y = row;
  /*
   * Now plot it
   */
  if (s->cursor_erased)
    CursorToggle (s);
  XFlushQueue();
  UNBLOCK_INPUT;

}



/*
 * Used for Expose events.  Have to get the text
 * back into the newly blank areas.
 */
void
Cdumprectangle (register int top, register int left, register int rows,
		register int cols, struct screen *s)
{
  if (s == selected_screen) ClearCursor(s);
  
  dump_windows (s->root_window,top,left,rows,cols);

  if (!updating_screen && s == selected_screen && s->cursor_erased)
    CursorToggle(s);

  return;
}



void
dump_windows (Lisp_Object window, register int top, register int left,
	      register int rows, register int cols)
{
  for (; !NILP(window); window = XWINDOW(window)->next)
    dump_window (window,top,left,rows,cols);
}


void
dump_window (Lisp_Object window, register int top, register int left,
	     register int rows, register int cols)
{
  struct window *w = XWINDOW(window);
  struct screen *s = XSCREEN(w->screen);
  struct line_header *l;
  struct char_block *cb,*end;
  int pixright = w->pixleft + w->pixwidth;
  int pixbot = w->pixtop + w->pixheight;
  int startx,starty,endx,endy,margin_x;

  if (!NILP(w->vchild))
    {
      dump_windows (w->vchild,top,left,rows,cols);
      return;
    }
  if (!NILP(w->hchild))
    {
      dump_windows (w->hchild,top,left,rows,cols);
      return;
    }
  if (NILP(w->buffer))
    abort();			/* No buffer...we're screwed */

  if (SCREENP(Vglobal_minibuffer_screen) && EQ(window,s->minibuffer_window)
      && s != XSCREEN(Vglobal_minibuffer_screen))
    return;

  /*
   * Find out if expose region intersects this window; if not, return.
   */
  if (left > pixright || (left + cols) < w->pixleft
      || top > pixbot || (top + rows) < w->pixtop)
    return;

  /*
   * Calc top and left of expose region for this window.
   */
  startx = max (w->pixleft, left);
  endx = min (pixright, left + cols);
  starty = max (w->pixtop, top);
  endy = min (pixbot, top + rows);
  margin_x = w->pixleft + LEFT_MARGIN (XBUFFER (w->buffer), s);

  /*
   * Clear the margin area to reset its background.
   */
  if (startx < margin_x)
    XTcolor_area (s, FACE_BG_PIXEL (&SCREEN_LEFT_MARGIN_FACE(s)),
		  startx, starty,
		  (min (margin_x, endx) - startx), (endy - starty));

  /*
   * Find starting line corresponding to this start ypos
   */
  l = w->lines;
  while (l && (l->ypos + l->descent) < starty)
    l = l->next;

  while (l && (l->ypos - l->ascent) <= endy)
    {
      if (l->in_display == -1)
	{
	  /* Line is being layed-out in redisplay process, so give it
	   * the left edge of expose region
	   */
	  l->in_display = startx;
	}
      else if (l->ypos)		/* Does line have vertical position? */
	{
	  cb = l->body;
	  while (cb && cb != l->end && (cb->xpos + cb->width) < startx)
	    cb = cb->next;
	  if (cb != l->end)
	    {
	      end = cb;
	      while (end && end != l->end && (end->xpos) <= endx)
		end = end->next;

	      /*
	       * Note we will not clear to the end of line here.  If
	       * expose event doesn't cover end of line, no need to plot
	       * (clear) it; if it does, it is clear anyway.
	       */
	      PlotTextLine (w,l,cb,end,0,BODY); /* Replot the line */
	    }
	  cb = l->margin_start;
	  while (cb && cb != l->margin_end && (cb->xpos + cb->width) < startx)
	    cb = cb->next;
	  if (cb != l->margin_end)
	    {
	      end = cb;
	      while (end && end != l->margin_end && (end->xpos <= endx))
		end = end->next;

	      PlotTextLine (w,l,cb,end,0,MARGIN); /* Replot the margin */
	    }
	}
      l = l->next;
    }
  /*
   * Check modeline for window
   */
  if (!EQ(window,s->minibuffer_window) && w->modeline)
    {
      l = w->modeline;

      if (l)
	{
	  cb = l->body;
	  while (cb && cb != l->end && (cb->xpos + cb->width) < startx)
	    cb = cb->next;
	  if (cb != l->end)
	    {
	      end = cb;
	      while (end && end != l->end && (end->xpos) <= endx)
		end = end->next;
	      PlotTextLine (w,l,cb,end,0,BODY); /* Replot the mode line */
	    }
	}
    }
  return;
}



/*
 * Artificially creating a cursor is hard, the actual position on the
 * screen (either where it is or last was) is tracked with vix_x,y.
 * Gnu Emacs code tends to assume a cursor exists in hardward at ws->cursor_x,y
 * and that output text will appear there.  During updates, the cursor is
 * supposed to be blinked out and will only reappear after the update
 * finishes.
 */
int
CursorToggle (struct screen *s)
{
  unsigned char a[2];
  int wid;
  struct window *w = XWINDOW(s->selected_window);
  Widget widget = s->display.x->edit_widget;
  Window x_win = XtWindow (widget);
  struct Lisp_Font *font = XFONT (SCREEN_DEFAULT_FONT (s));
  struct face *face = &SCREEN_NORMAL_FACE (s);

  if (!s->visible)
    {
      s->cursor_erased = 1;
      return 0;
      /* Currently the return values are not
       * used, but I could anticipate using
       * them in the future.
       */
    }

  if (s->phys_cursor_y < 0)
    {
      /* Not much can be done */
      XFlushQueue ();
      s->cursor_erased = 1;
      return 0;
    }

  /*
   * This may get called before any layout is done.  Blow if off in that
   * case.
   */

  if (!s->cur_line || !s->cur_char
      || s->cur_char->xpos < w->pixleft
      || s->cur_char->xpos > w->pixleft + w->pixwidth)
    {
      XFlushQueue ();
      s->cursor_erased = 1;
      return 0;
    }

  a[1] = 0;
  /* Epoch:
   * Potential problem:  cursors in minibuffer screen and current edit
   * screen.  If we are toggling cursor on screen other than current screen,
   * force it to not exist.
   */
  if (s != selected_screen) s->cursor_erased = 0;
  /* Die a horrible death, cursor */

  if (s->phys_cursor_x <= s->cur_line->chars)
    {
      a[0] = s->cur_char->ch == 0 ? ' ' : s->cur_char->ch;
      if (s->cur_char->face)
	face = s->cur_char->face;
      else if (s->cur_char->ch == 0 && s->cur_char->prev != 0)
	if (s->cur_char->prev->face)
	  face = s->cur_char->prev->face;
      if (face == &SCREEN_MODELINE_FACE(s))
	face = &SCREEN_NORMAL_FACE(s);
      font = XFONT (FACE_FONT (face));
      if (s->cur_char->char_b)
	wid = XTextWidth (font->font, (char *) a,1);
      else
	wid = XPIXMAP (glyph_to_pixmap (s->cur_char->glyph))->width;
      if (!s->cursor_erased)
	{
	  if (s->cur_char->char_b)
	    ShipOutTextBlock(a,1,
			     s->cur_char->xpos,s->cur_line->ypos,
			     s->cur_line->ascent,s->cur_line->descent,
			     NO_CURSOR, face,s->cur_w);
	  else 
	    ShipOutGlyphBlock(s->cur_char->glyph,
			      s->cur_char->xpos,s->cur_line->ypos,
			      s->cur_line->ascent,s->cur_line->descent,
			      NO_CURSOR, face,s->cur_w);
	}
      else
	{
	  /* ShipOutTextBlock worries about solid/hollow cursor */
	  if (s->cur_char->char_b)
	    ShipOutTextBlock(a,1,
			     s->cur_char->xpos,s->cur_line->ypos,
			     s->cur_line->ascent,s->cur_line->descent,
			     (s->cur_char->next == NULL ?
			      THIN_CURSOR : NORMAL_CURSOR),
			     face,s->cur_w);
	  else 
	    ShipOutGlyphBlock(s->cur_char->glyph,
			      s->cur_char->xpos,s->cur_line->ypos,
			      s->cur_line->ascent,s->cur_line->descent,
			      (s->cur_char->next == NULL ?
			       THIN_CURSOR : NORMAL_CURSOR),
			      face,s->cur_w);
	}
    }
  else
    {
      a[0] = s->cur_char->ch == 0 ? ' ' : s->cur_char->ch;
      if (s->cur_char->face)
	face = s->cur_char->face;
      else if (s->cur_char->ch == 0 && s->cur_char->prev != 0)
	if (s->cur_char->prev->face)
	  face = s->cur_char->prev->face;
      if (face == &SCREEN_MODELINE_FACE(s))
	face = &SCREEN_NORMAL_FACE(s);
      font = XFONT (FACE_FONT (face));
      if (s->cur_char->char_b)
	wid = XTextWidth (font->font, (char *) a, 1);
      else
	wid = XPIXMAP (glyph_to_pixmap (s->cur_char->glyph))->width;
      if (!s->cursor_erased)
	{
	  int height = s->cur_line->ascent ?
	    (s->cur_line->ascent + s->cur_line->descent) : font->height;

          if (!SCREENP(Vglobal_minibuffer_screen)
	      && s->cur_w == XWINDOW(s->minibuffer_window))
	    {
	      ;
	    }
	  else
	    {
	      XClearArea(x_current_display, x_win,
			 s->cur_char->xpos,
			 s->cur_line->ypos - s->cur_line->ascent,
			 wid,
			 height,0);
	    }
	}
      else if (!focus_cursor_p(s))
	{
#if 0
	  XGCValues values;

	  if (! XGetGCValues (x_current_display, s->display.x->cursor_gc,
			      GCBackground | GCForeground, &values))
	    abort();
	  XSetForeground (x_current_display, s->display.x->cursor_gc,
			  values.background);
	  XDrawRectangle (x_current_display, x_win, s->display.x->cursor_gc,
		       s->cur_char->xpos,
		       s->cur_line->ypos - s->cur_line->ascent,
		       wid - 1,
		       s->cur_line->ascent + s->cur_line->descent -1);
	  XSetForeground (x_current_display, s->display.x->cursor_gc,
			  values.foreground);
#endif
      }
      else
	{
	  if (s->cur_char->char_b)
	    ShipOutTextBlock(a,1,
			     s->cur_char->xpos,s->cur_line->ypos,
			     s->cur_line->ascent,s->cur_line->descent,
			     (s->cur_char->next == NULL ?
			      THIN_CURSOR : NORMAL_CURSOR),
			     face, s->cur_w);
	  else 
	    ShipOutGlyphBlock(s->cur_char->glyph,
			      s->cur_char->xpos,s->cur_line->ypos,
			      s->cur_line->ascent,s->cur_line->descent,
			      (s->cur_char->next == NULL ?
			       THIN_CURSOR : NORMAL_CURSOR),
			      face, s->cur_w);
	}
    }

  s->cursor_erased = !s->cursor_erased;

  if (!updating_screen)
    XFlushQueue();

  return 1;
}



/*
 * This routine is used by routines which are called to paint regions 
 * designated by Expose events.  If the cursor may be in the exposed
 * region, this routine makes sure it is gone so that dumprectangle can 
 * toggle it back into existance if dumprectangle is invoked when not in
 * the midst of a screen update.
 */
void
ClearCursor(struct screen *s)
{
  char a[1];
  int wid,height;
  struct Lisp_Font *font = XFONT (SCREEN_DEFAULT_FONT (s));
  
  BLOCK_INPUT;
  if (!s->visible)
    {
      s->cursor_erased = 1;
      UNBLOCK_INPUT;
      return;
    }
	
  if (s->phys_cursor_x >= s->width || s->phys_cursor_y < 0 ||
      s->phys_cursor_y >= s->height || !s->new_cur_char || !s->new_cur_line)
    {
      /* Not much can be done */
      s->cursor_erased= 1;
      UNBLOCK_INPUT;
      return;
    }

  a[0] = ' ';
  if (s->new_cur_char->face)
    font = XFONT (FACE_FONT (s->new_cur_char->face));
  wid = s->new_cur_char->ch == 0 ? XTextWidth (font->font, a, 1) : 
    s->new_cur_char->width;
  height = s->new_cur_line->ascent ?
    (s->new_cur_line->ascent + s->new_cur_line->descent) : font->height;

  XClearArea (x_current_display, XtWindow (s->display.x->edit_widget),
	      s->new_cur_char->xpos,
	      s->new_cur_line->ypos - s->new_cur_line->ascent,wid,
	      height,0);
  s->cursor_erased = 1;
  UNBLOCK_INPUT;
}



/*
 * Plot a line L (or portion of line from START to END) of text in window W.
 * START and END are considered, if non-zero
 */
void
PlotTextLine (struct window *w, struct line_header *l,
	      struct char_block *start, struct char_block *end, char clear,
	      int line_type)
{
  struct screen *s = XSCREEN(w->screen);
  unsigned char buf[1000];	/* Buffer for constructing string. */
  unsigned char *pos;		/* Position in buf */
  struct char_block *cb;	/* Current char in line */
  struct face *f;		/* Current style for plotting */
  int n = 0;			/* char count for current region */
  int xpos;			/* left pixel position of a block */
  struct char_block *start_block,*end_block;

  if (line_type == BODY)
    {
      start_block = l->body;
      end_block = l->end;
    }
  else
    {
      start_block = l->margin_start;
      end_block = l->margin_end;
    }

  if (l == 0 || l->ypos == 0) abort();
  pos = buf;
  cb = start ? start : start_block;
  while (cb != start_block && cb->prev->ch == ' ')
    cb = cb->prev;
  xpos = cb->xpos;
  f = cb->face;

  BLOCK_INPUT;
  while (cb && cb != end_block)
    {
      if (cb->face == f && n < 999 && cb->char_b && !cb->blank)
	{
	  *pos++ = cb->ch;
	  /* Update attributes */
	  cb->changed = cb->new = 0;
	  n++;
	  if (cb == end) break;	  
	  cb = cb->next;
	}
      else
	{
	  /* Time to ship out a block */
	  ShipOutTextBlock(buf,n,xpos,l->ypos,l->ascent,l->descent,
			   NO_CURSOR, f, w);
	  if (cb->blank)
	    {
	      cb->changed = cb->new = 0;
	      ShipOutBlankBlock (True, cb->width, cb->xpos, l->ypos,
				 l->ascent, l->descent, NO_CURSOR,
				 cb->face, w);
	      cb = cb->next;
	    }
	  else if (!cb->char_b)
	    {
	      cb->changed = cb->new = 0;
	      ShipOutGlyphBlock(cb->glyph,cb->xpos,l->ypos,l->ascent,
				l->descent, NO_CURSOR, cb->face, w);
	      cb = cb->next;
	    }
	  if (cb)
	    {
	      f = cb->face;
	      xpos = cb->xpos;
	    }
	  n = 0;
	  pos = buf;
	}
    }
  /* Ship out dangling stuff (can only have dangling text) */
  if (n)
    ShipOutTextBlock(buf,n,xpos,l->ypos,l->ascent,l->descent,
		     NO_CURSOR, f, w);
  if (line_type == BODY && clear && (l->ascent + l->descent > 0))
    if (l->lwidth < (w->pixleft + w->pixwidth - 1))
      {
	XClearArea(x_current_display, XtWindow (s->display.x->edit_widget),
		   end_block->xpos,(l->ypos - l->ascent),
		   (w->pixleft + w->pixwidth - l->lwidth),
		   (l->ascent + l->descent),0);
      }
  if (!l->modeline
      && l->mwidth < (w->pixleft + LEFT_MARGIN (XBUFFER (w->buffer), s)))
    {
      int width = (w->pixleft + LEFT_MARGIN (XBUFFER (w->buffer), s)
		   - l->mwidth);
      
      if (width)
	XTcolor_area (s, FACE_BG_PIXEL (&SCREEN_LEFT_MARGIN_FACE(s)),
		      l->mwidth,(l->ypos - l->ascent),
		      width, (l->ascent + l->descent));
    }

  UNBLOCK_INPUT;
}

/* Returns whether it actually cleared anything.
   (It doesn't need to if the font/glyph to be drawn exactly fills the
   target area.)
 */
Bool
ShipOutBlankBlock (Bool margin, int width, int x, int y, int a, int d,
		   int cursor, struct face *face, struct window *w)
{
  Display *dpy = x_current_display;
  struct screen *s = XSCREEN(w->screen);
  Window x_win = XtWindow (s->display.x->edit_widget);
  GC gc;
  XGCValues gcv;
  unsigned long bg_mask;
  struct face *f;
  struct Lisp_Font *font;
  Bool clear_rect_p;

  memset (&gcv, ~0, sizeof (XGCValues)); /* initialize all slots to ~0 */
  gcv.graphics_exposures = False;

  f = face ? face :
    (margin ? &SCREEN_LEFT_MARGIN_FACE(s) : &SCREEN_NORMAL_FACE(s));
  font = XFONT (FACE_FONT (f));

  /*
   * There are three possible ways to blank an area.
   *  - drawing a rectangle in the background color
   *    (which we do implicitly, by using XDrawImageString instead
   *     of XDrawString)
   *  - blitting in a bitmap, whose fg and bg match that of the text
   *  - blitting in a pixmap, which carries its colors with it
   * The gcv that is used for these operations doesn't have its font
   * specified, to maximize its reusability.
   */
  
  bg_mask = GCGraphicsExposures;
  if (NILP (f->back_pixmap))
    {
      clear_rect_p = False;
    }
  else if (XPIXMAP (f->back_pixmap)->depth == 0)
    {
      if (cursor && focus_cursor_p (s))
	{
	  gcv.foreground = FACE_BG_PIXEL (f);
	  gcv.background = FACE_FG_PIXEL (f);
	}
      else
	{
	  gcv.foreground = FACE_FG_PIXEL (f);
	  gcv.background = FACE_BG_PIXEL (f);
	}
      gcv.fill_style = FillOpaqueStippled;
      gcv.stipple    = XPIXMAP (f->back_pixmap)->pixmap;
      bg_mask |= (GCForeground | GCBackground | GCStipple | GCFillStyle);
      clear_rect_p = True;
    }
  else if (!cursor || !focus_cursor_p (s))
    {
      /* If the cursor is over tiled area, don't draw the tile, because
	 then the cursor wouldn't be visible (we can't invert colors when
	 there is a tile, because it carries its colors inside it.)
	 */
      gcv.fill_style = FillTiled;
      gcv.tile = XPIXMAP (f->back_pixmap)->pixmap;
      bg_mask |= (GCTile | GCFillStyle);
      clear_rect_p = True;
    }
  else
    {
      clear_rect_p = False;
    }

  if (!clear_rect_p && (margin
			|| (cursor == THIN_CURSOR)
			|| (!cursor
			    && (font->ascent < a || font->descent < d))))
    {
    /* If the height of the selected font is less than the line being
       redisplayed, then calling XDrawImageString won't clear the area
       completely.
       */
      gcv.foreground = FACE_BG_PIXEL (f);
      gcv.fill_style = FillSolid;
      bg_mask |= (GCForeground | GCFillStyle);
      clear_rect_p = True;
    }

  if (clear_rect_p)
    {
      /* Get a GC and draw the rectangle.
       */
      gc = gc_cache_lookup (the_gc_cache, &gcv, bg_mask);
      XFillRectangle (dpy, x_win, gc, x, y - a, width, a + d);
    }

  return clear_rect_p;
}


void
ShipOutGlyphBlock (GLYPH index, int x, int y, int a, int d,
		   int cursor, struct face *face, struct window *w)
{
  struct screen *s = XSCREEN(w->screen);
  Display *dpy = x_current_display;
  Window x_win = XtWindow (s->display.x->edit_widget);
  GC gc;
  XGCValues gcv;
  unsigned long glyph_mask;
  struct face *f;
  struct Lisp_Font *font;
  Lisp_Object p = glyph_to_pixmap (index);
  int bitmap_y_offset = 0;
  int width, height;

  if (NILP (p))
    abort ();

  width = XPIXMAP (p)->width;
  height = XPIXMAP (p)->height;

  memset (&gcv, ~0, sizeof (XGCValues)); /* initialize all slots to ~0 */
  gcv.graphics_exposures = False;

  f = face ? face : &SCREEN_LEFT_MARGIN_FACE(s);
  font = XFONT (FACE_FONT (f));

  /*
   * First we need to erase the area where the glyph is going to be
   * drawn.
   */
  ShipOutBlankBlock (True, min(width, (w->pixleft + w->pixwidth - x)),
		     x, y, a, d, cursor, face, w);

  glyph_mask = GCForeground | GCBackground | GCGraphicsExposures;
  gcv.foreground = FACE_FG_PIXEL (f);
  gcv.background = FACE_BG_PIXEL (f);

  bitmap_y_offset = (height - (a+d)) / 2;
  if (height > (a+d))
    height = a+d;

  if (XPIXMAP (p)->mask)
    {
      gcv.function = GXcopy;
      gcv.clip_mask = XPIXMAP (p)->mask;
      gcv.clip_x_origin = x;
      gcv.clip_y_origin = y - a - bitmap_y_offset;
      glyph_mask |= GCFunction | GCClipMask | GCClipXOrigin | GCClipYOrigin;
    }

  gc = gc_cache_lookup (the_gc_cache, &gcv, glyph_mask);

  /* depth of 0 means it's a bitmap, not a pixmap, and we should
     use XCopyPlane (1 = current foreground color, 0 = background)
     instead of XCopyArea, which means that the bits in the pixmap
     are actual pixel values, instead of symbolic of fg/bg.
     */
  if (XPIXMAP (p)->depth > 0 /* &&     I think this is always true -- jwz
      XPIXMAP (p)->depth == s->display.x->edit_widget->core.depth */ )
    XCopyArea (dpy, XPIXMAP (p)->pixmap, x_win, gc, 0, bitmap_y_offset,
	       width, height, x, y-a);
  else
    XCopyPlane (dpy, XPIXMAP (p)->pixmap, x_win, gc, 0,
		bitmap_y_offset < 0 ? 0 : bitmap_y_offset, width, height,
		x, bitmap_y_offset < 0 ? y - bitmap_y_offset - a : y-a, 1L);
}

void
ShipOutTextBlock (unsigned char *str, int count, int x, int y, int a,
		  int d, int cursor, struct face *face, struct window *w)
{
  struct screen *s = XSCREEN(w->screen);
  Display *dpy = x_current_display;
  Window x_win = XtWindow (s->display.x->edit_widget);
  GC gc;
  XGCValues gcv;
  unsigned long text_mask;
  struct face *f;
  struct Lisp_Font *font;
  short wid;
  Bool clear_rect_p;

  memset (&gcv, ~0, sizeof (XGCValues)); /* initialize all slots to ~0 */
  gcv.graphics_exposures = False;

  if (count < 1) return;	/* allow calling with 0 counts */

  f = face ? face : &SCREEN_NORMAL_FACE(s);
  font = XFONT (FACE_FONT (f));

  wid = min (XTextWidth (font->font, (char *) str, count),
	     (w->pixleft + w->pixwidth - x));

  /*
   * First we need to erase the area where the string is going to be
   * drawn.
   */
  clear_rect_p =
    ShipOutBlankBlock (False, wid, x, y, a, d, cursor, face, w);

  text_mask = GCForeground | GCBackground | GCFont | GCGraphicsExposures;
  gcv.foreground = FACE_FG_PIXEL (f);
  gcv.background = FACE_BG_PIXEL (f);
  gcv.font = font->font->fid;

  /* The focus cursor is done by drawing the character in its background
     on top of a background of the cursor color, unless Vbar_cursor is
     non-nil.  In that case it is a line drawn immediately before the
     character in the cursor color.
   */
  if (focus_cursor_p (s) && NILP (Vbar_cursor))
    {
      EmacsScreen ew = (EmacsScreen) s->display.x->edit_widget;

      if (cursor == NORMAL_CURSOR)
	{
	  gcv.foreground = FACE_BG_PIXEL (f);
	  gcv.background = ew->emacs_screen.cursor_color;
	}
      else if (cursor == THIN_CURSOR)
	{
	  gcv.foreground = ew->emacs_screen.cursor_color;
	  gcv.background = FACE_BG_PIXEL (f);
	}
    }

  /* Now get the GC and draw the string. */
  gc = gc_cache_lookup (the_gc_cache, &gcv, text_mask);
  if (cursor != THIN_CURSOR)
    {
      if (clear_rect_p)
	XDrawString (dpy, x_win, gc, x, y, (char *) str, count);
      else
	XDrawImageString (dpy, x_win, gc, x, y, (char *) str, count);
    }
  else if (cursor == THIN_CURSOR && focus_cursor_p (s) && NILP (Vbar_cursor))
    /* A thin cursor can only occur at eol where there is no character. */
    XFillRectangle (dpy, x_win, gc, x, y-a, EOL_CURSOR_WIDTH, a+d-1);

  if (cursor && focus_cursor_p (s) && !NILP (Vbar_cursor))
    {
      EmacsScreen ew = (EmacsScreen) s->display.x->edit_widget;

      gcv.foreground = ew->emacs_screen.cursor_color;
      gc = gc_cache_lookup (the_gc_cache, &gcv, GCForeground);

      XDrawLine (dpy, x_win, gc, x, y-a, x, y+d-1);
    }

  /* Draw underlining in same colors as the text.  We can use the same GC. */
  if (f->underline)
    {
      unsigned long upos;
      unsigned long uthick;
      if (!XGetFontProperty (font->font, XA_UNDERLINE_POSITION, &upos))
	upos = 0;
      if (!XGetFontProperty (font->font, XA_UNDERLINE_THICKNESS, &uthick))
	uthick = 1;
      if (uthick <= 1)
	XDrawLine (dpy, x_win, gc, x, y + upos, x + wid, y + upos);
      else
	XFillRectangle (dpy, x_win, gc, x, y + upos, wid, uthick);
    }

  /* The non-focus cursor is done by drawing a rectangle around the character
     after it has been drawn normally.  We need a new GC for this, since the
     cursor color isn't necessarily the same as the foreground of the text.
   */
  if (cursor && !focus_cursor_p (s))
    {
      EmacsScreen ew = (EmacsScreen) s->display.x->edit_widget;

      gcv.foreground = ew->emacs_screen.cursor_color;
      gc = gc_cache_lookup (the_gc_cache, &gcv, GCForeground);
      if (NILP(Vbar_cursor))
	{
	  if (cursor == NORMAL_CURSOR)
	    XDrawRectangle (dpy, x_win, gc, x, y-a, wid-1, a+d-1);
	  else
	    XDrawRectangle (dpy, x_win, gc, x, y-a, EOL_CURSOR_WIDTH, a+d-1);
	}
    }
}


/*
 * Color an area.
 */
static void
XTcolor_area (struct screen *s, unsigned long color, int x, int y,
	      int width, int height)
{
  GC gc;
  XGCValues gcv;
  unsigned long bg_mask;

  BLOCK_INPUT;

  memset (&gcv, ~0, sizeof (XGCValues));
  gcv.graphics_exposures = False;
  gcv.foreground = FACE_BG_PIXEL (&SCREEN_LEFT_MARGIN_FACE(s));
  gcv.fill_style = FillSolid;
  bg_mask = GCGraphicsExposures | GCForeground | GCFillStyle;
  gc = gc_cache_lookup (the_gc_cache, &gcv, bg_mask);

  XFillRectangle(x_current_display, XtWindow(s->display.x->edit_widget),
		 gc,x,y,width,height);

  UNBLOCK_INPUT;
}

/*
 * Clear region from ypos1 to ypos2, for entire window width
 */
void
XTclear_window_end (struct window *w, int ypos1, int ypos2)
{
  struct screen *s = XSCREEN(w->screen);
  struct buffer *b = XBUFFER(w->buffer);
  int left_margin = LEFT_MARGIN(b,s);

  BLOCK_INPUT;

  XClearArea (x_current_display, XtWindow(s->display.x->edit_widget),
	      w->pixleft + left_margin, ypos1,
	      w->pixwidth - left_margin, (ypos2 - ypos1), 0);
  XTcolor_area (s, FACE_BG_PIXEL (&SCREEN_LEFT_MARGIN_FACE(s)),
		w->pixleft, ypos1, left_margin, ypos2 - ypos1);
  UNBLOCK_INPUT;
}



/*
 * Shift region of lines according to scrolling info
 */
void
XTshift_region (struct window *w, struct line_header *start,
		struct line_header *end)
{
  struct screen *s = XSCREEN(w->screen);
  register int old_top,new_top,length,i;
  int margin_pixwidth = LEFT_MARGIN (XBUFFER (w->buffer), s);
  int margin_pixleft = w->pixleft + margin_pixwidth;
  Window x_win = XtWindow (s->display.x->edit_widget);

  BLOCK_INPUT;
  
  old_top = start->prevy - start->ascent;
  new_top = start->ypos - start->ascent;
  length = end->ypos + end->descent - (start->ypos - start->ascent);

  if (length > 0 && old_top != new_top)
    {
      XCopyArea (x_current_display,x_win, x_win, s->display.x->normal_gc,
                 w->pixleft, old_top,
                 w->pixwidth, length,
                 w->pixleft, new_top);

      if (new_top > old_top)
	{
	  /* Shifted region down */
	  length = new_top - old_top;
	  XClearArea (x_current_display, x_win,
                      margin_pixleft, old_top,
                      w->pixwidth - margin_pixwidth, length, 0);
	  if (margin_pixwidth)
	    XTcolor_area (s, FACE_BG_PIXEL (&SCREEN_LEFT_MARGIN_FACE(s)),
			  w->pixleft, old_top,
			  margin_pixwidth, length);
	}
      else
	{
	  /* Shifted region up */
	  i = end->ypos + end->descent;
	  length = old_top - new_top;
	  XClearArea (x_current_display, x_win,
                      margin_pixleft,i,
                      w->pixwidth - margin_pixwidth, length, 0);
	  if (margin_pixwidth)
	    XTcolor_area (s, FACE_BG_PIXEL (&SCREEN_LEFT_MARGIN_FACE(s)),
			  w->pixleft,i,
			  margin_pixwidth, length);
	}
    }
  UNBLOCK_INPUT;
}



/*
 * Fast insert for char in middle of line.
 * ASSUMPTIONS:  Remaining chars on line will be blt'ed.  No region needs 
 *		 to be cleared at end of line.
 */
void
InsertChar (struct window *w, struct line_header *l, struct char_block *new,
	    struct char_block *cb, struct char_block *end, char clear)
{
  struct screen *s = XSCREEN(w->screen);
  Window x_win = XtWindow (s->display.x->edit_widget);
  unsigned char b[2];

  b[1] = 0;
  b[0] = new->ch;
  BLOCK_INPUT;
  XCopyArea(x_current_display,x_win,x_win,s->display.x->normal_gc,
	    new->xpos,			/* start x */
	    l->ypos - l->ascent, 	/* start y */
	    end->xpos - cb->xpos,	/* width */
	    l->ascent + l->descent,	/* height */
	    cb->xpos,
	    l->ypos - l->ascent);

  ShipOutTextBlock(b,1,new->xpos,l->ypos,l->ascent,l->descent,0,
		      new->face,w);
  UNBLOCK_INPUT;  
  return;
}



/*
 * Real fcn to return width of text string displayed in FONT when under X.
 */
static int
XTtext_width (Lisp_Object f, const unsigned char *s, int len)
{
  return (XTextWidth (XFONT(f)->font, (char *) s, len));
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
  s->new_cur_line = s->cur_line = XWINDOW(s->selected_window)->lines;
  s->new_cur_char = s->cur_char = XWINDOW(s->selected_window)->lines->body;
  s->phys_cursor_x = -1;
  BLOCK_INPUT;
  XClearWindow (x_current_display, XtWindow (s->display.x->edit_widget));
  CursorToggle (s);
  if (!updating_screen)
    XFlushQueue();
  UNBLOCK_INPUT;
}

/* briefly swap the foreground and background colors.
 */

extern int select ();

static void
XTflash (s)
     struct screen *s;
{
  struct face *face;
  Display *dpy;
  Window w;
  XGCValues gcv;
  GC gc;

  if (updating_screen != 0)
    return;

  face = &SCREEN_NORMAL_FACE (s);
  dpy = XtDisplay (s->display.x->widget);
  w = XtWindow (s->display.x->edit_widget);
  memset (&gcv, ~0, sizeof (XGCValues)); /* initialize all slots to ~0 */
  gcv.foreground = (FACE_FG_PIXEL (face) ^ FACE_BG_PIXEL (face));
  gcv.function = GXxor;
  gcv.graphics_exposures = False;
  gc = gc_cache_lookup (the_gc_cache, &gcv,
			(GCForeground | GCFunction | GCGraphicsExposures));
  BLOCK_INPUT;
  XFillRectangle (dpy, w, gc, 0, 0, s->display.x->widget->core.width,
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
  XFillRectangle (dpy, w, gc, 0, 0, s->display.x->widget->core.width,
		  s->display.x->widget->core.height);
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

/* Make SCREEN the current input screen.  */
void
x_new_selected_screen (screen)
     struct screen *screen;
{
  struct screen* previous_screen = selected_screen;

  if (previous_screen != screen)
    {
      selected_screen = screen;

/*
      if (previous_screen && previous_screen != XSCREEN(Vterminal_screen))
	Fastmove_cursor (previous_screen);
*/
      update_cursor (screen, 1);
    }
}


static const char *events[] =
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

const char *
x_event_name (event_type)
     int event_type;
{
  if (event_type < 0) return 0;
  if (event_type >= (sizeof (events) / sizeof (char *))) return 0;
  return events [event_type];
}

/* Handling errors.

   If an X error occurs which we are not expecting, we have no alternative
   but to print it to stderr.  It would be nice to stuff it into a pop-up
   buffer, or to print it in the minibuffer, but that's not possible, because
   one is not allowed to do any I/O on the display connection from an error
   handler. The guts of Xlib expect these functions to either return or exit.


   ####  The following is pretty dubious; I'm no longer sure it's worth the
   ####  effort, but I'm not going to delete the code just yet...

   However, there are occasions when we might expect an error to reasonably
   occur.  The interface to this is as follows:

   Before calling some X routine which may error, call
	expect_x_error (dpy);

   Just after calling the X routine, call either:

	x_error_occurred_p (dpy);

   to ask whether an error happened (and was ignored), or:

	signal_if_x_error (dpy, resumable_p);

   which will call Fsignal() with args appropriate to the X error, if there
   was one.  (Resumable_p is whether the debugger should be allowed to
   continue from the call to signal.)

   You must call one of these two routines immediately after calling the X
   routine; think of them as bookends like BLOCK_INPUT and UNBLOCK_INPUT.
 */

#ifdef DUBIOUS_X_ERROR_HANDLING
static int error_expected;
static int error_occurred;
static XErrorEvent last_error;
#endif

static int
x_error_handler (disp, event)
     Display *disp;
     XErrorEvent *event;
{
#ifdef DUBIOUS_X_ERROR_HANDLING
  if (error_expected)
    {
      error_expected = 0;
      error_occurred = 1;
      last_error = *event;
    }
  else
#endif
    {
      fprintf (stderr, "\n%s: ",
	       (STRINGP (Vinvocation_name)
		? (char *) XSTRING (Vinvocation_name)->data
		: "lemacs"));
      XmuPrintDefaultErrorMessage (disp, event, stderr);
    }
  return 0;
}


#ifdef DUBIOUS_X_ERROR_HANDLING

void
expect_x_error (Display *dpy)
{
  if (error_expected) abort ();
  XSync (dpy, 0);	/* handle pending errors before setting flag */
  error_expected = 1;
  error_occurred = 0;
}

int
x_error_occurred_p (Display *dpy)
{
  int val;
  XSync (dpy, 0);	/* handle pending errors before setting flag */
  val = error_occurred;
  error_expected = 0;
  error_occurred = 0;
  return val;
}

int
signal_if_x_error (Display *dpy, int resumable_p)
{
  char buf [1024];
  Lisp_Object data;
  if (! x_error_occurred_p (dpy))
    return 0;
  data = Qnil;
  sprintf (buf, "0x%X", (unsigned int) last_error.resourceid);
  data = Fcons (build_string (buf), data);
  {
    char num [32];
    sprintf (num, "%d", last_error.request_code);
    XGetErrorDatabaseText (last_error.display, "XRequest", num, "",
			   buf, sizeof (buf));
    if (! *buf)
      sprintf (buf, "Request-%d", last_error.request_code);
    data = Fcons (build_string (buf), data);
  }
  XGetErrorText (last_error.display, last_error.error_code, buf, sizeof (buf));
  data = Fcons (build_string (buf), data);
 again:
  Fsignal (Qx_error, data);
  if (! resumable_p) goto again;
  return 1;
}
#endif

/* This is called when the display connection becomes hosed.
   It may not return.
 */
static int
x_IO_error_handler (disp)
     Display *disp;
{
  fprintf (stderr,
	   "\n%s: Fatal I/O Error %d (%s) on display connection \"%s\"\n",
	   (STRINGP (Vinvocation_name)
	    ? (char *) XSTRING (Vinvocation_name)->data
	    : "lemacs"),
	   errno, sys_errlist [errno], DisplayString (disp));
  fprintf (stderr,
      "  after %lu requests (%lu known processed) with %d events remaining.\n",
	   NextRequest (disp) - 1, LastKnownRequestProcessed (disp),
	   QLength(disp));
  if (_Xdebug)
    abort ();
  else
    {
      fprintf (stderr, "  Autosaving and exiting...\n");
      x_current_display = 0; /* it's dead! */
      Vwindow_system = Qnil; /* let it lie! */
      Fset (Qkill_emacs_hook, Qnil); /* too dangerous */
      Fkill_emacs (make_number (70));
    }
  return 0; /* not reached; suppress warnings */
}



/* Pixmap cache */

GLYPH continuer_glyph, truncator_glyph, rarrow_glyph, lucid_glyph;

/* Hashes pixmap name strings to glyph ids */
static c_hashtable pixmap_table;
/* Indexes glyph ids to pixmap Lisp objects */
Lisp_Object *glyph_to_pixmaps_table;
/* Size and fill pointer of above */
static int glyph_to_pixmaps_size, max_pixmap_id;

void
mark_glyph_pixmaps (void (*markobj) (Lisp_Object))
{
  int i;
  for (i = 0; i < max_pixmap_id; i++)
    ((*markobj) (glyph_to_pixmaps_table [i]));
}

Lisp_Object
glyph_to_pixmap (GLYPH g)
{
  Lisp_Object p;
  if (g >= (GLYPH) max_pixmap_id)
/*    abort (); */
    return Qnil; /* #### KLUDGE */
  p = glyph_to_pixmaps_table [g];
  if (! PIXMAPP (p)) abort ();
  return p;
}

void 
init_bitmap_tables_1 ()
{
  max_pixmap_id = 0;
  glyph_to_pixmaps_size = 0;
  glyph_to_pixmaps_table = 0;
  pixmap_table = 0;
}

static void
init_bitmap_tables ()
{
  Display *dpy = x_current_display;
  Screen *screen = DefaultScreenOfDisplay (dpy);

  pixmap_table = make_strings_hashtable (20);

  glyph_to_pixmaps_size = 50;
  glyph_to_pixmaps_table = (Lisp_Object *)
    xmalloc (glyph_to_pixmaps_size * sizeof (glyph_to_pixmaps_table[0]));
  max_pixmap_id = 0;

#define MAKE_BUILTIN(NAME) 					\
  NAME##_glyph = max_pixmap_id++;				\
  glyph_to_pixmaps_table [NAME##_glyph] =			\
    make_pixmap_from_data (screen, (char *) NAME##_bits,	\
			   NAME##_width, NAME##_height);	\
  XPIXMAP (glyph_to_pixmaps_table [NAME##_glyph])->mask =	\
    XPIXMAP (glyph_to_pixmaps_table [NAME##_glyph])->pixmap

  MAKE_BUILTIN (continuer);
  MAKE_BUILTIN (truncator);
  MAKE_BUILTIN (rarrow);
  MAKE_BUILTIN (lucid);
  Vlucid_logo = glyph_to_pixmaps_table [lucid_glyph];
#undef MAKE_BUILTIN
}


extern char *strdup();

GLYPH
x_get_pixmap (Lisp_Object lisp_name, char *hash_suffix)
{
  char hash_buf [255];
  char *name = (char *) XSTRING (lisp_name)->data;
  const void *hash_value; /* can't be GLYPH (16 bits) */
  Lisp_Object pixmap;
  GLYPH new;

  strcpy (hash_buf, name);
  if (hash_suffix)
    strcat (hash_buf, hash_suffix);

  if (gethash (hash_buf, pixmap_table, &hash_value))
    /* #### What does it mean for this to be 0?  Bug? */
    if (hash_value)
      return ((GLYPH) ((int) hash_value));

  /* #### shouldn't be using selected-screen here */
  pixmap = Fmake_pixmap (lisp_name, Fselected_screen ());

  if (max_pixmap_id >= glyph_to_pixmaps_size)
    {
      glyph_to_pixmaps_size += 50;
      glyph_to_pixmaps_table = (Lisp_Object *)
	xrealloc (glyph_to_pixmaps_table,
		  glyph_to_pixmaps_size * sizeof (Lisp_Object));
    }
  new = max_pixmap_id;
  max_pixmap_id++;
  glyph_to_pixmaps_table [new] = pixmap;
  puthash (xstrdup (hash_buf),
           (void *) ((int) hash_value),
           pixmap_table);
  return new;
}


static void
make_argc_argv (argv_list, argc, argv)
     Lisp_Object argv_list;
     int* argc;
     char*** argv;
{
  Lisp_Object next;
  int n = XINT (Flength (argv_list));
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

static XrmOptionDescRec emacs_options[] = {
  {"-geometry",	".geometry", XrmoptionSepArg, NULL},
  {"-iconic",	".iconic", XrmoptionNoArg, (XtPointer) "yes"},

  {"-internal-border-width", "*EmacsScreen.internalBorderWidth",
     XrmoptionSepArg, NULL},
  {"-ib",	"*EmacsScreen.internalBorderWidth", XrmoptionSepArg, NULL},

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

#ifdef DUBIOUS_X_ERROR_HANDLING
  error_expected = 0;
  error_occurred = 0;
#endif

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
    id_name = (char *) xmalloc (string_length (XSTRING (Vinvocation_name))
                                + strlen (hostname) + 2);
    sprintf (id_name, "%s@%s", XSTRING (Vinvocation_name)->data, hostname);
  }
  

#ifdef HPUX
  {
    int owner = getpid ();
    ioctl (x_file_descriptor, SIOCSPGRP, &owner);
  }
#else /* !HPUX */
#ifdef F_SETOWN
  old_fcntl_owner = fcntl (x_file_descriptor, F_GETOWN, 0);
#ifdef F_SETOWN_SOCK_NEG
  /* stdin is a socket here */
  fcntl (x_file_descriptor, F_SETOWN, -getpid ());
#else
  fcntl (x_file_descriptor, F_SETOWN, getpid ());
#endif /* F_SETOWN_SOCK_NEG */
#endif /* F_SETOWN */
#endif /* !HPUX */

#ifdef SIGIO
  init_sigio ();
#endif

  /* Must use interrupt input because we cannot otherwise
     arrange for C-g to be noticed immediately.
     We cannot connect it to SIGINT.  */
  Fset_input_mode (Qt, Qnil, Qt);

  clear_window_end_hook = XTclear_window_end;
  shift_region_hook = XTshift_region;

  update_line_hook = PlotTextLine;
  insert_chars_hook = InsertChar;
  text_width_hook = XTtext_width;

  clear_screen_hook = XTclear_screen;
  ring_bell_hook = XTring_bell;
  beep_hook = XTsimple_beep;
  update_begin_hook = XTupdate_begin;
  update_end_hook = XTupdate_end;
  set_terminal_window_hook = XTset_terminal_window;

  /* read_socket_hook = (Lisp_Object (*)())-1; */
  cursor_to_hook = XTcursor_to;

  scroll_region_ok = 1;		/* we'll scroll partial screens */
  char_ins_del_ok = 0;		/* just as fast to write the line */
  line_ins_del_ok = 1;		/* we'll just blt 'em */
  fast_clear_end_of_line = 1;	/* X does this well */
  memory_below_screen = 0;	/* we don't remember what scrolls 
				   off the bottom */
  baud_rate = 19200;
  x_interline_space = 0;

/*  Vlast_bar_cursor = Qnil;
   staticpro (&Vlast_bar_cursor); */

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
Apparently \"%s*geometry: %s\" or \"%s*geometry: %s\" was\n\
specified in the resource database.  Specifying \"*geometry\" will make\n\
emacs (and most other X programs) malfunction in obscure ways.  You\n\
should always use \".geometry\" instead.\n\n",
	       app_name, (char *) value.addr,
	       app_class, (char *) value.addr);
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

  /* #### This is fucking insane.  This is the only way I could get the `x'
     and `y' arguments to x-create-screen to be obeyed; it seems that no
     usage of XtSetValues can cause the shell widget's idea of the USPosition
     to be updated before the shell has been mapped.  So smash it. */
  {
    if (! XtIsSubclass (widget, wmShellWidgetClass)) abort ();
    ((WMShellWidget) widget)->wm.size_hints.flags |= USPosition;
    ((WMShellWidget) widget)->wm.size_hints.x = widget->core.x;
    ((WMShellWidget) widget)->wm.size_hints.y = widget->core.y;
  }

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
  struct Lisp_Font *font = XFONT (SCREEN_DEFAULT_FONT (s));

  x = INT_BORDER(s) + x * font->width + font->width / 2;
  y = INT_BORDER(s) + y * font->height + font->height / 2;

  BLOCK_INPUT;
  XWarpPointer (x_current_display, None, XtWindow(s->display.x->edit_widget),
		0, 0, 0, 0, x, y);
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
	  ev.window = emacs_window;
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
      XSETR (WM_COMMAND_screen, Lisp_Screen, s);
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
