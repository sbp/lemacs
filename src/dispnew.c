/* Updating of data structures for redisplay.
   Copyright (C) 1992, 1993 Free Software Foundation, Inc.

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


/* #include <signal.h>  use "syssignal.h" instead -jwz */

#include "config.h"

#include "syssignal.h"

#include "intl.h"
#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>

#if 0 /* this stuff looks a lot like systty.h --jwz */

#ifdef HAVE_TERMIO
#include <termio.h>
#ifdef TCOUTQ
#undef TIOCOUTQ
#define TIOCOUTQ TCOUTQ
#endif /* TCOUTQ defined */
#else
#ifdef HAVE_TERMIOS
#include <termios.h>
#else
#ifndef VMS
#include <sys/ioctl.h>
#endif /* not VMS */
#endif /* not HAVE_TERMIOS */
#endif /* not HAVE_TERMIO */

/* Allow m- file to inhibit use of FIONREAD.  */
#ifdef BROKEN_FIONREAD
#undef FIONREAD
#endif

/* Interupt input is not used if there is no FIONREAD.  */
#ifndef FIONREAD
#undef SIGIO
#endif

#else /* !0 */
#include "systty.h"
#endif /* !0 */

#include "termchar.h"
#include "termopts.h"
#include "cm.h"
#include "lisp.h"
#include "indent.h"
#include "buffer.h"
#include "extents.h"
#include "screen.h"
#include "window.h"
#include "commands.h"

#include "dispmisc.h"
#include "dispextern.h"
#include "faces.h"
#include "disptab.h"

#include "sysdep.h"

#ifdef HAVE_X_WINDOWS
#include "xterm.h"
#include "xobjs.h"
#endif	/* HAVE_X_WINDOWS */

#define max(a,b) ((a) > (b) ? (a) : (b))
#define min(a,b) ((a) < (b) ? (a) : (b))

#ifndef PENDING_OUTPUT_COUNT
/* Get number of chars of output now in the buffer of a stdio stream.
   This ought to be built in in stdio, but it isn't.
   Some s- files override this because their stdio internals differ.  */
#define PENDING_OUTPUT_COUNT(FILE) ((FILE)->_ptr - (FILE)->_base)
#endif

/* Nonzero means do not assume anything about current
 contents of actual terminal screen */
int screen_garbaged;

/* Nonzero means last display completed.  Zero means it was preempted. */
int display_completed;

int visible_bell;	/* If true and the terminal will support it
			   then the screen will flash instead of
			   feeping when an error occurs */

int inverse_video;	/* If true and the terminal will support it
			   then we will use inverse video */

int baud_rate;		/* Terminal speed, so we can calculate
			   the number of characters required to
			   make the cursor sit still for n secs. */

Lisp_Object Vwindow_system;	/* nil or a symbol naming the window system
				   under which emacs is running
				   ('x is the only current possibility) */

/* Version number of X windows: 10, 11 or nil.  */
Lisp_Object Vwindow_system_version;

/* Vector of glyph definitions.  Indexed by glyph number,
   the contents are a string which is how to output the glyph.
   If Vglyph_table is nil, a glyph is output by using its low 8 bits
   as a character code.  */
/* Lisp_Object Vglyph_table; */

/* Display table to use for vectors that don't specify their own.  */
/* Lisp_Object Vstandard_display_table; */

/* Nonzero means reading single-character input with prompt
   so put cursor on minibuffer after the prompt.  */
int cursor_in_echo_area;
Lisp_Object Qcursor_in_echo_area;

/* The currently selected screen.
   In a single-screen version, this variable is always 0.  */
struct screen *selected_screen;

/* In a single-screen version, the information that would otherwise
   exist inside a `struct screen' lives in the following variables instead. */

#ifndef MULTI_SCREEN

/* Desired terminal cursor position (to show position of point),
   origin zero */
/* int cursX, cursY; */

/* The current (physical) screen contents */
/* struct screen_glyphs *current_glyphs; */

/* The desired (virtual) screen contents */
/* struct screen_glyphs *desired_glyphs; */

#endif /* not MULTI_SCREEN */

/* This is a vector, made larger whenever it isn't large enough,
   which is used inside `update_screen' to hold the old contents
   of the SCREEN_PHYS_LINES of the screen being updated.  */
/*struct screen_glyphs **ophys_lines;*/
/* Length of vector currently allocated.  */
/*int ophys_lines_length;*/

FILE *termscript;	/* Stdio stream being used for copy of all output.  */

struct cm Wcm;		/* Structure for info on cursor positioning */

/* extern short ospeed;*/	/* Output speed (from sg_ospeed) */

int in_display;		/* 1 if in redisplay: can't handle SIGWINCH now.  */

/* int delayed_size_change; */  /* 1 means SIGWINCH happened when not safe. */
/* int delayed_screen_height; */  /* Remembered new screen height.  */
/* int delayed_screen_width; */   /* Remembered new screen width.  */
void update_cursor (struct screen *, int);
static void change_screen_size_1 (struct screen *screen, register int newlength,
				  register int newwidth, register int pretend);
extern struct window *find_window_by_pixel_pos (unsigned int pix_x,
						unsigned int pix_y,
						Lisp_Object win);
extern void check_screen_size (struct screen *screen, int *rows, int *cols);


extern struct Root_Block *cur_root;
extern struct Root_Block *mini_root;
extern struct Root_Block *root;

extern struct redisplay_block display_list[300];
extern int list_ptr;

struct char_block old_cur_char;
struct line_header old_cur_line;


/*
 *
 */
int
scroll_screen_lines (int from, int end, int amount, struct window *w)
{
  register struct line_header *l,*l1,*l2;
  register int i;

  if (amount == 0) return 1;

  list_ptr++;

  /* Find start and end of region */
  l = w->lines;

  i = from;
  while (l && i--) l = l->next;
  l1 = l2 = l;			/* start */

  /* If we're confused, signal to do a regular layout */
  if (!l)
    {
      list_ptr--;
      return -1;
    }

  /* Find end block of region; this isn't entirely accurate, and may
   * be clipped.
   */
  while (l->next && i--) l = l->next;
  l2 = l;			/* end */

  BLOCK_TYPE(list_ptr) = BLIT;
  BLOCK_BLIT_OLD_TOP(list_ptr) = l1->ypos;
  l1->prevy = l1->ypos;
  BLOCK_BLIT_START(list_ptr) = l1;
  BLOCK_BLIT_END(list_ptr) = l2;

  if (amount > 0)
    {
      for (i = 0; i < amount; i++)
	{
	  l = get_line();

	  if (l1->prev)
	    {
	      /* Inserting line in middle of window */
	      l->prev = l1->prev;
	      l1->prev->next = l;
	    }
	  else
	    {
	      /* New first line in window */
	      w->lines = l;
	    }
	  
	  l->next = l1;
	  l1->prev = l;
	}
      /* At this point, ypos in block to be shifted is wrong, but can't
       * be corrected til these new lines are layed out.
       */
    }
  if (amount < 0)
    {
      for (i = 0; i < (-amount); i++)
	{
	  l = l1->prev;

	  l1->prev = l->prev;	  
	  if (l->prev)
	    {
	      l->prev->next = l1;
	    }
	  if (l == w->lines)	  
	    w->lines =l1;
	  free_line(l);
	}

      i = ((l1->prev)
           ? (l1->prev->ypos + l1->prev->descent)
           : w->pixtop);

      while (l1)
	{
	  l1->prevy = l1->ypos;
	  l1->ypos = i + l1->ascent;

	  i += (l1->ascent + l1->descent);

	  l1 = l1->next;
	}
    }
  return 1;
}

/*
 * Replot an Emacs window in its screen.  This will also fly for minibuffer
 * window.
 */
#ifdef DEFINE_CHANGE_FUNCTIONS
static int cur_buf_modif;
#endif

void
update_window (struct window *w)
{
  struct screen *s = XSCREEN(w->screen);
  struct line_header *l,*start,*end;
  struct char_block *cb,*endc;
  int i = 0;

  if (list_ptr > -1)
    {
      if (BLOCK_TYPE(0) != AREA)
	CXTupdate_begin(w);

      for (i = 0; i <= list_ptr; i++)
	if ((BLOCK_TYPE(i) == BODY_LINE || BLOCK_TYPE(i) == MARGIN_LINE) &&
	    BLOCK_LINE(i)->shifted && !BLOCK_LINE(i)->changed) break;
      if (i < list_ptr)
	{
	  /* Found start, now find end of region */
	  start = BLOCK_LINE(i);
	  BLOCK_DONE(i) = 1;	/* Mark line as done */
	  for (; i <= list_ptr; i++)
	    {
	      if ((BLOCK_TYPE(i) != BODY_LINE && BLOCK_TYPE(i) != MARGIN_LINE) ||
		  BLOCK_LINE(i)->changed)
		break;
	      end = BLOCK_LINE(i);
	      BLOCK_DONE(i) = 1; /* Mark line as done */
	    }
	  shift_region(w,start,end);
	}
  
      i = 0;

      while (i <= list_ptr)
	{
	  if (!BLOCK_DONE(i))
	    switch (BLOCK_TYPE(i))
	      {
	      case BODY_LINE:	/* A regular line */
		l = BLOCK_LINE(i);
		cb = BLOCK_LINE_START(i);
		endc = BLOCK_LINE_END(i);
		update_line(w,l,cb,endc,BLOCK_LINE_CLEAR(i),BODY);
		l->new = l->changed = l->shifted = 0; /* Line is up to date */
		break;
	      case MARGIN_LINE:	/* A margin line */
		l = BLOCK_LINE(i);
		cb = BLOCK_LINE_START(i);
		endc = BLOCK_LINE_END(i);
		update_line(w,l,cb,endc,BLOCK_LINE_CLEAR(i),MARGIN);
		l->new = l->changed = l->shifted = 0; /* Line is up to date */
		break;
	      case AREA:	/* Region to blank */
		clear_window_end(w,BLOCK_AREA_TOP(i),BLOCK_AREA_BOTTOM(i));
		break;
	      case BLIT:	/* Region to copy */
		shift_region(w,BLOCK_BLIT_START(i),BLOCK_BLIT_END(i));
		break;
	      default:
		break;
	      }
	  i++;
	}
      list_ptr = -1;

      if (BLOCK_TYPE(0) != AREA)
	CXTupdate_end(w);
    }

  if (s == selected_screen && w == XWINDOW(s->selected_window))
    {
#ifdef DEFINE_CHANGE_FUNCTIONS      
      if (w != XWINDOW(s->minibuffer_window) &&
	  (BUF_MODIFF(XBUFFER(w->buffer)) == cur_buf_modif) &&
	  (point != w->last_point))
	{
	  signal_after_movement((NIL(w->last_point)) ? point : w->last_point);
	}

      if (w != XWINDOW(s->minibuffer_window))
	cur_buf_modif = BUF_MODIFF(XBUFFER(w->buffer));
#endif      
      update_cursor(s,(BLOCK_TYPE(0) == BLIT));
    }

  display_completed = 1;
  memset (display_list, 0, 300*sizeof(struct redisplay_block));
}

/*
 * Clean up at end of redisplay for screen.
 */
void
update_cursor (struct screen *s, int blit)
{
  if (redisplay_lock)
    return;

  /*
   * We haven't called update_end(), so InUpdate = 1, and cursor_to
   * will simply plot cursor in new position.
   */
  if (cursor_in_echo_area < 0)
    {
      /* Put cursor in */
      s->new_cur_char = s->new_cur_line->body;
      s->cursor_x = 0;
      cursor_to(s->new_cur_line,
		s->new_cur_char,
		s->cursor_y,
		s->cursor_x,
		s->new_cur_w,s);
    }
  else if (cursor_in_echo_area)
    {
      /* Put cursor */
      s->new_cur_char = s->new_cur_line->end;
      s->cursor_x = s->new_cur_line->chars;
      cursor_to(s->new_cur_line,
		s->new_cur_char,
		s->cursor_y,
		s->cursor_x,
		s->new_cur_w,s);
    }
  else 
    {
      if (blit)
	{
	  s->cur_char = s->new_cur_char;
	  s->cur_line = s->new_cur_line;
	  s->cur_w = s->new_cur_w;
	  s->phys_cursor_y = s->cursor_y;
	  s->phys_cursor_x = s->cursor_x;

	  Fastmove_cursor(s);
	}
      else
	cursor_to(s->new_cur_line,
		  s->new_cur_char,
		  s->cursor_y,
		  s->cursor_x,
		  s->new_cur_w,s);
    }
}

/*
 * Handle forward and backward cursor movement on current line
 */
int 
direct_output_forward_char(int n)
{
  register struct window *w = XWINDOW(selected_window);
  struct screen *s = XSCREEN(w->screen);

  /* Silently fail at window edges or on horizontally scrolled windows */
  if ((n < 0 &&
       (s->phys_cursor_x == 0 || s->phys_cursor_x == s->cur_line->chars)) ||
      (n > 0 && s->phys_cursor_x >= s->cur_line->chars - 1) ||
      XFASTINT(w->hscroll))
    return 0;
  
  /*
   * Set new cursor position.  Moving cursor will set this equal to the
   * prev position.
   */
  s->cursor_x += n;
  s->new_cur_line = s->cur_line;
  do
    {
      s->new_cur_char = (n > 0) ? s->cur_char->next : s->cur_char->prev;
    }
  while (s->new_cur_char->char_b == False);

  s->new_cur_w = w;

#ifdef DEFINE_CHANGE_FUNCTIONS
  if (w != XWINDOW(s->minibuffer_window))
    {
      signal_after_movement((NIL(w->last_point)) ? PT : w->last_point);
    }
#endif  
  
  w->last_point_x = make_number (s->cursor_x);
  w->last_point = make_number (PT);

  cursor_to(s->new_cur_line,s->new_cur_char,
	    s->cursor_y,s->cursor_x,s->new_cur_w,s);
  
  return 1;
}

/*
 * Handle insertion of a character at 'cur_line,cur_char'
 * Assume that redisplay() is atomic for each screen and won't be preempted.
 * Do fast insertion of single char on current line, assuming display
 * changes will be bounded by current line.  If insertion is in middle of
 * line, utilize bit blt'ing under X/terminal insert-char operation.
 */
int
output_for_insert(int c)
{
  register struct window *w = XWINDOW(selected_window);
  struct screen *s = XSCREEN(w->screen);
  register struct line_header *l = s->cur_line;
  register struct char_block *new,*cb = s->cur_char;
  register struct buffer *b = current_buffer;
#ifdef I18N4
  wchar_t a[2];
#else
  unsigned char a[2];			/* CRUDE */
#endif
  int pixright;
  struct Lisp_Font *font;

  if (
      /* Selected window is minibuffer & msg is displayed */
      (EQ(selected_window,minibuf_window) && echo_area_glyphs) ||
      /* Window is horizontally scrolled */
      (XINT(w->hscroll) > 0) ||
      /* Cursor on first character on line */
      (cb == l->body) ||
      /* Invalid cursor position info */
      (s->cursor_y < 0) ||
      (s->cursor_y > w->used_height) ||
      !display_completed ||
      /* Buffer is shared */
      buffer_shared > 1 ||
      /* Line contains tabs */
      l->tabs ||
      /* Overlay arrow string is in this buffer */
      (STRINGP(Voverlay_arrow_string) &&
       MARKERP(Voverlay_arrow_position) &&
       XMARKER(Voverlay_arrow_position)->buffer == b)
      )
    return 0;			/* Silently fail in these cases */

  a[1]=0;
  new = get_char_block ();
  new->ch = a[0] = c;
  new->char_b = 1;
  if (cb->face == cb->prev->face)
    new->face = cb->face;
  else
    {
      struct face *e_face;
      e_face = get_face_at_bufpos (s, b, PT, 0, 0);
      if (e_face != new->face)
	new->face = e_face;
    }

  font = XFONT (FACE_FONT (new->face));

  new->new = 1;			/* Flag -> this is new */
  new->width = text_width (FACE_FONT (new->face), a, 1);
  new->xpos = cb->xpos;
  
  pixright = (w->pixleft + w->pixwidth -
              XPIXMAP (glyph_to_pixmap (GLYPH_FROM_CHAR ('\\')))->width);
    
  if ((l->lwidth + new->width) >= pixright 
      || font->ascent > l->ascent 
      || font->descent > l->descent)
    {
      /* Fail here if we are going to go over the edge of this window, or
       * the new character's font is a different height than the line's
       * ascent or descent.
       */
      free_char_blocks (new,new);
      return 0;
    }
  
  /* Link it into the line */
  new->prev = cb->prev;
  new->prev->next = new;

  new->next = cb;
  cb->prev = new;

  /* Adjust attributes for rest of line */
  for ( ; cb; cb->xpos += new->width, cb = cb->next);

  /* Update the display */
  if (new->next == l->end)
    update_line(w,l,new,0,0,BODY);	/* Don't clear end of line */	
  else
    insert_chars(w,l,new,new->next,l->end,0); /* Faster */      

  l->lwidth += new->width;
  l->chars++;

  /* Update the cursor */
  s->cursor_x += 1;
  w->last_point_x = make_number (s->cursor_x);
  Fastmove_cursor(s);

  return 1;
}


/*
 * For pixel based fill mode, return info about whether inserting the next
 * character will push end of current line past fill-pixel value.
 * Return 1 if insertion will succeed, 0 if other action should take place
 * first
 */
int
pixel_insert_ok (int c, int fill)
{
  struct line_header *l = selected_screen->cur_line;
  struct char_block *cb = selected_screen->cur_char;
  struct buffer *b = current_buffer;
  struct face *face;
  struct Lisp_Font *font;

#ifdef I18N4
  wchar_t a[2];
#else
  unsigned char a[2];
#endif

  if (l->lwidth == 0) return 1;	/* Empty line */

  if (cb && (!cb->prev || cb->prev->face == cb->face))
    face = cb->face;
  else
    {
      face = get_face_at_bufpos (selected_screen, b, PT, 0, 0);
    }
  font = XFONT (FACE_FONT (face));
  
  a[1] = c; 
  a[0] = 0;

  return (((l->lwidth + text_width (FACE_FONT (face), a, 1)) <= fill) 
          ? 1 : 0);
}


/* Change the screen height and/or width.  Values may be given as zero to
   indicate no change is to take place. */

void
change_screen_size (screen, newheight, newwidth, pretend)
     register struct screen *screen;
     int newheight, newwidth, pretend;
{
  /* sometimes we get passed a size that's too small (esp. when a client widget gets
     resized, since we have no control over this).  So deal. */
  check_screen_size (screen, &newheight, &newwidth);
  if (in_display || gc_in_progress)
    {
      screen->size_change_pending = 1;
      screen->new_width = newwidth;
      screen->new_height = newheight;
      return;
    }

  screen->size_change_pending = 0;
  change_screen_size_1 (screen, newheight, newwidth, pretend);
}


static void
change_screen_size_1 (screen, newheight, newwidth, pretend)
     register struct screen *screen;
     register int newheight, newwidth, pretend;
{
  int font_height, font_width;
  int new_pixheight, new_pixwidth;
  struct Lisp_Font *font;

  if (in_display)
    abort ();

  if (NILP (SCREEN_DEFAULT_FONT (screen)))
    {
      SCREEN_HEIGHT (screen) = newheight;
      SCREEN_WIDTH (screen) = newwidth;
      return;
    }

  font = XFONT (SCREEN_DEFAULT_FONT (screen));

  /* This size-change overrides any pending one for this screen.  */
  SCREEN_NEW_HEIGHT (screen) = 0;
  SCREEN_NEW_WIDTH (screen) = 0;

  if (font == 0)
    {
      font_height = 13;
      font_width = 6;
    }
  else
    {
      font_height = font->height;
      font_width = font->width;
    }

  new_pixheight = newheight * font_height;
  new_pixwidth = (newwidth - 1) * font_width +
    max (font_width,
	 max (XPIXMAP (glyph_to_pixmap (continuer_glyph))->width,
	      XPIXMAP (glyph_to_pixmap (truncator_glyph))->width));

  if (!new_pixheight && !new_pixwidth)
    return;

  if (new_pixheight)
    {
      if (new_pixheight > MScreenLength) new_pixheight = MScreenLength;
      if (XSCREEN (WINDOW_SCREEN (XWINDOW (SCREEN_MINIBUF_WINDOW (screen))))
	  == screen
	  && ! EQ (SCREEN_MINIBUF_WINDOW (screen),
		   SCREEN_ROOT_WINDOW (screen)))
	{
	  /* Screen has both root and minibuffer.  */
	  set_window_height (SCREEN_ROOT_WINDOW (screen),
			     new_pixheight - font_height, 0);
	  XWINDOW (SCREEN_MINIBUF_WINDOW (screen))->pixtop
	    = new_pixheight - font_height + INT_BORDER (screen);
	  set_window_height (SCREEN_MINIBUF_WINDOW (screen), font_height, 0);
	}
      else
	/* Screen has just one top-level window.  */
	set_window_height (SCREEN_ROOT_WINDOW (screen), new_pixheight, 0);

      SCREEN_HEIGHT (screen) = newheight;
	
      if (SCREEN_IS_TERMCAP (screen) == output_termcap && !pretend)
	ScreenRows = newheight;
    }

  if (new_pixwidth)
    {
      if (new_pixwidth > MScreenWidth) new_pixwidth = MScreenWidth;
      set_window_width (SCREEN_ROOT_WINDOW (screen), new_pixwidth, 0);
      if (XSCREEN (WINDOW_SCREEN (XWINDOW (SCREEN_MINIBUF_WINDOW (screen))))
	  == screen)
	set_window_width (SCREEN_MINIBUF_WINDOW (screen), new_pixwidth, 0);

      SCREEN_WIDTH (screen) = newwidth;

      if (SCREEN_IS_TERMCAP (screen) && !pretend)
	ScreenCols = newwidth;
    }

  /* The message buffer may grow if the screen size grows, but it will
   * never shrink.
   */
  if (newwidth)
    if (SCREEN_MESSAGE_BUF (screen))
      {
	if (SCREEN_MESSAGE_BUF_SIZE (screen) < (newwidth + 1)
	    || !echo_area_glyphs)
	  {
	    char *new_message_buf
	      = (char *) xrealloc (SCREEN_MESSAGE_BUF (screen),
				   newwidth + 1);
	    if (echo_area_glyphs == SCREEN_MESSAGE_BUF (screen))
	      echo_area_glyphs = new_message_buf;
	    SCREEN_MESSAGE_BUF (screen) = new_message_buf;
	    SCREEN_MESSAGE_BUF_SIZE (screen) = newwidth;
	  }
      }
    else
      {
	SCREEN_MESSAGE_BUF (screen) = (char *) xmalloc (newwidth + 1);
	SCREEN_MESSAGE_BUF_SIZE (screen) = newwidth;
      }

  SET_SCREEN_GARBAGED (screen);
}

#ifdef SIGWINCH

void
window_change_signal ()
{
  int width, height;
  int old_errno = errno;
  Lisp_Object tail;

  if (interrupt_input)
    unrequest_sigio ();

  get_screen_size (&width, &height);

  /* The screen size change obviously applies to a termcap-controlled
     screen.  Find such a screen in the list, and assume it's the only
     one (since the redisplay code always writes to stdout, not a
     FILE * specified in the screen structure).  Record the new size,
     but don't reallocate the data structures now.  Let that be done
     later outside of the signal handler.  */

  for (tail = Vscreen_list; CONSP (tail); tail = XCONS (tail)->cdr)
    {
      struct screen *s = XSCREEN (XCONS (tail)->car);
	
      if (SCREEN_IS_TERMCAP (s))
	{
	  ++in_display;
	  change_screen_size (s, height, width, 0);
	  --in_display;
	  break;
	}
    }

  signal (SIGWINCH, (void (*)(int))window_change_signal);
  errno = old_errno;

  if (interrupt_input)
    request_sigio ();
}
#endif /* SIGWINCH */


char *terminal_type;

/* Initialization done when Emacs fork is started, before doing stty.
   Determine terminal type and set terminal_driver.
   Then invoke its decoding routine to set up variables
   in the terminal package */

void
init_display ()
{
  meta_key = 0;
  inverse_video = 0;
  cursor_in_echo_area = 0;
  terminal_type = (char *) 0;

  /* Look at the TERM variable */
  terminal_type = (char *) getenv ("TERM");
  if (!terminal_type && initial_screen_is_tty())
    {
#ifdef VMS
      fprintf (stderr, GETTEXT ("Please specify your terminal type.\n\
For types defined in VMS, use  set term /device=TYPE.\n\
For types not defined in VMS, use  define emacs_term \"TYPE\".\n\
\(The quotation marks are necessary since terminal types are lower case.)\n"));
#else
      fprintf (stderr, GETTEXT ("Please set the environment variable TERM; see tset(1).\n"));
#endif
      exit (1);
    }

#ifdef VMS
  init_vms_input ();            /* get_screen_size needs this */
  /* VMS DCL tends to upcase things, so downcase ter type.
     Hardly any uppercade letters in terminal types; should be none.  */
  {
    char *new = (char *) xmalloc (strlen (terminal_type) + 1);
    char *p;

    strcpy (new, terminal_type);

    for (p = new; *p; p++)
      if (isupper (*p))
	*p = tolower (*p);

    terminal_type = new;
  }	
#endif

  term_init (terminal_type);

#ifndef MULTI_SCREEN
  highlight_face.hilited = 1;
  if (term_does_underline)
    selection_face.underline = 1;
  else
    selection_face.hilited = 1;
#endif

#if 0
  SCREEN_WIDTH (selected_screen) = screen_width;
  SCREEN_HEIGHT (selected_screen) = screen_height;
#endif

/*
#ifdef HAVE_X_WINDOWS
  if (initial_screen_is_tty ())
#endif
    remake_screen_glyphs (selected_screen);
*/

  /* X and Y coordiates of the cursor between updates. */
  SCREEN_CURSOR_X (selected_screen) = 0;
  SCREEN_CURSOR_Y (selected_screen) = 0;

#ifdef HAVE_X_WINDOWS
  if (!initial_screen_is_tty ())
    {
      Vwindow_system = Qx;
      Vwindow_system_version = make_number (11);

      /* Don't handle SIGWINCH if using X.  */
      return;
    }
#endif /* HAVE_X_WINDOWS */

#ifdef SIGWINCH
#ifndef CANNOT_DUMP
  if (initialized)
#endif /* CANNOT_DUMP */
    signal (SIGWINCH, (void (*)(int))window_change_signal);
#endif /* SIGWINCH */
}

extern int minibuf_prompt_width;

int
pixel_to_glyph_translation (struct screen *s, register unsigned int pix_x,
			    register unsigned int pix_y, register int *x,
			    register int *y, struct window **w, int *bufp,
			    Lisp_Object *class)
{
  Lisp_Object win, buffer;
  struct buffer *b;
  register struct line_header *l = 0;
  register struct char_block *cb;
  struct position *pos;
  int startp, hscroll;
  int ypos;
  unsigned int win_x;

  /* It looks to me like the result value of pixel_to_glyph_translation() is
     0:  modeline
     1:  over text, or over a glyph in the lineinfo column;
     2:  not over text, not in a window, or over an inactive minibuffer.
   */

  *x = 0;
  *y = 0;
  *w = 0;
  *bufp = 0;
  *class = Qnil;

  if ((pix_y < INT_BORDER(s)) || (pix_y > INT_BORDER(s) + PIXH(s)))
    return 2;

  /*
   * Set the line assuming all lines use the default font.  We'll
   * change this later if we need to.
   */
  *y = (pix_y * s->height) / SCREEN_PIXHEIGHT(s);

  win = s->root_window;
  win_x = pix_x;
  if (win_x < INT_BORDER(s)) win_x = INT_BORDER(s);
  if (win_x > INT_BORDER(s) + PIXW(s)) win_x = INT_BORDER(s) + PIXW(s);
  *w = find_window_by_pixel_pos (win_x, pix_y, win);

  if (!*w)
    return 2;

  if (*w && MINI_WINDOW_P(*w) && !minibuf_level)
    {
      *w = 0;
      return 2;
    }

  if (pix_x < INT_BORDER(s))
    return 2;

  buffer = (*w)->buffer;
  b = XBUFFER(buffer);

  if ((*w)->modeline)
    l = (*w)->modeline;
  if (l)
    ypos = (*w)->pixtop + (*w)->pixheight - l->descent;

  if (!EQ(win,s->minibuffer_window) &&
      l && ypos - l->ascent <= pix_y && pix_y <= ypos + l->descent)
    {
      /* In mode line. */
      *w = 0;
      *x = ((pix_x * PIXW(s)) /
	    (XFONT (SCREEN_DEFAULT_FONT (s))->width * SCREEN_PIXWIDTH(s)));
      return 2;
    }
  else
    {
      Lisp_Object old_inhibit_quit = Vinhibit_quit;
      struct buffer *old_current_buffer = current_buffer;
      int cy;

      Vinhibit_quit = Qt;		/* don't allow quits out of here */
      current_buffer = b;
      *y = 0;
/*      *y = (XINT((*w)->pixtop) - INT_BORDER(s)) /
	FONT_HEIGHT(SCREEN_NORMAL_FACE(s).font);*/
      l = (*w)->lines;

      while (l)
	{
	  if (l->ypos - l->ascent <= pix_y && pix_y < l->ypos + l->descent)
	    break;
	  l = l->next; (*y)++;
	}
      /* Check for any glyphs at the pointer position. */
      if (l)
	{
	  cb = l->margin_start;
	  while (cb != l->margin_end)
	    {
	      if (cb->xpos <= pix_x && pix_x < cb->xpos + cb->width)
		break;
	      cb = cb->next;
	    }
	  if (cb != l->margin_end)
	    if (cb->e)
	      XSETEXTENT (*class, cb->e);
	}

      /* Determine the character position of the pointer. */
      if (l)
	{
	  if (pix_x < ((*w)->pixleft + LEFT_MARGIN (b, s, *w)))
	    {
	      *x = 0;
	    }
	  else
	    {
	      cb = l->body;
	      while (cb != l->end)
		{
		  if (cb->xpos <= pix_x && pix_x  < cb->xpos + cb->width)
		    break;
		  if (cb->char_b) (*x)++;
		  cb = cb->next;
		}
	      if (cb->e)
		XSETEXTENT (*class, cb->e);
	      while (!cb->char_b && cb != l->end)
		cb = cb->next;
	      if (cb == l->end ||
		  (cb == l->body && cb->ch == '|'
		   && cb->face == &SCREEN_MODELINE_FACE(s)))
		{
		  *y += (((*w)->pixtop - INT_BORDER (s)) /
			 XFONT (SCREEN_DEFAULT_FONT (s))->height);

                  current_buffer = old_current_buffer;
                  Vinhibit_quit = old_inhibit_quit;

		  if (cb != l->end)
		    {
		      *w = 0;
		      return 2;
		    }
		  else if (!NILP (*class))
		    return 1;
		  else
		    return 2;
		}
	      if ((*w)->pixleft != (INT_BORDER (s)))
		(*x)--;
	    }
	}

      startp = marker_position ((*w)->start);
      hscroll = XINT ((*w)->hscroll);
      /* New interface */
      cy = *y;
      pos = compute_motion (*w,startp,0,
			    (( *w == XWINDOW(s->minibuffer_window) &&
			      startp == 1)
			     ? minibuf_prompt_width : 0)
			    +
			    (hscroll ? 1 - hscroll : 0),
			    ZV,*y,*x,hscroll,0);
	
      current_buffer = old_current_buffer;
      Vinhibit_quit = old_inhibit_quit;
	
      *y = cy + (((*w)->pixtop - INT_BORDER(s)) /
		 XFONT (SCREEN_DEFAULT_FONT (s))->height);
      /*
       *could have gone 1 line to far, if the correct line wasn't long enough
       */
      if (pos->vpos == pix_y+1) pos->bufpos -= 1; /* backup over newline */
    }

  if (pos->bufpos > BUF_Z (XBUFFER((*w)->buffer)))
    {
      *y = (pix_y * s->height) / SCREEN_PIXHEIGHT(s);
      if (!NILP(*class))
	return 1;
      else
	return 2;
    }
  else
    {
      *bufp = pos->bufpos;

      if (NILP(*class) && pix_x < ((*w)->pixleft + LEFT_MARGIN (b, s, *w)))
	return 2;
      else
	return 1;
    }
}


DEFUN ("open-termscript", Fopen_termscript, Sopen_termscript,
  1, 1, "FOpen termscript file: ",
  "Start writing all terminal output to FILE as well as the terminal.\n\
FILE = nil means just close any termscript file currently open.")
  (file)
     Lisp_Object file;
{
  if (termscript != 0) fclose (termscript);
  termscript = 0;

  if (! NILP (file))
    {
      file = Fexpand_file_name (file, Qnil);
      termscript = fopen ((char *)XSTRING (file)->data, "w");
      if (termscript == 0)
	report_file_error (GETTEXT ("Opening termscript"), Fcons (file, Qnil));
    }
  return Qnil;
}

DEFUN ("send-string-to-terminal", Fsend_string_to_terminal,
  Ssend_string_to_terminal, 1, 1, 0,
  "Send STRING to the terminal without alteration.\n\
Control characters in STRING will have terminal-dependent effects.")
  (str)
     Lisp_Object str;
{
  CHECK_STRING (str, 0);
  fwrite (XSTRING (str)->data, 1, XSTRING (str)->size, stdout);
  fflush (stdout);
  if (termscript)
    {
      fwrite (XSTRING (str)->data, 1, XSTRING (str)->size, termscript);
      fflush (termscript);
    }
  return Qnil;
}

DEFUN ("ding", Fding, Sding, 0, 2, 0,
  "Beep, or flash the screen.\n\
Also, unless an argument is given,\n\
terminate any keyboard macro currently executing.\n\
When called from lisp, the second argument is what sound to make.")
  (arg, sound)
  Lisp_Object arg, sound;
{
  if (!NILP (arg))
    {
      ring_bell (sound);
      fflush (stdout);
    }
  else
    bitch_at_user (sound);

  return Qnil;
}

/* Lucid sound change */
void
bitch_at_user (sound)
     Lisp_Object sound;
{
  if (noninteractive)
    putchar (07);
  else if (!INTERACTIVE)  /* Stop executing a keyboard macro. */
    error (GETTEXT("Keyboard macro terminated by a command ringing the bell"));
  else
    ring_bell (sound);
  fflush (stdout);
}


/* so that .emacs can be loaded after screen is created */
DEFUN ("initialize-first-screen", Finitialize_first_screen,
       Sinitialize_first_screen,
  0, 0, 0, "Make redisplay work on the first screen (do this early.)")
  ()
{
  /* defined in xdisp.c because it needs to use "static void" functions */
  initialize_first_screen ();
  return Qnil;
}

void
syms_of_display ()
{
  defsubr (&Sopen_termscript);
  defsubr (&Sding);
  defsubr (&Ssend_string_to_terminal);
  defsubr (&Sinitialize_first_screen);

  defsymbol (&Qcursor_in_echo_area, "cursor-in-echo-area");

  DEFVAR_INT ("baud-rate", &baud_rate,
    "The output baud rate of the terminal.\n\
On most systems, changing this value will affect the amount of padding\n\
and the other strategic decisions made during redisplay.");
  DEFVAR_BOOL ("inverse-video", &inverse_video,
    "*Non-nil means invert the entire screen display.\n\
This means everything is in inverse video which otherwise would not be.");
  DEFVAR_BOOL ("visible-bell", &visible_bell,
    "*Non-nil means try to flash the screen to represent a bell.");
  DEFVAR_BOOL ("no-redraw-on-reenter", &no_redraw_on_reenter,
    "*Non-nil means no need to redraw entire screen after suspending.\n\
A non-nil value is useful if the terminal can automatically preserve\n\
Emacs's screen display when you reenter Emacs.\n\
It is up to you to set this variable if your terminal can do that.");
  DEFVAR_LISP ("window-system", &Vwindow_system,
    "A symbol naming the window-system under which Emacs is running,\n\
such as `x', or nil if emacs is running on an ordinary terminal.");
  DEFVAR_LISP ("window-system-version", &Vwindow_system_version,
    "The version number of the window system in use.\n\
For X windows, this is 10 or 11.");
  Vwindow_system_version = Qnil;
  DEFVAR_BOOL ("cursor-in-echo-area", &cursor_in_echo_area,
    "Non-nil means put cursor in minibuffer, at end of any message there.");

#if 0 /* this isn't implemented yet, so there's no point in telling the user */
  DEFVAR_LISP ("glyph-table", &Vglyph_table,
    "Table defining how to output a glyph code to the screen.\n\
If not nil, this is a vector indexed by glyph code to define the glyph.\n\
Each element can be:\n\
 integer: a glyph code which this glyph is an alias for.\n\
 string: output this glyph using that string (not impl. in X windows).\n\
 nil: this glyph mod 256 is char code to output,\n\
    and this glyph / 256 is face code for X windows (see `x-set-face').");
  Vglyph_table = Qnil;
#endif

#if 0
  DEFVAR_LISP ("standard-display-table", &Vstandard_display_table,
    "Display table to use for buffers that specify none.\n\
See `buffer-display-table' for more information.");
  Vstandard_display_table = Qnil;
#endif

  /* Initialize `window-system', unless init_display already decided it.  */
#ifdef CANNOT_DUMP
  if (noninteractive)
#endif
    Vwindow_system = Qnil;
}
