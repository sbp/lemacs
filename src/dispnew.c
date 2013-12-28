/* Updating of data structures for redisplay.
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


#include <signal.h>

#include "config.h"
#include <stdio.h>
#include <ctype.h>
#include "indent.h"

#ifdef NEED_TIME_H
#include <time.h>
#else /* not NEED_TIME_H */
#ifdef HAVE_TIMEVAL
#include <sys/time.h>
#endif /* HAVE_TIMEVAL */
#endif /* not NEED_TIME_H */

#ifdef HAVE_TERMIO
#include <termio.h>
#ifdef TCOUTQ
#undef TIOCOUTQ
#define TIOCOUTQ TCOUTQ
#endif /* TCOUTQ defined */
#else
#ifndef VMS
#include <sys/ioctl.h>
#endif /* not VMS */
#endif /* not HAVE_TERMIO */

/* Allow m- file to inhibit use of FIONREAD.  */
#ifdef BROKEN_FIONREAD
#undef FIONREAD
#endif

/* Interupt input is not used if there is no FIONREAD.  */
#ifndef FIONREAD
#undef SIGIO
#endif


void bzero ();
void bcopy ();

#include "termchar.h"
#include "termopts.h"
#include "cm.h"
#include "lisp.h"
#include "buffer.h"
#include "screen.h"
#include "window.h"
#include "commands.h"
#include "disptab.h"

#ifdef HAVE_X_WINDOWS
#include "xterm.h"
#endif	/* HAVE_X_WINDOWS */

#define max(a, b) ((a) > (b) ? (a) : (b))
#define min(a, b) ((a) < (b) ? (a) : (b))

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
Lisp_Object Vglyph_table;

/* Display table to use for vectors that don't specify their own.  */
Lisp_Object Vstandard_display_table;

/* Nonzero means reading single-character input with prompt
   so put cursor on minibuffer after the prompt.  */
int cursor_in_echo_area;
Lisp_Object Qcursor_in_echo_area;

/* The currently selected screen.
   In a single-screen version, this variable is always 0.  */
SCREEN_PTR selected_screen;

/* In a single-screen version, the information that would otherwise
   exist inside a `struct screen' lives in the following variables instead.  */

#ifndef MULTI_SCREEN

/* Desired terminal cursor position (to show position of point),
   origin zero */
int cursX, cursY;

/* The current (physical) screen contents */
struct screen_glyphs *current_glyphs;

/* The desired (virtual) screen contents */
struct screen_glyphs *desired_glyphs;

#endif /* not MULTI_SCREEN */

/* This is a vector, made larger whenever it isn't large enough,
   which is used inside `update_screen' to hold the old contents
   of the SCREEN_PHYS_LINES of the screen being updated.  */
struct screen_glyphs **ophys_lines;
/* Length of vector currently allocated.  */
int ophys_lines_length;

FILE *termscript;	/* Stdio stream being used for copy of all output.  */

struct cm Wcm;		/* Structure for info on cursor positioning */

extern short ospeed;	/* Output speed (from sg_ospeed) */

int in_display;		/* 1 if in redisplay: can't handle SIGWINCH now.  */

int delayed_size_change;  /* 1 means SIGWINCH happened when not safe.  */
int delayed_screen_height;  /* Remembered new screen height.  */
int delayed_screen_width;   /* Remembered new screen width.  */
void change_screen_size ();

static struct screen_glyphs *
make_screen_glyphs (screen, empty)
     register SCREEN_PTR screen;
     int empty;
{
  register int i;
  register width = SCREEN_WIDTH (screen);
  register height = SCREEN_HEIGHT (screen);
  register struct screen_glyphs *new =
    (struct screen_glyphs *) xmalloc (sizeof (struct screen_glyphs));

  SET_GLYPHS_SCREEN (new, screen);
  new->height = height;
  new->width = width;
  new->used = (int *) xmalloc (height * sizeof (int));
  new->glyphs = (GLYPH **) xmalloc (height * sizeof (GLYPH *));
#if 0
  new->highlight = (char *) xmalloc (height * sizeof (char));
#endif
  new->enable = (char *) xmalloc (height * sizeof (char));
  bzero (new->enable, height * sizeof (char));
  new->bufp = (int *) xmalloc (height * sizeof (int));
  bzero (new->bufp, height * sizeof (int));
  new->nruns = (int *) xmalloc (height * sizeof (int));
  bzero(new->nruns, height * sizeof (int));
  new->face_list =
    (struct run **) xmalloc (height * sizeof (struct run *));

#ifdef HAVE_X_WINDOWS
  if (SCREEN_IS_X (screen))
    {
      new->top_left_x = (short *) xmalloc (height * sizeof (short));
      new->top_left_y = (short *) xmalloc (height * sizeof (short));
      new->pix_width = (short *) xmalloc (height * sizeof (short));
      new->pix_height = (short *) xmalloc (height * sizeof (short));
      new->max_ascent = (short *) xmalloc (height * sizeof (short));
    }
#endif

  if (empty)
    {
      /* Make the buffer used by decode_mode_spec.  This buffer is also
	 used as temporary storage when updating the screen.  See scroll.c. */
      unsigned int total_glyphs = (width + 2) * sizeof (GLYPH);

      new->total_contents = (GLYPH *) xmalloc (total_glyphs);
      bzero (new->total_contents, total_glyphs);

      new->faces = 0;
    }
  else
    {
      unsigned int padded_width = width + 10;
      unsigned int total_glyphs = height * padded_width;

      new->total_contents = (GLYPH *) xmalloc (total_glyphs * sizeof (GLYPH));
      bzero (new->total_contents, total_glyphs * sizeof (GLYPH));
      for (i = 0; i < height; i++)
	new->glyphs[i] = new->total_contents + i * padded_width;

      new->faces = 
        (struct run *) xmalloc (total_glyphs * sizeof (struct run));

      bzero (new->faces, total_glyphs * sizeof (struct run));
      for (i = 0; i < height; i++)
	new->face_list[i] = new->faces + i * padded_width;
    }

  return new;
}

static void
free_screen_glyphs (screen, glyphs)
     SCREEN_PTR screen;
     struct screen_glyphs *glyphs;
{
  if (glyphs->total_contents)
    free (glyphs->total_contents);

  free (glyphs->used);
  free (glyphs->glyphs);
#if 0
  free (glyphs->highlight);
#endif
  free (glyphs->enable);
  free (glyphs->bufp);
  free (glyphs->nruns);
  free (glyphs->face_list);

  if (glyphs->faces)
    free ((char *)glyphs->faces);

#ifdef HAVE_X_WINDOWS
  if (SCREEN_IS_X (screen))
    {
      free (glyphs->top_left_x);
      free (glyphs->top_left_y);
      free (glyphs->pix_width);
      free (glyphs->pix_height);
      free (glyphs->max_ascent);
    }
#endif

  free (glyphs);
}

static void
remake_screen_glyphs (screen)
     SCREEN_PTR screen;
{
  if (SCREEN_CURRENT_GLYPHS (screen))
    free_screen_glyphs (screen, SCREEN_CURRENT_GLYPHS (screen));
  if (SCREEN_DESIRED_GLYPHS (screen))
    free_screen_glyphs (screen, SCREEN_DESIRED_GLYPHS (screen));
  if (SCREEN_TEMP_GLYPHS (screen))
    free_screen_glyphs (screen, SCREEN_TEMP_GLYPHS (screen));

  if (SCREEN_MESSAGE_BUF (screen))
    {
      char *new_message_buf
	= (char *) xrealloc (SCREEN_MESSAGE_BUF (screen),
			     SCREEN_WIDTH (screen) + 1);
      if (echo_area_glyphs == SCREEN_MESSAGE_BUF (screen))
	echo_area_glyphs = new_message_buf;
      SCREEN_MESSAGE_BUF (screen) = new_message_buf;
    }
  else
    SCREEN_MESSAGE_BUF (screen)
      = (char *) xmalloc (SCREEN_WIDTH (screen) + 1);

  SCREEN_CURRENT_GLYPHS (screen) = make_screen_glyphs (screen, 0);
  SCREEN_DESIRED_GLYPHS (screen) = make_screen_glyphs (screen, 0);
  SCREEN_TEMP_GLYPHS (screen) = make_screen_glyphs (screen, 1);
  SET_SCREEN_GARBAGED (screen);
}

/* Return the hash code of display_line p.  */

static int
line_hash_code (p, vpos)
     register struct screen_glyphs *p;
     int vpos;
{
  register GLYPH *body;
  register int h = 0;

  if (!p->enable[vpos])
    return 0;

  if (p->nruns[vpos] > 1 || p->face_list[vpos][0].faceptr->hilited)
    return -1;

  body = p->glyphs[vpos];

  if (must_write_spaces)
    while (1)
      {
	GLYPH g = *body++;

	if (g == 0)
	  break;
	h = (((h << 4) + (h >> 24)) & 0x0fffffff) + g - SPACEGLYPH;
      }
  else
    while (1)
      {
	GLYPH g = *body++;

	if (g == 0)
	  break;
	h = (((h << 4) + (h >> 24)) & 0x0fffffff) + g;
      }

  if (h)
    return h;
  return 1;
}

/* Return the cost to draw line p.  Cost is measured in characters,
   with white space counting as one character (unless the terminal
   requires those to be explicitly output). */

static unsigned int
line_draw_cost (screen, vpos)
     struct screen_glyphs *screen;
     int vpos;
{
  register GLYPH *beg = screen->glyphs[vpos];
  register GLYPH *end = screen->glyphs[vpos] + screen->used[vpos];
  register int i;
  register int tlen = GLYPH_TABLE_LENGTH;
  register Lisp_Object *tbase = GLYPH_TABLE_BASE;

  /* Ignore trailing and leading spaces if we can.  */
  if (!must_write_spaces)
    {
      while ((end != beg) && (*end == SPACEGLYPH))
	--end;
      if (end == beg)
	return (0); /* All blank line. */

      while (*beg == SPACEGLYPH)
	++beg;
    }

  /* If we don't have a glyph-table, each glyph is one character,
     so return the number of glyphs.  */
  if (tbase == 0)
    return end - beg;

  /* Otherwise, scan the glyphs and accumulate their total size in I.  */
  i = 0;
  while ((beg <= end) && *beg)
    {
      register GLYPH g = *beg++;

      if (GLYPH_SIMPLE_P (tbase, tlen, (int)g))
	i += 1;
      else
	i += GLYPH_LENGTH (tbase, g);
    }
  return i;
}

/* The functions on this page are the interface from xdisp.c to redisplay.
   They take cursor position arguments in origin 0.

   The only other interface into redisplay is through setting
   SCREEN_CURSOR_X (screen) and SCREEN_CURSOR_Y (screen)
   and SET_SCREEN_GARBAGED (screen). */

/* cancel_line eliminates any request to display a line at position `vpos' */

void
cancel_line (vpos, screen)
     int vpos;
     register SCREEN_PTR screen;
{
  SCREEN_DESIRED_GLYPHS (screen)->enable[vpos] = 0;
}

void
clear_screen_records (screen)
     register SCREEN_PTR screen;
{
  bzero (SCREEN_CURRENT_GLYPHS (screen)->enable, SCREEN_HEIGHT (screen));
}

/* Prepare to display on line VPOS starting at HPOS within it.
   Return the glyph string where that line */

void
get_display_line (s, vpos, hpos)
     register SCREEN_PTR s;
     int vpos;
     register int hpos;
{
  register struct screen_glyphs *desired_glyphs
    = SCREEN_DESIRED_GLYPHS (s);
  int run;

  if (vpos < 0 || (! SCREEN_VISIBLE_P (s)))
    /* abort ();   skip this */
    return;

  if ((desired_glyphs->enable[vpos]) && desired_glyphs->used[vpos] > hpos)
    /* abort ();   skip this */
    return;

  if (! desired_glyphs->enable[vpos])
    {
      desired_glyphs->used[vpos] = 0;
      desired_glyphs->nruns[vpos] = 0;
      desired_glyphs->face_list[vpos][0].type = unused_run;
      desired_glyphs->face_list[vpos][0].length = 0;
      desired_glyphs->face_list[vpos][0].faceptr = &SCREEN_NORMAL_FACE (s);
      desired_glyphs->face_list[vpos][0].w = 0;
      desired_glyphs->face_list[vpos][0].bufp = 0;
      desired_glyphs->face_list[vpos][0].class = Qnil;
      desired_glyphs->face_list[vpos][0].begin_p = 0;
#ifdef HAVE_X_WINDOWS
      desired_glyphs->face_list[vpos][0].pix_length = 0;
#endif
      desired_glyphs->enable[vpos] = 1;
      run = 0;
#ifdef HAVE_X_WINDOWS
      if (SCREEN_IS_X (s))
	{
	  desired_glyphs->pix_width[vpos] = 0;
	  desired_glyphs->pix_height[vpos] = 0;
	  desired_glyphs->max_ascent[vpos] = 0;
	  desired_glyphs->top_left_x[vpos] = 0;
	  desired_glyphs->top_left_y[vpos] = 0;
	}
#endif
    }
  else {
    /* Lucid change:  I don't know what this means, but it's probably
       better than crashing.  -- jwz. */
    if (desired_glyphs->nruns[vpos] == 0)
      return;

    /* More bogosity.  If all the runs on this line are 0 length, there's
       nothing to do. -- eb */
    /* Search through the face list looking either for runs of non-zero
       length, or for an unused_run indicating no more runs.  If we find
       an unused_run first, just return (there's nothing on this line.) */
    {
      int i = 0;

      while (desired_glyphs->face_list[vpos][i].length == 0)
	{
	  if (desired_glyphs->face_list[vpos][i].type == unused_run)
	    return;
	  i++;
	}
    }

    /* skip this */
    if (desired_glyphs->nruns[vpos] < 1)
      {
	desired_glyphs->nruns[vpos] = 1;
	desired_glyphs->used[vpos] = 0;
      }

    run = desired_glyphs->nruns[vpos] - 1;
  }

  if (hpos > desired_glyphs->used[vpos])
    {
      GLYPH *g = desired_glyphs->glyphs[vpos] + desired_glyphs->used[vpos];
      GLYPH *end = desired_glyphs->glyphs[vpos] + hpos;
      int diff = hpos - desired_glyphs->used[vpos];

      desired_glyphs->used[vpos] = hpos;
      desired_glyphs->face_list[vpos][run].length += diff;
#ifdef HAVE_X_WINDOWS
      if (SCREEN_IS_X (s))
	{
	  XFontStruct *font;
	  int width;

	  font = desired_glyphs->face_list[vpos][run].faceptr->font;
	  width = X_CHAR_WIDTH (font, SPACEGLYPH) * diff;
	  desired_glyphs->face_list[vpos][run].pix_length += width;
	  desired_glyphs->pix_width[vpos] += width;
      }
#endif
      while (g != end)
	*g++ = SPACEGLYPH;
    }
}

/* Like bcopy except never gets confused by overlap.  */

void
safe_bcopy (from, to, size)
     char *from, *to;
     int size;
{
  register char *endf;
  register char *endt;

  if (size == 0)
    return;

  /* If destination is higher in memory, and overlaps source zone,
     copy from the end. */
  if (from < to && from + size > to)
    {
      endf = from + size;
      endt = to + size;

      do
	*--endt = *--endf;
      while (endf != from);

      return;
    }

  bcopy (from, to, size);
}

/* Rotate a vector of SIZE bytes, by DISTANCE bytes.
   DISTANCE may be negative.  */

static void
rotate_vector (vector, size, distance)
     char *vector;
     int size;
     int distance;
{
  char *temp = (char *) alloca (size);

  if (distance < 0)
    distance += size;

  bcopy (vector, temp + distance, size - distance);
  bcopy (vector + size - distance, temp, distance);
  bcopy (temp, vector, size);
}

void update_begin ();
void set_terminal_window ();
void ins_del_lines ();
void update_end ();


/* Scroll lines from vpos `from' up to but not including vpos `end'
   down by `amount' lines (`amount' may be negative).
   Returns nonzero if done, zero if terminal cannot scroll them. */

int
scroll_screen_lines (screen, from, end, amount)
     register SCREEN_PTR screen;
     int from, end, amount;
{
  register struct screen_glyphs *current_screen
    = SCREEN_CURRENT_GLYPHS (screen);

  if (!line_ins_del_ok)
    return 0;

  if (amount == 0)
    return 1;

  if (amount > 0)
    {
      update_begin (screen);
      set_terminal_window (end + amount);
      if (!scroll_region_ok)
	ins_del_lines (end, -amount);
      ins_del_lines (from, amount);
      set_terminal_window (0);

      rotate_vector ((char*)(current_screen->glyphs + from),
		     sizeof (GLYPH *) * (end + amount - from),
		     amount * sizeof (GLYPH *));

      safe_bcopy ((char*)(current_screen->used + from),
		  (char*)(current_screen->used + from + amount),
		  (end - from) * sizeof current_screen->used[0]);

      safe_bcopy (current_screen->enable + from,
		  current_screen->enable + from + amount,
		  (end - from) * sizeof current_screen->enable[0]);

      bzero (current_screen->enable + from,
	     amount * sizeof current_screen->enable[0]);

      safe_bcopy ((char*)(current_screen->bufp + from),
		  (char*)(current_screen->bufp + from + amount),
		  (end - from) * sizeof current_screen->bufp[0]);

      rotate_vector ((char*)(current_screen->face_list + from),
		     sizeof (struct run *) * (end + amount - from),
		     amount * sizeof (struct run *));

      safe_bcopy ((char*)(current_screen->nruns + from),
		  (char*)(current_screen->nruns + from + amount),
		  (end - from) * sizeof current_screen->nruns[0]);

#ifdef HAVE_X_WINDOWS
      if (SCREEN_IS_X (screen))
	{
	  register int i;
	  register int pix_y;

	  pix_y = current_screen->top_left_y[0];

#if 0
	  safe_bcopy (current_screen->top_left_x + from,
		      current_screen->top_left_x + from + amount,
		      (end - from) * sizeof current_screen->top_left_x[0]);
#endif

	  safe_bcopy ((char*)(current_screen->pix_height + from),
		      (char*)(current_screen->pix_height + from + amount),
		      (end - from) * sizeof current_screen->pix_height[0]);

	  safe_bcopy ((char*)(current_screen->max_ascent + from),
		      (char*)(current_screen->max_ascent + from + amount),
		      (end - from) * sizeof current_screen->max_ascent[0]);

	  safe_bcopy ((char*)(current_screen->pix_width + from),
		      (char*)(current_screen->pix_width + from + amount),
		      (end - from) * sizeof current_screen->pix_width[0]);

	  for (i = 0; i < screen->height; i++)
	    {
	      current_screen->top_left_y[i] = pix_y;
	      pix_y += current_screen->pix_height[i];
	    }
	}
#endif /* HAVE_X_WINDOWS */

      update_end (screen);
    }

  if (amount < 0)
    {
      update_begin (screen);
      set_terminal_window (end);
      ins_del_lines (from + amount, amount);
      if (!scroll_region_ok)
	ins_del_lines (end + amount, -amount);
      set_terminal_window (0);

      rotate_vector ((char*)(current_screen->glyphs + from + amount),
		     sizeof (GLYPH *) * (end - from - amount),
		     amount * sizeof (GLYPH *));

      safe_bcopy ((char*)(current_screen->used + from),
		  (char*)(current_screen->used + from + amount),
		  (end - from) * sizeof current_screen->used[0]);

      safe_bcopy (current_screen->enable + from,
		  current_screen->enable + from + amount,
		  (end - from) * sizeof current_screen->enable[0]);

      bzero (current_screen->enable + end + amount,
	     - amount * sizeof current_screen->enable[0]);

      safe_bcopy ((char*)(current_screen->bufp + from),
		  (char*)(current_screen->bufp + from + amount),
		  (end - from) * sizeof current_screen->bufp[0]);

      rotate_vector ((char*)(current_screen->face_list + from + amount),
		     sizeof (struct run *) * (end - from - amount),
		     amount * sizeof (struct run *));

      safe_bcopy ((char*)(current_screen->nruns + from),
		  (char*)(current_screen->nruns + from + amount),
		  (end - from) * sizeof current_screen->nruns[0]);

#ifdef HAVE_X_WINDOWS
      if (SCREEN_IS_X (screen))
	{
	  register int pix_y;

	  pix_y = current_screen->top_left_y[0];

#if 0
	  safe_bcopy (current_screen->top_left_x + from,
		      current_screen->top_left_x + from + amount,
		      (end - from) * sizeof current_screen->top_left_x[0]);
#endif

	  safe_bcopy ((char*)(current_screen->pix_width + from),
		      (char*)(current_screen->pix_width + from + amount),
		      (end - from) * sizeof current_screen->pix_width[0]);

	  safe_bcopy ((char*)(current_screen->pix_height + from),
		      (char*)(current_screen->pix_height + from + amount),
		      (end - from) * sizeof current_screen->pix_height[0]);

	  safe_bcopy ((char*)(current_screen->max_ascent + from),
		      (char*)(current_screen->max_ascent + from + amount),
		      (end - from) * sizeof current_screen->max_ascent[0]);
	}
#endif /* HAVE_X_WINDOWS */

      update_end (screen);
    }
  return 1;
}

/* After updating a window w that isn't the full screen wide,
   copy all the columns that w does not occupy
   into the SCREEN_DESIRED_GLYPHS (screen) from the SCREEN_PHYS_GLYPHS (screen)
   so that update_screen will not change those columns.  */

void
preserve_other_columns (w)
     struct window *w;
{
  register int vpos;
  register struct screen_glyphs *current_screen, *desired_screen;
  register SCREEN_PTR screen = XSCREEN (w->screen);
  int start = XFASTINT (w->left);
  int end = XFASTINT (w->left) + XFASTINT (w->width);
  int bot = XFASTINT (w->top) + XFASTINT (w->height);

  current_screen = SCREEN_CURRENT_GLYPHS (screen);
  desired_screen = SCREEN_DESIRED_GLYPHS (screen);

  for (vpos = XFASTINT (w->top); vpos < bot; vpos++)
    {
      if (current_screen->enable[vpos] && desired_screen->enable[vpos])
	{
	  if (start > 0)
	    {
	      int len;

	      bcopy (current_screen->glyphs[vpos],
		     desired_screen->glyphs[vpos], start);
	      len = min (start, current_screen->used[vpos]);
	      if (desired_screen->used[vpos] < len)
		desired_screen->used[vpos] = len;
	    }
	  if (current_screen->used[vpos] > end
	      && desired_screen->used[vpos] < current_screen->used[vpos])
	    {
	      while (desired_screen->used[vpos] < end)
		desired_screen->glyphs[vpos][desired_screen->used[vpos]++]
		  = SPACEGLYPH;
	      bcopy (current_screen->glyphs + end,
		     desired_screen->glyphs + end,
		     current_screen->used[vpos] - end);
	      desired_screen->used[vpos] = current_screen->used[vpos];
	    }
	}
    }
}

/* On discovering that the redisplay for a window was no good,
   cancel the columns of that window, so that when the window is
   displayed over again get_display_line will not complain. */

void
cancel_my_columns (w)
     struct window *w;
{
  register int vpos;
  register SCREEN_PTR screen = XSCREEN (w->screen);
  register struct screen_glyphs *desired_glyphs = screen->desired_glyphs;
  register int start = XFASTINT (w->left);
  register int bot = XFASTINT (w->top) + XFASTINT (w->height);

  for (vpos = XFASTINT (w->top); vpos < bot; vpos++)
    if (desired_glyphs->enable[vpos]
	&& desired_glyphs->used[vpos] >= start)
      while (desired_glyphs->used[vpos] > start)
	{
	  register int n = desired_glyphs->nruns[vpos] - 1;

	  desired_glyphs->used[vpos]
	    -= desired_glyphs->face_list[vpos][n].length;
#ifdef HAVE_X_WINDOWS
	  if (SCREEN_IS_X (screen))
	    desired_glyphs->pix_width[vpos]
	      -= desired_glyphs->face_list[vpos][n].pix_length;
#endif
	  desired_glyphs->nruns[vpos]--;
	}
}


int fflush ();
void write_glyphs ();

/* These functions try to perform directly and immediately on the screen
   the necessary output for one change in the buffer.
   They may return 0 meaning nothing was done if anything is difficult,
   or 1 meaning the output was performed properly.
   They assume that the screen was up to date before the buffer
   change being displayed.  THey make various other assumptions too;
   see command_loop_1 where these are called.  */

int
direct_output_for_insert (g)
     int g;
{
  register SCREEN_PTR screen = selected_screen;
  register struct screen_glyphs *current_screen
    = SCREEN_CURRENT_GLYPHS (screen);
  register int i;

#ifndef COMPILER_REGISTER_BUG
  register
#endif /* COMPILER_REGISTER_BUG */
    struct window *w = XWINDOW (selected_window);
#ifndef COMPILER_REGISTER_BUG
  register
#endif /* COMPILER_REGISTER_BUG */
    int hpos = SCREEN_CURSOR_X (screen);
#ifndef COMPILER_REGISTER_BUG
  register
#endif /* COMPILER_REGISTER_BUG */
    int vpos = SCREEN_CURSOR_Y (screen);

  /* Give up if about to continue line */
  if (hpos - XFASTINT (w->left) + 1 + 1 >= XFASTINT (w->width)

  /* Avoid losing if cursor is in invisible text off left margin */
      || (XINT (w->hscroll) && hpos == XFASTINT (w->left))
    
  /* Give up if cursor outside window (in minibuf, probably) */
      || SCREEN_CURSOR_Y (screen) < XFASTINT (w->top)
      || SCREEN_CURSOR_Y (screen) >= XFASTINT (w->top) + XFASTINT (w->height)

  /* Give up if cursor not really at SCREEN_CURSOR_X, SCREEN_CURSOR_Y */
      || !display_completed

  /* Give up if buffer appears in two places.  */
      || buffer_shared > 1

  /* Give up if w is minibuffer and a message is being displayed there */
      || (MINI_WINDOW_P (w) && echo_area_glyphs))
    return 0;

  current_screen->glyphs[vpos][hpos] = g;
  for (i = vpos+1; i < SCREEN_HEIGHT (screen); i++)
    if (current_screen->bufp[i])
      current_screen->bufp[i]++;

  unchanged_modified = MODIFF;
  beg_unchanged = GPT - BEG;
  XFASTINT (w->last_point) = point;
  XFASTINT (w->last_point_x) = hpos;
  XFASTINT (w->last_modified) = MODIFF;
  XFASTINT (w->last_facechange) = FACECHANGE;

  if (hpos == current_screen->used[vpos])
    {
      int last_run;

      if (current_screen->nruns[vpos] == 0)
	current_screen->nruns[vpos] = 1;

      last_run = current_screen->nruns[vpos] - 1;

      current_screen->used[vpos] = hpos + 1;
      current_screen->glyphs[vpos][hpos + 1] = 0;
      current_screen->face_list[vpos][last_run].length++;

#ifdef HAVE_X_WINDOWS
      if (SCREEN_IS_X (screen))
	{
	  XFontStruct *font;
	  int width;

	  font = current_screen->face_list[vpos][last_run].faceptr->font;
	  if (!font)
	    font = screen->display.x->font;
	  width = X_CHAR_WIDTH (font, g);
	  current_screen->face_list[vpos][last_run].pix_length += width;
	  current_screen->pix_width[vpos] += width;
	}
#endif
    }

  write_glyphs (hpos, vpos, 1);
  fflush (stdout);
  ++SCREEN_CURSOR_X (screen);

  return 1;
}

int
direct_output_forward_char (n)
     int n;
{
  /* Ok, I'm sick of this.  This function doesn't work right when glyphs
     are involved, and neither does the compute_motion() hack below, so
     just blow it off.  If we return 0 here, the cursor position will
     be computed later in a way that works.  --jwz.
   */
  return 0;

#if 0
  register SCREEN_PTR screen = selected_screen;
  register struct window *w = XWINDOW (selected_window);

  /* Avoid losing if cursor is in invisible text off left margin */
  if (XINT (w->hscroll) && SCREEN_CURSOR_X (screen) == XFASTINT (w->left)

  /* Give up if about to continue line */
    || (SCREEN_CURSOR_X (screen) - XFASTINT (w->left) + 1 + 1
	>= XFASTINT (w->width))
    
  /* Give up if cursor outside window (in minibuf, probably) */
      || SCREEN_CURSOR_Y (screen) < XFASTINT (w->top)
      || SCREEN_CURSOR_Y (screen) >= XFASTINT (w->top) + XFASTINT (w->height)

  /* Give up if cursor not really at SCREEN_CURSOR_X, SCREEN_CURSOR_Y */
      || !display_completed

  /* Give up if buffer appears in two places.  */
      || buffer_shared > 1

  /* Give up if w is minibuffer and a message is being displayed there */
      || (MINI_WINDOW_P (w) && echo_area_glyphs))
    return 0;
  SCREEN_CURSOR_X (screen) += n;

  {
    /* This has to be a lot more complicated because of glyphs: to move
       forward one character, the cursor must sometimes move multiple
       character-positions on the screen.  Only compute-motion knows how
       to do this right, since the screen data structures really suck.
     */
    int cx = SCREEN_CURSOR_X (screen);
    int cy = SCREEN_CURSOR_Y (screen);
    int line_begin_pos      = SCREEN_CURRENT_GLYPHS (screen)->bufp[cy];
    int next_line_begin_pos = SCREEN_CURRENT_GLYPHS (screen)->bufp[cy + 1];

    struct position *position
      = compute_motion (SCREEN_SELECTED_WINDOW (screen),
			/* from, fromvpos, fromhpos, */
			line_begin_pos, cy, 0,
			/* to, tovpos, tohpos, */
			next_line_begin_pos, cy, cx + n,
			/* width, hscroll, tab_offset */
			XINT(w->width), XINT(w->hscroll),
			pos_tab_offset(w, line_begin_pos),
			0, 0);
    if (SCREEN_CURSOR_Y (screen) != position->vpos)
      return 0;
    SCREEN_CURSOR_X (screen) = position->hpos;
  }

  XFASTINT (w->last_point_x) = SCREEN_CURSOR_X (screen);
  XFASTINT (w->last_point) = point;
  cursor_to (SCREEN_CURSOR_Y (screen), SCREEN_CURSOR_X (screen));
  fflush (stdout);
  return 1;
#endif
}

static int
count_blanks (r)
     register GLYPH *r;
{
  register GLYPH *p = r;
  while (*r++ == SPACEGLYPH);
  return r - p - 1;
}

static int
count_match (str1, str2)
     GLYPH *str1, *str2;
{
  register GLYPH *p1 = str1;
  register GLYPH *p2 = str2;
  while (*p1++ == *p2++);
  return p1 - str1 - 1;
}

/* Char insertion/deletion cost vector, from term.c */
extern int *char_ins_del_vector;

void cursor_to ();
void clear_end_of_line ();
void delete_glyphs ();
void insert_spaceglyphs ();
void insert_glyphs ();

#define char_ins_del_cost(s) (&char_ins_del_vector[SCREEN_HEIGHT((s))])

static void
update_line (screen, vpos)
     register SCREEN_PTR screen;
     int vpos;
{
  register GLYPH *obody, *nbody, *op1, *op2, *np1, *temp;
  int tem;
  int osp, nsp, m1, m2, olen, nlen;
  int save;
  register struct screen_glyphs *current_screen
    = SCREEN_CURRENT_GLYPHS (screen);
  register struct screen_glyphs *desired_screen
    = SCREEN_DESIRED_GLYPHS (screen);
  int nruns = desired_screen->nruns[vpos];
  int runs = current_screen->nruns[vpos];
  struct run *temp_runs;

  if (! current_screen->enable[vpos])
    {
      olen = 0;
    }
  else
    {
      obody = current_screen->glyphs[vpos];
      olen = current_screen->used[vpos];
      if (!must_write_spaces
	  && runs == 1
	  && ! current_screen->face_list[vpos][0].faceptr->hilited
	  && ! current_screen->face_list[vpos][0].faceptr->underline)
	while (obody[olen - 1] == SPACEGLYPH && olen > 0)
	  olen--;
    }

  nbody = desired_screen->glyphs[vpos];
  nlen = desired_screen->used[vpos];

  /* One way or another, this will enable the line being updated. */
  current_screen->enable[vpos] = 1;
  current_screen->used[vpos] = desired_screen->used[vpos];
  current_screen->bufp[vpos] = desired_screen->bufp[vpos];
  current_screen->nruns[vpos] = desired_screen->nruns[vpos];

  if (!desired_screen->enable[vpos])
    {
      struct run *run = desired_screen->face_list[vpos];

      current_screen->face_list[vpos] = desired_screen->face_list[vpos];
      desired_screen->face_list[vpos] = run;

      nlen = 0;
      goto just_erase;
    }

  if (!must_write_spaces
      && runs == 1
      && ! desired_screen->face_list[vpos][0].faceptr->hilited
      && ! desired_screen->face_list[vpos][0].faceptr->underline)
    /* We know that the previous character byte contains 0. */
    while (nbody[nlen - 1] == SPACEGLYPH)
      nlen--;

  /* Exchange contents between current_screen and new_screen.  */
  temp = desired_screen->glyphs[vpos];
  desired_screen->glyphs[vpos] = current_screen->glyphs[vpos];
  current_screen->glyphs[vpos] = temp;

  /* If there's no i/d char or there are several runs,
     output non-matching runs...  */
  if (!char_ins_del_ok || nruns > 1)
    {
      int i,j;
      int this_run;
      struct run *new_runs = desired_screen->face_list[vpos];
      struct run *old_runs = current_screen->face_list[vpos];
      int len_runs_output;

      desired_screen->face_list[vpos] = old_runs;
      current_screen->face_list[vpos] = new_runs;

      i = 0;
      this_run = 0;
      len_runs_output = 0;

      while (this_run < nruns && nlen > 0)
	{
	  int run_len = new_runs[this_run].length;
	  int orun_len;

	  /* If past the end of the old runs, or the old contents,
	     just write out the rest of the line and quit. */
	  if (this_run >= runs || i > olen || olen == 0)
	    {
	      cursor_to (vpos, i);
	      write_glyphs (i, vpos, nlen - len_runs_output);
	      break;
	    }

	  orun_len = old_runs[this_run].length;
	  /* If this run has a different face, write out the whole run. */
	  if (old_runs[this_run].faceptr != new_runs[this_run].faceptr
	      || new_runs[this_run].faceptr->modif)
	    {
	      new_runs[this_run].faceptr->modif = 0;
	      cursor_to (vpos, i);
	      if (old_runs[this_run].faceptr->underline
		  && ! new_runs[this_run].faceptr->underline)
		{
		  clear_end_of_line (i + orun_len);
		}
	      write_glyphs (i, vpos, run_len);
	      i += run_len;
	    }
	  else
	    {
	      int len = min (run_len, orun_len);

	      /* Compare the contents, writing out what's changed. */
	      while (i < len_runs_output + len)
		{
		  if (i > olen || nbody[i] != obody[i])
		    {
		      cursor_to (vpos, i);
		      for (j = 1; (i + j < len_runs_output + len
				   && (nbody[i+j] != obody[i+j]));
			   j++);

		      write_glyphs (i, vpos, j);
		      i += j;
		    }
		  else
		    i++;
		}

	      /* If the new run is longer than the old, just rewrite those
		 trailing characters, as they could be the same but with
		 a different face. */
	      if (len < run_len)
		{
		  cursor_to (vpos, i);
		  write_glyphs (i, vpos, (run_len - len));
		  i += (run_len - len);
		}
	    }

	  this_run++;
	  len_runs_output += run_len;
	}

      /* Exits */
      goto just_erase;
    }

  if (!nlen)
    {
      temp_runs = desired_screen->face_list[vpos];
      desired_screen->face_list[vpos] = current_screen->face_list[vpos];
      current_screen->face_list[vpos] = temp_runs;

      /* Exits */
      goto just_erase;
    }

  if (current_screen->face_list[vpos][0].faceptr
      != desired_screen->face_list[vpos][0].faceptr
      || desired_screen->face_list[vpos][0].faceptr->modif)
    {
      desired_screen->face_list[vpos][0].faceptr->modif = 0;
      temp_runs = desired_screen->face_list[vpos];
      desired_screen->face_list[vpos] = current_screen->face_list[vpos];
      current_screen->face_list[vpos] = temp_runs;

      cursor_to (vpos, 0);
      write_glyphs (0, vpos, nlen);

      /* Exits */
      goto just_erase;
    }

  if (!olen)
    {
      temp_runs = desired_screen->face_list[vpos];
      desired_screen->face_list[vpos] = current_screen->face_list[vpos];
      current_screen->face_list[vpos] = temp_runs;

      nsp = (must_write_spaces
	     || desired_screen->face_list[vpos][0].faceptr->hilited
	     || desired_screen->face_list[vpos][0].faceptr->underline)
	? 0
	  : count_blanks (nbody);
      if (nlen > nsp)
	{
	  cursor_to (vpos, nsp);
	  write_glyphs (nsp, vpos, nlen - nsp);
	}

      return;
    }

  temp_runs = desired_screen->face_list[vpos];
  desired_screen->face_list[vpos] = current_screen->face_list[vpos];
  current_screen->face_list[vpos] = temp_runs;

  obody[olen] = 1;
  save = nbody[nlen];
  nbody[nlen] = 0;

  /* Compute number of leading blanks in old and new contents.  */
  osp = ((current_screen->face_list[vpos][0].faceptr->hilited
	  || current_screen->face_list[vpos][0].faceptr->underline)
	 ? 0
	 : count_blanks (obody));
  nsp = ((desired_screen->face_list[vpos][0].faceptr->hilited
	  || desired_screen->face_list[vpos][0].faceptr->underline)
	 ? 0
	 : count_blanks (nbody));

  /* Compute number of matching chars starting with first nonblank.  */
  m1 = count_match (obody + osp, nbody + nsp);

  /* Spaces in new match implicit space past the end of old.  */
  /* A bug causing this to be a no-op was fixed in 18.29.  */
  if (!must_write_spaces && osp + m1 == olen)
    {
      np1 = nbody + nsp;
      while (np1[m1] == SPACEGLYPH)
	m1++;
    }

  /* Avoid doing insert/delete char
     just cause number of leading spaces differs
     when the following text does not match. */
  if (m1 == 0 && osp != nsp)
    osp = nsp = min (osp, nsp);

  /* Find matching characters at end of line */
  op1 = obody + olen;
  np1 = nbody + nlen;
  op2 = op1 + m1 - min (olen - osp, nlen - nsp);
  while (op1 > op2 && op1[-1] == np1[-1])
    {
      op1--;
      np1--;
    }
  m2 = obody + olen - op1;

  /* Put correct value back in nbody[nlen].
     This is important because direct_output_for_insert
     can write into the line at a later point.  */
  nbody[nlen] = save;

  /* tem gets the distance to insert or delete.
     m2 is how many characters we save by doing so.
     Is it worth it?  */

  tem = (nlen - nsp) - (olen - osp);
  if (m2 && tem && (!char_ins_del_ok || m2 <= char_ins_del_cost (screen)[tem]))
    m2 = 0;

  /* nsp - osp is the distance to insert or delete.
     If that is nonzero, m1 is known to be nonzero also.
     m1 + m2 is how much we save by doing the ins/del.
     Is it worth it?  */

  if (nsp != osp
      && (!char_ins_del_ok
	  || m1 + m2 <= char_ins_del_cost (screen)[nsp - osp]))
    {
      m1 = 0;
      m2 = 0;
      osp = nsp = min (osp, nsp);
    }

  /* Now go through the line, inserting, writing
     and deleting as appropriate.  */

  if (osp > nsp)
    {
      cursor_to (vpos, nsp);
      delete_glyphs (osp - nsp);
    }
  else if (nsp > osp)
    {
      /* If going to delete chars later in line
	 and insert earlier in the line,
	 must delete first to avoid losing data in the insert */
      if (m2 && nlen < olen + nsp - osp)
	{
	  cursor_to (vpos, nlen - m2 + osp - nsp);
	  delete_glyphs (olen + nsp - osp - nlen);
	  olen = nlen - (nsp - osp);
	}
      cursor_to (vpos, osp);
      insert_spaceglyphs (osp, vpos, nsp - osp);
    }
  olen += nsp - osp;

  tem = nsp + m1 + m2;
  if (nlen != tem || olen != tem)
    {
      cursor_to (vpos, nsp + m1);
      if (!m2 || nlen == olen)
	{
	  /* If new text being written reaches right margin,
	     there is no need to do clear-to-eol at the end.
	     (and it would not be safe, since cursor is not
	     going to be "at the margin" after the text is done) */
	  if (nlen == SCREEN_WIDTH (screen))
	    olen = 0;
	  write_glyphs (nsp + m1, vpos, nlen - tem);
	}
      else if (nlen > olen)
	{
	  write_glyphs (nsp + m1, vpos, olen - tem);
	  insert_glyphs (nsp + m1 + olen - tem, vpos, nlen - olen);
	  olen = nlen;
	}
      else if (olen > nlen)
	{
	  write_glyphs (nsp + m1, vpos, nlen - tem);
	  delete_glyphs (olen - nlen);
	  olen = nlen;
	}
    }

 just_erase:
  /* If any unerased characters remain after the new line, erase them.  */
  if (olen > nlen)
    {
      cursor_to (vpos, nlen);
      clear_end_of_line (olen);
    }
}

#ifdef HAVE_X_WINDOWS
static void
x_update_line (screen, vpos, pix_y)
     register SCREEN_PTR screen;
     int vpos, pix_y;
{
  /* What we want to show. */
  register struct screen_glyphs *desired_screen
    = SCREEN_DESIRED_GLYPHS (screen);
  struct run *faces = desired_screen->face_list[vpos];
  int pix_length = desired_screen->pix_width[vpos];
  int used = desired_screen->used[vpos];
  int nruns = desired_screen->nruns[vpos];
  GLYPH *glyphs = desired_screen->glyphs[vpos];
  int pix_height = desired_screen->pix_height[vpos];
  int ascent = desired_screen->max_ascent[vpos];

  /* What is currently shown. */
  register struct screen_glyphs *current_screen
    = SCREEN_CURRENT_GLYPHS (screen);
  int currently_enabled = current_screen->enable[vpos];
  struct run *old_faces = 0;
  int old_pix_length = 0;
  int old_used = 0;
  int oruns = 0;
  GLYPH *old_glyphs = 0;
  int old_pix_y = 0;
  int old_pix_height = 0;
  int old_ascent = 0;

  GLYPH *temp;
  struct run *temp_runs;
  register int g, this_run;
  int line_changed;

  if (currently_enabled)
    {
      old_faces = current_screen->face_list[vpos];
      old_faces = current_screen->face_list[vpos];
      old_pix_length = current_screen->pix_width[vpos];
      old_used = current_screen->used[vpos];
      oruns = current_screen->nruns[vpos];
      old_glyphs = current_screen->glyphs[vpos];
      old_pix_y = current_screen->top_left_y[vpos];
      old_pix_height = current_screen->pix_height[vpos];
      old_ascent = current_screen->max_ascent[vpos];
    }

  current_screen->enable[vpos] = 1;
  current_screen->used[vpos] = desired_screen->used[vpos];
  current_screen->bufp[vpos] = desired_screen->bufp[vpos];
  current_screen->nruns[vpos] = desired_screen->nruns[vpos];
  current_screen->pix_width[vpos] = desired_screen->pix_width[vpos];
  current_screen->pix_height[vpos] = desired_screen->pix_height[vpos];
  current_screen->max_ascent[vpos] = desired_screen->max_ascent[vpos];

  current_screen->top_left_y[vpos] = pix_y;
#ifdef LINE_INFO_COLUMN
  current_screen->top_left_x[vpos] = screen->display.x->internal_border_width;
#else
  current_screen->top_left_x[vpos] = screen->display.x->internal_border_width;
#endif

  temp_runs = current_screen->face_list[vpos];
  current_screen->face_list[vpos] = desired_screen->face_list[vpos];
  desired_screen->face_list[vpos] = temp_runs;

  temp = desired_screen->glyphs[vpos];
  desired_screen->glyphs[vpos] = current_screen->glyphs[vpos];
  current_screen->glyphs[vpos] = temp;

/*  screen->cursor_y = vpos; */

#ifdef LINE_INFO_COLUMN
  /* If there was a lineinfo glyph and there isn't now, or it has
     changed, then clear it */
 /*  x_clear_lineinfo_glyph (screen, vpos); */
#endif

  if (used == 0)
    {
      /* Nothing here now.  Clear anything that was here before. */
      if (old_used > 0 || pix_y != old_pix_y)
	x_clear_display_line (vpos, old_pix_length);
      return;
    }

  /* Clear screen or something erased the line, or it was blank. */
  if (old_used == 0 || ! currently_enabled)
    {
      x_write_glyphs (screen, 0, vpos, used, 0, 0);
      return;
    }

  /* Remove blanks as optimization...? */

  if (pix_y < old_pix_y || pix_height > old_pix_height || ascent != old_ascent)
    {
      /* Different vertical coordinates, so rewrite the whole thing. */
      x_clear_display_line (vpos, old_pix_length);
      x_write_glyphs (screen, 0, vpos, used, 0, 0);
      return;
    }

  /* Go through all the runs. */
  g = 0;
  /* If this line wasn't enabled before, it's changed completely */
  line_changed = !currently_enabled;
  for (this_run = 0; this_run < nruns; this_run++)
    {
      /* If there are no more old runs to compare to, punt. */
      if (this_run >= oruns)
	{
	  x_write_glyphs (screen, g, vpos, used - g, 0, 0);
	  break;
	}

      {

	int from = g;
	int len = faces[this_run].length;
	int to = g + len;
	int oldlen = old_faces[this_run].length;
	int to_old = g + oldlen;

#ifdef LINE_INFO_COLUMN
	if (faces[this_run].type == column_glyph &&
	    old_faces[this_run].type == column_glyph)
	  {
	    if (faces[this_run].lineinfo_glyph_index ==
		old_faces[this_run].lineinfo_glyph_index)
	      {
		g = to;
		continue;
	      }
	  }
	else
#endif
	  /* If everything else is the same, compare the line contents. */
	  if (faces[this_run].faceptr == old_faces[this_run].faceptr
	      && ! faces[this_run].faceptr->modif
	      && faces[this_run].type == old_faces[this_run].type
	      && !line_changed)
	    {
	      while (from < to && from < to_old
		     && glyphs[from] == old_glyphs[from])
		from++;
	    }
	if (from != to)
	  {
	    x_write_glyphs (screen, from, vpos, to - from, 0, 0);
	    line_changed = 1;
	  }
	else if (from != to_old)
	  /* The new run only matches the beginning of the old run, so the line
	     has changed */
	  line_changed = 1;
	g = to;
	}
    }
  if (pix_length < old_pix_length)
    x_clear_display_line_end (vpos);
}
#endif /* HAVE_X_WINDOWS */

int detect_input_pending ();
int scrolling ();
int ioctl ();
unsigned sleep ();		/* int clashes with <unistd.h> */

/* At the time this function is called, no line is common
   to SCREEN_PHYS_LINES (screen) and SCREEN_DESIRED_LINES (screen).
   That is true again when this function returns.

   force' nonzero means do not stop for pending input

   value is nonzero if redisplay stopped due to pending input */

int
update_screen (s, force, inhibit_hairy_id)
     SCREEN_PTR s;
     int force;
     int inhibit_hairy_id;
{
  register struct screen_glyphs *current_screen = SCREEN_CURRENT_GLYPHS (s);
  register struct screen_glyphs *desired_screen = SCREEN_DESIRED_GLYPHS (s);
  register int i;
  int pause;
  int preempt_count;
  int input_pending;
  int cursor_hpos = SCREEN_CURSOR_X (s);
  int cursor_vpos = SCREEN_CURSOR_Y (s);
#ifdef HAVE_X_WINDOWS
  register int downto;
#endif

  input_pending = detect_input_pending ();
  if (input_pending && !force)
    {
      bzero (desired_screen->enable, SCREEN_HEIGHT (s));
      pause = 1;
      goto do_pause;
    }

  update_begin (s);

  if (!line_ins_del_ok)
    inhibit_hairy_id = 1;

  /* Don't compute for i/d line if just want cursor motion. */
  for (i = 0; i < SCREEN_HEIGHT (s); i++)
    if (desired_screen->enable[i])
      break;

  /* Try doing i/d line, if not yet inhibited.  */
  if (!inhibit_hairy_id && i < SCREEN_HEIGHT (s) && SCREEN_IS_TERMCAP (s))
    force |= scrolling (s);

#ifdef HAVE_X_WINDOWS
  if (SCREEN_IS_X (s))
    downto = s->display.x->internal_border_width;
#endif /* HAVE_X_WINDOWS */

  if (SCREEN_IS_TERMCAP (s))
    preempt_count = baud_rate / 2400;

  for (i = 0; i < SCREEN_HEIGHT (s) && (force || !input_pending); i++)
    {
      if (desired_screen->enable[i])
	{
	  if (SCREEN_IS_TERMCAP (s))
	    {
	      /* Flush out every so many lines.
		 Also flush out if likely to have more than 1k buffered
		 otherwise.   I'm told that some telnet connections get
		 really screwed by more than 1k output at once.  */
	      int outq = PENDING_OUTPUT_COUNT (stdout);
	      if (outq > ((--preempt_count < 0) ? 20 : 900))
		{
		  fflush (stdout);
		  if (baud_rate < 2400)
		    {
#ifdef TIOCOUTQ
		      if (ioctl (0, TIOCOUTQ, &outq) < 0)
			/* Probably not a tty.  Ignore the error and reset
			 * the outq count. */
			outq = PENDING_OUTPUT_COUNT (stdout);
#endif
		      outq *= 10;
		      outq /= baud_rate; /* outq is now in seconds */
		      if (outq)
			sleep (outq);
		    }
		  input_pending = detect_input_pending ();

		  preempt_count = baud_rate / 2400;
		}
	    }

#ifdef HAVE_X_WINDOWS
	  if (SCREEN_IS_X (s))
	    x_update_line (s, i, downto);
	  else
#endif
	    update_line (s, i);
	}

      if (SCREEN_IS_X (s))
	downto += s->display.x->text_height;
    }
  pause = (i < SCREEN_HEIGHT (s) - 1) ? i : 0;

  /* Now just clean up termcap drivers and set cursor, etc.  */
  if (!pause)
    {
      if (s == selected_screen && cursor_in_echo_area)
	{
	  if (cursor_in_echo_area < 0)
	    cursor_to (SCREEN_HEIGHT (s) - 1, 0);
	  else
	    cursor_to (SCREEN_HEIGHT (s) - 1,
		       min (SCREEN_WIDTH (s) - 1,
			    current_screen->used[SCREEN_HEIGHT (s) - 1]));
	  s->cursor_x = s->phys_cursor_x;
	  s->cursor_y = s->phys_cursor_y;
	}
      else
	cursor_to (cursor_vpos, max (min (cursor_hpos, SCREEN_WIDTH (s) - 1),
				     0));
    }

  bzero (desired_screen->enable, SCREEN_HEIGHT (s));
  update_end (s);

  if (termscript)
    fflush (termscript);
  fflush (stdout);

  /* Here if output is preempted because input is detected.  */
 do_pause:

  if (SCREEN_HEIGHT (s) == 0)
    /* abort ();   skip this */
    return 0;
  display_completed = !pause;

  return pause;
}


/* Decide what insert/delete line to do, and do it */

extern void scrolling_1 ();
int scrolling_max_lines_saved ();

int
scrolling (screen)
     SCREEN_PTR screen;
{
  int unchanged_at_top, unchanged_at_bottom;
  int window_size;
  int changed_lines;
  int *old_hash = (int *) alloca (SCREEN_HEIGHT (screen) * sizeof (int));
  int *new_hash = (int *) alloca (SCREEN_HEIGHT (screen) * sizeof (int));
  int *draw_cost = (int *) alloca (SCREEN_HEIGHT (screen) * sizeof (int));
  register int i;
  int free_at_end_vpos = SCREEN_HEIGHT (screen);
  register struct screen_glyphs *current_screen = SCREEN_CURRENT_GLYPHS (screen);
  register struct screen_glyphs *desired_screen = SCREEN_DESIRED_GLYPHS (screen);

  /* Compute hash codes of all the lines.
     Also calculate number of changed lines,
     number of unchanged lines at the beginning,
     and number of unchanged lines at the end.  */

  changed_lines = 0;
  unchanged_at_top = 0;
  unchanged_at_bottom = SCREEN_HEIGHT (screen);
  for (i = 0; i < SCREEN_HEIGHT (screen); i++)
    {
      old_hash[i] = line_hash_code (current_screen, i);
      if (! desired_screen->enable[i])
	new_hash[i] = old_hash[i];
      else
	new_hash[i] = line_hash_code (desired_screen, i);

      if (old_hash[i] != new_hash[i])
	{
	  changed_lines++;
	  unchanged_at_bottom = SCREEN_HEIGHT (screen) - i - 1;
	}
      else if (i == unchanged_at_top)
	unchanged_at_top++;
      draw_cost[i] = line_draw_cost (desired_screen, i);
    }

  /* If changed lines are few, don't allow preemption, don't scroll.  */
  if (changed_lines < baud_rate / 2400
      || unchanged_at_bottom == SCREEN_HEIGHT (screen))
    return 1;

  window_size = (SCREEN_HEIGHT (screen) - unchanged_at_top
		 - unchanged_at_bottom);

  if (scroll_region_ok)
    free_at_end_vpos -= unchanged_at_bottom;
  else if (memory_below_screen)
    free_at_end_vpos = -1;

  /* If large window, fast terminal and few lines in common between
     current screen and desired screen, don't bother with i/d calc. */
  if (window_size >= 18 && baud_rate > 2400
      && (window_size >=
	  10 * scrolling_max_lines_saved (unchanged_at_top,
					  SCREEN_HEIGHT (screen) - unchanged_at_bottom,
					  old_hash, new_hash, draw_cost)))
    return 0;

  scrolling_1 (screen, window_size, unchanged_at_top, unchanged_at_bottom,
	       draw_cost + unchanged_at_top - 1,
	       old_hash + unchanged_at_top - 1,
	       new_hash + unchanged_at_top - 1,
	       free_at_end_vpos - unchanged_at_top);

  return 0;
}

#ifdef HAVE_X_WINDOWS

extern void abort ();

int
pixel_to_glyph_translation (s, pix_x, pix_y, x, y, w, bufp, gly, class,
			    begin_p)
     SCREEN_PTR s;
     register unsigned int pix_x, pix_y;
     register int *x, *y;
     struct window** w;
     int* bufp;
     int* gly;
     Lisp_Object* class;
     int* begin_p;
{
  register struct screen_glyphs *current_screen = SCREEN_CURRENT_GLYPHS (s);
  register int line;
  int ibw = s->display.x->internal_border_width;
#ifdef LINE_INFO_COLUMN
  int licw  = s->display.x->line_info_column_width;
#endif
  register XFontStruct *this_font;
  int tab_width = 8; /* is set to buffer tab_width later */
  int tab_offset = 0;		/* Wrong, fix this later */
  int hscroll = 0;		/* Wrong, fix this later */
  struct buffer *buffer;
  int ctl_arrow;

  *x = 0;
  *y = 0;

  *w = 0;
  *bufp = 0;
  *gly = Qnil;
  *class = Qnil;
  *begin_p = 0;

  line = (pix_y - ibw) / s->display.x->text_height;

  if (line < 0 || line >= SCREEN_HEIGHT (s))
    return 2;

  *y = line;

  /* try to set the window to something to start with */
  if (current_screen->enable[line]
      && current_screen->face_list[line][0].type == window)
    {
      *w = current_screen->face_list[line][0].w;
      buffer = XBUFFER ((*w)->buffer);
      tab_width = buffer->tab_width;
      ctl_arrow = buffer->ctl_arrow;
    }

  if (!*w)
    return 2;

  /* check if minibuffer window is enabled */
  if (*w && MINI_WINDOW_P (*w) && !minibuf_level)
    {
      *w = 0;
      return 2;
    }

  if (pix_x <= ibw || pix_y <= ibw
      || pix_y >= s->display.x->pixel_height - ibw
      || ! current_screen->enable[line])
    {
      return 2;
    }

  if (pix_x >= current_screen->pix_width[line] + ibw
#ifdef LINE_INFO_COLUMN
      + licw
#endif
      )
    {
      *x = current_screen->used[line];
      return 2;
    }

#ifdef LINE_INFO_COLUMN
  if (pix_x >= ibw && pix_x <= ibw + licw) {
    int run = 0;
    while (current_screen->face_list[line][run].type == window ||
	   current_screen->face_list[line][run].type == column_glyph)
      run++;
    if (run != 0 && 
	current_screen->face_list[line][--run].type == column_glyph) {
      *class = current_screen->face_list[line][run].class;
      *begin_p = current_screen->face_list[line][run].begin_p;
      *gly = current_screen->glyphs[line][run];
      return 1;
    }
    return 2;
  }
#endif

    {
      register char g;
      register int this_pix;
      register int run, glyphs_in_run, glyphs_in_run_limit;
      int next_bufp, bufcol;
      int bufpos, minibuf_len;

      g = 0;
      run = 0;
      glyphs_in_run = current_screen->face_list[line][run].length;
      glyphs_in_run_limit = g + glyphs_in_run;
      bufpos = current_screen->bufp[line];
      next_bufp = bufpos;
      bufcol = 0;
      if (bufpos < 0)
	minibuf_len = strlen (minibuf_prompt);
      this_pix = current_screen->top_left_x[line];
      switch (current_screen->face_list[line][run].type)
	{
	case font:
	{
	  int c;
	  int c_width;
	  this_font = current_screen->face_list[line][run].faceptr->font;
	  if (!this_font)
	    this_font = s->display.x->font;
	  c = current_screen->glyphs[line][g];
	  c_width = X_CHAR_WIDTH (this_font, c == TABGLYPH ? ' ' : c);
	  this_pix += c_width;
	  next_bufp = bufpos + 1;
	  bufcol++;

	  c = ((bufpos > 0)
	       ? BUF_CHAR_AT (buffer, bufpos)
	       : minibuf_prompt[minibuf_len + bufpos - 1]);
	  if (c >= 040 && c < 0177) /* Normal character. */
	    {
	    }
	  else if (c == '\t')	/* Tab character. */
	    {
	      register int j;

	      j = tab_width - ((((bufcol - 1) + tab_offset + hscroll - (hscroll > 0))
				% tab_width)) - 1;
	      g += j;
	      bufcol += j;
	      this_pix += j * X_CHAR_WIDTH (this_font, ' ');
	    }
	  else if (c < 0200 && ctl_arrow)
	    {
	      g++;
	      bufcol++;
	      this_pix += X_CHAR_WIDTH (this_font, c ^ 0100);
	    }
	  else
	    {
	      g += 3;
	      bufcol += 3;
	      this_pix += X_CHAR_WIDTH (this_font, ((c >> 6) + '0'));
	      this_pix += X_CHAR_WIDTH (this_font, ((7 & (c >> 3)) + '0'));
	      this_pix += X_CHAR_WIDTH (this_font, ((7 & c) + '0'));
	    }
	  break;
	}
	  
	case space:
	  this_pix += current_screen->face_list[line][run].pix_length;
	  /* bufcol++; */ /* Only chars count for tabs */
	  next_bufp = bufpos + 1;
	  break;
	  
	case glyph:
	  /* bufcol++; */ /* Only chars count for tabs */
	case column_glyph:
	  this_pix += current_screen->face_list[line][run].pix_length;
	  *class = current_screen->face_list[line][run].class;
	  *begin_p = current_screen->face_list[line][run].begin_p;
	  *gly = current_screen->glyphs[line][g];
	  break;
	  
	case window:
	  *w = current_screen->face_list[line][run].w;
	  tab_width = XBUFFER((*w)->buffer)->tab_width;
	  next_bufp = current_screen->face_list[line][run].bufp;
	  g--;			/* window runs don't consume glyphs */
	  break;
	  
	default:
	  abort ();
	}
      
      while (this_pix < pix_x)
	{
	  *x = (*x) + 1;
	  g++;
	  if (next_bufp > BUF_ZV (buffer))
	    return 2;		/* Something's wrong */
	  bufpos = next_bufp;
	  *class = Qnil;
	  *begin_p = 0;
	  *gly = Qnil;
	  if (g >= glyphs_in_run_limit)
	    {
	      run++;
	      glyphs_in_run = current_screen->face_list[line][run].length;
	      glyphs_in_run_limit = g + glyphs_in_run;
	    }
	  if (current_screen->face_list[line][run].type == font)
	    {
	      this_font
		= current_screen->face_list[line][run].faceptr->font;
	      if (!this_font)
		this_font = s->display.x->font;
	    }
	  
	  switch (current_screen->face_list[line][run].type)
	    {
	case font:
	{
	  int c;
	  int c_width;
	  this_font = current_screen->face_list[line][run].faceptr->font;
	  if (!this_font)
	    this_font = s->display.x->font;
	  c = current_screen->glyphs[line][g];
	  c_width = X_CHAR_WIDTH (this_font, c == TABGLYPH ? ' ' : c);
	  this_pix += c_width;
	  next_bufp = bufpos + 1;
	  bufcol++;

	  c = ((bufpos > 0)
	       ? BUF_CHAR_AT (buffer, bufpos)
	       : minibuf_prompt[minibuf_len + bufpos - 1]);
	  if (c >= 040 && c < 0177) /* Normal character. */
	    {
	    }
	  else if (c == '\t')	/* Tab character. */
	    {
	      register int j;

	      j = tab_width - ((((bufcol - 1) + tab_offset + hscroll - (hscroll > 0))
				% tab_width)) - 1;
	      g += j;
	      bufcol += j;
	      this_pix += j * X_CHAR_WIDTH (this_font, ' ');
	    }
	  else if (c < 0200 && ctl_arrow)
	    {
	      g++;
	      bufcol++;
	      this_pix += X_CHAR_WIDTH (this_font, c ^ 0100);
	    }
	  else
	    {
	      g += 3;
	      bufcol += 3;
	      this_pix += X_CHAR_WIDTH (this_font, ((c >> 6) + '0'));
	      this_pix += X_CHAR_WIDTH (this_font, ((7 & (c >> 3)) + '0'));
	      this_pix += X_CHAR_WIDTH (this_font, ((7 & c) + '0'));
	    }
	  break;
	}

	    case glyph:
	      /* bufcol++; */ /* Only chars count for tabs */
	    case column_glyph:
	      this_pix += current_screen->face_list[line][run].pix_length;
	      *class = current_screen->face_list[line][run].class;
	      *begin_p = current_screen->face_list[line][run].begin_p;
	      *gly = current_screen->glyphs[line][g];
	      break;
	      
	    case space:
	      this_pix += current_screen->face_list[line][run].pix_length;
	      next_bufp = bufpos + 1;
	      break;
	      
	    case window:
	      *w = current_screen->face_list[line][run].w;
	      tab_width = XBUFFER((*w)->buffer)->tab_width;
	      next_bufp = current_screen->face_list[line][run].bufp;
	      break;
	      
	    default:
	      break;
	    }
	}
      
      if (bufpos < 0)
	return 2;
      *bufp = bufpos;
      return 1;
    }
}

extern int x_mouse_x, x_mouse_y;

#endif /* HAVE_X_WINDOWS */

void report_file_error ();

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
	report_file_error ("Opening termscript", Fcons (file, Qnil));
    }
  return Qnil;
}


#ifdef SIGWINCH
extern int interrupt_input;
int fclose ();

void
window_change_signal ()
{
  int width, height;
  extern int errno;
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
      SCREEN_PTR s = XSCREEN (XCONS (tail)->car);
	
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


/* Change the screen height and/or width.  Values may be given as zero to
   indicate no change is to take place. */

void
change_screen_size (screen, newlength, newwidth, pretend)
     register SCREEN_PTR screen;
     register int newlength, newwidth, pretend;
{
  if (in_display)
    abort ();

  /* This size-change overrides any pending one for this screen.  */
  SCREEN_NEW_HEIGHT (screen) = 0;
  SCREEN_NEW_WIDTH (screen) = 0;

  if ((newlength == 0 || newlength == SCREEN_HEIGHT (screen))
      && (newwidth == 0 || newwidth == SCREEN_WIDTH (screen)))
    return;

  if (newlength && newlength != SCREEN_HEIGHT (screen))
    {
      if (XSCREEN (WINDOW_SCREEN (XWINDOW (SCREEN_MINIBUF_WINDOW (screen))))
	  == screen
	  && ! EQ (SCREEN_MINIBUF_WINDOW (screen),
		   SCREEN_ROOT_WINDOW (screen)))
	{
	  /* Screen has both root and minibuffer.  */
	  set_window_height (SCREEN_ROOT_WINDOW (screen),
			     newlength - 1, 0);
	  XFASTINT (XWINDOW (SCREEN_MINIBUF_WINDOW (screen))->top)
	    = newlength - 1;
	  set_window_height (SCREEN_MINIBUF_WINDOW (screen), 1, 0);
	}
      else
	/* Screen has just one top-level window.  */
	set_window_height (SCREEN_ROOT_WINDOW (screen), newlength, 0);
	
      if (SCREEN_IS_TERMCAP (screen) == output_termcap && !pretend)
	ScreenRows = newlength;

    }

  if (newwidth && newwidth != SCREEN_WIDTH (screen))
    {
      set_window_width (SCREEN_ROOT_WINDOW (screen), newwidth, 0);
      if (XSCREEN (WINDOW_SCREEN (XWINDOW (SCREEN_MINIBUF_WINDOW (screen))))
	  == screen)
	set_window_width (SCREEN_MINIBUF_WINDOW (screen), newwidth, 0);
      SCREEN_WIDTH (screen) = newwidth;

      if (SCREEN_IS_TERMCAP (screen) && !pretend)
	ScreenCols = newwidth;
    }

  if (newlength)
    SCREEN_HEIGHT (screen) = newlength;

  remake_screen_glyphs (screen);
  calculate_costs (screen);
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

void bitch_at_user ();

/* Lucid sound change */
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
    error ("Keyboard macro terminated by a command ringing the bell");
  else
    ring_bell (sound);
  fflush (stdout);
}


/* Lucid fix (so that .emacs can be loaded after screen is created) */
DEFUN ("initialize-first-screen", Finitialize_first_screen,
       Sinitialize_first_screen,
  0, 0, 0, "Make redisplay work on the first screen (do this early.)")
  ()
{
  /* defined in xdisp.c because it needs to use "static void" functions */
  initialize_first_screen ();
  return Qnil;
}


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
  if (!terminal_type)
    {
#ifdef VMS
      fprintf (stderr, "Please specify your terminal type.\n\
For types defined in VMS, use  set term /device=TYPE.\n\
For types not defined in VMS, use  define emacs_term \"TYPE\".\n\
\(The quotation marks are necessary since terminal types are lower case.)\n");
#else
      fprintf (stderr, "Please set the environment variable TERM; see tset(1).\n");
#endif
      exit (1);
    }

#ifdef VMS
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

  remake_screen_glyphs (selected_screen);

  /* X and Y coordiates of the cursor between updates. */
  SCREEN_CURSOR_X (selected_screen) = 0;
  SCREEN_CURSOR_Y (selected_screen) = 0;

#ifdef HAVE_X_WINDOWS
  if (!inhibit_window_system && egetenv ("DISPLAY"))
    {
      Vwindow_system = intern ("x");
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

syms_of_display ()
{
  defsubr (&Sopen_termscript);
  defsubr (&Sding);
  defsubr (&Ssend_string_to_terminal);
  defsubr (&Sinitialize_first_screen);

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
  Qcursor_in_echo_area = intern ("cursor-in-echo-area");
  staticpro (&Qcursor_in_echo_area);

#if 0 /* this isn't implemented yet, so there's no point in telling the user */
  DEFVAR_LISP ("glyph-table", &Vglyph_table,
    "Table defining how to output a glyph code to the screen.\n\
If not nil, this is a vector indexed by glyph code to define the glyph.\n\
Each element can be:\n\
 integer: a glyph code which this glyph is an alias for.\n\
 string: output this glyph using that string (not impl. in X windows).\n\
 nil: this glyph mod 256 is char code to output,\n\
    and this glyph / 256 is face code for X windows (see `x-set-face').");
#endif
  Vglyph_table = Qnil;

  DEFVAR_LISP ("standard-display-table", &Vstandard_display_table,
    "Display table to use for buffers that specify none.\n\
See `buffer-display-table' for more information.");
  Vstandard_display_table = Qnil;

  /* Initialize `window-system', unless init_display already decided it.  */
#ifdef CANNOT_DUMP
  if (noninteractive)
#endif
    Vwindow_system = Qnil;
}
