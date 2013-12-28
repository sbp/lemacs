/* Display generation from window structure and buffer text.
   Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.

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
#include <stdio.h>
#include <stdarg.h>

#include "lisp.h"
#include "window.h"
#include "termchar.h"
#include "buffer.h"
#include "extents.h"
#include "screen.h"
#include "indent.h"
#include "commands.h"
#include "disptab.h"

#include "dispmisc.h"
#include "dispextern.h"
#include "faces.h"
#include "termhooks.h"

#ifdef HAVE_X_WINDOWS
#include "xterm.h"
#include "xobjs.h"
#endif
#include "process.h"            /* for Fget_buffer_process, Fprocess_status */

#include "sysdep.h"

/* New display declarations. */
/* Pointers to (free) lists for char_block, line_header, and font
 * structures.
 */
struct char_block *block_free;
struct line_header *line_free;

/* Globals used during redisplay process to locate cursor at point */
static int point_vpos;
static int point_hpos;
static struct line_header *point_v;
static struct char_block *point_h;

/* We keep track of the selected window's vpos separately in order to
   be able to correctly update the modeline when line-number-mode is
   active and multiple windows are showing the current buffer. */
static int selected_point_vpos;

/* Nonzero means print newline before next minibuffer message.  */

int noninteractive_need_newline;

/* Used to keep screen title from changing rapidly and needlessly while
   the scrollbars are updated.
*/
int redisplay_lock;

/* face for use when laying out mode line. */
struct face *mode_face;

#define min(a, b) ((a) < (b) ? (a) : (b))
#define max(a, b) ((a) > (b) ? (a) : (b))

Lisp_Object Vglobal_mode_string;

/* Marker for where to display an arrow on top of the buffer text.  */
Lisp_Object Voverlay_arrow_position;

/* String to display for the arrow.  */
Lisp_Object Voverlay_arrow_string;

/* Values of those variables at last redisplay.  */
static Lisp_Object last_arrow_position;
static Lisp_Object last_arrow_string;

/* The number of lines to try scrolling a
  window by when point leaves the window; if
  it is <=0 then point is centered in the window */
int scroll_step;

/* Nonzero means truncate lines in all windows less wide than the screen */
int truncate_partial_width_windows;

/* Whether to display line numbers in the mode line. */
int line_number_mode;

/* Number of windows showing the buffer of the selected window.
   keyboard.c refers to this.  */
int buffer_shared;

/* non-nil if a buffer has changed since the last time redisplay completed */
int buffers_changed;

/* non-nil if any extent has changed since the last time redisplay completed */
int extents_changed;

/* non-nil if any face has changed since the last time redisplay completed */
int faces_changed;

/* non-nil if any window has changed since the last time redisplay completed */
int windows_changed;

/* Nonzero means display mode line highlighted */
int mode_line_inverse_video;

/* Value of echo_area_glyphs when it was last acted on.
  If this is nonzero, there is a message on the screen
  in the minibuffer and it should be erased as soon
  as it is no longer requested to appear. */
static char *previous_echo_glyphs;

/* Prompt to display in front of the minibuffer contents */
Lisp_Object Vminibuf_prompt;

/* Width in columns of current minibuffer prompt.  */
int minibuf_prompt_width;

/* Smallest number of characters before the gap
   at any time since last redisplay that finished.
   Valid for current buffer when try_window_id can be called.  */
int beg_unchanged;

/* Smallest number of characters after the gap
   at any time since last redisplay that finished.
   Valid for current buffer when try_window_id can be called.  */
int end_unchanged;

/* MODIFF as of last redisplay that finished;
   if it matches MODIFF, beg_unchanged and end_unchanged
   contain no useful information */
int unchanged_modified;

/* Message to display instead of minibuffer contents
   This is what the functions error and message make,
   and command echoing uses it as well.
   It overrides the Vminibuf_prompt as well as the buffer.  */
char *echo_area_glyphs;

/* Nonzero if head_clip or tail_clip of current buffer has changed
   since last redisplay that finished */
int clip_changed;

/* Nonzero if try_window_id has made blank lines at window bottom
 since the last redisplay that paused */
static int blank_end_of_window;

/* The buffer position of the first character appearing
 entirely or partially on the current screen line.
 Or zero, which disables the optimization for the current screen line. */
static int this_line_bufpos;

/* Number of characters past the end of this line,
   including the terminating newline */
static int this_line_endpos;

/* The vertical position of this screen line. */
static int this_line_vpos;

static struct line_header *this_line_line;

/* Hpos value for start of display on this screen line.
   Usually zero, but negative if first character really began
   on previous line */
static int this_line_start_hpos;

/* Buffer that this_line variables are describing. */
static struct buffer *this_line_buffer;

/* flag if a subwindow is being displayed somewhere */
int subwindows_being_displayed;

/* flag if eob has already been layed out.  Used by try_layout.  Don't ask */
int eob_hack_flag;

/* END OF STILL IN USE VARS */




/* Nonzero if overlay arrow has been displayed once in this window.  */
/* static int overlay_arrow_seen; */

/* display_text_line sets these to the screen position (origin 0) of point,
   whether the window is selected or not.
   Set one to -1 first to determine whether point was found afterwards.  */

/* static int cursor_vpos; */
/* static int cursor_hpos; */

static void mark_window_display_accurate (Lisp_Object window, int flag);
static struct char_block *layout_string_inc (struct window *, 
                                             CONST unsigned char *string, 
                                             int hpos, GLYPH truncate,
                                             int mincol, int maxcol,
                                             struct line_header *, 
                                             struct face *, 
                                             int *ret, int clip,
					     int modeline);
static struct position *layout_text_line (struct window *, 
                                          int vpos, 
                                          int start, 
                                          int hpos, 
                                          int taboffset, 
                                          int *propagation,
					  int *glyph_prop,
                                          struct line_header *);
static int layout_margin_line (struct window *w, 
			       struct line_header *l, 
			       int glyph_count);
static int layout_mode_element (struct window *,
                                int hpos, int depth, int minendcol,
                                int maxendcol, Lisp_Object elt,
                                struct line_header *l, int modeline);

static int line_pool (int pool_size);
static int block_pool (int pool_size);
static void layout_echo_area_contents (void);

static void mark_all_windows_display_accurate (int flag);
static int layout_window (Lisp_Object, int);
static int layout_windows (Lisp_Object);
static int try_layout_id (Lisp_Object);
static void try_layout (Lisp_Object, register int);
static void layout_mode_line (struct window *);
static int redisplay_screen (struct screen *, int);
static unsigned char *decode_mode_spec (struct window *, register char,
					register int);
static int screen_title_display_string (struct window *w,
					 unsigned char *string,
					 int hpos, int mincol, int maxcol);
static char *window_line_number (struct window *);

#ifdef UNUSED /* >>> A great idea, but unimplemented */
/* Empty buffer made current in the minibuffer when echo display is on */
static Lisp_Object echo_area_buffer;
#endif

/* true iff we should redraw the mode lines on the next redisplay */
int redraw_mode_line;

extern struct char_block old_cur_char;
extern struct line_header old_cur_line;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Pending redisplay blocks for a given window.  Stack should be empty at
 * beginning and end of redisplay for window.
 */
struct redisplay_block display_list[300];
int list_ptr;

#define DECREMENT_LIST_PTR \
  if (BLOCK_LINE(list_ptr) == l) list_ptr--;\
  if (BLOCK_LINE(list_ptr) == l) list_ptr--;


/*
 * New functions and declarations for merged redisplay.
 */
 
static struct buffer *last_buffer;
static struct screen *last_screen;
static int last_buffer_modiff;
static int last_buffer_facechange;
static EXTENT_FRAGMENT last_buffer_extfrag;
static int last_extfrag_from_pos, last_extfrag_to_pos;
static struct face *last_extfrag_face;
 
static void
update_cache (struct buffer *buffer, EXTENT_FRAGMENT extfrag)
{
  last_extfrag_from_pos = extfrag->from;
  last_extfrag_to_pos = extfrag->to;
  last_buffer_extfrag = extfrag;
}

/*
 * This includes 0-width extents in the merged face.
 */
struct face *
get_face_at_bufpos (struct screen *screen, struct buffer *buffer, int pos)
{
  EXTENT_FRAGMENT extfrag;
  struct face *fp;

  /*
   * In most cases we don't want 0-width extents faces merged in.  So
   * we have to make sure, both before and after, that the cache is
   * not used.
   */
  extent_cache_invalid = 1;
  extfrag = buffer_extent_fragment_at (pos, buffer, screen, 1);
  extent_cache_invalid = 1;

  if (!extfrag->fp)
    return &SCREEN_NORMAL_FACE (screen);
  else
    return extfrag->fp;
}


/* START NEW REDISPLAY FUNCTIONS */

#ifdef ALLOC_NO_POOLS
#define LINE_POOL_SIZE 1
#else
#define LINE_POOL_SIZE 100
#endif

/*
 * Allocate a pool of line header blocks
 */
static int
line_pool (int pool_size)
{
  struct line_header *t;
  int i;

  t = (struct line_header *)
    xmalloc(pool_size * sizeof(struct line_header));

  if (!t)
    return 1;

  memset (t, 0, pool_size * sizeof(struct line_header));

  /*
   * Now, add these new blocks to the start of the free list
   * don't worry about prev pointers.
   */
  for (i = 0; i < pool_size; i++)
    {
      t->next = line_free;
      line_free = t;
      t++;
    }
  return 0;
}


/*
 * Return a new line header block
 */
struct line_header *
get_line ()
{
  struct line_header *l;
  
  if (!line_free && line_pool (LINE_POOL_SIZE))
    fprintf(stderr,GETTEXT ("Couldn't allocate space")), abort ();

  l = line_free;
  line_free = line_free->next;
  l->new = 1;
  l->tabs = 0;
  l->next = l->prev = 0;

  l->body = get_char_block ();
  l->end = l->body;
  l->body->ch = 0;
  l->body->glyph = -1;		/* 0 is a valid glyph */
  l->body->char_b = TRUE;
  l->body->blank = FALSE;
  l->body->new = 0;

  l->margin_start = get_char_block ();
  l->margin_end = l->margin_start;
  l->margin_start->ch = 0;
  l->margin_start->glyph = -1;
  l->margin_start->char_b = FALSE;
  l->margin_start->blank = FALSE;
  l->margin_start->new = 0;

  return l;
}


/*
 * Either return the next line_header for this window, or return a new
 * one.
 */
static struct line_header *
next_line (struct line_header *l)
{
  struct line_header *n;
  if (!l->next)
    {
      n = get_line();
      l->next = n;
      n->prev = l;
      return n;
    }
  return l->next;
}


#ifdef ALLOC_NO_POOLS
#define BLOCK_POOL_SIZE 1
#else
#define BLOCK_POOL_SIZE 1000
#endif

/*
 * Allocate a pool of character block structures
 */
static int
block_pool (int pool_size)
{
  struct char_block *t;
  int i;

  t = (struct char_block *) xmalloc(pool_size * sizeof(struct char_block));
  if (!t) return 1;

  memset(t, 0, pool_size * sizeof(struct char_block));
  
  /* Now, add these new blocks to the start of the free list
   * don't worry about prev pointers.
   */
  for (i = 0; i < pool_size; i++)
    {
      t->next = block_free;
      block_free = t;
      t++;
    }
  return 0;
}


/*
 * Return/allocate a char_block structure
 */
struct char_block *
get_char_block ()
{
  struct char_block *f;
  
  if (!block_free && block_pool (BLOCK_POOL_SIZE))
    fprintf(stderr,GETTEXT ("Couldn't allocate space")), abort ();

  f = block_free;
  block_free = block_free->next;
  memset(f,0,sizeof (struct char_block));
  f->new = 1;
  return f;
}


/*
 * Either return the next char_block on this line (not sentinel), or 
 * a new one.
 */
static struct char_block *
next_block (struct char_block *c, struct line_header *l)
{
  struct char_block *d;
  
  if (!c->next || c->ch == 0)
    {
      /* Insert new block prior to end of line dummy */
      d = get_char_block ();
      if (l->body == c) l->body = d;
      if (c->prev) c->prev->next = d;
      d->prev = c->prev;
      d->next = c;
      c->prev = d;
      return d;
    }
  /* node is already next in list, so return it */
  return c;
}

/*
 * Same as next_block except for margin glyphs.
 */
static struct char_block *
next_margin_block (struct char_block *c, struct line_header *l)
{
  struct char_block *d;
  
  if (!c->next || c->ch == 0)
    {
      /* Insert new block prior to end of line dummy */
      d = get_char_block ();
      if (l->margin_start == c) l->margin_start = d;
      if (c->prev) c->prev->next = d;
      d->prev = c->prev;
      d->next = c;
      c->prev = d;
      return d;
    }
  /* node is already next in list, so return it */
  return c;
}

/*
 * Return a bunch of char_block structures to the free list
 */
void
free_char_blocks (struct char_block *b, struct char_block *e)
{
#ifdef ALLOC_NO_POOLS
  struct char_block *mb = b;
  while (mb != e)
    {
      struct char_block *nb = mb->next;
      xfree (mb);
      mb = nb;
    }
  xfree (e);
#else
  e->next = block_free;
  block_free = b;
#endif
}


/*
 * Return a line of char_block structures to the free list and free the
 * line_header structure itself.
 */
void
free_line (struct line_header *l)
{
  if (l->end)
    free_char_blocks (l->body, l->end);
  if (l->margin_end)
    free_char_blocks (l->margin_start, l->margin_end);

#ifdef ALLOC_NO_POOLS
  xfree (l);
#else
  memset(l,0,sizeof(struct line_header));
  l->next = line_free;
  line_free = l;
#endif
}


static void
free_lines (struct line_header *l)
{
  struct line_header *p;

  p = l;
  while (l)
    {
      l = l->next;
      free_line(p);
      p = l;
    }
}


/*
 * Layout a string for display in the minibuffer/echo area.
 */
static void
layout_echo_area_contents ()
{
  struct screen *cur_s;
  struct window *w;
  struct line_header *l;
  struct char_block *changed;
  int hpos = 0,lwidth;

  cur_s = ((SCREENP (Vglobal_minibuffer_screen))
	  ? XSCREEN (Vglobal_minibuffer_screen) 
	  : selected_screen);
  w = XWINDOW (cur_s->minibuffer_window);

  l = window_display_lines (w);
  if (!l)
    {
      l = get_line();
      find_window_mirror(w)->lines = l;
      cur_s->cursor_x = cur_s->cursor_y = 0;
      selected_point_vpos = 0;
      cur_s->cur_mir = cur_s->new_cur_mir = find_window_mirror (w);
      cur_s->cur_char = cur_s->new_cur_char = l->body;
      cur_s->cur_line = cur_s->new_cur_line = l;
    }

  if (screen_garbaged)
    {
      Fredraw_display();
      screen_garbaged = 0;
    }

  if (echo_area_glyphs || minibuf_level == 0)
    {
      int new_ypos = 0;

      lwidth = l->lwidth;
      changed = layout_string_inc (w, ((echo_area_glyphs)
                                       ? (unsigned char *) echo_area_glyphs
				       : (unsigned char *) ""),
                                   0, 0, 0, PIXW (cur_s),
                                   l, 0, &hpos, 1, 0);
      l = window_display_lines (w);
      l->prevy = l->ypos;	/* Prev value */
      if (l->ypos != w->pixtop + l->ascent)
	new_ypos = 1;
      l->ypos = w->pixtop + l->ascent;
      /*
       * Make a redisplay block corresponding to this line in echo area
       */
      if (l->changed)
	{
	  list_ptr = 0;		/* First (only) entry */
	  BLOCK_TYPE(list_ptr) = BODY_LINE;
	  BLOCK_LINE(list_ptr) = l;
	  if (new_ypos)
	    BLOCK_LINE_START(list_ptr) = l->body;
	  else
	    BLOCK_LINE_START(list_ptr) = changed;
	  BLOCK_LINE_END(list_ptr) = 0;
	  BLOCK_LINE_CLEAR(list_ptr) = (lwidth > l->lwidth);
	  /* Do the update */
	  update_window (w);
	}

      if (!SCREENP(Vglobal_minibuffer_screen))
	{
	  /* Multiple minibuffers; must ensure coherency between them. */
	  Lisp_Object rest;
	  for (rest = Vscreen_list ; !NILP (rest) ; rest = XCONS(rest)->cdr)
	    {
	      struct screen *s;

	      if (!SCREENP (XCONS(rest)->car))
		abort ();
	      s = XSCREEN (XCONS (rest)->car);

	      if (s != selected_screen)
		{
		  struct line_header *tmpl;

		  w = XWINDOW(s->minibuffer_window);
		  tmpl = window_display_lines (w);
		  if (!tmpl)
		    {
		      tmpl = get_line();
		      find_window_mirror(w)->lines = tmpl;
		      s->cursor_x = s->cursor_y = 0;
		      selected_point_vpos = 0;
		      s->new_cur_mir = s->new_cur_mir = find_window_mirror (w);
		      s->cur_char = s->new_cur_char = tmpl->body;
		      s->cur_line = s->new_cur_line = tmpl;
		    }
		  if (NILP(Vsynchronize_minibuffers))
		    {
		      if (tmpl->body != tmpl->end)
			{
			  free_char_blocks (tmpl->body, tmpl->end->prev);
			  tmpl->end->xpos = w->pixleft
			    + LEFT_MARGIN (XBUFFER (w->buffer), s, w);
			  tmpl->body = tmpl->end;
			  tmpl->body->next = tmpl->body->prev = 0;
			  tmpl->chars = 0;
			  tmpl->changed = 1;
			  changed = tmpl->body;
			}
		    }
		  else
		    {
		      lwidth = tmpl->lwidth;
		      changed = layout_string_inc (w,
						   ((echo_area_glyphs)
						    ? (unsigned char *) echo_area_glyphs
						    : (unsigned char *) ""),
                                                   0, 0, 0, PIXW (s),
                                                   tmpl, 0, &hpos, 1, 0);
		      tmpl = window_display_lines (w);
		    }
		  tmpl->prevy = tmpl->ypos; /* Prev value */
		  tmpl->ypos = w->pixtop + tmpl->ascent;
		  /*
		   * Make a redisplay block corresponding to this line in echo
		   * area
		   */
		  if (tmpl->changed)
		    {
		      list_ptr = 0;	/* First (only) entry */
		      BLOCK_TYPE(list_ptr) = BODY_LINE;
		      BLOCK_LINE(list_ptr) = tmpl;
		      BLOCK_LINE_START(list_ptr) = changed;
		      BLOCK_LINE_END(list_ptr) = 0;
		      BLOCK_LINE_CLEAR(list_ptr) = (lwidth > tmpl->lwidth);
		      /* Do the update */
		      update_window (w);
		    }
		}
	    }
	}
    }
  /*
   * If we are in distinct minibuffer screen, minibuffer text is always
   * on line 1.  Otherwise, its on the last line of the screen.
   */
  if ((!SCREENP(Vglobal_minibuffer_screen) && cur_s->cursor_y == cur_s->height)
      || (SCREENP(Vglobal_minibuffer_screen) && cur_s->cursor_y == 1)
      || cursor_in_echo_area
      || !(cur_s->cur_mir && cur_s->new_cur_mir))
    {
      if (cursor_in_echo_area)
	{
	  cur_s->cursor_y = 0;
	  selected_point_vpos = 0;
	}
      cur_s->cursor_x = hpos;
      cur_s->new_cur_mir =
	find_window_mirror (XWINDOW (cur_s->minibuffer_window));
      cur_s->new_cur_char = l->end;
      cur_s->new_cur_line = l;

      if (!cursor_in_echo_area)
	{
	  cur_s->cur_mir = cur_s->new_cur_mir;
	  cur_s->cur_char = cur_s->new_cur_char;
	  cur_s->cur_line = cur_s->new_cur_line;
	}
    }

  if (EQ(minibuf_window,selected_window))
    this_line_bufpos = 0;

  previous_echo_glyphs = echo_area_glyphs;
}


/*
 * Synchronize contents of minibuffer for 
 */
static void
sync_minibuffer (Lisp_Object window)
{
  register struct window *w = XWINDOW(window);
  register struct window *m = XWINDOW(minibuf_window);

  w->buffer = m->buffer;
  w->start = m->start;
  XSETINT(w->last_modified,0); /* m->last_modified; */
  w->hscroll = m->hscroll;
  w->pointm = m->pointm;
  w->force_start = m->force_start;
  w->last_point = m->last_point;
  w->last_point_x = m->last_point_x;
  w->last_point_y = m->last_point_y;
  w->last_mark_x = m->last_mark_x;
  w->last_mark_y = m->last_mark_y;
  w->window_end_pos = m->window_end_pos;
  w->window_end_valid = m->window_end_valid;
  w->window_end_ppos = m->window_end_ppos;
}

/* If you are wondering why all this crap is necessary for subwindows
   it is because they break a big assumption of redisplay: if you draw
   over something it goes away.  Not so with subwindows.  They have to
   be unmapped.  But redisplay wasn't designed with keeping track of
   when something actually goes away in mind. */

/*
 * Mark all subwindows being displayed in the given window as being
 * displayed.  This will keep them from getting erroneously
 * unmapped.
 */
static int
mark_subwindows_on_window (struct window *w, int mark)
{
  struct line_header *l;
  struct char_block *cb;
  int cnt = 0;

  l = window_display_lines (w);
  while (l)
    {
      if (l->subwindow)
	{
	  cb = l->body;
	  while (cb)
	    {
	      if (!cb->char_b && SUBWINDOWP (glyph_to_pixmap (cb->glyph)))
		{
		  XSUBWINDOW (glyph_to_pixmap (cb->glyph))->being_displayed
		    = mark;
		  cnt++;
		}
	      cb = cb->next;
	    }
	  cb = l->margin_start;
	  while (cb)
	    {
	      if (!cb->char_b && SUBWINDOWP (glyph_to_pixmap (cb->glyph)))
		{
		  XSUBWINDOW (glyph_to_pixmap (cb->glyph))->being_displayed
		    = mark;
		  cnt++;
		}
	      cb = cb->next;
	    }
	}
      l = l->next;
    }
  if (!cnt)
    find_window_mirror (w)->subwindows_being_displayed = 0;

  return (cnt ? 1 : 0);
}

/*
 * Mark any subwindows on the given window and any of that windows
 * children.
 */
static int
update_screen_subwindows_traversal (Lisp_Object window, int mark)
{
  int cnt = 0;

  for (; !NILP (window); window = XWINDOW (window)->next)
    {
      if (!NILP (XWINDOW (window)->vchild))
	cnt +=
	  update_screen_subwindows_traversal (XWINDOW (window)->vchild, mark);
      else if (!NILP (XWINDOW (window)->hchild))
	cnt +=
	  update_screen_subwindows_traversal (XWINDOW (window)->hchild, mark);
      else if (find_window_mirror (XWINDOW (window))->subwindows_being_displayed)
	cnt += mark_subwindows_on_window (XWINDOW (window), mark);
    }

  return cnt;
}
      
/*
 * Check all windows on the given screen to see if they have been
 * displaying subwindows and if so, unmap any no longer being
 * displayed.
 */
static int
update_screen_subwindows (struct screen *s)
{
  mark_subwindow_display_status (s, 0);
  s->subwindows_being_displayed =
    update_screen_subwindows_traversal (s->root_window, 1);
  unmap_unmarked_subwindows_of_screen (s);

  return s->subwindows_being_displayed;
}

/*
 * Perform a redisplay of all screens.  This function and
 * redisplay_preserving_echo_area() are main entry points to redisplay
 * procedure. */
void
redisplay_1 (int force)
{
  struct window *w = XWINDOW(selected_window);  
  struct screen *s = XSCREEN(w->screen);
  register int pause = 0;
  int all_windows;
  register int tlbufpos, tlendpos;

  /* We use this to control calling update_scrollbars because doing so
     is expensive.  This variable is not perfect.  It is guaranteed
     that if it is 0, then nothing happened.  However it is quite
     possible that it is 1 and nothing happened either.  Always err
     on the side of caution when it comes to redisplay. */
  int did_something = 1;

  /*
   * scrollbars are in use.  Some data structures that the scrollbar
   * code depends on to function properly were not getting updated
   * otherwise.  This does not have any noticeable impact on the
   * scrollbar update time. */
  if (noninteractive || (detect_input_pending(1) && !redisplay_lock && !force))
    return;

  /* I don't understand the purpose of `redisplay_lock' but I'll
     assume it's there for a reason. --Ben */

  /* The reason that we do this first is that updating the menubar
     visibility can conceivably cause some or all of the text
     widget to get blanked.  Doing it first avoids excess
     flashing.  If it turns out that the contents of the menubar
     (or psheets, etc.) have some dependency on the redisplay
     information computed below, then we need to split up the
     visibility and contents updating like what was done for
     scrollbars. --Ben */
  if (!redisplay_lock)
    update_menubars ();

  /* If additional Expose events were generated as a result of any
     widget changing done above, then bail out now.  redisplay()
     will be called again soon. */
  if (detect_input_pending(1) && !redisplay_lock && !force)
    return;

  /* Just because selected screen is not visible doesn't mean we're done. */
#if 0
  if (!s->visible) return;
#endif

  hold_window_change();  
  if (screen_garbaged)
    {
      Fredraw_display();
      screen_garbaged = 0;
    }

  if (echo_area_glyphs || previous_echo_glyphs)
    {
      layout_echo_area_contents();
    }

  if (clip_changed || buffers_changed || extents_changed
      || faces_changed || windows_changed)
    redraw_mode_line++;

  if (XINT (w->last_modified) < MODIFF
      && XINT (w->last_modified) <= current_buffer->save_modified)
    {
      w->redo_mode_line = 1;
      if (buffer_shared > 1)
	redraw_mode_line++;
    }

  all_windows = redraw_mode_line || buffer_shared > 1;

  /* If specs on arrow have changed, do thorough redisplay
   * to ensure no arrows exist at prev position. */
  if (!EQ(Voverlay_arrow_position,last_arrow_position)
      || !EQ(Voverlay_arrow_string,last_arrow_string))
    all_windows = 1, clip_changed = 1;

  /* The cursor code sucks.  It can get confused about where the
     cursor is and where it is supposed to be.  So we put an extra
     check in here to help it out. */
  {
    struct window_mirror *mini_mirror =
      find_window_mirror (XWINDOW (s->minibuffer_window));

    if ((!cursor_in_echo_area && s->cur_mir == mini_mirror)
	|| (cursor_in_echo_area && s->cur_mir != mini_mirror))
      all_windows = 1;
  }

  tlbufpos = this_line_bufpos;
  tlendpos = this_line_endpos;
  if (!all_windows && tlbufpos > 0 && !w->redo_mode_line
      && this_line_buffer == current_buffer
      && current_buffer == XBUFFER(w->buffer)
      && !w->force_start
      && PT >= tlbufpos
      && PT <= Z - tlendpos
      && (XINT(w->last_modified) >= MODIFF
	  || (beg_unchanged >= tlbufpos - 1
	      && GPT >= tlbufpos
	      && ((FIXNUMP (current_buffer->selective_display)
		   && XINT (current_buffer->selective_display) > 0
		   ? (beg_unchanged >= tlbufpos
		      && GPT > tlbufpos)
		   : 1))
	      && end_unchanged >= tlendpos
	      && Z - GPT >= tlendpos)))
    {
      if (tlbufpos > BEGV && CHAR_AT(tlbufpos - 1) != '\n'
	  && (tlbufpos == ZV
	      || CHAR_AT(tlbufpos) == '\n'))
	{
	  goto cancel;
	}
	  
      if (XINT(w->last_modified) < MODIFF
	  || EQ(selected_window,minibuf_window))
	{
	  int fail = 0;
	  int glyph_prop = 0;
	  short old_ascent, old_descent;

	  old_cur_char.ch = s->cur_char->ch;
	  old_cur_char.glyph = s->cur_char->glyph;
	  old_cur_char.char_b = s->cur_char->char_b;
	  old_cur_char.blank = s->cur_char->blank;
	  old_cur_char.xpos = s->cur_char->xpos;
	  old_cur_char.face = s->cur_char->face;

	  old_cur_line.chars = s->cur_line->chars;
	  old_cur_line.ypos = s->cur_line->ypos;
	  old_cur_line.ascent = s->cur_line->ascent;
	  old_cur_line.descent = s->cur_line->descent; 	  

	  point_vpos = -1;
	  list_ptr = -1;

	  old_ascent = this_line_line->ascent;
	  old_descent = this_line_line->descent;

	  layout_text_line(w,this_line_vpos,tlbufpos,this_line_start_hpos,
			   pos_tab_offset(w,tlbufpos),
			   &fail,&glyph_prop,this_line_line);
	  eob_hack_flag = 0;

	  /*
	   * If propagating anything forward, fail here and redo entire
	   * window
	   */
	  if (fail)
	    {
	      struct char_block *cb = this_line_line->end->prev;
	      int count;

	      this_line_line->ascent = old_ascent;
	      this_line_line->descent = old_descent;

	      for (count = 0 ; count <= 1; count++)
		{
		  if ((count == 0 && list_ptr == 0)
		      || (count == 1
			  && (list_ptr == 1 && BLOCK_TYPE(1) == MARGIN_LINE)))
		    {
		      if (BLOCK_LINE_START(count))
			{
			  BLOCK_LINE_START(count)->ch = 0;
			  /* Let's REALLY make sure the character is "blotted"
			     out. */
			  BLOCK_LINE_START(count)->glyph = -1;
			  BLOCK_LINE_START(count)->face = 0;
			}
		      if (BLOCK_LINE_END(count))
			{
			  BLOCK_LINE_END(count)->ch = 0;
			  BLOCK_LINE_END(count)->glyph = -1;
			  BLOCK_LINE_END(count)->face = 0;
			}
		    }
		}
	      if (cb) cb->ch = 0;

	      s->cur_char = &old_cur_char;
	      s->cur_line = &old_cur_line;

	      goto cancel;
	    }

	  if (point_vpos >= 0 && this_line_bufpos
	      && this_line_endpos == tlendpos)
	    {
	      if (interrupt_input)
		unrequest_sigio();
	      stop_polling();

	      update_window(w);

	      if (interrupt_input)
		request_sigio();
	      start_polling();

	      goto finish;
	    }
	  else
	    {
	      struct char_block *cb = this_line_line->end->prev;
	      int count;

	      /* UGLY:  Here we will plot the \ or $ at end of line, and
	       * lose incremental display capabilities when we try laying
	       * out the line again.  "Blot" out the changed characters
	       * so they will get plotted.
	       */
	      
	      for (count = 0 ; count <= 1; count++)
		{
		  if ((count == 0 && list_ptr == 0)
		      || (count == 1
			  && (list_ptr == 1 && BLOCK_TYPE(1) == MARGIN_LINE)))
		    {
		      if (BLOCK_LINE_START(count))
			{
			  BLOCK_LINE_START(count)->ch = 0;
			  /* Let's REALLY make sure the character is "blotted"
			     out. */
			  BLOCK_LINE_START(count)->glyph = -1;
			  BLOCK_LINE_START(count)->face = 0;
			}
		      if (BLOCK_LINE_END(count))
			{
			  BLOCK_LINE_END(count)->ch = 0;
			  BLOCK_LINE_END(count)->glyph = -1;
			  BLOCK_LINE_END(count)->face = 0;
			}
		    }
		}
	      if (cb) cb->ch = 0;

	      s->cur_char = &old_cur_char;
	      s->cur_line = &old_cur_line;
	      
	      goto cancel;
	    }
	}
      else if (PT == XINT(w->last_point))
	{
	  if (echo_area_glyphs || previous_echo_glyphs)
	    {
	      did_something = 0;
	      goto finish;
	    }
	  unhold_window_change ();
	  return;
	}
      else
	{
	  struct position pos;
	  
	  pos = *compute_motion(w,tlbufpos,0,
				XINT (w->hscroll) ? 1 - XINT(w->hscroll) : 0,
				PT, 2, -(1 << (SHORTBITS - 1)),
				XINT(w->hscroll),
				pos_tab_offset(w,tlbufpos));


	  if (pos.vpos < 1)
	    {
	      struct char_block *cb;
	      int i;
	      
	      s->cursor_x = max (0,pos.hpos);
	      s->cursor_y = this_line_vpos;
	      selected_point_vpos = this_line_vpos;
	      
	      s->new_cur_line = this_line_line;

	      cb = s->new_cur_line->body;
	      i = s->cursor_x + window_needs_vertical_divider (w);
	      if (!i)
		{
		  while (cb && (cb->char_b == FALSE))
		    cb = cb->next;
		}
	      else
		{
		  while (cb && i--)
		    {
		      while (cb && (cb->char_b == FALSE))
			cb = cb->next;
		      cb = cb->next;
		      while (cb && (cb->char_b == FALSE))
			cb = cb->next;
		    }
		}
	      s->new_cur_char = cb;
	      s->new_cur_mir = find_window_mirror (w);

	      if (interrupt_input)
		unrequest_sigio();
	      stop_polling();

	      update_window(w);

	      if (interrupt_input)
		request_sigio();
	      start_polling();

	      goto finish;
	    }
	  else
	    {
	      goto cancel;
	    }
	}

    }
 cancel:
  
  this_line_bufpos = 0;
      
  /*
   * If in a distinct screen, redisplay minibuffer everytime we haven't
   * done so above.
   */
  if (SCREENP(Vglobal_minibuffer_screen) && echo_area_glyphs == 0
      && previous_echo_glyphs == 0)
    {
      /* Note:  bang out minibuffer contents regardless */
      did_something = redisplay_screen (XSCREEN(Vglobal_minibuffer_screen),1);
    }

  if (all_windows)
    {
      Lisp_Object cur_s, rest;

      XSETR (cur_s, Lisp_Screen, s);
      /* Do all windows.
       * Update current edit screen (if not already done
       * by updating minibuffer)
       */
      buffer_shared = 0;
      if (!EQ(cur_s,Vglobal_minibuffer_screen))
	{
	  did_something = redisplay_screen (s,1);
	}
      for (rest = Vscreen_list ; !NILP (rest) ; rest = XCONS(rest)->cdr)
	{
	  cur_s = XCONS(rest)->car;
	  if (!SCREENP (cur_s))
	    abort ();
	  if (pause)
	    break;

	  if ((!EQ(cur_s,w->screen))
	      && (!EQ(cur_s,Vglobal_minibuffer_screen))
	      && XSCREEN (cur_s)->visible)
	    {
	      int tmp;

	      tmp = redisplay_screen (XSCREEN(cur_s),force);
	      if (tmp == 2)
		pause = 1;
	      else
		did_something = tmp;
	    }
	}
    }
  else
    {
      /* Only do selected window */
      if (s != XSCREEN(Vglobal_minibuffer_screen))
	{
	  did_something = layout_window (selected_window,1);
	}
    }

 finish:
  if (!pause)
    {
      struct buffer *b = XBUFFER(w->buffer);
/*      current_buffer->force_redisplay = Qnil; */
      clip_changed = 0;
      blank_end_of_window = 0;
      unchanged_modified = BUF_MODIFF (b);
      beg_unchanged = BUF_GPT (b) - BUF_BEG (b);
      end_unchanged = BUF_Z (b) - BUF_GPT (b);

      w->last_point = make_number (BUF_PT (b));
      w->last_point_x = make_number (s->cursor_x);
      w->last_point_y = make_number (s->cursor_y);
      if (all_windows)
	{
	  mark_all_windows_display_accurate(1);
	}
      else
        {
	  w->redo_mode_line = 0;
	  w->last_modified = make_number (BUF_MODIFF(b));
	  w->window_end_valid = 1;
	  last_arrow_position = Voverlay_arrow_position;
	  last_arrow_string = Voverlay_arrow_string;
	}
#ifdef HAVE_X_WINDOWS
      if (redraw_mode_line || subwindows_being_displayed)
        {
	  Lisp_Object tail;
	  int cnt = 0;

          for (tail = Vscreen_list; CONSP (tail); tail = XCONS (tail)->cdr)
            {
              struct screen *s;
              if (!SCREENP (XCONS (tail)->car))
                continue;
              s = XSCREEN (XCONS (tail)->car);

              if (redraw_mode_line && SCREEN_IS_X (s) && !redisplay_lock)
                x_format_screen_title (s);

	      if (subwindows_being_displayed && s->subwindows_being_displayed)
		cnt += update_screen_subwindows (s);
            }

	  if (!cnt)
	    subwindows_being_displayed = 0;
        }
#endif /* HAVE_X_WINDOWS */

      redraw_mode_line = 0;
      buffers_changed = 0;
      extents_changed = 0;
      faces_changed = 0;
      windows_changed = 0;
    }
  else
    {
      /* Ugly:  we got preempted doing extra screens.  buffer_shared value is
       * invalid; increment it so that we'll try to get through all windows
       * again next time.
       */
      this_line_bufpos = 0;
      buffer_shared++;
    }

  if (screen_garbaged)
    {
      XSync (x_current_display, 0);
      redisplay ();
    }

  unhold_window_change();

  /* Now finally update the scrollbars to reflect the information
     just computed.  Do this when no longer "in_display". */
  if (did_something)
    update_scrollbars ();
}

void
redisplay ()
{
  redisplay_1 (0);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
void
redisplay_preserving_echo_area ()
{
  if (echo_area_glyphs == 0 && previous_echo_glyphs != 0)
    {
      if (*previous_echo_glyphs != 0)
	echo_area_glyphs = previous_echo_glyphs;
      redisplay ();
      echo_area_glyphs = 0;
    }
  else
    redisplay ();
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* FORCE nonzero means do not stop for pending input
 *
 * returns 0 if redisplay didn't do anything
 *	   1 if redisplay did do something
 *	   2 if redisplay was preempted by pending input
 */
static int
redisplay_screen (struct screen *s, int force)
{
  int input_pending = detect_input_pending(1);
  int retval;

  if (input_pending && !force)
    {
      display_completed = 0;
      return 2;
    }
  
  RW_FIXUP(s);
  retval = layout_windows (s->root_window);

  if (termscript)
    fflush(termscript);
  if (inhibit_window_system)
    fflush(stdout);

  mark_window_display_accurate(s->root_window,1);
  s->garbaged = 0;
  s->replot_lines = 0;
  return retval;
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
void
clear_display_structs (struct window_mirror *mir)
{
  struct line_header *l,*p;
  struct screen *s = mir->screen;

  if (s->cur_mir == mir)
    s->cur_mir = 0;
  if (s->new_cur_mir == mir)
    s->new_cur_mir = 0;
  if (mir->lines)
    {
      l = mir->lines;
      while (l)
	{
	  p = l;
	  if (s->cur_line == l)
	    { s->cur_line = 0; s->cur_char = 0; }
	  if (s->new_cur_line == l)
	    { s->new_cur_line = 0; s->new_cur_char = 0;}
	  l = l->next;
	  free_line(p);
	}
    }

  mir->lines = NULL;
  if (mir->modeline)
    {
      l = mir->modeline;
      if (l)
	{
	  free_line (l);
	}
    }
  mir->modeline = NULL;
  return;
}

void
free_display_structs (struct window_mirror *mir)
{
  for (; mir; mir = mir->next)
    {
      if (mir->hchild) free_display_structs (mir->hchild);
      if (mir->vchild) free_display_structs (mir->vchild);
      clear_display_structs (mir);
    }
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
static void
mark_all_windows_display_accurate (int flag)
{
  register Lisp_Object pw;
  register Lisp_Object rest;
  register struct screen *s;

  for (rest = Vscreen_list; !NILP(rest); rest = XCONS(rest)->cdr)
    {
      s = XSCREEN (XCONS (rest)->car);
      while (!EQ((pw = XWINDOW(s->root_window)->parent),s->root_window) && !NILP(pw))
	s->root_window = pw;
      mark_window_display_accurate(s->root_window,flag);
    }
}

static void
mark_window_display_accurate (Lisp_Object window, int flag)
{
  register struct window *w;
  
  for (; !NILP(window); window = w->next)
    {
      w = XWINDOW(window);

      if (!NILP(w->buffer))
	w->last_modified
	  = !flag ? Qzero : make_number (BUF_MODIFF(XBUFFER(w->buffer)));
      w->window_end_valid = 1;
      w->redo_mode_line = 0;

      if (!NILP(w->vchild))
	mark_window_display_accurate(w->vchild,flag);
      if (!NILP(w->hchild))
	mark_window_display_accurate(w->hchild,flag);
    }
  if (flag)
    {
      last_arrow_position = Voverlay_arrow_position;
      last_arrow_string = Voverlay_arrow_string;
    }
  else
    {
      last_arrow_position = Qt;
      last_arrow_string = Qt;
    }
}     
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
static int
layout_windows (Lisp_Object window)
{
  int retval = 0;

  for (; !NILP(window); window = XWINDOW(window)->next)
    if (layout_window(window,0))
      retval = 1;

  return retval;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* see comment before Fscroll_other_window in window.c */
extern int bastard_flag_from_hell;
extern struct window *bastard_window_from_hell;

static int
layout_window (Lisp_Object window, int just_this_one)
{
  struct window *w = XWINDOW (window);
  struct screen *s = XSCREEN (w->screen);
  struct buffer *b = XBUFFER (w->buffer);
  int height, opoint;
  struct buffer *old = current_buffer;
  struct position pos;
  register int startp;
  register int hscroll = XINT (w->hscroll);
  int tem,force = 0;
  int first=0;
  int retval;

  if (s->height == 0) abort ();

  /* If this is a combination window, do its children; that's all.  */

  if (!NILP (w->vchild))
    {
      return layout_windows (w->vchild);
    }
  if (!NILP (w->hchild))
    {
      return layout_windows (w->hchild);
    }
  /*
   * If minibuffer is in a distinct screen, this is the minibuffer window,
   * and this isn't the minibuffer screen, then return (silently fail).
   */
  if (SCREENP(Vglobal_minibuffer_screen) && EQ(window,minibuf_window) &&
      s != XSCREEN(Vglobal_minibuffer_screen))
    return 0;
  /*
   * If this is one of (potentially) many minibuffers, then synchronize
   * prior to doing layout.
   */
  if (!SCREENP(Vglobal_minibuffer_screen) && EQ(window,s->minibuffer_window))
    {
      if (!EQ(window,minibuf_window))
	{
	  if (!NILP(Vsynchronize_minibuffers))
	    {
	      /* We've got a duplicate minibuffer window & need to
	       * synchronize it
	       */
	      sync_minibuffer(window);
	    }
	  else
	    {
	      /* We've got a duplicate minibuffer and need to blow off
	       * doing any redisplay of its contents
	       */
	      struct line_header *l = window_display_lines (w);
	      struct Lisp_Font *font = XFONT (SCREEN_DEFAULT_FONT (s));
	      if (l->body != l->end)
		{
		  free_char_blocks (l->body, l->end->prev);
		}
	      l->ascent = font->ascent;
	      l->descent = font->descent;
	      l->lwidth = w->pixleft + LEFT_MARGIN (b, s, w);
	      l->ypos = w->pixtop + l->ascent;
	      l->body = l->end;
	      l->body->next = l->body->prev = 0;
	      l->end->xpos = w->pixleft + LEFT_MARGIN (b, s, w);
	      l->chars = 0;
	      l->changed = 1;
	      list_ptr = 0;
	      BLOCK_TYPE(list_ptr) = BODY_LINE;
	      BLOCK_LINE(list_ptr) = l;
	      BLOCK_LINE_START(list_ptr) = l->body;
	      BLOCK_LINE_END(list_ptr) = 0;
	      BLOCK_LINE_CLEAR(list_ptr) = 1;
	      goto do_update;
	    }
	}
    }
  
  if (NILP (w->buffer))
    abort ();			/* No buffer...we're screwed */

  if (redraw_mode_line)
    w->redo_mode_line = 1;

  /* Stack is empty initially */
  list_ptr = -1;
  /* Set up data on this window; select its buffer and point */
#if 0
  height = window_char_height(w);
  if (!EQ(window,s->minibuffer_window))
    height--;
#else
  height = window_displayed_height (w, 0);
#endif
  if (w == XWINDOW(s->minibuffer_window) && echo_area_glyphs)
    return 0;

  current_buffer = XBUFFER(w->buffer);

  if (MARGINCHANGE)
    w->size_change = 1;

  if (!just_this_one
      && b == XBUFFER (XWINDOW (selected_window)->buffer))
    buffer_shared++;

  opoint = PT;

/*  if (!just_this_one && !NILP(current_buffer->force_redisplay)) */
  if (!just_this_one)
    {
      force = 1;
    }

  if (!EQ (window,selected_window))
    {
      int pointm = marker_position (w->pointm);

      if (w != bastard_window_from_hell)
	{
	  if (pointm < BEGV)
	    SET_PT(BEGV);
	  else if (pointm > ZV)
	    SET_PT(ZV);
	  else
	    SET_PT(pointm);
	}
    }
  else if (bastard_flag_from_hell)
    {
      SET_PT (bastard_flag_from_hell);
    }

  if (XMARKER(w->start)->buffer != current_buffer)
    goto recenter;

  /* If window-start is pointing at point-max, that's bad.
     Unless it's also pointing at point-min, that is.
   */

  /* Chuck gets burned by his bad commenting habits.  I don't remember
     why I thought this is bad.  Having it in definitely screws up
     (set-window-start (selected-window) (point-max)). */
#if 0
  if (XMARKER (w->start)->bufpos == BUF_ZV (XMARKER (w->start)->buffer) &&
      XMARKER (w->start)->bufpos != BUF_BEG (XMARKER (w->start)->buffer))
    goto recenter;
#endif

  startp = marker_position(w->start);

  if (startp < BEGV)
    startp = BEGV;
  else if (startp > ZV)
    startp = ZV;

  if (w->force_start)
    {
      w->redo_mode_line = 1;
      w->force_start = 0;
      w->last_modified = Qzero;
      try_layout(window,startp);
      first++;
      if (point_vpos < 0 && !redisplay_lock)
	{
	  /* If point does not appear, move point so it does appear */
	  /* New interface */
	  pos = *compute_motion (w, startp, 0,
				 ((EQ (window, s->minibuffer_window)
				   && startp == 1)
				  ? minibuf_prompt_width : 0)
				 + (hscroll ? 1 - hscroll : 0),
				 ZV, height / 2,
				 - (1 << (SHORTBITS - 1)),
				 hscroll, pos_tab_offset (w, startp));
	  SET_PT (pos.bufpos);
	  if (w != XWINDOW (selected_window))
	    Fset_marker (w->pointm, make_number (PT), Qnil);
	  else
	    /* We want to change point permanently, so don't restore the
	     * old value.
	     */
	    opoint = PT;

	  if (w == XWINDOW(selected_window))
	    {
	      int i;
	      struct line_header *l = window_display_lines (w);
	      struct char_block *cb = 0;
	      s->cursor_x = max (0, pos.hpos);
	      s->cursor_y = pos.vpos;
	      selected_point_vpos = pos.vpos;
  	      /* Need to find cur_line, cur_char, etc. */
	      i = s->cursor_y;
	      while (l && i--) l = l->next;
	      if (l)
		cb = l->body;
	      i = s->cursor_x + window_needs_vertical_divider (w);
	      if (!i)
		{
		  while (cb && (cb->char_b == FALSE))
		    cb = cb->next;
		}
	      else
		{
		  while (cb && i--)
		    {
		      while (cb && (cb->char_b == FALSE))
			cb = cb->next;
		      cb = cb->next;
		      while (cb && (cb->char_b == FALSE))
			cb = cb->next;
		    }
		}
	      s->new_cur_mir = find_window_mirror (w);
	      s->new_cur_line = l;
	      s->new_cur_char = cb;
	    }
	}
      goto done;
    }
  /* New check...if window height has changed due to font change, etc.
   * and cursor is now off the screen, recenter window around the cursor.
   */
  /* This check is not needed.  Redisplay will handle this case
     later. */
#if 0
  if (EQ(window,selected_window) &&
      s->phys_cursor_y >= height)
    goto recenter;
#endif

  if (force) w->last_modified = Qzero;
  
  if (XINT(w->last_modified) >= MODIFF
      && PT >= startp && !clip_changed /* && !force */
      && just_this_one
      && w != XWINDOW(s->minibuffer_window))
    {
      /* No text has changed...only cursor has moved, and
       * it hasn't moved off the screen.
       */
      if (PT == XINT(w->last_point))
	{
	  /* If cursor hasn't actually moved, don't do anything
	   * (compute_motion is expensive in this context)
	   */
#if 0	  
	  fprintf(stderr,"Bong\n");
#endif	  
	  goto done;
	}
      else
	{
	  pos = *compute_motion (w, startp, 0, (hscroll ? 1-hscroll : 0),
				 PT, height + 1, 10000, hscroll,
				 pos_tab_offset (w, startp));
	  if (pos.vpos < height)
	    {
	      if (EQ(window,selected_window))
		{
		  int i;
		  struct line_header *l = window_display_lines (w);
		  struct char_block *cb;
	  
		  /* Most serious redisplay actions, like switching buffers or
		     changing window-start, cause mode-line updates; however,
		     simple cursor motion does not.  So if the physical y
		     position of the cursor on the screen has changed, we need
		     to recompute the mode line for line-number mode.  If line
		     number mode is off, then changing the cursor position
		     can't cause the mode-line to be out of date (well, the
		     "%p" spec may be out of date, but that is explicitly
		     allowed to lag behind slightly.)

		     #### This code needs to be duplicated somewhere, possibly
		     somewhere in try_layout(), because this branch only runs
		     if there is exaclty one window on the screen.  But I can't
		     figure out how to tell when the cursor has moved from down
		     in try_layout().  WHAT A MESS!!!!
		   */
		  if (line_number_mode && s->cursor_y != pos.vpos)
		    w->redo_mode_line = 1;


		  /* Point is still on screen */
		  s->cursor_x = max(0,pos.hpos); 
		  s->cursor_y = pos.vpos;
		  selected_point_vpos = pos.vpos;
#if DEBUG	  
		  fprintf(stderr,"Found cursor at %d,%d\n",s->cursor_x,s->cursor_y);
#endif
		  /* find new cur_line, cur_char, etc. */
		  i = s->cursor_y;
		  while (l && i--) l = l->next;
		  if (!l) { first++; goto recenter;}
		  cb = l->body;
		  i = s->cursor_x + window_needs_vertical_divider (w);
		  if (!i)
		    {
		      while (cb && (cb->char_b == FALSE))
			cb = cb->next;
		    }
		  else
		    {
		      while (cb && i--)
			{
			  while (cb && (cb->char_b == FALSE))
			    cb = cb->next;
			  cb = cb->next;
			  while (cb && (cb->char_b == FALSE))
			    cb = cb->next;
			}
		    }
		  if (!cb) {first++; goto recenter;}
		  s->new_cur_mir = find_window_mirror (w);
		  s->new_cur_line = l;
		  s->new_cur_char = cb;
		}
	      goto done;
	    }
	  else
	    {
	      first++;
	    }
	}
    }
  else if (w->start_at_line_beg
           && !(startp == BEGV || CHAR_AT (startp - 1) == '\n'))
    {
      /* Current starting point was at beginning of a line, but no longer
       * is; find a new starting point
       */
      goto recenter;
    }
/*  else if (!EQ(window,s->minibuffer_window) */
  
  else if (just_this_one && !EQ(window,s->minibuffer_window)
	   && PT >= startp
	   && XINT(w->last_modified)
	   && w->window_end_valid
	   && (!clip_changed)
	   && !blank_end_of_window
	   && !window_needs_vertical_divider (w)
	   && EQ(last_arrow_position,Voverlay_arrow_position)
	   && EQ(last_arrow_string,Voverlay_arrow_string)		 
/*	   && (tem = try_layout_id(window)) */
	   && 0	/* without try_layout_id this loop should always fail */
	   && tem != -2)
    {
      if (tem > 0)
	goto done;
    }
  else if ((w->size_change ||
	    (startp >= BEGV && startp <= ZV &&
	     (startp < ZV || startp == BEGV ||
	      (XINT(w->last_modified) >= MODIFF)))))
    {
      try_layout(window,startp);
      first++;
      if (point_vpos > -1)
	goto done;
    }
  
  w->last_modified = Qzero;
  w->redo_mode_line = 1;

  /* Try to scroll by a few lines */
  if (scroll_step && !clip_changed)
    {
      if (PT > startp)
	{
	  pos = *vmotion (Z - w->window_end_pos,
			  scroll_step, hscroll, window);
	  if (pos.vpos >= height)
	    goto scroll_fail;
	}
      pos = *vmotion (startp, PT < startp ? -scroll_step : scroll_step,
		      hscroll, window);
      if (PT >= pos.bufpos)
	{
	  try_layout(window,pos.bufpos);
	  if (point_vpos >= 0)
	    {
	      goto done;
	    }
	}
      scroll_fail:
      ;
    }
  
 recenter:
  /* Find a start position which centers point */
  pos = *vmotion (PT, - height / 2, hscroll, window);
 retry:
  /*
   * NOTE:  If FIRST is non-zero, we've already tried laying out window
   * contents, and have lost incremental layout information.  Set flag
   * so that everything is redrawn and no stray chars are left
   */
  tem = s->replot_lines;
  if (first) s->replot_lines = 1;
  try_layout(window, pos.bufpos);
  if (point_vpos == -1 && first < 3)
    {
      /* Ugly; cursor still isn't visible on screen
       * Try scrolling down until cursor is visible.
       * Otherwise, fail.
       */
      first++;
      pos = *vmotion (pos.bufpos, 1, hscroll, window);
      goto retry;
    }
  if (first) s->replot_lines = tem;
  startp = marker_position (w->start);
  w->start_at_line_beg = (startp == BEGV || CHAR_AT (startp - 1) == '\n');
 done:
  /*
   * Do the window's mode line if appropriate.  
   */
  if (!EQ(window,s->minibuffer_window) && (w->redo_mode_line
                                           || w->size_change))
    {
      layout_mode_line(w);
    }

/*  if (old != XBUFFER(w->buffer))
    old->force_redisplay = Qnil; */
  SET_PT(opoint);
  current_buffer = old;

  /*
   * Window's display structure is accurate.  Stack contains requests
   * to be replotted (and possibly erased in prev pos).
   */
 do_update:

  if (interrupt_input)
    unrequest_sigio();
  stop_polling();

  /*
   * If list_ptr is < 0 then we didn't do anything.  We return this
   * value to help redisplay_1 figure out if it really needs to call
   * update_scrollbar or not.
   * Also return 0 if list_ptr == 0 and BLOCK_TYPE(0) == AREA
   */
  if (list_ptr > 0)
    retval = 1;
  else if (list_ptr == 0 && BLOCK_TYPE(0) != AREA)
    retval = 1;
  else
    retval = 0;

  update_window(w);

  if (interrupt_input)
    request_sigio();
  start_polling();

  return retval;
}

/* This function causes more problems than its worth at the moment.  I
   don't think its optimizations will be terribly missed.  If I'm
   wrong, well, it will be getting fixed correctly in the new
   redisplay. */
#if 0
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Return 1 if successful, 0 if clueless, -1 to find a new window start,
 * -2 to do normal redisplay.
 */
static int
try_layout_id (Lisp_Object window)
{
  struct window *w = XWINDOW(window);
  struct screen *s = XSCREEN(w->screen);
#if 0
  int height = window_char_height(w);
#else
  int height = window_displayed_height(w,0) + !EQ(window,s->minibuffer_window);
#endif
  int pos;
  int vpos;
  int start = marker_position(w->start);
  int yend = (w->pixtop + w->pixheight
              - (window_display_modeline (w)->ascent
		 + window_display_modeline (w)->descent));
  int width = window_char_width(w);

  int hscroll = XINT(w->hscroll);
  int lmargin = hscroll > 0 ? 1 - hscroll : 0;
  register int i,tem;
  int last_text_vpos = 0;
  int stop_vpos;

  struct position val,bp,ep,xp,pp;
  int scroll_amount = 0;
  int delta;
  int tab_offset, epto;

  struct line_header *l;

  if (subwindows_being_displayed)
    return -2;

  if (!EQ(window,s->minibuffer_window)) height--;
  
  if (GPT - BEG < beg_unchanged)
    beg_unchanged = GPT - BEG;
  if (Z - GPT < end_unchanged)
    end_unchanged = Z - GPT;

  if (beg_unchanged + 1 < start)
    return 0;			/* Give up if changes go above window */

  bp = *compute_motion (w,start,0,lmargin,
			beg_unchanged+1,10000,10000,
			hscroll,pos_tab_offset(w,start));
  if (bp.vpos > height)
    {
      if (PT < bp.bufpos && !bp.contin)
	{
	  /* All changes are below the screen, and point is on screen.
	     Don't need to change the screen at all, but do need to
	     update window_end_pos */
	  bp = *compute_motion(w,start,0,lmargin,
			       Z, height, 0,
			       hscroll,pos_tab_offset(w,start));
	  w->window_end_vpos = height;
	  w->window_end_pos = Z - bp.bufpos;
	  return 1;
	}
      return 0;
    }

  vpos = bp.vpos;

  bp = *vmotion(bp.bufpos,0,hscroll,window);

  pos = bp.bufpos;
  val.hpos = lmargin;
  if (pos < start)
    return -1;

  /* If about to start displaying at beginning of continuation line,
   * really start w/ prev line.
   */
  if ((bp.contin && bp.bufpos - 2 == beg_unchanged && vpos > 0)
      || (FIXNUMP (current_buffer->selective_display)
          && XINT (current_buffer->selective_display) > 0
          && (bp.bufpos - 2 == beg_unchanged && vpos > 0)))
    {
      bp = *vmotion(bp.bufpos,-1,hscroll,window);
      --vpos;
      pos = bp.bufpos;
    }
  if (bp.contin && bp.hpos != lmargin)
    {
#ifdef DEBUG_WRAPPED      
      char ch1,ch2;
#endif      
      int p1,p2;
      int old_pt;
      
      /* FUDGE */
#ifdef DEBUG_WRAPPED
      ch1 = CHAR_AT(pos);
      ch2 = CHAR_AT(pos - 1);
#endif
      old_pt = PT;
      if (pos)
	{
	  SET_PT(pos);
	  p1 = current_column();
	  SET_PT(old_pt);
	}
      else
	p1 = current_column();

      old_pt = PT;
      if (pos - 1)
	{
	  SET_PT(pos - 1);
	  p2 = current_column();
	  SET_PT(old_pt);
	}

      val.hpos = p2 - p1 + 1 + lmargin;
      pos--;      
#if 0
      /* Original code was as follows:  This is no longer a valid
       * calculation of val.hpos since the line conceivably has more or less
       * characters than width (if proportional fonts)
       */
      val.hpos = bp.prevhpos - (width - 1) + lmargin;
      pos--;

#endif  
  
    }

  bp.vpos = vpos;
  
  /* Find first visible newline after which no more is changed */
  tem = find_next_newline(current_buffer,Z - max(end_unchanged, Z-ZV),1);

  if (FIXNUMP (current_buffer->selective_display)
      && XINT(current_buffer->selective_display) > 0)
    while (tem < ZV - 1
	   && (position_indentation (current_buffer, tem)
	       >= XINT(current_buffer->selective_display)))
      {
        tem = find_next_newline (current_buffer,tem,1);
      }

  /* Compute the cursor position after that newline */
  ep = *compute_motion(w,pos,vpos,val.hpos,tem,height,
		       -(1 << (SHORTBITS - 1)),
		       hscroll,pos_tab_offset(w,bp.bufpos));

  /* If changes reach past the text available on screen, just
     display rest of screen */

  if (ep.bufpos > Z - w->window_end_pos)
    stop_vpos = height;
  else
    stop_vpos = ep.vpos;

  if (stop_vpos == ep.vpos
      && (ep.bufpos == BEGV
	  || CHAR_AT(ep.bufpos - 1) != '\n'
	  || ep.bufpos == Z - end_unchanged))
    stop_vpos = ep.vpos + 1;

  point_vpos = -1;
  point_v = 0;
  point_h = 0;

  if (stop_vpos < height)
    {
      epto = pos_tab_offset(w,ep.bufpos);


      xp = *compute_motion(w,ep.bufpos,ep.vpos,ep.hpos,
			   Z - w->window_end_pos,
			   10000,0,
			   hscroll,epto);

      scroll_amount = xp.vpos - w->window_end_vpos;
	  
      /* Is everything on screen below changes whitespace?
	 If so, blow off scrolling */
      for (i = ep.bufpos; i < xp.bufpos; i++)
	{
	  tem = CHAR_AT(i);
	  if (tem != ' ' && tem != '\n' && tem != '\t')
	    break;
	}
      if (i == xp.bufpos)
	return -2;

      w->window_end_vpos = scroll_amount + w->window_end_vpos;

      /* Before doing any scrolling, verify that point will be on screen */
      if (PT > ep.bufpos && !(PT <= xp.bufpos && xp.bufpos < height))
	{
	  if (PT <= xp.bufpos)
	    {
	      pp = *compute_motion(w,ep.bufpos,ep.vpos,ep.hpos,
				   PT, height,
				   -(1 << (SHORTBITS -1)),
				   hscroll,epto);
	    }
	  else
	    {
	      pp = *compute_motion(w,xp.bufpos,xp.vpos,xp.hpos,
				   PT, height,
				   -(1 << (SHORTBITS -1)),
				   hscroll,pos_tab_offset(w,xp.bufpos));
	    }
	  if (pp.bufpos < PT || pp.vpos == height)
	    return 0;
	  point_vpos = pp.vpos;
	  point_hpos = pp.hpos + window_needs_vertical_divider (w);
	}

      list_ptr = -1;

#if 0
      if (DEBUG_ALL)
	print_window(w);
#endif
      /* #### Should it be possible for these to be nil here?  I'm really
	 not sure but it is happening.  For now, let's just put a check
	 in to handle that case. */
      if (s->cur_char)
	{
	  old_cur_char.ch = s->cur_char->ch;
	  old_cur_char.glyph = s->cur_char->glyph;
	  old_cur_char.char_b = s->cur_char->char_b;
	  old_cur_char.blank = s->cur_char->blank;
	  old_cur_char.xpos = s->cur_char->xpos;
	  old_cur_char.face = s->cur_char->face;
	}

      if (s->cur_line)
	{
	  old_cur_line.chars = s->cur_line->chars;
	  old_cur_line.ypos = s->cur_line->ypos;
	  old_cur_line.ascent = s->cur_line->ascent;
	  old_cur_line.descent = s->cur_line->descent;
	}
      
      if (stop_vpos - scroll_amount >= height
	  || ep.bufpos == xp.bufpos)
	{
	  if (scroll_amount < 0)
	    stop_vpos -= scroll_amount;
	  scroll_amount = 0;
	  blank_end_of_window = 1;
	}
      else if (!scroll_amount)
	{}
      else if (bp.bufpos == Z - end_unchanged)
	{
	  tem = scroll_screen_lines(bp.vpos - scroll_amount,
				    height - max(0,scroll_amount),
				    scroll_amount,w);
	  if (tem < 0)
	    {
	      /* Don't know how, but scrolling info is garbaged wrt the
	       * actual display structs.  Fail and do a regular layout.
	       */
	      return -2;
	    } 
	}
      else if (scroll_amount)
	{
	  tem = scroll_screen_lines(ep.vpos - scroll_amount,
				    height - max(0,scroll_amount),
				    scroll_amount,w);

	  if (tem < 0)
	    {
	      /* Don't know how, but scrolling info is garbaged wrt the
	       * actual display structs.  Fail and do a regular layout.
	       */
	      return -2;
	    }
	}
    }

#if 0
  if (DEBUG_ALL)
    print_window(w);
#endif

  if (stop_vpos >= height)
    { 
      return -2;
    }
#if 0
  if (stop_vpos >= height)
    {
      stop_vpos = height;
      scroll_amount = 0;
    }
#endif
  if (vpos == 0 && pos < marker_position(w->start))
    Fset_marker(w->start,make_number(pos),Qnil);

  last_text_vpos = vpos;

  tab_offset = pos_tab_offset(w,pos);
  if (val.hpos + hscroll - (hscroll > 0) < 0)
    tab_offset += width - 1;

  /* Layout lines that were inserted */

  {
    int propagation = 0;
    int glyph_prop = 0;
    int ypos;
    short old_asc, old_desc;

    if (vpos < stop_vpos && vpos < height)
      {
	i = vpos;
	l = window_display_lines (w);
	while (l && i--)
          l = l->next;
	if (l)
	  ypos = ((l->prev)
                  ? (l->prev->ypos + l->prev->descent)
                  : w->pixtop);

	while (vpos < stop_vpos)
	  {
	    old_asc = l->ascent;
	    old_desc = l->descent;
	    val = *layout_text_line(w,vpos,pos,val.hpos,
				    tab_offset,&propagation,&glyph_prop,l);
	    eob_hack_flag = 0;

	    propagation = 0;
	    l->shifted = 0;
	    tab_offset += width - 1;
	    if (val.vpos) tab_offset = 0;

	    l->prevy = l->ypos;
	    l->ypos = ypos + l->ascent;
	    ypos += (l->ascent + l->descent);
	    vpos++;
	    if (pos != val.bufpos)
	      last_text_vpos
		= vpos - (val.vpos && (CHAR_AT (val.bufpos - 1) != '\n'));
	    pos = val.bufpos;

	    l = next_line(l);
	  }
      }
    else
      {
	l = 0;
	ypos = 0;
      }
    if (vpos == height && l)
      {
	l->prev->next = 0;
	free_line(l);
	l = 0;
      }

    /*
     * If scroll_amount == 0 and l->ypos != (ypos + l->ascent) then
     * some line changed height and we are screwed.  Do a regular
     * redisplay.
     */
    if (!scroll_amount && l && (l->ypos != ypos + l->ascent))
      {
	if (l->prev)
	  {
	    l->prev->ascent = old_asc;
	    l->prev->descent = old_desc;
	  }
	return -2;
      }

    /* Now update y positions for remaining lines, and possibly clip
     * lines at end of window
     */
    if (scroll_amount > 0)
      {
	while (ypos < yend)
	  {
	    if (!l) break;
	    
	    if ((int)(ypos + l->ascent + l->descent) > yend)
	      {
		/* Remaining lines can be clipped */
#if 0
		if (l->prev) l->prev->next = 0;

		if (BLOCK_TYPE(0) == BLIT)
		  BLOCK_BLIT_END(0) = l->prev;
		 
		free_lines(l);
#endif		
		break;
	      }
	    l->prevy = l->ypos;
	    l->ypos = ypos + l->ascent;
	    ypos += (l->ascent + l->descent);

	    l = l->next;
	  }

	if (l && l != window_display_lines (w))
	  {
	    if (BLOCK_TYPE(0) == BLIT)
	      BLOCK_BLIT_END(0) = l->prev;
	    
	    if (l->prev) l->prev->next = 0;
	    free_lines(l);
	    l = 0;
	  }
      }	
  }

 if (vpos == height)
   {
     if (val.hpos < lmargin)
       val.bufpos++;
     w->window_end_vpos = last_text_vpos;
     w->window_end_pos = Z - val.bufpos;
   }

  if (scroll_amount < 0)
    {
      struct line_header *l,*prev;
      int ypos, last_bufpos;
      int propagation = 0;
      int glyph_prop = 0;
            
      vpos = xp.vpos;
      pos = xp.bufpos;
      last_bufpos = pos;

      BLOCK_BLIT_START(0)->prevy = BLOCK_BLIT_OLD_TOP(0);

      val.hpos = lmargin;
      if (pos == ZV)
	vpos = height + scroll_amount;
      else if (xp.contin && xp.hpos != lmargin)
	{
	  val.hpos = xp.prevhpos - width + lmargin;
	  pos--;
	}

      blank_end_of_window = 1;
      tab_offset = pos_tab_offset(w,pos);
      if (val.hpos < 0)
	tab_offset += width - 1;

      prev = BLOCK_BLIT_END(0);
      l = next_line(prev);
      ypos = prev->ypos + prev->descent;

      if (val.hpos < 0)
	tab_offset += width - 1;

      while (ypos < yend)
	{
	  val = *layout_text_line(w,vpos,pos,val.hpos,
				  tab_offset,&propagation,&glyph_prop,l);
	  eob_hack_flag = 0;

	  if (!window_needs_vertical_divider (w) && last_bufpos == val.bufpos)
	    {
	      if (point_vpos == vpos) point_vpos = -1;
	      DECREMENT_LIST_PTR;
	      goto window_done;
	    }

	  tab_offset += width - 1;
	  if (val.vpos) tab_offset = 0;
	  if ((int)(ypos + l->ascent + l->descent) > yend)
	    {
	      if (point_vpos == vpos) point_vpos = -1;
	      DECREMENT_LIST_PTR;
	      goto window_done;
	    }

	  l->prevy = l->ypos;
	  l->ypos = ypos + l->ascent;
	  ypos += (l->ascent + l->descent);
	  vpos++;
	  if (pos != val.bufpos)
	    last_text_vpos
	      = vpos - (val.vpos && (CHAR_AT(val.bufpos - 1) != '\n'));
	  pos = val.bufpos;

	  prev = l;
	  l = next_line(l);
	  last_bufpos = val.bufpos;
	}

     window_done:

      if (l && l != window_display_lines (w)
	  && !window_needs_vertical_divider (w))
	{
	  free_lines(l);
	  if (prev) prev->next = 0;
	}
      
      if (xp.bufpos == ZV
	  && xp.bufpos == PT)
	point_vpos = -1;
      
    }

  i = 1;
  l = window_display_lines (w);
  while (l->next) { l = l->next; i++; }

  w->window_end_ppos = l->ypos + l->descent;
  w->used_height = i;           /* # lines in window */

  if (yend - (int) (l->ypos + l->descent) > 0)
    {
      list_ptr++;
      BLOCK_TYPE(list_ptr) = AREA;
      BLOCK_AREA_TOP(list_ptr) = l->ypos + l->descent;
      BLOCK_AREA_BOTTOM(list_ptr) = yend;
    }

  if (scroll_amount)
    {
      delta = height - xp.vpos;
      if (delta < 0
	  || (delta > 0 && xp.bufpos < ZV)
	  || (delta == 0 && xp.hpos))
	{
	  val = *vmotion (Z - w->window_end_pos,
                          delta, hscroll, window);
	  w->window_end_pos = Z - val.bufpos;
	  w->window_end_vpos = val.vpos + w->window_end_vpos;
	}

/*      s->cur_char = &old_cur_char; */
/*      s->cur_line = &old_cur_line; */
    }

  if (list_ptr > -1 && w == XWINDOW(selected_window))
    {
      s->cur_char = &old_cur_char;
      s->cur_line = &old_cur_line;
    }

  w->window_end_valid = 0;

  if (point_vpos < 0)
    {
      val = *compute_motion(w,start,0,lmargin,
			    PT, 10000, 10000,
			    hscroll,pos_tab_offset(w,start));
      if (val.vpos >= height)
	{
	  /* Everything for this window is hosed */
	  s->replot_lines = 1;
	    
	  return 0;		/* Fail */
	}

      point_vpos = val.vpos;
      point_hpos = val.hpos + window_needs_vertical_divider (w);
    }

  /* Find cursor stuff */
  if (!cursor_in_echo_area)
    {
      s->cursor_x = max(0,point_hpos);
      s->cursor_y = point_vpos;
      selected_point_vpos = point_vpos;
      {
	int i;
	struct line_header *l = window_display_lines (w);
	struct char_block *cb = 0;

	i = s->cursor_y;
	while (l && i--) l = l->next;
	if (l)
	  cb = l->body;
	i = s->cursor_x;
	if (!i)
	  {
	    while (cb && (cb->char_b == FALSE))
	      cb = cb->next;
	  }
	else
	  {
	    while (cb && i--)
	      {
		while (cb && (cb->char_b == FALSE))
		  cb = cb->next;
		cb = cb->next;
		while (cb && (cb->char_b == FALSE))
		  cb = cb->next;
	      }
	  }

	s->new_cur_mir = find_window_mirror (w);
	s->new_cur_line = l;
	s->new_cur_char = cb;
      }
    }

  return 1;
}
#endif /* 0 */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Given WINDOW's current display structures and current buffer contents
 * begining at POS, produce a stack of "blocks" to plot (these will be a
 * full line at most), and update the display structures including attributes
 * and dependencies.  layout_text_line() is responsible for pushing "blocks"
 * on stack.
 */

static void
try_layout (Lisp_Object window, register int pos)
{
  register struct window *w = XWINDOW(window);
  struct screen *s = XSCREEN(w->screen);
  struct line_header *l,*prev = 0;
  int vpos = 0;
  int ypos = w->pixtop;
  int yend = w->pixtop + w->pixheight;
  int last_text_vpos = vpos;
  int last_bufpos;
  int width = window_char_width(w);
  struct position val,save_val;
  int tab_offset = pos_tab_offset(w,pos);
  int propagation = 0;
  int glyph_prop = 0;
  int before_vpos = selected_point_vpos;

  if (s == selected_screen && w == XWINDOW(s->selected_window) && s->cur_char && s->cur_line)
    {
      /* If window is on current screen and is the selected window,
       * save exact info on clearing cursor from where it is.  Pointing
       * into window's display structs isn't sufficient; the objects pointed
       * to may be freed and allocated with new, different information.  It
       * is ok if this is done repeatedly; subsequent times will be setting
       * values to what they already are.
       *
       * Following this call, s->new_cur_* will point into the display
       * structs for the new cursor position.  At this point, we are
       * guaranteed to follow this by calling update_cursor(), which will
       * set s->cur_* to be pointing into the display structs again.
       */
      old_cur_char.ch = s->cur_char->ch;
      old_cur_char.glyph = s->cur_char->glyph;
      old_cur_char.char_b = s->cur_char->char_b;
      old_cur_char.blank = s->cur_char->blank;
      old_cur_char.xpos = s->cur_char->xpos;
      old_cur_char.face = s->cur_char->face;

      old_cur_line.chars = s->cur_line->chars;
      old_cur_line.ypos = s->cur_line->ypos;
      old_cur_line.ascent = s->cur_line->ascent;
      old_cur_line.descent = s->cur_line->descent;

      s->cur_char = &old_cur_char;
      s->cur_line = &old_cur_line;
    }

  point_vpos = -1;		/* Attempt to locate cursor (point) */
  list_ptr = -1;		/* Begin redisplay for a window. */
  last_bufpos = -1;
  val.bufpos = -2;

  if (pos != marker_position(w->start))
    {
      Fset_marker(w->start,make_number(pos),Qnil);
    }

      
  if (!EQ(window,s->minibuffer_window) && window_display_modeline (w)
      && window_display_modeline (w)->ascent == 0)
    {
      int old_list_ptr = list_ptr;

      layout_mode_line(w);
      yend -= (window_display_modeline (w)->ascent
	       + window_display_modeline (w)->descent);
      if (window_display_modeline (w))
	{
	  free_line (window_display_modeline (w));
	  find_window_mirror(w)->modeline = NULL;
	}

      /* We used to assume list_ptr was always incremented in
         layout_mode_line.  We no longer assume this, so we no
         longer decrement list_ptr; instead, just use previous
         value of list_ptr.  Thanks to rowley@epx.cis.umn.edu 
         (Henry A. Rowley). */
      /* list_ptr--; */

      list_ptr = old_list_ptr;
    }
     
  if (window_display_modeline (w))
    yend -= (window_display_modeline (w)->ascent
	     + window_display_modeline (w)->descent);

  if (!window_display_lines (w))
    {
      l = get_line();
      find_window_mirror(w)->lines = l;
    }
  l = window_display_lines (w);

  val.hpos = XINT(w->hscroll) ? 1 - XINT(w->hscroll) : 0;

  eob_hack_flag = 0;
  while (ypos < yend)
    {
      save_val.hpos = val.hpos;
      
      val = *layout_text_line(w,vpos,pos,val.hpos,tab_offset,&propagation,
			      &glyph_prop,l);
      tab_offset += width - 1;
      if (val.vpos) tab_offset = 0;
      
      if ((int)(ypos + l->ascent + l->descent) > yend)
	{
	  /*
 	   * latest line extends past modeline, so ultimately trash it
	   * Be sure it doesn't accidently get redisplayed...
	   * If cursor was found on this line, mark it so that it wasn't,
	   * so window will scroll appropriately.
	   */
	  val.hpos = save_val.hpos;
	  if (point_vpos == vpos) point_vpos = -1;
	  DECREMENT_LIST_PTR;
	  goto layout_done;
	}

      /*
       * Line fits, so set it's y position relative to screen (0,0)
       * in pixels. */
      l->prevy = l->ypos;
      l->ypos = ypos + l->ascent;
      ypos += (l->ascent + l->descent);
      vpos++;
      if (pos != val.bufpos)
	last_text_vpos
	  = vpos - (val.vpos && (CHAR_AT (val.bufpos - 1) != '\n'));
      pos = val.bufpos;
      
      prev = l;
      l = next_line(l);      

      if (!window_needs_vertical_divider (w) && last_bufpos == val.bufpos)
	{
	  break;
	}
      last_bufpos = val.bufpos;
    }
 layout_done:

  /*
   * If remaining lines currently exist in display structures for this
   * window, then something has changed (window height, etc.), so remove
   * them.
   */
  if (l && l != window_display_lines (w) && !window_needs_vertical_divider (w))
    {
      free_lines(l);
      if (prev) prev->next = 0;
    }

  /*
   * Include split character in text on window if last line is split in
   * middle of a character
   */
  if (val.hpos < (XINT(w->hscroll) ? 1 - XINT(w->hscroll) : 0))
    pos++;

  /* Clear to end of window if window size has changed, other dire conditions,
   * or distance to end of window has decreased
   */
  if ((yend - ypos) > 0)
    {
      list_ptr++;
      BLOCK_TYPE(list_ptr) = AREA;
      BLOCK_AREA_TOP(list_ptr) = ypos;
      BLOCK_AREA_BOTTOM(list_ptr) = yend;
    }

  w->used_height = vpos;        /* # of lines displayed in window */
  w->window_end_vpos = last_text_vpos;
  w->window_end_ppos = ypos;    /* Pixel pos of end of window's text */
  w->window_end_pos = Z - pos;
  w->size_change = 0;
  w->window_end_valid = 0;	/* Not valid until display completes */
  BUF_MARGINCHANGE (XBUFFER (w->buffer)) = 0;

  if (line_number_mode && (w == XWINDOW (selected_window)) &&
      (before_vpos != selected_point_vpos))
    w->redo_mode_line = 1;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Macros for adding a character during layout.  Use this global array for
 * passing single-char values into text_width().  (UGLY)
 * next_char() -> only update positional attributes
 * add_char -> update everything
 */
/* char ac[2]; */

/*
 * The functions next_char, add_char, add_text_glyph and
 * update_position used to be macros.  They really need to be
 * commented and could probably stand a little clean-up work.  They
 * are basically straight conversions of the former macros to
 * functions.  The macros CHANGE_TEXT_GLYPH and CHANGE_CHAR were not
 * duplicated as functions.  Their functionality was merged into
 * update_position.
 */

#ifdef I18N4
#define CHAR_CB_F(block,letter)\
  (!(block)->char_b || ((wchar_t)((block)->ch)) != (letter))
#else
#define CHAR_CB_F(block,letter)\
  (!(block)->char_b || ((unsigned char)((block)->ch)) != (letter))
#endif
#define GLYPH_CB_F(block,indice)\
  ((block)->char_b || (((block)->glyph)) != (indice))

#define PIXMAP_CONTRIB_P(p) (XPIXMAP((p))->contrib_p)

#define MARK_SUBWINDOW_USAGE() {\
  l->subwindow = 1;\
  find_window_mirror (w)->subwindows_being_displayed = 1;\
  s->subwindows_being_displayed = 1;\
  subwindows_being_displayed = 1; }

extern int buffer_window_mru (struct window *w);

static int
next_char (struct window *w, struct layout_data *data, struct line_header *l,
	   int flag)
{
  struct screen *s = XSCREEN (w->screen);
  int wid;
#ifdef I18N4
  wchar_t ac[2];
#else
  unsigned char ac[2];
#endif

  if (data->cb->char_b)
    {
      ac[0] = data->cb->ch;
      wid = text_width (data->lfont, ac, 1);
    }
  else
    wid = glyph_width (data->cb->glyph, data->lfont);

  if (flag && (data->lwidth + wid) > data->pixright)
    {
      if (data->cb->char_b)
	return 0;
      else
	{
	  wid = data->pixright - data->lwidth;
	  if (!wid)
	    return 0;
	}
    }

  if (data->prev && data->prev->xpos + data->prev->width != data->cb->xpos)
    {
      data->endc = data->cb;
      data->cb->width = wid;
      data->cb->xpos = (data->prev
			? data->prev->xpos + data->prev->width
			: data->border);
    }

  if (!data->cb->char_b)
    {
      Lisp_Object lglyph = glyph_to_pixmap (data->cb->glyph);

      if ((PIXMAPP (lglyph) && PIXMAP_CONTRIB_P (lglyph))
	  || SUBWINDOWP (lglyph))
	{
	  if (glyph_height (data->cb->glyph, data->lfont) >
	      (data->asc + data->desc))
	    data->asc += (glyph_height (data->cb->glyph, data->lfont)
			  - data->asc - data->desc);
	  data->old_asc = 0;
	}
      if (SUBWINDOWP (glyph_to_pixmap (data->cb->glyph)))
	MARK_SUBWINDOW_USAGE();
    }

  data->cb->prev = data->prev;
  data->prev = data->cb;
  data->cb = data->cb->next;
  data->lwidth += data->prev->width;

  return 1;
}

static int
#ifdef I18N4
add_char (wchar_t letter, struct layout_data *data,
	  struct line_header *l, int flag)
#else
add_char (unsigned char letter, struct layout_data *data,
	  struct line_header *l, int flag)
#endif
{
  int wid;
#ifdef I18N4
  wchar_t ac[2];
#else
  unsigned char ac[2];
#endif

  ac[0] = letter;
  wid = text_width (data->lfont, ac, 1);

  if (flag && (data->lwidth + wid) > data->pixright)
    return 0;

  data->cb = next_block (data->cb, l);
  data->cb->ch = letter;
  data->cb->char_b = TRUE;
  data->cb->blank = FALSE;
  data->cb->width = wid;
  data->cb->xpos = (data->prev
		    ? data->prev->xpos + data->prev->width
		    : data->border);
  data->cb->face = data->face;
  data->cb->prev = data->prev;
  data->prev = data->cb;
  data->endc = data->cb;
  data->cb = data->cb->next;
  l->changed = 1;
  data->lwidth += data->prev->width;

  return 1;
}

static int
add_text_glyph (struct window *w, GLYPH letter, struct layout_data *data,
		struct line_header *l, int flag)
{
  struct screen *s = XSCREEN (w->screen);
  int wid;
  Lisp_Object lglyph = glyph_to_pixmap (letter);

  wid = glyph_width (letter, data->lfont);
  if (flag && (data->lwidth + wid) > data->pixright)
    {
      wid = data->pixright - data->lwidth;
      if (!wid)
	return 0;
    }

  if ((PIXMAPP (lglyph) && PIXMAP_CONTRIB_P (lglyph))
      || SUBWINDOWP (lglyph))
    {
      if (glyph_height (letter, data->lfont) > (data->asc + data->desc))
	{
	  data->asc += (glyph_height (letter, data->lfont)
			- data->asc - data->desc);
	  data->old_asc = 0;
	}
    }

  data->cb = next_block (data->cb,l);
  data->cb->glyph = letter;
  data->cb->char_b = FALSE;
  data->cb->blank = FALSE;
  data->cb->width = wid;
  data->cb->xpos = (data->prev
		    ? data->prev->xpos + data->prev->width
		    : data->border);
  data->cb->face = data->face;
  data->cb->prev = data->prev;
  data->prev = data->cb;
  data->endc = data->cb;
  data->cb = data->cb->next;

  if (SUBWINDOWP (glyph_to_pixmap (letter)))
    MARK_SUBWINDOW_USAGE();
  l->changed = 1;
  data->lwidth += data->prev->width;

  return 1;
}

GLYPH
extent_glyph (EXTENT e, int begin)
{
  if (begin)
    return extent_begin_glyph (e);
  else
    return extent_end_glyph (e);
}


/*
 * first loop layout being glyphs
 * second loop layout characters
 * third loop layout end glyphs */
static int
#ifdef I18N4
update_position (struct window *w, wchar_t letter,
		 struct layout_data *data,
		 struct line_header *l, int *glyph_prop,
		 int replot, int flag, int nochar)
#else
update_position (struct window *w, unsigned char letter,
		 struct layout_data *data,
		 struct line_header *l, int *glyph_prop,
		 int replot, int flag, int nochar)
#endif
{
  struct screen *s = XSCREEN (w->screen);
  int loop, eflag;
  struct char_block *change_marker = 0;

  for (loop = 1; loop <= 3; loop++)
    {
      struct extent *e;
      int i, col_cnt;
      int skip_char = (*glyph_prop < 0);
      Lisp_Object *class;

      if (loop == 1)
	{
	  col_cnt = data->display_info->begin_columns;
	  class = data->display_info->begin_class;
	  eflag = 1;
	}
      else if (loop == 3)
	{
	  col_cnt = data->display_info->end_columns;
	  class = data->display_info->end_class;
	  eflag = 0;
	}

      if (loop == 2 && !nochar && !skip_char)
	{
	  if (CHAR_CB_F(data->cb, letter) || replot)
	    {
	      /* If there are end glyphs to be displayed then *all*
		 of them must be able to fit on the line, else we
		 wrap the character to the next line. */
	      /* #### This can cause a blow up if the sum width of the end
		 glyph is wider than the window.  FIX ME. */
	      if (flag && data->text_glyph_width)
		{
		  int wid;
#ifdef I18N4
		  wchar_t ac[2];
#else
		  unsigned char ac[2];
#endif
		  ac[0] = letter;
		  wid = text_width (data->lfont, ac, 1);
		  if ((data->lwidth + wid + data->text_glyph_width)
		      > data->pixright)
		    {
		      data->text_glyph_width = 0;
		      data->text_glyphs_added = 0;
		      return 0;
		    }
		}

	      if (!add_char (letter, data, l, flag))
		return 0;
	      change_marker = change_marker ? change_marker : data->prev;
	    }
	  else if (data->cb->face != data->face)
	    {
	      int wid;
#ifdef I18N4
	      wchar_t ac[2];
#else
	      unsigned char ac[2];
#endif
	      ac[0] = letter;
	      wid = text_width (data->lfont, ac, 1);
	      if (flag && (data->lwidth + wid) > data->pixright)
		return 0;
	      data->cb->ch = letter;
	      data->cb->char_b = TRUE;
	      data->cb->blank = FALSE;
	      data->cb->width = wid;
	      data->cb->xpos = (data->prev
				? data->prev->xpos + data->prev->width
				: data->border);
	      data->cb->face = data->face;
	      data->cb->prev = data->prev;
	      data->prev = data->cb;
	      data->endc = data->cb;
	      data->cb = data->cb->next;
	      l->changed = 1;
	      data->lwidth += data->prev->width;

	      change_marker = change_marker ? change_marker : data->prev;
	    }
	  else
	    if (!next_char (w, data, l, flag))
	      return 0;

	  data->new_cpos = data->prev;
	}
      else
	{
	  if (*glyph_prop < 0)
	    *glyph_prop = 0;
	  if (col_cnt <= *glyph_prop)
	    {
	      *glyph_prop -= col_cnt;
	      col_cnt = 0;
	    }
	  i = *glyph_prop;

	  while (data->text_glyph_width && i < col_cnt)
	    {
	      e = XEXTENT (class[i]);

	      if (EXTENT_GLYPH_LAYOUT_P (e, GL_TEXT))
		{
                  struct face *t_face = data->face;
		  Lisp_Object t_lfont = data->lfont;

 		  if (EQ (Fextent_start_position (class[i]),
			  Fextent_end_position (class[i])))
		    {
		      data->face = get_face_at_bufpos (s, current_buffer,
						       data->pos);
		    }
		  data->lfont = FACE_FONT (data->face);

		  if (GLYPH_CB_F(data->cb, extent_glyph (e, eflag)) || replot)
		    {
		      if (!add_text_glyph (w, extent_glyph (e, eflag), data,
					   l, flag))
			return 0;
		      if (extent_face_id (e) >= 0)
			{
			  data->asc =
			    max (data->asc, XFONT (data->lfont)->ascent);
			  data->desc =
			    max (data->desc, XFONT (data->lfont)->descent);
			}
		      data->prev->e = e;
		      change_marker = (change_marker
				       ? change_marker
				       : data->prev);
		    }
		  else if (data->cb->face != data->face)
		    {
		      GLYPH glyph = extent_glyph (e, eflag);
		      int wid = glyph_width (glyph, data->lfont);
		      Lisp_Object lglyph = glyph_to_pixmap (glyph);

		      if (flag && (data->lwidth + wid) > data->pixright)
			{
			  wid = data->pixright - data->lwidth;
			  if (!wid)
			    return 0;
			}

		      if ((PIXMAPP (lglyph) && PIXMAP_CONTRIB_P (lglyph))
			  || SUBWINDOWP (lglyph))
			{
			  unsigned short height =
			    glyph_height (glyph, data->lfont);
			  if (height > (data->asc + data->desc))
			    {
			      data->asc += (height - data->asc - data->desc);
			      data->old_asc = 0;
			    }
			}

		      data->cb->glyph = glyph;
		      data->cb->char_b = FALSE;
		      data->cb->blank = FALSE;
		      data->cb->width = wid;
		      data->cb->xpos = (data->prev
					? data->prev->xpos + data->prev->width
					: data->border);
		      data->cb->face = data->face;
		      data->cb->prev = data->prev;
		      data->prev = data->cb;
		      data->endc = data->cb;
		      data->cb = data->cb->next;

		      if (SUBWINDOWP (glyph_to_pixmap (glyph)))
			MARK_SUBWINDOW_USAGE();
		      l->changed = 1;
		      data->lwidth += data->prev->width;

		      data->prev->e = e;
		      change_marker = (change_marker
				       ? change_marker
				       : data->prev);
		    }
		  else
		    if (!next_char (w, data, l, flag))
		      return 0;

		  data->text_glyphs_added++;
		  data->text_glyph_width -=
		    glyph_width (data->prev->glyph, data->lfont);

		  data->face = t_face;
		  data->lfont = t_lfont;
		}
	      i++;
	    }
	  if (col_cnt > *glyph_prop)
	    *glyph_prop = 0;
	}
    }

  if (change_marker)
    data->beginc = data->beginc ? data->beginc : change_marker;

  return 1;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Layout a line L of text of window W's buffer.  Line # VPOS in window,
 * and begins at START in the buffer.  L is marked "Changed" if something
 * in its contents has changed between display structs and actual buffer
 * contents.  L is marked "Shifted" if it is unchanged, but is shifting
 * vertically due to vertical dependencies on lines above it in the window.
 * Line is also marked "Changed" if the replot_lines flag has been
 * set for this iteration int he display code.  (Another external dependency)
 * Redisplay requests contain L, and the START and END of changed regions.
 */
static struct position val_layout_text_line;
static Lisp_Object glyph_list[100]; /* >>> Fixed limit */
static int glyph_pos[100]; /* #### oh for a rewrite */
static struct face *face_list[100]; /* mirrors glyph_list */

static struct position *
layout_text_line (struct window *w, int vpos, int start, int hpos,
		  int taboffset, int *propagation, int *glyph_prop,
		  struct line_header *l)
{
  struct screen *s = XSCREEN(w->screen);
  struct char_block *t;
  struct char_block *cpos;
  struct layout_data data;
  int pos = start;
  int pause;
  int end;
  int i;
#ifdef I18N4
  wchar_t c;
  wchar_t c1;
#else
  unsigned char c;
  unsigned char c1;
#endif
  int lastpos;
  int numchars;
  int inc = 0;
  int the_pos;
#ifdef I18N4
  wchar_t ac[2];
#else
  unsigned char ac[2];
#endif
  struct face *e_face;
  int multi = 0;		/* Last buffer char was a multi-character */
  char replot_lines;
  int tab_width = XINT(current_buffer->tab_width);
  Lisp_Object ctl_arrow = current_buffer->ctl_arrow;
  int ctl_p = !NILP (ctl_arrow);
  /* this gets cast to an unsigned char so 255 is the largest
     reasonable value to set it to. */
  int printable_min = (FIXNUMP (ctl_arrow)
		       ? XINT (ctl_arrow)
		       : ((EQ (ctl_arrow, Qt) || EQ (ctl_arrow, Qnil))
			  ? 255 : 160));
  struct position val;
  struct buffer *b = current_buffer;
  int invis,button_start,button_end,foo = 0;
  int hscroll = XINT (w->hscroll);
  int truncate = (hscroll 
                  || (truncate_partial_width_windows
                      && window_needs_vertical_divider (w))
                  || !NILP (current_buffer->truncate_lines));
  int selective = (FIXNUMP (current_buffer->selective_display)
                   ? XINT (current_buffer->selective_display)
                   : ((!NILP (current_buffer->selective_display))
                      ? -1 : 0));
  int selective_e = (selective
                     && !NILP (current_buffer->selective_display_ellipses));
  /* The cursor should get placed at the last 'visible' character which
     is not necessarily the last displayed character which numchars
     keeps track of.  This variable stores that location. */
  int selective_numchars = -1;
  struct char_block *selective_cb = 0;
#ifdef I18N4
  wchar_t *overwrite_string, aa;
#else
  unsigned char *overwrite_string, aa;
#endif
  int overwrite_count;  
  int glyph_count = 0;
  int glyphs_added = 0;
  int loop;
  int char_width;
  int break_flag = 0;

  data.border = w->pixleft + LEFT_MARGIN (b, s, w);
  data.pixright = w->pixleft + w->pixwidth;
  data.text_glyphs_added = 0;
  data.text_glyph_width = 0;

  if (tab_width <= 0 || tab_width > 20) tab_width = 8;

  data.cb = l->body;
  replot_lines = s->replot_lines;
  l->shifted = *propagation && !l->new;	/* Don't shift for new lines */
  l->changed = (w->size_change || replot_lines || l->changed ||
		data.cb->xpos != data.border);
  l->subwindow = 0;
  data.cb->xpos = data.border;

  l->tabs = 0;
  data.beginc = 0;
  data.endc = 0;
  data.prev = 0;
  data.new_cpos = 0;
  cpos = 0;
  numchars = 0;
  data.lwidth = data.border;
  /* Left pixel edge of window */
  overwrite_count = 0;
  overwrite_string = 0;		/* null string */

  /* Mark this line as being "in display" */
  l->in_display = -1;
  
  if (w == XWINDOW(XSCREEN(w->screen)->minibuffer_window) && start == 1 && l->prev == 0)
    {
      if (!NILP(Vminibuf_prompt))
	{
	  l->lwidth = data.lwidth;
	  l->chars = numchars;
	  data.beginc = layout_string_inc (w,
					    XSTRING(Vminibuf_prompt)->data, 0,
					    (!truncate ? continuer_glyph
					     : truncator_glyph),
					    0,

					   /* #### hey, what is this?
					      layout_string_inc acts like
					      this arg is COLUMNS, not PIXELS!
					      -jwz

					    data.pixright,
					    */
					   s->width - 5, /* kludge!!! */
					   
					   l, 0, &hpos,
					    0, 0);
	  minibuf_prompt_width = hpos;
	}
      data.cb = l->body;
      for (i = 0; i < hpos; i++)
	{
	  /* Assume:  we are skipping over valid characters */
	  data.lwidth += data.cb->width;
	  numchars++;
	  data.prev = data.cb;
	  data.cb = data.cb->next;
	}
    }

  data.lfont = SCREEN_DEFAULT_FONT (s);
  data.asc = data.desc = 0;

  /* Adjust right logical border of window by width of "truncate"
   * character; this is either '\' or '$'
   */
  data.pixright -= (truncate
	       ? glyph_width (truncator_glyph, data.lfont)
	       : glyph_width (continuer_glyph, data.lfont));

  /*
   * Why is this shit with extent_cache_invalid necessary?  Beats the
   * hell out of me.  Something somewhere seems to not like working
   * with the first buffer position.
   */
  if (pos == b->text.begv || pos == 1)
    extent_cache_invalid = 1;
  data.display_info = glyphs_from_bufpos (s,current_buffer,w,pos,0,0,0,0,0,1);

  e_face = data.display_info->faceptr;
  button_start = data.display_info->run_pos_lower;
  button_end = data.display_info->run_pos_upper;

  if (data.display_info->begin_columns || data.display_info->end_columns)
    button_end = button_start;

  if (e_face)
    {
      /* Set font according to style's font (if there) */
      data.face = e_face;
      data.lfont = FACE_FONT (data.face);
      data.asc = max (data.asc, XFONT (data.lfont)->ascent);
      data.desc = max (data.desc, XFONT (data.lfont)->descent);
    }
  else
    data.face = 0;		/* Font unchanged... */

  /*
   * If window's left edge isn't at screen edge, draw window divider '|'
   * on left edge of window.
   */
  if (window_needs_vertical_divider (w))
    {
      e_face = data.face;
      data.face = &SCREEN_MODELINE_FACE(s);

      c = '|';

      if (!update_position (w, c, &data, l, glyph_prop, (int) data.beginc,
			    1, 0))
	goto done;

      data.face = e_face;

      /* Don't change pos; buffer position hasn't changed.
       * This also won't constitute the entire line being changed/trashed
       */
    }
  
  numchars = hpos;

  if (hscroll)
    {
      /* Window is horizontally scrolled.  Overwrite '$' over first
       * actually displayed character on the line, if appropriate.
       */
      aa = '$';
      overwrite_string = &aa;
      overwrite_count = 1;
    }

  /* Check if overlay arrow should be displayed on this line */
  if (MARKERP (Voverlay_arrow_position) 
      && current_buffer == XMARKER (Voverlay_arrow_position)->buffer
      && start == marker_position (Voverlay_arrow_position) 
      && STRINGP (Voverlay_arrow_string))
    {
#ifdef I18N4
      wchar_t *p = (wchar_t *) XSTRING (Voverlay_arrow_string)->data;
#else
      unsigned char *p = XSTRING (Voverlay_arrow_string)->data;
#endif
      int len = string_length (XSTRING (Voverlay_arrow_string));
      /* Overwrite first 'len' displayed characters with the overlay
       * arrow string.
       */
      overwrite_string = p;
      overwrite_count = len;
    }
  end = ZV;
  pause = pos;  

  /*
   * Layout line based on current contents and buffer contents.
   * Record position of earliest change.
   */
  while (data.lwidth <= data.pixright)
    {
      inc = 0;
      glyphs_added = 0;
      data.text_glyphs_added = 0;
      data.old_asc = data.old_desc = 0;
      if (pos == pause)
	{
	  if (pos == end)
	    {
	      if (pos == button_end && !eob_hack_flag)
		{
		  break_flag = 1;
		  eob_hack_flag = 1;
		}
	      else
		break;
	    }
	  if (pos == button_start || pos == button_end || button_start == -1)
	    {
	      if (pos == button_end || button_end == -1)
		{
		  int tmp_g_prop = *glyph_prop;
		  if (tmp_g_prop < 0)
		    tmp_g_prop = 0;

		  /* Possible change in text attribution.  Find appropriate
		   * zone, face, etc.
		   */
		  if (pos == b->text.begv || pos == 1)
		    extent_cache_invalid = 1;
		  if (pos != start)
		    data.display_info = glyphs_from_bufpos (s,current_buffer,w,
							    pos, 0,0,0,0,0,1);
		  e_face = data.display_info->faceptr;
		  button_start = data.display_info->run_pos_lower;
		  button_end = data.display_info->run_pos_upper;

		  if (e_face)
		    {
		      data.face = e_face;
		      data.lfont = FACE_FONT (data.face);
		      data.old_asc = data.asc; data.old_desc = data.desc;
		      data.asc = max (data.asc, XFONT (data.lfont)->ascent);
		      data.desc = max (data.desc, XFONT (data.lfont)->descent);
		    }
		  else
		    {
		      data.lfont = SCREEN_DEFAULT_FONT (s);
		      data.old_asc = data.asc; data.old_desc = data.desc;
		      data.asc = max(data.asc, XFONT (data.lfont)->ascent);
		      data.desc = max(data.desc, XFONT (data.lfont)->descent);
		      data.face = 0;
		    }

		  data.text_glyph_width = 0;
		  for (loop = 0; loop < 2; loop++)
		    {
		      int col_cnt, eflag;
		      Lisp_Object *class;

		      /*
		       * The end glyphs must/should be checked first to
		       * ensure a logical display ordering.
		       */
		      col_cnt = loop ? data.display_info->begin_columns
			: data.display_info->end_columns;
		      class = loop ? data.display_info->begin_class
			: data.display_info->end_class;
		      eflag = loop ? 1 : 0;

		      /* glyph_prop is the number of text glyphs that
			 were actually displayed on the previous line */
		      if (col_cnt <= tmp_g_prop)
			{
			  tmp_g_prop -= col_cnt;
			  col_cnt = 0;
			}

		      if (col_cnt)
			{
			  int i;
			  for (i = tmp_g_prop; i < col_cnt; i++)
			    {
			      struct extent *e;
			      struct face *t_face;
			      Lisp_Object t_lfont;

			      e = XEXTENT(class[i]);

			      if (EQ (Fextent_start_position (class[i]),
				      Fextent_end_position (class[i])))
				{
				  t_face = get_face_at_bufpos (s, b, pos);
				  t_lfont = FACE_FONT (t_face);
				}
			      else
				{
				  t_face = data.face;
				  t_lfont = data.lfont;
				}

			      if (EXTENT_GLYPH_LAYOUT_P (e, GL_TEXT))
				{
				  data.text_glyph_width +=
				    glyph_width (extent_glyph (e, eflag),
						 t_lfont);
				}
			      else
				{
				  Lisp_Object obj;
				  XSETEXTENT (glyph_list[glyph_count],
					      XEXTENT (class[i]));
				  glyph_pos[glyph_count] = eflag;
				  obj = glyph_list[glyph_count];
				  face_list[glyph_count] = t_face;

				  glyphs_added++;
				  glyph_count++;
				}
			    }
			}
		    }
		}
	    }

	  /* Find next point where attribution might change
	   * (or gap position, etc.)
	   */
	  pause = ZV;
	  if (button_start == button_end)
	    {
	      pause = button_end + 1;
	      button_start = button_end = -1;
	    }
	  else
	    {
	      if (pos < PT && PT < pause)
		pause = PT;
	      if (pos < GPT && GPT < pause)
		pause = GPT;
	      if (pos < button_start && button_start < pause)
		pause = button_start;
	      else if (pos < button_end && button_end < pause)
		pause = button_end;
	    }

	  the_pos = pos;
	}

      data.pos = pos;

      if (break_flag)
	{
	  c = '\n';
	  multi = 0;
	}
      else
	{
	  c = CHAR_AT (the_pos);
	  the_pos++;
	  multi = 0;
	  /* Print nobreakspace as a regular space.  This should really be
	     done using character maps but we don't have those yet. */
	  if (c == 0240) c = ' ';
	}
      ac[0] = c;
      char_width = text_width (data.lfont, ac, 1);

      /* '\n' is now checked first to make sure it gets handled
         regardless of the setting of ctl-arrow.  This means we can't
         make it print as something else.  Oh for character maps. */
      if (c == '\n')
	{
	  inc = 0;
	  invis = 0;
	  if (!break_flag)
	    {
	      while (pos < end
		     && selective > 0
		     && position_indentation(current_buffer, pos + 1) >= selective)
		{
		  invis = 1;
		  pos = find_next_newline(current_buffer, pos + 1, 1);
		  if (CHAR_AT (pos-1) == '\n')
		    pos--;
		  data.pos = pos;
		}
	    }
	  if (invis && selective_e && !break_flag)
	    {
	      int i;
	      selective_numchars = numchars;
	      selective_cb = data.cb;

	      /* Insert ' ...' */
	      c = ' ';
	      multi = 1;
	      if (numchars < 0)
		numchars++;
	      else
		{
		  if (overwrite_count && numchars < overwrite_count)
		    c = *overwrite_string++;

		  if (!update_position (w, c, &data, l, glyph_prop,
					replot_lines, 1, 0))
		    goto done;

		  cpos = data.new_cpos;
		  inc = 1;
		}
	      c = '.';
	      for (i = 0; i < 3; i++)
		{
		  if (numchars < 0)
		    numchars++;
		  else
		    {
		      if (overwrite_count && numchars < overwrite_count)
			c = *overwrite_string++;

		      if (!update_position (w, c, &data, l, glyph_prop,
				       replot_lines, 1, 0))
			goto done;

		      inc++;
		    }

		  c = '.';
		}
	      multi = 0;
	    }
	  else
	    {
	      /*
	       * We need to force the display of glyphs on empty lines.
	       * The first line updates 'text glyphs.  The second ensures
	       * that layout_margin_line will be called to update the rest.
	       */
	      if (!update_position (w, ' ', &data, l, glyph_prop, replot_lines,
				    1, 1))
		goto done;
	      glyph_count += glyphs_added;
	    }
	  numchars += inc;
	  inc = 0;

	  /* This done is different from the others in that it does
	     not represent a breakout due to EOL being reached
	     internally.  So we need to reset these variables to what
	     they would have been during a regular exit from the
	     loop. */
/*	  glyphs_added = 0; */
	  data.text_glyphs_added = 0;
	  data.old_asc = data.old_desc = 0;

	  goto done;
	}
#ifdef I18N4
      else if (char_width && c >= (wchar_t) printable_min)
#else
      else if (char_width && c >= (unsigned char) printable_min)
#endif
	{
	  if (numchars < 0)
	    numchars++;
	  else
	    {
	      if (overwrite_count && numchars < overwrite_count)
		c = *overwrite_string++;

	      if (!update_position (w, c, &data, l, glyph_prop, replot_lines,
				    1, 0))
		goto done;

	      cpos = data.new_cpos;
	      inc = 1;
	    }
	}
      else if (c == '\t')
	{
	  l->tabs = 1;	  
	  c = ' ';
	  inc = 0;
	  multi = 1;
	  cpos = 0;
	  do
	    {
	      if (numchars + inc < 0)
		;
	      else
		{
		  if (overwrite_count && numchars + inc < overwrite_count)
		    c = *overwrite_string++;

		  if (!update_position (w, c, &data, l, glyph_prop,
					replot_lines, 1, 0))
		    goto done;

		  cpos = cpos ? cpos : data.new_cpos;
		  c = ' ';
		}
	      inc++;
	    }
	  while ((numchars + inc + taboffset + hscroll - (hscroll > 0))
		 % tab_width);
	  multi = 0;
	}
      else if (c == (('M' & 037)) && selective == -1)
	{
	  pos = find_next_newline(current_buffer,pos,1);
	  if (CHAR_AT (pos-1) == '\n')
	    pos--;
	  data.pos = pos;
	  if (selective_e)
	    {
	      int i;

	      selective_numchars = numchars;
	      selective_cb = data.cb;
	      /* Insert ' ...' */
	      c = ' ';
	      multi = 1;
	      if (numchars < 0)
		numchars++;
	      else
		{
		  if (overwrite_count && numchars < overwrite_count)
		    c = *overwrite_string++;

		  if (!update_position (w, c, &data, l, glyph_prop,
					replot_lines, 1, 0))
		    goto done;

		  cpos = data.new_cpos;
		  inc = 1;
		}
	      c = '.';
	      for (i = 0; i < 3; i++)
		{
		  if (numchars < 0)
		    numchars++;
		  else
		    {
		      if (overwrite_count && numchars < overwrite_count)
			c = *overwrite_string++;

		      if (!update_position (w, c, &data, l, glyph_prop,
					    replot_lines, 1, 0))
			goto done;

		      inc++;
		      c = '.';
		    }
		}
	      multi = 0;
	    }
	  numchars += inc;
	  inc = 0;
	  goto done;
	}
      else if ((c < 040 && ctl_p) || c == 0177)
	{
	  multi = 1;
	  if (numchars < 0)
	    numchars++;
	  else
	    {
	      c1 = '^';
	      if (overwrite_count && numchars < overwrite_count)
		c1 = *overwrite_string++;

	      if (!update_position (w, c1, &data, l, glyph_prop, replot_lines,
				    1, 0))
		goto done;

	      cpos = data.new_cpos;
	      inc = 1;
	    }
	  if (numchars < 0)
	    numchars++;
	  else
	    {
	      if (c == 0177)
		c1 = '?';
	      else
		c1 = c ^ 0100;
	      if (overwrite_count && numchars < overwrite_count)
		c1 = *overwrite_string++;

	      if (!update_position (w, c1, &data, l, glyph_prop, replot_lines,
				    1, 0))
		goto done;

	      inc++;
	    }
	  multi = 0;
	}
      else if (c >= 0200 || c < 040 || !char_width)
	{
	  multi = 1;
	  c1 = '\\';
	  if (numchars < 0)
	    numchars++;
	  else
	    {
	      if (overwrite_count && numchars < overwrite_count)
		c1 = *overwrite_string++;

	      if (!update_position (w, c1, &data, l, glyph_prop, replot_lines,
				    1, 0))
		goto done;

	      cpos = data.new_cpos;
	      inc = 1;
	    }
	  if (numchars < 0)
	    numchars++;
	  else
	    {
	      c1 = (c>>6) + '0';
	      if (overwrite_count && numchars < overwrite_count)
		c1 = *overwrite_string++;

	      if (!update_position (w, c1, &data, l, glyph_prop, replot_lines,
				    1, 0))
		goto done;

	      inc++;
	    }
	  if (numchars < 0)
	    numchars++;
	  else
	    {
	      c1 = (7 & (c >>3)) + '0';
	      if (overwrite_count && numchars < overwrite_count)
		c1 = *overwrite_string++;

	      if (!update_position (w, c1, &data, l, glyph_prop, replot_lines,
				    1, 0))
		goto done;

	      inc++;
	    }
	  if (numchars < 0)
	    numchars++;
	  else
	    {
	      c1 = (7 & c) + '0';
	      if (overwrite_count && numchars < overwrite_count)
		c1 = *overwrite_string++;

	      if (!update_position (w, c1, &data, l, glyph_prop, replot_lines,
				    1, 0))
		goto done;

	      inc++;
	    }
	  multi = 0;
	}
      else
	{
	  if (numchars < 0)
	    numchars++;
	  else
	    {
	      if (overwrite_count && numchars < overwrite_count)
		c = *overwrite_string++;

	      if (!update_position (w, c, &data, l, glyph_prop, replot_lines,
				    1, 0))
		goto done;

	      cpos = data.new_cpos;
	      inc = 1;
	    }
	}
      if (pos == PT && point_vpos < 0)
	{
	  point_vpos = vpos;
	  point_hpos = numchars;
	  point_v = l;
	  point_h = cpos ? cpos : l->body;
	}      
      pos++;
      numchars += inc;
      if (data.lwidth >= data.pixright) break;
    }
 done:
  if (data.text_glyph_width)
    data.lwidth -= data.text_glyph_width;
  if (data.text_glyphs_added)
    *glyph_prop = data.text_glyphs_added;
  else if (data.text_glyph_width)
    *glyph_prop = -1;
  else
    *glyph_prop = 0;

  glyph_count -= glyphs_added;
  val.hpos = -XINT(w->hscroll);
  if (val.hpos) val.hpos++;

  if (data.old_asc)
    data.asc = data.old_asc;
  else if (data.asc == 0)
    data.asc = XFONT (SCREEN_DEFAULT_FONT (s))->ascent;
  if (data.old_desc)
    data.desc = data.old_desc;
  else if (data.desc == 0)
    data.desc = XFONT (SCREEN_DEFAULT_FONT (s))->descent;

  val.vpos = 1;
  lastpos = pos;

  if (hscroll && numchars <= 0 && lastpos != start && overwrite_count == 1)
    {
      /* Here we have a horizontally scrolled line with some text, but
       * none which was actually layed out.  Layout the '$' in the leftmost
       * position of the window if needed
       */
      e_face = data.face;
      if (l->modeline)
	data.face = &SCREEN_MODELINE_FACE(s);
      else
	data.face = &SCREEN_NORMAL_FACE(s);
      if (GLYPH_CB_F(data.cb,truncator_glyph) || (data.cb->face != data.face)
	  || data.beginc)
	{
	  add_text_glyph (w, truncator_glyph, &data, l, 0);
	  data.prev->e = NULL;
	  data.beginc = data.prev;
	}
      else
	{
	  next_char (w, &data, l, 0);
	}

      data.face = e_face;
      /* If cursor goes here, position it appropriately
       */
      if (PT >= start && PT <= pos)
	{
	  point_vpos = vpos;
	  point_hpos = 0;
	  point_v = l;
	  point_h = (window_needs_vertical_divider (w)
                     ? l->body->next
                     : l->body);
	  foo++;
	}
      overwrite_count = 0;
    }

  if (overwrite_count && numchars < overwrite_count &&
      overwrite_string != &aa)
    {
      /* Layout remainder of overlay arrow string, etc.
       * DON'T executed this code for a '$' at left edge of screen
       */
      unsigned char c259;

      if (numchars < 0) numchars = 0;
      
      while (numchars < overwrite_count)
	{
	  c259 = *overwrite_string++;

	  if (CHAR_CB_F(data.cb,c259) || (data.cb->face != data.face) || data.beginc)
	    {
	      add_char (c259, &data, l, 0);
	      data.beginc = data.beginc ? data.beginc : data.prev;
	    }
	  else
	    {
	      next_char (w, &data, l, 0);
	    }
	  numchars ++;
	}
      if (PT == pos)
	{
	  point_vpos = vpos;
	  point_v = l;
	  point_h = l->body;
	  point_hpos = 0;
	}
    }

  if (multi)
    {
      /* Continuation in middle of character */
      val.hpos -= inc;
    }
  
  if (data.lwidth > data.pixright)
    {
      pos--;
      data.lwidth -= data.cb->width;
      numchars--;
    }

  /* Decide where next line begins */
  if (pos < ZV)
    {
      if (CHAR_AT (pos) == '\n')
	{
	  pos++;
	}
      else
	{
	  /* Stopped due to right edge of window */
	  if (truncate)
	    {
	      foo++;
	      if (data.cb != l->end && GLYPH_CB_F(data.cb,truncator_glyph))
		{
		  t = l->end->prev;
		  free_char_blocks (data.cb, t);
		  data.prev->next = l->end;
		  l->end->prev = data.prev;
		  data.cb = l->end;
		}
	      e_face = data.face;
	      if (l->modeline)
		data.face = &SCREEN_MODELINE_FACE(s);
	      else
		data.face = &SCREEN_NORMAL_FACE(s);
	      if (GLYPH_CB_F(data.cb,truncator_glyph)
		  || (data.cb->face != data.face) || replot_lines)
		{
		  add_text_glyph (w, truncator_glyph, &data, l, 0);
		  data.prev->e = NULL;
		  data.beginc = data.beginc ? data.beginc : data.prev;
		  data.endc = data.prev;
		}
	      else
		{
		  next_char (w, &data, l, 0);
		}
	      data.face = e_face;
	      /* Truncating, so start next line after next newline.
	       * Point is on this line if it is before the newlinw.
	       */
	      pos = find_next_newline(current_buffer,pos,1);
	      val.hpos = XINT(w->hscroll) ? 1 - XINT(w->hscroll) : 0;
	      lastpos = pos - (CHAR_AT (pos -1) == '\n');
	    }
	  else
	    {
	      e_face = data.face;
	      if (l->modeline)
		data.face = &SCREEN_MODELINE_FACE(s);
	      else
		data.face = &SCREEN_NORMAL_FACE(s);

	      if (data.cb != l->end && GLYPH_CB_F(data.cb,continuer_glyph))
		{
		  /* Only do this if incrementally necessary */
		  t = l->end->prev;
		  free_char_blocks (data.cb, t);
		  data.prev->next = l->end;
		  l->end->prev = data.prev;
		  data.cb = l->end;
		}
	      if (GLYPH_CB_F(data.cb,continuer_glyph)
		  || (data.cb->face != data.face) || replot_lines)
		{
		  add_text_glyph (w, continuer_glyph, &data, l, 0);
		  data.prev->e = NULL;
		  data.beginc = data.beginc ? data.beginc : data.prev;
		  data.endc = data.prev;
		}
	      else
		{
		  next_char (w, &data, l, 0);
		}

	      data.face = e_face;
	      val.vpos = 0;
	      lastpos--;
	    }
	}
    }

  /* Possibility exists that we are clipping all/part of line; free
   * those blocks.
   */
  if (data.cb != l->end && l->end != l->body)
    {
      t = l->end->prev;
      if (l->body == data.cb)
	{
	  l->body = l->end;
	}
      free_char_blocks (data.cb, t);
      if (!data.beginc) data.beginc = l->end;
      l->end->prev = data.prev;
      if (data.prev) data.prev->next = l->end;
      l->changed = 1;
    }

  /* Update point if at EOL or in invisible text. */
  if (start <= PT && PT <= lastpos && point_vpos < 0)
    {
      point_vpos = vpos;
      if (selective_numchars >= 0)
	{
	  point_hpos = selective_numchars;
	  point_h = selective_cb;
	}
      else
	{
	  point_hpos = numchars;
	  point_h = data.cb;
	}
      point_v = l;
    }

  if (point_vpos == vpos)
    {
      if (point_hpos < 0)
	{
	  point_hpos = 0;
	  point_h = l->body;
	}
      /* hpos of point may not exist anymore. */
      if (point_hpos >= numchars && numchars >= 0)
	{
	  point_hpos = numchars;
	  point_h = l->end;

	  if (truncate && l->end->prev && foo)
	    point_h = l->end->prev;
	}
      if (w == XWINDOW (s->selected_window) && !cursor_in_echo_area)
	{
	  s->cursor_y = point_vpos;
	  if (w == XWINDOW (selected_window))
	    selected_point_vpos = point_vpos;
	  s->cursor_x = point_hpos;
	  s->new_cur_mir = find_window_mirror (w);
	  s->new_cur_line = point_v;
	  s->new_cur_char = point_h;

	  if (hpos == (XINT (w->hscroll) ? 1 - XINT(w->hscroll) : 0)
	      && val.vpos)
	    {
	      this_line_bufpos = start;
	      this_line_buffer = current_buffer;
	      this_line_start_hpos = hpos;
	      this_line_vpos = point_vpos;
	      this_line_line = l;
	      this_line_start_hpos = hpos;
	      this_line_endpos = Z - lastpos;
	    }
	  else
	    {
	      this_line_bufpos = 0;
	    }
	}
    }

  /*
   * Decide on propagation value to pass to lines dependent on this one.
   * Decide on what redisplay action to take for this line:  If propagation
   * is one, then entire line must be replotted; otherwise, attempt to start
   * at position indicated by "Changed" pointer.
   */
  
  *propagation = *propagation || (l->ascent != data.asc
				  || l->descent != data.desc);
  if (data.asc != l->ascent || data.desc != l->descent)
    l->changed = 1;
  if (l->ascent != data.asc || l->descent != data.desc || *propagation)
    {
      data.beginc = l->body; data.endc = l->end;
    }

  if (replot_lines || w->size_change || (l->body == l->end && l->new))
    {
      l->changed = 1;
      data.beginc = l->body;
      data.endc = 0;
    }

  l->ascent = data.asc;
  l->descent = data.desc;
  l->lwidth = data.lwidth;
  l->end->xpos = ((l->end->prev)
                  ? l->end->prev->xpos + l->end->prev->width
                  : w->pixleft + LEFT_MARGIN (b, s, w));
  l->chars = (numchars > 0) ? numchars : 0;

  /*
   * If line has changed or is new, push it onto stack for plotting
   */
  if (l->changed || l->shifted)
    {
      list_ptr++;
      BLOCK_TYPE(list_ptr) = BODY_LINE;
      BLOCK_LINE(list_ptr) = l;
      BLOCK_LINE_START(list_ptr) = data.beginc;
      BLOCK_LINE_END(list_ptr) = data.endc;
      BLOCK_LINE_CLEAR(list_ptr) = 1;
      if (l->in_display != -1)
	{
	  data.cb = l->body;
	  while (data.cb)
	    {
	      if (data.cb->xpos + data.cb->width >= l->in_display)
		BLOCK_LINE_START(list_ptr) = data.cb;
	      data.cb = data.cb->next;
	    }
	}
    }

  /*
   * The following adds in any glyphs which are on the line.  The current
   * policy is that if there isn't room, don't display it.  It must be
   * called after the body block is shoved onto the redisplay stack.
   */

  if (layout_margin_line(w,l,glyph_count))
    *propagation = 1;

  l->in_display = 0;
  val.bufpos = pos;
  val_layout_text_line = val;
  return &val_layout_text_line;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Do incremental layout of STRING for window W, using specified STYLE.
 * HPOS = starting horizontal position
 * TRUNCATE = truncate character (or 0)
 * MINCOL, MAXCOL = minimum start and maximum end column
 * L = line struct to modify
 * STYLE = style for display on line
 * CLIP = 0/1 -> Clip remaining contents of L once STRING is layed out
 * returns pointer to first character of L that has changed.
 * modeline is used to ignore the margin widths.
 */
static struct char_block *
layout_string_inc (struct window *w, CONST unsigned char *string, 
                   int hpos, GLYPH truncate, 
                   int mincol, int maxcol,
                   struct line_header *l, struct face *face,
                   int *ret, 
                   int clip,
		   int modeline)
{
  struct screen *s = XSCREEN(w->screen);
  struct layout_data data;
  int tab_width = XINT(current_buffer->tab_width);
  int hscroll = XINT(w->hscroll);
#ifdef I18N4
  wchar_t ac[2];
#else
  unsigned char ac[2];
#endif
  char replot_lines;
  struct char_block *t;
#ifdef I18N4
  wchar_t c = 0;
  wchar_t c1;
#else
  unsigned char c = 0;
  unsigned char c1;
#endif
  int i;
  int numchars;
  struct buffer *b = XBUFFER (w->buffer);
  Lisp_Object ctl_arrow = b->ctl_arrow;
  int ctl_p = !NILP (ctl_arrow);
  int printable_min = (FIXNUMP (ctl_arrow)
		       ? XINT (ctl_arrow)
		       : ((EQ (ctl_arrow, Qt) || EQ (ctl_arrow, Qnil))
			  ? 256 : 160));
  int char_width;
#ifdef I18N4
  wchar_t *wstring;

  safe_mbstowcs ((char *) string, &wc_buf);
  wstring = wc_buf.data;
#endif

  data.face = face;
  data.border = w->pixleft;
  data.pixright = w->pixleft + w->pixwidth;
  data.lwidth = 0;
  data.display_info = 0;

  if (!modeline)
    data.border += LEFT_MARGIN(b,s,w);

  if (tab_width <= 0 || tab_width > 20) tab_width = 8;

  if (face)
    data.lfont = FACE_FONT (face);
  else
    data.lfont = SCREEN_DEFAULT_FONT (s);

  if (EQ (data.lfont, Qnil))
    return 0;

  replot_lines = s->replot_lines;
  data.beginc = 0;
  data.endc = 0;
  data.cb = l->body;
  data.prev = 0;
  data.new_cpos = 0;
  numchars = 0;

  /* Adjust right logical window border depending on
   * width of "truncate" character.
   */
  data.pixright -= (truncate
	       ? glyph_width (truncator_glyph, data.lfont)
	       : glyph_width (continuer_glyph, data.lfont));

  if (maxcol >= 0 && mincol > maxcol)
    mincol = maxcol;

  /* #### hey, can this ever be true?  Because of the -= just above.  -jwz. */
  if (maxcol == SCREEN_PIXWIDTH(s)) maxcol = 0;

  data.asc = data.desc = 0;
  data.old_asc = data.old_desc = 0;
  data.text_glyphs_added = data.text_glyph_width = 0;

  if (hpos == 0 && window_needs_vertical_divider (w))
    {
      c = '|';
      if (CHAR_CB_F(data.cb,c) || data.cb->face != face)
	{
	  data.beginc = data.cb;
	  if (!add_char (c, &data, l, 1))
	    goto done;
	}
      else
	{
	  if (!next_char (w, &data, l, 1))
	    goto done;
	}
    }

  if (hpos > 0)
    {
      for (i = 0; i < hpos; i++)
	{
	  /* ASSUME:  We will always be skipping over valid chars */
	  data.prev = data.cb->prev;
	  data.cb = next_block(data.cb,l);
	  data.prev = data.cb;
	  data.cb = data.cb->next;
	  numchars++;
	}
      data.lwidth = data.prev->xpos + data.prev->width;
    }

  while (data.lwidth <= data.pixright)
    {
      if (maxcol > 0 && numchars >= maxcol) break;
#ifdef I18N4
      c = (wchar_t) *wstring++;
#else
      c = (unsigned char) *string++;
#endif
      if (c == 0240) c = ' ';	/* display nobreakspace as a regular space */
      ac[0] = c;
      char_width = text_width (data.lfont, ac, 1);

      if (!c) break;

#ifdef I18N4
      if (c >= (wchar_t) printable_min && char_width)
#else
      if (c >= (unsigned char) printable_min && char_width)
#endif
	{
	  if (CHAR_CB_F(data.cb,c) || (data.cb->face != face) || replot_lines)
	    {
	      if (!add_char (c, &data, l, 1))
		goto done;
	      data.beginc = data.beginc ? data.beginc : data.prev;
	    }
	  else
	    {
	      if (!next_char (w, &data, l, 1))
		goto done;
	    }
	  numchars++;
	}
      else if (c == '\t')
	{
	  l->tabs = 1;
	  c = ' ';
	  do
	    {
	      if (CHAR_CB_F(data.cb,c) || (data.cb->face != face)
		  || replot_lines)
		{
		  if (!add_char (c, &data, l, 1))
		    goto done;
		  data.beginc = data.beginc ? data.beginc : data.prev;
		}
	      else
		{
		  if (!next_char (w, &data, l, 1))
		    goto done;
		}
	      numchars++;
	    }
	  while ((numchars + hscroll - (hscroll > 0)) % tab_width);
	}
      else if ((c < 040 && ctl_p) || c == 0177)
	{
	  c1 = '^';	      
	  if (CHAR_CB_F(data.cb,c1) || (data.cb->face != face) || 
              replot_lines)
	    {
	      if (!add_char (c1, &data, l, 1))
		goto done;
	      data.beginc = data.beginc ? data.beginc : data.prev;
	    }
	  else
	    {
	      if (!next_char (w, &data, l, 1))
		goto done;
	    }
	  numchars++;
	  if (c == 0177)
	    c1 = '?';
	  else
	    c1 = c ^ 0100;
	  if (CHAR_CB_F(data.cb,c1) || (data.cb->face != face) || 
              replot_lines)
	    {
	      if (!add_char (c1, &data, l, 1))
		goto done;
	      data.beginc = data.beginc ? data.beginc : data.prev;
	    }
	  else
	    {
	      if (!next_char (w, &data, l, 1))
		goto done;
	    }
	  numchars++;
	}
      else if (c >= 0200 || c < 040 || !char_width)
	{
	  c1 = '\\';
	  if (CHAR_CB_F(data.cb,c1) || (data.cb->face != face) || replot_lines)
	    {
	      if (!add_char (c1, &data, l, 1))
		goto done;
	      data.beginc = data.beginc ? data.beginc : data.prev;
	    }
	  else
	    {
	      if (!next_char (w, &data, l, 1))
		goto done;
	    }
	  numchars++;
	  c1 = (c>>6) + '0';
	  if (CHAR_CB_F(data.cb,c1) || (data.cb->face != face) || replot_lines)
	    {
	      if (!add_char (c1, &data, l, 1))
		goto done;
	      data.beginc = data.beginc ? data.beginc : data.prev;
	    }
	  else
	    {
	      if (!next_char (w, &data, l, 1))
		goto done;
	    }
	  numchars++;
	  c1 = (7 & (c>>3)) + '0';
	  if (CHAR_CB_F(data.cb,c1) || (data.cb->face != face) || replot_lines)
	    {
	      if (!add_char (c1, &data, l, 1))
		goto done;
	      data.beginc = data.beginc ? data.beginc : data.prev;
	    }
	  else
	    {
	      if (!next_char (w, &data, l, 1))
		goto done;
	    }
	  numchars++;
	  c1 = (7 & c) + '0';
	  if (CHAR_CB_F(data.cb,c1) || (data.cb->face != face) || replot_lines)
	    {
	      if (!add_char (c1, &data, l, 1))
		goto done;
	      data.beginc = data.beginc ? data.beginc : data.prev;
	    }
	  else
	    {
	      if (!next_char (w, &data, l, 1))
		goto done;
	    }
	  numchars++;
	}
      else
	{
	  if (CHAR_CB_F(data.cb,c) || (data.cb->face != face) || replot_lines)
	    {
	      if (!add_char (c, &data, l, 1))
		goto done;
	      data.beginc = data.beginc ? data.beginc : data.prev;
	    }
	  else
	    {
	      if (!next_char (w, &data, l, 1))
		goto done;
	    }
	  numchars++;
	}
      if (data.lwidth >= data.pixright) break;
    }
  done:

  if (data.asc == 0)
    data.asc = XFONT (data.lfont)->ascent;
  if (data.desc == 0)
    data.desc = XFONT (data.lfont)->descent;

  if (data.lwidth > data.pixright)
    {
      data.lwidth -= data.cb->width;
      numchars--;
    }

  if (c)
    {
      if (truncate)
	{
	  add_text_glyph (w, truncator_glyph, &data, l, 0);
	  data.prev->e = NULL;
	  numchars++;
	  data.beginc = data.beginc ? data.beginc : data.prev;
	}
    }
  else if (mincol >= 0)
    {
      int space_width;

      ac[0] = ' ';
      space_width = text_width (data.lfont, ac, 1);

      c = ' ';
      while (numchars < mincol && (data.lwidth + space_width) < data.pixright)
	{
	  if (CHAR_CB_F(data.cb,c) || (data.cb->face != face))
	    {
	      add_char (c, &data, l, 0);
	      data.beginc = data.beginc ? data.beginc : data.prev;
	    }
	  else
	    {
	      next_char (w, &data, l, 0);
	    }
	  numchars++;
	}
    }

  if (clip && (data.cb != l->end))
    {
      t = l->end->prev;
      if (l->body == data.cb)
	{
	  l->body = l->end;
	}
      free_char_blocks (data.cb, t);
      l->end->prev = data.prev;
      if (data.prev) data.prev->next = l->end;
      l->changed = 1;
    }

  l->lwidth = data.lwidth;
  l->end->xpos = l->end->prev ? l->end->prev->xpos + l->end->prev->width
    : (modeline ? w->pixleft : w->pixleft + LEFT_MARGIN(b,s,w));
  l->changed = l->changed || (l->ascent != data.asc)
    || (l->descent != data.desc);
  l->ascent = max(l->ascent,data.asc); l->descent = max(l->descent, data.desc);
  l->chars = numchars;

  *ret = numchars;

  return data.beginc;
}

  /*
   * The current layout policy is hardcoded.  Textual annotations
   * ('text) have already been laid out at this point.  This leaves
   * 'outside-margin, 'inside-margin, and 'whitespace.  The policy is:
   *
   * layout 'whitespace  --  If use_left_overflow is true, the left
   *	outside margin can be used as extra whitespace.
   *
   * layout 'inside-margin  -- If use_left_overflow is true, the left
   *	outside margin can be used as extra whitespace if none of it was
   *	used by 'whitespace glyphs.
   *
   * layout 'outside-margin -- Any leftover whitespace in the outside
   *	margin can now be used to layout 'outside-margin glyphs.
   *
   * At the moment a glyph is not allowed to overlap the outside margin
   * border.  That is, it cannot be partially in the outside margin and
   * partially in the edit buffer. */

static void
add_margin_glyph (struct window *w, GLYPH glyph_id, unsigned short gwidth,
		  struct line_header *l, struct layout_data *data,
		  struct extent *e, int blank)
{
  struct screen *s = XSCREEN (w->screen);
  int wid;
  Lisp_Object lglyph = glyph_to_pixmap (glyph_id);

  if (gwidth)
    wid = gwidth;
  else
    wid = glyph_width (glyph_id, data->lfont);

  if ((PIXMAPP (lglyph) && PIXMAP_CONTRIB_P (lglyph))
      || SUBWINDOWP (lglyph))
    {
      unsigned short height = glyph_height (glyph_id, data->lfont);

      if (height > (data->asc + data->desc))
	data->asc += (height - data->asc - data->desc);
    }

  data->cb = next_margin_block (data->cb, l);
  data->cb->glyph = glyph_id;
  data->cb->char_b = FALSE;
  if (blank)
    data->cb->blank = TRUE;
  else
    data->cb->blank = FALSE;
  data->cb->width = wid;

  if ((data->lwidth < data->border) && (data->lwidth + wid > data->border))
    data->cb->xpos = data->lwidth = data->border;
  else
    data->cb->xpos = data->lwidth;

  data->cb->e = e;
  data->lwidth = data->cb->xpos + wid;
  data->cb->face = data->face;
  data->cb->prev = data->prev;
  data->prev = data->cb;
  data->endc = data->cb;
  data->cb = data->cb->next;

  if (SUBWINDOWP (glyph_to_pixmap (glyph_id)))
    MARK_SUBWINDOW_USAGE();
  l->changed = 1;
}

static void
next_glyph (struct window *w, unsigned short gwidth, struct line_header *l,
	    struct layout_data *data, struct extent *e)
{
  struct screen *s = XSCREEN (w->screen);
  int wid;
  Lisp_Object lglyph = glyph_to_pixmap (data->cb->glyph);

  if (gwidth)
    wid = gwidth;
  else
    wid = glyph_width (data->cb->glyph, data->lfont);

  if (data->lwidth != data->cb->xpos || data->cb->width != wid)
    {
      data->endc = data->cb;
      data->cb->width = wid;

      if ((data->lwidth < data->border) && (data->lwidth + wid > data->border))
        data->cb->xpos = data->lwidth = data->border;
      else
        data->cb->xpos = data->lwidth;

      data->beginc = data->beginc ? data->beginc : data->cb;
    }

  if ((PIXMAPP (lglyph) && PIXMAP_CONTRIB_P (lglyph))
      || SUBWINDOWP (lglyph))
    {
      unsigned short height = glyph_height (data->cb->glyph, data->lfont);
      if (height > (data->asc + data->desc))
	data->asc += (height - data->asc - data->desc);
    }

  if (SUBWINDOWP (glyph_to_pixmap (data->cb->glyph)))
    MARK_SUBWINDOW_USAGE();

  data->cb->e = e;
  data->lwidth = data->cb->xpos + data->cb->width;
  data->cb->prev = data->prev;
  data->prev = data->cb;
  data->cb = data->cb->next;
}

#define CALCULATE_NEEDED_INFO(type)\
  ws_cnt = ov_cnt = 0;\
  needed_ws = needed_ov = 0;\
  for (indice = glyph_count - 1; indice >= 0; indice--)\
    {\
      e = XEXTENT(glyph_list[indice]);\
      Fset_extent_property (glyph_list[indice], Qglyph_invisible, Qt);\
      if (EXTENT_GLYPH_LAYOUT_P (e, (type)))\
	{\
	  if (face_list[indice] == &SCREEN_NORMAL_FACE(s)\
	       && data.lwidth < data.border)\
	    face_list[indice] = &SCREEN_LEFT_MARGIN_FACE(s);\
          wid = glyph_width (extent_glyph (e, glyph_pos[indice]),\
			     FACE_FONT (face_list[indice]));\
	  if (!ov_cnt && needed_ws + wid <= avail_ws)\
	    {\
	      ws_cnt++;\
	      needed_ws += wid;\
	    }\
	  else if (needed_ov + wid <= avail_ov)\
	    {\
	      ov_cnt++;\
	      needed_ov += wid;\
	    }\
	}\
    }

static int
layout_margin_line (struct window *w, struct line_header *l, int glyph_count)
{
  struct screen *s = XSCREEN(w->screen);
  struct buffer *b = XBUFFER(w->buffer);
  struct extent *e;
  struct layout_data data;

  int propagation = 0;
  int needed_ws, needed_ov, avail_ws, avail_ov;
  int ws_cnt, ov_cnt, wid, indice, loop;
  int w_start, i_start, o_start, t_start;
  int w_cnt, i_cnt, o_cnt;
  int mwidth = w->pixleft;
  struct char_block *tb;
  int type, start, next_start, count;
  int hscroll = XINT (w->hscroll);

  data.border = w->pixleft + LEFT_MARGIN(b,s,w);
  data.asc = l->ascent;
  data.desc = l->descent;
  data.old_desc = data.old_asc = 0;
  data.face = 0;
  data.lfont = Qnil;

  /* Calculate 'whitespace glyph info. */

  if (hscroll)
    {
      avail_ws = 0;
      tb = l->end;
    }
  else
    {
      tb = l->body;
      while (tb != l->end && tb->char_b && tb->ch == ' ') tb = tb->next;
      if (tb == l->end)
	{
	  /* whitespace glyphs are only allowed in whitespace
             explicitly created by the user, not in whitespace after a
             newline. */
	  if (l->body == l->end)
	    avail_ws = 0;
	  else
	    avail_ws = tb->xpos - data.border;
	}
      else
	avail_ws = tb->xpos - data.border;
    }
  t_start = tb->xpos;

  if (tb == l->body)
    data.endc = l->body;
  else
    data.endc = tb->prev;

  if (!EQ(b->use_left_overflow,Qnil))
    avail_ov = LEFT_MARGIN (b,s,w);
  else
    avail_ov = 0;

  ws_cnt = ov_cnt = 0;
  needed_ws = needed_ov = 0;
  for (indice = glyph_count - 1; indice >= 0; indice--)
    {
      e = XEXTENT(glyph_list[indice]);
      Fset_extent_property (glyph_list[indice], Qglyph_invisible, Qt);
      if (EXTENT_GLYPH_LAYOUT_P (e, GL_WHITESPACE))
	{
	  /* whitespace glyphs can't be in the margin area */
#if 0
	  if (face_list[indice] == &SCREEN_NORMAL_FACE(s)
	      && data.lwidth < data.border)
	    face_list[indice] = &SCREEN_LEFT_MARGIN_FACE(s);
#endif
          wid = glyph_width (extent_glyph (e, glyph_pos[indice]),
			     FACE_FONT (face_list[indice]));
	  if (!ov_cnt && needed_ws + wid <= avail_ws)
	    {
	      ws_cnt++;
	      needed_ws += wid;
	    }
	  else if (needed_ov + wid <= avail_ov)
	    {
	      ov_cnt++;
	      needed_ov += wid;
	    }
	}
    }

  /*
   * Determine starting pixel position of 'whitespace glyphs and the
   * number of them which will get displayed.
   */

  if (ov_cnt)
    {
      w_start = data.border - needed_ov;
      w_cnt = ov_cnt + ws_cnt;
    }
  else if (ws_cnt)
    {
      w_start = data.border + avail_ws - needed_ws;
      w_cnt = ws_cnt;
    }
  else
    {
      w_start = data.border + avail_ws;
      w_cnt = 0;
    }

  /* Calculate 'inside-margin glyph info. */

  avail_ws = w_start - data.border;
  if (avail_ws < 0)
    {
      avail_ov = LEFT_MARGIN(b,s,w) + avail_ws;
      avail_ws = 0;
    }
  else if (!EQ(b->use_left_overflow,Qnil))
    {
      avail_ov = LEFT_MARGIN(b,s,w);
    }
  else
    avail_ov = 0;

  CALCULATE_NEEDED_INFO (GL_INSIDE_MARGIN);

  /*
   * Determine starting pixel position of 'inside-margin glyphs and the
   * number of them which will get displayed.
   */

  if (ov_cnt)
    {
      if (w_start < data.border)
	{
	  i_start = w_start - needed_ov;
	  i_cnt = ov_cnt;
	}
      else
	{
	  i_start = data.border - needed_ov;
	  i_cnt = ov_cnt + ws_cnt;
	}
    }
  else if (ws_cnt)
    {
      i_start = data.border;
      i_cnt = ws_cnt;
    }
  else
    {
      i_start = min (w_start, data.border);
      i_cnt = 0;
    }

  /* Calculate 'outside-margin glyph info. */

  avail_ws = LEFT_MARGIN (b,s,w);
  if (i_start < data.border)
    avail_ws += (i_start - data.border);
  avail_ov = 0;

  CALCULATE_NEEDED_INFO (GL_OUTSIDE_MARGIN);

  /*
   * Determine starting pixel position of 'inside-margin glyphs and the
   * number of them which will get displayed.
   */

  o_start = w->pixleft;
  o_cnt = ws_cnt;

  /*
   * We now have the number of glyphs which are to get displayed, and
   * the starting positions of each glyph type.  These starting
   * positions are guaranteed not to overlap.  We can now layout each
   * glyph type in the order they normally appear on the screen.
   */

  if (!(w_cnt + i_cnt + o_cnt))
    {
      if (l->margin_start != l->margin_end)
	{
	  free_char_blocks (l->margin_start, l->margin_end->prev);
	  l->margin_start = l->margin_end;
	  l->margin_start->next = l->margin_start->prev = 0;
	  l->changed = 1;
	  l->margin_start->xpos = w->pixleft;
	  l->mwidth = w->pixleft;
	  list_ptr++;

	  /* Clear the outside margin area. */
	  BLOCK_TYPE(list_ptr) = MARGIN_LINE;
	  BLOCK_LINE(list_ptr) = l;
	  BLOCK_LINE_START(list_ptr) = 0;
	  BLOCK_LINE_END(list_ptr) = 0;
	  BLOCK_LINE_CLEAR(list_ptr) = 1;

	  /* Clear the inside margin area if needed. */
	  if (tb != l->body)
	    {
	      BLOCK_TYPE(list_ptr) = BODY_LINE;
	      BLOCK_LINE(list_ptr) = l;
	      BLOCK_LINE_START(list_ptr) = l->body;
	      BLOCK_LINE_END(list_ptr) = data.endc;
	      BLOCK_LINE_CLEAR(list_ptr) = 1;
	    }
	}
      else
	{
	  l->margin_start->xpos = w->pixleft;
	  l->mwidth = w->pixleft;
	}
      return 0;
    }

  data.cb = l->margin_start;
  data.prev = data.endc = data.beginc = data.new_cpos = 0;

  for (loop = 0; loop < 3; loop++)
    {
      switch (loop)
	{
	case 0:
	  {
	    count = o_cnt;
	    start = o_start;
	    next_start = i_start;
	    type = GL_OUTSIDE_MARGIN;
	  }
	  break;
	case 1:
	  {
	    count = i_cnt;
	    start = i_start;
	    next_start = w_start;
	    type = GL_INSIDE_MARGIN;
	  }
	  break;
	case 2:
	  {
	    count = w_cnt;
	    start = w_start;
	    next_start = t_start;
	    type = GL_WHITESPACE;
	  }
	  break;
	default:
	  abort ();
	}

      indice = 0;
      data.lwidth = start;
      while (count)
	{
	  int eflag = glyph_pos[indice];
	  e = XEXTENT(glyph_list[indice]);
	  Fset_extent_property (glyph_list[indice], Qglyph_invisible, Qnil);
	  if (EXTENT_GLYPH_LAYOUT_P (e, type))
	    {
	      count--;

	      data.face = face_list[indice];
	      data.lfont = FACE_FONT (data.face);

	      if (GLYPH_CB_F(data.cb, extent_glyph (e, eflag))
		  || data.cb->face != data.face
		  || data.beginc
		  || l->changed)
		{
		  add_margin_glyph (w, extent_glyph (e, eflag), 0, l, &data,
				    e, 0);
		  data.beginc = data.beginc ? data.beginc : data.prev;
		}
	      else
		{
		  next_glyph (w, 0, l, &data, e);
		}
	      if (data.prev->xpos < data.border)
		mwidth += wid;
	    }
	  indice++;
	}

      if (next_start < data.lwidth)
	abort ();	/* Something is screwed up in the algorithm. */

      if (((data.lwidth < data.border && next_start != data.border) ||
	   (data.lwidth >= data.border))
	  && data.lwidth != next_start)
	{
	  if (data.lwidth < data.border)
	    data.face = &SCREEN_LEFT_MARGIN_FACE(s);
	  else
	    data.face = &SCREEN_NORMAL_FACE(s);

	  data.lfont = FACE_FONT (data.face);

	  if (data.cb->blank || data.cb->face != data.face || data.beginc
	      || l->changed)
	    {
	      add_margin_glyph (w, -1, next_start - data.lwidth, l, &data,
				0, 1);
	      data.beginc = data.beginc ? data.beginc : data.prev;
	    }
	  else
	    {
	      next_glyph (w, next_start - data.lwidth, l, &data, 0);
	    }
	  if (data.prev->xpos < data.border)
	    mwidth += wid;
	}
    }

  if (data.cb != l->margin_end && l->margin_end != l->margin_start)
    {
      tb = l->margin_end->prev;
      if (l->margin_start == data.cb)
	  l->margin_start = l->margin_end;
      free_char_blocks (data.cb, tb);
      if (!data.beginc) data.beginc = l->margin_end;
      l->margin_end->prev = data.prev;
      if (data.prev) data.prev->next = l->margin_end;
      l->changed = 1;
    }

  propagation = (l->ascent != data.asc);
  l->changed = l->changed || propagation;
  l->ascent = max (l->ascent, data.asc);
  l->mwidth = mwidth;
  l->margin_end->xpos = l->margin_end->prev ? l->margin_end->prev->xpos +
    l->margin_end->prev->width : w->pixleft;

  if (l->changed || l->shifted)
    {
      list_ptr++;
      BLOCK_TYPE(list_ptr) = MARGIN_LINE;
      BLOCK_LINE(list_ptr) = l;
      BLOCK_LINE_START(list_ptr) = data.beginc;
      BLOCK_LINE_END(list_ptr) = data.endc;
      BLOCK_LINE_CLEAR(list_ptr) = 1;
    }

  return propagation;
}



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Layout the modeline for window W.  Unless circumstance are otherwise,
 * attempt to do incrementally - redisplaying only what has changed.
 */
static void
layout_mode_line (struct window *w)
{
  struct screen *s = XSCREEN(w->screen);
  int left = 0;
  int right;
  struct line_header *mode = window_display_modeline (w);
  struct line_header *new = get_line();
  struct char_block *cb,*cb2,*cb3,*end;

  if (!window_display_modeline (w))
    {
      /* Create mode line space 1st time for window */
      mode = get_line();
    }

  /* Prevent expose code from accessing modeline structures while we are
   * updating it.
   */
  find_window_mirror(w)->modeline = NULL;

/*  
  if (mode_line_inverse_video)
    mode_face = &SCREEN_HIGHLIGHT_FACE(s);
  else
    mode_face = &SCREEN_NORMAL_FACE(s);
*/
  mode_face = &SCREEN_MODELINE_FACE (s);
  right = w->pixwidth / (int) XFONT (FACE_FONT (mode_face))->width;
  new->ascent = 0; new->descent = 0;
  layout_mode_element (w, left, 0, right, right,
		       current_buffer->mode_line_format, new, 1);

  if (new->end->xpos < (w->pixleft + w->pixwidth))
    {
      cb = get_char_block();
      cb->ch = 0;
      cb->char_b = TRUE;
      cb->blank = TRUE;
      cb->width = w->pixleft + w->pixwidth - new->end->xpos;
      cb->face = mode_face;
      cb->xpos = new->end->xpos;
      cb->prev = new->end->prev;
      cb->next = new->end;
      new->end->xpos = cb->xpos + cb->width;
      new->end->prev->next = cb;
      new->end->prev = cb;
      new->lwidth += cb->width;
    }

  new->changed = 0;
  mode->prevy = mode->ypos;
  mode->ypos = w->pixtop + w->pixheight - mode->descent;
  new->ypos = w->pixtop + w->pixheight - new->descent;
  if ((new->ascent != mode->ascent) || (new->descent != mode->descent) ||
      (mode->prevy != mode->ypos) || (new->lwidth != mode->lwidth) ||
      s->replot_lines)
    {
      cb = new->body;
      end = 0;
      goto endit;
    }
  cb = new->body; cb2 = mode->body;
  while (1)
    {
      if (cb == new->end || cb2 == mode->end) break;
      if ((cb->xpos != cb2->xpos) ||
	  (cb->char_b != cb2->char_b) ||
	  (cb->char_b && (cb->ch != cb2->ch)) ||
	  (!cb->char_b && (cb->glyph != cb2->glyph)) ||
	  (cb->width != cb2->width) ||
	  (cb->face != cb2->face))
	{
	  /* Find end of changed region */
	  end = cb;
	  cb3 = cb->next; cb2 = cb2->next;
	  while (1)
	    {
	      if (cb3 == new->end || cb2 == mode->end) break;
	      if ((cb3->xpos != cb2->xpos) ||
		  (cb3->char_b != cb2->char_b) ||
		  (cb3->char_b && (cb3->ch != cb2->ch)) ||
		  (!cb3->char_b && (cb3->glyph != cb2->glyph)) ||
		  (cb3->face != cb2->face) ||
		  (cb3->width != cb2->width))
		{
		  end = cb3;
		}
	      if (cb2->next == NULL)
		abort ();
	      cb3 = cb3->next; cb2 = cb2->next;
	    }
	  break;
	}
      cb = cb->next; cb2 = cb2->next;
    }
 endit:
  
  if (cb != new->end)
    {
      /* Set window's modeline to reference new structure */
      find_window_mirror(w)->modeline = new;
      free_line(mode);		/* Junk old contents */
      
      new->changed = 1;
      new->modeline = 1;
      new->shifted = 0;
      if (list_ptr > -1 && BLOCK_TYPE(list_ptr) == AREA)
	{
	  BLOCK_AREA_BOTTOM(list_ptr) = (w->pixtop + w->pixheight -
                                         (new->ascent + new->descent));
	}
      list_ptr++;
      BLOCK_TYPE(list_ptr) = BODY_LINE;
      BLOCK_LINE(list_ptr) = new;
      BLOCK_LINE_START(list_ptr) = cb;
      BLOCK_LINE_END(list_ptr) = end;
      BLOCK_LINE_CLEAR(list_ptr) = 0; 
    }
  else
    {
      /* Set window's modeline to reference old structure */
      find_window_mirror(w)->modeline = mode;
      free_line(new);		/* Junk new copy */
    }
}

/* Contribute ELT to the mode line for window W.
   How it translates into text depends on its data type.
 
   HPOS is the position (absolute on screen) where this element's text
   should start.  The output is truncated automatically at the right
   edge of window W.
 
   DEPTH is the depth in recursion.  It is used to prevent
   infinite recursion here.
 
   MINENDCOL is the hpos before which the element may not end.
   The element is padded at the right with spaces if nec
   to reach this column.
 
   MAXENDCOL is the hpos past which this element may not extend.
   If MINENDCOL is > MAXENDCOL, MINENDCOL takes priority.
   (This is necessary to make nested padding and truncation work.)
 
   Returns the hpos of the end of the text generated by ELT and the
   first changed block in the line.
 
   FN is a function which takes the same args as display_string:
   (w, string, hpos, truncate, mincol, maxcol, line_header, this_face,
    ret, clip, modeline)
   It is used to actually output the data.
 */

static int
layout_mode_element (struct window *w, int hpos, int depth, int minendcol,
                     int maxendcol, Lisp_Object elt,
		     struct line_header *l, int modeline)
{
  register struct screen *s = XSCREEN (WINDOW_SCREEN (w));

 tail_recurse:
  if (depth > 10)
    goto invalid;

  depth++;

  if (STRINGP (elt))
    {
      /* A string: output it and check for %-constructs within it.  */
      unsigned char c;
      unsigned char *this = XSTRING (elt)->data;

      while (hpos < maxendcol && *this)
        {
          unsigned char *last = this;
          while ((c = *this++) != '\0' && c != '%')
            ;
          if (this - 1 != last)
            {
              int lim = --this - last + hpos;
	      if (modeline)
		layout_string_inc (w, last, hpos, 0, hpos,
				   min (lim, maxendcol), l,
				   &SCREEN_MODELINE_FACE (s), &hpos, 1, 1);
	      else
		hpos = screen_title_display_string (w, last, hpos, hpos,
						    min (lim, maxendcol));
            }
          else /* c == '%' */
            {
              int spec_width = 0;
              /* We can't allow -ve args due to the "%-" construct.
               * Argument specifies minwidth but not maxwidth
               * (maxwidth can be specified by
               * (<negative-number> . <stuff>) mode-line elements)
               */
	      while ((c = *this++, isdigit (c)))
                {
                  spec_width = spec_width * 10 + (c - '0');
                }

              spec_width += hpos;
              if (spec_width > maxendcol)
                spec_width = maxendcol;

              if (c == 'M')
                {
                  hpos = layout_mode_element (w, hpos, depth, spec_width,
                                              maxendcol, Vglobal_mode_string,
					      l, modeline);
                }
              else if (c != 0)
                {
		  if (modeline)
		    layout_string_inc (w,
				       decode_mode_spec (w,c,spec_width -hpos),
				       hpos, 0, spec_width, maxendcol, l,
				       &SCREEN_MODELINE_FACE (s), &hpos, 1, 1);
		  else
		    hpos = screen_title_display_string (w,
							decode_mode_spec
							(w,c,spec_width -hpos),
							hpos, spec_width,
							maxendcol);
                }
            }
        }
    }
  else if (SYMBOLP (elt))
    {
      /* A symbol: process the value of the symbol recursively
	 as if it appeared here directly.  Avoid error if symbol void.
	 Special case: if value of symbol is a string, output the string
	 literally.  */
      Lisp_Object tem;
      tem = Fboundp (elt);
      if (!NILP (tem))
        {
          tem = Fsymbol_value (elt);
          /* If value is a string, output that string literally:
             don't check for % within it.  */
          if (STRINGP (tem))
            {
	      if (modeline)
		layout_string_inc (w, XSTRING(tem)->data, hpos, 0, minendcol,
				   maxendcol, l, &SCREEN_MODELINE_FACE (s),
				   &hpos, 1, 1);
	      else
		hpos = screen_title_display_string (w, XSTRING (tem)->data,
						    hpos, minendcol,
						    maxendcol);
            }
          /* Give up right away for nil or t.  */
          else if (!EQ (tem, elt))
          {
            elt = tem; 
            goto tail_recurse; 
          }
        }
    }
  else if (CONSP (elt))
    {
      /* A cons cell: three distinct cases.
       * If first element is a string or a cons, process all the elements
       * and effectively concatenate them.
       * If first element is a negative number, truncate displaying cdr to
       * at most that many characters.  If positive, pad (with spaces)
       * to at least that many characters.
       * If first element is a symbol, process the cadr or caddr recursively
       * according to whether the symbol's value is non-nil or nil.
       */
      Lisp_Object car, tem;
      car = XCONS (elt)->car;
      if (SYMBOLP (car))
        {
          tem = Fboundp (car);
          elt = XCONS (elt)->cdr;
          if (!CONSP (elt))
            goto invalid;
          /* elt is now the cdr, and we know it is a cons cell.
             Use its car if CAR has a non-nil value.  */
          if (!NILP (tem))
            {
              tem = Fsymbol_value (car);
              if (!NILP (tem))
                {
                  elt = XCONS (elt)->car; 
                  goto tail_recurse; }
            }
          /* Symbol's value is nil (or symbol is unbound)
           * Get the cddr of the original list
           * and if possible find the caddr and use that.
           */
          elt = XCONS (elt)->cdr;
          if (NILP (elt))
            ;
          else if (!CONSP (elt))
            goto invalid;
          else
            {
              elt = XCONS (elt)->car;
              goto tail_recurse;
            }
        }
      else if (FIXNUMP (car))
        {
          int lim = XINT (car);
          elt = XCONS (elt)->cdr;
          if (lim < 0)
            /* Negative int means reduce maximum width.
             * DO NOT change MINENDCOL here!
             * (20 -10 . foo) should truncate foo to 10 col
             * and then pad to 20.
             */
            maxendcol = min (maxendcol, hpos - lim);
          else if (lim > 0)
            {
              /* Padding specified.  Don't let it be more than
               * current maximum.
               */
              lim += hpos;
              if (lim > maxendcol)
                lim = maxendcol;
              /* If that's more padding than already wanted, queue it.
               * But don't reduce padding already specified even if
               * that is beyond the current truncation point.
               */
              if (lim > minendcol)
                minendcol = lim;
            }
          goto tail_recurse;
        }
      else if (STRINGP (car) || CONSP (car))
        {
          int limit = 50;
          /* LIMIT is to protect against circular lists.  */
          while (CONSP (elt) && --limit > 0
                 && hpos < maxendcol)
            {
              hpos = layout_mode_element (w, hpos, depth, hpos, maxendcol,
                                          XCONS(elt)->car, l, modeline);
              elt = XCONS (elt)->cdr;
            }
        }
    }
  else
    {
    invalid:
      if (modeline)
	layout_string_inc (w, (unsigned char *) GETTEXT ("*invalid*"), hpos,
			   0, minendcol, maxendcol, l,
			   &SCREEN_MODELINE_FACE (s), &hpos, 1, 1);
      else
	return (screen_title_display_string (w, (unsigned char *) "*invalid*",
					     hpos, minendcol, maxendcol));
    }

  if (minendcol > hpos)
    {
      if (modeline)
	layout_string_inc (w, (unsigned char *) "", hpos, 0, minendcol, -1, l,
			   &SCREEN_MODELINE_FACE (s), &hpos, 0, 1);
      else
	hpos = screen_title_display_string (w, (unsigned char *) "", hpos,
					    minendcol, -1);
    }
  return hpos;
}
     
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Return a string for the output of a mode line %-spec
   for window W, generated by character C and width MAXWIDTH.  */

static char mode_buf[300];

static CONST char lots_of_dashes[] = "----------------------------------------------\
------------------------------------------------------------------------------\
------------------------------------------------------------------------------\
-----------------------------------------------------------------------------";

static unsigned char *
decode_mode_spec (struct window *w, char c, int maxwidth)
{
  Lisp_Object obj = Qnil;
  struct screen *s = XSCREEN (WINDOW_SCREEN (w));
  CONST char *str = "";
  struct buffer *buffer = XBUFFER (w->buffer);

  if (maxwidth > SCREEN_WIDTH (s))
    maxwidth = SCREEN_WIDTH (s);

  switch (c)
    {
    case 'b': 
      obj = buffer->name;
      break;

    case 'f': 
      obj = buffer->filename;
      break;

    case 'l':
      str = window_line_number (w);
      break;

    case 'm': 
      obj = buffer->mode_name;
      break;

    case 'n':
      if (BUF_BEGV (buffer) > BUF_BEG (buffer)
	  || BUF_ZV (buffer) < BUF_Z (buffer))
        str = GETTEXT (" Narrow");
      break;

    case '*':
      str = (!NILP (buffer->read_only)
	     ? "%"
	     : ((MODIFF > buffer->save_modified)
		? "*"
		: "-"));
      break;

      /* lemacs change, for Kyle and VM, so that a folder can be read-only
	 and yet still indicate whether it is modified in the modeline. */
    case '+':
      str = ((MODIFF > buffer->save_modified)
	     ? "*"
	     : "-");
      break;

    case 's':
      /* status of process */
      obj = Fget_buffer_process (w->buffer);
      if (NILP (obj))
	str = GETTEXT ("no process");
      else
        obj = Fsymbol_name (Fprocess_status (obj));
      break;

    case 'S':
      /* name of screen */
#ifdef MULTI_SCREEN
      obj = s->name;
#endif
      break;

    case 't':			/* indicate TEXT or BINARY */
#ifdef MSDOS
      mode_buf[0] = NILP (current_buffer->buffer_file_type) ? "T" : "B";
      mode_buf[1] = 0;
      return ((unsigned char *) mode_buf);
#else /* not MSDOS */
      return ((unsigned char *) "T");
#endif /* not MSDOS */

    case 'p':
      {
	int pos = marker_position (w->start);
	int total = BUF_ZV (buffer) - BUF_BEGV (buffer);

	if (w->window_end_pos <= BUF_Z (buffer) - BUF_ZV (buffer))
	  {
	    if (pos <= BUF_BEGV (buffer))
	      str = GETTEXT ("All");
	    else
	      str = GETTEXT ("Bottom");
	  }
	else if (pos <= BUF_BEGV (buffer))
	  str = GETTEXT ("Top");
	else
	  {
	    total = ((pos - BUF_BEGV (buffer)) * 100 + total - 1) / total;
	    /* We can't normally display a 3-digit number,
	       so get us a 2-digit number that is close.  */
	    if (total == 100)
	      total = 99;
	    sprintf (mode_buf, "%2d%%", total);
	    return (unsigned char *) mode_buf;
	  }
        break;
      }

      /* Display percentage of size above the bottom of the screen.  */
    case 'P':
      {
	int toppos = marker_position (w->start);
	int botpos = BUF_Z (buffer) - w->window_end_pos;
	int total = BUF_ZV (buffer) - BUF_BEGV (buffer);

	if (botpos >= BUF_ZV (buffer))
	  {
	    if (toppos <= BUF_BEGV (buffer))
	      str = GETTEXT ("All");
	    else
	      str = GETTEXT ("Bottom");
	  }
	else
	  {
	    total = ((botpos - BUF_BEGV (buffer)) * 100 + total - 1) / total;
	    /* We can't normally display a 3-digit number,
	       so get us a 2-digit number that is close.  */
	    if (total == 100)
	      total = 99;
	    if (toppos <= BUF_BEGV (buffer))
	      sprintf (mode_buf, "Top%2d%%", total);
	    else
	      sprintf (mode_buf, "%2d%%", total);
	    return (unsigned char *) mode_buf;
	  }
        break;
      }

    case '%':
      str = "%";
      break;

    case '[': 
      {
	int i;
	char *p;

	if (command_loop_level > 5)
        {
	  str = "[[[... ";
          break;
        }
	p = mode_buf;
	for (i = 0; i < command_loop_level; i++)
	  *p++ = '[';
	*p = 0;
	return (unsigned char *) mode_buf;
      }

    case ']': 
      {
	int i;
	char *p;

	if (command_loop_level > 5)
        {
	  str = " ...]]]";
          break;
        }
	p = mode_buf;
	for (i = 0; i < command_loop_level; i++)
	  *p++ = ']';
	*p = 0;
	return (unsigned char *) mode_buf;
      }

    case '-':
      {
	register char *p;
        int i;
	
	if (maxwidth < countof (lots_of_dashes))
	  return ((unsigned char *) lots_of_dashes);
	else
	  {
            if (maxwidth >= countof (mode_buf))
              maxwidth = countof (mode_buf) - 1;
	    for (p = mode_buf, i = maxwidth; i > 0; i--)
	      *p++ = '-';
	    *p = '\0';
	  }
	return (unsigned char *) mode_buf;
      }
    }

  if (STRINGP (obj))
    return (XSTRING (obj)->data);
  else
    /* Yeah, yeah, casting const to non-const */
    return ((unsigned char *) str);
}



/* Line numbers
   Remember that this stuff needs to be window-local, not buffer-local, and
   that the post-command-hook is no good because multiple redisplays might
   happen without coming back to top-level.

   This is a first pass, and isn't terribly efficient.

   Some day this code should be made efficient enough that the scrollbar code
   can use it, and display scrollbar thumbs based on lines rather than chars.
 */

static char window_line_number_buf [100];
static int previous_line_number;

static char *
window_line_number (struct window *w)
{
  if (line_number_mode)
    {
      struct buffer *b = XBUFFER (w->buffer);
      int end = ((w == XWINDOW (XSCREEN (w->screen)->selected_window))
		 ? BUF_PT (b)
		 : marker_position (w->pointm));
      int lots = 999999;
      int shortage, line;
      scan_buffer (b, '\n', end, -lots, &shortage, 0);
      line = lots - shortage + 1;
      /* Apparently the previous line crap is no longer needed.  I
         believe it was needed before I fixed a bug with the calling
         of redisplay within the scrollbar callback handler.  In other
         words this was a bug fix for a bug caused by a bug. */
#if 0
      if (redisplay_lock)
	{
	  sprintf (window_line_number_buf, "%d", previous_line_number);
	}
      else
#endif
	{
	  sprintf (window_line_number_buf, "%d", line);
	  previous_line_number = line;
	}
    }
  else
    {
      *window_line_number_buf = 0;
      previous_line_number = 0;
    }
  
  return (window_line_number_buf);
}


/* END NEW REDISPLAY FUNCTIONS */


/* START OLD REDISPLAY FUNCTIONS STILL IN USE */
static GLYPH measure_glyphs[80];

#define GLYPH_SET_VALUE(g, v) ((g) = (v))

/* Cache the state of glyphs_from_bufpos. */

#define ASCII_BEGIN_GLYPH 99	/* some random number */
#define ASCII_END_GLYPH 399	/* another random number */

extern int buffer_window_count (struct buffer *b, struct screen *s);

/* Return true iff:

   1.  the glyph is not a subwindow or
   2.  the glyph is a subwindow associated with the given screen AND
   	the given buffer is only displayed in 1 window or
	the given window is the most-recently used window
*/
static int
display_glyph_in_buffer (struct buffer *b, struct screen *s, struct window *w,
			 GLYPH g)
{
  struct Lisp_Subwindow *sw;

  if (!SUBWINDOWP (glyph_to_pixmap (g)))
    return 1;
  else
    sw = XSUBWINDOW (glyph_to_pixmap (g));

  if (XSCREEN (sw->screen) != s)
    return 0;
  else
    {
      /* buffer count shouldn't be able to be 0, but... */
      if (buffer_window_count (b, s) <= 1)
	return 1;
      else if (buffer_window_mru (w))
	return 1;
      else
	return 0;
    }
}

#define NO_CHAR_INFO(ch) (((ch)->width == 0) &&    \
			  ((ch)->rbearing == 0) && \
			  ((ch)->lbearing == 0))

/* #### this function duplicates text_width() and should be
   removed. */

static XCharStruct *
x_char_info (XFontStruct *font, unsigned int ch)
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

#ifdef I18N4
#define X_CHAR_WIDTH(fontset,ch) x_char_width (fontset, ch)
#else
#define X_CHAR_WIDTH(font,ch) ((font)->per_char 		    \
			       ? x_char_info ((font), (ch))->width  \
			       : ((ch), (font)->min_bounds.width))
#endif

static struct glyphs_from_chars displayed_glyphs;

/* Determine how the character at POS in BUFFER would be displayed on
   SCREEN.  ONLY_NONCHARS means calculate only glyphs displayed around
   or instead of the characters, PIXEL_VALUES means return the pixel
   dimensions of the displayed glyphs.  Return value is a
   struct glyphs_from_chars pointer. */

struct glyphs_from_chars *
glyphs_from_bufpos (struct screen *screen, struct buffer *buffer,
		    struct window *window,
		    int pos, struct Lisp_Vector *dp,
                    int hscroll, register int columns,
                    int tab_offset, int direction, int only_nonchars)
{
  int pixel_values = SCREEN_IS_X (screen);
  register EXTENT_FRAGMENT extfrag;
  register struct face *fp;
#ifdef I18N4
  register wchar_t c;
#else
  register UCHAR c;
#endif
  register GLYPH *gp;
  int ignore_glyphs = 0;
  /* we will cut our "run" short if we discover that the
     frag ends with some end glyphs */
  int frag_has_end_glyphs;

 /* retry: */

  gp = measure_glyphs;
  frag_has_end_glyphs = 0;
  displayed_glyphs.glyphs = gp;

  /* Check if we can use our cached values */
  if (!extent_cache_invalid
      && buffer == last_buffer
      && screen == last_screen
      && BUF_MODIFF (buffer) == last_buffer_modiff
      && BUF_FACECHANGE (buffer) == last_buffer_facechange)
    {
      if (pos >= last_extfrag_from_pos
	  && pos < last_extfrag_to_pos)
	{
	  extfrag = last_buffer_extfrag;
	  fp = last_extfrag_face;
	  goto cache_used;
	}
      else if (pos == last_extfrag_to_pos)
	{
	  extfrag = buffer_extent_fragment_at (pos, buffer, screen, 0);
	  fp = extfrag->fp;
	  if (!fp)
	    fp = &SCREEN_NORMAL_FACE (screen);
	  last_extfrag_face = fp;
	  update_cache (buffer, extfrag);
	  goto cache_used;
	}
    }

  last_buffer = buffer;
  last_screen = screen;
  last_buffer_modiff = BUF_MODIFF (buffer);
  last_buffer_facechange = BUF_FACECHANGE (buffer);
  extfrag = buffer_extent_fragment_at (pos, buffer, screen, 0);
  last_buffer_extfrag = extfrag;
  last_extfrag_from_pos = extfrag->from;
  last_extfrag_to_pos = extfrag->to;
  fp = extfrag->fp;
  if (!fp)
    fp = &SCREEN_NORMAL_FACE (screen);
  last_extfrag_face = fp;

 cache_used:

  displayed_glyphs.faceptr = fp;

#ifdef HAVE_X_WINDOWS
  /* Deal with begin/end glyphs.  Should make all this a function. */
  if (pixel_values)
    {
      displayed_glyphs.begin_pixel_width = 0;
      displayed_glyphs.end_pixel_width = 0;
      displayed_glyphs.pixel_height = XFONT (FACE_FONT (fp))->height;
    }
#endif

  displayed_glyphs.begin_or_end = 0;
  displayed_glyphs.n_nonfont = 0;
  displayed_glyphs.begin_columns = 0;
  displayed_glyphs.end_columns = 0;
  displayed_glyphs.begin_class[0] = Qnil;
  displayed_glyphs.end_class[0] = Qnil;

  /* Look for begin glyphs */
  if (extfrag && (pos == extfrag->from) && !ignore_glyphs)
    {
      int n_classes = 0;
      EXTENT *extents_vec = extfrag->extents_stack;
      EXTENT *extents_vec_bound = extents_vec + extfrag->number_of_extents;

      while (extents_vec < extents_vec_bound
	     && n_classes < GLYPH_CLASS_VECTOR_SIZE - 1)
        {
          EXTENT current_extent = *extents_vec++;
          GLYPH g = EXTENT_BEGIN_GLYPH_AT (current_extent, pos);

          if (g)
            {
#ifdef HAVE_X_WINDOWS
	      if (display_glyph_in_buffer (buffer, screen, window, g))
		{
		  if (pixel_values)
		    {
		      Lisp_Object lfont = FACE_FONT (fp);
		      displayed_glyphs.n_nonfont++;
		      XSETEXTENT (displayed_glyphs.begin_class[n_classes++],
				  current_extent);
		      *gp++ = g;
		      displayed_glyphs.begin_columns++;
		      displayed_glyphs.begin_pixel_width +=
			glyph_width (g, lfont);
		    }
		}
	      else
#endif
		*gp++ = ASCII_BEGIN_GLYPH;		    
            }
        }

      /* Note begin glyphs */
      if (n_classes > 0)
	displayed_glyphs.begin_or_end = 2;
      displayed_glyphs.begin_class[n_classes] = Qnil;
    }
  displayed_glyphs.pixel_width = displayed_glyphs.begin_pixel_width;

  if (! only_nonchars)
    {
      GLYPH *g0 = gp;

      c = BUF_CHAR_AT (buffer, pos);
      displayed_glyphs.c = c;
      if (c == '\n')		/* Newline character. */
	goto exit;

#ifdef I18N4
      if ((((c >= 040 && c < 0177) || c >= 0400) ||	/* Normal character. */
#else
      if (((c >= 040 && c < 0177) ||	/* Normal character. */
#endif
	   (!EQ (buffer->ctl_arrow, Qnil) &&	/* 8-bit display */
	    !EQ (buffer->ctl_arrow, Qt) &&
	    (FIXNUMP (buffer->ctl_arrow)
#ifdef I18N4
	     ? c >= XINT (buffer->ctl_arrow)
#else
	     ? c >= (UCHAR) XINT (buffer->ctl_arrow)
#endif
	     : c >= 0240)))
	  && (dp == 0
	      || NILP (DISP_CHAR_ROPE (dp, c))))
	{
	  GLYPH_SET_VALUE (*gp++, GLYPH_FROM_CHAR (c == 0240 ? ' ' : c));
	}
      else if (c == '\t')	/* Tab character. */
	{
	  register int j;
	  int tab_width = 
            ((XINT (buffer->tab_width) <= 0) || 
             (XINT (buffer->tab_width) > 20))
              ? 8:(XINT(buffer->tab_width));

	  j = tab_width - ((columns + tab_offset + hscroll - (hscroll > 0))
			   % tab_width);
	  while (j--)
	    GLYPH_SET_VALUE (*gp++, TABGLYPH);
	}
      else if (dp != 0 && STRINGP (DISP_CHAR_ROPE (dp, c)))
	{
	  register int i = 
            (XSTRING (DISP_CHAR_ROPE (dp, c))->size) / sizeof (GLYPH);
	  GLYPH *data = (GLYPH *) XSTRING (DISP_CHAR_ROPE (dp, c))->data;

	  /* Note that ropes are 16 bit entities stored as double chars */
	  while (i--)
	    GLYPH_SET_VALUE (*gp++, *data++);
	}
      else if (c < 0200 && !NILP (buffer->ctl_arrow))
	{
	  if (dp && FIXNUMP (DISP_CTRL_GLYPH (dp)))
	    GLYPH_SET_VALUE (*gp, XINT (DISP_CTRL_GLYPH (dp)));
	  else
	    GLYPH_SET_VALUE (*gp, GLYPH_FROM_CHAR ('^'));
	  gp++;
	  GLYPH_SET_VALUE (*gp, GLYPH_FROM_CHAR (c ^ 0100));
	  gp++;
	}
      else
	{
	  GLYPH_SET_VALUE 
            (*gp, (dp && FIXNUMP (DISP_ESCAPE_GLYPH (dp))
                   ? XINT (DISP_ESCAPE_GLYPH (dp))
                   : GLYPH_FROM_CHAR ('\\')));
	  gp++;
	  GLYPH_SET_VALUE (*gp, GLYPH_FROM_CHAR (((c >> 6) + '0')));
	  gp++;
	  GLYPH_SET_VALUE (*gp, GLYPH_FROM_CHAR (((7 & (c >> 3)) + '0')));
	  gp++;
	  GLYPH_SET_VALUE (*gp, GLYPH_FROM_CHAR (((7 & c) + '0')));
	  gp++;
	}

#ifdef HAVE_X_WINDOWS
      if (pixel_values)
	while (g0 < gp)
	  {
	    displayed_glyphs.pixel_width
	      += X_CHAR_WIDTH (FACE_X_FONT (fp),
			       ((*g0==TABGLYPH) ? ' ' : *g0));
	    g0++;
	  }
#endif
    }

 exit:

  /* Look for end glyphs */
  if (extfrag && !ignore_glyphs)
    {
      int n_classes = 0;
      EXTENT *extents_vec = extfrag->extents_stack;
      EXTENT *extents_vec_bound = extents_vec + extfrag->number_of_extents;
      int endpos = extfrag->to - 1;
      int save_found_glyphs = (pos == endpos);

      while (extents_vec < extents_vec_bound
	     && n_classes < GLYPH_CLASS_VECTOR_SIZE - 1)
        {
          EXTENT current_extent = *extents_vec++;

          if (EXTENT_END_GLYPH_P (current_extent))
            {
	      GLYPH g = EXTENT_END_GLYPH_AT (current_extent, endpos);
              if (g)
                {
                  frag_has_end_glyphs = 1;

                  if (save_found_glyphs)
                    {
#ifdef HAVE_X_WINDOWS
		      if (display_glyph_in_buffer (buffer, screen, window, g))
			{
			  if (pixel_values)
			    {
			      Lisp_Object lfont = FACE_FONT (fp);
			      
			      displayed_glyphs.n_nonfont++;
			      XSETEXTENT (displayed_glyphs.end_class[n_classes++],
					  current_extent);
			      *gp++ = g;
			      displayed_glyphs.end_columns++;
			      displayed_glyphs.end_pixel_width +=
				glyph_width (g, lfont);
			    }
			}
                      else
#endif
                        *gp++ = ASCII_END_GLYPH;
                    }
                }
            }
        }

      /* Note end glyphs */
      if (n_classes > 0)
        displayed_glyphs.begin_or_end++;
      displayed_glyphs.end_class[n_classes] = Qnil;

      displayed_glyphs.pixel_width += displayed_glyphs.end_pixel_width;
    }

#ifdef NEVER
  if (columns == 0
      && displayed_glyphs.pixel_width
      > (MAX_LINE_WIDTH (screen) - EOL_CURSOR_WIDTH))
    {
      /* Just remove all glyphs.  This is wrong, but better than looping */
      last_extfrag_from_pos = last_extfrag_to_pos = 0; /* Invalidate cache */
      ignore_glyphs = 1;
      goto retry;
    }
#endif

  /* set the displayed_glyphs "run limits" from the extent_fragment info
     and the selection state
   */
  {
    int run_end = last_extfrag_to_pos - 1;

    /* don't go to last position of an extent_fragment in the
       cheapo loop in display_text_line() if there are end glyphs there, 
       because you will lose */
    if (frag_has_end_glyphs)
      run_end -= 1;

    displayed_glyphs.run_pos_lower = pos;
    displayed_glyphs.run_pos_upper = run_end + 1;
  }

  displayed_glyphs.columns = gp - measure_glyphs;
  return &displayed_glyphs;
}


#ifdef REMOVE 
static int
display_mode_element (struct window *w,
		      register int hpos, int depth, int minendcol,
		      register int maxendcol, register Lisp_Object elt)
{
  register struct screen *s = XSCREEN (WINDOW_SCREEN (w));
 
 tail_recurse:
  if (depth > 10)
    goto invalid;
 
  depth++;
 
  if (STRINGP (elt))
    {
        /* A string: output it and check for %-constructs within it.  */
        register unsigned char c;
        register unsigned char *this = XSTRING (elt)->data;
 
        while (hpos < maxendcol && *this)
          {
            unsigned char *last = this;
            while ((c = *this++) != '\0' && c != '%')
              ;
            if (this - 1 != last)
              {
                register int lim = --this - last + hpos;
                hpos = screen_title_display_string (w, last, hpos, 0,
						    hpos,
						    min (lim, maxendcol),
						    &SCREEN_MODELINE_FACE (s),
						    0);
              }
            else /* c == '%' */
              {
                register int spec_width = 0;
 
                /* We can't allow -ve args due to the "%-" construct */
                /* Argument specifies minwidth but not maxwidth
                   (maxwidth can be specified by
                     (<negative-number> . <stuff>) mode-line elements) */
 
		while (((c = *this++), isdigit (c)))
                  {
                    spec_width = spec_width * 10 + (c - '0');
                  }
 
                spec_width += hpos;
                if (spec_width > maxendcol)
                  spec_width = maxendcol;
 
                if (c == 'M')
                  hpos = display_mode_element (w, hpos, depth,
                                               spec_width, maxendcol,
                                               Vglobal_mode_string);
                else if (c != 0)
                  hpos =
		    screen_title_display_string (w,
						 decode_mode_spec (w, c,
							maxendcol - hpos),
						 hpos, 0, spec_width,
						 maxendcol,
						 &SCREEN_MODELINE_FACE (s),0);
              }
          }
      }
 
  else if (SYMBOLP (elt))
      {
        /* A symbol: process the value of the symbol recursively
           as if it appeared here directly.  Avoid error if symbol void.
           Special case: if value of symbol is a string, output the string
           literally.  */
        register Lisp_Object tem;
        tem = Fboundp (elt);
        if (!NILP (tem))
          {
            tem = Fsymbol_value (elt);
            /* If value is a string, output that string literally:
               don't check for % within it.  */
            if (STRINGP (tem))
              hpos = screen_title_display_string (w,
						  XSTRING (tem)->data,
						  hpos, 0, minendcol,
						  maxendcol,
						  &SCREEN_MODELINE_FACE(s),0);
            /* Give up right away for nil or t.  */
            else if (!EQ (tem, elt))
              { elt = tem; goto tail_recurse; }
          }
      }
 
  else if (CONSP (elt))
      {
        register Lisp_Object car, tem;
 
        /* A cons cell: three distinct cases.
           If first element is a string or a cons, process all the elements
           and effectively concatenate them.
           If first element is a negative number, truncate displaying cdr to
           at most that many characters.  If positive, pad (with spaces)
           to at least that many characters.
           If first element is a symbol, process the cadr or caddr recursively
           according to whether the symbol's value is non-nil or nil.  */
        car = XCONS (elt)->car;
        if (SYMBOLP (car))
          {
            tem = Fboundp (car);
            elt = XCONS (elt)->cdr;
            if (!CONSP (elt))
              goto invalid;
            /* elt is now the cdr, and we know it is a cons cell.
               Use its car if CAR has a non-nil value.  */
            if (!NILP (tem))
              {
                tem = Fsymbol_value (car);
                if (!NILP (tem))
                  { elt = XCONS (elt)->car; goto tail_recurse; }
              }
            /* Symbol's value is nil (or symbol is unbound)
               Get the cddr of the original list
               and if possible find the caddr and use that.  */
            elt = XCONS (elt)->cdr;
            if (NILP (elt))
              ;
            else if (!CONSP (elt))
              goto invalid;
            elt = XCONS (elt)->car;
            goto tail_recurse;
          }
        else if (FIXNUMP (car))
          {
            register int lim = XINT (car);
            elt = XCONS (elt)->cdr;
            if (lim < 0)
              /* Negative int means reduce maximum width.
                 DO NOT change MINENDCOL here!
                 (20 -10 . foo) should truncate foo to 10 col
                 and then pad to 20.  */
              maxendcol = min (maxendcol, hpos - lim);
            else if (lim > 0)
              {
                /* Padding specified.  Don't let it be more than
                   current maximum.  */
                lim += hpos;
                if (lim > maxendcol)
                  lim = maxendcol;
                /* If that's more padding than already wanted, queue it.
                   But don't reduce padding already specified even if
                   that is beyond the current truncation point.  */
                if (lim > minendcol)
                  minendcol = lim;
              }
            goto tail_recurse;
          }
        else if (STRINGP (car) || CONSP (car))
          {
            register int limit = 50;
            /* LIMIT is to protect against circular lists.  */
            while (CONSP (elt) && --limit > 0
                   && hpos < maxendcol)
              {
                hpos = display_mode_element (w, hpos, depth,
                                             hpos, maxendcol,
                                             XCONS (elt)->car);
                elt = XCONS (elt)->cdr;
              }
          }
      }
 
  else
    {
    invalid:
      return (screen_title_display_string (w,
					   (unsigned char *) GETTEXT ("*invalid*"),
					   hpos, 0,
					   minendcol, maxendcol,
					   &SCREEN_MODELINE_FACE (s), 0));
    }
 
  if (minendcol > hpos)
    hpos = screen_title_display_string (w, (unsigned char *) "",
					hpos, 0, minendcol, -1,
					&SCREEN_MODELINE_FACE (s), 0);
 
  return hpos;
}
#endif


#ifdef MULTI_SCREEN
static void
redraw_screen (struct screen *s)
{
  struct screen *s2 = 0;

  if (noninteractive)
    return;

  /* clear_screen doesn't take a screen argument, 
     so swap s into selected_screen */
  if (s != selected_screen)
    s2 = selected_screen, selected_screen = s;
  clear_screen ();
  if (s2) selected_screen = s2;

  if (SCREEN_IS_TERMCAP (s))
    fflush (stdout);

/*  clear_screen_records (s); */
  windows_changed++;

  /* Mark all windows as INaccurate,
     so that every window will have its redisplay done.  */
  mark_window_display_accurate (SCREEN_ROOT_WINDOW (s), 0);
  SET_SCREEN_GARBAGED (s);

  s->replot_lines = 1;
}

DEFUN ("redraw-screen", Fredraw_screen, Sredraw_screen, 1, 1, 0,
  "Clear screen SCREEN and output again what is supposed to appear on it.")
  (screen)
     Lisp_Object screen;
{
  struct screen *s;

  CHECK_SCREEN (screen, 0);
  s = XSCREEN (screen);
  redraw_screen (s);

  return Qnil;
}

#endif /* not MULTI_SCREEN */

DEFUN ("redraw-display", Fredraw_display, Sredraw_display, 0, 0, "",
  "Redraw all screens marked as having their images garbled.")
  ()
{
#ifdef MULTI_SCREEN
  Lisp_Object screen, tail;

  for (tail = Vscreen_list; CONSP (tail); tail = XCONS (tail)->cdr)
    {
      screen = XCONS (tail)->car;
      if (XSCREEN (screen)->garbaged && XSCREEN (screen)->visible)
	Fredraw_screen (screen);
    }
  return Qnil;

#else  /* Not MULTI_SCREEN */

  /* #### note that single-screen and multi-screen versions of this function
     have totally different behavior!! */

  if (noninteractive)
    return;
  clear_screen ();
  fflush (stdout);
  clear_screen_records (selected_screen);
  windows_changed++;
  /* Mark all windows as INaccurate,
     so that every window will have its redisplay done.  */
  mark_window_display_accurate (XWINDOW (minibuf_window)->prev, 0);
  return Qnil;

#endif /* not MULTI_SCREEN */
}

DEFUN ("force-redisplay", Fforce_redisplay, Sforce_redisplay, 0, 0, "",
  "Force an immediate redisplay of all screens.\n\
Will still cause a redisplay when there is input pending (unlike when\n\
the display is updated from `next-event').  This function differs from\n\
`redraw-display' in that it causes an immediate, visible update of the\n\
display's contents.  Unlike `redraw-screen' or `recenter', it does not\n\
mark any screen's current contents as invalid.")
     ()
{
  redisplay_1 (1);
  return Qnil;
}

  

/* dump an informative message to the minibuf */
void
message (CONST char *fmt, ...)
{
  va_list args;

  va_start (args, fmt);

  if (!fmt)
    clear_message (1);
  else if (noninteractive || purify_flag)
    {
      if (noninteractive_need_newline)
	{
	  putc ('\n', stderr);
	  noninteractive_need_newline = 0;
	}
      if (fmt)                    /* m == 0 means clear echo-area msg */
      {
        vfprintf (stderr, fmt, args);
        fprintf (stderr, "\n");
      }
      fflush (stderr);
    }
  else if (INTERACTIVE)
    {
      struct screen *s = choose_minibuf_screen ();
      int prnt_sz = 0;

      /* It is bad to print gc messages on stdout before the first X screen
	 has been created.
       */
      if (!NILP (Vwindow_system) && !SCREEN_IS_X (s))
	goto punt;

      if (!SCREEN_MESSAGE_BUF (s))
        goto punt;

      /*
       * Always fit the entire message into the message buffer even if
       * not all of it can display.  The redisplay will display what
       * it can on each given screen.
       */
      do
	{
	  if (prnt_sz >= (SCREEN_MESSAGE_BUF_SIZE (s) - 1))
	    {
	      char *new_message_buf
		= (char *) xrealloc (SCREEN_MESSAGE_BUF (s),
				     prnt_sz * 2);
	      if (echo_area_glyphs == SCREEN_MESSAGE_BUF (s))
		echo_area_glyphs = new_message_buf;
	      SCREEN_MESSAGE_BUF (s) = new_message_buf;
	      SCREEN_MESSAGE_BUF_SIZE (s) = prnt_sz * 2;
	    }

	  prnt_sz = emacs_doprnt ((char *) SCREEN_MESSAGE_BUF (s),
				  SCREEN_MESSAGE_BUF_SIZE (s), 
				  fmt, -1, -1, args);
	}
      while (prnt_sz >= (SCREEN_MESSAGE_BUF_SIZE (s) - 1));
      echo_area_glyphs = SCREEN_MESSAGE_BUF (s);

      if (in_display > 0 || updating_screen != 0
	  || ! SCREEN_VISIBLE_P (s))
	goto punt;

      do {
	hold_window_change();
	layout_echo_area_contents();
	unhold_window_change();
      } while (screen_garbaged); 
    }
 punt:
  va_end (args);
}

void
clear_message (int do_update_screen)
{
  if (noninteractive || purify_flag)
  {
    if (noninteractive_need_newline)
      {
	putc ('\n', stderr);
	noninteractive_need_newline = 0;
      }
    fflush (stderr);
    goto punt;
  }
  else if (noninteractive || purify_flag)
    goto punt;
  else if (INTERACTIVE)
  {
    struct screen *s = choose_minibuf_screen ();

    if (!NILP (Vwindow_system) && !SCREEN_IS_X (s))
      goto punt;

    if (!SCREEN_MESSAGE_BUF (s))
      goto punt;

    if (echo_area_glyphs)
      /* careful of calling clear_message twice in a row and losing 
	 previous_echo_glyphs and thus not redisplaying. */
      {
	previous_echo_glyphs = (char *) "";/* echo_area_glyphs; */
	echo_area_glyphs = 0;
      }

    if (!do_update_screen)
      goto punt;

    if (in_display > 0 || updating_screen != 0
        || ! SCREEN_VISIBLE_P (s))
      goto punt;

    /* This doesn't work: echo_area_display (s); update_screen (s, 1, 1); */
    /* Let's try this instead: */
/* this doesn't work either; all that works is to do nothing.  Fuckme. 
   Rip out this echo_area_glyphs bullshit and all of the rest of the 
  echo area special cases and replace it with a real buffer in a real
  window. */
/*    redisplay_window (s->minibuffer_window, 1); */
  }
 punt:
  return;
}


/* Lucid addition */

Lisp_Object Vscreen_title_format;
Lisp_Object Vscreen_icon_title_format;

#ifdef HAVE_X_WINDOWS

static char screen_title_buffer [1024];
static int screen_title_buffer_index;

static int
screen_title_display_string (struct window *w, unsigned char *string,
			     int hpos, int mincol, int maxcol)
{
  int end = sizeof (screen_title_buffer) - 1;
  while (screen_title_buffer_index < hpos)
    screen_title_buffer [screen_title_buffer_index++] = ' ';
  if (maxcol > 0)
    end = min (maxcol, end);
  for (; *string && screen_title_buffer_index < end;
       string++, screen_title_buffer_index++)
    screen_title_buffer [screen_title_buffer_index] = *string;
  while (screen_title_buffer_index < mincol)
    screen_title_buffer [screen_title_buffer_index++] = ' ';
  screen_title_buffer [screen_title_buffer_index] = '\0';
  return screen_title_buffer_index;
}

/*
 * The next two functions are used to keep redisplay from updating
 * anything other than the edit area.  Currently they prevent the
 * screen title and menubar from being updated and prevent the cursor
 * positioning from being updated on screen.
 *
 * This is not really a clean way to be doing this.  Unfortunately it
 * would take a large amount of work (i.e. a substantial revision of
 * the redisplay) in order to make everything work cleanly.
 *
 * Only used by the scrollbar code (and window_scroll in window.c which is
 * the reason for the state variable:  it needs only a partial lock).
 */

void
lock_redisplay (int state)
{
  redisplay_lock = state;
}

void
unlock_redisplay (void)
{
  redisplay_lock = 0;
}

void
x_format_screen_title (struct screen *s)
{
  struct window *w = XWINDOW (s->selected_window);
  Lisp_Object title_format;
  Lisp_Object icon_format;
  Lisp_Object obuf = Fcurrent_buffer ();

  /* do not change for the minibuffer */
  if (MINI_WINDOW_P (w))
    return;
  /* protect from deleted buffers ! */
  if (NILP (XBUFFER (w->buffer)->name))
    return;
  /* evaluate screen-title-format and screen-icon-title-format in the
     buffer of the selected window of the screen in question.
   */
  Fset_buffer (w->buffer);
  title_format = Vscreen_title_format;
  icon_format = Vscreen_icon_title_format;
  Fset_buffer (obuf);
  screen_title_buffer_index = 0;
  layout_mode_element (w, 0, 0, 0, sizeof (screen_title_buffer) -1,
		       title_format, 0, 0);
  x_set_title_from_char (s, screen_title_buffer);

  if (!(EQ (icon_format, title_format) ||
	EQ (icon_format, Vscreen_title_format))) /* ??? WTF */
    {
      screen_title_buffer_index = 0;
      layout_mode_element (w, 0, 0, 0, sizeof (screen_title_buffer) -1,
			   icon_format, 0, 0);
    }
  x_set_icon_name_from_char (s, screen_title_buffer);
}

#endif /* HAVE_X_WINDOWS */



DEFUN ("message-displayed-p", Fmessage_displayed_p, Smessage_displayed_p,
       0, 1, 0,
  "Return a non-nil value if a message is presently displayed in the\n\
minibuffer's echo area.  If optional argument RETURN-STRING is non-nil,\n\
return a string containing the message, otherwise just return t.")
  (return_string)
   Lisp_Object return_string;
{
  if (echo_area_glyphs)
  {
    if (!NILP (return_string))
      return (make_string (echo_area_glyphs,
			   strlen (echo_area_glyphs)));
    else
      return (Qt);
  }
  else
    return (Qnil);
}

void
syms_of_xdisp ()
{
  staticpro (&Vminibuf_prompt);
  staticpro (&last_arrow_position);
  staticpro (&last_arrow_string);
  Vminibuf_prompt = Qnil;
  last_arrow_position = Qnil;
  last_arrow_string = Qnil;

#ifdef UNUSED
  /* >>>>> UNUSED (though a great idea, if the intent is
   * >>>>>  to replace echo_area_glyphs.) */
  staticpro (&echo_area_buffer);
  echo_area_buffer = Fget_buffer_create (build_string (DEFER_GETTEXT (" *echo area buffer*")));
#endif

  DEFVAR_LISP ("global-mode-string", &Vglobal_mode_string,
    "String displayed by mode-line-format's \"%m\" specification.");
  Vglobal_mode_string = Qnil;

  DEFVAR_LISP ("overlay-arrow-position", &Voverlay_arrow_position,
    "Marker for where to display an arrow on top of the buffer text.\n\
This must be the beginning of a line in order to work.\n\
See also `overlay-arrow-string'.");
  Voverlay_arrow_position = Qnil;

  DEFVAR_LISP ("overlay-arrow-string", &Voverlay_arrow_string,
    "String to display as an arrow.  See also `overlay-arrow-position'.");
  Voverlay_arrow_string = Qnil;

  DEFVAR_INT ("scroll-step", &scroll_step,
    "*The number of lines to try scrolling a window by when point moves out.\n\
If that fails to bring point back on screen, point is centered instead.\n\
If this is zero, point is always centered after it moves off screen.");

  DEFVAR_BOOL ("truncate-partial-width-windows",
	       &truncate_partial_width_windows,
 "*Non-nil means truncate lines in all windows less than full screen wide.");
  truncate_partial_width_windows = 1;

  DEFVAR_BOOL ("line-number-mode", &line_number_mode,
 "*Whether to display line numbers in the mode line.");
  line_number_mode = 0;

  DEFVAR_BOOL ("mode-line-inverse-video", &mode_line_inverse_video,
    "*Non-nil means use inverse video for the mode line.");
  mode_line_inverse_video = 1;

  /* Lucid addition */
  DEFVAR_LISP ("screen-title-format", &Vscreen_title_format,
  "Controls the title of the X window corresponding to the selected screen.\n\
This is the same format as `mode-line-format'.");
  Vscreen_title_format = Fpurecopy (build_string ("%S: %b"));

  DEFVAR_LISP ("screen-icon-title-format", &Vscreen_icon_title_format,
  "Controls the title of the icon corresponding to the selected screen.\n\
See also the variable `screen-title-format'");
  Vscreen_icon_title_format = Fpurecopy (build_string ("%b"));

#ifdef MULTI_SCREEN
  defsubr (&Sredraw_screen);
#endif /* MULTI_SCREEN */
  defsubr (&Sredraw_display);
  defsubr (&Sforce_redisplay);
  defsubr (&Smessage_displayed_p);
}

/* initialize the window system */
void
init_xdisp ()
{
  Lisp_Object root_window;
  struct window *mini_w;

  this_line_bufpos = 0;
  redisplay_lock = 0;
  previous_line_number = 0;
  eob_hack_flag = 0;

  mini_w = XWINDOW (minibuf_window);
  root_window = mini_w->prev;

  echo_area_glyphs = 0;
  previous_echo_glyphs = 0;

  if (!noninteractive)
    {
      /* allocate enough line structures for 5 screens with 40 lines */
      line_pool (200);
 
      /* allocate enough character structures for 5 screen with 40 lines
	 which each have 80 characters per line */
      block_pool (16000);
    }

#if 0 /* #### how can this possibly have worked??
	 s can never be an X screen when this is called!!
	 */

  if (!noninteractive)
    {
      struct screen *s = XSCREEN (WINDOW_SCREEN (XWINDOW (root_window)));
      struct Lisp_Font *font = XFONT (SCREEN_DEFAULT_FONT (s));

#ifdef HAVE_X_WINDOWS
      if (!NILP (Vwindow_system) && !SCREEN_IS_X (s))
	return;
#endif

      XWINDOW (root_window)->pixtop = 0;
      set_window_height (root_window,
 			 (SCREEN_HEIGHT (s) - 1) * font->height, 0);
      mini_w->pixtop = (SCREEN_HEIGHT (s) - 1) * font->height;
      set_window_height (minibuf_window, font->height, 0);

      XWINDOW (root_window)->pixwidth
        = SCREEN_WIDTH (s) * font->height;
      mini_w->pixwidth = SCREEN_WIDTH (s) * font->width;
    }

#endif /* 0 */


}
/* END OLD REDISPLAY FUNCTIONS STILL IN USE */
