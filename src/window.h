/* Window definitions for GNU Emacs.
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

#ifndef _EMACS_WINDOW_H_
#define _EMACS_WINDOW_H_

/* All windows in use are arranged into a tree, with pointers up and down.

Windows that are leaves of the tree are actually displayed
and show the contents of buffers.  Windows that are not leaves
are used for representing the way groups of leaf windows are
arranged on the screen.  Leaf windows never become non-leaves.
They are deleted only by calling delete-window on them (but
this can be done implicitly).  Combination windows can be created
and deleted at any time.

A leaf window has a non-nil buffer field, and also
 has markers in its start and pointm fields.  Non-leaf windows
 have nil in these fields.

Non-leaf windows are either vertical or horizontal combinations.

A vertical combination window has children that are arranged on the screen
one above the next.  Its vchild field points to the uppermost child.
The parent field of each of the children points to the vertical
combination window.  The next field of each child points to the
child below it, or is nil for the lowest child.  The prev field
of each child points to the child above it, or is nil for the
highest child.

A horizontal combination window has children that are side by side.
Its hchild field points to the leftmost child.  In each child
the next field points to the child to the right and the prev field
points to the child to the left.

The children of a vertical combination window may be leaf windows
or horizontal combination windows.  The children of a horizontal
combination window may be leaf windows or vertical combination windows.

At the top of the tree are two windows which have nil as parent.
The second of these is minibuf_window.  The first one manages all
the screen area that is not minibuffer, and is called the root window.
Different windows can be the root at different times;
initially the root window is a leaf window, but if more windows
are created then that leaf window ceases to be root and a newly
made combination window becomes root instead.

In any case, prev of the minibuf window is the root window and
next of the root window is the minibuf window.  To find the
root window at any time, do XWINDOW (minibuf_window)->prev.

*/

struct window
  {
    struct lcrecord_header header;
    /* ####
     * These fields no longer all have to be Lisp_Objects
     *  now that the record_header tells the GC to call
     *  the special mark_window procedure, which can be
     *  taught about which structure elements are tagged
     *  and which aren't.
     * For now I'm leaving them all tagged
     */

    /* The screen this window is on.  */
    Lisp_Object screen;
    /* t if this window is a minibuffer window.  */
    Lisp_Object mini_p;
    /* Following child (to right or down) at same level of tree */
    Lisp_Object next;
    /* Preceding child (to left or up) at same level of tree */
    Lisp_Object prev;
    /* First child of this window. */
    /* vchild is used if this is a vertical combination,
       hchild if this is a horizontal combination. */
    Lisp_Object hchild, vchild;
    /* The window this one is a child of. */
    Lisp_Object parent;
    /* The upper left corner coordinates of this window,
       as integers (pixels) relative to upper left corner of screen = 0, 0 */
    int pixleft;
    int pixtop;
    /* The size of the window (in pixels) */
    int pixheight;
    int pixwidth;
    /* The buffer displayed in this window */
    /* Of the fields vchild, hchild and buffer, only one is non-nil.  */
    Lisp_Object buffer;
    /* A marker pointing to where in the text to start displaying */
    Lisp_Object start;
    /* A marker pointing to where in the text point is in this window,
       used only when the window is not selected.
       This exists so that when multiple windows show one buffer
       each one can have its own value of point.  */
    Lisp_Object pointm;
    /* Number of columns display within the window is scrolled to the left. */
    Lisp_Object hscroll;
    /* Number saying how recently window was selected */
    Lisp_Object use_time;
    /* text.modified of displayed buffer as of last time display completed */
    Lisp_Object last_modified;
    /* Value of point at that time */
    Lisp_Object last_point;
    /* buf.face_change as of last time display completed */
    Lisp_Object last_facechange;
    /* marker used when restoring a window configuration */
    int config_mark;
/* The rest are currently not used or only half used */
    /* Screen coords of point at that time */
    Lisp_Object last_point_x;
    Lisp_Object last_point_y;
    /* Screen coords of mark as of last time display completed */
    /* May be nil if mark does not exist or was not on screen */
    Lisp_Object last_mark_x;
    Lisp_Object last_mark_y;
    /* Number of characters in buffer past bottom of window,
       as of last redisplay that finished. */
    int window_end_pos;
    /* Vertical position (relative to window top) of that buffer position
       of the first of those characters */
    int window_end_vpos;	/* lines */
    int window_end_ppos;	/* pixels */
    /* t if window_end_pos is truly valid.
       This is nil if nontrivial redisplay is preempted
       since in that case the screen image that window_end_pos
       did not get onto the screen.  */
    char window_end_valid;
    /* Non-nil means next redisplay must use the value of start
       set up for it in advance.  Set by scrolling commands.  */
    char force_start;
    /* Non-nil means must regenerate mode line of this window */
    char redo_mode_line;
    /* Non-nil means current value of `start'
       was the beginning of a line when it was chosen.  */
    char start_at_line_beg;
    /* Has window changed in size since last redisplay? */
    char size_change;

    /* Display-table to use for displaying chars in this window.
       Nil means use the buffer's own display-table.  */
    Lisp_Object display_table;
    /* Non-nil means window is marked as dedicated.  */
    Lisp_Object dedicated;

    /* Display structures for this window. */
    struct line_header *lines;

    /* Display structure for modeline */
    struct line_header *modeline;

    /* # of lines in window; only meaningful if window is "full" */
    int used_height;
  };

#ifdef emacs  /* some things other than emacs want the structs */

extern const struct lrecord_implementation lrecord_window[];

#define XWINDOW(a) ((struct window *) XPNTR(a))
#define CHECK_WINDOW(x, i) CHECK_RECORD ((x), lrecord_window, Qwindowp, (i))
#define WINDOWP(x) RECORD_TYPEP ((x), lrecord_window)

/* 1 if W is a minibuffer window.  */
#define MINI_WINDOW_P(W)  (!EQ ((W)->mini_p, Qnil))

/* Check only window on this screen. */
#define ONLY_WINDOW_P(w) (NILP ((w)->parent))

#define WINDOW_LEFT(w) (XINT ((w)->left))
#define WINDOW_TOP(w) (XINT ((w)->top))
#define WINDOW_WIDTH(w) (XINT ((w)->width))
#define WINDOW_HEIGHT(w) (XINT ((w)->height))


/* This is the window in which the terminal's cursor should
   be left when nothing is being done with it.  This must
   always be a leaf window, and its buffer is selected by
   the top level editing loop at the end of each command.

   This value is always the same as
    SCREEN_SELECTED_WINDOW (selected_screen).  */
extern Lisp_Object selected_window;

/* The minibuffer window of the selected screen.
   Note that you cannot test for minibufferness of an arbitrary window
   by comparing against this; but you can test for minibufferness of
   the selected window or of any window that is displayed.  */
extern Lisp_Object minibuf_window;

/* Non-nil => window to for C-M-v to scroll
   when the minibuffer is selected.  */
extern Lisp_Object Vminibuf_scroll_window;

/* nil or a symbol naming the window system
   under which emacs is running
   ('x is the only current possibility) */
extern Lisp_Object Vwindow_system;

/* Version number of X windows: 10, 11 or nil.  */
extern Lisp_Object Vwindow_system_version;

/* Prompt to display in front of the minibuffer contents, or nil */
extern Lisp_Object Vminibuf_prompt;
extern int minibuf_prompt_width;
extern int minibuf_prompt_pix_width;

/* Message to display instead of minibuffer contents 
   This is what the functions error and message make,
   and command echoing uses it as well. It overrides the
   minibuf_prompt as well as the buffer */
extern char *echo_area_glyphs;

/* Depth in minibuffer invocations */
extern int minibuf_level;

/* true iff we should redraw the mode lines on the next redisplay */
extern int redraw_mode_line;

/* Minimum value of bf_s1 since last redisplay that finished.
 Valid for current buffer unless Cant1WinOpt is nonzero. */
extern int beg_unchanged;

/* Minimum value of bf_s2 since last redisplay that finished.
 Valid for current buffer unless Cant1WinOpt is nonzero. */
extern int end_unchanged;

/* MODIFF as of last redisplay that finished;
 if it matches MODIFF, beg_unchanged and end_unchanged
 contain no useful information */
extern int unchanged_modified;

/* Nonzero if head_clip or tail_clip of current buffer has changed
 since last redisplay that finished */
extern int clip_changed;

/* Nonzero if window sizes or contents have changed
 since last redisplay that finished */
extern int windows_or_buffers_changed;

/* Number of windows displaying the selected buffer.
   Normally this is 1, but it can be more.  */
extern int buffer_shared;

extern void redisplay_preserving_echo_area (void);
extern void redisplay (void);
extern Lisp_Object make_window (void);

extern int window_char_left (struct window *);
extern int window_char_top (struct window *);
extern int window_char_width (struct window *);
extern int window_char_height (struct window *);

#endif /* emacs */

#endif /* _EMACS_WINDOW_H_ */
