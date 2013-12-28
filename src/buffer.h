/* Header file for the buffer manipulation primitives.
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

#ifndef _EMACS_BUFFER_H_
#define _EMACS_BUFFER_H_

#define SET_PT(arg) (set_point ((arg)))
#define SET_BUF_PT(buf, value) (set_buffer_point ((buf),(value)))


#define POINT_MARKER_P(marker) \
   (XMARKER (marker)->buffer != 0 && \
    EQ ((marker), XMARKER (marker)->buffer->point_marker))


/* Now similar macros for a specified buffer.
   Note that many of these evaluate the buffer argument more than once.  */

/* Character position of beginning of buffer.  */ 
#define BUF_BEG(buf) (1)
#define BEG BUF_BEG(current_buffer)

/* Character position of beginning of accessible range of buffer.  */ 
#define BUF_BEGV(buf) ((buf)->text.begv)
#define BEGV BUF_BEGV(current_buffer)

/* Address of beginning of accessible range of buffer.  */ 
#define BUF_BEGV_ADDR(buf) CHAR_ADDRESS (BUF_BEGV (buf))
#define BEGV_ADDR BUF_BEGV_ADDR(current_buffer)

/* Character position of point in buffer.  The "+ 0" makes this
   not an l-value, so you can't assign to it.  Use SET_PT instead.  */
#define BUF_PT(buf) ((buf)->text.pt + 0)
#define PT BUF_PT(current_buffer)

/* Address of point in buffer.  */ 
#define BUF_PT_ADDR(buf) CHAR_ADDRESS (BUF_PT (buf))
#define PT_ADDR BUF_PT_ADDR(current_buffer)

/* Character position of gap in buffer.  */ 
#define BUF_GPT(buf) ((buf)->text.gpt)
#define GPT BUF_GPT(current_buffer)

/* Character position of end of accessible range of buffer.  */ 
#define BUF_ZV(buf) ((buf)->text.zv)
#define ZV BUF_ZV(current_buffer)

/* Character position of end of buffer.  */ 
#define BUF_Z(buf) ((buf)->text.z)
#define Z BUF_Z(current_buffer)

/* Modification count.  */
#define BUF_MODIFF(buf) ((buf)->text.modiff)
#define MODIFF BUF_MODIFF(current_buffer)

/* Face changed.  */
#define BUF_FACECHANGE(buf) ((buf)->text.face_change)
#define FACECHANGE BUF_FACECHANGE(current_buffer)

/* Margin changed.  */
#define BUF_MARGINCHANGE(buf) ((buf)->text.margin_change)
#define MARGINCHANGE BUF_MARGINCHANGE(current_buffer)

/* Address of beginning of buffer.  */
#define BUF_BEG_ADDR(buf) ((buf)->text.beg)
#define BEG_ADDR BUF_BEG_ADDR(current_buffer)

/* Macro for setting the value of BUF_ZV (BUF) to VALUE,
   by varying the end of the accessible region.  */
#define SET_BUF_ZV(buf, value) ((buf)->text.zv = (value))

/* Size of gap.  */
#define BUF_GAP_SIZE(buf) ((buf)->text.gap_size)
#define GAP_SIZE BUF_GAP_SIZE(current_buffer)


/* Address of beginning of gap in buffer.  */ 
#define GPT_ADDR (current_buffer->text.beg + current_buffer->text.gpt - 1)

/* Address of end of gap in buffer.  */
#define GAP_END_ADDR (current_buffer->text.beg + current_buffer->text.gpt + current_buffer->text.gap_size - 1)

/* Address of end of accessible range of buffer.  */ 
#define ZV_ADDR (&CHAR_AT_POSITION (current_buffer->text.zv))


/* Macros from translating between position, pointer, and char. */

/* Convert the address of a char in the buffer into a character position.  */
#define PTR_CHAR_POS(ptr) \
  ((ptr) - (current_buffer)->text.beg					\
   - (ptr - (current_buffer)->text.beg < (unsigned) GPT ? 0 : GAP_SIZE)	\
   + 1)


/* Generate the position in buffer, taking the gap into account. */
#define BUF_TRUE_POS(buf, pos) ((pos) >= BUF_GPT ((buf)) ? \
				((pos) + BUF_GAP_SIZE ((buf))) : (pos))

/* Address of character at position POS in buffer BUF. */
#define BUF_CHAR_ADDRESS(buf, pos) ((buf)->text.beg \
				    + (BUF_TRUE_POS ((buf), (pos))) - 1)

/* The character at position POS in BUF. */
#define BUF_CHAR_AT(buf, pos) (*BUF_CHAR_ADDRESS ((buf), (pos)))

/* Address of character at position POS in current buffer. */
#define CHAR_ADDRESS(pos) (BUF_CHAR_ADDRESS ((current_buffer), (pos)))

/* Character at position n in current buffer.  No range checking */
#define CHAR_AT(n) (*CHAR_ADDRESS ((n)))

#ifdef HAVE_X_WINDOWS
/* Width of the outside margins in pixels. */
/* These definitions were modified to make sure that the left outside
   margin cannot overrun the window width, but the right side was not
   taken into account.  This must be fixed if right margins are ever
   implemented.
*/
#define LEFT_MARGIN_INTERNAL(buf,scr,bwin)				\
(MINI_WINDOW_P (bwin)							\
 ? 0									\
 : (NILP ((buf)->left_outside_margin_width)				\
    ? (NILP ((scr)->left_outside_margin_width)				\
       ? 0								\
       : XINT ((scr)->left_outside_margin_width))			\
    : XINT ((buf)->left_outside_margin_width)))
#define RIGHT_MARGIN_INTERNAL(buf,scr,bwin)				\
(MINI_WINDOW_P (bwin)							\
 ? 0									\
 : (NILP ((buf)->right_outside_margin_width)				\
    ? (NILP ((scr)->right_outside_margin_width)				\
       ? 0								\
       : XINT ((scr)->right_outside_margin_width))			\
    : XINT ((buf)->right_outside_margin_width)))

#ifdef I18N4
#define LEFT_MARGIN(buf,scr,bwin)					\
(LEFT_MARGIN_INTERNAL(buf,scr,bwin) *					\
 XmbTextEscapement (SCREEN_DEFAULT_X_FONT (scr), "M", 1))
#define RIGHT_MARGIN(buf,scr,bwin)					\
(RIGHT_MARGIN_INTERNAL(buf,scr,bwin) *					\
 XmbTextEscapement (SCREEN_DEFAULT_X_FONT (scr), "M", 1))
#else
#define LEFT_MARGIN(buf,scr,bwin)					\
(((LEFT_MARGIN_INTERNAL(buf,scr,bwin) + 2) > window_real_width (bwin))	\
 ? ((window_real_width (bwin) - 2) *					\
    XTextWidth (SCREEN_DEFAULT_X_FONT (scr), "M", 1))			\
 : (LEFT_MARGIN_INTERNAL(buf,scr,bwin) *				\
    XTextWidth (SCREEN_DEFAULT_X_FONT (scr), "M", 1)))
#define RIGHT_MARGIN(buf,scr,bwin)					\
(RIGHT_MARGIN_INTERNAL(buf,scr,bwin) *					\
 XTextWidth (SCREEN_DEFAULT_X_FONT (scr), "M", 1))
#endif
#endif



struct buffer_text
  {
#ifdef I18N4
    wchar_t *beg;		/* Actual address of buffer contents. */    
#else
    unsigned char *beg;		/* Actual address of buffer contents. */    
#endif
    int begv;			/* Index of beginning of accessible range. */
    int pt;			/* Position of point in buffer. */
    int gpt;			/* Index of gap in buffer. */
    int zv;			/* Index of end of accessible range. */
    int z;			/* Index of end of buffer. */
    int gap_size;		/* Size of buffer's gap */
    int modiff;			/* This counts buffer-modification events
				   for this buffer.  It is incremented for
				   each such event, and never otherwise
				   changed.  */
    int face_change;		/* This is set when a change in how the text
				   should be displayed (e.g., font, color)
				   is made. */
    int margin_change;		/* This is set when a change is made in the
				   width of either outside margin. */
  };

struct buffer
  {
    struct lcrecord_header header;

    /* This structure holds the coordinates of the buffer contents.  */
    struct buffer_text text;

    /* Flags saying which DEFVAR_PER_BUFFER variables
       are local to this buffer.  */
    int local_var_flags;

    /* Value of text.modiff as of when visited file was read or written. */
    int save_modified;

    /* Set to the modtime of the visited file when read or written.
       -1 means visited file was nonexistent.
       0 means visited file modtime unknown; in no case complain
       about any mismatch on next save attempt.  */
    int modtime;

    /* the value of text.modiff at the last auto-save. */
    int auto_save_modified;

    /* The time at which we detected a failure to auto-save,
       Or -1 if we didn't have a failure.  */
    int auto_save_failure_time;

    /* Position in buffer at which display started
       the last time this buffer was displayed */
    int last_window_start;

    struct stack_of_extents *cached_stack;

    /* These next two are exceptions -- both slots are be handled 
       "specially" by gc_sweep, and their contents are not lisp-accessible 
       as a local variable, but they are Lisp_Objects. */

    /* The markers that refer to this buffer.  This
       is actually a single marker -- successive elements in its marker
       `chain' are the other markers referring to this buffer */
    struct Lisp_Marker *markers;


    /* Everything from here down must be a Lisp_Object */

#define MARKED_SLOT(x) Lisp_Object x
#include "bufslots.h"
#undef MARKED_SLOT
};

extern CONST struct lrecord_implementation lrecord_buffer[];

#define XBUFFER(a) ((struct buffer *) XPNTR(a))
#define CHECK_BUFFER(x, i) CHECK_RECORD ((x), lrecord_buffer, Qbufferp, (i))
#define BUFFERP(x) RECORD_TYPEP ((x), lrecord_buffer)

extern struct buffer *current_buffer;

/* This structure holds the default values of the buffer-local variables
   defined with DefBufferLispVar, that have special slots in each buffer.
   The default value occupies the same slot in this structure
   as an individual buffer's value occupies in that buffer.
   Setting the default value also goes through the alist of buffers
   and stores into each buffer that does not say it has a local value.  */

extern Lisp_Object Vbuffer_defaults;

/* This structure marks which slots in a buffer have corresponding
   default values in buffer_defaults.
   Each such slot has a nonzero value in this structure.
   The value has only one nonzero bit.

   When a buffer has its own local value for a slot,
   the bit for that slot (found in the same slot in this structure)
   is turned on in the buffer's local_var_flags slot.

   If a slot in this structure is zero, then even though there may
   be a DefBufferLispVar for the slot, there is no default valuefeor it;
   and the corresponding slot in buffer_defaults is not used.  */

extern struct buffer buffer_local_flags;


/* Point in the current buffer.  This is an obsolete alias
   and should be eliminated.  */
/* #define point PT */

/* BUFFER_CEILING_OF (resp. BUFFER_FLOOR_OF), when applied to n, return
   the max (resp. min) p such that

   &BUF_CHAR_AT (b,p) - &BUF_CHAR_AT (b,n) == p - n       */

/*  Return the maximum index in the buffer it is safe to scan forwards
    past N to.  This is used to prevent buffer scans from running into
    the gap (see search.c) */
#define BUFFER_CEILING_OF(b,n) \
        (((n) < BUF_GPT(b) && BUF_GPT(b) < BUF_ZV(b) ? \
	  BUF_GPT(b) : BUF_ZV(b)) \
	 - 1)

/*  Return the minium index in the buffer it is safe to scan backwards
    past N to. */
#define BUFFER_FLOOR_OF(b,n) \
        (BUF_BEGV(b) <= BUF_GPT(b) && BUF_GPT(b) <= (n) ? \
	 BUF_GPT(b) : BUF_BEGV(b))


/* Allocation of buffer data. */

#ifdef REL_ALLOC

extern char* r_alloc (char **, unsigned long);
extern char* r_re_alloc (char **, unsigned long);
extern void r_alloc_free (void **);

#ifdef I18N4
#define BUFFER_ALLOC(data,size)\
	((wchar_t *) r_alloc (&data, (size) * sizeof(wchar_t)))
#define BUFFER_REALLOC(data,size)\
	((wchar_t *) r_re_alloc (&data, (size) * sizeof(wchar_t)))
#else /* not I18N4 */
#define BUFFER_ALLOC(data,size) \
	((unsigned char *) r_alloc ((char **) &(data), (size)))
#define BUFFER_REALLOC(data,size) \
	((unsigned char *) r_re_alloc ((char **) &(data), (size)))
#endif /* not I18N4 */

#define BUFFER_FREE(data) (r_alloc_free ((void **) &(data)))

#else /* !REL_ALLOC */

#ifdef I18N4
#define BUFFER_ALLOC(data,size)\
	(data = (wchar_t *) xmalloc ((size) * sizeof(wchar_t)))
#define BUFFER_REALLOC(data,size)\
	((wchar_t *) xrealloc ((data), (size) * sizeof(wchar_t)))
#else /* not I18N4 */
#define BUFFER_ALLOC(data,size) (data = (unsigned char *) xmalloc ((size)))
#define BUFFER_REALLOC(data,size) ((unsigned char *) xrealloc ((data), (size)))
#endif /* I18N4 */
#define BUFFER_FREE(data) (xfree ((data)))

#endif /* !REL_ALLOC */

/* A search buffer, with a fastmap allocated and ready to go.  */
extern struct re_pattern_buffer searchbuf;


extern Lisp_Object Vbuffer_alist;
extern void set_buffer_internal (struct buffer *b);

/* compatibility */
#define FETCH_CHAR(x) CHAR_AT((x))

#endif /* _EMACS_BUFFER_H_ */
