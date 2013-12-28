/* Header file for the buffer manipulation primitives.
   Copyright (C) 1985, 1986 Free Software Foundation, Inc.

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

#ifdef lint
#include "undo.h"
#endif /* lint */


#define SET_PT(arg) (set_point ((arg)))
#define SET_BUF_PT(buf, value) (set_buffer_point ((buf),(value)))

/* Character position of beginning of buffer.  */ 
#define BEG (1)

/* Character position of beginning of accessible range of buffer.  */ 
#define BEGV (current_buffer->text.begv)

/* Character position of point in buffer.  */ 
#define PT (current_buffer->text.pt)

/* Character position of gap in buffer.  */ 
#define GPT (current_buffer->text.gpt)

/* Character position of end of accessible range of buffer.  */ 
#define ZV (current_buffer->text.zv)

/* Character position of end of buffer.  */ 
#define Z (current_buffer->text.z)

/* Modification count.  */
#define MODIFF (current_buffer->text.modiff)

/* Face changed.  */
#define FACECHANGE (current_buffer->text.face_change)

/* Address of beginning of buffer.  */ 
#define BEG_ADDR (current_buffer->text.beg)

/* Address of beginning of accessible range of buffer.  */ 
#define BEGV_ADDR (CHAR_ADDRESS (current_buffer->text.begv))

/* Address of point in buffer.  */ 
#define PT_ADDR (CHAR_ADDRESS (current_buffer->text.pt))

/* Address of beginning of gap in buffer.  */ 
#define GPT_ADDR (current_buffer->text.beg + current_buffer->text.gpt - 1)

/* Address of end of gap in buffer.  */
#define GAP_END_ADDR (current_buffer->text.beg + current_buffer->text.gpt + current_buffer->text.gap_size - 1)

/* Address of end of accessible range of buffer.  */ 
#define ZV_ADDR (&CHAR_AT_POSITION (current_buffer->text.zv))

/* Size of gap.  */
#define GAP_SIZE (current_buffer->text.gap_size)


#define POINT_MARKER_P(marker) \
   (XMARKER (marker)->buffer != 0 && \
    EQ ((marker), XMARKER (marker)->buffer->point_marker))


/* Now similar macros for a specified buffer.
   Note that many of these evaluate the buffer argument more than once.  */

/* Character position of beginning of buffer.  */ 
#define BUF_BEG(buf) (1)

/* Character position of beginning of accessible range of buffer.  */ 
#define BUF_BEGV(buf) ((buf)->text.begv)

/* Character position of point in buffer.  */ 
#define BUF_PT(buf) ((buf)->text.pt)

/* Character position of gap in buffer.  */ 
#define BUF_GPT(buf) ((buf)->text.gpt)

/* Character position of end of accessible range of buffer.  */ 
#define BUF_ZV(buf) ((buf)->text.zv)

/* Character position of end of buffer.  */ 
#define BUF_Z(buf) ((buf)->text.z)

/* Modification count.  */
#define BUF_MODIFF(buf) ((buf)->text.modiff)

/* Face changed.  */
#define BUF_FACECHANGE(buf) ((buf)->text.face_change)

/* Address of beginning of buffer.  */
#define BUF_BEG_ADDR(buf) ((buf)->text.beg)

/* Macro for setting the value of BUF_ZV (BUF) to VALUE,
   by varying the end of the accessible region.  */
#define SET_BUF_ZV(buf, value) ((buf)->text.zv = (value))

/* Size of gap.  */
#define BUF_GAP_SIZE(buf) ((buf)->text.gap_size)


/* Macros from translating between position, pointer, and char. */


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

/* Convert the address of a char in the buffer into a character position.  */
#define PTR_CHAR_POS(ptr) \
((ptr) - (current_buffer)->text.beg - (ptr - (current_buffer)->text.beg < (unsigned) GPT ? 0 : GAP_SIZE))

struct buffer_text
  {
    unsigned char *beg;		/* Actual address of buffer contents. */    
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
  };

struct buffer
  {
    /* Everything before the `name' slot must be of a non-Lisp_Object type,
       and every slot after `name' must be a Lisp_Object.

       Check out mark_buffer (alloc.c) to see why.
     */

    /* This structure holds the coordinates of the buffer contents.  */
    struct buffer_text text;

    /* Next buffer, in chain of all buffers including killed buffers.
       This chain is used only for garbage collection, in order to
       collect killed buffers properly.  */
    struct buffer *next;

    /* Flags saying which DEFVAR_PER_BUFFER variables
       are local to this buffer.  */
    int local_var_flags;

    /* Value of text.modiff when buffer last saved */
    int save_modified;

    /* Set to the modtime of the visited file when read or written.
       -1 means visited file was nonexistent.
       0 means visited file modtime unknown; in no case complain
       about any mismatch on next save attempt.  */
    int modtime;

    /* the value of text.modiff at the last auto-save. */
    int auto_save_modified;

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
    Lisp_Object markers;

    /* Active regions in this buffer. */
    Lisp_Object extents;

    /* Everything from here down must be a Lisp_Object */

    /* the name of this buffer */
    Lisp_Object name;
#undef MARKED_SLOT
#define MARKED_SLOT(x) Lisp_Object x
#include "bufslots.h"
};

extern struct buffer *current_buffer;

/* This structure holds the default values of the buffer-local variables
   defined with DefBufferLispVar, that have special slots in each buffer.
   The default value occupies the same slot in this structure
   as an individual buffer's value occupies in that buffer.
   Setting the default value also goes through the alist of buffers
   and stores into each buffer that does not say it has a local value.  */

extern struct buffer buffer_defaults;

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

/* For each buffer slot, this points to the Lisp symbol name
   for that slot in the current buffer.  It is 0 for slots
   that don't have such names.  */

extern struct buffer buffer_local_symbols;

/* Point in the current buffer. */
#define point (current_buffer->text.pt)

/*  Return the maximum index in the buffer it is safe to scan forwards
    past N to.  This is used to prevent buffer scans from running into
    the gap (see search.c) */
#define BUFFER_CEILING_OF(n) (((n) < GPT && GPT < ZV ? GPT : ZV) - 1)

/*  Return the minium index in the buffer it is safe to scan backwards
    past N to. */
#define BUFFER_FLOOR_OF(n) (BEGV <= GPT && GPT <= (n) ? GPT : BEGV)

extern void reset_buffer ();

/* Functions to call before and after each text change. */
extern Lisp_Object Vbefore_change_function;
extern Lisp_Object Vafter_change_function;
extern Lisp_Object Vfirst_change_function;

/* Fields.

A field is like a marker but it defines a region rather than a
point.  Like a marker, a field is asocated with a buffer.
The field mechanism uses the marker mechanism in the
sense that its start and end points are maintained as markers
updated in the usual way as the buffer changes.

A field can be protected or unprotected.  If it is protected,
no modifications can be made that affect the field in its buffer,
when protected field checking is enabled.

Each field also contains an alist, in which you can store
whatever you like.  */

/* Slots in a field:  */

#define FIELD_BUFFER(f) (XVECTOR(f)->contents[1])
#define FIELD_START_MARKER(f) (XVECTOR(f)->contents[2])
#define FIELD_END_MARKER(f) (XVECTOR(f)->contents[3])
#define FIELD_PROTECTED_FLAG(f) (XVECTOR(f)->contents[4])
#define FIELD_ALIST(f) (XVECTOR(f)->contents[5])

/* Allocation of buffer data. */
#ifdef REL_ALLOC
#define BUFFER_ALLOC(data,size) ((unsigned char *) r_alloc (&data, (size)))
#define BUFFER_REALLOC(data,size) ((unsigned char *) r_re_alloc (&data, (size)))
#define BUFFER_FREE(data) (r_alloc_free (&data))
#else
#define BUFFER_ALLOC(data,size) (data = (unsigned char *) malloc ((size)))
#define BUFFER_REALLOC(data,size) ((unsigned char *) realloc ((data), (size)))
#define BUFFER_FREE(data) (free ((data)))
#endif
