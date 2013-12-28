/* This file is part of GNU Emacs.

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

#ifndef EXTENTS_DEFINED
#define EXTENTS_DEFINED

#include "dispextern.h"

struct extent
{
  unsigned short flags;
  short attr_index;
  short priority;
  int start;
  int end;
  struct extent *next;
  struct extent *previous;
  struct extent *e_next;
  struct extent *e_previous;
  Lisp_Object buffer;
  GLYPH begin_glyph;
  GLYPH end_glyph;
  Lisp_Object user_data;
};


typedef struct extent *EXTENT;
#define EXTENT_SIZE (sizeof (struct extent))

/* These macros are for dealing with the extent properties. */
#define EXTENT_FLAGS(e) ((e)->flags)
#define EXTENT_FLAG_P(e, flag) (EXTENT_FLAGS (e) & (flag))
#define SET_EXTENT_FLAG(e, flag) {EXTENT_FLAGS (e) |= (flag);}
#define CLEAR_EXTENT_FLAG(e, flag) {EXTENT_FLAGS (e) &= ~(flag);}

/* the flags -- remember that we only have 16 of them */
#define EF_INVISIBLE     0x1
#define EF_HIGHLIGHT     0x2
#define EF_WRITE_PROTECT 0x4
#define EF_START_GLYPH   0x8
#define EF_END_GLYPH     0x10
#define EF_MENU          0x20
#define EF_START_OPEN    0x40
#define EF_END_OPEN      0x80

#define EF_PROPERTIES    0xff

#define EF_WARN_MODIFY   0x100
#define EF_DUPLICABLE    0x200
#define EF_COLUMN        0x400
#define EF_DETACHED      0x800

#define EF_SELF_MARKED   0x1000
#define EF_LIST_MARKED   0x2000
#define EF_MARKED_MASK   0x3000

#define EF_DESTROYED     0x4000

#define EF_ALL           0xffff

/* GC flags */
#define MARK_EXTENT(e) SET_EXTENT_FLAG(e, EF_SELF_MARKED)
#define MARK_EXTENT_LIST(e) SET_EXTENT_FLAG(e, EF_LIST_MARKED)
#define EXTENT_MARKED_P(e) EXTENT_FLAG_P(e, EF_MARKED_MASK)
#define EXTENT_LIST_MARKED_P(e) EXTENT_FLAG_P(e, EF_LIST_MARKED)
#define UNMARK_EXTENT(e) CLEAR_EXTENT_FLAG(e, EF_MARKED_MASK)

/* detached from parent? */
#define EXTENT_DETACHED_P(e) (EXTENT_FLAG_P(e, EF_DETACHED))

/* obliterated? */
#define EXTENT_DESTROYED_P(e) (EXTENT_FLAG_P(e, EF_DESTROYED))

/* special graphics attribute meaning "use what anyone else's attributes" */
#define GA_NO_CHANGE 0
/* this number should be bigger than any of the "real" GA's */
#define GA_MAX 0x1000

extern Lisp_Object Qextent;
extern Lisp_Object Qextentp;

/* from alloc.c */
extern EXTENT make_extent();

extern Lisp_Object Fextentp();
extern Lisp_Object Fextent_length();
extern Lisp_Object Fextent_start_position();
extern Lisp_Object Fextent_end_position();
extern Lisp_Object Fextent_buffer();
extern Lisp_Object Fextent_to_generic_id();
extern Lisp_Object Fmap_extents();
extern Lisp_Object Fhighlight_extent();
extern Lisp_Object Fforce_highlight_extent();
extern Lisp_Object Fextent_flag_set();
extern Lisp_Object Fextent_write_protected_p();
extern Lisp_Object Fset_extent_write_protected();

struct extent_replica
{
  int start;
  int end;
  Lisp_Object extent;
};
typedef struct extent_replica *EXTENT_REPLICA;
typedef struct extent_replica *DUP;

#define XDUP(obj) XEXTENT_REPLICA(obj)
#define MARK_DUP(dup) {XMARK((dup)->extent);}
#define DUP_MARKED_P(dup) XMARKBIT((dup)->extent)
#define UNMARK_DUP(dup) {XUNMARK((dup)->extent);}

/* used in concat() and merge_replicas() */
struct merge_replicas_struct
{
  Lisp_Object dup_list;
  int entry_offset;
  int entry_length;
};

extern int inside_undo;
extern int in_display;

struct extent_fragment
{
  /* buffer and modification event of buffer for this fragment */
  struct buffer *buf;
  int modiff;
  int face_change;
  /* covers buffer positions [from, to-1] */
  int from;
  int to;
  /* these are the buffer array indices of positions from and to */
  int start_index;
  int end_index;
  /* first extent past the stack */
  EXTENT first_extent_past_stack;
  int number_of_extents;
  EXTENT *extents_stack;
  int extents_stack_length;
  struct face *fp;
};

typedef struct extent_fragment *EXTENT_FRAGMENT;

/* sort of a mini-version of the extents fragment -- each buffer
   with an extents list gets one of these */
struct stack_of_extents
{
  /* an extent "before" those on the stack, or 0 if none is known */
  EXTENT previous_extent;
  /* buf_index over which this stack lies */
  int buf_index;
  EXTENT *stack;
  int stack_length;
  int stack_index;
};

typedef int (*emf)(EXTENT extent, void *arg);
typedef int (*elisp_emf)(Lisp_Object extent_obj, void *arg);

extern void adjust_extents 
(int old_gap, int new_gap, int gap_size, struct buffer *buf);
extern void verify_extent_modification (struct buffer *buf, int from, int to);
/*extern int extent_endpoint (EXTENT extent, int endp);*/
extern void detach_extent (EXTENT extent);
/* extern void update_extent (EXTENT extent, int from, int to, int set_endpoints, struct buffer *buf); */
extern void map_extents (int from, int to, elisp_emf efn, emf fn, void *arg, 
                         struct buffer *buf, int closed_end);
extern Lisp_Object Fmap_extents 
(Lisp_Object function, Lisp_Object buffer, 
 Lisp_Object from, Lisp_Object to, Lisp_Object maparg, Lisp_Object closed_end);
/*extern int extent_highlightable_p (Lisp_Object extent_obj);*/
extern Lisp_Object Fhighlight_extent (Lisp_Object extent_obj, Lisp_Object flag);
extern Lisp_Object Fforce_highlight_extent (Lisp_Object extent_obj, Lisp_Object flag);
extern Lisp_Object Fextent_start_position (Lisp_Object extent_obj);
extern Lisp_Object Fextent_end_position (Lisp_Object extent_obj);
extern Lisp_Object Fextent_length (Lisp_Object extent_obj);
extern Lisp_Object Fextent_buffer (Lisp_Object extent_obj);
extern Lisp_Object Fextent_to_generic_id (Lisp_Object extent_obj);
extern int glyph_in_column_p (Lisp_Object extent_obj);
extern Lisp_Object Frestore_extent (Lisp_Object extent_obj);
extern void set_point (int position);
extern void set_buffer_point (struct buffer *buffer, int position);
extern int last_visible_position (int opoint, struct buffer *buf);
extern EXTENT extent_at (int pos, struct buffer *buf, int flag);
extern Lisp_Object Fextent_at (Lisp_Object pos, Lisp_Object buffer, 
                               Lisp_Object flag);
extern void free_buffer_cached_stack (struct buffer *b);
extern EXTENT_FRAGMENT buffer_extent_fragment_at 
(int pos, struct buffer *buf, struct screen *s);
extern void process_extents_for_insertion (int opoint, int length, 
                                           struct buffer *buf);
extern void process_extents_for_deletion 
(int from, int to, int start, int end, struct buffer *buf);
extern Lisp_Object replicate_extents (int opoint, int length, 
                                      struct buffer *buf);
extern void splice_in_extent_replicas 
(int opoint, int length, Lisp_Object dup_list, struct buffer *buf);
extern Lisp_Object merge_replicas 
(int number_of_lists, struct merge_replicas_struct *vec);
extern void syms_of_extents (void);
   
extern GLYPH extent_glyph_at (EXTENT extent, int pos, int endp);

extern Lisp_Object Vlast_highlighted_extent;
#define EXTENT_BEGIN_GLYPH_AT(extent,pos) extent_glyph_at (extent, pos, 0)
#define EXTENT_END_GLYPH_AT(extent, pos) extent_glyph_at (extent, pos, 1)

#endif /* EXTENTS_DEFINED */
