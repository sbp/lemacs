/* This file is part of GNU Emacs.

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

#ifndef _EMACS_EXTENTS_H_
#define _EMACS_EXTENTS_H_

#include "dispextern.h"

#define XEXTENT(a) ((struct extent *) XPNTR(a))
#define XEXTENT_REPLICA(a) ((struct extent_replica *) XPNTR(a))

struct extent_header
{
  LISP_WORD_TYPE start;
  LISP_WORD_TYPE end;
  Lisp_Object buf; /* A buffer or Qnil for extents;
                      an extent for extent_replicas */
};

struct extent
{
#ifdef LRECORD_EXTENT
  struct lrecord_header lheader;
#endif /* LRECORD_EXTENT */
  struct extent_header ehead;
  unsigned short flags;
  short attr_index;
  short priority;
  unsigned int layout:4;
  GLYPH glyph;
  struct extent *next;
  struct extent *previous;
  struct extent *e_next;
  struct extent *e_previous;
  Lisp_Object user_data;
};

#define extent_buffer(e) ((e)->ehead.buf)
#define extent_start(e) ((e)->ehead.end)
#define extent_end(e) ((e)->ehead.start)

#ifndef LRECORD_EXTENT
# define XSETEXTENT(e, p) XSET((e), Lisp_Extent, (p))
#else
# define XSETEXTENT(e, p) XSETR((e), extent_or_replica, (p))
#endif /* LRECORD_EXTENT */


struct extent_replica
{
#ifdef LRECORD_EXTENT
  struct { const struct lrecord_implementation *implementation; } lheader;
#endif /* LRECORD_EXTENT */
  /* "dead" extent_replicas have EQ(ehead->buf,Vthis_is_a_dead_extent_replica)
   */
  struct extent_header ehead;
};

#define dup_extent(e) ((e)->ehead.buf)
/*#define dup_buffer(e) ((e)->ehead.buf)*/
#define dup_start(e) ((e)->ehead.start)
#define dup_end(e) ((e)->ehead.end)
#define dup_live_p(e) \
  (!EQ ((e)->ehead.buf, Vthis_is_a_dead_extent_replica))

extern Lisp_Object Vthis_is_a_dead_extent_replica;


typedef struct extent *EXTENT;
#define EXTENT_SIZE (sizeof (struct extent))

/* These macros are for dealing with the extent properties. */
#define EXTENT_FLAGS(e) ((e)->flags)
#define EXTENT_FLAG_P(e, flag) (EXTENT_FLAGS (e) & (flag))
#define SET_EXTENT_FLAG(e, flag) do {EXTENT_FLAGS (e) |= (flag);} while (0)
#define CLEAR_EXTENT_FLAG(e, flag) do {EXTENT_FLAGS (e) &= ~(flag);} while (0)

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

#define EF_DESTROYED     0x1000

#ifndef LRECORD_EXTENT
# define EF_SELF_MARKED   0x2000
# define EF_MARKED_MASK   0x2000
# define MARK_EXTENT(e) SET_EXTENT_FLAG(e, EF_SELF_MARKED)
# define EXTENT_MARKED_P(e) EXTENT_FLAG_P(e, EF_MARKED_MASK)
# define UNMARK_EXTENT(e) CLEAR_EXTENT_FLAG(e, EF_MARKED_MASK)
#endif /* ! LRECORD_EXTENT */

#define EF_ALL           0xffff


/* the layouts for glyphs */
#define GL_OUTSIDE_MARGIN	0x1
#define GL_INSIDE_MARGIN	0x2
#define GL_WHITESPACE		0x4
#define GL_TEXT			0x8
#define EXTENT_GLYPH_LAYOUT(e) ((e)->layout)
#define EXTENT_GLYPH_LAYOUT_P(e, flag) (EXTENT_GLYPH_LAYOUT (e) & (flag))
#define SET_EXTENT_GLYPH_LAYOUT(e, layout) \
	do {EXTENT_GLYPH_LAYOUT (e) = (layout);} while (0)


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

#ifdef emacs	/* things other than emacs want the structs */

/* from alloc.c */
extern struct extent *make_extent (void);
extern struct extent_replica *make_extent_replica (void);

extern void print_extent_or_replica (Lisp_Object obj, 
                                     Lisp_Object printcharfun, int escape);

extern Lisp_Object Fextentp (Lisp_Object obj);
extern Lisp_Object Fextent_length (Lisp_Object ext);
extern Lisp_Object Fextent_start_position (Lisp_Object ext);
extern Lisp_Object Fextent_end_position (Lisp_Object ext);
extern Lisp_Object Fextent_buffer (Lisp_Object ext);
extern Lisp_Object Fmap_extents (Lisp_Object function, 
                                 Lisp_Object buffer, 
                                 Lisp_Object from, Lisp_Object to,
                                 Lisp_Object maparg,
                                 Lisp_Object closed_end);
extern Lisp_Object Fhighlight_extent (Lisp_Object extent, Lisp_Object flag);
extern Lisp_Object Fforce_highlight_extent (Lisp_Object ex, 
                                            Lisp_Object flag);
extern Lisp_Object Fforce_highlight_extent (Lisp_Object ex, Lisp_Object flag);
extern Lisp_Object Fextent_flag_set ();
extern Lisp_Object Fextent_write_protected_p ();
extern Lisp_Object Fset_extent_write_protected ();
extern Lisp_Object Fextent_layout (Lisp_Object extent);
extern Lisp_Object Fset_extent_layout (Lisp_Object extent, Lisp_Object layout);

#endif /* emacs */

typedef struct extent_replica *EXTENT_REPLICA;
typedef struct extent_replica *DUP;

#ifndef LRECORD_EXTENT
# define EXTENTP(x) ((XTYPE ((x)) == Lisp_Extent) \
                     && (XTYPE ((XEXTENT ((x)))->ehead.buf) != Lisp_Extent))
# define EXTENT_REPLICA_P(x) ((XTYPE ((x)) == Lisp_Extent) \
                     && (XTYPE ((XEXTENT ((x)))->ehead.buf) == Lisp_Extent))
#else /* LRECORD_EXTENT */
# define EXTENTP(x) (RECORD_TYPEP ((x), lrecord_extent))
# define EXTENT_REPLICA_P(x) (RECORD_TYPEP ((x), lrecord_extent_replica))
extern const struct lrecord_implementation lrecord_extent[2];
extern const struct lrecord_implementation lrecord_extent_replica[];
#endif /* LRECORD_EXTENT */


#define CHECK_EXTENT(x, i) \
  do { if (!EXTENTP ((x))) \
         x = wrong_type_argument (Qextentp, (x)); } while (0)

#define CHECK_EXTENT_REPLICA(x, i) \
  do { if (!EXTENT_REPLICA_P ((x))) \
         x = wrong_type_argument (Qextent_replica_p, (x)); } while (0)

extern Lisp_Object Qextentp, Qextent_replica_p;
extern Lisp_Object Fextentp (Lisp_Object object);
extern Lisp_Object Fextent_replica_p (Lisp_Object object);


#define XDUP(obj) (XEXTENT_REPLICA (obj))
#ifndef LRECORD_EXTENT
# define MARK_DUP(dup) do { XMARK (dup_extent ((dup))); } while (0)
# define DUP_MARKED_P(dup) (XMARKBIT (dup_extent ((dup))))
# define UNMARK_DUP(dup) do { XUNMARK (dup_extent ((dup))); } while (0)
#endif /* LRECORD_EXTENT */

#ifdef emacs

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
  LISP_WORD_TYPE from;
  LISP_WORD_TYPE to;
  /* these are the buffer array indices of positions from and to */
  LISP_WORD_TYPE start_index;
  LISP_WORD_TYPE end_index;
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
  LISP_WORD_TYPE buf_index;
  EXTENT *stack;
  int stack_length;
  int stack_index;
};

typedef int (*emf)(EXTENT extent, void *arg);
typedef int (*elisp_emf)(Lisp_Object extent_obj, void *arg);

extern void adjust_extents (int old_gap, int new_gap, int gap_size,
                            struct buffer *buf);
extern void verify_extent_modification (struct buffer *buf, int from, int to);
extern void detach_extent (EXTENT extent);
extern void map_extents (int from, int to, elisp_emf efn, emf fn, void *arg, 
                         struct buffer *buf, int closed_end);
extern Lisp_Object Fmap_extents (Lisp_Object function, Lisp_Object buffer, 
                                 Lisp_Object from, Lisp_Object to,
                                 Lisp_Object maparg, 
                                 Lisp_Object closed_end);
extern Lisp_Object Fhighlight_extent (Lisp_Object extent_obj, 
                                      Lisp_Object flag);
extern Lisp_Object Fforce_highlight_extent (Lisp_Object extent_obj, 
                                            Lisp_Object flag);
extern Lisp_Object Fextent_start_position (Lisp_Object extent_obj);
extern Lisp_Object Fextent_end_position (Lisp_Object extent_obj);
extern Lisp_Object Fextent_length (Lisp_Object extent_obj);
extern Lisp_Object Fextent_buffer (Lisp_Object extent_obj);
extern int glyph_in_column_p (Lisp_Object extent_obj);
extern Lisp_Object Frestore_extent (Lisp_Object extent_obj);
extern void set_point (int position);
extern void set_buffer_point (struct buffer *buffer, int position);
extern int last_visible_position (int opoint, struct buffer *buf);
extern EXTENT extent_at (int pos, struct buffer *buf, int flag);
extern Lisp_Object Fextent_at (Lisp_Object pos, Lisp_Object buffer, 
                               Lisp_Object flag);
extern void init_buffer_cached_stack (struct buffer* b);
extern void free_buffer_cached_stack (struct buffer *b);
extern void detach_buffer_extents (struct buffer *b);
extern EXTENT_FRAGMENT buffer_extent_fragment_at (int pos, 
                                                  struct buffer *buf,
                                                  struct screen *s);
extern void process_extents_for_insertion (int opoint, int length, 
                                           struct buffer *buf);
extern void process_extents_for_deletion (int from, int to, int start, int end,
                                          struct buffer *buf);
extern Lisp_Object replicate_extents (int opoint, int length, 
                                      struct buffer *buf);
extern void splice_in_extent_replicas (int opoint, int length,
                                       Lisp_Object dup_list, 
                                       struct buffer *buf);
extern Lisp_Object merge_replicas (int number_of_lists,
                                   struct merge_replicas_struct *vec);
extern void syms_of_extents (void);
   
extern void setup_extent_fragment_face_ptr (struct screen *s,
					    EXTENT_FRAGMENT extfrag);

extern int extent_cache_invalid;

extern GLYPH extent_glyph_at (EXTENT extent, int pos, int endp);

extern Lisp_Object Vlast_highlighted_extent;

#define EXTENT_BEGIN_GLYPH_AT(extent,pos) extent_glyph_at (extent, pos, 0)
#define EXTENT_END_GLYPH_AT(extent, pos) extent_glyph_at (extent, pos, 1)

#endif /* emacs */

#endif /* _EMACS_EXTENTS_H_ */
