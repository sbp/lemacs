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
  struct lrecord_header lheader;
  struct extent_header ehead;
  /* Doubly linked lists based on start-positions and end-positions */
  struct extent *next;
  struct extent *previous;
  struct extent *e_next;
  struct extent *e_previous;

  /* Extent properties are conceptually a plist, but the most common
     props are implemented as bits instead of conses.
   */
  struct
    {
      GLYPH glyph;		/* 16 bits (index into pixmap table) */
      short face_id;		/* 16 bits (index into face table, or -1) */
      short priority;		/* 16 bits (small signed integer) */

      /* These flags are simply an optimization for common boolean properties
	 which go onto the extent's property list.  Any of them would work if
	 done in the normal way, but the space savings of doing these in this
	 way is significant.  Note that we only have 16 bits to work with, and
	 that if you add a flag, there are numerous places in extents.c that
	 need to know about it.
       */
      unsigned int detached	: 1;  /* 1   whether it's in a buffer        */
      unsigned int destroyed	: 1;  /* 2   whether it's alive              */
      unsigned int glyph_layout	: 2;  /* 4   text, margins, or whitespace    */
      unsigned int glyph_end_p	: 1;  /* 5   start-glyph or end-glyph        */
      unsigned int start_open	: 1;  /* 6   insertion behavior at start     */
      unsigned int end_open	: 1;  /* 7   insertion behavior at end       */
      unsigned int read_only	: 1;  /* 8   text under extent is read-only  */
      unsigned int highlight	: 1;  /* 9   mouse motion highlights it      */
      unsigned int unique	: 1;  /* 10  there may be only one attached  */
      unsigned int duplicable	: 1;  /* 11  copied to strings by kill/undo  */
      unsigned int invisible	: 1;  /* 12  unimplemented                   */
      unsigned int unused_13	: 1;  /* 13				     */
      unsigned int unused_14	: 1;  /* 14				     */
      unsigned int unused_15	: 1;  /* 15				     */
      unsigned int unused_16	: 1;  /* 16				     */
      /* --- Add no more flags, lest the extent struct grow by another word. */
    } flags;
  Lisp_Object plist;
};

typedef struct extent *EXTENT;
#define XSETEXTENT(e, p) XSETR((e), extent_or_replica, (p))


#define extent_buffer(e) ((e)->ehead.buf)
#define extent_start(e) ((e)->ehead.end)
#define extent_end(e) ((e)->ehead.start)

#define extent_glyph(e)   ((e)->flags.glyph)
#define extent_face_id(e) ((e)->flags.face_id)
#define extent_priority(e) ((e)->flags.priority)


/* the layouts for glyphs (extent->flags.glyph_layout).  Must fit in 2 bits. */
#define GL_TEXT			0
#define GL_OUTSIDE_MARGIN	1
#define GL_INSIDE_MARGIN	2
#define GL_WHITESPACE		3

#define EXTENT_GLYPH_LAYOUT_P(e, layout) ((e)->flags.glyph_layout==(layout))

#define EXTENT_DETACHED_P(e)	((e)->flags.detached)
#define EXTENT_DESTROYED_P(e)	((e)->flags.destroyed)
#define EXTENT_START_GLYPH_P(e)	(extent_glyph(e) && !(e)->flags.glyph_end_p)
#define EXTENT_END_GLYPH_P(e)	(extent_glyph(e) &&  (e)->flags.glyph_end_p)
#define EXTENT_START_OPEN_P(e)	((e)->flags.start_open)
#define EXTENT_END_OPEN_P(e)	((e)->flags.end_open)
#define EXTENT_READ_ONLY_P(e)	((e)->flags.read_only)
#define EXTENT_HIGHLIGHT_P(e)	((e)->flags.highlight)
#define EXTENT_UNIQUE_P(e)	((e)->flags.unique)
#define EXTENT_DUPLICABLE_P(e)	((e)->flags.duplicable)
#define EXTENT_INVISIBLE_P(e)	((e)->flags.invisible)

/* special graphics attribute meaning "use what anyone else's attributes" */
#define GA_NO_CHANGE 0
/* this number should be bigger than any of the "real" GA's */
#define GA_MAX 0x1000


struct extent_replica
{
  struct { CONST struct lrecord_implementation *implementation; } lheader;
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


#ifdef emacs	/* things other than emacs want the structs */

extern Lisp_Object Qextent;
extern Lisp_Object Qextentp;

/* from alloc.c */
extern struct extent *make_extent (void);
extern struct extent_replica *make_extent_replica (Lisp_Object extent,
						   int start, int end);
extern Lisp_Object make_extent_internal (int from, int to, Lisp_Object buffer);

extern void update_extent_1 (EXTENT extent, int from, int to,
			     int set_endpoints, struct buffer *buf);

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
extern Lisp_Object Fextent_layout (Lisp_Object extent);
extern Lisp_Object Fset_extent_layout (Lisp_Object extent, Lisp_Object layout);

#endif /* emacs */

typedef struct extent_replica *EXTENT_REPLICA;
typedef struct extent_replica *DUP;

#define EXTENTP(x) (RECORD_TYPEP ((x), lrecord_extent))
#define EXTENT_REPLICA_P(x) (RECORD_TYPEP ((x), lrecord_extent_replica))
extern CONST struct lrecord_implementation lrecord_extent[2];
extern CONST struct lrecord_implementation lrecord_extent_replica[];


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
extern EXTENT extent_at (int pos, struct buffer *buf, Lisp_Object prop);
extern Lisp_Object Fextent_at (Lisp_Object pos, Lisp_Object buffer, 
                               Lisp_Object flag, Lisp_Object before);
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
void process_extents_for_destruction (int from, int to, struct buffer *buf);
extern Lisp_Object replicate_extents (int opoint, int length, 
                                      struct buffer *buf);
extern void splice_in_extent_replicas (int opoint, int length, int pos,
                                       Lisp_Object dup_list, 
                                       struct buffer *buf);
extern Lisp_Object merge_replicas (int number_of_lists,
                                   struct merge_replicas_struct *vec);
extern Lisp_Object shift_replicas (Lisp_Object dup_list, int offset, int length);
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
