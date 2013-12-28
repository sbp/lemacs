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

/* system */
#include "config.h"

#include <stdio.h>
#include <sys/time.h>
#include <sys/types.h>		/* some typedefs are used in sys/file.h */
#include <sys/file.h>
#include <sys/ioctl.h>		/* if not provided BLOCK_INPUT loses */
#include <signal.h>		/* must be before xterm.c ?? */
#include <string.h>
#include <errno.h>

/* editing and buffer operations */
#include "lisp.h"
#include "buffer.h" 
#include "process.h"

/* screen management */
#include "xterm.h"
#include "screen.h"
#include "window.h" 

/* Display Context for the icons */ 
#include <X11/Intrinsic.h>
#include <X11/IntrinsicP.h>
#include <X11/CoreP.h>

#ifdef ENERGIZE
#include <cdisplayctx.h>
#include <wimage.h>

/* Energize editor requests and I/O operations */
#include "editorside.h"

#include <editorreq.h>
#include <editorconn.h>
#include <editoption.h>
#endif

#include "extents-data.h"
#include "extents.h"
#include "faces.h"

#ifndef ENERGIZE
#include "hash.h"
#endif

/************** Typedefs and Structs ***********************/

struct slow_map_extents_arg
{
  Lisp_Object map_arg;
  Lisp_Object map_routine;
};

struct replicate_extents_struct
{
  int from;
  int length;
  struct buffer *buf;
  Lisp_Object head;
  Lisp_Object nconc_cell;
};

/* not used at the moment */   
struct process_extents_for_insertion_struct
{
  int opoint;
  int length;
  struct buffer *buf;
};
   
struct process_extents_for_deletion_struct
{
  int start;
  int end;
  int destroy_included_extents;
};

struct extent_at_struct
{
  EXTENT best_match;
  int flag;
};


/************** Functions ***********************/
extern DUP make_extent_replica (void);
#ifdef ENERGIZE
extern void *get_object (Id id, BufferInfo *binfo);
extern void put_object (Id id, BufferInfo *binfo, void *object);
extern void remove_object (Id id, BufferInfo *binfo);
#endif
extern Lisp_Object list_sort 
(Lisp_Object list, Lisp_Object lisp_arg, 
 int (*pred_fn)(Lisp_Object x, Lisp_Object y, Lisp_Object arg));

static void check_from_to (int from, int to, struct buffer* buf);
static int adjust_extent_index (int i, int from, int to, int amount);
static int extent_index_offset (int index, int offset, struct buffer *buf);
static int extent_index_to_buffer_pos (int i, struct buffer *buf);
static int buffer_pos_to_extent_index (int pos, struct buffer *buf);
static EXTENT buffer_starting_extent (int index, struct buffer *buf);
void adjust_extents (int old_gap, int new_gap, int gap_size, 
                     struct buffer *buf);
static int verify_extent_mf (EXTENT extent, void *dummy);
void verify_extent_modification (struct buffer *buf, int from, int to);
static void splice_extent_into_buffer (EXTENT extent, Lisp_Object obj);
static Lisp_Object make_extent_internal 
(int start, int end, Lisp_Object obj, Extent_Data *ext);
int extent_endpoint (EXTENT extent, int endp);
static void set_extent_flags (EXTENT extent);
static void set_extent_attributes_index (EXTENT extent);
static void install_extent_glyphs (EXTENT extent);
static void restore_extent_state (EXTENT extent);
static long extent_to_generic_id (Lisp_Object extent_obj);
void detach_extent (EXTENT extent);
void update_extent (EXTENT extent, int from, int to, int set_endpoints,
		    struct buffer *buf);
#ifdef ENERGIZE
Lisp_Object make_extent_for_data (BufferInfo *binfo, Extent_Data *ext,
                                  int from, int to, int set_endpoints);
#endif
void map_extents (int from, int to, elisp_emf efn, emf fn, void *arg, 
                  struct buffer *buf, int closed_end);
static int slow_map_extents_function (Lisp_Object extent_obj, void *arg);
Lisp_Object Fmap_extents (Lisp_Object function, Lisp_Object buffer, 
                          Lisp_Object from, Lisp_Object to, 
                          Lisp_Object maparg, Lisp_Object closed_end);
int extent_highlightable_p (Lisp_Object extent_obj);
static Lisp_Object do_highlight (Lisp_Object extent_obj, int flag);
Lisp_Object Fhighlight_extent (Lisp_Object extent_obj, Lisp_Object flag);
Lisp_Object Fforce_highlight_extent (Lisp_Object extent_obj, Lisp_Object flag);
Lisp_Object Fextent_start_position (Lisp_Object extent_obj);
Lisp_Object Fextent_end_position (Lisp_Object extent_obj);
Lisp_Object Fextent_length (Lisp_Object extent_obj);
Lisp_Object Fextent_buffer (Lisp_Object extent_obj);
int glyph_in_column_p (Lisp_Object extent_obj);
Lisp_Object Frestore_extent (Lisp_Object extent_obj);
static void set_point_internal (int charno);
void set_point (int position);
void set_buffer_point (struct buffer *buffer,  int position);
int last_visible_position (int opoint, struct buffer *buf);
static int extent_at_mf (EXTENT extent, void *arg);
EXTENT extent_at (int pos, struct buffer *buf, int flag);
Lisp_Object Fextent_at (Lisp_Object pos, Lisp_Object buffer, Lisp_Object flag);

static void soe_push (EXTENT extent, struct buffer *b);
static void soe_duplicate (int pos, EXTENT *copy_from, int copy_from_size, 
			   EXTENT trial_prev, struct buffer *b);
static void soe_delq (EXTENT extent, struct buffer *b);
static void soe_prune (struct buffer *b);
static void init_buffer_cached_stack (struct buffer *b);
static void soe_clear (struct buffer *b);
void free_buffer_cached_stack (struct buffer *b);
static EXTENT_FRAGMENT befa_internal (int pos, struct buffer *buf);
EXTENT_FRAGMENT buffer_extent_fragment_at (int pos, struct buffer *buf,
                                           struct screen *s);
static void init_extent_fragment (void);
void process_extents_for_insertion (int opoint, int length, 
                                    struct buffer *buf);
static int process_extents_for_deletion_mf (EXTENT extent, void *arg);
void process_extents_for_deletion (int from, int to, int start, int end,
				   struct buffer *buf);
void process_extents_for_destruction (int from, int to, struct buffer *buf);
static int replicate_extents_mf (EXTENT extent, void *arg);
Lisp_Object replicate_extents (int opoint, int length, struct buffer *buf);
void splice_in_extent_replicas (int opoint, int length, 
                                Lisp_Object dup_list, struct buffer *buf);
Lisp_Object merge_replicas (int number_of_lists, 
                            struct merge_replicas_struct *vec);
static void add_to_replicas_lists (c_hashtable table, Lisp_Object dup_list, 
                                   int offset, int length);
static void merge_replicas_concating_mf (void *key, void *contents, void *arg);
static int mrp_pred (Lisp_Object x, Lisp_Object y, Lisp_Object dummy);
static void merge_replicas_pruning_mf (void *key, void *contents, void *arg);
void syms_of_extents (void);


/************** Macros ***********************/

#define max(A,B) ((A) > (B) ? (A) : (B))
#define min(A,B) ((A) <= (B) ? (A) : (B))

#define umin(a,b) ((((unsigned long) (a)) < ((unsigned long) (b)))?(a):(b))
#define umax(a,b) ((((unsigned long) (a)) > ((unsigned long) (b)))?(a):(b))

#define MAX_ULONG ((unsigned long) -1)
#define MAX_INT ((long) 0x7fffffff)
   
#define BUFNAME(buf) &(XSTRING(buf->name)->data[0])
#define SYMNAME(sym) &(XSYMBOL(sym)->name->data[0])

/* if buf is the one in the cache, invalidate the cache -- used in
   places where changes to extents list occur that might not affect 
   the buffer modiff */
#define CHECK_RESET_EXTENT_FRAGMENT(x) \
{if (((char *) extent_fragment.buf) == ((char *)(x))) \
  {extent_fragment.buf = 0; Vextent_fragment_buffer = Qnil;}}

#define EXTENT_LESS_VALS(e,st,nd) (((e)->start < (st)) || \
				   (((e)->start == (st)) && \
				   ((e)->end > (nd))))

#define EXTENT_LESS(e,x) EXTENT_LESS_VALS(e, (x)->start, (x)->end)

#define EXTENT_LESS_EQ(e,x) (((e)->start < (x)->start) || \
			     (((e)->start == (x)->start) && \
			     ((e)->end >= (x)->end)))

#define EXTENT_E_LESS_VALS(e,st,nd) (((e)->end < (nd)) || \
				     (((e)->end == (nd)) && \
				     ((e)->start < (st))))

#define EXTENT_E_LESS(e,x) EXTENT_E_LESS_VALS(e,(x)->start,(x)->end)

#define EXTENT_OVER_INDEX(e,i) ((((e)->start <= (i)) && ((e)->end > (i))) || \
				(((e)->start == (i)) && ((e)->end == (i))))
  
#define EXTENT_PAST_INDEX(e,i) (((e)->end > (i)) || \
				((((e)->start == (i)) && ((e)->end == (i)))))

/************** Variables ***********************/

Lisp_Object Vlast_highlighted_extent;

static Lisp_Object Vextent_fragment_buffer;
static struct extent_fragment extent_fragment;
static struct extent_fragment default_extent_fragment;



/*********************************
  EXTENTS.C INTERNAL UTILITIES
  *******************************/

static void
check_from_to (int from, int to, struct buffer* buf)
{ 
  if ((from < BUF_BEG (buf)) || (from > BUF_Z (buf))) 
    error ("Bad buffer position %d for buffer %s", 
           from, BUFNAME(buf)); 
  if ((to < BUF_BEG (buf)) || (to > BUF_Z (buf))) 
    error ("Bad buffer position %d for buffer %s", 
	   to, BUFNAME(buf));
} 

static int
adjust_extent_index (int i, int from, int to, int amount)
{
  /* Some sanity checking */
  if (amount > 0)
    {
      if (i > to && i < to + amount)
	abort ();
    }
  else 
    {
      if (i > from + amount && i < from)
	abort ();
    }

  if (i >= from && i <= to)
    i += amount;

  return i;
}

static int
extent_index_offset (int index, int offset, struct buffer *buf)
{
  int pos = extent_index_to_buffer_pos (index, buf);
  return buffer_pos_to_extent_index (pos + offset, buf);
}

static int
extent_index_to_buffer_pos (int i, struct buffer *buf)
{
  if (i > BUF_GPT (buf) + BUF_GAP_SIZE (buf))
    i -= BUF_GAP_SIZE (buf);
  else if (i > BUF_GPT (buf))
    i = BUF_GPT (buf);

  if ((i < BUF_BEG (buf)) || (i > BUF_Z (buf)))
    abort();
  else
    return i;
}

static int
buffer_pos_to_extent_index (int pos, struct buffer *buf)
{
  if ((pos < BUF_BEG (buf)) || (pos > BUF_Z (buf)))
    abort();
  else if (pos <= BUF_GPT (buf))
    return pos;
  else 
    return (pos + BUF_GAP_SIZE (buf));
}

static EXTENT 
buffer_starting_extent (int index, struct buffer *buf)
{
  if (XTYPE (buf->extents) != Lisp_Extent)
    return 0;
  else
    return XEXTENT (buf->extents);
}

void
adjust_extents (int from, int to, int amount, struct buffer* buf)
{
  EXTENT current;

  if (NILP (buf->extents))
    return;
  else if (XTYPE (buf->extents) != Lisp_Extent)
    abort ();

  current = buffer_starting_extent (from, buf);
  
  CHECK_RESET_EXTENT_FRAGMENT (buf);
  
  while (current)
    {
      if (current->end < from)
	;
      else if (current->start <= to)
	{
	  current->start = adjust_extent_index (current->start, from, to,
						amount);
	  if (current->end <= to)
	    current->end = adjust_extent_index (current->end, from, to,
						amount);
	}
      else 
	break;
      
      current = current->next;
    }
  
  if (buf->cached_stack->buf_index > 0
      && buf->cached_stack->buf_index > from
      && buf->cached_stack->buf_index <= to)
    buf->cached_stack->buf_index += amount;
}

struct end_points
{
  int start;
  int end;
};

static int
verify_extent_mf (EXTENT extent, void *arg)
{
  struct end_points* ep = (struct end_points*)arg;
  
  if ((EXTENT_FLAGS(extent) & EF_WRITE_PROTECT)
      && (extent->end > ep->start)
      && (extent->start <= ep->end))
    {
      error ("Attempt to modify a read-only region");
      return 1;
    }
  else
    return 0;
}

void
verify_extent_modification (struct buffer *buf, int from, int to)
{
  struct end_points ep;

  check_from_to (from, to, buf);
  ep.start = buffer_pos_to_extent_index (from, buf);
  ep.end = buffer_pos_to_extent_index (to, buf);

  if (inside_undo ||
#ifdef ENERGIZE
      inside_parse_buffer ||
#endif
      NILP (buf->extents))
    return;
  else if (XTYPE (buf->extents) != Lisp_Extent)
    abort();
  else
    map_extents (from, to, 0, verify_extent_mf, (void*)&ep, buf, 0);
}

/* At the moment only this function, buffer_extent_fragment_at(), 
   update_cache_forward(), map_extents() and adjust_extents (),
   know how to "cdr" down a list of extents. 
   See the comment at map_extents() for information 
   about the ordering rule. */
static void 
splice_extent_into_buffer (EXTENT extent, Lisp_Object buffer)
{
  Lisp_Object extents_root;
  Lisp_Object extent_obj;
  struct buffer *buf = XBUFFER(buffer);

  CHECK_BUFFER (buffer, 0);  
  CHECK_RESET_EXTENT_FRAGMENT (buf);
   
  XSET (extent_obj, Lisp_Extent, extent);

  init_buffer_cached_stack (buf);

  extents_root = buf->extents;

  if (NILP (extents_root))
    buf->extents = extent_obj;
  else if (XTYPE (extents_root) != Lisp_Extent)
    abort();
  else
    {
      int start = extent->start;
      int end = extent->end;
      EXTENT tmp = buffer_starting_extent (start, buf);
      EXTENT prev = 0;

      while (tmp && EXTENT_LESS_VALS (tmp, start, end))
        {
          prev = tmp;
          tmp = tmp->next;
        }

      if (!tmp && !prev)
        abort();

      if (prev)
        { 
          EXTENT caboose = prev->next;
          prev->next = extent; 
          extent->previous = prev; 
          extent->next = caboose; 
          if (caboose)
            caboose->previous = extent;
        }
      else 
        {
          EXTENT engine = tmp->previous;
          if (engine)
            engine->next = tmp;
          extent->previous = engine; 
          extent->next = tmp; 
          tmp->previous = extent; 
        } 

      if (!prev)
        buf->extents = extent_obj;

      if (!tmp)
        tmp = prev;
      prev = 0;

      if (EXTENT_E_LESS_VALS (tmp, start, end))
        while (tmp && EXTENT_E_LESS_VALS (tmp, start, end))
          {
            prev = tmp;
            tmp = tmp->e_next;
          }
      else
        {
          prev = tmp;
	  while (prev && !EXTENT_E_LESS_VALS (prev, start, end))
            {
              tmp = prev;
              prev = tmp->e_previous;
            }
        }

      if (!tmp && !prev)
        abort();

      if (prev)
        { 
          EXTENT caboose = prev->e_next;
          prev->e_next = extent; 
          extent->e_previous = prev; 
          extent->e_next = caboose; 
          if (caboose)
            caboose->e_previous = extent;
        }
      else 
        {
          EXTENT engine = tmp->e_previous;
          if (engine)
            engine->e_next = tmp;
          extent->e_previous = engine; 
          extent->e_next = tmp; 
          tmp->e_previous = extent; 
        }
    }

  soe_push (extent, buf);
  CLEAR_EXTENT_FLAG (extent, EF_DETACHED);
}

static Lisp_Object
make_extent_internal (int from, int to, Lisp_Object buffer, Extent_Data *ext)
{
  EXTENT extent;
  Lisp_Object extent_obj = Qnil;
  struct gcpro gcpro1;
  struct buffer *buf = XBUFFER (buffer);

  CHECK_BUFFER (buffer, 0);  
  check_from_to (from, to, buf);
   
  if ((from < 0) || (to < from))
    error ("START == %d, END == %d -- bad start/end for extent",
           from, to);

  if (NILP (buf->name))
    error ("Can't put an extent in a killed buffer");

  if ((from < BUF_BEG(buf)) || (to > BUF_Z(buf)))
    error ("START == %d, END == %d -- bad start/end for extent in buffer %s",
           from, to, BUFNAME(buf));
    
  if (ext && ext->extent)
    error ("Already an extent (0x%x) with this data (0x%x)",
           ext->extent, ext);

  extent = make_extent();
  XSET(extent_obj, Lisp_Extent, extent);

  extent->buffer = buffer;
  extent->data = (void *) ext;
  extent->flags = 0;

  extent->start = buffer_pos_to_extent_index (from, buf);
  extent->end = buffer_pos_to_extent_index (to, buf);

  splice_extent_into_buffer (extent, buffer);

  if (ext)
    ext->extent = extent_obj;

  restore_extent_state (extent);
  return extent_obj;
}


int 
extent_endpoint (EXTENT extent, int endp)
{
  int i = (endp)?(extent->end):(extent->start);
  if (EXTENT_DESTROYED_P (extent))
    return -1;
  else if (EXTENT_DETACHED_P (extent))
    return 0;
  else if (XTYPE (extent->buffer) == Lisp_Buffer)
    return extent_index_to_buffer_pos (i, XBUFFER(extent->buffer));
  else
    return i;
}

static void
set_extent_flags (EXTENT extent)
{
  Extent_Data *ext = (Extent_Data *) extent->data;

  /* clear every flag except the EF_DETACHED flag */
  if (EXTENT_DESTROYED_P (extent))
    return;
  else if (EXTENT_DETACHED_P (extent))
    extent->flags = EF_DETACHED;
  else
    extent->flags = 0;

#ifdef ENERGIZE
  if (ext)
    {
      switch (ext->extentType)
	{
	case CEAttribute:
	  break;

	case CEAbbreviation:
	  break;

	case CEWriteProtect:
	  SET_EXTENT_FLAG (extent, EF_WRITE_PROTECT);
	  break;
          
	case CEGeneric:
	  if (ext->u.generic.gData->id)
	    SET_EXTENT_FLAG (extent, EF_MENU);
	  if (ext->u.generic.gData->image)
	    SET_EXTENT_FLAG (extent, EF_END_GLYPH);
	  if (ext->u.generic.gData->cl && ext->u.generic.gData->cl->image)
	    SET_EXTENT_FLAG (extent, EF_START_GLYPH);
	  if (ext->u.generic.gData->cl && 
	      (ext->u.generic.gData->cl->flags & CCElectric))
	    SET_EXTENT_FLAG (extent, EF_HIGHLIGHT);
	  if (ext->u.generic.gData->cl && 
	      (ext->u.generic.gData->cl->flags & CCWarnModified))
	    SET_EXTENT_FLAG (extent, EF_WARN_MODIFY);
	  if (ext->u.generic.gData->cl && 
	      (ext->u.generic.gData->cl->flags & CCColumn))
	    SET_EXTENT_FLAG (extent, EF_COLUMN);
	  SET_EXTENT_FLAG (extent, EF_DUPLICABLE);
	  break;
          
	default:
	  break;
	}
    }
#endif /* ENERGIZE */
}

static void
set_extent_attributes_index (EXTENT extent)
{
  Extent_Data *ext = (Extent_Data *) extent->data;
  int graphic_attributes;

  if (!ext) 
    extent->attr_index = -1;
#ifdef ENERGIZE
  else
    {
      switch (ext->extentType)
        {
        case CEAttribute:
          graphic_attributes = ext->u.attr.attrValue;
          break;
      
        case CEGeneric:
          graphic_attributes = ext->u.generic.gData->attribute;
          break;
      
        case CEWriteProtect:
          /* this type has NO display attributes */
          extent->attr_index = -1;
          return;
      
        default:
          graphic_attributes = GA_NO_CHANGE;
        }
  
      if (graphic_attributes >= GA_MAX) 
        graphic_attributes = GA_NO_CHANGE;

      extent->attr_index = graphic_attributes;
    }
#endif /* ENERGIZE */
  return;
}

static void
install_extent_glyphs (EXTENT extent)
{
#ifdef ENERGIZE
  Extent_Data *ext = (Extent_Data *) extent->data;

  if (!ext || (ext->extentType != CEGeneric))
    return;
  
  if (ext->u.generic.gData->cl)
    install_extent_IMAGE (extent, ext->u.generic.gData->cl->image,
			  EGT_START_GLYPH);
  install_extent_IMAGE (extent, ext->u.generic.gData->image, EGT_END_GLYPH);
#endif
}

static void
restore_extent_state (EXTENT extent)
{
  set_extent_flags (extent);
  set_extent_attributes_index (extent);
  install_extent_glyphs (extent);
}

#ifdef ENERGIZE
static long
extent_to_generic_id (Lisp_Object extent_obj)
{
  if (!EXTENTP (extent_obj)) 
    return 0;
  else 
    {
      Extent_Data *ext = (Extent_Data *) XEXTENT(extent_obj)->data;
      if (ext && ext->u.generic.gData)
        return (long) ext->u.generic.gData->id;
      else
        return 0;
   }
}
#endif /* ENERGIZE */


/*********************************
  EXTENTS.C EXTERNAL UTILITIES
  *******************************/

void 
detach_extent (EXTENT extent) 
{ 
  EXTENT prev = extent->previous; 
  EXTENT next = extent->next; 
  EXTENT e_prev = extent->e_previous; 
  EXTENT e_next = extent->e_next; 
  struct buffer *buf = XBUFFER(extent->buffer);
  Lisp_Object obj_extent = buf->extents;

  CHECK_RESET_EXTENT_FRAGMENT (buf);

  soe_delq (extent, buf);

  if (XEXTENT (obj_extent) == extent)
    {
      if (next)
        XSET (obj_extent, Lisp_Extent, next);
      else
        obj_extent = Qnil;
      
      XBUFFER (extent->buffer)->extents = obj_extent;
    }

  if (prev) 
    {
      prev->next = next; 
      extent->previous = 0;
    }
  if (next) 
    {
      next->previous = prev; 
      extent->next = 0; 
    }


  if (e_prev) 
    {
      e_prev->e_next = e_next; 
      extent->e_previous = 0;
    }
  if (e_next) 
    {
      e_next->e_previous = e_prev; 
      extent->e_next = 0; 
    }

  SET_EXTENT_FLAG (extent, EF_DETACHED);

  extent->start = 0;
  extent->end = 0;
}

void 
destroy_extent (EXTENT extent) 
{ 
  detach_extent (extent);
#ifdef ENERGIZE
  energize_extent_finalization (extent);
#endif
  extent->flags = EF_DESTROYED;
  extent->secondary_type = 0;
  extent->start = 0;
  extent->end = 0;
  extent->next = 0;
  extent->previous = 0;
  extent->e_next = 0;
  extent->e_previous = 0;
  extent->data = 0;
  extent->attr_index = 0;
  extent->buffer = Qnil;
}

void   
update_extent (EXTENT extent, int from, int to, int set_endpoints,
	       struct buffer *buf)
{
  Extent_Data *ext = (Extent_Data *) extent->data;
  
  if ((XTYPE (extent->buffer) != Lisp_Buffer) ||
      (XBUFFER(extent->buffer) != buf))
    error ("extent 0x%x not part of specified buffer %s", extent, 
           BUFNAME (buf));

  if (set_endpoints)
    {
      check_from_to (from, to, buf);
      
      /* most of the time the extent doesn't need to be changed -- the
	 big problem is that the kernel actually doesn't know what is going
	 all too often */
      if ((from < to) && ((EXTENT_FLAGS (extent) & EF_DETACHED) ||
			  (extent_endpoint (extent, 0) != from) ||
			  (extent_endpoint (extent, 1) != to)))
	{
	  int new_start = buffer_pos_to_extent_index (from, buf);
	  int new_end = buffer_pos_to_extent_index (to, buf);
	  Lisp_Object buffer;
	  XSET (buffer, Lisp_Buffer, buf);
	  
	  detach_extent (extent);
	  extent->start = new_start;
	  extent->end = new_end;
	  splice_extent_into_buffer (extent, buffer);
	}
    }

  restore_extent_state (extent);
  return;
}

#ifdef ENERGIZE

/* creates a new extent or update an old one for ext in binfo and returns it */
Lisp_Object
make_extent_for_data (BufferInfo *binfo, Extent_Data *ext, int from, int to,
		      int set_endpoints)
{
  Lisp_Object first, last;
  Lisp_Object extent_obj;
  Lisp_Object buffer = binfo->emacs_buffer;
  struct buffer *b = XBUFFER (buffer);
  
  to = min (to, BUF_Z (b));
  from = min (from, to);
  
  if (extent_obj = ext->extent)
    update_extent (XEXTENT (extent_obj), from, to, set_endpoints,
		   XBUFFER (buffer));
  else
    extent_obj = make_extent_internal (from, to, buffer, ext);
  
  SET_EXTENT_FLAG (XEXTENT (extent_obj), EF_DUPLICABLE);

  return extent_obj;
}

#endif /* ENERGIZE */


/*********************************
  EXTENTS.C MAPPING FUNCTIONS
  *******************************/


/* Apply a function to each extent overlapping [from, to) (or [from, to],
   if the closed_end arg is non-zero) in buffer. If the function ever
   returns a non-zero value, quit immediately.  At the moment only this
   function, buffer_extent_fragment_at(), update_cache_forward(), 
   adjust_extents () and splice_extent_into_buffer() 
   knows how to "cdr" down a list of extents.
   Extents are ordered with increasing start position and then decreasing
   end position. (This is what might be called "display order" -- if extent
   A occurs after extent B, then the display attributes of extent A
   override those of extent B in the region covered by both A and B. Note
   that multiple extents with the same start and end postions may be in any
   order.) Part of this comment is duplicated at update_cache_forward().*/
void
map_extents 
(int from, int to, elisp_emf efn, emf fn, void *arg, struct buffer *buf, 
 int closed_end)
{
  Lisp_Object extents_root = buf->extents;
  int start;
  int end;

  if ((from > to) || (NILP (extents_root)))
    return;
  
  /* making an error here is wrong as this can be called from within
     * make_gap () during which the buffer state is incoherent, ie the new
     * GPT is already set but the new Z is not. 
     * This is a little scary, maybe the function that updates the
     * extents when the gap is moved should not call map_extents. 
     * --Matthieu. */
  /* check_from_to(from, to, buf); */
  /* We silently return if mapping out of the buffer valid positions */
  if ((from < BUF_BEG (buf)) || (from > BUF_Z (buf))
      || (to < BUF_BEG (buf)) || (to > BUF_Z (buf)))
    return;

  start = buffer_pos_to_extent_index (from, buf);
  if (closed_end)
    {
      if (to == BUF_Z(buf))
        end = buffer_pos_to_extent_index (to, buf) + 1;
      else
        end = buffer_pos_to_extent_index (to + 1, buf);
    }
  else
    end = buffer_pos_to_extent_index (to, buf);

  if (XTYPE (extents_root) != Lisp_Extent)
    abort();
  else 
    {
      EXTENT tmp = buffer_starting_extent (start, buf);
      EXTENT next;

      if (efn)
        while (tmp)
          {
            /* this lets the map function be delete_extent, too */
            next = tmp->next;
            if (tmp->end < start)
              tmp = next;
            else if (tmp->start < end)
              {
                Lisp_Object tmp_obj;
                XSET (tmp_obj, Lisp_Extent, tmp);
                if ((*efn)(tmp_obj, arg))
                  return;
                tmp = next; 
              }
            else 
              return;
          }
      else
        while (tmp)
          {
            /* this lets the map function be delete_extent, too */
            next = tmp->next;
            if (tmp->end < start)
              tmp = next;
            else if (tmp->start < end)
              {
                if ((*fn)(tmp, arg))
                  return;
              }
            else 
              return;
   
            tmp = next;
          }
    }
  return;
}

static int
slow_map_extents_function (Lisp_Object extent_obj, void *arg)
{
  struct slow_map_extents_arg *slow_arg =
    (struct slow_map_extents_arg *) arg;
  if (NILP (call2 (slow_arg->map_routine, extent_obj, slow_arg->map_arg)))
    return 0;
  else
    return 1;
}

DEFUN ("map-extents", Fmap_extents, Smap_extents, 1, 6, 0,
       "Usage: (map-extents FUNCTION BUFFER FROM TO MAPARG) \n\
Map FUNCTION over the extents which overlap region in BUFFER starting at\n\
 FROM and ending at TO.  FUNCTION is called with arguments (extent, MAPARG).\n\
All arguments except FUNCTION are optional, with FROM, TO, MAPARG, and\n\
 BUFFER defaulting to the beginning of BUFFER, the end of BUFFER, NIL, and\n\
 current buffer, respectively.  MAP-EXTENTS always returns nil.")
  (function, buffer, from, to, maparg, closed_end)
  Lisp_Object function, buffer, from, to, maparg, closed_end;
{
  elisp_emf map_funct;
  int start, end, closed;
  
  if (!NILP (closed_end))
    closed = 1;
  else 
    closed = 0;

  if (NILP (buffer)) XSET (buffer, Lisp_Buffer, current_buffer);
  CHECK_BUFFER (buffer, 0);
  

  if (NILP (from)) XSET (from, Lisp_Int, BUF_BEG (XBUFFER (buffer)));
  if (NILP (to)) XSET (to, Lisp_Int, BUF_Z (XBUFFER (buffer)));
  CHECK_NUMBER (from, 1);
  CHECK_NUMBER (to, 2);
  check_from_to (XINT(from), XINT(to), (XBUFFER (buffer)));
  if (XINT(from) > XINT (to))
    error ("MAP-EXTENTS args FROM (== %d) and TO (== %d) are inconsistent.",
           XINT (from), XINT (to));
  
  if (XTYPE (function) == Lisp_Subr)
    {
      map_funct = (elisp_emf) XSUBR (function)->function;
      map_extents (XINT (from), XINT (to), map_funct, 0,
                   (void *)maparg, XBUFFER(buffer), closed);
    }
  else
    {
      struct slow_map_extents_arg sma_space;
      sma_space.map_arg = maparg;
      sma_space.map_routine = function;
      map_funct = slow_map_extents_function;
      map_extents 
        (XINT (from), XINT (to), map_funct, 0, (void *) &sma_space, 
         XBUFFER(buffer), closed);
    }
  
  return Qnil;
}


/*********************************
  EXTENTS.C GRAPHICAL DISPLAY
  *******************************/
int
extent_highlightable_p (Lisp_Object extent_obj)
{
 return (EXTENTP (extent_obj) && 
         (EXTENT_FLAGS (XEXTENT (extent_obj)) & EF_HIGHLIGHT));
}

/* The display code looks into the Vlast_highlighted_extent variable to 
   correctly display highlighted extents. NOTE: When we have extents
   that are either open or closed at either end, we will need to
   figure out the endpoints of the call to redisplay_no_change() 
   accordingly. */
static Lisp_Object
do_highlight (Lisp_Object extent_obj, int flag)
{
  if (((Vlast_highlighted_extent == extent_obj) && !NILP(flag)) ||
      (NILP (Vlast_highlighted_extent) && NILP (flag)))
    return;
  else
    {
      Lisp_Object old_parent = 
        (EXTENTP (Vlast_highlighted_extent))?
          (XEXTENT(Vlast_highlighted_extent)->buffer):Qnil;
      Lisp_Object new_parent = 
        (EXTENTP (extent_obj))?(XEXTENT(extent_obj)->buffer):Qnil;

      if (XTYPE(old_parent) == Lisp_Buffer)
        {
          Vlast_highlighted_extent = Qnil;
          BUF_FACECHANGE (XBUFFER (old_parent))++;
          windows_or_buffers_changed++;
        }

      if ((XTYPE(new_parent) == Lisp_Buffer) &&
          !NILP (flag))
        {
          Vlast_highlighted_extent = extent_obj;
          BUF_FACECHANGE (XBUFFER (new_parent))++;
          windows_or_buffers_changed++;
        }
      else
        Vlast_highlighted_extent = Qnil;

      return Qnil;
    }
}

DEFUN ("highlight-extent", Fhighlight_extent, Shighlight_extent, 1, 2, 0,
 "If EXTENT is `highlightable' (has the 'highlight property) then highlight\n\
it (by using merging it with 'highlight face.)  If FLAG is nil, then\n\
unhighlight it instead.")
     (extent_obj, flag)
     Lisp_Object extent_obj, flag;
{
  if (NILP (extent_obj))
    do_highlight (Qnil, Qnil);
  else if (!extent_highlightable_p (extent_obj))
    return Qnil;
  else
    return do_highlight (extent_obj, flag);
}


DEFUN ("force-highlight-extent", Fforce_highlight_extent, 
       Sforce_highlight_extent, 1, 2, 0,
 "Highlight any EXTENT if FLAG is not nil, else unhighlight it.\n\
This is the same as `highlight-extent', except that it will work even\n\
on extents without the 'highlight property.")
     (extent_obj, flag)
     Lisp_Object extent_obj, flag;
{
  if (NILP (extent_obj))
    do_highlight (Qnil, Qnil);
  else if (!EXTENTP (extent_obj))
    return Qnil;
  else
    return do_highlight (extent_obj, flag);
}


/*********************************
  EXTENTS.C DATATYPE FUNCTIONS
  *******************************/

Lisp_Object
extent_glyph_at (EXTENT extent, int pos, int endp)
{
  if (extent && extent->data && (XTYPE(extent->buffer) == Lisp_Buffer))
    {
      Extent_Data *ext = (Extent_Data *) extent->data;

      if (!ext)
	return Qnil;

#ifdef ENERGIZE
      if (endp)
        {
          if (pos == (extent_endpoint (extent, 1) - 1))
            return ext->end_glyph_index;
          else
            return Qnil;
        }
      else
        {
          if (pos == extent_endpoint (extent, 0))
            return ext->start_glyph_index;
          else
            return Qnil;
        }
#endif /* ENERGIZE */
    }
  else
    return Qnil;
}

DEFUN ("extent-start-position", Fextent_start_position, 
       Sextent_start_position, 1, 1, 0,
       "Return start position of EXTENT.")
     (extent_obj)
     Lisp_Object extent_obj;
{
  CHECK_EXTENT (extent_obj, 0);
  return make_number (extent_endpoint (XEXTENT(extent_obj), 0));
}


DEFUN ("extent-end-position", Fextent_end_position, 
       Sextent_end_position, 1, 1, 0,
       "Return first position after EXTENT.")
     (extent_obj)
     Lisp_Object extent_obj;
{
  CHECK_EXTENT (extent_obj, 0);
  return make_number (extent_endpoint (XEXTENT(extent_obj), 1));
}

DEFUN ("extent-length", Fextent_length, Sextent_length, 1, 1, 0,
       "Return length of EXTENT in characters.")
     (extent_obj)
     Lisp_Object extent_obj;
{
  CHECK_EXTENT (extent_obj, 0);
  return make_number (extent_endpoint (XEXTENT(extent_obj), 1) - 
                      extent_endpoint (XEXTENT(extent_obj), 0));
}

DEFUN ("extent-buffer", Fextent_buffer, Sextent_buffer, 1, 1, 0,
       "Return buffer of EXTENT.")
     (extent_obj)
     Lisp_Object extent_obj;
{
  CHECK_EXTENT (extent_obj, 0);
  return (XEXTENT(extent_obj)->buffer);
}


#ifdef ENERGIZE 

DEFUN ("extent-to-generic-id", Fextent_to_generic_id, Sextent_to_generic_id,
       1, 1, 0, "Return Energize ID of buffer of EXTENT.")
     (extent_obj)
     Lisp_Object extent_obj;
{
  long gid;
  extern Lisp_Object word_to_lisp ();
  CHECK_EXTENT (extent_obj, 0);
  gid = extent_to_generic_id (extent_obj);
  return word_to_lisp (gid);
}

#endif


/*********************************
  EXTENTS.C FLAGS INFO
  *******************************/

/* Does this extent have a lineinfo-column glyph */   
int
glyph_in_column_p (Lisp_Object extent_obj)
{
  CHECK_EXTENT (extent_obj, 0);
  return !!(EXTENT_FLAGS(XEXTENT(extent_obj)) & EF_COLUMN);
}

#if 0

DEFUN ("restore-extent", Frestore_extent, Srestore_extent, 1, 1, 0,
       "Reset extent graphics attributes and other properties.")
     (extent_obj)
     Lisp_Object extent_obj;
{
  if (EXTENTP (extent_obj))
    restore_extent_state (XEXTENT(extent_obj));
  return Qnil;
}

#endif

/* This is the only place `PT' is an lvalue in all of emacs. */

static void
set_point_internal (charno)
{
  point = charno;
  if (charno > GPT)
    charno += GAP_SIZE;
  XMARKER (current_buffer->point_marker)->bufpos = charno;
}

void
set_point (position)
     int position;
{
  int opoint = point;
  Lisp_Object obj;

  if (position == opoint)
    return;
  else if (position > Z)
    abort();

  if (position > ZV || position < BEGV)
    abort ();

  set_point_internal (position);

  if (!current_buffer->extents)
    abort();

  /* NOTE: We used to run "class hooks" at this point, and we
     will want to run extent hooks here in the future, both for
     point-entered and point-exited. */
}

void
set_buffer_point (buffer, position)
     struct buffer *buffer;
     int position;
{
  if (buffer == current_buffer)
    set_point (position);
  else
    {
      void internal_set_buffer (); /* in buffer.c */
      int count = specpdl_ptr - specpdl;
      /* bad news to pass non-lisp-object to this? */
      record_unwind_protect (internal_set_buffer, current_buffer);
      internal_set_buffer (buffer);
      set_point (position);
      unbind_to (count);
    }
}

/* Don't have invisible extents at the moment */
int
last_visible_position (int opoint, struct buffer *buf)
{
  return opoint;
}


static int
extent_at_mf (EXTENT extent, void *arg)
{
  struct extent_at_struct *eas_ptr = (struct extent_at_struct *) arg;

  if ((eas_ptr->flag == 0) || (EXTENT_FLAGS (extent) & eas_ptr->flag))
    {
      EXTENT current = eas_ptr->best_match;

      if (!current)
        eas_ptr->best_match = extent;
      else if (current->start > extent->start)
        return 0;
      /* we return the "last" best fit, instead of the first --
	 this is because then the glyph closest to two equivalent
	 extents corresponds to the "extent-at" the text just past
	 that same glyph */
      else if (EXTENT_LESS_EQ (current, extent))
        eas_ptr->best_match = extent;
    }

  return 0;
}

/* find "smallest" matching extent containing pos -- (flag == 0) means 
   all extents match, else (EXTENT_FLAGS (extent) & flag) must be true;
   for more than one matching extent with precisely the same endpoints,
   we choose the last extent in the extents_list */
EXTENT
extent_at (int pos, struct buffer *buf, int flag)
{
  if (NILP (buf->extents))
    return 0;
  else if (XTYPE (buf->extents) != Lisp_Extent)
    abort();
  else
    {
      struct extent_at_struct eas;
      eas.best_match = 0;
      eas.flag = flag;
      
      map_extents (pos, pos, 0, extent_at_mf, (void *) &eas, buf, 1);
      return eas.best_match;
    }
}

DEFUN ("make-extent", Fmake_extent, Smake_extent, 2, 3, 0,
       "Make extent for range [FROM, TO) in BUFFER -- BUFFER defaults to \n\
current buffer.  Insertions at point TO will be outside of the extent;\n\
insertions at FROM will be inside the extent (and the extent will grow.)")
  (from, to, buffer)
   Lisp_Object from, to, buffer;
{
  Lisp_Object extent_obj;
  if (NILP (buffer))
    XSET (buffer, Lisp_Buffer, current_buffer);
  extent_obj = make_extent_internal (XINT(from), XINT(to), buffer, 0);
  return extent_obj;
}

DEFUN ("delete-extent", Fdelete_extent, Sdelete_extent, 1, 1, 0,
 "Remove EXTENT from its buffer; this does not modify the buffer's text,\n\
only its display properties.")
  (extent_obj)
   Lisp_Object extent_obj;
{
  EXTENT extent;

  CHECK_EXTENT (extent_obj, 0);
  extent = XEXTENT (extent_obj);

  if (NILP (XBUFFER (extent->buffer)->name))
    error ("Deleting extent in killed buffed");

  BUF_FACECHANGE (XBUFFER (extent->buffer))++;
  windows_or_buffers_changed++;
  destroy_extent (extent);
  return Qnil;
}

DEFUN ("update-extent", Fupdate_extent, Supdate_extent, 3, 3, 0,
       "Set the endpoints of EXTENT to START, END.")
  (extent_obj, start, end)
   Lisp_Object extent_obj, start, end;
{
  EXTENT extent = XEXTENT (extent_obj);
  int from = XINT (start);
  int to = XINT (end);

  CHECK_EXTENT (extent_obj, 0);
  CHECK_NUMBER (start, 1);
  CHECK_NUMBER (end, 2);

  if (XTYPE (extent->buffer) != Lisp_Buffer
      || NILP (XBUFFER (extent->buffer)->name))
    error ("EXTENT arg to UPDATE-EXTENT doesn't belong to a buffer");

  check_from_to (from, to, XBUFFER (extent->buffer));
      
  if (from > to)
    error ("Bad START (== %d) and END (== %d) args to UPDATE-EXTENT", from, to);

  detach_extent (extent);
  extent->start = buffer_pos_to_extent_index (from, XBUFFER (extent->buffer));
  extent->end = buffer_pos_to_extent_index (to, XBUFFER (extent->buffer));
  splice_extent_into_buffer (extent, extent->buffer);

  BUF_FACECHANGE (XBUFFER (extent->buffer))++;
  windows_or_buffers_changed++;

  return extent_obj;
}


DEFUN ("set-extent-attribute", Fset_extent_attribute, 
       Sset_extent_attribute, 2, 2, 0,
 "Make EXTENT have ATTRIBUTE.\n\
ATTRIBUTE must be one of the following symbols:\n\
\n\
    highlight		highlight when the mouse moves over it\n\
    write-protected	text within this extent will be unmodifyable\n\
    invisible		don't display the text in this region\n\
    unhighlight		turn off `highlight'\n\
    writable		turn off `write-protected'\n\
    visible		turn off `invisible'")
  (extent_obj, attr)
   Lisp_Object extent_obj, attr;
{
  EXTENT extent = XEXTENT (extent_obj);

  CHECK_EXTENT (extent_obj, 0);
  
  if (XTYPE(extent->buffer) != Lisp_Buffer
      || NILP (XBUFFER (extent->buffer)->name))
    error ("EXTENT arg to SET-EXTENT-ATTRIBUTE doesn't belong to a buffer");

  if (XTYPE (attr) == Lisp_Int)
    {
      int attr_num = XINT (attr);
      if (attr_num < 0)
        attr_num = -1;
      else if (attr_num >= GA_MAX) 
        attr_num = GA_NO_CHANGE;
      extent->attr_index = attr_num;
    }
  else if (XTYPE (attr) == Lisp_Symbol)
    {
      if (EQ (intern ("highlight"), attr))
        { SET_EXTENT_FLAG (extent, EF_HIGHLIGHT); }
      else if (EQ (intern ("unhighlight"), attr))
        { CLEAR_EXTENT_FLAG (extent, EF_HIGHLIGHT); }
      else if (EQ (intern ("write-protected"), attr))
        { SET_EXTENT_FLAG (extent, EF_WRITE_PROTECT); }
      else if (EQ (intern ("writable"), attr))
        { CLEAR_EXTENT_FLAG (extent, EF_WRITE_PROTECT); }
      else if (EQ (intern ("invisible"), attr))
        { SET_EXTENT_FLAG (extent, EF_INVISIBLE); }
      else if (EQ (intern ("visible"), attr))
        { CLEAR_EXTENT_FLAG (extent, EF_INVISIBLE); }
      else if (EQ (intern ("duplicable"), attr))
        { SET_EXTENT_FLAG (extent, EF_DUPLICABLE); }
      else if (EQ (intern ("non-duplicable"), attr))
        { CLEAR_EXTENT_FLAG (extent, EF_DUPLICABLE); }
      else
        error ("Unknown attribute argument, %s, to SET-EXTENT-ATTRIBUTE.", 
               SYMNAME (attr));
    }
  else
    error ("Unknown type of attribute argument to SET-EXTENT-ATTRIBUTE.");

  BUF_FACECHANGE (XBUFFER (extent->buffer))++;
  windows_or_buffers_changed++;

  return extent_obj;
}


DEFUN ("extent-at", Fextent_at, Sextent_at, 1, 3, 0,
       "Find \"smallest\" extent at POS in BUFFER having FLAG set.  BUFFER\n\
defaults to the current buffer, FLAG defaults to nil, meaning that any\n\
extent will do. Possible values for FLAG are nil, 'menu, 'highlight,\n\
'invisible, and 'write-protected. Returns nil if there is no matching\n\
extent at POS.")
  (pos, buffer, flag)
   Lisp_Object pos, buffer, flag;
{
  int position = XINT (pos);
  int flag_to_check = 0;
  Lisp_Object extent_obj = Qnil;
  EXTENT extent;

  if (NILP (buffer))
    XSET (buffer, Lisp_Buffer, current_buffer);

  CHECK_NUMBER (pos, 0);
  CHECK_BUFFER (buffer, 1);
  check_from_to (position, position, XBUFFER (buffer));

  if (!NILP (flag))
    {
      CHECK_SYMBOL (flag, 2);

      if (EQ (intern ("menu"), flag))
        flag_to_check = EF_MENU;
      else if (EQ (intern ("highlight"), flag))
        flag_to_check = EF_HIGHLIGHT;
      else if (EQ (intern ("write-protected"), flag))
        flag_to_check = EF_WRITE_PROTECT;
      else if (EQ (intern ("start-glyph"), flag))
        flag_to_check = EF_START_GLYPH;
      else if (EQ (intern ("end-glyph"), flag))
        flag_to_check = EF_END_GLYPH;
      else if (EQ (intern ("invisible"), flag))
        flag_to_check = EF_INVISIBLE;
      else if (EQ (intern ("duplicable"), flag))
        flag_to_check = EF_DUPLICABLE;
      else
        error ("%s is unknown flag argument for extent-at", SYMNAME (flag));
    }

  if (extent = extent_at (position, XBUFFER (buffer), flag_to_check))
    XSET (extent_obj, Lisp_Extent, extent);

  return extent_obj;
}

DEFUN ("next-extent", Fnext_extent, Snext_extent, 1, 1, 0,
       "Find next extent after EXTENT. If EXTENT is a buffer\n\
return the first extent in the buffer.")
  (extent_obj)
   Lisp_Object extent_obj;
{
  if (XTYPE (extent_obj) == Lisp_Buffer)
    return XBUFFER (extent_obj)->extents;
  else if (XTYPE (extent_obj) == Lisp_Extent)
    {
      Lisp_Object return_val = Qnil;
      EXTENT next = XEXTENT(extent_obj)->next;

      if (next)
        XSET (return_val, Lisp_Extent, next);
      return return_val;
    }
  else
    return Qnil;
}

#if 0

/* There are 2 total orders on extents in a buffer -- the normal
   (or "display") order, and the "e-order". This returns the
   "next extent" in the e-ordering.  This might be a temporary function
   or it might be permanent. */

DEFUN ("next-e-extent", Fnext_e_extent, Snext_e_extent, 1, 1, 0,
       "Find next extent after EXTENT using the \"e\" order. If \n\
EXTENT is a buffer, return the first extent in the buffer.")
  (extent_obj)
   Lisp_Object extent_obj;
{
  if (XTYPE (extent_obj) == Lisp_Buffer)
    {
      Lisp_Object return_val;
      EXTENT tmp;

      if (XTYPE (XBUFFER (extent_obj)->extents) == Lisp_Extent)
        tmp = XEXTENT (XBUFFER (extent_obj)->extents);
      else
        return Qnil;

      while (tmp->e_previous)
        tmp = tmp->e_previous;

      XSET (return_val, Lisp_Extent, tmp);
      return return_val;
    }
  else if (XTYPE (extent_obj) == Lisp_Extent)
    {
      Lisp_Object return_val = Qnil;
      EXTENT next = XEXTENT(extent_obj)->e_next;

      if (next)
        XSET (return_val, Lisp_Extent, next);
      return return_val;
    }
  else
    return Qnil;
}

#endif

/* Purportedly temporary debugging function -- turns a stack of
   extents into something that can be looked at in elisp. For
   a detailed explanation of a stack of extents, see below. */

static Lisp_Object
soe_to_lisp (struct stack_of_extents *soe, struct buffer *buf)
{
  if (!soe || 
      (soe->buf_index <= 0))
    return Qnil;
  else
    {
      Lisp_Object struct_vec = Fmake_vector (make_number (6), 
                                             make_number (0));
      Lisp_Object *struct_v = &(XVECTOR (struct_vec)->contents[0]);
      Lisp_Object stack_vec = Fmake_vector (make_number (soe->stack_index),
                                            make_number (0));
      Lisp_Object *stack_v = &(XVECTOR (stack_vec)->contents[0]);

      struct_v[0] = intern (":PRIOR-EXTENT");
      if (soe->previous_extent)
        XSET(struct_v[1], Lisp_Extent, soe->previous_extent);
      else
        struct_v[1] = Qnil;
    
      struct_v[2] = intern (":BUFFER-POS");
      XSET(struct_v[3], Lisp_Int, 
           extent_index_to_buffer_pos (soe->buf_index, buf));
    
      struct_v[4] = intern (":STACK");
      struct_v[5] = stack_vec;

      {
        int i;
      
        for (i = 0; i < soe->stack_index; i++)
          {
            EXTENT tmp = soe->stack[i];

            if (tmp)
              {
                XSET(stack_v[i], Lisp_Extent, tmp);
              }
            else 
              stack_v[i] = Qnil;
          }
      }

      return struct_vec;
    }
}

#if 0

/* Elisp interface for debugging stacks of extents */
DEFUN ("stack-of-extents", Fstack_of_extents, 
       Sstack_of_extents, 1, 2, 0,
       "Return stack of extents for BUFFER. Optional arg POSITION supplied\n\
means compute the correct stack of extents for POSITION in BUFFER.")
  (buffer, position)
   Lisp_Object buffer, position;
{
  Lisp_Object return_value = Qnil;
  struct stack_of_extents *soe;
  struct buffer *buf = XBUFFER (buffer);
  CHECK_BUFFER (buffer, 0);
  if (!NILP (position))
    {
      struct stack_of_extents *tmp = buf->cached_stack;
      int pos;
      CHECK_NUMBER (position, 1);
      pos = XINT (position);
      if ((pos < BUF_BEG(buf)) || (BUF_Z (buf) < pos))
        error ("Bad position %d for buffer %s", pos, BUFNAME (buf));
      
      buf->cached_stack = 0;
      init_buffer_cached_stack (buf);
      befa_internal (pos, buf);
      soe = buf->cached_stack;
      buf->cached_stack = tmp;
    }
  else
    soe = buf->cached_stack;
      
  if (!soe)
    return Qnil;
  else
    return_value = soe_to_lisp (soe, buf);
  
  if (soe && (soe != buf->cached_stack))
    {
      EXTENT *tmp_stack = soe->stack;
      soe->stack = 0;
      if (tmp_stack)
        free (tmp_stack);
      free (soe);
    }

  return return_value;
}

#endif

/* Debugging and test function -- maybe permanent. */
/* Return 0 if stack is correct, 1 if stack has been cleared (which
   is not incorrect but isn't good news), and -1 if it is provably
   incorrect. */
int verify_buffer_stack (struct buffer *buf)
{
  struct stack_of_extents *soe;

  if (!buf)
    buf = current_buffer;

  soe = buf->cached_stack;

  if (!soe)
    {
      if (XTYPE (buf->extents) == Lisp_Extent)
        return -1;
      else if (buf->extents == Qnil)
        return 0;
      else
        abort ();
    }
  else if (soe->buf_index <= 0)
    return 1;
  else
    {
      int pos = extent_index_to_buffer_pos (soe->buf_index, buf);
      int return_value;
      buf->cached_stack = 0;
      init_buffer_cached_stack (buf);
      befa_internal (pos, buf);
      
      return_value = bcmp (soe, buf->cached_stack, sizeof (*soe));
      if (!return_value)
        return_value = 
          bcmp (soe->stack, buf->cached_stack->stack, 
                soe->stack_index * sizeof (EXTENT));
      if (return_value)
        return_value = -1;
      free_buffer_cached_stack (buf);
      buf->cached_stack = soe;
      return return_value;
    }
}


/* Long comment: 

   A "Stack Of Extents" (abbreviated SOE) is a data structure that is
   an abbreviated version of an extent_fragment.  However, unlike the
   extent_fragment, of which there are really only 2, which are used
   by redisplay, and which are supposed to be FAST to manage, every
   buffer with extents has an SOE, these are supposed to be robust,
   and they don't have to be that fast to manage. What an SOE does is
   cache the information needed to try to find, given a buffer index
   I, some extent E which is "before" I in the display order (meaning
   that E doesn't include I, and any extent E' that does include I is
   "greater than" E in the display order). The purpose of this is to
   make functions like Fextent_at() and buffer_extent_fragment_at() be
   as fast at the bottom of a buffer as they are at the top, no matter
   how many extents the buffer has. (We may want to consider having
   detach_extent() and splice_extent_into_buffer() update the SOE,
   too, so that updating extents will be faster and adding extents "in
   order" will be as fast as adding them in "reverse order" is.)

   The two orderings of extents in a buffer, normal (or display) order
   and e-order, are defined by macros at the top of the file.
   Basically, though, normal order sorts by increasing start index and
   e-order by increasing end index.

   The components of an SOE are the buffer index, the vector of
   extents (in display order) that lie over that index, and the
   "previous extent", which is the last extent in the e-order whose
   end index is <= the given buffer index and which is NOT over the
   buffer index (if the extent has 0 length, it will be over the index
   if the start and end values equal the index).

   An SOE is "cleared" or invalidated iff the buffer index value is <=
   0, since this is never a legal buffer index -- this clearing is
   done by soe_clear(), and is called any time the SOE can't get
   updated properly. 

   Each time an extent is spliced into the buffer, an attempt is made
   to "push" it onto the SOE (which means that we examine the new
   extent to see if it goes over that buffer index or if it should
   replace the previous extent).  When an extent is detached, we
   similarly attempt to delq it from the SOE.

   Any call to the function befa_internal() changes the SOE to use the
   new buffer index via soe_duplicate(). The function
   process_extents_for_deletion() may change the endpoints of extents,
   of adjust the buffer index, so it calls soe_prune(). This function
   removes any extents that are not over the buffer index, and
   corrects the previous extent value.

   There are also init and free functions for the SOE, called by
   splice_extent_into_buffer() and Fkill_buffer, respectively.



   Finally, what's the point of all of this? The point is that given
   an index I, the set S of extents over I, and the previous extent P,
   it is possible to compute an extent before an index I' (before in
   the display order). If I' >= I, this is just P (which can be easily
   recomputed from S[0] if S exists and P does not -- see
   soe_prune()).  If I' < I, then consider the interval [E, P] of
   extents in the e-order such that E is the first extent with end
   index less than I'. Then the minimum (in the normal order) E' of
   these extents is either strictly before I' or is the "top of the
   stack at" I', and in the latter case, E'->previous is before I'.

   The proof is left as an exercise for the readers.
 */

static void soe_push (EXTENT extent, struct buffer *b)
{
  struct stack_of_extents *soe = b->cached_stack;
  int index = soe->buf_index;
  int on_stack = 0;

  if (index <= 0)
    {
      soe_clear(b);
      return;
    }
  else if (EXTENT_OVER_INDEX (extent, index))
    on_stack = 1;

  if (on_stack)
    {
      if (soe->stack_index == soe->stack_length)
	{
	  soe->stack_length *= 2;
	  soe->stack = 
            (EXTENT*)xrealloc (soe->stack, 
                               soe->stack_length * sizeof (EXTENT));
	}

      if (soe->stack_index == 0)
	{
	  soe->stack[0] = extent;
	  soe->stack_index = 1;
	}
      else
	{
	  EXTENT *cef, *cef_bound, *cef_copy, *cef_put_here;

	  /* make sure that this guy isn't in the stack already */
	  soe_delq (extent, b);
      
	  cef = soe->stack;
	  cef_copy = cef;
	  cef_bound = cef + soe->stack_index;
	  cef_put_here = cef_bound;

	  while (cef < cef_bound)
	    {
	      if (EXTENT_LESS (extent, *cef))
		{
		  cef_put_here = cef;
		  break;
		}
	      cef++;
	    }
  
	  cef = cef_bound;
	  while (cef > cef_put_here)
	    {
	      *cef = *(cef - 1);
	      cef--;
	    }
	  *cef_put_here = extent;
	  soe->stack_index++;
	}
    }
  else if (extent->end < index)
    {
      EXTENT prev = soe->previous_extent;
      if (!prev || EXTENT_E_LESS (prev, extent))
	soe->previous_extent = extent;
    }
}

static void soe_duplicate (int index, EXTENT *copy_from, int copy_from_size, 
			   EXTENT trial_prev, struct buffer *b)
{
  struct stack_of_extents *soe = b->cached_stack;

  if (copy_from_size > 0)
    {
      if (soe->stack_length < copy_from_size)
	{
	  soe->stack_length = copy_from_size + 1;
	  soe->stack = 
            (EXTENT*)xrealloc (soe->stack, 
                               soe->stack_length * sizeof (EXTENT));
	}
      bcopy (copy_from, soe->stack, copy_from_size * sizeof (EXTENT));

      trial_prev = soe->stack[0];
    }
  
  soe->stack_index = copy_from_size;
  soe->buf_index = index;

  if (!trial_prev || !EXTENT_PAST_INDEX (trial_prev, index))
    trial_prev = 0;

  while (trial_prev && EXTENT_PAST_INDEX (trial_prev, index))
    trial_prev = trial_prev->e_previous;

  if (!trial_prev)
    soe_clear(b);
  else
    soe->previous_extent = trial_prev;
}


static void soe_delq (EXTENT extent, struct buffer *b)
{
  struct stack_of_extents *soe = b->cached_stack;
  EXTENT* cef = soe->stack;
  EXTENT* cef_copy = cef;
  EXTENT* cef_bound = cef + soe->stack_index;
  int found = 0;

  while (cef < cef_bound)
    {
      if (extent == *cef)
	{
	  *cef = 0;
	  found++;
	}
      cef++;
    }
  
  if (found > 0)
    {
      cef = cef_copy; 
      while (cef < cef_bound)
	{
	  if (*cef)
	    {
	      *cef_copy = *cef;
	      cef_copy++;
	    }
	  cef++;
	}

      soe->stack_index = cef_copy - soe->stack;
    }

  if (extent == soe->previous_extent)
    soe->previous_extent = extent->e_previous;
}

static void init_buffer_cached_stack (struct buffer *b)
{
  if (!b->cached_stack)
    {
      int default_stack_size = 10;
      struct stack_of_extents *new =
        (struct stack_of_extents *) 
          xmalloc (sizeof (struct stack_of_extents));
      bzero ((char *) new, sizeof (struct stack_of_extents));
      new->stack_length = default_stack_size;
      new->stack = (EXTENT *) xmalloc (default_stack_size * sizeof (EXTENT));
      b->cached_stack = new;
    }
}

static void soe_clear (struct buffer *b)
{
  struct stack_of_extents *soe = b->cached_stack;
  soe->stack_index = 0;
  soe->buf_index = 0;
  soe->previous_extent = 0;
}

static void soe_prune (struct buffer *b)
{
  struct stack_of_extents *soe = b->cached_stack;

  if (soe->buf_index <= 0)
    {
      soe_clear(b);
      return;
    }
  else
    {
      EXTENT* cef = soe->stack;
      EXTENT* cef_copy = cef;
      EXTENT* cef_bound = cef + soe->stack_index;
      int index = soe->buf_index;
      int removedp = 0;

      while (cef < cef_bound)
	{
	  EXTENT extent = *cef;
	  if (!EXTENT_OVER_INDEX (extent, index))
	    {
	      *cef = 0;
	      removedp = 1;
	    }
	  cef++;
	}
  
      if (removedp)
	{
	  cef = cef_copy; 
	  while (cef < cef_bound)
	    {
	      if (*cef)
		{
		  *cef_copy = *cef;
		  cef_copy++;
		}
	      cef++;
	    }

	  soe->stack_index = cef_copy - soe->stack;
	}
    }

  {
    EXTENT prev;
    int index = soe->buf_index;

    if (soe->stack_index > 0)
      prev = soe->stack[0];
    else
      prev = soe->previous_extent;

    while (prev && EXTENT_PAST_INDEX (prev, index))
      prev = prev->e_previous;
    if (prev)
      soe->previous_extent = prev;
    else
      soe_clear (b);
  }
}

void free_buffer_cached_stack (struct buffer *b)
{
  struct stack_of_extents *tmp = b->cached_stack;
  b->cached_stack = 0;
  if (tmp)
    {
      EXTENT *tmp_stack = tmp->stack;
      tmp->stack = 0;
      if (tmp_stack)
        free (tmp_stack);
      free (tmp);
    }
}

/* This function assumes that ef is correct and up to date, and then
   ef->start_index is incremented and ef->first_extent_past_stack is either
   0'd or certified correct, and then this function is called. This
   function removes all extents from the extents_stack that are "too small"
   (the only way the can be invalid here, since if they were "too big" now,
   then they would have been "too big" when ef->start_index was smaller).
   In particular, ef->extents_stack will be correct unless there are 
   "bigger extents" than those in the stack that remain to be pushed on it.
   
   At the same time, it sets ef->end_index to be the minimum of all of the
   end indicies of the extents left in the stack, if any, and the
   start_index of ef->first_extent_past_stack, if that exists. If none of
   these exist, the ef->end_index is set to be the max index of the buffer.
   This value of ef->end_index is at least as large as the correct value
   for ef->buffer and given ef->start_index, and it is correct if the
   extents_stack is complete and the value of ef->first_extent_past_stack
   passed in is correct. */
   
static void
cleanup_old_stack (EXTENT_FRAGMENT ef)
{
  EXTENT* cef = ef->extents_stack;
  EXTENT* cef_copy = cef;
  EXTENT* cef_bound = cef + ef->number_of_extents;
  int new_start = ef->start_index;
  int min_for_end = 
    (ef->first_extent_past_stack)?
      (ef->first_extent_past_stack->start):MAX_INT;

  while (cef < cef_bound)
    {
      int end = (*cef)->end;
      int start = (*cef)->start;
   
      /* flush any extent ending too soon, which includes any extent ending
         before this point or any extent of positive length ending AT this
         point */
      if ((end < new_start) ||
          ((end == new_start) && (start < end)))
	*cef = 0;
      /* if the extent wasn't flushed and is of positive length,
         then if it ends before our min so far, use this as the
         new min */
      else if ((end < min_for_end) && (start < end))
	min_for_end = end;
      cef++;
    }
  
    cef = cef_copy; 
    while (cef < cef_bound)
    {
      if (*cef)
	{
          *cef_copy = *cef;
          cef_copy++;
	}
      cef++;
    }

  ef->end_index = min_for_end;
  ef->number_of_extents = cef_copy - ef->extents_stack;
}

/* Long comment:
   
   This function updates the cached ef to "surround" the new buf_index 
   that is promised to be == the current ef->end_index. The on

   Extents are ordered with increasing start position and then decreasing
   end position. (This is what might be called "display order" -- if extent
   A occurs after extent B, then the display attributes of extent A
   override those of extent B in the region covered by both A and B. Note
   that multiple extents with the same start and end postions may be in any
   order.)

   The extents_stack has the extents that are "over" a given buffer
   index in the same order as in the buffer's extent list. The state
   coming in might look like this:


   extents_stack |
                 v

   [.......................................)
   [............................)
       [...........................)
       [.....................)
            [...........................)
                             |  <<-------  buf_index required to be this
            [________________)  <<-------  fragment boundaries

                                                    first_extent_past_stack |
                                                                            v
                                                    [........................)

                             [__)  new fragment boundaries
   
   If there is no first_extent_past_stack, then we just prune the 
   extents_stack as described below, and if there are no extents over
   the buf_index, the new fragment goes from buf_index to the end of
   the buffer. 
   
   Otherwise, the first_extent_past_stack is guaranteed to have its
   start index be >= buf_index of the fragment.

   If the first_extent_past_stack start is > buf_index, then the
   first_extent_past_stack will still be the same after the update.  In
   that case all that is needed is to update the extents_stack by deleting
   anything that "fell off the end", set the new start_index to the
   buf_index and compute the new end_index. (In the special case where
   there is one extent in the extents_stack, this calculation is easy,
   since that means that the new stack is empty and the new end_index for
   the fragment is next->start. If the extents_stack is empty, then
   the caller of this function is broken and we abort().)

   The remaining case is when the first_extent_past_stack starts at
   buf_index. If there are extents in the stack, then we need to remove all
   of the ones that have fallen off the end.  After that
   first_extent_past_stack gets pushed, and we look for any other extents
   that need pushing. While we are doing this, we need to figure out where
   the new fragment ends, and what the new first_extent_past_stack is. (We
   could consider optimizing the case of their being one extent in the
   stack


   NOTE: At the moment only this function, buffer_extent_fragment_at(),
   splice_extent_into_buffer(), adjust_extents () and map_extents() 
   know how to "cdr" down a list of extents. */

static EXTENT_FRAGMENT
update_cache_forward (EXTENT_FRAGMENT ef, int buf_index, struct buffer* buf)
{
  /* Incrementally update the cache forward.  This has to be FAST. */
  EXTENT next = ef->first_extent_past_stack;

  ef->start_index = buf_index;

  if (!next)
    {
      /* ef->first_extent_past_stack is correct here, because there
         isn't any */
      if (ef->number_of_extents == 1)
        {
          /* easy case for updating stack */
          ef->number_of_extents = 0;   
	  ef->end_index = MAX_INT;
        }
      else if (ef->number_of_extents == 0)
        abort();
      else
        /* this will fix ef->end_index, since ef->first_extent_past_stack
           is correctly 0 and there are no more extents to add to the stack */
        cleanup_old_stack (ef);
   
      /* the stack, ef->start_index, and ef->end_index are all
         correct, so just wrap_up */   
      goto wrap_up;
    }
  else if (buf_index < next->start)
    {
      /* ef->first_extent_past_stack is correct here, because it  won't
         change as the result of this operation */
      if (ef->number_of_extents == 1)
        {
          /* easy case for updating stack -- moving into a "hole" between
             extents */
          ef->number_of_extents = 0;
          ef->end_index = next->start;
        }
      else if (ef->number_of_extents == 0)
        abort();
      else
        /* the stack will add nothing new, because
           ef->first_extent_past_stack is too big to go on the stack, and
           so it is correct, too -- this means that call will set
           ef->end_index to be == correct value; see comment for
           cleanup_old_stack() */
        cleanup_old_stack (ef);

      /* the stack, ef->start_index, and ef->end_index are all
         correct, so just wrap_up */   
      goto wrap_up;   
    }
  else if (ef->number_of_extents > 0)
    {
      /* ef->first_extent_past_stack wrong here, because it is next,
         and next is going onto the stack -- the stack is wrong, too */
      ef->first_extent_past_stack = 0;
      /* this will set ef->end_index to be >= correct value; see
         comment for function */
      cleanup_old_stack (ef);
    }
  else 
    /* stack is empty so needs no pruning, and we know that next->end
       is at least as big as the fragment end_index */
    ef->end_index = next->end;

  /* If we get to this point, ef->start_index is correct,
     ef->end_index is >= the correct value, and
     ef->first_extent_past_stack is completely wrong */

  {
    /* retain last extent to be pushed on the stack */
    EXTENT last;
    do
      {
        if (ef->number_of_extents == ef->extents_stack_length)
          {
            ef->extents_stack_length *= 2;
            ef->extents_stack =
              (EXTENT*)xrealloc (ef->extents_stack,
                                 ef->extents_stack_length * sizeof (EXTENT));
          }
        ef->extents_stack [ef->number_of_extents] = next;
        ef->number_of_extents += 1;

        last = next;
        next = next->next;
      }
    while (next && (next->start == buf_index));

    /* make sure that ef->end_index is correct, since we may have
       come into this function with a value that is too big -- 
       recall that since the end values of the extents are
       decreasing while the start value stay the same, last->end
       has the smallest "end" of all things pushed onto the stack */
    if (last->end < ef->end_index)
      ef->end_index = last->end;
    if (next &&
        (next->start < ef->end_index))
      ef->end_index = next->start;
  }
  ef->first_extent_past_stack = next;

 wrap_up:
  ef->from = extent_index_to_buffer_pos (ef->start_index, buf);
  ef->to = 
    ((ef->end_index == MAX_INT)?MAX_INT:
     extent_index_to_buffer_pos (ef->end_index, buf));
  return ef;
}


/* Find extent fragment at pos in buf. NOTE: At the moment only this
   function, update_cache_forward(), splice_extent_into_buffer(), 
   adjust_extents () and map_extents() 
   know how to "cdr" down a list of extents. See the comment
   at map_extents() for information about the ordering rule. */

static EXTENT_FRAGMENT
befa_internal (int pos, struct buffer *buf)
{
  int cache_valid;
  EXTENT_FRAGMENT ef;
  EXTENT current;
  EXTENT trial_prev = 0;
  int buf_index;
  int new_start;
  int new_end;

  buf_index = buffer_pos_to_extent_index (pos, buf);
  ef = &extent_fragment;

  current = buffer_starting_extent (buf_index, buf);
  
  new_start = buffer_pos_to_extent_index (BUF_BEG(buf), buf);
  new_end = MAX_INT;
  ef->number_of_extents = 0;

  /* Find all of the extents "over" this fragment, and at the same time
     find the "first_extent_past_stack" for the fragment and the 
     start_index. */
  while (current && 
         (current->start <= buf_index))
    {
      trial_prev = current;

      if ((current->end <= buf_index) && (current->end > new_start))
        {
          new_start = current->end;
        }
      /* we repeat some code in this clause and the next to save tests */
      else if (current->end > buf_index)
	{
          if (current->start > new_start)
            new_start = current->start;   
          if (current->end < new_end)
            new_end = current->end;

	  if (ef->number_of_extents == ef->extents_stack_length)
	    {
	      ef->extents_stack_length *= 2;
	      ef->extents_stack =
		(EXTENT*)xrealloc (ef->extents_stack,
				   ef->extents_stack_length * sizeof (EXTENT));
	    }
	  ef->extents_stack [ef->number_of_extents] = current;
	  ef->number_of_extents += 1;
	}
      /* we repeat some code in this clause and the last to save tests */   
      else if ((current->end == current->start) && (current->end == buf_index))
	{
          new_end = new_start = buf_index;

	  if (ef->number_of_extents == ef->extents_stack_length)
	    {
	      ef->extents_stack_length *= 2;
	      ef->extents_stack =
		(EXTENT*)xrealloc (ef->extents_stack,
				   ef->extents_stack_length * sizeof (EXTENT));
	    }
	  ef->extents_stack [ef->number_of_extents] = current;
	  ef->number_of_extents += 1;
	}
        
      current = current->next;
    }

  /* Check the end_index for this fragment. */
  if (current && 
      (current->start < new_end))
    new_end = current->start;
  
  XSET (Vextent_fragment_buffer, Lisp_Buffer, buf);
  ef->buf = buf;
  ef->modiff = BUF_MODIFF (buf);
  ef->face_change = BUF_FACECHANGE (buf);
  ef->first_extent_past_stack = current;
  ef->start_index = new_start;
  ef->end_index = new_end;
  ef->from = extent_index_to_buffer_pos (new_start, buf);
  ef->to = 
    (new_end == MAX_INT)?MAX_INT:extent_index_to_buffer_pos (new_end, buf);

  soe_duplicate (buf_index, ef->extents_stack, 
                 ef->number_of_extents, trial_prev, buf);

  return ef;
}

EXTENT_FRAGMENT
buffer_extent_fragment_at (int pos, struct buffer *buf, struct screen *s)
{
  int cache_valid;
  EXTENT_FRAGMENT ef;
  EXTENT current;
  EXTENT next;
  int buf_index;
  int new_start;
  int new_end;

  if (NILP (buf->extents))
    {
      Vextent_fragment_buffer = Qnil;
      extent_fragment.buf = 0;
      default_extent_fragment.from = BUF_BEG (buf);
      default_extent_fragment.to = MAX_INT;
      return &default_extent_fragment;
    }
  else if (XTYPE (buf->extents) != Lisp_Extent)
    abort ();

  buf_index = buffer_pos_to_extent_index (pos, buf);
  ef = &extent_fragment;
  cache_valid = ((buf == ef->buf) && 
                 (BUF_MODIFF(buf) == ef->modiff) &&
		 (BUF_FACECHANGE (buf) == ef->face_change));

  if (cache_valid)
    {
      if (buf_index == ef->end_index)
	ef = update_cache_forward (ef, buf_index, buf); /* 99% of the time */
      else if ((pos >= extent_fragment.from) && (pos < extent_fragment.to))
	return ef;
      else 
	ef = befa_internal (pos, buf);
    }
  else 
    ef = befa_internal (pos, buf);

  setup_extent_fragment_face_ptr (s, ef);
  return ef;
}

static void
init_extent_fragment ()
{
  int l = 30;

  bzero ((char *) &extent_fragment, sizeof (extent_fragment));
  extent_fragment.extents_stack_length = l;
  extent_fragment.extents_stack = (EXTENT *) xmalloc (l * sizeof (EXTENT));

  bzero ((char *) &default_extent_fragment, sizeof (default_extent_fragment));
}

/* Modify all of the extents as required for the insertion. At the
   moment this function does nothing, but eventually it probably should
   adjust the endpoints of the extents that touch point in a manner that
   takes the the opened/closed property of the endpoint into account. */
void process_extents_for_insertion (int opoint, int length, struct buffer *buf)
{
  return;
}
   
static int   
process_extents_for_deletion_mf (EXTENT extent, void *arg)
{
  struct process_extents_for_deletion_struct *peds_ptr = 
    (struct process_extents_for_deletion_struct *) arg;
  
  if ((peds_ptr->start <= extent->start) && (extent->end <= peds_ptr->end))
    {
      if (peds_ptr->destroy_included_extents)
        destroy_extent (extent);
      else
        detach_extent (extent);
    }
  else if (peds_ptr->destroy_included_extents)
    return 0;
  /* the extent completely contains the deleted range, so we don't need to
     do anything about it */
  else if ((extent->start < peds_ptr->start) && (peds_ptr->end < extent->end))
    return 0;
  else
    /* these characters are going away, so the extent must be shortened 
       appropriately -- this code should probably do something about
       opened/closed endpoints, too */
    {
      int max_start = max (extent->start, peds_ptr->start);
      int min_end = min (extent->end, peds_ptr->end);
      /* this test is really unneeded, since map_extents() promises the
         two "spans of text" will overlap but it's cheap and
         I'm nervous */
      if (max_start < min_end)
        {
          if (max_start == extent->start)
            extent->start = min_end;
          else
            extent->end = max_start;
        }
    }
  return 0;
}

/* Delete all of the extents that are completely inside the range [from,
   to). Eventually, this function should also adjust the endpoints of the
   extents that overlap the about to be deleted range in a manner that
   takes the the opened/closed property of the endpoint into account.
   NOTE: This function must either preserve the internal ordering of the
   extents automatically or it must explicitly fix that ordering before
   quitting. At the moment the ordering is preserved automatically.
   [from to) is the range of POSITIONs being deleted and [start end) is the
   INDEX values of the gap when the deletion is completed.
   */
void process_extents_for_deletion (int from, int to, int start, int end,
				   struct buffer *buf)
{
  if (NILP (buf->extents))
    return;
  else if (XTYPE (buf->extents) != Lisp_Extent)
    abort();
  else
    {
      struct process_extents_for_deletion_struct peds;   

      check_from_to (from, to, buf);

      /* start and end don't need to be turned into index values because
         they are already -- from and to are buffer positions */
      peds.start = start;
      peds.end = end;
      peds.destroy_included_extents = 0;
      /* Need to do a map_extent with closed_end so that the extent
	 just beginning or ending on the old gap are processed too.
	 --Matthieu. */
      map_extents (from, to, 0, process_extents_for_deletion_mf,
                   (void *) &peds, buf, 1);

      if ((buf->cached_stack->buf_index >= start) && 
	  (buf->cached_stack->buf_index < end))
	buf->cached_stack->buf_index = start;
      soe_prune (buf);
    }
}

void process_extents_for_destruction (int from, int to, struct buffer *buf)
{
  if (NILP (buf->extents))
    return;
  else if (XTYPE (buf->extents) != Lisp_Extent)
    abort();
  else
    {
      struct process_extents_for_deletion_struct peds;   

      check_from_to (from, to, buf);

      peds.start = buffer_pos_to_extent_index (from, buf);
      peds.end = buffer_pos_to_extent_index (to, buf);
      peds.destroy_included_extents = 1;
      /* Need to do a map_extent with closed_end so that the extent
         just beginning or ending on the old gap are processed too.
         --Matthieu. */
      map_extents (from, to, 0, process_extents_for_deletion_mf,
                   (void *) &peds, buf, 1);
    }
}


static int
replicate_extents_mf (EXTENT extent, void *arg)
{
  struct replicate_extents_struct *res_ptr = 
    (struct replicate_extents_struct *) arg;
  Lisp_Object head = res_ptr->head;
  Lisp_Object tail = res_ptr->nconc_cell;
  int start = 
    extent_index_to_buffer_pos (extent->start, res_ptr->buf) - res_ptr->from;
  int end = 
    extent_index_to_buffer_pos (extent->end, res_ptr->buf) - res_ptr->from;
  
  if (EXTENT_FLAG_P (extent, EF_DUPLICABLE))
    {
      start = max (start, 0);
      end = min (end, res_ptr->length);

      /* this test should probably never fail, but I'm a bit confused at the
	 moment */
      if ((start < end) || 
	  ((start == end) && (extent->start == extent->end)))
	{
	  Lisp_Object new_cell;   
	  Lisp_Object replica;
	  DUP dup = make_extent_replica ();

	  XSET (replica, Lisp_Extent_Replica, dup);
	  XSET (dup->extent, Lisp_Extent, extent);
	  dup->start = start;
	  dup->end = end;
	  new_cell = Fcons (replica, Qnil);
   
	  if (NILP (head))
	    res_ptr->head = new_cell;
	  else
	    Fsetcdr (tail, new_cell);
	  res_ptr->nconc_cell = new_cell;
	}
    }  
  return 0;
}

Lisp_Object 
replicate_extents (int opoint, int length, struct buffer *buf)
{
  if (NILP (buf->extents))
    return Qnil;
  else if (XTYPE (buf->extents) != Lisp_Extent)
    abort();
  else
    {
      struct replicate_extents_struct res;
      res.from = opoint;
      res.length = length;
      res.head = Qnil;
      res.buf = buf;
      res.nconc_cell = 0;
      map_extents (opoint, opoint + length, 0, replicate_extents_mf, 
                   (void *) &res, buf, 0);
      return res.head;
    }
}

/* We have just inserted a string of size "length" at "opoint" -- we have
   the contents of the extents slot of the string on hand, and we now need
   to do "whatever" is necessary to make the extents in the buffer be
   correctly updated. If there are no extents on the string, then that is
   nothing. If there are extents and we are inside_undo, then the extents
   argument is taken as revealed truth and the state of the buffer extents
   must be restored so that the function above would return the same string
   extents if this corresponding string were to be deleted. If we are not
   inside undo then we just splice in those extents that correspond to
   deleted extents.

   Note: At the moment we ONLY handle the case of the dup_list argument
   be a list of extent_replicas.
   */

void 
splice_in_extent_replicas (int opoint, int length, 
                           Lisp_Object dup_list, struct buffer *buf)
{
  if ((!NILP(buf->extents) && (XTYPE (buf->extents) != Lisp_Extent)) || 
      (!NILP (dup_list) && (XTYPE (dup_list) != Lisp_Cons)))
    abort();
  if (NILP (dup_list))   
    return;
  else if (inside_undo)
    {
      Lisp_Object tail;
      int base_start = buffer_pos_to_extent_index (opoint, buf);
      int base_end = buffer_pos_to_extent_index (opoint + length, buf);

      for (tail = dup_list; !NILP (tail); tail = Fcdr (tail))
        {
          Lisp_Object current_replica = Fcar (tail);
          /* only process replicas at the moment */
          if (XTYPE (current_replica) == Lisp_Extent_Replica) 
            {
              DUP dup = XDUP (current_replica);
              EXTENT extent = XEXTENT (dup->extent);
              int new_start = base_start + dup->start;
              int new_end = base_start + dup->end;

              if (EXTENT_DESTROYED_P (extent))
                continue;

              /* paranoid testing which will go away eventually */
              if ((XTYPE (dup->extent) != Lisp_Extent) ||
                  (XBUFFER (extent->buffer) != buf))
                abort ();
              
              if (EXTENT_FLAGS (extent) & EF_DETACHED)
                {
                  int from = extent_index_to_buffer_pos (new_start, buf);
                  int to = extent_index_to_buffer_pos (new_end, buf);
                  update_extent (extent, from, to, 1, buf);
                }
              else if
                ((extent->start > extent_index_offset (base_end, 1, buf)) ||
                 (extent->end < extent_index_offset (base_start, -1, buf)))
                error ("extent 0x%x is all fouled up wrt. dup 0x%x",
                       extent, dup);
              else
                {
                  /* this should be safe because if you delete some text
                     all of the extents that were effected stay in the
                     same order, so when you restore what was removed
                     they should still be in the correct order */
                  extent->start = min (new_start, extent->start);
                  extent->end = max (new_end, extent->end);
		  soe_push (extent, buf);
                }
            }
        }
    }
  else
    {
      Lisp_Object tail;
      int base_start = buffer_pos_to_extent_index (opoint, buf);
      int base_end = buffer_pos_to_extent_index (opoint + length, buf);

      for (tail = dup_list; !NILP (tail); tail = Fcdr (tail))
        {
          Lisp_Object current_replica = Fcar (tail);
          /* only process replicas at the moment */
          if (XTYPE (current_replica) == Lisp_Extent_Replica) 
            {
              DUP dup = XDUP (current_replica);
              EXTENT extent = XEXTENT (dup->extent);
              int new_start = base_start + dup->start;
              int new_end = base_start + dup->end;

              if (EXTENT_DESTROYED_P (extent))
                continue;

              /* paranoid testing which will go away eventually */
              if ((XTYPE (dup->extent) != Lisp_Extent))
                abort ();

	      /* Energize extents like topleve-forms can only be pasted 
	       * in the buffer they come from.  This should be parametrized
	       * in the generic extent objects.  Right now just silently
	       * skip the extents if it's not from the same buffer.
	       * --Matthieu */
	      if (XBUFFER (extent->buffer) != buf)
		continue;

              if (EXTENT_FLAGS (extent) & EF_DETACHED)
                {
                  int from = extent_index_to_buffer_pos (new_start, buf);
                  int to = extent_index_to_buffer_pos (new_end, buf);
                  update_extent (extent, from, to, 1, buf);
                }
              else if (extent->end < new_start - 1)
                continue;
              else if (extent->start > new_end + 1)
                continue;
              else
                {
                  int from = 
                    extent_index_to_buffer_pos 
                      (min (new_start, extent->start), buf);
                  int to = 
                    extent_index_to_buffer_pos 
                      (max (new_end, extent->end), buf);
                  update_extent (extent, from, to, 1, buf);   
                }
            }
        }
    }
}


/* Merge dup_list[i] into a list of replicas -- if a dup
   in listi "overlaps at the end" matches a dup from listi+1 that "overlaps
   at the beginning", merge them into one contiguous dup in the returned
   list. It is weird and probably bogus if a "detached dup" doesn't merge 
   entirely, but it isn't an error. */
   
Lisp_Object 
merge_replicas (int number_of_lists, struct merge_replicas_struct *vec)
{
  c_hashtable table = 0;
  Lisp_Object cells_vec[2];
  int i;

  cells_vec[0] = Qnil;
  cells_vec[1] = Qnil;

  for (i = 0; i < number_of_lists; i++)
    {
      Lisp_Object dup_list = vec[i].dup_list;
      int offset = vec[i].entry_offset;
      int length = vec[i].entry_length;

      if (!NILP (dup_list))
        {
          if (!table)
            table = make_hashtable (10);
          add_to_replicas_lists (table, dup_list, offset, length);
        }
    }

  if (table)
    {
      maphash (merge_replicas_pruning_mf, table, (void *) table);
      maphash (merge_replicas_concating_mf, table, (void *) &(cells_vec[0]));
      free_hashtable (table);
    }
  return (cells_vec[0]);
}

static void 
add_to_replicas_lists
(c_hashtable table, Lisp_Object dup_list, int offset, int length)
{
  Lisp_Object tail;
  for (tail = dup_list; !NILP (tail); tail = Fcdr(tail))
    {
      Lisp_Object current_replica = Fcar (tail);
      if (XTYPE (current_replica) == Lisp_Extent_Replica) 
        {
          DUP dup = XDUP (current_replica);
          EXTENT extent = XEXTENT (dup->extent);
          int new_start = offset + dup->start;
          int new_end = offset + dup->end;
          Lisp_Object pre_existing_cell;
          Lisp_Object tmp;
          DUP new_dup;

          if (EXTENT_DESTROYED_P (extent))
            continue;

          new_dup = make_extent_replica ();
          bcopy ((char *) dup, (char *) new_dup, sizeof (*dup));
          new_dup->start += offset;
          new_dup->end += offset;
   
          /* paranoid testing which will go away eventually */
          if (XTYPE (dup->extent) != Lisp_Extent)
            abort ();
              
          if (!gethash ((void *) extent, table, (void **) &pre_existing_cell))
            pre_existing_cell = Qnil;
   
          XSET (tmp, Lisp_Extent_Replica, new_dup);
          tmp = Fcons (tmp, pre_existing_cell);
          puthash ((void *)extent, (void *) tmp, table);
        }
    }

  
}

static void merge_replicas_concating_mf (void *key, void *contents, void *arg)
{
  extern Lisp_Object nconc2();
  Lisp_Object extent_cell = (Lisp_Object) contents;
  Lisp_Object *cells_vec = (Lisp_Object *) arg;

  if (NILP (cells_vec[0]))
    cells_vec[0] = extent_cell;
  else
    nconc2 (cells_vec[1], extent_cell);

  cells_vec[1] = extent_cell;
  return;
}   
   
static int mrp_pred (Lisp_Object x, Lisp_Object y, Lisp_Object dummy)
{
  DUP dup1 = XDUP(x);
  DUP dup2 = XDUP(y);

  if (dup1->start < dup2->start)
    return 1;
  else if (dup1->start == dup2->start)
    {
      if (dup1->end <= dup2->end)
        return 1;
      else
        return -1;
    }
  return -1;
}
   
static void merge_replicas_pruning_mf (void *key, void *contents, void *arg)
{
  int changed = 0;
  Lisp_Object dup_list = (Lisp_Object) contents;
  c_hashtable table = (c_hashtable) arg;

  if (NILP (dup_list))
    return;
   
  /* sort and merge the dup_list */
  dup_list = list_sort (dup_list, Qnil, mrp_pred);
  {
    Lisp_Object current = dup_list;
    Lisp_Object tail = Fcdr (dup_list);
    DUP current_dup = XDUP (Fcar (current));
    DUP tail_dup = XDUP(Fcar (tail));

    while (!NILP (tail))
      {
        tail_dup = XDUP(Fcar (tail));   

        if (tail_dup->start <= current_dup->end - 1)
          {
            current_dup->end = max (tail_dup->end, current_dup->end);
            Fsetcdr (current, Fcdr (tail));
          }
        else
          {
            current = tail;
            current_dup = XDUP (Fcar (current));
          }
   
        tail = Fcdr (tail);
      }
  }
   
  /* now put back the munged list */
  puthash (key, (void *) dup_list, table);
  return;
}

void
syms_of_extents() 
{
  defsubr(&Sextent_length);
  defsubr(&Sextent_start_position);
  defsubr(&Sextent_end_position);
  defsubr(&Sextent_buffer);
#ifdef ENERGIZE
  defsubr(&Sextent_to_generic_id);
#endif
  defsubr(&Smap_extents);
  defsubr(&Shighlight_extent);
  defsubr(&Sforce_highlight_extent);
  defsubr(&Sextent_at);

  defsubr(&Smake_extent);
  defsubr(&Snext_extent);
  defsubr(&Sdelete_extent);
  defsubr(&Supdate_extent);
  defsubr(&Sset_extent_attribute);
#if 0
  defsubr(&Snext_e_extent);
  defsubr(&Sstack_of_extents);
#endif

  Ffset (intern ("set-extent-endpoints"), intern ("update-extent"));

  DEFVAR_LISP ("   last-highlighted-extent", &Vlast_highlighted_extent,
               "Last highlighted extent; don't touch this kluge.");
  Vlast_highlighted_extent = Qnil;

  DEFVAR_LISP ("   buffer-of-current-extent-fragment", 
               &Vextent_fragment_buffer,
               "Buffer for current extent fragment -- this is a GC hack.");
  Vextent_fragment_buffer = Qnil;

  init_extent_fragment ();
}
