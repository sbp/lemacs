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

#include "config.h"
#include "lisp.h"
#include "buffer.h" 
#include "process.h"

#include "xterm.h" 	/* for struct x_bitmap in set_extent_glyph_1() */

#include "extents.h"
#include "faces.h"
#include "hash.h"

#include <stdio.h>		/* for sprintf */

#ifdef ENERGIZE
extern void restore_energize_extent_state (EXTENT extent);
extern struct Energize_Extent_Data *energize_extent_data (EXTENT);
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
static void soe_push (EXTENT extent, struct buffer *b);
static void soe_delq (EXTENT extent, struct buffer *b);
static void soe_clear (struct buffer *b);

static EXTENT_FRAGMENT befa_internal (int pos, struct buffer *buf);
EXTENT_FRAGMENT buffer_extent_fragment_at (int pos, struct buffer *buf,
                                           struct screen *s);

static void add_to_replicas_lists (c_hashtable table, Lisp_Object dup_list, 
                                   int offset, int length);


/************** Macros ***********************/

#define max(A,B) ((A) > (B) ? (A) : (B))
#define min(A,B) ((A) <= (B) ? (A) : (B))

#define MAX_INT ((long) ((1L << VALBITS) - 1))

#define BUFNAME(buf) (&(XSTRING((buf)->name)->data[0]))
#define SYMNAME(sym) (&(XSYMBOL((sym))->name->data[0]))

/* if buf is the one in the cache, invalidate the cache -- used in
   places where changes to extents list occur that might not affect 
   the buffer modiff */
#define CHECK_RESET_EXTENT_FRAGMENT(x) \
{if (((char *) extent_fragment.buf) == ((char *)(x))) \
  {extent_fragment.buf = 0; Vextent_fragment_buffer = Qnil;}}

#define EXTENT_LESS_VALS(e,st,nd) ((extent_start (e) < (st)) || \
				   ((extent_start (e) == (st)) && \
				   (extent_end (e) > (nd))))

#define EXTENT_LESS(e,x) EXTENT_LESS_VALS(e, extent_start(x), extent_end(x))

#define EXTENT_LESS_EQ(e,x) ((extent_start (e) < extent_start (x)) || \
			     ((extent_start (e) == extent_start (x)) && \
			     (extent_end (e) >= extent_end (x))))

#define EXTENT_E_LESS_VALS(e,st,nd) ((extent_end (e) < (nd)) || \
				     ((extent_end (e) == (nd)) && \
				     (extent_start (e) < (st))))

#define EXTENT_E_LESS(e,x) \
	EXTENT_E_LESS_VALS ((e), extent_start (x), extent_end (x))

#define EXTENT_OVER_INDEX(e,i) (((extent_start (e) <= (i)) && \
				 (extent_end (e) > (i))) || \
				((extent_start (e) == (i)) && \
				 (extent_end (e) == (i))))
  
#define EXTENT_PAST_INDEX(e,i) ((extent_end (e) > (i)) || \
				(((extent_start (e) == (i)) && \
				  (extent_end (e) == (i)))))

/************** Variables ***********************/

Lisp_Object Vlast_highlighted_extent;

static Lisp_Object Vextent_fragment_buffer;
static struct extent_fragment extent_fragment;
static struct extent_fragment default_extent_fragment;

int extent_cache_invalid;	/* set this to force a recomputation */

Lisp_Object Qextentp;
Lisp_Object Qupdate_extent;

Lisp_Object Qhighlight;
Lisp_Object Qunhighlight;
Lisp_Object Qwrite_protected;
Lisp_Object Qwritable;
Lisp_Object Qinvisible;
Lisp_Object Qvisible;
Lisp_Object Qbegin_glyph;
Lisp_Object Qend_glyph;
Lisp_Object Qmenu;
Lisp_Object Qstart_open;
Lisp_Object Qend_open;
Lisp_Object Qcolumn;
Lisp_Object Qdetached;
Lisp_Object Qwarn_modify;
Lisp_Object Qduplicable;
Lisp_Object Qnon_duplicable;

Lisp_Object Qoutside_margin;
Lisp_Object Qinside_margin;
Lisp_Object Qwhitespace;
Lisp_Object Qtext;


#ifdef LRECORD_EXTENT

static Lisp_Object mark_extent (Lisp_Object, void (*) (Lisp_Object));
static Lisp_Object mark_extent_replica (Lisp_Object, void (*) (Lisp_Object));
static int sizeof_extent (void *h) {return (sizeof (struct extent));}
static int sizeof_replica (void *h) {return (sizeof (struct extent_replica));}

/* #### add extent_equal and extent_replica_equal */

DEFINE_LRECORD_IMPLEMENTATION (lrecord_extent,
                               mark_extent, print_extent_or_replica, 
                               0, sizeof_extent, 0);
DEFINE_LRECORD_IMPLEMENTATION (lrecord_extent_replica,
                               mark_extent_replica, print_extent_or_replica,
                               0, sizeof_replica, 0);


static Lisp_Object
mark_extent (Lisp_Object obj, void (*markobj) (Lisp_Object))
{
  struct extent *extent = XEXTENT (obj);
  Lisp_Object next;
  if (gc_record_type_p (extent->ehead.buf, lrecord_extent))
    /* Can't be a replica here! */
    abort ();

  ((markobj) (extent->user_data));

  ((markobj) (extent_buffer (extent)));

  if (!extent->next) return (Qnil);
  if (extent == extent->next) abort ();
  /* Mark the extent and all following extents in the chain.  We can
     stop marking as soon as we hit one that is already marked, because
     we know that we have either already marked the rest of the chain,
     or are about to (higher up on the stack.)

     This relies on the fact that a detached extent has no `next',
     and that extents of dead buffers are detached.
     */
  XSETEXTENT (next, extent->next);
  return (next);
}

static Lisp_Object
mark_extent_replica (Lisp_Object obj, void (*markobj) (Lisp_Object))
{
  struct extent_replica *dup = XDUP (obj);
  if (!gc_record_type_p (dup->ehead.buf, lrecord_extent))
    /* Can't be an extent here! */
    abort ();
  return (dup_extent (dup));
}

#endif /* LRECORD_EXTENT */


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
extent_index_to_buffer_pos (int i, struct buffer *buf)
{
  if (i >= BUF_GPT (buf) + BUF_GAP_SIZE (buf))
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
  else if (pos < BUF_GPT (buf))
    return pos;
  else 
    return (pos + BUF_GAP_SIZE (buf));
}

static int
extent_index_offset (int index, int offset, struct buffer *buf)
{
  int pos = extent_index_to_buffer_pos (index, buf);
  return buffer_pos_to_extent_index (pos + offset, buf);
}

static EXTENT 
buffer_starting_extent (int index, struct buffer *buf)
{
  struct stack_of_extents *soe = buf->cached_stack;
  EXTENT first_extent = NULL;
  EXTENT ext;
  int i;

  /* no extents on the buffer */
  if (!EXTENTP (buf->extents))
    return NULL;
  
  first_extent = XEXTENT (buf->extents);

  /* no cache or cache not valid */
  if (!soe || soe->buf_index <= 0)
    return first_extent;

  /* index is after the cached values */
  if (index >= soe->buf_index)
    {
      ext = NULL;
      for (i = 0; i < soe->stack_index; i++)
	if (extent_end (soe->stack [i]) > index)
	  {
	    ext = soe->stack [i];
	    break;
	  }
      if (!ext)
	ext = soe->previous_extent;
      return ext ? ext : first_extent;
    }

  /* index is before the cached values */

  /* The extent which is before the first one in the 
     cache is a good starting point for backing up
     previous extent. */
  if (soe->stack_index)
    ext = soe->stack [0];
  else
    /* if there is nothing in the cache the previous extent
       can be used */
    ext = soe->previous_extent;

  /* no previous extent in the cache */
  if (!ext)
    return first_extent;
	
  /* previous is closer to index than to the beginning of the buffer */
  if ((extent_end (ext) / 2) < index)
    {
      EXTENT starting_one = ext;

      while (ext && extent_end (ext) >= index)
	{
          ext = ext->e_previous;
          if (ext && extent_start (ext) < extent_start (starting_one))
	    starting_one = ext;
        }
      starting_one = starting_one->previous;
      return (ext && starting_one) ? starting_one : first_extent;
    }

  /* default */
  return first_extent;
}


void
adjust_extents (int from, int to, int amount, struct buffer* buf)
{
  EXTENT current;

  if (NILP (buf->extents))
    return;
  else if (!EXTENTP (buf->extents))
    abort ();

  current = buffer_starting_extent (from, buf);
  
  CHECK_RESET_EXTENT_FRAGMENT (buf);
  
  while (current)
    {
      if (extent_end (current) < from)
	;
      else if (extent_start (current) <= to)
	{
	  extent_start (current) = adjust_extent_index (extent_start (current),
							from, to, amount);
	  if (extent_end (current) <= to)
	    extent_end (current) = adjust_extent_index (extent_end (current),
							from, to, amount);
	}
      else 
	break;
      
      if (current == current->next) abort ();
      current = current->next;
    }
  
  if (buf->cached_stack
      && buf->cached_stack->buf_index > 0
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
      && (extent_end (extent) > ep->start)
      && (extent_start (extent) <= ep->end))
    {
      error ("Attempt to modify a read-only region");
      return 1;
    }
  else
    return 0;
}

#ifdef ENERGIZE
extern int inside_parse_buffer; /* total kludge */
#endif

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
  else if (!EXTENTP (buf->extents))
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
   
  XSETEXTENT (extent_obj, extent);

  init_buffer_cached_stack (buf);

  extents_root = buf->extents;

  if (NILP (extents_root))
    buf->extents = extent_obj;
  else if (!EXTENTP (extents_root))
    abort();
  else
    {
      int start = extent_start (extent);
      int end = extent_end (extent);
      EXTENT tmp = buffer_starting_extent (start, buf);
      EXTENT prev = (tmp ? tmp->previous : 0);

      while (tmp && EXTENT_LESS_VALS (tmp, start, end))
        {
          prev = tmp;
          tmp = tmp->next;
	  if (prev == tmp) abort ();
        }

      if (!tmp && !prev)
        abort();

      if (prev)
        { 
          EXTENT caboose = prev->next;
	  if (extent == prev) abort ();
	  if (extent == caboose) abort ();
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
	  if (extent == tmp) abort ();
          extent->previous = engine; 
          extent->next = tmp; 
          tmp->previous = extent; 

	  buf->extents = extent_obj;
        }

      if (!tmp)		/* one of these exists; pick one */
        tmp = prev;

      /* if tmp (some arbitrary extent in the middle of nowhere but likely
	 to be close to where we want to be) is before the extents we want
	 to insert between (in e order) then go forward
       */
      if (EXTENT_E_LESS_VALS (tmp, start, end))
	{
	  prev = tmp->e_previous;
	  while (tmp && EXTENT_E_LESS_VALS (tmp, start, end))
	    {
	      prev = tmp;
	      tmp = tmp->e_next;
	      if (prev == tmp) abort ();
	    }
	}
      else	/* go backward */
        {
          prev = tmp;
	  while (prev && !EXTENT_E_LESS_VALS (prev, start, end))
            {
	      /* we always go into this loop at least once */
              tmp = prev;
              prev = tmp->e_previous;
            }
	  if (prev == tmp) abort ();
        }

      if (!tmp && !prev)
        abort();

      if (prev)
        { 
          EXTENT caboose = prev->e_next;
	  if (extent == prev) abort ();
	  if (extent == caboose) abort ();
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
	  if (extent == tmp) abort ();
          extent->e_previous = engine; 
          extent->e_next = tmp; 
          tmp->e_previous = extent; 
        }
    }

  soe_push (extent, buf);
  CLEAR_EXTENT_FLAG (extent, EF_DETACHED);
}

Lisp_Object
make_extent_internal (int from, int to, Lisp_Object buffer)
{
  EXTENT extent;
  Lisp_Object extent_obj = Qnil;
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
    
  extent = make_extent();
  XSETEXTENT (extent_obj, extent);

  extent_buffer (extent) = buffer;
  extent->flags = 0;
  extent->priority = 0;

  extent_start (extent) = buffer_pos_to_extent_index (from, buf);
  extent_end (extent) = buffer_pos_to_extent_index (to, buf);

  splice_extent_into_buffer (extent, buffer);

  return extent_obj;
}


static int 
extent_endpoint (EXTENT extent, int endp)
{
  int i = (endp)?(extent_end (extent)):(extent_start (extent));
  if (EXTENT_DESTROYED_P (extent))
    return -1;
  else if (EXTENT_DETACHED_P (extent))
    return 0;
  else if (BUFFERP (extent_buffer (extent)))
    return extent_index_to_buffer_pos (i, XBUFFER(extent_buffer (extent)));
  else
    return i;
}


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
  struct buffer *buf = XBUFFER(extent_buffer (extent));
  Lisp_Object obj_extent = buf->extents;

  CHECK_RESET_EXTENT_FRAGMENT (buf);

  soe_delq (extent, buf);

  if (XEXTENT (obj_extent) == extent)
    {
      if (next)
        XSETEXTENT (obj_extent, next);
      else
        obj_extent = Qnil;
      
      XBUFFER (extent_buffer (extent))->extents = obj_extent;
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
      if (e_next == e_prev) abort ();
      e_prev->e_next = e_next; 
      extent->e_previous = 0;
    }
  if (e_next) 
    {
      if (e_next == e_prev) abort ();
      e_next->e_previous = e_prev; 
      extent->e_next = 0; 
    }

  SET_EXTENT_FLAG (extent, EF_DETACHED);

  extent_start (extent) = 0;
  extent_end (extent) = 0;
}

/* Detatch all extents in the buffer (called when buffer is killed.) */
void
detach_buffer_extents (struct buffer *b)
{
  EXTENT e;
  if (!EXTENTP (b->extents)) return;
  e = XEXTENT (b->extents);
  b->extents = Qnil;
  while (e)
    {
      EXTENT next = e->next;
      detach_extent (e);
      e = next;
    }
}

static void 
destroy_extent (EXTENT extent) 
{
  detach_extent (extent);
  extent->flags = EF_DESTROYED;
  extent_start (extent) = 0;
  extent_end (extent) = 0;
  extent->next = 0;
  extent->previous = 0;
  extent->e_next = 0;
  extent->e_previous = 0;
  extent->attr_index = 0;
  extent_buffer (extent) = Qnil;
}

void   
update_extent_1 (EXTENT extent, int from, int to, int set_endpoints,
		 struct buffer *buf)
{
  if ((!BUFFERP (extent_buffer (extent))) ||
      (XBUFFER(extent_buffer (extent)) != buf))
    error ("extent 0x%x not part of specified buffer %s",
	   (int) extent, 
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
	  XSETR (buffer, Lisp_Buffer, buf);
	  
	  detach_extent (extent);
	  extent_start (extent) = new_start;
	  extent_end (extent) = new_end;
	  splice_extent_into_buffer (extent, buffer);
	}
    }

#ifdef ENERGIZE
  restore_energize_extent_state (extent);
#endif
}


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
map_extents (int from, int to,
             elisp_emf efn, emf fn,
             void *arg, 
             struct buffer *buf, 
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

  if (!EXTENTP (extents_root))
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
	    if (next == tmp) abort ();
            if (extent_end (tmp) < start)
              tmp = next;
            else if (extent_start (tmp) < end)
              {
                Lisp_Object tmp_obj;
                XSETEXTENT (tmp_obj, tmp);
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
	    if (next == tmp) abort ();
            if (extent_end (tmp) < start)
              tmp = next;
            else if (extent_start (tmp) < end)
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
       "Map FUNCTION over the extents which overlap region in BUFFER,\n\
starting at FROM and ending at TO.  FUNCTION is called with the arguments\n\
 (extent, MAPARG).\n\
The arguments FROM, TO, MAPARG, and BUFFER default to the beginning of\n\
 BUFFER, the end of BUFFER, nil, and (current-buffer\, respectively.\n\
If the map function returns non-nil, then map-extents returns immediately.\n\
map-extents always returns nil.")
  (function, buffer, from, to, maparg, closed_end)
  Lisp_Object function, buffer, from, to, maparg, closed_end;
{
  elisp_emf map_funct;
  int closed;
  
  if (!NILP (closed_end))
    closed = 1;
  else 
    closed = 0;

  if (NILP (buffer))
    XSETR (buffer, Lisp_Buffer, current_buffer);
  CHECK_BUFFER (buffer, 0);
  

  if (NILP (from)) XSET (from, Lisp_Int, BUF_BEG (XBUFFER (buffer)));
  if (NILP (to)) XSET (to, Lisp_Int, BUF_Z (XBUFFER (buffer)));
  CHECK_FIXNUM (from, 1);
  CHECK_FIXNUM (to, 2);
  check_from_to (XINT(from), XINT(to), (XBUFFER (buffer)));
  if (XINT(from) > XINT (to))
    error ("MAP-EXTENTS args FROM (== %d) and TO (== %d) are inconsistent.",
           XINT (from), XINT (to));
  
  if (SUBRP (function))
    {
      map_funct = (elisp_emf) subr_function (XSUBR (function));
      map_extents (XINT (from), XINT (to), map_funct, 0,
                   LISP_TO_VOID (maparg), XBUFFER (buffer), closed);
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
static int
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
do_highlight (Lisp_Object extent_obj, Lisp_Object flag)
{
  if (((EQ (Vlast_highlighted_extent, extent_obj)) && !NILP(flag)) ||
      (NILP (Vlast_highlighted_extent) && NILP (flag)))
    return Qnil;
  else
    {
      Lisp_Object old_parent = 
        (EXTENTP (Vlast_highlighted_extent)
	 ? extent_buffer (XEXTENT(Vlast_highlighted_extent))
	 : Qnil);
      Lisp_Object new_parent = 
        (EXTENTP (extent_obj)
	 ? extent_buffer (XEXTENT (extent_obj))
	 : Qnil);

      if (BUFFERP (old_parent))
        {
          Vlast_highlighted_extent = Qnil;
          BUF_FACECHANGE (XBUFFER (old_parent))++;
          windows_or_buffers_changed++;
        }

      if ((BUFFERP (new_parent)) &&
          !NILP (flag))
        {
          Vlast_highlighted_extent = extent_obj;
          BUF_FACECHANGE (XBUFFER (new_parent))++;
          windows_or_buffers_changed++;
        }
      else
        Vlast_highlighted_extent = Qnil;
    }
  return Qnil;
}

DEFUN ("highlight-extent", Fhighlight_extent, Shighlight_extent, 1, 2, 0,
 "If EXTENT is `highlightable' (has the 'highlight property) then highlight\n\
it (by using merging it with 'highlight face.)  If FLAG is nil, then\n\
unhighlight it instead.")
     (extent_obj, flag)
     Lisp_Object extent_obj, flag;
{
  if (NILP (extent_obj))
    return do_highlight (Qnil, Qnil);
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
    return do_highlight (Qnil, Qnil);
  else if (!EXTENTP (extent_obj))
    return Qnil;
  else
    return do_highlight (extent_obj, flag);
}


/*********************************
  EXTENTS.C DATATYPE FUNCTIONS
  *******************************/

GLYPH
extent_glyph_at (EXTENT extent, int pos, int endp)
{
  if (! extent)
    return 0;
  else if (! BUFFERP (extent_buffer (extent)))
    return 0;
  else if (endp 
           && EXTENT_FLAG_P (extent, EF_END_GLYPH)
           && pos == (extent_endpoint (extent, 1) - 1))
    return extent->glyph;
  else if (!endp 
           && EXTENT_FLAG_P (extent, EF_START_GLYPH)
           && pos == extent_endpoint (extent, 0))
    return extent->glyph;
  else
    return 0;
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
  return (extent_buffer (XEXTENT (extent_obj)));
}


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

/* This is the only place `PT' is an lvalue in all of emacs. */

static void
set_point_internal (int charno)
{
  BUF_PT (current_buffer) = charno;
  if (charno > GPT)
    charno += GAP_SIZE;
  XMARKER (current_buffer->point_marker)->bufpos = charno;
}

void
set_point (position)
     int position;
{
  int opoint = point;

  if (position == opoint)
    return;
  else if (position > Z)
    abort();

  if (position > ZV || position < BEGV)
    abort ();

  set_point_internal (position);

  if (EQ (current_buffer->extents, Qzero))
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
      int speccount = specpdl_depth ();
      record_unwind_protect (Fset_buffer, Fcurrent_buffer ());
      set_buffer_internal (buffer);
      set_point (position);
      unbind_to (speccount, Qnil);
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
      else if (extent_start (current) > extent_start (extent))
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
  else if (!EXTENTP (buf->extents))
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


DEFUN ("extentp", Fextentp, Sextentp, 1, 1, 0,
  "T if OBJECT is an extent..")
  (extent)
     Lisp_Object extent;
{
  if (EXTENTP (extent))
    return Qt;
  return Qnil;
}


void
print_extent_or_replica (Lisp_Object obj, 
                         Lisp_Object printcharfun, int escapeflag)
{
  if (EXTENTP (obj))
    {
      if (escapeflag)
	{
	  const char *title = "";
	  const char *name = "";

	  if (BUFFERP (extent_buffer (XEXTENT (obj))))
	    {
	      struct buffer *buf = XBUFFER (extent_buffer (XEXTENT (obj)));
	      if (STRINGP (buf->name))
		{
		  title = " buffer ";
		  name = (char *) BUFNAME (buf);
		}
	      else
		{
		  title = " Killed Buffer";
		  name = "";
		}
	    }
	  
	  if (print_readably)
	    {
	      if (EXTENT_FLAGS (XEXTENT (obj)) & EF_DESTROYED)
		error ("printing unreadable object #<destroyed extent>");
	      else
		error ("printing unreadable object #<extent [%d, %d)>",
		       XINT(Fextent_start_position (obj)), 
		       XINT(Fextent_end_position (obj)));
	    }
	  
	  if (EXTENT_FLAGS (XEXTENT (obj)) & EF_DESTROYED)
	    write_string_1 ("#<destroyed extent", -1, printcharfun);
	  else
	    {
	      char buf[50];
	      write_string_1 ("#<extent ", -1, printcharfun);
	      if (EXTENT_FLAGS (XEXTENT (obj)) & EF_DETACHED)
		sprintf (buf, "(flags=0x%x) detached from", 
			 EXTENT_FLAGS (XEXTENT (obj)));
	      else
		sprintf (buf, "[%d, %d) (flags=0x%x) in",
			 XINT (Fextent_start_position (obj)), 
			 XINT (Fextent_end_position (obj)),
			 EXTENT_FLAGS (XEXTENT (obj)));
	      write_string_1 (buf, -1, printcharfun);
	      write_string_1 (title, -1, printcharfun);
	      write_string_1 (name, -1, printcharfun);
	    }
	}
      else
	{
	  if (print_readably)
	    error ("printing unreadable object #<extent>");
	  write_string_1 ("#<extent", -1, printcharfun);
	}
      write_string_1 (">", 1, printcharfun);
    }
  else if (EXTENT_REPLICA_P (obj))
    {
      if (escapeflag)
	{
	  char buf[50];

	  if (print_readably)
	    error ("printing unreadable object #<dup [%d, %d)>",
		   dup_start (XDUP (obj)), dup_end (XDUP (obj)));

	  write_string_1 ("#<dup ", -1, printcharfun);
	  if (EXTENTP (dup_extent (XDUP (obj))))
	    {
	      Lisp_Object extent_obj = dup_extent (XDUP (obj));
 	      sprintf (buf, "[%d, %d) of ",
 		       dup_start (XDUP (obj)), dup_end (XDUP (obj)));
              write_string_1 (buf, -1, printcharfun);
              print_extent_or_replica (extent_obj, printcharfun, escapeflag);
	    }
	  else
	    {
	      sprintf (buf, "[%d, %d) of 0x%x??!!!",
		       dup_start (XDUP (obj)), dup_end (XDUP (obj)),
		       (int) LISP_TO_VOID (dup_extent (XDUP (obj))));
              write_string_1 (buf, -1, printcharfun);
	    }
	}
      else
	{
	  if (print_readably)
	    error ("printing unreadable object #<extent>");
	  write_string_1 ("#<dup", -1, printcharfun);
	}
      write_string_1 (">", -1, printcharfun);
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
  CHECK_FIXNUM (from, 0);
  CHECK_FIXNUM (to, 0);
  if (NILP (buffer))
    XSETR (buffer, Lisp_Buffer, current_buffer);
  CHECK_BUFFER (buffer, 0);
  extent_obj = make_extent_internal (XINT (from), XINT (to), buffer);
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

  if (!BUFFERP (extent_buffer (extent))
      || NILP (XBUFFER (extent_buffer (extent))->name))
    error ("Deleting extent in killed buffer");

  BUF_FACECHANGE (XBUFFER (extent_buffer (extent)))++;
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
  CHECK_FIXNUM (start, 1);
  CHECK_FIXNUM (end, 2);

  if (!BUFFERP (extent_buffer (extent))
      || NILP (XBUFFER (extent_buffer (extent))->name))
    error ("EXTENT arg to UPDATE-EXTENT doesn't belong to a buffer");

  check_from_to (from, to, XBUFFER (extent_buffer (extent)));
      
  if (from > to)
    error ("Bad START (== %d) and END (== %d) args to UPDATE-EXTENT", from,to);

  detach_extent (extent);
  extent_start (extent) =
    buffer_pos_to_extent_index (from, XBUFFER (extent_buffer (extent)));
  extent_end (extent) =
    buffer_pos_to_extent_index (to, XBUFFER (extent_buffer (extent)));
  splice_extent_into_buffer (extent, extent_buffer (extent));

  BUF_FACECHANGE (XBUFFER (extent_buffer (extent)))++;
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
  
  if (!BUFFERP (extent_buffer (extent))
      || NILP (XBUFFER (extent_buffer (extent))->name))
    error ("EXTENT arg to SET-EXTENT-ATTRIBUTE doesn't belong to a buffer");

  if (FIXNUMP (attr))
    {
      int attr_num = XINT (attr);
      if (attr_num < 0)
        attr_num = -1;
      else if (attr_num >= GA_MAX) 
        attr_num = GA_NO_CHANGE;
      extent->attr_index = attr_num;
    }
  else if (SYMBOLP (attr))
    {
      if (EQ (Qhighlight, attr))
        { SET_EXTENT_FLAG (extent, EF_HIGHLIGHT); }
      else if (EQ (Qunhighlight, attr))
        { CLEAR_EXTENT_FLAG (extent, EF_HIGHLIGHT); }
      else if (EQ (Qwrite_protected, attr))
        { SET_EXTENT_FLAG (extent, EF_WRITE_PROTECT); }
      else if (EQ (Qwritable, attr))
        { CLEAR_EXTENT_FLAG (extent, EF_WRITE_PROTECT); }
      else if (EQ (Qinvisible, attr))
        { SET_EXTENT_FLAG (extent, EF_INVISIBLE); }
      else if (EQ (Qvisible, attr))
        { CLEAR_EXTENT_FLAG (extent, EF_INVISIBLE); }
      else if (EQ (Qduplicable, attr))
        { SET_EXTENT_FLAG (extent, EF_DUPLICABLE); }
      else if (EQ (Qnon_duplicable, attr))
        { CLEAR_EXTENT_FLAG (extent, EF_DUPLICABLE); }
      else
        error ("Unknown attribute argument, %s, to set-extent-attribute.", 
               SYMNAME (attr));
    }
  else
    error ("Unknown type of attribute argument to SET-EXTENT-ATTRIBUTE.");

  BUF_FACECHANGE (XBUFFER (extent_buffer (extent)))++;
  windows_or_buffers_changed++;

  return extent_obj;
}


DEFUN ("extent-attributes", Fextent_attributes, Sextent_attributes, 1, 2, 0,
 "Return a list of attributes of EXTENT.\n\
This list may contain any or none of the following symbols:\n\
\n\
    highlight		highlight when the mouse moves over it\n\
    write-protected	text within this extent will be unmodifyable\n\
    invisible		don't display the text in this region\n\
    begin-glyph		there is a begin-glyph\n\
    end-glyph		there is an end-glyph\n\
    detached		the text around the extent has been deleted\
")
  (extent_obj, raw_p)
   Lisp_Object extent_obj, raw_p;
{
  Lisp_Object result = Qnil;
  EXTENT extent;
  CHECK_EXTENT (extent_obj, 0);

  extent = XEXTENT (extent_obj);

  if (!NILP (raw_p))
    return make_number (extent->attr_index);

  if (EXTENT_FLAG_P (extent, EF_HIGHLIGHT))
    result = Fcons (Qhighlight, result);
  if (EXTENT_FLAG_P (extent, EF_WRITE_PROTECT))
    result = Fcons (Qwrite_protected, result);
  if (EXTENT_FLAG_P (extent, EF_INVISIBLE))
    result = Fcons (Qinvisible, result);
  if (EXTENT_FLAG_P (extent, EF_START_GLYPH))
    result = Fcons (Qbegin_glyph, result);
  if (EXTENT_FLAG_P (extent, EF_END_GLYPH))
    result = Fcons (Qend_glyph, result);
  if (EXTENT_FLAG_P (extent, EF_MENU))
    result = Fcons (Qmenu, result);
  if (EXTENT_FLAG_P (extent, EF_START_OPEN))
    result = Fcons (Qstart_open, result);
  if (EXTENT_FLAG_P (extent, EF_END_OPEN))
    result = Fcons (Qend_open, result);
  if (EXTENT_FLAG_P (extent, EF_COLUMN))
    result = Fcons (Qcolumn, result);
  if (EXTENT_FLAG_P (extent, EF_DETACHED))
    result = Fcons (Qdetached, result);
  if (EXTENT_FLAG_P (extent, EF_WARN_MODIFY))
    result = Fcons (Qwarn_modify, result);
  if (!EXTENT_FLAG_P (extent, EF_DUPLICABLE))
    result = Fcons (Qnon_duplicable, result);
  return result;
}

extern GLYPH x_get_pixmap (Lisp_Object, char *hash_suffix);

static void
set_extent_glyph_1 (Lisp_Object extent_obj, Lisp_Object glyph, int endp,
		    Lisp_Object layout)
{
  int change_p;
  int which = (endp ? EF_END_GLYPH : EF_START_GLYPH);
  EXTENT extent = XEXTENT (extent_obj);
  CHECK_EXTENT (extent_obj, 0);
  
  if (!BUFFERP (extent_buffer (extent))
      || NILP (XBUFFER (extent_buffer (extent))->name))
    error ("extent doesn't belong to a buffer");

  if (NILP (glyph))
    {
      change_p = !EXTENT_FLAG_P (extent, which);
      CLEAR_EXTENT_FLAG (extent, which);
      extent->glyph = 0;
    }
  else
    {
      if (endp && EXTENT_FLAG_P (extent, EF_START_GLYPH))
	signal_error (Qerror,
		      list2 (build_string ("cannot set end glyph while begin glyph is set"),
			     extent_obj));
      else if (!endp && EXTENT_FLAG_P (extent, EF_END_GLYPH))
	signal_error (Qerror,
		      list2 (build_string ("cannot set begin glyph while end glyph is set"),
			     extent_obj));

      if (NILP (layout))
	extent->layout = GL_TEXT;
      else
	{
	  CHECK_SYMBOL (layout, 0);
 	  if (EQ (Qoutside_margin, layout))
	    { SET_EXTENT_GLYPH_LAYOUT (extent, GL_OUTSIDE_MARGIN); }
	  else if (EQ (Qinside_margin, layout))
	    { SET_EXTENT_GLYPH_LAYOUT (extent, GL_INSIDE_MARGIN); }
	  else if (EQ (Qwhitespace, layout))
	    { SET_EXTENT_GLYPH_LAYOUT (extent, GL_WHITESPACE); }
	  else if (EQ (Qtext, layout))
	    { SET_EXTENT_GLYPH_LAYOUT (extent, GL_TEXT); }
	  else
	    {
	      if (endp)
		error ("Unknown layout policy, %s, to set-extent-end-glyph.",
		       SYMNAME (layout));
	      else
		error ("Unknown layout policy, %s, to set-extent-begin-glyph.",
		       SYMNAME (layout));
	    }
	}

      CHECK_STRING (glyph, 0);

      {
	GLYPH new_glyph = x_get_pixmap (glyph, 0);
	change_p = (!EXTENT_FLAG_P (extent, which) ||
		    extent->glyph != new_glyph);
	SET_EXTENT_FLAG (extent, which);
	extent->glyph = new_glyph;
      }
    }
  if (change_p)
    {
      BUF_FACECHANGE (XBUFFER (extent_buffer (extent)))++;
      windows_or_buffers_changed++;
    }
}

DEFUN ("set-extent-begin-glyph", Fset_extent_begin_glyph, 
       Sset_extent_begin_glyph, 2, 3, 0,
 "Display a bitmap at the beginning of the given extent.\n\
The begin-glyph should be a string naming a bitmap file (or nil.)\n\
The layout policy defaults to `text'.")
  (extent_obj, begin_glyph, layout)
   Lisp_Object extent_obj, begin_glyph, layout;
{
  set_extent_glyph_1 (extent_obj, begin_glyph, 0, layout);
  return extent_obj;
}


DEFUN ("set-extent-end-glyph", Fset_extent_end_glyph, 
       Sset_extent_end_glyph, 2, 3, 0,
 "Display a bitmap at the end of the given extent.\n\
The end-glyph should be a string naming a bitmap file (or nil.)\n\
The layout policy defaults to `text'.")
  (extent_obj, end_glyph, layout)
   Lisp_Object extent_obj, end_glyph, layout;
{
  set_extent_glyph_1 (extent_obj, end_glyph, 1, layout);
  return extent_obj;
}

DEFUN ("set-extent-layout", Fset_extent_layout,
       Sset_extent_layout, 2, 2, 0,
  "Set the layout policy of the given extent.\n\
Access this using the `extent-layout' function.")
	(extent, layout)
	Lisp_Object extent, layout;
{
  EXTENT e;

  CHECK_EXTENT (extent, 0);
  CHECK_SYMBOL (layout, 0);
  e = XEXTENT (extent);

  if (EQ (Qoutside_margin, layout))
    { SET_EXTENT_GLYPH_LAYOUT (e, GL_OUTSIDE_MARGIN); }
  else if (EQ (Qinside_margin, layout))
    { SET_EXTENT_GLYPH_LAYOUT (e, GL_INSIDE_MARGIN); }
  else if (EQ (Qwhitespace, layout))
    { SET_EXTENT_GLYPH_LAYOUT (e, GL_WHITESPACE); }
  else if (EQ (Qtext, layout))
    { SET_EXTENT_GLYPH_LAYOUT (e, GL_TEXT); }
  else
    signal_error (Qerror, list2 (build_string ("unknown layout type"),
				 layout));
  
  return layout;
}

DEFUN ("extent-layout", Fextent_layout, Sextent_layout, 1, 1, 0,
  "Return the layout policy associated with the given extent.\n\
Set this using the `set-extent-layout' function.")
	(extent)
	Lisp_Object extent;
{
  struct extent *e;

  CHECK_EXTENT (extent, 0);
  e = XEXTENT (extent);

  if (EXTENT_GLYPH_LAYOUT_P (e, GL_OUTSIDE_MARGIN))
    return Qoutside_margin;
  else if (EXTENT_GLYPH_LAYOUT_P (e, GL_INSIDE_MARGIN))
    return Qinside_margin;
  else if (EXTENT_GLYPH_LAYOUT_P (e, GL_WHITESPACE))
    return Qwhitespace;
  else if (EXTENT_GLYPH_LAYOUT_P (e, GL_TEXT))
    return Qtext;
  else
    return Qnil;
}

DEFUN ("extent-data", Fextent_data, Sextent_data, 1, 1, 0,
 "Return the user data associated with the given extent.\n\
Set this using the `set-extent-data' function.")
	(extent)
	Lisp_Object extent;
{
  CHECK_EXTENT (extent, 0);
  return XEXTENT (extent)->user_data;
}

DEFUN ("set-extent-data", Fset_extent_data, Sset_extent_data, 2, 2, 0,
 "Set the user data slot of the given extent.\n\
Access this using the `extent-data' function.")
	(extent, data)
	Lisp_Object extent, data;
{
  CHECK_EXTENT (extent, 0);
#ifdef ENERGIZE
  if (energize_extent_data (XEXTENT (extent)))
    error ("thou shalt not change the user-data of Energize extents");
#endif
  XEXTENT (extent)->user_data = data;
  return data;
}

DEFUN ("extent-priority", Fextent_priority, Sextent_priority, 1, 1, 0,
  "Returns the display priority of EXTENT; see `set-extent-priority'.")
     (extent)
     Lisp_Object extent;
{
  CHECK_EXTENT (extent, 0);
  return make_number (XEXTENT (extent)->priority);
}

DEFUN ("set-extent-priority", Fset_extent_priority, Sset_extent_priority,
       2, 2, 0,
  "Changes the display priority of EXTENT.\n\
When the extent attributes are being merged for display, the priority\n\
is used to determine which extent takes precedence in the event of a\n\
conflict (two extents whose faces both specify font, for example: the\n\
font of the extent with the higher priority will be used.)\n\
Extents are created with priority 0; priorities may be negative.")
	(extent, pri)
	Lisp_Object extent, pri;
{
  int p;
  CHECK_EXTENT (extent, 0);
  CHECK_FIXNUM (pri, 0);
  p = XINT (pri);
  if (p < -0x8000 || p > 0x7fff)	/* must fit in a short */
    error ("extent priority out of range");
  XEXTENT (extent)->priority = p;
  return pri;
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
  int position;
  int flag_to_check = 0;
  Lisp_Object extent_obj;
  EXTENT extent;

  if (NILP (buffer))
    XSETR (buffer, Lisp_Buffer, current_buffer);

  CHECK_FIXNUM_COERCE_MARKER (pos, 0);
  CHECK_BUFFER (buffer, 1);
  position = XINT (pos);
  check_from_to (position, position, XBUFFER (buffer));

  if (!NILP (flag))
    {
      CHECK_SYMBOL (flag, 2);

      if (EQ (Qmenu, flag))
        flag_to_check = EF_MENU;
      else if (EQ (Qhighlight, flag))
        flag_to_check = EF_HIGHLIGHT;
      else if (EQ (Qwrite_protected, flag))
        flag_to_check = EF_WRITE_PROTECT;
      else if (EQ (Qbegin_glyph, flag))
        flag_to_check = EF_START_GLYPH;
      else if (EQ (Qend_glyph, flag))
        flag_to_check = EF_END_GLYPH;
      else if (EQ (Qinvisible, flag))
        flag_to_check = EF_INVISIBLE;
      else if (EQ (Qduplicable, flag))
        flag_to_check = EF_DUPLICABLE;
      else
        error ("%s is unknown flag argument for extent-at", SYMNAME (flag));
    }

  extent = extent_at (position, XBUFFER (buffer), flag_to_check);
  if (!extent)
    return (Qnil);

  XSETEXTENT (extent_obj, extent);
  return (extent_obj);
}

DEFUN ("next-extent", Fnext_extent, Snext_extent, 1, 1, 0,
       "Find next extent after EXTENT. If EXTENT is a buffer\n\
return the first extent in the buffer.")
  (extent)
   Lisp_Object extent;
{
  Lisp_Object val;
  EXTENT next;

  if (BUFFERP (extent))
    return XBUFFER (extent)->extents;

  CHECK_EXTENT (extent, 0);
  next = XEXTENT (extent)->next;
  if (next == XEXTENT (extent)) abort ();

  if (!next)
    return (Qnil);
  XSETEXTENT (val, next);
  return (val);
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
  if (BUFFERP (extent_obj))
    {
      Lisp_Object return_val;
      EXTENT tmp;

      if (EXTENTP (XBUFFER (extent_obj)->extents))
        tmp = XEXTENT (XBUFFER (extent_obj)->extents);
      else
        return Qnil;

      while (tmp->e_previous)
        tmp = tmp->e_previous;

      XSETEXTENT (return_val, tmp);
      return return_val;
    }
  else if (EXTENTP (extent_obj))
    {
      Lisp_Object return_val = Qnil;
      EXTENT next = XEXTENT(extent_obj)->e_next;

      if (next)
        XSETEXTENT (return_val, next);
      return return_val;
    }
  else
    return Qnil;
}


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
      Lisp_Object struct_vec = make_vector (6, Qzero);
      Lisp_Object *struct_v = &(XVECTOR (struct_vec)->contents[0]);
      Lisp_Object stack_vec = make_vector (soe->stack_index, Qzero);
      Lisp_Object *stack_v = &(XVECTOR (stack_vec)->contents[0]);

      struct_v[0] = intern (":PRIOR-EXTENT");
      if (soe->previous_extent)
        XSETEXTENT (struct_v[1], soe->previous_extent);
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
                XSETEXTENT (stack_v[i], tmp);
              }
            else 
              stack_v[i] = Qnil;
          }
      }

      return struct_vec;
    }
}

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
      CHECK_FIXNUM (position, 1);
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
        xfree (tmp_stack);
      xfree (soe);
    }

  return return_value;
}

#endif

#if 0
/* Debugging and test function -- maybe permanent. */
/* Return 0 if stack is correct, 1 if stack has been cleared (which
   is not incorrect but isn't good news), and -1 if it is provably
   incorrect. */
static int
verify_buffer_stack (struct buffer *buf)
{
  struct stack_of_extents *soe;

  if (!buf)
    buf = current_buffer;

  soe = buf->cached_stack;

  if (!soe)
    {
      if (EXTENTP (buf->extents))
        return -1;
      else if (NILP (buf->extents))
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
      
      return_value = memcmp ((char *) soe, (char *) buf->cached_stack,
			     sizeof (*soe));
      if (!return_value)
        return_value = 
          memcmp ((char *) soe->stack, (char *) buf->cached_stack->stack, 
		  soe->stack_index * sizeof (EXTENT));
      if (return_value)
        return_value = -1;
      free_buffer_cached_stack (buf);
      buf->cached_stack = soe;
      return return_value;
    }
}
#endif /* 0 */


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

static void
soe_push (EXTENT extent, struct buffer *b)
{
  struct stack_of_extents *soe = b->cached_stack;
  if (soe)
    {
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
      else if (extent_end (extent) < index)
	{
	  EXTENT prev = soe->previous_extent;
	  if (!prev || EXTENT_E_LESS (prev, extent))
	    soe->previous_extent = extent;
	}
    }
}

static void
soe_duplicate (int index, EXTENT *copy_from, int copy_from_size, 
	       EXTENT trial_prev, struct buffer *b)
{
  struct stack_of_extents *soe = b->cached_stack;

  if (!soe)
    return;

  if (copy_from_size > 0)
    {
      if (soe->stack_length < copy_from_size)
	{
	  soe->stack_length = copy_from_size + 1;
	  soe->stack = 
            (EXTENT*)xrealloc (soe->stack, 
                               soe->stack_length * sizeof (EXTENT));
	}
      memcpy ((char *) soe->stack, (char *) copy_from,
	      copy_from_size * sizeof (EXTENT));

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


static void
soe_delq (EXTENT extent, struct buffer *b)
{
  struct stack_of_extents *soe = b->cached_stack;
  if (soe)
    {
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
}

void 
init_buffer_cached_stack (struct buffer *b)
{
  if (!b->cached_stack)
    {
      int default_stack_size = 10;
      struct stack_of_extents *new =
        (struct stack_of_extents *) 
          xmalloc (sizeof (struct stack_of_extents));
      memset ((char *) new, 0, sizeof (struct stack_of_extents));
      new->stack_length = default_stack_size;
      new->stack = (EXTENT *) xmalloc (default_stack_size * sizeof (EXTENT));
      b->cached_stack = new;
    }
}

static void
soe_clear (struct buffer *b)
{
  struct stack_of_extents *soe = b->cached_stack;
  if (soe)
    {
      soe->stack_index = 0;
      soe->buf_index = 0;
      soe->previous_extent = 0;
    }
}

static void
soe_prune (struct buffer *b)
{
  struct stack_of_extents *soe = b->cached_stack;

  if (!soe)
    return;
  
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

void
free_buffer_cached_stack (struct buffer *b)
{
  struct stack_of_extents *tmp = b->cached_stack;
  b->cached_stack = 0;
  if (tmp)
    {
      EXTENT *tmp_stack = tmp->stack;
      tmp->stack = 0;
      if (tmp_stack)
        xfree (tmp_stack);
      xfree (tmp);
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
  LISP_WORD_TYPE new_start = ef->start_index;
  LISP_WORD_TYPE min_for_end = 
    (ef->first_extent_past_stack
     ? extent_start (ef->first_extent_past_stack)
     : MAX_INT);

  while (cef < cef_bound)
    {
      LISP_WORD_TYPE end = extent_end (*cef);
      LISP_WORD_TYPE start = extent_start (*cef);
   
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
  else if (buf_index < extent_start (next))
    {
      /* ef->first_extent_past_stack is correct here, because it  won't
         change as the result of this operation */
      if (ef->number_of_extents == 1)
        {
          /* easy case for updating stack -- moving into a "hole" between
             extents */
          ef->number_of_extents = 0;
          ef->end_index = extent_start (next);
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
    /* stack is empty so needs no pruning, and we know that extent_end (next)
       is at least as big as the fragment end_index */
    ef->end_index = extent_end (next);

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
	if (next == last) abort ();
	if (next && next->previous != last) abort ();
      }
    while (next && (extent_start (next) == buf_index));

    /* make sure that ef->end_index is correct, since we may have
       come into this function with a value that is too big -- 
       recall that since the end values of the extents are
       decreasing while the start value stay the same, last->end
       has the smallest "end" of all things pushed onto the stack */
    if (extent_end (last) < ef->end_index)
      ef->end_index = extent_end (last);
    if (next &&
        (extent_start (next) < ef->end_index))
      ef->end_index = extent_start (next);
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
  EXTENT_FRAGMENT ef;
  EXTENT current;
  EXTENT trial_prev = 0;
  int buf_index;
  LISP_WORD_TYPE new_start;
  LISP_WORD_TYPE new_end;

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
         (extent_start (current) <= buf_index))
    {
      trial_prev = current;

      if ((extent_end (current) <= buf_index) &&
	  (extent_end (current) > new_start))
        {
          new_start = extent_end (current);
        }
      /* we repeat some code in this clause and the next to save tests */
      else if (extent_end (current) > buf_index)
	{
          if (extent_start (current) > new_start)
            new_start = extent_start (current);
          if (extent_end (current) < new_end)
            new_end = extent_end (current);

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
      else if ((extent_end (current) == extent_start (current)) &&
	       (extent_end (current) == buf_index))
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
        
      if (current == current->next) abort ();
      current = current->next;
    }

  /* Check the end_index for this fragment. */
  if (current && 
      (extent_start (current) < new_end))
    new_end = extent_start (current);
  
  XSETR (Vextent_fragment_buffer, Lisp_Buffer, buf);
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
  LISP_WORD_TYPE buf_index;

  if (NILP (buf->extents))
    {
      Vextent_fragment_buffer = Qnil;
      extent_fragment.buf = 0;
      default_extent_fragment.from = BUF_BEG (buf);
      default_extent_fragment.to = MAX_INT;
      return &default_extent_fragment;
    }
  else if (!EXTENTP (buf->extents))
    abort ();

  buf_index = buffer_pos_to_extent_index (pos, buf);
  ef = &extent_fragment;
  cache_valid = (!extent_cache_invalid &&
		 (buf == ef->buf) && 
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
  extent_cache_invalid = 0;
  return ef;
}

static void
init_extent_fragment ()
{
  int l = 30;

  memset ((char *) &extent_fragment, 0, sizeof (extent_fragment));
  extent_fragment.extents_stack_length = l;
  extent_fragment.extents_stack = (EXTENT *) xmalloc (l * sizeof (EXTENT));

  memset ((char *) &default_extent_fragment, 0,
	  sizeof (default_extent_fragment));
  extent_cache_invalid = 1;
}

/* Modify all of the extents as required for the insertion. At the
   moment this function does nothing, but eventually it probably should
   adjust the endpoints of the extents that touch point in a manner that
   takes the the opened/closed property of the endpoint into account. */
void
process_extents_for_insertion (int opoint, int length, struct buffer *buf)
{
  return;
}
   
static int   
process_extents_for_deletion_mf (EXTENT extent, void *arg)
{
  struct process_extents_for_deletion_struct *peds_ptr = 
    (struct process_extents_for_deletion_struct *) arg;
  
  if ((peds_ptr->start <= extent_start (extent)) &&
      (extent_end (extent) <= peds_ptr->end))
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
  else if ((extent_start (extent) < peds_ptr->start) &&
	   (peds_ptr->end < extent_end (extent)))
    return 0;
  else
    /* these characters are going away, so the extent must be shortened 
       appropriately -- this code should probably do something about
       opened/closed endpoints, too */
    {
      LISP_WORD_TYPE max_start = max (extent_start (extent), peds_ptr->start);
      LISP_WORD_TYPE min_end = min (extent_end (extent), peds_ptr->end);
      /* this test is really unneeded, since map_extents() promises the
         two "spans of text" will overlap but it's cheap and
         I'm nervous */
      if (max_start < min_end)
        {
          if (max_start == extent_start (extent))
            extent_start (extent) = min_end;
          else
            extent_end (extent) = max_start;
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
void
process_extents_for_deletion (int from, int to, int start, int end,
			      struct buffer *buf)
{
  if (NILP (buf->extents))
    return;
  else if (!EXTENTP (buf->extents))
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

      if (buf->cached_stack
	  && (buf->cached_stack->buf_index >= start)
	  && (buf->cached_stack->buf_index < end))
	buf->cached_stack->buf_index = start;
      soe_prune (buf);
    }
}

void
process_extents_for_destruction (int from, int to, struct buffer *buf)
{
  if (NILP (buf->extents))
    return;
  else if (!EXTENTP (buf->extents))
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
  int start = (extent_index_to_buffer_pos (extent_start (extent), res_ptr->buf)
	       - res_ptr->from);
  int end = (extent_index_to_buffer_pos (extent_end (extent), res_ptr->buf)
	     - res_ptr->from);
  
  if (EXTENT_FLAG_P (extent, EF_DUPLICABLE))
    {
      start = max (start, 0);
      end = min (end, res_ptr->length);

      /* this test should probably never fail, but I'm a bit confused at the
	 moment */
      if ((start < end) || 
	  ((start == end) && (extent_start (extent) == extent_end (extent))))
	{
	  Lisp_Object new_cell;   
	  Lisp_Object replica;
	  DUP dup = make_extent_replica ();

	  XSETEXTENT (replica, dup);
	  XSETEXTENT (dup_extent (dup), extent);

	  dup_start (dup) = start;
	  dup_end (dup) = end;
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
  else if (!EXTENTP (buf->extents))
    abort();
  else
    {
      struct replicate_extents_struct res;
      res.from = opoint;
      res.length = length;
      res.head = Qnil;
      res.buf = buf;
      res.nconc_cell = Qzero;
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
  if ((!NILP(buf->extents) && (!EXTENTP (buf->extents))) || 
      (!NILP (dup_list) && (!CONSP (dup_list))))
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
          if (EXTENT_REPLICA_P (current_replica)) 
            {
              DUP dup = XDUP (current_replica);
              EXTENT extent = XEXTENT (dup_extent (dup));
              int new_start = base_start + dup_start (dup);
              int new_end = base_start + dup_end (dup);

              if (EXTENT_DESTROYED_P (extent))
                continue;

              /* paranoid testing which will go away eventually */
              if ((!EXTENTP (dup_extent (dup))) ||
                  (XBUFFER (extent_buffer (extent)) != buf))
                abort ();
              
              if (EXTENT_FLAGS (extent) & EF_DETACHED)
                {
                  int from = extent_index_to_buffer_pos (new_start, buf);
                  int to = extent_index_to_buffer_pos (new_end, buf);
                  update_extent_1 (extent, from, to, 1, buf);
                }
              else if
                ((extent_start (extent) >
		  extent_index_offset (base_end, 1, buf)) ||
                 (extent_end (extent) <
		  extent_index_offset (base_start, -1, buf)))
                error ("extent 0x%x is all fouled up wrt. dup 0x%x",
                       (int) extent, (int) dup);
              else
                {
                  /* this should be safe because if you delete some text
                     all of the extents that were effected stay in the
                     same order, so when you restore what was removed
                     they should still be in the correct order */
                  extent_start (extent) = min (new_start,
					       extent_start (extent));
                  extent_end (extent) = max (new_end, extent_end (extent));
		  soe_push (extent, buf);
                }
            }
        }
    }
  else
    {
      Lisp_Object tail;
      int base_start = buffer_pos_to_extent_index (opoint, buf);
/*      int base_end = buffer_pos_to_extent_index (opoint + length, buf); */

      for (tail = dup_list; !NILP (tail); tail = Fcdr (tail))
        {
          Lisp_Object current_replica = Fcar (tail);
          /* only process replicas at the moment */
          if (EXTENT_REPLICA_P (current_replica)) 
            {
              DUP dup = XDUP (current_replica);
              EXTENT extent = XEXTENT (dup_extent (dup));
              int new_start = base_start + dup_start (dup);
              int new_end = base_start + dup_end (dup);

              if (EXTENT_DESTROYED_P (extent))
                continue;

              /* paranoid testing which will go away eventually */
              if ((!EXTENTP (dup_extent (dup))))
                abort ();

	      /* Energize extents like toplevel-forms can only be pasted 
	       * in the buffer they come from.  This should be parametrized
	       * in the generic extent objects.  Right now just silently
	       * skip the extents if it's not from the same buffer.
	       * --Matthieu */
	      if (XBUFFER (extent_buffer (extent)) != buf)
		continue;

              if (EXTENT_FLAGS (extent) & EF_DETACHED)
                {
                  int from = extent_index_to_buffer_pos (new_start, buf);
                  int to = extent_index_to_buffer_pos (new_end, buf);
                  update_extent_1 (extent, from, to, 1, buf);
                }
              else if (extent_end (extent) < new_start - 1)
                continue;
              else if (extent_start (extent) > new_end + 1)
                continue;
              else
                {
                  int from = 
                    extent_index_to_buffer_pos 
                      (min (new_start, extent_start (extent)), buf);
                  int to = 
                    extent_index_to_buffer_pos 
                      (max (new_end, extent_end (extent)), buf);
                  update_extent_1 (extent, from, to, 1, buf);   
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
   
static void merge_replicas_concating_mf (const void *, void *, void *);
static void merge_replicas_pruning_mf   (const void *, void *, void *);

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
add_to_replicas_lists (c_hashtable table,
		       Lisp_Object dup_list,
		       int offset, int length)
{
  Lisp_Object tail;
  for (tail = dup_list; !NILP (tail); tail = Fcdr(tail))
    {
      Lisp_Object current_replica = Fcar (tail);
      if (EXTENT_REPLICA_P (current_replica)) 
        {
          DUP dup = XDUP (current_replica);
          EXTENT extent = XEXTENT (dup_extent (dup));
          Lisp_Object pre_existing_cell;
          Lisp_Object tmp;
          DUP new_dup;
	  const void *vval;

          if (EXTENT_DESTROYED_P (extent))
            continue;

          new_dup = make_extent_replica ();
          memcpy ((char *) new_dup, (char *) dup, sizeof (*dup));
          dup_start (new_dup) += offset;
          dup_end (new_dup) += offset;
   
          /* paranoid testing which will go away eventually */
          if (!EXTENTP (dup_extent (dup)))
            abort ();
              
          if (!gethash ((void *) extent, table, &vval))
            pre_existing_cell = Qnil;
	  else
	    VOID_TO_LISP (pre_existing_cell, vval);
   
	  XSETEXTENT (tmp, new_dup);

          tmp = Fcons (tmp, pre_existing_cell);
          puthash (extent, LISP_TO_VOID (tmp), table);
        }
    }
}

static void 
merge_replicas_concating_mf (const void *key, void *contents, 
                             void *arg)
{
  Lisp_Object extent_cell;
  Lisp_Object *cells_vec = (Lisp_Object *) arg;
  VOID_TO_LISP (extent_cell, contents);

  if (NILP (cells_vec[0]))
    cells_vec[0] = extent_cell;
  else
    nconc2 (cells_vec[1], extent_cell);

  cells_vec[1] = extent_cell;
  return;
}

static int 
mrp_pred (Lisp_Object x, Lisp_Object y, Lisp_Object dummy)
{
  DUP dup1 = XDUP(x);
  DUP dup2 = XDUP(y);

  if (dup_start (dup1) < dup_start (dup2))
    return 1;
  else if (dup_start (dup1) == dup_start (dup2))
    {
      if (dup_end (dup1) <= dup_end (dup2))
        return 1;
      else
        return -1;
    }
  return -1;
}
   
static void
merge_replicas_pruning_mf (const void *key, void *contents, 
                           void *arg)
{
  Lisp_Object dup_list;
  c_hashtable table = (c_hashtable) arg;
  VOID_TO_LISP (dup_list, contents);

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

        if (dup_start (tail_dup) <= dup_end (current_dup) - 1)
          {
            dup_end (current_dup) = max (dup_end (tail_dup),
					 dup_end (current_dup));
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
  puthash (key, LISP_TO_VOID (dup_list), table);
}

void
syms_of_extents () 
{
  defsymbol (&Qextentp, "extentp");
  defsymbol (&Qupdate_extent, "update-extent");

  defsymbol (&Qhighlight, "highlight");
  defsymbol (&Qunhighlight, "unhighlight");
  defsymbol (&Qwrite_protected, "write-protected");
  defsymbol (&Qwritable, "writable");
  defsymbol (&Qinvisible, "invisible");
  defsymbol (&Qvisible, "visible");
  defsymbol (&Qbegin_glyph, "begin-glyph");
  defsymbol (&Qend_glyph, "end-glyph");
  defsymbol (&Qmenu, "menu");
  defsymbol (&Qstart_open, "start-open");
  defsymbol (&Qend_open, "end-open");
  defsymbol (&Qcolumn, "column");
  defsymbol (&Qdetached, "detached");
  defsymbol (&Qwarn_modify, "warn-modify");
  defsymbol (&Qduplicable, "duplicable");
  defsymbol (&Qnon_duplicable, "non-duplicable");

  defsymbol (&Qoutside_margin, "outside-margin");
  defsymbol (&Qinside_margin, "inside-margin");
  defsymbol (&Qwhitespace, "whitespace");
  defsymbol (&Qtext, "text");

  defsubr (&Sextentp);

  defsubr(&Sextent_length);
  defsubr(&Sextent_start_position);
  defsubr(&Sextent_end_position);
  defsubr(&Sextent_buffer);
  defsubr(&Smap_extents);
  defsubr(&Shighlight_extent);
  defsubr(&Sforce_highlight_extent);
  defsubr(&Sextent_at);

  defsubr(&Smake_extent);
  defsubr(&Snext_extent);
  defsubr(&Sdelete_extent);
  defsubr(&Supdate_extent);
  defsubr(&Sset_extent_attribute);
  defsubr(&Sextent_attributes);
  defsubr(&Sset_extent_begin_glyph);
  defsubr(&Sset_extent_end_glyph);
  defsubr(&Sset_extent_layout);
  defsubr(&Sextent_layout);
  defsubr(&Sextent_data);
  defsubr(&Sset_extent_data);
  defsubr(&Sextent_priority);
  defsubr(&Sset_extent_priority);
#if 0
  defsubr(&Snext_e_extent);
  defsubr(&Sstack_of_extents);
#endif

  Ffset (intern ("set-extent-endpoints"), Qupdate_extent);

  staticpro (&Vlast_highlighted_extent);
  Vlast_highlighted_extent = Qnil;

  staticpro (&Vextent_fragment_buffer);
  Vextent_fragment_buffer = Qnil;

  init_extent_fragment ();
}


Lisp_Object Vthis_is_a_dead_extent_replica;

void
init_extents_once ()
{
  XSETEXTENT (Vthis_is_a_dead_extent_replica, make_extent ());
  staticpro (&Vthis_is_a_dead_extent_replica);
}
