/* Storage allocation and gc for GNU Emacs Lisp interpreter.
   Copyright (C) 1985, 1986, 1988, 1992, 1993, 1994
   Free Software Foundation, Inc.

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
#include "lisp.h"
#ifndef standalone
#include "buffer.h"
#include "extents.h"
#include "window.h"
#include "events.h"
#include "bytecode.h"
#include "elhash.h"
#include "blockio.h"
#ifdef HAVE_X_WINDOWS
#include "xterm.h"
#ifdef MULTI_SCREEN
#include "screen.h"
#endif	/* MULTI_SCREEN */
#endif	/* HAVE_X_WINDOWS */
#endif

#include <stdio.h>

/* Define this to see where all that space is going... */
#define PURESTAT

/* Define this to use malloc/free with no freelist for all datatypes,
   the hope being that some debugging tools may help detect
   freed memory references */
/* #define ALLOC_NO_POOLS */

#include "puresize.h"

#include "backtrace.h"

#define max(A,B) ((A) > (B) ? (A) : (B))

#define SLOT_OFFSET(type, slot_name) \
  ((unsigned) (((char *) (&(((type *)0)->slot_name))) - ((char *)0)))

#define ALIGN_SIZE(len,unit) \
  ((((len) + (unit) - 1) / (unit)) * (unit))

/* Number of bytes of consing done since the last gc */
int consing_since_gc;
#ifdef EMACS_BTL
extern void cadillac_record_backtrace ();
#define INCREMENT_CONS_COUNTER(size) \
  do { \
    int __sz__ = ((int) (size)); \
    consing_since_gc += __sz__; \
    cadillac_record_backtrace (2, __sz__); \
  } while (0)
#else
#define INCREMENT_CONS_COUNTER(size) (consing_since_gc += (size))
#endif

/* Number of bytes of consing since gc before another gc should be done. */
int gc_cons_threshold;

/* Nonzero during gc */
int gc_in_progress;

/* Number of times GC has happened at this level or below.
 * Level 0 is most volatile, contrary to usual convention.
 *  (Of course, there's only one level at present) */
int gc_generation_number[1];

/* Nonzero when calling the hooks in Energize-beta */
int gc_currently_forbidden;

/* Hooks. */
Lisp_Object Vpre_gc_hook, Qpre_gc_hook;
Lisp_Object Vpost_gc_hook, Qpost_gc_hook;

/* "Garbage collecting" */
Lisp_Object Vgc_message;
static CONST char gc_default_message[] = "Garbage collecting";

#ifndef VIRT_ADDR_VARIES
extern
#endif /* VIRT_ADDR_VARIES */
 int malloc_sbrk_used;

#ifndef VIRT_ADDR_VARIES
extern
#endif /* VIRT_ADDR_VARIES */
 int malloc_sbrk_unused;

/* Non-zero means defun should do purecopy on the function definition */
int purify_flag;

extern Lisp_Object pure[];/* moved to pure.c to speed incremental linking */

#define PUREBEG ((unsigned char *) pure)

/* Index in pure at which next pure object will be allocated. */
static long pureptr;

#define PURIFIED(ptr) \
   ((PNTR_COMPARISON_TYPE) ((ptr)) < (PNTR_COMPARISON_TYPE) (PUREBEG + PURESIZE) \
     && (PNTR_COMPARISON_TYPE) ((ptr)) >= (PNTR_COMPARISON_TYPE) PUREBEG)

/* Non-zero if pureptr > PURESIZE; accounts for excess purespace needs. */
static long pure_lossage;

int
purified (Lisp_Object obj)
{
  if (!POINTER_TYPE_P (XGCTYPE (obj)))
    return (0);
  return (PURIFIED (XPNTR (obj)));
}

static int
check_purespace (int size)
{
  if (pure_lossage)
    {
      pure_lossage += size;
      return (0);
    }
  else if (pureptr + size > PURESIZE)
    {
      fprintf (stderr, GETTEXT ("\nERROR:  Pure Lisp storage exhausted!\n\n"));
      pure_lossage = size;
      return (0);
    }
  else
    return (1);
}



#ifndef PURESTAT

#define bump_purestat(p,b) do {} while (0) /* Do nothing */

#else /* PURESTAT */

static int purecopying_for_bytecode;

static int pure_sizeof (Lisp_Object /*, int recurse */);

/* Keep statistics on how much of what is in purespace */
struct purestat
{
  int nobjects;
  int nbytes;
  CONST char *name;
};

#define FMH(s,n) static struct purestat s = { 0, 0, n }
FMH (purestat_cons, "cons cells:");
FMH (purestat_float, "float objects:");
FMH (purestat_string_pname, "symbol-name strings:");
FMH (purestat_bytecode, "function objects:");
FMH (purestat_string_bytecodes, "byte-code strings:");
FMH (purestat_vector_bytecode_constants, "byte-constant vectors:");
FMH (purestat_string_interactive, "interactive strings:");
#ifdef I18N3
FMH (purestat_string_domain, "domain strings:");
#endif
FMH (purestat_string_documentation, "documentation strings:");
FMH (purestat_string_other_function, "other function strings:");
FMH (purestat_vector_other, "other vectors:");
FMH (purestat_string_other, "other strings:");
FMH (purestat_string_all, "all strings:");
FMH (purestat_vector_all, "all vectors:");

static struct purestat *purestats[] =
  {
    &purestat_cons,
    &purestat_float,
    &purestat_string_pname,
    &purestat_bytecode,
    &purestat_string_bytecodes,
    &purestat_vector_bytecode_constants,
    &purestat_string_interactive,
#ifdef I18N3
    &purestat_string_domain,
#endif
    &purestat_string_documentation,
    &purestat_string_other_function,
    &purestat_vector_other,
    &purestat_string_other,
    0,
    &purestat_string_all,
    &purestat_vector_all
  };
#undef FMH

static void
bump_purestat (struct purestat *purestat, int nbytes)
{
  if (pure_lossage) return;
  purestat->nobjects += 1;
  purestat->nbytes += nbytes;
}
#endif /* PURESTAT */


/* Maximum amount of C stack to save when a GC happens.  */

#ifndef MAX_SAVE_STACK
#define MAX_SAVE_STACK 16000
#endif

/* Buffer in which we save a copy of the C stack at each GC.  */

static char *stack_copy;
static int stack_copy_size;

/* Non-zero means ignore malloc warnings.  Set during initialization.  */
int ignore_malloc_warnings;

static void mark_object (Lisp_Object obj);

static void *breathing_space;

void
release_breathing_space ()
{
  if (breathing_space) 
  {
    void *tmp = breathing_space;
    breathing_space = 0;
    xfree (tmp);
  }
}


static Lisp_Object
malloc_warning_1 (Lisp_Object str)
{
  Fprinc (str, Vstandard_output);
  write_string_1 (GETTEXT (
     "\nKilling some buffers may delay running out of memory.\n"
     "However, certainly by the time you receive the 95% warning,\n"
     "you should clean up, kill this Emacs, and start a new one."),
		  -1, Vstandard_output);
  return Qnil;
}

/* malloc calls this if it finds we are near exhausting storage */
void
malloc_warning (CONST char *str)
{
  Lisp_Object val;

  if (ignore_malloc_warnings)
    return;

  val = build_string (str);
  internal_with_output_to_temp_buffer (GETTEXT (" *Danger*"), malloc_warning_1,
				       val, Qnil);
}


/* Called if malloc returns zero */
void
memory_full ()
{
  /* Force a GC next time eval is called.
     It's better to loop garbage-collecting (we might reclaim enough
     to win) than to loop beeping and barfing "Memory exhausted"
   */
  consing_since_gc = gc_cons_threshold + 1;
  release_breathing_space ();

#ifndef standalone
  /* Flush some histories which might conceivably contain
   *  garbalogical inhibitors */
  if (!NILP (Fboundp (Qvalues)))
    Fset (Qvalues, Qnil);
  Vcommand_history = Qnil;
#endif

  error (GETTEXT ("Memory exhausted"));
}

/* like malloc and realloc but check for no memory left, and block input. */

void *
xmalloc (int size)
{
  void *val;

  /* It is necessary to block SIGIO interrupts around calls to malloc()
     because the SIGIO handler interrogates the X queue to see if a
     control-g keypress event is pending.  It does this by calling
     XCheckIfEvent(), which can call malloc() in order to expand its
     internal buffers.  As malloc is not reentrant, this can corrupt
     the malloc lists.

     This is generally only a problem within the first few seconds after
     emacs has started up, because the X event buffers tend to reach a
     stable size fairly early, but it is a *potential* problem at all
     times.
   */
  BLOCK_INPUT;
  val = (void *) malloc (size);
  UNBLOCK_INPUT;

  if (!val && (size != 0)) memory_full ();
  return val;
}

void *
xrealloc (void *block, int size)
{
  void *val;

  BLOCK_INPUT;	/* see comment above */
  /* We must call malloc explicitly when BLOCK is 0, since some
     reallocs don't do this.  */
  if (! block)
    val = (void *) malloc (size);
  else
    val = (void *) realloc (block, size);
  UNBLOCK_INPUT;

  if (!val && (size != 0)) memory_full ();
  return val;
}

void
xfree (void *block)
{
  BLOCK_INPUT;
  free (block);
  UNBLOCK_INPUT;
}

char *
xstrdup (CONST char *str)
{
  char *val;
  int len = strlen (str) + 1;   /* for stupid terminating 0 */

  val = xmalloc (len);
  if (val == 0) return 0;
  memcpy (val, str, len);
  return (val);
}

#ifdef NEED_STRDUP
char *
strdup (CONST char *s)
{
  return xstrdup (s);
}
#endif /* NEED_STRDUP */


#if 1 /* lemacs doesn't malloc inside signal handlers */

void
uninterrupt_malloc (void)
{
  return;
}
#else /* unused */

/* Arranging to disable input signals while we're in malloc.

   This only works with GNU malloc.  To help out systems which can't
   use GNU malloc, all the calls to malloc, realloc, and free
   elsewhere in the code should be inside a BLOCK_INPUT/UNBLOCK_INPUT
   pairs; unfortunately, we have no idea what C library functions
   might call malloc, so we can't really protect them unless you're
   using GNU malloc.  Fortunately, most of the major operating can use
   GNU malloc.

   Really this should be changed to abort() if malloc is ever called 
   reentrantly, so that we can track down and BLOCK_INPUT around all
   library routines which malloc, as we do for the X routines.  
 */

#ifndef SYSTEM_MALLOC
extern void * (*__malloc_hook) ();
static void * (*old_malloc_hook) ();
extern void * (*__realloc_hook) ();
static void * (*old_realloc_hook) ();
extern void (*__free_hook) ();
static void (*old_free_hook) ();

static void
emacs_blocked_free (ptr)
     void *ptr;
{
  BLOCK_INPUT;
  __free_hook = old_free_hook;
  free (ptr);
  __free_hook = emacs_blocked_free;
  UNBLOCK_INPUT;
}

static void *
emacs_blocked_malloc (size)
     unsigned size;
{
  void *value;

  BLOCK_INPUT;
  __malloc_hook = old_malloc_hook;
  value = malloc (size);
  __malloc_hook = emacs_blocked_malloc;
  UNBLOCK_INPUT;

  return value;
}

static void *
emacs_blocked_realloc (ptr, size)
     void *ptr;
     unsigned size;
{
  void *value;

  BLOCK_INPUT;
  __realloc_hook = old_realloc_hook;
  value = realloc (ptr, size);
  __realloc_hook = emacs_blocked_realloc;
  UNBLOCK_INPUT;

  return value;
}

void
uninterrupt_malloc ()
{
  old_free_hook = __free_hook;
  __free_hook = emacs_blocked_free;

  old_malloc_hook = __malloc_hook;
  __malloc_hook = emacs_blocked_malloc;

  old_realloc_hook = __realloc_hook;
  __realloc_hook = emacs_blocked_realloc;
}
#endif /* not SYSTEM_MALLOC */

#endif /* Unused */


static void *
allocate_lisp_storage (int size)
{
  void *p = xmalloc (size);
  char *lim = ((char *) p) + size;
  Lisp_Object val;

  XSET (val, Lisp_Cons, lim);
  if ((char *) XCONS (val) != lim)
  {
    xfree (p);
    memory_full ();
  }
  return (p);
}


#define MARKED_RECORD_HEADER_P(lheader) \
  (((lheader)->implementation->finaliser) == this_marks_a_marked_record)
#define MARK_RECORD_HEADER(lheader) \
  do { (((lheader)->implementation)++); } while (0)
#define UNMARK_RECORD_HEADER(lheader) \
  do { (((lheader)->implementation)--); } while (0)


/* lrecords are chained together through their "next.v" field.
 * After doing the mark phase, the GC will walk this linked
 *  list and free any record which hasn't been marked 
 */
static struct lcrecord_header *all_lcrecords;

void *
alloc_lcrecord (int size, CONST struct lrecord_implementation *implementation)
{
  struct lcrecord_header *lcheader;

  if (size <= 0) abort ();
  if (implementation->static_size == 0)
    {
      if (!implementation->size_in_bytes_method)
	abort ();
    }
  else if (implementation->static_size != size)
    abort ();

  lcheader = allocate_lisp_storage (size);
  lcheader->lheader.implementation = implementation;
  lcheader->next = all_lcrecords;
  all_lcrecords = lcheader;
  INCREMENT_CONS_COUNTER (size);
  return (lcheader);
}

#if 0 /* Presently unused */
/* Very, very poor man's EGC?
 * This may be slow and thrash pages all over the place.
 *  Only call it if you really feel you must (and if the
 *  lrecord was fairly recently allocated.)
 * Otherwise, just let the GC do its job -- that's what it's there for
 */
void
free_lcrecord (struct lcrecord_header *lcrecord)
{
  if (all_lcrecords == lcrecord)
  {
    all_lcrecords = lcrecord->next;
  }
  else
  {
    struct lrecord_header *header = all_lcrecords;
    for (;;)
    {
      struct lrecord_header *next = header->next;
      if (next == lcrecord)
      {
        header->next = lrecord->next;
        break;
      }
      else if (next == 0)
        abort ();
      else
        header = next;
    }
  }
  if (lrecord->implementation->finaliser)
    ((lrecord->implementation->finaliser) (lrecord, 0));
  xfree (lrecord);
  return;
}
#endif /* Unused */


static void
disksave_object_finalisation_1 ()
{
  struct lcrecord_header *header;
  for (header = all_lcrecords; header; header = header->next)
    {
      if (header->lheader.implementation->finaliser)
	((header->lheader.implementation->finaliser) (header, 1));
    }
}
  

/* This must not be called -- it just serves as for EQ test
 *  If lheader->implementation->finaliser is this_marks_a_marked_record,
 *  then lrecord has been marked by the GC sweeper
 * header->implementation is put back to its correct value by
 *  sweep_records */
void
this_marks_a_marked_record (void *dummy0, int dummy1)
{
  abort ();
}

/* XGCTYPE for records */
int
gc_record_type_p (Lisp_Object frob, CONST struct lrecord_implementation *type)
{
  return (XGCTYPE (frob) == Lisp_Record
          && (XRECORD_LHEADER (frob)->implementation == type
              || XRECORD_LHEADER (frob)->implementation == type + 1));
}

#define ALLOCATE_FROB_FROM_BLOCK(type, result) \
  do { \
    if (current_##type##_block_index \
	== countof (current_##type##_block->block)) \
    { \
      struct type##_block *__new__ \
         = allocate_lisp_storage (sizeof (struct type##_block)); \
      __new__->prev = current_##type##_block; \
      current_##type##_block = __new__; \
      current_##type##_block_index = 0; \
    } \
    (result) = &(current_##type##_block->block[current_##type##_block_index++]); \
  } while (0)



/**********************************************************************/
/* Cons allocation                                                    */
/**********************************************************************/

/* Allocation of cons cells */
/* We store cons cells inside of cons_blocks, allocating a new
   cons_block with malloc whenever necessary.  Cons cells reclaimed by
   GC are put on a free list to be reallocated before allocating
   any new cons cells from the latest cons_block.

   Each cons_block is just under 2^n - MALLOC_OVERHEAD bytes long,
   since malloc really allocates in units of powers of two
   and uses 4 bytes for its own overhead. */

#ifndef MALLOC_OVERHEAD
#define MALLOC_OVERHEAD 4
#endif

#define CONS_BLOCK_SIZE \
    ((2048 - MALLOC_OVERHEAD - sizeof (struct cons_block *)) \
     / sizeof (struct Lisp_Cons))

#ifdef ALLOC_NO_POOLS
# undef CONS_BLOCK_SIZE
# define CONS_BLOCK_SIZE 1
#endif

struct cons_block
  {
    struct cons_block *prev;
    struct Lisp_Cons block[CONS_BLOCK_SIZE];
  };

static struct cons_block *current_cons_block;
static int current_cons_block_index;

/* 0 or a Lisp_Cons, chained through cons->car */
static struct Lisp_Cons *cons_free_list;

static void
init_cons_alloc ()
{
  current_cons_block = 0;
  current_cons_block_index = countof (current_cons_block->block);
  cons_free_list = 0;
}

/* Explicitly free a cons cell.  */
void
free_cons (struct Lisp_Cons *ptr)
{
#ifndef ALLOC_NO_POOLS
  Lisp_Object *chain = &(ptr->car);
  if (cons_free_list)
    XSET (*chain, Lisp_Cons, cons_free_list);
  else
    *chain = Qzero;
  cons_free_list = ptr;
#endif /* ALLOC_NO_POOLS */
}

DEFUN ("cons", Fcons, Scons, 2, 2, 0,
  "Create a new cons, give it CAR and CDR as components, and return it.")
  (car, cdr)
     Lisp_Object car, cdr;
{
  Lisp_Object val;

  if (cons_free_list)
    {
      Lisp_Object chain = cons_free_list->car;
      XSET (val, Lisp_Cons, cons_free_list);
      cons_free_list = ((CONSP (chain)) ? XCONS (chain) : 0);
    }
  else
    {
      struct Lisp_Cons *ptr;
      ALLOCATE_FROB_FROM_BLOCK (cons, ptr);
      XSET (val, Lisp_Cons, ptr);
    }
  XCONS (val)->car = car;
  XCONS (val)->cdr = cdr;
  INCREMENT_CONS_COUNTER (sizeof (struct Lisp_Cons));
  return val;
}

DEFUN ("list", Flist, Slist, 0, MANY, 0,
  "Return a newly created list with specified arguments as elements.\n\
Any number of arguments, even zero arguments, are allowed.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  Lisp_Object len, val, val_tail;

  len = make_number (nargs);
  val = Fmake_list (len, Qnil);
  val_tail = val;
  while (!NILP (val_tail))
    {
      XCONS (val_tail)->car = *args++;
      val_tail = XCONS (val_tail)->cdr;
    }
  return val;
}

Lisp_Object
list1 (Lisp_Object obj0)
{
  return (Fcons (obj0, Qnil));
}

Lisp_Object
list2 (Lisp_Object obj0, Lisp_Object obj1)
{
  return Fcons (obj0, list1 (obj1));
}

Lisp_Object
list3 (Lisp_Object obj0, Lisp_Object obj1, Lisp_Object obj2)
{
  return Fcons (obj0, list2 (obj1, obj2));
}

Lisp_Object
cons3 (Lisp_Object obj0, Lisp_Object obj1, Lisp_Object obj2)
{
  return Fcons (obj0, Fcons (obj1, obj2));
}

Lisp_Object
list4 (Lisp_Object obj0, Lisp_Object obj1, Lisp_Object obj2, Lisp_Object obj3)
{
  return Fcons (obj0, list3 (obj1, obj2, obj3));
}

Lisp_Object
list5 (Lisp_Object obj0, Lisp_Object obj1, Lisp_Object obj2, Lisp_Object obj3,
       Lisp_Object obj4)
{
  return Fcons (obj0, list4 (obj1, obj2, obj3, obj4));
}

DEFUN ("make-list", Fmake_list, Smake_list, 2, 2, 0,
  "Return a newly created list of length LENGTH, with each element being INIT.")
     (length, init)
     Lisp_Object length, init;
{
  Lisp_Object val;
  int size;

  if (!FIXNUMP (length) || XINT (length) < 0)
    length = wrong_type_argument (Qnatnump, length);
  size = XINT (length);

  val = Qnil;
  while (size-- > 0)
    val = Fcons (init, val);
  return val;
}

/**********************************************************************/
/* Float allocation                                                   */
/**********************************************************************/


#ifdef LISP_FLOAT_TYPE
/* Allocation of float cells, just like conses */
/* We store float cells inside of float_blocks, allocating a new
   float_block with malloc whenever necessary.  Float cells reclaimed by
   GC are put on a free list to be reallocated before allocating
   any new float cells from the latest float_block. */

#define FLOAT_BLOCK_SIZE \
    ((2048 - MALLOC_OVERHEAD - sizeof(struct float_block *)) \
     / sizeof (struct Lisp_Float))

#ifdef ALLOC_NO_POOLS
# undef FLOAT_BLOCK_SIZE
# define FLOAT_BLOCK_SIZE 1
#endif

struct float_block
  {
    struct float_block *prev;
    struct Lisp_Float block[FLOAT_BLOCK_SIZE];
  };

static struct float_block *current_float_block;
static int current_float_block_index;

/* 0 or a Lisp_Float, chained through float_next */
static struct Lisp_Float *float_free_list;

static void
init_float_alloc ()
{
  current_float_block = 0;
  current_float_block_index = countof (current_float_block->block);
  float_free_list = 0;
}

Lisp_Object
make_float (double float_value)
{
  Lisp_Object val;
  struct Lisp_Float *f;

  if (float_free_list)
    {
      f = float_free_list;
      float_free_list = float_next (f);
    }
  else
    {
      ALLOCATE_FROB_FROM_BLOCK (float, f);
      f->lheader.implementation = lrecord_float;
    }
  float_next (f) = ((struct Lisp_Float *) -1);
  float_data (f) = float_value;
  INCREMENT_CONS_COUNTER (sizeof (struct Lisp_Float));
  XSETFLOAT (val, f);
  return (val);
}

#endif /* LISP_FLOAT_TYPE */

/**********************************************************************/
/* Vector allocation                                                  */
/**********************************************************************/


static Lisp_Object all_vectors;

/* >>> This should allocate `small' vectors from a frob-block */
static struct Lisp_Vector *
make_vector_internal (int sizei)
{
  int sizem = (sizeof (struct Lisp_Vector)
               /* -1 because struct Lisp_Vector includes 1 slot,
                * +1 to account for vector_next */
               + (sizei - 1 + 1) * sizeof (Lisp_Object)
               );
  struct Lisp_Vector *p = allocate_lisp_storage (sizem);
#ifdef LRECORD_VECTOR
  set_lheader_implementation (&(p->lheader), lrecord_vector);
#endif

  INCREMENT_CONS_COUNTER (sizem);

  p->size = sizei;
  vector_next (p) = all_vectors;
  XSET (all_vectors, Lisp_Vector, p);
  return (p);
}



Lisp_Object
make_vector (int length, Lisp_Object init)
{
  int index;
  Lisp_Object vector;
  struct Lisp_Vector *p;

  if (length < 0)
    length = XINT (wrong_type_argument (Qnatnump, make_number (length)));

  p = make_vector_internal (length);
  XSET (vector, Lisp_Vector, p);

#if 0
  /* Initialise big arrays full of 0's quickly, for what that's worth */
  {
    char *travesty = (char *) &init;
    for (i = 1; i < sizeof (Lisp_Object); i++)
    {
      if (travesty[i] != travesty[0])
        goto fill;
    }
    memset (p->contents, travesty[0], length * sizeof (Lisp_Object));
    return (vector);
  }
 fill:
#endif
  for (index = 0; index < length; index++)
    p->contents[index] = init;

  return (vector);
}


DEFUN ("make-vector", Fmake_vector, Smake_vector, 2, 2, 0,
  "Return a newly created vector of length LENGTH, with each element being INIT.\n\
See also the function `vector'.")
     (length, init)
     Lisp_Object length, init;
{
  if (!FIXNUMP (length) || XINT (length) < 0)
    length = wrong_type_argument (Qnatnump, length);

  return (make_vector (XINT (length), init));
}

DEFUN ("vector", Fvector, Svector, 0, MANY, 0,
  "Return a newly created vector with specified arguments as elements.\n\
Any number of arguments, even zero arguments, are allowed.")
     (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  Lisp_Object vector;
  int index;
  struct Lisp_Vector *p;

  p = make_vector_internal (nargs);
  XSET (vector, Lisp_Vector, p);

  for (index = 0; index < nargs; index++)
    p->contents[index] = args[index];

  return (vector);
}


/**********************************************************************/
/* Bytecode (aka "compiled-function") allocation                      */
/**********************************************************************/

#define BYTECODE_BLOCK_SIZE \
    ((2048 - MALLOC_OVERHEAD - sizeof (struct bytecode_block *)) \
     / sizeof (struct Lisp_Bytecode))

#ifdef ALLOC_NO_POOLS
# undef BYTECODE_BLOCK_SIZE
# define BYTECODE_BLOCK_SIZE 1
#endif

struct bytecode_block
  {
    struct bytecode_block *prev;
    struct Lisp_Bytecode block[BYTECODE_BLOCK_SIZE];
  };

static struct bytecode_block *current_bytecode_block;
static int current_bytecode_block_index;

/* 0 or a Lisp_Bytecode, chained through bytecode->bytecodes */
static struct Lisp_Bytecode *bytecode_free_list;

static void
init_bytecode_alloc ()
{
  current_bytecode_block = 0;
  current_bytecode_block_index = countof (current_bytecode_block->block);
  bytecode_free_list = 0;
}


static Lisp_Object
make_bytecode (int make_pure)
{
  struct Lisp_Bytecode *b;
  Lisp_Object new;
  int size = sizeof (struct Lisp_Bytecode);

  if (make_pure && check_purespace (size))
    {
      b = (struct Lisp_Bytecode *) (PUREBEG + pureptr);
      set_lheader_implementation (&(b->lheader), lrecord_bytecode);
      pureptr += size;
      bump_purestat (&purestat_bytecode, size);
    }
  else
    {
      if (bytecode_free_list)
      {
        Lisp_Object chain = bytecode_free_list->bytecodes;
        b = bytecode_free_list;
        bytecode_free_list = ((COMPILEDP (chain)) ? XBYTECODE (chain) : 0);
      }
      else
      {
        ALLOCATE_FROB_FROM_BLOCK (bytecode, b);
        set_lheader_implementation (&(b->lheader), lrecord_bytecode);
      }
      INCREMENT_CONS_COUNTER (size);
    }
  b->maxdepth = 0;
  b->flags.documentationp = 0;
  b->flags.interactivep = 0;
  b->flags.domainp = 0; /* I18N3 */
  b->bytecodes = Qzero;
  b->constants = Qzero;
  b->arglist = Qnil;
  b->doc_and_interactive = Qnil;
  XSETR (new, Lisp_Compiled, b);
  return (new);
}


DEFUN ("make-byte-code", Fmake_byte_code, Smake_byte_code, 4, MANY, 0,
  "Create a compiled-function object.\n\
Usage: (arglist instructions constants stack-size\n\
	&optional doc-string interactive-spec)\n\
Note that, unlike all other emacs-lisp functions, calling this with five\n\
arguments is NOT the same as calling it with six arguments, the last of\n\
which is nil.  If the INTERACTIVE arg is specified as nil, then that means\n\
that this function was defined with `(interactive)'.  If the arg is not\n\
specified, then that means the function is not interactive.\n\
This is terrible behavior which is retained for compatibility with old\n\
`.elc' files which expected these semantics.")
     (nargs, args)
     int nargs;
     Lisp_Object *args;
{
/*   In a non-insane world this function would have this arglist...
     (arglist, instructions, constants, stack_size, doc_string, interactive)
     Lisp_Object arglist, instructions, constants, stack_size, doc_string,
	interactive;
 */
  Lisp_Object arglist      = args[0];
  Lisp_Object instructions = args[1];
  Lisp_Object constants    = args[2];
  Lisp_Object stack_size   = args[3];
  Lisp_Object doc_string   = ((nargs > 4) ? args[4] : Qnil);
  Lisp_Object interactive  = ((nargs > 5) ? args[5] : Qunbound);

  if (nargs > 6)
    return Fsignal (Qwrong_number_of_arguments, 
		    list2 (intern ("make-byte-code"), make_number (nargs)));

  CHECK_LIST (arglist, 0);
  CHECK_STRING (instructions, 0);
  CHECK_VECTOR (constants, 0);
  CHECK_NATNUM (stack_size, 0);
  /* doc_string may be nil, string, or int. */
  /* interactive may be list or string (or unbound). */

  if (purify_flag)
    {
      if (!purified (arglist))
	arglist = Fpurecopy (arglist);
      if (!purified (instructions))
	instructions = Fpurecopy (instructions);
      if (!purified (doc_string))
	doc_string = Fpurecopy (doc_string);
      if (!purified (interactive) && !EQ (interactive, Qunbound))
	interactive = Fpurecopy (interactive);

      /* Statistics are kept differently for the constants */
      if (!purified (constants))
#ifdef PURESTAT
	{
	  int old = purecopying_for_bytecode;
	  purecopying_for_bytecode = 1;
	  constants = Fpurecopy (constants);
	  purecopying_for_bytecode = old;
	}
#else
        constants = Fpurecopy (constants);
#endif /* PURESTAT */

#ifdef PURESTAT
      if (STRINGP (instructions))
	bump_purestat (&purestat_string_bytecodes, pure_sizeof (instructions));
      if (VECTORP (constants))
	bump_purestat (&purestat_vector_bytecode_constants,
		       pure_sizeof (constants));
      if (STRINGP (doc_string))
	/* These should be have been snagged by make-docfile... */
	bump_purestat (&purestat_string_documentation,
		       pure_sizeof (doc_string));
      if (STRINGP (interactive))
	bump_purestat (&purestat_string_interactive,
		       pure_sizeof (interactive));
#endif /* PURESTAT */
    }
  
  {
    int docp = !NILP (doc_string);
    int intp = !EQ (interactive, Qunbound);
#ifdef I18N3
    int domp = !NILP (Vfile_domain);
#endif
    Lisp_Object val = make_bytecode (purify_flag);
    struct Lisp_Bytecode *b = XBYTECODE (val);
    b->flags.documentationp = docp;
    b->flags.interactivep   = intp;
#ifdef I18N3
    b->flags.domainp        = domp;
#endif
    b->maxdepth  = XINT (stack_size);
    b->bytecodes = instructions;
    b->constants = constants;
    b->arglist   = arglist;

#ifdef I18N3
    if (docp && intp && domp)
      b->doc_and_interactive = (((purify_flag) ? pure_cons : Fcons)
				(doc_string,
				 (((purify_flag) ? pure_cons : Fcons)
				  (interactive, Vfile_domain))));
    else if (docp && domp)
      b->doc_and_interactive = (((purify_flag) ? pure_cons : Fcons)
				(doc_string, Vfile_domain));
    else if (intp && domp)
      b->doc_and_interactive = (((purify_flag) ? pure_cons : Fcons)
				(interactive, Vfile_domain));
    else
#endif
    if (docp && intp)
      b->doc_and_interactive = (((purify_flag) ? pure_cons : Fcons)
				(doc_string, interactive));
    else if (intp)
      b->doc_and_interactive = interactive;
#ifdef I18N3
    else if (domp)
      b->doc_and_interactive = Vfile_domain;
#endif
    else
      b->doc_and_interactive = doc_string;

    return (val);
  }
}



/**********************************************************************/
/* Symbol allocation                                                  */
/**********************************************************************/

/* Allocation of symbols.
   Just like allocation of conses. */

#define SYMBOL_BLOCK_SIZE \
    ((2048 - MALLOC_OVERHEAD - sizeof (struct symbol_block *)) \
     / sizeof (struct Lisp_Symbol))

#ifdef ALLOC_NO_POOLS
# undef SYMBOL_BLOCK_SIZE
# define SYMBOL_BLOCK_SIZE 1
#endif

struct symbol_block
  {
    struct symbol_block *prev;
    struct Lisp_Symbol block[SYMBOL_BLOCK_SIZE];
  };

static struct symbol_block *current_symbol_block;
static int current_symbol_block_index;

/* Chained through symbol_next (symbol) */
static struct Lisp_Symbol *symbol_free_list;

static void
init_symbol_alloc ()
{
  current_symbol_block = 0;
  current_symbol_block_index = countof (current_symbol_block->block);
  symbol_free_list = 0;
}

DEFUN ("make-symbol", Fmake_symbol, Smake_symbol, 1, 1, 0,
  "Return a newly allocated uninterned symbol whose name is NAME.\n\
Its value and function definition are void, and its property list is nil.")
  (str)
     Lisp_Object str;
{
  Lisp_Object val;
  struct Lisp_Symbol *p;

  CHECK_STRING (str, 0);

  if (symbol_free_list)
    {
      p = symbol_free_list;
      symbol_free_list = symbol_next (p);
    }
  else
    {
      ALLOCATE_FROB_FROM_BLOCK (symbol, p);
#ifdef LRECORD_SYMBOL
  set_lheader_implementation (&(p->lheader), lrecord_symbol);
#endif
    }
  p->name = XSTRING (str);
  p->plist = Qnil;
  p->value = Qunbound;
  p->function = Qunbound;
  symbol_next (p) = 0;
  INCREMENT_CONS_COUNTER (sizeof (struct Lisp_Symbol));
  XSETSYMBOL (val, p);
  return val;
}

/**********************************************************************/
/* Extent and extent_replica allocation                               */
/**********************************************************************/

/* Just like allocation of conses */

#define EXTENT_BLOCK_SIZE \
    ((2048 - MALLOC_OVERHEAD - sizeof (struct extent_block *)) \
     / sizeof (struct extent))
#define EXTENT_REPLICA_BLOCK_SIZE \
    ((2048 - MALLOC_OVERHEAD - sizeof (struct extent_replica_block *)) \
     / sizeof (struct extent_replica))

#ifdef ALLOC_NO_POOLS
# undef EXTENT_BLOCK_SIZE
# define EXTENT_BLOCK_SIZE 1
# undef EXTENT_REPLICA_BLOCK_SIZE
# define EXTENT_REPLICA_BLOCK_SIZE 1
#endif

struct extent_block
  {
    struct extent_block *prev;
    struct extent block[EXTENT_BLOCK_SIZE];
  };

struct extent_replica_block
  {
    struct extent_replica_block *prev;
    struct extent_replica block[EXTENT_REPLICA_BLOCK_SIZE];
  };

static struct extent_block *current_extent_block;
static int current_extent_block_index;

/* chained through extent->next */
static struct extent *extent_free_list;

static struct extent_replica_block *current_extent_replica_block;
static int current_extent_replica_block_index;

/* 0 or a Lisp_Extent_Replica, chained through dup->extent */
static struct extent_replica *extent_replica_free_list;

static void
init_extent_alloc ()
{
  current_extent_block = 0;
  current_extent_block_index = countof (current_extent_block->block);
  extent_free_list = 0;

  current_extent_replica_block = 0;
  current_extent_replica_block_index
    = countof (current_extent_replica_block->block);
  extent_replica_free_list = 0;
}

struct extent *
make_extent ()
{
  struct extent *e;

  if (extent_free_list)
    {
      e = extent_free_list;
      extent_free_list = extent_free_list->next;
    }
  else
    {
      ALLOCATE_FROB_FROM_BLOCK (extent, e);
      /* memset (e, 0, sizeof (struct extent)); */
    }
  set_lheader_implementation (&(e->lheader), lrecord_extent);
  extent_buffer (e) = Qzero;
  extent_start (e) = 0;
  extent_end (e) = 0;
  e->next = 0;
  e->previous = 0;
  e->e_next = 0;
  e->e_previous = 0;
  e->plist = Qnil;

  memset (&e->flags, 0, sizeof (e->flags));

  e->flags.end_open = 1;  /* default is for endpoints to behave like markers */

  INCREMENT_CONS_COUNTER (sizeof (struct extent));

  return (e);
}

struct extent_replica *
make_extent_replica (Lisp_Object extent, int start, int end)
{
  struct extent_replica *r;

  if (extent_replica_free_list)
    {
      Lisp_Object chain = dup_extent (extent_replica_free_list);
      r = extent_replica_free_list;
      extent_replica_free_list = ((EXTENT_REPLICA_P (chain)) 
                                  ? XDUP (chain) : 0);
    }
  else
    {
      ALLOCATE_FROB_FROM_BLOCK (extent_replica, r);
      /* memset (r, 0, sizeof (struct extent_replica)); */
    }
  set_lheader_implementation (&(r->lheader), lrecord_extent_replica);
  dup_extent (r) = extent;
  dup_start (r) = (start >= 0) ? start : 0;
  dup_end (r) = (end >= start) ? end : start;

  INCREMENT_CONS_COUNTER (sizeof (struct extent_replica));

  return (r);
}


/**********************************************************************/
/* Event allocation                                                   */
/**********************************************************************/

#define EVENT_BLOCK_SIZE \
    ((2048 - MALLOC_OVERHEAD - sizeof (struct event_block *)) \
     / sizeof (struct Lisp_Event))

#ifdef ALLOC_NO_POOLS
# undef EVENT_BLOCK_SIZE
# define EVENT_BLOCK_SIZE 1
#endif

struct event_block 
  {
    struct event_block *prev;
    struct Lisp_Event block[EVENT_BLOCK_SIZE];
  };

static struct event_block *current_event_block;
static int current_event_block_index;
static struct Lisp_Event *event_free_list;


Lisp_Object
make_event ()
{
  Lisp_Object val;
  struct Lisp_Event *e;

  if (event_free_list)
    {
      e = event_free_list;
      event_free_list = event_next (e);
    }
  else
    {
      ALLOCATE_FROB_FROM_BLOCK (event, e);
      set_lheader_implementation (&(e->lheader), lrecord_event);
    }
  INCREMENT_CONS_COUNTER (sizeof (struct Lisp_Event));

  XSETR (val, Lisp_Event, e);
  return val;
}
  

static void
init_event_alloc ()
{
  current_event_block = 0;
  current_event_block_index = countof (current_event_block->block);
  event_free_list = 0;
}



/**********************************************************************/
/* Marker allocation                                                  */
/**********************************************************************/

/* Allocation of markers.
   Works like allocation of conses. */

#define MARKER_BLOCK_SIZE \
    ((2048 - MALLOC_OVERHEAD - sizeof (struct marker_block *)) \
     / sizeof (struct Lisp_Marker))
#ifdef ALLOC_NO_POOLS
# undef MARKER_BLOCK_SIZE
# define MARKER_BLOCK_SIZE 1
#endif

struct marker_block
  {
    struct marker_block *prev;
    struct Lisp_Marker block[MARKER_BLOCK_SIZE];
  };

static struct marker_block *current_marker_block;
static int current_marker_block_index;


/* Chained through marker_next () */
static struct Lisp_Marker *marker_free_list;

static void
init_marker_alloc ()
{
  current_marker_block = 0;
  current_marker_block_index = countof (current_marker_block->block);
  marker_free_list = 0;
}

DEFUN ("make-marker", Fmake_marker, Smake_marker, 0, 0, 0,
  "Return a newly allocated marker which does not point at any place.")
  ()
{
  Lisp_Object val;
  struct Lisp_Marker *p;

  if (marker_free_list)
    {
      p = marker_free_list;
      marker_free_list = marker_next (p);
    }
  else
    {
      ALLOCATE_FROB_FROM_BLOCK (marker, p);
      set_lheader_implementation (&(p->lheader), lrecord_marker);
    }
  p->buffer = 0;
  p->bufpos = 0;
  marker_next (p) = 0;
  INCREMENT_CONS_COUNTER (sizeof (struct Lisp_Marker));
  XSETR (val, Lisp_Marker, p);
  return val;
}

/**********************************************************************/
/* String allocation                                                  */
/**********************************************************************/

/* The data for "short" strings generally resides inside of structs of type 
   string_chars_block. The Lisp_String structure is allocated just like any 
   other Lisp object (except for vectors), and these are freelisted when
   they get garbage collected. The data for short strings get compacted,
   but the data for large strings do not. 

   Previously Lisp_String structures were relocated, but this caused a lot
   of bus-errors because the C code didn't include enough GCPRO's for
   strings (since EVERY REFERENCE to a short string needed to be GCPRO'd so
   that the reference would get relocated).

   This new method makes things somewhat bigger, but it is MUCH safer.  */

#define STRING_BLOCK_SIZE \
    ((2048 - MALLOC_OVERHEAD - sizeof (struct string_block *)) \
     / sizeof (struct Lisp_String))

#ifdef ALLOC_NO_POOLS
# undef STRING_BLOCK_SIZE
# define STRING_BLOCK_SIZE 1
#endif

struct string_block
{
  struct string_block *prev;
  struct Lisp_String block[STRING_BLOCK_SIZE];
};

struct string_block *current_string_block;
static int current_string_block_index;
/* 0 or a Lisp_String.  Chained thought string->dup_list */
static struct Lisp_String *string_free_list;


/* String blocks contain this many useful bytes. */
#define STRING_CHARS_BLOCK_SIZE \
  (8192 - MALLOC_OVERHEAD - ((2 * sizeof (struct string_chars_block *)) \
                             + sizeof (LISP_WORD_TYPE)))
/* Block header for small strings. */
struct string_chars_block
{
  LISP_WORD_TYPE pos;
  struct string_chars_block *next;
  struct string_chars_block *prev;
  /* Contents of string_chars_block->string_chars are interleaved
     string_chars structures (see below) and the actual string data */
  unsigned char string_chars[STRING_CHARS_BLOCK_SIZE];
};

struct string_chars_block *first_string_chars_block;
struct string_chars_block *current_string_chars_block;

/* If SIZE is the length of a string, this returns how many bytes
 *  the string occupies in string_chars_block->string_chars
 *  (including alignment padding).
 */
#define STRING_FULLSIZE(s) \
   ALIGN_SIZE (((s) + 1 + sizeof (struct Lisp_String *)),\
               ALIGNOF (struct Lisp_String *))

#define BIG_STRING_FULLSIZE_P(fullsize) ((fullsize) >= STRING_CHARS_BLOCK_SIZE)
#define BIG_STRING_SIZE_P(size) (BIG_STRING_FULLSIZE_P (STRING_FULLSIZE(size)))

#define CHARS_TO_STRING_CHAR(x) \
  ((struct string_chars *) \
   (((char *) (x)) - (SLOT_OFFSET (struct string_chars, chars))))


struct string_chars
{
  struct Lisp_String *string;
  unsigned char chars[1];
};


static void
init_string_alloc ()
{
  current_string_block = 0;
  current_string_block_index = countof (current_string_block->block);
  string_free_list = 0;

  first_string_chars_block = 
    (struct string_chars_block *) xmalloc (sizeof (struct string_chars_block));
  first_string_chars_block->prev = 0;
  first_string_chars_block->next = 0;
  first_string_chars_block->pos = 0;
  current_string_chars_block = first_string_chars_block;
}


Lisp_Object
make_uninit_string (int length)
{
  struct Lisp_String *s;
  struct string_chars *s_chars;
  int fullsize = STRING_FULLSIZE (length);
  Lisp_Object val;

  if ((length < 0) || (fullsize <= 0))
    abort ();

  /* Allocate the string header */
  if (string_free_list)
  {
    Lisp_Object chain = string_free_list->dup_list;
    s = string_free_list;
    string_free_list = ((STRINGP (chain)) ? XSTRING (chain) : 0);
  }
  else
  {
    ALLOCATE_FROB_FROM_BLOCK (string, s);
  }

  /* Allocate the string's actual data */
  if (BIG_STRING_FULLSIZE_P (fullsize))
  {
    s_chars = (struct string_chars *) xmalloc (fullsize);
  }
  else if (fullsize <=
           (countof (current_string_chars_block->string_chars)
            - current_string_chars_block->pos))
  {
    /* This string can fit in the current string chars block */
    s_chars = (struct string_chars *)(current_string_chars_block->string_chars
                                      + current_string_chars_block->pos);
    current_string_chars_block->pos += fullsize;
  }
  else
  {
    /* Make a new current string chars block */
    struct string_chars_block *new 
      = (struct string_chars_block *) xmalloc (sizeof (struct string_chars_block));

    current_string_chars_block->next = new;
    new->prev = current_string_chars_block;
    new->next = 0;
    current_string_chars_block = new;
    new->pos = fullsize;
    s_chars = (struct string_chars *) current_string_chars_block->string_chars;
  }

  s_chars->string = s;
  s->data = &(s_chars->chars[0]);
  s->size = length;
  s->dup_list = Qnil;

  s->data[length] = 0;

  INCREMENT_CONS_COUNTER (sizeof (struct Lisp_String) + fullsize);

  XSET (val, Lisp_String, s);
  return (val);
}

DEFUN ("make-string", Fmake_string, Smake_string, 2, 2, 0,
  "Return a newly created string of length LENGTH, with each element being INIT.\n\
Both LENGTH and INIT must be numbers.")
  (length, init)
     Lisp_Object length, init;
{
  Lisp_Object val;

  if (!FIXNUMP (length) || XINT (length) < 0)
    length = wrong_type_argument (Qnatnump, length);
  CHECK_FIXNUM (init, 1);
  val = make_uninit_string (XINT (length));
  memset (XSTRING (val)->data, XINT (init), string_length (XSTRING (val)));
  return (val);
}

Lisp_Object
make_string (CONST char *contents, int length)
{
  Lisp_Object val;
  val = make_uninit_string (length);
  memcpy (XSTRING (val)->data, contents, length);
  return (val);
}

Lisp_Object
build_string (CONST char *str)
{
  return make_string (str, strlen (str));
}

/**********************************************************************/
/* Purity of essense, peace on earth                                  */
/**********************************************************************/

static int symbols_initialised;

Lisp_Object
make_pure_string (CONST char *data, int length,
                  int no_need_to_copy_data)
{
  Lisp_Object new;
  struct Lisp_String *s;
  int size = (sizeof (struct Lisp_String) + ((no_need_to_copy_data) 
                                             ? 0 
                                             /* + 1 for terminating 0 */
                                             : (length + 1)));
  size = ALIGN_SIZE (size, ALIGNOF (Lisp_Object));

  if (symbols_initialised && !pure_lossage)
    {
      /* Try to share some names.  Saves a few kbytes. */
      Lisp_Object tem = oblookup (Vobarray, (unsigned char *) data, length);
      if (SYMBOLP (tem))
	{
	  s = XSYMBOL (tem)->name;
	  if (!PURIFIED (s)) abort ();
	  XSET (new, Lisp_String, s);
	  return (new);
	}
    }

  if (!check_purespace (size))
    return (make_string (data, length));

  s = (struct Lisp_String *) (PUREBEG + pureptr);
  s->size = length;
  if (no_need_to_copy_data)
  {
    s->data = (unsigned char *) data;
  }
  else
  {
    s->data = (unsigned char *) s + sizeof (struct Lisp_String);
  memcpy (s->data, data, length);
  s->data[length] = 0;
  }
  /* A waste: always Qnil in pure strings. */
  s->dup_list = Qnil;
  pureptr += size;

#ifdef PURESTAT
  bump_purestat (&purestat_string_all, size);
  if (purecopying_for_bytecode)
    bump_purestat (&purestat_string_other_function, size);
#endif

  XSET (new, Lisp_String, s);
  return (new);
}


Lisp_Object
make_pure_pname (CONST char *data, int length, int no_need_to_copy_data)
{
  Lisp_Object name = make_pure_string (data, length, no_need_to_copy_data);
  bump_purestat (&purestat_string_pname, pure_sizeof (name));

  /* We've made (at least) Qnil now, and Vobarray will soon be set up. */
  symbols_initialised = 1;

  return (name);
}


Lisp_Object
pure_cons (Lisp_Object car, Lisp_Object cdr)
{
  Lisp_Object new;

  if (!check_purespace (sizeof (struct Lisp_Cons)))
    return (Fcons (Fpurecopy (car), Fpurecopy (cdr)));

  XSET (new, Lisp_Cons, PUREBEG + pureptr);
  pureptr += sizeof (struct Lisp_Cons);
  bump_purestat (&purestat_cons, sizeof (struct Lisp_Cons));

  XCONS (new)->car = Fpurecopy (car);
  XCONS (new)->cdr = Fpurecopy (cdr);
  return (new);
}

#ifdef LISP_FLOAT_TYPE

Lisp_Object
make_pure_float (double num)
{
  struct Lisp_Float *f;
  Lisp_Object val;

  /* Make sure that PUREBEG + pureptr is aligned on at least a sizeof
     (double) boundary.  Some architectures (like the sparc) require
     this, and I suspect that floats are rare enough that it's no
     tragedy for those that don't. */
  {
#if defined (__GNUC__) && (__GNUC__ >= 2)
    /* In gcc, we can directly ask what the alignment constraints of a
       structure are, but in general, that's not possible...  Arrgh!!
     */
    int alignment = __alignof (struct Lisp_Float);
#else /* !GNUC */
    /* Best guess is to make the `double' slot be aligned to the size
       of double (which is probably 8 bytes.)  This assumes that it's
       ok to align the beginning of the structure to the same boundary
       that the `double' slot in it is supposed to be aligned to; this
       should be ok because presumably there is padding in the layout
       of the struct to account for this.
     */
    int alignment = sizeof (float_data (f));
#endif
    char *p = ((char *) PUREBEG + pureptr);

    p = (char *) (((unsigned LISP_WORD_TYPE) p + alignment - 1) & - alignment);
    pureptr = p - (char *) PUREBEG;
  }

  if (!check_purespace (sizeof (struct Lisp_Float)))
    return (make_float (num));

  f = (struct Lisp_Float *) (PUREBEG + pureptr);
  set_lheader_implementation (&(f->lheader), lrecord_float);
  pureptr += sizeof (struct Lisp_Float);
  bump_purestat (&purestat_float, sizeof (struct Lisp_Float));

  float_next (f) = ((struct Lisp_Float *) -1);
  float_data (f) = num;
  XSETFLOAT (val, f);
  return (val);
}

#endif /* LISP_FLOAT_TYPE */

Lisp_Object
make_pure_vector (int len, Lisp_Object init)
{
  Lisp_Object new;
  int size = (sizeof (struct Lisp_Vector)
              + (len - 1) * sizeof (Lisp_Object));

  init = Fpurecopy (init);

  if (!check_purespace (size))
    return (make_vector (len, init));

  XSET (new, Lisp_Vector, PUREBEG + pureptr);
  pureptr += size;
  bump_purestat (&purestat_vector_all, size);

  XVECTOR (new)->size = len;

  for (size = 0; size < len; size++)
    XVECTOR (new)->contents[size] = init;

  return (new);
}

#if 0
/* Presently unused */
void *
alloc_pure_lrecord (int size, struct lrecord_implementation *implementation)
{
  struct lrecord_header *header = (void *) (PUREBEG + pureptr);

  if (pureptr + size > PURESIZE)
    pure_storage_exhausted ();

  set_lheader_implementation (header, implementation);
  header->next = 0;
  return (header);
}
#endif



DEFUN ("purecopy", Fpurecopy, Spurecopy, 1, 1, 0,
  "Make a copy of OBJECT in pure storage.\n\
Recursively copies contents of vectors and cons cells.\n\
Does not copy symbols.")
     (obj)
     Lisp_Object obj;
{
  int i;
  if (!purify_flag)
    return (obj);

  if (!POINTER_TYPE_P (XTYPE (obj))
      || PURIFIED (XPNTR (obj)))
    return (obj);

  switch (XTYPE (obj))
    {
    case Lisp_Cons:
      return pure_cons (XCONS (obj)->car, XCONS (obj)->cdr);

    case Lisp_String:
      return make_pure_string ((char *) XSTRING (obj)->data,
			       string_length (XSTRING (obj)), 
                               0);

    case Lisp_Vector:
      {
        struct Lisp_Vector *o = XVECTOR (obj);
        Lisp_Object new = make_pure_vector (vector_length (o), Qnil);
        for (i = 0; i < vector_length (o); i++)
	  XVECTOR (new)->contents[i] = Fpurecopy (o->contents[i]);
        return (new);
      }

    default:
      {
        if (COMPILEDP (obj))
          {
            struct Lisp_Bytecode *o = XBYTECODE (obj);
            Lisp_Object new = make_bytecode (1);
            struct Lisp_Bytecode *n = XBYTECODE (obj);
            n->flags = o->flags;
            n->bytecodes = Fpurecopy (o->bytecodes);
            n->constants = Fpurecopy (o->constants);
            n->arglist = Fpurecopy (o->arglist);
            n->doc_and_interactive = Fpurecopy (o->doc_and_interactive);
            return (new);
          }
#ifdef LISP_FLOAT_TYPE
        else if (FLOATP (obj))
          return make_pure_float (float_data (XFLOAT (obj)));
#endif /* LISP_FLOAT_TYPE */
	else if (!SYMBOLP (obj))
          signal_error (Qerror,
                        (list2 (build_string (GETTEXT ("Can't purecopy %S")),
                                obj)));
      }
    }
  return (obj);
}



void
report_pure_usage (int report_impurities,
                   int die_if_pure_storage_exceeded)
{
  if (pure_lossage)
    {
      const long report_round = 5000;

       fprintf (stderr, ("\n****\tPure Lisp storage exhausted!\n"
 "\tCheck whether you are loading .el files when .elc files were intended.\n"
 "\tOtherwise, increase PURESIZE in puresize.h and relink.\n\n"
 "\tPURESIZE is presently %ld.\n"
 "\tAn additional %ld bytes will guarantee enough pure space;\n"
 "\ta smaller increment may work (due to structure-sharing.)\n"
 "****\n"),
	       (long) PURESIZE,
               (((pure_lossage + report_round - 1)
                 / report_round) * report_round));
    }
  else
    {
      int lost = (PURESIZE - pureptr) / 1024;

      fprintf (stderr, GETTEXT ("Purespace usage: %ld of %ld (%d%%"),
               pureptr, (long) PURESIZE,
               (int) (pureptr / (PURESIZE / 100.0) + 0.5));
      if (lost > 2)
        fprintf (stderr, GETTEXT (" -- %dk wasted"), lost);
      fprintf (stderr, ").\n");
    }

#ifdef PURESTAT
  {
    int iii;

    purestat_vector_other.nbytes =
      purestat_vector_all.nbytes - purestat_vector_bytecode_constants.nbytes;
    purestat_vector_other.nobjects =
      purestat_vector_all.nobjects - purestat_vector_bytecode_constants.nobjects;

    purestat_string_other.nbytes =
      purestat_string_all.nbytes - (purestat_string_pname.nbytes +
                                    purestat_string_bytecodes.nbytes +
                                    purestat_string_interactive.nbytes +
                                    purestat_string_documentation.nbytes +
#ifdef I18N3
                                    purestat_string_domain.nbytes +
#endif
                                    purestat_string_other_function.nbytes);
    purestat_string_other.nobjects =
      purestat_string_all.nobjects - (purestat_string_pname.nobjects +
                                      purestat_string_bytecodes.nobjects +
                                      purestat_string_interactive.nobjects +
                                      purestat_string_documentation.nobjects +
#ifdef I18N3
                                      purestat_string_domain.nobjects +
#endif
                                      purestat_string_other_function.nobjects);

    fprintf (stderr, GETTEXT ("   %-24stotal:   bytes:\n"), "");

    for (iii = 0; iii < countof (purestats); iii++)
      if (!purestats[iii])
        fprintf (stderr, "\n");
      else
        fprintf (stderr, "   %-24s%5d  %7d  %2d%%\n",
                 purestats[iii]->name,
                 purestats[iii]->nobjects,
                 purestats[iii]->nbytes,
                  (int) (purestats[iii]->nbytes / (pureptr / 100.0) + 0.5));
  }
#endif /* PURESTAT */

  if (report_impurities)
    {
      Lisp_Object tem = Felt (Fgarbage_collect (), make_number (5));
      fprintf (stderr, GETTEXT ("\nImpurities:\n"));
      while (!NILP (tem))
	{
	  if (CONSP (tem) && SYMBOLP (Fcar (tem)) && CONSP (Fcdr (tem)))
	    {
	      int total = XINT (Fcar (Fcdr (tem)));
	      if (total > 0)
		{
		  char buf [100];
		  char *s = buf;
		  memcpy (buf, XSYMBOL (Fcar (tem))->name->data,
			  XSYMBOL (Fcar (tem))->name->size + 1);
		  while (*s++) if (*s == '-') *s = ' ';
		  s--; *s++ = ':'; *s = 0;
		  fprintf (stderr, "   %-32s%6d\n", buf, total);
		}
	      tem = Fcdr (Fcdr (tem));
	    }
	  else			/* WTF?! */
	    {
	      Fprin1 (tem, Qexternal_debugging_output);
	      tem = Qnil;
	    }
	}
      garbage_collect_1 ();         /* GC garbage_collect's garbage */
    }
  fprintf (stderr, "\n");
  fflush (stderr);

  if (pure_lossage && die_if_pure_storage_exceeded)
    fatal ("Pure storage exhausted");
}


/**********************************************************************/
/* staticpro                                                          */
/**********************************************************************/

struct gcpro *gcprolist;

/* 415 used Mly 29-Jun-93 */
#define NSTATICS 700
/* Not "static" because of linker lossage on some systems */
Lisp_Object *staticvec[NSTATICS]
     /* Force it into data space! */
     = {0};
static int staticidx;

/* Put an entry in staticvec, pointing at the variable whose address is given
 */
void
staticpro (Lisp_Object *varaddress)
{
  if (staticidx >= countof (staticvec))
    abort ();
  staticvec[staticidx++] = varaddress;
}


/* Mark reference to a Lisp_Object.  If the object referred to has not been
   seen yet, recursively mark all the references contained in it. */
   
static void
mark_object (Lisp_Object obj)
{
 tail_recurse:

  if (!POINTER_TYPE_P (XGCTYPE (obj)))
    return;
  if (PURIFIED (XPNTR (obj)))
    return;
  switch (XGCTYPE (obj))
    {
    case Lisp_Cons:
      {
	struct Lisp_Cons *ptr = XCONS (obj);
	if (XMARKBIT (ptr->car))
	  break;
	XMARK (ptr->car);
	/* If the cdr is nil, tail-recurse on the car.  */
	if (EQ (ptr->cdr, Qnil))
	  {
	    obj = ptr->car;
	  }
	else
	  {
	    mark_object (ptr->car);
	    obj = ptr->cdr;
	  }
	goto tail_recurse;
      }

    case Lisp_Record:
    /* case Lisp_Symbol_Value_Magic: */
      {
	struct lrecord_header *lheader = XRECORD_LHEADER (obj);
	CONST struct lrecord_implementation *implementation
	  = lheader->implementation;

	if (implementation->marker != 0) /* will be 0 if already marked */
	  {
	    MARK_RECORD_HEADER (lheader);
	    obj = ((implementation->marker) (obj, mark_object));
            if (!NILP (obj)) goto tail_recurse;
	  }
      }
      break;

    case Lisp_String:
      {
	struct Lisp_String *ptr = XSTRING (obj);

	if (!XMARKBIT (ptr->dup_list))
	  {
	    XMARK (ptr->dup_list);
	    obj = ptr->dup_list;
	    goto tail_recurse;
	  }
      }
      break;

    case Lisp_Vector:
      {
	struct Lisp_Vector *ptr = XVECTOR (obj);
	int len = vector_length (ptr);
	int i;

	if (len < 0)
	  break;		/* Already marked */
	ptr->size = -1 - len;	/* Else mark it */
	for (i = 0; i < len - 1; i++) /* and then mark its elements */
	  mark_object (ptr->contents[i]);
        if (len > 0)
        {
          obj = ptr->contents[len - 1];
          goto tail_recurse;
        }
      }
      break;

#ifndef LRECORD_SYMBOL
    case Lisp_Symbol:
      {
	struct Lisp_Symbol *sym = XSYMBOL (obj);

	while (!XMARKBIT (sym->plist))
	  {
	    XMARK (sym->plist);
	    mark_object (sym->value);
	    mark_object (sym->function);
	    {
	      /*  Open-code mark_string */
	      /*  symbol->name is a struct Lisp_String *, not a Lisp_Object */
	      struct Lisp_String *pname = sym->name;
	      if (!PURIFIED (pname)
		  && !XMARKBIT (pname->dup_list))
		{
		  XMARK (pname->dup_list);
		  mark_object (pname->dup_list);
		}
	    }
	    if (!symbol_next (sym))
	      {
		obj = sym->plist;
		goto tail_recurse;
	      }
	    mark_object (sym->plist);
	    /* Mark the rest of the symbols in the hash-chain */
	    sym = symbol_next (sym);
	  }
      }
      break;
#endif /* !LRECORD_SYMBOL */

    default:
      abort ();
    }
}

#ifdef PURESTAT
/* Simpler than mark-object, because pure structure can't 
   have any circularities
 */

#if 0 /* unused */
static int idiot_c_doesnt_have_closures;
static void
idiot_c (Lisp_Object obj)
{
  idiot_c_doesnt_have_closures += pure_sizeof (obj, 1);
}
#endif /* unused */

/* recurse arg isn't actually used */
static int
pure_sizeof (Lisp_Object obj /*, int recurse */)
{
  int total = 0;

 /*tail_recurse: */
  if (!POINTER_TYPE_P (XTYPE (obj))
      || !PURIFIED (XPNTR (obj)))
    return (total);

  /* symbol's sizes are accounted for separately */
  if (SYMBOLP (obj))
    return (total);

  switch (XTYPE (obj))
    {
    case Lisp_String:
      {
	struct Lisp_String *ptr = XSTRING (obj);
        int size = string_length (ptr);

        if (ptr->data != (unsigned char *) ptr + sizeof (struct Lisp_String))
        {
          /* string-data not allocated contiguously.  
             Probably (better be!!) a pointer constant "C" data. */
          size = sizeof (struct Lisp_String);
        }
        else
        {
          size = sizeof (struct Lisp_String) + size + 1;
          size = ALIGN_SIZE (size, sizeof (Lisp_Object));
        }
        total += size;
      }
      break;

    case Lisp_Vector:
      {
        struct Lisp_Vector *ptr = XVECTOR (obj);
        int len = vector_length (ptr);

        total += (sizeof (struct Lisp_Vector)
                  + (len - 1) * sizeof (Lisp_Object));
#if 0 /* unused */
        if (!recurse)
          break;
        { 
          int i;
	  for (i = 0; i < len - 1; i++)
	    total += pure_sizeof (ptr->contents[i], 1);
	}
        if (len > 0)
	  {
	    obj = ptr->contents[len - 1];
	    goto tail_recurse;
	  }
#endif /* unused */
      }
      break;

    case Lisp_Record:
      {
	struct lrecord_header *lheader = XRECORD_LHEADER (obj);
	CONST struct lrecord_implementation *implementation
	  = lheader->implementation;

        if (implementation->size_in_bytes_method)
          total += ((implementation->size_in_bytes_method) (lheader));
	else
          total += implementation->static_size;

#if 0 /* unused */
        if (!recurse)
          break;

	if (implementation->marker != 0)
        {
          int old = idiot_c_doesnt_have_closures;

          idiot_c_doesnt_have_closures = 0;
          obj = ((implementation->marker) (obj, idiot_c));
          total += idiot_c_doesnt_have_closures;
          idiot_c_doesnt_have_closures = old;
            
          if (!NILP (obj)) goto tail_recurse;
        }
#endif /* unused */
      }
      break;

    case Lisp_Cons:
      {
        struct Lisp_Cons *ptr = XCONS (obj);
        total += sizeof (*ptr);
#if 0 /* unused */
        if (!recurse)
          break;
	/* If the cdr is nil, tail-recurse on the car.  */
	if (EQ (ptr->cdr, Qnil))
	  {
	    obj = ptr->car;
	  }
	else
	  {
	    total += pure_sizeof (ptr->car, 1);
	    obj = ptr->cdr;
	  }
	goto tail_recurse;
#endif /* unused */
      }
      break;

      /* Others can't be purified */
    default:
      abort ();
    }
  return (total);
}
#endif /* PURESTAT */




/* Find all structures not marked, and free them. */

static int gc_count_conses_in_use, gc_count_conses_freelist;
static int gc_count_symbols_in_use, gc_count_symbols_freelist;
static int gc_count_vectors_used, gc_count_vector_total_size;
static int gc_count_vector_storage;
static int gc_count_bytecodes_in_use;
static int gc_count_bytecodes_freelist;
static int gc_count_strings_in_use;
static int gc_count_short_strings_in_use;
static int gc_count_strings_freelist;
static int gc_count_string_total_size;
static int gc_count_short_string_total_size;
static int gc_count_markers_in_use, gc_count_markers_freelist;
#ifdef LISP_FLOAT_TYPE
static int gc_count_floats_freelist, gc_count_floats_in_use;
#endif /* LISP_FLOAT_TYPE */
static int gc_count_events_freelist, gc_count_events_in_use;
static int gc_count_extents_freelist, gc_count_extents_in_use;
static int gc_count_extent_replicas_freelist, gc_count_extent_replicas_in_use;

/* static int gc_count_total_records_used, gc_count_records_total_size; */


/* This will be used more extensively In The Future */
static int last_lrecord_type_index_assigned;

static CONST struct lrecord_implementation *lrecord_implementations_table[128];
#define max_lrecord_type (countof (lrecord_implementations_table) - 1)

static int
lrecord_type_index (CONST struct lrecord_implementation *implementation)
{
  int type_index = *(implementation->lrecord_type_index);
  /* Have to do this circuitous and validation test because of problems
     dumping out initialised variables (ie can't set xxx_type_index to -1
     because that would make xxx_type_index read-only in a dumped emacs. */
  if (type_index < 0 || type_index > max_lrecord_type
      || lrecord_implementations_table[type_index] != implementation)
    {
      if (last_lrecord_type_index_assigned == max_lrecord_type)
        abort ();
      type_index = ++last_lrecord_type_index_assigned;
      lrecord_implementations_table[type_index] = implementation;
      *(implementation->lrecord_type_index) = type_index;
    }
  return (type_index);
}

/* stats on lcrecords in use - kinda kludgy */

static struct {
  int instances_in_use;
  int bytes_in_use;
  int instances_freed;
  int bytes_freed;
} lcrecord_stats [countof (lrecord_implementations_table)];


static void
reset_lcrecord_stats ()
{
  int i;
  for (i = 0; i < countof (lcrecord_stats); i++)
  {
    lcrecord_stats[i].instances_in_use = 0;
    lcrecord_stats[i].bytes_in_use = 0;
    lcrecord_stats[i].instances_freed = 0;
    lcrecord_stats[i].bytes_freed = 0;
  }
}

static void
tick_lcrecord_stats (CONST struct lrecord_header *h, int free_p)
{
  CONST struct lrecord_implementation *implementation = h->implementation;
  int type_index = lrecord_type_index (implementation);
  unsigned int sz = (implementation->size_in_bytes_method
                     ? ((implementation->size_in_bytes_method) (h))
                     : implementation->static_size);

  if (free_p)
    {
      lcrecord_stats[type_index].instances_freed++;
      lcrecord_stats[type_index].bytes_freed += sz;
    }
  else
    {
      lcrecord_stats[type_index].instances_in_use++;
      lcrecord_stats[type_index].bytes_in_use += sz;
    }
}


/* Free all unmarked records */
static void
sweep_lcrecords_1 (struct lcrecord_header **prev, int *used)
{
  struct lcrecord_header *header;
  int num_used = 0;
  /* int total_size = 0; */
  reset_lcrecord_stats ();
  for (header = *prev; header; )
    {
      struct lrecord_header *h = &(header->lheader);
      if (MARKED_RECORD_HEADER_P (h))
	{
	  UNMARK_RECORD_HEADER (h);
	  num_used++;
	  /* total_size += ((n->implementation->size_in_bytes) (h));*/
	  prev = &(header->next);
	  header = *prev;
	  tick_lcrecord_stats (h, 0);
	}
      else
	{
	  struct lcrecord_header *next = header->next;
          *prev = next;
	  tick_lcrecord_stats (h, 1);
	  if (h->implementation->finaliser)
	    ((h->implementation->finaliser) (h, 0));
	  xfree (header);
	  header = next;
	}
    }
  *used = num_used;
  /* *total = total_size; */
}

static void
sweep_vectors_1 (Lisp_Object *prev, 
                 int *used, int *total, int *storage)
{
  Lisp_Object vector;
  int num_used = 0;
  int total_size = 0;
  int total_storage = 0;

  for (vector = *prev; VECTORP (vector); )
    {
      struct Lisp_Vector *v = XVECTOR (vector);
      int len = v->size;
      if (len < 0)     /* marked */
	{
          len = - (len + 1);
	  v->size = len;
	  total_size += len;
          total_storage += (MALLOC_OVERHEAD
                            + sizeof (struct Lisp_Vector)
                            + (len - 1 + 1) * sizeof (Lisp_Object));
	  num_used++;
	  prev = &(vector_next (v));
	  vector = *prev;
	}
      else
	{
          Lisp_Object next = vector_next (v);
          *prev = next;
	  xfree (v);
	  vector = next;
	}
    }
  *used = num_used;
  *total = total_size;
  *storage = total_storage;
}


/* Uses "free" variables num_used, num_free */
#define SWEEP_FROB_BLOCK(typename, obj_type) \
  do { \
    struct typename##_block *_frob_current; \
    struct typename##_block **_frob_prev; \
    int _frob_limit; \
    \
    typename##_free_list = 0; \
    \
    for (_frob_prev = &current_##typename##_block, \
	    _frob_current = current_##typename##_block, \
	    _frob_limit = current_##typename##_block_index; \
         _frob_current; \
         ) \
    { \
      int _frob_iii; \
      int _frob_empty = 1; \
      obj_type *_frob_old_free_list = typename##_free_list; \
      \
      for (_frob_iii = 0; _frob_iii < _frob_limit; _frob_iii++) \
      { \
        obj_type *_frob_victim = &(_frob_current->block[_frob_iii]); \
        if (!MARKED_##typename##_P (_frob_victim)) \
        { \
          num_free++; \
          FREE_##typename (_frob_victim); \
        } \
        else \
        { \
          _frob_empty = 0; \
          num_used++; \
          UNMARK_##typename (_frob_victim); \
        } \
      } \
      if (!_frob_empty) \
      { \
        _frob_prev = &(_frob_current->prev); \
        _frob_current = _frob_current->prev; \
      } \
      else if (_frob_current == current_##typename##_block \
               && !_frob_current->prev) \
      { \
        /* No real point in freeing sole allocation block */ \
        break; \
      } \
      else \
      { \
        struct typename##_block *_frob_victim_block = _frob_current; \
        if (_frob_victim_block == current_##typename##_block) \
          current_##typename##_block_index \
            = countof (current_##typename##_block->block); \
        _frob_current = _frob_current->prev; \
        { \
          *_frob_prev = _frob_current; \
          xfree (_frob_victim_block); \
          /* Restore free list to what it was before victim was swept */ \
          typename##_free_list = _frob_old_free_list; \
          num_free -= _frob_limit; \
        } \
      } \
      _frob_limit = countof (current_##typename##_block->block); \
    } \
  } while (0)




static void
sweep_conses ()
{
  int num_free = 0, num_used = 0;

#define MARKED_cons_P(ptr) XMARKBIT ((ptr)->car)
#define UNMARK_cons(ptr) do { XUNMARK ((ptr)->car); } while (0)
#define FREE_cons(ptr) do { struct Lisp_Cons *p = (ptr); \
                            /* Inline free_cons (ptr) */ \
			    if (cons_free_list) \
                              XSET (p->car, Lisp_Cons, cons_free_list); \
			    else \
			      p->car = Qzero; \
                            cons_free_list = p; \
                          } while (0)

  SWEEP_FROB_BLOCK (cons, struct Lisp_Cons);

#undef MARKED_cons_P
#undef UNMARK_cons
#undef FREE_cons

  gc_count_conses_in_use = num_used;
  gc_count_conses_freelist = num_free;
}


static void
sweep_bytecodes ()
{
  int num_free = 0, num_used = 0;

#define MARKED_bytecode_P(ptr) MARKED_RECORD_HEADER_P (&((ptr)->lheader))
#define UNMARK_bytecode(ptr) UNMARK_RECORD_HEADER (&((ptr)->lheader))
#define FREE_bytecode(ptr) do { struct Lisp_Bytecode *p = (ptr); \
                                if (bytecode_free_list) \
                                  XSETR (p->bytecodes, Lisp_Bytecode, \
                                         bytecode_free_list); \
                                else \
                                  p->bytecodes = Qzero; \
		                 bytecode_free_list = p; \
                              } while (0)

  SWEEP_FROB_BLOCK (bytecode, struct Lisp_Bytecode);

#undef MARKED_bytecode_P
#undef UNMARK_bytecode
#undef FREE_bytecode

  gc_count_bytecodes_in_use = num_used;
  gc_count_bytecodes_freelist = num_free;
}


#ifdef LISP_FLOAT_TYPE
static void
sweep_floats ()
{
  int num_free = 0, num_used = 0;

#define MARKED_float_P(ptr) MARKED_RECORD_HEADER_P (&((ptr)->lheader))
#define UNMARK_float(ptr) UNMARK_RECORD_HEADER (&((ptr)->lheader))
#define FREE_float(ptr) do { struct Lisp_Float *p = (ptr); \
			     float_next (p) = float_free_list; \
		             float_free_list = p; \
                           } while (0)

  SWEEP_FROB_BLOCK (float, struct Lisp_Float);

#undef MARKED_float_P
#undef UNMARK_float
#undef FREE_float

  gc_count_floats_in_use = num_used;
  gc_count_floats_freelist = num_free;
}
#endif /* LISP_FLOAT_TYPE */

static void
sweep_symbols ()
{
  int num_free = 0, num_used = 0;

#ifndef LRECORD_SYMBOL
# define MARKED_symbol_P(ptr) XMARKBIT ((ptr)->plist)
# define UNMARK_symbol(ptr) do { XUNMARK ((ptr)->plist); } while (0)
#else
# define MARKED_symbol_P(ptr) MARKED_RECORD_HEADER_P (&((ptr)->lheader))
# define UNMARK_symbol(ptr) UNMARK_RECORD_HEADER (&((ptr)->lheader))
#endif /* !LRECORD_SYMBOL */
#define FREE_symbol(ptr) do { struct Lisp_Symbol *p = (ptr); \
                              symbol_next (p) = symbol_free_list; \
                              symbol_free_list = p; \
                            } while (0)

  SWEEP_FROB_BLOCK (symbol, struct Lisp_Symbol);

#undef MARKED_symbol_P
#undef UNMARK_symbol
#undef FREE_symbol

  gc_count_symbols_in_use = num_used;
  gc_count_symbols_freelist = num_free;
}


#ifndef standalone

#ifdef ENERGIZE
extern void energize_extent_finalization (struct extent *);
#else
#define energize_extent_finalization(extent) /* Empty */
#endif

static void
sweep_extents ()
{
  int num_free = 0, num_used = 0;

#define MARKED_extent_P(ptr) MARKED_RECORD_HEADER_P (&((ptr)->lheader))
#define UNMARK_extent(ptr) UNMARK_RECORD_HEADER (&((ptr)->lheader))
#define FREE_extent(ptr) do { struct extent *p = (ptr); \
                              energize_extent_finalization (p); \
                              /* memset (p, 0, sizeof (*p)); */ \
		              p->next = extent_free_list; \
              		      extent_free_list = p; \
                            } while (0)

  SWEEP_FROB_BLOCK (extent, struct extent);

#undef MARKED_extent_P
#undef UNMARK_extent
#undef FREE_extent

  gc_count_extents_in_use = num_used;
  gc_count_extents_freelist = num_free;
}

static void
sweep_extent_replicas ()
{
  int num_free = 0, num_used = 0;

#define MARKED_extent_replica_P(ptr) MARKED_RECORD_HEADER_P(&((ptr)->lheader))
#define UNMARK_extent_replica(ptr) UNMARK_RECORD_HEADER(&((ptr)->lheader))
#define FREE_extent_replica(ptr) do { struct extent_replica *p = (ptr); \
                                      Lisp_Object *n = &(dup_extent (p)); \
                                      /* memset (p, 0, sizeof (*p)); */\
                                      if (extent_replica_free_list) \
                                        XSETEXTENT (*n, \
                                                    extent_replica_free_list);\
                                      else \
                                        *n = Qzero; \
                                      extent_replica_free_list = p; \
                                    } while (0)

  SWEEP_FROB_BLOCK (extent_replica, struct extent_replica);

#undef MARKED_extent_replica_P
#undef UNMARK_extent_replica
#undef FREE_extent_replica

  gc_count_extent_replicas_in_use = num_used;
  gc_count_extent_replicas_freelist = num_free;
}


static void
sweep_events ()
{
  int num_free = 0, num_used = 0;

#define MARKED_event_P(ptr) MARKED_RECORD_HEADER_P (&((ptr)->lheader))
#define UNMARK_event(ptr) UNMARK_RECORD_HEADER (&((ptr)->lheader))
#define FREE_event(ptr) do { struct Lisp_Event *p = (ptr); \
		             set_event_next (p, event_free_list); \
              		     event_free_list = p; \
                           } while (0)

  SWEEP_FROB_BLOCK (event, struct Lisp_Event);

#undef MARKED_event_P
#undef UNMARK_event
#undef FREE_event

  gc_count_events_in_use = num_used;
  gc_count_events_freelist = num_free;
}


static void
sweep_markers ()
{
  int num_free = 0, num_used = 0;

#define MARKED_marker_P(ptr) MARKED_RECORD_HEADER_P (&((ptr)->lheader))
#define UNMARK_marker(ptr) UNMARK_RECORD_HEADER (&((ptr)->lheader))
#define FREE_marker(ptr) do { struct Lisp_Marker *p = (ptr); \
                              Lisp_Object tem; \
            		      XSETR (tem, Lisp_Marker, p); \
            		      unchain_marker (tem); \
            		      marker_next (p) = marker_free_list; \
                              marker_free_list = p; \
                            } while (0)

  SWEEP_FROB_BLOCK (marker, struct Lisp_Marker);

#undef MARKED_marker_P
#undef UNMARK_marker
#undef FREE_marker

  gc_count_markers_in_use = num_used;
  gc_count_markers_freelist = num_free;
}
#endif /* not standalone */


/* Compactify string chars, relocating the reference to each --
   free any empty string_chars_block we see. */
static void
compact_string_chars ()
{
  struct string_chars_block *to_sb = first_string_chars_block;
  int to_pos = 0;
  struct string_chars_block *from_sb;

  /* Scan each existing string block sequentially, string by string.  */
  for (from_sb = first_string_chars_block; from_sb; from_sb = from_sb->next)
    {
      int from_pos = 0;
      /* FROM_POS is the index of the next string in the block.  */
      while (from_pos < from_sb->pos)
        {
          struct string_chars *from_s_chars = 
            (struct string_chars *) &(from_sb->string_chars[from_pos]);
          struct string_chars *to_s_chars;
          struct Lisp_String *string = from_s_chars->string;
          int size = string_length (string);
          int fullsize = STRING_FULLSIZE (size);

          if (BIG_STRING_FULLSIZE_P (fullsize))
            abort ();

          /* Just skip it if it isn't marked.  */
          if (!XMARKBIT (string->dup_list))
            {
              from_pos += fullsize;
              continue;
            }

          /* If it won't fit in what's left of TO_SB, close TO_SB out
             and go on to the next string_chars_block.  We know that TO_SB
             cannot advance past FROM_SB here since FROM_SB is large enough
             to currently contain this string. */
          if ((to_pos + fullsize) > countof (to_sb->string_chars))
            {
              to_sb->pos = to_pos;
              to_sb = to_sb->next;
              to_pos = 0;
            }
             
          /* Compute new address of this string
             and update TO_POS for the space being used.  */
          to_s_chars = (struct string_chars *) &(to_sb->string_chars[to_pos]);

          /* Copy the string_chars to the new place.  */
          if (from_s_chars != to_s_chars)
            memcpy (to_s_chars, from_s_chars, fullsize);

          /* Relocate FROM_S_CHARS's reference */
          string->data = &(to_s_chars->chars[0]);
             
          from_pos += fullsize;
          to_pos += fullsize;
        }
    }

  /* Set current to the last string chars block still used and 
     free any that follow. */
  {
    struct string_chars_block *victim;

    for (victim = to_sb->next; victim; )
      {
	struct string_chars_block *next = victim->next;
	xfree (victim);
	victim = next;
      }

    current_string_chars_block = to_sb;
    current_string_chars_block->pos = to_pos;
    current_string_chars_block->next = 0;
  }
}

static void
sweep_strings ()
{
  int num_free = 0, num_used = 0;
  int num_small_used = 0, num_small_bytes = 0, num_bytes = 0;

#define MARKED_string_P(ptr) XMARKBIT ((ptr)->dup_list)
#define UNMARK_string(ptr) do { struct Lisp_String *p = (ptr); \
                                int size = string_length (p); \
                                XUNMARK (p->dup_list); \
                                num_bytes += size; \
                                if (!BIG_STRING_SIZE_P (size)) \
				{ num_small_bytes += size; \
                                  num_small_used++; \
                                } \
                              } while (0)
#define FREE_string(ptr) do { struct Lisp_String *p = ptr; \
                              int size = string_length (p); \
                              if (BIG_STRING_SIZE_P (size)) \
                                xfree (CHARS_TO_STRING_CHAR (p->data)); \
                              p->size = 0; \
                              p->data = 0; /* Core-dump-o-rama */ \
			      if (string_free_list) \
				XSET (p->dup_list, Lisp_String, \
				      string_free_list); \
			      else \
				p->dup_list = Qzero; \
                              string_free_list = p; \
                            } while (0)

  SWEEP_FROB_BLOCK (string, struct Lisp_String);

#undef MARKED_string_P
#undef UNMARK_string
#undef FREE_string

  gc_count_strings_in_use = num_used;
  gc_count_strings_freelist = num_free;
  gc_count_short_strings_in_use = num_small_used;
  gc_count_string_total_size = num_bytes;
  gc_count_short_string_total_size = num_small_bytes;
}


/* I hate duplicating all this crap! */
static int
marked_p (Lisp_Object obj)
{
  if (!POINTER_TYPE_P (XGCTYPE (obj))) return 1;
  if (PURIFIED (XPNTR (obj))) return 1;
  switch (XGCTYPE (obj))
    {
    case Lisp_Cons:
      return XMARKBIT (XCONS (obj)->car);
    case Lisp_Record:
      return MARKED_RECORD_HEADER_P (XRECORD_LHEADER (obj));
    case Lisp_String:
      return XMARKBIT (XSTRING (obj)->dup_list);
    case Lisp_Vector:
      return (vector_length (XVECTOR (obj)) < 0);
#ifndef LRECORD_SYMBOL
    case Lisp_Symbol:
      return XMARKBIT (XSYMBOL (obj)->plist);
#endif
    default:
      abort ();
    }
}

static void
gc_sweep ()
{
  prune_weak_hashtables (marked_p);

  compact_string_chars ();

  /* Put all unmarked strings on free list, free'ing the string chars
     of large unmarked strings */
  sweep_strings ();

  /* Put all unmarked conses on free list */
  sweep_conses ();

  /* Free all unmarked records */
  {
    int ignored;
    sweep_lcrecords_1 (&all_lcrecords, &ignored);
  }

  /* Free all unmarked vectors */
  sweep_vectors_1 (&all_vectors, 
                   &gc_count_vectors_used, &gc_count_vector_total_size,
                   &gc_count_vector_storage);

  /* Free all unmarked bytecode objects */
  sweep_bytecodes ();

#ifdef LISP_FLOAT_TYPE
  /* Put all unmarked floats on free list */
  sweep_floats ();
#endif

  /* Put all unmarked symbols on free list */
  sweep_symbols ();

#ifndef standalone
  /* Put all unmarked extents on free list */
  sweep_extents ();

  /* put all extent replicas on a free_list */
  sweep_extent_replicas ();

  /* Put all unmarked markers on free list.
     Dechain each one first from the buffer into which it points. */
  sweep_markers ();

  sweep_events ();
#endif

}

/* Clearing for disksave. */

extern Lisp_Object Vprocess_environment;
extern Lisp_Object Vdoc_directory;
extern Lisp_Object Vconfigure_info_directory;
extern Lisp_Object Vload_path;
extern Lisp_Object Vload_history;
extern Lisp_Object Vshell_file_name;

void
disksave_object_finalisation ()
{
  struct lcrecord_header *header;

  /* It's important that certain information from the environment not get
     dumped with the executable (pathnames, environment variables, etc.)
     To make it easier to tell when this has happend with strings(1) we
     clear some known-to-be-garbage blocks of memory, so that leftover
     results of old evaluation don't look like potential problems.
     But first we set some notable variables to nil and do one more GC,
     to turn those strings into garbage.
   */

  /* Yeah, this list is pretty ad-hoc... */
  Vprocess_environment = Qnil;
  Vexec_directory = Qnil;
  Vdata_directory = Qnil;
  Vdoc_directory = Qnil;
  Vconfigure_info_directory = Qnil;
  Vexec_path = Qnil;
  Vload_path = Qnil;
  /* Vdump_load_path = Qnil; */
  Vload_history = Qnil;
  Vshell_file_name = Qnil;

  garbage_collect_1 ();

  /* Run the disksave finalization methods of all live objects. */
  disksave_object_finalisation_1 ();

  BLOCK_INPUT;
  /* Zero out the unused portion of purespace */
  if (!pure_lossage)
    memset (  (char *) (PUREBEG + pureptr), 0,
	    (((char *) (PUREBEG + PURESIZE)) -
	     ((char *) (PUREBEG + pureptr))));

  /* Zero out the uninitialized (really, unused) part of the containers
     for the live strings. */
  {
    struct string_chars_block *scb;
    for (scb = first_string_chars_block; scb; scb = scb->next)
      /* from the block's fill ptr to the end */
      memset ((scb->string_chars + scb->pos), 0,
	      sizeof (scb->string_chars) - scb->pos);
  }

  /* There, that ought to be enough... */

  UNBLOCK_INPUT;
}


/* GC hooks */

/* Maybe we want to use this when doing a "panic" gc after memory_full()? */
static int gc_hooks_inhibited;

static Lisp_Object
gc_hook_2 (Lisp_Object hook_symbol)
{
  call1 (Vrun_hooks, hook_symbol);
  return Qnil;
}

static Lisp_Object
gc_hook_error_handler (Lisp_Object foo, Lisp_Object bar)
{
  return Qt;
}

static Lisp_Object
call_gc_hook (Lisp_Object hook_symbol)
{
  int speccount = specpdl_depth ();
  Lisp_Object tem;

  if (gc_currently_forbidden)
    abort ();
  if (gc_hooks_inhibited)
    return (Qnil);
  if (NILP (Vrun_hooks))
    return (Qnil);
  tem = find_symbol_value (hook_symbol);
  if (NILP (tem) || EQ (tem, Qunbound))
    return (Qnil);

  specbind (Qinhibit_quit, Qt);
  gc_currently_forbidden = 1;

  tem = condition_case_1 (Qt,
                          gc_hook_2, hook_symbol,
                          gc_hook_error_handler, Qnil);

  gc_currently_forbidden = 0;
  return unbind_to (speccount, tem);
}


#ifdef HAVE_X_WINDOWS
extern int x_show_gc_cursor (struct screen *s, Lisp_Object cursor);
#endif

extern void mark_redisplay (void (*) (Lisp_Object));

void
garbage_collect_1 ()
{
  char stack_top_variable;
  extern char *stack_bottom;
  int i;
  struct screen *s = selected_screen;
  int pre_hook_error_p = 0;
  int post_hook_error_p = 0;
  char *omessage = 0;

#ifdef HAVE_X_WINDOWS
  int cursor_changed = 0;
#endif

  if (gc_in_progress != 0)
    return;

  if (gc_currently_forbidden)
    return;

  if (echo_area_glyphs)
    {
      /* The contents of echo_area_glyphs gets smashed, so we must copy it. */
      omessage = (char *) alloca (strlen (echo_area_glyphs) + 1);
      strcpy (omessage, echo_area_glyphs);
    }

  pre_hook_error_p = !NILP (call_gc_hook (Qpre_gc_hook));

  gc_in_progress = 1;

  gc_generation_number[0]++;

#if MAX_SAVE_STACK > 0

  /* Save a copy of the contents of the stack, for debugging.  */
  if (!purify_flag)
    {
      i = &stack_top_variable - stack_bottom;
      if (i < 0) i = -i;
      if (i < MAX_SAVE_STACK)
	{
          BLOCK_INPUT;
	  if (stack_copy == 0)
	    stack_copy = (char *) malloc (stack_copy_size = i);
	  else if (stack_copy_size < i)
	    stack_copy = (char *) realloc (stack_copy, (stack_copy_size = i));
          UNBLOCK_INPUT;
	  if (stack_copy)
	    {
	      if ((int) (&stack_top_variable - stack_bottom) > 0)
		memcpy (stack_copy, stack_bottom, i);
	      else
		memcpy (stack_copy, &stack_top_variable, i);
	    }
	}
    }
#endif /* MAX_SAVE_STACK > 0 */

  if (!noninteractive)
    {
#ifdef HAVE_X_WINDOWS
      if (!NILP (Vgc_message) &&
	  !STRINGP (Vgc_message))
	cursor_changed = x_show_gc_cursor (s, Vgc_message);

      if (pre_hook_error_p || !cursor_changed)
#endif
	{
	  char *msg = (STRINGP (Vgc_message)
		       ? GETTEXT ((char *) XSTRING (Vgc_message)->data)
		       : GETTEXT ((char *) gc_default_message));
	  if (pre_hook_error_p) Fding (Qnil, Qnil);
	  message ("%s...%s", msg,
		   (pre_hook_error_p ? GETTEXT ("  ERROR in pre-gc-hook!") : ""));
	}
    }

  /* Do some totally ad-hoc resource clearing. */
  /* >>> generalize this? */
  clear_event_resource ();
  clear_output_stream_resource ();

  /* Mark all the special slots that serve as the roots of accessibility. */
  {
    struct gcpro *tail;
    struct catchtag *catch;
    struct handler *handler;
    struct backtrace *backlist;
    struct specbinding *bind;

    for (i = 0; i < staticidx; i++)
      mark_object (*(staticvec[i]));

    for (tail = gcprolist; tail; tail = tail->next)
      {
	for (i = 0; i < tail->nvars; i++)
	  mark_object (tail->var[i]);
      }

    for (bind = specpdl; bind != specpdl_ptr; bind++)
      {
	mark_object (bind->symbol);
	mark_object (bind->old_value);
      }

    for (catch = catchlist; catch; catch = catch->next)
      {
	mark_object (catch->tag);
	mark_object (catch->val);
      }

    for (handler = handlerlist; handler; handler = handler->next)
      {
	mark_object (handler->handlers);
	mark_object (handler->handler_arg);
      }

    for (backlist = backtrace_list; backlist; backlist = backlist->next)
      {
	int nargs = backlist->nargs;

	mark_object (*backlist->function);
	if (nargs == UNEVALLED || nargs == MANY)
	  mark_object (backlist->args[0]);
	else
	  for (i = 0; i < nargs; i++)
	    mark_object (backlist->args[i]);
      }

    mark_redisplay (mark_object);
  }

  gc_sweep ();

  consing_since_gc = 0;
  if (gc_cons_threshold < 10000)
    gc_cons_threshold = 10000;

  gc_in_progress = 0;

  post_hook_error_p = !NILP (call_gc_hook (Qpost_gc_hook));

  if (!noninteractive)
    {
#ifdef HAVE_X_WINDOWS
      if (cursor_changed)
	x_show_gc_cursor (s, Qnil);

      if (pre_hook_error_p ||
	  post_hook_error_p ||
	  !cursor_changed)
#endif
	{
	  char *msg = (STRINGP (Vgc_message)
		       ? GETTEXT ((char *) XSTRING (Vgc_message)->data)
		       : GETTEXT ((char *) gc_default_message));
	  if (post_hook_error_p) Fding (Qnil, Qnil);
	  if (post_hook_error_p)
	    message (GETTEXT ("%s... done; ERROR in post-gc-hook!"), msg);
	  else if (pre_hook_error_p)
	    message (GETTEXT ("%s... done; ERROR in pre-gc-hook!"), msg);
	  else if (omessage)
	    message ("%s", omessage);
	  else if (minibuf_level != 0)
            clear_message (1);
          else
            message (GETTEXT ("%s... done"), msg);
	}
    }

  if (!breathing_space)
    {
      BLOCK_INPUT;
      breathing_space = (void *) malloc (4096 - MALLOC_OVERHEAD);
      UNBLOCK_INPUT;
    }

  return;
}

#ifdef EMACS_BTL
 /* This isn't actually called.  BTL recognises the stack frame of the top
    of the garbage collector by noting that PC is between &garbage_collect_1
    and &BTL_after_garbage_collect_1_stub.  So this fn must be right here.
    There's not any other way to know the address of the end of a function.
  */
void BTL_after_garbage_collect_1_stub () { abort (); }
#endif

/* Debugging aids.  */

static Lisp_Object
gc_plist_hack (CONST char *name, int value, Lisp_Object tail)
{
  /* C doesn't have local functions (or closures, or GC, or readable syntax,
     or portable numeric datatypes, or bit-vectors, or characters, or
     arrays, or exceptions, or ...) */
  return (cons3 (intern (name), make_number (value), tail));
}

#define HACK_O_MATIC(type, name, pl) \
  { \
    int s = 0; \
    struct type##_block *x = current_##type##_block; \
    while (x) { s += sizeof (*x) + MALLOC_OVERHEAD; x = x->prev; } \
    (pl) = gc_plist_hack ((name), s, (pl)); \
  }

DEFUN ("garbage-collect", Fgarbage_collect, Sgarbage_collect, 0, 0, "",
  "Reclaim storage for Lisp objects no longer needed.\n\
Returns info on amount of space in use:\n\
 ((USED-CONSES . FREE-CONSES) (USED-SYMS . FREE-SYMS)\n\
  (USED-MARKERS . FREE-MARKERS) USED-STRING-CHARS USED-VECTOR-SLOTS\n\
  PLIST) \n\
  where `PLIST' is a list of alternating keyword/value pairs providing\n\
  more detailed information.\n\
Garbage collection happens automatically if you cons more than\n\
`gc-cons-threshold' bytes of Lisp data since previous garbage collection.")
  ()
{
  Lisp_Object pl = Qnil;
  Lisp_Object ret[6];
  int i;

  garbage_collect_1 ();

  for (i = 0; i < last_lrecord_type_index_assigned; i++)
    {
      if (lcrecord_stats[i].bytes_in_use != 0 
          || lcrecord_stats[i].bytes_freed != 0)
        {
          char buf [255];
          const char *name = lrecord_implementations_table[i]->name;
          sprintf (buf, "%s-storage", name);
          pl = gc_plist_hack (buf, lcrecord_stats[i].bytes_in_use, pl);
          sprintf (buf, "%ss-freed", name);
          if (lcrecord_stats[i].instances_freed != 0)
            pl = gc_plist_hack (buf, lcrecord_stats[i].instances_freed, pl);
          sprintf (buf, "%ss-used", name);
          pl = gc_plist_hack (buf, lcrecord_stats[i].instances_in_use, pl);
        }
    }

  HACK_O_MATIC (extent_replica, "extent-duplicate-storage", pl);
  pl = gc_plist_hack ("extent-duplicates-free", 
                      gc_count_extent_replicas_freelist, pl);
  pl = gc_plist_hack ("extent-duplicates-used", 
                      gc_count_extent_replicas_in_use, pl);
  HACK_O_MATIC (extent, "extent-storage", pl);
  pl = gc_plist_hack ("extents-free", gc_count_extents_freelist, pl);
  pl = gc_plist_hack ("extents-used", gc_count_extents_in_use, pl);
  HACK_O_MATIC (event, "event-storage", pl);
  pl = gc_plist_hack ("events-free", gc_count_events_freelist, pl);
  pl = gc_plist_hack ("events-used", gc_count_events_in_use, pl);
  HACK_O_MATIC (marker, "marker-storage", pl);
  pl = gc_plist_hack ("markers-free", gc_count_markers_freelist, pl);
  pl = gc_plist_hack ("markers-used", gc_count_markers_in_use, pl);
#ifdef LISP_FLOAT_TYPE
  HACK_O_MATIC (float, "float-storage", pl);
  pl = gc_plist_hack ("floats-free", gc_count_floats_freelist, pl);
  pl = gc_plist_hack ("floats-used", gc_count_floats_in_use, pl);
#endif /* LISP_FLOAT_TYPE */
  HACK_O_MATIC (string, "string-header-storage", pl);
  pl = gc_plist_hack ("long-strings-total-length", 
                      gc_count_string_total_size
		      - gc_count_short_string_total_size, pl);
  HACK_O_MATIC (string_chars, "short-string-storage", pl);
  pl = gc_plist_hack ("short-strings-total-length",
                      gc_count_short_string_total_size, pl);
  pl = gc_plist_hack ("strings-free", gc_count_strings_freelist, pl);
  pl = gc_plist_hack ("long-strings-used", 
                      gc_count_strings_in_use
		      - gc_count_short_strings_in_use, pl);
  pl = gc_plist_hack ("short-strings-used", 
                      gc_count_short_strings_in_use, pl);

  HACK_O_MATIC (bytecode, "bytecode-storage", pl);
  pl = gc_plist_hack ("bytecodes-free", gc_count_bytecodes_freelist, pl);
  pl = gc_plist_hack ("bytecodes-used", gc_count_bytecodes_in_use, pl);

  pl = gc_plist_hack ("vector-storage", gc_count_vector_storage, pl);
  pl = gc_plist_hack ("vectors-total-length", 
                      gc_count_vector_total_size, pl);
  pl = gc_plist_hack ("vectors-used", gc_count_vectors_used, pl);

  HACK_O_MATIC (symbol, "symbol-storage", pl);
  pl = gc_plist_hack ("symbols-free", gc_count_symbols_freelist, pl);
  pl = gc_plist_hack ("symbols-used", gc_count_symbols_in_use, pl);

  HACK_O_MATIC (cons, "cons-storage", pl);
  pl = gc_plist_hack ("conses-free", gc_count_conses_freelist, pl);
  pl = gc_plist_hack ("conses-used", gc_count_conses_in_use, pl);

  /* The things we do for backwards-compatibility */
  ret[0] = Fcons (make_number (gc_count_conses_in_use),
                  make_number (gc_count_conses_freelist));
  ret[1] = Fcons (make_number (gc_count_symbols_in_use),
                  make_number (gc_count_symbols_freelist));
  ret[2] = Fcons (make_number (gc_count_markers_in_use),
                  make_number (gc_count_markers_freelist));
  ret[3] = make_number (gc_count_string_total_size);
  ret[4] = make_number (gc_count_vector_total_size);
  ret[5] = pl;
  return (Flist (6, ret));
}
#undef HACK_O_MATIC

DEFUN ("consing-since-gc", Fconsing_since_gc, Sconsing_since_gc, 0, 0, "",
  "Return the number of bytes consed since the last garbage collection.")
  ()
{
  return (make_number (consing_since_gc));
}
  
DEFUN ("memory-limit", Fmemory_limit, Smemory_limit, 0, 0, "",
  "Return the address of the last byte Emacs has allocated, divided by 1024.\n\
This may be helpful in debugging Emacs's memory usage.\n\
The value is divided by 1024 to make sure it will fit in a lisp integer.")
  ()
{
  return (make_number ((LISP_WORD_TYPE) sbrk (0) / 1024));
}


/* Initialization */

void
init_alloc_once ()
{
  int iii;

#ifdef PURESTAT
  for (iii = 0; iii < countof (purestats); iii++)
  {
    if (! purestats[iii]) continue;
    purestats[iii]->nobjects = 0;
    purestats[iii]->nbytes = 0;
  }
  purecopying_for_bytecode = 0;
#endif

  last_lrecord_type_index_assigned = -1;
  for (iii = 0; iii < countof (lrecord_implementations_table); iii++)
  {
    lrecord_implementations_table[iii] = 0;
  }
  
  symbols_initialised = 0;

  gc_generation_number[0] = 0;
  /* purify_flag 1 is correct even if CANNOT_DUMP.
   * loadup.el will set to nil at end. */
  purify_flag = 1;
  pureptr = 0;
  pure_lossage = 0;
  breathing_space = 0;
  XSET (all_vectors, Lisp_Int, 0); /* Qzero may not be set yet. */
  XSET (Vgc_message, Lisp_Int, 0);
  all_lcrecords = 0;
  ignore_malloc_warnings = 1;
  init_string_alloc ();
  init_cons_alloc ();
  init_symbol_alloc ();
  init_bytecode_alloc ();
#ifdef LISP_FLOAT_TYPE
  init_float_alloc ();
#endif /* LISP_FLOAT_TYPE */
#ifndef standalone
  init_marker_alloc ();
  init_extent_alloc ();
  init_event_alloc ();
#endif
  ignore_malloc_warnings = 0;
  gcprolist = 0;
  staticidx = 0;
  consing_since_gc = 0;
  gc_cons_threshold = 100000;
#ifdef VIRT_ADDR_VARIES
  malloc_sbrk_unused = 1<<22;	/* A large number */
  malloc_sbrk_used = 100000;	/* as reasonable as any number */
#endif /* VIRT_ADDR_VARIES */
}

void
init_alloc ()
{
  gcprolist = 0;
}

void
syms_of_alloc ()
{
  DEFVAR_INT ("gc-cons-threshold", &gc_cons_threshold,
    "*Number of bytes of consing between garbage collections.\n\
Garbage collection can happen automatically once this many bytes have been\n\
allocated since the last garbage collection.  All data types count.\n\n\
Garbage collection happens automatically when `eval' or `funcall' are\n\
called.  (Note that `funcall' is called implicitly as part of evaluation.)\n\
By binding this temporarily to a large number, you can effectively\n\
prevent garbage collection during a part of the program.");

  DEFVAR_INT ("pure-bytes-used", &pureptr,
    "Number of bytes of sharable Lisp data allocated so far.");

#if 0
  DEFVAR_INT ("data-bytes-used", &malloc_sbrk_used,
    "Number of bytes of unshared memory allocated in this session.");

  DEFVAR_INT ("data-bytes-free", &malloc_sbrk_unused,
    "Number of bytes of unshared memory remaining available in this session.");
#endif

  DEFVAR_BOOL ("purify-flag", &purify_flag,
    "Non-nil means loading Lisp code in order to dump an executable.\n\
This means that certain objects should be allocated in shared (pure) space.");

  defsymbol (&Qpre_gc_hook, "pre-gc-hook");
  DEFVAR_LISP ("pre-gc-hook", &Vpre_gc_hook,
       "Function or functions to be run just before each garbage collection.\n\
Interrupts, garbage collection, and errors are inhibited while this hook\n\
runs, so be extremely careful in what you add here.  In particular, avoid\n\
consing, and do not interact with the user.");
  Vpre_gc_hook = Qnil;

  defsymbol (&Qpost_gc_hook, "post-gc-hook");
  DEFVAR_LISP ("post-gc-hook", &Vpost_gc_hook,
       "Function or functions to be run just after each garbage collection.\n\
Interrupts, garbage collection, and errors are inhibited while this hook\n\
runs, so be extremely careful in what you add here.  In particular, avoid\n\
consing, and do not interact with the user.");
  Vpost_gc_hook = Qnil;

  DEFVAR_LISP ("gc-message", &Vgc_message,
    "What to display to indicate that a garbage collection is in progress.\n\
If this is a string, it is printed in the echo area.\n\
If this is a cursor object, the mouse pointer of the selected screen is\n\
changed to that cursor for the duration of the garbage collection.\n\
See the variable `x-gc-pointer-shape', which is used to control this.");
  Vgc_message = make_pure_string ((CONST char *) gc_default_message,
				  countof (gc_default_message)-1, 1);

  gc_currently_forbidden = 0;
  gc_hooks_inhibited = 0;

  defsubr (&Scons);
  defsubr (&Slist);
  defsubr (&Svector);
  defsubr (&Smake_byte_code);
  defsubr (&Smake_list);
  defsubr (&Smake_vector);
  defsubr (&Smake_string);
  defsubr (&Smake_symbol);
  defsubr (&Smake_marker);
  defsubr (&Spurecopy);
  defsubr (&Sgarbage_collect);
  defsubr (&Smemory_limit);
  defsubr (&Sconsing_since_gc);
}
