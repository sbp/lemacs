/* Storage allocation and gc for GNU Emacs Lisp interpreter.
   Copyright (C) 1985, 1986, 1988 Free Software Foundation, Inc.

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


#include "config.h"
#include "lisp.h"
#ifndef standalone
#include "buffer.h"
#include "extents.h"
#include "window.h"
#include "events.h"
#include "keymap.h"
#ifdef HAVE_X_WINDOWS
#include "xterm.h"
#ifdef MULTI_SCREEN
#include "screen.h"
#endif	/* MULTI_SCREEN */
#endif	/* HAVE_X_WINDOWS */
#endif

#include "backtrace.h"

#define max(A,B) ((A) > (B) ? (A) : (B))

/* Macro to verify that storage intended for Lisp objects is not
   out of range to fit in the space for a pointer.
   ADDRESS is the start of the block, and SIZE
   is the amount of space within which objects can start.  */
#define VALIDATE_LISP_STORAGE(address, size)			\
  {								\
    Lisp_Object val;						\
    XSET (val, Lisp_Cons, (char *) address + size);		\
    if ((char *) XCONS (val) != (char *) address + size)	\
      {								\
	free (address);						\
	memory_full ();						\
      }								\
  }

/* Number of bytes of consing done since the last gc */
int consing_since_gc;
#ifdef EMACS_BTL
#define INCREMENT_CONS_COUNTER(size) \
{ \
  extern void cadillac_record_backtrace(); \
  int __sz__ = ((int) (size)); \
  consing_since_gc += __sz__; \
  cadillac_record_backtrace (2, __sz__); \
}
#else
#define INCREMENT_CONS_COUNTER(size) (consing_since_gc += (size))
#endif

/* Number of bytes of consing since gc before another gc should be done. */
int gc_cons_threshold;

/* Nonzero during gc */
int gc_in_progress;

/* Nonzero when calling the hooks in Energize-beta */
Lisp_Object Qgc_currently_forbidden;
int gc_currently_forbidden;

#ifndef VIRT_ADDR_VARIES
extern
#endif /* VIRT_ADDR_VARIES */
 int malloc_sbrk_used;

#ifndef VIRT_ADDR_VARIES
extern
#endif /* VIRT_ADDR_VARIES */
 int malloc_sbrk_unused;

/* Two thresholds controlling how much undo information to keep.  */
int undo_threshold;
int undo_high_threshold;

/* Non-nil means defun should do purecopy on the function definition */
Lisp_Object Vpurify_flag;

int pure[PURESIZE / sizeof (int)] = {0,};   /* Force it into data space! */

#define PUREBEG (char *) pure

/* Index in pure at which next pure object will be allocated. */
int pureptr;

/* define this to keep statistics on how much of what is in purespace */
#ifdef PURESTAT
unsigned int purestat [5];
#endif

/* Maximum amount of C stack to save when a GC happens.  */

#ifndef MAX_SAVE_STACK
#define MAX_SAVE_STACK 16000
#endif

/* Buffer in which we save a copy of the C stack at each GC.  */

char *stack_copy;
int stack_copy_size;

/* Non-zero means ignore malloc warnings.  Set during initialization.  */
int ignore_warnings;

Lisp_Object
malloc_warning_1 (str)
     Lisp_Object str;
{
  Fprinc (str, Vstandard_output);
  write_string ("\nKilling some buffers may delay running out of memory.\n", -1);
  write_string ("However, certainly by the time you receive the 95% warning,\n", -1);
  write_string ("you should clean up, kill this Emacs, and start a new one.", -1);
  return Qnil;
}

/* malloc calls this if it finds we are near exhausting storage */
void
malloc_warning (str)
     char *str;
{
  register Lisp_Object val;

  if (ignore_warnings)
    return;

  val = build_string (str);
  internal_with_output_to_temp_buffer (" *Danger*", malloc_warning_1, val, Qnil);
}

/* Called if malloc returns zero */
memory_full ()
{
  error ("Memory exhausted");
}

/* like malloc and realloc but check for no memory left */

long *
xmalloc (size)
     int size;
{
  register long *val;

  val = (long *) malloc (size);

  if (!val && size) memory_full ();
  return val;
}

long *
xrealloc (block, size)
     long *block;
     int size;
{
  register long *val;

  val = (long *) realloc (block, size);

  if (!val && size) memory_full ();
  return val;
}


#define EXTENT_BLOCK_SIZE \
  ((1020 - sizeof (struct extent_block *)) / sizeof (struct extent))

#define DUP_BLOCK_SIZE \
  ((1020 - sizeof (struct dup_block *)) / sizeof (struct extent_replica))

struct extent_block
  {
    struct extent_block *next;
    struct extent extents[EXTENT_BLOCK_SIZE];
  };

struct dup_block
  {
    struct dup_block *next;
    struct extent_replica dups[DUP_BLOCK_SIZE];
  };

struct extent_block *extent_block;
static int extent_block_index;

EXTENT extent_free_list;

struct dup_block *dup_block;
static int dup_block_index;

DUP dup_free_list;

static void
init_extents ()
{
  extent_block
    = (struct extent_block *) malloc (sizeof (struct extent_block));
  extent_block->next = 0;
  bzero (extent_block->extents, sizeof extent_block->extents);
  extent_block_index = 0;
  extent_free_list = 0;

  dup_block
    = (struct dup_block *) malloc (sizeof (struct dup_block));
  dup_block->next = 0;
  bzero (dup_block->dups, sizeof dup_block->dups);
  dup_block_index = 0;
  dup_free_list = 0;
}

EXTENT
make_extent ()
{
  EXTENT val;

  if (extent_free_list)
    {
      val = extent_free_list;
      extent_free_list = extent_free_list->next;
      val->next = 0;
    }
  else
    {
      if (extent_block_index == EXTENT_BLOCK_SIZE)
	{
	  register struct extent_block *newi
	    = (struct extent_block *) malloc (sizeof (struct extent_block));

	  if (!newi)
	    memory_full ();

          bzero ((char *) newi, sizeof (struct extent_block));

	  VALIDATE_LISP_STORAGE (newi, sizeof *newi);
	  newi->next = extent_block;
	  extent_block = newi;
	  extent_block_index = 0;
	}
      val = &extent_block->extents[extent_block_index++];
    }

  INCREMENT_CONS_COUNTER (sizeof (struct extent));


  return val;
}

DUP
make_extent_replica ()
{
  DUP val;

  if (dup_free_list)
    {
      val = dup_free_list;
      dup_free_list = (DUP) dup_free_list->extent;
      val->extent = 0;
    }
  else
    {
      if (dup_block_index == DUP_BLOCK_SIZE)
	{
	  struct dup_block *newd
	    = (struct dup_block *) malloc (sizeof (struct dup_block));

	  if (!newd)
	    memory_full ();

          bzero ((char *) newd, sizeof(struct dup_block));

	  VALIDATE_LISP_STORAGE (newd, sizeof(struct dup_block));
	  newd->next = dup_block;
	  dup_block = newd;
	  dup_block_index = 0;
	}
      val = &dup_block->dups[dup_block_index++];
    }

  INCREMENT_CONS_COUNTER (sizeof (struct extent_replica));

  return val;
}


#ifdef LISP_FLOAT_TYPE
/* Allocation of float cells, just like conses */
/* We store float cells inside of float_blocks, allocating a new
   float_block with malloc whenever necessary.  Float cells reclaimed by
   GC are put on a free list to be reallocated before allocating
   any new float cells from the latest float_block.

   Each float_block is just under 1020 bytes long,
   since malloc really allocates in units of powers of two
   and uses 4 bytes for its own overhead. */

#define FLOAT_BLOCK_SIZE \
  ((1020 - sizeof (struct float_block *)) / sizeof (struct Lisp_Float))

struct float_block
  {
    struct float_block *next;
    struct Lisp_Float floats[FLOAT_BLOCK_SIZE];
  };

struct float_block *float_block;
int float_block_index;

struct Lisp_Float *float_free_list;

void
init_float ()
{
  float_block = (struct float_block *) malloc (sizeof (struct float_block));
  float_block->next = 0;
  bzero (float_block->floats, sizeof float_block->floats);
  float_block_index = 0;
  float_free_list = 0;
}

/* Explicitly free a float cell.  */
free_float (ptr)
     struct Lisp_Float *ptr;
{
  XFASTINT (ptr->type) = (int) float_free_list;
  float_free_list = ptr;
}

Lisp_Object
make_float (float_value)
     double float_value;
{
  register Lisp_Object val;

  if (float_free_list)
    {
      XSET (val, Lisp_Float, float_free_list);
      float_free_list = (struct Lisp_Float *) XFASTINT (float_free_list->type);
    }
  else
    {
      if (float_block_index == FLOAT_BLOCK_SIZE)
	{
	  register struct float_block *new = (struct float_block *) malloc (sizeof (struct float_block));
	  if (!new) memory_full ();
	  VALIDATE_LISP_STORAGE (new, sizeof *new);
	  new->next = float_block;
	  float_block = new;
	  float_block_index = 0;
	}
      XSET (val, Lisp_Float, &float_block->floats[float_block_index++]);
    }
  XFLOAT (val)->data = float_value;
  XFLOAT (val)->type = 0;	/* bug chasing -wsr */
  INCREMENT_CONS_COUNTER (sizeof (struct Lisp_Float));
  return val;
}

#endif /* LISP_FLOAT_TYPE */

/* Allocation of cons cells */
/* We store cons cells inside of cons_blocks, allocating a new
   cons_block with malloc whenever necessary.  Cons cells reclaimed by
   GC are put on a free list to be reallocated before allocating
   any new cons cells from the latest cons_block.

   Each cons_block is just under 1020 bytes long,
   since malloc really allocates in units of powers of two
   and uses 4 bytes for its own overhead. */

#define CONS_BLOCK_SIZE \
  ((1020 - sizeof (struct cons_block *)) / sizeof (struct Lisp_Cons))

struct cons_block
  {
    struct cons_block *next;
    struct Lisp_Cons conses[CONS_BLOCK_SIZE];
  };

struct cons_block *cons_block;
int cons_block_index;

struct Lisp_Cons *cons_free_list;

void
init_cons ()
{
  cons_block = (struct cons_block *) malloc (sizeof (struct cons_block));
  cons_block->next = 0;
  bzero (cons_block->conses, sizeof cons_block->conses);
  cons_block_index = 0;
  cons_free_list = 0;
}

/* Explicitly free a cons cell.  */
free_cons (ptr)
     struct Lisp_Cons *ptr;
{
  XFASTINT (ptr->car) = (int) cons_free_list;
  cons_free_list = ptr;
}

DEFUN ("cons", Fcons, Scons, 2, 2, 0,
  "Create a new cons, give it CAR and CDR as components, and return it.")
  (car, cdr)
     Lisp_Object car, cdr;
{
  register Lisp_Object val;

  if (cons_free_list)
    {
      XSET (val, Lisp_Cons, cons_free_list);
      cons_free_list = (struct Lisp_Cons *) XFASTINT (cons_free_list->car);
    }
  else
    {
      if (cons_block_index == CONS_BLOCK_SIZE)
	{
	  register struct cons_block *new = (struct cons_block *) malloc (sizeof (struct cons_block));
	  if (!new) memory_full ();
	  VALIDATE_LISP_STORAGE (new, sizeof *new);
	  new->next = cons_block;
	  cons_block = new;
	  cons_block_index = 0;
	}
      XSET (val, Lisp_Cons, &cons_block->conses[cons_block_index++]);
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
     register Lisp_Object *args;
{
  register Lisp_Object len, val, val_tail;

  XFASTINT (len) = nargs;
  val = Fmake_list (len, Qnil);
  val_tail = val;
  while (!NILP (val_tail))
    {
      XCONS (val_tail)->car = *args++;
      val_tail = XCONS (val_tail)->cdr;
    }
  return val;
}

DEFUN ("make-list", Fmake_list, Smake_list, 2, 2, 0,
  "Return a newly created list of length LENGTH, with each element being INIT.")
  (length, init)
     register Lisp_Object length, init;
{
  register Lisp_Object val;
  register int size;

  if (XTYPE (length) != Lisp_Int || XINT (length) < 0)
    length = wrong_type_argument (Qnatnump, length);
  size = XINT (length);

  val = Qnil;
  while (size-- > 0)
    val = Fcons (init, val);
  return val;
}

/* Allocation of vectors */

struct Lisp_Vector *all_vectors;

DEFUN ("make-vector", Fmake_vector, Smake_vector, 2, 2, 0,
  "Return a newly created vector of length LENGTH, with each element being INIT.\n\
See also the function `vector'.")
  (length, init)
     register Lisp_Object length, init;
{
  register int sizei, index;
  register Lisp_Object vector;
  register struct Lisp_Vector *p;

  if (XTYPE (length) != Lisp_Int || XINT (length) < 0)
    length = wrong_type_argument (Qnatnump, length);
  sizei = XINT (length);

  p = (struct Lisp_Vector *) malloc (sizeof (struct Lisp_Vector) + (sizei - 1) * sizeof (Lisp_Object));
  if (p == 0)
    memory_full ();
  VALIDATE_LISP_STORAGE (p, 0);

  XSET (vector, Lisp_Vector, p);
  INCREMENT_CONS_COUNTER 
    (sizeof (struct Lisp_Vector) + (sizei - 1) * sizeof (Lisp_Object));

  p->size = sizei;
  p->next = all_vectors;
  all_vectors = p;

  for (index = 0; index < sizei; index++)
    p->contents[index] = init;

  return vector;
}

DEFUN ("vector", Fvector, Svector, 0, MANY, 0,
  "Return a newly created vector with specified arguments as elements.\n\
Any number of arguments, even zero arguments, are allowed.")
  (nargs, args)
     register int nargs;
     Lisp_Object *args;
{
  register Lisp_Object len, val;
  register int index;
  register struct Lisp_Vector *p;

  XFASTINT (len) = nargs;
  val = Fmake_vector (len, Qnil);
  p = XVECTOR (val);
  for (index = 0; index < nargs; index++)
    p->contents[index] = args[index];
  return val;
}

DEFUN ("make-byte-code", Fmake_byte_code, Smake_byte_code, 4, MANY, 0,
  "Create a byte-code object with specified arguments as elements.\n\
At least four arguments are required; only six have any significance.")
  (nargs, args)
     register int nargs;
     Lisp_Object *args;
{
  register Lisp_Object len, val;
  register int index;
  register struct Lisp_Vector *p;

  XFASTINT (len) = nargs;
  if (!NILP (Vpurify_flag))
#ifdef PURESTAT
    val = make_pure_vector (len, Lisp_Compiled);
#else
    val = make_pure_vector (len);
#endif
  else
    val = Fmake_vector (len, Qnil);
  p = XVECTOR (val);
  for (index = 0; index < nargs; index++)
    {
      if (!NILP (Vpurify_flag))
	args[index] = Fpurecopy (args[index]);
      p->contents[index] = args[index];
    }
  XSETTYPE (val, Lisp_Compiled);
  return val;
}

/* Allocation of symbols.
   Just like allocation of conses!

   Each symbol_block is just under 1020 bytes long,
   since malloc really allocates in units of powers of two
   and uses 4 bytes for its own overhead. */

#define SYMBOL_BLOCK_SIZE \
  ((1020 - sizeof (struct symbol_block *)) / sizeof (struct Lisp_Symbol))

struct symbol_block
  {
    struct symbol_block *next;
    struct Lisp_Symbol symbols[SYMBOL_BLOCK_SIZE];
  };

struct symbol_block *symbol_block;
int symbol_block_index;

struct Lisp_Symbol *symbol_free_list;

void
init_symbol ()
{
  symbol_block = (struct symbol_block *) malloc (sizeof (struct symbol_block));
  symbol_block->next = 0;
  bzero (symbol_block->symbols, sizeof symbol_block->symbols);
  symbol_block_index = 0;
  symbol_free_list = 0;
}

DEFUN ("make-symbol", Fmake_symbol, Smake_symbol, 1, 1, 0,
  "Return a newly allocated uninterned symbol whose name is NAME.\n\
Its value and function definition are void, and its property list is nil.")
  (str)
     Lisp_Object str;
{
  register Lisp_Object val;
  register struct Lisp_Symbol *p;

  CHECK_STRING (str, 0);

  if (symbol_free_list)
    {
      XSET (val, Lisp_Symbol, symbol_free_list);
      symbol_free_list
	= (struct Lisp_Symbol *) XFASTINT (symbol_free_list->value);
    }
  else
    {
      if (symbol_block_index == SYMBOL_BLOCK_SIZE)
	{
	  struct symbol_block *new = (struct symbol_block *) malloc (sizeof (struct symbol_block));
	  if (!new) memory_full ();
	  new->next = symbol_block;
	  VALIDATE_LISP_STORAGE (new, sizeof *new);
	  symbol_block = new;
	  symbol_block_index = 0;
	}
      XSET (val, Lisp_Symbol, &symbol_block->symbols[symbol_block_index++]);
    }
  p = XSYMBOL (val);
  p->name = XSTRING (str);
  p->plist = Qnil;
  p->value = Qunbound;
  p->function = Qunbound;
  p->next = 0;
  INCREMENT_CONS_COUNTER (sizeof (struct Lisp_Symbol));
  return val;
}

/* Allocation of markers.
   Works like allocation of conses. */

#define MARKER_BLOCK_SIZE \
  ((1020 - sizeof (struct marker_block *)) / sizeof (struct Lisp_Marker))

struct marker_block
  {
    struct marker_block *next;
    struct Lisp_Marker markers[MARKER_BLOCK_SIZE];
  };

struct marker_block *marker_block;
int marker_block_index;

struct Lisp_Marker *marker_free_list;

void
init_marker ()
{
  marker_block = (struct marker_block *) malloc (sizeof (struct marker_block));
  marker_block->next = 0;
  bzero (marker_block->markers, sizeof marker_block->markers);
  marker_block_index = 0;
  marker_free_list = 0;
}

DEFUN ("make-marker", Fmake_marker, Smake_marker, 0, 0, 0,
  "Return a newly allocated marker which does not point at any place.")
  ()
{
  register Lisp_Object val;
  register struct Lisp_Marker *p;

  if (marker_free_list)
    {
      XSET (val, Lisp_Marker, marker_free_list);
      marker_free_list
	= (struct Lisp_Marker *) XFASTINT (marker_free_list->chain);
    }
  else
    {
      if (marker_block_index == MARKER_BLOCK_SIZE)
	{
	  struct marker_block *new = (struct marker_block *) malloc (sizeof (struct marker_block));
	  if (!new) memory_full ();
	  VALIDATE_LISP_STORAGE (new, sizeof *new);
	  new->next = marker_block;
	  marker_block = new;
	  marker_block_index = 0;
	}
      XSET (val, Lisp_Marker, &marker_block->markers[marker_block_index++]);
    }
  p = XMARKER (val);
  p->buffer = 0;
  p->bufpos = 0;
  p->chain = Qnil;
  INCREMENT_CONS_COUNTER (sizeof (struct Lisp_Marker));
  return val;
}

/* Allocation of strings */

/* The data for "short" strings generally resides inside of structs of type 
   string_chars_block. The Lisp_String structure is allocated just like any 
   other Lisp object (except for vectors), and these are freelisted when
   the get garbage collected. The data for short strings get compacted,
   but the data for large strings do not. 

   Previously Lisp_String structures were relocated, but this caused a lot
   of bus-errors because the C code didn't include enough GCPRO's for
   strings (since EVERY REFERENCE to a short string needed to be GCPRO'd so
   that the reference would get relocated).

   This new method makes things somewhat bigger, but it is MUCH safer.  */


/* If SIZE is the length of a string, this returns how many bytes
   the string occupies in a string_chars_block (including padding).  */
#define PAD ((sizeof (struct Lisp_String *)) - 1)
#define ROUND_UP_STRING_SIZE(s) (((s) + 1 + PAD) & ~PAD)
#define STRING_FULLSIZE(size) \
ROUND_UP_STRING_SIZE ((size) + sizeof (struct Lisp_String *))

#define STRING_BLOCK_SIZE \
((1020 - sizeof (struct string_block *)) / sizeof (struct Lisp_String))
/* String blocks contain this many useful bytes.
   8188 is power of 2, minus 4 for malloc overhead. */
#define STRING_CHARS_BLOCK_SIZE \
(8188 - ((2 * sizeof (struct string_chars_block *))+ sizeof (int)))

struct string_chars
{
  struct Lisp_String *string;
  unsigned char chars[1];
};

#define SLOT_OFFSET(type, slot_name) \
((unsigned) (((char *) (&(((type *)0)->slot_name))) - ((char *) 0)))
#define CHARS_TO_STRING_CHAR(x) \
((struct string_chars *)\
 (((char *) (x)) - (SLOT_OFFSET(struct string_chars, chars))))

/* Block header for small strings. */
struct string_chars_block
{
  struct string_chars_block *next;
  struct string_chars_block *prev;
  int pos;
  unsigned char chars[STRING_CHARS_BLOCK_SIZE];
};

struct string_block
{
  struct string_block *next;
  struct Lisp_String strings[STRING_BLOCK_SIZE];
};

struct string_block *string_block;
static int string_block_index;
static struct Lisp_String *string_free_list;

struct string_chars_block *current_string_chars_block;
struct string_chars_block *first_string_chars_block;

#define NONRELOCATING_STRING_SIZE(size) ((size) >= 1020)
#define BIG_STRING_SIZE(size) (NONRELOCATING_STRING_SIZE(size))

static void
init_strings ()
{
  string_block = (struct string_block *) malloc (sizeof (struct string_block));
  string_block->next = 0;
  bzero (string_block->strings, sizeof string_block->strings);
  string_block_index = 0;
  string_free_list = 0;

  first_string_chars_block = 
    (struct string_chars_block *) malloc (sizeof (struct string_chars_block));
  current_string_chars_block = first_string_chars_block;
  current_string_chars_block->prev = 0;
  current_string_chars_block->next = 0;
  current_string_chars_block->pos = 0;
}

static struct Lisp_String *
make_string_internal ()
{
  struct Lisp_String *val;

  if (string_free_list)
    {
      val = string_free_list;
      string_free_list = (struct Lisp_String *)string_free_list->dup_list;
      val->dup_list = 0;
    }
  else
    {
      if (string_block_index == STRING_BLOCK_SIZE)
	{
	  struct string_block *new_sb
	    = (struct string_block *) malloc (sizeof (struct string_block));

	  if (!new_sb)
	    memory_full ();

          bzero ((char *) new_sb, sizeof (struct string_block));

	  VALIDATE_LISP_STORAGE (new_sb, sizeof *new_sb);
	  new_sb->next = string_block;
	  string_block = new_sb;
	  string_block_index = 0;
	}
      val = &string_block->strings[string_block_index++];
    }

  return val;
}


static struct string_chars *
allocate_string_chars (size, fullsize)
     int size;
     int fullsize;
{
  struct string_chars *s_chars;
  
  if (BIG_STRING_SIZE (size))
    {
      s_chars = (struct string_chars *) malloc (fullsize);
      if (!s_chars)
	memory_full ();
    }
  else if (fullsize <=
           (STRING_CHARS_BLOCK_SIZE - current_string_chars_block->pos))
    {
      /* This string can fit in the current string chars block */
      s_chars = 
        (struct string_chars *) 
          (current_string_chars_block->chars + 
           current_string_chars_block->pos);
      current_string_chars_block->pos += fullsize;
    }
  else
    {
      /* Make a new current string chars block */
      struct string_chars_block *new = 
        (struct string_chars_block *) 
          malloc (sizeof (struct string_chars_block));
      if (!new)
	memory_full ();

      current_string_chars_block->next = new;
      new->prev = current_string_chars_block;
      new->next = 0;
      current_string_chars_block = new;
      new->pos = fullsize;
      s_chars = (struct string_chars *) current_string_chars_block->chars;
    }

  return s_chars;
}


static Lisp_Object
make_uninit_string (length)
     int length;
{
  struct Lisp_String *string;
  struct string_chars *s_chars;
  Lisp_Object val;
  int fullsize = STRING_FULLSIZE (length);

  if ((length < 0) || (fullsize <= 0))
    abort ();

  string = make_string_internal();
  s_chars = allocate_string_chars (length, fullsize);
  s_chars->string = string;

  string->size = length;
  string->data = &(s_chars->chars[0]);
  string->dup_list = Qnil;

  string->data[length] = 0;

  XSET (val, Lisp_String, string);

  INCREMENT_CONS_COUNTER (sizeof (struct Lisp_String) + fullsize);
  return val;
}

DEFUN ("make-string", Fmake_string, Smake_string, 2, 2, 0,
  "Return a newly created string of length LENGTH, with each element being INIT.\n\
Both LENGTH and INIT must be numbers.")
  (length, init)
     Lisp_Object length, init;
{
  register Lisp_Object val;
  register unsigned char *p, *end, c;

  if (XTYPE (length) != Lisp_Int || XINT (length) < 0)
    length = wrong_type_argument (Qnatnump, length);
  CHECK_NUMBER (init, 1);
  val = make_uninit_string (XINT (length));
  c = XINT (init);
  p = XSTRING (val)->data;
  end = p + XSTRING (val)->size;
  while (p < end)
    *p++ = c;
  return val;
}

Lisp_Object
make_string_from_buffer (buf, index, length)
     struct buffer *buf;
     int index, length;
{
  Lisp_Object val = make_uninit_string (length);

  XSTRING (val)->dup_list = replicate_extents (index, length, buf);

  if (index < BUF_GPT (buf) && index + length > BUF_GPT (buf))
    move_gap (index);
  bcopy (BUF_CHAR_ADDRESS (buf, index), XSTRING (val)->data, length);
  return val;
}

Lisp_Object
make_string (contents, length)
     char *contents;
     int length;
{
  register Lisp_Object val;
  val = make_uninit_string (length);
  bcopy (contents, XSTRING (val)->data, length);
  return val;
}

Lisp_Object
build_string (str)
     char *str;
{
  return make_string (str, strlen (str));
}


/* Must get an error if pure storage is full,
 since if it cannot hold a large string
 it may be able to hold conses that point to that string;
 then the string is not protected from gc. */

int tl, tf, ts;

Lisp_Object
make_pure_string (data, length)
     char *data;
     int length;
{
  Lisp_Object new;
  unsigned char *chars;
  int size = sizeof (struct Lisp_String) + ROUND_UP_STRING_SIZE (length);

  if (pureptr + size > PURESIZE)
    error ("Pure Lisp storage exhausted");

  XSET (new, Lisp_String, PUREBEG + pureptr);
  chars = (unsigned char *) (PUREBEG + pureptr + sizeof (struct Lisp_String));
  XSTRING (new)->size = length;
  XSTRING (new)->data = chars;
  bcopy (data, XSTRING (new)->data, length);
  XSTRING (new)->data[length] = 0;
  XSTRING (new)->dup_list = Qnil;
  pureptr += ((size + sizeof (int) - 1) / sizeof (int)) * sizeof (int);
#ifdef PURESTAT
  purestat [1] += ((size + sizeof (int) - 1) / sizeof (int)) * sizeof (int);
#endif
  return new;
}

Lisp_Object
pure_cons (car, cdr)
     Lisp_Object car, cdr;
{
  register Lisp_Object new;

  if (pureptr + sizeof (struct Lisp_Cons) > PURESIZE)
    error ("Pure Lisp storage exhausted");
  XSET (new, Lisp_Cons, PUREBEG + pureptr);
  pureptr += sizeof (struct Lisp_Cons);
  XCONS (new)->car = Fpurecopy (car);
  XCONS (new)->cdr = Fpurecopy (cdr);
#ifdef PURESTAT
  purestat [0] += sizeof (struct Lisp_Cons);
#endif
  return new;
}

#ifdef LISP_FLOAT_TYPE

Lisp_Object
make_pure_float (num)
     double num;
{
  register Lisp_Object new;

  /* pure_floats have to be double aligned. */
  pureptr = (pureptr + 0x7) & ~0x7;

  if (pureptr + sizeof (struct Lisp_Float) > PURESIZE)
    error ("Pure Lisp storage exhausted");
  XSET (new, Lisp_Float, PUREBEG + pureptr);
  pureptr += sizeof (struct Lisp_Float);
  XFLOAT (new)->data = num;
  XFLOAT (new)->type = 0;	/* bug chasing -wsr */
#ifdef PURESTAT
  purestat [4] += sizeof (struct Lisp_Float);
#endif
  return new;
}

#endif /* LISP_FLOAT_TYPE */

Lisp_Object
#ifdef PURESTAT
make_pure_vector (len, stat_type)
     int len;
     enum Lisp_Type stat_type;
#else
make_pure_vector (len)
     int len;
#endif
{
  register Lisp_Object new;
  register int size = sizeof (struct Lisp_Vector) + (len - 1) * sizeof (Lisp_Object);

  if (pureptr + size > PURESIZE)
    error ("Pure Lisp storage exhausted");

  XSET (new, Lisp_Vector, PUREBEG + pureptr);
  pureptr += size;
  XVECTOR (new)->size = len;

#ifdef PURESTAT
  purestat [stat_type == Lisp_Compiled ? 3 : 2] += size;
#endif

  return new;
}

DEFUN ("purecopy", Fpurecopy, Spurecopy, 1, 1, 0,
  "Make a copy of OBJECT in pure storage.\n\
Recursively copies contents of vectors and cons cells.\n\
Does not copy symbols.")
  (obj)
     register Lisp_Object obj;
{
  register Lisp_Object new, tem;
  register int i;

  if (NILP (Vpurify_flag))
    return obj;

  if ((PNTR_COMPARISON_TYPE) XPNTR (obj) < (PNTR_COMPARISON_TYPE) ((char *) pure + PURESIZE)
      && (PNTR_COMPARISON_TYPE) XPNTR (obj) >= (PNTR_COMPARISON_TYPE) pure)
    return obj;

#ifdef SWITCH_ENUM_BUG
  switch ((int) XTYPE (obj))
#else
  switch (XTYPE (obj))
#endif
    {
    case Lisp_Marker:
      error ("Attempt to copy a marker to pure storage");

    case Lisp_Cons:
      return pure_cons (XCONS (obj)->car, XCONS (obj)->cdr);

#ifdef LISP_FLOAT_TYPE
    case Lisp_Float:
      return make_pure_float (XFLOAT (obj)->data);
#endif /* LISP_FLOAT_TYPE */

    case Lisp_String:
      return make_pure_string ((char *) XSTRING (obj)->data,
			       XSTRING (obj)->size);

    case Lisp_Compiled:
    case Lisp_Vector:
#ifdef PURESTAT
      new = make_pure_vector (XVECTOR (obj)->size, XTYPE (obj));
#else
      new = make_pure_vector (XVECTOR (obj)->size);
#endif
      for (i = 0; i < XVECTOR (obj)->size; i++)
	{
	  tem = XVECTOR (obj)->contents[i];
	  XVECTOR (new)->contents[i] = Fpurecopy (tem);
	}
      XSETTYPE (new, XTYPE (obj));
      return new;

    default:
      return obj;
    }
}

/* Recording what needs to be marked for gc.  */

struct gcpro *gcprolist;

#define NSTATICS 512

Lisp_Object *staticvec[NSTATICS] = {0};

int staticidx = 0;

/* Put an entry in staticvec, pointing at the variable whose address is given */

void
staticpro (varaddress)
     Lisp_Object *varaddress;
{
  staticvec[staticidx++] = varaddress;
  if (staticidx >= NSTATICS)
    abort ();
}

/* Flags are set during GC in the `size' component of a string or vector.
   On some machines, these flags are defined by the m- file to use
   different bits.

   On vectors, the flag means the vector has been marked.

   On string size field or a reference to a string, the flag means
   there are more entries in the chain. */

#ifndef ARRAY_MARK_FLAG
#define ARRAY_MARK_FLAG ((MARKBIT >> 1) & ~MARKBIT)
#endif /* no ARRAY_MARK_FLAG */

/* Any slot that is a Lisp_Object can point to a string
   and thus can be put on a string's reference-chain
   and thus may need to have its ARRAY_MARK_FLAG set.
   This includes the slots whose markbits are used to mark
   the containing objects.  */

#ifdef EMACS_BTL
void BTL_before_Fgarbage_collect_stub ()
{
  return;
}
#endif

#if ARRAY_MARK_FLAG == MARKBIT
you lose
#endif

int total_conses, total_markers, total_symbols;
int total_vector_size, total_string_size, total_strings, total_short_strings;
int total_free_strings, total_free_conses;
int total_free_markers, total_free_symbols;

#ifdef LISP_FLOAT_TYPE
int total_free_floats, total_floats;
#endif /* LISP_FLOAT_TYPE */

int total_free_events, total_events;

static void mark_object (), mark_buffer ();
static void mark_event (), mark_command_event_queue ();
static void clear_marks (), gc_sweep ();
static void compact_string_chars ();

/* Mark just one extent. */
static void
mark_one_extent (extent)
     EXTENT extent;
{
  if (!EXTENT_MARKED_P (extent))
    {
      MARK_EXTENT (extent);
      mark_object (&(extent->buffer));
    }
}

/* Mark a list of extents. */
static void
mark_extents (extent)
     EXTENT extent;
{
  if (!EXTENT_LIST_MARKED_P (extent))
    {
      EXTENT e = extent;
      while (e)
        {
	  MARK_EXTENT_LIST (e);
	  if (!EXTENT_MARKED_P (e))
	    mark_one_extent (e);
          e = e->next;
        }
    }
}

int total_free_extents, total_extents;
int total_free_dups, total_dups;


#ifdef HAVE_X_WINDOWS
extern unsigned long current_pointer_shape;
#endif



/* Mark reference to a Lisp_Object.  If the object referred to has not been
   seen yet, recursively mark all the references contained in it. */
   
static void
mark_object (objptr)
     Lisp_Object *objptr;
{
  register Lisp_Object obj;

  obj = *objptr;
  XUNMARK (obj);

 loop:

  if ((PNTR_COMPARISON_TYPE) XPNTR (obj) < (PNTR_COMPARISON_TYPE) ((char *) pure + PURESIZE)
      && (PNTR_COMPARISON_TYPE) XPNTR (obj) >= (PNTR_COMPARISON_TYPE) pure)
    return;
#ifdef SWITCH_ENUM_BUG
  switch ((int) XGCTYPE (obj))
#else
  switch (XGCTYPE (obj))
#endif
    {
    case Lisp_String:
      {
	struct Lisp_String *ptr = XSTRING (obj);

        if (!XMARKBIT (ptr->dup_list))
          {
            XMARK(ptr->dup_list);
            mark_object (&ptr->dup_list);
          }
      }
      break;

    case Lisp_Vector:
    case Lisp_Window:
    case Lisp_Process:
    case Lisp_Window_Configuration:
    case Lisp_Compiled:
      {
	register struct Lisp_Vector *ptr = XVECTOR (obj);
	register int size = ptr->size;
	register int i;

	if (size & ARRAY_MARK_FLAG) break;   /* Already marked */
	ptr->size |= ARRAY_MARK_FLAG; /* Else mark it */
	for (i = 0; i < size; i++)     /* and then mark its elements */
	  mark_object (&ptr->contents[i]);
      }
      break;

    case Lisp_Keymap:
      {
	register struct Lisp_Keymap *ptr = XKEYMAP (obj);
	register int size = ptr->size;
	if (size & ARRAY_MARK_FLAG) break; /* Already marked */
	ptr->size |= ARRAY_MARK_FLAG;      /* Else mark it */
					   /* and then mark its elements */
	mark_object (&ptr->parent);
	mark_object (&ptr->sub_maps_cache);
	mark_object (&ptr->table);
	mark_object (&ptr->inverse_table);
      }
      break;

#ifdef MULTI_SCREEN
    case Lisp_Screen:
      {
	register struct screen *ptr = XSCREEN (obj);
	register int size = ptr->size;
	register int i;

	if (size & ARRAY_MARK_FLAG)
	  break;   /* Already marked */

	ptr->size |= ARRAY_MARK_FLAG; /* Else mark it */
	mark_object (&ptr->name);
	mark_object (&ptr->root_window);
	mark_object (&ptr->selected_window);
	mark_object (&ptr->minibuffer_window);
	mark_object (&ptr->buffer_alist);
	mark_object (&ptr->param_alist);
	mark_object (&ptr->menubar_data);
	mark_object (&ptr->face_alist);
      }
      break;
#endif /* MULTI_SCREEN */

    case Lisp_Symbol:
      {
	register struct Lisp_Symbol *ptr = XSYMBOL (obj);
	struct Lisp_Symbol *ptrx;

	if (XMARKBIT (ptr->plist)) break;
	XMARK (ptr->plist);
	XSETTYPE (*(Lisp_Object *) &ptr->name, Lisp_String);
	mark_object (&ptr->name);
	mark_object ((Lisp_Object *) &ptr->value);
	mark_object (&ptr->function);
	mark_object (&ptr->plist);
	ptr = ptr->next;
	if (ptr)
	  {
	    ptrx = ptr;		/* Use pf ptrx avoids compiler bug on Sun */
	    XSETSYMBOL (obj, ptrx);
	    goto loop;
	  }
      }
      break;

    case Lisp_Marker:
      XMARK (XMARKER (obj)->chain);
      /*
	 DO NOT mark thru the marker's chain.
	 The buffer's markers chain does not preserve markers from gc;
	 Instead, markers are removed from the chain when they are freed
	 by gc.
      */
      break;

    case Lisp_Cons:
    case Lisp_Buffer_Local_Value:
    case Lisp_Some_Buffer_Local_Value:
      {
	register struct Lisp_Cons *ptr = XCONS (obj);
	if (XMARKBIT (ptr->car)) break;
	XMARK (ptr->car);
	mark_object (&ptr->car);
	objptr = &ptr->cdr;
	obj = ptr->cdr;
	goto loop;
      }

#ifdef LISP_FLOAT_TYPE
    case Lisp_Float:
      XMARK (XFLOAT (obj)->type);
      break;
#endif /* LISP_FLOAT_TYPE */

    case Lisp_Buffer:
      if (!XMARKBIT (XBUFFER (obj)->name))
	mark_buffer (obj);
      break;

    case Lisp_Extent:
      if (!EXTENT_MARKED_P (XEXTENT (obj)))
	mark_one_extent (XEXTENT (obj));
      break;

    case Lisp_Extent_Replica:
      if (!DUP_MARKED_P (XDUP (obj)))
        {
          MARK_DUP (XDUP (obj));
          mark_object (&(XDUP (obj)->extent));
        }
      break;

    case Lisp_Int:
    case Lisp_Void:
    case Lisp_Subr:
    case Lisp_Intfwd:
    case Lisp_Boolfwd:
    case Lisp_Objfwd:
    case Lisp_Buffer_Objfwd:
    case Lisp_Internal_Stream:
    /*
       Lisp_Buffer_Objfwd not done.
       All markable slots in current buffer marked anyway.

       Lisp_Objfwd not done.
       The places they point to are protected with staticpro.
    */
      break;

    case Lisp_Event:
      {
	struct Lisp_Event *ptr = XEVENT (obj);
	if (! XMARKBIT ((int) ptr->event_type))
	  mark_event (ptr);
	break;
      }

    default:
      abort ();
    }
}

/* Mark the pointers in a buffer structure.  */

static void
mark_buffer (buffer)
     Lisp_Object buffer;
{
  Lisp_Object tem;
  register struct buffer *buf = XBUFFER (buffer);
  register Lisp_Object *ptr;

  /* This is the buffer's markbit */
  mark_object (&buf->name);
  XMARK (buf->name);

  /* mark the extents attached to this string, if any */
  if (XTYPE (buf->extents) == Lisp_Extent)
    {
      EXTENT ext = XEXTENT (buf->extents);
      if (!EXTENT_LIST_MARKED_P (ext))
	mark_extents (ext);
    }
  else if (!NILP (buf->extents))
    mark_object (&buf->extents);

#undef MARKED_SLOT
#define MARKED_SLOT(x) mark_object(&(buf->x))
#include "bufslots.h"
}

/* Mark all pointers in an event object */

static void
mark_event (ptr)
     struct Lisp_Event *ptr;
{
  int type = ptr->event_type;
  XMARK (ptr->event_type);
  switch (type) {
  case key_press_event:
    mark_object (&ptr->event.key.key);
    break;
  case process_event:
    mark_object (&ptr->event.process.process);
    break;
  case timeout_event:
    mark_object (&ptr->event.timeout.function);
    mark_object (&ptr->event.timeout.object);
    break;
  case eval_event:
  case menu_event:
    mark_object (&ptr->event.eval.function);
    mark_object (&ptr->event.eval.object);
    break;
  case button_press_event:
  case button_release_event:
  case pointer_motion_event:
  case magic_event:
  case empty_event:
  case dead_event:
    break;
  default:
    abort ();
  }
}

  
/* Mark events waiting to be read */
static void
mark_command_event_queue ()
{
  struct Lisp_Event *event = command_event_queue->head;
  while (event) {
    mark_event (event);
    event = event->next;
  }
}

/* Find all structures not marked, and free them. */

static void
gc_sweep ()
{
  compact_string_chars ();

  /* Put all unmarked conses on free list */
  {
    register struct cons_block *cblk;
    register int lim = cons_block_index;
    register int num_free = 0, num_used = 0;

    cons_free_list = 0;
  
    for (cblk = cons_block; cblk; cblk = cblk->next)
      {
	register int i;
	for (i = 0; i < lim; i++)
	  if (!XMARKBIT (cblk->conses[i].car))
	    {
	      XFASTINT (cblk->conses[i].car) = (int) cons_free_list;
	      num_free++;
	      cons_free_list = &cblk->conses[i];
	    }
	  else
	    {
	      num_used++;
	      XUNMARK (cblk->conses[i].car);
	    }
	lim = CONS_BLOCK_SIZE;
      }
    total_conses = num_used;
    total_free_conses = num_free;
  }

#ifdef LISP_FLOAT_TYPE
  /* Put all unmarked floats on free list */
  {
    register struct float_block *fblk;
    register int lim = float_block_index;
    register int num_free = 0, num_used = 0;

    float_free_list = 0;
  
    for (fblk = float_block; fblk; fblk = fblk->next)
      {
	register int i;
	for (i = 0; i < lim; i++)
	  if (!XMARKBIT (fblk->floats[i].type))
	    {
	      XFASTINT (fblk->floats[i].type) = (int) float_free_list;
	      num_free++;
	      float_free_list = &fblk->floats[i];
	    }
	  else
	    {
	      num_used++;
	      XUNMARK (fblk->floats[i].type);
	    }
	lim = FLOAT_BLOCK_SIZE;
      }
    total_floats = num_used;
    total_free_floats = num_free;
  }
#endif /* LISP_FLOAT_TYPE */

  /* Put all unmarked extents on free list */
  {
    register struct extent_block *eblk;
    register int lim = extent_block_index;
    register int num_free = 0, num_used = 0;

    extent_free_list = 0;

    for (eblk = extent_block; eblk; eblk = eblk->next)
      {
	register int i;

	for (i = 0; i < lim; i++)
	  {
            EXTENT extent = &(eblk->extents[i]);
	    if (!EXTENT_MARKED_P (extent))
	      {
                bzero ((char *) extent, sizeof (struct extent));
		extent->next = extent_free_list;
		extent_free_list = extent;
#ifdef ENERGIZE
                {
                  extern void energize_extent_finalization ();
                  energize_extent_finalization (extent);
                }
#endif
		num_free++;
	      }
	    else
	      {
		num_used++;
		UNMARK_EXTENT (extent);
	      }
	  }
	lim = EXTENT_BLOCK_SIZE;
      }
    total_extents = num_used;
    total_free_extents = num_free;
  }


  /* put all extent replicas on a free_list */
  {
    struct dup_block *dblk;
    int lim = dup_block_index;
    int num_free = 0;
    int num_used = 0;

    dup_free_list = 0;

    for (dblk = dup_block; dblk; dblk = dblk->next)
      {
	register int i;

	for (i = 0; i < lim; i++)
	  {
            DUP dup = &(dblk->dups[i]);
	    if (!DUP_MARKED_P (dup))
	      {
                bzero ((char *) dup, sizeof (*dup));
		dup->extent = (Lisp_Object) dup_free_list;
		dup_free_list = dup;
		num_free++;
	      }
	    else
	      {
		num_used++;
		UNMARK_DUP (dup);
	      }
	  }
	lim = DUP_BLOCK_SIZE;
      }
    total_dups = num_used;
    total_free_dups = num_free;
  }

  /* Put all unmarked symbols on free list */
  {
    register struct symbol_block *sblk;
    register int lim = symbol_block_index;
    register int num_free = 0, num_used = 0;

    symbol_free_list = 0;
  
    for (sblk = symbol_block; sblk; sblk = sblk->next)
      {
	register int i;
	for (i = 0; i < lim; i++)
	  if (!XMARKBIT (sblk->symbols[i].plist))
	    {
	      XFASTINT (sblk->symbols[i].value) = (int) symbol_free_list;
	      symbol_free_list = &sblk->symbols[i];
	      num_free++;
	    }
	  else
	    {
	      num_used++;
	      sblk->symbols[i].name
		= XSTRING (*(Lisp_Object *) &sblk->symbols[i].name);
	      XUNMARK (sblk->symbols[i].plist);
	    }
	lim = SYMBOL_BLOCK_SIZE;
      }
    total_symbols = num_used;
    total_free_symbols = num_free;
  }

#ifndef standalone
  /* Put all unmarked markers on free list.
     Dechain each one first from the buffer it points into. */
  {
    register struct marker_block *mblk;
    struct Lisp_Marker *tem1;
    register int lim = marker_block_index;
    register int num_free = 0, num_used = 0;

    marker_free_list = 0;
  
    for (mblk = marker_block; mblk; mblk = mblk->next)
      {
	register int i;
	for (i = 0; i < lim; i++)
	  if (!XMARKBIT (mblk->markers[i].chain))
	    {
	      Lisp_Object tem;
	      tem1 = &mblk->markers[i];  /* tem1 avoids Sun compiler bug */
	      XSET (tem, Lisp_Marker, tem1);
	      unchain_marker (tem);
	      XFASTINT (mblk->markers[i].chain) = (int) marker_free_list;
	      marker_free_list = &mblk->markers[i];
	      num_free++;
	    }
	  else
	    {
	      num_used++;
	      XUNMARK (mblk->markers[i].chain);
	    }
	lim = MARKER_BLOCK_SIZE;
      }

    total_markers = num_used;
    total_free_markers = num_free;
  }

  /* Free all unmarked buffers */
  {
    register struct buffer *buffer = all_buffers, *prev = 0, *next;

    while (buffer)
      if (!XMARKBIT (buffer->name))
	{
	  if (prev)
	    prev->next = buffer->next;
	  else
	    all_buffers = buffer->next;
	  next = buffer->next;
	  free (buffer);
	  buffer = next;
	}
      else
	{
	  XUNMARK (buffer->name);
	  prev = buffer, buffer = buffer->next;
	}
  }

#endif /* standalone */

  /* Free all unmarked vectors */
  {
    register struct Lisp_Vector *vector = all_vectors, *prev = 0, *next;
    total_vector_size = 0;

    while (vector)
      if (!(vector->size & ARRAY_MARK_FLAG))
	{
	  if (prev)
	    prev->next = vector->next;
	  else
	    all_vectors = vector->next;
	  next = vector->next;
	  free (vector);
	  vector = next;
	}
      else
	{
	  vector->size &= ~ARRAY_MARK_FLAG;
	  total_vector_size += vector->size;
	  prev = vector, vector = vector->next;
	}
  }


  /* Put all unmarked strings on free list, free'ing the string chars
     of large unmarked strings */
  {
    register struct string_block *sblk;
    register int lim = string_block_index;
    register int num_free = 0, num_used = 0, num_small_used = 0, num_bytes = 0;

    string_free_list = 0;

    for (sblk = string_block; sblk; sblk = sblk->next)
      {
	register int i;

	for (i = 0; i < lim; i++)
	  {
            struct Lisp_String *string = &(sblk->strings[i]);
	    if (!XMARKBIT (string->dup_list))
	      {
                if (BIG_STRING_SIZE (string->size))
                  free ((char *)CHARS_TO_STRING_CHAR (string->data));
                bzero ((char *) string, sizeof (struct Lisp_String));
		string->dup_list = (Lisp_Object) string_free_list;
		string_free_list = string;
		num_free++;
	      }
	    else
	      {
		num_used++;
		XUNMARK (string->dup_list);
                if (!(BIG_STRING_SIZE (string->size)))
                  num_small_used++;
                num_bytes += string->size;
	      }
	  }
	lim = STRING_BLOCK_SIZE;
      }
    total_strings = num_used;
    total_short_strings = num_small_used;
    total_free_strings = num_free;
    total_string_size = num_bytes;
  }

  free_unmarked_events ();
}


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
            (struct string_chars *) &from_sb->chars[from_pos];
          struct string_chars *to_s_chars;
          struct Lisp_String *string = from_s_chars->string;
          int size = string->size;
          int fullsize = STRING_FULLSIZE (size);

          if (BIG_STRING_SIZE (size))
            abort();

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
          if ((to_pos + fullsize) > STRING_CHARS_BLOCK_SIZE)
            {
              to_sb->pos = to_pos;
              to_sb = to_sb->next;
              to_pos = 0;
            }
             
          /* Compute new address of this string
             and update TO_POS for the space being used.  */
          to_s_chars = (struct string_chars *) &to_sb->chars[to_pos];

          /* Copy the string_chars to the new place.  */
          if (from_s_chars != to_s_chars)
            bcopy (from_s_chars, to_s_chars, fullsize);

          /* Relocate FROM_S_CHARS's reference */
          string->data = &(to_s_chars->chars[0]);
             
          from_pos += fullsize;
          to_pos += fullsize;
        }
    }

  /* Set current to the last string chars block still used and 
     free any that follow. */
  {
    struct string_chars_block *this = to_sb->next;

    current_string_chars_block = to_sb;
    current_string_chars_block->pos = to_pos;
    current_string_chars_block->next = 0;

    while (this)
      {
        struct string_chars_block *tmp = this->next;
        free (this);
        this = tmp;
      }
  }
}


DEFUN ("garbage-collect", Fgarbage_collect, Sgarbage_collect, 0, 0, "",
  "Reclaim storage for Lisp objects no longer needed.\n\
Returns info on amount of space in use:\n\
 ((USED-CONSES . FREE-CONSES) (USED-SYMS . FREE-SYMS)\n\
  (USED-MARKERS . FREE-MARKERS) USED-STRING-CHARS USED-VECTOR-SLOTS\n\
  (USED-FLOATS . FREE-FLOATS) (USED-EVENTS . FREE-EVENTS))\n\
Garbage collection happens automatically if you cons more than\n\
`gc-cons-threshold' bytes of Lisp data since previous garbage collection.")
  ()
{
  register struct gcpro *tail;
  register struct specbinding *bind;
  struct catchtag *catch;
  struct handler *handler;
  register struct backtrace *backlist;
  register Lisp_Object tem;
  char *omessage = echo_area_glyphs;
  char stack_top_variable;
  extern char *stack_bottom;
  register int i;
  SCREEN_PTR s = selected_screen;

  if (gc_in_progress != 0)
    return Qnil;

  if (gc_currently_forbidden)
    return Qnil;

  gc_in_progress = 1;

#if MAX_SAVE_STACK > 0

  /* Save a copy of the contents of the stack, for debugging.  */
  if (NILP (Vpurify_flag))
    {
      i = &stack_top_variable - stack_bottom;
      if (i < 0) i = -i;
      if (i < MAX_SAVE_STACK)
	{
	  if (stack_copy == 0)
	    stack_copy = (char *) malloc (stack_copy_size = i);
	  else if (stack_copy_size < i)
	    stack_copy = (char *) realloc (stack_copy, (stack_copy_size = i));
	  if (stack_copy)
	    {
	      if ((int) (&stack_top_variable - stack_bottom) > 0)
		bcopy (stack_bottom, stack_copy, i);
	      else
		bcopy (&stack_top_variable, stack_copy, i);
	    }
	}
    }
#endif /* MAX_SAVE_STACK > 0 */

  if (!noninteractive)
    {
#ifdef HAVE_X_WINDOWS
      extern int x_show_gc_cursor (struct screen*);
      if (!x_show_gc_cursor (s))
#endif
	message ("Garbage collecting...");
    }

  /* Don't keep command history around forever
     You can't call nthcdr here because that's ^Gable, which will abort(),
     since gc_in_progress is true.
   */
  tem = Vcommand_history;
  for (i = 0; i < 30 && CONSP (tem); i++)
    tem = XCONS (tem)->cdr;
  if (CONSP (tem))
    XCONS (tem)->cdr = Qnil;
  /* Likewise for undo information.  */
  {
    register struct buffer *nextb = all_buffers;

    while (nextb)
      {
	nextb->undo_list 
	  = truncate_undo_list (nextb->undo_list, undo_threshold,
				undo_high_threshold);
	nextb = nextb->next;
      }
  }

  prepare_to_gc_events ();	/* cut some stuff loose */
  mark_command_event_queue ();

  /* Mark all the special slots that serve as the roots of accessibility.

     Usually the special slots to mark are contained in particular structures.
     Then we know no slot is marked twice because the structures don't overlap.
     In some cases, the structures point to the slots to be marked.
     For these, we use MARKBIT to avoid double marking of the slot.  */

  for (i = 0; i < staticidx; i++)
    mark_object (staticvec[i]);
  for (tail = gcprolist; tail; tail = tail->next)
    for (i = 0; i < tail->nvars; i++)
      if (!XMARKBIT (tail->var[i]))
	{
	  mark_object (&tail->var[i]);
	  XMARK (tail->var[i]);
	}
  for (bind = specpdl; bind != specpdl_ptr; bind++)
    {
      mark_object (&bind->symbol);
      mark_object (&bind->old_value);
    }
  for (catch = catchlist; catch; catch = catch->next)
    {
      mark_object (&catch->tag);
      mark_object (&catch->val);
    }  
  for (handler = handlerlist; handler; handler = handler->next)
    {
      mark_object (&handler->handler);
      mark_object (&handler->var);
    }  
  for (backlist = backtrace_list; backlist; backlist = backlist->next)
    {
      if (!XMARKBIT (*backlist->function))
	{
	  mark_object (backlist->function);
	  XMARK (*backlist->function);
	}
      if (backlist->nargs == UNEVALLED || backlist->nargs == MANY)
	i = 0;
      else
	i = backlist->nargs - 1;
      for (; i >= 0; i--)
	if (!XMARKBIT (backlist->args[i]))
	  {
	    mark_object (&backlist->args[i]);
	    XMARK (backlist->args[i]);
	  }
    }

  gc_sweep ();

  /* Clear the mark bits that we set in certain root slots.  */

  for (tail = gcprolist; tail; tail = tail->next)
    for (i = 0; i < tail->nvars; i++)
      XUNMARK (tail->var[i]);
  for (backlist = backtrace_list; backlist; backlist = backlist->next)
    {
      XUNMARK (*backlist->function);
      if (backlist->nargs == UNEVALLED || backlist->nargs == MANY)
	i = 0;
      else
	i = backlist->nargs - 1;
      for (; i >= 0; i--)
	XUNMARK (backlist->args[i]);
    }  
  XUNMARK (buffer_defaults.name);
  XUNMARK (buffer_local_symbols.name);

/*  clear_marks (); */

  consing_since_gc = 0;
  if (gc_cons_threshold < 10000)
    gc_cons_threshold = 10000;

  if (!noninteractive)
    {
#ifdef HAVE_X_WINDOWS
      extern int x_show_normal_cursor (struct screen* s);
      if (!x_show_normal_cursor (s))
#endif
	{
	  if (omessage)
	    message (omessage);
	  else 
	    message ("Garbage collecting...done");
	}
    }
  
  gc_in_progress = 0;

  return
    Fcons
    (Fcons (make_number (total_conses), make_number (total_free_conses)),
     Fcons
     (Fcons (make_number (total_symbols), make_number (total_free_symbols)),
      Fcons
      (Fcons (make_number (total_markers), make_number (total_free_markers)),
       Fcons
       (make_number (total_string_size),
	Fcons
	(make_number (total_vector_size),
#ifdef LISP_FLOAT_TYPE
	 Fcons
	 (Fcons (make_number (total_floats), make_number (total_free_floats)),
	  Fcons
	  (Fcons (make_number (total_events), make_number (total_free_events)),
	   Qnil))
#else /* not LISP_FLOAT_TYPE */
	 Fcons
	 (Fcons (make_number (total_events), make_number (total_free_events)),
	  Qnil)
#endif /* not LISP_FLOAT_TYPE */
	 )))));
}

/* Initialization */

init_alloc_once ()
{
#ifdef PURESTAT
  purestat [0] = 0; /* cons */
  purestat [1] = 0; /* string */
  purestat [2] = 0; /* vector */
  purestat [3] = 0; /* compiled */
  purestat [4] = 0; /* float */
#endif

  /* Used to do Vpurify_flag = Qt here, but Qt isn't set up yet!  */
  pureptr = 0;
  all_vectors = 0;
  ignore_warnings = 1;
  init_strings ();
  init_cons ();
  init_symbol ();
  init_marker ();
#ifdef LISP_FLOAT_TYPE
  init_float ();
#endif /* LISP_FLOAT_TYPE */
  init_extents ();
  ignore_warnings = 0;
  gcprolist = 0;
  staticidx = 0;
  consing_since_gc = 0;
  gc_cons_threshold = 100000;
#ifdef VIRT_ADDR_VARIES
  malloc_sbrk_unused = 1<<22;	/* A large number */
  malloc_sbrk_used = 100000;	/* as reasonable as any number */
#endif /* VIRT_ADDR_VARIES */
}

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
called.  (Note that `funcall' is called implicitly.)\n\
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

  DEFVAR_LISP ("purify-flag", &Vpurify_flag,
    "Non-nil means loading Lisp code in order to dump an executable.\n\
This means that certain objects should be allocated in shared (pure) space.");

  DEFVAR_INT ("undo-threshold", &undo_threshold,
    "Keep no more undo information once it exceeds this size.\n\
This threshold is applied when garbage collection happens.\n\
The size is counted as the number of bytes occupied,\n\
which includes both saved text and other data.");
  undo_threshold = 20000;

  DEFVAR_INT ("undo-high-threshold", &undo_high_threshold,
    "Don't keep more than this much size of undo information.\n\
A command which pushes past this size is itself forgotten.\n\
This threshold is applied when garbage collection happens.\n\
The size is counted as the number of bytes occupied,\n\
which includes both saved text and other data.");
  undo_high_threshold = 30000;

  DEFVAR_BOOL ("   gc-currently-forbidden", &gc_currently_forbidden,
               "internal variable used to control undo");
  gc_currently_forbidden = 0;
  Qgc_currently_forbidden = intern ("   gc-currently-forbidden");

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
}
