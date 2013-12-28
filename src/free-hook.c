/* Debugging hooks for malloc. */

/* These hooks work with gmalloc to catch allocation errors.
   In particular, the following is trapped:

   * Freeing the same pointer twice.
   * Trying to free a pointer not returned by malloc.
   * Trying to realloc a pointer not returned by malloc.

   In addition, every word of every block freed is set to
   0xdeadbeef.  This causes many uses of freed storage to be
   trapped or recognized.

   When you use this, the storage used by the last FREE_QUEUE_LIMIT
   calls to free() is not recycled.  When you call free for the Nth
   time, the (N - FREE_QUEUE_LIMIT)'th block is actually recycled.

   For these last FREE_QUEUE_LIMIT calls to free() a backtrace is
   saved showing where it was called from.  The function
   find_backtrace() is provided here to be called from GDB with a
   pointer (such as would be passed to free()) as argument, e.g.
   (gdb) p/a *find_backtrace (0x234000).  If SAVE_ARGS is defined,
   the first three arguments to each function are saved as well as the
   return addresses.

   If UNMAPPED_FREE is defined, instead of setting every word of freed
   storage to 0xdeadbeef, every call to malloc goes on its own page(s).
   When free() is called, the block is read and write protected.  This
   is very useful when debugging, since it usually generates a bus error
   when the deadbeef hack might only cause some garbage to be printed.
   However, this is too slow for everyday use, since it takes an enormous
   number of pages.


   Some other features that would be useful are:

   * Checking for storage leaks.
     This could be done by a GC-like facility that would scan the data
     segment looking for pointers to allocated storage and tell you
     about those that are no longer referenced.  This could be invoked
     at any time.  Another possibility is to report on what allocated
     storage is still in use when the process is exited.  Typically
     there will be a large amount, so this might not be very useful.
*/

#define sun4

#ifdef emacs
#include "cadillac-btl.h"
#include "config.h"
#include "lisp.h"
#else
void *malloc (unsigned long);
#endif

#include "hash.h"
#include "blockio.h"

#ifdef UNMAPPED_FREE
#include <sys/mman.h>
#include <sys/param.h>
#define ROUND_UP_TO_PAGE(i) (((i) + PAGEOFFSET) & PAGEMASK)
#endif

#include <sys/types.h>

void free (void *);

c_hashtable pointer_table;

void (*__free_hook)();
void *(*__malloc_hook)();

void *check_malloc (unsigned long);

typedef void (*fun_ptr)();

#define FREE_QUEUE_LIMIT 1000
#define TRACE_LIMIT 20

typedef struct {
  fun_ptr return_pc;
#ifdef SAVE_ARGS
  void *arg[3];
#endif
} fun_entry;

typedef struct {
  void *address;
  unsigned long length;
  fun_entry backtrace[TRACE_LIMIT];
} free_queue_entry;

free_queue_entry free_queue[FREE_QUEUE_LIMIT];

int current_free;

static void
init_frame (FRAME *fptr)
{
  FRAME tmp_frame;

#ifdef sparc
  /* Do the system trap ST_FLUSH_WINDOWS */
  asm ("ta 3");
  asm ("st %sp, [%i0+0]");
  asm ("st %fp, [%i0+4]");
#endif

  fptr->pc = (char *) init_frame;
  tmp_frame = *fptr;

  PREVIOUS_FRAME (tmp_frame);

  *fptr = tmp_frame;
  return;
}

#ifdef SAVE_ARGS
static void *
frame_arg (FRAME *fptr, int index)
{
  return ((void *) FRAME_ARG(*fptr, index));
}
#endif

static void
save_backtrace (FRAME *current_frame_ptr, fun_entry *table)
{
  int i = 0;
#ifdef SAVE_ARGS
  int j;
#endif
  FRAME current_frame = *current_frame_ptr;

  /* Get up and out of free() */
  PREVIOUS_FRAME (current_frame);

  /* now do the basic loop adding data until there is no more */
  while (PREVIOUS_FRAME (current_frame) && i < TRACE_LIMIT)
    {
      table[i].return_pc = (void (*)())FRAME_PC (current_frame);
#ifdef SAVE_ARGS
      for (j = 0; j < 3; j++)
	table[i].arg[j] = frame_arg (&current_frame, j);
#endif
      i++;
    }
  bzero ((void *)&table[i], sizeof (fun_entry) * (TRACE_LIMIT - i));
}

free_queue_entry *
find_backtrace (void *ptr)
{
  int i;

  for (i = 0; i < FREE_QUEUE_LIMIT; i++)
    if (free_queue[i].address == ptr)
      return &free_queue[i];

  return 0;
}

int strict_free_check;

void
check_free (void* ptr)
{
  FRAME start_frame;
  
  BLOCK_INPUT;
  init_frame (&start_frame);

  __free_hook = 0;
  __malloc_hook = 0;
  if (!pointer_table)
    pointer_table = make_hashtable (FREE_QUEUE_LIMIT * 2);
  if (ptr != 0)
    {
      long size;
#ifdef UNMAPPED_FREE
      unsigned long rounded_up_size;
#endif

      int present = (int)gethash (ptr, pointer_table, (void *)&size);

      if (!present)
	/* This can only happen if you try to free something that didn't
	   come from malloc */
	if (strict_free_check)
	  abort ();
	else
	  {
	    __free_hook = check_free;
	    __malloc_hook = check_malloc;
	    goto end;
	  }

      if (size < 0)
	/* This happens when you free twice */
	if (strict_free_check)
	  abort ();
	else
	  {
	    __free_hook = check_free;
	    __malloc_hook = check_malloc;
	    goto end;
	  }
      puthash (ptr, (void *)-size, pointer_table);
#ifdef UNMAPPED_FREE
      /* Round up size to an even number of pages. */
      rounded_up_size = ROUND_UP_TO_PAGE (size);
      /* Protect the pages freed from all access */
      if (strict_free_check)
	mprotect (ptr, rounded_up_size, PROT_NONE);
#else
      /* Set every word in the block to 0xdeadbeef */
      if (strict_free_check)
	{
	  unsigned long long_length = (size + (sizeof (long) - 1))
	    / sizeof (long);
	  unsigned long i;

	  for (i = 0; i < long_length; i++)
	    ((unsigned long *) ptr)[i] = 0xdeadbeef;
	}
#endif
      free_queue[current_free].address = ptr;
      free_queue[current_free].length = size;
      save_backtrace (&start_frame,
		      free_queue[current_free].backtrace);
      current_free++;
      if (current_free >= FREE_QUEUE_LIMIT)
	current_free = 0;
      /* Really free this if there's something there */
      {
	void *old = free_queue[current_free].address;

	if (old)
	  {
#ifdef UNMAPPED_FREE
	    unsigned long old_len = free_queue[current_free].length;

	    mprotect (old, old_len,  PROT_READ | PROT_WRITE | PROT_EXEC);
#endif
	    free (old);
	    remhash (old, pointer_table);
	  }
      }
    }
  __free_hook = check_free;
  __malloc_hook = check_malloc;

 end:
  UNBLOCK_INPUT;
}  

void *
check_malloc (unsigned long size)
{
  unsigned long rounded_up_size;
  void *result;

  BLOCK_INPUT;
  __free_hook = 0;
  __malloc_hook = 0;
  if (size == 0)
    {
      result = 0;
      goto end;
    }
#ifdef UNMAPPED_FREE
  /* Round up to an even number of pages. */
  rounded_up_size = ROUND_UP_TO_PAGE (size);
#else
  rounded_up_size = size;
#endif
  result = malloc (rounded_up_size);
  if (!pointer_table)
    pointer_table = make_hashtable (FREE_QUEUE_LIMIT * 2);
  puthash(result, (void *)size, pointer_table);
  __free_hook = check_free;
  __malloc_hook = check_malloc;
 end:
  UNBLOCK_INPUT;
  return result;
}

void *(*__realloc_hook)();

#ifdef MIN
#undef MIN
#endif
#define MIN(A, B) ((A) < (B) ? (A) : (B))

/* Don't optimize realloc */

void *
check_realloc (void * ptr, unsigned long size)
{
  int present;
  unsigned long old_size;
  void *result = malloc(size);
  
  BLOCK_INPUT;
  present = (int)gethash (ptr, pointer_table, (void *)&old_size);
  if (!present)
    /* This can only happen by reallocing a pointer that didn't
       come from malloc. */
    abort();
  if (result == 0)
    goto end;
  memcpy(result, ptr, MIN(size, old_size));
  free(ptr);
 end:
  UNBLOCK_INPUT;
  return result;
}
  
void
enable_strict_free_check ()
{
  strict_free_check = 1;
}

void
disable_strict_free_check ()
{
  strict_free_check = 0;
}

void *
block_input_malloc (unsigned long size);

void
block_input_free (void* ptr)
{
  BLOCK_INPUT;
  __free_hook = 0;
  __malloc_hook = 0;
  free (ptr);
  __free_hook = block_input_free;
  __malloc_hook = block_input_malloc;
  UNBLOCK_INPUT;
}

void *
block_input_malloc (unsigned long size)
{
  void* result;
  BLOCK_INPUT;
  __free_hook = 0;
  __malloc_hook = 0;
  result = malloc (size);
  __free_hook = block_input_free;
  __malloc_hook = block_input_malloc;
  UNBLOCK_INPUT;
  return result;
}


void *
block_input_realloc (void* ptr, unsigned long size)
{
  void* result;
  BLOCK_INPUT;
  __free_hook = 0;
  __malloc_hook = 0;
  __realloc_hook = 0;
  result = realloc (ptr, size);
  __free_hook = block_input_free;
  __malloc_hook = block_input_malloc;
  __realloc_hook = block_input_realloc;
  UNBLOCK_INPUT;
  return result;
}



#ifdef emacs
void
disable_free_hook ()
{
  __free_hook = block_input_free;
  __malloc_hook = block_input_malloc;
  __realloc_hook = block_input_realloc;
}

void
init_free_hook ()
{
  __free_hook = check_free;
  __malloc_hook = check_malloc;
  __realloc_hook = check_realloc;
  current_free = 0;
  strict_free_check = 1;
}

void really_free_one_entry (void *, int, int *);

DEFUN ("really-free", Freally_free, Sreally_free, 0, 1, "P",
       "Actually free the storage held by the free() debug hook.\n\
A no-op if the free hook is disabled.")
     (arg)
     Lisp_Object arg;
{
  int count[2];
  Lisp_Object lisp_count[2];

  if ((__free_hook != 0) && pointer_table)
    {
      count[0] = 0;
      count[1] = 0;
      __free_hook = 0;
      maphash ((maphash_function)really_free_one_entry, 
               pointer_table, (void *)&count);
      memset (free_queue, 0, sizeof (free_queue_entry) * FREE_QUEUE_LIMIT);
      current_free = 0;
      __free_hook = check_free;
      XSET(lisp_count[0], Lisp_Int, count[0]);
      XSET(lisp_count[1], Lisp_Int, count[1]);
      return Fcons(lisp_count[0], lisp_count[1]);
    }
  else
    return Fcons(make_number (0), make_number (0));
}

void
really_free_one_entry (void *key, int contents, int *countp)
{
  if (contents < 0)
    {
      free (key);
#ifdef UNMAPPED_FREE
      mprotect (key, -contents, PROT_READ | PROT_WRITE | PROT_EXEC);
#endif
      remhash (key, pointer_table);
      countp[0]++;
      countp[1] += -contents;
    }
}
  

void
syms_of_free_hook ()
{
  defsubr (&Sreally_free);
}

#else
void (*__free_hook)() = check_free;
void *(*__malloc_hook)() = check_malloc;
void *(*__realloc_hook)() = check_realloc;
#endif


#if defined(DEBUG_INPUT_BLOCKING) || defined (DEBUG_GCPRO)

#include "blockio.h"

typedef enum {
  block_type, unblock_type, totally_type,
  gcpro1_type, gcpro2_type, gcpro3_type, gcpro4_type, ungcpro_type
} blocktype;

struct block_input_history_struct {
  char *file;
  int line;
  blocktype type;
  int value;
  fun_entry backtrace[TRACE_LIMIT];
};

typedef struct block_input_history_struct block_input_history;

#endif

#ifdef DEBUG_INPUT_BLOCKING

int blhistptr;

#define BLHISTLIMIT 1000

block_input_history blhist[BLHISTLIMIT];

note_block_input (char *file, int line)
{
  note_block (file, line, block_type);
  if (x_input_blocked > 2) abort();
}

note_unblock_input (char* file, int line)
{
  note_block (file, line, unblock_type);
}

note_totally_unblocked (char* file, int line)
{
  note_block (file, line, totally_type);
}

note_block (char *file, int line, blocktype type)
{
  FRAME start_frame;

  init_frame (&start_frame);
  
  blhist[blhistptr].file = file;
  blhist[blhistptr].line = line;
  blhist[blhistptr].type = type;
  blhist[blhistptr].value = x_input_blocked;

  save_backtrace (&start_frame,
		  blhist[blhistptr].backtrace);

  blhistptr++;
  if (blhistptr >= BLHISTLIMIT)
    blhistptr = 0;
}

#endif


#ifdef DEBUG_GCPRO

int gcprohistptr;
#define GCPROHISTLIMIT 1000
block_input_history gcprohist[GCPROHISTLIMIT];

static void
log_gcpro (char *file, int line, struct gcpro *value, blocktype type)
{
  FRAME start_frame;

  if (type == ungcpro_type)
    {
      if (value == gcprolist) goto OK;
      if (! gcprolist) abort();
      if (value == gcprolist->next) goto OK;
      if (! gcprolist->next) abort();
      if (value == gcprolist->next->next) goto OK;
      if (! gcprolist->next->next) abort();
      if (value == gcprolist->next->next->next) goto OK;
      abort ();
    OK:;
    }
  init_frame (&start_frame);
  gcprohist[gcprohistptr].file = file;
  gcprohist[gcprohistptr].line = line;
  gcprohist[gcprohistptr].type = type;
  gcprohist[gcprohistptr].value = (int) value;
  save_backtrace (&start_frame, gcprohist[gcprohistptr].backtrace);
  gcprohistptr++;
  if (gcprohistptr >= GCPROHISTLIMIT)
    gcprohistptr = 0;
}

void
debug_gcpro1 (char *file, int line, struct gcpro *gcpro1, Lisp_Object *var)
{
  gcpro1->next = gcprolist; gcpro1->var = var; gcpro1->nvars = 1;
  gcprolist = gcpro1;
  log_gcpro (file, line, gcpro1, gcpro1_type);
}

void
debug_gcpro2 (char *file, int line, struct gcpro *gcpro1, struct gcpro *gcpro2,
	      Lisp_Object *var1, Lisp_Object *var2)
{
  gcpro1->next = gcprolist; gcpro1->var = var1; gcpro1->nvars = 1;
  gcpro2->next = gcpro1; gcpro2->var = var2; gcpro2->nvars = 1;
  gcprolist = gcpro2;
  log_gcpro (file, line, gcpro2, gcpro2_type);
}

void
debug_gcpro3 (char *file, int line, struct gcpro *gcpro1, struct gcpro *gcpro2,
	      struct gcpro *gcpro3, Lisp_Object *var1, Lisp_Object *var2,
	      Lisp_Object *var3)
{
  gcpro1->next = gcprolist; gcpro1->var = var1; gcpro1->nvars = 1;
  gcpro2->next = gcpro1; gcpro2->var = var2; gcpro2->nvars = 1;
  gcpro3->next = gcpro2; gcpro3->var = var3; gcpro3->nvars = 1;
  gcprolist = gcpro3;
  log_gcpro (file, line, gcpro3, gcpro3_type);
}

void
debug_gcpro4 (char *file, int line, struct gcpro *gcpro1, struct gcpro *gcpro2,
	      struct gcpro *gcpro3, struct gcpro *gcpro4, Lisp_Object *var1,
	      Lisp_Object *var2, Lisp_Object *var3, Lisp_Object *var4)
{
  log_gcpro (file, line, gcpro4, gcpro4_type);
  gcpro1->next = gcprolist; gcpro1->var = var1; gcpro1->nvars = 1;
  gcpro2->next = gcpro1; gcpro2->var = var2; gcpro2->nvars = 1;
  gcpro3->next = gcpro2; gcpro3->var = var3; gcpro3->nvars = 1;
  gcpro4->next = gcpro3; gcpro4->var = var4; gcpro4->nvars = 1;
  gcprolist = gcpro4;
}

void
debug_ungcpro (char *file, int line, struct gcpro *gcpro1)
{
  log_gcpro (file, line, gcpro1, ungcpro_type);
  gcprolist = gcpro1->next;
}

#endif
