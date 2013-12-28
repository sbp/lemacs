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
#include "intl.h"
#include "hash.h"
#include "elhash.h"

#include <stdio.h>              /* for sprintf */

Lisp_Object Qhashtablep;

#define LISP_OBJECTS_PER_HENTRY (sizeof (hentry) / sizeof (Lisp_Object))/* 2 */

struct hashtable_struct
{
  struct lcrecord_header header;
  unsigned int fullness;
  unsigned long (*hash_function) (CONST void *);
  int		(*test_function) (CONST void *, CONST void *);
  Lisp_Object zero_entry;
  Lisp_Object harray;
  Lisp_Object weak;	/* Qunbound if not weak; if weak, this is the next
			   weak table, or Qnil if this is the last.
			   Don't mark through this. */
};

static Lisp_Object Vweak_hash_tables;

static Lisp_Object mark_hashtable (Lisp_Object, void (*) (Lisp_Object));
static void print_hashtable (Lisp_Object, Lisp_Object, int);
DEFINE_LRECORD_IMPLEMENTATION ("hashtable", lrecord_hashtable,
                               mark_hashtable, print_hashtable, 0, 0,
			       sizeof (struct hashtable_struct));

#define HASHTABLEP(obj) RECORD_TYPEP((obj), lrecord_hashtable)
#define XHASHTABLE(obj) ((struct hashtable_struct *) (XPNTR (obj)))
#define CHECK_HASHTABLE(x, i) \
  { if (!HASHTABLEP((x))) wrong_type_argument (Qhashtablep, (x)); }

static Lisp_Object
mark_hashtable (Lisp_Object obj, void (*markobj) (Lisp_Object))
{
  struct hashtable_struct *table = XHASHTABLE (obj);
  if (!EQ (table->weak, Qunbound))
    {
      /* If the table is weak, we don't want to mark the keys and values
	 (we scan over them after everything else has been marked.)
	 Note that we will mark the table->harray itself later; it's hard
	 to mark that here without also marking its contents. */
      return Qnil;
    }
  ((markobj) (table->zero_entry));
  return (table->harray);
}
  
static void
print_hashtable (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  struct hashtable_struct *table = XHASHTABLE (obj);
  char buf[200];
  if (print_readably)
    error (GETTEXT ("printing unreadable object #<hashtable 0x%x>"),
	   (long) table);
  sprintf (buf, GETTEXT ("#<%stable %d/%d 0x%x>"),
	   (EQ (table->weak, Qunbound) ? "" : GETTEXT ("weak ")),
           table->fullness,
           (vector_length (XVECTOR (table->harray)) / LISP_OBJECTS_PER_HENTRY),
           (long) table);
  write_string_1 (buf, -1, printcharfun);
}

#define SLOT_OFFSET(type, slot_name) \
  ((unsigned) (((char *) (&(((type *)0)->slot_name))) - ((char *) 0)))

static void
ht_copy_to_c (struct hashtable_struct *ht,
              c_hashtable c_table)
{
  c_table->harray = (void *) XVECTOR (ht->harray)->contents;
  c_table->zero_set = (!EQ (ht->zero_entry, Qunbound));
  c_table->zero_entry = LISP_TO_VOID (ht->zero_entry);
  c_table->size = vector_length (XVECTOR (ht->harray))/LISP_OBJECTS_PER_HENTRY;
  c_table->fullness = ht->fullness;
  c_table->hash_function = ht->hash_function;
  c_table->test_function = ht->test_function;
  XSETR (c_table->elisp_table, Lisp_Hashtable, ht);
}

static void
ht_copy_from_c (c_hashtable c_table, 
                struct hashtable_struct *ht)
{
  struct Lisp_Vector dummy;
  /* C is truly hateful */
  void *vec_addr
    = ((char *) c_table->harray 
       - ((char *) &(dummy.contents) - (char *) &dummy));

  XSETVECTOR (ht->harray, vec_addr);
  if (c_table->zero_set)
    VOID_TO_LISP (ht->zero_entry, c_table->zero_entry);
  else
    ht->zero_entry = Qunbound;
  ht->fullness = c_table->fullness;
}


static struct hashtable_struct *
new_hashtable ()
{
  struct hashtable_struct *table
    = alloc_lcrecord (sizeof (struct hashtable_struct), lrecord_hashtable);
  table->harray = Qnil;
  table->zero_entry = Qunbound;
  table->fullness = 0;
  table->hash_function = 0;
  table->test_function = 0;
  return (table);
}

char *
elisp_hvector_malloc (bytes, table)
     unsigned int bytes;
     Lisp_Object table;
{
  Lisp_Object new_vector;
  struct hashtable_struct *ht;

  if (!HASHTABLEP (table)) abort ();
  ht = XHASHTABLE (table);
  if (bytes <= vector_length (XVECTOR (ht->harray)) * sizeof (Lisp_Object))
    abort ();
  new_vector = make_vector ((bytes / sizeof (Lisp_Object)), Qzero);
  return ((char *) (XVECTOR (new_vector)->contents));
}

void
elisp_hvector_free (ptr, table)
     void *ptr;
     Lisp_Object table;
{
  struct hashtable_struct *ht = XHASHTABLE (table);
  Lisp_Object current_vector = ht->harray;

  if (!HASHTABLEP (table)) abort ();
  if (!VECTORP (current_vector)) abort ();
  if (((void *) XVECTOR(current_vector)->contents) != ptr) abort ();
  ht->harray = Qnil;            /* Let GC do its job */
  return;
}


DEFUN ("hashtablep", Fhashtablep, Shashtablep, 1, 1, 0,
       "Returns t if OBJ is a hashtable, else nil.")
  (obj)
  Lisp_Object obj;
{
  return ((HASHTABLEP (obj)) ? Qt : Qnil);
}

/* C code can specify test and hash function for hash tables of lisp objects
   (including weak tables) but lisp code can only create EQ tables because we
   don't have a general lisp object hash function.
 */
Lisp_Object
make_lisp_hashtable (int size,
		     int (*test_function) (CONST void*, CONST void*),
		     unsigned long (*hash_function) (CONST void*))
{
  Lisp_Object result;
  struct hashtable_struct *table = new_hashtable ();
  table->harray = make_vector ((compute_harray_size (size)
				* LISP_OBJECTS_PER_HENTRY),
                               Qzero);
  table->test_function = test_function;
  table->hash_function = hash_function;
  table->weak = Qunbound;
  XSETR (result, Lisp_Hashtable, table);
  return (result);
}

DEFUN ("make-hashtable", Fmake_hashtable, Smake_hashtable, 1, 1, 0,
       "Make a hashtable of initial size SIZE.")
  (size)
  Lisp_Object size;
{
  CHECK_NATNUM (size, 0);
  return make_lisp_hashtable (XINT (size), 0, 0);
}

DEFUN ("copy-hashtable", Fcopy_hashtable, Scopy_hashtable, 1, 1, 0,
       "Make a new hashtable which contains the same keys and values\n\
as the given table.  The keys and values will not themselves be copied.")
  (old_table)
  Lisp_Object old_table;
{
  struct _C_hashtable old_htbl;
  struct _C_hashtable new_htbl;
  struct hashtable_struct *old_ht;
  struct hashtable_struct *new_ht;
  Lisp_Object result;

  CHECK_HASHTABLE (old_table, 0);
  old_ht = XHASHTABLE (old_table);
  ht_copy_to_c (old_ht, &old_htbl);

  /* we can't just call Fmake_hashtable() here because that will make a
     table that is slightly larger than the one we're trying to copy,
     which will make copy_hash() blow up. */
  new_ht = new_hashtable ();
  new_ht->fullness = 0;
  new_ht->zero_entry = Qunbound;
  new_ht->hash_function = old_ht->hash_function;
  new_ht->test_function = old_ht->test_function;
  new_ht->harray = Fmake_vector (Flength (old_ht->harray), Qzero);
  ht_copy_to_c (new_ht, &new_htbl);
  copy_hash (&new_htbl, &old_htbl);
  ht_copy_from_c (&new_htbl, new_ht);
  XSETR (result, Lisp_Hashtable, new_ht);

  if (EQ (old_ht->weak, Qunbound))
    new_ht->weak = Qunbound;
  else
    {
      new_ht->weak = Vweak_hash_tables;
      Vweak_hash_tables = result;
    }

  return (result);
}


DEFUN ("gethash", Fgethash, Sgethash, 2, 3, 0,
       "Find hash value for KEY in TABLE.\n\
If there is no corresponding value, return DEFAULT (default nil)")
  (key, table, defalt)
  Lisp_Object key, table, defalt; /* One can't even spell correctly in C */
{
  CONST void *vval;
  struct _C_hashtable htbl;
  if (!gc_in_progress)
    CHECK_HASHTABLE (table, 0);
  ht_copy_to_c (XHASHTABLE (table), &htbl);
  if (gethash (LISP_TO_VOID (key), &htbl, &vval))
    {
      Lisp_Object val;
      CVOID_TO_LISP (val, vval);
      return val;
    }
  else 
    return defalt;
}


DEFUN ("remhash", Fremhash, Sremhash, 2, 2, 0,
       "Remove hash value for KEY in TABLE.")
  (key, table)
  Lisp_Object key, table;
{
  struct _C_hashtable htbl;
  CHECK_HASHTABLE (table, 0);

  ht_copy_to_c (XHASHTABLE (table), &htbl);
  remhash (LISP_TO_VOID (key), &htbl);
  ht_copy_from_c (&htbl, XHASHTABLE (table));
  return Qnil;
}


DEFUN ("puthash", Fputhash, Sputhash, 3, 3, 0,
       "Hash KEY to VAL in TABLE.")
  (key, val, table)
  Lisp_Object key, val, table;
{
  struct hashtable_struct *ht;
  void *vkey = LISP_TO_VOID (key);

  CHECK_HASHTABLE (table, 0);
  ht = XHASHTABLE (table);
  if (!vkey)
    ht->zero_entry = val;
  else
    {
      struct gcpro gcpro1, gcpro2, gcpro3;
      struct _C_hashtable htbl;

      ht_copy_to_c (XHASHTABLE (table), &htbl);
      GCPRO3 (key, val, table);
      puthash (vkey, LISP_TO_VOID (val), &htbl);
      ht_copy_from_c (&htbl, XHASHTABLE (table));
      UNGCPRO;
    }
  return (val);
}

DEFUN ("clrhash", Fclrhash, Sclrhash, 1, 1, 0,
       "Flush TABLE.")
  (table)
  Lisp_Object table;
{
  struct _C_hashtable htbl;
  CHECK_HASHTABLE (table, 0);
  ht_copy_to_c (XHASHTABLE (table), &htbl);
  clrhash (&htbl);
  ht_copy_from_c (&htbl, XHASHTABLE (table));
  return Qnil;
}

DEFUN ("hashtable-fullness", Fhashtable_fullness, Shashtable_fullness, 1, 1, 0,
       "Returns number of entries in TABLE.")
  (table)
  Lisp_Object table;
{
  struct _C_hashtable htbl;
  CHECK_HASHTABLE (table, 0);
  ht_copy_to_c (XHASHTABLE (table), &htbl);
  return (make_number (htbl.fullness));
}


static void
verify_function (function, description)
     Lisp_Object function;
     char *description;
{
  if (SYMBOLP (function))
  {
    if (NILP (function))
      return;
    else
      function = indirect_function (function, 1);
  }
  if (SUBRP (function) || COMPILEDP (function))
    return;
  else if (CONSP (function))
  {
    Lisp_Object funcar = Fcar (function);
    if ((SYMBOLP (funcar)) 
        && (EQ (funcar, Qlambda) 
#ifdef MOCKLISP_SUPPORT
            || EQ (funcar, Qmocklisp) 
#endif
            || EQ (funcar, Qautoload)))
      return;
  }
  signal_error (Qinvalid_function, list1 (function));
}

static void
lisp_maphash_function (CONST void *void_key,
		       void *void_val,
		       void *void_fn)
{
  Lisp_Object key, val, fn;
  CVOID_TO_LISP (key, void_key);
  VOID_TO_LISP (val, void_val);
  VOID_TO_LISP (fn, void_fn);
  call2 (fn, key, val);
}


DEFUN ("maphash", Fmaphash, Smaphash, 2, 2, 0,
       "Map FUNCTION over entries in TABLE, calling it with two args,\n\
each key and value in the table.")
  (function, table)
  Lisp_Object function, table;
{
  struct _C_hashtable htbl;
  struct gcpro gcpro1, gcpro2;

  verify_function (function, GETTEXT ("hashtable mapping function"));
  CHECK_HASHTABLE (table, 0);
  ht_copy_to_c (XHASHTABLE (table), &htbl);
  GCPRO2 (table, function);
  maphash (lisp_maphash_function, &htbl, LISP_TO_VOID (function));
  UNGCPRO;
  return Qnil;
}


/* This function is for mapping a *C* function over the elements of a
   lisp hashtable.
 */
void
elisp_maphash (function, table, closure)
     maphash_function function;
     Lisp_Object table;
     void *closure;
{
  struct _C_hashtable htbl;

  CHECK_HASHTABLE (table, 0);
  ht_copy_to_c (XHASHTABLE (table), &htbl);
  maphash (function, &htbl, closure);
}

void
elisp_map_remhash (remhash_predicate function,
                   Lisp_Object table,
                   void *closure)
{
  struct _C_hashtable htbl;

  if (!gc_in_progress) CHECK_HASHTABLE (table, 0);
  ht_copy_to_c (XHASHTABLE (table), &htbl);
  map_remhash (function, &htbl, closure);
  ht_copy_from_c (&htbl, XHASHTABLE (table));
}

#if 0
void
elisp_table_op (table, op, arg1, arg2, arg3)
     Lisp_Object table;
     generic_hashtable_op op;
     void *arg1;
     void *arg2;
     void *arg3;

{
  struct _C_hashtable htbl;
  CHECK_HASHTABLE (table, 0);
  ht_copy_to_c (XHASHTABLE (table), &htbl);
  (*op) (&htbl, arg1, arg2, arg3);
  ht_copy_from_c (&htbl, XHASHTABLE (table));
}
#endif /* 0 */


Lisp_Object
make_weak_hashtable (int size,
		     int (*test_function) (CONST void*,CONST void*),
		     unsigned long (*hash_function) (CONST void*))
{
  Lisp_Object table = make_lisp_hashtable (size, test_function, hash_function);
  XHASHTABLE (table)->weak = Vweak_hash_tables;
  Vweak_hash_tables = table;
  return table;
}


DEFUN ("make-weak-hashtable", Fmake_weak_hashtable, Smake_weak_hashtable,
       1, 1, 0,
       "Make a weak hashtable of initial size SIZE.\n\
A weak hashtable is one whose pointers do not count as GC referents:\n\
if the only remaining pointer to an object is in a weak hash table,\n\
then that object will be removed from the table, and collected.  A\n\
non-weak hash table (or any other pointer) would prevent the object\n\
from being collected.")
  (size)
  Lisp_Object size;
{
  CHECK_NATNUM (size, 0);
  return make_weak_hashtable (XINT (size), 0, 0);
}

/* FUCK!!  It's not standard-conforming to cast pointers to functions
   to or from void*.  Give me a fucking break! */
struct marked_p_kludge_fmh 
{
  int (*obj_marked_p) (Lisp_Object);
};

static int
either_unmarked_p (CONST void *key, CONST void *contents, void *closure)
{
  Lisp_Object tem;
  struct marked_p_kludge_fmh *fmh = closure;

  CVOID_TO_LISP (tem, key);
  if (! ((*fmh->obj_marked_p) (tem))) return 1;
  CVOID_TO_LISP (tem, contents);
  if (! ((*fmh->obj_marked_p) (tem))) return 1;
  return 0;
}

void
prune_weak_hashtables (int (*obj_marked_p) (Lisp_Object))
{
  Lisp_Object rest, prev = Qnil;
  for (rest = Vweak_hash_tables;
       !NILP (rest);
       rest = XHASHTABLE (rest)->weak)
    {
      if (! ((*obj_marked_p) (rest)))
	{
	  /* This table itself is garbage.  Remove it from the list. */
	  if (NILP (prev))
	    Vweak_hash_tables = XHASHTABLE (rest)->weak;
	  else
	    XHASHTABLE (prev)->weak = XHASHTABLE (rest)->weak;
	}
      else
	{
          struct marked_p_kludge_fmh fmh;
          fmh.obj_marked_p = obj_marked_p;
	  /* Remove all of the pairs in which either the key or value
	     is unmarked. */
	  elisp_map_remhash (either_unmarked_p, rest, &fmh);

          /* >>> If alloc.c mark_object changes, this must change also... */
	  {
	    /* Now mark the vector itself.  (We don't need to call markobj
	       here because we know that everything *in* it is already marked,
	       we just need to prevent the vector itself from disappearing.)
	       (The remhash above has taken care of zero_entry.)
	     */
	    struct Lisp_Vector *ptr = XVECTOR (XHASHTABLE (rest)->harray);
	    int len = vector_length (ptr);
	    if (len >= 0) /* I don't think it can already be marked, but... */
	      ptr->size = -1 - len;
	  }
	  prev = rest;
	}
    }
}


void
syms_of_elhash () 
{
  defsubr (&Smake_hashtable);
  defsubr (&Scopy_hashtable);
  defsubr (&Shashtablep);
  defsubr (&Sgethash);
  defsubr (&Sputhash);
  defsubr (&Sremhash);
  defsubr (&Sclrhash);
  defsubr (&Smaphash);
  defsubr (&Shashtable_fullness);
  defsubr (&Smake_weak_hashtable);
  defsymbol (&Qhashtablep, "hashtablep");
  /* This must not be staticpro'd */
  Vweak_hash_tables = Qnil;
}
