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

#include "config.h"
#include "lisp.h"
#include "hash.h"
#include "elhash.h"


struct hashtable_struct
{
  Lisp_Object header;           /* 'hashtable */

  Lisp_Object harray;
  Lisp_Object zero_entry;
  Lisp_Object fullness;
};

#define LISP_OBJECTS_PER_HENTRY (sizeof (hentry) / sizeof (Lisp_Object))/* 2 */

#define HT_SIZE (sizeof (struct hashtable_struct) / sizeof (Lisp_Object))

#define XHASHTABLE(obj) \
	((struct hashtable_struct *) (XVECTOR ((obj))->contents))

#define HASHTABLEP(obj) ((VECTORP (obj)) && \
                         (XFASTINT (XVECTOR ((obj))->size) == HT_SIZE) && \
                         (XHASHTABLE ((obj))->header == Qhashtable))

#define CHECK_HASHTABLE(x, i) \
  { if (!HASHTABLEP((x))) wrong_type_argument (Qhashtablep, (x)); }

Lisp_Object Qhashtable;
Lisp_Object Qhashtablep;

static void
ht_copy_to_c (Lisp_Object lisp_table, c_hashtable c_table)
{
  struct hashtable_struct *ht = XHASHTABLE (lisp_table);
  Lisp_Object lobj;
  c_table->harray = (void *) XVECTOR (ht->harray)->contents;
  c_table->zero_set = (!EQ (ht->zero_entry, Qunbound));
  c_table->zero_entry = (void *) ht->zero_entry;
  c_table->size = XVECTOR (ht->harray)->size / LISP_OBJECTS_PER_HENTRY;
  c_table->fullness = XFASTINT (ht->fullness);
  c_table->hash_function = 0;
  c_table->test_function = 0;
  /* can't use casts as lvalues... */
  XSET (lobj, Lisp_Vector, lisp_table);
  c_table->elisp_table = (void *) lobj;
}

static void
ht_copy_from_c (c_hashtable c_table, Lisp_Object lisp_table)
{
  struct hashtable_struct *ht = XHASHTABLE (lisp_table);
  struct Lisp_Vector dummy;
  /* C is truly hateful */
  void *vec_addr
    = ((char *) c_table->harray 
       - ((char *) &(dummy.contents) - (char *) &dummy));
  
  XSET (ht->harray, Lisp_Vector, vec_addr);
  ht->zero_entry = ((c_table->zero_set) 
                    ? (Lisp_Object) c_table->zero_entry
                    : Qunbound);
  XSET (ht->fullness, Lisp_Int, c_table->fullness);
}


static Lisp_Object
new_hashtable ()
{
  Lisp_Object vector = Fmake_vector (make_number(HT_SIZE), Qnil);
  struct hashtable_struct *table = XHASHTABLE (vector);

  table->header = Qhashtable;
  table->harray = Qnil;
  table->zero_entry = Qunbound;
  XFASTINT (table->fullness) = 0;
  return (vector);
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
  if (bytes <= XVECTOR (ht->harray)->size * sizeof (Lisp_Object))
    abort ();
  new_vector = Fmake_vector (make_number (bytes / sizeof (Lisp_Object)), 0);
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
       "Returns T if OBJ is a hashtable, else NIL.")
  (obj)
  Lisp_Object obj;
{
  return (HASHTABLEP (obj)) ? Qt : Qnil;
}

DEFUN ("make-hashtable", Fmake_hashtable, Smake_hashtable, 1, 1, 0,
       "Make a hashtable of initial size SIZE.")
  (size)
  Lisp_Object size;
{
  Lisp_Object result;
  struct hashtable_struct *table;
  int tem;

  CHECK_FIXNUM (size, 0);
  tem = XINT (size);
  if (tem <= 0)
    error ("Bad size argument, %d, to make-hashtable.", tem);

  tem = compute_harray_size (tem);
  result = new_hashtable ();
  table = XHASHTABLE (result);
  table->harray = Fmake_vector (make_number (tem * LISP_OBJECTS_PER_HENTRY),
				0);
  return (result);
}


DEFUN ("copy-hashtable", Fcopy_hashtable, Scopy_hashtable, 1, 1, 0,
       "Make a new hashtable which contains the same keys and values\n\
as the given table.  The keys and values will not themselves be copied.")
  (old_table)
  Lisp_Object old_table;
{
  struct _C_hashtable old_htbl;
  struct _C_hashtable new_htbl;
  struct hashtable_struct *new_ht;
  Lisp_Object result;

  CHECK_HASHTABLE (old_table, 0);
  ht_copy_to_c (old_table, &old_htbl);

  /* we can't just call Fmake_hashtable() here because that will make a
     table that is slightly larger than the one we're trying to copy,
     which will make copy_hash() blow up. */
  result = new_hashtable ();
  new_ht = XHASHTABLE (result);
  new_ht->fullness = make_number (0);
  new_ht->zero_entry = Qunbound;
  new_ht->harray = Fmake_vector (Flength (XHASHTABLE (old_table)->harray), 0);
  ht_copy_to_c (result, &new_htbl);
  copy_hash (&new_htbl, &old_htbl);
  ht_copy_from_c (&new_htbl, result);
  return (result);
}


DEFUN ("gethash", Fgethash, Sgethash, 2, 3, 0,
       "Find hash value for KEY in TABLE.\n\
If there is no corresponding value, return DEFAULT (default nil)")
  (key, table, defalt)
  Lisp_Object key, table, defalt; /* One can't even spell correctly in C */ 
{
  Lisp_Object val;
  struct _C_hashtable htbl;
  CHECK_HASHTABLE (table, 0);
  ht_copy_to_c (table, &htbl);
  if (gethash ((void *)key, &htbl, (void **)&val))
    return val;
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
  ht_copy_to_c (table, &htbl);
  remhash ((void *)key, &htbl);
  ht_copy_from_c (&htbl, table);
  return Qnil;
}


DEFUN ("puthash", Fputhash, Sputhash, 3, 3, 0,
       "Hash KEY to VAL in TABLE.")
  (key, val, table)
  Lisp_Object key, val, table;
{
  struct hashtable_struct *ht;
  CHECK_HASHTABLE (table, 0);
  ht = XHASHTABLE (table);
  if ((int) val == 0)
    ht->zero_entry = val;
  else
  {
    struct gcpro gcpro1, gcpro2, gcpro3;
    struct _C_hashtable htbl;
    ht_copy_to_c (table, &htbl);
    GCPRO3 (key, val, table);
    puthash ((void *)key, (void *)val, &htbl);
    ht_copy_from_c (&htbl, table);
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
  ht_copy_to_c (table, &htbl);
  clrhash (&htbl);
  ht_copy_from_c (&htbl, table);
  return Qnil;
}

DEFUN ("hashtable-fullness", Fhashtable_fullness, Shashtable_fullness, 1, 1, 0,
       "Returns number of entries in TABLE.")
  (table)
  Lisp_Object table;
{
  struct _C_hashtable htbl;
  CHECK_HASHTABLE (table, 0);
  ht_copy_to_c (table, &htbl);
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
            || EQ (funcar, Qmocklisp) 
            || EQ (funcar, Qautoload)))
      return;
  }
  signal_error (Qinvalid_function, list1 (function));
}

static void
lisp_maphash_function (key, val, fn)
     void *key, *val, *fn;
{
  call2 ((Lisp_Object) fn, (Lisp_Object) key, (Lisp_Object) val);
}


DEFUN ("maphash", Fmaphash, Smaphash, 2, 2, 0,
       "Map FUNCTION over entries in TABLE, calling it with two args,\n\
each key and value in the table.")
  (function, table)
  Lisp_Object function, table;
{
  struct _C_hashtable htbl;
  struct gcpro gcpro1, gcpro2;

  verify_function (function, "hashtable mapping function");
  CHECK_HASHTABLE (table, 0);
  ht_copy_to_c (table, &htbl);
  GCPRO2 (table, function);
  maphash (lisp_maphash_function, &htbl, (void *) function);
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
  ht_copy_to_c (table, &htbl);
  maphash (function, &htbl, closure);
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
  ht_copy_to_c (table, &htbl);
  (*op) (&htbl, arg1, arg2, arg3);
  ht_copy_from_c (&htbl, table);
  return;
}
#endif

void
syms_of_elhash () 
{
  defsubr(&Smake_hashtable);
  defsubr(&Scopy_hashtable);
  defsubr(&Sgethash);
  defsubr(&Sputhash);
  defsubr(&Sremhash);
  defsubr(&Sclrhash);
  defsubr(&Smaphash);
  defsubr(&Shashtable_fullness);

  defsymbol (&Qhashtable, "hashtable");
  defsymbol (&Qhashtablep, "hashtablep");
}
