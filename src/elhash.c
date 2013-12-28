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


struct hashtable_struct
{
  Lisp_Object header;
  Lisp_Object harray;
  Lisp_Object zero_set;
  Lisp_Object zero_entry;
  Lisp_Object size;
  Lisp_Object fullness;
};

#define SLOT_OFFSET(type, slot_name) \
  ((unsigned) (((char *) (&(((type *)0)->slot_name))) - ((char *) 0)))

#define VECTOR_CONTENTS_OFFSET (SLOT_OFFSET (struct Lisp_Vector, contents))

#define HARRAY_LENGTH_FROM_BYTES(bytes) ((bytes) / sizeof (Lisp_Object))
#define HARRAY_LENGTH_FROM_SIZE(size) \
  (((size) * sizeof (hentry)) / sizeof (Lisp_Object))
#define HT_SIZE_FROM_BYTES(bytes) ((bytes) / sizeof (hentry))

#define DECLARE_HT(var) \
  struct _C_hashtable tmp_HT_Space; \
  c_hashtable var = &tmp_HT_Space;

#define CLEAR_HT(var) \
  bzero ((char *) var, sizeof (struct _C_hashtable));

#define INIT_HT(var, table) \
{ \
  struct hashtable_struct *tmp = XHASHTABLE(table); \
  var->harray = (void *) XVECTOR (tmp->harray)->contents; \
  var->zero_set = (NILP(tmp->zero_set))?0:1; \
  var->zero_entry = (void *) tmp->zero_entry; \
  var->size = XFASTINT (tmp->size); \
  var->fullness = XFASTINT (tmp->fullness); \
  var->hash_function = 0; \
  var->test_function = 0; \
  var->elisp_table = (void *) table; \
}

#define RESET_HT(var, table) \
{ \
  struct hashtable_struct *tmp = XHASHTABLE(table); \
  char *vec_addr = ((char *) var->harray) - VECTOR_CONTENTS_OFFSET; \
  XSET (tmp->harray, Lisp_Vector, vec_addr); \
  tmp->zero_set = (var->zero_set)?Qt:Qnil; \
  tmp->zero_entry = (Lisp_Object) var->zero_entry; \
  XSET (tmp->size, Lisp_Int, var->size); \
  XSET (tmp->fullness, Lisp_Int, var->fullness); \
}

#define HT_SIZE (sizeof(struct hashtable_struct) / sizeof (Lisp_Object))

#define NEW_HASHTABLE() (Fmake_vector (make_number(HT_SIZE), Qnil))

#define XHASHTABLE(obj) ((struct hashtable_struct *) (XVECTOR (obj)->contents))

#define HASHTABLE_HEADER(obj) (XHASHTABLE(obj)->header)
#define HASHTABLE_HARRAY(obj) (XHASHTABLE(obj)->harray)
#define HASHTABLE_ZERO_SET(obj) (XHASHTABLE(obj)->zero_set)
#define HASHTABLE_ZERO_ENTRY(obj) (XHASHTABLE(obj)->zero_entry)
#define HASHTABLE_SIZE(obj) (XHASHTABLE(obj)->size)
#define HASHTABLE_FULLNESS(obj) (XHASHTABLE(obj)->fullness)

#define HASHTABLEP(obj) ((XTYPE (obj) == Lisp_Vector) && \
                         (XFASTINT(XVECTOR (obj)->size) == HT_SIZE) && \
                         (HASHTABLE_HEADER (obj) == Qhashtable))

#define CHECK_HASHTABLE(x, i) \
  { if (!HASHTABLEP(x)) wrong_type_argument (Qhashtablep, (x)); }
 
Lisp_Object Qhashtable;
Lisp_Object Qhashtablep;
Lisp_Object Qhashemptymarker;

char *
elisp_hvector_malloc (bytes, table)
     unsigned int bytes;
     Lisp_Object table;
{
  Lisp_Object new_vector;

  if (!HASHTABLEP(table)) 
    error ("Bad table argument 0x%x -- can't resize hashtable.", table);
  
  if (HT_SIZE_FROM_BYTES (bytes) <= XFASTINT (HASHTABLE_SIZE (table)))
    error ("Bad resize argument, %d, for hashtable of size %d.", 
           HT_SIZE_FROM_BYTES (bytes), XFASTINT(HASHTABLE_SIZE (table)));

  new_vector = Fmake_vector (make_number (HARRAY_LENGTH_FROM_BYTES (bytes)),
                             make_number (0));

  return ((char *) (XVECTOR(new_vector)->contents));
}

void
elisp_hvector_free (ptr, table)
     void *ptr;
     Lisp_Object table;
{
  Lisp_Object current_vector;

  if (!HASHTABLEP(table)) 
    error ("Bad table argument 0x%x -- can't free hash array.", table);
  
  current_vector = HASHTABLE_HARRAY (table);

  if (XTYPE (current_vector) != Lisp_Vector)
    error ("Garbled hashtable 0x%x -- can't free hash array.", table);

  if (((void *) XVECTOR(current_vector)->contents) != ptr)
    error ("Bad ptr argument 0x%x -- can't free hash array = 0x%x.", 
           ptr, current_vector);
  else
    HASHTABLE_HARRAY (table) = Qnil; /* let GC do the rest */
  
  return;
}


DEFUN ("hashtablep", Fhashtablep, Shashtablep, 1, 1, 0,
       "Returns T if OBJ is a hashtable, else NIL.")
  (obj)
  Lisp_Object obj;
{
  return (HASHTABLEP(obj))?Qt:Qnil;
}

DEFUN ("make-hashtable", Fmake_hashtable, Smake_hashtable, 1, 1, 0,
       "Make a hashtable of size SIZE.")
  (size)
  Lisp_Object size;
{
  struct gcpro gcpro1;
  Lisp_Object hashtable = Qnil;

  CHECK_NUMBER (size, 0);
  if (XINT(size) <= 0) 
    error ("Bad SIZE argument, %d, to make-hashtable.", XINT(size));

  hashtable = NEW_HASHTABLE();
  GCPRO1 (hashtable);

  HASHTABLE_HEADER (hashtable) = Qhashtable;
  HASHTABLE_SIZE (hashtable) = make_number (compute_harray_size (XINT(size)));
  XSET (HASHTABLE_FULLNESS (hashtable), Lisp_Int, 0);
  XSET (HASHTABLE_ZERO_ENTRY (hashtable), Lisp_Int, 0);
  HASHTABLE_ZERO_SET (hashtable) = Qnil;
  {
    int vector_length = 
      HARRAY_LENGTH_FROM_SIZE (XINT (HASHTABLE_SIZE (hashtable)));
    HASHTABLE_HARRAY (hashtable) = 
      Fmake_vector (make_number (vector_length), make_number (0));
  }

  UNGCPRO;
  return hashtable;
}


DEFUN ("copy-hashtable", Fcopy_hashtable, Scopy_hashtable, 1, 1, 0,
       "Make a new hashtable which contains the same keys and values\n\
as the given table.  The keys and values will not themselves be copied.")
  (old_table)
  Lisp_Object old_table;
{
  DECLARE_HT (old_htbl);
  CHECK_HASHTABLE (old_table, 0);
  INIT_HT (old_htbl, old_table);
  {
    Lisp_Object new_table;
    DECLARE_HT (new_htbl);
    CLEAR_HT (new_htbl);
    /* we can't just call Fmake_hashtable() here because that will make a
       table that is slightly larger than the one we're trying to copy,
       which will make copy_hash() blow up.
     */
    new_table = NEW_HASHTABLE ();
    HASHTABLE_HEADER (new_table) = Qhashtable;
    HASHTABLE_SIZE (new_table) = HASHTABLE_SIZE (old_table);
    XSET (HASHTABLE_FULLNESS (new_table), Lisp_Int, 0);
    XSET (HASHTABLE_ZERO_ENTRY (new_table), Lisp_Int, 0);
    HASHTABLE_ZERO_SET (new_table) = Qnil;
    HASHTABLE_HARRAY (new_table) = 
      Fmake_vector (Flength (HASHTABLE_HARRAY (old_table)), make_number (0));
    INIT_HT (new_htbl, new_table);
    copy_hash (new_htbl, old_htbl);
    RESET_HT (new_htbl, new_table);
    return new_table;
  }
}


DEFUN ("gethash", Fgethash, Sgethash, 2, 2, 0,
       "Find hash value for OBJ in TABLE.")
  (obj, table)
  Lisp_Object obj, table;
{
  Lisp_Object val;
  DECLARE_HT (htbl);
  CHECK_HASHTABLE (table, 0);
  INIT_HT (htbl, table);

  if (gethash ((void *)obj, htbl, (void **)&val))
    return val;
  else 
    return Qhashemptymarker;
}


DEFUN ("remhash", Fremhash, Sremhash, 2, 2, 0,
       "Remove hash value for OBJ in TABLE.")
  (obj, table)
  Lisp_Object obj, table;
{
  Lisp_Object val;

  DECLARE_HT (htbl);

  CHECK_HASHTABLE (table, 0);

  INIT_HT (htbl, table);

  remhash ((void *)obj, htbl);

  RESET_HT (htbl, table);

  return Qnil;
}


DEFUN ("puthash", Fputhash, Sputhash, 3, 3, 0,
       "Hash OBJ to VAL in TABLE.")
  (obj, val, table)
  Lisp_Object obj, val, table;
{
  struct gcpro gcpro1, gcpro2, gcpro3;
  DECLARE_HT (htbl);

  GCPRO3 (obj, val, table);

  CHECK_HASHTABLE (table, 0);

  INIT_HT (htbl, table);

  puthash ((void *)obj, (void *)val, htbl);

  RESET_HT (htbl, table);

  UNGCPRO;

  return val;
}

DEFUN ("clrhash", Fclrhash, Sclrhash, 1, 1, 0,
       "Flush TABLE.")
  (table)
  Lisp_Object table;
{
  DECLARE_HT (htbl);

  CHECK_HASHTABLE (table, 0);

  INIT_HT (htbl, table);

  clrhash (htbl);

  RESET_HT (htbl, table);

  return Qnil;
}


DEFUN ("hashtable-fullness", Fhashtable_fullness, Shashtable_fullness, 1, 1, 0,
       "Returns number of entries in TABLE.")
  (table)
  Lisp_Object table;
{
  DECLARE_HT (htbl);
  CHECK_HASHTABLE (table, 0);
  INIT_HT (htbl, table);
  return (make_number (htbl->fullness));
}


static void
verify_function (function, description)
     Lisp_Object function;
     char *description;
{
  int type = XTYPE (function);

  switch (type)
    {
    case Lisp_Symbol:
      if (NILP (function))
        return;
      else
        {
          if (!NILP(Ffboundp (function)))
            return;
          else
            error ("A symbol used as a %s must be fboundp -- %s isn't",
                   description, XSYMBOL (function)->name->data, 0);
          break;
        } 

    case Lisp_Subr:
    case Lisp_Compiled:
      return;
          
    case Lisp_Cons:
      {
        Lisp_Object funcar = Fcar (function);
        if ((XTYPE (funcar) == Lisp_Symbol) &&
            (EQ (funcar, Qlambda) ||
             EQ (funcar, Qmocklisp) ||
             EQ (funcar, Qautoload)))
          return;

        /* else case falls through to default error case */
      }
    default:
      {
        Lisp_Object error_string = Fprin1_to_string (function, Qnil);
        error ("Can't use %s as a %s", 
               XSTRING (error_string)->data, description, 0);
      }
    }
}


static void
lisp_maphash_function (key, val, fn)
     void *key, *val, *fn;
{
  call2 ((Lisp_Object) fn, (Lisp_Object) key, (Lisp_Object) val);
}


DEFUN ("maphash", Fmaphash, Smaphash, 2, 2, 0,
       "Map FUNCTION over entries in TABLE with, calling it with two\n\
arguments, each key and value in the table.")
  (function, table)
  Lisp_Object function;
{
  DECLARE_HT (htbl);

  verify_function (function, "hashtable mapping function");
  CHECK_HASHTABLE (table, 0);

  INIT_HT (htbl, table);

  maphash (lisp_maphash_function, htbl, (void *) function);

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
  DECLARE_HT (htbl);
  CHECK_HASHTABLE (table, 0);
  INIT_HT (htbl, table);
  maphash (function, htbl, closure);
}


void
elisp_table_op (table, op, arg1, arg2, arg3)
     Lisp_Object table;
     generic_hashtable_op op;
     void *arg1;
     void *arg2;
     void *arg3;

{
  DECLARE_HT (htbl);

  CHECK_HASHTABLE (table, 0);

  INIT_HT (htbl, table);

  (*op) (htbl, arg1, arg2, arg3);

  RESET_HT (htbl, table);

  return;
}


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

  Qhashtable = intern ("hashtable");
  staticpro (&Qhashtable);

  Qhashtablep = intern ("hashtablep");
  staticpro (&Qhashtablep);

  Qhashemptymarker = intern ("empty-marker");
  staticpro (&Qhashemptymarker);
  Fset (Qhashemptymarker, Qhashemptymarker);
}
