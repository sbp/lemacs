#ifndef _HASH_H_
#define _HASH_H_

typedef struct
{
  CONST void *key;
  void	     *contents;
} hentry;

struct _C_hashtable
{
  hentry	*harray;
  int		zero_set;
  void		*zero_entry;
  unsigned int	size;		/* size of the hasharray */
  unsigned int	fullness;	/* number of entries in the hashtable */
  unsigned long (*hash_function) (CONST void *);
  int		(*test_function) (CONST void *, CONST void *);
#ifdef emacs
  Lisp_Object elisp_table;
#endif
};

typedef struct _C_hashtable *c_hashtable;

/* size is the number of initial entries. The hashtable will be grown
   automatically if the number of entries approaches the size */
extern c_hashtable make_hashtable (unsigned int size);

extern c_hashtable make_general_hashtable 
  (unsigned int hsize, unsigned long (*hash_function) (CONST void *),
   int (*test_function)(CONST void *, CONST void *));

extern c_hashtable make_strings_hashtable (unsigned int hsize);

/* clears the hash table. A freshly created hashtable is already cleared up */
extern void clrhash (c_hashtable hash);

/* frees the table and substructures */
extern void free_hashtable (c_hashtable hash);

/* returns a hentry whose key is 0 if the entry does not exist in hashtable */
extern CONST void* gethash (CONST void* key, c_hashtable hash, 
                            CONST void** ret_value);

/* key should be different from 0 */
extern void puthash (CONST void* key, void* contents, c_hashtable hash);

/* delete the entry which key is key */
extern void remhash (CONST void* key, c_hashtable hash);

typedef void (*maphash_function) (CONST void* key, void* contents, 
                                  void* arg);

typedef int (*remhash_predicate) (CONST void* key, CONST void* contents,
                                  void* arg);

typedef void (*generic_hashtable_op) (c_hashtable table, 
                                      void *arg1, void *arg2, void *arg3);

/* calls mf with the following arguments:  key, contents, arg; for every 
   entry in the hashtable */
extern void maphash (maphash_function fn, c_hashtable hash, void* arg);

/* delete objects from the table which satisfy the predicate */
extern void 
map_remhash (remhash_predicate predicate, c_hashtable hash, void *arg);

/* copies all the entries of src into dest -- dest is modified as needed
   so it is as big as src. */ 
extern void copy_hash (c_hashtable dest, c_hashtable src);

#ifdef emacs	/* for elhash.c */
extern unsigned int compute_harray_size (unsigned int);
#endif

#endif /* _HASH_H_ */
