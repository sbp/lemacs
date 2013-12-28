
#ifndef _EMACS_ELHASH_H_
#define _EMACS_ELHASH_H_

extern Lisp_Object Fmake_hashtable (Lisp_Object size);
extern Lisp_Object Fcopy_hashtable (Lisp_Object old_table);
extern Lisp_Object Fgethash (Lisp_Object obj, Lisp_Object table, 
                             Lisp_Object defalt);
extern Lisp_Object Fputhash (Lisp_Object obj, Lisp_Object val, 
                             Lisp_Object table);
extern Lisp_Object Fremhash (Lisp_Object obj, Lisp_Object table);
extern Lisp_Object Fhashtable_fullness (Lisp_Object table);

extern Lisp_Object make_lisp_hashtable (int size,
	 int (*test_function) (CONST void*,CONST void*),
	 unsigned long (*hash_function) (CONST void*));

extern Lisp_Object make_weak_hashtable (int size,
	 int (*test_function) (CONST void*,CONST void*),
	 unsigned long (*hash_function) (CONST void*));

extern void elisp_maphash (void (*fn) (CONST void *key, void *contents,
				       void *extra_arg),
                           Lisp_Object table, 
                           void *extra_arg);

extern void elisp_map_remhash (int (*fn) (CONST void *key,
					  CONST void *contents,
					  void *extra_arg),
                           Lisp_Object table, 
                           void *extra_arg);

extern void prune_weak_hashtables (int (*obj_marked_p) (Lisp_Object));

#endif /* _EMACS_ELHASH_H_ */
