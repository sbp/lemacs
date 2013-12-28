
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

extern void elisp_maphash (void (*fn) (void* key, void* contents, void* arg),
                           Lisp_Object table, 
                           void *closure);

#endif /* _EMACS_ELHASH_H_ */
