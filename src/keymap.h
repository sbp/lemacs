
struct Lisp_Keymap {
  /* The first two fields are really the header of a vector */
  int size;			/* must be KEYMAP_SIZE */
  struct Lisp_Vector *vec_next;	/* GC pointer */
  Lisp_Object parent;		/* The keymap to be searched after this one */
  Lisp_Object table;		/* The contents of this keymap */
  Lisp_Object inverse_table;	/* The inverse mapping of the above */

  Lisp_Object sub_maps_cache;	/* Cache of directly inferior keymaps;
				   This holds an alist, of the key and the
				   maps, or the modifier bit and the map.
				   If this is the symbol t, then the cache
				   needs to be recomputed.
				 */
};

#define KEYMAP_SIZE ((sizeof (struct Lisp_Keymap) / sizeof (int)) - 2)
