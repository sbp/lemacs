/* Manipulation of keymaps
   Copyright (C) 1985, 1991, 1992, 1993, 1994 Free Software Foundation, Inc.
   Totally redesigned by jwz in 1991.

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
#include <stdio.h>

#include "lisp.h"
#include "intl.h"
#include "keymap.h"
#include "commands.h"
#include "buffer.h"
#include "insdel.h"
#include "events.h"
#include "elhash.h"


/* A keymap contains four slots:

   parents	   Ordered list of keymaps to search after
                   this one if no match is found.
		   Keymaps can thus be arranged in a hierarchy.

   table	   A hash table, hashing keysyms to their bindings.
		   As in the rest of emacs, a keysym is either a symbol or
		   an integer, which is an ASCII code (of one of the printing
		   ASCII characters: not 003 meaning C-c, for instance.)

   inverse_table   A hash table, hashing bindings to the list of keysyms
		   in this keymap which are bound to them.  This is to make
		   the Fwhere_is_internal() function be fast.  It needs to be
		   fast because we want to be able to call it in realtime to
		   update the keyboard-equivalents on the pulldown menus.
                   Values of the table are either atoms (keysyms)
                   or a dotted list of keysyms.

   sub_maps_cache  An alist; for each entry in this keymap whose binding is
		   a keymap (that is, Fkeymapp()) this alist associates that
		   keysym with that binding.  This is used to optimize both
		   Fwhere_is_internal() and Faccessible_keymaps().  This slot
		   gets set to the symbol `t' every time a change is made to
		   this keymap, causing it to be recomputed when next needed.

   prompt          >>>

   default_binding >>>

   Sequences of keys are stored in the obvious way: if the sequence of keys
   "abc" was bound to some command `foo', the hierarchy would look like

      keymap-1: associates "a" with keymap-2
      keymap-2: associates "b" with keymap-3
      keymap-3: associates "c" with foo

>>>> FIX THIS OBSOLETE DOC
   However, bucky bits ("modifiers" to the X-minded) are represented in the
   keymap hierarchy as well.  (This lets us use EQable objects as hash keys.)
   If the key `C-a' was bound to some command, the hierarchy would look like

      keymap-1: associates the symbol `control' with keymap-2
      keymap-2: associates "a" with the command

   Likewise for all other bucky bits: meta, super, hyper, symbol, and shift.
   The ordering of these is strict: there will never be a `control' submap
   of a keymap which is bound to `meta', because that could lead to
   ambiguities.

   When associating a command with C-M-a, we'd have

      keymap-1: associates the symbol `control' with keymap-2
      keymap-2: associates the symbol `meta' with keymap-3
      keymap-3: associates "a" with the command

   Note that keymap-2 might have normal bindings in it, and keymap-1 might
   have a keymap bound to `meta' in it.  That would be the meta-map.
   Keymap-2 is the "control" map.  Keymap-3 is the "control-meta" map.

   If the command that "a" was bound to in keymap-3 was itself a keymap,
   then that would make the key "C-M-a" be a prefix character.
>>>>

   Note that this new model of keymaps takes much of the magic away from
   the Escape key: the value of the variable `esc-map' is no longer indexed
   in the `global-map' under the ESC key.  It's indexed under the symbol
   `meta'.  This is not user-visible, however; none of the "bucky" maps are.

   There is a hack in Flookup_key() that makes (lookup-key global-map "\^[")
   and (define-key some-random-map "\^[" my-esc-map) work as before, for
   compatibility.

   Since keymaps are opaque, the only way to extract information from them
   is with the functions lookup-key, key-binding, local-key-binding, and
   global-key-binding, which work just as before, and the new function
   map-keymap, which is roughly analagous to maphash.  

   Note that map-keymap perpetuates the illusion that the "bucky" submaps
   don't exist: if you map over a keymap with bucky submaps, it will also
   map over those submaps.  It does not, however, map over other random
   submaps of the keymap, just the bucky ones.

   One implication of this is that when you map over `global-map', you will
   also map over `esc-map'.  It is merely for compatibility that the esc-map
   is accessible at all; I think that's a bad thing, since it blurs the
   distinction between ESC and "meta" even more.  "M-x" is no more a two-
   key sequence than "C-x" is.

 */

struct keymap {
  struct lcrecord_header header;
  Lisp_Object parents;		/* Keymaps to be searched after this one
				 *  An ordered list */
  Lisp_Object prompt;           /* Qnil or a string to print in the minibuffer
                                 *  when reading from this keymap */
                                   
  Lisp_Object table;		/* The contents of this keymap */
  Lisp_Object inverse_table;	/* The inverse mapping of the above */

  Lisp_Object default_binding;  /* Use this if no other binding is found
                                 *  (this overrides parent maps and the
                                 *   normal global-map lookup.) */


  Lisp_Object sub_maps_cache;	/* Cache of directly inferior keymaps;
				   This holds an alist, of the key and the
				   maps, or the modifier bit and the map.
				   If this is the symbol t, then the cache
				   needs to be recomputed.
				 */
  int fullness;			/* How many entries there are in this table.
 				   This should be the same as the fullness
 				   of the `table', but hash.c is broken. */
  Lisp_Object name;             /* Just for debugging convenience */
};

#define XKEYMAP(a) ((struct keymap *) XPNTR(a))


#define min(a, b) ((a) < (b) ? (a) : (b))

/* Hash key is shifted so it can't conflict with eight-bit
   string-char constituents */
#define MAKE_MODIFIER_HASH_KEY(modifier) (make_number ((modifier) << 16))
#define MODIFIER_HASH_KEY_P(x) ((FIXNUMP((x))) ? (XINT ((x)) >> 16) : 0)



/* Actually allocate storage for these variables */

static Lisp_Object Vcurrent_global_map; /* Always a keymap */

static Lisp_Object Vmouse_grabbed_buffer;

/* Alist of minor mode variables and keymaps.  */
static Lisp_Object Qminor_mode_map_alist;

/* static Lisp_Object Qoverriding_local_map; */
static Lisp_Object Voverriding_local_map;


/* This is incremented whenever a change is made to a keymap.  This is
   so that things which care (such as the menubar code) can recompute
   privately-cached data when the user has changed keybindings.
 */
int keymap_tick;

/* Prefixing a key with this character is the same as sending a meta bit. */
int meta_prefix_char;

Lisp_Object Qkeymap;
Lisp_Object Qkeymapp;

Lisp_Object Vsingle_space_string;

Lisp_Object Qsuppress_keymap;

Lisp_Object Qmode_line_map;

static Lisp_Object describe_buffer_bindings (Lisp_Object descbuf);
static Lisp_Object describe_buffer_mouse_bindings (Lisp_Object descbuf);
static void describe_command (Lisp_Object definition);
static void describe_map (Lisp_Object keymap, Lisp_Object elt_prefix,
			  void (*elt_describer) (Lisp_Object),
			  int partial, 
			  Lisp_Object shadow, 
			  int mice_only_p);
Lisp_Object Qcontrol, Qctrl, Qmeta, Qsuper, Qhyper, Qsymbol, Qshift;
Lisp_Object Qbutton0, Qbutton1, Qbutton2, Qbutton3, Qbutton4, Qbutton5,
  Qbutton6, Qbutton7;
Lisp_Object Qbutton0up, Qbutton1up, Qbutton2up, Qbutton3up, Qbutton4up,
  Qbutton5up, Qbutton6up, Qbutton7up;
Lisp_Object Qmenu_selection;

/* Kludge kludge kludge */
Lisp_Object QLFD, QTAB, QRET, QESC, QDEL, QSPC, QBS;


static Lisp_Object mark_keymap (Lisp_Object, void (*) (Lisp_Object));
static void print_keymap (Lisp_Object, Lisp_Object, int);
/* Noo need for keymap_equal */
DEFINE_LRECORD_IMPLEMENTATION ("keymap", lrecord_keymap,
                               mark_keymap, print_keymap, 0, 0,
			       sizeof (struct keymap));
static Lisp_Object
mark_keymap (Lisp_Object obj, void (*markobj) (Lisp_Object))
{
  struct keymap *keymap = XKEYMAP (obj);
  ((markobj) (keymap->parents));
  ((markobj) (keymap->prompt));
  ((markobj) (keymap->inverse_table));
  ((markobj) (keymap->sub_maps_cache));
  ((markobj) (keymap->default_binding));
  ((markobj) (keymap->name));
  return (keymap->table);
}
  
static void
print_keymap (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  struct keymap *keymap = XKEYMAP (obj);
  char buf[200];
  int size = XFASTINT (Fkeymap_fullness (obj));
  if (print_readably)
    error ("printing unreadable object #<keymap 0x%x>", (long) keymap);
  write_string_1 ("#<keymap ", -1, printcharfun);
  if (!NILP (keymap->name))
    print_internal (keymap->name, printcharfun, 1);
  sprintf (buf, "%s%d entr%s 0x%x>",
           ((NILP (keymap->name)) ? "" : " "),
           size,
           ((size == 1) ? "y" : "ies"),
           (long) keymap);
  write_string_1 (buf, -1, printcharfun);
}


static Lisp_Object
traverse_keymaps (Lisp_Object start_keymap, Lisp_Object start_parents,
                  Lisp_Object (*mapper) (Lisp_Object keymap, void *mapper_arg),
                  void *mapper_arg)
{
  Lisp_Object keymap;
  Lisp_Object tail = start_parents;
  Lisp_Object malloc_sucks[10];
  Lisp_Object malloc_bites = Qnil;
  int stack_depth = 0;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
  GCPRO4 (*malloc_sucks, malloc_bites, start_keymap, tail);
  gcpro1.nvars = 0;

  start_keymap = get_keymap (start_keymap, 1, 1);
  keymap = start_keymap;
  /* Hack special-case parents at top-level */
  tail = ((!NILP (tail)) ? tail : XKEYMAP (keymap)->parents);

  for (;;)
  {
    Lisp_Object result;

    QUIT;
    result = ((mapper) (keymap, mapper_arg));
    if (!NILP (result))
    {
      while (CONSP (malloc_bites))
      {
        struct Lisp_Cons *victim = XCONS (malloc_bites);
        malloc_bites = victim->cdr;
        free_cons (victim);
      }
      UNGCPRO;
      return (result);
    }
    if (NILP (tail))
    {
      if (stack_depth == 0)
      {
        UNGCPRO;
        return (Qnil);          /* Nothing found */
      }
      stack_depth--;
      if (CONSP (malloc_bites))
      {
        struct Lisp_Cons *victim = XCONS (malloc_bites);
        tail = victim->car;
        malloc_bites = victim->cdr;
        free_cons (victim);
      }
      else
      {
        tail = malloc_sucks[stack_depth];
        gcpro1.nvars = stack_depth;
      }
      keymap = XCONS (tail)->car;
      tail = XCONS (tail)->cdr;
    }
    else if (!CONSP (tail))
      abort ();
    else
    {
      Lisp_Object parents;

      keymap = XCONS (tail)->car;
      if (!KEYMAPP (keymap)) abort ();
      tail = XCONS (tail)->cdr;
      parents = XKEYMAP (keymap)->parents;
      if (!CONSP (parents))
        ;
      else if (NILP (tail))
        /* Tail-recurse */
        tail = parents;
      else
      {
        if (CONSP (malloc_bites))
          malloc_bites = Fcons (tail, malloc_bites);
        else if (stack_depth < countof (malloc_sucks))
        {
          malloc_sucks[stack_depth++] = tail;
          gcpro1.nvars = stack_depth;
        }
        else
        {
          /* *&@##[*&^$ C. @#[$*&@# Unix.  Losers all. */
          int i;
          for (i = 0, malloc_bites = Qnil;
               i < countof (malloc_sucks);
               i++)
            malloc_bites = Fcons (malloc_sucks[i], malloc_bites);
          gcpro1.nvars = 0;
        }
        tail = parents;
      }
    }
    keymap = get_keymap (keymap, 1, 1);
    if (EQ (keymap, start_keymap))
    {
      signal_simple_error (GETTEXT("Cyclic keymap indirection"),
                           start_keymap);
    }
  }
}


static unsigned int
bucky_sym_to_bucky_bit (Lisp_Object sym)
{
  if (EQ (sym, Qcontrol))
    return MOD_CONTROL;
  else if (EQ (sym, Qmeta))
    return MOD_META;
  else if (EQ (sym, Qsuper))
    return MOD_SUPER;
  else if (EQ (sym, Qhyper))
    return MOD_HYPER;
  else if (EQ (sym, Qsymbol))
    return MOD_SYMBOL;
  else if (EQ (sym, Qshift))
    return MOD_SHIFT;
  else
    return 0;
}

static Lisp_Object
control_meta_superify (Lisp_Object frob, unsigned int modifiers)
{
  if (modifiers == 0)
    return frob;
  frob = Fcons (frob, Qnil);
  if (modifiers & MOD_SHIFT)
    frob = Fcons (Qshift, frob);
  if (modifiers & MOD_SYMBOL)
    frob = Fcons (Qsymbol, frob);
  if (modifiers & MOD_HYPER)
    frob = Fcons (Qhyper, frob);
  if (modifiers & MOD_SUPER)
    frob = Fcons (Qsuper, frob);
  if (modifiers & MOD_CONTROL)
    frob = Fcons (Qcontrol, frob);
  if (modifiers & MOD_META)
    frob = Fcons (Qmeta, frob);
  return (frob);
}

static Lisp_Object
make_key_description (CONST struct key_data *key, int prettify)
{
  Lisp_Object keysym = key->keysym;
  unsigned int modifiers = key->modifiers;

  if (prettify && FIXNUMP (keysym))
    {
      /* This is a little slow, but (control a) is prettier than (control 65).
	 It's now ok to do this for digit-chars too, since we've fixed the
	 bug where \9 read as the integer 9 instead of as the symbol with
	 "9" as its name.
       */
      char str [2];
      str[0] = XINT (keysym);
      str[1] = 0;
      keysym = intern (str);
    }
  return (control_meta_superify (keysym, modifiers));
}


/* Relies on caller to gc-protect args */
static Lisp_Object
keymap_lookup_directly (Lisp_Object keymap,
                        Lisp_Object keysym, unsigned int modifiers)
{
  struct keymap *k;

  if (!KEYMAPP (keymap)) abort ();
  if ((modifiers & ~(MOD_CONTROL | MOD_META | MOD_SUPER | MOD_HYPER
                     | MOD_SYMBOL | MOD_SHIFT)) != 0)
    abort ();

  k = XKEYMAP (keymap);

  /* If the keysym is a one-character symbol, use the char code instead. */
  if (SYMBOLP (keysym) && string_length (XSYMBOL (keysym)->name) == 1)
    keysym = make_number (XSYMBOL (keysym)->name->data [0]);

  if (modifiers != 0)
  {
    Lisp_Object submap = Fgethash (MAKE_MODIFIER_HASH_KEY (modifiers),
                                   k->table, Qnil);
    if (NILP (submap))
      return (Qnil);
    if (!KEYMAPP (submap))
      abort ();
    k = XKEYMAP (submap);
  }
  return (Fgethash (keysym, k->table, Qnil));
}

static void
keymap_store_inverse_internal (Lisp_Object inverse_table,
                               Lisp_Object keysym,
                               Lisp_Object value)
{
  Lisp_Object keys = Fgethash (value, inverse_table, Qunbound);

  if (EQ (keys, Qunbound))
    {
      keys = keysym;
      /* Don't cons this unless necessary */
      /* keys = Fcons (keysym, Qnil); */
      Fputhash (value, keys, inverse_table);
    }

  else if (!CONSP (keys))
    {
      /* Now it's necessary to cons */
      keys = Fcons (keys, keysym);
      Fputhash (value, keys, inverse_table);
    }
  else
    {
      while (CONSP (Fcdr (keys)))
	keys = XCONS (keys)->cdr;
      XCONS (keys)->cdr = Fcons (XCONS (keys)->cdr, keysym);
      /* No need to call puthash because we've destructively
         modified the list tail in place */
    }
}


static void
keymap_delete_inverse_internal (Lisp_Object inverse_table,
                                Lisp_Object keysym, 
                                Lisp_Object value)
{
  Lisp_Object keys = Fgethash (value, inverse_table, Qunbound);
  Lisp_Object new_keys = keys;
  Lisp_Object tail;
  Lisp_Object *prev;

  if (EQ (keys, Qunbound))
    abort ();

  for (prev = &new_keys, tail = new_keys;
       ;
       prev = &(XCONS (tail)->cdr), tail = XCONS (tail)->cdr)
    {
      if (EQ (tail, keysym))
	{
	  *prev = Qnil;
	  break;
	}
      else if (!CONSP (tail))
	abort ();
      else if (EQ (keysym, XCONS (tail)->car))
	{
	  *prev = XCONS (tail)->cdr;
	  break;
	}
    }

  if (NILP (new_keys))
    Fremhash (value, inverse_table);
  else if (!EQ (keys, new_keys))
    /* Removed the first elt */
    Fputhash (value, new_keys, inverse_table);
  /* else the list's tail has been modified, so we don't need to
     touch the hash table again (the pointer in there is ok).
   */
}


static void
keymap_store_internal (Lisp_Object keysym, struct keymap *keymap,
		       Lisp_Object value)
{
  Lisp_Object prev_value = Fgethash (keysym, keymap->table, Qnil);

  if (EQ (prev_value, value))
      return;
  if (!NILP (prev_value))
    keymap_delete_inverse_internal (keymap->inverse_table, 
                                    keysym, prev_value);
  if (NILP (value))
    {
      keymap->fullness--;
      if (keymap->fullness < 0) abort ();
      Fremhash (keysym, keymap->table);
    }
  else
    {
      if (NILP (prev_value))
	keymap->fullness++;
      Fputhash (keysym, value, keymap->table);
      keymap_store_inverse_internal (keymap->inverse_table, 
                                     keysym, value);
    }
  keymap_tick++;
}


static Lisp_Object
create_bucky_submap (struct keymap *k, unsigned int modifiers,
                     Lisp_Object parent_for_debugging_info)
{
  Lisp_Object submap = Fmake_sparse_keymap ();
  /* User won't see this, but it is nice for debugging Emacs */
  XKEYMAP (submap)->name
    = control_meta_superify (parent_for_debugging_info, modifiers);
  /* Invalidate cache */
  k->sub_maps_cache = Qt;
  keymap_store_internal (MAKE_MODIFIER_HASH_KEY (modifiers), k, submap);
  return (submap);
}


/* Relies on caller to gc-protect keymap, keysym, value */
static void
keymap_store (Lisp_Object keymap, CONST struct key_data *key,
              Lisp_Object value)
{
  Lisp_Object keysym = key->keysym;
  unsigned int modifiers = key->modifiers;
  struct keymap *k;

  if (!KEYMAPP (keymap)) abort ();
  if ((modifiers & ~(MOD_CONTROL | MOD_META | MOD_SUPER | MOD_HYPER
                     | MOD_SYMBOL | MOD_SHIFT)) != 0)
    abort ();

  k = XKEYMAP (keymap);

  /* If the keysym is a one-character symbol, use the char code instead. */
  if (SYMBOLP (keysym) && string_length (XSYMBOL (keysym)->name) == 1)
    keysym = make_number (XSYMBOL (keysym)->name->data [0]);

  if (modifiers != 0)
  {
    Lisp_Object submap = Fgethash (MAKE_MODIFIER_HASH_KEY (modifiers),
                                   k->table, Qnil);
    if (NILP (submap))
      submap = create_bucky_submap (k, modifiers, keymap);
    else if (!KEYMAPP (submap))
      abort ();
    k = XKEYMAP (submap);
  }
  k->sub_maps_cache = Qt; /* Invalidate cache */
  keymap_store_internal (keysym, k, value);
}



struct keymap_submaps_closure
{
  Lisp_Object *result_locative;
};

static void
keymap_submaps_mapper_0 (CONST void *hash_key, void *hash_contents, 
                         void *keymap_submaps_closure)
{
  Lisp_Object contents;
  VOID_TO_LISP (contents, hash_contents);
  /* Perform any autoloads, etc */
  (void) Fkeymapp (contents);
}

static void
keymap_submaps_mapper (CONST void *hash_key, void *hash_contents, 
                       void *keymap_submaps_closure)
{
  Lisp_Object key, contents;
  Lisp_Object *result_locative;
  struct keymap_submaps_closure *cl = keymap_submaps_closure;
  CVOID_TO_LISP (key, hash_key);
  VOID_TO_LISP (contents, hash_contents);
  result_locative = cl->result_locative;

  if (!NILP (Fkeymapp (contents)))
    *result_locative = Fcons (Fcons (key, contents), *result_locative);
}

static int map_keymap_sort_predicate (Lisp_Object obj1, Lisp_Object obj2, 
                                      Lisp_Object pred);

static Lisp_Object
keymap_submaps (Lisp_Object keymap)
{
  struct keymap *k = XKEYMAP (keymap);

  if (EQ (k->sub_maps_cache, Qt)) /* Unknown */
    {
      Lisp_Object result = Qnil;
      struct gcpro gcpro1, gcpro2;
      struct keymap_submaps_closure keymap_submaps_closure;

      GCPRO2 (keymap, result);
      keymap_submaps_closure.result_locative = &result;
      /* Do this first pass to touch (and load) any autoloaded maps */
      elisp_maphash (keymap_submaps_mapper_0, k->table,
		     &keymap_submaps_closure);
      result = Qnil;
      elisp_maphash (keymap_submaps_mapper, k->table,
		     &keymap_submaps_closure);
      /* keep it sorted so that the result of accessible-keymaps is ordered */
      k->sub_maps_cache = list_sort (result, 
				     Qnil,
				     map_keymap_sort_predicate);
      UNGCPRO;
    }
  return (k->sub_maps_cache);
}


static Lisp_Object
make_keymap (int size)
{
  Lisp_Object result;
  struct keymap *keymap = alloc_lcrecord (sizeof (struct keymap), 
                                          lrecord_keymap);

  XSETR (result, Lisp_Keymap, keymap);

  keymap->parents = Qnil;
  keymap->table = Qnil;
  keymap->prompt = Qnil;
  keymap->default_binding = Qnil;
  keymap->inverse_table = Qnil;
  keymap->sub_maps_cache = Qnil; /* No possible submaps */
  keymap->fullness = 0;
  if (size != 0) /* hack for copy-keymap */
    {
      keymap->table = Fmake_hashtable (make_number (size));
      /* Inverse table is often less dense because of duplicate key-bindings.
         If not, it will grow anyway. */
      keymap->inverse_table = Fmake_hashtable (make_number (size * 3 / 4));
    }
  keymap->name = Qnil;
  return (result);
}


DEFUN ("make-keymap", Fmake_keymap, Smake_keymap, 0, 0, 0,
  "Construct and return a new keymap object.  All entries in it are nil,\n\
meaning \"command undefined\".")
  ()
{
  return make_keymap (60);
}

DEFUN ("make-sparse-keymap", Fmake_sparse_keymap, Smake_sparse_keymap, 0, 0, 0,
  "Construct and return a new keymap object.  All entries in it are nil,\n\
meaning \"command undefined\".  The only difference between this function\n\
and make-keymap is that this function returns a \"smaller\" keymap (one\n\
that is expected to contain less entries.)  As keymaps dynamically resize,\n\
the distinction is not great.")
  ()
{
  return make_keymap (8);
}

DEFUN ("keymap-parents", Fkeymap_parents, Skeymap_parents, 1, 1, 0,
       "Returns the `parent' keymaps of the given keymap, or nil.\n\
The parents of a keymap are searched for keybindings when a key sequence\n\
isn't bound in this one.  `(current-global-map)' is the default parent\n\
of all keymaps.")
     (keymap)
     Lisp_Object keymap;
{
  keymap = get_keymap (keymap, 1, 1);
  return (Fcopy_sequence (XKEYMAP (keymap)->parents));
}

  
  
static Lisp_Object
traverse_keymaps_noop (Lisp_Object keymap, void *arg)
{
  return (Qnil);
}

DEFUN ("set-keymap-parents", Fset_keymap_parents, Sset_keymap_parents, 2, 2, 0,
       "Sets the `parent' keymaps of the given keymap.\n\
The parents of a keymap are searched for keybindings when a key sequence\n\
isn't bound in this one.  `(current-global-map)' is the default parent\n\
of all keymaps.")
     (keymap, parents)
     Lisp_Object keymap, parents;
{
  Lisp_Object k;
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (keymap, parents);
  keymap = get_keymap (keymap, 1, 1);

  if (KEYMAPP (parents))	/* backwards-compatibility */
    parents = list1 (parents);
  if (!NILP (parents))
  {
    Lisp_Object tail = parents;
    while (!NILP (tail))
      {
	QUIT;
	CHECK_CONS (tail, 0);
	k = XCONS (tail)->car;
	/* Require that it be an actual keymap object, rather than a symbol
	   with a (crockish) symbol-function which is a keymap */
	CHECK_KEYMAP (k, 1); /* get_keymap (k, 1, 1); */
	tail = XCONS (tail)->cdr;
      }
  }

  /* Check for circularities */
  traverse_keymaps (keymap, parents, traverse_keymaps_noop, 0);
  keymap_tick++;
  XKEYMAP (keymap)->parents = Fcopy_sequence (parents);
  UNGCPRO;
  return (parents);
}

DEFUN ("set-keymap-name", Fset_keymap_name, Sset_keymap_name, 2, 2, 0,
  "Sets the `name' of the KEYMAP to NEW-NAME\n\
The name is only a debugging convenience; it is not used except\n\
when printing the keymap.")
     (keymap, new_name)
     Lisp_Object keymap, new_name;
{
  keymap = get_keymap (keymap, 1, 1);

  XKEYMAP (keymap)->name = new_name;
  return (new_name);
}

/*
 * DEFUN ("keymap-name", Fkeymap_name, Skeymap_name, 1, 1, 0,
 *   "Returns the `name' of KEYMAP.\n\
 * The name is only a debugging convenience; it is not used except\n\
 * when printing the keymap.")
 *      (keymap)
 *      Lisp_Object keymap;
 * {
 *   keymap = get_keymap (keymap, 1, 1);
 * 
 *   return (XKEYMAP (keymap)->name);
 * }
 */

DEFUN ("set-keymap-prompt", Fset_keymap_prompt, Sset_keymap_prompt, 2, 2, 0,
  "Sets the `prompt' of KEYMAP to string NEW-PROMPT, or `nil'\n\
if no prompt is desired.  The prompt is shown in the echo-area\n\
when reading a key-sequence to be looked-up in this keymap.")
     (keymap, new_prompt)
     Lisp_Object keymap, new_prompt;
{
  keymap = get_keymap (keymap, 1, 1);
  
  if (!NILP (new_prompt))
    CHECK_STRING (new_prompt, 1);

  XKEYMAP (keymap)->prompt = new_prompt;
  return (new_prompt);
}

static Lisp_Object
keymap_prompt_mapper (Lisp_Object keymap, void *arg)
{
  return (XKEYMAP (keymap)->prompt);
}


DEFUN ("keymap-prompt", Fkeymap_prompt, Skeymap_prompt, 1, 2, 0,
  "Returns the `prompt' of the given keymap.\n\
If non-nil, the prompt is shown in the echo-area\n\
when reading a key-sequence to be looked-up in this keymap.")
     (keymap, use_inherited)
     Lisp_Object keymap, use_inherited;
{
  Lisp_Object prompt;

  keymap = get_keymap (keymap, 1, 1);
  prompt = XKEYMAP (keymap)->prompt;
  if (!NILP (prompt) || NILP (use_inherited))
    return (prompt);
  else
    return (traverse_keymaps (keymap, Qnil, keymap_prompt_mapper, 0));
}

DEFUN ("set-keymap-default-binding",
       Fset_keymap_default_binding, Sset_keymap_default_binding, 2, 2, 0,
  "Sets the default binding of KEYMAP to COMMAND, or `nil'\n\
if no default is desired.  The default-binding is returned when\n\
no other binding for a key-sequence is found in the keymap.\n\
If a keymap has a non-nil default-binding, neither the keymap's\n\
parents nor the current global map are searched for key bindings.")
     (keymap, command)
     Lisp_Object keymap, command;
{
  keymap = get_keymap (keymap, 1, 1);
  
  if (!NILP (command))
    CHECK_STRING (command, 1);

  XKEYMAP (keymap)->default_binding = command;
  return (command);
}

DEFUN ("keymap-default-binding",
       Fkeymap_default_binding, Skeymap_default_binding, 1, 1, 0,
  "Returns the default binding of KEYMAP, or `nil' if it has none.\n\
The default-binding is returned when no other binding for a key-sequence\n\
is found in the keymap.\n\
If a keymap has a non-nil default-binding, neither the keymap's\n\
parents nor the current global map are searched for key bindings.")
     (keymap)
     Lisp_Object keymap;
{
  keymap = get_keymap (keymap, 1, 1);
  return (XKEYMAP (keymap)->default_binding);
}



DEFUN ("keymapp", Fkeymapp, Skeymapp, 1, 1, 0,
  "Return t if ARG is a keymap object.\n\
The keymap may be autoloaded first if necessary.")
  (object)
     Lisp_Object object;
{
  Lisp_Object tem = get_keymap (object, 0, 1);
  return ((KEYMAPP (tem)) ? Qt : Qnil);
}

/* Check that OBJECT is a keymap (after dereferencing through any
   symbols).  If it is, return it.

   If AUTOLOAD is non-zero and OBJECT is a symbol whose function value
   is an autoload form, do the autoload and try again.

   ERROR controls how we respond if OBJECT isn't a keymap.
   If ERROR is non-zero, signal an error; otherwise, just return Qnil. 
 */
Lisp_Object
get_keymap (Lisp_Object object, int error, int autoload)
{
  while (1)
    {
      Lisp_Object tem = indirect_function (object, 0);
      
      if (KEYMAPP (tem))
	return tem;
      /* Should we do an autoload?  */
      else if (autoload
               /* (autoload "filename" doc nil keymap) */
               && SYMBOLP (object)
               && CONSP (tem)
               && EQ (XCONS (tem)->car, Qautoload)
               && EQ (Fcar (Fcdr (Fcdr (Fcdr (Fcdr (tem))))), Qkeymap))
	{
	  struct gcpro gcpro1, gcpro2;
	  GCPRO2 (tem, object);
	  do_autoload (tem, object);
	  UNGCPRO;
	}
      else if (error)
	object = wrong_type_argument (Qkeymapp, object);
      else
	return Qnil;
    }
}

/* Given OBJECT which was found in a slot in a keymap,
   trace indirect definitions to get the actual definition of that slot.
   An indirect definition is a list of the form
   (KEYMAP . INDEX), where KEYMAP is a keymap or a symbol defined as one
   and INDEX is an ASCII code, or a cons of (KEYSYM . MODIFIERS).
 */
static Lisp_Object
get_keyelt (Lisp_Object object)
{
  Lisp_Object map;

 tail_recurse:
  if (!CONSP (object))
    return (object);

  {
    struct gcpro gcpro1;
    GCPRO1 (object);
    map = XCONS (object)->car;
    map = get_keymap (map, 0, 1);
    UNGCPRO;
  }
  /* If the contents are (KEYMAP . ELEMENT), go indirect.  */
  if (!NILP (map))
  {
    Lisp_Object idx = Fcdr (object);
    Lisp_Object keysym;
    unsigned int modifiers;
    if (FIXNUMP (idx))
    {
      struct Lisp_Event event;
      event.event_type = empty_event;
      character_to_event (XINT (idx), &event);
      keysym = event.event.key.keysym;
      modifiers = event.event.key.modifiers;
    }
    else if (CONSP (idx))
    {
      keysym = XCONS (idx)->car;
      if (!FIXNUMP (XCONS (idx)->cdr))
        return (Qnil);
      modifiers = XINT (XCONS (idx)->cdr);
    }
    else
      abort ();
    /* >>> Doesn't search parent keymaps! */
    return (keymap_lookup_directly (map, keysym, modifiers));
  }
  else if (STRINGP (XCONS (object)->car))
  {
    /* If the keymap contents looks like (STRING . DEFN),
       use DEFN.
       Keymap alist elements like (CHAR MENUSTRING . DEFN)
       will be used by HierarKey menus.  */
    object = XCONS (object)->cdr;
    goto tail_recurse;
  }
  else
  {
    /* Anything else is really the value.  */
    return (object);
  }
}

static Lisp_Object
keymap_lookup_1 (Lisp_Object keymap, CONST struct key_data *key)
{
  return (get_keyelt (keymap_lookup_directly (keymap,
                                              key->keysym, key->modifiers)));
}


struct copy_keymap_inverse_closure
{
  Lisp_Object inverse_table;
};

static void
copy_keymap_inverse_mapper (CONST void *hash_key, void *hash_contents, 
                            void *copy_keymap_inverse_closure)
{
  Lisp_Object key, inverse_table, inverse_contents;
  struct copy_keymap_inverse_closure *closure = copy_keymap_inverse_closure;

  VOID_TO_LISP (inverse_table, closure);
  VOID_TO_LISP (inverse_contents, hash_contents);
  CVOID_TO_LISP (key, hash_key);
  /* copy-sequence deals with dotted lists. */
  if (CONSP (inverse_contents))
    inverse_contents = Fcopy_sequence (inverse_contents);
  Fputhash (key, inverse_contents, closure->inverse_table);
}


static Lisp_Object
copy_keymap_internal (struct keymap *keymap)
{
  Lisp_Object nkm = make_keymap (0);
  struct keymap *new_keymap = XKEYMAP (nkm);
  struct copy_keymap_inverse_closure copy_keymap_inverse_closure;
  copy_keymap_inverse_closure.inverse_table = keymap->inverse_table;

  new_keymap->parents = Fcopy_sequence (keymap->parents);
  new_keymap->fullness = keymap->fullness;
  new_keymap->sub_maps_cache = Qnil; /* No submaps */
  new_keymap->table = Fcopy_hashtable (keymap->table);
  new_keymap->inverse_table = Fcopy_hashtable (keymap->inverse_table);
  /* After copying the inverse map, we need to copy the conses which
     are its values, lest they be shared by the copy, and mangled.
   */
  elisp_maphash (copy_keymap_inverse_mapper, keymap->inverse_table,
		 &copy_keymap_inverse_closure);
  return nkm;
}


static Lisp_Object copy_keymap (Lisp_Object keymap);

struct copy_keymap_closure
{
  struct keymap *self;
};

static void
copy_keymap_mapper (CONST void *hash_key, void *hash_contents, 
                    void *copy_keymap_closure)
{
  Lisp_Object key, contents;
  struct copy_keymap_closure *closure = copy_keymap_closure;

  CVOID_TO_LISP (key, hash_key);
  VOID_TO_LISP (contents, hash_contents);
  /* When we encounter a keymap which is indirected through a
     symbol, we need to copy the sub-map.  In v18, the form
       (lookup-key (copy-keymap global-map) "\C-x")
     returned a new keymap, not the symbol 'Control-X-prefix.
   */
  contents = get_keymap (contents, 0, 1);  /* #### autoload GC-safe here? */
  if (KEYMAPP (contents))
    keymap_store_internal (key, closure->self, copy_keymap (contents));
}

static Lisp_Object
copy_keymap (Lisp_Object keymap)
{
  struct copy_keymap_closure copy_keymap_closure;

  if (!KEYMAPP (keymap)) abort ();
  keymap = copy_keymap_internal (XKEYMAP (keymap));
  copy_keymap_closure.self = XKEYMAP (keymap);
  elisp_maphash (copy_keymap_mapper,
		 XKEYMAP (keymap)->table,
		 &copy_keymap_closure);
  return keymap;
}


DEFUN ("copy-keymap", Fcopy_keymap, Scopy_keymap, 1, 1, 0,
  "Return a copy of the keymap KEYMAP.\n\
The copy starts out with the same definitions of KEYMAP,\n\
but changing either the copy or KEYMAP does not affect the other.\n\
Any key definitions that are subkeymaps are recursively copied.")
  (keymap)
     Lisp_Object keymap;
{
  keymap = get_keymap (keymap, 1, 1);
  return copy_keymap (keymap);
}


static int
keymap_fullness (Lisp_Object keymap)
{
  int fullness;
  Lisp_Object sub_maps;
  struct gcpro gcpro1, gcpro2;

  keymap = get_keymap (keymap, 1, 1);
  fullness = XKEYMAP (keymap)->fullness;
  sub_maps = keymap_submaps (keymap);
  GCPRO2 (keymap, sub_maps);
  for (; !NILP (sub_maps); sub_maps = XCONS (sub_maps)->cdr)
  {
    if (MODIFIER_HASH_KEY_P (XCONS (XCONS (sub_maps)->car)->car) != 0)
      {
	Lisp_Object sub_map = XCONS (XCONS (sub_maps)->car)->cdr;
	fullness--; /* don't count bucky maps */
	fullness += keymap_fullness (sub_map);
      }
  }
  UNGCPRO;
  return (fullness);
}

DEFUN ("keymap-fullness", Fkeymap_fullness, Skeymap_fullness, 1, 1, 0,
       "Returns the number of bindings in the keymap.")
     (keymap)
  Lisp_Object keymap;
{
  return (make_number (keymap_fullness (get_keymap (keymap, 1, 1))));
}


static void
define_key_check_keysym (Lisp_Object spec,
                         Lisp_Object keysym, unsigned int modifiers)
{
  /* Now, check and massage the trailing keysym specifier. */
  if (SYMBOLP (keysym))
  {
    if (string_length (XSYMBOL (keysym)->name) == 1)
    {
      keysym = make_number (XSYMBOL (keysym)->name->data [0]);
      goto fixnum_keysym;
    }
  }
  else if (FIXNUMP (keysym))
  {
  fixnum_keysym:
    if (XINT (keysym) < ' ' || XINT (keysym) > 127)
      signal_simple_error (GETTEXT("keysym must be in the printing ASCII range"),
                           keysym);
    /* >>> This bites!  I want to be able to write (control shift a) */
    if (modifiers & MOD_SHIFT)
      signal_simple_error (GETTEXT ("the `shift' modifier may not be applied to ASCII keysyms"),
                           spec);
  }
  else
  {
    signal_simple_error (GETTEXT ("unknown keysym specifier"),
                         keysym);
  }
}


/* Given any kind of key-specifier, return a keysym and modifier mask.
 */
static void
define_key_parser (Lisp_Object spec, struct key_data *returned_value)
{
  if (FIXNUMP (spec))
    {
      struct Lisp_Event event;
      event.event_type = empty_event;
      character_to_event (XINT (spec), &event);
      returned_value->keysym = event.event.key.keysym;
      returned_value->modifiers = event.event.key.modifiers;
    }
  else if (EVENTP (spec))
    {
      switch (XEVENT (spec)->event_type)
	{
	case key_press_event:
          {
            returned_value->keysym = XEVENT (spec)->event.key.keysym;
            returned_value->modifiers = XEVENT (spec)->event.key.modifiers;
	    break;
          }
	case button_press_event:
	case button_release_event:
	  {
	    int down = (XEVENT (spec)->event_type == button_press_event);
	    switch (XEVENT (spec)->event.button.button)
            {
	    case 1:
              returned_value->keysym = (down ? Qbutton1 : Qbutton1up); break;
	    case 2:
              returned_value->keysym = (down ? Qbutton2 : Qbutton2up); break;
	    case 3:
              returned_value->keysym = (down ? Qbutton3 : Qbutton3up); break;
	    case 4:
              returned_value->keysym = (down ? Qbutton4 : Qbutton4up); break;
	    case 5:
              returned_value->keysym = (down ? Qbutton5 : Qbutton5up); break;
	    case 6:
              returned_value->keysym = (down ? Qbutton6 : Qbutton6up); break;
	    case 7:
              returned_value->keysym = (down ? Qbutton7 : Qbutton7up); break;
	    default:
              returned_value->keysym =(down ? Qbutton0 : Qbutton0up); break;
	    }
	    returned_value->modifiers = XEVENT (spec)->event.button.modifiers;
	    break;
	  }
	default:
	  signal_error (Qwrong_type_argument,
			list2 (build_string
			       (GETTEXT ("unable to bind this type of event")),
			       spec));
	}
    }
  else if (SYMBOLP (spec))
    {
      /* Be nice, allow = to mean (=) */
      if (bucky_sym_to_bucky_bit (spec) != 0)
        signal_simple_error (GETTEXT ("Key is a modifier name"), spec);
      define_key_check_keysym (spec, spec, 0);
      returned_value->keysym = spec;
      returned_value->modifiers = 0;
    }
  else if (CONSP (spec))
    {
      unsigned int modifiers = 0;
      Lisp_Object keysym = Qnil;
      Lisp_Object rest = spec;

      /* First, parse out the leading modifier symbols. */
      while (CONSP (rest))
      {
        unsigned int modifier;

	keysym = XCONS (rest)->car;
        modifier = bucky_sym_to_bucky_bit (keysym);
        modifiers |= modifier;
	if (!NILP (XCONS (rest)->cdr))
	  {
	    if (! modifier)
	      signal_simple_error (GETTEXT ("unknown modifier"), keysym);
	  }
	else
	  {
	    if (modifier)
	      signal_simple_error (GETTEXT ("nothing but modifiers here"),
                                   spec);
	  }
	rest = XCONS (rest)->cdr;
	QUIT;
      }
      if (!NILP (rest))
        signal_simple_error (GETTEXT ("dotted list"), spec);

      define_key_check_keysym (spec, keysym, modifiers);
      returned_value->keysym = keysym;
      returned_value->modifiers = modifiers;
    }
  else
    {
      signal_simple_error (GETTEXT ("unknown key-sequence specifier"),
			   spec);
    }

  /* Convert single-character symbols into ints, since that's the
     way the events arrive from the keyboard... */
  if (SYMBOLP (returned_value->keysym) &&
      string_length (XSYMBOL (returned_value->keysym)->name) == 1)
    {
      returned_value->keysym =
	make_number (XSYMBOL (returned_value->keysym)->name->data [0]);

      /* Detect bogus (user-provided) keysyms like '\?C-a */
      if (XINT (returned_value->keysym) < ' ' ||
	  XINT (returned_value->keysym) > 127)
      signal_simple_error ("keysym must be in the printing ASCII range",
                           returned_value->keysym);
    }

  if (SYMBOLP (returned_value->keysym))
    {
      char *name = (char *) XSYMBOL (returned_value->keysym)->name->data;

      /* FSFmacs uses symbols with the printed representation of keysyms in
	 their names, like 'M-x, and we use the syntax '(meta x).  So, to avoid
	 confusion, notice the M-x syntax and signal an error - because
	 otherwise it would be interpreted as a regular keysym, and would even
	 show up in the list-buffers output, causing confusion to the naive.

	 We can get away with this because none of the X keysym names contain
	 a hyphen (some contain underscore, however.)

	 It might be useful to reject keysyms which are not x-valid-keysym-
	 name-p, but that would interfere with various tricks we do to
	 sanitize the Sun keyboards, and would make it trickier to
	 conditionalize a .emacs file for multiple X servers.
       */
      if (strchr (name, '-'))
	signal_simple_error ("invalid keysym (see doc of define-key)",
			     returned_value->keysym);

      /* #### Ok, this is a bit more dubious - make people not lose if they
	 do things like (global-set-key 'RET 'something) because that would
	 otherwise have the same problem as above.  (Gag!)  We silently
	 accept these as aliases for the "real" names.
       */
      else if (EQ (returned_value->keysym, QLFD))
	returned_value->keysym = QKlinefeed;
      else if (EQ (returned_value->keysym, QTAB))
	returned_value->keysym = QKtab;
      else if (EQ (returned_value->keysym, QRET))
	returned_value->keysym = QKreturn;
      else if (EQ (returned_value->keysym, QESC))
	returned_value->keysym = QKescape;
      else if (EQ (returned_value->keysym, QDEL))
	returned_value->keysym = QKdelete;
      else if (EQ (returned_value->keysym, QBS))
	returned_value->keysym = QKbackspace;
    }
}

/* This piece of crap is used by macros.c */
void
key_desc_list_to_event (Lisp_Object list, Lisp_Object event,
                        int allow_menu_events)
{
  struct key_data raw_key;

  if (allow_menu_events &&
      CONSP (list) &&
      EQ (XCONS (list)->car, Qmenu_selection))
    {
      Lisp_Object fn, arg;
      if (! NILP (Fcdr (Fcdr (list))))
	signal_simple_error (GETTEXT ("invalid menu event desc"), list);
      arg = Fcar (Fcdr (list));
      if (SYMBOLP (arg))
	fn = Qcall_interactively;
      else
	fn = Qeval;
      XEVENT (event)->channel = Qnil;
#ifdef EPOCH
      XEVENT (event)->epoch_event = Qnil;
#endif
      XEVENT (event)->event_type = menu_event;
      XEVENT (event)->event.eval.function = fn;
      XEVENT (event)->event.eval.object = arg;
      return;
    }

  define_key_parser (list, &raw_key);

  if (EQ (raw_key.keysym, Qbutton0) || EQ (raw_key.keysym, Qbutton0up) ||
      EQ (raw_key.keysym, Qbutton1) || EQ (raw_key.keysym, Qbutton1up) ||
      EQ (raw_key.keysym, Qbutton2) || EQ (raw_key.keysym, Qbutton2up) ||
      EQ (raw_key.keysym, Qbutton3) || EQ (raw_key.keysym, Qbutton3up) ||
      EQ (raw_key.keysym, Qbutton4) || EQ (raw_key.keysym, Qbutton4up) ||
      EQ (raw_key.keysym, Qbutton5) || EQ (raw_key.keysym, Qbutton5up) ||
      EQ (raw_key.keysym, Qbutton6) || EQ (raw_key.keysym, Qbutton6up) ||
      EQ (raw_key.keysym, Qbutton7) || EQ (raw_key.keysym, Qbutton7up))
    error (GETTEXT ("Mouse-clicks can't appear in saved keyboard macros."));

  XEVENT (event)->channel = Qnil;
#ifdef EPOCH
  XEVENT (event)->epoch_event = Qnil;
#endif
  XEVENT (event)->event_type = key_press_event;
  XEVENT (event)->event.key.keysym = raw_key.keysym;
  XEVENT (event)->event.key.modifiers = raw_key.modifiers;
}


static int
meta_prefix_char_p (CONST struct key_data *key)
{
  struct Lisp_Event event;
  if (meta_prefix_char < 0) return 0;
  event.event_type = key_press_event;
  event.event.key.keysym = key->keysym;
  event.event.key.modifiers = key->modifiers;
  return (meta_prefix_char == event_to_character (&event, 0, 0, 0));
}


/* ASCII grunge.
   Given a keysym, return another keysym/modifier pair which could be 
   considered the same key in an ASCII world.  Backspace returns ^H, for 
   example.
 */
static void
define_key_alternate_name (struct key_data *key,
                           struct key_data *returned_value)
{
  Lisp_Object keysym = key->keysym;
  unsigned int modifiers = key->modifiers;
  unsigned int modifiers_sans_control = (modifiers & (~MOD_CONTROL));
  unsigned int modifiers_sans_meta = (modifiers & (~MOD_META));
  returned_value->keysym = Qnil; /* By default, no "alternate" key */
  returned_value->modifiers = 0;
#define MACROLET(k,m) do { returned_value->keysym = (k); \
			   returned_value->modifiers = (m); \
                           return; } while (0)
  if (modifiers_sans_meta == MOD_CONTROL)
    {
      if EQ (keysym, QKspace)
        MACROLET (make_number ('@'), modifiers);
      else if (!FIXNUMP (keysym))
        return;
      else switch (XINT (keysym))
        {
        case '@':               /* c-@ => c-space */
          MACROLET (QKspace, modifiers);
        case 'h':               /* c-h => backspace */
          MACROLET (QKbackspace, modifiers_sans_control);
        case 'i':               /* c-i => tab */
          MACROLET (QKtab, modifiers_sans_control);
        case 'j':               /* c-j => linefeed */
          MACROLET (QKlinefeed, modifiers_sans_control);
        case 'm':               /* c-m => return */
          MACROLET (QKreturn, modifiers_sans_control);
        case '[':               /* c-[ => escape */
          MACROLET (QKescape, modifiers_sans_control);
        default:
          return;
	}
    }
  else if (modifiers_sans_meta != 0)
    return;
  else if (EQ (keysym, QKbackspace)) /* backspace => c-h */
    MACROLET (make_number ('h'), (modifiers | MOD_CONTROL));
  else if (EQ (keysym, QKtab))       /* tab => c-i */
    MACROLET (make_number ('i'), (modifiers | MOD_CONTROL));
  else if (EQ (keysym, QKlinefeed))  /* linefeed => c-j */
    MACROLET (make_number ('j'), (modifiers | MOD_CONTROL));
  else if (EQ (keysym, QKreturn))    /* return => c-m */
    MACROLET (make_number ('m'), (modifiers | MOD_CONTROL));
  else if (EQ (keysym, QKescape))    /* escape => c-[ */
    MACROLET (make_number ('['), (modifiers | MOD_CONTROL));
  else
    return;
#undef MACROLET
}


static void
ensure_meta_prefix_char_keymapp (Lisp_Object keys, int index,
                                 Lisp_Object keymap)
{
  char buf [255];
  Lisp_Object new_keys;
  int i;
  Lisp_Object mpc_binding;
  struct key_data meta_key;

  define_key_parser (make_number (meta_prefix_char), &meta_key);
  mpc_binding = keymap_lookup_1 (keymap, &meta_key);
  if (NILP (mpc_binding) || !NILP (Fkeymapp (mpc_binding)))
    return;

  if (index == 0)
    new_keys = keys;
  else if (STRINGP (keys))
    new_keys = Fsubstring (keys, Qzero, make_number (index));
  else if (VECTORP (keys))
    {
      new_keys = make_vector (index, Qnil);
      for (i = 0; i < index; i++)
	XVECTOR (new_keys)->contents [i] = XVECTOR (keys)->contents [i];
    }
  else
    abort ();
  if (EQ (keys, new_keys))
    sprintf (buf, GETTEXT ("can't bind %s: %s has a non-keymap binding"),
	     (char *) XSTRING (Fkey_description (keys))->data,
	     (char *) XSTRING (Fsingle_key_description 
			       (make_number (meta_prefix_char)))->data);
  else
    sprintf (buf, GETTEXT ("can't bind %s: %s %s has a non-keymap binding"),
	     (char *) XSTRING (Fkey_description (keys))->data,
	     (char *) XSTRING (Fkey_description (new_keys))->data,
	     (char *) XSTRING (Fsingle_key_description
			       (make_number (meta_prefix_char)))->data);
  signal_simple_error (buf, mpc_binding);
}


/* This comment supplies the doc string for define-key,
   for make-docfile to see.  We cannot put this in the real DEFUN
   due to limits in the Unix cpp.

DEFUN ("define-key", Foo, Sfoo, 3, 3, 0,
  "Args KEYMAP, KEYS, DEF.  Define key sequence KEYS, in KEYMAP, as DEF.\n\
KEYMAP is a keymap object.\n\
KEYS is the sequence of keystrokes to bind, described below.\n\
DEF is anything that can be a key's definition:\n\
 nil (means key is undefined in this keymap);\n\
 a command (a Lisp function suitable for interactive calling);\n\
 a string or key sequence vector (treated as a keyboard macro);\n\
 a keymap (to define a prefix key);\n\
 a symbol; when the key is looked up, the symbol will stand for its\n\
    function definition, that should at that time be one of the above,\n\
    or another symbol whose function definition is used, and so on.\n\
 a cons (STRING . DEFN), meaning that DEFN is the definition\n\
    (DEFN should be a valid definition in its own right);\n\
 or a cons (KEYMAP . CHAR), meaning use definition of CHAR in map KEYMAP.\n\
\n\
Contrary to popular belief, the world is not ASCII.  When running under a\n\
window manager, Emacs can tell the difference between, for example, the\n\
keystrokes control-h, control-shift-h, and backspace.  You can, in fact,\n\
bind different commands to each of these.\n\
\n\
A `key sequence' is a set of keystrokes.  A `keystroke' is a keysym and some\n\
set of modifiers (such as control and meta).  A `keysym' is what is printed\n\
on the keys on your keyboard.\n\
\n\
A keysym may be represented by a symbol, or (if and only if it is equivalent\n\
to a printing ASCII character) by its ASCII code.  The `A' key may be\n\
represented by the symbol `A' or by the number 65.  The `break' key may be\n\
represented only by the symbol `break'.\n\
\n\
A keystroke may be represented by a list: the last element of the list is\n\
the key (a symbol or number, as above) and the preceding elements are the\n\
symbolic names of modifier keys (control, meta, super, hyper, and shift.)\n\
Thus, the sequence control-b is represented by the forms `(control b)' \n\
and `(control 98)'.  A keystroke may also be represented by an event object,\n\
as returned by the `next-command-event' and `read-key-sequence' functions.\n\
\n\
Note that in this context, the keystroke `control-b' is *not* represented\n\
by the number 2 (the ASCII code for ^B).  See below.\n\
\n\
The `shift' modifier is somewhat of a special case.  You should not (and\n\
cannot) use `(meta shift a)' to mean `(meta A)', since for characters that\n\
have printing ASCII equivalents, the state of the shift key is implicit in\n\
the keysym (a vs. A).  You also cannot say `(shift =)' to mean `+', as that\n\
sort of thing varies from keyboard to keyboard.  The shift modifier is for\n\
use only with characters that do not have a second keysym on the same key,\n\
such as `backspace' and `tab'.\n\
\n\
A key sequence is a vector of keystrokes.  As a degenerate case, elements\n\
of this vector may also be keysyms if they have no modifiers.  That is,\n\
the `A' keystroke is represented by all of these forms:\n\
	A	65	(A)	(65)	[A]	[65]	[(A)]	[(65)]\n\
the `control-a' keystroke is represented by these forms:\n\
	(control A)	(control 65)	[(control A)]	[(control 65)]\n\
the key sequence `control-c control-a' is represented by these forms:\n\
	[(control c) (control a)]	[(control 99) (control 65)]\n\
\n\
Mouse button clicks work just like keypresses: (control button1) means\n\
pressing the left mouse button while holding down the control key.\n\
[(control c) (shift button3)] means control-c, hold shift, click right.\n\
\n\
Commands may be bound to the mouse-button up-stroke rather than the down-\n\
stroke as well.  `button1' means the down-stroke, and `button1up' means the\n\
up-stroke.  Different commands may be bound to the up and down strokes,\n\
though that is probably not what you want, so be careful.\n\
\n\
For backward compatibility, a key sequence may also be represented by a\n\
string.  In this case, it represents the key sequence(s) that would\n\
produce that sequence of ASCII characters in a purely ASCII world.  For\n\
example, a string containing the ASCII backspace character, \"\\^H\", would\n\
represent two key sequences: `(control h)' and `backspace'.  Binding a\n\
command to this will actually bind both of those key sequences.  Likewise\n\
for the following pairs:\n\
\n\
		control h	backspace\n\
		control i   	tab\n\
		control m   	return\n\
		control j   	linefeed\n\
		control [   	escape\n\
		control @	control space\n\
\n\
After binding a command to two key sequences with a form like\n\
\n\
	(define-key global-map \"\\^X\\^I\" \'command-1)\n\
\n\
it is possible to redefine only one of those sequences like so:\n\
\n\
	(define-key global-map [(control x) (control i)] \'command-2)\n\
	(define-key global-map [(control x) tab] \'command-3)\n\
\n\
Of course, all of this applies only when running under a window system.  If\n\
you're talking to emacs through an ASCII-only channel, you don't get any of\n\
these features.")
  (keymap, keys, def)
*/


DEFUN ("define-key", Fdefine_key, Sdefine_key, 3, 3, 0, 0
       /* See very large comment above */)
  (keymap, keys, def)
     Lisp_Object keymap;
     Lisp_Object keys;
     Lisp_Object def;
{
  int idx;
  int metized = 0;
  int size;
  int ascii_hack;
  struct gcpro gcpro1, gcpro2, gcpro3;

  if (VECTORP (keys))
    size = vector_length (XVECTOR (keys));
  else if (STRINGP (keys))
    size = string_length (XSTRING (keys));
  else if (FIXNUMP (keys) || SYMBOLP (keys) || CONSP (keys))
    {
      if (!CONSP (keys)) keys = list1 (keys);
      size = 1;
      keys = make_vector (1, keys); /* this is kinda sleazy. */
    }
  else
    {
      keys = wrong_type_argument (Qsequencep, keys);
      size = XINT (Flength (keys));
    }
  if (size == 0)
    return (Qnil);

  GCPRO3 (keymap, keys, def);

  /* ASCII grunge.
     When the user defines a key which, in a strictly ASCII world, would be
     produced by two different keys (^J and linefeed, or ^H and backspace,
     for example) then the binding will be made for both keysyms.

     This is done if the user binds a command to a string, as in
     (define-key map "\^H" 'something), but not when using one of the new
     syntaxes, like (define-key map '(control h) 'something).
   */
  ascii_hack = (STRINGP (keys));

  keymap = get_keymap (keymap, 1, 1);

  idx = 0;
  while (1)
    {
      Lisp_Object c;
      struct key_data raw_key1;
      struct key_data raw_key2;

      if (STRINGP (keys))
	c = make_number (XSTRING (keys)->data [idx]);
      else
	{
	  c = XVECTOR (keys)->contents [idx];
	  if (FIXNUMP (c) &&
	      (XINT (c) < ' ' || XINT (c) > 127))
	    args_out_of_range_3 (c, make_number (32), make_number (127));
	}

      define_key_parser (c, &raw_key1);

      if (!metized
	  && meta_prefix_char >= 0
	  && (XINT (c) == meta_prefix_char
              || meta_prefix_char_p (&raw_key1)))
	{
	  if (idx == (size - 1))
	    {
	      /* This is a hack to prevent a binding for the meta-prefix-char
		 from being made in a map which already has a non-empty "meta"
		 submap.  That is, we can't let both "escape" and "meta" have
		 a binding in the same keymap.  This implies that the idiom
		      (define-key my-map "\e" my-escape-map)
		      (define-key my-escape-map "a" 'my-command)
		 no longer works.  That's ok.  Instead the luser should do
		      (define-key my-map "\ea" 'my-command)
		 or, more correctly
		      (define-key my-map "\M-a" 'my-command)
		 and then perhaps
		      (defvar my-escape-map (lookup-key my-map "\e"))
		 if the luser really wants the map in a variable.
	       */
	      Lisp_Object mmap;
              struct gcpro gcpro1;

              GCPRO1 (c);
              mmap = Fgethash (MAKE_MODIFIER_HASH_KEY (MOD_META),
                               XKEYMAP (keymap)->table, Qnil);
	      if (!NILP (mmap)
		  && keymap_fullness (mmap) != 0)
		{
                  Lisp_Object desc
                    = Fsingle_key_description (make_number (meta_prefix_char));
		  signal_simple_error_2 (GETTEXT ("Map contains meta-bindings, can't bind"),
					 desc, keymap);
		}
              UNGCPRO;
	    }
	  else
	    {
	      metized = 1;
	      idx++;
	      continue;
	    }
	}

      if (ascii_hack)
	define_key_alternate_name (&raw_key1, &raw_key2);
      else
	{
	  raw_key2.keysym = Qnil;
	  raw_key2.modifiers = 0;
	}
      
      if (metized)
      {
	raw_key1.modifiers  |= MOD_META;
	raw_key2.modifiers |= MOD_META;
	metized = 0;
      }

      /* This crap is to make sure that someone doesn't bind something like
	 "C-x M-a" while "C-x ESC" has a non-keymap binding. */
      if ((raw_key1.modifiers & MOD_META) && meta_prefix_char >= 0)
	ensure_meta_prefix_char_keymapp (keys, idx, keymap);

      if (++idx == size)
	{
	  keymap_store (keymap, &raw_key1, def);
	  if (ascii_hack && !NILP (raw_key2.keysym))
	    keymap_store (keymap, &raw_key2, def);
	  UNGCPRO;
	  return def;
	}
      
      {
        Lisp_Object cmd;
        struct gcpro gcpro1;
        GCPRO1 (c);

        cmd = keymap_lookup_1 (keymap, &raw_key1);
	if (NILP (cmd))
	  {
	    cmd = Fmake_sparse_keymap ();
	    XKEYMAP (cmd)->name /* for debugging */
	      = list2 (make_key_description (&raw_key1, 1), keymap);
	    keymap_store (keymap, &raw_key1, cmd);
	  }
	if (NILP (Fkeymapp (cmd)))
          signal_simple_error_2 (GETTEXT ("invalid prefix keys in sequence"),
				 c, keys);

	if (ascii_hack && !NILP (raw_key2.keysym) &&
	    NILP (keymap_lookup_1 (keymap, &raw_key2)))
	  keymap_store (keymap, &raw_key2, cmd);

	keymap = get_keymap (cmd, 1, 1);
        UNGCPRO;
      }
    }
}




/* We need a very fast (i.e., non-consing) version of lookup-key in order 
   to make where-is-internal really fly.
 */

struct raw_lookup_key_mapper_closure
  {
    int remaining;
    CONST struct key_data *raw_keys;
    int raw_keys_count;
    int keys_so_far;
    int accept_default;
  };

static Lisp_Object raw_lookup_key_mapper (Lisp_Object k, void *);

/* Caller should gc-protect args (keymaps may autoload) */
static Lisp_Object
raw_lookup_key (Lisp_Object keymap,
                CONST struct key_data *raw_keys, int raw_keys_count,
                int keys_so_far, int accept_default)
{
  struct raw_lookup_key_mapper_closure c;
  c.remaining = raw_keys_count - 1;
  c.raw_keys = raw_keys;
  c.raw_keys_count = raw_keys_count;
  c.keys_so_far = keys_so_far;
  c.accept_default = accept_default;

  return (traverse_keymaps (keymap, Qnil, raw_lookup_key_mapper, &c));
}

static Lisp_Object
raw_lookup_key_mapper (Lisp_Object k, void *arg)
{
  struct raw_lookup_key_mapper_closure *c = arg;
  int accept_default = c->accept_default;
  int remaining = c->remaining;
  int keys_so_far = c->keys_so_far;
  CONST struct key_data *raw_keys = c->raw_keys;
  Lisp_Object cmd;
      
  if (meta_prefix_char < 0 || ! meta_prefix_char_p (&(raw_keys[0])))
  {
    /* Normal case: every case except the meta-hack (see below). */
    cmd = keymap_lookup_1 (k, &(raw_keys[0]));
	  
    if (remaining == 0)
      /* Return whatever we found if we're out of keys */
      ;
    else if (NILP (cmd))
      /* Found nothing (though perhaps parent map may have binding) */
      ;
    else if (NILP (Fkeymapp (cmd))) /* may autoload! */
      /* Didn't find a keymap, and we have more keys.
       * Return a fixnum to indicate that keys were too long.
       */
      cmd = make_number (keys_so_far + 1);
    else
      cmd = raw_lookup_key (cmd, raw_keys + 1, remaining, 
                            keys_so_far + 1, accept_default);
  }
  else
  {
    /* This is a hack so that looking up a key-sequence whose last
     * element is the meta-prefix-char will return the keymap that
     * the "meta" keys are stored in, if there is no binding for
     * the meta-prefix-char (and if this map has a "meta" submap.)
     * If this map doesnt have a "meta" submap, then the
     * meta-prefix-char is looked up just like any other key.
     */
    if (remaining == 0)
    {
      /* First look for the prefix-char directly */
      cmd = keymap_lookup_1 (k, &(raw_keys[0]));
      if (NILP (cmd))
      {
        /* Do kludgy return of the meta-map */ 
        cmd = Fgethash (MAKE_MODIFIER_HASH_KEY (MOD_META),
                        XKEYMAP (k)->table, Qnil);
      }
    }
    else
    {
      /* Search for the prefix-char-prefixed sequence directly */
      cmd = keymap_lookup_1 (k, &(raw_keys[0]));
      cmd = get_keymap (cmd, 0, 1);
      if (!NILP (cmd))
        cmd = raw_lookup_key (cmd, raw_keys + 1, remaining, 
                              keys_so_far + 1, accept_default);
      else if ((raw_keys[1].modifiers & MOD_META) == 0)
      {
        struct key_data metified;
        metified.keysym = raw_keys[1].keysym;
        metified.modifiers = raw_keys[1].modifiers | MOD_META;

        /* Search for meta-next-char sequence directly */
        cmd = keymap_lookup_1 (k, &metified);
        if (remaining == 1)
          ;
        else
        {
          cmd = get_keymap (cmd, 0, 1);
          if (!NILP (cmd))
            cmd = raw_lookup_key (cmd, raw_keys + 2, remaining - 1,
                                  keys_so_far + 2, accept_default);
        }
      }
    }
  }
  if (accept_default && NILP (cmd))
    cmd = XKEYMAP (k)->default_binding;
  return (cmd);
}

/* Value is number if `keys' is too long; NIL if valid but has no definition.*/
/* Caller should gc-protect arguments */
Lisp_Object
lookup_keys (Lisp_Object keymap, int nkeys, Lisp_Object *keys,
             int accept_default)
{
  struct key_data kkk[20];
  struct key_data *raw_keys;
  int i;

  if (nkeys == 0)
    return Qnil;

  if (nkeys > (countof (kkk)))
    raw_keys = kkk;
  else
    raw_keys = (struct key_data *) alloca (sizeof (struct key_data) * nkeys);

  for (i = 0; i < nkeys; i++)
    {
      define_key_parser (keys[i], &(raw_keys[i]));
    }
  return (raw_lookup_key (keymap, raw_keys, nkeys, 0, accept_default));
}



Lisp_Object
lookup_events (Lisp_Object event_head, int nmaps, Lisp_Object keymaps[],
               int accept_default)
{
  struct key_data kkk[20];

  int nkeys;
  struct key_data *raw_keys;
  struct Lisp_Event *e;
  Lisp_Object tem;
  struct gcpro gcpro1, gcpro2;
  int iii;

  CHECK_EVENT (event_head, 0);

  for (e = XEVENT (event_head), nkeys = 0; e; e = event_next (e), nkeys++)
    ;

  if (nkeys < (countof (kkk)))
    raw_keys = kkk;
  else
    raw_keys = (struct key_data *) alloca (sizeof (struct key_data) * nkeys);

  for (e = XEVENT (event_head), nkeys = 0; e; e = event_next (e), nkeys++)
    {
      Lisp_Object c;
      
      XSETR (c, Lisp_Event, e);
      define_key_parser (c, &(raw_keys[nkeys]));
    }
  GCPRO2 (keymaps[0], event_head);
  gcpro1.nvars = nmaps;
  /* >>>raw_keys[].keysym slots aren't gc-protected.  We rely (but shouldn't)
   * on somebody else somewhere (obarray) having a pointer to all keysyms. */
  for (iii = 0; iii < nmaps; iii++)
    {
      tem = raw_lookup_key (keymaps[iii], raw_keys, nkeys, 0, accept_default);
      if (FIXNUMP (tem))
	{
	  /* Too long in some local map means don't look at global map */
	  tem = Qnil;
	  break;
	}
      else if (!NILP (tem))
	break;
    }
  UNGCPRO;
  return (tem);
}


DEFUN ("lookup-key", Flookup_key, Slookup_key, 2, 3, 0,
  "In keymap KEYMAP, look up key sequence KEYS.  Return the definition.\n\
nil means undefined.  See doc of `define-key' for kinds of definitions\n\
and key-sequence specifications.\n\
Number as value means KEYS is \"too long\";\n\
that is, characters in it except for the last one\n\
fail to be a valid sequence of prefix characters in KEYMAP.\n\
The number is how many characters at the front of KEYS\n\
it takes to reach a non-prefix command.")
  (keymap, keys, accept_default)
     Lisp_Object keymap, keys, accept_default;
{
  if (VECTORP (keys))
    {
      return lookup_keys (keymap,
			  vector_length (XVECTOR (keys)),
                          XVECTOR (keys)->contents,
                          !NILP (accept_default));
    }
  else if (SYMBOLP (keys) || FIXNUMP (keys))
    {
      return lookup_keys (keymap, 1, &keys, !NILP (accept_default));
    }
  else if (!STRINGP (keys))
    {
      keys = wrong_type_argument (Qsequencep, keys);
      return Flookup_key (keymap, keys, accept_default);
    }
  else
    {
      int length = string_length (XSTRING (keys));
      int i;
      struct key_data *raw_keys
	= (struct key_data *) alloca (sizeof (struct key_data) * length);
      if (length == 0)
	return Qnil;

      for (i = 0; i < length; i++)
	{
          unsigned char n = (unsigned char) XSTRING (keys)->data[i];
	  define_key_parser (make_number (n), &(raw_keys[i]));
	}
      return (raw_lookup_key (keymap, raw_keys, length, 0,
                              !NILP (accept_default)));
    }
}


/* Given a key sequence, returns a list of keymaps to search for bindings.
   Does all manner of semi-hairy heuristics, like looking in the current
   buffer's map before looking in the global map and looking in the local
   map of the buffer in which the mouse was clicked in event0 is a click.

   It would be kind of nice if this were in Lisp so that this semi-hairy
   semi-heuristic command-lookup behaviour could be readily understood and
   customised.  However, this needs to be pretty fast, or performance of
   keyboard macros goes to shit; putting this in lisp slows macros down
   2-3x.  And they're already slower than v18 by 5-6x. 
 */

struct relevant_maps
  {
    int nmaps;
    unsigned int max_maps;
    Lisp_Object *maps;
    struct gcpro *gcpro;
  };

static void get_relevant_extent_keymaps (Lisp_Object pos,
                                         Lisp_Object buffer,
                                         Lisp_Object glyph,
                                         struct relevant_maps *closure);
static void get_relevant_minor_maps (Lisp_Object buffer,
                                     struct relevant_maps *closure);

static void
relevant_map_push (Lisp_Object map, struct relevant_maps *closure)
{ 
  unsigned int nmaps = closure->nmaps;

  if (!KEYMAPP (map))
    return;
  closure->nmaps = nmaps + 1;
  if (nmaps < closure->max_maps)
  {
    closure->maps[nmaps] = map;
    closure->gcpro->nvars = nmaps;
  }
}

static int
get_relevant_keymaps (Lisp_Object keys,
                      int max_maps, Lisp_Object maps[])
{
  Lisp_Object terminal = Qnil;
  struct gcpro gcpro1;
  struct relevant_maps closure;

  GCPRO1 (*maps);
  gcpro1.nvars = 0;
  closure.nmaps = 0;
  closure.max_maps = max_maps;
  closure.maps = maps;
  closure.gcpro = &gcpro1;

  if (EVENTP (keys))
  {
    struct Lisp_Event *e = XEVENT (keys);
    for (e = XEVENT (keys);
         event_next (e);
         e = event_next (e))
      ;
    XSETR (terminal, Lisp_Event, e);
  }
  else if (VECTORP (keys))
  {
    int len = vector_length (XVECTOR (keys));
    if (len > 1)
      terminal = XVECTOR (keys)->contents[len - 1];
  }

  if (KEYMAPP (Voverriding_local_map))
  {
    relevant_map_push (Voverriding_local_map, &closure);
  }
  else if (!EVENTP (terminal)
           || (XEVENT (terminal)->event_type != button_press_event 
               && XEVENT (terminal)->event_type != button_release_event))
  {
    Lisp_Object tem;
    XSETR (tem, Lisp_Buffer, current_buffer);
    /* It's not a mouse event; order of keymaps searched is:
       o  keymap of any/all extents under the mouse
       o  minor-mode maps
       o  local-map of current-buffer
       o  global-map
       */
    if (EVENTP (terminal))
    {
      get_relevant_extent_keymaps (make_number (PT), tem,
                                   Qnil,
                                   &closure);
    }
    get_relevant_minor_maps (tem, &closure);

    tem = current_buffer->keymap;
    if (!NILP (tem))
      relevant_map_push (tem, &closure);
  }
  else
  {
    /* It's a mouse event; order of keymaps searched is:
       o  local-map of mouse-grabbed-buffer
       o  keymap of any/all extents under the mouse
       if the mouse is over a modeline:
       o  mode-line-map of buffer corresponding to that modeline
       o  else, local-map of buffer under the mouse
       o  minor-mode maps
       o  local-map of current-buffer
       o  global-map
       */
    Lisp_Object window = Fevent_window (terminal);
    Lisp_Object screen = Fevent_screen (terminal);
    int mode_p = 0;

    if (BUFFERP (Vmouse_grabbed_buffer))
    {
      Lisp_Object map = XBUFFER (Vmouse_grabbed_buffer)->keymap;
      /* get_relevant_minor_maps (Vmouse_grabbed_buffer, &closure); ??? */
      if (!NILP (map))
        relevant_map_push (map, &closure);
    }

    if (NILP (window) && !NILP (Fscreenp (screen)))
    {
      mode_p = 1;
      window = window_from_coordinates (screen,
                                        XINT (Fevent_x (terminal)),
                                        XINT (Fevent_y (terminal)),
                                        0);
    }

    if (!NILP (window))
    {
      Lisp_Object buffer = Fwindow_buffer (window);
      if (!NILP (buffer))
      {
        if (mode_p)
        {
          Lisp_Object map = symbol_value_in_buffer (Qmode_line_map, buffer);
          if (!EQ (map, Qunbound) && !NILP (map))
            relevant_map_push (map, &closure);
        }
        else
        {
          /* if it was a modeline hit, then it can't have been over
             an extent with a keymap. */
          get_relevant_extent_keymaps (Fevent_point (terminal), buffer,
                                       Fevent_glyph (terminal),
                                       &closure);
        }
        if (!EQ (buffer, Vmouse_grabbed_buffer)) /* already pushed */
        {
          get_relevant_minor_maps (buffer, &closure);
          relevant_map_push (XBUFFER (buffer)->keymap, &closure);
        }
      }
    }
  }

  {
    int nmaps = closure.nmaps;
    /* Silently truncate at 100 keymaps to prevent infinite losssage */
    if (nmaps >= max_maps && max_maps > 0)
      maps[max_maps - 1] = Vcurrent_global_map;
    else
      maps[nmaps] = Vcurrent_global_map;
    UNGCPRO;
    return (nmaps + 1);
  }
}

/* Returns a set of keymaps extracted from the extents at POS in BUFFER.
   The GLYPH arg, if specified, is one more extent to look for a keymap in,
   and if it has one, its keymap will be the first element in the list 
   returned.  This is so we can correctly search the keymaps associated
   with glyphs which may be physically disjoint from their extents: for
   example, if a glyph is out in the margin, we should still consult the
   kemyap of that glyph's extent, which may not itself be under the mouse.
 */
static void
get_relevant_extent_keymaps (Lisp_Object pos, Lisp_Object buffer,
                             Lisp_Object glyph,
                             struct relevant_maps *closure)
{
  /* the glyph keymap, if any, comes first.
     (Processing it twice is no big deal: noop.) */
  if (!NILP (glyph))
  {
    Lisp_Object keymap = Fextent_property (glyph, Qkeymap);
    if (!NILP (keymap))
      relevant_map_push (get_keymap (keymap, 1, 1), closure);
  }

  /* Next check the extents at the text position, if any */
  if (!NILP (pos))
  {
    Lisp_Object extent;
    for (extent = Fextent_at (pos, buffer, Qkeymap, Qnil);
         !NILP (extent);
         extent = Fextent_at (pos, buffer, Qkeymap, extent))
    {
      Lisp_Object keymap = Fextent_property (extent, Qkeymap);
      if (!NILP (keymap))
        relevant_map_push (get_keymap (keymap, 1, 1), closure);
      QUIT;
    }
  }
}

static Lisp_Object
minor_mode_keymap_predicate (Lisp_Object assoc, Lisp_Object buffer)
{
  if (CONSP (assoc))
  {
    Lisp_Object sym = XCONS (assoc)->car;
    if (SYMBOLP (sym))
    {
      Lisp_Object val = symbol_value_in_buffer (sym, buffer);
      if (!EQ (val, Qnil) && !EQ (val, Qunbound))
      {
        Lisp_Object map = get_keymap (XCONS (assoc)->cdr, 0, 1);
        return (map);
      }
    }
  }
  return (Qnil);
}

static void
get_relevant_minor_maps (Lisp_Object buffer, struct relevant_maps *closure)
{
  Lisp_Object alist;

  /* Will you ever lose badly if you make this circular! */
  for (alist = symbol_value_in_buffer (Qminor_mode_map_alist, buffer);
       CONSP (alist);
       alist = XCONS (alist)->cdr)
  {
    Lisp_Object m = minor_mode_keymap_predicate (XCONS (alist)->car, buffer);
    if (!NILP (m)) relevant_map_push (m, closure);
    QUIT;
  }
}




/* >>> Would map-current-keymaps be a better thing?? */
DEFUN ("current-keymaps", Fcurrent_keymaps, Scurrent_keymaps, 0, 1, 0,
  ">>>> document me")
  (event_or_keys)
     Lisp_Object event_or_keys;
{
  struct gcpro gcpro1;
  Lisp_Object maps[100];
  Lisp_Object *gubbish = maps;
  int nmaps;

  GCPRO1 (event_or_keys);
  nmaps = get_relevant_keymaps (event_or_keys, countof (maps), gubbish);
  if (nmaps > countof (maps))
  {
    gubbish = (Lisp_Object *) alloca (nmaps * sizeof (Lisp_Object));
    nmaps = get_relevant_keymaps (event_or_keys, nmaps, gubbish);
  }
  UNGCPRO;
  return (Flist (nmaps, gubbish));
}

DEFUN ("key-binding", Fkey_binding, Skey_binding, 1, 2, 0,
  "Return the binding for command KEYS in current keymaps.\n\
KEYS is a string, a vector of events, or a vector of key-description lists\n\
as described in the documentation for the `define-key' function.\n\
The binding is probably a symbol with a function definition; see\n\
the documentation for `lookup-key' for more information.\n\
\n\
For key-presses, the order of keymaps searched is:\n\
  - the `keymap' property of any extent(s) at point;\n\
  - the current-local-map of the current-buffer;\n\
  - the current global map.\n\
\n\
For mouse-clicks, the order of keymaps searched is:\n\
  - the current-local-map of the `mouse-grabbed-buffer' if any;\n\
  - the `keymap' property of any extent(s) at the position of the click;\n\
  - the mode-line-map of the buffer corresponding to the mode-line under\n\
    the mouse (if the click happened over a mode line);\n\
  - the current-local-map of the buffer under the mouse;\n\
  - the current global map.")
  (keys, accept_default)
    Lisp_Object keys, accept_default;
{
  int i;
  Lisp_Object maps[100];
  int nmaps;
  struct gcpro gcpro1, gcpro2;
  GCPRO2 (keys, accept_default); /* get_relevant_keymaps may autoload */

  nmaps = get_relevant_keymaps (keys, countof (maps), maps);

  UNGCPRO;

  if (EVENTP (keys))           /* unadvertised "feature" for the future */
    return (lookup_events (keys, nmaps, maps, !NILP (accept_default)));

  for (i = 0; i < nmaps; i++)
    {
      Lisp_Object tem = Flookup_key (maps[i], keys, accept_default);
      if (FIXNUMP (tem))
	{
	  /* Too long in some local map means don't look at global map */
	  return (Qnil);
	}
      else if (!NILP (tem))
	return (tem);
    }
  return (Qnil);
}

/* Attempts to find a command corresponding to the event-sequence
   whose head is event0 (sequence is threaded though event_next).
   Returns either a command symbol or Qnil.
 */
Lisp_Object
event_binding (Lisp_Object event0, int accept_default)
{
  Lisp_Object maps[100];
  int nmaps;

  if (!EVENTP (event0)) abort ();

  nmaps = get_relevant_keymaps (event0, countof (maps), maps);
  return (lookup_events (event0, nmaps, maps, accept_default));
}


DEFUN ("use-global-map", Fuse_global_map, Suse_global_map, 1, 1, 0,
  "Select KEYMAP as the global keymap.")
  (keymap)
     Lisp_Object keymap;
{
  keymap = get_keymap (keymap, 1, 1);
  Vcurrent_global_map = keymap;
  return Qnil;
}

DEFUN ("use-local-map", Fuse_local_map, Suse_local_map, 1, 1, 0,
  "Select KEYMAP as the local keymap.\n\
If KEYMAP is nil, that means no local keymap.")
  (keymap)
     Lisp_Object keymap;
{
  if (!NILP (keymap))
    keymap = get_keymap (keymap, 1, 1);

  current_buffer->keymap = keymap;

  return Qnil;
}

DEFUN ("current-local-map", Fcurrent_local_map, Scurrent_local_map, 0, 0, 0,
  "Return current buffer's local keymap, or nil if it has none.")
  ()
{
  return current_buffer->keymap;
}

DEFUN ("current-global-map", Fcurrent_global_map, Scurrent_global_map, 0, 0, 0,
  "Return the current global keymap.")
  ()
{
  return (Vcurrent_global_map);
}


/* Since keymaps are arranged in a hierarchy, one keymap per bucky bit or
   prefix key, it's not entirely objvious what map-keymap should do, but 
   what it does is: map over all keys in this map; then recursively map
   over all submaps of this map that are "bucky" submaps.  This means that,
   when mapping over a keymap, it appears that "x" and "C-x" are in the
   same map, although "C-x" is really in the "control" submap of this one.
   However, since we don't recursively descend the submaps that are bound
   to prefix keys (like C-x, C-h, etc) the caller will have to recurse on
   those explicitly, if that's what they want.

   So the end result of this is that the bucky keymaps (the ones indexed
   under the symbols control, meta, super, hyper, symbol, and shift) are
   invisible from elisp.  They're just an implementation detail that code
   outside of this file doesn't need to know about.
 */

struct map_keymap_unsorted_closure
{
  void (*fn) (CONST struct key_data *, Lisp_Object binding, void *arg);
  void *arg;
  unsigned int modifiers;
};

/* used by map_keymap() */
static void
map_keymap_unsorted_mapper (CONST void *hash_key, void *hash_contents, 
                            void *map_keymap_unsorted_closure)
{
  Lisp_Object keysym;
  Lisp_Object contents;
  struct map_keymap_unsorted_closure *closure = map_keymap_unsorted_closure;
  unsigned int modifiers = closure->modifiers;
  unsigned int mod_bit;
  CVOID_TO_LISP (keysym, hash_key);
  VOID_TO_LISP (contents, hash_contents);
  mod_bit = MODIFIER_HASH_KEY_P (keysym);
  if (mod_bit != 0)
    {
      int omod = modifiers;
      closure->modifiers = (modifiers | mod_bit);
      contents = get_keymap (contents, 1, 1);
      elisp_maphash (map_keymap_unsorted_mapper,
		     XKEYMAP (contents)->table,
		     map_keymap_unsorted_closure);
      closure->modifiers = omod;
    }
  else
    {
      struct key_data key;
      key.keysym = keysym;
      key.modifiers = modifiers;
      ((*closure->fn) (&key, contents, closure->arg));
    }
}


struct map_keymap_sorted_closure
{
  Lisp_Object *result_locative;
};

/* used by map_keymap_sorted() */
static void
map_keymap_sorted_mapper (CONST void *hash_key, void *hash_contents, 
                          void *map_keymap_sorted_closure)
{
  struct map_keymap_sorted_closure *cl = map_keymap_sorted_closure;
  Lisp_Object key, contents;
  Lisp_Object *list = cl->result_locative;
  CVOID_TO_LISP (key, hash_key);
  VOID_TO_LISP (contents, hash_contents);
  *list = Fcons (Fcons (key, contents), *list);
}


/* used by map_keymap_sorted(), describe_map_sort_predicate(),
   and keymap_submaps().
 */
static int
map_keymap_sort_predicate (Lisp_Object obj1, Lisp_Object obj2, 
                           Lisp_Object pred)
{
  /* obj1 and obj2 are conses with keysyms in their cars.  Cdrs are ignored.
   */
  unsigned int bit1, bit2;
  int sym1_p = 0;
  int sym2_p = 0;
  obj1 = XCONS (obj1)->car;
  obj2 = XCONS (obj2)->car;

  if (EQ (obj1, obj2))
    return -1;
  bit1 = MODIFIER_HASH_KEY_P (obj1);
  bit2 = MODIFIER_HASH_KEY_P (obj2);
  
  /* If either is a symbol with a character-set-property, then sort it by
     that code instead of alphabetically.
   */
  if (! bit1 && SYMBOLP (obj1))
    {
      Lisp_Object code = Fget (obj1, Vcharacter_set_property, Qnil);
      if (FIXNUMP (code))
	obj1 = code, sym1_p = 1;
    }
  if (! bit2 && SYMBOLP (obj2))
    {
      Lisp_Object code = Fget (obj2, Vcharacter_set_property, Qnil);
      if (FIXNUMP (code))
	obj2 = code, sym2_p = 1;
    }

  /* all symbols (non-ASCIIs) come after integers (ASCIIs) */
  if (XTYPE (obj1) != XTYPE (obj2))
    return ((SYMBOLP (obj2)) ? 1 : -1);

  if (! bit1 && FIXNUMP (obj1)) /* they're both ASCII */
    {
      int o1 = XINT (obj1);
      int o2 = XINT (obj2);
      if (o1 == o2 &&		/* If one started out as a symbol and the */
	  sym1_p != sym2_p)	/* other didn't, the symbol comes last. */
	return (sym2_p ? 1 : -1);

    return ((o1 < o2) ? 1 : -1); /* else just compare them */
    }

  /* else they're both symbols.  If they're both buckys, then order them. */
  if (bit1 && bit2)
    return ((bit1 < bit2) ? 1 : -1);
  
  /* if only one is a bucky, then it comes later */
  if (bit1 || bit2)
    return (bit2 ? 1 : -1);

  /* otherwise, string-sort them. */
  {
    char *s1 = (char *) XSYMBOL (obj1)->name->data;
    char *s2 = (char *) XSYMBOL (obj2)->name->data;
    return (
#ifdef I18N2
	    (0 > strcoll (s1, s2))
#else
	    (0 > strcmp (s1, s2))
#endif
	    ? 1 : -1);
  }
}


/* used by map_keymap() */
static void
map_keymap_sorted (Lisp_Object keymap_table,
                   unsigned int modifiers, 
                   void (*function) (CONST struct key_data *key,
                                     Lisp_Object binding, 
                                     void *map_keymap_sorted_closure),
                   void *map_keymap_sorted_closure)
{
  struct gcpro gcpro1;
  Lisp_Object contents = Qnil;

  if (XINT (Fhashtable_fullness (keymap_table)) == 0)
    return;

  GCPRO1 (contents);

  {
    struct map_keymap_sorted_closure c1;
    c1.result_locative = &contents;
    elisp_maphash (map_keymap_sorted_mapper, keymap_table, &c1);
  }
  contents = list_sort (contents, Qnil, map_keymap_sort_predicate);
  for (; !NILP (contents); contents = XCONS (contents)->cdr)
    {
      Lisp_Object keysym = XCONS (XCONS (contents)->car)->car;
      Lisp_Object binding = XCONS (XCONS (contents)->car)->cdr;
      unsigned int sub_bits = MODIFIER_HASH_KEY_P (keysym);
      if (sub_bits != 0)
	map_keymap_sorted (XKEYMAP (get_keymap (binding, 1, 1))->table,
			   (modifiers | sub_bits),
			   function,
			   map_keymap_sorted_closure);
      else
      {
	struct key_data k;
        k.keysym = keysym;
        k.modifiers = modifiers;
        ((*function) (&k, binding, map_keymap_sorted_closure));
      }
    }
  UNGCPRO;
}


/* used by Fmap_keymap() */
static void
map_keymap_mapper (CONST struct key_data *key,
                   Lisp_Object binding, 
                   void *function)
{
  Lisp_Object fn;
  VOID_TO_LISP (fn, function);
  call2 (fn, make_key_description (key, 1), binding);
}


static void
map_keymap (Lisp_Object keymap_table, int sort_first,
            void (*function) (CONST struct key_data *key,
                              Lisp_Object binding,
                              void *fn_arg),
            void *fn_arg)
{
  if (sort_first)
    map_keymap_sorted (keymap_table, 0, function, fn_arg);
  else
    {
      struct map_keymap_unsorted_closure map_keymap_unsorted_closure;
      map_keymap_unsorted_closure.fn = function;
      map_keymap_unsorted_closure.arg = fn_arg;
      map_keymap_unsorted_closure.modifiers = 0;
      elisp_maphash (map_keymap_unsorted_mapper, keymap_table,
		     &map_keymap_unsorted_closure);
    }
}

DEFUN ("map-keymap", Fmap_keymap, Smap_keymap, 2, 3, 0,
  "Apply FUNCTION to each element of KEYMAP.  FUNCTION will be called with\n\
two arguments: a key-description list, and the binding.  The order in which\n\
the elements of the keymap are passed to the function is unspecified.  If\n\
the function inserts new elements into the keymap, it may or may not\n\
be called with them later.  No element of the keymap will ever be passed to\n\
the function more than once.\n\
\n\
The function will not be called on elements of this keymap's parents\n\
(see the function `keymap-parents') or upon keymaps which are contained\n\
within this keymap (multi-character definitions).\n\
It will be called on \"meta\" characters since they are not really\n\
two-character sequences.\n\
\n\
If the optional third argument SORT-FIRST is non-nil, then the elements of\n\
the keymap will be passed to the mapper function in a canonical order.\n\
Otherwise, they will be passed in hash (that is, random) order, which is\n\
faster.")
     (function, keymap, sort_first)
    Lisp_Object function, keymap, sort_first;
{
  struct gcpro gcpro1, gcpro2;

  if (!NILP (Fkeymapp (function))) /* tolerate obviously transposed args */
    {
      Lisp_Object tmp = function;
      function = keymap;
      keymap = tmp;
    }
  GCPRO2 (function, keymap);
  keymap = get_keymap (keymap, 1, 1);
  map_keymap (XKEYMAP (keymap)->table, !NILP (sort_first),
	      map_keymap_mapper, LISP_TO_VOID (function));
  UNGCPRO;
  return Qnil;
}



struct accessible_keymaps_closure
  {
    Lisp_Object tail;
  };


static void
accessible_keymaps_mapper_1 (Lisp_Object keysym, Lisp_Object contents,
                             unsigned int modifiers,
                             struct accessible_keymaps_closure *closure)
{
  unsigned int subbits = MODIFIER_HASH_KEY_P (keysym);

  if (subbits != 0)
    {
      Lisp_Object submaps;

      contents = get_keymap (contents, 1, 1);
      submaps = keymap_submaps (contents);
      for (; !NILP (submaps); submaps = XCONS (submaps)->cdr)
	{
	  accessible_keymaps_mapper_1 (XCONS (XCONS (submaps)->car)->car,
                                       XCONS (XCONS (submaps)->car)->cdr,
                                       (subbits | modifiers),
                                       closure);
	}
    }
  else
    {
      Lisp_Object thisseq = Fcar (Fcar (closure->tail));
      Lisp_Object cmd = get_keyelt (contents);
      Lisp_Object vec;
      int j;
      struct key_data key;
      key.keysym = keysym;
      key.modifiers = modifiers;

      if (NILP (cmd))
	abort ();
      cmd = get_keymap (cmd, 0, 1);
      if (!KEYMAPP (cmd))
	abort ();

      vec = make_vector (vector_length (XVECTOR (thisseq)) + 1, Qnil);
      for (j = 0; j < vector_length (XVECTOR (thisseq)); j++)
	XVECTOR (vec)->contents [j] = XVECTOR (thisseq)->contents [j];
      XVECTOR (vec)->contents [j] = make_key_description (&key, 1);

      nconc2 (closure->tail, list1 (Fcons (vec, cmd)));
    }
}


static Lisp_Object
accessible_keymaps_keymap_mapper (Lisp_Object thismap, void *arg)
{
  struct accessible_keymaps_closure *closure = arg;
  Lisp_Object submaps = keymap_submaps (thismap);

  for (; !NILP (submaps); submaps = XCONS (submaps)->cdr)
  {
    accessible_keymaps_mapper_1 (XCONS (XCONS (submaps)->car)->car,
                                 XCONS (XCONS (submaps)->car)->cdr,
                                 0,
                                 closure);
  }
  return (Qnil);
}


DEFUN ("accessible-keymaps", Faccessible_keymaps, Saccessible_keymaps,
  1, 2, 0,
  "Find all keymaps accessible via prefix characters from KEYMAP.\n\
Returns a list of elements of the form (KEYS . MAP), where the sequence\n\
KEYS starting from KEYMAP gets you to MAP.  These elements are ordered\n\
so that the KEYS increase in length.  The first element is ([] . KEYMAP).\n\
An optional argument PREFIX, if non-nil, should be a key sequence;\n\
then the value includes only maps for prefixes that start with PREFIX.")
  (startmap, prefix)
     Lisp_Object startmap, prefix;
{
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
  Lisp_Object accessible_keymaps = Qnil;
  struct accessible_keymaps_closure c;
  c.tail = Qnil;
  GCPRO4 (accessible_keymaps, c.tail, prefix, startmap);

 retry:
  startmap = get_keymap (startmap, 1, 1);
  if (NILP (prefix))
    prefix = make_vector (0, Qnil);
  else if (!VECTORP (prefix) || STRINGP (prefix))
  {
    prefix = wrong_type_argument (Qarrayp, prefix);
    goto retry;
  }
  else
  {
    int len = XINT (Flength (prefix));
    Lisp_Object def = Flookup_key (startmap, prefix, Qnil);
    Lisp_Object p;
    int iii;
    struct gcpro gcpro1;

    def = get_keymap (def, 0, 1);
    if (!KEYMAPP (def))
      goto RETURN;

    startmap = def;
    p = make_vector (len, Qnil);
    GCPRO1 (p);
    for (iii = 0; iii < len; iii++)
    {
      struct key_data key;
      define_key_parser (Faref (prefix, make_number (iii)), &key);
      XVECTOR (p)->contents[iii] = make_key_description (&key, 1);
    }
    UNGCPRO;
    prefix = p;
  }
  
  accessible_keymaps = list1 (Fcons (prefix, startmap));

  /* For each map in the list maps,
     look at any other maps it points to
     and stick them at the end if they are not already in the list */

  for (c.tail = accessible_keymaps;
       !NILP (c.tail);
       c.tail = XCONS (c.tail)->cdr)
    {
      Lisp_Object thismap = Fcdr (Fcar (c.tail));
      CHECK_KEYMAP (thismap, 0);
      traverse_keymaps (thismap, Qnil,
                        accessible_keymaps_keymap_mapper, &c);
    }
 RETURN:
  UNGCPRO;
  return (accessible_keymaps);
}



DEFUN ("key-description", Fkey_description, Skey_description, 1, 1, 0,
  "Return a pretty description of key-sequence KEYS.\n\
Control characters turn into \"C-foo\" sequences, meta into \"M-foo\"\n\
spaces are put between sequence elements, etc.")
  (keys)
     Lisp_Object keys;
{
  if (FIXNUMP (keys) || CONSP (keys) || SYMBOLP (keys) || EVENTP (keys))
    {
      return Fsingle_key_description (keys);
    }
  else if (VECTORP (keys) ||
	   STRINGP (keys))
    {
      Lisp_Object string = Qnil;
      /* Lisp_Object sep = Qnil; */
      int size = XINT (Flength (keys));
      int i;

      for (i = 0; i < size; i++)
	{
	  Lisp_Object s2 = Fsingle_key_description
	    (((STRINGP (keys))
	      ? make_number ((unsigned char) XSTRING (keys)->data[i])
	      : XVECTOR (keys)->contents[i]));

	  if (i == 0)
	    string = s2;
	  else
	    {
	      /* if (NILP (sep)) Lisp_Object sep = build_string (" ") */;
	      string = concat2 (string, concat2 (Vsingle_space_string, s2));
	    }
	}
      return (string);
    }
  return Fkey_description (wrong_type_argument (Qsequencep, keys));
}

DEFUN ("single-key-description", Fsingle_key_description,
       Ssingle_key_description, 1, 1, 0,
  "Return a pretty description of command character KEY.\n\
Control characters turn into C-whatever, etc.\n\
This differs from `text-char-description' in that it returns a description\n\
of a key read from the user rather than a character from a buffer.")
  (key)
     Lisp_Object key;
{
  if (SYMBOLP (key))
    key = Fcons (key, Qnil); /* sleaze sleaze */

  if (EVENTP (key) || FIXNUMP (key))
    {
      char buf [255];
      if (FIXNUMP (key))
	{
	  struct Lisp_Event event;
	  event.event_type = empty_event;
	  character_to_event (XINT (key), &event);
	  format_event_object (buf, &event, 1);
	}
      else
	format_event_object (buf, XEVENT (key), 1);
      return (build_string (buf));
    }

  if (CONSP (key))
    {
      char buf [255];
      char *bufp = buf;
      Lisp_Object rest;
      buf[0]=0;
      for (rest = key; !NILP (rest); rest = XCONS (rest)->cdr)
	{
	  Lisp_Object keysym = XCONS (rest)->car;
	  if (EQ (keysym, Qcontrol))    strcpy (bufp, "C-"), bufp += 2;
	  else if (EQ (keysym, Qctrl))  strcpy (bufp, "C-"), bufp += 2;
	  else if (EQ (keysym, Qmeta))  strcpy (bufp, "M-"), bufp += 2;
	  else if (EQ (keysym, Qsuper)) strcpy (bufp, "S-"), bufp += 2;
	  else if (EQ (keysym, Qhyper)) strcpy (bufp, "H-"), bufp += 2;
	  else if (EQ (keysym, Qsymbol)) strcpy (bufp, "Sym-"), bufp += 4;
	  else if (EQ (keysym, Qshift)) strcpy (bufp, "Sh-"), bufp += 3;
	  else if (FIXNUMP (keysym))
	    *bufp = XINT (keysym), bufp++, *bufp = 0;
	  else
	    {
	      CHECK_SYMBOL (keysym, 0);
	      if (EQ (keysym, QKlinefeed))	strcpy (bufp, "LFD");
	      else if (EQ (keysym, QKtab))	strcpy (bufp, "TAB");
	      else if (EQ (keysym, QKreturn))	strcpy (bufp, "RET");
	      else if (EQ (keysym, QKescape))	strcpy (bufp, "ESC");
	      else if (EQ (keysym, QKdelete))	strcpy (bufp, "DEL");
	      else if (EQ (keysym, QKspace))	strcpy (bufp, "SPC");
	      else if (EQ (keysym, QKbackspace))	strcpy (bufp, "BS");
	      else
		strcpy (bufp, (char *) XSYMBOL (keysym)->name->data);
	      if (!NILP (XCONS (rest)->cdr))
		signal_simple_error (GETTEXT ("invalid key description"),
				     key);
	    }
	}
      return build_string (buf);
    }
  return Fsingle_key_description
    (wrong_type_argument (intern ("char-or-event-p"), key));
}

DEFUN ("text-char-description", Ftext_char_description, Stext_char_description,
       1, 1, 0,
  "Return a pretty description of file-character CHAR.\n\
Unprintable characters turn into \"^char\" or \\NNN, depending on the value\n\
of the `ctl-arrow' variable.\n\
This differs from `single-key-description' in that it returns a description\n\
of a character from a buffer rather than a key read from the user.")
  (chr)
     Lisp_Object chr;
{
  char buf[200];
  char *p;
  unsigned int c;
  Lisp_Object ctl_arrow = current_buffer->ctl_arrow;
  int ctl_p = !NILP (ctl_arrow);
  int printable_min = (FIXNUMP (ctl_arrow)
		       ? XINT (ctl_arrow)
		       : ((EQ (ctl_arrow, Qt) || EQ (ctl_arrow, Qnil))
			  ? 256 : 160));

  if (EVENTP (chr))
    {
      Lisp_Object ch = Fevent_to_character (chr, Qnil, Qnil, Qt);
      if (NILP (ch))
	return
	  Fsignal (Qerror,
		   list2 (build_string
			  (GETTEXT ("character has no ASCII equivalent")),
                          Fcopy_event (chr, Qnil)));
      chr = ch;
    }

  CHECK_FIXNUM (chr, 0);

  c = XINT (chr) & 0377;
  p = buf;

  if (c >= printable_min)
    {
      *p++ = c;
    }
  else if (c < 040 && ctl_p)
    {
      *p++ = '^';
      *p++ = c + 64;		/* 'A' - 1 */
    }
  else if (c == 0177)
    {
      *p++ = '^';
      *p++ = '?';
    }
  else if (c >= 0200 || c < 040)
    {
      *p++ = '\\';
      *p++ = '0' + ((c & 0700) >> 6);
      *p++ = '0' + ((c & 0070) >> 3);
      *p++ = '0' + ((c & 0007));
    }
  else
    {
      *p++ = c;
    }

  *p = 0;
  return build_string (buf);
}


static Lisp_Object
where_is_internal (Lisp_Object definition, Lisp_Object map,
                   Lisp_Object shadow, Lisp_Object firstonly,
                   char *target_buffer);

/* >>> This function is a mess! */

/* >>> Doc doesn't match arglist order */
DEFUN ("where-is-internal", Fwhere_is_internal, Swhere_is_internal, 1, 5, 0,
  "Return list of keys that invoke DEFINITION in KEYMAP0 or KEYMAP1.\n\
If KEYMAP0 is nil, search only KEYMAP1.\n\
If KEYMAP1 is nil, use the current global map.\n\
\n\
If optional 4th arg FIRSTONLY is non-nil,\n\
return a string representing the first key sequence found,\n\
rather than a list of all possible key sequences.\n\
\n\
If optional 5th arg NOINDIRECT is non-nil, don't follow indirections\n\
to other keymaps or slots.  This makes it possible to search for an\n\
indirect definition itself.")
  (definition, keymap0, firstonly, keymap1, noindirect)
     Lisp_Object definition, keymap0, keymap1;
     Lisp_Object firstonly, noindirect;
     /* >>> RMSmacs args are (definition, keymap, firstonly, noindirect) */
{
  /* >>> NYI minor-mode-alist
   * >>> NYI extent-local maps
   */
  Lisp_Object found0;
  Lisp_Object found1;
  struct gcpro gcpro1;

  if (NILP (keymap1))
    keymap1 = Vcurrent_global_map;
  if (EQ (keymap0, keymap1))
    keymap0 = Qnil;

  if (NILP (keymap0))
    found0 = Qnil;
  else
    found0 = where_is_internal (definition, keymap0, Qnil, 
                                firstonly, 0);

  if (!NILP (firstonly) && !NILP (found0))
    return found0;
  GCPRO1 (found0);

  found1 = where_is_internal (definition, keymap1, keymap0, 
                              firstonly, 0);
  UNGCPRO;
  if (!NILP (firstonly))
    return found1;
  else if (NILP (found1))
    return found0;
  else if (NILP (found0))
    return found1;
  else
    return nconc2 (found0, found1);
}


/* This function is like
   (key-description (where-is-internal def local-map t global-map))
   except that it writes its output into a (char *) buffer that you 
   provide; it doesn't cons (or allocate memory) at all, so it's
   very fast.  This is used by menubar.c.
 */
void
where_is_to_char (Lisp_Object definition, 
                  Lisp_Object local_keymap, Lisp_Object global_keymap, 
                  char *buf)
{
  Lisp_Object found;
  if (NILP (global_keymap))
    global_keymap = Vcurrent_global_map;
  if (EQ (local_keymap, global_keymap))
    local_keymap = Qnil;

  buf[0] = 0;
  if (!NILP (local_keymap))
    {
      found = where_is_internal (definition, local_keymap, Qnil, Qt, buf);
      if (!NILP (found))
	return;
    }
  where_is_internal (definition, global_keymap, local_keymap, Qt, buf);
}


static Lisp_Object 
raw_keys_to_keys (struct key_data *keys, int count)
{
  Lisp_Object result = make_vector (count, Qnil);
  while (count--)
    XVECTOR (result)->contents [count] = make_key_description (&(keys[count]),
                                                               1);
  return (result);
}


static void
format_raw_keys (struct key_data *keys, int count, char *buf)
{
  int i;
  struct Lisp_Event event;
  event.event_type = key_press_event;
  for (i = 0; i < count; i++)
    {
      event.event.key.keysym    = keys[i].keysym;
      event.event.key.modifiers = keys[i].modifiers;
      format_event_object (buf, &event, 1);
      buf += strlen (buf);
      if (i < count-1)
	buf[0] = ' ', buf++;
    }
}


/* definition is the thing to look for.
   map is a keymap.
   shadow is a keymap or nil; if it is a keymap, and there is different
   binding in it of a key that we are considering returning, then we
   reconsider.
   firstonly means give up after finding the first match;
   keys_so_far and modifiers_so_far describe which map we're looking in;
   If we're in the "meta" submap of the map that "C-x 4" is bound to,
   then keys_so_far will be {(control x), \4}, and modifiers_so_far_so_far
   will be MOD_META.  That is, keys_so_far is the chain of keys that we
   have followed, and modifiers_so_far_so_far is the bits (partial keys)
   beyond that.
   
   (keys_so_far is a global buffer and the keys_count arg says how much
   of it we're currently interested in.)
   
   If target_buffer is provided, then we write a key-description into it,
   to avoid consing a string.  This only works with firstonly on.
   */

struct where_is_closure
  {
    Lisp_Object definition;
    Lisp_Object shadow;
    int firstonly;
    int keys_count;
    unsigned int modifiers_so_far;
    char *target_buffer;
    struct key_data *keys_so_far;
    int keys_so_far_total_size;
    int keys_so_far_malloced;
  };

static Lisp_Object where_is_recursive_mapper (Lisp_Object map, void *arg);

static Lisp_Object
where_is_recursive_mapper (Lisp_Object map, void *arg)
{
  struct where_is_closure *c = arg;
  Lisp_Object definition = c->definition;
  Lisp_Object shadow = c->shadow;
  const int firstonly = c->firstonly;
  const unsigned int keys_count = c->keys_count;
  const unsigned int modifiers_so_far = c->modifiers_so_far;
  char *target_buffer = c->target_buffer;
  Lisp_Object keys = Fgethash (definition,
                               XKEYMAP (map)->inverse_table,
                               Qnil);
  Lisp_Object submaps;
  Lisp_Object result = Qnil;

  if (!NILP (keys))
  {
    /* Verify that this key binding is not shadowed by another binding
       for the same key, before we say it exists.  The mechanism: look
       for a local definition of this key and if it is defined and does
       not match what we found then ignore this key.  Either nil or
       number as value from raw_lookup_key() means undefined.
       */
    Lisp_Object shadowed = Qnil;
    struct key_data *so_far = c->keys_so_far;

    for (;;)
    {
      Lisp_Object k = ((CONSP (keys)) ? XCONS (keys)->car : keys);

      so_far [keys_count].keysym = k;
      so_far [keys_count].modifiers = modifiers_so_far;
      if (!NILP (shadow))
        shadowed = raw_lookup_key (shadow,
                                   so_far,
                                   keys_count + 1, 
                                   0, 1);
      if (NILP (shadowed) ||
          FIXNUMP (shadowed) ||
          EQ (shadowed, definition))
      {
        if (target_buffer)
        {
          if (!firstonly) abort ();
          format_raw_keys (so_far,
                           keys_count + 1,
                           target_buffer);
          return (make_number (1));
        }
        else if (firstonly)
          return raw_keys_to_keys (so_far, keys_count + 1);
        else
          result = Fcons (raw_keys_to_keys (so_far, keys_count + 1),
                          result);
      }
      if (!CONSP (keys)) break;
      keys = XCONS (keys)->cdr;
    }
  }

  /* Now search the sub-keymaps of this map.
     If we're in "firstonly" mode and have already found one, this 
     point is not reached.  If we get one from lower down, either
     return it immediately (in firstonly mode) or tack it onto the
     end of the ones we've gotten so far.
     */
  for (submaps = keymap_submaps (map);
       !NILP (submaps);
       submaps = XCONS (submaps)->cdr)
  {
    Lisp_Object key    = XCONS (XCONS (submaps)->car)->car;
    Lisp_Object submap = XCONS (XCONS (submaps)->car)->cdr;
    unsigned int lower_modifiers;
    int lower_keys_count = keys_count;
    unsigned int bucky;

    submap = get_keymap (submap, 0, 1);

    if (EQ (submap, map))
      /* Arrgh!  Some loser has introduced a loop... */
      continue;

    /* If this is not a keymap, then that's probably because someone
       did an `fset' of a symbol that used to point to a map such that
       it no longer does.  Sigh.  Ignore this, and invalidate the cache
       so that it doesn't happen to us next time too.
       */
    if (NILP (submap))
    {
      XKEYMAP (map)->sub_maps_cache = Qt;
      continue;
    }

    /* If the map is a "bucky" map, then add a bit to the
       modifiers_so_far list.
       Otherwise, add a new raw_key onto the end of keys_so_far.
       */
    bucky = MODIFIER_HASH_KEY_P (key);
    if (bucky != 0)
      lower_modifiers = (modifiers_so_far | bucky);
    else
    {
      struct key_data *so_far = c->keys_so_far;
      lower_modifiers = 0;
      so_far [lower_keys_count].keysym = key;
      so_far [lower_keys_count].modifiers = modifiers_so_far;
      lower_keys_count++;
    }

    if (lower_keys_count >= c->keys_so_far_total_size)
    {
      int size = lower_keys_count + 50;
      if (! c->keys_so_far_malloced)
      {
        struct key_data *new = xmalloc (size * sizeof (struct key_data));
        memcpy (new, c->keys_so_far,
                c->keys_so_far_total_size * sizeof (struct key_data));
      }
      else
        c->keys_so_far = xrealloc (c->keys_so_far,
                                   size * sizeof (struct key_data));

      c->keys_so_far_total_size = size;
      c->keys_so_far_malloced = 1;
    }

    {
      Lisp_Object lower;

      c->keys_count = lower_keys_count;
      c->modifiers_so_far = lower_modifiers;

      lower = traverse_keymaps (submap, Qnil, where_is_recursive_mapper, c);
      c->keys_count = keys_count;
      c->modifiers_so_far = modifiers_so_far;

      if (!firstonly)
        result = nconc2 (lower, result);
      else if (!NILP (lower))
        return (lower);
    }
  }
  return (result);
}


static Lisp_Object
where_is_internal (Lisp_Object definition, Lisp_Object map,
                   Lisp_Object shadow, Lisp_Object firstonly,
                   char *target_buffer)
{
  Lisp_Object result;
  struct key_data raw[20];
  struct where_is_closure c;
  c.definition = definition;
  c.shadow = shadow;
  c.firstonly = !NILP (firstonly);
  c.keys_count = 0;
  c.modifiers_so_far = 0;
  c.target_buffer = target_buffer;
  c.keys_so_far = raw;
  c.keys_so_far_total_size = countof (raw);
  c.keys_so_far_malloced = 0;

  result = traverse_keymaps (map, Qnil, where_is_recursive_mapper, &c);

  if (NILP (firstonly))
    result = Fnreverse (result);

  if (c.keys_so_far_malloced)
    xfree (c.keys_so_far);
  return (result);
}


DEFUN ("describe-bindings", Fdescribe_bindings, Sdescribe_bindings, 0, 1, "P",
  "Show a list of all defined keys, and their definitions.\n\
The list is put in a buffer, which is displayed.\n\
If the argument is non-null, then only the mouse bindings are displayed.")
  (mice_only_p)
  Lisp_Object mice_only_p;
{
  internal_with_output_to_temp_buffer (GETTEXT ("*Help*"),
				       NILP (mice_only_p)
				       ? describe_buffer_bindings
				       : describe_buffer_mouse_bindings,
				       Fcurrent_buffer (), Qnil);
  return Qnil;
}


static Lisp_Object
describe_buffer_bindings_1 (Lisp_Object descbuf, int mice_only_p)
{
  CONST char *heading =
    (mice_only_p
     ? GETTEXT ("button          binding\n------          -------\n")
     : GETTEXT ("key             binding\n---             -------\n"));
  Lisp_Object shadow = Qnil;
  struct gcpro gcpro1, gcpro2;
  GCPRO2 (descbuf, shadow);

  Fset_buffer (Vstandard_output);

  {
    Lisp_Object alist = Qnil;
    struct gcpro gcpro1;
    GCPRO1 (alist);
    for (alist = symbol_value_in_buffer (Qminor_mode_map_alist, descbuf);
         CONSP (alist);
         alist = XCONS (alist)->cdr)
    {
      Lisp_Object pair = XCONS (alist)->car;
      Lisp_Object map = minor_mode_keymap_predicate (pair, descbuf);
      struct gcpro gcpro1, gcpro2;
      GCPRO2 (alist, pair);

      QUIT;
      if (KEYMAPP (map))
      {
        insert_string (GETTEXT ("Minor Mode Bindings for "));
        insert_string ("`");
        insert1 (Fsymbol_name (XCONS (pair)->car));
        insert_string ("':\n");        
        insert_string (heading);
        describe_map_tree (map, 0, shadow, Qnil, mice_only_p);
        shadow = Fcons (map, shadow);
        insert_string ("\n");
      }
      UNGCPRO;
    }
    UNGCPRO;
  }

  if (!NILP (XBUFFER (descbuf)->keymap))
    {
      insert_string (GETTEXT ("Local Bindings:\n"));
      insert_string (heading);
      describe_map_tree (XBUFFER (descbuf)->keymap, 0, Qnil, Qnil,
                         mice_only_p);
      shadow = Fcons (XBUFFER (descbuf)->keymap, shadow);
      insert_string ("\n");
    }

  insert_string (GETTEXT ("Global Bindings:\n"));
  insert_string (heading);

  describe_map_tree (Vcurrent_global_map, 0, shadow, Qnil, mice_only_p);

  Fset_buffer (descbuf);
  UNGCPRO;
  return Qnil;
}

static Lisp_Object
describe_buffer_bindings (Lisp_Object descbuf)
{
  return describe_buffer_bindings_1 (descbuf, 0);
}

static Lisp_Object
describe_buffer_mouse_bindings (Lisp_Object descbuf)
{
  return describe_buffer_bindings_1 (descbuf, 1);
}


/* Insert a desription of the key bindings in STARTMAP,
    followed by those of all maps reachable through STARTMAP.
   If PARTIAL is nonzero, omit certain "uninteresting" commands
    (such as `undefined').
   If SHADOW is non-nil, it is a list of other maps;
    don't mention keys which would be shadowed by any of them
   If PREFIX is non-nil, only list bindings which start with those keys
 */

void
describe_map_tree (Lisp_Object startmap, int partial, Lisp_Object shadow,
                   Lisp_Object prefix, int mice_only_p)
{
  Lisp_Object maps = Qnil;
  struct gcpro gcpro1, gcpro2;  /* get_keymap may autoload */
  GCPRO2 (maps, shadow);

  maps = Faccessible_keymaps (startmap, prefix);

  for (; !NILP (maps); maps = Fcdr (maps))
    {
      Lisp_Object sub_shadow = Qnil;
      Lisp_Object elt = Fcar (maps);
      Lisp_Object tail = shadow;
      int no_prefix = (VECTORP (Fcar (elt))
                       && XINT (Flength (Fcar (elt))) == 0);
      struct gcpro gcpro1, gcpro2, gcpro3;
      GCPRO3 (sub_shadow, elt, tail);

      for (; CONSP (tail); tail = XCONS (tail)->cdr)
      {
        Lisp_Object sh = XCONS (tail)->car;

        /* If the sequence by which we reach this keymap is zero-length,
           then the shadow maps for this keymap are just SHADOW.  */
        if (no_prefix)
          ;
        /* If the sequence by which we reach this keymap actually has
           some elements, then the sequence's definition in SHADOW is
           what we should use.  */
        else
	{
          sh = Flookup_key (sh, Fcar (elt), Qt);
	  if (FIXNUMP (sh))
	    sh = Qnil;
	}

        if (!NILP (sh))
        {
          Lisp_Object shm = get_keymap (sh, 0, 1);
          if (!KEYMAPP (shm))
            /* If sh is not nil and not a keymap, it completely shadows
               this map, so don't describe this map at all.  */
            goto SKIP;
          sub_shadow = Fcons (shm, sub_shadow);
        }
      }

      {
        /* Describe the contents of map MAP, assuming that this map
           itself is reached by the sequence of prefix keys KEYS (a vector).
           PARTIAL and SHADOW are as in `describe_map_tree'.  */
        Lisp_Object keysdesc
          = ((!no_prefix)
             ? concat2 (Fkey_description (Fcar (elt)), Vsingle_space_string)
             : Qnil);
        describe_map (Fcdr (elt), keysdesc,
                      describe_command,
                      partial,
                      sub_shadow,
                      mice_only_p);
      }
    SKIP:
      ;
    }
  UNGCPRO;
}


static void
describe_command (Lisp_Object definition)
{
  struct gcpro gcpro1;
  GCPRO1 (definition);

  Findent_to (make_number (16), make_number (3));
  if (SYMBOLP (definition))
    {
      insert1 (Fsymbol_name (definition));
    }
  else if (STRINGP (definition) || VECTORP (definition))
    {
      insert_string (GETTEXT ("Kbd Macro: "));
      insert1 (Fkey_description (definition));
    }
  else if (COMPILEDP (definition))
    insert_string (GETTEXT ("Anonymous Compiled Function"));
  else if (CONSP (definition) && EQ (XCONS (definition)->car, Qlambda))
    insert_string (GETTEXT ("Anonymous Lambda"));
  else if (KEYMAPP (definition))
    {
      Lisp_Object name = XKEYMAP (definition)->name;
      if (STRINGP (name) || (SYMBOLP (name) && !NILP (name)))
	{
	  insert_string (GETTEXT ("Prefix command "));
	  if (SYMBOLP (name) 
	      && EQ (find_symbol_value (name), definition))
	    insert1 (Fsymbol_name (name));
	  else
	    {
	      insert1 (Fprin1_to_string (name, Qnil));
	    }
	}
      else
	insert_string (GETTEXT ("Prefix Command"));
    }
  else
    insert_string (GETTEXT ("??"));

  insert_string ("\n");
  UNGCPRO;
}

struct describe_map_closure
  {
    Lisp_Object *list;	 /* pointer to the list to update */
    Lisp_Object partial; /* whether to ignore suppressed commands */
    Lisp_Object shadow;	 /* list of maps shadowing this one */
    Lisp_Object self;	 /* this map */
    Lisp_Object self_root; /* this map, or some map that has this map as
                              a parent.  this is the base of the tree */
    int mice_only_p;	 /* whether we are to display only button bindings */
  };

struct describe_map_shadow_closure
  {
    CONST struct key_data *raw_key;
    Lisp_Object self;
  };

static Lisp_Object
describe_map_mapper_shadow_search (Lisp_Object map, void *arg)
{
  struct describe_map_shadow_closure *c = arg;

  if (EQ (map, c->self))
    return (Qzero);              /* Not shadowed; terminate search */
  else if (!NILP (keymap_lookup_directly (map,
                                          c->raw_key->keysym,
                                          c->raw_key->modifiers)))
    return (Qt);
  else
    return (Qnil);
}
     

static Lisp_Object
keymap_lookup_inherited_mapper (Lisp_Object km, void *arg)
{
  struct key_data *k = arg;
  return (keymap_lookup_directly (km, k->keysym, k->modifiers));
}


static void
describe_map_mapper (CONST struct key_data *key,
                     Lisp_Object binding,
		     void *describe_map_closure)
{
  struct describe_map_closure *closure = describe_map_closure;
  Lisp_Object keysym = key->keysym;
  unsigned int modifiers = key->modifiers;

  /* Dont mention suppressed commands.  */
  if (SYMBOLP (binding)
      && !NILP (closure->partial)
      && !NILP (Fget (binding, closure->partial, Qnil)))
    return;
	      
  /* If we're only supposed to display mouse bindings and this isn't one,
     then bug out. */
  if (closure->mice_only_p &&
      (! (EQ (keysym, Qbutton0) || EQ (keysym, Qbutton1)
          || EQ (keysym, Qbutton2) || EQ (keysym, Qbutton3)
          || EQ (keysym, Qbutton4) || EQ (keysym, Qbutton5)
          || EQ (keysym, Qbutton6) || EQ (keysym, Qbutton7))))
    return;

  /* If this command in this map is shadowed by some other map, ignore it. */
  {
    Lisp_Object tail;

    for (tail = closure->shadow; CONSP (tail); tail = XCONS (tail)->cdr)
    {
      QUIT;
      if (!NILP (traverse_keymaps (XCONS (tail)->car, Qnil,
                                   keymap_lookup_inherited_mapper,
                                   /* Cast to discard `const' */
                                   (void *)key)))
        return;
    }
  }

  /* If this key is in some map of which this map is a parent, then ignore
     it (in that case, it has been shadowed.)
   */
  {
    Lisp_Object sh;
    struct describe_map_shadow_closure c;
    c.raw_key = key;
    c.self = closure->self;

    sh = traverse_keymaps (closure->self_root, Qnil,
                           describe_map_mapper_shadow_search, &c);
    if (!NILP (sh) && !EQ (sh, Qzero))
      return;
  }

  /* Otherwise add it to the list to be sorted. */
  *(closure->list) = Fcons (Fcons (Fcons (keysym, make_number (modifiers)),
                                   binding),
			    *(closure->list));
}


static int
describe_map_sort_predicate (Lisp_Object obj1, Lisp_Object obj2, 
			     Lisp_Object pred)
{
  /* obj1 and obj2 are conses of the form
     ( ( <keysym> . <modifiers> ) . <binding> )
     keysym and modifiers are used, binding is ignored.
   */
  unsigned int bit1, bit2;
  obj1 = XCONS (obj1)->car;
  obj2 = XCONS (obj2)->car;
  bit1 = XINT (XCONS (obj1)->cdr);
  bit2 = XINT (XCONS (obj2)->cdr);
  if (bit1 != bit2)
    return ((bit1 < bit2) ? 1 : -1);
  else
    return map_keymap_sort_predicate (obj1, obj2, pred);
}

/* Elide 2 or more consecutive numeric keysyms bound to the same thing,
   or 2 or more symbolic keysyms that are bound to the same thing and
   have consecutive character-set-properties.
 */
static int
elide_next_two_p (Lisp_Object list)
{
  Lisp_Object s1, s2;

#define CAR(x) (XCONS(x)->car)
#define CDR(x) (XCONS(x)->cdr)

  if (NILP (CDR (list)))
    return 0;

  /* next two bindings differ */
  if (!EQ (CDR (CAR (list)),
	   CDR (CAR (CDR (list)))))
    return 0;

  /* next two modifier-sets differ */
  if (!EQ (CDR (CAR (CAR (list))),
	   CDR (CAR (CAR (CDR (list))))))
    return 0;

  s1 = CAR (CAR (CAR (list)));
  s2 = CAR (CAR (CAR (CDR (list))));

  if (SYMBOLP (s1))
    {
      Lisp_Object code = Fget (s1, Vcharacter_set_property, Qnil);
      if (FIXNUMP (code)) s1 = code;
      else return 0;
    }
  if (SYMBOLP (s2))
    {
      Lisp_Object code = Fget (s2, Vcharacter_set_property, Qnil);
      if (FIXNUMP (code)) s2 = code;
      else return 0;
    }

  if (XINT (s1) == XINT (s2) ||
      XINT (s1) + 1 == XINT (s2))
    return 1;
  return 0;

#undef CDR
#undef CAR
}


static Lisp_Object
describe_map_parent_mapper (Lisp_Object keymap, void *arg)
{
  struct describe_map_closure *describe_map_closure = arg;
  describe_map_closure->self = keymap;
  map_keymap (XKEYMAP (keymap)->table,
              0, /* don't sort: we'll do it later */
              describe_map_mapper, describe_map_closure);
  return (Qnil);
}


static void
describe_map (Lisp_Object keymap, Lisp_Object elt_prefix,
	      void (*elt_describer) (Lisp_Object),
	      int partial, 
	      Lisp_Object shadow,
	      int mice_only_p)
{
  struct describe_map_closure describe_map_closure;
  Lisp_Object list = Qnil;
  int printable_min = (FIXNUMP (current_buffer->ctl_arrow)
		       ? XINT (current_buffer->ctl_arrow)
		       : ((EQ (current_buffer->ctl_arrow, Qt)
                           || EQ (current_buffer->ctl_arrow, Qnil))
			  ? 256 : 160));
  int elided = 0;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

  keymap = get_keymap (keymap, 1, 1);
  describe_map_closure.partial = (partial ? Qsuppress_keymap : Qnil);
  describe_map_closure.shadow = shadow;
  describe_map_closure.list = &list;
  describe_map_closure.self_root = keymap;
  describe_map_closure.mice_only_p = mice_only_p;

  GCPRO4 (keymap, elt_prefix, shadow, list);

  traverse_keymaps (keymap, Qnil,
                    describe_map_parent_mapper, &describe_map_closure);

  if (!NILP (list))
    {
      list = list_sort (list, Qnil, describe_map_sort_predicate);
      insert_string ("\n");
      while (!NILP (list))
	{
          Lisp_Object elt = XCONS (XCONS (list)->car)->car;
	  Lisp_Object keysym = XCONS (elt)->car;
	  unsigned int modifiers = XINT (XCONS (elt)->cdr);

	  if (!NILP (elt_prefix))
	    insert_from_string (elt_prefix, -1);

	  if (modifiers & MOD_META)    insert_string ("M-");
	  if (modifiers & MOD_CONTROL) insert_string ("C-");
	  if (modifiers & MOD_SUPER)   insert_string ("S-");
	  if (modifiers & MOD_HYPER)   insert_string ("H-");
	  if (modifiers & MOD_SYMBOL)  insert_string ("Sym-");
	  if (modifiers & MOD_SHIFT)   insert_string ("Sh-");
	  if (SYMBOLP (keysym))
	    {
	      Lisp_Object code = Fget (keysym, Vcharacter_set_property, Qnil);
	      int c = (FIXNUMP (code) ? XINT (code) : -1);
	      /* Calling Fsingle_key_description() would cons more */
	      if (EQ (keysym, QKlinefeed))	 insert_string ("LFD");
	      else if (EQ (keysym, QKtab))	 insert_string ("TAB");
	      else if (EQ (keysym, QKreturn))	 insert_string ("RET");
	      else if (EQ (keysym, QKescape))	 insert_string ("ESC");
	      else if (EQ (keysym, QKdelete))	 insert_string ("DEL");
	      else if (EQ (keysym, QKspace))	 insert_string ("SPC");
	      else if (EQ (keysym, QKbackspace)) insert_string ("BS");
	      else if (c >= printable_min)	 insert_char (c);
	      else insert1 (Fsymbol_name (keysym));
	    }
	  else if (FIXNUMP (keysym))
	    insert_char (XINT (keysym));
	  else
	    insert_string (GETTEXT ("---bad keysym---"));

	  if (elided)
	    elided = 0;
	  else
	    {
	      int k = 0;

	      while (elide_next_two_p (list))
		{
		  k++;
		  list = XCONS (list)->cdr;
		}
	      if (k != 0)
		{
		  if (k == 1)
		    insert_string (", ");
		  else
		    insert_string (" .. ");
		  elided = 1;
		  continue;
		}
	    }

	  /* Print a description of the definition of this character.  */
	  (*elt_describer) (XCONS (XCONS (list)->car)->cdr);
	  list = XCONS (list)->cdr;
	}
    }
  UNGCPRO;
}


void
syms_of_keymap ()
{
  DEFVAR_INT ("meta-prefix-char", &meta_prefix_char,
    "Meta-prefix character code.  Must be an ASCII integer.\n\
This character followed by some character `foo' turns into `Meta-foo'.\n\
To disable the meta-prefix-char, set it to a negative number.");
  meta_prefix_char = 033;

  DEFVAR_LISP ("mouse-grabbed-buffer", &Vmouse_grabbed_buffer,
  "A buffer which should be consulted first for all mouse activity.\n\
When a mouse-clicked it processed, it will first be looked up in the\n\
local-map of this buffer, and then through the normal mechanism if there\n\
is no binding for that click.  This buffer's value of `mode-motion-hook'\n\
will be consulted instead of the `mode-motion-hook' of the buffer of the\n\
window under the mouse.  You should *bind* this, not set it.");
  Vmouse_grabbed_buffer = Qnil;

  /* defsymbol (&Qoverriding_local_map, "overriding-local-map"); */
  DEFVAR_LISP ("overriding-local-map", &Voverriding_local_map,
    "Keymap that overrides all other local keymaps.\n\
If this variable is non-nil, it is used as a keymap instead of the\n\
buffer's local map, and the minor mode keymaps and extent-local keymaps.\n\
You should *bind* this, not set it.");
  Voverriding_local_map = Qnil;

  defsymbol (&Qminor_mode_map_alist, "minor-mode-map-alist");
  Fset (Qminor_mode_map_alist, Qnil);

  DEFVAR_INT ("keymap-tick", &keymap_tick,
	      "Incremented for each change to any keymap.");
  keymap_tick = 0;

  defsymbol (&Qkeymap, "keymap");
  defsymbol (&Qkeymapp, "keymapp");

  defsymbol (&Qsuppress_keymap, "suppress-keymap");

  defsymbol (&Qmode_line_map, "mode-line-map");

  staticpro (&Vcurrent_global_map);

  Vsingle_space_string = make_pure_string (" ", 1, 1);
  staticpro (&Vsingle_space_string);

  defsubr (&Skeymap_parents);
  defsubr (&Sset_keymap_parents);
/*defsubr (&Skeymap_name); */
  defsubr (&Sset_keymap_name);
  defsubr (&Skeymap_prompt);
  defsubr (&Sset_keymap_prompt);
  defsubr (&Skeymap_default_binding);
  defsubr (&Sset_keymap_default_binding);

  defsubr (&Skeymapp);
  defsubr (&Smake_keymap);
  defsubr (&Smake_sparse_keymap);

  defsubr (&Scopy_keymap);
  defsubr (&Skeymap_fullness);
  defsubr (&Smap_keymap);
  defsubr (&Sdefine_key);
  defsubr (&Slookup_key);
  defsubr (&Skey_binding);
  defsubr (&Suse_global_map);
  defsubr (&Suse_local_map);
  defsubr (&Scurrent_local_map);
  defsubr (&Scurrent_global_map);
  defsubr (&Scurrent_keymaps);
  defsubr (&Saccessible_keymaps);
  defsubr (&Skey_description);
  defsubr (&Ssingle_key_description);
  defsubr (&Swhere_is_internal);
  defsubr (&Sdescribe_bindings);

  defsubr (&Stext_char_description);

  defsymbol (&Qcontrol, "control");
  defsymbol (&Qctrl, "ctrl");
  defsymbol (&Qmeta, "meta"); 
  defsymbol (&Qsuper, "super"); 
  defsymbol (&Qhyper, "hyper"); 
  defsymbol (&Qsymbol, "symbol");
  defsymbol (&Qshift, "shift");
  defsymbol (&Qbutton0, "button0");
  defsymbol (&Qbutton1, "button1");
  defsymbol (&Qbutton2, "button2");
  defsymbol (&Qbutton3, "button3");
  defsymbol (&Qbutton4, "button4");
  defsymbol (&Qbutton5, "button5");
  defsymbol (&Qbutton6, "button6");
  defsymbol (&Qbutton7, "button7");
  defsymbol (&Qbutton0up, "button0up");
  defsymbol (&Qbutton1up, "button1up");
  defsymbol (&Qbutton2up, "button2up");
  defsymbol (&Qbutton3up, "button3up");
  defsymbol (&Qbutton4up, "button4up");
  defsymbol (&Qbutton5up, "button5up");
  defsymbol (&Qbutton6up, "button6up");
  defsymbol (&Qbutton7up, "button7up");
  defsymbol (&Qmenu_selection, "menu-selection");
  defsymbol (&QLFD, "LFD");
  defsymbol (&QTAB, "TAB");
  defsymbol (&QRET, "RET");
  defsymbol (&QESC, "ESC");
  defsymbol (&QDEL, "DEL");
  defsymbol (&QBS, "BS");
}

void
keys_of_keymap ()
{
  Lisp_Object ESC_prefix = intern ("ESC-prefix");
  Lisp_Object meta_disgustitute;

  Vcurrent_global_map = Fmake_keymap ();

  meta_disgustitute = Fmake_keymap ();
  Ffset (ESC_prefix, meta_disgustitute);
  keymap_store_internal (MAKE_MODIFIER_HASH_KEY (MOD_META),
                         XKEYMAP (Vcurrent_global_map),
                         meta_disgustitute);
  XKEYMAP (Vcurrent_global_map)->sub_maps_cache = Qt;
}
