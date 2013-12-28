/* Manipulation of keymaps
   Copyright (C) 1985-1993 Free Software Foundation, Inc.
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
#include "keymap.h"
#include "commands.h"
#include "buffer.h"
#include "insdel.h"
#include "events.h"
#include "elhash.h"


/* A keymap contains four slots:

   parent	   A keymap to search after this one if no match.
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

   Sequences of keys are stored in the obvious way: if the sequence of keys
   "abc" was bound to some command `foo', the hierarchy would look like

      keymap-1: associates "a" with keymap-2
      keymap-2: associates "b" with keymap-3
      keymap-3: associates "c" with foo

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
  /*>>> Multiple (search-list) parents? */
  Lisp_Object parent;		/* The keymap to be searched after this one */
  Lisp_Object prompt;           /* Qnil or a string to print in the minibuffer
                                 *  when reading from this keymap */
  /* >>>>> NYI Lisp_Object global_map_override; */
                                   
  Lisp_Object table;		/* The contents of this keymap */
  Lisp_Object inverse_table;	/* The inverse mapping of the above */

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

/* Actually allocate storage for these variables */

static Lisp_Object Vcurrent_global_map; /* Always a keymap */

static Lisp_Object Vmouse_grabbed_buffer;

/* This is incremented whenever a change is made to a keymap.  This is
   so that things which care (such as the menubar code) can recompute
   privately-cached data when the user has changed their keybindings.
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
static void describe_map (Lisp_Object map, Lisp_Object keys,
                          int partial, 
                          Lisp_Object shadow,
                          int mice_only_p);
static int bucky_sym_to_bucky_bit (Lisp_Object sym);
static void describe_map_1 (Lisp_Object keymap, Lisp_Object elt_prefix,
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


static Lisp_Object mark_keymap (Lisp_Object, void (*) (Lisp_Object));
static void print_keymap (Lisp_Object, Lisp_Object, int);
static int sizeof_keymap (void *h) { return (sizeof (struct keymap)); }
/* #### add keymap_equal */
DEFINE_LRECORD_IMPLEMENTATION (lrecord_keymap,
                               mark_keymap, print_keymap, 
                               0, sizeof_keymap, 0);
static Lisp_Object
mark_keymap (Lisp_Object obj, void (*markobj) (Lisp_Object))
{
  struct keymap *keymap = XKEYMAP (obj);
  ((markobj) (keymap->parent));
  ((markobj) (keymap->prompt));
  ((markobj) (keymap->inverse_table));
  ((markobj) (keymap->sub_maps_cache));
  ((markobj) (keymap->name));
  return (keymap->table);
}
  
static void
print_keymap (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  struct keymap *keymap = XKEYMAP (obj);
  char buf[30];
  int size = XFASTINT (Fkeymap_fullness (obj));
  if (print_readably)
    error ("printing unreadable object #<keymap 0x%x>", keymap->header.uid);
  write_string_1 ("#<keymap ", -1, printcharfun);
  if (!NILP (keymap->name))
    print_internal (keymap->name, printcharfun, 1);
  sprintf (buf, "%s%d entr%s 0x%x>",
           ((NILP (keymap->name)) ? "" : " "),
           size,
           ((size == 1) ? "y" : "ies"),
           keymap->header.uid);
  write_string_1 (buf, -1, printcharfun);
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
  Lisp_Object new_keys;
  Lisp_Object tail;
  Lisp_Object *prev;

  if (EQ (keys, Qunbound))
    abort ();

  new_keys = keys;
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
keymap_store_internal (keysym, keymap, value)
     Lisp_Object keysym, value;
     struct keymap *keymap;
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


/* Relies on caller to gc-protect km, keysym */
static Lisp_Object
keymap_lookup_directly (km, keysym, modifiers)
     Lisp_Object km, keysym;
     int modifiers;
{
  struct keymap *keymap;
  Lisp_Object submap;
  Lisp_Object submap_name;
  int submap_bit;

  if (!KEYMAPP (km)) abort ();
  keymap = XKEYMAP (km);
  if (! modifiers)
    {
      /* If the keysym is a one-character symbol, use the char code instead. */
      if (SYMBOLP (keysym) && XSYMBOL (keysym)->name->size == 1)
	keysym = make_number (XSYMBOL (keysym)->name->data [0]);
      return (Fgethash (keysym, keymap->table, Qnil));
    }
  else if (modifiers & MOD_META)
    submap_name = Qmeta, submap_bit = MOD_META;
  else if (modifiers & MOD_CONTROL)
    submap_name = Qcontrol, submap_bit = MOD_CONTROL;
  else if (modifiers & MOD_SUPER)
    submap_name = Qsuper, submap_bit = MOD_SUPER;
  else if (modifiers & MOD_HYPER)
    submap_name = Qhyper, submap_bit = MOD_HYPER;
  else if (modifiers & MOD_SYMBOL)
    submap_name = Qsymbol, submap_bit = MOD_SYMBOL;
  else if (modifiers & MOD_SHIFT)
    submap_name = Qshift, submap_bit = MOD_SHIFT;
  else
    abort ();

  submap = keymap_lookup_directly (km, submap_name, 0);
  if (NILP (submap))
    return Qnil;
  submap = get_keymap (submap, 1, 1);
  /* Durst hope for tail-recursion? */
  return (keymap_lookup_directly (submap,
				  keysym, (modifiers & (~submap_bit))));
}


/* Relies on caller to gc-protect keymap, keysym, value */
static void
keymap_store (keymap, keysym, modifiers, value)
     Lisp_Object keymap, keysym, value;
     int modifiers;
{
  Lisp_Object submap_name, submap;
  int submap_bit;

  if (!KEYMAPP (keymap))
    keymap = get_keymap (keymap, 1, 1);
  XKEYMAP (keymap)->sub_maps_cache = Qt; /* Invalidate cache */
  if (! modifiers)
    {
      /* If the keysym is a one-character symbol, use the char code instead. */
      if (SYMBOLP (keysym) && XSYMBOL (keysym)->name->size == 1)
	keysym = make_number (XSYMBOL (keysym)->name->data [0]);
      keymap_store_internal (keysym, XKEYMAP (keymap), value);
      return;
    }
  else if (modifiers & MOD_META)
    submap_name = Qmeta, submap_bit = MOD_META;
  else if (modifiers & MOD_CONTROL)
    submap_name = Qcontrol, submap_bit = MOD_CONTROL;
  else if (modifiers & MOD_SUPER)
    submap_name = Qsuper, submap_bit = MOD_SUPER;
  else if (modifiers & MOD_HYPER)
    submap_name = Qhyper, submap_bit = MOD_HYPER;
  else if (modifiers & MOD_SYMBOL)
    submap_name = Qsymbol, submap_bit = MOD_SYMBOL;
  else if (modifiers & MOD_SHIFT)
    submap_name = Qshift, submap_bit = MOD_SHIFT;
  else
    abort ();

  submap = keymap_lookup_directly (keymap, submap_name, 0);
  if (NILP (submap))
    {
      submap = Fmake_sparse_keymap ();
      /* User won't see this, but it is nice for debugging Emacs */
      XKEYMAP (submap)->name = list2 (submap_name, keymap);
      keymap_store (keymap, submap_name, 0, submap);
    }
  keymap_store (submap, keysym, (modifiers & (~submap_bit)), value);
}



struct keymap_submaps_closure
{
  Lisp_Object *result_locative;
};

static void
keymap_submaps_mapper_0 (const void *hash_key, void *hash_contents, 
                         void *keymap_submaps_closure)
{
  Lisp_Object contents;
  VOID_TO_LISP (contents, hash_contents);
  /* Perform any autoloads, etc */
  (void) Fkeymapp (contents);
}

static void
keymap_submaps_mapper (const void *hash_key, void *hash_contents, 
                       void *keymap_submaps_closure)
{
  Lisp_Object key, contents;
  Lisp_Object *result_locative;
  CVOID_TO_LISP (key, hash_key);
  VOID_TO_LISP (contents, hash_contents);
  result_locative = (((struct keymap_submaps_closure *) keymap_submaps_closure)
		     ->result_locative);

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

  keymap->parent = Qnil;
  keymap->table = Qnil;
  keymap->prompt = Qnil;
  /* keymap->inhibit_global_map = Qnil; */
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

DEFUN ("keymap-parent", Fkeymap_parent, Skeymap_parent, 1, 1, 0,
       "Returns the `parent' keymap of the given keymap, or nil.\n\
The parent of a keymap is searched for keybindings when a key sequence\n\
isn't bound in this one.  The (current-global-map) is the default parent\n\
of all keymaps.")
     (keymap)
     Lisp_Object keymap;
{
  keymap = get_keymap (keymap, 1, 1);
  return XKEYMAP (keymap)->parent;
}

DEFUN ("set-keymap-parent", Fset_keymap_parent, Sset_keymap_parent, 2, 2, 0,
       "Sets the `parent' keymap of the given keymap.\n\
The parent of a keymap is searched for keybindings when a key sequence\n\
isn't bound in this one.  The (current-global-map) is the default parent\n\
of all keymaps.")
     (keymap, parent)
     Lisp_Object keymap, parent;
{
  Lisp_Object k;
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (keymap, parent);
  keymap = get_keymap (keymap, 1, 1);

  if (!NILP (parent))
  {
    /* Require that it be an actual keymap object, rather than a symbol
       with a (crockish) symbol-function which is a keymap */
    CHECK_KEYMAP (parent, 1); /* get_keymap (parent, 1, 1); */
  }

  for (k = parent; !NILP (k); k = XKEYMAP (k)->parent)
  {
    QUIT;
    if (EQ (k, keymap))
      signal_error (Qerror, 
                    list3 (build_string ("Cyclic keymap indirection"),
                           keymap, 
                           parent));
  }
  keymap_tick++;
  XKEYMAP (keymap)->parent = parent;
  UNGCPRO;
  return (parent);
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

DEFUN ("keymap-prompt", Fkeymap_prompt, Skeymap_prompt, 1, 2, 0,
  "Returns the `prompt' of the given keymap.\n\
If non-nil, the prompt is shown in the echo-area\n\
when reading a key-sequence to be looked-up in this keymap.")
     (keymap, use_inherited)
     Lisp_Object keymap, use_inherited;
{
  for (;;)
    {
      Lisp_Object prompt;
      keymap = get_keymap (keymap, 1, 1);
      prompt = XKEYMAP (keymap)->prompt;
      if (!NILP (prompt))
	return (prompt);
      if (NILP (use_inherited))
	return (Qnil);
      keymap = XKEYMAP (keymap)->parent;
      if (NILP (keymap))
	return (Qnil);
      QUIT;
    }
}


DEFUN ("keymapp", Fkeymapp, Skeymapp, 1, 1, 0,
  "Return t if ARG is a keymap object.\n\
The keymap may be autoloaded first if necessary.")
  (object)
     Lisp_Object object;
{
  register Lisp_Object tem = get_keymap (object, 0, 1);
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
  Lisp_Object tem;

  while (1)
    {
      tem = indirect_function (object, 0);
      
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
  while (1)
    {
      register Lisp_Object map;

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
          int modifiers;
          if (FIXNUMP (idx))
          {
            struct Lisp_Event event;
            event.event_type = empty_event;
            character_to_event (XINT (idx), &event);
            keysym = event.event.key.key;
            modifiers = event.event.key.modifiers;
          }
          else if (CONSP (idx))
          {
            keysym = XCONS (idx)->car;
            if (!FIXNUMP (XCONS (idx)->cdr))
              return Qnil;
            modifiers = XINT (XCONS (idx)->cdr);
          }
          else
            abort ();
          /* >>>> Doesn't search parent keymaps! */
          return keymap_lookup_directly (map, keysym, modifiers);
        }
      else if (STRINGP (XCONS (object)->car))
      {
	/* If the keymap contents looks like (STRING . DEFN),
	   use DEFN.
	   Keymap alist elements like (CHAR MENUSTRING . DEFN)
	   will be used by HierarKey menus.  */
	  object = XCONS (object)->cdr;
      }
      else
      {
	/* Anything else is really the value.  */
	return object;
      }
    }
}

static Lisp_Object
keymap_lookup_1 (Lisp_Object keymap, Lisp_Object keysym, int modifiers)
{
  return (get_keyelt (keymap_lookup_directly (keymap, keysym, modifiers)));
}


struct copy_keymap_inverse_closure
{
  Lisp_Object inverse_table;
};

static void
copy_keymap_inverse_mapper (const void *hash_key, void *hash_contents, 
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

  new_keymap->parent = keymap->parent;
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
copy_keymap_mapper (const void *hash_key, void *hash_contents, 
                    void *copy_keymap_closure)
{
  Lisp_Object key, contents;
  struct copy_keymap_closure *closure
    = (struct copy_keymap_closure *) copy_keymap_closure;

  CVOID_TO_LISP (key, hash_key);
  VOID_TO_LISP (contents, hash_contents);
  /* Don't recursively copy keymaps that are indirected through symbols. */
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
    if (bucky_sym_to_bucky_bit (XCONS (XCONS (sub_maps)->car)->car))
      {
	Lisp_Object sub_map = XCONS (XCONS (sub_maps)->car)->cdr;
	fullness--; /* don't count bucky maps */
	fullness += keymap_fullness (sub_map);
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


/* Given any kind of key-specifier, return a keysym and modifier mask.
 */
static void
define_key_parser (Lisp_Object spec,
                   Lisp_Object *keysym_return, int *modifiers_return)
{
  if (SYMBOLP (spec))
    spec = Fcons (spec, Qnil); /* be nice */

  if (FIXNUMP (spec))
    {
      struct Lisp_Event event;
      event.event_type = empty_event;
      character_to_event (XINT (spec), &event);
      *keysym_return = event.event.key.key;
      *modifiers_return = event.event.key.modifiers;
      return;
    }
  else if (EVENTP (spec))
    {
      switch (XEVENT (spec)->event_type)
	{
	case key_press_event:
	  *keysym_return = XEVENT (spec)->event.key.key;
	  *modifiers_return = XEVENT (spec)->event.key.modifiers;
	  return;
	case button_press_event:
	case button_release_event:
	  {
	    register int down =
	      (XEVENT (spec)->event_type == button_press_event);
	    switch (XEVENT (spec)->event.button.button) {
	    case 1: *keysym_return = (down ? Qbutton1 : Qbutton1up); break;
	    case 2: *keysym_return = (down ? Qbutton2 : Qbutton2up); break;
	    case 3: *keysym_return = (down ? Qbutton3 : Qbutton3up); break;
	    case 4: *keysym_return = (down ? Qbutton4 : Qbutton4up); break;
	    case 5: *keysym_return = (down ? Qbutton5 : Qbutton5up); break;
	    case 6: *keysym_return = (down ? Qbutton6 : Qbutton6up); break;
	    case 7: *keysym_return = (down ? Qbutton7 : Qbutton7up); break;
	    default: *keysym_return =(down ? Qbutton0 : Qbutton0up); break;
	    }
	    *modifiers_return = XEVENT (spec)->event.button.modifiers;
	    return;
	  }
	default:
	  signal_error (Qwrong_type_argument,
			list2 (build_string
			       ("unable to bind this type of event"),
			       spec));
	}
    }
  else if (CONSP (spec))
    {
      int modifiers = 0;
      int mod_p;
      Lisp_Object keysym = Qnil;
      Lisp_Object rest = spec;

      /* First, parse out the leading modifier symbols.
       */
      while (!NILP (rest)) {
	mod_p = 1;
	keysym = XCONS (rest)->car;
	if (EQ (keysym, Qcontrol))    modifiers |= MOD_CONTROL;
	else if (EQ (keysym, Qctrl))  modifiers |= MOD_CONTROL;
	else if (EQ (keysym, Qmeta))  modifiers |= MOD_META;
	else if (EQ (keysym, Qsuper)) modifiers |= MOD_SUPER;
	else if (EQ (keysym, Qhyper)) modifiers |= MOD_HYPER;
	else if (EQ (keysym, Qsymbol))modifiers |= MOD_SYMBOL;
	else if (EQ (keysym, Qshift)) modifiers |= MOD_SHIFT;
	else mod_p = 0;
	if (!NILP (XCONS (rest)->cdr))
	  {
	    if (! mod_p)
	      signal_error (Qerror,
			    list2 (build_string ("unknown modifier"), keysym));
	  }
	else
	  {
	    if (mod_p)
	      signal_error (Qerror,
			    list2 (build_string ("nothing but modifiers here"),
				   spec));
	  }
	rest = XCONS (rest)->cdr;
	QUIT;
      }

      /* Now, check and massage the trailing keysym specifier. */
      if (SYMBOLP (keysym))
	{
	  if (XSYMBOL (keysym)->name->size == 1)
	    {
	      keysym = make_number (XSYMBOL (keysym)->name->data [0]);
	      goto fixnum_keysym;
	    }
	}
      else if (FIXNUMP (keysym))
	{
	fixnum_keysym:
	  if (XINT (keysym) < ' ' || XINT (keysym) > 127)
	    signal_error (Qerror,
			  list2 (build_string
				 ("keysym must be in the printing ASCII range"),
				 keysym));
	  if (modifiers & MOD_SHIFT)
	    signal_error (Qerror,
			  list2 (build_string (
		    "the `shift' modifier may not be applied to ASCII keysyms"),
				 spec));
	}
      else
	{
	  signal_error (Qerror, 
			list2 (build_string ("unknown keysym specifier"),
			       keysym));
	}

      *keysym_return = keysym;
      *modifiers_return = modifiers;
      return;
    }
  else
    {
      signal_error (Qerror, 
		    list2 (build_string ("unknown key-sequence specifier"),
			   spec));
    }
}

/* This piece of crap is used by macros.c */
void
key_desc_list_to_event (list, event, allow_menu_events)
     Lisp_Object list, event;
     int allow_menu_events;
{
  Lisp_Object keysym;
  int modifiers;

  if (allow_menu_events &&
      CONSP (list) &&
      EQ (XCONS (list)->car, Qmenu_selection))
    {
      Lisp_Object fn, arg;
      if (! NILP (Fcdr (Fcdr (list))))
	signal_error (Qerror, list2 (build_string ("invalid menu event desc"),
                                     list));
      arg = Fcar (Fcdr (list));
      if (SYMBOLP (arg))
	fn = Qcall_interactively;
      else
	fn = Qeval;
      XEVENT (event)->channel = Qnil;
      XEVENT (event)->event_type = menu_event;
      XEVENT (event)->event.eval.function = fn;
      XEVENT (event)->event.eval.object = arg;
      return;
    }

  define_key_parser (list, &keysym, &modifiers);

  if (EQ (keysym, Qbutton0) || EQ (keysym, Qbutton0up) ||
      EQ (keysym, Qbutton1) || EQ (keysym, Qbutton1up) ||
      EQ (keysym, Qbutton2) || EQ (keysym, Qbutton2up) ||
      EQ (keysym, Qbutton3) || EQ (keysym, Qbutton3up) ||
      EQ (keysym, Qbutton4) || EQ (keysym, Qbutton4up) ||
      EQ (keysym, Qbutton5) || EQ (keysym, Qbutton5up) ||
      EQ (keysym, Qbutton6) || EQ (keysym, Qbutton6up) ||
      EQ (keysym, Qbutton7) || EQ (keysym, Qbutton7up))
    error ("Mouse-clicks can't appear in saved keyboard macros.");

  XEVENT (event)->channel = Qnil;
  XEVENT (event)->event_type = key_press_event;
  XEVENT (event)->event.key.key = keysym;
  XEVENT (event)->event.key.modifiers = modifiers;
}


static int
meta_prefix_char_p (keysym, modifiers)
     Lisp_Object keysym;
     int modifiers;
{
  struct Lisp_Event event;
  if (meta_prefix_char < 0) return 0;
  event.event_type = key_press_event;
  event.event.key.key = keysym;
  event.event.key.modifiers = modifiers;
  return (meta_prefix_char == event_to_character (&event, 0, 0, 0));
}


/* ASCII grunge.
   Given a keysym, return another keysym/modifier pair which could be 
   considered the same key in an ASCII world.  Backspace returns ^H, for 
   example.
 */
static void
define_key_alternate_name (keysym, modifiers, keysym_ret, modifiers_ret)
     Lisp_Object keysym, *keysym_ret;
     int modifiers, *modifiers_ret;
{
  int modifiers_sans_control = (modifiers & (~MOD_CONTROL));
  int modifiers_sans_meta = (modifiers & (~MOD_META));
  *keysym_ret = Qnil;
  *modifiers_ret = 0;
  if (modifiers_sans_meta == MOD_CONTROL)
    {
      if EQ (keysym, QKspace)
        *keysym_ret = make_number ('@'), *modifiers_ret = modifiers;
      else if (FIXNUMP (keysym))
	{
	  int k = XINT (keysym);
	  if (k == '@')
	    *keysym_ret = QKspace,   *modifiers_ret = modifiers;
	  else if (k == 'h')
	    *keysym_ret = QKbackspace, *modifiers_ret = modifiers_sans_control;
	  else if (k == 'i')
	    *keysym_ret = QKtab,       *modifiers_ret = modifiers_sans_control;
	  else if (k == 'j')
	    *keysym_ret = QKlinefeed,  *modifiers_ret = modifiers_sans_control;
	  else if (k == 'm')
	    *keysym_ret = QKreturn,    *modifiers_ret = modifiers_sans_control;
	  else if (k == '[')
	    *keysym_ret = QKescape,    *modifiers_ret = modifiers_sans_control;
	}
    }
  else if (modifiers_sans_meta)
    ;
  else if (EQ (keysym, QKbackspace))
    *keysym_ret = make_number ('h'), *modifiers_ret = modifiers | MOD_CONTROL;
  else if (EQ (keysym, QKtab))
    *keysym_ret = make_number ('i'), *modifiers_ret = modifiers | MOD_CONTROL;
  else if (EQ (keysym, QKlinefeed))
    *keysym_ret = make_number ('j'), *modifiers_ret = modifiers | MOD_CONTROL;
  else if (EQ (keysym, QKreturn))
    *keysym_ret = make_number ('m'), *modifiers_ret = modifiers | MOD_CONTROL;
  else if (EQ (keysym, QKescape))
    *keysym_ret = make_number ('['), *modifiers_ret = modifiers | MOD_CONTROL;
}


static void
ensure_meta_prefix_char_keymapp (keys, index, keymap)
     Lisp_Object keys, keymap;
     int index;
{
  char buf [255];
  Lisp_Object new_keys;
  Lisp_Object meta_sym;
  int meta_mods, i;
  Lisp_Object mpc_binding;
  define_key_parser (make_number (meta_prefix_char), &meta_sym, &meta_mods);
  mpc_binding = keymap_lookup_1 (keymap, meta_sym, meta_mods);
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
    sprintf (buf, "can't bind %s: %s has a non-keymap binding",
	(char *) XSTRING (Fkey_description (keys))->data,
	(char *) XSTRING (Fsingle_key_description 
                          (make_number (meta_prefix_char)))->data);
  else
    sprintf (buf, "can't bind %s: %s %s has a non-keymap binding",
	(char *) XSTRING (Fkey_description (keys))->data,
	(char *) XSTRING (Fkey_description (new_keys))->data,
	(char *) XSTRING (Fsingle_key_description
                          (make_number (meta_prefix_char)))->data);
  signal_error (Qerror, list2 (build_string (buf), mpc_binding));
}


static Lisp_Object
make_key_description (keysym, modifiers, prettify)
     Lisp_Object keysym;
     int modifiers;
     int prettify;
{
  Lisp_Object result;
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
  if (! modifiers) return keysym;
  result = Fcons (keysym, Qnil);
  if (modifiers & MOD_SHIFT) result = Fcons (Qshift, result);
  if (modifiers & MOD_SYMBOL) result = Fcons (Qsymbol, result);
  if (modifiers & MOD_HYPER) result = Fcons (Qhyper, result);
  if (modifiers & MOD_SUPER) result = Fcons (Qsuper, result);
  if (modifiers & MOD_CONTROL) result = Fcons (Qcontrol, result);
  if (modifiers & MOD_META)  result = Fcons (Qmeta, result);
  return result;
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
    size = XVECTOR (keys)->size;
  else if (STRINGP (keys))
    size = XSTRING (keys)->size;
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
      Lisp_Object keysym, keysym2;
      int modifiers, modifiers2;

      if (STRINGP (keys))
	c = make_number (XSTRING (keys)->data [idx]);
      else
	{
	  c = XVECTOR (keys)->contents [idx];
	  if (FIXNUMP (c) &&
	      (XINT (c) < ' ' || XINT (c) > 127))
	    args_out_of_range_3 (c, make_number (32), make_number (127));
	}

      define_key_parser (c, &keysym, &modifiers);

      if (!metized
	  && meta_prefix_char >= 0
	  && (XINT (c) == meta_prefix_char
              || meta_prefix_char_p (keysym, modifiers)))
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
              struct gcpro gcpro1, gcpro2;
              GCPRO2 (keysym, c);
              mmap = keymap_lookup_1 (keymap, Qmeta, 0);
	      if (!NILP (mmap)
		  && keymap_fullness (mmap) != 0)
		{
                  Lisp_Object desc
                    = Fsingle_key_description (make_number (meta_prefix_char));
		  signal_error (Qerror, list3 (build_string
				  ("Map contains meta-bindings, can't bind"),
					       desc, keymap));
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
	define_key_alternate_name (keysym, modifiers, &keysym2, &modifiers2);
      else
	keysym2 = Qnil;
      
      if (metized) {
	modifiers  |= MOD_META;
	modifiers2 |= MOD_META;
	metized = 0;
      }

      /* This crap is to make sure that someone doesn't bind something like
	 "C-x M-a" while "C-x ESC" has a non-keymap binding. */
      if ((modifiers & MOD_META) && meta_prefix_char >= 0)
	ensure_meta_prefix_char_keymapp (keys, idx, keymap);

      if (++idx == size)
	{
	  keymap_store (keymap, keysym, modifiers, def);
	  if (ascii_hack && !NILP (keysym2))
	    keymap_store (keymap, keysym2, modifiers2, def);
	  UNGCPRO;
	  return def;
	}
      
      {
        Lisp_Object cmd;
        struct gcpro gcpro1, gcpro2, gcpro3;
        GCPRO3 (keysym, keysym2, c);

        cmd = keymap_lookup_1 (keymap, keysym, modifiers);
	if (NILP (cmd))
	  {
	    cmd = Fmake_sparse_keymap ();
	    XKEYMAP (cmd)->name =
	      list2 (make_key_description (keysym, modifiers, 1), keymap);
	    keymap_store (keymap, keysym, modifiers, cmd);
	  }
	if (NILP (Fkeymapp (cmd)))
          signal_error (Qerror,
                        list3 (build_string
                               ("invalid prefix keys in sequence"),
                               c, keys));

	if (ascii_hack && !NILP (keysym2) &&
	    NILP (keymap_lookup_1 (keymap, keysym2, modifiers2)))
	  keymap_store (keymap, keysym2, modifiers2, cmd);

	keymap = get_keymap (cmd, 1, 1);
        UNGCPRO;
      }
    }
}


/* We need a very fast (i.e., non-consing) version of lookup-key in order 
   to make where-is-internal really fly.
 */

struct raw_key {
  unsigned int bits;
  Lisp_Object keysym;
};


/* Caller should gc-protect args */
static Lisp_Object
raw_lookup_key (Lisp_Object keymap,
                const struct raw_key *raw_keys, int raw_keys_count,
                int keys_so_far)
{
  Lisp_Object k = keymap;
  const Lisp_Object keysym = raw_keys[0].keysym;
  const int modifiers = raw_keys[0].bits;
  const int remaining = raw_keys_count - 1;

  /* Loop over keymap and parents */
  while (1)
    {
      /* Do depth-first search */
      Lisp_Object cmd;
      int parental_unit;
      QUIT;
      k = get_keymap (k, 1, 1);
      parental_unit = (! NILP (XKEYMAP (k)->parent));
      
      if (meta_prefix_char < 0 || ! meta_prefix_char_p (keysym, modifiers))
	{
	  /* Normal case: every case except the meta-hack (see below). */
	  cmd = keymap_lookup_1 (k, keysym, modifiers);
	  
	  if (remaining == 0)
	    /* Return whatever we found if we're out of keys */
	    ;
	  else if (NILP (cmd))
	    /* Found nothing (though perhaps parent map may have binding) */
	    ;
	  else if (NILP (Fkeymapp (cmd)))
	    /* Didn't find a keymap, and we have more keys.
	     * Return a fixnum to indicate that keys were too long.
	     */
	    cmd = make_number (keys_so_far + 1);
#if 0
	  else if (parental_unit)
	    /* Durst hope for tail-recursion? */
	    return (raw_lookup_key (cmd, raw_keys + 1, remaining, 
				    keys_so_far + 1));
#endif
	  else
	    cmd = raw_lookup_key (cmd, raw_keys + 1, remaining, 
				  keys_so_far + 1);
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
	      cmd = keymap_lookup_1 (k, keysym, modifiers);
	      if (NILP (cmd))
		/* Do kludgy return of the meta-map */ 
		cmd = keymap_lookup_1 (k, Qmeta, 0);
	    }
	  else
	    {
	      /* Search for the prefix-char-prefixed sequence directly */
	      cmd = keymap_lookup_1 (k, keysym, modifiers);
	      cmd = get_keymap (cmd, 0, 1);
	      if (!NILP (cmd))
		cmd = raw_lookup_key (cmd, raw_keys + 1, remaining, 
				      keys_so_far + 1);
	      else if ((raw_keys[1].bits & MOD_META) == 0)
		{
		  /* Search for meta-next-char sequence directly */
		  cmd = keymap_lookup_1 (k, 
					 raw_keys[1].keysym, 
					 raw_keys[1].bits | MOD_META);
		  if (remaining == 1)
		    ;
		  else
		    {
		      cmd = get_keymap (cmd, 0, 1);
		      if (!NILP (cmd))
			cmd = raw_lookup_key (cmd, raw_keys + 2, remaining - 1,
					      keys_so_far + 2);
		    }
		}
	    }
	}
      if (!NILP (cmd))
	return (cmd);
      if (!parental_unit)
	return (Qnil);
      k = XKEYMAP (k)->parent;
    }
}


/* Value is number if `keys' is too long; NIL if valid but has no definition.*/
/* Caller should gc-protect arguments */
Lisp_Object
lookup_keys (Lisp_Object keymap, int nkeys, Lisp_Object *keys)
{
  struct raw_key kkk[20];
  struct raw_key *raw_keys;
  int i;

  if (nkeys == 0)
    return Qnil;

  if (nkeys > (countof (kkk)))
    raw_keys = kkk;
  else
    raw_keys = (struct raw_key *) alloca (sizeof (struct raw_key) * nkeys);

  for (i = 0; i < nkeys; i++)
    {
      Lisp_Object c = keys[i];
      Lisp_Object keysym;
      int modifiers;

      define_key_parser (c, &keysym, &modifiers);
      raw_keys[i].keysym = keysym;
      raw_keys[i].bits = modifiers;
    }
  return (raw_lookup_key (keymap, raw_keys, nkeys, 0));
}

Lisp_Object
lookup_events (Lisp_Object event_head, int nmaps, Lisp_Object keymaps[])
{
  struct raw_key kkk[20];

  int nkeys;
  struct raw_key *raw_keys;
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
    raw_keys = (struct raw_key *) alloca (sizeof (struct raw_key) * nkeys);

  for (e = XEVENT (event_head), nkeys = 0; e; e = event_next (e), nkeys++)
    {
      Lisp_Object c;
      Lisp_Object keysym;
      int modifiers;

      XSETR (c, Lisp_Event, e);
      define_key_parser (c, &keysym, &modifiers);
      raw_keys[nkeys].keysym = keysym;
      raw_keys[nkeys].bits = modifiers;
    }
  GCPRO2 (keymaps[0], event_head);
  gcpro1.nvars = nmaps;
  /* >>>raw_keys[].keysym slots aren't gc-protected.  We rely (but shouldn't)
   * on somebody else somewhere (obarray) having a pointer to all keysyms. */
  for (iii = 0; iii < nmaps; iii++)
    {
      tem = raw_lookup_key (keymaps[iii], raw_keys, nkeys, 0);
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

DEFUN ("lookup-key", Flookup_key, Slookup_key, 2, 2, 0,
  "In keymap KEYMAP, look up key sequence KEYS.  Return the definition.\n\
nil means undefined.  See doc of `define-key' for kinds of definitions\n\
and key-sequence specifications.\n\
Number as value means KEYS is \"too long\";\n\
that is, characters in it except for the last one\n\
fail to be a valid sequence of prefix characters in KEYMAP.\n\
The number is how many characters at the front of KEYS\n\
it takes to reach a non-prefix command.")
  (keymap, keys)
     register Lisp_Object keymap;
     Lisp_Object keys;
{
  if (VECTORP (keys))
    {
      return lookup_keys (keymap,
			  XVECTOR (keys)->size, XVECTOR (keys)->contents);
    }
  else if (SYMBOLP (keys) || FIXNUMP (keys))
    {
      return lookup_keys (keymap, 1, &keys);
    }
  else if (!STRINGP (keys))
    {
      keys = wrong_type_argument (Qsequencep, keys);
      return Flookup_key (keymap, keys);
    }
  else
    {
      int length = XSTRING (keys)->size;
      int i;
      struct raw_key *raw_keys
	= (struct raw_key *) alloca (sizeof (struct raw_key) * length);
      if (length == 0)
	return Qnil;

      for (i = 0; i < length; i++)
	{
	  Lisp_Object c;
	  Lisp_Object keysym;
	  int modifiers;

	  c = make_number ((unsigned char) XSTRING (keys)->data[i]);
	  define_key_parser (c, &keysym, &modifiers);
	  raw_keys[i].keysym = keysym;
	  raw_keys[i].bits = modifiers;
	}
      return (raw_lookup_key (keymap, raw_keys, length, 0));
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

static int
relevant_keymaps_to_search (Lisp_Object keys,
                            int max_maps, Lisp_Object maps[])
{
#define push_map_or_punt(m) \
  do { if (nmaps == max_maps) return (-1); \
       else if (!NILP ((m))) maps[nmaps++] = (m); } while (0)

  Lisp_Object terminal = Qnil;
  int nmaps = 0;

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
      int len = XVECTOR (keys)->size;
      if (len > 1)
	terminal = XVECTOR (keys)->contents[len - 1];
    }

  if (!EVENTP (terminal)
      || (XEVENT (terminal)->event_type != button_press_event 
          && XEVENT (terminal)->event_type != button_release_event))
    {
      /* It's not a mouse event, so use the local map of the current buffer. */
      Lisp_Object local_map = current_buffer->keymap;
      push_map_or_punt (local_map);
    }
  else
    {
      /* If it's a mouse event, then first check the mouse-grabbed-buffer.
	 After that, the interesting buffer is the buffer of the window under
	 the mouse.  If this was a mode-line hit instead of a text-hit, then
	 check the mode-line-map of that buffer.  Then, check the local map.
       */
      Lisp_Object window, screen;
      int mode_p;

      if (BUFFERP (Vmouse_grabbed_buffer))
	{
	  Lisp_Object local_map = XBUFFER (Vmouse_grabbed_buffer)->keymap;
	  push_map_or_punt (local_map);
	}

      window = Fevent_window (terminal);
      screen = Fevent_screen (terminal);
      mode_p = 0;

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
	      Lisp_Object local_map;
	      if (mode_p)
		{
		  Lisp_Object mode_line_map
                    = symbol_value_in_buffer (Qmode_line_map, buffer);
		  if (!EQ (mode_line_map, Qunbound))
		    push_map_or_punt (mode_line_map);
		}
	      local_map = XBUFFER (buffer)->keymap;
	      if (!EQ (buffer, Vmouse_grabbed_buffer))
		push_map_or_punt (local_map);
	    }
	}
    }
  push_map_or_punt (Vcurrent_global_map);
  return (nmaps);
#undef push_map_or_punt
}


/* Attempts to find a command corresponding to the event-sequence
   whose head is event0 (sequence is threaded though event_next).
   Returns either a command symbol or Qnil.
 */

Lisp_Object
event_binding (Lisp_Object event0)
{
  Lisp_Object maps[10];
  int nmaps;

  if (!EVENTP (event0)) abort ();

  nmaps = relevant_keymaps_to_search (event0, countof (maps), maps);
  if (nmaps < 0) return (Qnil); /* >>> Urk! */
  return (lookup_events (event0, nmaps, maps));
}

DEFUN ("key-binding", Fkey_binding, Skey_binding, 1, 1, 0,
  "Return the binding for command KEYS in current keymaps.\n\
KEYS is a string, a vector of events, or a vector of key-description lists\n\
as described in the documentation for the `define-key' function.\n\
The binding is probably a symbol with a function definition; see\n\
the documentation for `lookup-key' for more information.")
  (keys)
    Lisp_Object keys;
{
  int i;
  Lisp_Object maps[10];
  int nmaps = relevant_keymaps_to_search (keys, countof (maps), maps);
  if (nmaps < 0) abort (); /* this can't happen, right? */

#if 0
  if (EVENTP (keys))           /* unadvertised "feature" for the future */
    return (lookup_events (keys, nmaps, maps));
#endif

  for (i = 0; i < nmaps; i++)
    {
      Lisp_Object tem = Flookup_key (maps[i], keys);
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
  void (*fn) (Lisp_Object key, int modifiers, 
              Lisp_Object binding, void *arg);
  void *arg;
  int bits;
};

/* used by map_keymap() */
static void
map_keymap_unsorted_mapper (const void *hash_key, void *hash_contents, 
                            void *map_keymap_unsorted_closure)
{
  Lisp_Object key, contents;
  struct map_keymap_unsorted_closure *closure
    = (struct map_keymap_unsorted_closure *) map_keymap_unsorted_closure;
  int modifiers = closure->bits;
  int mod_bit;
  CVOID_TO_LISP (key, hash_key);
  VOID_TO_LISP (contents, hash_contents);
  mod_bit = bucky_sym_to_bucky_bit (key);
  if (mod_bit)
    {
      int omod = modifiers;
      closure->bits = modifiers | mod_bit;
      contents = get_keymap (contents, 1, 1);
      elisp_maphash (map_keymap_unsorted_mapper,
		     XKEYMAP (contents)->table,
		     map_keymap_unsorted_closure);
      closure->bits = omod;
    }
  else
    (*closure->fn) (key, modifiers, contents, closure->arg);
}


struct map_keymap_sorted_closure
{
  Lisp_Object *result_locative;
};

/* used by map_keymap_sorted() */
static void
map_keymap_sorted_mapper (const void *hash_key, void *hash_contents, 
                          void *map_keymap_sorted_closure)
{
  Lisp_Object key, contents;
  Lisp_Object *list = (((struct map_keymap_sorted_closure *)
                        map_keymap_sorted_closure)->result_locative);
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
  int bit1, bit2;
  int sym1_p = 0, sym2_p = 0;
  obj1 = XCONS (obj1)->car;
  obj2 = XCONS (obj2)->car;
  if (EQ (obj1, obj2)) return -1;

  bit1 = bucky_sym_to_bucky_bit (obj1);
  bit2 = bucky_sym_to_bucky_bit (obj2);
  
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

  if (FIXNUMP (obj1)) /* they're both ASCII */
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
  return ((0 > strcmp ((char *) XSYMBOL (obj1)->name->data,
		       (char *) XSYMBOL (obj2)->name->data))
	  ? 1 : -1);
}


/* used by map_keymap() */
static void
map_keymap_sorted (Lisp_Object keymap_table,
                   int bits, 
                   void (*function) (Lisp_Object key, int bits, 
                                     Lisp_Object binding, 
                                     void *map_keymap_sorted_closure),
                   void *map_keymap_sorted_closure)
{
  struct gcpro gcpro1, gcpro2, gcpro3;
  Lisp_Object key = Qnil;
  Lisp_Object binding = Qnil;
  Lisp_Object contents = Qnil;
  int sub_bit;

  if (XINT (Fhashtable_fullness (keymap_table)) == 0)
    return;

  GCPRO3 (key, binding, contents);

  {
    struct map_keymap_sorted_closure c1;
    c1.result_locative = &contents;
    elisp_maphash (map_keymap_sorted_mapper, keymap_table, &c1);
  }
  contents = list_sort (contents, Qnil, map_keymap_sort_predicate);
  for (; !NILP (contents); contents = XCONS (contents)->cdr)
    {
      key = XCONS (XCONS (contents)->car)->car;
      binding = XCONS (XCONS (contents)->car)->cdr;
      sub_bit = bucky_sym_to_bucky_bit (key);
      if (sub_bit)
	map_keymap_sorted (XKEYMAP (get_keymap (binding, 1, 1))->table,
			   (bits | sub_bit),
			   function,
			   map_keymap_sorted_closure);
      else
	(*function) (key, bits, binding, map_keymap_sorted_closure);
    }
  UNGCPRO;
}


/* used by Fmap_keymap() */
static void
map_keymap_mapper (Lisp_Object key, int bits, Lisp_Object binding, 
                   void *function)
{
  Lisp_Object fn;
  VOID_TO_LISP (fn, function);
  call2 (fn, make_key_description (key, bits, 1), binding);
}


static void
map_keymap (Lisp_Object keymap_table, int sort_first,
            void (*function) (Lisp_Object key, int modifiers,
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
      map_keymap_unsorted_closure.bits = 0;
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
The function will not be called on elements of this keymap's parent (see the\n\
function `keymap-parent') or upon keymaps which are contained within this\n\
keymap (multi-character definitions).  It will be called on \"meta\"\n\
characters, however, since they are not really two-character sequences.\n\
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



static int
bucky_sym_to_bucky_bit (sym)
     Lisp_Object sym;
{
  if (EQ (sym, Qcontrol)) return MOD_CONTROL;
  else if (EQ (sym, Qmeta)) return MOD_META;
  else if (EQ (sym, Qsuper)) return MOD_SUPER;
  else if (EQ (sym, Qhyper)) return MOD_HYPER;
  else if (EQ (sym, Qsymbol)) return MOD_SYMBOL;
  else if (EQ (sym, Qshift)) return MOD_SHIFT;
  else return 0;
}

static void
accessible_keymaps_mapper (Lisp_Object key, Lisp_Object contents,
                           int modifiers,
                           Lisp_Object accessible_keymaps_tail)
{
  int bit = bucky_sym_to_bucky_bit (key);

  if (bit)
    {
      Lisp_Object submaps;
      contents = get_keymap (contents, 1, 1);
      submaps = keymap_submaps (contents);
      for (; !NILP (submaps); submaps = XCONS (submaps)->cdr)
	{
	  accessible_keymaps_mapper (XCONS (XCONS (submaps)->car)->car,
                                 XCONS (XCONS (submaps)->car)->cdr,
                                 (bit | modifiers),
                                 accessible_keymaps_tail);
	}
    }
  else
    {
      Lisp_Object thisseq;
      Lisp_Object cmd = get_keyelt (contents);
      Lisp_Object vec;
      int j;

      if (NILP (cmd))
	abort ();
      if (NILP (Fkeymapp (cmd)))
	abort ();
      cmd = get_keymap (cmd, 1, 1);
      thisseq = Fcar (Fcar (accessible_keymaps_tail));

      vec = make_vector (XVECTOR (thisseq)->size + 1, Qnil);
      for (j = 0; j < XVECTOR (thisseq)->size; j++)
	XVECTOR (vec)->contents [j] = XVECTOR (thisseq)->contents [j];
      XVECTOR (vec)->contents [j] = make_key_description (key, modifiers, 1);

      nconc2 (accessible_keymaps_tail, 
	      list1 (Fcons (vec, cmd)));
    }
}


DEFUN ("accessible-keymaps", Faccessible_keymaps, Saccessible_keymaps,
  1, 1, 0,
  "Find all keymaps accessible via prefix characters from KEYMAP.\n\
Returns a list of elements of the form (KEYS . MAP), where the sequence\n\
KEYS starting from KEYMAP gets you to MAP.  These elements are ordered\n\
so that the KEYS increase in length.  The first element is ([] . KEYMAP).")
  (startmap)
     Lisp_Object startmap;
{
  Lisp_Object accessible_keymaps = list1 (Fcons (make_vector (0, Qnil),
						 get_keymap (startmap, 1, 1)));
  Lisp_Object tail;
  struct gcpro gcpro1;
  GCPRO1 (accessible_keymaps);
  
  /* For each map in the list maps,
     look at any other maps it points to
     and stick them at the end if they are not already in the list */

  for (tail = accessible_keymaps; !NILP (tail); tail = XCONS (tail)->cdr)
    {
      Lisp_Object thismap = Fcdr (Fcar (tail));
      CHECK_KEYMAP (thismap, 0);
      for (; !NILP (thismap); thismap = XKEYMAP (thismap)->parent)
	{
	  Lisp_Object submaps = keymap_submaps (thismap);
	  for (; !NILP (submaps); submaps = XCONS (submaps)->cdr)
	    {
	      accessible_keymaps_mapper (XCONS (XCONS (submaps)->car)->car,
					 XCONS (XCONS (submaps)->car)->cdr,
					 0,
					 tail);
	    }
	}
    }
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
      Lisp_Object s2;
      int i;
      int size = XINT (Flength (keys));
      for (i = 0; i < size; i++)
	{
	  s2 = Fsingle_key_description
	    (((STRINGP (keys))
	      ? make_number ((unsigned char) XSTRING (keys)->data[i])
	      : XVECTOR (keys)->contents[i]));

	  if (NILP (string))
	    string = s2;
	  else
	    {
	      /* if (NILP (sep)) Lisp_Object sep = build_string (" ") */;
	      string = concat2 (string, concat2 (Vsingle_space_string, s2));
	    }
	}
      return string;
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
		signal_error (Qerror,
			      list2 (build_string ("invalid key description"),
				     key));
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
  char buf[20];
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
		   list2 (build_string ("character has no ASCII equivalent"),
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
{
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
    {
      Lisp_Object found[2];
      found[0] = found0;
      found[1] = found1;
      return Fnconc (2, found);
    }
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
raw_keys_to_keys (struct raw_key *keys, int count)
{
  Lisp_Object result = make_vector (count, Qnil);
  while (count--)
    XVECTOR (result)->contents [count] =
      make_key_description (keys[count].keysym, keys[count].bits, 1);
  return result;
}


static void
format_raw_keys (struct raw_key *keys, int count, char *buf)
{
  int i;
  struct Lisp_Event event;
  event.event_type = key_press_event;
  for (i = 0; i < count; i++)
    {
      event.event.key.key = keys[i].keysym;
      event.event.key.modifiers = keys[i].bits;
      format_event_object (buf, &event, 1);
      buf += strlen (buf);
      if (i < count-1)
	buf[0] = ' ', buf++;
    }
}


struct keys_so_far
{
  int total_size;
  struct raw_key *raw_keys;
  int malloced;
};


static Lisp_Object
where_is_recursive_1 (Lisp_Object definition, Lisp_Object map,
                      Lisp_Object shadow, int firstonly,
                      int keys_count, int bits_so_far,
                      char *target_buffer,
                      struct keys_so_far *keys_so_far)

  /* definition is the thing to look for.
     map is a keymap.
     shadow is a keymap or nil; if it is a keymap, and there is different
	binding in it of a key that we are considering returning, then we
	reconsider.
     firstonly means give up after finding the first match;
     keys_so_far and bits_so_far describe which map we're looking in;
	If we're in the "meta" submap of the map that "C-x 4" is bound to,
	then keys_so_far will be {(control x), \4}, and bits_so_far will
	be MOD_META.  That is, keys_so_far is the chain of keys that we have
	followed, and bits_so_far is the bits (partial keys) beyond that.

     (keys_so_far is a global buffer and the keys_count arg says how much
     of it we're currently interested in.)

     If target_buffer is provided, then we write a key-description into it,
        to avoid consing a string.  This only works with firstonly on.
   */
{
  Lisp_Object result = Qnil;

  if (keys_count >= keys_so_far->total_size)
    {
      int size = keys_count + 50;
      keys_so_far->total_size = size;
      if (! keys_so_far->malloced)
        keys_so_far->raw_keys
          = (struct raw_key *) xmalloc (size * sizeof (struct raw_key));
      else
        keys_so_far->raw_keys
          = (struct raw_key *) xrealloc (keys_so_far->raw_keys,
                                         size * sizeof (struct raw_key));
      keys_so_far->malloced = 1;
    }

  for (map = get_keymap (map, 1, 1);
       !NILP (map);
       map = get_keymap (XKEYMAP (map)->parent, 0, 1))
    {
      Lisp_Object keys = Fgethash (definition,
				   XKEYMAP (map)->inverse_table,
				   Qnil);
      Lisp_Object submaps;

      if (!NILP (keys))
	{
	  /* Verify that this key binding is not shadowed by another binding
	     for the same key, before we say it exists.  The mechanism: look
	     for a local definition of this key and if it is defined and does
	     not match what we found then ignore this key.  Either nil or
	     number as value from raw_lookup_key() means undefined.
	   */
	  Lisp_Object shadowed = Qnil;
          struct raw_key *so_far = keys_so_far->raw_keys;

          for (;;)
            {
              Lisp_Object k = ((CONSP (keys)) ? XCONS (keys)->car : keys);

	      so_far [keys_count].keysym = k;
	      so_far [keys_count].bits = bits_so_far;
	      if (!NILP (shadow))
		shadowed = raw_lookup_key (shadow,
					   so_far,
					   keys_count + 1, 
					   0);
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
		    result =
		      Fcons (raw_keys_to_keys (so_far, keys_count + 1),
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
	  Lisp_Object lower;
	  int lower_bits;
	  int lower_keys_count = keys_count;
	  int bucky;

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

	  /* If the map is a "bucky" map, then add a bit to the bits_so_far
	     list.  Otherwise, add a new raw_key onto the end of keys_so_far.
	   */
	  bucky = bucky_sym_to_bucky_bit (key);
	  if (bucky)
	    lower_bits = bits_so_far | bucky;
	  else
	    {
	      struct raw_key *so_far = keys_so_far->raw_keys;
	      lower_bits = 0;
	      so_far [lower_keys_count].keysym = key;
	      so_far [lower_keys_count].bits = bits_so_far;
	      lower_keys_count++;
	    }
	  lower = where_is_recursive_1 (definition, submap, shadow, firstonly,
				      lower_keys_count, lower_bits,
				      target_buffer, keys_so_far);
	  if (!firstonly)
	    result = nconc2 (lower, result);
	  else if (!NILP (lower))
	    return lower;
	}
    }
  if (! (keys_count || bits_so_far))
    /* ...meaning we are the outermost call */
    return Fnreverse (result);
  else
    return (result);
}


static Lisp_Object
where_is_internal (Lisp_Object definition, Lisp_Object map,
                   Lisp_Object shadow, Lisp_Object firstonly,
                   char *target_buffer)
{
  Lisp_Object tem;
  struct raw_key raw[20];
  struct keys_so_far junk;
  junk.raw_keys = raw;
  junk.total_size = countof (raw);
  junk.malloced = 0;

  tem = where_is_recursive_1 (definition, map,
                              shadow, !NILP (firstonly),
                              0, 0,
                              target_buffer,
                              &junk);
  if (junk.malloced)
    xfree (junk.raw_keys);
  return (tem);
}


DEFUN ("describe-bindings", Fdescribe_bindings, Sdescribe_bindings, 0, 1, "P",
  "Show a list of all defined keys, and their definitions.\n\
The list is put in a buffer, which is displayed.\n\
If the argument is non-null, then only the mouse bindings are displayed.")
  (mice_only_p)
  Lisp_Object mice_only_p;
{
  internal_with_output_to_temp_buffer ("*Help*",
				       NILP (mice_only_p)
				       ? describe_buffer_bindings
				       : describe_buffer_mouse_bindings,
				       Fcurrent_buffer (), Qnil);
  return Qnil;
}


static Lisp_Object
describe_buffer_bindings_1 (Lisp_Object descbuf, int mice_only_p)
{
  const char *heading =
    (mice_only_p
     ? "button          binding\n------          -------\n"
     : "key             binding\n---             -------\n");
  struct gcpro gcpro1;

  GCPRO1 (descbuf);
  Fset_buffer (Vstandard_output);

  if (!NILP (XBUFFER (descbuf)->keymap))
    {
      insert_string ("Local Bindings:\n");
      insert_string (heading);
      describe_map_tree (XBUFFER (descbuf)->keymap, 0, Qnil,
                         mice_only_p);
      insert_string ("\n");
    }

  insert_string ("Global Bindings:\n");
  insert_string (heading);

  describe_map_tree (Vcurrent_global_map, 0, XBUFFER (descbuf)->keymap,
		     mice_only_p);

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
   If SHADOW is non-nil, it is another map;
    don't mention keys which would be shadowed by it */

void
describe_map_tree (Lisp_Object startmap, int partial, Lisp_Object shadow,
                   int mice_only_p)
{
  Lisp_Object maps, elt, sh;
  struct gcpro gcpro1;

  maps = Faccessible_keymaps (startmap);

  GCPRO1 (maps);

  for (; !NILP (maps); maps = Fcdr (maps))
    {
      elt = Fcar (maps);
      sh = Fcar (elt);
      if (NILP (shadow))
	sh = Qnil;
      else if (VECTORP (sh)
	       && XVECTOR (sh)->size == 0)
	sh = shadow;
      else
	{
	  sh = Flookup_key (shadow, Fcar (elt));
	  if (FIXNUMP (sh))
	    sh = Qnil;
	}
      if (NILP (sh) || !NILP (Fkeymapp (sh)))
	describe_map (Fcdr (elt), Fcar (elt), partial,
		      (NILP (sh) ? Qnil : get_keymap (sh, 1, 1)),
		      mice_only_p);
    }
  UNGCPRO;
}


static void
describe_command (Lisp_Object definition)
{
  struct gcpro gcpro1;
  GCPRO1 (definition);

  Findent_to (make_number (16), make_number (1));
  if (SYMBOLP (definition))
    {
      insert1 (Fsymbol_name (definition));
    }
  else if (STRINGP (definition) || VECTORP (definition))
    {
      insert_string ("Kbd Macro: ");
      insert1 (Fkey_description (definition));
    }
  else if (COMPILEDP (definition))
    insert_string ("Anonymous Compiled Function");
  else if (CONSP (definition) && EQ (XCONS (definition)->car, Qlambda))
    insert_string ("Anonymous Lambda");
  else if (KEYMAPP (definition))
    {
      Lisp_Object name = XKEYMAP (definition)->name;
      if (!NILP (name) 
	  && (SYMBOLP (name) || STRINGP (name)) /* >>>??? */
	  )
	{
	  insert_string ("Prefix Command ");
	  if (SYMBOLP (name) 
	      && EQ (find_symbol_value (name), definition))
	    insert1 (name);
	  else
	    {
	      insert1 (Fprin1_to_string (name, Qnil));
	    }
	}
      else
	insert_string ("Prefix Command");
    }
  else
    insert_string ("??");

  insert_string ("\n");
  UNGCPRO;
}

/* Describe the contents of map MAP, assuming that this map
   itself is reached by the sequence of prefix keys KEYS (a vector).
   PARTIAL and SHADOW are as in `describe_map_tree' above.  */

static void
describe_map (Lisp_Object map, Lisp_Object keys, 
              int partial, Lisp_Object shadow, int mice_only_p)
{
  Lisp_Object keysdesc;
  
  if (!NILP (keys) && XINT (Flength (keys)) > 0)
    keysdesc = concat2 (Fkey_description (keys), Vsingle_space_string);
  else
    keysdesc = Qnil;
  describe_map_1 (map, keysdesc, describe_command,
		  partial, shadow, mice_only_p);
}

struct describe_map_closure {
  Lisp_Object *list;	 /* pointer to the list to update */
  Lisp_Object partial;	 /* whether to ignore suppressed commands */
  Lisp_Object shadow;	 /* the map that shadows this one */
  Lisp_Object self;	 /* this map */
  Lisp_Object self_root; /* this map, or some map that has this map as
			    a parent.  this is the base of the tree */
  int mice_only_p;	 /* whether we are to display only button bindings */
};

static void
describe_map_mapper (Lisp_Object key, int bits, Lisp_Object binding,
		     void *describe_map_closure)
{
  Lisp_Object sh;
  struct describe_map_closure *closure =
    (struct describe_map_closure *) describe_map_closure;

  /* Dont mention suppressed commands.  */
  if (SYMBOLP (binding)
      && !NILP (closure->partial)
      && !NILP (Fget (binding, closure->partial, Qnil)))
    return;
	      
  /* If we're only supposed to display mouse bindings and this isn't one,
     then bug out. */
  if (closure->mice_only_p &&
      (! (EQ (key, Qbutton0) || EQ (key, Qbutton1) || EQ (key, Qbutton2) ||
	  EQ (key, Qbutton3) || EQ (key, Qbutton4) || EQ (key, Qbutton5) ||
	  EQ (key, Qbutton6) || EQ (key, Qbutton7))))
    return;

  /* If this command in this map is shadowed by some other map, ignore it. */
  if (!NILP (closure->shadow))
    {
      for (sh = closure->shadow;
	   !NILP (Fkeymapp (sh));
	   sh = XKEYMAP (sh)->parent)
	{
	  if (!NILP (keymap_lookup_directly (sh, key, bits)))
	    return;
	}
    }
  /* If this key is in some map of which this map is a parent, then ignore
     it (in that case, it has been shadowed.)
   */
  for (sh = closure->self_root;
       !EQ (sh, closure->self);
       sh = XKEYMAP (sh)->parent)
    {
      if (!NILP (keymap_lookup_directly (sh, key, bits)))
	return;
    }

  /* Otherwise add it to the list to be sorted. */
  *(closure->list) = Fcons (Fcons (Fcons (key, make_number (bits)), binding),
			    *(closure->list));
}


static int
describe_map_sort_predicate (Lisp_Object obj1, Lisp_Object obj2, 
			     Lisp_Object pred)
{
  /* obj1 and obj2 are conses of the form
     ( ( <keysym> . <bits> ) . <binding> )
     keysym and bits are used, binding is ignored.
   */
  int bit1, bit2;
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


static void
describe_map_1 (Lisp_Object keymap,
		Lisp_Object elt_prefix,
		void (*elt_describer) (Lisp_Object),
		int partial, 
		Lisp_Object shadow,
		int mice_only_p)
{
  struct describe_map_closure describe_map_closure;
  Lisp_Object list = Qnil;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
  int printable_min = (FIXNUMP (current_buffer->ctl_arrow)
		       ? XINT (current_buffer->ctl_arrow)
		       : ((EQ (current_buffer->ctl_arrow, Qt) ||
			   EQ (current_buffer->ctl_arrow, Qnil))
			  ? 256 : 160));
  int elided = 0;

  keymap = get_keymap (keymap, 1, 1);
  describe_map_closure.partial = (partial ? Qsuppress_keymap : Qnil);
  describe_map_closure.shadow = shadow;
  describe_map_closure.list = &list;
  describe_map_closure.self_root = keymap;
  describe_map_closure.mice_only_p = mice_only_p;

  GCPRO4 (keymap, elt_prefix, shadow, list);

  for (; !NILP (Fkeymapp (keymap)); keymap = XKEYMAP (keymap)->parent)
    {
      describe_map_closure.self = get_keymap (keymap, 1, 1);
      map_keymap (XKEYMAP (describe_map_closure.self)->table,
		  0, /* don't sort: we'll do it later */
		  describe_map_mapper, &describe_map_closure);
    }

  list = list_sort (list, Qnil, describe_map_sort_predicate);

  insert_string ("\n");
  while (!NILP (list))
    {
      Lisp_Object keysym = XCONS (XCONS (XCONS (list)->car)->car)->car;
      int modifiers = XINT (XCONS (XCONS (XCONS (list)->car)->car)->cdr);

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
	  if (EQ (keysym, QKlinefeed))		insert_string ("LFD");
	  else if (EQ (keysym, QKtab))		insert_string ("TAB");
	  else if (EQ (keysym, QKreturn))	insert_string ("RET");
	  else if (EQ (keysym, QKescape))	insert_string ("ESC");
	  else if (EQ (keysym, QKdelete))	insert_string ("DEL");
	  else if (EQ (keysym, QKspace))	insert_string ("SPC");
	  else if (EQ (keysym, QKbackspace))	insert_string ("BS");
	  else if (c >= printable_min)		insert_char (c);
	  else
	    insert_raw_string ((char *) XSYMBOL (keysym)->name->data,
			       XSYMBOL (keysym)->name->size);
	}
      else if (FIXNUMP (keysym))
	insert_char (XINT (keysym));
      else
	insert_string ("---bad keysym---");

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

  defsubr (&Skeymap_parent);
  defsubr (&Sset_keymap_parent);
/*defsubr (&Skeymap_name); */
  defsubr (&Sset_keymap_name);
  defsubr (&Skeymap_prompt);
  defsubr (&Sset_keymap_prompt);

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
}

void
keys_of_keymap ()
{
  Lisp_Object ESC_prefix = intern ("ESC-prefix");
  Vcurrent_global_map = Fmake_keymap ();
  Ffset (ESC_prefix, Fmake_keymap ());
  keymap_store (Vcurrent_global_map, Qmeta, 0, ESC_prefix);
}
