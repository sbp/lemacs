/* Manipulation of keymaps
   Copyright (C) 1985, 1986, 1987, 1988, 1992 Free Software Foundation, Inc.

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

#include "config.h"
#include <stdio.h>

#include "lisp.h"
#include "keymap.h"
#include "commands.h"
#include "buffer.h"
#include "events.h"
#include "insdel.h"
#include "elhash.h"

extern Lisp_Object Vcharacter_set_property;

#define min(a, b) ((a) < (b) ? (a) : (b))

/* Actually allocate storage for these variables */

static Lisp_Object Vcurrent_global_map;	/* Current global keymap */

/* This is incremented whenever a change is made to a keymap.  This is
   so that things which care (such as the menubar code) can recompute
   privately-cached data when the user has changed their keybindings.
 */
int keymap_tick;

/* Prefixing a key with this character is the same as sending a meta bit. */
int meta_prefix_char;

Lisp_Object Qkeymapp;

void describe_map_tree ();
static Lisp_Object describe_buffer_bindings ();
static Lisp_Object describe_buffer_mouse_bindings ();
static void describe_command ();
static void describe_map ();
static int bucky_sym_to_bucky_bit ();
static int describe_vector ();

Lisp_Object Qcontrol, Qctrl, Qmeta, Qsuper, Qhyper, Qsymbol, Qshift;
Lisp_Object Qbutton0, Qbutton1, Qbutton2, Qbutton3, Qbutton4, Qbutton5,
  Qbutton6, Qbutton7;
Lisp_Object Qbutton0up, Qbutton1up, Qbutton2up, Qbutton3up, Qbutton4up,
  Qbutton5up, Qbutton6up, Qbutton7up;
Lisp_Object Qmenu_selection;



static void
keymap_store_inverse_internal (keysym, keymap, value)
     Lisp_Object keysym, value;
     struct Lisp_Keymap *keymap;
{
  Lisp_Object keys = Fgethash (value, keymap->inverse_table, Qnil);
  if (NILP (keys))
    {
      keys = Fcons (keysym, Qnil);
      Fputhash (value, keys, keymap->inverse_table);
    }
  else
    {
      Lisp_Object tail = keys;
      while (!NILP (XCONS (tail)->cdr))
	tail = XCONS (tail)->cdr;
      XCONS (tail)->cdr = Fcons (keysym, Qnil);
      /* We don't need to call puthash here, because we've modified the
	 list directly (the pointer in the hash table is still valid.)
       */
    }
}


static void
keymap_delete_inverse_internal (keysym, keymap, value)
     Lisp_Object keysym, value;
     struct Lisp_Keymap *keymap;
{
  Lisp_Object keys = Fgethash (value, keymap->inverse_table, Qnil);
  Lisp_Object new_keys;
  if (NILP (keys))
    abort ();
  new_keys = delq_no_quit (keysym, keys);
  if (NILP (new_keys))
    Fremhash (value, keymap->inverse_table);
  else if (!EQ (keys, new_keys))	/* meaning it was the first elt */
    Fputhash (value, new_keys, keymap->inverse_table);
  /* else the list's tail has been modified, so we don't need to
     touch the hash table again (the pointer in there is ok).
   */
}


static void
keymap_store_internal (keysym, keymap, value)
     Lisp_Object keysym, value;
     struct Lisp_Keymap *keymap;
{
  Lisp_Object prev_value = Fgethash (keysym, keymap->table, Qnil);
  if (!NILP (prev_value))
    keymap_delete_inverse_internal (keysym, keymap, prev_value);
  if (NILP (value))
    {
      if (!NILP (prev_value))
	{
	  if ((--keymap->fullness) < 0) abort ();
	  Fremhash (keysym, keymap->table);
	}
    }
  else
    {
      if (NILP (prev_value))
	keymap->fullness++;
      Fputhash (keysym, value, keymap->table);
      keymap_store_inverse_internal (keysym, keymap, value);
    }
  keymap_tick++;
}


static Lisp_Object
keymap_lookup (km, keysym, modifiers)
     Lisp_Object km, keysym;
     int modifiers;
{
  struct Lisp_Keymap *keymap = XKEYMAP (km);
  Lisp_Object submap, submap_name;
  int submap_bit;
  if (!KEYMAPP (km)) abort ();
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

  submap = keymap_lookup (km, submap_name, 0);
  if (NILP (submap))
    return Qnil;
  submap = get_keymap (submap);
  CHECK_KEYMAP (submap, 0);
  return keymap_lookup (submap, keysym, (modifiers & (~submap_bit)));
}


static void
keymap_store (keymap, keysym, modifiers, value)
     Lisp_Object keymap, keysym, value;
     int modifiers;
{
  Lisp_Object submap_name, submap;
  int submap_bit;
  if (!KEYMAPP (keymap))
    keymap = get_keymap (keymap);
  XKEYMAP (keymap)->sub_maps_cache = Qt;
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

  submap = keymap_lookup (keymap, submap_name, 0);
  if (NILP (submap))
    keymap_store (keymap, submap_name, 0, (submap = Fmake_sparse_keymap ()));
  keymap_store (submap, keysym, (modifiers & (~submap_bit)), value);
}



static void
keymap_submaps_mapper (hash_key, hash_contents, closure)
     void *hash_key, *hash_contents, *closure;
{
  Lisp_Object key = (Lisp_Object) hash_key;
  Lisp_Object contents = (Lisp_Object) hash_contents;
  struct Lisp_Keymap *self = (struct Lisp_Keymap *) closure;
  if (NILP (Fkeymapp (contents)))
    return;
  self->sub_maps_cache = Fcons (Fcons (key, contents), self->sub_maps_cache);
}

static int map_keymap_sort_predicate ();

static Lisp_Object
keymap_submaps (keymap)
     struct Lisp_Keymap *keymap;
{
  if (!EQ (keymap->sub_maps_cache, Qt))
    return keymap->sub_maps_cache;
  keymap->sub_maps_cache = Qnil;
  elisp_maphash (keymap_submaps_mapper, keymap->table, (void *) keymap);
  /* keep it sorted so that the result of accessible-keymaps is ordered */
  keymap->sub_maps_cache = list_sort (keymap->sub_maps_cache, Qnil,
				      map_keymap_sort_predicate);
  return keymap->sub_maps_cache;
}


static Lisp_Object
make_keymap (size)
     int size;
{
  Lisp_Object keymap = Fmake_vector (make_number (KEYMAP_SIZE), 0);
  XSETTYPE (keymap, Lisp_Keymap);
  XKEYMAP (keymap)->parent = Qnil;
  if (size) /* hack for copy-keymap */
    {
      XKEYMAP (keymap)->table = Fmake_hashtable (make_number (size));
      XKEYMAP (keymap)->inverse_table = Fmake_hashtable (make_number (size));
    }
  XKEYMAP (keymap)->sub_maps_cache = Qnil;
  XKEYMAP (keymap)->fullness = 0;
  XKEYMAP (keymap)->name = Qnil;
  return keymap;
}

DEFUN ("make-keymap", Fmake_keymap, Smake_keymap, 0, 0, 0,
  "Construct and return a new keymap object.  All entries in it are nil,\n\
meaning \"command undefined\".")
  ()
{
  return make_keymap (127);
}

DEFUN ("make-sparse-keymap", Fmake_sparse_keymap, Smake_sparse_keymap, 0, 0, 0,
  "Construct and return a new keymap object.  All entries in it are nil,\n\
meaning \"command undefined\".  The only difference between this function\n\
and make-keymap is that this function returns a \"smaller\" keymap (one\n\
that is expected to contain less entries.)  As keymaps dynamically resize,\n\
the distinction is not great.")
  ()
{
  return make_keymap (10);
}

DEFUN ("keymap-parent", Fkeymap_parent, Skeymap_parent, 1, 1, 0,
       "Returns the `parent' keymap of the given keymap, or nil.\n\
The parent of a keymap is searched for keybindings when a key sequence\n\
isn't bound in this one.  The (current-global-map) is the default parent\n\
of all keymaps.")
     (keymap)
     Lisp_Object keymap;
{
  keymap = get_keymap (keymap);
  CHECK_KEYMAP (keymap, 0);
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

  CHECK_KEYMAP (keymap, 0);
  if (!NILP (parent))
    CHECK_KEYMAP (parent, 1);
  
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
  return (parent);
}

DEFUN ("set-keymap-name", Fset_keymap_name, Sset_keymap_name, 2, 2, 0,
  "Sets the `name' of the given keymap to NEW-NAME\n\
The name is only a debugging convenience; it is not used except\n\
when printing the keymap.")
     (keymap, new_name)
     Lisp_Object keymap, new_name;
{
  Lisp_Object old_name;
  CHECK_KEYMAP (keymap, 0);
  old_name = XKEYMAP (keymap)->name;
  XKEYMAP (keymap)->name = new_name;
  return Qnil;
}

extern Lisp_Object QKbackspace, QKtab, QKlinefeed, QKreturn, 
 QKescape, QKspace, QKdelete, QKundefined;


DEFUN ("keymapp", Fkeymapp, Skeymapp, 1, 1, 0,
  "Return t if ARG is a keymap object.")
  (object)
     Lisp_Object object;
{
  register Lisp_Object tem;
  tem = object;
  while (SYMBOLP (tem))
    {
      tem = XSYMBOL (tem)->function;
      if (EQ (tem, Qunbound))
	return Qnil;
      QUIT;
    }
  return (KEYMAPP (tem) ? Qt : Qnil);
}

Lisp_Object
get_keymap_1 (object, error)
     Lisp_Object object;
     int error;
{
  register Lisp_Object tem;

  while (1)
    {
      tem = object;
      while (SYMBOLP (tem) && !EQ (tem, Qunbound))
	{
	  tem = XSYMBOL (tem)->function;
	  QUIT;
	}
      if (KEYMAPP (tem))
	return tem;
      else if (error)
	object = wrong_type_argument (Qkeymapp, object);
      else
	return Qnil;
    }
}

Lisp_Object
get_keymap (object)
     Lisp_Object object;
{
  return get_keymap_1 (object, 1);
}


static Lisp_Object
access_keymap (map, idx)
     Lisp_Object map;
     Lisp_Object idx;
{
  Lisp_Object keysym;
  int modifiers;
  switch (XTYPE (idx)) {
  case Lisp_Int:
    {
      struct Lisp_Event event;
      event.event_type = empty_event;
      character_to_event (idx, &event);
      keysym = event.event.key.key;
      modifiers = event.event.key.modifiers;
    }
    break;
  case Lisp_Cons:
    keysym = XCONS (idx)->car;
    modifiers = XCONS (idx)->cdr;
    if (!FIXNUMP (modifiers)) return Qnil;
    break;
  default:
    abort ();
  }
  return keymap_lookup (map, keysym, modifiers);
}


/* Given OBJECT which was found in a slot in a keymap,
   trace indirect definitions to get the actual definition of that slot.
   An indirect definition is a list of the form
   (KEYMAP . INDEX), where KEYMAP is a keymap or a symbol defined as one
   and INDEX is an ASCII code, or a cons of (KEYSYM . MODIFIERS).
 */

static Lisp_Object
get_keyelt (object)
     register Lisp_Object object;
{
  while (1)
    {
      register Lisp_Object map, tem;

      map = get_keymap_1 (Fcar_safe (object), 0);
      tem = Fkeymapp (map);
      /* If the contents are (KEYMAP . ELEMENT), go indirect.  */
      if (!NILP (tem))
	{
	  object = Fcdr (object);
	  return access_keymap (map, object);
	}
      else
	/* If the keymap contents looks like (STRING . DEFN),
	   use DEFN.
	   Keymap alist elements like (CHAR MENUSTRING . DEFN)
	   will be used by HierarKey menus.  */
	if (CONSP (object)
	    && STRINGP (XCONS (object)->car))
	  object = XCONS (object)->cdr;
      else
	/* Anything else is really the value.  */
	return object;
    }
}


static void
copy_keymap_inverse_mapper (hash_key, hash_contents, closure)
     void *hash_key, *hash_contents, *closure;
{
  Lisp_Object inverse_table = (Lisp_Object) closure;
  Lisp_Object inverse_contents = (Lisp_Object) hash_contents;
  Lisp_Object oiq = Vinhibit_quit;
  Vinhibit_quit = Qt;
  inverse_contents = Fcopy_sequence (inverse_contents);
  Vinhibit_quit = oiq;
  Fputhash ((Lisp_Object) hash_key, inverse_contents, inverse_table);
}


static Lisp_Object
copy_keymap_internal (keymap)
     struct Lisp_Keymap *keymap;
{
  Lisp_Object nkm = make_keymap (0);
  struct Lisp_Keymap *new_keymap = XKEYMAP (nkm);
  new_keymap->parent = keymap->parent;
  new_keymap->fullness = keymap->fullness;
  new_keymap->sub_maps_cache = Qnil;
  new_keymap->table = Fcopy_hashtable (keymap->table);
  new_keymap->inverse_table = Fcopy_hashtable (keymap->inverse_table);
  /* After copying the inverse map, we need to copy the conses which
     are its values, lest they be shared by the copy, and mangled.
   */
  elisp_maphash (copy_keymap_inverse_mapper, keymap->inverse_table,
		 (void *) keymap->inverse_table);
  return nkm;
}


static Lisp_Object copy_keymap ();

static void
copy_keymap_mapper (hash_key, hash_contents, closure)
     void *hash_key, *hash_contents, *closure;
{
  Lisp_Object key = (Lisp_Object) hash_key;
  Lisp_Object contents = (Lisp_Object) hash_contents;
  struct Lisp_Keymap *self = (struct Lisp_Keymap *) closure;
  /* Don't recursively copy keymaps that are indirected through symbols. */
  if (KEYMAPP (contents))
    keymap_store_internal (key, self, copy_keymap (contents));
}

static Lisp_Object
copy_keymap (keymap)
     Lisp_Object keymap;
{
  if (!KEYMAPP (keymap)) abort ();
  keymap = copy_keymap_internal (XKEYMAP (keymap));
  elisp_maphash (copy_keymap_mapper,
		 XKEYMAP (keymap)->table,
		 XKEYMAP (keymap));
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
  keymap = get_keymap (keymap);
  CHECK_KEYMAP (keymap, 0);
  return copy_keymap (keymap);
}


static int
keymap_fullness (keymap)
     struct Lisp_Keymap *keymap;
{
/*  int fullness = XFASTINT (Fhashtable_fullness (keymap->table));*/
  int fullness = keymap->fullness;
  Lisp_Object sub_maps = keymap_submaps (keymap);
  for (; !NILP (sub_maps); sub_maps = XCONS (sub_maps)->cdr)
    if (bucky_sym_to_bucky_bit (XCONS (XCONS (sub_maps)->car)->car))
      {
	Lisp_Object sub_map = XCONS (XCONS (sub_maps)->car)->cdr;
	fullness--; /* don't count bucky maps */
	fullness += keymap_fullness (XKEYMAP (get_keymap (sub_map)));
      }
  return fullness;
}

DEFUN ("keymap-fullness", Fkeymap_fullness, Skeymap_fullness, 1, 1, 0,
       "Returns the number of bindings in the keymap.")
     (keymap)
     Lisp_Object keymap;
{
  return make_number (keymap_fullness (XKEYMAP (get_keymap (keymap))));
}



/* Given any kind of key-specifier, return a keysym and modifier mask.
 */
static void
define_key_parser (spec, keysym_return, modifiers_return)
     Lisp_Object spec, *keysym_return;
     int *modifiers_return;
{
  if (SYMBOLP (spec)) spec = Fcons (spec, Qnil); /* be nice */
  switch (XTYPE (spec)) {
  case Lisp_Int:
    {
      struct Lisp_Event event;
      event.event_type = empty_event;
      character_to_event (XINT (spec), &event);
      *keysym_return = event.event.key.key;
      *modifiers_return = event.event.key.modifiers;
      return;
    }
  case Lisp_Event:
    switch (XEVENT (spec)->event_type) {
    case key_press_event:
      *keysym_return = XEVENT (spec)->event.key.key;
      *modifiers_return = XEVENT (spec)->event.key.modifiers;
      return;
    case button_press_event:
    case button_release_event:
      {
	register int down = (XEVENT (spec)->event_type == button_press_event);
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
      while (1)
	Fsignal (Qerror,
		 Fcons (build_string ("unable to bind this type of event"),
			Fcons (spec, Qnil)));
    }
  case Lisp_Cons:
    {
      int modifiers = 0;
      int mod_p;
      Lisp_Object keysym;
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
	if (!NILP (XCONS (rest)->cdr)) {
	  if (! mod_p)
	    while (1)
	      Fsignal (Qerror, Fcons (build_string ("unknown modifier"),
				      Fcons (keysym, Qnil)));
	} else {
	  if (mod_p)
	    while (1)
	      Fsignal (Qerror,
		       Fcons (build_string ("nothing but modifiers here"),
			      Fcons (spec, Qnil)));
	}
	rest = XCONS (rest)->cdr;
	QUIT;
      }

      /* Now, check and massage the trailing keysym specifier.
       */
      switch (XTYPE (keysym)) {
      case Lisp_Symbol:
	if (XSYMBOL (keysym)->name->size != 1)
	  break;
	XSET (keysym, Lisp_Int, XSYMBOL (keysym)->name->data [0]);
	/* fall through */
      case Lisp_Int:
	if (XINT (keysym) < ' ' || XINT (keysym) > 127)
	  while (1)
	    Fsignal (Qerror,
		     Fcons (build_string
			    ("keysym must be in the printing ASCII range"),
			    Fcons (keysym, Qnil)));
	if (modifiers & MOD_SHIFT)
	  while (1)
	    Fsignal (Qerror,
		     Fcons (build_string (
		   "the `shift' modifier may not be applied to ASCII keysyms"),
			    Fcons (spec, Qnil)));
	break;
      default:
	while (1)
	  Fsignal (Qerror, Fcons (build_string ("unknown keysym specifier"),
				  Fcons (keysym, Qnil)));
      }

      *keysym_return = keysym;
      *modifiers_return = modifiers;
      return;
    }
  default:
    while (1)
      Fsignal (Qerror, Fcons (build_string ("unknown key-sequence specifier"),
			      Fcons (spec, Qnil)));
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
	Fsignal (Qerror, Fcons (build_string ("invalid menu event desc"),
				Fcons (list, Qnil)));
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
  return (meta_prefix_char == event_to_character (&event, 0));
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
      if (keysym == '@')
	*keysym_ret = QKspace,	   *modifiers_ret = modifiers;
      else if (keysym == QKspace)
	*keysym_ret = '@',	   *modifiers_ret = modifiers;
      else if (keysym == 'h')
	*keysym_ret = QKbackspace, *modifiers_ret = modifiers_sans_control;
      else if (keysym == 'i')
	*keysym_ret = QKtab,	   *modifiers_ret = modifiers_sans_control;
      else if (keysym == 'j')
	*keysym_ret = QKlinefeed,  *modifiers_ret = modifiers_sans_control;
      else if (keysym == 'm')
	*keysym_ret = QKreturn,	   *modifiers_ret = modifiers_sans_control;
      else if (keysym == '[')
	*keysym_ret = QKescape,	   *modifiers_ret = modifiers_sans_control;
    }
  else if (modifiers_sans_meta)
    ;
  else if (EQ (keysym, QKbackspace))
    *keysym_ret = 'h', *modifiers_ret = modifiers | MOD_CONTROL;
  else if (EQ (keysym, QKtab))
    *keysym_ret = 'i', *modifiers_ret = modifiers | MOD_CONTROL;
  else if (EQ (keysym, QKlinefeed))
    *keysym_ret = 'j', *modifiers_ret = modifiers | MOD_CONTROL;
  else if (EQ (keysym, QKreturn))
    *keysym_ret = 'm', *modifiers_ret = modifiers | MOD_CONTROL;
  else if (EQ (keysym, QKescape))
    *keysym_ret = '[', *modifiers_ret = modifiers | MOD_CONTROL;
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
  define_key_parser (meta_prefix_char, &meta_sym, &meta_mods);
  mpc_binding = get_keyelt (keymap_lookup (keymap, meta_sym, meta_mods));
  if (NILP (mpc_binding) || !NILP (Fkeymapp (mpc_binding)))
    return;

  if (index == 0)
    new_keys = keys;
  else if (STRINGP (keys))
    new_keys = Fsubstring (keys, 0, index);
  else if (VECTORP (keys))
    {
      new_keys = Fmake_vector (index, Qnil);
      for (i = 0; i < index; i++)
	XVECTOR (new_keys)->contents [i] = XVECTOR (keys)->contents [i];
    }
  else
    abort ();
  if (keys == new_keys)
    sprintf (buf, "can't bind %s: %s has a non-keymap binding",
	(char *) XSTRING (Fkey_description (keys))->data,
	(char *) XSTRING (Fsingle_key_description (meta_prefix_char))->data);
  else
    sprintf (buf, "can't bind %s: %s %s has a non-keymap binding",
	(char *) XSTRING (Fkey_description (keys))->data,
	(char *) XSTRING (Fkey_description (new_keys))->data,
	(char *) XSTRING (Fsingle_key_description (meta_prefix_char))->data);
  while (1)
    Fsignal (Qerror, Fcons (build_string (buf), Fcons (mpc_binding, Qnil)));
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
      str[0] = XFASTINT (keysym);
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
     register Lisp_Object keymap;
     Lisp_Object keys;
     Lisp_Object def;
{
  int idx;
  Lisp_Object c;
  Lisp_Object tem;
  Lisp_Object cmd;
  Lisp_Object keysym, keysym2;
  int modifiers, modifiers2;
  int metized = 0;
  int size;
  int ascii_hack;

  switch (XTYPE (keys)) {
  case Lisp_String:
    size = XSTRING (keys)->size; break;
  case Lisp_Vector:
    size = XVECTOR (keys)->size; break;
  case Lisp_Int:
  case Lisp_Symbol:
    keys = Fcons (keys, Qnil); /* fall through */
  case Lisp_Cons:
    size = 1;
    keys = Fmake_vector (1, keys); /* this is kinda sleazy. */
    break;
  default:
    return
      Fsignal (Qerror,
	       Fcons (build_string ("must be a string, vector, or list"),
		      Fcons (keys, Qnil)));
  }

  /* ASCII grunge.
     When the user defines a key which, in a strictly ASCII world, would be
     produced by two different keys (^J and linefeed, or ^H and backspace,
     for example) then the binding will be made for both keysyms.

     This is done if the user binds a command to a string, as in
     (define-key map "\^H" 'something), but not when using one of the new
     syntaxes, like (define-key map '(control h) 'something).
   */
  ascii_hack = (STRINGP (keys));

  keymap = get_keymap (keymap);
  idx = 0;
  while (1)
    {
      if (STRINGP (keys))
	c = XSTRING (keys)->data [idx];
      else {
	c = XVECTOR (keys)->contents [idx];
	if (FIXNUMP (c) &&
	    (XINT (c) < ' ' || XINT (c) > 127))
	  return
	    Fsignal (Qerror,
		     Fcons (build_string
			    ("keysym must be in the printing ASCII range"),
			    Fcons (c, Qnil)));
      }

      define_key_parser (c, &keysym, &modifiers);

      if (!metized &&
	  meta_prefix_char >= 0 &&
	  (c == meta_prefix_char || meta_prefix_char_p (keysym, modifiers)))
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
		      (defvar my-escape-map (lookup-key my-map "\^["))
		 if the luser really wants the map in a variable.
	       */
	      Lisp_Object mmap = get_keyelt (keymap_lookup (keymap, Qmeta, 0));
	      if (!NILP (mmap) &&
		  keymap_fullness (XKEYMAP (get_keymap (mmap))) != 0)
		{
		  char buf [255];
		  sprintf (buf,
			   "can't bind %s: this map contains meta-bindings",
			   (char *) XSTRING (Fsingle_key_description
					     (meta_prefix_char))->data);
		  return
		    Fsignal (Qerror,
			     Fcons (build_string (buf), Fcons (keymap, Qnil)));
		}
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

      if (++idx == size) {
	keymap_store (keymap, keysym, modifiers, def);
	if (ascii_hack && !NILP (keysym2))
	  keymap_store (keymap, keysym2, modifiers2, def);
	return def;
      }
      
      cmd = get_keyelt (keymap_lookup (keymap, keysym, modifiers));
      if (NILP (cmd))
	{
	  cmd = Fmake_sparse_keymap ();
	  keymap_store (keymap, keysym, modifiers, cmd);
	}
      tem = Fkeymapp (cmd);
      if (NILP (tem))
	return
	  Fsignal (Qerror,
		   Fcons (build_string ("invalid prefix keys in sequence"),
			  Fcons (c, Fcons (keys, Qnil))));

      if (ascii_hack && !NILP (keysym2) &&
	  NILP (get_keyelt (keymap_lookup (keymap, keysym2, modifiers2))))
	keymap_store (keymap, keysym2, modifiers2, cmd);

      keymap = get_keymap (cmd);
    }
}


/* We need a very fast (i.e., non-consing) version of lookup-key in order 
   to make where-is-internal really fly.
 */

struct raw_key {
  unsigned int bits;
  Lisp_Object keysym;
};


static Lisp_Object
raw_lookup_key (keymap, raw_keys, raw_keys_count)
     Lisp_Object keymap;
     struct raw_key *raw_keys;
     int raw_keys_count;
{
  int metized = 0;
  Lisp_Object parent;
  struct raw_key *raw_keys_left;
  int remaining;

  keymap = get_keymap (keymap);
  parent = XKEYMAP (keymap)->parent;
  remaining = raw_keys_count;
  raw_keys_left = raw_keys;

  while (1)
    {
      Lisp_Object cmd;
      Lisp_Object keysym = raw_keys_left->keysym;
      int modifiers = raw_keys_left->bits;
      remaining--;

      if (!metized &&
	  meta_prefix_char >= 0 &&
	  (meta_prefix_char_p (keysym, modifiers)))
	{
	  if (remaining == 0)
	    {
	      /* This is a hack so that looking up a key-sequence whose last
		 element is the meta-prefix-char will return the keymap that
		 the "meta" keys are stored in, if there is no binding for
		 the meta-prefix-char (and if this map has a "meta" submap.)
		 If this map doesnt have a "meta" submap, then the
		 meta-prefix-char is looked up just like any other key.
	       */
	      if (NILP (keymap_lookup (keymap, keysym, modifiers)))
		{
		  Lisp_Object mmap =
		    get_keyelt (keymap_lookup (keymap, Qmeta, 0));
		  if (!NILP (mmap))
		    return mmap;
		}
	    }
	  else
	    {
	      metized = 1;
	      raw_keys_left++;
	      continue;
	    }
	}

      if (metized)
	{
	  modifiers |= MOD_META;
	  metized = 0;
	}

      cmd = get_keyelt (keymap_lookup (keymap, keysym, modifiers));

      if (remaining == 0)
	{
	  if (NILP (cmd) && !NILP (parent) && !NILP (Fkeymapp (parent)))
	    {
	      QUIT;
	      return raw_lookup_key (parent, raw_keys, raw_keys_count);
	    }
	  else
	    return cmd;
	}

      if (NILP (Fkeymapp (cmd)))
	{
	  if (!NILP (parent) && !NILP (Fkeymapp (parent)))
	    {
	      QUIT;
	      return raw_lookup_key (parent, raw_keys, raw_keys_count);
	    }
	  else
	    return make_number (raw_keys_count - remaining);
	}
      keymap = get_keymap (cmd);
      raw_keys_left++;
      QUIT;
    }
  abort ();
}


static struct raw_key *lookup_key_buf;
static int lookup_key_buf_total_size;

/* Value is number if `keys' is too long; NIL if valid but has no definition. 
*/
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
  Lisp_Object c;
  int i = 0;
  int size;
  Lisp_Object keysym;
  int modifiers;

  switch (XTYPE (keys)) {
  case Lisp_String:
    size = XSTRING (keys)->size;
    break;
  case Lisp_Vector:
    size = XVECTOR (keys)->size;
    break;
  case Lisp_Int:
  case Lisp_Symbol:
    size = 1;
    break;
  default:
    return
      Fsignal (Qerror, Fcons (build_string ("must be a string or vector"),
			      Fcons (keys, Qnil)));
  }

  if (size == 0) return Qnil;

  if (size >= lookup_key_buf_total_size)
    {
      lookup_key_buf_total_size = size + 50;
      lookup_key_buf = (struct raw_key *)
	xrealloc (lookup_key_buf, sizeof (struct raw_key) *
		  lookup_key_buf_total_size);
    }

  for (i = 0; i < size; i++)
    {
      switch (XTYPE (keys)) {
      case Lisp_String:
	c = XSTRING (keys)->data [i];
	break;
      case Lisp_Vector:
	c = XVECTOR (keys)->contents [i];
	break;
      case Lisp_Int:
      case Lisp_Symbol:
	c = keys;
	break;
      default:
	abort ();
      }
      define_key_parser (c, &keysym, &modifiers);
      lookup_key_buf[i].keysym = keysym;
      lookup_key_buf[i].bits = modifiers;
    }
  return raw_lookup_key (keymap, lookup_key_buf, size);
}


extern Lisp_Object Vmouse_grabbed_buffer;

DEFUN ("key-binding", Fkey_binding, Skey_binding, 1, 1, 0,
  "Return the binding for command KEYS in current keymaps.\n\
KEYS is a string, a vector of events, or a vector of key-description lists\n\
as described in the documentation for the `define-key' function.\n\
The binding is probably a symbol with a function definition.")
  (keys)
     Lisp_Object keys;
{
  Lisp_Object map, value;
  int mouse_p = 0;
  Lisp_Object mouse_buffer = Qnil;

  /* If this is a mouse-click event, then the "local" keymap is considered
     to be the local map of the buffer in the window over which the mouse
     was clicked, not necessarily the window which point is in.
   */
  if ((VECTORP (keys) && XVECTOR (keys)->size > 0))
    {
      Lisp_Object event = XVECTOR (keys)->contents [XVECTOR (keys)->size - 1];
      if (EVENTP (event) &&
	  (XEVENT (event)->event_type == button_press_event ||
	   XEVENT (event)->event_type == button_release_event))
	{
	  Lisp_Object window = Fevent_window (event);
	  if (!NILP (window))
	    mouse_buffer = Fwindow_buffer (window);
	  mouse_p = 1;
	}
    }

  /* If this is a mouse-click event, and if `mouse-grabbed-buffer' is a
     buffer, then it is consulted before either the local map (in this
     case, the map of the buffer of the window under point) or the global
     map.  (The motion-handling code also consults mouse-grabbed-buffer.)
   */
  if (mouse_p && BUFFERP (Vmouse_grabbed_buffer))
    {
      Lisp_Object grabbed_map = XBUFFER (Vmouse_grabbed_buffer)->keymap;
      if (!NILP (grabbed_map))
	{
	  value = Flookup_key (grabbed_map, keys);
	  if (!NILP (value) && !FIXNUMP (value))
	    return value;
	}
    }

  /* If it's a mouse event, the local-map of the current buffer is not
     consulted at all (unless the mouse is over this buffer.) */
  if (NILP (mouse_buffer))
    map = current_buffer->keymap;
  else
    map = XBUFFER (mouse_buffer)->keymap;

  if (!NILP (map))
    {
      value = Flookup_key (map, keys);
      if (! NILP (value) && !FIXNUMP (value))
	return value;
    }
  map = Vcurrent_global_map;
  value = Flookup_key (map, keys);
  if (FIXNUMP (value))
    return Qnil;
  return value;
}


DEFUN ("use-global-map", Fuse_global_map, Suse_global_map, 1, 1, 0,
  "Select KEYMAP as the global keymap.")
  (keymap)
     Lisp_Object keymap;
{
  keymap = get_keymap (keymap);
  CHECK_KEYMAP (keymap, 0);
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
    keymap = get_keymap (keymap);

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

struct map_keymap_closure {
  void (*fn) ();
  void *arg;
  int bits;
};

/* used by map_keymap() */
static void
map_keymap_unsorted_mapper (hash_key, hash_contents, junk)
     void *hash_key, *hash_contents, *junk;
{
  Lisp_Object key = (Lisp_Object) hash_key;
  Lisp_Object contents = (Lisp_Object) hash_contents;
  struct map_keymap_closure *closure = (struct map_keymap_closure *) junk;
  int modifiers = closure->bits;
  int mod_bit = bucky_sym_to_bucky_bit (key);
  if (mod_bit)
    {
      int omod = modifiers;
      closure->bits = modifiers | mod_bit;
      contents = get_keymap (contents);
      CHECK_KEYMAP (contents, 0);
      elisp_maphash (map_keymap_unsorted_mapper,
		     XKEYMAP (contents)->table,
		     junk);
      closure->bits = omod;
    }
  else
    (*closure->fn) (key, modifiers, contents, closure->arg);
}


/* used by map_keymap_sorted() */
static void
map_keymap_sorted_mapper (hash_key, hash_contents, closure)
     void *hash_key, *hash_contents, *closure;
{
  Lisp_Object key = (Lisp_Object) hash_key;
  Lisp_Object contents = (Lisp_Object) hash_contents;
  Lisp_Object *list = (Lisp_Object *) closure;
  *list = Fcons (Fcons (key, contents), *list);
}


/* used by map_keymap_sorted(), describe_vector_sort_predicate(),
   and keymap_submaps().
 */
static int
map_keymap_sort_predicate (obj1, obj2, pred)
     Lisp_Object obj1, obj2, pred;
{
  /* obj1 and obj2 are conses with keysyms in their cars.  Cdrs are ignored.
   */
  int bit1, bit2;
  int sym1_p = 0, sym2_p = 0;
  obj1 = XCONS (obj1)->car;
  obj2 = XCONS (obj2)->car;
  if (obj1 == obj2) return -1;

  bit1 = bucky_sym_to_bucky_bit (obj1);
  bit2 = bucky_sym_to_bucky_bit (obj2);
  
  /* If either is a symbol with a character-set-property, then sort it by
     that code instead of alphabetically.
   */
  if (! bit1 && SYMBOLP (obj1))
    {
      Lisp_Object code = Fget (obj1, Vcharacter_set_property);
      if (FIXNUMP (code))
	obj1 = code, sym1_p = 1;
    }
  if (! bit2 && SYMBOLP (obj2))
    {
      Lisp_Object code = Fget (obj2, Vcharacter_set_property);
      if (FIXNUMP (code))
	obj2 = code, sym2_p = 1;
    }

  /* all symbols (non-ASCIIs) come after integers (ASCIIs) */
  if (XTYPE (obj1) != XTYPE (obj2))
    return ((SYMBOLP (obj2)) ? 1 : -1);

  if (FIXNUMP (obj1)) /* they're both ASCII */
    {
      if (obj1 == obj2 &&	/* If one started out as a symbol and the */
	  sym1_p != sym2_p)	/* other didn't, the symbol comes last. */
	return (sym2_p ? 1 : -1);

    return ((obj1 < obj2) ? 1 : -1);	/* else just compare them */
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
map_keymap_sorted (keymap, bits, function, closure)
     struct Lisp_Keymap *keymap;
     int bits;
     void (*function) ();
     void *closure;
{
  struct gcpro gcpro1, gcpro2, gcpro3;
  Lisp_Object key, binding;
  Lisp_Object contents = Qnil;
  int sub_bit;
  if (! XFASTINT (Fhashtable_fullness (keymap->table)))
    return;
  GCPRO3 (key, binding, contents);
  elisp_maphash (map_keymap_sorted_mapper, keymap->table, (void *) &contents);
  contents = list_sort (contents, Qnil, map_keymap_sort_predicate);
  for (; !NILP (contents); contents = XCONS (contents)->cdr)
    {
      key = XCONS (XCONS (contents)->car)->car;
      binding = XCONS (XCONS (contents)->car)->cdr;
      sub_bit = bucky_sym_to_bucky_bit (key);
      if (sub_bit)
	map_keymap_sorted (XKEYMAP (get_keymap (binding)), bits | sub_bit,
			   function, closure);
      else
	(*function) (key, bits, binding, closure);
    }
  UNGCPRO;
}


/* externally callable; used by Fmap_keymap() */
void
map_keymap (keymap, sort_first, function, fn_arg)
     struct Lisp_Keymap *keymap;
     int sort_first;
     void (*function) ();
     void *fn_arg;
{
  if (sort_first)
    map_keymap_sorted (keymap, 0, function, fn_arg);
  else
    {
      struct map_keymap_closure closure;
      closure.fn = function;
      closure.arg = fn_arg;
      closure.bits = 0;
      elisp_maphash (map_keymap_unsorted_mapper, keymap->table,
		     (void *) &closure);
    }
}

/* used by Fmap_keymap() */
static void
map_keymap_mapper (key, bits, binding, function)
     Lisp_Object key, binding;
     int bits;
     void *function;
{
  call2 ((Lisp_Object) function,
	 make_key_description (key, bits, 1),
	 binding);
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
  if (!NILP (Fkeymapp (function))) /* tolerate obviously transposed args */
    {
      Lisp_Object tmp = function;
      function = keymap;
      keymap = tmp;
    }
  keymap = get_keymap (keymap);
  CHECK_KEYMAP (keymap, 0);
  map_keymap (XKEYMAP (keymap), !NILP (sort_first),
	      map_keymap_mapper, (void *) function);
  return Qnil;
}



/* we use these globals as scratch space because the alternative is
   even uglier... (ok, well, we could define a struct and pass it instead...)
 */
static Lisp_Object accessible_keymaps;
static Lisp_Object accessible_keymaps_tail;

static int
bucky_sym_to_bucky_bit (sym)
     Lisp_Object sym;
{
  if (sym == Qcontrol) return MOD_CONTROL;
  else if (sym == Qmeta) return MOD_META;
  else if (sym == Qsuper) return MOD_SUPER;
  else if (sym == Qhyper) return MOD_HYPER;
  else if (sym == Qsymbol) return MOD_SYMBOL;
  else if (sym == Qshift) return MOD_SHIFT;
  else return 0;
}

static void
accessible_keymaps_mapper (hash_key, hash_contents, closure)
     void *hash_key, *hash_contents, *closure;
{
  Lisp_Object key = (Lisp_Object) hash_key;
  Lisp_Object contents = (Lisp_Object) hash_contents;
  int modifiers = (int) closure;

  Lisp_Object thisseq, cmd, vec;
  int j, bit = bucky_sym_to_bucky_bit (key);
  if (bit)
    {
      Lisp_Object submaps;
      contents = get_keymap (contents);
      CHECK_KEYMAP (contents, 0);
      submaps = keymap_submaps (XKEYMAP (contents));
      for (; !NILP (submaps); submaps = XCONS (submaps)->cdr)
	{
	  accessible_keymaps_mapper ((void *)XCONS (XCONS (submaps)->car)->car,
				     (void *)XCONS (XCONS (submaps)->car)->cdr,
				     (void *)(bit | modifiers));
	}
      return;
    }
  cmd = get_keyelt (contents);
  if (NILP (cmd))
    abort ();
  if (NILP (Fkeymapp (cmd)))
    abort ();
  cmd = get_keymap (cmd);
  thisseq = Fcar (Fcar (accessible_keymaps_tail));

  vec = Fmake_vector (XVECTOR (thisseq)->size + 1, Qnil);
  for (j = 0; j < XVECTOR (thisseq)->size; j++)
    XVECTOR (vec)->contents [j] = XVECTOR (thisseq)->contents [j];
  XVECTOR (vec)->contents [j] = make_key_description (key, modifiers, 1);
  nconc2 (accessible_keymaps_tail, Fcons (Fcons (vec, cmd), Qnil));
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
  accessible_keymaps = Fcons (Fcons (Fmake_vector (0, Qnil),
				     get_keymap (startmap)), Qnil);
  accessible_keymaps_tail = accessible_keymaps;

  /* For each map in the list maps,
     look at any other maps it points to
     and stick them at the end if they are not already in the list */

  for (accessible_keymaps_tail = accessible_keymaps;
       !NILP (accessible_keymaps_tail);
       accessible_keymaps_tail = XCONS (accessible_keymaps_tail)->cdr)
    {
/*    Lisp_Object thisseq = Fcar (Fcar (accessible_keymaps_tail)); */
      Lisp_Object thismap = Fcdr (Fcar (accessible_keymaps_tail));
      CHECK_KEYMAP (thismap, 0);
      for (; !NILP (thismap); thismap = XKEYMAP (thismap)->parent)
	{
	  Lisp_Object submaps = keymap_submaps (XKEYMAP (thismap));
	  for (; !NILP (submaps); submaps = XCONS (submaps)->cdr)
	    accessible_keymaps_mapper((void *)XCONS(XCONS (submaps)->car)->car,
				      (void *)XCONS(XCONS (submaps)->car)->cdr,
				      (void *) 0);
	}
    }
  return accessible_keymaps;
}



Lisp_Object Qsingle_key_description, Qkey_description;

DEFUN ("key-description", Fkey_description, Skey_description, 1, 1, 0,
  "Return a pretty description of key-sequence KEYS.\n\
Control characters turn into \"C-foo\" sequences, meta into \"M-foo\"\n\
spaces are put between sequence elements, etc.")
  (keys)
     Lisp_Object keys;
{
  switch (XTYPE (keys))
    {
    case Lisp_Event:
    case Lisp_Symbol:
    case Lisp_Cons:
    case Lisp_Int:
      return Fsingle_key_description (keys);
    case Lisp_Vector:
    case Lisp_String:
      {
	int string_p = (STRINGP (keys));
	Lisp_Object string = Qnil;
	Lisp_Object sep = Qnil;
	Lisp_Object s2;
	int i;
	int size = string_p ? XSTRING (keys)->size : XVECTOR (keys)->size;
	for (i = 0; i < size; i++) {
	  s2 = Fsingle_key_description (string_p ?
					XSTRING (keys)->data[i] :
					XVECTOR (keys)->contents[i]);

	  if (NILP (string))
	    string = s2;
	  else {
	    if (NILP (sep)) sep = build_string (" ");
	    string = concat2 (string, concat2 (sep, s2));
	  }
	}
	return string;
      }
    default:
      return
	Fsignal (Qerror, Fcons (build_string ("unknown key sequence type"),
				Fcons (keys, Qnil)));
    }
}

DEFUN ("single-key-description", Fsingle_key_description,
       Ssingle_key_description, 1, 1, 0,
  "Return a pretty description of command character KEY.\n\
Control characters turn into C-whatever, etc.")
  (key)
     Lisp_Object key;
{
  if (SYMBOLP (key))
    key = Fcons (key, Qnil); /* sleaze sleaze */

  switch (XTYPE (key))
    {
    case Lisp_Event:
    case Lisp_Int:
      {
	char buf [255];
	if (FIXNUMP (key)) {
	  struct Lisp_Event event;
	  event.event_type = empty_event;
	  character_to_event (key, &event);
	  format_event_object (buf, &event, 1);
	}
	else
	  format_event_object (buf, XEVENT (key), 1);
	return build_string (buf);
      }
    case Lisp_Cons:
      {
	char buf [255];
	char *bufp = buf;
	Lisp_Object rest;
	buf[0]=0;
	for (rest = key; !NILP (rest); rest = XCONS (rest)->cdr) {
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
	  else {
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
	      return
		Fsignal (Qerror,
			 Fcons (build_string ("invalid key description"),
				Fcons (key, Qnil)));
	  }
	}
	return build_string (buf);
      }
    default:
      return
	Fsignal (Qerror,
		 Fcons (build_string ("must be a character or an event."),
			Fcons (key, Qnil)));
    }
}

static char *
push_text_char_description (c, p)
     register unsigned int c;
     register char *p;
{
  if (c >= 0200)
    {
      *p++ = 'M';
      *p++ = '-';
      c -= 0200;
    }
  if (c < 040)
    {
      *p++ = '^';
      *p++ = c + 64;		/* 'A' - 1 */
    }
  else if (c == 0177)
    {
      *p++ = '^';
      *p++ = '?';
    }
  else
    *p++ = c;
  return p;  
}

DEFUN ("text-char-description", Ftext_char_description, Stext_char_description, 1, 1, 0,
  "Return a pretty description of file-character CHAR.\n\
Control characters turn into \"^char\", etc.")
  (chr)
     Lisp_Object chr;
{
  char tem[6];

  if (EVENTP (chr))
    {
      Lisp_Object ch = Fevent_to_character (chr, Qt);
      if (NILP (ch))
	return
	  Fsignal (Qerror,
		   Fcons (build_string ("character has no ASCII equivalent"),
			  Fcons (Fcopy_event (chr, Qnil), Qnil)));
      chr = ch;
    }

  CHECK_FIXNUM (chr, 0);

  *push_text_char_description (XINT (chr) & 0377, tem) = 0;

  return build_string (tem);
}



static Lisp_Object where_is_recursive ();

DEFUN ("where-is-internal", Fwhere_is_internal, Swhere_is_internal, 1, 5, 0,
  "Return list of keys that invoke DEFINITION in optional 2nd argument KEYMAP\n\
or optional 4th argument GLOBAL_KEYMAP.\n\
If KEYMAP is nil, search only GLOBAL_KEYMAP.\n\
If GLOBAL_KEYMAP is nil, use the current global map.\n\
\n\
If optional 3rd arg FIRSTONLY is non-nil,\n\
return the first key sequence found, rather than a list of all possible\n\
key sequences.")
  (definition, local_keymap, firstonly, global_keymap, noindirect)
     Lisp_Object definition, local_keymap, global_keymap;
     Lisp_Object firstonly, noindirect;
{
  Lisp_Object found [2];
  if (NILP (global_keymap))
    global_keymap = Vcurrent_global_map;
  if (EQ (local_keymap, global_keymap))
    local_keymap = Qnil;

  if (NILP (local_keymap))
    found[0] = Qnil;
  else
    found[0] = where_is_recursive (definition, local_keymap, Qnil,
				   firstonly, 0, 0, 0);

  if (!NILP (firstonly) && !NILP (found[0]))
    return found[0];

  found[1] = where_is_recursive (definition, global_keymap, local_keymap,
				 firstonly, 0, 0, 0);
  if (!NILP (firstonly))
    return found[1];
  else if (NILP (found[1]))
    return found[0];
  else if (NILP (found[0]))
    return found[1];
  else
    return Fnconc (2, found);
}


/* This function is like
   (key-description (where-is-internal def local-map t global-map))
   except that it writes its output into a (char *) buffer that you 
   provide; it doesn't cons (or allocate memory) at all, so it's
   very fast.  This is used by menubar.c.
 */
void
where_is_to_char (definition, local_keymap, global_keymap, buf)
     Lisp_Object definition, local_keymap, global_keymap;
     char *buf;
{
  Lisp_Object found;
  if (NILP (global_keymap))
    global_keymap = Vcurrent_global_map;
  if (EQ (local_keymap, global_keymap))
    local_keymap = Qnil;

  buf[0] = 0;
  if (!NILP (local_keymap))
    {
      found = where_is_recursive (definition, local_keymap, Qnil, Qt,
				  0, 0, buf);
      if (!NILP (found))
	return;
    }
  where_is_recursive (definition, global_keymap, local_keymap, Qt,
		      0, 0, buf);
}


static struct raw_key *keys_so_far;
static int keys_so_far_total_size;


static Lisp_Object 
raw_keys_to_keys (keys, count)
     struct raw_key *keys;
     int count;
{
  Lisp_Object result = Fmake_vector (make_number (count), Qnil);
  while (count--)
    XVECTOR (result)->contents [count] =
      make_key_description (keys[count].keysym, keys[count].bits, 1);
  return result;
}


static void
format_raw_keys (keys, count, buf)
     struct raw_key *keys;
     int count;
     char *buf;
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


static Lisp_Object
where_is_recursive (definition, map, shadow, firstonly,
		    keys_count, bits_so_far, target_buffer)
     Lisp_Object definition, map, shadow, firstonly;
     int keys_count, bits_so_far;
     char *target_buffer;
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

  if (keys_count >= keys_so_far_total_size)
    {
      keys_so_far_total_size = keys_count + 50;
      keys_so_far = (struct raw_key *)
	xrealloc (keys_so_far,
		  keys_so_far_total_size * sizeof (struct raw_key));
    }

  for (map = get_keymap (map);
       !NILP (map);
       map = get_keymap_1 (XKEYMAP (map)->parent, 0))
    {
      Lisp_Object keys = Fgethash (definition, XKEYMAP (map)->inverse_table,
				   Qnil);
      Lisp_Object submaps;

      if (!(NILP (keys)))
	{
	  /* Verify that this key binding is not shadowed by another binding
	     for the same key, before we say it exists.  The mechanism: look
	     for a local definition of this key and if it is defined and does
	     not match what we found then ignore this key.  Either nil or
	     number as value from raw_lookup_key() means undefined.
	   */
	  Lisp_Object shadowed = Qnil;

	  for (; !NILP (keys); keys = XCONS (keys)->cdr)
	    {
	      keys_so_far [keys_count].keysym = XCONS (keys)->car;
	      keys_so_far [keys_count].bits = bits_so_far;
	      if (!NILP (shadow))
		shadowed = raw_lookup_key (shadow, keys_so_far, keys_count+1);
	      if (NILP (shadowed) ||
		  FIXNUMP (shadowed) ||
		  shadowed == definition)
		{
		  if (target_buffer)
		    {
		      if (NILP (firstonly)) abort ();
		      format_raw_keys (keys_so_far, keys_count + 1,
				       target_buffer);
		      return 1;
		    }
		  else if (!NILP (firstonly))
		    return raw_keys_to_keys (keys_so_far, keys_count + 1);
		  else
		    result =
		      Fcons (raw_keys_to_keys (keys_so_far, keys_count + 1),
			     result);
		}
	    }
	}

      /* Now search the sub-keymaps of this map.
	 If we're in "firstonly" mode and have already found one, this 
	 point is not reached.  If we get one from lower down, either
	 return it immediately (in firstonly mode) or tack it onto the
	 end of the ones we've gotten so far.
       */
      for (submaps = keymap_submaps (XKEYMAP (map));
	   !NILP (submaps);
	   submaps = XCONS (submaps)->cdr)
	{
	  Lisp_Object key    = XCONS (XCONS (submaps)->car)->car;
	  Lisp_Object submap = XCONS (XCONS (submaps)->car)->cdr;
	  Lisp_Object lower;
	  int lower_bits;
	  int lower_keys_count = keys_count;
	  int bucky;

	  submap = get_keymap_1 (submap, 0);

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
	      lower_bits = 0;
	      keys_so_far [lower_keys_count].keysym = key;
	      keys_so_far [lower_keys_count].bits = bits_so_far;
	      lower_keys_count++;
	    }
	  lower = where_is_recursive (definition, submap, shadow, firstonly,
				      lower_keys_count, lower_bits,
				      target_buffer);
	  if (NILP (firstonly))
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


DEFUN ("describe-bindings", Fdescribe_bindings, Sdescribe_bindings, 0, 1, "P",
  "Show a list of all defined keys, and their definitions.\n\
The list is put in a buffer, which is displayed.\n\
If the argument is non-null, then only the mouse bindings are displayed.")
  (mice_only_p)
  Lisp_Object mice_only_p;
{
  register Lisp_Object thisbuf;
  XSET (thisbuf, Lisp_Buffer, current_buffer);
  internal_with_output_to_temp_buffer ("*Help*",
				       NILP (mice_only_p)
				       ? describe_buffer_bindings
				       : describe_buffer_mouse_bindings,
				       thisbuf, Qnil);
  return Qnil;
}


static Lisp_Object
describe_buffer_bindings_1 (descbuf, mice_only_p)
     Lisp_Object descbuf;
     int mice_only_p;
{
  register Lisp_Object start1;
  char *heading = (mice_only_p
		   ? "button          binding\n------          -------\n"
		   : "key             binding\n---             -------\n");
  Fset_buffer (Vstandard_output);

  start1 = XBUFFER (descbuf)->keymap;
  if (!NILP (start1))
    {
      insert_string ("Local Bindings:\n");
      insert_string (heading);
      describe_map_tree (start1, 0, Qnil, Qnil, mice_only_p);
      insert_string ("\n");
    }

  insert_string ("Global Bindings:\n");
  insert_string (heading);

  describe_map_tree (Vcurrent_global_map, 0, XBUFFER (descbuf)->keymap, Qnil, mice_only_p);

  Fset_buffer (descbuf);
  return Qnil;
}

static Lisp_Object
describe_buffer_bindings (descbuf)
     Lisp_Object descbuf;
{
  return describe_buffer_bindings_1 (descbuf, 0);
}

static Lisp_Object
describe_buffer_mouse_bindings (descbuf)
     Lisp_Object descbuf;
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
describe_map_tree (startmap, partial, shadow, chartab, mice_only_p)
     Lisp_Object startmap, shadow;
     int partial;
     Lisp_Object chartab;
     int mice_only_p;
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
		      (NILP (sh) ? Qnil : get_keymap (sh)),
		      chartab, mice_only_p);
    }
  UNGCPRO;
}


#define insert1(arg)  { Lisp_Object tem = (arg); Finsert (1, &tem); }

static void
describe_command (definition)
     Lisp_Object definition;
{
  Findent_to (make_number (16), make_number (1));
  if (SYMBOLP (definition))
    {
      insert_raw_string ((char *) XSYMBOL (definition)->name->data,
			 XSYMBOL (definition)->name->size);
      insert_string ("\n");
    }
  else
    {
      if (!NILP (Fkeymapp (definition)))
	insert_string ("Prefix Command\n");
      else if (STRINGP (definition) ||
	       VECTORP (definition))
	{
	  Lisp_Object string;
	  insert_string ("Kbd Macro: ");
	  string = Fkey_description (definition);
	  insert_raw_string ((char *) XSTRING (string)->data,
			     XSTRING (string)->size);
	  insert_string ("\n");
	}
      else if (COMPILEDP (definition))
	insert_string ("Anonymous Compiled Function\n");
      else if (CONSP (definition) &&
	       XCONS (definition)->car == Qlambda)
	insert_string ("Anonymous Lambda\n");
      else
	insert_string ("??\n");
    }
}

/* Describe the contents of map MAP, assuming that this map
   itself is reached by the sequence of prefix keys KEYS (a vector).
   PARTIAL, SHADOW and CHARTAB are as in `describe_map_tree' above.  */

static void
describe_map (map, keys, partial, shadow, chartab, mice_only_p)
     Lisp_Object map, keys;
     int partial;
     Lisp_Object shadow;
     Lisp_Object chartab;
     int mice_only_p;
{
  Lisp_Object keysdesc = 0;
  struct gcpro gcpro1;
  
  GCPRO1 (keysdesc);

  if (!NILP (keys) && XINT (Flength (keys)) > 0)
    keysdesc = concat2 (Fkey_description (keys), build_string (" "));
  else
    keysdesc = Qnil;
  describe_vector (map, keysdesc, describe_command,
		   partial, shadow, chartab, mice_only_p);
  UNGCPRO;
}

struct describe_vector_closure {
  Lisp_Object *list;	 /* pointer to the list to update */
  Lisp_Object partial;	 /* whether to ignore suppressed commands */
  Lisp_Object shadow;	 /* the map that shadows this one */
  Lisp_Object self;	 /* this map */
  Lisp_Object self_root; /* this map, or some map that has this map as
			    a parent.  this is the base of the tree */
  int mice_only_p;	 /* whether we are to display only button bindings */
};

static void
describe_vector_mapper (key, bits, binding, junk)
     Lisp_Object key, binding;
     int bits;
     void *junk;
{
  Lisp_Object sh;
  struct describe_vector_closure *closure =
    (struct describe_vector_closure *) junk;

  /* Dont mention suppressed commands.  */
  if (SYMBOLP (binding) &&
      !NILP (closure->partial) &&
      !NILP (Fget (binding, closure->partial)))
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
	  if (!NILP (keymap_lookup (sh, key, bits)))
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
      if (!NILP (keymap_lookup (sh, key, bits)))
	return;
    }

  /* Otherwise add it to the list to be sorted. */
  *(closure->list) = Fcons (Fcons (Fcons (key, bits), binding),
			    *(closure->list));
}


static int
describe_vector_sort_predicate (obj1, obj2, pred)
     Lisp_Object obj1, obj2, pred;
{
  /* obj1 and obj2 are conses of the form
     ( ( <keysym> . <bits> ) . <binding> )
     keysym and bits are used, binding is ignored.
   */
  int bit1, bit2;
  obj1 = XCONS (obj1)->car;
  obj2 = XCONS (obj2)->car;
  bit1 = XFASTINT (XCONS (obj1)->cdr);
  bit2 = XFASTINT (XCONS (obj2)->cdr);
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
elide_next_two_p (list)
     Lisp_Object list;
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
      Lisp_Object code = Fget (s1, Vcharacter_set_property);
      if (FIXNUMP (code)) s1 = code;
      else return 0;
    }
  if (SYMBOLP (s2))
    {
      Lisp_Object code = Fget (s2, Vcharacter_set_property);
      if (FIXNUMP (code)) s2 = code;
      else return 0;
    }

  if (XFASTINT (s1) == XFASTINT (s2) ||
      XFASTINT (s1) + 1 == XFASTINT (s2))
    return 1;
  return 0;

#undef CDR
#undef CAR
}


static int
describe_vector (keymap, elt_prefix, elt_describer, partial, shadow, chartab,
		 mice_only_p)
     Lisp_Object keymap;
     Lisp_Object elt_prefix;
     int (*elt_describer) ();
     int partial;
     Lisp_Object shadow;
     Lisp_Object chartab;
     int mice_only_p;
{
  struct describe_vector_closure closure;
  Lisp_Object list = Qnil;
  keymap = get_keymap (keymap);
  closure.partial = (partial ? intern ("suppress-keymap") : Qnil);
  closure.shadow = shadow;
  closure.list = &list;
  closure.self_root = keymap;
  closure.mice_only_p = mice_only_p;

  if (!NILP (chartab))
    CHECK_VECTOR (chartab, 0);

  for (; !NILP (Fkeymapp (keymap)); keymap = XKEYMAP (keymap)->parent)
    {
      closure.self = get_keymap (keymap);
      map_keymap (XKEYMAP (closure.self),
		  0, /* don't sort: we'll do it later */
		  describe_vector_mapper, (void *) &closure);
    }

  list = list_sort (list, Qnil, describe_vector_sort_predicate);

  insert_raw_string ("\n", 1);
  while (!NILP (list)) {
    Lisp_Object keysym = XCONS (XCONS (XCONS (list)->car)->car)->car;
    int modifiers = XCONS (XCONS (XCONS (list)->car)->car)->cdr;
    if (!NILP (elt_prefix))
      insert_relocatable_raw_string ((char *) XSTRING (elt_prefix)->data,
				     XSTRING (elt_prefix)->size,
				     elt_prefix);

    if (modifiers & MOD_META)    insert_raw_string ("M-", 2);
    if (modifiers & MOD_CONTROL) insert_raw_string ("C-", 2);
    if (modifiers & MOD_SUPER)   insert_raw_string ("S-", 2);
    if (modifiers & MOD_HYPER)   insert_raw_string ("H-", 2);
    if (modifiers & MOD_SYMBOL)  insert_raw_string ("Sym-", 4);
    if (modifiers & MOD_SHIFT)   insert_raw_string ("Sh-", 3);
    switch (XTYPE (keysym)) {
    case Lisp_Symbol:
      /* Calling Fsingle_key_description() would cons more */
      if (EQ (keysym, QKlinefeed))	insert_raw_string ("LFD", 3);
      else if (EQ (keysym, QKtab))	insert_raw_string ("TAB", 3);
      else if (EQ (keysym, QKreturn))	insert_raw_string ("RET", 3);
      else if (EQ (keysym, QKescape))	insert_raw_string ("ESC", 3);
      else if (EQ (keysym, QKdelete))	insert_raw_string ("DEL", 3);
      else if (EQ (keysym, QKspace))	insert_raw_string ("SPC", 3);
      else if (EQ (keysym, QKbackspace)) insert_raw_string ("BS", 2);
      else insert_raw_string ((char *) XSYMBOL (keysym)->name->data,
			      XSYMBOL (keysym)->name->size);
      break;
    case Lisp_Int:
      {
	char string [1];
	string [0] = XFASTINT (keysym);
	insert_raw_string (string, 1);
	break;
      }
    default:
      insert_string ("---bad keysym---");
    }

    {
      int k = 0;

      while (elide_next_two_p (list))
	k++, list = XCONS (list)->cdr;
      
      if (k)
	{
	  if (k == 1)
	    insert_raw_string (", ", 2);
	  else
	    insert_raw_string (" .. ", 4);
	  continue;
	}
    }
    
    /* Print a description of the definition of this character.  */
    (*elt_describer) (XCONS (XCONS (list)->car)->cdr);
    list = XCONS (list)->cdr;
  }
  return 0;
}


/* Apropos */
static Lisp_Object apropos_predicate;
static Lisp_Object apropos_accumulate;

static void
apropos_accum (symbol, string)
     Lisp_Object symbol, string;
{
  register Lisp_Object tem;

  tem = Fstring_match (string, Fsymbol_name (symbol), Qnil);
  if (!NILP (tem) && !NILP (apropos_predicate))
    tem = call1 (apropos_predicate, symbol);
  if (!NILP (tem))
    apropos_accumulate = Fcons (symbol, apropos_accumulate);
}

DEFUN ("apropos-internal", Fapropos_internal, Sapropos_internal, 1, 2, 0, 
  "Show all symbols whose names contain match for REGEXP.\n\
If optional 2nd arg PRED is non-nil, (funcall PRED SYM) is done\n\
for each symbol and a symbol is mentioned only if that returns non-nil.\n\
Return list of symbols found.")
  (string, pred)
     Lisp_Object string, pred;
{
  struct gcpro gcpro1, gcpro2;
  CHECK_STRING (string, 0);
  apropos_predicate = pred;
  GCPRO2 (apropos_predicate, apropos_accumulate);
  apropos_accumulate = Qnil;
  map_obarray (Vobarray, apropos_accum, string);
  apropos_accumulate = Fsort (apropos_accumulate, Qstring_lessp);
  UNGCPRO;
  return apropos_accumulate;
}

void
syms_of_keymap ()
{
  DEFVAR_INT ("meta-prefix-char", &meta_prefix_char,
    "Meta-prefix character code.  Must be an ASCII integer.\n\
This character followed by some character `foo' turns into `Meta-foo'.\n\
To disable the meta-prefix-char, set it to a negative number.");
  meta_prefix_char = 033;

  DEFVAR_INT ("keymap-tick", &keymap_tick,
	      "Incremented for each change to any keymap.");
  keymap_tick = 0;

  Qsingle_key_description = intern ("single-key-description");
  staticpro (&Qsingle_key_description);

  Qkey_description = intern ("key-description");
  staticpro (&Qkey_description);

  Qkeymapp = intern ("keymapp");
  staticpro (&Qkeymapp);

  staticpro (&Vcurrent_global_map);

  defsubr (&Skeymapp);
  defsubr (&Smake_keymap);
  defsubr (&Smake_sparse_keymap);
  defsubr (&Skeymap_parent);
  defsubr (&Sset_keymap_parent);
  defsubr (&Sset_keymap_name);
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
  defsubr (&Stext_char_description);
  defsubr (&Swhere_is_internal);
  defsubr (&Sdescribe_bindings);
  defsubr (&Sapropos_internal);

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

  /* no need to staticpro these because where_is_internal and lookup_key
     do not eval, and do not use the data in these after they exit.
   */
  keys_so_far_total_size = 50;
  keys_so_far = (struct raw_key *)
    xmalloc (sizeof (struct raw_key) * keys_so_far_total_size);

  lookup_key_buf_total_size = 50;
  lookup_key_buf = (struct raw_key *)
    xmalloc (sizeof (struct raw_key) * keys_so_far_total_size);
}

void
keys_of_keymap ()
{
  Lisp_Object ESC_prefix = intern ("ESC-prefix");
  Vcurrent_global_map = Fmake_keymap ();
  Ffset (ESC_prefix, Fmake_keymap ());
  keymap_store (Vcurrent_global_map, Qmeta, 0, ESC_prefix);
}
