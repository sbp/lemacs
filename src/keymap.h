/* keymap-hacking prototypes
   Copyright (C) 1992, 1993 Free Software Foundation, Inc.

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


#ifndef _EMACS_KEYMAP_H_
#define _EMACS_KEYMAP_H_

extern const struct lrecord_implementation lrecord_keymap[];

#define CHECK_KEYMAP(x, i) CHECK_RECORD ((x), lrecord_keymap, Qkeymapp, (i))
#define KEYMAPP(x) RECORD_TYPEP ((x), lrecord_keymap)

extern Lisp_Object Qkeymapp;

extern Lisp_Object get_keymap (Lisp_Object object, int error, int autoload);
extern Lisp_Object lookup_keys (Lisp_Object keymap, 
                                int nkeys, Lisp_Object *);
extern Lisp_Object lookup_events (Lisp_Object event_head,
                                  int nmaps, Lisp_Object maps[]);

extern Lisp_Object Fkey_description (Lisp_Object keys);
extern Lisp_Object Fsingle_key_description (Lisp_Object key);
extern Lisp_Object Fwhere_is_internal (Lisp_Object definition, 
                                       Lisp_Object local_keymap,
                                       Lisp_Object firstonly,
                                       Lisp_Object global_keymap,
                                       Lisp_Object noindirect);

extern Lisp_Object Fkeymap_name (Lisp_Object keymap);
extern Lisp_Object Fset_keymap_name (Lisp_Object keymap, Lisp_Object name);
extern Lisp_Object Fkeymap_prompt (Lisp_Object keymap, Lisp_Object inherit);
extern Lisp_Object Fset_keymap_prompt (Lisp_Object keymap, Lisp_Object prompt);

extern int interrupt_char;
extern Lisp_Object help_char;
extern int meta_prefix_char;

#endif /* _EMACS_KEYMAP_H_ */
