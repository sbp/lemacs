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

extern void make_face_ready (struct screen* s, struct face* face);
extern void flush_face_cache ();
extern void setup_extent_fragment_face_ptr (struct screen *s, EXTENT_FRAGMENT extfrag);
extern void ensure_face_ready (struct screen* s, int f);
extern void init_screen_faces (struct screen *s);
