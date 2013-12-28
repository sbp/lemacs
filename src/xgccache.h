/* Efficient caching of X GCs (graphics contexts).
   Copyright (C) 1993 Free Software Foundation, Inc.

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

/* Written by jwz, 14 jun 93 */

#ifndef _XGCCACHE_H_
#define _XGCCACHE_H_

struct gc_cache;
struct gc_cache *make_gc_cache (Display *, Window);
void free_gc_cache (struct gc_cache *cache);
GC gc_cache_lookup (struct gc_cache *, XGCValues *, unsigned long mask);

/* #### Once we support multiple displays, this should be gotten via the
   screen object (in some per-display area shared among multiple screens.) */
extern struct gc_cache *the_gc_cache;

#endif /* _XGCCACHE_H_ */
