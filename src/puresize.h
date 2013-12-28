/* Definition of PURESIZE.
   Copyright (C) 1986, 1988, 1992, 1993 Free Software Foundation, Inc.

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

#ifndef PURESIZE_H
#define PURESIZE_H

/* # bytes of pure Lisp code to leave space for.
   Note that s-vms.h, s-aix3-2.h, m-sun2.h and m-sparc.h may override
   this default.
 */

#ifndef PURESIZE
# ifdef ENERGIZE
#  define PURESIZE 570000
# else
#  define PURESIZE 290000
# endif
#endif

#endif /* PURESIZE_H */
