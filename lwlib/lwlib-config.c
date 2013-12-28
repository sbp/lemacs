/* Flags indicating how lwlib was compiled.
   Copyright (C) 1994 Lucid, Inc.

This file is part of the Lucid Widget Library.

The Lucid Widget Library is free software; you can redistribute it and/or 
modify it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

The Lucid Widget Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of 
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* This is a kludge to make sure emacs can only link against a version of
   lwlib that was compiled in the right way.  Emacs references symbols which
   correspond to the way it thinks lwlib was compiled, and if lwlib wasn't
   compiled in that way, then somewhat meaningful link errors will result.
   The alternatives to this range from obscure link errors, to obscure
   runtime errors that look a lot like bugs.
 */

#include "lwlib.h"

#include <X11/Xlib.h>	/* to get XlibSpecificationRelease */
#ifdef USE_MOTIF
#include <Xm/Xm.h>	/* to get XmVersion */
#endif

#if (XlibSpecificationRelease == 5)
int lwlib_uses_x11r5;
#else
int lwlib_does_not_use_x11r5;
#endif

#ifdef USE_LUCID
int lwlib_uses_lucid;
#else
int lwlib_does_not_use_lucid;
#endif

#ifdef USE_MOTIF
int lwlib_uses_motif;
#else
int lwlib_does_not_use_motif;
#endif

#if (XmVersion >= 1002)
int lwlib_uses_motif_1_2;
#else
int lwlib_does_not_use_motif_1_2;
#endif

#ifdef USE_OLIT
int lwlib_uses_olit;
#else
int lwlib_does_not_use_olit;
#endif

#ifdef USE_XAW
int lwlib_uses_xaw;
#else
int lwlib_does_not_use_xaw;
#endif

#ifdef ENERGIZE
int lwlib_uses_energize;
#else
int lwlib_does_not_use_energize;
#endif
