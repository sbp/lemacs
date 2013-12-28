/* Define scrollbar instance.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.

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

#ifndef _EMACS_SCROLLBAR_H_
#define _EMACS_SCROLLBAR_H_

#ifdef HAVE_X_WINDOWS

#include "xintrinsic.h"
#include "lwlib.h"

struct scrollbar_instance
{
  /* Window scrollbar is "attached" to. */
  Lisp_Object window;

  /* Pointer to the scrollbar widget this structure describes. */
  Widget scrollbar_widget;

  /* Unique scrollbar identifier and name. */
  unsigned int scrollbar_id;
  char *scrollbar_name;

  /* This flag indicates if the scrollbar is currently in use. */
  char scrollbar_is_active;

  /* This flag indicates if a data parameter has changed. */
  char scrollbar_instance_changed;

  /* Positioning and sizing information for scrollbar and slider. */
  scrollbar_values data;

  /* This points to the next scrollbar. */
  struct scrollbar_instance *next;
};

extern int scrollbar_width;
extern void mark_scrollbar (struct scrollbar_instance *instance,
			    void (*markobj) (Lisp_Object));

#endif /* HAVE_X_WINDOWS */
#endif /* _EMACS_SCROLLBAR_H_ */
