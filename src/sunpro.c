/* Sunpro-specific routines.

   Copyright (C) 1994 Sun Microsystems, Inc.

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
#include "lisp.h"
#include "intl.h"

/* ####

  The following junk used to be in lisp/prim/files.el.  It obviously
  doesn't belong there, but should go somewhere.

  (if (fboundp 'ut-log-text)	;; #### Sun stuff; what is this?
      (ut-log-text "Reading a file."))
*/

/* Whether usage tracking is turned on (Sun only) */
Lisp_Object Vusage_tracking;
#ifdef USAGE_TRACKING
#include <ut.h>
#endif

DEFUN  ("ut-log-text", 
	Fut_log_text, 
	Sut_log_text, 
	1, MANY, 0,
	"Log a usage-tracking message if `usage-tracking' is non-nil.\n\
Args are the same as to `format'.  Returns whether the message was\n\
actually logged.  If usage-tracking support was not compiled in, this\n\
function has no effect and always returns `nil'.  See function\n\
`has-usage-tracking-p'.")
     (nargs, args)
     int nargs;
     Lisp_Object *args;
{
#ifdef USAGE_TRACKING
  Lisp_Object xs;
  unsigned char *s; /* #### Does not support I18N4. */

  if (!NILP (Vusage_tracking)) {
    xs = Fformat (nargs, args);
    CHECK_STRING (xs, 0);
    s = XSTRING (xs)->data;
    ut_log_text((char *) s);
  }
  return Vusage_tracking;
#else
  return Qnil;
#endif
}

DEFUN ("has-usage-tracking-p", Fhas_usage_tracking_p,
       Shas_usage_tracking_p, 0, 0, 0,
       "Whether usage tracking support has been compiled in.")
     (void)
{
#ifdef USAGE_TRACKING
  return Qt;
#else
  return Qnil;
#endif
}

void
init_sunpro(void)
{
#ifdef USAGE_TRACKING
  if (!purify_flag) {	       /* Enabled only when not dumping an executable */
    Vusage_tracking = Qt;
    ut_initialize("xemacs", NULL, NULL);
  } else {
    Vusage_tracking = Qnil;
  }
#else
  Vusage_tracking = Qnil;
#endif
}

void
syms_of_sunpro(void)
{
  defsubr (&Sut_log_text);
  defsubr (&Shas_usage_tracking_p);

  DEFVAR_LISP ("usage-tracking", &Vusage_tracking,
    "Whether usage tracking is turned on (Sun only).\n\
Has no effect if usage tracking support has not been compiled in.");
  Vusage_tracking = Qnil;
}
