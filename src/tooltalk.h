/* ToolTalk Interface.
   Copyright (C) 1993 Sun Microsystems, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#ifndef _EMACS_TOOLTALK_H_
#define _EMACS_TOOLTALK_H_

struct Lisp_Tooltalk_Message;
extern CONST struct lrecord_implementation lrecord_tooltalk_message[];
#define XTOOLTALK_MESSAGE(a) ((struct Lisp_Tooltalk_Message *) XPNTR(a))
#define CHECK_TOOLTALK_MESSAGE(x, i) CHECK_RECORD ((x), lrecord_tooltalk_message, Qtooltalk_messagep, (i))
#define TOOLTALK_MESSAGEP(x) RECORD_TYPEP ((x), lrecord_tooltalk_message)
extern Lisp_Object Qtooltalk_messagep;

struct Lisp_Tooltalk_Pattern;
extern CONST struct lrecord_implementation lrecord_tooltalk_pattern[];
#define XTOOLTALK_PATTERN(a) ((struct Lisp_Tooltalk_Pattern *) XPNTR(a))
#define CHECK_TOOLTALK_PATTERN(x, i) CHECK_RECORD ((x), lrecord_tooltalk_pattern, Qtooltalk_patternp, (i))
#define TOOLTALK_PATTERNP(x) RECORD_TYPEP ((x), lrecord_tooltalk_pattern)
extern Lisp_Object Qtooltalk_patternp;

#define TOOLTALK_CALLBACK_KEY 100
#define TOOLTALK_PLIST_KEY 101

#define XSTRING_DATA(s) \
  ((XSTRING(s)->size > 0) ? (char *)(XSTRING(s)->data) : "")

#define CHECK_TOOLTALK_CONSTANT(x, i) \
  { if ((!FIXNUMP(x)) && (!SYMBOLP(x))) x = wrong_type_argument (Qsymbolp, (x)); }

#define VALID_TOOLTALK_MESSAGEP(m) \
   (m && (tt_ptr_error(m) == TT_OK))

#define VALID_TOOLTALK_PATTERNP(p) \
   (p && (tt_ptr_error(p) == TT_OK))

extern Lisp_Object box_tooltalk_message(Tt_message m);
extern Tt_message unbox_tooltalk_message(Lisp_Object msg);

extern Lisp_Object Qtooltalk_error;

#endif /* _EMACS_TOOLTALK_H_ */

