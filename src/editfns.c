/* Lisp functions pertaining to editing.
   Copyright (C) 1985, 1986, 1987, 1989, 1992 Free Software Foundation, Inc.

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
#include <pwd.h>
#include "lisp.h"
#include "buffer.h"
#include "window.h"

#ifdef NEED_TIME_H
#include <time.h>
#else /* not NEED_TIME_H */
#ifdef HAVE_TIMEVAL
#include <sys/time.h>
#endif /* HAVE_TIMEVAL */
#endif /* not NEED_TIME_H */

#define min(a, b) ((a) < (b) ? (a) : (b))
#define max(a, b) ((a) > (b) ? (a) : (b))

/* Some static data, and a function to initialize it for each run */

Lisp_Object Vsystem_name;
Lisp_Object Vuser_real_name;	/* login name of current user ID */
Lisp_Object Vuser_full_name;	/* full name of current user */
Lisp_Object Vuser_name;		/* user name from USER or LOGNAME.  */

void
init_editfns ()
{
  unsigned char *user_name;
  register unsigned char *p, *q, *r;
  struct passwd *pw;	/* password entry for the current user */
  extern char *index ();
  Lisp_Object tem;

  /* Set up system_name even when dumping.  */

  Vsystem_name = build_string (get_system_name ());
  p = XSTRING (Vsystem_name)->data;
  while (*p)
    {
      if (*p == ' ' || *p == '\t')
	*p = '-';
      p++;
    }

#ifndef CANNOT_DUMP
  /* Don't bother with this on initial start when just dumping out */
  if (!initialized)
    return;
#endif /* not CANNOT_DUMP */

  pw = (struct passwd *) getpwuid (getuid ());
  Vuser_real_name = build_string (pw ? pw->pw_name : "unknown");

  user_name = (unsigned char *) getenv ("USER");
  if (!user_name)
    user_name = (unsigned char *) getenv ("LOGNAME");
  if (user_name)
    Vuser_name = build_string (user_name);
  else
    Vuser_name = Vuser_real_name;

  tem = Fstring_equal (Vuser_name, Vuser_real_name);
  if (!NILP (tem))
    pw = (struct passwd *) getpwnam ((char *) user_name);
  
  p = (unsigned char *) (pw ? USER_FULL_NAME : "unknown");
  q = (unsigned char *) index (p, ',');
  Vuser_full_name = make_string (p, q ? q - p : strlen ((char*)p));
  
#ifdef AMPERSAND_FULL_NAME
  p = XSTRING (Vuser_full_name)->data;
  q = (unsigned char *) index (p, '&');
  /* Substitute the login name for the &, upcasing the first character.  */
  if (q)
    {
      r = (unsigned char *) alloca (strlen (p) +
				    XSTRING (Vuser_name)->size + 1);
      bcopy (p, r, q - p);
      r[q - p] = 0;
      strcat (r, (char *) XSTRING (Vuser_name)->data);
      r[q - p] = UPCASE (r[q - p]);
      strcat (r, q + 1);
      Vuser_full_name = build_string (r);
    }
#endif /* AMPERSAND_FULL_NAME */
}

DEFUN ("char-to-string", Fchar_to_string, Schar_to_string, 1, 1, 0,
  "Convert arg CHAR to a one-character string containing that character.")
  (n)
     Lisp_Object n;
{
  char c;
  CHECK_NUMBER (n, 0);

  c = XINT (n);
  return make_string (&c, 1);
}

DEFUN ("string-to-char", Fstring_to_char, Sstring_to_char, 1, 1, 0,
  "Convert arg STRING to a character, the first character of that string.")
  (str)
     register Lisp_Object str;
{
  register Lisp_Object val;
  register struct Lisp_String *p;
  CHECK_STRING (str, 0);

  p = XSTRING (str);
  if (p->size)
    XFASTINT (val) = ((unsigned char *) p->data)[0];
  else
    XFASTINT (val) = 0;
  return val;
}

static Lisp_Object
buildmark (val)
     int val;
{
  register Lisp_Object mark;
  mark = Fmake_marker ();
  Fset_marker (mark, make_number (val), Qnil);
  return mark;
}

DEFUN ("point", Fpoint, Spoint, 0, 0, 0,
  "Return value of point, as an integer.\n\
Beginning of buffer is position (point-min)")
  ()
{
  Lisp_Object temp;
  XFASTINT (temp) = point;
  return temp;
}

DEFUN ("point-marker", Fpoint_marker, Spoint_marker, 0, 1, 0,
   "Return value of point, as a marker object.\n\
This marker is a copy; you may modify it with reckless abandon.\n\
If the argument to this function is non-nil, then it returns the real\n\
point-marker; modifying the position of this marker willl move point.\n\
It is illegal to change the buffer of it, or make it point nowhere.")
  (dont_copy_p)
{
  if (NILP (dont_copy_p))
    return Fcopy_marker (current_buffer->point_marker);
  return current_buffer->point_marker;
}

static int
in_accessible_range (buf, loc)
     struct buffer *buf;
     int loc;
{
  if (loc < BUF_BEGV (buf))
    return BUF_BEGV (buf);

  if (loc > BUF_ZV (buf))
    return BUF_ZV (buf);

  return loc;
}

DEFUN ("goto-char", Fgoto_char, Sgoto_char, 1, 1, "NGoto char: ",
  "Set point to POSITION, a number or marker.\n\
Beginning of buffer is position (point-min), end is (point-max).")
  (n)
     register Lisp_Object n;
{
  CHECK_NUMBER_COERCE_MARKER (n, 0);

  SET_PT (in_accessible_range (current_buffer, XINT (n)));
  return n;
}

static Lisp_Object
region_limit (beginningp)
     int beginningp;
{
  register Lisp_Object m;

  m = Fmarker_position (current_buffer->mark);
  if (NILP (m)) error ("There is no region now");
  if ((point < XFASTINT (m)) == beginningp)
    return (make_number (point));
  else
    return (m);
}

DEFUN ("region-beginning", Fregion_beginning, Sregion_beginning, 0, 0, 0,
  "Return position of beginning of region, as an integer.")
  ()
{
  return (region_limit (1));
}

DEFUN ("region-end", Fregion_end, Sregion_end, 0, 0, 0,
  "Return position of end of region, as an integer.")
  ()
{
  return (region_limit (0));
}

/* Whether to use lispm-style active-regions */
int zmacs_regions;

/* Whether the region is currently active.  This is not per-buffer because
   there can be only one active region at a time (since it does double-duty
   as the X primary selection.)
 */
int zmacs_region_active_p;

extern Lisp_Object Vrun_hooks;

int zmacs_region_stays;
Lisp_Object Vzmacs_update_region_hook,     Qzmacs_update_region_hook;
Lisp_Object Vzmacs_activate_region_hook,   Qzmacs_activate_region_hook;
Lisp_Object Vzmacs_deactivate_region_hook, Qzmacs_deactivate_region_hook;

DEFUN ("zmacs-activate-region", Fzmacs_activate_region,
       Szmacs_activate_region, 0, 0, 0,
 "Make the region between `point' and `mark' be in the active (hilighted)\n\
state, if `zmacs-regions' is true.  Only a very small number of commands\n\
should ever do this.")
    ()
{
  if (! zmacs_regions) return Qnil;
  zmacs_region_active_p++;
  if (!NILP (Vrun_hooks) && zmacs_region_active_p == 1)
    call1 (Vrun_hooks, Qzmacs_activate_region_hook);
  return Qt;
}


DEFUN ("zmacs-deactivate-region", Fzmacs_deactivate_region,
       Szmacs_deactivate_region, 0, 0, 0,
 "Make the region between `point' and `mark' no longer be in the active\n\
(hilighted) state, if `zmacs-regions' is true.  You shouldn't need to call\n\
this; the command loop calls it when appropriate.")
    ()
{
  if (! zmacs_region_active_p) return Qnil;
  zmacs_region_active_p = 0;
  if (!NILP (Vrun_hooks)) call1 (Vrun_hooks, Qzmacs_deactivate_region_hook);
  return Qnil;
}


void
zmacs_update_region ()
{
  if (zmacs_region_active_p && !NILP (Vrun_hooks))
    call1 (Vrun_hooks, Qzmacs_update_region_hook);
}


DEFUN ("mark-marker", Fmark_marker, Smark_marker, 0, 1, 0,
  "Return this buffer's mark, as a marker object.\n\
If `zmacs-regions' is true, then this returns nil unless the region is\n\
currently in the active (hilighted) state.  With an argument of t, this\n\
returns the mark (if there is one) regardless of the zmacs-region state.\n\
You should *generally* not use the mark unless the region is active, if\n\
the user has expressed a preference for the zmacs-region model.\n\
Watch out!  Moving this marker changes the mark position.\n\
If you set the marker not to point anywhere, the buffer will have no mark.")
  (inactive_p)
    Lisp_Object inactive_p;
{
  if (! zmacs_regions || zmacs_region_active_p || !NILP (inactive_p))
    return current_buffer->mark;
  return Qnil;
}

Lisp_Object
save_excursion_save ()
{
  register int visible = XBUFFER (XWINDOW (selected_window)->buffer) == current_buffer;

  if (XINT (Fpoint ()) != XINT (Fmarker_position (Fpoint_marker (Qt))))
    abort ();
  return Fcons (Fpoint_marker (Qnil),
		Fcons (Fcopy_marker (current_buffer->mark), visible
		       ? Qt : Qnil));
}

Lisp_Object
save_excursion_restore (info)
     register Lisp_Object info;
{
  register Lisp_Object tem;

  tem = Fmarker_buffer (Fcar (info));
  /* If buffer being returned to is now deleted, avoid error */
  /* Otherwise could get error here while unwinding to top level
     and crash */
  /* In that case, Fmarker_buffer returns nil now.  */
  if (NILP (tem))
    return Qnil;
  Fset_buffer (tem);
  tem = Fcar (info);
  Fgoto_char (tem);
  unchain_marker (tem);
  tem = Fcar (Fcdr (info));
  Fset_marker (current_buffer->mark, tem, Fcurrent_buffer ());
  unchain_marker (tem);
  tem = Fcdr (Fcdr (info));
  if (!NILP (tem) && current_buffer != XBUFFER (XWINDOW (selected_window)->buffer))
    Fswitch_to_buffer (Fcurrent_buffer (), Qnil);
  return Qnil;
}

DEFUN ("save-excursion", Fsave_excursion, Ssave_excursion, 0, UNEVALLED, 0,
  "Save point (and mark), execute BODY, then restore point and mark.\n\
Executes BODY just like `progn'.  Point and mark values are restored\n\
even in case of abnormal exit (throw or error).")
  (args)
     Lisp_Object args;
{
  register Lisp_Object val;
  int count = specpdl_ptr - specpdl;

  record_unwind_protect (save_excursion_restore, save_excursion_save ());
			 
  val = Fprogn (args);
  unbind_to (count);
  return val;
}

DEFUN ("buffer-size", Fbufsize, Sbufsize, 0, 0, 0,
  "Return the number of characters in the current buffer.")
  ()
{
  Lisp_Object temp;
  XFASTINT (temp) = Z - BEG;
  return temp;
}

DEFUN ("point-min", Fpoint_min, Spoint_min, 0, 0, 0,
  "Return the minimum permissible value of point in the current buffer.\n\
This is 1, unless a clipping restriction is in effect.")
  ()
{
  Lisp_Object temp;
  XFASTINT (temp) = BEGV;
  return temp;
}

DEFUN ("point-min-marker", Fpoint_min_marker, Spoint_min_marker, 0, 0, 0,
  "Return a marker to the minimum permissible value of point in this buffer.\n\
This is the beginning, unless a clipping restriction is in effect.")
  ()
{
  return buildmark (BEGV);
}

DEFUN ("point-max", Fpoint_max, Spoint_max, 0, 0, 0,
  "Return the maximum permissible value of point in the current buffer.\n\
This is (1+ (buffer-size)), unless a clipping restriction is in effect,\n\
in which case it is less.")
  ()
{
  Lisp_Object temp;
  XFASTINT (temp) = ZV;
  return temp;
}

DEFUN ("point-max-marker", Fpoint_max_marker, Spoint_max_marker, 0, 0, 0,
  "Return a marker to the maximum permissible value of point in this buffer.\n\
This is (1+ (buffer-size)), unless a clipping restriction is in effect,\n\
in which case it is less.")
  ()
{
  return buildmark (ZV);
}

DEFUN ("following-char", Ffollchar, Sfollchar, 0, 0, 0,
  "Return the character following point, as a number.")
  ()
{
  Lisp_Object temp;
  XFASTINT (temp) = CHAR_AT (point);
  return temp;
}

DEFUN ("preceding-char", Fprevchar, Sprevchar, 0, 0, 0,
  "Return the character preceding point, as a number.")
  ()
{
  Lisp_Object temp;
  if (point <= BEGV)
    XFASTINT (temp) = 0;
  else
    XFASTINT (temp) = CHAR_AT (point - 1);
  return temp;
}

DEFUN ("bobp", Fbobp, Sbobp, 0, 0, 0,
  "Return T if point is at the beginning of the buffer.\n\
If the buffer is narrowed, this means the beginning of the narrowed part.")
  ()
{
  if (point == BEGV)
    return Qt;
  return Qnil;
}

DEFUN ("eobp", Feobp, Seobp, 0, 0, 0,
  "Return T if point is at the end of the buffer.\n\
If the buffer is narrowed, this means the end of the narrowed part.")
  ()
{
  if (point == ZV)
    return Qt;
  return Qnil;
}

DEFUN ("bolp", Fbolp, Sbolp, 0, 0, 0,
  "Return T if point is at the beginning of a line.")
  ()
{
  if (point == BEGV || CHAR_AT (point - 1) == '\n')
    return Qt;
  return Qnil;
}

DEFUN ("eolp", Feolp, Seolp, 0, 0, 0,
  "Return T if point is at the end of a line.\n\
`End of a line' includes point being at the end of the buffer.")
  ()
{
  if (point == ZV || CHAR_AT (point) == '\n')
    return Qt;
  return Qnil;
}

DEFUN ("char-after", Fchar_after, Schar_after, 1, 1, 0,
  "Return character in current buffer at position POS.\n\
POS is an integer or a buffer pointer.\n\
If POS is out of range, the value is nil.")
  (pos)
     Lisp_Object pos;
{
  register Lisp_Object val;
  register int n;

  CHECK_NUMBER_COERCE_MARKER (pos, 0);

  n = XINT (pos);
  if (n < BEGV || n >= ZV) return Qnil;

  XFASTINT (val) = CHAR_AT (n);
  return val;
}

DEFUN ("user-login-name", Fuser_login_name, Suser_login_name, 0, 0, 0,
  "Return the name under which the user logged in, as a string.\n\
This is based on the effective uid, not the real uid.\n\
Also, if the environment variable USER or LOGNAME is set,\n\
that determines the value of this function.")
  ()
{
  return Vuser_name;
}

DEFUN ("user-real-login-name", Fuser_real_login_name, Suser_real_login_name,
  0, 0, 0,
  "Return the name of the user's real uid, as a string.\n\
Differs from `user-login-name' when running under `su'.")
  ()
{
  return Vuser_real_name;
}

DEFUN ("user-uid", Fuser_uid, Suser_uid, 0, 0, 0,
  "Return the effective uid of Emacs, as an integer.")
  ()
{
  return make_number (geteuid ());
}

DEFUN ("user-real-uid", Fuser_real_uid, Suser_real_uid, 0, 0, 0,
  "Return the real uid of Emacs, as an integer.")
  ()
{
  return make_number (getuid ());
}

DEFUN ("user-full-name", Fuser_full_name, Suser_full_name, 0, 0, 0,
  "Return the full name of the user logged in, as a string.")
  ()
{
  return Vuser_full_name;
}

DEFUN ("system-name", Fsystem_name, Ssystem_name, 0, 0, 0,
  "Return the name of the machine you are running on, as a string.")
  ()
{
  return Vsystem_name;
}

/* Lucid addition
 */

DEFUN ("current-time-seconds", Fcurrent_time_seconds, Scurrent_time_seconds,
       0, 1, 0,
"Returns the current time as the number of seconds since Jan. 1, 1970, 00:00,\n\
Greenwich Mean Time.  Since emacs can't represent integers that large, this\n\
is returned as a cons of two 16-bit numbers.  You can pass this value as an\n\
argument to current-time-string to get that time as a string.")
   (cons)
   Lisp_Object cons;
{
  unsigned int top, bot;
  long now = time ((long *) 0);
  top = (now >> 16) & 0xFFFF;
  bot = now & 0xFFFF;
  XSET (top, Lisp_Int, top);
  XSET (bot, Lisp_Int, bot);
  if (NILP (cons))
    return Fcons (top, bot);
  CHECK_CONS (cons, 0);
  Fsetcar (cons, top);
  Fsetcdr (cons, bot);
  return cons;
}

/* Lucid change
   made this take an optional arg, the time to grind.
 */
DEFUN ("current-time-string", Fcurrent_time_string, Scurrent_time_string, 0, 1, 0,
  "Return the current time, as a human-readable string.\n\
Programs can use it too, since the number of columns in each field is fixed.\n\
The format is `Sun Sep 16 01:03:52 1973'.\n\
If the optional argument is non-nil, it must be a cons of two integers, which\n\
are the upper and lower 16 bits of a time in seconds (such as the value \n\
returned by the current-time-seconds function.\n\
In a future Emacs version, the time zone may be added at the end,\n\
if we can figure out a reasonably easy way to get that information.")
  (arg)
{
  long now;
  char buf[30];
  register char *tem;

  if (NILP (arg))
    now = time ((long *) 0);
  else {
    int top, bottom;
    CHECK_CONS (arg, 1);
    top = Fcar(arg), bottom = Fcdr(arg);
    if (XTYPE (bottom) == Lisp_Cons) bottom = Fcar (bottom);
    CHECK_NUMBER (top, 1);
    CHECK_NUMBER (bottom, 1);
    now = (XINT (top) << 16) | XINT (bottom);
  }
  tem = (char *) ctime (&now);

  strncpy (buf, tem, 24);
  buf[24] = 0;

  return build_string (buf);
}

#ifdef unix

DEFUN ("set-default-file-mode", Fset_default_file_mode, Sset_default_file_mode, 1, 1, "p",
  "Set Unix `umask' value to ARGUMENT, and return old value.\n\
The `umask' value is the default protection mode for new files.")
  (nmask)
     Lisp_Object nmask;
{
  CHECK_NUMBER (nmask, 0);
  return make_number (umask (XINT (nmask)));
}

DEFUN ("unix-sync", Funix_sync, Sunix_sync, 0, 0, "",
  "Tell Unix to finish all pending disk updates.")
  ()
{
  sync ();
  return Qnil;
}

#endif /* unix */

void
insert1 (arg)
     Lisp_Object arg;
{
  Finsert (1, &arg);
}


DEFUN ("insert", Finsert, Sinsert, 0, MANY, 0,
  "Insert the arguments, either strings or characters, at point.\n\
Point moves forward so that it ends up after the inserted text.\n\
Any other markers at the point of insertion remain before the text.")
  (nargs, args)
     int nargs;
     register Lisp_Object *args;
{
  register int argnum;
  register Lisp_Object tem;
  char str[1];

  for (argnum = 0; argnum < nargs; argnum++)
    {
      tem = args[argnum];
    retry:
      if (XTYPE (tem) == Lisp_Int)
	{
	  str[0] = XINT (tem);
	  insert_raw_string (str, 1);
	}
      else if (XTYPE (tem) == Lisp_String)
	{
	  insert_from_string (tem, 0, XSTRING (tem)->size);
	}
      else
	{
	  tem = wrong_type_argument (Qchar_or_string_p, tem);
	  goto retry;
	}
    }
  zmacs_region_stays = 0;
  return Qnil;
}

DEFUN ("insert-before-markers", Finsert_before_markers, Sinsert_before_markers, 0, MANY, 0,
  "Insert strings or characters at point, relocating markers after the text.\n\
Point moves forward so that it ends up after the inserted text.\n\
Any other markers at the point of insertion also end up after the text.")
  (nargs, args)
     int nargs;
     register Lisp_Object *args;
{
  register int argnum;
  register Lisp_Object tem;
  char str[1];

  for (argnum = 0; argnum < nargs; argnum++)
    {
      tem = args[argnum];
    retry:
      if (XTYPE (tem) == Lisp_Int)
	{
	  str[0] = XINT (tem);
	  insert_before_markers (str, 1);
	}
      else if (XTYPE (tem) == Lisp_String)
	{
	  insert_from_string_before_markers (tem, 0, XSTRING (tem)->size);
	}
      else
	{
	  tem = wrong_type_argument (Qchar_or_string_p, tem);
	  goto retry;
	}
    }
  zmacs_region_stays = 0;
  return Qnil;
}

DEFUN ("insert-char", Finsert_char, Sinsert_char, 2, 2, 0,
  "Insert COUNT (second arg) copies of CHAR (first arg).\n\
Point and all markers are affected as in the function `insert'.\n\
Both arguments are required.")
  (chr, count)
       Lisp_Object chr, count;
{
  register unsigned char *string;
  register int strlen;
  register int i, n;

  CHECK_NUMBER (chr, 0);
  CHECK_NUMBER (count, 1);

  n = XINT (count);
  if (n < 0)
    return Qnil;

  strlen = max (n, 256);
  string = (unsigned char *) alloca (strlen);
  for (i = 0; i < strlen; i++)
    string[i] = XFASTINT (chr);

  while (n >= strlen)
    {
      insert_raw_string (string, strlen);
      n -= strlen;
    }
  if (n > 0)
    insert_raw_string (string, n);

  zmacs_region_stays = 0;
  return Qnil;
}


extern Lisp_Object make_string_from_buffer ();

/* Return a string with the contents of the current region */

DEFUN ("buffer-substring", Fbuffer_substring, Sbuffer_substring, 2, 2, 0,
  "Return the contents of part of the current buffer as a string.\n\
The two arguments START and END are character positions;\n\
they can be in either order.")
  (b, e)
     Lisp_Object b, e;
{
  register int beg, end;

  validate_region (&b, &e);
  beg = XINT (b);
  end = XINT (e);

  return make_string_from_buffer (current_buffer, beg, end - beg);
}

DEFUN ("buffer-string", Fbuffer_string, Sbuffer_string, 0, 0, 0,
  "Return the contents of the current buffer as a string.")
  ()
{
  return make_string_from_buffer (current_buffer, BEGV, ZV - BEGV);
}

DEFUN ("insert-buffer-substring", Finsert_buffer_substring, Sinsert_buffer_substring,
  1, 3, 0,
  "Insert before point a substring of the contents buffer BUFFER.\n\
BUFFER may be a buffer or a buffer name.\n\
Arguments START and END are character numbers specifying the substring.\n\
They default to the beginning and the end of BUFFER.")
  (buf, b, e)
     Lisp_Object buf, b, e;
{
  register int beg, end, exch;
  register struct buffer *bp;

  buf = Fget_buffer (buf);
  bp = XBUFFER (buf);

  if (NILP (b))
    beg = BUF_BEGV (bp);
  else
    {
      CHECK_NUMBER_COERCE_MARKER (b, 0);
      beg = XINT (b);
    }
  if (NILP (e))
    end = BUF_ZV (bp);
  else
    {
      CHECK_NUMBER_COERCE_MARKER (e, 1);
      end = XINT (e);
    }

  if (beg > end)
    exch = beg, beg = end, end = exch;

  /* Move the gap or create enough gap in the current buffer.  */

  if (point != GPT)
    move_gap (point);
  if (GAP_SIZE < end - beg)
    make_gap (end - beg - GAP_SIZE);

  if (!(BUF_BEGV (bp) <= beg
	&& beg <= end
        && end <= BUF_ZV (bp)))
    args_out_of_range (b, e);

  /* Now the actual insertion will not do any gap motion,
     so it matters not if BUF is the current buffer.  */
  if (beg < BUF_GPT (bp))
    {
      insert_buffer_string (bp, beg, min (end, BUF_GPT (bp)) - beg);
      beg = min (end, BUF_GPT (bp));
    }
  if (beg < end)
    insert_buffer_string (bp, beg, end - beg);

  return Qnil;
}

DEFUN ("subst-char-in-region", Fsubst_char_in_region,
  Ssubst_char_in_region, 4, 5, 0,
  "From START to END, replace FROMCHAR with TOCHAR each time it occurs.\n\
If optional arg NOUNDO is non-nil, don't record this change for undo\n\
and don't mark the buffer as really changed.")
  (start, end, fromchar, tochar, noundo)
     Lisp_Object start, end, fromchar, tochar, noundo;
{
  register int pos, stop, look;

  validate_region (&start, &end);
  CHECK_NUMBER (fromchar, 2);
  CHECK_NUMBER (tochar, 3);

  pos = XINT (start);
  stop = XINT (end);
  look = XINT (fromchar);

  modify_region (pos, stop);
  if (! NILP (noundo))
    {
      if (MODIFF - 1 == current_buffer->save_modified)
	current_buffer->save_modified++;
      if (MODIFF - 1 == current_buffer->auto_save_modified)
	current_buffer->auto_save_modified++;
    }

  while (pos < stop)
    {
      if (CHAR_AT (pos) == look)
	{
	  if (NILP (noundo))
	    record_change (pos, 1);
	  *(CHAR_ADDRESS (pos)) = XINT (tochar);
	  if (NILP (noundo))
	    signal_after_change (pos, 1, 1);
	}
      pos++;
    }

  return Qnil;
}

DEFUN ("translate-region", Ftranslate_region, Stranslate_region, 3, 3, 0,
  "From START to END, translate characters according to TABLE.\n\
TABLE is a string; the Nth character in it is the mapping\n\
for the character with code N.  Returns the number of characters changed.")
  (start, end, table)
     Lisp_Object start;
     Lisp_Object end;
     register Lisp_Object table;
{
  register struct Lisp_String *t_str = XSTRING (table);
  register int pos, stop;	/* Limits of the region. */
  register int oc;		/* Old character. */
  register int nc;		/* New character. */
  int cnt;			/* Number of changes made. */
  Lisp_Object z;		/* Return. */
  int size;			/* Size of translate table. */

  validate_region (&start, &end);
  CHECK_STRING (table, 2);

  size = t_str->size;

  pos = XINT (start);
  stop = XINT (end);
  modify_region (pos, stop);

  cnt = 0;
  for (; pos < stop; ++pos)
    {
      oc = CHAR_AT (pos);
      if (oc < size)
	{
	  nc = t_str->data[oc];
	  if (nc != oc)
	    {
	      record_change (pos, 1);
	      *(CHAR_ADDRESS (pos)) = nc;
	      signal_after_change (pos, 1, 1);
	      ++cnt;
	    }
	}
    }

  XFASTINT (z) = cnt;
  return (z);
}

DEFUN ("delete-region", Fdelete_region, Sdelete_region, 2, 2, "r",
  "Delete the text between point and mark.\n\
When called from a program, expects two arguments,\n\
positions (integers or markers) specifying the stretch to be deleted.")
  (b, e)
     Lisp_Object b, e;
{
  validate_region (&b, &e);
  del_range (XINT (b), XINT (e));
  zmacs_region_stays = 0;
  return Qnil;
}

DEFUN ("widen", Fwiden, Swiden, 0, 0, "",
  "Remove restrictions (narrowing) from current buffer.\n\
This allows the buffer's full text to be seen and edited.")
  ()
{
  BEGV = BEG;
  SET_BUF_ZV (current_buffer, Z);
  clip_changed = 1;
  zmacs_region_stays = 0;
  return Qnil;
}

DEFUN ("narrow-to-region", Fnarrow_to_region, Snarrow_to_region, 2, 2, "r",
  "Restrict editing in this buffer to the current region.\n\
The rest of the text becomes temporarily invisible and untouchable\n\
but is not deleted; if you save the buffer in a file, the invisible\n\
text is included in the file.  \\[widen] makes all visible again.\n\
See also `save-restriction'.\n\
\n\
When calling from a program, pass two arguments; positions (integers\n\
or markers) bounding the text that should remain visible.")
  (b, e)
     register Lisp_Object b, e;
{
  register int i;

  CHECK_NUMBER_COERCE_MARKER (b, 0);
  CHECK_NUMBER_COERCE_MARKER (e, 1);

  if (XINT (b) > XINT (e))
    {
      i = XFASTINT (b);
      b = e;
      XFASTINT (e) = i;
    }

  if (!(BEG <= XINT (b) && XINT (b) <= XINT (e) && XINT (e) <= Z))
    args_out_of_range (b, e);

  BEGV = XFASTINT (b);
  SET_BUF_ZV (current_buffer, XFASTINT (e));
  if (point < XFASTINT (b))
    SET_PT (XFASTINT (b));
  if (point > XFASTINT (e))
    SET_PT (XFASTINT (e));
  clip_changed = 1;
  zmacs_region_stays = 0;
  return Qnil;
}

Lisp_Object
save_restriction_save ()
{
  register Lisp_Object bottom, top;
  /* Note: I tried using markers here, but it does not win
     because insertion at the end of the saved region
     does not advance mh and is considered "outside" the saved region. */
  XFASTINT (bottom) = BEGV - BEG;
  XFASTINT (top) = Z - ZV;

  return Fcons (Fcurrent_buffer (), Fcons (bottom, top));
}

Lisp_Object
save_restriction_restore (data)
     Lisp_Object data;
{
  register struct buffer *buf;
  register int newhead, newtail;
  register Lisp_Object tem;

  buf = XBUFFER (XCONS (data)->car);

  data = XCONS (data)->cdr;

  tem = XCONS (data)->car;
  newhead = XINT (tem);
  tem = XCONS (data)->cdr;
  newtail = XINT (tem);
  if (newhead + newtail > BUF_Z (buf) - BUF_BEG (buf))
    {
      newhead = 0;
      newtail = 0;
    }
  BUF_BEGV (buf) = BUF_BEG (buf) + newhead;
  SET_BUF_ZV (buf, BUF_Z (buf) - newtail);
  clip_changed = 1;

  /* If point is outside the new visible range, move it inside. */
  SET_BUF_PT (buf, in_accessible_range (buf, BUF_PT (buf)));

  return Qnil;
}

DEFUN ("save-restriction", Fsave_restriction, Ssave_restriction, 0, UNEVALLED, 0,
  "Execute BODY, saving and restoring current buffer's restrictions.\n\
The buffer's restrictions make parts of the beginning and end invisible.\n\
\(They are set up with `narrow-to-region' and eliminated with `widen'.)\n\
This special form, `save-restriction', saves the current buffer's restrictions\n\
when it is entered, and restores them when it is exited.\n\
So any `narrow-to-region' within BODY lasts only until the end of the form.\n\
The old restrictions settings are restored\n\
even in case of abnormal exit (throw or error).\n\
\n\
The value returned is the value of the last form in BODY.\n\
\n\
`save-restriction' can get confused if, within the BODY, you widen\n\
and then make changes outside the area within the saved restrictions.\n\
\n\
Note: if you are using both `save-excursion' and `save-restriction',\n\
use `save-excursion' outermost:\n\
    (save-excursion (save-restriction ...))")
  (body)
     Lisp_Object body;
{
  register Lisp_Object val;
  int count = specpdl_ptr - specpdl;

  record_unwind_protect (save_restriction_restore, save_restriction_save ());
  val = Fprogn (body);
  unbind_to (count);
  return val;
}

DEFUN ("message", Fmessage, Smessage, 1, MANY, 0,
  "Print a one-line message at the bottom of the screen.\n\
The first argument is a control string.\n\
It may contain %s or %d or %c to print successive following arguments.\n\
%s means print an argument as a string, %d means print as number in decimal,\n\
%c means print a number as a single character.\n\
The argument used by %s must be a string or a symbol;\n\
the argument used by %d or %c must be a number.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  register Lisp_Object val;

#ifdef MULTI_SCREEN
  extern Lisp_Object Vglobal_minibuffer_screen;

  if (XTYPE (Vglobal_minibuffer_screen) == Lisp_Screen)
    Fmake_screen_visible (Vglobal_minibuffer_screen);
#endif

  val = Fformat (nargs, args);
  message ("%s", XSTRING (val)->data);
  return val;
}

DEFUN ("format", Fformat, Sformat, 1, MANY, 0,
  "Format a string out of a control-string and arguments.\n\
The first argument is a control string.\n\
The other arguments are substituted into it to make the result, a string.\n\
It may contain %-sequences meaning to substitute the next argument.\n\
%s means print a string argument.  Actually, prints any object, with `princ'.\n\
%d means print as number in decimal (%o octal, %x hex).\n\
%c means print a number as a single character.\n\
%S means print any object as an s-expression (using prin1).\n\
The argument used for %d, %o, %x or %c must be a number.")
  (nargs, args)
     int nargs;
     register Lisp_Object *args;
{
  register int n;
  register int total = 5;
  char *buf;
  register unsigned char *format, *end;
  register unsigned char **strings;
  int length;
  extern char *index ();

  CHECK_STRING (args[0], 0);

  format = XSTRING(args[0])->data;
  end = format + XSTRING(args[0])->size;

  /* We have to do so much work in oder to prepare to call doprnt
     that we might as well do all of it ourself...  (Which would also
     circumvent C asciz cretinism by allowing ascii 000 chars to appear)
   */
  n = 0;
  while (format < end)
    if (*format++ == '%')
      {
	int minlen;

	/* Process a numeric arg and skip it.  */
	minlen = atoi (format);
	if (minlen > 0)
	  total += minlen;
	else
	  total -= minlen;
	while ((*format >= '0' && *format <= '9')
	       || *format == '-' || *format == ' ')
	  format++;

	if (*format == '%')
	  format++;
	else if (++n >= nargs)
	  ;
	else if (*format == 'S')
	  {
	    /* For `S', prin1 the argument and then treat like a string.  */
	    register Lisp_Object tem;
	    tem = Fprin1_to_string (args[n], Qnil);
	    args[n] = tem;
	    goto string;
	  }
	else if (XTYPE (args[n]) == Lisp_Symbol)
	  {
	    XSET (args[n], Lisp_String, XSYMBOL (args[n])->name);
	    goto string;
	  }
	else if (XTYPE (args[n]) == Lisp_String)
	  {
	  string:
	    total += XSTRING (args[n])->size;
	  }
	/* Would get MPV otherwise, since Lisp_Int's `point' to low memory.  */
	else if (XTYPE (args[n]) == Lisp_Int && *format != 's')
	  total += 10;
	else
	  {
	    /* Anything but a string, convert to a string using princ.  */
	    register Lisp_Object tem;
	    tem = Fprin1_to_string (args[n], Qt);
	    args[n] = tem;
	    goto string;
	  }
      }

    strings = (unsigned char **) alloca ((n + 1) * sizeof (unsigned char *));
    for (; n >= 0; n--)
      {
	if (n >= nargs)
	  strings[n] = (unsigned char *) "";
	else if (XTYPE (args[n]) == Lisp_Int)
	  /* We checked above that the corresponding format effector
	     isn't %s, which would cause MPV.  */
	  strings[n] = (unsigned char *) XINT (args[n]);
	else
	  strings[n] = XSTRING (args[n])->data;
      }

    /* Format it in bigger and bigger buf's until it all fits. */
    while (1)
      {
	buf = (char *) alloca (total + 1);
	buf[total - 1] = 0;

	doprnt (buf, total + 1, strings[0], end, nargs, strings + 1);
	if (buf[total - 1] == 0)
	  break;

	total *= 2;
      }

    return build_string (buf);
  }

/* VARARGS 1 */
Lisp_Object
#ifdef NO_ARG_ARRAY
format1 (string1, arg0, arg1, arg2, arg3, arg4)
     int arg0, arg1, arg2, arg3, arg4;
#else
format1 (string1)
#endif
     char *string1;
{
  char buf[100];
#ifdef NO_ARG_ARRAY
  int args[5];
  args[0] = arg0;
  args[1] = arg1;
  args[2] = arg2;
  args[3] = arg3;
  args[4] = arg4;
  doprnt (buf, sizeof buf, string1, 0, 5, args);
#else
  doprnt (buf, sizeof buf, string1, 0, 5, &string1 + 1);
#endif
  return build_string (buf);
}

DEFUN ("char-equal", Fchar_equal, Schar_equal, 2, 2, 0,
  "Return t if two characters match, optionally ignoring case.\n\
Both arguments must be characters (i.e. integers).\n\
Case is ignored if `case-fold-search' is non-nil in the current buffer.")
  (c1, c2)
     register Lisp_Object c1, c2;
{
  unsigned char *downcase = DOWNCASE_TABLE;
  CHECK_NUMBER (c1, 0);
  CHECK_NUMBER (c2, 1);

  if (!NILP (current_buffer->case_fold_search)
      ? downcase[0xff & XFASTINT (c1)] == downcase[0xff & XFASTINT (c2)]
      : XINT (c1) == XINT (c2))
    return Qt;
  return Qnil;
}

#ifndef MAINTAIN_ENVIRONMENT /* it is done in environ.c in that case */
DEFUN ("getenv", Fgetenv, Sgetenv, 1, 1, 0,
  "Return the value of environment variable VAR, as a string.\n\
VAR should be a string.  Value is nil if VAR is undefined in the environment.")
  (str)
     Lisp_Object str;
{
  register char *val;
  CHECK_STRING (str, 0);
  val = (char *) egetenv (XSTRING (str)->data);
  if (!val)
    return Qnil;
  return build_string (val);
}
#endif /* MAINTAIN_ENVIRONMENT */

void
syms_of_editfns ()
{
  DEFVAR_LISP ("system-name", &Vsystem_name,
	       "The name of the machine Emacs is running on.");
  
  DEFVAR_LISP ("user-full-name", &Vuser_full_name,
	       "The full name of the user logged in.");

  DEFVAR_LISP ("user-name", &Vuser_name,
	       "The user's name, based on the effective uid.");

  DEFVAR_LISP ("user-real-name", &Vuser_real_name,
	       "The user's name, base upon the real uid.");

  DEFVAR_BOOL ("zmacs-regions", &zmacs_regions,
	       "*Whether LISPM-style active regions should be used.\n\
This means that commands which operate on the region (the area between the\n\
point and the mark) will only work while the region is in the ``active''\n\
state, which is indicated by hilighting.  Executing most commands causes\n\
the region to not be in the active state, so (for example) \\[kill-region] will only\n\
work immediately after activating the region.\n\
\n\
More specifically:\n\
\n\
 - Commands which operate on the region only work if the region is active.\n\
 - Only a very small set of commands cause the region to become active:\n\
   Those commands whose semantics are to mark an area, like mark-defun.\n\
 - The region is deactivated after each command that is executed, except that:\n\
 - \"Motion\" commands do not change whether the region is active or not.\n\
\n\
set-mark-command (C-SPC) pushes a mark and activates the region.  Moving the\n\
cursor with normal motion commands (C-n, C-p, etc) will cause the region\n\
between point and the recently-pushed mark to be highlighted.  It will\n\
remain highlighted until some non-motion comand is executed.\n\
\n\
exchange-point-and-mark (\\[exchange-point-and-mark]) activates the region.  So if you mark a\n\
region and execute a command that operates on it, you can reactivate the\n\
same region with \\[exchange-point-and-mark] (or perhaps \\[exchange-point-and-mark] \\[exchange-point-and-mark]) to operate on it\n\
again.\n\
\n\
Generally, commands which push marks as a means of navigation (like\n\
beginning-of-buffer and end-of-buffer (M-< and M->)) do not activate the\n\
region.  But commands which push marks as a means of marking an area of\n\
text (like mark-defun (\\[mark-defun]), mark-word (\\[mark-word]) or mark-whole-buffer (\\[mark-whole-buffer]))\n\
do activate the region.");
  /* Zmacs style active regions are now ON by default */
  zmacs_regions = 1;
  zmacs_region_active_p = 0;

  DEFVAR_LISP ("zmacs-activate-region-hook", &Vzmacs_activate_region_hook,
       "Function or functions called when the region becomes active;\n\
see the variable `zmacs-regions'.");
  DEFVAR_LISP ("zmacs-deactivate-region-hook", &Vzmacs_deactivate_region_hook,
       "Function or functions called when the region becomes inactive;\n\
see the variable `zmacs-regions'.");
  DEFVAR_LISP ("zmacs-update-region-hook", &Vzmacs_update_region_hook,
       "Function or functions called when the active region changes.\n\
This is called after each command that sets `region-stays' to t.\n\
See the variable `zmacs-regions'.");
  DEFVAR_BOOL ("zmacs-region-stays", &zmacs_region_stays,
   "Commands which do not wish to affect whether the region is currently\n\
hilighted should set this to t.  Normally, the region is turned off after\n\
executing each command that did not explicitly turn it on with the function\n\
zmacs-activate-region. Setting this to true lets a command be non-intrusive.\n\
See the variable `zmacs-regions'.");
  Vzmacs_update_region_hook = Qnil;
  Vzmacs_activate_region_hook = Qnil;
  Vzmacs_deactivate_region_hook = Qnil;
  zmacs_region_stays = 0;
  Qzmacs_update_region_hook = intern ("zmacs-update-region-hook");
  Qzmacs_activate_region_hook = intern ("zmacs-activate-region-hook");
  Qzmacs_deactivate_region_hook = intern ("zmacs-deactivate-region-hook");
  staticpro (&Qzmacs_update_region_hook);
  staticpro (&Qzmacs_activate_region_hook);
  staticpro (&Qzmacs_deactivate_region_hook);

  defsubr (&Schar_equal);
  defsubr (&Sgoto_char);
  defsubr (&Sstring_to_char);
  defsubr (&Schar_to_string);
  defsubr (&Sbuffer_substring);
  defsubr (&Sbuffer_string);

  defsubr (&Spoint_marker);
  defsubr (&Smark_marker);
  defsubr (&Spoint);
  defsubr (&Sregion_beginning);
  defsubr (&Sregion_end);
  defsubr (&Ssave_excursion);

  defsubr (&Sbufsize);
  defsubr (&Spoint_max);
  defsubr (&Spoint_min);
  defsubr (&Spoint_min_marker);
  defsubr (&Spoint_max_marker);

  defsubr (&Sbobp);
  defsubr (&Seobp);
  defsubr (&Sbolp);
  defsubr (&Seolp);
  defsubr (&Sfollchar);
  defsubr (&Sprevchar);
  defsubr (&Schar_after);
  defsubr (&Sinsert);
  defsubr (&Sinsert_before_markers);
  defsubr (&Sinsert_char);

  defsubr (&Suser_login_name);
  defsubr (&Suser_real_login_name);
  defsubr (&Suser_uid);
  defsubr (&Suser_real_uid);
  defsubr (&Suser_full_name);
  defsubr (&Scurrent_time_string);
  defsubr (&Scurrent_time_seconds);
  defsubr (&Ssystem_name);
  defsubr (&Sset_default_file_mode);
  defsubr (&Sunix_sync);
  defsubr (&Smessage);
  defsubr (&Sformat);
#ifndef MAINTAIN_ENVIRONMENT /* in environ.c */
  defsubr (&Sgetenv);
#endif

  defsubr (&Sinsert_buffer_substring);
  defsubr (&Ssubst_char_in_region);
  defsubr (&Stranslate_region);
  defsubr (&Sdelete_region);
  defsubr (&Swiden);
  defsubr (&Snarrow_to_region);
  defsubr (&Ssave_restriction);
  
  defsubr (&Szmacs_activate_region);
  defsubr (&Szmacs_deactivate_region);
}
