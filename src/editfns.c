/* Lisp functions pertaining to editing.
   Copyright (C) 1985, 1986, 1987, 1989, 1992, 1993
   Free Software Foundation, Inc.

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

#include <stdarg.h>
#include <ctype.h>
#include <stdio.h>              /* for sprintf */

#ifdef VMS
#include "vms-pwd.h"
#else
#include <pwd.h>
#endif

#include "lisp.h"
#include "buffer.h"
#include "window.h"
#include "insdel.h"
#include "events.h"             /* for EVENTP */

#include "systime.h"

#define min(a, b) ((a) < (b) ? (a) : (b))
#define max(a, b) ((a) > (b) ? (a) : (b))

/* Some static data, and a function to initialize it for each run */

Lisp_Object Vsystem_name;
Lisp_Object Vuser_real_name;	/* login name of current user ID */
Lisp_Object Vuser_full_name;	/* full name of current user */
Lisp_Object Vuser_name;		/* user name from USER or LOGNAME.  */

extern char *get_system_name (void);

Lisp_Object Qformat;

Lisp_Object Qpoint, Qmark, Qregion_beginning, Qregion_end;

void
init_editfns ()
{
  char *user_name;
#ifndef AMPERSAND_FULL_NAME
  register unsigned char *p, *q;
#else
  register char *p, *q;
#endif
  struct passwd *pw;	/* password entry for the current user */
  Lisp_Object tem;

  /* Set up system_name even when dumping.  */

  Vsystem_name = build_string (get_system_name ());
#ifndef AMPERSAND_FULL_NAME
  p = XSTRING (Vsystem_name)->data;
#else
  p = (char *) XSTRING (Vsystem_name)->data;
#endif
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

  /* Get the effective user name, by consulting environment variables,
     or the effective uid if those are unset.  */
  user_name = getenv ("USER");
  if (!user_name)
    user_name = getenv ("LOGNAME");
  if (!user_name)
    {
      pw = (struct passwd *) getpwuid (geteuid ());
      user_name = (char *) (pw ? pw->pw_name : "unknown");
    }
  Vuser_name = build_string (user_name);

  /* If the user name claimed in the environment vars differs from
     the real uid, use the claimed name to find the full name.  */
  tem = Fstring_equal (Vuser_name, Vuser_real_name);
  if (NILP (tem))
    pw = (struct passwd *) getpwnam ((char *) XSTRING (Vuser_name)->data);
  
#ifndef AMPERSAND_FULL_NAME
  p = (unsigned char *) ((pw) ? USER_FULL_NAME : "unknown");
  q = (unsigned char *) strchr ((char *) p, ',');
#else
  p = (char *) ((pw) ? USER_FULL_NAME : "unknown");
  q = (char *) strchr ((char *) p, ',');
#endif
  Vuser_full_name = make_string ((char *) p,
				 (q ? q - p : strlen ((char *) p)));
  
#ifdef AMPERSAND_FULL_NAME
  p = (char *) XSTRING (Vuser_full_name)->data;
  q = strchr (p, '&');
  /* Substitute the login name for the &, upcasing the first character.  */
  if (q)
    {
      char *r = (char *) alloca (strlen (p) + XSTRING (Vuser_name)->size + 1);
      memcpy (r, p, q - p);
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

  if (EVENTP (n))
    {
      Lisp_Object ch = Fevent_to_character (n, Qt, Qnil, Qnil);
      if (NILP (ch))
	return
	  Fsignal (Qerror,
		   list2 (build_string ("character has no ASCII equivalent:"),
			  Fcopy_event (n, Qnil)));
      n = ch;
    }

  CHECK_FIXNUM (n, 0);

  c = XINT (n);
  return make_string (&c, 1);
}

DEFUN ("string-to-char", Fstring_to_char, Sstring_to_char, 1, 1, 0,
  "Convert arg STRING to a character, the first character of that string.")
  (str)
     register Lisp_Object str;
{
  register struct Lisp_String *p;
  CHECK_STRING (str, 0);

  p = XSTRING (str);
  if (p->size != 0)
    return (make_number (((unsigned char *) p->data)[0]));
  else                          /*>>> Gag me! */
    return (Qzero);
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
  return (make_number (point));
}

DEFUN ("point-marker", Fpoint_marker, Spoint_marker, 0, 1, 0,
   "Return value of point, as a marker object.\n\
This marker is a copy; you may modify it with reckless abandon.\n\
If the argument to this function is non-nil, then it returns the real\n\
point-marker; modifying the position of this marker willl move point.\n\
It is illegal to change the buffer of it, or make it point nowhere.")
  (dont_copy_p)
  Lisp_Object dont_copy_p;
{
  if (NILP (dont_copy_p))
    return Fcopy_marker (current_buffer->point_marker);
  return current_buffer->point_marker;
}

int
clip_to_bounds (int lower, int num, int upper)
{
  if (num < lower)
    return lower;
  else if (num > upper)
    return upper;
  else
    return num;
}

DEFUN ("goto-char", Fgoto_char, Sgoto_char, 1, 1, "NGoto char: ",
  "Set point to POSITION, a number or marker.\n\
Beginning of buffer is position (point-min), end is (point-max).")
  (n)
     register Lisp_Object n;
{
  CHECK_FIXNUM_COERCE_MARKER (n, 0);

  SET_PT (clip_to_bounds (BEGV, XINT (n), ZV));
  return n;
}

static Lisp_Object
region_limit (int beginningp)
{
  register Lisp_Object m;

#if 0 /* RMSmacs */
  if (!NILP (Vtransient_mark_mode) && NILP (Vmark_even_if_inactive)
      && NILP (current_buffer->mark_active))
    Fsignal (Qmark_inactive, Qnil);
#endif
  m = Fmarker_position (current_buffer->mark);
  if (NILP (m)) error ("There is no region now");
  if (!!(point < XINT (m)) == !!beginningp)
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

int zmacs_region_stays;

Lisp_Object Vzmacs_update_region_hook,     Qzmacs_update_region_hook;
Lisp_Object Vzmacs_activate_region_hook,   Qzmacs_activate_region_hook;
Lisp_Object Vzmacs_deactivate_region_hook, Qzmacs_deactivate_region_hook;

DEFUN ("zmacs-activate-region", Fzmacs_activate_region,
       Szmacs_activate_region, 0, 0, 0,
 "Make the region between `point' and `mark' be in the active (highlighted)\n\
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
(highlighted) state, if `zmacs-regions' is true.  You shouldn't need to call\n\
this; the command loop calls it when appropriate.\n\
Returns t if the region had been active, nil otherwise.")
    ()
{
  if (! zmacs_region_active_p)
    return Qnil;
  zmacs_region_active_p = 0;
  if (!NILP (Vrun_hooks))
    call1 (Vrun_hooks, Qzmacs_deactivate_region_hook);
  return Qt;
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
currently in the active (highlighted) state.  With an argument of t, this\n\
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
  int visible = (XBUFFER (XWINDOW (selected_window)->buffer)
		 == current_buffer);
  Lisp_Object tem = ((visible) ? Qt : Qnil);

  if (XINT (Fpoint ()) != XINT (Fmarker_position (Fpoint_marker (Qt))))
    abort ();

#if 0 /* RMSmacs */
  tem = Fcons (tem, current_buffer->mark_active)
#endif

  return Fcons (Fpoint_marker (Qnil),
		Fcons (Fcopy_marker (current_buffer->mark),
                       tem));
}

Lisp_Object
save_excursion_restore (Lisp_Object info)
{
  Lisp_Object tem;
  int visible;
  struct gcpro gcpro1, gcpro2;

  tem = Fmarker_buffer (Fcar (info));
  /* If buffer being returned to is now deleted, avoid error */
  /* Otherwise could get error here while unwinding to top level
     and crash */
  /* In that case, Fmarker_buffer returns nil now.  */
  if (NILP (tem))
    return Qnil;
  /* Need gcpro in case Lisp hooks get run */
  GCPRO2 (info, tem);
  Fset_buffer (tem);
  tem = Fcar (info);
  Fgoto_char (tem);
  unchain_marker (tem);
  tem = Fcar (Fcdr (info));
  Fset_marker (current_buffer->mark, tem, Fcurrent_buffer ());
  unchain_marker (tem);
  tem = Fcdr (Fcdr (info));
  visible = !NILP (tem);
#if 0 /* RMSmacs */
  visible = !NILP (Fcar (tem));
#endif

#if 0 /* We used to make the current buffer visible in the selected window
	 if that was true previously.  That avoids some anomalies.
	 But it creates others, and it wasn't documented, and it is simpler
	 and cleaner never to alter the window/buffer connections.  */
/* ## I'm certain some code somewhere depends on this behavior. --jwz */

  if (visible 
      && (current_buffer != XBUFFER (XWINDOW (selected_window)->buffer)))
    Fswitch_to_buffer (Fcurrent_buffer (), Qnil);
#endif

#if 0 /* RMSmacs */
  visible = !NILP (current_buffer->mark_active);
  current_buffer->mark_active = Fcdr (tem);
  if (! NILP (current_buffer->mark_active))
    call1 (Vrun_hooks, intern ("activate-mark-hook"));
  else if (visible)
    call1 (Vrun_hooks, intern ("deactivate-mark-hook"));
#endif

  while (CONSP (info))
  {
    struct Lisp_Cons *victim = XCONS (info);
    info = victim->cdr;
    free_cons (victim);
  }
  UNGCPRO;
  return Qnil;
}

DEFUN ("save-excursion", Fsave_excursion, Ssave_excursion, 0, UNEVALLED, 0,
  "Save point, mark, and current buffer; execute BODY; restore those things.\n\
Executes BODY just like `progn'.\n\
The values of point, mark and the current buffer are restored\n\
even in case of abnormal exit (throw or error).")
  (args)
     Lisp_Object args;
{
  int speccount = specpdl_depth ();

  record_unwind_protect (save_excursion_restore, save_excursion_save ());
			 
  return unbind_to (speccount, Fprogn (args));
}

DEFUN ("buffer-size", Fbufsize, Sbufsize, 0, 0, 0,
  "Return the number of characters in the current buffer.")
  ()
{
  return (make_number (Z - BEG));
}

DEFUN ("point-min", Fpoint_min, Spoint_min, 0, 0, 0,
  "Return the minimum permissible value of point in the current buffer.\n\
This is 1, unless a clipping restriction is in effect.")
  ()
{
  return (make_number (BEGV));
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
  return (make_number (ZV));
}

DEFUN ("point-max-marker", Fpoint_max_marker, Spoint_max_marker, 0, 0, 0,
  "Return a marker to the maximum permissible value of point in this buffer.\n\
This is (1+ (buffer-size)), unless a clipping restriction is in effect,\n\
in which case it is less.")
  ()
{
  return buildmark (ZV);
}

DEFUN ("following-char", Ffollowing_char, Sfollowing_char, 0, 0, 0,
  "Return the character following point, as a number.\n\
At the end of the buffer or accessible region, return 0.")
  ()
{
  if (point >= ZV)
    return (Qzero);             /* >>> Gag me! */
  else
    return (make_number (FETCH_CHAR (point)));
}

DEFUN ("preceding-char", Fprevious_char, Sprevious_char, 0, 0, 0,
  "Return the character preceding point, as a number.\n\
At the beginning of the buffer or accessible region, return 0.")
  ()
{
  if (point <= BEGV)
    return (Qzero);             /* >>> Gag me! */
  else
    return (make_number (FETCH_CHAR (point - 1)));
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
  if (point == BEGV || FETCH_CHAR (point - 1) == '\n')
    return Qt;
  return Qnil;
}

DEFUN ("eolp", Feolp, Seolp, 0, 0, 0,
  "Return T if point is at the end of a line.\n\
`End of a line' includes point being at the end of the buffer.")
  ()
{
  if (point == ZV || FETCH_CHAR (point) == '\n')
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
  register int n;

  CHECK_FIXNUM_COERCE_MARKER (pos, 0);

  n = XINT (pos);
  if (n < BEGV || n >= ZV)
    return Qnil;
  return (make_number (FETCH_CHAR (n)));
}

DEFUN ("user-login-name", Fuser_login_name, Suser_login_name, 0, 0, 0,
  "Return the name under which the user logged in, as a string.\n\
This is based on the effective uid, not the real uid.\n\
Also, if the environment variable USER or LOGNAME is set,\n\
that determines the value of this function.")
  ()
{
  return (Fcopy_sequence (Vuser_name));
}

DEFUN ("user-real-login-name", Fuser_real_login_name, Suser_real_login_name,
  0, 0, 0,
  "Return the name of the user's real uid, as a string.\n\
Differs from `user-login-name' when running under `su'.")
  ()
{
  return (Fcopy_sequence (Vuser_real_name));
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
  return (Fcopy_sequence (Vuser_full_name));
}

DEFUN ("system-name", Fsystem_name, Ssystem_name, 0, 0, 0,
  "Return the name of the machine you are running on, as a string.")
  ()
{
    return (Fcopy_sequence (Vsystem_name));
}

DEFUN ("current-time", Fcurrent_time, Scurrent_time, 0, 0, 0,
  "Return the current time, as the number of seconds since 12:00 AM January 1970.\n\
The time is returned as a list of three integers.  The first has the\n\
most significant 16 bits of the seconds, while the second has the\n\
least significant 16 bits.  The third integer gives the microsecond\n\
count.\n\
\n\
The microsecond count is zero on systems that do not provide\n\
resolution finer than a second.")
  ()
{
  EMACS_TIME t;
  Lisp_Object result[3];

  EMACS_GET_TIME (t);
  XSET (result[0], Lisp_Int, (EMACS_SECS (t) >> 16) & 0xffff);
  XSET (result[1], Lisp_Int, (EMACS_SECS (t) >> 0)  & 0xffff);
  XSET (result[2], Lisp_Int, EMACS_USECS (t));

  return Flist (3, result);
}


static int
lisp_time_argument (specified_time, result)
     Lisp_Object specified_time;
     time_t *result;
{
  if (NILP (specified_time))
    return time (result) != -1;
  else
    {
      Lisp_Object high, low;
      high = Fcar (specified_time);
      CHECK_NUMBER (high, 0);
      low = Fcdr (specified_time);
      if (XTYPE (low) == Lisp_Cons)
	low = Fcar (low);
      CHECK_NUMBER (low, 0);
      *result = (XINT (high) << 16) + (XINT (low) & 0xffff);
      return *result >> 16 == XINT (high);
    }
}

DEFUN ("current-time-string", Fcurrent_time_string, Scurrent_time_string, 0, 1, 0,
  "Return the current time, as a human-readable string.\n\
Programs can use this function to decode a time,\n\
since the number of columns in each field is fixed.\n\
The format is `Sun Sep 16 01:03:52 1973'.\n\
If an argument is given, it specifies a time to format\n\
instead of the current time.  The argument should have the form:\n\
  (HIGH . LOW)\n\
or the form:\n\
  (HIGH LOW . IGNORED).\n\
Thus, you can use times obtained from `current-time'\n\
and from `file-attributes'.")
  (specified_time)
  Lisp_Object specified_time;
{
  time_t value;
  char buf[30];
  register char *tem;

  if (! lisp_time_argument (specified_time, &value))
    value = -1;
  tem = (char *) ctime (&value);

  strncpy (buf, tem, 24);
  buf[24] = 0;

  return build_string (buf);
}

#define TM_YEAR_ORIGIN 1900

/* Yield A - B, measured in seconds.  */
static long
difftm(a, b)
     struct tm *a, *b;
{
  int ay = a->tm_year + (TM_YEAR_ORIGIN - 1);
  int by = b->tm_year + (TM_YEAR_ORIGIN - 1);
  return
    (
     (
      (
       /* difference in day of year */
       a->tm_yday - b->tm_yday
       /* + intervening leap days */
       +  ((ay >> 2) - (by >> 2))
       -  (ay/100 - by/100)
       +  ((ay/100 >> 2) - (by/100 >> 2))
       /* + difference in years * 365 */
       +  (long)(ay-by) * 365
       )*24 + (a->tm_hour - b->tm_hour)
      )*60 + (a->tm_min - b->tm_min)
     )*60 + (a->tm_sec - b->tm_sec);
}

DEFUN ("current-time-zone", Fcurrent_time_zone, Scurrent_time_zone, 0, 1, 0,
  "Return the offset and name for the local time zone.\n\
This returns a list of the form (OFFSET NAME).\n\
OFFSET is an integer number of seconds ahead of UTC (east of Greenwich).\n\
    A negative value means west of Greenwich.\n\
NAME is a string giving the name of the time zone.\n\
If an argument is given, it specifies when the time zone offset is determined\n\
instead of using the current time.  The argument should have the form:\n\
  (HIGH . LOW)\n\
or the form:\n\
  (HIGH LOW . IGNORED).\n\
Thus, you can use times obtained from `current-time'\n\
and from `file-attributes'.\n\
\n\
Some operating systems cannot provide all this information to Emacs;\n\
in this case, `current-time-zone' returns a list containing nil for\n\
the data it can't find.")
  (specified_time)
     Lisp_Object specified_time;
{
  time_t value;
  struct tm *t;

  if (lisp_time_argument (specified_time, &value)
      && (t = gmtime (&value)) != 0)
    {
      struct tm gmt;
      long offset;
      char *s, buf[6];

      gmt = *t;		/* Make a copy, in case localtime modifies *t.  */
      t = localtime (&value);
      offset = difftm (t, &gmt);
      s = 0;
#ifdef HAVE_TM_ZONE
      if (t->tm_zone)
	s = t->tm_zone;
#else /* not HAVE_TM_ZONE */
#ifdef HAVE_TZNAME
      if (t->tm_isdst == 0 || t->tm_isdst == 1)
	s = tzname[t->tm_isdst];
#endif
#endif /* not HAVE_TM_ZONE */
      if (!s)
	{
	  /* No local time zone name is available; use "+-NNNN" instead.  */
	  int am = (offset < 0 ? -offset : offset) / 60;
	  sprintf (buf, "%c%02d%02d", (offset < 0 ? '-' : '+'), am/60, am%60);
	  s = buf;
	}
      return list2 (make_number (offset), build_string (s));
    }
  else
    return list2 (Qnil, Qnil);
}


void
insert1 (arg)
     Lisp_Object arg;
{
  struct gcpro gcpro1;
  GCPRO1 (arg);
  Finsert (1, &arg);
  UNGCPRO;
}


/* Callers passing one argument to Finsert need not gcpro the
   argument "array", since the only element of the array will
   not be used after calling insert or insert_from_string, so
   we don't care if it gets trashed.  */

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

  for (argnum = 0; argnum < nargs; argnum++)
    {
      tem = args[argnum];
    retry:
      if (FIXNUMP (tem))
	{
	  insert_char (XINT (tem));
	}
      else if (STRINGP (tem))
	{
	  insert_from_string (tem, -1);
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

  for (argnum = 0; argnum < nargs; argnum++)
    {
      tem = args[argnum];
    retry:
      if (FIXNUMP (tem))
	{
	  char str[1];
	  str[0] = XINT (tem);
	  insert_before_markers (str, 1, Qzero);
	}
      else if (STRINGP (tem))
	{
	  insert_from_string_before_markers (tem, -1);
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
  register char *string;
  register int strlen;
  register int i, n;

  CHECK_FIXNUM (chr, 0);
  CHECK_FIXNUM (count, 1);

  n = XINT (count);
  if (n <= 0)
    return Qnil;
  strlen = min (n, 256);
  string = (char *) alloca (strlen);
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


/* Making strings from buffer contents.  */

/* We don't want to use plain old make_string here, because it calls
   make_uninit_string, which can cause the buffer arena to be
   compacted.  make_string has no way of knowing that the data has
   been moved, and thus copies the wrong data into the string.  This
   doesn't effect most of the other users of make_string, so it should
   be left as is.  But we should use this function when conjuring
   buffer substrings.  */

Lisp_Object
make_string_from_buffer (struct buffer *buf,
                         int index, int length)
{
  Lisp_Object val = make_uninit_string (length);

  XSTRING (val)->dup_list = replicate_extents (index, length, buf);

  {
    int len1 = BUF_GPT (buf) - index;
    register unsigned char *start1 = BUF_CHAR_ADDRESS (buf, index);
    register unsigned char *dest = XSTRING (val)->data;

    if (len1 < 0)
    {
      /* Completely after gap */
      memcpy (dest, start1, length);
    }
    else if (length <= len1)
    {
      /* Completely before gap */
      memcpy (dest, start1, length);
    }
    else
    {
      /* Spans gap */
      int index2 = index + len1;
      register unsigned char *start2 = BUF_CHAR_ADDRESS (buf, index2);

      memcpy (dest, start1, len1);
      memcpy (dest + len1, start2, length - len1);
    }
  }

  return val;
}

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

/* >>> This should take buffer, start, end as optional args!!! */
DEFUN ("buffer-string", Fbuffer_string, Sbuffer_string, 0, 0, 0,
  "Return the contents of the current buffer as a string.")
  ()
{
  return make_string_from_buffer (current_buffer, BEGV, ZV - BEGV);
}

DEFUN ("insert-buffer-substring", Finsert_buffer_substring, Sinsert_buffer_substring,
  1, 3, 0,
  "Insert before point a substring of the contents of buffer BUFFER.\n\
BUFFER may be a buffer or a buffer name.\n\
Arguments START and END are character numbers specifying the substring.\n\
They default to the beginning and the end of BUFFER.")
  (buf, b, e)
     Lisp_Object buf, b, e;
{
  register int beg, end;
  register struct buffer *bp;

  buf = get_buffer (buf, 1);
  bp = XBUFFER (buf);

  if (NILP (b))
    beg = BUF_BEGV (bp);
  else
    {
      CHECK_FIXNUM_COERCE_MARKER (b, 0);
      beg = XINT (b);
    }
  if (NILP (e))
    end = BUF_ZV (bp);
  else
    {
      CHECK_FIXNUM_COERCE_MARKER (e, 1);
      end = XINT (e);
    }

  if (beg > end)
    {
      int exch = beg;
      beg = end;
      end = exch;
    }

  /* Move the gap or create enough gap in the current buffer.  */

  if (point != GPT)
    move_gap (current_buffer, point);
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

DEFUN ("compare-buffer-substrings", Fcompare_buffer_substrings, Scompare_buffer_substrings,
  6, 6, 0,
  "Compare two substrings of two buffers; return result as number.\n\
the value is -N if first string is less after N-1 chars,\n\
+N if first string is greater after N-1 chars, or 0 if strings match.\n\
Each substring is represented as three arguments: BUFFER, START and END.\n\
That makes six args in all, three for each substring.\n\n\
The value of `case-fold-search' in the current buffer\n\
determines whether case is significant or ignored.")
  (buffer1, start1, end1, buffer2, start2, end2)
     Lisp_Object buffer1, start1, end1, buffer2, start2, end2;
{
  register int begp1, endp1, begp2, endp2, len1, len2, length, i;
  register struct buffer *bp1, *bp2;
  register unsigned char *trt 
    = ((!NILP (current_buffer->case_fold_search))
       ? XSTRING (current_buffer->case_canon_table)->data 
       : 0);

  /* Find the first buffer and its substring.  */

  if (NILP (buffer1))
    bp1 = current_buffer;
  else
    {
      Lisp_Object buf1;
      buf1 = get_buffer (buffer1, 1);
      bp1 = XBUFFER (buf1);
    }

  if (NILP (start1))
    begp1 = BUF_BEGV (bp1);
  else
    {
      CHECK_NUMBER_COERCE_MARKER (start1, 1);
      begp1 = XINT (start1);
    }
  if (NILP (end1))
    endp1 = BUF_ZV (bp1);
  else
    {
      CHECK_NUMBER_COERCE_MARKER (end1, 2);
      endp1 = XINT (end1);
    }

  if (begp1 > endp1)
  {
    int temp = begp1; begp1 = endp1; endp1 = temp;
  }

  if (!(BUF_BEGV (bp1) <= begp1
	&& begp1 <= endp1
        && endp1 <= BUF_ZV (bp1)))
    args_out_of_range (start1, end1);

  /* Likewise for second substring.  */

  if (NILP (buffer2))
    bp2 = current_buffer;
  else
    {
      Lisp_Object buf2;
      buf2 = get_buffer (buffer2, 1);
      bp2 = XBUFFER (buffer2);
    }

  if (NILP (start2))
    begp2 = BUF_BEGV (bp2);
  else
    {
      CHECK_NUMBER_COERCE_MARKER (start2, 4);
      begp2 = XINT (start2);
    }
  if (NILP (end2))
    endp2 = BUF_ZV (bp2);
  else
    {
      CHECK_NUMBER_COERCE_MARKER (end2, 5);
      endp2 = XINT (end2);
    }

  if (begp2 > endp2)
  {
    int temp = begp2; begp2 = endp2; endp2 = temp;
  }

  if (!(BUF_BEGV (bp2) <= begp2
	&& begp2 <= endp2
        && endp2 <= BUF_ZV (bp2)))
    args_out_of_range (start2, end2);

  len1 = endp1 - begp1;
  len2 = endp2 - begp2;
  length = len1;
  if (len2 < length)
    length = len2;

  for (i = 0; i < length; i++)
    {
      int c1 = *BUF_CHAR_ADDRESS (bp1, begp1 + i);
      int c2 = *BUF_CHAR_ADDRESS (bp2, begp2 + i);
      if (trt)
	{
	  c1 = trt[c1];
	  c2 = trt[c2];
	}
      if (c1 < c2)
	return make_number (- 1 - i);
      if (c1 > c2)
	return make_number (i + 1);
    }

  /* The strings match as far as they go.
     If one is shorter, that one is less.  */
  if (length < len1)
    return make_number (length + 1);
  else if (length < len2)
    return make_number (- length - 1);

  /* Same length too => they are equal.  */
  return Qzero;
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
  CHECK_FIXNUM (fromchar, 2);
  CHECK_FIXNUM (tochar, 3);

  pos = XINT (start);
  stop = XINT (end);
  look = XINT (fromchar);

  modify_region (current_buffer, pos, stop);
  if (! NILP (noundo))
    {
      if (MODIFF - 1 == current_buffer->save_modified)
	current_buffer->save_modified++;
      if (MODIFF - 1 == current_buffer->auto_save_modified)
	current_buffer->auto_save_modified++;
    }

  while (pos < stop)
    {
      if (FETCH_CHAR (pos) == look)
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
  register int pos, stop;	/* Limits of the region. */
  register unsigned char *tt;	/* Trans table. */
  register int oc;		/* Old character. */
  register int nc;		/* New character. */
  int cnt;			/* Number of changes made. */
  int size;			/* Size of translate table. */

  validate_region (&start, &end);
  CHECK_STRING (table, 2);

  size = XSTRING (table)->size;
  tt = XSTRING (table)->data;

  pos = XINT (start);
  stop = XINT (end);
  modify_region (current_buffer, pos, stop);

  cnt = 0;
  for (; pos < stop; ++pos)
    {
      oc = FETCH_CHAR (pos);
      if (oc < size)
	{
	  nc = tt[oc];
	  if (nc != oc)
	    {
	      record_change (pos, 1);
	      *(CHAR_ADDRESS (pos)) = nc;
	      signal_after_change (pos, 1, 1);
	      ++cnt;
	    }
	}
    }

  return make_number (cnt);
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
  /* Changing the buffer bounds invalidates any recorded current column.  */
  invalidate_current_column ();
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
  CHECK_FIXNUM_COERCE_MARKER (b, 0);
  CHECK_FIXNUM_COERCE_MARKER (e, 1);

  if (XINT (b) > XINT (e))
    {
      Lisp_Object tem;
      tem = b;
      b = e;
      e = tem;
    }

  if (!(BEG <= XINT (b) && XINT (b) <= XINT (e) && XINT (e) <= Z))
    args_out_of_range (b, e);

  BEGV = XFASTINT (b);
  SET_BUF_ZV (current_buffer, XFASTINT (e));
  if (point < XINT (b))
    SET_PT (XINT (b));
  if (point > XINT (e))
    SET_PT (XINT (e));
  clip_changed = 1;
  /* Changing the buffer bounds invalidates any recorded current column.  */
  invalidate_current_column ();
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
  bottom = make_number (BEGV - BEG);
  top = make_number (Z - ZV);

  return Fcons (Fcurrent_buffer (), Fcons (bottom, top));
}

Lisp_Object
save_restriction_restore (data)
     Lisp_Object data;
{
  struct buffer *buf;
  int newhead, newtail;
  Lisp_Object tem;

  buf = XBUFFER (Fcar (data));
  tem = Fcdr (data);
  newhead = XINT (Fcar (tem));
  newtail = XINT (Fcdr (tem));
  while (CONSP (data))
    {
      struct Lisp_Cons *victim = XCONS (data);
      data = victim->cdr;
      free_cons (victim);
    }

  if (newhead + newtail > BUF_Z (buf) - BUF_BEG (buf))
    {
      newhead = 0;
      newtail = 0;
    }
  BUF_BEGV (buf) = BUF_BEG (buf) + newhead;
  SET_BUF_ZV (buf, BUF_Z (buf) - newtail);
  clip_changed = 1;

  /* If point is outside the new visible range, move it inside. */
  SET_BUF_PT (buf,
              clip_to_bounds (BUF_BEGV (buf),
                              BUF_PT (buf),
                              BUF_ZV (buf)));

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
  int speccount = specpdl_depth ();

  record_unwind_protect (save_restriction_restore, save_restriction_save ());

  return unbind_to (speccount, Fprogn (body));
}

DEFUN ("message", Fmessage, Smessage, 1, MANY, 0,
  "Print a one-line message at the bottom of the screen.\n\
The first argument is a control string.\n\
It may contain %s or %d or %c to print successive following arguments.\n\
%s means print an argument as a string, %d means print as number in decimal,\n\
%c means print a number as a single character.\n\
The argument used by %s must be a string or a symbol;\n\
the argument used by %d or %c must be a number.\n\
\n\
If the only argument is nil, clear any existing message; let the\n\
minibuffer contents show.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  /* Caller is assumed to gcpro args */
  register Lisp_Object val;

#ifdef MULTI_SCREEN
  if (!NILP (Fscreenp (Vglobal_minibuffer_screen)))
    Fmake_screen_visible (Vglobal_minibuffer_screen);
#endif

  if (nargs == 1 && NILP (args[0]))
  {
    clear_message (1);
    return (Qnil);
  }
  else
    {
      val = Fformat (nargs, args);
      message ("%s", XSTRING (val)->data);  /* Loses \000's.  Unix sucks */
      return (val);
    }
}

DEFUN ("format", Fformat, Sformat, 1, MANY, 0,
  "Format a string out of a control-string and arguments.\n\
The first argument is a control string.\n\
The other arguments are substituted into it to make the result, a string.\n\
It may contain %-sequences meaning to substitute the next argument.\n\
%s means print strings using `princ' and other objects using `prin1'.\n\
%S means print all objects using `prin1' (including strings.)\n\
%d means print as number in decimal (%o octal, %x hex).\n\
%c means print a number as a single character.\n\
The argument used for %d, %o, %x or %c must be a number.\n\
Use %% to put a single % into the output.")
  (nargs, args)                 /* Note!! args side-effected! */
     int nargs;
     register Lisp_Object *args;
{
  /* Caller is assumed to gcpro ARGS */
  register int n;		/* The number of the next arg to substitute */
  register int total = 5;	/* An estimate of the final length */
  register unsigned char *format;
  register int fsize;
  int pos;

  CHECK_STRING (args[0], 0);

  format = XSTRING (args[0])->data;
  fsize = XSTRING (args[0])->size;

  /* We have to do so much work in order to prepare to call doprnt
     that we might as well do all of it ourself...  (Which would also
     circumvent C asciz cretinism by allowing ascii 000 chars to appear)
   */
  n = 0;
  pos = 0;
  while (1)
    {
      static const char printf_arguments[] = "0123456789- .";
      Lisp_Object argn;
      int minlen;
      int ch;
      unsigned char *last = format;

      format = (unsigned char *) memchr (format, '%', fsize - pos);
      if (!format)
	{
	  total += fsize - pos;
	  break;
	}
      else
	{
	  int delta = format - last;
	  total += delta;
	  pos += delta;
	}

      ch = *++format;
      pos++;
      /* Process a numeric arg and skip it.  */
      if (strchr (printf_arguments, ch))
	{
	  minlen = atoi ((char *) format);
	  if (minlen > 0)
	    total += minlen;
	  else
	    total -= minlen;
	  while (1)
	    {
	      if (pos >= fsize)
		signal_error (Qerror,
			      list2 (build_string 
				     ("Unterminated format specification in"),
				     args[0]));
	      pos++;
	      ch = *++format;
	      if (!strchr (printf_arguments, ch))
		break;
	    }
	}

      if (ch == '%')
	{
	  /* %% means insert n %'s */
	  format++;
	  pos++;
	  continue;
	}
      else if (++n >= nargs)
	/* Ran out of arguments -- get WNA error below */
	continue;

      argn = args[n];
      if (ch == 'S' || ch == 's')
	{
	  if (ch == 's' && STRINGP (argn))
	    goto string;
	  /* For `S', prin1 the argument and then treat like a string.  */
	  argn = Fprin1_to_string (argn, Qnil);
	relocate:
	  args[n] = argn;
	  /* In case GC relocated it */
	  format = XSTRING (args[0])->data + pos;
	string:
	  total += XSTRING (argn)->size;
	}
      else if (SYMBOLP (argn))
	{
	  /* princ symbols */
	  XSET (argn, Lisp_String, XSYMBOL (argn)->name);
	  args[n] = argn;
	  goto string;
	}
      else if (STRINGP (argn))
	{
	  goto string;
	}
      else if (FIXNUMP (argn))
	{
#ifdef LISP_FLOAT_TYPE
	  /* The following loop assumes the Lisp type indicates
	     the proper way to pass the argument.
	     So make sure we have a flonum if the argument should
	     be a double.  */
	  /* >>> This is bogus -- emacs_doprnt doesn't even hack floats!! */
	  if (ch == 'e' || ch == 'f' || ch == 'g')
	    args[n] = Ffloat (argn);
#endif
	  total += 10;
	}
#ifdef LISP_FLOAT_TYPE
      else if (FLOATP (argn))
	{
	  /* >>> This is bogus -- emacs_doprnt doesn't even hack floats!! */
	  if (! (ch == 'e' || ch == 'f' || ch == 'g'))
	    argn = Ftruncate (argn);
	  total += 20;
	}
#endif
      else
	{
	  /* Anything but a string or number; convert to string using princ. */
	  argn = Fprin1_to_string (argn, Qt);
	  goto relocate;
	}
    }

  if (n > nargs - 1)	/* allow too many args for string, but not too few */
    return Fsignal (Qwrong_number_of_arguments,
                    list3 (Qformat,
                           make_number (nargs - 1), 
                           args[0]));

  /* Format it in bigger and bigger buf's until it all fits. */
  while (1)
    {
      char *buf = (char *) alloca (total + 1);
      int len;
      len = emacs_doprnt_lisp (buf, total + 1,
			       (char *) XSTRING (args[0])->data, fsize,
			       nargs - 1,
			       args + 1);

      if (len <= total)
	return (make_string (buf, len));

      total *= 2;
    }
}

/* VARARGS 1 */
Lisp_Object
format1 (const char *fmt, ...)
{
  char buf[200];
  va_list args;

  va_start (args, fmt);

  emacs_doprnt (buf, sizeof (buf), 
                fmt, -1,
                -1, args);

  va_end (args);

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
  CHECK_FIXNUM (c1, 0);
  CHECK_FIXNUM (c2, 1);

  if (!NILP (current_buffer->case_fold_search)
      ? (downcase[0xff & XFASTINT (c1)] == downcase[0xff & XFASTINT (c2)]
	 && (XFASTINT (c1) & ~0xff) == (XFASTINT (c2) & ~0xff))
      : XINT (c1) == XINT (c2))
    return Qt;
  return Qnil;
}

DEFUN ("getenv", Fgetenv, Sgetenv, 1, 2, "sEnvironment variable: \np",
  "Return the value of environment variable VAR, as a string.\n\
When invoked interactively, print the value in the echo area.\n\
VAR is a string, the name of the variable,\n\
 or the symbol t, meaning to return an alist representing the\n\
 current environment.")
  (str, interactivep)
     Lisp_Object str, interactivep;
{
  Lisp_Object value;
#ifdef MAINTAIN_ENVIRONMENT
  value = lisp_getenv (str);
#else
  register char *val;
  if (EQ (str, Qt))
    error (
     "(getenv t) is not supported unless compiled with MAINTAIN_ENVIRONMENT");
  CHECK_STRING (str, 0);
  val = (char *) egetenv (XSTRING (str)->data);
  value = (val ? build_string (val) : Qnil);
#endif

  if (!NILP (interactivep))
    {
      if (NILP (value))
	message ("%s not defined in environment", XSTRING (str)->data);
      else
	message ("\"%s\"", XSTRING (value)->data);
    }
  return value;
}

void
syms_of_editfns ()
{
  staticpro (&Vsystem_name);
  staticpro (&Vuser_full_name);
  staticpro (&Vuser_name);
  staticpro (&Vuser_real_name);

  DEFVAR_BOOL ("zmacs-regions", &zmacs_regions,
	       "*Whether LISPM-style active regions should be used.\n\
This means that commands which operate on the region (the area between the\n\
point and the mark) will only work while the region is in the ``active''\n\
state, which is indicated by highlighting.  Executing most commands causes\n\
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
highlighted should set this to t.  Normally, the region is turned off after\n\
executing each command that did not explicitly turn it on with the function\n\
zmacs-activate-region. Setting this to true lets a command be non-intrusive.\n\
See the variable `zmacs-regions'.");
  Vzmacs_update_region_hook = Qnil;
  Vzmacs_activate_region_hook = Qnil;
  Vzmacs_deactivate_region_hook = Qnil;
  zmacs_region_stays = 0;

  defsymbol (&Qzmacs_update_region_hook, "zmacs-update-region-hook");
  defsymbol (&Qzmacs_activate_region_hook, "zmacs-activate-region-hook");
  defsymbol (&Qzmacs_deactivate_region_hook, "zmacs-deactivate-region-hook");

  defsymbol (&Qpoint, "point");
  defsymbol (&Qmark, "mark");
  defsymbol (&Qregion_beginning, "region-beginning");
  defsymbol (&Qregion_end, "region-end");
  defsymbol (&Qformat, "format");

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
  defsubr (&Sfollowing_char);
  defsubr (&Sprevious_char);
  defsubr (&Schar_after);
  defsubr (&Sinsert);
  defsubr (&Sinsert_before_markers);
  defsubr (&Sinsert_char);

  defsubr (&Suser_login_name);
  defsubr (&Suser_real_login_name);
  defsubr (&Suser_uid);
  defsubr (&Suser_real_uid);
  defsubr (&Suser_full_name);
  defsubr (&Scurrent_time);
  defsubr (&Scurrent_time_string);
  defsubr (&Scurrent_time_zone);
  defsubr (&Ssystem_name);
  defsubr (&Smessage);
  defsubr (&Sformat);
  defsubr (&Sgetenv);

  defsubr (&Sinsert_buffer_substring);
  defsubr (&Scompare_buffer_substrings);
  defsubr (&Ssubst_char_in_region);
  defsubr (&Stranslate_region);
  defsubr (&Sdelete_region);
  defsubr (&Swiden);
  defsubr (&Snarrow_to_region);
  defsubr (&Ssave_restriction);
  
  defsubr (&Szmacs_activate_region);
  defsubr (&Szmacs_deactivate_region);
}
