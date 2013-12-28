/* Lisp parsing and input streams.
   Copyright (C) 1992 Free Software Foundation, Inc.

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


#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>

#include "config.h"
#include "lisp.h"

#ifndef standalone
#include "buffer.h"
#include "paths.h"
#include "commands.h"
#endif

#ifdef lint
#include <sys/inode.h>
#endif /* lint */

#ifndef X_OK
#define X_OK 01
#endif

#ifdef LISP_FLOAT_TYPE
#include <math.h>
#endif /* LISP_FLOAT_TYPE */

Lisp_Object Qread_char, Qget_file_char, Qstandard_input;
Lisp_Object Qvariable_documentation, Vvalues, Vstandard_input, Vafter_load_alist;

/* non-zero if inside `load' */
int load_in_progress;

/* Search path for files to be loaded. */
Lisp_Object Vload_path;

/* File for get_file_char to read from.  Use by load */
static FILE *instream;

/* When nonzero, read conses in pure space */
static int read_pure;

/* For use within read-from-string (this reader is non-reentrant!!) */
static int read_from_string_index;
static int read_from_string_limit;

/* Handle unreading and rereading of characters.
   Write READCHAR to read a character,
   UNREAD(c) to unread c to be read again. */

#define READCHAR readchar (readcharfun)
#define UNREAD(c) unreadchar (readcharfun, c)

static int
readchar (readcharfun)
     Lisp_Object readcharfun;
{
  Lisp_Object tem;
  register struct buffer *inbuffer;
  register int c, mpos;

  if (BUFFERP (readcharfun))
    {
      inbuffer = XBUFFER (readcharfun);

      if (BUF_PT (inbuffer) >= BUF_ZV (inbuffer))
	return -1;
      c = *(unsigned char *) BUF_CHAR_ADDRESS (inbuffer, BUF_PT (inbuffer));
      SET_BUF_PT (inbuffer, BUF_PT (inbuffer) + 1);

      return c;
    }
  if (MARKERP (readcharfun))
    {
      inbuffer = XMARKER (readcharfun)->buffer;

      mpos = marker_position (readcharfun);

      if (mpos > BUF_ZV (inbuffer) - 1)
	return -1;
      c = *(unsigned char *) BUF_CHAR_ADDRESS (inbuffer, mpos);
      if (mpos != BUF_GPT (inbuffer))
	XMARKER (readcharfun)->bufpos++;
      else
	Fset_marker (readcharfun, make_number (mpos + 1),
		     Fmarker_buffer (readcharfun));
      return c;
    }
  if (EQ (readcharfun, Qget_file_char))
    return getc (instream);

  if (STRINGP (readcharfun))
    {
      register int c;
      /* This used to be return of a conditional expression,
	 but that truncated -1 to a char on VMS.  */
      if (read_from_string_index < read_from_string_limit)
	c = XSTRING (readcharfun)->data[read_from_string_index++];
      else
	c = -1;
      return c;
    }

  tem = call0 (readcharfun);

  if (NILP (tem))
    return -1;
  return XINT (tem);
}

/* Unread the character C in the way appropriate for the stream READCHARFUN.
   If the stream is a user function, call it with the char as argument.  */

static void
unreadchar (readcharfun, c)
     Lisp_Object readcharfun;
     int c;
{
  if (BUFFERP (readcharfun))
    {
      if (XBUFFER (readcharfun) == current_buffer)
	SET_PT (point - 1);
      else
	SET_BUF_PT (XBUFFER (readcharfun), BUF_PT (XBUFFER (readcharfun)) - 1);
    }
  else if (MARKERP (readcharfun))
    XMARKER (readcharfun)->bufpos--;
  else if (STRINGP (readcharfun))
    read_from_string_index--;
  else if (EQ (readcharfun, Qget_file_char))
    ungetc (c, instream);
  else
    call1 (readcharfun, make_number (c));
}

static Lisp_Object read0 (), read1 (), read_list (), read_vector ();

/* get a character from the tty */

#ifdef standalone     /* This is normally defined in event-stream.c */

DEFUN ("read-char", Fread_char, Sread_char, 0, 0, 0, "") ()
{
  return getchar ();
}

#endif


DEFUN ("get-file-char", Fget_file_char, Sget_file_char, 0, 0, 0,
  "Don't use this yourself.")
  ()
{
  register Lisp_Object val;
  XSET (val, Lisp_Int, getc (instream));
  return val;
}

static void readevalloop ();
static Lisp_Object load_unwind ();

DEFUN ("load", Fload, Sload, 1, 4, 0,
  "Execute a file of Lisp code named FILE.\n\
First try FILE with `.elc' appended, then try with `.el',\n\
 then try FILE unmodified.\n\
This function searches the directories in `load-path'.\n\
If optional second arg MISSING-OK is non-nil,\n\
 report no error if FILE doesn't exist.\n\
Print messages at start and end of loading unless\n\
 optional third arg NOMESSAGE is non-nil.\n\
If optional fourth arg NOSUFFIX is non-nil, don't try adding\n\
 suffixes `.elc' or `.el' to the specified name FILE.\n\
Return t if file exists.")
  (str, missing_ok, nomessage, nosuffix)
     Lisp_Object str, missing_ok, nomessage, nosuffix;
{
  register FILE *stream;
  register int fd = -1;
  register Lisp_Object lispstream;
  register FILE **ptr;
  int count = specpdl_ptr - specpdl;
  Lisp_Object temp;
  struct gcpro gcpro1;

  CHECK_STRING (str, 0);
  str = Fsubstitute_in_file_name (str);

  /* Avoid weird lossage with null string as arg,
     since it would try to load a directory as a Lisp file */
  if (XSTRING (str)->size > 0)
    {
      fd = openp (Vload_path, str, !NILP (nosuffix) ? "" : ".elc:.el:", 0, 0);
    }

  if (fd < 0)
    if (NILP (missing_ok))
      while (1)
	Fsignal (Qfile_error, Fcons (build_string ("Cannot open load file"),
				     Fcons (str, Qnil)));
    else return Qnil;

  stream = fdopen (fd, "r");
  if (stream == 0)
    {
      close (fd);
      error ("Failure to create stdio stream for %s", XSTRING (str)->data);
    }

  if (NILP (nomessage))
    message ("Loading %s...", XSTRING (str)->data);

  GCPRO1 (str);
  /* We may not be able to store STREAM itself as a Lisp_Object pointer
     since that is guaranteed to work only for data that has been malloc'd.
     So malloc a full-size pointer, and record the address of that pointer.  */
  ptr = (FILE **) xmalloc (sizeof (FILE *));
  *ptr = stream;
  XSET (lispstream, Lisp_Internal_Stream, (int) ptr);
  record_unwind_protect (load_unwind, lispstream);
  load_in_progress++;
  readevalloop (Qget_file_char, stream, Feval, 0);
  unbind_to (count);

  /* Run any load-hooks for this file.  */
  temp = Fassoc (str, Vafter_load_alist);
  if (!NILP (temp))
    Fprogn (Fcdr (temp));
  UNGCPRO;

  if (!noninteractive && NILP (nomessage))
    message ("Loading %s...done", XSTRING (str)->data);
  return Qt;
}

static Lisp_Object
load_unwind (stream)  /* used as unwind-protect function in load */
     Lisp_Object stream;
{
  fclose (*(FILE **) XSTRING (stream));
  free ((char *)XPNTR (stream));
  if (--load_in_progress < 0) load_in_progress = 0;
  return Qnil;
}


static int
complete_filename_p (pathname)
     Lisp_Object pathname;
{
  register unsigned char *s = XSTRING (pathname)->data;
  return (*s == '/'
#ifdef ALTOS
	  || *s == '@'
#endif
#ifdef VMS
	  || index (s, ':')
#endif /* VMS */
	  );
}

DEFUN ("locate-file", Flocate_file, Slocate_file, 2, 4, 0,
  "Search for FILENAME through PATH-LIST, expanded by one of the optional\n\
SUFFIXES (string of suffixes separated by \":\"s), checking for access\n\
MODE (0|1|2|4 = exists|executable|writeable|readable), default readable.")
  (file, path, suff, mode)
     Lisp_Object file, path, suff, mode;
{
  Lisp_Object tp;
  char *sstr = "";
  CHECK_STRING (file, 0);
  if (!NILP (suff))
    {
      CHECK_STRING (suff, 0);
      sstr = (char *)(XSTRING (suff)->data);
    }
  if (!(NILP (mode) || (FIXNUMP (mode) && XINT (mode) >= 0)))
    mode = wrong_type_argument (Qnatnump, mode);
  locate_file (path, file, sstr, &tp, (NILP (mode) ? R_OK : XFASTINT (mode)));
  return tp;
}


/* Search for a file whose name is STR, looking in directories
   in the Lisp list PATH, and trying suffixes from SUFFIX.
   SUFFIX is a string containing possible suffixes separated by colons.
   On success, returns a file descriptor.  On failure, returns -1.

   MODE nonnegative means don't open the files,
   just look for one for which access(file,MODE) succeeds.  In this case,
   returns 1 on success.

   If STOREPTR is nonzero, it points to a slot where the name of
   the file actually found should be stored as a Lisp string.
   Nil is stored there on failure.  */

int
locate_file (path, str, suffix, storeptr, mode)
     Lisp_Object path, str;
     char *suffix;
     Lisp_Object *storeptr;
     int mode;
{
  register int fd;
  int fn_size = 100;
  char buf[100];
  register char *fn = buf;
  int absolute = 0;
  int want_size;
  register Lisp_Object filename;
  struct stat st;

  if (storeptr)
    *storeptr = Qnil;

  if (complete_filename_p (str))
    absolute = 1;

  for (; !NILP (path); path = Fcdr (path))
    {
      char *nsuffix;

      filename = Fexpand_file_name (str, Fcar (path));
      if (!complete_filename_p (filename))
	/* If there are non-absolute elts in PATH (eg ".") */
	/* Of course, this could conceivably lose if luser sets
	   default-directory to be something non-absolute... */
	{
	  filename = Fexpand_file_name (filename, current_buffer->directory);
	  if (!complete_filename_p (filename))
	    /* Give up on this path element! */
	    continue;
	}

      /* Calculate maximum size of any filename made from
	 this path element/specified file name and any possible suffix.  */
      want_size = strlen (suffix) + XSTRING (filename)->size + 1;
      if (fn_size < want_size)
	fn = (char *) alloca (fn_size = 100 + want_size);

      nsuffix = suffix;

      /* Loop over suffixes.  */
      while (1)
	{
	  char *esuffix = (char *) index (nsuffix, ':');
	  int lsuffix = esuffix ? esuffix - nsuffix : strlen (nsuffix);

	  /* Concatenate path element/specified name with the suffix.  */
	  strncpy (fn, XSTRING (filename)->data, XSTRING (filename)->size);
	  fn[XSTRING (filename)->size] = 0;
	  if (lsuffix != 0)  /* Bug happens on CCI if lsuffix is 0.  */
	    strncat (fn, nsuffix, lsuffix);

	  /* Ignore file if it's a directory.  */
	  if (stat (fn, &st) >= 0
	      && (st.st_mode & S_IFMT) != S_IFDIR)
	    {
	      /* Check that we can access or open it.  */
	      if (mode>=0)
		fd = access (fn, mode);
	      else
		fd = open (fn, 0, 0);

	      if (fd >= 0)
		{
		  /* We succeeded; return this descriptor and filename.  */
		  if (storeptr)
		    *storeptr = build_string (fn);
		  return fd;
		}
	    }

	  /* Advance to next suffix.  */
	  if (esuffix == 0)
	    break;
	  nsuffix += lsuffix + 1;
	}
      if (absolute) return -1;
    }

  return -1;
}


Lisp_Object
unreadpure (dummy)	/* Used as unwind-protect function in readevalloop */
     Lisp_Object dummy;
{
  read_pure = 0;
  return Qnil;
}

static void
readevalloop (readcharfun, stream, evalfun, printflag)
     Lisp_Object readcharfun;
     FILE *stream;     
     Lisp_Object (*evalfun) ();
     int printflag;
{
  register int c;
  register Lisp_Object val;
  int count = specpdl_ptr - specpdl;

  specbind (Qstandard_input, readcharfun);

  while (1)
    {
      instream = stream;
      c = READCHAR;
      if (c == ';')
	{
	  while ((c = READCHAR) != '\n' && c != -1);
	  continue;
	}
      if (c < 0) break;
      if (c == ' ' || c == '\t' || c == '\n' || c == '\f') continue;

      if (!NILP (Vpurify_flag) && c == '(')
	{
	  record_unwind_protect (unreadpure, Qnil);
	  val = read_list (-1, readcharfun);
	  unbind_to (count + 1);
	}
      else
	{
	  UNREAD (c);
	  val = read0 (readcharfun);
	}

      val = (*evalfun) (val);
      if (printflag)
	{
	  Vvalues = Fcons (val, Vvalues);
	  if (EQ (Vstandard_output, Qt))
	    Fprin1 (val, Qnil);
	  else
	    Fprint (val, Qnil);
	}
    }

  unbind_to (count);
}

#ifndef standalone

DEFUN ("eval-buffer", Feval_buffer, Seval_buffer, 1, 2, "bBuffer: ",
  "Execute BUFFER as Lisp code.\n\
Programs can pass argument PRINTFLAG which controls printing of output:\n\
nil means discard it; anything else is stream for print.")
  (bufname, printflag)
     Lisp_Object bufname, printflag;
{
  int count = specpdl_ptr - specpdl;
  Lisp_Object tem, buf;

  buf = Fget_buffer (bufname);
  if (NILP (buf))
    error ("No such buffer.");

  if (NILP (printflag))
    tem = Qsymbolp;
  else
    tem = printflag;
  specbind (Qstandard_output, tem);
  record_unwind_protect (save_excursion_restore, save_excursion_save ());
  SET_BUF_PT (XBUFFER (buf), BUF_BEGV (XBUFFER (buf)));
  readevalloop (buf, 0, Feval, !NILP (printflag));
  unbind_to (count);

  return Qnil;
}

DEFUN ("eval-region", Feval_region, Seval_region, 2, 3, "r",
  "Execute the region as Lisp code.\n\
When called from programs, expects two arguments,\n\
giving starting and ending indices in the current buffer\n\
of the text to be executed.\n\
Programs can pass third argument PRINTFLAG which controls output:\n\
nil means discard it; anything else is stream for printing it.\n\
\n\
If there is no error, point does not move.  If there is an error,\n\
point remains at the end of the last character read from the buffer.")
  (b, e, printflag)
     Lisp_Object b, e, printflag;
{
  int count = specpdl_ptr - specpdl;
  Lisp_Object tem;

  if (NILP (printflag))
    tem = Qsymbolp;
  else
    tem = printflag;
  specbind (Qstandard_output, tem);
  if (NILP (printflag))
    record_unwind_protect (save_excursion_restore, save_excursion_save ());
  record_unwind_protect (save_restriction_restore, save_restriction_save ());
  SET_PT (XINT (b));
  Fnarrow_to_region (make_number (BEGV), e);
  readevalloop (Fcurrent_buffer (), 0, Feval, !NILP (printflag));
  unbind_to (count);
  return Qnil;
}

#endif /* standalone */

DEFUN ("read", Fread, Sread, 0, 1, 0,
  "Read one Lisp expression as text from STREAM, return as Lisp object.\n\
If STREAM is nil, use the value of `standard-input' (which see).\n\
STREAM or the value of `standard-input' may be:\n\
 a buffer (read from point and advance it)\n\
 a marker (read from where it points and advance it)\n\
 a function (call it with no arguments for each character,\n\
     call it with a char as argument to push a char back)\n\
 a string (takes text from string, starting at the beginning)\n\
 t (read text line using minibuffer and use it).")
  (readcharfun)
     Lisp_Object readcharfun;
{
  extern Lisp_Object Fread_minibuffer ();

  if (NILP (readcharfun))
    readcharfun = Vstandard_input;
  if (EQ (readcharfun, Qt))
    readcharfun = Qread_char;

#ifndef standalone
  if (EQ (readcharfun, Qread_char))
    return Fread_minibuffer (build_string ("Lisp expression: "), Qnil);
#endif

  if (STRINGP (readcharfun))
    return Fcar (Fread_from_string (readcharfun, Qnil, Qnil));

  return read0 (readcharfun);
}

DEFUN ("read-from-string", Fread_from_string, Sread_from_string, 1, 3, 0,
  "Read one Lisp expression which is represented as text by STRING.\n\
Returns a cons: (OBJECT-READ . FINAL-STRING-INDEX).\n\
START and END optionally delimit a substring of STRING from which to read;\n\
 they default to 0 and (length STRING) respectively.")
  (string, start, end)
     Lisp_Object string, start, end;
{
  int startval, endval;
  Lisp_Object tem;

  CHECK_STRING (string,0);

  if (NILP (end))
    endval = XSTRING (string)->size;
  else
    { CHECK_FIXNUM (end,2);
      endval = XINT (end);
      if (endval < 0 || endval > XSTRING (string)->size)
	args_out_of_range (string, end);
    }

  if (NILP (start))
    startval = 0;
  else
    { CHECK_FIXNUM (start,1);
      startval = XINT (start);
      if (startval < 0 || startval > endval)
	args_out_of_range (string, start);
    }

  read_from_string_index = startval;
  read_from_string_limit = endval;

  tem = read0 (string);
  return Fcons (tem, make_number (read_from_string_index));
}

/* Use this for recursive reads, in contexts where internal tokens are not allowed. */

static Lisp_Object
read0 (readcharfun)
     Lisp_Object readcharfun;
{
  register Lisp_Object val;
  char c;

  val = read1 (readcharfun);
  if (XTYPE (val) == Lisp_Internal)
    {
      c = XINT (val);
      return Fsignal (Qinvalid_read_syntax, Fcons (make_string (&c, 1), Qnil));
    }

  return val;
}

static int read_buffer_size;
static char *read_buffer;

static int
read_escape (readcharfun)
     Lisp_Object readcharfun;
{
  register int c = READCHAR;
  switch (c)
    {
    case 'a':
      return 7; 	/* some systems don't know '\a' */
    case 'b':
      return '\b';
    case 'e':
      return 033;
    case 'f':
      return '\f';
    case 'n':
      return '\n';
    case 'r':
      return '\r';
    case 't':
      return '\t';
    case 'v':
      return '\v';
    case '\n':
      return -1;

    case 'M':
      c = READCHAR;
      if (c != '-')
	error ("Invalid escape character syntax");
      c = READCHAR;
      if (c == '\\')
	c = read_escape (readcharfun);
      return c | 0200;

    case 'C':
      c = READCHAR;
      if (c != '-')
	error ("Invalid escape character syntax");
    case '^':
      c = READCHAR;
      if (c == '\\')
	c = read_escape (readcharfun);
      if (c == '?')
	return 0177;
      return (c & 0200) | (c & 037);
      
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
      /* An octal escape, as in ANSI C.  */
      {
	register int i = c - '0';
	register int count = 0;
	while (++count < 3)
	  {
	    if ((c = READCHAR) >= '0' && c <= '7')
	      {
		i *= 8;
		i += c - '0';
	      }
	    else
	      {
		UNREAD (c);
		break;
	      }
	  }
	return i;
      }

    case 'x':
      /* A hex escape, as in ANSI C.  */
      {
	int i = 0;
	while (1)
	  {
	    c = READCHAR;
	    if (c >= '0' && c <= '9')
	      {
		i *= 16;
		i += c - '0';
	      }
	    else if ((c >= 'a' && c <= 'f')
		     || (c >= 'A' && c <= 'F'))
	      {
		i *= 16;
		if (c >= 'a' && c <= 'f')
		  i += c - 'a' + 10;
		else
		  i += c - 'A' + 10;
	      }
	    else
	      {
		UNREAD (c);
		break;
	      }
	  }
	return i;
      }

    default:
      return c;
    }
}

static Lisp_Object
read1 (readcharfun)
     register Lisp_Object readcharfun;
{
  register int c;
  int uninterned_symbol = 0;

 retry:

  c = READCHAR;
  if (c < 0) return Fsignal (Qend_of_file, Qnil);

  switch (c)
    {
    case '(':
      return read_list (0, readcharfun);

    case '[':
#ifdef PURESTAT
      return read_vector (readcharfun, Lisp_Vector);
#else
      return read_vector (readcharfun);
#endif

    case ')':
    case ']':
    case '.':
      {
	register Lisp_Object val;
	XSET (val, Lisp_Internal, c);
	return val;
      }

    case '#':
      c = READCHAR;
      if (c == '[')
	{
	  /* accept compiled functions at read-time so that we don't have to
	     build them at load-time. */
#ifdef PURESTAT
	  Lisp_Object vec = read_vector (readcharfun, Lisp_Compiled);
#else
	  Lisp_Object vec = read_vector (readcharfun);
#endif
	  XSETTYPE (vec, Lisp_Compiled);
	  if (XVECTOR (vec)->size < 4 || XVECTOR (vec)->size > 6)
	    return Fsignal (Qinvalid_read_syntax,
			    Fcons (build_string
				 ("#[...] used with wrong number of elements"),
				   Qnil));
	  return (NILP (Vpurify_flag) ? vec : Fpurecopy (vec));
	}
      else if (c == ':')
	{
	  uninterned_symbol = 1;
	  c = READCHAR;
	  goto UNINTERNED_SYMBOL;
	}
      
      UNREAD (c);
      return Fsignal (Qinvalid_read_syntax, Fcons (make_string ("#", 1), Qnil));

    case ';':
      while ((c = READCHAR) >= 0 && c != '\n');
      goto retry;

    case '\'':
      {
	return Fcons (Qquote, Fcons (read0 (readcharfun), Qnil));
      }

    case '?':
      {
	register Lisp_Object val;

	c = READCHAR;
	if (c < 0) return Fsignal (Qend_of_file, Qnil);

	if (c == '\\')
	  XSET (val, Lisp_Int, read_escape (readcharfun));
	else
	  XSET (val, Lisp_Int, c);

	return val;
      }

    case '\"':
      {
	register char *p = read_buffer;
	register char *end = read_buffer + read_buffer_size;
	register int c;
	int cancel = 0;

	while ((c = READCHAR) >= 0
	       && c != '\"')
	  {
	    if (p == end)
	      {
		char *new = (char *) xrealloc (read_buffer, read_buffer_size *= 2);
		p = new + (p - read_buffer);
		read_buffer = new;
		end = read_buffer + read_buffer_size;
	      }
	    if (c == '\\')
	      c = read_escape (readcharfun);
	    /* c is -1 if \ newline has just been seen */
	    if (c < 0)
	      {
		if (p == read_buffer)
		  cancel = 1;
	      }
	    else
	      *p++ = c;
	  }
	if (c < 0) return Fsignal (Qend_of_file, Qnil);

	/* If purifying, and string starts with \ newline,
	   return zero instead.  This is for doc strings
	   that we are really going to find in etc/DOC.nn.nn  */
	if (!NILP (Vpurify_flag) && NILP (Vdoc_file_name) && cancel)
	  return make_number (0);

	if (read_pure)
	  return make_pure_string (read_buffer, p - read_buffer);
	else
	  return make_string (read_buffer, p - read_buffer);
      }

    default:
      uninterned_symbol = 0;
      if (c <= 040) goto retry;
    UNINTERNED_SYMBOL:
      {
	register char *p = read_buffer;
	int saw_a_backslash = 0; /* whether there were backslashes in token */

	{
	  register char *end = read_buffer + read_buffer_size;

	  while (c > 040 && 
		 !(c == '\"' || c == '\'' || c == ';' || c == '?'
		   || c == '(' || c == ')'
#ifndef LISP_FLOAT_TYPE		/* we need to see <number><dot><number> */
		   || c =='.'
#endif /* not LISP_FLOAT_TYPE */
		   || c == '[' || c == ']' || c == '#'
		   ))
	    {
	      if (p == end)
		{
		  register char *new = (char *) xrealloc (read_buffer, read_buffer_size *= 2);
		  p += new - read_buffer;
		  read_buffer += new - read_buffer;
		  end = read_buffer + read_buffer_size;
		}
	      if (c == '\\')
		{
		  c = READCHAR;
		  saw_a_backslash = 1;
		}
	      *p++ = c;
	      c = READCHAR;
	    }

	  if (p == end)
	    {
	      char *new = (char *) xrealloc (read_buffer, read_buffer_size *= 2);
	      p += new - read_buffer;
	      read_buffer += new - read_buffer;
/*	      end = read_buffer + read_buffer_size;  */
	    }
	  *p = 0;
	  if (c >= 0)
	    UNREAD (c);
	}

	/* Is it an integer? */
	if (! (saw_a_backslash || uninterned_symbol))
	  /* If a token had any backslashes in it, it is disqualified from
	     being an integer or a float.  This means that 123\456 is a
	     symbol, as is \123 (which is the way (intern "123") prints.)
	     Also, if token was preceeded by #:, it's always a symbol.
	   */
	{
	  register char *p1;
	  register Lisp_Object val;
	  p1 = read_buffer;
	  if (*p1 == '+' || *p1 == '-') p1++;
	  if (p1 != p)
	    {
	      while (p1 != p && (c = *p1) >= '0' && c <= '9') p1++;
	      if (p1 == p)
		/* It is. */
		{
		  int number = 0;

#if 0
		  /* This change breaks many elisp applications. */
		  if (read_buffer[0] == '0')
		    {
		      p = read_buffer;
		      /* Lucid fix */
		      while (p1 != p) {
			
			if (*p < '0' || *p > '7')
			  return Fsignal
			    (Qinvalid_read_syntax,
			     Fcons (make_string ("non-octal digit", 15),
				    Qnil));
			
			number = (number << 3) + *p++ - '0';
		      }
		    }
		  else
#endif
		    number = atoi (read_buffer);

		  XSET (val, Lisp_Int, number);
		  return val;
		}
	    }
#ifdef LISP_FLOAT_TYPE
	  if (isfloat_string (read_buffer))
	    return make_float (atof (read_buffer));
#endif
	}

	if (uninterned_symbol)
	  {
	    if (read_pure)
	      return Fmake_symbol (make_pure_string (read_buffer,
						     strlen (read_buffer)));
	    else
	      return Fmake_symbol (make_string (read_buffer,
						strlen (read_buffer)));
	  }
	else
	  return intern (read_buffer);
      }
    }
}

#ifdef LISP_FLOAT_TYPE

#include <ctype.h>
#define LEAD_INT 1
#define DOT_CHAR 2
#define TRAIL_INT 4
#define E_CHAR 8
#define EXP_INT 16

int
isfloat_string (cp)
     register char *cp;
{
  register state;
  
  state = 0;
  if (*cp == '+' || *cp == '-')
    cp++;

  if (isdigit(*cp))
    {
      state |= LEAD_INT;
      while (isdigit (*cp))
	cp ++;
    }
  if (*cp == '.')
    {
      state |= DOT_CHAR;
      cp++;
    }
  if (isdigit(*cp))
    {
      state |= TRAIL_INT;
      while (isdigit (*cp))
	cp++;
    }
  if (*cp == 'e')
    {
      state |= E_CHAR;
      cp++;
    }
  if ((*cp == '+') || (*cp == '-'))
    cp++;

  if (isdigit (*cp))
    {
      state |= EXP_INT;
      while (isdigit (*cp))
	cp++;
    }
  return (*cp == 0
	  && (state == (LEAD_INT|DOT_CHAR|TRAIL_INT)
	      || state == (LEAD_INT|E_CHAR|EXP_INT)
	      || state == (LEAD_INT|DOT_CHAR|TRAIL_INT|E_CHAR|EXP_INT)));
}
#endif /* LISP_FLOAT_TYPE */

static Lisp_Object
#ifdef PURESTAT
read_vector (readcharfun, stat_type)
     Lisp_Object readcharfun;
     enum Lisp_Type stat_type;
#else
read_vector (readcharfun)
     Lisp_Object readcharfun;
#endif
{
  register int i;
  register int size;
  register Lisp_Object *ptr;
  register Lisp_Object tem, vector;
  register struct Lisp_Cons *otem;
  Lisp_Object len;

  tem = read_list (1, readcharfun);
  len = Flength (tem);
  vector = (read_pure
#ifdef PURESTAT
	    ? make_pure_vector (XINT (len), stat_type)
#else
	    ? make_pure_vector (XINT (len))
#endif
	    : Fmake_vector (len, Qnil));


  size = XVECTOR (vector)->size;
  ptr = XVECTOR (vector)->contents;
  for (i = 0; i < size; i++)
    {
      ptr[i] = read_pure ? Fpurecopy (Fcar (tem)) : Fcar (tem);
      otem = XCONS (tem);
      tem = Fcdr (tem);
      free_cons (otem);
    }
  return vector;
}
  
/* flag = 1 means check for ] to terminate rather than ) and .
   flag = -1 means check for starting with defun
    and make structure pure.  */

static Lisp_Object
read_list (flag, readcharfun)
     int flag;
     register Lisp_Object readcharfun;
{
  /* -1 means check next element for defun,
     0 means don't check,
     1 means already checked and found defun. */
  int defunflag = flag < 0 ? -1 : 0;
  Lisp_Object val, tail;
  register Lisp_Object elt, tem;
  struct gcpro gcpro1, gcpro2;

  val = Qnil;
  tail = Qnil;

  while (1)
    {
      GCPRO2 (val, tail);
      elt = read1 (readcharfun);
      UNGCPRO;
      if (XTYPE (elt) == Lisp_Internal)
	{
	  if (flag > 0)
	    {
	      if (XINT (elt) == ']')
		return val;
	      return Fsignal (Qinvalid_read_syntax, Fcons (make_string (") or . in a vector", 18), Qnil));
	    }
	  if (XINT (elt) == ')')
	    return val;
	  if (XINT (elt) == '.')
	    {
	      GCPRO2 (val, tail);
	      if (!NILP (tail))
		XCONS (tail)->cdr = read0 (readcharfun);
	      else
		val = read0 (readcharfun);
	      elt = read1 (readcharfun);
	      UNGCPRO;
	      if (XTYPE (elt) == Lisp_Internal && XINT (elt) == ')')
		return val;
	      return Fsignal (Qinvalid_read_syntax, Fcons (make_string (". in wrong context", 18), Qnil));
	    }
	  return Fsignal (Qinvalid_read_syntax, Fcons (make_string ("] in a list", 11), Qnil));
	}
      tem = (read_pure && flag <= 0
	     ? pure_cons (elt, Qnil)
	     : Fcons (elt, Qnil));
      if (!NILP (tail))
	XCONS (tail)->cdr = tem;
      else
	val = tem;
      tail = tem;
      if (defunflag < 0)
	defunflag = EQ (elt, Qdefun);
      else if (defunflag > 0)
	read_pure = 1;
    }
}

Lisp_Object Vobarray;
Lisp_Object initial_obarray;

Lisp_Object
check_obarray (obarray)
     Lisp_Object obarray;
{
  while (!VECTORP (obarray) || XVECTOR (obarray)->size == 0)
    {
      /* If Vobarray is now invalid, force it to be valid.  */
      if (EQ (Vobarray, obarray)) Vobarray = initial_obarray;

      obarray = wrong_type_argument (Qvectorp, obarray);
    }
  return obarray;
}

static int hash_string ();
Lisp_Object oblookup ();

Lisp_Object
intern (str)
     char *str;
{
  Lisp_Object tem;
  int len = strlen (str);
  Lisp_Object obarray = Vobarray;
  if (!VECTORP (obarray) || XVECTOR (obarray)->size == 0)
    obarray = check_obarray (obarray);
  tem = oblookup (obarray, str, len);
  if (SYMBOLP (tem))
    return tem;
  return Fintern ((!NILP (Vpurify_flag)
		   ? make_pure_string (str, len)
		   : make_string (str, len)),
		  obarray);
}

DEFUN ("intern", Fintern, Sintern, 1, 2, 0,
  "Return the canonical symbol whose name is STRING.\n\
If there is none, one is created by this function and returned.\n\
A second optional argument specifies the obarray to use;\n\
it defaults to the value of `obarray'.")
  (str, obarray)
     Lisp_Object str, obarray;
{
  register Lisp_Object tem, sym, *ptr;

  if (NILP (obarray)) obarray = Vobarray;
  obarray = check_obarray (obarray);

  CHECK_STRING (str, 0);

  tem = oblookup (obarray, XSTRING (str)->data, XSTRING (str)->size);
  if (!FIXNUMP (tem))
    return tem;

  if (!NILP (Vpurify_flag))
    str = Fpurecopy (str);
  sym = Fmake_symbol (str);

  ptr = &XVECTOR (obarray)->contents[XINT (tem)];
  if (SYMBOLP (*ptr))
    XSYMBOL (sym)->next = XSYMBOL (*ptr);
  else
    XSYMBOL (sym)->next = 0;
  *ptr = sym;
  return sym;
}

DEFUN ("intern-soft", Fintern_soft, Sintern_soft, 1, 2, 0,
  "Return the canonical symbol whose name is STRING, or nil if none exists.\n\
A second optional argument specifies the obarray to use;\n\
it defaults to the value of `obarray'.")
  (str, obarray)
     Lisp_Object str, obarray;
{
  register Lisp_Object tem;

  if (NILP (obarray)) obarray = Vobarray;
  obarray = check_obarray (obarray);

  CHECK_STRING (str, 0);

  tem = oblookup (obarray, XSTRING (str)->data, XSTRING (str)->size);
  if (!FIXNUMP (tem))
    return tem;
  return Qnil;
}

Lisp_Object
oblookup (obarray, ptr, size)
     Lisp_Object obarray;
     register char *ptr;
     register int size;
{
  int hash, obsize;
  register Lisp_Object tail;
  Lisp_Object bucket, tem;

  if (!VECTORP (obarray) ||
      (obsize = XVECTOR (obarray)->size) == 0)
    {
      obarray = check_obarray (obarray);
      obsize = XVECTOR (obarray)->size;
    }
  /* Combining next two lines breaks VMS C 2.3.  */
  hash = hash_string (ptr, size);
  hash %= obsize;
  bucket = XVECTOR (obarray)->contents[hash];
  if (XFASTINT (bucket) == 0)
    ;
  else if (!SYMBOLP (bucket))
    error ("Bad data in guts of obarray"); /* Like CADR error message */
  else for (tail = bucket; ; XSET (tail, Lisp_Symbol, XSYMBOL (tail)->next))
      {
	if (XSYMBOL (tail)->name->size == size &&
	    !bcmp (XSYMBOL (tail)->name->data, ptr, size))
	  return tail;
	else if (XSYMBOL (tail)->next == 0)
	  break;
      }
  XSET (tem, Lisp_Int, hash);
  return tem;
}

static int
hash_string (ptr, len)
     unsigned char *ptr;
     int len;
{
  register unsigned char *p = ptr;
  register unsigned char *end = p + len;
  register unsigned char c;
  register int hash = 0;

  while (p != end)
    {
      c = *p++;
      if (c >= 0140) c -= 40;
      hash = ((hash<<3) + (hash>>28) + c);
    }
  return hash & 07777777777;
}

void
map_obarray (obarray, fn, arg)
     Lisp_Object obarray;
     int (*fn) ();
     Lisp_Object arg;
{
  register int i;
  register Lisp_Object tail;
  CHECK_VECTOR (obarray, 1);
  for (i = XVECTOR (obarray)->size - 1; i >= 0; i--)
    {
      tail = XVECTOR (obarray)->contents[i];
      if (XFASTINT (tail) != 0)
	while (1)
	  {
	    (*fn) (tail, arg);
	    if (XSYMBOL (tail)->next == 0)
	      break;
	    XSET (tail, Lisp_Symbol, XSYMBOL (tail)->next);
	  }
    }
}

mapatoms_1 (sym, function)
     Lisp_Object sym, function;
{
  call1 (function, sym);
}

DEFUN ("mapatoms", Fmapatoms, Smapatoms, 1, 2, 0,
  "Call FUNCTION on every symbol in OBARRAY.\n\
OBARRAY defaults to the value of `obarray'.")
  (function, obarray)
     Lisp_Object function, obarray;
{
  Lisp_Object tem;

  if (NILP (obarray)) obarray = Vobarray;
  obarray = check_obarray (obarray);

  map_obarray (obarray, mapatoms_1, function);
  return Qnil;
}

#define OBARRAY_SIZE 509

void
init_obarray ()
{
  Lisp_Object oblength;
  int hash;
  Lisp_Object *tem;

  XFASTINT (oblength) = OBARRAY_SIZE;

  Qnil = Fmake_symbol (make_pure_string ("nil", 3));
  /* Bootstrapping problem: Qnil isn't set when make_pure_string is
     called the first time. */
  XSYMBOL (Qnil)->name->dup_list = Qnil;
  Vobarray = Fmake_vector (oblength, make_number (0));
  initial_obarray = Vobarray;
  staticpro (&Vobarray);
  staticpro (&initial_obarray);
  /* Intern nil in the obarray */
  /* These locals are to kludge around a pyramid compiler bug. */
  hash = hash_string ((unsigned char *) "nil", 3);
  /* Separate statement here to avoid VAXC bug. */
  hash %= OBARRAY_SIZE;
  tem = &XVECTOR (Vobarray)->contents[hash];
  *tem = Qnil;

  Qunbound = Fmake_symbol (make_pure_string ("unbound", 7));
  XSYMBOL (Qnil)->function = Qunbound;
  XSYMBOL (Qunbound)->value = Qunbound;
  XSYMBOL (Qunbound)->function = Qunbound;

  Qt = intern ("t");
  XSYMBOL (Qnil)->value = Qnil;
  XSYMBOL (Qnil)->plist = Qnil;
  XSYMBOL (Qt)->value = Qt;

  /* Qt is correct even if CANNOT_DUMP.  loadup.el will set to nil at end.  */
  Vpurify_flag = Qt;

  Qvariable_documentation = intern ("variable-documentation");

  read_buffer_size = 100;
  read_buffer = (char *) xmalloc (read_buffer_size);
}

void
defsubr (sname)
     struct Lisp_Subr *sname;
{
  Lisp_Object sym;
  sym = intern (sname->symbol_name);
  XSET (XSYMBOL (sym)->function, Lisp_Subr, sname);
}

#ifdef NOTDEF /* use fset in subr.el now */
void
defalias (sname, string)
     struct Lisp_Subr *sname;
     char *string;
{
  Lisp_Object sym;
  sym = intern (string);
  XSET (XSYMBOL (sym)->function, Lisp_Subr, sname);
}
#endif /* NOTDEF */

/* New replacement for DefIntVar; it ignores the doc string argument
   on the assumption that make-docfile will handle that.  */
/* Define an "integer variable"; a symbol whose value is forwarded
 to a C variable of type int.  Sample call: */
  /* DEFVARINT ("indent-tabs-mode", &indent_tabs_mode, "Documentation");  */

void
defvar_int (namestring, address, doc)
     char *namestring;
     int *address;
     char *doc;
{
  Lisp_Object sym;
  sym = intern (namestring);
  XSET (XSYMBOL (sym)->value, Lisp_Intfwd, address);
}

/* Similar but define a variable whose value is T if address contains 1,
 NIL if address contains 0 */

void
defvar_bool (namestring, address, doc)
     char *namestring;
     int *address;
     char *doc;
{
  Lisp_Object sym;
  sym = intern (namestring);
  XSET (XSYMBOL (sym)->value, Lisp_Boolfwd, address);
}

/* Similar but define a variable whose value is the Lisp Object stored at address. */

void
defvar_lisp (namestring, address, doc)
     char *namestring;
     Lisp_Object *address;
     char *doc;
{
  Lisp_Object sym;
  sym = intern (namestring);
  XSET (XSYMBOL (sym)->value, Lisp_Objfwd, address);
  staticpro (address);
}

/* Similar but don't request gc-marking of the C variable.
   Used when that variable will be gc-marked for some other reason,
   since marking the same slot twice can cause trouble with strings.  */

void
defvar_lisp_nopro (namestring, address, doc)
     char *namestring;
     Lisp_Object *address;
     char *doc;
{
  Lisp_Object sym;
  sym = intern (namestring);
  XSET (XSYMBOL (sym)->value, Lisp_Objfwd, address);
}

#ifndef standalone

/* Similar but define a variable whose value is the Lisp Object stored in
 the current buffer.  address is the address of the slot in the buffer that is current now. */

void
defvar_per_buffer (namestring, address, doc)
     char *namestring;
     Lisp_Object *address;
     char *doc;
{
  Lisp_Object sym;
  int offset;
  extern struct buffer buffer_local_symbols;

  sym = intern (namestring);
  offset = (char *)address - (char *)current_buffer;

  XSET (XSYMBOL (sym)->value, Lisp_Buffer_Objfwd,
	(Lisp_Object *) offset);
  *(Lisp_Object *)(offset + (char *)&buffer_local_symbols) = sym;
  if (*(int *)(offset + (char *)&buffer_local_flags) == 0)
    /* Did a DEFVAR_PER_BUFFER without initializing the corresponding
       slot of buffer_local_flags */
    abort ();
}

#endif /* standalone */

init_read ()
{
#ifdef PATH_LOADSEARCH
  char *normal = PATH_LOADSEARCH;
  Lisp_Object normal_path;

  /* Warn if dirs in the *standard* path don't exist.  */
  normal_path = decode_env_path ("", normal);
  for (; !NILP (normal_path); normal_path = XCONS (normal_path)->cdr)
    {
      Lisp_Object dirfile;
      dirfile = Fcar (normal_path);
      if (!NILP (dirfile))
	{
	  dirfile = Fdirectory_file_name (dirfile);
	  if (access (XSTRING (dirfile)->data, 0) < 0)
	    printf ("Warning: lisp library (%s) does not exist.\n",
		    XSTRING (Fcar (normal_path))->data);
	}
    }
#else
  char *normal = 0;
#endif
  Vvalues = Qnil;

  if (egetenv ("EMACSLOADPATH") || normal)
    Vload_path = decode_env_path ("EMACSLOADPATH", normal);
  else
    Vload_path = Qnil; /* further frobbed by startup.el */

#ifndef CANNOT_DUMP
  if (!NILP (Vpurify_flag))
    /* loadup.el will frob this some more */
    Vload_path = Fcons (build_string ("../lisp/prim"), Vload_path);
#endif /* not CANNOT_DUMP */
  load_in_progress = 0;
}

void
syms_of_read ()
{
  defsubr (&Sread);
  defsubr (&Sread_from_string);
  defsubr (&Sintern);
  defsubr (&Sintern_soft);
  defsubr (&Sload);
  defsubr (&Slocate_file);
  defsubr (&Seval_buffer);
  defsubr (&Seval_region);
#ifdef standalone
  defsubr (&Sread_char);
#endif
  defsubr (&Sget_file_char);
  defsubr (&Smapatoms);

  DEFVAR_LISP ("obarray", &Vobarray,
    "Symbol table for use by `intern' and `read'.\n\
It is a vector whose length ought to be prime for best results.\n\
The vector's contents don't make sense if examined from Lisp programs;\n\
to find all the symbols in an obarray, use `mapatoms'.");

  DEFVAR_LISP ("values", &Vvalues,
    "List of values of all expressions which were read, evaluated and printed.\n\
Order is reverse chronological.");

  DEFVAR_LISP ("standard-input", &Vstandard_input,
    "Stream for read to get input from.\n\
See documentation of `read' for possible values.");
  Vstandard_input = Qt;

  DEFVAR_LISP ("load-path", &Vload_path,
    "*List of directories to search for files to load.\n\
Each element is a string (directory name) or nil (try default directory).\n\n\
Note that the elements of this list *may not* begin with \"~\", so you must\n\
call `expand-file-name' on them before adding them to this list.\n\n\
Initialized based on EMACSLOADPATH environment variable, if any,\n\
otherwise to default specified in by file `paths.h' when Emacs was built.\n\
If there were no paths specified in `paths.h', then emacs chooses a default\n\
value for this variable by looking around in the file-system near the\n\
directory in which the emacs executable resides.");

  DEFVAR_BOOL ("load-in-progress", &load_in_progress,
    "Non-nil iff inside of `load'.");

  DEFVAR_LISP ("after-load-alist", &Vafter_load_alist,
    "An alist of expressions to be evalled when particular files are loaded.\n\
Each element looks like (FILENAME FORMS...).\n\
When `load' is run and the file-name argument is FILENAME,\n\
the FORMS in the corresponding element are executed at the end of loading.\n\n\
FILENAME must match exactly!  Normally FILENAME is the name of a library,\n\
with no directory specified, since that is how `load' is normally called.\n\
An error in FORMS does not undo the load,\n\
but does prevent execution of the rest of the FORMS.");
  Vafter_load_alist = Qnil;

  Qstandard_input = intern ("standard-input");
  staticpro (&Qstandard_input);

  Qread_char = intern ("read-char");
  staticpro (&Qread_char);

  Qget_file_char = intern ("get-file-char");
  staticpro (&Qget_file_char);
}
