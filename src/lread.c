/* Lisp parsing and input streams.
   Copyright (C) 1985, 1986, 1987, 1988, 1989, 1992, 1993, 1994
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
#include "lisp.h"
#include "intl.h"

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <ctype.h>

#ifndef standalone
#include "buffer.h"
#include "paths.h"
#include "commands.h"
#include "bytecode.h"
#endif

#ifdef lint
#include <sys/inode.h>
#endif /* lint */

#ifndef X_OK
#define X_OK 01
#endif

#ifdef LISP_FLOAT_TYPE
#include <stdlib.h>
#include <math.h>
#endif /* LISP_FLOAT_TYPE */

Lisp_Object Qread_char, Qstandard_input;
Lisp_Object Qvariable_documentation;
Lisp_Object Qvariable_domain;	/* I18N3 */
Lisp_Object Vvalues, Vstandard_input, Vafter_load_alist;
Lisp_Object Qcurrent_load_list;
Lisp_Object Qload;

/* non-zero if inside `load' */
int load_in_progress;

/* Whether Fload() should check whether the .el is newer when loading .elc */
int load_warn_when_source_newer;
/* Whether Fload() should check whether the .elc doesn't exist */
int load_warn_when_source_only;

/* Search path for files to be loaded. */
Lisp_Object Vload_path;

/* This is the user-visible association list that maps features to
   lists of defs in their load files. */
Lisp_Object Vload_history;

/* This is useud to build the load history. */
Lisp_Object Vcurrent_load_list;


/* We may not be able to store STREAM itself as a Lisp_Object pointer
   since that is guaranteed to work only for data that has been malloc'd.
   So malloc a full-size pointer, and record the address of that pointer.  */

struct input_stream
  {
    struct lcrecord_header header;
    /* For reading from stdio FILE's */
    FILE *stdio_stream;
    /* For read-from-string */
    Lisp_Object string;
    int string_input_index;
    int string_input_limit;
  };

static Lisp_Object mark_input_stream (Lisp_Object, void (*) (Lisp_Object));
static void print_input_stream (Lisp_Object, Lisp_Object, int);
static void finalise_input_stream (void *header, int);
static int sizeof_istream (void *h) { return (sizeof (struct input_stream)); }
DEFINE_LRECORD_IMPLEMENTATION (lrecord_input_stream,
                               mark_input_stream, print_input_stream, 
                               finalise_input_stream, sizeof_istream, 0);
#define INPUTSTREAMP(s) RECORD_TYPEP ((s), lrecord_input_stream)
#define XINPUTSTREAM(s) ((struct input_stream *)XRECORD(s))

static Lisp_Object
mark_input_stream (Lisp_Object obj, void (*markobj) (Lisp_Object))
{
  return (XINPUTSTREAM (obj)->string);
}

static void
print_input_stream (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  char buf[30];
  sprintf (buf, GETTEXT ("#<input-stream 0x%x>"),
	   XINPUTSTREAM (obj)->header.uid); 
  write_string_1 (buf, -1, printcharfun);
}

static void
finalise_input_stream (void *header, int for_disksave)
{
  struct input_stream *s = (struct input_stream *) header;
  if (s->stdio_stream)
    fclose (s->stdio_stream);
  s->stdio_stream = 0;
}
  

/* Handle unreading and rereading of characters. */

static int
readchar (readcharfun)
     Lisp_Object readcharfun;
{
  Lisp_Object tem;
  register int c, mpos;

  if (BUFFERP (readcharfun))
    {
      struct buffer *inbuffer = XBUFFER (readcharfun);

      if (BUF_PT (inbuffer) >= BUF_ZV (inbuffer))
	return -1;
#ifdef I18N4
      c = *BUF_CHAR_ADDRESS (inbuffer, BUF_PT (inbuffer));
#else
      c = *(unsigned char *) BUF_CHAR_ADDRESS (inbuffer, BUF_PT (inbuffer));
#endif
      SET_BUF_PT (inbuffer, BUF_PT (inbuffer) + 1);

      return c;
    }
  if (MARKERP (readcharfun))
    {
      struct buffer *inbuffer = XMARKER (readcharfun)->buffer;

      mpos = marker_position (readcharfun);

      if (mpos > BUF_ZV (inbuffer) - 1)
	return -1;
#ifdef I18N4
      c = *BUF_CHAR_ADDRESS (inbuffer, mpos);
#else
      c = *(unsigned char *) BUF_CHAR_ADDRESS (inbuffer, mpos);
#endif
      if (mpos != BUF_GPT (inbuffer))
	XMARKER (readcharfun)->bufpos++;
      else
	Fset_marker (readcharfun, make_number (mpos + 1),
		     Fmarker_buffer (readcharfun));
      return c;
    }
  if (INPUTSTREAMP (readcharfun))
    {
      struct input_stream *s = XINPUTSTREAM (readcharfun);

      if (s->stdio_stream)
	return (getc (s->stdio_stream));

      if (!NILP (s->string))
	{
	  if (s->string_input_index < s->string_input_limit)
	    return (XSTRING (s->string)->data[s->string_input_index++]);
	  else
	    return (-1);
	}
      return (-1);
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
      struct buffer *b = XBUFFER (readcharfun);
      if (b == current_buffer)
	SET_PT (PT - 1);
      else
	SET_BUF_PT (b, BUF_PT (b) - 1);
    }
  else if (MARKERP (readcharfun))
    XMARKER (readcharfun)->bufpos--;
  else if (INPUTSTREAMP (readcharfun))
    {
      struct input_stream *s = XINPUTSTREAM (readcharfun);

      if (s->stdio_stream)
	ungetc (c, s->stdio_stream);
      else if (!NILP (s->string))
	{
	  if (s->string_input_index > 0)
	    s->string_input_index--;
	}
    }
  else
    call1 (readcharfun, make_number (c));
}

static Lisp_Object read0 (Lisp_Object readcharfun);
static Lisp_Object read1 (Lisp_Object readcharfun);
/* flag = 1 means check for ] to terminate rather than ) and .
   flag = -1 means check for starting with defun
    and make structure pure.  */
static Lisp_Object read_list (Lisp_Object readcharfun,
                              int terminator,
                              int allow_dotted_lists);

/* get a character from the tty */

#ifdef standalone     /* This is normally defined in event-stream.c */

#define kludge DEFUN /* to keep this away from make-docfile... */
kludge ("read-char", Fread_char, Sread_char, 0, 0, 0, "") ()
{
  return getchar ();
}
#undef kludge
#endif /* standalone */



static void readevalloop (Lisp_Object readcharfun, 
                          Lisp_Object sourcefile,
                          Lisp_Object (*evalfun) (Lisp_Object),
                          int printflag);

static Lisp_Object
load_unwind (stream)  /* used as unwind-protect function in load */
     Lisp_Object stream;
{
  struct input_stream *s = XINPUTSTREAM (stream);
  if (s->stdio_stream)
    fclose (s->stdio_stream);
  s->stdio_stream = 0;
  if (--load_in_progress < 0)
    load_in_progress = 0;
  return Qnil;
}

#ifdef I18N3
Lisp_Object Vfile_domain;

Lisp_Object
restore_file_domain (Lisp_Object val)
{
  Vfile_domain = val;
  return Qnil;
}
#endif /* I18N3 */


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
  int speccount = specpdl_depth ();
  Lisp_Object newer = Qnil;
  int source_only = 0;
  Lisp_Object handler;
  struct gcpro gcpro1, gcpro2;
  GCPRO2 (str, newer);

  CHECK_STRING (str, 0);
  str = Fsubstitute_in_file_name (str);

  /* If file name is magic, call the handler.  */
  handler = Ffind_file_name_handler (str);
  if (!NILP (handler))
    {
      UNGCPRO;
      return call5 (handler, Qload, str, missing_ok, nomessage, nosuffix);
    }

  /* Avoid weird lossage with null string as arg,
     since it would try to load a directory as a Lisp file.
     Unix truly sucks */
  if (XSTRING (str)->size > 0)
    {
      Lisp_Object found;

      fd = locate_file (Vload_path, str, 
                        ((!NILP (nosuffix)) ? "" : ".elc:.el:"),
                        &found,
                        -1);

      if (fd < 0)
	{
	  if (NILP (missing_ok))
	    signal_error (Qfile_error,
			  list2 (build_string
				 (GETTEXT ("Cannot open load file")), str));
	  else
	    {
	      UNGCPRO;
	      return Qnil;
	    }
	}
      else if (load_warn_when_source_newer &&
	       !memcmp (".elc",
			(char *) &(XSTRING (found)->data
				   [XSTRING (found)->size - 4]),
			4))
	{
	  struct stat s1, s2;
	  if (! fstat (fd, &s1))	/* can't fail, right? */
	    {
	      int result;
	      /* temporarily hack the 'c' off the end of the filename */
	      XSTRING (found)->data[XSTRING (found)->size - 1] = 0;
	      result = stat ((char *) XSTRING (found)->data, &s2);
	      if (result >= 0 &&
		  (unsigned) s1.st_mtime < (unsigned) s2.st_mtime)
              {
                struct gcpro gcpro1;
                GCPRO1 (found);
		newer = Ffile_name_nondirectory (found);
                UNGCPRO;
              }
	      /* put the 'c' back on (kludge-o-rama) */
	      XSTRING (found)->data[XSTRING (found)->size - 1] = 'c';
	    }
	}
      else if (load_warn_when_source_only &&
	       /* `found' ends in ".el" */
	       !memcmp (".el",
			(char *) &(XSTRING (found)->data
				   [XSTRING (found)->size - 3]),
			3) &&
	       /* `str' does not end in ".el" */
	       memcmp (".el",
		       (char *) &(XSTRING (str)->data
				  [XSTRING (str)->size - 3]),
		       3))
	{
	  source_only = 1;
	}
    }

  stream = fdopen (fd, "r");
  if (stream == 0)
    {
      emacs_close (fd);
      error (GETTEXT ("Failure to create stdio stream for %s"),
	     XSTRING (str)->data);
    }

  if (!NILP (newer))
    {
      message (GETTEXT ("Loading %s...  (file %s is newer)"),
	       XSTRING (str)->data, XSTRING (newer)->data);
      nomessage = Qnil; /* we printed the first one, so print "done" too */
    }
  else if (source_only)
    {
      message (GETTEXT ("Loading %s...  (file %s.elc does not exist)"),
	       XSTRING (str)->data,
	       XSTRING (Ffile_name_nondirectory (str))->data);
      nomessage = Qnil;
    }
  else if (NILP (nomessage))
    message (GETTEXT ("Loading %s..."), XSTRING (str)->data);

  {
    /* Lisp_Object's must be malloc'ed, not stack-allocated */
    Lisp_Object lispstream;
    struct input_stream *s = alloc_lcrecord (sizeof (struct input_stream), 
                                             lrecord_input_stream);
    s->stdio_stream = stream;
    s->string = Qnil;
    XSET (lispstream, Lisp_Record, s);

    record_unwind_protect (load_unwind, lispstream);
    load_in_progress++;

#ifdef I18N3
    record_unwind_protect (restore_file_domain, Vfile_domain);
    Vfile_domain = Qnil; /* set it to nil; a call to #'domain will set it. */
#endif

    readevalloop (lispstream, str, Feval, 0);
    unbind_to (speccount, Qnil);
  }

  {
    Lisp_Object tem;
    /* >>> Disgusting kludge */
    /* Run any load-hooks for this file.  */
    tem = Fassoc (str, Vafter_load_alist);
    if (!NILP (tem))
      Fprogn (Fcdr (tem));
  }

  if (noninteractive || !NILP (nomessage))
    ;
  else if (!NILP (newer))
    message (GETTEXT ("Loading %s...done  (file %s is newer)"),
	     XSTRING (str)->data, XSTRING (newer)->data);
  else
    message (GETTEXT ("Loading %s...done"), XSTRING (str)->data);

  UNGCPRO;
  return Qt;
}


DEFUN ("locate-file", Flocate_file, Slocate_file, 2, 4, 0,
  "Search for FILENAME through PATH-LIST, expanded by one of the optional\n\
SUFFIXES (string of suffixes separated by \":\"s), checking for access\n\
MODE (0|1|2|4 = exists|executable|writeable|readable), default readable.")
  (file, path, suff, mode)
     Lisp_Object file, path, suff, mode;
{
  Lisp_Object tp;

  CHECK_STRING (file, 0);
  if (!NILP (suff))
    {
      CHECK_STRING (suff, 0);
    }
  if (!(NILP (mode) || (FIXNUMP (mode) && XINT (mode) >= 0)))
    mode = wrong_type_argument (Qnatnump, mode);
  locate_file (path, file, 
               ((NILP (suff)) ? "" : (char *) (XSTRING (suff)->data)),
	       &tp, (NILP (mode) ? R_OK : XINT (mode)));
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
     CONST char *suffix;
     Lisp_Object *storeptr;
     int mode;
{
  register int fd;
  int fn_size = 100;
  char buf[100];
  register char *fn = buf;
  int absolute;
  int want_size;
  struct stat st;
  Lisp_Object filename = Qnil;
  struct gcpro gcpro1, gcpro2, gcpro3;

  GCPRO3 (path, str, filename);

  if (storeptr)
    *storeptr = Qnil;

  absolute = !NILP (Ffile_name_absolute_p (str));

  for (; !NILP (path); path = Fcdr (path))
    {
      CONST char *nsuffix;

      filename = Fexpand_file_name (str, Fcar (path));
      if (NILP (Ffile_name_absolute_p (filename)))
	/* If there are non-absolute elts in PATH (eg ".") */
	/* Of course, this could conceivably lose if luser sets
	   default-directory to be something non-absolute... */
	{
	  filename = Fexpand_file_name (filename, current_buffer->directory);
	  if (NILP (Ffile_name_absolute_p (filename)))
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
	  char *esuffix = (char *) strchr (nsuffix, ':');
	  int lsuffix = ((esuffix) ? (esuffix - nsuffix) : strlen (nsuffix));

	  /* Concatenate path element/specified name with the suffix.  */
	  strncpy (fn, (char *) XSTRING (filename)->data, 
                   XSTRING (filename)->size);
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
		fd = emacs_open (fn, 0, 0);

	      if (fd >= 0)
		{
		  /* We succeeded; return this descriptor and filename.  */
		  if (storeptr)
		    *storeptr = build_string (fn);
                  UNGCPRO;
		  return fd;
		}
	    }

	  /* Advance to next suffix.  */
	  if (esuffix == 0)
	    break;
	  nsuffix += lsuffix + 1;
	}
      if (absolute) 
      {
        UNGCPRO;
        return -1;
      }
    }

  UNGCPRO;
  return -1;
}

#ifdef LOADHIST

/* Merge the list we've accumulated of globals from the current input source
   into the load_history variable.  The details depend on whether
   the source has an associated file name or not. */

static void
build_load_history (int loading, Lisp_Object source)
{
  register Lisp_Object tail, prev, newelt;
  register Lisp_Object tem, tem2;
  register int foundit;

  /* Don't bother recording anything for preloaded files.  */
  if (purify_flag)
    return;

  tail = Vload_history;
  prev = Qnil;
  foundit = 0;
  while (!NILP (tail))
    {
      tem = Fcar (tail);

      /* Find the feature's previous assoc list... */
      if (!NILP (Fequal (source, Fcar (tem))))
	{
	  foundit = 1;

	  /*  If we're loading, remove it. */
	  if (loading)
	    {	  
	      if (NILP (prev))
		Vload_history = Fcdr (tail);
	      else
		Fsetcdr (prev, Fcdr (tail));
	    }

	  /*  Otherwise, cons on new symbols that are not already members.  */
	  else
	    {
	      tem2 = Vcurrent_load_list;

	      while (CONSP (tem2))
		{
		  newelt = Fcar (tem2);

		  if (NILP (Fmemq (newelt, tem)))
		    Fsetcar (tail, Fcons (Fcar (tem),
					  Fcons (newelt, Fcdr (tem))));

		  tem2 = Fcdr (tem2);
		  QUIT;
		}
	    }
	}
      else
	prev = tail;
      tail = Fcdr (tail);
      QUIT;
    }

  /* If we're loading, cons the new assoc onto the front of load-history,
     the most-recently-loaded position.  Also do this if we didn't find
     an existing member for the current source.  */
  if (loading || !foundit)
    Vload_history = Fcons (Fnreverse (Vcurrent_load_list),
			   Vload_history);
}

#else /* !LOADHIST */
#define build_load_history(x,y)
#endif /* !LOADHIST */


static void
readevalloop (Lisp_Object readcharfun, 
              Lisp_Object sourcename,
              Lisp_Object (*evalfun) (Lisp_Object),
              int printflag)
{
  register int c;
  register Lisp_Object val;
  int speccount = specpdl_depth ();
  struct gcpro gcpro1;

  specbind (Qstandard_input, readcharfun);
  specbind (Qcurrent_load_list, Qnil);

  GCPRO1 (sourcename);

  LOADHIST_ATTACH (sourcename);

  while (1)
    {
      QUIT;
      c = readchar (readcharfun);
      if (c == ';')
	{
          /* Skip comment */
	  while ((c = readchar (readcharfun)) != '\n' && c != -1)
            QUIT;
	  continue;
	}
      if (c < 0)
        break;
      if (c == ' ' || c == '\t' || c == '\n' || c == '\f')
        continue;

#if 0 /* defun hack */
      if (purify_flag && c == '(')
	{
	  val = read_list (readcharfun, ')', 1, read_pure, 0, 1);
	}
      else
	{
	  unreadchar (readcharfun, c);
	  val = read0 (readcharfun);
	}
#else /* No "defun hack" -- Emacs 19 uses read-time syntax for bytecodes */
      unreadchar (readcharfun, c);
      val = read0 (readcharfun);
#endif
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

  build_load_history (1,        /* >>> This isn't right */
                      sourcename);
  UNGCPRO;

  unbind_to (speccount, Qnil);
}

#ifndef standalone

DEFUN ("eval-buffer", Feval_buffer, Seval_buffer, 1, 2, "bBuffer: ",
  "Execute BUFFER as Lisp code.\n\
Programs can pass argument PRINTFLAG which controls printing of output:\n\
nil means discard it; anything else is stream for print.")
  (bufname, printflag)
     Lisp_Object bufname, printflag;
{
  int speccount = specpdl_depth ();
  Lisp_Object tem, buf;

  buf = Fget_buffer (bufname);
  if (NILP (buf))
    error (GETTEXT ("No such buffer."));

  if (NILP (printflag))
    tem = Qsymbolp;             /* >>>> #@[]*&$#*[& SI:NULL-STREAM */
  else
    tem = printflag;
  specbind (Qstandard_output, tem);
  record_unwind_protect (save_excursion_restore, save_excursion_save ());
  SET_BUF_PT (XBUFFER (buf), BUF_BEGV (XBUFFER (buf)));
  readevalloop (buf, XBUFFER (buf)->filename, Feval, !NILP (printflag));

  return unbind_to (speccount, Qnil);
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
  int speccount = specpdl_depth ();
  Lisp_Object tem;
  Lisp_Object cbuf = Fcurrent_buffer ();

  if (NILP (printflag))
    tem = Qsymbolp;             /* >>>> #@[]*&$#*[& SI:NULL-STREAM */
  else
    tem = printflag;
  specbind (Qstandard_output, tem);
  if (NILP (printflag))
    record_unwind_protect (save_excursion_restore, save_excursion_save ());
  record_unwind_protect (save_restriction_restore, save_restriction_save ());
  /* This both uses b and checks its type.  */
  Fgoto_char (b);
  Fnarrow_to_region (make_number (BEGV), e);
  readevalloop (cbuf, XBUFFER (cbuf)->filename, Feval, !NILP (printflag));
  return unbind_to (speccount, Qnil);
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
  if (NILP (readcharfun))
    readcharfun = Vstandard_input;
  if (EQ (readcharfun, Qt))
    readcharfun = Qread_char;

#ifndef standalone
  if (EQ (readcharfun, Qread_char))
  {
    Lisp_Object val = call1 (Qread_from_minibuffer, 
                             build_string (GETTEXT ("Lisp expression: ")));
    return (Fcar (Fread_from_string (val, Qnil, Qnil)));
  }
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
    {
      CHECK_FIXNUM (end,2);
      endval = XINT (end);
      if (endval < 0 || endval > XSTRING (string)->size)
	args_out_of_range (string, end);
    }

  if (NILP (start))
    startval = 0;
  else
    {
      CHECK_FIXNUM (start,1);
      startval = XINT (start);
      if (startval < 0 || startval > endval)
	args_out_of_range (string, start);
    }

  {
    /* Lisp_Object's must be malloc'ed, not stack-allocated */
    Lisp_Object lispstream;
    struct input_stream *s = alloc_lcrecord (sizeof (struct input_stream), 
                                             lrecord_input_stream);
    s->stdio_stream = 0;
    s->string = string;
    s->string_input_index = startval;
    s->string_input_limit = endval;

    XSET (lispstream, Lisp_Record, s);

    tem = read0 (lispstream);
    return Fcons (tem, make_number (s->string_input_index));
  }
}

/* Use this for recursive reads, in contexts where internal tokens are not allowed. */

static Lisp_Object
read0 (Lisp_Object readcharfun)
{
  register Lisp_Object val;

  val = read1 (readcharfun);
  if (CONSP (val) && EQ (XCONS (val)->car, Qunbound))
    {
      char c = XINT (XCONS (val)->cdr);
      free_cons (XCONS (val));
      return Fsignal (Qinvalid_read_syntax, list1 (make_string (&c, 1)));
    }

  return val;
}

static int read_buffer_size;
static char *read_buffer;

static int
read_escape (readcharfun)
     Lisp_Object readcharfun;
{
  register int c = readchar (readcharfun);
  switch (c)
    {
    case 'a':
      return '\007'; 	/* some systems don't know '\a' */
    case 'b':
      return '\b';
    case 'd':
      return 0177;
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
      c = readchar (readcharfun);
      if (c != '-')
	error (GETTEXT ("Invalid escape character syntax"));
      c = readchar (readcharfun);
      if (c == '\\')
	c = read_escape (readcharfun);
      return c | 0200;

    case 'C':
      c = readchar (readcharfun);
      if (c != '-')
	error (GETTEXT ("Invalid escape character syntax"));
    case '^':
      c = readchar (readcharfun);
      if (c == '\\')
	c = read_escape (readcharfun);
      if (c == '?')
	return 0177;
      else
        return (c & (0200 | 037));
      
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
	    if ((c = readchar (readcharfun)) >= '0' && c <= '7')
	      {
		i *= 8;
		i += c - '0';
	      }
	    else
	      {
		unreadchar (readcharfun, c);
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
	    c = readchar (readcharfun);
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
		unreadchar (readcharfun, c);
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
read_atom (register Lisp_Object readcharfun,
           int firstchar,
           int uninterned_symbol)
{
  register char *p = read_buffer;
  register char *end = read_buffer + read_buffer_size;
  int saw_a_backslash = 0;      /* if there were backslashes in token */
  register int c = firstchar;

  while (c > 040 
         && !(c == '\"' || c == '\'' || c == ';'
              || c == '(' || c == ')'
              || c == '[' || c == ']' || c == '#'
              ))
    {
      if (p == end)
	{
	  register char *new
	    = (char *) xrealloc (read_buffer, read_buffer_size *= 2);
	  p += new - read_buffer;
	  read_buffer += new - read_buffer;
	  end = read_buffer + read_buffer_size;
          QUIT;
	}
      if (c == '\\')
	{
	  c = readchar (readcharfun);
	  saw_a_backslash = 1;
	}
      *p++ = c;
      c = readchar (readcharfun);
    }

  if (p == end)
    {
      char *new = (char *) xrealloc (read_buffer, read_buffer_size *= 2);
      p += new - read_buffer;
      read_buffer += new - read_buffer;
      /* end = read_buffer + read_buffer_size; */
    }
  *p = 0;
  if (c >= 0)
    unreadchar (readcharfun, c);

  /* Is it an integer? */
  if (! (saw_a_backslash || uninterned_symbol))
    {
      /* If a token had any backslashes in it, it is disqualified from
	 being an integer or a float.  This means that 123\456 is a
	 symbol, as is \123 (which is the way (intern "123") prints.)
	 Also, if token was preceeded by #:, it's always a symbol.
       */
      register char *p1;
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
		  while (p1 != p) 
		    {
		      if (*p < '0' || *p > '7')
			return Fsignal (Qinvalid_read_syntax,
					list1 (build_string (GETTEXT ("non-octal digit"))));
		      number = (number << 3) + *p++ - '0';
		    }
		}
	      else
#endif
		number = atoi (read_buffer);

	      return (make_number (number));
	    }
	}
#ifdef LISP_FLOAT_TYPE
      if (isfloat_string (read_buffer))
	return make_float (atof (read_buffer));
#endif
    }

  {
    int len = p - read_buffer;

    if (uninterned_symbol)
      return (Fmake_symbol ((purify_flag) 
                            ? make_pure_pname (read_buffer, len, 0)
                            : make_string (read_buffer, len)));
    else
      /* intern will purecopy pname if necessary */
      return (Fintern (make_string (read_buffer, len), Qnil));
  }
}


static Lisp_Object read_bytecode (Lisp_Object readcharfun, int terminator);
static Lisp_Object read_vector (Lisp_Object readcharfun, int terminator);

static int
reader_nextchar (Lisp_Object readcharfun)
{
  int c;

 retry:
  QUIT;
  c = readchar (readcharfun);
  if (c < 0)
    signal_error (Qend_of_file, list1 (readcharfun));

  switch (c)
    {
    default:
      {
	/* Ignore whitespace and control characters */
	if (c <= 040)
	  goto retry;
	return (c);
      }

    case ';':
      {
        /* Comment */
        while ((c = readchar (readcharfun)) >= 0 && c != '\n')
          QUIT;
        goto retry;
      }
    }
}

#if 0
static Lisp_Object
list2_pure (int pure, Lisp_Object a, Lisp_Object b)
{
  if (pure)
    return (pure_cons (a, pure_cons (b, Qnil)));
  else
    return (list2 (a, b));
}
#endif
  
static Lisp_Object
read1 (Lisp_Object readcharfun)
{
  int c;

 retry:
  c = reader_nextchar (readcharfun);

  switch (c)
    {
    case '(':
      return read_list (readcharfun, ')', 1);

    case '[':
      return (read_vector (readcharfun, ']'));

    case ')':
    case ']':
      {
        return (Fcons (Qunbound, make_number (c)));
      }

    case '.':
      {
#ifdef LISP_FLOAT_TYPE
	/* If a period is followed by a number, then we should read it
	   as a floating point number.  Otherwise, it denotes a dotted
	   pair.
	 */
	c = readchar (readcharfun);
	unreadchar (readcharfun, c);

	if (! isdigit (c))
	  return (Fcons (Qunbound, make_number ('.')));

	/* Note that read_atom will loop
	   at least once, assuring that we will not try to UNREAD
           two characters in a row.  */
        return (read_atom (readcharfun, '.', 0));

#else  /* ! LISP_FLOAT_TYPE */
	return (Fcons (Qunbound, make_number ('.')));
#endif /* ! LISP_FLOAT_TYPE */
      }

    case '#':
      {
	c = readchar (readcharfun);
	switch (c)
        {
        case '[':
          {
            /* "#["-- byte-code constant syntax */
            return (read_bytecode (readcharfun, ']'
                                   /* purecons #[...] syntax */
                                   /*, purify_flag */));
          }
	case ':':
          {
            /* "#:"-- quasi-implemented gensym syntax */
            return (read_atom (readcharfun, readchar (readcharfun), 1));
          }
        case '\'':
          {
            /* #'x => (function x) */
            return (list2 (Qfunction, read0 (readcharfun)));
          }
        case '(':
          {
            /* "#(" -- Scheme/CL vector syntax */
            return (read_vector (readcharfun, ')'));
          }

        default:
          {
            unreadchar (readcharfun, c);
            return Fsignal (Qinvalid_read_syntax, 
                            list1 (make_string ("#", 1)));
          }
        }
      }

    case '\'':
      {
        /* Quote */
        return list2 (Qquote, read0 (readcharfun));
      }

    case '?':
      {
        /* Evil GNU Emacs "character" (ie integer) syntax */
	c = readchar (readcharfun);
	if (c < 0) return Fsignal (Qend_of_file, list1 (readcharfun));

	if (c == '\\')
	  return (make_number (read_escape (readcharfun)));
	else
          return (make_number (c));
      }

    case '\"':
      {
        /* String */
	register char *p = read_buffer;
	register char *end = read_buffer + read_buffer_size;
	int cancel = 0;

	while ((c = readchar (readcharfun)) >= 0
	       && c != '\"')
	  {
	    if (p == end)
	      {
		char *new = (char *) xrealloc (read_buffer, read_buffer_size *= 2);
		p = new + (p - read_buffer);
		read_buffer = new;
		end = read_buffer + read_buffer_size;
                QUIT;
	      }
	    if (c == '\\')
	      c = read_escape (readcharfun);
	    /* c is -1 if \ newline has just been seen */
	    if (c == -1)
	      {
		if (p == read_buffer)
		  cancel = 1;
	      }
	    else
	      *p++ = c;
	  }
	if (c < 0) return Fsignal (Qend_of_file, list1 (readcharfun));

	/* If purifying, and string starts with \ newline,
	   return zero instead.  This is for doc strings
	   that we are really going to find in etc/DOC.nn.nn  */
	if (purify_flag && NILP (Vdoc_file_name) && cancel)
	  return (Qzero);

        return make_string (read_buffer, p - read_buffer);
      }

    default:
      {
	/* Ignore whitespace and control characters */
	if (c <= 040)
	  goto retry;
	return (read_atom (readcharfun, c, 0));
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
isfloat_string (register CONST char *cp)
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
	      || state == (DOT_CHAR|TRAIL_INT)
	      || state == (LEAD_INT|E_CHAR|EXP_INT)
	      || state == (LEAD_INT|DOT_CHAR|TRAIL_INT|E_CHAR|EXP_INT)
	      || state == (DOT_CHAR|TRAIL_INT|E_CHAR|EXP_INT)));
}
#endif /* LISP_FLOAT_TYPE */

static void *
sequence_reader (Lisp_Object readcharfun,
                 int terminator,
                 void *state,
                 void * (*conser) (Lisp_Object readcharfun,
                                   void *state, int len))
{
  int len;

  for (len = 0; ; len++)
  {
    int ch;

    QUIT;
    ch = reader_nextchar (readcharfun);

    if (ch == terminator)
      return (state);
    else
      unreadchar (readcharfun, ch);
    if (ch == ']')
      signal_error (Qinvalid_read_syntax,
                    list1 (build_string (GETTEXT ("\"]\" in a list"))));
    else if (ch == ')')
      signal_error (Qinvalid_read_syntax,
                    list1 (build_string (GETTEXT ("\")\" in a vector"))));
    state = ((conser) (readcharfun, state, len));
  }
}


struct read_list_state 
  {
    Lisp_Object head; Lisp_Object tail;
    int allow_dotted_lists; 
    int terminator;
  };

static void *
read_list_conser (Lisp_Object readcharfun, void *state, int len)
{
  struct read_list_state *s = state;
  Lisp_Object elt;

  elt = read1 (readcharfun);
  if (CONSP (elt) && EQ (XCONS (elt)->car, Qunbound))
  {
    Lisp_Object tem = elt;
    int ch;

    elt = XCONS (elt)->cdr;
    free_cons (XCONS (tem));
    tem = Qnil;
    ch = XINT (elt);
    if (ch != '.')
      signal_error (Qerror,
                    list2 (build_string
			   (GETTEXT ("BUG! Internal reader error %o")), elt));
    else if (!s->allow_dotted_lists)
      signal_error (Qinvalid_read_syntax,
                    list1 (build_string (GETTEXT ("\".\" in a vector"))));
    else
    {
      if (!NILP (s->tail))
        XCONS (s->tail)->cdr = read0 (readcharfun);
      else
        s->head = read0 (readcharfun);
      elt = read1 (readcharfun);
      if (CONSP (elt) && EQ (XCONS (elt)->car, Qunbound)
          && XINT (XCONS (elt)->cdr) == s->terminator)
      {
        free_cons (XCONS (elt));
        unreadchar (readcharfun, s->terminator);
        goto done;
      }
      signal_error (Qinvalid_read_syntax,
                    list1 (build_string (GETTEXT (". in wrong context"))));
    }
  }

#if 0
  if (NILP (tail) && defun_hack && EQ (elt, Qdefun) && !read_pure)
  {
    record_unwind_protect (unreadpure, Qzero);
    read_pure = 1;
  }
#endif
  elt = Fcons (elt, Qnil);
  if (!NILP (s->tail))
    XCONS (s->tail)->cdr = elt;
  else
    s->head = elt;
  s->tail = elt;
 done:
  return (s);
}


static Lisp_Object
read_list (Lisp_Object readcharfun,
           int terminator,
           int allow_dotted_lists)
{
  struct read_list_state s;
  struct gcpro gcpro1, gcpro2;

  s.head = Qnil;
  s.tail = Qnil;
  s.allow_dotted_lists = allow_dotted_lists;
  s.terminator = terminator;
  GCPRO2 (s.head, s.tail);

  (void) sequence_reader (readcharfun,
                          terminator,
                          &s,
                          read_list_conser);
  UNGCPRO;
  return (s.head);
}

static Lisp_Object
read_vector (Lisp_Object readcharfun,
             int terminator)
{
  Lisp_Object tem;
  Lisp_Object *p;
  int len;
  int i;
  struct read_list_state s;
  struct gcpro gcpro1, gcpro2;


  s.head = Qnil;
  s.tail = Qnil;
  s.allow_dotted_lists = 0;
  GCPRO2 (s.head, s.tail);
  
  (void) sequence_reader (readcharfun,
                          terminator,
                          &s,
                          read_list_conser);
  UNGCPRO;
  tem = s.head;
  len = XINT (Flength (tem));

  s.head = make_vector (len, Qnil);

  for (i = 0, p = &(XVECTOR (s.head)->contents[0]);
       i < len;
       i++, p++)
  {
    struct Lisp_Cons *otem = XCONS (tem);
    tem = Fcar (tem);
    *p = tem;
    tem = otem->cdr;
    free_cons (otem);
  }
  return (s.head);
}

static Lisp_Object
read_bytecode (Lisp_Object readcharfun, int terminator)
{
  /* Accept compiled functions at read-time so that we don't 
     have to build them at load-time. */
  Lisp_Object stuff;
  Lisp_Object make_byte_code_args[COMPILED_DOMAIN + 1];
  struct gcpro gcpro1;
  int len;
  int iii;

  stuff = read_list (readcharfun, terminator, 0);
  len = XINT (Flength (stuff));
  if (len < COMPILED_STACK_DEPTH + 1 || len > COMPILED_DOMAIN + 1)
    return Fsignal (Qinvalid_read_syntax,
                    list1 (build_string (GETTEXT ("#[...] used with wrong number of elements"))));

  for (iii = 0; CONSP (stuff); iii++)
  {
    struct Lisp_Cons *victim = XCONS (stuff);
    make_byte_code_args[iii] = Fcar (stuff);
    stuff = Fcdr (stuff);
    free_cons (victim);
  }
  GCPRO1 (make_byte_code_args[0]);
  gcpro1.nvars = len;

  /* make-byte-code looks at purify_flag, which should have the same
   *  value as our "read-pure" argument */
  RETURN_UNGCPRO (Fmake_byte_code (len, make_byte_code_args));
}



void
init_lread ()
{
#ifdef PATH_LOADSEARCH
  char *normal = PATH_LOADSEARCH;

  /* Warn if dirs in the *standard* path don't exist.  */
  {
    Lisp_Object normal_path = decode_env_path (0, normal);
    for (; !NILP (normal_path); normal_path = XCONS (normal_path)->cdr)
      {
        Lisp_Object dirfile;
        dirfile = Fcar (normal_path);
        if (!NILP (dirfile))
          {
	    dirfile = Fdirectory_file_name (dirfile);
            if (access (XSTRING (dirfile)->data, 0) < 0)
              printf (GETTEXT ("Warning: lisp library (%s) does not exist.\n"),
                      XSTRING (Fcar (normal_path))->data);
          }
      }
  }
#else
  char *normal = 0;
#endif
  Vvalues = Qnil;

  /* further frobbed by startup.el if nil. */
  Vload_path = decode_env_path ("EMACSLOADPATH", normal);

#ifndef CANNOT_DUMP
  if (purify_flag)
    /* loadup.el will frob this some more */
    /* >>> unix-specific */
    Vload_path = Fcons (build_string ("../lisp/prim"), Vload_path);
#endif /* not CANNOT_DUMP */
  load_in_progress = 0;
}

void
syms_of_lread ()
{
  defsubr (&Sread);
  defsubr (&Sread_from_string);
  defsubr (&Sload);
  defsubr (&Slocate_file);
  defsubr (&Seval_buffer);
  defsubr (&Seval_region);
#ifdef standalone
  defsubr (&Sread_char);
#endif
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

  DEFVAR_BOOL ("load-warn-when-source-newer", &load_warn_when_source_newer,
  "*Whether `load' should check whether the source is newer than the binary;\n\
If this variable is true, then when a `.elc' file is being loaded and the\n\
corresponding `.el' is newer, a warning message will be printed.");
  load_warn_when_source_newer = 0;

  DEFVAR_BOOL ("load-warn-when-source-only", &load_warn_when_source_only,
  "*Whether `load' should warn when loading a .el file instead of an .elc.\n\
If this variable is true, then when load is called with a filename without\n\
an extension, and the .elc version doesn't exist but the .el version does,\n\
then a message will be printed.  If an explicit extension is passed to load,\n\
no warning will be printed.");
  load_warn_when_source_only = 0;

#ifdef LOADHIST
  DEFVAR_LISP ("load-history", &Vload_history,
    "Alist mapping source file names to symbols and features.\n\
Each alist element is a list that starts with a file name,\n\
except for one element (optional) that starts with nil and describes\n\
definitions evaluated from buffers not visiting files.\n\
The remaining elements of each list are symbols defined as functions\n\
or variables, and cons cells `(provide . FEATURE)' and `(require . FEATURE)'.");
  Vload_history = Qnil;

  DEFVAR_LISP ("current-load-list", &Vcurrent_load_list,
    "Used for internal purposes by `load'.");
  Vcurrent_load_list = Qnil;
#endif

  defsymbol (&Qstandard_input, "standard-input");
  defsymbol (&Qread_char, "read-char");
  defsymbol (&Qcurrent_load_list, "current-load-list");
  defsymbol (&Qload, "load");

  read_buffer_size = 100;
  read_buffer = (char *) xmalloc (read_buffer_size);

#ifdef I18N3
  Vfile_domain = Qnil;
#endif
}
