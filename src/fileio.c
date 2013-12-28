/* File IO for GNU Emacs.
   Copyright (C) 1985, 1986, 1987, 1988, 1992, 1993, 1994
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

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

#if !defined (S_ISLNK) && defined (S_IFLNK)
#  define S_ISLNK(m) (((m) & S_IFMT) == S_IFLNK)
#endif

#if !defined (S_ISREG) && defined (S_IFREG)
#  define S_ISREG(m) (((m) & S_IFMT) == S_IFREG)
#endif

#ifdef VMS
#include "vms-pwd.h"
#else
#include <pwd.h>
#endif

#include <ctype.h>

#ifdef VMS
#include "vms-dir.h"
#include <perror.h>
#include <stddef.h>
#include <string.h>
#else
#ifdef USG5_4
#include <sys/dirent.h>
#include <fcntl.h>
#include <utime.h>
#else
#include <sys/dir.h>
#endif /* USG5_4 */
#include <errno.h>
#include <sys/param.h>
#endif /* VMS */

#include <errno.h>

#define err_str(a) ((a) < sys_nerr ? sys_errlist[a] : "unknown error")

#ifdef APOLLO
#include <sys/time.h>
#endif

#ifndef USG
#ifndef VMS
#ifndef BSD4_1
#define HAVE_FSYNC
#endif
#endif
#endif

#include "lisp.h"
#include "intl.h"
#include "buffer.h"
#include "insdel.h"

#include "window.h"             /* echo_area_glyphs, minibuf_level */

#ifdef VMS
#include <file.h>
#include <rmsdef.h>
#include <fab.h>
#include <nam.h>
#endif

#include "systime.h"

#include "sysdep.h"

#ifdef HPUX
#include <netio.h>
#ifdef HPUX_PRE_8_0
#include <errnet.h>
#endif
#endif

#ifndef O_WRONLY
#define O_WRONLY 1
#endif

#define min(a, b) ((a) < (b) ? (a) : (b))
#define max(a, b) ((a) > (b) ? (a) : (b))

/* Nonzero during writing of auto-save files */
static int auto_saving;

/* Set by auto_save_1 to mode of original file so Fwrite_region will create
   a new file with the same mode as the original */
static int auto_save_mode_bits;

/* Alist of elements (REGEXP . HANDLER) for file names 
   whose I/O is done with a special handler.  */
Lisp_Object Vfile_name_handler_alist;

/* Functions to be called to process text properties in inserted file.  */
Lisp_Object Vafter_insert_file_functions;

/* Functions to be called to create text property annotations for file.  */
Lisp_Object Vwrite_region_annotate_functions;

/* On VMS, nonzero means write new files with record format stmlf.
   Zero means use var format.  */
int vms_stmlf_recfm;

Lisp_Object Qfile_name_handler_alist;

Lisp_Object Qfile_error, Qfile_already_exists;

Lisp_Object Qauto_save_hook;

Lisp_Object Qcar_less_than_car;

DOESNT_RETURN
report_file_error (string, data)
     CONST char *string;
     Lisp_Object data;
{
  Lisp_Object errstring;

  if (errno >= 0 && errno < sys_nerr)
    errstring = build_string (sys_errlist[errno]);
  else
    errstring = build_string (GETTEXT ("undocumented error code"));

  /* System error messages are capitalized.  Downcase the initial
     unless it is followed by a slash.  */
  if (XSTRING (errstring)->data[1] != '/')
    XSTRING (errstring)->data[0] = DOWNCASE (XSTRING (errstring)->data[0]);

  signal_error (Qfile_error,
                Fcons (build_string (string), Fcons (errstring, data)));
}

static Lisp_Object
close_file_unwind (fd)
     Lisp_Object fd;
{
  if (CONSP (fd))
    {
      if (FIXNUMP (XCONS (fd)->car))
	emacs_close (XINT (XCONS (fd)->car));

      free_cons (XCONS (fd));
    }
  else
    emacs_close (XINT (fd));

  return Qnil;
}

Lisp_Object Qexpand_file_name;
Lisp_Object Qdirectory_file_name;
Lisp_Object Qfile_name_directory;
Lisp_Object Qfile_name_nondirectory;
Lisp_Object Qunhandled_file_name_directory;
Lisp_Object Qfile_name_as_directory;
Lisp_Object Qcopy_file;
Lisp_Object Qmake_directory;
Lisp_Object Qdelete_directory;
Lisp_Object Qdelete_file;
Lisp_Object Qrename_file;
Lisp_Object Qadd_name_to_file;
Lisp_Object Qmake_symbolic_link;
Lisp_Object Qfile_exists_p;
Lisp_Object Qfile_executable_p;
Lisp_Object Qfile_readable_p;
Lisp_Object Qfile_symlink_p;
Lisp_Object Qfile_writable_p;
Lisp_Object Qfile_directory_p;
Lisp_Object Qfile_accessible_directory_p;
Lisp_Object Qfile_modes;
Lisp_Object Qset_file_modes;
Lisp_Object Qfile_newer_than_file_p;
Lisp_Object Qinsert_file_contents;
Lisp_Object Qwrite_region;
Lisp_Object Qverify_visited_file_modtime;
Lisp_Object Qset_visited_file_modtime;

/* If FILENAME is handled specially on account of its syntax,
   return its handler function.  Otherwise, return nil.  */

DEFUN ("find-file-name-handler", 
       Ffind_file_name_handler, Sfind_file_name_handler, 1, 1, 0,
    "Return FILENAME's handler function, if its syntax is handled specially.\n\
Otherwise, return nil.\n\
A file name is handled if one of the regular expressions in\n\
`file-name-handler-alist' matches it.")
   (filename)
  Lisp_Object filename;
{
  /* This function must not munge the match data.  */
  Lisp_Object chain;

  CHECK_STRING (filename, 0);

  for (chain = Vfile_name_handler_alist; CONSP (chain);
       chain = XCONS (chain)->cdr)
    {
      Lisp_Object elt = XCONS (chain)->car;
      QUIT;
      if (CONSP (elt))
	{
	  Lisp_Object string;
	  string = XCONS (elt)->car;
	  if (STRINGP (string)
	      && fast_string_match (string, filename) >= 0)
	    return (XCONS (elt)->cdr);
	}
    }
  return Qnil;
}

static Lisp_Object
call2_check_string (Lisp_Object fn, Lisp_Object arg0, Lisp_Object arg1)
{
  Lisp_Object result = call2 (fn, arg0, arg1);
  CHECK_STRING (result, 0);
  return (result);
}

static Lisp_Object
call3_check_string (Lisp_Object fn, Lisp_Object arg0, 
                    Lisp_Object arg1, Lisp_Object arg2)
{
  Lisp_Object result = call3 (fn, arg0, arg1, arg2);
  CHECK_STRING (result, 0);
  return (result);
}


DEFUN ("file-name-directory", Ffile_name_directory, Sfile_name_directory,
  1, 1, 0,
  "Return the directory component in file name NAME.\n\
Return nil if NAME does not include a directory.\n\
Otherwise return a directory spec.\n\
Given a Unix syntax file name, returns a string ending in slash;\n\
on VMS, perhaps instead a string ending in `:', `]' or `>'.")
  (file)
     Lisp_Object file;
{
  register unsigned char *beg;
  register unsigned char *p;
  Lisp_Object handler;

  CHECK_STRING (file, 0);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (file);
  if (!NILP (handler))
    return (call2_check_string (handler, Qfile_name_directory, file));

  beg = XSTRING (file)->data;
  p = beg + XSTRING (file)->size;

  while (p != beg && p[-1] != '/'
#ifdef VMS
	 && p[-1] != ':' && p[-1] != ']' && p[-1] != '>'
#endif /* VMS */
	 ) p--;

  if (p == beg)
    return Qnil;
  return make_string ((char *) beg, p - beg);
}

DEFUN ("file-name-nondirectory", Ffile_name_nondirectory, Sfile_name_nondirectory,
  1, 1, 0,
  "Return file name NAME sans its directory.\n\
For example, in a Unix-syntax file name,\n\
this is everything after the last slash,\n\
or the entire name if it contains no slash.")
  (file)
     Lisp_Object file;
{
  register unsigned char *beg, *p, *end;
  Lisp_Object handler;

  CHECK_STRING (file, 0);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (file);
  if (!NILP (handler))
    return (call2_check_string (handler, Qfile_name_nondirectory, file));

  beg = XSTRING (file)->data;
  end = p = beg + XSTRING (file)->size;

  while (p != beg && p[-1] != '/'
#ifdef VMS
	 && p[-1] != ':' && p[-1] != ']' && p[-1] != '>'
#endif /* VMS */
	 ) p--;

  return make_string ((char *) p, end - p);
}

DEFUN ("unhandled-file-name-directory",
       Funhandled_file_name_directory, Sunhandled_file_name_directory, 1, 1, 0,
  "Return a directly usable directory name somehow associated with FILENAME.\n\
A `directly usable' directory name is one that may be used without the\n\
intervention of any file handler.\n\
If FILENAME is a directly usable file itself, return\n\
(file-name-directory FILENAME).\n\
The `call-process' and `start-process' functions use this function to\n\
get a current directory to run processes in.")
  (filename)
    Lisp_Object filename;
{
  Lisp_Object handler;

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename);
  if (!NILP (handler))
    return call2 (handler, Qunhandled_file_name_directory, filename);

  return Ffile_name_directory (filename);
}


static char *
file_name_as_directory (out, in)
     char *out, *in;
{
  int size = strlen (in) - 1;

  strcpy (out, in);

#ifdef VMS
  /* Is it already a directory string? */
  if (in[size] == ':' || in[size] == ']' || in[size] == '>')
    return out;
  /* Is it a VMS directory file name?  If so, hack VMS syntax.  */
  else if (! index (in, '/')
	   && ((size > 3 && ! strcmp (&in[size - 3], ".DIR"))
	       || (size > 3 && ! strcmp (&in[size - 3], ".dir"))
	       || (size > 5 && (! strncmp (&in[size - 5], ".DIR", 4)
				|| ! strncmp (&in[size - 5], ".dir", 4))
		   && (in[size - 1] == '.' || in[size - 1] == ';')
		   && in[size] == '1')))
    {
      register char *p, *dot;
      char brack;

      /* x.dir -> [.x]
	 dir:x.dir --> dir:[x]
	 dir:[x]y.dir --> dir:[x.y] */
      p = in + size;
      while (p != in && *p != ':' && *p != '>' && *p != ']') p--;
      if (p != in)
	{
	  strncpy (out, in, p - in);
	  out[p - in] = '\0';
	  if (*p == ':')
	    {
	      brack = ']';
	      strcat (out, ":[");
	    }
	  else
	    {
	      brack = *p;
	      strcat (out, ".");
	    }
	  p++;
	}
      else
	{
	  brack = ']';
	  strcpy (out, "[.");
	}
      dot = index (p, '.');
      if (dot)
	{
	  /* blindly remove any extension */
	  size = strlen (out) + (dot - p);
	  strncat (out, p, dot - p);
	}
      else
	{
	  strcat (out, p);
	  size = strlen (out);
	}
      out[size++] = brack;
      out[size] = '\0';
    }
#else /* not VMS */
  /* For Unix syntax, Append a slash if necessary */
  if (out[size] != '/')
    strcat (out, "/");
#endif /* not VMS */
  return out;
}

DEFUN ("file-name-as-directory", Ffile_name_as_directory,
       Sfile_name_as_directory, 1, 1, 0,
  "Return a string representing file FILENAME interpreted as a directory.\n\
This operation exists because a directory is also a file, but its name as\n\
a directory is different from its name as a file.\n\
The result can be used as the value of `default-directory'\n\
or passed as second argument to `expand-file-name'.\n\
For a Unix-syntax file name, just appends a slash.\n\
On VMS, converts \"[X]FOO.DIR\" to \"[X.FOO]\", etc.")
  (file)
     Lisp_Object file;
{
  char *buf;
  Lisp_Object handler;

  CHECK_STRING (file, 0);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (file);
  if (!NILP (handler))
    return (call2_check_string (handler, Qfile_name_as_directory, file));

  buf = (char *) alloca (XSTRING (file)->size + 10);
  return build_string (file_name_as_directory (buf, (char *) XSTRING (file)->data));
}

/*
 * Convert from directory name to filename.
 * On VMS:
 *       xyzzy:[mukesh.emacs] => xyzzy:[mukesh]emacs.dir.1
 *       xyzzy:[mukesh] => xyzzy:[000000]mukesh.dir.1
 * On UNIX, it's simple: just make sure there is a terminating /

 * Value is nonzero if the string output is different from the input.
 */

static int
directory_file_name (src, dst)
     CONST char *src;
     char *dst;
{
  long slen;
#ifdef VMS
  long rlen;
  char * ptr, * rptr;
  char bracket;
  struct FAB fab = cc$rms_fab;
  struct NAM nam = cc$rms_nam;
  char esa[NAM$C_MAXRSS];
#endif /* VMS */

  slen = strlen (src);
#ifdef VMS
  if (! index (src, '/')
      && (src[slen - 1] == ']'
	  || src[slen - 1] == ':'
	  || src[slen - 1] == '>'))
    {
      /* VMS style - convert [x.y.z] to [x.y]z, [x] to [000000]x */
      fab.fab$l_fna = src;
      fab.fab$b_fns = slen;
      fab.fab$l_nam = &nam;
      fab.fab$l_fop = FAB$M_NAM;

      nam.nam$l_esa = esa;
      nam.nam$b_ess = sizeof esa;
      nam.nam$b_nop |= NAM$M_SYNCHK;

      /* We call SYS$PARSE to handle such things as [--] for us. */
      if (SYS$PARSE(&fab, 0, 0) == RMS$_NORMAL)
	{
	  slen = nam.nam$b_esl;
	  if (esa[slen - 1] == ';' && esa[slen - 2] == '.')
	    slen -= 2;
	  esa[slen] = '\0';
	  src = esa;
	}
      if (src[slen - 1] != ']' && src[slen - 1] != '>')
	{
	  /* what about when we have logical_name:???? */
	  if (src[slen - 1] == ':')
	    {			/* Xlate logical name and see what we get */
	      ptr = strcpy (dst, src); /* upper case for getenv */
	      while (*ptr)
		{
		  *ptr = toupper (*ptr);
		  ptr++;
		}
	      dst[slen - 1] = 0;	/* remove colon */
	      if (!(src = egetenv (dst)))
		return 0;
	      /* should we jump to the beginning of this procedure?
		 Good points: allows us to use logical names that xlate
		 to Unix names,
		 Bad points: can be a problem if we just translated to a device
		 name...
		 For now, I'll punt and always expect VMS names, and hope for
		 the best! */
	      slen = strlen (src);
	      if (src[slen - 1] != ']' && src[slen - 1] != '>')
		{ /* no recursion here! */
		  strcpy (dst, src);
		  return 0;
		}
	    }
	  else
	    {		/* not a directory spec */
	      strcpy (dst, src);
	      return 0;
	    }
	}
      bracket = src[slen - 1];

      /* If bracket is ']' or '>', bracket - 2 is the corresponding
	 opening bracket.  */
      ptr = index (src, bracket - 2);
      if (ptr == 0)
	{ /* no opening bracket */
	  strcpy (dst, src);
	  return 0;
	}
      if (!(rptr = rindex (src, '.')))
	rptr = ptr;
      slen = rptr - src;
      strncpy (dst, src, slen);
      dst[slen] = '\0';
      if (*rptr == '.')
	{
	  dst[slen++] = bracket;
	  dst[slen] = '\0';
	}
      else
	{
	  /* If we have the top-level of a rooted directory (i.e. xx:[000000]),
	     then translate the device and recurse. */
	  if (dst[slen - 1] == ':'
	      && dst[slen - 2] != ':'	/* skip decnet nodes */
	      && strcmp(src + slen, "[000000]") == 0)
	    {
	      dst[slen - 1] = '\0';
	      if ((ptr = egetenv (dst))
		  && (rlen = strlen (ptr) - 1) > 0
		  && (ptr[rlen] == ']' || ptr[rlen] == '>')
		  && ptr[rlen - 1] == '.')
		{
		  char * buf = (char *) alloca (strlen (ptr) + 1);
		  strcpy (buf, ptr);
		  buf[rlen - 1] = ']';
		  buf[rlen] = '\0';
		  return directory_file_name (buf, dst);
		}
	      else
		dst[slen - 1] = ':';
	    }
	  strcat (dst, "[000000]");
	  slen += 8;
	}
      rptr++;
      rlen = strlen (rptr) - 1;
      strncat (dst, rptr, rlen);
      dst[slen + rlen] = '\0';
      strcat (dst, ".DIR.1");
      return 1;
    }
#endif /* VMS */
  /* Process as Unix format: just remove any final slash.
     But leave "/" unchanged; do not change it to "".  */
  strcpy (dst, src);
  if (slen > 1 && dst[slen - 1] == '/')
    dst[slen - 1] = 0;
  return 1;
}

DEFUN ("directory-file-name", Fdirectory_file_name, Sdirectory_file_name,
  1, 1, 0,
  "Returns the file name of the directory named DIR.\n\
This is the name of the file that holds the data for the directory DIR.\n\
This operation exists because a directory is also a file, but its name as\n\
a directory is different from its name as a file.\n\
In Unix-syntax, this function just removes the final slash.\n\
On VMS, given a VMS-syntax directory name such as \"[X.Y]\",\n\
it returns a file name such as \"[X]Y.DIR.1\".")
  (directory)
     Lisp_Object directory;
{
  char *buf;
  Lisp_Object handler;

  CHECK_STRING (directory, 0);

#if 0 /* >>> WTF? */
  if (NILP (directory))
    return Qnil;
#endif

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (directory);
  if (!NILP (handler))
    return (call2_check_string (handler, Qdirectory_file_name, directory));
#ifdef VMS
  /* 20 extra chars is insufficient for VMS, since we might perform a
     logical name translation. an equivalence string can be up to 255
     chars long, so grab that much extra space...  - sss */
  buf = (char *) alloca (XSTRING (directory)->size + 20 + 255);
#else
  buf = (char *) alloca (XSTRING (directory)->size + 20);
#endif
  directory_file_name ((char *) XSTRING (directory)->data, buf);
  return build_string (buf);
}

DEFUN ("make-temp-name", Fmake_temp_name, Smake_temp_name, 1, 1, 0,
  "Generate temporary file name (string) starting with PREFIX (a string).\n\
The Emacs process number forms part of the result,\n\
so there is no danger of generating a name being used by another process.")
  (prefix)
     Lisp_Object prefix;
{
  CONST char suffix[] = "XXXXXX";
  char *data;
  int len;
  Lisp_Object val;

  CHECK_STRING (prefix, 0);
  len = string_length (XSTRING (prefix));
  val = make_uninit_string (len + countof (suffix) - 1);
  data = (char *) XSTRING (val)->data;
  memcpy (data, XSTRING (prefix)->data, len);
  memcpy (data + len, suffix, countof (suffix));
  mktemp (data);

  return val;
}

DEFUN ("expand-file-name", Fexpand_file_name, Sexpand_file_name, 1, 2, 0,
  "Convert FILENAME to absolute, and canonicalize it.\n\
Second arg DEFAULT is directory to start with if FILENAME is relative\n\
 (does not start with slash); if DEFAULT is nil or missing,\n\
the current buffer's value of default-directory is used.\n\
Path components that are `.' are removed, and \n\
path components followed by `..' are removed, along with the `..' itself;\n\
note that these simplifications are done without checking the resulting\n\
paths in the file system.\n\
An initial `~/' expands to your home directory.\n\
An initial `~USER/' expands to USER's home directory.\n\
See also the function `substitute-in-file-name'.")
     (name, defalt)
     Lisp_Object name, defalt;
{
  unsigned char *nm;
  
  register unsigned char *newdir, *p, *o;
  int tlen;
  unsigned char *target;
  struct passwd *pw;
#ifdef VMS
  unsigned char * colon = 0;
  unsigned char * close = 0;
  unsigned char * slash = 0;
  unsigned char * brack = 0;
  int lbrack = 0, rbrack = 0;
  int dots = 0;
#endif /* VMS */
  Lisp_Object handler;
  
  CHECK_STRING (name, 0);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (name);
  if (!NILP (handler))
    return (call3_check_string (handler, Qexpand_file_name, name, defalt));

  /* Use the buffer's default-directory if DEFALT is omitted.  */
  if (NILP (defalt))
    defalt = current_buffer->directory;
  CHECK_STRING (defalt, 1);

  /* Make sure DEFALT is properly expanded.
     It would be better to do this down below where we actually use
     defalt.  Unfortunately, calling Fexpand_file_name recursively
     could invoke GC, and the strings might be relocated.  This would
     be annoying because we have pointers into strings lying around
     that would need adjusting, and people would add new pointers to
     the code and forget to adjust them, resulting in intermittent bugs.
     Putting this call here avoids all that crud.

     The EQ test avoids infinite recursion.  */
  if (! NILP (defalt) && !EQ (defalt, name)
      /* This saves time in a common case.  */
      /* >>> Unix-specific */
      && XSTRING (defalt)->data[0] != '/')
    {
      struct gcpro gcpro1;

      GCPRO1 (name);
      defalt = Fexpand_file_name (defalt, Qnil);
      UNGCPRO;
    }

#ifdef VMS
  /* Filenames on VMS are always upper case.  */
  name = Fupcase (name);
#endif

  nm = XSTRING (name)->data;
  
  /* If nm is absolute, flush ...// and detect /./ and /../.
     If no /./ or /../ we can return right away. */
  if (
      nm[0] == '/'
#ifdef VMS
      || index (nm, ':')
#endif /* VMS */
      )
    {
      /* If it turns out that the filename we want to return is just a
	 suffix of FILENAME, we don't need to go through and edit
	 things; we just need to construct a new string using data
	 starting at the middle of FILENAME.  If we set lose to a
	 non-zero value, that means we've discovered that we can't do
	 that cool trick.  */
      int lose = 0;

      p = nm;
      while (*p)
	{
	  /* Since we know the path is absolute, we can assume that each
	     element starts with a "/".  */

	  /* "//" anywhere isn't necessarily hairy; we just start afresh
	     with the second slash.  */
	  if (p[0] == '/' && p[1] == '/'
#ifdef APOLLO
	      /* // at start of filename is meaningful on Apollo system */
	      && nm != p
#endif /* APOLLO */
	      )
	    nm = p + 1;

	  /* "~" is hairy as the start of any path element.  */
	  if (p[0] == '/' && p[1] == '~')
	    nm = p + 1, lose = 1;

	  /* "." and ".." are hairy.  */
	  if (p[0] == '/' && p[1] == '.'
	      && (p[2] == '/' || p[2] == 0
		  || (p[2] == '.' && (p[3] == '/' || p[3] == 0))))
	    lose = 1;
#ifdef VMS
	  if (p[0] == '\\')
	    lose = 1;
	  if (p[0] == '/') {
	    /* if dev:[dir]/, move nm to / */
	    if (!slash && p > nm && (brack || colon)) {
	      nm = (brack ? brack + 1 : colon + 1);
	      lbrack = rbrack = 0;
	      brack = 0;
	      colon = 0;
	    }
	    slash = p;
	  }
	  if (p[0] == '-')
#ifndef VMS4_4
	    /* VMS pre V4.4,convert '-'s in filenames. */
	    if (lbrack == rbrack)
	      {
		if (dots < 2)	/* this is to allow negative version numbers */
		  p[0] = '_';
	      }
	    else
#endif /* VMS4_4 */
	      if (lbrack > rbrack &&
		  ((p[-1] == '.' || p[-1] == '[' || p[-1] == '<') &&
		   (p[1] == '.' || p[1] == ']' || p[1] == '>')))
		lose = 1;
#ifndef VMS4_4
	      else
		p[0] = '_';
#endif /* VMS4_4 */
	  /* count open brackets, reset close bracket pointer */
	  if (p[0] == '[' || p[0] == '<')
	    lbrack++, brack = 0;
	  /* count close brackets, set close bracket pointer */
	  if (p[0] == ']' || p[0] == '>')
	    rbrack++, brack = p;
	  /* detect ][ or >< */
	  if ((p[0] == ']' || p[0] == '>') && (p[1] == '[' || p[1] == '<'))
	    lose = 1;
	  if ((p[0] == ':' || p[0] == ']' || p[0] == '>') && p[1] == '~')
	    nm = p + 1, lose = 1;
	  if (p[0] == ':' && (colon || slash))
	    /* if dev1:[dir]dev2:, move nm to dev2: */
	    if (brack)
	      {
		nm = brack + 1;
		brack = 0;
	      }
	    /* if /pathname/dev:, move nm to dev: */
	    else if (slash)
	      nm = slash + 1;
	    /* if node::dev:, move colon following dev */
	    else if (colon && colon[-1] == ':')
	      colon = p;
	    /* if dev1:dev2:, move nm to dev2: */
	    else if (colon && colon[-1] != ':')
	      {
		nm = colon + 1;
		colon = 0;
	      }
	  if (p[0] == ':' && !colon)
	    {
	      if (p[1] == ':')
		p++;
	      colon = p;
	    }
	  if (lbrack == rbrack)
	    if (p[0] == ';')
	      dots = 2;
	    else if (p[0] == '.')
	      dots++;
#endif /* VMS */
	  p++;
	}
      if (!lose)
	{
#ifdef VMS
	  if (index (nm, '/'))
	    return build_string (sys_translate_unix (nm));
#endif /* VMS */
	  if (nm == XSTRING (name)->data)
	    return name;
	  return build_string ((char *) nm);
	}
    }

  /* Now determine directory to start with and put it in newdir */

  newdir = 0;

  if (nm[0] == '~')		/* prefix ~ */
  {
    if (nm[1] == '/'
#ifdef VMS
	|| nm[1] == ':'
#endif /* VMS */
	|| nm[1] == 0)		/* ~ by itself */
      {
	if (!(newdir = (unsigned char *) egetenv ("HOME")))
	  newdir = (unsigned char *) "";
	nm++;
#ifdef VMS
	nm++;			/* Don't leave the slash in nm.  */
#endif /* VMS */
      }
    else			/* ~user/filename */
      {
	for (p = nm; *p && (*p != '/'
#ifdef VMS
			    && *p != ':'
#endif /* VMS */
			    ); p++);
	o = (unsigned char *) alloca (p - nm + 1);
	memcpy (o, (char *) nm, p - nm);
	o [p - nm] = 0;

	pw = (struct passwd *) getpwnam ((char *) o + 1);
	if (pw)
	  {
	    newdir = (unsigned char *) pw -> pw_dir;
#ifdef VMS
	    nm = p + 1;		/* skip the terminator */
#else
	    nm = p;
#endif /* VMS */
	  }

	/* If we don't find a user of that name, leave the name
	   unchanged; don't move nm forward to p.  */
      }
  }

  if (nm[0] != '/'
#ifdef VMS
      && !index (nm, ':')
#endif /* not VMS */
      && !newdir)
    {
      newdir = XSTRING (defalt)->data;
    }

  if (newdir != 0)
    {
      /* Get rid of any slash at the end of newdir.  */
      int length = strlen ((char *) newdir);
      /* Adding `length > 1 &&' makes ~ expand into / when homedir
	 is the root dir.  People disagree about whether that is right.
	 Anyway, we can't take the risk of this change now.  */
      if (newdir[length - 1] == '/')
	{
	  unsigned char *temp = (unsigned char *) alloca (length);
	  memcpy (temp, newdir, length - 1);
	  temp[length - 1] = 0;
	  newdir = temp;
	}
      tlen = length + 1;
    }
  else
    tlen = 0;

  /* Now concatenate the directory and name to new space in the stack frame */
  tlen += strlen ((char *) nm) + 1;
  target = (unsigned char *) alloca (tlen);
  *target = 0;

  if (newdir)
    {
#ifndef VMS
      if (nm[0] == 0 || nm[0] == '/')
	strcpy ((char *) target, (char *) newdir);
      else
#endif
      file_name_as_directory ((char *) target, (char *) newdir);
    }

  strcat ((char *) target, (char *) nm);
#ifdef VMS
  if (index (target, '/'))
    strcpy (target, sys_translate_unix (target));
#endif /* VMS */

  /* Now canonicalize by removing /. and /foo/.. if they appear.  */

  p = target;
  o = target;

  while (*p)
    {
#ifdef VMS
      if (*p != ']' && *p != '>' && *p != '-')
	{
	  if (*p == '\\')
	    p++;
	  *o++ = *p++;
	}
      else if ((p[0] == ']' || p[0] == '>') && p[0] == p[1] + 2)
	/* brackets are offset from each other by 2 */
	{
	  p += 2;
	  if (*p != '.' && *p != '-' && o[-1] != '.')
	    /* convert [foo][bar] to [bar] */
	    while (o[-1] != '[' && o[-1] != '<')
	      o--;
	  else if (*p == '-' && *o != '.')
	    *--p = '.';
	}
      else if (p[0] == '-' && o[-1] == '.' &&
	       (p[1] == '.' || p[1] == ']' || p[1] == '>'))
	/* flush .foo.- ; leave - if stopped by '[' or '<' */
	{
	  do
	    o--;
	  while (o[-1] != '.' && o[-1] != '[' && o[-1] != '<');
	  if (p[1] == '.')	/* foo.-.bar ==> bar*/
	    p += 2;
	  else if (o[-1] == '.') /* '.foo.-]' ==> ']' */
	    p++, o--;
	  /* else [foo.-] ==> [-] */
	}
      else
	{
#ifndef VMS4_4
	  if (*p == '-' &&
	      o[-1] != '[' && o[-1] != '<' && o[-1] != '.' &&
	      p[1] != ']' && p[1] != '>' && p[1] != '.')
	    *p = '_';
#endif /* VMS4_4 */
	  *o++ = *p++;
	}
#else /* not VMS */
      if (*p != '/')
 	{
	  *o++ = *p++;
	}
      else if (!strncmp ((char *) p, "//", 2)
#ifdef APOLLO
	       /* // at start of filename is meaningful in Apollo system */
	       && o != target
#endif /* APOLLO */
	       )
	{
	  o = target;
	  p++;
	}
      else if (p[0] == '/'
               && p[1] == '.'
               && (p[2] == '/' || p[2] == 0))
	{
	  /* If "/." is the entire filename, keep the "/".  Otherwise,
	     just delete the whole "/.".  */
	  if (o == target && p[2] == '\0')
	    *o++ = *p;
	  p += 2;
	}
      else if (!strncmp ((char *) p, "/..", 3)
	       /* `/../' is the "superroot" on certain file systems.  */
	       && o != target
	       && (p[3] == '/' || p[3] == 0))
	{
	  while (o != target && *--o != '/')
	    ;
#ifdef APOLLO
	  if (o == target + 1 && o[-1] == '/' && o[0] == '/')
	    ++o;
	  else
#endif /* APOLLO */
	  if (o == target && *o == '/')
	    ++o;
	  p += 3;
	}
      else
 	{
	  *o++ = *p++;
	}
#endif /* not VMS */
    }

  return make_string ((char *) target, o - target);
}

extern char *realpath ();

DEFUN ("file-truename", Ffile_truename, Sfile_truename, 1, 2, 0,
  "Returns the canonical name of the given FILE.\n\
Second arg DEFAULT is directory to start with if FILE is relative\n\
 (does not start with slash); if DEFAULT is nil or missing,\n\
 the current buffer's value of default-directory is used.\n\
No component of the resulting pathname will be a symbolic link, as\n\
 in the realpath() function.")
     (filename, defalt)
     Lisp_Object filename, defalt;
{
  struct gcpro gcpro1;
  Lisp_Object expanded_name;

  CHECK_STRING (filename, 0);

  GCPRO1 (filename);
  expanded_name = Fexpand_file_name (filename, defalt);
  UNGCPRO;

  if (!STRINGP (expanded_name))
    return Qnil;

#ifdef VMS
  return (expanded_name);
#else
  {
    char resolved_path[MAXPATHLEN];
    char path[MAXPATHLEN];
    char *p = path;
    int elen = string_length (XSTRING (expanded_name));
    
    if (elen >= countof (path))
      goto toolong;
    
    memcpy (path, (char *) XSTRING (expanded_name)->data, elen + 1);
    /* memset (resolved_path, 0, sizeof (resolved_path)); */

    /* Try doing it all at once. */
    if (!realpath (path, resolved_path))
    {
      /* Didn't resolve it -- have to do it one component at a time. */
      /* "realpath" is a typically useless, stupid un*x piece of crap.
         It claims to return a useful value in the "error" case, but since
         there is no indication provided of how far along the pathname
         the function went before erring, there is no way to use the
         partial result returned.  What a piece of junk. */
      for (;;)
      {
        p = (char *) memchr (p + 1, '/', elen - (p + 1 - path));
        if (p)
          *p = 0;

        /* memset (resolved_path, 0, sizeof (resolved_path)); */
        if (realpath (path, resolved_path))
        {
          if (p)
            *p = '/';
          else
            break;

        }
        else if (errno == ENOENT)
        {
          /* Failed on this component.  Just tack on the rest of
             the string and we are done. */
          int rlen = strlen (resolved_path);

          /* "On failure, it returns NULL, sets errno to indicate
             the error, and places in resolved_path the absolute pathname
             of the path component which could not be resolved." */
          if (p)
          {
            int plen = elen - (p - path);
            
            if (rlen > 1 && resolved_path[rlen - 1] == '/')
              rlen = rlen - 1;

            if (plen + rlen + 1 > countof (resolved_path))
              goto toolong;

            resolved_path[rlen] = '/';
            memcpy (resolved_path + rlen + 1, p + 1, plen + 1 - 1);
          }
          break;
        }
        else
          goto lose;
      }
    }

    {
      int rlen = strlen (resolved_path);
      if (elen > 0 && XSTRING (expanded_name)->data[elen - 1] == '/'
          && !(rlen > 0 && resolved_path[rlen - 1] == '/'))
      {
        if (rlen + 1 > countof (resolved_path))
          goto toolong;
        resolved_path[rlen] = '/';
        resolved_path[rlen + 1] = 0;
        rlen = rlen + 1;
      }
      return make_string (resolved_path, rlen);
    }

  toolong:
    errno = ENAMETOOLONG;
    goto lose;
  lose:
    report_file_error (GETTEXT ("Finding truename"), list1 (expanded_name));
  }
#endif /* not VMS */
}


DEFUN ("substitute-in-file-name", Fsubstitute_in_file_name,
  Ssubstitute_in_file_name, 1, 1, 0,
  "Substitute environment variables referred to in FILENAME.\n\
`$FOO' where FOO is an environment variable name means to substitute\n\
the value of that variable.  The variable name should be terminated\n\
with a character not a letter, digit or underscore; otherwise, enclose\n\
the entire variable name in braces.\n\
If `/~' appears, all of FILENAME through that `/' is discarded.\n\n\
On VMS, `$' substitution is not done; this function does little and only\n\
duplicates what `expand-file-name' does.")
  (string)
     Lisp_Object string;
{
  unsigned char *nm;

  register unsigned char *s, *p, *o, *x, *endp;
  unsigned char *target;
  int total = 0;
  int substituted = 0;
  unsigned char *xnm;

  CHECK_STRING (string, 0);

  nm = XSTRING (string)->data;
  endp = nm + XSTRING (string)->size;

  /* If /~ or // appears, discard everything through first slash. */

  for (p = nm; p != endp; p++)
    {
      if ((p[0] == '~' ||
#ifdef APOLLO
	   /* // at start of file name is meaningful in Apollo system */
	   (p[0] == '/' && p - 1 != nm)
#else /* not APOLLO */
	   p[0] == '/'
#endif /* not APOLLO */
	   )
	  && p != nm &&
#ifdef VMS
	  (p[-1] == ':' || p[-1] == ']' || p[-1] == '>' ||
#endif /* VMS */
	  p[-1] == '/')
#ifdef VMS
	  )
#endif /* VMS */
	{
	  nm = p;
	  substituted = 1;
	}
    }

#ifdef VMS
  return build_string (nm);
#else

  /* See if any variables are substituted into the string
     and find the total length of their values in `total' */

  for (p = nm; p != endp;)
    if (*p != '$')
      p++;
    else
      {
	p++;
	if (p == endp)
	  goto badsubst;
	else if (*p == '$')
	  {
	    /* "$$" means a single "$" */
	    p++;
	    total -= 1;
	    substituted = 1;
	    continue;
	  }
	else if (*p == '{')
	  {
	    o = ++p;
	    while (p != endp && *p != '}') p++;
	    if (*p != '}') goto missingclose;
	    s = p;
	  }
	else
	  {
	    o = p;
	    while (p != endp && (isalnum (*p) || *p == '_')) p++;
	    s = p;
	  }

	/* Copy out the variable name */
	target = (unsigned char *) alloca (s - o + 1);
	strncpy ((char *) target, (char *) o, s - o);
	target[s - o] = 0;

	/* Get variable value */
	o = (unsigned char *) egetenv ((char *) target);
/* The presence of this code makes vax 5.0 crash, for reasons yet unknown */
#if 0
#ifdef USG
	if (!o && !strcmp (target, "USER"))
	  o = egetenv ("LOGNAME");
#endif /* USG */
#endif /* 0 */
	if (!o) goto badvar;
	total += strlen ((char *) o);
	substituted = 1;
      }

  if (!substituted)
    return string;

  /* If substitution required, recopy the string and do it */
  /* Make space in stack frame for the new copy */
  xnm = (unsigned char *) alloca (XSTRING (string)->size + total + 1);
  x = xnm;

  /* Copy the rest of the name through, replacing $ constructs with values */
  for (p = nm; *p;)
    if (*p != '$')
      *x++ = *p++;
    else
      {
	p++;
	if (p == endp)
	  goto badsubst;
	else if (*p == '$')
	  {
	    *x++ = *p++;
	    continue;
	  }
	else if (*p == '{')
	  {
	    o = ++p;
	    while (p != endp && *p != '}') p++;
	    if (*p != '}') goto missingclose;
	    s = p++;
	  }
	else
	  {
	    o = p;
	    while (p != endp && (isalnum (*p) || *p == '_')) p++;
	    s = p;
	  }

	/* Copy out the variable name */
	target = (unsigned char *) alloca (s - o + 1);
	strncpy ((char *) target, (char *) o, s - o);
	target[s - o] = 0;

	/* Get variable value */
	o = (unsigned char *) egetenv ((char *) target);
/* The presence of this code makes vax 5.0 crash, for reasons yet unknown */
#if 0
#ifdef USG
	if (!o && !strcmp (target, "USER"))
	  o = egetenv ("LOGNAME");
#endif /* USG */
#endif /* 0 */
	if (!o)
	  goto badvar;

	strcpy ((char *) x, (char *) o);
	x += strlen ((char *) o);
      }

  *x = 0;

  /* If /~ or // appears, discard everything through first slash. */

  for (p = xnm; p != x; p++)
    if ((p[0] == '~' ||
#ifdef APOLLO
	 /* // at start of file name is meaningful in Apollo system */
	 (p[0] == '/' && p - 1 != xnm)
#else /* not APOLLO */
	 p[0] == '/'
#endif /* not APOLLO */
	 )
	&& p != nm
	/* don't do p[-1] if that would go off the beginning --jwz */
	&& (p > xnm && p[-1] == '/'))
      xnm = p;

  return make_string ((char *) xnm, x - xnm);

 badsubst:
  error (GETTEXT ("Bad format environment-variable substitution"));
 missingclose:
  error (GETTEXT ("Missing \"}\" in environment-variable substitution"));
 badvar:
  error (GETTEXT ("Substituting nonexistent environment variable \"%s\""),
	 target);

  /* NOTREACHED */
#endif /* not VMS */
}

/* (directory-file-name (expand-file-name FOO)) */

Lisp_Object
expand_and_dir_to_file (filename, defdir)
     Lisp_Object filename, defdir;
{
  register Lisp_Object abspath;
  struct gcpro gcpro1;

  GCPRO1 (filename);
  abspath = Fexpand_file_name (filename, defdir);
#ifdef VMS
  {
    register int c = XSTRING (abspath)->data[XSTRING (abspath)->size - 1];
    if (c == ':' || c == ']' || c == '>')
      abspath = Fdirectory_file_name (abspath);
  }
#else
  /* Remove final slash, if any (unless path is root).
     stat behaves differently depending!  */
  if (XSTRING (abspath)->size > 1
      && XSTRING (abspath)->data[XSTRING (abspath)->size - 1] == '/')
    /* We cannot take shortcuts; they might be wrong for magic file names.  */
    abspath = Fdirectory_file_name (abspath);
#endif
  UNGCPRO;
  return abspath;
}

static void
barf_or_query_if_file_exists (absname, querystring, interactive)
     Lisp_Object absname;
     CONST char *querystring;
     int interactive;
{
  if (access ((char *) XSTRING (absname)->data, 4) >= 0)
    {
      register Lisp_Object tem;
      struct gcpro gcpro1;

      GCPRO1 (absname);
      if (interactive)
        tem = call1 (Qyes_or_no_p,
                     (format1 (GETTEXT ("File %s already exists; %s anyway? "),
                               XSTRING (absname)->data, querystring)));
      else
        tem = Qnil;
      UNGCPRO;
      if (NILP (tem))
	Fsignal (Qfile_already_exists,
		 list2 (build_string (GETTEXT ("File already exists")),
			absname));
    }
  return;
}

DEFUN ("copy-file", Fcopy_file, Scopy_file, 2, 4,
  "fCopy file: \nFCopy %s to file: \np\nP",
  "Copy FILE to NEWNAME.  Both args must be strings.\n\
Signals a `file-already-exists' error if file NEWNAME already exists,\n\
unless a third argument OK-IF-ALREADY-EXISTS is supplied and non-nil.\n\
A number as third arg means request confirmation if NEWNAME already exists.\n\
This is what happens in interactive use with M-x.\n\
Fourth arg KEEP-TIME non-nil means give the new file the same\n\
last-modified time as the old one.  (This works on only some systems.)\n\
A prefix arg makes KEEP-TIME non-nil.")
  (filename, newname, ok_if_already_exists, keep_date)
     Lisp_Object filename, newname, ok_if_already_exists, keep_date;
{
  int ifd, ofd, n;
  char buf[16 * 1024];
  struct stat st;
  Lisp_Object handler;
  int speccount = specpdl_depth ();
  struct gcpro gcpro1, gcpro2;
  int input_file_statable_p;

  GCPRO2 (filename, newname);
  CHECK_STRING (filename, 0);
  CHECK_STRING (newname, 1);
  filename = Fexpand_file_name (filename, Qnil);
  newname = Fexpand_file_name (newname, Qnil);

  /* If the input file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename);
  /* Likewise for output file name.  */
  if (NILP (handler))
    handler = Ffind_file_name_handler (newname);
  if (!NILP (handler))
  {
    UNGCPRO;
    return call5 (handler, Qcopy_file, filename, newname,
		  ok_if_already_exists, keep_date);
  }

  /* When second argument is a directory, copy the file into it.
     (copy-file "foo" "bar/") == (copy-file "foo" "bar/foo")
   */
  if (!NILP (Ffile_directory_p (newname)))
    {
      Lisp_Object args[3];
      struct gcpro gcpro1;
      int i = 1;

      args[0] = newname; 
      args[1] = Qnil; args[2] = Qnil;
      GCPRO1 (*args); 
      gcpro1.nvars = 3;
      if (XSTRING (newname)->data [XSTRING (newname)->size - 1] != '/')
	args[i++] = build_string ("/");
      args[i++] = Ffile_name_nondirectory (filename);
      newname = Fconcat (i, args);
      UNGCPRO;
    }

  if (NILP (ok_if_already_exists)
      || FIXNUMP (ok_if_already_exists))
    barf_or_query_if_file_exists (newname, GETTEXT ("copy to it"),
				  FIXNUMP (ok_if_already_exists));

  ifd = emacs_open ((char *) XSTRING (filename)->data, 0, 0);
  if (ifd < 0)
    report_file_error (GETTEXT ("Opening input file"), Fcons (filename, Qnil));

  record_unwind_protect (close_file_unwind, make_number (ifd));

  /* We can only copy regular files and symbolic links.  Other files are not
     copyable by us. */
  input_file_statable_p = (fstat (ifd, &st) >= 0);

#if defined (S_ISREG) && defined (S_ISLNK)
  if (input_file_statable_p)
    {
      if (!(S_ISREG (st.st_mode)) && !(S_ISLNK (st.st_mode)))
	{
#if defined (EISDIR)
	  /* Get a better looking error message. */
	  errno = EISDIR;
#endif /* EISDIR */
	report_file_error ("Non-regular file", Fcons (filename, Qnil));
	}
    }
#endif /* S_ISREG && S_ISLNK */

#ifdef VMS
  /* Create the copy file with the same record format as the input file */
  ofd = sys_creat ((char *) XSTRING (newname)->data, 0666, ifd);
#else
  ofd = creat ((char *) XSTRING (newname)->data, 0666);
#endif /* VMS */
  if (ofd < 0)
    report_file_error (GETTEXT ("Opening output file"), list1 (newname));

  {
    Lisp_Object ofd_locative = list1 (make_number (ofd));

    record_unwind_protect (close_file_unwind, ofd_locative);

    immediate_quit = 1;
    QUIT;
    while ((n = emacs_read (ifd, buf, sizeof (buf))) > 0)
    {
      if (emacs_write (ofd, buf, n) != n)
	report_file_error (GETTEXT ("I/O error"), list1 (newname));
    }
    immediate_quit = 0;

    if (input_file_statable_p)
    {
      if (!NILP (keep_date))
      {
        EMACS_TIME atime, mtime;
        EMACS_SET_SECS_USECS (atime, st.st_atime, 0);
        EMACS_SET_SECS_USECS (mtime, st.st_mtime, 0);
        EMACS_SET_UTIMES ((char *) XSTRING (newname)->data, atime, mtime);
      }
#ifdef APOLLO
      if (!egetenv ("USE_DOMAIN_ACLS"))
#endif
	chmod ((char *) XSTRING (newname)->data, st.st_mode & 07777);
    }

    /* We'll close it by hand */
    XCONS (ofd_locative)->car = Qnil;

    /* Close ifd */
    unbind_to (speccount, Qnil);
  }

  if (emacs_close (ofd) < 0)
    report_file_error (GETTEXT ("I/O error"), Fcons (newname, Qnil));

  UNGCPRO;
  return Qnil;
}

DEFUN ("make-directory-internal", Fmake_directory_internal,
       Smake_directory_internal, 1, 1, 0,
  "Create a directory.  One argument, a file name string.")
  (dirname)
     Lisp_Object dirname;
{
  char dir [MAXPATHLEN];
  Lisp_Object handler;

  CHECK_STRING (dirname, 0);
  dirname = Fexpand_file_name (dirname, Qnil);

  handler = Ffind_file_name_handler (dirname);
  if (!NILP (handler))
    return (call3 (handler, Qmake_directory, dirname, Qnil));
 
  if (XSTRING (dirname)->size > (sizeof (dir) - 1))
    {
      return Fsignal (Qfile_error,
		      list3 (build_string (GETTEXT ("Creating directory")),
			     build_string (GETTEXT ("pathame too long")),
			     dirname));
    }
  strncpy (dir, (char *) XSTRING (dirname)->data, XSTRING (dirname)->size + 1);

#ifndef VMS
  if (dir [XSTRING (dirname)->size - 1] == '/')
    dir [XSTRING (dirname)->size - 1] = 0;
#endif

  if (mkdir (dir, 0777) != 0)
    report_file_error (GETTEXT ("Creating directory"), list1 (dirname));

  return Qnil;
}

DEFUN ("delete-directory", Fdelete_directory, Sdelete_directory, 1, 1, "FDelete directory: ",
  "Delete a directory.  One argument, a file name string.")
  (dirname)
     Lisp_Object dirname;
{
  Lisp_Object handler;

  CHECK_STRING (dirname, 0);
  dirname = Fexpand_file_name (dirname, Qnil);

  handler = Ffind_file_name_handler (dirname);
  if (!NILP (handler))
    return (call2 (handler, Qdelete_directory, dirname));

  if (rmdir ((char *) XSTRING (dirname)->data) != 0)
    report_file_error (GETTEXT ("Removing directory"), list1 (dirname));

  return Qnil;
}

DEFUN ("delete-file", Fdelete_file, Sdelete_file, 1, 1, "fDelete file: ",
  "Delete specified file.  One argument, a file name string.\n\
If file has multiple names, it continues to exist with the other names.")
  (filename)
     Lisp_Object filename;
{
  Lisp_Object handler;
  CHECK_STRING (filename, 0);
  filename = Fexpand_file_name (filename, Qnil);

  handler = Ffind_file_name_handler (filename);
  if (!NILP (handler))
    return call2 (handler, Qdelete_file, filename);

  if (0 > unlink ((char *) XSTRING (filename)->data))
    report_file_error (GETTEXT ("Removing old name"), list1 (filename));
  return Qnil;
}

DEFUN ("rename-file", Frename_file, Srename_file, 2, 3,
  "fRename file: \nFRename %s to file: \np",
  "Rename FILE as NEWNAME.  Both args strings.\n\
If file has names other than FILE, it continues to have those names.\n\
Signals a `file-already-exists' error if a file NEWNAME already exists\n\
unless optional third argument OK-IF-ALREADY-EXISTS is non-nil.\n\
A number as third arg means request confirmation if NEWNAME already exists.\n\
This is what happens in interactive use with M-x.")
  (filename, newname, ok_if_already_exists)
     Lisp_Object filename, newname, ok_if_already_exists;
{
  Lisp_Object handler;
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (filename, newname);
  CHECK_STRING (filename, 0);
  CHECK_STRING (newname, 1);
  filename = Fexpand_file_name (filename, Qnil);
  newname = Fexpand_file_name (newname, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename);
  if (NILP (handler))
    handler = Ffind_file_name_handler (newname);
  if (!NILP (handler))
  {
    UNGCPRO;
    return call4 (handler, Qrename_file,
		  filename, newname, ok_if_already_exists);
  }

  /* When second argument is a directory, rename the file into it.
     (rename-file "foo" "bar/") == (rename-file "foo" "bar/foo")
   */
  if (!NILP (Ffile_directory_p (newname)))
    {
      Lisp_Object args[3];
      struct gcpro gcpro1;
      int i = 1;

      args[0] = newname; 
      args[1] = Qnil; args[2] = Qnil;
      GCPRO1 (*args); 
      gcpro1.nvars = 3;
      if (XSTRING (newname)->data [XSTRING (newname)->size - 1] != '/')
	args[i++] = build_string ("/");
      args[i++] = Ffile_name_nondirectory (filename);
      newname = Fconcat (i, args);
      UNGCPRO;
    }

  if (NILP (ok_if_already_exists)
      || FIXNUMP (ok_if_already_exists))
    barf_or_query_if_file_exists (newname, GETTEXT ("rename to it"),
				  FIXNUMP (ok_if_already_exists));

#ifndef BSD4_1
  if (0 > rename ((char *) XSTRING (filename)->data,
		  (char *) XSTRING (newname)->data))
#else
  if (0 > link ((char *) XSTRING (filename)->data, XSTRING (newname)->data)
      || 0 > unlink ((char *) XSTRING (filename)->data))
#endif
    {
      if (errno == EXDEV)
	{
	  Fcopy_file (filename, newname,
		      /* We have already prompted if it was an integer,
			 so don't have copy-file prompt again.  */
		      ((NILP (ok_if_already_exists)) ? Qnil : Qt),
                      Qt);
	  Fdelete_file (filename);
	}
      else
	{
	  report_file_error (GETTEXT ("Renaming"), list2 (filename, newname));
	}
    }
  UNGCPRO;
  return Qnil;
}

DEFUN ("add-name-to-file", Fadd_name_to_file, Sadd_name_to_file, 2, 3,
  "fAdd name to file: \nFName to add to %s: \np",
  "Give FILE additional name NEWNAME.  Both args strings.\n\
Signals a `file-already-exists' error if a file NEWNAME already exists\n\
unless optional third argument OK-IF-ALREADY-EXISTS is non-nil.\n\
A number as third arg means request confirmation if NEWNAME already exists.\n\
This is what happens in interactive use with M-x.")
  (filename, newname, ok_if_already_exists)
     Lisp_Object filename, newname, ok_if_already_exists;
{
  Lisp_Object handler;
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (filename, newname);
  CHECK_STRING (filename, 0);
  CHECK_STRING (newname, 1);
  filename = Fexpand_file_name (filename, Qnil);
  newname = Fexpand_file_name (newname, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename);
  if (!NILP (handler))
    {
      UNGCPRO;
      return call3 (handler, Qadd_name_to_file, filename, newname);
    }

  if (NILP (ok_if_already_exists)
      || FIXNUMP (ok_if_already_exists))
    barf_or_query_if_file_exists (newname, GETTEXT ("make it a new name"),
				  FIXNUMP (ok_if_already_exists));
  unlink ((char *) XSTRING (newname)->data);
  if (0 > link ((char *) XSTRING (filename)->data,
		(char *) XSTRING (newname)->data))
    {
      report_file_error (GETTEXT ("Adding new name"),
			 list2 (filename, newname));
    }

  UNGCPRO;
  return Qnil;
}

#ifdef S_IFLNK
DEFUN ("make-symbolic-link", Fmake_symbolic_link, Smake_symbolic_link, 2, 3,
  "FMake symbolic link to file: \nFMake symbolic link to file %s: \np",
  "Make a symbolic link to FILENAME, named LINKNAME.  Both args strings.\n\
Signals a `file-already-exists' error if a file NEWNAME already exists\n\
unless optional third argument OK-IF-ALREADY-EXISTS is non-nil.\n\
A number as third arg means request confirmation if NEWNAME already exists.\n\
This happens for interactive use with M-x.")
  (filename, linkname, ok_if_already_exists)
     Lisp_Object filename, linkname, ok_if_already_exists;
{
  Lisp_Object handler;
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (filename, linkname);
  CHECK_STRING (filename, 0);
  CHECK_STRING (linkname, 1);
#if 0 /* This made it impossible to make a link to a relative name.  */
  filename = Fexpand_file_name (filename, Qnil);
#endif
  linkname = Fexpand_file_name (linkname, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename);
  if (!NILP (handler))
  {
    UNGCPRO;
    return call4 (handler, Qmake_symbolic_link, filename, linkname,
		  ok_if_already_exists);
  }

  if (NILP (ok_if_already_exists)
      || FIXNUMP (ok_if_already_exists))
    barf_or_query_if_file_exists (linkname, GETTEXT ("make it a link"),
				  FIXNUMP (ok_if_already_exists));
  if (0 > symlink ((char *) XSTRING (filename)->data,
		   (char *) XSTRING (linkname)->data))
    {
      /* If we didn't complain already, silently delete existing file.  */
      if (errno == EEXIST)
	{
	  unlink ((char *) XSTRING (linkname)->data);
	  if (0 <= symlink ((char *) XSTRING (filename)->data,
                            (char *) XSTRING (linkname)->data))
	    return Qnil;
	}

      report_file_error (GETTEXT ("Making symbolic link"),
			 list2 (filename, linkname));
    }
  UNGCPRO;
  return Qnil;
}
#endif /* S_IFLNK */

#ifdef VMS

DEFUN ("define-logical-name", Fdefine_logical_name, Sdefine_logical_name,
       2, 2, "sDefine logical name: \nsDefine logical name %s as: ",
  "Define the job-wide logical name NAME to have the value STRING.\n\
If STRING is nil or a null string, the logical name NAME is deleted.")
  (varname, string)
     Lisp_Object varname;
     Lisp_Object string;
{
  CHECK_STRING (varname, 0);
  if (NILP (string))
    delete_logical_name (XSTRING (varname)->data);
  else
    {
      CHECK_STRING (string, 1);

      if (XSTRING (string)->size == 0)
        delete_logical_name (XSTRING (varname)->data);
      else
        define_logical_name (XSTRING (varname)->data, XSTRING (string)->data);
    }

  return string;
}
#endif /* VMS */

#ifdef HPUX_NET

DEFUN ("sysnetunam", Fsysnetunam, Ssysnetunam, 2, 2, 0,
       "Open a network connection to PATH using LOGIN as the login string.")
     (path, login)
     Lisp_Object path, login;
{
  int netresult;
  
  CHECK_STRING (path, 0);
  CHECK_STRING (login, 0);  
  
  netresult = netunam (XSTRING (path)->data, XSTRING (login)->data);

  if (netresult == -1)
    return Qnil;
  else
    return Qt;
}
#endif /* HPUX_NET */

DEFUN ("file-name-absolute-p", Ffile_name_absolute_p, Sfile_name_absolute_p,
       1, 1, 0,
       "Return t if file FILENAME specifies an absolute path name.\n\
On Unix, this is a name starting with a `/' or a `~'.")
     (filename)
     Lisp_Object filename;
{
  unsigned char *ptr;

  CHECK_STRING (filename, 0);
  ptr = XSTRING (filename)->data;
  if (*ptr == '/' || *ptr == '~'
#ifdef VMS
/* ??? This criterion is probably wrong for '<'.  */
      || index (ptr, ':') || index (ptr, '<')
      || (*ptr == '[' && (ptr[1] != '-' || (ptr[2] != '.' && ptr[2] != ']'))
	  && ptr[1] != '.')
#endif /* VMS */
      )
    return Qt;
  else
    return Qnil;
}

DEFUN ("file-exists-p", Ffile_exists_p, Sfile_exists_p, 1, 1, 0,
  "Return t if file FILENAME exists.  (This does not mean you can read it.)\n\
See also `file-readable-p' and `file-attributes'.")
  (filename)
     Lisp_Object filename;
{
  Lisp_Object abspath;
  Lisp_Object handler;

  CHECK_STRING (filename, 0);
  abspath = Fexpand_file_name (filename, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (abspath);
  if (!NILP (handler))
    return call2 (handler, Qfile_exists_p, abspath);

  if (access ((char *) XSTRING (abspath)->data, 0) >= 0)
    return (Qt);
  else
    return (Qnil);
}

DEFUN ("file-executable-p", Ffile_executable_p, Sfile_executable_p, 1, 1, 0,
  "Return t if FILENAME can be executed by you.\n\
For a directory, this means you can access files in that directory.")
  (filename)
    Lisp_Object filename;

{
  Lisp_Object abspath;
  Lisp_Object handler;

  CHECK_STRING (filename, 0);
  abspath = Fexpand_file_name (filename, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (abspath);
  if (!NILP (handler))
    return call2 (handler, Qfile_executable_p, abspath);

  if (access ((char *) XSTRING (abspath)->data, 1) >= 0)
    return (Qt);
  else
    return (Qnil);
}

DEFUN ("file-readable-p", Ffile_readable_p, Sfile_readable_p, 1, 1, 0,
  "Return t if file FILENAME exists and you can read it.\n\
See also `file-exists-p' and `file-attributes'.")
  (filename)
     Lisp_Object filename;
{
  Lisp_Object abspath;
  Lisp_Object handler;

  CHECK_STRING (filename, 0);
  abspath = Fexpand_file_name (filename, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (abspath);
  if (!NILP (handler))
    return call2 (handler, Qfile_readable_p, abspath);

  if (access ((char *) XSTRING (abspath)->data, 4) >= 0)
    return (Qt);
  else
    return (Qnil);
}

DEFUN ("file-symlink-p", Ffile_symlink_p, Sfile_symlink_p, 1, 1, 0,
  "If file FILENAME is the name of a symbolic link\n\
returns the name of the file to which it is linked.\n\
Otherwise returns NIL.")
  (filename)
     Lisp_Object filename;
{
#ifdef S_IFLNK
  char *buf;
  int bufsize;
  int valsize;
  Lisp_Object val;
  Lisp_Object handler;

  CHECK_STRING (filename, 0);
  filename = Fexpand_file_name (filename, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename);
  if (!NILP (handler))
    return call2 (handler, Qfile_symlink_p, filename);

  bufsize = 100;
  while (1)
    {
      buf = (char *) xmalloc (bufsize);
      memset (buf, 0, bufsize);
      valsize = readlink ((char *) XSTRING (filename)->data, buf, bufsize);
      if (valsize < bufsize) break;
      /* Buffer was not long enough */
      xfree (buf);
      bufsize *= 2;
    }
  if (valsize == -1)
    {
      xfree (buf);
      return Qnil;
    }
  val = make_string (buf, valsize);
  xfree (buf);
  return val;
#else /* not S_IFLNK */
  return Qnil;
#endif /* not S_IFLNK */
}

#ifdef SOLARIS_BROKEN_ACCESS
/* In Solaris 2.1, the readonly-ness of the filesystem is not
   considered by the access system call.  This is Sun's bug, but we
   still have to make Emacs work.  */

#include <sys/statvfs.h>

static int
ro_fsys (path)
    char *path;
{
    struct statvfs statvfsb;

    if (statvfs(path, &statvfsb))
      return 1;  /* error from statvfs, be conservative and say not wrtable */
    else
      /* Otherwise, fsys is ro if bit is set.  */
      return statvfsb.f_flag & ST_RDONLY;
}
#else
/* But on every other os, access has already done the right thing.  */
#define ro_fsys(path) 0
#endif

/* Having this before file-symlink-p mysteriously caused it to be forgotten
   on the RT/PC.  */
DEFUN ("file-writable-p", Ffile_writable_p, Sfile_writable_p, 1, 1, 0,
  "Return t if file FILENAME can be written or created by you.")
  (filename)
     Lisp_Object filename;
{
  Lisp_Object abspath, dir;
  Lisp_Object handler;

  CHECK_STRING (filename, 0);
  abspath = Fexpand_file_name (filename, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (abspath);
  if (!NILP (handler))
    return call2 (handler, Qfile_writable_p, abspath);

  if (access ((char *) XSTRING (abspath)->data, 0) >= 0)
    {
      if (access ((char *) XSTRING (abspath)->data, 2) >= 0
          && ! ro_fsys ((char *) XSTRING (abspath)->data))
	return (Qt);
      else
	return (Qnil);
    }

  dir = Ffile_name_directory (abspath);
#ifdef VMS
  if (!NILP (dir))
    dir = Fdirectory_file_name (dir);
#endif /* VMS */
  if (access (!NILP (dir) ? (char *) XSTRING (dir)->data : "", 2) >= 0
      && ! ro_fsys ((char *) XSTRING (dir)->data))
    return (Qt);
  else
    return (Qnil);
}

DEFUN ("file-directory-p", Ffile_directory_p, Sfile_directory_p, 1, 1, 0,
  "Return t if file FILENAME is the name of a directory as a file.\n\
A directory name spec may be given instead; then the value is t\n\
if the directory so specified exists and really is a directory.")
  (filename)
     Lisp_Object filename;
{
  register Lisp_Object abspath;
  struct stat st;
  Lisp_Object handler;

  abspath = expand_and_dir_to_file (filename, current_buffer->directory);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (abspath);
  if (!NILP (handler))
    return call2 (handler, Qfile_directory_p, abspath);

  if (stat ((char *) XSTRING (abspath)->data, &st) < 0)
    return Qnil;
  return (st.st_mode & S_IFMT) == S_IFDIR ? Qt : Qnil;
}

DEFUN ("file-accessible-directory-p",
       Ffile_accessible_directory_p, Sfile_accessible_directory_p, 1, 1, 0,
  "Return t if file FILENAME is the name of a directory as a file,\n\
and files in that directory can be opened by you.  In order to use a\n\
directory as a buffer's current directory, this predicate must return true.\n\
A directory name spec may be given instead; then the value is t\n\
if the directory so specified exists and really is a readable and\n\
searchable directory.")
  (filename)
     Lisp_Object filename;
{
  Lisp_Object handler;
  struct gcpro gcpro1;

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename);
  if (!NILP (handler))
    return call2 (handler, Qfile_accessible_directory_p, filename);

  GCPRO1 (filename);
  if (NILP (Ffile_directory_p (filename)))
    {
      UNGCPRO;
      return (Qnil);
    }
  handler = Ffile_executable_p (filename);
  UNGCPRO;
  return (handler);
}

DEFUN ("file-modes", Ffile_modes, Sfile_modes, 1, 1, 0,
  "Return mode bits of FILE, as an integer.")
  (filename)
     Lisp_Object filename;
{
  Lisp_Object abspath;
  struct stat st;
  Lisp_Object handler;

  abspath = expand_and_dir_to_file (filename, current_buffer->directory);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (abspath);
  if (!NILP (handler))
    return call2 (handler, Qfile_modes, abspath);

  if (stat ((char *) XSTRING (abspath)->data, &st) < 0)
    return Qnil;
  return make_number (st.st_mode & 07777);
}

DEFUN ("set-file-modes", Fset_file_modes, Sset_file_modes, 2, 2, 0,
  "Set mode bits of FILE to MODE (an integer).\n\
Only the 12 low bits of MODE are used.")
  (filename, mode)
     Lisp_Object filename, mode;
{
  Lisp_Object abspath;
  Lisp_Object handler;

  abspath = Fexpand_file_name (filename, current_buffer->directory);
  CHECK_FIXNUM (mode, 1);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (abspath);
  if (!NILP (handler))
    return call3 (handler, Qset_file_modes, abspath, mode);

#ifndef APOLLO
  if (chmod ((char *) XSTRING (abspath)->data, XINT (mode)) < 0)
    report_file_error (GETTEXT ("Doing chmod"), Fcons (abspath, Qnil));
#else /* APOLLO */
  if (!egetenv ("USE_DOMAIN_ACLS"))
    {
      struct stat st;
      struct timeval tvp[2];

      /* chmod on apollo also change the file's modtime; need to save the
	 modtime and then restore it. */
      if (stat (XSTRING (abspath)->data, &st) < 0)
	{
	  report_file_error (GETTEXT ("Doing chmod"), Fcons (abspath, Qnil));
	  return (Qnil);
	}
 
      if (chmod (XSTRING (abspath)->data, XINT (mode)) < 0)
 	report_file_error (GETTEXT ("Doing chmod"), Fcons (abspath, Qnil));
 
      /* reset the old accessed and modified times.  */
      tvp[0].tv_sec = st.st_atime + 1; /* +1 due to an Apollo roundoff bug */
      tvp[0].tv_usec = 0;
      tvp[1].tv_sec = st.st_mtime + 1; /* +1 due to an Apollo roundoff bug */
      tvp[1].tv_usec = 0;
 
      if (utimes (XSTRING (abspath)->data, tvp) < 0)
 	report_file_error (GETTEXT ("Doing utimes"), Fcons (abspath, Qnil));
    }
#endif /* APOLLO */

  return Qnil;
}

DEFUN ("set-default-file-modes", Fset_default_file_modes, Sset_default_file_modes, 1, 1, 0,
    "Set the file permission bits for newly created files.\n\
MASK should be an integer; if a permission's bit in MASK is 1,\n\
subsequently created files will not have that permission enabled.\n\
Only the low 9 bits are used.\n\
This setting is inherited by subprocesses.")
  (mode)
     Lisp_Object mode;
{
  CHECK_FIXNUM (mode, 0);
  
  umask ((~ XINT (mode)) & 0777);

  return Qnil;
}

DEFUN ("default-file-modes", Fdefault_file_modes, Sdefault_file_modes, 0, 0, 0,
    "Return the default file protection for created files.\n\
The umask value determines which permissions are enabled in newly\n\
created files.  If a permission's bit in the umask is 1, subsequently\n\
created files will not have that permission enabled.")
  ()
{
  int mode;

  mode = umask (0);
  umask (mode);

  return make_number ((~ mode) & 0777);
}

#ifndef VMS
DEFUN ("unix-sync", Funix_sync, Sunix_sync, 0, 0, "",
  "Tell Unix to finish all pending disk updates.")
  ()
{
  sync ();
  return Qnil;
}
#endif /* !VMS */


DEFUN ("file-newer-than-file-p", Ffile_newer_than_file_p, Sfile_newer_than_file_p, 2, 2, 0,
  "Return t if file FILE1 is newer than file FILE2.\n\
If FILE1 does not exist, the answer is nil;\n\
otherwise, if FILE2 does not exist, the answer is t.")
  (file1, file2)
     Lisp_Object file1, file2;
{
  Lisp_Object abspath1, abspath2;
  struct stat st;
  int mtime1;
  Lisp_Object handler;
  struct gcpro gcpro1, gcpro2;

  CHECK_STRING (file1, 0);
  CHECK_STRING (file2, 0);

  abspath1 = Qnil;
  GCPRO2 (abspath1, file2);
  abspath1 = expand_and_dir_to_file (file1, current_buffer->directory);
  abspath2 = expand_and_dir_to_file (file2, current_buffer->directory);
  UNGCPRO;

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (abspath1);
  if (NILP (handler))
    handler = Ffind_file_name_handler (abspath2);
  if (!NILP (handler))
    return call3 (handler, Qfile_newer_than_file_p, abspath1, abspath2);

  if (stat ((char *) XSTRING (abspath1)->data, &st) < 0)
    return Qnil;

  mtime1 = st.st_mtime;

  if (stat ((char *) XSTRING (abspath2)->data, &st) < 0)
    return Qt;

  return (mtime1 > st.st_mtime) ? Qt : Qnil;
}


DEFUN ("compute-buffer-file-truename", Fcompute_buffer_file_truename,
       Scompute_buffer_file_truename, 0, 0, 0,
  "Recomputes this buffer's value of `buffer-file-truename'\n\
based on the current value of `buffer-file-name'.")
       ()
{
  Lisp_Object fn, dn;

  if (NILP (current_buffer->filename))
  {
    current_buffer->truename = Qnil;
    return (Qnil);
  }

  fn = Ffile_truename (current_buffer->filename, Qnil);
  if (NILP (fn))
    {
      struct gcpro gcpro1;

      /* If the file name is resolvable, we're done.  Otherwise, the file
	 probably doesn't exist yet.  First, resolve the file's directory...
       */
      dn = Ffile_name_directory (current_buffer->filename);
      GCPRO1 (dn);
      fn = Ffile_truename (dn, Qnil);
      if (! NILP (fn))
        dn = fn;
      
      /* ...and then expand the file relative to that. */
      fn = Fexpand_file_name
	     (Ffile_name_nondirectory (current_buffer->filename), dn);
      UNGCPRO;
    }
  current_buffer->truename = fn;

  if (find_file_use_truenames)
    {
      struct gcpro gcpro1;
      GCPRO1 (fn);
      current_buffer->filename = fn;
      current_buffer->directory = Ffile_name_directory (fn);
      UNGCPRO;
    }
  return fn;
}



DEFUN ("insert-file-contents", Finsert_file_contents, Sinsert_file_contents,
  1, 4, 0,
  "Insert contents of file FILENAME after point.\n\
Returns list of absolute file name and length of data inserted.\n\
If second argument VISIT is non-nil, the buffer's visited filename\n\
and last save file modtime are set, and it is marked unmodified.\n\
If visiting and the file does not exist, visiting is completed\n\
before the error is signaled.\n\n\
The optional third and fourth arguments BEG and END\n\
specify what portion of the file to insert.\n\
If VISIT is non-nil, BEG and END must be nil.")
  (filename, visit, beg, end)
     Lisp_Object filename, visit, beg, end;
{
  struct stat st;
  register int fd;
  register int inserted = 0;
  register int how_much;
  int speccount = specpdl_depth ();
  struct gcpro gcpro1, gcpro2;
  Lisp_Object handler, val;
  int total;

  val = Qnil;

  if (!NILP (beg) || !NILP (end))
    if (!NILP (visit))
      error (GETTEXT ("Attempt to visit less than an entire file"));

  if (!NILP (beg))
    CHECK_FIXNUM (beg, 0);
  else
    beg = Qzero;

  if (!NILP (end))
    CHECK_FIXNUM (end, 0);

  GCPRO2 (filename, visit);
  if (!NILP (current_buffer->read_only))
    Fbarf_if_buffer_read_only ();

  filename = Fexpand_file_name (filename, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename);
  if (!NILP (handler))
    {
      val = call5 (handler, Qinsert_file_contents, filename, visit, beg, end);
      st.st_mtime = 0;
      goto handled;
    }

  fd = -1;

#ifndef APOLLO
  if (stat ((char *) XSTRING (filename)->data, &st) < 0
      || (fd = emacs_open ((char *) XSTRING (filename)->data, 0, 0)) < 0)
#else /* APOLLO */
  if ((fd = emacs_open ((char *) XSTRING (filename)->data, 0)) < 0
      || fstat (fd, &st) < 0)
#endif /* APOLLO */
    {
      if (fd >= 0) emacs_close (fd);
      if (NILP (visit))
	report_file_error (GETTEXT ("Opening input file"),
			   Fcons (filename, Qnil));
      st.st_mtime = -1;
      how_much = 0;
      goto notfound;
    }

  record_unwind_protect (close_file_unwind, make_number (fd));

#ifdef S_IFSOCK
  /* This code will need to be changed in order to work on named
     pipes, and it's probably just not worth it.  So we should at
     least signal an error.  */
  if ((st.st_mode & S_IFMT) == S_IFSOCK)
    return Fsignal (Qfile_error,
                    list2 (build_string (GETTEXT ("reading from named pipe")),
                           filename));
#endif

  /* Supposedly happens on VMS.  */
  if (st.st_size < 0)
    error (GETTEXT ("File size is negative"));

  if (NILP (end))
    {
      XSETINT (end, st.st_size);
      if (XINT (end) != st.st_size)
	error (GETTEXT ("maximum buffer size exceeded"));
    }

  total = XINT (end) - XINT (beg);

  {
    Lisp_Object temp;

    /* Make sure point-max won't overflow after this insertion.  */
    XSET (temp, Lisp_Int, total);
    if (total != XINT (temp))
      error (GETTEXT ("maximum buffer size exceeded"));
  }

  if (NILP (visit) && total > 0)
    prepare_to_modify_buffer (PT, PT);

  move_gap (current_buffer, PT);
  if (GAP_SIZE < total)
    make_gap (total - GAP_SIZE);

  /* #### Probably beg should be interpreted in wide-character positions when
     I18N4 is on -- this will seek into the middle of a multi-byte char.
   */
  if (XINT (beg) != 0)
    {
      if (lseek (fd, XINT (beg), 0) < 0)
	report_file_error (GETTEXT ("Setting file position"),
			   Fcons (filename, Qnil));
    }

#ifdef I18N4
  {
    register FILE *fp = fdopen (fd, "r");
    register int wc; /* NOT wchar_t! */
    register wchar_t *p = CHAR_ADDRESS (PT + inserted - 1) + 1;
    while ((wc = fgetwc (fp)) != EOF)
      {
	*p++ = (wchar_t) wc;
	GPT++;
	GAP_SIZE--;
	ZV++;
	Z++;
	inserted++;
      }
    how_much = ferror (fp) ? -1 : 0;
    fclose (fp);
  }
#else /* not I18N4 */
  while (1)
    {
      int try = min (total - inserted, 64 << 10);
      int this_len;

      /* Allow quitting out of the actual I/O.  */
      immediate_quit = 1;
      QUIT;
      this_len = emacs_read (fd, (char *) CHAR_ADDRESS (PT + inserted - 1) + 1,
			     try);
      immediate_quit = 0;

      if (this_len <= 0)
	{
	  how_much = this_len;
	  break;
	}

      GPT += this_len;
      GAP_SIZE -= this_len;
      ZV += this_len;
      Z += this_len;
      inserted += this_len;
    }
#endif /* I18N4 */

  if (inserted > 0)
    {
      record_insert (PT, inserted);
      MODIFF++;
    }

  /* Close the file */
  unbind_to (speccount, Qnil);

  if (how_much < 0)
    error (GETTEXT ("IO error reading %s: %s"),
	   XSTRING (filename)->data, err_str (errno));

 notfound:
 handled:

  if (!NILP (visit))
    {
      current_buffer->undo_list = Qnil;
#ifdef APOLLO
      stat (XSTRING (filename)->data, &st);
#endif
      current_buffer->modtime = st.st_mtime;
      current_buffer->save_modified = MODIFF;
      current_buffer->auto_save_modified = MODIFF;
      current_buffer->save_length = make_number (Z - BEG);
#ifdef CLASH_DETECTION
      if (NILP (handler))
	{
	  if (!NILP (current_buffer->filename))
	    unlock_file (current_buffer->filename);
	  unlock_file (filename);
	}
#endif /* CLASH_DETECTION */
      current_buffer->filename = filename;
      Fcompute_buffer_file_truename ();
      /* If visiting nonexistent file, return nil.  */
      if (current_buffer->modtime == -1)
	report_file_error (GETTEXT ("Opening input file"),
			   list1 (filename));
    }

  /* NO!!!!! if (NILP (visit) && total > 0) */
  signal_after_change (PT, 0, inserted);
  
  if (inserted > 0)
    {
      Lisp_Object p = Vafter_insert_file_functions;
      struct gcpro gcpro1;

      GCPRO1 (p);
      while (!NILP (p))
	{
	  Lisp_Object insval = call1 (Fcar (p), make_number (inserted));
	  if (!NILP (insval))
	    {
	      CHECK_NATNUM (insval, 0);
	      inserted = XFASTINT (insval);
	    }
	  QUIT;
	  p = Fcdr (p);
	}
      UNGCPRO;
    }

  UNGCPRO;
  if (!NILP (val))
    return (val);
  else
    return (list2 (filename, make_number (inserted)));
}


static int a_write (int desc,
#ifdef I18N4
		    CONST wchar_t *buf,
#else
		    CONST unsigned char *buf,
#endif
		    int len, int pos,
                    Lisp_Object *annot);
static Lisp_Object build_annotations (Lisp_Object start, Lisp_Object end);

DEFUN ("write-region", Fwrite_region, Swrite_region, 3, 5,
  "r\nFWrite region to file: ",
  "Write current region into specified file.\n\
When called from a program, takes three arguments:\n\
START, END and FILENAME.  START and END are buffer positions.\n\
Optional fourth argument APPEND if non-nil means\n\
  append to existing file contents (if any).\n\
Optional fifth argument VISIT if t means\n\
  set the last-save-file-modtime of buffer to this file's modtime\n\
  and mark buffer not modified.\n\
If VISIT is a string, it is a second file name;\n\
  the output goes to FILENAME, but the buffer is marked as visiting VISIT.\n\
  VISIT is also the file name to lock and unlock for clash detection.\n\
If VISIT is neither t nor nil nor a string,\n\
  that means do not print the \"Wrote file\" message.\n\
Kludgy feature: if START is a string, then that string is written\n\
to the file, instead of any buffer contents, and END is ignored.")
  (start, end, filename, append, visit)
     Lisp_Object start, end, filename, append, visit;
{
  register int desc;
  int failure;
  int save_errno = 0;
  struct stat st;
  Lisp_Object fn;
#ifdef VMS
  Lisp_Object fname = Qnil;  /* If non-nil, original filename (must rename) */
#endif /* VMS */
  int visiting_other = STRINGP (visit);
  int visiting = (EQ (visit, Qt) || visiting_other);
  int quietly = (!visiting && !NILP (visit));
  Lisp_Object visit_file = Qnil;
  Lisp_Object annotations = Qnil;

  /* Special kludge to simplify auto-saving */
  if (NILP (start))
    {
      start = make_number (BEG);
      end = make_number (Z);
    }
  else if (!STRINGP (start))
    validate_region (&start, &end);

  {
    Lisp_Object handler;
    struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
    GCPRO4 (start, filename, visit, visit_file);

    if (visiting_other)
      visit_file = Fexpand_file_name (visit, Qnil);
    else
      visit_file = filename;
    filename = Fexpand_file_name (filename, Qnil);

    UNGCPRO;

    /* If the file name has special constructs in it,
       call the corresponding file handler.  */
    handler = Ffind_file_name_handler (filename);

    if (!NILP (handler))
      {
        Lisp_Object val = call6 (handler, Qwrite_region, start, end, 
                                 filename, append, visit);
	/* Do this before reporting IO error
	   to avoid a "file has changed on disk" warning on
	   next attempt to save.  */
	if (visiting)
	  {
	    current_buffer->modtime = 0;
	    current_buffer->save_modified = MODIFF;
	    current_buffer->save_length = make_number (Z - BEG);
	    current_buffer->filename = visit_file;
	  }
	return val;
      }
  }

#ifdef CLASH_DETECTION
  if (!auto_saving)
    {
      struct gcpro gcpro1, gcpro2, gcpro3;
      GCPRO3 (start, filename, visit_file);
      lock_file (visit_file);
      UNGCPRO;
    }
#endif /* CLASH_DETECTION */

  annotations = build_annotations (start, end);

  fn = filename;
  desc = -1;
  if (!NILP (append))
    desc = emacs_open ((char *) XSTRING (fn)->data, O_WRONLY, 0);

  if (desc < 0)
#ifndef VMS
    {
      desc = creat ((char *) XSTRING (fn)->data,
		    ((auto_saving) ? auto_save_mode_bits : 0666));
    }
#else /* not VMS */
  {
    if (auto_saving)	/* Overwrite any previous version of autosave file */
      {
	unsigned char *fn_data = XSTRING (fn)->data;
	/* if fn exists, truncate to zero length */
	vms_truncate (fn_data);
	desc = emacs_open (fn_data, O_RDWR);
	if (desc < 0)
	  desc = creat_copy_attrs ((STRINGP (current_buffer->filename)
				    ? XSTRING (current_buffer->filename)->data
				    : 0),
				   fn_data);
      }
    else		/* Write to temporary name and rename if no errors */
      {
	Lisp_Object temp_name;

	struct gcpro gcpro1, gcpro2, gcpro3;
	GCPRO3 (start, filename, visit_file);
	{
	  struct gcpro gcpro1, gcpro2, gcpro3; /* Don't have GCPRO6 */

	  GCPRO3 (fn, fname, annotations);

	  temp_name = Ffile_name_directory (filename);

	  if (NILP (temp_name))
	    desc = creat ((char *) XSTRING (fn)->data, 0666);
	  else
	    {
	      temp_name = Fmake_temp_name (concat2 (temp_name,
						    build_string("$$SAVE$$")));
	      fname = filename;
	      fn = temp_name;
	      desc = creat_copy_attrs (fname, XSTRING (fn)->data);
	      if (desc < 0)
		{
		  unsigned char *fn_data;
		  /* If we can't open the temporary file, try creating a new
		     version of the original file.  VMS "creat" creates a
		     new version rather than truncating an existing file. */
		  fn = fname;
		  fname = Qnil;
		  fn_data = XSTRING (fn)->data;
		  desc = creat (fn_data, 0666);
#if 0                           /* This can clobber an existing file and fail
				   to replace it, if the user runs out of
				   space.  */
		  if (desc < 0)
		    {
		      /* We can't make a new version;
			 try to truncate and rewrite existing version if any.
		       */
		      vms_truncate (fn_data);
		      desc = emacs_open (fn_data, O_RDWR);
		    }
#endif
		}
	    }
	  UNGCPRO;
	}
	UNGCPRO;
      }
  }
#endif /* VMS */

  if (desc < 0)
    {
#ifdef CLASH_DETECTION
      save_errno = errno;
      if (!auto_saving) unlock_file (visit_file);
      errno = save_errno;
#endif /* CLASH_DETECTION */
      report_file_error (GETTEXT ("Opening output file"),
			 Fcons (filename, Qnil));
    }

  {
    int speccount = specpdl_depth ();
    Lisp_Object desc_locative = Fcons (make_number (desc), Qnil);

    record_unwind_protect (close_file_unwind, desc_locative);

    if (!NILP (append))
      {
	if (lseek (desc, 0, 2) < 0)
	  {
#ifdef CLASH_DETECTION
	    if (!auto_saving) unlock_file (visit_file);
#endif /* CLASH_DETECTION */
	    report_file_error (GETTEXT ("Lseek error"),
			       list1 (filename));
	  }
      }

#ifdef VMS
/*
 * Kludge Warning: The VMS C RTL likes to insert carriage returns
 * if we do writes that don't end with a carriage return. Furthermore
 * it cannot handle writes of more then 16K. The modified
 * version of "sys_write" in SYSDEP.C (see comment there) copes with
 * this EXCEPT for the last record (iff it doesn't end with a carriage
 * return). This implies that if your buffer doesn't end with a carriage
 * return, you get one free... tough. However it also means that if
 * we make two calls to sys_write (a la the following code) you can
 * get one at the gap as well. The easiest way to fix this (honest)
 * is to move the gap to the next newline (or the end of the buffer).
 * Thus this change.
 *
 * Yech!
 */
    if (GPT > BEG && *GPT_ADDR[-1] != '\n')
      move_gap (find_next_newline (current_buffer, GPT, 1));
#endif

    failure = 0;
    immediate_quit = 1;

    if (STRINGP (start))
      {
#ifdef I18N4
	/* #### The following presumes that strings contain wide chars,
	   which currently they don't.  Fix this somehow. */
	  abort ();
#else
	failure = (0 > (a_write (desc, 
                                 XSTRING (start)->data,
                                 string_length (XSTRING (start)),
                                 0,
                                 &annotations)));
	save_errno = errno;
#endif
      }
    else if (XINT (start) != XINT (end))
      {
        int nwritten = 0;
        int start1 = XINT (start);
        int end1 = XINT (end);
	if (start1 < GPT)
	  {
            int len = min (GPT, end1) - start1;
	    failure = (0 > (a_write (desc,
                                     CHAR_ADDRESS (start1),
                                     len,
                                     start1, &annotations)));
            nwritten += len;
	    save_errno = errno;
	  }

	if (end1 > GPT && !failure)
	  {
	    int start2 = max (start1, GPT);
            int len = end1 - start2;
	    failure = (0 > (a_write (desc,
                                     CHAR_ADDRESS (start2),
                                     len,
                                     start2, &annotations)));
            nwritten += len;
	    save_errno = errno;
	  }

      if (nwritten == 0)
	{
	  /* If file was empty, still need to write the annotations */
	  failure = (0 > (a_write (desc, CHAR_ADDRESS (BEG), 0,
				   start1, &annotations)));
	  save_errno = errno;
	}
      }

    immediate_quit = 0;

#ifdef HAVE_FSYNC
    /* Note fsync appears to change the modtime on BSD4.2 (both vax and sun).
       Disk full in NFS may be reported here.  */
    /* mib says that closing the file will try to write as fast as NFS can do
       it, and that means the fsync here is not crucial for autosave files.  */
    if (!auto_saving && fsync (desc) < 0)
      {
	failure = 1;
	save_errno = errno;
      }
#endif

    /* Spurious "file has changed on disk" warnings have been 
       observed on Suns as well.
       It seems that `close' can change the modtime, under nfs.

       (This has supposedly been fixed in Sunos 4,
       but who knows about all the other machines with NFS?)  */
#if 0

    /* On VMS and APOLLO, must do the stat after the close
       since closing changes the modtime.  */
#ifndef VMS
#ifndef APOLLO
    /* Recall that #if defined does not work on VMS.  */
#define fileio_FOO
    fstat (desc, &st);
#endif
#endif
#endif

    /* NFS can report a write failure now.  */
    if (emacs_close (desc) < 0)
      {
	failure = 1;
	save_errno = errno;
      }

    /* Discard the unwind-protect */
    XCONS (desc_locative)->car = Qnil;
    unbind_to (speccount, Qnil);
  }
    

#ifdef VMS
  /* If we wrote to a temporary name and had no errors, rename to real name. */
  if (!NILP (fname))
    {
      if (!failure)
	{
	  failure = (rename ((char *) XSTRING (fn->data), 
			     (char *) XSTRING (fname->data))
		     != 0);
	  save_errno = errno;
	}
      fn = fname;
    }
#endif /* VMS */

#ifndef fileio_FOO
  stat ((char *) XSTRING (fn)->data, &st);
#endif

#ifdef CLASH_DETECTION
  if (!auto_saving)
    unlock_file (visit_file);
#endif /* CLASH_DETECTION */

  /* Do this before reporting IO error
     to avoid a "file has changed on disk" warning on
     next attempt to save.  */
  if (visiting)
    current_buffer->modtime = st.st_mtime;

  if (failure)
    error (GETTEXT ("IO error writing %s: %s"), 
           XSTRING (fn)->data, 
           err_str (save_errno));

  if (visiting)
    {
      current_buffer->save_modified = MODIFF;
      current_buffer->save_length = make_number (Z - BEG);
      current_buffer->filename = visit_file;
    }
  else if (quietly)
    {
      return Qnil;
    }

  if (!auto_saving)
    {
      if (visiting_other)
        message (GETTEXT ("Wrote %s"), XSTRING (visit_file)->data);
      else
	{
	  struct gcpro gcpro1;
	  Lisp_Object fsp;
	  GCPRO1 (fn);

	  fsp = Ffile_symlink_p (fn);
	  if (NILP (fsp))
	    message (GETTEXT ("Wrote %s"), XSTRING (fn)->data);
	  else
	    message (GETTEXT ("Wrote %s (symlink to %s)"), 
		     XSTRING (fn)->data, XSTRING (fsp)->data);
	  UNGCPRO;
	}
    }
  return Qnil;
}

#ifdef I18N4
/* wc_write is a replacement for emacs_write which uses fputwc() to
   convert wide characters to multibyte characters.
*/
static int
wc_write (int desc, register wchar_t *addr, register int len)
{
  FILE *fp = fdopen (desc, "r+");
  register wchar_t wc;
  register int nbytes = 0;

  while (len-- > 0) {
    wc = fputwc (*addr++, fp);
    if (wc == EOF)
      break;
    nbytes++;
  }
  fflush (fp);
  return nbytes;
}
#endif

/* Write LEN bytes starting at ADDR to FD.
   For the I18N4 version, it is LEN wide characters, not LEN bytes.
   Returns 0 for success, something else for failure.
   If it fails, e_write_error is set to the error number.
 */
static int
e_write (int desc,
#ifdef I18N4
	 CONST wchar_t *addr,
#else
	 CONST unsigned char *addr,
#endif
	 int len)
{
#ifdef I18N4
  wchar_t buf[16 * 1024];
  register wchar_t *p, *end;
#else
  char buf[16 * 1024];
  register char *p, *end;
#endif

  if (!EQ (current_buffer->selective_display, Qt))
#ifdef I18N4
    return wc_write (desc, (wchar_t *) addr, len) - len;
#else
    return emacs_write (desc, (char *) addr, len) - len;
#endif
  else
    {
      /* Urk! */
      p = buf;
      end = p + sizeof (buf);
      while (len--)
	{
	  if (p == end)
	    {
#ifdef I18N4
	      if (wc_write (desc, buf, sizeof (buf)) != sizeof (buf))
#else
	      if (emacs_write (desc, buf, sizeof (buf)) != sizeof (buf))
#endif
		return -1;
	      p = buf;
	    }
	  *p = *addr++;
	  if (*p++ == '\015')
	    p[-1] = '\n';
	}
      if (p != buf)
	{
#ifdef I18N4
	  if (wc_write (desc, buf, p - buf) != p - buf)
#else
	  if (emacs_write (desc, buf, p - buf) != p - buf)
#endif
	    return -1;
	}
    }
  return 0;
}

/* ### This is such a load of shit!!!!  There is no way we should define
   something so stupid as a subr, just sort the fucking list more
   intelligently. */
DEFUN ("car-less-than-car", Fcar_less_than_car, Scar_less_than_car, 2, 2, 0,
  "Return t if (car A) is numerically less than (car B).")
  (a, b)
     Lisp_Object a, b;
{
  return Flss (Fcar (a), Fcar (b));
}

/* Build the complete list of annotations appropriate for writing out
   the text between START and END, by calling all the functions in
   write-region-annotate-functions and merging the lists they return.  */

static Lisp_Object
build_annotations (Lisp_Object start, Lisp_Object end)
{
  Lisp_Object annotations;
  Lisp_Object p, res;
  struct gcpro gcpro1, gcpro2;

  annotations = Qnil;
  p = Vwrite_region_annotate_functions;
  GCPRO2 (annotations, p);
  while (!NILP (p))
    {
      res = call2 (Fcar (p), start, end);
      (void) Flength (res);     /* Check basic validity of return value */
      annotations = merge (annotations, res, Qcar_less_than_car);
      p = Fcdr (p);
    }
  UNGCPRO;
  return annotations;
}

/* Write to descriptor DESC the LEN characters starting at ADDR,
   assuming they start at position POS in the buffer.
   Intersperse with them the annotations from *ANNOT
   (those which fall within the range of positions POS to POS + LEN),
   each at its appropriate position.

   Modify *ANNOT by discarding elements as we output them.
   The return value is negative in case of system call failure.  */

/* #### does this need I18N4 changes?? */
static int
a_write (int desc,
#ifdef I18N4
	 CONST wchar_t *buf,
#else
	 CONST unsigned char *buf,
#endif
	 int len, int pos, Lisp_Object *annot)
{
  Lisp_Object tem;
#ifdef I18N4
  CONST wchar_t *addr = buf;
#else
  CONST unsigned char *addr = buf;
#endif
  int nextpos;
  int lastpos = pos + len;

  for (;;)
    {
      tem = Fcar_safe (Fcar (*annot));
      if (FIXNUMP (tem) && XINT (tem) >= pos && XFASTINT (tem) <= lastpos)
	nextpos = XFASTINT (tem);
      else
	return e_write (desc, addr, lastpos - pos);
      if (nextpos > pos)
	{
	  if (0 > e_write (desc, addr, nextpos - pos))
	    return -1;
	  addr += nextpos - pos;
	  pos = nextpos;
	}
      tem = Fcdr (Fcar (*annot));
      if (STRINGP (tem))
	{
#ifdef I18N4
	/* #### The following presumes that strings contain wide chars,
	   which currently they don't.  Fix this somehow. */
	  abort ();
#else
	  if (0 > e_write (desc, XSTRING (tem)->data, XSTRING (tem)->size))
	    return -1;
#endif
	}
      *annot = Fcdr (*annot);
    }
}



#if 0
#include <des_crypt.h>

#define CRYPT_BLOCK_SIZE 8	/* bytes */
#define CRYPT_KEY_SIZE 8	/* bytes */

DEFUN ("encrypt-string", Fencrypt_string, Sencrypt_string, 2, 2, 0,
"Encrypt STRING using KEY.")
  (string, key)
     Lisp_Object string, key;
{
  char *encrypted_string, *raw_key;
  int rounded_size, extra, key_size;

  CHECK_STRING (string, 0);
  CHECK_STRING (key, 1);

  extra = XSTRING (string)->size % CRYPT_BLOCK_SIZE;
  rounded_size = XSTRING (string)->size + extra;
  encrypted_string = alloca (rounded_size + 1);
  memcpy (encrypted_string, XSTRING (string)->data, XSTRING (string)->size);
  memset (encrypted_string + rounded_size - extra, 0, extra + 1);

  if (XSTRING (key)->size > CRYPT_KEY_SIZE)
    key_size = CRYPT_KEY_SIZE;
  else
    key_size = XSTRING (key)->size;

  raw_key = alloca (CRYPT_KEY_SIZE + 1);
  memcpy (raw_key, XSTRING (key)->data, key_size);
  memset (raw_key + key_size, 0, (CRYPT_KEY_SIZE + 1) - key_size);

  (void) ecb_crypt (raw_key, encrypted_string, rounded_size,
		    DES_ENCRYPT | DES_SW);
  return make_string (encrypted_string, rounded_size);
}

DEFUN ("decrypt-string", Fdecrypt_string, Sdecrypt_string, 2, 2, 0,
"Decrypt STRING using KEY.")
  (string, key)
     Lisp_Object string, key;
{
  char *decrypted_string, *raw_key;
  int string_size, key_size;

  CHECK_STRING (string, 0);
  CHECK_STRING (key, 1);

  string_size = XSTRING (string)->size + 1;
  decrypted_string = alloca (string_size);
  memcpy (decrypted_string, XSTRING (string)->data, string_size);
  decrypted_string[string_size - 1] = '\0';

  if (XSTRING (key)->size > CRYPT_KEY_SIZE)
    key_size = CRYPT_KEY_SIZE;
  else
    key_size = XSTRING (key)->size;

  raw_key = alloca (CRYPT_KEY_SIZE + 1);
  memcpy (raw_key, XSTRING (key)->data, key_size);
  memset (raw_key + key_size, 0, (CRYPT_KEY_SIZE + 1) - key_size);


  (void) ecb_crypt (raw_key, decrypted_string, string_size,
		    DES_DECRYPT | DES_SW);
  return make_string (decrypted_string, string_size - 1);
}
#endif


DEFUN ("verify-visited-file-modtime", Fverify_visited_file_modtime,
  Sverify_visited_file_modtime, 1, 1, 0,
  "Return t if last mod time of BUF's visited file matches what BUF records.\n\
This means that the file has not been changed since it was visited or saved.")
  (buf)
     Lisp_Object buf;
{
  struct buffer *b;
  struct stat st;
  Lisp_Object handler;

  CHECK_BUFFER (buf, 0);
  b = XBUFFER (buf);

  if (!STRINGP (b->filename)) return Qt;
  if (b->modtime == 0) return Qt;

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (b->filename);
  if (!NILP (handler))
    return call2 (handler, Qverify_visited_file_modtime, buf);

  if (stat ((char *) XSTRING (b->filename)->data, &st) < 0)
    {
      /* If the file doesn't exist now and didn't exist before,
	 we say that it isn't modified, provided the error is a tame one.  */
      if (errno == ENOENT || errno == EACCES || errno == ENOTDIR)
	st.st_mtime = -1;
      else
	st.st_mtime = 0;
    }
  if (st.st_mtime == b->modtime
      /* If both are positive, accept them if they are off by one second.  */
      || (st.st_mtime > 0 && b->modtime > 0
	  && (st.st_mtime == b->modtime + 1
	      || st.st_mtime == b->modtime - 1)))
    return Qt;
  return Qnil;
}

DEFUN ("clear-visited-file-modtime", Fclear_visited_file_modtime,
  Sclear_visited_file_modtime, 0, 0, 0,
  "Clear out records of last mod time of visited file.\n\
Next attempt to save will certainly not complain of a discrepancy.")
  ()
{
  current_buffer->modtime = 0;
  return Qnil;
}

DEFUN ("visited-file-modtime", Fvisited_file_modtime,
  Svisited_file_modtime, 0, 0, 0,
  "Return the current buffer's recorded visited file modification time.\n\
The value is a list of the form (HIGH . LOW), like the time values\n\
that `file-attributes' returns.")
  ()
{
  return long_to_cons (current_buffer->modtime);
}

DEFUN ("set-visited-file-modtime", Fset_visited_file_modtime,
  Sset_visited_file_modtime, 0, 1, 0,
  "Update buffer's recorded modification time from the visited file's time.\n\
Useful if the buffer was not read from the file normally\n\
or if the file itself has been changed for some known benign reason.\n\
An argument specifies the modification time value to use\n\
(instead of that of the visited file), in the form of a list\n\
(HIGH . LOW) or (HIGH LOW).")
  (time_list)
     Lisp_Object time_list;
{
  if (!NILP (time_list))
    current_buffer->modtime = cons_to_long (time_list);
  else
    {
      register Lisp_Object filename;
      struct stat st;
      Lisp_Object handler;

      filename = Fexpand_file_name (current_buffer->filename, Qnil);

      /* If the file name has special constructs in it,
	 call the corresponding file handler.  */
      handler = Ffind_file_name_handler (filename);
      if (!NILP (handler))
	/* The handler can find the file name the same way we did.  */
	return call2 (handler, Qset_visited_file_modtime, Qnil);
      else if (stat ((char *) XSTRING (filename)->data, &st) >= 0)
	current_buffer->modtime = st.st_mtime;
    }

  return Qnil;
}

DEFUN ("set-buffer-modtime", Fset_buffer_modtime,
  Sset_buffer_modtime, 1, 2, 0,
  "Update BUFFER's recorded modification time from the associated \n\
file's modtime, if there is an associated file. If not, use the \n\
current time. In either case, if the optional arg TIME is supplied, use \n\
that is it is either an integer or a cons of two integers.")
  (buf, in_time)
  Lisp_Object buf, in_time;
{
  unsigned long time_to_use = 0;
  int set_time_to_use = 0;
  struct stat st;

  CHECK_BUFFER (buf, 0);

  if (!NILP (in_time))
    {
      if (FIXNUMP (in_time))
        {
          CHECK_FIXNUM (in_time, 1);
          time_to_use = XINT (in_time);
          set_time_to_use = 1;
        }
      else if ((CONSP (in_time)) &&
               (FIXNUMP (Fcar (in_time))) && 
               (FIXNUMP (Fcdr (in_time))))
        {
          time_to_use = lisp_to_word (in_time);
          set_time_to_use = 1;
        }
    }

  if (!set_time_to_use)
    {
      Lisp_Object filename = Qnil;
      struct gcpro gcpro1, gcpro2;
      GCPRO2 (buf, filename);

      if (STRINGP (XBUFFER (buf)->filename))
        filename = Fexpand_file_name (XBUFFER (buf)->filename, Qnil);
      else
        filename = Qnil;
  
      UNGCPRO;

      if (!NILP (filename) && !NILP (Ffile_exists_p (filename)))
        {
          if (stat ((char *) XSTRING (filename)->data, &st) >= 0)
            time_to_use = st.st_mtime;
          else
            time_to_use = time ((time_t *) 0);
        }
      else
	time_to_use = time ((time_t *) 0);
    }

  XBUFFER (buf)->modtime = time_to_use;

  return Qnil;
}


static Lisp_Object
auto_save_error (Lisp_Object condition_object, Lisp_Object ignored)
{
  unsigned char *name = XSTRING (current_buffer->name)->data;

  ring_bell (intern("auto-save-error"));  /* Lucid sound change */
  message (GETTEXT ("Autosaving...error for %s"), name);
  Fsleep_for (make_number (1));
  message (GETTEXT ("Autosaving...error!for %s"), name);
  Fsleep_for (make_number (1));
  message (GETTEXT ("Autosaving...error for %s"), name);
  Fsleep_for (make_number (1));
  return Qnil;
}

static Lisp_Object
auto_save_1 (Lisp_Object ignored)
{
  struct stat st;
  Lisp_Object fn = current_buffer->filename;
  Lisp_Object a = current_buffer->auto_save_file_name;

  if (!STRINGP (a))
    return (Qnil);

  /* Get visited file's mode to become the auto save file's mode.  */
  if (STRINGP (fn) &&
      stat ((char *) XSTRING (fn)->data, &st) >= 0)
    /* But make sure we can overwrite it later!  */
    auto_save_mode_bits = st.st_mode | 0600;
  else
    /* default mode for auto-save files of buffers with no file is
       readable by owner only.  This may annoy some small number of
       people, but the alternative removes all privacy from email. */
    auto_save_mode_bits = 0600;

  return
    Fwrite_region (Qnil, Qnil, a, Qnil, Qlambda);
}


/* Fdo_auto_save() checks whether a GC is in progress when it is called,
   and if so, tries to avoid touching lisp objects.

   The only time that Fdo_auto_save() is called while GC is in progress
   is if we're going down, as a result of an abort() or a kill signal.
   It's fairly important that we generate autosave files in that case!
 */

DEFUN ("do-auto-save", Fdo_auto_save, Sdo_auto_save, 0, 2, "",
  "Auto-save all buffers that need it.\n\
This is all buffers that have auto-saving enabled\n\
and are changed since last auto-saved.\n\
Auto-saving writes the buffer into a file\n\
so that your editing is not lost if the system crashes.\n\
This file is not the file you visited; that changes only when you save.\n\n\
Non-nil first argument means do not print any message if successful.\n\
Non-nil second argument means save only current buffer.")
  (no_message, current_only)
     Lisp_Object no_message, current_only;
{
  struct buffer *old = current_buffer, *b;
  Lisp_Object tail, buf;
  int auto_saved = 0;
  int do_handled_files;
  char *omessage = echo_area_glyphs;
  Lisp_Object oquit;

  /* Ordinarily don't quit within this function,
     but don't make it impossible to quit (in case we get hung in I/O).  */
  oquit = Vquit_flag;
  Vquit_flag = Qnil;

  /* No GCPRO needed, because (when it matters) all Lisp_Object variables
     point to non-strings reached from Vbuffer_alist.  */

  auto_saving = 1;
  if (minibuf_level != 0 || gc_in_progress)
    no_message = Qt;

  /* Vrun_hooks is nil before emacs is dumped, and inc-vers.el will
     eventually call do-auto-save, so don't err here in that case. */
  if (!NILP (Vrun_hooks) && !gc_in_progress)
    call1 (Vrun_hooks, Qauto_save_hook);

  /* First, save all files which don't have handlers.  If Emacs is
     crashing, the handlers may tweak what is causing Emacs to crash
     in the first place, and it would be a shame if Emacs failed to
     autosave perfectly ordinary files because it couldn't handle some
     ange-ftp'd file.  */
  for (do_handled_files = 0; do_handled_files < 2; do_handled_files++)
  {
    for (tail = Vbuffer_alist;
         XGCTYPE (tail) == Lisp_Cons;
         tail = XCONS (tail)->cdr)
    {
      buf = XCONS (XCONS (tail)->car)->cdr;
      b = XBUFFER (buf);

      if (!NILP (current_only)
          && b != current_buffer)
        continue;
      
      /* Check for auto save enabled
         and file changed since last auto save
         and file changed since last real save.  */
      if (XGCTYPE (b->auto_save_file_name) == Lisp_String
	  && b->save_modified < BUF_MODIFF (b)
	  && b->auto_save_modified < BUF_MODIFF (b)
          && (do_handled_files
              || NILP (Ffind_file_name_handler (b->auto_save_file_name))))
      {
        if (!gc_in_progress &&
            (XINT (b->save_length) * 10
             > (BUF_Z (b) - BUF_BEG (b)) * 13)
            /* A short file is likely to change a large fraction;
               spare the user annoying messages.  */
            && XINT (b->save_length) > 5000
            /* These messages are frequent and annoying for `*mail*'.  */
            && !EQ (b->filename, Qnil)
            && NILP (no_message))
        {
          /* It has shrunk too much; turn off auto-saving here.
             Unless we're about to crash, in which case auto-save it
             anyway.
             */
          message (GETTEXT ("Buffer %s has shrunk a lot; auto save turned off there"),
                   XSTRING (b->name)->data);
          /* User can reenable saving with M-x auto-save.  */
          b->auto_save_file_name = Qnil;
          /* Prevent warning from repeating if user does so.  */
          b->save_length = Qzero;
          Fsleep_for (make_number (1));
          continue;
        }
        set_buffer_internal (b);
        if (!auto_saved && NILP (no_message))
          message (GETTEXT ("Auto-saving..."));
        condition_case_1 (Qt,
                          auto_save_1, Qnil,
                          auto_save_error, Qnil);
        auto_saved++;
        b->auto_save_modified = BUF_MODIFF (b);
        current_buffer->save_length = make_number (Z - BEG);
        set_buffer_internal (old);
      }
    }
  }

  /* Prevent another auto save till enough input events come in.  */
  if (auto_saved)
    record_auto_save ();

  if (auto_saved && NILP (no_message))
    message ("%s", ((omessage) ? omessage : GETTEXT ("Auto-saving...done")));

  Vquit_flag = oquit;

  auto_saving = 0;
  return Qnil;
}

DEFUN ("set-buffer-auto-saved", Fset_buffer_auto_saved,
  Sset_buffer_auto_saved, 0, 0, 0,
  "Mark current buffer as auto-saved with its current text.\n\
No auto-save file will be written until the buffer changes again.")
  ()
{
  current_buffer->auto_save_modified = MODIFF;
  current_buffer->save_length = make_number (Z - BEG);
  return Qnil;
}

DEFUN ("recent-auto-save-p", Frecent_auto_save_p, Srecent_auto_save_p,
  0, 0, 0,
  "Return t if buffer has been auto-saved since last read in or saved.")
  ()
{
  return (current_buffer->save_modified < current_buffer->auto_save_modified) ? Qt : Qnil;
}

void
syms_of_fileio ()
{
  defsymbol (&Qexpand_file_name, "expand-file-name");
  defsymbol (&Qdirectory_file_name, "directory-file-name");
  defsymbol (&Qfile_name_directory, "file-name-directory");
  defsymbol (&Qfile_name_nondirectory, "file-name-nondirectory");
  defsymbol (&Qunhandled_file_name_directory, "unhandled-file-name-directory");
  defsymbol (&Qfile_name_as_directory, "file-name-as-directory");
  defsymbol (&Qcopy_file, "copy-file");
  defsymbol (&Qmake_directory, "make-directory");
  defsymbol (&Qdelete_directory, "delete-directory");
  defsymbol (&Qdelete_file, "delete-file");
  defsymbol (&Qrename_file, "rename-file");
  defsymbol (&Qadd_name_to_file, "add-name-to-file");
  defsymbol (&Qmake_symbolic_link, "make-symbolic-link");
  defsymbol (&Qfile_exists_p, "file-exists-p");
  defsymbol (&Qfile_executable_p, "file-executable-p");
  defsymbol (&Qfile_readable_p, "file-readable-p");
  defsymbol (&Qfile_symlink_p, "file-symlink-p");
  defsymbol (&Qfile_writable_p, "file-writable-p");
  defsymbol (&Qfile_directory_p, "file-directory-p");
  defsymbol (&Qfile_accessible_directory_p, "file-accessible-directory-p");
  defsymbol (&Qfile_modes, "file-modes");
  defsymbol (&Qset_file_modes, "set-file-modes");
  defsymbol (&Qfile_newer_than_file_p, "file-newer-than-file-p");
  defsymbol (&Qinsert_file_contents, "insert-file-contents");
  defsymbol (&Qwrite_region, "write-region");
  defsymbol (&Qverify_visited_file_modtime, "verify-visited-file-modtime");
  defsymbol (&Qset_visited_file_modtime, "set-visited-file-modtime");
  defsymbol (&Qcar_less_than_car, "car-less-than-car"); /* Vomitous! */

  defsymbol (&Qfile_error, "file-error");
  defsymbol (&Qfile_already_exists, "file-already-exists");

  defsymbol (&Qfile_name_handler_alist, "file-name-handler-alist");
  defsymbol (&Qauto_save_hook, "auto-save-hook");

  pure_put (Qfile_error, Qerror_conditions,
            list2 (Qfile_error, Qerror));
  pure_put (Qfile_error, Qerror_message,
            build_string (DEFER_GETTEXT ("File error")));

  pure_put (Qfile_already_exists, Qerror_conditions,
            list3 (Qfile_already_exists, Qfile_error, Qerror));
  pure_put (Qfile_already_exists, Qerror_message,
            build_string (DEFER_GETTEXT ("File already exists")));

  DEFVAR_BOOL ("vms-stmlf-recfm", &vms_stmlf_recfm,
    "*Non-nil means write new files with record format `stmlf'.\n\
nil means use format `var'.  This variable is meaningful only on VMS.");
  vms_stmlf_recfm = 0;

  DEFVAR_LISP ("file-name-handler-alist", &Vfile_name_handler_alist,
    "*Alist of elements (REGEXP . HANDLER) for file names handled specially.\n\
If a file name matches REGEXP, then all I/O on that file is done by calling\n\
HANDLER.\n\
\n\
The first argument given to HANDLER is the name of the I/O primitive\n\
to be handled; the remaining arguments are the arguments that were\n\
passed to that primitive.  For example, if you do\n\
    (file-exists-p FILENAME)\n\
and FILENAME is handled by HANDLER, then HANDLER is called like this:\n\
    (funcall HANDLER 'file-exists-p FILENAME)\n\
The function `find-file-name-handler' checks this list for a handler\n\
for its argument.");
  Vfile_name_handler_alist = Qnil;

  DEFVAR_LISP ("after-insert-file-functions", &Vafter_insert_file_functions,
    "A list of functions to be called at the end of `insert-file-contents'.\n\
Each is passed one argument, the number of bytes inserted.  It should return\n\
the new byte count, and leave point the same.  If `insert-file-contents' is\n\
intercepted by a handler from `file-name-handler-alist', that handler is\n\
responsible for calling the after-insert-file-functions if appropriate.");
  Vafter_insert_file_functions = Qnil;

  DEFVAR_LISP ("write-region-annotate-functions",
	       &Vwrite_region_annotate_functions,
    "A list of functions to be called at the start of `write-region'.\n\
Each is passed two arguments, START and END as for `write-region'.\n\
It should return a list of pairs (POSITION . STRING) of strings to be\n\
effectively inserted at the specified positions of the file being written\n\
\(1 means to insert before the first byte written).  The POSITIONs must be\n\
sorted into increasing order.  If there are several functions in the list,\n\
the several lists are merged destructively.");
  Vwrite_region_annotate_functions = Qnil;

  defsubr (&Sfind_file_name_handler);

  defsubr (&Sfile_name_directory);
  defsubr (&Sfile_name_nondirectory);
  defsubr (&Sunhandled_file_name_directory);
  defsubr (&Sfile_name_as_directory);
  defsubr (&Sdirectory_file_name);
  defsubr (&Smake_temp_name);
  defsubr (&Sexpand_file_name);
  defsubr (&Sfile_truename);
  defsubr (&Ssubstitute_in_file_name);
  defsubr (&Scopy_file);
  defsubr (&Smake_directory_internal);
  defsubr (&Sdelete_directory);
  defsubr (&Sdelete_file);
  defsubr (&Srename_file);
  defsubr (&Sadd_name_to_file);
#ifdef S_IFLNK
  defsubr (&Smake_symbolic_link);
#endif /* S_IFLNK */
#ifdef VMS
  defsubr (&Sdefine_logical_name);
#endif /* VMS */
#ifdef HPUX_NET
  defsubr (&Ssysnetunam);
#endif /* HPUX_NET */
  defsubr (&Sfile_name_absolute_p);
  defsubr (&Sfile_exists_p);
  defsubr (&Sfile_executable_p);
  defsubr (&Sfile_readable_p);
  defsubr (&Sfile_writable_p);
  defsubr (&Sfile_symlink_p);
  defsubr (&Sfile_directory_p);
  defsubr (&Sfile_accessible_directory_p);
  defsubr (&Sfile_modes);
  defsubr (&Sset_file_modes);
  defsubr (&Sset_default_file_modes);
  defsubr (&Sdefault_file_modes);
  defsubr (&Sunix_sync);
  defsubr (&Sfile_newer_than_file_p);
  defsubr (&Sinsert_file_contents);
  defsubr (&Swrite_region);
  defsubr (&Scar_less_than_car); /* Vomitous! */
#if 0
  defsubr (&Sencrypt_string);
  defsubr (&Sdecrypt_string);
#endif
  defsubr (&Sverify_visited_file_modtime);
  defsubr (&Sclear_visited_file_modtime);
  defsubr (&Svisited_file_modtime);
  defsubr (&Sset_visited_file_modtime);
  defsubr (&Sset_buffer_modtime);
  defsubr (&Scompute_buffer_file_truename);

  defsubr (&Sdo_auto_save);
  defsubr (&Sset_buffer_auto_saved);
  defsubr (&Srecent_auto_save_p);
}
