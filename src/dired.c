/* Lisp functions for making directory listings.
   Copyright (C) 1985, 1986, 1992, 1993, 1994 Free Software Foundation, Inc.

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

#include "config.h"

#ifdef VMS
#include <string.h>
#include <rms.h>
#include <rmsdef.h>
#endif

/* The d_nameln member of a struct dirent includes the '\0' character
   on some systems, but not on others.  What's worse, you can't tell
   at compile-time which one it will be, since it really depends on
   the sort of system providing the filesystem you're reading from,
   not the system you are running on.  Paul Eggert
   <eggert@bi.twinsun.com> says this occurs when Emacs is running on a
   SunOS 4.1.2 host, reading a directory that is remote-mounted from a
   Solaris 2.1 host and is in a native Solaris 2.1 filesystem.

   Since applying strlen to the name always works, we'll just do that.  */
#define NAMLEN(p) strlen (p->d_name)

#ifdef SYSV_SYSTEM_DIR

#include <dirent.h>
#define DIRENTRY struct dirent

#else

#ifdef NONSYSTEM_DIR_LIBRARY
#include "ndir.h"
#else /* not NONSYSTEM_DIR_LIBRARY */
#include <sys/dir.h>
#endif /* not NONSYSTEM_DIR_LIBRARY */

#define DIRENTRY struct direct

extern DIR *opendir ();
extern struct direct *readdir ();

#endif

#include "lisp.h"
#include "intl.h"
#include "buffer.h"
#include "commands.h"

#include "regex.h"

#define min(a, b) ((a) < (b) ? (a) : (b))

/* if system does not have symbolic links, it does not have lstat.
   In that case, use ordinary stat instead.  */

#ifndef S_IFLNK
#define lstat stat
#endif

Lisp_Object Vcompletion_ignored_extensions;

Lisp_Object Qdirectory_files;
Lisp_Object Qfile_name_completion;
Lisp_Object Qfile_name_all_completions;
Lisp_Object Qfile_attributes;

DEFUN ("directory-files", Fdirectory_files, Sdirectory_files, 1, 5, 0,
  "Return a list of names of files in DIRECTORY.\n\
There are four optional arguments:\n\
If FULL is non-nil, absolute pathnames of the files are returned.\n\
If MATCH is non-nil, only pathnames containing that regexp are returned.\n\
If NOSORT is non-nil, the list is not sorted--its order is unpredictable.\n\
 NOSORT is useful if you plan to sort the result yourself.\n\
If FILES-ONLY is the symbol t, then only the \"files\" in the directory\n\
 will be returned; subdirectories will be excluded.  If FILES-ONLY is not\n\
 nil and not t, then only the subdirectories will be returned.  Otherwise,\n\
 if FILES-ONLY is nil (the default) then both files and subdirectories will\n\
 be returned.")
  (dirname, full, match, nosort, files_only)
     Lisp_Object dirname, full, match, nosort, files_only;
{
  DIR *d;
  int dirname_length;
  Lisp_Object list, name, dirfilename;
  Lisp_Object handler;

  char statbuf [MAXNAMLEN+2];
  char *statbuf_tail;
  Lisp_Object tail_cons;
  char slashfilename[MAXNAMLEN+2];
  char *filename = slashfilename;

  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

  /* >>> Needs more gcpro's */
  GCPRO4 (dirname, match, files_only, tail_cons);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (dirname);
  if (!NILP (handler))
  {
    UNGCPRO;
    if (!NILP (files_only))
      return call6 (handler, Qdirectory_files, dirname, full, match, nosort,
                    files_only);
    else
      return call5 (handler, Qdirectory_files, dirname, full, match, nosort);
  }

  dirname = Fexpand_file_name (dirname, Qnil);
  dirfilename = Fdirectory_file_name (dirname);

  {
    /* Lemacs: this should come before the opendir() because it might error. */
    /*>>> need more gcpro's */
    Lisp_Object name_as_dir = Ffile_name_as_directory (dirname);
    CHECK_STRING (name_as_dir, 0);
    memcpy (statbuf, ((char *) XSTRING (name_as_dir)->data),
           XSTRING (name_as_dir)->size);
    statbuf_tail = statbuf + XSTRING (name_as_dir)->size;
  }

  /* Lemacs: this must come after Ffile_name_as_directory() or searchbuf
     gets smashed.   This should come before the opendir() because it
     might signal an error.
   */
  if (!NILP (match))
    {
      CHECK_STRING (match, 3);

      /* MATCH might be a flawed regular expression.  Rather than
	 catching and signalling our own errors, we just call
	 compile_pattern to do the work for us.  */
#ifdef VMS
      compile_pattern (match, &searchbuf, 0,
		       XSTRING (XBUFFER (Vbuffer_defaults)->downcase_table)->data);
#else
      compile_pattern (match, &searchbuf, 0, 0);
#endif
    }

  /* Now searchbuf is the compiled form of MATCH; don't call anything
     which might compile a new regexp until we're done with the loop!  */


  /* Do this opendir after anything which might signal an error; if
     an error is signalled while the directory stream is open, we
     have to make sure it gets closed, and setting up an
     unwind_protect to do so would be a pain.  */
  d = opendir ((char *) XSTRING (dirfilename)->data);
  if (! d)
    report_file_error (GETTEXT ("Opening directory"), list1 (dirname));

  list = Qnil;
  tail_cons = Qnil;
  dirname_length = XSTRING (dirname)->size;
#ifndef VMS
  if (dirname_length == 0
      || XSTRING (dirname)->data[dirname_length - 1] != '/')
  {
    *filename++ = '/';
    dirname_length++;
  }
#endif /* VMS */

  /* Loop reading blocks */
  while (1)
    {
      DIRENTRY *dp = readdir (d);
      int len;

      if (!dp) break;
      len = NAMLEN (dp);
      if (dp->d_ino)
	{
	  strncpy (filename, dp->d_name, len);
	  filename[len] = 0;
	  if (NILP (match)
	      || (0 <= re_search (&searchbuf, filename, len, 0, len, 0)))
	    {
	      if (!NILP (files_only))
		{
		  int dir_p;
		  struct stat st;

		  memcpy (statbuf_tail, filename, len);
		  statbuf_tail [len] = 0;

		  if (stat (statbuf, &st) < 0)
		    dir_p = 0;
		  else
		    dir_p = ((st.st_mode & S_IFMT) == S_IFDIR);

		  if (EQ (files_only, Qt) && dir_p)
		    continue;
		  else if (!EQ (files_only, Qt) && !dir_p)
		    continue;
		}

	      if (!NILP (full))
		name = concat2 (dirname, build_string (slashfilename));
	      else
		name = make_string (filename, len);

	      if (NILP (tail_cons))
		{
		  list = list1 (name);
		  tail_cons = list;
		}
	      else
		{
		  XCONS (tail_cons)->cdr = list1 (name);
		  tail_cons = XCONS (tail_cons)->cdr;
		}
	    }
	}
    }
  closedir (d);
  UNGCPRO;
  if (!NILP (nosort))
    return list;
  return Fsort (Fnreverse (list), Qstring_lessp);
}

static Lisp_Object file_name_completion (Lisp_Object file, 
                                         Lisp_Object dirname, 
                                         int all_flag, int ver_flag);

DEFUN ("file-name-completion", Ffile_name_completion, Sfile_name_completion,
  2, 2, 0,
  "Complete file name FILE in directory DIR.\n\
Returns the longest string common to all filenames in DIR\n\
that start with FILE.\n\
If there is only one and FILE matches it exactly, returns t.\n\
Returns nil if DIR contains no name starting with FILE.\n\
\n\
Filenames which end with any member of `completion-ignored-extensions'\n\
are not considered as possible completions for FILE unless there is no\n\
other possible completion.  `completion-ignored-extensions' is not applied\n\
to the names of directories.")
  (file, dirname)
     Lisp_Object file, dirname;
{
  Lisp_Object handler;
  /* Don't waste time trying to complete a null string.
     Besides, this case happens when user is being asked for
     a directory name and has supplied one ending in a /.
     We would not want to add anything in that case
     even if there are some unique characters in that directory.  */
  if (STRINGP (file) && XSTRING (file)->size == 0)
    return file;

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (dirname);
  if (!NILP (handler))
    return call3 (handler, Qfile_name_completion, file, dirname);

  return file_name_completion (file, dirname, 0, 0);
}

DEFUN ("file-name-all-completions", Ffile_name_all_completions,
  Sfile_name_all_completions, 2, 2, 0,
  "Return a list of all completions of file name FILE in directory DIR.\n\
These are all file names in directory DIR which begin with FILE.\n\
\n\
Filenames which end with any member of `completion-ignored-extensions'\n\
are not considered as possible completions for FILE unless there is no\n\
other possible completion.  `completion-ignored-extensions' is not applied\n\
to the names of directories.")
  (file, dirname)
     Lisp_Object file, dirname;
{
  Lisp_Object handler;

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (dirname);
  if (!NILP (handler))
    return call3 (handler, Qfile_name_all_completions, file, dirname);

  return file_name_completion (file, dirname, 1, 0);
}

static int
file_name_completion_stat (dirname, dp, st_addr)
     Lisp_Object dirname;
     DIRENTRY *dp;
     struct stat *st_addr;
{
  int len = NAMLEN (dp);
  int pos = XSTRING (dirname)->size;
  char *fullname = (char *) alloca (len + pos + 2);

  memcpy (fullname, XSTRING (dirname)->data, pos);
#ifndef VMS
  if (fullname[pos - 1] != '/')
    fullname[pos++] = '/';
#endif

  memcpy (fullname + pos, dp->d_name, len);
  fullname[pos + len] = 0;

  return stat (fullname, st_addr);
}


#ifdef VMS
extern DIRENTRY * readdirver ();
#endif

static Lisp_Object
file_name_completion (file, dirname, all_flag, ver_flag)
     Lisp_Object file, dirname;
     int all_flag, ver_flag;
{
  DIR *d;
  int matchcount = 0;
  Lisp_Object bestmatch = Qnil;
  int bestmatchsize = 0;
  struct stat st;
  int passcount;
  int speccount = specpdl_depth ();
  DIRENTRY *((*readfunc) ()) = readdir;
  struct gcpro gcpro1, gcpro2, gcpro3;

  GCPRO3 (file, dirname, bestmatch);

  CHECK_STRING (file, 0);

#ifdef VMS
  /* Filename completion on VMS ignores case, since VMS filesys does.  */
  specbind (Qcompletion_ignore_case, Qt);

  if (ver_flag)
    readfunc = readdirver;
  file = Fupcase (file);
#endif /* VMS */

  dirname = Fexpand_file_name (dirname, Qnil);
  bestmatch = Qnil;

  /* With passcount = 0, ignore files that end in an ignored extension.
     If nothing found then try again with passcount = 1, don't ignore them.
     If looking for all completions, start with passcount = 1,
     so always take even the ignored ones.

     ** It would not actually be helpful to the user to ignore any possible
     completions when making a list of them.**  */

  for (passcount = !!all_flag; NILP (bestmatch) && passcount < 2; passcount++)
    {
      d = opendir ((char *) XSTRING (Fdirectory_file_name (dirname))->data);
      if (!d)
	report_file_error (GETTEXT ("Opening directory"), list1 (dirname));

      /* Loop reading blocks */
      /* (att3b compiler bug requires do a null comparison this way) */
      while (1)
	{
	  DIRENTRY *dp;
	  int len;
          int directoryp;
          int ignored_extension_p = 0;

	  dp = (*readfunc) (d);
	  if (!dp) break;

	  len = NAMLEN (dp);

	  if (!NILP (Vquit_flag) && NILP (Vinhibit_quit))
	    goto quit;
	  if (!dp->d_ino
	      || len < XSTRING (file)->size
	      || 0 <= scmp ((unsigned char *) dp->d_name,
			    XSTRING (file)->data,
			    XSTRING (file)->size))
	    continue;

          if (file_name_completion_stat (dirname, dp, &st) < 0)
            continue;

          directoryp = ((st.st_mode & S_IFMT) == S_IFDIR);
          if (!directoryp)
            {
	      /* Compare extensions-to-be-ignored against end of this file name */
	      /* if name is not an exact match against specified string */
	      if (!passcount && len > XSTRING (file)->size)
		{
		  Lisp_Object tem;
		  /* and exit this for loop if a match is found */
		  for (tem = Vcompletion_ignored_extensions;
		       CONSP (tem); tem = XCONS (tem)->cdr)
		    {
		      Lisp_Object elt = XCONS (tem)->car;
		      int skip;

		      if (!STRINGP (elt)) continue;
		      skip = len - XSTRING (elt)->size;
		      if (skip < 0) continue;

		      if (0 > scmp ((unsigned char *) dp->d_name + skip,
				     XSTRING (elt)->data,
				     XSTRING (elt)->size))
			{
			  ignored_extension_p = 1;
			  break;
			}
		    }
		}
	    }

	  /* Unless an ignored-extensions match was found,
             process this name as a completion */
	  if (passcount || !ignored_extension_p)
	    {
	      /* Update computation of how much all possible completions match */

	      matchcount++;

	      if (all_flag || NILP (bestmatch))
		{
                  Lisp_Object name = Qnil;
                  struct gcpro gcpro1;
                  GCPRO1 (name);
		  /* This is a possible completion */
		  if (directoryp)
		    {
		      /* This completion is a directory; make it end with '/' */
		      name = Ffile_name_as_directory (make_string (dp->d_name, len));
		    }
		  else
		    name = make_string (dp->d_name, len);
		  if (all_flag)
		    {
		      bestmatch = Fcons (name, bestmatch);
		    }
		  else
		    {
		      bestmatch = name;
		      bestmatchsize = XSTRING (name)->size;
		    }
                  UNGCPRO;
		}
	      else
		{
		  int compare = min (bestmatchsize, len);
		  unsigned char *p1 = XSTRING (bestmatch)->data;
		  unsigned char *p2 = (unsigned char *) dp->d_name;
		  int matchsize = scmp (p1, p2, compare);

		  if (matchsize < 0)
		    matchsize = compare;
		  if (completion_ignore_case)
		    {
		      /* If this is an exact match except for case,
			 use it as the best match rather than one that is not
			 an exact match.  This way, we get the case pattern
			 of the actual match.  */
		      if ((matchsize == len
			   && matchsize + !!directoryp 
			      < XSTRING (bestmatch)->size)
			  ||
			  /* If there is no exact match ignoring case,
			     prefer a match that does not change the case
			     of the input.  */
			  (((matchsize == len)
			    ==
			    (matchsize + !!directoryp 
			     == string_length (XSTRING (bestmatch))))
			   /* If there is more than one exact match aside from
			      case, and one of them is exact including case,
			      prefer that one.  */
			   && !memcmp (p2, XSTRING (file)->data,
				       string_length (XSTRING (file)))
			   && memcmp (p1, XSTRING (file)->data,
				      string_length (XSTRING (file)))))
			{
			  bestmatch = make_string (dp->d_name, len);
			  if (directoryp)
			    bestmatch = Ffile_name_as_directory (bestmatch);
			}
		    }

		  /* If this dirname all matches,
		     see if implicit following slash does too.  */
		  if (directoryp
		      && compare == matchsize
		      && bestmatchsize > matchsize
		      && p1[matchsize] == '/')
		    matchsize++;
		  bestmatchsize = matchsize;
		}
	    }
	}
      closedir (d);
    }

  unbind_to (speccount, Qnil);

  UNGCPRO;

  if (all_flag || NILP (bestmatch))
    return bestmatch;
  if (matchcount == 1 && bestmatchsize == XSTRING (file)->size)
    return Qt;
  return Fsubstring (bestmatch, make_number (0), make_number (bestmatchsize));
 quit:
  UNGCPRO;
  if (d) closedir (d);
  Vquit_flag = Qnil;
  return Fsignal (Qquit, Qnil);
}


#ifdef VMS

DEFUN ("file-name-all-versions", Ffile_name_all_versions,
  Sfile_name_all_versions, 2, 2, 0,
  "Return a list of all versions of file name FILE in directory DIR.")
  (file, dirname)
     Lisp_Object file, dirname;
{
  return file_name_completion (file, dirname, 1, 1);
}

DEFUN ("file-version-limit", Ffile_version_limit, Sfile_version_limit, 1, 1, 0,
  "Return the maximum number of versions allowed for FILE.\n\
Returns nil if the file cannot be opened or if there is no version limit.")
  (filename)
     Lisp_Object filename;
{
  Lisp_Object retval;
  struct FAB    fab;
  struct RAB    rab;
  struct XABFHC xabfhc;
  int status;

  filename = Fexpand_file_name (filename, Qnil);
  CHECK_STRING (filename, 0);
  fab      = cc$rms_fab;
  xabfhc   = cc$rms_xabfhc;
  fab.fab$l_fna = XSTRING (filename)->data;
  fab.fab$b_fns = strlen (fab.fab$l_fna);
  fab.fab$l_xab = (char *) &xabfhc;
  status = sys$open (&fab, 0, 0);
  if (status != RMS$_NORMAL)	/* Probably non-existent file */
    return Qnil;
  sys$close (&fab, 0, 0);
  if (xabfhc.xab$w_verlimit == 32767)
    return Qnil;		/* No version limit */
  else
    return make_number (xabfhc.xab$w_verlimit);
}

#endif /* VMS */


static Lisp_Object
wasteful_word_to_lisp (unsigned int item)
{
  /* Compatibility: in other versions, file-attributes returns a LIST
     of two 16 bit integers... */
  Lisp_Object cons = word_to_lisp (item);
  XCONS (cons)->cdr = Fcons (XCONS (cons)->cdr, Qnil);
  return cons;
}

DEFUN ("file-attributes", Ffile_attributes, Sfile_attributes, 1, 1, 0,
  "Return a list of attributes of file FILENAME.\n\
Value is nil if specified file cannot be opened.\n\
Otherwise, list elements are:\n\
 0. t for directory, string (name linked to) for symbolic link, or nil.\n\
 1. Number of links to file.\n\
 2. File uid.\n\
 3. File gid.\n\
 4. Last access time, as a list of two integers.\n\
  First integer has high-order 16 bits of time, second has low 16 bits.\n\
 5. Last modification time, likewise.\n\
 6. Last status change time, likewise.\n\
 7. Size in bytes. (-1, if number is out of range).\n\
 8. File modes, as a string of ten letters or dashes as in ls -l.\n\
 9. t iff file's gid would change if file were deleted and recreated.\n\
10. inode number.\n\
11. Device number.\n\
\n\
If file does not exist, returns nil.")
  (filename)
     Lisp_Object filename;
{
  Lisp_Object values[12];
  Lisp_Object dirname = Qnil;
  struct stat s;
  char modes[10];
  Lisp_Object handler;
  struct gcpro gcpro1, gcpro2;

  filename = Fexpand_file_name (filename, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename);
  if (!NILP (handler))
    return call2 (handler, Qfile_attributes, filename);

  if (lstat ((char *) XSTRING (filename)->data, &s) < 0)
    return Qnil;

  GCPRO2 (filename, dirname);

#ifdef BSD4_2
  dirname = Ffile_name_directory (filename);
#endif

  switch (s.st_mode & S_IFMT)
    {
    default:
      values[0] = Qnil;
      break;
    case S_IFDIR:
      values[0] = Qt;
      break;
#ifdef S_IFLNK
    case S_IFLNK:
      values[0] = Ffile_symlink_p (filename);
      break;
#endif
    }
  values[1] = make_number (s.st_nlink);
  values[2] = make_number (s.st_uid);
  values[3] = make_number (s.st_gid);
  values[4] = wasteful_word_to_lisp (s.st_atime);
  values[5] = wasteful_word_to_lisp (s.st_mtime);
  values[6] = wasteful_word_to_lisp (s.st_ctime);
  values[7] = make_number (s.st_size);
  /* If the size is out of range, give back -1.  */
  /* >>> Fix when Emacs gets bignums! */
  if (XINT (values[7]) != s.st_size)
    XSETINT (values[7], -1);
  filemodestring (&s, modes);
  values[8] = make_string (modes, 10);
#ifdef BSD4_3 /* Gross kludge to avoid lack of "#if defined(...)" in VMS */
#define BSD4_2 /* A new meaning to the term `backwards compatibility' */
#endif
#ifdef BSD4_2			/* file gid will be dir gid */
  {
    struct stat sdir;

    if (!NILP (dirname) && stat ((char *) XSTRING (dirname)->data, &sdir) == 0)
      values[9] = (sdir.st_gid != s.st_gid) ? Qt : Qnil;
    else                        /* if we can't tell, assume worst */
      values[9] = Qt;
  }
#else                           /* file gid will be egid */
  values[9] = (s.st_gid != getegid ()) ? Qt : Qnil;
#endif	/* BSD4_2 (or BSD4_3) */
#ifdef BSD4_3
#undef BSD4_2 /* ok, you can look again without throwing up */
#endif
  values[10] = make_number (s.st_ino);
  values[11] = make_number (s.st_dev);
  UNGCPRO;
  return Flist (countof (values), values);
}

void
syms_of_dired ()
{
  defsymbol (&Qdirectory_files, "directory-files");
  defsymbol (&Qfile_name_completion, "file-name-completion");
  defsymbol (&Qfile_name_all_completions, "file-name-all-completions");
  defsymbol (&Qfile_attributes, "file-attributes");

  defsubr (&Sdirectory_files);
  defsubr (&Sfile_name_completion);
#ifdef VMS
  defsubr (&Sfile_name_all_versions);
  defsubr (&Sfile_version_limit);
#endif /* VMS */
  defsubr (&Sfile_name_all_completions);
  defsubr (&Sfile_attributes);

  DEFVAR_LISP ("completion-ignored-extensions", &Vcompletion_ignored_extensions,
    "*Completion ignores filenames ending in any string in this list.\n\
This variable does not affect lists of possible completions,\n\
but does affect the commands that actually do completions.\n\
It is used by the functions `file-name-completion' and\n\
`file-name-all-completions'.");
  Vcompletion_ignored_extensions = Qnil;
}
