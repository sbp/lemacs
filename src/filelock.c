/* Copyright (C) 1985, 1986, 1987, 1992, 1993 Free Software Foundation, Inc.

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

#include <stdio.h>		/* For sprintf */
#include <sys/types.h>
#include <sys/stat.h>

#include <errno.h>
#include <sys/file.h>
#ifdef USG
#include <fcntl.h>
#endif /* USG */

#ifdef VMS
#include "vms-pwd.h"
#else
#include <pwd.h>
#endif

#include "lisp.h"
#include "paths.h"
#include "buffer.h"

extern int errno;

#ifdef CLASH_DETECTION
  
Lisp_Object Vlock_directory;
Lisp_Object Vsuperlock_path;

/* If system does not have symbolic links, it does not have lstat.
   In that case, use ordinary stat instead.  */

#ifndef S_IFLNK
#define lstat stat
#endif

Lisp_Object Qask_user_about_supersession_threat;
Lisp_Object Qask_user_about_lock;

static void fill_in_lock_file_name (char *lockfile, Lisp_Object fn);
static void lock_superlock (const char *lfname);

static Lisp_Object
lock_file_owner_name (const char *lfname)
{
  struct stat s;
  struct passwd *the_pw = 0;

  if (lstat (lfname, &s) == 0)
    the_pw = getpwuid (s.st_uid);
  return (the_pw == 0 ? Qnil : build_string (the_pw->pw_name));
}


/* lock_file locks file fn,
   meaning it serves notice on the world that you intend to edit that file.
   This should be done only when about to modify a file-visiting
   buffer previously unmodified.
   Do not (normally) call lock_buffer for a buffer already modified,
   as either the file is already locked, or the user has already
   decided to go ahead without locking.

   When lock_buffer returns, either the lock is locked for us,
   or the user has said to go ahead without locking.

   If the file is locked by someone else, lock_buffer calls
   ask-user-about-lock (a Lisp function) with two arguments,
   the file name and the name of the user who did the locking.
   This function can signal an error, or return t meaning
   take away the lock, or return nil meaning ignore the lock.  */

/* The lock file name is the file name with "/" replaced by "!"
   and put in the Emacs lock directory.  */
/* (ie., /ka/king/junk.tex -> /!/!ka!king!junk.tex). */

static int lock_file_1 (const char *lfname, int mode);
static int lock_if_free (const char *lfname);

void
lock_file (fn)
     Lisp_Object fn;
{
  register Lisp_Object attack;
  register char *lfname;
  struct gcpro gcpro1;

  if (NILP (Vlock_directory) || NILP (Vsuperlock_path))
    return;
  CHECK_STRING (fn, 0);
  CHECK_STRING (Vlock_directory, 1);

  GCPRO1 (fn);

  /* Create the name of the lock-file for file fn */
  lfname = (char *) alloca (XSTRING (fn)->size +
			    XSTRING (Vlock_directory)->size + 1);
  fill_in_lock_file_name (lfname, fn);

  /* See if this file is visited and has changed on disk since it was
     visited.  */
  {
    register Lisp_Object subject_buf = Fget_file_buffer (fn);
    if (!NILP (subject_buf)
	&& NILP (Fverify_visited_file_modtime (subject_buf))
	&& !NILP (Ffile_exists_p (fn)))
      call1 (Qask_user_about_supersession_threat, fn);
  }

  /* Try to lock the lock. */
  if (lock_if_free (lfname) <= 0)
    /* Return now if we have locked it, or if lock dir does not exist */
    goto done;

  /* Else consider breaking the lock */
  attack = call2 (Qask_user_about_lock, fn,
		  lock_file_owner_name (lfname));
  if (!NILP (attack))
    /* User says take the lock */
    {
      CHECK_STRING (Vsuperlock_path, 0);
      lock_superlock (lfname);
      lock_file_1 (lfname, O_WRONLY);
      unlink ((char *) XSTRING (Vsuperlock_path)->data);
      goto done;
    }
  /* User says ignore the lock */
 done:
  UNGCPRO;
  return;
}

static void
fill_in_lock_file_name (char *lockfile, Lisp_Object fn)
     /* fn must be a Lisp_String! */
{
  register char *p;
  CHECK_STRING (Vlock_directory, 0);

  strcpy (lockfile, (char*) XSTRING (Vlock_directory)->data);

  p = lockfile + strlen (lockfile);

  strcpy (p, (char *) XSTRING (fn)->data);

  for (; *p; p++)
    {
      if (*p == '/')
	*p = '!';
    }
}

/* Lock the lock file named LFNAME.
   If MODE is O_WRONLY, we do so even if it is already locked.
   If MODE is O_WRONLY | O_EXCL | O_CREAT, we do so only if it is free.
   Return 1 if successful, 0 if not.  */

static int
lock_file_1 (const char *lfname, int mode)
{
  register int fd;
  char buf[20];

  if ((fd = emacs_open (lfname, mode, 0666)) >= 0)
    {
#ifdef USG
      chmod (lfname, 0666);
#else
      fchmod (fd, 0666);
#endif
      sprintf (buf, "%d ", getpid ());
      emacs_write (fd, buf, strlen (buf));
      emacs_close (fd);
      return 1;
    }
  else
    return 0;
}

static int current_lock_owner (const char *);

/* Lock the lock named LFNAME if possible.
   Return 0 in that case.
   Return positive if lock is really locked by someone else.
   Return -1 if cannot lock for any other reason.  */

static int
lock_if_free (const char *lfname)
{
  register int clasher;

  while (lock_file_1 (lfname, O_WRONLY | O_EXCL | O_CREAT) == 0)
    {
      if (errno != EEXIST)
	return -1;
      clasher = current_lock_owner (lfname);
      if (clasher != 0)
	if (clasher != getpid ())
	  return (clasher);
	else return (0);
      /* Try again to lock it */
    }
  return 0;
}

static int current_lock_owner_1 (const char *);

/* Return the pid of the process that claims to own the lock file LFNAME,
   or 0 if nobody does or the lock is obsolete,
   or -1 if something is wrong with the locking mechanism.  */

static int
current_lock_owner (const char *lfname)
{
  int owner = current_lock_owner_1 (lfname);
  if (owner == 0 && errno == ENOENT)
    return (0);
  /* Is it locked by a process that exists?  */
  if (owner != 0 && (kill (owner, 0) >= 0 || errno == EPERM))
    return (owner);
  if (unlink (lfname) < 0)
    return (-1);
  return (0);
}

static int
current_lock_owner_1 (const char *lfname)
{
  register int fd;
  char buf[20];
  int tem;

  fd = emacs_open (lfname, O_RDONLY, 0666);
  if (fd < 0)
    return 0;
  tem = emacs_read (fd, buf, sizeof buf);
  emacs_close (fd);
  return (tem <= 0 ? 0 : atoi (buf));
}


void
unlock_file (fn)
     register Lisp_Object fn;
{
  register char *lfname;
  if (NILP (Vlock_directory) || NILP (Vsuperlock_path)) return;
  CHECK_STRING (fn, 0);
  CHECK_STRING (Vlock_directory, 0);
  CHECK_STRING (Vsuperlock_path, 0);

  lfname = (char *) alloca (XSTRING (fn)->size +
			    XSTRING (Vlock_directory)->size + 1);
  fill_in_lock_file_name (lfname, fn);

  lock_superlock (lfname);

  if (current_lock_owner_1 (lfname) == getpid ())
    unlink (lfname);

  unlink ((char *) XSTRING (Vsuperlock_path)->data);
}

static void
lock_superlock (lfname)
     const char *lfname;
{
  register int i, fd;

  for (i = -20; i < 0 &&
       (fd = emacs_open ((char *) XSTRING (Vsuperlock_path)->data,
			 O_WRONLY | O_EXCL | O_CREAT, 0666)) < 0;
       i++)
    {
      if (errno != EEXIST)
	return;
      sleep (1);
    }
  if (fd >= 0)
    {
#ifdef USG
      chmod ((char *) XSTRING (Vsuperlock_path)->data, 0666);
#else
      fchmod (fd, 0666);
#endif
      emacs_write (fd, lfname, strlen (lfname));
      emacs_close (fd);
    }
}

void
unlock_all_files ()
{
  register Lisp_Object tail;
  register struct buffer *b;

  for (tail = Vbuffer_alist; XGCTYPE (tail) == Lisp_Cons;
       tail = XCONS (tail)->cdr)
    {
      b = XBUFFER (XCONS (XCONS (tail)->car)->cdr);
      if (STRINGP (b->filename) &&
	  b->save_modified < BUF_MODIFF (b))
        if (STRINGP (b->filename))
          unlock_file (b->filename);
    }
}


DEFUN ("lock-buffer", Flock_buffer, Slock_buffer,
  0, 1, 0,
  "Lock FILE, if current buffer is modified.\n\
FILE defaults to current buffer's visited file,\n\
or else nothing is done if current buffer isn't visiting a file.")
  (fn)
     Lisp_Object fn;
{
  if (NILP (fn))
    fn = current_buffer->filename;
  CHECK_STRING (fn, 0);
  if (current_buffer->save_modified < MODIFF
      && !NILP (fn))
    lock_file (fn);
  return Qnil;    
}

DEFUN ("unlock-buffer", Funlock_buffer, Sunlock_buffer,
  0, 0, 0,
 "Unlock the file visited in the current buffer,\n\
if it should normally be locked.")
  ()
{
  if (current_buffer->save_modified < MODIFF
      && STRINGP (current_buffer->filename))
    unlock_file (current_buffer->filename);
  return Qnil;
}


/* Unlock the file visited in buffer BUFFER.  */

void
unlock_buffer (buffer)
     struct buffer *buffer;
{
  if (buffer->save_modified < BUF_MODIFF (buffer)
      && STRINGP (buffer->filename))
    unlock_file (buffer->filename);
}

DEFUN ("file-locked-p", Ffile_locked_p, Sfile_locked_p, 0, 1, 0,
  "Return nil if the FILENAME is not locked,\n\
t if it is locked by you, else a string of the name of the locker.")
  (fn)
  Lisp_Object fn;
{
  register char *lfname;
  int owner;

  if (NILP (Vlock_directory) || NILP (Vsuperlock_path))
    return Qnil;
  CHECK_STRING (Vlock_directory, 1);

  fn = Fexpand_file_name (fn, Qnil);

  /* Create the name of the lock-file for file filename */
  lfname = (char *) alloca (XSTRING (fn)->size +
			    XSTRING (Vlock_directory)->size + 1);
  fill_in_lock_file_name (lfname, fn);

  owner = current_lock_owner (lfname);
  if (owner <= 0)
    return (Qnil);
  else if (owner == getpid ())
    return (Qt);
  
  return (lock_file_owner_name (lfname));
}

void
syms_of_filelock ()
{
  defsubr (&Sunlock_buffer);
  defsubr (&Slock_buffer);
  defsubr (&Sfile_locked_p);

  defsymbol (&Qask_user_about_supersession_threat,
             "ask-user-about-supersession-threat");
  defsymbol (&Qask_user_about_lock, "ask-user-about-lock");

  DEFVAR_LISP ("lock-directory", &Vlock_directory, "Don't change this");
  DEFVAR_LISP ("superlock-path", &Vsuperlock_path, "Don't change this");
#ifdef PATH_LOCK
  Vlock_directory = build_string (PATH_LOCK);
#else
  Vlock_directory = Qnil;
#endif
#ifdef PATH_SUPERLOCK
  Vsuperlock_path = build_string (PATH_SUPERLOCK);
#else
  Vsuperlock_path = Qnil;
#endif
}

void
init_filelock (void)
{
  /* All done dynamically by startup.el */
}

#endif /* CLASH_DETECTION */
