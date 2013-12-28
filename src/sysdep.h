/* System-dependent prototypes
   Copyright (C) 1985, 1993, 1994 Free Software Foundation, Inc.

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

#ifndef _EMACS_SYSDEP_H_
#define _EMACS_SYSDEP_H_

#if !defined(VMS) || !defined(LINK_CRTL_SHARE) || !defined(SHAREABLE_LIB_BUG)
extern char **environ;
#else
extern noshare char **environ;
#endif /* VMS sharable environ buh */

struct emacs_tty;
extern int emacs_get_tty (int fd, struct emacs_tty *settings);
extern int emacs_set_tty (int fd, struct emacs_tty *settings, int waitp);


extern void discard_tty_input (void);

extern void stuff_char (int c);

extern void init_baud_rate (void);

extern void set_exclusive_use (int fd);

extern void wait_without_blocking (void);

SIGTYPE wait_for_termination_signal (int ignore);

/* Wait for subprocess with process id `pid' to terminate and
   make sure it will get eliminated (not remain forever as a zombie) */
extern void wait_for_termination (int pid);

/* flush any pending output
 * (may flush input as well; it does not matter the way we use it)
 */
extern void flush_pending_output (int channel);

extern void child_setup_tty (int out);

extern void setpgrp_of_tty (int pid);

extern int interrupt_input;

/* Suspend the Emacs process; give terminal to its superior.  */
extern void sys_suspend (void);

extern void init_sigio (void);

extern void reset_sigio (void);

extern void request_sigio (void);

extern void unrequest_sigio (void);

extern void init_sys_modes (void);

/* Return nonzero if safe to use tabs in output.
   At the time this is called, init_sys_modes has not been done yet.  */
extern int tabs_safe_p (void);

/* Get terminal size from system.
   If zero or a negative number is stored, the value is not valid.  */
extern void get_screen_size (int *widthp, int *heightp);
/* Set the logical window size associated with descriptor FD */
extern int set_window_size (int fd, int height, int width);

/* Prepare the terminal for exiting Emacs; move the cursor to the
   bottom of the frame, turn off interrupt-driven I/O, etc.  */
extern void reset_sys_modes (void);

/* Set up the proper status flags for use of a pty.  */
extern void setup_pty (int fd);

/* Return the address of the start of the text segment prior to unexec. */
extern char *start_of_text (void);
/* Return the address of the start of the data segment prior to unexec. */
extern void *start_of_data (void);
/* Return the address of the end of the text segment prior to unexec. */
extern char *end_of_text (void);
/* Return the address of the end of the data segment prior to unexec. */
extern char *end_of_data (void);


/* Get_system_name returns as its value a string for system-name to return. */
extern char *get_system_name (void);


#ifndef HAVE_SELECT
extern int select_alarmed;
extern int select (int nfds, int *rfds, int *wfds, int *efds, int *timeout);
extern void read_input_waiting (void);
#endif /* HAVE_SELECT */

#ifdef F_SETOWN
extern int old_fcntl_owner;
#endif


#ifndef USG5_4
extern int sys_open (CONST char *path, int oflag, int mode);
#endif

extern int sys_close (int fd);

extern int sys_read (int fildes, void *buf, unsigned int nbyte);

extern int sys_write (int fildes, CONST void *buf, unsigned int nbyte);

extern int sys_access (CONST char *path, int mode);

unsigned int sys_getuid (void);

extern int sys_creat ();

extern int sys_fwrite (char *ptr, int size, int num, FILE *fp);

#ifndef HAVE_GETWD
extern char *getwd (char *pathname);
#endif

#ifndef HAVE_RENAME
extern int rename (CONST char *from, CONST char *to);
#endif

extern int setpriority (int which, int who, int prio);

#ifndef HAVE_VFORK
extern pid_t vfork (void);
#endif

#if defined(MISSING_UTIMES) || defined(IRIS_UTIME)
struct timeval;
extern int utimes (CONST char *file, CONST struct timeval *tvp);
#endif


#ifndef HAVE_DUP2
extern int dup2 (int oldd, int newd);
#endif

#if !defined(HAVE_GETTIMEOFDAY) && defined(HAVE_TIMEVAL)
struct timeval;
struct timezone;
extern int gettimeofday (struct timeval *, struct timezone *);
#endif

#if defined(SYSV_SYSTEM_DIR)
#if !defined(AIX) && !defined(sun) && !defined(__alpha) && !defined(LINUX) && !defined(__OSF1__) && !defined(IRIX5) && !defined(__NetBSD__)
/* Linux added here by Raymond L. Toy <toy@alydar.crd.ge.com> for Lemacs. */
/* IRIX5 added here by Daniel Rich <drich@lerc.nasa.gov> for lemacs */
/* NetBSD added here by James R Grinter <jrg@doc.ic.ac.uk> for lemacs */
extern int closedir (DIR *dirp);
#else
extern int closedir ();
#endif
#endif

#ifdef NONSYSTEM_DIR_LIBRARY
extern DIR *opendir (CONST char *filename)
extern int closedir (DIR *dirp);
extern struct direct *readdir (DIR *dirp);
extern struct direct *readdirver (DIR *dirp);
#endif


#ifdef VMS
extern void init_vms_input (void);
extern void stop_vms_input (void);

extern int vms_truncate (char *fn);
extern int vlimit (void);       /* BTW, name conflicts with unix syscall */
extern int define_logical_name (char *varname, char *string)
extern int delete_logical_name (char *varname);
extern int rename_sans_version (char *from, char *to);
extern char *vmserrstr (int status);
extern char *sys_translate_vms (char *vfile);
extern char *sys_translate_unix (char *ufile);
extern int creat_copy_attrs (char *old, char *new);
extern int link (char *file, char *new);
#endif /* VMS */

#ifdef AIX
extern void hft_init (void);
extern void hft_reset (void);
#endif

#ifdef BSD_PGRPS
extern int inherited_pgroup;
extern void narrow_foreground_group (void);
extern void widen_foreground_group (void);
#endif /* BSD */

#ifndef HAVE_STRERROR
extern CONST char *strerror (int);
#endif

#endif /* _EMACS_SYSDEP_H_ */
