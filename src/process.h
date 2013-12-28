/* Definitions for asynchronous process control in GNU Emacs.
   Copyright (C) 1985, 1992, 1993 Free Software Foundation, Inc.

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

#ifndef _EMACS_PROCESS_H_
#define _EMACS_PROCESS_H_

#ifndef subprocesses
#undef XPROCESS
#undef CHECK_PROCESS
#define PROCESSP(x) 0
#define Fprocess_status(x) Qnil
#define Fget_process(x) Qnil
#define Fget_buffer_process(x) Qnil
#define kill_buffer_processes(x) 0
#define close_process_descs() 0
#define init_process() 0
extern void wait_without_blocking (void);

#else /* subprocesses */

/* Only process.c needs to know about the guts of this */
struct Lisp_Process;
extern const struct lrecord_implementation lrecord_process[];
#define XPROCESS(a) ((struct Lisp_Process *) XPNTR(a))
#define CHECK_PROCESS(x, i) CHECK_RECORD ((x), lrecord_process, Qprocessp, (i))
#define PROCESSP(x) RECORD_TYPEP ((x), lrecord_process)

#ifdef emacs

extern Lisp_Object Qprocessp;

extern Lisp_Object Fget_process (Lisp_Object name);
extern Lisp_Object Fget_buffer_process (Lisp_Object name);
extern Lisp_Object Fprocessp (Lisp_Object object);
extern Lisp_Object Fprocess_status (Lisp_Object process);
extern Lisp_Object Fkill_process (Lisp_Object process, 
                                  Lisp_Object current_group);
extern Lisp_Object Fdelete_process (Lisp_Object process);
extern Lisp_Object Fopen_network_stream (Lisp_Object name,
					 Lisp_Object buffer,
					 Lisp_Object host,
					 Lisp_Object service);
extern Lisp_Object Fprocess_kill_without_query (Lisp_Object, Lisp_Object);

extern void kill_buffer_processes (Lisp_Object buffer);
extern void close_process_descs (void);

extern void set_process_filter (Lisp_Object proc,
				Lisp_Object filter, int filter_does_read);

#define ChannelMask(n) (1<<(n))

/* True iff we are about to fork off a synchronous process or if we
   are waiting for it.  */
extern int synch_process_alive;

/* Nonzero => this is a string explaining death of synchronous subprocess.  */
extern const char *synch_process_death;

/* If synch_process_death is zero,
   this is exit code of synchronous subprocess.  */
extern int synch_process_retcode;


extern void update_process_status (/* Lisp_Object process,
                                      Lisp_Object status_symbol,
                                      int exit_code, int core_dumped */
                                   );

extern void get_process_file_descriptors (struct Lisp_Process *p,
					  int *infd, int *outfd);

#ifdef HAVE_SOCKETS
extern int network_connection_p (/* Lisp_Object process */);
#else
#define network_connection_p(x) 0
#endif

extern Lisp_Object Qrun, Qexit, Qopen, Qclosed;

/* Report all recent events of a change in process status
   (either run the sentinel or output a message).
   This is done while Emacs is waiting for keyboard input.  */
extern void status_notify ();

extern void deactivate_process (Lisp_Object proc);

#ifdef VMS
extern void create_process (Lisp_Object process, char **new_argv,
                            const char *current_dir);
#endif

extern void child_setup (int in, int out, int err, 
                         char **new_argv, char **env,
                         int set_pgrp, 
                         const char *current_dir);

extern int read_process_output (Lisp_Object proc, int channel);


#endif /* subprocesses */

/* The name of the file open to get a null file, or a data sink.
   VMS, MS-DOS, and OS/2 redefine this.  */
#ifndef NULL_DEVICE
#define NULL_DEVICE "/dev/null"
#endif

/* A string listing the possible suffixes used for executable files,
   separated by colons.  VMS, MS-DOS, and OS/2 redefine this.  */
#ifndef EXEC_SUFFIXES
#define EXEC_SUFFIXES ""
#endif

#endif /* emacs */

#endif /* _EMACS_PROCESS_H_ */
