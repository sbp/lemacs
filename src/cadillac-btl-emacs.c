/*
 * cadillac-btl-emacs.c, Module CADILLAC
 *
 * ***************************************************************************
 *
 *        Copyright (C) 1990 by Lucid Inc.,  All Rights Reserved
 *
 * ***************************************************************************
 *
 * Elisp interface to btl code.
 *
 * Revision:	24-Jan-92 14:21:29
 *
 * Programmer: Harlan Sexton
 *
 * $Header: cadillac-btl-emacs.c,v 100.2 92/04/18 10:55:15 eb Exp $
 *
 * Edit-History:
 *
 * Created:  8-Nov-90 by hbs
 *
 * End-of-Edit-History
 */

#include "config.h"
#include "lisp.h"
#include "cadillac-btl-extern.h"

#include <a.out.h>
#include <sys/mman.h>

extern char *sys_errlist[];
extern int errno;

/* from emacs.c */
extern Lisp_Object Vexecution_path;

/* "really" defined in debug-malloc.c, but this is what common symbols
   are all about */
int dmb_edition_number;

#define ROUND_UP(val, mod) ((1 + (((unsigned long) (val))/(mod))) * (mod))
#define ROUND_DOWN(val, mod) ((((unsigned long) (val))/(mod)) * (mod))

Lisp_Object VBTL_id_tag;
Lisp_Object QBTL_id_tag;

static Lisp_Object
btl_coerce_string (Lisp_Object string, int default_to_execfile)
{
  if (default_to_execfile &&
      (XTYPE (string) != Lisp_String) &&
      (XTYPE (Vexecution_path) == Lisp_String))
    return Vexecution_path;
  else
    return Fexpand_file_name (string, Qnil);
}

DEFUN ("summarize-logging", Fsummarize_logging, 
       Ssummarize_logging, 1, 6, 0, 
  "Summarize logging output in FILE. Nodes whose weights are less\n\
that optional (float) arg CUTOFF will not appear in the printed tree, \n\
nor will nodes that occur at depth below optional arg MAX_DEPTH. Optional\n\
arg EXECFILE specifies which image to use in processing the data. ROOT \n\
specfies which function to treat as root of the call tree. COLLAPSE-TREE \n\
causes long linear subtrees to be reduced to their leaf node. Returns NIL.")
  (file, cutoff, max_depth, execfile, root, collapse_tree)
Lisp_Object file, cutoff, max_depth, execfile, root;
{
  char *execfile_name = 0;
  char *root_name = 0;

  file = btl_coerce_string (file, 0);
  execfile = btl_coerce_string (execfile, 1);

  /* if limit and interval not set, use the defaults */
  if (XTYPE (cutoff) != Lisp_Float) cutoff = make_float (0.0);
  if (XTYPE (max_depth) != Lisp_Int) XSET (max_depth, Lisp_Int, -1);

  if (!NILP(execfile)) 
    execfile_name = (char *) XSTRING(execfile)->data;

  if (!NILP (root)) 
    {
      if (XTYPE (root) == Lisp_String)
        root_name = (char *) XSTRING(root)->data;
      else if (XTYPE (root) == Lisp_Symbol)
        root_name = (char *) XSYMBOL (root)->name->data;
      else
        error ("ROOT argument %s must be either a string or a symbol",
               XSTRING (Fprin1_to_string (root, Qnil))->data);
    }

  cadillac_summarize_logging ((char *) XSTRING(file)->data,
                              XFLOAT(cutoff)->data, 
                              XINT(max_depth),
                              execfile_name,
                              root_name,
                              (NILP (collapse_tree))?0:1);
  
  return Qnil;
}


DEFUN ("initialize-pc-logging-internal", Finitialize_pc_logging_internal, 
       Sinitialize_pc_logging_internal, 1, 4, 0,
  "Initialize pc logging with output to FILE. Optional arg LIMIT \n\
is the number of stack frames to write to the output file (defaults to a \n\
million), and optional arg INTERVAL is how many micro-seconds to wait \n\
between samples -- on most machines this is a joke since interrupts can't\n\
come faster than once every few milliseconds. (The last optional arg, EMACS, \n\
is provided in case the code that automatically computes the name of the running\n\
image fails somehow.) The function START-LOGGING\n\ causes the data-collection \n\
process to begin, the function STOP-LOGGING stops it, and the function \n\
TERMINATE-LOGGING ends the process and closes the output file. \n\n\
Returns T if successful, NIL if it failed to initialize logging.")
  (file, emacs_execfile, limit, interval)
Lisp_Object file, emacs_execfile, limit, interval;
{
  int return_val;

  file = btl_coerce_string (file, 0);
  emacs_execfile = btl_coerce_string (emacs_execfile, 1);

  /* if limit and interval not set, use the defaults */
  if (NILP (limit)) XSET (limit, Lisp_Int, 0);
  if (NILP (interval)) XSET (interval, Lisp_Int, 0);
  CHECK_NUMBER (limit, 0);
  CHECK_NUMBER (interval, 0);

  return_val = 
    cadillac_initialize_pc_logging ((char *) XSTRING(file)->data, 
                                    (char *) XSTRING(emacs_execfile)->data, 
                                    XINT(limit), 
                                    XINT(interval));
  return (return_val)?Qt:Qnil;
}

DEFUN ("initialize-backtrace-logging-internal", 
       Finitialize_backtrace_logging_internal, 
       Sinitialize_backtrace_logging_internal, 1, 4, 0,
  "Initialize backtrace logging with output to FILE. Optional arg LIMIT \n\
is the number of stack frames to write to the output file (defaults to a \n\
million), and optional arg INTERVAL is how many micro-seconds to wait \n\
between samples -- on most machines this is a joke since interrupts can't\n\
come faster than once every few milliseconds. (The last optional arg, EMACS, \n\
is provided in case the code that automatically computes the name of the running\n\
image fails somehow.) The function START-LOGGING\n\ causes the data-collection \n\
process to begin, the function STOP-LOGGING stops it, and the function \n\
TERMINATE-LOGGING ends the process and closes the output file. \n\n\
Returns T if successful, NIL if it failed to initialize logging.")
    (file, limit, interval, emacs_execfile)
Lisp_Object file, limit, interval, emacs_execfile;
{
  int return_val;

  file = btl_coerce_string (file, 0);
  emacs_execfile = btl_coerce_string (emacs_execfile, 1);

  /* if limit and interval not set, use the defaults */
  if (NILP (limit)) XSET (limit, Lisp_Int, 0);
  if (NILP (interval)) XSET (interval, Lisp_Int, 0);
  CHECK_NUMBER (limit, 0);
  CHECK_NUMBER (interval, 0);

  return_val = 
    cadillac_initialize_backtrace_logging 
      ((char *) XSTRING(file)->data, 
       (char *) XSTRING(emacs_execfile)->data, 
       XINT(limit), 
       XINT(interval));
  return (return_val)?Qt:Qnil;
}


DEFUN ("start-logging", Fstart_logging, Sstart_logging, 0, 0, 0,
 "Start the currently initialized logging process, if there is one. \n\
Returns T if successful, NIL if logging is not initialized.")
    ()
{
  int return_val;

  return_val = cadillac_start_logging ();
  return (return_val)?Qt:Qnil;
}

DEFUN ("stop-logging", Fstop_logging, Sstop_logging, 0, 0, 0,
 "Stop the currently initialized logging process, if there is one. \n\
Returns T if successful, NIL if logging is not initialized.")
    ()
{
  int return_val;

  return_val = cadillac_stop_logging ();
  return (return_val)?Qt:Qnil;
}

DEFUN ("terminate-logging", Fterminate_logging, Sterminate_logging, 0, 0, 0,
 "Terminate the currently initialized logging process, if there is one. \n\
Returns T if successful, NIL if logging is not initialized.")
    ()
{
  int return_val;

  return_val = cadillac_terminate_logging ();
  return (return_val)?Qt:Qnil;
}

DEFUN ("set-log-signal", Fset_log_signal, Sset_log_signal, 0, 1, 0,
 "Set the signal used by logging code to optional arg SIGNAL. If SIGNAL is \n\
or negative, resets to default which is SIGPROF. Only works if logging has \n\
never been started or has been terminated. \n\
Returns T if successful, NIL if not.")
    (signal)
Lisp_Object signal;
{
  int return_val;

  if (NILP (signal)) XSET (signal, Lisp_Int, -1);
  CHECK_NUMBER (signal, 0);

  return_val = cadillac_set_log_signal (XINT(signal));
  return (return_val)?Qt:Qnil;
}

/* Until we switch from ATT malloc to Lucid gmalloc, can't use this. */
#ifndef NCR486
DEFUN ("scan-malloc-heap", Fscan_malloc_heap, Sscan_malloc_heap, 0, 1, 0,
 "Scan the malloc heap for non-free'd objects with level number >= LEVEL. \n\
With no arg defaults to the current number. Make sure that BTL is on.")
    (level)
Lisp_Object level;
{
  if (NILP (level)) 
    XSET (level, Lisp_Int, dmb_edition_number);

  CHECK_NUMBER (level, 0);

  malloc_scan_heap (XINT(level));
  return Qnil;
}
#endif /* ! NCR486 */


void
syms_of_cadillac_btl_emacs() 
{
  defsubr(&Sinitialize_backtrace_logging_internal);
  defsubr(&Sinitialize_pc_logging_internal);
  defsubr(&Sstart_logging);
  defsubr(&Sstop_logging);
  defsubr(&Sterminate_logging);
  defsubr(&Ssummarize_logging);
  defsubr(&Sset_log_signal);

#ifndef NCR486
  defsubr(&Sscan_malloc_heap);
#endif /* ! NCR486 */

  QBTL_id_tag = intern ("cadillac-id-tag");
  staticpro (&QBTL_id_tag);
  VBTL_id_tag = QBTL_id_tag;
  Fset (QBTL_id_tag, QBTL_id_tag);

  DEFVAR_BOOL ("elisp-only-btl", &emacs_btl_elisp_only_p,
               "variable used to control backtrace logging being elisp only");

#ifdef sun4
  DEFVAR_BOOL ("ignore-o7-reg", 
               &cadillac_btl_always_ignore_the_o7_register_contents,
               "ignore the o7 reg in btl leaves -- set to NIL for optimized code");
  cadillac_btl_always_ignore_the_o7_register_contents = 1;
#endif

  DEFVAR_INT ("malloc-level-number", &dmb_edition_number,
              "variable used to mark malloc levels");
  dmb_edition_number = 0;
}
