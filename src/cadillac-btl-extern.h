/*
 * cadillac-btl-extern.h, Module CADILLAC
 *
 * ***************************************************************************
 *
 *        Copyright (C) 1990 by Lucid Inc.,  All Rights Reserved
 *
 * ***************************************************************************
 *
 * Header file for Cadillac backtrace logging external functions.
 *
 * Revision:	23-Jan-92 11:43:29
 *
 * Programmer: Harlan Sexton
 *
 * $Header: cadillac-btl-extern.h,v 100.1 92/04/13 12:09:08 devin Exp $
 *
 * Edit-History:
 *
 * Created: 19-Nov-90 by hbs
 *
 * End-of-Edit-History
 */

/* cadillac-btl.c */

#ifdef EMACS_BTL
extern int emacs_btl_elisp_only_p;
#endif

#ifdef sun4
extern int cadillac_btl_always_ignore_the_o7_register_contents;
#endif

extern int cadillac_start_logging (void);
extern int cadillac_stop_logging (void);
extern int cadillac_terminate_logging (void);
extern int cadillac_initialize_pc_logging 
  (char *outfile, char *execfile, long limit, long interval);
extern int cadillac_initialize_backtrace_logging 
  (char *outfile, char *execfile, long limit, long interval);
extern int cadillac_set_log_signal (int signal);
extern void cadillac_record_backtrace (int skip, int weight);

/* cadillac-btl-process.c */

extern void cadillac_summarize_logging 
  (char *data_file, double cutoff, int depth, 
   char *exec_file, char *root, int collapse_tree);

