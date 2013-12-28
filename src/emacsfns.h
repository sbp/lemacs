/* Various function declarations for GNU Emacs.
   Used to be part of lisp.h
   Copyright (C) 1985, 1986, 1987, 1992, 1993, 1994
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

#ifndef _EMACSFNS_H_
#define _EMACSFNS_H_

/* defined in sysdep.c, also declared in sysdep.h.
   (These may be called via the emacs_open, etc macros instead...)
 */

extern int sys_open (CONST char *path, int oflag, int mode);
extern int sys_close (int fd);
extern int sys_read (int fildes, void *buf, unsigned int nbyte);
extern int sys_write (int fildes, CONST void *buf, unsigned int nbyte);
extern int sys_access (CONST char *path, int mode);
unsigned int sys_getuid (void);

struct buffer;                  /* "buffer.h" */
struct screen;                  /* "screen.h" */
struct window;                  /* "window.h" */
struct Lisp_Event;              /* "events.h" */
struct Lisp_Process;            /* "process.c" */
struct stat;                    /* <sys/stat.h> */

extern void ring_bell (Lisp_Object sound);

/* Defined in doprnt.c */
#include <stdarg.h>             /* for va_list */
extern int emacs_doprnt (char *buffer, int bufsize, 
                         CONST char *format, int format_length,
                         int nargs, va_list vargs);
extern int emacs_doprnt_lisp (char *buffer, int bufsize, 
			      CONST char *format, int format_length,
			      int nargs, Lisp_Object *largs);

/* Defined in data.c */
extern Lisp_Object Qnil, Qt, Qquote, Qlambda, Qfunction, Qunbound;
extern Lisp_Object Qerror_conditions, Qerror_message, Qtop_level, Qsignal;
extern Lisp_Object Qerror, Qquit, Qwrong_type_argument, Qargs_out_of_range;
extern Lisp_Object Qvoid_variable, Qvoid_function;
extern Lisp_Object Qsetting_constant, Qinvalid_read_syntax;
extern Lisp_Object Qinvalid_function, Qwrong_number_of_arguments, Qno_catch;
extern Lisp_Object Qend_of_file, Qarith_error;
extern Lisp_Object Qrange_error, Qdomain_error, Qsingularity_error;
extern Lisp_Object Qsingularity_error, Qoverflow_error, Qunderflow_error;
extern Lisp_Object Qbeginning_of_buffer, Qend_of_buffer, Qbuffer_read_only;

extern Lisp_Object Qintegerp, Qnatnump, Qsymbolp, Qlistp, Qconsp;
extern Lisp_Object Qstringp, Qarrayp, Qsequencep, Qbufferp, Qsubrp;
extern Lisp_Object Qchar_or_string_p, Qmarkerp, Qvectorp;
extern Lisp_Object Qinteger_or_marker_p, Qboundp, Qfboundp;
extern Lisp_Object Qcdr, Qdefault, Qignore;

extern Lisp_Object Qnumberp, Qnumber_or_marker_p;

extern Lisp_Object Qvalues;
extern Lisp_Object Qeval;
extern Lisp_Object Qeventp;
extern Lisp_Object Qstring_or_buffer_p;

extern Lisp_Object Qfloatp;
extern Lisp_Object Ftruncate (Lisp_Object n);

extern Lisp_Object Flistp (Lisp_Object x);

extern Lisp_Object Fcar (Lisp_Object cons), Fcar_safe (Lisp_Object cons);
extern Lisp_Object Fcdr (Lisp_Object cons), Fcdr_safe (Lisp_Object cons);
extern Lisp_Object Fsetcar (Lisp_Object cons, Lisp_Object val);
extern Lisp_Object Fsetcdr (Lisp_Object cons, Lisp_Object val);
extern Lisp_Object Faref (Lisp_Object array, Lisp_Object idx);
extern Lisp_Object Faset (Lisp_Object array, Lisp_Object idx, Lisp_Object x);
extern Lisp_Object Farray_length (Lisp_Object array);
extern Lisp_Object Felt (Lisp_Object seq, Lisp_Object idx);

extern Lisp_Object Fboundp (Lisp_Object sym);
extern Lisp_Object Ffboundp (Lisp_Object);
extern Lisp_Object Ffset (Lisp_Object sym, Lisp_Object val);
extern Lisp_Object Fsymbol_plist (Lisp_Object sym);
extern Lisp_Object Fsetplist (Lisp_Object sym, Lisp_Object val);
extern Lisp_Object Fsymbol_function (Lisp_Object sym);
extern Lisp_Object Fsymbol_value (Lisp_Object sym);
extern Lisp_Object Fdefault_value (Lisp_Object sym);
extern Lisp_Object Fdefault_boundp (Lisp_Object sym);
extern Lisp_Object Fset (Lisp_Object sym, Lisp_Object val);
extern Lisp_Object Fset_default (Lisp_Object sym, Lisp_Object val);
extern Lisp_Object Fsymbol_name (Lisp_Object sym);
extern Lisp_Object Findirect_function (Lisp_Object object);
extern Lisp_Object indirect_function (Lisp_Object object, int error);
extern Lisp_Object symbol_value_in_buffer (Lisp_Object sym, Lisp_Object buf);
extern void decache_buffer_local_variables (struct buffer *buf);
extern void store_symval_forwarding (Lisp_Object sym,
                                     Lisp_Object valcontents, 
                                     Lisp_Object newval);
extern Lisp_Object Fmake_local_variable (Lisp_Object object);
extern Lisp_Object Fzerop (Lisp_Object);
extern Lisp_Object Fnumber_to_string (Lisp_Object num);
extern Lisp_Object Fstring_to_number (Lisp_Object str);

#ifndef make_number
extern Lisp_Object make_number (LISP_WORD_TYPE);
#endif
extern DOESNT_RETURN pure_write_error (void);
extern DOESNT_RETURN args_out_of_range (Lisp_Object, Lisp_Object);
extern DOESNT_RETURN args_out_of_range_3 (Lisp_Object, Lisp_Object, Lisp_Object);
extern Lisp_Object wrong_type_argument (Lisp_Object pred, Lisp_Object value);

extern Lisp_Object long_to_cons (unsigned long);
extern unsigned long cons_to_long (Lisp_Object);
extern Lisp_Object word_to_lisp (unsigned int);
extern unsigned int lisp_to_word (Lisp_Object);

extern Lisp_Object Fcompiled_function_instructions (Lisp_Object function);
extern Lisp_Object Fcompiled_function_constants (Lisp_Object function);
extern Lisp_Object Fcompiled_function_stack_depth (Lisp_Object function);
extern Lisp_Object Fcompiled_function_arglist (Lisp_Object function);
extern Lisp_Object Fcompiled_function_interactive (Lisp_Object function);
extern Lisp_Object Fcompiled_function_domain (Lisp_Object function);

extern Lisp_Object Fquo (int nargs, Lisp_Object *args);
extern Lisp_Object Fsub1 (Lisp_Object num);
extern Lisp_Object Fadd1 (Lisp_Object num);
extern Lisp_Object Fgtr (Lisp_Object num1, Lisp_Object num2);
extern Lisp_Object Flss (Lisp_Object num1, Lisp_Object num2);
extern Lisp_Object Fleq (Lisp_Object num1, Lisp_Object num2);
extern Lisp_Object Fgeq (Lisp_Object num1, Lisp_Object num2);
extern Lisp_Object Fminus (int nargs, Lisp_Object *args);
extern Lisp_Object Fplus (int nargs, Lisp_Object *args);
extern Lisp_Object Fmin (int nargs, Lisp_Object *args);
extern Lisp_Object Fmax (int nargs, Lisp_Object *args);
extern Lisp_Object Ftimes (int nargs, Lisp_Object *args);
extern Lisp_Object Frem (Lisp_Object num1, Lisp_Object num2);

/* defined in floatfns.c */
extern double extract_float (Lisp_Object);
extern Lisp_Object Ffloat (Lisp_Object n);


/* Defined in fns.c */

extern Lisp_Object list_sort (Lisp_Object list, 
                              Lisp_Object lisp_arg,
                              int (*pred_fn) (Lisp_Object first,
                                              Lisp_Object second,
                                              Lisp_Object lisp_arg));
extern Lisp_Object Fsort (Lisp_Object list, 
                          Lisp_Object pred);
extern Lisp_Object merge (Lisp_Object org_l1, Lisp_Object org_l2,
                          Lisp_Object pred);

extern void run_hook_with_args (Lisp_Object hook_var, int nargs, ...);

extern Lisp_Object Qstring_lessp, Qyes_or_no_p;
extern Lisp_Object Vfeatures;
extern Lisp_Object Fidentity (Lisp_Object x);
extern Lisp_Object Frandom (Lisp_Object arg);
extern Lisp_Object Flength (Lisp_Object seq);
extern Lisp_Object Fappend (int nargs, Lisp_Object *args);
extern Lisp_Object Fconcat (int nargs, Lisp_Object *args);
extern Lisp_Object Fvconcat (int nargs, Lisp_Object *args);
extern Lisp_Object Fcopy_sequence (Lisp_Object seq);
extern Lisp_Object Fsubstring (Lisp_Object str, Lisp_Object s, Lisp_Object e);
extern Lisp_Object Fnthcdr (Lisp_Object n, Lisp_Object list);
extern Lisp_Object Fmemq (Lisp_Object elt, Lisp_Object list);
extern Lisp_Object Fassq (Lisp_Object key, Lisp_Object alist);
extern Lisp_Object Fassoc (Lisp_Object elt, Lisp_Object list);
extern Lisp_Object Fdelete (Lisp_Object elt, Lisp_Object list);
extern Lisp_Object Fmember (Lisp_Object elt, Lisp_Object list);
extern Lisp_Object Frassq (Lisp_Object key, Lisp_Object alist);
extern Lisp_Object Fdelq (Lisp_Object elt, Lisp_Object list);
extern Lisp_Object memq_no_quit (Lisp_Object elt, Lisp_Object list);
extern Lisp_Object delq_no_quit (Lisp_Object elt, Lisp_Object list);
extern Lisp_Object assq_no_quit (Lisp_Object key, Lisp_Object alist);
extern Lisp_Object rassq_no_quit (Lisp_Object key, Lisp_Object alist);
extern Lisp_Object assoc_no_quit (Lisp_Object key, Lisp_Object alist);
extern Lisp_Object Freverse (Lisp_Object list), Fnreverse (Lisp_Object list);
extern Lisp_Object Fget (Lisp_Object sym, Lisp_Object prop, Lisp_Object def);
extern Lisp_Object Fput (Lisp_Object sym, Lisp_Object prop, Lisp_Object val);

extern void pure_put (Lisp_Object sym, Lisp_Object prop, Lisp_Object val);
extern Lisp_Object Fremprop (Lisp_Object sym, Lisp_Object prop);
extern Lisp_Object Fequal (Lisp_Object one, Lisp_Object two);
extern int internal_equal (Lisp_Object, Lisp_Object, int depth);
extern Lisp_Object Ffillarray (Lisp_Object array, Lisp_Object init);
extern Lisp_Object Fnconc (int nargs, Lisp_Object *args);
extern Lisp_Object Fmapcar (Lisp_Object fn, Lisp_Object seq);
extern Lisp_Object Ffeaturep (Lisp_Object name);
extern Lisp_Object Frequire (Lisp_Object name, Lisp_Object filename);
extern Lisp_Object Fprovide (Lisp_Object name);
extern Lisp_Object concat2 (Lisp_Object s1, Lisp_Object s2);
extern Lisp_Object nconc2 (Lisp_Object l1, Lisp_Object l2);
extern Lisp_Object Fcopy_alist (Lisp_Object alist);

extern Lisp_Object Fplay_sound (Lisp_Object sound, Lisp_Object volume);
extern void bitch_at_user (Lisp_Object sound);
extern int not_on_console;


/* Defined in alloc.c */
extern void release_breathing_space (void);
extern void uninterrupt_malloc (void);
extern Lisp_Object Fcons (Lisp_Object car, Lisp_Object cdr);
extern Lisp_Object Flist (int nargs, Lisp_Object *args);
extern Lisp_Object Fmake_list (Lisp_Object length, Lisp_Object init);
extern Lisp_Object Fmake_vector (Lisp_Object length, Lisp_Object init);
extern Lisp_Object make_vector (int length, Lisp_Object init);
extern Lisp_Object Fvector (int nargs, Lisp_Object *args);
extern Lisp_Object Fmake_symbol (Lisp_Object name);
extern Lisp_Object Fmake_marker (void);
extern Lisp_Object Fmake_string (Lisp_Object length, Lisp_Object init);
extern void garbage_collect_1 (void);
extern Lisp_Object Fgarbage_collect (void);
extern Lisp_Object list1 (Lisp_Object);
extern Lisp_Object list2 (Lisp_Object, Lisp_Object);
extern Lisp_Object list3 (Lisp_Object, Lisp_Object, Lisp_Object);
extern Lisp_Object list4 (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object);
extern Lisp_Object list5 (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object,
			  Lisp_Object);

extern void malloc_warning (CONST char *);
extern DOESNT_RETURN memory_full (void);
extern void *xmalloc (int size);
extern void *xrealloc (void *, int size);
extern void xfree (void *);
extern char *xstrdup (CONST char *);
extern void disksave_object_finalisation (void);
extern int purify_flag;
extern int gc_in_progress;
extern int gc_currently_forbidden;
extern int gc_generation_number[1];
extern int purified (Lisp_Object);

extern Lisp_Object build_string (CONST char *);
extern Lisp_Object make_string (CONST char *, int);
extern Lisp_Object make_uninit_string (int length);

extern Lisp_Object make_float (double float_value);

extern Lisp_Object Fmake_byte_code (int nargs, Lisp_Object *args);

extern Lisp_Object Fpurecopy (Lisp_Object);
extern void report_pure_usage (int report_impurities);
extern Lisp_Object make_pure_string (CONST char *, int len, int nocopy);
extern Lisp_Object make_pure_pname (CONST char *, int len, int nocopy);
extern Lisp_Object pure_cons (Lisp_Object, Lisp_Object);
extern Lisp_Object make_pure_vector (int len, Lisp_Object init);

extern void free_cons (struct Lisp_Cons *ptr);

#ifdef LISP_FLOAT_TYPE
extern Lisp_Object make_pure_float (double float_value);
/* extern void free_float (struct Lisp_Float *); */
#endif

/* Defined in print.c */
extern Lisp_Object Vprin1_to_string_buffer;
extern Lisp_Object Fprin1 (Lisp_Object obj, Lisp_Object printcharfun);
extern Lisp_Object Fprinc (Lisp_Object obj, Lisp_Object printcharfun);
extern Lisp_Object Fprint (Lisp_Object obj, Lisp_Object printcharfun);
extern Lisp_Object Fprin1_to_string (Lisp_Object obj, Lisp_Object noescape);
extern Lisp_Object Fterpri (Lisp_Object printcharfun);
extern Lisp_Object Vstandard_output, Qstandard_output;
extern Lisp_Object Qexternal_debugging_output;
extern void temp_output_buffer_setup (CONST char *bufname);
extern void temp_output_buffer_show (Lisp_Object buf, Lisp_Object same_scrn);
/* NOTE:  Do not call this with the data of a Lisp_String.  Use princ.
 * Note: stream should be defaulted before calling
 *  (eg Qnil means stdout, not Vstandard_output, etc) */
extern void write_string_1 (CONST char *s, int size, 
                            Lisp_Object printcharfun);
extern void print_internal (Lisp_Object obj, 
                            Lisp_Object printcharfun, 
                            int escapeflag);
extern Lisp_Object Vprint_level;
extern Lisp_Object Vprint_length;
extern int print_escape_newlines;
extern int print_readably;
extern Lisp_Object Qprint_escape_newlines;
extern Lisp_Object internal_with_output_to_temp_buffer
  (CONST char *bufname, 
   Lisp_Object (*function) (Lisp_Object args),
   Lisp_Object args,
   Lisp_Object same_screen);
extern void float_to_string (char *buf, double data);
extern void print_symbol (Lisp_Object, Lisp_Object stream, int escapeflag);
extern void print_bytecode (Lisp_Object, Lisp_Object stream, int escapeflag);
extern void print_float (Lisp_Object, Lisp_Object stream, int escapeflag);
extern void clear_output_stream_resource (void);

/* Defined in intl.c */
extern Lisp_Object Fignore_defer_gettext (Lisp_Object obj);
extern Lisp_Object Fgettext (Lisp_Object string);
extern Lisp_Object Fdgettext (Lisp_Object domain, Lisp_Object string);
extern Lisp_Object Fdomain (Lisp_Object);

/* Defined in lread.c */
extern Lisp_Object Qvariable_documentation, Qstandard_input, Qread_char;
extern Lisp_Object Qvariable_domain; /* I18N3 */
extern Lisp_Object Qload;
extern Lisp_Object Vobarray, Vstandard_input;
extern Lisp_Object Vvalues;
extern Lisp_Object Fread (Lisp_Object readcharfun);
extern Lisp_Object Fread_from_string (Lisp_Object string, 
                                      Lisp_Object start, Lisp_Object end);
extern Lisp_Object Fintern (Lisp_Object str, Lisp_Object obarray);
extern Lisp_Object Fintern_soft (Lisp_Object str, Lisp_Object obarray);
extern Lisp_Object Fload (Lisp_Object filename, Lisp_Object missing_ok,
                          Lisp_Object nomessage, Lisp_Object nosuffix);
extern int locate_file (Lisp_Object path, 
                        Lisp_Object str, CONST char *suffix, 
                        Lisp_Object *storeptr, int mode);
extern int hash_string (CONST unsigned char *, int len);
extern Lisp_Object intern (CONST char *);
extern Lisp_Object oblookup (Lisp_Object obarray,
			     CONST unsigned char *, int len);
extern void map_obarray (Lisp_Object obarray, 
                         void (*fn) (Lisp_Object sym, Lisp_Object arg),
                         Lisp_Object arg);
extern int isfloat_string (CONST char *);

#ifdef LOADHIST /* this is just a stupid idea */
#define LOADHIST_ATTACH(x) \
 do { if (initialized) Vcurrent_load_list = Fcons (x, Vcurrent_load_list); } \
 while (0)
extern Lisp_Object Vcurrent_load_list;
extern Lisp_Object Vload_history;
#else /*! LOADHIST */
# define LOADHIST_ATTACH(x)
#endif /*! LOADHIST */


/* Defined in editfns.c */
extern int clip_to_bounds (int lower, int num, int upper);
extern Lisp_Object Fwiden (void);
extern Lisp_Object Fnarrow_to_region (Lisp_Object b, Lisp_Object e);
extern Lisp_Object Vprefix_arg, Vcurrent_prefix_arg;
extern Lisp_Object Qminus, Qcurrent_prefix_arg;
extern Lisp_Object Fgoto_char (Lisp_Object pos);
extern Lisp_Object Fpoint_min_marker (void), Fpoint_max_marker (void);
extern Lisp_Object Fpoint_min (void), Fpoint_max (void);
extern Lisp_Object Fpoint (void);
extern Lisp_Object Fpoint_marker (Lisp_Object dont_copy_p);
extern Lisp_Object Fmark_marker (Lisp_Object inactive_p);
extern Lisp_Object Ffollowing_char (void), Fprevious_char (void);
extern Lisp_Object Fchar_after (Lisp_Object pos);
extern Lisp_Object Finsert (int nargs, Lisp_Object *args);
extern Lisp_Object Finsert_char (Lisp_Object ch, Lisp_Object count);
extern void insert1 (Lisp_Object arg);
extern Lisp_Object Finsert_before_markers (int nargs, Lisp_Object *args);
extern Lisp_Object Finsert_buffer_substring (Lisp_Object buffer, 
                                             Lisp_Object b, Lisp_Object e);
extern Lisp_Object Fdelete_region (Lisp_Object b, Lisp_Object e);
extern Lisp_Object Feolp (void), Feobp (void), Fbolp (void), Fbobp (void);
extern Lisp_Object Fformat (int nargs, Lisp_Object *args);
extern Lisp_Object format1 (CONST char *fmt, ...); /* ... Lisp_Object */
extern Lisp_Object Fbuffer_substring (Lisp_Object start, Lisp_Object end);
extern Lisp_Object Fbuffer_string (void);
extern Lisp_Object make_string_from_buffer (struct buffer *buf,
                                            int index, int length);
extern Lisp_Object Fstring_equal (Lisp_Object s1, Lisp_Object s2);
extern Lisp_Object Fstring_lessp (Lisp_Object s1, Lisp_Object s2);
extern Lisp_Object save_excursion_save (void), save_restriction_save (void);
extern Lisp_Object save_excursion_restore (Lisp_Object info);
extern Lisp_Object save_restriction_restore (Lisp_Object info);
extern Lisp_Object Fchar_to_string (Lisp_Object ch);
extern Lisp_Object Fzmacs_activate_region (void);
extern Lisp_Object Fzmacs_deactivate_region (void);
extern Lisp_Object Fcurrent_time_seconds (Lisp_Object cons);
extern Lisp_Object Fgetenv (Lisp_Object var, Lisp_Object interactivep);
extern Lisp_Object Qpoint, Qmark, Qregion_beginning, Qregion_end;
extern Lisp_Object Qformat;


/* Defined in cmds.c */
extern Lisp_Object Fforward_char (Lisp_Object n);
extern Lisp_Object Fforward_line (Lisp_Object n);
extern Lisp_Object Fend_of_line (Lisp_Object n);
extern Lisp_Object Fbeginning_of_line (Lisp_Object n);
extern Lisp_Object Qself_insert_command;


/* Defined in buffer.c */
extern void validate_region (Lisp_Object *beginning, Lisp_Object *end);
extern void record_buffer (Lisp_Object buf);
extern Lisp_Object Fset_buffer_left_margin_width (Lisp_Object width,
						 Lisp_Object buffer);
extern Lisp_Object Fset_buffer_right_margin_width (Lisp_Object width,
						  Lisp_Object buffer);
extern Lisp_Object Fbuffer_left_margin_width (Lisp_Object buffer);
extern Lisp_Object Fbuffer_right_margin_width (Lisp_Object buffer);
extern Lisp_Object Ferase_buffer (void);
extern Lisp_Object Fbuffer_disable_undo (Lisp_Object buffer);
extern Lisp_Object Fkill_buffer (Lisp_Object buffer);
extern Lisp_Object Fbuffer_name (Lisp_Object buffer);
extern Lisp_Object get_buffer (Lisp_Object name,
                               int error_if_deleted_or_does_not_exist);
extern Lisp_Object Fget_buffer (Lisp_Object name);
extern Lisp_Object Fget_buffer_create (Lisp_Object name);
extern Lisp_Object Fget_file_buffer (Lisp_Object fn);
extern Lisp_Object Fset_buffer (Lisp_Object buffer);
extern Lisp_Object Fbarf_if_buffer_read_only (void);
extern Lisp_Object Fcurrent_buffer (void);
extern Lisp_Object Fswitch_to_buffer (Lisp_Object buf, Lisp_Object norecord);
extern Lisp_Object Fpop_to_buffer (Lisp_Object bufname, Lisp_Object other,
                                   Lisp_Object same_screen);
extern Lisp_Object Fother_buffer (Lisp_Object buffer, Lisp_Object screen);
extern Lisp_Object Fbuffer_list (Lisp_Object screen);
extern Lisp_Object Fset_buffer_modified_p (Lisp_Object flag);
extern Lisp_Object QSscratch;   /* "*scratch*" */
extern Lisp_Object Qbuffer_file_name, Qbuffer_undo_list;
extern Lisp_Object Qdefault_directory;
extern int find_file_compare_truenames;
extern int find_file_use_truenames;

/* Functions to call before and after each text change. */
extern Lisp_Object Vbefore_change_function;
extern Lisp_Object Qbefore_change_function;
extern Lisp_Object Vafter_change_function;
extern Lisp_Object Qafter_change_function;
extern Lisp_Object Vfirst_change_hook;
extern Lisp_Object Qfirst_change_hook;
extern Lisp_Object Vinhibit_read_only;

extern Lisp_Object Qpermanent_local, Qprotected_field;


/* defined in marker.c */
extern int marker_position (Lisp_Object marker);
extern void unchain_marker (Lisp_Object marker);
extern Lisp_Object Fset_marker (Lisp_Object marker, 
                                Lisp_Object pos, Lisp_Object buffer);
extern Lisp_Object Fmarker_position (Lisp_Object m);
extern Lisp_Object Fmarker_buffer (Lisp_Object m);
extern Lisp_Object Fcopy_marker (Lisp_Object m);
extern Lisp_Object set_marker_restricted (Lisp_Object marker,
                                          Lisp_Object pos, Lisp_Object buf);
extern Lisp_Object Fbuffer_modified_p (Lisp_Object buffer);
extern Lisp_Object Fgenerate_new_buffer_name (Lisp_Object name,
                                              Lisp_Object ignore);
extern Lisp_Object Frename_buffer (Lisp_Object name, Lisp_Object unique);

/* defined in fileio.c */
extern Lisp_Object Qfile_name_handler_alist;
extern Lisp_Object Qfile_error;
extern Lisp_Object Ffile_name_as_directory (Lisp_Object fn);
extern Lisp_Object Fexpand_file_name (Lisp_Object fn, Lisp_Object def);
extern Lisp_Object Ffile_name_nondirectory (Lisp_Object fn);
extern Lisp_Object Fsubstitute_in_file_name (Lisp_Object fn);
extern Lisp_Object Ffile_symlink_p (Lisp_Object fn);
extern Lisp_Object Ffile_truename (Lisp_Object name, Lisp_Object def);
extern Lisp_Object Ffile_name_nondirectory (Lisp_Object fn);
extern Lisp_Object Ffile_name_directory (Lisp_Object fn);
extern Lisp_Object Fdirectory_file_name (Lisp_Object fn);
extern Lisp_Object Fdirectory_file_name (Lisp_Object fn);
extern Lisp_Object Ffile_directory_p (Lisp_Object fn);
extern Lisp_Object Ffile_readable_p (Lisp_Object fn);
extern Lisp_Object Ffile_name_absolute_p (Lisp_Object fn);
extern Lisp_Object Ffile_exists_p (Lisp_Object fn);
extern Lisp_Object Ffile_executable_p (Lisp_Object filename);
extern Lisp_Object Ffile_accessible_directory_p (Lisp_Object fn);
extern void record_auto_save (void);
extern Lisp_Object Ffind_file_name_handler (Lisp_Object filename);
extern DOESNT_RETURN report_file_error (CONST char *string, Lisp_Object data);
extern Lisp_Object expand_and_dir_to_file (Lisp_Object fn, Lisp_Object def);
extern Lisp_Object Fwrite_region (Lisp_Object start, Lisp_Object end,
                                  Lisp_Object filename, 
                                  Lisp_Object append, Lisp_Object visit);
extern Lisp_Object Fdo_auto_save (Lisp_Object nomsg, Lisp_Object current_only);
extern Lisp_Object Fverify_visited_file_modtime (Lisp_Object buffer);
extern Lisp_Object Funhandled_file_name_directory (Lisp_Object filename);
extern Lisp_Object Fset_buffer_modtime (Lisp_Object buf, Lisp_Object in_time);

/* defined in filelock.c */
extern void lock_file (Lisp_Object fn);
extern void unlock_file (Lisp_Object fn);
extern void unlock_all_files (void);
extern Lisp_Object Flock_buffer (Lisp_Object fn);
extern Lisp_Object Funlock_buffer (void);
extern void unlock_buffer (struct buffer *buffer);
extern Lisp_Object Ffile_locked_p (Lisp_Object fn);


/* defined in event*.c */
extern Lisp_Object Fread_key_sequence (Lisp_Object prompt);
extern Lisp_Object Fsit_for (Lisp_Object seconds, Lisp_Object nodisp);
extern Lisp_Object Fsleep_for (Lisp_Object seconds);
extern Lisp_Object Faccept_process_output (Lisp_Object process,
					   Lisp_Object timeout_secs,
					   Lisp_Object timeout_msecs);
extern Lisp_Object Fnext_event_1 (Lisp_Object event, Lisp_Object prompt);
extern Lisp_Object Fnext_command_event (Lisp_Object event);
extern Lisp_Object Fdispatch_event (Lisp_Object event);
extern void wait_delaying_user_input (int (*predicate) (void *arg),
                                      void *predicate_arg);
extern int detect_input_pending (void);
extern void enqueue_command_event (Lisp_Object event);

extern Lisp_Object Fallocate_event (void);
extern Lisp_Object Fdeallocate_event (Lisp_Object event);
extern Lisp_Object Fcopy_event (Lisp_Object from, Lisp_Object to);

extern Lisp_Object make_event (void);
extern void clear_event_resource (void);

extern Lisp_Object Fcharacter_to_event (Lisp_Object ch, Lisp_Object event);
extern Lisp_Object Fevent_to_character (Lisp_Object e,
					Lisp_Object allow_extra_modifiers,
					Lisp_Object allow_meta,
					Lisp_Object allow_non_ascii);

extern Lisp_Object Fevent_window (Lisp_Object event);
extern Lisp_Object Fevent_buffer (Lisp_Object event);
extern Lisp_Object Fevent_screen (Lisp_Object event);
extern Lisp_Object Fevent_button (Lisp_Object event);
extern Lisp_Object Fevent_function (Lisp_Object event);
extern Lisp_Object Fevent_glyph (Lisp_Object event);
extern Lisp_Object Fevent_key (Lisp_Object event);
extern Lisp_Object Fevent_modifiers (Lisp_Object event);
extern Lisp_Object Fevent_modifier_bits (Lisp_Object event);
extern Lisp_Object Fevent_object (Lisp_Object event);
extern Lisp_Object Fevent_point (Lisp_Object event);
extern Lisp_Object Fevent_process (Lisp_Object event);
extern Lisp_Object Fevent_timestamp (Lisp_Object event);
extern Lisp_Object Fevent_x (Lisp_Object event);
extern Lisp_Object Fevent_y (Lisp_Object event);
extern Lisp_Object Fevent_x_pixel (Lisp_Object event);
extern Lisp_Object Fevent_y_pixel (Lisp_Object event);


extern Lisp_Object Fadd_timeout (Lisp_Object secs, 
                                 Lisp_Object function, Lisp_Object object, 
                                 Lisp_Object resignal);
extern Lisp_Object Fdisable_timeout (Lisp_Object id); 

extern Lisp_Object QKbackspace, QKtab, QKlinefeed, QKreturn;
extern Lisp_Object QKescape, QKspace, QKdelete, QKnosymbol;

/* Defined in event-stream.c */
extern Lisp_Object reset_this_command_keys (Lisp_Object dummy);
extern Lisp_Object event_binding (Lisp_Object event0);
extern int event_to_character (struct Lisp_Event *event,
			       int allow_extra_modifiers,
			       int allow_meta,
			       int allow_non_ascii);
extern Lisp_Object Fenqueue_command_event (Lisp_Object, Lisp_Object);

/* defined in keymap.c */
extern Lisp_Object Fcurrent_local_map (void);
extern void where_is_to_char (Lisp_Object definition, 
                              Lisp_Object local_map, Lisp_Object global_map,
                              char *buf);
extern Lisp_Object Fkeymapp (Lisp_Object);
extern void describe_map_tree (Lisp_Object startmap, int partial,
                               Lisp_Object shadow, int mice_only_p);
extern Lisp_Object Fmake_sparse_keymap (void);
extern Lisp_Object Fkeymap_fullness (Lisp_Object keymap);
extern Lisp_Object Fkey_description (Lisp_Object key);
extern Lisp_Object Fsingle_key_description (Lisp_Object key);
extern Lisp_Object Ftext_char_description (Lisp_Object c);
extern Lisp_Object Qmode_line_map;
extern Lisp_Object Vsingle_space_string;
extern Lisp_Object Qcontrol, Qctrl, Qmeta, Qsuper, Qhyper, Qsymbol, Qshift;
extern Lisp_Object Qkeymap, Qkeymapp;

/* Defined in elhash.c */
extern Lisp_Object Fhashtablep (Lisp_Object obj);
extern Lisp_Object Fmake_hashtable (Lisp_Object size);
extern Lisp_Object Fcopy_hashtable (Lisp_Object old_table);
extern Lisp_Object Fgethash (Lisp_Object key, Lisp_Object table,
                             Lisp_Object def);
extern Lisp_Object Fremhash (Lisp_Object key, Lisp_Object table);
extern Lisp_Object Fputhash (Lisp_Object key, Lisp_Object val,
                             Lisp_Object table);
extern Lisp_Object Fclrhash (Lisp_Object table);
extern Lisp_Object Fhashtable_fullness (Lisp_Object table);
extern Lisp_Object Fmaphash (Lisp_Object function, Lisp_Object table);

extern Lisp_Object Vcharacter_set_property;


/* Defined in abbrev.c */
extern Lisp_Object Vfundamental_mode_abbrev_table;
extern Lisp_Object Fexpand_abbrev (void);


/* defined in search.c */
struct re_pattern_buffer;
struct re_registers;
extern Lisp_Object Fstring_match (Lisp_Object regexp,
                                  Lisp_Object string, Lisp_Object start);
extern Lisp_Object Fmatch_beginning (Lisp_Object n);
extern Lisp_Object Fmatch_end (Lisp_Object n);
extern Lisp_Object Fskip_chars_forward (Lisp_Object string, Lisp_Object lim);
extern Lisp_Object Fskip_chars_backward (Lisp_Object string, Lisp_Object lim);
extern int scan_buffer (struct buffer *buf,
			int target, int pos, int cnt, int *shortage);
extern int find_next_newline (struct buffer *buf, int from, int cnt);
extern void compile_pattern (Lisp_Object pattern, 
                             struct re_pattern_buffer *bufp, 
                             struct re_registers *regp,
                             char *translate);
int fast_string_match (Lisp_Object regexp, Lisp_Object string);
extern Lisp_Object Fre_search_forward (Lisp_Object string, Lisp_Object bound,
                                       Lisp_Object noerror, Lisp_Object count);

/* defined in syntax.c */
extern int scan_words (int from, int count);
extern Lisp_Object Fforward_word (Lisp_Object n);


/* defined in minibuf.c */
extern int minibuf_level;
extern int scmp (CONST unsigned char *s1, CONST unsigned char *s2, int len);
extern Lisp_Object Fread_from_minibuffer (Lisp_Object prompt, 
                                          Lisp_Object init,
                                          Lisp_Object keymap,
                                          Lisp_Object read_crock,
                                          Lisp_Object hist);
extern int completion_ignore_case;
extern Lisp_Object Qcompletion_ignore_case;

extern Lisp_Object Vminibuffer_zero;


/* Defined in callint.c */
extern Lisp_Object Vcommand_history;
extern Lisp_Object Qcall_interactively;
extern Lisp_Object Fcall_interactively (Lisp_Object fn, Lisp_Object record);
extern Lisp_Object Fprefix_numeric_value (Lisp_Object prefix);
extern Lisp_Object Qread_from_minibuffer;
extern Lisp_Object Qenable_recursive_minibuffers;
extern Lisp_Object Qcompleting_read;
extern Lisp_Object Qread_file_name;
extern Lisp_Object Qread_directory_name;
extern Lisp_Object Qread_buffer;


/* defined in casefiddle.c */
extern Lisp_Object Fupcase (Lisp_Object obj);
extern Lisp_Object Fdowncase (Lisp_Object obj);
extern Lisp_Object Fcapitalize (Lisp_Object obj);
extern Lisp_Object Fupcase_region (Lisp_Object b, Lisp_Object e);
extern Lisp_Object Fdowncase_region (Lisp_Object b, Lisp_Object e);
extern Lisp_Object Fcapitalize_region (Lisp_Object b, Lisp_Object e);
extern Lisp_Object upcase_initials_region (Lisp_Object b, Lisp_Object e);
extern Lisp_Object Fupcase_word (Lisp_Object arg);
extern Lisp_Object Fdowncase_word (Lisp_Object arg);
extern Lisp_Object Fcapitalize_word (Lisp_Object arg);


/* defined in keyboard.c */
extern int interrupt_char;
extern Lisp_Object help_char;
extern Lisp_Object Vhelp_form;
extern Lisp_Object Fdiscard_input (void), Frecursive_edit (void);
extern Lisp_Object Finput_pending_p (void);
extern Lisp_Object Qhelp_form, Qunread_command_event, Qdisabled, Qtop_level;
extern void stuff_buffered_input (Lisp_Object stuffstring);
extern Lisp_Object Fset_input_mode (Lisp_Object interrupt,
                                    Lisp_Object flow, 
                                    Lisp_Object meta);
extern SIGTYPE interrupt_signal (int dummy);
extern void stop_polling (void);
extern void start_polling (void);
extern int command_loop_level;

/* defined in indent.c */
extern Lisp_Object Fvertical_motion (Lisp_Object lines, Lisp_Object window);
extern Lisp_Object Findent_to (Lisp_Object col, Lisp_Object mincol);
extern Lisp_Object Fcurrent_column (void); 
extern int current_column (void);
extern int pos_tab_offset (struct window *w, int pos);
extern void invalidate_current_column (void);
extern int position_indentation (struct buffer *buf, int pos);


/* defined in undo.c */
extern Lisp_Object Fundo_boundary (void);
extern Lisp_Object truncate_undo_list (Lisp_Object list, int min, int max);
extern void record_change (int beg, int length);
extern void record_insert (int beg, int length);
extern void record_delete (int beg, int length);
extern void record_extent (Lisp_Object extent, int attached);
extern void modify_region (struct buffer *buf, int start, int end);
extern void record_extent (Lisp_Object extent, int attached);

/* defined in window.c */
extern Lisp_Object Qwindowp;
extern Lisp_Object Qscroll_up, Qscroll_down;
extern Lisp_Object Fselected_window (void);
extern Lisp_Object Fwindow_buffer (Lisp_Object window);
extern Lisp_Object Fget_buffer_window (Lisp_Object buffer, 
                                       Lisp_Object screen,
                                       Lisp_Object invisible_too);
extern Lisp_Object Fsave_window_excursion (Lisp_Object body);
extern Lisp_Object Fset_window_configuration (Lisp_Object config);
extern Lisp_Object Fcurrent_window_configuration (Lisp_Object screen);
extern Lisp_Object Fdisplay_buffer (Lisp_Object buffer, 
                                    Lisp_Object notthiswindow, 
                                    Lisp_Object overridescreen);
extern Lisp_Object Freplace_buffer_in_windows (Lisp_Object buffer);
extern Lisp_Object Fwindow_dedicated_p (Lisp_Object window);
extern Lisp_Object Fnext_window (Lisp_Object window, 
                                 Lisp_Object minibuf, 
                                 Lisp_Object all_screens,
                                 Lisp_Object force);
extern Lisp_Object Fdelete_window (Lisp_Object window);
extern Lisp_Object Fselect_window (Lisp_Object window);
extern Lisp_Object Fset_window_buffer (Lisp_Object window, 
                                       Lisp_Object buffer);
extern Lisp_Object Fsplit_window (Lisp_Object window, 
                                  Lisp_Object chsize, 
                                  Lisp_Object horflag);
extern Lisp_Object Frecenter (Lisp_Object arg);
extern Lisp_Object Fmove_to_window_line (Lisp_Object arg);
extern Lisp_Object next_screen_window (struct screen *screen,
                                       Lisp_Object window, Lisp_Object mini);
extern void set_window_height (Lisp_Object window, int height, int nodelete);
extern void set_window_width (Lisp_Object window, int height, int nodelete);
extern int window_internal_height (struct window *);
extern Lisp_Object Fbuffer_left_margin_pixwidth (Lisp_Object buffer);
extern Lisp_Object Fbuffer_right_margin_pixwidth (Lisp_Object buffer);


/* defined in screen.c */
extern Lisp_Object Fscreenp (Lisp_Object obj);
extern Lisp_Object Flive_screen_p (Lisp_Object obj);
extern Lisp_Object Fselect_screen (Lisp_Object scr);
extern Lisp_Object Fselected_screen (void);
extern Lisp_Object Fwindow_screen (Lisp_Object window);
extern Lisp_Object Fscreen_root_window (Lisp_Object screen);
extern Lisp_Object Fscreen_selected_window (Lisp_Object screen);
extern Lisp_Object Fscreen_list (void);
extern Lisp_Object Fnext_screen (Lisp_Object screen, 
                                 Lisp_Object miniscreen,
                                 Lisp_Object visible_only_p);
extern Lisp_Object Fdelete_screen (Lisp_Object screen);
extern Lisp_Object Fread_mouse_position (Lisp_Object screen);
extern Lisp_Object Fset_mouse_position (Lisp_Object screen,
                                        Lisp_Object x, Lisp_Object y);
extern Lisp_Object Fmake_screen_visible (Lisp_Object screen);
extern Lisp_Object Fmake_screen_invisible (Lisp_Object screen);
extern Lisp_Object Ficonify_screen (Lisp_Object screen);
extern Lisp_Object Fdeiconify_screen (Lisp_Object screen);
extern Lisp_Object Fscreen_visible_p (Lisp_Object screen);
extern Lisp_Object Fvisible_screen_list (void);
extern Lisp_Object Fscreen_parameters (Lisp_Object screen);
extern Lisp_Object Fmodify_screen_parameters (Lisp_Object screen,
                                              Lisp_Object alist);
extern Lisp_Object Fscreen_height (Lisp_Object screen);
extern Lisp_Object Fscreen_width (Lisp_Object screen);
extern Lisp_Object Fset_screen_height (Lisp_Object screen,
                                       Lisp_Object rows, Lisp_Object pretend);
extern Lisp_Object Fset_screen_width  (Lisp_Object screen,
                                       Lisp_Object cols, Lisp_Object pretend);
extern Lisp_Object Fset_screen_size (Lisp_Object screen, 
                                     Lisp_Object cols, Lisp_Object rows, 
                                     Lisp_Object pretend);
extern Lisp_Object Fset_screen_position (Lisp_Object screen,
                                         Lisp_Object xoffset, 
                                         Lisp_Object yoffset);
extern Lisp_Object Fcoordinates_in_window_p (Lisp_Object coords,
                                             Lisp_Object window);
extern Lisp_Object Flocate_window_from_coordinates (Lisp_Object screen,
                                                    Lisp_Object coords);
extern Lisp_Object window_from_coordinates (Lisp_Object frame,
                                            int x, int y,
                                            Lisp_Object *part);
extern Lisp_Object Vscreen_list;
extern Lisp_Object Vglobal_minibuffer_screen;
extern int allow_deletion_of_last_visible_screen;

extern Lisp_Object Vcreate_screen_hook, Qcreate_screen_hook;
extern Lisp_Object Vmouse_enter_screen_hook, Qmouse_enter_screen_hook;
extern Lisp_Object Vmouse_leave_screen_hook, Qmouse_leave_screen_hook;
extern Lisp_Object Vmap_screen_hook, Qmap_screen_hook;
extern Lisp_Object Vunmap_screen_hook, Qunmap_screen_hook;
extern Lisp_Object Vmouse_motion_handler;
extern Lisp_Object Vsynchronize_minibuffers;

extern Lisp_Object Qscreenp, Qlive_screen_p, Qdelete_screen;
extern Lisp_Object Qselect_screen_hook, Qdeselect_screen_hook;


/* defined in xdisp.c */
extern Lisp_Object Fredraw_display (void);
extern Lisp_Object Fredraw_screen (Lisp_Object screen);
extern Lisp_Object Fmessage_displayed_p (Lisp_Object return_string);
extern Lisp_Object Vscreen_title_format, Vscreen_icon_title_format;
extern Lisp_Object Voverlay_arrow_position, Voverlay_arrow_string;




/* defined in emacs.c */
extern DOESNT_RETURN fatal (CONST char *fmt, ...); /* ... printf args */
extern DOESNT_RETURN error (CONST char *fmt, ...); /* ... printf args */
extern void message (CONST char *fmt, ...);        /* ... printf args */
extern void clear_message (int update_screen);

extern SIGTYPE fatal_error_signal (int sig);

extern Lisp_Object decode_env_path (CONST char *evarname, CONST char *def);
/* Nonzero means don't do interactive redisplay and don't change tty modes */
extern int noninteractive;
/* Nonzero means don't do use window-system-specific display code */
extern int inhibit_window_system;
extern Lisp_Object Fkill_emacs (Lisp_Object arg);

extern Lisp_Object Vcommand_line_args;
extern Lisp_Object Vinvocation_name;
extern Lisp_Object Vexecution_path;

extern int emacs_priority;

extern Lisp_Object Qx;
extern Lisp_Object Qsave_buffers_kill_emacs;
extern Lisp_Object Qkill_emacs_hook;

/* defined in callproc.c */
extern Lisp_Object Vexec_path, Vexec_directory, Vdata_directory;


/* defined in environ.c */
extern int size_of_current_environ (void);
extern void get_current_environ (char **memory_block);
extern Lisp_Object lisp_getenv (Lisp_Object str);
/* extern void set_environment_alist (Lisp_Object str, Lisp_Object val); */


/* defined in doc.c */
extern Lisp_Object Vdoc_file_name;
extern Lisp_Object Fsubstitute_command_keys (Lisp_Object string);
extern Lisp_Object Fdocumentation (Lisp_Object fun, Lisp_Object raw);
extern Lisp_Object Fdocumentation_property (Lisp_Object sym, Lisp_Object prop,
                                            Lisp_Object raw);

/* defined in bytecode.c */
extern Lisp_Object Qbytecode;
extern Lisp_Object Fbyte_code (Lisp_Object bytestr, 
                               Lisp_Object constants_vector, 
                               Lisp_Object maxdepth);


/* defined in macros.c */
extern Lisp_Object Fexecute_kbd_macro (Lisp_Object macro, 
                                       Lisp_Object prefixarg);

/* defined in extents.c */
extern void set_point (int position);
extern void set_buffer_point (struct buffer *buffer, int position);
extern Lisp_Object Fdelete_extent (Lisp_Object extent);
extern Lisp_Object Fdetach_extent (Lisp_Object);
extern Lisp_Object Fmake_extent (Lisp_Object from, Lisp_Object to,
                                 Lisp_Object buffer);
extern Lisp_Object Fextent_property (Lisp_Object extent, Lisp_Object);
extern Lisp_Object Fset_extent_property (Lisp_Object,Lisp_Object,Lisp_Object);
extern Lisp_Object Fset_extent_priority (Lisp_Object extent, Lisp_Object pri);
extern Lisp_Object Fset_extent_begin_glyph (Lisp_Object extent,
					    Lisp_Object begin_glyph,
					    Lisp_Object layout);
extern Lisp_Object Fset_extent_begin_glyph (Lisp_Object extent_obj,
					    Lisp_Object glyph,
					    Lisp_Object layout);
extern Lisp_Object Fset_extent_end_glyph (Lisp_Object extent_obj,
					  Lisp_Object glyph,
					  Lisp_Object layout);
extern Lisp_Object replicate_extents (int opoint, int length, 
                                      struct buffer *buf);
extern void init_buffer_cached_stack (struct buffer* b);
extern void free_buffer_cached_stack (struct buffer *b);
extern void detach_buffer_extents (struct buffer *b);
extern Lisp_Object Fextent_replica_extent (Lisp_Object dup);
extern Lisp_Object Fextent_replica_start (Lisp_Object dup);
extern Lisp_Object Fextent_replica_end (Lisp_Object dup);
extern Lisp_Object Fset_extent_endpoints (Lisp_Object,
					  Lisp_Object, Lisp_Object);
extern Lisp_Object Fextent_at (Lisp_Object pos, Lisp_Object buffer,
			       Lisp_Object property, Lisp_Object before);

extern Lisp_Object Qextentp;

extern Lisp_Object Qdetached, Qdestroyed, Qbegin_glyph, Qend_glyph;
extern Lisp_Object Qstart_open, Qend_open, Qread_only, Qhighlight;
extern Lisp_Object Qunique, Qduplicable, Qinvisible;
extern Lisp_Object Qoutside_margin, Qinside_margin, Qwhitespace, Qtext;
extern Lisp_Object Qglyph_invisible;

/* defined in xfns.c */
extern Lisp_Object Fx_close_current_connection (void);
extern Lisp_Object Fx_set_scrollbar_pointer (Lisp_Object screen,
					     Lisp_Object cursor);
extern Lisp_Object Vbar_cursor;
extern Lisp_Object Qx_EnterNotify_internal, Qx_LeaveNotify_internal;
extern Lisp_Object Qx_FocusIn_internal, Qx_FocusOut_internal;
extern Lisp_Object Qx_VisibilityNotify_internal;
extern Lisp_Object Qx_non_VisibilityNotify_internal;
extern Lisp_Object Qx_MapNotify_internal, Qx_UnmapNotify_internal;
extern Lisp_Object Qstring, Qname, Qboolean, Qinteger, Qpointer;


/* defined in faces.c */
extern Lisp_Object Qface, Qextent_face, Qset_extent_face;
extern unsigned long load_pixmap (struct screen *s, Lisp_Object name,
				  unsigned int *wP, unsigned int *hP,
				  unsigned int *dP, unsigned long *maskP);

/* Defined in font_lock.c */
extern Lisp_Object Qcomment, Qblock_comment;
extern Lisp_Object Qbeginning_of_defun, Qend_of_defun;


/* defined in dispnew.c */
extern Lisp_Object Vwindow_system;
extern Lisp_Object Vwindow_system_version;
extern Lisp_Object Fding (Lisp_Object arg, Lisp_Object sound);
extern Lisp_Object Qcursor_in_echo_area;


/* defined in filemode.c */
extern void filemodestring (struct stat *statp, char *str);

/* defined in getloadavg.c */
extern int getloadavg (double loadavg[], int nelem);

/* defined in vm-limit.c */
extern void memory_warnings (void *start, void (*warnfun) (CONST char *));

/* defined in mocklisp.c */
extern Lisp_Object Vmocklisp_arguments, Qmocklisp, Qmocklisp_arguments;
extern Lisp_Object ml_apply (Lisp_Object function, Lisp_Object args);

/* defined in menubar.c */
void initialize_screen_menubar (struct screen *s);
extern Lisp_Object Fpopup_dialog_box (Lisp_Object dbox_desc);
extern Lisp_Object Fpopup_menu (Lisp_Object menu_desc);
extern int menubar_has_changed;
extern Lisp_Object Qcurrent_menubar;
extern Lisp_Object Qactivate_menubar_hook;
extern Lisp_Object Vactivate_menubar_hook;
extern unsigned int popup_id_tick;
extern int popup_menu_up_p;
extern int dbox_up_p;

/* defined in scrollbar.c */
void initialize_screen_scrollbars (struct screen *s);


/* Defined in eval.c */
extern Lisp_Object Qautoload, Qexit, Qinteractive, Qcommandp, Qdefun, Qmacro;
extern Lisp_Object Vinhibit_quit, Vquit_flag, Qinhibit_quit;
extern Lisp_Object Vautoload_queue;
extern Lisp_Object Vrun_hooks;
extern Lisp_Object Fuser_variable_p (Lisp_Object);
extern Lisp_Object Finteractive_p (void);
extern DOESNT_RETURN signal_error (Lisp_Object sig, Lisp_Object data);
extern DOESNT_RETURN signal_simple_error (CONST char *, Lisp_Object);
extern DOESNT_RETURN signal_simple_error_2 (CONST char *,
					    Lisp_Object, Lisp_Object);
extern Lisp_Object Fprogn (Lisp_Object args);
extern Lisp_Object Fcommandp (Lisp_Object obj);
extern Lisp_Object Feval (Lisp_Object form);
extern Lisp_Object Fapply (int nargs, Lisp_Object *args);
extern Lisp_Object Ffuncall (int nargs, Lisp_Object *args);
extern Lisp_Object Fbacktrace (Lisp_Object stream, Lisp_Object detailed);
extern Lisp_Object apply1 (Lisp_Object fn, Lisp_Object args);
extern Lisp_Object call0 (Lisp_Object fn);
extern Lisp_Object call1 (Lisp_Object fn, Lisp_Object arg0);
extern Lisp_Object call2 (Lisp_Object fn, Lisp_Object a0, Lisp_Object a1);
extern Lisp_Object call3 (Lisp_Object fn,
                          Lisp_Object a0, Lisp_Object a1, Lisp_Object a2);
extern Lisp_Object call4 (Lisp_Object fn,
                          Lisp_Object a0, Lisp_Object a1, Lisp_Object a2,
                          Lisp_Object a3);
extern Lisp_Object call5 (Lisp_Object fn,
                          Lisp_Object a0, Lisp_Object arg1, Lisp_Object a2,
                          Lisp_Object a3, Lisp_Object a4);
extern Lisp_Object call6 (Lisp_Object fn,
                          Lisp_Object a0, Lisp_Object arg1, Lisp_Object a2,
                          Lisp_Object a3, Lisp_Object a4, Lisp_Object a5);
extern Lisp_Object Fsignal (Lisp_Object signame, Lisp_Object data);
/* C Code should be using internal_catch, record_unwind_p, condition_case_1 */
/* extern Lisp_Object Fcatch (Lisp_Object args); */
/* extern Lisp_Object Funwind_protect (Lisp_Object args); */
/* extern Lisp_Object Fcondition_case (Lisp_Object args); */
extern Lisp_Object Fthrow (Lisp_Object tag, Lisp_Object val);
extern Lisp_Object internal_catch (Lisp_Object tag, 
                                   Lisp_Object (*func) (Lisp_Object arg),
                                   Lisp_Object arg,
                                   int *threw);
extern Lisp_Object condition_case_1 (Lisp_Object handlers,
                                     Lisp_Object (*bfun) (Lisp_Object barg),
                                     Lisp_Object barg,
                                     Lisp_Object (*hfun) (Lisp_Object val,
                                                          Lisp_Object harg),
                                     Lisp_Object harg);
extern Lisp_Object Fcondition_case_3 (Lisp_Object bodyform, 
                                      Lisp_Object var, 
                                      Lisp_Object handlers);
extern Lisp_Object unbind_to (int n, Lisp_Object val);
extern void specbind (Lisp_Object symbol, Lisp_Object value);
extern void record_unwind_protect (Lisp_Object (*function) (Lisp_Object arg),
                                   Lisp_Object arg);
extern void do_autoload (Lisp_Object fundef, Lisp_Object funname);
extern Lisp_Object un_autoload (Lisp_Object oldqueue);
extern Lisp_Object find_symbol_value (Lisp_Object symbol);
extern Lisp_Object top_level_value (Lisp_Object symbol);

extern char *egetenv (CONST char *);
/* extern char *getenv (CONST char *); */

/* From unex*.c */
extern int unexec (char *new_name, char *a_name,
                   unsigned int data_start, 
                   unsigned int bss_start, 
                   unsigned int entry_address);

#endif /* _EMACSFNS_H_ */
