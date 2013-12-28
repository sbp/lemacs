/* Various function declarations for GNU Emacs.
   Used to be part of lisp.h
   Copyright (C) 1992-1993 Free Software Foundation, Inc.

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

extern void ring_bell (Lisp_Object sound);

struct buffer;
struct screen;
struct window;

/* Define in doprnt.c */
extern int doprnt (char *buffer, int bufsize, 
                   char *format, char *format_end, 
                   int nargs, char **args);

/* Defined in data.c */
extern Lisp_Object Qnil, Qt, Qquote, Qlambda, Qfunction, Qunbound;
extern Lisp_Object Qerror_conditions, Qerror_message, Qtop_level;
extern Lisp_Object Qerror, Qquit, Qwrong_type_argument, Qargs_out_of_range;
extern Lisp_Object Qvoid_variable, Qvoid_function;
extern Lisp_Object Qsetting_constant, Qinvalid_read_syntax;
extern Lisp_Object Qinvalid_function, Qwrong_number_of_arguments, Qno_catch;
extern Lisp_Object Qend_of_file, Qarith_error;
extern Lisp_Object Qbeginning_of_buffer, Qend_of_buffer, Qbuffer_read_only;

extern Lisp_Object Qintegerp, Qnatnump, Qsymbolp, Qlistp, Qconsp;
extern Lisp_Object Qstringp, Qarrayp, Qsequencep, Qbufferp;
extern Lisp_Object Qchar_or_string_p, Qmarkerp, Qvectorp;
extern Lisp_Object Qinteger_or_marker_p, Qboundp, Qfboundp;
extern Lisp_Object Qcdr;

extern Lisp_Object Qnumberp, Qnumber_or_marker_p;

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
void decache_buffer_local_variables (struct buffer *buf);
extern void store_symval_forwarding (Lisp_Object sym,
                                     Lisp_Object valcontents, 
                                     Lisp_Object newval);
extern Lisp_Object Fmake_local_variable (Lisp_Object object);
extern Lisp_Object Fzerop (Lisp_Object);
extern Lisp_Object Fint_to_string (Lisp_Object num);
extern Lisp_Object Fstring_to_int (Lisp_Object str);

extern Lisp_Object make_number (int);
extern void pure_write_error (void);
extern void args_out_of_range (Lisp_Object, Lisp_Object);
extern void args_out_of_range_3 (Lisp_Object, Lisp_Object, Lisp_Object);
extern Lisp_Object wrong_type_argument (Lisp_Object pred, Lisp_Object value);

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

#ifdef NEED_STRDUP
extern char *strdup (char *);
#endif

extern Lisp_Object word_to_lisp (unsigned int);
extern unsigned int lisp_to_word (Lisp_Object);

extern Lisp_Object list_sort (Lisp_Object list, 
                              Lisp_Object lisp_arg,
                              int (*pred_fn) (Lisp_Object first,
                                              Lisp_Object second,
                                              Lisp_Object lisp_arg));
extern Lisp_Object Fsort (Lisp_Object list, 
                          Lisp_Object pred);
extern Lisp_Object merge (Lisp_Object org_l1, Lisp_Object org_l2,
                          Lisp_Object pred);

extern Lisp_Object safe_funcall_hook (Lisp_Object hook, 
                                      int nargs, Lisp_Object arg1,
                                      Lisp_Object arg2, 
                                      Lisp_Object arg3);

extern Lisp_Object Qstring_lessp;
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
extern Lisp_Object Fget (Lisp_Object sym, Lisp_Object prop);
extern Lisp_Object Fput (Lisp_Object sym, Lisp_Object prop, Lisp_Object val);
extern Lisp_Object Fremprop (Lisp_Object sym, Lisp_Object prop);
extern Lisp_Object Fequal (Lisp_Object one, Lisp_Object two);
extern Lisp_Object Ffillarray (Lisp_Object array, Lisp_Object init);
extern Lisp_Object Fnconc (int nargs, Lisp_Object *args);
extern Lisp_Object Fmapcar (Lisp_Object fn, Lisp_Object seq);
extern Lisp_Object Qyes_or_no_p;
extern Lisp_Object Ffeaturep (Lisp_Object name);
extern Lisp_Object Frequire (Lisp_Object name, Lisp_Object filename);
extern Lisp_Object Fprovide (Lisp_Object name);
extern Lisp_Object concat2 (Lisp_Object s1, Lisp_Object s2);
extern Lisp_Object nconc2 (Lisp_Object l1, Lisp_Object l2);
extern Lisp_Object Fcopy_alist (Lisp_Object alist);
extern Lisp_Object Fplay_sound (Lisp_Object sound, Lisp_Object volume);

extern void bitch_at_user (Lisp_Object sound);

/* Defined in alloc.c */
extern void memory_full (void);
extern void *xmalloc (int size);
extern void *xrealloc (void *p, int size);
extern void xfree (void *p);
extern void disksave_object_finalisation (void);
extern Lisp_Object Vpurify_flag;
extern Lisp_Object Fcons (Lisp_Object car, Lisp_Object cdr);
extern Lisp_Object Flist (int nargs, Lisp_Object *args);
extern Lisp_Object Fmake_list (Lisp_Object length, Lisp_Object init);
extern Lisp_Object Fmake_vector (Lisp_Object length, Lisp_Object init);
extern Lisp_Object Fvector (Lisp_Object nargs, Lisp_Object *args);
extern Lisp_Object Fmake_symbol (Lisp_Object name);
extern Lisp_Object Fmake_marker (void);
extern Lisp_Object Fmake_string (Lisp_Object length, Lisp_Object init);
extern Lisp_Object build_string (const char *);
extern Lisp_Object make_string (const char *, int);
extern Lisp_Object Fpurecopy (Lisp_Object);
extern Lisp_Object make_pure_string (const char *, int);
extern Lisp_Object pure_cons (Lisp_Object, Lisp_Object);
extern Lisp_Object make_pure_vector (int len, enum Lisp_Type vector_type);
extern Lisp_Object make_float (double float_value);
extern Lisp_Object Fgarbage_collect (void);
extern Lisp_Object list1 (Lisp_Object);
extern Lisp_Object list2 (Lisp_Object, Lisp_Object);
extern Lisp_Object list3 (Lisp_Object, Lisp_Object, Lisp_Object);
extern Lisp_Object list4 (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object);
extern Lisp_Object list5 (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object,
			  Lisp_Object);
extern void free_cons (struct Lisp_Cons *ptr);


/* Defined in print.c */
extern Lisp_Object Vprin1_to_string_buffer;
extern Lisp_Object Fprin1 (Lisp_Object obj, Lisp_Object printcharfun);
extern Lisp_Object Fprinc (Lisp_Object obj, Lisp_Object printcharfun);
extern Lisp_Object Fprint (Lisp_Object obj, Lisp_Object printcharfun);
extern Lisp_Object Fprin1_to_string (Lisp_Object obj, Lisp_Object noescape);
extern Lisp_Object Fterpri (Lisp_Object printcharfun);
extern Lisp_Object Vstandard_output, Qstandard_output;
extern void temp_output_buffer_setup (const char *bufname);
extern void temp_output_buffer_show (Lisp_Object buf, Lisp_Object same_scrn);
/* NOTE:  Do not call this with the data of a Lisp_String.  Use princ. */
extern void write_string_internal (const char *ptr, int size, 
                                   Lisp_Object printcharfun);
/* NOTE:  Do not call this with the data of a Lisp_String.  Use princ */
extern void write_string (const char *data, int size);
extern void write_string_1 (char *, int, Lisp_Object);
extern void print_internal (Lisp_Object obj, 
                            Lisp_Object printcharfun, 
                            Lisp_Object escapeflag);
extern int Vprint_level;
extern int Vprint_length;
extern int print_escape_newlines;
extern int print_readably;
extern Lisp_Object Qprint_escape_newlines;
extern Lisp_Object internal_with_output_to_temp_buffer
  (const char *bufname, 
   Lisp_Object (*function) (Lisp_Object args),
   Lisp_Object args,
   Lisp_Object same_screen);
extern void temp_output_buffer_setup (const char *bufname);
extern void float_to_string (char *buf, double data);



/* Defined in lread.c */
extern Lisp_Object Qvariable_documentation, Qstandard_input, Qread_char;
extern Lisp_Object Vobarray, Vstandard_input;
extern Lisp_Object Fread (Lisp_Object readcharfun);
extern Lisp_Object Fread_from_string (Lisp_Object string, 
                                      Lisp_Object start, Lisp_Object end);
extern Lisp_Object Fintern (Lisp_Object str, Lisp_Object obarray);
extern Lisp_Object Fintern_soft (Lisp_Object str, Lisp_Object obarray);
extern Lisp_Object Fload (Lisp_Object filename, Lisp_Object missing_ok,
                          Lisp_Object nomessage, Lisp_Object nosuffix);
extern int locate_file (Lisp_Object path, 
                        Lisp_Object str, const char *suffix, 
                        Lisp_Object *storeptr, int mode);
extern Lisp_Object intern (const char *);
extern Lisp_Object oblookup (Lisp_Object obarray,
			     const unsigned char *s, int len);
extern void map_obarray (Lisp_Object obarray, 
                         void (*fn) (Lisp_Object sym, Lisp_Object arg),
                         Lisp_Object arg);
extern int isfloat_string (const char *s);

void defsymbol (Lisp_Object *location, const char *name);


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
extern Lisp_Object format1 ();
extern Lisp_Object Fbuffer_substring (Lisp_Object start, Lisp_Object end);
extern Lisp_Object Fbuffer_string (void);
extern Lisp_Object make_string_from_buffer (struct buffer *buf,
                                            int index, int length);
extern Lisp_Object Fstring_equal (Lisp_Object s1, Lisp_Object s2);
extern Lisp_Object Fstring_lessp (Lisp_Object s1, Lisp_Object s2);
extern Lisp_Object Fbuffer_substring_lessp ();
extern Lisp_Object save_excursion_save (void), save_restriction_save (void);
extern Lisp_Object save_excursion_restore (Lisp_Object info);
extern Lisp_Object save_restriction_restore (Lisp_Object info);
extern Lisp_Object Fchar_to_string (Lisp_Object ch);
extern Lisp_Object Fzmacs_activate_region (void);
extern Lisp_Object Fzmacs_deactivate_region (void);
extern Lisp_Object Fcurrent_time_seconds (Lisp_Object cons);


/* Defined in cmds.c */
extern Lisp_Object Fforward_char (Lisp_Object n);
extern Lisp_Object Fforward_line (Lisp_Object n);
extern Lisp_Object Fend_of_line (Lisp_Object n);
extern Lisp_Object Fbeginning_of_line (Lisp_Object n);


/* Defined in buffer.c */
extern Lisp_Object Vbuffer_alist;
extern void validate_region (Lisp_Object *beginning, Lisp_Object *end);
extern void record_buffer (Lisp_Object buf);

extern Lisp_Object Fbuffer_list (Lisp_Object);
extern Lisp_Object Fget_buffer (Lisp_Object);
extern Lisp_Object Fget_file_buffer (Lisp_Object);
extern Lisp_Object Fget_buffer_create (Lisp_Object);
extern Lisp_Object Fgenerate_new_buffer_name (Lisp_Object);
extern Lisp_Object Fbuffer_name (Lisp_Object);
extern Lisp_Object Fbuffer_file_name (Lisp_Object);
extern Lisp_Object Fbuffer_local_variables (Lisp_Object);
extern Lisp_Object Fbuffer_modified_p (Lisp_Object);
extern Lisp_Object Fset_buffer_modified_p (Lisp_Object);
extern Lisp_Object Fbuffer_modified_tick (Lisp_Object);
extern Lisp_Object Frename_buffer (Lisp_Object, Lisp_Object);
extern Lisp_Object Fother_buffer (Lisp_Object, Lisp_Object);
extern Lisp_Object Fbuffer_disable_undo (Lisp_Object);
extern Lisp_Object Fbuffer_enable_undo (Lisp_Object);
extern Lisp_Object Fkill_buffer (Lisp_Object);
extern Lisp_Object Fswitch_to_buffer (Lisp_Object, Lisp_Object);
extern Lisp_Object Fpop_to_buffer (Lisp_Object, Lisp_Object, Lisp_Object);
extern Lisp_Object Fcurrent_buffer (void);
extern Lisp_Object Fset_buffer (Lisp_Object);
extern Lisp_Object Fbarf_if_buffer_read_only (void);
extern Lisp_Object Fbury_buffer (Lisp_Object);
extern Lisp_Object Ferase_buffer (void);
extern Lisp_Object Fkill_all_local_variables (void);


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

/* defined in fileio.c */

extern Lisp_Object Qfile_error;

extern Lisp_Object Ffile_name_directory (Lisp_Object);
extern Lisp_Object Ffile_name_nondirectory (Lisp_Object);
extern Lisp_Object Ffile_name_as_directory (Lisp_Object);
extern Lisp_Object Fdirectory_file_name (Lisp_Object);
extern Lisp_Object Fmake_temp_name (Lisp_Object);
extern Lisp_Object Fexpand_file_name (Lisp_Object, Lisp_Object);
extern Lisp_Object Ftruename (Lisp_Object, Lisp_Object);
extern Lisp_Object Fsubstitute_in_file_name (Lisp_Object);
extern Lisp_Object Fcopy_file (Lisp_Object, Lisp_Object,
			       Lisp_Object, Lisp_Object);
extern Lisp_Object Fmake_directory (Lisp_Object);
extern Lisp_Object Fremove_directory (Lisp_Object);
extern Lisp_Object Fdelete_file (Lisp_Object);
extern Lisp_Object Frename_file (Lisp_Object, Lisp_Object, Lisp_Object);
extern Lisp_Object Fadd_name_to_file (Lisp_Object, Lisp_Object, Lisp_Object);
extern Lisp_Object Ffile_name_absolute_p (Lisp_Object);
extern Lisp_Object Ffile_exists_p (Lisp_Object);
extern Lisp_Object Ffile_executable_p (Lisp_Object);
extern Lisp_Object Ffile_readable_p (Lisp_Object);
extern Lisp_Object Ffile_symlink_p (Lisp_Object);
extern Lisp_Object Ffile_writable_p (Lisp_Object);
extern Lisp_Object Ffile_directory_p (Lisp_Object);
extern Lisp_Object Ffile_modes (Lisp_Object);
extern Lisp_Object Fset_file_modes (Lisp_Object, Lisp_Object);
extern Lisp_Object Ffile_newer_than_file_p (Lisp_Object, Lisp_Object);
extern Lisp_Object Finsert_file_contents (Lisp_Object, Lisp_Object);
extern Lisp_Object Fwrite_region (Lisp_Object, Lisp_Object, Lisp_Object,
				  Lisp_Object, Lisp_Object);
extern Lisp_Object Fverify_visited_file_modtime (Lisp_Object);
extern Lisp_Object Fclear_visited_file_modtime (void);
extern Lisp_Object Fset_visited_file_modtime (void);
extern Lisp_Object Fset_buffer_modtime (Lisp_Object, Lisp_Object);
extern Lisp_Object Fdo_auto_save (Lisp_Object);
extern Lisp_Object Fset_buffer_auto_saved (void);
extern Lisp_Object Frecent_auto_save_p (void);
extern void report_file_error (const char *, Lisp_Object);


/* defined in filelock.c */

extern void lock_file (Lisp_Object fn);
extern void unlock_file (Lisp_Object fn);
extern void unlock_all_files (void);
extern Lisp_Object Flock_buffer (Lisp_Object fn);
extern Lisp_Object Funlock_buffer (void);
extern void unlock_buffer (struct buffer *buffer);
extern Lisp_Object Ffile_locked_p (Lisp_Object fn);


/* defined in event-stream.c */

extern Lisp_Object Finput_pending_p (void);
extern Lisp_Object Fenqueue_command_event (Lisp_Object, Lisp_Object);
extern Lisp_Object Fnext_event (Lisp_Object);
extern Lisp_Object Fnext_command_event (Lisp_Object);
extern Lisp_Object Fread_char (void);
extern Lisp_Object Fdiscard_input (void);
extern Lisp_Object Faccept_process_output (Lisp_Object);
extern Lisp_Object Fsleep_for (Lisp_Object);
extern Lisp_Object Fsit_for (Lisp_Object, Lisp_Object);
extern Lisp_Object Fadd_timeout (Lisp_Object, Lisp_Object,
				 Lisp_Object, Lisp_Object);
extern Lisp_Object Fdisable_timeout (Lisp_Object);
extern Lisp_Object Fdispatch_event (Lisp_Object);
extern Lisp_Object Fread_key_sequence (Lisp_Object);

extern void wait_delaying_user_input (int (*predicate) (void *arg),
                                      void *predicate_arg);
extern int detect_input_pending (void);
extern void enqueue_command_event (Lisp_Object event);
extern void cancel_echoing ();

/* Defined in events.c */

extern Lisp_Object Fevent_to_character (Lisp_Object, Lisp_Object);
extern Lisp_Object Fcharacter_to_event (Lisp_Object, Lisp_Object);
extern Lisp_Object Feventp (Lisp_Object);
extern Lisp_Object Fkey_press_event_p (Lisp_Object);
extern Lisp_Object Fbutton_press_event_p (Lisp_Object);
extern Lisp_Object Fbutton_release_event_p (Lisp_Object);
extern Lisp_Object Fbutton_event_p (Lisp_Object);
extern Lisp_Object Fmotion_event_p (Lisp_Object);
extern Lisp_Object Fprocess_event_p (Lisp_Object);
extern Lisp_Object Ftimeout_event_p (Lisp_Object);
extern Lisp_Object Fmenu_event_p (Lisp_Object);
extern Lisp_Object Feval_event_p (Lisp_Object);
extern Lisp_Object Fevent_timestamp (Lisp_Object);
extern Lisp_Object Fevent_key (Lisp_Object);
extern Lisp_Object Fevent_button (Lisp_Object);
extern Lisp_Object Fevent_modifier_bits (Lisp_Object);
extern Lisp_Object Fevent_modifiers (Lisp_Object);
extern Lisp_Object Fevent_x_pixel (Lisp_Object);
extern Lisp_Object Fevent_y_pixel (Lisp_Object);
extern Lisp_Object Fevent_window (Lisp_Object);
extern Lisp_Object Fevent_point (Lisp_Object);
extern Lisp_Object Fevent_x (Lisp_Object);
extern Lisp_Object Fevent_y (Lisp_Object);
extern Lisp_Object Fevent_glyph (Lisp_Object);
extern Lisp_Object Fevent_process (Lisp_Object);
extern Lisp_Object Fevent_function (Lisp_Object);
extern Lisp_Object Fevent_object (Lisp_Object);

void character_to_event (unsigned int, struct Lisp_Event *);


/* Defined in event-alloc.c */

extern Lisp_Object Fallocate_event (void);
extern Lisp_Object Fdeallocate_event (Lisp_Object);
extern Lisp_Object Fcopy_event (Lisp_Object, Lisp_Object);


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


/* defined in process.c */
extern Lisp_Object Fprocessp (Lisp_Object);
extern Lisp_Object Fget_process (Lisp_Object);
extern Lisp_Object Fget_buffer_process (Lisp_Object);
extern Lisp_Object Fdelete_process (Lisp_Object);
extern Lisp_Object Fprocess_status (Lisp_Object);
extern Lisp_Object Fprocess_exit_status (Lisp_Object);
extern Lisp_Object Fprocess_id (Lisp_Object);
extern Lisp_Object Fprocess_name (Lisp_Object);
extern Lisp_Object Fprocess_command (Lisp_Object);
extern Lisp_Object Fset_process_buffer (Lisp_Object, Lisp_Object);
extern Lisp_Object Fprocess_buffer (Lisp_Object);
extern Lisp_Object Fprocess_mark (Lisp_Object);
extern Lisp_Object Fset_process_filter (Lisp_Object, Lisp_Object);
extern Lisp_Object Fprocess_filter (Lisp_Object);
extern Lisp_Object Fprocess_sentinel (Lisp_Object);
extern Lisp_Object Fset_process_sentinel (Lisp_Object, Lisp_Object);
extern Lisp_Object Fprocess_kill_without_query (Lisp_Object, Lisp_Object);
extern Lisp_Object Fprocess_kill_without_query_p (Lisp_Object);
extern Lisp_Object Flist_processes (void);
extern Lisp_Object Fprocess_list (void);
extern Lisp_Object Fstart_process (int, Lisp_Object *);
extern Lisp_Object Fopen_network_stream (Lisp_Object, Lisp_Object,
					 Lisp_Object, Lisp_Object);
extern Lisp_Object Fprocess_send_region (Lisp_Object, Lisp_Object,
					 Lisp_Object);
extern Lisp_Object Fprocess_send_string (Lisp_Object, Lisp_Object);
extern Lisp_Object Finterrupt_process (Lisp_Object, Lisp_Object);
extern Lisp_Object Fkill_process (Lisp_Object, Lisp_Object);
extern Lisp_Object Fquit_process (Lisp_Object, Lisp_Object);
extern Lisp_Object Fstop_process (Lisp_Object, Lisp_Object);
extern Lisp_Object Fcontinue_process (Lisp_Object, Lisp_Object);
extern Lisp_Object Fsignal_process (Lisp_Object, Lisp_Object);
extern Lisp_Object Fprocess_send_eof (Lisp_Object);
extern Lisp_Object Fprocess_connection (Lisp_Object);
extern Lisp_Object Fprocess_send_eof (Lisp_Object);


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
extern int scan_buffer (int target, int pos, int cnt, int *shortage);
extern int find_next_newline (int from, int cnt);
extern void compile_pattern (Lisp_Object pattern, 
                             struct re_pattern_buffer *bufp, 
                             char *translate);
int fast_string_match (Lisp_Object regexp, Lisp_Object string);

/* defined in syntax.c */
extern int scan_words (int from, int count);
extern Lisp_Object Fforward_word (Lisp_Object n);


/* defined in minibuf.c */

extern int scmp (const char *s1, const char *s2, int len);

extern Lisp_Object Fread_from_minibuffer (Lisp_Object prompt, 
                                          Lisp_Object init,
                                          Lisp_Object keymap,
                                          Lisp_Object read_crock,
                                          Lisp_Object hist);
extern Lisp_Object Qread_from_minibuffer;

extern Lisp_Object Vminibuffer_zero;

/* Defined in callint.c */

extern Lisp_Object Vcommand_history;
extern Lisp_Object Qcall_interactively;
extern Lisp_Object Fcall_interactively (Lisp_Object fn, Lisp_Object record);
extern Lisp_Object Fprefix_numeric_value (Lisp_Object prefix);
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
extern Lisp_Object Vhelp_form, Vcommand_loop;
extern void initial_command_loop (void);
extern Lisp_Object Fdiscard_input (void), Frecursive_edit (void);
extern Lisp_Object Fcommand_execute (Lisp_Object cmd, Lisp_Object record);
extern Lisp_Object Finput_pending_p (void);
extern Lisp_Object Qhelp_form;
extern void stuff_buffered_input (Lisp_Object stuffstring);
extern Lisp_Object Fset_input_mode (Lisp_Object interrupt,
                                    Lisp_Object flow, 
                                    Lisp_Object meta);
extern SIGTYPE interrupt_signal (int dummy);


/* defined in keymap.c */
extern Lisp_Object Fmake_keymap (void);
extern Lisp_Object Fmake_sparse_keymap (void);
extern Lisp_Object Fkeymap_parent (Lisp_Object);
extern Lisp_Object Fset_keymap_parent (Lisp_Object, Lisp_Object);
extern Lisp_Object Fset_keymap_name (Lisp_Object, Lisp_Object);
extern Lisp_Object Fkeymapp (Lisp_Object);
extern Lisp_Object Fcopy_keymap (Lisp_Object);
extern Lisp_Object Fkeymap_fullness (Lisp_Object);
extern Lisp_Object Fdefine_key (Lisp_Object, Lisp_Object, Lisp_Object);
extern Lisp_Object Flookup_key (Lisp_Object, Lisp_Object);
extern Lisp_Object Fkey_binding (Lisp_Object);
extern Lisp_Object Fuse_global_map (Lisp_Object);
extern Lisp_Object Fuse_local_map (Lisp_Object);
extern Lisp_Object Fcurrent_local_map (void);
extern Lisp_Object Fcurrent_global_map (void);
extern Lisp_Object Fmap_keymap (Lisp_Object, Lisp_Object, Lisp_Object);
extern Lisp_Object Faccessible_keymaps (Lisp_Object);
extern Lisp_Object Fkey_description (Lisp_Object);
extern Lisp_Object Fsingle_key_description (Lisp_Object);
extern Lisp_Object Ftext_char_description (Lisp_Object);
extern Lisp_Object Fwhere_is_internal (Lisp_Object, Lisp_Object,
				       Lisp_Object, Lisp_Object, Lisp_Object);
extern Lisp_Object Fdescribe_bindings (Lisp_Object);
extern Lisp_Object Fapropos_internal (Lisp_Object, Lisp_Object);

extern void where_is_to_char (Lisp_Object definition, 
                              Lisp_Object local_map, Lisp_Object global_map,
                              char *buf);
extern void describe_map_tree (Lisp_Object startmap, int partial,
                               Lisp_Object shadow, Lisp_Object chartab,
                               int mice_only_p);
extern Lisp_Object Fmake_sparse_keymap (void);

extern Lisp_Object get_keymap (Lisp_Object, int);

/* defined in indent.c */ 
extern Lisp_Object Fvertical_motion (Lisp_Object, Lisp_Object); 
extern Lisp_Object Findent_to (Lisp_Object, Lisp_Object); 
extern Lisp_Object Fcurrent_column (void); 
extern int current_column (void);


/* defined in undo.c */
extern Lisp_Object Fundo_boundary (void);
extern Lisp_Object truncate_undo_list (Lisp_Object list, int min, int max);
extern void record_change (int beg, int length);
extern void record_insert (int beg, int length);
extern void record_delete (int beg, int length);
extern void modify_region (int start, int end);

/* defined in insdel.c */

extern void prepare_to_modify_buffer (int, int);
extern void modify_region (int, int);
extern void signal_before_change (int, int);
extern void signal_after_change (int, int, int);
extern void insert_relocatable_raw_string (const char *, int, Lisp_Object);
extern void insert_from_string (Lisp_Object, int, int);
extern void insert_raw_string (const char *, int);
extern void insert (const char *, int);
extern void insert_string (const char *);
extern void insert_char (char);
extern void insert_before_markers (const char *, int);
extern void insert_from_string_before_markers (Lisp_Object, int, int);
extern void insert_buffer_string (struct buffer *, int, int);
extern void del_range (int, int);


/* defined in window.c */

extern Lisp_Object Qwindowp;
extern Lisp_Object Fselected_window (void);
extern Lisp_Object Fminibuffer_window (void);
extern Lisp_Object Fwindow_minibuffer_p (Lisp_Object);
extern Lisp_Object Fpos_visible_in_window_p (Lisp_Object, Lisp_Object);
extern Lisp_Object Fwindow_buffer (Lisp_Object);
extern Lisp_Object Fwindow_height (Lisp_Object);
extern Lisp_Object Fwindow_width (Lisp_Object);
extern Lisp_Object Fwindow_hscroll (Lisp_Object);
extern Lisp_Object Fset_window_hscroll (Lisp_Object, Lisp_Object);
extern Lisp_Object Fwindow_edges (Lisp_Object);
extern Lisp_Object Fwindow_point (Lisp_Object);
extern Lisp_Object Fwindow_start (Lisp_Object);
extern Lisp_Object Fwindow_end (Lisp_Object);
extern Lisp_Object Fset_window_point (Lisp_Object, Lisp_Object);
extern Lisp_Object Fset_window_start (Lisp_Object, Lisp_Object, Lisp_Object);
extern Lisp_Object Fwindow_dedicated_p (Lisp_Object);
extern Lisp_Object Fset_window_buffer_dedicated (Lisp_Object, Lisp_Object);
extern Lisp_Object Fwindow_display_table (Lisp_Object);
extern Lisp_Object Fset_window_display_table (Lisp_Object, Lisp_Object);
extern Lisp_Object Fdelete_window (Lisp_Object);
extern Lisp_Object Fnext_window (Lisp_Object, Lisp_Object,
				 Lisp_Object, Lisp_Object);
extern Lisp_Object Fprevious_window (Lisp_Object, Lisp_Object,
				     Lisp_Object, Lisp_Object);
extern Lisp_Object Fother_window (Lisp_Object, Lisp_Object, Lisp_Object);
extern Lisp_Object Fget_lru_window (Lisp_Object);
extern Lisp_Object Fget_largest_window (Lisp_Object);
extern Lisp_Object Fget_buffer_window (Lisp_Object, Lisp_Object, Lisp_Object);
extern Lisp_Object Fdelete_other_windows (Lisp_Object);
extern Lisp_Object Fdelete_windows_on (Lisp_Object);
extern Lisp_Object Freplace_buffer_in_windows (Lisp_Object);
extern Lisp_Object Fset_window_buffer (Lisp_Object, Lisp_Object);
extern Lisp_Object Fselect_window (Lisp_Object);
extern Lisp_Object Fdisplay_buffer (Lisp_Object, Lisp_Object, Lisp_Object);
extern Lisp_Object Fsplit_window (Lisp_Object, Lisp_Object, Lisp_Object);
extern Lisp_Object Fenlarge_window (Lisp_Object, Lisp_Object);
extern Lisp_Object Fshrink_window (Lisp_Object, Lisp_Object);
extern Lisp_Object Fscroll_up (Lisp_Object);
extern Lisp_Object Fscroll_down (Lisp_Object);
extern Lisp_Object Fscroll_other_window (Lisp_Object);
extern Lisp_Object Fscroll_left (Lisp_Object);
extern Lisp_Object Fscroll_right (Lisp_Object);
extern Lisp_Object Frecenter (Lisp_Object);
extern Lisp_Object Fmove_to_window_line (Lisp_Object);
extern Lisp_Object Fset_window_configuration (Lisp_Object);
extern Lisp_Object Fcurrent_window_configuration (void);
extern Lisp_Object Fsave_window_excursion (Lisp_Object);

extern void temp_output_buffer_show (Lisp_Object buf, 
                                     Lisp_Object same_screen);
extern Lisp_Object next_screen_window (struct screen *screen,
                                       Lisp_Object window, Lisp_Object mini);
extern void set_window_height (Lisp_Object window, int height, int nodelete);
extern void set_window_width (Lisp_Object window, int height, int nodelete);
extern Lisp_Object make_window (void);
extern int window_height (Lisp_Object);
extern int window_internal_height (struct window *);
extern void change_window_height (int, int);

/* defined in screen.c */
extern Lisp_Object Fscreenp (Lisp_Object obj);
extern Lisp_Object Flive_screen_p (Lisp_Object obj);
extern Lisp_Object Fselect_screen (Lisp_Object scr);
extern Lisp_Object Ffocus_screen ();
extern Lisp_Object Funfocus_screen ();
extern Lisp_Object Fselected_screen (void);
extern Lisp_Object Fwindow_screen (Lisp_Object window);
extern Lisp_Object Fscreen_root_window ();
extern Lisp_Object Fscreen_selected_window ();
extern Lisp_Object Fscreen_list (void);
extern Lisp_Object Fnext_screen (Lisp_Object screen, 
                                 Lisp_Object miniscreen,
                                 Lisp_Object visible_only_p);
extern Lisp_Object Fdelete_screen (Lisp_Object screen);
extern Lisp_Object Fread_mouse_position ();
extern Lisp_Object Fset_mouse_position ();
extern Lisp_Object Fmake_screen_visible ();
extern Lisp_Object Fmake_screen_invisible ();
extern Lisp_Object Ficonify_screen ();
extern Lisp_Object Fdeiconify_screen ();
extern Lisp_Object Fscreen_visible_p (Lisp_Object screen);
extern Lisp_Object Fvisible_screen_list (void);
extern Lisp_Object Fscreen_parameters ();
extern Lisp_Object Fmodify_screen_parameters ();
extern Lisp_Object Fscreen_pixel_size ();
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
extern Lisp_Object Fcoordinates_in_window_p ();
extern Lisp_Object Flocate_window_from_coordinates ();

extern Lisp_Object next_screen (Lisp_Object, int, int);
extern Lisp_Object prev_screen (Lisp_Object, int, int);
extern void change_screen_size (struct screen *, int, int, int);


/* defined in emacs.c */
extern void fatal ();
extern void message ();
extern SIGTYPE fatal_error_signal (int sig);

extern Lisp_Object decode_env_path (const char *evarname, const char *def);
/* Nonzero means don't do interactive redisplay and don't change tty modes */
extern int noninteractive;
/* Nonzero means don't do use window-system-specific display code */
extern int inhibit_window_system;
extern Lisp_Object Fkill_emacs (Lisp_Object arg);

/* defined in callproc.c */
extern Lisp_Object Vexec_path, Vexec_directory, Vdata_directory;

/* defined in environ.c */
extern int size_of_current_environ (void);
extern void get_current_environ (char **memory_block);
extern Lisp_Object Fgetenv (Lisp_Object var, Lisp_Object interactivep);
/*extern void set_environment_alist (Lisp_Object str, Lisp_Object val);*/

/* defined in dispnew.c */
#if 0
Lisp_Object Vglyph_table;
Lisp_Object Vstandard_display_table;
#endif
extern Lisp_Object Vwindow_system;
extern Lisp_Object Vwindow_system_version;
extern Lisp_Object Fding (Lisp_Object arg, Lisp_Object sound);


/* defined in doc.c */
extern Lisp_Object Vdoc_file_name;
extern Lisp_Object Fsubstitute_command_keys (Lisp_Object string);
extern Lisp_Object Fdocumentation (Lisp_Object fun /*, Lisp_Object raw */);
extern Lisp_Object Fdocumentation_property (Lisp_Object sym, Lisp_Object prop
                                            /* , Lisp_Object raw */);

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
extern Lisp_Object Fmake_extent (Lisp_Object from, Lisp_Object to,
                                 Lisp_Object buffer);
extern Lisp_Object Fset_extent_data (Lisp_Object extent, Lisp_Object data);
extern Lisp_Object replicate_extents (int opoint, int length, 
                                      struct buffer *buf);
extern void print_extent_or_replica (Lisp_Object obj, 
                                     Lisp_Object printcharfun, int escape);


/* defined in xfns.c */

extern Lisp_Object Fx_create_screen (Lisp_Object, Lisp_Object);
extern Lisp_Object Fx_focus_screen (Lisp_Object, Lisp_Object);
extern Lisp_Object Fx_get_resource (Lisp_Object, Lisp_Object,
				    Lisp_Object, Lisp_Object);
extern Lisp_Object Fx_valid_color_name_p (Lisp_Object, Lisp_Object);
extern Lisp_Object Fx_valid_keysym_name_p (Lisp_Object);
extern Lisp_Object Fx_set_screen_pointer (Lisp_Object, Lisp_Object,
					  Lisp_Object, Lisp_Object);
extern Lisp_Object Fx_open_connection (Lisp_Object);
extern Lisp_Object Fx_window_id (Lisp_Object);
extern Lisp_Object Fx_close_current_connection (void);
extern Lisp_Object Fx_debug_mode (Lisp_Object);
extern void Xatoms_of_xfns (void);

/* Defined in faces.c */

extern unsigned long
load_pixmap (struct screen *s, Lisp_Object name,
	     unsigned int *wP, unsigned int *hP, unsigned int *dP,
	     unsigned long *maskP);

/* Defined in xselect.c */

extern void Xatoms_of_xselect (void);
extern Lisp_Object Fx_own_selection_internal (Lisp_Object, Lisp_Object);
extern Lisp_Object Fx_get_selection_internal (Lisp_Object, Lisp_Object);
extern Lisp_Object Fx_disown_selection_internal (Lisp_Object, Lisp_Object);
extern Lisp_Object Fx_selection_owner_p (Lisp_Object);
extern Lisp_Object Fx_selection_exists_p (Lisp_Object);
extern Lisp_Object Fx_get_cutbuffer_internal (Lisp_Object);
extern Lisp_Object Fx_store_cutbuffer_internal (Lisp_Object, Lisp_Object);
extern Lisp_Object Fx_rotate_cutbuffers_internal (Lisp_Object);



extern void message ();

extern int unexec ();

extern int pixel_to_glyph_translation (struct screen *s,
				       unsigned int, unsigned int,
				       int *, int *,
				       struct window **,
				       int *, int *, Lisp_Object *, int *);
extern void redisplay (void);
extern void redisplay_preserving_echo_area (void);

extern Lisp_Object Fsend_string_to_terminal (Lisp_Object);

extern void format_event_object (char *, struct Lisp_Event *, int);

extern Lisp_Object Fkey_binding (Lisp_Object);

extern Lisp_Object Fwhere_is_internal (Lisp_Object, Lisp_Object, Lisp_Object,
				       Lisp_Object, Lisp_Object);
extern Lisp_Object Fkey_description (Lisp_Object);
extern Lisp_Object Fevent_window (Lisp_Object);


/* Defined in eval.c */
extern Lisp_Object Qautoload, Qexit, Qinteractive, Qcommandp, Qdefun, Qmacro;
extern Lisp_Object Vinhibit_quit, Vquit_flag, Qinhibit_quit;
extern Lisp_Object Vmocklisp_arguments, Qmocklisp, Qmocklisp_arguments;
extern Lisp_Object Vautoload_queue;
extern Lisp_Object Vrun_hooks;
extern Lisp_Object Fuser_variable_p (Lisp_Object);
extern Lisp_Object Finteractive_p (void);
extern void signal_error (Lisp_Object sig, Lisp_Object data);
extern Lisp_Object Fprogn (Lisp_Object args);
extern Lisp_Object Fcommandp (Lisp_Object obj);
extern Lisp_Object Feval (Lisp_Object form);
extern Lisp_Object Fapply (int nargs, Lisp_Object *args);
extern Lisp_Object Ffuncall (int nargs, Lisp_Object *args);
extern Lisp_Object Fbacktrace (Lisp_Object stream);
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
extern Lisp_Object Fsignal (Lisp_Object signame, Lisp_Object data);
/* C Code should be using internal_catch, record_unwind_p, condition_case_1 */
/* extern Lisp_Object Fcatch (Lisp_Object args); */
/* extern Lisp_Object Funwind_protect (Lisp_Object args); */
/* extern Lisp_Object Fcondition_case (Lisp_Object args); */
extern Lisp_Object Fthrow (Lisp_Object tag, Lisp_Object val);
extern Lisp_Object internal_catch (Lisp_Object tag, 
                                   Lisp_Object (*func) (Lisp_Object arg),
                                   Lisp_Object arg);
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
extern void record_unwind_protect (Lisp_Object (*function) (Lisp_Object),
                                   Lisp_Object arg);
extern void do_autoload (Lisp_Object fundef, Lisp_Object funname);
extern void error ();

#ifdef MAINTAIN_ENVIRONMENT
extern char *egetenv (const char *);
extern char *getenv (const char *);
#else
#define egetenv getenv
#endif

#endif /* _EMACSFNS_H_ */
