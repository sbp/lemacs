/* Various display-releated prototypes.
   Copyright (C) 1993 Free Software Foundation, Inc.

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

#ifndef _EMACS_DISPMISC_H_
#define _EMACS_DISPMISC_H_

/* #include "dispextern.h" -- this requires lisp.h */

/*>>> copied from sysdep.h */
extern void request_sigio (void);
extern void unrequest_sigio (void);

struct screen;
struct window;
struct line_header;
struct char_block;

/* defined in termcap.c */
#ifndef tputs
extern void tputs (const char *string, int nlines, void (*outfun) (int));
extern int tgetent (char *bp, char *name);
extern int tgetflag (const char *cap);
extern int tgetnum (const char *cap);
extern const char *tgetstr (const char *cap, char **area);
#endif

/* defined in tparam.c */
extern char *tparam (const char *string, char *outstring, int len,
                     int arg0, int arg1, int arg2, int arg4);

/* defined in term.c */
extern int initial_screen_is_tty (void);
extern void set_terminal_modes (void);
extern void reset_terminal_modes (void);
extern void cursor_to (struct line_header *l, struct char_block *cb, int row,
		       int col, struct window *win, struct screen *s);
extern void calculate_costs (struct screen *);
extern void term_init (char *terminal_type);
extern void set_terminal_window (int size);
extern void ins_del_lines (int vpos, int n);
extern int string_cost (const char *str);
extern int per_line_cost (const char *str);
extern void clear_screen (void);
extern void clear_end_of_line (int first_unused_hpos);
extern int text_width (Lisp_Object font, unsigned char *s,
		       int l);

extern void delete_glyphs (int n);
extern void insert_glyphs (int hpos, int vpos, int len);
extern void insert_spaceglyphs (int hpos, int vpos, int len);
extern void write_glyphs (int hpos, int vpos, int len);

extern void update_begin (struct screen *);
extern void update_end (struct screen *);

extern void shift_region (struct window *w, struct line_header *start,
			  struct line_header *end);
extern void update_line (struct window *w, struct line_header *l,
			 struct char_block *cb, struct char_block *end,
			 char clear, int line_type);
extern void clear_window_end (struct window *w, int ystart, int yend);
extern void insert_chars (struct window *w, struct line_header *l,
			  struct char_block *new, struct char_block *cb,
			  struct char_block *end, char clear);

/* Screen currently being updated, or 0 if not in an update. */
extern struct screen *updating_screen;


/* defined in menubar.c */
extern int menubar_has_changed;




/* defined in xterm.c */
extern void CXTupdate_begin (struct window *w);
extern void CXTupdate_end (struct window *w);

/* defined in xdisp.c */
extern void redisplay_preserving_echo_area (void);
extern void initialize_first_screen (void);
extern struct line_header *get_line (void);
extern void free_line (struct line_header *);
extern struct char_block *get_char_block (void);
extern void free_char_blocks (struct char_block *b, struct char_block *e);
extern void update_window (struct window *);
extern void update_window_attributes (struct window *);
/* Can't declare this, as including file might not grok Lisp_Object
   extern void clear_display_structs (Lisp_Object window); */
extern int minibuf_prompt_width;
extern int minibuf_prompt_pix_width;


/* defined in dispnew.c */
extern int direct_output_forward_char (int n);
/* Can't declare this, as including file might not grok Lisp_Object
   extern int pixel_to_glyph_translation (struct screen *, 
   				      unsigned int pix_x, unsigned int pix_y,
                                      int *x, int *y, 
                                      struct window **,
                                      int *bufp, 
                                      int *gly, 
                                      Lisp_Object *class,
                                      int *begin_p);
*/
extern void change_screen_size (struct screen *, 
                                int newlength, int newwidth, 
                                int pretend);
struct buffer;
extern struct face *get_face_at_bufpos (struct screen *, struct buffer *,
                                        register int, int *, int *);

/* defined in scroll.c */
extern void do_line_insertion_deletion_costs (struct screen *,
                                              const char *ins_line_string,
                                              const char *multi_ins_string,
                                              const char *del_line_string, 
                                              const char *multi_del_string,
                                              const char *setup_string,
                                              const char *cleanup_string,
                                              int coefficient);
extern int scroll_cost (struct screen *, int from, int to, int amount);
extern int scroll_screen_lines (int, int, int, struct window *);
extern void scrolling_1 (struct screen *, int window_size, 
                         int unchanged_at_top, int unchanged_at_bottom,
                         int *draw_cost, 
                         int *old_hash, int *new_hash, 
                         int free_at_end);
extern int scrolling_max_lines_saved (int start, int end, 
                                      int *oldhash, int *newhash, 
                                      int *cost);

/* defined in faces.c */
extern void flush_face_cache (void);
/* This gets set to 1 when the user has changed fonts, colors, etc.,
   meaning that our cached GCs need to be rebuilt. */
extern int face_cache_invalid;



/* defined in menubar.c */
extern void update_psheets (void);
extern void update_screen_menubars (void);

#endif /* _EMACS_DISPMISC_H_ */
