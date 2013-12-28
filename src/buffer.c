/* Buffer manipulation primitives for GNU Emacs.
   Copyright (C) 1985-1993 Free Software Foundation, Inc.

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


#include <stdio.h>		/* For sprintf */
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/param.h>

#ifndef MAXPATHLEN
/* in 4.1, param.h fails to define this. */
#define MAXPATHLEN 1024
#endif /* not MAXPATHLEN */

#include "config.h"
#include "lisp.h"
#include "window.h"
#include "commands.h"
#include "buffer.h"
#include "insdel.h"
#include "screen.h"
#include "syntax.h"

#include "extents.h"          /* for EXTENTP, for weird mark_buffer kludge */

#include "process.h"            /* for kill_buffer_processes */

struct buffer *current_buffer;		/* the current buffer */

/* First buffer in chain of all buffers (in reverse order of creation).
   Threaded through ->next.  */

struct buffer *all_buffers;

/* This structure holds the default values of the buffer-local variables
   defined with DEFVAR_PER_BUFFER, that have special slots in each buffer.
   The default value occupies the same slot in this structure
   as an individual buffer's value occupies in that buffer.
   Setting the default value also goes through the alist of buffers
   and stores into each buffer that does not say it has a local value.  */
struct buffer buffer_defaults;

/* A Lisp_Object pointer to the above, used for staticpro */
static Lisp_Object Vbuffer_defaults;

/* This structure marks which slots in a buffer have corresponding
   default values in buffer_defaults.
   Each such slot has a nonzero value in this structure.
   The value has only one nonzero bit.

   When a buffer has its own local value for a slot,
   the bit for that slot (found in the same slot in this structure)
   is turned on in the buffer's local_var_flags slot.

   If a slot in this structure is -1, then even though there may
   be a DEFVAR_PER_BUFFER for the slot, there is no default value for it;
   and the corresponding slot in buffer_defaults is not used.

   If a slot is -2, then there is no DEFVAR_PER_BUFFER for it,
   but there is a default value which is copied into each buffer.

   If a slot in this structure corresponding to a DEFVAR_PER_BUFFER is
   zero, that is a bug */
struct buffer buffer_local_flags;

/* This structure holds the names of symbols whose values may be
   buffer-local.  It is indexed and accessed in the same way as the above. */
struct buffer buffer_local_symbols;

/* A Lisp_Object pointer to the above, used for staticpro */
static Lisp_Object Vbuffer_local_symbols;

Lisp_Object Fset_buffer ();

/* Alist of all buffer names vs the buffers. */
/* This used to be a variable, but is no longer,
   to prevent lossage due to user rplac'ing this alist or its elements.
   Note that there is a per-screen copy of this as well; the screen slot
   and the global variable contain the same data, but possibly in different
   orders, so that the buffer ordering can be per-screen.
  */
Lisp_Object Vbuffer_alist;

/* Functions to call before and after each text change. */
Lisp_Object Vbefore_change_function;
Lisp_Object Vafter_change_function;

/* Function to call before changing an unmodified buffer.  */
Lisp_Object Vfirst_change_function;

Lisp_Object Qfundamental_mode;
Lisp_Object Qmode_class;
Lisp_Object Qpermanent_local;

Lisp_Object Qprotected_field;

Lisp_Object QSFundamental;	/* A string "Fundamental" */

Lisp_Object Qkill_buffer_hook;

Lisp_Object Qrename_auto_save_file;
Lisp_Object Qdelete_auto_save_files; /* Should really be DEFVAR_LISP */

static void
nsberror (spec)
     Lisp_Object spec;
{
  if (STRINGP (spec))
    error ("No buffer named %s", XSTRING (spec)->data);
  error ("Invalid buffer argument");
}

DEFUN ("buffer-list", Fbuffer_list, Sbuffer_list, 0, 1, 0,
  "Return a list of all existing live buffers.\n\
The order is specific to the selected screen; if the optional SCREEN\n\
argument is provided, the ordering for that screen is returned instead.\n\
If the SCREEN argument is t, then the global (non-screen) ordering is\n\
returned instead.")
  (screen)
  Lisp_Object screen;
{
  Lisp_Object list;
  if (EQ (screen, Qt))
    list = Vbuffer_alist;
  else if (NILP (screen))
    list = selected_screen->buffer_alist;
  else
    {
      CHECK_SCREEN (screen, 0);
      list = XSCREEN (screen)->buffer_alist;
    }
  return Fmapcar (Qcdr, list);
}

DEFUN ("get-buffer", Fget_buffer, Sget_buffer, 1, 1, 0,
  "Return the buffer named NAME (a string).\n\
If there is no live buffer named NAME, return nil.\n\
NAME may also be a buffer; if so, the value is that buffer.")
  (name)
     register Lisp_Object name;
{
  if (BUFFERP (name))
    return name;
  CHECK_STRING (name, 0);

  return Fcdr (Fassoc (name, Vbuffer_alist));
}


int find_file_compare_truenames;
int find_file_use_truenames;


DEFUN ("get-file-buffer", Fget_file_buffer, Sget_file_buffer, 1, 1, 0,
  "Return the buffer visiting file FILENAME (a string).\n\
If there is no such live buffer, return nil.")
  (filename)
     register Lisp_Object filename;
{
  register Lisp_Object tail, buf, tem;
  CHECK_STRING (filename, 0);
  filename = Fexpand_file_name (filename, Qnil);

  if (find_file_compare_truenames || find_file_use_truenames)
    {
      Lisp_Object fn = Ftruename (filename, Qnil);
      if (NILP (fn))
	{
	  Lisp_Object dn = Ffile_name_directory (filename);
	  fn = Ftruename (dn, Qnil);
	  if (! NILP (fn)) dn = fn;
	  fn = Fexpand_file_name (Ffile_name_nondirectory (filename), dn);
	}
      filename = fn;
    }

  for (tail = Vbuffer_alist; CONSP (tail); tail = XCONS (tail)->cdr)
    {
      buf = Fcdr (XCONS (tail)->car);
      if (!BUFFERP (buf)) continue;
      if (!STRINGP (XBUFFER (buf)->filename)) continue;
      tem = Fstring_equal (filename,
			   (find_file_compare_truenames
			    ? XBUFFER (buf)->truename
			    : XBUFFER (buf)->filename));
      if (!NILP (tem))
	return buf;
    }
  return Qnil;
}


static void
push_buffer_alist (name, buf)
     Lisp_Object name, buf;
{
  Lisp_Object cons = Fcons (name, buf);
  Lisp_Object rest;
  Vbuffer_alist = nconc2 (Vbuffer_alist, Fcons (cons, Qnil));
#ifdef MULTI_SCREEN
  if (! Vscreen_list) /* ick: this is 0 early in temacs startup */
    return;
  for (rest = Vscreen_list; !NILP (rest); rest = XCONS (rest)->cdr)
    {
      struct screen *s;
      if (!SCREENP (XCONS (rest)->car))
	abort ();
      s = XSCREEN (XCONS (rest)->car);
      s->buffer_alist = nconc2 (s->buffer_alist, Fcons (cons, Qnil));
    }
#endif
}

static void
delete_from_buffer_alist (buf)
     Lisp_Object buf;
{
  Lisp_Object cons = Frassq (buf, Vbuffer_alist);
  Lisp_Object rest;
  if (NILP (cons))
    return; /* abort() ? */
  Vbuffer_alist = delq_no_quit (cons, Vbuffer_alist);
#ifdef MULTI_SCREEN
  for (rest = Vscreen_list; !NILP (rest); rest = XCONS (rest)->cdr)
    {
      struct screen *s;
      if (!SCREENP (XCONS (rest)->car))
	abort ();
      s = XSCREEN (XCONS (rest)->car);
      s->buffer_alist = delq_no_quit (cons, s->buffer_alist);
    }
#endif
}


extern void init_buffer_cached_stack (struct buffer* b);

DEFUN ("get-buffer-create", Fget_buffer_create, Sget_buffer_create, 1, 1, 0,
  "Return the buffer named NAME, or create such a buffer and return it.\n\
A new buffer is created if there is no live buffer named NAME.\n\
If NAME starts with a space, the new buffer does not keep undo information.\n\
If NAME is a buffer instead of a string, then it is the value returned.\n\
The value is never nil.")  
  (name)
     register Lisp_Object name;
{
  register Lisp_Object buf, function, tem;
  int count = specpdl_depth;
  register struct buffer *b;

  buf = Fget_buffer (name);
  if (!NILP (buf))
    return buf;

  b = (struct buffer *) xmalloc (sizeof (struct buffer));

  XSET (buf, Lisp_Buffer, b);

  BUF_GAP_SIZE (b) = 20;
  BUFFER_ALLOC (BUF_BEG_ADDR (b), BUF_GAP_SIZE (b));
  if (! BUF_BEG_ADDR (b))
    memory_full ();

  BUF_PT (b) = 1;
  BUF_GPT (b) = 1;
  BUF_BEGV (b) = 1;
  BUF_ZV (b) = 1;
  BUF_Z (b) = 1;
  BUF_MODIFF (b) = 1;
  BUF_FACECHANGE (b) = 1;

  /* Put this on the chain of all buffers including killed ones.  */
  b->next = all_buffers;
  all_buffers = b;

  b->save_length = make_number (0);
  b->last_window_start = 1;

  b->name = name;
  if (XSTRING (name)->data[0] != ' ')
    b->undo_list = Qnil;
  else
    b->undo_list = Qt;

  reset_buffer (b);

  /* initialize the extent cache */
  b->cached_stack = 0;
  init_buffer_cached_stack (b);

  /* Put this in the alist of all live buffers.  */
  push_buffer_alist (name, buf);

  b->extents = Qnil;
  b->markers = Qnil;
  b->mark = Fmake_marker ();
  b->point_marker = Fmake_marker ();
  Fset_marker (b->point_marker, make_number (1), buf);

  function = XBUFFER (Vbuffer_defaults)->major_mode;
  if (NILP (function))
    {
      tem = Fget (current_buffer->major_mode, Qmode_class);
      if (EQ (tem, Qnil))
	function = current_buffer->major_mode;
    }

  if (NILP (function) || EQ (function, Qfundamental_mode))
    return buf;

  /* To select a nonfundamental mode,
     select the buffer temporarily and then call the mode function. */

  record_unwind_protect (save_excursion_restore, save_excursion_save ());

  Fset_buffer (buf);
  call0 (function);

  return unbind_to (count, buf);
}


static void
reset_buffer_local_variables (b)
     register struct buffer *b;
{
  /* Reset the major mode to Fundamental, together with all the
     things that depend on the major mode.
     default-major-mode is handled at a higher level.
     We ignore it here.  */
  b->major_mode = Qfundamental_mode;
  b->keymap = Qnil;
  b->abbrev_table = Vfundamental_mode_abbrev_table;
  b->mode_name = QSFundamental;
  b->minor_modes = Qnil;
  b->downcase_table = Vascii_downcase_table;
  b->upcase_table = Vascii_upcase_table;
  b->case_canon_table = Vascii_downcase_table;
  b->case_eqv_table = Vascii_upcase_table;
#if 0
  b->sort_table = XSTRING (Vascii_sort_table);
  b->folding_sort_table = XSTRING (Vascii_folding_sort_table);
#endif /* 0 */

  /* Reset all per-buffer variables to their defaults.  */
  b->local_var_alist = Qnil;
  b->local_var_flags = 0;

  /* For each slot that has a default value,
     copy that into the slot.  */

  {
    struct buffer *def = XBUFFER (Vbuffer_defaults);
    int tem;
#define MARKED_SLOT(slot) \
    tem = *((int *) &(buffer_local_flags.slot)); \
    if (tem > 0 || tem == -2) \
      b->slot = def->slot;
#include "bufslots.h"
#undef MARKED_SLOT
  }
}

/* Reinitialize everything about a buffer except its name and contents.  */

void
reset_buffer (b)
     register struct buffer *b;
{
  b->filename = Qnil;
  b->truename = Qnil;
  b->directory = (current_buffer) ? current_buffer->directory : Qnil;
  b->modtime = 0;
  b->save_modified = 1;
  b->save_length = make_number (0);
  b->last_window_start = 1;
  b->backed_up = Qnil;
  b->auto_save_modified = 0;
  b->auto_save_file_name = Qnil;
  b->read_only = Qnil;
  b->dedicated_screen = Qnil;
  reset_buffer_local_variables(b);
}


DEFUN ("generate-new-buffer-name", Fgenerate_new_buffer_name, Sgenerate_new_buffer_name,
  1, 1, 0,
  "Return a string that is the name of no existing buffer based on NAME.\n\
If there is no live buffer named NAME, then return NAME.\n\
Otherwise modify name by appending `<NUMBER>', incrementing NUMBER\n\
until an unused name is found, and then return that name.")
 (name)
     register Lisp_Object name;
{
  register Lisp_Object gentemp, tem;
  int count;
  char number[10];

  CHECK_STRING (name, 0);

  tem = Fget_buffer (name);
  if (NILP (tem))
    return (name);

  count = 1;
  while (1)
    {
      sprintf (number, "<%d>", ++count);
      gentemp = concat2 (name, build_string (number));
      tem = Fget_buffer (gentemp);
      if (NILP (tem))
	return (gentemp);
    }
}


DEFUN ("buffer-name", Fbuffer_name, Sbuffer_name, 0, 1, 0,
  "Return the name of BUFFER, as a string.\n\
With no argument or nil as argument, return the name of the current buffer.")
  (buffer)
     register Lisp_Object buffer;
{
  if (NILP (buffer))
    return current_buffer->name;
  CHECK_BUFFER (buffer, 0);
  return XBUFFER (buffer)->name;
}

DEFUN ("buffer-file-name", Fbuffer_file_name, Sbuffer_file_name, 0, 1, 0,
  "Return name of file BUFFER is visiting, or nil if none.\n\
No argument or nil as argument means use the current buffer.")
  (buffer)
     register Lisp_Object buffer;
{
  if (NILP (buffer))
    return current_buffer->filename;
  CHECK_BUFFER (buffer, 0);
  return XBUFFER (buffer)->filename;
}

DEFUN ("buffer-local-variables", Fbuffer_local_variables,
  Sbuffer_local_variables, 0, 1, 0,
  "Return an alist of variables that are buffer-local in BUFFER.\n\
Each element looks like (SYMBOL . VALUE) and describes one variable.\n\
Note that storing new VALUEs in these elements doesn't change the variables.\n\
No argument or nil as argument means use current buffer as BUFFER.")
  (buffer)
     register Lisp_Object buffer;
{
  register struct buffer *buf;
  register Lisp_Object val;

  if (NILP (buffer))
    buf = current_buffer;
  else
    {
      CHECK_BUFFER (buffer, 0);
      buf = XBUFFER (buffer);
    }

  {
    /* Reference each variable in the alist in our current buffer.
       If inquiring about the current buffer, this gets the current values,
       so store them into the alist so the alist is up to date.
       If inquiring about some other buffer, this swaps out any values
       for that buffer, making the alist up to date automatically.  */
    register Lisp_Object tem;
    for (tem = buf->local_var_alist; CONSP (tem); tem = XCONS (tem)->cdr)
      {
	Lisp_Object v1 = Fsymbol_value (XCONS (XCONS (tem)->car)->car);
	if (buf == current_buffer)
	  XCONS (XCONS (tem)->car)->cdr = v1;
      }
  }

  /* Make a copy of the alist, to return it.  */
  val = Fcopy_alist (buf->local_var_alist);

  /* Add on all the variables stored in special slots.  */
  {
    int mask;
    struct buffer *syms = XBUFFER (Vbuffer_local_symbols);
#define MARKED_SLOT(slot) \
    mask = *((int *) &(buffer_local_flags.slot)); \
    if (mask == -1 || (buf->local_var_flags & mask)) \
      val = Fcons (Fcons (syms->slot, buf->slot), val);
#include "bufslots.h"
#undef MARKED_SLOT
  }
  return (val);
}

DEFUN ("buffer-dedicated-screen", Fbuffer_dedicated_screen, Sbuffer_dedicated_screen,
       0, 1, 0,
       "Return the screen dedicated to this BUFFER, or nil if there is none.\n\
No argument or nil as argument means use current buffer as BUFFER.")
  (buffer)
     register Lisp_Object buffer;
{
  register struct buffer *buf;
  if (NILP (buffer))
    buf = current_buffer;
  else
    {
      CHECK_BUFFER (buffer, 0);
      buf = XBUFFER (buffer);
    }

  return buf->dedicated_screen;
}

DEFUN ("set-buffer-dedicated-screen", Fset_buffer_dedicated_screen,
       Sset_buffer_dedicated_screen,
       2, 2, 0,
       "For this BUFFER, set the SCREEN dedicated to it.\n\
SCREEN must be a screen or nil.")
  (buffer, screen)
     register Lisp_Object buffer, screen;
{
  register struct buffer *buf;
  if (NILP (buffer))
    buf = current_buffer;
  else
    {
      CHECK_BUFFER (buffer, 0);
      buf = XBUFFER (buffer);
    }

  if (!NILP (screen))
    CHECK_SCREEN (screen, 0);

  return buf->dedicated_screen = screen;
}



DEFUN ("buffer-modified-p", Fbuffer_modified_p, Sbuffer_modified_p,
  0, 1, 0,
  "Return t if BUFFER was modified since its file was last read or saved.\n\
No argument or nil as argument means use current buffer as BUFFER.")
  (buffer)
     register Lisp_Object buffer;
{
  register struct buffer *buf;
  if (NILP (buffer))
    buf = current_buffer;
  else
    {
      CHECK_BUFFER (buffer, 0);
      buf = XBUFFER (buffer);
    }

  return buf->save_modified < BUF_MODIFF (buf) ? Qt : Qnil;
}

DEFUN ("set-buffer-modified-p", Fset_buffer_modified_p, Sset_buffer_modified_p,
  1, 1, 0,
  "Mark current buffer as modified or unmodified according to FLAG.\n\
A non-nil FLAG means mark the buffer modified.")
  (flag)
     register Lisp_Object flag;
{
  register int already;
  register Lisp_Object fn;
#ifdef ENERGIZE
  Lisp_Object starting_flag = 
    (current_buffer->save_modified < BUF_MODIFF (current_buffer))?Qt:Qnil;
  Lisp_Object argument_flag = (NILP (flag))?Qnil:Qt;
#endif  

#ifdef CLASH_DETECTION
  /* If buffer becoming modified, lock the file.
     If buffer becoming unmodified, unlock the file.  */

  fn = current_buffer->filename;
  if (!NILP (fn))
    {
      already = current_buffer->save_modified < MODIFF;
      if (!already && !NILP (flag))
	lock_file (fn);
      else if (already && NILP (flag))
	unlock_file (fn);
    }
#endif                          /* CLASH_DETECTION */

  current_buffer->save_modified = NILP (flag) ? MODIFF : 0;
  redraw_mode_line++;

#ifdef ENERGIZE
  /* don't send any notification if we are "setting" the modification bit
     to be the same as it already was */
  if (starting_flag != argument_flag)
    {
      extern Lisp_Object Venergize_buffer_modified_hook;
      safe_funcall_hook (Venergize_buffer_modified_hook, 3, flag,
			 make_number (BEG), make_number (Z));
    }
#endif                          /* ENERGIZE */

  return flag;
}

DEFUN ("buffer-modified-tick", Fbuffer_modified_tick, Sbuffer_modified_tick,
  0, 1, 0,
  "Return BUFFER's tick counter, incremented for each change in text.\n\
Each buffer has a tick counter which is incremented each time the text in\n\
that buffer is changed.  It wraps around occasionally.\n\
No argument or nil as argument means use current buffer as BUFFER.")
  (buffer)
     register Lisp_Object buffer;
{
  register struct buffer *buf;
  if (NILP (buffer))
    buf = current_buffer;
  else
    {
      CHECK_BUFFER (buffer, 0);
      buf = XBUFFER (buffer);
    }

  return make_number (BUF_MODIFF (buf));
}

DEFUN ("rename-buffer", Frename_buffer, Srename_buffer, 1, 2,
       "sRename buffer (to new name): ",
  "Change current buffer's name to NEWNAME (a string).\n\
If second arg DISTINGUISH is nil or omitted, it is an error if a\n\
buffer named NEWNAME already exists.\n\
If DISTINGUISH is non-nil, come up with a new name using\n\
`generate-new-buffer-name'.\n\
Return the name we actually gave the buffer.\n\
This does not change the name of the visited file (if any).")
  (name, distinguish)
     register Lisp_Object name, distinguish;
{
  register Lisp_Object tem, buf;

  CHECK_STRING (name, 0);
  tem = Fget_buffer (name);
  if (XBUFFER (tem) == current_buffer)
    return (current_buffer->name);
  if (!NILP (tem))
    {
      if (!NILP (distinguish))
	name = Fgenerate_new_buffer_name (name);
      else
	error ("Buffer name \"%s\" is in use", XSTRING (name)->data);
    }

  current_buffer->name = name;
  XSET (buf, Lisp_Buffer, current_buffer);
  /* The aconses in the Vbuffer_alist are shared with screen->buffer_alist,
     so this will change it in the per-screen ordering as well. */
  Fsetcar (Frassq (buf, Vbuffer_alist), name);
  if (NILP (current_buffer->filename) && !NILP (current_buffer->auto_save_file_name))
    call0 (Qrename_auto_save_file);
  return (name);
}

DEFUN ("other-buffer", Fother_buffer, Sother_buffer, 0, 2, 0,
  "Return most recently selected buffer other than BUFFER.\n\
Buffers not visible in windows are preferred to visible buffers.\n\
If no other buffer exists, the buffer `*scratch*' is returned.\n\
If BUFFER is omitted or nil, some interesting buffer is returned.\n\
\n\
The ordering is for this screen; If second optional argument SCREEN\n\
is provided, then the ordering is for that screen.  If the second arg\n\
is t, then the global ordering is returned.")
  (buffer, screen)
     register Lisp_Object buffer, screen;
{
  register Lisp_Object tail, buf, notsogood, tem;
  Lisp_Object alist;

  notsogood = Qnil;

#ifdef MULTI_SCREEN
  if (EQ (screen, Qt))
    alist = Vbuffer_alist;
  else if (NILP (screen))
    alist = selected_screen->buffer_alist;
  else
    {
      CHECK_SCREEN (screen, 0);
      alist = XSCREEN (screen)->buffer_alist;
    }
#else
  alist = Vscreen_alist;
#endif

  for (tail = alist; !NILP (tail); tail = Fcdr (tail))
    {
      buf = Fcdr (Fcar (tail));
      if (EQ (buf, buffer))
	continue;
      if (XSTRING (XBUFFER (buf)->name)->data[0] == ' ')
	continue;
      tem = Fget_buffer_window (buf, screen, Qnil);
      if (NILP (tem))
	return buf;
      if (NILP (notsogood))
	notsogood = buf;
    }
  if (!NILP (notsogood))
    return notsogood;
  return Fget_buffer_create (build_string ("*scratch*"));
}

DEFUN ("buffer-disable-undo", Fbuffer_disable_undo, Sbuffer_disable_undo, 1, 1, 0,
  "Make BUFFER stop keeping undo information.\n\
Any undo records it already has are discarded.")
  (buffer)
     register Lisp_Object buffer;
{
  CHECK_BUFFER (buffer, 0);
  XBUFFER (buffer)->undo_list = Qt;
  return Qnil;
}

DEFUN ("buffer-enable-undo", Fbuffer_enable_undo, Sbuffer_enable_undo,
       0, 1, "",
  "Start keeping undo information for buffer BUFFER.\n\
No argument or nil as argument means do this for the current buffer.")
  (buffer)
     register Lisp_Object buffer;
{
  register struct buffer *b;
  register Lisp_Object buf1;

  if (NILP (buffer))
    b = current_buffer;
  else
    {
      buf1 = Fget_buffer (buffer);
      if (NILP (buf1)) nsberror (buffer);
      b = XBUFFER (buf1);
    }

  if (EQ (b->undo_list, Qt))
    b->undo_list = Qnil;

  return Qnil;
}

#ifdef subprocesses
extern void kill_buffer_processes (Lisp_Object);
#endif

DEFUN ("kill-buffer", Fkill_buffer, Skill_buffer, 1, 1, "bKill buffer: ",
  "Kill the buffer BUFFER.\n\
The argument may be a buffer or may be the name of a buffer.\n\
An argument of nil means kill the current buffer.\n\n\
Value is t if the buffer is actually killed, nil if user says no.\n\n\
The value of `kill-buffer-hook' (which may be local to that buffer),\n\
if not void, is a list of functions to be called, with no arguments,\n\
before the buffer is actually killed.  The buffer to be killed is current\n\
when the hook functions are called.\n\
\n\
Any processes that have this buffer as the `process-buffer' are killed\n\
with `delete-process'.")
  (bufname)
     Lisp_Object bufname;
{
  Lisp_Object buf;
  register struct buffer *b;
  register Lisp_Object tem;
  struct gcpro gcpro1, gcpro2;

  if (NILP (bufname))
    buf = Fcurrent_buffer ();
  else
    buf = Fget_buffer (bufname);
  if (NILP (buf))
    nsberror (bufname);

  b = XBUFFER (buf);

  /* Don't kill the minibuffer now current.  */
  if (EQ (buf, Vminibuffer_zero))
    return Qnil;

  /* Query if the buffer is still modified.  */
  if (INTERACTIVE && !NILP (b->filename)
      && BUF_MODIFF (b) > b->save_modified)
    {
      GCPRO2 (buf, bufname);
      tem = call1 (Qyes_or_no_p,
                   (format1 ("Buffer %s modified; kill anyway? ",
                             XSTRING (b->name)->data)));
      UNGCPRO;
      if (NILP (tem))
	return Qnil;
    }

  /* Run kill-buffer hooks with the buffer to be killed temporarily selected,
     unless the buffer is already dead.
   */
  if (!NILP (b->name))
    {
      int count = specpdl_depth;

      record_unwind_protect (save_excursion_restore, save_excursion_save ());
      set_buffer_internal (b);
      call1 (Vrun_hooks, Qkill_buffer_hook);
#ifdef HAVE_X_WINDOWS
      /* If an X selection was in this buffer, disown it.
	 We could have done this by simply adding this function to the
	 kill-buffer-hook, but the user might mess that up.
	 */
      if (!NILP (Vwindow_system))
	call0 (intern ("xselect-kill-buffer-hook"));
#endif
      unbind_to (count, Qnil);
    }

  /* We have no more questions to ask.  Verify that it is valid
     to kill the buffer.  This must be done after the questions
     since anything can happen within yes-or-no-p.  */

  if (NILP (b->name))
    return Qnil;

  /* Make this buffer not be current.
     In the process, notice if this is the sole visible buffer
     and give up if so.  */
  if (b == current_buffer)
    {
      tem = Fother_buffer (buf, Qnil);
      Fset_buffer (tem);
      if (b == current_buffer)
	return Qnil;
    }

  /* Now there is no question: we can kill the buffer.  */

#ifdef CLASH_DETECTION
  /* Unlock this buffer's file, if it is locked.  */
  unlock_buffer (b);
#endif /* CLASH_DETECTION */

#ifdef subprocesses
  kill_buffer_processes (buf);
#endif /* subprocesses */

  tem = Vinhibit_quit;
  Vinhibit_quit = Qt;
  delete_from_buffer_alist (buf);
  Freplace_buffer_in_windows (buf);
  Vinhibit_quit = tem;

  /* Delete any auto-save file.  */
  if (STRINGP (b->auto_save_file_name))
    {
      Lisp_Object tem;
      tem = Fsymbol_value (Qdelete_auto_save_files);
      if (! NILP (tem))
	unlink ((char *) XSTRING (b->auto_save_file_name)->data);
    }

  /* Unchain all markers of this buffer
     and leave them pointing nowhere.  */
  for (tem = b->markers; !EQ (tem, Qnil); )
    {
      register struct Lisp_Marker *m = XMARKER (tem);
      m->buffer = 0;
      tem = m->chain;
      m->chain = Qnil;
    }
  b->markers = Qnil;
  b->name = Qnil;
  BUFFER_FREE (BUF_BEG_ADDR (b));
  b->undo_list = Qnil;
  b->extents = Qnil;
  free_buffer_cached_stack (b);

  return Qt;
}

/* Put the element for buffer BUF at the front of buffer-alist.
   This is done when a buffer is selected "visibly".
   It keeps buffer-alist in the order of recency of selection
   so that other_buffer will return something nice.  */

void
record_buffer (buf)
     Lisp_Object buf;
{
  register Lisp_Object link, prev;

  prev = Qnil;
  for (link = Vbuffer_alist; CONSP (link); link = XCONS (link)->cdr)
    {
      if (EQ (XCONS (XCONS (link)->car)->cdr, buf))
	break;
      prev = link;
    }
  /* Effectively do Vbuffer_alist = delq_no_quit (link, Vbuffer_alist) */
  if (NILP (prev))
    Vbuffer_alist = XCONS (Vbuffer_alist)->cdr;
  else
    XCONS (prev)->cdr = XCONS (XCONS (prev)->cdr)->cdr;
  XCONS(link)->cdr = Vbuffer_alist;
  Vbuffer_alist = link;

#ifdef MULTI_SCREEN
  /* That was the global one.  Now do the same thing for the
     per-screen buffer-alist. */
  prev = Qnil;
  for (link = selected_screen->buffer_alist; CONSP (link);
       link = XCONS (link)->cdr)
    {
      if (EQ (XCONS (XCONS (link)->car)->cdr, buf))
	break;
      prev = link;
    }
  /* Effectively do s->buffer_alist = delq_no_quit (link, s->buffer_alist) */
  if (NILP (prev))
    selected_screen->buffer_alist = XCONS (selected_screen->buffer_alist)->cdr;
  else
    XCONS (prev)->cdr = XCONS (XCONS (prev)->cdr)->cdr;
  XCONS(link)->cdr = selected_screen->buffer_alist;
  selected_screen->buffer_alist = link;
#endif

}

DEFUN ("switch-to-buffer", Fswitch_to_buffer, Sswitch_to_buffer, 1, 2, "BSwitch to buffer: ",
  "Select buffer BUFFER in the current window.\n\
BUFFER may be a buffer or a buffer name.\n\
Optional second arg NORECORD non-nil means\n\
do not put this buffer at the front of the list of recently selected ones.\n\
\n\
WARNING: This is NOT the way to work on another buffer temporarily\n\
within a Lisp program!  Use `set-buffer' instead.  That avoids messing with\n\
the window-buffer correspondences.")
  (bufname, norecord)
     Lisp_Object bufname, norecord;
{
  register Lisp_Object buf;
  Lisp_Object tem;

  if (EQ (minibuf_window, selected_window))
    error ("Cannot switch buffers in minibuffer window");
  tem = Fwindow_dedicated_p (selected_window);
  if (!NILP (tem))
    error ("Cannot switch buffers in a dedicated window");

  if (NILP (bufname))
    buf = Fother_buffer (Fcurrent_buffer (), Qnil);
  else
    buf = Fget_buffer_create (bufname);
  Fset_buffer (buf);
  if (NILP (norecord))
    record_buffer (buf);

  Fset_window_buffer (EQ (selected_window, minibuf_window)
		      ? Fnext_window (minibuf_window, Qnil, Qnil, Qnil) :
		      selected_window,
		      buf);

  return Qnil;
}

DEFUN ("pop-to-buffer", Fpop_to_buffer, Spop_to_buffer, 1, 3, 0,
  "Select buffer BUFFER in some window, preferably a different one.\n\
If BUFFER is nil, then some other buffer is chosen.\n\
If `pop-up-windows' is non-nil, windows can be split to do this.\n\
If optional second arg WINDOW is non-nil, insist on finding another\n\
window even if BUFFER is already visible in the selected window.\n\
If optional third arg is non-nil, it is the screen to pop to this\n\
buffer on.")
  (bufname, not_this_window_p, on_screen)
     Lisp_Object bufname, not_this_window_p, on_screen;
{
  Lisp_Object buf, window, screen;
  if (NILP (bufname))
    buf = Fother_buffer (Fcurrent_buffer (), Qnil);
  else
    buf = Fget_buffer_create (bufname);
  Fset_buffer (buf);
  window = Fdisplay_buffer (buf, not_this_window_p, on_screen);
  screen = Fwindow_screen (window);
  /* if the display-buffer hook decided to show this buffer in another
     screen, then select that screen. */
  if (!EQ (screen, Fselected_screen ()))
    Fselect_screen (screen);
  record_buffer (buf);
  Fselect_window (window);
  return Qnil;
}

DEFUN ("current-buffer", Fcurrent_buffer, Scurrent_buffer, 0, 0, 0,
  "Return the current buffer as a Lisp object.")
  ()
{
  register Lisp_Object buf;
  XSET (buf, Lisp_Buffer, current_buffer);
  return buf;
}

extern int last_known_column_point;

/* Set the current buffer to b */

void
set_buffer_internal (b)
     register struct buffer *b;
{
  register struct buffer *old_buf;
  register Lisp_Object tail;

  if (current_buffer == b)
    return;

  windows_or_buffers_changed = 1;
  old_buf = current_buffer;
  current_buffer = b;
  last_known_column_point = -1;   /* invalidate indentation cache */

  /* Look down buffer's list of local Lisp variables
     to find and update any that forward into C variables. */

  for (tail = b->local_var_alist; !NILP (tail); tail = XCONS (tail)->cdr)
    {
      Lisp_Object sym = XCONS (XCONS (tail)->car)->car;
      Lisp_Object valcontents = XSYMBOL (sym)->value;

      if (XTYPE (valcontents) == Lisp_Buffer_Local_Value
	  || XTYPE (valcontents) == Lisp_Some_Buffer_Local_Value)
	{
	  enum Lisp_Type tem = XTYPE (XCONS (valcontents)->car);
        
	  if (tem == Lisp_Boolfwd
	      || tem == Lisp_Intfwd
	      || tem == Lisp_Objfwd)
	    {
	      /* Just reference the variable
		 to cause it to become set for this buffer.  */
	      Fsymbol_value (sym);
	    }
	}
    }


  /* Do the same with any others that were local to the previous buffer */

  if (old_buf)
    {
      for (tail = old_buf->local_var_alist; !NILP (tail); tail = XCONS (tail)->cdr)
	{
	  Lisp_Object sym = XCONS (XCONS (tail)->car)->car;
	  Lisp_Object valcontents = XSYMBOL (sym)->value;

	  if (XTYPE (valcontents) == Lisp_Buffer_Local_Value
	      || XTYPE (valcontents) == Lisp_Some_Buffer_Local_Value)
	    {
	      enum Lisp_Type tem = XTYPE (XCONS (valcontents)->car);
	      if (tem == Lisp_Boolfwd
		  || tem == Lisp_Intfwd
		  || tem == Lisp_Objfwd)
	      {
		/* Just reference the variable
		   to cause it to become set for this buffer.  */
		Fsymbol_value (sym);
	      }
	    }
	}
    }
}

DEFUN ("set-buffer", Fset_buffer, Sset_buffer, 1, 1, 0,
  "Make the buffer BUFFER current for editing operations.\n\
BUFFER may be a buffer or the name of an existing buffer.\n\
See also `save-excursion' when you want to make a buffer current temporarily.\n\
This function does not display the buffer, so its effect ends\n\
when the current command terminates.\n\
Use `switch-to-buffer' or `pop-to-buffer' to switch buffers permanently.")
  (bufname)
     register Lisp_Object bufname;
{
  register Lisp_Object buffer;
  buffer = Fget_buffer (bufname);
  if (NILP (buffer))
    nsberror (bufname);
  if (NILP (XBUFFER (buffer)->name))
    error ("Selecting deleted buffer");
  set_buffer_internal (XBUFFER (buffer));
  return buffer;
}

DEFUN ("barf-if-buffer-read-only", Fbarf_if_buffer_read_only,
				   Sbarf_if_buffer_read_only, 0, 0, 0,
  "Signal a `buffer-read-only' error if the current buffer is read-only.")
  ()
{
  while (!NILP (current_buffer->read_only))
    Fsignal (Qbuffer_read_only, (Fcons (Fcurrent_buffer (), Qnil)));
  return Qnil;
}

DEFUN ("bury-buffer", Fbury_buffer, Sbury_buffer, 0, 1, "",
  "Put BUFFER at the end of the list of all buffers.\n\
There it is the least likely candidate for `other-buffer' to return;\n\
thus, the least likely buffer for \\[switch-to-buffer] to select by default.")
  (buffer)
     register Lisp_Object buffer;
{
  register Lisp_Object aelt, link;

  if (NILP (buffer))
    {
      XSET (buffer, Lisp_Buffer, current_buffer);
      Fswitch_to_buffer (Fother_buffer (buffer, Qnil), Qnil);
    }
  else
    {
      Lisp_Object buf1 = Fget_buffer (buffer);
      if (NILP (buf1))
	nsberror (buffer);
      buffer = buf1;
    }	  

  aelt = rassq_no_quit (buffer, Vbuffer_alist);
  link = memq_no_quit (aelt, Vbuffer_alist);
  Vbuffer_alist = delq_no_quit (aelt, Vbuffer_alist);
  XCONS (link)->cdr = Qnil;
  Vbuffer_alist = nconc2 (Vbuffer_alist, link);
#ifdef MULTI_SCREEN
  aelt = rassq_no_quit (buffer, selected_screen->buffer_alist);
  link = memq_no_quit (aelt, selected_screen->buffer_alist);
  selected_screen->buffer_alist =
    delq_no_quit (aelt, selected_screen->buffer_alist);
  XCONS (link)->cdr = Qnil;
  selected_screen->buffer_alist = nconc2 (selected_screen->buffer_alist, link);
#endif
  return Qnil;
}

DEFUN ("erase-buffer", Ferase_buffer, Serase_buffer, 0, 0, 0,
  "Delete the entire contents of the current buffer.\n\
Any clipping restriction in effect (see `narrow-to-buffer') is removed,\n\
so the buffer is truly empty after this.")
  ()
{
  Fwiden ();
  del_range (BEG, Z);
  current_buffer->last_window_start = 1;
  /* Prevent warnings, or suspension of auto saving, that would happen
     if future size is less than past size.  Use of erase-buffer
     implies that the future text is not really related to the past text.  */
  XFASTINT (current_buffer->save_length) = 0;
  return Qnil;
}

void
validate_region (b, e)
     register Lisp_Object *b, *e;
{
  register int i;

  CHECK_FIXNUM_COERCE_MARKER (*b, 0);
  CHECK_FIXNUM_COERCE_MARKER (*e, 1);

  if (XINT (*b) > XINT (*e))
    {
      i = XFASTINT (*b);	/* This is legit even if *b is < 0 */
      *b = *e;
      XFASTINT (*e) = i;	/* because this is all we do with i.  */
    }

  if (!(BEGV <= XINT (*b) && XINT (*b) <= XINT (*e)
        && XINT (*e) <= ZV))
    args_out_of_range (*b, *e);
}


DEFUN ("kill-all-local-variables", Fkill_all_local_variables, Skill_all_local_variables,
  0, 0, 0,
  "Switch to Fundamental mode by killing current buffer's local variables.\n\
Most local variable bindings are eliminated so that the default values\n\
become effective once more.  Also, the syntax table is set from\n\
`standard-syntax-table', the local keymap is set to nil,\n\
and the abbrev table from `fundamental-mode-abbrev-table'.\n\
This function also forces redisplay of the mode line.\n\
\n\
Every function to select a new major mode starts by\n\
calling this function.\n\n\
As a special exception, local variables whose names have\n\
a non-nil `permanent-local' property are not eliminated by this function.")
  ()
{
  register Lisp_Object alist, sym, tem;
  Lisp_Object oalist;
  oalist = current_buffer->local_var_alist;

  /* Make sure no local variables remain set up with this buffer
     for their current values.  */

  for (alist = oalist; !NILP (alist); alist = XCONS (alist)->cdr)
    {
      sym = XCONS (XCONS (alist)->car)->car;

      /* Reference each variable in the alist in our current buffer, so
	 that the cache is fully computed; we need the cache to be up
	 to date in order to correctly hack the permanent-local property.
       */
      tem = Fsymbol_value (sym);
      XCONS (XCONS (alist)->car)->cdr = tem;

      tem = XCONS (XCONS (XSYMBOL (sym)->value)->cdr)->car;
      if (XBUFFER (tem) != current_buffer) abort ();
      /* Symbol is now set up for this buffer's old local value.
	 Set it up for the current buffer with the default value.
	 */
      tem = XCONS (XCONS (XSYMBOL (sym)->value)->cdr)->cdr;
      XCONS (tem)->car = tem;
      XCONS (XCONS (XSYMBOL (sym)->value)->cdr)->car = Fcurrent_buffer ();
      store_symval_forwarding (sym, XCONS (XSYMBOL (sym)->value)->car,
			       XCONS (tem)->cdr);
    }

  /* Actually eliminate all local bindings of this buffer.  */

  reset_buffer_local_variables (current_buffer);

  /* Redisplay mode lines; we are changing major mode.  */

  redraw_mode_line++;

  /* Any which are supposed to be permanent,
     make local again, with the same values they had.  */
     
  for (alist = oalist; !NILP (alist); alist = XCONS (alist)->cdr)
    {
      sym = XCONS (XCONS (alist)->car)->car;
      tem = Fget (sym, Qpermanent_local);
      if (! NILP (tem))
	{
	  Fmake_local_variable (sym);
	  Fset (sym, XCONS (XCONS (alist)->car)->cdr);
	}
    }

  /* Force mode-line redisplay.  Useful here because all major mode
     commands call this function.  */
  redraw_mode_line++;

  return Qnil;
}

#if 0
DEFUN ("region-fields", Fregion_fields, Sregion_fields, 2, 4, "", 
  "Return list of fields overlapping a given portion of a buffer.\n\
The portion is specified by arguments START, END and BUFFER.\n\
BUFFER defaults to the current buffer.\n\
Optional 4th arg ERROR-CHECK non nil means just report an error\n\
if any protected fields overlap this portion.")
  (start, end, buffer, error_check)
     Lisp_Object start, end, buffer, error_check;
{
  register int start_loc, end_loc;
  Lisp_Object fieldlist;
  Lisp_Object collector;

  if (NILP (buffer))
    fieldlist = current_buffer->fieldlist;
  else
    {
      CHECK_BUFFER (buffer, 1);
      fieldlist = XBUFFER (buffer)->fieldlist;
    }

  CHECK_FIXNUM_COERCE_MARKER (start, 2);
  start_loc = XINT (start);

  CHECK_FIXNUM_COERCE_MARKER (end, 2);
  end_loc = XINT (end);
  
  collector = Qnil;
  
  while (CONSP (fieldlist))
    {
      register Lisp_Object field;
      register int field_start, field_end;

      field = XCONS (fieldlist)->car;
      field_start = marker_position (FIELD_START_MARKER (field)) - 1;
      field_end = marker_position (FIELD_END_MARKER (field));

      if ((start_loc < field_start && end_loc > field_start)
	  || (start_loc >= field_start && start_loc < field_end))
	{
	  if (!NILP (error_check))
	    {
	      if (!NILP (FIELD_PROTECTED_FLAG (field)))
		{
		  struct gcpro gcpro1;
		  GCPRO1 (fieldlist);
		  Fsignal (Qprotected_field, list1 (field));
		  UNGCPRO;
		}
	    }
	  else
	    collector = Fcons (field, collector);
	}
      
      fieldlist = XCONS (fieldlist)->cdr;
    }

  return collector;
}
#endif /* 0 */

extern Lisp_Object Vprin1_to_string_buffer;	/* in print.c */

void
init_buffer_once ()
{
  register Lisp_Object tem;
  struct buffer *def = &buffer_defaults;

  XSET (Vbuffer_defaults, Lisp_Buffer, def);
  XSET (Vbuffer_local_symbols, Lisp_Buffer, &buffer_local_symbols);
  /* Make sure all markable slots in buffer_defaults
     are initialized reasonably, so mark_buffer won't choke.  */
  reset_buffer (def);
  reset_buffer (&buffer_local_symbols);

  /* Set up the default values of various buffer slots.  */
  /* Must do these before making the first buffer! */

  /* real setup is done in loaddefs.el */
  def->mode_line_format = build_string ("%-");
  def->abbrev_mode = Qnil;
  def->overwrite_mode = Qnil;
  def->case_fold_search = Qt;
  def->auto_fill_function = Qnil;
  def->selective_display = Qnil;
#ifndef old
  def->selective_display_ellipses = Qt;
#endif
  def->abbrev_table = Qnil;
  def->display_table = Qnil;
  def->undo_list = Qnil;

  XFASTINT (def->tab_width) = 8;
  def->truncate_lines = Qnil;
  def->ctl_arrow = Qt;

  XFASTINT (def->fill_column) = 70;
  XFASTINT (def->left_margin) = 0;

  /* Assign the local-flags to the slots that have default values.
     The local flag is a bit that is used in the buffer
     to say that it has its own local value for the slot.
     The local flag bits are in the local_var_flags slot of the buffer.  */

  /* The buffer_local_flags cast crock won't work if this isn't true */
  if (sizeof (int) > sizeof (Lisp_Object)) abort ();

  /* 0 means not a lisp var, -1 means always local, else mask */
  memset (&buffer_local_flags, 0, sizeof buffer_local_flags);
  XFASTINT (buffer_local_flags.filename) = -1;
  XFASTINT (buffer_local_flags.truename) = -1;
  XFASTINT (buffer_local_flags.directory) = -1;
  XFASTINT (buffer_local_flags.backed_up) = -1;
  XFASTINT (buffer_local_flags.save_length) = -1;
  XFASTINT (buffer_local_flags.auto_save_file_name) = -1;
  XFASTINT (buffer_local_flags.read_only) = -1;
  XFASTINT (buffer_local_flags.major_mode) = -1;
  XFASTINT (buffer_local_flags.mode_name) = -1;
  XFASTINT (buffer_local_flags.undo_list) = -1;

  XFASTINT (buffer_local_flags.mode_line_format) = 1;
  XFASTINT (buffer_local_flags.abbrev_mode) = 2;
  XFASTINT (buffer_local_flags.overwrite_mode) = 4;
  XFASTINT (buffer_local_flags.case_fold_search) = 8;
  XFASTINT (buffer_local_flags.auto_fill_function) = 0x10;
  XFASTINT (buffer_local_flags.selective_display) = 0x20;
#ifndef old
  XFASTINT (buffer_local_flags.selective_display_ellipses) = 0x40;
#endif
  XFASTINT (buffer_local_flags.tab_width) = 0x80;
  XFASTINT (buffer_local_flags.truncate_lines) = 0x100;
  XFASTINT (buffer_local_flags.ctl_arrow) = 0x200;
  XFASTINT (buffer_local_flags.fill_column) = 0x400;
  XFASTINT (buffer_local_flags.left_margin) = 0x800;
  XFASTINT (buffer_local_flags.abbrev_table) = 0x1000;
  XFASTINT (buffer_local_flags.display_table) = 0x2000;
#if 0
  XFASTINT (buffer_local_flags.fieldlist) = 0x4000;
#endif
  XFASTINT (buffer_local_flags.syntax_table) = 0x8000;

  Vbuffer_alist = Qnil;
  current_buffer = 0;
  all_buffers = 0;

  QSFundamental = build_string ("Fundamental");

  Qfundamental_mode = intern ("fundamental-mode");
  def->major_mode = Qfundamental_mode;

  Vprin1_to_string_buffer = Fget_buffer_create (build_string (" prin1"));
  /* Want no undo records for prin1_to_string_buffer */
  Fbuffer_disable_undo (Vprin1_to_string_buffer);
  /* magic invisible buffer */
  Vbuffer_alist = Qnil;

#ifdef MULTI_SCREEN
  if (Vscreen_list) /* this is 0 now, but let's be safe... */
    {
      Lisp_Object rest;
      for (rest = Vscreen_list; !NILP (rest); rest = XCONS (rest)->cdr)
	XSCREEN (XCONS (rest)->car)->buffer_alist = Qnil;
    }
#endif

  tem = Fset_buffer (Fget_buffer_create (build_string ("*scratch*")));
  /* Want no undo records for *scratch*
     until after Emacs is dumped */
  Fbuffer_disable_undo (tem);
}

extern char *getwd ();

void
init_buffer ()
{
  char buf[MAXPATHLEN+1];

  Fset_buffer (Fget_buffer_create (build_string ("*scratch*")));
  if (getwd (buf) == 0)
    fatal ("`getwd' failed: %s.\n", buf);

#ifndef VMS
  /* Maybe this should really use some standard subroutine
     whose definition is filename syntax dependent.  */
  if (buf[strlen (buf) - 1] != '/')
    strcat (buf, "/");
#endif /* not VMS */
  current_buffer->directory = build_string (buf);
}

/* Define a variable whose value is the Lisp Object stored in
   the current buffer. */
static void
defvar_per_buffer (const char *namestring, int offset)
{
  Lisp_Object sym = intern (namestring);

  if (*(int *)(offset + (char *)&buffer_local_flags) == 0)
    /* Did a DEFVAR_PER_BUFFER without initializing the corresponding
       slot of buffer_local_flags */
    abort ();

  *((Lisp_Object *)(offset + (char *)XBUFFER (Vbuffer_local_symbols))) = sym;

  XSET (XSYMBOL (sym)->value, Lisp_Buffer_Objfwd, (Lisp_Object *) offset);
}

/* DOC is ignored because it is snagged and recorded externally 
 *  by make-docfile */
#define DEFVAR_PER_BUFFER(lname, field_name, doc)  \
  defvar_per_buffer ((lname), ((char *)&(buffer_local_flags.field_name) \
                               - (char *)&(buffer_local_flags)))


/* initialize the buffer routines */
void
syms_of_buffer ()
{
  struct buffer *def = XBUFFER (Vbuffer_defaults);

  staticpro (&Vbuffer_defaults);
  staticpro (&Vbuffer_local_symbols);
  staticpro (&Qfundamental_mode);
  staticpro (&QSFundamental);
  staticpro (&Vbuffer_alist);

  defsymbol (&Qmode_class, "mode-class");
  defsymbol (&Qrename_auto_save_file, "rename-auto-save-file");
  defsymbol (&Qdelete_auto_save_files, "delete-auto-save-files");
  defsymbol (&Qkill_buffer_hook, "kill-buffer-hook");
  defsymbol (&Qpermanent_local, "permanent-local");
  defsymbol (&Qprotected_field, "protected-field");

  Fput (Qprotected_field, Qerror_conditions,
	list2 (Qprotected_field, Qerror));
  Fput (Qprotected_field, Qerror_message,
	build_string ("Attempt to modify a protected field"));

  /* All these use DEFVAR_LISP_NOPRO because the slots in
     buffer_defaults will all be marked via Vbuffer_defaults.  */

  DEFVAR_LISP_NOPRO ("default-mode-line-format",
	      &def->mode_line_format,
    "Default value of `mode-line-format' for buffers that don't override it.\n\
This is the same as (default-value 'mode-line-format).");

  DEFVAR_LISP_NOPRO ("default-abbrev-mode",
	      &def->abbrev_mode,
    "Default value of `abbrev-mode' for buffers that do not override it.\n\
This is the same as (default-value 'abbrev-mode).");

  DEFVAR_LISP_NOPRO ("default-ctl-arrow",
	      &def->ctl_arrow,
    "Default value of `ctl-arrow' for buffers that do not override it.\n\
This is the same as (default-value 'ctl-arrow).");

  DEFVAR_LISP_NOPRO ("default-truncate-lines",
	      &def->truncate_lines,
    "Default value of `truncate-lines' for buffers that do not override it.\n\
This is the same as (default-value 'truncate-lines).");

  DEFVAR_LISP_NOPRO ("default-fill-column",
	      &def->fill_column,
    "Default value of `fill-column' for buffers that do not override it.\n\
This is the same as (default-value 'fill-column).");

  DEFVAR_LISP_NOPRO ("default-left-margin",
	      &def->left_margin,
    "Default value of `left-margin' for buffers that do not override it.\n\
This is the same as (default-value 'left-margin).");

  DEFVAR_LISP_NOPRO ("default-tab-width",
	      &def->tab_width,
    "Default value of `tab-width' for buffers that do not override it.\n\
This is the same as (default-value 'tab-width).");

  DEFVAR_LISP_NOPRO ("default-case-fold-search",
	      &def->case_fold_search,
    "Default value of `case-fold-search' for buffers that don't override it.\n\
This is the same as (default-value 'case-fold-search).");

  DEFVAR_PER_BUFFER ("mode-line-format", mode_line_format, 0);

/* This doc string is too long for cpp; cpp dies if it isn't in a comment.
   But make-docfile finds it!
  DEFVAR_PER_BUFFER ("mode-line-format", mode_line_format,
    "Template for displaying mode line for current buffer.\n\
Each buffer has its own value of this variable.\n\
Value may be a string, a symbol or a list or cons cell.\n\
For a symbol, its value is used (but it is ignored if t or nil).\n\
 A string appearing directly as the value of a symbol is processed verbatim\n\
 in that the %-constructs below are not recognized.\n\
For a list whose car is a symbol, the symbol's value is taken,\n\
 and if that is non-nil, the cadr of the list is processed recursively.\n\
 Otherwise, the caddr of the list (if there is one) is processed.\n\
For a list whose car is a string or list, each element is processed\n\
 recursively and the results are effectively concatenated.\n\
For a list whose car is an integer, the cdr of the list is processed\n\
  and padded (if the number is positive) or truncated (if negative)\n\
  to the width specified by that number.\n\
A string is printed verbatim in the mode line except for %-constructs:\n\
  (%-constructs are allowed when the string is the entire mode-line-format\n\
   or when it is found in a cons-cell or a list)\n\
  %b -- print buffer name.      %f -- print visited file name.\n\
  %* -- print *, % or hyphen.   %m -- print value of mode-name (obsolete).\n\
  %s -- print process status.   %M -- print value of global-mode-string. (obs)\n\
  %S -- print name of selected screen (only meaningful under X Windows).\n\
  %p -- print percent of buffer above top of window, or top, bot or all.\n\
  %n -- print Narrow if appropriate.\n\
  %[ -- print one [ for each recursive editing level.  %] similar.\n\
  %% -- print %.   %- -- print infinitely many dashes.\n\
Decimal digits after the % specify field width to which to pad.");
*/

  DEFVAR_LISP_NOPRO ("default-major-mode", &def->major_mode,
    "*Major mode for new buffers.  Defaults to `fundamental-mode'.\n\
nil here means use current buffer's major mode.");

  DEFVAR_PER_BUFFER ("major-mode", major_mode,
    "Symbol for current buffer's major mode.");

  DEFVAR_PER_BUFFER ("mode-name", mode_name,
    "Pretty name of current buffer's major mode (a string).");

  DEFVAR_PER_BUFFER ("abbrev-mode", abbrev_mode,
    "Non-nil turns on automatic expansion of abbrevs as they are inserted.\n\
Automatically becomes buffer-local when set in any fashion.");

  DEFVAR_PER_BUFFER ("case-fold-search", case_fold_search,
    "*Non-nil if searches should ignore case.\n\
Automatically becomes buffer-local when set in any fashion.");

  DEFVAR_PER_BUFFER ("fill-column", fill_column,
    "*Column beyond which automatic line-wrapping should happen.\n\
Automatically becomes buffer-local when set in any fashion.");

  DEFVAR_PER_BUFFER ("left-margin", left_margin,
    "*Column for the default indent-line-function to indent to.\n\
Linefeed indents to this column in Fundamental mode.\n\
Automatically becomes buffer-local when set in any fashion.");

  DEFVAR_PER_BUFFER ("tab-width", tab_width,
    "*Distance between tab stops (for display of tab characters), in columns.\n\
Automatically becomes buffer-local when set in any fashion.");

  DEFVAR_PER_BUFFER ("ctl-arrow", ctl_arrow,
    "*Non-nil means display control chars with uparrow.\n\
Nil means use backslash and octal digits.\n\
An integer means characters >= ctl-arrow are assumed to be printable, and\n\
will be displayed as a single glyph.\n\
Any other value is the same as 160 - the code SPC with the high bit on.\n\
\n\
The interpretation of this variable is likely to change in the future.\n\
\n\
Automatically becomes buffer-local when set in any fashion.\n\
This variable does not apply to characters whose display is specified\n\
in the current display table (if there is one).");

  DEFVAR_PER_BUFFER ("truncate-lines", truncate_lines,
    "*Non-nil means do not display continuation lines;\n\
give each line of text one screen line.\n\
Automatically becomes buffer-local when set in any fashion.\n\
\n\
Note that this is overridden by the variable\n\
`truncate-partial-width-windows' if that variable is non-nil\n\
and this buffer is not full-screen width.");

  DEFVAR_PER_BUFFER ("default-directory", directory,
    "Name of default directory of current buffer.  Should end with slash.\n\
Each buffer has its own value of this variable.");

  DEFVAR_PER_BUFFER ("auto-fill-function", auto_fill_function,
    "Function called (if non-nil) to perform auto-fill.\n\
It is called after self-inserting a space at a column beyond `fill-column'.\n\
Each buffer has its own value of this variable.\n\
NOTE: This variable is not an ordinary hook;\n\
It may not be a list of functions.");

  DEFVAR_PER_BUFFER ("buffer-file-name", filename,
    "Name of file visited in current buffer, or nil if not visiting a file.\n\
Each buffer has its own value of this variable.");

  DEFVAR_PER_BUFFER ("buffer-file-truename", truename,
    "The real name of the file visited in the current buffer, \n\
or nil if not visiting a file.  This is the result of passing \n\
buffer-file-name to the real-path-name function.  Every buffer \n\
has its own value of this variable.  This variable is automatically \n\
maintained by the functions that change the file name associated \n\
with a buffer.");

  DEFVAR_PER_BUFFER ("buffer-auto-save-file-name", auto_save_file_name,
    "Name of file for auto-saving current buffer,\n\
or nil if buffer should not be auto-saved.\n\
Each buffer has its own value of this variable.");

  DEFVAR_PER_BUFFER ("buffer-read-only", read_only,
    "Non-nil if this buffer is read-only.\n\
Each buffer has its own value of this variable.");

  DEFVAR_PER_BUFFER ("buffer-backed-up", backed_up,
    "Non-nil if this buffer's file has been backed up.\n\
Backing up is done before the first time the file is saved.\n\
Each buffer has its own value of this variable.");

  DEFVAR_PER_BUFFER ("buffer-saved-size", save_length,
    "Length of current buffer when last read in, saved or auto-saved.\n\
0 initially.\n\
Each buffer has its own value of this variable.");

  DEFVAR_PER_BUFFER ("selective-display", selective_display,
    "Non-nil enables selective display:\n\
Integer N as value means display only lines\n\
 that start with less than n columns of space.\n\
A value of t means, after a ^M, all the rest of the line is invisible.\n\
 Then ^M's in the file are written into files as newlines.\n\n\
Automatically becomes buffer-local when set in any fashion.");

  DEFVAR_PER_BUFFER ("local-abbrev-table", abbrev_table,
    "Local (mode-specific) abbrev table of current buffer.");

#ifndef old
  DEFVAR_PER_BUFFER ("selective-display-ellipses", selective_display_ellipses,
    "t means display ... on previous line when a line is invisible.\n\
Automatically becomes buffer-local when set in any fashion.");
#endif

  DEFVAR_PER_BUFFER ("overwrite-mode", overwrite_mode,
    "Non-nil if self-insertion should replace existing text.\n\
Automatically becomes buffer-local when set in any fashion.");

#if 0
  DEFVAR_PER_BUFFER ("buffer-display-table", display_table,
    "Display table that controls display of the contents of current buffer.\n\
Automatically becomes buffer-local when set in any fashion.\n\
The display table is a vector created with `make-display-table'.\n\
The first 256 elements control how to display each possible text character.\n\
The value should be a \"rope\" (see `make-rope') or nil;\n\
nil means display the character in the default fashion.\n\
The remaining five elements are ropes that control the display of\n\
  the end of a truncated screen line (element 256);\n\
  the end of a continued line (element 257);\n\
  the escape character used to display character codes in octal (element 258);\n\
  the character used as an arrow for control characters (element 259);\n\
  the decoration indicating the presence of invisible lines (element 260).\n\
If this variable is nil, the value of `standard-display-table' is used.\n\
Each window can have its own, overriding display table.");
#endif

#if 0
  DEFVAR_PER_BUFFER ("buffer-field-list", fieldlist,
    "List of fields in the current buffer.  See `add-field'.");
#endif

  DEFVAR_BOOL ("find-file-compare-truenames", &find_file_compare_truenames,
    "If this is true, then the find-file command will check the truenames\n\
of all visited files when deciding whether a given file is already in\n\
a buffer, instead of just the buffer-file-name.  This means that if you\n\
attempt to visit another file which is a hard-link or symbolic-link to a\n\
file which is already in a buffer, the existing buffer will be found instead\n\
of a newly-created one.\n\
\n\
See also the variable find-file-use-truenames.");
  find_file_compare_truenames = 0;

  DEFVAR_BOOL ("find-file-use-truenames", &find_file_use_truenames,
    "If this is true, then a buffer's visited file-name will always be\n\
chased back to the real file; it will never be a symbolic link, and there\n\
will never be a symbolic link anywhere in its directory path.\n\
That is, the buffer-file-name and buffer-file-truename will be equal.\n\
\n\
See also the variable find-file-compare-truenames.");
  find_file_use_truenames = 0;

  DEFVAR_LISP ("before-change-function", &Vbefore_change_function,
	       "Function to call before each text change.\n\
Two arguments are passed to the function: the positions of\n\
the beginning and end of the range of old text to be changed.\n\
For an insertion, the beginning and end are at the same place.\n\
No information is given about the length of the text after the change.\n\
position of the change\n\
\n\
While executing the `before-change-function', changes to buffers do not\n\
cause calls to any `before-change-function' or `after-change-function'.");
  Vbefore_change_function = Qnil;

  DEFVAR_LISP ("after-change-function", &Vafter_change_function,
	       "Function to call after each text change.\n\
Three arguments are passed to the function: the positions of\n\
the beginning and end of the range of changed text,\n\
and the length of the pre-change text replaced by that range.\n\
For an insertion, the pre-change length is zero;\n\
for a deletion, that length is the number of characters deleted,\n\
and the post-change beginning and end are at the same place.\n\
\n\
While executing the `after-change-function', changes to buffers do not\n\
cause calls to any `before-change-function' or `after-change-function'.");
  Vafter_change_function = Qnil;

  DEFVAR_LISP ("first-change-function", &Vfirst_change_function,
  "Function to call before changing a buffer which is unmodified.\n\
The function is called, with no arguments, if it is non-nil.");
  Vfirst_change_function = Qnil;

  DEFVAR_PER_BUFFER ("buffer-undo-list", undo_list,
    "List of undo entries in current buffer.");

  defsubr (&Sbuffer_list);
  defsubr (&Sget_buffer);
  defsubr (&Sget_file_buffer);
  defsubr (&Sget_buffer_create);
  defsubr (&Sgenerate_new_buffer_name);
  defsubr (&Sbuffer_name);
  defsubr (&Sbuffer_file_name);
  defsubr (&Sbuffer_local_variables);
  defsubr (&Sbuffer_dedicated_screen);
  defsubr (&Sset_buffer_dedicated_screen);
  defsubr (&Sbuffer_modified_p);
  defsubr (&Sset_buffer_modified_p);
  defsubr (&Sbuffer_modified_tick);
  defsubr (&Srename_buffer);
  defsubr (&Sother_buffer);
  defsubr (&Sbuffer_disable_undo);
  defsubr (&Sbuffer_enable_undo);
  defsubr (&Skill_buffer);
  defsubr (&Serase_buffer);
  defsubr (&Sswitch_to_buffer);
  defsubr (&Spop_to_buffer);
  defsubr (&Scurrent_buffer);
  defsubr (&Sset_buffer);
  defsubr (&Sbarf_if_buffer_read_only);
  defsubr (&Sbury_buffer);
  defsubr (&Skill_all_local_variables);
#if 0
  defsubr (&Sregion_fields);
#endif
}
