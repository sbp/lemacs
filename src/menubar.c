/* Implements an elisp-programmable menubar.
   Copyright (C) 1993, 1994
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

/* created 16-dec-91 by jwz */

#include "config.h"
#include "intl.h"

#include <stdio.h>	/* for sprintf */

#include "lisp.h"

#include "xintrinsic.h"
#include <X11/StringDefs.h>
#ifdef LWLIB_USES_MOTIF
#include <Xm/Xm.h> /* for XmVersion */
#endif

#include "EmacsManager.h"
#include "EmacsScreen.h"
#include "EmacsShell.h"
#include "screen.h"
#include "xterm.h"
#include "events.h"
#include "window.h"
#include "buffer.h"

#include "commands.h"           /* zmacs_regions */

#include "dispmisc.h"

#include "lwlib.h"

#if !defined(LWLIB_USES_OLIT)
/* right now there is only Motif and Athena support for dialog boxes */
# define HAVE_DIALOG_BOXES
#endif

/* This variable is 1 if the menubar widget has to be updated. 
 It is set to 1 by set-menubar-dirty-flag and cleared when the widget
 has been indapted. */
int menubar_has_changed;

Lisp_Object Qcurrent_menubar;
Lisp_Object Qmenu_no_selection_hook;

static int set_screen_menubar (struct screen *s,
			       int deep_p,
			       int first_time_p);

/* we need a unique id for each popup menu and dialog box */
static unsigned int lwlib_id_tick;

/* count of menus/dboxes currently up */
int popup_menu_up_p;

int menubar_show_keybindings;

int popup_menu_titles;

LWLIB_ID
new_lwlib_id ()
{
  return ++lwlib_id_tick;
}


/* Converting Lisp menu tree descriptions to lwlib's `widget_value' form.

   menu_item_descriptor_to_widget_value() converts a lisp description of a
   menubar into a tree of widget_value structures.  It allocates widget_values
   with malloc_widget_value() and allocates other storage only for the `key'
   slot.  All other slots are filled with pointers to Lisp_String data.  We
   allocate a widget_value description of the menu or menubar, and hand it to
   lwlib, which then makes a copy of it, which it manages internally.  We then
   immediately free our widget_value tree; it will not be referenced again.

   This function is highly recursive (it follows the menu trees) and may call
   eval.  The reason we keep pointers to lisp string data instead of copying
   it and freeing it later is to avoid the speed penalty that would entail
   (since this needs to be fast, in the simple cases at least.)  (The reason
   we malloc/free the keys slot is because there's not a lisp string around
   for us to use in that case.)

   Since we keep pointers to lisp strings, and we call eval, we could lose if
   GC relocates (or frees) those strings.  It's not easy to gc protect the
   strings because of the recursive nature of this function, and the fact that
   it returns a data structure that gets freed later.  So...  we do the
   sleaziest thing possible and inhibit GC for the duration.  This is probably
   not a big deal...
 */

#if 1
  /* Eval the activep slot of the menu item */
# define wv_set_evalable_slot(slot,form)	\
   do { Lisp_Object _f_ = (form);		\
	  slot = (NILP (_f_) ? 0 : 		\
		  EQ (_f_, Qt) ? 1 :		\
		  !NILP (Feval (_f_)));		\
      } while (0)
#else
  /* Treat the activep slot of the menu item as a boolean */
# define wv_set_evalable_slot(slot,form)	\
      slot = (!NILP ((form)))
#endif


/* menu_item_descriptor_to_widget_value() mallocs a widget_value, but then
   may signal lisp errors.  If an error does not occur, the cons cell we have
   here has had its car and cdr set to 0 to tell us not to do anything.
   Otherwise we free the widget value.  (This has nothing to do with GC, it's
   just about not dropping pointers to malloc'd data when errors happen.)
 */
static Lisp_Object
widget_value_unwind (Lisp_Object closure)
{
  widget_value *wv = (widget_value *) cons_to_long (closure);
  if (wv)
    {
      BLOCK_INPUT;
      free_widget_value (wv);
      UNBLOCK_INPUT;
    }
  return Qnil;
}

/* This does the dirty work.  gc_currently_forbidden is 1 when this is called.
 */
static void menu_item_leaf_to_widget_value (Lisp_Object desc,
					    widget_value *wv,
					    int allow_text_field_p,
					    int no_keys_p);

static widget_value *
menu_item_descriptor_to_widget_value_1 (Lisp_Object desc, 
					int menubar_p, int deep_p,
					int depth)
{
  int menubar_root_p = (menubar_p && depth == 0);
  widget_value *wv;
  Lisp_Object wv_closure;
  int count = specpdl_depth ();
  int partition_seen = 0;

  BLOCK_INPUT;
  wv = malloc_widget_value ();
  UNBLOCK_INPUT;

  wv_closure = long_to_cons ((unsigned long) wv);
  record_unwind_protect (widget_value_unwind, wv_closure);

/*  IGNORE_DEFER_GETTEXT (desc);			/ * I18N3 */
  if (STRINGP (desc))
    {
      wv->name = GETTEXT ((char *) XSTRING (desc)->data);
      wv->value = 0;
      wv->enabled = 1;
    }
  else if (VECTORP (desc))
    {
      menu_item_leaf_to_widget_value (desc, wv, 0, (menubar_p && depth<=1));
    }
  else if (CONSP (desc))
    {
      widget_value *prev = 0;

/*      IGNORE_DEFER_GETTEXT (XCONS (desc)->car);		/ * I18N3 */
      if (STRINGP (XCONS (desc)->car))
	{
	  wv->name = GETTEXT ((char *) XSTRING (XCONS (desc)->car)->data);
	  desc = Fcdr (desc);

	  if (popup_menu_titles && depth == 0)
	    {
	      /* Simply prepend three more widget values to the contents of
		 the menu: a label, and two separators (to get a double line.)
	       */
	      widget_value *title_wv, *sep1_wv, *sep2_wv;
	      BLOCK_INPUT;
	      title_wv = malloc_widget_value ();
	      sep1_wv = malloc_widget_value ();
	      sep2_wv = malloc_widget_value ();
	      UNBLOCK_INPUT;
	      title_wv->name = wv->name;
	      title_wv->value = 0;
	      title_wv->enabled = 1;
	      title_wv->next = sep1_wv;
	      sep1_wv->name = "---------";
	      sep1_wv->value = 0;
	      sep1_wv->enabled = 1;
	      sep1_wv->next = sep2_wv;
	      sep2_wv->name = "---------";
	      sep2_wv->value = 0;
	      sep2_wv->enabled = 1;
	      sep2_wv->next = 0;

	      wv->contents = title_wv;
	      prev = sep2_wv;
	    }
	}
      else if (menubar_root_p)
	wv->name = "menubar";
      else
	{
	  signal_error (Qerror,
                        list2 (build_string (GETTEXT
			       ("menu name (first element) must be a string")),
                               desc));
	}

      wv->value = 0;
      wv->enabled = 1;
      if (deep_p || menubar_root_p)
	{
	  widget_value *next;
	  for (; !NILP (desc); desc = Fcdr (desc))
	    {
	      Lisp_Object child = Fcar (desc);
	      if (menubar_root_p && NILP (child))	/* the partition */
		{
		  if (partition_seen)
		    error (
		     "more than one partition (nil) in menubar description");
		  partition_seen = 1;
		  BLOCK_INPUT;
		  next = malloc_widget_value ();
		  UNBLOCK_INPUT;
		  next->name = "";
		  next->value = 0;
		  next->enabled = 0;
		}
	      else
		{
		  next = menu_item_descriptor_to_widget_value_1 (child,
								 menubar_p,
								 deep_p,
								 depth + 1);
		}
	      if (! next)
		continue;
	      else if (prev)
		prev->next = next;
	      else
		wv->contents = next;
	      prev = next;
	    }
	}
      if (deep_p && !wv->contents)
	wv = 0;
    }
  else if (NILP (desc))
    error ("nil may not appear in menu descriptions");
  else
    signal_simple_error ("unrecognised menu descriptor", desc);

  if (wv)
    {
      /* Completed normally.  Clear out the cons that widget_value_unwind()
	 will be called with to tell it not to free the wv (as we are
	 returning it.) */
      XCONS (wv_closure)->car = Qzero;
      XCONS (wv_closure)->cdr = Qzero;
    }

  unbind_to (count, Qnil);
  return wv;
}

Lisp_Object Q_active, Q_suffix, Q_keys, Q_style, Q_selected;
Lisp_Object Qtoggle, Qradio;

#define KEYWORDP(obj) (SYMBOLP ((obj)) && \
		       (XSYMBOL((obj))->name->data [0] == ':'))

static void
menu_item_leaf_to_widget_value (Lisp_Object desc, widget_value *wv,
				int allow_text_field_p, int no_keys_p)
{
  Lisp_Object name = Qnil;
  Lisp_Object callback = Qnil;
  Lisp_Object suffix = Qnil;
  Lisp_Object active_p = Qt;
  Lisp_Object selected_p = Qnil;
  Lisp_Object keys = Qnil;
  Lisp_Object style = Qnil;
  int length = vector_length (XVECTOR (desc));
  Lisp_Object *contents = XVECTOR (desc)->contents;
  int plist_p;
  int selected_spec = 0;

  if (length < 3)
    signal_simple_error ("button descriptors must be at least 3 long", desc);

  /* length 3:		[ "name" callback active-p ]
     length 4:		[ "name" callback active-p suffix ]
     		   or	[ "name" callback keyword  value  ]
     length 5+:		[ "name" callback [ keyword value ]+ ]
   */
  if (length == 3)
    plist_p = 0;
  else if (length == 4)
    plist_p = KEYWORDP (contents [2]);
  else
    plist_p = 1;

  if (!plist_p)
    /* the old way */
    {
      name = contents [0];
      callback = contents [1];
      active_p = contents [2];
      if (length == 4)
	suffix = contents [3];
    }
  else
    {
      /* the new way */
      int i;
      if (length & 1)
	signal_simple_error (
		"button descriptor has an odd number of keywords and values",
			     desc);

      name = contents [0];
      callback = contents [1];
      for (i = 2; i < length; i += 2)
	{
	  Lisp_Object key = contents [i];
	  Lisp_Object val = contents [i+1];
	  if      (EQ (key, Q_active))   active_p = val;
	  else if (EQ (key, Q_suffix))   suffix = val;
	  else if (EQ (key, Q_keys))     keys = val;
	  else if (EQ (key, Q_style))    style = val;
	  else if (EQ (key, Q_selected)) selected_p = val, selected_spec = 1;
	  else if (!KEYWORDP (key))
	    signal_simple_error_2 ("not a keyword", key, desc);
	  else
	    signal_simple_error_2 ("unknown menu item keyword", key, desc);
	}
    }

  CHECK_STRING (name, 0);
  wv->name = (char *) XSTRING (name)->data;

  if (NILP (suffix))
    wv->value = 0;
  else
    {
      CHECK_STRING (suffix, 0);
      wv->value = (char *) XSTRING (suffix)->data;
    }

  wv_set_evalable_slot (wv->enabled, active_p);		/* runs Feval */
  wv_set_evalable_slot (wv->selected, selected_p);	/* runs Feval */

  wv->call_data = LISP_TO_VOID (callback);

  if (no_keys_p || !menubar_show_keybindings)
    wv->key = 0;
  else if (!NILP (keys))	/* Use this string to generate key bindings */
    {
      CHECK_STRING (keys, 0);
      keys = Fsubstitute_command_keys (keys);
      if (string_length (XSTRING (keys)) > 0)
	wv->key = xstrdup ((char *) XSTRING (keys)->data);
      else
	wv->key = 0;
    }
  else if (SYMBOLP (callback))	/* Show the binding of this command. */
    {
      char buf [1024];
      where_is_to_char (callback, Fcurrent_local_map (), Qnil, buf);
      if (buf [0])
	wv->key = xstrdup (buf);
      else
	wv->key = 0;
    }

  CHECK_SYMBOL (style, 0);
  if (NILP (style))
    {
      /* If the callback is nil, treat this item like unselectable text.
	 This way, dashes will show up as a separator. */
      if (NILP (callback))
	wv->type = UNSPECIFIED_TYPE;
      else
	wv->type = BUTTON_TYPE;
    }
  else if (EQ (style, Qtoggle))
    wv->type = TOGGLE_TYPE;
  else if (EQ (style, Qradio))
    wv->type = RADIO_TYPE;
  else if (EQ (style, Qtext))
    {
      if (! allow_text_field_p)
	signal_simple_error ("text field not allowed in this context", desc);
      wv->type = TEXT_TYPE;
      wv->value = wv->name;
      wv->name = "value";
    }
  else
    signal_simple_error_2 ("unknown style", style, desc);

  if (selected_spec && wv->type != TOGGLE_TYPE && wv->type != RADIO_TYPE)
    signal_simple_error (
		    ":selected only makes sense with :style toggle or radio",
			 desc);
}


#if 0
static void
print_widget_value (widget_value *wv, int depth)
{
  char d [200];
  int i;
  for (i = 0; i < depth; i++) d[i] = ' ';
  d[depth]=0;
  printf ("%sname:    %s\n", d, (wv->name ? wv->name : "(null)"));
  if (wv->value) printf ("%svalue:   %s\n", d, wv->value);
  if (wv->key)   printf ("%skey:     %s\n", d, wv->key);
  printf ("%senabled: %d\n", d, wv->enabled);
  if (wv->contents)
    {
      printf ("\n%scontents: \n", d);
      print_widget_value (wv->contents, depth + 5);
    }
  if (wv->next)
    {
      printf ("\n");
      print_widget_value (wv->next, depth);
    }
}
#endif

extern Lisp_Object
restore_gc_inhibit (Lisp_Object val);

static widget_value *
menu_item_descriptor_to_widget_value (Lisp_Object desc, 
				      int menubar_p, int deep_p)
{
  widget_value *wv;
  int count = specpdl_depth ();
  record_unwind_protect (restore_gc_inhibit,
			 make_number (gc_currently_forbidden));
  gc_currently_forbidden = 1;
  wv = menu_item_descriptor_to_widget_value_1 (desc, menubar_p, deep_p, 0);
  unbind_to (count, Qnil);
  return wv;
}


/* This recursively calls free_widget_value() on the tree of widgets.
   It must free all data that was malloc'ed for these widget_values.
   Currently, emacs only allocates new storage for the `key' slot.
   All other slots are pointers into the data of Lisp_Strings, and
   must be left alone.
 */
static void
free_menubar_widget_value_tree (widget_value *wv)
{
  if (! wv) return;
  if (wv->key) xfree (wv->key);

  wv->name = wv->value = wv->key = (char *) 0xDEADBEEF;

  if (wv->contents && (wv->contents != (widget_value*)1))
    {
      free_menubar_widget_value_tree (wv->contents);
      wv->contents = (widget_value *) 0xDEADBEEF;
    }
  if (wv->next)
    {
      free_menubar_widget_value_tree (wv->next);
      wv->next = (widget_value *) 0xDEADBEEF;
    }
  BLOCK_INPUT;
  free_widget_value (wv);
  UNBLOCK_INPUT;
}


/* There is exactly one of these per screen. 
   It doesn't really need to be an lrecord (it's not lisp-accessible)
   but it makes marking slightly more modular.
 */

struct menubar_data
{
  struct lcrecord_header header;

  LWLIB_ID id;

  /* This is the last buffer for which the menubar was displayed.
     If the buffer has changed, we may have to update things.
   */
  Lisp_Object last_menubar_buffer;

  /* This is a vector of all of the callbacks of the menubar menu buttons.
     This is used only to protect them from being GCed, since the only other
     pointer to these lisp objects might be down in the private lwlib.c
     structures, which GC doesn't know about.
   */
  Lisp_Object menubar_callbacks;

  /* This flag tells us if the menubar contents are up-to-date with respect
     to the current menubar structure.  If we want to actually pull down a
     menu and this is false, then we need to update things.
   */
  char menubar_contents_up_to_date;
};

static Lisp_Object
mark_menubar_data (Lisp_Object obj, void (*markobj) (Lisp_Object))
{
  struct menubar_data *data = XRECORD (obj);
  ((markobj) (data->last_menubar_buffer));
  return (data->menubar_callbacks);
}
static void
print_menubar_data (Lisp_Object obj, Lisp_Object stream, int escapeflag)
{
  char buf[200];
  sprintf (buf, "#<menubar-data 0x%x>", (long) XRECORD (obj));
  write_string_1 (buf, -1, stream);
}

DEFINE_LRECORD_IMPLEMENTATION ("menubar-data", lrecord_menubar_data,
                               mark_menubar_data, print_menubar_data, 0, 0,
			       sizeof (struct menubar_data));


#define SCREEN_MENUBAR_DATA(screen) \
  ((struct menubar_data *) XRECORD ((screen)->menubar_data))


/* This is like SCREEN_MENUBAR_DATA(s)->menubar_callbacks, but contains an
   alist of (id . vector) for the callbacks of the popup menus and dialog
   boxes.  The menubar_callbacks are really just a degenerate case of this,
   but it is easier if those are screen-local, and popups are global.
 */
static Lisp_Object Vpopup_callbacks;


static int
gcpro_menu_callbacks_1 (Lisp_Object menu, Lisp_Object *vector, int index)
{
  if (NILP (menu))
    return index;
/*  IGNORE_DEFER_GETTEXT (menu);		/ * I18N3 */
  if (VECTORP (menu))
    {
      if (vector_length (XVECTOR (menu)) > 2)
	{
          struct Lisp_Vector *v = XVECTOR (*vector);
	  if (vector_length (v) <= index)
	    {
	      /* reallocate the vector by doubling its size */
	      Lisp_Object new_vector = make_vector (index * 2, Qnil);
	      memcpy (XVECTOR (new_vector)->contents,
		      v->contents,
		      vector_length (v) * sizeof (Lisp_Object));
	      *vector = new_vector;
              v = XVECTOR (new_vector);
	    }
	  v->contents [index] = XVECTOR (menu)->contents [1];
	  index++;
	}
    }
  else if (STRINGP (menu))
    ;
  else if (CONSP (menu))
      {
	Lisp_Object current;
	for (current = menu; !NILP (current); current = Fcdr (current))
	  index = gcpro_menu_callbacks_1 (Fcar (current), vector, index);
      }
#if 0
  else
      /* syntax checking has already been done */
    /* not if we're in non-deep mode it hasn't; the error won't be
       signalled until the menubar is activated. */
    abort ();
#endif

  return index;
}

static void
gcpro_menu_callbacks (Lisp_Object menu, Lisp_Object *vector)
{
  int i, end;
  if (NILP (*vector))
    *vector = make_vector (10, Qnil);
  else if (!VECTORP (*vector))
    abort ();

  end = gcpro_menu_callbacks_1 (menu, vector, 0);

  /* pad it with nil, so that we don't continue protecting things
     we don't need any more */
  for (i = end; i < vector_length (XVECTOR (*vector)); i++)
    XVECTOR (*vector)->contents [i] = Qnil;
}

static void
gcpro_popup_callbacks (Lisp_Object id, Lisp_Object menu)
{
  Lisp_Object data;
  if (!NILP (assq_no_quit (id, Vpopup_callbacks)))
    abort ();
  data = Fcons (id, Qnil);
  gcpro_menu_callbacks (menu, &XCONS(data)->cdr);
  Vpopup_callbacks = Fcons (data, Vpopup_callbacks);
}

static void
ungcpro_popup_callbacks (Lisp_Object id)
{
  Lisp_Object this = assq_no_quit (id, Vpopup_callbacks);
  if (NILP (this))
    abort ();
  Vpopup_callbacks = delq_no_quit (this, Vpopup_callbacks);
}

/* The order in which callbacks are run is funny to say the least.
   It's sometimes tricky to avoid running a callback twice, and to
   avoid returning prematurely.  So, this function returns true
   if the menu's callbacks are no longer gc protected.  So long
   as we unprotect them before allowing other callbacks to run,
   everything should be ok.
 */
static int
popup_handled_p (Lisp_Object id)
{
  return (NILP (assq_no_quit (id, Vpopup_callbacks)));
}


Lisp_Object Qactivate_menubar_hook, Vactivate_menubar_hook;

static void
pre_activate_callback (widget, id, client_data)
     Widget widget;
     LWLIB_ID id;
     XtPointer client_data;
{
  struct gcpro gcpro1;
  struct screen* s = x_any_window_to_screen (XtWindow (widget));
  Lisp_Object rest = Qnil;
  int any_changes = 0;

  if (!s)
    s = x_any_window_to_screen (XtWindow (XtParent (widget)));
  if (!s)
    return;

  if (!RECORD_TYPEP (s->menubar_data, lrecord_menubar_data))
    return;

  /* make the activate-menubar-hook be a list of functions, not a single
     function, just to simplify things. */
  if (!NILP (Vactivate_menubar_hook) &&
      (!CONSP (Vactivate_menubar_hook) ||
       EQ (XCONS (Vactivate_menubar_hook)->car, Qlambda)))
    Vactivate_menubar_hook = Fcons (Vactivate_menubar_hook, Qnil);

  GCPRO1 (rest);
  for (rest = Vactivate_menubar_hook; !NILP (rest); rest = Fcdr (rest))
    if (!EQ (call0 (XCONS (rest)->car), Qt))
      any_changes = 1;
  if (any_changes ||
      !SCREEN_MENUBAR_DATA (s)->menubar_contents_up_to_date)
    set_screen_menubar (s, 1, 0);
  UNGCPRO;
}


extern Time mouse_timestamp;
extern Time global_mouse_timestamp;

static void
menubar_selection_callback (Widget ignored_widget,
			    LWLIB_ID ignored_id,
			    XtPointer client_data)
{
  Lisp_Object event, fn, arg;
  Lisp_Object data;

  if (((LISP_WORD_TYPE) client_data) == 0)
    return;

  VOID_TO_LISP (data, client_data);

  /* Flush the X and process input */
  Faccept_process_output (Qnil, Qnil, Qnil);

  if (((LISP_WORD_TYPE) client_data) == -1)
    {
      fn = Vrun_hooks;
      arg = Qmenu_no_selection_hook;
      if (NILP (fn))
        fn = Qsymbolp;          /* something innocuous */
    }
  else if (SYMBOLP (data))
    {
      fn = Qcall_interactively;
      arg = data;
    }
  else if (CONSP (data))
    {
      fn = Qeval;
      arg = data;
    }
  else
    {
      fn = Qeval;
      arg = list3 (Qsignal,
                   list2 (Qquote, Qerror),
                   list2 (Qquote, list2 (build_string
					 (GETTEXT ("illegal menu callback")),
                                         data)));
    }

  event = Fallocate_event ();
  XEVENT (event)->event_type = menu_event;
  XEVENT (event)->event.eval.function = fn;
  XEVENT (event)->event.eval.object = arg;

  /* This is the timestamp used for asserting focus so we need to get an
     up-to-date value event if no events has been dispatched to emacs
   */
  mouse_timestamp = global_mouse_timestamp;

  enqueue_command_event (event);
}

DEFUN ("set-menubar-dirty-flag", Fset_menubar_dirty_flag,
       Sset_menubar_dirty_flag, 0, 0, 0,
       "Tells emacs that the menubar widget has to be updated")
     ()
{
  menubar_has_changed = 1;
  return Qnil;
}

#ifdef ENERGIZE
extern int *get_psheets_for_buffer (Lisp_Object, int *);

static void
set_panel_button_sensitivity (struct screen* s, widget_value* data)
{
  struct x_display *x = s->display.x;
  struct window *window = XWINDOW (s->selected_window);
  int current_buffer_psheets_count = 0;
  int *current_buffer_psheets =
    get_psheets_for_buffer (window->buffer, &current_buffer_psheets_count);
  int panel_enabled = x->desired_psheets || current_buffer_psheets_count;
  widget_value* val;
  for (val = data->contents; val; val = val->next)
    if (!strcmp (val->name, "sheet"))
      {
	val->enabled = panel_enabled;
	return;
      }
}
#endif /* ENERGIZE */

static widget_value*
compute_menubar_data (struct screen* s, Lisp_Object menubar, int deep_p)
{
  widget_value* data;

  if (NILP (menubar))
    data = 0;
  else
    {
      data = menu_item_descriptor_to_widget_value (menubar, 1, deep_p);
#ifdef ENERGIZE
      if (data)
	set_panel_button_sensitivity (s, data);
#endif
    }
  return data;
}

static Lisp_Object Vblank_menubar;

static int
set_screen_menubar (struct screen *s, int deep_p, int first_time_p)
{
  widget_value *data;
  Lisp_Object menubar;
  int menubar_visible;
  struct x_display *x = s->display.x;
  int id = (int) s;

  if (! SCREEN_IS_X (s))
    return 0;

  /***** first compute the contents of the menubar *****/

  if (! first_time_p)
    {
      /* evaluate `current-menubar' in the buffer of the selected window
	 of the screen in question.
       */
      Lisp_Object obuf = Fcurrent_buffer ();
      Fset_buffer (XWINDOW (s->selected_window)->buffer);
      menubar = Fsymbol_value (Qcurrent_menubar);
      Fset_buffer (obuf);
    }
  else
    {
      /* That's a little tricky the first time since the screen isn't
	 fully initalized yet. */
      menubar = Fsymbol_value (Qcurrent_menubar);
    }

  if (NILP (menubar))
    {
      menubar = Vblank_menubar;
      menubar_visible = 0;
    }
  else
    menubar_visible = 1;

  data = compute_menubar_data (s, menubar, deep_p);
  if (!data || (!data->next && !data->contents))
    abort ();
  
  if (NILP (s->menubar_data))
    {
      struct menubar_data *data = alloc_lcrecord (sizeof (struct menubar_data),
						  lrecord_menubar_data);

      data->id = new_lwlib_id ();
      data->last_menubar_buffer = Qnil;
      data->menubar_callbacks = Qnil;
      data->menubar_contents_up_to_date = 0;
      XSET (s->menubar_data, Lisp_Record, data);
    }

  /***** now store into the menubar widget, creating it if necessary *****/

  id = SCREEN_MENUBAR_DATA (s)->id;
  if (!x->menubar_widget)
    {
      Widget parent = x->container;

      if (!first_time_p)
	abort ();

      /* It's the first time we've mapped the menubar so compute its
	 contents completely once.  This makes sure that the menubar
	 components are created with the right type. */
      if (!deep_p)
	{
	  free_menubar_widget_value_tree (data);
	  data = compute_menubar_data (s, menubar, 1);
	}

      BLOCK_INPUT;

      x->menubar_widget =
	lw_create_widget ("menubar", "menubar", id, data, parent,
			  0, pre_activate_callback,
			  menubar_selection_callback, 0);

      UNBLOCK_INPUT;
    }
  else
    {
      BLOCK_INPUT;
      lw_modify_all_widgets (id, data, deep_p ? True : False);
      UNBLOCK_INPUT;
    }

  SCREEN_MENUBAR_DATA (s)->menubar_contents_up_to_date = deep_p;
  SCREEN_MENUBAR_DATA (s)->last_menubar_buffer =
    XWINDOW (s->selected_window)->buffer;

  gcpro_menu_callbacks (menubar, &SCREEN_MENUBAR_DATA (s)->menubar_callbacks);

  return menubar_visible;
}


/* Called from x_create_widgets() to create the inital menubar of a screen
   before it is mapped, so that the window is mapped with the menubar already
   there instead of us tacking it on later and thrashing the window after it
   is visible. */
int
initialize_screen_menubar (struct screen *s)
{
  return set_screen_menubar (s, 1, 1);
}


static LWLIB_ID last_popup_selection_callback_id;

static void
popup_selection_callback (widget, id, client_data)
     Widget widget;
     LWLIB_ID id;
     XtPointer client_data;
{
  last_popup_selection_callback_id = id;
  menubar_selection_callback (widget, id, client_data);
  /* lw_destroy_all_widgets() will be called from popup_down_callback() */
}

static void
popup_down_callback (widget, id, client_data)
     Widget widget;
     LWLIB_ID id;
     XtPointer client_data;
{
  Lisp_Object lid = make_number (id);
  if (popup_handled_p (lid))
    return;
  if (popup_menu_up_p == 0)
    abort ();
  ungcpro_popup_callbacks (lid);
  popup_menu_up_p--;
  /* if this isn't called immediately after the selection callback, then
     there wasn't a menu selection. */
  if (id != last_popup_selection_callback_id)
    menubar_selection_callback (widget, id, (XtPointer) -1);
  BLOCK_INPUT;
  lw_destroy_all_widgets (id);
  UNBLOCK_INPUT;
}

#ifdef HAVE_DIALOG_BOXES

static void maybe_run_dbox_text_callback (LWLIB_ID);

static void
dbox_selection_callback (widget, id, client_data)
     Widget widget;
     LWLIB_ID id;
     XtPointer client_data;
{
  /* This is called with client_data == -1 when WM_DELETE_WINDOW is sent
     instead of a button being selected. */
  Lisp_Object lid = make_number (id);
  if (popup_handled_p (lid))
    return;
  if (popup_menu_up_p == 0)
    abort ();
  ungcpro_popup_callbacks (lid);
  popup_menu_up_p--;
  maybe_run_dbox_text_callback (id);
  menubar_selection_callback (widget, id, client_data);
  BLOCK_INPUT;
  lw_destroy_all_widgets (id);
  UNBLOCK_INPUT;
}

static void
maybe_run_dbox_text_callback (LWLIB_ID id)
{
  widget_value *wv;
  int got_some;
  BLOCK_INPUT;
  wv = malloc_widget_value ();
  wv->name = "value";
  got_some = lw_get_some_values (id, wv);
  UNBLOCK_INPUT;
  if (got_some)
    {
      Lisp_Object text_field_callback;
      char *text_field_value = wv->value;
      VOID_TO_LISP (text_field_callback, wv->call_data);
      if (text_field_value)
	{
	  void *tmp = LISP_TO_VOID (list2 (text_field_callback,
                                           build_string (text_field_value)));
	  menubar_selection_callback (0, id, (XtPointer) tmp);
	  xfree (text_field_value);
	}
    }
  BLOCK_INPUT;
  free_widget_value (wv);
  UNBLOCK_INPUT;
}

#endif /* HAVE_DIALOG_BOXES */


/* This comment supplies the doc string for define-key,
   for make-docfile to see.  We cannot put this in the real DEFUN
   due to limits in the Unix cpp.

DEFUN ("popup-menu", Ffoo, Sfoo, 1, 1, 0,
       "Pop up the given menu.\n\
A menu description is a list of menu items, strings, and submenus.\n\
\n\
The first element of a menu must be a string, which is the name of the menu.\n\
This is the string that will be displayed in the parent menu, if any.  For\n\
toplevel menus, it is ignored.  This string is not displayed in the menu\n\
itself.\n\
\n\
If an element of a menu is a string, then that string will be presented in\n\
the menu as unselectable text.\n\
\n\
If an element of a menu is a string consisting solely of hyphens, then that\n\
item will be presented as a solid horizontal line.\n\
\n\
If an element of a menu is a list, it is treated as a submenu.  The name of\n\
that submenu (the first element in the list) will be used as the name of the\n\
item representing this menu on the parent.\n\
\n\
Otherwise, the element must be a vector, which describes a menu item.\n\
A menu item can have any of the following forms:\n\
\n\
 [ \"name\" callback <active-p> ]\n\
 [ \"name\" callback <active-p> \"suffix\" ]\n\
 [ \"name\" callback :<keyword> <value>  :<keyword> <value> ... ]\n\
\n\
The name is the string to display on the menu; it is filtered through the\n\
resource database, so it is possible for resources to override what string\n\
is actually displayed.\n\
\n\
If the `callback' of a menu item is a symbol, then it must name a command.\n\
It will be invoked with `call-interactively'.  If it is a list, then it is\n\
evaluated with `eval'.\n\
\n\
The possible keywords are this:\n\
\n\
 :active   <form>    Same as <active-p> in the first two forms: the\n\
                     expression is evaluated just before the menu is\n\
                     displayed, and the menu will be selectable only if\n\
                     the result is non-nil.\n\
\n\
 :suffix   \"string\"  Same as \"suffix\" in the second form: the suffix is\n\
                     appended to the displayed name, providing a convenient\n\
                     way of adding the name of a command's ``argument'' to\n\
                     the menu, like ``Kill Buffer NAME''.\n\
\n\
 :keys     \"string\"  Normally, the keyboard equivalents of commands in\n\
                     menus are displayed when the `callback' is a symbol.\n\
                     This can be used to specify keys for more complex menu\n\
                     items.  It is passed through `substitute-command-keys'\n\
                     first.\n\
\n\
 :style    <style>   Specifies what kind of object this menu item is:\n\
\n\
                        nil     A normal menu item.\n\
                        toggle  A toggle button.\n\
                        radio   A radio button.\n\
\n\
                     The only difference between toggle and radio buttons is\n\
                     how they are displayed.  But for consistency, a toggle\n\
                     button should be used when there is one option whose\n\
                     value can be turned on or off, and radio buttons should\n\
                     be used when there is a set of mutally exclusive\n\
                     options.  When using a group of radio buttons, you\n\
                     should arrange for no more than one to be marked as\n\
                     selected at a time.\n\
\n\
 :selected <form>    Meaningful only when STYLE is `toggle' or `radio'.\n\
                     This specifies whether the button will be in the\n\
                     selected or unselected state.\n\
\n\
For example:\n\
\n\
 [ \"Save As...\"    write-file  t ]\n\
 [ \"Revert Buffer\" revert-buffer (buffer-modified-p) ]\n\
 [ \"Read Only\"     toggle-read-only :style toggle :selected buffer-read-only ]\n\
\n\
See menubar.el for many more examples.")
	(menu_description)
 */

DEFUN ("popup-menu", Fpopup_menu, Spopup_menu, 1, 1, 0, 0
       /* See very large comment above */)
     (menu_desc)
     Lisp_Object menu_desc;
{
  int menu_id;
  struct screen *s = selected_screen;
  widget_value *data;
  Widget parent, menu;

  if (!SCREEN_IS_X (s)) error (GETTEXT ("not an X screen"));
  if (SYMBOLP (menu_desc))
    menu_desc = Fsymbol_value (menu_desc);
  CHECK_CONS (menu_desc, 0);
/*  IGNORE_DEFER_GETTEXT (XCONS (menu_desc)->car);	/ * I18N3 */
  CHECK_STRING (XCONS (menu_desc)->car, 0);
  data = menu_item_descriptor_to_widget_value (menu_desc, 0, 1);

  if (! data) error (GETTEXT ("no menu"));
  
  parent = s->display.x->widget;

  BLOCK_INPUT;
  menu_id = new_lwlib_id ();
  menu = lw_create_widget ("popup", data->name, menu_id, data, parent, 1, 0,
			   popup_selection_callback, popup_down_callback);
  free_menubar_widget_value_tree (data);

  gcpro_popup_callbacks (make_number (menu_id), menu_desc);

  /* Setting zmacs-region-stays is necessary here because executing a command
     from a menu is really a two-command process: the first command (bound to
     the button-click) simply pops up the menu, and returns.  This causes a
     sequence of magic-events (destined for the popup-menu widget) to begin.
     Eventually, a menu item is selected, and a menu-event blip is pushed onto
     the end of the input stream, which is then executed by the event loop.
     
     So there are two command-events, with a bunch of magic-events between
     them.  We don't want the *first* command event to alter the state of the
     region, so that the region can be available as an argument for the second
     command.
   */
  if (zmacs_regions)
    zmacs_region_stays = 1;

  popup_menu_up_p++;
  lw_popup_menu (menu);
  /* this speeds up display of pop-up menus */
  XFlush (XtDisplay (parent));
  UNBLOCK_INPUT;
  return Qnil;
}

DEFUN ("popup-menu-up-p", Fpopup_menu_up_p, Spopup_menu_up_p, 0, 0, 0,
       "Returns t if a popup menu is up, nil otherwise.\n\
See `popup-menu'.")
     ()
{
  return popup_menu_up_p ? Qt : Qnil;
}

#ifdef HAVE_DIALOG_BOXES

static CONST char * CONST button_names [] = {
  "button1", "button2", "button3", "button4", "button5",
  "button6", "button7", "button8", "button9", "button10" };

/* can't have static frame locals because of some broken compilers */
static char tmp_dbox_name [255];

static widget_value *
dbox_descriptor_to_widget_value (Lisp_Object desc)
{
  char *name;
  int lbuttons = 0, rbuttons = 0;
  int partition_seen = 0;
  int text_field_p = 0;
  int allow_text_p = 1;
  widget_value *prev = 0, *kids = 0;
  int n = 0;

  CHECK_CONS (desc, 0);
/*  IGNORE_DEFER_GETTEXT (XCONS (desc)->car);	/ * I18N3 */
  CHECK_STRING (XCONS (desc)->car, 0);
  name = GETTEXT ((char *) XSTRING (XCONS (desc)->car)->data);
  desc = XCONS (desc)->cdr;
  if (!CONSP (desc))
    error (GETTEXT ("dialog boxes must have some buttons"));

  kids = prev = malloc_widget_value ();
  prev->name = "message";
  prev->value = name;
  prev->enabled = 1;

  for (; !NILP (desc); desc = Fcdr (desc))
    {
      Lisp_Object button = XCONS (desc)->car;
      widget_value *wv;

      if (NILP (button))
	{
	  if (partition_seen)
	    error (GETTEXT("more than one partition (nil) seen in dbox spec"));
	  partition_seen = 1;
	  continue;
	}
      CHECK_VECTOR (button, 0);
      BLOCK_INPUT;
      wv = malloc_widget_value ();
      UNBLOCK_INPUT;

      menu_item_leaf_to_widget_value (button, wv, allow_text_p, 1);

      if (wv->type == TEXT_TYPE)
	{
	  text_field_p = 1;
	  allow_text_p = 0;	 /* only allow one */
	}
      else			/* it's a button */
	{
	  allow_text_p = 0;	 /* only allow text field at the front */
	  wv->value = wv->name;	/* what a mess... */
	  wv->name = (char *) button_names [n];

	  if (partition_seen)
	    rbuttons++;
	  else
	    lbuttons++;
	  n++;

	  if (lbuttons > 9 || rbuttons > 9)
	    error (GETTEXT ("too many buttons (9)")); /* #### this leaks */
	}

      prev->next = wv;
      prev = wv;
    }

  if (n == 0)
    error (GETTEXT ("dialog boxes must have some buttons"));
  {
    char type = (text_field_p ? 'P' : 'Q');
    widget_value *dbox;
    sprintf (tmp_dbox_name, "%c%dBR%d", type, lbuttons + rbuttons, rbuttons);
    BLOCK_INPUT;
    dbox = malloc_widget_value ();
    UNBLOCK_INPUT;
    dbox->name = tmp_dbox_name;
    dbox->contents = kids;

    return dbox;
  }
}


DEFUN ("popup-dialog-box", Fpopup_dialog_box, Spopup_dialog_box, 1, 1, 0,
       "Pop up a dialog box.\n\
A dialog box description is a list.\n\
\n\
The first element of a dialog box must be a string, which is the title or\n\
question.\n\
\n\
The rest of the elements are descriptions of the dialog box's buttons.\n\
Each of these is a vector, the syntax of which is essentially the same as\n\
that of popup menu items.  They may have any of the following forms:\n\
\n\
 [ \"name\" callback <active-p> ]\n\
 [ \"name\" callback <active-p> \"suffix\" ]\n\
 [ \"name\" callback :<keyword> <value>  :<keyword> <value> ... ]\n\
\n\
The name is the string to display on the button; it is filtered through the\n\
resource database, so it is possible for resources to override what string\n\
is actually displayed.\n\
\n\
If the `callback' of a button is a symbol, then it must name a command.\n\
It will be invoked with `call-interactively'.  If it is a list, then it is\n\
evaluated with `eval'.\n\
\n\
One (and only one) of the buttons may be `nil'.  This marker means that all\n\
following buttons should be flushright instead of flushleft.\n\
\n\
Though the keyword/value syntax is supported for dialog boxes just as in \n\
popup menus, the only keyword which is both meaningful and fully implemented\n\
for dialog box buttons is `:active'.")
     (dbox_desc)
     Lisp_Object dbox_desc;
{
  int dbox_id;
  struct screen *s = selected_screen;
  widget_value *data;
  Widget parent, dbox;

  if (!SCREEN_IS_X (s)) error (GETTEXT ("not an X screen"));
  if (SYMBOLP (dbox_desc))
    dbox_desc = Fsymbol_value (dbox_desc);
  CHECK_CONS (dbox_desc, 0);
/*  IGNORE_DEFER_GETTEXT (XCONS (dbox_desc)->car);	/ * I18N3 */
  CHECK_STRING (XCONS (dbox_desc)->car, 0);
  data = dbox_descriptor_to_widget_value (dbox_desc);

  if (! data) abort ();
  
  parent = s->display.x->widget;

  BLOCK_INPUT;
  dbox_id = new_lwlib_id ();
  dbox = lw_create_widget (data->name, "dialog", dbox_id, data, parent, 1, 0,
			   dbox_selection_callback, 0);
  lw_modify_all_widgets (dbox_id, data, True);
  lw_modify_all_widgets (dbox_id, data->contents, True);
  free_menubar_widget_value_tree (data);

  gcpro_popup_callbacks (make_number (dbox_id), dbox_desc);

  /* Setting zmacs-region-stays is necessary here because executing a command
     from a menu is really a two-command process: the first command (bound to
     the button-click) simply pops up the menu, and returns.  This causes a
     sequence of magic-events (destined for the popup-menu widget) to begin.
     Eventually, a menu item is selected, and a menu-event blip is pushed onto
     the end of the input stream, which is then executed by the event loop.
     
     So there are two command-events, with a bunch of magic-events between
     them.  We don't want the *first* command event to alter the state of the
     region, so that the region can be available as an argument for the second
     command.
   */
  if (zmacs_regions)
    zmacs_region_stays = 1;

  popup_menu_up_p++;
  lw_pop_up_all_widgets (dbox_id);
  UNBLOCK_INPUT;
  return Qnil;
}
#endif /* HAVE_DIALOG_BOXES */


#ifdef ENERGIZE
extern int desired_debuggerpanel_exposed_p;
extern int current_debuggerpanel_exposed_p;
extern int debuggerpanel_sheet;
extern void notify_energize_sheet_hidden (unsigned long);
#endif

/* #### I don't think that the `inhibit_menubar_change' flag
   has any real purpose.  Its only use seems to be so that
   update_screen_menubar() can still update the Energize-specific
   windows even when the menubar shouldn't be updated.
   Instead of doing it this way, the Energize junk should
   be separated out from this function.  --Ben */

static void
update_screen_menubar (struct screen *s, int inhibit_menubar_change)
{
  struct x_display *x = s->display.x;

  /* We assume the menubar contents has changed if the global flag is set,
     or if the current buffer has changed, or if the menubar has never
     been updated before.
   */
  int menubar_contents_changed =
    (menubar_has_changed
     || NILP (s->menubar_data)
     || (!EQ (SCREEN_MENUBAR_DATA (s)->last_menubar_buffer,
	      XWINDOW (s->selected_window)->buffer)));

  int menubar_was_visible = XtIsManaged (x->menubar_widget);
  int menubar_will_be_visible = menubar_was_visible;
  int menubar_visibility_changed;
  Cardinal new_num_top_widgets = 1; /* for the menubar */
  
#ifdef ENERGIZE
  int *old_sheets = x->current_psheets;
  int *new_sheets = x->desired_psheets;
  int old_count = x->current_psheet_count;
  int new_count = x->desired_psheet_count;
  Lisp_Object old_buf = x->current_psheet_buffer;
  Lisp_Object new_buf = x->desired_psheet_buffer;
  int psheets_changed = (old_sheets != new_sheets
			 || old_count != new_count
			 || !EQ (old_buf, new_buf));
  int debuggerpanel_changed = (desired_debuggerpanel_exposed_p
			       != current_debuggerpanel_exposed_p);

  if (desired_debuggerpanel_exposed_p && x->top_widgets [1] == 0)
    /* This happens when the screen was just created. */
    debuggerpanel_changed = 1;

  x->current_psheets = x->desired_psheets;
  x->current_psheet_count = x->desired_psheet_count;
  x->current_psheet_buffer = x->desired_psheet_buffer;
#endif /* ENERGIZE */

  if (menubar_contents_changed && !inhibit_menubar_change)
    menubar_will_be_visible = set_screen_menubar (s,
#ifndef LWLIB_USES_OLIT
						  0,
#else /* LWLIB_USES_OLIT */
      /* ####  BUG BUG BUG!
	 ####  The lwlib OLIT code doesn't correctly implement "non-deep"
	 ####  mode.  This must be fixed before this is usable at all.
       */
						  1,
#endif /* LWLIB_USES_OLIT */
						  0);

  menubar_visibility_changed = menubar_was_visible != menubar_will_be_visible;

  if (! (menubar_visibility_changed
#ifdef ENERGIZE
	 || psheets_changed || debuggerpanel_changed
#endif
	 ))
    return;

  BLOCK_INPUT;

  XtVaSetValues (x->container,
		 XtNpreserveSameSize, True,	/* Bow to the will of Jwz */
		 XtNrefigureMode, False,
		 0);

  /* Set menubar visibility */
  if (menubar_visibility_changed)
    (menubar_will_be_visible ? XtManageChild : XtUnmanageChild)
      (x->menubar_widget);

#ifdef ENERGIZE
  /* Set debugger panel visibility */
  if (debuggerpanel_changed)
    {
      Widget w;
      int sheet = debuggerpanel_sheet;

      w = lw_get_widget (sheet, x->container, 0);
      if (desired_debuggerpanel_exposed_p)
	{
	  if (! w)
	    w = lw_make_widget (sheet, x->container, 0);
	  x->top_widgets[1] = w;
	  XtManageChild (w);
	}
      else
	{
	  notify_energize_sheet_hidden (sheet);
	  if (w)
	    XtUnmanageChild (w);
	}
    }

  /* Set psheet visibility.  For the moment we just unmanage all the old
   ones, and then manage all the new ones.  If the number of psheets
   ever becomes a large number (i.e. > 1), then we can worry about a
   more sophisticated way of doing this. */
  if (psheets_changed)
    {
      int i;
      Widget w;
      unsigned long sheet;

      for (i=0; i<old_count; i++)
	{
	  sheet = old_sheets[i];
	  w = lw_get_widget (sheet, x->container, 0);
	  notify_energize_sheet_hidden (sheet);
	  if (w)
	    XtUnmanageChild (w);
	}

      for (i=0; i<new_count; i++)
	{
	  sheet = new_sheets[i];
	  /* #### This unconditional call to lw_make_widget() is a bad
	     idea.  Doesn't it cause a memory leak if the widget already
	     exists?

	     #### How does Energize know that a sheet just got displayed?
	   */
	  w = lw_make_widget (sheet, x->container, 0);
	  x->top_widgets[2+i] = w;
	  XtManageChild (w);
	}
    }

  new_num_top_widgets += 1+new_count;
#endif /* ENERGIZE */

  /* Note that new_num_top_widgets doesn't need to reflect the actual
     number of top widgets, but just the limit of x->top_widgets[].
     The EmacsManager will make sure that each of the widgets listed
     is non-NULL and is managed, and will ignore the ones that aren't. */
  XtVaSetValues (x->container, XtNnumTopAreaWidgets, new_num_top_widgets);
  XtVaSetValues (x->container, XtNrefigureMode, True, 0);
  EmacsManagerForceRefigure (x->container);
  XtVaSetValues (x->container, XtNpreserveSameSize, False, 0);
  /* The window size might not have changed but the text size
     did; thus, the base size might be incorrect.  So update
     it. */
  EmacsShellUpdateSizeHints (x->widget);

  UNBLOCK_INPUT;

#ifdef ENERGIZE
  /* Give back the focus to emacs if no psheets are displayed anymore */
  if (psheets_changed)
    {
      Lisp_Object screen;
      XSETR (screen, Lisp_Screen, s);
      Fselect_screen (screen);
    }
#endif /* ENERGIZE */
}

void
update_menubars ()
{
  struct screen *s;
  Lisp_Object tail;
  
  for (tail = Vscreen_list; CONSP (tail); tail = XCONS (tail)->cdr)
    {
      Lisp_Object screen = XCONS (tail)->car;
      if (!SCREENP (screen))
	continue;
      s = XSCREEN (screen);
      if (!SCREEN_IS_X (s))
	continue;
      /* The minibuffer does not have its own menubar, but uses
	 whatever menubar is already there.  This avoids unseemly
	 menubar flashing. */
      if (MINI_WINDOW_P (XWINDOW (s->selected_window)))
	update_screen_menubar (s, 1);
      else
	update_screen_menubar (s, 0);
    }

  menubar_has_changed = 0;

#ifdef ENERGIZE
  current_debuggerpanel_exposed_p = desired_debuggerpanel_exposed_p;
#endif
}

void
free_screen_menubar (struct screen *s)	/* called from Fdelete_screen() */
{
  Widget menubar_widget;
  if (! SCREEN_IS_X (s))
    return;
  
  menubar_widget = s->display.x->menubar_widget;
  if (menubar_widget)
    {
      LWLIB_ID id = SCREEN_MENUBAR_DATA (s)->id;
      BLOCK_INPUT;
      lw_destroy_all_widgets (id);
      UNBLOCK_INPUT;
    }

#ifdef ENERGIZE
  {
    /* Also destroy this screen's psheets */
    Widget parent = s->display.x->container;
    int *sheets = s->display.x->current_psheets;
    int i = s->display.x->current_psheet_count;
    while (i--)
      {
	unsigned long sheet = sheets [i];
	Widget w = lw_get_widget (sheet, parent, 0);
	if (w)
	  lw_destroy_widget (w);
      }
    s->display.x->current_psheet_count = 0;

    /* Is this necessary? */
    sheets = s->display.x->desired_psheets;
    i = s->display.x->desired_psheet_count;
    while (i--)
      {
	unsigned long sheet = sheets [i];
	Widget w = lw_get_widget (sheet, parent, 0);
	if (w)
	  lw_destroy_widget (w);
      }
    s->display.x->desired_psheet_count = 0;

    /* sigh... debugger panel is special... */
    if (debuggerpanel_sheet)
      {
	Widget w = lw_get_widget (debuggerpanel_sheet, parent, 0);
	if (w)
	  lw_destroy_widget (w);
      }
  }
#endif /* ENERGIZE */
}


/* This is a kludge to make sure emacs can only link against a version of
   lwlib that was compiled in the right way.  Emacs references symbols which
   correspond to the way it thinks lwlib was compiled, and if lwlib wasn't
   compiled in that way, then somewhat meaningful link errors will result.
   The alternatives to this range from obscure link errors, to obscure
   runtime errors that look a lot like bugs.
 */

static void
sanity_check_lwlib ()
{
  extern int lwlib_uses_x11r5, lwlib_does_not_use_x11r5;
  extern int lwlib_uses_lucid, lwlib_does_not_use_lucid;
  extern int lwlib_uses_motif, lwlib_does_not_use_motif;
  extern int lwlib_uses_motif_1_2, lwlib_does_not_use_motif_1_2;
  extern int lwlib_uses_olit, lwlib_does_not_use_olit;
  extern int lwlib_uses_xaw, lwlib_does_not_use_xaw;
  extern int lwlib_uses_energize, lwlib_does_not_use_energize;

#if (XlibSpecificationRelease == 5)
  lwlib_uses_x11r5 = 1;
#else
  lwlib_does_not_use_x11r5 = 1;
#endif
#ifdef LWLIB_USES_MOTIF
  lwlib_uses_motif = 1;
#else
  lwlib_does_not_use_motif = 1;
#endif
#if (XmVersion >= 1002)
  lwlib_uses_motif_1_2 = 1;
#else
  lwlib_does_not_use_motif_1_2 = 1;
#endif
#ifdef LWLIB_USES_OLIT
  lwlib_uses_olit = 1;
#else
  lwlib_does_not_use_olit = 1;
#endif
#if !defined(LWLIB_USES_MOTIF) && !defined(LWLIB_USES_OLIT)
  lwlib_uses_xaw = 1;
#else
  lwlib_does_not_use_xaw = 1;
#endif
#ifdef ENERGIZE
  lwlib_uses_energize = 1;
#else
  lwlib_does_not_use_energize = 1;
#endif
}


void
syms_of_menubar ()
{
  defsymbol (&Q_active,   ":active");   Fset (Q_active, Q_active);
  defsymbol (&Q_suffix,   ":suffix");   Fset (Q_suffix, Q_suffix);
  defsymbol (&Q_keys,     ":keys");     Fset (Q_keys,   Q_keys);
  defsymbol (&Q_style,    ":style");    Fset (Q_style,  Q_style);
  defsymbol (&Q_selected, ":selected"); Fset (Q_selected, Q_selected);

  defsymbol (&Qtoggle, "toggle");
  defsymbol (&Qradio, "radio");

  defsymbol (&Qmenu_no_selection_hook, "menu-no-selection-hook");

  popup_menu_up_p = 0;
  last_popup_selection_callback_id = -1;
  lwlib_id_tick = (1<<16);	/* start big, to not conflict with Energize */

  Vpopup_callbacks = Qnil;
  staticpro (&Vpopup_callbacks);

  {
    Lisp_Object menu_item[3];
    menu_item[0] = make_string ("", 0);
    menu_item[1] = Qnil;
    menu_item[2] = Qnil;
    Vblank_menubar = Fpurecopy (Fcons (Fvector (3, &menu_item[0]), Qnil));
    staticpro (&Vblank_menubar);
  }

  defsubr (&Sset_menubar_dirty_flag);
  defsubr (&Spopup_menu);
  defsubr (&Spopup_menu_up_p);
#ifdef HAVE_DIALOG_BOXES
  defsubr (&Spopup_dialog_box);
#endif

/*
 *
 *  This DEFVAR_LISP is just for the benefit of make-docfile.  there is no
 *  C variable Vcurrent_menubar - all C code must access the menubar via
 *  Qcurrent_menubar because it can be buffer-local.
 *

  DEFVAR_LISP ("current-menubar", &Vcurrent_menubar,
   "The current menubar.  This may be buffer-local.\n\
\n\
When the menubar is changed, the function `set-menubar-dirty-flag' has to\n\
be called for the menubar to be updated on the screen.  See `set-menubar'\n\
and `set-buffer-menubar'.\n\
\n\
A menubar is a list of menus and menu-items.\n\
A menu is a list of menu items, strings, and submenus.\n\
\n\
The first element of a menu must be a string, which is the name of the menu.\n\
This is the string that will be displayed in the parent menu, if any.  For\n\
toplevel menus, it is ignored.  This string is not displayed in the menu\n\
itself.\n\
\n\
If an element of a menu (or menubar) is a string, then that string will be\n\
presented as unselectable text.\n\
\n\
If an element of a menu is a string consisting solely of hyphens, then that\n\
item will be presented as a solid horizontal line.\n\
\n\
If an element of a menu is a list, it is treated as a submenu.  The name of\n\
that submenu (the first element in the list) will be used as the name of the\n\
item representing this menu on the parent.\n\
\n\
If an element of a menubar is `nil', then it is used to represent the\n\
division between the set of menubar-items which are flushleft and those\n\
which are flushright.\n\
\n\
Otherwise, the element must be a vector, which describes a menu item.\n\
A menu item can have any of the following forms:\n\
\n\
 [ \"name\" callback <active-p> ]\n\
 [ \"name\" callback <active-p> \"suffix\" ]\n\
 [ \"name\" callback :<keyword> <value>  :<keyword> <value> ... ]\n\
\n\
The name is the string to display on the menu; it is filtered through the\n\
resource database, so it is possible for resources to override what string\n\
is actually displayed.\n\
\n\
If the `callback' of a menu item is a symbol, then it must name a command.\n\
It will be invoked with `call-interactively'.  If it is a list, then it is\n\
evaluated with `eval'.\n\
\n\
The possible keywords are this:\n\
\n\
 :active   <form>    Same as <active-p> in the first two forms: the\n\
                     expression is evaluated just before the menu is\n\
                     displayed, and the menu will be selectable only if\n\
                     the result is non-nil.\n\
\n\
 :suffix   \"string\"  Same as \"suffix\" in the second form: the suffix is\n\
                     appended to the displayed name, providing a convenient\n\
                     way of adding the name of a command's ``argument'' to\n\
                     the menu, like ``Kill Buffer NAME''.\n\
\n\
 :keys     \"string\"  Normally, the keyboard equivalents of commands in\n\
                     menus are displayed when the `callback' is a symbol.\n\
                     This can be used to specify keys for more complex menu\n\
                     items.  It is passed through `substitute-command-keys'\n\
                     first.\n\
\n\
 :style    <style>   Specifies what kind of object this menu item is:\n\
\n\
                        nil     A normal menu item.\n\
                        toggle  A toggle button.\n\
                        radio   A radio button.\n\
\n\
                     The only difference between toggle and radio buttons is\n\
                     how they are displayed.  But for consistency, a toggle\n\
                     button should be used when there is one option whose\n\
                     value can be turned on or off, and radio buttons should\n\
                     be used when there is a set of mutally exclusive\n\
                     options.  When using a group of radio buttons, you\n\
                     should arrange for no more than one to be marked as\n\
                     selected at a time.\n\
\n\
 :selected <form>    Meaningful only when STYLE is `toggle' or `radio'.\n\
                     This specifies whether the button will be in the\n\
                     selected or unselected state.\n\
\n\
For example:\n\
\n\
 [ \"Save As...\"    write-file  t ]\n\
 [ \"Revert Buffer\" revert-buffer (buffer-modified-p) ]\n\
 [ \"Read Only\"     toggle-read-only :style toggle :selected buffer-read-only ]\n\
\n\
See menubar.el for many more examples.\n\
\n\
After the menubar is clicked upon, but before any menus are popped up,\n\
the functions on the `activate-menubar-hook' are invoked to make changes\n\
to the menus and menubar.  This is intended to implement lazy alteration\n\
of the sensitivity of menu items.");
  */

  defsymbol (&Qcurrent_menubar, "current-menubar");
  Fset (Qcurrent_menubar, Qnil);

  DEFVAR_LISP ("activate-menubar-hook", &Vactivate_menubar_hook,
   "Function or functions called before a menubar menu is pulled down.\n\
These functions are called with no arguments, and should interrogate and\n\
modify the value of `current-menubar' as desired.\n\
\n\
The functions on this hook are invoked after the mouse goes down, but before\n\
the menu is mapped, and may be used to activate, deactivate, add, or delete\n\
items from the menus.\n\
\n\
These functions may return the symbol `t' to assert that they have made\n\
no changes to the menubar.  If any other value is returned, the menubar is\n\
recomputed.  If `t' is returned but the menubar has been changed, then the\n\
changes may not show up right away.  Returning `nil' when the menubar has\n\
not changed is not so bad; more computation will be done, but redisplay of\n\
the menubar will still be performed optimally.");
  Vactivate_menubar_hook = Qnil;
  defsymbol (&Qactivate_menubar_hook, "activate-menubar-hook");

/*
 *  This DEFVAR_LISP is just for the benefit of make-docfile.
  DEFVAR_LISP ("menu-no-selection-hook", &Vmenu_no_selection_hook,
   "Function or functions to call when a menu or dialog box is dismissed\n\
without a selecting having been made.");
 */
  Fset (Qmenu_no_selection_hook, Qnil);

  DEFVAR_BOOL ("menubar-show-keybindings", &menubar_show_keybindings,
    "If true, the menubar will display keyboard equivalents.\n\
If false, only the command names will be displayed.");
  menubar_show_keybindings = 1;

  DEFVAR_BOOL ("popup-menu-titles", &popup_menu_titles,
	       "If true, popup menus will have title bars at the top.");
  popup_menu_titles = 1;

  sanity_check_lwlib ();
}
