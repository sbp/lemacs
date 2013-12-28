/* Implements an elisp-programmable menubar.
   Copyright (C) 1993
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

#include <stdio.h>	/* for sprintf */

#include "lisp.h"

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Paned.h>

#include "screen.h"
#include "xterm.h"
#include "events.h"
#include "window.h"
#include "buffer.h"

#include "commands.h"           /* zmacs_regions */

#include "dispmisc.h"

#include "lwlib.h"

#ifdef LWLIB_USES_MOTIF
/* right now there is only Motif support for dialog boxes */
# define HAVE_DIALOG_BOXES
#endif

/* This variable is 1 if the menubar widget has to be updated. 
 It is set to 1 by set-menubar-dirty-flag and cleared when the widget
 has been indapted. */
int menubar_has_changed;

Lisp_Object Qcurrent_menubar;
Lisp_Object Qmenu_no_selection_hook;

static void set_screen_menubar (struct screen *s, int deep_p);

/* we need a unique id for each popup menu and dialog box */
unsigned int popup_id_tick;

/* count of menus/dboxes currently up */
int popup_menu_up_p;
int dbox_up_p;

int menubar_show_keybindings;


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
# define wv_set_enabled(wv,form)			\
   do { Lisp_Object _f_ = (form);			\
	  (wv)->enabled = (NILP (_f_) ? 0 : 		\
			   EQ (_f_, Qt) ? 1 :		\
			   !NILP (Feval (_f_)));	\
      } while (0)
#else
  /* Treat the activep slot of the menu item as a boolean */
# define wv_set_enabled(wv,form)			\
      (wv)->enabled = (!NILP ((form)))
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
static widget_value *
menu_item_descriptor_to_widget_value_1 (Lisp_Object desc, 
					int menubar_p, int deep_p)
{
  widget_value *wv;
  Lisp_Object wv_closure;
  int count = specpdl_depth ();

  BLOCK_INPUT;
  wv = malloc_widget_value ();
  UNBLOCK_INPUT;

  wv_closure = long_to_cons ((unsigned long) wv);
  record_unwind_protect (widget_value_unwind, wv_closure);

  if (STRINGP (desc))
    {
      wv->name = (char *) XSTRING (desc)->data;
      wv->value = 0;
      wv->enabled = 1;
    }
  else if (VECTORP (desc))
    {
      if (vector_length (XVECTOR (desc)) < 3 ||
	  vector_length (XVECTOR (desc)) > 4)
	signal_error (Qerror, 
                      list2 (build_string
			     ("button descriptors must be 3 or 4 long"),
			     desc));

      CHECK_STRING (XVECTOR (desc)->contents [0], 0);
      wv->name = (char *) XSTRING (XVECTOR (desc)->contents [0])->data;
      if (vector_length (XVECTOR (desc)) == 4
          && !NILP (XVECTOR (desc)->contents [3]))
	{
	  CHECK_STRING (XVECTOR (desc)->contents [3], 0);
	  wv->value = (char *) XSTRING (XVECTOR (desc)->contents [3])->data;
	}
      else
	wv->value = 0;
      wv_set_enabled (wv, XVECTOR (desc)->contents [2]);  /* runs Feval */
      {
	Lisp_Object fn = XVECTOR (desc)->contents [1];
	wv->call_data = LISP_TO_VOID (fn);
	if (menubar_show_keybindings && SYMBOLP (fn))
	  {
	    char buf [1024];
	    where_is_to_char (fn, Fcurrent_local_map (), Qnil, buf);
	    if (buf [0])
	      wv->key = xstrdup (buf);
	    else
	      wv->key = 0;
	  }
      }
    }
  else if (CONSP (desc))
    {
      if (STRINGP (XCONS (desc)->car))
	{
	  wv->name = (char *) XSTRING (XCONS (desc)->car)->data;
	  desc = Fcdr (desc);
	}
      else if (menubar_p)
	wv->name = "menubar";
      else
	{
	  signal_error (Qerror,
                        list2 (build_string
			       ("menu name (first element) must be a string"),
                               desc));
	}

      wv->value = 0;
      wv->enabled = 1;
      if (deep_p || menubar_p)
	{
	  widget_value *prev = 0, *next;
	  for (; !NILP (desc); desc = Fcdr (desc))
	    {
	      next = menu_item_descriptor_to_widget_value_1 (Fcar (desc),
							     0, deep_p);
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
  else
    {
      if (NILP (desc)) /* ignore nil for now */
	wv = 0;
      else
	signal_error (Qerror, list2 (build_string ("unrecognised descriptor"),
                                     desc));
    }

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


static Lisp_Object
restore_gc_inhibit (Lisp_Object val)
{
  gc_currently_forbidden = XINT (val);
  return val;
}

static widget_value *
menu_item_descriptor_to_widget_value (Lisp_Object desc, 
				      int menubar_p, int deep_p)
{
  widget_value *wv;
  int count = specpdl_depth ();
  record_unwind_protect (restore_gc_inhibit,
			 make_number (gc_currently_forbidden));
  gc_currently_forbidden = 1;
  wv = menu_item_descriptor_to_widget_value_1 (desc, menubar_p, deep_p);
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


struct menubar_data
{
  struct lcrecord_header header;
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
  char buf[40];
  sprintf (buf, "#<menubar-data 0x%x>", 
           ((struct menubar_data *) XRECORD (obj))->header.uid);
  write_string_1 (buf, -1, stream);
}
static int sizeof_menubar_data (void *h)
{
  return (sizeof (struct menubar_data)); 
}
DEFINE_LRECORD_IMPLEMENTATION (lrecord_menubar_data,
                               mark_menubar_data, print_menubar_data,
                               0, sizeof_menubar_data, 0);


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
  else
      /* syntax checking has already been done */
      abort ();

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
    set_screen_menubar (s, 1);
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
  Faccept_process_output (Qnil);

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
                   list2 (Qquote, list2 (build_string ("illegal menu callback:"),
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

static void
set_screen_menubar (struct screen *s, int deep_p)
{
  widget_value *data;
  Lisp_Object obuf = Fcurrent_buffer ();
  Lisp_Object menubar;

  if (! SCREEN_IS_X (s))
    return;

  /* evaluate `current-menubar' in the buffer of the selected window of
     the screen in question.
     */
  Fset_buffer (XWINDOW (s->selected_window)->buffer);
  menubar = Fsymbol_value (Qcurrent_menubar);
  Fset_buffer (obuf);

  data = compute_menubar_data (s, menubar, deep_p);

  if (NILP (s->menubar_data))
  {
    struct menubar_data *data = alloc_lcrecord (sizeof (struct menubar_data),
                                                lrecord_menubar_data);

    data->last_menubar_buffer = Qnil;
    data->menubar_callbacks = Qnil;
    data->menubar_contents_up_to_date = 0;
    XSET (s->menubar_data, Lisp_Record, data);
  }

  {
    Widget menubar_widget = s->display.x->menubar_widget;
    int id = (int) s;

    if (data && !data->next && !data->contents)
      abort ();

    BLOCK_INPUT;

    if (!data)
      {
	if (menubar_widget)
	  lw_destroy_all_widgets (id);
	s->display.x->menubar_widget = 0;
      }
    else if (menubar_widget)
      lw_modify_all_widgets (id, data, deep_p ? True : False);
    else
      {
	Widget parent = s->display.x->column_widget;

	/* It's the first time we map the menubar so compute its
	   contents completely once.  This makes sure that the menubar
	   components are created with the right type. */
	if (!deep_p)
	  {
	    free_menubar_widget_value_tree (data);
	    data = compute_menubar_data (s, menubar, 1);
	  }

	menubar_widget =
	  lw_create_widget ("menubar", "menubar", id, data, parent,
			    0, pre_activate_callback,
			    menubar_selection_callback, 0);
	s->display.x->menubar_widget = menubar_widget;
	XtVaSetValues (menubar_widget,
		       XtNshowGrip, 0,
		       XtNresizeToPreferred, 1,
		       XtNallowResize, 1,
		       0);
      }
    UNBLOCK_INPUT;
  }
  if (data)
    free_menubar_widget_value_tree (data);
  SCREEN_MENUBAR_DATA (s)->menubar_contents_up_to_date = deep_p;
  SCREEN_MENUBAR_DATA (s)->last_menubar_buffer =
    XWINDOW (s->selected_window)->buffer;
  menubar_has_changed = 0;

  gcpro_menu_callbacks (menubar, &SCREEN_MENUBAR_DATA (s)->menubar_callbacks);
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
  if (popup_menu_up_p == 0) abort ();
  popup_menu_up_p--;
  /* if this isn't called immediately after the selection callback, then
     there wasn't a menu selection. */
  if (id != last_popup_selection_callback_id)
    menubar_selection_callback (widget, id, (XtPointer) -1);
  BLOCK_INPUT;
  lw_destroy_all_widgets (id);
  UNBLOCK_INPUT;
  ungcpro_popup_callbacks (make_number (id));
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
  if (dbox_up_p == 0) abort ();
  dbox_up_p--;
  maybe_run_dbox_text_callback (id);
  menubar_selection_callback (widget, id, client_data);
  BLOCK_INPUT;
  lw_destroy_all_widgets (id);
  UNBLOCK_INPUT;
  ungcpro_popup_callbacks (make_number (id));
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


DEFUN ("popup-menu", Fpopup_menu, Spopup_menu, 1, 1, 0,
       "Pop up the given menu.\n\
A menu description is a list of menu items, strings, and submenus.\n\
\n\
The first element of a menu must be a string, which is the name of the\n\
menu.  This is the string that will be displayed in the parent menu, if\n\
any.  For toplevel menus, it is ignored.  This string is not displayed\n\
in the menu itself.\n\
\n\
A menu item is a vector of three or four elements:\n\
\n\
 - the name of the menu item (a string);\n\
 - the `callback' of that item;\n\
 - whether this item is active (selectable);\n\
 - and an optional string to append to the name.\n\
\n\
If the `callback' of a menu item is a symbol, then it must name a command.\n\
It will be invoked with `call-interactively'.  If it is a list, then it is\n\
evaluated with `eval'.\n\
\n\
The fourth element of a menu item is a convenient way of adding the name\n\
of a command's ``argument'' to the menu, like ``Kill Buffer NAME''.\n\
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
The syntax, more precisely:\n\
\n\
   form		:=  <something to pass to `eval'>\n\
   command	:=  <a symbol or string, to pass to `call-interactively'>\n\
   callback 	:=  command | form\n\
   active-p	:=  <t, nil, or a form to evaluate to decide whether this\n\
		    item should be selectable>\n\
   text		:=  <string, non selectable>\n\
   name		:=  <string>\n\
   argument	:=  <string>\n\
   menu-item	:=  '['  name callback active-p [ argument ]  ']'\n\
   menu		:=  '(' name [ menu-item | menu | text ]+ ')'")
     (menu_desc)
     Lisp_Object menu_desc;
{
  int menu_id;
  struct screen *s = selected_screen;
  widget_value *data;
  Widget parent, menu;

  if (!SCREEN_IS_X (s)) error ("not an X screen");
  if (SYMBOLP (menu_desc))
    menu_desc = Fsymbol_value (menu_desc);
  CHECK_CONS (menu_desc, 0);
  CHECK_STRING (XCONS (menu_desc)->car, 0);
  data = menu_item_descriptor_to_widget_value (menu_desc, 0, 1);

  if (! data) error ("no menu");
  
  parent = s->display.x->widget;

  BLOCK_INPUT;
  menu_id = ++popup_id_tick;
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
  UNBLOCK_INPUT;
  return Qnil;
}

DEFUN ("popup-menu-up-p", Fpopup_menu_up_p, Spopup_menu_up_p, 0, 0, 0,
       "Returns T if a popup menu is up, NIL otherwise.\n\
See popup-menu.")
     ()
{
  return popup_menu_up_p ? Qt : Qnil;
}

#ifdef HAVE_DIALOG_BOXES

static const char * const button_names [] = {
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
  widget_value *prev = 0, *kids = 0;
  int n = 0;

  CHECK_CONS (desc, 0);
  CHECK_STRING (XCONS (desc)->car, 0);
  name = (char *) XSTRING (XCONS (desc)->car)->data;
  desc = XCONS (desc)->cdr;
  if (!CONSP (desc))
    error ("dialog boxes must have some buttons");

  kids = prev = malloc_widget_value ();
  prev->name = "message";
  prev->value = name;
  prev->enabled = 1;

  if (VECTORP (XCONS (desc)->car) &&
      EQ (XVECTOR (XCONS (desc)->car)->contents [0], Qtext))
    {
      Lisp_Object button = XCONS (desc)->car;
      widget_value *wv;
      if (vector_length (XVECTOR (button)) != 4)
	error ("dialog box text field descriptors must be 4 long");
      CHECK_STRING (XVECTOR (button)->contents [2], 0);
      BLOCK_INPUT;
      wv = malloc_widget_value ();
      UNBLOCK_INPUT;
      wv->name = "value";
      wv->value = (char *) XSTRING (XVECTOR (button)->contents [2])->data;
      wv->call_data = LISP_TO_VOID (XVECTOR (button)->contents [1]);
      wv_set_enabled (wv, XVECTOR (button)->contents [3]);
      text_field_p = 1;
      prev->next = wv;
      prev = wv;
      desc = Fcdr (desc);
    }

  for (; !NILP (desc); desc = Fcdr (desc))
    {
      Lisp_Object button = XCONS (desc)->car;
      Lisp_Object cb;
      widget_value *wv;

      if (NILP (button))
	{
	  if (partition_seen)
	    error ("more than one partition (nil) seen in dbox spec");
	  partition_seen = 1;
	  continue;
	}
      CHECK_VECTOR (button, 0);
      if (vector_length (XVECTOR (button)) != 3)
        signal_error (Qerror, list2 (build_string
                                     ("button descriptors must be 3 long"),
                                     button));
      CHECK_STRING (XVECTOR (button)->contents [0], 0);
      cb = XVECTOR (button)->contents [1];
      
      BLOCK_INPUT;
      wv = malloc_widget_value ();
      UNBLOCK_INPUT;
      wv->name = (char *) button_names [n];
      wv->value = (char *) XSTRING (XVECTOR (button)->contents [0])->data;
      wv_set_enabled (wv, XVECTOR (button)->contents [2]);
      wv->call_data = LISP_TO_VOID (XVECTOR (button)->contents [1]);

      if (partition_seen)
	rbuttons++;
      else
	lbuttons++;
      n++;

      if (lbuttons > 9 || rbuttons > 9)
	error ("too many buttons (9)"); /* #### this leaks */

      prev->next = wv;
      prev = wv;
    }

  if (n == 0)
    error ("dialog boxes must have some buttons");
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
 - The first element of the list is a string to display in the dialog box.\n\
 - The rest of the elements are descriptions of the dialog box's buttons.\n\
   Each one is a vector of three elements:\n\
   - The first element is the text of the button.\n\
   - The second element is the `callback'.\n\
   - The third element is t or nil, whether this button is selectable.\n\
\n\
If the `callback' of a button is a symbol, then it must name a command.\n\
It will be invoked with `call-interactively'.  If it is a list, then it is\n\
evaluated with `eval'.\n\
\n\
One (and only one) of the buttons may be `nil'.  This marker means that all\n\
following buttons should be flushright instead of flushleft.\n\
\n\
The syntax, more precisely:\n\
\n\
   form		:=  <something to pass to `eval'>\n\
   command	:=  <a symbol or string, to pass to `call-interactively'>\n\
   callback 	:=  command | form\n\
   active-p	:=  <t, nil, or a form to evaluate to decide whether this\n\
		    button should be selectable>\n\
   name		:=  <string>\n\
   partition	:=  'nil'\n\
   button	:=  '['  name callback active-p ']'\n\
   dialog	:=  '(' name [ button ]+ [ partition [ button ]+ ] ')'")
     (dbox_desc)
     Lisp_Object dbox_desc;
{
  int dbox_id;
  struct screen *s = selected_screen;
  widget_value *data;
  Widget parent, dbox;

  if (!SCREEN_IS_X (s)) error ("not an X screen");
  if (SYMBOLP (dbox_desc))
    dbox_desc = Fsymbol_value (dbox_desc);
  CHECK_CONS (dbox_desc, 0);
  CHECK_STRING (XCONS (dbox_desc)->car, 0);
  data = dbox_descriptor_to_widget_value (dbox_desc);

  if (! data) abort ();
  
  parent = s->display.x->widget;

  BLOCK_INPUT;
  dbox_id = ++popup_id_tick;
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

  dbox_up_p++;
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

void
update_screen_menubars ()
{
  struct screen* s;
  Lisp_Object tail;
  int save_menubar_has_changed = menubar_has_changed;
  
  for (tail = Vscreen_list; CONSP (tail); tail = XCONS (tail)->cdr)
    {
      Lisp_Object screen = XCONS (tail)->car;
      if (!SCREENP (screen))
	continue;
      s = XSCREEN (screen);
      
      if (!SCREEN_IS_X (s))
	continue;
      
      /* If the menubar_has_changed flag was set, or the displayed buffer
	 has changed we have to update the menubar. */
      if (save_menubar_has_changed
	  || !RECORD_TYPEP (s->menubar_data, lrecord_menubar_data)
	  || (!EQ (SCREEN_MENUBAR_DATA (s)->last_menubar_buffer,
		   XWINDOW (s->selected_window)->buffer)))
	if (!MINI_WINDOW_P (XWINDOW (s->selected_window)))
#ifndef LWLIB_USES_OLIT
	  set_screen_menubar (s, 0);
#else /* LWLIB_USES_OLIT */
      /* ####  BUG BUG BUG!
	 ####  The lwlib OLIT code doesn't correctly implement "non-deep" mode.
	 ####  This must be fixed before this is usable at all.
       */
	  set_screen_menubar (s, 1);
#endif /* LWLIB_USES_OLIT */
    }
}

extern void fix_pane_constraints (Widget);

static void
update_one_screen_psheets (screen)
     Lisp_Object screen;
{
  struct screen *s = XSCREEN (screen);
  struct x_display *x = s->display.x;
  
#ifdef ENERGIZE
  int i;
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
#endif
  int menubar_changed;
  
  menubar_changed = (x->menubar_widget
		     && !XtIsManaged (x->menubar_widget));

#ifdef ENERGIZE
  x->current_psheets = x->desired_psheets;
  x->current_psheet_count = x->desired_psheet_count;
  x->current_psheet_buffer = x->desired_psheet_buffer;
#endif

  if (! (menubar_changed
#ifdef ENERGIZE
	 || psheets_changed || debuggerpanel_changed
#endif
	 ))
    return;

  BLOCK_INPUT;
  XawPanedSetRefigureMode (x->column_widget, 0);
  
  /* the order in which children are managed is the top to
     bottom order in which they are displayed in the paned window.
     First, remove the text-area widget.
   */
  XtUnmanageChild (x->edit_widget);

#ifdef ENERGIZE
  /* Remove the psheets that are there now
   */
  if (menubar_changed || debuggerpanel_changed || psheets_changed)
    {
      i = old_count;
      while (i)
	{
	  Widget w;
	  unsigned long sheet = old_sheets[--i];
	  w = lw_get_widget (sheet, x->column_widget, 0);
	  if (psheets_changed && w)
	    {
	      notify_energize_sheet_hidden (sheet);
	      XtVaSetValues (w, XtNmappedWhenManaged, 0, 0);
	      XtUnmanageChild (w);
	      XtUnmapWidget (w);
	    }
	}
    }

  /* remove debugger panel if present */
  if (current_debuggerpanel_exposed_p && debuggerpanel_sheet &&
      (menubar_changed || debuggerpanel_changed))
    {
      Widget w;
      int sheet = debuggerpanel_sheet;
      w = lw_get_widget (sheet, x->column_widget, 0);
      if (!desired_debuggerpanel_exposed_p && w)
	{
	  notify_energize_sheet_hidden (sheet);
	  XtVaSetValues (w, XtNmappedWhenManaged, 0, 0);
	  XtUnmanageChild (w);
	  XtUnmapWidget (w);
	}
    }
#endif

  /* remove the menubar that is there now, and put up the menubar that
     should be there.
   */
  if (menubar_changed)
    {
      XtManageChild (x->menubar_widget);
      XtMapWidget (x->menubar_widget);
      XtVaSetValues (x->menubar_widget, XtNmappedWhenManaged, 1, 0);
    }

#ifdef ENERGIZE
  /* add debugger panel if desired */
  if (desired_debuggerpanel_exposed_p && debuggerpanel_sheet &&
      (menubar_changed || debuggerpanel_changed))
    {
      Widget w;
      w = lw_make_widget (debuggerpanel_sheet, x->column_widget, 0);
      fix_pane_constraints (w);
      XtManageChild (w);
      XtMapWidget (w);
      XtVaSetValues (w, XtNmappedWhenManaged, 1, 0);
    }
  
  /* Add the psheets that should be there now
   */
  i = new_count;
  while (i)
    {
      Widget w;
      unsigned long sheet = new_sheets[--i];
      w = lw_make_widget (sheet, x->column_widget, 0);
      fix_pane_constraints (w);
      /* Put the mappedWhenManaged property back in or the Motif widgets
	 refuse to take the focus! */
      XtVaSetValues (w, XtNmappedWhenManaged, 1, 0);
      XtManageChild (w);
    }

  /* Give back the focus to emacs if no p_sheets are displayed anymore */
  if (psheets_changed)
    Fselect_screen (screen);
#endif

  /* Re-manage the text-area widget */
  XtManageChild (x->edit_widget);

  /* and now thrash the sizes */
  XawPanedSetRefigureMode (x->column_widget, 1);
  UNBLOCK_INPUT;
}

void
update_psheets ()
{
  struct screen* s;
  Lisp_Object tail;

  for (tail = Vscreen_list; CONSP (tail); tail = XCONS (tail)->cdr)
    {
      Lisp_Object screen = XCONS (tail)->car;
      struct window* w;
      struct buffer* buf;
      if (!SCREENP (screen))
	continue;
      s = XSCREEN (screen);
      w = XWINDOW (s->selected_window);
      buf = XBUFFER (w->buffer);

      if (!SCREEN_IS_X (s)
	  || MINI_WINDOW_P (w)
	  || EQ (screen, Vglobal_minibuffer_screen))
	continue;

      update_one_screen_psheets (screen);
    }
#ifdef ENERGIZE
  current_debuggerpanel_exposed_p = desired_debuggerpanel_exposed_p;
#endif
}


void
free_screen_menubar (struct screen *s)	/* called from Fdelete_screen() */
{
  Widget menubar_widget;
  int id;

  if (! SCREEN_IS_X (s))
    return;
  
  menubar_widget = s->display.x->menubar_widget;
  id = (int) s;
  
  if (menubar_widget)
    {
      BLOCK_INPUT;
      lw_destroy_all_widgets (id);
      UNBLOCK_INPUT;
    }

#ifdef ENERGIZE
  {
    /* Also destroy this screen's psheets */
    Widget parent = s->display.x->column_widget;
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
#endif
}


void
syms_of_menubar ()
{
  defsymbol (&Qmenu_no_selection_hook, "menu-no-selection-hook");

  popup_menu_up_p = 0;
  last_popup_selection_callback_id = -1;
  popup_id_tick = (1<<16);	/* start big, to not conflict with Energize */

  Vpopup_callbacks = Qnil;
  staticpro (&Vpopup_callbacks);

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
The first element of a menu must be a string, which is the name of the\n\
menu.  This is the string that will be displayed in the menubar, or in\n\
the parent menu.  This string is not displayed in the menu itself.\n\
\n\
A menu item is a vector of three or four elements:\n\
\n\
 - the name of the menu item (a string);\n\
 - the `callback' of that item;\n\
 - whether this item is active (selectable);\n\
 - and an optional string to append to the name.\n\
\n\
If the `callback' of a menu item is a symbol, then it must name a command.
It will be invoked with `call-interactively'.  If it is a list, then it is
evaluated with `eval'.\n\
\n\
The fourth element of a menu item is a convenient way of adding the name\n\
of a command's ``argument'' to the menu, like ``Kill Buffer NAME''.\n\
\n\
If an element of a menu (or menubar) is a string, then that string will be\n\
presented in the menu (or menubar) as unselectable text.\n\
\n\
If an element of a menu is a string consisting solely of hyphens, then that\n\
item will be presented as a solid horizontal line.\n\
\n\
If an element of a menu is a list, it is treated as a submenu.  The name of\n\
that submenu (the first element in the list) will be used as the name of\n\
the item representing this menu on the parent.\n\
\n\
If an element of a menubar is `nil', then it is used to represent the\n\
division between the set of menubar-items which are flushleft and those\n\
which are flushright.  (Note: this isn't completely implemented yet.)\n\
\n\
After the menubar is clicked upon, but before any menus are popped up,\n\
the functions on the `activate-menubar-hook' are invoked to make changes\n\
to the menus and menubar.  This is intended to implement lazy alteration\n\
of the sensitivity of menu items.\n\
\n\
The syntax, more precisely:\n\
\n\
   form		:=  <something to pass to `eval'>\n\
   command	:=  <a symbol or string, to pass to `call-interactively'>\n\
   callback 	:=  command | form\n\
   active-p	:=  <t, nil, or a form to evaluate to decide whether this\n\
		    item should be selectable>\n\
   text		:=  <string, non selectable>\n\
   name		:=  <string>\n\
   argument	:=  <string>\n\
   menu-item	:=  '['  name callback active-p [ argument ]  ']'\n\
   menu		:=  '(' name [ menu-item | menu | text ]+ ')'\n\
   partition	:=  'nil'\n\
   menubar	:=  '(' [ menu-item | menu | text ]* [ partition ]\n\
		        [ menu-item | menu | text ]*\n\
		     ')'");

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
}
