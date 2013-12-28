/* A general interface to the widgets of different toolkits.
   Copyright (C) 1992 Lucid, Inc.

This file is part of the Lucid Widget Library.

The Lucid Widget Library is free software; you can redistribute it and/or 
modify it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

The Lucid Widget Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of 
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <X11/StringDefs.h>
#include "lwlib-internal.h"

#ifdef USE_MOTIF
#include "lwlib-Xm.h"
#endif

#ifdef USE_LUCID
#include "lwlib-Xlw.h"
#endif

#ifdef USE_OLIT
#include "lwlib-Xol.h"
#endif

#if (!defined(USE_MOTIF) && !defined(USE_LUCID) && !defined(USE_OLIT))
#error at least one of USE_MOTIF, USE_LUCID or USE_OLIT must be defined
#endif


/* List of all widgets managed by the library. */
static widget_info*
all_widget_info = NULL;

/* Forward declarations */
static void
instanciate_widget_instance (widget_instance* instance);

/* utility functions for widget_instance and widget_info */
static widget_info *
allocate_widget_info (char* type, char* name, BITS32 id, widget_value* val,
		      lw_callback pre_activate_cb, lw_callback selection_cb,
		      lw_callback post_activate_cb)
{
  widget_info* info = (widget_info*)malloc (sizeof (widget_info));
  info->type = type;
  info->name = name;
  info->id = id;
  info->val = val;
  info->busy = False;
  info->pre_activate_cb = pre_activate_cb;
  info->selection_cb = selection_cb;
  info->post_activate_cb = post_activate_cb;
  info->instances = NULL;

  info->next = all_widget_info;
  all_widget_info = info;

  return info;
}

static void
mark_widget_destroyed (Widget widget, XtPointer closure, XtPointer call_data)
{
  widget_instance* instance = (widget_instance*)closure;

  /* be very conservative */
  if (instance->widget == widget)
    instance->widget = NULL;
}

static widget_instance *
allocate_widget_instance (widget_info* info, Widget parent, Boolean pop_up_p)
{
  widget_instance* instance =
    (widget_instance*)malloc (sizeof (widget_instance));
  instance->parent = parent;
  instance->pop_up_p = pop_up_p;
  instance->info = info;
  instance->next = info->instances;
  info->instances = instance;

  instanciate_widget_instance (instance);

  XtAddCallback (instance->widget, XtNdestroyCallback,
		 mark_widget_destroyed, (XtPointer)instance);
  return instance;
}

static widget_info *
get_widget_info (BITS32 id, Boolean remove_p)
{
  widget_info* info;
  widget_info* prev;
  for (prev = NULL, info = all_widget_info;
       info;
       prev = info, info = info->next)
    if (info->id == id)
     {
       if (remove_p)
	 {
	   if (prev)
	     prev->next = info->next;
	   else
	     all_widget_info = info->next;
	 }
      return info;
     }
  return NULL;
}

static widget_instance *
get_widget_instance (Widget widget, Boolean remove_p)
{
  widget_info* info;
  widget_instance* instance;
  widget_instance* prev;
  for (info = all_widget_info; info; info = info->next)
    for (prev = NULL, instance = info->instances;
	 instance;
	 prev = instance, instance = instance->next)
      if (instance->widget == widget)
	{
	  if (remove_p)
	    {
	      if (prev)
		prev->next = instance->next;
	      else
		info->instances = NULL;
	    }
	  return instance;
	}
}


/* utility function for widget_value */
static char *
safe_strdup (char* s)
{
  return s ? strdup (s) : NULL;
}

static void
safe_free_str (char* s)
{
  int i;
  if (s)
    free (s);
}

static Boolean
safe_strcmp (char* s1, char* s2)
{
  if (!!s1 ^ !!s2) return True;
  return (s1 && s2) ? strcmp (s1, s2) : s1 ? False : !!s2;
}

static int
max (int i1, int i2)
{
  return i1 > i2 ? i1 : i2;
}

static widget_value *
copy_widget_value (widget_value* val, change_type change,
		   Boolean contents_p, Boolean next_p)
{
  widget_value* copy;
  
  if (!val)
    return NULL;

  copy = (widget_value*)malloc (sizeof (widget_value));
  copy->name = safe_strdup (val->name);
  copy->value = safe_strdup (val->value);
  copy->key = safe_strdup (val->key);
  copy->enabled = val->enabled;
  copy->selected = val->selected;
  copy->change = change;

  if (contents_p)
    copy->contents = copy_widget_value (val->contents, change, contents_p,
					next_p);
  else
    copy->contents = NULL;

  copy->call_data = val->call_data;

  if (next_p)
    copy->next = copy_widget_value (val->next, change, contents_p, next_p);
  else
    copy->next = NULL;

  return copy;
}

static void
free_widget_value (widget_value* val, Boolean contents_p, Boolean next_p)
{
  int i;
  widget_value* next;
  if (!val)
    return;

  if (contents_p)
    free_widget_value (val->contents, contents_p, next_p);

  safe_free_str (val->name);
  safe_free_str (val->value);
  safe_free_str (val->key);

  next = val->next;
  free (val);

  /* tail rec */
  if (next_p)
    free_widget_value (next, contents_p, next_p);
}


#if 0
# define EXPLAIN(name, oc, nc, desc, a1, a2)				\
   printf ("Change: \"%s\"\tmax(%s=%d,%s=%d)\t%s %d %d\n",		\
	   name,							\
	   (oc == NO_CHANGE ? "none" :					\
	    (oc == INVISIBLE_CHANGE ? "invisible" :			\
	     (oc == VISIBLE_CHANGE ? "visible" :			\
	      (oc == STRUCTURAL_CHANGE ? "structural" : "???")))),	\
	   oc,								\
	   (nc == NO_CHANGE ? "none" :					\
	    (nc == INVISIBLE_CHANGE ? "invisible" :			\
	     (nc == VISIBLE_CHANGE ? "visible" :			\
	      (nc == STRUCTURAL_CHANGE ? "structural" : "???")))),	\
	   nc, desc, a1, a2)
#else
# define EXPLAIN(name, oc, nc, desc, a1, a2)
#endif


static widget_value *
merge_widget_value (widget_value* val1, widget_value* val2)
{
  change_type change;
  widget_value* merged_next;
  widget_value* merged_contents;

  if (!val1)
    {
      if (val2)
	return copy_widget_value (val2, STRUCTURAL_CHANGE, True, True);
      else
	return NULL;
    }
  if (!val2)
    {
      free_widget_value (val1, True, True);
      return NULL;
    }
  
  change = NO_CHANGE;

  if (safe_strcmp (val1->name, val2->name))
    {
      EXPLAIN (val1->name, change, STRUCTURAL_CHANGE, "name change",
	       val1->name, val2->name);
      change = max (change, STRUCTURAL_CHANGE);
      safe_free_str (val1->name);
      val1->name = safe_strdup (val2->name);
    }
  if (safe_strcmp (val1->value, val2->value))
    {
      EXPLAIN (val1->name, change, VISIBLE_CHANGE, "value change",
	       val1->value, val2->value);
      change = max (change, VISIBLE_CHANGE);
      safe_free_str (val1->value);
      val1->value = safe_strdup (val2->value);
    }
  if (safe_strcmp (val1->key, val2->key))
    {
      EXPLAIN (val1->name, change, VISIBLE_CHANGE, "key change",
	       val1->key, val2->key);
      change = max (change, VISIBLE_CHANGE);
      safe_free_str (val1->key);
      val1->key = safe_strdup (val2->key);
    }
  if (val1->enabled != val2->enabled)
    {
      EXPLAIN (val1->name, change, VISIBLE_CHANGE, "enablement change",
	       val1->enabled, val2->enabled);
      change = max (change, VISIBLE_CHANGE);
      val1->enabled = val2->enabled;
    }
  if (val1->call_data != val2->call_data)
    {
      EXPLAIN (val1->name, change, INVISIBLE_CHANGE, "call-data change",
	       val1->call_data, val2->call_data);
      change = max (change, INVISIBLE_CHANGE);
      val1->call_data = val2->call_data;
    }

  merged_contents = merge_widget_value (val1->contents, val2->contents);

  if (val1->contents && !merged_contents)
    {
      EXPLAIN (val1->name, change, INVISIBLE_CHANGE, "(contents gone)",
	       0, 0);
      change = max (change, INVISIBLE_CHANGE);
    }
  else if (merged_contents && merged_contents->change != NO_CHANGE)
    {
      EXPLAIN (val1->name, change, INVISIBLE_CHANGE, "(contents change)",
	       0, 0);
      change = max (change, INVISIBLE_CHANGE);
    }

  val1->contents = merged_contents;

  merged_next = merge_widget_value (val1->next, val2->next);

  if (val1->next && !merged_next)
    {
      EXPLAIN (val1->name, change, STRUCTURAL_CHANGE, "(following gone)",
	       0, 0);
      change = max (change, STRUCTURAL_CHANGE);
    }
  else if (merged_next)
    {
      if (merged_next->change)
	EXPLAIN (val1->name, change, merged_next->change, "(following change)",
		 0, 0);
      change = max (change, merged_next->change);
    }

  val1->next = merged_next;

  val1->change = change;
  
  return val1;
}


/* modifying the widgets */
static Widget
name_to_widget (widget_instance* instance, char* name)
{
  Widget widget = NULL;

  if (!strcmp (XtName (instance->widget), name))
    widget = instance->widget;
  else
    {
      /* Static buffer for prepending '*' to the name passed to
       * XtNameToWidget. */
      static char* real_name = NULL;
      static int real_name_length = 0;
      
      int length = strlen (name);
      if (real_name_length < length)
	{
	  real_name_length = length + 1;
	  real_name = (real_name ? realloc (real_name, real_name_length)
		       : malloc (real_name_length));
	}
      
      real_name [0] = '*';
      strcpy (real_name + 1, name);
      
      widget = XtNameToWidget (instance->widget, real_name);
    }
  return widget;
}

static void
update_one_widget_instance (widget_instance* instance)
{
  widget_value *val;
  Widget widget;

  if (!instance->widget)
    /* the widget was destroyed */
    return;

  for (val = instance->info->val; val; val = val->next)
    if (val->change != NO_CHANGE)
      {
	widget = name_to_widget (instance, val->name);

	if (widget)
	  {
#ifdef USE_MOTIF
	    xm_update_one_widget (instance, widget, val);
#endif
#ifdef USE_LUCID
	    xlw_update_one_widget (instance, widget, val);
#endif
#ifdef USE_OLIT
	    xol_update_one_widget (instance, widget, val);
#endif
	  }
      }
}

static void
update_all_widget_values (widget_info* info)
{
  widget_instance* instance;
  widget_value* val;

  for (instance = info->instances; instance; instance = instance->next)
    update_one_widget_instance (instance);

  for (val = info->val; val; val = val->next)
    val->change = NO_CHANGE;
}

void
lw_modify_all_widgets (BITS32 id, widget_value* val)
{
  widget_info* info = get_widget_info (id, False);

  if (!info)
    return;

  info->val = merge_widget_value (info->val, val);
  update_all_widget_values (info);
}


/* creating the widgets */

static void
initialize_widget_instance (widget_instance* instance)
{
  widget_value* val;

  for (val = instance->info->val; val; val = val->next)
    val->change = STRUCTURAL_CHANGE;

  update_one_widget_instance (instance);

  for (val = instance->info->val; val; val = val->next)
    val->change = NO_CHANGE;
}


static widget_creation_function
find_in_table (char* type, widget_creation_entry* table)
{
  widget_creation_entry* cur;
  for (cur = table; cur->type; cur++)
    if (!strcmp (type, cur->type))
      return cur->function;
  return NULL;
}

static void
instanciate_widget_instance (widget_instance* instance)
{
  widget_creation_function function = NULL;

#ifdef USE_MOTIF
  function = find_in_table (instance->info->type, xm_creation_table);
#else
#ifdef USE_LUCID
  function = find_in_table (instance->info->type, xlw_creation_table);
#else
#ifdef USE_OLIT
  function = find_in_table (instance->info->type, xol_creation_table);
#endif
#endif
#endif

  if (!function)
    abort ();

  instance->widget = (*function) (instance);

  if (!instance->widget)
    abort ();
}

#if 0
  Widget new_shell;
  Widget new_parent;
  widget_function func;

  if (!pop_up_p)
    {
      new_shell = parent;
      new_parent = NULL;
    }
  else
    {
      new_shell = NULL;
      new_parent = parent;
    }
  
  widget = NULL;

  if (dialog_spec_p (info->type))
    widget = make_dialog_box (info->type, new_parent, new_shell);
  else
    {
      func = name_to_function (info->type);
      if (func)
	widget = (*func) (new_parent, new_shell, info->type);
    }

#endif /* !0 */


Widget
lw_create_widget (char* type, char* name, BITS32 id, widget_value* val,
		  Widget parent, Boolean pop_up_p, lw_callback pre_activate_cb,
		  lw_callback selection_cb, lw_callback post_activate_cb)
{
  widget_info* info;
  widget_instance* instance;
  
  info = get_widget_info (id, False);

  if (!info)
    info = allocate_widget_info (type, name, id,
				 copy_widget_value (val, STRUCTURAL_CHANGE,
						    True, True),
				 pre_activate_cb, selection_cb,
				 post_activate_cb);
  else if (val)
    {
      info->val = merge_widget_value (info->val, val);
      update_all_widget_values (info);
    }

  instance = allocate_widget_instance (info, parent, pop_up_p);
  initialize_widget_instance (instance);
  
  return instance->widget;
}
		  

/* destroying the widgets */
static void
destroy_one_instance (widget_instance* instance)
{
  if (instance->widget)
    XtDestroyWidget (instance->widget);
  free (instance);
}

void
lw_destroy_widget (Widget w)
{
  widget_instance* instance = get_widget_instance (w, True);
  
  if (instance)
    {
      destroy_one_instance (instance);

      if (!instance->info->instances)
	lw_destroy_all_widgets (instance->info->id);
    }
}

void
lw_destroy_all_widgets (BITS32 id)
{
  widget_info* info = get_widget_info (id, True);
  widget_instance* instance;
  widget_instance* next;

  if (info)
    {
      for (instance = info->instances; instance; )
	{
	  next = instance->next;
	  destroy_one_instance (instance);
	  instance = next;
	}
      if (info->val)
	free_widget_value (info->val, True, True);
      free (info);
    }
}

void
lw_popup_menu (Widget widget)
{
#ifdef USE_MOTIF
  xm_popup_menu (widget);
#endif
#ifdef USE_LUCID
  xlw_popup_menu (widget);
#endif
#ifdef USE_OLIT
  xol_popup_menu (widget);
#endif
}

/* get the values back */
widget_value*
lw_get_values (Widget w)
{
  widget_instance* instance = get_widget_instance (w, False);
  widget_value* result;
  widget_value* val;
  Widget widget;

  if (!instance)
    return NULL;

  result = instance->info->val;

  for (val = result; val; val = val->next)
    {
      widget = name_to_widget (instance, val->name);
      
      if (widget)
	{
#ifdef USE_MOTIF
	  xm_update_one_value (instance, widget, val);
#endif
#ifdef USE_LUCID
	  xlw_update_one_value (instance, widget, val);
#endif
#ifdef USE_OLIT
	  xol_update_one_value (instance, widget, val);
#endif
	}
    }

  return result;
}


/* get the id */

BITS32
lw_get_widget_id (Widget w)
{
  widget_instance* instance = get_widget_instance (w, False);

  return instance ? instance->info->id : 0;
}

/* set the keyboard focus */
void
lw_set_keyboard_focus (Widget parent, Widget w)
{
#ifdef USE_MOTIF
  xm_set_keyboard_focus (parent, w);
#else
  XtSetKeyboardFocus (parent, w);
#endif
}

/* Show busy */
static void
show_one_widget_busy (Widget w, Boolean flag)
{
  Pixel foreground = 0;
  Pixel background = 1;
  Widget widget_to_invert = XtNameToWidget (w, "*sheet");
  if (!widget_to_invert)
    widget_to_invert = w;
  
  XtVaGetValues (widget_to_invert,
		 XtNforeground, &foreground,
		 XtNbackground, &background,
		 0);
  XtVaSetValues (widget_to_invert,
		 XtNforeground, background,
		 XtNbackground, foreground,
		 0);
}

void
lw_show_busy (Widget w, Boolean busy)
{
  widget_instance* instance = get_widget_instance (w, False);
  widget_info* info;
  widget_instance* next;

  if (instance)
    {
      info = instance->info;
      if (info->busy != busy)
	{
	  for (next = info->instances; next; next = next->next)
	    if (next->widget)
	      show_one_widget_busy (next->widget, busy);
	  info->busy = busy;
	}
    }
}
