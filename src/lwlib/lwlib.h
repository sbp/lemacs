#ifndef LWLIB_H
#define LWLIB_H

#include <X11/Intrinsic.h>

/*
** Widget values depend on the Widget type:
** 
** widget:   (name value key enabled data contents/selected)
**
** label:    ("name" "string" NULL NULL NULL NULL)
** button:   ("name" "string" "key" T/F data <default-button-p>)
** button w/menu: 
**           ("name" "string" "key" T/F data (label|button|button w/menu...))
** menubar:  ("name" NULL NULL T/F data (button w/menu))
** selectable thing:
**           ("name" "string" "key" T/F data T/F)
** checkbox: selectable thing
** radio:    ("name" NULL NULL T/F data (selectable thing...))
** strings:  ("name" NULL NULL T/F data (selectable thing...))
** text:     ("name" "string" <ign> T/F data)
*/

typedef unsigned long
BITS32;

typedef enum _change_type
{
  NO_CHANGE = 0,
  INVISIBLE_CHANGE = 1,
  VISIBLE_CHANGE = 2,
  STRUCTURAL_CHANGE = 3
} change_type;

typedef struct _widget_value
{
  /* name of widget */
  char*		name;
  /* value (meaning depend on widget type) */
  char*		value;
  /* keyboard equivalent. no implications for XtTranslations */ 
  char*		key;
  /* true if enabled */
  Boolean	enabled;
  /* true if selected */
  Boolean	selected;
  /* true if was edited (maintained by get_value) */
  Boolean	edited;
  /* true if has changed (maintained by lw library) */
  change_type	change;
  /* Contents of the sub-widgets, also selected slot for checkbox */
  struct _widget_value*	contents;
  /* data passed to callback */
  XtPointer	call_data;
  /* next one in the list */
  struct _widget_value*	next;
  /* slot for the toolkit dependent part.  Always initialize to NULL. */
  void* toolkit_data;
} widget_value;


typedef void
(*lw_callback) (Widget w, BITS32 id, void* data);

void 
lw_register_widget (char* type, char* name, BITS32 id, widget_value* val,
		    lw_callback pre_activate_cb, lw_callback selection_cb,
		    lw_callback post_activate_cb);

Widget
lw_get_widget (BITS32 id, Widget parent, Boolean pop_up_p);

Widget
lw_make_widget (BITS32 id, Widget parent, Boolean pop_up_p);

Widget
lw_create_widget (char* type, char* name, BITS32 id, widget_value* val,
		  Widget parent, Boolean pop_up_p, lw_callback pre_activate_cb,
		  lw_callback selection_cb, lw_callback post_activate_cb);
		  
BITS32
lw_get_widget_id (Widget w);

void
lw_modify_all_widgets (BITS32 id, widget_value* val, Boolean deep_p);

void
lw_destroy_widget (Widget w);

void
lw_destroy_all_widgets (BITS32 id);

void
lw_destroy_everything ();

void
lw_destroy_all_pop_ups ();

Widget
lw_raise_all_pop_up_widgets ();

widget_value*
lw_get_all_values (BITS32 id);

Boolean
lw_get_some_values (BITS32 id, widget_value* val);

void
lw_pop_up_all_widgets (BITS32 id);

void
lw_pop_down_all_widgets (BITS32 id);

/* Toolkit independent way of focusing on a Widget at the Xt level. */
void
lw_set_keyboard_focus (Widget parent, Widget w);

/* Silly Energize hack to invert the "sheet" button */
void
lw_show_busy (Widget w, Boolean busy);

#endif /* LWLIB_H */
