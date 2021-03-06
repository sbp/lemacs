#ifndef LWLIB_H
#define LWLIB_H

#include <X11/Intrinsic.h>

/* To eliminate use of `const' in the lwlib sources, define CONST_IS_LOSING. */
#undef CONST
#ifdef CONST_IS_LOSING
# define CONST
#else
# define CONST const
#endif

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
** scrollbar:("name" NULL NULL T/F NULL NULL)
** selectable thing:
**           ("name" "string" "key" T/F data T/F)
** checkbox: selectable thing
** radio:    ("name" NULL NULL T/F data (selectable thing...))
** strings:  ("name" NULL NULL T/F data (selectable thing...))
** text:     ("name" "string" <ign> T/F data)
**
** Note that the above is EXTREMELY bogus.  The "type" of the various entities
** that a widget_value structure can represent is implicit in the contents of
** half a dozen slots, instead of there simply being a type field.  This 
** should all be rethunk.  I've added a type field, but for now it's only used
** by the new xlwmenu radiobutton/togglebutton code.
*/

typedef unsigned long LWLIB_ID;

typedef enum _change_type
{
  NO_CHANGE = 0,
  INVISIBLE_CHANGE = 1,
  VISIBLE_CHANGE = 2,
  STRUCTURAL_CHANGE = 3
} change_type;

typedef enum _widget_value_type
{
  UNSPECIFIED_TYPE = 0,
  BUTTON_TYPE = 1,
  TOGGLE_TYPE = 2,
  RADIO_TYPE = 3,
  TEXT_TYPE = 4,
  SEPARATOR_TYPE = 5,
  CASCADE_TYPE = 6,
  PUSHRIGHT_TYPE = 7
    /* ... add (and use!) the rest ... */
} widget_value_type;

typedef enum _scroll_action
{
  SCROLLBAR_LINE_UP = 0,
  SCROLLBAR_LINE_DOWN = 1,
  SCROLLBAR_PAGE_UP = 2,
  SCROLLBAR_PAGE_DOWN = 3,
  SCROLLBAR_DRAG = 4,
  SCROLLBAR_CHANGE = 5,
  SCROLLBAR_TOP = 6,
  SCROLLBAR_BOTTOM = 7
} scroll_action;

typedef struct _scroll_event
{
  scroll_action action;
  int slider_value;
  Time time;
} scroll_event;

typedef struct _scrollbar_values
{
  int line_increment;
  int page_increment;

  int minimum;
  int maximum;

  int slider_size;
  int slider_position;

  int scrollbar_height;
  int scrollbar_pos;
} scrollbar_values;

typedef struct _widget_value
{
  /* This slot is only partially utilized right now. */
  widget_value_type type;

  /* name of widget */
  char*		name;
  /* value (meaning BOGUSLY depend on widget type) */
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
  /* tell us if we should free the toolkit data slot when freeing the
     widget_value itself. */
  Boolean free_toolkit_data;

  /* data defining a scrollbar; only valid if type == "scrollbar" */
  scrollbar_values *scrollbar_data;

  /* we resource the widget_value structures; this points to the next
     one on the free list if this one has been deallocated.
   */
  struct _widget_value *free_list;
} widget_value;


typedef void (*lw_callback) (Widget w, LWLIB_ID id, XtPointer data);

void  lw_register_widget (CONST char* type, CONST char* name, LWLIB_ID id,
			  widget_value* val, lw_callback pre_activate_cb,
			  lw_callback selection_cb,
			  lw_callback post_activate_cb);
Widget lw_get_widget (LWLIB_ID id, Widget parent, Boolean pop_up_p);
Widget lw_make_widget (LWLIB_ID id, Widget parent, Boolean pop_up_p);
Widget lw_create_widget (CONST char* type, CONST char* name, LWLIB_ID id,
			 widget_value* val, Widget parent, Boolean pop_up_p,
			 lw_callback pre_activate_cb,
			 lw_callback selection_cb,
			 lw_callback post_activate_cb);
LWLIB_ID lw_get_widget_id (Widget w);
void lw_modify_all_widgets (LWLIB_ID id, widget_value* val, Boolean deep_p);
void lw_destroy_widget (Widget w);
void lw_destroy_all_widgets (LWLIB_ID id);
void lw_destroy_everything (void);
void lw_destroy_all_pop_ups (void);
Widget lw_raise_all_pop_up_widgets (void);
widget_value* lw_get_all_values (LWLIB_ID id);
Boolean lw_get_some_values (LWLIB_ID id, widget_value* val);
void lw_pop_up_all_widgets (LWLIB_ID id);
void lw_pop_down_all_widgets (LWLIB_ID id);
widget_value *malloc_widget_value ();
void free_widget_value (widget_value *);
void lw_popup_menu (Widget);

/* Toolkit independent way of focusing on a Widget at the Xt level. */
void lw_set_keyboard_focus (Widget parent, Widget w);

/* Silly Energize hack to invert the "sheet" button */
void lw_show_busy (Widget w, Boolean busy);

#endif /* LWLIB_H */
