#ifndef _XlwMenuP_h
#define _XlwMenuP_h

#include "xlwmenu.h"
#include <X11/CoreP.h>

/* Elements in the stack arrays. */
typedef struct _window_state
{
  Window	window;
  Position	x;
  Position	y;
  Dimension	width;
  Dimension	height;
  Dimension	label_width;
  Dimension	toggle_width;
} window_state;


/* New fields for the XlwMenu widget instance record */
typedef struct _XlwMenu_part 
{
  /* slots set by the resources */

#ifdef USE_MOTIF
  XmFontList	font_list;
  XmFontList	font_list_2;
  XmFontList	fallback_font_list;
#else
  XFontStruct *	font;
#endif
  Dimension	font_ascent, font_descent;  /* extracted from font/fontlist */

  Pixel		foreground;
  Pixel		button_foreground;
  Dimension	margin;
  Dimension	horizontal_margin;
  Dimension	vertical_margin;
  Dimension	column_spacing;
  Dimension	shadow_thickness;
  Dimension	indicator_size;
  Pixel 	top_shadow_color;
  Pixel 	bottom_shadow_color;
  Pixel 	select_color;
  Pixmap	top_shadow_pixmap;
  Pixmap	bottom_shadow_pixmap;
  Cursor	cursor_shape;
  XtCallbackList	open;
  XtCallbackList	select;
  widget_value*	contents;
  int		horizontal;
  Boolean	use_backing_store;

  /* State of the XlwMenu */
  int			old_depth;
  widget_value**	old_stack;
  int			old_stack_length;

  /* New state after the user moved */
  int			new_depth;
  widget_value**	new_stack;
  int			new_stack_length;

  /* Window resources */
  window_state*		windows;
  int			windows_length;

  /* Internal part, set by the XlwMenu */
  GC			foreground_gc;
  GC			button_gc;
  GC			background_gc;
  GC			inactive_gc;
  GC			inactive_button_gc;
  GC			shadow_top_gc;
  GC			shadow_bottom_gc;
  GC			select_gc;
  Cursor		cursor;
  Boolean		popped_up;
  Pixmap		gray_pixmap;
} XlwMenuPart;

/* Full instance record declaration */
typedef struct _XlwMenuRec 
{
  CorePart	core;
  XlwMenuPart	menu;
} XlwMenuRec;

/* New fields for the XlwMenu widget class record */
typedef struct 
{
  int	dummy;
} XlwMenuClassPart;

/* Full class record declaration. */
typedef struct _XlwMenuClassRec 
{
  CoreClassPart		core_class;
  XlwMenuClassPart	menu_class;
} XlwMenuClassRec;

/* Class pointer. */
extern XlwMenuClassRec xlwMenuClassRec;

#endif /* _XlwMenuP_h */
