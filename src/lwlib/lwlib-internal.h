/* $Header: lwlib-internal.h,v 100.2 92/04/30 11:14:17 devin Exp $ */
#ifndef LWLIB_INTERNAL_H
#define LWLIB_INTERNAL_H

#include "lwlib.h"

typedef struct _widget_instance
{
  Widget		widget;
  Widget		parent;
  Boolean		pop_up_p;
  struct _widget_info*		info;
  struct _widget_instance*	next;
} widget_instance;

typedef struct _widget_info
{
  char*			type;
  char*			name;
  BITS32		id;
  widget_value*		val;
  Boolean		busy;
  lw_callback		pre_activate_cb;
  lw_callback		selection_cb;
  lw_callback		post_activate_cb;
  struct _widget_instance*	instances;
  struct _widget_info*		next;
} widget_info;

typedef Widget
(*widget_creation_function) (widget_instance* instance);

typedef struct _widget_creation_entry
{
  char*				type;
  widget_creation_function	function;
} widget_creation_entry;

#ifdef USE_ATHENA
Boolean lw_athena_widget_p (Widget widget);
#endif

#endif /* LWLIB_INTERNAL_H */
