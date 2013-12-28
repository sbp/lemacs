/* $Header: lwlib-Xlw.h,v 1.2 92/05/04 15:04:14 devin Exp $ */

#ifndef LWLIB_XLW_H
#define LWLIB_XLW_H

#include "lwlib-internal.h"

extern widget_creation_entry xlw_creation_table [];

Boolean
lw_lucid_widget_p (Widget widget);

void
xlw_update_one_widget (widget_instance* instance, Widget widget,
		       widget_value* val);

void
xm_update_one_value (widget_instance* instance, Widget widget,
		     widget_value* val);

void
xlw_popup_menu (Widget widget);

#endif /* LWLIB_XLW_H */
