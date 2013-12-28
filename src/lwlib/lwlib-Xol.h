/* $Header: lwlib-Xol.h,v 1.1 92/05/08 14:35:21 devin Exp $ */

#ifndef LWLIB_XOL_H
#define LWLIB_XOL_H

#include "lwlib-internal.h"

extern widget_creation_entry xol_creation_table [];

Boolean
lw_olit_widget_p (Widget widget);

void
xol_update_one_widget (widget_instance* instance, Widget widget,
		       widget_value* val);

void
xm_update_one_value (widget_instance* instance, Widget widget,
		     widget_value* val);

void
xol_popup_menu (Widget widget);

#endif /* LWLIB_XOL_H */
