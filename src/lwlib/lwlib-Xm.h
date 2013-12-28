/* $Header: lwlib-Xm.h,v 100.4 92/05/04 15:04:27 devin Exp $ */

#ifndef LWLIB_XM_H
#define LWLIB_XM_H

#include "lwlib-internal.h"

extern widget_creation_entry xm_creation_table [];

Boolean
lw_motif_widget_p (Widget widget);

void
xm_update_one_widget (widget_instance* instance, Widget widget,
		      widget_value* val);

void
xm_update_one_value (widget_instance* instance, Widget widget,
		     widget_value* val);

void
xm_set_keyboard_focus (Widget parent, Widget w);

void
xm_popup_menu (Widget widget);

#endif /* LWLIB_XM_H */
