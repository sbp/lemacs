#ifndef _EXTW_XLIB_H_
#define _EXTW_XLIB_H_

#define extw_shell_send 0
#define extw_client_send 1

typedef enum {
  extw_notify_init,
  extw_notify_end,
  extw_notify_qg,
  extw_notify_gm,
  extw_notify_set_focus,
  extw_notify_focus_in,
  extw_notify_focus_out
} en_extw_notify;

extern Atom a_EXTW_QUERY_GEOMETRY, a_EXTW_GEOMETRY_MANAGER,
     a_EXTW_WIDGET_GEOMETRY, a_EXTW_NOTIFY;
extern int extw_which_side;

typedef enum {
  EXTW_TYPE_NONE,
  EXTW_TYPE_XLIB,
  EXTW_TYPE_XT,
  EXTW_TYPE_MOTIF
} en_extw_type;

void extw_initialize_atoms(Display *display);
void extw_send_notify_3(Display *display, Window win, en_extw_notify type,
			long data0, long data1, long data2);

#endif /* _EXTW_XLIB_H_ */
