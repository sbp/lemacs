#ifndef _ExternalShell_h
#define _ExternalShell_h

#ifndef XtNwindow
#define XtNwindow "window"
#endif
#ifndef XtCWindow
#define XtCWindow "Window"
#endif

#ifndef XtNclientTimeout
#define XtNclientTimeout "clientTimeout"
#endif
#ifndef XtCClientTimeout
#define XtCClientTimeout "ClientTimeout"
#endif

#ifndef XtNdeadClient
#define XtNdeadClient "deadClient"
#endif
#ifndef XtCDeadClient
#define XtCDeadClient "DeadClient"
#endif

typedef struct _ExternalShellClassRec *ExternalShellWidgetClass;
typedef struct _ExternalShellRec *ExternalShellWidget;
extern WidgetClass externalShellWidgetClass;

Bool ExternalShellReady(Widget w, Window win, long event_mask);
void ExternalShellSetFocus(Widget w);
void ExternalShellUnrealize(Widget w);

#define is_external_shell(w) (XtClass (w) == externalShellWidgetClass)

#endif /* _ExternalShell_h */
