
#ifndef _EmacsShellWidget_h
#define _EmacsShellWidget_h

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

typedef struct _EmacsShellClassRec *EmacsShellWidgetClass;
typedef struct _EmacsShellRec *EmacsShellWidget;
extern WidgetClass emacsShellWidgetClass;

void EmacsShellReady(Widget w, Window win, long event_mask);
#if 0
void EmacsShellSuppressEvents(Widget w, long event_mask);
#endif
void EmacsShellSetFocus(Widget w);

#define is_emacs_shell(w) (XtClass (w) == emacsShellWidgetClass)

#endif /* _EmacsShellWidget_h */
