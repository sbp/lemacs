
#ifndef _EmacsShellWidget_h
#define _EmacsShellWidget_h

#ifndef XtNwindow
#define XtNwindow "window"
#endif
#ifndef XtCWindow
#define XtCWindow "Window"
#endif

typedef struct _EmacsShellClassRec *EmacsShellWidgetClass;
typedef struct _EmacsShellRec *EmacsShellWidget;
extern WidgetClass emacsShellWidgetClass;

#endif /* _EmacsShellWidget_h */
