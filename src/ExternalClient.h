#ifndef _ExternalClient_h
#define _ExternalClient_h

#ifndef XtNshellTimeout
#define XtNshellTimeout "shellTimeout"
#endif
#ifndef XtCShellTimeout
#define XtCShellTimeout "ShellTimeout"
#endif

#ifndef XtNdeadShell
#define XtNdeadShell "deadShell"
#endif
#ifndef XtCDeadShell
#define XtCDeadShell "DeadShell"
#endif

#ifndef XtNemacsProcID
#define XtNemacsProcID "emacsProcID"
#endif
#ifndef XtCEmacsProcID
#define XtCEmacsProcID "EmacsProcID"
#endif

#ifndef XtNshellReadyCallback
#define XtNshellReadyCallback "shellReadyCallback"
#endif

#ifndef XtNshellName
#define XtNshellName "shellName"
#endif
#ifndef XtCShellName
#define XtCShellName "ShellName"
#endif

#ifndef XtNuseToolTalk
#define XtNuseToolTalk "useToolTalk"
#endif
#ifndef XtCUseToolTalk
#define XtCUseToolTalk "UseToolTalk"
#endif

typedef struct _ExternalClientClassRec *ExternalClientWidgetClass;
typedef struct _ExternalClientRec *ExternalClientWidget;
extern WidgetClass externalClientWidgetClass;

/* External entry points when using direct Xlib */

void ExternalClientInitialize (Display *display);
void ExternalClientEventHandler (Display *display, Window win, XEvent *event);

#endif /* _ExternalClient_h */
