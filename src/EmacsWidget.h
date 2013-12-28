
#ifndef _EmacsClientWidget_h
#define _EmacsClientWidget_h

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

typedef struct _EmacsClientClassRec *EmacsClientWidgetClass;
typedef struct _EmacsClientRec *EmacsClientWidget;
extern WidgetClass emacsClientWidgetClass;

#endif /* _EmacsClientWidget_h */
