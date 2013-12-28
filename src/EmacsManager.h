#ifndef _EmacsManager_h
#define _EmacsManager_h

#ifndef XtNtextArea
#define XtNtextArea "textArea"
#endif
#ifndef XtCTextArea
#define XtCTextArea "TextArea"
#endif

#ifndef XtNtopAreaWidgets
#define XtNtopAreaWidgets "topAreaWidgets"
#endif
#ifndef XtCTopAreaWidgets
#define XtCTopAreaWidgets "TopAreaWidgets"
#endif

#ifndef XtNnumTopAreaWidgets
#define XtNnumTopAreaWidgets "numTopAreaWidgets"
#endif
#ifndef XtCNumTopAreaWidgets
#define XtCNumTopAreaWidgets "NumTopAreaWidgets"
#endif

#ifndef XtNscrollbarWidget
#define XtNscrollbarWidget "scrollbarWidget"
#endif
#ifndef XtCScrollbarWidget
#define XtCScrollbarWidget "ScrollbarWidget"
#endif

#ifndef XtNscrollBarPlacement
#define XtNscrollBarPlacement "scrollBarPlacement"
#endif
#ifndef XtCScrollBarPlacement
#define XtCScrollBarPlacement "ScrollBarPlacement"
#endif
#ifndef XtRScrollBarPlacement
#define XtRScrollBarPlacement "ScrollBarPlacement"
#endif

#ifndef XtNpreserveSameSize
#define XtNpreserveSameSize "preserveSameSize"
#endif
#ifndef XtCPreserveSameSize
#define XtCPreserveSameSize "PreserveSameSize"
#endif

#ifndef XtNrefigureMode
#define XtNrefigureMode "refigureMode"
#endif
#ifndef XtCRefigureMode
#define XtCRefigureMode "RefigureMode"
#endif

/* scrollbar placement types; like in ScrolledW.h */

#define EM_TOP          1
#define EM_BOTTOM       0
#define EM_LEFT         2
#define EM_RIGHT        0
 
#define XtTOP_LEFT      (EM_TOP | EM_LEFT)
#define XtBOTTOM_LEFT   (EM_BOTTOM  | EM_LEFT)
#define XtTOP_RIGHT     (EM_TOP | EM_RIGHT)
#define XtBOTTOM_RIGHT  (EM_BOTTOM  | EM_RIGHT)
 
typedef struct _EmacsManagerClassRec *EmacsManagerWidgetClass;
typedef struct _EmacsManagerRec *EmacsManagerWidget;
extern WidgetClass emacsManagerWidgetClass;

/* External entry points */
void EmacsManagerForceRefigure (Widget w);

#endif /* _EmacsManager_h */
