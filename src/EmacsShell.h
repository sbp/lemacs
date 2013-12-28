#ifndef _EmacsShell_h
#define _EmacsShell_h

#ifndef XtNwidthCells
#define XtNwidthCells "widthCells"
#endif
#ifndef XtCWidthCells
#define XtCWidthCells "WidthCells"
#endif

#ifndef XtNheightCells
#define XtNheightCells "heightCells"
#endif
#ifndef XtCHeightCells
#define XtCHeightCells "HeightCells"
#endif

#ifndef XtNminWidthCells
#define XtNminWidthCells "minWidthCells"
#endif
#ifndef XtCMinWidthCells
#define XtCMinWidthCells "MinWidthCells"
#endif

#ifndef XtNminHeightCells
#define XtNminHeightCells "minHeightCells"
#endif
#ifndef XtCMinHeightCells
#define XtCMinHeightCells "MinHeightCells"
#endif

typedef struct _EmacsShellClassRec *EmacsShellWidgetClass;
typedef struct _EmacsShellRec *EmacsShellWidget;
extern WidgetClass emacsShellWidgetClass;

void EmacsShellUpdateSizeHints (Widget gw);
void EmacsShellSetSizeUserSpecified (Widget gw);
void EmacsShellSetPositionUserSpecified (Widget gw);
void EmacsShellSmashIconicHint (Widget shell, int iconic_p);

#endif /* _EmacsShell_h */
