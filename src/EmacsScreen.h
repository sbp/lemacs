#ifndef _EmacsScreen_h
#define _EmacsScreen_h

#define XtNminibuffer "minibuffer"
#define XtCMinibuffer "Minibuffer"
#define XtNunsplittable "unsplittable"
#define XtCUnsplittable "Unsplittable"
#define XtNinternalBorderWidth "internalBorderWidth"
#define XtCInternalBorderWidth "InternalBorderWidth"
#define XtNinterline "interline"
#define XtCInterline "Interline"

#ifndef XtNfont
#define XtNfont "font"
#endif
#ifndef XtCFont
#define XtCFont "Font"
#endif
#ifndef XtNforeground
#define XtNforeground "foreground"
#endif
#ifndef XtCForeground
#define XtCForeground "Foreground"
#endif

#define XtNcursorColor "cursorColor"
#define XtCCursorColor "CursorColor"
#define XtNbarCursor "barCursor"
#define XtCBarCursor "BarCursor"

#define XtNvisualBell "visualBell"
#define XtCVisualBell "VisualBell"
#define XtCBellVolume "BellVolume"
#define XtNbellVolume "bellVolume"

#define XtNpointerBackground "pointerBackground"
#define XtNpointerColor "pointerColor"

#define XtNtextPointer "textPointer"
#define XtNspacePointer "spacePointer"
#define XtNmodeLinePointer "modePointer"
#define XtNgcPointer "gcPointer"

#define XtNemacsScreen "emacsScreen"
#define XtCEmacsScreen "EmacsScreen"

#ifndef XtNgeometry
#define XtNgeometry "geometry"
#endif
#ifndef XtCGeometry
#define XtCGeometry "Geometry"
#endif

#define XtNinitialGeometry "initialGeometry"
#define XtCInitialGeometry "InitialGeometry"

#define XtNmenubar "menubar"
#define XtCMenubar "menubar"

/* structures
 */
typedef struct _EmacsScreenRec *EmacsScreen;
typedef struct _EmacsScreenClassRec *EmacsScreenClass;

extern WidgetClass emacsScreenClass;

extern struct _DisplayContext* display_context;

/* Special entrypoints */
void EmacsScreenSetCharSize (Widget widget, int rows, int cols);

#endif /* _EmacsScreen_h */
