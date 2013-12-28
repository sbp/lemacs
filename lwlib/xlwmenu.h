#ifndef _XlwMenu_h
#define _XlwMenu_h

/***********************************************************************
 *
 * XlwMenu Widget
 *
 ***********************************************************************/

#include "lwlib.h"

/* Resource names used by the XlwMenu widget */
#define XtNbuttonForeground "buttonForeground"
#define XtCButtonForeground "ButtonForeground"
#define XtNmargin "margin"
#define XtNhorizontalSpacing "horizontalSpacing"
#define XtNverticalSpacing "verticalSpacing"
#define XtNarrowSpacing "arrowSpacing"
#define XtNmenu "menu"
#define XtCMenu "Menu"
#define XtNopen "open"
#define XtNselect "select"
#define XtNmenuBorderWidth "menuBorderWidth"
#define XtNhorizontal "horizontal"
#define XtCHorizontal "Horizontal"
#ifndef XtNcursor
#define XtNcursor "cursor"
#endif
#ifndef XtCCursor
#define XtCCursor "Cursor"
#endif
#ifndef XtNuseBackingStore
#define XtNuseBackingStore "useBackingStore"
#endif
#ifndef XtCUseBackingStore
#define XtCUseBackingStore "UseBackingStore"
#endif

/* Motif-compatible resource names */
#ifndef XmNshadowThickness
# define XmNshadowThickness	"shadowThickness"
# define XmCShadowThickness	"ShadowThickness"
# define XmNtopShadowColor	"topShadowColor"
# define XmCTopShadowColor	"TopShadowColor"
# define XmNbottomShadowColor	"bottomShadowColor"
# define XmCBottomShadowColor	"BottomShadowColor"
# define XmNtopShadowPixmap	"topShadowPixmap"
# define XmCTopShadowPixmap	"TopShadowPixmap"
# define XmNbottomShadowPixmap	"bottomShadowPixmap"
# define XmCBottomShadowPixmap	"BottomShadowPixmap"
# define XmRHorizontalDimension	"HorizontalDimension"
# define XmNspacing		"spacing"
# define XmCSpacing		"Spacing"
# define XmNindicatorSize	"indicatorSize"
# define XmCIndicatorSize	"IndicatorSize"
# define XmNselectColor		"selectColor"
# define XmCSelectColor		"SelectColor"
# define XmNmarginHeight	"marginHeight"
# define XmCMarginHeight	"MarginHeight"
# define XmNmarginWidth		"marginWidth"
# define XmCMarginWidth		"MarginWidth"
# define XmRVerticalDimension	"VerticalDimension"
#endif

typedef struct _XlwMenuRec *XlwMenuWidget;
typedef struct _XlwMenuClassRec *XlwMenuWidgetClass;

extern WidgetClass xlwMenuWidgetClass;

void
pop_up_menu (XlwMenuWidget mw, XButtonPressedEvent* event);

#endif /* _XlwMenu_h */
