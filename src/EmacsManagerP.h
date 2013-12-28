#ifndef _EmacsManagerP_h
#define _EmacsManagerP_h


#include "xintrinsicp.h"
#ifdef LWLIB_USES_MOTIF
#include "xmmanagerp.h"
#endif
#include "EmacsManager.h"

typedef struct {		/* new fields for EmacsManager class */
  int dummy;
} EmacsManagerClassPart;

typedef struct _EmacsManagerClassRec {	/* full class record declaration */
  CoreClassPart core_class;
  CompositeClassPart composite_class;
#ifdef LWLIB_USES_MOTIF
  ConstraintClassPart constraint_class;
  XmManagerClassPart manager_class;
#endif
  EmacsManagerClassPart emacs_manager_class;
} EmacsManagerClassRec;

typedef struct {		/* new fields for EmacsManager widget */
  Widget text_area;
  WidgetList top_area_widgets;
  Cardinal num_top_area_widgets;
  Widget scrollbar_widget;
  unsigned char scrollbar_placement;
  Boolean preserve_same_size;
  Boolean refigure_mode;
} EmacsManagerPart;

typedef struct _EmacsManagerRec {	/* full instance record */
    CorePart core;
    CompositePart composite;
#ifdef LWLIB_USES_MOTIF
    ConstraintPart constraint;
    XmManagerPart manager;
#endif
    EmacsManagerPart emacs_manager;
} EmacsManagerRec;

extern EmacsManagerClassRec emacsManagerClassRec;	 /* class pointer */

#endif /* _EmacsManagerP_h */
