
#ifndef _EmacsClientWidgetP_h
#define _EmacsClientWidgetP_h

#include "EmacsWidget.h"
#include <Xm/PrimitiveP.h>

typedef struct {			/* new fields for EmacsClient class */
   int dummy;
} EmacsClientClassPart;

typedef struct _EmacsClientClassRec {	/* full class record declaration */
    CoreClassPart core_class;
    XmPrimitiveClassPart primitive_class;
    EmacsClientClassPart emacsClient_class;
} EmacsClientClassRec;

typedef struct {			/* new fields for EmacsClient widget */
    Bool dead_shell;		/* is the shell dead? */
    unsigned long shell_timeout;/* how long to wait for shell's response */
    int shell_ready;		/* is the shell ready? */
    Window event_window;
    long event_mask;
    Bool has_focus;
    char *emacs_procid;         
    XtCallbackList shell_ready_callback;
    String shell_name;
    Bool use_tooltalk;
} EmacsClientPart;

typedef struct _EmacsClientRec {	/* full instance record */
    CorePart core;
    XmPrimitivePart primitive;
    EmacsClientPart emacsClient;
} EmacsClientRec;

extern EmacsClientClassRec emacsClientClassRec;	 /* class pointer */

#endif /* _EmacsClientWidgetP_h */
