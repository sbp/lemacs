#ifndef _ExternalClientP_h
#define _ExternalClientP_h

#include "ExternalClient.h"
#ifdef EXTW_USES_MOTIF
#include <Xm/PrimitiveP.h>
#endif

typedef struct {		/* new fields for ExternalClient class */
   int dummy;
} ExternalClientClassPart;

typedef struct _ExternalClientClassRec {	/* full class record declaration */
    CoreClassPart core_class;
#ifdef EXTW_USES_MOTIF
    XmPrimitiveClassPart primitive_class;
#endif
    ExternalClientClassPart externalClient_class;
} ExternalClientClassRec;

typedef struct {		/* new fields for ExternalClient widget */
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
} ExternalClientPart;

typedef struct _ExternalClientRec {	/* full instance record */
    CorePart core;
#ifdef EXTW_USES_MOTIF
    XmPrimitivePart primitive;
#endif
    ExternalClientPart externalClient;
} ExternalClientRec;

extern ExternalClientClassRec externalClientClassRec;	 /* class pointer */

#endif /* _ExternalClientP_h */
