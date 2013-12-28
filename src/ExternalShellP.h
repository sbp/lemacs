#ifndef _ExternalShellP_h
#define _ExternalShellP_h

#include "xintrinsic.h"
#include <X11/ShellP.h>
#include "ExternalShell.h"

typedef struct {		/* new fields for ExternalShell class */
   int dummy;
} ExternalShellClassPart;

typedef struct _ExternalShellClassRec {	/* full class record declaration */
    CoreClassPart core_class;
    CompositeClassPart composite_class;
    ShellClassPart shell_class;
    ExternalShellClassPart externalShell_class;
} ExternalShellClassRec;

typedef struct {		/* new fields for ExternalShell widget */
    Window external_window;	/* an already-created window to run on */
    Bool dead_client;		/* is the client dead? */
    unsigned long client_timeout;/* how long to wait for client's response */

    /* private */
    unsigned char client_type;
} ExternalShellPart;

typedef struct _ExternalShellRec {	/* full instance record */
    CorePart core;
    CompositePart composite;
    ShellPart shell;
    ExternalShellPart externalShell;
} ExternalShellRec;

extern ExternalShellClassRec externalShellClassRec;	 /* class pointer */

#endif /* _ExternalShellP_h */
