
#ifndef _EmacsShellWidgetP_h
#define _EmacsShellWidgetP_h

#include "xintrinsic.h"
#include <X11/ShellP.h>
#include "EmacsShell.h"

typedef struct {			/* new fields for EmacsShell class */
   int dummy;
} EmacsShellClassPart;

typedef struct _EmacsShellClassRec {	/* full class record declaration */
    CoreClassPart core_class;
    CompositeClassPart composite_class;
    ShellClassPart shell_class;
    EmacsShellClassPart emacsShell_class;
} EmacsShellClassRec;

typedef struct {			/* new fields for EmacsShell widget */
    Window external_window;	/* an already-created window to run on */
    Bool dead_client;		/* is the client dead? */
    unsigned long client_timeout;/* how long to wait for client's response */
} EmacsShellPart;

typedef struct _EmacsShellRec {	/* full instance record */
    CorePart core;
    CompositePart composite;
    ShellPart shell;
    EmacsShellPart emacsShell;
} EmacsShellRec;

extern EmacsShellClassRec emacsShellClassRec;	 /* class pointer */

#endif /* _EmacsShellWidgetP_h */
