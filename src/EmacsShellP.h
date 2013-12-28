
#ifndef _EmacsShellWidgetP_h
#define _EmacsShellWidgetP_h

#include <X11/Intrinsic.h>
#include <X11/ShellP.h>
#include "EmacsShell.h"

typedef struct {			/* new fields for EmacsShell class */
   int dummy;
} EmacsShellClassPart;

typedef struct _EmacsShellClassRec {	/* full class record declaration */
    CoreClassPart core_class;
    CompositeClassPart composite_class;
    ShellClassPart shell_class;
    WMShellClassPart   wm_shell_class;
    VendorShellClassPart vendor_shell_class;
    TopLevelShellClassPart top_level_shell_class;
    ApplicationShellClassPart application_shell_class;
    EmacsShellClassPart emacsShell_class;
} EmacsShellClassRec;

typedef struct {			/* new fields for EmacsShell widget */
    Window external_window;	/* an already-created window to run on */
} EmacsShellPart;

typedef struct _EmacsShellRec {	/* full instance record */
    CorePart core;
    CompositePart composite;
    ShellPart shell;
    WMShellPart	wm;
    VendorShellPart	vendor;
    TopLevelShellPart topLevel;
    ApplicationShellPart application;
    EmacsShellPart emacsShell;
} EmacsShellRec;

extern EmacsShellClassRec emacsShellClassRec;	 /* class pointer */

#endif /* _EmacsShellWidgetP_h */
