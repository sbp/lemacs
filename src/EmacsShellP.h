#ifndef _EmacsShellP_h
#define _EmacsShellP_h

#include "xintrinsic.h"
#include <X11/ShellP.h>
#include "EmacsShell.h"

typedef struct {		/* new fields for EmacsShell class */
  int dummy;
} EmacsShellClassPart;

typedef struct _EmacsShellClassRec {	/* full class record declaration */
  CoreClassPart core_class;
  CompositeClassPart composite_class;
  ShellClassPart shell_class;
  WMShellClassPart wm_shell_class;
  VendorShellClassPart vendor_shell_class;
  TopLevelShellClassPart top_level_shell_class;
  EmacsShellClassPart emacs_shell_class;
} EmacsShellClassRec;

typedef struct {		/* new fields for EmacsShell widget */
  int width_cells, height_cells;
  int min_width_cells, min_height_cells;
} EmacsShellPart;

typedef struct _EmacsShellRec {	/* full instance record */
    CorePart core;
    CompositePart composite;
    ShellPart shell;
    WMShellPart wm;
    VendorShellPart vendor;
    TopLevelShellPart top_level;
    EmacsShellPart emacs_shell;
} EmacsShellRec;

extern EmacsShellClassRec emacsShellClassRec;	 /* class pointer */

#endif /* _EmacsShellP_h */
