/* ColumnWidget -- a simple Composite widget that manages its children in
 * a column.
 */

#ifndef _ColumnWidgetP_h
#define _ColumnWidgetP_h

#include "ColumnWidget.h"


typedef struct {
   int dummy;
} ColumnPart;

typedef struct _ColumnRec {
    CorePart		core;
    CompositePart	composite;
    ColumnPart		column;
} ColumnRec;

typedef struct {
   int dummy;
} ColumnClassPart;

typedef struct _ColumnClassRec {	/* full instance record */
    CoreClassPart	core_class;
    CompositeClassPart	composite_class;
    ColumnClassPart	column_class;
} ColumnClassRec;

extern ColumnClassRec columnClassRec;

#endif /* _ColumnWidgetP_h */
