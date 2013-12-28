/* ColumnWidget -- a simple Composite widget that manages its children in
 * a column.
 */

#include <stdio.h>
#include <X11/StringDefs.h>
#include <X11/IntrinsicP.h>
#include <X11/cursorfont.h>
#include "ColumnWidgetP.h"

#undef MAX
#undef MIN
#define MAX(a,b) ((a)>(b)?(a):(b))
#define MIN(a,b) ((a)<(b)?(a):(b))

static void Initialize();
static void Resize();
static void ChangeManaged();
static XtGeometryResult GeometryManager();
static XtGeometryResult PreferredSize();
static XtGeometryResult try_layout();

ColumnClassRec columnClassRec = {
    { /*
       *	core_class fields
       */
    /* superclass		*/	(WidgetClass)&compositeClassRec,
    /* class_name		*/	"Column",
    /* widget_size		*/	sizeof(ColumnRec),
    /* class_initialize		*/	NULL,
    /* class_part_initialize	*/	NULL,
    /* class_inited		*/	FALSE,
    /* initialize		*/	Initialize,
    /* initialize_hook		*/	NULL,
    /* realize			*/	XtInheritRealize,
    /* actions			*/	NULL,
    /* num_actions		*/	0,
    /* resources		*/	NULL,
    /* resource_count		*/	0,
    /* xrm_class		*/	NULLQUARK,
    /* compress_motion		*/	TRUE,
    /* compress_exposure	*/	TRUE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest		*/	FALSE,
    /* destroy			*/	NULL,
    /* resize			*/	Resize,
    /* expose			*/	NULL,
    /* set_values		*/	NULL,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	XtInheritSetValuesAlmost,
    /* get_values_hook		*/	NULL,
    /* accept_focus		*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* tm_table			*/	NULL,
    /* query_geometry		*/	PreferredSize,
    /* display_accelerator	*/	NULL,
    /* extension		*/	NULL
    },
    { /*
       *	composite_class fields
       */
    /* geometry_manager		*/	GeometryManager,
    /* change_managed		*/	ChangeManaged,
    /* insert_child		*/	XtInheritInsertChild,
    /* delete_child		*/	XtInheritDeleteChild,
    /* extension		*/	NULL
    }
};

WidgetClass columnWidgetClass = (WidgetClass) &columnClassRec;

static void Initialize (request, new)
     ColumnWidget request, new;
{
    if (request->core.width  <= 0) new->core.width  = 5;
    if (request->core.height <= 0) new->core.height = 5;
}

static void do_layout (ColumnWidget);

static void Resize (w)
     ColumnWidget w;
{
    printf("Resize\n");fflush(stdout);
    do_layout (w);
}

static void
do_layout (parent)
     ColumnWidget parent;
{
    Widget child;
    int i;
    Dimension childheight = 0;
    Position ypos = 0;
    Dimension pad = 0;
    int n_managed_children = 0;
    
    /* compute total and height of all managed children
     */
    for (i=0; i < parent->composite.num_children; i++) {
       child = parent->composite.children[i];
       if (child->core.managed) {
	  n_managed_children++;
	  childheight += child->core.height + (child->core.border_width * 2);
       }
    }

    printf("kids=%d, total-h=%d\n", n_managed_children, childheight);
    fflush(stdout);

    /* divide remaining space by number of children
     */
    if ((n_managed_children > 1) && (parent->core.height > childheight))
      pad = (Dimension) (parent->core.height - childheight) /
	(Dimension) (n_managed_children - 1);

    printf("pad = %d\n", pad);

    /* move things around
     */
    for (i = 0; i < parent->composite.num_children; i++) {
       child = parent->composite.children[i];
       if (child->core.managed) {
	  printf("moving kid %d to 0, %d + %d\n", i, ypos, child->core.height);
	  fflush(stdout);
	  XtMoveWidget (child, 0, ypos);
	  ypos += pad + child->core.height + (child->core.border_width * 2);
      }
   }
}

static XtGeometryResult PreferredSize (w, request, preferred)
     ColumnWidget w;
     XtWidgetGeometry *request, *preferred;
{
    Widget child;
    int i;
    
    printf("PreferredSize\n");fflush(stdout);

    /* if no changes to w or h, just agree */
    if (! (request->request_mode & CWWidth) &&
	! (request->request_mode & CWHeight))
      return XtGeometryYes;

    /* calculate min size */
    preferred->width = 0;
    preferred->height = 0;
    for (i=0; i<w->composite.num_children; i++) {
       child = w->composite.children[i];
       if (child->core.managed) {
	  preferred->width += child->core.width + 2*child->core.border_width;
	  preferred->height += child->core.height + 2*child->core.border_width;
       }
    }
    preferred->request_mode = CWWidth|CWHeight;

    /* if both w and h are requested... */
    if ((request->request_mode & CWWidth) &&
	(request->request_mode & CWHeight)) {
       /* if we are to be the same or bigger, say ok */
       if ((preferred->width >= request->width) &&
	   (preferred->height >= request->height)) {
	  preferred->width = request->width;
	  preferred->height = request->height;
	  return XtGeometryYes;
       }
       /* if both dimensions unacceptable, say no */
       else
         if ((preferred->width < request->width) &&
	     (preferred->height < request->height))
	   return XtGeometryNo;
         /* otherwise, one must be right, so say almost */
         else
	   return XtGeometryAlmost;
    }
    /* if only the width is requested, either it's ok or it isn't */
    else
      if (request->request_mode & CWWidth) {
	 if (preferred->width >= request->width) {
	    preferred->width = request->width;
	    return XtGeometryYes;
	 }
	 else
	    return XtGeometryNo;
      }
    /* if only height is requested... */
    else
      if (request->request_mode & CWHeight) {
	 if (preferred->height >= request->height) {
	    preferred->height = request->height;
	    return XtGeometryYes;
	 }
	 else
	    return XtGeometryNo;
      }
    return XtGeometryYes;
}

static XtGeometryResult GeometryManager (w, request, reply)
     Widget w;
     XtWidgetGeometry *request, *reply;
{
    ColumnWidget cw = (ColumnWidget) w->core.parent;
    Mask mask;
    XtGeometryResult result;
    Dimension wdelta, hdelta;

    /* child-widgets aren't allowed to change their position
     */
    if (((request->request_mode & CWX) && request->x != w->core.x) ||
	((request->request_mode & CWY) && request->y != w->core.y))
      return XtGeometryNo;

    if (request->request_mode & (CWWidth|CWHeight|CWBorderWidth)) {
       /* save orig size, set corresponding widget fields to requested sizes */
       Dimension savewidth = w->core.width;
       Dimension saveheight = w->core.height;
       Dimension saveborderwidth = w->core.border_width;
       if (request->request_mode & CWWidth) w->core.width = request->width;
       if (request->request_mode & CWHeight) w->core.height = request->height;
       if (request->request_mode & CWBorderWidth)
          w->core.border_width = request->border_width;
       
       /* see if we can still handle all the children if the req is granted */
       result = try_layout(cw, &mask, &wdelta, &hdelta);

       /* if the children won't fit, restore widget to orig size, say no */
       if (result == XtGeometryNo) {
	  w->core.width = savewidth;
	  w->core.height = saveheight;
	  w->core.border_width = saveborderwidth;
	  return XtGeometryNo;
       }
       /* if only one dim. doesn't fit, restore that one, and say almost */
       if (result == XtGeometryAlmost)
	  if (! (mask & CWWidth)) {
	     reply->width = w->core.width = savewidth;
	  if (! (mask & CWHeight)) {
	     reply->height = w->core.height = saveheight;
	     reply->border_width = saveborderwidth;
	     w->core.border_width = saveborderwidth;
	  }
	  return XtGeometryAlmost;
       }
       /* else everything fits... */
       do_layout (cw);
       return XtGeometryYes;
    }
    return XtGeometryYes;
}

static XtGeometryResult try_layout (parent, mask, w_delta, h_delta)
     ColumnWidget parent;
     Mask *mask;
     Dimension *w_delta, *h_delta;
{
    int i;
    Dimension max_width = 0, total_height = 0;
    
    /* get bbox of all children */
    for (i=0; i<parent->composite.num_children; i++) {
       Widget child;
       Dimension width, height;
       child = parent->composite.children[i];
       if (child->core.managed) {
	  width  = child->core.width  + 2*child->core.border_width;
	  height = child->core.height + 2*child->core.border_width;
	  total_height += height;
	  max_width = MAX(max_width, width);
       }
    }
    /* if everyone doesn't fit, ask if we can grow.  Return result, after
       setting the mask to indicate which (if any) dimension is ok */
    if (total_height > parent->core.height ||
	max_width > parent->core.width) {
       XtGeometryResult result;
       Dimension replyWidth, replyHeight;
       Dimension width  = MAX(max_width, parent->core.width);
       Dimension height = MAX(total_height, parent->core.height);

       result = XtMakeResizeRequest((Widget) parent, width, height,
				    &replyWidth, &replyHeight);
       *mask = 0;
       if (max_width == replyWidth) *mask = CWWidth;
       if (total_height == replyHeight) *mask |= CWHeight;
       if (result == XtGeometryAlmost)
          XtMakeResizeRequest ((Widget) parent, replyWidth, replyHeight,
			       NULL, NULL);
       *w_delta = max_width - parent->core.width;
       *h_delta = total_height - parent->core.height;
       return result;
    }
    /* if everybody fits, just say yes */
    *mask = CWWidth|CWHeight;
    return XtGeometryYes;
}


static void ChangeManaged (w)
     ColumnWidget w;
{
    XtGeometryResult result;
    Dimension width, height, delta, i;
    Mask mask;
    Widget child;
    
    /* see if everyone fits */
    result = try_layout (w, &mask, &width, &height);

    /* if no, shrink the kids */
    if (result != XtGeometryYes) {
       if (w->composite.num_children > 0) {
	  delta = width / w->composite.num_children;
	  for (i=0; i < w->composite.num_children; i++) {
	     child = w->composite.children[i];
	     height = MIN((int)child->core.height,
			  (int)w->core.height - (int)child->core.border_width);
	     if (child->core.managed)
	        XtResizeWidget (child, child->core.width - delta, height,
				child->core.border_width);
	  }
       }
    }
    /* move 'em out */
    do_layout(w);
}
