/* Implements a lightweight menubar widget.
   Copyright (C) 1992 Lucid, Inc.

This file is part of the Lucid Widget Library.

The Lucid Widget Library is free software; you can redistribute it and/or 
modify it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

The Lucid Widget Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of 
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Created by devin@lucid.com */

#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/cursorfont.h>
#include <X11/bitmaps/gray>
#include "xlwmenuP.h"

static char 
xlwMenuTranslations [] = 
"<BtnDown>:	start()\n"
"<BtnMotion>:	drag()\n"
"<BtnUp>:	select()\n"
;

#define offset(field) XtOffset(XlwMenuWidget, field)
static XtResource 
xlwMenuResources[] =
{ 
  {XtNfont,  XtCFont, XtRFontStruct, sizeof(XFontStruct *),
     offset(menu.font),XtRString, "XtDefaultFont"},
  {XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
     offset(menu.foreground), XtRString, "XtDefaultForeground"},
  {XtNmargin, XtCMargin, XtRDimension,  sizeof(Dimension),
     offset(menu.margin), XtRImmediate, (XtPointer)0},
  {XtNhorizontalSpacing, XtCMargin, XtRDimension,  sizeof(Dimension),
     offset(menu.horizontal_spacing), XtRImmediate, (XtPointer)3},
  {XtNverticalSpacing, XtCMargin, XtRDimension,  sizeof(Dimension),
     offset(menu.vertical_spacing), XtRImmediate, (XtPointer)1},
  {XtNarrowSpacing, XtCMargin, XtRDimension,  sizeof(Dimension),
     offset(menu.arrow_spacing), XtRImmediate, (XtPointer)10},

  {XmNshadowThickness, XmCShadowThickness, XmRHorizontalDimension,
     sizeof (Dimension), offset (menu.shadow_thickness),
     XtRImmediate, (XtPointer) 2},
  {XmNtopShadowColor, XmCTopShadowColor, XtRPixel, sizeof (Pixel),
     offset (menu.top_shadow_color), XtRImmediate, (XtPointer)-1},
  {XmNbottomShadowColor, XmCBottomShadowColor, XtRPixel, sizeof (Pixel),
     offset (menu.bottom_shadow_color), XtRImmediate, (XtPointer)-1},
  {XmNtopShadowPixmap, XmCTopShadowPixmap, XtRPixmap, sizeof (Pixmap),
     offset (menu.top_shadow_pixmap), XtRImmediate, (XtPointer)None},
  {XmNbottomShadowPixmap, XmCBottomShadowPixmap, XtRPixmap, sizeof (Pixmap),
     offset (menu.bottom_shadow_pixmap), XtRImmediate, (XtPointer)None},

  {XtNopen, XtCCallback, XtRCallback, sizeof(XtPointer), 
     offset(menu.open), XtRCallback, (XtPointer)NULL},
  {XtNselect, XtCCallback, XtRCallback, sizeof(XtPointer), 
     offset(menu.select), XtRCallback, (XtPointer)NULL},
  {XtNmenu, XtCMenu, XtRPointer, sizeof(XtPointer),
     offset(menu.contents), XtRImmediate, (XtPointer)NULL},
  {XtNcursor, XtCCursor, XtRCursor, sizeof(Cursor),
     offset(menu.cursor_shape), XtRString, (XtPointer)"right_ptr"},
  {XtNhorizontal, XtCHorizontal, XtRInt, sizeof(int),
     offset(menu.horizontal), XtRImmediate, (XtPointer)True},
};
#undef offset

static Boolean XlwMenuSetValues();
static void XlwMenuRealize();
static void XlwMenuRedisplay();
static void XlwMenuResize();
static void XlwMenuInitialize();
static void XlwMenuRedisplay();
static void XlwMenuDestroy();
static void XlwMenuClassInitialize();
static void Start();
static void Drag();
static void Select();

static XtActionsRec 
xlwMenuActionsList [] =
{
  {"start",		Start},
  {"drag",		Drag},
  {"select",		Select},
};

#define SuperClass ((CoreWidgetClass)&coreClassRec)

XlwMenuClassRec xlwMenuClassRec =
{
  {  /* CoreClass fields initialization */
    (WidgetClass) SuperClass,		/* superclass		  */	
    "XlwMenu",				/* class_name		  */
    sizeof(XlwMenuRec),			/* size			  */
    XlwMenuClassInitialize,		/* class_initialize	  */
    NULL,				/* class_part_initialize  */
    FALSE,				/* class_inited		  */
    XlwMenuInitialize,			/* initialize		  */
    NULL,				/* initialize_hook	  */
    XlwMenuRealize,			/* realize		  */
    xlwMenuActionsList,			/* actions		  */
    XtNumber(xlwMenuActionsList),	/* num_actions		  */
    xlwMenuResources,			/* resources		  */
    XtNumber(xlwMenuResources),		/* resource_count	  */
    NULLQUARK,				/* xrm_class		  */
    TRUE,				/* compress_motion	  */
    TRUE,				/* compress_exposure	  */
    TRUE,				/* compress_enterleave    */
    FALSE,				/* visible_interest	  */
    XlwMenuDestroy,			/* destroy		  */
    XlwMenuResize,			/* resize		  */
    XlwMenuRedisplay,			/* expose		  */
    XlwMenuSetValues,			/* set_values		  */
    NULL,				/* set_values_hook	  */
    XtInheritSetValuesAlmost,		/* set_values_almost	  */
    NULL,				/* get_values_hook	  */
    NULL,				/* accept_focus		  */
    XtVersion,				/* version		  */
    NULL,				/* callback_private	  */
    xlwMenuTranslations,		/* tm_table		  */
    XtInheritQueryGeometry,		/* query_geometry	  */
    XtInheritDisplayAccelerator,	/* display_accelerator	  */
    NULL				/* extension		  */
  },  /* XlwMenuClass fields initialization */
  {
    0					/* dummy */
  },
};

WidgetClass xlwMenuWidgetClass = (WidgetClass) &xlwMenuClassRec;

/* Utilities */
static void
push_new_stack (XlwMenuWidget mw, widget_value* val)
{
  if (!mw->menu.new_stack)
    {
      mw->menu.new_stack_length = 10;
      mw->menu.new_stack =
	(widget_value**)XtCalloc (mw->menu.new_stack_length,
				  sizeof (widget_value*));
    }
  else if (mw->menu.new_depth == mw->menu.new_stack_length)
    {
      mw->menu.new_stack_length *= 2;
      mw->menu.new_stack =
	(widget_value**)XtRealloc ((char*)mw->menu.new_stack,
				   mw->menu.new_stack_length * sizeof (widget_value*));
    }
  mw->menu.new_stack [mw->menu.new_depth++] = val;
}

static void
pop_new_stack_if_no_contents (XlwMenuWidget mw)
{
  if (mw->menu.new_depth)
    {
      if (!mw->menu.new_stack [mw->menu.new_depth - 1]->contents)
	mw->menu.new_depth -= 1;
    }
}

static void
make_old_stack_space (XlwMenuWidget mw, int n)
{
  if (!mw->menu.old_stack)
    {
      mw->menu.old_stack_length = 10;
      mw->menu.old_stack =
	(widget_value**)XtCalloc (mw->menu.old_stack_length,
				  sizeof (widget_value*));
    }
  else if (mw->menu.old_stack_length < n)
    {
      mw->menu.old_stack_length *= 2;
      mw->menu.old_stack =
	(widget_value**)XtRealloc ((char*)mw->menu.old_stack,
				   mw->menu.old_stack_length * sizeof (widget_value*));
    }
}

/* Size code */
static Boolean
all_dashes_p (char* s)
{
  char* p;
  for (p = s; *p == '-'; p++);
  return !*p;
}

static int
string_width (XlwMenuWidget mw, char* s)
{
  XCharStruct xcs;
  int drop;
  
  XTextExtents (mw->menu.font, s, strlen (s), &drop, &drop, &drop, &xcs);
  return xcs.width;
}

static int
arrow_width (XlwMenuWidget mw)
{
  return mw->menu.font->ascent / 2 | 1;
}

/* Returns the sizes of an item */
static void
size_menu_item (XlwMenuWidget mw, widget_value* val, int horizontal_p,
		int* label_width, int* rest_width, int* height)
{
  if (all_dashes_p (val->name))
    {
      *height = 2;
      *label_width = 1;
      *rest_width = 0;
    }
  else
    {
      *height =
	mw->menu.font->ascent + mw->menu.font->descent
	  + 2 * mw->menu.vertical_spacing + 2 * mw->menu.shadow_thickness;
      
      *label_width =
	string_width (mw, val->name) + mw->menu.horizontal_spacing
	  + mw->menu.shadow_thickness;
      
      *rest_width =  mw->menu.horizontal_spacing + mw->menu.shadow_thickness;
      if (!horizontal_p)
	{
	  if (val->contents)
	    *rest_width += arrow_width (mw) + mw->menu.arrow_spacing;
	  else if (val->key)
	    *rest_width +=
	      string_width (mw, val->key) + mw->menu.arrow_spacing;
	}
    }
}

static void
size_menu (XlwMenuWidget mw, int level)
{
  int		label_width = 0;
  int		rest_width = 0;
  int		max_rest_width = 0;
  int		height = 0;
  int		horizontal_p = mw->menu.horizontal && (level == 0);
  widget_value*	val;
  window_state*	ws;

  if (level >= mw->menu.old_depth)
    abort ();

  ws = &mw->menu.windows [level];  
  ws->width = 0;
  ws->height = 0;
  ws->label_width = 0;

  for (val = mw->menu.old_stack [level]->contents; val; val = val->next)
    {
      size_menu_item (mw, val, horizontal_p, &label_width, &rest_width,
		      &height);
      if (horizontal_p)
	{
	  ws->width += label_width + rest_width;
	  if (height > ws->height)
	    ws->height = height;
	}
      else
	{
	  if (label_width > ws->label_width)
	    ws->label_width = label_width;
	  if (rest_width > max_rest_width)
	    max_rest_width = rest_width;
	  ws->height += height;
	}
    }
  
  if (horizontal_p)
    ws->label_width = 0;
  else
    ws->width = ws->label_width + max_rest_width;

  ws->width += 2 * mw->menu.shadow_thickness;
  ws->height += 2 * mw->menu.shadow_thickness;
}


/* Display code */
static void
draw_arrow (XlwMenuWidget mw, Window window, GC gc, int x, int y, int width)
{
  XPoint points [3];
  points [0].x = x;
  points [0].y = y + mw->menu.font->ascent;
  points [1].x = x;
  points [1].y = y;
  points [2].x = x + width;
  points [2].y = y + mw->menu.font->ascent / 2;
  
  XFillPolygon (XtDisplay (mw), window, gc, points, 3, Convex,
		CoordModeOrigin);
}

/* This is XmSHADOW_OUT */
static void
draw_shadow_rectangle (XlwMenuWidget mw, Window window,
		      int x, int y, int width, int height)
{
  Display *dpy = XtDisplay (mw);
  GC top_gc = mw->menu.shadow_top_gc;
  GC bottom_gc = mw->menu.shadow_bottom_gc;
  int thickness = mw->menu.shadow_thickness;
  XPoint points [4];
  points [0].x = x;
  points [0].y = y;
  points [1].x = x + width;
  points [1].y = y;
  points [2].x = x + width - thickness;
  points [2].y = y + thickness;
  points [3].x = x;
  points [3].y = y + thickness;
  XFillPolygon (dpy, window, top_gc, points, 4, Convex, CoordModeOrigin);
  points [0].x = x;
  points [0].y = y + thickness;
  points [1].x = x;
  points [1].y = y + height;
  points [2].x = x + thickness;
  points [2].y = y + height - thickness;
  points [3].x = x + thickness;
  points [3].y = y + thickness;
  XFillPolygon (dpy, window, top_gc, points, 4, Convex, CoordModeOrigin);
  points [0].x = x + width;
  points [0].y = y;
  points [1].x = x + width - thickness;
  points [1].y = y + thickness;
  points [2].x = x + width - thickness;
  points [2].y = y + height - thickness;
  points [3].x = x + width;
  points [3].y = y + height - thickness;
  XFillPolygon (dpy, window, bottom_gc, points, 4, Convex, CoordModeOrigin);
  points [0].x = x;
  points [0].y = y + height;
  points [1].x = x + width;
  points [1].y = y + height;
  points [2].x = x + width;
  points [2].y = y + height - thickness;
  points [3].x = x + thickness;
  points [3].y = y + height - thickness;
  XFillPolygon (dpy, window, bottom_gc, points, 4, Convex, CoordModeOrigin);
}


/* Display the menu item and increment where.x and where.y to show how large
** the menu item was. 
*/
static void
display_menu_item (XlwMenuWidget mw, widget_value* val, window_state* ws,
		   XPoint* where, Boolean highlighted_p, Boolean horizontal_p,
		   Boolean just_compute_p)
{
  GC fgc;
  GC bgc;
  int font_ascent = mw->menu.font->ascent;
  int font_descent = mw->menu.font->descent;
  int shadow = mw->menu.shadow_thickness;
  int separator_p = all_dashes_p (val->name);
  int h_spacing = mw->menu.horizontal_spacing;
  int v_spacing = mw->menu.vertical_spacing;
  int label_width;
  int rest_width;
  int height;
  int width;

  /* compute the sizes of the item */
  size_menu_item (mw, val, horizontal_p, &label_width, &rest_width, &height);

  if (horizontal_p)
    width = label_width + rest_width;
  else
    {
      label_width = ws->label_width;
      width = ws->width - 2 * shadow;
    }

  /* Only highlight an enabled item that has a callback. */
  if (highlighted_p)
    if (!val->enabled || !(val->call_data || val->contents))
      highlighted_p = 0;

  /* do the drawing. */
  if (!just_compute_p)
    {
      /* Add the shadow border of the containing menu */
      int x = where->x + shadow;
      int y = where->y + shadow;

      /* pick the foreground and background GC. */
      fgc = val->enabled ? mw->menu.foreground_gc : mw->menu.inactive_gc;
      bgc = mw->menu.background_gc;
      
      if (separator_p)
	{
	  XDrawLine (XtDisplay (mw), ws->window, mw->menu.shadow_bottom_gc,
		     x, y, x + width, y);
	  XDrawLine (XtDisplay (mw), ws->window, mw->menu.shadow_top_gc,
		     x, y + 1, x + width, y + 1);
	}
      else 
	{
	  XFillRectangle (XtDisplay (mw), ws->window, bgc, x, y, width,
			  height);
	  XDrawString (XtDisplay (mw), ws->window, fgc,
		       x + h_spacing + shadow,
		       y + v_spacing + shadow + font_ascent,
		       val->name, strlen (val->name));
	  
	  if (!horizontal_p)
	    {
	      if (val->contents)
		{
		  int a_w = arrow_width (mw);
		  draw_arrow (mw, ws->window, fgc,
			      x + width - (a_w + shadow + h_spacing),
			      y + v_spacing + shadow, a_w);
		}
	      else if (val->key)
		{
		  XDrawString (XtDisplay (mw), ws->window, fgc,
			       x + label_width + mw->menu.arrow_spacing,
			       y + v_spacing + shadow + font_ascent,
			       val->key, strlen (val->key));
		}
	    }
	  if (highlighted_p)
	    draw_shadow_rectangle (mw, ws->window, x, y, width, height);
	}
    }
  
  where->x += width;
  where->y += height;
}

static void
display_menu (XlwMenuWidget mw, int level, Boolean just_compute_p,
	      XPoint* highlighted_pos, XPoint* hit, widget_value** hit_return,
	      widget_value* this, widget_value* that)
{
  widget_value*	val;
  widget_value* following_item;
  window_state* ws;
  XPoint	where;
  int horizontal_p = mw->menu.horizontal && (level == 0);
  int highlighted_p;
  int just_compute_this_one_p;

  if (level >= mw->menu.old_depth)
    abort ();

  if (level < mw->menu.old_depth - 1)
    following_item = mw->menu.old_stack [level + 1];
  else 
    following_item = NULL;

  if (hit)
    *hit_return = NULL;

  where.x = 0;
  where.y = 0;

  ws = &mw->menu.windows [level];
  for (val = mw->menu.old_stack [level]->contents; val; val = val->next)
    {
      highlighted_p = val == following_item;
      if (highlighted_p && highlighted_pos)
	{
	  if (horizontal_p)
	    highlighted_pos->x = where.x;
	  else
	    highlighted_pos->y = where.y;
	}
      
      just_compute_this_one_p =
	just_compute_p || ((this || that) && val != this &&  val != that);

      display_menu_item (mw, val, ws, &where, highlighted_p, horizontal_p,
			 just_compute_this_one_p);

      if (highlighted_p && highlighted_pos)
	{
	  if (horizontal_p)
	    highlighted_pos->y = where.y;
	  else
	    highlighted_pos->x = where.x;
	}

      if (hit
	  && !*hit_return
	  && (horizontal_p ? hit->x < where.x : hit->y < where.y)
	  && !all_dashes_p (val->name))
	*hit_return = val;

      if (horizontal_p)
	where.y = 0;
      else
	where.x = 0;
    }
  
  if (!just_compute_p)
    draw_shadow_rectangle (mw, ws->window, 0, 0, ws->width, ws->height);
}

/* Motion code */
static void
set_new_state (XlwMenuWidget mw, widget_value* val, int level)
{
  int i;
  
  mw->menu.new_depth = 0;
  for (i = 0; i < level; i++)
    push_new_stack (mw, mw->menu.old_stack [i]);
  push_new_stack (mw, val);
}

static void
make_windows_if_needed (XlwMenuWidget mw, int n)
{
  int i;
  int start_at;
  XSetWindowAttributes xswa;
  int mask;
  Window root = RootWindowOfScreen (DefaultScreenOfDisplay (XtDisplay (mw)));
  window_state* windows;
  
  if (mw->menu.windows_length >= n)
    return;

  xswa.save_under = True;
  xswa.override_redirect = True;
  xswa.background_pixel = mw->core.background_pixel;
  xswa.border_pixel = mw->core.border_pixel;
  xswa.event_mask =
    ExposureMask | ButtonMotionMask | ButtonReleaseMask | ButtonPressMask;
  xswa.cursor = mw->menu.cursor_shape;
  mask = CWSaveUnder | CWOverrideRedirect | CWBackPixel | CWBorderPixel
    | CWEventMask | CWCursor;
  
  if (!mw->menu.windows)
    {
      mw->menu.windows =
	(window_state*)XtMalloc (n * sizeof (window_state));
      start_at = 0;
    }
  else
    {
      mw->menu.windows =
	(window_state*)XtRealloc ((char*)mw->menu.windows,
				  n * sizeof (window_state));
      start_at = mw->menu.windows_length;
    }
  mw->menu.windows_length = n;

  windows = mw->menu.windows;

  for (i = start_at; i < n; i++)
   {
     windows [i].x = 0;
     windows [i].y = 0;
     windows [i].width = 1;
     windows [i].height = 1;
     windows [i].window =
       XCreateWindow (XtDisplay (mw), root, 0, 0, 1, 1,
		      0, 0, CopyFromParent, CopyFromParent, mask, &xswa);
  }
}

/* Updates old_stack from new_stack and redisplays. */
static void
remap_menubar (XlwMenuWidget mw)
{
  int i;
  int last_same;
  XPoint size;
  XPoint selection_position;
  int redraw;
  int old_depth = mw->menu.old_depth;
  int new_depth = mw->menu.new_depth;
  widget_value** old_stack;
  widget_value** new_stack;
  window_state* windows;
  widget_value* old_selection;
  widget_value* new_selection;

  /* Check that enough windows and old_stack are ready. */
  make_windows_if_needed (mw, new_depth);
  make_old_stack_space (mw, new_depth);
  windows = mw->menu.windows;
  old_stack = mw->menu.old_stack;
  new_stack = mw->menu.new_stack;

  /* compute the last identical different entry */
  for (i = 1; i < old_depth && i < new_depth; i++)
    if (old_stack [i] != new_stack [i])
      break;
  last_same = i - 1;

  /* Memorize the previously selected item to be able to refresh it */
  old_selection = last_same + 1 < old_depth ? old_stack [last_same + 1] : NULL;
  if (old_selection && !old_selection->enabled)
    old_selection = NULL;
  new_selection = last_same + 1 < new_depth ? new_stack [last_same + 1] : NULL;
  if (new_selection && !new_selection->enabled)
    new_selection = NULL;

  /* updates old_state from new_state.  It has to be done now because
     display_menu (called below) uses the old_stack to know what to display. */
  for (i = last_same + 1; i < new_depth; i++)
    old_stack [i] = new_stack [i];
  mw->menu.old_depth = new_depth;

  /* refresh the last seletion */
  selection_position.x = 0;
  selection_position.y = 0;
  display_menu (mw, last_same, new_selection == old_selection,
		&selection_position, NULL, NULL, old_selection, new_selection);

  /* Now popup the new menus */
  for (i = last_same + 1; i < new_depth && new_stack [i]->contents; i++)
    {
      window_state* previous_ws = &windows [i - 1];
      window_state* ws = &windows [i];

      ws->x =
	previous_ws->x + selection_position.x + mw->menu.shadow_thickness;
      if (!mw->menu.horizontal || i > 1)
	ws->x += mw->menu.shadow_thickness;
      ws->y =
	previous_ws->y + selection_position.y + mw->menu.shadow_thickness;

      size_menu (mw, i);

      XMoveResizeWindow (XtDisplay (mw), ws->window, ws->x, ws->y,
			 ws->width, ws->height);
      XMapRaised (XtDisplay (mw), ws->window);
      display_menu (mw, i, False, &selection_position, NULL, NULL, NULL, NULL);
    }

  /* unmap the menus that popped down */
  for (i = new_depth - 1; i < old_depth; i++)
    if (i >= new_depth || !new_stack [i]->contents)
      XUnmapWindow (XtDisplay (mw), windows [i].window);
}

static Boolean
motion_event_is_in_menu (XlwMenuWidget mw, XMotionEvent* ev, int level,
			 XPoint* relative_pos)
{
  window_state* ws = &mw->menu.windows [level];
  int x = level == 0 ? ws->x : ws->x + mw->menu.shadow_thickness;
  int y = level == 0 ? ws->y : ws->y + mw->menu.shadow_thickness;
  relative_pos->x = ev->x_root - x;
  relative_pos->y = ev->y_root - y;
  return (x < ev->x_root && ev->x_root < x + ws->width
	  && y < ev->y_root && ev->y_root < y + ws->height);
}

static Boolean
map_event_to_widget_value (XlwMenuWidget mw, XMotionEvent* ev,
			   widget_value** val, int* level)
{
  int 		i;
  XPoint	relative_pos;
  window_state*	ws;

  *val = NULL;
  
  /* Find the window */
  for (i = mw->menu.old_depth - 1; i >= 0; i--)
    {
      ws = &mw->menu.windows [i];
      if (ws && motion_event_is_in_menu (mw, ev, i, &relative_pos))
	{
	  display_menu (mw, i, True, NULL, &relative_pos, val, NULL, NULL);

	  if (*val)
	    {
	      *level = i + 1;
	      return True;
	    }
	}
    }
  return False;
}

/* Procedures */
static void
make_shadow_gcs (XlwMenuWidget mw, Pixmap gray_pixmap)
{
  XGCValues xgcv;
  unsigned long pm = 0;
  Display *dpy = XtDisplay ((Widget) mw);
  Colormap cmap = DefaultColormapOfScreen (XtScreen ((Widget) mw));
  XColor topc, botc;
  int top_frobbed = 0, bottom_frobbed = 0;

  if (mw->menu.top_shadow_color == -1)
    mw->menu.top_shadow_color = mw->core.background_pixel;
  if (mw->menu.bottom_shadow_color == -1)
    mw->menu.bottom_shadow_color = mw->menu.foreground;

  if (mw->menu.top_shadow_color == mw->core.background_pixel ||
      mw->menu.top_shadow_color == mw->menu.foreground)
    {
      topc.pixel = mw->core.background_pixel;
      XQueryColor (dpy, cmap, &topc);
      topc.red   *= 1.2;
      topc.green *= 1.2;
      topc.blue  *= 1.2;
      if (XAllocColor (dpy, cmap, &topc))
	{
	  mw->menu.top_shadow_color = topc.pixel;
	  top_frobbed = 1;
	}
    }
  if (mw->menu.bottom_shadow_color == mw->menu.foreground ||
      mw->menu.bottom_shadow_color == mw->core.background_pixel)
    {
      botc.pixel = mw->core.background_pixel;
      XQueryColor (dpy, cmap, &botc);
      botc.red   *= 0.6;
      botc.green *= 0.6;
      botc.blue  *= 0.6;
      if (XAllocColor (dpy, cmap, &botc))
	{
	  mw->menu.bottom_shadow_color = botc.pixel;
	  bottom_frobbed = 1;
	}
    }

  if (top_frobbed && bottom_frobbed)
    {
      int top_avg = ((topc.red / 3) + (topc.green / 3) + (topc.blue / 3));
      int bot_avg = ((botc.red / 3) + (botc.green / 3) + (botc.blue / 3));
      Pixel tmp;
      if (bot_avg > top_avg)
	tmp = mw->menu.top_shadow_color,
	mw->menu.top_shadow_color = mw->menu.bottom_shadow_color,
	mw->menu.bottom_shadow_color = tmp;
    }

  if (!mw->menu.top_shadow_pixmap &&
      mw->menu.top_shadow_color == mw->core.background_pixel)
    {
      mw->menu.top_shadow_pixmap = gray_pixmap;
      mw->menu.top_shadow_color = mw->menu.foreground;
    }
  if (!mw->menu.bottom_shadow_pixmap &&
      mw->menu.bottom_shadow_color == mw->core.background_pixel)
    {
      mw->menu.bottom_shadow_pixmap = gray_pixmap;
      mw->menu.bottom_shadow_color = mw->menu.foreground;
    }

  xgcv.fill_style = FillStippled;
  xgcv.foreground = mw->menu.top_shadow_color;
  xgcv.stipple = mw->menu.top_shadow_pixmap;
  pm = (xgcv.stipple ? GCStipple|GCFillStyle : 0);
  mw->menu.shadow_top_gc = XtGetGC ((Widget)mw, GCForeground | pm, &xgcv);
  xgcv.foreground = mw->menu.bottom_shadow_color;
  xgcv.stipple = mw->menu.bottom_shadow_pixmap;
  pm = (xgcv.stipple ? GCStipple|GCFillStyle : 0);
  mw->menu.shadow_bottom_gc = XtGetGC ((Widget)mw, GCForeground | pm, &xgcv);
}


static void
XlwMenuInitialize (Widget request, Widget new, ArgList args,
		   Cardinal *num_args)
{
  /* Get the GCs and the widget size */
  XlwMenuWidget mw = (XlwMenuWidget)new;

  XSetWindowAttributes xswa;
  XPoint size;
  int mask;
  
  Window window = RootWindowOfScreen (DefaultScreenOfDisplay (XtDisplay (mw)));
  Display* display = XtDisplay (mw);
  XGCValues xgcv;
  Pixmap gray_pixmap;
  
  gray_pixmap = XCreatePixmapFromBitmapData (display, window,
					     gray_bits, gray_width,
					     gray_height, 1, 0, 1);
  
  xgcv.font = mw->menu.font->fid;
  xgcv.foreground = mw->menu.foreground;
  xgcv.background = mw->core.background_pixel;
  mw->menu.foreground_gc = XtGetGC ((Widget)mw,
				    GCFont | GCForeground | GCBackground,
				    &xgcv);
  
  xgcv.fill_style = FillStippled;
  xgcv.stipple = gray_pixmap;
  mw->menu.inactive_gc = XtGetGC ((Widget)mw,
				     (GCFont | GCForeground | GCBackground
				      | GCFillStyle | GCStipple), &xgcv);
  
  xgcv.foreground = mw->core.background_pixel;
  xgcv.background = mw->menu.foreground;
  mw->menu.background_gc = XtGetGC ((Widget)mw,
				     GCFont | GCForeground | GCBackground,
				     &xgcv);

  mw->menu.cursor = XCreateFontCursor (display, mw->menu.cursor_shape);
  
  make_shadow_gcs (mw, gray_pixmap);

  xswa.background_pixel = mw->core.background_pixel;
  xswa.border_pixel = mw->core.border_pixel;
  mask = CWBackPixel | CWBorderPixel;
  
  mw->menu.popped_up = False;

  mw->menu.old_depth = 1;
  mw->menu.old_stack = (widget_value**)XtMalloc (sizeof (widget_value*));
  mw->menu.old_stack_length = 1;
  mw->menu.old_stack [0] = mw->menu.contents;

  mw->menu.new_depth = 0;
  mw->menu.new_stack = 0;
  mw->menu.new_stack_length = 0;
  push_new_stack (mw, mw->menu.contents);

  mw->menu.windows = (window_state*)XtMalloc (sizeof (window_state));
  mw->menu.windows_length = 1;
  mw->menu.windows [0].x = 0;
  mw->menu.windows [0].y = 0;
  mw->menu.windows [0].width = 0;
  mw->menu.windows [0].height = 0;
  size_menu (mw, 0);

  mw->core.width = mw->menu.windows [0].width;
  mw->core.height = mw->menu.windows [0].height;
}

static void
XlwMenuClassInitialize ()
{
}

static void
XlwMenuRealize (Widget w, Mask *valueMask, XSetWindowAttributes *attributes)
{
  XlwMenuWidget mw = (XlwMenuWidget)w;
  XSetWindowAttributes xswa;
  int mask;

  (*xlwMenuWidgetClass->core_class.superclass->core_class.realize)
    (w, valueMask, attributes);

  xswa.save_under = True;
  xswa.cursor = mw->menu.cursor_shape;
  mask = CWSaveUnder | CWCursor;
  XChangeWindowAttributes (XtDisplay (w), XtWindow (w), mask, &xswa);

  mw->menu.windows [0].window = XtWindow (w);
  mw->menu.windows [0].x = w->core.x;
  mw->menu.windows [0].y = w->core.y;
  mw->menu.windows [0].width = w->core.width;
  mw->menu.windows [0].height = w->core.height;
}

static void 
XlwMenuRedisplay (Widget w, XEvent* ev, Region region)
{
  XlwMenuWidget mw = (XlwMenuWidget)w;
  int i;

  for (i = 0; i < mw->menu.old_depth; i++)
    if (ev->xexpose.window == mw->menu.windows [i].window)
      {
	display_menu (mw, i, False, NULL, NULL, NULL, NULL, NULL);
	return;
      }
}

static void 
XlwMenuDestroy (Widget w)
{
  XlwMenuWidget mw = (XlwMenuWidget)w;
  XtReleaseGC (w, mw->menu.foreground_gc);
  XtReleaseGC (w, mw->menu.background_gc);
  XtReleaseGC (w, mw->menu.inactive_gc);
  XtReleaseGC (w, mw->menu.shadow_top_gc);
  XtReleaseGC (w, mw->menu.shadow_bottom_gc);

  /* should also destroy the menu windows. */
}

static Boolean 
XlwMenuSetValues (Widget current, Widget request, Widget new)
{
  XlwMenuWidget oldmw = (XlwMenuWidget)current;
  XlwMenuWidget mw = (XlwMenuWidget)new;
  Boolean redisplay = False;

  /* should update the GCs. */
  return True;
}

static void 
XlwMenuResize (Widget w)
{
  XlwMenuWidget mw = (XlwMenuWidget)w;

  mw->menu.windows [0].width = mw->core.width;
  mw->menu.windows [0].height = mw->core.height;
}

/* Action procedures */
static void
handle_motion_event (XlwMenuWidget mw, XMotionEvent* ev)
{
  widget_value*		val;
  int 			level;

  if (!map_event_to_widget_value (mw, ev, &val, &level))
    pop_new_stack_if_no_contents (mw);
  else
    set_new_state (mw, val, level);
  remap_menubar (mw);

  /* Sync with the display.  Makes it feel better on X terms. */
  XSync (XtDisplay (mw), False);
}

static void 
Start (Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
  XlwMenuWidget mw = (XlwMenuWidget)w;

  XtCallCallbackList ((Widget)mw, mw->menu.open, NULL);
  
  /* notes the absolute position of the menubar window */
  mw->menu.windows [0].x = ev->xmotion.x_root - ev->xmotion.x;
  mw->menu.windows [0].y = ev->xmotion.y_root - ev->xmotion.y;

  /* handles the down like a move, slots are compatible */
  handle_motion_event (mw, &ev->xmotion);
}

static void 
Drag (Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
  XlwMenuWidget mw = (XlwMenuWidget)w;
  handle_motion_event (mw, &ev->xmotion);
}

static void 
Select (Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
  XlwMenuWidget mw = (XlwMenuWidget)w;
  widget_value* selected_item = mw->menu.old_stack [mw->menu.old_depth - 1];
  
  /* pop down everything */
  mw->menu.new_depth = 1;
  remap_menubar (mw);

  if (mw->menu.popped_up)
    {
      mw->menu.popped_up = False;
      XtUngrabPointer ((Widget)mw, ev->xmotion.time);
      XtPopdown (XtParent (mw));
    }

  /* callback */
  XtCallCallbackList ((Widget)mw, mw->menu.select, (XtPointer)selected_item);
  
}


/* Special code to pop-up a menu */
void
pop_up_menu (XlwMenuWidget mw, XButtonPressedEvent* event)
{
  int		x = event->x_root;
  int		y = event->y_root;
  int		w;
  int		h;
  int		borderwidth = mw->menu.shadow_thickness;
  Screen*	screen = XtScreen (mw);
  XPoint	size;

  XtCallCallbackList ((Widget)mw, mw->menu.open, NULL);

  size_menu (mw, 0);

  w = mw->menu.windows [0].width;
  h = mw->menu.windows [0].height;

  x -= borderwidth;
  y -= borderwidth;
  if (x < borderwidth)
    x = borderwidth;
  if (x + w + 2 * borderwidth > WidthOfScreen (screen))
    x = WidthOfScreen (screen) - w - 2 * borderwidth;
  if (y < borderwidth)
    y = borderwidth;
  if (y + h + 2 * borderwidth> HeightOfScreen (screen))
    y = HeightOfScreen (screen) - h - 2 * borderwidth;

  mw->menu.popped_up = True;
  XtConfigureWidget (XtParent (mw), x, y, w, h,
		     XtParent (mw)->core.border_width);
  XtPopup (XtParent (mw), XtGrabExclusive);
  display_menu (mw, 0, False, NULL, NULL, NULL, NULL, NULL);
  XtGrabPointer ((Widget)mw, False,
		ButtonMotionMask | ButtonReleaseMask | ButtonPressMask,
		GrabModeAsync, GrabModeAsync, None, mw->menu.cursor_shape,
		event->time);

  mw->menu.windows [0].x = x + borderwidth;
  mw->menu.windows [0].y = y + borderwidth;

  handle_motion_event (mw, (XMotionEvent*)event);
}
