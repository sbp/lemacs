/* Implements a lightweight menubar widget.  
   Copyright (C) 1992, 1993, 1994 Lucid, Inc.

This file is part of the Lucid Widget Library.

The Lucid Widget Library is free software; you can redistribute it and/or 
modify it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

The Lucid Widget Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of 
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Created by devin@lucid.com */

#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>

#include <sys/types.h>
#include <X11/Xos.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/cursorfont.h>
#include <X11/bitmaps/gray>

#ifdef USE_MOTIF
#include <Xm/Xm.h>
#endif

#include "xlwmenuP.h"

static char 
xlwMenuTranslations [] = 
"<BtnDown>:	start()\n\
<BtnMotion>:	drag()\n\
<BtnUp>:	select()\n\
";

#define offset(field) XtOffset(XlwMenuWidget, field)
static XtResource 
xlwMenuResources[] =
{ 
#ifdef USE_MOTIF
  /* There are three font list resources, so that we can accept either of
     the resources *fontList: or *font:, and so that we can tell the
     difference between them being specified, and being defaulted to a
     font from the XtRString specified here.
   */
  {XmNfontList,  XmCFontList, XmRFontList, sizeof(XmFontList),
     offset(menu.font_list),  XtRImmediate, (XtPointer)0},
  {XtNfont,      XtCFont,     XmRFontList, sizeof(XmFontList),
     offset(menu.font_list_2),XtRImmediate, (XtPointer)0},
  {XmNfontList,  XmCFontList, XmRFontList, sizeof(XmFontList),
     offset(menu.fallback_font_list),
     /* We must use an iso8859-1 font here, or people without $LANG set lose.
	It's fair to assume that those who do have $LANG set also have the
	*fontList resource set, or at least know how to deal with this.
      */
     XtRString, "-*-helvetica-bold-r-*-*-*-120-*-*-*-*-iso8859-1"},
#else
  {XtNfont,  XtCFont, XtRFontStruct, sizeof(XFontStruct *),
     offset(menu.font),XtRString, "XtDefaultFont"},
#endif
  {XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
     offset(menu.foreground), XtRString, "XtDefaultForeground"},
  {XtNbuttonForeground, XtCButtonForeground, XtRPixel, sizeof(Pixel),
     offset(menu.button_foreground), XtRString, "XtDefaultForeground"},
  {XtNmargin, XtCMargin, XtRDimension,  sizeof(Dimension),
     offset(menu.margin), XtRImmediate, (XtPointer)2},
  {XmNmarginWidth, XmCMarginWidth, XmRHorizontalDimension, sizeof(Dimension),
     offset(menu.horizontal_margin), XtRImmediate, (XtPointer)2},
  {XmNmarginHeight, XmCMarginHeight, XmRVerticalDimension, sizeof(Dimension),
     offset(menu.vertical_margin), XtRImmediate, (XtPointer)1},
  {XmNspacing, XmCSpacing, XmRHorizontalDimension,  sizeof(Dimension),
     offset(menu.column_spacing), XtRImmediate, (XtPointer)4},
  {XmNindicatorSize, XmCIndicatorSize, XtRDimension,  sizeof(Dimension),
     offset(menu.indicator_size), XtRImmediate, (XtPointer)0},
  {XmNshadowThickness, XmCShadowThickness, XmRHorizontalDimension,
     sizeof (Dimension), offset (menu.shadow_thickness),
     XtRImmediate, (XtPointer) 2},
  {XmNselectColor, XmCSelectColor, XtRPixel, sizeof (Pixel),
     offset (menu.select_color), XtRImmediate, (XtPointer)-1},
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
  {XtNuseBackingStore, XtCUseBackingStore, XtRBoolean, sizeof (Boolean),
     offset (menu.use_backing_store), XtRImmediate, (XtPointer)False},
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

#ifdef USE_MOTIF
static XFontStruct * default_font_of_font_list (XmFontList);
#endif

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
static char *
safe_strdup (char* s)
{
  char *result;
  if (! s) return 0;
  result = (char *) malloc (strlen (s) + 1);
  if (! result)
    return 0;
  strcpy (result, s);
  return result;
}

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
  if (!s || s[0] == '\0')
    return False;
  for (p = s; *p == '-'; p++);
  return !*p;
}

static int
string_width (XlwMenuWidget mw,
#ifdef USE_MOTIF
	      XmString s
#else
	      char* s
#endif
	      )
{
#ifdef USE_MOTIF
  Dimension width, height;
  XmStringExtent (mw->menu.font_list, s, &width, &height);
  return width;
#else
  XCharStruct xcs;
  int drop;
  XTextExtents (mw->menu.font, s, strlen (s), &drop, &drop, &drop, &xcs);
  return xcs.width;
#endif
}

static void
massage_resource_name (CONST char *in, char *out)
{
  /* Turn a random string into something suitable for using as a resource.
     For example:

     "Kill Buffer"		->	"killBuffer"
     "Find File..."		->	"findFile"
     "Search and Replace..."	->	"searchAndReplace"
   */

# define GOOD_CHAR(c) (((c) >= 'a' && (c) <= 'z') || \
		       ((c) >= 'A' && (c) <= 'Z') || \
		       ((c) >= '0' && (c) <= '9') || \
		       ((c) == '_') || \
		       ((c) > 0240))
  int firstp = 1;
  while (*in)
    {
      if (GOOD_CHAR ((unsigned char) *in))
	{
	  if (firstp)
	    *out = tolower (*in);
	  else
	    *out = toupper (*in);
	  firstp = 0;
	  in++;
	  out++;
	  while (GOOD_CHAR ((unsigned char) *in))
	    {
	      *out = *in;
	      in++;
	      out++;
	    }
	}
      else
	{
	  /* A bogus char between words; skip it. */
	  in++;
	}
    }
  *out = 0;
#undef GOOD_CHAR
}

static XtResource
nameResource[] =
{ 
  { "labelString",  "LabelString", XtRString, sizeof(String),
    0, XtRImmediate, 0 }
};

#ifdef USE_MOTIF

/*
 *    This function looks through string searching for parameter
 *    inserts of the form:
 *    %[padding]1
 *    padding is space (' ') or dash ('-') characters meaning
 *    padding to the left or right of the inserted parameter.
 *    In essence all %1 strings are replaced by value in the return
 *    value (which the caller is expected to free).
 *    %% means insert one % (like printf).
 *    %1 means insert value.
 *    %-1 means insert value followed by one space. The latter is
 *    not inserted if value is a zero length string.
 */
static char*
parameterize_string (char *string, char *value)
{
  char* percent;
  char* result;
  unsigned done = 0;
  unsigned ntimes;

  if (!string)
    string = "";

  if (!value)
    value = "";

  for (ntimes = 1, result = string; percent = strchr(result, '%'); ntimes++)
    result = &percent[1];
  
  result = XtMalloc((ntimes * strlen(value)) + strlen(string) + 4);
  result[0] = '\0';

  while (percent = strchr(string, '%'))
    {
      unsigned left_pad;
      unsigned right_pad;
      char* p;

      if (percent[1] == '%') {	/* it's a real % */
	strncat(result, string, 1 + percent - string); /* incl % */
	string = &percent[2];	/* after the second '%' */
	continue;		/* with the while() loop */
      }

      left_pad = 0;
      right_pad = 0;

      for (p = &percent[1]; *p; p++)
	{
	  if (*p == ' ')
	    {			/* left pad */
	      left_pad++;
	    }
	  else if (*p == '-')
	    {			/* right pad */
	      right_pad++;
	    }
	  else if (*p == '1')
	    {			/* param and terminator */
	      strncat(result, string, percent - string);
	      if (value[0] != '\0') {
		unsigned i;
		for (i = 0; i < left_pad; i++)
		  strcat(result, " ");
		strcat(result, value);
		for (i = 0; i < right_pad; i++)
		  strcat(result, " ");
	      }
	      string = &p[1];	/* after the '1' */
	      done++;		/* no need to do old way */
	      break;		/* out of for() loop */
	    }
	  else
	    {			/* bogus, copy the format as is */
				/* out of for() loop */
	      strncat(result, string, 1 + p - string);
	      string=&p[1];
	      break;		
	    }
	}
    }

  /*
   *    Copy the tail of the string
   */
  strcat(result, string);

  /*
   *    If we have not processed a % string, and we have a value, tail it.
   */
  if (!done && value[0] != '\0')
    {
      strcat(result, " ");
      strcat(result, value);
    }

  return result;
}

XmString
resource_widget_value (XlwMenuWidget mw, widget_value* val)
{
  if (!val->toolkit_data)
    {
      char *resourced_name = NULL;
      char *converted_name;
      XmString complete_name;
      char massaged_name [1024];

      /* Convert value style name into resource style name.
         eg: "Free Willy" becomes "freeWilly"
       */
      massage_resource_name (val->name, massaged_name);

      /* If we have a value (parameter) see if we can find a "Named" resource.
       */
      if (val->value)
	{
	  char named_name[1024];
	  sprintf(named_name, "%sNamed", massaged_name);
	  XtGetSubresources ((Widget) mw,
			     (XtPointer) &resourced_name,
			     named_name, named_name,
			     nameResource, 1, NULL, 0);
	}

      /* If nothing yet, try to load from the massaged name.
       */
      if (!resourced_name)
	{
	  XtGetSubresources ((Widget) mw,
			     (XtPointer) &resourced_name,
			     massaged_name, massaged_name,
			     nameResource, 1, NULL, 0);
	}

      /* Still nothing yet, use the name as the value. */
      if (!resourced_name)
	resourced_name = val->name;

      /* Parameterize the string. */
      converted_name = parameterize_string(resourced_name, val->value);

      /* Improve OSF's bottom line. */
      complete_name = XmStringCreateLtoR (converted_name,
					  XmSTRING_DEFAULT_CHARSET);
      XtFree (converted_name);

      val->toolkit_data = complete_name;
      val->free_toolkit_data = True;
    }
  return ((XmString) val->toolkit_data);
}

/*
 *    These two routines should be a seperate file..djw
 */
char *
xlw_create_localized_string (Widget w,
			     char* name,
			     char** args,
			     unsigned nargs)
{
  char* string = NULL;
  char* arg = NULL;

  if (nargs > 0)
    arg = args[0];

  XtGetSubresources (w,
		    (XtPointer)&string,
		     name,
		     name,
		     nameResource, 1,
		     NULL, 0
		     );

  if (!string)
    string = name;

  return parameterize_string (string, arg);
}

XmString
xlw_create_localized_xmstring (Widget w,
			       char* name,
			       char** args,
			       unsigned nargs)
{
  char*    string = xlw_create_localized_string (w, name, args, nargs);
  XmString xm_string = XmStringCreateLtoR (string, XmSTRING_DEFAULT_CHARSET);
  XtFree(string);
  return xm_string;
}

#else /* !Motif */

static char*
resource_widget_value (XlwMenuWidget mw, widget_value* val)
{
  if (!val->toolkit_data)
    {
      char* resourced_name = NULL;
      char* complete_name;
      char massaged_name [1024];

      massage_resource_name (val->name, massaged_name);

      XtGetSubresources ((Widget) mw,
			 (XtPointer) &resourced_name,
			 massaged_name, massaged_name,
			 nameResource, 1, NULL, 0);
      if (!resourced_name)
	resourced_name = val->name;
      if (!val->value)
	complete_name = safe_strdup (resourced_name);
      else
	{
	  int complete_length =
	    strlen (resourced_name) + strlen (val->value) + 2;
	  complete_name = XtMalloc (complete_length);
	  *complete_name = 0;
	  strcat (complete_name, resourced_name);
	  strcat (complete_name, " ");
	  strcat (complete_name, val->value);
	}

      val->toolkit_data = complete_name;
      val->free_toolkit_data = True;
    }
  return (char*)val->toolkit_data;
}

#endif /* !Motif */

/*
 *    Code for drawing strings.
 */
static void
string_draw(
	    XlwMenuWidget mw,
	    Window window,
	    int x, int y,
	    GC gc,
#ifdef USE_MOTIF
	    XmString string
#else
	    char* string
#endif
) {
#ifdef USE_MOTIF
  XmStringDraw (XtDisplay (mw), window,
		mw->menu.font_list,
		string, gc,
		x, y,
		1000,	/* ???? width */
		XmALIGNMENT_BEGINNING,
		0, /* ???? layout_direction */
		0);
#else
  XDrawString (XtDisplay (mw), window, gc,
	       x, y + mw->menu.font_ascent, string, strlen (string));

#endif
}

static void 
binding_draw(XlwMenuWidget mw, Window w, int x, int y, GC gc, char* value) {
#ifdef USE_MOTIF
  XmString xm_value = XmStringCreateLtoR(value, XmSTRING_DEFAULT_CHARSET);
  string_draw(mw, w, x, y, gc, xm_value);
  XmStringFree(xm_value);
#else
  string_draw(mw, w, x, y, gc, value);
#endif
}

/*
 *    Low level code for drawing 3-D edges.
 */
static void
shadow_rectangle_draw(
		      Display *dpy,
		      Window window,
		      GC top_gc,
		      GC bottom_gc,
		      int x, int y, unsigned width, unsigned height,
		      unsigned thickness
) {
  XPoint points [4];

  if (!thickness)
    return;

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

typedef enum e_shadow_type {
  SHADOW_BACKGROUND,
  SHADOW_OUT,
  SHADOW_IN,
  SHADOW_ETCHED_OUT,
  SHADOW_ETCHED_IN,
  SHADOW_ETCHED_OUT_DASH,
  SHADOW_ETCHED_IN_DASH,
  SHADOW_SINGLE_LINE,
  SHADOW_DOUBLE_LINE,
  SHADOW_SINGLE_DASHED_LINE,
  SHADOW_DOUBLE_DASHED_LINE,
  SHADOW_NO_LINE
} shadow_type;

static void
shadow_draw(
	    XlwMenuWidget mw,
	    Window window,
	    int x, int y, unsigned width, unsigned height,
	    shadow_type type
	    ) {
  Display *dpy = XtDisplay (mw);
  GC top_gc;
  GC bottom_gc;
  int thickness = mw->menu.shadow_thickness;
  XPoint points [4];
  Boolean etched = False;

  switch (type) {
  case SHADOW_BACKGROUND:
    top_gc = bottom_gc = mw->menu.background_gc;
    break;
  case SHADOW_ETCHED_IN:
    top_gc = mw->menu.shadow_bottom_gc;
    bottom_gc = mw->menu.shadow_top_gc;
    etched = True;
    break;
  case SHADOW_ETCHED_OUT:
    top_gc = mw->menu.shadow_top_gc;
    bottom_gc = mw->menu.shadow_bottom_gc;
    etched = True;
    break;
  case SHADOW_IN:
    top_gc = mw->menu.shadow_bottom_gc;
    bottom_gc = mw->menu.shadow_top_gc;
    break;
  case SHADOW_OUT:
  default:
    top_gc = mw->menu.shadow_top_gc;
    bottom_gc = mw->menu.shadow_bottom_gc;
    break;
  }

  if (etched) {
    unsigned half = thickness/2;
    shadow_rectangle_draw(
			  dpy,
			  window,
			  top_gc,
			  top_gc,
			  x, y,
			  width - half, height - half,
			  thickness - half
			  );
    shadow_rectangle_draw(
			  dpy,
			  window,
			  bottom_gc,
			  bottom_gc,
			  x + half, y + half,
			  width - half , height - half,
			  half
			  );
  } else {
    shadow_rectangle_draw(
			  dpy,
			  window,
			  top_gc,
			  bottom_gc,
			  x, y,
			  width, height,
			  thickness
			  );
  }
}

static void 
arrow_decoration_draw(
		      XlwMenuWidget mw,
		      Window window,
		      int x, int y,
		      unsigned width,
		      Boolean raised
) {
  Display *dpy = XtDisplay (mw);
  GC top_gc;
  GC bottom_gc;
  GC select_gc;
  int thickness = mw->menu.shadow_thickness;
  XPoint points [4];
  int half_width;

  if (width & 0x1)
    half_width = width/2 + 1;
  else
    half_width = width/2;

  select_gc = mw->menu.background_gc;

  if (raised) {
    top_gc = mw->menu.shadow_bottom_gc;
    bottom_gc = mw->menu.shadow_top_gc;
  } else {
    top_gc = mw->menu.shadow_top_gc;
    bottom_gc = mw->menu.shadow_bottom_gc;
  }

  /*
   *    Fill internal area, we do this first so that the borders
   *    have a nice sharp edge.
   */
  points [0].x = x + thickness;
  points [0].y = y + thickness;
  points [1].x = x + width - thickness;
  points [1].y = y + half_width;
  points [2].x = x + width - thickness;
  points [2].y = y + half_width + thickness;
  points [3].x = x + thickness;
  points [3].y = y + width - thickness;
    
  XFillPolygon (
		dpy,
		window,
		select_gc,
		points,
		4, 
		Convex,
		CoordModeOrigin
		);

  /* left border */
  points [0].x = x;
  points [0].y = y + thickness;
  points [1].x = x + thickness;
  points [1].y = y + thickness;
  points [2].x = x + thickness;
  points [2].y = y + width - thickness;
  points [3].x = x;
  points [3].y = y + width - thickness;
  
  XFillPolygon(dpy, window, top_gc, points, 4, Convex, CoordModeOrigin);

  /* bottom shadow */
  points [0].x = x;
  points [0].y = y + width - thickness;
  points [1].x = x + width;
  points [1].y = y + half_width;
  points [2].x = x + width;
  points [2].y = y + half_width + thickness;
  points [3].x = x;
  points [3].y = y + width;
  
  XFillPolygon(dpy, window, bottom_gc, points, 4, Convex, CoordModeOrigin);

  /* top border */
  points [0].x = x;
  points [0].y = y;
  points [1].x = x + width;
  points [1].y = y + half_width;
  points [2].x = x + width;
  points [2].y = y + half_width + thickness;
  points [3].x = x;
  points [3].y = y + thickness;
  
  XFillPolygon(dpy, window, top_gc, points, 4, Convex, CoordModeOrigin);
}

static void
toggle_decoration_draw(
		       XlwMenuWidget mw,
		       Window window,
		       int x, int y,
		       unsigned width,
		       Boolean set
) {
  Display *dpy = XtDisplay(mw);
  int thickness = mw->menu.shadow_thickness;
  shadow_type type;
  GC select_gc = mw->menu.select_gc;

  if (set)
    type = SHADOW_IN;
  else
    type = SHADOW_OUT;

  /*
   *    Fill internal area.
   */
  if (set) 
    XFillRectangle(
		   dpy,
		   window,
		   select_gc,
		   x + thickness,
		   y + thickness,
		   width - (2*thickness),
		   width - (2*thickness)
		   );

  shadow_draw(mw, window, x, y, width, width, type);
}

static void
radio_decoration_draw(
		      XlwMenuWidget mw,
		      Window window,
		      int x, int y,
		      unsigned width,
		      Boolean enabled
		      ) {
  Display *dpy = XtDisplay (mw);
  GC top_gc;
  GC bottom_gc;
  GC select_gc = mw->menu.select_gc;
  int thickness = mw->menu.shadow_thickness;
  XPoint points[6];
  int half_width;
  int npoints;

  if (width & 0x1)
    width++;

  half_width = width/2;

  if (enabled)
    {
      top_gc = mw->menu.shadow_bottom_gc;
      bottom_gc = mw->menu.shadow_top_gc;
    }
  else
    {
      top_gc = mw->menu.shadow_top_gc;
      bottom_gc = mw->menu.shadow_bottom_gc;
    }

#if 1
  /*
   *    Draw the bottom first, just incase the regions overlap.
   *    The top should cast the longer shadow.
   */
  points [0].x = x; /* left corner */
  points [0].y = y + half_width;
  points [1].x = x + half_width; /* bottom corner */
  points [1].y = y + width;
  points [2].x = x + half_width; /* bottom inside corner */
  points [2].y = y + width - thickness;
  points [3].x = x + thickness; /* left inside corner */
  points [3].y = y + half_width;

  XFillPolygon (dpy, window, bottom_gc, points, 4, Convex, CoordModeOrigin);

  points [0].x = x + half_width; /* bottom corner */
  points [0].y = y + width;
  points [1].x = x + width; /* right corner */
  points [1].y = y + half_width;
  points [2].x = x + width - thickness; /* right inside corner */
  points [2].y = y + half_width;
  points [3].x = x + half_width; /* bottom inside corner */
  points [3].y = y + width - thickness;

  XFillPolygon (dpy, window, bottom_gc, points, 4, Convex, CoordModeOrigin);

  points [0].x = x; /* left corner */
  points [0].y = y + half_width;
  points [1].x = x + half_width; /* top corner */
  points [1].y = y;
  points [2].x = x + half_width; /* top inside corner */
  points [2].y = y + thickness;
  points [3].x = x + thickness; /* left inside corner */
  points [3].y = y + half_width;

  XFillPolygon (dpy, window, top_gc, points, 4, Convex, CoordModeOrigin);

  points [0].x = x + half_width; /* top corner */
  points [0].y = y;
  points [1].x = x + width; /* right corner */
  points [1].y = y + half_width;
  points [2].x = x + width - thickness; /* right inside corner */
  points [2].y = y + half_width;
  points [3].x = x + half_width; /* top inside corner */
  points [3].y = y + thickness;

  XFillPolygon (dpy, window, top_gc, points, 4, Convex, CoordModeOrigin);
#else
  /*
   *    Draw the bottom first, just incase the regions overlap.
   *    The top should cast the longer shadow.
   */
  npoints = 0;
  points [npoints].x = x; /* left corner */
  points [npoints++].y = y + half_width;
  points [npoints].x = x + half_width; /* bottom corner */
  points [npoints++].y = y + width;
  points [npoints].x = x + width; /* right corner */
  points [npoints++].y = y + half_width;
  points [npoints].x = x + width - thickness; /* right inside corner */
  points [npoints++].y = y + half_width;
  points [npoints].x = x + half_width; /* bottom inside corner */
  points [npoints++].y = y + width - thickness;
  points [npoints].x = x + thickness; /* left inside corner */
  points [npoints++].y = y + half_width;

  XFillPolygon(dpy, window, bottom_gc, 
	       points, npoints, Nonconvex, CoordModeOrigin);

  npoints = 0;

  points [npoints].x = x; /* left corner */
  points [npoints++].y = y + half_width;
  points [npoints].x = x + half_width; /* top corner */
  points [npoints++].y = y;
  points [npoints].x = x + width; /* right corner */
  points [npoints++].y = y + half_width;
  points [npoints].x = x + width - thickness; /* right inside corner */
  points [npoints++].y = y + half_width;
  points [npoints].x = x + half_width; /* top inside corner */
  points [npoints++].y = y + thickness;
  points [npoints].x = x + thickness; /* left inside corner */
  points [npoints++].y = y + half_width;

  XFillPolygon(dpy, window, top_gc, points, npoints, Nonconvex, CoordModeOrigin);
#endif


  /*
   *    Fill internal area.
   */
  if (enabled)
    {
      points [0].x = x + thickness;
      points [0].y = y + half_width;
      points [1].x = x + half_width;
      points [1].y = y + thickness;
      points [2].x = x + width - thickness;
      points [2].y = y + half_width;
      points [3].x = x + half_width;
      points [3].y = y + width - thickness;
      XFillPolygon (dpy,
		    window,
		    select_gc,
		    points,
		    4, 
		    Convex,
		    CoordModeOrigin
		    );
    }
}

static void
separator_decoration_draw(
			  XlwMenuWidget mw,
			  Window window,
			  int x, int y,
			  unsigned width,
			  Boolean vertical,
			  shadow_type type
			  ) {
  Display *dpy = XtDisplay (mw);
  GC top_gc;
  GC bottom_gc;
  unsigned offset = 0;
  unsigned top_line_thickness = 0;
  unsigned bottom_line_thickness = 0;
  Boolean dashed = False;
  int i;

  switch (type) {
  case SHADOW_NO_LINE: /* nothing to do */
    return; 
  case SHADOW_SINGLE_LINE:
    top_gc = bottom_gc = mw->menu.foreground_gc;
    top_line_thickness = 1;
    break;
  case SHADOW_SINGLE_DASHED_LINE:
    top_gc = bottom_gc = mw->menu.foreground_gc;
    top_line_thickness = 1;
    dashed = True;
    break;
  case SHADOW_DOUBLE_LINE:
    top_gc = bottom_gc = mw->menu.foreground_gc;
    top_line_thickness = bottom_line_thickness = 1;
    offset = 1;
    break;
  case SHADOW_DOUBLE_DASHED_LINE:
    top_gc = bottom_gc = mw->menu.foreground_gc;
    top_line_thickness = bottom_line_thickness = 1;
    offset = 1;
    dashed = True;
    break;
  case SHADOW_ETCHED_OUT_DASH:
    top_gc = mw->menu.shadow_top_gc;
    bottom_gc = mw->menu.shadow_bottom_gc;
    top_line_thickness = mw->menu.shadow_thickness/2;
    bottom_line_thickness = mw->menu.shadow_thickness - top_line_thickness;
    dashed = True;
    break;
  case SHADOW_ETCHED_IN_DASH:
    top_gc = mw->menu.shadow_bottom_gc;
    bottom_gc = mw->menu.shadow_top_gc;
    top_line_thickness = mw->menu.shadow_thickness/2;
    bottom_line_thickness = mw->menu.shadow_thickness - top_line_thickness;
    dashed = True;
    break;
  case SHADOW_ETCHED_OUT:
    top_gc = mw->menu.shadow_top_gc;
    bottom_gc = mw->menu.shadow_bottom_gc;
    top_line_thickness = mw->menu.shadow_thickness/2;
    bottom_line_thickness = mw->menu.shadow_thickness - top_line_thickness;
    break;
  case SHADOW_ETCHED_IN:
  default:
    top_gc = mw->menu.shadow_bottom_gc;
    bottom_gc = mw->menu.shadow_top_gc;
    top_line_thickness = mw->menu.shadow_thickness/2;
    bottom_line_thickness = mw->menu.shadow_thickness - top_line_thickness;
    break;
  }
 
  if (dashed) {
    XGCValues values;
    values.line_style = LineOnOffDash;
    if (top_line_thickness > 0)
      XChangeGC(dpy, top_gc, GCLineStyle, &values);
    if (bottom_line_thickness > 0 && bottom_gc != top_gc)
      XChangeGC(dpy, bottom_gc, GCLineStyle, &values);
  }
  
  for (i = 0; i < top_line_thickness; i++)
    XDrawLine(dpy, window, top_gc, x, y + i, x + width, y + i);

  for (i = 0; i < bottom_line_thickness; i++)
    XDrawLine(
	      dpy, window, bottom_gc, 
	      x, y + top_line_thickness + offset + i,
	      x + width, y + top_line_thickness + offset + i
	      );

  if (dashed) {
    XGCValues values;
    values.line_style = LineSolid;
    if (top_line_thickness > 0)
      XChangeGC(dpy, top_gc, GCLineStyle, &values);
    if (bottom_line_thickness > 0 && bottom_gc != top_gc)
      XChangeGC(dpy, bottom_gc, GCLineStyle, &values);
  }
}

static widget_value_type
menu_item_type(widget_value* val) {
  if (val->type != UNSPECIFIED_TYPE)
    return val->type;
  else if (all_dashes_p(val->name))
    return SEPARATOR_TYPE;
  else if (val->name && val->name[0] == '\0') /* push right */
    return PUSHRIGHT_TYPE;
  else if (val->contents) /* cascade */
    return CASCADE_TYPE;
  else if (val->call_data) /* push button */
    return BUTTON_TYPE;
  else
    return TEXT_TYPE;
}

static void
label_button_size(
		  XlwMenuWidget mw,
		  widget_value* val,
		  Boolean in_menubar,
		  unsigned* toggle_width,
		  unsigned* label_width,
		  unsigned* bindings_width,
		  unsigned* height
) {
  *height = (mw->menu.font_ascent + mw->menu.font_descent +
	     2 * mw->menu.vertical_margin +
	     2 * mw->menu.shadow_thickness);
  /* no left column decoration */
  *toggle_width = mw->menu.horizontal_margin + mw->menu.shadow_thickness;;

  *label_width  = string_width (mw, resource_widget_value (mw, val));
  *bindings_width =  mw->menu.horizontal_margin + mw->menu.shadow_thickness;
}

static void
label_button_draw(
		  XlwMenuWidget mw, 
		  widget_value* val,
		  Boolean       in_menubar,
		  Boolean       highlighted,
		  Window        window, 
		  int x, int y, 
		  unsigned width,
		  unsigned height,
		  unsigned label_offset,
		  unsigned binding_tab
) {
  int y_offset = mw->menu.shadow_thickness + mw->menu.vertical_margin;

  if (!label_offset)
    label_offset = mw->menu.shadow_thickness + mw->menu.horizontal_margin;

  /*
   *    Draw the label string.
   */
  string_draw(
	      mw,
	      window,
	      x + label_offset, y + y_offset, 
	      mw->menu.foreground_gc,
	      resource_widget_value(mw, val)
	      );
}

static void
push_button_size(
		 XlwMenuWidget mw,
		 widget_value* val,
		 Boolean in_menubar,
		 unsigned* toggle_width,
		 unsigned* label_width,
		 unsigned* bindings_width,
		 unsigned* height
) {
  /* inherit */
  label_button_size(
		    mw, val, in_menubar,
		    toggle_width, label_width, bindings_width,
		    height
		    );
  
  /* key bindings to display? */
  if (!in_menubar && val->key) {
    int w;
#ifdef USE_MOTIF
    XmString key = XmStringCreateLtoR (val->key, XmSTRING_DEFAULT_CHARSET);
    w = string_width(mw, key);
    XmStringFree (key);
#else
    char *key = val->key;
    w = string_width(mw, key);
#endif
    *bindings_width += w + mw->menu.column_spacing;
  }
}

static void
push_button_draw(
		 XlwMenuWidget mw, 
		 widget_value* val,
		 Boolean       in_menubar,
		 Boolean       highlighted,
		 Window        window, 
		 int x, int y, 
		 unsigned width, unsigned height,
		 unsigned      label_offset,
		 unsigned      binding_offset
) {
  int y_offset = mw->menu.shadow_thickness + mw->menu.vertical_margin;
  GC gc;
  shadow_type type;
  Boolean menu_pb = in_menubar && (menu_item_type(val) == BUTTON_TYPE);

  /*
   *    Draw the label string.
   */
  if (!label_offset)
    label_offset = mw->menu.shadow_thickness + mw->menu.horizontal_margin;
  
  if (menu_pb) {
    if (val->enabled)
      gc = mw->menu.button_gc;
    else
      gc = mw->menu.inactive_button_gc;
  } else {
    if (val->enabled)
      gc = mw->menu.foreground_gc;
    else
      gc = mw->menu.inactive_gc;
  }

  string_draw(
	      mw,
	      window,
	      x + label_offset, y + y_offset, 
	      gc,
	      resource_widget_value(mw, val)
	      );

  /*
   *    Draw the keybindings
   */
  if (val->key) {
    if (!binding_offset) {
      unsigned s_width = string_width(mw, resource_widget_value(mw, val));
      binding_offset = label_offset + s_width +  mw->menu.shadow_thickness;
    }
    binding_draw(mw, window,
		 x + binding_offset + mw->menu.column_spacing,
		 y + y_offset, gc, val->key);
  }

  /*
   *    Draw the shadow
   */
  if (menu_pb) {
    if (highlighted)
      type = SHADOW_OUT;
    else 
      type = SHADOW_ETCHED_IN;
  } else {
    if (highlighted)
      type = SHADOW_OUT;
    else 
      type = SHADOW_BACKGROUND;
  }

  shadow_draw(mw, window, x, y, width, height, type);
}

static unsigned
arrow_decoration_height(XlwMenuWidget mw) {

  unsigned result = (mw->menu.font_ascent + mw->menu.font_descent)/2;
  
  result += 2 * mw->menu.shadow_thickness;

  if (result > (mw->menu.font_ascent + mw->menu.font_descent))
    result = mw->menu.font_ascent + mw->menu.font_descent;

  return result;
}

static void
cascade_button_size(
		    XlwMenuWidget mw,
		    widget_value* val,
		    Boolean in_menubar,
		    unsigned* toggle_width,
		    unsigned* label_width,
		    unsigned* arrow_width,
		    unsigned* height
) {
  /* inherit */
  label_button_size(
		    mw, val, in_menubar,
		    toggle_width, label_width, arrow_width,
		    height
		    );
  /* we have a pull aside arrow */
  if (!in_menubar) {
    *arrow_width += arrow_decoration_height(mw) + mw->menu.column_spacing;
  }
}

static void
cascade_button_draw(
		    XlwMenuWidget mw, 
		    widget_value* val,
		    Boolean       in_menubar,
		    Boolean       highlighted,
		    Window        window, 
		    int x, int y, 
		    unsigned width, unsigned height,
		    unsigned      label_offset,
		    unsigned      binding_offset
) {
  shadow_type type;

  /*
   *    Draw the label string.
   */
  label_button_draw(mw, val, in_menubar, highlighted,
	     window, x, y, width, height, label_offset, binding_offset);

  /*
   *    Draw the pull aside arrow
   */
  if (!in_menubar && val->contents) {
    int y_offset;
    unsigned arrow_height = arrow_decoration_height(mw);

    y_offset = mw->menu.shadow_thickness + mw->menu.vertical_margin +
      (mw->menu.font_ascent+mw->menu.font_descent - arrow_height)/2;

    if (!binding_offset) {
      unsigned s_width = string_width(mw, resource_widget_value(mw, val));

      if (!label_offset)
	label_offset = mw->menu.shadow_thickness + mw->menu.horizontal_margin;

      binding_offset = label_offset + s_width +  mw->menu.shadow_thickness;
    }

    arrow_decoration_draw(
			  mw,
			  window,
			  x + binding_offset + mw->menu.column_spacing,
			  y + y_offset,
			  arrow_height,
			  highlighted
			  );
  }

  /*
   *    Draw the shadow
   */
  if (highlighted)
    type = SHADOW_OUT;
  else
    type = SHADOW_BACKGROUND;

  shadow_draw(mw, window, x, y, width, height, type);
}

static unsigned
toggle_decoration_height(XlwMenuWidget mw) {
  unsigned rv;
  if (mw->menu.indicator_size > 0)
    rv = mw->menu.indicator_size;
  else
    rv = mw->menu.font_ascent;

  if (rv > (mw->menu.font_ascent+mw->menu.font_descent))
    rv = mw->menu.font_ascent+mw->menu.font_descent;

  return rv;
}

static void
toggle_button_size(
		   XlwMenuWidget mw,
		   widget_value* val,
		   Boolean in_menubar,
		 unsigned* toggle_width,
		 unsigned* label_width,
		 unsigned* bindings_width,
		 unsigned* height
) {
  /* inherit */
  push_button_size(
		   mw, val, in_menubar,
		   toggle_width, label_width, bindings_width,
		   height
		   );
  /* we have a toggle */
  *toggle_width += toggle_decoration_height(mw) + mw->menu.column_spacing;
}

static void
toggle_button_draw(
		   XlwMenuWidget mw, 
		   widget_value* val,
		   Boolean       in_menubar,
		   Boolean highlighted,
		   Window        window, 
		   int x, int y, 
		   unsigned width, unsigned height,
		   unsigned      label_tab,
		   unsigned      binding_tab
) {
  int x_offset;
  int y_offset;
  unsigned t_height = toggle_decoration_height(mw);
  
  /*
   *    Draw a toggle.
   */
  x_offset = mw->menu.shadow_thickness + mw->menu.horizontal_margin;
  y_offset = mw->menu.shadow_thickness + mw->menu.vertical_margin;
  y_offset += (mw->menu.font_ascent + mw->menu.font_descent - t_height)/2;

  toggle_decoration_draw(mw, window, x + x_offset, y + y_offset, t_height, val->selected);

  /*
   *    Draw the pushbutton parts.
   */
  push_button_draw(mw, val, in_menubar, highlighted, window, x, y, width, height, label_tab, binding_tab);
}

static unsigned
radio_decoration_height(XlwMenuWidget mw) {
  return toggle_decoration_height(mw);
}

static void
radio_button_draw(
		  XlwMenuWidget mw, 
		  widget_value* val,
		  Boolean       in_menubar,
		  Boolean       highlighted,
		  Window        window, 
		  int x, int y, 
		  unsigned width, unsigned height,
		  unsigned      label_tab,
		  unsigned      binding_tab
) {
  int x_offset;
  int y_offset;
  unsigned r_height = radio_decoration_height(mw);
  
  /*
   *    Draw a toggle.
   */
  x_offset = mw->menu.shadow_thickness + mw->menu.horizontal_margin;
  y_offset = mw->menu.shadow_thickness + mw->menu.vertical_margin;
  y_offset += (mw->menu.font_ascent + mw->menu.font_descent - r_height)/2;

  radio_decoration_draw(mw, window, x + x_offset, y + y_offset, r_height, val->selected);

  /*
   *    Draw the pushbutton parts.
   */
  push_button_draw(mw, val, in_menubar, highlighted, window, x, y, width, height, label_tab, binding_tab);
}

static struct _shadow_names {
  char*       name;
  shadow_type type;
} shadow_names[] = {
  { "singleLine", SHADOW_SINGLE_LINE },
  { "doubleLine", SHADOW_DOUBLE_LINE },
  { "singleDashedLine", SHADOW_SINGLE_DASHED_LINE },
  { "doubleDashedLine", SHADOW_DOUBLE_DASHED_LINE },
  { "noLine", SHADOW_NO_LINE },
  { "shadowEtchedIn", SHADOW_ETCHED_IN },
  { "shadowEtchedOut", SHADOW_ETCHED_OUT },
  { "shadowEtchedInDash", SHADOW_ETCHED_IN_DASH },
  { "shadowEtchedOutDash", SHADOW_ETCHED_OUT_DASH }
};

static shadow_type
separator_type(char* name) {
  int i;

  if (name) {
    for (i = 0; i < XtNumber(shadow_names); i++ ) {
      if (strcmp(name, shadow_names[i].name) == 0)
	return shadow_names[i].type;
    }
  }
  return SHADOW_BACKGROUND;
}

static unsigned
separator_decoration_height(XlwMenuWidget mw, widget_value* val) {

  switch (separator_type(val->value)) {
  case SHADOW_NO_LINE:
  case SHADOW_SINGLE_LINE:
  case SHADOW_SINGLE_DASHED_LINE:
    return 1;
  case SHADOW_DOUBLE_LINE:
  case SHADOW_DOUBLE_DASHED_LINE:
    return 3;
  case SHADOW_ETCHED_OUT:
  case SHADOW_ETCHED_IN:
  default:
    return mw->menu.shadow_thickness;
  }
}

static void
separator_size(
	       XlwMenuWidget mw,
	       widget_value* val,
	       Boolean in_menubar,
	       unsigned* toggle_width,
	       unsigned* label_width,
	       unsigned* rest_width,
	       unsigned* height
) {
  *height = separator_decoration_height(mw, val);
  *label_width = 1;
  *toggle_width = *rest_width = 0;
}

static void
separator_draw(
	       XlwMenuWidget mw, 
	       widget_value* val,
	       Boolean       in_menubar,
	       Boolean       highlighted,
	       Window        window, 
	       int x, int y, 
	       unsigned width, unsigned height,
	       unsigned      label_tab,
	       unsigned      binding_tab
) {
  unsigned sep_width;

  if (in_menubar)
    sep_width = height;
  else
    sep_width = width;

  separator_decoration_draw(
			    mw,
			    window,
			    x,
			    y,
			    sep_width,
			    in_menubar,
			    separator_type(val->value)
			    );
}

static void
pushright_size(
	       XlwMenuWidget mw,
	       widget_value* val,
	       Boolean in_menubar,
	       unsigned* toggle_width,
	       unsigned* label_width,
	       unsigned* rest_width,
	       unsigned* height
) {
  *height = *label_width = *toggle_width = *rest_width = 0;
}

static void
size_menu_item(
	       XlwMenuWidget mw,
	       widget_value* val,
	       int horizontal,
	       unsigned* toggle_width,
	       unsigned* label_width,
	       unsigned* rest_width,
	       unsigned* height
) {

  void (*function_ptr)(
		       XlwMenuWidget mw,
		       widget_value* val,
		       Boolean in_menubar,
		       unsigned* toggle_width,
		       unsigned* label_width,
		       unsigned* rest_width,
		       unsigned* height
		       );
  switch (menu_item_type(val)) {
  case TOGGLE_TYPE:
  case RADIO_TYPE:
    function_ptr = toggle_button_size;
    break;
  case SEPARATOR_TYPE:
    function_ptr = separator_size;
    break;
  case CASCADE_TYPE:
    function_ptr = cascade_button_size;
    break;
  case BUTTON_TYPE:
    function_ptr = push_button_size;
    break;
  case PUSHRIGHT_TYPE:
    function_ptr = pushright_size;
    break;
  case TEXT_TYPE:
  default:
    function_ptr = label_button_size;
    break;
  }

  (*function_ptr)(
		  mw,
		  val,
		  horizontal,
		  toggle_width,
		  label_width,
		  rest_width,
		  height
		  );
}

static void
display_menu_item(
		  XlwMenuWidget mw,
		  widget_value* val,
		  window_state* ws,
		  XPoint* where,
		  Boolean highlighted,
		  Boolean horizontal,
		  Boolean just_compute
) {
  
  int x = where->x /* + mw->menu.shadow_thickness */ ;
  int y = where->y /* + mw->menu.shadow_thickness */ ;
  unsigned toggle_width;
  unsigned label_width;
  unsigned binding_width;
  unsigned width;
  unsigned height;
  unsigned label_tab;
  unsigned binding_tab;
  void     (*function_ptr)(
			   XlwMenuWidget mw,
			   widget_value* val,
			   Boolean in_menubar,
			   Boolean highlighted,
			   Window        window, 
			   int x, int y, 
			   unsigned width, unsigned height,
			   unsigned      label_tab,
			   unsigned      binding_tab
			   );

  size_menu_item (
		  mw, val, horizontal,
		  &toggle_width, &label_width, &binding_width, &height
		  );

  if (horizontal) {
    width = toggle_width + label_width + binding_width;
    height = ws->height - 2 * mw->menu.shadow_thickness;
    } else {
    width = ws->width - 2 * mw->menu.shadow_thickness;
    toggle_width = ws->toggle_width;
    label_width = ws->label_width;
  }
  
  where->x += width;
  where->y += height;

  if (just_compute)
    return;

  label_tab = toggle_width;
  binding_tab = toggle_width + label_width;

  switch (menu_item_type(val)) {
  case TOGGLE_TYPE:
    function_ptr = toggle_button_draw;
    break;
  case RADIO_TYPE:
    function_ptr = radio_button_draw;
    break;
  case SEPARATOR_TYPE:
    function_ptr = separator_draw;
    break;
  case CASCADE_TYPE:
    function_ptr = cascade_button_draw;
    break;
  case BUTTON_TYPE:
    function_ptr = push_button_draw;
    break;
  case TEXT_TYPE:
    function_ptr = label_button_draw;
    break;
  default: /* do no drawing */
    return;
  }

  (*function_ptr)(
		  mw,
		  val,
		  horizontal,
		  highlighted,
		  ws->window, 
		  x, y, 
		  width, height,
		  label_tab,
		  binding_tab
		  );
}

static void
size_menu (XlwMenuWidget mw, int level)
{
  unsigned      toggle_width;
  unsigned	label_width;
  unsigned	rest_width;
  unsigned	height;
  unsigned	max_toggle_width = 0;
  unsigned	max_label_width = 0;
  unsigned	max_rest_width = 0;
  unsigned	max_height = 0;
  int		horizontal_p = mw->menu.horizontal && (level == 0);
  widget_value*	val;
  window_state*	ws;

  if (level >= mw->menu.old_depth)
    abort ();

  ws = &mw->menu.windows [level];  

  for (val = mw->menu.old_stack [level]->contents; val; val = val->next)
    {
      size_menu_item (
		      mw,
		      val,
		      horizontal_p,
		      &toggle_width,
		      &label_width,
		      &rest_width,
		      &height
		      );
      if (horizontal_p)
	{
	  max_label_width += toggle_width + label_width + rest_width;
	  if (height > max_height)
	    max_height = height;
	}
      else
	{
	  if (toggle_width > max_toggle_width)
	    max_toggle_width = toggle_width;
	  if (label_width > max_label_width)
	    max_label_width = label_width;
	  if (rest_width > max_rest_width)
	    max_rest_width = rest_width;
	  max_height += height;
	}
    }
  
  ws->height = max_height;
  ws->width = max_label_width + max_rest_width + max_toggle_width;
  ws->toggle_width = max_toggle_width;
  ws->label_width = max_label_width;

  ws->width += 2 * mw->menu.shadow_thickness;
  ws->height += 2 * mw->menu.shadow_thickness;
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

  where.x = mw->menu.shadow_thickness;
  where.y = mw->menu.shadow_thickness;

  ws = &mw->menu.windows [level];
  for (val = mw->menu.old_stack [level]->contents; val; val = val->next)
    {
      XPoint start;

      /* If this is the partition (the dummy item which says that menus
	 after this should be flushright) then figure out how big the
	 following items are.  This means we walk down the tail of the
	 list twice, but that's no big deal - it's short.
       */
      if (horizontal_p && (menu_item_type(val) == PUSHRIGHT_TYPE))
	{
	  widget_value *rest;
	  XPoint flushright_size;
	  int new_x;
	  flushright_size.x = 0;
	  flushright_size.y = 0;
	  for (rest = val; rest; rest = rest->next)
	    display_menu_item (mw, rest, ws, &flushright_size,
			       highlighted_p, horizontal_p, True);
	  new_x = ws->width - (flushright_size.x + mw->menu.shadow_thickness);
	  if (new_x > where.x)
	    where.x = new_x;
	  /* We know what we need; don't draw this item. */
	  continue;
	}

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

      start.x = where.x;
      start.y = where.y;
      display_menu_item (mw, val, ws, &where, highlighted_p, horizontal_p,
			 just_compute_this_one_p);

      if (highlighted_p && highlighted_pos)
	{
	  if (horizontal_p)
	    highlighted_pos->y = ws->height;
	  else
	    highlighted_pos->x = ws->width;
	}

      if (hit && !*hit_return && !all_dashes_p(val->name))
	{
	  if (horizontal_p && hit->x > start.x && hit->x < where.x)
	    *hit_return = val;
	  else if (!horizontal_p && hit->y > start.y && hit->y < where.y)
	    *hit_return = val;
	}

      if (horizontal_p)
	where.y = mw->menu.shadow_thickness;
      else
	where.x = mw->menu.shadow_thickness;
    }

  /* Draw slab edges around menu */
  if (!just_compute_p)
    shadow_draw(mw, ws->window, 0, 0, ws->width, ws->height, SHADOW_OUT);
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
    ExposureMask | ButtonMotionMask | PointerMotionHintMask
      | ButtonReleaseMask | ButtonPressMask;
  xswa.cursor = mw->menu.cursor_shape;
  mask = CWSaveUnder | CWOverrideRedirect | CWBackPixel | CWBorderPixel
    | CWEventMask | CWCursor;

  if (mw->menu.use_backing_store)
    {
      xswa.backing_store = Always;
      mask |= CWBackingStore;
    }
  
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

/* Make the window fit in the screen */
static void
fit_to_screen (XlwMenuWidget mw, window_state* ws, window_state* previous_ws,
	       Boolean horizontal_p)
{
  int screen_width = WidthOfScreen (XtScreen (mw));
  int screen_height = HeightOfScreen (XtScreen (mw));

  if (ws->x < 0)
    ws->x = 0;
  else if ((int) (ws->x + ws->width) > screen_width)
    {
      if (!horizontal_p)
	ws->x = previous_ws->x - ws->width;
      else
	ws->x = screen_width - ws->width;
    }
  if (ws->y < 0)
    ws->y = 0;
  else if ((int) (ws->y + ws->height) > screen_height)
    {
      if (horizontal_p)
	{
	  /* A pulldown must either be entirely above or below the menubar.
	     If we're here, the pulldown doesn't fit below the menubar, so
             let's determine if it will fit above the menubar.
             Only put it above if there is more room above than below.
	     Note shadow_thickness offset to allow for slab surround.
	     */
	  if (ws->y > (screen_height / 2))
	    ws->y = previous_ws->y - ws->height + mw->menu.shadow_thickness;
	}
      else
	{
	  ws->y = screen_height - ws->height;
	   /* if it's taller than the screen, display the topmost part
	      that will fit, beginning at the top of the screen. */
	  if (ws->y < 0)
	    ws->y = 0;
	}
    }
}

/* Updates old_stack from new_stack and redisplays. */
static void
remap_menubar (XlwMenuWidget mw)
{
  int i;
  int last_same;
  XPoint selection_position;
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

      ws->x = previous_ws->x + selection_position.x;
      ws->y = previous_ws->y + selection_position.y;

      /* take into account the slab around the new menu */
      ws->y -= mw->menu.shadow_thickness;

      size_menu (mw, i);

      fit_to_screen (mw, ws, previous_ws, mw->menu.horizontal && i == 1);

      XClearWindow (XtDisplay (mw), ws->window);
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
  return (x < ev->x_root && ev->x_root < (int) (x + ws->width)
	  && y < ev->y_root && ev->y_root < (int) (y + ws->height));
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
make_drawing_gcs (XlwMenuWidget mw)
{
  XGCValues xgcv;
  unsigned long flags = (GCFont | GCForeground | GCBackground);

#ifdef USE_MOTIF
  xgcv.font = default_font_of_font_list (mw->menu.font_list)->fid;
#else
  xgcv.font = mw->menu.font->fid;
#endif

  xgcv.foreground = mw->core.background_pixel;
  xgcv.background = mw->menu.foreground;
  mw->menu.background_gc = XtGetGC ((Widget)mw, flags, &xgcv);

  xgcv.foreground = mw->menu.foreground;
  xgcv.background = mw->core.background_pixel;
  mw->menu.foreground_gc = XtGetGC ((Widget)mw, flags, &xgcv);

  if (mw->menu.select_color != (Pixel)-1) {
    xgcv.foreground = mw->menu.select_color;
  } else {
    Display *dpy = XtDisplay(mw);
    if (CellsOfScreen(DefaultScreenOfDisplay(dpy)) <= 2) { /* mono */
      xgcv.foreground = mw->menu.foreground;
    } else { /* color */
      XColor xcolor;
      Display *dpy = XtDisplay ((Widget) mw);
      Colormap cmap = DefaultColormapOfScreen (XtScreen ((Widget) mw));
      xcolor.pixel = mw->core.background_pixel;
      XQueryColor (dpy, cmap, &xcolor);
      xcolor.red   *= 0.85;
      xcolor.green *= 0.85;
      xcolor.blue  *= 0.85;
      if (XAllocColor (dpy, cmap, &xcolor))
	xgcv.foreground = xcolor.pixel;
    }
  }
  xgcv.background = mw->core.background_pixel;
  mw->menu.select_gc = XtGetGC ((Widget)mw, flags, &xgcv);

  xgcv.foreground = mw->menu.foreground;
  xgcv.background = mw->core.background_pixel;
  xgcv.fill_style = FillStippled;
  xgcv.stipple = mw->menu.gray_pixmap;
  mw->menu.inactive_gc = XtGetGC ((Widget)mw,
				  (flags | GCFillStyle | GCStipple),
				  &xgcv);

  xgcv.foreground = mw->menu.button_foreground;
  xgcv.background = mw->core.background_pixel;
  mw->menu.button_gc = XtGetGC ((Widget)mw, flags, &xgcv);
  
  xgcv.fill_style = FillStippled;
  xgcv.stipple = mw->menu.gray_pixmap;
  mw->menu.inactive_button_gc = XtGetGC ((Widget)mw,
				  (flags | GCFillStyle | GCStipple),
					 &xgcv);
}

static void
release_drawing_gcs (XlwMenuWidget mw)
{
  XtReleaseGC ((Widget) mw, mw->menu.foreground_gc);
  XtReleaseGC ((Widget) mw, mw->menu.button_gc);
  XtReleaseGC ((Widget) mw, mw->menu.inactive_gc);
  XtReleaseGC ((Widget) mw, mw->menu.inactive_button_gc);
  XtReleaseGC ((Widget) mw, mw->menu.background_gc);
  XtReleaseGC ((Widget) mw, mw->menu.select_gc);
  /* let's get some segvs if we try to use these... */
  mw->menu.foreground_gc = (GC) -1;
  mw->menu.button_gc = (GC) -1;
  mw->menu.inactive_gc = (GC) -1;
  mw->menu.inactive_button_gc = (GC) -1;
  mw->menu.background_gc = (GC) -1;
  mw->menu.select_gc = (GC) -1;
}

#define MINL(x,y) ((((unsigned long) (x)) < ((unsigned long) (y))) \
		   ? ((unsigned long) (x)) : ((unsigned long) (y)))

static void
make_shadow_gcs (XlwMenuWidget mw)
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
      /* don't overflow/wrap! */
      topc.red   = MINL (65535, topc.red   * 1.2);
      topc.green = MINL (65535, topc.green * 1.2);
      topc.blue  = MINL (65535, topc.blue  * 1.2);
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
      if (bot_avg > top_avg)
	{
	  Pixel tmp = mw->menu.top_shadow_color;
	  mw->menu.top_shadow_color = mw->menu.bottom_shadow_color;
	  mw->menu.bottom_shadow_color = tmp;
	}
      else if (topc.pixel == botc.pixel)
	{
	  if (botc.pixel == mw->menu.foreground)
	    mw->menu.top_shadow_color = mw->core.background_pixel;
	  else
	    mw->menu.bottom_shadow_color = mw->menu.foreground;
	}
    }

  if (!mw->menu.top_shadow_pixmap &&
      mw->menu.top_shadow_color == mw->core.background_pixel)
    {
      mw->menu.top_shadow_pixmap = mw->menu.gray_pixmap;
      mw->menu.top_shadow_color = mw->menu.foreground;
    }
  if (!mw->menu.bottom_shadow_pixmap &&
      mw->menu.bottom_shadow_color == mw->core.background_pixel)
    {
      mw->menu.bottom_shadow_pixmap = mw->menu.gray_pixmap;
      mw->menu.bottom_shadow_color = mw->menu.foreground;
    }

  xgcv.fill_style = FillOpaqueStippled;
  xgcv.foreground = mw->menu.top_shadow_color;
  xgcv.background = mw->core.background_pixel;
  xgcv.stipple = mw->menu.top_shadow_pixmap;
  pm = (xgcv.stipple ? GCStipple|GCFillStyle : 0);
  mw->menu.shadow_top_gc = XtGetGC((Widget)mw, GCForeground|GCBackground|pm, &xgcv);

  xgcv.foreground = mw->menu.bottom_shadow_color;
  xgcv.stipple = mw->menu.bottom_shadow_pixmap;
  pm = (xgcv.stipple ? GCStipple|GCFillStyle : 0);
  mw->menu.shadow_bottom_gc = XtGetGC ((Widget)mw, GCForeground|GCBackground|pm, &xgcv);
}


static void
release_shadow_gcs (XlwMenuWidget mw)
{
  XtReleaseGC ((Widget) mw, mw->menu.shadow_top_gc);
  XtReleaseGC ((Widget) mw, mw->menu.shadow_bottom_gc);
}

static void
extract_font_extents (XlwMenuWidget mw)
{
#ifdef USE_MOTIF
  /* Find the maximal ascent/descent of the fonts in the font list
     so that all menu items can be the same height... */
  mw->menu.font_ascent  = 0;
  mw->menu.font_descent = 0;
  
  {
    XmFontContext context;
    XmStringCharSet charset;
    XFontStruct *font;

    if (! XmFontListInitFontContext (&context, mw->menu.font_list))
      abort ();
    while (XmFontListGetNextFont (context, &charset, &font))
      {
	if (font->ascent > mw->menu.font_ascent)
	  mw->menu.font_ascent = font->ascent;
	if (font->descent > mw->menu.font_descent)
	  mw->menu.font_descent = font->descent;
	XtFree (charset);
      }
    XmFontListFreeFontContext (context);
  }
#else
  mw->menu.font_ascent  = mw->menu.font->ascent;
  mw->menu.font_descent = mw->menu.font->descent;
#endif
}


#ifdef USE_MOTIF
static XFontStruct *
default_font_of_font_list (XmFontList font_list)
{
  XFontStruct *font = 0;
# if 0
  /* Xm/Label.c does this: */
  _XmFontListGetDefaultFont (font_list, &font);
# else  /* !0 */
  {
    XmFontContext context;
    XmStringCharSet charset;
    if (! XmFontListInitFontContext (&context, font_list))
      abort ();
    if (! XmFontListGetNextFont (context, &charset, &font))
      abort ();
    XmFontListFreeFontContext (context);
    XtFree (charset);
  }
# endif /* !0 */

  if (! font) abort ();
  return font;
}
#endif

static void
XlwMenuInitialize (Widget request, Widget new, ArgList args,
		   Cardinal *num_args)
{
  /* Get the GCs and the widget size */
  XlwMenuWidget mw = (XlwMenuWidget)new;
  
  XSetWindowAttributes xswa;
  int mask;
  
  Window window = RootWindowOfScreen (DefaultScreenOfDisplay (XtDisplay (mw)));
  Display* display = XtDisplay (mw);
  
/*  mw->menu.cursor = XCreateFontCursor (display, mw->menu.cursor_shape); */
  mw->menu.cursor = mw->menu.cursor_shape;
  
  mw->menu.gray_pixmap = XCreatePixmapFromBitmapData (display, window,
						      gray_bits, gray_width,
						      gray_height, 1, 0, 1);
  
#ifdef USE_MOTIF
  /* The menu.font_list slot came from the *fontList resource (Motif standard.)
     The menu.font_list_2 slot came from the *font resource, for backward
     compatibility with older versions of this code, and consistency with the
     rest of emacs.  If both font and fontList are specified, we use font.
     If only one is specified, we use that.  If neither are specified, we
     use the "fallback" value.  What a kludge!!!

     Note that this has the bug that a more general wildcard like "*fontList:"
     will override a more specific resoure like "Emacs*menubar.font:".  But
     I can't think of a way around that.
   */
  if (mw->menu.font_list)	  /* if *fontList is specified, use that */
    ;
  else if (mw->menu.font_list_2)  /* else if *font is specified, use that */
    mw->menu.font_list = mw->menu.font_list_2;
  else				  /* otherwise use default */
    mw->menu.font_list = mw->menu.fallback_font_list;
#endif

  make_drawing_gcs (mw);
  make_shadow_gcs (mw);
  extract_font_extents (mw);

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
  if (mw->menu.use_backing_store)
    {
      xswa.backing_store = Always;
      mask |= CWBackingStore;
    }
  XChangeWindowAttributes (XtDisplay (w), XtWindow (w), mask, &xswa);

  mw->menu.windows [0].window = XtWindow (w);
  mw->menu.windows [0].x = w->core.x;
  mw->menu.windows [0].y = w->core.y;
  mw->menu.windows [0].width = w->core.width;
  mw->menu.windows [0].height = w->core.height;
}

/* Only the toplevel menubar/popup is a widget so it's the only one that
   receives expose events through Xt.  So we repaint all the other panes
   when receiving an Expose event. */
static void 
XlwMenuRedisplay (Widget w, XEvent* ev, Region region)
{
  XlwMenuWidget mw = (XlwMenuWidget)w;
  int i;

  for (i = 0; i < mw->menu.old_depth; i++)
    display_menu (mw, i, False, NULL, NULL, NULL, NULL, NULL);
}

static void 
XlwMenuDestroy (Widget w)
{
  int i;
  XlwMenuWidget mw = (XlwMenuWidget) w;

  release_drawing_gcs (mw);
  release_shadow_gcs (mw);

  /* this doesn't come from the resource db but is created explicitly
     so we must free it ourselves. */
  XFreePixmap (XtDisplay (mw), mw->menu.gray_pixmap);
  mw->menu.gray_pixmap = (Pixmap) -1;

  /* Don't free mw->menu.contents because that comes from our creator.
     The `*_stack' elements are just pointers into `contents' so leave
     that alone too.  But free the stacks themselves. */
  if (mw->menu.old_stack) XtFree ((char *) mw->menu.old_stack);
  if (mw->menu.new_stack) XtFree ((char *) mw->menu.new_stack);

  /* Remember, you can't free anything that came from the resource
     database.  This includes:
         mw->menu.cursor
         mw->menu.top_shadow_pixmap
         mw->menu.bottom_shadow_pixmap
         mw->menu.font
         mw->menu.font_set
     Also the color cells of top_shadow_color, bottom_shadow_color,
     foreground, and button_foreground will never be freed until this
     client exits.  Nice, eh?
   */

  /* start from 1 because the one in slot 0 is w->core.window */
  for (i = 1; i < mw->menu.windows_length; i++)
    XDestroyWindow (XtDisplay (mw), mw->menu.windows [i].window);
  if (mw->menu.windows)
    XtFree ((char *) mw->menu.windows);
}

static Boolean 
XlwMenuSetValues (Widget current, Widget request, Widget new)
{
  XlwMenuWidget oldmw = (XlwMenuWidget)current;
  XlwMenuWidget newmw = (XlwMenuWidget)new;
  Boolean redisplay = False;
  int i;

  if (newmw->menu.contents
      && newmw->menu.contents->contents
      && newmw->menu.contents->contents->change >= VISIBLE_CHANGE)
    redisplay = True;

  if (newmw->core.background_pixel != oldmw->core.background_pixel
      || newmw->menu.foreground != oldmw->menu.foreground
      /* For the XEditResource protocol, which may want to change the font. */
#ifdef USE_MOTIF
      || newmw->menu.font_list != oldmw->menu.font_list
      || newmw->menu.font_list_2 != oldmw->menu.font_list_2
      || newmw->menu.fallback_font_list != oldmw->menu.fallback_font_list
#else
      || newmw->menu.font != oldmw->menu.font
#endif
      )
    {
      release_drawing_gcs (newmw);
      make_drawing_gcs (newmw);
      redisplay = True;
      
      for (i = 0; i < oldmw->menu.windows_length; i++)
	{
	  XSetWindowBackground (XtDisplay (oldmw),
				oldmw->menu.windows [i].window,
				newmw->core.background_pixel);
	  /* clear windows and generate expose events */
	  XClearArea (XtDisplay (oldmw), oldmw->menu.windows[i].window,
		      0, 0, 0, 0, True);
	}
    }

  return redisplay;
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
handle_single_motion_event (XlwMenuWidget mw, XMotionEvent* ev)
{
  widget_value*	val;
  int 		level;

  if (!map_event_to_widget_value (mw, ev, &val, &level))
    pop_new_stack_if_no_contents (mw);
  else
    set_new_state (mw, val, level);
  remap_menubar (mw);
  
  /* Sync with the display.  Makes it feel better on X terms. */
  XSync (XtDisplay (mw), False);
}

static void
handle_motion_event (XlwMenuWidget mw, XMotionEvent* ev)
{
  int x = ev->x_root;
  int y = ev->y_root;
  int state = ev->state;

  handle_single_motion_event (mw, ev);

  /* allow motion events to be generated again */
  if (ev->is_hint
      && XQueryPointer (XtDisplay (mw), ev->window,
			&ev->root, &ev->subwindow,
			&ev->x_root, &ev->y_root,
			&ev->x, &ev->y,
			&ev->state)
      && ev->state == state
      && (ev->x_root != x || ev->y_root != y))
    handle_single_motion_event (mw, ev);
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
		 (ButtonMotionMask | PointerMotionHintMask | ButtonReleaseMask
		  | ButtonPressMask),
		 GrabModeAsync, GrabModeAsync, None, mw->menu.cursor_shape,
		 event->time);

  mw->menu.windows [0].x = x + borderwidth;
  mw->menu.windows [0].y = y + borderwidth;

  handle_motion_event (mw, (XMotionEvent*)event);
}

/*
 *    This is a horrible function which should not be needed.
 *    use it to put the resize method back the way the XlwMenu
 *    class initializer put it. Motif screws with this when
 *    the XlwMenu class gets instantiated.
 */
void
xlw_unmunge_class_resize(Widget w) {
  if (w->core.widget_class->core_class.resize != XlwMenuResize)
    w->core.widget_class->core_class.resize = XlwMenuResize;
}

