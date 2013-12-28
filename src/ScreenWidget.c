/* The emacs screen widget.
   Copyright (C) 1992, 1993 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <stdio.h>
#include "config.h"
#include "lisp.h"
#include "xterm.h"
#include "dispextern.h"
#include "screen.h"

#include <X11/StringDefs.h>
#include <X11/IntrinsicP.h>
#include <X11/cursorfont.h>
#include "ScreenWidgetP.h"
#include <X11/Shell.h>

/* This sucks: this is the first default that x-faces.el tries.  This won't
   be used unless neither the "Emacs.EmacsScreen.font" resource nor the
   "Emacs.EmacsScreen.default.attributeFont" resource is set; the screen
   may have the wrong default size if this font doesn't exist, but some other
   font that x-faces.el does.  The workaround is to specify some font in the
   resource database; I don't know a solution other than duplicating the font-
   searching code from x-faces.el in this file.

   This also means that if "Emacs.EmacsScreen.font" is specified as a non-
   existent font, then Xt is going to substitute "XtDefaultFont" for it,
   which is a different size than this one.  The solution for this is to
   make x-faces.el try to use XtDefaultFont.  The problem with that is that
   XtDefaultFont is almost certainly variable-width.
 */
#define DEFAULT_FACE_FONT "-*-courier-medium-r-*-*-*-120-*-*-*-*-iso8859-*"

#define COLOR_SCREEN_P(w) (XCellsOfScreen (XtScreen (w)) > 2)

void emacs_Xt_focus_event_handler ();

static void EmacsScreenInitialize (Widget, Widget, ArgList, Cardinal *);
static void EmacsScreenDestroy (Widget);
static void EmacsScreenRealize (Widget, XtValueMask*, XSetWindowAttributes*);
void EmacsScreenResize (Widget widget);
static Boolean EmacsScreenSetValues (Widget, Widget, Widget,
				     ArgList, Cardinal *);
static XtGeometryResult EmacsScreenQueryGeometry (Widget, XtWidgetGeometry*,
						  XtWidgetGeometry*);

static void
key_press (Widget w, XEvent* event, String *params, Cardinal *n_params);
static void
emacs_screen_focus_handler (Widget w, XEvent *event, String *params,
			    Cardinal *n_params);


#undef XtOffset
#define XtOffset(p_type,field) \
	((Cardinal) (((char *) (&(((p_type)0)->field))) - ((char *)0)))
#define offset(field) XtOffset(EmacsScreenWidget, emacs_screen.field)

static XtResource resources[] = {
  {XtNminibuffer, XtCMinibuffer, XtRInt, sizeof (int),
     offset (minibuffer), XtRImmediate, (XtPointer)0},
  {XtNunsplittable, XtCUnsplittable, XtRBoolean, sizeof (Boolean),
     offset (unsplittable), XtRImmediate, (XtPointer)0},
  {XtNinternalBorderWidth, XtCInternalBorderWidth, XtRInt, sizeof (int),
     offset (internal_border_width), XtRImmediate, (XtPointer)4},
  {XtNinterline, XtCInterline, XtRInt, sizeof (int),
     offset (interline), XtRImmediate, (XtPointer)0},
  {XtNfont,  XtCFont, XtRFontStruct, sizeof(XFontStruct *),
     offset(font),XtRString, DEFAULT_FACE_FONT},
  {XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
     offset(foreground_pixel), XtRString, "XtDefaultForeground"},
  {XtNcursorColor, XtCForeground, XtRPixel, sizeof(Pixel),
     offset(cursor_color), XtRString, "XtDefaultForeground"},
  {XtNbarCursor, XtCBarCursor, XtRBoolean, sizeof (Boolean),
     offset (bar_cursor), XtRImmediate, (XtPointer)0},
  {XtNvisualBell, XtCVisualBell, XtRBoolean, sizeof (Boolean),
     offset (visual_bell), XtRImmediate, (XtPointer)0},
  {XtNbellVolume, XtCBellVolume, XtRInt, sizeof (int),
     offset (bell_volume), XtRImmediate, (XtPointer)0},
  {XtNgeometry, XtCGeometry, XtRString, sizeof(char *),
     offset (emacs_geometry), XtRImmediate, 0},
  {XtNemacsScreen, XtCEmacsScreen, XtRPointer, sizeof (XtPointer),
     offset (screen), XtRImmediate, 0},
};

static XtResource initial_geometry_resources[] = {
  {XtNinitialGeometry, XtCInitialGeometry, XtRString, sizeof(char *),
     0, XtRImmediate, 0},
};
#undef offset

static XtActionsRec
emacsScreenActionsTable [] = {
  {"keypress", key_press},
  {"focus_in",  emacs_screen_focus_handler},
  {"focus_out", emacs_screen_focus_handler},
};

static char
emacsScreenTranslations [] = "<KeyPress>: keypress()\n\
<FocusIn>: focus_in()\n\
<FocusOut>: focus_out()\n\
";

EmacsScreenClassRec emacsScreenClassRec = {
    { /* core fields */
    /* superclass		*/	&widgetClassRec, /*&xmLabelClassRec,*/
    /* class_name		*/	"EmacsScreen",
    /* widget_size		*/	sizeof(EmacsScreenRec),
    /* class_initialize		*/	0,
    /* class_part_initialize	*/	0,
    /* class_inited		*/	FALSE,
    /* initialize		*/	EmacsScreenInitialize,
    /* initialize_hook		*/	0,
    /* realize			*/	EmacsScreenRealize,
    /* actions			*/	emacsScreenActionsTable,
    /* num_actions		*/	XtNumber (emacsScreenActionsTable),
    /* resources		*/	resources,
    /* resource_count		*/	XtNumber(resources),
    /* xrm_class		*/	NULLQUARK,
    /* compress_motion		*/	TRUE,
    /* compress_exposure	*/	TRUE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest		*/	FALSE,
    /* destroy			*/	EmacsScreenDestroy,
    /* resize			*/	EmacsScreenResize,
    /* expose			*/	XtInheritExpose,
    /* set_values		*/	EmacsScreenSetValues,
    /* set_values_hook		*/	0,
    /* set_values_almost	*/	XtInheritSetValuesAlmost,
    /* get_values_hook		*/	0,
    /* accept_focus		*/	XtInheritAcceptFocus,
    /* version			*/	XtVersion,
    /* callback_private		*/	0,
    /* tm_table			*/	emacsScreenTranslations,
    /* query_geometry		*/	EmacsScreenQueryGeometry,
    /* display_accelerator	*/	XtInheritDisplayAccelerator,
    /* extension		*/	0
    }
};

WidgetClass emacsScreenWidgetClass = (WidgetClass) &emacsScreenClassRec;

static void
get_default_char_pixel_size (EmacsScreenWidget ew, int* pixel_width,
			     int* pixel_height)
{
  *pixel_width = XTextWidth (ew->emacs_screen.font, "n", 1);
  if (ew->emacs_screen.screen->display.x->text_height)
    *pixel_height = ew->emacs_screen.screen->display.x->text_height;
  else
    *pixel_height =
      ew->emacs_screen.font->ascent + ew->emacs_screen.font->descent;
}

static void
pixel_to_char_size (EmacsScreenWidget ew,
		    Dimension pixel_width, Dimension pixel_height,
		    int* char_width, int* char_height)
{
  int cpw;
  int cph;

  get_default_char_pixel_size (ew, &cpw, &cph);
  *char_width =
    (pixel_width - 2 * ew->emacs_screen.internal_border_width) / cpw;
  *char_height =
    (pixel_height - 2 * ew->emacs_screen.internal_border_width) / cph; 
}

static void
char_to_pixel_size (EmacsScreenWidget ew, int char_width, int char_height,
		    Dimension* pixel_width, Dimension* pixel_height)
{
  int cpw;
  int cph;

  get_default_char_pixel_size (ew, &cpw, &cph);
  *pixel_width =
    char_width * cpw + 2 * ew->emacs_screen.internal_border_width;
  *pixel_height =
    char_height * cph + 2 * ew->emacs_screen.internal_border_width;
}

static void
round_size_to_char (EmacsScreenWidget ew,
		    Dimension in_width, Dimension in_height,
		    Dimension* out_width, Dimension* out_height)
{
  int char_width;
  int char_height;
  pixel_to_char_size (ew, in_width, in_height, &char_width, &char_height);
  char_to_pixel_size (ew, char_width, char_height, out_width, out_height);
}

static Widget
get_wm_shell (Widget w)
{
  Widget wmshell;

  for (wmshell = XtParent (w);
       wmshell && !XtIsWMShell (wmshell);
       wmshell = XtParent (wmshell));

  return wmshell;
}

static void
set_screen_size (EmacsScreenWidget ew)
{
  int x = 0;
  int y = 0;
  unsigned int columns = 80;
  unsigned int rows = 40;
  int parse_result = 0;
  struct screen* screen = ew->emacs_screen.screen;
  Dimension pixel_width, pixel_height;
  Widget wmshell;
  char shell_position [32];

  static Boolean is_first_screen = True;

  wmshell = get_wm_shell ((Widget)ew);

  if (!ew->emacs_screen.emacs_geometry)
    XtVaGetValues (wmshell, XtNgeometry,
		   &ew->emacs_screen.emacs_geometry, 0);

  if (!ew->emacs_screen.emacs_geometry)
    XtVaGetValues (XtParent (wmshell), XtNgeometry,
		   &ew->emacs_screen.emacs_geometry, 0);

  if (!ew->emacs_screen.emacs_geometry && is_first_screen)
    {
      XtGetSubresources ((Widget)ew,
			 (XtPointer)&ew->emacs_screen.emacs_geometry,
			 ew->core.name,
			 ew->core.widget_class->core_class.class_name,
			 initial_geometry_resources,
			 XtNumber (initial_geometry_resources),
			 0, 0);
    }

  is_first_screen = False;

  if (ew->emacs_screen.emacs_geometry)
    parse_result = XParseGeometry (ew->emacs_screen.emacs_geometry,
				   &x, &y, &columns, &rows);
  else
    parse_result = 0;

  change_screen_size (screen, rows, columns, 0);
  char_to_pixel_size (ew, columns, rows, &pixel_width, &pixel_height);
  ew->core.width = pixel_width;
  ew->core.height = pixel_height;
  
  /* If a position was specified, assign it to the shell widget. */
  if (parse_result & (XValue | YValue))
    {
      /* the tricky things with the sign is to make sure that
	 -0 is printed -0. */
      int len;
      char *tem;
      sprintf (shell_position, "=%c%d%c%d",
	       parse_result & XNegative ? '-' : '+', x < 0 ? -x : x,
	       parse_result & YNegative ? '-' : '+', y < 0 ? -y : y);
      len = strlen (shell_position) + 1;
      tem = xmalloc (len);
      strncpy (tem, shell_position, len);
      XtVaSetValues (wmshell, XtNgeometry, tem, 0);
    }
  else if (parse_result & (WidthValue | HeightValue))
    {
      int len;
      char *tem;
      sprintf (shell_position, "=%dx%d", pixel_width, pixel_height);
      len = strlen (shell_position) + 1;
      tem = xmalloc (len);
      strncpy (tem, shell_position, len);
      XtVaSetValues (wmshell, XtNgeometry, tem, 0);
    }
}

static void
update_wm_hints (EmacsScreenWidget ew)
{
  Widget wmshell = get_wm_shell ((Widget)ew);
  int cw;
  int ch;
  Dimension rounded_width;
  Dimension rounded_height;
  int char_width;
  int char_height;
  int base_width;
  int base_height;

  pixel_to_char_size (ew, ew->core.width, ew->core.height,
		      &char_width, &char_height);
  char_to_pixel_size (ew, char_width, char_height,
		      &rounded_width, &rounded_height);
  get_default_char_pixel_size (ew, &cw, &ch);

  base_width = (wmshell->core.width - ew->core.width
		+ (rounded_width - (char_width * cw)));
  base_height = (wmshell->core.height - ew->core.height
		+ (rounded_height - (char_height * ch)));

  XtVaSetValues (wmshell,
		 XtNbaseWidth, base_width,
		 XtNbaseHeight, base_height,
		 XtNwidthInc, cw, 
		 XtNheightInc, ch,
		 XtNminWidth, base_width + 10 * cw,
		 XtNminHeight, base_height + 4 * ch,
		 0);
}

static void
create_screen_gcs (EmacsScreenWidget ew)
{
  struct screen* s = ew->emacs_screen.screen;

  s->display.x->normal_gc =
    XCreateGC (XtDisplay (ew), RootWindowOfScreen (XtScreen (ew)), 0, 0);
  s->display.x->reverse_gc =
    XCreateGC (XtDisplay (ew), RootWindowOfScreen (XtScreen (ew)), 0, 0);
  s->display.x->cursor_gc =
    XCreateGC (XtDisplay (ew), RootWindowOfScreen (XtScreen (ew)), 0, 0);
}

static void
setup_screen_gcs (EmacsScreenWidget ew)
{
  XGCValues gc_values;
  struct screen* s = ew->emacs_screen.screen;

  static char cursor_bits[] =
    {
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    };


  /* Normal video */
  gc_values.font = ew->emacs_screen.font->fid;
  gc_values.foreground = ew->emacs_screen.foreground_pixel;
  gc_values.background = ew->core.background_pixel;
  gc_values.graphics_exposures = False;
  XChangeGC (XtDisplay (ew), s->display.x->normal_gc,
	     (GCFont | GCForeground | GCBackground | GCGraphicsExposures),
	     &gc_values);

  /* Reverse video style. */
  gc_values.font = ew->emacs_screen.font->fid;
  gc_values.foreground = ew->core.background_pixel;
  gc_values.background = ew->emacs_screen.foreground_pixel;
  gc_values.graphics_exposures = False;
  XChangeGC (XtDisplay (ew), s->display.x->reverse_gc,
	     (GCFont | GCForeground | GCBackground | GCGraphicsExposures),
	     &gc_values);

  /* Cursor has to have an empty stipple. */
  gc_values.font = ew->emacs_screen.font->fid;
  gc_values.foreground = ew->core.background_pixel;
  gc_values.background = ew->emacs_screen.cursor_color;
  gc_values.graphics_exposures = False;
  gc_values.stipple =
    XCreateBitmapFromData (XtDisplay (ew),
			   RootWindowOfScreen (XtScreen (ew)),
			   cursor_bits, 16, 16);
  XChangeGC (XtDisplay (ew), s->display.x->cursor_gc,
	     (GCFont | GCForeground | GCBackground | GCGraphicsExposures
	      | GCStipple),
	     &gc_values);
}

static void
update_various_screen_slots (EmacsScreenWidget ew)
{
  struct x_display* x = ew->emacs_screen.screen->display.x;
  x->pixel_height = ew->core.height;
  x->pixel_width = ew->core.width;
  x->internal_border_width = ew->emacs_screen.internal_border_width;
}

static void 
EmacsScreenInitialize (Widget request, Widget new,
		       ArgList dum1, Cardinal *dum2)
{
  EmacsScreenWidget ew = (EmacsScreenWidget)new;

  if (!ew->emacs_screen.screen)
    {
      fprintf (stderr,
	       "can't create an emacs screen widget without a screen\n");
      exit (1);
    }

  /* If the "Emacs.EmacsScreen.{default,Face}.{attributeFont,AttributeFont}"
     resource is set, then it always overrides "Emacs.EmacsScreen.{font,Font}".
     It's unfortunate that we have to do this, but we need to know the font
     size for screen-sizing purposes before the faces get initialized.  If
     the "default.attributeFont" isn't set, then we use the font of this
     ScreenWidget itself, defaulting to XtDefaultFont.  Up in the lisp code,
     the "default" face will use the screen's font if its own is not set,
     so everything stays in sync -- it's not possible for the screen's font
     and the default face's font to be different.
   */
  {
    XFontStruct *f = 0;
    XtResource face_res;
    face_res.resource_name = "attributeFont";
    face_res.resource_class = "AttributeFont";
    face_res.resource_type = XtRFontStruct;
    face_res.resource_size = sizeof (XFontStruct *);
    face_res.resource_offset = 0;
    face_res.default_type = XtRImmediate;
    face_res.default_addr = 0;
    XtGetSubresources ((Widget) ew, (XtPointer) &f, "default", "Face",
		       &face_res, 1, NULL, 0);
      
    if (f)
      ew->emacs_screen.font = f;
    else if (! ew->emacs_screen.font)
      {
	fprintf (stderr, "emacs screen widget could not load a font\n");
	exit (1);
      }
  }

  set_screen_size (ew);
  create_screen_gcs (ew);
  setup_screen_gcs (ew);
  update_various_screen_slots (ew);
}


static void
EmacsScreenRealize (Widget widget, XtValueMask *mask,
		    XSetWindowAttributes *attrs)
{
  EmacsScreenWidget ew = (EmacsScreenWidget)widget;

  attrs->event_mask = (KeyPressMask | ExposureMask | ButtonPressMask |
		       ButtonReleaseMask | StructureNotifyMask |
		       FocusChangeMask | PointerMotionHintMask |
		       PointerMotionMask | LeaveWindowMask | EnterWindowMask |
		       VisibilityChangeMask | PropertyChangeMask |
		       StructureNotifyMask | SubstructureNotifyMask |
		       SubstructureRedirectMask);
  *mask |= CWEventMask;
  XtCreateWindow (widget, InputOutput, (Visual *)CopyFromParent, *mask,
		  attrs);
  update_wm_hints (ew);
}

extern void free_screen_faces (struct screen *);

static void
EmacsScreenDestroy (Widget widget)
{
  EmacsScreenWidget ew = (EmacsScreenWidget) widget;
  struct screen* s = ew->emacs_screen.screen;

  if (! s) abort ();
  if (! s->display.x) abort ();
  if (! s->display.x->normal_gc) abort ();

  /* this would be called from Fdelete_screen() but it needs to free some
     stuff after the widget has been finalized but before the widget has
     been freed. */
  free_screen_faces (s);

  /* need to be careful that the face-freeing code doesn't free these too */
  XFreeGC (XtDisplay (widget), s->display.x->normal_gc);
  XFreeGC (XtDisplay (widget), s->display.x->reverse_gc);
  XFreeGC (XtDisplay (widget), s->display.x->cursor_gc);
}

void
EmacsScreenResize (Widget widget)
{
  EmacsScreenWidget ew = (EmacsScreenWidget)widget;
  struct screen *s = ew->emacs_screen.screen;
  int columns;
  int rows;
  
  pixel_to_char_size (ew, ew->core.width, ew->core.height, &columns, &rows);
  change_screen_size (s, rows, columns, 0);
  update_wm_hints (ew);
  update_various_screen_slots (ew);
}

static Boolean
EmacsScreenSetValues (Widget cur_widget, Widget req_widget, Widget new_widget,
		      ArgList dum1, Cardinal *dum2)
{
  EmacsScreenWidget cur = (EmacsScreenWidget)cur_widget;
  EmacsScreenWidget new = (EmacsScreenWidget)new_widget;

  Boolean needs_a_refresh = False;
  Boolean has_to_recompute_size;
  Boolean has_to_recompute_gcs;
  Boolean has_to_update_hints;

  int char_width, char_height;
  Dimension pixel_width;
  Dimension pixel_height;
  
  has_to_recompute_gcs = (cur->emacs_screen.font != new->emacs_screen.font
			  || (cur->emacs_screen.foreground_pixel
			      != new->emacs_screen.foreground_pixel)
			  || (cur->core.background_pixel
			      != new->core.background_pixel)
			  );
  
  has_to_recompute_size = (cur->emacs_screen.font != new->emacs_screen.font
			   && cur->core.width == new->core.width
			   && cur->core.height == new->core.height);

  has_to_update_hints = (cur->emacs_screen.font != new->emacs_screen.font);

  if (has_to_recompute_gcs)
    {
      setup_screen_gcs (new);
      needs_a_refresh = True;
    }
			  
  if (has_to_recompute_size)
    {
      pixel_width = new->core.width;
      pixel_height = new->core.height;
      pixel_to_char_size (new, pixel_width, pixel_height, &char_width,
			  &char_height);
      char_to_pixel_size (new, char_width, char_height, &pixel_width,
			  &pixel_height);
      new->core.width = pixel_width;
      new->core.height = pixel_height;

      change_screen_size (new->emacs_screen.screen, char_height, char_width,
			  0);
      needs_a_refresh = True;
    }

  if (has_to_update_hints)
    update_wm_hints (new);

  update_various_screen_slots (new);

  return needs_a_refresh;
}

static XtGeometryResult
EmacsScreenQueryGeometry (Widget widget, XtWidgetGeometry* request,
			  XtWidgetGeometry* result)
{
  EmacsScreenWidget ew = (EmacsScreenWidget)widget;

  int mask = request->request_mode;
  Dimension ok_width, ok_height;

  if (mask & (CWWidth | CWHeight))
    {
      round_size_to_char (ew,
			  (mask & CWWidth) ? request->width : ew->core.width,
			  ((mask & CWHeight) ? request->height
			   : ew->core.height),
			  &ok_width, &ok_height);
      if ((mask & CWWidth) && (ok_width != request->width))
	{
	  result->request_mode |= CWWidth;
	  result->width = ok_width;
	}
      if ((mask & CWHeight) && (ok_height != request->height))
	{
	  result->request_mode |= CWHeight;
	  result->height = ok_height;
	}
    }
  return result->request_mode ? XtGeometryAlmost : XtGeometryYes;
}


/* action proc */
static void
key_press (Widget w, XEvent* event, String *params, Cardinal *n_params)
{
}

static void
emacs_screen_focus_handler (Widget w, XEvent *event, String *params,
			    Cardinal *n_params)
{
  emacs_Xt_focus_event_handler (event, 0);
}

/* Special entrypoints */
void
EmacsScreenSetCharSize (Widget widget, int columns, int rows)
{
  EmacsScreenWidget ew = (EmacsScreenWidget) widget;
  Dimension pixel_width, pixel_height, granted_width, granted_height;
  XtGeometryResult result;
  if (columns < 3) columns = 3;  /* no way buddy */
  if (rows < 3) rows = 3;
  char_to_pixel_size (ew, columns, rows, &pixel_width, &pixel_height);
  result = XtMakeResizeRequest ((Widget)ew,
				pixel_width, pixel_height,
				&granted_width, &granted_height);
  if (result == XtGeometryAlmost)
    XtMakeResizeRequest ((Widget) ew, granted_width, granted_height,
			 NULL, NULL);
  /* damn Paned widget won't ever change its width.  Force it. */
  if (ew->core.width != pixel_width)
    {
      XtVaSetValues (XtParent ((Widget) ew), XtNwidth, pixel_width, 0);
      XtVaSetValues ((Widget) ew, XtNwidth, pixel_width, 0);
    }
}


