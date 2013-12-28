/* The emacs screen widget.
   Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.

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
#include "intl.h"
#include "lisp.h"
#include "xterm.h"
#include "xobjs.h"
#include "dispextern.h"
#include "screen.h"

#include <X11/StringDefs.h>
#include "xintrinsicp.h"
#include <X11/cursorfont.h>
#include "EmacsScreenP.h"
#include <X11/Shell.h>
#include <X11/ShellP.h>

#ifndef LWLIB_USES_MOTIF /* Athena */
#include <X11/Xaw/Paned.h>
#endif /* Athena */

#ifdef EXTERNAL_WIDGET
#include "EmacsShell.h"
#endif

#ifdef DEBUG_WIDGET
extern int debug_widget;
#endif

#define max(a, b) ((a) > (b) ? (a) : (b))

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

   #### Perhaps we could have this code explicitly set XtDefaultFont to this?
 */
#ifdef I18N4
/* Use more "generic" font base so that an appropriate font set can be constructed
   from this base according to the locale. */
#define DEFAULT_FONT_SET "-*-*-medium-r-*-*-*-120-*-*-*-*-*-*"
#else
#define DEFAULT_FACE_FONT "-*-courier-medium-r-*-*-*-120-*-*-*-*-iso8859-*"
#endif

void emacs_Xt_focus_event_handler (XEvent *x_event, struct screen *s);

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
#define offset(field) XtOffset(EmacsScreen, emacs_screen.field)

static XtResource resources[] = {
  {XtNgeometry, XtCGeometry, XtRString, sizeof(String),
     offset (geometry), XtRString, (XtPointer) 0},
  {XtNiconic, XtCIconic, XtRBoolean, sizeof(Boolean),
     offset (iconic), XtRImmediate, (XtPointer) False},

  {XtNemacsScreen, XtCEmacsScreen, XtRPointer, sizeof (XtPointer),
     offset (screen), XtRImmediate, 0},
  {XtNmenubar, XtCMenubar, XtRBoolean, sizeof (Boolean),
     offset (menubar_p), XtRImmediate, (XtPointer)1},

  {XtNminibuffer, XtCMinibuffer, XtRInt, sizeof (int),
     offset (minibuffer), XtRImmediate, (XtPointer)0},
  {XtNunsplittable, XtCUnsplittable, XtRBoolean, sizeof (Boolean),
     offset (unsplittable), XtRImmediate, (XtPointer)0},
  {XtNinternalBorderWidth, XtCInternalBorderWidth, XtRInt, sizeof (int),
     offset (internal_border_width), XtRImmediate, (XtPointer)4},
  {XtNinterline, XtCInterline, XtRInt, sizeof (int),
     offset (interline), XtRImmediate, (XtPointer)0},
#ifdef I18N4
  {XtNfontSet,  XtCFontSet, XtRFontSet, sizeof(XFontSet),
     offset(font), XtRString, DEFAULT_FONT_SET},
#else
  {XtNfont,  XtCFont, XtRFontStruct, sizeof(XFontStruct *),
     offset(font), XtRString, DEFAULT_FACE_FONT},
#endif
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
};

#undef offset

static XtActionsRec
emacsScreenActionsTable [] = {
  {"keypress",  key_press},
  {"focus_in",  emacs_screen_focus_handler},
  {"focus_out", emacs_screen_focus_handler},
};

static char
emacsScreenTranslations [] = "\
<KeyPress>: keypress()\n\
<FocusIn>:  focus_in()\n\
<FocusOut>: focus_out()\n\
";

EmacsScreenClassRec emacsScreenClassRec = {
    { /* core fields */
    /* superclass		*/	&widgetClassRec,
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

WidgetClass emacsScreenClass = (WidgetClass) &emacsScreenClassRec;

static void
get_default_char_pixel_size (EmacsScreen ew, int* pixel_width,
			     int* pixel_height)
{
#ifdef I18N4
  *pixel_width = XmbTextEscapement (ew->emacs_screen.font, "n", 1);
#else
  *pixel_width = XTextWidth (ew->emacs_screen.font, "n", 1);
#endif

#if 0
  /* #### x->text_height does not have the right value at this point.
     What was the point of using this value?  It doesn't really save
     anything.  Was it intended to differ from ascent+descent?
   */
  if (ew->emacs_screen.screen->display.x->text_height)
    *pixel_height = ew->emacs_screen.screen->display.x->text_height;
  else
#endif

    *pixel_height =
#ifdef I18N4
      XExtentsOfFontSet (ew->emacs_screen.font)->max_logical_extent.height;
#else
      ew->emacs_screen.font->ascent + ew->emacs_screen.font->descent;
#endif
}

static void
pixel_to_char_size (EmacsScreen ew,
		    Dimension pixel_width, Dimension pixel_height,
		    int* char_width, int* char_height)
{
  int cpw;
  int cph;
  int egw;

  get_default_char_pixel_size (ew, &cpw, &cph);
  egw = max (cpw,
	     (int) max (XPIXMAP (glyph_to_pixmap (continuer_glyph))->width,
			XPIXMAP (glyph_to_pixmap (truncator_glyph))->width));

  *char_width = 1 +
    (int)((pixel_width - egw) - 2 * ew->emacs_screen.internal_border_width) /
      cpw;
  *char_height =
    (int)(pixel_height - 2 * ew->emacs_screen.internal_border_width) / cph; 
}

static void
char_to_pixel_size (EmacsScreen ew, int char_width, int char_height,
		    Dimension* pixel_width, Dimension* pixel_height)
{
  int cpw;
  int cph;

  get_default_char_pixel_size (ew, &cpw, &cph);
  *pixel_width =
    (char_width - 1) * cpw + 2 * ew->emacs_screen.internal_border_width +
      max (cpw,
	   (int) max (XPIXMAP (glyph_to_pixmap (continuer_glyph))->width,
		      XPIXMAP (glyph_to_pixmap (truncator_glyph))->width));
  *pixel_height =
    char_height * cph + 2 * ew->emacs_screen.internal_border_width;
}

static void
round_size_to_char (EmacsScreen ew,
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
       (wmshell
	&& !XtIsWMShell (wmshell)
#ifdef EXTERNAL_WIDGET
	/* The Emacs shell is not normally recognized as a WM shell but
	   for purposes it should be.
	 */
	&& !is_emacs_shell (wmshell)
#endif
	);
       wmshell = XtParent (wmshell))
    ;

  return wmshell;
}

static void
mark_shell_size_user_specified (Widget wmshell)
{
  if (! XtIsWMShell (wmshell)) abort ();
  /* This is kind of sleazy, but I can't see how else to tell it to make it
     mark the WM_SIZE_HINTS size as user specified when appropriate. */
  ((WMShellWidget) wmshell)->wm.size_hints.flags |= USSize;
}

static void
set_wm_shell_iconic_p (Widget shell, int iconic_p)
{
  /* This is all that SHOULD be necessary to do what we want: */
  XtVaSetValues (shell, XtNiconic, iconic_p, 0);

  /* Except that the above SetValues call doesn't actually DO anything
     (the TopLevelShell only looks at `iconic' when it is being
     *initialized* instead of when it is being *managed*, dammit) so
     we have to smash the contents of the wmshell by hand.  Xt sucks. */
  {
    WMShellWidget wmshell;
    int old, new;
    if (! XtIsSubclass (shell, wmShellWidgetClass)) abort ();
    wmshell = (WMShellWidget) shell;
    old = (wmshell->wm.wm_hints.flags & StateHint
	   ? wmshell->wm.wm_hints.initial_state
	   : NormalState);
    new = (iconic_p ? IconicState : NormalState);
    wmshell->wm.wm_hints.flags |= StateHint;
    wmshell->wm.wm_hints.initial_state = new;

    /* If the window has already been created, update the properties too... */
    if (old != new && XtWindow (shell))
      XSetWMHints (XtDisplay (shell), XtWindow (shell),
		   &wmshell->wm.wm_hints);
  }
}


#ifdef EXTERNAL_WIDGET
/* find the EmacsShell parent, or return NULL if none. */
Widget
get_emacs_shell (Widget w)
{
  Widget emshell;

  for (emshell = XtParent (w);
       emshell && !is_emacs_shell (emshell);
       emshell = XtParent (emshell));

  return emshell;
}
#endif /* EXTERNAL_WIDGET */

/* Can't have static frame locals because of some broken compilers.
   Normally, initializing a variable like this doesn't work in emacs,
   but it's ok in this file because it must come after lastfile (and
   thus have its data not go into text space) because Xt needs to
   write to initialized data objects too.
 */
static Boolean first_screen_p = True;

static void
set_screen_size (EmacsScreen ew)
{
  /* The widget hierarchy is

	argv[0]			shell		pane		SCREEN-NAME
	ApplicationShell	TopLevelShell	XmMainWindow	EmacsScreen

     We accept geometry specs in this order:

	*SCREEN-NAME.geometry
	*EmacsScreen.geometry
	Emacs.geometry

     Other possibilities for widget hierarchies might be

	argv[0]			screen		pane	SCREEN-NAME
	ApplicationShell	TopLevelShell	Paned	EmacsScreen
     or
	argv[0]			SCREEN-NAME	pane	SCREEN-NAME
	ApplicationShell	TopLevelShell	Paned	EmacsScreen
     or
	argv[0]			SCREEN-NAME	pane	emacsTextPane
	ApplicationShell	EmacsScreen	Paned	EmacsTextPane

     With the current setup, the text-display-area is the part which is
     an emacs "screen", since that's the only part managed by emacs proper
     (the menubar and the parent of the menubar and all that sort of thing
     are managed by lwlib.)

#ifdef EXTERNAL_WIDGET
     The EmacsShell widget is simply a replacement for the Shell widget 
     which is able to deal with using an externally-supplied window instead
     of always creating its own.  It is not actually emacs specific, and
     should possibly have class "Shell" instead of "EmacsShell" to simplify
     the resources.
#endif

   */

  /* Geometry of the AppShell */
  int app_flags = 0;
  int app_x = 0;
  int app_y = 0;
  unsigned int app_w = 0;
  unsigned int app_h = 0;
  
  /* Geometry of the EmacsScreen */
  int screen_flags = 0;
  int screen_x = 0;
  int screen_y = 0;
  unsigned int screen_w = 0;
  unsigned int screen_h = 0;
  
  /* Hairily merged geometry */
  int x = 0;
  int y = 0;
  unsigned int w = 80;
  unsigned int h = 40;
  int flags = 0;
  
  Widget wmshell = get_wm_shell ((Widget) ew);
  Widget app_shell = XtParent ((Widget) wmshell);
  
  
  if (! XtIsSubclass (wmshell, shellWidgetClass)) abort ();
  if (! XtIsSubclass (app_shell, shellWidgetClass)) abort ();

  /* If the EmacsScreen doesn't have a geometry but the shell does,
     treat that as the geometry of the screen.  (Is this bogus?
     I'm not sure.) */
  if (ew->emacs_screen.geometry == 0)
    XtVaGetValues (wmshell, XtNgeometry, &ew->emacs_screen.geometry, 0);

  /* If the Shell is iconic, then the EmacsScreen is iconic.  (Is
     this bogus? I'm not sure.) */
  if (!ew->emacs_screen.iconic)
    XtVaGetValues (wmshell, XtNiconic, &ew->emacs_screen.iconic, 0);
  
  
  {
    char *geom = 0;
    XtVaGetValues (app_shell, XtNgeometry, &geom, 0);
    if (geom)
      app_flags = XParseGeometry (geom, &app_x, &app_y, &app_w, &app_h);
  }
  
  if (ew->emacs_screen.geometry)
    screen_flags = XParseGeometry (ew->emacs_screen.geometry,
				   &screen_x, &screen_y,
				   &screen_w, &screen_h);
  
  if (first_screen_p)
    {
      /* If this is the first screen created:
         ====================================

         - Use the ApplicationShell's size/position, if specified.
           (This is "Emacs.geometry", or the "-geometry" command line arg.)
         - Else use the EmacsScreen's size/position.
           (This is "*SCREEN-NAME.geometry")

	 - If the AppShell is iconic, the screen should be iconic.

	 AppShell comes first so that -geometry always applies to the first
	 screen created, even if there is an "every screen" entry in the
	 resource database.
       */
      if (app_flags & (XValue | YValue))
	{
	  x = app_x; y = app_y;
	  flags |= (app_flags & (XValue | YValue | XNegative | YNegative));
	}
      else if (screen_flags & (XValue | YValue))
	{
	  x = screen_x; y = screen_y;
	  flags |= (screen_flags & (XValue | YValue | XNegative | YNegative));
	}

      if (app_flags & (WidthValue | HeightValue))
	{
	  w = app_w; h = app_h;
	  flags |= (app_flags & (WidthValue | HeightValue));
	}
      else if (screen_flags & (WidthValue | HeightValue))
	{
	  w = screen_w; h = screen_h;
	  flags |= (screen_flags & (WidthValue | HeightValue));
	}

      /* If the AppShell is iconic, then the EmacsScreen is iconic. */
      if (!ew->emacs_screen.iconic)
	XtVaGetValues (app_shell, XtNiconic, &ew->emacs_screen.iconic, 0);

      first_screen_p = False;
    }
  else
    {
      /* If this is not the first screen created:
         ========================================

         - use the EmacsScreen's size/position if specified
         - Otherwise, use the ApplicationShell's size, but not position.

         So that means that one can specify the position of the first screen
         with "Emacs.geometry" or `-geometry'; but can only specify the
	 position of subsequent screens with "*SCREEN-NAME.geometry".

	 AppShell comes second so that -geometry does not apply to subsequent
	 screens when there is an "every screen" entry in the resource db,
	 but does apply to the first screen.
       */
      if (screen_flags & (XValue | YValue))
	{
	  x = screen_x; y = screen_y;
	  flags |= (screen_flags & (XValue | YValue | XNegative | YNegative));
	}

      if (screen_flags & (WidthValue | HeightValue))
	{
	  w = screen_w; h = screen_h;
	  flags |= (screen_flags & (WidthValue | HeightValue));
	}
      else if (app_flags & (WidthValue | HeightValue))
	{
	  w = app_w;
	  h = app_h;
	  flags |= (app_flags & (WidthValue | HeightValue));
	}
    }

  {
    struct screen* screen = ew->emacs_screen.screen;
    Dimension pixel_width, pixel_height;
    char shell_position [32];


    change_screen_size (screen, h, w, 0);
    char_to_pixel_size (ew, w, h, &pixel_width, &pixel_height);
    ew->core.width = pixel_width;
    ew->core.height = pixel_height;

    /* If a position was specified, assign it to the shell widget.
       (Else WM won't do anything with it.)
     */
    if (flags & (XValue | YValue))
      {
	/* the tricky things with the sign is to make sure that
	   -0 is printed -0. */
	int len;
	char *tem;
	sprintf (shell_position, "=%c%d%c%d",
		 flags & XNegative ? '-' : '+', x < 0 ? -x : x,
		 flags & YNegative ? '-' : '+', y < 0 ? -y : y);
	len = strlen (shell_position) + 1;
	tem = xmalloc (len);
	strncpy (tem, shell_position, len);
	XtVaSetValues (wmshell, XtNgeometry, tem, 0);
      }
    else if (flags & (WidthValue | HeightValue))
      {
	int len;
	char *tem;
	sprintf (shell_position, "=%dx%d", pixel_width, pixel_height);
	len = strlen (shell_position) + 1;
	tem = xmalloc (len);
	strncpy (tem, shell_position, len);
	XtVaSetValues (wmshell, XtNgeometry, tem, 0);
      }

    /* If the geometry spec we're using has W/H components, mark the size
       in the WM_SIZE_HINTS as user specified. */
    if (flags & (WidthValue | HeightValue))
      mark_shell_size_user_specified (wmshell);

    /* Also assign the iconic status of the screen to the Shell, so that
       the WM sees it. */
    set_wm_shell_iconic_p (wmshell, ew->emacs_screen.iconic);
  }
}


static void
update_wm_hints (EmacsScreen ew)
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

#ifdef EXTERNAL_WIDGET
  /* don't do anything if we're a child of an EmacsShell */
  if (!XtIsWMShell(wmshell))
    return;
#endif

  pixel_to_char_size (ew, ew->core.width, ew->core.height,
		      &char_width, &char_height);
  char_to_pixel_size (ew, char_width, char_height,
		      &rounded_width, &rounded_height);
  get_default_char_pixel_size (ew, &cw, &ch);

  base_width = (wmshell->core.width - ew->core.width
		+ (rounded_width - (char_width * cw)));
  base_height = (wmshell->core.height - ew->core.height
		+ (rounded_height - (char_height * ch)));

  /* This is kind of sleazy, but I can't see how else to tell it to
     make it mark the WM_SIZE_HINTS size as user specified.
   */
/*  ((WMShellWidget) wmshell)->wm.size_hints.flags |= USSize;*/

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
update_various_screen_slots (EmacsScreen ew)
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
  EmacsScreen ew = (EmacsScreen)new;

  if (!ew->emacs_screen.screen)
    fatal ("can't create an emacs screen widget without a screen\n");

  /* If the "Emacs.EmacsScreen.{default,Face}.{attributeFont,AttributeFont}"
     resource is set, then it always overrides "Emacs.EmacsScreen.{font,Font}".
     It's unfortunate that we have to do this, but we need to know the font
     size for screen-sizing purposes before the faces get initialized.  If
     the "default.attributeFont" isn't set, then we use the font of this
     EmacsScreen itself, defaulting to XtDefaultFont.  Up in the lisp code,
     the "default" face will use the screen's font if its own is not set,
     so everything stays in sync -- it's not possible for the screen's font
     and the default face's font to be different.
   */
  {
#ifdef I18N4
    XFontSet f = 0;
#else
    XFontStruct *f = 0;
#endif
    XtResource face_res;
    face_res.resource_name = "attributeFont";
    face_res.resource_class = "AttributeFont";
#ifdef I18N4
    face_res.resource_type = XtRFontSet;
    face_res.resource_size = sizeof (XFontSet);
#else
    face_res.resource_type = XtRFontStruct;
    face_res.resource_size = sizeof (XFontStruct *);
#endif
    face_res.resource_offset = 0;
    face_res.default_type = XtRImmediate;
    face_res.default_addr = 0;
    XtGetSubresources ((Widget) ew, (XtPointer) &f, "default", "Face",
		       &face_res, 1, NULL, 0);
      
    if (f)
      ew->emacs_screen.font = f;
    else if (! ew->emacs_screen.font)
      fatal ("screen widget could not load a font\n");
  }

  set_screen_size (ew);
  update_various_screen_slots (ew);
}


static void
EmacsScreenRealize (Widget widget, XtValueMask *mask,
		    XSetWindowAttributes *attrs)
{
  EmacsScreen ew = (EmacsScreen)widget;
#ifdef EXTERNAL_WIDGET
  Widget emshell = get_emacs_shell (widget);
#endif
  long other_event_mask;

  attrs->event_mask = (ExposureMask | StructureNotifyMask |
		       VisibilityChangeMask | PropertyChangeMask |
		       StructureNotifyMask | SubstructureNotifyMask |
		       SubstructureRedirectMask);

  other_event_mask = KeyPressMask | ButtonPressMask | ButtonReleaseMask |
		     FocusChangeMask | PointerMotionHintMask |
		     PointerMotionMask | LeaveWindowMask | EnterWindowMask;

#ifdef I18N4
  /* Make sure that events wanted by the input method are selected. */
  other_event_mask |= input_method_event_mask;
#endif

#if 1
  attrs->event_mask |= other_event_mask;
#endif

#ifdef EXTERNAL_WIDGET
  if (!emshell)
    {
# ifdef DEBUG_WIDGET
      if (debug_widget)
	printf ("no emacs shell.\n");
# endif
      attrs->event_mask |= other_event_mask;
    }
# ifdef DEBUG_WIDGET
  else
    if (debug_widget)
      printf ("yes emacs shell.\n");
# endif
#endif /* EXTERNAL_WIDGET */

  *mask |= CWEventMask;
  XtCreateWindow (widget, InputOutput, (Visual *)CopyFromParent, *mask,
		  attrs);

#ifdef EXTERNAL_WIDGET
  if (emshell)
    EmacsShellReady (emshell, XtWindow (widget), KeyPressMask);
#endif

  update_wm_hints (ew);
}

extern void free_screen_faces (struct screen *);

static void
EmacsScreenDestroy (Widget widget)
{
  EmacsScreen ew = (EmacsScreen) widget;
  struct screen* s = ew->emacs_screen.screen;

  if (! s) abort ();
  if (! s->display.x) abort ();

  /* this would be called from Fdelete_screen() but it needs to free some
     stuff after the widget has been finalized but before the widget has
     been freed. */
  free_screen_faces (s);
}

void
EmacsScreenResize (Widget widget)
{
  EmacsScreen ew = (EmacsScreen)widget;
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
  EmacsScreen cur = (EmacsScreen)cur_widget;
  EmacsScreen new = (EmacsScreen)new_widget;

  Boolean needs_a_refresh = False;
  Boolean has_to_recompute_size;
  Boolean has_to_recompute_gcs = 0;
  Boolean has_to_update_hints;

  int char_width, char_height;
  Dimension pixel_width;
  Dimension pixel_height;
  
#ifdef I18N4
  has_to_recompute_gcs = ((cur->emacs_screen.foreground_pixel
			      != new->emacs_screen.foreground_pixel)
			  || (cur->core.background_pixel
			      != new->core.background_pixel)
			  );
#else
  has_to_recompute_gcs = (cur->emacs_screen.font != new->emacs_screen.font
			  || (cur->emacs_screen.foreground_pixel
			      != new->emacs_screen.foreground_pixel)
			  || (cur->core.background_pixel
			      != new->core.background_pixel)
			  );
#endif
  has_to_recompute_size = (cur->emacs_screen.font != new->emacs_screen.font
			   && cur->core.width == new->core.width
			   && cur->core.height == new->core.height);

  has_to_update_hints = (cur->emacs_screen.font != new->emacs_screen.font);

  if (has_to_recompute_gcs)
    {
      needs_a_refresh = True;
    }

  if (has_to_recompute_size)
    {
#if 0
      /* I think none of this junk is necessary.
	 When the font of the screen changes, it happens via the call to
	 XtSetValues() in update_EmacsScreen() in faces.c.  Which then does
	 Fset_screen_size() to make sure that the pixel size of the screen
	 is still sensible.  This code was apparently trying to do that
	 itself, in a kind of strange way.  (Fset_screen_size() will
	 eventually call EmacsScreenSetCharSize() in this file.)

	 It's probably appropriate that changing the font of the screen
	 via the higher level face mechanism may cause a resize, but simply
	 setting XtNfont doesn't.				-jwz
       */


      /* note that cur and new have the same w/h here because of how
	 has_to_recompute_size was computed. */
      pixel_width = cur->core.width;
      pixel_height = cur->core.height;
      pixel_to_char_size (cur, pixel_width, pixel_height, &char_width,
			  &char_height);
      char_to_pixel_size (new, char_width, char_height, &pixel_width,
			  &pixel_height);
      new->core.width = pixel_width;
      new->core.height = pixel_height;

      change_screen_size (new->emacs_screen.screen, char_height, char_width,
			  0);
#endif
      needs_a_refresh = True;
    }

  if (has_to_update_hints)
    update_wm_hints (new);

  update_various_screen_slots (new);

  if (cur->emacs_screen.iconic != new->emacs_screen.iconic)
    set_wm_shell_iconic_p (get_wm_shell ((Widget) cur),
			   new->emacs_screen.iconic);

  return needs_a_refresh;
}

static XtGeometryResult
EmacsScreenQueryGeometry (Widget widget, XtWidgetGeometry* request,
			  XtWidgetGeometry* result)
{
  EmacsScreen ew = (EmacsScreen)widget;

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


/* I don't know why this is necessary; Matthieu said he had to do
   it to make the focus handlers work??
 */
static void
key_press (Widget w, XEvent* event, String *params, Cardinal *n_params)
{
}

static void
emacs_screen_focus_handler (Widget w, XEvent *event, String *params,
			    Cardinal *n_params)
{
  struct screen *s = x_any_window_to_screen (event->xfocus.window);

  if (!s) return;	/* Does this happen?  What does it mean? */

#ifdef EXTERNAL_WIDGET
  /* External widget lossage: Ben or Chuck said:
     YUCK.  The only way to make focus changes work properly is to
     completely ignore all FocusIn/FocusOut events and depend only
     on notifications from the EmacsClient widget. */
  if (s->display.x->emacs_shell_p)
    return;
#endif

  emacs_Xt_focus_event_handler (event, 0);
}

/* Special entrypoints */
void
EmacsScreenSetCharSize (Widget widget, int columns, int rows)
{
  EmacsScreen ew = (EmacsScreen) widget;
  Dimension pixel_width, pixel_height;
  if (columns < 3) columns = 3;  /* no way buddy */
  if (rows < 3) rows = 3;

  char_to_pixel_size (ew, columns, rows, &pixel_width, &pixel_height);

#ifdef LWLIB_USES_MOTIF
  /* For Motif, it's easy: */
  XtVaSetValues ((Widget) ew,
		 XtNwidth, pixel_width,
		 XtNheight, pixel_height,
		 0);

#else /* Athena */
  {
    /* For Athena, we must sacrifice a goat to the vengeful, angry gods
       of geometry management: */
    struct screen *s = ew->emacs_screen.screen;
    int old_container_width   = s->display.x->container->core.width;
    int old_container_height  = s->display.x->container->core.height;
    int old_container2_width  = s->display.x->container2->core.width;
    int old_container2_height = s->display.x->container2->core.height;
    int wdelta = pixel_width  - ew->core.width;
    int hdelta = pixel_height - ew->core.height;

    if (wdelta != 0 || hdelta != 0)
      {
	XawPanedSetRefigureMode (s->display.x->container, False);
	XawPanedSetRefigureMode (s->display.x->container2, False);

	XtVaSetValues ((Widget) ew,
		       XtNwidth,  pixel_width,
		       XtNheight, pixel_height,
		       0);

	XtVaSetValues (s->display.x->container2,
		       XtNwidth,  old_container2_width + wdelta,
		       XtNheight, old_container2_height + hdelta,
		       0);
	XtVaSetValues (s->display.x->container,
		       XtNwidth,  old_container_width + wdelta,
		       XtNheight, old_container_height + hdelta,
		       0);

	XawPanedSetRefigureMode (s->display.x->container2, True);
	XawPanedSetRefigureMode (s->display.x->container, True);
      }
  }
#endif /* Athena */

  /* We have just called XtVaSetValues() on the EmacsScreen.
     It's size had better be what we set it to.
     (Unless it has not yet been mapped, in which case its
     size does not change but that's ok.)
   */
/* ####
  if (XtIsManaged (get_wm_shell ((Widget) ew)) &&
      (ew->core.width != pixel_width ||
       ew->core.height != pixel_height))
    abort ();
 */
}
