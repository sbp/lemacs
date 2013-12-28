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

/* #### Note to potential hackers: Don't mess with this unless you're
   sure you know what you're doing!  Xt is a lot more subtle than
   you may think. */

#include "config.h"
#include "intl.h"
#include "lisp.h"

#include <stdio.h>
#include "xterm.h"
#include "xobjs.h"
#include "dispextern.h"
#include "screen.h"
#include "faces.h"

#include <X11/StringDefs.h>
#include <X11/Shell.h>
#ifndef LWLIB_USES_MOTIF /* Athena */
# include "EmacsManager.h"
#endif /* Athena */

#include "EmacsScreenP.h"

#define max(a, b) ((a) > (b) ? (a) : (b))

void emacs_Xt_focus_event_handler (XEvent *x_event, struct screen *s);

static void EmacsScreenInitialize (Widget, Widget, ArgList, Cardinal *);
static void EmacsScreenDestroy (Widget);
static void EmacsScreenRealize (Widget, XtValueMask*, XSetWindowAttributes*);
static void EmacsScreenResize (Widget widget);
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
  {XtNinitiallyUnmapped, XtCInitiallyUnmapped, XtRBoolean, sizeof (Boolean),
     offset (initially_unmapped), XtRImmediate, (XtPointer)0},
  {XtNminibuffer, XtCMinibuffer, XtRInt, sizeof (int),
     offset (minibuffer), XtRImmediate, (XtPointer)0},
  {XtNunsplittable, XtCUnsplittable, XtRBoolean, sizeof (Boolean),
     offset (unsplittable), XtRImmediate, (XtPointer)0},
  {XtNinternalBorderWidth, XtCInternalBorderWidth, XtRInt, sizeof (int),
     offset (internal_border_width), XtRImmediate, (XtPointer)4},
  {XtNscrollBarWidth, XtCScrollBarWidth, XtRInt, sizeof (int),
     offset (scrollbar_width), XtRImmediate, (XtPointer)-1},
  {XtNinterline, XtCInterline, XtRInt, sizeof (int),
     offset (interline), XtRImmediate, (XtPointer)0},
  {
#ifdef I18N4
    XtNfontSet, XtCFontSet, XtRFontSet,    sizeof(XFontSet),
#else
    XtNfont,    XtCFont,    XtRFontStruct, sizeof(XFontStruct *),
#endif
    offset(font), XtRImmediate, (XtPointer)0
  },
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
  {XtNuseBackingStore, XtCUseBackingStore, XtRBoolean, sizeof (Boolean),
     offset (use_backing_store), XtRImmediate, (XtPointer)0},
  {XtNpreferredWidth, XtCPreferredWidth, XtRDimension, sizeof (Dimension),
     offset (preferred_width), XtRImmediate, (XtPointer)0},
  {XtNpreferredHeight, XtCPreferredHeight, XtRDimension, sizeof (Dimension),
     offset (preferred_height), XtRImmediate, (XtPointer)0},
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

/* If we're running under Motif, make this widget a subclass
   of XmPrimitive.  It's not clear this is necessary, but it
   may make focus behavior work better. */

EmacsScreenClassRec emacsScreenClassRec = {
    { /* core fields */
#ifdef LWLIB_USES_MOTIF
    /* superclass		*/	(WidgetClass) &xmPrimitiveClassRec,
#else
    /* superclass		*/	&widgetClassRec,
#endif
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
    },
#ifdef LWLIB_USES_MOTIF
    {	/* XmPrimitiveClassPart
	 */
      (XtWidgetProc) _XtInherit,	/* border_highlight */
      (XtWidgetProc) _XtInherit,	/* border_unhighlight */
      /* Setting the following to NULL causes PrimitiveInitialize()
	 not to add traversal (TAB etc. to switch focus) and
	 focus-in/out (border highlight/unhighlight) translations.
	 If you want those translations, use the value XtInheritTranslations
	 instead.  Doing this, however, will interfere with Emacs
	 focus handling (which highlights/unhighlights the text cursor),
	 and will lead to strange display results around the border of the
	 widget. */
      NULL,				/* translations */
      NULL,				/* arm_and_activate */
      NULL,				/* get resources */
      0,				/* num get_resources */
      NULL,				/* extension */
    },
#endif /* LWLIB_USES_MOTIF */
    {
      0
    }
};
WidgetClass emacsScreenClass = (WidgetClass) &emacsScreenClassRec;

static void
get_default_char_pixel_size (EmacsScreen ew, int* pixel_width,
			     int* pixel_height)
{
  struct Lisp_Font *font;

  if (NILP (SCREEN_DEFAULT_FONT (ew->emacs_screen.screen)) ||
      ! (font = XFONT (SCREEN_DEFAULT_FONT (ew->emacs_screen.screen))) ||
      ! font->width ||
      ! font->height)
    abort ();

  *pixel_height = font->height;
  *pixel_width = font->width;

  /* there used to be code here that calculated the width of the 'n' 
     character.  Look at Fmake_font() for where this is done now. */
}

/* This takes the size in pixels of the text area, and returns the number
   of characters that will fit there, taking into account the internal
   border width, and the pixel width of the line terminator glyphs (which
   always count as one "character" wide, even if they are not the same size
   as the default character size of the default font.)

   Therefore the result is not necessarily a multiple of anything in
   particular.
 */
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

/* Given a character size, this returns the minimum number of pixels necessary
   to display that many characters, taking into account the internal border
   width, and the size of the line terminator glyphs (assuming the line
   terminators take up exactly one character position.)

   Therefore the result is not necessarily a multiple of anything in
   particular.
 */
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

/* Given a pixel size, rounds DOWN to the smallest size in pixels necessary
   to display the same number of characters as are displayable now.
 */
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
    fatal ("can't create an emacs screen widget without a screen.");
}

static void
EmacsScreenRealize (Widget widget, XtValueMask *mask,
		    XSetWindowAttributes *attrs)
{
  /* #### I'm not convinced that it is really necessary to
     have a custom Realize() method.  This will especially
     be the case once the event loop is rewritten to always
     go through XtDispatchEvent(). */

  EmacsScreen ew = (EmacsScreen)widget;

  attrs->event_mask = (ExposureMask | StructureNotifyMask |
		       VisibilityChangeMask | PropertyChangeMask |
		       StructureNotifyMask | SubstructureNotifyMask |
		       SubstructureRedirectMask | KeyPressMask |
		       ButtonPressMask | ButtonReleaseMask |
		       FocusChangeMask | PointerMotionHintMask |
		       PointerMotionMask | LeaveWindowMask |
		       EnterWindowMask);

#ifdef I18N4
  /* Make sure that events wanted by the input method are selected. */
  attrs->event_mask |= input_method_event_mask;
#endif

  *mask |= CWEventMask;

  if (ew->emacs_screen.use_backing_store)
    {
      attrs->backing_store = Always;
      *mask |= CWBackingStore;
    }
  XtCreateWindow (widget, InputOutput, (Visual *)CopyFromParent, *mask,
		  attrs);
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

/* DO NOT CALL THIS FUNCTION!  Only Xt is supposed to do this. */

static void
EmacsScreenResize (Widget widget)
{
  EmacsScreen ew = (EmacsScreen)widget;
  struct screen *s = ew->emacs_screen.screen;
  int columns;
  int rows;
  
  pixel_to_char_size (ew, ew->core.width, ew->core.height, &columns, &rows);
  change_screen_size (s, rows, columns, 0);
  update_various_screen_slots (ew);

  /* Now we tell the EmacsShell that we've changed the size of the non-fixed
     portion of the screen.  Note that, if we the resize occurred as a result
     of EmacsScreenSetCharSize(), this information will be stored twice.
     This is not a big deal, as storing this information doesn't actually
     do anything until the next resize. */
  if (s->display.x->top_level_screen_p)
    x_wm_set_variable_size (s->display.x->widget, columns, rows);
}

static Boolean
EmacsScreenSetValues (Widget cur_widget, Widget req_widget, Widget new_widget,
		      ArgList dum1, Cardinal *dum2)
{
  EmacsScreen cur = (EmacsScreen)cur_widget;
  EmacsScreen new = (EmacsScreen)new_widget;

  /* BPW: this function used to do way too much stuff.  I've gotten
     rid of the junk.  Pretty much everything interesting will get
     done in the resize method, which will (if necessary) get called
     by Xt when this function returns (see below).
   */

  /* #### This function will not work if it is not called from
     update_EmacsScreen(), called from Fset_face_attribute_internal().
     The code located there should be moved inside of here instead,
     so that things work if either Fset_face_attribute_internal() is
     called or XtSetValues() is called.
     */

  if (cur->emacs_screen.iconic != new->emacs_screen.iconic &&
      new->emacs_screen.screen->display.x->top_level_screen_p)
    x_wm_set_shell_iconic_p (new->emacs_screen.screen->display.x->widget,
			     new->emacs_screen.iconic);

  return False;

  /* Note that if either (a) we return True, or (b) the width or
     height has changed, an Expose event will be generated.  The Xt
     manual says you should not return True if the width or height has
     changed, because then two Expose events will be generated.

     In any case, there is no need to return True because
     Fset_face_attribute_internal(), which does the resource
     setting, automatically forces a redisplay as necessary. */
}

static XtGeometryResult
EmacsScreenQueryGeometry (Widget widget, XtWidgetGeometry* request,
			  XtWidgetGeometry* result)
{
  EmacsScreen ew = (EmacsScreen)widget;
  int mask = request->request_mode;
  Dimension width, height;
  Dimension ok_width, ok_height;

  /* We have a definite preference for what size we would like
     to be.

     1) If a preferred size was specified for us, use it.
        (This is used when managing or unmanaging the menubar)
     2) If a proposed size was given, round it to the nearest
        multiple of the default char size and return it.
     3) Otherwise, take our current size and round it to the
        nearest multiple of the default char size. */

  width = mask & CWWidth ? request->width : ew->core.width;
  height = mask & CWHeight ? request->height : ew->core.height;
  round_size_to_char (ew, width, height, &ok_width, &ok_height);
  if (ew->emacs_screen.preferred_width)
    ok_width = ew->emacs_screen.preferred_width;
  if (ew->emacs_screen.preferred_height)
    ok_height = ew->emacs_screen.preferred_height;
  result->request_mode |= CWWidth | CWHeight;
  result->width = ok_width;
  result->height = ok_height;
  if (((mask & CWWidth) && ok_width != request->width)
      || ((mask & CWHeight) && ok_height != request->height))
    return XtGeometryAlmost;
  else
    return XtGeometryYes;
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
  /* External widget lossage: Ben said:
     YUCK.  The only way to make focus changes work properly is to
     completely ignore all FocusIn/FocusOut events and depend only
     on notifications from the ExternalClient widget. */
  if (s->display.x->external_window_p)
    return;
#endif

  emacs_Xt_focus_event_handler (event, 0);
}

/********************* Special entrypoints *******************/

void
EmacsScreenRecomputeCellSize (Widget w)
{
  EmacsScreen ew = (EmacsScreen) w;
  int cw;
  int ch;

  if (! XtIsSubclass (w, emacsScreenClass))
    abort ();

  get_default_char_pixel_size (ew, &cw, &ch);
  x_wm_set_cell_size (ew->emacs_screen.screen->display.x->widget, cw, ch);
}

/* Set the size of the widget to have the number of rows and columns
   specified.  This both causes the X window to change and the
   internal screen structures to get modified to match. */

void
EmacsScreenSetCharSize (Widget widget, int columns, int rows)
{
  EmacsScreen ew = (EmacsScreen) widget;
  Dimension pixel_width, pixel_height;
  struct x_display *x = ew->emacs_screen.screen->display.x;
  if (columns < 3) columns = 3;  /* no way buddy */
  if (rows < 3) rows = 3;

  char_to_pixel_size (ew, columns, rows, &pixel_width, &pixel_height);

  if (x->top_level_screen_p)
    x_wm_set_variable_size (x->widget, columns, rows);

  XtVaSetValues ((Widget) ew,
		 XtNwidth, pixel_width,
		 XtNheight, pixel_height,
		 0);
}
