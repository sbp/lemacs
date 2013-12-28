/* Functions for the X window system.
   Copyright (C) 1989, 1992 Free Software Foundation.

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

/* Substantially rewritten for Lucid GNU Emacs.  */

#include <stdio.h>
#include <signal.h>
#include "config.h"

#include <X11/IntrinsicP.h>	/* CoreP.h needs this */
#include <X11/CoreP.h>		/* foul, but we need this to use our own
				   window inside a widget instead of one 
				   that Xt creates... */
#include <X11/StringDefs.h>
#include <X11/Shell.h>

#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Label.h>

#include "ScreenWidgetP.h"

#ifdef LINE_INFO_WIDGET
# include "LineInfoWidget.h"
# include "LineInfoWidgetP.h"
#endif

#include "EmacsShell.h"
#include "EmacsShellP.h"

#ifdef ENERGIZE
#include "dbox.h"
#endif

#ifdef USE_SPARC_SOUND
# include <netdb.h>
#endif

#include "lisp.h"
#include "xterm.h"
#include "window.h"
#include "buffer.h"
#include "extents.h"
#include "screen.h"
#include "events.h"
#include "faces.h"

#ifdef HAVE_X_WINDOWS
extern void abort ();

void x_set_screen_param ();

#define min(a,b) ((a) < (b) ? (a) : (b))
#define max(a,b) ((a) > (b) ? (a) : (b))
#define COLOR_SCREEN_P(d) (XCellsOfScreen (DefaultScreenOfDisplay (d)) > 2)

/* X Resource data base */
static XrmDatabase xrdb;

/* Title name and application name for X stuff. */
extern char *id_name;

Lisp_Object Vx_gc_pointer_shape;

/* If non-nil, use vertical bar cursor. */
Lisp_Object Vbar_cursor;

/* The X Visual we are using for X windows (the default) */
Visual *screen_visual;

/* How many screens this X display has. */
int x_screen_count;

/* The vendor supporting this X server. */
Lisp_Object Vx_vendor;

/* The vendor's release number for this X server. */
int x_release;

/* Height of this X screen in pixels. */
int x_screen_height;

/* Height of this X screen in millimeters. */
int x_screen_height_mm;

/* Width of this X screen in pixels. */
int x_screen_width;

/* Width of this X screen in millimeters. */
int x_screen_width_mm;

/* Does this X screen do backing store? */
Lisp_Object Vx_backing_store;

/* Does this X screen do save-unders? */
int x_save_under;

/* Number of planes for this screen. */
int x_screen_planes;

/* X Visual type of this screen. */
Lisp_Object Vx_screen_visual;

/* Where bitmaps are; initialized from resource database */
Lisp_Object Vx_bitmap_file_path;

Atom Xatom_WM_TAKE_FOCUS, Xatom_WM_SAVE_YOURSELF, Xatom_WM_DELETE_WINDOW,
 Xatom_WM_PROTOCOLS, Xatom_CLIPBOARD;

static char *x_visual_strings[] =
  {
    "StaticGray",
    "GrayScale",
    "StaticColor",
    "PseudoColor",
    "TrueColor",
    "DirectColor"
  };

Lisp_Object Qundefined_color;
Lisp_Object Qx_resource_name;

extern Lisp_Object Vwindow_system_version;

/* Default parameters to use when creating screens.  */
Lisp_Object Vx_screen_defaults;

/* Do we accept events send by other clients? */
int x_allow_sendevents;


/* Return the Emacs screen-object corresponding to an X window */
struct screen *
x_window_to_screen (wdesc)
     Window wdesc;
{
  Lisp_Object tail, screen;
  struct screen *s;

  for (tail = Vscreen_list; CONSP (tail); tail = XCONS (tail)->cdr)
    {
      screen = XCONS (tail)->car;
      if (XTYPE (screen) != Lisp_Screen)
        continue;
      s = XSCREEN (screen);
      if (SCREEN_IS_X (s) && XtWindow (s->display.x->edit_widget) == wdesc)
        return s;
    }
  return 0;
}

/* Like x_window_to_screen but also compares the window with the widget's
   widows */
struct screen *
x_any_window_to_screen (wdesc)
     Window wdesc;
{
  Lisp_Object tail, screen;
  struct screen *s;
  struct x_display *x;

  for (tail = Vscreen_list; CONSP (tail); tail = XCONS (tail)->cdr)
    {
      screen = XCONS (tail)->car;
      if (XTYPE (screen) != Lisp_Screen)
        continue;
      s = XSCREEN (screen);
      if (!SCREEN_IS_X (s))
	continue;
      x = s->display.x;
      /* This screen matches if the window is any of its widgets. */
      if (wdesc == XtWindow (x->widget) ||
	  wdesc == XtWindow (x->column_widget) ||
	  wdesc == XtWindow (x->edit_widget))
	return s;
      /* Match if the window is this screen's menubar. */
      if (x->menubar_widget &&
	  wdesc == XtWindow (x->menubar_widget))
	return s;
      /* Do *not* match if the window is this screen's psheet. */
    }
  return 0;
}

Lisp_Object text_part_sym;
Lisp_Object modeline_part_sym;


/* Connect the screen-parameter names (symbols) to the corresponding
   X Resource Manager names.  The name of a parameter, as a Lisp symbol,
   has an `x-resource-name' property which is a Lisp_String. */

init_x_parm_symbols ()
{
  register Lisp_Object propname;
  Qx_resource_name = propname = intern ("x-resource-name");

#define def(sym, rsrc) \
  Fput (intern (sym), propname, build_string (rsrc))
#define defi(sym,rsrc) \
    def (sym, rsrc); Fput (intern (sym), Qintegerp, Qt)

  def ("cursor-color", XtNcursorColor);
  def ("border-color", XtNborderColor);
  defi("border-width", XtNborderWidth);
  defi("internal-border-width", XtNinternalBorderWidth);
/*  def ("icon-type", "iconType"); */
#ifdef LINE_INFO_COLUMN
  defi("line-info-column-width", XtNlineInfoColumnWidth);
  def ("line-info-column-foreground-color", XtNlineInfoColumnForeground);
  def ("line-info-column-background-color", XtNlineInfoColumnBackground);
#endif
/*  def ("vertical-scroll-bar", XtNverticalScrollBar); */
/*  def ("horizontal-scroll-bar", XtNhorizontalScrollBar); */
  def ("minibuffer", XtNminibuffer);
  def ("unsplittable", XtNunsplittable);
  defi("inter-line-space", XtNinterline);
  def ("bar-cursor", XtNbarCursor);
  def ("visual-bell", XtNvisualBell);
  defi("bell-volume", XtNbellVolume);
  
#undef def
}


/* Insert a description of internally-recorded parameters of screen X
   into the parameter alist *ALISTPTR that is to be given to the user.
   Only parameters that are specific to the X window system
   and whose values are not correctly recorded in the screen's
   param_alist need to be considered here.  */

extern Lisp_Object Vinvocation_name;

static void
color_to_string (w, pixel, buf)
     Widget w;
     unsigned long pixel;
     char *buf;
{
  XColor color;
  color.pixel = pixel;
  BLOCK_INPUT;
  XQueryColor (XtDisplay (w), w->core.colormap, &color);
  UNBLOCK_INPUT;
  sprintf (buf, "#%04x%04x%04x", color.red, color.green, color.blue);
}


x_report_screen_params (s, alistptr)
     struct screen *s;
     Lisp_Object *alistptr;
{
  TopLevelShellWidget shell = (TopLevelShellWidget)s->display.x->widget;
  EmacsScreenWidget w = (EmacsScreenWidget)s->display.x->edit_widget;
  char buf [255];

#define store_int(sym, slot) \
  store_in_alist (alistptr, sym, make_number (slot))
#define store_str(sym, slot) \
  store_in_alist (alistptr, sym, build_string (slot))
#define store_bool(sym, slot) \
  store_in_alist (alistptr, sym, (slot) ? Qt : Qnil)
#define store_color(sym, slot) \
  color_to_string ((Widget) w, slot, buf); \
  store_in_alist (alistptr, sym, build_string (buf))
#define store_curs(sym, slot) /* #### don't have the strings any more... */

  store_color ("cursor-color", w->emacs_screen.cursor_color);
  store_color ("border-color", w->core.border_pixel);
  store_int ("left", shell->core.x);
  store_int ("top", shell->core.y);
  store_int ("border-width", w->core.border_width);
  store_int ("internal-border-width", w->emacs_screen.internal_border_width);
  store_int ("inter-line-space", w->emacs_screen.interline);
  store_bool ("minibuffer", w->emacs_screen.minibuffer);
  store_bool ("unsplittable", w->emacs_screen.minibuffer);
  store_bool ("visual-bell", w->emacs_screen.visual_bell); 
  store_bool ("bar-cursor",  w->emacs_screen.bar_cursor);
  sprintf (buf, "0x%x", XtWindow (w));
  store_str ("window-id", buf);
}


/* Functions called only from `x_set_screen_param' to set
** individual parameters. */

static void
x_set_internal_border_width (s, arg)
     struct screen *s;
     Lisp_Object arg;
{
  CHECK_NUMBER (arg, 0);

  BLOCK_INPUT;
  XtVaSetValues (s->display.x->edit_widget, 
		 XtNinternalBorderWidth, XINT (arg));
  UNBLOCK_INPUT;
}


void 
x_set_title_from_char (struct screen* s, char* name)
{
  char *old_name = 0;
  Arg av [1];
  BLOCK_INPUT;
  XtSetArg (av[0], XtNtitle, &old_name);
  XtGetValues (s->display.x->widget, av, 1);
  if (!old_name || strcmp (name, old_name))
    {
      XtSetArg (av[0], XtNtitle, name);
      XtSetValues (s->display.x->widget, av, 1);
    }
  UNBLOCK_INPUT;
}

void
x_set_icon_name_from_char (struct screen* s, char* name)
{
  char *old_name = 0;
  Arg av [1];
  BLOCK_INPUT;
  XtSetArg (av[0], XtNiconName, &old_name);
  XtGetValues (s->display.x->widget, av, 1);
  if (!old_name || strcmp (name, old_name))
    {
      XtSetArg (av[0], XtNiconName, name);
      XtSetValues (s->display.x->widget, av, 1);
    }
  UNBLOCK_INPUT;
}

/* Report to X that a screen parameter of screen S is being set or changed.
   If the parameter is not specially recognized, do nothing.
 */

void
x_set_screen_values (s, alist)
     struct screen *s;
     Lisp_Object alist;
{
  Lisp_Object tail;
  Widget w = s->display.x->edit_widget;
  
  for (tail = alist; !EQ (tail, Qnil); tail = Fcdr (tail))
    {
      Lisp_Object elt = Fcar (tail);
      Lisp_Object prop = Fcar (elt);
      Lisp_Object val = Fcdr (elt);
      
      if (XTYPE (prop) == Lisp_String)
	{
	  if (XSTRING (prop)->size == 0)
	    continue;

	  BLOCK_INPUT;
	  if (XTYPE (val) == Lisp_String)
	    XtVaSetValues (w, XtVaTypedArg, XSTRING (prop)->data, XtRString,
			   XSTRING (val)->data, XSTRING (val)->size + 1,
			   0);
	  else
	    XtVaSetValues (w, XtVaTypedArg,
			   XSTRING (prop)->data, XtRInt, XINT (val),
			   sizeof (int),
			   0);
	  UNBLOCK_INPUT;
	}
      else
	{
	  Lisp_Object str = Fget (prop, Qx_resource_name);
	  int int_p = !NILP (Fget (prop, Qintegerp));

	  if (NILP (str))
	    {
	      if (!EQ (prop, intern ("menubar")))
		store_screen_param (s, prop, val);
	      continue;
	    }

	  CHECK_STRING (str, 0);
	  BLOCK_INPUT;
	  if (int_p)
	    {
	      CHECK_NUMBER (val, 0);
	      XtVaSetValues (w, (char *) XSTRING (str)->data, XINT (val),
			     0);
	    }
	  else if (EQ (val, Qt))
	    XtVaSetValues (w, (char *) XSTRING (str)->data, /* XtN... */
			   True,
			   0);
	  else if (EQ (val, Qnil))
	    XtVaSetValues (w, (char *) XSTRING (str)->data, /* XtN... */
			   False,
			   0);
	  else
	    {
	      CHECK_STRING (val, 0);
	      XtVaSetValues (w, XtVaTypedArg,
			     (char *) XSTRING (str)->data, /* XtN... */
			     XtRString,
			     XSTRING (val)->data, XSTRING (val)->size + 1,
			     0);
	    }
	  UNBLOCK_INPUT;
	}
    }
}

void
fix_pane_constraints (w)
  Widget w;
{
  BLOCK_INPUT;
  XtVaSetValues (w, XtNshowGrip, 0, XtNresizeToPreferred, 1,
		 XtNallowResize, 1, 0);
  UNBLOCK_INPUT;
}

/* remove this when we do not use uilib anymore */
void
sheet_callback (){}

#ifdef ENERGIZE

extern int *get_psheets_for_buffer ();

void
recompute_screen_menubar (screen)
     struct screen *screen;
{
  /* #### This shouldn't be necessary any more */
}


/* This function is invoked by each menu item which Energize should handle.
   Defined in xterm.c.
 */
extern void client_menu_item_cb ();

void
make_psheets_desired (s, buffer)
     struct screen* s;
     Lisp_Object buffer;
{
  struct x_display *x = s->display.x;
  int count;
  int i;
  int *psheets;

  if (NILP (buffer) || !(psheets = get_psheets_for_buffer (buffer, &count)))
    {
      x->desired_psheets = 0;
      x->desired_psheet_count = 0;
      x->desired_psheet_buffer = Qnil;
    }
  else
    {
      /* Do not show the debugger panel in this function.  The
       * debugger panel should never be listed in the visible psheets. */
      extern int debuggerpanel_sheet;
      
      if (count == 1 && psheets [0] == debuggerpanel_sheet)
	return;

      x->desired_psheets = psheets;
      x->desired_psheet_count = count;
      x->desired_psheet_buffer = buffer;
    }
}

Lisp_Object
desired_psheet_buffer (struct screen* s)
{
  return s->display.x->desired_psheet_buffer;
}

/* This function is invoked when the user clicks on the "sheet" button.
 */
DEFUN ("energize-toggle-psheet", Fenergize_toggle_psheet,
       Senergize_toggle_psheet, 0, 0, "",
       "")
     ()
{
  struct screen *screen = selected_screen;
  Lisp_Object buffer = Fwindow_buffer (Fselected_window ());
  if (EQ (buffer, desired_psheet_buffer (screen)))
    make_psheets_desired (screen, Qnil);
  else
    make_psheets_desired (screen, buffer);
  return Qnil;
}


void energize_show_menubar_of_buffer ();

/* This is called when a buffer becomes visible in some window.

   Show the menubar associated with this buffer, and show the psheets as
   well if this buffer is the last buffer whose psheets were visible in
   this screen.
 */
void energize_buffer_shown_hook (window)
     struct window *window;
{
  struct screen* screen = XSCREEN (window->screen);
  Lisp_Object buffer = window->buffer;
  Lisp_Object pbuf;

  if (! SCREEN_IS_X (screen)) return;
  pbuf = desired_psheet_buffer (screen);

  if (!MINI_WINDOW_P (window))
    energize_show_menubar_of_buffer (window->screen, buffer,
				     (EQ (buffer, pbuf) ? Qt : Qnil));
}


static int
find_buffer_in_different_window (window, buffer, not_in)
     struct window* window;
     Lisp_Object buffer;
     struct window* not_in;
{
  Lisp_Object child;
  if (!NILP (window->buffer))
    {
      /* a leaf window */
      return window->buffer == buffer && window != not_in;
    }
  else
    {
      /* a non leaf window, visit either the hchild or the vchild */
      for (child = !NILP (window->vchild) ? window->vchild : window->hchild;
	   !NILP (child);
	   child = XWINDOW (child)->next)
	{
	  if (find_buffer_in_different_window (XWINDOW (child), buffer,
					       not_in))
	    return 1;
	}
      return 0;
    }
}

/* returns 1 if the buffer is only visible in window on screen s */
static int
buffer_only_visible_in_this_window_p (buffer, s, window)
     Lisp_Object buffer;
     struct screen* s;
     struct window* window;
{
  return !find_buffer_in_different_window (XWINDOW (s->root_window), buffer,
					   window);
}

/* This is called just before a buffer which is visible becomes invisible,
   either because some other buffer is about to be made visible in its window,
   or because that window is being deleted.

   If this buffer's psheets are visible, hide them.
 */
void energize_buffer_hidden_hook (window)
     struct window *window;
{
  struct screen *s;
  Lisp_Object old_buffer;
  s = XSCREEN (window->screen);

  if (! SCREEN_IS_X (s)) return;

  /* hides the p_sheet if we are changing the buffer of the
   * selected window of the screen and the p_sheet where displayed */
  if (EQ (window->buffer, desired_psheet_buffer (s))
      && buffer_only_visible_in_this_window_p (window->buffer, s, window))
    make_psheets_desired (s, Qnil);
}


/* This is called just before the selected window is no longer the selected
   window because some other window is being selected.  The given window is
   not being deleted, it is merely no longer the selected one.

   This doesn't do anything right now.
 */
void energize_window_deselected_hook (window)
     struct window *window;
{
}


/* This is called just after a window has been selected.

   Show the menubar associated with this buffer; leave the psheets as
   they are.
 */
void energize_window_selected_hook (window)
     struct window *window;
{
  struct screen* screen = XSCREEN (window->screen);
  Lisp_Object buffer = window->buffer;

  if (SCREEN_IS_X (screen) && !MINI_WINDOW_P (window))
    energize_show_menubar_of_buffer (window->screen, buffer, Qnil);
}



int current_debuggerpanel_exposed_p;
int desired_debuggerpanel_exposed_p;
int debuggerpanel_sheet;

void
energize_show_menubar_of_buffer (screen, buffer, psheets_too)
     Lisp_Object screen, buffer, psheets_too;
{
  struct screen* s;
  struct x_display *x;
  Lisp_Object menubar;
  
  if (NILP (screen))
    s = selected_screen;
  else {
    CHECK_SCREEN (screen, 0);
    s = XSCREEN (screen);
  }

  if (! SCREEN_IS_X (s)) error ("not an X screen");
  x = s->display.x;

  if (! NILP (psheets_too))
    {
      Lisp_Object buffer;
      XSET (buffer, Lisp_Buffer, current_buffer);
      make_psheets_desired (s, buffer);
    }
}


#endif /* ENERGIZE */

/* The one and only application context associated with the connection
** to the one and only X display that Emacs uses. */
XtAppContext Xt_app_con;

/* The one and only application shell.  Emacs screens are popup shells of this
** application. */
Widget Xt_app_shell;

extern Lisp_Object Vscreen_title_format, Vscreen_icon_title_format;
extern Lisp_Object Vscreen_list;

static void
maybe_set_screen_title_format (shell)
     Widget shell;
{
  BLOCK_INPUT;

  if (NILP (Vscreen_list) ||
      (NILP (Fcdr (Vscreen_list)) &&
       XSCREEN (Fcar (Vscreen_list))->display.nothing == 1))
    /* Only do this if this is the first X screen we're creating.
       If the *title resource (or -title option) was specified, then
       set screen-title-format to its value.
     */
    {
      /* No doubt there's a less stupid way to do this. */
      char *results [2];
      XtResource resources [2];
      results [0] = results [1] = 0;
      resources [0].resource_name = XtNtitle;
      resources [0].resource_class = XtCTitle;
      resources [0].resource_type = XtRString;
      resources [0].resource_size = sizeof (String);
      resources [0].resource_offset = 0;
      resources [0].default_type = XtRString;
      resources [0].default_addr = 0;
      resources [1].resource_name = XtNiconName;
      resources [1].resource_class = XtCIconName;
      resources [1].resource_type = XtRString;
      resources [1].resource_size = sizeof (String);
      resources [1].resource_offset = sizeof (char *);
      resources [1].default_type = XtRString;
      resources [1].default_addr = 0;
      XtGetSubresources (XtParent(shell), (XtPointer)results, shell->core.name,
			 shell->core.widget_class->core_class.class_name,
			 resources, XtNumber (resources), 0, 0);
      if (results[0])
	Vscreen_title_format = build_string (results[0]);
      if (results[1])
	Vscreen_icon_title_format = build_string (results[1]);
    }
  UNBLOCK_INPUT;
}


/* Creates the widgets for a screen.  Parms is an alist of
resources/values to use for the screen.  (ignored right now).
reslisp_window_id is a Lisp description of an X window or Xt widget to
resParse.  (ignored right now).  */

static void
x_create_widgets (s, parms, lisp_window_id)
     struct screen *s;
     Lisp_Object parms;
     Lisp_Object lisp_window_id;
{
  Widget shell_widget;
  Widget pane_widget;
  Widget screen_widget;
  char* name;
  Arg al [25];
  int ac = 0;
  Atom properties [1];

  BLOCK_INPUT;

  if (XTYPE (s->name) == Lisp_String)
     name = (char*)XSTRING (s->name)->data;
  else
    name = "emacs";
       
  ac = 0;
  XtSetArg (al[ac], XtNallowShellResize, 1); ac++;
  XtSetArg (al[ac], XtNinput, 1); ac++;
  shell_widget = XtCreatePopupShell (name, emacsShellWidgetClass, Xt_app_shell,
				     al, ac);
  maybe_set_screen_title_format (shell_widget);

  ac = 0;
  XtSetArg (al[ac], XtNborderWidth, 0); ac++;
  pane_widget = XtCreateWidget ("pane", panedWidgetClass, shell_widget, al,
				ac);

  /* mappedWhenManaged to false tells to the paned window to not map/unmap 
   * the emacs screen when changing menubar.  This reduces flickering a lot.
   */
  ac = 0;
  XtSetArg (al[ac], XtNmappedWhenManaged, 0); ac++;
  XtSetArg (al[ac], XtNshowGrip, 0); ac++;
  XtSetArg (al[ac], XtNallowResize, 1); ac++;
  XtSetArg (al[ac], XtNresizeToPreferred, 1); ac++;
  XtSetArg (al[ac], XtNemacsScreen, s); ac++;
  screen_widget = XtCreateWidget ("screen", emacsScreenWidgetClass,
				  pane_widget, al, ac);

  s->display.x->widget = shell_widget;
  s->display.x->column_widget = pane_widget;
  s->display.x->edit_widget = screen_widget;

  if (NILP (Vx_screen_defaults))
    x_set_screen_values (s, parms);
  else
    x_set_screen_values (s, nconc2 (Fcopy_sequence (parms),
				    Vx_screen_defaults));

  XtManageChild (screen_widget);
  XtManageChild (pane_widget);
  XtRealizeWidget (shell_widget);

  properties [0] = Xatom_WM_TAKE_FOCUS;
  XChangeProperty (XtDisplay (shell_widget), XtWindow (shell_widget),
		   Xatom_WM_PROTOCOLS, XA_ATOM, 32, PropModeAppend,
		   (unsigned char*)properties, 1);

  XtMapWidget (screen_widget);

  XtPopup (shell_widget, XtGrabNone);
  UNBLOCK_INPUT;

#if 0
 * Forget this for now
 *  if (NILP(lisp_window_id))
 *     window_id = 0;
 *  else
 *    { CHECK_STRING(lisp_window_id, 0);
 *      string = (char *) (XSTRING(lisp_window_id)->data);
 *      if (string[0] == '0' && (string[1] == 'x' || string[1] == 'X'))
 *         sscanf(string+2, "%xu", &window_id);
 *#ifdef ENERGIZE
 *      else if (string[0] == 'w'){
 *	sscanf (string+1, "%x", &parent_widget);
 *	if (parent_widget)
 *	  window_id = XtWindow (parent_widget);
 *      }
 *#endif
 *      else
 *         sscanf (string, "%lu", &window_id);
 *    }
#endif
}

static void
allocate_x_display_struct (s)
     struct screen* s;
{
  s->output_method = output_x_window;
  s->display.x = (struct x_display *)xmalloc (sizeof (struct x_display));

  /* zero out all slots. */
  bzero (s->display.x, sizeof (struct x_display));
}

Lisp_Object Vcreate_screen_hook;

DEFUN ("x-create-screen", Fx_create_screen, Sx_create_screen,
       1, 2, 0,
  "Make a new X window, which is considered a \"screen\" in Emacs terms.\n\
Return an Emacs screen object representing the X window.\n\
ALIST is an alist of screen parameters.\n\
The value of `x-screen-defaults' is an additional alist\n\
of default parameters which apply when not overridden by ALIST.\n\
Optional second argument is the numerical ID of the X window to use for this\n\
screen (in order to run Emacs on a window created by some other program).\n\
Since this ID number is an unsigned long, you must pass it as a string.\n\
It may be a string of decimal numbers, or a string of hex numbers beginning\n\
with \"0x\"."
#ifdef ENERGIZE
"\nIn Energize it can be the address of a parent widget to\n\
use, it then has to begin with \"w\"."
#endif
)
  (parms, lisp_window_id)
     Lisp_Object parms, lisp_window_id;
{
  struct screen *s;
  Lisp_Object screen = Qnil;
  Lisp_Object name = Qnil;
  Lisp_Object menubar = Qnil;
  struct gcpro gcpro1;
  struct gcpro gcpro2;
  GCPRO2 (screen, name);
  
  if (x_current_display == 0)
    error ("X windows are not in use or not initialized");
  
  s = make_screen (1);
  
  allocate_x_display_struct (s);
  name = Fassq (intern ("name"), parms);
  menubar = Fassq (intern ("menubar"), parms);
  if (!NILP (name))
    {
      name = Fcdr (name);
      CHECK_STRING (name, 0);
    }
  else
    name = build_string ("emacs");

  s->name = name;

  XSET (screen, Lisp_Screen, s);

  x_create_widgets (s, parms, lisp_window_id);
  
  /* do this after anything that might call Fsignal() before the screen
   * is in a usable state. */
  Vscreen_list = Fcons (screen, Vscreen_list);

  /* This runs lisp code, and thus might GC.  If the selected screen is still
     the terminal screen (meaning that we're in the middle of creating the
     initial X screen) then select the X screen now, so that GC messages don't
     get written on the terminal screen.  This is kind of a hack...
   */
  if (selected_screen == XSCREEN (Vterminal_screen))
    select_screen (s);

  init_screen_faces (s);

  if (!NILP (menubar))
    Fset_screen_menubar (Fcdr (menubar), screen);

  x_format_screen_title (s, XWINDOW (s->selected_window));

  if (!NILP (Vcreate_screen_hook))
    call1 (Vcreate_screen_hook, screen);
  
  UNGCPRO;
  return screen;
}


DEFUN ("x-focus-screen", Fx_focus_screen, Sx_focus_screen, 1, 2, 0,
  "Obsolete function.  Superceded by select-screen")
     (focus_p, screen)
     Lisp_Object focus_p, screen;
{
  Lisp_Object screen_to_select;
  
  if (NILP (screen))
    {
      XSET (screen_to_select, Lisp_Screen, selected_screen);
    }
  else
    {
      CHECK_SCREEN (screen, 0);
      screen_to_select = screen;
    }
  Fselect_screen (screen_to_select);
  
  return screen_to_select;
}


Lisp_Object Vcurrent_menubar;

void
map_psheets (screen)
     struct screen *screen;
{
  /* #### obsolete */
}


DEFUN ("x-show-lineinfo-column",
       Fx_show_lineinfo_column, Sx_show_lineinfo_column, 0, 1, 0,
       "Make the current emacs screen have a lineinfo column.")
     (screen)
     Lisp_Object screen;
{
  struct screen* s;
  struct x_display *x;
  int just_created;
  
  if (NILP (screen))
    s = selected_screen;
  else {
    CHECK_SCREEN (screen, 0);
    s = XSCREEN (screen);
  }
  if (! SCREEN_IS_X (s)) error ("not an X screen");

  x = s->display.x;

#ifdef LINE_INFO_WIDGET
  
  BLOCK_INPUT;
  XawPanedSetRefigureMode (x->row_widget, 0);
  
  /* the order in which children are managed is the top to
     bottom order in which they are displayed in the paned window. */
  
  XtUnmanageChild (x->edit_widget);
  if (x->lineinfo_widget)
    XtUnmanageChild (x->lineinfo_widget);
  else {
    x->lineinfo_widget =
      XtVaCreateWidget ("lineinfo_widget", lineInfoWidgetClass,
			x->row_widget,
			XtNwidth,  50,
			XtNheight, s->display.x->pixel_height,
			XtNmappedWhenManaged, 0,
			XtNshowGrip, 0,
			0);
    ((LineInfoWidget) x->lineinfo_widget)->lineInfo.screen = s;
    just_created = 1;
  }
  XtManageChild (x->lineinfo_widget);
  XtManageChild (x->edit_widget);

  if (just_created)
    XStoreName (x_current_display, XtWindow(x->lineinfo_widget), "lineinfo_widget");

  XawPanedSetRefigureMode (x->row_widget, 1);

  UNBLOCK_INPUT;

#else
#ifdef LINE_INFO_COLUMN

  s->display.x->line_info_column_width =
    s->display.x->default_line_info_column_width;

#else
  error("support for the lineinfo column was not compiled into emacs.");

#endif
#endif
  return Qnil;
}


DEFUN ("x-hide-lineinfo-column",
       Fx_hide_lineinfo_column, Sx_hide_lineinfo_column, 0, 1, 0,
       "Make the given emacs screen not have a lineinfo column.")
     (screen)
{
  struct screen *s;
  
  if (NILP (screen))
    s = selected_screen;
  else {
    CHECK_SCREEN (screen, 0);
    s = XSCREEN (screen);
  }
  if (! SCREEN_IS_X (s)) error ("not an X screen");

#ifdef LINE_INFO_WIDGET
  if (! s->display.x->lineinfo_widget) return Qnil;
  XtUnmanageChild (s->display.x->lineinfo_widget);

#else
#ifdef LINE_INFO_COLUMN
  s->display.x->line_info_column_width = 0;

#else
  error("support for the lineinfo column was not compiled into emacs.");
#endif
#endif
  return Qnil;
}


DEFUN ("x-color-display-p", Fx_color_display_p, Sx_color_display_p, 0, 0, 0,
  "Return t if the X display used currently supports color.")
  ()
{
  if (XINT (x_screen_planes) <= 2)
    return Qnil;

  switch (screen_visual->class)
    {
    case StaticColor:
    case PseudoColor:
    case TrueColor:
    case DirectColor:
      return Qt;

    default:
      return Qnil;
    }
}

DEFUN ("x-pixel-width", Fx_pixel_width, Sx_pixel_width, 1, 1, 0,
  "Return the width in pixels of screen S.")
  (screen)
     Lisp_Object screen;
{
  CHECK_SCREEN (screen, 0);
  return make_number (PIXEL_WIDTH (XSCREEN (screen)));
}

DEFUN ("x-pixel-height", Fx_pixel_height, Sx_pixel_height, 1, 1, 0,
  "Return the height in pixels of screen S.")
  (screen)
     Lisp_Object screen;
{
  CHECK_SCREEN (screen, 0);
  return make_number (PIXEL_HEIGHT (XSCREEN (screen)));
}


DEFUN ("x-set-screen-icon-pixmap", Fx_set_screen_icon_pixmap,
       Sx_set_screen_icon_pixmap, 2, 3, 0,
       "Set the icon-pixmap of the given screen.\n\
This should be the name of a bitmap file, or a bitmap description list\n\
of the form (width height \"bitmap-data\").\n\
If the optional third argument is specified, it is the bitmap to use for\n\
the icon-pixmap-mask (not all window managers obey this.)\n\
Warning: when you call this function, the pixmap of the previous icon\n\
of this screen (if any) is currently not freed.")
  (screen, pixmap, mask)
    Lisp_Object screen, pixmap, mask;
{
  struct screen *s;
  Arg av [10];
  int ac = 0;
  int w, h, d;
  Pixmap p, m;
  if (NILP (screen))
    s = selected_screen;
  else
    {
      CHECK_SCREEN (screen, 0);
      s = XSCREEN (screen);
    }
  if (NILP (pixmap))
    p = 0;
  else
    p = (Pixmap) load_pixmap (s, pixmap, &w, &h, &d);
  if (NILP (mask))
    m = 0;
  else
    m = (Pixmap) load_pixmap (s, mask, &w, &h, &d);
  XtSetArg (av [ac], XtNiconPixmap, p); ac++;
  XtSetArg (av [ac], XtNiconMask, m); ac++;
  XtSetValues (s->display.x->widget, av, ac);
  return pixmap;
}


static Cursor grabbed_cursor;

DEFUN ("x-grab-pointer", Fx_grab_pointer, Sx_grab_pointer, 0, 2, 0,
  "Grab the pointer and restrict it to its current window.  If optional\n\
SHAPE is non-nil, change the pointer shape to that.  If second optional\n\
argument MOUSE-ONLY is non-nil, ignore keyboard events during the grab.")
  (shape, ignore_keyboard)
     Lisp_Object shape, ignore_keyboard;
{
  Window w;
  int pointer_mode, result;

  BLOCK_INPUT;
  if (! NILP (ignore_keyboard))
    pointer_mode = GrabModeSync;
  else
    pointer_mode = GrabModeAsync;

  if (! NILP (shape))
    {
      CHECK_NUMBER (shape, 0);
      grabbed_cursor = XCreateFontCursor (x_current_display, XINT (shape));
    }

  w = XtWindow (XSCREEN (XWINDOW (selected_window)->screen)->display.x->edit_widget);

  result = XGrabPointer (x_current_display, w,
			 False,
			 ButtonMotionMask | ButtonPressMask
			 | ButtonReleaseMask | PointerMotionHintMask,
			 GrabModeAsync,	      /* Keep pointer events flowing */
			 pointer_mode,	      /* Stall keyboard events */
			 w,		      /* Stay in this window */
			 grabbed_cursor,
			 CurrentTime);
  UNBLOCK_INPUT;
  if (result == GrabSuccess)
    {
      return Qt;
    }

  BLOCK_INPUT;
  XFreeCursor (x_current_display, grabbed_cursor);
  UNBLOCK_INPUT;
  return Qnil;
}

DEFUN ("x-ungrab-pointer", Fx_ungrab_pointer, Sx_ungrab_pointer, 0, 0, 0,
  "Release the pointer.")
  ()
{
  BLOCK_INPUT;
  XUngrabPointer (x_current_display, CurrentTime);

  if ((int) grabbed_cursor)
    {
      XFreeCursor (x_current_display, grabbed_cursor);
      grabbed_cursor = (Cursor) 0;
    }

  UNBLOCK_INPUT;
  return Qnil;
}

/* These keep track of the rectangle following the pointer. */
int mouse_rectangle_x, mouse_rectangle_y, mouse_rectangle_width;

#define DRAW_MOUSE_RECTANGLE x_rectangle(s, s->display.x->cursor_gc, \
					 mouse_rectangle_x,	     \
					 mouse_rectangle_y,	     \
					 mouse_rectangle_width, 1)

#define ERASE_MOUSE_RECTANGLE {                                           \
				 x_rectangle(s, s->display.x->reverse_gc, \
					     mouse_rectangle_x,	          \
					     mouse_rectangle_y,	          \
					     mouse_rectangle_width, 1);   \
			      }

static Lisp_Object
highlightable_class_at_mouse (mouse_class, window, buffer_offset)
     Lisp_Object mouse_class;
     struct window *window;
     int buffer_offset;
{
  EXTENT extent;

  if (extent_highlightable_p (mouse_class))
    return mouse_class;

  if (window && buffer_offset)
    {
      extent = extent_at 
        (buffer_offset, XBUFFER (window->buffer), EF_HIGHLIGHT);
      if (!extent)
	return Qnil;
      else
        {
          Lisp_Object tmp;
          XSET (tmp, Lisp_Extent, extent);
          return tmp;
        }
    }
  return Qnil;
}


/* handlers for the eval-events pushed on the queue by event-Xt.c */

Lisp_Object Qx_EnterNotify_internal, Qx_LeaveNotify_internal;
Lisp_Object Qx_FocusIn_internal, Qx_FocusOut_internal;
Lisp_Object Qx_VisibilityNotify_internal, Qx_non_VisibilityNotify_internal;
Lisp_Object Qx_MapNotify_internal, Qx_UnmapNotify_internal;

extern Lisp_Object Vmouse_enter_hook, Vmouse_left_hook;
extern Lisp_Object Vmap_screen_hook, Vunmap_screen_hook;

DEFUN ("x-EnterNotify-internal", Fx_EnterNotify_internal,
       Sx_EnterNotify_internal, 1, 1, 0, "hands off")
     (screen)
     Lisp_Object screen;
{
  CHECK_SCREEN (screen, 0);
  XSCREEN (screen)->display.x->mouse_p = 1;
  if (! NILP (Vmouse_enter_hook))
    call1 (Vmouse_enter_hook, screen);
  return Qnil;
}

DEFUN ("x-LeaveNotify-internal", Fx_LeaveNotify_internal,
       Sx_LeaveNotify_internal, 1, 1, 0, "hands off")
     (screen)
     Lisp_Object screen;
{
  CHECK_SCREEN (screen, 0);
  XSCREEN (screen)->display.x->mouse_p = 0;
  if (! NILP (Vmouse_left_hook))
    call1 (Vmouse_left_hook, screen);
  return Qnil;
}

/* This is true if any widget in any emacs screen has the X keyboard focus
   Flase otherwise. */
int any_screen_has_focus_p;

/* The select-screen-hook and deselect-screen-hook are run from the 
   select_screen() function; however, we run them from the FocusIn and
   FocusOut handlers as well, because under X, the "selectedness" of a
   screen has slightly funny semantics; according to emacs, a screen is
   not "deselected" unless some other screen is selected.  This is so that
   there is always some current window and buffer, and so on.  However, it's
   useful to run the deselect-screen-hook when emacs loses the X keyboard
   focus (that is, no emacs screen is the X selected window).  Likewise,
   it's useful to run the select-screen- hook when some emacs window regains
   the focus, even if that window is already the selected screen from emacs's
   point of view.

   If we don't do this, then the select-screen-hook (meaning auto-raise)
   isn't run if (in a point-to-type world) the mouse moves into the same
   emacs window that it originally left.  Clearly this isn't what someone
   who would want auto-raise would want.
 */

extern Lisp_Object Qselect_screen_hook, Qdeselect_screen_hook;

DEFUN ("x-FocusIn-internal", Fx_FocusIn_internal,
       Sx_FocusIn_internal, 1, 1, 0, "hands off")
     (screen)
     Lisp_Object screen;
{
  int getting_focus = !any_screen_has_focus_p;
  CHECK_SCREEN (screen, 0);
  any_screen_has_focus_p = 1;
  XSCREEN (screen)->display.x->focus_p = 1;
  if (XSCREEN (screen) == selected_screen)
    {
      x_screen_redraw_cursor (XSCREEN (screen));
      /* see comment above */
      if (getting_focus)
	if (!NILP (Vrun_hooks))
	  call1 (Vrun_hooks, Qselect_screen_hook);
    }
  else
    select_screen (XSCREEN (screen));
  return Qnil;
}

DEFUN ("x-FocusOut-internal", Fx_FocusOut_internal,
       Sx_FocusOut_internal, 1, 1, 0, "hands off")
     (screen)
     Lisp_Object screen;
{
  CHECK_SCREEN (screen, 0);
  any_screen_has_focus_p = 0;
  XSCREEN (screen)->display.x->focus_p = 0;
  if (XSCREEN (screen) == selected_screen)
    x_screen_redraw_cursor (XSCREEN (screen));
  /* see comment above */
  if (!NILP (Vrun_hooks))
    call1 (Vrun_hooks, Qdeselect_screen_hook);
  return Qnil;
}

DEFUN ("x-VisibilityNotify-internal", Fx_VisibilityNotify_internal,
       Sx_VisibilityNotify_internal, 1, 1, 0, "hands off")
     (screen)
     Lisp_Object screen;
{
  CHECK_SCREEN (screen, 0);
  XSCREEN (screen)->display.x->totally_visible_p = 1;
  return Qnil;
}

DEFUN ("x-non-VisibilityNotify-internal", Fx_non_VisibilityNotify_internal,
       Sx_non_VisibilityNotify_internal, 1, 1, 0, "hands off")
     (screen)
     Lisp_Object screen;
{
  CHECK_SCREEN (screen, 0);
  XSCREEN (screen)->display.x->totally_visible_p = 0;
  return Qnil;
}

DEFUN ("x-MapNotify-internal", Fx_MapNotify_internal,
       Sx_MapNotify_internal, 1, 1, 0, "hands off")
     (screen)
     Lisp_Object screen;
{
  CHECK_SCREEN (screen, 0);

  XSCREEN (screen)->display.x->totally_visible_p = 1;
  if (! XSCREEN (screen)->visible)
    {
      XSCREEN (screen)->visible = 1;
      SET_SCREEN_GARBAGED (XSCREEN (screen));
      if (! NILP (Vmap_screen_hook))
	call1 (Vmap_screen_hook, screen);
    }
  return Qnil;
}


DEFUN ("x-UnmapNotify-internal", Fx_UnmapNotify_internal,
       Sx_UnmapNotify_internal, 1, 1, 0, "hands off")
     (screen)
     Lisp_Object screen;
{
  CHECK_SCREEN (screen, 0);

  XSCREEN (screen)->display.x->totally_visible_p = 0;
  if (XSCREEN (screen)->visible)
    {
      XSCREEN (screen)->visible = 0;
      if (! NILP (Vunmap_screen_hook))
	call1 (Vunmap_screen_hook, screen);
    }
  return Qnil;
}


#if 0 /* #### This stuff is obsolete; with the new event model, 
	      regular keyboard macros work just as well as this.
       */
DEFUN ("x-rebind-key", Fx_rebind_key, Sx_rebind_key, 3, 3, 0,
"Rebind X keysym KEYSYM, with MODIFIERS, to generate NEWSTRING.\n\
KEYSYM is a string which conforms to the X keysym definitions found\n\
in X11/keysymdef.h, sans the initial XK_. MODIFIERS is nil or a\n\
list of strings specifying modifier keys such as Control_L, which must\n\
also be depressed for NEWSTRING to appear.")
  (x_keysym, modifiers, newstring)
     register Lisp_Object x_keysym;
     register Lisp_Object modifiers;
     register Lisp_Object newstring;
{
  char *rawstring;
  register KeySym keysym, modifier_list[16];

  CHECK_STRING (x_keysym, 1);
  CHECK_STRING (newstring, 3);

  BLOCK_INPUT;
  keysym = XStringToKeysym ((char *) XSTRING (x_keysym)->data);
  UNBLOCK_INPUT;

  if (keysym == NoSymbol)
    error ("Keysym does not exist");

  if (NILP (modifiers))
    {
      BLOCK_INPUT;
      XRebindKeysym (x_current_display, keysym, modifier_list, 0,
		     XSTRING (newstring)->data, XSTRING (newstring)->size);
      UNBLOCK_INPUT;
    }
  else
    {
      register Lisp_Object rest, mod;
      register int i = 0;

      for (rest = modifiers; !NILP (rest); rest = Fcdr (rest))
	{
	  if (i == 16)
	    error ("Can't have more than 16 modifiers");

	  mod = Fcar (rest);
	  CHECK_STRING (mod, 3);
	  BLOCK_INPUT;
	  modifier_list[i] = XStringToKeysym ((char *) XSTRING (mod)->data);
	  UNBLOCK_INPUT;
	  if (modifier_list[i] == NoSymbol
	      || !IsModifierKey (modifier_list[i]))
	    error ("Element is not a modifier keysym");
	  i++;
	}
      
      BLOCK_INPUT;
      XRebindKeysym (x_current_display, keysym, modifier_list, i,
		     XSTRING (newstring)->data, XSTRING (newstring)->size);
      UNBLOCK_INPUT;
    }

  return Qnil;
}
  
DEFUN ("x-rebind-keys", Fx_rebind_keys, Sx_rebind_keys, 2, 2, 0,
  "Rebind KEYCODE to list of strings STRINGS.\n\
STRINGS should be a list of 16 elements, one for each shift combination.\n\
nil as element means don't change.\n\
See the documentation of `x-rebind-key' for more information.")
  (keycode, strings)
     register Lisp_Object keycode;
     register Lisp_Object strings;
{
  register Lisp_Object item;
  register unsigned char *rawstring;
  KeySym rawkey, modifier[1];
  int strsize;
  register unsigned i;

  CHECK_NUMBER (keycode, 1);
  CHECK_CONS (strings, 2);
  rawkey = (KeySym) ((unsigned) (XINT (keycode))) & 255;
  for (i = 0; i <= 15; strings = Fcdr (strings), i++)
    {
      item = Fcar (strings);
      if (!NILP (item))
	{
	  CHECK_STRING (item, 2);
	  strsize = XSTRING (item)->size;
	  rawstring = (unsigned char *) xmalloc (strsize);
	  bcopy (XSTRING (item)->data, rawstring, strsize);
	  modifier[1] = 1 << i;
	  BLOCK_INPUT;
	  XRebindKeysym (x_current_display, rawkey, modifier, 1,
			 rawstring, strsize);
	  UNBLOCK_INPUT;
	}
    }
  return Qnil;
}
#endif


/* This comment supplies the doc string for x-get-resource,
   for make-docfile to see.  We cannot put this in the real DEFUN
   due to limits in the Unix cpp.

DEFUN ("x-get-resource", Fx_get_resource, Sx_get_resource, 3, 4, 0,
       "Retrieve an X resource from the resource manager.\n\
The first arg is the name of the resource to retrieve, such as \"font\".\n\
The second arg is the class of the resource to retrieve, like \"Font\".\n\
The third arg should be one of the symbols string, integer, or boolean,\n\
specifying the type of object that the database is searched for.\n\
The fourth arg is the screen to search for the resources on, defaulting\n\
to the selected screen.\n\
\n\
The call\n\
    (x-get-resource \"font\" \"Font\" 'string)\n\
\n\
is an interface to the C call\n\
\n\
    XrmGetResource (db, \"Emacs.this_screen_name.font\",\n\
			\"Emacs.EmacsScreen.Font\",\n\
			\"String\");\n\
\n\
Therefore if you want to retrieve a deeper resource, for example,\n\
\"Emacs.foo.foreground\", you need to specify the same number of links\n\
in the class path:\n\
    (x-get-resource \"foo.foreground\" \"Thing.Foreground\" 'string)\n\
\n\
which is equivalent to 
\n\
    XrmGetResource (db, \"Emacs.screen_name.foo.foreground\",\n\
			\"Emacs.EmacsScreen.Thing.Foreground\",\n\
			\"String\");\n\

\n\
The returned value of this function is nil if the queried resource is not\n\
found.  If the third arg is `string', a string is returned, and if it is\n\
`integer', an integer is returned.  If the third arg is `boolean', then the\n\
returned value is the list (t) for true, (nil) for false, and is nil to\n\
mean ``unspecified.''")
     (name, class, type, screen)
*/


DEFUN ("x-get-resource", Fx_get_resource, Sx_get_resource, 3, 4, 0, 0)
     (name, class, type, screen)
     Lisp_Object name, class, type, screen;
{
  char *name_string, *class_string;
  char *app_name, *app_class, *screen_name, *screen_class, *s;
  Widget widget;

  CHECK_STRING (name, 0);
  CHECK_STRING (class, 0);
  CHECK_SYMBOL (type, 0);
  if (NILP (screen))
    screen = Fselected_screen ();
  else
    CHECK_SCREEN (screen, 0);
  if (! SCREEN_IS_X (XSCREEN (screen)))
/*    error ("not an X screen"); */
    return Qnil;

  widget = XSCREEN (screen)->display.x->widget;
  BLOCK_INPUT;
  XtGetApplicationNameAndClass (XtDisplay (widget), &app_name, &app_class);
  UNBLOCK_INPUT;
  screen_name = widget->core.name;
  screen_class = XtClass (widget)->core_class.class_name;
  name_string = (char *) alloca (XSTRING (name)->size + strlen (app_name)
				 + strlen (screen_name) + 3);
  class_string = (char *) alloca (XSTRING (class)->size + strlen (app_class)
				  + strlen (screen_class) + 3);
  strcpy (name_string, (char *) app_name);
  for (s = name_string; *s; s++) if (*s == '.') *s = '_';
  strcat (name_string, ".");
  s++;
  strcat (name_string, (char *) screen_name);
  for (; *s; s++) if (*s == '.') *s = '_';
  strcat (name_string, ".");
  strcat (name_string, (char *) XSTRING (name)->data);
  strcpy (class_string, app_class);
  strcat (class_string, ".");
  strcat (class_string, screen_class);
  strcat (class_string, ".");
  strcat (class_string, (char *) XSTRING (class)->data);

  {
    XrmValue xrm_value;
    XrmName namelist [100];
    XrmClass classlist [100];
    XrmName *namerest = namelist;
    XrmClass *classrest = classlist;
    XrmRepresentation xrm_type;
    XrmRepresentation string_quark;
    int result;
    BLOCK_INPUT;
    XrmStringToNameList (name_string, namelist);
    XrmStringToClassList (class_string, classlist);
    string_quark = XrmStringToQuark ("String");
    UNBLOCK_INPUT;

    /* ensure that they have the same length */
    while (namerest [0] && classrest [0])
      namerest++, classrest++;
    if (namerest [0] || classrest [0])
      Fsignal (Qerror,
	       Fcons (build_string
		      ("class list and name list must be the same length"),
		      Fcons (build_string (name_string),
			     Fcons (build_string (class_string), Qnil))));
    BLOCK_INPUT;
    result = XrmQGetResource (XtDatabase (XtDisplay (widget)),
			      namelist, classlist, &xrm_type, &xrm_value);
    UNBLOCK_INPUT;

    if (result != True || xrm_type != string_quark)
      return Qnil;
    s = (char *) xrm_value.addr;
  }

  if (EQ (type, intern ("string")))
    return build_string (s);
  else if (EQ (type, intern ("boolean")))
    {
      if (!strcasecmp (s, "off") || !strcasecmp (s, "false") ||
	  !strcasecmp (s,"no"))
	return Fcons (Qnil, Qnil);
      else if (!strcasecmp (s, "on") || !strcasecmp (s, "true") ||
	       !strcasecmp (s, "yes"))
	return Fcons (Qt, Qnil);
      else
	{
	  char str[255];
	  sprintf (str, "can't convert %s: %s to a Boolean",
		   name_string, s);
	  return Fsignal (Qerror, Fcons (build_string (str), Qnil));
	}
    }
  else if (EQ (type, intern ("integer")))
    {
      int i, c;
      if (1 != sscanf (s, "%d%c", &i, &c))
	{
	  char str [255];
	  sprintf (str, "can't convert %s: %s to an integer",
		   name_string, s);
	  return Fsignal (Qerror, Fcons (build_string (str), Qnil));
	}
      else
	return make_number (i);
    }
  else
    return
      Fsignal (Qwrong_type_argument,
	       Fcons (build_string ("should be string, integer, or boolean"),
		      Fcons (type, Qnil)));
}


/* Cursors.  Once emacs allocates an X cursor, it never frees it.
   Presumably cursors are very lightweight, and this it ok.  If
   this turns out not to be the case, we should only cache the 
   last N cursors used (2<n<10?) and XFreeCursor() on the least
   recently used ones.
 */

Lisp_Object Vcursor_alist;

/* XmuCvtStringToCursor is a little bogus, and when it can't convert to
   a real cursor, it will sometimes return a "success" value, after 
   triggering a BadPixmap error.  It then gives you a cursor that will
   itself generate BadCursor errors.  So we install this error handler
   to catch/notice the X error and take that as meaning "couldn't convert."
 */

static int XmuCvtStringToCursor_got_error;
static int XmuCvtStringToCursor_error_handler (dpy, error)
     Display *dpy;
     XErrorEvent *error;
{
  XmuCvtStringToCursor_got_error = 1;
  return 0;
}

DEFUN ("x-valid-color-name-p", Fx_valid_color_name_p, Sx_valid_color_name_p,
       1, 2, 0,
       "Returns true if COLOR names a color that X knows about.\n\
Valid color names are listed in the file /usr/lib/X11/rgb.txt, or\n\
whatever the equivalent is on your system.")
     (color, screen)
     Lisp_Object color, screen;
{
  int ok;
  XColor c;
  Widget widget;
  if (NILP (screen))
    screen = Fselected_screen ();
  CHECK_SCREEN (screen, 0);
  if (! SCREEN_IS_X (XSCREEN (screen)))
    return Qnil;
  CHECK_STRING (color, 0);
  widget = XSCREEN (screen)->display.x->widget;
  BLOCK_INPUT;
  ok = XParseColor (XtDisplay (widget),
		    DefaultColormapOfScreen (XtScreen (widget)),
		    (char *) XSTRING (color)->data, &c);
  UNBLOCK_INPUT;
  return ok ? Qt : Qnil;
}

DEFUN ("x-valid-keysym-name-p", Fx_valid_keysym_name_p, Sx_valid_keysym_name_p,
       1, 1, 0,
  "Returns true if KEYSYM names a keysym that the X library knows about.\n\
Valid keysyms are listed in the files /usr/include/X11/keysymdef.h and in\n\
/usr/lib/X11/XKeysymDB, or whatever the equivalents are on your system.")
     (keysym)
     Lisp_Object keysym;
{
  CHECK_STRING (keysym, 0);
  if (XStringToKeysym ((char *) XSTRING (keysym)->data))
    return Qt;
  return Qnil;
}


Cursor
x_get_cursor (s, name, fg, bg, noerror)
     struct screen *s;
     Lisp_Object name, fg, bg;
     int noerror;
{
  Cursor cursor;
  Lisp_Object cons, ofg, obg;
  if (noerror)
    {
      if ((XTYPE (name) != Lisp_String) ||
	  (!NILP (fg) && XTYPE (fg) != Lisp_String) ||
	  (!NILP (bg) && XTYPE (bg) != Lisp_String))
	return 0;
    }
  else
    {
      CHECK_STRING (name, 0);
      if (!NILP (fg)) CHECK_STRING (fg, 0);
      if (!NILP (bg)) CHECK_STRING (bg, 0);
    }
  cons = assoc_no_quit (name, Vcursor_alist);
  if (NILP (cons))
    {
      int (*old_handler) ();
      XrmValue arg, from, to;
      int nargs = 1;
      Screen *screen = XtScreen (s->display.x->widget);
      arg.addr = (XtPointer) &screen;
      arg.size = sizeof (Screen *);
      from.addr = (XtPointer) XSTRING (name)->data;
      from.size = (unsigned int) XSTRING (name)->size;
      to.addr = 0;
      to.size = 0;
      BLOCK_INPUT;
      XSync (XtDisplay (s->display.x->widget), 0);
      XmuCvtStringToCursor_got_error = 0;
      old_handler = XSetErrorHandler (XmuCvtStringToCursor_error_handler);
      XmuCvtStringToCursor (&arg, &nargs, &from, &to);
      XSync (XtDisplay (s->display.x->widget), 0);
      XSetErrorHandler (old_handler);
      UNBLOCK_INPUT;
      if (XmuCvtStringToCursor_got_error) cursor = 0;
      else if (to.addr) cursor = *((Cursor *) to.addr);
      else cursor = 0;
      if (! cursor && noerror)
	return 0;
      else if (! cursor)
	while (1)
	  Fsignal (Qerror, Fcons (build_string ("unknown cursor"),
				  Fcons (name, Qnil)));
      ofg = obg = Qnil;
      cons = Fcons (name, Fcons (word_to_lisp ((int) cursor),
				 Fcons (ofg, Fcons (obg, Qnil))));
      Vcursor_alist = Fcons (cons, Vcursor_alist);
    }
  else
    {
      cursor = lisp_to_word (XCONS (XCONS (cons)->cdr)->car);
      ofg = XCONS (XCONS (XCONS (cons)->cdr)->cdr)->car;
      obg = XCONS (XCONS (XCONS (XCONS (cons)->cdr)->cdr)->cdr)->car;
    }
  if (!NILP (fg) && !NILP (bg) &&
      (NILP (Fequal (fg, ofg)) || NILP (Fequal (bg, obg))))
    {
      XColor fgc, bgc;
      int result;
      Widget widget = s->display.x->widget;
      Display *dpy = XtDisplay (widget);
      Colormap cmap = DefaultColormapOfScreen (XtScreen (widget));

      BLOCK_INPUT;
      result = XParseColor (dpy, cmap, (char *) XSTRING (fg)->data, &fgc);
      UNBLOCK_INPUT;
      if (! result && noerror)
	return 0;
      else if (! result)
	while (1)
	  Fsignal (Qerror, Fcons (build_string ("unrecognised color"),
				  Fcons (fg, Qnil)));
      BLOCK_INPUT;
      result = XParseColor (dpy, cmap, (char *) XSTRING (bg)->data, &bgc);
      UNBLOCK_INPUT;
      if (! result && noerror)
	return 0;
      else if (! result)
	while (1)
	  Fsignal (Qerror, Fcons (build_string ("unrecognised color"),
				  Fcons (bg, Qnil)));
      XCONS (XCONS (XCONS (cons)->cdr)->cdr)->car = fg;
      XCONS (XCONS (XCONS (XCONS (cons)->cdr)->cdr)->cdr)->car = bg;
      BLOCK_INPUT;
      XRecolorCursor (dpy, cursor, &fgc, &bgc);
      UNBLOCK_INPUT;
    }
  return cursor;
}


DEFUN ("x-set-screen-pointer", Fx_set_screen_pointer, Sx_set_screen_pointer,
       2, 4, 0,
       "Set the mouse cursor of SCREEN to the cursor named CURSOR-NAME,\n\
with colors FOREGROUND and BACKGROUND.  The string may be any of the\n\
standard cursor names from appendix B of the Xlib manual (also known as\n\
the file <X11/cursorfont.h>) minus the XC_ prefix, or it may be a font\n\
name and glyph index of the form \"FONT fontname index [[font] index]\",\n\
or it may be a bitmap file acceptable to XmuLocateBitmapFile().\n\
If it is a bitmap file, and if a bitmap file whose name is the name of\n\
the cursor with \"msk\" exists, then it is used as the mask.  For example,\n\
a pair of files may be named \"cursor.xbm\" and \"cursor.xbmmsk\".")
     (screen, cursor_name, fg, bg)
     Lisp_Object screen, cursor_name, fg, bg;
{
  Cursor cursor;
  CHECK_SCREEN (screen, 0);
  if (! SCREEN_IS_X (XSCREEN (screen)))
    return Qnil;
  cursor = x_get_cursor (XSCREEN (screen), cursor_name, fg, bg, 0);
  BLOCK_INPUT;
  XDefineCursor (XtDisplay (XSCREEN (screen)->display.x->edit_widget),
		 XtWindow (XSCREEN (screen)->display.x->edit_widget),
		 cursor);
  UNBLOCK_INPUT;
  store_screen_param (XSCREEN (screen), intern ("pointer"), cursor_name);
  return Qnil;
}


/* GC calls x_show_gc_cursor() to change the mouse pointer to indicate GC.
   If this returns 0, then it couldn't change the cursor for whatever reason,
   and a minibuffer message will be output instead.  x_show_normal_cursor()
   will be called at the end.
*/
static int change_cursor_for_gc ();

int
x_show_gc_cursor (s)
     struct screen* s;
{
  return change_cursor_for_gc (s, 1);
}

int
x_show_normal_cursor (s)
     struct screen* s;
{
  return change_cursor_for_gc (s, 0);
}


static int
change_cursor_for_gc (s, gc_p)
     struct screen *s;
     int gc_p;
{
  Cursor cursor;
  static int changed;

  if (!s || !SCREEN_IS_X(s) || NILP (Vx_gc_pointer_shape))
    return 0;
  
  if (! gc_p && !changed)
    return 0;

  if (gc_p)
    cursor = x_get_cursor (s, Vx_gc_pointer_shape, Qnil, Qnil, 1);
  else
    cursor = x_get_cursor (s, get_screen_param (s, intern ("pointer")),
			   Qnil, Qnil, 1);
  if (! cursor)
    {
      if (gc_p)
	message ("Garbage collecting... (x-gc-pointer-shape is bogus!)");
      changed = 0;
      return 1;
    }

  BLOCK_INPUT;
  XDefineCursor (XtDisplay (s->display.x->edit_widget),
		 XtWindow (s->display.x->edit_widget),
		 cursor);
  XFlushQueue ();
  UNBLOCK_INPUT;
  changed = 1;
  return 1;
}



Visual *
select_visual (screen, depth)
     Screen *screen;
     unsigned int *depth;
{
  Visual *v;
  XVisualInfo *vinfo, vinfo_template;
  int n_visuals;

  BLOCK_INPUT;
  v = DefaultVisualOfScreen (screen);
  vinfo_template.visualid = XVisualIDFromVisual (v);
  vinfo = XGetVisualInfo (x_current_display, VisualIDMask, &vinfo_template,
			  &n_visuals);
  UNBLOCK_INPUT;
  if (n_visuals == 0)
    fatal ("Can't get proper X visual info");

  if ((1 << vinfo->depth) == vinfo->colormap_size)
    *depth = vinfo->depth;
  else
    {
      int i = 0;
      int n = vinfo->colormap_size - 1;
      while (n)
	{
	  n = n >> 1;
	  i++;
	}
      *depth = i;
    }

  XFree ((char *) vinfo);
  return v;
}

#ifdef USE_SPARC_SOUND
extern int not_on_console;  /* defined in fns.c */
#endif

DEFUN ("x-open-connection", Fx_open_connection, Sx_open_connection,
       1, 1, 0, "Open a connection to an X server.\n\
Argument ARGV is a list of strings describing the command line options.\n\
Returns a copy of ARGV from which the arguments used by the Xt code\n\
to open the connect have been removed.")
 	(argv_list)
	Lisp_Object argv_list;
{
  Lisp_Object argv_rest;
  unsigned int n_planes;
  register Screen *x_screen;

  if (x_current_display != 0)
    error ("X server connection is already initialized");

  /* This is what sets x_current_display.  This also initializes many symbols,
     such as those used for input. */
  argv_rest = x_term_init (argv_list);
  
  text_part_sym = intern ("text-part");
  modeline_part_sym = intern ("modeline-part");
  
  XFASTINT (Vwindow_system_version) = 11;
  
  x_screen = DefaultScreenOfDisplay (x_current_display);

  x_screen_count = make_number (ScreenCount (x_current_display));
  Vx_vendor = build_string (ServerVendor (x_current_display));
  x_release = make_number (VendorRelease (x_current_display));
                    
  x_screen_height = make_number (HeightOfScreen (x_screen));
  x_screen_height_mm = make_number (HeightMMOfScreen (x_screen));
  x_screen_width = make_number (WidthOfScreen (x_screen));
  x_screen_width_mm = make_number (WidthMMOfScreen (x_screen));

  switch (DoesBackingStore (x_screen))
    {
    case Always:
      Vx_backing_store = intern ("Always");
      break;

    case WhenMapped:
      Vx_backing_store = intern ("WhenMapped");
      break;

    case NotUseful:
      Vx_backing_store = intern ("NotUseful");
      break;

    default:
      error ("Strange value for BackingStore.");
      break;
    }

  if (DoesSaveUnders (x_screen) == True)
    x_save_under = Qt;
  else
    x_save_under = Qnil;

  screen_visual = select_visual (x_screen, &n_planes);
  x_screen_planes = make_number (n_planes);
  Vx_screen_visual = intern (x_visual_strings [screen_visual->class]);

  Xatoms_of_xfns ();
  Xatoms_of_xselect ();

#ifdef USE_SPARC_SOUND
  /* When running on a SparcStation, we cannot use digitized sounds as
     beeps unless emacs is running on the same machine that $DISPLAY
     points to, and $DISPLAY points to screen 0 of that machine.
   */
  {
    char *dpy = x_current_display->display_name;
    char *tail = (char *) strchr (dpy, ':');
    if (! tail ||
	strncmp (tail, ":0", 2))
      not_on_console = 1;
    else {
      char dpyname[255], localname[255];
      strncpy (dpyname, dpy, tail-dpy);
      dpyname [tail-dpy] = 0;
      if (!*dpyname ||
	  !strcmp(dpyname, "unix") ||
	  !strcmp(dpyname, "localhost"))
	not_on_console = 0;
      else if (gethostname (localname, sizeof (localname)))
	not_on_console = 1;  /* can't find hostname? */
      else {
	/* gethostbyname() reuses the structure it returns,
	   so we have to copy the string out of it. */
	struct hostent *h = gethostbyname (dpyname);
	not_on_console = !h || !!(strcmp (localname, h->h_name));
      }
    }
  }
#endif

  return argv_rest;
}

DEFUN ("x-window-id", Fx_window_id, Sx_window_id, 1, 1, 0,
       "Get the ID of the X11 window. This gives us a chance to manipulate\n\
the Emacs window from within a different program. Since the id is an\n\
unsigned long, we return it as a string.")
  (screen)
  Lisp_Object screen;
{
  char str[20];

  CHECK_SCREEN (screen, 0);
  sprintf (str, "%lu", XtWindow (XSCREEN (screen)->display.x->edit_widget));
  return (make_string (str, strlen (str)));
}


DEFUN ("x-close-current-connection", Fx_close_current_connection,
       Sx_close_current_connection,
       0, 0, 0, "Close the connection to the current X server.")
  ()
{
  /* This is ONLY used when killing emacs;  For switching displays
     we'll have to take care of setting CloseDownMode elsewhere. */
#ifdef FREE_CHECKING
  extern void (*__free_hook)();
  int checking_free;
#endif

  if (x_current_display)
    {
      BLOCK_INPUT;
#ifdef FREE_CHECKING
      checking_free = (__free_hook != 0);
      
      /* Disable strict free checking, to avoid bug in X library */
      if (checking_free)
	disable_strict_free_check ();
#endif
      XCloseDisplay (x_current_display);
      x_current_display = 0;
      Vwindow_system = Qnil;
#ifdef FREE_CHECKING
      if (checking_free)
	enable_strict_free_check ();
#endif
      UNBLOCK_INPUT;
    }
  else
    fatal ("No current X display connection to close");
  return Qnil;
}

static int
emacs_safe_XSyncFunction(dpy)
     register Display *dpy;
{
  BLOCK_INPUT;
  XSync (dpy, 0);
  UNBLOCK_INPUT;
  return 0;
}

DEFUN ("x-debug-mode", Fx_debug_mode, Sx_debug_mode, 1, 1, 0,
       "With a true arg, put the connection to the X server in synchronous\n\
mode; this is slower.  False turns it off.\n\
Do not simply call XSynchronize() from gdb; that won't work.")
    (arg)
    Lisp_Object arg;
{
  if (!NILP (arg))
    {
      BLOCK_INPUT;
      XSetAfterFunction (x_current_display, emacs_safe_XSyncFunction);
      UNBLOCK_INPUT;
      message ("X connection is synchronous");
    }
  else
    {
      BLOCK_INPUT;
      XSetAfterFunction (x_current_display, 0);
      UNBLOCK_INPUT;
      message ("X connection is asynchronous");
    }
  return arg;
}


syms_of_xfns ()
{
  init_x_parm_symbols ();

  /* This is zero if not using X windows.  */
  x_current_display = 0;

  DEFVAR_LISP ("x-gc-pointer-shape", &Vx_gc_pointer_shape,
   "The shape of the mouse-pointer during garbage collection.\n\
If this is nil, then the cursor will not be changed, and echo-area messages\n\
will be used instead.");
  Vx_gc_pointer_shape = Qnil;

  DEFVAR_LISP ("bar-cursor", &Vbar_cursor,
	       "Use vertical bar cursor if non-nil.");
  Vbar_cursor = Qnil;

  DEFVAR_LISP ("x-screen-defaults", &Vx_screen_defaults,
    "Alist of default screen-creation parameters for X-window screens.\n\
These override what is specified in `~/.Xdefaults' but are overridden\n\
by the arguments to the particular call to `x-create-screen'.");
  Vx_screen_defaults = Qnil;

  DEFVAR_INT ("x-screen-count", &x_screen_count,
	      "The number of screens associated with the current display.");
  DEFVAR_INT ("x-release", &x_release,
	      "The release number of the X server in use.");
  DEFVAR_LISP ("x-vendor", &Vx_vendor,
	       "The vendor supporting the X server in use.");
  DEFVAR_INT ("x-screen-height", &x_screen_height,
	      "The height of this X screen in pixels.");
  DEFVAR_INT ("x-screen-height-mm", &x_screen_height_mm,
	      "The height of this X screen in millimeters.");
  DEFVAR_INT ("x-screen-width", &x_screen_width,
	      "The width of this X screen in pixels.");
  DEFVAR_INT ("x-screen-width-mm", &x_screen_width_mm,
	      "The width of this X screen in millimeters.");
  DEFVAR_LISP ("x-backing-store", &Vx_backing_store,
	       "The backing store capability of this screen.\n\
Values can be the symbols Always, WhenMapped, or NotUseful.");
  DEFVAR_BOOL ("x-save-under", &x_save_under,
	       "Non-nil means this X screen supports the SaveUnder feature.");
  DEFVAR_INT ("x-screen-planes", &x_screen_planes,
	      "The number of planes this monitor supports.");
  DEFVAR_LISP ("x-screen-visual", &Vx_screen_visual,
	       "The default X visual for this X screen.");

  DEFVAR_LISP ("x-bitmap-file-path", &Vx_bitmap_file_path,
       "A list of the directories in which X bitmap files may be found.\n\
If nil, this is initialized from the \"*bitmapFilePath\" resource.");
  Vx_bitmap_file_path = Qnil;

  DEFVAR_LISP ("create-screen-hook", &Vcreate_screen_hook,
   "A function of one argument, called with each newly-created screen.");
  Vcreate_screen_hook = Qnil;

  DEFVAR_BOOL ("x-allow-sendevents", &x_allow_sendevents,
    "*Non-nil means to allow synthetic events.  Nil means they are ignored.\n\
Beware: allowing emacs to process SendEvents opens a big security hole.");
  x_allow_sendevents = 0;

  staticpro (&Vcursor_alist);
  Vcursor_alist = Qnil;

  defsubr (&Sx_pixel_width);
  defsubr (&Sx_pixel_height);
  defsubr (&Sx_window_id);
  defsubr (&Sx_color_display_p);
  defsubr (&Sx_grab_pointer);
  defsubr (&Sx_ungrab_pointer);
  defsubr (&Sx_create_screen);
  defsubr (&Sx_open_connection);
  defsubr (&Sx_close_current_connection);
#ifdef ENERGIZE
  defsubr (&Senergize_toggle_psheet);
#endif
  defsubr (&Sx_focus_screen);
  defsubr (&Sx_show_lineinfo_column);
  defsubr (&Sx_hide_lineinfo_column);
  defsubr (&Sx_debug_mode);
  defsubr (&Sx_get_resource);
  defsubr (&Sx_set_screen_icon_pixmap);
  defsubr (&Sx_set_screen_pointer);
  defsubr (&Sx_valid_color_name_p);
  defsubr (&Sx_valid_keysym_name_p);

  defsubr (&Sx_EnterNotify_internal);
  defsubr (&Sx_LeaveNotify_internal);
  defsubr (&Sx_FocusIn_internal);
  defsubr (&Sx_FocusOut_internal);
  defsubr (&Sx_MapNotify_internal);
  defsubr (&Sx_UnmapNotify_internal);
  defsubr (&Sx_VisibilityNotify_internal);
  defsubr (&Sx_non_VisibilityNotify_internal);

  Qx_EnterNotify_internal = intern ("x-EnterNotify-internal");
  Qx_LeaveNotify_internal = intern ("x-LeaveNotify-internal");
  Qx_FocusIn_internal  = intern ("x-FocusIn-internal");
  Qx_FocusOut_internal = intern ("x-FocusOut-internal");
  Qx_MapNotify_internal = intern ("x-MapNotify-internal");
  Qx_UnmapNotify_internal = intern ("x-UnmapNotify-internal");
  Qx_VisibilityNotify_internal = intern ("x-VisibilityNotify-internal");
  Qx_non_VisibilityNotify_internal = intern("x-non-VisibilityNotify-internal");
}

Xatoms_of_xfns ()
{
#define ATOM(x) XInternAtom(x_current_display, (x), False)

  BLOCK_INPUT;
  Xatom_WM_PROTOCOLS = ATOM("WM_PROTOCOLS");
  Xatom_WM_DELETE_WINDOW = ATOM("WM_DELETE_WINDOW");
  Xatom_WM_SAVE_YOURSELF = ATOM("WM_SAVE_YOURSELF");
  Xatom_WM_TAKE_FOCUS = ATOM("WM_TAKE_FOCUS");
  UNBLOCK_INPUT;
}

#endif /* HAVE_X_WINDOWS */

