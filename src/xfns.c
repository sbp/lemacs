/* Functions for the X window system.
   Copyright (C) 1989, 1992, 1993, 1994 Free Software Foundation.

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

#include "config.h"

#include <stdio.h>
/* #include <signal.h>  use "syssignal.h" instead -jwz */
#include "syssignal.h"

#include "xintrinsicp.h"	/* CoreP.h needs this */
#include <X11/CoreP.h>		/* foul, but we need this to use our own
				   window inside a widget instead of one 
				   that Xt creates... */
#include <X11/StringDefs.h>
#include <X11/Xresource.h>
#include <X11/ShellP.h>

#ifdef LWLIB_USES_MOTIF
# include <Xm/MainW.h>
# include <Xm/PanedW.h>
#else /* Athena */
# include <X11/Xmu/Converters.h>	/* For XtorientVertical */
# include <X11/Xaw/Paned.h>
#endif

#ifdef USG
#undef USG	/* ####KLUDGE for Solaris 2.2 and up */
#include <X11/Xos.h>
#define USG
#else
#include <X11/Xos.h>
#endif

#undef CONST
#ifdef CONST_IS_LOSING          /* Restore Emacs' idea of CONST */
# define CONST
#else
# define CONST const
#endif

#include "EmacsScreenP.h"

#ifdef EXTERNAL_WIDGET
#include "EmacsShell.h"
#include "EmacsShellP.h"
#endif

/* Do the EDITRES protocol if running the X11R5 version of Athena */
#if ((XtSpecificationRelease >= 5) && !defined (LWLIB_USES_MOTIF))
#define HACK_EDITRES
extern void _XEditResCheckMessages();
#endif /* R5 + Athena */

#ifdef USE_SOUND
# include <netdb.h>
#endif

#include "lisp.h"
#include "intl.h"
#include "xterm.h"
#include "window.h"
#include "buffer.h"
#include "extents.h"
#include "screen.h"
#include "events.h"
#include "faces.h"
#include "xobjs.h"

#ifdef HAVE_X_WINDOWS

#if 1
#include "xgccache.h"
struct gc_cache *the_gc_cache;
#endif

#define min(a,b) ((a) < (b) ? (a) : (b))
#define max(a,b) ((a) > (b) ? (a) : (b))
#define COLOR_SCREEN_P(d) (XCellsOfScreen (DefaultScreenOfDisplay (d)) > 2)

#ifdef DUBIOUS_X_ERROR_HANDLING
Lisp_Object Qx_error;
#endif

Lisp_Object Vx_gc_pointer_shape;
Lisp_Object Vx_scrollbar_pointer_shape;

/* If non-nil, use vertical bar cursor. */
Lisp_Object Vbar_cursor;

/* The application class of Emacs. */
Lisp_Object Vx_emacs_application_class;

/* The screen on which we have placed a WM_COMMAND property.  Only one. */
Lisp_Object WM_COMMAND_screen;

Atom Xatom_WM_TAKE_FOCUS;
Atom Xatom_WM_SAVE_YOURSELF;
Atom Xatom_WM_DELETE_WINDOW;
Atom Xatom_WM_PROTOCOLS;
Atom Xatom_WM_STATE;

Lisp_Object Qx_resource_name;
Lisp_Object Qstring;
Lisp_Object Qname;
Lisp_Object Qboolean;
Lisp_Object Qinteger;
Lisp_Object Qpointer;
Lisp_Object Qscrollbar_pointer;

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
      if (!SCREENP (screen))
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
      if (!SCREENP (screen))
        continue;
      s = XSCREEN (screen);
      if (!SCREEN_IS_X (s))
	continue;
      x = s->display.x;
      /* This screen matches if the window is any of its widgets. */
      if (wdesc == XtWindow (x->widget) ||
	  wdesc == XtWindow (x->container) ||
#ifndef LWLIB_USES_MOTIF
	  wdesc == XtWindow (x->container2) ||
#endif
	  wdesc == XtWindow (x->edit_widget))
	return s;
      /* Match if the window is this screen's menubar. */
      if (x->menubar_widget &&
	  wdesc == XtWindow (x->menubar_widget))
	return s;
      /* Match if the window is this screen's scrollbar manager. */
      if (x->scrollbar_manager &&
	  wdesc == XtWindow (x->scrollbar_manager))
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

static void
init_x_parm_symbols ()
{
  defsymbol (&text_part_sym, "text-part");
  defsymbol (&modeline_part_sym, "modeline-part");

  defsymbol (&Qx_resource_name, "x-resource-name");

#define def(sym, rsrc) \
   pure_put (intern (sym), Qx_resource_name, build_string (rsrc))
#define defi(sym,rsrc) \
   def (sym, rsrc); pure_put (intern (sym), Qintegerp, Qt)

  def ("cursor-color", XtNcursorColor);
  def ("border-color", XtNborderColor);
  defi("border-width", XtNborderWidth);
  defi("internal-border-width", XtNinternalBorderWidth);
  defi("width", XtNwidth);
  defi("height", XtNheight);
  defi("x", XtNx);
  defi("y", XtNy);
  def ("iconic", XtNiconic);
  def ("minibuffer", XtNminibuffer);
  def ("unsplittable", XtNunsplittable);
  defi("inter-line-space", XtNinterline);
  def ("menubar", XtNmenubar);
  
#undef def
}


/* Insert a description of internally-recorded parameters of screen X
   into the parameter alist *ALISTPTR that is to be given to the user.
   Only parameters that are specific to the X window system
   and whose values are not correctly recorded in the screen's
   param_alist need to be considered here.  */

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


void
x_report_screen_params (s, alistptr)
     struct screen *s;
     Lisp_Object *alistptr;
{
  TopLevelShellWidget shell = (TopLevelShellWidget)s->display.x->widget;
  EmacsScreen w = (EmacsScreen)s->display.x->edit_widget;
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
/*  store_bool ("visual-bell", w->emacs_screen.visual_bell); */
/*  store_bool ("bar-cursor",  w->emacs_screen.bar_cursor); */
  sprintf (buf, "0x%x", XtWindow (w));
  store_str ("window-id", buf);
}


/* Functions called only from `x_set_screen_param' to set
** individual parameters. */

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
x_set_screen_values (struct screen *s, Lisp_Object alist)
{
  int x = 0, y = 0;
  Dimension width = 0, height = 0;
  Bool size_specified_p = False;
  Bool position_specified_p = False;
  Lisp_Object tail;
  Widget w = s->display.x->edit_widget;
  
  for (tail = alist; !EQ (tail, Qnil); tail = Fcdr (tail))
    {
      Lisp_Object elt = Fcar (tail);
      Lisp_Object prop = Fcar (elt);
      Lisp_Object val = Fcdr (elt);
      
      if (STRINGP (prop))
	{
	  if (XSTRING (prop)->size == 0)
	    continue;

	  BLOCK_INPUT;
	  if (STRINGP (val))
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
#if 0
      /* mly wants this, but it's not reasonable to change the name of a
	 screen after it has been created, because the old name was used
	 for resource lookup. */
      else if (EQ (elt, Qname))
        {
          CHECK_STRING (val, 0);
          s->name = val;
        }
#endif /* 0 */
      else
	{
	  Lisp_Object str = Fget (prop, Qx_resource_name, Qnil);
	  int int_p = !NILP (Fget (prop, Qintegerp, Qnil));

	  if (NILP (prop) || NILP (str))
            /* >>> No error-checking on property names!  FMH!!! */
	    /* RMS apparently thinks this is a feature.
	       This whole screen-parameters things has got to go. */
	    continue;
	  CHECK_STRING (str, 0);

	  /* Kludge the width/height so that we interpret them in characters
	     instead of pixels.  Yuck yuck yuck. */
	  if (!strcmp ((char *) XSTRING (str)->data, "width"))
	    {
	      CHECK_FIXNUM (val, 0);
	      width = XINT (val);
	      size_specified_p = True;
	      continue;
	    }
	  if (!strcmp ((char *) XSTRING (str)->data, "height"))
	    {
	      CHECK_FIXNUM (val, 0);
	      height = XINT (val);
	      size_specified_p = True;
	      continue;
	    }
	  /* Further kludge the x/y. */
	  if (!strcmp ((char *) XSTRING (str)->data, "x"))
	    {
	      CHECK_FIXNUM (val, 0);
	      x = XINT (val);
	      position_specified_p = True;
	      continue;
	    }
	  if (!strcmp ((char *) XSTRING (str)->data, "y"))
	    {
	      CHECK_FIXNUM (val, 0);
	      y = XINT (val);
	      position_specified_p = True;
	      continue;
	    }

	  BLOCK_INPUT;
	  if (int_p)
	    {
	      CHECK_FIXNUM (val, 0);
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

  /* Kludge kludge kludge. */
#if 0
  if (size_specified_p || position_specified_p)
    {
      char geom [512];
      if (size_specified_p && position_specified_p)
	sprintf (geom, "=%dx%d%c%d%c%d",
		 width, height,
		 (x < 0 ? '-' : '+'), x,
		 (y < 0 ? '-' : '+'), y);
      else if (position_specified_p)
	sprintf (geom, "%c%d%c%d",
		 (x < 0 ? '-' : '+'), x,
		 (y < 0 ? '-' : '+'), y);
      else
	sprintf (geom, "=%dx%d", width, height);

      XtVaSetValues (w, XtNgeometry, xstrdup (geom), 0);
    }
#else
  if (size_specified_p)
    {
      Lisp_Object screen;
      XSETR (screen, Lisp_Screen, s);
      if (width == 0) width = 80; /* SCREEN_WIDTH(s) isn't set yet */
      if (height == 0) height = 40; /* SCREEN_HEIGHT(s) isn't set yet */
      Fset_screen_size (screen, make_number (width), make_number (height),
			Qnil);
    }
  /* Kludge kludge kludge kludge. */
  if (position_specified_p)
    {
      Lisp_Object screen;
      XSETR (screen, Lisp_Screen, s);
      Fset_screen_position (screen, make_number (x), make_number (y));
    }
#endif
}

/* The one and only application context associated with the connection
** to the one and only X display that Emacs uses. */
XtAppContext Xt_app_con;

/* The one and only application shell.  Emacs screens are popup shells of this
** application. */
Widget Xt_app_shell;

static void
maybe_set_screen_title_format (shell)
     Widget shell;
{
  BLOCK_INPUT;

  if (NILP (Vscreen_list) ||
      (NILP (Fcdr (Vscreen_list)) &&
       EQ (Fcar (Vscreen_list), Vterminal_screen)))

    /* Only do this if this is the first X screen we're creating (when
       the screen-list contains only one screen, the terminal screen.)

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


extern void maybe_store_wm_command (struct screen *);

static void hack_wm_protocols (Widget);
static void store_class_hints (Widget, char *);

/* Creates the widgets for a screen.  Parms is an alist of
   resources/values to use for the screen.  (ignored right now).
   lisp_window_id is a Lisp description of an X window or Xt
   widget to parse.

   This function does not map the window.
 */
static void
x_create_widgets (struct screen *s,
		  Lisp_Object parms
#ifdef EXTERNAL_WIDGET
		  , Lisp_Object lisp_window_id
#endif
		  )
{
  Widget shell_widget;
  Widget pane_widget;
  Widget screen_widget;
  Widget scrollbar_manager;
#ifndef LWLIB_USES_MOTIF
  Widget lower_pane;
#endif
#ifdef ENERGIZE
  Widget psheet_manager;
#endif
  Widget menubar_widget;
#ifdef EXTERNAL_WIDGET
  Window window_id;
#endif
  char *name;
  Arg av [25];
  int ac = 0;

  BLOCK_INPUT;

  if (STRINGP (s->name))
     name = (char*) XSTRING (s->name)->data;
  else
    name = "emacs";
       
  /* The widget hierarchy is

	argv[0]			shell		pane		SCREEN-NAME
	ApplicationShell	TopLevelShell	XmMainWindow	EmacsScreen

     However the shell/EmacsShell widget has WM_CLASS of SCREEN-NAME/Emacs.
     Normally such shells have name/class shellname/appclass, which in this
     case would be "shell/Emacs" instead of "screen-name/Emacs".  We could
     also get around this by naming the shell "screen-name", but that would
     be confusing because the text area (the EmacsScreen widget inferior of
     the shell) is also called that.  So we just set the WM_CLASS property.
   */

#ifdef EXTERNAL_WIDGET
  if (NILP (lisp_window_id))
    window_id = 0;
  else
    {
      char *string;

      CHECK_STRING (lisp_window_id, 0);
      string = (char *) (XSTRING (lisp_window_id)->data);
      if (string[0] == '0' && (string[1] == 'x' || string[1] == 'X'))
	sscanf (string+2, "%xu", &window_id);
#if 0
      else if (string[0] == 'w')
	{
	  sscanf (string+1, "%x", &parent_widget);
	  if (parent_widget)
	    window_id = XtWindow (parent_widget);
	}
#endif
      else
	sscanf (string, "%lu", &window_id);
    }
#endif /* EXTERNAL_WIDGET */

  ac = 0;
  XtSetArg (av[ac], XtNallowShellResize, True); ac++;

#ifdef EXTERNAL_WIDGET
  if (window_id)
    {
      XtSetArg (av[ac], XtNwindow, window_id); ac++;
    }
  else
#endif
    {
      XtSetArg (av[ac], XtNinput, True); ac++;
    }

  shell_widget = XtCreatePopupShell ("shell",
				     (
#ifdef EXTERNAL_WIDGET
				      window_id ? emacsShellWidgetClass :
#endif
				      topLevelShellWidgetClass
				      ),
				     Xt_app_shell,
				     av, ac);
  maybe_set_screen_title_format (shell_widget);

#ifdef LWLIB_USES_MOTIF
  ac = 0;
  XtSetArg (av[ac], XtNborderWidth, 0); ac++;
  XtSetArg (av[ac], XmNspacing, 0); ac++;
  pane_widget = XmCreateMainWindow (shell_widget, "pane", av, ac);

  /* Now create the initial menubar widget. */
  s->display.x->container = pane_widget;
  initialize_screen_menubar (s);
  menubar_widget = s->display.x->menubar_widget;

  ac = 0;
  XtSetArg (av[ac], XtNmappedWhenManaged, False); ac++;
  XtSetArg (av[ac], XtNemacsScreen, s); ac++;
  screen_widget = XtCreateWidget (name,
				  emacsScreenClass,
				  pane_widget, av, ac);

  initialize_screen_scrollbars (s);
  scrollbar_manager = s->display.x->scrollbar_manager;

  if (scrollbar_manager)
    {
      Dimension sb_manager_width;
      XtVaGetValues (scrollbar_manager, XmNwidth, &sb_manager_width, 0);
      if (sb_manager_width != 0 && sb_manager_width != scrollbar_width)
	scrollbar_width = sb_manager_width;
    }
  else
    scrollbar_width = 0;

# ifdef ENERGIZE
  ac = 0;
  XtSetArg (av[ac], XtNmappedWhenManaged, True); ac++;
  XtSetArg (av[ac], XmNseparatorOn, False); ac++;
  XtSetArg (av[ac], XmNmarginHeight, 0); ac++;
  XtSetArg (av[ac], XmNmarginWidth, 0); ac++;
  XtSetArg (av[ac], XmNsashHeight, 0); ac++;
  XtSetArg (av[ac], XmNsashIndent, 0); ac++;
  XtSetArg (av[ac], XmNsashWidth, 0); ac++;
  XtSetArg (av[ac], XmNsashShadowThickness, 0); ac++;
  XtSetArg (av[ac], XmNspacing, 0); ac++;
  XtSetArg (av[ac], XmNshadowThickness, 0); ac++;
  psheet_manager = XmCreatePanedWindow (pane_widget, "psheet_manager", av, ac);
# endif /* ENERGIZE */

  XmMainWindowSetAreas (pane_widget,
			menubar_widget,		/* menubar (maybe 0) */
			0,			/* command area (psheets) */
			0,			/* horizontal scroll */
			scrollbar_manager,	/* vertical scroll (maybe 0) */
			screen_widget);		/* work area */

#else /* !LWLIB_USES_MOTIF (meaning Athena) */

  /* Create a vertical Paned to hold menubar / psheets / and-the-rest */
  ac = 0;
  XtSetArg (av[ac], XtNborderWidth, 0); ac++;
  XtSetArg (av[ac], XtNorientation, XtorientVertical); ac++;
  pane_widget = XtCreateWidget ("pane",
				panedWidgetClass,
				shell_widget, av, ac);

  /* Create the initial menubar widget. */
  s->display.x->container = pane_widget;
  initialize_screen_menubar (s);
  menubar_widget = s->display.x->menubar_widget;

  /* Create a horizontal Paned to hold scrollbars and the text area */
  ac = 0;
  XtSetArg (av[ac], XtNmappedWhenManaged, True); ac++;
  XtSetArg (av[ac], XtNshowGrip, False); ac++;
  XtSetArg (av[ac], XtNallowResize, True); ac++;
  XtSetArg (av[ac], XtNresizeToPreferred, True); ac++;
  XtSetArg (av[ac], XtNorientation, XtorientHorizontal); ac++;
  XtSetArg (av[ac], XtNinternalBorderWidth, 1); ac++; /* sb_margin... */
  lower_pane = XtCreateWidget ("lower_pane",
				panedWidgetClass,
				pane_widget, av, ac);

  /* Create the initial scrollbars */
  s->display.x->container2 = lower_pane;
  initialize_screen_scrollbars (s);
  scrollbar_manager = s->display.x->scrollbar_manager;

  /* Create the text area */
  ac = 0;
  XtSetArg (av[ac], XtNmappedWhenManaged, True); ac++;
  XtSetArg (av[ac], XtNshowGrip, False); ac++;
  XtSetArg (av[ac], XtNallowResize, True); ac++;
  XtSetArg (av[ac], XtNresizeToPreferred, True); ac++;
  XtSetArg (av[ac], XtNborderWidth, 0); ac++;	/* should this be settable? */
  XtSetArg (av[ac], XtNemacsScreen, s); ac++;
  screen_widget = XtCreateWidget (name,
				  emacsScreenClass,
				  lower_pane, av, ac);

#endif /* !LWLIB_USES_MOTIF (meaning Athena) */

  s->display.x->widget = shell_widget;
  s->display.x->edit_widget = screen_widget;

#ifdef ENERGIZE
  /* Initially unmanaged; no psheets yet. */
  s->display.x->psheet_manager = psheet_manager;
#endif

  if (menubar_widget)
    XtManageChild (menubar_widget);
  if (scrollbar_manager)
    XtManageChild (scrollbar_manager);
  XtManageChild (screen_widget);
#ifndef LWLIB_USES_MOTIF /* Athena */
  XtManageChild (lower_pane);
#endif
  XtManageChild (pane_widget);

#ifdef LWLIB_USES_MOTIF
  {
    /* The MainWindow likes to borrow the space for the menubar and scrollbar
       from the text area; this is not good, so after realizing it (but before
       mapping it) reset the size of the text area to what it wanted to be
       created with. */
    Dimension width, height;
    XtVaGetValues (screen_widget, XtNwidth, &width, XtNheight, &height, 0);
    XtRealizeWidget (shell_widget);
    XtVaSetValues (screen_widget, XtNwidth, width, XtNheight, height, 0);
  }
#else
  XtRealizeWidget (shell_widget);
#endif

  if (!NILP (Vx_screen_defaults) || !NILP (parms))
    x_set_screen_values (s, (NILP (parms)
			     ? Vx_screen_defaults
			     : nconc2 (Fcopy_sequence (parms),
				       Vx_screen_defaults)));

  store_class_hints (shell_widget, name);
  maybe_store_wm_command (s);
  hack_wm_protocols (shell_widget);

#ifdef I18N4
  if (input_context)
    {
      main_window = XtWindow (screen_widget);
      XSetICValues (input_context,
		    XNClientWindow, main_window,
		    XNFocusWindow, main_window,
		    NULL);
    }
#endif

#ifdef HACK_EDITRES
  XtAddEventHandler (shell_widget, 0, True, _XEditResCheckMessages, 0);
#endif

  /* Do a stupid property change to force the server to generate a
     propertyNotify event so that the event_stream server timestamp will
     be initialized to something relevant to the time we created the window.
     */
  XChangeProperty (XtDisplay (screen_widget), XtWindow (screen_widget),
		   Xatom_WM_PROTOCOLS, XA_ATOM, 32, PropModeAppend,
		   (unsigned char*) NULL, 0);

  XtMapWidget (screen_widget);
  UNBLOCK_INPUT;
}


/* If the WM_PROTOCOLS property does not already contain WM_TAKE_FOCUS
   and WM_DELETE_WINDOW, then add them.  (They may already be present
   because of the toolkit (Motif adds them, for example, but Xt doesn't.)
 */
static void
hack_wm_protocols (Widget widget)
{
  Display *dpy = XtDisplay (widget);
  Window w = XtWindow (widget);
  int need_delete = 1;
  int need_focus = 1;
  BLOCK_INPUT;
  {
    Atom type, *atoms = 0;
    int format = 0;
    unsigned long nitems = 0;
    unsigned long bytes_after;

    if (Success == XGetWindowProperty (dpy, w, Xatom_WM_PROTOCOLS,
				       0, 100, False, XA_ATOM,
				       &type, &format, &nitems, &bytes_after,
				       (unsigned char **) &atoms)
	&& format == 32 && type == XA_ATOM)
      while (nitems > 0)
	{
	  nitems--;
	  if (atoms [nitems] == Xatom_WM_DELETE_WINDOW)   need_delete = 0;
	  else if (atoms [nitems] == Xatom_WM_TAKE_FOCUS) need_focus = 0;
	}
    if (atoms) XFree ((char *) atoms);
  }
  {
    Atom props [10];
    int count = 0;
    if (need_delete) props [count++] = Xatom_WM_DELETE_WINDOW;
    if (need_focus)  props [count++] = Xatom_WM_TAKE_FOCUS;
    if (count)
      XChangeProperty (dpy, w, Xatom_WM_PROTOCOLS, XA_ATOM, 32, PropModeAppend,
		       (unsigned char *) props, count);
  }
  UNBLOCK_INPUT;
}

static void
store_class_hints (Widget shell, char *screen_name)
{
  Display *dpy = XtDisplay (shell);
  char *app_name, *app_class;
  XClassHint classhint;
  BLOCK_INPUT;
  XtGetApplicationNameAndClass (dpy, &app_name, &app_class);
  classhint.res_name = screen_name;
  classhint.res_class = app_class;
  XSetClassHint (dpy, XtWindow (shell), &classhint);
  UNBLOCK_INPUT;
}

static void
allocate_x_display_struct (s)
     struct screen* s;
{
  s->output_method = output_x_window;
  s->display.x = (struct x_display *)xmalloc (sizeof (struct x_display));

  /* zero out all slots. */
  memset (s->display.x, 0, sizeof (struct x_display));
  /* yeah, except the lisp ones */
  s->display.x->icon_pixmap = Qnil;
  s->display.x->icon_pixmap_mask = Qnil;
#ifdef ENERGIZE
  s->display.x->current_psheet_buffer = Qnil;
  s->display.x->desired_psheet_buffer = Qnil;
#endif
}

int
x_screen_iconified_p (Lisp_Object screen)
{
  Atom actual_type;
  int actual_format;
  unsigned long nitems, bytesafter;
  unsigned long *datap = 0;
  Widget widget;
  int result = 0;

  if (!SCREEN_IS_X (XSCREEN (screen)))
    return 0;

  widget = XSCREEN (screen)->display.x->widget;
  BLOCK_INPUT;
  if (Success == XGetWindowProperty (XtDisplay (widget), XtWindow (widget),
				     Xatom_WM_STATE, 0, 2, False,
				     Xatom_WM_STATE, &actual_type,
				     &actual_format, &nitems, &bytesafter,
				     (unsigned char **) &datap)
      && datap)
    {
      if (nitems <= 2	/* "suggested" by ICCCM version 1 */
	  && datap [0] == IconicState)
	result = 1;
      XFree ((char *) datap);
    }
  UNBLOCK_INPUT;
  return result;
}


Lisp_Object Vdefault_screen_name;

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
with \"0x\".")
  (parms, lisp_window_id)
     Lisp_Object parms, lisp_window_id;
{
  struct screen *s;
  Lisp_Object screen = Qnil;
  Lisp_Object name = Qnil;
  struct gcpro gcpro1;
  struct gcpro gcpro2;
  GCPRO2 (screen, name);
  
  if (x_current_display == 0)
    error (GETTEXT ("X windows are not in use or not initialized"));
  
#ifndef EXTERNAL_WIDGET
  if (!NILP (lisp_window_id))
    error ("support for external widgets was not enabled at compile-time");
#endif

  s = make_screen (1);
  
  allocate_x_display_struct (s);
  name = Fassq (Qname, parms);
  if (!NILP (name))
    {
      name = Fcdr (name);
      CHECK_STRING (name, 0);
    }
  else if (STRINGP (Vdefault_screen_name))
    name = Vdefault_screen_name;
  else
    name = build_string ("emacs");

  s->name = name;

  XSETR (screen, Lisp_Screen, s);

  x_create_widgets (s, parms
#ifdef EXTERNAL_WIDGET
		    , lisp_window_id
#endif
		    );
  
  /* do this after anything that might call Fsignal() before the screen
   * is in a usable state. */
  Vscreen_list = Fcons (screen, Vscreen_list);

  /* This runs lisp code, and thus might GC.  If the selected screen is still
     the terminal screen (meaning that we're in the middle of creating the
     initial X screen) then select the X screen now, so that GC messages don't
     get written on the terminal screen.  This is kind of a hack...
   */
  if (selected_screen == XSCREEN (Vterminal_screen))
    select_screen_internal (s);
  init_screen_faces (s);
  /* This is the first place all the needed information is available to
     initialize these variables.
     */
  XWINDOW (s->root_window)->pixleft = INT_BORDER (s);
  XWINDOW (s->root_window)->pixtop = INT_BORDER (s);
  if (!NILP (XWINDOW (s->root_window)->next))
    {
      int font_height = XFONT (SCREEN_DEFAULT_FONT (s))->height;
      struct window *mini_window = XWINDOW(XWINDOW(s->root_window)->next);

      mini_window->pixleft = INT_BORDER (s);
      mini_window->pixtop = PIXH (s) + INT_BORDER(s) - font_height;
      mini_window->pixwidth = PIXW (s);
      mini_window->pixheight = font_height;
    }

  x_format_screen_title (s);

  /* Now map the window (shell widget) if appropriate. */
  BLOCK_INPUT;
  XtPopup (s->display.x->widget, XtGrabNone);
  UNBLOCK_INPUT;

  run_hook_with_args (Qcreate_screen_hook, 1, screen);
  UNGCPRO;
  return screen;
}


struct screen *
get_screen_on_screen (Screen *sc)
{
  Lisp_Object tail, screen;
  struct screen *s;

  for (tail = Vscreen_list; CONSP (tail); tail = XCONS (tail)->cdr)
    {
      screen = XCONS (tail)->car;
      if (!SCREENP (screen))
        continue;
      s = XSCREEN (screen);
      if (SCREEN_IS_X (s) && XtScreen (s->display.x->edit_widget) == sc)
        return s;
    }
  return 0;
}

struct screen *
get_screen_on_display (Display *d)
{
  Lisp_Object tail, screen;
  struct screen *s;

  for (tail = Vscreen_list; CONSP (tail); tail = XCONS (tail)->cdr)
    {
      screen = XCONS (tail)->car;
      if (!SCREENP (screen))
        continue;
      s = XSCREEN (screen);
      if (SCREEN_IS_X (s) && XtDisplay (s->display.x->edit_widget) == d)
        return s;
    }
  return 0;
}

struct screen *
get_x_screen (Lisp_Object screen)
{
  struct screen *s;

 retry:
  if (NILP (screen))
    s = selected_screen;
  else
    {
      CHECK_SCREEN (screen, 0);
      s = XSCREEN (screen);
    }

  if (!SCREEN_IS_X (s))
    {
      /* >>> define this function?? */
      screen = wrong_type_argument (intern ("x-screen-p"), (screen));
      goto retry;
    }
  return (s);
}


DEFUN ("x-display-visual-class", Fx_display_visual_class,
       Sx_display_visual_class, 0, 1, 0,
       "Returns the visual class of the display `screen' is on.\n\
The returned value will be one of the symbols StaticGray, GrayScale,\n\
StaticColor, PseudoColor, TrueColor, or DirectColor.")
	(screen)
     Lisp_Object screen;
{
  struct screen *s = get_x_screen (screen);

  switch (DefaultVisualOfScreen (XtScreen (s->display.x->widget))->class)
    {
    case StaticGray:  return (intern ("StaticGray"));
    case GrayScale:   return (intern ("GrayScale"));
    case StaticColor: return (intern ("StaticColor"));
    case PseudoColor: return (intern ("PseudoColor"));
    case TrueColor:   return (intern ("TrueColor"));
    case DirectColor: return (intern ("DirectColor"));
    default:
      error (GETTEXT ("display has an unknown visual class"));
    }
}

DEFUN ("x-pixel-width", Fx_pixel_width, Sx_pixel_width, 0, 1, 0,
  "Returns the width in pixels of the given screen.")
  (screen)
     Lisp_Object screen;
{
  struct screen *s = get_x_screen (screen);

  return make_number (PIXEL_WIDTH (s));
}

DEFUN ("x-pixel-height", Fx_pixel_height, Sx_pixel_height, 0, 1, 0,
  "Returns the height in pixels of the given screen.")
  (screen)
     Lisp_Object screen;
{
  struct screen *s = get_x_screen (screen);

  return make_number (PIXEL_HEIGHT (s));
}

DEFUN ("x-display-pixel-width", Fx_display_pixel_width, Sx_display_pixel_width,
       0, 1, 0,
       "Returns the width in pixels of the display `screen' is on.")
     (screen)
  Lisp_Object screen;
{
  struct screen *s = get_x_screen (screen);
  Display *dpy = XtDisplay (s->display.x->widget);

  return make_number (DisplayWidth (dpy, DefaultScreen (dpy)));
}

DEFUN ("x-display-pixel-height", Fx_display_pixel_height,
       Sx_display_pixel_height,
       0, 1, 0,
       "Returns the height in pixels of the display `screen' is on.")
     (screen)
  Lisp_Object screen;
{
  struct screen *s = get_x_screen (screen);
  Display *dpy = XtDisplay (s->display.x->widget);

  return make_number (DisplayHeight (dpy, DefaultScreen (dpy)));
}

DEFUN ("x-display-planes", Fx_display_planes, Sx_display_planes,
       0, 1, 0,
       "Returns the number of bitplanes of the display `screen' is on.")
     (screen)
  Lisp_Object screen;
{
  struct screen *s = get_x_screen (screen);
  Display *dpy = XtDisplay (s->display.x->widget);

  return make_number (DisplayPlanes (dpy, DefaultScreen (dpy)));
}

DEFUN ("x-display-color-cells", Fx_display_color_cells, Sx_display_color_cells,
       0, 1, 0,
       "Returns the number of color cells of the display `screen' is on.")
     (screen)
  Lisp_Object screen;
{
  struct screen *s = get_x_screen (screen);
  Display *dpy = XtDisplay (s->display.x->widget);

  return make_number (DisplayCells (dpy, DefaultScreen (dpy)));
}

DEFUN ("x-server-vendor", Fx_server_vendor, Sx_server_vendor, 0, 1, 0,
       "Returns the vendor ID string of the X server `screen' is on.")
     (screen)
  Lisp_Object screen;
{
  struct screen *s = get_x_screen (screen);
  Display *dpy = XtDisplay (s->display.x->widget);
  char *vendor = ServerVendor (dpy);

  if (vendor)
    return (build_string (vendor));
  else
    return (build_string (""));
}

DEFUN ("x-server-version", Fx_server_version, Sx_server_version, 0, 1, 0,
       "Returns the version numbers of the X server `screen' is on.\n\
The returned value is a list of three integers: the major and minor\n\
version numbers of the X Protocol in use, and the vendor-specific release\n\
number.  See also `x-server-vendor'.")
     (screen)
  Lisp_Object screen;
{
  struct screen *s = get_x_screen (screen);
  Display *dpy = XtDisplay (s->display.x->widget);

  return list3 (make_number (ProtocolVersion (dpy)),
		make_number (ProtocolRevision (dpy)),
		make_number (VendorRelease (dpy)));
}


DEFUN ("x-set-screen-icon-pixmap", Fx_set_screen_icon_pixmap,
       Sx_set_screen_icon_pixmap, 2, 3, 0,
  "Set the icon of the given screen to the given pixmap,\n\
which should be an object returned by `make-pixmap', or nil.\n\
If the given pixmap has a mask, that will be used as the icon mask;\n\
 however, not all window managers support this.\n\
The window manager is also not required to support color pixmaps,\n\
 only bitmaps (one plane deep.)\n\
If the second argument is a pixmap without a mask, then the optional\n\
 third argument may be the pixmap to use as the mask (it must be one\n\
 plane deep.)")
  (screen, pixmap, mask)
    Lisp_Object screen, pixmap, mask;
{
  struct screen *s = get_x_screen (screen);
  Pixmap x_pixmap, x_mask;

  if (!NILP (pixmap))
    CHECK_PIXMAP (pixmap, 0);
  if (!NILP (mask))
    CHECK_PIXMAP (mask, 0);

  if (!NILP (pixmap))
    {
      x_pixmap = XPIXMAP (pixmap)->pixmap;
      x_mask = XPIXMAP (pixmap)->mask;
      if (x_mask) mask = Qnil;
    }
  else
    {
      x_pixmap = 0;
      x_mask = 0;
      mask = Qnil;
    }

  if (!NILP (mask))
    {
      if (XPIXMAP (mask)->depth > 1)
	signal_error (Qerror, list2 (build_string
				     (GETTEXT ("mask must be one plane")),
				     mask));
      x_mask = XPIXMAP (mask)->pixmap;
    }

  /* GC-protect the lisp objects (and underlying X data) */
  s->display.x->icon_pixmap = pixmap;
  s->display.x->icon_pixmap_mask = mask;

  /* Store the X data into the widget. */
  {
    Arg av [10];
    int ac = 0;
    XtSetArg (av [ac], XtNiconPixmap, x_pixmap); ac++;
    XtSetArg (av [ac], XtNiconMask, x_mask); ac++;
    XtSetValues (s->display.x->widget, av, ac);
  }

  return pixmap;
}


DEFUN ("x-grab-pointer", Fx_grab_pointer, Sx_grab_pointer, 0, 2, 0,
  "Grab the pointer and restrict it to its current window.\n\
If optional CURSOR argument is non-nil, change the pointer shape to that\n\
 until `x-ungrab-pointer' is called (it should be an object returned by the\n\
 `make-cursor' function.)\n\
If the second optional argument MOUSE-ONLY is non-nil, ignore all keyboard\n\
 events during the grab.\n\
Returns t if the grab is successful, nil otherwise.")
  (cursor, ignore_keyboard)
     Lisp_Object cursor, ignore_keyboard;
{
  Window w;
  int pointer_mode, result;

  if (! NILP (cursor))
    CHECK_CURSOR (cursor, 0);

  if (! NILP (ignore_keyboard))
    pointer_mode = GrabModeSync;
  else
    pointer_mode = GrabModeAsync;

  w = XtWindow (XSCREEN (XWINDOW (selected_window)->screen)
		->display.x->edit_widget);

  BLOCK_INPUT;
  /* #### Possibly this needs to gcpro the cursor somehow, but it doesn't
     seem to cause a problem if XFreeCursor is called on a cursor in use
     in a grab; I suppose the X server counts the grab as a reference
     and doesn't free it until it exits? */
  result = XGrabPointer (x_current_display, w,
			 False,
			 ButtonMotionMask | ButtonPressMask
			 | ButtonReleaseMask | PointerMotionHintMask,
			 GrabModeAsync,	      /* Keep pointer events flowing */
			 pointer_mode,	      /* Stall keyboard events */
			 w,		      /* Stay in this window */
			 (NILP (cursor) ? 0 : XCURSOR (cursor)->cursor),
			 CurrentTime);
  UNBLOCK_INPUT;
  return ((result == GrabSuccess) ? Qt : Qnil);
}

DEFUN ("x-ungrab-pointer", Fx_ungrab_pointer, Sx_ungrab_pointer, 0, 0, 0,
  "Release a pointer grab made with `x-grab-pointer.'")
  ()
{
  BLOCK_INPUT;
  XUngrabPointer (x_current_display, CurrentTime);
  UNBLOCK_INPUT;
  return Qnil;
}


/* handlers for the eval-events pushed on the queue by event-Xt.c */

Lisp_Object Qx_EnterNotify_internal, Qx_LeaveNotify_internal;
Lisp_Object Qx_FocusIn_internal, Qx_FocusOut_internal;
Lisp_Object Qx_VisibilityNotify_internal, Qx_non_VisibilityNotify_internal;
Lisp_Object Qx_MapNotify_internal, Qx_UnmapNotify_internal;

DEFUN ("x-EnterNotify-internal", Fx_EnterNotify_internal,
       Sx_EnterNotify_internal, 1, 1, 0, "hands off")
     (screen)
     Lisp_Object screen;
{
  CHECK_SCREEN (screen, 0);
  if (! SCREEN_IS_X (XSCREEN (screen))) return Qnil; /* may be deleted */
/*  XSCREEN (screen)->display.x->mouse_p = 1; */
  run_hook_with_args (Qmouse_enter_screen_hook, 1, screen);
  return Qnil;
}

DEFUN ("x-LeaveNotify-internal", Fx_LeaveNotify_internal,
       Sx_LeaveNotify_internal, 1, 1, 0, "hands off")
     (screen)
     Lisp_Object screen;
{
  CHECK_SCREEN (screen, 0);
  if (! SCREEN_IS_X (XSCREEN (screen)))
    return Qnil; /* may be deleted */
/*  XSCREEN (screen)->display.x->mouse_p = 0; */
  run_hook_with_args (Qmouse_leave_screen_hook, 1, screen);
  return Qnil;
}

/* This is true if any widget in any emacs screen has the X keyboard focus
   Flase otherwise. */
int any_screen_has_focus_p;

/* The select-screen-hook and deselect-screen-hook are run from the 
   select_screen_internal() function; however, we run them from the FocusIn
   and FocusOut handlers as well, because under X, the "selectedness" of a
   screen has slightly funny semantics; according to emacs, a screen is
   not "deselected" unless some other screen is selected.  This is so that
   there is always some current window and buffer, and so on.  However, it's
   useful to run the deselect-screen-hook when emacs loses the X keyboard
   focus (that is, no emacs screen is the X selected window).  Likewise,
   it's useful to run the select-screen-hook when some emacs window regains
   the focus, even if that window is already the selected screen from emacs's
   point of view.

   If we don't do this, then the select-screen-hook (meaning auto-raise)
   isn't run if (in a point-to-type world) the mouse moves into the same
   emacs window that it originally left.  Clearly this isn't what someone
   who would want auto-raise would want.

   This means that sometimes the deselect-screen-hook will be called twice.
   This kind of stinks.

   If there are two screens, s1, and s2, where s1 is selected:

    mouse moves into s1:   FocusIn s1 runs select-screen-hook, because s1
    			   is the selected screen (FocusIn only does this
			   for the selected screen.)

    			   select_screen_internal doesn't run deselect-hook
			   since s1 is already selected_screen, although it
			   didn't have the X focus.

			   net result: select s1 run.

    mouse moves into s2:   FocusOut s1 runs deselect-screen-hook, but the
			   selected_screen is still s1.

			   FocusIn s2 does not run select-screen-hook,
			   because s2 is not the selected screen.

			   select_screen_internal runs deselect-hook s1,
			   then selects s2, then runs select-hook s2.

			   net result: deselect s1, deselect s1, select s2.
			   One of those deselects is superfluous.

    mouse moves elsewhere: FocusOut s2 runs deselect-screen-hook, but the
			   selected_screen is still s2.

			   net result: deselect s2.

    mouse moves into s1:   FocusIn s1 does not run select-screen-hook.

    			   select_screen_internal runs deselect-hook s2;
			   changes selected_screen, runs select-hook s1.

			   net result: deselect s2, select s1.
			   That deselect is superfluous.

   Things get even nastier if there is a big delay between when select-screen
   is called and when the Focus events are handled (as when lisp code calls
   select-screen and then doesn't return to top level for a while.)
 */

DEFUN ("x-FocusIn-internal", Fx_FocusIn_internal,
       Sx_FocusIn_internal, 1, 1, 0, "hands off")
     (screen)
     Lisp_Object screen;
{
  int getting_focus = !any_screen_has_focus_p;
  CHECK_SCREEN (screen, 0);
  if (! SCREEN_IS_X (XSCREEN (screen))) return Qnil; /* may be deleted */
  any_screen_has_focus_p = 1;
  XSCREEN (screen)->display.x->focus_p = 1;
  if (XSCREEN (screen) == selected_screen)
    {
      Fastmove_cursor (XSCREEN (screen));
      /* see comment above */
      if (getting_focus)
	if (!NILP (Vrun_hooks))
	  call1 (Vrun_hooks, Qselect_screen_hook);
    }
  else
    select_screen_internal (XSCREEN (screen));
  return Qnil;
}

DEFUN ("x-FocusOut-internal", Fx_FocusOut_internal,
       Sx_FocusOut_internal, 1, 1, 0, "hands off")
     (screen)
     Lisp_Object screen;
{
  CHECK_SCREEN (screen, 0);
  if (! SCREEN_IS_X (XSCREEN (screen))) return Qnil; /* may be deleted */
  any_screen_has_focus_p = 0;
  XSCREEN (screen)->display.x->focus_p = 0;
  if (XSCREEN (screen) == selected_screen)
    {
      Fastmove_cursor (XSCREEN (screen));
      /* see comment above */
      if (!NILP (Vrun_hooks))
	call1 (Vrun_hooks, Qdeselect_screen_hook);
    }
  return Qnil;
}

DEFUN ("x-VisibilityNotify-internal", Fx_VisibilityNotify_internal,
       Sx_VisibilityNotify_internal, 1, 1, 0, "hands off")
     (screen)
     Lisp_Object screen;
{
  CHECK_SCREEN (screen, 0);
  if (! SCREEN_IS_X (XSCREEN (screen))) return Qnil; /* may be deleted */
  XSCREEN (screen)->display.x->totally_visible_p = 1;
  return Qnil;
}

DEFUN ("x-non-VisibilityNotify-internal", Fx_non_VisibilityNotify_internal,
       Sx_non_VisibilityNotify_internal, 1, 1, 0, "hands off")
     (screen)
     Lisp_Object screen;
{
  CHECK_SCREEN (screen, 0);
  if (! SCREEN_IS_X (XSCREEN (screen))) return Qnil; /* may be deleted */
  XSCREEN (screen)->display.x->totally_visible_p = 0;
  return Qnil;
}

DEFUN ("x-MapNotify-internal", Fx_MapNotify_internal,
       Sx_MapNotify_internal, 1, 1, 0, "hands off")
     (screen)
     Lisp_Object screen;
{
  CHECK_SCREEN (screen, 0);
  if (! SCREEN_IS_X (XSCREEN (screen))) return Qnil; /* may be deleted */
  XSCREEN (screen)->display.x->totally_visible_p = 1;
  if (! XSCREEN (screen)->visible)
    {
      XSCREEN (screen)->visible = 1;
      SET_SCREEN_GARBAGED (XSCREEN (screen));
      run_hook_with_args (Qmap_screen_hook, 1, screen);
    }
  return Qnil;
}


DEFUN ("x-UnmapNotify-internal", Fx_UnmapNotify_internal,
       Sx_UnmapNotify_internal, 1, 1, 0, "hands off")
     (screen)
     Lisp_Object screen;
{
  CHECK_SCREEN (screen, 0);
  if (! SCREEN_IS_X (XSCREEN (screen))) return Qnil; /* may be deleted */
  XSCREEN (screen)->display.x->totally_visible_p = 0;
  if (XSCREEN (screen)->visible)
    {
      XSCREEN (screen)->visible = 0;
      run_hook_with_args (Qunmap_screen_hook, 1, screen);
    }
  return Qnil;
}


#if 0 /* #### This stuff is obsolete; with the new event model, 
	      regular keyboard macros work just as well as this.
       */
xxxxDEFUN ("x-rebind-key", Fx_rebind_key, Sx_rebind_key, 3, 3, 0,
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
    error (GETTEXT ("Keysym does not exist"));

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
	    error (GETTEXT ("Can't have more than 16 modifiers"));

	  mod = Fcar (rest);
	  CHECK_STRING (mod, 3);
	  BLOCK_INPUT;
	  modifier_list[i] = XStringToKeysym ((char *) XSTRING (mod)->data);
	  UNBLOCK_INPUT;
	  if (modifier_list[i] == NoSymbol
	      || !IsModifierKey (modifier_list[i]))
	    error (GETTEXT ("Element is not a modifier keysym"));
	  i++;
	}
      
      BLOCK_INPUT;
      XRebindKeysym (x_current_display, keysym, modifier_list, i,
		     XSTRING (newstring)->data, XSTRING (newstring)->size);
      UNBLOCK_INPUT;
    }

  return Qnil;
}
  
xxxxDEFUN ("x-rebind-keys", Fx_rebind_keys, Sx_rebind_keys, 2, 2, 0,
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

  CHECK_FIXNUM (keycode, 1);
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
\n\
The first arg is the name of the resource to retrieve, such as \"font\".\n\
The second arg is the class of the resource to retrieve, like \"Font\".\n\
The third arg should be one of the symbols string, integer, or boolean,\n\
specifying the type of object that the database is searched for.\n\
The fourth arg is the screen to search for the resources on, defaulting\n\
to the selected screen.\n\
\n\
The resource names passed to this function are looked up relative to the\n\
screen widget, so the call\n\
\n\
    (x-get-resource \"font\" \"Font\" 'string)\n\
\n\
is an interface to a C call something like\n\
\n\
    XrmGetResource (db, \"lemacs.shell.pane.this_screen_name.font\",\n\
			\"Emacs.Shell.Paned.EmacsScreen.Font\",\n\
			\"String\");\n\
\n\
Therefore if you want to retrieve a deeper resource, for example,\n\
\"Emacs.foo.foreground\", you need to specify the same number of links\n\
in the class path:\n\
\n\
    (x-get-resource \"foo.foreground\" \"Thing.Foreground\" 'string)\n\
\n\
which is equivalent to something like\n\
\n\
    XrmGetResource (db, \"lemacs.shell.pane.this_screen_name.foo.foreground\",\n\
			\"Emacs.Shell.Paned.EmacsScreen.Thing.Foreground\",\n\
			\"String\");\n\
\n\
The returned value of this function is nil if the queried resource is not\n\
found.  If the third arg is `string', a string is returned, and if it is\n\
`integer', an integer is returned.  If the third arg is `boolean', then the\n\
returned value is the list (t) for true, (nil) for false, and is nil to\n\
mean ``unspecified.''")
     (name, class, type, screen)
*/


static void
construct_name_list (Widget widget, char *name, char *class)
{
  char *stack [100][2];
  Widget this;
  int count = 0;
  char *name_tail, *class_tail;

  for (this = widget; this; this = XtParent (this))
    {
      stack [count][0] = this->core.name;
      stack [count][1] = XtClass (this)->core_class.class_name;
      count++;
    }

  /* The root widget is an application shell; resource lookups use the
     specified application name and application class in preference to
     the name/class of that widget (which is argv[0] / "ApplicationShell").
     Generally the app name and class will be argv[0] / "Emacs" but
     the former can be set via the -name command-line option, and the
     latter can be set by changing `x-emacs-application-class' in
     lisp/term/x-win.el.
   */
  BLOCK_INPUT;
  XtGetApplicationNameAndClass (XtDisplay (widget),
				&stack [count-1][0],
				&stack [count-1][1]);
  UNBLOCK_INPUT;

  name [0] = 0;
  class [0] = 0;

  name_tail  = name;
  class_tail = class;
  count--;
  for (; count >= 0; count--)
    {
      strcat (name_tail,  stack [count][0]);
      for (; *name_tail; name_tail++)
	if (*name_tail == '.') *name_tail = '_';
      strcat (name_tail, ".");
      name_tail++;

      strcat (class_tail, stack [count][1]);
      for (; *class_tail; class_tail++)
	if (*class_tail == '.') *class_tail = '_';
      strcat (class_tail, ".");
      class_tail++;
    }
}

DEFUN ("x-get-resource", Fx_get_resource, Sx_get_resource, 3, 4, 0, 0)
     (name, class, type, screen)
     Lisp_Object name, class, type, screen;
{
  char name_string [1024], class_string [1024];
  char *raw_result;
  XrmDatabase db;

  CHECK_STRING (name, 0);
  CHECK_STRING (class, 0);
  CHECK_SYMBOL (type, 0);
  if (NILP (screen))
    screen = Fselected_screen ();
  else
    CHECK_SCREEN (screen, 0);
  if (! SCREEN_IS_X (XSCREEN (screen)))
    return Qnil;

  db = XtDatabase (XtDisplay (XSCREEN (screen)->display.x->widget));

  /* Actually we only really need to call this once per screen, since we know
     that Xt widgets can't ever be reparented.  But it's not a big performance
     hit, so why bother making the screen struct any larger... */
  construct_name_list (XSCREEN (screen)->display.x->edit_widget,
		       name_string, class_string);

  strcat (name_string,  (char *) XSTRING (name)->data);
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
      signal_error (Qerror,
                    list3 (build_string
                          (GETTEXT ("class list and name list must be the same length")),
                           build_string (name_string),
                           build_string (class_string)));
    BLOCK_INPUT;
    result = XrmQGetResource (db, namelist, classlist, &xrm_type, &xrm_value);
    UNBLOCK_INPUT;

    if (result != True || xrm_type != string_quark)
      return Qnil;
    raw_result = (char *) xrm_value.addr;
  }

  if (EQ (type, Qstring))
    return build_string (raw_result);
  else if (EQ (type, Qboolean))
    {
      if (!strcasecmp (raw_result, "off") ||
	  !strcasecmp (raw_result, "false") ||
	  !strcasecmp (raw_result,"no"))
	return Fcons (Qnil, Qnil);
      else if (!strcasecmp (raw_result, "on") ||
	       !strcasecmp (raw_result, "true") ||
	       !strcasecmp (raw_result, "yes"))
	return Fcons (Qt, Qnil);
      else
	{
	  char str[255];
	  sprintf (str, "can't convert %s: %s to a Boolean",
		   name_string, raw_result);
	  return Fsignal (Qerror, list1 (build_string (str)));
	}
    }
  else if (EQ (type, Qinteger))
    {
      int i;
      char c;
      if (1 != sscanf (raw_result, "%d%c", &i, &c))
	{
	  char str [255];
	  sprintf (str, "can't convert %s: %s to an integer",
		   name_string, raw_result);
	  return Fsignal (Qerror, list1 (build_string (str)));
	}
      else
	return make_number (i);
    }
  else
    return
      Fsignal (Qwrong_type_argument,
	       list2 (build_string (GETTEXT ("should be string, integer, or boolean")),
                      type));
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


DEFUN ("x-set-screen-pointer", Fx_set_screen_pointer, Sx_set_screen_pointer,
       2, 2, 0,
       "Set the mouse cursor of SCREEN to the given cursor,\n\
which should be an object returned by `make-cursor'.")
     (screen, cursor)
     Lisp_Object screen, cursor;
{
  CHECK_CURSOR (cursor, 0);
  CHECK_SCREEN (screen, 0);
  if (! SCREEN_IS_X (XSCREEN (screen)))
    return Qnil;
  BLOCK_INPUT;
  XDefineCursor (XtDisplay (XSCREEN (screen)->display.x->edit_widget),
		 XtWindow (XSCREEN (screen)->display.x->edit_widget),
		 XCURSOR (cursor)->cursor);
  XSync (XtDisplay (XSCREEN (screen)->display.x->edit_widget), 0);
  UNBLOCK_INPUT;
  /* #### If the user cuts this pointer, we'll get X errors.
     This needs to be rethunk. */
  /* change_cursor_for_gc() acesses this */
  store_screen_param (XSCREEN (screen), Qpointer, cursor);
  return Qnil;
}

DEFUN ("x-set-scrollbar-pointer", Fx_set_scrollbar_pointer,
       Sx_set_scrollbar_pointer, 2, 2, 0,
       "Set the mouse cursor of the scrollbars on SCREEN to the given\n\
cursor, which should be an object returned by `make-cursor'.")
     (screen, cursor)
     Lisp_Object screen, cursor;
{
  Widget sbm;
  CHECK_SCREEN (screen, 0);
  if (! SCREEN_IS_X (XSCREEN (screen)))
    return Qnil;
  if (! CURSORP (cursor))
    return Qnil;
  sbm = XSCREEN (screen)->display.x->scrollbar_manager;
  if (! sbm)
    return Qnil;
  BLOCK_INPUT;
  XDefineCursor (XtDisplay (sbm), XtWindow (sbm), XCURSOR (cursor)->cursor);
  XSync (XtDisplay (sbm), 0);
  UNBLOCK_INPUT;
  /* #### If the user cuts this pointer, we'll get X errors.
     This needs to be rethunk. */
  store_screen_param (XSCREEN (screen), Qscrollbar_pointer, cursor);
  return Qnil;
}


/* GC calls x_show_gc_cursor() with a cursor object to turn on the GC cursor,
   and with nil to turn it off.
*/

static Lisp_Object Vpre_gc_cursor;

int
x_show_gc_cursor (struct screen* s, Lisp_Object cursor)
{
  Lisp_Object screen;
  XSETR (screen, Lisp_Screen, s);
  if (NILP (cursor))
    {
      if (!NILP (Vpre_gc_cursor))
	{
	  if (!CURSORP (Vpre_gc_cursor)) abort ();
	  /* We know it's a screen, we know it's a pointer, so
	     x-set-screen-pointer won't error. */
	  Fx_set_screen_pointer (screen, Vpre_gc_cursor);
	  return 1;
	}
      return 0;
    }
  else if (CURSORP (cursor))
    {
      Vpre_gc_cursor = get_screen_param (XSCREEN (screen), Qpointer);

      if (!CURSORP (Vpre_gc_cursor))
	/* if we don't know what cursor is there, don't change to the GC
	   cursor, because we'd have no way of changing back... */
	return 0;

      /* We know it's a screen, we know it's a pointer, so
	 x-set-screen-pointer won't error. */
      Fx_set_screen_pointer (screen, cursor);
      return 1;
    }
  else
    return 0;
}


static void Xatoms_of_xfns (void);

#ifdef TOOLTALK
extern void init_tooltalk (int *argc, char **argv);
#ifdef SPARCWORKS
extern void init_sparcworks (int *argc, char **argv);
#endif
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

#ifdef TOOLTALK
  extern void make_argc_argv(Lisp_Object argv_list, int *argc, char ***argv);
  extern Lisp_Object make_arg_list(int argc, char **argv);
  int argc;
  char **argv;
#endif

  if (x_current_display != 0)
    error (GETTEXT ("X server connection is already initialized"));

  /* This is what sets x_current_display.  This also initializes many symbols,
     such as those used for input. */
  argv_rest = x_term_init (argv_list);
  
  Vwindow_system_version = make_number (11);
  Xatoms_of_xfns ();
  Xatoms_of_xselect ();

#ifdef USE_SOUND
  /* When running on a SparcStation or SGI, we cannot use digitized sounds as
     beeps unless emacs is running on the same machine that $DISPLAY points
     to, and $DISPLAY points to screen 0 of that machine.
   */
  {
    char *dpy = DisplayString (x_current_display);
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
#endif /* USE_SOUND */

#ifdef TOOLTALK
  make_argc_argv (argv_rest, &argc, &argv);
#ifdef SPARCWORKS
  init_sparcworks (&argc, argv);
#endif
  init_tooltalk (&argc, argv);
  argv_rest = make_arg_list (argc, argv);
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
  if (! SCREEN_IS_X (XSCREEN (screen)))
    return Qnil;
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
    fatal (GETTEXT ("No current X display connection to close"));
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
      message (GETTEXT ("X connection is synchronous"));
    }
  else
    {
      BLOCK_INPUT;
      XSetAfterFunction (x_current_display, 0);
      UNBLOCK_INPUT;
      message (GETTEXT ("X connection is asynchronous"));
    }
  return arg;
}


void
syms_of_xfns ()
{
  /* This is zero if not using X windows.  */
  x_current_display = 0;

  WM_COMMAND_screen = Qnil;
  staticpro (&WM_COMMAND_screen);

  DEFVAR_LISP ("x-gc-pointer-shape", &Vx_gc_pointer_shape,
   "The shape of the mouse-pointer during garbage collection.\n\
If this is nil, then the cursor will not be changed, and echo-area messages\n\
will be used instead.");
  Vx_gc_pointer_shape = Qnil;

  DEFVAR_LISP ("x-scrollbar-pointer-shape", &Vx_scrollbar_pointer_shape,
  "The shape of the mouse pointer when over a scrollbar.");
  Vx_scrollbar_pointer_shape = Qnil;

  DEFVAR_LISP ("bar-cursor", &Vbar_cursor,
	       "Use vertical bar cursor if non-nil.");
  Vbar_cursor = Qnil;

  DEFVAR_LISP ("x-screen-defaults", &Vx_screen_defaults,
    "Alist of default screen-creation parameters for X-window screens.\n\
These override what is specified in `~/.Xdefaults' but are overridden\n\
by the arguments to the particular call to `x-create-screen'.");
  Vx_screen_defaults = Qnil;

  DEFVAR_LISP ("default-screen-name", &Vdefault_screen_name,
    "The default name to assign to newly-created screens.\n\
This can be overridden by arguments to `x-create-screen'.\n\
This must be a string.");
  Vdefault_screen_name = Fpurecopy (build_string ("emacs"));

  DEFVAR_LISP ("x-emacs-application-class", &Vx_emacs_application_class,
	       "The X application class of the Emacs process.\n\
This controls, among other things, the name of the `app-defaults' file\n\
that emacs will use.  For changes to this variable to take effect, they\n\
must be made before the connection to the X server is initialized, that is,\n\
this variable may only be changed before emacs is dumped, or by setting it\n\
in the file lisp/term/x-win.el.");
  Vx_emacs_application_class = Fpurecopy (build_string ("Emacs"));

  DEFVAR_BOOL ("x-allow-sendevents", &x_allow_sendevents,
    "*Non-nil means to allow synthetic events.  Nil means they are ignored.\n\
Beware: allowing emacs to process SendEvents opens a big security hole.");
  x_allow_sendevents = 0;

  /* This isn't really the right place for this...  Initialized in xterm.c */
  DEFVAR_LISP ("lucid-logo", &Vlucid_logo, 0);
  Vlucid_logo = Qnil;

  staticpro (&Vpre_gc_cursor);
  Vpre_gc_cursor = Qnil;

  defsubr (&Sx_display_visual_class);
  defsubr (&Sx_pixel_width);
  defsubr (&Sx_pixel_height);
  defsubr (&Sx_display_pixel_width);
  defsubr (&Sx_display_pixel_height);
  defsubr (&Sx_display_planes);
  defsubr (&Sx_display_color_cells);
  defsubr (&Sx_server_vendor);
  defsubr (&Sx_server_version);
  defsubr (&Sx_window_id);
  defsubr (&Sx_grab_pointer);
  defsubr (&Sx_ungrab_pointer);
  defsubr (&Sx_create_screen);
  defsubr (&Sx_open_connection);
  defsubr (&Sx_close_current_connection);
  defsubr (&Sx_debug_mode);
  defsubr (&Sx_get_resource);
  defsubr (&Sx_set_screen_icon_pixmap);
  defsubr (&Sx_set_screen_pointer);
  defsubr (&Sx_set_scrollbar_pointer);
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

#ifdef DUBIOUS_X_ERROR_HANDLING
  defsymbol (&Qx_error, "x-error");
#endif

  defsymbol (&Qx_EnterNotify_internal, "x-EnterNotify-internal");
  defsymbol (&Qx_LeaveNotify_internal, "x-LeaveNotify-internal");
  defsymbol (&Qx_FocusIn_internal, "x-FocusIn-internal");
  defsymbol (&Qx_FocusOut_internal, "x-FocusOut-internal");
  defsymbol (&Qx_MapNotify_internal, "x-MapNotify-internal");
  defsymbol (&Qx_UnmapNotify_internal, "x-UnmapNotify-internal");
  defsymbol (&Qx_VisibilityNotify_internal, "x-VisibilityNotify-internal");
  defsymbol (&Qx_non_VisibilityNotify_internal, "x-non-VisibilityNotify-internal");
  defsymbol (&Qstring, "string");
  defsymbol (&Qname, "name");
  defsymbol (&Qboolean, "boolean");
  defsymbol (&Qinteger, "integer");
  defsymbol (&Qpointer, "pointer");
  defsymbol (&Qscrollbar_pointer, "scrollbar-pointer");

  init_bitmap_tables_1 ();
  init_x_parm_symbols ();
}

static void
Xatoms_of_xfns ()
     /* Once we have multiple displays, this will need to be done differently,
	since atoms are per-display.  This info should live on some per-
	connection object.
      */
{
#define ATOM(x) XInternAtom(x_current_display, (x), False)

  BLOCK_INPUT;
  Xatom_WM_PROTOCOLS = ATOM("WM_PROTOCOLS");
  Xatom_WM_DELETE_WINDOW = ATOM("WM_DELETE_WINDOW");
  Xatom_WM_SAVE_YOURSELF = ATOM("WM_SAVE_YOURSELF");
  Xatom_WM_TAKE_FOCUS = ATOM("WM_TAKE_FOCUS");
  Xatom_WM_STATE = ATOM("WM_STATE");
  UNBLOCK_INPUT;

  /* This needs to be per-display once we handle multiple displays.
     It should be shared among screens on the same display. */
  the_gc_cache = make_gc_cache (x_current_display,
				RootWindow (x_current_display,
					    DefaultScreen(x_current_display)));
}

#endif /* HAVE_X_WINDOWS */
