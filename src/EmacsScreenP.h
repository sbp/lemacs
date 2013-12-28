#ifndef _EmacsScreenP_h
#define _EmacsScreenP_h

#include "xintrinsicp.h"
#include <X11/CoreP.h>
#ifdef LWLIB_USES_MOTIF
#include "xmprimitivep.h"
#endif
#include "EmacsScreen.h"

typedef struct {
  struct screen*	screen;		/* the *emacs* screen object */

  /* Resources that can't be done from lisp.
   */
  char*		geometry;		/* geometry spec of this screen */
  Boolean	iconic;			/* whether this screen is iconic */

  /* The rest of this is crap and should be deleted.
   */
  int		minibuffer;	/* 0: normal screens with minibuffers.
				 * 1: screens without minibuffers 
				 * 2: minibuffer only. */
  Boolean	unsplittable;	/* screen can only have one window */

  int		internal_border_width;	/* internal borders */
  int		scrollbar_width;	/* width of screen scrollbars */
  int		interline;		/* skips between lines */

#ifdef I18N4
  XFontSet	font;			/* font set */
#else
  XFontStruct*	font;			/* font */
#endif
  Pixel		foreground_pixel;	/* foreground */

  Pixel		cursor_color;		/* text cursor color */
  Boolean	bar_cursor;		/* 1 if bar, 0 if block */

  Boolean	visual_bell;		/* flash instead of beep */
  int		bell_volume;		/* how loud is beep */

  Boolean	menubar_p;		/* initially show a menubar? */
  Boolean	initially_unmapped;	/* inhibit initial window mapping */
  Boolean	use_backing_store;	/* backing store for menubar & ew? */

  Dimension     preferred_width;        /* if non-zero, preferred size for */
  Dimension     preferred_height;	/* QueryGeometry() */
  /* private state */

} EmacsScreenPart;

typedef struct _EmacsScreenRec {	/* full instance record */
    CorePart		core;
#ifdef LWLIB_USES_MOTIF
    XmPrimitivePart	primitive;
#endif
    EmacsScreenPart	emacs_screen;
} EmacsScreenRec;

typedef struct {			/* new fields for EmacsScreen class */
    int dummy;
} EmacsScreenClassPart;

typedef struct _EmacsScreenClassRec {	/* full class record declaration */
    CoreClassPart		core_class;
#ifdef LWLIB_USES_MOTIF
    XmPrimitiveClassPart	primitive_class;
#endif
    EmacsScreenClassPart	emacs_screen_class;
} EmacsScreenClassRec;

extern EmacsScreenClassRec emacsScreenClassRec;	 /* class pointer */



#endif /* _EmacsScreenP_h */
