#ifndef _EmacsScreenP_h
#define _EmacsScreenP_h

#include <X11/IntrinsicP.h>
#include <X11/CoreP.h>
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
  int		interline;		/* skips between lines */

  XFontStruct*	font;			/* font */
  Pixel		foreground_pixel;	/* foreground */

  Pixel		cursor_color;		/* text cursor color */
  Boolean	bar_cursor;		/* 1 if bar, 0 if block */

  Boolean	visual_bell;		/* flash instead of beep */
  int		bell_volume;		/* how loud is beep */

  /* private state */

} EmacsScreenPart;

typedef struct _EmacsScreenRec {	/* full instance record */
    CorePart		core;
    EmacsScreenPart	emacs_screen;
} EmacsScreenRec;

typedef struct {			/* new fields for EmacsScreen class */
    int dummy;
} EmacsScreenClassPart;

typedef struct _EmacsScreenClassRec {	/* full class record declaration */
    CoreClassPart		core_class;
    EmacsScreenClassPart	emacs_screen_class;
} EmacsScreenClassRec;

extern EmacsScreenClassRec emacsScreenClassRec;	 /* class pointer */



#endif /* _EmacsScreenP_h */
