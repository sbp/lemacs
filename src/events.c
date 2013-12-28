/* Events: printing them, converting them to and from characters.
   Copyright (C) 1991, 1992 Free Software Foundation, Inc.

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

#include "config.h"
#include "lisp.h"
#include "buffer.h"
#include "window.h"
#include "screen.h"
#include "events.h"
#include "indent.h"

extern Lisp_Object Qeventp;
Lisp_Object QKbackspace, QKtab, QKlinefeed, QKreturn, QKescape,
 QKspace, QKdelete, QKnosymbol;

void
character_to_event (c, event)
     unsigned int c;
     struct Lisp_Event *event;
{
  unsigned int m = 0;
  if (event->event_type == dead_event)
    error ("character-to-event called with a deallocated event!");

  if (c > 127) c -= 128, m  = MOD_META;
  if (c < ' ') c += '@', m |= MOD_CONTROL;
  if (m & MOD_CONTROL) {
    switch (c) {
    case 'I': c = QKtab;	m &= ~MOD_CONTROL; break;
    case 'J': c = QKlinefeed;	m &= ~MOD_CONTROL; break;
    case 'M': c = QKreturn;	m &= ~MOD_CONTROL; break;
    case '[': c = QKescape;	m &= ~MOD_CONTROL; break;
# if 0
    /* This is probably too controversial... */
    case 'H': c = QKbackspace;	m &= ~MOD_CONTROL; break;
# endif
    }
    if (c >= 'A' && c <= 'Z') c -= 'A'-'a';
  }
  else if (c == 127) c = QKdelete;
  else if (c == ' ') c = QKspace;
  
  event->event_type		= key_press_event;
  event->channel		= Qnil;
  event->timestamp		= 0;
  event->event.key.key		= c;
  event->event.key.modifiers	= m;
}


/* This variable controls what character name -> character code mapping
   we are using.  Window-system-specific code sets this to some symbol,
   and we use that symbol as the plist key to convert keysyms into 8-bit
   codes.  In this way one can have several character sets predefined and
   switch them by changing this.
 */
Lisp_Object Vcharacter_set_property;

int
event_to_character (event, lenient)	/* This is worthless and weak */
     struct Lisp_Event *event;
     int lenient;
{
  int c;
  if (event->event_type != key_press_event) {
    if (event->event_type == dead_event) abort ();
    return -1;
  }
  if (!lenient &&
      event->event.key.modifiers & (MOD_SUPER|MOD_HYPER|MOD_SYMBOL))
    return -1;
  if (FIXNUMP (event->event.key.key))	    c = event->event.key.key;
  else if (EQ (event->event.key.key, QKbackspace))	c = '\b';
  else if (EQ (event->event.key.key, QKtab))		c = '\t';
  else if (EQ (event->event.key.key, QKlinefeed))	c = '\n';
  else if (EQ (event->event.key.key, QKreturn))		c = '\r';
  else if (EQ (event->event.key.key, QKescape))		c = 27;
  else if (EQ (event->event.key.key, QKspace))		c = ' ';
  else if (EQ (event->event.key.key, QKdelete))		c = 127;

  else if (!SYMBOLP (event->event.key.key))
    abort ();
  else if (!NILP (Vcharacter_set_property))
    {
      /* Allow window-system-specific extensibility of the keysym->code mapping
       */
      Lisp_Object code = Fget (event->event.key.key, Vcharacter_set_property);
      if (!FIXNUMP (code))
	return -1;
      c = XINT (code);
    }
  else return -1;

  if (event->event.key.modifiers & MOD_CONTROL) {
    if (c >= 'a' && c <= 'z')
      c -= ('a' - 'A');
    else
      if (c >= 'A' && c <= 'Z' && !lenient) /* reject Control-Shift- keys */
	return -1;

    if (c >= '@' && c <= '_')
      c -= '@';
    else if (c == ' ')  /* C-space and C-@ are the same. */
      c = 0;
    else
      if (! lenient) return -1;
  }
  /* I'm not sure this is right, given the keysym stuff above.
     Once the tty interface is implemented, it might turn out to
     be wrong to interpret the high bit as meta.  Possibly the
     tty layer will do the meta hacking, and doing it here would
     mean we were doing it twice.
   */
  if (event->event.key.modifiers & MOD_META)
    c |= 0200;
  return c;
}

DEFUN ("event-to-character", Fevent_to_character, Sevent_to_character,
       1, 2, 0,
 "Returns the closest ASCII approximation to the given event object.\n\
If the event isn't a keypress, this returns nil.\n\
If the second argument is non-nil, then this is lenient in its \n\
translation; it will ignore modifier keys other than control and meta,\n\
and will ignore the shift modifier on those characters which have no \n\
shifted ASCII equivalent (Control-Shift-A for example, will be mapped to \n\
the same ASCII code as Control-A.)  If the second arg is nil, then nil \n\
will be returned for events which have no direct ASCII equivalent.")
     (event, lenient)
     Lisp_Object event, lenient;
{
  int c;
  CHECK_EVENT (event, 0);
  if (XEVENT (event)->event_type == dead_event)
    error ("event-to-character called with a deallocated event!");
  c = event_to_character (XEVENT (event), !NILP (lenient));
  return (c == -1 ? Qnil : make_number (c));
}


DEFUN ("character-to-event", Fcharacter_to_event, Scharacter_to_event, 1, 2, 0,
  "Converts a numeric ASCII value to an event structure, replete with\n\
bucky bits.  The character is the first argument, and the event to fill\n\
in is the second.  This function contains knowledge about what the codes\n\
``mean'' -- for example, the number 9 is converted to the character ``Tab'',\n\
not the distinct character ``Control-I''.\n\
\n\
If the optional second argument is an event, it is modified; otherwise, a\n\
new event object is created.\n\
\n\
Beware that character-to-event and event-to-character are not strictly \n\
inverse functions, since events contain much more information than the \n\
ASCII character set can encode.")
     (ch, event)
     Lisp_Object ch, event;
{
  CHECK_FIXNUM (ch, 0);
  if (NILP (event))
    event = Fallocate_event ();
  else
    CHECK_EVENT (event, 0);
  character_to_event (XINT (ch), XEVENT (event));
  return event;
}


#ifdef HAVE_X_WINDOWS
extern char* x_event_name ();
#endif

void
format_event_object (buf, event, brief)
     char *buf;
     struct Lisp_Event *event;
     int brief;
{
  int mod, mouse_p = 0;
  Lisp_Object key;
  switch (event->event_type) {
  case key_press_event:
    mod = event->event.key.modifiers;
    key = event->event.key.key;
    /* Hack. */
    if (! brief && FIXNUMP (key) &&
	mod & (MOD_CONTROL|MOD_META|MOD_SUPER|MOD_HYPER)) {
      if (XINT (key) >= 'a' && XINT (key) <= 'z')
	XFASTINT (key) -= 'a'-'A';
      else if (XINT (key) >= 'A' && XINT (key) <= 'Z')
	mod |= MOD_SHIFT;
    }
    break;
  case button_release_event:
    mouse_p++;
  case button_press_event:
    mouse_p++;
    mod = event->event.button.modifiers;
    key = make_number (event->event.button.button + '0');
    break;
  case magic_event:
    {
      char *name =
#ifdef HAVE_X_WINDOWS
	x_event_name (((XEvent *) &event->event.magic.underlying_event)
		      ->xany.type);
#else
        0;
#endif
      if (name) strcpy (buf, name);
      else strcpy (buf, "???");
      return;
    }
  case pointer_motion_event:	strcpy (buf, "motion");	return;
  case menu_event:		strcpy (buf, "menu"); 	return;
  case eval_event:		strcpy (buf, "eval"); 	return;
  case process_event:		strcpy (buf, "process");return;
  case timeout_event:		strcpy (buf, "timeout");return;
  case empty_event:		strcpy (buf, "EMPTY-EVENT"); return;
  case dead_event:		strcpy (buf, "DEAD-EVENT");  return;
  default:
    abort ();
  }
#define modprint1(x)  { strcpy(buf,(x)); buf+=sizeof(x)-1; }
#define modprint(x,y) { if (brief) modprint1(y) else modprint1(x) }
  if (mod & MOD_CONTROL) modprint ("control-", "C-");
  if (mod & MOD_META)    modprint ("meta-",    "M-");
  if (mod & MOD_SUPER)   modprint ("super-",   "S-");
  if (mod & MOD_HYPER)   modprint ("hyper-",   "H-");
  if (mod & MOD_SYMBOL)  modprint ("symbol-",  "Sym-");
  if (mod & MOD_SHIFT)   modprint ("shift-",   "Sh-");
  if (mouse_p) {
    modprint1 ("button");
    --mouse_p;
  }
#undef modprint
#undef modprint1

  switch (XTYPE (key)) {
  case Lisp_Int:
    buf[0] = XINT (key);
    buf[1] = 0;
    buf++;
    break;
  case Lisp_Symbol:
    {
      char *str = 0;
      if (brief) {
	if (EQ (key, QKlinefeed)) str = "LFD";
	else if (EQ (key, QKtab)) str = "TAB";
	else if (EQ (key, QKreturn)) str = "RET";
	else if (EQ (key, QKescape)) str = "ESC";
	else if (EQ (key, QKdelete)) str = "DEL";
	else if (EQ (key, QKspace)) str = "SPC";
	else if (EQ (key, QKbackspace)) str = "BS";
      }
      if (str) {
	int i = strlen (str);
	strncpy (buf, str, i+1);
	str += i;
      }
      else {
	strncpy (buf, (char *) XSYMBOL (key)->name->data,
		 XSYMBOL (key)->name->size+1);
	str += XSYMBOL (key)->name->size;
      }
      break;
    }
  default:
    abort ();
  }
  if (mouse_p) strncpy (buf, "up", 4);
}


/* 
 * some predicates and accessors
 */

Lisp_Object Qeventp, Qkey_press_event_p, Qbutton_event_p, Qmouse_event_p,
 Qprocess_event_p;

DEFUN ("eventp", Feventp, Seventp, 1, 1, 0,
       "True if the argument is an event object.")
     (obj)
     Lisp_Object obj;
{
  return ((EVENTP (obj)) ? Qt : Qnil);
}

#define EVENT_PRED(type) \
  return ((EVENTP (obj) && \
	   XEVENT (obj)->event_type == (type)) \
	  ? Qt : Qnil)

DEFUN ("key-press-event-p", Fkey_press_event_p, Skey_press_event_p, 1, 1, 0,
       "True if the argument is a key-press event object.")
     (obj)
{ EVENT_PRED (key_press_event); }

DEFUN ("button-press-event-p", Fbutton_press_event_p, Sbutton_press_event_p,
       1, 1, 0, "True if the argument is a mouse-button-press event object.")
     (obj)
{ EVENT_PRED (button_press_event); }

DEFUN ("button-release-event-p", Fbutton_release_event_p,
       Sbutton_release_event_p, 1, 1, 0,
       "True if the argument is a mouse-button-release event object.")
     (obj)
{ EVENT_PRED (button_release_event); }

DEFUN ("button-event-p", Fbutton_event_p,
       Sbutton_event_p, 1, 1, 0,
       "True if the argument is a button-press or button-release event object.")
     (obj)
{
  return ((EVENTP (obj) &&
	   (XEVENT (obj)->event_type == button_press_event ||
	    XEVENT (obj)->event_type == button_release_event))
	  ? Qt : Qnil);
}

DEFUN ("motion-event-p", Fmotion_event_p, Smotion_event_p, 1, 1, 0,
       "True if the argument is a mouse-motion event object.")
     (obj)
{ EVENT_PRED (pointer_motion_event); }

DEFUN ("process-event-p", Fprocess_event_p, Sprocess_event_p, 1, 1, 0,
       "True if the argument is a process-output event object.")
     (obj)
{ EVENT_PRED (process_event); }

DEFUN ("timeout-event-p", Ftimeout_event_p, Stimeout_event_p, 1, 1, 0,
       "True if the argument is a timeout event object.")
     (obj)
{ EVENT_PRED (timeout_event); }

DEFUN ("menu-event-p", Fmenu_event_p, Smenu_event_p, 1, 1, 0,
       "True if the argument is a menu event object.")
     (obj)
{ EVENT_PRED (menu_event); }

DEFUN ("eval-event-p", Feval_event_p, Seval_event_p, 1, 1, 0,
       "True if the argument is an `eval' or `menu' event object.")
     (obj)
{
  return ((EVENTP (obj) &&
	   (XEVENT (obj)->event_type == menu_event ||
	    XEVENT (obj)->event_type == eval_event))
	  ? Qt : Qnil);
}

#define CHECK_EVENT_SAFE(e) \
{ CHECK_EVENT ((e),0); \
  if ((XEVENT (e)->event_type < first_event_type) \
      || (XEVENT (e)->event_type > last_event_type)) \
     abort (); \
  if (XEVENT (e)->event_type == dead_event) error ("deallocated event"); \
}

DEFUN ("event-timestamp", Fevent_timestamp, Sevent_timestamp, 1, 1, 0,
  "Returns the timestamp of the given event object.")
     (event)
{
  CHECK_EVENT_SAFE (event);
  /* This junk is so that timestamps don't get to be negative, but contain
     as many bits as this particular emacs will allow.
   */
  return make_number (((1 << (VALBITS - 1)) - 1) &
		      XEVENT (event)->timestamp);
}

#define CHECK_EVENT_TYPE(e,t1,sym) \
{ CHECK_EVENT_SAFE (e); \
  if (XEVENT(e)->event_type != (t1)) \
     e = wrong_type_argument ((sym),(e)); \
}

#define CHECK_EVENT_TYPE2(e,t1,t2,sym) \
{ CHECK_EVENT_SAFE (e); \
  if (XEVENT(e)->event_type != (t1) && XEVENT(e)->event_type != (t2)) \
     e = wrong_type_argument ((sym),(e));\
}

DEFUN ("event-key", Fevent_key, Sevent_key, 1, 1, 0,
       "Returns the KeySym of the given key-press event.  This will be the\n\
ASCII code of a printing character, or a symbol.")
     (event)
{
  CHECK_EVENT_TYPE (event, key_press_event, Qkey_press_event_p);
  return XEVENT (event)->event.key.key;
}

DEFUN ("event-button", Fevent_button, Sevent_button, 1, 1, 0,
       "Returns the button-number of the given mouse-button-press event.")
     (event)
{
  CHECK_EVENT_TYPE2 (event, button_press_event, button_release_event,
		     Qbutton_event_p);
  return make_number (XEVENT (event)->event.button.button);
}

DEFUN ("event-modifier-bits", Fevent_modifier_bits, Sevent_modifier_bits,
       1, 1, 0,
       "Returns a number representing the modifier keys which were down \n\
when the given mouse or keyboard event was produced.  See also the function\n\
event-modifiers.")
     (event)
{
  Lisp_Object result;
  CHECK_EVENT_SAFE (event);
  if (XEVENT (event)->event_type != key_press_event &&
      XEVENT (event)->event_type != button_press_event &&
      XEVENT (event)->event_type != button_release_event)
    wrong_type_argument (intern ("key-or-mouse-event-p"), event);
  return make_number((XEVENT (event)->event_type != key_press_event)
		     ? XEVENT (event)->event.key.modifiers
		     : XEVENT (event)->event.button.modifiers);
}

DEFUN ("event-modifiers", Fevent_modifiers, Sevent_modifiers, 1, 1, 0,
       "Returns a list of symbols, the names of the modifier keys which \n\
were down when the given mouse or keyboard event was produced.\n\
See also the function event-modifier-bits.")
     (event)
{
  int mod = XINT (Fevent_modifier_bits (event));
  Lisp_Object result = Qnil;
  if (mod & MOD_SHIFT)   result = Fcons (intern ("shift"), result);
  if (mod & MOD_SYMBOL)  result = Fcons (intern ("symbol"), result);
  if (mod & MOD_HYPER)   result = Fcons (intern ("hyper"), result);
  if (mod & MOD_SUPER)   result = Fcons (intern ("super"), result);
  if (mod & MOD_META)    result = Fcons (intern ("meta"), result);
  if (mod & MOD_CONTROL) result = Fcons (intern ("control"), result);
  return result;
}

DEFUN ("event-x-pixel", Fevent_x_pixel, Sevent_x_pixel, 1, 1, 0,
 "Returns the X position of the given mouse-motion, button-press, or\n\
button-release event in pixels.")
     (event)
{
  CHECK_EVENT_SAFE (event);
  if (XEVENT (event)->event_type == pointer_motion_event)
    return make_number (XEVENT (event)->event.motion.x);
  else if (XEVENT (event)->event_type == button_press_event ||
	   XEVENT (event)->event_type == button_release_event)
    return make_number (XEVENT (event)->event.button.x);
  else
    wrong_type_argument (Qmouse_event_p, event);
}

DEFUN ("event-y-pixel", Fevent_y_pixel, Sevent_y_pixel, 1, 1, 0,
 "Returns the Y position of the given mouse-motion, button-press, or\n\
button-release event in pixels.")
     (event)
{
  CHECK_EVENT_SAFE (event);
  if (XEVENT (event)->event_type == pointer_motion_event)
    return make_number (XEVENT (event)->event.motion.y);
  else if (XEVENT (event)->event_type == button_press_event ||
	   XEVENT (event)->event_type == button_release_event)
    return make_number (XEVENT (event)->event.button.y);
  wrong_type_argument (Qmouse_event_p, event);
}


void
event_pixel_translation (event, char_x, char_y, w, bufp, class, the_hard_way)
     Lisp_Object event, *class;
     int *char_x, *char_y, *bufp;
     struct window **w;
     int the_hard_way;
{
  int pix_x, pix_y, begin_p;
  int glyph, res;
  Lisp_Object window, screen;
  
  CHECK_EVENT_SAFE (event);
  if (XEVENT (event)->event_type == pointer_motion_event) {
    pix_x  = XEVENT (event)->event.motion.x;
    pix_y  = XEVENT (event)->event.motion.y;
    screen = XEVENT (event)->channel;
  }
  else if (XEVENT (event)->event_type == button_press_event ||
	   XEVENT (event)->event_type == button_release_event) {
    pix_x  = XEVENT (event)->event.button.x;
    pix_y  = XEVENT (event)->event.button.y;
    screen = XEVENT (event)->channel;
  }
  else
    wrong_type_argument (Qmouse_event_p, event);

  res = pixel_to_glyph_translation (XSCREEN (screen), pix_x, pix_y,
				    char_x, char_y, w, bufp,
				    &glyph, class, &begin_p);
  /* It looks to me like the result value of pixel_to_glyph_translation() is
     0:  modeline
     1:  over text, or over a glyph in the lineinfo column;
     2:  not over text, not in a window, or over an inactive minibuffer.
   */
  if (res == 2)
    *bufp = 0;
  else if (*w && NILP ((*w)->buffer))
    *w = 0; /* Why does this happen? */

#if 0

  if (the_hard_way && *bufp)
    /* pixel_to_glyph_translation() doesn't really work when what you're
       interested in is the buffer position of a pixel position, though
       it works for the other values.  So if we want the point, we use
       compute_motion() to get the buffer position, because doing this is
       loads easier than actually fixing the redisplay data structures to
       be valid.  What a crock.
     */
    {
      struct position *posval;
      XSETWINDOW (window, *w);
      posval = compute_motion (window, marker_position ((*w)->start), 0, 0,
			       ZV, *char_y, *char_x,
			       XFASTINT ((*w)->width), XINT ((*w)->hscroll),
			       0, 0, 0);
      *bufp = posval->bufpos;
    }
#endif
}


DEFUN ("event-window", Fevent_window, Sevent_window, 1, 1, 0,
 "Given a mouse motion, button press, or button release event, compute\n\
and return the window on which that event occurred.  This may be nil if\n\
the event did not occur in an emacs window (in the border or modeline.)")
     (event)
{
  int char_x, char_y, bufp;
  struct window *w;
  Lisp_Object window, class;
  event_pixel_translation (event, &char_x, &char_y, &w, &bufp, &class, 0);
  if (! w) return Qnil;
  XSET (window, Lisp_Window, w);
  return window;
}


DEFUN ("event-point", Fevent_point, Sevent_point, 1, 1, 0,
 "Returns the character position of the given mouse-motion, button-press,\n\
or button-release event.  If the event did not occur over a window, or did\n\
not occur over text, then this returns nil.  Otherwise, it returns an index\n\
into the buffer visible in the event's window.")
     (event)
{
  int char_x, char_y, bufp, class;
  struct window *w;
  event_pixel_translation (event, &char_x, &char_y, &w, &bufp, &class, 1);
  if (! w) return Qnil;
  if (! bufp) return Qnil;
  return make_number (bufp);
}

DEFUN ("event-x", Fevent_x, Sevent_x, 1, 1, 0,
 "Returns the X position of the given mouse-motion, button-press, or\n\
button-release event in characters.")
     (event)
{
  int char_x, char_y, bufp, class;
  struct window *w;
  event_pixel_translation (event, &char_x, &char_y, &w, &bufp, &class, 0);
  if (! w) return Qnil;
  return make_number (char_x);
}

DEFUN ("event-y", Fevent_y, Sevent_y, 1, 1, 0,
 "Returns the Y position of the given mouse-motion, button-press, or\n\
button-release event in characters.")
     (event)
{
  int char_x, char_y, bufp, class;
  struct window *w;
  event_pixel_translation (event, &char_x, &char_y, &w, &bufp, &class, 0);
  if (! w) return Qnil;
  return make_number (char_y);
}


DEFUN ("event-glyph", Fevent_glyph, Sevent_glyph, 1, 1, 0,
 "If the given mouse-motion, button-press, or button-release event happened\n\
on top of a glyph, this returns it; else nil.")
     (event)
{
  int char_x, char_y, bufp;
  Lisp_Object class;
  struct window *w;

  event_pixel_translation (event, &char_x, &char_y, &w, &bufp, &class, 0);
  if (! w) return Qnil;
  return class;
}


DEFUN ("event-process", Fevent_process, Sevent_process, 1, 1, 0,
 "Returns the process of the given proces-output event.")
     (event)
{
  CHECK_EVENT_TYPE (event, process_event, Qprocess_event_p);
  return (XEVENT (event)->event.process.process);
}

DEFUN ("event-function", Fevent_function, Sevent_function, 1, 1, 0,
 "Returns the callback function of the given timeout, menu, or eval event.")
     (event)
{
  CHECK_EVENT_SAFE (event);
  switch (XEVENT (event)->event_type) {
  case timeout_event:
    return (XEVENT (event)->event.timeout.function);
  case menu_event:
  case eval_event:
    return (XEVENT (event)->event.eval.function);
  default:
    wrong_type_argument (intern ("timeout-or-eval-event-p"), event);
  }
}

DEFUN ("event-object", Fevent_object, Sevent_object, 1, 1, 0,
 "Returns the callback function argument of the given timeout, menu, or\n\
eval event.")
     (event)
{
  CHECK_EVENT_SAFE (event);
  switch (XEVENT (event)->event_type) {
  case timeout_event:
    return (XEVENT (event)->event.timeout.object);
  case menu_event:
  case eval_event:
    return (XEVENT (event)->event.eval.object);
  default:
    wrong_type_argument (intern ("timeout-or-eval-event-p"), event);
  }
}

Lisp_Object
event_equal (o1, o2)			/* only Fequal() uses this */
     Lisp_Object o1, o2;
{
  int size;
  if (XEVENT (o1)->event_type != XEVENT (o2)->event_type) return Qnil;
  if (XEVENT (o1)->channel != XEVENT (o2)->channel) return Qnil;
/*  if (XEVENT (o1)->timestamp != XEVENT (o2)->timestamp) return Qnil; */
  switch (XEVENT (o1)->event_type) {
    
  case process_event:
    return (EQ (XEVENT (o1)->event.process.process,
		XEVENT (o2)->event.process.process)
	    ? Qt : Qnil);
    
  case timeout_event:
    if (NILP (Fequal (XEVENT (o1)->event.timeout.function,
		      XEVENT (o2)->event.timeout.function)))
      return Qnil;
    if (NILP (Fequal (XEVENT (o1)->event.timeout.object,
		      XEVENT (o2)->event.timeout.object)))
      return Qnil;
    return Qt;
    
  case key_press_event:
    return ((XEVENT (o1)->event.key.key == XEVENT (o2)->event.key.key &&
	     XEVENT (o1)->event.key.modifiers ==
	     XEVENT (o2)->event.key.modifiers)
	    ? Qt : Qnil);
  case button_press_event:
  case button_release_event:
    return ((XEVENT (o1)->event.button.button ==
	     XEVENT (o2)->event.button.button &&
	     XEVENT (o1)->event.button.modifiers ==
	     XEVENT (o2)->event.button.modifiers)
	    ? Qt : Qnil);
  case pointer_motion_event:
    return ((XEVENT (o1)->event.motion.x == XEVENT (o2)->event.motion.x &&
	     XEVENT (o1)->event.motion.y == XEVENT (o2)->event.motion.y)
	    ? Qt : Qnil);
  case menu_event:
  case eval_event:
    if (NILP (Fequal (XEVENT (o1)->event.eval.function,
		      XEVENT (o2)->event.eval.function)))
      return Qnil;
    if (NILP (Fequal (XEVENT (o1)->event.eval.object,
		      XEVENT (o2)->event.eval.object)))
      return Qnil;
    return Qt;
  case magic_event:
    return (bcmp ((char*) &(XEVENT (o1)->event.magic),
		  (char*) &(XEVENT (o2)->event.magic),
		  sizeof (struct magic_data))
	    ? Qnil : Qt);

  default:
    error ("unknown event type");
  }
}


void
syms_of_events ()
{
  DEFVAR_LISP ("character-set-property", &Vcharacter_set_property,
   "A symbol used to look up the 8-bit character of a keysym.\n\
To convert a keysym symbol to an 8-bit code, as when that key is\n\
bound to self-insert-command, we will look up the property that this\n\
variable names on the property list of the keysym-symbol.  The window-\n\
system-specific code will set up appropriate properties and set this\n\
variable.");
  Vcharacter_set_property = Qnil;

  defsubr (&Scharacter_to_event);
  defsubr (&Sevent_to_character);

  defsubr (&Seventp);
  defsubr (&Skey_press_event_p);
  defsubr (&Sbutton_press_event_p);
  defsubr (&Sbutton_release_event_p);
  defsubr (&Smotion_event_p);
  defsubr (&Sprocess_event_p);
  defsubr (&Stimeout_event_p);
  defsubr (&Smenu_event_p);
  defsubr (&Seval_event_p);

  defsubr (&Sevent_timestamp);
  defsubr (&Sevent_key);
  defsubr (&Sevent_button);
  defsubr (&Sevent_modifier_bits);
  defsubr (&Sevent_modifiers);
  defsubr (&Sevent_x_pixel);
  defsubr (&Sevent_y_pixel);
  defsubr (&Sevent_window);
  defsubr (&Sevent_x);
  defsubr (&Sevent_y);
  defsubr (&Sevent_point);
  defsubr (&Sevent_glyph);
  defsubr (&Sevent_process);
  defsubr (&Sevent_function);
  defsubr (&Sevent_object);

  Qeventp	     = intern ("eventp");
  Qkey_press_event_p = intern ("key-press-event-p");
  Qbutton_event_p    = intern ("button-event-p");
  Qmouse_event_p     = intern ("mouse-event-p");
  Qprocess_event_p   = intern ("process-event-p");

  QKbackspace = KEYSYM ("backspace");
  QKtab       = KEYSYM ("tab");
  QKlinefeed  = KEYSYM ("linefeed");
  QKreturn    = KEYSYM ("return");
  QKescape    = KEYSYM ("escape");
  QKspace     = KEYSYM ("space");
  QKdelete    = KEYSYM ("delete");
  QKnosymbol  = KEYSYM ("NoSymbol");
}
