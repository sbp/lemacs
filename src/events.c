/* Events: printing them, converting them to and from characters.
   Copyright (C) 1991, 1992, 1993, 1994 Free Software Foundation, Inc.

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

#include "extents.h"	/* Just for the EXTENTP abort check... */

#include <stdio.h>              /* for sprintf */

Lisp_Object QKbackspace, QKtab, QKlinefeed, QKreturn, QKescape,
 QKspace, QKdelete, QKnosymbol;

int
command_event_p (struct Lisp_Event *event)
{
  switch (event->event_type)
  {
  case key_press_event:
  case button_press_event:
  case button_release_event:
  case menu_event:
    return (1);
  default:
    return (0);
  }
}


void
character_to_event (unsigned int c, struct Lisp_Event *event)
{
  Lisp_Object k = Qnil;
  unsigned int m = 0;
  if (event->event_type == dead_event)
    error ("character-to-event called with a deallocated event!");

  c &= 255;
  if (c > 127) c -= 128, m  = MOD_META;
  if (c < ' ') c += '@', m |= MOD_CONTROL;
  if (m & MOD_CONTROL)
    {
      switch (c)
	{
	case 'I': k = QKtab;	  m &= ~MOD_CONTROL; break;
	case 'J': k = QKlinefeed; m &= ~MOD_CONTROL; break;
	case 'M': k = QKreturn;	  m &= ~MOD_CONTROL; break;
	case '[': k = QKescape;	  m &= ~MOD_CONTROL; break;
# if 0
	  /* This is probably too controversial... */
	case 'H': k = QKbackspace; m &= ~MOD_CONTROL; break;
# endif
	}
      if (c >= 'A' && c <= 'Z') c -= 'A'-'a';
    }
  else if (c == 127) k = QKdelete;
  else if (c == ' ') k = QKspace;
  
  event->event_type		= key_press_event;
  event->channel		= Qnil;
  event->timestamp		= 0;
  event->event.key.keysym	= (!NILP (k) ? k : make_number (c));
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
event_to_character (struct Lisp_Event *event,
		    int allow_extra_modifiers,
		    int allow_meta,
		    int allow_non_ascii)
{
  int c;
  if (event->event_type != key_press_event)
    {
      if (event->event_type == dead_event) abort ();
      return -1;
    }
  if (!allow_extra_modifiers &&
      event->event.key.modifiers & (MOD_SUPER|MOD_HYPER|MOD_SYMBOL))
    return -1;
  if (FIXNUMP (event->event.key.keysym))   c = XINT (event->event.key.keysym);
  else if (EQ (event->event.key.keysym, QKbackspace))	c = '\b';
  else if (EQ (event->event.key.keysym, QKtab))		c = '\t';
  else if (EQ (event->event.key.keysym, QKlinefeed))	c = '\n';
  else if (EQ (event->event.key.keysym, QKreturn))	c = '\r';
  else if (EQ (event->event.key.keysym, QKescape))	c = 27;
  else if (EQ (event->event.key.keysym, QKspace))	c = ' ';
  else if (EQ (event->event.key.keysym, QKdelete))	c = 127;

  else if (!SYMBOLP (event->event.key.keysym))
    abort ();
  else if (allow_non_ascii && !NILP (Vcharacter_set_property))
    {
      /* Allow window-system-specific extensibility of keysym->code mapping */
      Lisp_Object code = Fget (event->event.key.keysym,
                               Vcharacter_set_property,
			       Qnil);
      if (!FIXNUMP (code))
	return -1;
      c = XINT (code);
    }
  else
    return -1;

  if (event->event.key.modifiers & MOD_CONTROL)
    {
      if (c >= 'a' && c <= 'z')
	c -= ('a' - 'A');
      else
	/* reject Control-Shift- keys */
	if (c >= 'A' && c <= 'Z' && !allow_extra_modifiers)
	  return -1;
      
      if (c >= '@' && c <= '_')
	c -= '@';
      else if (c == ' ')  /* C-space and C-@ are the same. */
	c = 0;
      else
	/* reject keys that can't take Control- modifiers */
	if (! allow_extra_modifiers) return -1;
    }

  if (event->event.key.modifiers & MOD_META)
    {
      if (! allow_meta) return -1;
      if (c & 0200) return -1;		/* don't allow M-oslash (overlap) */
      c |= 0200;
    }
  return c;
}


DEFUN ("event-to-character", Fevent_to_character, Sevent_to_character,
       1, 4, 0,
 "Returns the closest ASCII approximation to the given event object.\n\
If the event isn't a keypress, this returns nil.\n\
If the ALLOW-EXTRA-MODIFIERS argument is non-nil, then this is lenient in\n\
 its translation; it will ignore modifier keys other than control and meta,\n\
 and will ignore the shift modifier on those characters which have no\n\
 shifted ASCII equivalent (Control-Shift-A for example, will be mapped to\n\
 the same ASCII code as Control-A.)\n\
If the ALLOW-META argument is non-nil, then the Meta modifier will be\n\
 represented by turning on the high bit of the byte returned; otherwise, nil\n\
 will be returned for events containing the Meta modifier.\n\
If the ALLOW-NON-ASCII argument is non-nil, then characters which are\n\
 present in the prevailing character set (see the `character-set-property'\n\
 variable) will be returned as their code in that character set, instead of\n\
 the return value being restricted to ASCII.\n\
Note that specifying both ALLOW-META and ALLOW-NON-ASCII is ambiguous, as\n\
 both use the high bit; `M-x' and `oslash' will be indistinguishable.")
     (event, allow_extra_modifiers, allow_meta, allow_non_ascii)
     Lisp_Object event, allow_extra_modifiers, allow_meta, allow_non_ascii;
{
  int c;
  CHECK_EVENT (event, 0);
  if (XEVENT (event)->event_type == dead_event)
    error ("event-to-character called with a deallocated event!");
  c = event_to_character (XEVENT (event),
			  !NILP (allow_extra_modifiers),
			  !NILP (allow_meta),
			  !NILP (allow_non_ascii));
  return (c < 0 ? Qnil : make_number (c));
}


extern void key_desc_list_to_event (); /* from keymap.c */

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
  if (NILP (event))
    event = Fallocate_event ();
  else
    CHECK_EVENT (event, 0);
  if (FIXNUMP (ch))
    character_to_event (XINT (ch), XEVENT (event));
  else if (CONSP (ch) || SYMBOLP (ch))
    key_desc_list_to_event (ch, event, 1);
  else
    CHECK_FIXNUM (ch, 0);
  return event;
}


#ifdef HAVE_X_WINDOWS
extern char* x_event_name ();
#endif

void
format_event_object (char *buf, struct Lisp_Event *event, int brief)
{
  int mouse_p = 0;
  int mod;
  Lisp_Object key;

  switch (event->event_type)
    {
    case key_press_event:
      {
        mod = event->event.key.modifiers;
        key = event->event.key.keysym;
        /* Hack. */
        if (! brief && FIXNUMP (key) &&
            mod & (MOD_CONTROL|MOD_META|MOD_SUPER|MOD_HYPER))
	{
	  int k = XINT (key);
	  if (k >= 'a' && k <= 'z')
	    key = make_number (k - ('a'-'A'));
	  else if (k >= 'A' && k <= 'Z')
	    mod |= MOD_SHIFT;
	}
        break;
      }
    case button_release_event:
      mouse_p++;
      /* Fall through */
    case button_press_event:
      {
        mouse_p++;
        mod = event->event.button.modifiers;
        key = make_number (event->event.button.button + '0');
        break;
      }
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

  if (FIXNUMP (key))
    {
      buf[0] = XINT (key);
      buf[1] = 0;
      buf++;
    }
  else if (SYMBOLP (key))
    {
      CONST char *str = 0;
      if (brief)
	{
	  if (EQ (key, QKlinefeed)) str = "LFD";
	  else if (EQ (key, QKtab)) str = "TAB";
	  else if (EQ (key, QKreturn)) str = "RET";
	  else if (EQ (key, QKescape)) str = "ESC";
	  else if (EQ (key, QKdelete)) str = "DEL";
	  else if (EQ (key, QKspace)) str = "SPC";
	  else if (EQ (key, QKbackspace)) str = "BS";
	}
      if (str)
	{
	  int i = strlen (str);
	  memcpy (buf, str, i+1);
	  str += i;
	}
      else
	{
	  memcpy (buf, XSYMBOL (key)->name->data,
                XSYMBOL (key)->name->size + 1);
	  str += XSYMBOL (key)->name->size;
	}
    }
  else
    abort ();
  if (mouse_p)
    strncpy (buf, "up", 4);
}


static void
print_event_1 (CONST char *str, Lisp_Object obj, Lisp_Object printcharfun)
{
  char buf[255];
  write_string_1 (str, -1, printcharfun);
  format_event_object (buf, XEVENT (obj), 0);
  write_string_1 (buf, -1, printcharfun);
}

void
print_event (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  switch (XEVENT (obj)->event_type) 
    {
    case key_press_event:
      print_event_1 ("#<keypress-event ", obj, printcharfun);
      break;
    case button_press_event:
      print_event_1 ("#<buttondown-event ", obj, printcharfun);
      break;
    case button_release_event:
      print_event_1 ("#<buttonup-event ", obj, printcharfun);
      break;
    case magic_event:
      print_event_1 ("#<magic-event ", obj, printcharfun);
      break;
    case pointer_motion_event:
      {
	char buf[100];
	sprintf (buf, "#<motion-event %d, %d",
		 XEVENT (obj)->event.motion.x, XEVENT (obj)->event.motion.y);
	write_string_1 (buf, -1, printcharfun);
	break;
      }
    case process_event:
      {
	write_string_1 ("#<process-event ", -1, printcharfun);
	print_internal (XEVENT (obj)->event.process.process, printcharfun, 1);
	break;
      }
    case timeout_event:
      {
	write_string_1 ("#<timeout-event ", -1, printcharfun);
	print_internal (XEVENT (obj)->event.timeout.object, printcharfun, 1);
	break;
      }
    case empty_event:
      {
	write_string_1 ("#<empty-event", -1, printcharfun);
	break;
      }
    case menu_event:
    case eval_event:
      {
	write_string_1 ("#<", -1, printcharfun);
	if (XEVENT (obj)->event_type == menu_event)
	  write_string_1 ("menu", -1, printcharfun);
	else
	  write_string_1 ("eval", -1, printcharfun);
	write_string_1 ("-event (", -1, printcharfun);
	print_internal (XEVENT (obj)->event.eval.function, printcharfun, 1);
	write_string_1 (" ", -1, printcharfun);
	print_internal (XEVENT (obj)->event.eval.object, printcharfun, 1);
	write_string_1 (")", -1, printcharfun);
	break;
      }
    case dead_event:
      {
	write_string_1 ("#<DEALLOCATED-EVENT", -1, printcharfun);
	break;
      }
    default:
      {
	write_string_1 ("#<UNKNOWN-EVENT-TYPE", -1, printcharfun);
	break;
      }
    }
  write_string_1 (">", -1, printcharfun);
}

/* 
 * some predicates and accessors
 */

Lisp_Object Qeventp;
Lisp_Object Qkey_press_event_p;
Lisp_Object Qbutton_event_p;
Lisp_Object Qmouse_event_p;
Lisp_Object Qprocess_event_p;

#define CHECK_EVENT_SAFE(e, i) \
{ CHECK_EVENT ((e),(i)); \
  if ((XEVENT (e)->event_type > last_event_type) \
      || (XEVENT (e)->event_type == dead_event)) \
    dead_event_error ((e)); \
}

static void
dead_event_error (Lisp_Object event)
{
  struct Lisp_Event *e = XEVENT (event);
  if (e->event_type > last_event_type)
    abort ();
  /* >>> better error??? */
  signal_error (Qerror, (list2 (build_string ("deallocated event"),
                                event)));
}


DEFUN ("eventp", Feventp, Seventp, 1, 1, 0,
       "True if the argument is an event object.")
     (obj)
     Lisp_Object obj;
{
  return ((EVENTP (obj)) ? Qt : Qnil);
}

/* DEFUN ("event-next", Fevent_next, Sevent_next, 1, 1, 0,
 *   "Returns the event object's `next' event, or nil if it has none.\n\
 * The `next-event' field is changed by calling `set-next-event'.")
 *      (event)
 *      Lisp_Object event;
 * {
 *   struct Lisp_Event *e;
 *   CHECK_EVENT_SAFE (event, 0);
 * 
 *   e = event_next (XEVENT (event));
 *   if (!e)
 *     return Qnil;
 *   XSETR (event, Lisp_Event, e);
 *   return (event);
 * }
 * 
 * DEFUN ("set-event-next", Fset_event_next, Sset_event_next, 2, 2, 0,
 *   "Set the `next event' of EVENT to NEXT-EVENT.\n\
 * NEXT-EVENT must be an event object or nil.")
 *      (event, next_event)
 *      Lisp_Object event, next_event;
 * {
 *   struct Lisp_Event *e;
 * 
 *   CHECK_EVENT_SAFE (event, 0);
 *   if (NILP (next_event))
 *     {
 *       event_next (XEVENT (event)) = 0;
 *       return (Qnil);
 *     }
 * 
 *   CHECK_EVENT_SAFE (next_event, 1);
 *   for (e = XEVENT (next_event); e; e = event_next (e))
 *     {
 *       QUIT;
 *       if (e == XEVENT (event))
 * 	signal_error (Qerror, 
 * 		      list3 (build_string ("Cyclic event-next"),
 * 			     event, 
 * 			     next_event));
 *     }
 *   event_next (XEVENT (event)) = XEVENT (next_event);
 *   return (next_event);
 * }
 */


#define EVENT_PRED(type) \
  return ((EVENTP (obj) && XEVENT (obj)->event_type == (type)) \
	  ? Qt : Qnil)

DEFUN ("key-press-event-p", Fkey_press_event_p, Skey_press_event_p, 1, 1, 0,
       "True if the argument is a key-press event object.")
     (obj)
  Lisp_Object obj;
{ EVENT_PRED (key_press_event); }

DEFUN ("button-press-event-p", Fbutton_press_event_p, Sbutton_press_event_p,
       1, 1, 0, "True if the argument is a mouse-button-press event object.")
     (obj)
  Lisp_Object obj;
{ EVENT_PRED (button_press_event); }

DEFUN ("button-release-event-p", Fbutton_release_event_p,
       Sbutton_release_event_p, 1, 1, 0,
       "True if the argument is a mouse-button-release event object.")
     (obj)
  Lisp_Object obj;
{ EVENT_PRED (button_release_event); }

DEFUN ("button-event-p", Fbutton_event_p,
       Sbutton_event_p, 1, 1, 0,
       "True if the argument is a button-press or button-release event object.")
     (obj)
  Lisp_Object obj;
{
  return ((EVENTP (obj)
	   && (XEVENT (obj)->event_type == button_press_event ||
	       XEVENT (obj)->event_type == button_release_event))
	  ? Qt : Qnil);
}

DEFUN ("motion-event-p", Fmotion_event_p, Smotion_event_p, 1, 1, 0,
       "True if the argument is a mouse-motion event object.")
     (obj)
     Lisp_Object obj;
{ EVENT_PRED (pointer_motion_event); }

DEFUN ("process-event-p", Fprocess_event_p, Sprocess_event_p, 1, 1, 0,
       "True if the argument is a process-output event object.")
     (obj)
     Lisp_Object obj;
{ EVENT_PRED (process_event); }

DEFUN ("timeout-event-p", Ftimeout_event_p, Stimeout_event_p, 1, 1, 0,
       "True if the argument is a timeout event object.")
     (obj)
     Lisp_Object obj;
{ EVENT_PRED (timeout_event); }

DEFUN ("menu-event-p", Fmenu_event_p, Smenu_event_p, 1, 1, 0,
       "True if the argument is a menu event object.")
     (obj)
     Lisp_Object obj;
{ EVENT_PRED (menu_event); }

DEFUN ("eval-event-p", Feval_event_p, Seval_event_p, 1, 1, 0,
       "True if the argument is an `eval' or `menu' event object.")
     (obj)
     Lisp_Object obj;
{
  return ((EVENTP (obj) &&
	   (XEVENT (obj)->event_type == menu_event ||
	    XEVENT (obj)->event_type == eval_event))
	  ? Qt : Qnil);
}

DEFUN ("event-timestamp", Fevent_timestamp, Sevent_timestamp, 1, 1, 0,
  "Returns the timestamp of the given event object.")
     (event)
  Lisp_Object event;
{
  CHECK_EVENT_SAFE (event, 0);
  /* This junk is so that timestamps don't get to be negative, but contain
     as many bits as this particular emacs will allow.
   */
  return make_number (((1L << (VALBITS - 1)) - 1) &
		      XEVENT (event)->timestamp);
}

#define CHECK_EVENT_TYPE(e,t1,sym) \
{ CHECK_EVENT_SAFE (e, 0); \
  if (XEVENT(e)->event_type != (t1)) \
     e = wrong_type_argument ((sym),(e)); \
}

#define CHECK_EVENT_TYPE2(e,t1,t2,sym) \
{ CHECK_EVENT_SAFE (e, 0); \
  if (XEVENT(e)->event_type != (t1) && XEVENT(e)->event_type != (t2)) \
     e = wrong_type_argument ((sym),(e));\
}

DEFUN ("event-key", Fevent_key, Sevent_key, 1, 1, 0,
       "Returns the KeySym of the given key-press event.  This will be the\n\
ASCII code of a printing character, or a symbol.")
     (event)
  Lisp_Object event;
{
  CHECK_EVENT_TYPE (event, key_press_event, Qkey_press_event_p);
  return (XEVENT (event)->event.key.keysym);
}

DEFUN ("event-button", Fevent_button, Sevent_button, 1, 1, 0,
       "Returns the button-number of the given mouse-button-press event.")
     (event)
  Lisp_Object event;
{
  CHECK_EVENT_TYPE2 (event, button_press_event, button_release_event,
		     Qbutton_event_p);
  return make_number (XEVENT (event)->event.button.button);
}

DEFUN ("event-modifier-bits", Fevent_modifier_bits, Sevent_modifier_bits,
       1, 1, 0,
       "Returns a number representing the modifier keys which were down\n\
when the given mouse or keyboard event was produced.  See also the function\n\
event-modifiers.")
     (event)
  Lisp_Object event;
{
 again:
  CHECK_EVENT_SAFE (event, 0);
  if (XEVENT (event)->event_type == key_press_event)
    return make_number (XEVENT (event)->event.key.modifiers);
  else if (XEVENT (event)->event_type == button_press_event ||
	   XEVENT (event)->event_type == button_release_event)
    return make_number (XEVENT (event)->event.button.modifiers);
  else if (XEVENT (event)->event_type == pointer_motion_event)
    return make_number (XEVENT (event)->event.motion.modifiers);
  else
    {
      event = wrong_type_argument (intern ("key-or-mouse-event-p"), event);
      goto again;
    }
}

DEFUN ("event-modifiers", Fevent_modifiers, Sevent_modifiers, 1, 1, 0,
       "Returns a list of symbols, the names of the modifier keys\n\
which were down when the given mouse or keyboard event was produced.\n\
See also the function event-modifier-bits.")
     (event)
  Lisp_Object event;
{
  int mod = XINT (Fevent_modifier_bits (event));
  Lisp_Object result = Qnil;
  if (mod & MOD_SHIFT)   result = Fcons (Qshift, result);
  if (mod & MOD_SYMBOL)  result = Fcons (Qsymbol, result);
  if (mod & MOD_HYPER)   result = Fcons (Qhyper, result);
  if (mod & MOD_SUPER)   result = Fcons (Qsuper, result);
  if (mod & MOD_META)    result = Fcons (Qmeta, result);
  if (mod & MOD_CONTROL) result = Fcons (Qcontrol, result);
  return result;
}

DEFUN ("event-x-pixel", Fevent_x_pixel, Sevent_x_pixel, 1, 1, 0,
 "Returns the X position of the given mouse-motion, button-press, or\n\
button-release event in pixels.")
     (event)
  Lisp_Object event;
{
  CHECK_EVENT_SAFE (event, 0);
  if (XEVENT (event)->event_type == pointer_motion_event)
    return make_number (XEVENT (event)->event.motion.x);
  else if (XEVENT (event)->event_type == button_press_event ||
	   XEVENT (event)->event_type == button_release_event)
    return make_number (XEVENT (event)->event.button.x);
  else
    return wrong_type_argument (Qmouse_event_p, event);
}

DEFUN ("event-y-pixel", Fevent_y_pixel, Sevent_y_pixel, 1, 1, 0,
 "Returns the Y position of the given mouse-motion, button-press, or\n\
button-release event in pixels.")
     (event)
  Lisp_Object event;
{
  CHECK_EVENT_SAFE (event, 0);
  if (XEVENT (event)->event_type == pointer_motion_event)
    return make_number (XEVENT (event)->event.motion.y);
  else if (XEVENT (event)->event_type == button_press_event ||
	   XEVENT (event)->event_type == button_release_event)
    return make_number (XEVENT (event)->event.button.y);
  else
    return wrong_type_argument (Qmouse_event_p, event);
}


extern int pixel_to_glyph_translation ();

static void
event_pixel_translation (event, char_x, char_y, w, bufp, class)
     Lisp_Object event, *class;
     int *char_x, *char_y, *bufp;
     struct window **w;
{
  int pix_x, pix_y;
  int res;
  Lisp_Object screen;
  
  CHECK_EVENT_SAFE (event, 0);
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
				    char_x, char_y, w, bufp, class);
  /* It looks to me like the result value of pixel_to_glyph_translation() is
     0:  modeline
     1:  over text, or over a glyph in the lineinfo column;
     2:  not over text, not in a window, or over an inactive minibuffer.
   */
  if (res == 2)
    *bufp = 0;
  else if (*w && NILP ((*w)->buffer))
    /* Why does this happen?  (Does it still happen?)
       I guess the window has gotten reused as a non-leaf... */
    *w = 0;

  /* #### pixel_to_glyph_translation() sometimes returns garbage...
     The word has type Lisp_Record (presumably meaning `extent') but the
     pointer points to random memory, often filled with 0, sometimes not.
   */
  if (!NILP (*class) && !EXTENTP (*class))
    abort ();
}


DEFUN ("event-screen", Fevent_screen, Sevent_screen, 1, 1, 0,
 "Given a mouse motion, button press, or button release event, return\n\
the screen on which that event occurred.  This will be nil for non-mouse\n\
events.")
/* #### This would be redundant if event-window returned a window even
   if over the modeline, and if there was some other easy way to detect
   a modeline hit.
 */
     (event)
  Lisp_Object event;
{
  CHECK_EVENT_SAFE (event, 0);
  if (SCREENP (XEVENT (event)->channel))
    return (XEVENT (event)->channel);
  else
    return Qnil;
}


DEFUN ("event-window", Fevent_window, Sevent_window, 1, 1, 0,
 "Given a mouse motion, button press, or button release event, compute\n\
and return the window on which that event occurred.  This may be nil if\n\
the event did not occur in an emacs window (in the border or modeline.)")
     (event)
  Lisp_Object event;
{
  int char_x, char_y, bufp;
  struct window *w;
  Lisp_Object window, class;
  event_pixel_translation (event, &char_x, &char_y, &w, &bufp, &class);
  if (! w) return Qnil;
  XSETR (window, Lisp_Window, w);
  return window;
}


DEFUN ("event-point", Fevent_point, Sevent_point, 1, 1, 0,
 "Returns the character position of the given mouse-motion, button-press,\n\
or button-release event.  If the event did not occur over a window, or did\n\
not occur over text, then this returns nil.  Otherwise, it returns an index\n\
into the buffer visible in the event's window.")
     (event)
  Lisp_Object event;
{
  int char_x, char_y, bufp;
  Lisp_Object class;
  struct window *w;
  event_pixel_translation (event, &char_x, &char_y, &w, &bufp, &class);
  if (! w) return Qnil;
  if (! bufp) return Qnil;
  return make_number (bufp);
}

DEFUN ("event-x", Fevent_x, Sevent_x, 1, 1, 0,
 "Returns the X position of the given mouse-motion, button-press, or\n\
button-release event in characters.")
     (event)
  Lisp_Object event;
{
  int char_x, char_y, bufp;
  Lisp_Object class;
  struct window *w;
  event_pixel_translation (event, &char_x, &char_y, &w, &bufp, &class);
/*  if (! w) return Qnil; */
  return make_number (char_x);
}

DEFUN ("event-y", Fevent_y, Sevent_y, 1, 1, 0,
 "Returns the Y position of the given mouse-motion, button-press, or\n\
button-release event in characters.")
     (event)
  Lisp_Object event;
{
  int char_x, char_y, bufp;
  Lisp_Object class;
  struct window *w;
  event_pixel_translation (event, &char_x, &char_y, &w, &bufp, &class);
/*  if (! w) return Qnil; */
  return make_number (char_y);
}


DEFUN ("event-glyph", Fevent_glyph, Sevent_glyph, 1, 1, 0,
 "If the given mouse-motion, button-press, or button-release event happened\n\
on top of a glyph, this returns it; else nil.")
     (event)
  Lisp_Object event;
{
  int char_x, char_y, bufp;
  Lisp_Object class;
  struct window *w;

  event_pixel_translation (event, &char_x, &char_y, &w, &bufp, &class);
  if (! w) return Qnil;
  return class;
}


DEFUN ("event-process", Fevent_process, Sevent_process, 1, 1, 0,
 "Returns the process of the given process-output event.")
     (event)
  Lisp_Object event;
{
  CHECK_EVENT_TYPE (event, process_event, Qprocess_event_p);
  return (XEVENT (event)->event.process.process);
}

DEFUN ("event-function", Fevent_function, Sevent_function, 1, 1, 0,
 "Returns the callback function of the given timeout, menu, or eval event.")
     (event)
  Lisp_Object event;
{
  CHECK_EVENT_SAFE (event, 0);
  switch (XEVENT (event)->event_type) {
  case timeout_event:
    return (XEVENT (event)->event.timeout.function);
  case menu_event:
  case eval_event:
    return (XEVENT (event)->event.eval.function);
  default:
    return wrong_type_argument (intern ("timeout-or-eval-event-p"), event);
  }
}

DEFUN ("event-object", Fevent_object, Sevent_object, 1, 1, 0,
 "Returns the callback function argument of the given timeout, menu, or\n\
eval event.")
     (event)
  Lisp_Object event;
{
 again:
  CHECK_EVENT_SAFE (event, 0);
  switch (XEVENT (event)->event_type)
    {
    case timeout_event:
      return (XEVENT (event)->event.timeout.object);
    case menu_event:
    case eval_event:
      return (XEVENT (event)->event.eval.object);
    default:
      event = wrong_type_argument (intern ("timeout-or-eval-event-p"), event);
      goto again;
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
  defsubr (&Sbutton_event_p);
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
  defsubr (&Sevent_screen);
  defsubr (&Sevent_window);
  defsubr (&Sevent_x);
  defsubr (&Sevent_y);
  defsubr (&Sevent_point);
  defsubr (&Sevent_glyph);
  defsubr (&Sevent_process);
  defsubr (&Sevent_function);
  defsubr (&Sevent_object);

  defsymbol (&Qeventp, "eventp");
  defsymbol (&Qkey_press_event_p, "key-press-event-p");
  defsymbol (&Qbutton_event_p, "button-event-p");
  defsymbol (&Qmouse_event_p, "mouse-event-p");
  defsymbol (&Qprocess_event_p, "process-event-p");

  QKbackspace = KEYSYM ("backspace");
  QKtab       = KEYSYM ("tab");
  QKlinefeed  = KEYSYM ("linefeed");
  QKreturn    = KEYSYM ("return");
  QKescape    = KEYSYM ("escape");
  QKspace     = KEYSYM ("space");
  QKdelete    = KEYSYM ("delete");
  QKnosymbol  = KEYSYM ("NoSymbol");

  staticpro (&QKbackspace);
  staticpro (&QKtab);
  staticpro (&QKlinefeed);
  staticpro (&QKreturn);
  staticpro (&QKescape);
  staticpro (&QKspace);
  staticpro (&QKdelete);
  staticpro (&QKnosymbol);
}
