#include "config.h"
#include "intl.h"
#include "lisp.h"
#include "bytecode.h"

#ifdef I18N4
#include <stdio.h>
#include "xterm.h"  /* For definition of x_current_display */

wchar_t temp_c;		/* Used by GET_SYNTAX() */


/* do_translate -- Look up translation of character, checking bounds.
   This function assumes that translate is non-null, so be sure to check
   before calling!
*/
wchar_t
do_translate (unsigned char *translate, register wchar_t c)
{
  if (IS_ASCII (c))
    c = translate[c];
  else if (IS_ISO_WIDE (c)) {
    c = ISO_WIDE_TO_BYTE (c);
    c = translate[c];
    c = ISO_BYTE_TO_WIDE (c);
  }
  return c;
}


/* Scratch buffers for converting between multi-byte and wide strings */
multibyte_string mb_buf = EMPTY_MULTIBYTE_STRING;
widechar_string  wc_buf = EMPTY_WIDECHAR_STRING;


/* safe_wcstombs -- Convert wide character string to multi-byte string type.
   Returns number of bytes, not including terminating null.
*/
size_t
safe_wcstombs (CONST wchar_t *wcs, multibyte_string *buf)
{
  /* Guess at number of bytes, assuming the worst case. */
  size_t safe_mb_len = wslen (wcs) * sizeof (wchar_t) + 1;

  SET_MB_STRING_SIZE (buf, safe_mb_len);
  buf->in_use = wcstombs (buf->data, wcs, buf->size);
  return buf->in_use;
}


/* safe_mbstowcs -- Convert multi-byte string to wide character string type.
   Returns number of bytes, not including terminating null.
*/
size_t
safe_mbstowcs (CONST char *mbs, widechar_string *buf)
{
  /* Guess at number of characters, assuming the worst case. */
  size_t safe_wc_len = strlen (mbs) + 1;

  SET_WC_STRING_SIZE (buf, safe_wc_len);
  buf->in_use = mbstowcs (buf->data, mbs, buf->size);
  return buf->in_use;
}


/* wc_substring_to_mb -- Convert wide character substring to multi-byte string type.
   
   Uses wide character buffer for auxiliary storage.
   Returns number of bytes, not including terminating null.
*/
size_t
wc_substring_to_mb (CONST wchar_t *wcs, size_t len,
		    multibyte_string *mb_buf, widechar_string *wc_buf)
{
  /* First copy wide character substring to scratch buffer,
     and make it null-terminated. */
  SET_WC_STRING_SIZE (wc_buf, len + 1);
  memcpy (wc_buf->data, wcs, len * sizeof(wchar_t));
  wc_buf->data[len] = '\0';

  return safe_wcstombs (wc_buf->data, mb_buf);
}


/* mb_substring_to_wc -- Convert multi-byte substring to wide character string type.

   Uses multi-byte buffer for auxiliary storage.
   Returns number of characters, not including terminating null.
*/
size_t
mb_substring_to_wc (CONST char *mbs, size_t len,
		    multibyte_string *mb_buf, widechar_string *wc_buf)
{
  /* First copy multi-byte substring to scratch buffer,
     and make it null-terminated. */
  SET_MB_STRING_SIZE (mb_buf, len + 1);
  memcpy (mb_buf->data, mbs, len);
  mb_buf->data[len] = '\0';

  return safe_mbstowcs (mb_buf->data, wc_buf);
}


/* wc_offset_to_mb -- Convert wide character offset to multi-byte offset.
*/
size_t
wc_offset_to_mb (char *mbs, int pos)
{
  register char *c = mbs;

  if (pos <= 0)
    return pos;

  while (pos-- > 0)
    c += mblen (c, sizeof (wchar_t));
  return c - mbs;
}


/****************************/
/*  associative array type  */
/****************************/

/* search_assoc_array -- Search associative array for a pair matching c.
*/
static wchar_int_pair *
search_assoc_array (assoc_array *array, wchar_t c)
{
  register int i;
  register wchar_int_pair *p;

  for (i = 0, p = array->data; i < array->in_use; i++, p++)
    if (p->wchar == c)
      return p;
  return NULL;
}

/* add_assoc_entry -- Add pair to associative array.
*/
void
set_assoc_entry (assoc_array *array, wchar_t c, int num)
{
  wchar_int_pair *p = search_assoc_array (array, c);

  if (!p) {
    if (ARRAY_FULL (array))
      GROW_ASSOC_ARRAY (array, 32);
    p = &(array->data[array->in_use++]);
    p->wchar = c;
  }
  p->value = num;
}


int
assoc_array_lookup (assoc_array *array, wchar_t c)
{
  wchar_int_pair *p = search_assoc_array (array, c);

  if (p)
    return p->value;
  else
    return array->default_value;
}


/***********************/
/*  set of chars type  */
/***********************/

/* empty_set_of_chars -- Make set empty with room to grow.
*/
void
empty_set_of_chars (set_of_chars *set)
{
  if (!set->data)
    GROW_SET_OF_CHARS (set, 64);
  set->data[0] = '\0';
  set->in_use = 1;
  set->complement = False;
}


/* add_to_set_of_chars -- Add c to set.  (Doesn't do anything with complement flag.)
*/
void
add_to_set_of_chars (set_of_chars *set, wchar_t c)
{
  if (IN_SET_OF_CHARS_RAW (set, c))
    return;

  if (!set->data)
    empty_set_of_chars (set);
  if ARRAY_FULL (set)
    GROW_SET_OF_CHARS (set, 32);

  set->data[set->in_use - 1] = c;
  set->data[set->in_use++] = '\0';
}


/* Test c for membership in set.
*/
BOOL
in_set_of_chars (set_of_chars *set, wchar_t c)
{
  BOOL result;

  if (set->anychar)
    return TRUE;
  result = (wschr (set->data, c) ? TRUE : FALSE);
  if (set->complement)
    result = !result;
  return result;
}


/********/


/* x_char_width -- Return width of designated character in the font set.

   I made this a function instead of a macro so that it would be easier to
   pass the address of the character to XwcTextEscapement.
*/
int
x_char_width (XFontSet font_set, wchar_t wc)
{
  return XwcTextEscapement (font_set, &wc, 1);
}


XIM input_method;
XIC input_context;
unsigned long input_method_event_mask;
Atom wc_atom;

/* init_input -- Set things up for i18n level 4 input.
*/
void init_input (CONST char *res_name, CONST char *res_class)
{
  XIMStyles *styles;
  XIMStyle supported_style = 0;
  unsigned short i;

  input_method = 0;
  input_context = 0;
  input_method_event_mask = 0;

  input_method = XOpenIM (x_current_display, NULL,
			  (char *) res_name, (char *) res_class);

  if (!input_method)
    return;

  /* Query input method for supported input styles and pick one.
     Right now, we choose a style which supports root-window preediting. */
  XGetIMValues (input_method, XNQueryInputStyle, &styles, NULL);
  for (i = 0; i < styles->count_styles; i++)
    {
      if (styles->supported_styles[i] == (XIMPreeditNothing|XIMStatusNothing))
	{
	  supported_style = styles->supported_styles[i];
	  break;
	}
    }

  if (!supported_style)
    return;

  input_context = XCreateIC (input_method,
			     XNInputStyle, supported_style,
			     NULL);
  if (!input_context)
    return;

  XGetICValues (input_context,
		XNFilterEvents, &input_method_event_mask,
		NULL);

  /* Get a new atom for wide character client messages. */
  wc_atom = XInternAtom (x_current_display, "Wide Character Event", False);
}


static widechar_string composed_input_buf = EMPTY_WIDECHAR_STRING;
Window main_window;  /* Convenient way to refer to main Era window. */

/* get_composed_input -- Process results of input method composition.

   This function copies the results of the input method composition to
   composed_input_buf.  Then for each character, a custom event of type
   wc_atom is sent with the character as its data.

   It is probably more efficient to copy the composition results to some
   allocated memory and send a single event pointing to that memory.
   That would cut down on the event processing as well as allow quick
   insertion into the buffer of the whole string.  It might require some
   care, though, to avoid fragmenting memory through the allocation and
   freeing of many small chunks.  Maybe the existing systme for
   (single-byte) string allocation can be used, multipling the length by
   sizeof (wchar_t) to get the right size.
*/
void
get_composed_input (XKeyPressedEvent *x_key_event)
{
  KeySym keysym;
  Status status;
  int len;
  register int i;
  XClientMessageEvent new_event;

 try_again:
  len = XwcLookupString (input_context, x_key_event, composed_input_buf.data,
			 composed_input_buf.size, &keysym, &status);
  switch (status)
    {
    case XBufferOverflow:
      GROW_WC_STRING (&composed_input_buf, 32);
      goto try_again;
    case XLookupChars:
      break;
    default:
      abort ();
    }

  new_event.type = ClientMessage;
  new_event.display = x_key_event->display;
  new_event.window = x_key_event->window;
  new_event.message_type = wc_atom;
  new_event.format = 32;  /* 32-bit wide data */
  new_event.data.l[2] = new_event.data.l[3] = new_event.data.l[4] = 0L;
  new_event.data.l[0] = x_key_event->time;
  for (i = 0; i < len; i++) {
    new_event.data.l[1] = ((wchar_t *) composed_input_buf.data)[i];
    XSendEvent (x_current_display, main_window, False, 0L, (XEvent *) &new_event);
  }
}
#endif /* I18N4 */


Lisp_Object Qdomain;
Lisp_Object Qdefer_gettext;

DEFUN ("ignore-defer-gettext", Fignore_defer_gettext, Signore_defer_gettext,
       1, 1, 0,
  "If OBJ is of the form (defer-gettext \"string\"), return the string.\n\
The purpose of the defer-gettext symbol is to identify strings which\n\
are translated when they are referenced instead of when they are defined.")
  (obj)
     Lisp_Object obj;
{
  if (CONSP (obj) && SYMBOLP (Fcar (obj)) && EQ (Fcar (obj), Qdefer_gettext))
    return Fcar (Fcdr (obj));
  else
    return obj;
}

DEFUN ("gettext", Fgettext, Sgettext, 1, 1, 0,
  "Look up STRING in the default message domain and return its translation.\n\
This function does nothing if I18N3 was not enabled when Emacs was compiled.")
  (string)
     Lisp_Object string;
{
#ifdef I18N3
  if (STRINGP (string))
    return build_string (gettext ((char *) XSTRING (string)->data));
  else
    return string;
#else
  return string;
#endif
}

DEFUN ("dgettext", Fdgettext, Sdgettext, 2, 2, 0,
  "Look up STRING in the specified message domain and return its translation.\n\
This function does nothing if I18N3 was not enabled when Emacs was compiled.")
  (domain, string)
     Lisp_Object domain, string;
{
  CHECK_STRING (domain, 1);
  CHECK_STRING (string, 1);
#ifdef I18N3
  return build_string (dgettext ((char *) XSTRING (domain)->data,
				 (char *) XSTRING (string)->data));
#else
  return string;
#endif
}

DEFUN ("bind-text-domain", Fbind_text_domain, Sbind_text_domain, 2, 2, 0,
  "Associate a pathname with a message domain.\n\
Here's how the path to message files is constructed under SunOS 5.0:\n\
  {pathname}/{LANG}/LC_MESSAGES/{domain}.mo\n\
This function does nothing if I18N3 was not enabled when Emacs was compiled.")
  (domain, pathname)
     Lisp_Object domain, pathname;
{
  CHECK_STRING (domain, 1);
  CHECK_STRING (pathname, 1);
#ifdef I18N3
  return build_string (bindtextdomain ((char *) XSTRING (domain)->data,
				       (char *) XSTRING (pathname)->data));
#else
  return Qnil;
#endif
}

extern int load_in_progress;

DEFUN ("domain", Fdomain, Sdomain, 1, 1, 0,
 "Specify the domain used for translating messages in this source file.\n\
The domain declaration may only appear at top-level, and should preceed\n\
all function and variable definitions.\n\
\n\
The presence of this declaration in a compiled file effectively sets the\n\
domain of all functions and variables which are defined in that file.\n\
Bug: it has no effect on source (.el) files, only compiled (.elc) files.")
     (domain_name)
     Lisp_Object domain_name;
{
  CHECK_STRING (domain_name, 0);
  if (load_in_progress)
    {
#ifdef I18N3
      Vfile_domain = Fpurecopy (domain_name);
      return Vfile_domain;
#else
      return (domain_name);
#endif
    }
  else
    return Qnil;
}

void
syms_of_intl (void)
{
  defsymbol (&Qdomain, "domain");

  /* defer-gettext is defined as a symbol because when it is used in menu
     specification strings, it is not evaluated as a function by
     menu_item_descriptor_to_widget_value(). */
  defsymbol (&Qdefer_gettext, "defer-gettext");

  defsubr (&Signore_defer_gettext);
  defsubr (&Sgettext);
  defsubr (&Sdgettext);
  defsubr (&Sbind_text_domain);
  defsubr (&Sdomain);
}


#if 0
int isphonogram (int x) { abort (); }
int isideogram (int x) { abort (); }
int wschr  (wchar_t *s, int c) { abort (); }
int wslen  (wchar_t *s) { abort (); }
int fputwc () { abort (); }
int fgetwc () { abort (); }
#endif
