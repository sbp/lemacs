#ifndef _EMACS_INTL_H_
#define _EMACS_INTL_H_

#include <ctype.h>

#ifdef I18N2
#include <string.h>
#endif

#if 0
 /* This def is no good because it attempts to write into purespace if
    the menu items have been copied.  There are currently no uses of it,
    and this has too big an impact on the source code anyway.  We can
    tag strings to be translated in other ways, by teaching the extractor
    program about the syntax of "(defvar .*-menubar" or by flagging the
    strings with special comments like ";###gettext" at end-of-line. --jwz
  */
/*# define IGNORE_DEFER_GETTEXT(obj)	obj = Fignore_defer_gettext (obj)*/
#else
/*# define IGNORE_DEFER_GETTEXT(obj) (obj)*/
#endif

#ifdef I18N3

# if 0
#  include <libintl.h>	/* SunOS 4 doesn't have this loser */
# else
   extern char *dgettext (CONST char *, CONST char *);
   extern char *gettext (CONST char *);
   extern char *textdomain(CONST char *);
   extern char *bindtextdomain (CONST char *, CONST char *);
# endif

# define GETTEXT(x)  gettext(x)

#else /* !I18N3 */
# define GETTEXT(x)  (x)
#endif /* !I18N3 */

#ifdef _EMACS_LISP_H_
extern Lisp_Object Qdefer_gettext;
#ifdef I18N3
extern Lisp_Object Vfile_domain;
#endif
#endif /* _EMACS_LISP_H_ */

/* DEFER_GETTEXT is used to identify strings which are translated when
   they are referenced instead of when they are defined.
   These include Qerror_messages and initialized arrays of strings.
*/
#define DEFER_GETTEXT(x)  (x)

#ifdef I18N4
#include <stdlib.h>
#include <X11/Xlib.h>
#include "iso-wide.h"

#include <wctype.h>
#include <widec.h>

wchar_t do_translate (unsigned char *translate, register wchar_t c);

/****************************/
/*  dynamic array template  */
/****************************/

#define DYNAMIC_ARRAY(data_type)					\
  data_type *data;	/* data[ 0 ] .. data[ in_use-1 ], or NULL. */	\
  size_t size;		/* Number of elements allocated. */		\
  size_t in_use		/* Number of elements in use. */


#define EMPTY_DYNAMIC_ARRAY  NULL, 0, 0

#define ARRAY_FULL(array)  ((array)->in_use == (array)->size)

/* SET_ARRAY_SIZE -- Make sure ARRAY can accomodate LENGTH elements.
*/
#define SET_ARRAY_SIZE(array, length, data_type)			\
{									\
  if ((length) > (array)->size) {					\
    data_type *new_data;						\
    new_data = (data_type *) xmalloc ((length) * sizeof (data_type));	\
    memcpy (new_data,(array)->data,(array)->size * sizeof(data_type));	\
    if ((array)->data)							\
      xfree ((array)->data);						\
    (array)->size = (length);						\
    (array)->data = new_data;						\
  }									\
}

/* GROW_ARRAY -- Increase ARRAY size by GROWTH_SIZE.
*/
#define GROW_ARRAY(array, growth_size, data_type)  \
  SET_ARRAY_SIZE (array, (array)->size + (growth_size), data_type)


/****************************/
/*  character string types  */
/****************************/

typedef struct {
  DYNAMIC_ARRAY (char);
} multibyte_string;

typedef struct {
  DYNAMIC_ARRAY (wchar_t);
} widechar_string;

#define EMPTY_MULTIBYTE_STRING  { EMPTY_DYNAMIC_ARRAY }
#define EMPTY_WIDECHAR_STRING   { EMPTY_DYNAMIC_ARRAY }

#define SET_MB_STRING_SIZE(string, length)  SET_ARRAY_SIZE (string, length, char)
#define SET_WC_STRING_SIZE(string, length)  SET_ARRAY_SIZE (string, length, wchar_t)

#define GROW_MB_STRING(string, growth_size)  GROW_ARRAY (string, growth_size, char)
#define GROW_WC_STRING(string, growth_size)  GROW_ARRAY (string, growth_size, wchar_t)

extern multibyte_string mb_buf;
extern widechar_string  wc_buf;

size_t safe_wcstombs (CONST wchar_t *wcs, multibyte_string *buf);
size_t safe_mbstowcs (CONST char *mbs, widechar_string *buf);
size_t wc_substring_to_mb (CONST wchar_t *wcs, size_t len,
			   multibyte_string *mb_buf, widechar_string *wc_buf);
size_t mb_substring_to_wc (CONST char *mbs, size_t len,
			   multibyte_string *mb_buf, widechar_string *wc_buf);
size_t wc_offset_to_mb (char *mbs, int pos);

/********/

extern XIM input_method;
extern XIC input_context;
extern unsigned long input_method_event_mask;
extern Atom wc_atom;
extern Window main_window;

extern int x_char_width (XFontSet font_set, wchar_t wc);
extern void get_composed_input (XKeyPressedEvent *x_event);

#endif /* I18N4 */

#endif /* _EMACS_INTL_H _*/
