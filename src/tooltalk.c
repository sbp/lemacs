/* Tooltalk support for Emacs.
   Copyright (C) 1993, 1994 Sun Microsystems, Inc.

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

#include <stdio.h>
#include <X11/Xlib.h>
#include <tt_c.h>

#include "intl.h"
#include "lisp.h"
#include "xterm.h"
#include "blockio.h"
#include "process.h"
#include "tooltalk.h"

#define writeit(x) (write_string_1 ((x), -1, printcharfun))

/* static Tt_pattern tooltalk_pattern; */ /* UNUSED */
Lisp_Object Vtooltalk_fd;

#ifdef TT_DEBUG
static FILE *tooltalk_log_file;
#endif

static Lisp_Object 
  Vtooltalk_message_handler_hook,
  Vtooltalk_pattern_handler_hook,
  Vtooltalk_unprocessed_message_hook;

static Lisp_Object
  Qtooltalk_message_handler_hook,
  Qtooltalk_pattern_handler_hook,
  Qtooltalk_unprocessed_message_hook;

static Lisp_Object 
  Qreceive_tooltalk_message,
  Qtt_address,
  Qtt_args_count,
  Qtt_arg_bval,
  Qtt_arg_ival,
  Qtt_arg_mode,
  Qtt_arg_type,
  Qtt_arg_val,
  Qtt_class,
  Qtt_category,
  Qtt_disposition,
  Qtt_file,
  Qtt_gid,
  Qtt_handler,
  Qtt_handler_ptype,
  Qtt_object,
  Qtt_op,
  Qtt_opnum,
  Qtt_otype,
  Qtt_scope,
  Qtt_sender,
  Qtt_sender_ptype,
  Qtt_session,
  Qtt_state,
  Qtt_status,
  Qtt_status_string,
  Qtt_uid, 
  Qtt_callback,
  Qtt_plist,
  Qtt_prop,

  Qtt_reject,                /* return-tooltalk-message */
  Qtt_reply,
  Qtt_fail,

  Q_TT_MODE_UNDEFINED,       /* enum Tt_mode */
  Q_TT_IN,
  Q_TT_OUT,
  Q_TT_INOUT,
  Q_TT_MODE_LAST,

  Q_TT_SCOPE_NONE,            /* enum Tt_scope */
  Q_TT_SESSION,
  Q_TT_FILE,
  Q_TT_BOTH,
  Q_TT_FILE_IN_SESSION,

  Q_TT_CLASS_UNDEFINED,       /* enum Tt_class */
  Q_TT_NOTICE,
  Q_TT_REQUEST,
  Q_TT_CLASS_LAST,

  Q_TT_CATEGORY_UNDEFINED,    /* enum Tt_category */
  Q_TT_OBSERVE,
  Q_TT_HANDLE,
  Q_TT_CATEGORY_LAST,

  Q_TT_PROCEDURE,             /* typedef enum Tt_address */
  Q_TT_OBJECT,
  Q_TT_HANDLER,
  Q_TT_OTYPE,
  Q_TT_ADDRESS_LAST,

  Q_TT_CREATED,               /* enum Tt_state */
  Q_TT_SENT,
  Q_TT_HANDLED,
  Q_TT_FAILED,
  Q_TT_QUEUED,
  Q_TT_STARTED,
  Q_TT_REJECTED,
  Q_TT_STATE_LAST,

  Q_TT_DISCARD,              /* enum Tt_disposition */
  Q_TT_QUEUE,
  Q_TT_START;

Lisp_Object Qtooltalk_error;



/*                                     */
/* machinery for tooltalk-message type */
/*                                     */

Lisp_Object Qtooltalk_messagep;

struct Lisp_Tooltalk_Message
{
  struct lcrecord_header header;
  Tt_message m;
};

static Lisp_Object mark_tooltalk_message (Lisp_Object, void (*) (Lisp_Object));
static void print_tooltalk_message (Lisp_Object, Lisp_Object, int);
DEFINE_LRECORD_IMPLEMENTATION ("tooltalk-message", lrecord_tooltalk_message,
                               mark_tooltalk_message, print_tooltalk_message, 
                               0, 0,
			       sizeof (struct Lisp_Tooltalk_Message));

static Lisp_Object
mark_tooltalk_message (Lisp_Object obj, void (*markobj) (Lisp_Object))
{
  return Qnil;
}

static void
print_tooltalk_message (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  struct Lisp_Tooltalk_Message *p = XTOOLTALK_MESSAGE (obj);
  
  char buf[200];

  if (print_readably)
    error (GETTEXT ("printing unreadable object #<tooltalk_message 0x%x>"),
	   (long) p);

  sprintf (buf, "#<tooltalk_message id:%d>", p->m);
  writeit (buf);
}

Lisp_Object
make_tooltalk_message (Tt_message m)
{
  struct Lisp_Tooltalk_Message *p
    = alloc_lcrecord (sizeof (struct Lisp_Tooltalk_Message),
		      lrecord_tooltalk_message);
  Lisp_Object val;

  p->m = m;
  XSETR (val, Lisp_Tooltalk_Message, p);
  return val;
}


Tt_message
unbox_tooltalk_message (Lisp_Object msg)
{
  CHECK_TOOLTALK_MESSAGE (msg, 0);
  return XTOOLTALK_MESSAGE (msg)->m;
}

DEFUN ("tooltalk_messagep", Ftooltalk_messagep, Stooltalk_messagep, 1, 1, 0,
       "Whether the given object is a tooltalk message.")
  (obj)
  Lisp_Object obj;
{
  return (TOOLTALK_MESSAGEP (obj) ? Qt : Qnil);
}




/*                                     */
/* machinery for tooltalk-pattern type */
/*                                     */

Lisp_Object Qtooltalk_patternp;

struct Lisp_Tooltalk_Pattern
{
  struct lcrecord_header header;
  Tt_pattern p;
};

static Lisp_Object mark_tooltalk_pattern (Lisp_Object, void (*) (Lisp_Object));
static void print_tooltalk_pattern (Lisp_Object, Lisp_Object, int);
DEFINE_LRECORD_IMPLEMENTATION ("tooltalk-pattern", lrecord_tooltalk_pattern,
                               mark_tooltalk_pattern, print_tooltalk_pattern, 
                               0, 0,
			       sizeof (struct Lisp_Tooltalk_Pattern));

static Lisp_Object
mark_tooltalk_pattern (Lisp_Object obj, void (*markobj) (Lisp_Object))
{
  return Qnil;
}

static void
print_tooltalk_pattern (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  struct Lisp_Tooltalk_Pattern *p = XTOOLTALK_PATTERN (obj);
  
  char buf[200];

  if (print_readably)
    error (GETTEXT ("printing unreadable object #<tooltalk_pattern 0x%x>"),
	   (long) p);

  sprintf (buf, "#<tooltalk_pattern id:%d>", p->p);
  writeit (buf);
}

Lisp_Object
make_tooltalk_pattern (Tt_pattern m)
{
  struct Lisp_Tooltalk_Pattern *p
    = alloc_lcrecord (sizeof (struct Lisp_Tooltalk_Pattern),
		      lrecord_tooltalk_pattern);
  Lisp_Object val;

  p->p = m;
  XSETR (val, Lisp_Tooltalk_Pattern, p);
  return val;
}


Tt_pattern
unbox_tooltalk_pattern (Lisp_Object msg)
{
  CHECK_TOOLTALK_PATTERN (msg, 0);
  return XTOOLTALK_PATTERN (msg)->p;
}

DEFUN ("tooltalk_patternp", Ftooltalk_patternp, Stooltalk_patternp, 1, 1, 0,
       "Whether the given object is a tooltalk pattern.")
  (obj)
  Lisp_Object obj;
{
  return (TOOLTALK_PATTERNP (obj) ? Qt : Qnil);
}



static Lisp_Object 
make_tooltalk_constant (char *name, int value)
{
  Lisp_Object s = intern(name);
  
  Fset(s, make_number(value));
  return s;
}


static int 
tooltalk_constant_value (Lisp_Object s)
{
  if (FIXNUMP(s))
    return XINT(s);
  else if (SYMBOLP(s))
    return XINT(XSYMBOL(s)->value);
  else
    return 0;   /* should never occur */
}


static void
check_status (Tt_status st)
{
  if (tt_is_err(st))
    Fsignal(Qtooltalk_error, Fcons(
      build_string(tt_status_message(st)), Qnil));
}

DEFUN ("receive-tooltalk-message", 
       Freceive_tooltalk_message, 
       Sreceive_tooltalk_message, 
       0, 2, 0,

       "Run tt_message_receive().")
    (ignore1, ignore2)
    Lisp_Object ignore1, ignore2; /* filters are called with two arguments. */
{
  Tt_message mess = tt_message_receive();
  Lisp_Object msg = make_tooltalk_message(mess);
  struct gcpro gcpro1;

  GCPRO1(msg);
  if (mess != NULL && !NILP(Vtooltalk_unprocessed_message_hook))
    run_hook_with_args (Qtooltalk_unprocessed_message_hook, 1, msg);
  UNGCPRO;

  /* see comment in event-stream.c about this return value. */
  return make_number(0);
}

Tt_callback_action
tooltalk_message_callback (Tt_message m, Tt_pattern p)
{
  Lisp_Object cb;
  Lisp_Object msg = make_tooltalk_message(m);    
  Lisp_Object pat = make_tooltalk_pattern(p);
  struct gcpro gcpro1, gcpro2;

#ifdef TT_DEBUG
  int i, j;

  fprintf(tooltalk_log_file, "message_cb: %d\n", m);
  fprintf(tooltalk_log_file, "op: %s (", tt_message_op(m));
  for (j = tt_message_args_count(m), i = 0; i < j; i++) {
    fprintf(tooltalk_log_file, "%s \"%s\"", tt_message_arg_type(m, i),
	    tt_message_arg_val(m, i));
    fprintf(tooltalk_log_file, "%s", i == j-1 ? ")" : ", ");
  }
  fprintf(tooltalk_log_file, "\n\n");
  fflush(tooltalk_log_file);
#endif

  VOID_TO_LISP (cb, tt_message_user (m, TOOLTALK_CALLBACK_KEY));
  GCPRO2(msg, pat);
  if (!NILP(Vtooltalk_message_handler_hook))
    run_hook_with_args(Qtooltalk_message_handler_hook, 2, msg, pat);

  if ((SYMBOLP(cb) && EQ(Qt, Ffboundp(cb))) ||
      (CONSP(cb) && EQ(Qlambda, Fcar(cb)) && !NILP(Flistp(Fcar(Fcdr(cb))))))
    call2(cb, msg, pat);
  UNGCPRO;

  tt_message_destroy(m);

  return TT_CALLBACK_PROCESSED;
}


Tt_callback_action
tooltalk_pattern_callback (Tt_message m, Tt_pattern p)
{
  Lisp_Object cb;
  Lisp_Object msg = make_tooltalk_message (m);    
  Lisp_Object pat = make_tooltalk_pattern (p);
  struct gcpro gcpro1, gcpro2;

#ifdef TT_DEBUG
  int i, j;

  fprintf(tooltalk_log_file, "pattern_cb: %d\n", m);
  fprintf(tooltalk_log_file, "op: %s (", tt_message_op(m));
  for (j = tt_message_args_count(m), i = 0; i < j; i++) {
    fprintf(tooltalk_log_file, "%s \"%s\"", tt_message_arg_type(m, i),
	    tt_message_arg_val(m, i));
    fprintf(tooltalk_log_file, "%s", i == j-1 ? ")" : ", ");
  }
  fprintf(tooltalk_log_file, "\n\n");
  fflush(tooltalk_log_file);
#endif

  VOID_TO_LISP (cb, tt_pattern_user (p, TOOLTALK_CALLBACK_KEY));
  GCPRO2(msg, pat);
  if (!NILP(Vtooltalk_pattern_handler_hook))
    run_hook_with_args(Qtooltalk_pattern_handler_hook, 2, msg, pat);

  if (SYMBOLP(cb) && EQ(Qt, Ffboundp(cb)))
    call2(cb, msg, pat);
  UNGCPRO;

  tt_message_destroy(m);
  return TT_CALLBACK_PROCESSED;
}


static Lisp_Object
tt_mode_symbol (Tt_mode n)
{
  switch (n) {
  case TT_MODE_UNDEFINED:             return Q_TT_MODE_UNDEFINED;
  case TT_IN:                         return Q_TT_IN;			
  case TT_OUT:                        return Q_TT_OUT;
  case TT_INOUT:                      return Q_TT_INOUT;
  case TT_MODE_LAST:                  return Q_TT_MODE_LAST;
  default:                            return Qnil;
  }
}


static Lisp_Object 
tt_scope_symbol (Tt_scope n)
{
  switch(n) {
  case TT_SCOPE_NONE:                 return Q_TT_SCOPE_NONE;
  case TT_SESSION:                    return Q_TT_SESSION;
  case TT_FILE:                       return Q_TT_FILE;
  case TT_BOTH:                       return Q_TT_BOTH;
  case TT_FILE_IN_SESSION:            return Q_TT_FILE_IN_SESSION;
  default:                            return Qnil;
  }
}


static Lisp_Object
tt_class_symbol (Tt_class n)
{
  switch(n) {
  case TT_CLASS_UNDEFINED:            return Q_TT_CLASS_UNDEFINED;
  case TT_NOTICE:                     return Q_TT_NOTICE;
  case TT_REQUEST:                    return Q_TT_REQUEST;
  case TT_CLASS_LAST:                 return Q_TT_CLASS_LAST;
  default:                            return Qnil;
  }
}


/*
 * This is not being used.  Is that a mistake or is this function
 * simply not necessary?
 */
#if 0
static Lisp_Object
tt_category_symbol (Tt_category n)
{
  switch(n) {
  case TT_CATEGORY_UNDEFINED:         return Q_TT_CATEGORY_UNDEFINED;
  case TT_OBSERVE:                    return Q_TT_OBSERVE;
  case TT_HANDLE:                     return Q_TT_HANDLE;
  case TT_CATEGORY_LAST:              return Q_TT_CATEGORY_LAST;
  default:                            return Qnil;
  }
}
#endif /* 0 */

static Lisp_Object
tt_address_symbol (Tt_address n)
{
  switch(n) {
  case TT_PROCEDURE:                  return Q_TT_PROCEDURE;
  case TT_OBJECT:                     return Q_TT_OBJECT;
  case TT_HANDLER:                    return Q_TT_HANDLER;
  case TT_OTYPE:                      return Q_TT_OTYPE;
  case TT_ADDRESS_LAST:               return Q_TT_ADDRESS_LAST;
  default:                            return Qnil;
  }
}


static Lisp_Object
tt_state_symbol (Tt_state n)
{
  switch(n) {
  case TT_CREATED:                    return Q_TT_CREATED;
  case TT_SENT:                       return Q_TT_SENT;
  case TT_HANDLED:                    return Q_TT_HANDLED;
  case TT_FAILED:                     return Q_TT_FAILED;
  case TT_QUEUED:                     return Q_TT_QUEUED;
  case TT_STARTED:                    return Q_TT_STARTED;
  case TT_REJECTED:                   return Q_TT_REJECTED;
  case TT_STATE_LAST:                 return Q_TT_STATE_LAST;
  default:                            return Qnil;
  }
}


static Lisp_Object
tt_build_string (char *s)
{
  return build_string((s) ? s : "");
}


static Lisp_Object
tt_opnum_string (int n)
{
  char buf[32];

  sprintf(buf, "%u", n);
  return build_string(buf);
}



static Lisp_Object
tt_message_arg_ival_string (Tt_message m, int n)
{
  char buf[32];
  int value;

  check_status(tt_message_arg_ival(m, n, &value));
  sprintf(buf, "%d", value);
  return build_string(buf);
}

static Lisp_Object
tt_message_arg_bval_vector (Tt_message m, int n)
{
  unsigned char *value;
  int len = 0;

  check_status(tt_message_arg_bval(m, n, &value, &len));

  return make_string((char *) value, len);
}



DEFUN ("get-tooltalk-message-attribute", 
       Fget_tooltalk_message_attribute, 
       Sget_tooltalk_message_attribute, 
       2, 3, 0,
       "Returns the indicated Tooltalk message attribute.  Attributes are\n\
identified by symbols with the same name (underscores and all) as the\n\
suffix of the Tooltalk tt_message_<attribute> function that extracts the value.\n\
String attribute values are copied, enumerated type values (except disposition)\n\
are converted to symbols - e.g. TT_HANDLER is 'TT_HANDLER, uid and gid are \n\
represented by fixnums (small integers), opnum is converted to a string,\n\
and disposition is converted to a fixnum.  We convert opnum (a C int) to a \n\
string, e.g. 123 => \"123\" because there's no guarantee that opnums will fit\n\
within the range of Lisp integers.\n\
\n\
Use the 'plist attribute instead of the C API 'user attribute\n\
for user defined message data.  To retrieve the value of a message property\n\
specify the indicator for argn.  For example to get the value of a property\n\
called 'rflag, use\n\
   (get-tooltalk-message-attribute msg 'plist 'rflag)\n\
\n\
To get the value of a message argument use one of the 'arg_val (strings),\n\
'arg_ival (integers), or 'arg_bval (strings with embedded nulls), attributes.\n\
For example to get the integer value of the third argument:\n\
\n\
   (get-tooltalk-message-attribute msg 'arg_ival 2)\n\
\n\
As you can see, argument numbers are zero based.  The type of each argument\n\
can be retrieved with the 'arg_type attribute; however, Tooltalk doesn't\n\
define any semantics for the string value of 'arg_type.  Conventionally\n\
\"string\" is used for strings and \"int\" for 32 bit integers.  Note that\n\
Emacs Lisp stores the lengths of strings explicitly (unlike C) so treating the\n\
value returned by 'arg_bval like a string is fine.")
    (msg, indicator, argn)
    Lisp_Object msg;
    Lisp_Object indicator;
    Lisp_Object argn;
{
  Tt_message m = unbox_tooltalk_message(msg);
  int n = 0;

  CHECK_SYMBOL(indicator, 0);
  if (EQ(indicator, (Qtt_arg_bval))  || 
      EQ(indicator, (Qtt_arg_ival))  || 
      EQ(indicator, (Qtt_arg_mode))  || 
      EQ(indicator, (Qtt_arg_type))  || 
      EQ(indicator, (Qtt_arg_val))) {
    CHECK_FIXNUM(argn, 0);
    n = XINT(argn);
  }

  if (!VALID_TOOLTALK_MESSAGEP(m))
    return Qnil;

  else if (EQ(indicator, Qtt_arg_bval))
    return tt_message_arg_bval_vector(m, n);

  else if (EQ(indicator, Qtt_arg_ival))
    return tt_message_arg_ival_string(m, n);

  else if (EQ(indicator, Qtt_arg_mode))
    return tt_mode_symbol(tt_message_arg_mode(m, n));

  else if (EQ(indicator, Qtt_arg_type))
    return tt_build_string(tt_message_arg_type(m, n));

  else if (EQ(indicator, Qtt_arg_val))
    return tt_message_arg_bval_vector(m, n);

  else if (EQ(indicator, Qtt_args_count))
      return make_number(tt_message_args_count(m));

  else if (EQ(indicator, Qtt_address))
      return tt_address_symbol(tt_message_address(m));

  else if (EQ(indicator, Qtt_class))
    return tt_class_symbol(tt_message_class(m));

  else if (EQ(indicator, Qtt_disposition))
    return make_number(tt_message_disposition(m));

  else if (EQ(indicator, Qtt_file))
    return tt_build_string(tt_message_file(m));

  else if (EQ(indicator, Qtt_gid))
    return make_number(tt_message_gid(m));

  else if (EQ(indicator, Qtt_handler))
    return tt_build_string(tt_message_handler(m));

  else if (EQ(indicator, Qtt_handler_ptype))
    return tt_build_string(tt_message_handler_ptype(m));

  else if (EQ(indicator, Qtt_object))
    return tt_build_string(tt_message_object(m));

  else if (EQ(indicator, Qtt_op))
    return tt_build_string(tt_message_op(m));

  else if (EQ(indicator, Qtt_opnum))
    return tt_opnum_string(tt_message_opnum(m));

  else if (EQ(indicator, Qtt_otype))
    return tt_build_string(tt_message_otype(m));

  else if (EQ(indicator, Qtt_scope))
    return tt_scope_symbol(tt_message_scope(m));

  else if (EQ(indicator, Qtt_sender))
    return tt_build_string(tt_message_sender(m));

  else if (EQ(indicator, Qtt_sender_ptype))
    return tt_build_string(tt_message_sender_ptype(m));

  else if (EQ(indicator, Qtt_session))
    return tt_build_string(tt_message_session(m));

  else if (EQ(indicator, Qtt_state))
    return tt_state_symbol(tt_message_state(m));

  else if (EQ(indicator, Qtt_status))
    return make_number(tt_message_status(m));

  else if (EQ(indicator, Qtt_status_string))
    return tt_build_string(tt_message_status_string(m));

  else if (EQ(indicator, Qtt_uid))
    return make_number(tt_message_uid(m));
  
  else if (EQ(indicator, Qtt_callback))
    {
      Lisp_Object retval;
      VOID_TO_LISP (retval, tt_message_user(m, TOOLTALK_CALLBACK_KEY));
      return (retval);
    }

  else if (EQ(indicator, Qtt_prop))
    {
      Lisp_Object tempval;
      VOID_TO_LISP (tempval, tt_message_user(m, TOOLTALK_PLIST_KEY));
      return Fget(tempval, argn, Qnil);
    }

  else if (EQ(indicator, Qtt_plist))
    {
      Lisp_Object tempval;
      VOID_TO_LISP (tempval, tt_message_user(m, TOOLTALK_PLIST_KEY));
      return Fsymbol_plist(tempval);
    }

  else
    return Qnil;
}



DEFUN ("set-tooltalk-message-attribute", 
       Fset_tooltalk_message_attribute, 
       Sset_tooltalk_message_attribute, 
       3, 4, 0,
       "Initialize one Tooltalk message attribute.\n\
\n\
Attribute names and values are the same as for get-tooltalk-message-attribute.\n\
A property list is provided for user data (instead of the 'user message\n\
attribute), see get-tooltalk-message-attribute.\n\
\n\
The value of callback should be the name of a function of one argument.\n\
It will be applied to the message and matching pattern each time the state of the\n\
message changes.  This is usually used to notice when the messages state has\n\
changed to TT_HANDLED (or TT_FAILED), so that reply argument values\n\
can be used.\n\
\n\
If one of the argument attributes is specified, 'arg_val, 'arg_ival, or\n\
'arg_bval then argn must be the number of an already created argument.\n\
New arguments can be added to a message with add-tooltalk-message-arg.")
    (value, msg, indicator, argn)
    Lisp_Object value;
    Lisp_Object msg;
    Lisp_Object indicator;
    Lisp_Object argn;
{
  Tt_message m = unbox_tooltalk_message(msg);
  int n = 0;

  CHECK_SYMBOL(indicator, 0);
  if (EQ(indicator, (Qtt_arg_bval))  || 
      EQ(indicator, (Qtt_arg_ival))  || 
      EQ(indicator, (Qtt_arg_val))) {
    CHECK_FIXNUM(argn, 0);
    n = XINT(argn);
  }

  if (!VALID_TOOLTALK_MESSAGEP(m))
    return Qnil;

  else if (EQ(indicator, Qtt_address)) {
    CHECK_TOOLTALK_CONSTANT(value, 0);
    tt_message_address_set(m, tooltalk_constant_value(value));
  }
  else if (EQ(indicator, Qtt_class)) {
    CHECK_TOOLTALK_CONSTANT(value, 0);
    tt_message_class_set(m, tooltalk_constant_value(value));
  }
  else if (EQ(indicator, Qtt_file)) {
    CHECK_STRING(value, 0);
    tt_message_file_set(m, XSTRING_DATA(value));
  }
  else if (EQ(indicator, Qtt_handler_ptype)) {
    CHECK_STRING(value, 0);
    tt_message_handler_ptype_set(m, XSTRING_DATA(value));
  }
  else if (EQ(indicator, Qtt_handler)) {
    CHECK_STRING(value, 0);
    tt_message_handler_set(m, XSTRING_DATA(value));
  }
  else if (EQ(indicator, Qtt_object)) {
    CHECK_STRING(value, 0);
    tt_message_object_set(m, XSTRING_DATA(value));
  }
  else if (EQ(indicator, Qtt_op)) {
    CHECK_STRING(value, 0);
    tt_message_op_set(m, XSTRING_DATA(value));
  }
  else if (EQ(indicator, Qtt_otype)) {
    CHECK_STRING(value, 0);
    tt_message_otype_set(m, XSTRING_DATA(value));
  }
  else if (EQ(indicator, Qtt_scope)) {
    CHECK_TOOLTALK_CONSTANT(value, 0);
    tt_message_scope_set(m, tooltalk_constant_value(value));
  }
  else if (EQ(indicator, Qtt_sender_ptype)) {
    CHECK_STRING(value, 0);
    tt_message_sender_ptype_set(m, XSTRING_DATA(value));
  }
  else if (EQ(indicator, Qtt_session)) {
    CHECK_STRING(value, 0);
    tt_message_session_set(m, XSTRING_DATA(value));
  }
  else if (EQ(indicator, Qtt_arg_bval)) {
    CHECK_STRING(value, 0);
    tt_message_arg_bval_set(m, n, XSTRING(value)->data, XSTRING(value)->size);
  }
  else if (EQ(indicator, Qtt_arg_ival)) {
    CHECK_FIXNUM(value, 0);
    tt_message_arg_ival_set(m, n, XINT(value));
  }
  else if (EQ(indicator, Qtt_arg_val)) {
    CHECK_STRING(value, 0);
    tt_message_arg_val_set(m, n, XSTRING_DATA(value));
  }
  else if (EQ(indicator, Qtt_status)) {
    CHECK_FIXNUM(value, 0);
    tt_message_status_set(m, XINT(value));
  }
  else if (EQ(indicator, Qtt_status_string)) {
    CHECK_STRING(value, 0);
    tt_message_status_string_set(m, XSTRING_DATA(value));
  }
  else if (EQ(indicator, Qtt_callback)) {
    CHECK_SYMBOL(value, 0);
    tt_message_user_set(m, TOOLTALK_CALLBACK_KEY, LISP_TO_VOID (value));
  }
  else if (EQ(indicator, Qtt_prop))
    {
      Lisp_Object tempval;
      VOID_TO_LISP (tempval, tt_message_user(m, TOOLTALK_PLIST_KEY));
      return Fput(tempval, argn, value);
    }

  else if (EQ(indicator, Qtt_plist)) {
    Lisp_Object tempval;
    CHECK_LIST(value, 0);
    VOID_TO_LISP (tempval, tt_message_user(m, TOOLTALK_PLIST_KEY));
    return Fsetplist(tempval, value);
  }
  return Qnil;
}



DEFUN ("return-tooltalk-message", 
       Freturn_tooltalk_message,
       Sreturn_tooltalk_message,
       1, 2, 0,
       "Send a reply to this message.  The second argument can be\n\
'reply, 'reject or 'fail; the default is 'reply.  Before sending\n\
a reply all message arguments whose mode is TT_INOUT or TT_OUT should\n\
have been filled in - see set-tooltalk-message-attribute.")
    (msg, mode)
    Lisp_Object msg, mode;
{
  Tt_message m = unbox_tooltalk_message(msg);

  if (NILP(mode))
    mode = Qtt_reply;
  else
    CHECK_SYMBOL(mode, 0);

  if (!VALID_TOOLTALK_MESSAGEP(m))      return Qnil;
  else if (EQ(mode, Qtt_reply))    tt_message_reply(m);
  else if (EQ(mode, Qtt_reject))   tt_message_reject(m);
  else if (EQ(mode, Qtt_fail))     tt_message_fail(m);

  return Qnil;
}

DEFUN ("create-tooltalk-message",
       Fcreate_tooltalk_message, 
       Screate_tooltalk_message, 
       0, 1, 0,
       "Create a new tooltalk message.  The messages session attribute is\n\
initialized to the default session.  Other attributes can be initialized\n\
with set-tooltalk-message-attribute.  Make-tooltalk-message is the\n\
preferred to create and initialize a message.\n\
Optional no_callback arg says don't add a C-level callback at all.\n\
Normally don't do that; just don't specify the Lisp callback when\n\
calling make-tooltalk-message.")
    (no_callback)
    Lisp_Object no_callback;
{
  Tt_message m = tt_message_create();
  Lisp_Object msg = make_tooltalk_message (m);
  Lisp_Object plist_sym = Fmake_symbol (build_string
					("Tooltalk Message plist")); 
  if (NILP(no_callback)) {
    tt_message_callback_add(m, tooltalk_message_callback);
  }
  tt_message_session_set(m, tt_default_session());
  tt_message_user_set(m, TOOLTALK_CALLBACK_KEY, LISP_TO_VOID (Qnil));
  tt_message_user_set(m, TOOLTALK_PLIST_KEY, LISP_TO_VOID (plist_sym));
  return msg;
}

DEFUN ("destroy-tooltalk-message",
       Fdestroy_tooltalk_message, 
       Sdestroy_tooltalk_message, 
       1, 1, 0,
       "Apply tt_message_destroy to the message.  It's not necessary\n\
to destroy messages after they've been processed by a message or\n\
pattern callback; the Lisp/Tooltalk callback machinery does this\n\
for you.")
     (msg)
     Lisp_Object msg;
{
  Tt_message m = unbox_tooltalk_message(msg);

  if (VALID_TOOLTALK_MESSAGEP(m))
      tt_message_destroy(m);

  return Qnil;
}


DEFUN ("add-tooltalk-message-arg",
       Fadd_tooltalk_message_arg, 
       Sadd_tooltalk_message_arg, 
       3, 4, 0,
"Append one new argument to the message.  Mode must be one of: TT_IN,\n\
TT_INOUT, or TT_OUT, type must be a string, and value can\n\
be a string or an integer.   Tooltalk doesn't\n\
define any semantics for type, so only the participants in the\n\
protocol you're using need to agree what types mean (if anything).\n\
Conventionally \"string\" is used for strings and \"int\" for 32 bit integers.\n\
Arguments can initialized by providing a value or with\n\
set-tooltalk-message-attribute, the latter is neccessary if you\n\
want to initialize the argument with a string that can contain\n\
embedded nulls (use 'arg_bval).")
     (msg, mode, vtype, value)
     Lisp_Object msg, mode, vtype, value;
{
  Tt_message m = unbox_tooltalk_message(msg);
  Tt_mode n;

  CHECK_STRING(vtype, 0);
  CHECK_TOOLTALK_CONSTANT(mode, 0);

  n = tooltalk_constant_value(mode);

  if (!VALID_TOOLTALK_MESSAGEP(m))
    return Qnil;
  else if (NILP(value))
    tt_message_arg_add(m, n, XSTRING_DATA(vtype), NULL);
  else if (STRINGP(value))
    tt_message_arg_add(m, n, XSTRING_DATA(vtype), XSTRING_DATA(value));
  else if (FIXNUMP(value))
    tt_message_iarg_add(m, n, XSTRING_DATA(vtype), XINT(value));

  return Qnil;
}


DEFUN ("send-tooltalk-message",
       Fsend_tooltalk_message, 
       Ssend_tooltalk_message, 
       1, 1, 0,
       "Send the message on its way.  Once the message has been sent it's\n\
almost always a good idea to get rid of it with destroy-tooltalk-message.")
     (msg)
     Lisp_Object msg;
{
  Tt_message m = unbox_tooltalk_message(msg);

  if (VALID_TOOLTALK_MESSAGEP(m))
      tt_message_send(m);

  return Qnil;
}


DEFUN ("create-tooltalk-pattern",
       Fcreate_tooltalk_pattern, 
       Screate_tooltalk_pattern, 
       0, 0, 0,
       "Create a new Tooltalk pattern and initialize its session attribute to\n\
be the default session.")
    ()
{
  Tt_pattern p = tt_pattern_create();
  Lisp_Object pat = make_tooltalk_pattern(p);
  Lisp_Object plist_sym = Fmake_symbol(build_string
				       ("Tooltalk Pattern plist")); 

  tt_pattern_callback_add(p, tooltalk_pattern_callback);
  tt_pattern_session_add(p, tt_default_session());
  tt_pattern_user_set(p, TOOLTALK_CALLBACK_KEY, LISP_TO_VOID (Qnil));
  tt_pattern_user_set(p, TOOLTALK_PLIST_KEY, LISP_TO_VOID (plist_sym));

  return pat;
}


DEFUN ("destroy-tooltalk-pattern",
       Fdestroy_tooltalk_pattern, 
       Sdestroy_tooltalk_pattern, 
       1, 1, 0,
       "Apply tt_pattern_destroy to the pattern.  This effectively unregisters\n\
the pattern.")
     (pat)
     Lisp_Object pat;
{
  Tt_pattern p = unbox_tooltalk_pattern(pat);

  if (VALID_TOOLTALK_PATTERNP(p))
      tt_pattern_destroy(p);

  return Qnil;
}


DEFUN ("add-tooltalk-pattern-attribute", 
       Fadd_tooltalk_pattern_attribute, 
       Sadd_tooltalk_pattern_attribute, 
       3, 3, 0,
       "Add one value to the indicated pattern attribute.  All Tooltalk\n\
pattern attributes are supported except 'user.  The names of attributes\n\
are the same as the Tooltalk accessors used to set them less the\n\
\"tooltalk_pattern_\" prefix and the \"_add\" ...")
    (value, pat, indicator)
    Lisp_Object value;
    Lisp_Object pat;
    Lisp_Object indicator;
{
  Tt_pattern p = unbox_tooltalk_pattern(pat);

  CHECK_SYMBOL(indicator, 0);

  if (!VALID_TOOLTALK_PATTERNP(p))
    return Qnil;

  else if (EQ(indicator, Qtt_category)) {
    CHECK_TOOLTALK_CONSTANT(value, 0);
    tt_pattern_category_set(p, tooltalk_constant_value(value));
  }
  else if (EQ(indicator, Qtt_address)) {
    CHECK_TOOLTALK_CONSTANT(value, 0);
    tt_pattern_address_add(p, tooltalk_constant_value(value));
  }
  else if (EQ(indicator, Qtt_class)) {
    CHECK_TOOLTALK_CONSTANT(value, 0);
    tt_pattern_class_add(p, tooltalk_constant_value(value));
  }
  else if (EQ(indicator, Qtt_disposition)) {
    CHECK_TOOLTALK_CONSTANT(value, 0);
    tt_pattern_disposition_add(p, tooltalk_constant_value(value));
  }
  else if (EQ(indicator, Qtt_file)) {
    CHECK_STRING(value, 0);
    tt_pattern_file_add(p, XSTRING_DATA(value));
  }
  else if (EQ(indicator, Qtt_object)) {
    CHECK_STRING(value, 0);
    tt_pattern_object_add(p, XSTRING_DATA(value));
  }
  else if (EQ(indicator, Qtt_op)) {
    CHECK_STRING(value, 0);
    tt_pattern_op_add(p, XSTRING_DATA(value));
  }
  else if (EQ(indicator, Qtt_otype)) {
    CHECK_STRING(value, 0);
    tt_pattern_otype_add(p, XSTRING_DATA(value));
  }
  else if (EQ(indicator, Qtt_scope)) {
    CHECK_TOOLTALK_CONSTANT(value, 0);
    tt_pattern_scope_add(p, tooltalk_constant_value(value));
  }
  else if (EQ(indicator, Qtt_sender)) {
    CHECK_STRING(value, 0);
    tt_pattern_sender_add(p, XSTRING_DATA(value));
  }
  else if (EQ(indicator, Qtt_sender_ptype)) {
    CHECK_STRING(value, 0);
    tt_pattern_sender_ptype_add(p, XSTRING_DATA(value));
  }
  else if (EQ(indicator, Qtt_session)) {
    CHECK_STRING(value, 0);
    tt_pattern_session_add(p, XSTRING_DATA(value));
  }
  else if (EQ(indicator, Qtt_state)) {
    CHECK_TOOLTALK_CONSTANT(value, 0);
    tt_pattern_state_add(p, tooltalk_constant_value(value));
  }
  else if (EQ(indicator, Qtt_callback)) {
    CHECK_SYMBOL(value, 0);
    tt_pattern_user_set(p, TOOLTALK_CALLBACK_KEY, LISP_TO_VOID (value));
  }

  return Qnil;
}


DEFUN ("add-tooltalk-pattern-arg",
       Fadd_tooltalk_pattern_arg, 
       Sadd_tooltalk_pattern_arg, 
       3, 4, 0,
       "Add one, fully specified, argument to a tooltalk pattern.  Mode must\n\
be one of TT_IN, TT_INOUT, or TT_OUT, type must be a string.\n\
Value can be an integer, string or nil.  If value is an integer then\n\
an integer argument (tt_pattern_iarg_add) added otherwise a string argument\n\
is added.  At present there's no way to add a binary data argument.")
     (pat, mode, vtype, value)
     Lisp_Object pat, mode, vtype, value;
{
  Tt_pattern p = unbox_tooltalk_pattern(pat);
  Tt_mode n;

  CHECK_STRING(vtype, 0);
  CHECK_TOOLTALK_CONSTANT(mode, 0);

  n = tooltalk_constant_value(mode);

  if (!VALID_TOOLTALK_PATTERNP(p))
    return Qnil;
  else if (NILP(value))
    tt_pattern_arg_add(p, n, XSTRING_DATA(vtype), NULL);
  else if (STRINGP(value))
    tt_pattern_arg_add(p, n, XSTRING_DATA(vtype), XSTRING_DATA(value));
  else if (FIXNUMP(value))
    tt_pattern_iarg_add(p, n, XSTRING_DATA(vtype), XINT(value));

  return Qnil;
}


DEFUN ("register-tooltalk-pattern",
       Fregister_tooltalk_pattern, 
       Sregister_tooltalk_pattern, 
       1, 1, 0,
       "Emacs will begin receiving messages that match this pattern.")
     (pat)
     Lisp_Object pat;
{
  Tt_pattern p = unbox_tooltalk_pattern(pat);

  if (VALID_TOOLTALK_PATTERNP(p))
      tt_pattern_register(p);

  return Qnil;
}


DEFUN ("unregister-tooltalk-pattern",
       Funregister_tooltalk_pattern, 
       Sunregister_tooltalk_pattern, 
       1, 1, 0,
       "Emacs will stop receiving messages that match this pattern.")
     (pat)
     Lisp_Object pat;
{
  Tt_pattern p = unbox_tooltalk_pattern(pat);

  if (VALID_TOOLTALK_PATTERNP(p))
      tt_pattern_unregister(p);

  return Qnil;
}


DEFUN ("tooltalk-pattern-prop-get",
       Ftooltalk_pattern_prop_get,
       Stooltalk_pattern_prop_get,
       2, 2, 0,
       "Return the value of the indicated property.   This is the last\n\
value set with (tooltalk-pattern-prop-set value pattern indicator).")
     (pat, indicator)
     Lisp_Object pat, indicator;
{
  Tt_pattern p = unbox_tooltalk_pattern(pat);

  if (VALID_TOOLTALK_PATTERNP(p))
    {
      Lisp_Object tempval;
      VOID_TO_LISP (tempval, tt_pattern_user(p, TOOLTALK_PLIST_KEY));
      return Fget(tempval, indicator, Qnil);
    }
  else
    return Qnil;
}


DEFUN ("tooltalk-pattern-prop-set",
       Ftooltalk_pattern_prop_set,
       Stooltalk_pattern_prop_set,
       2, 2, 0,
       "Set the value of the indicated property.  It can be retrieved\n\
with (tooltalk-pattern-prop-get pattern indicator).")
     (pat, indicator)
     Lisp_Object pat, indicator;
{
  Tt_pattern p = unbox_tooltalk_pattern(pat);

  if (VALID_TOOLTALK_PATTERNP(p))
    {
      Lisp_Object tempval;
      VOID_TO_LISP (tempval, tt_pattern_user(p, TOOLTALK_PLIST_KEY));
      return Fget(tempval, indicator, Qnil);
    }

  return Qnil;
}


DEFUN ("tooltalk-pattern-plist-get",
       Ftooltalk_pattern_plist_get,
       Stooltalk_pattern_plist_get,
       1, 1, 0,
       "Return the current value of the patterns plist.\n\
It can be set with (tooltalk-pattern-plist-set plist pattern).")
     (pat)
     Lisp_Object pat;
{
  Tt_pattern p = unbox_tooltalk_pattern(pat);

  if (VALID_TOOLTALK_PATTERNP(p))
    {
      Lisp_Object tempval;
      VOID_TO_LISP (tempval, tt_pattern_user(p, TOOLTALK_PLIST_KEY));
      return Fsymbol_plist(tempval);
    }
  else
    return Qnil;
}


DEFUN ("tooltalk-pattern-plist-set",
       Ftooltalk_pattern_plist_set,
       Stooltalk_pattern_plist_set,
       2, 2, 0,
       "Set the value of the patterns property-list.  It can be retrieved\n\
with (tooltalk-pattern-plist-get pattern).")
     (value, pat)
     Lisp_Object value, pat;
{
  Tt_pattern p = unbox_tooltalk_pattern(pat);

  CHECK_LIST(value, 0);
  
  if (VALID_TOOLTALK_PATTERNP(p))
    {
      Lisp_Object tempval;
      VOID_TO_LISP (tempval, tt_pattern_user(p, TOOLTALK_PLIST_KEY));
      return Fsetplist(tempval, value);
    }
  else
    return Qnil;
}

DEFUN ("tooltalk-default-procid", 
       Ftooltalk_default_procid, 
       Stooltalk_default_procid, 
       0, 0, 0,
       "Returns current default process identifier for your process."
       )
    ()
{
  char *procid = tt_default_procid();
  if (!procid)
    return Qnil;
  return build_string(procid);
}

DEFUN ("tooltalk-default-session", 
       Ftooltalk_default_session, 
       Stooltalk_default_session, 
       0, 0, 0,
       "Returns current default session identifier for the current default procid."
       )
    ()
{
  char *session = tt_default_session();
  if (!session)
    return Qnil;
  return build_string(session);
}

extern Lisp_Object connect_to_file_descriptor (Lisp_Object, Lisp_Object,
					       Lisp_Object, Lisp_Object);

static void
init_tooltalk (void)
{
  char *retval;
  Lisp_Object lp;
  Lisp_Object fil;

  retval = tt_open();
  if (tt_ptr_error(retval) != TT_OK)
    return;
    
  Vtooltalk_fd = make_number (tt_fd ());

  tt_session_join(tt_default_session());

  lp = connect_to_file_descriptor (build_string ("tooltalk"), Qnil,
				   Vtooltalk_fd, Vtooltalk_fd);
  if (!NILP(lp))
    {
      /* Don't ask the user for confirmation when exiting Emacs */
      Fprocess_kill_without_query (lp, Qnil);
      XSETR (fil, Lisp_Subr, &Sreceive_tooltalk_message);
      set_process_filter (lp, fil, 1);
    }
  else
    {
      tt_close ();
      Vtooltalk_fd = Qnil;
      return;
    }

#if defined (USG) && defined (sparc)
  /* Apparently the tt_message_send_on_exit() function does not exist
     under SunOS 4.x or IRIX 5 or various other non-Solaris-2 systems.
     No big deal if we don't do the following under those systems. */
  {
    Tt_message exit_msg = tt_message_create();
    
    tt_message_op_set(exit_msg, "emacs-aborted");
    tt_message_scope_set(exit_msg, TT_SESSION);
    tt_message_class_set(exit_msg, TT_NOTICE);
    tt_message_send_on_exit(exit_msg);
    tt_message_destroy(exit_msg);
  }
#endif
}


DEFUN ("tooltalk-open-connection",
       Ftooltalk_open_connection, Stooltalk_open_connection,
       0, 0, 0,
       "Opens a connection to the ToolTalk server.\n\
Returns t if successful, nil otherwise.")
 	()
/*
       "Opens a connection to the ToolTalk server.\n\
Argument ARGV is a list of strings describing the command line options.\n\
Returns a copy of ARGV from which the arguments used by the ToolTalk code\n\
to open the connection have been removed.")
 	(argv_list)
	Lisp_Object argv_list;
 */
{
  if (!NILP (Vtooltalk_fd))
    error ("Already connected to ToolTalk.");
  if (noninteractive)
    error ("Can't connect to ToolTalk in batch mode.");
  init_tooltalk ();
  return (NILP (Vtooltalk_fd) ? Qnil : Qt);
}


void
syms_of_tooltalk (void)
{
  defsymbol (&Qtooltalk_messagep, "tooltalk-messagep");
  defsubr (&Stooltalk_messagep);
  defsymbol (&Qtooltalk_patternp, "tooltalk-patternp");
  defsubr (&Stooltalk_patternp);
  defsymbol (&Qtooltalk_message_handler_hook, "tooltalk-message-handler-hook");
  defsymbol (&Qtooltalk_pattern_handler_hook, "tooltalk-pattern-handler-hook");
  defsymbol (&Qtooltalk_unprocessed_message_hook,
	     "tooltalk-unprocessed-message-hook");

  DEFVAR_LISP ("tooltalk-fd", &Vtooltalk_fd,
 "File descriptor returned by tt_initialize, or nil if not connected to ToolTalk.");
  Vtooltalk_fd = Qnil;

  DEFVAR_LISP ("tooltalk-message-handler-hook", 
	      &Vtooltalk_message_handler_hook, 
    "List of functions to be applied to each ToolTalk message reply received.\n\
This will always occur as a result of our sending a request message.\n\
Functions will be called with two arguments, the message and the\n\
corresponding pattern.  This hook will not be called if the request\n\
message was created without a C-level callback function (see\n\
`tooltalk-unprocessed-message-hook').");
  Vtooltalk_message_handler_hook = Qnil;

  DEFVAR_LISP ("tooltalk-pattern-handler-hook", 
	      &Vtooltalk_pattern_handler_hook, 
    "List of functions to be applied to each pattern-matching ToolTalk message.\n\
This is all messages except those handled by `tooltalk-message-handler-hook'.\n\
Functions will be called with two arguments, the message and the\n\
corresponding pattern.");
  Vtooltalk_pattern_handler_hook = Qnil;

  DEFVAR_LISP ("tooltalk-unprocessed-message-hook",
	      &Vtooltalk_unprocessed_message_hook,
    "List of functions to be applied to each unprocessed ToolTalk message.\n\
Unprocessed messages are messages that didn't match any patterns.");
  Vtooltalk_unprocessed_message_hook = Qnil;

  defsubr(&Sreceive_tooltalk_message);
  defsubr(&Screate_tooltalk_message);
  defsubr(&Sdestroy_tooltalk_message);
  defsubr(&Sadd_tooltalk_message_arg);
  defsubr(&Sget_tooltalk_message_attribute);
  defsubr(&Sset_tooltalk_message_attribute);
  defsubr(&Ssend_tooltalk_message);
  defsubr(&Sreturn_tooltalk_message);
  defsubr(&Screate_tooltalk_pattern);
  defsubr(&Sdestroy_tooltalk_pattern);  
  defsubr(&Sadd_tooltalk_pattern_attribute);  
  defsubr(&Sadd_tooltalk_pattern_arg);  
  defsubr(&Sregister_tooltalk_pattern);  
  defsubr(&Sunregister_tooltalk_pattern);  
  defsubr(&Stooltalk_pattern_plist_set);
  defsubr(&Stooltalk_pattern_plist_get);
  defsubr(&Stooltalk_pattern_prop_set);
  defsubr(&Stooltalk_pattern_prop_get);
  defsubr(&Stooltalk_default_procid);
  defsubr(&Stooltalk_default_session);
  defsubr(&Stooltalk_open_connection);

  defsymbol (&Qtooltalk_error, "tooltalk-error");

  Qreceive_tooltalk_message = intern("receive-tooltalk-message");
  Qtt_address = intern("address");
  Qtt_args_count = intern("args_count");
  Qtt_arg_bval = intern("arg_bval");
  Qtt_arg_ival = intern("arg_ival");
  Qtt_arg_mode = intern("arg_mode");
  Qtt_arg_type = intern("arg_type");
  Qtt_arg_val = intern("arg_val");
  Qtt_class = intern("class");
  Qtt_category = intern("category");
  Qtt_disposition = intern("disposition");
  Qtt_file = intern("file");
  Qtt_gid = intern("gid");
  Qtt_handler = intern("handler");
  Qtt_handler_ptype = intern("handler_ptype");
  Qtt_object = intern("object");
  Qtt_op = intern("op");
  Qtt_opnum = intern("opnum");
  Qtt_otype = intern("otype");
  Qtt_scope = intern("scope");
  Qtt_sender = intern("sender");
  Qtt_sender_ptype = intern("sender_ptype");
  Qtt_session = intern("session");
  Qtt_state = intern("state");
  Qtt_status = intern("status");
  Qtt_status_string = intern("status_string");
  Qtt_uid = intern("uid");
  Qtt_callback = intern("callback");
  Qtt_prop = intern("prop");
  Qtt_plist = intern("plist");
  Qtt_reject = intern("reject");
  Qtt_reply = intern("reply");
  Qtt_fail = intern("fail");

#define MAKE_CONSTANT(name) \
  make_tooltalk_constant(#name, name)
  
  Q_TT_MODE_UNDEFINED = MAKE_CONSTANT(TT_MODE_UNDEFINED);
  Q_TT_IN = MAKE_CONSTANT(TT_IN);
  Q_TT_OUT = MAKE_CONSTANT(TT_OUT);
  Q_TT_INOUT = MAKE_CONSTANT(TT_INOUT);
  Q_TT_MODE_LAST = MAKE_CONSTANT(TT_MODE_LAST);

  Q_TT_SCOPE_NONE = MAKE_CONSTANT(TT_SCOPE_NONE);
  Q_TT_SESSION = MAKE_CONSTANT(TT_SESSION);
  Q_TT_FILE = MAKE_CONSTANT(TT_FILE);
  Q_TT_BOTH = MAKE_CONSTANT(TT_BOTH);
  Q_TT_FILE_IN_SESSION = MAKE_CONSTANT(TT_FILE_IN_SESSION);

  Q_TT_CLASS_UNDEFINED = MAKE_CONSTANT(TT_CLASS_UNDEFINED);
  Q_TT_NOTICE = MAKE_CONSTANT(TT_NOTICE);
  Q_TT_REQUEST = MAKE_CONSTANT(TT_REQUEST);
  Q_TT_CLASS_LAST = MAKE_CONSTANT(TT_CLASS_LAST);

  Q_TT_CATEGORY_UNDEFINED = MAKE_CONSTANT(TT_CATEGORY_UNDEFINED);
  Q_TT_OBSERVE = MAKE_CONSTANT(TT_OBSERVE);
  Q_TT_HANDLE = MAKE_CONSTANT(TT_HANDLE);
  Q_TT_CATEGORY_LAST = MAKE_CONSTANT(TT_CATEGORY_LAST);

  Q_TT_PROCEDURE = MAKE_CONSTANT(TT_PROCEDURE);
  Q_TT_OBJECT = MAKE_CONSTANT(TT_OBJECT);
  Q_TT_HANDLER = MAKE_CONSTANT(TT_HANDLER);
  Q_TT_OTYPE = MAKE_CONSTANT(TT_OTYPE);
  Q_TT_ADDRESS_LAST = MAKE_CONSTANT(TT_ADDRESS_LAST);

  Q_TT_CREATED = MAKE_CONSTANT(TT_CREATED);
  Q_TT_SENT = MAKE_CONSTANT(TT_SENT);
  Q_TT_HANDLED = MAKE_CONSTANT(TT_HANDLED);
  Q_TT_FAILED = MAKE_CONSTANT(TT_FAILED);
  Q_TT_QUEUED = MAKE_CONSTANT(TT_QUEUED);
  Q_TT_STARTED = MAKE_CONSTANT(TT_STARTED);
  Q_TT_REJECTED = MAKE_CONSTANT(TT_REJECTED);
  Q_TT_STATE_LAST = MAKE_CONSTANT(TT_STATE_LAST);

  Q_TT_DISCARD = MAKE_CONSTANT(TT_DISCARD);
  Q_TT_QUEUE = MAKE_CONSTANT(TT_QUEUE);
  Q_TT_START = MAKE_CONSTANT(TT_START);


#undef MAKE_CONSTANT

  pure_put (Qtooltalk_error, Qerror_conditions,
	    list2 (Qtooltalk_error, Qerror));
  pure_put (Qtooltalk_error, Qerror_message,
	    build_string (DEFER_GETTEXT ("ToolTalk error")));
}

