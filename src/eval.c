/* Evaluator for GNU Emacs Lisp interpreter.
   Copyright (C) 1985-1993 Free Software Foundation, Inc.

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

/* Debugging hack */
int always_gc;


#include "config.h"
#include "lisp.h"
#include <stdio.h>
#include <setjmp.h>

#ifdef HAVE_X_WINDOWS
#include "blockio.h"
#endif

#ifndef standalone
#include "commands.h"
#else
#define INTERACTIVE 1
#endif

#include "backtrace.h"

struct backtrace *backtrace_list;
struct catchtag *catchlist;

Lisp_Object Qautoload, Qmacro, Qexit, Qinteractive, Qcommandp, Qdefun;
Lisp_Object Vquit_flag, Vinhibit_quit, Qinhibit_quit;
Lisp_Object Qmocklisp_arguments, Vmocklisp_arguments, Qmocklisp;
Lisp_Object Qand_rest, Qand_optional;
Lisp_Object Qeval;
Lisp_Object Qdebug_on_error;

Lisp_Object Vrun_hooks;

/* Non-nil means record all fset's and provide's, to be undone
   if the file being autoloaded is not fully loaded.
   They are recorded by being consed onto the front of Vautoload_queue:
   (FUN . ODEF) for a defun, (OFEATURES . nil) for a provide.  */

Lisp_Object Vautoload_queue;

/* Current number of specbindings allocated in specpdl.  */
static int specpdl_size;

/* Pointer to beginning of specpdl.  */
struct specbinding *specpdl;

/* Pointer to first unused element in specpdl.  */
struct specbinding *specpdl_ptr;

/* specpdl_ptr - specpdl.  */
int specpdl_depth;

/* Maximum size allowed for specpdl allocation */
int max_specpdl_size;

/* Depth in Lisp evaluations and function calls.  */
int lisp_eval_depth;

/* Maximum allowed depth in Lisp evaluations and function calls.  */
int max_lisp_eval_depth;

/* Nonzero means enter debugger before next function call */
int debug_on_next_call;

/* List of conditions (non-nil atom means all) which cause a backtrace
   if an error is handled by the command loop's error handler.  */
Lisp_Object Vstack_trace_on_error;

/* List of conditions (non-nil atom means all) which enter the debugger
   if an error is handled by the command loop's error handler.  */
Lisp_Object Vdebug_on_error;

/* Nonzero means enter debugger if a quit signal
 is handled by the command loop's error handler. */
int debug_on_quit;

/* Nonzero means we are trying to enter the debugger.
   This is to prevent recursive attempts.  */
int entering_debugger;

Lisp_Object Vdebugger;

static Lisp_Object funcall_lambda (Lisp_Object fn, 
                                   int nargs, Lisp_Object *args);

static Lisp_Object apply_lambda (Lisp_Object fun, Lisp_Object args, 
                                 int evalflag);


extern Lisp_Object ml_apply ();    /* Apply a mocklisp function to
				      unevaluated argument list */



void
init_eval_once ()
{
  specpdl_size = 50;
  specpdl_depth = 0;
  specpdl = (struct specbinding *)
    xmalloc (specpdl_size * sizeof (struct specbinding));
  max_specpdl_size = 600;
  max_lisp_eval_depth = 200;
}

void
init_eval ()
{
  specpdl_ptr = specpdl;
  specpdl_depth = 0;
  catchlist = 0;
  handlerlist = 0;
  backtrace_list = 0;
  Vquit_flag = Qnil;
  debug_on_next_call = 0;
  lisp_eval_depth = 0;
  entering_debugger = 0;
}

static Lisp_Object
call_debugger (arg)
     Lisp_Object arg;
{
  if (lisp_eval_depth + 20 > max_lisp_eval_depth)
    max_lisp_eval_depth = lisp_eval_depth + 20;
  if (specpdl_size + 40 > max_specpdl_size)
    max_specpdl_size = specpdl_size + 40;
  debug_on_next_call = 0;
  entering_debugger = 1;
  return apply1 (Vdebugger, arg);
}

static void
do_debug_on_call (code)
     Lisp_Object code;
{
  debug_on_next_call = 0;
  backtrace_list->debug_on_exit = 1;
  call_debugger (list1 (code));
}

static int
wants_debugger (list, conditions)
     Lisp_Object list, conditions;
{
  if (NILP (list))
    return 0;
  if (! CONSP (list))
    return 1;

  while (CONSP (conditions))
    {
      Lisp_Object this, tail;
      this = XCONS (conditions)->car;
      for (tail = list; CONSP (tail); tail = XCONS (tail)->cdr)
	if (EQ (XCONS (tail)->car, this))
	  return 1;
      conditions = XCONS (conditions)->cdr;
    }
  return 0;
}

/* Returns Qunbound if didn't call debugger */
static Lisp_Object
signal_call_debugger (conditions, sig, data)
     Lisp_Object conditions, sig, data;
{
  if (wants_debugger (Vstack_trace_on_error, conditions))
    internal_with_output_to_temp_buffer ("*Backtrace*", 
                                         Fbacktrace,
                                         Qnil,
                                         Qnil);
  if (!entering_debugger
      && (EQ (sig, Qquit)
          ? debug_on_quit
          : wants_debugger (Vdebug_on_error, conditions)))
  {
    Lisp_Object val;
    int count = specpdl_depth;

    specbind (Qdebug_on_error, Qnil);
    val = call_debugger (list2 (Qerror, (Fcons (sig, data))));
    return unbind_to (count, val);
  }
  /* Tell caller that debugger wasn't called */
  return (Qunbound);
}



/* NOTE!!! Every function that can call EVAL must protect its args
   and temporaries from garbage collection while it needs them.
   The definition of `For' shows what you have to do.  */

DEFUN ("or", For, Sor, 0, UNEVALLED, 0,
  "Eval args until one of them yields non-nil, then return that value.\n\
The remaining args are not evalled at all.\n\
If all args return nil, return nil.")
  (args)
     Lisp_Object args;
{
  register Lisp_Object val;
  Lisp_Object args_left;
  struct gcpro gcpro1;

  if (NILP (args))
    return Qnil;

  args_left = args;
  GCPRO1 (args_left);

  do
    {
      val = Feval (Fcar (args_left));
      if (!NILP (val))
	break;
      args_left = Fcdr (args_left);
    }
  while (!NILP (args_left));

  UNGCPRO;
  return val;
}

DEFUN ("and", Fand, Sand, 0, UNEVALLED, 0,
  "Eval args until one of them yields nil, then return nil.\n\
The remaining args are not evalled at all.\n\
If no arg yields nil, return the last arg's value.")
  (args)
     Lisp_Object args;
{
  register Lisp_Object val;
  Lisp_Object args_left;
  struct gcpro gcpro1;

  if (NILP (args))
    return Qt;

  args_left = args;
  GCPRO1 (args_left);

  do
    {
      val = Feval (Fcar (args_left));
      if (NILP (val))
	break;
      args_left = Fcdr (args_left);
    }
  while (!NILP (args_left));

  UNGCPRO;
  return val;
}

DEFUN ("if", Fif, Sif, 2, UNEVALLED, 0,
  "(if COND THEN ELSE...): if COND yields non-nil, do THEN, else do ELSE...\n\
Returns the value of THEN or the value of the last of the ELSE's.\n\
THEN must be one expression, but ELSE... can be zero or more expressions.\n\
If COND yields nil, and there are no ELSE's, the value is nil.")
  (args)
     Lisp_Object args;
{
  register Lisp_Object cond;
  struct gcpro gcpro1;

  GCPRO1 (args);
  cond = Feval (Fcar (args));
  UNGCPRO;

  if (!NILP (cond))
    return Feval (Fcar (Fcdr (args)));
  return Fprogn (Fcdr (Fcdr (args)));
}

DEFUN ("cond", Fcond, Scond, 0, UNEVALLED, 0,
  "(cond CLAUSES...): try each clause until one succeeds.\n\
Each clause looks like (CONDITION BODY...).  CONDITION is evaluated\n\
and, if the value is non-nil, this clause succeeds:\n\
then the expressions in BODY are evaluated and the last one's\n\
value is the value of the cond-form.\n\
If no clause succeeds, cond returns nil.\n\
If a clause has one element, as in (CONDITION),\n\
CONDITION's value if non-nil is returned from the cond-form.")
  (args)
     Lisp_Object args;
{
  register Lisp_Object clause, val;
  struct gcpro gcpro1;

  val = Qnil;
  GCPRO1 (args);
  while (!NILP (args))
    {
      clause = Fcar (args);
      val = Feval (Fcar (clause));
      if (!NILP (val))
	{
	  if (!EQ (XCONS (clause)->cdr, Qnil))
	    val = Fprogn (XCONS (clause)->cdr);
	  break;
	}
      args = XCONS (args)->cdr;
    }
  UNGCPRO;

  return val;
}

DEFUN ("progn", Fprogn, Sprogn, 0, UNEVALLED, 0,
  "(progn BODY...): eval BODY forms sequentially and return value of last one.")
  (args)
     Lisp_Object args;
{
  register Lisp_Object val, tem;
  Lisp_Object args_left;
  struct gcpro gcpro1;

  /* In Mocklisp code, symbols at the front of the progn arglist
   are to be bound to zero. */
  if (!EQ (Vmocklisp_arguments, Qt))
    {
      val = make_number (0);
      while (!NILP (args) && (tem = Fcar (args), SYMBOLP (tem)))
	{
	  QUIT;
	  specbind (tem, val), args = Fcdr (args);
	}
    }

  if (NILP (args))
    return Qnil;

  args_left = args;
  GCPRO1 (args_left);

  do
    {
      val = Feval (Fcar (args_left));
      args_left = Fcdr (args_left);
    }
  while (!NILP (args_left));

  UNGCPRO;
  return val;
}

DEFUN ("prog1", Fprog1, Sprog1, 1, UNEVALLED, 0,
  "(prog1 FIRST BODY...): eval FIRST and BODY sequentially; value from FIRST.\n\
The value of FIRST is saved during the evaluation of the remaining args,\n\
whose values are discarded.")
  (args)
     Lisp_Object args;
{
  Lisp_Object val;
  register Lisp_Object args_left;
  struct gcpro gcpro1, gcpro2;
  register int argnum = 0;

  if (NILP (args))
    return Qnil;

  args_left = args;
  val = Qnil;
  GCPRO2 (args, val);

  do
    {
      if (!(argnum++))
        val = Feval (Fcar (args_left));
      else
	Feval (Fcar (args_left));
      args_left = Fcdr (args_left);
    }
  while (!NILP (args_left));

  UNGCPRO;
  return val;
}

DEFUN ("prog2", Fprog2, Sprog2, 2, UNEVALLED, 0,
  "(prog1 X Y BODY...): eval X, Y and BODY sequentially; value from Y.\n\
The value of Y is saved during the evaluation of the remaining args,\n\
whose values are discarded.")
  (args)
     Lisp_Object args;
{
  Lisp_Object val;
  register Lisp_Object args_left;
  struct gcpro gcpro1, gcpro2;
  register int argnum = -1;

  val = Qnil;

  if (NILP (args))
    return Qnil;

  args_left = args;
  val = Qnil;
  GCPRO2 (args, val);

  do
    {
      if (!(argnum++))
        val = Feval (Fcar (args_left));
      else
	Feval (Fcar (args_left));
      args_left = Fcdr (args_left);
    }
  while (!NILP (args_left));

  UNGCPRO;
  return val;
}

DEFUN ("setq", Fsetq, Ssetq, 0, UNEVALLED, 0,
  "(setq SYM VAL SYM VAL ...): set each SYM to the value of its VAL.\n\
The SYMs are not evaluated.  Thus (setq x y) sets x to the value of y.\n\
Each SYM is set before the next VAL is computed.")
  (args)
     Lisp_Object args;
{
  register Lisp_Object args_left;
  register Lisp_Object val, sym;
  struct gcpro gcpro1;

  if (NILP (args))
    return Qnil;

  args_left = args;
  GCPRO1 (args);

  do
    {
      val = Feval (Fcar (Fcdr (args_left)));
      sym = Fcar (args_left);
      Fset (sym, val);
      args_left = Fcdr (Fcdr (args_left));
    }
  while (!NILP (args_left));

  UNGCPRO;
  return val;
}
     
DEFUN ("quote", Fquote, Squote, 1, UNEVALLED, 0,
  "Return the argument, without evaluating it.  `(quote x)' yields `x'.")
  (args)
     Lisp_Object args;
{
  return Fcar (args);
}
     
DEFUN ("function", Ffunction, Sfunction, 1, UNEVALLED, 0,
  "Like `quote', but preferred for objects which are functions.\n\
In byte compilation, `function' causes its argument to be compiled.\n\
`quote' cannot do that.")
  (args)
     Lisp_Object args;
{
  return Fcar (args);
}

#ifndef standalone
DEFUN ("interactive-p", Finteractive_p, Sinteractive_p, 0, 0, 0,
  "Return t if function in which this appears was called interactively.\n\
This means that the function was called with call-interactively (which\n\
includes being called as the binding of a key)\n\
and input is currently coming from the keyboard (not in keyboard macro).")
  ()
{
  register struct backtrace *btp;
  register Lisp_Object fun;

  if (!INTERACTIVE)
    return Qnil;

  /*  Unless the object was compiled, skip the frame of interactive-p itself
      (if interpreted) or the frame of byte-code (if called from
      compiled function).  */
  btp = backtrace_list;
  if (! (COMPILEDP (*btp->function)))
    btp = btp->next;
  for (;
       btp && (btp->nargs == UNEVALLED
	       || EQ (*btp->function, Qbytecode));
       btp = btp->next)
    {}
  /* btp now points at the frame of the innermost function
     that DOES eval its args.
     If it is a built-in function (such as load or eval-region)
     return nil.  */
  fun = Findirect_function (*btp->function);
  /* Beats me why this is necessary, but it is */
  if (btp && EQ (*btp->function, Qcall_interactively))
    return Qt;
  if (SUBRP (fun))
    return Qnil;
  /* btp points to the frame of a Lisp function that called interactive-p.
     Return t if that function was called interactively.  */
  if (btp && btp->next && EQ (*btp->next->function, Qcall_interactively))
    return Qt;
  return Qnil;
}
#endif /* not standalone */

DEFUN ("defun", Fdefun, Sdefun, 2, UNEVALLED, 0,
  "(defun NAME ARGLIST [DOCSTRING] BODY...): define NAME as a function.\n\
The definition is (lambda ARGLIST [DOCSTRING] BODY...).\n\
See also the function `interactive'.")
  (args)
     Lisp_Object args;
{
  register Lisp_Object fn_name;
  register Lisp_Object defn;

  fn_name = Fcar (args);
  defn = Fcons (Qlambda, Fcdr (args));
  if (!NILP (Vpurify_flag))
    defn = Fpurecopy (defn);
  Ffset (fn_name, defn);
  return fn_name;
}

DEFUN ("defmacro", Fdefmacro, Sdefmacro, 2, UNEVALLED, 0,
  "(defmacro NAME ARGLIST [DOCSTRING] BODY...): define NAME as a macro.\n\
The definition is (macro lambda ARGLIST [DOCSTRING] BODY...).\n\
When the macro is called, as in (NAME ARGS...),\n\
the function (lambda ARGLIST BODY...) is applied to\n\
the list ARGS... as it appears in the expression,\n\
and the result should be a form to be evaluated instead of the original.")
  (args)
     Lisp_Object args;
{
  register Lisp_Object fn_name;
  register Lisp_Object defn;

  fn_name = Fcar (args);
  defn = Fcons (Qmacro, Fcons (Qlambda, Fcdr (args)));
  if (!NILP (Vpurify_flag))
    defn = Fpurecopy (defn);
  Ffset (fn_name, defn);
  return fn_name;
}

DEFUN ("defvar", Fdefvar, Sdefvar, 1, UNEVALLED, 0,
  "(defvar SYMBOL INITVALUE DOCSTRING): define SYMBOL as a variable.\n\
You are not required to define a variable in order to use it,\n\
but the definition can supply documentation and an initial value\n\
in a way that tags can recognize.\n\n\
INITVALUE is evaluated, and used to set SYMBOL, only if SYMBOL's value is void.\n\
If SYMBOL is buffer-local, its default value is what is set;\n\
 buffer-local values are not affected.\n\
INITVALUE and DOCSTRING are optional.\n\
If DOCSTRING starts with *, this variable is identified as a user option.\n\
 This means that M-x set-variable and M-x edit-options recognize it.\n\
If INITVALUE is missing, SYMBOL's value is not set.")
  (args)
     Lisp_Object args;
{
  register Lisp_Object sym, tem;

  sym = Fcar (args);
  tem = Fcdr (args);
  if (!NILP (tem))
    {
      tem = Fdefault_boundp (sym);
      if (NILP (tem))
	Fset_default (sym, Feval (Fcar (Fcdr (args))));
    }
  tem = Fcar (Fcdr (Fcdr (args)));
  if (!NILP (tem))
    {
      if (!NILP (Vpurify_flag))
	tem = Fpurecopy (tem);
      Fput (sym, Qvariable_documentation, tem);
    }
  return sym;
}

DEFUN ("defconst", Fdefconst, Sdefconst, 2, UNEVALLED, 0,
  "(defconst SYMBOL INITVALUE DOCSTRING): define SYMBOL as a constant variable.\n\
The intent is that programs do not change this value, but users may.\n\
Always sets the value of SYMBOL to the result of evalling INITVALUE.\n\
If SYMBOL is buffer-local, its default value is what is set;\n\
 buffer-local values are not affected.\n\
DOCSTRING is optional.\n\
If DOCSTRING starts with *, this variable is identified as a user option.\n\
 This means that M-x set-variable and M-x edit-options recognize it.\n\n\
Note: do not use `defconst' for user options in libraries that are not\n\
normally loaded, since it is useful for users to be able to specify\n\
their own values for such variables before loading the library.\n\
Since `defconst' unconditionally assigns the variable,\n\
it would override the user's choice.")
  (args)
     Lisp_Object args;
{
  register Lisp_Object sym, tem;

  sym = Fcar (args);
  Fset_default (sym, Feval (Fcar (Fcdr (args))));
  tem = Fcar (Fcdr (Fcdr (args)));
  if (!NILP (tem))
    {
      if (!NILP (Vpurify_flag))
	tem = Fpurecopy (tem);
      Fput (sym, Qvariable_documentation, tem);
    }
  return sym;
}

DEFUN ("user-variable-p", Fuser_variable_p, Suser_variable_p, 1, 1, 0,
  "Returns t if VARIABLE is intended to be set and modified by users.\n\
\(The alternative is a variable used internally in a Lisp program.)\n\
Determined by whether the first character of the documentation\n\
for the variable is \"*\"")
  (variable)
     Lisp_Object variable;
{
  Lisp_Object documentation;
  
  documentation = Fget (variable, Qvariable_documentation);
  if (FIXNUMP (documentation) && XINT (documentation) < 0)
    return Qt;
  if ((STRINGP (documentation)) &&
      ((unsigned char) XSTRING (documentation)->data[0] == '*'))
    return Qt;
  return Qnil;
}  

DEFUN ("let*", FletX, SletX, 1, UNEVALLED, 0,
  "(let* VARLIST BODY...): bind variables according to VARLIST then eval BODY.\n\
The value of the last form in BODY is returned.\n\
Each element of VARLIST is a symbol (which is bound to nil)\n\
or a list (SYMBOL VALUEFORM) (which binds SYMBOL to the value of VALUEFORM).\n\
Each VALUEFORM can refer to the symbols already bound by this VARLIST.")
  (args)
     Lisp_Object args;
{
  Lisp_Object varlist, val, elt;
  int count = specpdl_depth;
  struct gcpro gcpro1, gcpro2, gcpro3;

  GCPRO3 (args, elt, varlist);

  varlist = Fcar (args);
  while (!NILP (varlist))
    {
      QUIT;
      elt = Fcar (varlist);
      if (SYMBOLP (elt))
	specbind (elt, Qnil);
      else if (! NILP (Fcdr (Fcdr (elt))))
	signal_error (Qerror,
                      list2 (build_string (
			     "`let' bindings can have only one value-form"),
                             elt));
      else
	{
	  val = Feval (Fcar (Fcdr (elt)));
	  specbind (Fcar (elt), val);
	}
      varlist = Fcdr (varlist);
    }
  UNGCPRO;
  val = Fprogn (Fcdr (args));
  return unbind_to (count, val);
}

DEFUN ("let", Flet, Slet, 1, UNEVALLED, 0,
  "(let VARLIST BODY...): bind variables according to VARLIST then eval BODY.\n\
The value of the last form in BODY is returned.\n\
Each element of VARLIST is a symbol (which is bound to nil)\n\
or a list (SYMBOL VALUEFORM) (which binds SYMBOL to the value of VALUEFORM).\n\
All the VALUEFORMs are evalled before any symbols are bound.")
  (args)
     Lisp_Object args;
{
  Lisp_Object *temps, tem;
  register Lisp_Object elt, varlist;
  int count = specpdl_depth;
  register int argnum;
  struct gcpro gcpro1, gcpro2;

  varlist = Fcar (args);

  /* Make space to hold the values to give the bound variables */
  elt = Flength (varlist);
  temps = (Lisp_Object *) alloca (XFASTINT (elt) * sizeof (Lisp_Object));

  /* Compute the values and store them in `temps' */

  GCPRO2 (args, *temps);
  gcpro2.nvars = 0;

  for (argnum = 0; !NILP (varlist); varlist = Fcdr (varlist))
    {
      QUIT;
      elt = Fcar (varlist);
      if (SYMBOLP (elt))
	temps [argnum++] = Qnil;
      else if (! NILP (Fcdr (Fcdr (elt))))
	signal_error (Qerror,
                      list2 (build_string (
			     "`let' bindings can have only one value-form"),
                             elt));
      else
	temps [argnum++] = Feval (Fcar (Fcdr (elt)));
      gcpro2.nvars = argnum;
    }
  UNGCPRO;

  varlist = Fcar (args);
  for (argnum = 0; !NILP (varlist); varlist = Fcdr (varlist))
    {
      elt = Fcar (varlist);
      tem = temps[argnum++];
      if (SYMBOLP (elt))
	specbind (elt, tem);
      else
	specbind (Fcar (elt), tem);
    }

  elt = Fprogn (Fcdr (args));
  return unbind_to (count, elt);
}

DEFUN ("while", Fwhile, Swhile, 1, UNEVALLED, 0,
  "(while TEST BODY...): if TEST yields non-nil, eval BODY... and repeat.\n\
The order of execution is thus TEST, BODY, TEST, BODY and so on\n\
until TEST returns nil.")
  (args)
     Lisp_Object args;
{
  Lisp_Object test, body, tem;
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (test, body);

  test = Fcar (args);
  body = Fcdr (args);
  while (tem = Feval (test), !NILP (tem))
    {
      QUIT;
      Fprogn (body);
    }

  UNGCPRO;
  return Qnil;
}

DEFUN ("macroexpand", Fmacroexpand, Smacroexpand, 1, 2, 0,
  "Return result of expanding macros at top level of FORM.\n\
If FORM is not a macro call, it is returned unchanged.\n\
Otherwise, the macro is expanded and the expansion is considered\n\
in place of FORM.  When a non-macro-call results, it is returned.\n\n\
The second optional arg ENVIRONMENT species an environment of macro\n\
definitions to shadow the loaded ones for use in file byte-compilation.")
  (form, env)
     register Lisp_Object form;
     Lisp_Object env;
{
  /* With cleanups from Hallvard Furuseth.  */
  register Lisp_Object expander, sym, def, tem;

  while (1)
    {
      /* Come back here each time we expand a macro call,
	 in case it expands into another macro call.  */
      if (!CONSP (form))
	break;
      /* Set SYM, give DEF and TEM right values in case SYM is not a symbol. */
      def = sym = XCONS (form)->car;
      tem = Qnil;
      /* Trace symbols aliases to other symbols
	 until we get a symbol that is not an alias.  */
      while (SYMBOLP (def))
	{
	  QUIT;
	  sym = def;
	  tem = Fassq (sym, env);
	  if (NILP (tem))
	    {
	      def = XSYMBOL (sym)->function;
	      if (!EQ (def, Qunbound))
		continue;
	    }
	  break;
	}
      /* Right now TEM is the result from SYM in ENV,
	 and if TEM is nil then DEF is SYM's function definition.  */
      if (NILP (tem))
	{
	  /* SYM is not mentioned in ENV.
	     Look at its function definition.  */
	  if (EQ (def, Qunbound)
	      || !CONSP (def))
	    /* Not defined or definition not suitable */
	    break;
	  if (EQ (XCONS (def)->car, Qautoload))
	    {
	      /* Autoloading function: will it be a macro when loaded?  */
	      tem = Fcar (Fnthcdr (make_number (4), def));
	      if (NILP (tem))
		break;
	      /* Yes, load it and try again.  */
	      do_autoload (def, sym);
	      continue;
	    }
	  else if (!EQ (XCONS (def)->car, Qmacro))
	    break;
	  else expander = XCONS (def)->cdr;
	}
      else
	{
	  expander = XCONS (tem)->cdr;
	  if (NILP (expander))
	    break;
	}
      form = apply1 (expander, XCONS (form)->cdr);
    }
  return form;
}

DEFUN ("catch", Fcatch, Scatch, 1, UNEVALLED, 0,
  "(catch TAG BODY...): eval BODY allowing nonlocal exits using `throw'.\n\
TAG is evalled to get the tag to use.  Then the BODY is executed.\n\
Within BODY, (throw TAG) with same tag exits BODY and exits this `catch'.\n\
If no throw happens, `catch' returns the value of the last BODY form.\n\
If a throw happens, it specifies the value to return from `catch'.")
  (args)
     Lisp_Object args;
{
  register Lisp_Object tag;
  struct gcpro gcpro1;

  GCPRO1 (args);
  tag = Feval (Fcar (args));
  UNGCPRO;
  return internal_catch (tag, Fprogn, Fcdr (args));
}

/* Set up a catch, then call C function FUNC on argument ARG.
   FUNC should return a Lisp_Object.
   This is how catches are done from within C code. */

Lisp_Object
internal_catch (tag, func, arg)
     Lisp_Object tag;
     Lisp_Object (*func) (Lisp_Object);
     Lisp_Object arg;
{
  /* This structure is made part of the chain `catchlist'.  */
  struct catchtag c;

  /* Fill in the components of c, and put it on the list.  */
  c.next = catchlist;
  c.tag = tag;
  c.val = Qnil;
  c.backlist = backtrace_list;
  c.handlerlist = handlerlist;
  c.lisp_eval_depth = lisp_eval_depth;
  c.pdlcount = specpdl_depth;
  c.gcpro = gcprolist;
  catchlist = &c;

  /* Call FUNC.  */
  if (! _setjmp (c.jmp))
    c.val = (*func) (arg);

  /* Throw works by a longjmp that comes right here.  */
  catchlist = c.next;
  return c.val;
}

/* Unwind the specbind, catch, and handler stacks back to CATCH, and
   jump to that CATCH, returning VALUE as the value of that catch.

   This is the guts Fthrow and Fsignal; they differ only in the way
   they choose the catch tag to throw to.  A catch tag for a
   condition-case form has a TAG of Qnil.

   Before each catch is discarded, unbind all special bindings and
   execute all unwind-protect clauses made above that catch.  Unwind
   the handler stack as we go, so that the proper handlers are in
   effect for each unwind-protect clause we run.  At the end, restore
   some static info saved in CATCH, and longjmp to the location
   specified in the

   This is used for correct unwinding in Fthrow and Fsignal.  */

static void
unwind_to_catch (catch, value)
     struct catchtag *catch;
     Lisp_Object value;
{
  register int last_time;

  /* Save the value in the tag.  */
  catch->val = value;

  do
    {
      last_time = catchlist == catch;
      /* Unwind the specpdl stack, and then restore the proper set of
         handlers.  */
      unbind_to (catchlist->pdlcount, Qnil);
      handlerlist = catchlist->handlerlist;
      catchlist = catchlist->next;
    }
  while (! last_time);

  gcprolist = catch->gcpro;
  backtrace_list = catch->backlist;
  lisp_eval_depth = catch->lisp_eval_depth;
  _longjmp (catch->jmp, 1);
}

DEFUN ("throw", Fthrow, Sthrow, 2, 2, 0,
  "(throw TAG VALUE): throw to the catch for TAG and return VALUE from it.\n\
Both TAG and VALUE are evalled.")
  (tag, val)
     register Lisp_Object tag, val;
{
  register struct catchtag *c;

  while (1)
    {
      if (!NILP (tag))
	for (c = catchlist; c; c = c->next)
	  {
	    if (EQ (c->tag, tag))
	      unwind_to_catch (c, val);
	  }
      tag = Fsignal (Qno_catch, list2 (tag, val));
    }
  /* getting tired of compilation warnings */
  return Qnil;
}


DEFUN ("unwind-protect", Funwind_protect, Sunwind_protect, 1, UNEVALLED, 0,
  "Do BODYFORM, protecting with UNWINDFORMS.\n\
Usage looks like (unwind-protect BODYFORM UNWINDFORMS...).\n\
If BODYFORM completes normally, its value is returned\n\
after executing the UNWINDFORMS.\n\
If BODYFORM exits nonlocally, the UNWINDFORMS are executed anyway.")
  (args)
     Lisp_Object args;
{
  Lisp_Object val;
  int count = specpdl_depth;

  record_unwind_protect (0, Fcdr (args));
  val = Feval (Fcar (args));
  return unbind_to (count, val);  
}

/* Chain of condition handlers currently in effect.
   The elements of this chain are contained in the stack frames
   of Fcondition_case and internal_condition_case.
   When an error is signaled (by calling Fsignal, below),
   this chain is searched for an element that applies.  */

struct handler *handlerlist;

/* Split out from condition_case_3 so that primitive C callers
   don't have to cons up a lisp handler form to be evaluated. */
Lisp_Object
condition_case_1 (Lisp_Object handlers,
                  Lisp_Object (*bfun) (Lisp_Object barg),
                  Lisp_Object barg,
                  Lisp_Object (*hfun) (Lisp_Object val, Lisp_Object harg),
                  Lisp_Object harg)
{
  struct catchtag c;
  struct handler h;

  c.tag = Qnil;
  c.val = Qnil;
  c.backlist = backtrace_list;
  c.handlerlist = handlerlist;
  c.lisp_eval_depth = lisp_eval_depth;
  c.pdlcount = specpdl_depth;
  c.gcpro = gcprolist;
  if (_setjmp (c.jmp))
    {
      return ((*hfun) (c.val, harg));
    }
  c.next = catchlist;
  catchlist = &c;
  h.handlers = handlers;
  h.handler_arg = harg;         /* Must record this for GC */
  
  h.next = handlerlist;
  h.tag = &c;
  handlerlist = &h;

  barg = ((*bfun) (barg));
  catchlist = c.next;
  handlerlist = h.next;
  return barg;
}

static Lisp_Object
run_condition_case_handlers (Lisp_Object val, Lisp_Object var)
{
  int speccount = specpdl_depth;

  if (!NILP (var))
    specbind (var, Fcar (val));
  val = Fprogn (Fcdr (val));
  return unbind_to (speccount, val);
}


/* Here for bytecode to call non-consfully */
Lisp_Object
Fcondition_case_3 (Lisp_Object bodyform, 
                   Lisp_Object var, Lisp_Object handlers)
{
  Lisp_Object val;

  CHECK_SYMBOL (var, 0);

  for (val = handlers; ! NILP (val); val = Fcdr (val))
    {
      Lisp_Object tem;
      tem = Fcar (val);
      if ((!NILP (tem)) 
          && (!CONSP (tem) || (!SYMBOLP (XCONS (tem)->car))))
	error ("Invalid condition handler", tem);
    }

  return condition_case_1 (handlers, 
                           Feval, bodyform,
                           run_condition_case_handlers,
                           var);
}



DEFUN ("condition-case", Fcondition_case, Scondition_case, 2, UNEVALLED, 0,
  "Regain control when an error is signaled.\n\
Usage looks like (condition-case VAR BODYFORM HANDLERS...).\n\
executes BODYFORM and returns its value if no error happens.\n\
Each element of HANDLERS looks like (CONDITION-NAME BODY...)\n\
where the BODY is made of Lisp expressions.\n\n\
A handler is applicable to an error\n\
if CONDITION-NAME is one of the error's condition names.\n\
If an error happens, the first applicable handler is run.\n\
\n\
When a handler handles an error,\n\
control returns to the condition-case and the handler BODY... is executed\n\
with VAR bound to (SIGNALED-CONDITIONS . SIGNAL-DATA).\n\
VAR may be nil; then you do not get access to the signal information.\n\
\n\
The value of the last BODY form is returned from the condition-case.\n\
See also the function `signal' for more info.")
  (args)
     Lisp_Object args;
{
  return Fcondition_case_3 (Fcar (Fcdr (args)),
                            Fcar (args),
                            Fcdr (Fcdr (args)));
} 


extern int in_display;

DEFUN ("signal", Fsignal, Ssignal, 2, 2, 0,
  "Signal an error.  Args are SIGNAL-NAME, and associated DATA.\n\
A signal name is a symbol with an `error-conditions' property\n\
that is a list of condition names.\n\
A handler for any of those names will get to handle this signal.\n\
The symbol `error' should normally be one of them.\n\
\n\
DATA should be a list.  Its elements are printed as part of the error message.\n\
If the signal is handled, DATA is made available to the handler.\n\
See also the function `condition-case'.")
  (sig, data)
     Lisp_Object sig, data;
{
  register struct handler *allhandlers = handlerlist;
  Lisp_Object conditions;
  extern int gc_in_progress;
  extern int waiting_for_input;

  immediate_quit = 0;
  if (gc_in_progress || waiting_for_input)
    abort ();

  in_display = 0;		/* Otherwise, we are hosed */
  TOTALLY_UNBLOCK_INPUT;

  conditions = Fget (sig, Qerror_conditions);

  for (; handlerlist; handlerlist = handlerlist->next)
    {
      register Lisp_Object handlers = handlerlist->handlers;

      /* t is used by handlers for all conditions, set up by C code. 
       *  debugger is not called even if debug_on_error */
      if (EQ (handlers, Qt))
      {
        struct handler *h = handlerlist;
        handlerlist = allhandlers;
        /* Doesn't return */
        unwind_to_catch (h->tag, Fcons (sig, data));
      }
      /* >>> compatibility kludge for C command-loop in keyboard.c */
      /* error is used similarly, but means display a backtrace too */
      else if (EQ (handlers, Qerror))
      {
        Lisp_Object tem;
        tem = signal_call_debugger (conditions, sig, data);
        if (EQ (tem, Qunbound))
        {
          struct handler *h = handlerlist;
          handlerlist = allhandlers;
          /* Doesn't return */
          unwind_to_catch (h->tag, Fcons (sig, data));
        }
        else
          /* Have called debugger; return value to signaller  */
          return (tem);
      }
      else
      {
        register Lisp_Object h;

        for (h = handlers; CONSP (h); h = Fcdr (h))
        {
          Lisp_Object clause = Fcar (h);
          Lisp_Object tem = Fcar (clause);
          if (!EQ (tem, Qt))
            /* (condition-case c # (t c)) catches -all- signals
             *   Use with caution! */
            tem = Fmemq (tem, conditions);
          if (!NILP (tem))
          
          {
            Lisp_Object tem;
            tem = signal_call_debugger (conditions, sig, data);
            if (EQ (tem, Qunbound))
            {
              struct handler *h = handlerlist;
              handlerlist = allhandlers;
              /* Doesn't return */
              unwind_to_catch (h->tag, Fcons (Fcons (sig, data),
                                              Fcdr (clause)));
            }
            else
#if 1                           /* RMS Claims: */
              /* Most callers are not prepared to handle gc if this returns.
                 So, since this feature is not very useful, take it out.  */
              /* Have called debugger; return value to signaller  */
              return (tem);
#else  /* But I (Mly) claim to have fixed all callers -- in Emacs "Lucid+Mlynarik 19.3", that is */
            error ("Returning a value from an error is no longer supported");
#endif
          }
        }
      }
    }

  handlerlist = allhandlers;
  /* If no handler is present now, try to run the debugger,
     and if that fails, throw to top level.  */
  signal_call_debugger (conditions, sig, data);
  return Fthrow (Qtop_level, Qt);
}

/* Utility function.  Doesn't return. */
void
signal_error (Lisp_Object sig, Lisp_Object data)
{
  while (1)
    Fsignal (sig, data);
}


/* dump an error message; called like printf */

/* VARARGS 1 */
void
error (m, a1, a2, a3)
     char *m;
     void *a1, *a2, *a3;
{
  char buf[200];
  sprintf (buf, m, a1, a2, a3);

  while (1)
    Fsignal (Qerror, Fcons (build_string (buf), Qnil));
}

#ifndef standalone
DEFUN ("commandp", Fcommandp, Scommandp, 1, 1, 0,
  "T if FUNCTION makes provisions for interactive calling.\n\
This means it contains a description for how to read arguments to give it.\n\
The value is nil for an invalid function or a symbol with no function\n\
definition.\n\
\n\
Interactively callable functions include strings and vectors (treated\n\
as keyboard macros), lambda-expressions that contain a top-level call\n\
to `interactive', autoload definitions made by `autoload' with non-nil\n\
fourth argument, and some of the built-in functions of Lisp.\n\
\n\
Also, a symbol satisfies `commandp' if its function definition does so.")
  (function)
     Lisp_Object function;
{
  register Lisp_Object fun;
  register Lisp_Object funcar;

  fun = function;

  fun = indirect_function (fun, 0);
  if (EQ (fun, Qunbound))
    return Qnil;

  /* Emacs primitives are interactive if their DEFUN specifies an
     interactive spec.  */
  if (SUBRP (fun))
    {
      if (XSUBR (fun)->prompt)
	return Qt;
      else
	return Qnil;
    }

  /* Bytecode objects are interactive if they are long enough to
     have an element whose index is COMPILED_INTERACTIVE, which is
     where the interactive spec is stored.  */
  else if (COMPILEDP (fun))
    return (XVECTOR (fun)->size > COMPILED_INTERACTIVE
	    ? Qt : Qnil);

  /* Strings and vectors are keyboard macros.  */
  if (VECTORP (fun) || STRINGP (fun))
    return Qt;

  /* Lists may represent commands.  */
  if (!CONSP (fun))
    return Qnil;
  funcar = Fcar (fun);
  if (!SYMBOLP (funcar))
    return Fsignal (Qinvalid_function, list1 (fun));
  if (EQ (funcar, Qlambda))
    return Fassq (Qinteractive, Fcdr (Fcdr (fun)));
  if (EQ (funcar, Qmocklisp))
    return Qt;  /* All mocklisp functions can be called interactively */
  if (EQ (funcar, Qautoload))
    return Fcar (Fcdr (Fcdr (Fcdr (fun))));
  else
    return Qnil;
}
#endif /* !standalone */

/* ARGSUSED */
DEFUN ("autoload", Fautoload, Sautoload, 2, 5, 0,
  "Define FUNCTION to autoload from FILE.\n\
FUNCTION is a symbol; FILE is a file name string to pass to `load'.\n\
Third arg DOCSTRING is documentation for the function.\n\
Fourth arg INTERACTIVE if non-nil says function can be called interactively.\n\
Fifth arg MACRO if non-nil says the function is really a macro.\n\
Third through fifth args give info about the real definition.\n\
They default to nil.\n\
If FUNCTION is already defined other than as an autoload,\n\
this does nothing and returns nil.")
  (function, file, docstring, interactive, macro)
     Lisp_Object function, file, docstring, interactive, macro;
{
  CHECK_SYMBOL (function, 0);
  CHECK_STRING (file, 1);

  /* If function is defined and not as an autoload, don't override */
  if (!EQ (XSYMBOL (function)->function, Qunbound)
      && !(CONSP (XSYMBOL (function)->function)
	   && EQ (XCONS (XSYMBOL (function)->function)->car, Qautoload)))
    return Qnil;

  return Ffset (function, Fcons (Qautoload, list4 (file,
                                                   docstring,
                                                   interactive,
                                                   macro)));
}

static Lisp_Object
un_autoload (oldqueue)
     Lisp_Object oldqueue;
{
  register Lisp_Object queue, first, second;

  /* Queue to unwind is current value of Vautoload_queue.
     oldqueue is the shadowed value to leave in Vautoload_queue.  */
  queue = Vautoload_queue;
  Vautoload_queue = oldqueue;
  while (CONSP (queue))
    {
      first = Fcar (queue);
      second = Fcdr (first);
      first = Fcar (first);
      if (EQ (second, Qnil))
	Vfeatures = first;
      else
	Ffset (first, second);
      queue = Fcdr (queue);
    }
  return Qnil;
}

void
do_autoload (fundef, funname)
     Lisp_Object fundef, funname;
{
  int count = specpdl_depth;
  Lisp_Object fun;

  fun = funname;
  CHECK_SYMBOL (funname, 0);

  /* Value saved here is to be restored into Vautoload_queue */
  record_unwind_protect (un_autoload, Vautoload_queue);
  Vautoload_queue = Qt;
  Fload (Fcar (Fcdr (fundef)), Qnil, noninteractive ? Qt : Qnil, Qnil);
  /* Once loading finishes, don't undo it.  */
  Vautoload_queue = Qt;
  unbind_to (count, Qnil);

  fun = indirect_function (fun, 0);

  if (EQ (fun, Qunbound)
      || (CONSP (fun)
          && EQ (XCONS (fun)->car, Qautoload)))
    error ("Autoloading failed to define function %s",
	   XSYMBOL (funname)->name->data);
}

DEFUN ("eval", Feval, Seval, 1, 1, 0,
  "Evaluate FORM and return its value.")
  (form)
     Lisp_Object form;
{
  Lisp_Object fun, val, original_fun, original_args;
  Lisp_Object funcar;
  struct backtrace backtrace;
  struct gcpro gcpro1, gcpro2, gcpro3;

  if (!CONSP (form))
  {
    if (!SYMBOLP (form))
      return form;
    if (EQ (Vmocklisp_arguments, Qt))
      return Fsymbol_value (form);
    val = Fsymbol_value (form);
    if (NILP (val))
      XFASTINT (val) = 0;
    else if (EQ (val, Qt))
      XFASTINT (val) = 1;
    return val;
  }

  QUIT;
  if ((consing_since_gc > gc_cons_threshold) || always_gc)
    {
      GCPRO1 (form);
      Fgarbage_collect ();
      UNGCPRO;
    }

  if (++lisp_eval_depth > max_lisp_eval_depth)
    {
      if (max_lisp_eval_depth < 100)
	max_lisp_eval_depth = 100;
      if (lisp_eval_depth > max_lisp_eval_depth)
	error ("Lisp nesting exceeds max-lisp-eval-depth", 0, 0, 0);
    }

  original_fun = Fcar (form);
  original_args = Fcdr (form);

#ifdef EMACS_BTL
  backtrace.id_number = 0;
#endif
  backtrace.next = backtrace_list;
  backtrace_list = &backtrace;
  backtrace.function = &original_fun; /* This also protects them from gc */
  backtrace.args = &original_args;
  backtrace.nargs = UNEVALLED;
  backtrace.evalargs = 1;
  backtrace.debug_on_exit = 0;

  if (debug_on_next_call)
    do_debug_on_call (Qt);

  /* At this point, only original_fun and original_args
     have values that will be used below */
 retry:
  fun = indirect_function (original_fun, 1);

  if (SUBRP (fun))
    {
      struct Lisp_Subr *subr = XSUBR (fun);
      int max_args = subr->max_args;
      Lisp_Object (*fn) () = subr_function (subr);
      Lisp_Object argvals[SUBR_MAX_ARGS];
      Lisp_Object args_left;
      Lisp_Object nargs = XINT (Flength (original_args));
      register int i;

      args_left = original_args;

      if (nargs < subr->min_args ||
	  (max_args >= 0 && max_args < nargs))
	return Fsignal (Qwrong_number_of_arguments, 
                        list2 (fun, make_number (nargs)));

      if (max_args == UNEVALLED)
	{
	  backtrace.evalargs = 0;
	  val = ((*fn) (args_left));
	  goto done;
	}

      if (max_args == MANY)
	{
	  /* Pass a vector of evaluated arguments */
	  Lisp_Object *vals;
	  register int argnum = 0;

	  vals = (Lisp_Object *) alloca (nargs * sizeof (Lisp_Object));

	  GCPRO3 (args_left, fun, fun);
	  gcpro3.var = vals;
	  gcpro3.nvars = 0;

	  while (!NILP (args_left))
	    {
	      vals[argnum++] = Feval (Fcar (args_left));
	      args_left = Fcdr (args_left);
	      gcpro3.nvars = argnum;
	    }
	  backtrace.args = vals;
	  backtrace.nargs = nargs;

	  val = ((*fn) (nargs, vals));
          UNGCPRO;
	  goto done;
	}

      GCPRO3 (args_left, fun, fun);
      gcpro3.var = argvals;
      gcpro3.nvars = 0;

      for (i = 0; i < max_args; args_left = Fcdr (args_left))
	{
	  argvals[i] = Feval (Fcar (args_left));
	  gcpro3.nvars = ++i;
	}

      UNGCPRO;

      backtrace.args = argvals;
      backtrace.nargs = nargs;

      switch (max_args)
	{
	case 0:
	  val = ((*fn) ());
	  goto done;
	case 1:
	  val = ((*fn) (argvals[0]));
	  goto done;
	case 2:
	  val = ((*fn) (argvals[0], argvals[1]));
	  goto done;
	case 3:
	  val = ((*fn) (argvals[0], argvals[1], argvals[2]));
	  goto done;
	case 4:
	  val = ((*fn) (argvals[0], argvals[1], argvals[2], argvals[3]));
	  goto done;
	case 5:
	  val = ((*fn) (argvals[0], argvals[1], argvals[2],
                        argvals[3], argvals[4]));
	  goto done;
	case 6:
	  val = ((*fn) (argvals[0], argvals[1], argvals[2],
                        argvals[3], argvals[4], argvals[5]));
	  goto done;
	case 7:
	  val = ((*fn) (argvals[0], argvals[1], argvals[2],
                        argvals[3], argvals[4], argvals[5],
                        argvals[6]));
	  goto done;

	default:
	  /* Someone has created a subr that takes more arguments than
	     is supported by this code.  We need to either rewrite the
	     subr to use a different argument protocol, or add more
	     cases to this switch.  */
	  abort ();
	}
    }
  if (COMPILEDP (fun))
    val = apply_lambda (fun, original_args, 1);
  else
    {
      if (!CONSP (fun))
	return Fsignal (Qinvalid_function, list1 (fun));
      funcar = Fcar (fun);
      if (!SYMBOLP (funcar))
	return Fsignal (Qinvalid_function, list1 (fun));
      if (EQ (funcar, Qautoload))
	{
	  do_autoload (fun, original_fun);
	  goto retry;
	}
      if (EQ (funcar, Qmacro))
	val = Feval (apply1 (Fcdr (fun), original_args));
      else if (EQ (funcar, Qlambda))
	val = apply_lambda (fun, original_args, 1);
      else if (EQ (funcar, Qmocklisp))
	val = ml_apply (fun, original_args);
      else
	return Fsignal (Qinvalid_function, list1 (fun));
    }
 done:
  if (!EQ (Vmocklisp_arguments, Qt))
    {
      if (NILP (val))
	XFASTINT (val) = 0;
      else if (EQ (val, Qt))
	XFASTINT (val) = 1;
    }
  lisp_eval_depth--;
  if (backtrace.debug_on_exit)
    val = call_debugger (list2 (Qexit, val));
  backtrace_list = backtrace.next;
  return val;
}

DEFUN ("apply", Fapply, Sapply, 2, MANY, 0,
  "Call FUNCTION with our remaining args, using our last arg as list of args.\n\
Thus, (apply '+ 1 2 '(3 4)) returns 10.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  register int i, numargs;
  register Lisp_Object spread_arg;
  register Lisp_Object *funcall_args;
  Lisp_Object fun;
  struct gcpro gcpro1;

  fun = args [0];
  funcall_args = 0;
  spread_arg = args [nargs - 1];
  CHECK_LIST (spread_arg, nargs);
  
  numargs = XINT (Flength (spread_arg));

  if (numargs == 0)
    return Ffuncall (nargs - 1, args);
  else if (numargs == 1)
    {
      args [nargs - 1] = XCONS (spread_arg)->car;
      return Ffuncall (nargs, args);
    }

  numargs += nargs - 2;

  fun = indirect_function (fun, 0);
  if (EQ (fun, Qunbound))
    {
      /* Let funcall get the error */
      fun = args[0];
    }
  else if (SUBRP (fun))
    {
      struct Lisp_Subr *subr = XSUBR (fun);
      int max_args = subr->max_args;

      if (numargs < subr->min_args
	  || (max_args >= 0 && max_args < numargs))
        {
          /* Let funcall get the error */
        }
      else if (max_args > numargs)
	{
	  /* Avoid making funcall cons up yet another new vector of arguments
             by explicitly supplying nil's for optional values */
          funcall_args = (Lisp_Object *) alloca ((1 + max_args)
                                                 * sizeof (Lisp_Object));
          for (i = numargs; i < max_args;)
            funcall_args[++i] = Qnil;
	  GCPRO1 (*funcall_args);
	  gcpro1.nvars = 1 + max_args;
        }
    }
  /* We add 1 to numargs because funcall_args includes the
     function itself as well as its arguments.  */
  if (!funcall_args)
    {
      funcall_args = (Lisp_Object *) alloca ((1 + numargs)
					     * sizeof (Lisp_Object));
      GCPRO1 (*funcall_args);
      gcpro1.nvars = 1 + numargs;
    }

  memcpy ((char *)funcall_args, (char *)args, nargs * sizeof (Lisp_Object));
  /* Spread the last arg we got.  Its first element goes in
     the slot that it used to occupy, hence this value of I.  */
  i = nargs - 1;
  while (!NILP (spread_arg))
    {
      funcall_args [i++] = XCONS (spread_arg)->car;
      spread_arg = XCONS (spread_arg)->cdr;
    }
  RETURN_UNGCPRO (Ffuncall (gcpro1.nvars, funcall_args));
}

/* Apply fn to arg */
Lisp_Object
apply1 (fn, arg)
     Lisp_Object fn, arg;
{
  struct gcpro gcpro1;

  GCPRO1 (fn);
  if (NILP (arg))
    RETURN_UNGCPRO (Ffuncall (1, &fn));
  gcpro1.nvars = 2;

#ifdef NO_ARG_ARRAY
  {
    Lisp_Object args[2];
    args[0] = fn;
    args[1] = arg;
    gcpro1.var = args;
    RETURN_UNGCPRO (Fapply (2, args));
  }
#else /* not NO_ARG_ARRAY */
  RETURN_UNGCPRO (Fapply (2, &fn));
#endif /* not NO_ARG_ARRAY */
}

/* Call function fn on no arguments */
Lisp_Object
call0 (fn)
     Lisp_Object fn;
{
  struct gcpro gcpro1;

  GCPRO1 (fn);
  RETURN_UNGCPRO (Ffuncall (1, &fn));
}


/* Call function fn with argument arg */
/* ARGSUSED */
Lisp_Object
call1 (fn, arg)
     Lisp_Object fn, arg;
{
  struct gcpro gcpro1;
#ifdef NO_ARG_ARRAY
  Lisp_Object args[2];  

  args[0] = fn;
  args[1] = arg;
  GCPRO1 (args[0]);
  gcpro1.nvars = 2;
  RETURN_UNGCPRO (Ffuncall (2, args));
#else /* not NO_ARG_ARRAY */
  GCPRO1 (fn);
  gcpro1.nvars = 2;
  RETURN_UNGCPRO (Ffuncall (2, &fn));
#endif /* not NO_ARG_ARRAY */
}

/* Call function fn with arguments arg, arg1 */
/* ARGSUSED */
Lisp_Object
call2 (fn, arg, arg1)
     Lisp_Object fn, arg, arg1;
{
  struct gcpro gcpro1;
#ifdef NO_ARG_ARRAY
  Lisp_Object args[3];
  args[0] = fn;
  args[1] = arg;
  args[2] = arg1;
  GCPRO1 (args[0]);
  gcpro1.nvars = 3;
  RETURN_UNGCPRO (Ffuncall (3, args));
#else /* not NO_ARG_ARRAY */
  GCPRO1 (fn);
  gcpro1.nvars = 3;
  RETURN_UNGCPRO (Ffuncall (3, &fn));
#endif /* not NO_ARG_ARRAY */
}

/* Call function fn with arguments arg, arg1, arg2 */
/* ARGSUSED */
Lisp_Object
call3 (fn, arg, arg1, arg2)
     Lisp_Object fn, arg, arg1, arg2;
{
  struct gcpro gcpro1;
#ifdef NO_ARG_ARRAY
  Lisp_Object args[4];
  args[0] = fn;
  args[1] = arg;
  args[2] = arg1;
  args[3] = arg2;
  GCPRO1 (args[0]);
  gcpro1.nvars = 4;
  RETURN_UNGCPRO (Ffuncall (4, args));
#else /* not NO_ARG_ARRAY */
  GCPRO1 (fn);
  gcpro1.nvars = 4;
  RETURN_UNGCPRO (Ffuncall (4, &fn));
#endif /* not NO_ARG_ARRAY */
}

/* Call function fn with arguments arg0, arg1, arg2, arg3 */
Lisp_Object
call4 (Lisp_Object fn,
       Lisp_Object arg0, Lisp_Object arg1, Lisp_Object arg2, Lisp_Object arg3)
{
  struct gcpro gcpro1;
  Lisp_Object args[5];
  args[0] = fn;
  args[1] = arg0;
  args[2] = arg1;
  args[3] = arg2;
  args[4] = arg3;
  GCPRO1 (args[0]);
  gcpro1.nvars = 5;
  RETURN_UNGCPRO (Ffuncall (5, args));
}

/* Call function fn with arguments arg0, arg1, arg2, arg3, arg4 */
Lisp_Object
call5 (Lisp_Object fn,
       Lisp_Object arg0, Lisp_Object arg1, Lisp_Object arg2, 
       Lisp_Object arg3, Lisp_Object arg4)
{
  struct gcpro gcpro1;
  Lisp_Object args[6];
  args[0] = fn;
  args[1] = arg0;
  args[2] = arg1;
  args[3] = arg2;
  args[4] = arg3;
  args[5] = arg4;
  GCPRO1 (args[0]);
  gcpro1.nvars = 6;
  RETURN_UNGCPRO (Ffuncall (6, args));
}

#ifdef EMACS_BTL
#include "btl-get.h"
int
btl_symbol_id_number (sym)
     Lisp_Object sym;
{
  if (SYMBOLP (sym))
    {
      extern Lisp_Object VBTL_id_tag;
      register Lisp_Object tag = VBTL_id_tag;
      Lisp_Object id;
      int foundp = 0;
    
      BTL_GET (sym, tag, id, foundp);
      if (foundp && (FIXNUMP (id)))
        {
          int id_number = XINT(id);
          if (id_number > 0)
            return id_number;
        }
    }

  return 0;
}
#endif

DEFUN ("funcall", Ffuncall, Sfuncall, 1, MANY, 0,
  "Call first argument as a function, passing remaining arguments to it.\n\
Thus,  (funcall 'cons 'x 'y)  returns  (x . y).")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  Lisp_Object fun;
  Lisp_Object funcar;
  int numargs = nargs - 1;
  Lisp_Object val;
  struct backtrace backtrace;
  register int i;

  QUIT;
  if ((consing_since_gc > gc_cons_threshold) || always_gc)
    Fgarbage_collect ();

  if (++lisp_eval_depth > max_lisp_eval_depth)
    {
      if (max_lisp_eval_depth < 100)
	max_lisp_eval_depth = 100;
      if (lisp_eval_depth > max_lisp_eval_depth)
	error ("Lisp nesting exceeds max-lisp-eval-depth", 0, 0, 0);
    }

#ifdef EMACS_BTL
  backtrace.id_number = 0;
#endif
  backtrace.next = backtrace_list;
  backtrace_list = &backtrace;
  backtrace.function = &args[0];
  backtrace.args = &args[1];
  backtrace.nargs = nargs - 1;
  backtrace.evalargs = 0;
  backtrace.debug_on_exit = 0;

  if (debug_on_next_call)
    do_debug_on_call (Qlambda);

 retry:

  fun = args[0];

#ifdef EMACS_BTL
  {
    extern int emacs_btl_elisp_only_p;
    extern int btl_symbol_id_number ();
    if (emacs_btl_elisp_only_p)
      backtrace.id_number = btl_symbol_id_number (fun);
  }
#endif

  fun = indirect_function (fun, 1);

  if (SUBRP (fun))
    {
      struct Lisp_Subr *subr = XSUBR (fun);
      int max_args = subr->max_args;
      Lisp_Object (*fn) () = subr_function (subr);
      Lisp_Object argvals[SUBR_MAX_ARGS];
      register Lisp_Object *internal_args;

      if (numargs < subr->min_args
	  || (max_args >= 0 && max_args < numargs))
	{
	  return Fsignal (Qwrong_number_of_arguments, 
                          list2 (fun, make_number (numargs)));
	}

      if (max_args == UNEVALLED)
	return Fsignal (Qinvalid_function, list1 (fun));

      if (max_args == MANY)
	{
	  val = ((*fn) (numargs, args + 1));
	  goto done;
	}

      if (max_args > numargs)
	{
          for (i = 0; i < numargs; i++)
            argvals[i] = args[i + 1];
	  for (i = numargs; i < max_args; i++)
	    argvals[i] = Qnil;
          internal_args = argvals;
	}
      else
	internal_args = args + 1;
      switch (max_args)
	{
	case 0:
	  val = ((*fn) ());
	  goto done;
	case 1:
	  val = ((*fn) (internal_args[0]));
	  goto done;
	case 2:
	  val = ((*fn) (internal_args[0], internal_args[1]));
	  goto done;
	case 3:
	  val = ((*fn) (internal_args[0], internal_args[1],
                        internal_args[2]));
	  goto done;
	case 4:
	  val = ((*fn) (internal_args[0], internal_args[1],
                        internal_args[2], internal_args[3]));
	  goto done;
	case 5:
	  val = ((*fn) (internal_args[0], internal_args[1],
                        internal_args[2], internal_args[3],
                        internal_args[4]));
	  goto done;
	case 6:
	  val = ((*fn) (internal_args[0], internal_args[1],
                        internal_args[2], internal_args[3],
                        internal_args[4], internal_args[5]));
	  goto done;
	case 7:
	  val = ((*fn) (internal_args[0], internal_args[1],
                        internal_args[2], internal_args[3],
                        internal_args[4], internal_args[5],
                        internal_args[6]));
	  goto done;

	default:

	  /* If a subr takes more than 6 arguments without using MANY
	     or UNEVALLED, we need to extend this function to support it. 
	     Until this is done, there is no way to call the function.  */
	  abort ();
	}
    }
  if (COMPILEDP (fun))
    val = funcall_lambda (fun, numargs, args + 1);
  else
    {
      if (!CONSP (fun))
	return Fsignal (Qinvalid_function, list1 (fun));
      funcar = Fcar (fun);
      if (!SYMBOLP (funcar))
	return Fsignal (Qinvalid_function, list1 (fun));
      if (EQ (funcar, Qlambda))
	val = funcall_lambda (fun, numargs, args + 1);
      else if (EQ (funcar, Qmocklisp))
	val = ml_apply (fun, Flist (numargs, args + 1));
      else if (EQ (funcar, Qautoload))
	{
	  do_autoload (fun, args[0]);
	  goto retry;
	}
      else
	return Fsignal (Qinvalid_function, list1 (fun));
    }
 done:
  lisp_eval_depth--;
  if (backtrace.debug_on_exit)
    val = call_debugger (list2 (Qexit, val));
  backtrace_list = backtrace.next;
  return val;
}


static Lisp_Object
apply_lambda (fun, args, eval_flag)
     Lisp_Object fun, args;
     int eval_flag;
{
  Lisp_Object args_left;
  int numargs = XINT (Flength (args));
  register Lisp_Object *arg_vector;
  struct gcpro gcpro1, gcpro2, gcpro3;
  register int i;
  register Lisp_Object tem;

  arg_vector = (Lisp_Object *) alloca (numargs * sizeof (Lisp_Object));
  args_left = args;

  GCPRO3 (*arg_vector, args_left, fun);
  gcpro1.nvars = 0;

  for (i = 0; i < numargs;)
    {
      tem = Fcar (args_left), args_left = Fcdr (args_left);
      if (eval_flag) tem = Feval (tem);
      arg_vector[i++] = tem;
      gcpro1.nvars = i;
    }

  UNGCPRO;

  if (eval_flag)
    {
      backtrace_list->args = arg_vector;
      backtrace_list->nargs = i;
    }
  backtrace_list->evalargs = 0;
  tem = funcall_lambda (fun, numargs, arg_vector);

  /* Do the debug-on-exit now, while arg_vector still exists.  */
  if (backtrace_list->debug_on_exit)
    tem = call_debugger (list2 (Qexit, tem));
  /* Don't do it again when we return to eval.  */
  backtrace_list->debug_on_exit = 0;
  return tem;
}

/* Apply a Lisp function FUN to the NARGS evaluated arguments in ARG_VECTOR
   and return the result of evaluation.
   FUN must be either a lambda-expression or a compiled-code object.  */

static Lisp_Object
funcall_lambda (fun, nargs, arg_vector)
     Lisp_Object fun;
     int nargs;
     register Lisp_Object *arg_vector;
{
  Lisp_Object val, tem;
  register Lisp_Object syms_left;
  register Lisp_Object next;
  int count = specpdl_depth;
  register int i;
  int optional = 0, rest = 0;

  specbind (Qmocklisp_arguments, Qt);   /* t means NOT mocklisp! */

  if (CONSP (fun))
    syms_left = Fcar (Fcdr (fun));
  else if (COMPILEDP (fun))
    syms_left = XVECTOR (fun)->contents[COMPILED_ARGLIST];
  else abort ();

  i = 0;
  for (; !NILP (syms_left); syms_left = Fcdr (syms_left))
    {
      QUIT;
      next = Fcar (syms_left);
      if (!SYMBOLP (next))
	signal_error (Qinvalid_function, list1 (fun));
      if (EQ (next, Qand_rest))
	rest = 1;
      else if (EQ (next, Qand_optional))
	optional = 1;
      else if (rest)
	{
	  specbind (next, Flist (nargs - i, &arg_vector[i]));
	  i = nargs;
	}
      else if (i < nargs)
	{
	  tem = arg_vector[i++];
	  specbind (next, tem);
	}
      else if (!optional)
	return Fsignal (Qwrong_number_of_arguments,
                        list2 (fun, make_number (nargs)));
      else
	specbind (next, Qnil);
    }

  if (i < nargs)
    return Fsignal (Qwrong_number_of_arguments, 
                    list2 (fun, make_number (nargs)));

  if (CONSP (fun))
    val = Fprogn (Fcdr (Fcdr (fun)));
  else
    val = Fbyte_code (XVECTOR (fun)->contents[COMPILED_BYTECODE],
		      XVECTOR (fun)->contents[COMPILED_CONSTANTS],
		      XVECTOR (fun)->contents[COMPILED_STACK_DEPTH]);
  return unbind_to (count, val);
}

static void
grow_specpdl ()
{
  if (specpdl_size >= max_specpdl_size)
    {
      if (max_specpdl_size < 400)
	max_specpdl_size = 400;
      if (specpdl_size >= max_specpdl_size)
	{
	  if (!NILP (Vdebug_on_error))
	    /* Leave room for some specpdl in the debugger.  */
	    max_specpdl_size = specpdl_size + 100;
	  Fsignal (Qerror,
		   list1 (build_string ("Variable binding depth exceeds max-specpdl-size")));
	}
    }
  specpdl_size *= 2;
  if (specpdl_size > max_specpdl_size)
    specpdl_size = max_specpdl_size;
  specpdl = (struct specbinding *) xrealloc (specpdl, specpdl_size * sizeof (struct specbinding));
  specpdl_ptr = specpdl + specpdl_depth;
}

void
specbind (symbol, value)
     Lisp_Object symbol, value;
{
  Lisp_Object ovalue;

  CHECK_SYMBOL (symbol, 0);

  if (specpdl_depth >= specpdl_size)
    grow_specpdl ();
  specpdl_ptr->symbol = symbol;
  specpdl_ptr->func = 0;
  ovalue = XSYMBOL (symbol)->value;
  specpdl_ptr->old_value = (EQ (ovalue, Qunbound)
			    ? Qunbound : Fsymbol_value (symbol));
  specpdl_ptr++;
  specpdl_depth++;
  if (XTYPE (ovalue) == Lisp_Buffer_Objfwd)
    store_symval_forwarding (symbol, ovalue, value);
  else
    Fset (symbol, value);
}

void
record_unwind_protect (function, arg)
     Lisp_Object (*function)();
     Lisp_Object arg;
{
  if (specpdl_depth >= specpdl_size)
    grow_specpdl ();
  specpdl_ptr->func = function;
  specpdl_ptr->symbol = Qnil;
  specpdl_ptr->old_value = arg;
  specpdl_ptr++;
  specpdl_depth++;
}

Lisp_Object
unbind_to (int count, Lisp_Object value)
{
  int quitf = !NILP (Vquit_flag);
  struct gcpro gcpro1;

  GCPRO1 (value);

  Vquit_flag = Qnil;

  while (specpdl_depth != count)
    {
      --specpdl_ptr;
      --specpdl_depth;
      if (specpdl_ptr->func != 0)
	(*specpdl_ptr->func) (specpdl_ptr->old_value);
      /* Note that a "binding" of nil is really an unwind protect,
	so in that case the "old value" is a list of forms to evaluate.  */
      else if (NILP (specpdl_ptr->symbol))
	Fprogn (specpdl_ptr->old_value);
      else
        Fset (specpdl_ptr->symbol, specpdl_ptr->old_value);
    }
  if (NILP (Vquit_flag) && quitf) Vquit_flag = Qt;

  UNGCPRO;

  return value;
}

#if 0

/* Get the value of symbol's global binding, even if that binding
 is not now dynamically visible.  */

Lisp_Object
top_level_value (symbol)
     Lisp_Object symbol;
{
  register struct specbinding *ptr = specpdl;

  CHECK_SYMBOL (symbol, 0);
  for (; ptr != specpdl_ptr; ptr++)
    {
      if (EQ (ptr->symbol, symbol))
	return ptr->old_value;
    }
  return Fsymbol_value (symbol);
}

Lisp_Object
top_level_set (symbol, newval)
     Lisp_Object symbol, newval;
{
  register struct specbinding *ptr = specpdl;

  CHECK_SYMBOL (symbol, 0);
  for (; ptr != specpdl_ptr; ptr++)
    {
      if (EQ (ptr->symbol, symbol))
	{
	  ptr->old_value = newval;
	  return newval;
	}
    }
  return Fset (symbol, newval);
}  

#endif /* 0 */

DEFUN ("backtrace-debug", Fbacktrace_debug, Sbacktrace_debug, 2, 2, 0,
  "Set the debug-on-exit flag of eval frame LEVEL levels down to FLAG.\n\
The debugger is entered when that frame exits, if the flag is non-nil.")
  (level, flag)
     Lisp_Object level, flag;
{
  register struct backtrace *backlist = backtrace_list;
  register int i;

  CHECK_FIXNUM (level, 0);

  for (i = 0; backlist && i < XINT (level); i++)
    {
      backlist = backlist->next;
    }

  if (backlist)
    backlist->debug_on_exit = !NILP (flag);

  return flag;
}

DEFUN ("backtrace", Fbacktrace, Sbacktrace, 0, 1, "",
  "Print a trace of Lisp function calls currently active.\n\
Output stream used is value of `standard-output'.")
  (stream)
  Lisp_Object stream;
{
  register struct backtrace *backlist = backtrace_list;
  register int i;
  Lisp_Object tail;
  Lisp_Object tem;
  int old_pr = print_readably;
  int old_level = Vprint_level;
  struct gcpro gcpro1, gcpro2;

  entering_debugger = 0;

  XFASTINT (Vprint_level) = 3;
  print_readably = 0;
  tail = Qnil;

  GCPRO2 (tail, stream);

  if (NILP (stream)) stream = Vstandard_output;

  while (backlist)
    {
      write_string_1 (((backlist->debug_on_exit) ? "* " : "  "), 2,
                      stream);
      if (backlist->nargs == UNEVALLED)
	{
	  Fprin1 (Fcons (*backlist->function, *backlist->args), Qnil);
	}
      else
	{
	  tem = *backlist->function;
	  Fprin1 (tem, Qnil);	/* This can QUIT */
	  write_string ("(", 1);
	  if (backlist->nargs == MANY)
	    {
	      for (tail = *backlist->args, i = 0;
		   !NILP (tail);
		   tail = Fcdr (tail), i++)
		{
		  if (i) write_string (" ", 1);
		  Fprin1 (Fcar (tail), Qnil);
		}
	    }
	  else
	    {
	      for (i = 0; i < backlist->nargs; i++)
		{
		  if (i != 0) write_string (" ", 1);
		  Fprin1 (backlist->args[i], Qnil);
		}
	    }
	}
      write_string (")\n", 2);
      backlist = backlist->next;
    }

  Vprint_level = old_level;
  print_readably = old_pr;
  UNGCPRO;
  return Qnil;
}

DEFUN ("backtrace-frame", Fbacktrace_frame, Sbacktrace_frame, 1, 1, "",
  "Return the function and arguments N frames up from current execution point.\n\
If that frame has not evaluated the arguments yet (or is a special form),\n\
the value is (nil FUNCTION ARG-FORMS...).\n\
If that frame has evaluated its arguments and called its function already,\n\
the value is (t FUNCTION ARG-VALUES...).\n\
A &rest arg is represented as the tail of the list ARG-VALUES.\n\
FUNCTION is whatever was supplied as car of evaluated list,\n\
or a lambda expression for macro calls.\n\
If N is more than the number of frames, the value is nil.")
  (nframes)
     Lisp_Object nframes;
{
  register struct backtrace *backlist = backtrace_list;
  register int i;
  Lisp_Object tem;

  CHECK_NATNUM (nframes, 0);

  /* Find the frame requested.  */
  for (i = 0; i < XFASTINT (nframes); i++)
    backlist = backlist->next;

  if (!backlist)
    return Qnil;
  if (backlist->nargs == UNEVALLED)
    return Fcons (Qnil, Fcons (*backlist->function, *backlist->args));
  else
    {
      if (backlist->nargs == MANY)
	tem = *backlist->args;
      else
	tem = Flist (backlist->nargs, backlist->args);

      return Fcons (Qt, Fcons (*backlist->function, tem));
    }
}

void
syms_of_eval ()
{
  DEFVAR_INT ("max-specpdl-size", &max_specpdl_size,
    "Limit on number of Lisp variable bindings & unwind-protects before error.");

  DEFVAR_INT ("max-lisp-eval-depth", &max_lisp_eval_depth,
    "Limit on depth in `eval', `apply' and `funcall' before error.\n\
This limit is to catch infinite recursions for you before they cause\n\
actual stack overflow in C, which would be fatal for Emacs.\n\
You can safely make it considerably larger than its default value,\n\
if that proves inconveniently small.");

  DEFVAR_LISP ("quit-flag", &Vquit_flag,
    "Non-nil causes `eval' to abort, unless `inhibit-quit' is non-nil.\n\
Typing C-G sets `quit-flag' non-nil, regardless of `inhibit-quit'.");
  Vquit_flag = Qnil;

  DEFVAR_LISP ("inhibit-quit", &Vinhibit_quit,
    "Non-nil inhibits C-g quitting from happening immediately.\n\
Note that `quit-flag' will still be set by typing C-g,\n\
so a quit will be signalled as soon as `inhibit-quit' is nil.\n\
To prevent this happening, set `quit-flag' to nil\n\
before making `inhibit-quit' nil.");
  Vinhibit_quit = Qnil;

  defsymbol (&Qinhibit_quit, "inhibit-quit");

  defsymbol (&Qautoload, "autoload");

  defsymbol (&Qdebug_on_error, "debug-on-error");

  defsymbol (&Qmacro, "macro");

  /* Note that the process handling also uses Qexit, but we don't want
     to staticpro it twice, so we just do it here.  */
  defsymbol (&Qexit, "exit");

  defsymbol (&Qinteractive, "interactive");

  defsymbol (&Qcommandp, "commandp");

  defsymbol (&Qdefun, "defun");

  defsymbol (&Qand_rest, "&rest");

  defsymbol (&Qand_optional, "&optional");

  defsymbol (&Qeval, "eval");

  DEFVAR_LISP ("stack-trace-on-error", &Vstack_trace_on_error,
    "*Non-nil means automatically display a backtrace buffer\n\
after any error that is handled by the editor command loop.\n\
If the value is a list, an error only means to display a backtrace\n\
if one of its condition symbols appears in the list.");
  Vstack_trace_on_error = Qnil;

  DEFVAR_LISP ("debug-on-error", &Vdebug_on_error,
    "*Non-nil means enter debugger if an error is signalled.\n\
Does not apply to errors handled by `condition-case'.\n\
If the value is a list, an error only means to enter the debugger\n\
if one of its condition symbols appears in the list.\n\
See also variable `debug-on-quit'.");
  Vdebug_on_error = Qnil;

  DEFVAR_BOOL ("debug-on-quit", &debug_on_quit,
    "*Non-nil means enter debugger if quit is signalled (C-G, for example).\n\
Does not apply if quit is handled by a `condition-case'.");
  debug_on_quit = 0;

  DEFVAR_BOOL ("debug-on-next-call", &debug_on_next_call,
    "Non-nil means enter debugger before next `eval', `apply' or `funcall'.");

  DEFVAR_LISP ("debugger", &Vdebugger,
    "Function to call to invoke debugger.\n\
If due to frame exit, args are `exit' and the value being returned;\n\
 this function's value will be returned instead of that.\n\
If due to error, args are `error' and a list of the args to `signal'.\n\
If due to `apply' or `funcall' entry, one arg, `lambda'.\n\
If due to `eval' entry, one arg, t.");
  Vdebugger = Qnil;

  defsymbol (&Qmocklisp_arguments, "mocklisp-arguments");

  DEFVAR_LISP ("mocklisp-arguments", &Vmocklisp_arguments,
    "While in a mocklisp function, the list of its unevaluated args.");
  Vmocklisp_arguments = Qt;

  DEFVAR_LISP ("run-hooks", &Vrun_hooks,
    "Set to the function `run-hooks', if that function has been defined.\n\
Otherwise, nil (in a bare Emacs without preloaded Lisp code).");
  Vrun_hooks = Qnil;

  staticpro (&Vautoload_queue);
  Vautoload_queue = Qnil;

  defsubr (&Sor);
  defsubr (&Sand);
  defsubr (&Sif);
  defsubr (&Scond);
  defsubr (&Sprogn);
  defsubr (&Sprog1);
  defsubr (&Sprog2);
  defsubr (&Ssetq);
  defsubr (&Squote);
  defsubr (&Sfunction);
  defsubr (&Sdefun);
  defsubr (&Sdefmacro);
  defsubr (&Sdefvar);
  defsubr (&Sdefconst);
  defsubr (&Suser_variable_p);
  defsubr (&Slet);
  defsubr (&SletX);
  defsubr (&Swhile);
  defsubr (&Smacroexpand);
  defsubr (&Scatch);
  defsubr (&Sthrow);
  defsubr (&Sunwind_protect);
  defsubr (&Scondition_case);
  defsubr (&Ssignal);
  defsubr (&Sinteractive_p);
  defsubr (&Scommandp);
  defsubr (&Sautoload);
  defsubr (&Seval);
  defsubr (&Sapply);
  defsubr (&Sfuncall);
  defsubr (&Sbacktrace_debug);
  defsubr (&Sbacktrace);
  defsubr (&Sbacktrace_frame);
}
