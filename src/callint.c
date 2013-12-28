/* Call a Lisp function interactively.
   Copyright (C) 1985, 1986, 1992, 1993, 1994 Free Software Foundation, Inc.

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
#include "intl.h"
#include "buffer.h"
#include "bytecode.h"
#include "commands.h"
#include "window.h"

extern int num_input_chars;

Lisp_Object Vprefix_arg, Vcurrent_prefix_arg;
Lisp_Object Qcall_interactively;
Lisp_Object Vcommand_history;

Lisp_Object Vcommand_debug_status, Qcommand_debug_status;
Lisp_Object Qenable_recursive_minibuffers;

Lisp_Object Qminus;
Lisp_Object Qcurrent_prefix_arg;

Lisp_Object Quser_variable_p;
Lisp_Object Qread_from_minibuffer;
Lisp_Object Qread_file_name;
Lisp_Object Qread_directory_name;
Lisp_Object Qcompleting_read;
Lisp_Object Qread_buffer;
Lisp_Object Qread_function;
Lisp_Object Qread_variable;
Lisp_Object Qread_minibuffer;
Lisp_Object Qread_command;
Lisp_Object Qread_number;
Lisp_Object Qread_string;
Lisp_Object Qevents_to_keys;

/* This comment supplies the doc string for interactive,
   for make-docfile to see.  We cannot put this in the real DEFUN
   due to limits in the Unix cpp.

DEFUN ("interactive", Ffoo, Sfoo, 0, 0, 0,
 "Specify a way of parsing arguments for interactive use of a function.\n\
For example, write\n\
  (defun foo (arg) \"Doc string\" (interactive \"p\") ...use arg...)\n\
to make ARG be the prefix argument when `foo' is called as a command.\n\
The \"call\" to `interactive' is actually a declaration rather than a function;\n\
 it tells `call-interactively' how to read arguments\n\
 to pass to the function.\n\
When actually called, `interactive' just returns nil.\n\
\n\
The argument of `interactive' is usually a string containing a code letter\n\
 followed by a prompt.  (Some code letters do not use I/O to get\n\
 the argument and do not need prompts.)  To prompt for multiple arguments,\n\
 give a code letter, its prompt, a newline, and another code letter, etc.\n\
 Prompts are passed to format, and may use % escapes to print the\n\
 arguments that have already been read.\n\
If the argument is not a string, it is evaluated to get a list of\n\
 arguments to pass to the function.\n\
Just `(interactive)' means pass no args when calling interactively.\n\
\nCode letters available are:\n\
a -- Function name: symbol with a function definition.\n\
b -- Name of existing buffer.\n\
B -- Name of buffer, possibly nonexistent.\n\
c -- Character.\n\
C -- Command name: symbol with interactive function definition.\n\
d -- Value of point as number.  Does not do I/O.\n\
D -- Directory name.\n\
e -- Last mouse event.  Does not do I/O.\n\
f -- Existing file name.\n\
F -- Possibly nonexistent file name.\n\
k -- Key sequence (a vector of events).\n\
m -- Value of mark as number.  Does not do I/O.\n\
n -- Number read using minibuffer.\n\
N -- Prefix arg converted to number, or if none, do like code `n'.\n\
p -- Prefix arg converted to number.  Does not do I/O.\n\
P -- Prefix arg in raw form.  Does not do I/O.\n\
r -- Region: point and mark as 2 numeric args, smallest first.  Does no I/O.\n\
s -- Any string.\n\
S -- Any symbol.\n\
v -- Variable name: symbol that is user-variable-p.\n\
x -- Lisp expression read but not evaluated.\n\
X -- Lisp expression read and evaluated.\n\
In addition, if the string begins with `*'\n\
 then an error is signaled if the buffer is read-only.\n\
 This happens before reading any arguments.\n\
If the string begins with `@', then the window the mouse is over is selected\n\
 before anything else is done.\n\
If the string begins with `_', then this command will not cause the region\n\
 to be deactivated when it completes; that is, `zmacs-region-stays' will be\n\
 set to t when the command exits successfully.\n\
You may use any of `@', `*' and `_' at the beginning of the string;\n\
 they are processed in the order that they appear."
*/

/* ARGSUSED */
DEFUN ("interactive", Finteractive, Sinteractive, 0, UNEVALLED, 0,
  0 /* See immediately above */)
  (args)
     Lisp_Object args;
{
  return Qnil;
}

/* Quotify EXP: if EXP is constant, return it.
   If EXP is not constant, return (quote EXP).  */
static Lisp_Object
quotify_arg (exp)
     register Lisp_Object exp;
{
  if (!FIXNUMP (exp) && !STRINGP (exp)
      && !NILP (exp) && !EQ (exp, Qt))
    return Fcons (Qquote, Fcons (exp, Qnil));

  return exp;
}

/* Modify EXP by quotifying each element (except the first).  */
static Lisp_Object
quotify_args (exp)
     Lisp_Object exp;
{
  register Lisp_Object tail;
  register struct Lisp_Cons *ptr;
  for (tail = exp; CONSP (tail); tail = ptr->cdr)
    {
      ptr = XCONS (tail);
      ptr->car = quotify_arg (ptr->car);
    }
  return exp;
}

static int
check_mark ()
{
  Lisp_Object tem;

  if (zmacs_regions && !zmacs_region_active_p)
    error (GETTEXT ("The region is not active now"));

  tem = Fmarker_buffer (current_buffer->mark);
  if (NILP (tem) || (XBUFFER (tem) != current_buffer))
    error (GETTEXT ("The mark is not set now"));

  return (marker_position (current_buffer->mark));
}

Lisp_Object Vcurrent_mouse_event;

static Lisp_Object
callint_prompt (CONST char *prompt_start, int prompt_length,
                CONST Lisp_Object *args, int nargs)
{
  Lisp_Object s = make_string (prompt_start, prompt_length);

  if (!strchr ((char *) XSTRING (s)->data, '%'))
    return (s);

  {
    /* Oh fuck!  Fformat smashes its arg vector!  Have to copy it */
    Lisp_Object *xargs;
    struct gcpro gcpro1;

    xargs = (Lisp_Object *) alloca ((nargs + 1) * sizeof (Lisp_Object));
    xargs[0] = s;
    memcpy (xargs + 1, args, nargs * sizeof (Lisp_Object));

    GCPRO1 (*xargs);
    gcpro1.nvars = (nargs + 1);
    RETURN_UNGCPRO (Fformat (nargs + 1, xargs));
  }
}

DEFUN ("call-interactively", Fcall_interactively, Scall_interactively, 1, 2, 0,
  "Call FUNCTION, reading args according to its interactive calling specs.\n\
The function contains a specification of how to do the argument reading.\n\
In the case of user-defined functions, this is specified by placing a call\n\
to the function `interactive' at the top level of the function body.\n\
See `interactive'.\n\
\n\
If optional second arg RECORD-FLAG is the symbol `lambda', the interactive\n\
calling arguments for FUNCTION are read and returned as a list,\n\
but the function is not called on them.\n\
\n\
If RECORD-FLAG is `t' then unconditionally put this command in the\n\
command-history.  Otherwise, this is done only if an arg is read using\n\
the minibuffer.")
  (function, record_flag)
     Lisp_Object function, record_flag;
{
  int speccount = specpdl_depth ();
  /* Save this now, since use of minibuffer will clobber it. */
  Lisp_Object prefix = Vcurrent_prefix_arg;

  Lisp_Object fun;
  Lisp_Object specs = Qnil;
  /* If SPECS is a string, we reset prompt_data to XSTRING (specs)->data
   *  every time a GC might have occurred */
  CONST char *prompt_data = 0;
  int prompt_index = 0;
  int argcount;
  int set_zmacs_region_stays = 0;

 retry:

  fun = indirect_function (function, 1);

  /* Decode the kind of function.  Either handle it and return,
     or go to `lose' if not interactive, or go to `retry'
     to specify a different function, or set either PROMPT_DATA or SPECS. */

  if (SUBRP (fun))
    {
      prompt_data = XSUBR (fun)->prompt;
      if (!prompt_data)
	{
	lose:
	  function = wrong_type_argument (Qcommandp, function);
	  goto retry;
	}
    }
  else if (COMPILEDP (fun))
    {
      struct Lisp_Bytecode *b = XBYTECODE (fun);
      if (!(b->flags.interactivep))
        goto lose;
      specs = bytecode_interactive (b);
    }
  else if (!CONSP (fun))
    goto lose;
  else
    {
      Lisp_Object funcar = Fcar (fun);

      if (EQ (funcar, Qautoload))
	{
	  struct gcpro gcpro1, gcpro2;
	  GCPRO2 (function, prefix);
	  do_autoload (fun, function);
	  UNGCPRO;
	  goto retry;
	}
      else if (EQ (funcar, Qlambda))
	{
	  specs = Fassq (Qinteractive, Fcdr (Fcdr (fun)));
	  if (NILP (specs))
	    goto lose;
	  specs = Fcar (Fcdr (specs));
	}
#ifdef MOCKLISP_SUPPORT
      else if (EQ (funcar, Qmocklisp))
	return ml_apply (fun, Qinteractive);
#endif
      else
	goto lose;
    }

  /* If either specs or prompt_data is set to a string, use it.  */
  if (!STRINGP (specs) && prompt_data == 0)
    {
      struct gcpro gcpro1, gcpro2;
      int i = num_input_chars;
      GCPRO2 (function, specs);
      specs = Feval (specs);
      if (EQ (record_flag, Qlambda))
	{
	  UNGCPRO;
	  return (specs);
	}
      if (!NILP (record_flag) || i != num_input_chars)
	{
	  /* >>>>> There's no way for an interactive spec to put something
	   *  other than its literal arguments on the command-history.
	   *  For example, it should be possible to put (region-beginning)
	   *  on instead of some random point position like 259.
	   */
	  Vcommand_history
	    = Fcons (Fcons (function, quotify_args (Fcopy_sequence (specs))),
		     Vcommand_history);
	}
      RETURN_UNGCPRO (apply1 (fun, specs));
    }

  /* Here if function specifies a string to control parsing the defaults */

#ifdef I18N3
  /* Translate interactive prompt. */
  if (STRINGP (specs))
    {
      Lisp_Object domain = Qnil;
      if (COMPILEDP (fun))
	domain = Fcompiled_function_domain (fun);
      if (NILP (domain))
	specs = Fgettext (specs);
      else
	specs = Fdgettext (domain, specs);
    }
  else if (prompt_data)
    /* We do not have to worry about domains in this case because
       prompt_data is non-nil only for built-in functions, which
       always use the default domain. */
    prompt_data = gettext (prompt_data);
#endif

  /* Handle special starting chars `*' and `@' and `_'.  */
  prompt_index = 0;
  {
    struct gcpro gcpro1, gcpro2;
    GCPRO2 (function, specs);

    for (;;)
      {
	if (STRINGP (specs)) prompt_data = (CONST char *) XSTRING (specs)->data;
    
	if (prompt_data[prompt_index] == '*')
	  {
	    prompt_index++;
	    if (!NILP (current_buffer->read_only))
	      Fbarf_if_buffer_read_only ();
	  }
	else if (prompt_data[prompt_index] == '@')
	  {
	    prompt_index++;
	    if (! NILP (Vcurrent_mouse_event)) 
	      {
		Lisp_Object window = Fevent_window (Vcurrent_mouse_event);
		if (!NILP (window))
		  Fselect_window (window);
	      }
	  }
	else if (prompt_data[prompt_index] == '_')
	  {
	    prompt_index++;
	    set_zmacs_region_stays = 1;
	  }
	else
	  {
	    UNGCPRO;
	    break;
	  }
      }
  }

  /* Count the number of arguments the interactive spec would have
     us give to the function.  */
  argcount = 0;
  {
    CONST char *tem;
    for (tem = prompt_data + prompt_index; *tem; )
      {
	/* 'r' specifications ("point and mark as 2 numeric args")
	   produce *two* arguments.  */
	if (*tem == 'r')
	  argcount += 2;
	else
	  argcount += 1;
	tem = (CONST char *) strchr (tem + 1, '\n');
	if (!tem)
	  break;
	tem++;
      }
  }

  if (argcount == 0)
    {
      /* Interactive function or no arguments; just call it */
      if (EQ (record_flag, Qlambda))
	return (Qnil);
      if (!NILP (record_flag))
	{
	  Vcommand_history = Fcons (list1 (function), Vcommand_history);
	}
      specbind (Qcommand_debug_status, Qnil);
      fun = call0 (fun);
      if (set_zmacs_region_stays)
	zmacs_region_stays = 1;
      return (unbind_to (speccount, fun));
    }

  /* Read interactive arguments */
  {
    /* args[-1] is the function to call */
    /* args[n] is the n'th argument to the function */
    int alloca_size = (1	/* function to call */
                       + argcount /* actual arguments */
                       + argcount /* visargs */
                       + argcount /* varies */
                       );
    Lisp_Object *args
      = (((Lisp_Object *) alloca (sizeof (Lisp_Object) * alloca_size))
         + 1);
    /* visargs is an array of either Qnil or user-friendlier versions (often
     *  strings) of previous arguments, to use in prompts for succesive
     *  arguments.  ("Often strings" because emacs didn't used to have
     *  format %S and prin1-to-string.) */
    Lisp_Object *visargs = args + argcount;
    /* If varies[i] is non-null, the i'th argument shouldn't just have
       its value in this call quoted in the command history.  It should be
       recorded as a call to the function named varies[i]]. */
    Lisp_Object *varies = visargs + argcount;
    int arg_from_tty = 0;
    register int argnum;
    struct gcpro gcpro1, gcpro2;

    args[-1] = function;
    for (argnum = 0; argnum < alloca_size - 1; argnum++)
      args[argnum] = Qnil;

    /* Must GC-protect args[-1] (ie function) because Ffuncall doesn't */
    /* `function' itself isn't GC-protected -- use args[-1] from here
       (actually, doesn't matter since Emacs GC doesn't relocate, sigh) */
    GCPRO2 (prefix, args[-1]);
    gcpro2.nvars = alloca_size;

    /* Marginal kludge.  Use an evaluated interactive spec instead of this!
     * if (SYMBOLP (args[-1])
     *     && !NILP (Fget (args[-1], Qenable_recursive_minibuffers, Qnil)))
     *   specbind (Qenable_recursive_minibuffers, Qt);
     */

    for (argnum = 0; ; argnum++)
      {
	CONST char *prompt_start = prompt_data + prompt_index + 1;
	CONST char *prompt_limit = (CONST char *) strchr (prompt_start, '\n');
	int prompt_length;
	prompt_length = ((prompt_limit) 
			 ? (prompt_limit - prompt_start)
			 : strlen (prompt_start));
	if (prompt_limit && prompt_limit[1] == 0)
	  {
	    prompt_limit = 0;	/* "sfoo:\n" -- strip tailing return */
	    prompt_length -= 1;
	  }
	/* This uses `visargs' instead of `args' so that global-set-key
	   prompts with "Set key C-x C-f to command: "instead of printing
	   event objects in there.
	 */
#define PROMPT() callint_prompt (prompt_start, prompt_length, visargs, argnum)
	switch (prompt_data[prompt_index])
	  {
	  case 'a':		/* Symbol defined as a function */
	    {
	      Lisp_Object tem = call1 (Qread_function, PROMPT ());
	      args[argnum] = tem;
	      arg_from_tty = 1;
	      break;
	    }
	  case 'b':   		/* Name of existing buffer */
	    {
	      Lisp_Object def = Fcurrent_buffer ();
	      if (EQ (selected_window, minibuf_window))
		def = Fother_buffer (def, Qnil);
              /* read-buffer returns a buffer name, not a buffer! */
	      args[argnum] = call3 (Qread_buffer, PROMPT (), def, Qt);
	      arg_from_tty = 1;
	      break;
	    }
	  case 'B':		/* Name of buffer, possibly nonexistent */
	    {
              /* read-buffer returns a buffer name, not a buffer! */
	      args[argnum] = call2 (Qread_buffer, PROMPT (),
				    Fother_buffer (Fcurrent_buffer (), Qnil));
	      arg_from_tty = 1;
	      break;
	    }
	  case 'c':		/* Character */
	    {
              Lisp_Object tem;
	      int speccount = specpdl_depth ();

	      specbind (Qcursor_in_echo_area, Qt);
	      message ("%s", XSTRING (PROMPT ())->data);
	      tem = (call0 (Qread_char));
              args[argnum] = tem;
              /* visargs[argnum] = Fsingle_key_description (tem); */

	      unbind_to (speccount, Qnil);

	      /* #### `C-x / a' should not leave the prompt in the minibuffer.
		 This isn't the right fix, because (message ...) (read-char)
		 shouldn't leave the message there either... */
	      message (0);

	      arg_from_tty = 1;
	      break;
	    }
	  case 'C':		/* Command: symbol with interactive function */
	    {
	      Lisp_Object tem = call1 (Qread_command, PROMPT ());
	      args[argnum] = tem;
	      arg_from_tty = 1;
	      break;
	    }
	  case 'd':		/* Value of point.  Does not do I/O.  */
	    {
	      args[argnum] = make_number (PT);
	      varies[argnum] = Qpoint;
	      break;
	    }
	  case 'e':
	    {
	      args[argnum] = Vcurrent_mouse_event;
	      break;
	    }
	  case 'D':		/* Directory name. */
	    {
	      args[argnum] = call4 (Qread_directory_name, PROMPT (),
				    Qnil, /* dir */
				    current_buffer->directory, /* default */
				    Qt /* must-match */
				    );
	      arg_from_tty = 1;
	      break;
	    }
	  case 'f':		/* Existing file name. */
	    {
	      Lisp_Object tem = call4 (Qread_file_name, PROMPT (),
				       Qnil, /* dir */
				       Qnil, /* default */
				       make_number (0) /* must-match */
				       );
              args[argnum] = tem;
	      arg_from_tty = 1;
	      break;
	    }
	  case 'F':		/* Possibly nonexistent file name. */
	    {
	      args[argnum] = call4 (Qread_file_name, PROMPT (),
				    Qnil, /* dir */
				    Qnil, /* default */
				    Qnil /* must-match */
				    );
	      arg_from_tty = 1;
	      break;
	    }
	  case 'k':		/* Key sequence (vector of events) */
	    {
	      Lisp_Object tem = Fread_key_sequence (PROMPT ());
	      visargs[argnum] = Fkey_description (tem);
	      args[argnum] = call1 (Qevents_to_keys, tem);
	      arg_from_tty = 1;
	      break;
	    }

	  case 'm':		/* Value of mark.  Does not do I/O.  */
	    {
	      args[argnum] = make_number (check_mark ());
	      varies[argnum] = Qmark;
	      break;
	    }
	  case 'n':		/* Read number from minibuffer.  */
	    {
	    read_number:
	      args[argnum] = call2 (Qread_number, PROMPT (), Qt);
	      /* numbers are too boring to go on command history */
	      /* arg_from_tty = 1; */
              break;
            }
	  case 'N':		/* Prefix arg, else number from minibuffer */
	    {
	      if (NILP (prefix))
		goto read_number;
              else
                goto prefix_value;
	    }
	  case 'P':		/* Prefix arg in raw form.  Does no I/O.  */
	    {
	      args[argnum] = prefix;
	      break;
	    }
	  case 'p':		/* Prefix arg converted to number.  No I/O. */
            {
            prefix_value:
              {
                Lisp_Object tem = Fprefix_numeric_value (prefix);
                args[argnum] = tem;
              }
              break;
            }
	  case 'r':		/* Region, point and mark as 2 args. */
	    {
	      int tem = check_mark ();
	      args[argnum] = make_number (((PT < tem) ? PT : tem));
	      varies[argnum] = Qregion_beginning;
	      args[++argnum] = make_number (((PT > tem) ? PT : tem));
	      varies[argnum] = Qregion_end;
	      break;
	    }
	  case 's':		/* String read via minibuffer.  */
	    {
	      args[argnum] = call1 (Qread_string, PROMPT ());
	      arg_from_tty = 1;
	      break;
	    }
	  case 'S':		/* Any symbol.  */
	    {
#if 0				/* Historical crock */
	      Lisp_Object tem = intern ("minibuffer-local-ns-map");
	      tem = find_symbol_value (tem);
	      if (EQ (tem, Qunbound)) tem = Qnil;
	      tem = call3 (Qread_from_minibuffer, PROMPT (), Qnil, tem);
	      args[argnum] = Fintern (tem, Qnil);
#else /* 1 */
	      visargs[argnum] = Qnil;
	      for (;;)
		{
		  Lisp_Object tem = call5 (Qcompleting_read,
					   PROMPT (),
					   Vobarray,
					   Qnil,
					   Qnil,
					   /* nil, or prev attempt */
					   visargs[argnum]);
		  visargs[argnum] = tem;
		  /* I could use condition-case with this loser, but why bother?
		   * tem = Fread (tem); check-symbol-p;
		   */
		  tem = Fintern (tem, Qnil);
		  args[argnum] = tem;
		  if (string_length (XSYMBOL (tem)->name) > 0)
		    /* Don't accept the empty-named symbol.  If the loser
		       really wants this s/he can call completing-read 
                       directly */
		    break;
		}
#endif /* 1 */
	      arg_from_tty = 1;
	      break;
	    }
	  case 'v':		/* Variable name: user-variable-p symbol */
	    {
	      Lisp_Object tem = call1 (Qread_variable, PROMPT ());
	      args[argnum] = tem;
	      arg_from_tty = 1;
	      break;
	    }
	  case 'x':		/* Lisp expression read but not evaluated */
	    {
	      args[argnum] = call1 (Qread_minibuffer, PROMPT ());
              /* visargs[argnum] = Fprin1_to_string (args[argnum], Qnil); */
	      arg_from_tty = 1;
	      break;
	    }
	  case 'X':		/* Lisp expression read and evaluated */
	    {
	      Lisp_Object tem = call1 (Qread_minibuffer, PROMPT ());
	      /* visargs[argnum] = Fprin1_to_string (tem, Qnil); */
	      args[argnum] = Feval (tem);
	      arg_from_tty = 1;
	      break;
	    }
	  default:
	    {
	      error (GETTEXT ("Invalid `interactive' control letter \"%c\" (#o%03o)."),
		     prompt_data[prompt_index],
		     prompt_data[prompt_index]);
	    }
	  }
#undef PROMPT
	if (NILP (visargs[argnum]))
	  visargs[argnum] = args[argnum];

	if (!prompt_limit)
	  break;
	if (STRINGP (specs)) prompt_data = (CONST char *) XSTRING (specs)->data;
	prompt_index += prompt_length + 1 + 1; /* +1 to skip spec, +1 for \n */
      }
    unbind_to (speccount, Qnil);

    QUIT;

    if (EQ (record_flag, Qlambda))
      {
	RETURN_UNGCPRO (Flist (argcount, args));
      }
      
    if (arg_from_tty || !NILP (record_flag))
      {
        /* Reuse visargs as a temporary for constructing the command history */
	for (argnum = 0; argnum < argcount; argnum++)
	  {
	    if (!NILP (varies[argnum]))
	      visargs[argnum] = list1 (varies[argnum]);
	    else
	      visargs[argnum] = quotify_arg (args[argnum]);
	  }
	Vcommand_history = Fcons (Fcons (args[-1], Flist (argcount, visargs)),
				  Vcommand_history);
      }

    specbind (Qcommand_debug_status, Qnil);
    fun = Ffuncall (argcount + 1, args - 1);
    UNGCPRO;
    if (set_zmacs_region_stays)
      zmacs_region_stays = 1;
    return (unbind_to (speccount, fun));
  }
}

DEFUN ("prefix-numeric-value", Fprefix_numeric_value, Sprefix_numeric_value,
  1, 1, 0,
  "Return numeric meaning of raw prefix argument ARG.\n\
A raw prefix argument is what you get from `(interactive \"P\")'.\n\
Its numeric meaning is what you would get from `(interactive \"p\")'.")
  (raw)
     Lisp_Object raw;
{
  int val;

  if (NILP (raw))
    val = 1;
  else if (SYMBOLP (raw))
    val = -1;
  else if (FIXNUMP (raw))
    val = XINT (raw);
  else if (CONSP (raw) && FIXNUMP (XCONS (raw)->car))
    val = XINT (XCONS (raw)->car);
  else
    val = 1;

  return (make_number (val));
}

void
syms_of_callint ()
{
  defsymbol (&Qminus, "-");

  defsymbol (&Qcall_interactively, "call-interactively");
  defsymbol (&Qread_from_minibuffer, "read-from-minibuffer");
  defsymbol (&Qcompleting_read, "completing-read");
  defsymbol (&Qread_file_name, "read-file-name");
  defsymbol (&Qread_directory_name, "read-directory-name");
  defsymbol (&Qread_string, "read-string");
  defsymbol (&Qread_buffer, "read-buffer");
  defsymbol (&Qread_variable, "read-variable");
  defsymbol (&Qread_function, "read-function");
  defsymbol (&Qread_command, "read-command");
  defsymbol (&Qread_number, "read-number");
  defsymbol (&Qread_minibuffer, "read-minibuffer");
  defsymbol (&Qevents_to_keys, "events-to-keys");
  defsymbol (&Qcommand_debug_status, "command-debug-status");
  defsymbol (&Qenable_recursive_minibuffers, "enable-recursive-minibuffers");
  defsymbol (&Qcommand_debug_status, "command-debug-status");
  defsymbol (&Quser_variable_p, "user-variable-p");
  defsymbol (&Qcurrent_prefix_arg, "current-prefix-arg");

  DEFVAR_LISP ("prefix-arg", &Vprefix_arg,
    "The value of the prefix argument for the next editing command.\n\
It may be a number, or the symbol `-' for just a minus sign as arg,\n\
or a list whose car is a number for just one or more C-U's\n\
or nil if no argument has been specified.\n\
\n\
You cannot examine this variable to find the argument for this command\n\
since it has been set to nil by the time you can look.\n\
Instead, you should use the variable `current-prefix-arg', although\n\
normally commands can get this prefix argument with (interactive \"P\").");
  Vprefix_arg = Qnil;

  DEFVAR_LISP ("current-prefix-arg", &Vcurrent_prefix_arg,
    "The value of the prefix argument for this editing command.\n\
It may be a number, or the symbol `-' for just a minus sign as arg,\n\
or a list whose car is a number for just one or more C-U's\n\
or nil if no argument has been specified.\n\
This is what `(interactive \"P\")' returns.");
  Vcurrent_prefix_arg = Qnil;

  DEFVAR_LISP ("current-mouse-event", &Vcurrent_mouse_event,
    "The mouse-button event which invoked this command, or nil.\n\
This is what `(interactive \"e\")' returns.");
  Vcurrent_mouse_event = Qnil;

  DEFVAR_LISP ("command-history", &Vcommand_history,
    "List of recent commands that read arguments from terminal.\n\
Each command is represented as a form to evaluate.");
  Vcommand_history = Qnil;

  DEFVAR_LISP ("command-debug-status", &Vcommand_debug_status,
    "Debugging status of current interactive command.\n\
Bound each time `call-interactively' is called;\n\
may be set by the debugger as a reminder for itself.");
  Vcommand_debug_status = Qnil;

  defsubr (&Sinteractive);
  defsubr (&Scall_interactively);
  defsubr (&Sprefix_numeric_value);
}
