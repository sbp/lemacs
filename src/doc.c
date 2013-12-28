/* Record indices of function doc strings stored in a file.
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


#include "config.h"

#include <stdio.h>
#include <sys/types.h>
#include <sys/file.h>	/* Must be after sys/types.h for USG and BSD4_1*/

#ifdef USG5
#include <fcntl.h>
#endif

#ifndef O_RDONLY
#define O_RDONLY 0
#endif

#include "lisp.h"
#include "buffer.h"
#include "insdel.h"

Lisp_Object Vdoc_file_name;

static Lisp_Object
get_doc_string (filepos)
     long filepos;
{
  char buf[512 * 32 + 1];
  register int fd;
  register char *name;
  register char *p, *p1;
  register int count;

  if (!STRINGP (Vexec_directory)
      || !STRINGP (Vdoc_file_name))
    return Qnil;

  name = (char *) alloca (XSTRING (Vexec_directory)->size
			  + XSTRING (Vdoc_file_name)->size + 8);
  strcpy (name, (char *) XSTRING (Vexec_directory)->data);
  strcat (name, (char *) XSTRING (Vdoc_file_name)->data);
#ifdef VMS
#ifndef VMS4_4
  /* For VMS versions with limited file name syntax,
     convert the name to something VMS will allow.  */
  p = name;
  while (*p)
    {
      if (*p == '-')
	*p = '_';
      p++;
    }
#endif /* not VMS4_4 */
#ifdef VMS4_4
  strcpy (name, sys_translate_unix (name));
#endif /* VMS4_4 */
#endif /* VMS */

  fd = open (name, O_RDONLY, 0);
  if (fd < 0)
    error ("Cannot open doc string file \"%s\"", name);
  if (0 > lseek (fd, filepos, 0))
    {
      close (fd);
      error ("Position %ld out of range in doc string file \"%s\"",
	     filepos, name);
    }
  p = buf;
  while (p != buf + sizeof buf - 1)
    {
      count = read (fd, p, 512);
      p[count] = 0;
      if (!count)
	break;
      p1 = strchr (p, '\037');
      if (p1)
	{
	  *p1 = 0;
	  p = p1;
	  break;
	}
      p += count;
    }
  close (fd);
  return make_string (buf, p - buf);
}

DEFUN ("documentation", Fdocumentation, Sdocumentation, 1, 1, 0,
  "Return the documentation string of FUNCTION.")
  (fun1)
     Lisp_Object fun1;
{
  Lisp_Object fun;
  Lisp_Object funcar;
  Lisp_Object tem;

  fun = fun1;
  while (SYMBOLP (fun))
    fun = Fsymbol_function (fun);
  if (SUBRP (fun))
    {
      if (XSUBR (fun)->doc == 0) return Qnil;
      if ((int) XSUBR (fun)->doc >= 0)
	return Fsubstitute_command_keys (build_string (XSUBR (fun)->doc));
      return Fsubstitute_command_keys (get_doc_string (- (int) XSUBR (fun)->doc));
    }
  if (COMPILEDP (fun))
    {
      if (XVECTOR (fun)->size <= COMPILED_DOC_STRING)
	return Qnil;
      tem = XVECTOR (fun)->contents[COMPILED_DOC_STRING];
      if (STRINGP (tem))
	return Fsubstitute_command_keys (tem);
      if (FIXNUMP (tem) && XINT (tem) >= 0)
	return Fsubstitute_command_keys (get_doc_string (XFASTINT (tem)));
      return Qnil;
    }
  if (KEYMAPP (fun))
    return build_string ("Prefix command (definition is a keymap of subcommands).");
  if (STRINGP (fun))
    return build_string ("Keyboard macro.");
  if (!CONSP (fun))
    return Fsignal (Qinvalid_function, Fcons (fun, Qnil));
  funcar = Fcar (fun);
  if (!SYMBOLP (funcar))
    return Fsignal (Qinvalid_function, Fcons (fun, Qnil));
  if (XSYMBOL (funcar) == XSYMBOL (Qlambda)
      || XSYMBOL (funcar) == XSYMBOL (Qautoload))
    {
      tem = Fcar (Fcdr (Fcdr (fun)));
      if (STRINGP (tem))
	return Fsubstitute_command_keys (tem);
      if (FIXNUMP (tem) && XINT (tem) >= 0)
	return Fsubstitute_command_keys (get_doc_string (XFASTINT (tem)));
      return Qnil;
    }
  if (XSYMBOL (funcar) == XSYMBOL (Qmocklisp))
    return Qnil;
  if (XSYMBOL (funcar) == XSYMBOL (Qmacro))
    return Fdocumentation (Fcdr (fun));
  else
    return Fsignal (Qinvalid_function, Fcons (fun, Qnil));
}

DEFUN ("documentation-property", Fdocumentation_property, 
       Sdocumentation_property, 2, 2, 0,
  "Return the documentation string that is SYMBOL's PROP property.\n\
This differs from using `get' only in that it can refer to strings\n\
stored in the `etc/DOC' file.")
  (sym, prop)
     Lisp_Object sym, prop;
{
  register Lisp_Object tem;

  tem = Fget (sym, prop);
  if (FIXNUMP (tem))
    tem = get_doc_string (XINT (tem) > 0 ? XINT (tem) : - XINT (tem));
  return Fsubstitute_command_keys (tem);
}

DEFUN ("Snarf-documentation", Fsnarf_documentation, Ssnarf_documentation,
  1, 1, 0,
  "Used during Emacs initialization, before dumping runnable Emacs,\n\
to find pointers to doc strings stored in `etc/DOC...' and\n\
record them in function definitions.\n\
One arg, FILENAME, a string which does not include a directory.\n\
The file is found in `../etc' now; found in the `exec-directory'\n\
when doc strings are referred to later in the dumped Emacs.")
  (filename)
     Lisp_Object filename;
{
  int fd;
  char buf[1024 + 1];
  register int filled;
  register int pos;
  register char *p, *end;
  Lisp_Object sym, fun, tem;
  char *name;

  CHECK_STRING (filename, 0);

#ifndef CANNOT_DUMP
  name = (char *) alloca (XSTRING (filename)->size + 8);
  strcpy (name, "../etc/");
#else /* CANNOT_DUMP */
  CHECK_STRING (Vexec_directory, 0);
  name = (char *) alloca (XSTRING (filename)->size +
			  XSTRING (Vexec_directory)->size + 1);
  strcpy (name, XSTRING (Vexec_directory)->data);
#endif /* CANNOT_DUMP */
  strcat (name, (char *) XSTRING (filename)->data);
#ifdef VMS
#ifndef VMS4_4
  /* For VMS versions with limited file name syntax,
     convert the name to something VMS will allow.  */
  p = name;
  while (*p)
    {
      if (*p == '-')
	*p = '_';
      p++;
    }
#endif /* not VMS4_4 */
#ifdef VMS4_4
  strcpy (name, sys_translate_unix (name));
#endif /* VMS4_4 */
#endif /* VMS */

  fd = open (name, O_RDONLY, 0);
  if (fd < 0)
    report_file_error ("Opening doc string file",
		       Fcons (build_string (name), Qnil));
  Vdoc_file_name = filename;
  filled = 0;
  pos = 0;
  while (1)
    {
      if (filled < 512)
	filled += read (fd, &buf[filled], sizeof buf - 1 - filled);
      if (!filled)
	break;

      buf[filled] = 0;
      p = buf;
      end = buf + (filled < 512 ? filled : filled - 128);
      while (p != end && *p != '\037') p++;
      /* p points to ^_Ffunctionname\n or ^_Vvarname\n.  */
      if (p != end)
	{
	  end = strchr (p, '\n');
	  sym = oblookup (Vobarray, (unsigned char *) p + 2, end - p - 2);
	  if (SYMBOLP (sym))
	    {
	      if (p[1] == 'V')
		{
		  /* Install file-position as variable-documentation property
		     and make it negative for a user-variable
		     (doc starts with a `*').  */
		  Fput (sym, Qvariable_documentation,
			make_number ((pos + end + 1 - buf)
				     * (end[1] == '*' ? -1 : 1)));
		}
	      else if (p[1] == 'F')
		{
		  fun = XSYMBOL (sym)->function;

		  if (CONSP (fun) &&
		      EQ (XCONS (fun)->car, Qmacro))
		    fun = XCONS (fun)->cdr;

		  if (SUBRP (fun))
		    XSUBR (fun)->doc = (char *) - (pos + end + 1 - buf);
		  else if (CONSP (fun))
		    {
		      tem = XCONS (fun)->car;
		      if (EQ (tem, Qlambda) || EQ (tem, Qautoload))
			{
			  tem = Fcdr (Fcdr (fun));
			  if (CONSP (tem) &&
			      FIXNUMP (XCONS (tem)->car))
			    XFASTINT (XCONS (tem)->car) = (pos + end + 1 - buf);
			}
		    }
		  else if (COMPILEDP (fun))
		    {
		      if (XVECTOR (fun)->size > COMPILED_DOC_STRING &&
			  FIXNUMP (XVECTOR (fun)->contents[COMPILED_DOC_STRING]))
			XFASTINT (XVECTOR (fun)->contents[COMPILED_DOC_STRING])
			  = (pos + end + 1 - buf);
		    }
		}
	      else error ("DOC file invalid at position %d", pos);
	    }
	}
      pos += end - buf;
      filled -= end - buf;
      memcpy (buf, end, filled);
    }
  close (fd);
  return Qnil;
}

static void
verify_doc_mapper (Lisp_Object sym, Lisp_Object closure)
{
  if (!NILP (Ffboundp (sym)))
    {
      int doc = 0;
      Lisp_Object fun = XSYMBOL (sym)->function;
      if (CONSP (fun) &&
	  EQ (XCONS (fun)->car, Qmacro))
	fun = XCONS (fun)->cdr;

      if (SUBRP (fun))
	doc = (int) XSUBR (fun)->doc;
      else if (SYMBOLP (fun))
	doc = -1;
      else if (KEYMAPP (fun))
	doc = -1;
      else if (CONSP (fun))
	{
	  Lisp_Object tem = XCONS (fun)->car;
	  if (EQ (tem, Qlambda) || EQ (tem, Qautoload))
	    {
	      doc = -1;
	      tem = Fcdr (Fcdr (fun));
	      if (CONSP (tem) &&
		  FIXNUMP (XCONS (tem)->car))		  
		doc = XINT (XCONS (tem)->car);
	    }
	}
      else if (COMPILEDP (fun))
	{
	  doc = -1;
	  if (XVECTOR (fun)->size > COMPILED_DOC_STRING &&
	      FIXNUMP (XVECTOR (fun)->contents[COMPILED_DOC_STRING]))
	    doc = XFASTINT (XVECTOR (fun)->contents[COMPILED_DOC_STRING]);
	}

      if (doc == 0)
	{
	  fprintf (stderr, "Warning: doc lost for function %s.\n",
		   (char *) XSYMBOL (sym)->name->data);
	  XCONS (closure)->cdr = Qt;
	}
    }
  if (!NILP (Fboundp (sym)))
    {
      Lisp_Object doc = Fget (sym, Qvariable_documentation);
      if (FIXNUMP (doc) && XFASTINT (doc) == 0)
	{
	  fprintf (stderr, "Warning: doc lost for variable %s.\n",
		   (char *) XSYMBOL (sym)->name->data);
	  XCONS (closure)->cdr = Qt;
	}
    }
}

DEFUN ("Verify-documentation", Fverify_documentation, Sverify_documentation,
       0, 0, 0,
       "Used to make sure everything went well with Snarf-documentation.\n\
Writes to stderr if not.")
     ()
{
  Lisp_Object closure = Fcons (Qnil, Qnil);
  struct gcpro gcpro1;
  GCPRO1 (closure);
  map_obarray (Vobarray, verify_doc_mapper, closure);
  if (!NILP (Fcdr (closure)))
    fprintf (stderr, "\n\
This is usually because some files were preloaded by loaddefs.el or\n\
site-load.el, but were not passed to make-docfile by ymakefile.\n\n");
  UNGCPRO;
  return (NILP (Fcdr (closure)) ? Qt : Qnil);
}


DEFUN ("substitute-command-keys", Fsubstitute_command_keys,
  Ssubstitute_command_keys, 1, 1, 0,
  "Substitute key descriptions for command names in STRING.\n\
Return a new string which is STRING with substrings of the form \\=\\[COMMAND]\n\
replaced by either:  a keystroke sequence that will invoke COMMAND,\n\
or \"M-x COMMAND\" if COMMAND is not on any keys.\n\
Substrings of the form \\=\\{MAPVAR} are replaced by summaries\n\
\(made by describe-bindings) of the value of MAPVAR, taken as a keymap.\n\
Substrings of the form \\=\\<MAPVAR> specify to use the value of MAPVAR\n\
as the keymap for future \\=\\[COMMAND] substrings.\n\
\\=\\= quotes the following character and is discarded;\n\
thus, \\=\\=\\=\\= puts \\=\\= into the output, and \\=\\=\\=\\[ puts \\=\\[ into the output.")
  (str)
     Lisp_Object str;
{
  unsigned char *buf;
  int changed = 0;
  register unsigned char *strp;
  register unsigned char *bufp;
  register unsigned char *send;
  int bsize;
  unsigned char *new;
  Lisp_Object tem = Qnil;
  Lisp_Object keymap;
  unsigned char *start;
  int length;
  struct gcpro gcpro1;

  if (NILP (str))
    return Qnil;

  GCPRO1 (tem);

  CHECK_STRING (str, 0);
  strp = XSTRING(str)->data;
  send = strp + XSTRING(str)->size;

  keymap = current_buffer->keymap;

  bsize = XSTRING (str)->size;
  bufp = buf = (unsigned char *) xmalloc (bsize);

  while (strp < send)
    {
      if (strp[0] == '\\' && strp[1] == '=')
	{
	  /* \= quotes the next character;
	     thus, to put in \[ without its special meaning, use \=\[.  */
	  changed = 1;
	  *bufp++ = strp[2];
	  strp += 3;
	}
      else if (strp[0] == '\\' && strp[1] == '[')
	{
	  changed = 1;
	  strp += 2;		/* skip \[ */
	  start = strp;

	  while (strp < send && *strp != ']')
	    strp++;
	  length = strp - start;
	  strp++;		/* skip ] */

	  tem = Fintern (make_string ((char *) start, length), Qnil);
	  tem = Fwhere_is_internal (tem, keymap, Qt, Qnil, Qnil);

	  if (NILP (tem))	/* but not on any keys */
	    {
	      new = (unsigned char *) xrealloc (buf, bsize += 4);
	      bufp += new - buf;
	      buf = new;
	      memcpy (bufp, "M-x ", 4);
	      bufp += 4;
	      goto subst;
	    }
	  else
	    {			/* function is on a key */
	      tem = Fkey_description (tem);
	      goto subst_string;
	    }
	}
      /* \{foo} is replaced with a summary of the keymap (symeval foo).
	 \<foo> just sets the keymap used for \[cmd].  */
      else if (strp[0] == '\\' && (strp[1] == '{' || strp[1] == '<'))
	{
	  struct buffer *oldbuf;
	  Lisp_Object name;

	  changed = 1;
	  strp += 2;		/* skip \{ or \< */
	  start = strp;

	  while (strp < send && *strp != '}' && *strp != '>')
	    strp++;
	  length = strp - start;
	  strp++;			/* skip } or > */

	  /* Get the value of the keymap in TEM, or nil if undefined.
	     Do this while still in the user's current buffer
	     in case it is a local variable.  */
	  name = Fintern (make_string ((char *) start, length), Qnil);
	  tem = Fboundp (name);
	  if (! NILP (tem))
	    {
	      tem = Fsymbol_value (name);
	      if (! NILP (tem))
		tem = get_keymap (tem, 0);
	    }

	  /* Now switch to a temp buffer.  */
	  oldbuf = current_buffer;
	  internal_set_buffer (XBUFFER (Vprin1_to_string_buffer));

	  if (NILP (tem))
	    {
	      name = Fsymbol_name (name);
	      insert_string ("\nUses keymap \"");
	      insert_from_string (name, 0, XSTRING (name)->size);
	      insert_string ("\", which is not currently defined.\n");
	      if (start[-1] == '<') keymap = Qnil;
	    }
	  else if (start[-1] == '<')
	    keymap = tem;
	  else
	    describe_map_tree (tem, 1, Qnil, Qnil, 0);
	  tem = Fbuffer_string ();
	  Ferase_buffer ();
	  internal_set_buffer (oldbuf);

	subst_string:
	  start = XSTRING (tem)->data;
	  length = XSTRING (tem)->size;
	subst:
	  new = (unsigned char *) xrealloc (buf, bsize += length);
	  bufp += new - buf;
	  buf = new;
	  memcpy (bufp, start, length);
	  bufp += length;
	}
      else			/* just copy other chars */
	*bufp++ = *strp++;
    }

  if (changed)			/* don't bother if nothing substituted */
    tem = make_string ((char *) buf, bufp - buf);
  else
    tem = str;
  xfree (buf);
  UNGCPRO;
  return tem;
}

void
syms_of_doc ()
{
  DEFVAR_LISP ("internal-doc-file-name", &Vdoc_file_name,
    "Name of file containing documentation strings of built-in symbols.");
  Vdoc_file_name = Qnil;

  defsubr (&Sdocumentation);
  defsubr (&Sdocumentation_property);
  defsubr (&Ssnarf_documentation);
  defsubr (&Sverify_documentation);
  defsubr (&Ssubstitute_command_keys);
}
