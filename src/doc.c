/* Record indices of function doc strings stored in a file.
   Copyright (C) 1985, 1986, 1992, 1993 Free Software Foundation, Inc.

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
#include "intl.h"

#include <stdio.h>	/* for printf and stderr */
#include <sys/types.h>
#include <sys/file.h>	/* Must be after sys/types.h for USG and BSD4_1*/

#ifdef USG5
#include <fcntl.h>
#endif

#ifndef O_RDONLY
#define O_RDONLY 0
#endif

#include "lisp.h"
#include "bytecode.h"
#include "buffer.h"
#include "keymap.h"
#include "insdel.h"

Lisp_Object Vdoc_file_name;

Lisp_Object
get_doc_string (long filepos)
{
  char buf[512 * 32 + 1];
  register int fd;
  register char *name;
  register char *p, *p1;
  register int count;

  if (!STRINGP (Vdata_directory)
      || !STRINGP (Vdoc_file_name))
    return Qnil;

  name = (char *) alloca (string_length (XSTRING (Vdata_directory))
			  + string_length (XSTRING (Vdoc_file_name))
                          + 8);
  strcpy (name, (char *) XSTRING (Vdata_directory)->data);
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

  fd = emacs_open (name, O_RDONLY, 0);
  if (fd < 0)
    error (GETTEXT ("Cannot open doc string file \"%s\""), name);
  if (0 > lseek (fd, filepos, 0))
    {
      emacs_close (fd);
      error (GETTEXT ("Position %ld out of range in doc string file \"%s\""),
	     filepos, name);
    }
  p = buf;
  while (p != buf + sizeof buf - 1)
    {
      count = emacs_read (fd, p, 512);
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
  emacs_close (fd);
  return make_string (buf, p - buf);
}

DEFUN ("documentation", Fdocumentation, Sdocumentation, 1, 2, 0,
  "Return the documentation string of FUNCTION.\n\
Unless a non-nil second argument is given, the\n\
string is passed through `substitute-command-keys'.")
  (function, raw)
     Lisp_Object function, raw;
{
  Lisp_Object fun;
  Lisp_Object doc;

  fun = Findirect_function (function);
  if (SUBRP (fun))
    {
      if (XSUBR (fun)->doc == 0)
	return (Qnil);
      else if ((LISP_WORD_TYPE) XSUBR (fun)->doc >= 0)
	doc = build_string (XSUBR (fun)->doc);
      else
        doc = get_doc_string (- (long) XSUBR (fun)->doc);
    }
  else if (COMPILEDP (fun))
    {
      Lisp_Object tem;
      struct Lisp_Bytecode *b = XBYTECODE (fun);
      if (! (b->flags.documentationp))
        return (Qnil);
      tem = bytecode_documentation (b);
      if (STRINGP (tem))
	doc = tem;
      else if (FIXNUMP (tem) && XINT (tem) >= 0)
	doc = get_doc_string ((long) XINT (tem));
      else
        return (Qnil);
    }
  else if (KEYMAPP (fun))
    return build_string (GETTEXT ("Prefix command (definition is a keymap of subcommands)."));
  else if (STRINGP (fun) || VECTORP (fun))
    return build_string (GETTEXT ("Keyboard macro."));
  else if (!CONSP (fun))
    return Fsignal (Qinvalid_function, list1 (fun));
  else
    {
      Lisp_Object funcar = Fcar (fun);

      if (!SYMBOLP (funcar))
	return Fsignal (Qinvalid_function, list1 (fun));
      else if (EQ (funcar, Qlambda)
             || EQ (funcar, Qautoload))
	{
	  Lisp_Object tem = Fcar (Fcdr (Fcdr (fun)));
	  if (STRINGP (tem))
	    doc = tem;
	  else if (FIXNUMP (tem) && XINT (tem) >= 0)
	    doc = get_doc_string ((long) XINT (tem));
	  else
	    return (Qnil);
	}
#ifdef MOCKLISP_SUPPORT
    else if (XSYMBOL (funcar) == XSYMBOL (Qmocklisp))
      return (Qnil);
#endif
    else if (XSYMBOL (funcar) == XSYMBOL (Qmacro))
      return (Fdocumentation (Fcdr (fun), raw));
    else
      return Fsignal (Qinvalid_function, list1 (fun));
  }

  if (NILP (raw))
    {
      struct gcpro gcpro1;
#ifdef I18N3
      Lisp_Object domain = Qnil;
      if (COMPILEDP (fun))
	domain = Fcompiled_function_domain (fun);
      if (NILP (domain))
	doc = Fgettext (doc);
      else
	doc = Fdgettext (domain, doc);
#endif

      GCPRO1 (doc);
      doc = Fsubstitute_command_keys (doc);
      UNGCPRO;
    }
  return (doc);
}

DEFUN ("documentation-property", Fdocumentation_property, 
       Sdocumentation_property, 2, 3, 0,
  "Return the documentation string that is SYMBOL's PROP property.\n\
This is like `get', but it can refer to strings stored in the\n\
`data-directory/DOC' file; and if the value is a string, it is passed\n\
through `substitute-command-keys'.  A non-nil third argument avoids this\n\
translation.")
  (sym, prop, raw)
     Lisp_Object sym, prop, raw;
{
  register Lisp_Object doc;
#ifdef I18N3
  register Lisp_Object domain;
#endif

  doc = Fget (sym, prop, Qnil);
  if (FIXNUMP (doc))
    doc = get_doc_string ((long) ((XINT (doc) > 0)
                                  ? XINT (doc) : 
                                  - XINT (doc)));
#ifdef I18N3
  if (!NILP (doc))
    {
      domain = Fget (sym, Qvariable_domain, Qnil);
      if (NILP (domain))
	doc = Fgettext (doc);
      else
	doc = Fdgettext (domain, doc);
    }
#endif    
  if (NILP (raw) && STRINGP (doc))
    doc = Fsubstitute_command_keys (doc);
  return (doc);
}

static void
weird_doc (Lisp_Object sym, CONST char *weirdness, CONST char *type, int pos)
{
#ifdef ENERGIZE /* hide kludgery... */
  if (!strcmp (weirdness, GETTEXT ("duplicate"))) return;
#endif
  message (GETTEXT ("Note: Strange doc (%s) for %s %s @ %d"),
           weirdness, type, XSYMBOL (sym)->name->data, pos);
}


DEFUN ("Snarf-documentation", Fsnarf_documentation, Ssnarf_documentation,
  1, 1, 0,
  "Used during Emacs initialization, before dumping runnable Emacs,\n\
to find pointers to doc strings stored in `.../etc/DOC' and\n\
record them in function definitions.\n\
One arg, FILENAME, a string which does not include a directory.\n\
The file is written to `../etc', and later found in `data-directory'\n\
when doc strings are referred to in the dumped Emacs.")
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

#ifndef CANNOT_DUMP
  if (!purify_flag)
    error (GETTEXT ("Snarf-documentation can only be called in an undumped Emacs"));
#endif

  CHECK_STRING (filename, 0);

#ifndef CANNOT_DUMP
  name = (char *) alloca (string_length (XSTRING (filename)) + 14);
  strcpy (name, "../etc/");
#else /* CANNOT_DUMP */
  CHECK_STRING (Vdata_directory, 0);
  name = (char *) alloca (string_length (XSTRING (filename)) 
                          + string_length (XSTRING (Vdata_directory))
                          + 1);
  strcpy (name, XSTRING (Vdata_directory)->data);
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

  fd = emacs_open (name, O_RDONLY, 0);
  if (fd < 0)
    report_file_error (GETTEXT ("Opening doc string file"),
		       Fcons (build_string (name), Qnil));
  Vdoc_file_name = filename;
  filled = 0;
  pos = 0;
  while (1)
    {
      if (filled < 512)
	filled += emacs_read (fd, &buf[filled], sizeof buf - 1 - filled);
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
              Lisp_Object offset = make_number (pos + end + 1 - buf);
	      /* Attach a docstring to a variable */
	      if (p[1] == 'V')
		{
		  /* Install file-position as variable-documentation property
		     and make it negative for a user-variable
		     (doc starts with a `*').  */
		  Lisp_Object old = Fget (sym, Qvariable_documentation, Qzero);
                  if (!EQ (old, Qzero))
		    {
		      weird_doc (sym, GETTEXT ("duplicate"),
				 GETTEXT ("variable"), pos);
		      /* In the case of duplicate doc file entries, always
			 take the later one.  But if the doc is not an int
			 (a string, say) leave it alone. */
		      if (!FIXNUMP (old))
			goto weird;
		    }
		  Fput (sym, Qvariable_documentation,
                        ((end[1] == '*') 
                         ? make_number (- XINT (offset))
                         : offset));
		}
	      /* Attach a docstring to a function.
                 The type determines where the docstring is stored.  */
	      else if (p[1] == 'F')
		{
                  fun = XSYMBOL (sym)->function;/*indirect_function (sym,0);*/

		  if (CONSP (fun) && EQ (XCONS (fun)->car, Qmacro))
		    fun = XCONS (fun)->cdr;

                  if (EQ (fun, Qunbound))
                  {
                    /* May have been #if'ed out or something */
                    weird_doc (sym, GETTEXT ("not fboundp"),
			       GETTEXT ("function"), pos);
                    goto weird;
                  }
		  else if (SUBRP (fun))
                  {
                    /* Lisp_Subrs have a slot for it.  */
                    if (XSUBR (fun)->doc)
		      {
			weird_doc (sym, GETTEXT ("duplicate"),
				   GETTEXT ("subr"), pos);
			goto weird;
		      }
		    XSUBR (fun)->doc = (char *) (- XINT (offset));
                  }
		  else if (CONSP (fun))
		    {
                      /* If it's a lisp form, stick it in the form.  */
		      tem = XCONS (fun)->car;
		      if (EQ (tem, Qlambda) || EQ (tem, Qautoload))
			{
			  tem = Fcdr (Fcdr (fun));
			  if (CONSP (tem) &&
			      FIXNUMP (XCONS (tem)->car))
			    {
			      Lisp_Object old = XCONS (tem)->car;
			      if (!EQ (old, Qzero))
				{
				  weird_doc (sym, GETTEXT ("duplicate"),
					     (EQ (tem, Qlambda)
					      ? GETTEXT ("lambda")
					      : GETTEXT ("autoload")),
					     pos);
				  /* In the case of duplicate doc file entries,
				     always take the later one.  But if the doc
				     is not an int (a string, say) leave it
				     alone. */
				  if (!FIXNUMP (old))
				    goto weird;
				}
			      XCONS (tem)->car = offset;
			    }
                          else goto weird_function;
                        }
                      else goto weird_function;
		    }
		  else if (COMPILEDP (fun))
		    {
                      /* Bytecode objects sometimes have slots for it.  */
                      struct Lisp_Bytecode *b = XBYTECODE (fun);

		      /* This bytecode object must have a slot for the
			 docstring, since we've found a docstring for it.
		         Unless there were multiple definitions of it, and
			 the latter one didn't have any doc, which is a legal
			 if slightly bogus situation, so don't blow up. */

                      if (! (b->flags.documentationp))
			{
			  weird_doc (sym, GETTEXT ("no doc slot"),
				     GETTEXT ("bytecode"), pos);
			  goto weird;
			}
		      else
			{
			  Lisp_Object old = bytecode_documentation (b);
			  if (!EQ (old, Qzero))
			    {
			      weird_doc (sym, GETTEXT ("duplicate"),
					 GETTEXT ("bytecode"), pos);
			      /* In the case of duplicate doc file entries,
				 always take the later one.  But if the doc is
				 not an int (a string, say) leave it alone. */
			      if (!FIXNUMP (old))
				goto weird;
			    }
			  set_bytecode_documentation (b, offset);
			}
                    }
                  else
                    {
                      /* Otherwise the function is undefined or
                         otherwise weird.   Ignore it. */
                    weird_function:
                      weird_doc (sym, GETTEXT ("weird function"),
				 GETTEXT ("function"), pos);
                      goto weird;
                    }
                }
	      else
                {
                /* lose: */
                  error (GETTEXT ("DOC file invalid at position %d"), pos);
                weird:
                  /* goto lose */;
                }
            }
	}
      pos += end - buf;
      filled -= end - buf;
      memcpy (buf, end, filled);
    }
  emacs_close (fd);
  return Qnil;
}


#if 1	/* Don't warn about functions whose doc was lost because they were
	   wrapped by advice-freeze.el... */
static int
kludgily_ignore_lost_doc_p (Lisp_Object sym)
{
# define kludge_prefix "ad-Orig-"
  return (string_length (XSYMBOL (sym)->name) > sizeof (kludge_prefix) &&
	  !strncmp ((char *) XSYMBOL (sym)->name->data, kludge_prefix,
		    sizeof (kludge_prefix) - 1));
# undef kludge_prefix
}
#else
# define kludgily_ignore_lost_doc_p(sym) 0
#endif


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
	doc = (LISP_WORD_TYPE) XSUBR (fun)->doc;
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
          struct Lisp_Bytecode *b = XBYTECODE (fun);
          if (! (b->flags.documentationp))
            doc = -1;
          else
            {
              Lisp_Object tem = bytecode_documentation (b);
              if (FIXNUMP (tem))
                doc = XINT (tem);
            }
	}

      if (doc == 0 && !kludgily_ignore_lost_doc_p (sym))
	{
	  fprintf (stderr, GETTEXT ("Warning: doc lost for function %s.\n"),
		   (char *) XSYMBOL (sym)->name->data);
	  XCONS (closure)->cdr = Qt;
	}
    }
  if (!NILP (Fboundp (sym)))
    {
      Lisp_Object doc = Fget (sym, Qvariable_documentation, Qnil);
      if (FIXNUMP (doc) && XFASTINT (doc) == 0)
	{
	  fprintf (stderr, GETTEXT ("Warning: doc lost for variable %s.\n"),
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
    fprintf (stderr, GETTEXT ("\n\
This is usually because some files were preloaded by loaddefs.el or\n\
site-load.el, but were not passed to make-docfile by ymakefile.\n\n"));
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
  register unsigned char *strdata;
  register unsigned char *bufp;
  int strlength;
  int idx;
  int bsize;
  unsigned char *new;
  Lisp_Object tem = Qnil;
  Lisp_Object keymap;
  unsigned char *start;
  int length;
  Lisp_Object name;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

  if (NILP (str))
    return Qnil;

  CHECK_STRING (str, 0);
  tem = Qnil;
  keymap = Qnil;
  name = Qnil;
  GCPRO4 (str, tem, keymap, name);

  keymap = current_buffer->keymap;

  strlength = string_length (XSTRING (str));
  bsize = strlength;
  buf = (unsigned char *) xmalloc (bsize);
  bufp = buf;

  /* Have to reset strdata every time GC might be called */
  strdata = (unsigned char *) XSTRING (str)->data;
  for (idx = 0; idx < strlength; )
    {
      unsigned char *strp = strdata + idx;
  
      if (strp[0] != '\\')
	{
	  /* just copy other chars */
	  *bufp++ = *strp;
	  idx++;
	}
      else switch (strp[1])
	{
	default:
	  {
	    /* just copy unknown escape sequences */
	    *bufp++ = *strp;
	    idx++;
	    break;
	  }
	case '=':
	  {
	    /* \= quotes the next character;
	       thus, to put in \[ without its special meaning, use \=\[.  */
	    changed = 1;
	    *bufp++ = strp[2];
	    idx += 3;
	    break;
	  }
	case '[':
	  {
	    changed = 1;
	    idx += 2;		/* skip \[ */
	    strp += 2;
	    start = strp;

	    while ((idx < strlength)
		   && *strp != ']')
	      { 
		strp++; 
		idx++; 
	      }
	    length = strp - start;
	    idx++;		/* skip ] */

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
	case '{':
	case '<':
	  {
	    /* \{foo} is replaced with a summary of keymap (symbol-value foo).
	       \<foo> just sets the keymap used for \[cmd].  */
	    struct buffer *oldbuf;

	    changed = 1;
	    idx += 2;		/* skip \{ or \< */
	    strp += 2;
	    start = strp;

	    while ((idx < strlength)
		   && *strp != '}' && *strp != '>')
	      { 
		strp++; 
		idx++; 
	      }
	    length = strp - start;
	    idx++;		/* skip } or > */

	    /* Get the value of the keymap in TEM, or nil if undefined.
	       Do this while still in the user's current buffer
	       in case it is a local variable.  */
	    name = Fintern (make_string ((char *) start, length), Qnil);
	    tem = Fboundp (name);
	    if (! NILP (tem))
	      {
		tem = Fsymbol_value (name);
		if (! NILP (tem))
		  tem = get_keymap (tem, 0, 1);
	      }

	    /* Now switch to a temp buffer.  */
	    oldbuf = current_buffer;
	    set_buffer_internal (XBUFFER (Vprin1_to_string_buffer));

	    if (NILP (tem))
	      {
		char buf[255], *b = buf;
		*b++ = '\n';
		sprintf (b, GETTEXT (
		"Uses keymap \"%s\", which is not currently defined."),
			 (char *) XSTRING (Fsymbol_name (name))->data);
		b += strlen (b);
		*b++ = '\n';
		*b++ = 0;
		insert_string (buf);

		if (start[-1] == '<') keymap = Qnil;
	      }
	    else if (start[-1] == '<')
	      keymap = tem;
	    else
	      describe_map_tree (tem, 1, Qnil, 0);
	    tem = Fbuffer_string ();
	    Ferase_buffer ();
	    set_buffer_internal (oldbuf);
	    goto subst_string;

	  subst_string:
	    start = XSTRING (tem)->data;
	    length = string_length (XSTRING (tem));
	  subst:
	    bsize += length;
	    new = (unsigned char *) xrealloc (buf, bsize);
	    bufp += new - buf;
	    buf = new;
	    memcpy (bufp, start, length);
	    bufp += length;

	    /* Reset STRDATA in case gc relocated it.  */
	    strdata = (unsigned char *) XSTRING (str)->data;

	    break;
	  }
	}
    }

  if (changed)			/* don't bother if nothing substituted */
    tem = make_string ((char *) buf, bufp - buf);
  else
    tem = str;
  xfree (buf);
  UNGCPRO;
  return (tem);
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
