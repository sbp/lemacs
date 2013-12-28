/* Lisp object printing and output streams.
   Copyright (C) 1985, 1986, 1988 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
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

#include "lisp.h"

#ifndef standalone
#include "buffer.h"
#include "extents.h"
#include "screen.h"
#include "window.h"
#include "process.h"
#include "events.h"
#include "keymap.h"

extern void format_event_object ();

#endif /* not standalone */

Lisp_Object Vstandard_output, Qstandard_output;

#ifdef LISP_FLOAT_TYPE
Lisp_Object Vfloat_output_format, Qfloat_output_format;
#endif /* LISP_FLOAT_TYPE */

/* Avoid actual stack overflow in print.  */
int print_depth;

/* Maximum length of list to print in full; noninteger means
   effectively infinity */

Lisp_Object Vprint_length;

/* Maximum depth of list to print in full; noninteger means
   effectively infinity.  */

Lisp_Object Vprint_level;

/* Nonzero means print newlines in strings as \n.  */

int print_escape_newlines;
int print_readably;
int print_gensym;

Lisp_Object Qprint_escape_newlines;
Lisp_Object Qprint_readably;

/* Nonzero means print newline before next minibuffer message.
   Defined in xdisp.c */

extern int noninteractive_need_newline;
#ifdef MAX_PRINT_CHARS
static int print_chars;
static int max_print;
#endif /* MAX_PRINT_CHARS */

#define PRINTER_BUFFER_SIZE 4096
static char *printer_buffer;
static char *printer_buffer_fill;
static char *printer_buffer_end;


/* Low level output routines for charaters and strings */

/* Lisp functions to do output using a stream
 must have the stream in a variable called printcharfun
 and must start with PRINTPREPARE and end with PRINTFINISH.
 Use PRINTCHAR to output one character,
 or call strout to output a block of characters.
 Also, each one must have the declarations
   struct buffer *old = current_buffer;
   int old_point = -1, start_point;
   Lisp_Object original;
*/ 

#define PRINTPREPARE \
   printer_buffer = alloca (PRINTER_BUFFER_SIZE); \
   printer_buffer_fill = printer_buffer; \
   printer_buffer_end = printer_buffer + PRINTER_BUFFER_SIZE; \
   original = printcharfun; \
   if (NILP (printcharfun)) printcharfun = Qt; \
   if (XTYPE (printcharfun) == Lisp_Buffer) \
     { if (XBUFFER (printcharfun) != current_buffer) Fset_buffer (printcharfun); \
       printcharfun = Qnil;}\
   if (XTYPE (printcharfun) == Lisp_Marker) \
     { if (XMARKER (original)->buffer != current_buffer) \
         internal_set_buffer (XMARKER (original)->buffer); \
       old_point = point; \
       SET_PT (marker_position (printcharfun)); \
       start_point = point; \
       printcharfun = Qnil;}

#define PRINTFINISH \
   dump_printer_buffer (); \
   if (XTYPE (original) == Lisp_Marker) \
     Fset_marker (original, make_number (point), Qnil); \
   if (old_point >= 0) \
     SET_PT ((old_point >= start_point ? point - start_point : 0) + old_point); \
   if (old != current_buffer) \
     internal_set_buffer (old)

#define PRINTCHAR(ch) printchar (ch, printcharfun)

static void
insert_in_printer_buffer (string, char_count)
     char *string;
     int char_count;
{
  char *new_end;

   while ((new_end = printer_buffer_fill + char_count) > printer_buffer_end)
     {
       int this_char_count = char_count - (new_end - printer_buffer_end);

       bcopy (string, printer_buffer_fill, this_char_count);
       insert_raw_string (printer_buffer, printer_buffer_end - printer_buffer);
       printer_buffer_fill = printer_buffer;
       string += this_char_count;
       char_count -= this_char_count;
     }
  bcopy (string, printer_buffer_fill, char_count);
  printer_buffer_fill += char_count;
}

static void
dump_printer_buffer ()
{
  insert_raw_string (printer_buffer, printer_buffer_fill - printer_buffer);
  printer_buffer_fill = printer_buffer;
}
       
  

/* Index of first unused element of above */
static int printbufidx;

static void
printchar (ch, fun)
     unsigned char ch;
     Lisp_Object fun;
{
  Lisp_Object ch1;

#ifdef MAX_PRINT_CHARS
  if (max_print)
    print_chars++;
#endif /* MAX_PRINT_CHARS */
#ifndef standalone
  if (EQ (fun, Qnil))
    {
      QUIT;
      insert_in_printer_buffer ((char *) &ch, 1);
      return;
    }

  if (EQ (fun, Qt))
    {
      if (noninteractive)
	{
	  putchar (ch);
	  noninteractive_need_newline = 1;
	  return;
	}

      if (echo_area_glyphs != SCREEN_MESSAGE_BUF (selected_screen))
	{
	  echo_area_glyphs = SCREEN_MESSAGE_BUF (selected_screen);
	  printbufidx = 0;
	}

      if (printbufidx < SCREEN_WIDTH (selected_screen) - 1)
	SCREEN_MESSAGE_BUF (selected_screen)[printbufidx++] = ch;
      SCREEN_MESSAGE_BUF (selected_screen)[printbufidx] = 0;

      return;
    }
#endif /* not standalone */

  XFASTINT (ch1) = ch;
  call1 (fun, ch1);
}

static void
strout (ptr, size, printcharfun)
     char *ptr;
     int size;
     Lisp_Object printcharfun;
{
  int i = 0;

  if (EQ (printcharfun, Qnil))
    {
      insert_in_printer_buffer (ptr, size >= 0 ? size : strlen (ptr));
#ifdef MAX_PRINT_CHARS
      if (max_print)
        print_chars += size >= 0 ? size : strlen(ptr);
#endif /* MAX_PRINT_CHARS */
      return;
    }
  if (EQ (printcharfun, Qt))
    {
      i = size >= 0 ? size : strlen (ptr);
#ifdef MAX_PRINT_CHARS
      if (max_print)
        print_chars += i;
#endif /* MAX_PRINT_CHARS */

      if (noninteractive)
	{
	  fwrite (ptr, 1, i, stdout);
	  noninteractive_need_newline = 1;
	  return;
	}

      if (echo_area_glyphs != SCREEN_MESSAGE_BUF (selected_screen))
	{
	  echo_area_glyphs = SCREEN_MESSAGE_BUF (selected_screen);
	  printbufidx = 0;
	}

      if (i > SCREEN_WIDTH (selected_screen) - printbufidx - 1)
	i = SCREEN_WIDTH (selected_screen) - printbufidx - 1;
      bcopy (ptr, &SCREEN_MESSAGE_BUF (selected_screen) [printbufidx], i);
      printbufidx += i;
      SCREEN_MESSAGE_BUF (selected_screen) [printbufidx] = 0;

      return;
    }

  if (size >= 0)
    while (i < size)
      PRINTCHAR (ptr[i++]);
  else
    while (ptr[i])
      PRINTCHAR (ptr[i++]);
}

DEFUN ("write-char", Fwrite_char, Swrite_char, 1, 2, 0,
  "Output character CHAR to stream STREAM.\n\
STREAM defaults to the value of `standard-output' (which see).")
  (ch, printcharfun)
     Lisp_Object ch, printcharfun;
{
  struct buffer *old = current_buffer;
  int old_point = -1;
  int start_point;
  Lisp_Object original;

  if (NILP (printcharfun))
    printcharfun = Vstandard_output;
  CHECK_NUMBER (ch, 0);
  PRINTPREPARE;
  PRINTCHAR (XINT (ch));
  PRINTFINISH;
  return ch;
}

write_string (data, size)
     char *data;
     int size;
{
  struct buffer *old = current_buffer;
  Lisp_Object printcharfun;
  int old_point = -1;
  int start_point;
  Lisp_Object original;

  printcharfun = Vstandard_output;

  PRINTPREPARE;
  strout (data, size, printcharfun);
  PRINTFINISH;
}

write_string_1 (data, size, printcharfun)
     char *data;
     int size;
     Lisp_Object printcharfun;
{
  struct buffer *old = current_buffer;
  int old_point = -1;
  int start_point;
  Lisp_Object original;

  PRINTPREPARE;
  strout (data, size, printcharfun);
  PRINTFINISH;
}


#ifndef standalone

void
temp_output_buffer_setup (bufname)
    char *bufname;
{
  register struct buffer *old = current_buffer;
  register Lisp_Object buf;

  Fset_buffer (Fget_buffer_create (build_string (bufname)));

  current_buffer->read_only = Qnil;
  Ferase_buffer ();

  XSET (buf, Lisp_Buffer, current_buffer);
  specbind (Qstandard_output, buf);

  internal_set_buffer (old);
}

Lisp_Object
internal_with_output_to_temp_buffer (bufname, function, args, same_screen)
     char *bufname;
     Lisp_Object (*function) ();
     Lisp_Object args;
     Lisp_Object same_screen;
{
  int count = specpdl_ptr - specpdl;
  Lisp_Object buf, val;

  temp_output_buffer_setup (bufname);
  buf = Vstandard_output;

  val = (*function) (args);

  temp_output_buffer_show (buf, same_screen);

  unbind_to (count);
  return val;
}

DEFUN ("with-output-to-temp-buffer", Fwith_output_to_temp_buffer, Swith_output_to_temp_buffer,
       1, UNEVALLED, 0,
  "Bind `standard-output' to buffer BUFNAME, eval BODY, then show that buffer.\n\
The buffer is cleared out initially, and marked as unmodified when done.\n\
All output done by BODY is inserted in that buffer by default.\n\
The buffer is displayed in another window, but not selected.\n\
The value of the last form in BODY is returned.\n\
If BODY does not finish normally, the buffer BUFNAME is not displayed.\n\n\
If variable `temp-buffer-show-hook' is non-nil, call it at the end\n\
to get the buffer displayed.  It gets one argument, the buffer to display.")
  (args)
     Lisp_Object args;
{
  struct gcpro gcpro1;
  Lisp_Object name;
  int count = specpdl_ptr - specpdl;
  Lisp_Object buf, val;

  GCPRO1(args);
  name = Feval (Fcar (args));
  UNGCPRO;

  CHECK_STRING (name, 0);
  temp_output_buffer_setup ((char *) XSTRING (name)->data);
  buf = Vstandard_output;

  val = Fprogn (Fcdr (args));

  temp_output_buffer_show (buf, Qnil);

  unbind_to (count);
  return val;
}
#endif /* not standalone */

static void print ();

DEFUN ("terpri", Fterpri, Sterpri, 0, 1, 0,
  "Output a newline to STREAM.\n\
If STREAM is omitted or nil, the value of `standard-output' is used.")
  (printcharfun)
     Lisp_Object printcharfun;
{
  struct buffer *old = current_buffer;
  int old_point = -1;
  int start_point;
  Lisp_Object original;

  if (NILP (printcharfun))
    printcharfun = Vstandard_output;
  PRINTPREPARE;
  PRINTCHAR ('\n');
  PRINTFINISH;
  return Qt;
}

DEFUN ("prin1", Fprin1, Sprin1, 1, 2, 0,
  "Output the printed representation of OBJECT, any Lisp object.\n\
Quoting characters are printed when needed to make output that `read'\n\
can handle, whenever this is possible.\n\
Output stream is STREAM, or value of `standard-output' (which see).")
  (obj, printcharfun)
     Lisp_Object obj, printcharfun;
{
  struct buffer *old = current_buffer;
  int old_point = -1;
  int start_point;
  Lisp_Object original;

#ifdef MAX_PRINT_CHARS
  max_print = 0;
#endif /* MAX_PRINT_CHARS */
  if (NILP (printcharfun))
    printcharfun = Vstandard_output;
  PRINTPREPARE;
  print_depth = 0;
  print (obj, printcharfun, 1);
  PRINTFINISH;
  return obj;
}

/* a buffer which is used to hold output being built by prin1-to-string */
Lisp_Object Vprin1_to_string_buffer;

DEFUN ("prin1-to-string", Fprin1_to_string, Sprin1_to_string, 1, 2, 0,
  "Return a string containing the printed representation of OBJECT,\n\
any Lisp object.  Quoting characters are used when needed to make output\n\
that `read' can handle, whenever this is possible, unless the optional\n\
second argument NOESCAPE is non-nil.")
  (obj, noescape)
     Lisp_Object obj, noescape;
{
  struct buffer *old = current_buffer;
  int old_point = -1;
  int start_point;
  Lisp_Object original, printcharfun;

  printcharfun = Vprin1_to_string_buffer;
  PRINTPREPARE;
  print_depth = 0;
  print (obj, printcharfun, NILP (noescape));
  /* Make Vprin1_to_string_buffer be the default buffer after PRINTFINSH */
  PRINTFINISH;
  internal_set_buffer (XBUFFER (Vprin1_to_string_buffer));
  {
    struct gcpro gcpro1;

    GCPRO1 (obj);
    obj = Fbuffer_string ();
    Ferase_buffer ();
    internal_set_buffer (old);
    UNGCPRO;
  }
  return obj;
}

DEFUN ("princ", Fprinc, Sprinc, 1, 2, 0,
  "Output the printed representation of OBJECT, any Lisp object.\n\
No quoting characters are used; no delimiters are printed around\n\
the contents of strings.\n\
Output stream is STREAM, or value of standard-output (which see).")
  (obj, printcharfun)
     Lisp_Object obj, printcharfun;
{
  struct buffer *old = current_buffer;
  int old_point = -1;
  int start_point;
  Lisp_Object original;

  if (NILP (printcharfun))
    printcharfun = Vstandard_output;
  PRINTPREPARE;
  print_depth = 0;
  print (obj, printcharfun, 0);
  PRINTFINISH;
  return obj;
}

DEFUN ("print", Fprint, Sprint, 1, 2, 0,
  "Output the printed representation of OBJECT, with newlines around it.\n\
Quoting characters are printed when needed to make output that `read'\n\
can handle, whenever this is possible.\n\
Output stream is STREAM, or value of `standard-output' (which see).")
  (obj, printcharfun)
     Lisp_Object obj, printcharfun;
{
  struct buffer *old = current_buffer;
  int old_point = -1;
  int start_point;
  Lisp_Object original;

#ifdef MAX_PRINT_CHARS
  print_chars = 0;
  max_print = MAX_PRINT_CHARS;
#endif /* MAX_PRINT_CHARS */
  if (NILP (printcharfun))
    printcharfun = Vstandard_output;
  PRINTPREPARE;
  print_depth = 0;
  PRINTCHAR ('\n');
  print (obj, printcharfun, 1);
  PRINTCHAR ('\n');
  PRINTFINISH;
#ifdef MAX_PRINT_CHARS
  max_print = 0;
  print_chars = 0;
#endif /* MAX_PRINT_CHARS */
  return obj;
}

#ifdef LISP_FLOAT_TYPE

void
float_to_string (buf, data)
     char *buf;
/*
 * This buffer should be at least as large as the max string size of the
 * largest float, printed in the biggest notation.  This is undoubtably
 * 20d float_output_format, with the negative of the C-constant "HUGE"
 * from <math.h>.
 * 
 * On the vax the worst case is -1e38 in 20d format which takes 61 bytes.
 * 
 * I assume that IEEE-754 format numbers can take 329 bytes for the worst
 * case of -1e307 in 20d float_output_format. What is one to do (short of
 * re-writing _doprnt to be more sane)?
 * 			-wsr
 */
     double data;
{
  register unsigned char *cp, c;
  register int width;
      
  if (NILP (Vfloat_output_format)
      || XTYPE (Vfloat_output_format) != Lisp_String)
  lose:
    sprintf (buf, "%.16g", data);
  else			/* oink oink */
    {
      /* Check that the spec we have is fully valid.
	 This means not only valid for printf,
	 but meant for floats, and reasonable.  */
      cp = XSTRING (Vfloat_output_format)->data;

      if (cp[0] != '%')
	goto lose;
      if (cp[1] != '.')
	goto lose;

      cp += 2;
      for (width = 0;
	   ((c = *cp) >= '0' && c <= '9');
	   cp++)
	{
	  width *= 10;
	  width += c - '0';
	}

      if (*cp != 'e' && *cp != 'f' && *cp != 'g')
	goto lose;

      if (width < (*cp != 'e') || width > DBL_DIG)
	goto lose;

      if (cp[1] != 0)
	goto lose;

      sprintf (buf, (char *)XSTRING (Vfloat_output_format)->data, data);
    }

  /* added by jwz: don't allow "1.0" to print as "1"; that destroys
     the read-equivalence of lisp objects.  (* x 1) and (* x 1.0) do
     not do the same thing, so it's important that the printed
     representation of that form not be corrupted by the printer.
   */
  {
    register char *s;
    for (s = buf; *s; s++)
      /* if there's a non-digit, then there is a decimal point, or
	 it's in exponential notation, both of which are ok. */
      if (*s < '0' || *s > '9')
	return;
    /* otherwise, we need to hack it. */
    *s++ = '.';
    *s++ = '0';
    *s = 0;
  }
}
#endif /* LISP_FLOAT_TYPE */

static void
print (obj, printcharfun, escapeflag)
#ifndef RTPC_REGISTER_BUG
     register Lisp_Object obj;
#else
     Lisp_Object obj;
#endif
     register Lisp_Object printcharfun;
     int escapeflag;
{
  char buf[30];

  QUIT;

  print_depth++;

  if (print_depth > 200)
    error ("Apparently circular structure being printed");
#ifdef MAX_PRINT_CHARS
  if (max_print && print_chars > max_print)
    {
      PRINTCHAR ('\n');
      print_chars = 0;
    }
#endif /* MAX_PRINT_CHARS */

#ifdef SWITCH_ENUM_BUG
  switch ((int) XTYPE (obj))
#else
  switch (XTYPE (obj))
#endif
    {
    default:
      if (print_readably)
	error ("printing illegal data type #o%3o", (int) XTYPE (obj));
      /* We're in trouble if this happens!
	 Probably should just abort () */
      strout ("#<EMACS BUG: ILLEGAL DATATYPE ", -1, printcharfun);
      sprintf (buf, "(#o%3o)", (int) XTYPE (obj));
      strout (buf, -1, printcharfun);
      strout (" Save your buffers immediately and please report this bug>",
	      -1, printcharfun);
      break;

#ifdef LISP_FLOAT_TYPE
    case Lisp_Float:
      {
	char pigbuf[350];	/* see comments in float_to_string */

	float_to_string (pigbuf, XFLOAT(obj)->data);
	strout (pigbuf, -1, printcharfun);
      }
      break;
#endif /* LISP_FLOAT_TYPE */

    case Lisp_Int:
      sprintf (buf, "%d", XINT (obj));
      strout (buf, -1, printcharfun);
      break;

    case Lisp_String:
      if (!escapeflag)
	strout ((char *) XSTRING (obj)->data, XSTRING (obj)->size, printcharfun);
      else
	{
	  register int i;
	  register unsigned char *p = XSTRING (obj)->data;
	  register unsigned char c;

	  PRINTCHAR ('\"');
	  for (i = XSTRING (obj)->size; i > 0; i--)
	    {
	      QUIT;
	      c = *p++;
	      if (c == '\n' && print_escape_newlines)
		{
		  PRINTCHAR ('\\');
		  PRINTCHAR ('n');
		}
	      else
		{
		  if (c == '\"' || c == '\\')
		    PRINTCHAR ('\\');
		  PRINTCHAR (c);
		}
	    }
	  PRINTCHAR ('\"');
	}
      break;

    case Lisp_Symbol:
      {
	register int confusing;
	register unsigned char *p = XSYMBOL (obj)->name->data;
	register unsigned char *end = p + XSYMBOL (obj)->name->size;
	register unsigned char c;

	if (print_gensym) {
	  Lisp_Object tem = oblookup (Vobarray, p, end-p);
	  if (!EQ (tem, obj))
	    /* (read) would return a new symbol with the same name.
	       This isn't quite correct, because that symbol might not
	       really be uninterned (it might be interned in some other
	       obarray) but there's no way to win in that case without
	       implementing a real package system.
	     */
	    strout ("#:", 2, printcharfun);
	}

	if (p != end && (*p == '-' || *p == '+')) p++;
        if (p == end)
	  confusing = 0;
	else
	  {
	    while (p != end && *p >= '0' && *p <= '9')
	      p++;
	    confusing = (end == p);
	  }

	p = XSYMBOL (obj)->name->data;
	while (p != end)
	  {
	    QUIT;
	    c = *p++;
	    if (escapeflag)
	      {
		if (c == '\"' || c == '\\' || c == '\'' || c == ';' || c == '#' ||
		    c == '(' || c == ')' || c == ',' || c =='.' || c == '`' ||
		    c == '[' || c == ']' || c == '?' || c <= 040 || confusing)
		  PRINTCHAR ('\\'), confusing = 0;
	      }
	    PRINTCHAR (c);
	  }
      }
      break;

    case Lisp_Cons:
      /* If deeper than spec'd depth, print placeholder.  */
      if (XTYPE (Vprint_level) == Lisp_Int
	  && print_depth > XINT (Vprint_level))
	{
	  strout ("...", -1, printcharfun);
	  break;
	}

      /* If print_readably is on, print (quote -foo-) as '-foo- */
      if (print_readably &&
	  EQ (XCONS (obj)->car, Qquote) &&
	  XTYPE (XCONS (obj)->cdr) == Lisp_Cons &&
	  NILP (XCONS (XCONS (obj)->cdr)->cdr)) {
	PRINTCHAR ('\'');
	print (XCONS (XCONS (obj)->cdr)->car, printcharfun, escapeflag);
	break;
      }

      PRINTCHAR ('(');
      {
	register int i = 0;
	register int max = 0;

	if (XTYPE (Vprint_length) == Lisp_Int)
	  max = XINT (Vprint_length);
	while (CONSP (obj))
	  {
	    if (i++)
	      PRINTCHAR (' ');
	    if (max && i > max)
	      {
		strout ("...", 3, printcharfun);
		break;
	      }
	    print (Fcar (obj), printcharfun, escapeflag);
	    obj = Fcdr (obj);
	  }
      }
      if (!NILP (obj) && !CONSP (obj))
	{
	  strout (" . ", 3, printcharfun);
	  print (obj, printcharfun, escapeflag);
	}
      PRINTCHAR (')');
      break;

    case Lisp_Compiled:
      strout ((print_readably ? "#" : "#<byte-code "), -1, printcharfun);
    case Lisp_Vector:
      PRINTCHAR ('[');
      {
	register int i;
	register Lisp_Object tem;
	for (i = 0; i < XVECTOR (obj)->size; i++)
	  {
	    if (i) PRINTCHAR (' ');
	    tem = XVECTOR (obj)->contents[i];
	    print (tem, printcharfun, escapeflag);
	  }
      }
      PRINTCHAR (']');
      if (!print_readably && XTYPE (obj) == Lisp_Compiled)
	PRINTCHAR ('>');
      break;

#ifndef standalone
    case Lisp_Extent:
      if (escapeflag)
	{
          char *title = "";
	  char *name = "";

          if (XTYPE (XEXTENT(obj)->buffer) == Lisp_Buffer)
            {
              struct buffer *buf = XBUFFER(XEXTENT(obj)->buffer);
	      if (XTYPE (buf->name) == Lisp_String)
		{
		  name = (char *)&(XSTRING(buf->name)->data[0]);
		  title = "buffer ";
		}
	      else
		{
		  title = "Killed Buffer";
		  name = "";
		}
            }
	  
	  if (print_readably)
	    {
	      if (EXTENT_FLAGS(XEXTENT(obj)) & EF_DESTROYED)
		error ("printing unreadable object #<destroyed extent>");
	      else
		error ("printing unreadable object #<extent [%d, %d)>",
		       XINT(Fextent_start_position (obj)), 
		       XINT(Fextent_end_position (obj)));
	    }
	  
          if (EXTENT_FLAGS(XEXTENT(obj)) & EF_DESTROYED)
	    strout ("#<destroyed extent>", -1, printcharfun);
	  else
	    {
	      strout ("#<extent ", -1, printcharfun);
	      if (EXTENT_FLAGS(XEXTENT(obj)) & EF_DETACHED)
		sprintf (buf, "(flags=0x%x) detached from %s%s", 
			 EXTENT_FLAGS (XEXTENT(obj)),
			 title, name);
	      else
		sprintf (buf, "[%d, %d) (flags=0x%x) in %s%s", 
			 XINT(Fextent_start_position (obj)), 
			 XINT(Fextent_end_position (obj)),
			 EXTENT_FLAGS (XEXTENT(obj)),
			 title, name);
	      strout (buf, -1, printcharfun);
	      PRINTCHAR ('>');
	    }
	}
      else
	strout ("#<extent>", -1, printcharfun);
      break;

    case Lisp_Extent_Replica:
      if (escapeflag)
	{
	  if (print_readably)
	    error ("printing unreadable object #<dup [%d, %d)>",
		   XDUP(obj)->start, XDUP(obj)->end);
	  
	  strout ("#<dup ", -1, printcharfun);
          if (XTYPE (XDUP(obj)->extent) == Lisp_Extent)
            {
              Lisp_Object extent_obj = XDUP(obj)->extent;
              int from = XINT(Fextent_start_position (extent_obj));
              int to = XINT(Fextent_end_position (extent_obj));
              sprintf (buf, "[%d, %d) of extent 0x%x [%d, %d)",
                       XDUP(obj)->start, XDUP(obj)->end, XDUP(obj)->extent,
                       from, to);
            }
          else
            sprintf (buf, "[%d, %d) of 0x%x",
                     XDUP(obj)->start, XDUP(obj)->end, XDUP(obj)->extent);
            
          strout (buf, -1, printcharfun);
	  PRINTCHAR ('>');
	}
      else
	strout ("#<dup>", -1, printcharfun);
      break;

    case Lisp_Buffer:
      if (print_readably) {
	if (NILP (XBUFFER (obj)->name))
	  error ("printing unreadable object #<killed buffer>");
	else
	  error ("printing unreadable object #<buffer %s>",
		 XSTRING (XBUFFER (obj)->name)->data);
      }
      if (NILP (XBUFFER (obj)->name))
	strout ("#<killed buffer>", -1, printcharfun);
      else if (escapeflag)
	{
	  strout ("#<buffer ", -1, printcharfun);
	  strout ((char *) XSTRING (XBUFFER (obj)->name)->data, -1, printcharfun);
	  PRINTCHAR ('>');
	}
      else
	strout ((char *) XSTRING (XBUFFER (obj)->name)->data, -1, printcharfun);
      break;

    case Lisp_Process:
      if (escapeflag)
	{
	  if (print_readably)
	    error ("printing unreadable object #<process %s>",
		   XSTRING (XPROCESS (obj)->name)->data);
	  
	  strout ("#<process ", -1, printcharfun);
	  strout ((char *) XSTRING (XPROCESS (obj)->name)->data, -1, printcharfun);
	  PRINTCHAR ('>');
	}
      else
	strout ((char *) XSTRING (XPROCESS (obj)->name)->data, -1, printcharfun);
      break;

    case Lisp_Window:
      if (print_readably)
	error ("printing unreadable object #<window %d>",
	       XFASTINT (XWINDOW (obj)->sequence_number));
      
      strout ("#<window ", -1, printcharfun);
      sprintf (buf, "%d", XFASTINT (XWINDOW (obj)->sequence_number));
      strout (buf, -1, printcharfun);
      if (!NILP (XWINDOW (obj)->buffer))
	{
	  unsigned char *p = XSTRING (XBUFFER (XWINDOW (obj)->buffer)->name)->data;
	  strout (" on ", -1, printcharfun);
	  strout ((char *) p, -1, printcharfun);
	}
      PRINTCHAR ('>');
      break;

    case Lisp_Window_Configuration:
      if (print_readably)
	error ("printing unreadable object #<window-configuration>");
      
      strout ("#<window-configuration>", -1, printcharfun);
      break;

#ifdef MULTI_SCREEN
    case Lisp_Screen:
      if (print_readably)
	error ("printing unreadable object #<screen %s 0x%x>",
	       XSTRING (XSCREEN (obj)->name)->data,
	       XFASTINT (XSCREEN (obj)));
      
      strout ("#<screen ", -1, printcharfun);
      strout ((char *) XSTRING (XSCREEN (obj)->name)->data, -1, printcharfun);
      sprintf (buf, " 0x%x", XFASTINT (XSCREEN (obj)));
      strout (buf, -1, printcharfun);
      strout (">", -1, printcharfun);
      break;
#endif /* MULTI_SCREEN */

    case Lisp_Marker:
      if (print_readably)
	error ("printing unreadable object #<marker>");
      
      strout ("#<marker ", -1, printcharfun);
      if (!(XMARKER (obj)->buffer))
	strout ("in no buffer", -1, printcharfun);
      else
	{
	  sprintf (buf, "at %d", marker_position (obj));
	  strout (buf, -1, printcharfun);
	  strout (" in ", -1, printcharfun);
	  strout ((char *) XSTRING (XMARKER (obj)->buffer->name)->data, -1,
		  printcharfun);
	}
      PRINTCHAR ('>');
      break;

    case Lisp_Event:
      if (print_readably) error ("printing unreadable object #<event>");
      switch (XEVENT (obj)->event_type) {
      case key_press_event:
      case button_press_event:
      case button_release_event:
      case magic_event:
	switch (XEVENT (obj)->event_type) {
	case key_press_event:
	  strout ("#<keypress-event ",   -1, printcharfun); break;
	case button_press_event:
	  strout ("#<buttondown-event ", -1, printcharfun); break;
	case button_release_event:
	  strout ("#<buttonup-event ",   -1, printcharfun); break;
	case magic_event:
	  strout ("#<magic-event ",      -1, printcharfun); break;
	}
	format_event_object (buf, XEVENT (obj), 0);
	strout (buf, -1, printcharfun);
	PRINTCHAR ('>');
	break;

      case pointer_motion_event:
	sprintf (buf, "#<motion-event %d, %d>",
		 XEVENT (obj)->event.motion.x, XEVENT (obj)->event.motion.y);
	strout (buf, -1, printcharfun);
	break;

      case process_event:
	strout ("#<process-event ", -1, printcharfun);
	print (XEVENT (obj)->event.process.process, printcharfun, 1);
	PRINTCHAR ('>');
	break;

      case timeout_event:
	strout ("#<timeout-event ", -1, printcharfun);
	print (XEVENT (obj)->event.timeout.object, printcharfun, 1);
	PRINTCHAR ('>');
	break;

      case empty_event:
	strout ("#<empty-event>", -1, printcharfun);
	break;

      case menu_event:
      case eval_event:
	strout ("#<", -1, printcharfun);
	if (XEVENT (obj)->event_type == menu_event)
	  strout ("menu", -1, printcharfun);
	else
	  strout ("eval", -1, printcharfun);
	strout ("-event (", -1, printcharfun);
	print (XEVENT (obj)->event.eval.function, printcharfun, 1);
	PRINTCHAR (' ');
	print (XEVENT (obj)->event.eval.object, printcharfun, 1);
	strout (")>", -1, printcharfun);
	break;

      case dead_event:
	strout ("#<DEALLOCATED-EVENT>", -1, printcharfun);
	break;

      default:
	strout ("#<UNKNOWN-EVENT-TYPE>", -1, printcharfun);
	break;
      }
      break;

    case Lisp_Keymap:
      {
	int size = XFASTINT (Fhashtable_fullness (XKEYMAP (obj)->table));
	sprintf (buf, "#<keymap %d entr%s>", size, (size == 1) ? "y" : "ies");
	strout (buf, -1, printcharfun);
      }
      break;
#endif /* standalone */

    case Lisp_Subr:
      if (print_readably)
	error ("printing unreadable object #<subr %s>",
	       XSUBR (obj)->symbol_name);
      
      strout ("#<subr ", -1, printcharfun);
      strout (XSUBR (obj)->symbol_name, -1, printcharfun);
      PRINTCHAR ('>');
      break;
    }

  print_depth--;
}

void
syms_of_print ()
{
  staticpro (&Qprint_escape_newlines);
  Qprint_escape_newlines = intern ("print-escape-newlines");
  staticpro (&Qprint_readably);
  Qprint_readably = intern ("print-readably");

  DEFVAR_LISP ("standard-output", &Vstandard_output,
    "Output stream `print' uses by default for outputting a character.\n\
This may be any function of one argument.\n\
It may also be a buffer (output is inserted before point)\n\
or a marker (output is inserted and the marker is advanced)\n\
or the symbol t (output appears in the minibuffer line).");
  Vstandard_output = Qt;
  Qstandard_output = intern ("standard-output");
  staticpro (&Qstandard_output);

#ifdef LISP_FLOAT_TYPE
  DEFVAR_LISP ("float-output-format", &Vfloat_output_format,
    "The format descriptor string that lisp uses to print floats.\n\
This is a %-spec like those accepted by `printf' in C,\n\
but with some restrictions.  It must start with the two characters `%.'.\n\
After that comes an integer precision specification,\n\
and then a letter which controls the format.\n\
The letters allowed are `e', `f' and `g'.\n\
Use `e' for exponential notation \"DIG.DIGITSeEXPT\"\n\
Use `f' for decimal point notation \"DIGITS.DIGITS\".\n\
Use `g' to choose the shorter of those two formats for the number at hand.\n\
The precision in any of these cases is the number of digits following\n\
the decimal point.  With `f', a precision of 0 means to omit the\n\
decimal point.  0 is not allowed with `f' or `g'.\n\n\
A value of nil means to use `%.16g'.\n\
\n\
Regardless of the value of `float-output-format', a floating point number\n\
will never be printed in such a way that it is ambiguous with an integer;\n\
that is, a floating-point number will always be printed with a decimal\n\
point and/or an exponent, even if the digits following the decimal point\n\
are all zero.  This is to preserve read-equivalence.");
  Vfloat_output_format = Qnil;
  Qfloat_output_format = intern ("float-output-format");
  staticpro (&Qfloat_output_format);
#endif /* LISP_FLOAT_TYPE */

  DEFVAR_LISP ("print-length", &Vprint_length,
    "Maximum length of list to print before abbreviating.\
A value of nil means no limit.");
  Vprint_length = Qnil;

  DEFVAR_LISP ("print-level", &Vprint_level,
    "Maximum depth of list nesting to print before abbreviating.\
A value of nil means no limit.");
  Vprint_level = Qnil;

  DEFVAR_BOOL ("print-escape-newlines", &print_escape_newlines,
    "Non-nil means print newlines in strings as backslash-n.");
  print_escape_newlines = 0;

  DEFVAR_BOOL ("print-readably", &print_readably,
    "If non-nil, then all objects will be printed in a readable form.\n\
If an object has no readable representation, then an error is signalled.\n\
When this is true, compiled-function objects will be written in #[...] form\n\
instead of in #<byte-code [...]> form.\n\
Do not SET this variable; bind it instead.");
  print_readably = 0;

  DEFVAR_BOOL ("print-gensym", &print_gensym,
    "If non-nil, then uninterned symbols (those made with `make-symbol'\n\
instead of `intern') will be preceeded by \"#:\", which tells the reader to\n\
create a new symbol instead of interning.  Beware: the #: syntax creates a\n\
new symbol each time it is seen, so if you print an object which contains\n\
two pointers to the same uninterned symbol, `read' will not duplicate that\n\
structure.");
  print_gensym = 0;

  /* prin1_to_string_buffer initialized in init_buffer_once in buffer.c */
  staticpro (&Vprin1_to_string_buffer);

  defsubr (&Sprin1);
  defsubr (&Sprin1_to_string);
  defsubr (&Sprinc);
  defsubr (&Sprint);
  defsubr (&Sterpri);
  defsubr (&Swrite_char);
#ifndef standalone
  defsubr (&Swith_output_to_temp_buffer);
#endif /* not standalone */
}
