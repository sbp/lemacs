/* Lisp object printing and output streams.
   Copyright (C) 1985, 1986, 1988, 1992, 1993, 1994
   Free Software Foundation, Inc.

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

#include "lisp.h"
#include "intl.h"

#ifndef standalone
#include "buffer.h"
#include "bytecode.h"
#include "extents.h"
#include "screen.h"             /* for SCREEN_MESSAGE_BUF */
#include "window.h"             /* for echo_area_glyphs */
#include "insdel.h"             /* for insert_raw_string */

#endif /* not standalone */

Lisp_Object Vstandard_output, Qstandard_output;

/* The subroutine object for external-debugging-output is kept here
   for the convenience of the debugger.  */
Lisp_Object Qexternal_debugging_output;
Lisp_Object Qalternate_debugging_output;
 
/* Avoid actual stack overflow in print.  */
static int print_depth;

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

static int max_print;


/* Low level output routines for characters and strings */

#define PRINTER_BUFFER_SIZE 4096

struct output_stream
{
  struct lcrecord_header header;
  struct buffered_output
    {
      char *start;
      char *fill_pointer;
      char *end;
    } buffered_output;

  /* How many characters have been written so far */
  int pos;

  /* For writing to stdio FILE's */
  FILE *stdio_stream;

  /* For prin1 and friends */
  Lisp_Object function;
};
#define OUTPUTSTREAMP(s) RECORD_TYPEP ((s), lrecord_output_stream)
#define XOUTPUTSTREAM(s) ((struct output_stream *)XRECORD(s))

static Lisp_Object mark_output_stream (Lisp_Object, void (*) (Lisp_Object));
static void print_output_stream (Lisp_Object, Lisp_Object, int);
static void finalise_output_stream (void *, int);
DEFINE_LRECORD_IMPLEMENTATION ("output-stream", lrecord_output_stream,
                               mark_output_stream, print_output_stream,
                               finalise_output_stream, 0,
			       sizeof (struct output_stream));

static void flush_stream_output_buffer (struct output_stream *stream);

static Lisp_Object
mark_output_stream (Lisp_Object obj, void (*markobj) (Lisp_Object))
{
  return (XOUTPUTSTREAM (obj)->function);
}

static void
print_output_stream (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  /* This should NEVER be called, since these streams don't (yet) actually
     escape to Lisp from the confines of the printer */
  char buf[100];
  sprintf (buf, GETTEXT ("#<output-stream 0x%x>"), (long) XOUTPUTSTREAM (obj));
  write_string_1 (buf, -1, printcharfun);
}

static void
finalise_output_stream (void *header, int for_disksave)
{
  struct output_stream *stream = header;
  if (for_disksave)
    flush_stream_output_buffer (stream);
  if (stream->stdio_stream)
    fclose (stream->stdio_stream);
  stream->stdio_stream = 0;
}

/* Object-oriented programming at its finest! */
static void
miscellaneous_output_kludges (Lisp_Object function,
                              CONST char *str, int len,
                              /* So we can note its relocation if necessary */
                              Lisp_Object string_or_zero)
{
  if (len < 0) return;

  /* Emacs won't print whilst GCing, but an external debugger might */
  if (gc_in_progress) return;

  if (OUTPUTSTREAMP (function))
    {
      struct output_stream *stream = XOUTPUTSTREAM (function);

      stream->pos += len;
      if (stream->stdio_stream)
	{
	  /* stdio does its own buffering */
	  fwrite (str, 1, len, stream->stdio_stream);
      
	  /* kludge to tell the "message" function to print a newline */
	  if (noninteractive && stream->stdio_stream == stdout)
	    noninteractive_need_newline = 1;
	}
      else
	{
	  int offset = 0;
	  char *fill = stream->buffered_output.fill_pointer;
          struct gcpro gcpro1, gcpro2;
          GCPRO2 (function, string_or_zero);

	  if (STRINGP (string_or_zero))
	    {
	      offset = str - (char *) XSTRING (string_or_zero)->data;
	      str -= offset;
	    }

	  for (;;)
	    {
	      char *new_end = fill + len;
	      int delta = new_end - stream->buffered_output.end;

	      if (delta < 0)
                break;
	      delta = len - delta;

	      memcpy (fill, str + offset, delta);
	      stream->buffered_output.fill_pointer = fill + delta;
        
	      flush_stream_output_buffer (stream);
	      fill = stream->buffered_output.fill_pointer;

	      offset += delta;
	      len -= delta; 
	      if (STRINGP (string_or_zero))
		{
		  /* GC may have relocated it. */
		  str = (char *) XSTRING (string_or_zero)->data;
		}
	    }
	  memcpy (fill, str + offset, len);
	  stream->buffered_output.fill_pointer = fill + len;
          UNGCPRO;
	}
    }
#ifndef standalone
  else if (BUFFERP (function))
    {
      Lisp_Object old = Fcurrent_buffer ();
      struct gcpro gcpro1, gcpro2;
      GCPRO2 (old, string_or_zero);

      Fset_buffer (function);
      insert_relocatable_raw_string (str, len, string_or_zero);
      Fset_buffer (old);
      UNGCPRO;
    }
  else if (MARKERP (function))
    {
      /* marker_position will err if marker doesn't point anywhere */
      int spoint = marker_position (function);
      int opoint;
      int npoint;
      Lisp_Object old = Fcurrent_buffer ();
      struct gcpro gcpro1, gcpro2, gcpro3;
      GCPRO3 (function, string_or_zero, old);

      Fset_buffer (Fmarker_buffer (function));
      opoint = PT;
      SET_PT (spoint);
      insert_relocatable_raw_string (str, len, string_or_zero);
      npoint = PT;
      Fset_marker (function, make_number (npoint), Qnil);
      if (opoint >= spoint)
        SET_PT (npoint + (opoint - spoint));
      else
        SET_PT (opoint);
      Fset_buffer (old);
      UNGCPRO;
    }
  else if (SCREENP (function))
    {
      /* let's just try using message and see what happens */
      /* let's not - that screws up command error messages badly. -jwz */
#if 1
      struct screen *s = XSCREEN (function);
      int swidth = SCREEN_WIDTH (s);
      char *sbuf = SCREEN_MESSAGE_BUF (s);
      int pos = 0;

      if (!sbuf)
	/* sbuf may be 0 before first screen made */
	return;

      if (echo_area_glyphs != sbuf)
	{
	  echo_area_glyphs = sbuf;
	  pos = 0;
	}
      else
	{
	  /* Append to what is already displayed */
	  pos = strlen (sbuf);	/* always 0(yuck!)-terminated */
	}

      if (pos + len >= swidth)
	len = swidth - 1 - pos;

      if (len > 0)
	{
	  memcpy (sbuf + pos, str, len);
	  sbuf[pos + len] = 0;
	}
#else
      char *sbuf = (char *) alloca (len * sizeof (char *) + 1);

      strncpy (sbuf, str, len);
      sbuf[len] = 0;
      message ("%s", sbuf);
#endif
    }
#endif /* not standalone */
  else if (EQ (function, Qt) || EQ (function, Qnil))
    {
      fwrite (str, 1, len, stdout);
    }
  else
    {
      int iii;
      struct gcpro gcpro1, gcpro2;
      GCPRO2 (function, string_or_zero);

      for (iii = 0; iii < len; iii++)
	{
	  call1 (function, make_number (str[iii]));
	  if (STRINGP (string_or_zero))
	    str = (char *) XSTRING (string_or_zero)->data;
	}
      UNGCPRO;
    }
}


static void
flush_stream_output_buffer (struct output_stream *stream)
{
  if (stream->stdio_stream)
    fflush (stream->stdio_stream);
  else if (stream->buffered_output.start)
    {
      char *fill = stream->buffered_output.fill_pointer;
      char *start = stream->buffered_output.start;

      stream->buffered_output.fill_pointer = start;
      miscellaneous_output_kludges (stream->function, start, fill - start,
				    Qzero);
    }
}


static Lisp_Object
canonicalise_printcharfun (Lisp_Object printcharfun)
{
  if (NILP (printcharfun))
    printcharfun = Vstandard_output;

  if (EQ (printcharfun, Qt) || NILP (printcharfun))
    {
      if (noninteractive)
	printcharfun = Qnil;	/* print to stdout */
#ifndef standalone
      else 
	printcharfun = Fselected_screen (); /* print to minibuffer */
#endif
    }
  return (printcharfun);
}


/* nil, or chained by XOUTPUTSTREAM ()->function */
static Lisp_Object Voutput_stream_resource;

/* This is called by GC */
void
clear_output_stream_resource (void)
{
  Voutput_stream_resource = Qnil;
}

static Lisp_Object
print_prepare (Lisp_Object printcharfun, int buffer_size)
{
  Lisp_Object xstream;
  struct output_stream *s;
  FILE *stdio_stream = 0;

  /* Emacs won't print whilst GCing, but an external debugger might */
  if (gc_in_progress) return (Qnil);

  printcharfun = canonicalise_printcharfun (printcharfun);
  if (EQ (printcharfun, Qnil))
    {
      stdio_stream = stdout;
#if 0 /* Why do this? */
      if (noninteractive)
	stdio_stream = stderr;
#endif
      buffer_size = 0;
    }
#if 0 /* Don't bother */
  else if (SUBRP (indirect_function (printcharfun, 0))
           && (XSUBR (indirect_function (printcharfun, 0)) 
               == Sexternal_debugging_output))
    {
      stdio_stream = stderr;
      buffer_size = 0;
    }
#endif
  else if (buffer_size <= 0)
    buffer_size = 1;
  
  /* BLOCK_INPUT; */
  if (OUTPUTSTREAMP (Voutput_stream_resource))
  {
    s = XOUTPUTSTREAM (Voutput_stream_resource);
    Voutput_stream_resource = s->function;
  }
  else
  {
    s = alloc_lcrecord (sizeof (struct output_stream), lrecord_output_stream);
  }
  /* UNBLOCK_INPUT; */

  if (buffer_size > 0)
    {
      char *buffer = xmalloc (buffer_size);
      s->buffered_output.start = buffer;
      s->buffered_output.fill_pointer = buffer;
      s->buffered_output.end = buffer + buffer_size;
    }
  else
    {
      s->buffered_output.start = 0;
      s->buffered_output.fill_pointer = 0;
      s->buffered_output.end = 0;
    }
  s->stdio_stream = stdio_stream;
  s->function = printcharfun;
  s->pos = 0;
  XSET (xstream, Lisp_Record, s);
  return (xstream);
}

static void
print_finish (Lisp_Object stream)
{
  struct gcpro gcpro1;
  struct output_stream *s = XOUTPUTSTREAM (stream);

  /* Emacs won't print whilst GCing, but an external debugger might */
  if (gc_in_progress) return;

  GCPRO1 (stream);

  flush_stream_output_buffer (s);
  s = XOUTPUTSTREAM (stream); /* for hypothetical relocating GC (ho ho ho) */

  /* We can only do this freeing hack because the
   *  stream can't escape out to user-land.
   * If any object print methods were to call out to Lisp
   *  code with the stream as an argument, we'd be shafted.
   */

  s->stdio_stream = 0; /* Don't close it! */
  if (s->buffered_output.start)
    {
      xfree (s->buffered_output.start);
      s->buffered_output.start = 0;
      s->buffered_output.fill_pointer = 0;
      s->buffered_output.end = 0;
    }

  /* BLOCK_INPUT; */
  s->function = Voutput_stream_resource;
  Voutput_stream_resource = stream;
  /* UNBLOCK_INPUT; */
  UNGCPRO;
}

#if 1 /* Prefer space over "speed" */
#define write_char_internal(string_of_length_1, stream) \
  write_string_1 ((string_of_length_1), 1, (stream))
#else
#define write_char_internal(string_of_length_1, stream) \
  miscellaneous_output_kludges ((stream), (string_of_length_1), 1, Qzero)
#endif

/* NOTE:  Do not call this with the data of a Lisp_String,
 *  as printcharfun might cause a GC, which might cause
 *  the string's data to be relocated.
 *  Use print_object_internal (string, printcharfun, 0)
 *  to princ a Lisp_String
 * Note: "stream" should be the result of "canonicalise_printcharfun"
 *  (ie Qnil means stdout, not Vstandard_output, etc)
 */
void
write_string_1 (CONST char *str, int size, Lisp_Object stream)
{
  if (size < 0)
    size = strlen (str);

  miscellaneous_output_kludges (stream, str, size, Qzero);
}



DEFUN ("write-char", Fwrite_char, Swrite_char, 1, 2, 0,
  "Output character CHAR to stream STREAM.\n\
STREAM defaults to the value of `standard-output' (which see).")
  (ch, printcharfun)
     Lisp_Object ch, printcharfun;
{
  char str[1];

  CHECK_FIXNUM (ch, 0);
  str[0] = XINT (ch);
  miscellaneous_output_kludges (canonicalise_printcharfun (printcharfun),
                                str, 1, Qzero);
  return ch;
}

#ifndef standalone

void
temp_output_buffer_setup (CONST char *bufname)
{
  register struct buffer *old = current_buffer;
  Lisp_Object buf;

  Fset_buffer (Fget_buffer_create (build_string (bufname)));

  current_buffer->read_only = Qnil;
  Ferase_buffer ();

  XSETR (buf, Lisp_Buffer, current_buffer);
  specbind (Qstandard_output, buf);

  set_buffer_internal (old);
}

Lisp_Object
internal_with_output_to_temp_buffer (CONST char *bufname, 
                                     Lisp_Object (*function) (Lisp_Object arg),
                                     Lisp_Object arg, 
                                     Lisp_Object same_screen)
{
  int speccount = specpdl_depth ();
  struct gcpro gcpro1, gcpro2, gcpro3;
  Lisp_Object buf = Qnil;

  GCPRO3 (buf, arg, same_screen);

  temp_output_buffer_setup (bufname);
  buf = Vstandard_output;

  arg = (*function) (arg);

  temp_output_buffer_show (buf, same_screen);
  UNGCPRO;

  return unbind_to (speccount, arg);
}

DEFUN ("with-output-to-temp-buffer", Fwith_output_to_temp_buffer, Swith_output_to_temp_buffer,
       1, UNEVALLED, 0,
  "Bind `standard-output' to buffer BUFNAME, eval BODY, then show that buffer.\n\
The buffer is cleared out initially, and marked as unmodified when done.\n\
All output done by BODY is inserted in that buffer by default.\n\
The buffer is displayed in another window, but not selected.\n\
The value of the last form in BODY is returned.\n\
If BODY does not finish normally, the buffer BUFNAME is not displayed.\n\n\
If variable `temp-buffer-show-function' is non-nil, call it at the end\n\
to get the buffer displayed.  It gets one argument, the buffer to display.")
  (args)
     Lisp_Object args;
{
  struct gcpro gcpro1;
  Lisp_Object name;
  int speccount = specpdl_depth ();
  Lisp_Object buf, val;

  GCPRO1 (args);
  name = Feval (Fcar (args));
  UNGCPRO;

  CHECK_STRING (name, 0);
  temp_output_buffer_setup ((char *) XSTRING (name)->data);
  buf = Vstandard_output;

  val = Fprogn (Fcdr (args));

  temp_output_buffer_show (buf, Qnil);

  return unbind_to (speccount, val);
}
#endif /* not standalone */

DEFUN ("terpri", Fterpri, Sterpri, 0, 1, 0,
  "Output a newline to STREAM.\n\
If STREAM is omitted or nil, the value of `standard-output' is used.")
  (printcharfun)
     Lisp_Object printcharfun;
{
  char str[1];
  str[0] = '\n';
  miscellaneous_output_kludges (canonicalise_printcharfun (printcharfun),
                                str, 1, Qzero);
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
  Lisp_Object stream = Qnil;
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (obj, stream);
  max_print = 0;
  print_depth = 0;
  stream = print_prepare (printcharfun, PRINTER_BUFFER_SIZE);
  print_internal (obj, stream, 1);
  print_finish (stream);
  UNGCPRO;
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
  Lisp_Object old = Fcurrent_buffer ();
  struct buffer *out = XBUFFER (Vprin1_to_string_buffer);
  Lisp_Object stream = Qnil;
  struct gcpro gcpro1, gcpro2, gcpro3;

  GCPRO3 (obj, old, stream);
  stream = print_prepare (Vprin1_to_string_buffer, PRINTER_BUFFER_SIZE);
  set_buffer_internal (out);
  Ferase_buffer ();
  print_depth = 0;
  print_internal (obj, stream, NILP (noescape));
  print_finish (stream);
  stream = Qnil;                /* No GC surprises! */
  obj = make_string_from_buffer (out, 
                                 BUF_BEG (out),
                                 BUF_Z (out) - 1);
  Ferase_buffer ();
  Fset_buffer (old);
  UNGCPRO;
  return (obj);
}

DEFUN ("princ", Fprinc, Sprinc, 1, 2, 0,
  "Output the printed representation of OBJECT, any Lisp object.\n\
No quoting characters are used; no delimiters are printed around\n\
the contents of strings.\n\
Output stream is STREAM, or value of standard-output (which see).")
  (obj, printcharfun)
     Lisp_Object obj, printcharfun;
{
  Lisp_Object stream = Qnil;
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (obj, stream);
  stream = print_prepare (printcharfun, PRINTER_BUFFER_SIZE);
  print_depth = 0;
  print_internal (obj, stream, 0);
  print_finish (stream);
  UNGCPRO;
  return (obj);
}

DEFUN ("print", Fprint, Sprint, 1, 2, 0,
  "Output the printed representation of OBJECT, with newlines around it.\n\
Quoting characters are printed when needed to make output that `read'\n\
can handle, whenever this is possible.\n\
Output stream is STREAM, or value of `standard-output' (which see).")
  (obj, printcharfun)
     Lisp_Object obj, printcharfun;
{
  Lisp_Object stream = Qnil;
  struct gcpro gcpro1, gcpro2;

#ifdef MAX_PRINT_CHARS
  max_print = MAX_PRINT_CHARS;
#endif

  GCPRO2 (obj, stream);
  stream = print_prepare (printcharfun, PRINTER_BUFFER_SIZE);
  print_depth = 0;
  write_char_internal ("\n", stream);
  print_internal (obj, stream, 1);
  write_char_internal ("\n", stream);
  print_finish (stream);
  UNGCPRO;
  max_print = 0;
  return obj;
}

#ifdef LISP_FLOAT_TYPE

Lisp_Object Vfloat_output_format;
Lisp_Object Qfloat_output_format;

void
float_to_string (char *buf, double data)
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
{
  register unsigned char *cp, c;
  register int width;
      
  if (NILP (Vfloat_output_format)
      || !STRINGP (Vfloat_output_format))
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
      for (width = 0; (c = *cp, isdigit (c)); cp++)
	{
	  width *= 10;
	  width += c - '0';
	}

      if (*cp != 'e' && *cp != 'f' && *cp != 'g')
	goto lose;

      if (width < (int) (*cp != 'e') || width > DBL_DIG)
	goto lose;

      if (cp[1] != 0)
	goto lose;

      sprintf (buf, (char *) XSTRING (Vfloat_output_format)->data, data);
    }

  /* added by jwz: don't allow "1.0" to print as "1"; that destroys
     the read-equivalence of lisp objects.  (* x 1) and (* x 1.0) do
     not do the same thing, so it's important that the printed
     representation of that form not be corrupted by the printer.
   */
  {
    register char *s = buf;
    if (*s == '-') s++;
    for (; *s; s++)
      /* if there's a non-digit, then there is a decimal point, or
	 it's in exponential notation, both of which are ok. */
      if (!isdigit (*s))
	goto DONE;
    /* otherwise, we need to hack it. */
    *s++ = '.';
    *s++ = '0';
    *s = 0;
  }
 DONE:

  /* Some machines print "0.4" as ".4".  I don't like that. */
  if (buf [0] == '.' || (buf [0] == '-' && buf [1] == '.'))
    {
      int i;
      for (i = strlen (buf) + 1; i >= 0; i--)
	buf [i+1] = buf [i];
      buf [(buf [0] == '-' ? 1 : 0)] = '0';
    }
}
#endif /* LISP_FLOAT_TYPE */

void
print_vector_internal (CONST char *start, CONST char *end,
                       Lisp_Object obj, 
                       Lisp_Object printcharfun, int escapeflag)
{
  register int i;
  int len = vector_length (XVECTOR (obj));
  int last = len;
  struct gcpro gcpro1, gcpro2;
  GCPRO2 (obj, printcharfun);

  if (FIXNUMP (Vprint_length))
    {
      int max = XINT (Vprint_length);
      if (max < len) last = max;
    }

  write_string_1 (start, -1, printcharfun);
  for (i = 0; i < last; i++)
    {
      register Lisp_Object elt = XVECTOR (obj)->contents[i];
      if (i != 0) write_char_internal (" ", printcharfun);
      print_internal (elt, printcharfun, escapeflag);
    }
  UNGCPRO;
  if (last != len)
    write_string_1 (" ...", 4, printcharfun);
  write_string_1 (end, -1, printcharfun);
}

void
print_internal (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  char buf[256];
#ifdef I18N4
  int obj_type;
#endif

  QUIT;

  /* Emacs won't print whilst GCing, but an external debugger might */
  if (gc_in_progress) return;

  print_depth++;

  if (print_depth > 200)
    error (GETTEXT ("Apparently circular structure being printed"));
#ifdef MAX_PRINT_CHARS
  if (max_print > 0 && OUTPUTSTREAMP (printcharfun))
  {
    if (XOUTPUTSTREAM (printcharfun)->pos > max_print)
    {
      write_char_internal ("\n", printcharfun);
      max_print = XOUTPUTSTREAM (printcharfun)->pos + MAX_PRINT_CHARS;
    }
  }
#endif /* MAX_PRINT_CHARS */

#ifdef I18N4
  /* This is a terrible kludge, necessary until certain issues are
     resolved.  In the SunOS 5.0 wide character model, two-byte
     multi-byte characters of the form 1xxxxxxx 1yyyyyyy have the
     following wide character representation:

		 00110000 00000000 00xxxxxx xyyyyyyy

     But since Lisp objects have their type encoded in the top 6 bits,
     the type is interpreted as 001100, or Lisp_Process.  We will
     kluge around this for now by assuming that all real Lisp_Process
     objects point to a memory location greater than 0x3fff. */

  if (XUINT(obj) >> 14 == 0xc000)
    obj_type = -1;
  else
    obj_type = XTYPE (obj);
  switch (obj_type)
#else /* not I18N4 */
  switch (XTYPE (obj))
#endif /* I18N4 */
    {
    case Lisp_Int:
      {
	sprintf (buf, "%d", XINT (obj));
	write_string_1 (buf, -1, printcharfun);
	break;
      }

#ifdef I18N4
    case -1:
      sprintf (buf, "%d", XFASTINT (obj));
      write_string_1 (buf, -1, printcharfun);
      break;
#endif

    case Lisp_String:
      {
	int size = string_length (XSTRING (obj));
	struct gcpro gcpro1, gcpro2;
	GCPRO2 (obj, printcharfun);

	if (!escapeflag)
	  {
	    /* This deals with GC-relocation */
	    miscellaneous_output_kludges (printcharfun,
					  (char *) XSTRING (obj)->data, size,
					  obj);
	  }
	else
	  {
	    register int i;
	    register struct Lisp_String *s = XSTRING (obj);
	    int last = 0;

	    write_char_internal ("\"", printcharfun);
	    for (i = 0; i < size; i++)
	      {
		register unsigned char ch = s->data[i];
		if (ch == '\"' || ch == '\\' 
		    || (ch == '\n' && print_escape_newlines))
		  {
		    if (i > last)
		      {
			miscellaneous_output_kludges
			  (printcharfun, (char *) s->data + last, i - last,
			   obj);
		      }
		    if (ch == '\n')
		      {
			write_string_1 ("\\n", 2, printcharfun);
		      }
		    else
		      {
			write_char_internal ("\\", printcharfun);
			write_char_internal ((char *) (s->data + i),
					     printcharfun);
		      }
		    last = i + 1;
		  }
	      }
	    if (size > last)
	      {
		miscellaneous_output_kludges
		  (printcharfun, (char *) s->data + last, size - last,
		   obj);
	      }
	    write_char_internal ("\"", printcharfun);
	  }
	UNGCPRO;
	break;
      }

    case Lisp_Cons:
      {
	struct gcpro gcpro1, gcpro2;

	/* If deeper than spec'd depth, print placeholder.  */
	if (FIXNUMP (Vprint_level)
	    && print_depth > XINT (Vprint_level))
	  {
	    write_string_1 ("...", 3, printcharfun);
	    break;
	  }

	/* If print_readably is on, print (quote -foo-) as '-foo-
	   (Yeah, this should really be what print-pretty does, but we
	   don't have the rest of a pretty printer, and this actually
	   has non-negligible impact on size/speed of .elc files.)
	 */
	if (print_readably &&
	    EQ (XCONS (obj)->car, Qquote) &&
	    CONSP (XCONS (obj)->cdr) &&
	    NILP (XCONS (XCONS (obj)->cdr)->cdr))
	  {
	    obj = XCONS (XCONS (obj)->cdr)->car;
	    GCPRO2 (obj, printcharfun);
	    write_char_internal ("'", printcharfun);
	    UNGCPRO;
	    print_internal (obj, printcharfun, escapeflag);
	    break;
	  }

	GCPRO2 (obj, printcharfun);
	write_char_internal ("(", printcharfun);
	{
	  register int i = 0;
	  register int max = 0;

	  if (FIXNUMP (Vprint_length))
	    max = XINT (Vprint_length);
	  while (CONSP (obj))
	    {
	      if (i++)
		write_char_internal (" ", printcharfun);
	      if (max && i > max)
		{
		  write_string_1 ("...", 3, printcharfun);
		  break;
		}
	      print_internal (Fcar (obj), printcharfun, escapeflag);
	      obj = Fcdr (obj);
	    }
	}
	if (!NILP (obj) && !CONSP (obj))
	  {
	    write_string_1 (" . ", 3, printcharfun);
	    print_internal (obj, printcharfun, escapeflag);
	  }
	UNGCPRO;
	write_char_internal (")", printcharfun);
	break;
      }

#ifndef LRECORD_VECTOR
    case Lisp_Vector:
      {
	/* If deeper than spec'd depth, print placeholder.  */
	if (FIXNUMP (Vprint_level)
	    && print_depth > XINT (Vprint_level))
	  {
	    write_string_1 ("...", 3, printcharfun);
	    break;
	  }

	/* God intended that this be #(...), you know. */
	print_vector_internal ("[", "]", obj, printcharfun, escapeflag);
	break;
      }
#endif /* !LRECORD_VECTOR */

#ifndef LRECORD_SYMBOL
    case Lisp_Symbol:
      {
        print_symbol (obj, printcharfun, escapeflag);
        break;
      }
#endif /* !LRECORD_SYMBOL */

    case Lisp_Record:
      {
	struct lrecord_header *lheader = XRECORD_LHEADER (obj);
	struct gcpro gcpro1, gcpro2;

	GCPRO2 (obj, printcharfun);
	((lheader->implementation->printer) (obj, printcharfun, escapeflag));
	UNGCPRO;
	break;
      }

    default:
      {
	/* We're in trouble if this happens!
	   Probably should just abort () */
	if (print_readably)
	  error (GETTEXT ("printing illegal data type #o%03o"),
		 (int) XTYPE (obj));
	write_string_1 ("#<EMACS BUG: ILLEGAL DATATYPE ", -1,
			printcharfun);
	sprintf (buf, "(#o%3o)", (int) XTYPE (obj));
	write_string_1 (buf, -1, printcharfun);
	write_string_1 (" Save your buffers immediately and please report this bug>",
			-1, printcharfun);
	break;
      }
    }

  print_depth--;
}

static void
print_bytecode_internal (CONST char *start, CONST char *end,
                         Lisp_Object obj, 
                         Lisp_Object printcharfun, int escapeflag)
{
  struct Lisp_Bytecode *b = XBYTECODE (obj); /* GC doesn't relocate */
  int docp = b->flags.documentationp;
  int intp = b->flags.interactivep;
  struct gcpro gcpro1, gcpro2;
  char buf[100];
  GCPRO2 (obj, printcharfun);

  write_string_1 (start, -1, printcharfun);
  /* COMPILED_ARGLIST = 0 */
  print_internal (b->arglist, printcharfun, escapeflag);
  /* COMPILED_BYTECODE = 1 */
  write_char_internal (" ", printcharfun);
  print_internal (b->bytecodes, printcharfun, escapeflag);
  /* COMPILED_CONSTANTS = 2 */
  write_char_internal (" ", printcharfun);
  print_internal (b->constants, printcharfun, escapeflag);
  /* COMPILED_STACK_DEPTH = 3 */
  sprintf (buf, " %d", b->maxdepth);
  write_string_1 (buf, -1, printcharfun);
  /* COMPILED_DOC_STRING = 4 */
  if (docp || intp)
    {
      write_char_internal (" ", printcharfun);
      print_internal (bytecode_documentation (b), printcharfun, escapeflag);
    }
  /* COMPILED_INTERACTIVE = 5 */
  if (intp)
    {
      write_char_internal (" ", printcharfun);
      print_internal (bytecode_interactive (b), printcharfun, escapeflag);
    }
  UNGCPRO;
  write_string_1 (end, -1, printcharfun);
}

void
print_bytecode (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  print_bytecode_internal (((print_readably) ? "#[" : "#<byte-code "),
                           ((print_readably) ? "]" : ">"),
                           obj, printcharfun, escapeflag);
}

#ifdef LISP_FLOAT_TYPE
void
print_float (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  char pigbuf[350];	/* see comments in float_to_string */

  float_to_string (pigbuf, float_data (XFLOAT (obj)));
  write_string_1 (pigbuf, -1, printcharfun);
}
#endif /* LISP_FLOAT_TYPE */

void
print_symbol (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  /* >>> Bug!! (intern "") isn't printed in some distinguished way */
  /* >>>  (the reader also loses on it) */
  register struct Lisp_String *name = XSYMBOL (obj)->name;
  register int size = name->size;
  struct gcpro gcpro1, gcpro2;

  if (!escapeflag)
    {
      /* This deals with GC-relocation */
      Lisp_Object nameobj;
      XSET (nameobj, Lisp_String, name);
      miscellaneous_output_kludges (printcharfun,
				    (char *) name->data, size, 
				    nameobj);
      return;
    }
  GCPRO2 (obj, printcharfun);

  if (print_gensym) 
    {
      Lisp_Object tem = oblookup (Vobarray, name->data, size);
      if (!EQ (tem, obj))
	/* (read) would return a new symbol with the same name.
	   This isn't quite correct, because that symbol might not
	   really be uninterned (it might be interned in some other
	   obarray) but there's no way to win in that case without
	   implementing a real package system.
	   */
	write_string_1 ("#:", 2, printcharfun);
    }

  /* Does it look like an integer or a float? */
  {
    unsigned char *data = name->data;
    int confusing = 0;

    if (size == 0)
      goto not_yet_confused;    /* Really confusing */
    else if (isdigit (data[0]))
      confusing = 0;
    else if (size == 1)
      goto not_yet_confused;
    else if (data[0] == '-' || data[0] == '+')
      confusing = 1;
    else
      goto not_yet_confused;

    for (; confusing < size; confusing++)
      { 
        if (!isdigit (data[confusing]))
          {
            confusing = 0;
            break;
          }
      }
  not_yet_confused:

#ifdef LISP_FLOAT_TYPE
    if (!confusing)
      confusing = isfloat_string (data);
#endif
    if (confusing)
      write_char_internal ("\\", printcharfun);
  }

  {
    Lisp_Object nameobj;
    register int i;
    int last = 0;
        
    XSET (nameobj, Lisp_String, name);
    for (i = 0; i < size; i++)
      {
	register unsigned char c = name->data[i];

	if (c == '\"' || c == '\\' || c == '\'' || c == ';' || c == '#' ||
	    c == '(' || c == ')' || c == ',' || c =='.' || c == '`' ||
	    c == '[' || c == ']' || c == '?' || c <= 040)
	  {
	    if (i > last)
	      {
		miscellaneous_output_kludges (printcharfun, 
					      (char *) name->data + last,
					      i - last,
					      nameobj);
	      }
	    write_char_internal ("\\", printcharfun);
	    last = i;
	  }
      }
    miscellaneous_output_kludges (printcharfun, 
                                  (char *) name->data + last, size - last,
                                  nameobj);
  }
  UNGCPRO;
}


int alternate_do_pointer;
char alternate_do_string[5000];

DEFUN ("alternate-debugging-output", Falternate_debugging_output,
       Salternate_debugging_output, 1, 1, 0,
  "Append output to the array `alternate_do_string'.  This can be used\n\
in place of `external-debugging-output' as a function to be passed to\n\
`print'.  Before calling `print', set `alternate_do_pointer' to 0.\n")
  (character)
     Lisp_Object character;
{
  CHECK_FIXNUM (character, 0);
  alternate_do_string[alternate_do_pointer++] = (char) (XINT (character));
  alternate_do_string[alternate_do_pointer] = 0;
  return character;
}

DEFUN ("external-debugging-output", Fexternal_debugging_output,
       Sexternal_debugging_output, 1, 1, 0,
  "Write CHARACTER to stderr.\n\
You can call print while debugging emacs, and pass it this function\n\
to make it write to the debugging output.\n")
  (character)
     Lisp_Object character;
{
  if (STRINGP (character))
    fwrite (XSTRING (character)->data, 1, string_length (XSTRING (character)),
            stderr);
  else
  {
    CHECK_FIXNUM (character, 0);
    putc (XINT (character), stderr);
  }
  return character;
}

#if 1
/* Debugging kludge -- unbuffered */
static int debug_print_length = 50;
static int debug_print_level = 15;
Lisp_Object debug_temp;
void
debug_print (Lisp_Object debug_print_obj)
{
  int old_max_print = max_print;
  int old_print_readably = print_readably;
  int old_print_depth = print_depth;
  Lisp_Object old_print_length = Vprint_length;
  Lisp_Object old_print_level = Vprint_level;
  Lisp_Object old_inhibit_quit = Vinhibit_quit;
  struct gcpro gcpro1, gcpro2, gcpro3;
  GCPRO3 (old_print_level, old_print_length, old_inhibit_quit);

  if (gc_in_progress)
    fprintf (stderr,
	     GETTEXT ("** gc-in-progress!  Bad idea to print anything! **\n"));

  max_print = 0;
  print_depth = 0;
  print_readably = 0;
  /* Could use unwind-protect, but why bother? */
  if (debug_print_length > 0)
    Vprint_length = make_number (debug_print_length);
  if (debug_print_level > 0)
    Vprint_level = make_number (debug_print_level);
  print_internal (debug_print_obj, Qexternal_debugging_output, 1);
  fprintf (stderr, "\n");
  fflush (stderr);
  Vinhibit_quit = old_inhibit_quit;
  Vprint_level = old_print_level;
  Vprint_length = old_print_length;
  print_depth = old_print_depth;
  print_readably = old_print_readably;
  max_print = old_max_print;
  UNGCPRO;
}
#endif /* debugging kludge */


void
syms_of_print ()
{
  Voutput_stream_resource = Qnil;
  staticpro (&Voutput_stream_resource);

  defsymbol (&Qprint_escape_newlines, "print-escape-newlines");
  defsymbol (&Qprint_readably, "print-readably");

  alternate_do_pointer = 0;

  DEFVAR_LISP ("standard-output", &Vstandard_output,
    "Output stream `print' uses by default for outputting a character.\n\
This may be any function of one argument.\n\
It may also be a buffer (output is inserted before point)\n\
or a marker (output is inserted and the marker is advanced)\n\
or the symbol t (output appears in the minibuffer line).");
  Vstandard_output = Qt;
  defsymbol (&Qstandard_output, "standard-output");

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
  defsymbol (&Qfloat_output_format, "float-output-format");
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
When print-readably is true, compiled-function objects will be written in\n\
 #[...] form instead of in #<byte-code [...]> form, and two-element lists\n\
 of the form (quote object) will be written as the equivalent 'object.\n\
Do not SET this variable; bind it instead.");
  print_readably = 0;

  DEFVAR_BOOL ("print-gensym", &print_gensym,
    "If non-nil, then uninterned symbols will be printed specially.\n\
Uninterned symbols are those which are not present in `obarray', that is,\n\
those which were made with `make-symbol' or by calling `intern' with a\n\
second argument.\n\
\n\
When print-gensym is true, such symbols will be preceeded by \"#:\", which\n\
causes the reader to create a new symbol instead of interning and returning\n\
an existing one.  Beware: the #: syntax creates a new symbol each time it is\n\
seen, so if you print an object which contains two pointers to the same\n\
uninterned symbol, `read' will not duplicate that structure.\n\
\n\
Also, since emacs has no real notion of packages, there is no way for the\n\
printer to distinguish between symbols interned in no obarray, and symbols\n\
interned in an alternate obarray.");
  print_gensym = 0;

  /* prin1_to_string_buffer initialized in init_buffer_once in buffer.c */
  staticpro (&Vprin1_to_string_buffer);

  defsubr (&Sprin1);
  defsubr (&Sprin1_to_string);
  defsubr (&Sprinc);
  defsubr (&Sprint);
  defsubr (&Sterpri);
  defsubr (&Swrite_char);
  defsubr (&Salternate_debugging_output);
  defsymbol (&Qalternate_debugging_output, "alternate-debugging-output");
  defsubr (&Sexternal_debugging_output);
  defsymbol (&Qexternal_debugging_output, "external-debugging-output");
#ifndef standalone
  defsubr (&Swith_output_to_temp_buffer);
#endif /* not standalone */
}
