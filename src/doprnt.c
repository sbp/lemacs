/* Output like sprintf to a buffer of specified size.
   Also takes args differently: pass one pointer to an array of strings
   in addition to the format string which is separate.
   Copyright (C) 1985, 1992, 1993 Free Software Foundation, Inc.
   Rewritten by mly to use varargs.h.

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

#include <stdio.h>
#include <ctype.h>
#include <stdarg.h>

#include "lisp.h"

static int
doprnt_1 (CONST char *string, int len, int minlen,
          char **pbufptr, int bufsize)
{
  register char *bufptr = *pbufptr;

  if (len < 0) len = strlen (string);

  if (minlen > 0)
    {
      while (minlen > len && bufsize > 0)
	{
	  *bufptr++ = ' ';
	  bufsize--;
	  minlen--;
	}
      minlen = 0;
    }
  if (len > bufsize)
    len = bufsize;
  memcpy (bufptr, string, len);
  bufptr += len;
  bufsize -= len;
  if (minlen < 0)
    {
      minlen = -minlen;
      while (minlen >= len && bufsize > 0)
	{
	  *bufptr++ = ' ';
	  bufsize--;
	  minlen--;
	}
    }
  *pbufptr = bufptr;
  return (bufsize);
}


static int
emacs_doprnt_1 (char *buffer, int bufsize, 
		CONST char *format, int format_length,
		int nargs, 
		/*>>> Gag me, gag me, gag me */
		Lisp_Object *largs, va_list vargs)
{
  register CONST char *fmt = format; /* Pointer into format string */
  register CONST char *format_end;
  char *bufptr = buffer;        /* Pointer into output buffer.. */
  int cnt = 0;                  /* Argnum counter */
  /* int size;  --  Field width factor; e.g., %90d */

  int argnum;                   /* Arg number, where 1 means first arg */
  int place;                    /* Digit multiplier; 10's place, etc. */

  if (format_length < 0)
    format_length = strlen (format);
  format_end = format + format_length;

  bufsize--;                    /* space for terminating 0 */
  while (fmt != format_end && bufsize > 0) /* Loop until end of format
					      string or buffer full */
    {
      char ch = *fmt++;

      if (ch == '%')		/* Check for a '%' character */
	{
	  /* Copy this one %-spec into fmtcopy.  */
	  char fmtcpy[20];
	  int flen;

	  fmtcpy[0] = '%';
	  for (flen = 1; ;)
	    {
	      ch = *fmt;

	      /* If we encounter a $, scan backwards to get a decimal digit
		 string specifying the argument number and reset the %-spec.
		 If there is no such number, error. */
	      if (ch == '$')
		{
		  argnum = 0;
		  place = 1;
		  while (isdigit(fmtcpy[--flen]))
		    {
		      argnum += (fmtcpy[flen] - '0') * place;
		      place *= 10;
		    }
		  if (argnum == 0)
		    error (GETTEXT ("No positive integer between %% and $"));

		  cnt = argnum - 1;
		  flen++;
		  fmt++;

		  if (fmt == format_end)
		    error (GETTEXT ("%%$ appears at end of string"));
		}

	      fmtcpy[flen] = ch;
	      if (fmt == format_end)
		error (GETTEXT ("Invalid format operation %s"), fmtcpy);
	      fmt++; flen++;
	      if (!isdigit(ch) && ch != '-' && ch != ' ' && ch != '$')
		break;
	    }
	  fmtcpy[flen] = 0;

	  switch (ch)
	    {
	    default:
	      error (GETTEXT ("Invalid format operation %%%c"), ch);
	      break;

	      /* case 'b': */
	    case 'd':
	    case 'o':
	    case 'x':
	      {
		char tembuf[512];
		int a;

		if (cnt == nargs)
		  error (GETTEXT ("Format string wants too many arguments"));
              
		if (largs)
		  a = XINT (largs[cnt]);
		else
		  a = va_arg (vargs, int);
		cnt++;

		sprintf (tembuf, fmtcpy, a);
		/* Now copy tembuf into final output, truncating as nec.  */
		bufsize = doprnt_1 (tembuf, strlen (tembuf), 0, &bufptr,
				    bufsize);
		break;
	      }

	    case 'S':
	    case 's':
	      {
		char *string;
		int string_length;
		int minlen = 0;

		if (cnt == nargs)
		  error (GETTEXT ("Too few arguments for format string"));
              
		if (ch == 'S') fmtcpy[flen - 1] = 's'; /* printf wants this */

		if (largs)
		  {
		    Lisp_Object a = largs[cnt];
		    if (!STRINGP (a))
		      {
                        error (GETTEXT ("format specifier doesn't match argument type"));
			bufsize = doprnt_1 ("[not a string]", -1, 0,
					    &bufptr, bufsize);
			goto punt;
		      }
		    else
		      {
			string = (char *) XSTRING (a)->data;
			string_length = XSTRING (a)->size;
		      }
		  }
		else
		  {
		    string = va_arg (vargs, char *);
		    string_length = -1;
		  }
		cnt++;

		if (fmtcpy[1] != 's')
		  minlen = atoi (&fmtcpy[1]);
                
		/* Copy string into final output, truncating if no room.  */
		bufsize = doprnt_1 (string, string_length, minlen,
				    &bufptr, bufsize);
	      punt:
		break;
	      }

	    case 'c':
	      {
		char a;

		if (cnt == nargs)
		  error (GETTEXT ("Format string wants too many arguments"));

		if (largs)
		  a = (char) XINT (largs[cnt]);
		else
		  a = va_arg (vargs, int); /* not char -- "the type used in
					      va_arg is supposed to match the
					      actual type **after default
					      promotions**." */
		cnt++;

		*bufptr++ = a;
		bufsize--;
		break;
	      }

	    case '%':
	      {
		*bufptr++ = '%';
		bufsize--;
		break;
	      }
	    }
	}
      else
	{
	  *bufptr++ = ch;	/* Just some characters; Copy 'em */
	  bufsize--;
	}
    }

  *bufptr = 0;			/* Make sure our string end with a '\0' */
  return (bufptr - buffer);
}

/* You really don't want to know why this is necessary... */
static int
emacs_doprnt_2 (char *buffer, int bufsize, 
		CONST char *format, int format_length,
		int nargs, Lisp_Object *largs, ...)
{
  va_list vargs;
  int val;
  va_start (vargs, largs);
  val = emacs_doprnt_1 (buffer, bufsize, format, format_length, nargs,
			largs, vargs);
  va_end (vargs);
  return val;
}

int
emacs_doprnt (char *buffer, int bufsize, 
	      CONST char *format, int format_length,
	      int nargs, va_list vargs)
{
  return emacs_doprnt_1 (buffer, bufsize, format, format_length, nargs,
			 0, vargs);
}

int
emacs_doprnt_lisp (char *buffer, int bufsize, 
		   CONST char *format, int format_length,
		   int nargs, Lisp_Object *largs)
{
  return emacs_doprnt_2 (buffer, bufsize, format, format_length, nargs, largs);
}
