/* Interface from Emacs to terminfo.
   Copyright (C) 1985, 1986, 1993 Free Software Foundation, Inc.

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

/* Define these variables that serve as global parameters to termcap,
   so that we do not need to conditionalize the places in Emacs
   that set them.  */

char *UP, *BC, PC;
#ifdef HAVE_TERMIOS	/* copied from sysdep.c -jwz */
speed_t ospeed;
#else
short ospeed;
#endif

/* Interface to curses/terminfo library.
   Turns out that all of the terminfo-level routines look
   like their termcap counterparts except for tparm, which replaces
   tgoto.  Not only is the calling sequence different, but the string
   format is different too.
*/

#include <curses.h>
#include <term.h>

extern void *xmalloc (int size);

char *
tparam (string, outstring, len, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)
     const char *string;
     char *outstring;
     int arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9;
{
  char *temp;

  temp = tparm (string, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
  if (outstring == 0)
    outstring = ((char *) (xmalloc ((strlen (temp)) + 1)));
  strcpy (outstring, temp);
  return outstring;
}
