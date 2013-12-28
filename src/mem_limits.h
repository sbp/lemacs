/* Includes for memory limit warnings.
   Copyright (C) 1990-1993 Free Software Foundation, Inc.

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

#ifndef _EMACS_MEM_LIMITS_H_
#define _EMACS_MEM_LIMITS_H_

#ifndef BSD4_2
#ifndef USG
#include <sys/vlimit.h>
#endif /* not USG */
#else /* if BSD4_2 */
#include <sys/time.h>
#include <sys/resource.h>
#endif /* BSD4_2 */

#ifdef __STDC__
typedef void *POINTER;
#else
typedef char *POINTER;
#endif

typedef unsigned long SIZE;

#ifdef emacs
extern POINTER start_of_data ();

#ifdef BSD
#ifndef DATA_SEG_BITS
#define start_of_data() &etext
#endif
#endif

#else  /* Not emacs */ 
#define start_of_data() &etext
#endif /* Not emacs */

  

/* start of data space; can be changed by calling malloc_init */
static POINTER data_space_start;

/* Number of bytes of writable memory we can expect to be able to get */
static unsigned int lim_data;



#ifdef USG

get_lim_data ()
{
  extern long ulimit ();
    
#ifdef ULIMIT_BREAK_VALUE
  lim_data = ULIMIT_BREAK_VALUE;
#else
  lim_data = ulimit (3, 0);
#endif

  lim_data -= (long) data_space_start;
}

#else /* not USG */
#ifndef BSD4_2

get_lim_data ()
{
  lim_data = vlimit (LIM_DATA, -1);
}

#else /* BSD4_2 */

get_lim_data ()
{
  struct rlimit XXrlimit;

  getrlimit (RLIMIT_DATA, &XXrlimit);
#ifdef RLIM_INFINITY
  lim_data = XXrlimit.rlim_cur & RLIM_INFINITY; /* soft limit */
#else
  lim_data = XXrlimit.rlim_cur;	/* soft limit */
#endif
}
#endif /* BSD4_2 */
#endif /* not USG */

#endif /* _EMACS_MEM_LIMITS_H_ */
