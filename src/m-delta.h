/* Machine-dependent configuration for GNU Emacs for Motorola Delta machines.
   Copyright (C) 1986 Free Software Foundation, Inc.

   Modified by Steve Alexander at Motorola Cleveland 7/25/88
   for use with s-usg5-3.h on Motorola Delta systems.

This file is part of GNU Emacs.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU Emacs General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU Emacs, but only under the conditions described in the
GNU Emacs General Public License.   A copy of this license is
supposed to have been given to you along with GNU Emacs so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  */

/* The following re-define cpp symbols from s-usg5-3.h
   to make appropriate for the Motorola Delta series computers */

#undef  KERNEL_FILE
#define KERNEL_FILE "/sysV68"


/* We have sockets, but only if NSE is installed */
#undef HAVE_SOCKETS
#undef HAVE_PTYS
#undef SYSV_PTYS
#undef LDAV_SYMBOL


/* The following three symbols give information on
 the size of various data types.  */

#define SHORTBITS 16            /* Number of bits in a short */

#define INTBITS 32              /* Number of bits in an int */

#define LONGBITS 32             /* Number of bits in a long */

/* Define BIG_ENDIAN iff lowest-numbered byte in a word
   is the most significant byte.  */

#define BIG_ENDIAN

/* Define NO_ARG_ARRAY if you cannot take the address of the first of a
 * group of arguments and treat it as an array of the arguments.  */

/* #define NO_ARG_ARRAY */

/* Define WORD_MACHINE if addresses and such have
 * to be corrected before they can be used as byte counts.  */

/* #define WORD_MACHINE */

/* Define how to take a char and sign-extend into an int.
   On machines where char is signed, this is a no-op.  */

#define SIGN_EXTEND_CHAR(c) (c)

/* Say this machine is a 68000 */

#ifndef m68000
#define m68000
#endif

#ifndef m68k
#define m68k
#endif

/* Use type int rather than a union, to represent Lisp_Object */
/* This is desirable for most machines.  */

#define NO_UNION_TYPE

/* Define EXPLICIT_SIGN_EXTEND if XINT must explicitly sign-extend
   the 24-bit bit field into an int.  In other words, if bit fields
   are always unsigned.

   If you use NO_UNION_TYPE, this flag does not matter.  */

#define EXPLICIT_SIGN_EXTEND

/* Sys V/68 and most System V's don't have a true load average */
/* Data type of load average, as read out of kmem.  */
/* #define LOAD_AVE_TYPE struct sysinfo */

/* Convert that into an integer that is 100 for a load average of 1.0  */

/* #define LOAD_AVE_CVT(x) (int) (((double)(x.cpu[CPU_USER])) * 100.0 / ) */

/* Define CANNOT_DUMP on machines where unexec does not work.
   Then the function dump-emacs will not be defined
   and temacs will do (load "loadup") automatically unless told otherwise.  */
/* #define CANNOT_DUMP */

/* Define VIRT_ADDR_VARIES if the virtual addresses of
   pure and impure space as loaded can vary, and even their
   relative order cannot be relied on.

   Otherwise Emacs assumes that data space precedes text space,
   numerically.  */

/* #define VIRT_ADDR_VARIES */  /* Karl Kleinpaste says this isn't needed.  */

/* Define C_ALLOCA if this machine does not support a true alloca
   and the one written in C should be used instead.
   Define HAVE_ALLOCA to say that the system provides a properly
   working alloca function and it should be used.
   Define neither one if an assembler-language alloca
   in the file alloca.s should be used.  */

/* System V/68 has alloca in the PW library, but it doesn't work
   properly.  The GNU Emacs 18.55 alloca.s doesn't work with this
   system either !   */

#define C_ALLOCA

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */

#define NO_REMAP

/* Use Terminfo, not Termcap.  */

#define TERMINFO


/* XINT must explicitly sign-extend */

#define EXPLICIT_SIGN_EXTEND

/* For alloca.c */
#define STACK_DIRECTION -1

#ifdef GHS_C  /* Green Hills C compiler */
/* for the greenhills compiler optimise */
#define C_OPTIMIZE_SWITCH -OLM
#endif

/* Make etc/fakemail happy */
#define MAIL_PROGRAM_NAME "/bin/mail"

#define HAVE_SYSVIPC
