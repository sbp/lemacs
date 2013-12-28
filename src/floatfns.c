/* Primitive operations on floating point for GNU Emacs Lisp interpreter.
   Copyright (C) 1988-1993 Free Software Foundation, Inc.

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

/* ANSI C requires only these float functions:
   acos, asin, atan, atan2, ceil, cos, cosh, exp, fabs, floor, fmod,
   frexp, ldexp, log, log10, modf, pow, sin, sinh, sqrt, tan, tanh.

   Define HAVE_INVERSE_HYPERBOLIC if you have acosh, asinh, and atanh.
   Define HAVE_CBRT if you have cbrt().
   Define HAVE_RINT if you have rint().
   If you don't define these, then the appropriate routines will be simulated.

   Define HAVE_MATHERR if on a system supporting the SysV matherr() callback.
   (This should happen automatically.)

   Define FLOAT_CHECK_ERRNO if the float library routines set errno.
   This has no effect if HAVE_MATHERR is defined.

   Define FLOAT_CATCH_SIGILL if the float library routines signal SIGILL.
   (What systems actually do this?  Let me know. -jwz)

   Define FLOAT_CHECK_DOMAIN if the float library doesn't handle errors by
   either setting errno, or signalling SIGFPE/SIGILL.  Otherwise, domain and
   range checking will happen before calling the float routines.  This has
   no effect if HAVE_MATHERR is defined (since matherr will be called when
   a domain error occurs.)
 */

#include <signal.h>

#include "config.h"
#include "lisp.h"

#ifdef LISP_FLOAT_TYPE

#include <math.h>

#if defined(DOMAIN) && defined(SING) && defined(OVERFLOW)
    /* If those are defined, then this is probably a `matherr' machine. */
# ifndef HAVE_MATHERR
#  define HAVE_MATHERR
# endif
#endif

#ifdef HAVE_MATHERR
# ifdef FLOAT_CHECK_ERRNO
#  undef FLOAT_CHECK_ERRNO
# endif
# ifdef FLOAT_CHECK_DOMAIN
#  undef FLOAT_CHECK_DOMAIN
# endif
#endif

#ifdef FLOAT_CHECK_ERRNO
# include <errno.h>
#endif

extern Lisp_Object Qarith_error, Qrange_error, Qdomain_error;
extern Lisp_Object Qsingularity_error, Qoverflow_error, Qunderflow_error;

/* Nonzero while executing in floating point.
   This tells float_error what to do.  */
static int in_float;

#ifdef FLOAT_CHECK_ERRNO
#define IN_FLOAT(d)					\
  do {							\
    in_float = 1; errno = 0; (d); in_float = 0;		\
    switch (errno) {					\
    case 0: break;					\
    case EDOM:	 Fsignal (Qdomain_error, Qnil); break;	\
    case ERANGE: Fsignal (Qrange_error, Qnil); break; 	\
    default:	 Fsignal (Qarith_error, Qnil); break; 	\
    }							\
  } while (0)
#else
#define IN_FLOAT(d) (in_float = 1, (d), in_float = 0)
#endif

#ifndef HAVE_RINT
#define rint(x) (floor((x)+0.5))
#endif

#define range_error(op,arg) \
	Fsignal (Qrange_error, list2 (build_string ((op)), (arg)))
#define domain_error(op,arg) \
	Fsignal (Qdomain_error, list2 (build_string ((op)), (arg)))
#define domain_error2(op,a1,a2) \
	Fsignal (Qdomain_error, list3 (build_string ((op)), (a1), (a2)))

/* Extract a Lisp number as a `double', or signal an error.  */

double
extract_float (arg)
     Lisp_Object arg;
{
  CHECK_NUMBER (arg, 0);

  if (FLOATP (arg))
    return XFLOAT (arg)->data;
  return (double) XINT (arg);
}

DEFUN ("acos", Facos, Sacos, 1, 1, 0,
  "Return the inverse cosine of ARG.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
#ifdef FLOAT_CHECK_DOMAIN
  if (d > 1.0 || d < -1.0)
    domain_error ("acos", arg);
#endif
  IN_FLOAT (d = acos (d));
  return make_float (d);
}

DEFUN ("acosh", Facosh, Sacosh, 1, 1, 0,
  "Return the inverse hyperbolic cosine of ARG.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
#ifdef FLOAT_CHECK_DOMAIN
  if (d < 1.0)
    domain_error ("acosh", arg);
#endif
#ifdef HAVE_INVERSE_HYPERBOLIC
  IN_FLOAT (d = acosh (d));
#else
  IN_FLOAT (d = log (d + sqrt (d*d - 1.0)));
#endif
  return make_float (d);
}

DEFUN ("asin", Fasin, Sasin, 1, 1, 0,
  "Return the inverse sine of ARG.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
#ifdef FLOAT_CHECK_DOMAIN
  if (d > 1.0 || d < -1.0)
    domain_error ("asin", arg);
#endif
  IN_FLOAT (d = asin (d));
  return make_float (d);
}

DEFUN ("asinh", Fasinh, Sasinh, 1, 1, 0,
  "Return the inverse hyperbolic sine of ARG.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
#ifdef HAVE_INVERSE_HYPERBOLIC
  IN_FLOAT (d = asinh (d));
#else
  IN_FLOAT (d = log (d + sqrt (d*d + 1.0)));
#endif
  return make_float (d);
}

DEFUN ("atan", Fatan, Satan, 1, 2, 0,
  "Return the inverse tangent of ARG.")
  (arg1, arg2)
     register Lisp_Object arg1, arg2;
{
  double d = extract_float (arg1);
  if (NILP (arg2))
    IN_FLOAT (d = atan (d));
  else
    {
      double d2 = extract_float (arg2);
#ifdef FLOAT_CHECK_DOMAIN
      if (d == 0.0 && d2 == 0.0)
	domain_error2 ("atan", arg1, arg2);
#endif
      IN_FLOAT (d = atan2 (d, d2));
    }
  return make_float (d);
}

DEFUN ("atanh", Fatanh, Satanh, 1, 1, 0,
  "Return the inverse hyperbolic tangent of ARG.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
#ifdef FLOAT_CHECK_DOMAIN
  if (d >= 1.0 || d <= -1.0)
    domain_error ("atanh", arg);
#endif
#ifdef HAVE_INVERSE_HYPERBOLIC
  IN_FLOAT (d = atanh (d));
#else
  IN_FLOAT (d = 0.5 * log ((1.0 + d) / (1.0 - d)));
#endif
  return make_float (d);
}

DEFUN ("cos", Fcos, Scos, 1, 1, 0,
  "Return the cosine of ARG.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
  IN_FLOAT (d = cos (d));
  return make_float (d);
}

DEFUN ("cosh", Fcosh, Scosh, 1, 1, 0,
  "Return the hyperbolic cosine of ARG.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
#ifdef FLOAT_CHECK_DOMAIN
  if (d > 710.0 || d < -710.0)
    range_error ("cosh", arg);
#endif
  IN_FLOAT (d = cosh (d));
  return make_float (d);
}

DEFUN ("cube-root", Fcube_root, Scube_root, 1, 1, 0,
  "Return the cube root of ARG.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
#ifdef HAVE_CBRT
  IN_FLOAT (d = cbrt (d));
#else
  if (d >= 0.0)
    IN_FLOAT (d = pow (d, 1.0/3.0));
  else
    IN_FLOAT (d = -pow (-d, 1.0/3.0));
#endif
  return make_float (d);
}

DEFUN ("exp", Fexp, Sexp, 1, 1, 0,
  "Return the exponential base e of ARG.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
#ifdef FLOAT_CHECK_DOMAIN
  if (d > 709.7827)   /* Assume IEEE doubles here */
    range_error ("exp", arg);
  else if (d < -709.0)
    return make_float (0.0);
  else
#endif
    IN_FLOAT (d = exp (d));
  return make_float (d);
}

DEFUN ("log", Flog, Slog, 1, 2, 0,
  "Return the natural logarithm of ARG.\n\
With two arguments, return the logarithm of ARG to the base ARG2.")
  (arg1, arg2)
     register Lisp_Object arg1, arg2;
{
  double d = extract_float (arg1);
#ifdef FLOAT_CHECK_DOMAIN
  if (d <= 0.0)
    domain_error2 ("log", arg1, arg2);
#endif
  if (NILP (arg2))
    IN_FLOAT (d = log (d));
  else
    {
      double d2 = extract_float (arg2);
#ifdef FLOAT_CHECK_DOMAIN
      if (d2 <= 0.0 || d2 == 1.0)
	domain_error2 ("log", arg1, arg2);
#endif
      if (d2 == 10.0)
	IN_FLOAT (d = log10 (d));
      else
	IN_FLOAT (d = log (d) / log (d2));
    }
  return make_float (d);
}

DEFUN ("log10", Flog10, Slog10, 1, 1, 0,
  "Return the logarithm base 10 of ARG.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
#ifdef FLOAT_CHECK_DOMAIN
  if (d <= 0.0)
    domain_error ("log10", arg);
#endif
  IN_FLOAT (d = log10 (d));
  return make_float (d);
}

DEFUN ("expt", Fexpt, Sexpt, 2, 2, 0,
  "Return the exponential x ** y.")
  (arg1, arg2)
     register Lisp_Object arg1, arg2;
{
  double f1, f2;

  CHECK_NUMBER (arg1, 0);
  CHECK_NUMBER (arg2, 0);
  if ((FIXNUMP (arg1)) && /* common lisp spec */
      (FIXNUMP (arg2))) /* don't promote, if both are ints */
    {
      int acc, x, y;
      x = XINT (arg1);
      y = XINT (arg2);
      
      if (y < 0)
	{
	  if (x == 1)
	    acc = 1;
	  else if (x == -1)
	    acc = (y & 1) ? -1 : 1;
	  else
	    acc = 0;
	}
      else
	{
	  acc = 1;
	  while (y > 0)
	    {
	      if (y & 1)
		acc *= x;
	      x *= x;
	      y = (unsigned)y >> 1;
	    }
	}
      return XSET (x, Lisp_Int, acc);
    }
  f1 = (FLOATP (arg1)) ? XFLOAT (arg1)->data : XINT (arg1);
  f2 = (FLOATP (arg2)) ? XFLOAT (arg2)->data : XINT (arg2);
  /* Really should check for overflow, too */
  if (f1 == 0.0 && f2 == 0.0)
    f1 = 1.0;
#ifdef FLOAT_CHECK_DOMAIN
  else if ((f1 == 0.0 && f2 < 0.0) || (f1 < 0 && f2 != floor(f2)))
    domain_error2 ("pow", arg1, arg2);
#endif
  IN_FLOAT (f1 = pow (f1, f2));
  return make_float (f1);
}

DEFUN ("sin", Fsin, Ssin, 1, 1, 0,
  "Return the sine of ARG.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
  IN_FLOAT (d = sin (d));
  return make_float (d);
}

DEFUN ("sinh", Fsinh, Ssinh, 1, 1, 0,
  "Return the hyperbolic sine of ARG.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
#ifdef FLOAT_CHECK_DOMAIN
  if (d > 710.0 || d < -710.0)
    range_error ("sinh", arg);
#endif
  IN_FLOAT (d = sinh (d));
  return make_float (d);
}

DEFUN ("sqrt", Fsqrt, Ssqrt, 1, 1, 0,
  "Return the square root of ARG.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
#ifdef FLOAT_CHECK_DOMAIN
  if (d < 0.0)
    domain_error ("sqrt", arg);
#endif
  IN_FLOAT (d = sqrt (d));
  return make_float (d);
}

DEFUN ("tan", Ftan, Stan, 1, 1, 0,
  "Return the tangent of ARG.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
  double c = cos (d);
#ifdef FLOAT_CHECK_DOMAIN
  if (c == 0.0)
    domain_error ("tan", arg);
#endif
  IN_FLOAT (d = sin (d) / c);
  return make_float (d);
}

DEFUN ("tanh", Ftanh, Stanh, 1, 1, 0,
  "Return the hyperbolic tangent of ARG.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
  IN_FLOAT (d = tanh (d));
  return make_float (d);
}

DEFUN ("abs", Fabs, Sabs, 1, 1, 0,
  "Return the absolute value of ARG.")
  (arg)
     register Lisp_Object arg;
{
  CHECK_NUMBER (arg, 0);

  if (FLOATP (arg))
    IN_FLOAT (arg = make_float ((double) fabs (XFLOAT (arg)->data)));
  else if (XINT (arg) < 0)
    XSETINT (arg, - XFASTINT (arg));

  return arg;
}

DEFUN ("float", Ffloat, Sfloat, 1, 1, 0,
  "Return the floating point number equal to ARG.")
  (arg)
     register Lisp_Object arg;
{
  CHECK_NUMBER (arg, 0);

  if (FIXNUMP (arg))
    return make_float ((double) XINT (arg));
  else				/* give 'em the same float back */
    return arg;
}

/* the rounding functions  */

DEFUN ("ceiling", Fceiling, Sceiling, 1, 1, 0,
  "Return the smallest integer no less than ARG.  (Round toward +inf.)")
  (arg)
     register Lisp_Object arg;
{
  CHECK_NUMBER (arg, 0);

  if (FLOATP (arg))
    IN_FLOAT (XSET (arg, Lisp_Int, ceil (XFLOAT (arg)->data)));

  return arg;
}

DEFUN ("floor", Ffloor, Sfloor, 1, 1, 0,
  "Return the largest integer no greater than ARG.  (Round towards -inf.)")
  (arg)
     register Lisp_Object arg;
{
  CHECK_NUMBER (arg, 0);

  if (FLOATP (arg))
    IN_FLOAT (XSET (arg, Lisp_Int, floor (XFLOAT (arg)->data)));

  return arg;
}

DEFUN ("round", Fround, Sround, 1, 1, 0,
  "Return the nearest integer to ARG.")
  (arg)
     register Lisp_Object arg;
{
  CHECK_NUMBER (arg, 0);

  if (FLOATP (arg))
    IN_FLOAT (XSET (arg, Lisp_Int, rint (XFLOAT (arg)->data)));

  return arg;
}

DEFUN ("truncate", Ftruncate, Struncate, 1, 1, 0,
       "Truncate a floating point number to an int.\n\
Rounds the value toward zero.")
  (arg)
     register Lisp_Object arg;
{
  CHECK_NUMBER (arg, 0);

  if (FLOATP (arg))
    XSET (arg, Lisp_Int, (int) XFLOAT (arg)->data);

  return arg;
}

DEFUN ("fceiling", Ffceiling, Sfceiling, 1, 1, 0,
  "Return the smallest integer no less than ARG, as a float.\n\
\(Round toward +inf.\)")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
  IN_FLOAT (d = ceil (d));
  return make_float (d);
}

DEFUN ("ffloor", Fffloor, Sffloor, 1, 1, 0,
  "Return the largest integer no greater than ARG, as a float.\n\
\(Round towards -inf.\)")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
  IN_FLOAT (d = floor (d));
  return make_float (d);
}

DEFUN ("fround", Ffround, Sfround, 1, 1, 0,
  "Return the nearest integer to ARG, as a float.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
  IN_FLOAT (d = rint (d));
  return make_float (d);
}

DEFUN ("ftruncate", Fftruncate, Sftruncate, 1, 1, 0,
       "Truncate a floating point number to an integral float value.\n\
Rounds the value toward zero.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
  if (d >= 0.0)
    IN_FLOAT (d = floor (d));
  else
    IN_FLOAT (d = ceil (d));
  return make_float (d);
}

#ifdef FLOAT_CATCH_SIGILL
static void
float_error (signo)
     int signo;
{
  if (! in_float)
    fatal_error_signal (signo);

#ifdef BSD4_1
  sigrelse (SIGILL);
#else /* not BSD4_1 */
  sigsetmask (0);
#endif /* not BSD4_1 */

  in_float = 0;

  Fsignal (Qarith_error, Qnil);
}

/* Another idea was to replace the library function `infnan'
   where SIGILL is signaled.  */

#endif /* FLOAT_CATCH_SIGILL */

#ifdef HAVE_MATHERR
int 
matherr (x)
     struct exception *x;
{
  Lisp_Object args;
  if (! in_float)
    /* Not called from emacs-lisp float routines; do the default thing. */
    return 0;
  args =
    Fcons (build_string (x->name),
	   Fcons (make_float (x->arg1),
		  ((!strcmp (x->name, "log") || !strcmp (x->name, "pow"))
		   ? Fcons (make_float (x->arg2), Qnil)
		   : Qnil)));
  switch (x->type)
    {
    case DOMAIN:	Fsignal (Qdomain_error, args);		break;
    case SING:		Fsignal (Qsingularity_error, args);	break;
    case OVERFLOW:	Fsignal (Qoverflow_error, args);	break;
    case UNDERFLOW:	Fsignal (Qunderflow_error, args);	break;
    default:		Fsignal (Qarith_error, args);		break;
    }
  return (1);	/* don't set errno or print a message */
}
#endif /* HAVE_MATHERR */

void
init_floatfns ()
{
#ifdef FLOAT_CATCH_SIGILL
  signal (SIGILL, float_error);
#endif 
  in_float = 0;
}

void
syms_of_floatfns ()
{
  defsubr (&Sacos);
  defsubr (&Sacosh);
  defsubr (&Sasin);
  defsubr (&Sasinh);
  defsubr (&Satan);
  defsubr (&Satanh);
  defsubr (&Scos);
  defsubr (&Scosh);
  defsubr (&Scube_root);
  defsubr (&Sexp);
  defsubr (&Slog);
  defsubr (&Slog10);
  defsubr (&Sexpt);
  defsubr (&Ssin);
  defsubr (&Ssinh);
  defsubr (&Ssqrt);
  defsubr (&Stan);
  defsubr (&Stanh);

  defsubr (&Sabs);
  defsubr (&Sfloat);
  defsubr (&Sceiling);
  defsubr (&Sfloor);
  defsubr (&Sround);
  defsubr (&Struncate);
  defsubr (&Sfceiling);
  defsubr (&Sffloor);
  defsubr (&Sfround);
  defsubr (&Sftruncate);
}

#else /* not LISP_FLOAT_TYPE */

init_floatfns ()
{}

syms_of_floatfns ()
{}

#endif /* not LISP_FLOAT_TYPE */
