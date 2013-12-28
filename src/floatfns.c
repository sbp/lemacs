/* Primitive operations on floating point for GNU Emacs Lisp interpreter.
   Copyright (C) 1988 Free Software Foundation, Inc.

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


#include <signal.h>

#include "config.h"
#include "lisp.h"

Lisp_Object Qarith_error;

#ifdef LISP_FLOAT_TYPE
#include <math.h>

/* Nonzero while executing in floating point.
   This tells float_error what to do.  */

static int in_float;

#define IN_FLOAT(d) (in_float = 1, (d), in_float = 0)

/* Extract a Lisp number as a `double', or signal an error.  */

double
extract_float (num)
     Lisp_Object num;
{
  CHECK_NUMBER_OR_FLOAT (num, 0);

  if (XTYPE (num) == Lisp_Float)
    return XFLOAT (num)->data;
  return (double) XINT (num);
}

DEFUN ("acos", Facos, Sacos, 1, 1, 0,
  "Return the inverse cosine of ARG.")
  (num)
     register Lisp_Object num;
{
  double d = extract_float (num);
  IN_FLOAT (d = acos (d));
  return make_float (d);
}

DEFUN ("acosh", Facosh, Sacosh, 1, 1, 0,
  "Return the inverse hyperbolic cosine of ARG.")
  (num)
     register Lisp_Object num;
{
  double d = extract_float (num);
  IN_FLOAT (d = acosh (d));
  return make_float (d);
}

DEFUN ("asin", Fasin, Sasin, 1, 1, 0,
  "Return the inverse sine of ARG.")
  (num)
     register Lisp_Object num;
{
  double d = extract_float (num);
  IN_FLOAT (d = asin (d));
  return make_float (d);
}

DEFUN ("asinh", Fasinh, Sasinh, 1, 1, 0,
  "Return the inverse hyperbolic sine of ARG.")
  (num)
     register Lisp_Object num;
{
  double d = extract_float (num);
  IN_FLOAT (d = asinh (d));
  return make_float (d);
}

DEFUN ("atan", Fatan, Satan, 1, 1, 0,
  "Return the inverse tangent of ARG.")
  (num)
     register Lisp_Object num;
{
  double d = extract_float (num);
  IN_FLOAT (d = atan (d));
  return make_float (d);
}

DEFUN ("atanh", Fatanh, Satanh, 1, 1, 0,
  "Return the inverse hyperbolic tangent of ARG.")
  (num)
     register Lisp_Object num;
{
  double d = extract_float (num);
  IN_FLOAT (d = atanh (d));
  return make_float (d);
}

DEFUN ("bessel-j0", Fbessel_j0, Sbessel_j0, 1, 1, 0,
  "Return the bessel function j0 of ARG.")
  (num)
     register Lisp_Object num;
{
  double d = extract_float (num);
  IN_FLOAT (d = j0 (d));
  return make_float (d);
}

DEFUN ("bessel-j1", Fbessel_j1, Sbessel_j1, 1, 1, 0,
  "Return the bessel function j1 of ARG.")
  (num)
     register Lisp_Object num;
{
  double d = extract_float (num);
  IN_FLOAT (d = j1 (d));
  return make_float (d);
}

DEFUN ("bessel-jn", Fbessel_jn, Sbessel_jn, 2, 2, 0,
  "Return the order N bessel function output jn of ARG.\n\
The first arg (the order) is truncated to an integer.")
  (num1, num2)
     register Lisp_Object num1, num2;
{
  int i1 = extract_float (num1);
  double f2 = extract_float (num2);

  IN_FLOAT (f2 = jn (i1, f2));
  return make_float (f2);
}

DEFUN ("bessel-y0", Fbessel_y0, Sbessel_y0, 1, 1, 0,
  "Return the bessel function y0 of ARG.")
  (num)
     register Lisp_Object num;
{
  double d = extract_float (num);
  IN_FLOAT (d = y0 (d));
  return make_float (d);
}

DEFUN ("bessel-y1", Fbessel_y1, Sbessel_y1, 1, 1, 0,
  "Return the bessel function y1 of ARG.")
  (num)
     register Lisp_Object num;
{
  double d = extract_float (num);
  IN_FLOAT (d = y1 (d));
  return make_float (d);
}

DEFUN ("bessel-yn", Fbessel_yn, Sbessel_yn, 2, 2, 0,
  "Return the order N bessel function output yn of ARG.\n\
The first arg (the order) is truncated to an integer.")
  (num1, num2)
     register Lisp_Object num1, num2;
{
  int i1 = extract_float (num1);
  double f2 = extract_float (num2);

  IN_FLOAT (f2 = yn (i1, f2));
  return make_float (f2);
}

DEFUN ("cube-root", Fcube_root, Scube_root, 1, 1, 0,
  "Return the cube root of ARG.")
  (num)
     register Lisp_Object num;
{
  double d = extract_float (num);
  IN_FLOAT (d = cbrt (d));
  return make_float (d);
}

DEFUN ("cos", Fcos, Scos, 1, 1, 0,
  "Return the cosine of ARG.")
  (num)
     register Lisp_Object num;
{
  double d = extract_float (num);
  IN_FLOAT (d = cos (d));
  return make_float (d);
}

DEFUN ("cosh", Fcosh, Scosh, 1, 1, 0,
  "Return the hyperbolic cosine of ARG.")
  (num)
     register Lisp_Object num;
{
  double d = extract_float (num);
  IN_FLOAT (d = cosh (d));
  return make_float (d);
}

DEFUN ("erf", Ferf, Serf, 1, 1, 0,
  "Return the mathematical error function of ARG.")
  (num)
     register Lisp_Object num;
{
  double d = extract_float (num);
  IN_FLOAT (d = erf (d));
  return make_float (d);
}

DEFUN ("erfc", Ferfc, Serfc, 1, 1, 0,
  "Return the complementary error function of ARG.")
  (num)
     register Lisp_Object num;
{
  double d = extract_float (num);
  IN_FLOAT (d = erfc (d));
  return make_float (d);
}

DEFUN ("exp", Fexp, Sexp, 1, 1, 0,
  "Return the exponential base e of ARG.")
  (num)
     register Lisp_Object num;
{
  double d = extract_float (num);
  IN_FLOAT (d = exp (d));
  return make_float (d);
}

DEFUN ("expm1", Fexpm1, Sexpm1, 1, 1, 0,
  "Return the exp (x)-1 of ARG.")
  (num)
     register Lisp_Object num;
{
  double d = extract_float (num);
  IN_FLOAT (d = expm1 (d));
  return make_float (d);
}

DEFUN ("log-gamma", Flog_gamma, Slog_gamma, 1, 1, 0,
  "Return the log gamma of ARG.")
  (num)
     register Lisp_Object num;
{
  double d = extract_float (num);
  IN_FLOAT (d = lgamma (d));
  return make_float (d);
}

DEFUN ("log", Flog, Slog, 1, 1, 0,
  "Return the natural logarithm of ARG.")
  (num)
     register Lisp_Object num;
{
  double d = extract_float (num);
  IN_FLOAT (d = log (d));
  return make_float (d);
}

DEFUN ("log10", Flog10, Slog10, 1, 1, 0,
  "Return the logarithm base 10 of ARG.")
  (num)
     register Lisp_Object num;
{
  double d = extract_float (num);
  IN_FLOAT (d = log10 (d));
  return make_float (d);
}

DEFUN ("log1p", Flog1p, Slog1p, 1, 1, 0,
  "Return the log (1+x) of ARG.")
  (num)
     register Lisp_Object num;
{
  double d = extract_float (num);
  IN_FLOAT (d = log1p (d));
  return make_float (d);
}

DEFUN ("expt", Fexpt, Sexpt, 2, 2, 0,
  "Return the exponential x ** y.")
  (num1, num2)
     register Lisp_Object num1, num2;
{
  double f1, f2;

  CHECK_NUMBER_OR_FLOAT (num1, 0);
  CHECK_NUMBER_OR_FLOAT (num2, 0);
  if ((XTYPE (num1) == Lisp_Int) && /* common lisp spec */
      (XTYPE (num2) == Lisp_Int)) /* don't promote, if both are ints */
    {				/* this can be improved by pre-calculating */
      int acc, x, y;		/* some binary powers of x then acumulating */
      /* these, therby saving some time. -wsr */
      x = XINT (num1);
      y = XINT (num2);
      acc = 1;
      
      if (y < 0)
	{
	  for (; y < 0; y++)
	    acc /= x;
	}
      else
	{
	  for (; y > 0; y--)
	    acc *= x;
	}
      return XSET (x, Lisp_Int, acc);
    }
  f1 = (XTYPE (num1) == Lisp_Float) ? XFLOAT (num1)->data : XINT (num1);
  f2 = (XTYPE (num2) == Lisp_Float) ? XFLOAT (num2)->data : XINT (num2);
  IN_FLOAT (f1 = pow (f1, f2));
  return make_float (f1);
}

DEFUN ("sin", Fsin, Ssin, 1, 1, 0,
  "Return the sine of ARG.")
  (num)
     register Lisp_Object num;
{
  double d = extract_float (num);
  IN_FLOAT (d = sin (d));
  return make_float (d);
}

DEFUN ("sinh", Fsinh, Ssinh, 1, 1, 0,
  "Return the hyperbolic sine of ARG.")
  (num)
     register Lisp_Object num;
{
  double d = extract_float (num);
  IN_FLOAT (d = sinh (d));
  return make_float (d);
}

DEFUN ("sqrt", Fsqrt, Ssqrt, 1, 1, 0,
  "Return the square root of ARG.")
  (num)
     register Lisp_Object num;
{
  double d = extract_float (num);
  IN_FLOAT (d = sqrt (d));
  return make_float (d);
}

DEFUN ("tan", Ftan, Stan, 1, 1, 0,
  "Return the tangent of ARG.")
  (num)
     register Lisp_Object num;
{
  double d = extract_float (num);
  IN_FLOAT (d = tan (d));
  return make_float (d);
}

DEFUN ("tanh", Ftanh, Stanh, 1, 1, 0,
  "Return the hyperbolic tangent of ARG.")
  (num)
     register Lisp_Object num;
{
  double d = extract_float (num);
  IN_FLOAT (d = tanh (d));
  return make_float (d);
}

DEFUN ("abs", Fabs, Sabs, 1, 1, 0,
  "Return the absolute value of ARG.")
  (num)
     register Lisp_Object num;
{
  CHECK_NUMBER_OR_FLOAT (num, 0);

  if (XTYPE (num) == Lisp_Float)
    IN_FLOAT (num = make_float (fabs (XFLOAT (num)->data)));
  else if (XINT (num) < 0)
    XSETINT (num, - XFASTINT (num));

  return num;
}

DEFUN ("float", Ffloat, Sfloat, 1, 1, 0,
  "Return the floating point number equal to ARG.")
  (num)
     register Lisp_Object num;
{
  CHECK_NUMBER_OR_FLOAT (num, 0);

  if (XTYPE (num) == Lisp_Int)
    return make_float ((double) XINT (num));
  else				/* give 'em the same float back */
    return num;
}

DEFUN ("logb", Flogb, Slogb, 1, 1, 0,
  "Returns the integer that is the base 2 log of ARG.\n\
This is the same as the exponent of a float.")
     (num)
Lisp_Object num;
{
  Lisp_Object val;
  double f;

  CHECK_NUMBER_OR_FLOAT (num, 0);
  f = (XTYPE (num) == Lisp_Float) ? XFLOAT (num)->data : XINT (num);
  IN_FLOAT (val = logb (f));
  XSET (val, Lisp_Int, val);
  return val;
}

/* the rounding functions  */

DEFUN ("ceiling", Fceiling, Sceiling, 1, 1, 0,
  "Return the smallest integer no less than ARG.  (Round toward +inf.)")
  (num)
     register Lisp_Object num;
{
  CHECK_NUMBER_OR_FLOAT (num, 0);

  if (XTYPE (num) == Lisp_Float)
    IN_FLOAT (XSET (num, Lisp_Int, ceil (XFLOAT (num)->data)));

  return num;
}

DEFUN ("floor", Ffloor, Sfloor, 1, 1, 0,
  "Return the largest integer no greater than ARG.  (Round towards -inf.)")
  (num)
     register Lisp_Object num;
{
  CHECK_NUMBER_OR_FLOAT (num, 0);

  if (XTYPE (num) == Lisp_Float)
    IN_FLOAT (XSET (num, Lisp_Int, floor (XFLOAT (num)->data)));

  return num;
}

DEFUN ("round", Fround, Sround, 1, 1, 0,
  "Return the nearest integer to ARG.")
  (num)
     register Lisp_Object num;
{
  CHECK_NUMBER_OR_FLOAT (num, 0);

  if (XTYPE (num) == Lisp_Float)
    IN_FLOAT (XSET (num, Lisp_Int, rint (XFLOAT (num)->data)));

  return num;
}

DEFUN ("truncate", Ftruncate, Struncate, 1, 1, 0,
       "Truncate a floating point number to an int.\n\
Rounds the value toward zero.")
  (num)
     register Lisp_Object num;
{
  CHECK_NUMBER_OR_FLOAT (num, 0);

  if (XTYPE (num) == Lisp_Float)
    XSET (num, Lisp_Int, (int) XFLOAT (num)->data);

  return num;
}

#ifdef BSD
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

#endif /* BSD */

init_floatfns ()
{
  signal (SIGILL, float_error);
  in_float = 0;
}

syms_of_floatfns ()
{
  defsubr (&Sacos);
  defsubr (&Sacosh);
  defsubr (&Sasin);
  defsubr (&Sasinh);
  defsubr (&Satan);
  defsubr (&Satanh);
  defsubr (&Sbessel_y0);
  defsubr (&Sbessel_y1);
  defsubr (&Sbessel_yn);
  defsubr (&Sbessel_j0);
  defsubr (&Sbessel_j1);
  defsubr (&Sbessel_jn);
  defsubr (&Scube_root);
  defsubr (&Scos);
  defsubr (&Scosh);
  defsubr (&Serf);
  defsubr (&Serfc);
  defsubr (&Sexp);
  defsubr (&Sexpm1);
  defsubr (&Slog_gamma);
  defsubr (&Slog);
  defsubr (&Slog10);
  defsubr (&Slog1p);
  defsubr (&Sexpt);
  defsubr (&Ssin);
  defsubr (&Ssinh);
  defsubr (&Ssqrt);
  defsubr (&Stan);
  defsubr (&Stanh);

  defsubr (&Sabs);
  defsubr (&Sfloat);
  defsubr (&Slogb);
  defsubr (&Sceiling);
  defsubr (&Sfloor);
  defsubr (&Sround);
  defsubr (&Struncate);
}

#else /* not LISP_FLOAT_TYPE */

init_floatfns ()
{}

syms_of_floatfns ()
{}

#endif /* not LISP_FLOAT_TYPE */
