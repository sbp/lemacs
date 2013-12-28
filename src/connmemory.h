/*
 * Just a cast-free interface to xmalloc
 */

#ifndef Lisp_Type
#include "lisp.h"
#endif

#define new(type, n) ((type*) xmalloc(sizeof(type) * n))
#define grow(type, ptr, n) ((type*) xrealloc((long*) ptr, sizeof(type) * n))
