#include "config.h"
#include "puresize.h"
#include "lisp.h"

Lisp_Object pure[PURESIZE / sizeof (Lisp_Object)]
     /* Force linker to put it into data space! */
     = {0, };
