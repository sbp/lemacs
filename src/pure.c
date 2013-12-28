#include "config.h"
#include "puresize.h"

int pure[PURESIZE / sizeof (int)] = {0,};   /* Force it into data space! */
