/*
 * b2m - a filter for Babyl -> Unix mail files
 *
 * usage:	b2m < babyl > mailbox
 *
 * I find this useful whenever I have to use a
 * system which - shock horror! - doesn't run
 * Gnu emacs. At least now I can read all my
 * Gnumacs Babyl format mail files!
 *
 * it's not much but it's free!
 *
 *   Ed Wilkinson
 *   E.Wilkinson@massey.ac.nz
 *   Mon Nov 7 15:54:06 PDT 1988
 *
 *   Heiko Muenkel
 *   muenkel@tnt.uni-hannover.de
 *   Sun Feb 27 1993
 *   Added the '--help' option and fixed some bugs.
 *
 */

#include <stdio.h>
#include <time.h>
#include <sys/types.h>

#include "../src/config.h"

#if __STDC__ || defined(STDC_HEADERS)
# include <stdlib.h>
# include <string.h>
#else /* ! (__STDC__ || defined(STDC_HEADERS)) */
# ifdef USG
#  include <string.h>
# else
#  include <strings.h>
#  endif

/* BSD's strings.h does not declare the type of strtok.  */
extern char *strtok ();
#endif /* ! (__STDC__ || defined(STDC_HEADERS)) */


#ifndef TRUE
#define TRUE  (1)
#endif
#ifndef FALSE
#define FALSE (0)
#endif

int header = FALSE, printing;
time_t ltoday;
char from[256], labels[256], data[256], *p, *today;

void
main (argc, argv)
     int argc;
     char **argv;
{
  if ((argc == 2) && 
      (strcmp (argv[1], "--help") == 0))
    {
      fprintf(stderr, "%s - a filter for Babyl -> Unix mail files\n", argv[0]);
      fprintf(stderr, "Usage: %s < babyl > mailbox\n", argv[0]);
      exit (-1);
    }

  ltoday = time(0);
  today = ctime(&ltoday);

  if (gets(data))
    if (strncmp(data, "BABYL OPTIONS:", 14)) {
      fprintf(stderr, "%s: not a Babyl mailfile!\n", argv[0]);
      exit(-1);
    } else
      printing = FALSE;
  else
    exit(-1);
  if (printing)
    puts(data);

  while (gets(data)) {

#if 0
    /* What was this for?  Does somebody have something against blank
       lines?  */
    if (!strcmp(data, ""))
      exit(0);
#endif

    if (!strcmp(data, "*** EOOH ***") && !printing) {
      printing = header = TRUE;
      printf("From %s %s", argv[0], today);
      continue;
    }
    
    if (!strcmp(data, "")) {
      /* save labels */
      gets(data);
      p = strtok(data, " ,\r\n\t");
      strcpy(labels, "X-Babyl-Labels: ");

      while ((p = strtok(NULL, " ,\r\n\t")))
      {
	strcat(labels, p);
	strcat(labels, ", ");
      }

      labels[strlen(labels) - 2] = '\0';
      printing = header = FALSE;
      continue;
    }

    if (!strlen(data) && header) {
      header = FALSE;
      if (strcmp(labels, "X-Babyl-Labels"))
	puts(labels);
    }
    
    if (printing)
      puts(data);
  }
}
