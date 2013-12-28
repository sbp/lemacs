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
 */

#include <stdio.h>
#include <time.h>

#define NO_SHORTNAMES
#include "../src/config.h"

#ifndef USG
#include <strings.h>
#else
#include <string.h>
#endif

/* Why can't Sun learn to write libraries and include files? */
extern char *strtok ();

#define TRUE  (1)
#define FALSE (0)

int header = FALSE, printing;
long ltoday;
char from[256], labels[256], data[256], *p, *today;

main(argc, argv)
char **argv;
{
  ltoday = time(0);
  today = ctime(&ltoday);

  if (gets(data))
    if (strcmp(data, "BABYL OPTIONS:")) {
      fprintf(stderr, "b2m: not a Babyl mailfile!\n");
      exit(-1);
    } else
      printing = FALSE;
  else
    exit(-1);
  if (printing)
    puts(data);

  while (gets(data)) {
    if (!strcmp(data, ""))
      exit(0);

    if (!strcmp(data, "*** EOOH ***") && !printing) {
      printing = header = TRUE;
      printf("From b2m %s", today);
      continue;
    }
    
    if (!strcmp(data, "")) {
      /* save labels */
      gets(data);
      p = strtok(data, " ,\r\n\t");
      strcpy(labels, "X-Babyl-Labels: ");

      while (p = strtok(NULL, " ,\r\n\t")) {
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
