/*
 * yow.c
 * 
 * Print a quotation from Zippy the Pinhead.
 * Qux <Kaufman-David@Yale> March 6, 1986
 * 
 * With dynamic memory allocation.
 */

#include <stdio.h>
#include <ctype.h>
#include "../src/paths.h"      /* For PATH_EXEC.  */

#define BUFSIZE  80
#define SEP      '\0'

#ifndef YOW_FILE
#define YOW_FILE "yow.lines"
#endif

main (argc, argv)
     int argc;
     char *argv[];
{
  FILE *fp;
  char file[BUFSIZ];
  void yow(), setup_yow();

  if (argc > 2 && !strcmp (argv[1], "-f"))
    strcpy (file, argv[2]);
  else
#ifdef vms
    sprintf (file, "%s%s", PATH_EXEC, YOW_FILE);
#else
    sprintf (file, "%s/%s", PATH_EXEC, YOW_FILE);
#endif

  if ((fp = fopen(file, "r")) == NULL) {
    perror(file);
    exit(1);
  }

  /* initialize random seed */
  srand((int) (getpid() + time((long *) 0)));

  setup_yow(fp);
  yow(fp);
  fclose(fp);
  exit(0);
}

static long len = -1;
static long header_len;

#define AVG_LEN 40		/* average length of a quotation */

/* Sets len and header_len */
void
setup_yow(fp)
     FILE *fp;
{
  int c;

  /* Get length of file */
  /* Because the header (stuff before the first SEP) can be very long,
   * thus biasing our search in favor of the first quotation in the file,
   * we explicitly skip that. */
  while ((c = getc(fp)) != SEP) {
    if (c == EOF) {
      fprintf(stderr, "File contains no separators.\n");
      exit(2);
    }
  }
  header_len = ftell(fp);
  if (header_len > AVG_LEN)
    header_len -= AVG_LEN;	/* allow the first quotation to appear */
	
  if (fseek(fp, 0L, 2) == -1) {
    perror("fseek 1");
    exit(1);
  }
  len = ftell(fp) - header_len;
}


/* go to a random place in the file and print the quotation there */
void
yow (fp)
     FILE *fp;
{
  long offset;
  int c, i = 0;
  char *buf;
  unsigned int bufsize;
  char *malloc(), *realloc();

  offset = rand() % len + header_len;
  if (fseek(fp, offset, 0) == -1) {
    perror("fseek 2");
    exit(1);
  }

  /* Read until SEP, read next line, print it.
     (Note that we will never print anything before the first seperator.)
     If we hit EOF looking for the first SEP, just recurse. */
  while ((c = getc(fp)) != SEP)
    if (c == EOF) {
      yow(fp);
      return;
    }

  /* Skip leading whitespace, then read in a quotation.
     If we hit EOF before we find a non-whitespace char, recurse. */
  while (isspace(c = getc(fp)))
    ;
  if (c == EOF) {
    yow(fp);
    return;
  }

  bufsize = BUFSIZE;
  buf = malloc(bufsize);
  if (buf == (char *)0) {
    fprintf(stderr, "can't allocate any memory\n");
    exit (3);
  }

  buf[i++] = c;
  while ((c = getc(fp)) != SEP && c != EOF) {
    buf[i++] = c;
	
    if (i == bufsize-1) {
      /* Yow! Is this quotation too long yet? */
      bufsize *= 2;
      buf = realloc(buf, bufsize);
      if (buf == (char *)0) {
	fprintf(stderr, "can't allocate more memory\n");
	exit (3);
      }
    }
  }
  buf[i++] = 0;
  printf("%s\n", buf);
}

