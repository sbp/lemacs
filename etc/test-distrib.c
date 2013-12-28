#include <stdio.h>
#if __STDC__
# include <stdlib.h>
# include <string.h>
# ifndef _POSIX_SOURCE
#  define _POSIX_SOURCE
# endif
#endif

/* Break string in two parts to avoid buggy C compilers that ignore characters
   after nulls in strings.  */

char string1[] = "Testing distribution of nonprinting chars:\n\
Should be 0177: \177 Should be 0377: \377 Should be 0212: \212.\n\
Should be 0000: ";

char string2[] = ".\n\
This file is read by the `test-distribution' program.\n\
If you change it, you will make that program fail.\n";

char buf[300];
  
/* Like `read' but keeps trying until it gets SIZE bytes or reaches eof.  */
int
cool_read (fd, buf, size)
     int fd;
     char *buf;
     int size;
{
  int num, sofar = 0;

  while (1)
    {
      if ((num = read (fd, buf + sofar, size - sofar)) == 0)
	return sofar;
      else if (num < 0)
	return num;
      sofar += num;
    }
}

main (argc, argv)
     int argc;
     char **argv;
{
  char *file = (argc > 1 ? argv[1] : "testfile");
  int fd = open (file, 0);

  if (fd < 0)
    {
      char buf [255];
      sprintf (buf, "opening `%s'", file);
      perror (buf);
      exit (2);
    }
  if (cool_read (fd, buf, sizeof string1) != sizeof string1 ||
      strcmp (buf, string1) ||
      cool_read (fd, buf, sizeof string2) != sizeof string2 - 1 ||
      strncmp (buf, string2, sizeof string2 - 1))
    {
      fprintf (stderr, "Data in file `%s' has been damaged.\n\
Most likely this means that many nonprinting characters\n\
have been corrupted in the files of Emacs, and it will not work.\n",
	       file);
      exit (2);
    }
  close (fd);
#ifdef VMS
  exit (1);			/* On VMS, success is 1.  */
#else
  exit (0);
#endif
}
