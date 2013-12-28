/* Client process that communicates with GNU Emacs acting as server.
   Copyright (C) 1986, 1987, 1993 Free Software Foundation, Inc.

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


#define NO_SHORTNAMES
#include "../src/config.h"
#undef read
#undef write
#undef open
#undef close
#undef signal

#if __STDC__
#include <stdlib.h>
#include <unistd.h>
#endif

#if !defined(HAVE_SOCKETS) && !defined(HAVE_SYSVIPC)
#include <stdio.h>

main (argc, argv)
     int argc;
     char **argv;
{
  fprintf (stderr, "%s: Sorry, the Emacs server is supported only\n",
	   argv[0]);
  fprintf (stderr, "on systems with Berkeley sockets or System V IPC.\n");
  exit (1);
}

#else /* HAVE_SOCKETS or HAVE_SYSVIPC */

#if ! defined (HAVE_SYSVIPC)
/* BSD code is very different from SYSV IPC code */

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/stat.h>
#include <stdio.h>
#include <errno.h>

extern int sys_nerr;
extern char *sys_errlist[];
extern int errno;

void
main (argc, argv)
     int argc;
     char **argv;
{
  char system_name[256];
  int s, i;
  FILE *out;
  struct sockaddr_un server;
  char *homedir, *cwd, *str;
  char string[BUFSIZ];

  if (argc < 2)
    {
      fprintf (stderr, "Usage: %s [+linenumber] filename\n", argv[0]);
      exit (1);
    }

  /* 
   * Open up an AF_UNIX socket in this person's home directory
   */

  if ((s = socket (AF_UNIX, SOCK_STREAM, 0)) < 0)
    {
      fprintf (stderr, "%s: ", argv[0]);
      perror ("socket");
      exit (1);
    }
  server.sun_family = AF_UNIX;
#ifndef SERVER_HOME_DIR
  {
    struct stat statbfr;

    gethostname (system_name, sizeof (system_name));
    sprintf (server.sun_path, "/tmp/esrv%d-%s", geteuid (), system_name);

    if (stat (server.sun_path, &statbfr) == -1)
      {
	if (errno == ENOENT)
	  fprintf (stderr,
		   "Can't find socket; have you started the server?\n");
	else
	  perror ("stat");
	exit (1);
      }
    if (statbfr.st_uid != geteuid())
      {
	fprintf (stderr, "Illegal socket owner\n");
	exit (1);
      }
  }
#else
  if ((homedir = getenv ("HOME")) == NULL)
    {
      fprintf (stderr, "%s: No home directory\n", argv[0]);
      exit (1);
    }
  sprintf (server.sun_path, "/tmp/esrv%d", geteuid ());
#if 0
  strcpy (server.sun_path, homedir);
  strcat (server.sun_path, "/.emacs_server");
#endif
#endif /* SERVER_HOME_DIR */
  if (connect (s, (struct sockaddr *) &server, strlen (server.sun_path) + 2)
      < 0)
    {
      fprintf (stderr, "%s: ", argv[0]);
      perror ("connect");
      exit (1);
    }
  if ((out = (FILE *) fdopen (s, "r+")) == NULL)
    {
      fprintf (stderr, "%s: ", argv[0]);
      perror ("fdopen");
      exit (1);
    }

  cwd = (char *) getwd (string);
  if (cwd == 0)
    {
      /* getwd puts message in STRING if it fails.  */
      fprintf (stderr, "%s: %s (%s)\n", argv[0], string,
	       (errno < sys_nerr) ? sys_errlist[errno] : "unknown error");
      exit (1);
    }

  for (i = 1; i < argc; i++)
    {
      if (*argv[i] == '+')
	{
	  char *p = argv[i] + 1;
	  while (*p >= '0' && *p <= '9') p++;
	  if (*p != 0)
	    fprintf (out, "%s/", cwd);
	}
      else if (*argv[i] != '/')
	fprintf (out, "%s/", cwd);
      fprintf (out, "%s ", argv[i]);
    }
  fprintf (out, "\n");
  fflush (out);

  printf ("Waiting for Emacs...");
  fflush (stdout);

  rewind (out); /* re-read the output */
  str = fgets (string, BUFSIZ, out); 

  /* Now, wait for an answer and print any messages.  */
  
  while ((str = fgets (string, BUFSIZ, out)))
    printf ("%s", str);
  
  exit (0);
}

#else /* This is the SYSV IPC section */

#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <stdio.h>

main (argc, argv)
     int argc;
     char **argv;
{
  int s;
  key_t key;
  struct msgbuf * msgp =
      (struct msgbuf *) malloc (sizeof *msgp + BUFSIZ);
  struct msqid_ds * msg_st;
  char *homedir, buf[BUFSIZ];
  char gwdirb[BUFSIZ];
  char *cwd;
  char *temp;
  char *getwd (), *getcwd (), *getenv ();
  char argv0[BUFSIZ];

  strcpy (argv0, argv[0]);

  if (argc < 2)
    {
      fprintf (stderr, "Usage: %s [+linenumber] filename\n", argv[0]);
      exit (1);
    }

  /*
   * Create a message queue using ~/.emacs_server as the path for ftok
   */
  if ((homedir = getenv ("HOME")) == NULL)
    {
      fprintf (stderr, "%s: No home directory\n", argv[0]);
      exit (1);
    }
  strcpy (buf, homedir);
  strcat (buf, "/.emacs_server");
  creat (buf, 0600);
  key = ftok (buf, 1);	/* unlikely to be anyone else using it */
  s = msgget (key, 0600 | IPC_CREAT);
  if (s == -1)
    {
      fprintf (stderr, "%s: ", argv[0]);
      perror ("msgget");
      exit (1);
    }

  /* Determine working dir, so we can prefix it to all the arguments.  */
#ifdef BSD
  temp = getwd (gwdirb);
#else
  temp = getcwd (gwdirb, sizeof gwdirb);
#endif

  cwd = gwdirb;
  if (temp != 0)
    {
      /* On some systems, cwd can look like `@machine/...';
	 ignore everything before the first slash in such a case.  */
      while (*cwd && *cwd != '/')
	cwd++;
      strcat (cwd, "/");
    }
  else
    {
      fprintf (stderr, cwd);
      exit (1);
    }

  msgp->mtext[0] = 0;
  argc--; argv++;
  while (argc)
    {
      if (*argv[0] == '+')
	{
	  char *p = argv[0] + 1;
	  while (*p >= '0' && *p <= '9') p++;
	  if (*p != 0)
	    strcat (msgp->mtext, cwd);
	}
      else if (*argv[0] != '/')
	strcat (msgp->mtext, cwd);

      strcat (msgp->mtext, argv[0]);
      strcat (msgp->mtext, " ");
      argv++; argc--;
    }
  strcat (msgp->mtext, "\n");
  msgp->mtype = 1;
  if (msgsnd (s, msgp, strlen (msgp->mtext)+1, 0) < 0)
    {
      fprintf (stderr, "%s: ", argv0);
      perror ("msgsnd");
      exit (1);
    }
  /*
   * Now, wait for an answer
   */
  printf ("Waiting for Emacs...");
  fflush (stdout);

  msgrcv (s, msgp, BUFSIZ, getpid (), 0);	/* wait for anything back */
  strcpy (buf, msgp->mtext);

  printf ("\n%s\n", buf);
  exit (0);
}

#endif /* HAVE_SYSVIPC */

#endif /* HAVE_SOCKETS or HAVE_SYSVIPC */
