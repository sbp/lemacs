/* Give this program DOCSTR.mm.nn as standard input
   and it outputs to standard output
   a file of texinfo input containing the doc strings.
   
   This version sorts the output by function name.
   */

#include "../src/config.h"

#include <stdio.h>
#include <ctype.h>
#if __STDC__ || defined(STDC_HEADERS)
# include <stdlib.h> /* for qsort() and malloc() */
# include <string.h>
static void *xmalloc (int);
# ifndef CONST
#  define CONST const
# endif
#else
extern char *malloc ();
static void *xmalloc ();
# ifndef CONST
#  define CONST
# endif
#endif

#define NUL	'\0'
#define MARKER '\037'

#define DEBUG 0

typedef struct line LINE;

struct line
{
  LINE *next;			/* ptr to next or NULL */
  char *line;			/* text of the line */
};

typedef struct docstr DOCSTR;

struct docstr			/* Allocated thing for an entry. */
{
  DOCSTR *next;			/* next in the chain */
  char *name;			/* name of the function or var */
  LINE *first;			/* first line of doc text. */
  char type;			/* 'F' for function, 'V' for variable */
};


/* Print error message.  `s1' is printf control string, `s2' is arg for it. */

static void
error (s1, s2)
     char *s1, *s2;
{
  fprintf (stderr, "sorted-doc: ");
  fprintf (stderr, s1, s2);
  fprintf (stderr, "\n");
}

/* Print error message and exit.  */

static void
fatal (s1, s2)
     char *s1, *s2;
{
  error (s1, s2);
  exit (1);
}

/* Like malloc but get fatal error if memory is exhausted.  */

static void *
xmalloc (size)
     int size;
{
  char *result = malloc ((unsigned)size);
  if (result == NULL)
    fatal ("%s", "virtual memory exhausted");
  return result;
}

char *
strsav (str)
     char * str;
{
  char *buf = xmalloc (strlen (str) + 1);
  (void) strcpy (buf, str);
  return (buf);
}

/* Comparison function for qsort to call.  */

int
cmpdoc (a, b)
     DOCSTR **a;
     DOCSTR **b;
{
  register int val = strcmp ((*a)->name, (*b)->name);
  if (val) return val;
  return (*a)->type - (*b)->type;
}


enum state
{
  WAITING, BEG_NAME, NAME_GET, BEG_DESC, DESC_GET
};

char *states[] =
{
  "WAITING", "BEG_NAME", "NAME_GET", "BEG_DESC", "DESC_GET"
};
    
int
main ()
{
  register DOCSTR *dp = NULL;	/* allocated DOCSTR */
  register LINE *lp = NULL;	/* allocated line */
  register char *bp;		/* ptr inside line buffer */
  /* int notfirst = 0;		/ * set after read something */
  register enum state state = WAITING; /* state at start */
  int cnt = 0;			/* number of DOCSTRs read */

  DOCSTR *docs;			/* chain of allocated DOCSTRS */
  char buf[512];		/* line buffer */
    
  while (1)			/* process one char at a time */
    {
      /* this char from the DOCSTR file */
      register int ch = getchar ();

      /* Beginnings */

      if (state == WAITING)
	{
	  if (ch == MARKER)
	    state = BEG_NAME;
	}
      else if (state == BEG_NAME)
	{
	  cnt++;
	  if (dp == NULL)	/* first dp allocated */
	    {
	      docs = dp = (DOCSTR*) xmalloc (sizeof (DOCSTR));
	    }
	  else			/* all the rest */
	    {
	      dp->next = (DOCSTR*) xmalloc (sizeof (DOCSTR));
	      dp = dp->next;
	    }
	  lp = NULL;
	  dp->next = NULL;
	  bp = buf;
	  state = NAME_GET;
	  /* Record whether function or variable.  */
	  dp->type = ch;
	  ch = getchar ();
	}
      else if (state == BEG_DESC)
	{
	  if (lp == NULL)	/* first line for dp */
	    {
	      dp->first = lp = (LINE*)xmalloc (sizeof (LINE));
	    }
	  else			/* continuing lines */
	    {
	      lp->next = (LINE*)xmalloc (sizeof (LINE));
	      lp = lp->next;
	    }
	  lp->next = NULL;
	  bp = buf;
	  state = DESC_GET;
	}
	
      /* process gets */

      if (state == NAME_GET || state == DESC_GET)
	{
	  if (ch != MARKER && ch != '\n' && ch != EOF)
	    {
	      *bp++ = ch;
	    }
	  else			/* saving and changing state */
	    {
	      *bp = NUL;
	      bp = strsav (buf);

	      if (state == NAME_GET)
		dp->name = bp;
	      else
		lp->line = bp;

	      bp = buf;
	      state =  (ch == MARKER) ? BEG_NAME : BEG_DESC;
	    }
	}			/* NAME_GET || DESC_GET */
      if (ch == EOF)
	break;
    }

  {
    DOCSTR **array;
    register int i;		/* counter */

    /* build array of ptrs to DOCSTRs */

    array = (DOCSTR**)xmalloc (cnt * sizeof (*array));
    for (dp = docs, i = 0; dp != NULL ; dp = dp->next)
      array[i++] = dp;

    /* sort the array by name; within each name, by type */

    qsort ((char*)array, cnt, sizeof (DOCSTR*),
	   /* was cast to (int (*)(CONST void *, CONST void *))
	      but that loses on HP because CONST_IS_LOSING. */
	   /* This one loses too: (int (*)()) */
	   /* Ok, so let's try const instead of CONST.  Fuck me!!! */
	   (int (*)(const void *, const void *))
	   cmpdoc);

    /* write the output header */

    printf ("\\input texinfo  @c -*-texinfo-*-\n");
    printf ("@setfilename ../info/summary\n");
    printf ("@settitle Command Summary for GNU Emacs\n");
    printf ("@unnumbered Command Summary for GNU Emacs\n");
    printf ("@table @asis\n");

    /* print each function from the array */

    for (i = 0; i < cnt; i++)
      {
	printf ("\n@item %s @code{%s}\n@display\n",
		array[i]->type == 'F' ? "Function" : "Variable",
		array[i]->name);

	for (lp = array[i]->first; lp != NULL ; lp = lp->next)
	  {
	    for (bp = lp->line; *bp; bp++)
	      {
		/* the characters "@{}" need special treatment */
		if (*bp == '@' || *bp == '{' || *bp == '}')
		  {
		    putchar('@');
		  }
		putchar(*bp);
	      }
	    putchar ('\n');
	  }
	printf("@end display\n");
      }

    printf ("@end table\n");
    printf ("@bye\n");
  }

  return 0;
}
