/* gnushdrs.c - retrieve headers from news for GNUS - rick sladkey
   Copyright (C) 1993 Free Software Foundation, Inc.

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

/*
   This program takes a directory name and a series of articles in
   that directory and generates an Emacs-Lisp expression in the format
   expected by GNUS from a retrieve-headers function.  It was inspired
   by Scott Ballantyne's getgnushdrs.c but is otherwise unrelated.  It
   doesn't require cnews libraries and the output format is different
   and much faster for Emacs to parse.

   The output looks like:

   ([article subject from xref lines data message-id references] ... )

   i.e. a list of vectors of article header fields.  The article and
   and lines fields are numbers.  The others are quoted strings unless
   that field is missing and then they are nil.
*/

#include <stdio.h>
#include <fcntl.h>

#define HDR_MAXCHARS	1024
#define HDR_MAXLINES	128

char c[HDR_MAXCHARS + 3];
int csize;
int l[HDR_MAXLINES];
int lsize;

int main(argc, argv)
int argc;
char **argv;
{
	int art;
	char *file;
	int fd;
	int i;
	int cc;
	int inheader;

	argv++; argc--;
	if (argc == 0)
		exit(1);
	if (chdir(argv[0]) < 0)
		exit(1);
	argv++; argc--;
	printf("(\n");
	for (art = 0; art < argc; art++) {
		file = argv[art];
		fd = open(file, O_RDONLY, 0);
		if (fd < 0)
			continue;
		csize = read(fd, c, HDR_MAXCHARS);
		close(fd);
		if (csize <= 0)
			continue;
		c[csize] = c[csize + 1] = c[csize + 2] = '\n';
		csize += 2;

		lsize = 0;
		l[lsize++] = 0;
		inheader = 1;
		for (i = 0; i < csize; i++) {
			if ((cc = c[i]) == '\n') {
				l[lsize++] = i + 1;
				if ((cc = c[i + 1]) == '\n')
					break;
				if (cc != ' ' && cc != '\t')
					inheader = 1;
			}
			else if (cc == ':')
				inheader = 0;
			else if (inheader && cc >= 'A' && cc <= 'Z')
				c[i] += 'a' - 'A';
		}
		
		if (!dump_field("message-id", (char *) 0, 0, 1))
			continue;

		printf("[%s", file);
		dump_field("subject", "\"(None)\"", 0, 0);
		dump_field("from", "\"(Unknown User)\"", 0, 0);
		dump_field("xref", "nil", 0, 0);
		dump_field("lines", "0", 1, 0);
		dump_field("date", "nil", 0, 0);
		dump_field("message-id", "nil", 0, 0);
		if (!dump_field("references", (char *) 0, 0, 0))
			dump_field("in-reply-to", "nil", 0, 0);
		printf("]\n");
	}
	printf(")\n");
}

int dump_field(field, def, numeric, verify)
char *field;
char *def;
int numeric;
int verify;
{
	int i, j;
	int n, m;
	char *p;
	char cc;

	n = strlen(field);
	for (i = 0; i < lsize; i++) {
		p = &c[l[i]];
		if (*p == *field && strncmp(p, field, n) == 0 && p[n] == ':') {
			if (verify)
				return 1;
			p += n + 1;
			if (numeric) {
				printf(" %d", atoi(p));
				return 1;
			}
			putchar(' ');
			putchar('"');
			for (j = 0; *p == ' ' || *p == '\t'; j++) {
				while (*p == ' ' || *p == '\t')
					p++;
				if (j)
					putchar(' ');
				for (cc = *p; cc != '\n'; cc = *++p) {
					if (cc == '\\' || cc == '"')
						putchar('\\');
					putchar(cc);
				}
				p++;
			}
			putchar('"');
			return 1;
		}
	}
	if (verify)
		return 0;
	if (def) {
		printf(" %s", def);
		return 1;
	}
	return 0;
}

