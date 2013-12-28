/*
 * Portable implementation of realpath() written by Chris Myers
 * <chris@wugate.wustl.edu>.
 *
 * Copyright (c) 1990, 1991 Washington University in Saint Louis, MO
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by Washington University,
 *	Saint Louis, Missouri and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1990, 1991 Washington University in St. Louis, MO.\n\
 All rights reserved.\n";
#endif /* not lint */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <strings.h>

char *
realpath(pathname, result)
char	*pathname, *result;

{
struct	stat	sbuf;
char	curpath[MAXPATHLEN],
	workpath[MAXPATHLEN],
	linkpath[MAXPATHLEN],
        namebuf[MAXPATHLEN],
	*where,
	*ptr,
	*last;
int	len;

   strcpy(curpath, pathname);

   if (*pathname != '/') {
      if (!getwd(workpath)) {
         strcpy(result, ".");
         return(NULL);
      }
   } else *workpath = NULL;

   /* curpath is the path we're still resolving      */
   /* linkpath is the path a symbolic link points to */
   /* workpath is the path we've resolved            */

loop:
   where = curpath;
   while (*where != NULL) {
      if (!strcmp(where, ".")) {
         where++;
         continue;
      }

      /* deal with "./" */
      if (!strncmp(where, "./", 2)) {
         where += 2;
         continue;
      }

      /* deal with "../" */
      if (!strncmp(where, "../", 3)) {
         where += 3;
         ptr = last = workpath;
         while (*ptr) {
            if (*ptr == '/') last = ptr;
            ptr++;
         }
         *last = NULL;
         continue;
      }

      ptr = strchr(where, '/');
      if (!ptr)
         ptr = where + strlen(where) - 1;
      else
         *ptr = NULL;

      strcpy(namebuf, workpath);
      for (last = namebuf; *last; last++) continue;
      if (*--last != '/') strcat(namebuf, "/");
      strcat(namebuf, where);

      where = ++ptr;
      if (lstat(namebuf, &sbuf) == -1) {
         strcpy(result, namebuf);
         return(NULL);
      }

      if ((sbuf.st_mode & S_IFLNK) == S_IFLNK) {
         len = readlink(namebuf, linkpath, MAXPATHLEN);
         if (len == 0) {
            strcpy(result, namebuf);
            return(NULL);
         }
         *(linkpath + len) = NULL; /* readlink doesn't null-terminate result */
         if (*linkpath == '/') *workpath = NULL;
         if (*where) {
            strcat(linkpath, "/");
            strcat(linkpath, where);
         }
         strcpy(curpath, linkpath);
         goto loop;
      }

      if ((sbuf.st_mode & S_IFDIR) == S_IFDIR) {
         strcpy(workpath, namebuf);
         continue;
      }

      if (*where) {
         strcpy(result, namebuf);
         return(NULL);  /* path/notadir/morepath */
      } else
         strcpy(workpath, namebuf);
   }
   strcpy(result, workpath);
   return(result);

}
