#ifdef __GNUC__
#include <stdlib.h>
#include <stddef.h>

extern void *	memchr(CONST void *, int, size_t);
extern int 	memcmp(CONST void *, CONST void *, size_t);
extern void *	memcpy(void *, CONST void *, size_t);
/* extern void * memmove(void *, CONST void *, size_t);*/
extern void *	memset(void *, int, int);
extern char *	strcat(char *, CONST char *);
extern char *	strchr(CONST char *, int);
extern int	strcmp(CONST char *, CONST char *);

#include <stdio.h> /* else can't declare FILE */

extern FILE     *fopen(CONST char *, CONST char *);
extern FILE     *freopen(CONST char *, CONST char *, FILE *);
extern FILE     *tmpfile(void);
extern int      fclose(FILE *);
extern char     *fgets(char *, int, FILE *);
extern int      fgetc(FILE *);
extern int      fflush(FILE *);
extern int      fprintf(FILE *, CONST char *, ...);
extern int      fputc(char, FILE *);
extern int      fputs(CONST char *, FILE *);
extern size_t   fread(void *, size_t, size_t, FILE *);
extern int      fscanf(FILE *, CONST char *, ...);
extern int	fgetpos(FILE *, long *);
extern int      fseek(FILE *, long, int);
extern int	fsetpos(FILE *, CONST long *);
extern long     ftell(FILE *);
extern size_t   fwrite(CONST void *, size_t, size_t, FILE *);
extern char	*gets(char *);
extern void     perror(CONST char *);
extern int      printf(CONST char *, ...);
extern int      puts(CONST char *);
extern int      remove(CONST char *);
extern int      rename(CONST char *, CONST char *);
extern int      rewind(FILE *);
extern int	scanf(CONST char *, ...);
extern int	sscanf(CONST char *, CONST char *, ...);
extern void 	setbuf(FILE *, char *);
extern int 	setvbuf(FILE *, char *, int, size_t);
extern int	ungetc(int, FILE *);
extern int	vprintf(CONST char *, void *);
extern int	vfprintf(FILE *, CONST char *, void *);
extern char	*vsprintf(char *, CONST char *, void *);


struct stat;
struct timeval;
struct timezone;


extern int _filbuf ();
extern int _flsbuf ();
extern int gethostname (char *, int);
extern int sigblock (int);
#ifndef sigmask
extern int sigmask (int);
#endif
extern int sigsetmask (int);
extern int sigpause (int);
extern void tzset (void);
extern int ioctl (int, int, ...);
extern int killpg (int, int);
/* extern int setpgrp (int, int); */
extern int fsync (int);
extern int lstat (const char *, struct stat *);
extern int fchmod (int, mode_t);
extern char *mktemp (char *);
/* extern int creat (const char *, mode_t); better no decl than a conflicting one... */
extern int utimes (const char *, struct timeval *);
extern int symlink (const char *, const char *);
/* extern int readlink (const char *, char *, int); */
extern void sync (void);
extern time_t time (time_t *);
extern int gettimeofday (struct timeval *, struct timezone *);
struct nlist;
extern int nlist (const char *, struct nlist *);
/* extern int lseek (int, long, int); better no decl than a conflicting one... */
extern int munmap (void *, int);
extern int brk (void *);
extern void *sbrk (int);
extern int mprotect (void *, int, int);
extern int socket (int, int, int);
struct sockaddr;
extern int connect (int, struct sockaddr *, int);
struct rusage;
extern int wait3 (void *, int, struct rusage *);
extern int nice (int);
struct rlimit;
extern int getrlimit (int, struct rlimit *);
extern int getpagesize (void);
#endif /* __GNUC__ */
