// Swarm library. Copyright © 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#ifndef _MISC_H
#define _MISC_H

#include <swarmconfig.h>

#include <stddef.h>
#if defined(__hpux__) && defined(HAVE_SYS_SIGEVENT_H)
#include <sys/sigevent.h>
#endif
#include <stdio.h>
#include <stdarg.h>
#include <string.h>

#include <unistd.h>
#include <time.h>
#include <sys/time.h>
#include <errno.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <math.h>

#include <limits.h>

#define MAX_ALIGNMENT __alignof__(double)

#ifdef __jmalloc_h
#error
#endif

#ifndef HAVE_REALPATH
/* Conflicts can occur when Checker fails to find stub, but it library
   routine does exist. */
#define realpath stdlib_realpath
#include <stdlib.h>
#undef realpath
#else
#include <stdlib.h>
#endif

#ifndef __ptr_t
/* This is a macro and not a typedef so that it can be combined with const. */
#define __ptr_t void *
#endif

/* Expected to be declared by stdlib.h are atoi, getenv, qsort. */
/* Expected to be declared by unistd.h are access, getpid, and sleep. */
/* Expected to be declared by time.h are clock, time, and gettimeofday. */
/* Expected to be declared by sys/time.h. is timeval. */

void *xmalloc (size_t size);
void *xmalloc_atomic (size_t size);
void *xcalloc (size_t nmemb, size_t size);
void *xrealloc (void *buf, size_t size);
void xfree (void *buf);
#define XFREE(buf) xfree((void *)(buf))

char *dropdir (char *path);

char *xstrdup (const char *string);

#ifndef HAVE_STRNDUP
char *strndup (const char *s, size_t n);
#endif

#ifndef HAVE_STPCPY
char *stpcpy (char *dest, const char *src);
#endif

#ifndef HAVE_STRPBRK
char *strpbrk (const char *s, const char *accept);
#endif

#ifndef HAVE_STRSEP
char *strsep (char **string, const char *delim);
#endif

#ifndef HAVE_STRCHR
const char *strchr (const char *string, int c);
#endif

#ifndef HAVE_STRLEN
size_t strlen (const char *);
#endif

#ifndef HAVE_STRNLEN
size_t strnlen (const char *string, size_t maxlen);
#endif

#ifndef HAVE_STRCMP
int strcmp (const char *p1, const char *p2);
#endif

#ifndef HAVE_STRNCMP
int strncmp (const char *p1, const char *p2, size_t n);
#endif

#ifndef HAVE_MEMSET
void *memset (void *dstpp, int c, size_t len);
#endif

#ifndef HAVE_MEMCPY
void *memcpy (void *, const void *, size_t);
#endif

#ifndef HAVE_MEMCHR
void *memchr (const void *s, int c, size_t n);
#endif

#ifndef HAVE_REALPATH
const char *realpath (const char *path, char *resolved_path);
#endif

#ifndef HAVE_STRTOLL
long long strtoll (const char *nptr, char **endptr, int base);
#endif

#ifndef LLONG_MIN
#ifdef LONG_LONG_MIN
#define LLONG_MIN LONG_LONG_MIN
#elif SIZEOF_LONG == SIZEOF_LONG_LONG
#define LLONG_MIN LONG_MIN
#else
#error long long min value missing
#endif
#endif

#ifndef LLONG_MAX
#ifdef LONG_LONG_MAX
#define LLONG_MAX LONG_LONG_MAX
#elif SIZEOF_LONG == SIZEOF_LONG_LONG
#define LLONG_MAX LONG_MAX
#else
#error long long max value missing
#endif
#endif

const char *find_executable (const char *program_name);

void debugabort (const char *filename, unsigned lineno, const char *function) __attribute__ ((noreturn));

#define abort() debugabort(__FILE__,__LINE__, __PRETTY_FUNCTION__)

#define isDigit(ch) ((ch) >= '0' && (ch) <= '9')
#define isSpace(ch) ((ch) == ' ' || (ch) == '\t' || (ch) == '\n' || (ch) == '\r' || (ch) == '\f')
#define isUpper(ch) ((ch) >= 'A' && (ch) <= 'Z')
#define isLower(ch) ((ch) >= 'a' && (ch) <= 'z')
#define isAlpha(ch) (isUpper(ch) || isLower(ch))
#define isAlnum(ch) (isAlpha(ch) || isDigit(ch))
#define isPunct(ch) ((((ch) > ' ') && ((ch) < '0')) || (((ch) > '9') && ((ch) < 'A')) || (((ch) > 'Z') && ((ch) < 'a')) || (((ch) > 'z') && ((ch) <= '~')))
#define isPrint(ch) (isPunct(ch) || isAlnum(ch) || ((ch) == ' '))

#endif

