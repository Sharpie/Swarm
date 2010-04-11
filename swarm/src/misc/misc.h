// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

#ifndef _MISC_H
#define _MISC_H

#include <Swarm/swarmconfig.h>

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
#include <objc/objc.h> // BOOL

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
#ifdef __MINGW32__
#define BOOL WINBOOL
/* Avoid defines like Rectangle which will cause problems in tkobjc */
#define NOGDI
#include <windows.h>
#undef BOOL
#define sleep(x) (Sleep((x)*1000),0)
#endif

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
#ifdef __LONG_LONG_MAX__
#define LLONG_MIN (-__LONG_LONG_MAX__ - 1)
#else
#error long long min value missing
#endif
#endif
#endif

#ifndef LLONG_MAX
#ifdef LONG_LONG_MAX
#define LLONG_MAX LONG_LONG_MAX
#elif SIZEOF_LONG == SIZEOF_LONG_LONG
#define LLONG_MAX LONG_MAX
#else
#ifdef __LONG_LONG_MAX__
#define LLONG_MAX __LONG_LONG_MAX__
#else
#error long long max value missing
#endif
#endif
#endif

const char *find_executable (const char *program_name);

void debugabort (const char *filename, unsigned lineno, const char *function) __attribute__ ((noreturn));

typedef int (*quicksort_compar_fn_t) (const void *, const void *);

extern void quicksort (void *const pbase, size_t total_elems,
                       size_t size, quicksort_compar_fn_t cmp);

#define abort() debugabort(__FILE__,__LINE__, __PRETTY_FUNCTION__)

#undef isDigit
#undef isUpper

static inline BOOL
isDigit (char ch)
{
  return ch >= '0' && ch <= '9'; 
}

static inline BOOL
isSpace (char ch)
{
  return ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' || ch == '\f';
}

static inline BOOL
isUpper (char ch)
{
  return ch >= 'A' && ch <= 'Z';
}

static inline BOOL
isLower (char ch)
{
  return ch >= 'a' && ch <= 'z';
}

static inline BOOL
isAlpha (char ch)
{
  return isUpper (ch) || isLower (ch);
}

static inline BOOL
isAlnum (char ch)
{
  return isAlpha (ch) || isDigit (ch);
}

static inline BOOL
isPunct (char ch)
{
  return (((ch > ' ') && (ch < '0'))
          || ((ch > '9') && (ch < 'A'))
          || ((ch > 'Z') && (ch < 'a'))
          || ((ch > 'z') && (ch <= '~')));
}

static inline BOOL
isPrint (char ch) 
{ 
  return isPunct (ch) || isAlnum (ch) || ch == ' ';
}

#endif

