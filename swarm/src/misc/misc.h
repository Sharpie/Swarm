#ifndef _MISC_H
#define _MISC_H

#include <stddef.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <sys/time.h>
#ifdef __CYGWIN32__
int gettimeofday (struct timeval *p, struct timezone *z);
#endif

#include "config.h"

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

#ifndef HAVE_STRDUP
char *strdup (const char *string);
#endif

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

const char *find_executable (const char *program_name);

#endif
