#ifndef _MISC_H
#define _MISC_H
#include <stddef.h>

#ifndef __ptr_t
/* This is a macro and not a typedef so that it can be combined with const. */
#define __ptr_t void *
#endif

void * xmalloc (size_t size);
void * xmalloc_atomic (size_t size);
void * xcalloc (size_t nmemb, size_t size);
void * xrealloc (void *buf, size_t size);
void xfree (void *buf);

char *strdup (const char *string);
char *strndup (const char *s, size_t n);
char *stpcpy (char *dest, const char *src);

char *strpbrk (const char *s, const char *accept);
char *strsep (char **string, const char *delim);
#define strchr swarm_strchr
const char *strchr (const char *string, int c);

size_t strlen (const char *);
size_t strnlen (const char *string, size_t maxlen);
int strcmp (const char *p1, const char *p2);
int strncmp (const char *p1, const char *p2, size_t n);

void *memset (void *dstpp, int c, size_t len);
void *memcpy (void *, const void *, size_t);
void *memchr (const void *s, int c, size_t n);

const char *myrealpath (const char *path, char *resolved_path);
const char *find_executable (const char *program_name);

#endif
