#ifndef _MISC_H
#define _MISC_H
#include <stddef.h>

#ifndef __ptr_t
typedef void * __ptr_t;
#endif

void * xmalloc (size_t size);
void * xmalloc_atomic (size_t size);
void * xcalloc (size_t nmemb, size_t size);
void * xrealloc (void *buf, size_t size);
void xfree (void *buf);

char *strdup (const char *string);
char *strsep (char **string, const char *delim);
char *strndup (const char *s, size_t n);
char *stpcpy (char *dest, const char *src);

size_t strnlen (const char *string, size_t maxlen);
#endif
