#include <stdlib.h>
#include "misc.h"

#ifdef USE_GC
#include <gc.h>
#endif

void *
xmalloc (size_t size)
{
  void *ptr;
#ifndef USE_GC
  ptr = malloc (size);
#else
  ptr = GC_MALLOC (size);
#endif
  if (ptr == NULL) abort ();
  return ptr;
}

void *
xmalloc_atomic (size_t size)
{
  void *ptr;
#ifndef USE_GC
  ptr = malloc(size);
#else
  ptr = GC_MALLOC_ATOMIC (size);
#endif
  if (ptr == NULL) abort ();
  return ptr;
}

void *
xcalloc (size_t nmemb, size_t size)
{
  void *ptr;
#ifndef USE_GC
  ptr = calloc (nmemb, size);
#else
  ptr = GC_MALLOC (nmemb * size);
#endif
  if (ptr == NULL) abort ();
  return ptr;
}

void *
xrealloc (void *buf, size_t size)
{
  void *ptr;
#ifndef USE_GC
  ptr = realloc (buf, size);
#else
  ptr = GC_REALLOC (buf, size);
#endif
  if (ptr == NULL) abort ();
  return ptr;
}

void
xfree (void *buf)
{
#ifndef DISABLE_FREE
#ifndef USE_GC
  free (buf);
#else
  GC_FREE (buf);
#endif
#endif
}
