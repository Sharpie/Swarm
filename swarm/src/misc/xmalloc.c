#include <misc.h>

void *GC_malloc_uncollectable (size_t) __attribute__ ((weak, alias ("malloc")));
void *GC_malloc_atomic_uncollectable (size_t) __attribute__ ((weak, alias ("malloc")));
void *GC_realloc (void *buf, size_t size) __attribute__ ((weak, alias ("realloc")));
void GC_free (void *buf) __attribute__ ((weak, alias ("free")));


void *
xmalloc (size_t size)
{
  void *ptr;

  ptr = GC_malloc_uncollectable (size);
  if (ptr == NULL)
    abort ();
  return ptr;
}

void *
xmalloc_atomic (size_t size)
{
  void *ptr;

  ptr = GC_malloc_atomic_uncollectable (size);
  if (ptr == NULL) 
    abort ();
  return ptr;
}

void *
xcalloc (size_t nmemb, size_t size)
{
  void *ptr;

  ptr = GC_malloc_uncollectable (nmemb * size);
  if (ptr == NULL)
    abort ();
  memset (ptr, 0, nmemb * size);
  return ptr;
}

void *
xrealloc (void *buf, size_t size)
{
  void *ptr;

  ptr = GC_realloc (buf, size);
  if (ptr == NULL)
    abort ();
  return ptr;
}

void
xfree (void *buf)
{
#ifndef DISABLE_FREE
    GC_free (buf);
#endif
}

