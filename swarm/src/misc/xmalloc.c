#include <misc.h>

// weak attribute only for ELF targets not Mach-O.
#ifdef __MACH__
void *GC_malloc_atomic_uncollectable (size_t);
void *GC_malloc_uncollectable (size_t);
void *GC_realloc (void *buf, size_t size);
void GC_free (void *buf);
#else
void *GC_malloc_atomic_uncollectable (size_t); __attribute__ ((weak));
void *GC_malloc_uncollectable (size_t); __attribute__ ((weak));
void *GC_realloc (void *buf, size_t size); __attribute__ ((weak));
void GC_free (void *buf); __attribute__ ((weak));
#endif

void *
GC_malloc_uncollectable (size_t size)
{
  return malloc (size);
}

void *
GC_malloc_atomic_uncollectable (size_t size)
{
  return malloc (size);
}

void *
GC_realloc (void *ptr, size_t size)
{
  return realloc (ptr, size);
}

void
GC_free (void *ptr)
{
  free (ptr);
}

#if 0
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

#else

void *
xmalloc (size_t size)
{
  void *ptr;

  ptr = malloc (size);
  if (ptr == NULL) 
    abort ();
  return ptr;
}

void *
xmalloc_atomic (size_t size)
{
  void *ptr;

  ptr = malloc (size);
  if (ptr == NULL) 
    abort ();
  return ptr;
}

void *
xcalloc (size_t nmemb, size_t size)
{
  void *ptr;

  ptr = calloc (nmemb, size);
  if (ptr == NULL)
    abort ();
  return ptr;
}

void *
xrealloc (void *buf, size_t size)
{
  void *ptr;

  ptr = realloc (buf, size);
  if (ptr == NULL)
    abort ();
  return ptr;
}

void
xfree (void *buf)
{
#ifndef DISABLE_FREE
    free (buf);
#endif
}
#endif
