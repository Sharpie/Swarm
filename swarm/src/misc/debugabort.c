#include <misc.h>

#undef abort

void
debugabort (const char *filename, unsigned lineno, const char *function)
{
  fprintf (stderr, "%s:%u %s\n", filename, lineno, function);
  abort ();
}
