#include "misc.h"

const char *
strchr (const char *s, int c)
{
  int i;

  while (*s)
    {
      if (*s == c)
        return s;
      s++;
    }
  return NULL;
}
