#include <misc.h>

char *
dropdir (char *path)
{
  unsigned start_len = strlen (path), len;

  if (start_len > 0)
    if (path[start_len - 1] == '/')
      start_len--;

  for (len = start_len; len > 0; len--)
    {
      char *ptr = &path[len - 1];

      if (*ptr == '/' && start_len > 1)
        {
          ptr++;
          *ptr = '\0';
          return path;
        }
    }
  return NULL;
}
