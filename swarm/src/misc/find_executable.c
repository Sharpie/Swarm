#include "misc.h"

#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <stdlib.h>
#include <stdio.h>

#define FALSE 0
#define TRUE 1

static int
maybe_executable (const char *filename)
{ 
  if (access (filename, R_OK|X_OK) < 0)
    return FALSE;
  /* Should check that inodes match up, but only know how 
     do that on Linux. */
  return TRUE;
}

const char *
find_executable (const char *program_name)
{ 
  char *executable_name;

#ifdef __linux__
  /* The executable is accessible as /proc/<pid>/exe. We try this
     first because it is safer: no race condition w.r.t. the file
     system. It may fail, however, if the user has not compiled /proc
     support into his kernel. */
  {
    char buf[6+10+5];
    int fd;

    sprintf (buf, "/proc/%d/exe", getpid ());
    fd = open (buf, O_RDONLY, 0644);
    if (fd != -1)
      return buf;
  }
#endif
  /* Now we guess the executable's full path. We assume the executable
     has been called via execlp() or execvp() with properly set up
     argv[0].  The login(1) convention to add a '-' prefix to argv[0]
     is not supported. */
  {
    int has_slash = FALSE;

    {
      const char * p;

      for (p = program_name; *p; p++)
        {
          if (*p == '/')
            {
              has_slash = TRUE;
              break;
            }
        }
    }
    if (!has_slash)
      { 
        /* exec searches paths without slashes in the directory list given
           by $PATH. */
        const char *path = getenv ("PATH");

        if (!(path == NULL))
          { 
            const char *p;
            const char *p_next;

            for (p = path; *p; p = p_next)
              { 
                const char * q;
                unsigned int p_len;

                for (q = p; *q; q++)
                  {
                    if (*q == ':')
                      break;
                  }
                p_len = q - p;
                p_next = (*q == '\0' ? q : q + 1);
                /* We have a path item at p, of length p_len.
                   Now concatenate the path item and program_name. */
                {
                  char *concat_name = (char *)malloc (p_len + strlen (program_name) + 2);

                  if (concat_name == NULL)
                    {
                      errno = ENOMEM;
                      goto notfound;
                    }
                  if (p_len == 0)
                    strcpy (concat_name, program_name);
                  else
                    {
                      memcpy (concat_name, p, p_len);
                      sprintf (concat_name + p_len, "/%s", program_name);
                    }
                  if (maybe_executable (concat_name))
                    /* Assume we have found the executable */
                    {
                      program_name = concat_name;
                      goto resolve;
                    }
                  free (concat_name);
                }
              }
          }
        /* Not found in the PATH, assume the current directory. */
      }

    /* exec treats paths containing slashes as relative to the current
       directory. */
    if (maybe_executable (program_name))
      resolve:
    /* resolve program_name */
    {
      executable_name = (char *)malloc (MAXPATHLEN);
      if (executable_name == NULL)
        {
          errno = ENOMEM;
          goto notfound;
        }
      if (myrealpath (program_name, executable_name) == NULL)
        {
          free (executable_name);
          goto notfound;
        }
      return executable_name;
    }
    errno = ENOENT;
  notfound:
    return NULL;
  }
}
