#include "misc.h"

#include <sys/param.h>
#include <unistd.h>
#include <errno.h>

#ifdef __CYGWIN32__
#define PATHNAME_UNIX_UNC
#endif

#define HAVE_READLINK
#define ELOOP_VALUE ELOOP

/* Library-Funktion realpath implementieren:
   [Copyright: SUN Microsystems, B. Haible]
   TITLE
   REALPATH(3)
   SYNOPSIS
   char* realpath (const char* path, char resolved_path[MAXPATHLEN]);
   DESCRIPTION
   realpath() expands all symbolic links  and  resolves  refer-
   ences  to '/./', '/../' and extra '/' characters in the null
   terminated string named by path and stores the canonicalized
   absolute pathname in the buffer named by resolved_path.  The
   resulting path will have no symbolic links  components,  nor
   any '/./' or '/../' components.
   RETURN VALUES
   realpath() returns a pointer to the  resolved_path  on  suc-
   cess.   On  failure, it returns NULL, sets errno to indicate
   the error, and places in resolved_path the absolute pathname
   of the path component which could not be resolved. 
*/

const char *
myrealpath (const char *path, char *resolved_path)
/* Methode: benutze getwd und readlink. */
{
  char mypath[MAXPATHLEN];
  int symlinkcount = 0; /* Anzahl bisher aufgetretener symbolischer Links */
  char* resolved_limit = &resolved_path[MAXPATHLEN-1];
  /* Gültige Pointer sind die mit resolved_path <= ptr <= resolved_limit.
     In *resolved_limit darf höchstens noch ein Nullbyte stehen.
     (Analog mit mypath.) */
  char* resolve_start;
  {
    char* resolved_ptr = resolved_path; /* (bleibt stets <= resolved_limit) */
    /* evtl. Working-Directory benutzen: */
    if (!(path[0]=='/')) /* kein absoluter Pathname ? */
      { 
        if (getcwd (resolved_path, MAXPATHLEN + 1) == NULL)
          return NULL;
        resolved_ptr = resolved_path;
        while (*resolved_ptr) { resolved_ptr++; }
        if (resolved_ptr < resolved_limit)
          *resolved_ptr++ = '/';
        resolve_start = resolved_ptr;
      }
    else
      resolve_start = resolved_ptr = &resolved_path[0];
    /* Dann path selber einkopieren: */
    {
      const char* path_ptr = path;
      
      while ((resolved_ptr < resolved_limit) && *path_ptr)
        *resolved_ptr++ = *path_ptr++;
      /* Mit '/' und einem Nullbyte abschließen:*/
      if (resolved_ptr < resolved_limit)
        *resolved_ptr++ = '/';
      *resolved_ptr = 0;
    }
  }
  /* Los geht's nun in resolved_path ab resolve_start.  */
  {
    char *from_ptr = resolve_start;
    char *to_ptr = resolve_start;
    while ((to_ptr < resolved_limit) && (*from_ptr))
      /* Bis hierher hat der Pfad in  resolved_path[0]...to_ptr[-1]
         die Gestalt '/subdir1/subdir2/.../txt', 
         wobei 'txt' evtl. leer, aber kein subdir leer. */
      {
        char next = *from_ptr++; *to_ptr++ = next;

        if ((next == '/') && (to_ptr > resolved_path+1))
          /* to_ptr[-1]='/'  ->  Directory ...to_ptr[-2] auflösen: */
          {
            char* last_subdir_end = &to_ptr[-2];

            switch (*last_subdir_end)
              {
              case '/':
#ifdef PATHNAME_UNIX_UNC
                if (to_ptr > resolved_path+2)
#endif
                  /* '//' wird zu '/' vereinfacht: */
                  to_ptr--;
                break;
              case '.':
                {
                  char* last_subdir_ptr = &last_subdir_end[-1];
                  if (to_ptr > resolved_path+2)
                    { if (*last_subdir_ptr == '.')
                      {
                        if ((to_ptr > resolved_path+4) && (*--last_subdir_ptr == '/'))
                          /* letztes subdir war '/../' */
                          /* Dafür das subdir davor entfernen: */
                          {
                            while ((last_subdir_ptr > resolved_path) && !(*--last_subdir_ptr == '/'));
                            to_ptr = last_subdir_ptr+1;
                          }
                      }
                    else if (*last_subdir_ptr == '/')
                      {
                        /*  letztes subdir war '/./'
                            entfernen: */
                        to_ptr = last_subdir_end;
                      }
                    }
                }
                break;
              default:
                /* nach einem normalen subdir */
#ifdef HAVE_READLINK
                /* symbolischen Link lesen: */
                to_ptr[-1]=0; /* '/' durch 0 ersetzen */
                  {
#ifdef __CYGWIN32__
                    /* needed for Win95 only
                       readlink() doesn't work right on NFS mounted directories
                       (it returns -1,ENOENT or -1,EIO).
                       So check for a directory first. */
                    struct stat statbuf;
                    if (lstat(resolved_path,&statbuf) < 0)
                      return NULL; /* Fehler */
                    if (S_ISDIR(statbuf.st_mode))
                      /* Verzeichnis, kein symbolisches Link */
                      to_ptr[-1] = '/'; /* wieder den '/' eintragen */
                    else
#endif
                      { 
                        int linklen = readlink(resolved_path,mypath,sizeof(mypath)-1);

                        if (linklen >=0)
                          /* war ein symbolisches Link */
                          {
                            if (++symlinkcount > MAXSYMLINKS)
                              {
                                errno = ELOOP_VALUE;
                                return NULL;
                              }
                            /* noch aufzulösenden path-Anteil an den Link-Inhalt anhängen: */
                            {
                              char* mypath_ptr = &mypath[linklen]; /* ab hier ist Platz */
                              char* mypath_limit = &mypath[MAXPATHLEN-1]; /* bis hierher */
                              if (mypath_ptr < mypath_limit) { *mypath_ptr++ = '/'; } /* erst ein '/' anhängen */
                              /* dann den Rest: */
                              while ((mypath_ptr <= mypath_limit) && (*mypath_ptr = *from_ptr++))
                                mypath_ptr++;
                              *mypath_ptr = 0; /* und mit 0 abschließen */
                            }
                            /* Dies ersetzt bzw. ergänzt den path: */
                            if (mypath[0] == '/')
                              /* ersetzt den path: */
                              {
                                from_ptr = &mypath[0]; to_ptr = resolved_path;
                                while ((*to_ptr++ = *from_ptr++));
                                from_ptr = resolved_path;
                              }
                            else
                              /* ergänzt den path: */
                                { 
                                  /* Linknamen streichen. Dazu bis zum letzten '/' suchen: */
                                  {
                                    char* ptr = &to_ptr[-1];

                                    while ((ptr > resolved_path) && !(ptr[-1] == '/'))
                                      ptr--;
                                    from_ptr = ptr;
                                  }
                                  {
                                    char* mypath_ptr = &mypath[0]; to_ptr = from_ptr;
                                    while ((to_ptr <= resolved_limit) && (*to_ptr++ = *mypath_ptr++));
                                  }
                                }
                            to_ptr = from_ptr;
                          }
                        else
#if (defined(sgi) || defined(__sgi))
                          if ((errno == EINVAL) || (errno == ENXIO))
#elif defined(__MINT__)
                            if ((errno == EINVAL) || (errno == EACCESS))
#elif defined(__CYGWIN32__)
                              if ((errno == EINVAL) || (errno == EACCES))
#else
                                if (errno == EINVAL)
#endif
                                  /* kein symbolisches Link */
                                  to_ptr[-1] = '/'; /* wieder den '/' eintragen */
                                else
                                  return NULL; /* Fehler */
                      }
                  }
#endif
                break;
              }
          }
      }
    /* dann zum nächsten subdir
       ein '/' am Ende streichen: */
    if ((to_ptr[-1] == '/')
#ifdef PATHNAME_UNIX_UNC
        && (to_ptr > resolved_path+2)
#else
        && (to_ptr > resolved_path+1)
#endif
        )
      to_ptr--;
    to_ptr[0] = 0; /* durch 0 abschließen */
    return resolved_path; /* fertig */
  }
}
