// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj.h>
#import <tkobjc/TkExtra.h>

#import "simtools_tcl.x"
#import "analysis_tcl.x"

#include <unistd.h>
#include <string.h>

TkExtra *globalTkInterp;

id <Error> WindowCreation, WindowUsage, MissingFiles;

static void
ensureBltSupportFiles (void)
{
  const char *fileName = "bltGraph.tcl";
  const char *basePath = [globalTkInterp globalVariableValue: "blt_library"];
  char buf[strlen (basePath) + 1 + strlen (fileName) + 1];

  strcpy (buf, basePath);
  strcat (buf, "/");
  strcat (buf, fileName);

  if (access (buf, F_OK) == -1)
    {
      fprintf (stderr, "BLT support file `%s' not found\n", fileName);
      fprintf (stderr, "If the directory `%s' not the intended location for "
               "the BLT support files,\nplease adjust BLT_LIBRARY to the "
               "right place.\n", basePath);
      exit (1);
    }
}

static void
registerInterp (void)
{
  [globalTkInterp registerObject: globalTkInterp withName: "globalTkInterp"];
}

void
initTkObjc (int argc, char ** argv)
{
  int i;

  for (i = 1; i < argc; i++)
    {
      if (!strcmp(argv[i], "-batchmode"))
        {
          globalTkInterp = [Tcl alloc];		  // misnomer
          [globalTkInterp initWithArgc: 1 argv: argv];
          registerInterp ();
          return;
        }
    }
  
  deferror (WindowCreation, NULL);
  deferror (WindowUsage, NULL);
 
  globalTkInterp = [TkExtra alloc];
  [globalTkInterp initWithArgc: 1 argv: argv];
  registerInterp ();

  [globalTkInterp eval: simtools_tcl];
  [globalTkInterp eval: analysis_tcl];

  ensureBltSupportFiles ();
    
  // (nelson) I think this is ok: lets us load cool graph code.
  // presumably, $blt_library is always set right.
  [globalTkInterp eval: "lappend auto_path $blt_library"];
}

