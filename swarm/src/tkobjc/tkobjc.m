// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj.h>
#import <tkobjc/TkExtra.h>

#import "simtools_tcl.x"
#import "analysis_tcl.x"

TkExtra *globalTkInterp;

id <Error> WindowCreation, WindowUsage, MissingFiles;

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
}

