// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj.h>
#import <tkobjc/TkExtra.h>
#import <tkobjc/control.h>

#import "simtools_tcl.x"
#import "analysis_tcl.x"

TkExtra *globalTkInterp;

id <Error> WindowCreation, WindowUsage;

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
          tkobjc_registerCommand (globalTkInterp, "globalTkInterp");          
          return;
        }
    }
  
  deferror(WindowCreation, NULL);
  deferror(WindowUsage, NULL);
 
  globalTkInterp = [TkExtra alloc];
  [globalTkInterp initWithArgc: 1 argv: argv];
  [globalTkInterp registerObject: globalTkInterp withName: "globalTkInterp"];

  [globalTkInterp eval: simtools_tcl];
  [globalTkInterp eval: analysis_tcl];

  // (nelson) I think this is ok: lets us load cool graph code.
  // presumably, $blt_library is always set right.
  [globalTkInterp eval: "lappend auto_path $blt_library"];
}
