// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj.h>

#import "internal.h"
#import <gui.h>
#import <tkobjc/global.h>

id <TkExtra> globalTkInterp;

#import "simtools_tcl.x"
#import "analysis_tcl.x"

id <Error> WindowCreation, WindowUsage, MissingFiles;

void
initTkObjc (int argc, const char **argv)
{
  int i;

  for (i = 1; i < argc; i++)
    {
      if (!strcmp(argv[i], "-batchmode"))
        {
          tkobjc_initTclInterp (1, argv);
          return;
        }
    }

  deferror (WindowCreation, NULL);
  deferror (WindowUsage, NULL);
 
  tkobjc_initTkInterp (1, argv);

  [globalTkInterp eval: simtools_tcl];
  [globalTkInterp eval: analysis_tcl];
}

