// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj.h>
#import <tkobjc/TkExtra.h>

extern TkExtra *globalTkInterp;

extern id <Error>
  WindowCreation,		// error while creating a window
  WindowUsage;                  // error while a window was being used

void initTkObjc(int argc, char ** argv);

