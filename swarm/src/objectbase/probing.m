// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <collections.h>
#import <swarmobject/probing.h>
#import "probing.sym"

id probeLibrary;

void initProbing() {
  static BOOL  already_initialized = 0;

  if ( already_initialized ) return;
  already_initialized = 1;

  defsymbol( DefaultString );
  defsymbol( CharString );
  defsymbol( IntString );  

  probeLibrary = [ProbeLibrary create: globalZone];
}
