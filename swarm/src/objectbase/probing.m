// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <collections.h>
#import <objectbase/probing.h>

#import "local.h"

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

int p_compare(id a, id b){
  if(!([a compare: b]))
    return 0 ;
  else
    return -1 ;
}
