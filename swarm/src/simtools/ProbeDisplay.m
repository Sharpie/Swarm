// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtools/ProbeDisplay.h>
#import <simtools/SimpleProbeDisplay.h>
#import <simtools/CompleteProbeDisplay.h>

// SAFEPROBES enables lots of error checking here.
#define SAFEPROBES 1

@implementation ProbeDisplay

-setProbedObject: (id) anObject {
  if (SAFEPROBES) {
    if (probedObject != 0) {
      [InvalidCombination raiseEvent:
        "It is an error to reset the object when building a ProbeDisplay\n"];
      return nil;
    }
  }
  probedObject = anObject;
  return self;
}

-getProbedObject {
  return probedObject;
}

-setProbeMap: (ProbeMap *) p {
  if (SAFEPROBES) {
    if (probeMap != 0) {
      [InvalidCombination raiseEvent:
  "It is an error to reset the probe map when building a ProbeDisplay\n"];
      return nil;
    }
  }
  probeMap = p;
  return self;
}

-getProbeMap {
  return probeMap;
}

-createEnd {
  id probeDisplay ;
	
  if (SAFEPROBES) {
    if (probedObject == 0) {
      [InvalidCombination raiseEvent:
        "ProbeDisplay object was not properly initialized\n"];
      return nil;
    }
  }

  if(probeMap == nil){
    probeDisplay = [CompleteProbeDisplay createBegin: [self getZone]] ;
    [probeDisplay setProbedObject: probedObject] ;
    probeDisplay = [probeDisplay createEnd] ;
  } else {
    probeDisplay = [SimpleProbeDisplay createBegin: [self getZone]] ;
    [probeDisplay setProbedObject: probedObject] ;
    [probeDisplay setProbeMap: probeMap] ;
    probeDisplay = [probeDisplay createEnd] ;
  }

  [self drop] ;
  return probeDisplay;
}

@end


