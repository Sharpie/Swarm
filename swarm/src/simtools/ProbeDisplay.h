// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <swarmobject.h>

@interface ProbeDisplay : SwarmObject {
  id probedObject;
  ProbeMap * probeMap;
}

-setProbedObject: anObject;
-getProbedObject ;
-setProbeMap: (ProbeMap *) probeMap;
-getProbeMap ;
-createEnd;

@end
