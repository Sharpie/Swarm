// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/SwarmObject.h>
#import <objectbase/ProbeMap.h>

@interface ProbeDisplay : SwarmObject
{
  id probedObject;
  ProbeMap * probeMap;

  const char *windowGeometryRecordName;
}

-setWindowGeometryRecordName : (const char *)theName;
-setProbedObject: anObject;
-getProbedObject ;
-setProbeMap: (ProbeMap *) probeMap;
-getProbeMap ;
-createEnd;
@end
