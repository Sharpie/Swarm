// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/SwarmObject.h>
#import <objectbase.h>

@interface ProbeDisplay: SwarmObject
{
  id probedObject;
  id <ProbeMap> probeMap;

  const char *windowGeometryRecordName;
}

- setWindowGeometryRecordName: (const char *)theName;
- setProbedObject: anObject;
- getProbedObject;
- setProbeMap: probeMap;
- getProbeMap;
- createEnd;
@end
