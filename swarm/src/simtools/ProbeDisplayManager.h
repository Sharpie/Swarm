// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// List of display probes that are in the system, used to update
// them in the schedule.

#import <swarmobject/SwarmObject.h>

@interface ProbeDisplayManager : SwarmObject
{
  id probeList;
  BOOL dropImmediatelyFlag;
}

- (void)setDropImmediatelyFlag: (BOOL)dropImmediatelyFlag;
- (BOOL)getDropImmediatelyFlag;

- createEnd;

- createProbeDisplayFor             : anObject;

- createProbeDisplayFor             : anObject
         setWindowGeometryRecordName: (const char *)windowGeometryRecordName;


- createDefaultProbeDisplayFor      : anObject;

- createDefaultProbeDisplayFor      : anObject
         setWindowGeometryRecordName: (const char *)windowGeometryRecordName;

- createCompleteProbeDisplayFor     : anObject;

- createCompleteProbeDisplayFor     : anObject
         setWindowGeometryRecordName: (const char *)windowGeometryRecordName;

- addProbeDisplay: probeDisplay;
- dropProbeDisplaysFor: anObject;
- removeProbeDisplay: probeDisplay;
- update;
@end
