// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// List of display probes that are in the system, used to update
// them in the schedule.

#import <swarmobject/SwarmObject.h>
@interface ProbeDisplayManager : SwarmObject {
  id probeList;
}

-createEnd;
-createProbeDisplayFor: (id) anObject;
-createCompleteProbeDisplayFor: (id) anObject;
-addProbeDisplay: pd;
-removeProbeDisplayFor: anObject ;
-removeProbeDisplay: pd;
-update;

@end
