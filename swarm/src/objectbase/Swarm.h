// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <activity.h>
#import <activity/SwarmProcess.h>

@interface Swarm : CSwarmProcess
{
}

// Override these methods to make useful Swarm subclasses.
- buildObjects;
- buildActions;
- activateIn: swarmContext;
// You may also want to override createBegin and createEnd.

// These methods are copied over from SwarmObject - probe support.
- getProbeMap;
- getCompleteProbeMap;
- getProbeForVariable: (const char *)aVariable;

@end
