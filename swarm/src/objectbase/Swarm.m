// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <swarmobject/Swarm.h>
#import <swarmobject.h>

// Swarm is the base class for users to build their own Swarms. Most
// of the real work here is scheduling, that is done in activity. The
// "Swarm" class here adds some useful object functionality: a couple
// of standard methods for building a swarm, and an example of activateIn.

@implementation Swarm

// Override this to let your Swarm create the objects that it contains.
-buildObjects {
  return self;
}

// Override this to let your Swarm build its actions.
-buildActions {
  return self;
}

// Override this to activate any actions you built in buildActions.
// Note, you must activate yourself first before you can activate actions
// inside you. Example subclass method:
//   [super activateIn: swarmContext];
//   [myFancySchedule activateIn: self];
//   return [self getSwarmActivity];
-activateIn: (id) swarmContext {
  [super activateIn: swarmContext];
  return [self getSwarmActivity];
}


// These methods are needed to support probing of Swarms. Normally they
// comes from SwarmObject, but Swarm is not a subclass of SwarmObject.
// Multiple inheritance by cut and paste :-)

-getProbeMap {
  return [probeLibrary getProbeMapFor: [self class]] ;
}

-getCompleteProbeMap {
  return [probeLibrary getCompleteProbeMapFor: [self class]] ;
}

-getProbeForVariable: (char *) aVariable {
  return [probeLibrary getProbeForVariable: aVariable inClass: [self class]];
}

@end
