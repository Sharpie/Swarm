// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/Swarm.h>
#import <objectbase.h>

//S: The Swarm class encapsulates all of the abstract execution machinery in 
//S: the activity library, as well as the notion of a group of related
//S: objects. Metaphorically, a "Swarm" is a combination of a collection of 
//S: objects and a schedule of activity over those objects. Hence, the
//S: Swarm class provides the behavior necessary for creating such an object 
//S: and starting up the schedule. 
//D: Swarm is the base class for users to build their own Swarms. Most
//D: of the real work here is scheduling, that is done in activity. The
//D: "Swarm" class here adds some useful object functionality: a couple
//D: of standard methods for building a swarm, and an example of activateIn.
@implementation Swarm

// Override this to let your Swarm create the objects that it contains.
- buildObjects
{
  return self;
}

// Override this to let your Swarm build its actions.
- buildActions
{
  return self;
}

// Override this to activate any actions you built in buildActions.
// Note, you must activate yourself first before you can activate actions
// inside you. Example subclass method:
//   [super activateIn: swarmContext];
//   [myFancySchedule activateIn: self];
//   return [self getSwarmActivity];
- activateIn:  swarmContext
{
  [super activateIn: swarmContext];
  return [self getSwarmActivity];
}


// These methods are needed to support probing of Swarms. Normally they
// comes from SwarmObject, but Swarm is not a subclass of SwarmObject.
// Multiple inheritance by cut and paste :-)

- getProbeMap
{
  return [probeLibrary getProbeMapFor: [self class]] ;
}

- getCompleteProbeMap
{
  return [probeLibrary getCompleteProbeMapFor: [self class]] ;
}

- getProbeForVariable: (const char *)aVariable
{
  return [probeLibrary getProbeForVariable: aVariable inClass: [self class]];
}

@end
