// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

#import <objectbase/Swarm.h>
#import <objectbase.h>
#import <defobj/directory.h>

@implementation Swarm
PHASE(Creating)

PHASE(Using)

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
- (id <Activity>)activateIn:  swarmContext
{
  [super activateIn: swarmContext];
  return [self getSwarmActivity];
}


// These methods are needed to support probing of Swarms. Normally they
// comes from SwarmObject, but Swarm is not a subclass of SwarmObject.
// Multiple inheritance by cut and paste :-)

- getProbeMap
{
  return [probeLibrary getProbeMapForObject: self];
}

- getCompleteProbeMap
{
  return [probeLibrary getCompleteProbeMapForObject: self];
}

- getProbeForVariable: (const char *)aVariable
{
  return [probeLibrary getProbeForVariable: aVariable inObject: self];
}

@end
