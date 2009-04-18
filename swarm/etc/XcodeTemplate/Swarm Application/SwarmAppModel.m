//
//  ÇPROJECTNAMEÈModel.m
//  ÇPROJECTNAMEÈ
//
//  Created by ÇFULLUSERNAMEÈ on ÇDATEÈ.
//  Copyright ÇORGANIZATIONNAMEÈ ÇYEARÈ. All rights reserved.
//

#import <Cocoa/Cocoa.h>

#import "ÇPROJECTNAMEÈModel.h"

@implementation ÇPROJECTNAMEÈModel

// createBegin: here we set up the default simulation parameters.

+ createBegin: aZone
{
	ÇPROJECTNAMEÈModel *obj;

	// First, call our superclass createBegin - the return value is the
	// allocated HeatbugModelSwarm object.

	obj = [super createBegin: aZone];

	obj->cycle = 0;

	return obj;
}

// createEnd: we could create some objects here if we knew we needed
// them. But this method is called before the user is allowed to fill
// in any customization of the model, so we defer most object creation
// to later. (In this example, this method does nothing at all and could
// just be inherited. But it's here to show you a place to customize.)

- createEnd
{
	return [super createEnd];
}

// Now it's time to build the model objects. We use various parameters
// inside ourselves to choose how to create things.

- buildObjects
{
	// allow our parent class to build anything.
	[super buildObjects];
  

	return self;
}

// Here is where the model schedule is built, the data structures
// that define the simulation of time in the mode. The core is an
// actionGroup that has a list of actions. That's then put in a Schedule.

- buildActions
{
  [super buildActions];
  
  // Create the list of simulation actions. 
  modelActions = [ActionGroup create: self];
  [modelActions createActionTo: self message: M(step)];

  // Create the schedule.
  modelSchedule = [Schedule create: self setRepeatInterval: 1];
  [modelSchedule at: 0 createAction: modelActions];

  return self;
}

// Now set up the model's activation. swarmContext indicates where
// we're being started in - typically, this model is run as a subswarm
// of an observer swarm.

- activateIn: swarmContext
{
  // First, activate ourselves via the superclass activateIn: method.
  // Just pass along the context: the activity library does the right thing.

  [super activateIn: swarmContext];

  // Now activate our own schedule.

  [modelSchedule activateIn: self];

  // Finally, return our activity.

  return [self getActivity];
}

- (void)step
{
  ++cycle;
}

@end
