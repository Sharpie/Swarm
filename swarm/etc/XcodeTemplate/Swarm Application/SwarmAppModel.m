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

// create:withParameters: here we create the model with provided parameters.

+ create: (id)anObserver withParameters: (NSDictionary *)params
{
  ÇPROJECTNAMEÈModel *obj;

  // call our super class first
  obj = [super create: anObserver withParameters: params];

  // We can access the parameters here and save them in instance
  // variables.  We can also just access the parameters later
  // whenever they are needed like in -buildObjects.

  // obj->width = [[params objectForKey: @"width"] intValue];
  // obj->height = [[params objectForKey: @"height"] intValue];

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
  
  // Initialize random number generator and some distributions.

  // NSString *s = [simulationParameters objectForKey: @"randomSeed"];
  // RNG = [MT19937gen create: self setStateFromSeed: [s intValue]];
  // randomDouble = [UniformDoubleDist create: self setGenerator: RNG
  //				    setDoubleMin: 0.0 setMax: 1.0];
  // randomNormal = [NormalDist create: self setGenerator: RNG];

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

// Provide access to some convenience objects
- (long)numberOfCycles { return cycle; }
- (id)randomDouble { return randomDouble; }
- (id)randomNormal { return randomNormal; }

// Default step method scheduled for the model.  Fill this method
// with behaviors for the top-level model.  By default we keep
// track of a generic time counter for the simulation.

- (void)step
{
  ++cycle;
}

@end
