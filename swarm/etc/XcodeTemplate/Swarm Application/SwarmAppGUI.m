//
//  ÇPROJECTNAMEÈGUI.m
//  ÇPROJECTNAMEÈ Swarm Application
//
//  Created by ÇFULLUSERNAMEÈ on ÇDATEÈ.
//  Copyright ÇORGANIZATIONNAMEÈ ÇYEARÈ. All rights reserved.
//

#import "ÇPROJECTNAMEÈGUI.h"

@implementation ÇPROJECTNAMEÈGUI

+ createBegin:  aZone 
{
	ÇPROJECTNAMEÈGUI *obj;
	
	// Superclass createBegin to allocate ourselves.
	obj = [super createBegin: aZone];
	
	// Load the default parameters
	NSString *paramFile = [[NSBundle mainBundle] pathForResource: @"ÇPROJECTNAMEÈ" ofType: @"swarm"];
	obj->simParameters = [[NSMutableDictionary dictionaryWithContentsOfFile: paramFile] retain];
	if (!obj->simParameters) {
		printf("Could not get parameter file\n");
		abort();
	}

	// frequency of display update
	NSString *s = [obj->simParameters objectForKey: @"displayFrequency"];
	obj->displayFrequency = [s intValue];
	
	return obj;
}

- buildObjects
{
  [super buildObjects];

#if 0
  NSString *s;
  int aRun;
  simParameters = [[NSDictionary dictionaryWithDictionary: runParameters] retain];
  s = [simParameters objectForKey: @"outputRun"];
  if (s != nil)
		aRun = [s intValue];
	else
		aRun = 0;
#endif

  // create the model
  mainModel = [ÇPROJECTNAMEÈModel create: self];

  // Now, let the model swarm build its objects.
  [mainModel buildObjects];

  // All done - we're ready to build a schedule and go.
  return self;
}  

// Create the actions necessary for the simulation. This is where
// the schedule is built (but not run!)

- buildActions 
{
  [super buildActions];
  
  // First, let our model swarm build its own schedule.

  [mainModel buildActions];

  if (displayFrequency) {
      // Create an ActionGroup for display. This is pretty minimal in this
      // case. Note, there's no doTkEvents message - no control panel!
      
      displayActions = [ActionGroup create: self];
      
      [displayActions createActionTo: self
                      message: M(updateWorldDisplay)];
      
      // the displaySchedule controls how often we write data out.
      displaySchedule = [Schedule createBegin: self];
      [displaySchedule setRepeatInterval: displayFrequency];
      displaySchedule = [displaySchedule createEnd];
      
      [displaySchedule at: 0 createAction: displayActions];
  }
  
  return self;
}  

// activateIn: - get the Swarm ready to run.
- activateIn:  swarmContext 
{
  // First, activate ourselves (just pass along the context).
  [super activateIn: swarmContext];
  
  // We need to activate the model swarm.
  [mainModel activateIn: self];
  
  // Now activate our schedules in ourselves. Note that we just activate
  // both schedules: the activity library will merge them properly.
  if (displayFrequency)
    [displaySchedule activateIn: self];
  
  // Activate returns the swarm activity - the thing that's ready to run.
  return [self getActivity];
}

// the HeatbugObserverSwarm had a go method inherited from GUISwarm,
// but we have to define our own here. It's pretty simple. There's also
// a friendly message printed out here just in case someone is confused
// when they run heatbugs and see no graphics.

- go 
{
  [[self getActivity] run];
  return [[self getActivity] getStatus];
}

- (void) updateWorldDisplay
{
  printf("updateWorldDisplay\n");
}

@end
