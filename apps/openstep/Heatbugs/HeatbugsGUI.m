//
//  HeatbugsGUI.m
//  Heatbugs
//
//  Created by Scott Christley on 4/17/09.
//  Copyright __MyCompanyName__ 2009. All rights reserved.
//

#import "HeatbugsGUI.h"

@implementation HeatbugsGUI

+ createBegin:  aZone 
{
	HeatbugsGUI *obj;
	
	// Superclass createBegin to allocate ourselves.
	obj = [super createBegin: aZone];

	// frequency of display update
	//NSString *s = [obj->simParameters objectForKey: @"displayFrequency"];
	//obj->displayFrequency = [s intValue];
	obj->displayFrequency = 1;
	
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
  mainModel = [HeatbugsModel create: self];

  // Now, let the model swarm build its objects.
  [mainModel buildObjects];

	heatDisplay = 
    [Value2dDisplay createBegin: [self getZone]];
	[heatDisplay setDiscrete2dToDisplay: [mainModel getHeat]];
	[heatDisplay setDisplayMappingM: 32768 C: 0];   // turn [0,32768) -> [0,64)
	heatDisplay = [heatDisplay createEnd];
	[heatDisplay updateDisplay];
	
	heatbugDisplay = 
    [Object2dDisplay createBegin: [self getZone]];
	[heatbugDisplay setDiscrete2dToDisplay: [mainModel getWorld]];
	heatbugDisplay = [heatbugDisplay createEnd];
	//[heatbugDisplay setObjectCollection: [mainModel getHeatbugList]];
	[heatbugDisplay updateDisplay];

	
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

#if 0
      [displayActions createActionTo: heatDisplay
                      message: M(update)];
      [displayActions createActionTo: heatbugDisplay
							 message: M(update)];
      [displayActions createActionTo: self
							 message: M(swarmHasUpdatedNotification)];
#endif

      [displayActions createActionTo: self
                             message: M(updateGraphicalDisplays)];
      
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

#if 0
- (void) updateWorldDisplay
{
  printf("updateWorldDisplay\n");

	[heatDisplay updateDisplay];
	[heatbugDisplay updateDisplay];
	[self swarmHasUpdatedNotification];
	//[self viewNeedsDisplay: [_delegate heatbugSpaceView]];
}
#endif

- (void) updateGraphicalDisplays
{
	//NSLog(@"updateGraphicalDisplays");
    
	// Updating the graphical views is performed in two steps:
	//
	// 1) Update the displays, such as Object2DDisplay and Value2DDisplay,
	//    with the current model state.
	//
	// 2) Inform ourselves that the updates are complete and the graphical
	//    views need re-display.  Graphical drawing can only be performed
	//    in the main thread, not the secondary thread so this special
	//    update method sends a message to our controller class (MyDocument)
	//    on the main thread.  The -swarmHasUpdated: method in MyDocument
	//    is called with
	
	// Update any displays with current model state.
	[heatDisplay updateDisplay];
	[heatbugDisplay updateDisplay];
    
	// Inform ourselves that updates are complete and graphical views
	// should be re-displayed.
	[self swarmHasUpdatedNotification];
}

- (id)heatDisplay { return heatDisplay; }
- (id)heatbugDisplay { return heatbugDisplay; }

@end
