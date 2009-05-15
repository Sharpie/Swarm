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

  // Perform any initialization required.
  // However, do not access the simulation parameters as they are not
  // available yet, do that in -createEnd.

  return obj;
}

- createEnd
{
  // frequency of display update
  NSString *s = [[self simulationParameters] objectForKey: @"displayFrequency"];
  displayFrequency = [s intValue];
  
  return [super createEnd];
}

// Create any objects for the simulation.  Specifically
// we have the top level model swarm build itself.  This
// is where we build any display objects.

- buildObjects
{
  [super buildObjects];
	
  // create the model
  mainModel = [ÇPROJECTNAMEÈModel create: self];
	
  // Now, let the model swarm build its objects.
  [mainModel buildObjects];
	
  // Finally, build any display objects such as Object2DDisplay
  // and attach them to appropriate model swarm objects.
	
  // someDisplay = [Object2dDisplay createBegin: [self getZone]];
  // [someDisplay setDiscrete2dToDisplay: ...];
  // [someDisplay createEnd];
	
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
    // case.  We just periodically call a method where we can update
    // any displays and graphical views.
		
    displayActions = [ActionGroup create: self];
		
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


// Provide access to the top-level Swarm model we are observing.
- mainModel { return mainModel; }


// We provide access to the display objects we are maintaining.
// The GUI controller (MyDocument class by default) will access
// these display objects when updating the graphical interface.

// - someDisplay { return someDisplay; }


// Update graphical views to display current model state.  This
// method gets called periodically from the Swarm scheduler as
// setup in the -buildActions method.  This is the simplest form
// with just a single method to update everything in one shot.

- (void) updateGraphicalDisplays
{
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
  //    is called with ourselves as the sender.
	
  // Update any displays with current model state.
  // [someDisplay update];
	
  // Inform ourselves that updates are complete and graphical views
  // should be re-displayed.
  [self swarmHasUpdatedNotification];
}

@end
