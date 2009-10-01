//
//  ÇPROJECTNAMEÈBatch.m
//  ÇPROJECTNAMEÈ Swarm Application
//
//  Created by ÇFULLUSERNAMEÈ on ÇDATEÈ.
//  Copyright ÇORGANIZATIONNAMEÈ ÇYEARÈ. All rights reserved.
//

#import "ÇPROJECTNAMEÈBatch.h"

@implementation ÇPROJECTNAMEÈBatch

- createEnd
{
    // experiment duration
	NSString *s = [simulationParameters objectForKey: @"cycles"];
	experimentDuration = [s intValue];
	
	s = [simulationParameters objectForKey: @"outputRate"];
	outputRate = [s intValue];
    
	return [super createEnd];
}

- buildObjects
{
    [super buildObjects];
    
    // create the model
    mainModel = [ÇPROJECTNAMEÈModel create: self withParameters: simulationParameters];
	[mainModel createEnd];
    
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
    
    if (outputRate) {
        // Create an ActionGroup for batch out.
        
        batchActions = [ActionGroup create: self];
        
        [batchActions createActionTo: self
                             message: M(outputBatchData)];
        
        // the batchSchedule controls how often we write data out.
        batchSchedule = [Schedule createBegin: self];
        [batchSchedule setRepeatInterval: outputRate];
        batchSchedule = [batchSchedule createEnd];
        
        [batchSchedule at: 0 createAction: batchActions];
    }
    
    // We also add in a "stopSchedule", another schedule with an absolute
    // time event - stop the system at experiment duration. 
    stopSchedule = [Schedule create: self];
    [stopSchedule at: experimentDuration 
      createActionTo: self 
             message: M(stopRunning)];
    
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
    if (outputRate)
        [batchSchedule activateIn: self];
    
    [stopSchedule activateIn: self];
    
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

// And the termination method. When this fires we just terminate everything
// that's running and close our output file(s) by dropping the EZGraph which
// "owns" the sequence(s) we are logging.

- stopRunning 
{
    [getTopLevelActivity() terminate]; // Terminate the simulation.
    
    return self;
}

- (void) outputBatchData
{
    printf("outputBatchData\n");
}

@end
