// Heatbugs application. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import "HeatbugBatchSwarm.h"
#import "HeatbugModelSwarm.h"

@implementation HeatbugBatchSwarm

// createBegin: here we set up the default observation parameters.

+createBegin: (id) aZone {
  HeatbugBatchSwarm * obj;

  // Superclass createBegin to allocate ourselves.
  obj = [super createBegin: aZone];

  // Fill in the relevant parameters.
  obj->loggingFrequency = 1;
  obj->experimentDuration = 250;

  return obj;
}

- buildObjects
{
  id modelZone;					       // zone for model.

  [super buildObjects];

  // create a zone for the model, create the model there.

  modelZone = [Zone create: [self getZone]];
  heatbugModelSwarm = [HeatbugModelSwarm create: modelZone];

  // In HeatbugObserverSwarm, we'd build some probes and wait for a
  // user control event (this allows the user to fiddle with the 
  // parameters of the experiment). But since we don't have any graphics, 
  // we load the batch.setup parameter file (which should contain values
  // for such variables as experimentDuration and loggingFrequency) and 
  // the model.setup parameter file (which contains values for the model
  // specific variables such as numBugs etc.).
  
  [ObjectLoader load: self 
                fromAppDataFileNamed: "batch.setup"];
  [ObjectLoader load: heatbugModelSwarm
                fromAppDataFileNamed: "experiment.setup"];

  // Now, let the model swarm build its objects.
  [heatbugModelSwarm buildObjects];

  // Finally, build some data analysis objects. In this case we're just
  // going to create an EZGraph (with graphics turned off and fileI/O
  // turned on) collect some statistics (the average) over the collection
  // of heatbugs (which we get from the heatbugModelSwarm).

  // If the user sets loggingFrequency to 0 s/he does not require the
  // logging of results at all. Consequently, some objects will not 
  // be created -> the schedule will also be simplified. This sort of 
  // switch is useful when the Sim could potentially log many different
  // aspects of the model...

  if(loggingFrequency){
    unhappyGraph = [EZGraph createBegin: [self getZone]];
    [unhappyGraph setGraphics: 0] ;
    [unhappyGraph setFileOutput: 1] ;
    unhappyGraph = [unhappyGraph createEnd] ;

    [unhappyGraph createAverageSequence: "unhappiness.output"
                           withFeedFrom: [heatbugModelSwarm getHeatbugList] 
                            andSelector: M(getUnhappiness)] ;
  }
  // All done - we're ready to build a schedule and go.
  return self;
}  

// Create the actions necessary for the simulation. This is where
// the schedule is built (but not run!)

-buildActions {
  [super buildActions];
  
  // First, let our model swarm build its own schedule.

  [heatbugModelSwarm buildActions];
  
  if(loggingFrequency){
  
    // Create an ActionGroup for display. This is pretty minimal in this
    // case. Note, there's no doTkEvents message - no control panel!

    displayActions = [ActionGroup create: [self getZone]];

    // Now schedule the update of the unhappyGraph, which will in turn 
    // cause the fileI/O to occur...

    [displayActions createActionTo: unhappyGraph message: M(step)];

    // the displaySchedule controls how often we write data out.
    displaySchedule = [Schedule createBegin: [self getZone]];
    [displaySchedule setRepeatInterval: loggingFrequency];
    displaySchedule = [displaySchedule createEnd];

    [displaySchedule at: 0 createAction: displayActions];
  }

  // We also add in a "stopSchedule", another schedule with an absolute
  // time event - stop the system at time . 

  stopSchedule = [Schedule create: [self getZone]];
  [stopSchedule at: experimentDuration 
    createActionTo: self 
         message: M(stopRunning)];
  
  return self;
}  

// activateIn: - get the Swarm ready to run.
-activateIn: (id) swarmContext {
  // First, activate ourselves (just pass along the context).
  [super activateIn: swarmContext];

  // We need to activate the model swarm.
  [heatbugModelSwarm activateIn: self];

  // Now activate our schedules in ourselves. Note that we just activate
  // both schedules: the activity library will merge them properly.
  [stopSchedule activateIn: self];
  if(loggingFrequency)
    [displaySchedule activateIn: self];

  // Activate returns the swarm activity - the thing that's ready to run.
  return [self getActivity];
}

// the HeatbugObserverSwarm had a go method inherited from GUISwarm,
// but we have to define our own here. It's pretty simple. There's also
// a friendly message printed out here just in case someone is confused
// when they run heatbugs and see no graphics.

-go {
  printf(
    "You typed 'heatbugs -batchmode', so we're running without graphics.\n");

  printf("Heatbugs is running for %d timesteps.\n",experimentDuration) ;
 
  if(loggingFrequency)
    printf("It is logging data every %d timesteps to: unhappiness.output.\n",
            loggingFrequency);

  [[self getActivity] run];
  return [[self getActivity] getStatus];
}

// And the termination method. When this fires we just terminate everything
// that's running and close our output file(s) by dropping the EZGraph which
// "owns" the sequence(s) we are logging.

-stopRunning {
  [getTopLevelActivity() terminate]; // Terminate the simulation.

  if(loggingFrequency)
    [unhappyGraph drop] ;              // Close the output file.

  return self;
}

@end
