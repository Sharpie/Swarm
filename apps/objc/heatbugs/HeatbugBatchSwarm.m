// Heatbugs application. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import "HeatbugBatchSwarm.h"
#import "HeatbugModelSwarm.h"
#import <collections.h>

@implementation HeatbugBatchSwarm

// createBegin: here we set up the default observation parameters.
+createBegin: (id) aZone {
  HeatbugBatchSwarm * obj;

  // Superclass createBegin to allocate ourselves.
  obj = [super createBegin: aZone];

  // Fill in the relevant parameters.
  obj->displayFrequency = 1;
  return obj;
}

-buildObjects {
  id modelZone;					  // zone for model.

  [super buildObjects];

  // create a zone for the model, create the model there.
  modelZone = [Zone create: [self getZone]];
  heatbugModelSwarm = [HeatbugModelSwarm create: modelZone];

  // In HeatbugObserverSwarm, we'd build some probes and wait for a
  // user control event. But since we don't have any graphics, we just
  // build the model and go. Eventually we intend to have an object that
  // reads experiment parameters from a file to set things up.
  
  // First, let the model swarm build its objects.
  [heatbugModelSwarm buildObjects];

  // Now build some data analysis objects. In this case we're just
  // going to create an averager to collect some statistics   
  unhappinessAverager = [Averager createBegin: [self getZone]];
  [unhappinessAverager setList: [heatbugModelSwarm getHeatbugList]];
  [unhappinessAverager setProbedSelector: M(getUnhappiness)];
  unhappinessAverager = [unhappinessAverager createEnd];

  // And open a file for writing (see -writeData for comments)
  outputFile = fopen("heatbugs.data", "w");

  // All done - we're ready to build a schedule and go.
  return self;
}  

// Create the actions necessary for the simulation. This is where
// the schedule is built (but not run!)
-buildActions {
  [super buildActions];
  
  // First, let our model swarm build its own schedule.
  [heatbugModelSwarm buildActions];
  
  // Create an ActionGroup for display. This is pretty minimal in this
  // case. Note, there's no doTkEvents message - no control panel!
  displayActions = [ActionGroup create: [self getZone]];
  // Now schedule the update of the unhappiness graph
  [displayActions createActionTo: unhappinessAverager message: M(update)];
  [displayActions createActionTo: self message: M(writeData)];

  // the displaySchedule controls how often we write data out.
  displaySchedule = [Schedule createBegin: [self getZone]];
  [displaySchedule setRepeatInterval: displayFrequency];
  displaySchedule = [displaySchedule createEnd];
  [displaySchedule at: 0 createAction: displayActions];

  // we also add in a "stopSchedule", another schedule with an absolute
  // time event - stop the system at time 250. 
  stopSchedule = [Schedule create: [self getZone]];
  [stopSchedule at: 250 createActionTo: self message: M(stopRunning)];
  
  return self;
}  

// activateIn: - get the Swarm ready to run.
-activateIn: (id) swarmContext {
  // First, activate ourselves (just pass along the context).
  [super activateIn: swarmContext];

  // And, we need to activate the model swarm, also in ourselves.
  [heatbugModelSwarm activateIn: self];

  // Now activate our schedules in ourselves. Note that we just activate
  // both schedules: the activity library will merge them properly.
  [displaySchedule activateIn: self];
  [stopSchedule activateIn: self];

  // Activate returns the swarm activity - the thing that's ready to run.
  return [self getSwarmActivity];
}

// the HeatbugObserverSwarm had a go method inherited from GUISwarm,
// but we have to define our own here. It's pretty simple. There's also
// a friendly message printed out here just in case someone is confused
// when they run heatbugs and see no graphics.
-go {
  printf("No DISPLAY environment variable was set, so we're running without graphics.\n");
  printf("Heatbugs is running for 250 time steps and writing data to heatbugs.data.\n");
  [swarmActivity run];
  return [swarmActivity getStatus];
}


// The "writeData" method writes out data to the given file. This is a
// very primitive way of doing data collection - just stdio calls. We
// would like to have fancy File objects, as well as parallels to the
// ActiveGraph object that chain data analysis objects to files.
-writeData {
  fprintf(outputFile, "%d %g\n", getCurrentTime(),
	  [unhappinessAverager getAverage]);
  return self;
}

// And the termination method. When this fires we just terminate everything
// that's running and close our output files.
-stopRunning {
  [getTopLevelActivity() terminate];
  fclose(outputFile);
  return self;
}

@end
