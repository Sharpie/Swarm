// Mousetrap application. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import "MousetrapBatchSwarm.h"
#import "MousetrapModelSwarm.h"
#import <collections.h>

@implementation MousetrapBatchSwarm

// createBegin: here we set up the default observation parameters.

+ createBegin: aZone
{
  MousetrapBatchSwarm *obj;

  // Superclass createBegin to allocate ourselves.

  obj = [super createBegin: aZone];

  // Fill in the relevant parameters.

  obj->loggingFrequency = 1;

  return obj;
}

- buildObjects
{
  id modelZone;					       // zone for model.
  
  [super buildObjects];
  
  // create a zone for the model, create the model there.

  modelZone = [Zone create: [self getZone]];
  mousetrapModelSwarm = [MousetrapModelSwarm create: modelZone];
  
  // In MousetrapObserverSwarm, we'd build some probes and wait for a
  // user control event (this allows the user to fiddle with the 
  // parameters of the experiment). But since we don't have any graphics, 
  // we load the batch.setup parameter file (which should contain values
  // for such variables as experimentDuration and loggingFrequency) and 
  // the model.setup parameter file (which contains values for the model
  // specific variables such as gridSize, etc.).
  
  {
    const char *configPath = [arguments getAppConfigPath];
    char buf[strlen (configPath) + 12];

    sprintf (buf, "%sbatch.setup", configPath);
    [ObjectLoader load: self fromFileNamed: buf];
    sprintf (buf, "%smodel.setup", configPath);
    [ObjectLoader load: mousetrapModelSwarm fromFileNamed: buf];
  }
  // Now, let the model swarm build its objects.

  [mousetrapModelSwarm buildObjects];

  // Finally, build some data analysis objects. In this case we're just
  // going to create an EZGraph (with graphics turned off and fileI/O
  // turned on) to collect some statistics on mousetrap activity.

  // If the user sets loggingFrequency to 0 s/he does not require the
  // logging of results at all. Consequently, some objects will not 
  // be created -> the schedule will also be simplified. This sort of 
  // switch is useful when the Sim could potentially log many different
  // aspects of the model...

  if (loggingFrequency)
    {
      triggerGraph = [EZGraph createBegin: [self getZone]];
      [triggerGraph setGraphics:   0];
      [triggerGraph setFileOutput: 1];
      triggerGraph = [triggerGraph createEnd];
      
      // We create two sequences for the EZGraph to monitor:
      // 	1 - the total number of traps triggered so far
      //    2 - the number of currently pending trigger events
      // These are both created with the "createSequence" method
      // on EZGraph. In non-graphics mode, the data is written
      // to the respective ".output" files.
      
      [triggerGraph createSequence: "trigger.output"
                    withFeedFrom: [mousetrapModelSwarm getStats] 
                    andSelector: M(getNumTriggered)];
      
      [triggerGraph createSequence: "delta-trigger.output"
                    withFeedFrom: [mousetrapModelSwarm getStats] 
                    andSelector: M(getNumBalls)];
    }	
  
  // All done building objects - we're ready to build a schedule and go.
  
  return self;
}  

// Create the actions necessary for the simulation. This is where
// the schedule is built (but not run!)

- buildActions
{
  // First, let our super build any actions
  
  [super buildActions];
  
  // Then, let our model swarm build its actions

  [mousetrapModelSwarm buildActions];

  // schedule observations and data output to files (if logging....)
  
  if (loggingFrequency)
    {
      // Create an ActionGroup for display. This is pretty minimal in this
      // case. Note, there's no doTkEvents message - no control panel!
      
      displayActions = [ActionGroup create: [self getZone]];
      
      // Now schedule the update of the triggerGraph, which will in turn 
      // cause the fileI/O to occur...
      // Also, check to see if we should stop
      
      [displayActions createActionTo: triggerGraph message: M(step)];
      [displayActions createActionTo: self         message: M(checkToStop)];
      
      // the displaySchedule controls how often we write data out.
      
      displaySchedule = [Schedule createBegin: [self getZone]];
      [displaySchedule setRepeatInterval: loggingFrequency];
      displaySchedule = [displaySchedule createEnd];
      
      [displaySchedule at: 0 createAction: displayActions];
    }
  
  return self;
}

// activateIn: - get the Swarm ready to run.

- activateIn: swarmContext
{
  // First, activate ourselves (just pass along the context).
  
  [super activateIn: swarmContext];

  // Then, activate the model swarm.

  [mousetrapModelSwarm activateIn: self];

  // Now activate our schedules in ourselves. Note that we just activate
  // both schedules: the activity library will merge them properly.

  if (loggingFrequency)
    [displaySchedule activateIn: self];
  
  // Activate returns the swarm activity - the thing that's ready to run.
  
  return [self getActivity];
}

// the MousetrapObserverSwarm had a go method inherited from GUISwarm,
// but we have to define our own here. It's pretty simple. There's also
// a friendly message printed out here just in case someone is confused
// when they run mousetraps and see no graphics.

- go
{
  printf("You typed 'mousetrap -batchmode', so we're running without graphics.\n");
  
  printf("mousetrap is running to completion.\n");
 
  if (loggingFrequency)
    printf ("It is logging data every %d timesteps to: trigger.output.\n",
            loggingFrequency);
  
  [[self getActivity] run]; 		// Run it!

  // The model swarm completed. Close up and quit.

  if (loggingFrequency)
    [triggerGraph drop];               // Close the output file.
  
  return [[self getActivity] getStatus];
}

// monitor method - if all the balls have landed, time to quit!

- checkToStop
{
  if ([[mousetrapModelSwarm getStats] getNumBalls] == 0)
    {
      printf("All the balls have landed!\n");
      
      [getTopLevelActivity() terminate]; // Terminate the simulation.
    }
  
  return self;
}

@end
