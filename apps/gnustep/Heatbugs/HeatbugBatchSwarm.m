// Heatbugs application. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import "HeatbugBatchSwarm.h"
#import "HeatbugModelSwarm.h"
#import <simtools.h> // ObjectLoader
#import <analysis.h> // EZGraph

#include <misc.h> // printf

@implementation HeatbugBatchSwarm

// createBegin: here we set up the default observation parameters.

+ createBegin:  aZone 
{
  HeatbugBatchSwarm *obj;

  // Superclass createBegin to allocate ourselves.
  obj = [super createBegin: aZone];

  // Fill in the relevant parameters.
  obj->loggingFrequency = 10;
  obj->experimentDuration = 250;
  obj->heatbugDisplay = nil;

  return obj;
}

- (HeatbugModelSwarm *)getModel
{
  return heatbugModelSwarm;
}

- (id <Object2dDisplay>)getHeatbugDisplay
{
  return heatbugDisplay;
}

- (id <Value2dDisplay>)getHeatDisplay
{
  return heatDisplay;
}

- buildObjects
{
    const char *c = "modelSwarm";
  [super buildObjects];

  // IMPORTANT!!

  // Create the model inside us - no longer create `Zone's explicitly.
  // The Zone is now created implicitly through the call to create the
  // `Swarm' inside `self'.

  // But since we don't have any graphics, we load the object from the
  // global `lispAppArchiver' instance which is created automatically
  // from the file called `heatbugs.scm'

  // `modelSwarm' is the key in `heatbugs.scm' which contains the
  // instance variables for the HeatbugModelSwarm class, such as
  // numBugs etc.

  heatbugModelSwarm = [HeatbugModelSwarm create: self];

#if 0
  if ((heatbugModelSwarm = [lispAppArchiver getWithZone: self 
                                            key: c]) == nil)
    raiseEvent(InvalidOperation, 
               "Can't find the parameters to create modelSwarm");
#endif

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

  if(loggingFrequency)
    {
#if 0
      unhappyGraph = [EZGraph createBegin: self];
      [unhappyGraph setGraphics: 0] ;
      [unhappyGraph setFileOutput: 1] ;
      unhappyGraph = [unhappyGraph createEnd] ;
      
      [unhappyGraph createAverageSequence: "unhappiness.output"
                    withFeedFrom: [heatbugModelSwarm getHeatbugList] 
                    andSelector: M(getUnhappiness)] ;
#endif
    }

  // And also create an Object2dDisplay: this object draws heatbugs on
  // the worldRaster widget for us, and also receives probes.

  heatDisplay = 
    [Value2dDisplay createBegin: [self getZone]];
  [heatDisplay setDiscrete2dToDisplay: [heatbugModelSwarm getHeat]];
  [heatDisplay setDisplayMappingM: 32768 C: 0];	  // turn [0,32768) -> [0,64)
  heatDisplay = [heatDisplay createEnd];

  heatbugDisplay = 
    [Object2dDisplay createBegin: [self getZone]];
  [heatbugDisplay setDiscrete2dToDisplay: [heatbugModelSwarm getWorld]];
  heatbugDisplay = [heatbugDisplay createEnd];
  [heatbugDisplay setObjectCollection: [heatbugModelSwarm getHeatbugList]];

  // All done - we're ready to build a schedule and go.
  return self;
}  

// Create the actions necessary for the simulation. This is where
// the schedule is built (but not run!)

- buildActions 
{
  [super buildActions];
  
  // First, let our model swarm build its own schedule.

  [heatbugModelSwarm buildActions];
  
  if(loggingFrequency)
    {
      
      // Create an ActionGroup for display. This is pretty minimal in this
      // case. Note, there's no doTkEvents message - no control panel!
      
      displayActions = [ActionGroup create: self];
      
      // Now schedule the update of the unhappyGraph, which will in turn 
      // cause the fileI/O to occur...
#if 0
      [displayActions createActionTo: unhappyGraph message: M(step)];
#endif
#if 1
      [displayActions createActionTo: self
		      message: M(updateWorldDisplay)];
#endif

      // the displaySchedule controls how often we write data out.
      displaySchedule = [Schedule createBegin: self];
      [displaySchedule setRepeatInterval: loggingFrequency];
      displaySchedule = [displaySchedule createEnd];
      
      [displaySchedule at: 0 createAction: displayActions];
    }
  
  // We also add in a "stopSchedule", another schedule with an absolute
  // time event - stop the system at time . 
#if 0  
  stopSchedule = [Schedule create: self];
  [stopSchedule at: experimentDuration 
                createActionTo: self 
                message: M(stopRunning)];
#endif

  return self;
}  

// activateIn: - get the Swarm ready to run.
- activateIn:  swarmContext 
{
  // First, activate ourselves (just pass along the context).
  [super activateIn: swarmContext];
  
  // We need to activate the model swarm.
  [heatbugModelSwarm activateIn: self];
  
  // Now activate our schedules in ourselves. Note that we just activate
  // both schedules: the activity library will merge them properly.
  [stopSchedule activateIn: self];
  if (loggingFrequency)
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
  printf ("You typed `heatbugs -b' or `heatbugs --batch', so we're running without graphics.\n");

  printf ("Heatbugs is running for %d timesteps.\n",experimentDuration) ;
 
  if (loggingFrequency)
    printf ("It is logging data every %d timesteps to: unhappiness.output.\n",
            loggingFrequency);
  
  [[self getActivity] run];
  return [[self getActivity] getStatus];
}

// And the termination method. When this fires we just terminate everything
// that's running and close our output file(s) by dropping the EZGraph which
// "owns" the sequence(s) we are logging.

- stopRunning 
{
  [getTopLevelActivity() terminate]; // Terminate the simulation.

#if 0
  if(loggingFrequency)
    [unhappyGraph drop] ;              // Close the output file.
#endif

  return self;
}

- (void) updateWorldDisplay
{
  fprintf(stderr, "updateWorldDisplay\n");
  [self viewNeedsDisplay: [_delegate heatbugSpaceView]];
}

@end
