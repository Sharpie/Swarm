// Mousetraps application. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import "MousetrapModelSwarm.h"
#import <simtools.h>

// First, the implementation of an object to keep statistics on
// the mousetrap world. It is an object that is model specific,
// but does not live "in" the mousetrap grid. 

// Implementation of stats tracker. Counts how many times anything has
// ever been triggered, also how many balls are in the air.

@implementation MousetrapStatistics

- (int)getNumTriggered
{ 
  return numTriggered;
}

- (int)getNumBalls
{
  return numBalls;
}

- addOneBall
{
  numBalls++;
  return self;
}

-addOneTriggered
{
  numTriggered++;
  return self;
}

- removeOneBall
{
  if (numBalls > 0)
    numBalls--;
  else
    fprintf(stderr, "Error: negative balls!\n");
  return self;
}

@end

// And now the implementation of the MousetrapModelSwarm

// The MousetrapModelSwarm defines the mousetrap world. All of the
// structures specific to the model are built and scheduled here.
// Observations on the model are built and scheduled in
// the MousetrapObserverSwarm

@implementation MousetrapModelSwarm

// These methods provide access to the objects inside the ModelSwarm.
// These objects are the ones visible to other classes via message call.

- (MousetrapStatistics *)getStats
{
  return stats;
}

- (int)getGridSize
{
  return gridSize; 
}

- (double)getTriggerLikelihood
{
  return triggerLikelihood; 
}

- (int)getNumberOutputTriggers
{
  return numberOutputTriggers;
}

- (int)getMaxTriggerDistance
{
  return maxTriggerDistance;
}

- (int)getMaxTriggerTime
{
  return maxTriggerTime;
}

- (Grid2d *)getWorld
{
  return grid;
}

- (Mousetrap *)getMousetrapAtX: (int)x Y: (int)y
{
  return [grid getObjectAtX: x Y: y];
}

// createBegin: here we set up the default simulation parameters.

+ createBegin: aZone
{
  MousetrapModelSwarm * obj;
  id <ProbeMap> probeMap;
  
  // First, call our superclass createBegin - the return value is the
  // allocated MousetrapModelSwarm object.
  
  obj = [super createBegin: aZone];

  // Now fill in various simulation parameters with default values.
  
  obj->gridSize = 50;
  obj->triggerLikelihood = 1.0;
  obj->numberOutputTriggers = 2;
  obj->maxTriggerDistance = 4;
  obj->maxTriggerTime = 16;
  obj->trapDensity = 1.0;

  // And build a customized probe map. Without a probe map, the default
  // is to show all variables and messages. Here we choose to
  // customize the appearance of the probe, give a nicer interface.

  probeMap = [EmptyProbeMap createBegin: aZone];
  [probeMap setProbedClass: [self class]];
  probeMap = [probeMap createEnd];

  // Add in a bunch of variables, one per simulation parameter

  [probeMap addProbe: [probeLibrary getProbeForVariable: "gridSize"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "triggerLikelihood"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "numberOutputTriggers"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "maxTriggerDistance"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "maxTriggerTime"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "trapDensity"
				    inClass: [self class]]];

  // Now install our custom probeMap into the probeLibrary.
  
  [probeLibrary setProbeMap: probeMap For: [self class]];
  
  return obj;
}

// createEnd: we could create some objects here if we knew we needed
// them. But this method is called before the user is allowed to fill
// in any customization of the model, so we defer most object creation
// to later. (In this example, this method does nothing at all and could
// just be inherited. But it's here to show you a place to customize.)

- createEnd
{
  id tempObj;
  
  tempObj = [super createEnd];
  
  randomGenerator = [PMMLCG1 create: [self getZone]
			     setStateFromSeed: 1234567890];
  uniform0to1 = [UniformDouble create: [self getZone]
			       setGenerator: randomGenerator
			       setDoubleMin: 0.0L
			       setMax: 1.0L];
  return tempObj;
}

// Now it's time to build the model objects. We use various parameters
// inside ourselves to choose how to create things.

- buildObjects
{
  int x, y;
  
  // First, allow our parent class to build anything.
  
  [super buildObjects];
  
  // Then, create a statistics object to manage gathering statistics
  
  stats = [MousetrapStatistics create: [self getZone]];
  
  // Now set up the grid used to represent agent position
  
  grid = [Grid2d createBegin: [self getZone]];
  [grid setSizeX: gridSize Y: gridSize];
  grid = [grid createEnd];
  
  // Then create the mousetraps themselves. We create a mousetrap for
  // each point in the Grid2d, initialize it, and anchor it down
  // in the grid.

  // Note that we don't create a mousetrapList equivalent to the
  // heatbugList in Heatbugs, as we will be scheduling the
  // mousetraps dynamically (although we could create such a
  // list for other purposes, such as observation....)

  for (y = 0; y < gridSize; y++)
    for (x = 0; x < gridSize; x++)
      if (trapDensity >= 1.0 || 
	  (float)[uniform0to1 getDoubleSample] < trapDensity)
        {
          Mousetrap * aMousetrap;
          aMousetrap = [Mousetrap create: [self getZone]
                                  setModelSwarm: self
                                  setXCoord: x 
                                  setYCoord: y
                                  setGenerator: randomGenerator];
          [grid putObject: aMousetrap atX: x Y: y];
        }
  
  return self;
}

// Here is where the model schedule is built, the data structures
// that define the simulation of time in the model. 

// Here, we implement *dynamic scheduling*

// Here is where mousetrap differs from time-step models like heatbugs.
// Mousetrap uses a discrete-event time-update, so we don't create
// a regularly repeated actionGroup and put it on a schedule. 
// Instead, we merely create an empty schedule, and let it
// know that once an action has been executed, it is to be
// dropped from the schedule (setAutoDrop = 1).

- buildActions
{
  // First, we let our superClass build actions
  
  [super buildActions];
  
  // just make one schedule. Autodrop, so old activities get destroyed.
  
  modelSchedule = [Schedule createBegin: [self getZone]];
  [modelSchedule setAutoDrop: 1];
  modelSchedule = [modelSchedule createEnd];

  // Now, we add one action to the schedule: trigger the mousetrap
  // at the center of the grid. And we also bump up the count of
  // "balls in the air" for bookeeping (This is the ball we will
  // "toss in" from the "outside" to start the chain-reaction.)

  [self scheduleTriggerAt: 0 For: [grid getObjectAtX: gridSize/2 Y: gridSize/2]];
  [stats addOneBall];
  return self;
}

// scheduleTriggerAt	*dynamic scheduling*

// This is how new actions get added to the schedule. When a mousetrap
// triggers, it randomly picks some other "nearby" mousetraps to
// trigger. "Triggering"  mousetraps simply means to add a "trigger" action 
// on them to the schedule, inserted at the proper time in the future.

- scheduleTriggerAt: (int)n For: (Mousetrap *)trap
{
  [modelSchedule at: n createActionTo: trap message: M(trigger)];
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
  

  //                  Activity Control Usage
  //   Attach a controller to the model swarm activity.  This interface
  // will function in the same way as the interface to the observer
  // swarm activity; but, the methods will not do the same things.
  // A "run" command to the observer swarm will execute the events
  // on the observer swarm schedule until that activity gets a command
  // telling it to stop.  A "run" on the model swarm, however, will
  // not do anything because the model swarm activity is not controllable.
  // This will change when we have a run-level tree or stack that will
  // provide the bookkeeping necessary to safely run subactivities.  With
  // the execution of the "run" method on the ActivityControl that is 
  // controlling a subactivity, will update the state variables shown
  // on the ActivityControl dashboard even though no messages were sent
  // to the activity, itself.
  //   See the comments for the ActivityControl attached to the observer
  // swarm in MousetrapObserverSwarm.m
  //
  modelActCont = [ActivityControl createBegin: [self getZone]];
  modelActCont = [modelActCont createEnd];
  [modelActCont setDisplayName: "Model Swarm Controller"];
  // attach the AC
  [modelActCont attachToActivity: [self getSwarmActivity]];
  // create a probe display for the AC
  createArchivedProbeDisplay(modelActCont);
  
  // Finally, return our activity.
  return [self getSwarmActivity];
}

@end
