// Mousetraps application. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import "MousetrapModelSwarm.h"
#import <simtools.h>

// Implementation of stats tracker. Counts how many times anything has
// ever been triggered, also how many balls are in the air.
@implementation MousetrapStatistics
-(int) getNumTriggered { return numTriggered; }
-(int) getNumBalls { return numBalls; }
-addOneBall { numBalls++; return self; }
-addOneTriggered { numTriggered++; return self; }
-removeOneBall {
  if (numBalls > 0)
    numBalls--;
  else
    fprintf(stderr, "Error: negative balls!\n");
  return self;
}
@end


@implementation MousetrapModelSwarm

-(MousetrapStatistics *) getStats { return stats; }
-(int) getGridSize { return gridSize; }
-(double) getTriggerLikelihood { return triggerLikelihood; }
-(int) getNumberOutputTriggers { return numberOutputTriggers; }
-(int) getMaxTriggerDistance { return maxTriggerDistance; }
-(int) getMaxTriggerTime { return maxTriggerTime; }

-(Mousetrap *)getMousetrapAtX: (int) x Y: (int) y {
  return [grid getObjectAtX: x Y: y];
}

// createBegin: here we set up the default simulation parameters.
+createBegin: (id) aZone {
  MousetrapModelSwarm * obj;
  ProbeMap * probeMap;

  obj = [super createBegin: aZone];

  obj->gridSize = 50;
  obj->triggerLikelihood = 1.0;
  obj->numberOutputTriggers = 2;
  obj->maxTriggerDistance = 4;
  obj->maxTriggerTime = 16;
  obj->trapDensity = 1.0;

  probeMap = [EmptyProbeMap createBegin: aZone];
  [probeMap setProbedClass: [self class]];
  probeMap = [probeMap createEnd];

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
  [probeMap addProbe: [[probeLibrary getProbeForMessage: "reloadAll"
                             inClass: [self class]]
                        setHideResult: 1]];
#ifdef INTERACTIVEMESSAGES
  [probeMap addProbe: [[probeLibrary getProbeForMessage: "restart"
                             inClass: [self class]]
                        setHideResult: 1]];
  [probeMap addProbe: [[probeLibrary getProbeForMessage: "dropBallAtX:Y:"
                             inClass: [self class]]
                        setHideResult: 1]];
#endif
  
  [probeLibrary setProbeMap: probeMap For: [self class]];
  
  return obj;
}

-buildObjects {
  int x, y;

  // allow our parent class to build anything.
  [super buildObjects];

  // statistics object
  stats = [MousetrapStatistics create: [self getZone]];
  
  // build grid
  grid = [Grid2d createBegin: [self getZone]];
  [grid setSizeX: gridSize Y: gridSize];
  grid = [grid createEnd];
  
  for (y = 0; y < gridSize; y++)
    for (x = 0; x < gridSize; x++)
      if (trapDensity >= 1.0 || [uniformRandom rFloat] < trapDensity) {
	Mousetrap * aMousetrap;
	aMousetrap = [Mousetrap create: [self getZone]
				setModelSwarm: self
				setXCoord: x setYCoord: y];
	[grid putObject: aMousetrap atX: x Y: y];
      }
  
  return self;
}

-buildActions {
  [super buildActions];

  // just make one schedule. Autodrop, so old activities get destroyed.
  modelSchedule = [Schedule createBegin: [self getZone]];
  [modelSchedule setAutoDrop: 1];
  modelSchedule = [modelSchedule createEnd];

  // schedule the first mousetrap
  [self scheduleTriggerAt: 0 For: [grid getObjectAtX: gridSize/2 Y: gridSize/2]];
  [stats addOneBall];
  return self;
}

// This can be called anytime - dynamic scheduling.
-scheduleTriggerAt: (int) n For: (Mousetrap *) trap {
  [modelSchedule at: n createActionTo: trap message: M(trigger)];
  return self;
}

-activateIn: (id) swarmContext {
  [super activateIn: swarmContext];
  [modelSchedule activateIn: self];
  return [self getSwarmActivity];
}

-reloadAll {
  int x, y;
  Mousetrap * trap;

  for (y = 0; y < gridSize; y++)
    for (x = 0; x < gridSize; x++) {
      trap = [grid getObjectAtX: x Y: y];
      if (trap)
	[trap reload];
    }
  return self;
}

#ifdef INTERACTIVEMESSAGES
// messages for interacting with a running mousetraps simulation.
// there's some glitch in the scheduling code that makes dynamic
// scheduling from inside the display code not work, so these are disabled
// for now.

-restart {
  [self scheduleTriggerAt: getCurrentTime() + 1 For: [grid getObjectAtX: gridSize/2 Y: gridSize/2]];
  [stats addOneBall];
  return self;
}
  
-dropBallAtX: (int) x Y: (int) y {
  Mousetrap * trap;

  trap = [grid getObjectAtX: x Y: y];
  if (trap) {
    [self scheduleTriggerAt: getCurrentTime() + 1 For: trap];
    [stats addOneBall];
  }
  return self;
}

#endif

@end
