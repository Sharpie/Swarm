// Mousetraps application. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// The Mousetrap Model swarm encapsulates all the objects used in the
// simulated mousetrap world itself (but not the user interface objects)

// The actions built here implement *dynamic scheduling*
// Note that the MousetrapModelSwarm uses dynamic scheduling
// while the MousetrapObserverSwarm uses static scheduling

#import <objectbase/Swarm.h>
#import <objectbase/SwarmObject.h>
#import <space.h>
#import <random.h>
#import "Mousetrap.h"

// First, a special object to help in collecting statistics.

@interface MousetrapStatistics: SwarmObject
{
  int numTriggered;
  int numBalls;
}

- addOneTriggered;
- addOneBall;
- removeOneBall;
- (int)getNumTriggered;
- (int)getNumBalls;

@end

// Now, the the mousetrap model swarm.

@interface MousetrapModelSwarm: Swarm
{
  int gridSize;					  // simulation parameters
  double triggerLikelihood;
  int numberOutputTriggers;
  int maxTriggerDistance;
  int maxTriggerTime;
  double trapDensity;

  id modelActions;				  // scheduling data structures
  id modelSchedule;

  MousetrapStatistics *stats;			  // statistics object
  id <Grid2d> grid;				  // world

  // Declare the ActivityControl, which will serve as an interface to
  //   the model swarm activity.
  id <ActivityControl> modelActCont;

@private
  id <PMMLCG1> randomGenerator;
  id <UniformDouble> uniform0to1;
}

// Methods for the MousetrapModelSwarm

- (MousetrapStatistics *)getStats;		  // modelSwarm methods
- (int)getGridSize;				  // These methods allow the 
- (Mousetrap *)getMousetrapAtX: (int)x Y: (int)y; // model swarm to be observed
- (double)getTriggerLikelihood;
- (int)getNumberOutputTriggers;
- (int)getMaxTriggerDistance;
- (int)getMaxTriggerTime;
- (id <Grid2d>)getWorld;                             // for Probes
- (id <Schedule>)getSchedule;

// Methods overridden to create the model Swarm

+ createBegin: aZone;
- createEnd;
- buildObjects;
- buildActions;
- activateIn: swarmContext;

// A special method for *dynamic scheduling*

- scheduleTriggerAt: (int)n For: (Mousetrap *)trap;   

@end
