// Mousetraps application. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <swarmobject.h>
#import <space.h>
#import <activity.h>
#import <collections.h>
#import <simtools.h>
#import "Mousetrap.h"

// First, a special object to help in collecting statistics.
@interface MousetrapStatistics : SwarmObject {
  int numTriggered;
  int numBalls;
}
-addOneTriggered;
-addOneBall;
-removeOneBall;
-(int) getNumTriggered;
-(int) getNumBalls;
@end

// Now, the swarm.
@interface MousetrapModelSwarm : Swarm {
  int gridSize;					  // simulation parameters
  double triggerLikelihood;
  int numberOutputTriggers;
  int maxTriggerDistance;
  int maxTriggerTime;
  double trapDensity;

  id modelActions;				  // scheduling data structures
  id modelSchedule;

  MousetrapStatistics * stats;			  // statistics object
  Grid2d * grid;				  // world
}

-(MousetrapStatistics *)getStats;
-(int) getGridSize;
-(Mousetrap *) getMousetrapAtX: (int) x Y: (int) y;
-(double) getTriggerLikelihood;
-(int) getNumberOutputTriggers;
-(int) getMaxTriggerDistance;
-(int) getMaxTriggerTime;

+createBegin: (id) aZone;
-buildObjects;
-buildActions;
-activateIn: (id) swarmContext;

-scheduleTriggerAt: (int) n For: (Mousetrap *) trap; // dynamic scheduling

-reloadAll;

@end
