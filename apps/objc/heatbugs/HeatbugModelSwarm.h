// Heatbugs application. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// The Heatbug Model swarm encapsulates all the objects used in the
// simulated heatbug world itself (but not the user interface objects)

#import "Heatbug.h"
#import "HeatSpace.h"
#import <space.h>
#import <activity.h>
#import <collections.h>
#import <objectbase/Swarm.h>
#import <objectbase.h>

@interface HeatbugModelSwarm: Swarm
{
  int numBugs;					  // simulation parameters
  double evaporationRate;
  double diffuseConstant;
  int worldXSize, worldYSize;
  int minIdealTemp, maxIdealTemp;
  int minOutputHeat, maxOutputHeat;
  double randomMoveProbability;

  id modelActions;				  // scheduling data structures
  id modelSchedule;

  id heatbugList;				  // list of all the heatbugs
  Grid2d *world;				  // objects representing
  HeatSpace *heat;				  // the world
}

- getHeatbugList;				  // access methods into the
- (Grid2d *)getWorld;				  // model swarm. These methods
- (HeatSpace *)getHeat;				  // allow the model swarm to
						  // be observed.

- addHeatbug: (Heatbug *)bug;			  // special method for demo

+ createBegin: aZone;				  // extra methods you
- createEnd;					  // provide for Swarms
- buildObjects;
- buildActions;
- activateIn: swarmContext;

@end
