// ModelSwarm.h					SimpleBug App

#import "Bug.h"
#import "FoodSpace.h"
#import <swarmobject/Swarm.h>
#import <space.h>
#import <activity.h>

@interface ModelSwarm: Swarm {

  int worldXSize, worldYSize;
  float seedProb;
  float bugDensity;

  FoodSpace * food;
  Grid2d * world;
  Bug * reportBug;

  id bugList;
  id modelActions;
  id modelSchedule;
}

+createBegin: (id) aZone;
-createEnd;
-buildObjects;
-buildActions;
-activateIn: (id) swarmContext;

@end


