// ModelSwarm.h					simpleExperBug 

#import "Bug.h"
#import "FoodSpace.h"

#import <objectbase/Swarm.h>
#import <space.h>

@interface ModelSwarm: Swarm
{
  int worldXSize, worldYSize;

  float seedProb;
  float bugDensity;

  int time;

  FoodSpace *foodSpace;
  id <Grid2d> world;

  id bugList;
  id modelActions;
  id modelSchedule;
}

- getWorld;
- getFoodSpace;
- getBugList;
- (int)getTime;

- setWorldXSize: (int)x YSize: (int)y;
- setSeedProb: (float)s bugDensity: (float)b;

+ createBegin: aZone;
- createEnd;

- buildObjects;
- buildActions;
- activateIn: swarmContext;

@end


