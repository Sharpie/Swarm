// ModelSwarm.h					simpleExperBug 

#import "Bug.h"
#import "FoodSpace.h"

#import <objectbase/Swarm.h>
#import <space.h>

@interface ModelSwarm: Swarm
{
  unsigned worldXSize, worldYSize;

  float seedProb;
  float bugDensity;

  unsigned time;

  FoodSpace *foodSpace;
  id <Grid2d> world;

  id bugList;
  id modelActions;
  id modelSchedule;
}

- getWorld;
- getFoodSpace;
- getBugList;
- (unsigned)getTime;

- setWorldXSize: (unsigned)x YSize: (unsigned)y;
- setSeedProb: (float)s bugDensity: (float)b;

+ createBegin: aZone;
- createEnd;

- buildObjects;
- buildActions;
- activateIn: swarmContext;

@end


