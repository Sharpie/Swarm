// ModelSwarm.h					SimpleBug App

#import "Bug.h"
#import "FoodSpace.h"
#import <objectbase/Swarm.h>
#import <space.h>

@interface ModelSwarm: Swarm
{
  int worldXSize, worldYSize;
  float seedProb;
  float bugDensity;

  FoodSpace *food;
  id <Grid2d> world;

  id bugList;
  id modelActions;
  id modelSchedule;

  id output;
}

- getWorld;
- getFood;
- getBugList;

- createEnd;

- (void)resetParameters;
- buildObjects;
- buildActions;
- activateIn: swarmContext;

- (void)drop;
- (BOOL)checkToStop;
@end


