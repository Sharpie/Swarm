// ModelSwarm.h					SimpleBug App

#import "Bug.h"
#import "FoodSpace.h"
#import <objectbase/Swarm.h>

@interface ModelSwarm: Swarm 
{
  int worldXSize, worldYSize;
  int xPos, yPos;
  float seedProb;

  FoodSpace * foodSpace;
  Bug * aBug;
  id modelSchedule;
}

+ createBegin: aZone;
- createEnd;
- buildObjects;
- buildActions;
- activateIn: swarmContext;

@end


