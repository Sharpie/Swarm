// ModelSwarm.h					SimpleBug App

#import "Bug.h"
#import "FoodSpace.h"
#import <swarmobject/Swarm.h>
#import <space.h>
#import <activity.h>

@interface ModelSwarm: Swarm {

  int worldXSize, worldYSize;
  int xPos, yPos;
  float seedProb;

  FoodSpace * foodSpace;
  Bug * aBug;
  id modelSchedule;

}

+createBegin: (id) aZone;
-createEnd;
-buildObjects;
-buildActions;
-activateIn: (id) swarmContext;

@end


