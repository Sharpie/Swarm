// Bug.h					simpleExperBug	

#import "FoodSpace.h"

#import <objectbase/SwarmObject.h>
#import <space.h>

@interface Bug: SwarmObject
{
  int xPos, yPos;
  int worldXSize, worldYSize;
  
  id <Grid2d> world;
  FoodSpace *foodSpace;

  int haveEaten;
}

- setWorld: w Food: f;

- createEnd;

- setX: (int)x Y: (int)y;
- step;

@end

