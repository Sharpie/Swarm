// Bug.h					simpleExperBug	

#import "FoodSpace.h"

#import <objectbase/SwarmObject.h>
#import <space.h>
#import <simtools.h>

@interface Bug: SwarmObject
{
  int xPos, yPos;
  int worldXSize, worldYSize;
  
  Grid2d *world;
  FoodSpace *foodSpace;

  int haveEaten;
}

- setWorld: w Food: f;

- createEnd;

- setX: (int)x Y: (int)y;
- step;

@end

