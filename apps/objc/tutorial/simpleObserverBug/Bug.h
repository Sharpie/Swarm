// Bug.h					SimpleBug

#import <objectbase/SwarmObject.h>
#import <space.h>
#import <gui.h>
#import "FoodSpace.h"

@interface Bug: SwarmObject
{
  int xPos, yPos;
  int worldXSize, worldYSize;
  
  id <Grid2d> world;
  id food;

  int haveEaten;
}

- setWorld: w Food: f;
- createEnd;

- setX: (int) x Y: (int) y;
- step;
- drawSelfOn: (id <Raster>)r;

@end

