// Bug.h					SimpleBug

#import <swarmobject/SwarmObject.h>
#import <space.h>
#import "FoodSpace.h"

@interface Bug: SwarmObject {

  int xPos, yPos;
  int worldXSize, worldYSize;
  
  Grid2d * world;
  id food;

  int haveEaten;

}

-setWorld: (id) w Food: (id) f;
-createEnd;

-setX: (int) x Y: (int) y;
-step;
-drawSelfOn: (Raster *) r;

@end

