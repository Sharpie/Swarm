// Bug.h					simpleExperBug	

#import "FoodSpace.h"

#import <swarmobject/SwarmObject.h>
#import <space.h>
#import <simtools.h>


@interface Bug: SwarmObject {

  int xPos, yPos;
  int worldXSize, worldYSize;
  
  Grid2d * world;
  FoodSpace * foodSpace;

  int haveEaten;
}

-setWorld: (id) w Food: (id) f;

-createEnd;

-setX: (int) x Y: (int) y;
-step;
-drawSelfOn: (Raster *) r;

@end

