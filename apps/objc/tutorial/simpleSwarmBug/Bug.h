// Bug.h					SimpleBug

#import <swarmobject/SwarmObject.h>
#import <space.h>

@interface Bug: SwarmObject {

  int xPos, yPos;
  int worldXSize, worldYSize;
  
  id foodSpace;

}

-setWorldSizeX: (int) x Y: (int) y;
-setFoodSpace: (id) f;
-createEnd;

-setX: (int) x Y: (int) y;

-step;

@end

