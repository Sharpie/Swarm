// Bug.h					SimpleBug

#import <objectbase/SwarmObject.h>
#import <space.h>

@interface Bug: SwarmObject
{
  int xPos, yPos;
  int worldXSize, worldYSize;
  
  id foodSpace;
}

- setWorldSizeX: (int)x Y: (int)y;
- setFoodSpace: f;
- createEnd;

- setX: (int)x Y: (int)y;

- step;

@end

