// Bug.m

#import "Bug.h"
#import <simtools.h>

@implementation Bug

-setX: (int) x Y: (int) y {
  xPos = x;
  yPos = y;
  printf("I started at X = %d Y = %d \n\n", xPos, yPos);
  return self;
}

-setWorldSizeX: (int) xSize Y: (int) ySize {
  worldXSize = xSize;
  worldYSize = ySize;
  return self;
}

-step {

  xPos = xPos + [uniformIntRand getIntegerWithMin: -1 withMax: 1];
  yPos = yPos + [uniformIntRand getIntegerWithMin: -1 withMax: 1];

  xPos = (xPos + worldXSize) % worldXSize;
  yPos = (yPos + worldYSize) % worldYSize;

  printf( "I moved to X = %d Y = %d \n", xPos, yPos);

  return self;
}
  
@end

  


