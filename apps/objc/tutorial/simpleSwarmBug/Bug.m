// Bug.m					SimpleBug

#import "Bug.h"
#import "FoodSpace.h"
#import <random.h>

@implementation Bug

- setWorldSizeX: (int)xSize Y: (int)ySize
{
  worldXSize = xSize;
  worldYSize = ySize;
  return self;
}

- setFoodSpace: f
{
   foodSpace = f;
   return self;
}

- createEnd
{
   [super createEnd];
   return self;
}

- setX: (int)x Y: (int)y
{
  xPos = x;
  yPos = y;
  printf("\n I started at X = %d Y = %d \n\n", xPos, yPos);
  return self;
}

- step
{
  xPos = xPos + [uniformIntRand getIntegerWithMin: -1 withMax: 1];
  yPos = yPos + [uniformIntRand getIntegerWithMin: -1 withMax: 1];

  xPos = (xPos + worldXSize) % worldXSize;
  yPos = (yPos + worldYSize) % worldYSize;

  if ([foodSpace getValueAtX: xPos Y: yPos] == 1)
    {
      [foodSpace putValue: 0 atX: xPos Y: yPos];
      printf( "I found food at X = %d Y = %d ! \n", xPos, yPos);
    }
  
  return self;
}
  
@end

  


