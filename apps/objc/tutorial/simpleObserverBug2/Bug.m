// Bug.m					SimpleBug

#import "Bug.h"
#import <simtools.h>

@implementation Bug

-setWorld: (id) w Food: (id) f {
   world = w;
   food = f;
   return self;
}

-createEnd {
   [super createEnd];

   worldXSize = [world getSizeX];
   worldYSize = [world getSizeY];
   return self;
}

-setX: (int) x Y: (int) y {
  xPos = x;
  yPos = y;
  return self;
}

-step {
  int newX, newY;

  haveEaten = 0;

  newX = xPos + [uniformIntRand getIntegerWithMin: -1 withMax: 1];
  newY = yPos + [uniformIntRand getIntegerWithMin: -1 withMax: 1];

  newX = (newX + worldXSize) % worldXSize;
  newY = (newY + worldYSize) % worldYSize;

  if ([world getObjectAtX: newX Y: newY] == nil) {
    [world putObject: nil atX: xPos Y: yPos];
    xPos = newX;
    yPos = newY;
    [world putObject: self atX: newX Y: newY];
  }
     
  if ([food getValueAtX: xPos Y: yPos] == 1) {
      [food putValue: 0 atX: xPos Y: yPos];
      haveEaten = 1; 
  }

  return self;
}

-drawSelfOn: (Raster *) r {
  [r drawPointX: xPos Y: yPos Color: 3];
  return self;
}

  
@end

  


