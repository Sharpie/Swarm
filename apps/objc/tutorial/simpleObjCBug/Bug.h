// Bug.h

#import <objectbase/SwarmObject.h>

@interface Bug: SwarmObject
{
  int xPos, yPos;				// Internal Variables
  int worldXSize, worldYSize;
}

- setX: (int)x Y: (int)y;
- setWorldSizeX: (int)x Y: (int)y;		// Methods
- step;

@end

