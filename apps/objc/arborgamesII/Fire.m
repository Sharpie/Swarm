#import <space.h>
#import "Fire.h"
#import "Forest.h"
#import "Tree.h"

#import <random.h>

@implementation Fire

- setWorldSize: (int)aSize
{
  worldSize = aSize;
  return self;
}

- (void)setForest: aForest
{
  forest = aForest;
}

- setFreqLStrikes: (int)freq
{
  freqLStrikes = freq;
  return self;
}

- setFireGrid: aGrid
{
  fireGrid = aGrid;
  return self;
}

- getFireGrid
{
  return fireGrid;
}

- step
{
  int i,rX,rY;
  int numStrikes;
  id aTree;

  [fireGrid fastFillWithValue: 0];

  
  numStrikes = [uniformIntRand getIntegerWithMin: 0 withMax: freqLStrikes];
  for (i = 0; i < numStrikes; i++)
    {
      rX = [uniformIntRand getIntegerWithMin: 0 withMax: worldSize - 1];
      rY = [uniformIntRand getIntegerWithMin: 0 withMax: worldSize - 1];
      if ((aTree = [forest getTreeType: 1 AtX: rX Y: rY]))
        [aTree burn];
    }
  
  return self;
}

@end


