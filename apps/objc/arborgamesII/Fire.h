#import <objectbase/SwarmObject.h>

@interface Fire: SwarmObject
{
  id forest;
  int worldSize;
  int freqLStrikes;
  id fireGrid;
}

- setFreqLStrikes: (int)freq;
- setWorldSize: (int)aSize;
- (void)setForest: aForest;
- setFireGrid: aGrid;
- getFireGrid;
- step;
@end


