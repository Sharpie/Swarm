#import <space/Discrete2d.h>

@interface SeedSpace: Discrete2d
{
  int maxSeeds;
  double deathRate;
}

- setDeathRate: (double)d;
- addSeeds: (int)seedNum X: (int)x Y: (int)y;
- (int)seedsAtX: (int)theX Y: (int)theY;
- setMaxSeeds: (int)aValue;
- initializeLattice;
- stepRule;
@end







