
#import <objectbase.h>
#import <objectbase/SwarmObject.h>
#import "SeedSpace.h"
#import <gui.h>

@class Tree;



@interface Species: SwarmObject
{
  //these are private variables that can only be accessed by "get methods."   
  int ageLevels[5];
  
  int resistance[3];

  int heat[3];
  int canopyStructure;

  double seedMortalityRate;
  int shadeTolerance;
  id seedSpace;

  id theForest;
  id modelSwarm;

  int initialPopulation;  // Both mature and young forest...
  int numberOfTrees;      // Only mature adults.
  int totalNumberOfTrees; // Used to calculate relative proportion...
  int showAgeLevel;

  char *colorName;
  char *speciesName;

  Color color;

  int seedPeriodicity;
  int seedsPerSquare;
  int seedingRadius;
  int worldSize;
}


- setColorMapEntry: (int)aColor;
- (void)setForest: aForest;
- initWorldSize: (int)aSize;
- setModelSwarm: aSwarm;
- createEnd;
- showAgeLevel;
- showSpecies;
- (int)getShowAgeLevel;
- (char *)getSpeciesName;
- (char *)getColorName;
- (Tree *)createTree: aZone;
- (int)getInitialPopulation;
- (int)getColor;
- (unsigned)getCount;
- setTotalNumberOfTrees: (int) aTotal;
- (int)stillActive;
- (double)getRelativeProportion;
- incrementPopulation;
- (void)decrementPopulation;
- getSeedSpace;
- (int)getCanopyStructure;
- (int)getLevelAtAge: (int)theAge;
- (int)getAgeLevel: (int) anAgeLevel;
- (int)getResistance: (int)aLevel;
- (int)getSeedPeriodicity;
- (int)getHeatAtAge: (int) theAge;
- (int)getPotencyAtX: (int) theX Y: (int) theY;
- (int)getSeedingRadius;
- (int)getSeedsPerSquare;
- (int)getWorldSize;
- destroySeedsAtX: (int)theX Y: (int)theY;
@end
















