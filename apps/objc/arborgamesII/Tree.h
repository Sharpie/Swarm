#import <objectbase/SwarmObject.h>
#import <gui.h>

@class Species;
@class Forest;

@interface Tree: SwarmObject
{

  Forest*  theForest;
  id whichGrid;

  int leftEdge, rightEdge, topEdge, bottomEdge; //seeding square edges
  int seedPeriodicity, seedsPerSquare; 
  Species * species;
   
  int age;
  int totalHeat;
  int seedingCounter;
  int x, y;			

  member_t listMembership;
}

- (void)setForest: aForest;
- setSpecies: aSpecies;
- (void)initializeReproductionIvars;
- setAge: (int)anAge;
- setSeedingCounter: (int)aYear;
- createEnd;

- (int)getAge; 
- (int)getX;
- (int)getY;
- getSpecies;
- clearSeedingCounter;
- (int) incSeedingCounter;
- (int) incAge;
- setX: (int)x Y: (int)y;			
- setGrid: theGrid;
- getGrid;
- (int)step;
- spreadSeeds;
- (void)burn;
- fireWithHeat: (int)theHeat;
- drawSelfOn: (id <Raster>)r;
- (int)getSpeciesIdentifier;
- (void)drop;

@end
