#import <objectbase.h>
#import <objectbase/SwarmObject.h>
#import <collections.h> //for raceArray
#import <space.h>  //for Discrete2d

@interface SchellingWorld: SwarmObject  
{
  unsigned int RACES;
  id modelSwarm; 

  unsigned int xsize, ysize;


  BOOL edgeWrap;
  BOOL SYNCHRONOUS;
  int radius;
  unsigned int nhoodWidth;
  int nhoodType;


  id <Discrete2d> objectGrid;
  id <Array> raceArray;

}


- (int)wrapCoord: (int)inCoord atModulus: (int)inModulus;
- (int)wrapXCoord: (int)inCoord;
- (int)wrapYCoord: (int)inCoord;


- setSizeX: (unsigned)x Y: (unsigned)y;

- createObjectGridX: (unsigned) x Y: (unsigned) y;

- getObjectGrid;

- (void)setModelSwarm: m;

- (BOOL)findEmptyPerpendicularX: (int*)newX Y: (int*)newY;

- (BOOL)findEmptyLocationX: (int*)newX Y: (int*)newY;

- (BOOL)findNearestAcceptableColor: (int)col Tolerance:(double)tol X: (int*)newX Y: (int*)newY;

- (void)setNhoodRadius: (int)r NhoodType: (int)n EdgeWrap: (BOOL)wrap Sync: (BOOL)sync;

- (void)setRaces: (unsigned)r;

- createEnd;

- createRaceArray;

- stepRule;


- (long)getVisibleNRace: (int)op InVicinityX: (int)x Y: (int)y;

- (long)getVisiblePopulationX: (int)x Y: (int)y;


- removeObject: aPerson atX: (int)x Y: (int)y;

- addObject: aPerson atX: (int)x Y: (int)y;

- (long)addRace: (int)stance atX: (int)x Y: (int)y;


- (long)removeRace: (int)stance atX: (int)x Y: (int)y ;

- (void)printDiagnostics;

@end


