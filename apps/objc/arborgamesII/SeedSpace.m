#import <math.h>
#import "SeedSpace.h"

@implementation SeedSpace

+ createBegin: aZone 
{
  SeedSpace * obj ;

  obj = [super createBegin: aZone] ;
  obj->maxSeeds = 100 ;
  obj->deathRate = 1.0 ;
  return obj ;
}

- setMaxSeeds: (int)aValue 
{
  maxSeeds = aValue ;
  return self ;
}

- setDeathRate: (double)d 
{
  deathRate = d;
  return self;
}

- initializeLattice 
{
  // unsigned x, y;

//   for (y = 0; y < ysize; y++)
//     for (x = 0; x < xsize; x++) {
//       *(discrete2dSiteAt(lattice, offsets, x, y)) = 0 ;
//     }

  [self fastFillWithValue: 0];

  return self;
}

- createEnd 
{
  [super createEnd];
  [self initializeLattice];
  return self;
}

- stepRule 
{
  unsigned x;
  int test ;
  double newVal ;

//   for (x = 0; x < xsize; x++)
//     for (y = 0; y < ysize; y++) {
//       test = (int) *(discrete2dSiteAt(lattice, offsets, x, y)) ;
//       if(test){
//         newVal = (double) test ;
//         newVal *= deathRate ; 
//         *(discrete2dSiteAt(lattice, offsets, x, y)) = 
//           (id) ((int) floor(newVal));
//       }
//     }

//PJ 2004-08-08: If we are going to be nasty and directly
//access the lattice, we might as well do it in the fastest way
  int totallength = xsize *ysize;
  for (x = 0; x < totallength; x++)
    {
      test = (int) lattice[x] ;
      // if(test){//don't bother testing. there are always seeds
      newVal = test * deathRate ; 
      lattice[x] = (id) ((int) floor(newVal));
    }

  return self;
}

- addSeeds: (int)seedNum X: (int)x Y: (int)y 
{
  int seedsHere;

  // replace direct access to the lattice like this:
  // seedsHere = (int) *(discrete2dSiteAt(lattice, offsets, x, y)) ;
  // with this:
  seedsHere = [self getValueAtX: x Y: y];
  //fprintf(stderr,"%d %d seedsHere = %d seednum=%d\n",x, y , seedsHere, seedNum);
  if (seedNum <= maxSeeds - seedsHere)
    seedsHere += seedNum;
  else
    seedsHere = maxSeeds;
        
  // *(discrete2dSiteAt(lattice, offsets, x, y)) = 
  //        (id) seedsHere ;
  
  [self putValue: seedsHere atX: x Y: y];
  //fprintf(stderr,"Finished %d %d seedsHere = %d seednum=%d\n",x,y, seedsHere, seedNum);
 
  return self;
}

- (int)seedsAtX: (int)theX Y: (int)theY {
  //return (int) *(discrete2dSiteAt(lattice, offsets, theX, theY)) ;
  return [self getValueAtX: theX Y: theY];
}

@end
