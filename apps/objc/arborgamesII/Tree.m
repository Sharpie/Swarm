#import <space.h>
#import <gui.h>
#import "Tree.h"
#import "Species.h"
#import "SeedSpace.h"

#import "Forest.h"
#import <misc.h>

@implementation Tree


- (void)setForest: aForest
{
  theForest = aForest;
}

- setAge: (int)anAge
{
  age = anAge;
  return self;
}

- setSeedingCounter: (int)aYear
{
  seedingCounter = aYear;
  return self;
}

- setSpecies: aSpecies
{
 
  species = aSpecies;

  return self;
}

- (void)initializeReproductionIvars
{
  int seedingRadius = [species getSeedingRadius];
  
  int worldSize = [species getWorldSize];

  leftEdge = ((x - seedingRadius) > 0)? x-seedingRadius : 0;
  rightEdge = ((x + seedingRadius) < worldSize)? x+seedingRadius : worldSize-1;
  topEdge = ((y - seedingRadius)> 0) ? y-seedingRadius : 0;
  bottomEdge = ((y + seedingRadius) < worldSize) ? y + seedingRadius : worldSize-1;

  seedsPerSquare = [species getSeedsPerSquare];
  seedPeriodicity = [species getSeedPeriodicity];  
}

- setX: (int)inX Y: (int)inY
{
  x = inX;
  y = inY;
  return self;
}

- setGrid: theGrid
{
  whichGrid = theGrid;
  return self;
}

- getGrid
{
  return whichGrid;
}


- createEnd
{
  age = 0;
  totalHeat = 0;
  seedingCounter = 0;

  return self;
}

- (int)getAge
{
  return age;
}

- (int)getX
{
  return x;
}

- (int)getY
{
  return y;
}

- getSpecies
{
  return species;
}

- clearSeedingCounter
{
  seedingCounter = 0;
  return self;
}

- (int)incSeedingCounter
{
  seedingCounter++;
  return seedingCounter;
}

- (int)incAge
{
  age++;
  return age;
}

- (void)burn
{
  [theForest fireAtX: x Y: y Heat: [species getHeatAtAge: age]];
}



- fireWithHeat: (int)theHeat
{
  int i;

  totalHeat += theHeat;
  
  for(i = 2 ; i < 5 ; i++)
    if(age <= [species getAgeLevel:i])
      {
    	break ;
      }

  if( i == 5 )
    {
      exit(-1) ;
    }
  
  i -= 2 ;

  if (totalHeat > [species getResistance: i])
    {
      [self burn];
    }
  return self;
}


- (int)step
{
  totalHeat = 0;

  if (age == [species getAgeLevel: 1])
    {
      if ( [theForest getTreeType: 1 AtX: x Y: y] )
	{ 
	  return 1 ; 
	}//failed graduation and died
      else 
	{ 
	  age++; 
	  return 2; 
	} //graduate into mature forest
    }


  else if ( (age > [species getAgeLevel: 2]) && (age <= [species getAgeLevel: 3]) )
    {
      [self spreadSeeds];
    }


  else if(age >= [species getAgeLevel: 4])
    {
      return 3 ; //died
    }

  age++;
  return 0;//status nominal
}

// pj 2004-09-22 I've fiddled it to try to
// accelerate it, taking repeated calculations out of for loop.  
// Now relies on IVARs leftEdge,RightEdge, topEdge, bottomEdge, 
// seedsPerSquare and seedPeriodicity.  
- spreadSeeds
{
  int i, j;
  SeedSpace * seedSpace = [species getSeedSpace];

  seedingCounter++;

  if(seedingCounter >= seedPeriodicity)
    {
      for(i = leftEdge; i <= rightEdge ; i++)
	for(j = topEdge; j <= bottomEdge  ; j++)
	  //next should not be needed after correcting radius edges
	  //if ( (i >= 0) && (j >= 0) && (i < worldSize) && (j < worldSize) )
	    {
	      [seedSpace addSeeds: seedsPerSquare X: i Y: j] ;
	    }
      seedingCounter = 0 ;
    }
  return self;
}


- drawSelfOn: (id <Raster>)r
{
  if ( [species getShowAgeLevel] )
    [r drawPointX: x Y: y Color: ([species getLevelAtAge: age] + 25)];
  else 
    [r drawPointX: x Y: y Color: [species getColor]];

  return self;
}

- (int)getSpeciesIdentifier
{
  return [species getColor];
}

- (void)drop
{
  if (age > [species getAgeLevel: 1])[species decrementPopulation];
  [super drop];
}

@end
