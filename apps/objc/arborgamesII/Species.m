#import <space.h>
#import "ForestModelSwarm.h"
#import "Forest.h"
#import "SeedSpace.h"
#import "Tree.h"
#import "Species.h"
#import <misc.h>  //for exit() via stdlib.h

@implementation Species


- showSpecies 
{
  showAgeLevel = 0 ;
  return self ;
}

- showAgeLevel 
{
  showAgeLevel = 1 ;
  return self ;
}

- (void)setForest: aForest 
{
  theForest = aForest ;
}

- initWorldSize: (int)aSize 
{
  worldSize = aSize ;  
  seedSpace = [SeedSpace createBegin: [self getZone]];
  [seedSpace setSizeX: worldSize Y: worldSize];
  [seedSpace setDeathRate: seedMortalityRate];
  seedSpace = [seedSpace createEnd];
  numberOfTrees = 0 ;

  return self ;
}

- setModelSwarm: aSwarm 
{
  modelSwarm = aSwarm ;
  return self ;
}

- createEnd 
{
  return self ;
}

- (int)getShowAgeLevel 
{
  return showAgeLevel ;
}

- (char *)getSpeciesName 
{
  return speciesName ;
}

- (char *)getColorName 
{
  return colorName ;
}

- (Tree *)createTree: aZone;
{
  Tree * aTree = [Tree createBegin: aZone]; 
  aTree = [aTree createEnd];
  [aTree setForest: theForest];
  [aTree setSpecies: self];					    

  if (!theForest){exit(1);}

  return aTree;
}

- setColorMapEntry: (int)aColor 
{
  color = aColor ;
  return self ;
}

- (int)getInitialPopulation 
{
  return initialPopulation ;
}

- getSeedSpace 
{
  return seedSpace ;
}

- (int)getCanopyStructure 
{
  return canopyStructure ;
}

- (int)getAgeLevel: (int)anAgeLevel 
{
  return ageLevels[anAgeLevel] ;
}

- (int)getResistance: (int)anAgeLevel 
{
  return resistance[anAgeLevel] ;
}



- (int)getSeedPeriodicity 
{
  return seedPeriodicity ;
}

- (int)getLevelAtAge: (int)theAge 
{
  int i ;

  for(i = 0 ; i < 5 ; i++)
    if(theAge <= ageLevels[i])
      break ;

  if( i == 5 ){
    printf("Major problem with step/age/ageLevel mechanism!!!\n") ;
    exit(-1) ;
  }

  return i ;
}

- (int)getHeatAtAge: (int)theAge 
{
  int i ;

  for(i = 2 ; i < 5 ; i++)
    if(theAge <= ageLevels[i])
      break ;

  if( i == 5 ){
    printf("Major problem with step/age/ageLevel mechanism!!!\n") ;
    exit(-1) ;
  }

  i -= 2 ;

  return heat[i] ;
}

- (int)getPotencyAtX: (int)theX Y: (int)theY 
{
  Tree * aTree ;
  int numSeeds ;

  numSeeds = [seedSpace seedsAtX: theX Y: theY] ;
  if(numSeeds)
    {
      aTree = [theForest getTreeType: 1 AtX: theX Y: theY] ;
      if(aTree)
	{
	  //fprintf(stderr,"at (%d %d) numseeds=%d spec=%s\n" ,theX, theY, numSeeds,speciesName);
	  //fprintf(stderr,"treeage is %d\n",[aTree getAge]);
	  if( [[aTree getSpecies] getCanopyStructure] > shadeTolerance)
	    return 0 ;
	}
    }
  return numSeeds ;
}

- incrementPopulation
{
  numberOfTrees++ ;
  return self ;
}

- (void)decrementPopulation
{
  numberOfTrees -= 1 ;
}

- setTotalNumberOfTrees: (int)aTotal 
{
  totalNumberOfTrees = aTotal ;
  return self ;
}

- (unsigned)getCount
{
  return numberOfTrees;
}

- (int)stillActive 
{
  if(numberOfTrees)
    return 1 ;
  else 
    return 0 ;
}

- (double)getRelativeProportion 
{
  return ((double) numberOfTrees) / ((double) totalNumberOfTrees) ;
}

- (int)getColor 
{
  return color ;
}
- (int)getSeedingRadius
{
  return seedingRadius;
}
- (int)getSeedsPerSquare
{
  return seedsPerSquare;
}

- (int)getWorldSize
{
  return worldSize;
}


- destroySeedsAtX: (int)theX Y: (int)theY 
{
  [seedSpace putValue: 0 atX: theX Y: theY] ;
  return self ;
}

@end
