#import "Forest.h"
#import "Tree.h"
#import <space.h>
#import <random.h>
#import "Species.h"


@implementation Forest

- setWorldSize: (int)aSize
{
  worldSize = aSize;
  return self;
}

- (int)getWorldSize
{
  return worldSize;
}

- createEnd
{
  
#ifdef USELISTS
  
 
  matureTreeList = [OrderedSet createBegin: [self getZone]];
  [matureTreeList setIndexFromMemberLoc: offsetof(Tree, listMembership)];
  matureTreeList = [matureTreeList createEnd];
  
  youngTreeList = [OrderedSet createBegin: [self getZone]];
  [youngTreeList setIndexFromMemberLoc: offsetof(Tree, listMembership)];
  youngTreeList = [youngTreeList createEnd];
 
#endif

  
  matureTreeGrid = [[[Grid2d createBegin: [self getZone]] setSizeX: worldSize Y: worldSize] createEnd];
       
  youngTreeGrid = [[[Grid2d createBegin: [self getZone]] setSizeX: worldSize Y: worldSize] createEnd];
   
  return self;
}

- getTreeGrid: (int)i
{
  return ((i==1) ? matureTreeGrid : youngTreeGrid);
}

#ifdef USELISTS
- getTreeList: (int)i
{
  return ((i==1) ? matureTreeList : youngTreeList);
}
#endif


- step
{
   [self processSpeciesGermination];

   [self processSpeciesSeeds];

   [self processMatureTrees];

   [self processYoungTrees];

   [self updatePopulationRecords];

  return self;
}

- getTreeType: (int)i AtX: (int)xVal Y: (int)yVal
{
  id treeGrid = ((i==1) ? matureTreeGrid : youngTreeGrid);
  return [treeGrid getObjectAtX: xVal Y: yVal];
}


- addTree: aTree atX: (int)xVal Y: (int)yVal
{
  int gridType = 0;
  
  if ( [aTree getAge] <= [[ aTree getSpecies ] getAgeLevel: 1] )
    {
      gridType = 0;
    }
  else
    {
      gridType = 1;
    }
 
  [self addTree: aTree Type: gridType atX: xVal Y: yVal];
  

  return self; 
}


- addTree: aTree Type: (int)i atX: (int)xVal Y: (int)yVal
{
  id treeGrid = ((i==1) ? matureTreeGrid : youngTreeGrid);

#ifdef USELISTS
  id treeList =  ((i==1) ? matureTreeList : youngTreeList);
  if (!aTree) raiseEvent(InvalidArgument,"Inserted a nil");
#endif

  if (i == 1) //a mature tree was added
    [[aTree getSpecies] incrementPopulation];

#ifdef USELISTS
  [treeList  addFirst: aTree];
#endif 

  [aTree setGrid: treeGrid];  
  [aTree setX: xVal Y: yVal];
  
  [treeGrid putObject: aTree atX: xVal Y: yVal];

  [aTree initializeReproductionIvars];// tree uses its species and position
  // to set important instance variables (IVARS)

  return self; 
}



- addTree: aTree
{
  int x,y;
  id treeGrid;
  int gridType;

  if ([aTree getAge] <= [[ aTree getSpecies ] getAgeLevel: 1])
    {
      gridType = 0;
      treeGrid = youngTreeGrid;
     }
  else
    {
      gridType = 1;
      treeGrid = matureTreeGrid;
     }
  do 
    {
      x = [uniformIntRand getIntegerWithMin: 0 withMax: [treeGrid getSizeX] - 1];
      y = [uniformIntRand getIntegerWithMin: 0 withMax: [treeGrid getSizeY] - 1];
    } while([treeGrid getObjectAtX: x Y: y]);

 
  [self addTree: aTree Type: gridType atX: x Y: y];


  return self;
}



- fireAtX: (int)theX Y: (int)theY Heat: (int)generatedHeat
{
  int i ;
  int speciesNumber=[speciesList getCount];
  [self destroyTreesAtX: theX Y: theY];

  for(i = 0 ; i < speciesNumber ; i++)
    [[speciesList atOffset: i] destroySeedsAtX: theX Y: theY] ;
  
  //Now cycle around vicinity telling other cells there is fire here
  {
    int i,j ;
    id anObj ;
    
    for(i = theX - 1 ; i < theX + 2 ; i++)
      for(j = theY - 1 ; j < theY + 2 ; j++)
	if( (i >= 0) && (j >= 0) && (i < worldSize) && (j < worldSize) )
	  if( (anObj = [self getTreeType: 1 AtX: i Y: j]) )
	    [anObj fireWithHeat: generatedHeat] ;
  }


  [fireGrid putValue: 1 atX: theX Y: theY] ;
  
  return self ;
}




- destroyTreesAtX: (int)theX Y: (int)theY
{
  int i;

  for (i = 0; i < 2; i++)
    {
      id treeGrid = ((i==1) ? matureTreeGrid : youngTreeGrid);

#ifdef USELISTS
      id treeList = ((i==1) ? matureTreeList : youngTreeList);     
#endif

      id anObj;
      
      if ((anObj = [treeGrid getObjectAtX: theX Y: theY]))
	{

#ifdef USELISTS
	  id returnTree = [treeList remove: anObj];
	  if (!returnTree)raiseEvent(InvalidArgument,"destroy tree mistake");
#endif
	  [treeGrid putObject: nil atX: theX Y: theY];
	  [anObj drop];
	}
    
   }  
  return self;
}



- setSpeciesList: aList
{
  speciesList = aList;
  return self;
}


- setFireGrid: aGrid
{
  fireGrid = aGrid;
  return self;
}


- processSpeciesGermination
{
  int i,j,k,aPotencyVal;
  int totalSeeds, choice;
  int speciesNumber = [speciesList getCount];
  int potencyArray[speciesNumber];

  for(i = 0; i < worldSize; i++)
    for(j = 0; j < worldSize; j++)
      {
        if ([youngTreeGrid getObjectAtX: i Y: j])
          continue;
        
        totalSeeds = 0;
        
        for (k = 0; k < speciesNumber; k++)
          {
            aPotencyVal = 
              [[speciesList atOffset: k ] getPotencyAtX: i Y: j];
            potencyArray[k] = aPotencyVal;
            totalSeeds += aPotencyVal;
          }
        
        if (!totalSeeds)
          continue;
        
        totalSeeds++;
        
        choice = [uniformIntRand getIntegerWithMin: 0 withMax: totalSeeds - 1];
        
        choice--;
        
        for (k = 0; k < speciesNumber; k++)
          {
            choice -= potencyArray[k];
            
            if (choice < 0)
              {
		Tree * aTree = [[speciesList atOffset: k] createTree: [self getZone]];
		[aTree setX: i Y: j];
		[aTree setAge: 0];
                [self addTree: aTree  atX: i Y: j];
                break; 
              }
          }
        
        if (choice >= 0)
          {
            printf("Major problem in spring module\n");
            exit(-1);
          }
      }

  return self;
}


- processSpeciesSeeds
{
  int i;
  int speciesNumber= [speciesList getCount];
  for(i = 0 ; i < speciesNumber ; i++)
    [[[speciesList atOffset: i] getSeedSpace]  stepRule];
  return self;
}


- processMatureTrees
{
  Tree * aTree;

#ifdef USELISTS
  id <Index> index ;
  id exitQ1;
#endif

  int count;
  int j=0;
 

#ifdef USELISTS
  exitQ1 = [List create: [self getZone]];
  index = [matureTreeList  begin: [self getZone]];
  j = 0;

  count =[matureTreeList getCount] ;

  for( aTree = [index next]; [index getLoc]==Member ;  aTree = [index next])
    {
      int treeStatus = 99;
      if (!aTree) raiseEvent(InvalidArgument,"Tree was nil");

#else

  id * lattice = [matureTreeGrid getLattice];

   count = worldSize * worldSize;
    
   for ( j=0; j < count; j++)
     {
      int treeStatus = 99;
      aTree = lattice[j];

#endif
     
      if (aTree) treeStatus = [aTree step];
      if (treeStatus == 3) 
	{
	;
	  [matureTreeGrid putObject: nil atX:  [aTree getX] Y: [aTree getY]];
#ifdef USELISTS
	  [exitQ1 addLast: aTree];
#else
          [aTree drop];
#endif
	}
      else if (treeStatus == 1 || treeStatus == 2)
	{
	  fprintf(stderr,"Tree age %d ageLevels=(%d,%d,%d,%d)\n",
	  	  [aTree getAge],[[aTree getSpecies]  getAgeLevel: 1],
		  [[aTree getSpecies]  getAgeLevel: 2],
		  [[aTree getSpecies]  getAgeLevel: 3],
		  [[aTree getSpecies]  getAgeLevel: 4]);
	  raiseEvent(InvalidArgument,"Mature tree wrong treeStatus=%d\n",treeStatus);
	}
      

#ifdef USELISTS
      j++;
#endif
      
     }

#ifdef USELISTS
  
  [index drop];

  index = [exitQ1 begin: [self getZone]];
  for( aTree = [index next]; [index getLoc]==Member ;  aTree = [index next])
    {
      id treeList ;
      id returnTree;
      
      //  if ([aTree getGrid] == youngTreeGrid)
      // 	{
      // 	  raiseEvent(InvalidArgument,"wrong kind of tree in exitQ1");
      //    }
      treeList = matureTreeList;
      returnTree = [treeList remove: aTree] ;
    }
  [index drop] ;
  [exitQ1 deleteAll];
#endif
  
  return self;
}


- processYoungTrees
{
  Tree * aTree;
  int count;
  int j=0;

#ifdef USELISTS
  id <Index> index ;
  id exitQ0, graduateQ;
  exitQ0 = [List create: [self getZone]];

  graduateQ = [List create: [self getZone]];

    
  index = [youngTreeList  begin: [self getZone]] ;
   
  j = 0;
  count = [youngTreeList getCount]; 

  for ( aTree = [index next]; [index getLoc]==Member ;  aTree = [index next])
    {
      int treeStatus;
#else

  id * lattice = [youngTreeGrid getLattice];//all cells in one big vector
  count = worldSize * worldSize;//total number of cells in grid
    
  for ( j=0; j < count; j++)
     {
       int treeStatus = 99;
       aTree = lattice[j];
#endif
   
       treeStatus = [aTree step];

       if (  treeStatus == 1 || treeStatus == 2) 
	 {
	   if ( treeStatus == 1 ) 
	     {
	       
	       [youngTreeGrid putObject: nil atX: [aTree getX] Y: [aTree getY]];
#ifdef USELISTS
	      
 	      [exitQ0 addLast: aTree];
#else
	      [aTree drop];
#endif
	     }
	   else
	     {
	       [[aTree getSpecies] incrementPopulation];
	       [youngTreeGrid  putObject: nil atX: [aTree getX] Y: [aTree getY]];
	       [matureTreeGrid  putObject: aTree atX: [aTree getX] Y: [aTree getY]];
	       [aTree setGrid: matureTreeGrid ];
#ifdef USELISTS
	       [graduateQ addLast: aTree];
#endif
	     }
	 }
       if (treeStatus == 3) 
	 {
	   fprintf(stderr,"Tree age %d ageLevels=(%d,%d,%d,%d)\n",
		   [aTree getAge],[[aTree getSpecies]  getAgeLevel: 1],
		   [[aTree getSpecies]  getAgeLevel: 2],
		   [[aTree getSpecies]  getAgeLevel: 3],
		   [[aTree getSpecies]  getAgeLevel: 4]);
	   
	   raiseEvent(InvalidArgument,"Young Trees: wrong tree return value\n");
	 }
#ifdef USELISTS
       j++;
     }
#else
    }
#endif

#ifdef USELISTS
  
  [index drop];
  
  index = [exitQ0 begin: [self getZone]];
  for( aTree = [index next]; [index getLoc]==Member ;  aTree = [index next])
    {
      id treeList ;
      id returnTree;
      
      if ([aTree getGrid] == matureTreeGrid )
	{
      	  raiseEvent(InvalidArgument,"wrong kind of tree in exitQ0");
	}
      
      treeList = youngTreeList;
      returnTree = [treeList remove: aTree] ;
    }

  [index drop] ;
  
  [exitQ0 deleteAll];
  
  index = [graduateQ begin: [self getZone]];
  for( aTree = [index next]; [index getLoc]==Member ;  aTree = [index next])
    {
      id returnTree;
      
      returnTree = [youngTreeList remove: aTree]; 
      if (returnTree != aTree) raiseEvent(InvalidArgument,"wrong tree in Forest graduation");
      [matureTreeList addFirst: aTree];
    }
  [index drop] ;
  
#endif 
  return self;
}


// This was the step method in Arborgames-2.2's ForestModelSwarm
- updatePopulationRecords
{
  int total, i ;
  int speciesNumber = [speciesList getCount];
  total = 0 ;

  for(i = 0 ; i < speciesNumber ; i++) 
    total += [[speciesList atOffset: i] getCount] ;

  for(i = 0 ; i < speciesNumber ; i++) 
    [[speciesList atOffset: i] setTotalNumberOfTrees: total] ;

  return self ;
}

@end
