#import "SchellingWorld.h"
#import "Person.h"
#import "ModelSwarm.h"
#import "Nhood2dCounter.h"
#import <space.h>


@implementation SchellingWorld

/*
  There can be any number of races.  So to keep track of the data on 
  "how many people of each type can I see from x,y", use a Swarn Array
  of Int2d objects.  There is one Int2d for each type we are tracking.
  

*/

- (int)wrapCoord: (int)inCoord atModulus: (int)inModulus
{
  if (inCoord < 0 || inCoord >= inModulus)
    { 
      inCoord = (inCoord + inModulus) % inModulus; 
    }
  return inCoord;
}

- (int)wrapXCoord: (int)inCoord
{
  return [self wrapCoord: inCoord atModulus: xsize];
}

- (int)wrapYCoord: (int)inCoord
{
  return [self wrapCoord: inCoord atModulus: ysize];
}


- setSizeX: (unsigned)x Y: (unsigned)y
{
  xsize = x;
  ysize = y;

  return self;
}


- createObjectGridX: (unsigned) x Y: (unsigned) y
{
  id aGrid;
  aGrid = [Discrete2d createBegin: [self getZone]];
  [aGrid setSizeX: x Y: y];
  aGrid = [aGrid createEnd];
  return aGrid;
}

- getObjectGrid
{
  return objectGrid;
}


- (void)setModelSwarm: m 
{
  modelSwarm=m;
}


- (BOOL)findEmptyLocationX: (int*)newX Y: (int*)newY
{
  int x,y;
  int proposedX=0, proposedY =0;
  unsigned int i,j;
  BOOL moved = NO;


  // Pick a random starting point
  x = [modelSwarm getRandomIntMin: 0 Max: xsize-1];
  y = [modelSwarm getRandomIntMin: 0 Max: ysize-1];
 
  // Find the first empty spot
  for(i = 0; i < xsize; i++)
    { 
      proposedX = [self wrapXCoord: x + i];
      for(j = 0; j <  ysize; j++) 
	{
	  proposedY = [self wrapYCoord: y + j];
	  if(!([objectGrid getObjectAtX: proposedX Y: proposedY]))
	    {
	      moved = YES;

	      *newX = proposedX ;
	      *newY = proposedY ;
	      break; 
	    } 
	  
	}
      if (moved == YES) break;
      
    }
  
  return moved;
}



- (void)setNhoodRadius: (int)r NhoodType: (int)n EdgeWrap: (BOOL)wrap Sync: (BOOL)sync
{
  radius = r;
  nhoodType = n;
  edgeWrap = wrap;
  SYNCHRONOUS = sync;
}


- (void)setRaces: (unsigned)r
{
  RACES = r;
}


- createEnd
{
  if (RACES < 1 || radius < 1 ) raiseEvent(InvalidArgument,"radius or nhoodType is not set");
  [self createRaceArray];
  
  objectGrid = [self createObjectGridX: xsize Y: ysize];
  return [super createEnd];
}


- createRaceArray
{
  unsigned i;
  raceArray = [Array create: [self getZone] setCount: RACES];
  for (i=0; i < RACES; i++)
    {
      Nhood2dCounter * aNhood = [Nhood2dCounter createBegin: [self getZone] setSizeX: xsize Y: ysize];
      [aNhood setNhoodRadius: radius NhoodType: nhoodType EdgeWrap: edgeWrap Sync: SYNCHRONOUS];
      aNhood = [aNhood createEnd];
      [raceArray atOffset: i put: aNhood]; 
    }
  return self;
}



//pj-2001-05-02 The stepRule only needs to be called for a SYNCHRONOUS model,
//because that one accumulates the changes and pushes them all through at once.
//The ASYNCHRONOUS alternative flushes through each agent's action immediately.

- stepRule
{
  Nhood2dCounter * aNhood;
  id index = [raceArray begin: [self getZone]];
  for (aNhood = [index next]; [index getLoc]==Member; aNhood = [index next])
    {
      [aNhood stepRule];
    }
  [index drop];

  return self;
}


 


- (long)getVisibleNRace: (int)op InVicinityX: (int)x Y: (int)y
{
  return [[raceArray atOffset: op] getNumVisibleAtX: x Y: y] ;
}



// if you have a lot of races, this might be slower than
// adding a different Nhood2dCounter to measure visible population

- (long)getVisiblePopulationX: (int)x Y: (int)y
{
  int sum = 0;
  Nhood2dCounter * aNhood;
  id index = [raceArray begin: [self getZone]];
  
  for (aNhood = [index next]; [index getLoc]==Member; aNhood = [index next])
    {
      sum += [aNhood getNumVisibleAtX: x Y: y];
    }
  [index drop];

  return sum;
}


- removeObject: aPerson atX: (int)x Y: (int)y
{
  int agentColor = [aPerson getColor];
  [self removeRace: agentColor atX: x  Y: y];
  [objectGrid putObject: nil atX: x Y: y];
  return self;
}


- addObject: aPerson atX: (int)x Y: (int)y
{
   int agentColor = [aPerson getColor];
   [self addRace: agentColor atX: x Y: y];
   [objectGrid putObject: aPerson atX: x Y: y];
   return self;
}


- (long)addRace: (int)stance atX: (int)x Y: (int)y 
{
  long retval;

  retval = [[raceArray atOffset: stance] addOneAtX: x Y: y] ;
  return retval;
}


- (long)removeRace: (int)stance atX: (int)x Y: (int)y 
{
  long retval =  [[raceArray atOffset: stance] removeOneAtX: x Y: y];
  return retval;
}


- (void)printDiagnostics
{
  unsigned int i, j, k;
 
  for (i = 0; i < xsize; i++)
    {
      for(j=0; j < ysize; j++)
	{
	  for (k=0; k< [raceArray getCount]; k++)
	    {
	      fprintf(stderr,"Race (%d, %d) race: %d num %ld \n", i,j,  k,
		      [[raceArray atOffset: k] getNumVisibleAtX: i Y: j]) ; 
	    }
	  fprintf(stderr,"\n");
	}
      fprintf(stderr,"\n");
    }
}

@end


