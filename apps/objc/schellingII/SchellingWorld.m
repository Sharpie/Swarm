#import "SchellingWorld.h"
#import "Person.h"
#import "ModelSwarm.h"
#import "Nhood2dCounter.h"
#import <space.h>


@implementation SchellingWorld

/*
  There can be any number of races.  So to keep track of the data on
  "how many people of each type can I see from x,y", use a Swarm Array
  of Nhood2dCounter objects.  There is one Nhood2dCounter for each
  type we are tracking.
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





- (BOOL)findEmptyPerpendicularX: (int*)newX Y: (int*)newY
{
  int range;
  int x = *newX;
  int y = *newY;
  int RANGE = (xsize < ysize) ? xsize/2 : ysize/2;
  unsigned int i, j;

  BOOL moved = NO;

  for (range = 1; range < RANGE; range++)
    {
      BOOL inbounds[4]={YES,YES,YES,YES};
      int proposedX[4] = {x+range,x+range,x-range,x-range};
      int proposedY[4] = {y+range,y-range,y+range,y-range};



      if (edgeWrap)
	for (i =0; i< 4; i++)
	  {
	    proposedX[i]= [self wrapXCoord: proposedX[i]];
	    proposedY[i]= [self wrapYCoord: proposedY[i]];
	  }
      else
	for (i = 0; i < 4; i++)
	  {
	    if (proposedX[i]<0 || proposedY[i] <0 || proposedX[i]>xsize-1 || proposedY[i]>ysize-1)
	      inbounds[i]=NO;
	  }

      j = [uniformUnsRand getUnsignedWithMin: 0 withMax: 3];  

      for(i = j; i < 4 +j; i++)
	{ 
	  int t = i;
	  if (t > 3) {t = t-4;}
	  if(inbounds[t] && !([objectGrid getObjectAtX: proposedX[t] Y: proposedY[t]]))
	      {
		//	fprintf(stderr,"moved range= %d, t=%d, x=%d,y=%d, newx=%d,newy=%d\n",range,t,x,y,proposedX[i],proposedY[i]);
		moved = YES;
		
		*newX = proposedX[t] ;
		*newY = proposedY[t] ;
		break; 
	      } 
	  
	}
      if (moved == YES) break;
      
    }
  
  return moved;
}



// This was the method before I came along. Clearly, not very good.
// it ends up searching just left and right
- (BOOL)findEmptyLocationX: (int*)newX Y: (int*)newY
{
  int x,y;
  int proposedX=0, proposedY =0;
  unsigned int i,j;
  BOOL moved = NO;


  // Pick a random starting point
  x = *newX + [modelSwarm getRandomIntMin: 0 Max: xsize-1];
  y = *newY + [modelSwarm getRandomIntMin: 0 Max: ysize-1];
 
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



// Recall the neighborhood type ( nhoodType ) 1 == von Neumann neighborhood
//  2 == Moore

// This algorithm to find the nearest improvement searches in larger and larger
// neighborhoods.  For example, in a model with VN neighborhoods, the agent first searches
// 

//                   1
//                 1   1
//                   1

// and then

//                   1
//                 1   1
//               1       1
//                 1   1
//                   1

// and then

//                   1
//                 1   1
//               1       1
//             1           1
//               1       1
//                 1   1
//                   1

// The agent will take the first improvement found, and the algorithm starts at a random position
// within this diamond shape each time.

// On the other hand, a Moore neighborhood is just progressively larger squares

//                 1 1 1
//                 1   1
//                 1 1 1

//                 1 1 1 1 1
//                 1       1
//                 1       1
//                 1       1
//                 1 1 1 1 1

// I did it this way because I'm stubborn


- (BOOL)findNearestAcceptableColor: (int)col Tolerance:(double)tol X: (int*)newX Y: (int*)newY
{
  int i, j, range;
  int centerx = (*newX);
  int centery = (*newY);
  int RANGE = (xsize < ysize) ? xsize/2 : ysize/2; //see 1/2 world in either direction
  BOOL moved = NO;
  double otherProportion; 

  int
    myabs ( x )
    {
      return (x>0) ? x: -x;
    }
 

  //fprintf(stderr,"BEGIN Find nearest \n");
  for (range = 1; range < RANGE; range ++)
    {
      int n=(range*2)*(range*2) ; // n of cells to be checked
      int x[n];
      int y[n];
      int viablePosition = 0;
      for (i = -range; i < (1+range); i++)
	{
	  int candidateX = centerx + i;
	  BOOL inboundsX = YES;
	  
	  if (edgeWrap == YES) candidateX = [self wrapXCoord: candidateX]; 
	  else 
	    inboundsX = candidateX >= 0 && candidateX < xsize;
	  
	  if (i == -range || i == range)
	    {
	      for ( j = -range; j < (1+range); j++)  
		{
		  int candidateY = -1; 
		  BOOL inboundsY = YES;
		 
		  if (nhoodType == 1) candidateY = centery;
		  else candidateY = centery + j;
		
		  if (edgeWrap == YES)candidateY = [self wrapYCoord: candidateY]; 
		  else 
		    inboundsY = (candidateY >= 0) && (candidateY < ysize);
		  
		  if ( inboundsX && inboundsY ) 
		    {
		      x[viablePosition] = candidateX; 
		      y[viablePosition] = candidateY;
		      // fprintf(stderr,"I. viablPos=%d oldx=%d, oldy=%d,i=%d j=%d x=%d, y=%d\n", viablePosition,centerx,centery,i,j,x[viablePosition],y[viablePosition]);
		      viablePosition++;
		    }
		  if (nhoodType == 1) break;
		}
	    }
	  else 
	    {
	      for ( j= 0; j < 2; j++)
		{
		  int candidateY = -1;
		  BOOL inboundsY = YES;

		  if (nhoodType == 2) candidateY = centery - range + 2 * j * range;
		
		  else if (i > 0)candidateY = centery - (range - i ) + 2 * j * (range - i) ;
		  else           candidateY = centery - (range + i ) + 2 * j * (range + i) ;

		  if (edgeWrap == YES)candidateY = [self wrapYCoord: candidateY]; 
		  else 
		    inboundsY = (candidateY >= 0) && (candidateY < ysize);
		  
		  if ( inboundsX && inboundsY ) 
		    {
		      x[viablePosition] = candidateX; 
		      y[viablePosition] = candidateY;
		      // fprintf(stderr,"II. viablPos=%d oldx=%d, oldy=%d,i=%d j=%d x=%d, y=%d\n", viablePosition,centerx,centery,i,j,x[viablePosition],y[viablePosition]);
		      viablePosition++;
		    }
		}
	    }
	}
    


      // begin in this array at a random spot, so we get a random smattering
      // of the "first possible improvement"
 
      j = [uniformUnsRand getUnsignedWithMin: 0 withMax: viablePosition-1];  //randomly chosen starting point

      for (i=0; i < viablePosition; i++)
	{
	  int pos = (i + j);
	  if (pos > viablePosition-1) pos = pos-viablePosition;

	  // fprintf(stderr,"range=%d,viablePos=%d, i=%d, centerx=%d,centery=%d, pos=%d, x[i]=%d, y[i]=%d \n",  range,viablePosition, i, centerx, centery, pos, x[i], y[i]);

	  if ( ![objectGrid getObjectAtX: x[i] Y: y[i]])
	    {
	      int visibleColor =  [self getVisibleNRace: col InVicinityX: x[i] Y: y[i]];
	      int visiblePeople = [self getVisiblePopulationX: x[i] Y: y[i]];
	      BOOL selfVisibleFromXY = NO;
	      //moore
	      if (nhoodType == 2) selfVisibleFromXY= ((centerx-x[i] <= radius ) && (centery -y[i] <= radius))? YES: NO;
	      
	      else selfVisibleFromXY = (myabs(centerx-x[i]) + myabs(centery-y[i]) <= radius)? YES: NO ;

	      if ( selfVisibleFromXY )
		{
		  visibleColor -= 1; visiblePeople -=1; //don't count self in perceptions
		}

	      otherProportion = 1 - ((double)visibleColor/visiblePeople);
	      
	      // fprintf(stderr,"col=%d, tolerance=%1.2f \n ",col, tol);
	      // fprintf(stderr,"visibleColor=%d, visiblePeople=%d, otherProportion=%1.2f \n",    visibleColor, visiblePeople, otherProportion);
	      if (otherProportion < tol)
		{
		  moved = YES;
		  *newX = x[i];
		  *newY = y[i];
		  // fprintf(stderr,"MOVED: moved = %d\n", moved);
		  break;
		}
	      else
		{
		  //fprintf(stderr,"NOT MOVED \n");
		}
	    }
	  
	  if (moved == YES) break;
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


