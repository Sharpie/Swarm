#import "Person.h"

@implementation Person

// A bunch of methods that set
// one parameter or another


- createEnd
{
  return [super createEnd];

}


- setX: (int)inx Y: (int)iny 
{
  x = inx;
  y = iny;
  
  return self;
}


- (int)getX 
{
  return x;
}

- (int)getY 
{
  return y;
}


- (void)setNhoodType: (int)nhood
{
  nhoodType = nhood;
}

- (void)setNhoodRadius: (int)rad
{
  radius = rad;
}


- (void)setEdgeWrap: (BOOL)wrap
{
  edgeWrap = wrap;
}


- setWorld: w 
{
  myWorld = w;
  xsize = [[myWorld getObjectGrid] getSizeX];
  ysize = [[myWorld getObjectGrid] getSizeY];

  return self;
}

- setColor: (int)c 
{
  myColor = c;
  return self;
}

	
- (int)getColor 
{
  return myColor;
}


- (void)setIDNumber: (int)numb
{
  idnumber = numb;
}	


- setTolerance: (double)t
{
  myTolerance = t;
  return self;
}

- (double)getTolerance 
{
  return myTolerance;
}


// The method that implements the agents
// actual behavior - look around and try
// to move if you are unhappy
- step  
{
  double fracMyColor = [self getFractionOf: myColor];
  unhappy = 0; //make happy to start

  //fprintf(stderr,"ID: %d fracMyColor=%f\n", idnumber, fracMyColor);
  // Then see if you are happy in your neighborhood
  // and if you are not try to move somewhere else
  if( myTolerance < 1.0 - fracMyColor  )
    {
      unhappy = 1; 
      [self moveToNewLocation];
    }
  return self;
}

- moveToNewLocation 
{
  BOOL newLocation;
  int newX = 0, newY = 0;
  
  // Find an empty neighbor according to
  // the definition of neighborhoood. The coordinates 
  // are put into newX and newY. The return value
  // is a YES/NO variable indicating a position was found.
  newLocation = [myWorld findEmptyLocationX: &newX Y: &newY];
  if (newLocation == YES)
    {

      [myWorld removeObject: self atX: x Y: y];
      [myWorld addObject: self atX: newX Y: newY];

      x = newX;
      y = newY;
    }
  return self;
}



- (double)getFractionOf: (int)t 
{
  int sumSimilar = [myWorld getVisibleNRace: myColor InVicinityX: x Y: y];
  
  int numNeighbors = [myWorld getVisiblePopulationX: x Y: y ];
  
  // uncomment this if you want to see the match/mismatch between the agent's view
  // and the data the world reports. With ASYNCHRONOUS updating, there should be no mismatch
  // [self verifyNhoodData: t];
  
  return (double) (sumSimilar-1)/(numNeighbors-1);
  //suppose you want the agent to count itself. then change the
  //above to
  //  return (double) sumSimilar/numNeighbors;
}


- (double)verifyNhoodData: (int)t 
{
  int numNeighbors = 0;
  int sumSimilar = 0;
  int i,j;
  Person * neighbor;
 

  int
    myabs ( xx )
    {
      return (xx>0) ? xx: -xx;
    }
  
  
  if (nhoodType == 1)// This enforces a VonNeuman neighborhood 
    {
      
      for(i = -radius; i < radius+1; i++)
	{
	  int xcoord = x + i;
	  int llimit = -radius -i;
	  int ulimit = radius + 1 + i;
	  if (i > 0) 
	    {
	      llimit = -radius + i;
	      ulimit = radius -i +1;
	    }
	  for (j = llimit; j < ulimit ; j++)
	    {
	      int ycoord = y + j ;
	     
	      if ( xcoord< 0 || xcoord >= xsize)
		{
		  if (edgeWrap == NO) continue;
		  else xcoord = [self wrapXCoord: xcoord];
		}
	      if ( ycoord< 0 || ycoord >= ysize)
		{
		  if (edgeWrap == NO) continue;
		  else ycoord = [self wrapYCoord: ycoord];
		}
	      
	      if((neighbor = [[myWorld getObjectGrid] getObjectAtX: xcoord Y: ycoord]))
		{
		  numNeighbors++;
		  if([neighbor getColor] == t)
		    {
		      sumSimilar++;
		    }
		}
	    }
	}

    } 
  else 
    {
      // This enforces a Moore neighborhood
      for(i = -radius; i< radius + 1; i++) 
	{
	  int xcoord = x+i;
	  if ( xcoord< 0 || xcoord >= xsize)
	    {
	      if (edgeWrap == NO) continue;
	      else xcoord = [self wrapXCoord: xcoord];
	    }
	  
	  for(j = -radius; j< radius +1 ; j++) 
	    {
	      int ycoord = y+j;
	      if ( ycoord< 0 || ycoord >= ysize)
		{
		  if (edgeWrap == NO) continue;
		  else ycoord = [self wrapYCoord: ycoord];
		}
	      if( (neighbor = [[myWorld getObjectGrid] getObjectAtX: xcoord Y: ycoord] ))
		{
		  numNeighbors++;
		  if([neighbor getColor] == t)
		    sumSimilar++;
		}
	    }
	}
    }
   if ( [myWorld getVisibleNRace: myColor InVicinityX: x Y: y] != sumSimilar  ) 
    fprintf(stderr,"Problem at (%d,%d) World says %ld %ld I say %d %d\n",x,y,[myWorld getVisibleNRace: myColor InVicinityX: x Y: y],[myWorld getVisiblePopulationX: x Y: y ],   sumSimilar, numNeighbors);

   return (double)[myWorld getVisibleNRace: myColor InVicinityX: x Y: y]/[myWorld getVisiblePopulationX: x Y: y ];
}
 

- drawSelfOn: (id)rast 
{
  // A method to draw the agent on the raster
  [rast drawPointX: x Y: y Color: myColor];
  return self;
}


- (int)getUnhappy 
{
  return unhappy;
}





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


@end
