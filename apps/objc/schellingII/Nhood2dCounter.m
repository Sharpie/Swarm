#import "Nhood2dCounter.h"


@implementation Nhood2dCounter


// for fast access to nhoodInclude array.
#define nhoodMask(x,y) ((nhoodInclude) + (nhoodOffsets)[(y)] + (x))



// for fast access to positions where data is stored.
#define D2D(l,off,x,y) ((l) + (off)[(y)] + (x))


+ createBegin: aZone setSizeX: (unsigned)x Y: (unsigned)y
{
  Nhood2dCounter *obj = [self createBegin: aZone];
  obj->xsize = x;
  obj->ysize = y;
  return obj;
}


- (void)setNhoodRadius: (int)r NhoodType: (int)n EdgeWrap: (BOOL)wrap Sync: (BOOL)sync
{
  radius = r; 
  nhoodWidth = 2 * radius + 1;
  nhoodType = n;
  edgeWrap = wrap;
  SYNCHRONOUS = sync;


}


- (void)setNhoodRadius: (int)r
{
  radius = r;
  nhoodWidth = 2 * radius + 1;
}


- (void)setNhoodType: (int)n
{
  nhoodType = n;
}




- createNhoodMask
{
  unsigned i, j = 0;

  int
    myabs ( x )
    {
      return (x>0) ? x: -x;
    }
 
  nhoodOffsets = [[self getZone] alloc: nhoodWidth * sizeof(long)];

  for (i = 0; i < nhoodWidth; i++)
    nhoodOffsets[i] = nhoodWidth * i; 


   nhoodInclude = [[self getZone] allocBlock: ( nhoodWidth * nhoodWidth * sizeof(BOOL))];

  if (nhoodType == 1) //von neuman neighborhood;
    {
      for (i = 0; i < nhoodWidth; i++)
	for (j = 0; j < nhoodWidth; j++)
	  {
	    *nhoodMask(i,j) = NO;
	    if ( (myabs(i-radius) + myabs(j-radius)) <= radius ) *nhoodMask(i,j) = YES; 
	   
	  }
    }

  else //moore neighborhood;
    {
      for (i = 0; i < nhoodWidth; i++)
	for (j = 0; j < nhoodWidth; j++)
	  {
	    *nhoodMask(i,j) = YES;
	  }
    }


  fprintf (stderr, "Here is the nhood matrix \n");
  for (i = 0; i < nhoodWidth; i++)
    {
      for (j = 0; j < nhoodWidth; j++)
	{
	  fprintf (stderr, "%d ", *nhoodMask(i,j));
	}
      fprintf (stderr, "\n");
    }
  return self;
}


- (BOOL *)getNhoodMaskPtr
{
  return nhoodInclude;
}



- (long *)getNhoodOffsets
{
  return nhoodOffsets;
}



- createStorage
{
  unsigned i = 0;
  
  gridOffsets = [[self getZone] alloc: ysize * sizeof (*gridOffsets)];
  for (i = 0; i < ysize; i++)
    gridOffsets[i] = xsize * i; 

  visible = [[self getZone] allocBlock: (xsize * ysize * sizeof(long))];
  memset (visible, 0, xsize * ysize * sizeof (long));
    
  change = [[self getZone] allocBlock: (xsize * ysize * sizeof(long))];
  memset (change, 0, xsize * ysize * sizeof (long)); 

  count = [[self getZone] allocBlock: (xsize * ysize * sizeof(long))];
  memset (count, 0, xsize * ysize * sizeof (long));

  return self;
}








- createEnd
{

  [self createNhoodMask];
  [self createStorage];
  return [super createEnd];
}





//pj-2001-05-02 The stepRule only needs to be called for a SYNCHRONOUS model,
//because that one accumulates the changes and pushes them all through at once.
//The ASYNCHRONOUS alternative flushes through each agent's action immediately.

- stepRule
{
  unsigned int xx, yy;
  
  if (SYNCHRONOUS != YES) return self;
  else
    {


      for (xx = 0; xx < xsize; xx++)
	for (yy = 0; yy < ysize; yy++)
	  {
	    
	    int  intlevel = 0;
	    
	    if ( (intlevel = *D2D(change,gridOffsets,xx,yy) ) != 0)
	      {
		[self updateIntPtr: visible Impact: intlevel AtX: xx Y: yy ];
		*D2D(count,gridOffsets,xx,yy) += intlevel;
		totalCount += intlevel;
	       
	      }
	    
	    
	    //clear off the accumulator
	    *(D2D(change, gridOffsets, xx, yy)) = 0;
	  }
      
    }  
  return self;
}
 

- (long)getCountAtX: (unsigned)x Y: (unsigned)y
{
  return totalCount;
}


- (long)getNumVisibleAtX: (unsigned)x Y: (unsigned)y
{
  return *(D2D(visible,gridOffsets,x,y));
}


- (long*)getPointer
{
  return visible;
}


- (long *)getGridOffsets
{
  return gridOffsets;
}



- (long)updateIntPtr: (long int*)p Impact: (int)input AtX: (unsigned)x Y: (unsigned)y
{
  unsigned int r, c; 
  int xcoord = 0, ycoord = 0;
  long value = 0;
  
  for (r = 0; r < nhoodWidth; r++)
    {
      BOOL outOfBoundsX = NO, outOfBoundsY = NO;
      xcoord = x+r-radius;
      
      outOfBoundsX = (xcoord < 0) || (xcoord > (int)xsize - 1);
      if (outOfBoundsX)
	{
	  if (edgeWrap == NO) continue;
	  else xcoord = (xcoord + xsize)%xsize;
	}
      for (c = 0; c < nhoodWidth; c++)
	{
	  if (*nhoodMask(r,c) == NO ) continue;
	  ycoord = y+c-radius;
	  outOfBoundsY =  (ycoord < 0)||(ycoord > (int)ysize-1);
	  if (outOfBoundsY)
	    {
	      if (edgeWrap == NO) continue; //if outside bounds, then quit here
	      else ycoord = (ycoord + ysize) % ysize;
	    }
	  value = *(D2D(p,gridOffsets,xcoord,ycoord)) += input;
	}
    }
  return value;
}


- (long)changeValue: (int)value AtX: (unsigned)x Y: (unsigned)y
{
  if (SYNCHRONOUS)
    {
      *D2D(change,gridOffsets,x,y) += value;
    }
  else
    {
      value = [self updateIntPtr: visible Impact: value AtX: x Y: y];
      *D2D(count,gridOffsets,x,y) += value;
      totalCount += value;
    }
  
  return value ;    
}


- (long)addOneAtX: (unsigned)x Y: (unsigned)y 
{
  long retval = [self changeValue: 1L AtX: x Y: y];
  return retval;
}


- (long)removeOneAtX: (unsigned)x Y: (unsigned)y 
{
  long retval = [self changeValue: -1L AtX: x Y: y];
  return retval;
}


- (void)printDiagnostics
{
  unsigned int i, j;
 
  for (i = 0; i < xsize; i++)
    {
      for(j=0; j < ysize; j++)
	{
	  fprintf(stderr,"%d ,%d: %ld,%ld \n",i,j,
		  *D2D(visible,gridOffsets,i,j),*D2D(change,gridOffsets,i,j)); 
	}
      fprintf(stderr,"\n");
    }
  fprintf(stderr,"\n");
}






- (unsigned)wrapCoord: (unsigned)inCoord atModulus: (unsigned)inModulus
{
  if (inCoord < 0 || inCoord >= inModulus)
    { 
      inCoord = (inCoord + inModulus) % inModulus; 
    }
  return inCoord;
}

- (unsigned)wrapXCoord: (unsigned)inCoord
{
  return [self wrapCoord: inCoord atModulus: xsize];
}

- (unsigned)wrapYCoord: (unsigned)inCoord
{
  return [self wrapCoord: inCoord atModulus: ysize];
}



@end


