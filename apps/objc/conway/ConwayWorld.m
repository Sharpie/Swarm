
#import "ConwayWorld.h"
#import "ObserverSwarm.h"  //for observer

@implementation ConwayWorld

- setObserver: anObject
{
  observer= anObject;
  return self;
}


- swapColorAtX: (unsigned) x Y: (unsigned) y
{

  long int newState;
  long int oldState = [self getValueAtX: x Y: y];

  newState = (oldState == 1 ) ? 0 : 1;

  [self putValue: newState atX: x Y: y];

  // DEBUG
  //fprintf(stderr,"Conway at x,y (%d,%d),oldState=%ld,newState=%ld \n",x,y,oldState,newState);

  [observer updateGUI];

  return self;
}


- eraseAll
{
  unsigned int x, y;

  for (x = 0; x < xsize; x++)
    for (y = 0; y < ysize; y++)
      {
	[self putValue: 0L atX: x Y: y];
      }

  return self;
}


//This stepRule is the one used in Swarm's ConwayLife2d class.
//I'm copying it here so we can edit, revise, replace, etc.

- stepRule
{
  long newState;
  unsigned x, y;

  for (x = 0; x < xsize; x++)
    for (y = 0; y < ysize; y++)
      {
        unsigned sum;
        unsigned xm1, xp1, ym1, yp1;

        sum = 0;

	//pj 2003-02-2: for speed, I'm replacing the slow use of %
	//xm1 = (x + xsize - 1) % xsize;
	if (x == 0) xm1 = xsize - 1;
	else xm1 = x - 1;
 
        //xp1 = (x + 1) % xsize;
	if (x >= xsize -1) xp1 = 0;
	else xp1 = x + 1;

	//ym1 = (y + ysize - 1) % ysize;
	if (y <= 0) ym1 = ysize -1;
	else ym1 = y -1;
	
        //yp1 = (y + 1) % ysize;
	if (y >= ysize -1) yp1 = 0;
	else yp1 = y + 1;

        sum += [self getValueAtX: xm1 Y: ym1];
        sum += [self getValueAtX: x   Y: ym1];
        sum += [self getValueAtX: xp1 Y: ym1];

        sum += [self getValueAtX: xm1 Y: y];
        sum += [self getValueAtX: xp1 Y: y];

        sum += [self getValueAtX: xm1 Y: yp1];
        sum += [self getValueAtX: x   Y: yp1];
        sum += [self getValueAtX: xp1 Y: yp1];

        if ([self getValueAtX: x Y: y] == 1)
          newState = (sum == 2 || sum == 3) ? 1 : 0;
        else
          newState = (sum == 3) ? 1 : 0;

        [self putValue: newState atX: x Y: y];
      }

  [self updateLattice];
  return self;
}

@end
