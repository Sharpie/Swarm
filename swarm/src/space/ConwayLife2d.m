// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <space/ConwayLife2d.h>
#import <random.h>

@implementation ConwayLife2d

PHASE(Creating)

+ createBegin: aZone
{
  id r;

  r = [super createBegin: aZone];
  [r setNumStates: 2];
  return r;
}  

- initializeLattice
{
  unsigned x, y;

  for (y = 0; y < ysize; y++)
    for (x = 0; x < xsize; x++)
      {
        long newState;

        newState = [uniformIntRand getIntegerWithMin: 0L withMax: 2L] == 2;
        [self putValue: newState atX: x Y: y];
      }
  [self updateLattice];
  return self;
}

PHASE(Using)

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
        xm1 = (x + xsize - 1) % xsize;
        xp1 = (x + 1) % xsize;
        ym1 = (y + ysize - 1) % ysize;
        yp1 = (y + 1) % ysize;
        
#ifndef NODIRECTACCESS
        sum += (long) *(discrete2dSiteAt(lattice, offsets, xm1, ym1));
        sum += (long) *(discrete2dSiteAt(lattice, offsets, x, ym1));
        sum += (long) *(discrete2dSiteAt(lattice, offsets, xp1, ym1));
        
        sum += (long) *(discrete2dSiteAt(lattice, offsets, xm1, y));
        sum += (long) *(discrete2dSiteAt(lattice, offsets, xp1, y));
        
        sum += (long) *(discrete2dSiteAt(lattice, offsets, xm1, yp1));
        sum += (long) *(discrete2dSiteAt(lattice, offsets, x, yp1));
        sum += (long) *(discrete2dSiteAt(lattice, offsets, xp1, yp1));
#else
        sum += [self getValueAtX: xm1 Y: ym1];
        sum += [self getValueAtX: x   Y: ym1];
        sum += [self getValueAtX: xp1 Y: ym1];
        
        sum += [self getValueAtX: xm1 Y: y];
        sum += [self getValueAtX: xp1 Y: y];
        
        sum += [self getValueAtX: xm1 Y: yp1];
        sum += [self getValueAtX: x   Y: yp1];
        sum += [self getValueAtX: xp1 Y: yp1];
#endif      
        
        if ([self getValueAtX: x Y: y] == 1)
          newState = (sum == 2 || sum == 3) ? 1 : 0;
        else
          newState = (sum == 3) ? 1 : 0;
        
#ifndef NODIRECTACCESS
        *(discrete2dSiteAt(newLattice, offsets, x, y)) = (id) newState;
#else
        [self putValue: newState atX: x Y: y];
#endif
      }
  [self updateLattice];
  return self;
}

@end
