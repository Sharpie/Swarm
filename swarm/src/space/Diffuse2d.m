// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <space/Diffuse2d.h>
#import <math.h>

// basic CA diffusion.
// newHeat = evap (self + k * (nbdavg - self))
//   where self is our own heat
//   nbdavg is the appropriately weighted average of our neighbour's heat
//   k is the diffusion constant
//   evap is the evaporation rate.

//S: 2d difussion with evaporation.
//D: Discrete 2nd order approximation to 2d diffusion with evaporation. 
//D: Math is done in integers on the range [0,0x7fff]. 
@implementation Diffuse2d

const unsigned maxStates = 0x7fff;

//M: Set diffusion constant and evaporation rate to 1.0, numStates to 0x7fff.
+ createBegin: (id) aZone
{
  id r = [super createBegin: aZone];

  [r setDiffusionConstant: 1.0];
  [r setEvaporationRate: 1.0];
  [r setNumStates: maxStates];

  return r;
}

//M: Set the diffusion constant. Values over 1.0 might not be valid.
- setDiffusionConstant: (double)d
{
  diffusionConstant = d;

  return self;
}

//M: Set the evaporation rate. Values over 1.0 don't make much sense
- setEvaporationRate: (double)e
{
  evaporationRate = e;

  return self;
}

//M: Initialize world to 0.
- initializeLattice
{
  unsigned x, y;

  for (y = 0; y < ysize; y++)
    for (x = 0; x < xsize; x++)
      {
        long newState;

#ifdef RANDOMINIT
        newState = (long)[uniformRandom rMax: maxStates];
#else
        newState = 0L;
#endif
        [self putValue: newState atX: x Y: y];
      }
  [self updateLattice];
  return self;
}

//M: Run discrete approximation to diffusion. Roughly, it's 
//M: newHeat = evapRate * (self + diffuseConstant*(nbdavg - self)) 
//M: where nbdavg is the weighted average of the 8 neighbours. 
- stepRule
{
  unsigned x, y;
  
  for (x = 0; x < xsize; x++)
    for (y = 0; y < ysize; y++)
      {
        double delta, d;
        long sum;
        long newState;
        unsigned xm1, xp1, ym1, yp1;
        
        xm1 = (x + xsize - 1) % xsize;
        xp1 = (x + 1) % xsize;
        ym1 = (y + ysize - 1) % ysize;
        yp1 = (y + 1) % ysize;
        
        sum = 0;
#ifdef FIRSTORDER
        // do first order approximation (010 141 010)
        sum += (long) *(discrete2dSiteAt(lattice, offsets, x, ym1));   // north
        sum += (long) *(discrete2dSiteAt(lattice, offsets, x, yp1));   // south
        sum += (long) *(discrete2dSiteAt(lattice, offsets, xm1, y));   // east
        sum += (long) *(discrete2dSiteAt(lattice, offsets, xp1, y));   // west
        sum -= 4 * (long) (*(discrete2dSiteAt(lattice, offsets, x, y))); //self
        delta = (double) sum / 4.0;			  // average
#else
        // second order approximation (141 4G4 141)
        sum += (long) *(discrete2dSiteAt(lattice, offsets, xm1, ym1));     //NW
        sum += 4 * (long) (*(discrete2dSiteAt(lattice, offsets, x, ym1))); //N
        sum += (long) *(discrete2dSiteAt(lattice, offsets, xp1, ym1));     //NE
        sum += 4 * (long) (*(discrete2dSiteAt(lattice, offsets, xm1, y))); //W
        sum += 4 * (long) (*(discrete2dSiteAt(lattice, offsets, xp1, y))); //E
        sum += (long) *(discrete2dSiteAt(lattice, offsets, xm1, yp1));     //SW
        sum += 4 * (long) (*(discrete2dSiteAt(lattice, offsets, x, yp1))); //S
        sum += (long) *(discrete2dSiteAt(lattice, offsets, xp1, yp1));     //SE
        sum -= 20 * (long) (*(discrete2dSiteAt(lattice, offsets, x, y)));//self
        delta = (double) sum / 20.0;
#endif      
      
        // now change my value by that smoothing
        d = ((double)((long)*(discrete2dSiteAt(lattice, offsets, x, y)))) +
          delta * diffusionConstant;
        
        // now do evaporation
        d *= evaporationRate;
        
        // now scale back to state values
        // the rounding in the default case is different from David
        // Hiebeler's original Swarm code.
        if (d < 0)
          newState = 0L;
        else if (d + 0.5 >= maxStates)
          newState = maxStates;
        else
          newState = (long)floor (d+0.5);	  // round to nearest.
        // can't use rint(), sigh.
        
        *(discrete2dSiteAt (newLattice, offsets, x, y)) = (id)newState;
      }
  [self updateLattice];
  return self;
}

@end
