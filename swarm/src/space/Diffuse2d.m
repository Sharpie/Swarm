// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

#import <space/Diffuse2d.h>
#import <math.h>

// basic CA diffusion.
// newHeat = evap (self + k * (nbdavg - self))
//   where self is our own heat
//   nbdavg is the appropriately weighted average of our neighbour's heat
//   k is the diffusion constant
//   evap is the evaporation rate.

#define MAXSTATES 0x7fff

@implementation Diffuse2d

PHASE(Creating)

+ create: aZone setSizeX: (unsigned)x Y: (unsigned)y setDiffusionConstant: (double)d setEvaporationRate: (double)e
{
  Diffuse2d *obj = [self createBegin: aZone];
  
  obj->xsize = x;
  obj->ysize = y;
  obj->diffusionConstant = d;
  obj->evaporationRate = e;
  obj->numStates = MAXSTATES;

  return [obj createEnd];
}

+ createBegin: aZone
{
  Diffuse2d *r = [super createBegin: aZone];

  r->diffusionConstant = 1.0;
  r->evaporationRate = 1.0;
  r->numStates = MAXSTATES;

  return r;
}

- initializeLattice
{
  unsigned x, y;

  for (y = 0; y < ysize; y++)
    for (x = 0; x < xsize; x++)
      {
        long newState;

#ifdef RANDOMINIT
        newState = (long)[uniformRandom rMax: MAXSTATES];
#else
        newState = 0L;
#endif
        [self putValue: newState atX: x Y: y];
      }
  [self updateLattice];
  return self;
}

PHASE(Setting)

- setDiffusionConstant: (double)d
{
  diffusionConstant = d;

  return self;
}

- setEvaporationRate: (double)e
{
  evaporationRate = e;

  return self;
}

PHASE(Using)

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
        else if (d + 0.5 >= MAXSTATES)
          newState = MAXSTATES;
        else
          newState = (long) floor (d+0.5);	  // round to nearest.
        // can't use rint(), sigh.
        
        *(discrete2dSiteAt (newLattice, offsets, x, y)) = (id)newState;
      }
  [self updateLattice];
  return self;
}

@end
