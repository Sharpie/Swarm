// Heatbugs application. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Bits to support a specialization of diffusion objects: "heat space".
// Most of the real work is done in Diffuse, which implements a CA.
// These functions simplify and stereotype access to the space variable,
// making the Heatbug code higher level.

#import "HeatSpace.h"

// global constant: maximum heat.
// This could just be used from the Diffuse2d object's max states.

const HeatValue maxHeat = 0x7fff;

@implementation HeatSpace

// Add heat to the current spot. This code checks the bounds on maxHeat,
// pegs value at the top.

-addHeat: (HeatValue) moreHeat X: (int) x Y: (int) y {
  HeatValue heatHere;

  heatHere = [self getValueAtX: x Y: y];	  // read the heat
  if (moreHeat <= maxHeat - heatHere)             // would add be too big?
    heatHere = heatHere + moreHeat;		  //   no, just add
  else
    heatHere = maxHeat;				  //   yes, use max
  [self putValue: heatHere atX: x Y: y];	  // set the heat.
  return self;
}

// Search the 9 cell neighbourhood for the requested extreme (cold or hot)
// The X and Y arguments are used both as input (where to search from)
// and as output (pointers are filled with the coordinate of the extreme).
// Note that wraparound edges (boundary conditions) are implicitly in
// the code - look at the call to [self getCellX:Y:].

-(HeatValue) findExtremeType: (HeatExtremeType) type X: (int *) px Y: (int *) py {
  HeatValue bestHeat;
  int x, y;
  int bestX, bestY;

  // prime loop: assume extreme is right where we're standing

  bestX = *px;
  bestY = *py;
  bestHeat = [self getValueAtX: bestX Y: bestY];

  // Now scan through the world, finding the best cell in the 8 cell nbd.
  // Note that this is slightly biased: if two cells have the same
  // best heat, then the one more to the top (or left) is preferred.
  // To do this exactly, you have to keep a list of all best ones and
  // then choose a random element.

  for (x = *px - 1; x <= *px + 1; x++) {
    for (y = *py - 1; y <= *py + 1; y++) {
      HeatValue heatHere;
      BOOL hereIsBetter;

      heatHere = [self getValueAtX: (x+xsize)%xsize Y: (y+ysize)%ysize];
      hereIsBetter = (type == cold) ? (heatHere < bestHeat)
	                            : (heatHere > bestHeat);
      if (hereIsBetter) {			  // this spot more extreme
	bestHeat = heatHere;			  // update information
	bestX = x;
	bestY = y;
      }
    }
  }

  // Now we've found the requested extreme. Arrange to return the
  // information (normalize coordinates), and return the heat we found.

  *px = (bestX + xsize) % xsize;
  *py = (bestY + ysize) % ysize;

  return bestHeat;
}

@end
