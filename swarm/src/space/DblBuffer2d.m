// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <space/DblBuffer2d.h>
#include <misc.h> // memcpy

// general plan:
// wBuf1 and wBuf2 always point to the allocated storage.
// lattice points to the current valid state of the lattice, suitable
//   for reading.
// newLattice points to the (partially constructed) next lattice state,
//   suitable only for writing.
// [DblBuffer2d updateLattice] sets lattice to be what newLattice was.
//
// Summary: only *read* from lattice, only *write* to newLattice.

@implementation DblBuffer2d

PHASE(Creating)

+ create: aZone setSizeX: (unsigned)x Y: (unsigned)y
{
  return [super create: aZone setSizeX: x Y: y];
}

// allocate buffers.
- createEnd
{
  if (xsize == 0 || ysize == 0)
    [InvalidCombination
      raiseEvent: "DblBuffer2d not initialized correctly.\n"];
  
  // should call parent class of our parent's createEnd
  wBuf1 = [self allocLattice];
  wBuf2 = [self allocLattice];
  lattice = wBuf1;
  newLattice = wBuf2;

  [self makeOffsets];
  return self;
}

PHASE(Setting)
PHASE(Using)

- (id *)getNewLattice
{
  return newLattice;
}

// swap lattice pointers around (see comment at top of file.)
- updateLattice
{
  if (lattice == wBuf1 && newLattice == wBuf2)
    {
      lattice = wBuf2;
      newLattice = wBuf1;
  }
  else if (lattice == wBuf2 && newLattice == wBuf1)
    {
      lattice = wBuf1;
      newLattice = wBuf2;
    }
  else
    {
      [WarningMessage
        raiseEvent:
          "DblBuffer2d: Sanity check failed when swapping lattice pointers!\n"];
      return nil;
    }

  // after update, make newLattice == lattice. This handles cases where people
  // want to only modify parts of newLattice and then call updateLattice.
  memcpy (newLattice, lattice, xsize * ysize * sizeof (*lattice));

  return self;
}

// override puts to use new lattice, not old.
- putObject: anObject atX: (unsigned)x Y: (unsigned)y
{
  *discrete2dSiteAt (newLattice, offsets, x, y) = anObject;
  return self;
}

// override puts to use new lattice, not old.
- putValue: (long)v atX: (unsigned)x Y: (unsigned)y
{
  *discrete2dSiteAt (newLattice, offsets, x, y) = (id)v;
  return self;
}

- (void)drop
{
  [[self getZone] free: newLattice];
  [super drop];
}

@end
