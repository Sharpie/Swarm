// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <space/DblBuffer2d.h>

// general plan:
// wBuf1 and wBuf2 always point to the allocated storage.
// lattice points to the current valid state of the lattice, suitable
//   for reading.
// newLattice points to the (partially constructed) next lattice state,
//   suitable only for writing.
// [Ca2d updateLattice] sets lattice to be what newLattice was.
//
// Summary: only *read* from lattice, only *write* to newLattice.


//S: A double buffered space.
//D: DblBuffer2d augments Discrete2d to provide a form of double buffered
//D: space. Two lattices are maintained: lattice (the current state), and
//D: newLattice (the future state). All reads take place from lattice, all
//D: writes take place to newLattice. newLattice is copied to lattice when
//D: updateLattice is called.  DblBuffer2d can be used to implement one
//D: model of concurrent action, like in Ca2ds.  NOTE: be very careful if
//D: you're using low-level macro access to the world, in particular be
//D: sure that you preserve the write semantics on the newLattice.

@implementation DblBuffer2d

// allocate buffers.
//M: Rewrites the method from Discrete2d. Allocate two lattices,
//M: makes the offsets.
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

//M: Return a pointer to the newLattice buffer.
- (id *)getNewLattice
{
  return newLattice;
}

// swap lattice pointers around (see comment at top of file.)
//M: Copy newLattice to lattice, in effect updating the lattice. 
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
          "Ca2d: Sanity check failed when swapping lattice pointers!\n"];
      return nil;
    }

  // after update, make newLattice == lattice. This handles cases where people
  // want to only modify parts of newLattice and then call updateLattice.
  memcpy (newLattice, lattice, xsize * ysize * sizeof (*lattice));

  return self;
}

// override puts to use new lattice, not old.
//M: Overridden so writes happen to newLattice.
- putObject: anObject atX: (int)x Y: (int)y
{
  *discrete2dSiteAt (newLattice, offsets, x, y) = anObject;
  return self;
}

// override puts to use new lattice, not old.
//M: Overridden so writes happen to newLattice.
- putValue: (long)v atX: (int)x Y: (int)y
{
  *discrete2dSiteAt (newLattice, offsets, x, y) = (id)v;
  return self;
}

@end
