// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.
#define __USE_FIXED_PROTOTYPES__  // for gcc headers

#import <stdlib.h>
#import <space/Discrete2d.h>
#import <string.h>

// Note - this code assumes that ints can be written in where ids have been
// allocated. It uses casts to do this, and I think is portable to all
// modern architectures.

@implementation Discrete2d

-setSizeX: (int) x Y: (int) y {
  if (lattice)
    [InvalidArgument raiseEvent: "You cannot reset the grid size after creation.\n"];
  xsize = x;
  ysize = y;
  return self;
}

-createEnd {
  if (xsize <= 0 || ysize <= 0)
    [InvalidCombination raiseEvent: "invalid size in creation of Discrete2d\n"];
  lattice = [self allocLattice];
  [self makeOffsets];

  return self;
}

-(id *) allocLattice {
  void * p;
  p = [[self getZone] alloc: xsize * ysize * sizeof(id)];
  memset(p, 0, xsize * ysize * sizeof (id));
  return p;
}

// part of createEnd, really, but separated out for ease of inheritance.
-makeOffsets {
  int i;

  // precalculate offsets based on the y coordinate. This lets
  // us avoid arbitrary multiplication in array lookup.
  offsets = [[self getZone] alloc: ysize * sizeof(*offsets)];

  for (i = 0; i < ysize; i++)
    offsets[i] = xsize * i;                       // cache this multiplaction
  return self;
}

-(int) getSizeX {
  return xsize;
}

-(int) getSizeY {
  return ysize;
}

-getObjectAtX: (int) x Y: (int) y {
  return *discrete2dSiteAt(lattice, offsets, x, y);
}

-(long) getValueAtX: (int) x Y: (int) y {
  return (long) *discrete2dSiteAt(lattice, offsets, x, y);
}

-putObject: anObject atX: (int) x Y: (int) y {
  *discrete2dSiteAt(lattice, offsets, x, y) = anObject;
  return self;
}

-putValue: (long) v atX: (int) x Y: (int) y {
  *discrete2dSiteAt(lattice, offsets, x, y) = (id) v;
  return self;
}

-fastFillWithValue: (long) aValue {
  int i, lcount ;
  
  lcount = xsize * ysize ;

  for(i = 0 ; i < lcount ; i++)
    lattice[i] = (id) aValue ;

  return self ;
}

-fastFillWithObject: anObj {
  int i, lcount ;
  
  lcount = xsize * ysize ;

  for(i = 0 ; i < lcount ; i++)
    lattice[i] = anObj ;

  return self ;
}

-fillWithValue: (long) aValue {
  unsigned x, y;

  for (y = 0; y < ysize; y++)
    for (x = 0; x < xsize; x++)
      [self putValue: aValue atX: x Y: y];

  return self;
}

-fillWithObject: anObj {
  unsigned x, y;

  for (y = 0; y < ysize; y++)
    for (x = 0; x < xsize; x++)
      [self putObject: anObj atX: x Y: y];

  return self;
}

-(id *) getLattice {
  return lattice;
}

-(long *) getOffsets {
  return offsets;
}

@end
