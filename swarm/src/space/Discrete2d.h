// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Generic support for 2d discrete lattices.
// Each point in the lattice stores either an id or a integer.
// Ie: 32 bits per point. This is not 100% general, but is the best
// compromise I know of.

// See FUTURE-DESIGN for notes on where this is going.

#import <swarmobject.h>

@interface Discrete2d: SwarmObject {
@public
  int xsize, ysize;
  id * lattice;
  int * offsets;
}

-setSizeX: (int) x Y: (int) y;
-createEnd;
-makeOffsets;
-(id *)allocLattice;

-(int) getSizeX;
-(int) getSizeY;

-getObjectAtX: (int) x Y: (int) y;
-(int) getValueAtX: (int) x Y: (int) y;

-putObject: anObject atX: (int) x Y: (int) y;
-putValue: (int) v atX: (int) x Y: (int) y;

-(id *) getLattice;
-(int *) getOffsets;
@end

// fast macro to access lattice array. Use this cautiously.
// We define this here to allow library authors to get around the
// getFooAtX:Y: and setFooAtX:Y: methods. This plays havoc with
// inheritance, of course.
#define discrete2dSiteAt(l, offsets, x, y) ((l) + (offsets)[(y)] + (x))
