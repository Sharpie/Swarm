// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Generic support for 2d discrete lattices.
// Each point in the lattice stores either an id or a integer.
// Ie: 32 bits per point. This is not 100% general, but is the best
// compromise I know of.

// See FUTURE-DESIGN for notes on where this is going.

#import <objectbase/SwarmObject.h>
#import <space.h>

@interface Discrete2d: SwarmObject <_Discrete2d>
{
@public
  unsigned xsize, ysize;
  id *lattice;
  long *offsets;
}

- setSizeX: (unsigned)x Y: (unsigned)y;
- createEnd;
- makeOffsets;
- (id *)allocLattice;

- (unsigned)getSizeX;
- (unsigned)getSizeY;

- getObjectAtX: (unsigned)x Y: (unsigned)y;
- (long)getValueAtX: (unsigned)x Y: (unsigned)y;

- putObject: anObject atX: (unsigned)x Y: (unsigned)y;
- putValue: (long)v atX: (unsigned)x Y: (unsigned)y;

- fastFillWithValue: (long)aValue;
- fastFillWithObject: anObj;

- fillWithValue: (long)aValue;
- fillWithObject: anObj;

- (id *)getLattice;
- (long *)getOffsets;

- copyDiscrete2d: a toDiscrete2d: b;
- (int)setDiscrete2d: a toFile: (const char *)filename;
@end

// fast macro to access lattice array. Use this cautiously.
// We define this here to allow library authors to get around the
// getFooAtX:Y: and setFooAtX:Y: methods. This plays havoc with
// inheritance, of course.
#define discrete2dSiteAt(l, offsets, x, y) ((l) + (offsets)[(y)] + (x))
