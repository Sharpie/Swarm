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

// Generic support for 2d discrete lattices.
// Each point in the lattice stores either an id or a integer.
// Ie: 32 bits per point. This is not 100% general, but is the best
// compromise I know of.

// See FUTURE-DESIGN for notes on where this is going.

#import <Swarm/SwarmObject.h>
#import <Swarm/space.h>

@interface Discrete2d: SwarmObject <Discrete2d>
{
  BOOL objectFlag;
@public
  unsigned xsize, ysize;
  id *lattice;
  long *offsets;
}

+ create: (id <Zone>)aZone setSizeX: (unsigned)x Y: (unsigned)y;
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

- setLattice: (id *)lattice;
- (void)setObjectFlag: (BOOL)objectFlag;

- (id *)getLattice;
- (long *)getOffsets;

- copyDiscrete2d: (id <Discrete2d>)a toDiscrete2d: (id <Discrete2d>)b;
- (int)setDiscrete2d: (id <Discrete2d>)a toFile: (const char *)filename;

- hdf5InCreate: hdf5Obj;
- hdf5In: hdf5Obj;
- (void)hdf5OutShallow: hdf5Obj;
- (void)hdf5OutDeep: (id <OutputStream>)hdf5Obj;
- (void)lispOutShallow: (id <OutputStream>)stream;
- (void)lispOutDeep: stream;
@end

// fast macro to access lattice array. Use this cautiously.
// We define this here to allow library authors to get around the
// getFooAtX:Y: and setFooAtX:Y: methods. This plays havoc with
// inheritance, of course.
#define discrete2dSiteAt(l, offsets, x, y) ((l) + (offsets)[(y)] + (x))
