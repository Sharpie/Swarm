// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <space/Grid2d.h>

// this currently only allows one occupant per cell. Bad thing.

@implementation Grid2d

+createBegin: (id) aZone {
  id r;
  r = [super createBegin: aZone];
  [r setOverwriteWarnings: 1];
  return r;
}

// Note - we don't use the superclass method, we write our own.
-putObject: anObject atX: (int) x Y: (int) y {
  // this warning isn't such a great idea, maybe. We need a better
  // 2d space object, undoubtedly.
  id objectThere;
  objectThere = *discrete2dSiteAt(lattice, offsets, x, y);

  if (overwriteWarnings &&
      (anObject != nil) && 
      (objectThere != nil)) {
    [WarningMessage raiseEvent: "Grid2d: you're overwriting object %x at (%d,%d) with object %x. Grid2d does not support two objects in one place.\n", objectThere, x, y, anObject];
  }

  *discrete2dSiteAt(lattice, offsets, x, y) = anObject;
  return self;
}

-setOverwriteWarnings: (BOOL) b {
  overwriteWarnings = b;
  return self;
}

@end
