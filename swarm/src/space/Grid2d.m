// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <space/Grid2d.h>

// This currently only allows one occupant per cell.  Bad thing.

//S: A 2d container class for agents.
//D: Grid2d is a generic container class to represent agent position on
//D: a 2d lattice.
//D: It gets most of its behaviour from Discrete2d, adding extra code to check
//D: that you don't overwrite things by accident. Grid2d is pretty primitive:
//D: only one object can be stored at a site, no boundary conditions are
//D: implied, etc.

@implementation Grid2d

//M: Make overwrite warnings be on by default.
+ createBegin: aZone
{
  id r = [super createBegin: aZone];

  [r setOverwriteWarnings: 1];

  return r;
}

// Note - we don't use the superclass method, we write our own.
//M: Replaces the Discrete2d method.
//M: First check to see if it should do overwrite warnings, and if so
//M: if you're going to overwrite: if both conditions are true,
//M: print out a warning message.
//M: Regardless of the check, it writes the new object in. 
- putObject: anObject atX: (int)x Y: (int)y
{
  // This warning isn't such a great idea, maybe. We need a better
  // 2d space object, undoubtedly.
  id objectThere = *discrete2dSiteAt(lattice, offsets, x, y);

  if (overwriteWarnings &&
      (anObject != nil) && 
      (objectThere != nil))
    [WarningMessage
      raiseEvent: 
        "Grid2d: you're overwriting object %x at (%d,%d) with object %x.\n"
      "Grid2d does not support two objects in one place.\n",
      objectThere, x, y, anObject];
  
  *discrete2dSiteAt(lattice, offsets, x, y) = anObject;
  return self;
}

//M: If set to true, then if you try to store something at a site that
//M: doesn't have 0x0 there, a warning will be generated. 
- setOverwriteWarnings: (BOOL)b
{
  overwriteWarnings = b;
  return self;
}

@end
