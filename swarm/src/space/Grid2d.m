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

#import <space/Grid2d.h>

// This currently only allows one occupant per cell.  Bad thing.

@implementation Grid2d

PHASE(Creating)

// Make overwrite warnings be on by default.
+ createBegin: aZone
{
  Grid2d *r = [super createBegin: aZone];
  
  r->overwriteWarnings = YES;
  
  return r;
}

+ create: aZone setSizeX: (unsigned)x Y: (unsigned)y
{
  return [super create: aZone setSizeX: x Y: y];
}


PHASE(Setting)
PHASE(Using)

// Note - we don't use the superclass method, we write our own.
- putObject: anObject atX: (unsigned)x Y: (unsigned)y
{
  // This warning isn't such a great idea, maybe. We need a better
  // 2d space object, undoubtedly.
  id objectThere = *discrete2dSiteAt(lattice, offsets, x, y);

  if (overwriteWarnings &&
      (anObject != nil) && 
      (objectThere != nil))
    [WarningMessage
      raiseEvent: 
        "Grid2d: you're overwriting object %x at (%d,%d) with object %p (%s).\n"
      "Grid2d does not support two objects in one place.\n",
      objectThere, x, y, anObject, [anObject getName]];
  
  *discrete2dSiteAt(lattice, offsets, x, y) = anObject;

  return self;
}

- setOverwriteWarnings: (BOOL)b
{
  overwriteWarnings = b;

  return self;
}

@end
