// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <space.h> // Grid2d
#import <space/Discrete2d.h>

@interface Grid2d: Discrete2d <Grid2d>
{
  BOOL overwriteWarnings;
}

+ create: aZone setSizeX: (unsigned)x Y: (unsigned)y;
- setOverwriteWarnings: (BOOL)b;

@end
