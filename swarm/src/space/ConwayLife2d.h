// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Conway's game of life, simple test of Ca2d.

#import <space.h> // ConwayLife2d
#import <space/Ca2d.h>

@interface ConwayLife2d: Ca2d <ConwayLife2d>
{
}
+ createBegin: aZone;
- initializeLattice;
- stepRule;
@end

