// Swarm library. Copyright © 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <space.h> // Int2dFiler, Discrete2d
#import <objectbase/SwarmObject.h>

// generic object to handle filing of 2d objects.
// Tell it what message to send, and it sends it.
// also knows how to construct probes.

@interface Int2dFiler: SwarmObject <Int2dFiler>
{
  id <Discrete2d> discrete2d;
  SEL valueMessage;
  int background;
}

- setDiscrete2dToFile: aSpace;
- setValueMessage: (SEL)aSelector;
- setBackground: (int)aValue;
- fileTo: (const char *)fileName;

@end
