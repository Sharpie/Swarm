// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc.h>
#import <objectbase.h>
#import <space.h>

// generic object to handle filing of 2d objects.
// Tell it what message to send, and it sends it.
// also knows how to construct probes.

@interface Int2dFiler: SwarmObject 
{
  Discrete2d *discrete2d;
  SEL valueMessage;
  int background;
}

- setDiscrete2dToFile: (Discrete2d *)aSpace;
- setValueMessage: (SEL)aSelector;
- setBackground: (int)aValue;
- fileTo: (const char *)fileName;

@end
