// Mousetraps application. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <activity.h>
#import <objectbase/SwarmObject.h>
#import <random.h>

// The definition of a mousetrap object. We inherit code from the generic
// SwarmObject, which provides memory allocation and other niceties. It
// does not provide any sort of agent behaviour, though, that's up to us.


@interface Mousetrap: SwarmObject
{
  // First, the variables for a mousetrap object
@public
  int xCoord;				// X and Y coordinates
  int yCoord;
  BOOL triggered;			// Triggered state (0=no, 1=yes)
  id displayWidget;			// Where we are displayed
  id modelSwarm;			// our modelSwarm
@private
  id <UniformDouble> uniform0to1;
  id <UniformInteger> uniformRadius;
  id <UniformUnsigned> uniformTrigTime;
}

  // Methods that a mousetrap responds to

+ create: aZone setModelSwarm: modelSwarm setXCoord: (int)x setYCoord: (int)y setGenerator: randGen;
- trigger;
- setDisplayWidget: widget;

@end

