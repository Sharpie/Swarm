// Mousetraps application. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import "Mousetrap.h"
#import <tkobjc.h>
#import <simtools.h>
#import "MousetrapModelSwarm.h"

@implementation Mousetrap

+create: aZone setModelSwarm:  s setXCoord: (int)x setYCoord: (int)y {
  Mousetrap *newTrap;

  newTrap = [self create: aZone];
  newTrap->modelSwarm = s;
  newTrap->xCoord = x;
  newTrap->yCoord = y;
  return newTrap;
}

// Mousetraps draw themselves during the trigger event. Kind of messy
// for a functional split, but it's efficient.
-setDisplayWidget: w {
  displayWidget = w;
  return self;
}

-trigger {
  int n, xTrigger, yTrigger;
  unsigned triggerTick;

  // take this ball out of the air
  [[modelSwarm getStats] removeOneBall];

  // calculate whether we actually fire in response to the event.
  if (!triggered &&
      ([modelSwarm getTriggerLikelihood] >= 1.0 ||
       [uniformRandom rFloat] < [modelSwarm getTriggerLikelihood])) {

    // mark ourselves as triggered
    triggered = 1;

    // keep stats, draw it on the screen
    [[modelSwarm getStats] addOneTriggered];
    if (displayWidget)
      [displayWidget drawPointX: xCoord Y: yCoord Color: 2];

    // now schedule trigger events for neighbours.
    for ( n = [modelSwarm getNumberOutputTriggers]; n > 0; n-- ) {
      int size, maxD;
      Mousetrap * trap;

      // Find a nearby trap
      size = [modelSwarm getGridSize];
      maxD = [modelSwarm getMaxTriggerDistance];
      xTrigger = (xCoord + size + [uniformRandom rMin: -maxD Max: maxD + 1]) % size;
      yTrigger = (yCoord + size + [uniformRandom rMin: -maxD Max: maxD + 1]) % size;

      // schedule a trigger for it in the future.
      triggerTick = getCurrentTime() + [uniformRandom rMin: 1 Max: [modelSwarm getMaxTriggerTime]];

      trap = [modelSwarm getMousetrapAtX: xTrigger Y: yTrigger];
      if (trap) {
	// Add a new ball in the air.
	[[modelSwarm getStats] addOneBall];
	[modelSwarm scheduleTriggerAt: triggerTick For: trap];
      }
    }
  }
  return self;
}

// recover the trap
-reload {
  triggered = 0;
  if (displayWidget)
    [displayWidget drawPointX: xCoord Y: yCoord Color: 1];
  return self;
}

@end
