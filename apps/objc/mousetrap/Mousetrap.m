// Mousetraps application. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Mousetraps are, well, mousetraps that live in fixed positions on
// a 2D lattice. Each mousetrap has N "ping-pong" balls. When "triggered"
// (hit by another ping-pong ball) it releases its ping-pong balls
// into the air, which hit other mousetraps and trigger them.
// Tossing ping-pong balls into the air to trigger other mousetraps
// is accomplished by picking "nearby" mousetraps, and arranging for
// them to be sent trigger messages in the "near" future.

// This is a discrete-event model of object updates in time.
// The discrete event model is implemented via *dynamic scheduling*

#import "Mousetrap.h"
#import <simtools.h>
#import "MousetrapModelSwarm.h"

@implementation Mousetrap

  // Here, we just use the simplified create message instead of
  // the more general createBegin/createEnd pair.
  // During create, we initialize a mousetraps crucial state

+ create: aZone setModelSwarm: s setXCoord: (int)x setYCoord: (int)y setGenerator: randGen
{
  Mousetrap *newTrap;
  int maxD;

  newTrap = [self create: aZone];
  newTrap->modelSwarm = s;
  newTrap->xCoord = x;
  newTrap->yCoord = y;

  // create the distributions to be used
  maxD = [newTrap->modelSwarm getMaxTriggerDistance];
  newTrap->uniform0to1 =
    [UniformDouble create: [newTrap->modelSwarm getZone]
		   setGenerator: randGen
		   setDoubleMin: (double) 0.0L
		   setMax: (double) 1.0L];
  newTrap->uniformRadius =
    [UniformInteger create: [newTrap->modelSwarm getZone]
		    setGenerator: randGen
		    setIntegerMin: -maxD
		    setMax: maxD];

  newTrap->uniformTrigTime =
    [UniformUnsigned create: [newTrap->modelSwarm getZone]
		     setGenerator: randGen
		     setUnsignedMin: 1L
		     setMax: [newTrap->modelSwarm getMaxTriggerTime]];

  return newTrap;
}

// Mousetraps draw themselves during the trigger event. Kind of messy
// for a functional split, but it's efficient.

// Here we record who to display ourselves on.

- setDisplayWidget: w
{
  displayWidget = w;
  return self;
}

#ifdef SCHEDULE_INSPECTION
- setScheduleItem: w
{
  scheduleItem = w;
  return self;
}
#endif

 // The crucial step for a Mousetrap (equivalent to "step" for a Heatbug)

- trigger
{
  int n, xTrigger, yTrigger;
  unsigned triggerTick;
  
  // We've been "hit" by a trigger message (ping-pong ball)
  
  // First, take this ball out of the air for stats reporting

  [[modelSwarm getStats] removeOneBall];

  // If we have not already been triggered (i.e., if triggered = 0)
  // calculate whether we actually fire in response to the event.

  if (!triggered
      && ([modelSwarm getTriggerLikelihood] >= 1.0 ||
          (float)[uniform0to1 getDoubleSample] < 
          [modelSwarm getTriggerLikelihood]))
    {
      
      int size;
      
      // mark ourselves as triggered, do book-keeping, and draw 
      // ourselves as triggered on the display widget
      
      triggered = YES;
      
      [[modelSwarm getStats] addOneTriggered];
      
      if (displayWidget)
        {
#ifdef SCHEDULE_INSPECTION
          [scheduleItem trigger: displayWidget X: xCoord Y: yCoord];
#endif
          [displayWidget drawPointX: xCoord Y: yCoord Color: 2];
        }
      
      // now schedule trigger events for neighbours.
      // We have n ping-pong balls to toss into the air, so we
      // need to pick n other mousetraps "nearby" and arrange for
      // them to be sent trigger messages in the "near" future.
      // "near" in both cases is set by parameters.
      
      size = [modelSwarm getGridSize];
      for (n = [modelSwarm getNumberOutputTriggers]; n > 0; n--)
        {
          Mousetrap *trap;
          
          // This is where *dynamic scheduling* of actions is prepared for
          
          // First, find a nearby trap - done by picking a random X,Y location near us
          // Note how wraparound is handled by adding "size" and then taking
          // the result modulo "size".
          
          xTrigger =
            (xCoord + size + [uniformRadius getIntegerSample]) % size;
          yTrigger =
            (yCoord + size + [uniformRadius getIntegerSample]) % size;
          
          // Then, decide on a "nearby" time in the future to fire the selected trap
          
          triggerTick = getCurrentTime() + [uniformTrigTime getUnsignedSample];
          
          // Now, we get the id of the trap at our randomly chosen X,Y position
          
          trap = [modelSwarm getMousetrapAtX: xTrigger Y: yTrigger];
          
          if (trap)
            {
              
              // If there was a trap at those X,Y coordinates:
              // First, we add a new "ball" in the air, for statistics purposes
              // Then, we tell our modelSwarm to schedule a trigger message
              // to be sent to that mousetrap at the selected time.
              // This is where *dynamic scheduling* is actually accomplished
              
              [[modelSwarm getStats] addOneBall];
#ifdef SCHEDULE_INSPECTION
              [scheduleItem at: triggerTick
                            owner: trap
                            widget: displayWidget
                            x: xCoord y: yCoord];
#endif
              [modelSwarm scheduleTriggerAt: triggerTick For: trap];
            }
        }
    }
  
  // That's all!
  
  // If we were triggered already, or if there was no mousetrap at the
  // randomly selected positions, we simply return without doing
  // anything except for statistics bookeeping (we "absorb" a ball
  // that landed on us, and, if we were not already triggered, we
  // "toss" N balls into the "air"  - we did all this in the code
  // above.) 
  
  return self;
}


@end
