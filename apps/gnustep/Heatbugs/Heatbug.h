// Heatbugs application. Copyright � 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Heatbug, a simple Swarm agent.

#import <space.h>				  // we use Space features
#if 0
#import <gui.h>
#endif
#import "HeatSpace.h"				  // we also have a heat object
#include <AppKit/AppKit.h>
#import "HeatbugsController.h"

// The definition of a Heatbug object. We inherit code from the generic
// SwarmObject, which provides memory allocation and other niceties. It
// does not provide any sort of agent behaviour, though, that's up to us.
// First, heatbugs have a lot of state variables:

@interface Heatbug: SwarmObject
{
  double unhappiness;				  // my current unhappiness
  int x, y;					  // my spatial coordinates
  HeatValue idealTemperature;			  // my ideal temperature
  HeatValue outputHeat;				  // how much heat I put out
  float randomMoveProbability;			  // chance of moving randomly
  
  id <Grid2d> world;				  // the world I live in
  int worldXSize, worldYSize;			  // how big that world is
  HeatSpace *heat;				  // the heat for the world
#ifndef GNUSTEP
  Color bugColor;				  // my colour (display)
#else
  HeatbugsController *theController;
#endif
}

// these methods are used to initialize the object's state. First,
// methods that have to be sent to create an object.

- setWorld: (id <Grid2d>)w Heat: (HeatSpace *)h;   // which world are we in?

// As a convention, we put the -createEnd here to indicate that we're done
// defining new methods that are required before createEnd.

- createEnd;

// This method reads an aspect of the bug's state. You could also get
// the same information via a probe.

- (double)getUnhappiness;

// Other methods to set Heatbug state. These can be called after the
// object has been fully created.

- setIdealTemperature: (HeatValue)i;
- setOutputHeat: (HeatValue)o;
- setRandomMoveProbability: (float)p;
- setX: (int)x Y: (int)y;			  // bug's position
#ifndef GNUSTEP
- setBugColor: (Color)c;			  // bug's colour (display)
#else
- (void)setController: (HeatbugsController *)aController;
#endif

// The major heatbug behaviour: do one "time step" (all bug action).

- step;

// extra display code (heatbugs currently draw themselves)
#ifndef GNUSTEP
- drawSelfOn: (id <Raster>)r;
#else
- drawSelfOn: (NSImage *)anImage;
#endif

@end

