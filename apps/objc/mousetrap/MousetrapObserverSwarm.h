// Mousetraps application. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// The MousetrapObserverSwarm is a swarm of objects set up to observe a
// Mousetrap model when the graphical interface is running. The most
// important object is the MousetrapModelSwarm, but we also have
// graphical windows and data analysis and stuff.

#import <swarmobject.h>
#import <space.h>
#import <activity.h>
#import <tkobjc.h>
#import <collections.h>
#import <simtools.h>
#import <analysis.h>
#import "MousetrapModelSwarm.h"

@interface MousetrapObserverSwarm: GUISwarm
{
  int displayFrequency;			// one parameter: update frequency

  id displayActions;			// schedule data structs
  id displaySchedule;

  MousetrapModelSwarm *mousetrapModelSwarm;	// the Swarm we're observing

 // Display objects, widgets, etc.

  XColormap *colormap;			// allocate colors
  ZoomRaster *displayWindow;		// 2d display widget
  EZGraph *triggerGraph;		// graphing widget

  Object2dDisplay *mousetrapDisplay;    // to Probe mousetraps

  // declare the activity controller, which provides an interface 
  //   to the observerSwarm activity.
  id <ActivityControl> observerActCont;
}

// Methods overriden to make the Swarm

+ createBegin: aZone;
- createEnd;
- buildObjects;
- buildActions;
- activateIn: swarmContext;		// Context is self (ObserverSwarm).

- checkToStop;			        // Method to end simulation

@end
