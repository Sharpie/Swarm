// Heatbugs application. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// The HeatbugObserverSwarm is a swarm of objects set up to observe a
// Heatbugs model when the graphical interface is running. The most
// important object is the heatbugModelSwarm, but we also have
// graphical windows and data analysis and stuff.

#import <simtoolsgui.h>
#import <analysis.h> // EZGraph
#import "HeatbugModelSwarm.h"

#import <simtoolsgui/GUISwarm.h>

@interface HeatbugObserverSwarm: GUISwarm
{
  int displayFrequency;				// one parameter: update freq

  id displayActions;				// schedule data structs
  id displaySchedule;

  HeatbugModelSwarm *heatbugModelSwarm;	  	// the Swarm we're observing

  // Lots of display objects. First, widgets

  id <Colormap> colormap;			// allocate colours
  id <ZoomRaster> worldRaster;			// 2d display widget
  EZGraph *unhappyGraph;			// graphing widget
  
  // Now, higher order display and data objects

  Value2dDisplay *heatDisplay;			// display the heat
  Object2dDisplay *heatbugDisplay;	        // display the heatbugs
}

// Methods overriden to make the Swarm.

+ createBegin: aZone;
- createEnd;
- buildObjects;
- buildActions;
- activateIn: swarmContext;

- graphBug: aBug;
@end
