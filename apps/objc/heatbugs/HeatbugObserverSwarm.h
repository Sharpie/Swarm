// Heatbugs application. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// The HeatbugObserverSwarm is a swarm of objects set up to observe a
// Heatbugs model when the graphical interface is running. The most
// important object is the heatbugModelSwarm, but we also have
// graphical windows and data analysis and stuff.

#import <swarmobject.h>
#import <space.h>
#import <activity.h>
#import <tkobjc.h>
#import <collections.h>
#import <simtools.h>
#import <analysis.h>
#import "HeatbugModelSwarm.h"

@interface HeatbugObserverSwarm : GUISwarm {
  int displayFrequency;				  // one parameter: update freq

  id displayActions;				  // schedule data structs
  id displaySchedule;

  HeatbugModelSwarm * heatbugModelSwarm;	  // the Swarm we're observing

  // Lots of display objects. First, widgets

  XColormap * colormap;				  // allocate colours
  ZoomRaster * worldRaster;			  // 2d display widget
  EZGraph * unhappyGraph;			  // graphing widget

  // Now, higher order display and data objects

  Value2dDisplay * heatDisplay;			  // display the heat
  Object2dDisplay * heatbugDisplay;		  // display the heatbugs
}

// Methods overriden to make the Swarm.

+createBegin: (id) aZone;
-createEnd;
-buildObjects;
-buildActions;
-activateIn: (id) swarmContext;

-graphBug: aBug ;
@end
