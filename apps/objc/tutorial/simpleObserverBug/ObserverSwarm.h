// ObserverSwarm.h

#import <swarmobject.h>
#import <space.h>
#import <activity.h>
#import <tkobjc.h>
#import <collections.h>
#import <simtools.h>
#import <analysis.h>
#import "ModelSwarm.h"

@interface ObserverSwarm : GUISwarm {
  int displayFrequency;                           // one parameter: update freq

  id displayActions;                              // schedule data structs
  id displaySchedule;

  ModelSwarm * modelSwarm;          		  // the Swarm we're observing

  // Lots of display objects. First, widgets

  XColormap * colorMap;                           // allocate colours
  ZoomRaster * worldRaster;                       // 2d display widget

 // Now, higher order display and data objects

  Value2dDisplay * foodDisplay;                   // display the heat
  Object2dDisplay * bugDisplay;                   // display the heatbugs
}

// Methods overriden to make the Swarm.

+createBegin: (id) aZone;
-createEnd;
-buildObjects;
-buildActions;
-activateIn: (id) swarmContext;

@end

