// Mousetraps application. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <swarmobject.h>
#import "MousetrapModelSwarm.h"

@interface MousetrapObserverSwarm : GUISwarm {
  int displayFrequency;

  id displayActions;
  id displaySchedule;

  MousetrapModelSwarm * mousetrapModelSwarm;

  XColormap * colormap;
  ZoomRaster * displayWindow;
  BLTGraph *triggerGraph;
  GraphElement *triggerData, *deltaTriggerData;
  ActiveGraph *triggerGrapher, *deltaTriggerGrapher;
}

+createBegin: (id) aZone;
-buildObjects;
-buildActions;
-activateIn: (id) swarmContext;

-checkToStop;

@end
