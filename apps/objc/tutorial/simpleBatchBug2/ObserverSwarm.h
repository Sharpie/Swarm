// ObserverSwarm.h

#import "ModelSwarm.h"
#import <simtoolsgui/GUISwarm.h>

@interface ObserverSwarm: GUISwarm
{
  int displayFrequency;                           // one parameter: update freq

  id displayActions;                              // schedule data structs
  id displaySchedule;
  id stopSchedule;

  ModelSwarm *modelSwarm;          		  // the Swarm we're observing

  // Lots of display objects. First, widgets

  id <Colormap> colorMap;                         // allocate colours
  id <ZoomRaster> worldRaster;                    // 2d display widget

 // Now, higher order display and data objects

  id <Value2dDisplay> foodDisplay;                // display the heat
  id <Object2dDisplay> bugDisplay;                // display the heatbugs
}

// Methods overriden to make the Swarm.

+ createBegin: aZone;
- createEnd;
- buildObjects;
- takeScreenShot;
// new method to end the model's run and close it.
- stopRunning;
- buildActions;
- (id <Activity>)activateIn: swarmContext;

- (void)drop;

@end

