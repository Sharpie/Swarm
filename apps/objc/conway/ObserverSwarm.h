#import <objectbase.h>
#import <simtoolsgui.h>
#import <simtoolsgui/GUISwarm.h>
#import <space.h>
#import <activity.h>
#import <analysis.h>
#import <collections.h>
#import <simtools.h>
#import "ConwayWorld.h"

@interface ObserverSwarm : GUISwarm {
  int displayFrequency;				
  int worldSizeX;
  int worldSizeY;
  
  id displayActions;				
  id displaySchedule;

  id <Colormap> colormap;
  id <ZoomRaster> worldRaster;
  id  conwayWorld;
  id <Object2dDisplay> valueDisplay;
  // id <EZGraph> moveGraph;

}

// Methods overriden to make the Swarm.
+createBegin: (id) aZone;
-createEnd;


-buildObjects;

- updateGUI;
- eraseConway;


-buildActions;
-activateIn: (id) swarmContext;
-eraseRaster;

@end
