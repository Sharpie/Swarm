// Schellings Segregation Model a la Axtell and Epstein
// Code by Benedikt Stefansson, <benedikt@ucla.edu>. 
// First version July 1997

#import <objectbase.h>
#import <simtoolsgui.h>
#import <simtoolsgui/GUISwarm.h>
#import <space.h>
#import <activity.h>
#import <analysis.h>
#import <collections.h>
#import <simtools.h>
#import "ModelSwarm.h"

@interface ObserverSwarm : GUISwarm {
  int displayFrequency;				

  id displayActions;				
  id displaySchedule;

  ModelSwarm * modelSwarm;	 
  id <Colormap> colormap;
  id <ZoomRaster> worldRaster;
  id <Object2dDisplay> worldDisplay;
  id <EZGraph> moveGraph;

}

// Methods overriden to make the Swarm.
+createBegin: (id) aZone;
-createEnd;
-buildObjects;
-buildActions;
-activateIn: (id) swarmContext;
-eraseRaster;

@end
