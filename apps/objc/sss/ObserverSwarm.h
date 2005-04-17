// Sugarscape in Swarm. Copyright © 1997-1998 Nelson Minar
// This program is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtoolsgui.h>
#import <simtoolsgui/GUISwarm.h>
#import <analysis.h> // EZGraph
#import <space.h> // Value2dDisplay, Object2dDisplay
#import "ModelSwarm.h"

// The observer swarm contains all the objects used to instrument a model
@interface ObserverSwarm: GUISwarm
{
  ModelSwarm *modelSwarm;			  // the model being run

  char * parameterFile;
  int displayFrequency;				  // how often to update
  int drawPopulationGraph;			  // flag
  int drawWealthHistogram;			  // flag

  id <Colormap> colormap;			  // colours
  id <ZoomRaster> worldRaster;			  // window on world
  id <Value2dDisplay> sugarDisplay;		  // sugar displayer
  id <Object2dDisplay> agentDisplay;		  // agent displayer
  id <EZGraph> populationGraph;			  // population graph
  id <EZGraph> attributeGraph;			  // agent attributes graph
  id <EZBin> wealthHistogram;			  // histogram of wealth

  id displayActions;				  // schedule objects
  id displaySchedule;
}

+ createBegin: aZone;
- buildObjects;

- (char*)setParameterFile: (char*)aString;
- saveParameters: (char*)aString;

- buildActions;
- (id <Activity>)activateIn: swarmContext;

@end

