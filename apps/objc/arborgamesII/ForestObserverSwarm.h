#import <defobj.h>
#import <simtoolsgui/GUISwarm.h>
#import <space.h>
#import <gui.h>

#import "ForestModelSwarm.h"
#import "Species.h"
#import <analysis.h>  //for EZGraph

#define MAKE_RASTER_SCREENSHOT(widget) \
{ \
char filename[40]; \
sprintf (filename, "%s-%s-%07d.png", [arguments getAppName], #widget, \
         getCurrentTime ()); \
[[[[[[Pixmap createBegin: [self getZone]] \
              setWidget: widget] \
             setDecorationsFlag: YES] \
            createEnd] save: filename] drop]; \
}
#define MAKE_EZGRAPH_SCREENSHOT(widget) \
{ \
char filename[40]; \
sprintf (filename, "%s-%s-%07d.png", [arguments getAppName], #widget, \
         getCurrentTime ()); \
[actionCache doTkEvents]; \
[[[[[[Pixmap createBegin: [self getZone]] \
              setWidget: [widget getGraph]] \
             setDecorationsFlag: NO] \
            createEnd] save: filename] drop]; \
}

@interface ForestObserverSwarm: GUISwarm
{
  int displayFrequency;
  int showSeedDistribution;
  int showPopulationGraph;

  int speciesNumber;
  id speciesList;

  id displayList;
  id rasterList;

  id displayActions;
  id displaySchedule;

  ForestModelSwarm *forestModelSwarm;

  id <Colormap> colormap;

  id <ZoomRaster> forestRaster;
  id <ZoomRaster> youngForestRaster;
  id <ZoomRaster> fireRaster;

  id <EZGraph> popGraph;id <EZGraph> speciesGraph; id <EZGraph> entropyGraph;
  id speciesEntropy;

  id <Object2dDisplay> treeDisplay;
  id <Object2dDisplay> youngForestDisplay;
  id <Value2dDisplay> fireDisplay;
}

// Methods overriden to make the Swarm.
+ createBegin: aZone;
- createEnd;
- buildObjects;
- buildActions;
- activateIn: swarmContext;
- showAgeLevel;
- showSpecies;
@end

