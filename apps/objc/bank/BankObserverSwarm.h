#import <analysis.h>
#import <simtoolsgui.h> // ActiveGraph
#import <simtoolsgui/GUISwarm.h>
#import "BankModelSwarm.h"

@interface BankObserverSwarm: GUISwarm
{
  int displayFrequency;
  int interactiveGraph;
  id displayActions;	
  id displaySchedule;

  BankModelSwarm *bankModelSwarm;

  id <Canvas> graphCanvas;

  id <Graph> investorGraph;			  
  id <GraphElement> investorData;			  
  Entropy *investorEntropy;   		  
  id <ActiveGraph> investorGrapher;		  
  id <GraphElement> borrowerData;			  
  Entropy *borrowerEntropy;   		  
  id <ActiveGraph> borrowerGrapher;		  
  id activeBanks;
  
  /*
  XColormap * colormap;				  // allocate colours
  ZoomRaster * worldRaster;			  // 2d display widget


  Value2dDisplay * heatDisplay;			  // display the heat
  Object2dDisplay * bankDisplay;		  // display the banks
  */
}

+ createBegin: aZone;
- createEnd;
- buildObjects;
- buildActions;
- activateIn: swarmContext;

- redistribute;

@end
