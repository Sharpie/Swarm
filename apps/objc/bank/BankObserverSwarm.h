// Copyright (C) 1996-1998 Santa Fe Institute.
#import <analysis.h> // Entropy
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
  id <Entropy> investorEntropy;   		  
  id <ActiveGraph> investorGrapher;		  
  id <GraphElement> borrowerData;			  
  id <Entropy> borrowerEntropy;   		  
  id <ActiveGraph> borrowerGrapher;		  
  id activeBanks;
  
#if 0
  XColormap *colormap;			    // allocate colours
  ZoomRaster *worldRaster;		    // 2d display widget


  Value2dDisplay *heatDisplay;		    // display the heat
  Object2dDisplay *bankDisplay;		    // display the banks
#endif
}

+ createBegin: aZone;
- createEnd;
- buildObjects;
- buildActions;
- activateIn: swarmContext;

- redistribute;

@end
