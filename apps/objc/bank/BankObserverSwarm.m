#import "BankObserverSwarm.h"
#import "BankModelSwarm.h"
#import <collections.h>
#import <objectbase.h>

@implementation BankObserverSwarm

// createBegin: here we set up the default observation parameters.
+ createBegin: aZone
{
  BankObserverSwarm *obj;
  id <ProbeMap> probeMap;
  
  obj = [super createBegin: aZone];

  obj->displayFrequency = 1;
  obj->interactiveGraph = 1;

  probeMap = [EmptyProbeMap createBegin: aZone];
  [probeMap setProbedClass: [self class]];
  probeMap = [probeMap createEnd];

  [probeMap addProbe: [probeLibrary getProbeForVariable: "displayFrequency"
				    inClass: [self class]]];
  
  [probeMap addProbe: [probeLibrary getProbeForVariable: "interactiveGraph"
				    inClass: [self class]]];
  
  [probeMap addProbe: [[probeLibrary getProbeForMessage: "redistribute"
                                     inClass: [self class]]
			setHideResult: 1]];
  
  [probeLibrary setProbeMap: probeMap For: [self class]];

  return obj;
}

- createEnd
{
  return [super createEnd];
}

- _graphCanvasDeath_: caller
{
  [graphCanvas drop];
  graphCanvas = nil;
  [bankModelSwarm setGraphCanvas: nil];

  return self;
}

- buildObjects
{
  id modelZone;	
  
  [super buildObjects];
  
  modelZone = [Zone create: [self getZone]];
  bankModelSwarm = [BankModelSwarm create: modelZone];
  
  // Now create probe objects on the model and ourselves. This gives a
  // simple user interface to let the user change parameters.
  CREATE_ARCHIVED_PROBE_DISPLAY (bankModelSwarm);
  CREATE_ARCHIVED_PROBE_DISPLAY (self);
  
  [controlPanel waitForControlEvent];
  if ([controlPanel getState] == ControlStateQuit)
    return self;

  if (interactiveGraph)
    {
      graphCanvas = [Canvas createBegin: [self getZone]];
      SET_WINDOW_GEOMETRY_RECORD_NAME (graphCanvas);
      graphCanvas = [graphCanvas createEnd];

      [graphCanvas enableDestroyNotification: self
                   notificationMethod: @selector(_graphCanvasDeath_:)];

      [[graphCanvas setHeight: 500] setWidth: 500];
      [graphCanvas setWindowTitle: "The Emergence Of Banking"];
      [globalTkInterp eval: 
                        "pack %s -expand 1 -fill both; update idletasks", 
                      [graphCanvas getWidgetName]];
    }
  
  [bankModelSwarm setGraphCanvas: graphCanvas];
  [bankModelSwarm buildObjects];

  activeBanks = [EZGraph createBegin: [self getZone]];
  SET_WINDOW_GEOMETRY_RECORD_NAME (activeBanks);
  [activeBanks setTitle: "Active Banks"];
  [activeBanks setAxisLabelsX: "Time" Y: "Banks"];
  activeBanks = [activeBanks createEnd];

  [activeBanks createTotalSequence: "Survivors" 
                      withFeedFrom: [bankModelSwarm getEntityList]
                       andSelector: M(incident)];

  investorGraph = [BLTGraph createBegin: [self getZone]];
  SET_WINDOW_GEOMETRY_RECORD_NAME (investorGraph);
  investorGraph = [investorGraph createEnd];

  [investorGraph title: "Entropy of investor link distribution"];
  [investorGraph axisLabelsX: "time" Y: "prop. of Max. Entropy"];
  [investorGraph pack];

  investorData = [investorGraph createElement];
  [investorData setLabel: "H(investors)"];

  investorEntropy = [Entropy createBegin: [self getZone]];
  [investorEntropy setCollection: [bankModelSwarm getEntityList]];
  [investorEntropy setProbedSelector: M(getInvestorLinkProbability)];
  investorEntropy = [investorEntropy createEnd];

  investorGrapher = [ActiveGraph createBegin: [self getZone]];
  [investorGrapher setElement: investorData];
  [investorGrapher setDataFeed: investorEntropy]; // chain them up
  [investorGrapher setProbedSelector: M(getEntropy)];
  investorGrapher = [investorGrapher createEnd];

  borrowerData = [investorGraph createElement];
  [borrowerData setLabel: "H(borrowers)"];

  borrowerEntropy = [Entropy createBegin: [self getZone]];
  [borrowerEntropy setCollection: [bankModelSwarm getEntityList]];
  [borrowerEntropy setProbedSelector: M(getBorrowLinkProbability)];
  borrowerEntropy = [borrowerEntropy createEnd];

  borrowerGrapher = [ActiveGraph createBegin: [self getZone]];
  [borrowerGrapher setElement: borrowerData];
  [borrowerGrapher setDataFeed: borrowerEntropy]; // chain them up
  [borrowerGrapher setProbedSelector: M(getEntropy)];
  borrowerGrapher = [borrowerGrapher createEnd];

/*
  colormap = [XColormap create: [self getZone]];

  // Colours [0,64) are assigned to the range Red [0, 1), for heat display.
  for (i = 0; i < 64; i++)
    [colormap setColor: i ToRed: (double)i / 63.0 Green: 0 Blue: 0];
  // Colour 64 is set to green, to display heatbugs
  [colormap setColor: 64 ToName: "green"];
  // Colour 65 is set to white, used in this case below on probed heatbug.
  [colormap setColor: 65 ToName: "white"];

  // Now go in to the heatbugs in the model and set their colours to green (64)
  [[heatbugModelSwarm getHeatbugList] forEach: M(setBugColor:) : (void *) 64];
  
  // Next, create a 2d window for display, set its size, zoom factor, title.
  worldRaster = [ZoomRaster create: [self getZone]];
  [worldRaster setColormap: colormap];
  [worldRaster setZoomFactor: 4];
  [worldRaster setWidth: [[heatbugModelSwarm getWorld] getSizeX]
	       Height: [[heatbugModelSwarm getWorld] getSizeY]];
  [worldRaster setWindowTitle: "Heat World"];
  [worldRaster pack];				  // draw the window.

  // Now create a Value2dDisplay: this is a special object that will
  // display arbitrary 2d value arrays on a given Raster widget.
  heatDisplay = [Value2dDisplay createBegin: [self getZone]];
  [heatDisplay setDisplayWidget: worldRaster Colormap: colormap];
  [heatDisplay setDiscrete2dToDisplay: [heatbugModelSwarm getHeat]];
  [heatDisplay setDisplayMappingM: 512 C: 0];	  // turn [0,32768) -> [0,64)
  heatDisplay = [heatDisplay createEnd];

  // And also create an Object2dDisplay: this object draws heatbugs on
  // the worldRaster widget for us, and also receives probes.
  heatbugDisplay = [Object2dDisplay createBegin: [self getZone]];
  [heatbugDisplay setDisplayWidget: worldRaster];
  [heatbugDisplay setDiscrete2dToDisplay: [heatbugModelSwarm getWorld]];
  [heatbugDisplay setObjectCollection: [heatbugModelSwarm getHeatbugList]];
  [heatbugDisplay setDisplayMessage: M(drawSelfOn:)];   // draw method
  heatbugDisplay = [heatbugDisplay createEnd];
  // Also, tell the world raster to send mouse clicks to the heatbugDisplay
  // this allows the user to right-click on the display to probe the bugs.
  [worldRaster setButton: ButtonRight Client: heatbugDisplay Message: M(makeProbeAtX:Y:)];


  // Finally, we create a Probe display to probe a particular heatbug.
  // Probes can also be created on the fly, we just do this here for demo.
  heatbugToProbe = [[heatbugModelSwarm getHeatbugList] first];
  [heatbugToProbe setBugColor: 65];
  [probeDisplayManager createProbeDisplayFor: heatbugToProbe];

  // All done - we're ready to build a schedule and go.
*/
  return self;
}  

- redistribute
{
  [[bankModelSwarm getTheFNet] redistribute];
  return self;
}

- buildActions
{
  [super buildActions];
  
  [bankModelSwarm buildActions];
  
  displayActions = [ActionGroup create: [self getZone]];
/*
  // Schedule up the methods to draw the display of the world
  [displayActions createActionTo: heatDisplay         message: M(display)];
  [displayActions createActionTo: heatbugDisplay      message: M(display)];
  [displayActions createActionTo: worldRaster         message: M(drawSelf)];
  // Now schedule the update of the unhappiness graph
  // Schedule the update of the probe displays
*/
  [displayActions createActionTo: activeBanks         message: M(step)];
  [displayActions createActionTo: investorEntropy     message: M(update)];
  [displayActions createActionTo: investorGrapher     message: M(step)];
  [displayActions createActionTo: borrowerEntropy     message: M(update)];
  [displayActions createActionTo: borrowerGrapher     message: M(step)];
  [displayActions createActionTo: probeDisplayManager message: M(update)];
  [displayActions createActionTo: actionCache         message: M(doTkEvents)];

  displaySchedule = [Schedule createBegin: [self getZone]];
  [displaySchedule setRepeatInterval: displayFrequency];
  displaySchedule = [displaySchedule createEnd];
  [displaySchedule at: 0 createAction: displayActions];

  return self;
}  

- activateIn: swarmContext
{

  [super activateIn: swarmContext];

  [bankModelSwarm activateIn: self];

  [displaySchedule activateIn: self];

  return [self getSwarmActivity];
}

// You could override the "go" method here if you want something special
// to happen when the model and observer actually start running. But
// the default GUISwarm go is probably good enough.

@end
