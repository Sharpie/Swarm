// Mousetraps application. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import "MousetrapObserverSwarm.h"

@implementation MousetrapObserverSwarm

+createBegin: (id) aZone {
  MousetrapObserverSwarm * obj;
  ProbeMap * probeMap;
  
  obj = [super createBegin: aZone];

  obj->displayFrequency = 1;

  probeMap = [EmptyProbeMap createBegin: aZone];
  [probeMap setProbedClass: [self class]];
  probeMap = [probeMap createEnd];

  [probeMap addProbe: [probeLibrary getProbeForVariable: "displayFrequency"
				    inClass: [self class]]];

  [probeLibrary setProbeMap: probeMap For: [self class]];

  return obj;
}

-buildObjects {
  id modelZone;
  int x, y, size;
  
  [super buildObjects];
  
  modelZone = [Zone create: [self getZone]];
  mousetrapModelSwarm = [MousetrapModelSwarm create: modelZone];
  
  [probeDisplayManager createProbeDisplayFor: mousetrapModelSwarm];
  [probeDisplayManager createProbeDisplayFor: self];

  [controlPanel waitForControlEvent];
  if ([controlPanel getState] == ControlStateQuit)
    return self;


  // Finished probing, set it all up.
  
  [mousetrapModelSwarm buildObjects];

  // create a colormap
  colormap = [XColormap create: globalZone];
  [colormap setColor: 1 ToGrey: 0.3];
  [colormap setColor: 2 ToName: "red"];

  // set up the graph of number triggered
  triggerGraph = [BLTGraph create: globalZone];
  [triggerGraph title: "Total number vs. time"];
  [triggerGraph axisLabelsX: "time" Y: "number"];

  triggerData = [triggerGraph createElement];
  [triggerData setLabel: "total triggered"];
  [triggerData setColor: "red"];

  triggerGrapher = [ActiveGraph createBegin: globalZone];
  [triggerGrapher setElement: triggerData];
  [triggerGrapher setDataFeed: [mousetrapModelSwarm getStats]];
  [triggerGrapher setProbedSelector: M(getNumTriggered)];
  triggerGrapher = [triggerGrapher createEnd];

  // and the graph of how many are waiting to go
  deltaTriggerData = [triggerGraph createElement];
  [deltaTriggerData setLabel: "total balls in air"];
  [deltaTriggerData setColor: "black"];

  deltaTriggerGrapher = [ActiveGraph createBegin: globalZone];
  [deltaTriggerGrapher setElement: deltaTriggerData];
  [deltaTriggerGrapher setDataFeed: [mousetrapModelSwarm getStats]];
  [deltaTriggerGrapher setProbedSelector: M(getNumBalls)];
  deltaTriggerGrapher = [deltaTriggerGrapher createEnd];

  [triggerGraph pack];

  // create a window for display
  displayWindow = [ZoomRaster create: globalZone];
  [displayWindow setColormap: colormap];
  [displayWindow setZoomFactor: 6];
  [displayWindow setWidth: [mousetrapModelSwarm getGridSize]
		 Height: [mousetrapModelSwarm getGridSize]];

  // draw all the mousetraps in
  size = [mousetrapModelSwarm getGridSize];
  for (x = 0; x < size; x++)
    for (y = 0; y < size; y++) {
      Mousetrap * trap;
      trap = [mousetrapModelSwarm getMousetrapAtX: x Y: y];
      if (trap) {
	[displayWindow drawPointX: x Y: y Color: 1];
	[trap setDisplayWidget: displayWindow];
      }
    }
	
  [displayWindow drawSelf];
  [displayWindow pack];
  
  return self;
}  

-buildActions {
  [super buildActions];
  
  [mousetrapModelSwarm buildActions];

  // MODIFY: schedule display objects here.
  displayActions = [ActionGroup create: [self getZone]];
  [displayActions createActionTo: displayWindow       message: M(drawSelf)];
  [displayActions createActionTo: triggerGrapher      message: M(step)];
  [displayActions createActionTo: deltaTriggerGrapher message: M(step)];
  [displayActions createActionTo: probeDisplayManager message: M(update)];
  [displayActions createActionTo: self                message: M(checkToStop)];
  [displayActions createActionTo: controlPanel        message: M(doTkEvents)];
  
  displaySchedule = [Schedule createBegin: [self getZone]];
  [displaySchedule setRepeatInterval: displayFrequency];
  displaySchedule = [displaySchedule createEnd];
  [displaySchedule at: 0 createAction: displayActions];

  return self;
}  

-activateIn: (id) swarmContext {
  [super activateIn: swarmContext];

  [mousetrapModelSwarm activateIn: self];
  [displaySchedule activateIn: self];
  
  return [self getSwarmActivity];
}

// monitor method - if all the balls have landed, time to quit!
-checkToStop {
  if ([[mousetrapModelSwarm getStats] getNumBalls] == 0) {
    printf("All the balls have landed!\n");
    [controlPanel setStateStopped];
  }
  return self;
}

@end
