#import "ObserverSwarm.h"
#import <collections.h>
#import <objectbase.h>

@implementation ObserverSwarm

// createBegin: here we set up the default observation parameters.
+createBegin: (id) aZone {
  ObserverSwarm * obj;
  id <ProbeMap> probeMap;
  
  // Superclass createBegin to allocate ourselves.
  obj = [super createBegin: aZone];

  // Fill in the relevant parameters (only one, in this case).
  obj->displayFrequency = 1;
  obj->worldSizeX = 100;
  obj->worldSizeY = 100;

  // Also, build a customized probe map. Without a probe map, the default
  // is to show all variables and messages. Here we choose to
  // customize the appearance of the probe, give a nicer interface.
  probeMap = [EmptyProbeMap createBegin: aZone];
  [probeMap setProbedClass: [self class]];
  probeMap = [probeMap createEnd];

  // Add in a bunch of variables, one per simulation parameters
  [probeMap addProbe: [probeLibrary getProbeForVariable: "displayFrequency"
				    inClass: [self class]]];

  [probeMap addProbe: [probeLibrary getProbeForVariable: "worldSizeX"
                                    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "worldSizeY"
                                    inClass: [self class]]];

  // Upon further review, I see no reason for a user to ever need
  // a manual update of the gui. If eraseConway is called, the
  // erasure is called automatically
  //[probeMap addProbe: [[probeLibrary getProbeForMessage: "updateGUI"
  //			                        inClass: [self class]]
  //			                  setHideResult: 0]];


 [probeMap addProbe: [[probeLibrary getProbeForMessage: "eraseConway"
			                        inClass: [self class]]
			                  setHideResult: 0]];

  // Now install our custom probeMap into the probeLibrary.
  [probeLibrary setProbeMap: probeMap For: [self class]];

  return obj;
}


// createEnd: create objects we know we'll need. In this case, none,
// but you might want to override this.
-createEnd {
  return [super createEnd];
}

-buildObjects {

  [super buildObjects];

  CREATE_ARCHIVED_PROBE_DISPLAY (self);

  [controlPanel setStateStopped];

  // Check now if the user hit the quit button: if so, abort.
  if ([controlPanel getState] == ControlStateQuit) return self;



  colormap = [Colormap create: self];

  // set up the colormap, it maps colors used
  // by the Tk widgets - which have actual names -
  // into our internal representation as arbitrary integers
  [colormap setColor: 10 ToName: "blue"];
  [colormap setColor: 11 ToName: "red"];
  [colormap setColor: 1 ToName: "white"];

  // create the 2d window which is used to
  // display the position of agents on the toroid
  worldRaster = [ZoomRaster createBegin: self];
  SET_WINDOW_GEOMETRY_RECORD_NAME (worldRaster);
  worldRaster = [worldRaster createEnd];
  [worldRaster setColormap: colormap];
  [worldRaster setZoomFactor: 6];
  [worldRaster setWidth: worldSizeX Height: worldSizeY ];
  [worldRaster setWindowTitle: "Conway's world"];
  [worldRaster pack];

  conwayWorld = [ConwayWorld createBegin: self];
  [conwayWorld setSizeX: worldSizeX Y: worldSizeY];
  [conwayWorld setNumStates: 2];
 
  // [conwayWorld initializeLattice];
  conwayWorld = [conwayWorld createEnd];
  [conwayWorld setObserver: self];



  valueDisplay = 
    [Value2dDisplay create: self
                    setDisplayWidget: worldRaster
                    colormap: colormap
                    setDiscrete2dToDisplay: conwayWorld];
  

  // This allows the user to right-click on the display to probe agents
  [worldRaster setButton: ButtonRight Client: conwayWorld Message: M(swapColorAtX:Y:)];

  // Finally add one graph widget to show how many
  // people are unhappy and trying to move
//   moveGraph = [EZGraph createBegin: self];
//   SET_WINDOW_GEOMETRY_RECORD_NAME (moveGraph);
//   [moveGraph setTitle: "Fraction of people unhappy"];
//   [moveGraph setAxisLabelsX: "time" Y: "number"];
//   moveGraph = [moveGraph createEnd];


  return self;
}  

- updateGUI
{
  [conwayWorld updateLattice];
  [valueDisplay display];
  [worldRaster  drawSelf];
  return self;
}

- eraseConway
{
  if (conwayWorld) [conwayWorld eraseAll];
  [self updateGUI];
  return self;
}



-buildActions {
  [super buildActions];

  // Then create a group of actions for the observer
  displayActions = [ActionGroup create: self ];

  [displayActions createActionTo: probeDisplayManager message: M(update)];
  [displayActions createActionTo: actionCache         message: M(doTkEvents)];
  //[displayActions createActionTo: self                message: M(eraseRaster)];
  [displayActions createActionTo: conwayWorld                message: M(stepRule)];
  [displayActions createActionTo: valueDisplay        message: M(display)];
  [displayActions createActionTo: worldRaster         message: M(drawSelf)];
  // [displayActions createActionTo: moveGraph	message:M(step)];

  // Put these actions on a schedule to be repeated at a certain frequency
  displaySchedule = [Schedule createBegin: self];
  [displaySchedule setRepeatInterval: displayFrequency]; // note frequency!
  displaySchedule = [displaySchedule createEnd];
  [displaySchedule at: 0 createAction: displayActions];
  
  return self;
}  

// activateIn: - activate the schedules so they're ready to run.
// The swarmContext argument has to do with what we were activated *in*.
// Typically the ObserverSwarm is the top-level Swarm, so it's activated

// in "nil". But other Swarms and Schedules and such will be activated
// inside of us.
-activateIn: (id) swarmContext {
  // First, activate ourselves (just pass along the context).
  [super activateIn: swarmContext];

  // Now activate our schedule in ourselves. This arranges for the
  // execution of the schedule we built.
  [displaySchedule activateIn: self];


  // Activate returns the swarm activity - the thing that's ready to run.
  return [self getSwarmActivity];
}

-eraseRaster {
  // A method that overrides the default
  // black background for raster and paints
  // the raster white instead
  [worldRaster fillRectangleX0: 0
	                    Y0: 0
	                    X1: worldSizeX 
			    Y1: worldSizeY
	                 Color: 1]; 
	return self;

}


@end
