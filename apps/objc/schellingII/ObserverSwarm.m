// Schellings Segregation Model a la Axtell and Epstein
// Code by Benedikt Stefansson, <benedikt@ucla.edu>. 
// First version July 1997


#import "ObserverSwarm.h"
#import "ModelSwarm.h"
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

  // Also, build a customized probe map. Without a probe map, the default
  // is to show all variables and messages. Here we choose to
  // customize the appearance of the probe, give a nicer interface.
  probeMap = [EmptyProbeMap createBegin: aZone];
  [probeMap setProbedClass: [self class]];
  probeMap = [probeMap createEnd];

  // Add in a bunch of variables, one per simulation parameters
  [probeMap addProbe: [probeLibrary getProbeForVariable: "displayFrequency"
				    inClass: [self class]]];

  // Now install our custom probeMap into the probeLibrary.
  [probeLibrary setProbeMap: probeMap For: [self class]];

  return obj;
}


// createEnd: create objects we know we'll need. In this case, none,
// but you might want to override this.
-createEnd {
  return [super createEnd];
}

// Create the objects used in the display of the model. 
-buildObjects {
  id modelZone;					  // zone for model.

  [super buildObjects];

  modelZone = [Zone create: self];
  modelSwarm = [ModelSwarm create: modelZone];
  
 
  CREATE_ARCHIVED_PROBE_DISPLAY (modelSwarm);
  CREATE_ARCHIVED_PROBE_DISPLAY (self);


  [controlPanel setStateStopped];

  // Check now if the user hit the quit button: if so, abort.
  if ([controlPanel getState] == ControlStateQuit) return self;

  [modelSwarm buildObjects];


  colormap = [Colormap create: self];

  // set up the colormap, it maps colors used
  // by the Tk widgets - which have actual names -
  // into our internal representation as arbitrary integers
  [colormap setColor: 0 ToName: "blue"];
  [colormap setColor: 1 ToName: "red"];
  [colormap setColor: 2 ToName: "green"];
  [colormap setColor: 3 ToName: "yellow"];
  [colormap setColor: 4 ToName: "purple"];
  [colormap setColor: 5 ToName: "cyan2"];
  [colormap setColor: 6 ToName: "LightSkyBlue3"];
  [colormap setColor: 7 ToName: "SlateGray4"];
  [colormap setColor: 8 ToName: "MidnightBlue"];
  [colormap setColor: 9 ToName: "DarkKhaki"];
  [colormap setColor: 10 ToName: "LightGoldenrod"];
  [colormap setColor: 11 ToName: "IndianRed"];
  [colormap setColor: 12 ToName: "DarkOrange"];
  [colormap setColor: 13 ToName: "HotPink"];
  [colormap setColor: 14 ToName: "SteelBlue2"];
  [colormap setColor: 15 ToName: "VioletRed"];
  [colormap setColor: 16 ToName: "turquoise4"];
  [colormap setColor: 17 ToName: "burlywood2"];
  [colormap setColor: 18 ToName: "gray70"];
  [colormap setColor: 19 ToName: "thistle1"];


  [colormap setColor: 21 ToName: "white"];

  // create the 2d window which is used to
  // display the position of agents on the toroid
  worldRaster = [ZoomRaster createBegin: self];
  SET_WINDOW_GEOMETRY_RECORD_NAME (worldRaster);
  worldRaster = [worldRaster createEnd];
  [worldRaster setColormap: colormap];
  [worldRaster setZoomFactor: 6];
  [worldRaster setWidth: [modelSwarm getWorldSize] Height: [modelSwarm getWorldSize]];
  [worldRaster setWindowTitle: "Schelling's world"];
  [worldRaster pack];

  // Create Object2dDisplay which knows how to
  // display a 2d space of objects/agents and
  // respond to mouseclicks from the raster
  worldDisplay = [Object2dDisplay createBegin: self ];
  [worldDisplay setDisplayWidget: worldRaster];
  [worldDisplay setDiscrete2dToDisplay: [[modelSwarm getWorld] getObjectGrid]];
  [worldDisplay setObjectCollection: [modelSwarm getAgentList]];
  [worldDisplay setDisplayMessage: M(drawSelfOn:)];   
  worldDisplay = [worldDisplay createEnd];

  // This allows the user to right-click on the display to probe agents
  [worldRaster setButton: ButtonRight Client: worldDisplay Message: M(makeProbeAtX:Y:)];

  // Finally add one graph widget to show how many
  // people are unhappy and trying to move
  moveGraph = [EZGraph createBegin: self];
  SET_WINDOW_GEOMETRY_RECORD_NAME (moveGraph);
  [moveGraph setTitle: "Fraction of people unhappy"];
  [moveGraph setAxisLabelsX: "time" Y: "number"];
  moveGraph = [moveGraph createEnd];

  [moveGraph createAverageSequence: "unhappy"
  withFeedFrom: [modelSwarm getAgentList]
  andSelector: M(getUnhappy)] ;


  return self;
}  

-buildActions {
  [super buildActions];

  // Tell the model Swarm to build it's schedules
  [modelSwarm buildActions];

  // Then create a group of actions for the observer
  displayActions = [ActionGroup create: self ];

  [displayActions createActionTo: probeDisplayManager message: M(update)];
  [displayActions createActionTo: actionCache         message: M(doTkEvents)];
  [displayActions createActionTo: self                message: M(eraseRaster)];
  [displayActions createActionTo: worldDisplay        message: M(display)];
  [displayActions createActionTo: worldRaster         message: M(drawSelf)];
  [displayActions createActionTo: moveGraph	message:M(step)];

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

  // Activate the model swarm in ourselves. The model swarm is a
  // subswarm of the observer swarm.
  [modelSwarm activateIn: self];

  // Activate returns the swarm activity - the thing that's ready to run.
  return [self getSwarmActivity];
}

-eraseRaster {
  // A method that overrides the default
  // black background for raster and paints
  // the raster white instead
  [worldRaster fillRectangleX0: 0
	                    Y0: 0
	                    X1: [modelSwarm getWorldSize] 
			    Y1: [modelSwarm getWorldSize]
	                 Color: 21]; 
	return self;

}


@end
