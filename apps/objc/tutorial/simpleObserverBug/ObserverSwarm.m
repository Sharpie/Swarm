// ObserverSwarm.m

#import "ObserverSwarm.h"
#import "ModelSwarm.h"
#import <collections.h>
#import <objectbase.h>
#import <analysis.h>
#import <gui.h>

@implementation ObserverSwarm

+ createBegin: aZone
{
  ObserverSwarm *obj;
  
  // createBegin: here we set up the default simulation parameters.

  // Superclass createBegin to allocate ourselves.

  obj = [super createBegin: aZone];

  // Fill in the relevant parameters (only one, in this case).

  obj->displayFrequency = 1;

  return obj;
}

- createEnd
{
  return [super createEnd];
}

- buildObjects
{
  [super buildObjects];
  
  // First, we create the model that we're actually observing. The
  // model is a subswarm of the observer. Note that creating the
  // modelSwarm in the current Swarm, by referring to "self" in the
  // [ModelSwarm create: self] call does actually create the new Swarm
  // in it's *own* segregated Zone. H

  // However, it does this in such a way that when current
  // (ObserverSwarm) is dropped it will also drop the ModelSwarm and all
  // its allocated storage (Zone)

  // Thus there should be no need to explicitly create any Zones,
  // except for the top level Zone, unless specifically required for a
  // particular application
  
  modelSwarm = [ModelSwarm create: self];

  // Instruct the control panel to wait for a button event.
  // We halt here until someone hits a control panel button.
   
  [controlPanel setStateStopped];

  // OK - the user said "go" so we're ready to start

  [modelSwarm buildObjects];

  // Now get down to building our own display objects.

  // First, create a colormap: this is a global resource, the information
  // here is used by lots of different objects.

  colorMap = [Colormap create: self];

  [colorMap setColor: 0 ToName: "black"];
  [colorMap setColor: 1 ToName: "red"];
  [colorMap setColor: 2 ToName: "green"];

  // Next, create a 2d window for display, set its size, zoom factor, title.

  worldRaster = [ZoomRaster create: self];
  [worldRaster setColormap: colorMap];
  [worldRaster setZoomFactor: 4];
  [worldRaster setWidth: [[modelSwarm getWorld] getSizeX]
                 Height: [[modelSwarm getWorld] getSizeY]];
  [worldRaster setWindowTitle: "Food World"];
  [worldRaster pack];                             // draw the window.

  // Now create a Value2dDisplay: this is a special object that will
  // display arbitrary 2d value arrays on a given Raster widget.

  foodDisplay = [Value2dDisplay createBegin: self];
  [foodDisplay setDisplayWidget: worldRaster colormap: colorMap];
  [foodDisplay setDiscrete2dToDisplay: [modelSwarm getFood]];
  foodDisplay = [foodDisplay createEnd];

  // And also create an Object2dDisplay: this object draws bugs on
  // the worldRaster widget for us.

  bugDisplay = [Object2dDisplay createBegin: self];
  [bugDisplay setDisplayWidget: worldRaster];
  [bugDisplay setDiscrete2dToDisplay: [modelSwarm getWorld]];
  [bugDisplay setObjectCollection: [modelSwarm getBugList]];
  [bugDisplay setDisplayMessage: M(drawSelfOn:)];   // draw method
  bugDisplay = [bugDisplay createEnd];

  return self;
}

- buildActions
{

  [super buildActions];
  
  // First, let our model swarm build its own schedule.

  [modelSwarm buildActions];

  // Create an ActionGroup for display: a bunch of things that occur in
  // a specific order, but at one step of simulation time. Some of these
  // actions could be executed in parallel, but we don't explicitly
  // notate that here.

  displayActions = [ActionGroup create: self];

  // Schedule up the methods to draw the display of the world

  [displayActions createActionTo: foodDisplay         message: M(display)];
  [displayActions createActionTo: bugDisplay          message: M(display)];
  [displayActions createActionTo: worldRaster         message: M(drawSelf)];

  [displayActions createActionTo: actionCache         message: M(doTkEvents)];

  // And the display schedule. Note the repeat interval is set from our
  // own Swarm data structure. Display is frequently the slowest part of a
  // simulation, so redrawing less frequently can be a help.

  displaySchedule = [Schedule createBegin: self];
  [displaySchedule setRepeatInterval: displayFrequency]; // note frequency!
  displaySchedule = [displaySchedule createEnd];
  [displaySchedule at: 0 createAction: displayActions];
 
  return self;
}

- activateIn: swarmContext
{
  
// activateIn: - activate the schedules so they're ready to run.
// The swarmContext argument has to do with what we were activated *in*.
// Typically the ObserverSwarm is the top-level Swarm, so it's activated
// in "nil". But other Swarms and Schedules and such will be activated
// inside of us.

  [super activateIn: swarmContext];

  // Activate the model swarm in ourselves. The model swarm is a
  // subswarm of the observer swarm.

  [modelSwarm activateIn: self];

  // Now activate our schedule in ourselves. This arranges for the
  // execution of the schedule we built.

  [displaySchedule activateIn: self];

  // Activate returns the swarm activity - the thing that's ready to run.

  return [self getSwarmActivity];
}

@end

