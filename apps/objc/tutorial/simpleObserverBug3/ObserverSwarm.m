 // ObserverSwarm.m

#import "ObserverSwarm.h"
#import "ModelSwarm.h"
#import <activity.h>
#import <simtoolsgui.h>
#import "Parameters.h"

@implementation ObserverSwarm

+ createBegin: aZone
{
  ObserverSwarm *obj;
  id <ProbeMap> probeMap;

  // createBegin: here we set up the default simulation parameters.

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

  [probeMap addProbe: [probeLibrary getProbeForMessage: "takeScreenShot"
				    inClass: [self class]]];

  // Now install our custom probeMap into the probeLibrary.

  [probeLibrary setProbeMap: probeMap For: [self class]];

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
  // model is a subswarm of the observer. 

  // Make the bug model swarm, creating the object from the default
  // `lispAppArchiver' instance which looks for the `bug.scm'
  // datafile.


  if ((modelSwarm = 
       [lispAppArchiver getWithZone: self key: "modelSwarm"]) == nil)
    raiseEvent(InvalidOperation,
               "Can't find the modelSwarm parameters");

  // Now create probe objects on the model and ourselves. This gives a
  // simple user interface to let the user change parameters.

  //CREATE_PROBE_DISPLAY (modelSwarm);
  CREATE_ARCHIVED_PROBE_DISPLAY (self);

  // Instruct the control panel to wait for a button event.
  // We halt here until someone hits a control panel button.

  // Now that we're using Probes, the user can set the parameters
  // in the ModelSwarm probe window - we halt here to allow
  // the user to change parameters.

#ifdef UNATTENDED

    [controlPanel setStateRunning];

#else

    [controlPanel setStateStopped];

#endif

  // When the user hits "go" on the control panel, we resume here

  // OK - the user said "go" so we're ready to start

    [modelSwarm resetParameters];
    [modelSwarm buildObjects];

  // Now get down to building our own display objects.

  // First, create a colormap: this is a global resource, the information
  // here is used by lots of different objects.

  colorMap = [Colormap create: self];

  [colorMap setColor: 0 ToName: "black"];
  [colorMap setColor: 1 ToName: "red"];
  [colorMap setColor: 2 ToName: "green"];

  // Next, create a 2d window for display, set its size, zoom factor, title.

  worldRaster = [ZoomRaster create: self setWindowGeometryRecordName: "bugRaster"];
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

  // Also, tell the world raster to send mouse clicks to the heatbugDisplay
  // this allows the user to right-click on the display to probe the bugs.

  [worldRaster setButton: ButtonRight
               Client: bugDisplay
               Message: M(makeProbeAtX:Y:)];

  return self;
}

- takeScreenShot
{
  char filename[40];
  id aPix;
  sprintf (filename, "screenRun%03dTime%07ld.png",getInt((Parameters*)arguments,"run"),currentTime);
  [actionCache doTkEvents];
  
  aPix =[ Pixmap createBegin: self];
   [aPix setWidget: nil];
   [aPix    setDecorationsFlag: NO];
    aPix= [aPix createEnd];
   [aPix save: filename];
   [aPix drop];

  return self;
}

- stopRunning
{
  // If compiled with the EXTRACPPFLAG=-DUNATTENDED, a picture will be
  // saved in the current directory

  currentTime = getCurrentTime();

	if(currentTime == getInt((Parameters*)arguments,"experimentDuration"))
	{
#ifdef UNATTENDED
  //First, we need a picture 
  [self takeScreenShot];
 // This makes the simulation activity stop and Swarm quits.	
  [controlPanel setStateQuit];
#else

  // If you don't define UNATTENDED, then the simulation just pauses here,
  // and user intervention with the control panel can start it again.
  [controlPanel setStateStopped];

#endif

	}

  return self;
}


- buildActions
{

// Create the actions necessary for the simulation. 

  [super buildActions];

  // First, let our model swarm build its own schedule.

  [modelSwarm buildActions];

  // Create an ActionGroup for display. 

  displayActions = [ActionGroup create: self];

  // Schedule up the methods to draw the display of the world

  [displayActions createActionTo: foodDisplay         message: M(display)];
  [displayActions createActionTo: bugDisplay          message: M(display)];
  [displayActions createActionTo: worldRaster         message: M(drawSelf)];

  [displayActions createActionTo: actionCache         message: M(doTkEvents)];
  [displayActions createActionTo: probeDisplayManager message: M(update)];     

  // And the display schedule. Note the repeat interval is set from our
  // own Swarm data structure. Display is frequently the slowest part of a
  // simulation, so redrawing less frequently can be a help.

  displaySchedule = [Schedule createBegin: self];
  [displaySchedule setRepeatInterval: displayFrequency]; // note frequency!
  displaySchedule = [displaySchedule createEnd];
  [displaySchedule at: 0 createAction: displayActions];

  //if we might like to let this model run unattended, it must be able
  //to stop on its own.  This is a one-time schedule that pops up and 
  //stops the simulation when it reaches the "duration".


    stopSchedule = [Schedule createBegin: [self getZone]];
    [stopSchedule setRepeatInterval: 1];
    stopSchedule = [stopSchedule createEnd];
    [stopSchedule at: 0 createActionTo: self message: M(stopRunning)];

 
  return self;
}

- activateIn: swarmContext
{
// activateIn: - activate the schedules so they're ready to run.

  [super activateIn: swarmContext];

  // Activate the model swarm in ourselves. The model swarm is a
  // subswarm of the observer swarm.

  [modelSwarm activateIn: self];

  // Now activate our schedule in ourselves. This arranges for the
  // execution of the schedule we built.

  [displaySchedule activateIn: self];

  // Introduce the stopSchedule
  
  [stopSchedule activateIn: self];

  // Activate returns the swarm activity - the thing that's ready to run.

  return [self getSwarmActivity];
}

@end








