// Mousetraps application. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import "MousetrapObserverSwarm.h"
#import "MousetrapModelSwarm.h"
#import <collections.h>
#import <swarmobject.h>
#import <analysis.h>

// The MousetrapObserverSwarm is the top-level swarm that
// watches and reports on what's happening in the 
// MousetrapModelSwarm. It is like the lab-bench 
// on which the mousetrap world is located, along with
// the various instruments that we construct to
// monitor that world.

@implementation MousetrapObserverSwarm

// +createBegin: here we set up the default observation parameters.

//    This is a class method (note the "+"), so it adds 
//    functionality to the "factory" method for creating a GUISwarm
//    that is unique to the creation of a MousetrapObserverSwarm.
//    This method is actually invoked in main.m

+createBegin: (id) aZone {
  MousetrapObserverSwarm * obj;
  ProbeMap * probeMap;

  // invoke our superClass createBegin to allocate ourselves.
  // obj is the allocated ObserverSwarm
  
  obj = [super createBegin: aZone];

  // Fill in the relevant parameters (only one, in this case).

  obj->displayFrequency = 1;

  // Also, build a customized probe map. Without a probe map, the default
  // is to show all variables and messages. Here we choose to
  // customize the appearance of the probe, give a nicer interface.

  probeMap = [EmptyProbeMap createBegin: aZone];
  [probeMap setProbedClass: [self class]];
  probeMap = [probeMap createEnd];

  // Add in a bunch of variables, one per simulation parameter
  // and install our custom probeMap into the probeLibrary.

  [probeMap addProbe: [probeLibrary getProbeForVariable: "displayFrequency"
				    inClass: [self class]]];
  [probeLibrary setProbeMap: probeMap For: [self class]];

  return obj;		// We return the newly created ObserverSwarm
}

// createEnd: create objects we know we'll need. In this case, none,
// but you might want to override this.

-createEnd {
  return [super createEnd];
}


// Create the objects used in the display of the model. 
// Here, we create the objects used in the experiment
// Primairly, the Mousetrap model itself, but also the
// various instrumentation that observes the model. 

-buildObjects {
  id modelZone;					// zone for model
  int x, y, size;

  // Let our superClass build any objects it needs to first
  
  [super buildObjects];

  // Then, we create the model that we're actually observing. The
  // model is a subswarm of the observer. We create the model in
  // its own zone, so storage is segregated.
  
  modelZone = [Zone create: [self getZone]];
  mousetrapModelSwarm = [MousetrapModelSwarm create: modelZone];

  // Now create probe objects on the model and ourselves. This gives a
  // simple user interface to let the user change parameters.
  
  [probeDisplayManager createProbeDisplayFor: mousetrapModelSwarm];
  [probeDisplayManager createProbeDisplayFor: self];

  // Instruct the control panel to wait for a button event: we halt here
  // until someone hits a control panel button so the user can get a
  // chance to fill in parameters before the simulation runs
  // If the user hit the quit button here, we quit.

  [controlPanel waitForControlEvent];
  if ([controlPanel getState] == ControlStateQuit)
    return self;

  // OK - the user has specified all the parameters for the simulation.
  // Now we're ready to start.

  // First, let the model swarm build its objects.
  
  [mousetrapModelSwarm buildObjects];

  // create a colormap

  colormap = [XColormap create: globalZone];
  [colormap setColor: 1 ToGrey: 0.3];
  [colormap setColor: 2 ToName: "red"];

  // set up the EZGraph object
  
  triggerGraph = [EZGraph createBegin: [self getZone]];
  [triggerGraph setTitle: "Trigger data vs. time"];
  [triggerGraph setAxisLabelsX: "time" Y: "number triggered"];
  triggerGraph = [triggerGraph createEnd] ;

  // Now, we create two observations to plot on mousetraps with time:
  //    1) The total number of traps triggered so far
  //    2) The current pending trigger events (number of ping-pong
  //           balls still "in the air")

  [triggerGraph createSequence: "Total triggered"
                         withFeedFrom: [mousetrapModelSwarm getStats] 
                          andSelector: M(getNumTriggered)] ;

  [triggerGraph createSequence: "Pending triggers"
                         withFeedFrom: [mousetrapModelSwarm getStats] 
                          andSelector: M(getNumBalls)] ;

  // Next, create a 2d window to display the mousetrap world
  // and  set its size, zoom factor, title.

  displayWindow = [ZoomRaster create: globalZone];
  [displayWindow setColormap: colormap];
  [displayWindow setZoomFactor: 6];
  [displayWindow setWidth: [mousetrapModelSwarm getGridSize]
		 Height: [mousetrapModelSwarm getGridSize]];
  [displayWindow setWindowTitle: "Mousetrap World"];

  // draw all the mousetraps 

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

  // Also create an Object2dDisplay: this object is just used
  // to receive and handle probes on mousetraps.

  // Note that we currently have to set a DisplayMessage, but
  // we don't use it - this will get fixed, as Object2dDisplays 
  // have other uses besides "displaying".
  // Compare with Heatbugs, in which we have a collection of heatbugs
  // to display all at once. In mousetraps, traps display themselves
  // when they are triggered (as well as when they are initialized, above).
  // This is a little unclean as is, but the main point of this
  // app is to illustrate dynamic scheduling, so we'll let it slide
  // for now...

  mousetrapDisplay = [Object2dDisplay createBegin: [self getZone]];
  [mousetrapDisplay setDisplayWidget: displayWindow];
  [mousetrapDisplay setDiscrete2dToDisplay: [mousetrapModelSwarm getWorld]];
  [mousetrapDisplay setDisplayMessage: M(drawSelfOn:)];   // draw method
  mousetrapDisplay = [mousetrapDisplay createEnd];

  // And tell the displayWindow to send mouse clicks to the mousetrapDisplay
  // this allows the user to right-click on the display to probe the traps.

  [displayWindow setButton: ButtonRight 
		 Client:    mousetrapDisplay
                 Message:   M(makeProbeAtX:Y:)];

  // All done - we're ready to build a schedule and go
  
  return self;
}  

// Create the actions necessary for the simulation. This is where
// the schedule is built (but not run!)

// Here we create a display schedule - this is used to display the
// state of the world and check for user input. This schedule should
// be thought of as independent from the model - in particular, you
// will also want to run the model without any display.


-buildActions {

  // First, let our superclass build any actions

  [super buildActions];

  // Then, let our model swarm build its own schedule
  
  [mousetrapModelSwarm buildActions];

  // Create an ActionGroup for display: a bunch of things that occur in
  // a specific order, but in the same step of simulation time. Some of 
  // these actions could be executed in parallel, but we don't explicitly
  // notate that here.

  displayActions = [ActionGroup create: [self getZone]];

  // Schedule up the methods to draw the display of the world
  //  and to update the graph of the statistics we're keeping

  [displayActions createActionTo: displayWindow       message: M(drawSelf)];
  [displayActions createActionTo: triggerGraph	      message: M(step)];

  // Schedule the update of the probe display

  [displayActions createActionTo: probeDisplayManager message: M(update)];

  // Check to see if the simulation has ended (all the balls have landed)

  [displayActions createActionTo: self                message: M(checkToStop)];

  // Finally, schedule an update for the whole user interface code.
  // This is crucial: without this, no graphics update and the control
  // panel will be dead. It's best to put it at the end of the display schedule

  [displayActions createActionTo: controlPanel        message: M(doTkEvents)];

  // And the display schedule. Note the repeat interval is set from our
  // own Swarm data structure. Display is frequently the slowest part of a
  // simulation, so redrawing less frequently can be a help.
  
  displaySchedule = [Schedule createBegin: [self getZone]];
  [displaySchedule setRepeatInterval: displayFrequency];
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

  // Activate the model swarm in ourselves. The model swarm is a
  // subswarm of the observer swarm.

  [mousetrapModelSwarm activateIn: self];

  // Now activate our schedule in ourselves. This arranges for the
  // execution of the schedule we built.

  [displaySchedule activateIn: self];

  // Activate returns the swarm activity - the thing that's ready to run,
  // which is the activity tree rooted in "nil" that we've just added
  // ourselves onto as new leaves
  
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
