// Heatbugs application. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import "HeatbugModelSwarm.h"
#import <simtools.h>

@implementation HeatbugModelSwarm

// These methods provide access to the objects inside the ModelSwarm.
// These objects are the ones visible to other classes via message call.
// In theory we could just let other objects use Probes to read our state,
// but message access is frequently more convenient.

-getHeatbugList {
  return heatbugList;
}

-getWorld {
  return world;
}

-getHeat {
  return heat;
}

// This method isn't normally used, but is convenient when running probes:
// it lets you easily clone a heatbug and drag it into the model.

-addHeatbug: (Heatbug *) bug {
  [heatbugList addLast: bug];
  return self;
}

// createBegin: here we set up the default simulation parameters.

+createBegin: (id) aZone {
  HeatbugModelSwarm * obj;
  id <ProbeMap> probeMap;

  // First, call our superclass createBegin - the return value is the
  // allocated HeatbugModelSwarm object.

  obj = [super createBegin: aZone];

  // Now fill in various simulation parameters with default values.

  obj->numBugs = 100;
  obj->evaporationRate = 0.99;
  obj->diffuseConstant = 1.0;
  obj->worldXSize = 80;
  obj->worldYSize = 80;
  obj->minIdealTemp = 17000;
  obj->maxIdealTemp = 31000;
  obj->minOutputHeat = 3000;
  obj->maxOutputHeat = 10000;
  obj->randomMoveProbability = 0.0;

  // And build a customized probe map. Without a probe map, the default
  // is to show all variables and messages. Here we choose to
  // customize the appearance of the probe, give a nicer interface.

  probeMap = [EmptyProbeMap createBegin: aZone];
  [probeMap setProbedClass: [self class]];
  probeMap = [probeMap createEnd];

  // Add in a bunch of variables, one per simulation parameter

  [probeMap addProbe: [probeLibrary getProbeForVariable: "numBugs"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "diffuseConstant"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "worldXSize"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "worldYSize"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "minIdealTemp"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "maxIdealTemp"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "minOutputHeat"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "maxOutputHeat"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "randomMoveProbability"
				    inClass: [self class]]];

  // And one method, the "addHeatbug" method for a probe demo.
  // we also hide the return value for this message probe, just for nicety.

  [probeMap addProbe: [[probeLibrary getProbeForMessage: "addHeatbug:"
			     inClass: [self class]]
			setHideResult: 1]];

  // Now install our custom probeMap into the probeLibrary.

  [probeLibrary setProbeMap: probeMap For: [self class]];
  
  return obj;
}

// createEnd: we could create some objects here if we knew we needed
// them. But this method is called before the user is allowed to fill
// in any customization of the model, so we defer most object creation
// to later. (In this example, this method does nothing at all and could
// just be inherited. But it's here to show you a place to customize.)

-createEnd {
  return [super createEnd];
}

// Now it's time to build the model objects. We use various parameters
// inside ourselves to choose how to create things.

-buildObjects {
  int i;

  // allow our parent class to build anything.

  [super buildObjects];
  
  // First, set up objects used to represent the environment.
  // The heatspace agent represents the spatial property of heat.
  // It is initialized via various model parameters.

  heat = [HeatSpace createBegin: [self getZone]];
  [heat setSizeX: worldXSize Y: worldYSize];
  [heat setDiffusionConstant: diffuseConstant];
  [heat setEvaporationRate: evaporationRate];
  heat = [heat createEnd];

  // Now set up the grid used to represent agent position

  world = [Grid2d createBegin: [self getZone]];
  [world setSizeX: worldXSize Y: worldYSize];
  world = [world createEnd];

  // Create a list to keep track of the heatbugs in the model.

  heatbugList = [List create: [self getZone]];
  
  // Create heatbugs themselves. This is a fairly complex step, as is
  // appropriate: the heatbugs are essential aspects of the simulation.

  // First, a quick hack. During creation we might put several heatbugs
  // in the same square. This is a design flaw, but it's one that's not
  // fatal, so we ask the world object not to warn us about it. This is
  // not an example to be emulated :-)

  [world setOverwriteWarnings: 0];

  // Now a loop to create a bunch of heatbugs.

  for (i = 0; i < numBugs; i++) {
    Heatbug * hbug;
    int idealTemp, outputHeat;

    // Choose a random ideal temperature, output heat from the specified
    // range (model parameters).

//    idealTemp = [uniformRandom rMin: minIdealTemp Max: maxIdealTemp];
//    outputHeat = [uniformRandom rMin: minOutputHeat Max: maxOutputHeat];

    idealTemp = [uniformIntRand
                  getIntegerWithMin: minIdealTemp withMax: maxIdealTemp];
    outputHeat = [uniformIntRand
                   getIntegerWithMin: minOutputHeat withMax: maxOutputHeat];


    // Create the heatbug, set the creation time variables

    hbug = [Heatbug createBegin: [self getZone]];
    [hbug setWorld: world Heat: heat];
    hbug = [hbug createEnd];

    // Add the bug to the end of the list.

    [heatbugList addLast: hbug];

    // Now initialize the rest of the heatbug's state.

    [hbug setIdealTemperature: idealTemp];
    [hbug setOutputHeat: outputHeat];

//    [hbug setX: [uniformRandom rMax: worldXSize]  // random position
//	  Y: [uniformRandom rMax: worldYSize]];

    [hbug setX: [uniformIntRand
                  getIntegerWithMin: 0L
                  withMax: (worldXSize-1)]  // random position
          Y: [uniformIntRand getIntegerWithMin: 0L withMax: (worldYSize-1)]];
  }
  [world setOverwriteWarnings: 1];		  // ok, done cheating.

  return self;
}

// Here is where the model schedule is built, the data structures
// that define the simulation of time in the mode. The core is an
// actionGroup that has a list of actions. That's then put in a Schedule.

-buildActions {

  [super buildActions];
  
  // Create the list of simulation actions. We put these in an action
  // group, because we want these actions to be executed in a specific
  // order, but these steps should take no (simulated) time. The
  // M(foo) means "The message called <foo>". You can send a message
  // To a particular object, or ForEach object in a collection.

  // Note we update the heatspace in two phases: first run diffusion,
  // then run "updateWorld" to actually enact the changes the heatbugs
  // have made. The ordering here is significant!

  modelActions = [ActionGroup create: [self getZone]];
  [modelActions createActionTo:      heat        message: M(stepRule)];
  [modelActions createActionForEach: heatbugList message: M(step)];  
  [modelActions createActionTo:      heat        message: M(updateLattice)];

  // Then we create a schedule that executes the modelActions. modelActions
  // is an ActionGroup, by itself it has no notion of time. In order to
  // have it executed in time, we create a Schedule that says to use
  // the modelActions ActionGroup at particular times.
  // This schedule has a repeat interval of 1, it will loop every time step.
  // The action is executed at time 0 relative to the beginning of the loop.

  // This is a simple schedule, with only one action that is just
  // repeated every time. See mousetraps for more complicated schedules.
  
  modelSchedule = [Schedule createBegin: [self getZone]];
  [modelSchedule setRepeatInterval: 1];
  modelSchedule = [modelSchedule createEnd];
  [modelSchedule at: 0 createAction: modelActions];

  return self;
}

// Now set up the model's activation. swarmContext indicates where
// we're being started in - typically, this model is run as a subswarm
// of an observer swarm.

-activateIn: (id) swarmContext {

  // First, activate ourselves via the superclass activateIn: method.
  // Just pass along the context: the activity library does the right thing.

  [super activateIn: swarmContext];

  // Now activate our own schedule.

  [modelSchedule activateIn: self];

  // Finally, return our activity.

  return [self getActivity];
}

@end
