// ModelSwarm.m					SimpleBug app

#import "ModelSwarm.h"
#import <simtools.h>

@implementation ModelSwarm  

// These methods provide access to the objects inside the ModelSwarm.
// These objects are the ones visible to other classes via message call.
// In theory we could just let other objects use Probes to read our state,
// but message access is frequently more convenient.

-getBugList {
  return bugList;
}

-getWorld {
  return world;
}

-getFood {
  return food;
}

+createBegin: (id) aZone 
{
  ModelSwarm * obj;
  id <ProbeMap> probeMap;

  // in createBegin, we set up the simulation parameters

  // First, call our superclass createBegin - the return value is the
  // allocated BugSwarm object.

  obj = [super createBegin: aZone];

  // Now fill in various simulation parameters with default values.

  obj->worldXSize = 80;
  obj->worldYSize = 80;
  obj->seedProb   = 0.5;
  obj->bugDensity = 0.1;

  // And build a customized probe map. Without a probe map, the default
  // is to show all variables and messages. Here we choose to
  // customize the appearance of the probe, give a nicer interface.

  probeMap = [EmptyProbeMap createBegin: aZone];
  [probeMap setProbedClass: [self class]];
  probeMap = [probeMap createEnd];

  // Add in a bunch of variables, one per simulation parameter

  [probeMap addProbe: [probeLibrary getProbeForVariable: "worldXSize"
                                    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "worldYSize"
                                    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "seedProb"
                                    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "bugDensity"
                                    inClass: [self class]]];

  // Now install our custom probeMap into the probeLibrary.

  [probeLibrary setProbeMap: probeMap For: [self class]];
  
  // We've created the BugSwarm and initialized it.
  // return the id of the newly created Swarm

  return obj;
}

-createEnd {
  return [super createEnd];
}

-buildObjects {
  Bug * aBug;
  int x,y;
  
  // Here, we create the objects in the model

  // Then, create the food space and initialize it

  food = [FoodSpace createBegin: [self getZone]];
  [food setSizeX: worldXSize Y: worldYSize];
  food = [food createEnd];
  
  [food seedFoodWithProb: seedProb];

  // Now set up the grid used to represent agent position
  // Grid2d enforces only 1 bug per site

  world = [Grid2d createBegin: [self getZone]];
  [world setSizeX: worldXSize Y: worldYSize];
  world = [world createEnd];
  [world fillWithObject: nil];

  // Now, create a bunch of bugs to live in the world

  bugList = [List create: [self getZone]];

  for (y = 0; y < worldYSize; y++)
    for (x = 0; x < worldXSize; x++) 
      if ([uniformDblRand getDoubleWithMin: 0.0 withMax: 1.0] < bugDensity) {

         aBug = [Bug createBegin: [self getZone]];
         [aBug setWorld: world Food: food];
         aBug = [aBug createEnd];
         [aBug setX: x Y: y];

         [bugList addLast: aBug];
      }

  return self;
}

-buildActions {

  // Create the list of simulation actions. We put these in an action
  // group, because we want these actions to be executed in a specific
  // order, but these steps should take no (simulated) time. The
  // M(foo) means "The message called <foo>". You can send a message
  // To a particular object, or ForEach object in a collection.

  modelActions = [ActionGroup create: [self getZone]];
  [modelActions createActionForEach: bugList    message: M(step)];

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

-activateIn: (id) swarmContext {

  // Here, we activate the swarm in the context passed in
  // Then we activate our schedule in ourselves

  [super activateIn: swarmContext];

  [modelSchedule activateIn: self];

  return [self getSwarmActivity];

}

@end



  


