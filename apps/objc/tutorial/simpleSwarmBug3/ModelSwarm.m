// ModelSwarm.m					SimpleBug app

#import "ModelSwarm.h"
#import "Bug.h"
#import <simtools.h>
#import <collections.h>

@implementation ModelSwarm  

+createBegin: (id) aZone {
  ModelSwarm * obj;

  // in createBegin, we set up the simulation parameters

  // First, call our superclass createBegin - the return value is the
  // allocated BugSwarm object.

  obj = [super createBegin: aZone];

  // Now fill in various simulation parameters with default values.

  obj->worldXSize = 80;
  obj->worldYSize = 80;
  obj->seedProb   = 0.5;
  obj->bugDensity = 0.1;

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

  // First, we load our parameters from a file so we don't have 
  // to recompile everytime we want to change something

  [ObjectLoader load: self fromFileNamed: "model.setup"] ;

  // Then, create the food space and initialize it

  food = [FoodSpace createBegin: self];
  [food setSizeX: worldXSize Y: worldYSize];
  food = [food createEnd];
  
  [food seedFoodWithProb: seedProb];

  // Now set up the grid used to represent agent position
  // Grid2d enforces only 1 bug per site

  world = [Grid2d createBegin: self];
  [world setSizeX: worldXSize Y: worldYSize];
  world = [world createEnd];
  [world fillWithObject: nil];

  // Now, create a bunch of bugs to live in the world

  bugList = [List create: self];

  for (y = 0; y < worldYSize; y++)
    for (x = 0; x < worldXSize; x++) 
      if ([uniformDblRand getDoubleWithMin: 0.0 withMax: 1.0] < bugDensity) {

         aBug = [Bug createBegin: self];
         [aBug setWorld: world Food: food];
         aBug = [aBug createEnd];
         [aBug setX: x Y: y];

         [bugList addLast: aBug];
      }

  reportBug = [bugList removeFirst];
  [bugList addFirst: reportBug];

  return self;
}

-buildActions {

  // Create actionGroup

  modelActions = [ActionGroup create: self];
  [modelActions createActionForEach: bugList    message: M(step)];
  [modelActions createActionTo:      reportBug 	message: M(report)];

  // This is a simple schedule, with only one action that is just
  // repeated every time. See mousetraps for more complicated schedules.

  modelSchedule = [Schedule createBegin: self];
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



  


