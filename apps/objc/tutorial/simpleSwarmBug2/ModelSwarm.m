// ModelSwarm.m

#import "ModelSwarm.h"
#import "Bug.h"
#import <simtools.h>
#import <collections.h>

@implementation ModelSwarm  

+createBegin: (id) aZone {
  ModelSwarm * obj;

  obj = [super createBegin: aZone];

  // Fill in various simulation parameters with default values.

  obj->worldXSize = 80;
  obj->worldYSize = 80;
  obj->seedProb   = 0.8;
  obj->bugDensity = 0.1;

  // return the id of the newly created Swarm

  return obj;
}

-createEnd {
  return [super createEnd];
}

-buildObjects {
  Bug * aBug;
  int x,y;
  
  // First, create the foodWorld and initialize it

  food = [FoodSpace createBegin: [self getZone]];
  [food setSizeX: worldXSize Y: worldYSize];
  food = [food createEnd];
  
  [food seedFoodWithProb: seedProb];

  // Now set up the grid used to represent agent position

  world = [Grid2d createBegin: [self getZone]];
  [world setSizeX: worldXSize Y: worldYSize];
  world = [world createEnd];
  [world fillWithObject: nil];

  // Now, create a bunch of bugs to live in the world

  // First, we create a List object to manage the bugs
  // for us.

  bugList = [List create: [self getZone]];

  // Then, we iterate over the possible sites in the world,
  // with a certain probability of creating a bug at 
  // each site.

  for (y = 0; y < worldYSize; y++)
    for (x = 0; x < worldXSize; x++) 
      if ([uniformDblRand getDoubleWithMin: 0.0 withMax: 1.0] < bugDensity) {

         aBug = [Bug createBegin: [self getZone]];
         [aBug setWorld: world Food: food];
         aBug = [aBug createEnd];
         [aBug setX: x Y: y];

         [bugList addLast: aBug];
      }

  // enlist a "reporter" bug to let us know how things are going
  // We just pop the first bug we created and then return it

  reportBug = [bugList removeFirst];
  [bugList addFirst: reportBug];

  return self;
}


-buildActions {

  // Create an ActionGroup to hold the messages over the bugs

  modelActions = [ActionGroup create: [self getZone]];
  [modelActions createActionForEach: bugList    message: M(step)];
  [modelActions createActionTo:      reportBug 	message: M(report)];

  // Make a schedule and insert the ActionGroup as the only action

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

  return [self getActivity];

}

@end



  


