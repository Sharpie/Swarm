// ModelSwarm.m					simpleExperBug 

#import "ModelSwarm.h"
#import <activity.h>
#import <collections.h>
#import <random.h>

@implementation ModelSwarm  

// These methods provide access to the objects inside the ModelSwarm.

- getBugList
{
  return bugList;
}

- getWorld
{
  return world;
}

- getFoodSpace
{
  return foodSpace;
}

- (int)getTime
{
   return time;
}

- setWorldXSize: (int)x YSize: (int)y
{
  worldXSize = x;
  worldYSize = y;
  return self;
}

- setSeedProb: (float)s bugDensity: (float)b
{
  seedProb = s;
  bugDensity = b;
  return self;
}


+ createBegin: aZone
{
  ModelSwarm *obj;

  // First, call our superclass createBegin - the return value is the
  // allocated ModelSwarm instance.

  obj = [super createBegin: aZone];

  // Now fill in various simulation parameters with default values.
  // Note: the ParameterManager will override these settings

  obj->worldXSize = 80;
  obj->worldYSize = 80;
  obj->seedProb   = 0.5;
  obj->bugDensity = 0.1;

  // We've created the modelSwarm and initialized it.
  // return the id of the newly created Swarm

  return obj;
}

- createEnd
{
  return [super createEnd];
}

- buildObjects
{
  Bug *aBug;
  int x,y;
  
  // Here, we create the objects in the model

  // First, create the food space and initialize it

  foodSpace = [FoodSpace createBegin: self];
  [foodSpace setSizeX: worldXSize Y: worldYSize];
  foodSpace = [foodSpace createEnd];
  
  [foodSpace seedFoodWithProb: seedProb];

  // Now set up the grid used to represent agent position
  // Grid2d enforces only 1 bug per site

  world = [Grid2d createBegin: self];
  [world setSizeX: worldXSize Y: worldYSize];
  world = [world createEnd];

  [world fillWithObject: nil];

  // Now, create a bunch of bugs to live in the world 
  //      and a list to manage them

  bugList = [List create: self];

  for (y = 0; y < worldYSize; y++)
    for (x = 0; x < worldXSize; x++) 
      if ([uniformDblRand getDoubleWithMin: 0.0 withMax: 1.0] <= bugDensity)
        {
          aBug = [Bug createBegin: self];
          [aBug setWorld: world Food: foodSpace];
          aBug = [aBug createEnd];
          
          [aBug setX: x Y: y];
          [world putObject: aBug atX: x Y: y];
          
          [bugList addLast: aBug];
        }
  
  time = 0;
  
  return self;
}


- buildActions
{
  // Create the list of simulation actions. 
  
  modelActions = [ActionGroup create: self];
  
  [modelActions createActionForEach: bugList    message: M(step)];
  [modelActions createActionTo:      self	message: M(checkToStop)];
  
  // Then we create a schedule that executes the modelActions. 
  
  modelSchedule = [Schedule createBegin: self];
  [modelSchedule setRepeatInterval: 1];
  modelSchedule = [modelSchedule createEnd];
  [modelSchedule at: 0 createAction: modelActions]; 
  
  return self;
}


- activateIn: swarmContext
{
  // Here, we activate the swarm in the context passed in
  // Then we activate our schedule in ourselves

  // When running under ExperSwarm, swarmContext = nil

  [super activateIn: swarmContext];
  [modelSchedule activateIn: self];

  return [self getActivity];
}


-checkToStop {

  // If the bugs have eaten all the food, the model run is over. 

  if ([foodSpace getFood] <= 0)
    [[self getActivity] terminate];

  // if not, increment time and continue running the model

  time++ ;

  return self;
}

@end



  


