// ModelSwarm.m					SimpleBug app

#import "ModelSwarm.h"
#import <simtools.h>

@implementation ModelSwarm

+createBegin: (id) aZone {
  ModelSwarm * obj;

  // First, call our superclass createBegin - the return value is the
  // allocated ModelSwarm object.

  obj = [super createBegin: aZone];

  // Now fill in various simulation parameters with default values.

  obj->worldXSize = 80;
  obj->worldYSize = 80;
  obj->seedProb = 0.1;
  obj->xPos = 40;
  obj->yPos = 40;

  // We've created the ModelSwarm and initialized it.
  // Now we can return the id of the newly created Swarm
  // to whoever asked us for one, and go back to the more
  // normal responses of an object to messages....
  // Creation is always strange....

  return obj;
}

-createEnd {
  return [super createEnd];

// createEnd is a place to do any last minute tidying up after
// the creator is done - via more normal modes of operation
// However, we don't do anything here - except allow our
// super-class to clean up if it wants to....

}

-buildObjects {

  // Here, we create the objects in the model
  // This is what we did in main.m in the previous model

  // First, create the foodWorld and initialize it

  foodSpace = [FoodSpace createBegin: globalZone];
  [foodSpace setSizeX: worldXSize Y: worldYSize];
  foodSpace = [foodSpace createEnd];

  [foodSpace seedFoodWithProb: seedProb];

  // Then, create a bug to live in the world

  aBug = [Bug createBegin: globalZone];
  [aBug setWorldSizeX: worldXSize Y: worldYSize];
  [aBug setFoodSpace: foodSpace];
  aBug = [aBug createEnd];

  [aBug setX: xPos Y: yPos];

  return self;
}

-buildActions {


  // Build a simple schedule to send a "step" message to 
  // the bug every time step

  modelSchedule = [Schedule createBegin: self];
  [modelSchedule setRepeatInterval: 1];
  modelSchedule = [modelSchedule createEnd];
  [modelSchedule at: 0 createActionTo: aBug  message: M(step)];

  return self;

}

-activateIn: (id) swarmContext {

  // Activate ourselves in swarmContext

  [super activateIn: swarmContext];

  // and activate the schedule we built above in ourselves

  [modelSchedule activateIn: self];

  // Return the activity we built 

  return [self getActivity];

}

@end



  


