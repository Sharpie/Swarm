// By Sven Thommesen <sthomme@humsci.auburn.edu>
// "HumbugBatchSwarm.m"

#import <collections.h>
#import <analysis.h>

#import "HumbugBatchSwarm.h"

@implementation HumbugBatchSwarm

+createBegin: (id) aZone {
  HumbugBatchSwarm * obj;

  obj = [super createBegin: aZone];

  obj->displayFrequency = 1;
  return obj;
}

-buildObjects {
  id modelZone;

  [super buildObjects];

  modelZone = [Zone create: [self getZone]];
  humbugModelSwarm = [HumbugModelSwarm create: modelZone];

  [humbugModelSwarm buildObjects];

  return self;
}  

-buildActions {
  [super buildActions];
  
  [humbugModelSwarm buildActions];
  
  stopSchedule = [Schedule create: [self getZone]];
  [stopSchedule at: 5 createActionTo: self message: M(stopRunning)];
  
  return self;
}  

-activateIn: (id) swarmContext {

  [super activateIn: swarmContext];

  [humbugModelSwarm activateIn: self];

  [stopSchedule activateIn: self];

  return [self getSwarmActivity];
}

-go {

  // printf(
  // "You specified -batchmode, so we're running without graphics.\n");

  [ [ self getActivity] run ];

  return [ [ self getActivity] getStatus ];

}


-stopRunning {
  [getTopLevelActivity() terminate];
  return self;
}

@end
