// By Sven Thommesen <sthomme@humsci.auburn.edu>
// "HumbugBatchSwarm.h"

// Purpose: create the ModelSwarm and run it
// with parameters set by the user.

#import <stdio.h>

#import "HumbugModelSwarm.h"

@interface HumbugBatchSwarm : Swarm {

   int displayFrequency;

   HumbugModelSwarm * humbugModelSwarm;

   id displayActions;
   id displaySchedule;
   id stopSchedule;

}

// Create the object:
+createBegin: (id) aZone;

// Do the usual Swarm things:
-buildObjects;
-buildActions;
-activateIn: (id) swarmContext;

// Special messages only for batch runs:
-go;
-stopRunning;

@end
