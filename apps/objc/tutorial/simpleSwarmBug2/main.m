// simpleSwarmBug2

// We extend the model in this version, but......
//  ......not here! Our model is now encapsulated in
// the ModelSwarm, so any extensions will now happen
// there. This main.m is exactly the same as the previous
// one: We just build and start the modelSwarm

#import <simtools.h>                
#import <activity.h>
#import "ModelSwarm.h"

int
main(int argc, char ** argv) {
  ModelSwarm * modelSwarm;

  // Swarm initialization: all Swarm apps must call this first.

  initSwarm(argc, argv);

  // Make the bug model swarm

  modelSwarm =  [ModelSwarm create: globalZone];

  // Now send messages to the newly created swarm telling it
  // to build its internal objects and its schedule.
  // Then activate the swarm.

  [modelSwarm buildObjects];
  [modelSwarm buildActions];
  [modelSwarm activateIn: nil];	// Top-level swarm is activated in nil
  
  // Now the swarm is built, activated, and ready to go...

  [[modelSwarm getActivity] run];

  // the swarm has finished (terminated), so it's time to quit

  return 0;

}


