// simpleSwarmBug


#import <simtools.h>                
#import "ModelSwarm.h"

int
main (int argc, const char **argv)
{
  id modelSwarm;

  initSwarm (argc, argv);

  // Make the model swarm

  modelSwarm =  [ModelSwarm create: globalZone];

  // Now send messages to the newly created swarm telling it
  // to build its internal objects and its activities.
  // Then activate the swarm.

  [modelSwarm buildObjects];
  [modelSwarm buildActions];
  [modelSwarm activateIn: nil];		// Top-level swarm is activated in nil
  
  // Now the swarm is built, activated, and ready to go...
  // so "run" it.

  [[modelSwarm getActivity] run];

  // the swarm has finished (terminated), so it's time to quit

  return 0;

}


