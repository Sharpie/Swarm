// main.m 					simpleExperBug

#import "ExperSwarm.h"
#import <simtools.h>                // ... for initSwarm () 

int
main (int argc, const char **argv)
{
  ExperSwarm *experSwarm;

  initSwarm (argc, argv);

  // Make the experiment swarm

  experSwarm =  [ExperSwarm createBegin: globalZone];
  SET_WINDOW_GEOMETRY_RECORD_NAME (experSwarm);
  experSwarm = [experSwarm createEnd];

  // Now send messages to the newly created swarm telling it
  // to build its internal objects and its schedule.
  // Then activate the swarm.

  [experSwarm buildObjects];
  [experSwarm buildActions];
  [experSwarm activateIn: nil];		// Top-level swarm is activated in nil
  
  // Now the swarm is built, activated, and ready to go...

  // We tell the swarm itself to go, instead of an activity
  // because the experSwarm is a GUI-swarm, and has its
  // own controlPanel that we can talk to.

  [experSwarm go];

  // the swarm has finished (terminated), so it's time to quit

  return 0;

}


