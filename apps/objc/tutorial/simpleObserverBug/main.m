// simpleBug - tutorial application


#import <simtools.h>                // ... for initSwarm() 
#import "ObserverSwarm.h"

// The main() function is the top-level place where everything starts.
// For a typical Swarm simulation, in main() you create a toplevel
// Swarm, let it build and activate, and set it to running.

int
main (int argc, const char ** argv)
{
  ObserverSwarm * observerSwarm;

  // Swarm initialization: all Swarm apps must call this first.

  initSwarm (argc, argv);

  // Make the bug model swarm

  observerSwarm =  [ObserverSwarm create: globalZone];

  // Now send messages to the newly created swarm telling it
  // to build its internal objects and its schedule.
  // Then activate the swarm.

  [observerSwarm buildObjects];
  [observerSwarm buildActions];
  [observerSwarm activateIn: nil];	// Top-level swarm is activated in nil
  
  // Now the swarm is built, activated, and ready to go...

  // We tell the swarm itself to go, instead of an activity
  // because the observerSwarm is a GUI-swarm, and has its
  // own controlPanel that we can talk to.

  [observerSwarm go];

  // the swarm has finished (terminated), so it's time to quit

  return 0;

}


