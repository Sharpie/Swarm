// simpleBug - tutorial application


#import <simtools.h>                // ... for initSwarm() 
#import "ObserverSwarm.h"
#import "Parameters.h"

// The main() function is the top-level place where everything starts.
// For a typical Swarm simulation, in main() you create a toplevel
// Swarm, let it build and activate, and set it to running.

int
main (int argc, const char **argv)
{
  ObserverSwarm *observerSwarm;

  // Swarm initialization: all Swarm apps must call this first.
  // Now we are using the Arguments form of this function, which automatically
  // creates an object called "arguments" using the class we specify, Parameters
  initSwarmArguments (argc, argv, [Parameters class]);

  // Make the bug observer swarm

  observerSwarm =  [ObserverSwarm create: globalZone];

  //now run the init method of the arguments object (Parameters class)
  [(id) arguments init];
  CREATE_ARCHIVED_PROBE_DISPLAY (arguments);

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


