// simpleBug - tutorial application

#import <simtools.h>                // ... for initSwarmBatch () 
#import <activity.h>
#import "ModelSwarm.h"

// The main() function is the top-level place where everything starts.
// For a typical Swarm simulation, in main() you create a toplevel
// Swarm, let it build and activate, and set it to running.

int
main (int argc, const char **argv)
{
  ModelSwarm *modelSwarm;

  // Swarm initialization: all Swarm apps must call this first.

  initSwarmBatch (argc, argv);

  // Make the bug model swarm, creating the object from the default
  // `lispAppArchiver' instance which looks for the `bug.scm'
  // datafile, so we don't have to recompile everytime we want to
  // change something
  
  if ((modelSwarm = 
       [lispAppArchiver getWithZone: globalZone object: "modelSwarm"]) == nil)
    raiseEvent(InvalidOperation,
               "Can't find the modelSwarm parameters");

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
