// Heatbugs application. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import "HeatbugObserverSwarm.h"
#import "HeatbugBatchSwarm.h"

// The main() function is the top-level place where everything starts.
// For a typical Swarm simulation, in main() you create a toplevel
// Swarm, let it build and activate, and set it to running.

int
main(int argc, char ** argv) {
  HeatbugObserverSwarm * observerSwarm;
  HeatbugBatchSwarm * batchSwarm;

  // Swarm initialization: all Swarm apps must call this first.
  initSwarm(argc, argv);

  // swarmGUIMode is set in initSwarm(). It's set to be 1 if your
  // DISPLAY environment variable is set (ie, you have an X server to
  // do graphics with). Otherwise, it's set to 0.
  
  if (swarmGUIMode == 1) {
    // We've got graphics, so make a full ObserverSwarm to get GUI objects
    observerSwarm = [HeatbugObserverSwarm create: globalZone];
    [observerSwarm buildObjects];
    [observerSwarm buildActions];
    [observerSwarm activateIn: nil];
    [observerSwarm go];
  } else {
    // No graphics - make a batchmode swarm and run it.
    batchSwarm = [HeatbugBatchSwarm create: globalZone];
    [batchSwarm buildObjects];
    [batchSwarm buildActions];
    [batchSwarm activateIn: nil];
    [batchSwarm go];
  }

  // The toplevel swarm has finished processing, so it's time to quit.
  return 0;
}
