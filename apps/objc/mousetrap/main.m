// Mousetrap application. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtools.h>                // ... for initSwarm() and swarmGUIMode
#import "MousetrapObserverSwarm.h"
#import "MousetrapBatchSwarm.h"

// The main() function is the top-level place where everything starts.
// For a typical Swarm simulation, in main() you create a toplevel
// Swarm, let it build its objects and activities, and set it running.

int
main(int argc, char ** argv) {
  id theTopLevelSwarm ;

  // Swarm initialization: all Swarm apps must call this first.

  initSwarm(argc, argv);

  // swarmGUIMode is set in initSwarm(). It's set to be 0 if you
  // typed mousetrap -batchmode. Otherwise, it's set to 1.
  
  if (swarmGUIMode == 1)
    {
      // We've got graphics, so make a full ObserverSwarm to get GUI objects
      
      theTopLevelSwarm = [MousetrapObserverSwarm createBegin: globalZone];
      SET_WINDOW_GEOMETRY_RECORD_NAME (theTopLevelSwarm);
      theTopLevelSwarm = [theTopLevelSwarm createEnd];
    }
  else

    // No graphics - make a batchmode swarm and run it.

    theTopLevelSwarm = [MousetrapBatchSwarm create: globalZone];

  // either way, build objects, actions, activate, and run

  [theTopLevelSwarm buildObjects];
  [theTopLevelSwarm buildActions];
  [theTopLevelSwarm activateIn: nil];
  [theTopLevelSwarm go];

  // theTopLevelSwarm has finished processing, so it's time to quit.

  return 0;
}
