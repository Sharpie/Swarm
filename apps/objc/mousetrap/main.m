// Mousetrap application. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import "MousetrapObserverSwarm.h"
#import "MousetrapBatchSwarm.h"
#import <simtoolsgui.h>

// The main() function is the top-level place where everything starts.
// For a typical Swarm simulation, in main() you create a toplevel
// Swarm, let it build its objects and activities, and set it running.

int
main (int argc, const char **argv)
{
  id theTopLevelSwarm;

  // Swarm initialization: all Swarm apps must call this first.

  initSwarmApp (argc, argv, "1.4.1", "bug-swarm@santafe.edu");

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
    {
      // create an instance of the Archiver to retrieve the file
      // set the path to `batch.scm'
      id archiver = [Archiver create: globalZone fromLispPath: "batch.scm"];

      // retrieve the object from the archiver, if it can't be found
      // just raise an event; note that the call to the
      // archiver will actually *instantiate* the object if the
      // parameters are found in the Lisp file
      if ((theTopLevelSwarm = 
           [archiver getWithZone: globalZone object: "batchSwarm"]) == nil)
        raiseEvent(InvalidOperation, 
                   "Can't find archiver file or appropriate key");
      
      [archiver drop];
    }

  // either way, build objects, actions, activate, and run

  [theTopLevelSwarm buildObjects];
  [theTopLevelSwarm buildActions];
  [theTopLevelSwarm activateIn: nil];
  [theTopLevelSwarm go];

  // theTopLevelSwarm has finished processing, so it's time to quit.

  return 0;
}
