// simpleBatchBug2 - tutorial application


#import <simtools.h>                // ... for initSwarm() 
#import "ObserverSwarm.h"
#import "BatchSwarm.h"
#import "Parameters.h"

// The main() function is the top-level place where everything starts.
// For a typical Swarm simulation, in main() you create a toplevel
// Swarm, let it build and activate, and set it to running.

int
main (int argc, const char **argv)
{
  id theTopLevelSwarm;
  id topLevelActivity;
  initSwarmArguments (argc, argv, [Parameters class]);

  // Make the bug observer swarm if swarmGUIMode equals YES

  if (swarmGUIMode == YES)
    {
      theTopLevelSwarm =  [ObserverSwarm create: globalZone];
      
      //now run the init method of the arguments object (Parameters class)
      [(id) arguments init];
      CREATE_ARCHIVED_PROBE_DISPLAY (arguments);
      
    }
  else 
    {
       theTopLevelSwarm =  [BatchSwarm create: globalZone];
    }

  
  [theTopLevelSwarm buildObjects];
  [theTopLevelSwarm buildActions];
  topLevelActivity = [theTopLevelSwarm activateIn: nil];	
  
  // Now the swarm is built, activated, and ready to go. The go 
  // method differs between the GUI and non-GUI modes.

  [theTopLevelSwarm go];
  
  [topLevelActivity drop];
  [theTopLevelSwarm drop];
  // the swarm has finished (terminated), so it's time to quit

  return 0;

}


