#import <simtools.h> // initSwarm
#import <simtoolsgui.h> // SET_WINDOW_GEOMETRY_RECORD_NAME
#import "ForestObserverSwarm.h"
#import "ForestBatchSwarm.h"

int
main (int argc, const char **argv)
{
  ForestObserverSwarm *observerSwarm;
  ForestBatchSwarm *batchSwarm;

  initSwarm (argc, argv);

  if (swarmGUIMode == 1)
    {
      observerSwarm = [ForestObserverSwarm createBegin: globalZone];
      SET_WINDOW_GEOMETRY_RECORD_NAME (observerSwarm);
      observerSwarm = [observerSwarm createEnd];
      [observerSwarm buildObjects];
      [observerSwarm buildActions];
      [observerSwarm activateIn: nil];
      [observerSwarm go];
    }
  else
    {
      // initSwarm creates a globally accessible object "lispAppArchiver"
      // which defaults to use a file named "arborgames.scm".  If we wanted
      // to store parameters in a different file, we could easily 
      // customize, as is done with species.scm in ForestModelSwarm.m
      if ((batchSwarm = [lispAppArchiver getWithZone: globalZone
                                             key: "batchSwarm"]) == nil)
	raiseEvent(InvalidOperation,
		   "Can't find the parameters to create batchSwarm");
      
      [batchSwarm buildObjects];
      [batchSwarm buildActions];
      [batchSwarm activateIn: nil];
      [batchSwarm go];
    }
  
  exit (0);
}
