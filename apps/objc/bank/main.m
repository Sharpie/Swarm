#import "BankObserverSwarm.h"
#import "BankBatchSwarm.h"

int
main (int argc, char ** argv)
{
  BankObserverSwarm *observerSwarm;
  BankBatchSwarm *batchSwarm;
  
  initSwarm (argc, argv);
  initGraphLibrary ();
  
  // swarmGUIMode is set in initSwarm(). It's set to be 1 if your
  // DISPLAY environment variable is set (ie, you have an X server to
  // do graphics with). Otherwise, it's set to 0.
  
  if (swarmGUIMode == 1)
    {
      observerSwarm = [BankObserverSwarm createBegin: globalZone];
      SET_WINDOW_GEOMETRY_RECORD_NAME (observerSwarm);
      observerSwarm = [observerSwarm createEnd];

      [observerSwarm buildObjects];
      [observerSwarm buildActions];
      [observerSwarm activateIn: nil];
      [observerSwarm go];
    }
  else
    {
      batchSwarm = [BankBatchSwarm create: globalZone];
      [batchSwarm buildObjects];
      [batchSwarm buildActions];
      [batchSwarm activateIn: nil];
      [batchSwarm go];
    }

  return 0;
}
