// Schellings Segregation Model a la Axtell and Epstein
// Code by Benedikt Stefansson, <benedikt@ucla.edu>. 
// First version July 1997

#import "ObserverSwarm.h"
#import "Parameters.h"
#import "BatchSwarm.h"

int
main(int argc,const char ** argv) {
  ObserverSwarm * observerSwarm;
  id topLevelActivity;

  initSwarmArguments(argc, argv,[Parameters class]);

  [(Parameters*)arguments init];
  CREATE_ARCHIVED_PROBE_DISPLAY (arguments);
 
  if (swarmGUIMode == 1)
    {
      observerSwarm = [ObserverSwarm createBegin: globalZone];
      SET_WINDOW_GEOMETRY_RECORD_NAME (observerSwarm);
      observerSwarm = [observerSwarm createEnd];
    }
  else
    observerSwarm = [BatchSwarm create: globalZone];
  
  
  [observerSwarm buildObjects];
  [observerSwarm buildActions];
  topLevelActivity = [observerSwarm activateIn: nil];
  [observerSwarm go];
  [topLevelActivity drop];
  [observerSwarm drop];
  

  return 0;
}
