#import "ObserverSwarm.h"

int
main(int argc,const char ** argv) {
  ObserverSwarm * observerSwarm;

  initSwarm(argc, argv);

  observerSwarm = [ObserverSwarm createBegin: globalZone];
  SET_WINDOW_GEOMETRY_RECORD_NAME (observerSwarm);
  observerSwarm = [observerSwarm createEnd];

  [observerSwarm buildObjects];
  [observerSwarm buildActions];
  [observerSwarm activateIn: nil];
  [observerSwarm go];

  return 0;
}
