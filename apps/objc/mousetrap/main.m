// Mousetraps application. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import "MousetrapObserverSwarm.h"

int
main(int argc, char ** argv) {
  MousetrapObserverSwarm * observerSwarm;

  initSwarm(argc, argv);

  observerSwarm = [MousetrapObserverSwarm create: globalZone];
  [observerSwarm buildObjects];
  [observerSwarm buildActions];
  [observerSwarm activateIn: nil];
  [observerSwarm go];

  return 0;
}
