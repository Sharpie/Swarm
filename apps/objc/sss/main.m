// Sugarscape in Swarm. Copyright © 1997-1998 Nelson Minar
// This program is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import "ObserverSwarm.h"
#import <simtools.h>   // initSwarm macro
#import <simtoolsgui.h>

int
main (int argc, const char **argv)
{
  ObserverSwarm *observerSwarm;

  initSwarm (argc, argv);

  observerSwarm = [ObserverSwarm createBegin: globalZone];
  SET_WINDOW_GEOMETRY_RECORD_NAME (observerSwarm);
  observerSwarm = [observerSwarm createEnd];
  [observerSwarm buildObjects];
  [observerSwarm buildActions];
  [observerSwarm activateIn: nil];
  [observerSwarm go];

  return 0;
}
