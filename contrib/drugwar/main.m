// DrugWar model. Copyright © 2000 Swarm Development Group
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.
#import "GUI.h"
#import "Society.h"

#import "graph/graph.h"
#import <simtools.h>

int
main (int argc, const char **argv)
{
  id theTopLevelSwarm;

  initSwarm (argc, argv);
  initGraphLibrary ();

  if (swarmGUIMode == 1)
    {
      theTopLevelSwarm = [GUI createBegin: globalZone];
      SET_WINDOW_GEOMETRY_RECORD_NAME (theTopLevelSwarm);
      theTopLevelSwarm = [theTopLevelSwarm createEnd];
    }
  else
    theTopLevelSwarm = [Society create: globalZone];
  [theTopLevelSwarm build];
  [theTopLevelSwarm activateIn: nil];
  [theTopLevelSwarm go];

  // theTopLevelSwarm has finished processing, so it's time to quit.
  return 0;
}
