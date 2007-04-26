// DrugWar model.  Copyright © 2000 Swarm Development Group
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtoolsgui/GUISwarm.h>

@interface GUI: GUISwarm
{
  int displayFrequency;
  id displayActions;
  id displaySchedule;
  id canvas;
  unsigned canvasHeight, canvasWidth;
  unsigned nodeSize;

  id guiZone;

  id society;
}

+ createBegin: aZone;
- createEnd;
- build;
- buildProbes;
- buildActivity;
- activateIn: swarmContext;
@end
