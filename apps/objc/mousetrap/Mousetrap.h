// Mousetraps application. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <activity.h>
#import <swarmobject.h>

@interface Mousetrap : SwarmObject {
  int xCoord;
  int yCoord;
  int triggered;
  id displayWidget;
  id modelSwarm;
}

+create: aZone setModelSwarm: modelSwarm setXCoord: (int)x setYCoord: (int)y;
-trigger;
-setDisplayWidget: (id) widget;
-reload;
@end
