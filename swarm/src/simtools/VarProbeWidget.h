// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/SwarmObject.h>
#import <objectbase/VarProbe.h>
#import <gui.h>

#ifdef USE_WIDGET
#include <tkobjc/Widget.h>
@interface VarProbeWidget: Widget
#else
@interface VarProbeWidget: SwarmObject
#endif
{
  id myObject;
  VarProbe *myProbe;
  id <Frame> myLeft;
  id <Frame> myRight;
  id <Label> myLabel;
  int maxLabelWidth;
  int interactive;
  id <VarProbeEntry> myEntry;
#ifndef USE_WIDGET
  id parent;
#endif
}

+ createBegin: aZone;
- setObject: obj;
- setProbe: (Probe *)the_probe;
- setMyLeft: aFrame;
- setMyRight: aFrame;
- setMaxLabelWidth: (int)width;
- createEnd;
- pack;
- setValue;
- update;
- Spawn;
- idReceive;
- (const char *)package;
- (const char *)getId;
#ifndef USE_WIDGET
- focus;
- setParent: parent;
#endif
@end
