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
  id <VarProbeLabel> myLabel;
  int maxLabelWidth;
  BOOL interactiveFlag;
  id <VarProbeEntry> myEntry;
#ifndef USE_WIDGET
  id parent;
#endif
}

+ createBegin: aZone;
- setObject: obj;
- setProbe: probe;
- setMyLeft: aFrame;
- setMyRight: aFrame;
- setMaxLabelWidth: (int)width;
- createEnd;
- pack;
- setVariableValue: (const char *)windowName;
- update;
- Spawn: (const char *)windowName;
- idReceive: (const char *)windowName;
- (const char *)package: (const char *)widgetName;
- (const char *)getId: (const char *)widgetName;
#ifndef USE_WIDGET
- setParent: parent;
#endif
@end
