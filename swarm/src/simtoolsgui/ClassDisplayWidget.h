// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/SwarmObject.h>
#import <objectbase/ProbeMap.h>
#import <simtoolsgui/VarProbeWidget.h>
#import <simtoolsgui/MessageProbeWidget.h>
#import <gui.h>

#ifdef USE_FRAME
#import <tkobjc/Frame.h>
@interface ClassDisplayWidget: Frame
#else
@interface ClassDisplayWidget: SwarmObject
#endif
{
  id probedObject;
  Class theClass;
  id <ClassDisplayLabel> myTitle;
  ProbeMap *probeMap;
  id <Frame> leftFrame, rightFrame, middleFrame, bottomFrame;
  id <ClassDisplayHideButton> hideB;
  id <SuperButton> superB;
  id topRow;
  unsigned count;
  int maxLabelWidth;
  id *widgets;
  id mySuperclass;
  id mySubclass;
  id owner;
#ifndef USE_FRAME
  const char *widgetName;
  id parent;
#endif
}

+ createBegin: aZone;
- setProbedObject: anObject;
- setClassToDisplay: (Class)aClass;
- setMaxLabelWidth: (int)width;
- setOwner: anOwner;
- setMySuperclass: aWidget;
- setMySubclass: aWidget;
- createEnd;

- getProbedObject;
- (id <ProbeMap>)getProbeMap;
- (void)armSuperButton;
- (void)update;

#ifndef USE_FRAME
- setParent: (id <Frame>)parent;
- (const char *)getWidgetName;
- (void)pack;
#endif

- (const char *)getId: (const char *)windowName;
- (const char *)package: (const char *)windowName;

@end
