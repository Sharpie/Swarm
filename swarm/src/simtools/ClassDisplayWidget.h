// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/SwarmObject.h>
#import <objectbase/ProbeMap.h>
#import <simtools/VarProbeWidget.h>
#import <simtools/MessageProbeWidget.h>

#import <tkobjc/Frame.h>
#import <gui.h>

@interface ClassDisplayWidget: Frame
{
  id probedObject;
  Class theClass;
  id <Label> myTitle;
  ProbeMap *probeMap;
  id <Frame> leftFrame, rightFrame, middleFrame, bottomFrame;
  id hideB;
  id superB;
  id topRow;
  int numberOfProbes;
  int maxLabelWidth;
  id *widgets;
  id mySuperclass;
  id mySubclass;
  id owner;
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
- getProbeMap;
- armSuperButton;
- update;

- (const char *)getId;
- (const char *)package;

@end
