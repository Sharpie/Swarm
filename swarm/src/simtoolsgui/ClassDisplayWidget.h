// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

#import <Swarm/SwarmObject.h>
#import <Swarm/ProbeMap.h>
#import <Swarm/VarProbeWidget.h>
#import <Swarm/MessageProbeWidget.h>
#import <Swarm/gui.h>

#ifdef USE_FRAME
#import <Swarm/Frame.h>
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
