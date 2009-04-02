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
#import <Swarm/VarProbe.h>
#import <Swarm/gui.h>

#ifdef USE_WIDGET
#include <Swarm/Widget.h>
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
- (void)pack;
- setVariableValue: (const char *)windowName;
- (void)update;
- Spawn: (const char *)windowName;
- idReceive: (const char *)windowName;
- (const char *)package: (const char *)widgetName;
- (const char *)getId: (const char *)widgetName;
#ifndef USE_WIDGET
- setParent: parent;
#endif
@end
