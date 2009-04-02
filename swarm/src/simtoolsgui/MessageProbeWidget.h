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

#import <Swarm/simtoolsgui.h> // MessageProbeWidget
#import <Swarm/SwarmObject.h>

#import <Swarm/objectbase.h> // MessageProbe
#import <Swarm/gui.h>

#ifdef USE_FRAME
#import <Swarm/Frame.h>
@interface MessageProbeWidget: Frame <MessageProbeWidget>
#else
@interface MessageProbeWidget: SwarmObject <MessageProbeWidget>
#endif
{
  id myObject;
  int argCount;
  id <MessageProbe> myProbe;
  id <Widget> *myWidgets;
  id <MessageProbeEntry> resultMessageProbeEntry;
  int maxReturnWidth;
  char resultType;
  id resultObject;
  BOOL *objWindows;
  char **obj_args;
#ifndef USE_FRAME
  id parent;
  const char *widgetName;
#endif
}

+ createBegin: aZone;
- setObject: obj;
- setProbe: (id <Probe>)theProbe;
- setMaxReturnWidth: (int)width;
- createEnd;
- (void)update;
- (void)dynamic;
- Spawn: (const char *)widgetName;
- argSpawn: (const char *)widgetName arg:(int)which;
- (const char *)getId: (const char *)windowName;
- (const char *)getId: (const char *)windowName arg: (int)which;
- (const char *)package: (const char *)windowName;
- (const char *)package: (const char *)windowName arg: (int)which;
- idReceive: (const char *)windowName arg: (int)which;

#ifndef USE_FRAME
- setParent: (id <Frame>)parent;
- (void)pack;
- (const char *)getWidgetName;
#endif
@end
