// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/SwarmObject.h>
#import <objectbase/MessageProbe.h>
#import <gui.h>

#ifdef USE_FRAME
#import <tkobjc/Frame.h>
@interface MessageProbeWidget: Frame
#else
@interface MessageProbeWidget: SwarmObject
#endif
{
  id myObject;
  int argCount;
  MessageProbe *myProbe;
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
- update;
- dynamic;
- Spawn;
- argSpawn: (int)which;
- (const char *)getId;
- (const char *)getId: (int)which;
- (const char *)package;
- (const char *)package: (int)which;
- idReceive: (int)which;

#ifndef USE_FRAME
- setParent: parent;
- pack;
- (const char *)getWidgetName;
#endif
@end
