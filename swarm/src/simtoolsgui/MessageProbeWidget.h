// Swarm library. Copyright � 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtoolsgui.h> // MessageProbeWidget
#import <objectbase/SwarmObject.h>

#import <objectbase.h> // MessageProbe
#import <gui.h>

#ifdef USE_FRAME
#import <tkobjc/Frame.h>
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
- update;
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
- pack;
- (const char *)getWidgetName;
#endif
@end
