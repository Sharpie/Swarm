// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtoolsgui.h> // GUIComposite
#import <objectbase/SwarmObject.h>
#import <collections.h>

@interface GUIComposite: SwarmObject <GUIComposite>
{
  const char *baseWindowGeometryRecordName;
  id <Map> componentList;
  BOOL saveSizeFlag;
}

- setWindowGeometryRecordName: (const char *)windowGeometryRecordName;
- setWindowGeometryRecordNameForComponent: (const char *)componentName
                                   widget: widget;
- setSaveSizeFlag: (BOOL)saveSizeFlag;
- enableDestroyNotification: notificationTarget
         notificationMethod: (SEL)notificationMethod;
- disableDestroyNotification;
- (void)drop;
@end
