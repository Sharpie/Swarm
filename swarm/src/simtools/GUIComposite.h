// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/SwarmObject.h>

@interface GUIComposite: SwarmObject
{
  const char *baseWindowGeometryRecordName;
  id notificationTarget;
  SEL notificationMethod;
}

- setWindowGeometryRecordName: (const char *)windowGeometryRecordName;
- (const char *)windowGeometryRecordNameForComponent: (const char *)componentName;
- (const char *)windowGeometryRecordName;
- enableDestroyNotification: notificationTarget
         notificationMethod: (SEL)notificationMethod;
@end
