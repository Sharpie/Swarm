// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/SwarmObject.h>
#import <tkobjc/Widget.h>

@interface ArchivedGeometryWidget : Widget
{
  const char *windowGeometryRecordName;
  id notificationTarget;
  SEL destroyNotificationMethod;
}

- setupDestroyNotification: notificationTarget
        notificationMethod: (SEL)destroyNotificationMethod;
- setWindowGeometryRecordName : (const char *)recordName;
- loadWindowGeometryRecord;
- updateArchiver;
- createEnd;
- registerAndLoad;
- (void)drop;

@end
