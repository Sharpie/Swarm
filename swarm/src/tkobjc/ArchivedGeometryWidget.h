// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/Widget.h>

@interface ArchivedGeometryWidget: Widget
{
  const char *windowGeometryRecordName;
  id destroyNotificationTarget;
  SEL destroyNotificationMethod;
  BOOL destroyedFlag;
}

- enableDestroyNotification: notificationTarget
         notificationMethod: (SEL)destroyNotificationMethod;
- disableDestroyNotification;
- setWindowGeometryRecordName: (const char *)recordName;
- loadWindowGeometryRecord;
- updateArchiver;
- createEnd;
- registerAndLoad;
- (BOOL)getDestroyedFlag;
- (void)drop;

@end
