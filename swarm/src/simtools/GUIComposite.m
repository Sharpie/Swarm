// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtools/GUIComposite.h>
#import <collections.h>
#import <simtools.h>

@implementation GUIComposite

static int
compareFunc (id obj1, id obj2)
{
  return strcmp ((const char *)obj1, (const char *)obj2);
}

- createEnd
{
  [super createEnd];
  componentList = [Map createBegin: [self getZone]];
  [componentList setCompareFunction: compareFunc];
  componentList = [componentList createEnd];
  return self;
}

- setWindowGeometryRecordName: (const char *)windowGeometryRecordName
{
  baseWindowGeometryRecordName = windowGeometryRecordName;
  return self;
}

- setWindowGeometryRecordNameForComponent: (const char *)componentName
                                   widget: widget
{
  [componentList at: (id)componentName replace: widget];
  [widget setWindowGeometryRecordName: 
            buildWindowGeometryRecordName (baseWindowGeometryRecordName, 
                                           componentName)];
  return self;
}

- enableDestroyNotification: theNotificationTarget
         notificationMethod: (SEL)theNotificationMethod
{
  [componentList forEach: 
                   @selector(enableDestroyNotification:notificationMethod:)
                 : theNotificationTarget
                 : (id)theNotificationMethod];
  return self;
}
@end
