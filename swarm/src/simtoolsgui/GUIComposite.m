// Swarm library. Copyright � 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtoolsgui/GUIComposite.h>
#import <simtoolsgui.h> // buildWindowGeometryRecordName
#import <collections.h>

static int
compareFunc (id obj1, id obj2)
{
  return strcmp ((const char *)obj1, (const char *)obj2);
}

@implementation GUIComposite

PHASE(Creating)

+ createBegin: aZone
{
  GUIComposite *obj = [super createBegin: aZone];

  obj->componentList = [Map createBegin: aZone];
  [obj->componentList setCompareFunction: compareFunc];
  obj->componentList = [obj->componentList createEnd];

  return obj;
}

- setWindowGeometryRecordName: (const char *)windowGeometryRecordName
{
  baseWindowGeometryRecordName = STRDUP (windowGeometryRecordName);

  return self;
}

- setWindowGeometryRecordNameForComponent: (const char *)componentName
                                   widget: widget
{
  if ([componentList at: (id)componentName])
    [componentList at: (id)componentName replace: widget];
  else
    [componentList at: (id)componentName insert: widget];

  [widget setWindowGeometryRecordName: 
            buildWindowGeometryRecordName (baseWindowGeometryRecordName, 
                                           componentName)];

  return self;
}

PHASE(Using)

- enableDestroyNotification: theNotificationTarget
         notificationMethod: (SEL)theNotificationMethod
{
  [componentList forEach: 
                   @selector (enableDestroyNotification:notificationMethod:)
                 : theNotificationTarget
                 : (id)theNotificationMethod];

  return self;
}

- disableDestroyNotification
{
  [componentList forEach: @selector (disableDestroyNotification)];

  return self;
}

- (void)drop
{
  FREEBLOCK (baseWindowGeometryRecordName);
  [super drop];
}
@end
