// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/ArchivedGeometryWidget.h>
#import <tkobjc/WindowGeometryRecord.h>

#import "global.h"

void archiverUnregister (id client);
void archiverRegister (id client);
void archiverPut (const char *key, id object);
id archiverGet (const char *key);

@implementation ArchivedGeometryWidget

- setWindowGeometryRecordName : (const char *)name
{
  windowGeometryRecordName = name;
  return self;
}

- loadWindowGeometryRecord
{
  id windowGeometryRecord = nil;

  if (windowGeometryRecordName)
    windowGeometryRecord = archiverGet (windowGeometryRecordName);
  return windowGeometryRecord;
}

+ createBegin: aZone
{
  ArchivedGeometryWidget *obj = [super createBegin: aZone];

  obj->windowGeometryRecordName = NULL;
  obj->destroyedFlag = NO;
  return obj;
}

- registerAndLoad
{
  id windowGeometryRecord;

  archiverRegister (self);
  windowGeometryRecord = [self loadWindowGeometryRecord];
  if (windowGeometryRecord)
    [self setWindowGeometry: [windowGeometryRecord getWindowGeometry]];
  return self;
}

- createEnd
{
  [super createEnd];
  [self registerAndLoad];
  return self;
}

- updateArchiver
{
  if (windowGeometryRecordName)
    {
      id windowGeometryRecord = archiverGet (windowGeometryRecordName);
      
      if (windowGeometryRecord == nil)
        windowGeometryRecord = [WindowGeometryRecord create: [self getZone]];
      
      [windowGeometryRecord setWindowGeometry: [self getWindowGeometry]];
      archiverPut (windowGeometryRecordName, windowGeometryRecord);
    }
  return self;
}

- (void)drop
{ 
  archiverUnregister (self);

  if (!destroyedFlag)
    [globalTkInterp eval: "destroy %s", [parent getWidgetName]]; 
  [super drop];
}

- (BOOL)getDestroyedFlag
{
  return destroyedFlag;
}

- _notifyTarget_
{
  destroyedFlag = YES;
  [destroyNotificationTarget perform: destroyNotificationMethod with: self];
  return self;
}

static void
structure_proc (ClientData clientdata, XEvent *eventptr)
{
  if (eventptr->type == DestroyNotify)
    [(id)clientdata _notifyTarget_];
}

- enableDestroyNotification: theNotificationTarget
         notificationMethod: (SEL)theNotificationMethod
{
  if (theNotificationTarget)
    {
      Tk_Window tkwin = Tk_NameToWindow ([globalTkInterp interp],
                                         (char *)widgetName,
                                         [globalTkInterp mainWindow]);
      Tk_CreateEventHandler (tkwin, StructureNotifyMask, structure_proc, self);
      
      destroyNotificationTarget = theNotificationTarget;
      destroyNotificationMethod = theNotificationMethod;
    }
  return self;
}

- disableDestroyNotification
{
  if (destroyNotificationTarget != nil)
    {
      Tk_Window tkwin = Tk_NameToWindow ([globalTkInterp interp],
                                         (char *)widgetName,
                                         [globalTkInterp mainWindow]);
      
      Tk_DeleteEventHandler (tkwin, StructureNotifyMask, structure_proc, self);
      destroyNotificationTarget = nil;
    }
  return self;
}

@end
