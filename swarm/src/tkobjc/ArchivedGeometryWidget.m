// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import "internal.h"
#import <tkobjc/ArchivedGeometryWidget.h>
#import <tkobjc/WindowGeometryRecord.h>
#import <tkobjc/global.h>
#import <defobj.h> // Archiver methods

@implementation ArchivedGeometryWidget

PHASE(Creating)

+ createBegin: aZone
{
  ArchivedGeometryWidget *obj = [super createBegin: aZone];

  obj->windowGeometryRecordName = NULL;
  obj->destroyedFlag = NO;
  return obj;
}

- setWindowGeometryRecordName: (const char *)name
{
  windowGeometryRecordName = name;
  return self;
}

- setSaveSizeFlag: (BOOL)theSaveSizeFlag
{
  saveSizeFlag = theSaveSizeFlag;

  return self;
}

- loadWindowGeometryRecord
{
  id windowGeometryRecord = nil;

  if (windowGeometryRecordName)
    windowGeometryRecord = [lispArchiver getObject: windowGeometryRecordName];
  return windowGeometryRecord;
}

- registerAndLoad
{
  id windowGeometryRecord;

  [lispArchiver registerClient: self];
  windowGeometryRecord = [self loadWindowGeometryRecord];
  tkobjc_setName (self, windowGeometryRecordName);
  if (windowGeometryRecord)
    {
      if ([windowGeometryRecord getSizeFlag])
        [self setWidth: [windowGeometryRecord getWidth]
              Height: [windowGeometryRecord getHeight]];
      if ([windowGeometryRecord getPositionFlag])
        [self setX: [windowGeometryRecord getX]
              Y: [windowGeometryRecord getY]];
    }

  return self;
}

- createEnd
{
  [super createEnd];
  [self registerAndLoad];

  return self;
}

PHASE(Using)

- updateArchiver
{
  if (windowGeometryRecordName)
    {
      id windowGeometryRecord =
        [lispArchiver getObject: windowGeometryRecordName];
      
      if (windowGeometryRecord == nil)
        windowGeometryRecord = [WindowGeometryRecord create: [self getZone]];

      if (saveSizeFlag)
        [windowGeometryRecord setWidth: [self getWidth]
                              Height: [self getHeight]];
      [windowGeometryRecord setX: [self getX] Y: [self getY]];
      [lispArchiver putShallow: windowGeometryRecordName
                    object: windowGeometryRecord];
    }
  return self;
}

- (void)drop
{ 
  [lispArchiver unregisterClient: self];

  [super drop];
}
@end

