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

  [super drop];
}

@end
