// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

#include "internal.h"
#import <tkobjc/ArchivedGeometryWidget.h>
#import <tkobjc/WindowGeometryRecord.h>
#import <tkobjc/global.h>
#import <defobj/defalloc.h> // getZone
#import <defobj.h> // Archiver methods

@implementation ArchivedGeometryWidget

PHASE(Creating)

+ createBegin: (id <Zone>)aZone
{
  ArchivedGeometryWidget *obj = [super createBegin: aZone];

  obj->windowGeometryRecordName = NULL;
  obj->destroyedFlag = NO;
  return obj;
}

+ create: (id <Zone>)aZone setWindowGeometryRecordName: (const char *)name
{
  return [[[self createBegin: aZone]
            setWindowGeometryRecordName: name]
           createEnd];
}

- setWindowGeometryRecordName: (const char *)name
{
  windowGeometryRecordName = name ? STRDUP (name) : NULL;
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
      id topLevel = [self getTopLevel];

      if ([windowGeometryRecord getPositionFlag])
        [topLevel setX: [windowGeometryRecord getX]
                  Y: [windowGeometryRecord getY]];
    }

  return self;
}

- updateSize
{
  id windowGeometryRecord = [self loadWindowGeometryRecord];

  if (windowGeometryRecord)
    {
      if ([windowGeometryRecord getSizeFlag])
        [self setWidth: [windowGeometryRecord getWidth]
              Height: [windowGeometryRecord getHeight]];
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

- (void)updateArchiver: archiver
{
  if (windowGeometryRecordName)
    {
      id windowGeometryRecord =
        [archiver getObject: windowGeometryRecordName];
      
      if (windowGeometryRecord == nil)
        windowGeometryRecord = [WindowGeometryRecord create: getZone (self)];

      if (saveSizeFlag)
        [windowGeometryRecord setWidth: [self getWidth]
                              Height: [self getHeight]];
      [windowGeometryRecord setX: [self getX] Y: [self getY]];
      [archiver putShallow: windowGeometryRecordName
                    object: windowGeometryRecord];
    }
}

- (void)drop
{ 
  if (windowGeometryRecordName)
    FREEBLOCK (windowGeometryRecordName);

  [lispArchiver unregisterClient: self];

  [super drop];
}
@end

