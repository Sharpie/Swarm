// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/SwarmObject.h>

extern id archiver;

@interface Archiver: SwarmObject
{
  id currentApplicationKey;
  id applicationMap;
  const char *path;
  id clients;
}

+ createBegin: aZone;

// If default archive exists, loads and returns archive, otherwise return nil.
+ load: aZone;

// If path exists, loads and returns archive, otherwise return nil.
+ load: aZone fromPath: (const char *)path;

// If path exists, loads and returns archive, otherwise
// return empty archive (archiveFileName is set to argument).
+ ensure: aZone path: (const char *)path;

// If default archive exists, loads and returns archive, otherwise
// return empty archive (archiveFilename is set to default).
+ ensure: aZone;

- setPath: (const char *)path;
- save;
- getMap;

void archiverRegister (id client);
void archiverUnregister (id client);
void archiverPut (const char *key, id object);
id archiverGet (const char *key);
void archiverSave (void);

- in: expr;
+ in: aZone expr: expr;
- out: outputCharStream;

@end

