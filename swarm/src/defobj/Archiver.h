// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj/Create.h>
#import <defobj.h>
#import <collections.h>

extern id archiver;

@interface Archiver: CreateDrop_s
{
  id currentApplicationKey;
  id <Map> applicationMap;
  const char *lispPath;
@public
  id clients;
}

+ createBegin: aZone;
- getApplication;
- setLispPath: (const char *)lispPath;
- save;

- lispOut: outputCharStream;

void archiverRegister (id client);
void archiverUnregister (id client);
void archiverSave (void);

void lispArchiverPut (const char *key, id object);
id lispArchiverGet (const char *key);
void HDF5ArchiverPut (const char *key, id object);
id HDF5ArchiverGet (const char *key);

@end

