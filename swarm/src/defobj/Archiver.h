// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj/Create.h>
#import <defobj.h>
#import <collections.h>

#include <swarmconfig.h>

extern id archiver;

@interface Archiver_c: CreateDrop_s
{
  id currentApplicationKey;
  id <Map> applicationMap;
  BOOL inhibitLoadFlag;
  const char *lispPath;
  const char *hdf5Path;
@public
  id <List> classes;
  id <List> instances;
}

+ createBegin: aZone;
- setInhibitLoadFlag: (BOOL)inhibitLoadFlag;
- getApplication;
- setLispPath: (const char *)lispPath;
- setHDF5Path: (const char *)HDF5Path;
- save;

- lispOut: outputCharStream;

void archiverRegister (id client);
void archiverUnregister (id client);
void archiverSave (void);

void lispArchiverPut (const char *key, id object, BOOL deepFlag);
id lispArchiverGet (const char *key);

void hdf5ArchiverPut (const char *key, id object, BOOL deepFlag);
id hdf5ArchiverGet (const char *key);

@end

