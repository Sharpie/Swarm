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

- registerClient: client;
- unregisterClient: client;

- lispGet: (const char *)key;
- lispPutDeep: (const char *)key object: object;
- lispPutShallow: (const char *)key object: object;

- hdf5Get: (const char *)key;
- hdf5PutDeep: (const char *)key object: object;
- hdf5PutShallow: (const char *)key object: object;
@end

