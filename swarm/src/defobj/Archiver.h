// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj/Create.h>
#import <defobj.h>
#import <collections.h>

#include <swarmconfig.h>

extern id hdf5Archiver;
extern id lispArchiver;

@interface Archiver_c: CreateDrop_s
{
  id currentApplicationKey;
  id <Map> applicationMap;
  BOOL hdf5Flag;
  BOOL inhibitLoadFlag;
  BOOL systemArchiverFlag;
  const char *path;
@public
  id <List> classes;
  id <List> instances;
}

+ createBegin: aZone;
- setInhibitLoadFlag: (BOOL)inhibitLoadFlag;
- setPath: (const char *)path;
- setHDF5Flag: (BOOL)hdf5Flag;
- setSystemArchiverFlag: (BOOL)systemArchiverFlag;
- setDefaultLispPath;
- setDefaultHDF5Path;
- setDefaultAppLispPath;
- setDefaultAppHDF5Path;

- createAppKey: (const char *)appName mode: (const char *)modeName;
- ensureApp: appKey;

- hdf5LoadObjectMap: hdfObj key: appKey;
- hdf5LoadArchiver: hdf5File;
- lispLoadArchiver: expr;

- getApplication;

- registerClient: client;
- unregisterClient: client;

- save;

- getObject: (const char *)key;
- putDeep: (const char *)key object: object;
- putShallow: (const char *)key object: object;

@end
