// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj/Create.h>
#import <defobj.h>
#import <collections.h>

#include <swarmconfig.h>

#ifdef HAVE_HDF5
#define id hdf5id
#include <hdf5.h>
#undef id
#define SWARMARCHIVER_HDF5 "swarmArchiver.hdf"
#endif
#define SWARMARCHIVER_LISP ".swarmArchiver"

#define ARCHIVER_FUNCTION_NAME "archiver"

extern id archiver;

@interface Archiver_c: CreateDrop_s
{
  id currentApplicationKey;
  id <Map> applicationMap;
  const char *lispPath;
#ifdef HAVE_HDF5
  const char *hdf5Path;
#endif
@public
  id <List> classes;
  id <List> instances;
}

+ createBegin: aZone;
- getApplication;
- setLispPath: (const char *)lispPath;
#ifdef HAVE_HDF5
- setHDF5Path: (const char *)HDF5Path;
#endif
- save;

- lispOut: outputCharStream;

void archiverRegister (id client);
void archiverUnregister (id client);
void archiverSave (void);

void lispArchiverPut (const char *key, id object, BOOL deepFlag);
id lispArchiverGet (const char *key);

#ifdef HAVE_HDF5
void hdf5ArchiverPut (const char *key, id object, BOOL deepFlag);
id hdf5ArchiverGet (const char *key);
#endif

@end

#ifdef HAVE_HDF5
@interface HDF5: CreateDrop
{
  hid_t loc_id;
  id parent;
  const char *name;
}
- setParent: parent;
- setName: (const char *)name;
- createEnd;
- store: (const char *)name type: (const char *)type ptr: (void *)ptr;
@end
#endif
