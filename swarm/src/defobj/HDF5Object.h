// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#include <swarmconfig.h>

#ifdef HAVE_HDF5
#define id hdf5id
#include <hdf5.h>
#undef id
#endif

#import <defobj/Create.h>
#import <collections.h> // Map

#define ATTRIB_TYPE_NAME "type"

@interface HDF5CompoundType_c: CreateDrop_s
{
  const char *name;
  Class class;
@public
#ifdef HAVE_HDF5
  hid_t tid;
#endif
}
- setClass: class;
- setName: (const char *)name;
- createEnd;
#ifdef HAVE_HDF5
- (hid_t)getTid;
#endif
- getClass;
- (void)drop;
@end

@interface HDF5_c: CreateDrop_s
{
  id parent;
  const char *name;
  BOOL createFlag;
  BOOL datasetFlag;
#ifdef HAVE_HDF5
  hid_t loc_id;
#endif

  id c_type;

  unsigned c_count;
#ifdef HAVE_HDF5
  hid_t c_sid;
  hid_t c_msid;
  hsize_t c_rnlen;
#endif
  const char **c_rnbuf;
}
+ createBegin: aZone;
- setParent: parent;
- setName: (const char *)name;
- setCreateFlag: (BOOL)createFlag;
- setDatasetFlag: (BOOL)datasetFlag;
#ifdef HAVE_HDF5
- setId: (hid_t)locId;
#endif

- setCompoundType: compoundType count: (unsigned)count;
- setRowNameLength: (size_t)len;
- createEnd;

- (BOOL)getDatasetFlag;
- getCompoundType;
- (const char *)getName;

- nameRecord: (unsigned)recordNumber name: (const char *)recordName;
- numberRecord: (unsigned)recordNumber;
- selectRecord: (unsigned)recordNumber;
- storeTypeName: (const char *)typeName;
- storeAttribute: (const char *)attributeName value: (const char *)valueString;
- storeObject: obj;
- storeAsDataset: (const char *)name typeName: (const char *)typeName type: (const char *)type ptr: (void *)ptr;

- iterate: (void (*) (id hdf5Obj))iterateFunc;
- iterateAttributes: (void (*) (const char *key, const char *value))iterateFunc;

- writeRowNames;

- (void)drop;
@end

