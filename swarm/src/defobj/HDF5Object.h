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
#define ATTRIB_COMPONENT_TYPE_NAME "component-type"


@interface HDF5CompoundType_c: CreateDrop_s
{
  Class class;
@public
  const char *name;
#ifdef HAVE_HDF5
  hid_t tid;
  hid_t did;
#endif
  id <Map> stringMap;
}
- setClass: class;
- setName: (const char *)name;
- createEnd;
#ifdef HAVE_HDF5
- (hid_t)getTid;
- setDataset: (hid_t)did;
#endif
- pack: (void *)buf to: obj;
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
  hid_t psid;
#endif

  id baseTypeObject;
  id compoundType;

  unsigned c_count;
#ifdef HAVE_HDF5
  hid_t c_sid;
  hsize_t c_rnnlen;
  hsize_t c_rnmlen;
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

- setCompoundType: compoundType;
- setCount: (unsigned)count;
- createEnd;

- setBaseTypeObject: typeObject;

- (BOOL)checkName: (const char *)name;

- (BOOL)getDatasetFlag;
- getCompoundType;
- (unsigned)getCount;
- (const char *)getName;
- getClass;

- nameRecord: (unsigned)recordNumber name: (const char *)recordName;
- numberRecord: (unsigned)recordNumber;
- selectRecord: (unsigned)recordNumber;

- storeTypeName: (const char *)typeName;
- storeComponentTypeName: (const char *)typeName;

- shallowLoadObject: obj;
- shallowStoreObject: obj;

- loadDataset: (void *)ptr;
- storeAsDataset: (const char *)name typeName: (const char *)typeName type: (const char *)type ptr: (void *)ptr;

- iterate: (int (*) (id hdf5Obj))iterateFunc;

- storeAttribute: (const char *)attributeName value: (const char *)valueString;
- (const char *)getAttribute: (const char *)attrName;
- iterateAttributes: (int (*) (const char *key, const char *value))iterateFunc;

- (const char **)readRowNames;
- writeRowNames;

- (void)drop;
@end

