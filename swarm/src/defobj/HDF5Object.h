// Swarm library. Copyright © 1996-2000 Swarm Development Group.
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
#import "internal.h" // fcall_type_t

#define ATTRIB_TYPE_NAME "type"
#define ATTRIB_COMPONENT_TYPE_NAME "component-type"


@interface HDF5CompoundType_c: CreateDrop_s <HDF5CompoundType>
{
  id prototype;
@public
  const char *name;
#ifdef HAVE_HDF5
  hid_t tid;
  hid_t did;
#endif
  id <Map> stringMaps;
}
- setPrototype: prototype;
- setName: (const char *)name;
- createEnd;
#ifdef HAVE_HDF5
- (hid_t)getTid;
- setDataset: (hid_t)did;
- (void)packObj: (void *)buf to: obj;
- (void)packBuf: obj to: (void *)buf;
#endif
- (void)writeLevel: (const char *)varName;
- (void)writeLevels;
- getPrototype;
- (void)drop;
@end

@interface HDF5_c: CreateDrop_s <HDF5>
{
  id parent;
  const char *name;
  BOOL writeFlag;
  BOOL datasetFlag;
#ifdef HAVE_HDF5
  hid_t loc_id;
  hid_t psid;
  hid_t bsid;
#endif
  fcall_type_t vector_type;
  void *vector_buf;

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
- setWriteFlag: (BOOL)writeFlag;
- setDatasetFlag: (BOOL)datasetFlag;
- setExtensibleVectorType: (fcall_type_t)extensibleVectorType;
- setExtensibleDoubleVector;
#ifdef HAVE_HDF5
- setId: (hid_t)locId;
#endif

- setCompoundType: compoundType;
- setCount: (unsigned)count;
- createEnd;

- setBaseTypeObject: typeObject;

- (BOOL)checkName: (const char *)name;
- (BOOL)checkDatasetName: (const char *)name;

- (BOOL)getDatasetFlag;
- (BOOL)getWriteFlag;
- (fcall_type_t)getDatasetType;
- (size_t)getDatasetRank;
- (size_t)getDatasetDimension: (unsigned)dimNumber;
- getCompoundType;
- (unsigned)getCount;
- (const char *)getName;
- getClass;

- (void)assignIvar: obj;

- (void)nameRecord: (unsigned)recordNumber name: (const char *)recordName;
- (void)numberRecord: (unsigned)recordNumber;
- (void)selectRecord: (unsigned)recordNumber;

- (void)storeTypeName: (const char *)typeName;
- (void)storeComponentTypeName: (const char *)typeName;

- (void)shallowLoadObject: obj;
- (void)shallowStoreObject: obj;

- (void)loadDataset: (void *)ptr;
- (void)storeAsDataset: (const char *)name typeName: (const char *)typeName type: (fcall_type_t)type rank: (unsigned)rank dims: (unsigned *)dims ptr: (void *)ptr;

- (void)addDoubleToVector: (double)val;

- (void)iterate: (int (*) (id hdf5Obj))iterateFunc drop: (BOOL)dropFlag;
- (void)iterate: (int (*) (id hdf5Obj))iterateFunc;

- (void)storeAttribute: (const char *)attributeName value: (const char *)valueString;
- (const char *)getAttribute: (const char *)attrName;
- (void)iterateAttributes: (int (*) (const char *key, const char *value))iterateFunc;

- (const char **)readRowNames;
- (void)writeRowNames;
- (void)writeLevels;

- (void)flush;
- (void)drop;
@end

