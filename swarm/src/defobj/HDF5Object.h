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

#import <Swarm/swarmconfig.h>

#ifdef HAVE_HDF5
#define id hdf5id
#include <hdf5.h>
#undef id
#endif

#import <Swarm/Create.h>
#import <Swarm/collections.h> // Map
#import <Swarm/internal.h> // fcall_type_t

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
  hid_t plist;
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
- getParent;
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
- (const char *)getHDF5Name;
- (Class)getClass;

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

