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

@interface HDF5CompoundType_c: CreateDrop_s
{
  Class class;
@public
#ifdef HAVE_HDF5
  hid_t tid;
#endif
}
- setSourceClass: (Class)class;
- createEnd;
- (void)drop;
@end

@interface HDF5_c: CreateDrop_s
{
  id parent;
  const char *name;
  BOOL createGroupFlag;
#ifdef HAVE_HDF5
  hid_t loc_id;
#endif

  id <HDF5CompoundType> c_type;

  unsigned c_count;
#ifdef HAVE_HDF5
  hid_t c_sid;
  hid_t c_msid;
  hid_t c_did;
  
  hsize_t c_rnlen;
  hid_t c_rntid;
  hid_t c_rnsid;
  hid_t c_rnaid;
#endif
  const char **c_rnbuf;
}
+ createBegin: aZone;
- setParent: parent;
- setName: (const char *)name;
- setCreateGroupFlag: (BOOL)createGroupFlag;
- setRecordType: compoundType count: (unsigned)count;
- setRowNameLength: (size_t)len;
- createEnd;

- nameRecord: (unsigned)recordNumber name: (const char *)recordName;
- numberRecord: (unsigned)recordNumber;
- selectRecord: (unsigned)recordNumber;
- storeTypeName: (const char *)typeName;
- storeAttribute: (const char *)attributeName value: (const char *)valueString;
- storeObject: obj;
- storeAsDataset: (const char *)name typeName: (const char *)typeName type: (const char *)type ptr: (void *)ptr;
- (const char *)getName;
- (void)drop;
@end
