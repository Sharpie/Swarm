// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#include <swarmconfig.h>

#ifdef HAVE_HDF5

#define id hdf5id
#include <hdf5.h>
#undef id

#import <defobj/Create.h>
#import <collections.h> // Map

@interface HDF5CompoundType_c: CreateDrop_s
{
  Class class;
@public
  hid_t tid;
}
- setSourceClass: (Class)class;
- createEnd;
- (void)drop;
@end

@interface HDF5_c: CreateDrop_s
{
  hid_t loc_id;
  id parent;
  const char *name;

  id <HDF5CompoundType> c_type;
  unsigned c_count;
  hid_t c_sid;
  hid_t c_msid;
  hid_t c_did;
}
- setParent: parent;
- setName: (const char *)name;
- setType: compoundType count: (unsigned)count;
- createEnd;
- selectRecord: (unsigned)recordNumber;
- storeObject: obj;
- storeAsDataset: (const char *)name type: (const char *)type ptr: (void *)ptr;
- (void)drop;
@end

#endif
