// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#include <swarmconfig.h> // HAVE_HDF5


#import <defobj/HDF5Object.h>

#ifdef HAVE_HDF5
#import <defobj/internal.h>

#include <hdf5.h>
#include <misc.h> // strncpy
#include <math.h> // log10

#define REF2STRING_CONV "ref->string"
#define ROWNAMES "row.names"
#define ATTRIB_TYPENAME "type"

static unsigned hdf5InstanceCount = 0;

static hid_t
hdf5_tid_for_objc_type (const char *type)
{
  hid_t tid;

  switch (*type)
    {
    case _C_CHR:
      tid = H5T_NATIVE_CHAR;
      break;
    case _C_UCHR:
      tid = H5T_NATIVE_UCHAR;
      break;
    case _C_SHT:
      tid = H5T_NATIVE_SHORT;
      break;
    case _C_USHT:
      tid = H5T_NATIVE_USHORT;
      break;
    case _C_INT:
      tid = H5T_NATIVE_INT;
      break;
    case _C_UINT:
      tid = H5T_NATIVE_UINT;
      break;
    case _C_LNG:
      tid = H5T_NATIVE_LONG;
      break;
    case _C_ULNG:
      tid = H5T_NATIVE_ULONG;
      break;
    case _C_FLT:
      tid = H5T_NATIVE_FLOAT;
      break;
    case _C_DBL:
      tid = H5T_NATIVE_DOUBLE;
      break;
    default:
      abort ();
    }
  return tid;
}
#endif

@implementation HDF5CompoundType_c
PHASE(Creating)
- setSourceClass: (Class)theClass
{
  class = theClass;
  return self;
}

- createEnd
{
#ifdef HAVE_HDF5
  void insert_var (struct objc_ivar *ivar)
    {
      if (H5Tinsert (tid, ivar->ivar_name, ivar->ivar_offset,
                     hdf5_tid_for_objc_type (ivar->ivar_type)) < 0)
        raiseEvent (SaveError, "unable to insert to compound type");
    }
  
  [super createEnd];
  
  if ((tid = H5Tcreate (H5T_COMPOUND, class->instance_size)) < 0)
    raiseEvent (SaveError, "unable to create compound type");
  
  map_ivars (class->ivars, insert_var);
#else
  hdf5_not_available ();
#endif
  return self;
}

PHASE(Using)
- (void)drop
{
#ifdef HAVE_HDF5
  if (H5Tclose (tid) < 0)
    raiseEvent (SaveError, "unable to close compound type");
  [super drop];
#else
  hdf5_not_available ();
#endif
}
@end

@implementation HDF5_c
PHASE(Creating)
+ createBegin: aZone
{
  HDF5_c *obj = [super createBegin: aZone];

  obj->createGroupFlag = YES;
  return obj;
}

- setParent: theParent
{
  parent = theParent;
  return self;
}

- setName: (const char *)theName
{
  name = theName;
  return self;
}

- setCreateGroupFlag: (BOOL)theCreateGroupFlag
{
  createGroupFlag = theCreateGroupFlag;
  return self;
}

- setRecordType: theCompoundType count: (unsigned)theRecordCount
{
#ifdef HAVE_HDF5
  c_type = theCompoundType;
  c_count = theRecordCount;
  c_rnlen = 1 + (unsigned) log10 ((double) c_count);
#else
  hdf5_not_available ();
#endif
  return self;
}

- setRowNameLength: (size_t)rnlen
{
#ifdef HAVE_HDF5
  c_rnlen = rnlen;
#else
  hdf5_not_available ();
#endif
  return self;
}

#ifdef HAVE_HDF5
static herr_t
ref_string (hid_t sid, hid_t did, H5T_cdata_t *cdata,
            size_t count, void *buf, void *bkg)
{
  if (cdata->command == H5T_CONV_CONV)
    {
      const char *srcbuf[count];
      char *destbuf = buf;
      const char **recptr = srcbuf;
      size_t i;
      size_t maxlen = H5Tget_size (did);

      memcpy (srcbuf, buf, sizeof (srcbuf));

      for (i = 0; i < count; i++)
        {
          strncpy (destbuf, *recptr, maxlen);
          recptr++;
          destbuf += maxlen;
        }
    }
  return 0;
}

#endif

- createEnd
{
#ifdef HAVE_HDF5
  [super createEnd];
  if (parent == nil)
    {
      if ((loc_id = H5Fcreate (name, H5F_ACC_TRUNC,
                               H5P_DEFAULT, H5P_DEFAULT))  < 0)
        raiseEvent (SaveError, "Failed to create HDF5 file `%s'", name);
    }
  else if (c_type)
    {
      hsize_t dims[1];
      hsize_t mdims[1];

      dims[0] = c_count;
      if ((c_sid = H5Screate_simple (1, dims, NULL)) < 0)
        raiseEvent (SaveError, "unable to create (compound) space");

      mdims[0] = 1;
      if ((c_msid = H5Screate_simple (1, mdims, NULL)) < 0)
        raiseEvent (SaveError, "unable to create (compound) point space");
      
      loc_id = ((HDF5_c *) parent)->loc_id;
      if ((c_did = H5Dcreate (loc_id,
                              name,
                              ((HDF5CompoundType_c *) c_type)->tid,
                              c_sid,
                              H5P_DEFAULT)) < 0)
        raiseEvent (SaveError, "unable to create (compound) dataset");

      {
        hsize_t dims[1];
        
        if ((c_rntid = H5Tcopy (H5T_C_S1)) < 0)
          raiseEvent (SaveError, "unable to copy string type");
        if ((H5Tset_size (c_rntid, c_rnlen + 1)) < 0)
          raiseEvent (SaveError, "unable to set string size");
        dims[0] = c_count;
        if ((c_rnsid = H5Screate_simple (1, dims, NULL)) < 0)
          raiseEvent (SaveError, "unable to create row names data space");
        
        if ((c_rnaid = H5Acreate (c_did, ROWNAMES,
                                  c_rntid, c_rnsid, H5P_DEFAULT)) < 0)
          raiseEvent (SaveError, 
                      "unable to create row names attribute dataset");
        c_rnbuf = xcalloc (c_count, sizeof (const char *));
      }
    }
  else
    {
      if (createGroupFlag)
        {
          if ((loc_id = H5Gcreate (((HDF5_c *) parent)->loc_id, name, 0)) < 0)
            raiseEvent (SaveError, "Failed to create HDF5 group `%s'", name);
        }
      else
        loc_id = ((HDF5_c *) parent)->loc_id;
    }
  if (hdf5InstanceCount == 0)
    {
      if (H5Tregister_soft (REF2STRING_CONV,
                            H5T_REFERENCE,
                            H5T_STRING, ref_string) == -1)
        raiseEvent (SaveError, "unable to register ref->string converter");
    }
  hdf5InstanceCount++;
#else
  hdf5_not_available ();
#endif
  return self;
}

PHASE(Using)

- (const char *)getName
{
  return name;
}

- storeTypeName: (const char *)typeName
{
  hid_t type_tid, type_sid, type_aid;
  hsize_t dims[1];
  hid_t did = c_type ? c_did : loc_id;
  
  dims[0] = 1;
  
  if ((type_tid = H5Tcopy (H5T_C_S1)) < 0)
    raiseEvent (SaveError, "unable to copy string type");
  if ((H5Tset_size (type_tid, strlen (typeName) + 1)) < 0)
    raiseEvent (SaveError, "unable to set string size");
  if ((type_sid = H5Screate_simple (1, dims, NULL)) < 0)
    raiseEvent (SaveError, "unable to create row names data space");
  
  if ((type_aid = H5Acreate (did, ATTRIB_TYPENAME,
                             type_tid, type_sid, H5P_DEFAULT)) < 0)
    raiseEvent (SaveError, 
                "unable to create type attribute dataset");
  
  if (H5Awrite (type_aid, type_tid, (void *) typeName) < 0)
    raiseEvent (SaveError, "unable to write type name attribute");

  if (H5Aclose (type_aid) < 0)
    raiseEvent (SaveError, "unable to close type name attribute");

  if (H5Tclose (type_tid) < 0)
    raiseEvent (SaveError, "unable to close type name type");
  
  if (H5Sclose (type_sid) < 0)
    raiseEvent (SaveError, "unable to close type name space");
  return self;
}

- storeAsDataset: (const char *)datasetName type: (const char *)type ptr: (void *)ptr
{
#ifdef HAVE_HDF5
  hid_t scalar_space (void)
    {
      hid_t space;
      hsize_t dims[1];
      
      dims[0] = 1;
      if ((space = H5Screate_simple (1, dims, NULL)) == -1)
        raiseEvent (SaveError, "unable to create dataspace");
      return space;
    }

  void store (hid_t sid, hid_t memtid, hid_t tid)
    {
      hid_t did;
      
      if ((did = H5Dcreate (loc_id, datasetName,
                            tid, sid, H5P_DEFAULT)) < 0)
        raiseEvent (SaveError, "unable to create dataset `%s'", datasetName);
      
      
      if (H5Dwrite (did, memtid, sid, sid, H5P_DEFAULT, ptr) < 0)
        raiseEvent (SaveError, "unable to write to dataset `%s'", datasetName);

      if (H5Dclose (did) < 0)
        raiseEvent (SaveError, "unable to close dataset `%s'", datasetName);
      if (H5Sclose (sid) < 0)
        raiseEvent (SaveError, "unable to close dataspace");
    }
  void store_string (hid_t sid)
    {
      const char *str = *(const char **)ptr;
      hid_t memtid, tid;
      
      if ((memtid = H5Tcopy (H5T_STD_REF_OBJ)) < 0)
        raiseEvent (SaveError, "unable to copy reference type");
      if ((H5Tset_size (memtid, sizeof (const char *))) < 0)
        raiseEvent (SaveError, "unable to set size of reference type");

      if ((tid = H5Tcopy (H5T_C_S1)) < 0)
        raiseEvent (SaveError, "unable to copy string type");
      if ((H5Tset_size (tid, strlen (str) + 1)) < 0)
        raiseEvent (SaveError, "unable to set size of string type");

      store (sid, memtid, tid);
      
      if (H5Tclose (memtid) < 0)
        raiseEvent (SaveError, "unable to close reference type");

      if (H5Tclose (tid) < 0)
        raiseEvent (SaveError, "unable to close string type");
    }

  if (*type == _C_ARY_B)
    {
      void hdf5_setup_array (unsigned rank, unsigned *dims, 
                             const char *baseType)
        {
          hssize_t hdf5dims[rank];
          unsigned i;
          hid_t sid;
          
          for (i = 0; i < rank; i++)
            hdf5dims[i] = dims[i];
          
          if ((sid = H5Screate_simple (rank, hdf5dims, NULL)) < 0)
            raiseEvent (SaveError, "unable to create array dataspace");
          
          if (*baseType == _C_CHARPTR)
            store_string (sid);
          else
            {
              hid_t tid = hdf5_tid_for_objc_type (baseType);

              store (sid, tid, tid);
            }
        }
      process_array (type,
                     hdf5_setup_array,
                     NULL, NULL,
                     NULL, NULL, 
                     NULL,
                     ptr,
                     NULL);
    }
  else if (*type == _C_CHARPTR)
    store_string (scalar_space ());
  else
    {
      hid_t tid = hdf5_tid_for_objc_type (type);

      store (scalar_space (), tid, tid);
    }
#else
  hdf5_not_available ();
#endif
  return self;
}

- selectRecord: (unsigned)recordNumber
{
#ifdef HAVE_HDF5
  hssize_t coord[1][1];
  
  coord[0][0] = recordNumber;
  if (H5Sselect_elements (c_sid, H5S_SELECT_SET, 1,
                          (const hssize_t **) coord) < 0)
    raiseEvent (SaveError, "unable to select record: %u", recordNumber);
#else
  hdf5_not_available ();
#endif
  return self;
}

- nameRecord: (unsigned)recordNumber name: (const char *)recordName
{
#ifdef HAVE_HDF5
  c_rnbuf[recordNumber] = strdup (recordName);
#else
  hdf5_not_available ();
#endif
  return self;
}

- numberRecord: (unsigned)recordNumber
{
#ifdef HAVE_HDF5
  char fmt[2 + c_rnlen + 1 + 1];
  char buf[c_rnlen + 1];

  sprintf (fmt, "%%0%uu", (unsigned) c_rnlen);
  sprintf (buf, fmt, recordNumber);
  
  c_rnbuf[recordNumber] = strdup (buf);
#else
  hdf5_not_available ();
#endif
  return self;
}

- storeObject: obj
{
#ifdef HAVE_HDF5
  if (H5Dwrite (c_did, ((HDF5CompoundType_c *) c_type)->tid,
                c_msid, c_sid, H5P_DEFAULT, obj) < 0)
    raiseEvent (SaveError, "unable to store object");
#else
  hdf5_not_available ();
#endif
  return self;
}

- (void)drop
{
#ifdef HAVE_HDF5
  if (parent == nil)
    {
      if (H5Fclose (loc_id) < 0)
        raiseEvent (SaveError, "Failed to close HDF5 file");
    }
  else if (c_type)
    {
      hid_t rnmemtid;

      if ((rnmemtid = H5Tcopy (H5T_STD_REF_OBJ)) < 0)
        raiseEvent (SaveError, "unable to copy reference type");
      if ((H5Tset_size (rnmemtid, sizeof (const char *))) < 0)
        raiseEvent (SaveError, "unable to set size of reference type");

      if (H5Awrite (c_rnaid, rnmemtid, c_rnbuf) < 0)
        raiseEvent (SaveError, "unable to write row names dataset");
      if (H5Aclose (c_rnaid) < 0)
        raiseEvent (SaveError, "unable to close row names dataset");
      if (H5Sclose (c_rnsid) < 0)
        raiseEvent (SaveError, "unable to close row names dataspace");
      if (H5Tclose (c_rntid) < 0)
        raiseEvent (SaveError, "unable to close row names string type");
      {
        unsigned ri;

        for (ri = 0; ri < c_count; ri++)
          if (c_rnbuf[ri])
            XFREE (c_rnbuf[ri]);
        XFREE (c_rnbuf);
      }

      if (H5Sclose (c_sid) < 0)
        raiseEvent (SaveError, "Failed to close (compound) space");
      if (H5Sclose (c_msid) < 0)
        raiseEvent (SaveError, "Failed to close (compound) point space");
      if (H5Dclose (c_did) < 0)
        raiseEvent (SaveError, "Failed to close (compound) dataset");
      
    }
  else
    {
      if (createGroupFlag)
        if (H5Gclose (loc_id) < 0)
          raiseEvent (SaveError, "Failed to close HDF5 group");
    }
  hdf5InstanceCount--;
  if (hdf5InstanceCount == 0)
    {
      if (H5Tunregister (ref_string) == -1)
        raiseEvent (SaveError, "unable to unregister ref->string converter");
    }
  [super drop];
#else
  hdf5_not_available ();
#endif
}  

@end
