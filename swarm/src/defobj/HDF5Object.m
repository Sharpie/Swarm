// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#include <swarmconfig.h>

#ifdef HAVE_HDF5

#import <defobj/HDF5Object.h>
#import <defobj/internal.h>

#include <hdf5.h>


@implementation HDF5_c
PHASE(Creating)
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

- createEnd
{
  [super createEnd];
  if (parent == nil)
    {
      if ((loc_id = H5Fcreate (name, H5F_ACC_TRUNC,
                               H5P_DEFAULT, H5P_DEFAULT))  < 0)
        raiseEvent (SaveError, "Failed to create HDF5 file `%s'", name);
    }
  else
    {
      if ((loc_id = H5Gcreate (((HDF5_c *) parent)->loc_id, name, 0)) < 0)
        raiseEvent (SaveError, "Failed to create HDF5 group `%s'", name);
    }
  return self;
}

PHASE(Using)

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

- store: (const char *)datasetName type: (const char *)type ptr: (void *)ptr
{
  hid_t scalar_space (void)
    {
      hid_t space;
      hsize_t dims[1];
      
      dims[0] = 1;
      if ((space = H5Screate_simple (1, dims, NULL)) == -1)
        raiseEvent (SaveError, "unable to create dataspace");
      return space;
    }

  void store (hid_t sid, hid_t tid)
    {
      hid_t did;
      
      if ((did = H5Dcreate (loc_id, datasetName,
                            tid, sid, H5P_DEFAULT)) < 0)
        raiseEvent (SaveError, "unable to store %s as char", datasetName);
      
      if (H5Dwrite (did, tid, sid, sid, H5P_DEFAULT, ptr) < 0)
        raiseEvent (SaveError, "unable to write %s as char", datasetName);
      if (H5Dclose (did) < 0)
        raiseEvent (SaveError, "unable to close dataset %s", datasetName);
      if (H5Sclose (sid) < 0)
        raiseEvent (SaveError, "unable to close dataspace");
    }
  void store_string (hid_t sid)
    {
      const char *str = *(const char **)ptr;
      hid_t tid = H5Tcopy (H5T_C_S1);
      H5Tset_size (tid, strlen (str) + 1);
      
      store (sid, tid);
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
            store (sid, hdf5_tid_for_objc_type (baseType));
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
    store (scalar_space (), hdf5_tid_for_objc_type (type));
  return self;
}

- (void)drop
{
  if (parent == nil)
    {
      if (H5Fclose (loc_id) < 0)
        raiseEvent (SaveError, "Failed to close HDF5 file");
    }
  else
    {
      if (H5Gclose (loc_id) < 0)
        raiseEvent (SaveError, "Failed to close HDF5 group");
    }
}  

@end
#endif
