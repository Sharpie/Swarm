// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#include <swarmconfig.h> // HAVE_HDF5


#import <defobj/HDF5Object.h>

#ifdef HAVE_HDF5
#import <defobj/internal.h> // map_ivars

#include <hdf5.h>
#include <misc.h> // strncpy
#include <math.h> // log10

#define REF2STRING_CONV "ref->string"
#define STRING2REF_CONV "string->ref"
#define ROWNAMES "row.names"

static unsigned hdf5InstanceCount = 0;

static hid_t
tid_for_objc_type (const char *type)
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

static hid_t
make_string_ref_type (void)
{
  hid_t memtid;
  
  if ((memtid = H5Tcopy (H5T_STD_REF_OBJ)) < 0)
    raiseEvent (LoadError, "Unable to copy H5T_STD_REF_OBJ");
  if (H5Tset_size (memtid, sizeof (const char *)) < 0)
    raiseEvent (LoadError, "unable to set size of reference type");
  return memtid;
}    

static const char *
get_attribute (hid_t oid, const char *attrName)
{
  hid_t aid, sid, tid;
  H5T_class_t class;
  const char *ret = NULL;
  hid_t str_ref_tid;

  str_ref_tid = make_string_ref_type ();
  
  if ((aid = H5Aopen_name (oid, attrName)) < 0)
    raiseEvent (LoadError, "could not open attribute `%s'", attrName);
  
  if ((sid = H5Aget_space (aid)) < 0)
    raiseEvent (LoadError,
                "could not open space of attribute `%s'",
                attrName);
  
  if ((tid = H5Aget_type (aid)) < 0)
    raiseEvent (LoadError,
                "could not get type of attribute `%s'", attrName);
  
  if ((class = H5Tget_class (tid)) < 0)
    raiseEvent (LoadError,
                "could not get type class of attribute `%s'", attrName);
  {
    int rank;
    
    if ((rank = H5Sget_simple_extent_ndims (sid)) < 0)
      raiseEvent  (LoadError,
                   "could not get rank of attribute space `%s'",
                   attrName);
    {
      hsize_t dims[rank];
      
      if (H5Sget_simple_extent_dims (sid, dims, NULL) < 0)
        raiseEvent (LoadError,
                    "could not get extent of attribute space `%s'",
                    attrName);
      
      if (rank == 1 && dims[0] == 1 && class == H5T_STRING)
        {
          if (H5Aread (aid, str_ref_tid, &ret) < 0)
            raiseEvent (LoadError,
                        "unable to read attribute `%s'",
                        attrName);
        }
    }
  }
  if (H5Aclose (aid) < 0)
    raiseEvent (LoadError, "unable to close attribute `%s'", attrName);
  if (H5Tclose (tid) < 0)
    raiseEvent (LoadError, "unable to close attribute `%s' tid", attrName);
  if (H5Sclose (sid) < 0)
    raiseEvent (LoadError,
                "unable to close attribute `%s' space",
                attrName);
  if (H5Tclose (str_ref_tid) < 0)
    raiseEvent (LoadError,
                "unable to close string reference type");

  return ret;
}

static const char *
objc_type_for_tid (hid_t tid)
{
  H5T_class_t tid_class;
  size_t tid_size;
  const char *type;

  if ((tid_class = H5Tget_class (tid)) < 0)
    raiseEvent (LoadError, "cannot get class of tid");

  if ((tid_size = H5Tget_size (tid)) < 0)
    raiseEvent (LoadError, "cannot get size of tid");

  switch (tid_class)
    {
    case H5T_INTEGER:
      {
        H5T_sign_t tid_sign;

        if ((tid_sign = H5Tget_sign (tid)) < 0)
          raiseEvent (LoadError, "cannot get sign type of tid");
        
        if (tid_size == sizeof (char))
          if (tid_sign == H5T_SGN_2)
            type = @encode (char);
          else
            type = @encode (unsigned char);
        else if (tid_size == sizeof (short))
          if (tid_sign == H5T_SGN_2)
            type = @encode (short);
          else
            type = @encode (unsigned short);
        else if (tid_size == sizeof (int))
          if (tid_sign == H5T_SGN_2)
            type = @encode (int);
          else
            type = @encode (unsigned);
        else if (tid_size == sizeof (long))
          if (tid_sign == H5T_SGN_2)
            type = @encode (long);
          else
            type = @encode (unsigned long);
        else
          abort ();
      }
      break;
    case H5T_FLOAT:
      if (tid_size == sizeof (float))
        type = @encode (float);
      else if (tid_size == sizeof (double))
        type = @encode (double);
      else
        abort ();
      break;
    case H5T_STRING:
      type = @encode (const char *);
      break;
    default:
      abort ();
    }
  return type;
}

#endif

@implementation HDF5CompoundType_c
PHASE(Creating)
- setClass: theClass
{
  class = (Class) theClass;
  name = [class name];

  return self;
}

- setName: (const char *)theName
{
  name = theName;
  return self;
}

#ifdef HAVE_HDF5
- setTid: (hid_t)theTid
{
  tid = theTid;
  return self;
}

static hid_t
create_compound_type_from_class (Class class)
{
  hid_t tid;
  void insert_var (struct objc_ivar *ivar)
    {
      if (H5Tinsert (tid, ivar->ivar_name, ivar->ivar_offset,
                     tid_for_objc_type (ivar->ivar_type)) < 0)
        raiseEvent (SaveError, "unable to insert to compound type");
    }
  
  if ((tid = H5Tcreate (H5T_COMPOUND, class->instance_size)) < 0)
    raiseEvent (SaveError, "unable to create compound type");
  
  map_ivars (class->ivars, insert_var);
  return tid;
}

static Class
create_class_from_compound_type (id aZone, hid_t tid, const char *typeName)
{
  unsigned i, count;
  Class class;
  size_t tid_size;
  
  if (H5Tget_class (tid) != H5T_COMPOUND)
    abort ();
  
  if ((count = H5Tget_nmembers (tid)) < 0)
    raiseEvent (LoadError, "unable to get compound type member count");
  
  if (count == 0)
    raiseEvent (LoadError, "compound type should have at least one member");

  if ((tid_size = H5Tget_size (tid)) < 0)
    raiseEvent (LoadError,
                "unable to get compound type size");
  
  if ((class = objc_lookup_class (typeName)))
    {
      struct objc_ivar *ivar_list = class->ivars->ivar_list;
      if (class->ivars->ivar_count != count)
        raiseEvent (LoadError,
                    "differing ivar count in compound type and class `%s'",
                    [class name]);
      
      if (tid_size != class->instance_size)
        raiseEvent (LoadError,
                    "differing compound type and instance size `%s'",
                    [class name]);

      for (i = 0; i < count; i++)
        {
          const char *name;

          if ((name = H5Tget_member_name (tid, i)) < 0)
            raiseEvent (LoadError,
                        "unable to get compound type member name #%u",
                        i);
          if (strcmp (name, ivar_list[i].ivar_name) != 0)
            raiseEvent (LoadError,
                        "compound type member name != ivar name `%s' != `%s'",
                        name, ivar_list[i].ivar_name);
          {
            hid_t mtid;
            const char *type;

            if ((mtid = H5Tget_member_type (tid, i)) < 0)
              raiseEvent (LoadError,
                          "unable to get compound type member type #%u",
                          i);
            
            type = objc_type_for_tid (mtid);
            
            if (strcmp (type, ivar_list[i].ivar_type) != 0)
              raiseEvent (LoadError,
                          "compound type member type != ivar type `%s' != `%s'",
                          type, ivar_list[i].ivar_type);
          }
          {
            int offset;
            
            if ((offset = H5Tget_member_offset (tid, i)) < 0)
              raiseEvent (LoadError,
                          "unable to get compound type offset #%u",
                          i);
            if (offset != ivar_list[i].ivar_offset)
              raiseEvent (LoadError,
                          "compound type member pos != ivar pos `%u' != `%u'",
                          offset, ivar_list[i].ivar_offset);
          }
        }
      return class;
    }
  else
    {
      Class newClass = [CreateDrop class];
      id classObj = [id_BehaviorPhase_s createBegin: aZone];
      struct objc_ivar_list *ivars =
        xmalloc (sizeof (struct objc_ivar_list) +
                 (count - 1) * sizeof (struct objc_ivar));
      struct objc_ivar *ivar_list = ivars->ivar_list;

      [classObj setName: strdup (typeName)];
      [classObj setClass: getClass (newClass)];
      [classObj setDefiningClass: newClass];
      [classObj setSuperclass: newClass];

      ((Class) classObj)->instance_size = tid_size;

      for (i = 0; i < count; i++)
        {
          hid_t mtid;

          if ((ivar_list[i].ivar_name = H5Tget_member_name (tid, i)) < 0)
            raiseEvent (LoadError,
                        "unable to get compound type member name #%u",
                        i);
          if ((mtid = H5Tget_member_type (tid, i)) < 0)
            raiseEvent (LoadError,
                        "unable to get compound type member type #%u",
                        i);
          ivar_list[i].ivar_type = objc_type_for_tid (mtid);
          if ((ivar_list[i].ivar_offset = H5Tget_member_offset (tid, i)) < 0)
            raiseEvent (LoadError,
                        "unable to get compound type offset #%u",
                        i);
        }
      ((Class) classObj)->ivars = ivars;
      return classObj;
    }

}
#endif

- createEnd
{
#ifdef HAVE_HDF5
  
  [super createEnd];
  if (class != Nil)
    tid = create_compound_type_from_class (class);
  else if (tid != 0)
    class = create_class_from_compound_type ([self getZone], tid, name);
  else
    abort();
#else
  hdf5_not_available ();
#endif
  return self;
}

PHASE(Using)

#ifdef HAVE_HDF5
- (hid_t)getTid
{
  return tid;
}
#endif

- getClass
{
  return class;
}

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

  obj->createFlag = NO;
  obj->datasetFlag = NO;
#ifdef HAVE_HDF5
  obj->loc_id = 0;
#endif
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

#ifdef HAVE_HDF5
- setId: (hid_t)theLocId
{
  loc_id = theLocId;
  return self;
}
#endif

- setCreateFlag: (BOOL)theCreateFlag
{
  createFlag = theCreateFlag;
  return self;
}

- setDatasetFlag: (BOOL)theDatasetFlag
{
  datasetFlag = theDatasetFlag;
  return self;
}

- setCompoundType: theCompoundType count: (unsigned)theRecordCount
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

static herr_t
string_ref (hid_t sid, hid_t did, H5T_cdata_t *cdata,
            size_t count, void *buf, void *bkg)
{
  if (cdata->command == H5T_CONV_CONV)
    {
      size_t size = H5Tget_size (sid);
      unsigned char srcbuf[size * count], *srcptr = srcbuf;
      size_t i;
      
      memcpy (srcbuf, buf, sizeof (srcbuf));
      for (i = 0; i < count;i ++)
        {
          ((const char **) buf)[i] = strdup (srcptr);
          srcptr += size;
        }
    }
  return 0;
}

#endif

- createEnd
{
#ifdef HAVE_HDF5
  [super createEnd];
  if (createFlag)
    {
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
          
          if ((loc_id = H5Dcreate (((HDF5_c *) parent)->loc_id,
                                   name,
                                   [c_type getTid],
                                   c_sid,
                                   H5P_DEFAULT)) < 0)
            raiseEvent (SaveError, "unable to create (compound) dataset");
          
          {
            c_rnbuf = xcalloc (c_count, sizeof (const char *));
          }
        }
      else
        {
          if (!datasetFlag)
            {
              if ((loc_id = H5Gcreate (((HDF5_c *) parent)->loc_id, name, 0))
                  < 0)
                raiseEvent (SaveError, "Failed to create HDF5 group `%s'",
                            name);
            }
          else
            loc_id = ((HDF5_c *) parent)->loc_id;
        }
    }
  else
    {
      if (parent == nil)
        {
          if ((loc_id = H5Fopen (name, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
            raiseEvent (LoadError, "Failed to open HDF5 file `%s'", name);
        }
      else
        {
          if (loc_id == 0)
            {
              if ((loc_id = H5Gopen (((HDF5_c *) parent)->loc_id, name)) < 0)
                raiseEvent (LoadError, "Failed to open HDF5 group `%s'", name);
            }
          if (datasetFlag)
            {
              const char *typeName = get_attribute (loc_id, ATTRIB_TYPE_NAME);
              hid_t tid;

              if ((tid = H5Dget_type (loc_id)) < 0)
                raiseEvent (LoadError, "Failed to get type of dataset");

              if (strcmp (typeName, "String") != 0)
                c_type = [[[[HDF5CompoundType createBegin: [self getZone]]
                             setTid: tid]
                            setName: typeName]
                           createEnd];
            }
        }
    }
  
  if (hdf5InstanceCount == 0)
    {
      if (H5Tregister_soft (REF2STRING_CONV,
                            H5T_REFERENCE,
                            H5T_STRING, ref_string) == -1)
        raiseEvent (SaveError, "unable to register ref->string converter");
      if (H5Tregister_soft (STRING2REF_CONV,
                            H5T_STRING,
                            H5T_REFERENCE, string_ref) == -1)
        raiseEvent (LoadError, "unable to register string->ref converter");
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

- (BOOL)getDatasetFlag
{
  return datasetFlag;
}

- getCompoundType
{
  return c_type;
}

- iterateAttributes: (void (*) (const char *key, const char *value))iterateFunc
{
#ifdef HAVE_HDF5

  herr_t process_attribute (hid_t oid, const char *attrName, void *client)
    {
      const char *value = get_attribute (oid, attrName);

      if (value)
        iterateFunc (attrName, value);

      return 0;
    }
  
  if (H5Aiterate (loc_id, NULL, process_attribute, NULL) < 0)
    raiseEvent (LoadError, "unable to iterate over attributes");
  
#else
  hdf5_not_available ();
#endif
  return self;
}

- iterate: (void (*) (id hdf5obj))iterateFunc
{
#ifdef HAVE_HDF5
  herr_t process_object (hid_t oid, const char *memberName, void *client)
    {
      H5G_stat_t statbuf;
      
      if (H5Gget_objinfo (oid, memberName, 1, &statbuf) == -1)
        raiseEvent (LoadError, "Cannot query object `%s'", memberName);  
      
      if (statbuf.type == H5G_GROUP)
        {
          hid_t gid;
          id group;
          
          if ((gid = H5Gopen (oid, memberName)) < 0)
            raiseEvent (LoadError, "cannot open group `%s'", memberName);
          group = [[[[[[HDF5 createBegin: [self getZone]]
                        setParent: self]
                       setCreateFlag: NO]
                      setName: memberName]
                     setId: gid]
                    createEnd];
          iterateFunc (group);
          [group drop];
        }
      else if (statbuf.type == H5G_DATASET)
        {
          id dataset;
          hid_t did;

          if ((did = H5Dopen (oid, memberName)) < 0)
            raiseEvent (LoadError, "cannot open dataset `%s'", memberName);
          dataset = [[[[[[[HDF5 createBegin: [self getZone]]
                           setParent: self]
                          setCreateFlag: NO]
                         setDatasetFlag: YES]
                        setName: memberName]
                       setId: did]
                      createEnd];
          iterateFunc (dataset);
          [dataset drop];
        }
      else
        raiseEvent (LoadError, "Cannot process HDF5 type %u",
                    (unsigned) statbuf.type);
      return 0;
    }
  if (H5Giterate (loc_id, ".", NULL, process_object, self) < 0)
    raiseEvent (LoadError, "cannot iterate over HDF5 objects");
#else
  hdf5_not_available ();
#endif  
  return self;
}

- storeTypeName: (const char *)typeName
{
#ifdef HAVE_HDF5
  [self storeAttribute: ATTRIB_TYPE_NAME value: typeName];
#else
  hdf5_not_available ();
#endif
  return self;
}

#ifdef HAVE_HDF5
static void
hdf5_store_attribute (hid_t did,
                      const char *attributeName,
                      const char *valueString)
{
  hid_t type_tid, type_sid, type_aid;
  hsize_t dims[1];
  
  dims[0] = 1;
  
  if ((type_tid = H5Tcopy (H5T_C_S1)) < 0)
    raiseEvent (SaveError, "unable to copy string type");
  if ((H5Tset_size (type_tid, strlen (valueString) + 1)) < 0)
    raiseEvent (SaveError, "unable to set string size");
  if ((type_sid = H5Screate_simple (1, dims, NULL)) < 0)
    raiseEvent (SaveError, "unable to create attribute data space");
  
  if ((type_aid = H5Acreate (did, attributeName,
                             type_tid, type_sid, H5P_DEFAULT)) < 0)
    raiseEvent (SaveError, 
                "unable to create type attribute dataset");
  
  if (H5Awrite (type_aid, type_tid, (void *) valueString) < 0)
    raiseEvent (SaveError, "unable to write type name attribute");

  if (H5Aclose (type_aid) < 0)
    raiseEvent (SaveError, "unable to close type name attribute");

  if (H5Tclose (type_tid) < 0)
    raiseEvent (SaveError, "unable to close type name type");
  
  if (H5Sclose (type_sid) < 0)
    raiseEvent (SaveError, "unable to close type name space");
}
#endif

- storeAttribute: (const char *)attributeName value: (const char *)valueString
{
#ifdef HAVE_HDF5
  hdf5_store_attribute (loc_id, attributeName, valueString);
#else
  hdf5_not_available ();
#endif
  return self;
}

- storeAsDataset: (const char *)datasetName
        typeName: (const char *)typeName
            type: (const char *)type
             ptr: (void *)ptr
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

      if (typeName)
        hdf5_store_attribute (did, ATTRIB_TYPE_NAME, typeName);
      
      if (H5Dclose (did) < 0)
        raiseEvent (SaveError, "unable to close dataset `%s'", datasetName);
      if (H5Sclose (sid) < 0)
        raiseEvent (SaveError, "unable to close dataspace");
    }
  void store_string (hid_t sid)
    {
      const char *str = *(const char **) ptr;
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
              hid_t tid = tid_for_objc_type (baseType);

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
      hid_t tid = tid_for_objc_type (type);

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
  if (H5Dwrite (loc_id, [c_type getTid],
                c_msid, c_sid, H5P_DEFAULT, obj) < 0)
    raiseEvent (SaveError, "unable to store object");
#else
  hdf5_not_available ();
#endif
  return self;
}

- writeRowNames
{
#ifdef HAVE_HDF5
  hsize_t dims[1];
  hid_t rnmemtid = make_string_ref_type ();
  hid_t rntid, rnsid, rnaid;
  
  dims[0] = c_count;
  if ((rnsid = H5Screate_simple (1, dims, NULL)) < 0)
    raiseEvent (SaveError, "unable to create row names data space");

  if ((rntid = H5Tcopy (H5T_C_S1)) < 0)
    raiseEvent (SaveError, "unable to copy string type");
  if ((H5Tset_size (rntid, c_rnlen + 1)) < 0)
    raiseEvent (SaveError, "unable to set string size");
  
  if ((rnaid = H5Acreate (loc_id, ROWNAMES,
                          rntid, rnsid, H5P_DEFAULT)) < 0)
    raiseEvent (SaveError, 
                "unable to create row names attribute dataset");
  
  if (H5Awrite (rnaid, rnmemtid, c_rnbuf) < 0)
    raiseEvent (SaveError, "unable to write row names dataset");
  
  if (H5Tclose (rntid) < 0)
    raiseEvent (SaveError, "unable to close row names string type");
  if (H5Aclose (rnaid) < 0)
    raiseEvent (SaveError, "unable to close row names dataset");
  if (H5Sclose (rnsid) < 0)
    raiseEvent (SaveError, "unable to close row names dataspace");
  if (H5Tclose (rnmemtid) < 0)
    raiseEvent (SaveError, "unable to close row names reference type");
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
      if (createFlag)
        {
          unsigned ri;
          if (H5Sclose (c_sid) < 0)
            raiseEvent (SaveError, "Failed to close (compound) space");
          if (H5Sclose (c_msid) < 0)
            raiseEvent (SaveError, "Failed to close (compound) point space");

          for (ri = 0; ri < c_count; ri++)
            if (c_rnbuf[ri])
              XFREE (c_rnbuf[ri]);
          XFREE (c_rnbuf);
        }
      if (H5Dclose (loc_id) < 0)
        raiseEvent (SaveError, "Failed to close (compound) dataset");
    }
  else
    {
      if (!datasetFlag)
        if (H5Gclose (loc_id) < 0)
          raiseEvent (SaveError, "Failed to close HDF5 group");
    }
  hdf5InstanceCount--;
  if (hdf5InstanceCount == 0)
    {
      if (H5Tunregister (ref_string) == -1)
        raiseEvent (SaveError, "unable to unregister ref->string converter");
      if (H5Tunregister (string_ref) == -1)
        raiseEvent (LoadError, "unable to unregister string->ref converter");
    }
  [super drop];
#else
  hdf5_not_available ();
#endif
}  

@end
