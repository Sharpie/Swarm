// Swarm library. Copyright � 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#include <swarmconfig.h> // HAVE_HDF5, PTRINT, HAVE_JDK

#import <defobj/HDF5Object.h>
#import <defobj.h> // STRDUP, ZSTRDUP, SSTRDUP, FREEBLOCK, SFREEBLOCK
#import <defobj/defalloc.h> // getZone
#import <defobj/directory.h> // swarm_directory_ensure_class_named

#import "internal.h" // map_object_ivars, class_generate_name, ivar_ptr_for_name
#ifdef HAVE_JDK
#import "java.h"
#import <defobj/JavaProxy.h>
#endif

#ifdef HAVE_HDF5

#include <hdf5.h>
#include <misc.h> // strncpy, log10, INT_MIN

#define REF2STRING_CONV "ref->string"
#define STRING2REF_CONV "string->ref"
#define ROWNAMES "row.names"
#define LEVELSPREFIX "levels."

static unsigned hdf5InstanceCount = 0;

static hid_t
tid_for_fcall_type (fcall_type_t type)
{
  hid_t tid;

  switch (type)
    {
    case fcall_type_boolean:
      tid = H5T_NATIVE_UCHAR;
      break;
    case fcall_type_schar:
      tid = H5T_NATIVE_CHAR;
      break;
    case fcall_type_uchar:
      tid = H5T_NATIVE_UCHAR;
      break;
    case fcall_type_sshort:
      tid = H5T_NATIVE_SHORT;
      break;
    case fcall_type_ushort:
      tid = H5T_NATIVE_USHORT;
      break;
    case fcall_type_sint:
      tid = H5T_NATIVE_INT;
      break;
    case fcall_type_uint:
      tid = H5T_NATIVE_UINT;
      break;
    case fcall_type_slong:
      tid = H5T_NATIVE_LONG;
      break;
    case fcall_type_ulong:
      tid = H5T_NATIVE_ULONG;
      break;
    case fcall_type_slonglong:
      tid = H5T_NATIVE_LLONG;
      break;
    case fcall_type_ulonglong:
      tid = H5T_NATIVE_ULLONG;
      break;
    case fcall_type_float:
      tid = H5T_NATIVE_FLOAT;
      break;
    case fcall_type_double:
      tid = H5T_NATIVE_DOUBLE;
      break;
    case fcall_type_long_double:
      tid = H5T_NATIVE_LDOUBLE;
      break;
    default:
      abort ();
    }
  return tid;
}

static BOOL
compare_types (fcall_type_t ivarType, fcall_type_t type)
{
  if (ivarType != type)
    {
      if (ivarType == fcall_type_slong && type == fcall_type_sint
          && sizeof (long) == sizeof (int))
        return YES;
      else if (ivarType == fcall_type_ulong && type == fcall_type_uint
               && sizeof (unsigned long) == sizeof (unsigned))
        return YES;
      else if (ivarType == fcall_type_boolean && type == fcall_type_uchar
               && sizeof (BOOL) == sizeof (unsigned char))
        return YES;
      else if (ivarType == fcall_type_slonglong
               && type == fcall_type_slong
               && sizeof (long long) == sizeof (long))
        return YES;
      else if (ivarType == fcall_type_ulonglong
               && type == fcall_type_ulong
               && sizeof (unsigned long long) == sizeof (unsigned long))
        return YES;
      else
        return NO;
    }
  return YES;
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

static void
suppress_messages (void (*func) ())
{
  void *client_data;
  H5E_auto_t errfunc;

  herr_t quiet_errfunc (void *client_data)
    {
      return 0;
    }

  H5Eget_auto (&errfunc, &client_data);
  H5Eset_auto (quiet_errfunc, NULL);
  func ();
  H5Eset_auto (errfunc, client_data);  
}
  
static unsigned
get_attribute_string_list (hid_t oid,
                           const char *attrName,
                           const char ***strings)
{
  hid_t aid, sid, tid;
  H5T_class_t class;
  hid_t str_ref_tid;
  unsigned retcount = 0;
  void func () { aid = H5Aopen_name (oid, attrName); }

  suppress_messages (func);
  if (aid < 0)
    return 0;

  str_ref_tid = make_string_ref_type ();
  
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
      
      if (rank == 1 && class == H5T_STRING)
        {
          void *ptr;

          retcount = dims[0];

          if (strings)
            {
              ptr = [scratchZone alloc: sizeof (const char *) * retcount];
              
              if (H5Aread (aid, str_ref_tid, ptr) < 0)
                raiseEvent (LoadError,
                            "unable to read attribute `%s'",
                        attrName);
              *strings = ptr;
            }
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

  return retcount;
}

static unsigned
get_attribute_levels_string_list (hid_t oid,
                                  const char *ivarName,
                                  const char ***levelStrings)
{
  char buf[strlen (LEVELSPREFIX) + strlen (ivarName) + 1];
  char *p;
  
  p = stpcpy (buf, LEVELSPREFIX);
  p = stpcpy (p, ivarName);
  
  return get_attribute_string_list (oid, buf, levelStrings);
}

static const char *
get_attribute (hid_t oid, const char *attrName)
{
  const char **ret;
  unsigned retcount = get_attribute_string_list (oid, attrName, &ret);

  if (retcount == 1)
    return ret[0];
  else if (retcount > 1)
    raiseEvent (LoadError,
                "attribute `%s' string list should be of length 1 (%u)",
                attrName, retcount);
  return NULL;
}

static fcall_type_t
fcall_type_for_tid (hid_t tid)
{
  H5T_class_t tid_class;
  size_t tid_size;
  fcall_type_t type;

  if ((tid_class = H5Tget_class (tid)) < 0)
    raiseEvent (LoadError, "cannot get class of tid");

  if ((tid_size = H5Tget_size (tid)) == 0)
    raiseEvent (LoadError, "cannot get size of tid");

  switch (tid_class)
    {
    case H5T_INTEGER:
      {
        H5T_sign_t tid_sign;

        if ((tid_sign = H5Tget_sign (tid)) < 0)
          raiseEvent (LoadError, "cannot get sign type of tid");
        
        if (tid_size == sizeof (char))
          {
            if (tid_sign == H5T_SGN_2)
              type = fcall_type_schar;
            else
              type = fcall_type_uchar;
          }
        else if (tid_size == sizeof (short))
          {
            if (tid_sign == H5T_SGN_2)
              type = fcall_type_sshort;
            else
              type = fcall_type_ushort;
          }
        else if (tid_size == sizeof (int))
          {
            if (tid_sign == H5T_SGN_2)
              type = fcall_type_sint;
            else
              type = fcall_type_uint;
          }
        else if (tid_size == sizeof (long))
          {
            if (tid_sign == H5T_SGN_2)
              type = fcall_type_slong;
            else
              type = fcall_type_ulong;
          }
        else if (tid_size == sizeof (long long))
          {
            if (tid_sign == H5T_SGN_2)
              type = fcall_type_slonglong;
            else
              type = fcall_type_ulonglong;
          }
        else
          abort ();
      }
      break;
    case H5T_FLOAT:
      if (tid_size == sizeof (float))
        type = fcall_type_float;
      else if (tid_size == sizeof (double))
        type = fcall_type_double;
      else if (tid_size == sizeof (long double))
        type = fcall_type_long_double;
      else
        abort ();
      break;
    case H5T_STRING:
      type = fcall_type_string;
      break;
    default:
      abort ();
    }
  return type;
}

#else

static void
hdf5_not_available (void)
{
  raiseEvent (NotImplemented,
              "HDF5 serialization not available on this configuration");
}

#endif


@implementation HDF5CompoundType_c
PHASE(Creating)
- setPrototype: thePrototype
{
  prototype = thePrototype;
  name = [prototype getName];

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
create_compound_type_from_prototype (id prototype)
{
  hid_t tid;
  size_t size;
  size_t offset = 0;
  BOOL insertFlag;

  void insert_var (const char *name, fcall_type_t type, void *ptr,
                   unsigned rank, unsigned *dims)
    {
      if (rank > 0)
        raiseEvent (SaveError, "cannot store arrays in compound types\n");
      else if (type == fcall_type_object)
        raiseEvent (SaveWarning,
                    "cannot store objects in compound types -- skipping\n");
      else
        {
          if (type == fcall_type_string)
            type = fcall_type_sint;
          
          offset = alignsizeto (offset, fcall_type_alignment (type));
          
          if (insertFlag)
            {
              if (H5Tinsert (tid, name, offset,
                             tid_for_fcall_type (type)) < 0)
                raiseEvent (SaveError, "unable to insert to compound type\n");
            }
          
          offset += fcall_type_size (type);
        }
    }

  offset = 0;
  insertFlag = NO;
  map_object_ivars (prototype, insert_var);
  size = offset;

  if ((tid = H5Tcreate (H5T_COMPOUND, size)) < 0)
    raiseEvent (SaveError, "unable to create compound type");
  
  offset = 0;
  insertFlag = YES;
  map_object_ivars (prototype, insert_var);
  
  return tid;
}

static void
create_class_from_compound_type (id aZone,
                                 hid_t tid,
                                 hid_t did,
                                 const char *typeName,
                                 Class *classPtr)
{
  unsigned i, count;
  size_t tid_size;
  Class class;
  
  if (H5Tget_class (tid) != H5T_COMPOUND)
    abort ();
  
  if ((count = H5Tget_nmembers (tid)) < 0)
    raiseEvent (LoadError, "unable to get compound type member count");
  
  if (count == 0)
    raiseEvent (LoadError, "compound type should have at least one member");

  if ((tid_size = H5Tget_size (tid)) == 0)
    raiseEvent (LoadError,
                "unable to get compound type size");
  
  if ((class = swarm_directory_ensure_class_named (typeName)))
    *classPtr = class;
  else
    {
      Class newClass = [CreateDrop class];
      id classObj = [id_BehaviorPhase_s createBegin: aZone];
      struct objc_ivar_list *ivars =
        [aZone alloc: (sizeof (struct objc_ivar_list) +
                       (count - 1) * sizeof (struct objc_ivar))];
      struct objc_ivar *ivar_list = ivars->ivar_list;
      size_t next_offset, min_offset;

      size_t set_ivar (size_t offset, unsigned i)
        {
          size_t hoffset, noffset;
          hid_t mtid;
          const char *name;
          fcall_type_t type;
          
          if ((name = H5Tget_member_name (tid, i)) < 0)
            raiseEvent (LoadError,
                        "unable to get compound type member name #%u",
                        i);
          if ((mtid = H5Tget_member_type (tid, i)) < 0)
            raiseEvent (LoadError,
                        "unable to get compound type member type #%u",
                        i);
          type = fcall_type_for_tid (mtid);

          if (get_attribute_levels_string_list (did, name, NULL) > 0)
            {
              if (type != fcall_type_sint)
                raiseEvent (LoadError, "expecting int string index");

              type = fcall_type_string;
            }

          if ((hoffset = H5Tget_member_offset (tid, i)) < 0)
            raiseEvent (LoadError,
                        "unable to get compound type offset #%u",
                        i);

          if (H5Tclose (mtid) < 0)
            raiseEvent (LoadError, "unable to close member type");

          noffset = alignsizeto (offset,
                                 fcall_type_alignment (type));
          
          if (min_offset > 0)
            hoffset += min_offset;
          
          ivar_list[i].ivar_type = objc_type_for_fcall_type (type);
          ivar_list[i].ivar_name = name;
          ivar_list[i].ivar_offset = noffset;
          return noffset + fcall_type_size (type);
        }
      
      [classObj setName: ZSTRDUP (aZone, typeName)];
      [classObj setClass: getClass (newClass)];
      [classObj setDefiningClass: newClass];
      [classObj setSuperclass: newClass];
      
      min_offset = 0;
      min_offset = set_ivar (([Object_s class])->instance_size, 0);
      next_offset = min_offset;
      
      for (i = 0; i < count; i++)
        next_offset = set_ivar (next_offset, i);
      
      ivars->ivar_count = count;
      ((Class) classObj)->ivars = ivars;
      {
        struct objc_ivar *ivar = &ivar_list[count - 1];
        size_t size = ivar->ivar_offset +
          fcall_type_size (fcall_type_for_objc_type (*ivar->ivar_type));
        
        ((Class) classObj)->instance_size = size;
      }
      *classPtr = [classObj createEnd];
    }
}
#endif

- createEnd
{
#ifdef HAVE_HDF5
  id aZone = [self getZone];

  [super createEnd];
  
  stringMaps = [[[Map createBegin: aZone]
                  setCompareFunction: compareCStrings]
                 createEnd];
  
  if (prototype)
    tid = create_compound_type_from_prototype (prototype);
  else if (tid)
    {
      Class class;

      create_class_from_compound_type (aZone, tid, did, name, &class);
      {
#ifdef HAVE_JDK
        jclass clazz = 0;

        if (swarmDirectory)
          clazz = SD_JAVA_FIND_CLASS_JAVA (class);

        if (clazz && !java_objc_proxy_p (clazz))
          prototype = SD_JAVA_INSTANTIATE (clazz)->object;
        else
#endif
          prototype = [aZone allocIVars: class];
      }
      if (did)
        {
          void process_ivar (const char *ivarName, fcall_type_t type,
                             void *ptr, unsigned rank, unsigned *dims)
            {
              const char **strings;
              unsigned count =
                get_attribute_levels_string_list (did, ivarName, &strings);
              
              if (count > 0)
                {
                  if (type != fcall_type_string)
                    raiseEvent (LoadError,
                                "ivar `%s' has %u strings, but not a char *",
                                name, count);

                  {
                    id <Map> stringMap =
                      [[[Map createBegin: aZone]
                         setCompareFunction: compareCStrings]
                        createEnd];
                    PTRUINT i;

                    for (i = 0; i < count; i++)
                      [stringMap at: (id) strings[i] insert: (id) (i + 1)];
                    
                    [stringMaps at: (id) STRDUP (ivarName) insert: stringMap];
                  }
                }
            }
          map_object_ivars (prototype, process_ivar);
        }
    }
  else
    abort();
#else
  hdf5_not_available ();
#endif
  return self;
}

PHASE(Setting)
#ifdef HAVE_HDF5
- setDataset: (hid_t)loc_id
{
  did = loc_id;
  return self;
}
#endif

PHASE(Using)

#ifdef HAVE_HDF5
- (hid_t)getTid
{
  return tid;
}

#endif

- getPrototype
{
  return prototype;
}

#ifdef HAVE_HDF5
- (void)packObj: (void *)buf to: obj
{
  unsigned inum = 0;

  void process_ivar (const char *ivarName, fcall_type_t ivarType,
                     void *val_ptr, unsigned rank, unsigned *dims)
    {
      hid_t mtid;
      fcall_type_t type;
      size_t hoffset;
      
      if ((mtid = H5Tget_member_type (tid, inum)) < 0)
        raiseEvent (LoadError,
                    "unable to get compound type member type #%u",
                    inum);
      type = fcall_type_for_tid (mtid);

      if ((hoffset = H5Tget_member_offset (tid, inum)) < 0)
        raiseEvent (LoadError,
                    "unable to get compound type offset #%u",
                    inum);

      if (type == fcall_type_sint && ivarType == fcall_type_string
          && rank == 0)
        {
          id <Map> stringMap = [stringMaps at: (id) ivarName];
          
          if (!stringMap)
            raiseEvent (LoadError,
                        "expecting string table for int -> char * conversion");
          {
            PTRINT offset = *(int *) (buf + hoffset);
            id <MapIndex> mi = [stringMap begin: scratchZone];
            const char *key;

            if (offset > 0)
              {
                if ([mi findNext: (id) offset])
                  key = (const char *) [mi getKey];
                else
                  abort ();
              }
            else
              key = NULL;

            if ([obj respondsTo: M(isJavaProxy)])
              {
                types_t val;

                val.string = key;
                object_setVariable (obj, ivarName, &val);
              }
            else
              *(const char **) val_ptr = key;
            [mi drop];
          }
        }
      else
        {
          if (!compare_types (ivarType, type))
            raiseEvent (LoadError, "ivar `%s' in `%s': `%u' != mtid: `%u'", 
                        name, [obj name],
                        ivarType, type);
          
          if ([obj respondsTo: M(isJavaProxy)])
            object_setVariable (obj, ivarName, buf + hoffset);
          else
            {
              unsigned i;
              unsigned count = 1;
              
              for (i = 0; i < rank; i++)
                count *= dims[i];
              memcpy (val_ptr, buf + hoffset, fcall_type_size (type) * count);
            }
        }
      inum++;
    }
  map_object_ivars (obj, process_ivar);
}

- (void)packBuf: obj to: (void *)buf
{
  unsigned inum = 0;

  void process_ivar (const char *ivarName, fcall_type_t ivarType,
                     void *val_ptr, unsigned rank, unsigned *dims)
    {
      hid_t mtid;
      fcall_type_t type;
      size_t hoffset;
      
      if ((mtid = H5Tget_member_type (tid, inum)) < 0)
        raiseEvent (LoadError,
                    "unable to get compound type member type #%u",
                    inum);
      type = fcall_type_for_tid (mtid);

      if ((hoffset = H5Tget_member_offset (tid, inum)) < 0)
        raiseEvent (LoadError,
                    "unable to get compound type offset #%u",
                    inum);
      
      if (type == fcall_type_sint && ivarType == fcall_type_string
          && rank == 0)
        {
          id <Map> stringMap = [stringMaps at: (id) ivarName];
          
          if (!stringMap)
            {
              stringMap = [[[Map createBegin: [self getZone]]
                             setCompareFunction: compareCStrings]
                            createEnd];

              [stringMaps at: (id) STRDUP (ivarName) insert: stringMap];
            }
          {
            const char *key = *(const char **) val_ptr;
            int *ptr = (int *) (buf + hoffset);
            PTRUINT offset = INT_MIN;

            if (key)
              {
                offset = (PTRUINT) [stringMap at: (id) key];
                
                if (offset == 0)
                  {
                    offset = [stringMap getCount] + 1;

                    [stringMap at: (id) key insert: (id) offset];
                  }
              }
            *ptr = offset;
          }
        }
      else if (ivarType == fcall_type_object)
        {
          raiseEvent (SaveWarning, "skipping `%s'\n", ivarName);
          return;
        }
      else
        {
          if (!compare_types (ivarType, type))
            raiseEvent (SaveError,
                        "differing source and target types %d/%d (%s)\n",
                        ivarType, type, ivarName);
          memcpy (buf + hoffset,
                  val_ptr,
                  fcall_type_size (type));
        }
      inum++;
    }
  map_object_ivars (obj, process_ivar);
}

static void
hdf5_delete_attribute (hid_t loc_id, const char *name)
{
  void func ()
    {
      H5Adelete (loc_id, name);
    }
  suppress_messages (func);
}
#endif

- (void)writeLevel: (const char *)varName
{
#ifdef HAVE_HDF5
  hsize_t dims[1];
  hid_t memtid = make_string_ref_type ();
  hid_t stringtid, sid, aid;
  id <Map> stringMap = [stringMaps at: (id) varName];
  size_t count = [stringMap getCount];
  
  dims[0] = count;
  if ((sid = H5Screate_simple (1, dims, NULL)) < 0)
    raiseEvent (SaveError, "unable to create row names data space");

  if ((stringtid = H5Tcopy (H5T_C_S1)) < 0)
    raiseEvent (SaveError, "unable to copy string type");

  {
    id <MapIndex> mi = [stringMap begin: scratchZone];
    const char *key;
    size_t maxlen = 0;

    for ([mi next: (id *) &key];
         [mi getLoc] == (id) Member;
         [mi next: (id *) &key])
      {
        size_t len;

        len = strlen (key);
        if (len > maxlen)
          maxlen = len;
      }

    if ((H5Tset_size (stringtid, maxlen + 1)) < 0)
      raiseEvent (SaveError, "unable to set string size");
    [mi drop];
  }

  {
    char levelAttrName[strlen (LEVELSPREFIX) + strlen (varName) + 1];
    char *p;

    p = stpcpy (levelAttrName, LEVELSPREFIX);
    stpcpy (p, varName);

    hdf5_delete_attribute (did, levelAttrName);
    if ((aid = H5Acreate (did, levelAttrName, stringtid,
                          sid, H5P_DEFAULT)) < 0)
      raiseEvent (SaveError, "unable to create levels attribute dataset");
  }
  
  {
    const char *buf[count];
    unsigned i;
    id mi = [stringMap begin: scratchZone];
    const char *key;
    PTRINT offset;

    for (i = 0, offset = (PTRINT) [mi next: (id *) &key];
         [mi getLoc] == (id) Member;
         offset = (PTRINT) [mi next: (id *) &key], i++)
      buf[offset - 1] = key;
    
    if (H5Awrite (aid, memtid, buf) < 0)
      raiseEvent (SaveError, "unable to write levels dataset");
    [mi drop];
  }
  
  if (H5Tclose (stringtid) < 0)
    raiseEvent (SaveError, "unable to close levels string type");
  if (H5Aclose (aid) < 0)
    raiseEvent (SaveError, "unable to close levels dataset");
  if (H5Sclose (sid) < 0)
    raiseEvent (SaveError, "unable to close levels dataspace");
  if (H5Tclose (memtid) < 0)
    raiseEvent (SaveError, "unable to close levels reference type");
#else
  hdf5_not_available ();
#endif
}

- (void)writeLevels
{
  void store_level (const char *ivarName, fcall_type_t type,
                    void *ivar_ptr, unsigned rank, unsigned *dims)
    {
      if (type == fcall_type_string)
        [self writeLevel: ivarName];
    }
  map_object_ivars (prototype, store_level);
}

- (void)drop
{
#ifdef HAVE_HDF5
  if (H5Tclose (tid) < 0)
    raiseEvent (SaveError, "unable to close compound type");

  // Need to keep the keys of the sub-Maps, since they are either
  // original object state, or new object state.
  [stringMaps deleteAll];
  [stringMaps drop];
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

  obj->writeFlag = NO;
  obj->datasetFlag = NO;
#ifdef HAVE_HDF5
  obj->c_rnnlen = 0;
  obj->c_rnmlen = 0;
  obj->loc_id = 0;
#endif
  return obj;
}

- setParent: theParent
{
  parent = theParent;
  return self;
}

#ifdef HAVE_HDF5
- setId: (hid_t)theLocId
{
  loc_id = theLocId;
  return self;
}
#endif

- setWriteFlag: (BOOL)theWriteFlag
{
  writeFlag = theWriteFlag;
  return self;
}

- setDatasetFlag: (BOOL)theDatasetFlag
{
  datasetFlag = theDatasetFlag;
  return self;
}

- setCompoundType: theCompoundType
{
#ifdef HAVE_HDF5
  compoundType = theCompoundType;
  c_count = 1;
#else
  hdf5_not_available ();
#endif
  return self;
}

- setCount: (unsigned)theRecordCount
{
#ifdef HAVE_HDF5
  c_count = theRecordCount;
  c_rnnlen = 1 + (unsigned) log10 ((double) c_count);
#else
  hdf5_not_available ();
#endif
  return self;
}

#ifdef HAVE_HDF5
static herr_t
ref_string (hid_t sid, hid_t did, H5T_cdata_t *cdata,
            size_t count, size_t stride, size_t bkg_stride,
            void *buf, void *bkg,
            hid_t dset_xfer_plid)
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
            size_t count, size_t stride, size_t bkg_stride,
            void *buf, void *bkg, hid_t xfer_plid)
{
  if (cdata->command == H5T_CONV_CONV)
    {
      size_t size = H5Tget_size (sid);
      unsigned char srcbuf[size * count], *srcptr = srcbuf;
      size_t i;
      
      memcpy (srcbuf, buf, sizeof (srcbuf));
      for (i = 0; i < count;i ++)
        {
          ((const char **) buf)[i] = SSTRDUP (srcptr);
          srcptr += size;
        }
    }
  return 0;
}

static hid_t
hdf5_open_file (const char *name, unsigned flags)
{ 
  hid_t loc_id;
  void func () { loc_id = H5Fopen (name, flags, H5P_DEFAULT); }
  
  suppress_messages (func);
  return loc_id;
}

static hid_t
hdf5_open_dataset (id parent, const char *name, hid_t tid, hid_t sid)
{
  hid_t parent_loc_id = ((HDF5_c *) parent)->loc_id;
  hid_t loc_id;
  void func () { H5Gunlink (parent_loc_id, name); }
  
  suppress_messages (func);

  if ((loc_id = H5Dcreate (parent_loc_id,
                           name,
                           tid,
                           sid,
                           H5P_DEFAULT)) < 0)
    raiseEvent (SaveError, "unable to create (compound) dataset");
  return loc_id;
}

#endif

- createEnd
{
#ifdef HAVE_HDF5
  [super createEnd];

  if (writeFlag)
    {
      if (parent == nil)
        {
          if ((loc_id = hdf5_open_file (name, H5F_ACC_RDWR)) < 0)
            loc_id = H5Fcreate (name, H5F_ACC_TRUNC,
                                H5P_DEFAULT, H5P_DEFAULT);
          if (loc_id < 0)
            raiseEvent (SaveError,
                        "failed to open HDF5 file for r/w`%s'",
                        name);
        }
      else 
        {
          if (compoundType)
            {
              hsize_t dims[1];
              dims[0] = c_count;

              if ((c_sid = H5Screate_simple (1, dims, NULL)) < 0)
                raiseEvent (SaveError, "unable to create (compound) space");
              loc_id = hdf5_open_dataset (parent,
                                          name,
                                          [compoundType getTid],
                                          c_sid);
              {
                size_t size = c_count * sizeof (const char *);
                
                c_rnbuf = [getZone (self) alloc: size];
                memset (c_rnbuf, 0, size);
              }
            }
          else
            {
              hid_t parent_loc_id = ((HDF5_c *) parent)->loc_id;

              if (!datasetFlag)
                {

                  if ([parent checkName: name])
                    loc_id = H5Gopen (parent_loc_id, name);
                  else
                    loc_id = H5Gcreate (parent_loc_id, name, 0);
                  if (loc_id < 0)
                    raiseEvent (SaveError,
                                "failed to open HDF5 group `%s' r/w\n",
                                name);
                }
              else
                loc_id = parent_loc_id;
            }
        }
    }
  else
    {
      if (parent == nil)
        {
          if ((loc_id = hdf5_open_file (name, H5F_ACC_RDONLY)) < 0)
            return nil;
        }
      else
        {
          if (loc_id == 0)
            {
              void func ()
                {
                  hid_t parent_loc_id = ((HDF5_c *) parent)->loc_id;
                  
                  if (!datasetFlag)
                    loc_id = H5Gopen (parent_loc_id, name);
                  else
                    loc_id = H5Dopen (parent_loc_id, name);
                }
              suppress_messages (func);
              if (loc_id < 0)
                return nil;
            }
          if (datasetFlag)
            {
              hid_t tid;
              H5T_class_t class;

              if ((tid = H5Dget_type (loc_id)) < 0)
                raiseEvent (LoadError, "failed to get type of dataset");

              if ((class = H5Tget_class (tid)) < 0)
                raiseEvent (LoadError, "failed to get class of dataset type");

              if (class == H5T_COMPOUND)
                {
                  id aZone = [self getZone];
                  
                  compoundType = [[[HDF5CompoundType createBegin: aZone]
                                    setTid: tid]
                                   setDataset: loc_id];
                  
                  {
                    int rank;
                    hsize_t dims[1];
                    
                    if ((c_sid = H5Dget_space (loc_id)) < 0)
                      raiseEvent (LoadError,
                                  "failed to get space of dataset");
                    
                    if ((rank = H5Sget_simple_extent_ndims (c_sid)) < 0)
                      raiseEvent  (LoadError,
                                   "could not get rank of space");
                    if (rank != 1)
                      raiseEvent (LoadError, "expected rank of 1");
                  
                    if (H5Sget_simple_extent_dims (c_sid, dims, NULL) < 0)
                    raiseEvent (LoadError,
                                "could not get extent of space");
                    c_count = dims[0];
                  }
                  {
                    const char *componentTypeName =
                      get_attribute (loc_id, ATTRIB_COMPONENT_TYPE_NAME);
                    
                    if (componentTypeName)
                      [compoundType setName: componentTypeName];
                    else if (c_count > 1)
                      [compoundType setName: class_generate_name ()];
                    else
                      {
                        [compoundType
                          setName:
                            (get_attribute (loc_id, ATTRIB_TYPE_NAME)
                             ?: class_generate_name ())];
                        
                        if (c_count != 1)
                          raiseEvent (LoadError, "expecting a point space");
                      }
                    compoundType = [compoundType createEnd];
                  }
                }
            }
        }
    }
  
  if (hdf5InstanceCount == 0)
    {
      if (H5Tregister (H5T_PERS_SOFT,
                       REF2STRING_CONV,
                       H5T_STD_REF_OBJ,
                       H5T_C_S1,
                       ref_string) < 0)
        raiseEvent (SaveError, "unable to register ref->string converter");
      if (H5Tregister (H5T_PERS_SOFT,
                       STRING2REF_CONV,
                       H5T_C_S1,
                       H5T_STD_REF_OBJ,
                       string_ref) < 0)
        raiseEvent (LoadError, "unable to register string->ref converter");
    }
  hdf5InstanceCount++;
  {
    hsize_t pdims[1];
    
    pdims[0] = 1;
    if ((psid = H5Screate_simple (1, pdims, NULL)) < 0)
      raiseEvent (SaveError, "unable to create point space");
  }
#else
  hdf5_not_available ();
#endif
  return self;
}
PHASE(Setting)

- setName: (const char *)theName
{
  name = theName;
  return self;
}

- setBaseTypeObject: theBaseTypeObject
{
  baseTypeObject = theBaseTypeObject;
  return self;
}

PHASE(Using)

- (const char *)getName
{
  return name;
}

- (BOOL)getWriteFlag
{
  return writeFlag;
}

- (BOOL)getDatasetFlag
{
  return datasetFlag;
}

- (size_t)getDatasetRank
{
#ifdef HAVE_HDF5
  if (datasetFlag)
    {
      hid_t sid;
      hsize_t rank;

      if ((sid = H5Dget_space (loc_id)) < 0)
        raiseEvent (LoadError, "could not get dataset space");
      
      if ((rank = H5Sget_simple_extent_ndims (sid)) < 0)
        raiseEvent  (LoadError,
                     "could not get rank of data space");
      
      if (H5Sclose (sid) < 0)
        raiseEvent (LoadError, "could not close dataset sid");
      return rank;
    }
  else
    abort ();
#else
  hdf5_not_available ();
#endif
  return 0;
}

- (size_t)getDatasetDimension: (unsigned)dimNumber
{
#ifdef HAVE_HDF5
  if (datasetFlag)
    {
      size_t rank = [self getDatasetRank];
      hsize_t dims[rank];
      hid_t sid;

      if ((sid = H5Dget_space (loc_id)) < 0)
        raiseEvent (LoadError, "could not get dataset space");
      
      if (H5Sget_simple_extent_dims (sid, dims, NULL) < 0)
        raiseEvent (LoadError, "could not get dimensions for row names space");
    
      if (H5Sclose (sid) < 0)
        raiseEvent (LoadError, "could not close dataset sid");
      
      if (dimNumber >= rank)
        raiseEvent (InvalidArgument, 
                    "requested dimension number not less than rank");
      
      return dims[dimNumber];
    }
  else
    abort ();
#else
  hdf5_not_available  ();
#endif
  return 0;
}

- (fcall_type_t)getDatasetType
{
#ifdef HAVE_HDF5
  fcall_type_t type;
  hid_t tid;

  if ((tid = H5Dget_type (loc_id)) < 0)
    raiseEvent (LoadError, "cannot get dataset type");

  type = fcall_type_for_tid (tid);

  if (H5Tclose (tid) < 0)
    raiseEvent (LoadError, "cannot close dataset type");
  return type;
#else
  hdf5_not_available ();
  return -1;
#endif
}

- getCompoundType
{
  return compoundType;
}

- (unsigned)getCount
{
  return c_count;
}

- (BOOL)checkName: (const char *)objName
{
#ifdef HAVE_HDF5
  struct H5G_stat_t statbuf;
  BOOL result;

  void func ()
    {
      result = (H5Gget_objinfo (loc_id, objName, 1, &statbuf) >= 0);
    }
  suppress_messages (func);
  return result;
#else
  hdf5_not_available ();
  return NO;
#endif
}

- (BOOL)checkDatasetName: (const char *)datasetName
{
#ifdef HAVE_HDF5
  struct H5G_stat_t statbuf;

  if (H5Gget_objinfo (loc_id, datasetName, 1, &statbuf) < 0)
    return NO;
  else if (statbuf.type == H5G_DATASET)
    return YES;
  else
    return NO;
#else
  hdf5_not_available ();
  return NO;
#endif
}

- (void)iterate: (int (*) (id hdf5obj))iterateFunc drop: (BOOL)dropFlag
{
#ifdef HAVE_HDF5
  herr_t process_object (hid_t oid, const char *memberName, void *client)
    {
      int ret = 0;
      H5G_stat_t statbuf;
      
      if (H5Gget_objinfo (oid, memberName, 1, &statbuf) < 0)
        raiseEvent (LoadError, "Cannot query object `%s'", memberName);  
      
      if (statbuf.type == H5G_GROUP)
        {
          hid_t gid;
          id group;
          
          if ((gid = H5Gopen (oid, memberName)) < 0)
            raiseEvent (LoadError, "cannot open group `%s'", memberName);
          group = [[[[[[HDF5 createBegin: [self getZone]]
                        setParent: self]
                       setWriteFlag: NO]
                      setName: memberName]
                     setId: gid]
                    createEnd];
          ret = iterateFunc (group);
          if (dropFlag)
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
                          setWriteFlag: NO]
                         setDatasetFlag: YES]
                        setName: memberName]
                       setId: did]
                      createEnd];
          ret = iterateFunc (dataset);
          if (dropFlag)
            [dataset drop];
        }
      else
        raiseEvent (LoadError, "Cannot process HDF5 type %u",
                    (unsigned) statbuf.type);
      return ret;
    }
  if (H5Giterate (loc_id, ".", NULL, process_object, self) < 0)
    raiseEvent (LoadError, "cannot iterate over HDF5 objects");
#else
  hdf5_not_available ();
#endif  
}

- (void)iterate: (int (*) (id hdf5obj))iterateFunc
{
  [self iterate: iterateFunc drop: YES];
}


- (void)iterateAttributes: (int (*) (const char *key, const char *value))iterateFunc
{
#ifdef HAVE_HDF5

  herr_t process_attribute (hid_t oid, const char *attrName, void *client)
    {
      if (!(strcmp (attrName, ROWNAMES) == 0
            || strcmp (attrName, "names") == 0
            || strncmp (attrName, LEVELSPREFIX, strlen (LEVELSPREFIX)) == 0))
        {
          const char *value = get_attribute (oid, attrName);
          
          if (value)
            return iterateFunc (attrName, value);
        } 
      return 0;
    }
  
  if (H5Aiterate (loc_id, NULL, process_attribute, NULL) < 0)
    raiseEvent (LoadError, "unable to iterate over attributes");
  
#else
  hdf5_not_available ();
#endif
}

- (const char *)getAttribute: (const char *)attrName
{
#ifdef HAVE_HDF5
  return get_attribute (loc_id, attrName);
#else
  hdf5_not_available ();
  return NULL;
#endif
}

- getClass
{
#ifdef HAVE_HDF5
  if (datasetFlag)
    {
      if (compoundType)
        return SD_GETCLASS ([compoundType getPrototype]);
      else
        abort ();
    }
  else
    {
      const char *typeName;

      if (baseTypeObject)
        typeName = SSTRDUP ((const char *) ((void **) baseTypeObject)[2]);
      else
        typeName = get_attribute (loc_id, ATTRIB_TYPE_NAME);
      
      if (typeName)
        {
          Class class = objc_lookup_class (typeName);
 
          SFREEBLOCK (typeName);

          if (class != Nil)
            return class;
          else
            {
              id typeObject = baseTypeObject;

              int process_object (id hdf5Obj)
                {
                  if (((HDF5_c *) hdf5Obj)->datasetFlag)
                    {
                      hid_t did = ((HDF5_c *) hdf5Obj)->loc_id;
                      hid_t tid, sid;
                      unsigned rank;
                      fcall_type_t baseType;
                      
                      if ((tid = H5Dget_type (did)) < 0)
                        raiseEvent (LoadError, "could not get dataset type");
                      
                      baseType = fcall_type_for_tid (tid);
                      
                      if ((sid = H5Dget_space (did)) < 0)
                        raiseEvent (LoadError, "could not get dataset space");
                      
                      if ((rank = H5Sget_simple_extent_ndims (sid)) < 0)
                        raiseEvent  (LoadError,
                                     "could not get rank of data space");
                      {
                        unsigned udims[rank];
                        hsize_t hdims[rank];
                        unsigned i;
                        
                        if (H5Sget_simple_extent_dims (sid, hdims, NULL) < 0)
                          raiseEvent (LoadError,
                                      "could not get extent of data space");
                        for (i = 0; i < rank; i++)
                          udims[i] = hdims[i];

                        class_addVariable (typeObject,
                                           [hdf5Obj getName],
                                           baseType,
                                           rank,
                                           udims);
                      }
                      if (H5Tclose (tid) < 0)
                        raiseEvent (LoadError, "could not close dataset tid");
                      if (H5Sclose (sid) < 0)
                        raiseEvent (LoadError, "could not close dataset sid");
                    }
                  else
                    class_addVariable (typeObject,
                                       [hdf5Obj getName],
                                       fcall_type_object,
                                       0,
                                       NULL);
                  return 0;
                }
              [self iterate: process_object];
              return typeObject;
            }
        }
      else
        return Nil;
    }
#else
  hdf5_not_available ();
  return Nil;
#endif
}

- (void)assignIvar: obj
{
  const char *ivarName = [self getName];
  if ([obj respondsTo: M(isJavaProxy)])
    {
      if ([self getDatasetFlag])
        {
          unsigned rank = [self getDatasetRank];
          unsigned dims[rank], i;
          
          for (i = 0; i < rank; i++)
            dims[i] = [self getDatasetDimension: i];
          
          {
            fcall_type_t type = [self getDatasetType];
            unsigned char buf[object_getVariableElementCount (obj,
                                                              ivarName,
                                                              type,
                                                              rank,
                                                              dims) *
                             fcall_type_size (type)];
            
            [self loadDataset: buf];
            object_setVariable (obj, ivarName, buf);
          }
        }
      else
        {
          types_t buf;

          buf.object = hdf5In ([obj getZone], self);
          object_setVariable (obj, ivarName, &buf);
        }
    }
  else
    {
      void *ptr = ivar_ptr_for_name (obj, ivarName);
      
      if (ptr == NULL)
        raiseEvent (InvalidArgument,
                    "could not find ivar `%s'", ivarName);
      
      if ([self getDatasetFlag])
        [self loadDataset: ptr];
      else
        *(id *) ptr = hdf5In ([obj getZone], self);
    }
}

- (void)storeTypeName: (const char *)typeName
{
#ifdef HAVE_HDF5
  [self storeAttribute: ATTRIB_TYPE_NAME value: typeName];
#else
  hdf5_not_available ();
#endif
}

- (void)storeComponentTypeName: (const char *)typeName
{
#ifdef HAVE_HDF5
  [self storeAttribute: ATTRIB_COMPONENT_TYPE_NAME value: typeName];
#else
  hdf5_not_available ();
#endif
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

  hdf5_delete_attribute (did, attributeName);
  
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

- (void)storeAttribute: (const char *)attributeName value: (const char *)valueString
{
#ifdef HAVE_HDF5
  hdf5_store_attribute (loc_id, attributeName, valueString);
#else
  hdf5_not_available ();
#endif
}

- (void)storeAsDataset: (const char *)datasetName
              typeName: (const char *)typeName
                  type: (fcall_type_t)type
                  rank: (unsigned)rank
                  dims: (unsigned *)dims
                   ptr: (void *)ptr
{
#ifdef HAVE_HDF5
  void store (hid_t sid, hid_t memtid, hid_t tid)
    {
      hid_t did;
      
      did = hdf5_open_dataset (self, datasetName, tid, sid);
      if (H5Dwrite (did, memtid, sid, sid, H5P_DEFAULT, ptr) < 0)
        raiseEvent (SaveError, "unable to write to dataset `%s'", datasetName);

      if (typeName)
        hdf5_store_attribute (did, ATTRIB_TYPE_NAME, typeName);
      
      if (H5Dclose (did) < 0)
        raiseEvent (SaveError, "unable to close dataset `%s'", datasetName);
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

  if (rank > 0)
    {
      hssize_t hdf5dims[rank];
      unsigned i;
      hid_t sid;
      
      for (i = 0; i < rank; i++)
        hdf5dims[i] = dims[i];
      
      if ((sid = H5Screate_simple (rank, hdf5dims, NULL)) < 0)
        raiseEvent (SaveError, "unable to create array dataspace");
      
      if (type == fcall_type_string)
        store_string (sid);
      else
        {
          hid_t tid = tid_for_fcall_type (type);
          
          store (sid, tid, tid);
        }
      if (H5Sclose (sid) < 0)
        raiseEvent (SaveError, "unable to close array dataspace");
    }
  else if (type == fcall_type_string)
    store_string (psid);
  else
    {
      hid_t tid = tid_for_fcall_type (type);

      store (psid, tid, tid);
    }
#else
  hdf5_not_available ();
#endif
}

- (void)loadDataset: (void *)ptr
{
#ifdef HAVE_HDF5
  hid_t sid, tid, memtid;

  if ((sid = H5Dget_space (loc_id)) < 0)
    raiseEvent (LoadError, "cannot get dataset space");

  if ((tid = H5Dget_type (loc_id)) < 0)
    raiseEvent (LoadError, "cannot get dataset type");

  {
    H5T_class_t class;

    if ((class = H5Tget_class (tid)) < 0)
      raiseEvent (LoadError, "cannot get class of type");
    
    if (class == H5T_STRING)
      memtid = make_string_ref_type ();
    else
      memtid = tid;
  }

  if (H5Dread (loc_id, memtid, sid, sid, H5P_DEFAULT, ptr) < 0)
    raiseEvent (LoadError, "cannot read dataset");

  if (memtid != tid)
    if (H5Tclose (memtid) < 0)
      raiseEvent (LoadError, "cannot close dataset mem type");
  
  if (H5Tclose (tid) < 0)
    raiseEvent (LoadError, "cannot close dataset type");
  
  if (H5Sclose (sid) < 0)
    raiseEvent (LoadError, "cannot close dataset space");
#else
  hdf5_not_available ();
#endif
}

- (void)selectRecord: (unsigned)recordNumber
{
#ifdef HAVE_HDF5
  hssize_t coord[1][1];
  
  coord[0][0] = recordNumber;
  if (H5Sselect_elements (c_sid, H5S_SELECT_SET, 1,
                          (const hssize_t **) coord) < 0)
    raiseEvent (InvalidArgument, "unable to select record: %u", recordNumber);
#else
  hdf5_not_available ();
#endif
}

- (void)nameRecord: (unsigned)recordNumber name: (const char *)recordName
{
#ifdef HAVE_HDF5
  size_t len = strlen (recordName);

  if (len > c_rnmlen)
    c_rnmlen = len;
  c_rnbuf[recordNumber] = STRDUP (recordName);
#else
  hdf5_not_available ();
#endif
}

- (void)numberRecord: (unsigned)recordNumber
{
#ifdef HAVE_HDF5
  char fmt[2 + c_rnnlen + 1 + 1];
  char buf[c_rnnlen + 1];

  sprintf (fmt, "%%0%uu", (unsigned) c_rnnlen);
  sprintf (buf, fmt, recordNumber);
  
  c_rnbuf[recordNumber] = STRDUP (buf);
#else
  hdf5_not_available ();
#endif
}

- (void)shallowLoadObject: obj
{
#ifdef HAVE_HDF5
  if (compoundType == nil)
    raiseEvent (LoadError,
                "shallow datasets are expected to have compound type");

  {
    hid_t tid = [compoundType getTid];
    size_t tid_size;
    
    if ((tid_size = H5Tget_size (tid)) == 0)
      raiseEvent (LoadError, "unable to get compound type size");
    
    {
      unsigned char buf[tid_size];
      
      if (H5Dread (loc_id, tid,
                   psid, c_sid, H5P_DEFAULT, buf) < 0)
        raiseEvent (LoadError, "unable to load record");
      
      [compoundType packObj: buf to: obj];
    }
  }
#else
  hdf5_not_available ();
#endif
}

- (void)shallowStoreObject: obj
{
#ifdef HAVE_HDF5
    
  if (compoundType == nil)
    raiseEvent (SaveError,
                "shallow datasets are expected to have compound type");
  
  {
    hid_t tid = [compoundType getTid];
    size_t tid_size;
    
    if ((tid_size = H5Tget_size (tid)) == 0)
      raiseEvent (LoadError,
                  "unable to get compound type size");
    
    {
      unsigned char buf[tid_size];

      [compoundType packBuf: obj to: buf];
      
      if (H5Dwrite (loc_id, tid, psid, c_sid, H5P_DEFAULT, buf) < 0)
      raiseEvent (SaveError, "unable to store record");
    }
  }
#else
  hdf5_not_available ();
#endif
}

- (const char **)readRowNames
{
#ifdef HAVE_HDF5
  hid_t aid, sid;
  hid_t memtid = make_string_ref_type ();
  int rank;
  hsize_t dims[1];
  const char **buf;

  if ((aid = H5Aopen_name (loc_id, ROWNAMES)) < 0)
    raiseEvent (LoadError, "could not get row names attribute");
  
  if ((sid = H5Aget_space (aid)) < 0)
    raiseEvent (LoadError, "could not get row names space");
  
  if ((rank = H5Sget_simple_extent_ndims (sid)) < 0)
    raiseEvent (LoadError, "could not get row names space rank");
  
  if (rank != 1)
    raiseEvent (LoadError, "row names space rank should be 1");
  
  if (H5Sget_simple_extent_dims (sid, dims, NULL) < 0)
    raiseEvent (LoadError, "could not get dimensions for row names space");

  if (c_count != dims[0])
    raiseEvent (LoadError, "row names vector different size from table");

  buf = [getZone (self) alloc: sizeof (const char *) * c_count];

  if (H5Aread (aid, memtid, buf) < 0)
    raiseEvent (LoadError, "could not get row names vecotr");
  
  if (H5Aclose (aid) < 0)
    raiseEvent (LoadError, "could not close row names attribute");

  if (H5Sclose (sid) < 0)
    raiseEvent (LoadError, "could not close row names space");
  
  if (H5Tclose (memtid) < 0)
    raiseEvent (LoadError, "could not close reference type");

  return buf;
#else
  hdf5_not_available ();
  return NULL;
#endif
}

- (void)writeRowNames
{
#ifdef HAVE_HDF5
  hsize_t dims[1];
  hid_t rnmemtid = make_string_ref_type ();
  hid_t rntid, rnsid, rnaid;

  hdf5_delete_attribute (loc_id, ROWNAMES);
  dims[0] = c_count;
  if ((rnsid = H5Screate_simple (1, dims, NULL)) < 0)
    raiseEvent (SaveError, "unable to create row names data space");

  if ((rntid = H5Tcopy (H5T_C_S1)) < 0)
    raiseEvent (SaveError, "unable to copy string type");

  {
    hsize_t len = c_rnmlen ?: c_rnnlen;
    
    if ((H5Tset_size (rntid, len + 1)) < 0)
      raiseEvent (SaveError, "unable to set string size");
  }
  
  if ((rnaid = H5Acreate (loc_id, ROWNAMES,
                          rntid, rnsid, H5P_DEFAULT)) < 0)
    raiseEvent (SaveError, "unable to create row names attribute dataset");
  
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
}

- (void)writeLevels
{
#ifdef HAVE_HDF5
  [compoundType setDataset: loc_id];
  [compoundType writeLevels];
#else
  hdf5_not_available ();
#endif
}

- (void)flush
{
#ifdef HAVE_HDF5
  if (H5Fflush (loc_id, H5F_SCOPE_GLOBAL) <0)
    raiseEvent (SaveError, "Failed to flush HDF5 file");
#else
  hdf5_not_available ();
#endif
}

- (void)drop
{
#ifdef HAVE_HDF5
  if (parent == nil)
    {
      if (H5Fclose (loc_id) < 0)
        raiseEvent (SaveError, "Failed to close HDF5 file");
    }
  else
    {
      if (compoundType)
        {
          if (c_sid != psid)
            if (H5Sclose (c_sid) < 0)
              raiseEvent (InvalidArgument, "Failed to close (compound) space");

          if (H5Dclose (loc_id) < 0)
            raiseEvent (SaveError, "Failed to close (compound) dataset");
          
          if (writeFlag)
            {
              unsigned ri;

              for (ri = 0; ri < c_count; ri++)
                if (c_rnbuf[ri])
                  FREEBLOCK (c_rnbuf[ri]);
              FREEBLOCK (c_rnbuf);
            }
        }
      else
        {
          if (!datasetFlag)
            if (H5Gclose (loc_id) < 0)
              raiseEvent (SaveError, "Failed to close HDF5 group");
        }
      if (H5Sclose (psid) < 0)
        raiseEvent (SaveError, "Failed to close point space");
    }
  hdf5InstanceCount--;
  if (hdf5InstanceCount == 0)
    {
      if (H5Tunregister (H5T_PERS_SOFT, REF2STRING_CONV, -1, -1,
                         ref_string) < 0)
        raiseEvent (SaveError, "unable to unregister ref->string converter");
      if (H5Tunregister (H5T_PERS_SOFT, STRING2REF_CONV, -1, -1,
                         string_ref) < 0)
        raiseEvent (LoadError, "unable to unregister string->ref converter");
    }
  [super drop];
#else
  hdf5_not_available ();
#endif
}  

@end
