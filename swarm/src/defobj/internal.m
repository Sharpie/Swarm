// Swarm library. Copyright © 1998-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj/DefObject.h> // id_BehaviorPhase_s
#import <defobj/Create.h> // CreateDrop
#import <defobj.h> // raiseEvent, DSIZE, SSTRDUP
#import "internal.h"

#include <misc.h> // strtoul, isDigit
#include <objc/objc-api.h>
#import <collections/predicates.h>

#import <swarmconfig.h>
#ifdef HAVE_JDK
#import <defobj/directory.h> // SD_JAVA_FINDJAVA, SD_JAVA_FINDJAVACLASS
#import "java.h" // map_java_ivars, GETVALUE
#endif

#define TYPE_BOOLEAN "boolean"
#define TYPE_SHORT "short"
#define TYPE_UNSIGNED_SHORT "unsigned short"
#define TYPE_INT "int"
#define TYPE_UNSIGNED "unsigned"
#define TYPE_LONG "long"
#define TYPE_UNSIGNED_LONG "unsigned long"
#define TYPE_LONG_LONG "long long"
#define TYPE_UNSIGNED_LONG_LONG "unsigned long long"
#define TYPE_FLOAT "float"
#define TYPE_DOUBLE "double"
#define TYPE_LONG_DOUBLE "long double"
#define TYPE_STRING "string"
#define TYPE_OBJECT "object"

size_t
alignsizeto (size_t pos, size_t alignment)
{
  size_t mask = (alignment - 1);

  if ((pos & mask) == 0)
    return pos;
  else
    return (pos + alignment) & ~mask;
}

void *
alignptrto (void *ptr, size_t alignment)
{
  PTRUINT mask = (alignment - 1);
  PTRUINT pos = (PTRUINT) ptr;

  if ((pos & mask) == 0)
    return ptr;
  else
    return (void *) ((pos + alignment) & ~mask);
}

size_t
fcall_type_alignment (fcall_type_t varType)
{
  size_t alignment = 0;

  switch (varType)
    {
    case fcall_type_boolean:
      alignment = __alignof__ (BOOL);
      break;
    case fcall_type_sshort: case fcall_type_ushort:
      alignment = __alignof__ (short);
      break;
    case fcall_type_sint: case fcall_type_uint:
      alignment = __alignof__ (int);
      break;
    case fcall_type_slong: case fcall_type_ulong:
      alignment = __alignof__ (long);
      break;
    case fcall_type_slonglong: case fcall_type_ulonglong:
      alignment = __alignof__ (long long);
      break;
    case fcall_type_float:
      alignment = __alignof__ (float);
      break;
    case fcall_type_double:
      alignment = __alignof__ (double);
      break;
    case fcall_type_long_double:
      alignment = __alignof__ (long double);
      break;
    case fcall_type_schar: case fcall_type_uchar:
      alignment = __alignof__ (char);
      break;
    case fcall_type_string:
      alignment = __alignof__ (const char *);
      break;
    case fcall_type_object:
      alignment = __alignof__ (id);
      break;
    case fcall_type_class:
      alignment = __alignof__ (Class);
      break;

    default:
      abort ();
    }
  return alignment;
}

static unsigned
get_rank (const char *type)
{
  unsigned rank = 0;
  char *tail;
  
  do {
    
    errno = 0;
    strtoul (type + 1, &tail, 10);
    
    if (errno != 0)
      raiseEvent (InvalidArgument,
                  "Value out of range [%s]", type + 1);
    
    rank++;
    type = tail;
  } while (*tail == _C_ARY_B);
  return rank;
}

const char *
objc_array_subtype (const char *type, unsigned *dims)
{
  unsigned dimnum = 0;
  char *tail;
  long val;
  
  do {
    errno = 0;
    val = strtoul (type + 1, &tail, 10);
    if (dims)
      dims[dimnum] = val;
    if (errno != 0)
      raiseEvent (InvalidArgument, "Value out of range [%s]", type + 1);
    dimnum++;
    type = tail;
  } while (*tail == _C_ARY_B);
  return tail;
}

void
process_array (unsigned rank,
               unsigned *dims,
               fcall_type_t baseType,
               void (*start_dim) (unsigned dimnum),
               void (*end_dim) (void),
               void (*start_element) (void),
               void (*end_element) (void),
               void (*output_type) (fcall_type_t type,
                                    unsigned offset,
                                    void *data),
               const void *ptr,
               void *data)
{
  unsigned coord[rank];
      
  void permute (unsigned dim)
    {
      unsigned i;
      
      if (dim < rank)
        {
          if (start_dim)
            start_dim (dim);
          for (i = 0; i < dims[dim]; i++)
            {
              coord[dim] = i;
              permute (dim + 1);
            }
          if (end_dim)
            end_dim ();
        }
      else
        {
          unsigned offset = 0;
          unsigned mult = 1;
          
          offset = coord[rank - 1];
          for (i = rank - 1; i > 0; i--)
            {
              mult *= dims[i];
              offset += coord[i - 1] * mult;
            }
          if (start_element)
            start_element ();
          if (output_type)
            output_type (baseType, offset, NULL);
          if (end_element)
            end_element ();
        }
    }
  permute (0);
}

void
objc_process_array (const char *type,
                    void (*setup_array) (unsigned rank, unsigned *dims,
                                         fcall_type_t type),
                    void (*start_dim) (unsigned dimnum),
                    void (*end_dim) (void),
                    void (*start_element) (void),
                    void (*end_element) (void),
                    void (*output_type) (fcall_type_t type,
                                         unsigned offset,
                                         void *data),
                    const void *ptr,
                    void *data)
{
  unsigned rank = get_rank (type);
  unsigned dims[rank];
  fcall_type_t baseType;
  char objcArraySubtype = *objc_array_subtype (type, dims);

  baseType = fcall_type_for_objc_type (objcArraySubtype);
  
  if (setup_array)
    setup_array (rank, dims, baseType);
  
  process_array (rank, dims, baseType,
                 start_dim, end_dim,
                 start_element, end_element,
                 output_type,
                 ptr,
                 data);
}

static void
map_objc_ivars (id obj,
                void (*process_object) (const char *name,
                                        fcall_type_t type,
                                        void *ptr,
                                        unsigned rank,
                                        unsigned *dims))
{
  void map_class_ivars (Class class)
    {
      struct objc_ivar_list *ivars = class->ivars;
  
      if (class->super_class)
        {
          if (strcmp (class->super_class->name, "Object_s") != 0)
            map_class_ivars (class->super_class);
        }
  
      if (ivars)
        {
          unsigned i, ivar_count = ivars->ivar_count;
          struct objc_ivar *ivar_list = ivars->ivar_list;
          
          for (i = 0; i < ivar_count; i++)
            {
              // Special case to allow member_t for setIndexFromMemberLoc:
              // lists.
              if (strcmp (ivar_list[i].ivar_type,
                          "{?=\"memberData\"[2^v]}") == 0)
                continue;
              else if (*ivar_list[i].ivar_type == _C_PTR)
                continue;
              else if (*ivar_list[i].ivar_type == _C_ARY_B)
                {
                  unsigned rank = get_rank (ivar_list[i].ivar_type);
                  unsigned dims[rank];
                  const char *baseType;
                  
                  baseType = objc_array_subtype (ivar_list[i].ivar_type, dims);
                  process_object (ivar_list[i].ivar_name,
                                  fcall_type_for_objc_type (*baseType),
                                  ivar_list[i].ivar_offset + (void *) obj,
                                  rank,
                                  dims);
                }
              else
                process_object (ivar_list[i].ivar_name,
                                fcall_type_for_objc_type (*ivar_list[i].ivar_type),
                                ivar_list[i].ivar_offset + (void *) obj,
                                0,
                                NULL);
            }
        }
    }
  map_class_ivars (getClass (obj));
}

void
map_object_ivars (id object,
                  void (*process_object) (const char *name,
                                          fcall_type_t type,
                                          void *ptr,
                                          unsigned rank,
                                          unsigned *dims))
{
#ifdef HAVE_JDK
  if ([object respondsTo: M(isJavaProxy)])
    map_java_ivars (SD_JAVA_FINDJAVA (object), process_object);
  else
#endif
    map_objc_ivars (object, process_object);
}

struct objc_ivar *
find_ivar (Class class, const char *name)
{
  struct objc_ivar_list *ivars = class->ivars;

  if (class->super_class)
    {
      if (strcmp (class->super_class->name, "Object_s") != 0)
        {
          struct objc_ivar *ret = find_ivar (class->super_class, name);
          
          if (ret)
            return ret;
        }
    }
  
  if (ivars)
    {
      unsigned i, ivar_count = ivars->ivar_count;
      struct objc_ivar *ivar_list = ivars->ivar_list;

      for (i = 0; i < ivar_count; i++)
        {
          if (strcmp (ivar_list[i].ivar_name, name) == 0)
            return &ivars->ivar_list[i];
        }
      return NULL;
    }
  return NULL;
}

void *
ivar_ptr_for_name (id obj, const char *name)
{
  struct objc_ivar *ivar = find_ivar (getClass (obj), name);

  if (ivar)
    return (void *) obj + ivar->ivar_offset;
  return NULL;
}

#if 0
const char *
objc_type_for_array (const char *baseType, unsigned rank, unsigned *dims)
{
  unsigned i;
  char nbuf[DSIZE (unsigned) + 1];
  char buf[rank * (2 + DSIZE (unsigned)) + strlen (baseType) + 1], *p = buf;

  for (i = 0; i < rank ; i++)
    {
      *p++ = '[';
      sprintf (nbuf, "%u", dims[i]);
      p = stpcpy (p, nbuf);
    }
  p = stpcpy (p, baseType);
  for (i = 0; i < rank ; i++)
    *p++ = ']';
  *p = '\0';
  return SSTRDUP (buf);
}
#endif

void
lisp_process_array (unsigned rank, unsigned *dims, fcall_type_t type,
                    const void *ptr, void *data,
                    id <OutputStream> stream,
                    BOOL deepFlag)
{
  BOOL firstElement;
  
  void lisp_start_dim (unsigned dim)
    {
      [stream catStartExpr];
      firstElement = YES;
    }
  void lisp_end_dim (void)
    {
      [stream catEndExpr];
    }
  void lisp_start_element (void)
    {
      if (!firstElement)
        [stream catSeparator];
    }
  void lisp_end_element (void)
    {
      firstElement = NO;
    }
  void lisp_array_output_type (fcall_type_t type,
                               unsigned offset,
                               void *data)
    {
      lisp_output_type (type, ptr, offset, data, stream, deepFlag);
    }
  
  [stream catArrayRank: rank];
  process_array (rank, dims, type,
                 lisp_start_dim,
                 lisp_end_dim,
                 lisp_start_element,
                 lisp_end_element,
                 lisp_array_output_type,
                 ptr,
                 data);
  [stream catEndArray];
}

void
lisp_output_type (fcall_type_t type,
                  const void *ptr,
                  unsigned offset,
                  void *data,
                  id <OutputStream> stream,
                  BOOL deepFlag)
{
  switch (type)
    {
    case fcall_type_object:
      {
        id obj = ((id *) ptr)[offset];

        if (obj == nil || !deepFlag)
          [stream catBoolean: NO];
        else
          [obj lispOutDeep: stream];
        break;
      }
    case fcall_type_class:
      [stream catClass: *(Class *) ptr];
      break;
    case fcall_type_selector:
      raiseEvent (NotImplemented, "Selectors not supported");
      break;
    case fcall_type_boolean:
      [stream catBoolean: ((BOOL *) ptr)[offset]];
      break;
    case fcall_type_schar:
    case fcall_type_uchar:
      [stream catChar: ((unsigned char *) ptr)[offset]];
      break;
    case fcall_type_sshort:
      [stream catShort: ((short *) ptr)[offset]];
      break;
    case fcall_type_ushort:
      [stream catUnsignedShort: ((unsigned short *) ptr)[offset]];
      break;
    case fcall_type_sint:
      [stream catInt: ((int *) ptr)[offset]];
      break;
    case fcall_type_uint:
      [stream catUnsigned: ((unsigned *) ptr)[offset]];
      break;
    case fcall_type_slong:
      [stream catLong: ((long *) ptr)[offset]];
      break;
    case fcall_type_ulong:
      [stream catUnsignedLong: ((unsigned long *) ptr)[offset]];
      break;
    case fcall_type_slonglong:
      [stream catLongLong: ((long long *) ptr)[offset]];
      break;
    case fcall_type_ulonglong:
      [stream catUnsignedLongLong: ((unsigned long long *) ptr)[offset]];
      break;
    case fcall_type_float:
      [stream catFloat:((float *) ptr)[offset]];
      break;
    case fcall_type_double:
      [stream catDouble: ((double *) ptr)[offset]];
      break;
    case fcall_type_long_double:
      [stream catLongDouble: ((long double *) ptr)[offset]];
      break;
    case fcall_type_void:
      abort ();
      break;
    case fcall_type_string:
      [stream catString: ((const char **) ptr)[offset]];
      break;
    default:
      abort ();
      break;
    }
}

  
fcall_type_t
fcall_type_for_lisp_type (const char *lispTypeString)
{
  if (strcmp (lispTypeString, TYPE_BOOLEAN) == 0)
    return fcall_type_boolean;
  else if (strcmp (lispTypeString, TYPE_SHORT) == 0)
    return fcall_type_sshort;
  else if (strcmp (lispTypeString, TYPE_UNSIGNED_SHORT) == 0)
    return fcall_type_ushort;
  else if (strcmp (lispTypeString, TYPE_INT) == 0)
    return fcall_type_sint;
  else if (strcmp (lispTypeString, TYPE_UNSIGNED) == 0)
    return fcall_type_uint;
  else if (strcmp (lispTypeString, TYPE_LONG) == 0)
    return fcall_type_slong;
  else if (strcmp (lispTypeString, TYPE_UNSIGNED_LONG) == 0)
    return fcall_type_ulong;
  else if (strcmp (lispTypeString, TYPE_LONG_LONG) == 0)
    return fcall_type_slonglong;
  else if (strcmp (lispTypeString, TYPE_UNSIGNED_LONG_LONG) == 0)
    return fcall_type_ulonglong;
  else if (strcmp (lispTypeString, TYPE_FLOAT) == 0)
    return fcall_type_float;
  else if (strcmp (lispTypeString, TYPE_DOUBLE) == 0)
    return fcall_type_double;
  else if (strcmp (lispTypeString, TYPE_LONG_DOUBLE) == 0)
    return fcall_type_long_double;
  else if (strcmp (lispTypeString, TYPE_STRING) == 0)
    return fcall_type_string;
  else if (strcmp (lispTypeString, TYPE_OBJECT) == 0)
    return fcall_type_object;
  else
    abort ();
}

const char *
lisp_type_for_objc_type (const char *varType,
                         void (*func) (unsigned dim, unsigned count))
{
  const char *baseType;
  unsigned dimnum;

  void expand_type (const char *type)
    {
      switch (*type)
        {
        case _C_SHT:
          baseType = TYPE_SHORT;
          break;
        case _C_USHT:
          baseType = TYPE_UNSIGNED_SHORT;
          break;
        case _C_INT:
          baseType = TYPE_INT;
          break;
        case _C_UINT:
          baseType = TYPE_UNSIGNED;
          break;
        case _C_LNG:
          baseType = TYPE_LONG;
          break;
        case _C_ULNG:
          baseType = TYPE_UNSIGNED_LONG;
          break;
        case _C_LNG_LNG:
          baseType = TYPE_LONG_LONG;
          break;
        case _C_ULNG_LNG:
          baseType = TYPE_UNSIGNED_LONG_LONG;
          break;
        case _C_FLT:
          baseType = TYPE_FLOAT;
          break;
        case _C_DBL:
          baseType = TYPE_DOUBLE;
          break;
        case _C_LNG_DBL:
          baseType = TYPE_LONG_DOUBLE;
          break;
        case _C_CHARPTR:
          baseType = TYPE_STRING;
          break;
        case _C_ID:
          baseType = TYPE_OBJECT;
          break;
        case _C_ARY_B:
          type++;
          {
            char *tail;
            unsigned count = strtoul (type, &tail, 10);
            
            if (func)
              func (dimnum, count);
            dimnum++;
            expand_type (tail);
          }
          break;
        default:
          abort ();
        }
    }
  dimnum = 0;
  expand_type (varType);
  return baseType;
}

#if ((__GNUC__ == 2) && (__GNUC_MINOR__ >= 8)) || (__GNUC__ > 2)
id
nil_method (id receiver, SEL op, ...)
{
  raiseEvent (InvalidArgument,  "The message `%s' was sent to nil.\n",
              sel_get_name (op));
  return nil;
}
#endif

char *
zstrdup (id aZone, const char *str)
{
  size_t len = strlen (str); 
  char *ptr = [(aZone) alloc: len + 1];

  strcpy (ptr, str); 
  return ptr;
}

static char objc_types[FCALL_TYPE_COUNT] = {
  _C_VOID,
  _C_UCHR, /* boolean */
  _C_UCHR,
  _C_CHR,
  _C_USHT,
  _C_SHT,
  _C_UINT,
  _C_INT,
  _C_ULNG,
  _C_LNG,
  _C_ULNG_LNG,
  _C_LNG_LNG,
  _C_FLT,
  _C_DBL,
  _C_LNG_DBL,
  _C_ID,
  _C_CLASS,
  _C_CHARPTR,
  _C_SEL,
  '\001',
  '\002'
};

size_t
fcall_type_size (fcall_type_t type)
{
  switch (type)
    {
    case fcall_type_void:
      return 0;
    case fcall_type_boolean:
      return sizeof (BOOL);
    case fcall_type_uchar:
      return sizeof (unsigned char);
    case fcall_type_schar:
      return sizeof (char);
    case fcall_type_ushort:
      return sizeof (unsigned short);
    case fcall_type_sshort:
      return sizeof (short);
    case fcall_type_uint:
      return sizeof (unsigned);
    case fcall_type_sint:
      return sizeof (int);
    case fcall_type_ulong:
      return sizeof (unsigned long);
    case fcall_type_slong:
      return sizeof (long);
    case fcall_type_ulonglong:
      return sizeof (unsigned long long);
    case fcall_type_slonglong:
      return sizeof (long long);
    case fcall_type_float:
      return sizeof (float);
    case fcall_type_double:
      return sizeof (double);
    case fcall_type_long_double:
      return sizeof (long double);
    case fcall_type_object:
      return sizeof (id);
    case fcall_type_class:
      return sizeof (Class);
    case fcall_type_string:
      return sizeof (const char *);
    case fcall_type_selector:
      return sizeof (SEL);
#ifdef HAVE_JDK
    case fcall_type_jobject:
      return sizeof (jobject);
    case fcall_type_jstring:
      return sizeof (jstring);
#endif
    default:
      abort ();
    }
}

fcall_type_t
fcall_type_for_objc_type (char objcType)
{
  unsigned i;
  
  for (i = 0; i < FCALL_TYPE_COUNT; i++)
    if (objcType == objc_types[i])
      return (fcall_type_t) i;
  raiseEvent (InvalidArgument, "Could not find objc type `%c'\n", objcType);
  return (fcall_type_t) 0;
}

const char *
objc_type_for_fcall_type (fcall_type_t type)
{
  char *objctype = [globalZone alloc: 2];
  
  objctype[0] = objc_types[type];
  objctype[1] = '\0';
  return objctype;
}

struct objc_ivar_list *
ivar_extend_list (struct objc_ivar_list *ivars, unsigned additional)
{
  unsigned existing = ivars ? ivars->ivar_count : 0;
  unsigned count = existing + additional;
  struct objc_ivar_list *newivars;
  size_t size = sizeof (struct objc_ivar_list) + 
    (count - 1) * sizeof (struct objc_ivar);
  
  if (additional == 0)
    newivars = xmalloc (size);
  else
    newivars = xrealloc (ivars, size);

  if (existing > 0)
    memcpy (newivars->ivar_list, ivars->ivar_list, 
            existing * sizeof (struct objc_ivar));
  newivars->ivar_count = existing;
  return newivars;
}

Class 
class_copy (Class class)
{
  size_t classSize = sizeof (struct objc_class);
  Class newClass = xmalloc (classSize);
  
  memcpy (newClass, class, classSize);
  newClass->ivars = ivar_extend_list (newClass->ivars, 0);
  return newClass;
}

id
type_create (id aZone, const char *typeName)
{
  Class newClass = [CreateDrop class];
  id typeObject = [id_BehaviorPhase_s createBegin: aZone];

  [typeObject setName: ZSTRDUP (aZone, typeName)];
  [typeObject setClass: getClass (newClass)];
  [typeObject setDefiningClass: newClass]; 
  [typeObject setSuperclass: newClass];

  return typeObject;
}

void
class_addVariable (Class class, const char *varName, fcall_type_t varType,
             unsigned rank, unsigned *dims)
{
  struct objc_ivar *il;
  
  class->ivars = ivar_extend_list (class->ivars, 1);
  il = &class->ivars->ivar_list[class->ivars->ivar_count];
  
  il->ivar_offset = alignsizeto (class->instance_size,
                                 fcall_type_alignment (varType));
  il->ivar_type = objc_type_for_fcall_type (varType);
  il->ivar_name = SSTRDUP (varName);
  class->instance_size = il->ivar_offset + fcall_type_size (varType);
  class->ivars->ivar_count++; 
}

static unsigned generatedClassNameCount = 0;

const char *
class_generate_name (void)
{
  char buf[5 + DSIZE (unsigned) + 1];

  sprintf (buf, "Class%u", generatedClassNameCount);
  generatedClassNameCount++;

  return SSTRDUP (buf);
}

static fcall_type_t
object_ivar_type (id obj, const char *ivarName, BOOL *isArrayPtr)
{
#ifdef HAVE_JDK
  if ([obj respondsTo: M(isJavaProxy)])
    return java_object_ivar_type (SD_JAVA_FINDJAVA (obj), ivarName, isArrayPtr);
  else
#endif
    {
      struct objc_ivar *ivar = find_ivar (getClass (obj), ivarName);
      
      if (*ivar->ivar_type == _C_ARY_B)
        {
          unsigned rank = get_rank (ivar->ivar_type);
          unsigned dims[rank];
          const char *baseType = objc_array_subtype (ivar->ivar_type, dims);
          
          if (isArrayPtr)
            *isArrayPtr = YES;

          return fcall_type_for_objc_type (*baseType);
        }
      else
        {
          if (isArrayPtr)
            *isArrayPtr = NO;
          return fcall_type_for_objc_type (*ivar->ivar_type);
        }
    }
}

void
object_setVariable (id obj, const char *ivarName, void *inbuf)
{
#ifdef HAVE_JDK
  if ([obj respondsTo: M(isJavaProxy)])
    java_object_setVariable (SD_JAVA_FINDJAVA (obj), ivarName, inbuf);
  else
#endif
    {
      struct objc_ivar *ivar = find_ivar (getClass (obj), ivarName);
      void *ptr = (void *) obj + ivar->ivar_offset;
      unsigned count = 1;
      fcall_type_t type;
      
      if (ivar == NULL)
        raiseEvent (InvalidArgument, "could not find ivar `%s'", ivarName);
      
      if (*ivar->ivar_type == _C_ARY_B)
        {
          unsigned rank = get_rank (ivar->ivar_type);
          unsigned dims[rank];
          const char *baseType = objc_array_subtype (ivar->ivar_type, dims);
          unsigned i;
          
          type = fcall_type_for_objc_type (*baseType);
          for (i = 0; i < rank; i++)
            count *= dims[i];
        }
      else if (*ivar->ivar_type == _C_PTR)
        return;
      else
        type = fcall_type_for_objc_type (*ivar->ivar_type);
      
      memcpy (ptr, inbuf, count * fcall_type_size (type)); 
    }
}

unsigned
object_getVariableElementCount (id obj,
                                const char *ivarName,
                                fcall_type_t itype,
                                unsigned irank,
                                unsigned *idims)
{
#ifdef HAVE_JDK
  if ([obj respondsTo: M(isJavaProxy)])
    return java_object_getVariableElementCount (SD_JAVA_FINDJAVA (obj),
                                                ivarName,
                                                itype,
                                                irank,
                                                idims);
  else
#endif
    abort ();
}

void
object_setVariableFromExpr (id obj, const char *ivarName, id expr)
{
   types_t buf;
  
  if (quotedp (expr))
     expr = [expr getQuotedObject];
  
  if (arrayp (expr))
    {
      if ([obj respondsTo: M(isJavaProxy)])
        {
          fcall_type_t type = [expr getArrayType];
          unsigned count = object_getVariableElementCount (obj,
                                                           ivarName,
                                                           type,
                                                           [expr getRank],
                                                           [expr getDims]);
          {
            unsigned char buf[fcall_type_size (type) * count];

            [expr convertToType: type dest: buf];
            object_setVariable (obj, ivarName, buf);
          }
        }
      else
        {
          struct objc_ivar *ivar = find_ivar (getClass (obj), ivarName);
          void *ptr = (void *) obj + ivar->ivar_offset;
          const char *atype = ivar->ivar_type;
          
          while (isDigit (*atype) || *atype == _C_ARY_B)
            atype++;
          [expr convertToType: fcall_type_for_objc_type (*atype) dest: ptr];
        }
    }
  else if (valuep (expr))
    {
      fcall_type_t ntype = [expr getValueType];
      
      switch (ntype)
        {
        case fcall_type_object:
          buf.object = [expr getObject];
          break;
        case fcall_type_class:
          buf.class = [expr getClass];
          break;
        case fcall_type_long_double:
          buf._long_double = [expr getLongDouble];
          break;
        case fcall_type_double:
          buf._double = [expr getDouble];
          break;
        case fcall_type_float:
          buf._float = [expr getFloat];
          break;
        case fcall_type_boolean:
          buf.boolean = [expr getBoolean];
          break;
        case fcall_type_slonglong:
          {
            long long val = [expr getLongLong];
            fcall_type_t type = object_ivar_type (obj, ivarName, NULL);
            
            switch (type)
              {
              case fcall_type_sint:
                buf.sint = (int) val;
                break;
              case fcall_type_uint:
                buf.uint = (unsigned) val;
                break;
              case fcall_type_sshort:
                buf.sshort = (short) val;
                break;
              case fcall_type_ushort:
                buf.ushort = (unsigned short) val;
                break;
              case fcall_type_slong:
                buf.slong = (long) val;
                break;
              case fcall_type_ulong:
                buf.ulong = (unsigned long) val;
                break;
              case fcall_type_slonglong:
                buf.slonglong = val;
                break;
              case fcall_type_ulonglong:
                buf.ulonglong = (unsigned long long) val;
                break;
              default:
                abort ();
              }
          }
        case fcall_type_schar:
          buf.schar = [expr getChar];
          break;
        default:
          raiseEvent (InvalidArgument, "Unknown value type %d", ntype);
          break;
        }
      object_setVariable (obj, ivarName, &buf);
    }
  else if (stringp (expr))
    {
      fcall_type_t type = object_ivar_type (obj, ivarName, NULL);
      buf.string = OSTRDUP (obj, [expr getC]);
      if (!(type == fcall_type_string || type == fcall_type_jstring))
        raiseEvent(InvalidArgument, "`%s' is not a string", ivarName);
      object_setVariable (obj, ivarName, &buf);
    }
  else if (archiver_list_p (expr))
    {
      id first = [expr getFirst];
      
      if (stringp (first))
        {
          const char *funcName = [first getC];
          
          if (strcmp (funcName, MAKE_INSTANCE_FUNCTION_NAME) == 0)
            buf.object = lispIn ([obj getZone], expr);
          else if (strcmp (funcName, MAKE_CLASS_FUNCTION_NAME) == 0)
            buf.class = lispIn ([obj getZone], expr);
          else if (strcmp (funcName, PARSE_FUNCTION_NAME) != 0)
            raiseEvent (InvalidArgument, "function not %s",
                        MAKE_INSTANCE_FUNCTION_NAME
                        " or "
                        MAKE_CLASS_FUNCTION_NAME);
          object_setVariable (obj, ivarName, &buf);
        }
      else
        raiseEvent (InvalidArgument, "argument not a string");
    }
  else
    raiseEvent (InvalidArgument, "Unknown type `%s'", [expr name]);
}

