// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         FArguments.m
Description:  used for packing arguments to a foreign call
Library:      defobj
*/

#import <defobj/FArguments.h>
#import <objc/objc-api.h>
#import <defalloc.h>
#include <misc.h> // stpcpy
#include <swarmconfig.h> // HAVE_JDK

#ifndef USE_AVCALL
#undef PACKAGE
#undef VERSION
#include <ffi.h>
#undef PACKAGE
#undef VERSION
#else
#include <avcall.h>
#endif

#ifdef HAVE_JDK
#include <directory.h> // jniEnv
#endif

const char *java_type_signature[FCALL_TYPE_COUNT] = {
  "V", "C", "C", "S", "S",
  "I", "I", "J", "J", 

  /* bogus */
  "J", "J",

  "F", "D",
  "X",
  "Ljava/lang/String;", 
  "Lswarm/Selector;",
  "Ljava/lang/Object;",
  "Ljava/lang/String;"
};

char objc_types[FCALL_TYPE_COUNT] = {
  _C_VOID,
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
  _C_ID,
  _C_CHARPTR,
  _C_SEL,
  '\001',
  '\002'
};

#define ALLOCTYPE(type) [[self getZone] allocBlock: fcall_type_size (type)]

static size_t
fcall_type_size (fcall_type_t type)
{
  switch (type)
    {
    case fcall_type_void:
      return 0;
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
    case fcall_type_object:
      return sizeof (id);
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

@implementation FArguments_c
PHASE(Creating)

+ createBegin: aZone
{
  FArguments_c *newArguments;

  newArguments = [aZone allocIVars: self];
  newArguments->assignedArgumentCount = 0;
  newArguments->hiddenArgumentCount = 0;
  newArguments->argTypes = [aZone allocBlock:
                                    (sizeof (fcall_type_t) * 
                                     (MAX_ARGS + MAX_HIDDEN))];
  newArguments->argValues = [aZone allocBlock: (sizeof (void *) * 
                                                (MAX_ARGS + MAX_HIDDEN))];
#ifndef USE_AVCALL
  newArguments->ffiArgTypes = [aZone allocBlock: (sizeof (ffi_type *) * 
				       (MAX_ARGS + MAX_HIDDEN))];
  newArguments->ffiReturnType = &ffi_type_void;
#endif
  newArguments->returnType = 0;
  newArguments->result = NULL;
  newArguments->javaSignatureLength = 0;
  return newArguments;
}

- setJavaFlag: (BOOL)theJavaFlag
{
  javaFlag = theJavaFlag;
  return self;
}

- _addArgument_: (void *)value ofType: (fcall_type_t)type
{
  size_t size = 0;
  unsigned offset = MAX_HIDDEN + assignedArgumentCount;
#ifdef HAVE_JDK
  jstring string;
#endif

  if (assignedArgumentCount == MAX_ARGS)
    raiseEvent (SourceMessage,
                "Types already assigned to maximum number arguments in the call!\n");

#ifdef HAVE_JDK
  if (javaFlag)
    {
      if (type == fcall_type_object)
        type = fcall_type_jobject;
      else if (type == fcall_type_string)
        {
          const char *str = *(const char **) value;
          
          string = (*jniEnv)->NewStringUTF (jniEnv, str);
          size = sizeof (jstring);
          value = &string;
          type = fcall_type_jstring;
        }
      else
        size = fcall_type_size (type);
    }
  else
#endif
    size = fcall_type_size (type);
  argTypes[offset] = type;
  argValues[offset] = ALLOCTYPE (type);
  memcpy (argValues[offset], value, size);
  javaSignatureLength += strlen (java_type_signature[type]);
  assignedArgumentCount++;

  return self;
}

static unsigned
get_fcall_type_for_objc_type (char objcType)
{
  unsigned i;
  
  for (i = 0; i < FCALL_TYPE_COUNT; i++)
    if (objcType == objc_types[i])
      return i;
  abort ();
}


- addArgument: (void *)value ofObjCType: (char)objcType
{
  return [self _addArgument_: value
               ofType: get_fcall_type_for_objc_type (objcType)];
}

#define ADD_PRIMITIVE(fcall_type, type, value)  { javaSignatureLength += strlen (java_type_signature[fcall_type]); argValues[MAX_HIDDEN + assignedArgumentCount] = ALLOCTYPE (fcall_type); argTypes[MAX_HIDDEN + assignedArgumentCount] = fcall_type; *(type *) argValues[MAX_HIDDEN + assignedArgumentCount] = value; assignedArgumentCount++; }

- addChar: (char)value
{
  ADD_PRIMITIVE (fcall_type_schar, char, value);
  return self;
}

- addUnsignedChar: (unsigned char)value
{
  ADD_PRIMITIVE (fcall_type_uchar, unsigned char, value);
  return self;
}

- addShort: (short)value 
{
  ADD_PRIMITIVE (fcall_type_sshort, short, value);
  return self;
}

- addUnsignedShort: (unsigned short)value 
{
  ADD_PRIMITIVE (fcall_type_ushort, unsigned short, value);
  return self;
}

- addInt: (int)value
{
  ADD_PRIMITIVE (fcall_type_sint, int, value);
  return self;
}

- addUnsigned: (unsigned)value
{
  ADD_PRIMITIVE (fcall_type_uint, unsigned, value);
  return self;
}

- addLong: (long)value
{
  ADD_PRIMITIVE (fcall_type_slong, long, value);
  return self;
}

- addUnsignedLong: (unsigned long)value
{
  ADD_PRIMITIVE (fcall_type_ulong, unsigned long, value);
  return self;
}

- addLongLong: (long long)value
{
  ADD_PRIMITIVE (fcall_type_slonglong, long long, value);
  return self;
}

- addUnsignedLongLong: (unsigned long long)value
{
  ADD_PRIMITIVE (fcall_type_ulonglong, unsigned long long, value);
  return self;
}

- addFloat: (float)value
{
  ADD_PRIMITIVE (fcall_type_float, float, value); 
  return self;
}

- addDouble: (double)value
{
  ADD_PRIMITIVE (fcall_type_double, double, value);
  return self;
}

- addString: (const char *)value
{
  ADD_PRIMITIVE (fcall_type_string, const char *, value);
  return self;
}

- addObject: value
{
  ADD_PRIMITIVE (fcall_type_object, id, value);
  return self;
}

- _setReturnType_: (fcall_type_t)type
{
  javaSignatureLength += strlen (java_type_signature[type]);

  if (javaFlag)
    {
      if (type == fcall_type_object)
        type = fcall_type_jobject;
      else if (type == fcall_type_string)
        type = fcall_type_jstring;
    }
  
  switch (type)
    {
    case fcall_type_uchar:
      result = &resultVal.uchar;
      break;
    case fcall_type_schar:
      result = &resultVal.schar;
      break;
    case fcall_type_ushort:
      result = &resultVal.ushort;
      break;
    case fcall_type_sshort:
      result = &resultVal.sshort;
      break;
    case fcall_type_uint:
      result = &resultVal.uint;
      break;
    case fcall_type_sint:
      result = &resultVal.sint;
      break;
    case fcall_type_ulong:
      result = &resultVal.ulong;
      break;
    case fcall_type_slong:
      result = &resultVal.slong;
      break;
    case fcall_type_ulonglong:
      result = &resultVal.ulonglong;
      break;
    case fcall_type_slonglong:
      result = &resultVal.slonglong;
      break;
    case fcall_type_float:
      result = &resultVal._float;
      break;
    case fcall_type_double:
      result = &resultVal._double;
      break;
    case fcall_type_object:
      result = &resultVal.object;
      break;
    case fcall_type_string:
      result = &resultVal.string;
      break;
    case fcall_type_selector:
      result = &resultVal.selector;
      break;
    case fcall_type_void:
      result = NULL;
      break;
    case fcall_type_jobject:
      result = &resultVal.object;
      break;
    case fcall_type_jstring:
      result = &resultVal.object;
      break;
    default:
      abort ();
    }
  returnType = type;
  return self;
}

- setObjCReturnType: (char)objcType
{
  return [self _setReturnType_: get_fcall_type_for_objc_type (objcType)];
}

static const char *
createJavaSignature (FArguments_c *self)
{
  unsigned i;
  char *str, *p;

  str = [[self getZone] allocBlock: self->javaSignatureLength + 3];
  p = stpcpy (str, "(");
  for (i = 0; i < self->assignedArgumentCount; i++)
    p = stpcpy (p, java_type_signature [self->argTypes[i + MAX_HIDDEN]]);
  
  p = stpcpy (p, ")");
  p = stpcpy (p, java_type_signature [self->returnType]);
  return str;
}

- createEnd
{
  [super createEnd];
  setMappedAlloc (self);
  javaSignature = createJavaSignature ((FArguments_c *) self);
  return self;
}

PHASE(Using)

- (void *)getResult
{
  return result;
}

- (void)mapAllocations: (mapalloc_t)mapalloc
{
  unsigned i;
#define MAX_TOTAL (MAX_HIDDEN + MAX_ARGS)

  if (!includeBlocks (mapalloc))
    return;

  for (i = 0; i < assignedArgumentCount; i++)
    {
      unsigned offset = i + MAX_HIDDEN;
      fcall_type_t type = argTypes[offset];

#ifdef HAVE_JDK      
      if (javaFlag && type == fcall_type_string)
        mapalloc->size = sizeof (jstring);
      else
#endif
        mapalloc->size = fcall_type_size (type);
      
      mapAlloc (mapalloc, argValues[offset]);
    }

  mapalloc->size = sizeof (void *) * MAX_TOTAL;
  mapAlloc (mapalloc, argValues);
  
  mapalloc->size = sizeof (fcall_type_t) * MAX_TOTAL;
  mapAlloc (mapalloc, argTypes);

#ifndef USE_AVCALL  
  mapalloc->size = sizeof (ffi_type *) * MAX_TOTAL;
  mapAlloc (mapalloc, ffiArgTypes);
#endif
  
  mapalloc->size = javaSignatureLength + 3;
  mapAlloc (mapalloc, (char *) javaSignature);
}
@end
