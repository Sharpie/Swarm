// Swarm library. Copyright © 1996-2000 Swarm Development Group.
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
#import <defobj/defalloc.h>
#import "internal.h"
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
#import <defobj/directory.h> // jniEnv
#import <defobj/javavars.h> // f_retTypeFid, c_boolean
#endif

#define ZALLOCBLOCK(aZone, size) [aZone allocBlock: size]
#define ALLOCBLOCK(size) ZALLOCBLOCK([self getZone], size)
#define ALLOCTYPE(type) ALLOCBLOCK (fcall_type_size (type))

@implementation FArguments_c
PHASE(Creating)

+ createBegin: aZone
{
  FArguments_c *newArguments;

  newArguments = [aZone allocIVars: self];
  newArguments->assignedArgumentCount = 0;
  newArguments->hiddenArgumentCount = 0;
#ifndef USE_AVCALL
  newArguments->ffiReturnType = &ffi_type_void;
#endif
  newArguments->returnType = 0;
  newArguments->result = NULL;
  newArguments->javaSignatureLength = 2; // "()"
#ifdef HAVE_JDK
  newArguments->pendingGlobalRefFlag = NO;
#endif
  return newArguments;
}

- setSelector: (SEL)selector
{
  const char *type = sel_get_type (selector);

  if (!type)
    {
      const char *name = sel_get_name (selector);

      selector = sel_get_any_typed_uid (name);
      type = sel_get_type (selector);
    }
#ifdef HAVE_JDK
  if (swarmDirectory && javaFlag)
    {
      jobject jsel = SD_FINDJAVA (jniEnv, (id) selector);
      
      if (jsel)
        {
          const char *sig =
            java_ensure_selector_type_signature (jniEnv, jsel);
          
          [self setJavaSignature: sig];
          [scratchZone free: (void *) sig];
          {
            jobject retType =
              (*jniEnv)->GetObjectField (jniEnv, jsel, f_retTypeFid);
            
            if ((*jniEnv)->IsSameObject (jniEnv, retType, c_boolean))
              [self setBooleanReturnType];
            else
              [self setObjCReturnType: *type];
            (*jniEnv)->DeleteLocalRef (jniEnv, retType);
          }
        }
      else
        abort ();
    }
  else
#endif
    [self setObjCReturnType: *type];
  return self;
}

+ create: aZone setSelector: (SEL)aSel setJavaFlag: (BOOL)theJavaFlag
{
  return [[[[self createBegin: aZone]
             setJavaFlag: theJavaFlag]
            setSelector: aSel]
           createEnd];
}


- setJavaSignature: (const char *)theJavaSignature
{
  char *buf = ALLOCBLOCK (strlen (theJavaSignature) + 1);

  strcpy (buf, theJavaSignature);
  javaSignature = buf;
  javaFlag = YES;
  return self;
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

  if (assignedArgumentCount == MAX_ARGS)
    raiseEvent (SourceMessage,
                "Types already assigned to maximum number arguments in the call!\n");

#ifdef HAVE_JDK
  if (type == fcall_type_object)
    [self addObject: *(id *) value];
  else if (type == fcall_type_string)
    [self addString: *(const char **) value];
  else
#endif
    {
      size = fcall_type_size (type);
      argTypes[offset] = type;
      argValues[offset] = ALLOCBLOCK (size);
      memcpy (argValues[offset], value, size);
      javaSignatureLength += strlen (java_signature_for_fcall_type (type));
      assignedArgumentCount++;
    }
  return self;
}

- addArgument: (void *)value ofObjCType: (char)objcType
{
  return [self _addArgument_: value
               ofType: fcall_type_for_objc_type (objcType)];
}

#define ADD_PRIMITIVE(fcall_type, type, value)  { javaSignatureLength += strlen (java_signature_for_fcall_type (fcall_type)); argValues[MAX_HIDDEN + assignedArgumentCount] = ALLOCTYPE (fcall_type); argTypes[MAX_HIDDEN + assignedArgumentCount] = fcall_type; *(type *) argValues[MAX_HIDDEN + assignedArgumentCount] = value; assignedArgumentCount++; }

- addBoolean: (BOOL)value
{
  ADD_PRIMITIVE (fcall_type_boolean, BOOL, value);
  return self;
}

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

- addLongDouble: (long double)value
{
  ADD_PRIMITIVE (fcall_type_long_double, long double, value);
  return self;
}

- addString: (const char *)str
{
#ifdef HAVE_JDK
  if (javaFlag)
    {
      unsigned offset = MAX_HIDDEN + assignedArgumentCount;
      jstring string;
      size_t size;
      void *ptr;
      jobject lref;
      
      lref = (*jniEnv)->NewStringUTF (jniEnv, str);
      string = (*jniEnv)->NewGlobalRef (jniEnv, lref);
      (*jniEnv)->DeleteLocalRef (jniEnv, lref);

      size = sizeof (jstring);
      ptr = &string;
      argTypes[offset] = fcall_type_jstring;
      argValues[offset] = ALLOCBLOCK (size);
      memcpy (argValues[offset], ptr, size);
      javaSignatureLength += strlen (java_signature_for_fcall_type (fcall_type_jstring));
      assignedArgumentCount++;
    }
  else
#endif
    ADD_PRIMITIVE (fcall_type_string, const char *, str);
  return self;
}

- addJavaObject: (JOBJECT)jobj;
{
#ifdef HAVE_JDK
  unsigned offset = MAX_HIDDEN + assignedArgumentCount;
  size_t size;
  void *ptr;
  size = sizeof (jobject);

  jobj = (*jniEnv)->NewGlobalRef (jniEnv, jobj);
  ptr = &jobj;
  argTypes[offset] = fcall_type_jobject;
  argValues[offset] = ALLOCBLOCK (size);
  memcpy (argValues[offset], ptr, size);
  javaSignatureLength += strlen (java_signature_for_fcall_type (fcall_type_jobject));
  assignedArgumentCount++;
#else
  abort ();
#endif
  return self;
}

- addObject: value
{
#ifdef HAVE_JDK
  if (javaFlag)
    [self addJavaObject: SD_FINDJAVA (jniEnv, value)];
  else
#endif
    ADD_PRIMITIVE (fcall_type_object, id, value);
  return self;
}

- _setReturnType_: (fcall_type_t)type
{
  if (javaFlag)
    {
      if (type == fcall_type_object)
        type = fcall_type_jobject;
      else if (type == fcall_type_string)
        type = fcall_type_jstring;
    }
  javaSignatureLength += strlen (java_signature_for_fcall_type (type));

  switch (type)
    {
    case fcall_type_boolean:
      result = &resultVal.boolean;
      break;
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
    case fcall_type_long_double:
      result = &resultVal._long_double;
      break;
    case fcall_type_object:
      result = &resultVal.object;
      break;
    case fcall_type_class:
      result = &resultVal.class;
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
  return [self _setReturnType_: fcall_type_for_objc_type (objcType)];
}

- setBooleanReturnType
{
  return [self _setReturnType_: fcall_type_boolean];
}

static const char *
createJavaSignature (FArguments_c *self)
{
  unsigned i;
  char *str, *p;

  str = ALLOCBLOCK (self->javaSignatureLength + 1);
  p = stpcpy (str, "(");
  for (i = 0; i < self->assignedArgumentCount; i++)
    p = stpcpy (p, java_signature_for_fcall_type (self->argTypes[i + MAX_HIDDEN]));
  
  p = stpcpy (p, ")");
  p = stpcpy (p, java_signature_for_fcall_type (self->returnType));
  return str;
}

- createEnd
{
  [super createEnd];
  setMappedAlloc (self);
  if (!javaSignature)
    javaSignature = createJavaSignature ((FArguments_c *) self);
  else
    // Set this here rather than in setJavaSignature so that the 
    // signature can be forced (it will be munged by the argument
    // methods).
    javaSignatureLength = strlen (javaSignature);
  return self;
}

PHASE(Using)

- (void *)getResult
{
  return result;
}

- (BOOL)getJavaFlag
{
  return javaFlag;
}

- (void)dropAllocations: (BOOL)componentAlloc
{
#ifdef HAVE_JDK  
  unsigned i;

  if (pendingGlobalRefFlag)
    {
      (*jniEnv)->DeleteGlobalRef (jniEnv,
				  (jobject) ((types_t *) result)->object);
      pendingGlobalRefFlag = NO;
    }
  for (i = 0; i < assignedArgumentCount; i++)
    {
      unsigned offset = i + MAX_HIDDEN;
      fcall_type_t type = argTypes[offset];

      if (type == fcall_type_jstring || type == fcall_type_jobject)
        (*jniEnv)->DeleteGlobalRef (jniEnv, *(jobject *) argValues[offset]);
    }
#endif
  [super dropAllocations: componentAlloc];
}

- (void)mapAllocations: (mapalloc_t)mapalloc
{
  unsigned i;

  if (!includeBlocks (mapalloc))
    return;

  for (i = 0; i < assignedArgumentCount; i++)
    {
      unsigned offset = i + MAX_HIDDEN;
      fcall_type_t type = argTypes[offset];

      mapalloc->size = fcall_type_size (type);
      mapAlloc (mapalloc, argValues[offset]);
    }
  mapalloc->size = javaSignatureLength + 1;
  mapAlloc (mapalloc, (char *) javaSignature);
}
@end
