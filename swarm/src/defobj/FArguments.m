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

/*
Name:         FArguments.m
Description:  used for packing arguments to a foreign call
Library:      defobj
*/

#import <defobj/FArguments.h>
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

#import <defobj/directory.h>
#import "COM.h"

#ifdef HAVE_JDK
#import "java.h" // jniEnv, java_ensure_selector_type_signature, java_signature_for_fcall_type
#import "javavars.h" // f_retTypeFid, c_boolean
#endif

#define ALLOCBLOCKGC(size) [_obj_GCFixedRootZone allocBlock: size]
// Use GC zone for drop consistency on argValues
#define ALLOCTYPE(type) ALLOCBLOCKGC (fcall_type_size (type))

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
  newArguments->retVal.type = 0;
  newArguments->result = NULL;
  newArguments->javaSignatureLength = 2; // "()"
#ifdef HAVE_JDK
  newArguments->pendingGlobalRefFlag = NO;
#endif
  return newArguments;
}

- setLanguage: (id <Symbol>)theLanguage
{
  language = theLanguage;
  return self;
}

- setSelector: (SEL)selector
{
  const char *type = swarm_sel_getTypeEncoding (selector);

  if (!type)
    {
      const char *name = swarm_sel_getName (selector);

      selector = swarm_sel_getUidWithType (name);
      type = swarm_sel_getTypeEncoding (selector);
    }
  
  {
    COMobject cSel;
#ifdef HAVE_JDK
    jobject jSel;
#endif
    
    if ((cSel = SD_COM_FIND_SELECTOR_COM (selector)))
      {
        language = LanguageCOM;
        if (COM_selector_is_boolean_return (cSel))
          [self setBooleanReturnType];
        else
          [self setObjCReturnType: *type];
      }
#ifdef HAVE_JDK
    else if ((jSel = SD_JAVA_FIND_SELECTOR_JAVA (selector)))
      {
        const char *sig =
          java_ensure_selector_type_signature (jSel);
        
        [self setJavaSignature: sig];
        [scratchZone free: (void *) sig];
        {
          jobject retType =
            (*jniEnv)->GetObjectField (jniEnv, jSel, f_retTypeFid);
          
          if ((*jniEnv)->IsSameObject (jniEnv, retType, c_boolean))
            [self setBooleanReturnType];
          else
            [self setObjCReturnType: *type];
          (*jniEnv)->DeleteLocalRef (jniEnv, retType);
        }
      }
#endif
    else
      {
        language = LanguageObjc;
        [self setObjCReturnType: *type];
      }
  }
  return self;
}

- setSelector: (SEL)selector forTarget: (id)theTarget
{
  const char *type = swarm_method_getTypeEncoding(swarm_class_getInstanceMethod(swarm_object_getClass(theTarget), selector));
  
  {
    COMobject cSel;
#ifdef HAVE_JDK
    jobject jSel;
#endif
    
    if ((cSel = SD_COM_FIND_SELECTOR_COM (selector)))
      {
        language = LanguageCOM;
        if (COM_selector_is_boolean_return (cSel))
          [self setBooleanReturnType];
        else
          [self setObjCReturnType: *type];
      }
#ifdef HAVE_JDK
    else if ((jSel = SD_JAVA_FIND_SELECTOR_JAVA (selector)))
      {
        const char *sig =
          java_ensure_selector_type_signature (jSel);
        
        [self setJavaSignature: sig];
        [scratchZone free: (void *) sig];
        {
          jobject retType =
            (*jniEnv)->GetObjectField (jniEnv, jSel, f_retTypeFid);
          
          if ((*jniEnv)->IsSameObject (jniEnv, retType, c_boolean))
            [self setBooleanReturnType];
          else
            [self setObjCReturnType: *type];
          (*jniEnv)->DeleteLocalRef (jniEnv, retType);
        }
      }
#endif
    else
      {
        language = LanguageObjc;
        [self setObjCReturnType: *type];
      }
  }
  return self;
}

+ create: aZone setSelector: (SEL)aSel
{
  return [[(FArguments_c *)[self createBegin: aZone] setSelector: aSel] createEnd];
}


- setJavaSignature: (const char *)theJavaSignature
{
  char *buf = ALLOCBLOCKGC (strlen (theJavaSignature) + 1);

  strcpy (buf, theJavaSignature);
  javaSignature = buf;
  language = LanguageJava;
  return self;
}

#ifdef HAVE_JDK
#define ADD_PRIMITIVE_SIZE(fcall_type) javaSignatureLength += strlen (java_signature_for_fcall_type (fcall_type))
#else
#define ADD_PRIMITIVE_SIZE(fcall_type)
#endif

- addArgument: (types_t *)value ofType: (fcall_type_t)type
{
  size_t size = 0;
  unsigned offset = MAX_HIDDEN + assignedArgumentCount;

  if (assignedArgumentCount == MAX_ARGS)
    raiseEvent (SourceMessage,
                "Types already assigned to maximum number arguments in the call!\n");

  if (type == fcall_type_object)
    [self addObject: value->object];
  else if (type == fcall_type_string)
    [self addString: value->string];
  else if (type == fcall_type_selector)
    [self addSelector: value->selector];
  else
    {
      size = fcall_type_size (type);
      argTypes[offset] = type;
      // use GC zone for consistency with drop
      argValues[offset] = ALLOCBLOCKGC (size);
      memcpy (argValues[offset], value, size);
      ADD_PRIMITIVE_SIZE (type);
      assignedArgumentCount++;
    }
  return self;
}

- addArgument: (void *)value ofObjCType: (char)objcType
{
  return [self addArgument: value
               ofType: fcall_type_for_objc_type (objcType)];
}

#define ADD_PRIMITIVE(fcall_type, type, value)  { ADD_PRIMITIVE_SIZE (fcall_type); argValues[MAX_HIDDEN + assignedArgumentCount] = ALLOCTYPE (fcall_type); argTypes[MAX_HIDDEN + assignedArgumentCount] = fcall_type; *(type *) argValues[MAX_HIDDEN + assignedArgumentCount] = value; assignedArgumentCount++; }

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
  if (language == LanguageJava)
    {
      unsigned offset = MAX_HIDDEN + assignedArgumentCount;
      jstring string;
      size_t size;
      void *ptr;
      
      if (str)
        {
          jobject lref = (*jniEnv)->NewStringUTF (jniEnv, str);
          string = (*jniEnv)->NewGlobalRef (jniEnv, lref);
          (*jniEnv)->DeleteLocalRef (jniEnv, lref);
        }
      else
        string = 0;
      size = sizeof (jstring);
      ptr = &string;
      argTypes[offset] = fcall_type_jstring;
      argValues[offset] = ALLOCBLOCKGC (size);
      memcpy (argValues[offset], ptr, size);
      ADD_PRIMITIVE_SIZE (fcall_type_jstring);
      assignedArgumentCount++;
    }
  else
#endif
    ADD_PRIMITIVE (fcall_type_string, const char *, str);
  return self;
}

- addJavaObject: (JOBJECT)jobj type: (fcall_type_t)type;
{
#ifdef HAVE_JDK
  unsigned offset = MAX_HIDDEN + assignedArgumentCount;
  size_t size;
  void *ptr;
  size = sizeof (jobject);

  jobj = (*jniEnv)->NewGlobalRef (jniEnv, jobj);
  ptr = &jobj;
  argTypes[offset] = type;
  argValues[offset] = ALLOCBLOCKGC (size);
  memcpy (argValues[offset], ptr, size);
  ADD_PRIMITIVE_SIZE (type);
  assignedArgumentCount++;
#else
  abort ();
#endif
  return self;
}

- addJavaObject: (JOBJECT)jobj
{
  return [self addJavaObject: jobj type: fcall_type_object];
}

- (void)addObject: value
{
#ifdef HAVE_JDK
  if (language == LanguageJava)
    [self addJavaObject: SD_JAVA_FIND_OBJECT_JAVA (value)
          type: fcall_type_jobject];
  else
#endif
    ADD_PRIMITIVE (fcall_type_object, id, value);
}

- addSelector: (SEL)aSel
{
#ifdef HAVE_JDK
  if (language == LanguageJava)
    [self addJavaObject: SD_JAVA_FIND_SELECTOR_JAVA (aSel)
	  type: fcall_type_jselector];
  else
#endif
    ADD_PRIMITIVE (fcall_type_selector, SEL, aSel);
  return self;
}

- setReturnType: (fcall_type_t)type
{
  if (language == LanguageJava)
    {
      if (type == fcall_type_object)
        type = fcall_type_jobject;
      else if (type == fcall_type_string)
        type = fcall_type_jstring;
    }
  ADD_PRIMITIVE_SIZE (type);
  switch (type)
    {
    case fcall_type_boolean:
      result = &retVal.val.boolean;
      break;
    case fcall_type_uchar:
      result = &retVal.val.uchar;
      break;
    case fcall_type_schar:
      result = &retVal.val.schar;
      break;
    case fcall_type_ushort:
      result = &retVal.val.ushort;
      break;
    case fcall_type_sshort:
      result = &retVal.val.sshort;
      break;
    case fcall_type_uint:
      result = &retVal.val.uint;
      break;
    case fcall_type_sint:
      result = &retVal.val.sint;
      break;
    case fcall_type_ulong:
      result = &retVal.val.ulong;
      break;
    case fcall_type_slong:
      result = &retVal.val.slong;
      break;
    case fcall_type_ulonglong:
      result = &retVal.val.ulonglong;
      break;
    case fcall_type_slonglong:
      result = &retVal.val.slonglong;
      break;
    case fcall_type_float:
      result = &retVal.val._float;
      break;
    case fcall_type_double:
      result = &retVal.val._double;
      break;
    case fcall_type_long_double:
      result = &retVal.val._long_double;
      break;
    case fcall_type_object:
      result = &retVal.val.object;
      break;
    case fcall_type_class:
      result = &retVal.val._class;
      break;
    case fcall_type_string:
      result = &retVal.val.string;
      break;
    case fcall_type_selector:
      result = &retVal.val.selector;
      break;
    case fcall_type_void:
      result = NULL;
      break;
    case fcall_type_jobject:
      result = &retVal.val.object;
      break;
    case fcall_type_jstring:
      result = &retVal.val.object;
      break;
    default:
      abort ();
    }
  retVal.type = type;
  return self;
}

- setObjCReturnType: (char)objcType
{
  return [self setReturnType: fcall_type_for_objc_type (objcType)];
}

- setBooleanReturnType
{
  return [self setReturnType: fcall_type_boolean];
}

#ifdef HAVE_JDK
static const char *
createJavaSignature (FArguments_c *self)
{
  unsigned i;
  char *str, *p;

  str = ALLOCBLOCKGC (self->javaSignatureLength + 1);
  p = stpcpy (str, "(");
  for (i = 0; i < self->assignedArgumentCount; i++)
    p = stpcpy (p, java_signature_for_fcall_type (self->argTypes[i + MAX_HIDDEN]));
  
  p = stpcpy (p, ")");
  p = stpcpy (p, java_signature_for_fcall_type (self->retVal.type));
  return str;
}
#endif

- createEnd
{
  [super createEnd];
  setMappedAlloc (self);
#ifdef HAVE_JDK
  if (language == LanguageJava)
    {
      if (!javaSignature)
        javaSignature = createJavaSignature ((FArguments_c *) self);
      else
        // Set this here rather than in setJavaSignature so that the 
        // signature can be forced (it will be munged by the argument
        // methods).
        javaSignatureLength = strlen (javaSignature);
    }
#endif
  return self;
}

PHASE(Using)

- (void *)getResult
{
  return result;
}

- (void)dropAllocations: (BOOL)componentAlloc
{
#ifdef HAVE_JDK  
  if (pendingGlobalRefFlag)
    {
      (*jniEnv)->DeleteGlobalRef (jniEnv,
				  (jobject) ((types_t *) result)->object);
      pendingGlobalRefFlag = NO;
    }
  {
    unsigned i;
    
    for (i = 0; i < assignedArgumentCount; i++)
      {
        unsigned offset = i + MAX_HIDDEN;
        fcall_type_t type = argTypes[offset];
        
        if (type == fcall_type_jstring
            || type == fcall_type_jobject
            || type == fcall_type_jselector)
          {
            jobject obj = *(jobject *) argValues[offset];

            if (obj)
              (*jniEnv)->DeleteGlobalRef (jniEnv, obj);
          }
      }
  }
#endif
  [super dropAllocations: componentAlloc];
}

- (void)mapAllocations: (mapalloc_t)mapalloc
{
  unsigned i;

  if (!includeBlocks (mapalloc))
    return;

  mapalloc->zone = _obj_GCFixedRootZone;
  for (i = 0; i < assignedArgumentCount; i++)
    {
      unsigned offset = i + MAX_HIDDEN;
      fcall_type_t type = argTypes[offset];

      mapalloc->size = fcall_type_size (type);
      mapAlloc (mapalloc, argValues[offset]);
    }
#ifdef HAVE_JDK
  if (javaSignature)
    {
      mapalloc->size = javaSignatureLength + 1;
      mapAlloc (mapalloc, (char *) javaSignature);
    }
#endif
}

- (id <Symbol>)getLanguage
{
  return language;
}

- (val_t)getRetVal
{
  return retVal;
}

- (void)drop
{
  [self dropAllocations: YES];
}
@end
