// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         FArguments.m
Description:  used for packing arguments to a foreign call
Library:      defobj
*/

#import "FArguments.h"
#import <objc/objc-api.h>
#import <defalloc.h>
#include <misc.h> // stpcpy

#include <swarmconfig.h>

#ifndef USE_AVCALL
#undef PACKAGE
#undef VERSION
#include <ffi.h>
#undef PACKAGE
#undef VERSION

// swarm_types table is modified in such way that 
// swarm_types[fcall_type_float] == &ffi_type_double
// due to ffi bug

ffi_type *ffi_types[FCALL_TYPE_COUNT] = { &ffi_type_void,
                                          &ffi_type_uchar, &ffi_type_schar, 
                                          &ffi_type_ushort, &ffi_type_sshort, 
                                          &ffi_type_uint, &ffi_type_sint, 
                                          &ffi_type_ulong, &ffi_type_slong, 
                                          &ffi_type_double, &ffi_type_double,
                                          &ffi_type_pointer,
                                          &ffi_type_pointer, 
                                          &ffi_type_pointer, 
                                          &ffi_type_pointer };
#endif


const char *java_type_signature[FCALL_TYPE_COUNT] = {
  "V", "C", "C", "S", "S", "I", 
  "I", "J", "J", "F", "D", "X",
  "Ljava/lang/String;", 
  "Lswarm/Selector;",
  "Ljava/lang/Object;"
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
  _C_FLT,
  _C_DBL,
  _C_ID,
  _C_CHARPTR,
  _C_SEL,
  '\0'
};

@implementation FArguments

+ createBegin: aZone
{
  FArguments *newArguments;

  newArguments = [aZone allocIVars: self];
  newArguments->assignedArgumentCount = 0;
  newArguments->hiddenArgumentCount = 0;
#ifndef USE_AVCALL
  newArguments->ffiArgTypes = [aZone allocBlock: (sizeof (ffi_type *) * 
				       (MAX_ARGS + MAX_HIDDEN))];
  newArguments->argTypes = [aZone allocBlock:
                                    (sizeof (fcall_type_t) * 
                                     (MAX_ARGS + MAX_HIDDEN))];
#else
  abort ();
#endif
  newArguments->argValues = [aZone allocBlock: (sizeof (void *) * 
                                                (MAX_ARGS + MAX_HIDDEN))];
  newArguments->returnType = 0;
  newArguments->ffiReturnType = &ffi_type_void;
  newArguments->result = NULL;
  newArguments->javaSignatureLength = 0;
  return newArguments;
}


- _addArgument_: (void *)value ofType: (fcall_type_t)type
{
  if (assignedArgumentCount == MAX_ARGS)
    raiseEvent (SourceMessage,
                "Types already assigned to maximum number arguments in the call!\n");

  argTypes[MAX_HIDDEN + assignedArgumentCount] = type;
#ifndef USE_AVCALL
  argValues[MAX_HIDDEN + assignedArgumentCount] = 
    [[self getZone] allocBlock: ffi_types[type]->size];
  memcpy (argValues[MAX_HIDDEN + assignedArgumentCount], 
          value,
          ffi_types[argTypes[MAX_HIDDEN + assignedArgumentCount]]->size);
#else
  abort ();
#endif
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

#define ADD_COMMON_TEST if (assignedArgumentCount == MAX_ARGS) raiseEvent (SourceMessage, "Types already assigned to all arguments in the call!\n"); if (!value) raiseEvent (SourceMessage, "NULL pointer passed as a pointer to argument!\n");

#ifndef USE_AVCALL
#define ADD_PRIMITIVE(fcall_type, type, value)  { ADD_COMMON_TEST; javaSignatureLength++; argValues[MAX_HIDDEN + assignedArgumentCount] = [[self getZone] allocBlock: ffi_types[(fcall_type)]->size]; argTypes[MAX_HIDDEN + assignedArgumentCount] = fcall_type; *(type *) argValues[MAX_HIDDEN + assignedArgumentCount++] = value; }
#else
#define ADD_PRIMITIVE(type) abort ()
#endif

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

- _setReturnType_: (fcall_type_t)type
{
  javaSignatureLength += strlen (java_type_signature[type]);
  
#ifndef USE_AVCALL
  result = (void *) [[self getZone] allocBlock: ffi_types[type]->size];
#else
  abort ();
#endif
  returnType = type;
  return self;
}

- setObjCReturnType: (char)objcType
{
  return [self _setReturnType_: get_fcall_type_for_objc_type (objcType)];
}

void
add_ffi_types (FArguments * self)
{
  unsigned i;

  for (i = 0; i < self->assignedArgumentCount; i++)
#ifndef USE_AVCALL
    *(self->ffiArgTypes + i + MAX_HIDDEN) =
      ffi_types [self->argTypes[i + MAX_HIDDEN]];
  self->ffiReturnType = ffi_types [self->returnType];
#else
  abort ();
#endif
}

static const char *
createJavaSignature (FArguments *self)
{
  unsigned i;
  char *str, *p;

  str = [[self getZone] allocBlock: self->javaSignatureLength + 3];
  p = stpcpy (str, "(");
  printf ("%u\n", self->assignedArgumentCount);
  for (i = 0; i < self->assignedArgumentCount; i++)
    p = stpcpy (p, java_type_signature [self->argTypes[i + MAX_HIDDEN]]);
  
  p = stpcpy (p, ")");
  p = stpcpy (p, java_type_signature [self->returnType]);
  printf("\n%s\n", str);
  return str;
}

- createEnd
{
  setMappedAlloc (self);
  javaSignature = createJavaSignature ((FArguments *) self);
  return self;
}

- (void *)getResult
{
  return result;
}

- (void)mapAllocations: (mapalloc_t)mapalloc
{
  unsigned i;

  for (i = 0; i< assignedArgumentCount; i++)
    mapAlloc (mapalloc, argValues[MAX_HIDDEN + i]);
  mapAlloc (mapalloc, argTypes);
  mapAlloc (mapalloc, ffiArgTypes);
  mapAlloc (mapalloc, argValues);
  mapAlloc (mapalloc, (char *) javaSignature);
  if (result)
      mapAlloc (mapalloc,  result);
}

@end


