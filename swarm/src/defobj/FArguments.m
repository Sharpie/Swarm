// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
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

ffi_type *ffi_types[number_of_types] = { &ffi_type_void, &ffi_type_uchar, 
                                         &ffi_type_schar, &ffi_type_ushort, 
                                         &ffi_type_sshort, &ffi_type_uint,
                                         &ffi_type_sint, &ffi_type_ulong, 
                                         &ffi_type_slong, &ffi_type_double, 
                                         &ffi_type_double,
                                         &ffi_type_pointer,
                                         &ffi_type_pointer, 
                                         &ffi_type_pointer };
#endif


const char *java_type_signature[number_of_types] = {
  "V", "C", "C", "S", "S", "I", 
  "I", "J", "J", "F", "D", NULL,
  "Ljava/lang/String;", 
  "Ljava/lang/Object;"
};

char objc_types[number_of_types] = {
  _C_VOID,
  _C_UCHR,
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
  '\0'
};

unsigned java_type_signature_length[number_of_types] = { 1, 1, 1, 1, 1, 1,
                                                         1, 1, 1, 1, 1, 0,
                                                         18, 18 };
@implementation FArguments

+ createBegin: aZone
{
  FArguments *newArguments;

  newArguments = [aZone allocIVars: self];
  newArguments->assignedArguments = 0;
  newArguments->hiddenArguments = 0;
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
  if (assignedArguments == MAX_ARGS)
    raiseEvent (SourceMessage, "Types already assigned to maximum number
arguments in the call!\n");

  if (type != fcall_type_jobject)
    {
      argTypes[MAX_HIDDEN + assignedArguments] = type;
#ifndef USE_AVCALL
      argValues[MAX_HIDDEN + assignedArguments] = 
	[[self getZone] allocBlock: ffi_types[type]->size];
      memcpy (argValues[MAX_HIDDEN + assignedArguments], 
	      value, ffi_types[argTypes[MAX_HIDDEN + 
                                       assignedArguments]]->size);
#else
      abort ();
#endif
      assignedArguments++;
      javaSignatureLength++;
    }
  else
    {
      switch  (type)
	{ 
	case fcall_type_float:
	  [self addFloat: *(float *) value];
	  break;
	default:
	  raiseEvent (SourceMessage, "Passing pointers or structures to Java is not possible!\n");
	  break;
	}
    }
  return self;
}

static unsigned
get_fcall_type_for_objc_type (char objcType)
{
  unsigned i;
  
  for (i = 0; i < number_of_types; i++)
    if (objcType == objc_types[i])
      return i;
  abort ();
}


- addArgument: (void *)value ofObjCType: (char)objcType
{
  return [self _addArgument_: value
               ofType: get_fcall_type_for_objc_type (objcType)];
}

#define ADD_COMMON_TEST if (assignedArguments == MAX_ARGS) raiseEvent (SourceMessage, "Types already assigned to all arguments in the call!\n"); if (!value) raiseEvent (SourceMessage, "NULL pointer passed as a pointer to argument!\n");



#ifndef USE_AVCALL
#define ADD_COMMON(fcall_type, type, value)  { ADD_COMMON_TEST; javaSignatureLength++; argValues[MAX_HIDDEN + assignedArguments] = [[self getZone] allocBlock: ffi_types[(fcall_type)]->size]; argTypes[MAX_HIDDEN + assignedArguments] = fcall_type; *(type *) argValues[MAX_HIDDEN + assignedArguments++] = value; }
#else
#define ADD_COMMON(type) abort ()
#endif

- addChar: (char)value
{
  ADD_COMMON (fcall_type_schar, char, value);
  return self;
}

- addUnsignedChar: (unsigned char)value
{
  ADD_COMMON (fcall_type_uchar, unsigned char, value);
  return self;
}

- addShort: (short)value 
{
  ADD_COMMON (fcall_type_sshort, short, value);
  return self;
}

- addUnsignedShort: (unsigned short)value 
{
  ADD_COMMON (fcall_type_ushort, unsigned short, value);
  return self;
}

- addInt: (int)value
{
  ADD_COMMON (fcall_type_sint, int, value);
  return self;
}

- addUnsigned: (unsigned)value
{
  ADD_COMMON (fcall_type_uint, unsigned, value);
  return self;
}

- addLong: (long)value
{
  ADD_COMMON (fcall_type_slong, long, value);
  return self;
}

- addUnsignedLong: (unsigned long)value
{
  ADD_COMMON (fcall_type_ulong, unsigned long, value);
  return self;
}

- addFloat: (float)value
{
  ADD_COMMON (fcall_type_float, float, value); 
  return self;
}

- addDouble: (double)value
{
  ADD_COMMON (fcall_type_double, double, value);
  return self;
}

- _setReturnType_: (fcall_type_t)type
{
  if (type > number_of_types)
      raiseEvent(SourceMessage,
                 "Unkown return type for foerign function call!\n"); 
  switch (type)
    {
    case fcall_type_void:
    case fcall_type_uchar:
    case fcall_type_schar:
    case fcall_type_ushort:
    case fcall_type_sshort:
    case fcall_type_uint:
    case fcall_type_sint:
    case fcall_type_ulong:
    case fcall_type_slong:
    case fcall_type_float:
    case fcall_type_double: javaSignatureLength++; break;
    default:
      fprintf(stderr, "Unknown type %d!\n", type);
      fflush(stderr);
	  raiseEvent (SourceMessage,
                      "Java methods can not return pointers or structures - specify strings and arrays directly!\n");
    }
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
switch_to_ffi_types (FArguments * self)
{
  unsigned i;

  for (i = MAX_HIDDEN; 
       i < MAX_HIDDEN + self->assignedArguments; 
       i++)
#ifndef USE_AVCALL
    *(self->ffiArgTypes + i) = ffi_types [self->argTypes[i]];
  self->ffiReturnType = ffi_types [self->returnType];
#else
  abort ();
#endif
}

static const char *
createJavaSignature (FArguments * self)
{
  unsigned i;
  unsigned offset = 0;
  char *str;

  str = [[self getZone] allocBlock: self->javaSignatureLength + 3];
  str[offset++] = '(';
  for (i = MAX_HIDDEN; 
       i < MAX_HIDDEN + self->assignedArguments; 
       i++)
    {
       strcpy (str + offset, java_type_signature [self->argTypes[i]]);
       offset += java_type_signature_length [self->argTypes[i]];
    }

  str[offset++] = ')';
  strcpy (str + offset, java_type_signature [self->returnType]);
  offset += strlen (java_type_signature [self->returnType]);
  str[offset++]='\0';
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

  for (i = 0; i< assignedArguments; i++)
    mapAlloc (mapalloc, argValues[MAX_HIDDEN + i]);
  mapAlloc (mapalloc, argTypes);
  mapAlloc (mapalloc, ffiArgTypes);
  mapAlloc (mapalloc, argValues);
  mapAlloc (mapalloc, (char *) javaSignature);
  if (result)
      mapAlloc (mapalloc,  result);
}

@end


