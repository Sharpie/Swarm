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

ffi_type *swarm_types[number_of_types] = { &ffi_type_void, &ffi_type_uchar, 
                                           &ffi_type_schar, &ffi_type_ushort, 
                                           &ffi_type_sshort, &ffi_type_uint,
                                           &ffi_type_sint, &ffi_type_ulong, 
                                           &ffi_type_slong, &ffi_type_float, 
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
  newArguments->argTypes = [aZone allocBlock: (sizeof (ffi_type *) * 
				       (MAX_ARGS + MAX_HIDDEN))];
#else
  abort ();
#endif
  newArguments->argValues = [aZone allocBlock: (sizeof (void *) * 
					(MAX_ARGS + MAX_HIDDEN))];
  newArguments->returnType = swarm_type_void;
  newArguments->result = NULL;
  newArguments->javaSignatureLength = 0;
  return newArguments;
}


- addArgument: (void *)value ofType: (unsigned)type
{
  if (assignedArguments == MAX_ARGS)
    raiseEvent (SourceMessage, "Types already assigned to maximum number
arguments in the call!\n");

  if (type <= swarm_type_double && type != swarm_type_float)
    {
      argTypes[MAX_HIDDEN + assignedArguments] = (void *) type;
#ifndef USE_AVCALL
      argValues[MAX_HIDDEN + assignedArguments] = 
	[[self getZone] allocBlock: swarm_types[type]->size];
      memcpy (argValues[MAX_HIDDEN + assignedArguments], 
	      value, swarm_types[(unsigned) argTypes[MAX_HIDDEN + 
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
	case swarm_type_float:
	  [self addFloat: *(float *) value];
	  break;
	default:
	  raiseEvent (SourceMessage, "Passing pointers or structures to Java is not possible!\n");
	  break;
	}
    }
  return self;
}

#define ADD_COMMON_TEST if (assignedArguments == MAX_ARGS) raiseEvent (SourceMessage, "Types already assigned to all arguments in the call!\n"); if (!value) raiseEvent (SourceMessage, "NULL pointer passed as a pointer to argument!\n");



#ifndef USE_AVCALL
#define ADD_COMMON(swarm_type, type)  { ADD_COMMON_TEST; javaSignatureLength++; argValues[MAX_HIDDEN + assignedArguments] = [[self getZone] allocBlock: swarm_types[(swarm_type)]->size]; argTypes[MAX_HIDDEN + assignedArguments] = (void *)swarm_type; *(type *) argValues[MAX_HIDDEN + assignedArguments++] = value; }
#else
#define ADD_COMMON(type) abort ()
#endif

- addChar: (char)value
{
  ADD_COMMON (swarm_type_schar, char);
  return self;
}

- addUnsignedChar: (unsigned char)value
{
  ADD_COMMON (swarm_type_uchar, unsigned char);
  return self;
}

- addShort: (short)value 
{
  ADD_COMMON (swarm_type_sshort, short);
  return self;
}

- addUnsignedShort: (unsigned short)value 
{
  ADD_COMMON (swarm_type_ushort, unsigned short);
  return self;
}

- addInt: (int)value
{
  ADD_COMMON (swarm_type_sint, int);
  return self;
}

- addUnsigned: (unsigned)value
{
  ADD_COMMON (swarm_type_uint, unsigned);
  return self;
}

- addLong: (long)value
{
  ADD_COMMON (swarm_type_slong, long);
  return self;
}

- addUnsignedLong: (unsigned long)value
{
  ADD_COMMON (swarm_type_ulong, unsigned long);
  return self;
}

- addFloat: (float)value
{
  /* in case the function to be called is compiled with compiler other
     than gcc, that does automatic casting of floats to doubles */
  ADD_COMMON (swarm_type_double, double); 
  return self;
}

- addDouble: (double)value
{
  ADD_COMMON (swarm_type_double, double);
  return self;
}

- setReturnType: (unsigned)type
{
  if (type > number_of_types)
      raiseEvent(SourceMessage,
                 "Unkown return type for foerign function call!\n"); 
  switch (type)
    {
    case swarm_type_void:
    case swarm_type_uchar:
    case swarm_type_schar:
    case swarm_type_ushort:
    case swarm_type_sshort:
    case swarm_type_uint:
    case swarm_type_sint:
    case swarm_type_ulong:
    case swarm_type_slong:
    case swarm_type_float:
    case swarm_type_double: javaSignatureLength++; break;
    default:
	  raiseEvent (SourceMessage,
                      "Java methods can not return pointers or structures - specify strings and arrays directly!\n");
    }
#ifndef USE_AVCALL
  result = (void *) [[self getZone] allocBlock: swarm_types[type]->size];
#else
  abort ();
#endif
  returnType = (void *) type;
  return self;
}

void
switch_to_ffi_types (FArguments * self)
{
  unsigned i;

  for (i = MAX_HIDDEN; 
       i < MAX_HIDDEN + self->assignedArguments; 
       i++)
#ifndef USE_AVCALL
      *(self->argTypes + i) = 
	  ((void *) swarm_types [*(int *) (self->argTypes + i)]);
  self->returnType = (void *) (swarm_types [(unsigned) (self->returnType)]);
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
       strcpy (str + offset, 
	       java_type_signature [*(int *) (self->argTypes + i)]);
       offset += java_type_signature_length [*(int *) (self->argTypes + i)];
    }

  str[offset++] = ')';
  strcpy (str + offset, java_type_signature [(unsigned) self->returnType]);
  offset += strlen (java_type_signature [(unsigned) self->returnType]);
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
  mapAlloc (mapalloc, (void *) javaSignature);
  mapAlloc (mapalloc, argTypes);
  mapAlloc (mapalloc, argValues);
  if (result)
      mapAlloc (mapalloc,  result);
}

@end

