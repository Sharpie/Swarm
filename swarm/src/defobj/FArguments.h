// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         FArguments.h
Description:  used for packing arguments to a foreign call
Library:      defobj
*/

#import <defobj.h>
#import <defobj/Create.h>
#include <objc/objc.h> // retval_t


#define number_of_types 15 

typedef enum {fcall_type_void = 0, fcall_type_uchar, fcall_type_schar,
              fcall_type_ushort, fcall_type_sshort, fcall_type_uint,
              fcall_type_sint, fcall_type_ulong, fcall_type_slong,
              fcall_type_float, fcall_type_double, fcall_type_object,
              fcall_type_string, fcall_type_selector, 
              fcall_type_jobject} fcall_type_t;

#define MAX_ARGS        5
#define MAX_HIDDEN      3

@interface FArguments: CreateDrop_s
{
@public
   unsigned assignedArgumentCount;
   unsigned hiddenArgumentCount;
   fcall_type_t *argTypes;
   void **ffiArgTypes;
   fcall_type_t returnType;
   void *ffiReturnType;
   void **argValues;
   void *result;
   const char *javaSignature; 
   unsigned javaSignatureLength;
}
+ createBegin: aZone;
- addArgument: (void *)value ofObjCType: (char)type;
- addChar: (char)value;
- addUnsignedChar: (unsigned char)value;
- addShort: (short)value;
- addUnsignedShort: (unsigned short)value;
- addInt: (int)value;
- addUnsigned: (unsigned)value;
- addLong: (long)value;
- addUnsignedLong: (unsigned long)value;
- addFloat: (float)value;
- addDouble: (double)value;
- _setReturnType_: (fcall_type_t)type;
- setObjCReturnType: (char)type;
- createEnd;
- (void *)getResult;
- (void)mapAllocations: (mapalloc_t)mapalloc;
@end
