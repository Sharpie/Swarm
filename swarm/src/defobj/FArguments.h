// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
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


#define number_of_types 14 

enum {swarm_type_void = 0, swarm_type_uchar, swarm_type_schar,
      swarm_type_ushort, swarm_type_sshort, swarm_type_uint,
      swarm_type_sint, swarm_type_ulong, swarm_type_slong,
      swarm_type_float, swarm_type_double, swarm_type_object,
      swarm_type_string, swarm_type_jobject} swarm_type_enum;

#define MAX_ARGS        5
#define MAX_HIDDEN      3

@interface FArguments: CreateDrop_s
{
@public
   unsigned assignedArguments;
   unsigned hiddenArguments;
   void **argTypes;
   int *argSwarmTypes;
   void **argValues;
   void *returnType;
   int returnSwarmType;
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
- setObjCReturnType: (char)type;
- createEnd;
- (void *)getResult;
- (void)mapAllocations: (mapalloc_t)mapalloc;
@end
