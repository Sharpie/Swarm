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
#include <ffi.h>
#undef PACKAGE
#undef VERSION
#import <swarmconfig.h>

#define number_of_types 14

#define swarm_type_void    0
#define swarm_type_uchar   1
#define swarm_type_schar   2
#define swarm_type_ushort  3
#define swarm_type_sshort  4
#define swarm_type_uint    5
#define swarm_type_sint    6 
#define swarm_type_ulong   7
#define swarm_type_slong   8
#define swarm_type_float   9 
#define swarm_type_double  10
#define swarm_type_pointer 11
#define swarm_type_string  12
#define swarm_type_jobject 13

#define JOBJECT void *

#define MAX_ARGS        5
#define MAX_HIDDEN      3

@interface FArguments: CreateDrop_s
{
@public
   unsigned assignedArguments;
   unsigned hiddenArguments;
   void **argTypes;
   void **argValues;
   void *returnType;
   void *result;
   const char *javaSignature; 
   unsigned javaSignatureLength;
}
+ createBegin: aZone;
- addArgument: (void *)value ofType: (unsigned int)type;
- addShort: (short)value;
- addChar: (char)value;
- addInt: (int)value;
- addLong: (long)value;
- addFloat: (float)value;
- addDouble: (double)value;
- setReturnType: (unsigned)type;
- createEnd;
- (void *)getResult;
- (void)mapAllocations: (mapalloc_t)mapalloc;
@end




