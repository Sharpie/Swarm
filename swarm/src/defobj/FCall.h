// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         FCall.h
Description:  foreign function call
Library:      defobj
*/

#import <defobj.h>
#include <ffi.h>
#include <jni.h>
#include "JavaEnv.h"

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

extern ffi_type * swarm_types[number_of_types];
extern char * java_type_signatures[number_of_types];
extern void * java_static_call_functions[number_of_types];
extern void * java_call_functions[number_of_types];

void init_java (void);

enum callTypes { ccall, objccall, javacall, javastaticcall};

@interface FCall: CreateDrop_s
{
@public
   unsigned int callType;
   unsigned int argNo;
   unsigned int assignedArguments;
   unsigned int hiddenArguments;
   void ** argTypes;
   void ** argValues;
   void * returnType;
   void * result;
   ffi_cif cif;
   void (*function)();
   void * object;
   void * class;
   void * method;
   unsigned int signatureLength;
}
+ createBegin: aZone;
- setCallType: (unsigned int) callType;
- setFunction: (void (*)()) fn;
- setMethod: (SEL) method inObject: (id) object;
- setJavaMethod: (const char *) methodName inClass: (const char *) className
       inObject: (jobject) obj;
- setJavaMethod: (const char *) methodName inClass: (const char *) className;
- setNumberOfArguments: (int) argNo;
- addArgument: (void *) value ofType: (unsigned int) type;
- addShort: (short) value;
- addChar: (char) value;
- addInt: (int) value;
- addLong: (long) value;
- addFloat: (float) value;
- addDouble: (double) value;
- addString: (char *) value;
- addJObject: (jobject) value;
- setReturnType: (unsigned int) type;
- setStringReturnType;
- setJObjectReturnType;
- createEnd;
- (void) _performAction_: anActivity;
- (char *) getStringResult;
- (jobject) getJObjectResult;
- (void *) getResult;
@end




