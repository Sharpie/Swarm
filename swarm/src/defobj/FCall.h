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
#include "swarm_types.h"

extern ffi_type * swarm_types[number_of_types];
extern char * java_type_signatures[number_of_types];
extern void * java_static_call_functions[number_of_types];
extern void * java_call_functions[number_of_types];

void init_java (void);

enum callTypes { ccall, objccall, javacall, javastaticcall};

@interface StructureTypeFactory: Object_s
{
@public
   unsigned int fieldsNo;
   unsigned int assignedFields;
   ffi_type ** fieldTypes;
   ffi_type * structureType;
}
+ create: aZone;
- createNewStructureType: aZone numberOfFields: (unsigned int) fieldsNo;
- createCopyOfStructureType: aZone;
- addNewField: (ffi_type *) type;
- atOffset: (unsigned int) offset changeType: (ffi_type *) type;
- (ffi_type *) returnStructureType;
@end

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
