// Swarm library. Copyright © 1996-1999 Santa Fe Institute.
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
#import "internal.h"

#include <swarmconfig.h>
#ifdef USE_AVCALL
#include <avcall.h>
#endif

#define MAX_ARGS 10
#define MAX_HIDDEN 3
#define MAX_TOTAL (MAX_HIDDEN + MAX_ARGS)

@interface FArguments_c: CreateDrop_s
{
@public
   unsigned assignedArgumentCount;
   unsigned hiddenArgumentCount;
   fcall_type_t returnType;
   types_t resultVal;
#ifdef HAVE_JDK
   BOOL pendingGlobalRefFlag;
#endif
   fcall_type_t argTypes[MAX_TOTAL];
   void *argValues[MAX_TOTAL];
#ifndef USE_AVCALL
   void *ffiArgTypes[MAX_TOTAL];
   void *ffiReturnType;
#else
   av_alist java_avalist;
   av_alist objc_avalist;
#endif
   void *result;
   const char *javaSignature; 
   unsigned javaSignatureLength;
   BOOL javaFlag;
}
+ createBegin: aZone;
- setJavaFlag: (BOOL)javaFlag;
- setSelector: (SEL)aSel;
+ create: aZone setSelector: (SEL)aSel setJavaFlag: (BOOL)theJavaFlag;
- setJavaSignature: (const char *)javaSignature;
- addArgument: (void *)value ofObjCType: (char)type;
- addBoolean: (BOOL)value;
- addChar: (char)value;
- addUnsignedChar: (unsigned char)value;
- addShort: (short)value;
- addUnsignedShort: (unsigned short)value;
- addInt: (int)value;
- addUnsigned: (unsigned)value;
- addLong: (long)value;
- addUnsignedLong: (unsigned long)value;
- addLongLong: (long long)value;
- addUnsignedLongLong: (unsigned long long)value;
- addFloat: (float)value;
- addDouble: (double)value;
- addLongDouble: (long double)value;
- addString: (const char *)value;
- addObject: value;
- addJavaObject: (JOBJECT)javaObject;
- _setReturnType_: (fcall_type_t)type;
- setObjCReturnType: (char)type;
- setBooleanReturnType;
- createEnd;
- (BOOL)getJavaFlag;
- (void *)getResult;
- (void)mapAllocations: (mapalloc_t)mapalloc;
@end
