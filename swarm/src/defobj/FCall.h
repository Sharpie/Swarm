// Swarm library. Copyright � 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         FCall.h
Description:  foreign function call
Library:      defobj
*/


#import "FArguments.h"
#include <swarmconfig.h>
#ifndef USE_AVCALL
#undef PACKAGE
#undef VERSION
#include <ffi.h>
#undef PACKAGE
#undef VERSION
#endif

#include <defobj.h> // JOBJECT

#ifdef HAVE_JDK
void defobj_init_java_call_tables (void *jEnv);
#endif

void updateTarget (id self, id target);
#ifdef HAVE_JDK
void updateJavaTarget (id self, JOBJECT target);
#endif

@interface FCall_c: CreateDrop_s
{
@public
   call_t callType;
   FArguments_c *fargs; 
#ifndef USE_AVCALL
   ffi_cif cif;
#endif
   func_t ffunction;
   void *fobject;
   void *fclass;
   void *fmethod;
   const char *methodName;
}
+ createBegin: aZone;
- setArguments: args;
- getArguments;
- setFunctionPointer: (func_t)fn;
- setMethod: (SEL)method inObject: object;
- setJavaMethod: (const char *)methodName inObject: (JOBJECT)obj;
- setJavaMethod: (const char *)methodName inClass: (const char *)className;
- createEnd;
- (void)performCall;
- (void *)getResult;
- (retval_t)getRetVal: (retval_t)retVal buf: (types_t *)buf;
- (func_t)getFunctionPointer;
- (call_t)getCallType;
@end
