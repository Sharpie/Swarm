// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         FCall.h
Description:  foreign function call
Library:      defobj
*/


#import <defobj/FArguments.h>
#include <swarmconfig.h>
#ifndef USE_AVCALL
#undef PACKAGE
#undef VERSION
#include <ffi.h>
#undef PACKAGE
#undef VERSION
#endif

#import <defobj.h> // JOBJECT

#ifdef HAVE_JDK
void defobj_init_java_call_tables (void *jEnv);
#endif

void updateTarget (id self, id target);
#ifdef HAVE_JDK
void updateJavaTarget (id self, JOBJECT target);
#endif

@interface FCall_c: CreateDrop_s <FCall>
{
@public
   call_t callType;
   FArguments_c *fargs; 
#ifndef USE_AVCALL
   ffi_cif cif;
#endif
   void *COM_params;
   func_t ffunction;
   void **gcInfo;
   void *fmethod;
   const char *methodName;
   BOOL fobjectPendingGlobalRefFlag;
}
+ createBegin: aZone;
+ create: aZone target: obj
                selector: (SEL)aSel
                arguments: (id <FArguments>)fa;
+ create: aZone target: obj
                methodName: (const char *)methodName
                arguments: (id <FArguments>)fa;
- setArguments: args;
- getArguments;
- setFunctionPointer: (func_t)fn;
- setMethodFromSelector: (SEL)method inObject: object;
- setMethodFromName: (const char *)methodName inObject: obj;
- setJavaMethodFromName: (const char *)methodName inObject: (JOBJECT)jObj;
- setJavaMethodFromName: (const char *)methodName inClass: (const char *)className;
- createEnd;
- (void)performCall;
- (void *)getResult;
- (retval_t)getRetVal: (retval_t)retVal buf: (types_t *)buf;
- (func_t)getFunctionPointer;
- (call_t)getCallType;
- (void)dropAllocations: (BOOL)componentAlloc;
- (void)drop;
@end
