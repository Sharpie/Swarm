// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         FCall.h
Description:  foreign function call
Library:      defobj
*/


#import "FArguments.h"

#ifdef HAVE_JDK
void init_javacall_tables (void * jEnv);
#endif

enum callTypes { ccall, objccall, javacall, javastaticcall};

@interface FCall: CreateDrop_s
{
@public
   unsigned int callType;
   FArguments * args; 
   void *result;
   ffi_cif cif;
   void (*ffunction)();
   void *fobject;
   void *fclass;
   void *fmethod;
   char *methodName;
}
+ createBegin: aZone;
- setArguments: args;
- getArguments;
- setFunction: (void (*)())fn;
- setMethod: (SEL)method inObject: object;
- setJavaMethod: (const char *)methodName inObject: (JOBJECT)obj;
- setJavaMethod: (const char *)methodName inClass: (const char *)className;
- createEnd;
- (void)_performAction_: anActivity;
- (void *)getResult;
@end




