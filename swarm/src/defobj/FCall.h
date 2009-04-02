// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

/*
Name:         FCall.h
Description:  foreign function call
Library:      defobj
*/


#import <Swarm/FArguments.h>
#import <Swarm/swarmconfig.h>
#ifndef USE_AVCALL
#undef PACKAGE
#undef VERSION
#include <ffi.h>
#undef PACKAGE
#undef VERSION
#endif

#import <Swarm/defobj.h> // JOBJECT

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



#ifdef HAVE_JDK
void defobj_init_java_call_tables (void *jEnv);
#endif

void updateTarget (FCall_c *self, id target);
#ifdef HAVE_JDK
void updateJavaTarget (FCall_c  *self, JOBJECT target);
#endif

