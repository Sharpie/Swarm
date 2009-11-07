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
Name:         FArguments.h
Description:  used for packing arguments to a foreign call
Library:      defobj
*/

#import <Swarm/defobj.h>
#import <Swarm/Create.h>
#import <Swarm/swarm-objc-api.h> // retval_t
#import <Swarm/internal.h>

#import <Swarm/swarmconfig.h>
#ifdef USE_AVCALL
#include <avcall.h>
#endif

#define MAX_ARGS 10
#define MAX_HIDDEN 3
#define MAX_TOTAL (MAX_HIDDEN + MAX_ARGS)

@interface FArguments_c: CreateDrop_s <FArguments>
{
@public
   unsigned assignedArgumentCount;
   unsigned hiddenArgumentCount;
   val_t retVal;
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
   id <Symbol> language;
}
+ createBegin: aZone;
- setLanguage: (id <Symbol>)language;
- setSelector: (SEL)aSel;
- setSelector: (SEL)aSel forTarget: (id)theTarget;
+ create: aZone setSelector: (SEL)aSel;
- setJavaSignature: (const char *)javaSignature;
- addArgument: (types_t *)value ofType: (fcall_type_t)type;
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
- (void)addObject: value;
- addSelector: (SEL)aSel;
- addJavaObject: (JOBJECT)javaObject;
- addJavaObject: (JOBJECT)javaObject type: (fcall_type_t)type;
- setObjCReturnType: (char)type;
- setReturnType: (fcall_type_t)retType;
- setBooleanReturnType;
- createEnd;
- (void *)getResult;
- (val_t)getRetVal;
- (void)dropAllocations: (BOOL)componentAlloc;
- (void)mapAllocations: (mapalloc_t)mapalloc;
- (id <Symbol>)getLanguage;
- (void)drop;
@end
