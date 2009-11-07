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
Name:         Action.m
Description:  atomic action included to be included in an activity 
Library:      activity
*/

#import <activity/Action.h>
#import <activity/ActionGroup.h>
#import <activity/Schedule.h>

#include <swarmconfig.h>
#import <defobj/FCall.h>
#ifdef HAVE_JDK
#import "java.h" // SD_JAVA_FIND_OBJECT_JAVA
#endif
#import <defobj/defalloc.h> // getZone
#include <misc.h> // abort in ActionForEachHomogeneous

#define PERFORM(call) perform_imp (call, M(performCall))

extern id uniformUnsRand;
@protocol _MinimalRandom
- (unsigned)getUnsignedWithMin: (unsigned)minVal withMax: (unsigned)maxVal;
@end

@implementation CAction
PHASE(Creating)
- createEnd
{
  [super createEnd];
  autoDropFlag = YES;
  setMappedAlloc (self);
  return self;
}

PHASE(Using)
- getOwner
{
  return owner;
}

- (void)mapAllocations: (mapalloc_t)mapalloc
{
}
@end

@implementation PAction
PHASE(Creating)
- createEnd
{
  [super createEnd];

#if SWARM_OBJC_DONE
  perform_imp = [FCall_c instanceMethodFor: M(performCall)];
#else
  perform_imp = swarm_class_getMethodImplementation([FCall_c class], M(performCall));
#endif

  return self;
}

PHASE(Using)
@end

@implementation PFAction
PHASE(Creating)
+ createBegin: aZone
{
  PFAction *action = [super createBegin: aZone];

  action->argCount = 0;
  return action;
}

- (void)setArg1: anArg
{
  arg1 = anArg;
  if (argCount < 1)
    argCount = 1;
}

- (void)setArg2: anArg
{
  arg2 = anArg;
  if (argCount < 2)
    argCount = 2;
}

- (void)setArg3: anArg
{
  arg3 = anArg;
  if (argCount < 3)
    argCount = 3;
}

PHASE(Using)
- (void)_addArguments_: (id <FArguments>)arguments
{
  if (argCount >= 1)
    [arguments addObject: arg1];
  if (argCount >= 2)
    [arguments addObject: arg2];
  if (argCount >= 3)
    [arguments addObject: arg3];
}

- (void)_performAction_: (id <Activity>)anActivity
{
  PERFORM (call);
}

- (void)describe: outputCharStream
{
  [outputCharStream catC: "PFAction"];
}

- (unsigned)getNArgs
{
  return argCount;
}

- getArg1
{
  return arg1;
}

- getArg2
{
  return arg2;
}

- getArg3
{
  return arg3;
}

- (void)mapAllocations: (mapalloc_t)mapalloc
{
  if (call && autoDropFlag)
    {
      mapObject (mapalloc, [call getArguments]);
      mapObject (mapalloc, call);
    }
}

@end

@implementation FAction_c
PHASE(Creating)
- createEnd
{
  [super createEnd];
  
  autoDropFlag = NO;
  return self;
}

- setCall: fcall
{
  call = fcall;
  return self;
}

PHASE(Setting)
- setAutoDrop: (BOOL)theAutoDropFlag
{
  autoDropFlag = theAutoDropFlag;
  return self;
}

PHASE(Using)
- (void)_performAction_: (id <Activity>)anActivity
{
  if (target) // in the case of FActionForEach
    updateTarget ((FCall_c *)call, target);
  
  PERFORM (call);
}

- (void)describe: outputCharStream
{
  [outputCharStream catC: "FAction\n"];
}
@end

@implementation ActionCall_c
PHASE(Creating)
- (void)setFunctionPointer: (func_t)theFuncPtr
{
  funcPtr = theFuncPtr;
}

- createEnd
{
  id <FArguments> arguments;

  [super createEnd];

  arguments = [FArguments createBegin: getCZone (getZone (self))];

  [arguments setObjCReturnType: _C_VOID];
  [self _addArguments_: arguments];
  arguments = [arguments createEnd];

  call = [FCall createBegin: getCZone (getZone (self))];
  [call setFunctionPointer: funcPtr];
  [call setArguments: arguments];
  call = [call createEnd];
  
  return self;
}

PHASE(Using)
- (func_t)getFunctionPointer
{
  return funcPtr;
}

static void
describeFunctionCall (id stream, func_t fptr, int nargs, id arg1, id arg2, id arg3)
{
  char buffer[100];

  sprintf (buffer, "(function at " PTRHEXFMT ")(", fptr);
  [stream catC: buffer];
  if (nargs > 0)
    {
      sprintf( buffer, PTRHEXFMT, arg1 );
      [stream catC: buffer];
      if (nargs > 1)
        {
          sprintf (buffer, ", " PTRHEXFMT, arg2);
          [stream catC: buffer];
          if (nargs > 2)
            {
              sprintf (buffer, ", " PTRHEXFMT, arg3);
              [stream catC: buffer];
            }
        }
    }
  [stream catC: ")\n"];
}

- (void)describe: outputCharStream
{
  describeFunctionCall (outputCharStream,
                        [call getFunctionPointer],
                        argCount, arg1, arg2, arg3);
}
@end

@implementation ActionTo_c
PHASE(Creating)
- (void)setTarget: aTarget
{
  protoTarget = target = aTarget;
}

- createEnd
{
  [super createEnd];

  if (protoTarget)
    call = [self _createCall_: protoTarget];
  else
    call = nil;
  return self;
}

PHASE(Setting)

- (void)setMessageSelector: (SEL)sel
{
  selector = sel;

  if (call)
    {
      [[call getArguments] dropAllocations: YES];
      [(id) call dropAllocations: YES];
      call = [self _createCall_: target];
    }
}

- _createCall_: theTarget
{
  id <FArguments> arguments =
    [FArguments createBegin: getCZone (getZone (self))];
 
#if SWARM_OSX
  [arguments setSelector: selector forTarget: theTarget];
#else
  [arguments setSelector: selector];
#endif
  if ([theTarget respondsTo: M(isJavaProxy)])
    [arguments setLanguage: LanguageJava];
  else
    [arguments setLanguage: LanguageObjc];
  [self _addArguments_: arguments];
  arguments = [arguments createEnd];

  return [FCall create: getCZone (getZone (self))
                target: theTarget
                selector: selector
                arguments: arguments];
}
PHASE(Using)

- (void)_performAction_: (id <Activity>)anActivity
{
  if (call)
    {
      updateTarget ((FCall_c *)call, target);
      PERFORM (call);
    }
  else
    {
      id fc = [self _createCall_: target];

      PERFORM (fc);
      [[fc getArguments] dropAllocations: YES];
      [fc dropAllocations: YES];
    }
}

- getTarget
{
  return target;
}

- (SEL)getMessageSelector
{
  return selector;
}

static void
describeMessageArgs(id stream, SEL msg, int nargs, id arg1, id arg2, id arg3)
{
  char buffer[100];

  sprintf (buffer, " %s", swarm_sel_getName (msg));
  [stream catC: buffer];
  if (nargs > 0)
    {
      sprintf (buffer, " " PTRHEXFMT, arg1);
      [stream catC: buffer];
      if (nargs > 1)
        {
          sprintf (buffer, " " PTRHEXFMT, arg2);
          [stream catC: buffer];
          if (nargs > 2)
            {
              sprintf (buffer, " " PTRHEXFMT, arg3);
              [stream catC: buffer];
            }
        }
    }
  [stream catC: "]\n"];
}

- (void)describe: stream
{
  char buffer[100];

  _obj_formatIDString (buffer, self);
  [stream catC: buffer];
  [stream catC: "["];
  _obj_formatIDString (buffer, target);
  [stream catC: buffer];
  describeMessageArgs (stream, selector, argCount, arg1, arg2, arg3);
}

@end

@implementation ActionForEach_c
PHASE(Creating)
- createEnd
{
  protoTarget = nil;
  return [super createEnd];
}

PHASE(Setting)
- setDefaultOrder: (id <Symbol>)aSymbol
{
  setDefaultOrder (&bits, aSymbol);
  return self;
}

PHASE(Using)
- (void)_performAction_: (id <Activity>)anActivity
{
  id memberAction;
  
  if (getBit (bits, BitRandomized))
    memberAction = 
      [id_ForEachActivity_c _createRandom_: self : anActivity ];
  else
    memberAction = [id_ForEachActivity_c _create_: self : anActivity ];
  
  setClass (memberAction, id_ActionTo_c);
}

- (id <Symbol>)getDefaultOrder
{
  return getDefaultOrder (bits);
}

- (void)describe: stream
{
  char buffer[100];

  [stream catC: "[[foreach: "];
  _obj_formatIDString (buffer, target);
  [stream catC: buffer];
  [stream catC: "]"];
  describeMessageArgs (stream, selector, argCount, arg1, arg2, arg3);

}

@end

@implementation ActionForEachHomogeneous_c
#define ACTION_HOMOGENEOUS_TYPE ActionForEachHomogeneous_c
#define SETUPCALL imp = swarm_class_getMethodImplementation (swarm_object_getClass([target getFirst]), selector)
#define UPDATEOBJCTARGET(target)
#define PERFORMOBJCCALL(target) imp (target, selector)
#undef UPDATEJAVATARGET
#undef PERFORMJAVACALL
#include "ActionHomogeneous.m"
#undef PERFORMOBJCCALL
#undef UPDATEOBJCTARGET
#undef SETUPCALL
#undef ACTION_HOMOGENEOUS_TYPE
@end

@implementation FActionForEachHeterogeneous_c
PHASE(Creating)
- (void)setTarget: theTarget
{
  target = theTarget;
}

PHASE(Setting)
- setDefaultOrder: (id <Symbol>)aSymbol
{
  setDefaultOrder (&bits, aSymbol);
  return self;
}

PHASE(Using)

- (void)_performAction_: (id <Activity>)anActivity
{
  id memberAction;
  
  if (getBit (bits, BitRandomized))
    memberAction = 
          [id_ForEachActivity_c _createRandom_: self : anActivity];
  else
    memberAction = [id_ForEachActivity_c _create_: self : anActivity];
  
  setClass (memberAction, id_FAction_c);
}

- getTarget
{
  return target;
}

- (id <Symbol>)getDefaultOrder
{
  return getDefaultOrder (bits);
}

- (void)describe: stream
{
  char buffer[100];

  [stream catC: "[[faction foreach: "];
  _obj_formatIDString (buffer, target);
  [stream catC: buffer];
  [stream catC: "]"];
}

@end

@implementation FActionForEachHomogeneous_c
#define ACTION_HOMOGENEOUS_TYPE FActionForEachHomogeneous_c
#undef SETUPCALL
#define UPDATEOBJCTARGET(target) updateTarget ((FCall_c *)call, target)
#define PERFORMOBJCCALL(target) PERFORM (call)
#ifdef HAVE_JDK
#define UPDATEJAVATARGET(jtarget) updateJavaTarget (call, jtarget)
#define PERFORMJAVACALL(target) PERFORM (call)
#endif
#include "ActionHomogeneous.m"
#undef UPDATEJAVACALL
#undef UPDATECALL
#undef ACTION_HOMOGENEOUS_TYPE
@end
