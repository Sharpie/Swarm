// Swarm library. Copyright © 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         Action.m
Description:  atomic action included to be included in an activity 
Library:      activity
*/

#import <activity/Action.h>
#import <activity/ActionGroup.h>
#import <activity/Schedule.h>

#include <swarmconfig.h>
#ifdef HAVE_JDK
#import <defobj/directory.h>
#import <defobj/FCall.h>
#import <defobj/javavars.h>
#endif

@implementation CAction
PHASE(Creating)
PHASE(Using)
- getOwner
{
  return owner;
}

- (void)drop
{
  [self dropAllocations: YES];
}
@end

@implementation PAction
PHASE(Creating)
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
  [call performCall];
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

- (void)dropAllocations: (BOOL)componentAlloc
{
  if (call)
    {
      [[call getArguments] drop];
      [call drop];
    }
  [super dropAllocations: componentAlloc];
}
@end

@implementation FAction_c
PHASE(Creating)
- setCall: fcall
{
  call = fcall;
  return self;
}

PHASE(Using)
- (void)_performAction_: (id <Activity>)anActivity
{
  if (target) // in the case of FActionForEach
    updateTarget (call, target);
  
  [call performCall];
}

- (void)describe: outputCharStream
{
  [outputCharStream catC: "FAction"];
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

  sprintf (buffer, "(function at " PTRFMT ")(", fptr);
  [stream catC: buffer];
  if (nargs > 0)
    {
      sprintf( buffer, PTRFMT, arg1 );
      [stream catC: buffer];
      if (nargs > 1)
        {
          sprintf (buffer, ", " PTRFMT, arg2);
          [stream catC: buffer];
          if (nargs > 2)
            {
              sprintf (buffer, ", " PTRFMT, arg3);
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

- (void)setMessageSelector: (SEL)aSel
{
  selector = aSel;
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

PHASE(Using)
- _createCall_: theTarget
{
  id <FArguments> arguments =
    [FArguments createBegin: getCZone (getZone (self))];
  id <FCall> fc = [FCall createBegin: getCZone (getZone (self))];
  
#ifdef HAVE_JDK
  if ([theTarget respondsTo: M(isJavaProxy)])
    {
      jobject jsel = SD_FINDJAVA (jniEnv, (id) selector);
      const char *sig =
        swarm_directory_ensure_selector_type_signature (jniEnv, jsel);
      
      [arguments setJavaSignature: sig];
    }
#endif
  [arguments setObjCReturnType: _C_ID];
  [self _addArguments_: arguments];
  arguments = [arguments createEnd];

  [fc setArguments: arguments];
#ifdef HAVE_JDK
  if ([theTarget respondsTo: M(isJavaProxy)])
    [fc setJavaMethod: sel_get_name (selector)
        inObject: SD_FINDJAVA (jniEnv, theTarget)];
  else
#endif
    [fc setMethod: selector inObject: theTarget];
  return [fc createEnd];
}

- (void)_performAction_: (id <Activity>)anActivity
{
  if (call)
    {
      updateTarget (call, target);
      [call performCall];
    }
  else
    {
      id fc = [self _createCall_: target];
        
      [fc performCall];
      [[fc getArguments] drop];
      [fc drop];
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

  sprintf (buffer, " %s", sel_get_name (msg));
  [stream catC: buffer];
  if (nargs > 0)
    {
      sprintf (buffer, " " PTRFMT, arg1);
      [stream catC: buffer];
      if (nargs > 1)
        {
          sprintf (buffer, " " PTRFMT, arg2);
          [stream catC: buffer];
          if (nargs > 2)
            {
              sprintf (buffer, " " PTRFMT, arg3);
              [stream catC: buffer];
            }
        }
    }
  [stream catC: "]\n"];
}

- (void)describe: stream
{
  char buffer[100];

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
- (void)setDefaultOrder: (id <Symbol>)aSymbol
{
  setDefaultOrder (&bits, aSymbol);
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

@implementation FActionForEach_c
PHASE(Creating)
+ createBegin: aZone
{
  FActionForEach_c *obj = [super createBegin: aZone];

  obj->finalizationFlag = YES;
#ifdef HAVE_JDK
  obj->javaAryLen = 0;
#endif
  return obj;
}

- setTarget: theTarget
{
  target = theTarget;
  return self;
}

- setFinalizationFlag: (BOOL)theFinalizationFlag
{
  finalizationFlag = theFinalizationFlag;
  return self;
}

- createEnd
{
  BOOL allSameFlag = [target allSameClass];

  [super createEnd];
  if (finalizationFlag)
    if (allSameFlag)
      {
#ifdef HAVE_JDK
        if ([target respondsTo: M(isJavaCollection)]
            && getDefaultOrder (bits) == Sequential)
          {
            jobject coll = SD_FINDJAVA (jniEnv, (id) target);
            jarray lref;
            jclass class = (*jniEnv)->GetObjectClass (jniEnv, coll);
            jmethodID method;
            jsize i;
            
            if (!(method =
                  (*jniEnv)->GetMethodID (jniEnv,
                                          class,
                                          "size",
                                          "()I")))
              abort ();
            javaAryLen = (*jniEnv)->CallIntMethod (jniEnv, coll, method);
            if (!(method =
                  (*jniEnv)->GetMethodID (jniEnv,
                                          class,
                                          "get",
                                          "(I)Ljava/lang/Object;")))
              abort ();
            (*jniEnv)->DeleteLocalRef (jniEnv, class);
            if (!(lref =
                  (*jniEnv)->NewObjectArray (jniEnv, javaAryLen, c_Object,
                                             NULL)))
              abort ();
            javaAry = (*jniEnv)->NewGlobalRef (jniEnv, lref);
            (*jniEnv)->DeleteLocalRef (jniEnv, lref);
            {
              jobject javaProtoTarget =
                (*jniEnv)->CallObjectMethod (jniEnv, coll, method, 0);
              
              (*jniEnv)->SetObjectArrayElement (jniEnv, javaAry, 0,
                                                javaProtoTarget);
              (*jniEnv)->DeleteLocalRef (jniEnv, javaProtoTarget);
            }
            for (i = 1; i < javaAryLen; i++)
              {
                jobject obj =
                  (*jniEnv)->CallObjectMethod (jniEnv, coll, method, i);
                (*jniEnv)->SetObjectArrayElement (jniEnv, javaAry, i, obj);
                (*jniEnv)->DeleteLocalRef (jniEnv, obj);
              }
          }
#endif
      }
    else
      raiseEvent (InvalidArgument,
                  "Collection not homogeneous, finalization cannot be used");
  else
    {
      if (!allSameFlag && [call getCallType] != objccall)
        raiseEvent (InvalidArgument,
                    "Heterogeneous collections must be Objective C calls");
    }
  return self;
}

PHASE(Setting)

- (void)setDefaultOrder: (id <Symbol>)aSymbol
{
  setDefaultOrder (&bits, aSymbol);
}

PHASE(Using)

- (void)_performAction_: (id <Activity>)anActivity
{
#ifdef HAVE_JDK
  if (javaAryLen)
    {
      jsize i;

      for (i = 0; i < javaAryLen; i++)
	{
	  jobject obj = (*jniEnv)->GetObjectArrayElement (jniEnv, javaAry, i);
	  
	  updateJavaTarget (call, obj);
	  [call performCall];
	  (*jniEnv)->DeleteLocalRef (jniEnv, obj);
	}
    }
  else
#endif
    {
      id memberAction;
      
      if (getBit (bits, BitRandomized))
        memberAction = 
          [id_ForEachActivity_c _createRandom_: self : anActivity];
      else
        memberAction = [id_ForEachActivity_c _create_: self : anActivity];
      
      setClass (memberAction, id_FAction_c);
    }
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

- (void)drop
{
#ifdef HAVE_JDK
  if (javaAryLen)
    (*jniEnv)->DeleteGlobalRef (jniEnv, javaAry);
#endif
  [super drop];
}
@end
