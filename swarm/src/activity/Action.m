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
#import <defobj/directory.h>

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

@implementation CFAction
PHASE(Creating)
+ createBegin: aZone
{
  CFAction *action = [super createBegin: aZone];

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
- (void)_addArguments_
{
  if (argCount >= 1)
    [arguments addObject: arg1];
  if (argCount >= 2)
    [arguments addObject: arg2];
  if (argCount >= 3)
    [arguments addObject: arg3];
}

- (void)_performAction_: anActivity
{
  [call performCall];
}

- (void)describe: outputCharStream
{
  [outputCharStream catC: "CFAction"];
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

- (void)drop
{
  if (call)
    {
      [[call getArguments] drop];
      [call drop];
    }
  [super drop];
}
@end

@implementation FAction_s
PHASE(Creating)
- setCall: fcall
{
  call = fcall;
  return self;
}

PHASE(Using)
- (void)_performAction_: anActivity
{
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
  [super createEnd];

  arguments = [FArguments createBegin: getZone (self)];

  [arguments setObjCReturnType: _C_VOID];
  [self _addArguments_];
  arguments = [arguments createEnd];

  call = [FCall createBegin: getZone (self)];
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
  target = aTarget;
}

- (void)setMessageSelector: (SEL)aSel
{
  selector = aSel;
}

- createEnd
{
  id protoTarget = target;

  [super createEnd];

  if ([self conformsTo: @protocol (ActionForEach)])
    protoTarget = [target allSameClass] ? [target getFirst] : nil;
  
  if (protoTarget)
    {
      arguments = [FArguments createBegin: getZone (self)];

      if ([protoTarget respondsTo: M(isJavaProxy)])
        {
          jobject jsel = SD_FINDJAVA (jniEnv, (id) selector);
          const char *sig =
            swarm_directory_ensure_selector_type_signature (jniEnv, jsel);
          
          [arguments setJavaSignature: sig];
        }
      [arguments setObjCReturnType: _C_ID];
      [self _addArguments_];
      arguments = [arguments createEnd];
      call = [self _createCall_: protoTarget];
    }
  else
    call = nil;
  return self;
}

PHASE(Using)
- _createCall_: protoTarget
{
  id fc = [FCall createBegin: getZone (self)];

  [fc setArguments: arguments];
  if ([protoTarget respondsTo: M(isJavaProxy)])
    [fc setJavaMethod: sel_get_name (selector)
        inObject: SD_FINDJAVA (jniEnv, protoTarget)];
  else
    [fc setMethod: selector inObject: protoTarget];
  return [fc createEnd];
}

- (void)_performAction_: anActivity
{
  updateTarget (call, target);
  if (call)
    [call performCall];
  else
    {
      id fc = [self _createCall_: target];
        
      [fc performCall];
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
PHASE(Setting)
- (void)setDefaultOrder: aSymbol
{
  setDefaultOrder (&bits, aSymbol);
}

PHASE(Using)
- (void)_performAction_: anActivity
{
  id memberAction;

  if (getBit (bits, BitRandomized))
    memberAction = 
      [id_ForEachActivity_c _createRandom_: self : anActivity ];
  else
    memberAction = [id_ForEachActivity_c _create_: self : anActivity ];

  setClass (memberAction, id_ActionTo_c);
}

- getDefaultOrder
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

