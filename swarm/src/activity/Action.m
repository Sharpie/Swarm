// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
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

#import <objc/objc-api.h>


@implementation CAction
PHASE(Using)
- getOwner
{
  return owner;
}

//
// temporary support for dropping actions (must be removed from owner first!)
//
- (void)drop
{
  [self dropAllocations: YES];
}

@end


@implementation ActionCall_0

- (void)setFunctionPointer: (func_t)fptr
{
  funcPtr = fptr;
}

- (func_t)getFunctionPointer
{
  return funcPtr;
}

- (int)getNArgs
{
  return 0;
}

- (void)_performAction_: anActivity
{
  funcPtr ();
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
  describeFunctionCall (outputCharStream, funcPtr, 0, 0, 0, 0);
}

@end


@implementation ActionCall_1

- (int)getNArgs
{
  return 1;
}

- (void)setArg1: anArg
{
  arg1 = anArg;
}

- getArg1
{
  return arg1;
}

- (void)_performAction_: anActivity
{
  ((void (*) (id)) funcPtr) (arg1);
}

- (void)describe: outputCharStream
{
  describeFunctionCall (outputCharStream, funcPtr, 1, arg1, 0, 0);
}

@end


@implementation ActionCall_2

- (int)getNArgs
{
  return 2;
}

- (void)setArg2: anArg
{
  arg2 = anArg;
}

- getArg2
{
  return arg2;
}

- (void)_performAction_: anActivity
{
  ((void (*) (id, id)) funcPtr) (arg1, arg2);
}

- (void)describe: outputCharStream
{
  describeFunctionCall (outputCharStream, funcPtr, 2, arg1, arg2, 0);
}

@end


@implementation ActionCall_3

- (int)getNArgs
{
  return 3;
}

- (void)setArg3: anArg
{
  arg3 = anArg;
}

- getArg3
{
  return arg3;
}

- (void)_performAction_: anActivity
{
  ((void (*) (id, id, id)) funcPtr) (arg1, arg2, arg3);
}

- (void)describe: outputCharStream
{
  describeFunctionCall (outputCharStream, funcPtr, 3, arg1, arg2, arg3);
}

@end


@implementation ActionTo_0

- (void)setTarget: aTarget
{
  target = aTarget;
}

- getTarget
{
  return target;
}

- (void)setMessageSelector: (SEL)aSel
{
  selector = aSel;
}

- (SEL)getMessageSelector
{
  return selector;
}

- (int)getNArgs
{
  return 0;
}

- (void)_performAction_: anActivity
{
  [target perform: selector];
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

static void
describeMessage(id stream, id target, SEL msg, int nargs, id arg1, id arg2, id arg3)
{
  char buffer[100];

  [stream catC: "["];
  _obj_formatIDString (buffer, target);
  [stream catC: buffer];
  describeMessageArgs (stream, msg, nargs, arg1, arg2, arg3);
}

- (void)describe: outputCharStream
{
  describeMessage (outputCharStream, target, selector, 0, 0, 0, 0);
}

@end


@implementation ActionTo_1

- (int)getNArgs
{
  return 1;
}

- (void)setArg1: anArg
{
  arg1 = anArg;
}

- getArg1
{
  return arg1;
}

- (void)_performAction_: anActivity
{
  [target perform: selector with: arg1];
}

- (void)describe: outputCharStream
{
  describeMessage (outputCharStream, target, selector, 1, arg1, 0, 0);
}

@end


@implementation ActionTo_2

- (int)getNArgs
{
  return 2;
}

- (void)setArg2: anArg
{
  arg2 = anArg;
}

- getArg2
{
  return arg2;
}

- (void)_performAction_: anActivity
{
  [target perform: selector with: arg1 with: arg2];
}

- (void)describe: outputCharStream
{
  describeMessage (outputCharStream, target, selector, 2, arg1, arg2, 0);
}

@end


@implementation ActionTo_3

- (int)getNArgs
{
  return 3;
}

- (void)setArg3: anArg
{
  arg3 = anArg;
}

- getArg3
{
  return arg3;
}

- (void)_performAction_: anActivity
{
  [target perform: selector with: arg1 with: arg2 with: arg3];
}

- (void)describe: outputCharStream
{
  describeMessage (outputCharStream, target, selector, 3, arg1, arg2, arg3);
}

@end


@implementation ActionForEach_0

- (void)_performAction_: anActivity
{
  id  memberAction;

  if (getBit (bits, BitRandomized))
    memberAction = 
      [id_ForEachActivity_c _createRandom_: self : anActivity ];
  else
    memberAction = [id_ForEachActivity_c _create_: self : anActivity ];

  setClass (memberAction, id_ActionTo_0);
}

static void
describeForEachMessage (id stream, id target, SEL msg, int nargs, id arg1, id arg2, id arg3)
{
  char buffer[100];

  [stream catC: "[[foreach: "];
  _obj_formatIDString (buffer, target);
  [stream catC: buffer];
  [stream catC: "]"];
  describeMessageArgs (stream, msg, nargs, arg1, arg2, arg3);
}

- (void)describe: outputCharStream
{
  describeForEachMessage (outputCharStream, target, selector, 0, 0, 0, 0);
}

@end


@implementation ActionForEach_1

- (void)_performAction_: anActivity
{
  id memberAction;

  if (getBit (bits, BitRandomized))
    memberAction = 
      [id_ForEachActivity_c _createRandom_: self : anActivity ];
  else
    memberAction = [id_ForEachActivity_c _create_: self : anActivity ];

  setClass (memberAction, id_ActionTo_1);
}

- (void)describe: outputCharStream
{
  describeForEachMessage (outputCharStream, target, selector, 1, arg1, 0, 0);
}

@end


@implementation ActionForEach_2

- (void)_performAction_: anActivity
{
  id  memberAction;

  if (getBit (bits, BitRandomized))
    memberAction = 
      [id_ForEachActivity_c _createRandom_: self : anActivity];
  else
    memberAction = [id_ForEachActivity_c _create_: self : anActivity];

  setClass (memberAction, id_ActionTo_2);
}

- (void)describe: outputCharStream
{
  describeForEachMessage (outputCharStream, target, selector, 2, arg1, arg2, 0);
}

@end

@implementation ActionForEach_3

- (void)_performAction_: anActivity
{
  id  memberAction;

  memberAction = [id_ForEachActivity_c _create_: self : anActivity];
  setClass (memberAction, id_ActionTo_3);
}

- (void)describe: outputCharStream
{
  describeForEachMessage (outputCharStream, target, selector, 3, arg1, arg2, arg3);
}

@end

