// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
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


@implementation CAction

- getOwner
{
  return owner;
}

//
// temporary support for dropping actions (must be removed from owner first!)
//
- (void) drop
{
  [self dropAllocations: 1];
}

@end


@implementation ActionCall_0

- (void) setFunctionPointer: (func_t)fptr
{
  funcPtr = fptr;
}

- (func_t) getFunctionPointer
{
  return funcPtr;
}

- (int) getNArgs
{
  return 0;
}

- (void) _performAction_: anActivity
{
  funcPtr();
}

@end


@implementation ActionCall_1

- (int) getNArgs
{
  return 1;
}

- (void) setArg1: anArg
{
  arg1 = anArg;
}

- getArg1
{
  return arg1;
}

- (void) _performAction_: anActivity
{
  ((void(*)(id))funcPtr)( arg1 );
}

@end


@implementation ActionCall_2

- (int) getNArgs
{
  return 2;
}

- (void) setArg2: anArg
{
  arg2 = anArg;
}

- getArg2
{
  return arg2;
}

- (void) _performAction_: anActivity
{
  ((void(*)(id, id))funcPtr)( arg1, arg2 );
}

@end


@implementation ActionCall_3

- (int) getNArgs
{
  return 3;
}

- (void) setArg3: anArg
{
  arg3 = anArg;
}

- getArg3
{
  return arg3;
}

- (void) _performAction_: anActivity
{
  ((void(*)(id, id, id))funcPtr)( arg1, arg2, arg3 );
}

@end


@implementation ActionTo_0

- (void) setTarget: aTarget
{
  target = aTarget;
}

- getTarget
{
  return target;
}

- (void) setMessageSelector: (SEL)aSel
{
  selector = aSel;
}

- (SEL) getMessageSelector
{
  return selector;
}

- (int) getNArgs
{
  return 0;
}

- (void) _performAction_: anActivity
{
  [target perform: selector];
}

@end


@implementation ActionTo_1

- (int) getNArgs
{
  return 1;
}

- (void) setArg1: anArg
{
  arg1 = anArg;
}

- getArg1
{
  return arg1;
}

- (void) _performAction_: anActivity
{
  [target perform: selector with: arg1];
}

@end


@implementation ActionTo_2

- (int) getNArgs
{
  return 2;
}

- (void) setArg2: anArg
{
  arg2 = anArg;
}

- getArg2
{
  return arg2;
}

- (void) _performAction_: anActivity
{
  [target perform: selector with: arg1 with: arg2];
}

@end


@implementation ActionTo_3

- (int) getNArgs
{
  return 3;
}

- (void) setArg3: anArg
{
  arg3 = anArg;
}

- getArg3
{
  return arg3;
}

- (void) _performAction_: anActivity
{
  [target perform: selector with: arg1 with: arg2 with: arg3];
}

@end


@implementation ActionForEach_0

- (void) _performAction_: anActivity
{
  id  memberAction;

  memberAction = [id_ForEachActivity_c _create_: self : anActivity ];
  setClass( memberAction, id_ActionTo_0 );
}

@end


@implementation ActionForEach_1

- (void) _performAction_: anActivity
{
  id  memberAction;

  memberAction = [id_ForEachActivity_c _create_: self : anActivity ];
  setClass( memberAction, id_ActionTo_1 );
}

@end


@implementation ActionForEach_2

- (void) _performAction_: anActivity
{
  id  memberAction;

  memberAction = [id_ForEachActivity_c _create_: self : anActivity ];
  setClass( memberAction, id_ActionTo_2 );
}

@end

@implementation ActionForEach_3

- (void) _performAction_: anActivity
{
  id  memberAction;

  memberAction = [id_ForEachActivity_c _create_: self : anActivity ];
  setClass( memberAction, id_ActionTo_3 );
}

@end
