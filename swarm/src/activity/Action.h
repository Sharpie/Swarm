// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         Action.h
Description:  action included in an activity plan
Library:      activity
*/

#import <activity/ActionPlan.h>
#import <activity.h>

@interface Action_c : Object_s
{
@public
  ActionPlan_c  *ownerPlan;    // ActivityPlan which owns action
  member_t      ownerActions;  // list of actions owned by ActivityPlan
}
/*** methods implemented in .m file ***/
- getOwner;
@end

@interface ActionCall_0 : Action_c
{
@public
  func_t  funcPtr;      // pointer to function to be called
}
/*** methods implemented in .m file ***/
- (void) setFunctionPointer: (func_t)fptr;
- (func_t) getFunctionPointer;
- (int) getNArgs;
- (void) _performAction_: anActivity;
- (void) _dropFrom_: aZone;
@end

@interface ActionCall_1 : ActionCall_0
{
@public
  id  arg1;           // first argument
}
/*** methods implemented in .m file ***/
- (int) getNArgs;
- (void) setArg1: anArg;
- getArg1;
- (void) _performAction_: anActivity;
@end

@interface ActionCall_2 : ActionCall_1
{
@public
  id  arg2;           // second argument
}
/*** methods implemented in .m file ***/
- (int) getNArgs;
- (void) setArg2: anArg;
- getArg2;
- (void) _performAction_: anActivity;
@end

@interface ActionCall_3 : ActionCall_2
{
@public
  id  arg3;           // third argument
}
/*** methods implemented in .m file ***/
- (int) getNArgs;
- (void) setArg3: anArg;
- getArg3;
- (void) _performAction_: anActivity;
@end

@interface ActionTo_0 : Action_c
{
@public
  id   target;         // receiver of action message
  SEL  selector;       // selector of message to send
}
/*** methods implemented in .m file ***/
- (void) setTarget: aTarget;
- getTarget;
- (void) setMessageSelector: (SEL)aSel;
- (SEL) getMessageSelector;
- (int) getNArgs;
- (void) _performAction_: anActivity;
- (void) _dropFrom_: aZone;
@end

@interface ActionTo_1 : ActionTo_0
{
@public
  id  arg1;           // first argument
}
/*** methods implemented in .m file ***/
- (int) getNArgs;
- (void) setArg1: anArg;
- getArg1;
- (void) _performAction_: anActivity;
@end

@interface ActionTo_2 : ActionTo_1
{
@public
  id  arg2;           // second argument
}
/*** methods implemented in .m file ***/
- (int) getNArgs;
- (void) setArg2: anArg;
- getArg2;
- (void) _performAction_: anActivity;
@end

@interface ActionTo_3 : ActionTo_2
{
@public
  id  arg3;           // third argument
}
/*** methods implemented in .m file ***/
- (int) getNArgs;
- (void) setArg3: anArg;
- getArg3;
- (void) _performAction_: anActivity;
@end

@interface ActionForEach_0 : ActionTo_0
/*** methods implemented in .m file ***/
- (void) _performAction_: anActivity;
@end

@interface ActionForEach_1 : ActionTo_1
/*** methods implemented in .m file ***/
- (void) _performAction_: anActivity;
@end

@interface ActionForEach_2 : ActionTo_2
/*** methods implemented in .m file ***/
- (void) _performAction_: anActivity;
@end

@interface ActionForEach_3 : ActionTo_3
/*** methods implemented in .m file ***/
- (void) _performAction_: anActivity;
@end
