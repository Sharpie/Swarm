// Swarm library. Copyright (C) 1996-1997-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         Action.h
Description:  action included in an activity plan
Library:      activity
*/

#import <activity/CompoundAction.h>
#import <activity.h>

@interface CAction : Object_s
{
@public
  ActionType_c  *owner;        // action type that binds action in its context
  member_t      ownerActions;  // internal links in actions owned by ActionType
}
/*** methods in CAction (inserted from .m file by m2h) ***/
- getOwner;
- (void) drop;
@end

@interface ActionCall_0 : CAction
{
@public
  func_t  funcPtr;      // pointer to function to be called
}
/*** methods in ActionCall_0 (inserted from .m file by m2h) ***/
- (void) setFunctionPointer: (func_t)fptr;
- (func_t) getFunctionPointer;
- (int) getNArgs;
- (void) _performAction_: anActivity;
- (void) describe: outputCharStream;
@end

@interface ActionCall_1 : ActionCall_0
{
@public
  id  arg1;           // first argument
}
/*** methods in ActionCall_1 (inserted from .m file by m2h) ***/
- (int) getNArgs;
- (void) setArg1: anArg;
- getArg1;
- (void) _performAction_: anActivity;
- (void) describe: outputCharStream;
@end

@interface ActionCall_2 : ActionCall_1
{
@public
  id  arg2;           // second argument
}
/*** methods in ActionCall_2 (inserted from .m file by m2h) ***/
- (int) getNArgs;
- (void) setArg2: anArg;
- getArg2;
- (void) _performAction_: anActivity;
- (void) describe: outputCharStream;
@end

@interface ActionCall_3 : ActionCall_2
{
@public
  id  arg3;           // third argument
}
/*** methods in ActionCall_3 (inserted from .m file by m2h) ***/
- (int) getNArgs;
- (void) setArg3: anArg;
- getArg3;
- (void) _performAction_: anActivity;
- (void) describe: outputCharStream;
@end

@interface ActionTo_0 : CAction
{
@public
  id   target;         // receiver of action message
  SEL  selector;       // selector of message to send
}
/*** methods in ActionTo_0 (inserted from .m file by m2h) ***/
- (void) setTarget: aTarget;
- getTarget;
- (void) setMessageSelector: (SEL)aSel;
- (SEL) getMessageSelector;
- (int) getNArgs;
- (void) _performAction_: anActivity;
- (void) describe: outputCharStream;
@end

@interface ActionTo_1 : ActionTo_0
{
@public
  id  arg1;           // first argument
}
/*** methods in ActionTo_1 (inserted from .m file by m2h) ***/
- (int) getNArgs;
- (void) setArg1: anArg;
- getArg1;
- (void) _performAction_: anActivity;
- (void) describe: outputCharStream;
@end

@interface ActionTo_2 : ActionTo_1
{
@public
  id  arg2;           // second argument
}
/*** methods in ActionTo_2 (inserted from .m file by m2h) ***/
- (int) getNArgs;
- (void) setArg2: anArg;
- getArg2;
- (void) _performAction_: anActivity;
- (void) describe: outputCharStream;
@end

@interface ActionTo_3 : ActionTo_2
{
@public
  id  arg3;           // third argument
}
/*** methods in ActionTo_3 (inserted from .m file by m2h) ***/
- (int) getNArgs;
- (void) setArg3: anArg;
- getArg3;
- (void) _performAction_: anActivity;
- (void) describe: outputCharStream;
@end

@interface ActionForEach_0 : ActionTo_0
/*** methods in ActionForEach_0 (inserted from .m file by m2h) ***/
- (void) _performAction_: anActivity;
- (void) describe: outputCharStream;
@end

@interface ActionForEach_1 : ActionTo_1
/*** methods in ActionForEach_1 (inserted from .m file by m2h) ***/
- (void) _performAction_: anActivity;
- (void) describe: outputCharStream;
@end

@interface ActionForEach_2 : ActionTo_2
/*** methods in ActionForEach_2 (inserted from .m file by m2h) ***/
- (void) _performAction_: anActivity;
- (void) describe: outputCharStream;
@end

@interface ActionForEach_3 : ActionTo_3
/*** methods in ActionForEach_3 (inserted from .m file by m2h) ***/
- (void) _performAction_: anActivity;
- (void) describe: outputCharStream;
@end
