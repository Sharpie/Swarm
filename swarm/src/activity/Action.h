// Swarm library. Copyright © 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         Action.h
Description:  action included in an activity plan
Library:      activity
*/

#import <defobj.h>
#import <defobj/Create.h>
#import <activity/CompoundAction.h>
#import <activity.h>

@interface CAction: CreateDrop_s <Action>
{
@public
  ActionType_c *owner;       // action type that binds action in its context
  member_t ownerActions;     // internal links in actions owned by ActionType
  unsigned bits;             // bit allocations
}
- getOwner;
- (void)drop;
@end

@interface CFAction: CAction <ActionArgs, Action>
{
  unsigned argCount;
  id arg1, arg2, arg3;
  id <FArguments> arguments;
  id <FCall> call;
}
+ createBegin: aZone;
- (void)_addArguments_;
- (unsigned)getNArgs;
- (void)setArg1: arg1;
- (void)setArg2: arg2;
- (void)setArg3: arg3;
- getArg1;
- getArg2;
- getArg3;
- (void)_performAction_: activity;
@end

@interface FAction_s: CAction <Action>
{
  id <FCall> call;
}
- setCall: fcall;
- (void)_performAction_: anActivity;
- (void)describe: outputCharStream;
@end

@interface ActionCall_c: CFAction
{
}
- (void)setFunctionPointer: (func_t)fptr;
- createEnd;
- (func_t)getFunctionPointer;
- (void)describe: outputCharStream;
@end

@interface ActionTo_c: CFAction <ActionArgs>
{
@public
  id target;
  SEL selector;
}
- createEnd;
- _createCall_: protoTarget;
- (void)_performAction_: activity;
- (void)setTarget: aTarget;
- getTarget;
- (void)setMessageSelector: (SEL)aSel;
- (SEL)getMessageSelector;
- (void)describe: outputCharStream;
@end

@interface ActionForEach_c: ActionTo_c <ActionForEach>
- (void)setDefaultOrder: aSymbol;
- (void)describe: outputCharStream;
- (void)_performAction_: activity;
@end
