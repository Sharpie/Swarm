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
Name:         Action.h
Description:  action included in an activity plan
Library:      activity
*/

#import <Swarm/defobj.h>
#import <Swarm/Create.h>
#import <Swarm/CompoundAction.h>
#import <Swarm/activity.h>

#import <Swarm/swarmconfig.h>

@interface CAction: CreateDrop_s
{
@public
  ActionType_c *owner;       // action type that binds action in its context
  member_t ownerActions;     // internal links in actions owned by ActionType
  unsigned bits;             // bit allocations
  BOOL autoDropFlag;
}
- getOwner;
@end

@interface PAction: CAction
{
@public
  id <FCall> call;
  IMP perform_imp; 
  id target; 
}
- createEnd;
@end

@interface PFAction: PAction <Action, ActionArgs>
{
  unsigned argCount;
  id arg1, arg2, arg3;
}
+ createBegin: aZone;
- (void)_addArguments_: (id <FArguments>)arguments;
- (unsigned)getNArgs;
- (void)setArg1: arg1;
- (void)setArg2: arg2;
- (void)setArg3: arg3;
- getArg1;
- getArg2;
- getArg3;
- (void)mapAllocations: (mapalloc_t)mapalloc;
- (void)_performAction_: (id <Activity>)activity;
@end

@interface FAction_c: PAction <Action>
{
}
- createEnd;
- setCall: fcall;
- setAutoDrop: (BOOL)autoDrop;
- (void)_performAction_: (id <Activity>)anActivity;
- (void)describe: outputCharStream;
@end

@interface ActionCall_c: PFAction
{
  func_t funcPtr;
}
- (void)setFunctionPointer: (func_t)funcPtr;
- createEnd;
- (func_t)getFunctionPointer;
- (void)describe: outputCharStream;
@end

@interface ActionTo_c: PFAction <ActionArgs, ActionTo>
{
@public
  SEL selector;
  id protoTarget;
}
- createEnd;
- _createCall_: protoTarget;
- (void)_performAction_: (id <Activity>)activity;
- (void)setTarget: aTarget;
- getTarget;
- (void)setMessageSelector: (SEL)aSel;
- (SEL)getMessageSelector;
- (void)describe: outputCharStream;
@end

@interface ActionForEach_c: ActionTo_c <ActionForEach>
{
}
- setDefaultOrder: (id <Symbol>)aSymbol;
- (void)describe: outputCharStream;
- (void)_performAction_: (id <Activity>)activity;
@end

@interface ActionForEachHomogeneous_c: CAction <ActionForEachHomogeneous>
{
  SEL selector;
  id target;
  IMP imp;
  size_t targetCount;
  id *objcTargets;
}
- createEnd;
- (void)setTarget: aTarget;
- (void)setMessageSelector: (SEL)aSel;
- setDefaultOrder: (id <Symbol>)aSymbol;
- (SEL)getMessageSelector;
- getTarget;
- (void)describe: outputCharStream;
- (void)_performAction_: (id <Activity>)activity;
@end

@interface FActionForEachHeterogeneous_c: FAction_c <FActionForEachHeterogeneous>
{
}
- (void)setTarget: target;
- setDefaultOrder: (id <Symbol>)aSymbol;
- (void)_performAction_: (id <Activity>)anActivity;
- (id <Symbol>)getDefaultOrder;
- (void)describe: stream;
@end

@interface FActionForEachHomogeneous_c: FAction_c <FActionForEachHomogeneous>
{
  size_t targetCount;
#ifdef HAVE_JDK
  JOBJECT *javaTargets;
#endif
  id *objcTargets;
}
- (void)setTarget: target;
- createEnd;
- setDefaultOrder: (id <Symbol>)aSymbol;
- (void)_performAction_: (id <Activity>)anActivity;
- (id <Symbol>)getDefaultOrder;
- (void)describe: stream;
@end

