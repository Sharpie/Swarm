// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         ActionGroup.h
Description:  collection of actions under partial order constraints
Library:      activity
*/

#import <activity/ActionPlan.h>
#import <activity/Activity.h>
#import <activity/GenericSwarm.h>
#import <collections/OrderedSet.h>

@interface ActionGroup_c : OrderedSet_c
{
@public
// variables for ActionPlan mixin inheritance (referenced by source inclusion)
  id  variableDefs;     // variable defs referenceable within plan
  id  activityRefs;     // activities currently running this plan
}
/*** methods implemented in ActionPlan.m file ***/
- (void) setDefaultOrder: aSymbol;
- (void) setAutoDrop: (BOOL)autoDrop;
- defineVariable;
- defineArgument;
- defineResult;
- (void) _createVarDefsArray_;
/*** methods implemented in .m file ***/
- createEnd;
/*** methods implemented in ActionPlan.m file ***/
- getDefaultOrder;
- (BOOL) getAutoDrop;
- getActivities;
- getVariableDefs;
- _createActivity_: activityClass : indexClass;
- (void) _performPlan_;
- _activateIn_: swarmContext : activityClass : indexClass;
- activateIn: swarmContext;
- activateIn: swarmContext : arg1;
- activateIn: swarmContext : arg1 : arg2;
- activateIn: swarmContext : arg1 : arg2 : arg3;;
- (void) drop;
/*** methods implemented in .m file ***/
- _activateUnderSwarm_: activityClass : indexClass : swarmContext;
- createActionCall: (func_t)fptr;
- createActionCall: (func_t)fptr : arg1;
- createActionCall: (func_t)fptr : arg1 : arg2;
- createActionCall: (func_t)fptr : arg1 : arg2 : arg3;
- createActionTo: target message: (SEL)aSel;
- createActionTo: target message: (SEL)aSel : arg1;
- createActionTo: target message: (SEL)aSel : arg1 : arg2;
- createActionTo: target message: (SEL)aSel : arg1 : arg2 : arg3;
- createActionForEach: target message: (SEL)aSel;
- createActionForEach: target message: (SEL)aSel : arg1;
- createActionForEach: target message: (SEL)aSel : arg1 : arg2;
- createActionForEach: target message: (SEL)aSel : arg1 : arg2 : arg3;
@end

@interface GroupActivity_c : Activity_c
@end

@interface GroupIndex_c : ListIndex_mlinks
{
@public
  id <Activity>  activity;               // activity for which index created
}
/*** methods implemented in ActionPlan.m file ***/
- getHoldType;
/*** methods implemented in .m file ***/
- nextAction: (id *)status;
- (void) drop;
@end

@interface ForEachActivity_c : Activity_c
/*** methods implemented in .m file ***/
+ _create_: forEachAction : anActivity;
- getCurrentMember;
@end

@interface ForEachIndex_c : Object_s
{
@public
  id <Activity>    activity;       // activity for which index created
  id <Index>       memberIndex;    // index into ActionForEach target collection
  ActionForEach_0  *memberAction;  // local copy of original ForEach action
}
/*** methods implemented in .m file ***/
- nextAction: (id *)status;
- get;
- getLoc;
- getHoldType;
- (void) drop;
@end
