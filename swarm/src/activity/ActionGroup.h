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
Name:         ActionGroup.h
Description:  collection of actions under partial order constraints
Library:      activity
*/

#import <Swarm/OrderedSet.h>
#import <Swarm/Permutation.h>
#import <Swarm/CompoundAction.h>
#import <Swarm/XActivity.h>
#import <Swarm/activity.h> // Action, ActionGroup protocols
#import <Swarm/Zone.h>

@interface ActionGroup_c: OrderedSet_c <ActionGroup>
{
@public
   // variables for CompoundAction mixin inheritance
   // (referenced by source inclusion)
  id activityRefs; // activities currently running this plan
}
/*** methods implemented in CompoundAction.m file ***/
- setAutoDrop: (BOOL)autoDrop;
- (BOOL)getAutoDrop;
- setDefaultOrder: (id <Symbol>)aSymbol;
- (id <Symbol>)getDefaultOrder;
- (id <Activity>)activate;
- (id <Activity>)activateIn: swarmContext;
- _activateIn_: swarmContext : (Class)activityClass : (Class)indexClass: (Zone_c *)aZone;
- (void)_performPlan_;
- _createActivity_: (Activity_c *)ownerActivity : (Class)activityClass : (Class)indexClass : (Zone_c *)aZone;
- (void)drop;
- _createPermutedIndex_: aZone activity: activity;
/*** methods in ActionGroup_c (inserted from .m file by m2h) ***/
- createEnd;
- _activateUnderSwarm_: (Class)activityClass : (Class)indexClass : swarmContext: (Zone_c *)swarmZone;
- (id <FAction>)createFAction: call;
- createAction: anActionType;
- (id <ActionCall>)createActionCall: (func_t)fptr;
- (id <ActionCall>)createActionCall: (func_t)fptr : arg1;
- (id <ActionCall>)createActionCall: (func_t)fptr : arg1 : arg2;
- (id <ActionCall>)createActionCall: (func_t)fptr : arg1 : arg2 : arg3;
- (id <ActionTo>)createActionTo: target message: (SEL)aSel;
- (id <ActionTo>)createActionTo: target message: (SEL)aSel : arg1;
- (id <ActionTo>)createActionTo: target message: (SEL)aSel : arg1 : arg2;
- (id <ActionTo>)createActionTo: target message: (SEL)aSel : arg1 : arg2 : arg3;
- (id <ActionForEachHomogeneous>)createActionForEachHomogeneous: target message: (SEL)aSel;
- (id <ActionForEach>)createActionForEach: target message: (SEL)aSel;
- (id <ActionForEach>)createActionForEach: target message: (SEL)aSel : arg1;
- (id <ActionForEach>)createActionForEach: target message: (SEL)aSel : arg1 : arg2;
- (id <ActionForEach>)createActionForEach: target message: (SEL)aSel : arg1 : arg2 : arg3;
- (id <ActionForEach>)createActionForEach: target message: (SEL)aSel : arg1 : arg2 : arg3;

- (id <FActionForEachHomogeneous>)createFActionForEachHomogeneous: target call: (id <FCall>)call;
- (id <FActionForEachHeterogeneous>)createFActionForEachHeterogeneous: target call: (id <FCall>)call;

- (void)mapAllocations: (mapalloc_t)mapalloc;
- (void)describe: outputCharStream;
- (void)describeForEach: outputCharStream;
@end

@interface ConcurrentGroup_c: ActionGroup_c <ConcurrentGroup>
{
  id <Action> actionConcurrent;  // action that includes group in schedule
}
/*** methods in ConcurrentGroup_c (inserted from .m file by m2h) ***/
- createEnd;
- (void)_setActionConcurrent_: action;
- _getEmptyActionConcurrent_;
- (void)mapAllocations: (mapalloc_t)mapalloc;
@end

@interface GroupActivity_c: Activity_c
/*** methods in GroupActivity_c (inserted from .m file by m2h) ***/
@end

@interface GroupIndex_c: ListIndex_mlinks <ActivityIndex>
{
@public
  id <Activity> activity;               // activity for which index created
}
- setActivity: (id <Activity>)activity;
/*** methods implemented in CompoundAction.m file ***/
- getHoldType;
/*** methods in GroupIndex_c (inserted from .m file by m2h) ***/
- nextAction: (id *)status;
- (void)dropAllocations: (BOOL)componentAlloc;
@end

@interface GroupPermutedIndex_c: PermutedIndex_c <ActivityIndex>
{
@public
  id <Activity> activity;
}
- setActivity: activity;
- getHoldType;
- nextAction: (id *)status;
- (void)dropAllocations: (BOOL)componentAlloc;

@end

@interface ForEachActivity_c: Activity_c <ForEachActivity>
/*** methods in ForEachActivity_c (inserted from .m file by m2h) ***/
+ _create_: forEachAction : anActivity;
+ _createRandom_: forEachAction: anActivity;
- getCurrentMember;
@end

@interface ForEachIndex_c: Object_s <ActivityIndex>
{
@public
  id <Activity> activity;          // activity for which index created
  id <Index> memberIndex;          // index into target collection
  id <ActionForEach> memberAction; // local copy of original ForEach action
}
/*** methods in ForEachIndex_c (inserted from .m file by m2h) ***/
- nextAction: (id *)status;
- get;
- (id <Symbol>)getLoc;
- getHoldType;
- (void)mapAllocations: (mapalloc_t)mapalloc;

- setOffset: (unsigned)offset;
- (unsigned)getOffset;
- (void)setLoc: (id <Symbol>)locSymbol;
- remove;
- put: anObject;
- findPrev: anObject;
- findNext: anObject;
- prev;
- next;
- getCollection;
@end
