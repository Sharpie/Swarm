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
Name:         Schedule.h
Description:  Schedule -- collection of actions ordered by time values
Library:      activity
*/

#import <Swarm/ActionGroup.h>
#import <Swarm/XActivity.h>
#import <Swarm/Action.h> // CAction
#import <Swarm/Map.h>
#import <Swarm/Zone.h>

@interface Schedule_c: Map_c <Schedule>
{
@public
  // variables for CompoundAction mixin inheritance
  // (referenced by source inclusion)
  id activityRefs;            // activities currently running this plan

  // locally defined variables
  id concurrentGroupType;     // type for group of actions at same time
  timeval_t  repeatInterval;  // rescheduling interval, or zero
  BOOL keepEmptyFlag;
}
+ createBegin: (id <Zone>)aZone;
/*** methods implemented in CompoundAction.m file ***/
- setAutoDrop: (BOOL)autoDrop;
- (BOOL)getAutoDrop;
- setDefaultOrder: (id <Symbol>)aSymbol;
- (id <Symbol>)getDefaultOrder;
- setKeepEmptyFlag: (BOOL)keepEmptyFlag;
- (id <Activity>)activate;
- (id <Activity>)activateIn: swarmContext;
- _activateIn_: swarmContext : (Class)activityClass : (Class)indexClass : (Zone_c *)aZone;
- (void)_performPlan_;
- _createActivity_: (Activity_c *)ownerActivity : (Class)activityClass : (Class)indexClass : (Zone_c *)aZone;
- (void)drop;
/*** methods in Schedule_c (inserted from .m file by m2h) ***/
+ create: (id <Zone>)aZone setRepeatInterval: (timeval_t)rptInterval;
+ create: (id <Zone>)aZone setAutoDrop: (BOOL)autoDrop;
- (void)setConcurrentGroupType: groupType;
- (void)setSingletonGroups: (BOOL)singletonGroups;
- setRelativeTime: (BOOL)relativeTime;
- createEnd;
- setRepeatInterval: (timeval_t)rptInterval;
- getConcurrentGroupType;
- (BOOL)getSingletonGroups;
- (BOOL)getRelativeTime;
- (timeval_t)getRepeatInterval;
- _activateUnderSwarm_: (Class)activityClass : (Class)indexClass : swarmContext : (Zone_c *)swarmZone;
- (id <ActionGroup>)insertGroup: (timeval_t)aKey;
- remove: anAction;
- (id <FAction>)at: (timeval_t)tVal createFAction: call;
- at: (timeval_t)tVal createAction: anActionType;
- (id <ActionCall>)at: (timeval_t)tVal createActionCall: (func_t)fptr;
- (id <ActionCall>)at: (timeval_t)tVal createActionCall: (func_t)fptr : arg1;
- (id <ActionCall>)at: (timeval_t)tVal createActionCall: (func_t)fptr : arg1 : arg2;
- (id <ActionCall>)at: (timeval_t)tVal createActionCall: (func_t)fptr : arg1 : arg2 : arg3;
- (id <ActionTo>)at: (timeval_t)tVal createActionTo: target message: (SEL)aSel;
- (id <ActionTo>)at: (timeval_t)tVal createActionTo: target message: (SEL)aSel : arg1;
- (id <ActionTo>)at: (timeval_t)tVal createActionTo: target message: (SEL)aSel : arg1 : arg2;
- (id <ActionTo>)at: (timeval_t)tVal createActionTo: target message: (SEL)aSel:arg1:arg2:arg3;;
- (id <ActionForEachHomogeneous>)at: (timeval_t)tVal createActionForEachHomogeneous: target message: (SEL)aSel;
- (id <ActionForEach>)at: (timeval_t)tVal createActionForEach: target message: (SEL)aSel;
- (id <ActionForEach>)at: (timeval_t)tVal createActionForEach: target message: (SEL)aSel : arg1;
- (id <ActionForEach>)at: (timeval_t)tVal createActionForEach: target message: (SEL)aSel:arg1:arg2;
- (id <ActionForEach>)at: (timeval_t)tVal createActionForEach: t message: (SEL)
aSel:arg1:arg2:arg3;
- (id <FActionForEachHeterogeneous>)at: (timeval_t)tVal createFActionForEachHeterogeneous: target call: (id <FCall>)call;
- (id <FActionForEachHomogeneous>)at: (timeval_t)tVal createFActionForEachHomogeneous: target call: (id <FCall>)call;

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
- (id <FActionForEachHeterogeneous>)createFActionForEachHeterogeneous: target call: (id <FCall>)call;
- (id <FActionForEachHomogeneous>)createFActionForEachHomogeneous: target call: (id <FCall>)call;
- (void)mapAllocations: (mapalloc_t)mapalloc;
- (void)describe: outputCharStream;
- (void)describeForEach: outputCharStream;
- (void)describeForEachID: outputCharStream;
@end

extern void _activity_insertAction (Schedule_c *, timeval_t, CAction *);

@interface ActionConcurrent_c: CAction <ActionConcurrent>
{
@public
  ActionGroup_c *concurrentGroup;  // concurrent group to be executed
}
/*** methods in ActionConcurrent_c (inserted from .m file by m2h) ***/
- (void)_performAction_: (id <Activity>)anActivity;
- (id <ActionGroup>)getConcurrentGroup;
- (void)mapAllocations: (mapalloc_t)mapalloc;
- (void)describe: outputCharStream;
@end

@interface ConcurrentSchedule_c: Schedule_c <ConcurrentSchedule>
{
@public
  CAction *actionConcurrent;  // action that includes group in schedule
}
+ createBegin: (id <Zone>)aZone;
/*** methods in ConcurrentSchedule_c (inserted from .m file by m2h) ***/
- (void)addLast: anAction;
- (void)_setActionConcurrent_: action;
- _getEmptyActionConcurrent_;
- (void)mapAllocations: (mapalloc_t)mapalloc;
@end

@interface ActivationOrder_c: ConcurrentSchedule_c <ActivationOrder>
/*** methods in ActivationOrder_c (inserted from .m file by m2h) ***/
- (void)addLast: mergeAction;
- remove: mergeAction;
- _getEmptyActionConcurrent_;
@end

@interface ScheduleActivity_c: Activity_c <ScheduleActivity>
{
@public
  ScheduleActivity_c *swarmActivity; // controlling swarm activity, if any
  id <Action> mergeAction;           // merge action of swarm activity, if any
  long activationNumber;             // sequential # of activation within swarm
}
/*** methods in ScheduleActivity_c (inserted from .m file by m2h) ***/
- (timeval_t)getCurrentTime;
- stepUntil: (timeval_t)tVal;
- (void)mapAllocations: (mapalloc_t)mapalloc;
- (void)dropAllocations: (BOOL)componentAlloc;
@end

@interface ScheduleIndex_c: MapIndex_c
{
@public
  ScheduleActivity_c *activity; // activity for which index created
  id <Action> currentAction;    // action at current index position
  timeval_t currentTime;        // clock value for activity
  timeval_t startTime;          // time when current execution started
}
/*** methods implemented in CompoundAction.m file ***/
- getHoldType;
/*** methods in ScheduleIndex_c (inserted from .m file by m2h) ***/
- nextAction: (id *)status;
- remove;
- get;
- (timeval_t)getCurrentTime;
- setCurrentTime: (timeval_t)timeval;
- setActivity: (id <Activity>)activity;
- (void)mapAllocations: (mapalloc_t)mapalloc;
- (void)dropAllocations: (BOOL)componentAlloc;
@end

@interface ActionChanged_c: CAction <Action>
{
@public
  ActionConcurrent_c *actionAtIndex;   // action for new concurrent group
}
/*** methods in ActionChanged_c (inserted from .m file by m2h) ***/
- (void)_performAction_: (id <Activity>)anActivity;
- (void)describe: outputCharStream;
- (void)mapAllocations: (mapalloc_t)mapalloc;
@end

//
// SwarmActivity_c and ActionMerge_c declared in Schedule.h so that import is
// is minimized for user subclassing of a custom Swarm 
//
@class GenericSwarm_c;

@interface SwarmActivity_c: ScheduleActivity_c <SwarmActivity>
{
@public
  id swarm;             // object that encapsulates the activity
  long nextActivation;  // next unused activation number
}
/*** methods in SwarmActivity_c (manually inserted) ***/
- (void)terminate;
- getSubactivities;
- getSwarm;
- (id <Schedule>)getSynchronizationSchedule;
- (void)mapAllocations: (mapalloc_t)mapalloc;
@end

@interface ActionMerge_c: CAction <Action>
{
@public
  ScheduleActivity_c *subactivity;  // activity holding for merge 
  id collectionOfActions;           // collection ofActions to be merged
 
  // collection field is used only to speed up access to Schedule that is
  // to be merged, otherway to get id of this Schedule is to access
  // subactivity->currentIndex->collection with some casting
}
/*** methods in ActionMerge_c (manually inserted) ***/
- (void)_performAction_: (id <Activity>)callerActivity;
- (void)mapAllocations: (mapalloc_t)mapalloc;
@end
