// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         Schedule.h
Description:  Schedule -- collection of actions ordered by time values
Library:      activity
*/

#import <activity/ActionGroup.h>
#import <activity/XActivity.h>
#import <collections/Map.h>

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
+ createBegin: aZone;
/*** methods implemented in CompoundAction.m file ***/
- (void)setAutoDrop: (BOOL)autoDrop;
- (BOOL)getAutoDrop;
- (void)setDefaultOrder: aSymbol;
- getDefaultOrder;
- setKeepEmptyFlag: (BOOL)keepEmptyFlag;
- activate;
- activateIn: swarmContext;
- _activateIn_: swarmContext : activityClass : indexClass;
- (void)_performPlan_;
- _createActivity_: ownerActivity : activityClass : indexClass;
- (void)drop;
/*** methods in Schedule_c (inserted from .m file by m2h) ***/
+ create: aZone setRepeatInterval: (timeval_t)rptInterval;
+ create: aZone setAutoDrop: (BOOL)autoDrop;
- (void)setConcurrentGroupType: groupType;
- (void)setSingletonGroups: (BOOL)singletonGroups;
- setRelativeTime: (BOOL)relativeTime;
- createEnd;
- setRepeatInterval: (timeval_t)rptInterval;
- getConcurrentGroupType;
- (BOOL)getSingletonGroups;
- (BOOL)getRelativeTime;
- (timeval_t)getRepeatInterval;
- _activateUnderSwarm_: activityClass : indexClass : swarmContext;
- insertGroup: aKey;
- remove: anAction;
- at: (timeval_t)tVal createFAction: call;
- at: (timeval_t)tVal createAction: anActionType;
- at: (timeval_t)tVal createActionCall: (func_t)fptr;
- at: (timeval_t)tVal createActionCall: (func_t)fptr : arg1;
- at: (timeval_t)tVal createActionCall: (func_t)fptr : arg1 : arg2;
- at: (timeval_t)tVal createActionCall: (func_t)fptr : arg1 : arg2 : arg3;
- at: (timeval_t)tVal createActionTo: target message: (SEL)aSel;
- at: (timeval_t)tVal createActionTo: target message: (SEL)aSel : arg1;
- at: (timeval_t)tVal createActionTo: target message: (SEL)aSel : arg1 : arg2;
- at: (timeval_t)tVal createActionTo: target message: (SEL)aSel:arg1:arg2:arg3;;
- at: (timeval_t)tVal createActionForEach: target message: (SEL)aSel;
- at: (timeval_t)tVal createActionForEach: target message: (SEL)aSel : arg1;
- at: (timeval_t)tVal createActionForEach: target message: (SEL)aSel:arg1:arg2;
- at: (timeval_t)tVal createActionForEach: t message: (SEL)aSel:arg1:arg2:arg3;;
- createFAction: call;
- createAction: anActionType;
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
- (void)mapAllocations: (mapalloc_t)mapalloc;
- (void)describe: outputCharStream;
- (void)describeForEach: outputCharStream;
- (void)describeForEachID: outputCharStream;
@end

extern void _activity_insertAction (Schedule_c *, timeval_t, CAction *);

@interface ActionConcurrent_c: CAction
{
@public
  CompoundAction_c *concurrentGroup;  // concurrent group to be executed
}
/*** methods in ActionConcurrent_c (inserted from .m file by m2h) ***/
- (void)_performAction_: anActivity;
- (void)mapAllocations: (mapalloc_t)mapalloc;
- (void)describe: outputCharStream;
@end

@interface ConcurrentSchedule_c: Schedule_c <ConcurrentSchedule>
{
  CAction *actionConcurrent;  // action that includes group in schedule
}
+ createBegin: aZone;
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
  Activity_c *swarmActivity;  // controlling swarm activity, if any
  id mergeAction;             // merge action of swarm activity, if any
  id mergeExternalAction;     // merge action for external swarms
  long activationNumber;      // sequential id of activation within swarm
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
  id <Activity> activity; // activity for which index created
  id currentAction;       // action at current index position
  timeval_t currentTime;  // clock value for activity
  timeval_t startTime;    // time when current execution started
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

@interface ActionChanged_c: CreateDrop
{
@public
  ActionConcurrent_c *actionAtIndex;   // action for new concurrent group
}
/*** methods in ActionChanged_c (inserted from .m file by m2h) ***/
- (void)_performAction_: anActivity;
- (void)describe: outputCharStream;
@end

//
// SwarmActivity_c and ActionMerge_c declared in Schedule.h so that import is
// is minimized for user subclassing of a custom Swarm 
//
@class GenericSwarm_c;

@interface SwarmActivity_c: ScheduleActivity_c <SwarmActivity>
{
@public
  id swarm;           // object that encapsulates the activity
  int nextActivation;  // next unused activation number
}
/*** methods in SwarmActivity_c (manually inserted) ***/
- (void)terminate;
- getSubactivities;
- getSwarm;
- getSynchronizationSchedule;
- (void)mapAllocations: (mapalloc_t)mapalloc;
@end

@interface ActionMerge_c: CAction
{
@public
  ScheduleActivity_c *subactivity;  // activity holding for merge 
  id collectionOfActions;           // collection ofActions to be merged
 
  // collection field is used only to speed up access to Schedule that is
  // to be merged, otherway to get id of this Schedule is to access
  // subactivity->currentIndex->collection with some casting

  BOOL immediateReturnRequestFlag;   // tell merged activity immediately return
}
/*** methods in ActionMerge_c (manually inserted) ***/
- (void)_performAction_: callerActivity;
- (void)mapAllocations: (mapalloc_t)mapalloc;
@end
