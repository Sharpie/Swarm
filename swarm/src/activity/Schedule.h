// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         Schedule.h
Description:  Schedule -- collection of actions ordered by time values
Library:      activity
*/

#import <activity/ActionPlan.h>
#import <activity/Activity.h>
#import <collections/Map.h>

@interface Schedule_c : Map_c
{
@public
// variables for ActionPlan mixin inheritance (referenced by source inclusion)
  id  variableDefs;     // variable defs referenceable within plan
  id  activityRefs;     // activities currently running this plan
// locally defined variables
  id         concurrentGroupType;  // type for group of actions at same time
  timeval_t  repeatInterval;       // rescheduling interval, or zero
}
/*** methods implemented in ActionPlan.m file ***/
- (void) setDefaultOrder: aSymbol;
- (void) setAutoDrop: (BOOL)autoDrop;
- defineVariable;
- defineResult;
- defineArgument;
/*** methods implemented in .m file ***/
- (void) setConcurrentGroupType: groupType;
- (void) setSingletonGroups: (BOOL)singletonGroups;
- (void) setRelativeTime: (BOOL)relativeTime;
- (void) setRepeatInterval: (timeval_t)tVal;
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
- getConcurrentGroupType;
- (BOOL) getSingletonGroups;
- (BOOL) getRelativeTime;
- (void) setRepeatInterval: (timeval_t)tVal;
- (timeval_t) getRepeatInterval;
- _activateUnderSwarm_: activityClass : indexClass : swarmContext;
- insertGroup: aKey;
- remove: anAction;
- at: (timeval_t)tVal createActionCall: (func_t)fptr;;
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
@end

extern void _activity_insertAction( Schedule_c *, timeval_t, Action_c *);

@interface ActionConcurrent_c : Action_c
{
@public
  ActionPlan_c  *actionPlan;      // plan to be executed by action
}
- (void) _performAction_: anActivity;
- (void) _dropFrom_: aZone;
@end

@interface ScheduleActivity_c : Activity_c
{
@public
  id    mergeAction;   // merge action running activity under swarm
}
/*** methods implemented in .m file ***/
- (timeval_t) getCurrentTime;
- (void) _drop_;
@end

@interface ScheduleIndex_c : MapIndex_c
{
@public
  id         activity;        // activity for which index created
  id         currentAction;   // action at current index position
  timeval_t  currentTime;     // clock value for activity
  timeval_t  startTime;       // time when current execution started
}
/*** methods implemented in ActionPlan.m file ***/
- getHoldType;
/*** methods implemented in .m file ***/
- nextAction: (id *)status;
- remove;
- get;
- (timeval_t) getCurrentTime;
- (void) drop;
@end

@interface ActionChanged_c : CreateDrop
{
@public
  ActionConcurrent_c  *actionAtIndex;   // action for new concurrent group
}
- (void) _performAction_: anActivity;
@end

#import <activity/GenericSwarm.h>  // include for reference in ActionPlan
