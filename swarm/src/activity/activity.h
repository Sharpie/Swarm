// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         activity.h
Description:  processing control over all levels of swarm execution 
Library:      activity
*/

#import <collections.h>
typedef unsigned   timeval_t;        // type for time values

//
// ActionPlan -- a collection of actions to be performed in a defined order
//
@deftype ActionPlan <Collection, ActionType>
CREATING
- (void)	setDefaultOrder: aSymbol;
- (void)	setAutoDrop: (BOOL)autoDrop;

-		defineVariable;
USING
-		getDefaultOrder;
- (BOOL)	getAutoDrop;

-		activateIn: swarmContext;
-		activateIn: swarmContext : arg1;
-		activateIn: swarmContext : arg1 : arg2;
-		activateIn: swarmContext : arg1 : arg2 : arg3;

-		getActivities;

-		getVariableDefs;
@end

// values for DefaultOrder

extern id <Symbol>  Concurrent, Sequential, Randomized;

//
// VariableDefinition, ArgumentDefinition, ResultDefinition --
//   variables defined as part of action plan
//

@deftype VariableDefinition <DefinedObject>
-		getActionPlan;
@end

@deftype ArgumentDefinition <VariableDefinition> @end
@deftype ResultDefinition   <VariableDefinition> @end


//
// ActionGroup -- a collection of actions under partial order constraints
//             (can be subclassed to define custom rules of action composition)
//
@deftype ActionGroup <ActionPlan, OrderedSet, CREATABLE>
- createAction: anActionType;
- createAction: anActionType : arg1;
- createAction: anActionType : arg1 : arg2;
- createAction: anActionType : arg1 : arg2 : arg3;

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

//
// TimeIndexedPlan -- messages shared by Schedule and Swarm
//
@deftype TimeIndexedPlan
CREATING
- (void)	setConcurrentGroupType: groupType;
- (void)	setSingletonGroups: (BOOL)singletonGroups;
SETTING
- (void)	setRepeatInterval: (timeval_t)tVal;
USING
-		getConcurrentGroupType;
- (BOOL)	getSingletonGroups;
- (timeval_t)	getRepeatInterval;
@end

//
// GenericSwarm -- object to coordinate a collection of started activities
//
@deftype GenericSwarm <ActionGroup, TimeIndexedPlan, GetOwner, CREATABLE>
SETTING
-		setSwarmObjects: aCollection;
USING
-		getSwarmObjects;

-		getSwarmActivity;
-		getSubswarms;
@end

//
// ActivationOrder -- default concurrent group type for swarm
//
@deftype ActivationOrder <ActionGroup, CREATABLE>
@end

//
// Schedule -- collection of actions ordered by time values
//
@deftype Schedule <ActionPlan, TimeIndexedPlan, Map, CREATABLE>
CREATING
- (void)	create: aZone setRepeatInterval: (timeval_t)repeatInterval;
- (void)	setRelativeTime: (BOOL)relative;
USING
- (BOOL)	getRelativeTime;

- at: (timeval_t)tVal createAction: actionType;
- at: (timeval_t)tVal createAction: actionType : arg1;
- at: (timeval_t)tVal createAction: actionType : arg1 : arg2;
- at: (timeval_t)tVal createAction: actionType : arg1 : arg2 : arg3;

- at: (timeval_t)tVal createActionCall: (func_t)fptr;
- at: (timeval_t)tVal createActionCall: (func_t)fptr:arg1;
- at: (timeval_t)tVal createActionCall: (func_t)fptr:arg1:arg2;
- at: (timeval_t)tVal createActionCall: (func_t)fptr:arg1:arg2:arg3;

- at: (timeval_t)tVal createActionTo: target message: (SEL)aSel;
- at: (timeval_t)tVal createActionTo: target message: (SEL)aSel:arg1;
- at: (timeval_t)tVal createActionTo: target message: (SEL)aSel:arg1:arg2;
- at: (timeval_t)tVal createActionTo: target message: (SEL)aSel:arg1:arg2:arg3;

- at: (timeval_t)tVal createActionForEach: target message: (SEL)aSel;
- at: (timeval_t)tVal createActionForEach: target message: (SEL)aSel:arg1;
- at: (timeval_t)tVal createActionForEach: target message: (SEL)aSel:arg1:arg2;
- at: (timeval_t)tVal createActionForEach: target message: (SEL)aSel:
                                                                arg1:arg2:arg3;
@end


//
// ActionArgs -- messages shared by ActionCall, ActionTo, and ActionForEach
//
@deftype ActionArgs <GetOwner>
- (int)		getNArgs;
- (void)	setArg1: arg1;
-		getArg1;
- (void)	setArg2: arg2;
-		getArg2;
- (void)	setArg3: arg3;
-		getArg3;
@end

//
// ActionCall <ActionArgs>
//
@deftype ActionCall<ActionArgs>
- (void)	setFunctionPointer: (func_t)fptr;
- (func_t)	getFunctionPointer;
@end

//
// ActionTo -- an action defined by sending an Objective C message
//
@deftype ActionTo <ActionArgs>
- (void)	setTarget: target;
-		getTarget;
- (void)	setMessageSelector: (SEL)aSel;
- (SEL)		getMessageSelector;
@end

//
// ActionForEach -- an action defined by sending to every member of a collection
//
@deftype ActionForEach <ActionTo>
@end


//
// Activity -- state of processing within an action plan
//
@deftype Activity <DefinedObject, Drop>
-		getCompound;
-		getEventLog;

- (void)	setVar: aVariableDef to: value;
-		getVar: aVariableDef;
- (void)	setArg: (int)argPos to: value;
-		getArg: (int)argPos;
- (void)	setResult: value;
-		getResult;

-		getVariableValues;

-		run;
-               stop;
- (void)	terminate;

-		next;
-		step;
-		stepEntry;
-		stepExit;

-		getStatus;
-		getHoldType;

-		getRunContext;
- (void)	setOwnerActivity: anActivity;  // unimplemented
-		getOwnerActivity;
-		getSubactivities;

-		getHoldingActions;   // unimplemented
-		getReleasedActions;  // unimplemented

-		setSerialMode: (BOOL)serialMode;
- (BOOL)	getSerialMode;

-		getCurrentAction;       // serial mode only
-		getCurrentSubactivity;  // serial mode only
@end

// values returned by getStatus and getHoldType

extern id <Symbol>  Initialized, Running, Stopped, Holding, Released,
                    Terminated, Completed;
extern id <Symbol>  HoldStart, HoldEnd;

// action plan variable which supplies the activity running an action

extern id  currentActivityVar;


//
// GroupActivity -- state of processing within an ActionGroup
//
@deftype GroupActivity <Activity>
@end

//
// ForEachActivity -- state of execution within a ForEach action
//
@deftype ForEachActivity <Activity>
-		getCurrentMember;
@end

//
// ScheduleActivity -- state of execution within a Schedule
//
@deftype ScheduleActivity <Activity>
-		setTerminateAtEnd: (BOOL)terminateAtEnd;
- (BOOL)	getTerminateAtEnd;

- (void)	setMergeMode: (BOOL)mergeMode;
-		getMergeMode;

- (timeval_t)   getCurrentTime;

-		stepUntil: (timeval_t)tVal;  // unimplemented
- (void)        resetTimes: (timeval_t)tVal;  // unimplemented
@end

//
// SwarmActivity -- a collection of started subactivities
//
@deftype SwarmActivity <ScheduleActivity>
-		getMergingActivities;
@end

extern id <Error>  AlreadyStarted;


//
// Access to execution context of current action.
//
@deftype ActivityMessages  // declare messages used by macros
- (timeval_t)	_getCurrentTime_;
-		_getCurrentScheduleActivity_;
-		_getCurrentSwarmActivity_;
-		_getTopLevelActivity_;
@end

extern id    _activity_current;      // global variable for current activity
extern BOOL (*_activity_trace)(id);  // trace function for activity execution

#define getCurrentActivity() \
_activity_current

#define getCurrentAction() \
[getCurrentActivity() getCurrentAction]

#define getCurrentTime() \
[getCurrentActivity() _getCurrentTime_]

#define getCurrentScheduleActivity() \
[getCurrentActivity() _getCurrentScheduleActivity_]

#define getCurrentSchedule() \
[getCurrentScheduleActivity() getActionPlan]

#define getCurrentSwarmActivity() \
[getCurrentActivity() _getCurrentSwarmActivity_]

#define getCurrentSwarm() \
[getCurrentSwarmActivity()] getSwarm]

#define getCurrentSwarmActivity() \
[getCurrentActivity() _getCurrentSwarmActivity_]

#define getTopLevelActivity() \
[getCurrentActivity() _getTopLevelActivity_]

//
// _activity_argval -- inline function to wrap non-id values as action arguments
//
extern inline id _activity_argval( id argval );
#define V(argval) _activity_argval( argval )

//
// Include automatically generated definitions for this module.
//
#import <activity/types.h>
