// Swarm library. Copyright (C) 1996-1997-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         activity.h
Description:  processing control over all levels of swarm execution 
Library:      activity
*/

#import <collections.h>

//
// ProcessType -- specification of a process
//
// ProcessType will eventually define support for parameterization of all
// processes.  A process is a uniquely identified course of events that
// conforms to a specification of external and internal behavior.
//
// Processes include both objects and executable actions.  Objects change
// their state and behavior based on a sequence of externally initiated
// actions.  An action, in contrast, typically has no external behavior
// other than to be executed in its entirety as a unit (yielding either an
// observable result or some change of state in a surrounding environment).
//
// Parameterization will be based a uniform framework that defines all input
// parameters, internal state variables, and/or final results, and that
// provides for binding of these data in a context of execution.  This
// parameterization framework is still being established, but ProcessType
// objects will eventually record the specification of any process, to the
// extent that this specification is defined.
//
@deftype ProcessType
@end

//
// ActionType -- specification of an executable process
//
// An action type is a type of process that may be initiated as a unit of
// execution by an external request.  A typical action has a well-defined
// duration determined by a fixed set of actions that execute within it.
// Externally initiated interaction typically occurs only at the start or
// end of the overall process.  A typical action is executed in its entirety
// once an external request that initiates it has occurred.  Some actions may
// also have internal events that cannot begin or complete until other actions
// from a containing environment have also begun or completed their execution.
// Such ordering constraints can be defined either within an action type or
// as part of a dynamic context of execution.
//
// Executable actions include both actions compiled in a host language (such
// as C functions or Objective C messages) and compound actions built at
// runtime for interpretation by the Swarm abstract machine.
//
@deftype ActionType
-		activateIn: swarmContext;
@end


//
// CompoundAction --
//   a collection of actions to be performed in any order consistent with a
//   set of ordering constraints
//
@deftype CompoundAction <ActionType, Collection>
CREATING
- (void)	setDefaultOrder: aSymbol;
- (void)	setAutoDrop: (BOOL)autoDrop;
USING
-		getDefaultOrder;
- (BOOL)	getAutoDrop;
@end

// values for DefaultOrder
extern id <Symbol>  Concurrent, Sequential, Randomized;

//
// ActionCreating -- protocol shared by ActionGroup and Schedule
//
@deftype ActionCreating
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
@end

//
// ActionGroup --
//   a collection of actions under total or partial order constraints
//
@deftype ActionGroup <CompoundAction, ActionCreating, OrderedSet, CREATABLE>
@end

//
// timeval_t, TimebaseMax --
//   type of a time value, and maximum value of a value of type timeval_t
//
#ifndef DEFINED_timeval_t
#define DEFINED_timeval_t
typedef unsigned long timeval_t;
#endif
extern const timeval_t  TimebaseMax;

//
// Schedule -- collection of actions ordered by time values
//
@deftype Schedule <CompoundAction, ActionCreating, Map, CREATABLE>
CREATING
-		create: aZone setRelativeTime: (BOOL)relativeTime;
-		create: aZone setRepeatInterval: (timeval_t)repeatInterval;
SETTING
- (void)	setConcurrentGroupType: groupType;
- (void)	setSingletonGroups: (BOOL)singletonGroups;

- (void)	setRelativeTime: (BOOL)relativeTime;
- (void)	setRepeatInterval: (timeval_t)repeatInterval;
USING
-		getConcurrentGroupType;
- (BOOL)	getSingletonGroups;

- (BOOL)	getRelativeTime;
- (timeval_t)	getRepeatInterval;

- at: (timeval_t)tVal createAction: anActionType;

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
// SwarmProcess -- an object that holds a collection of concurrent subprocesses
//
@deftype SwarmProcess <ActionType, Zone, CREATABLE>
CREATING
-		setInternalZoneType: aZoneType;
- (void)	setSynchronizationType: aScheduleType;

- (void)	setInternalTimeMultiplier: (timeval_t)multiplier;
USING
-		getSynchronizationType;
- (timeval_t)	getInternalTimeMultiplier;

-		getInternalZone;
-		getActivity;
@end


//
// Action --
//   an action type that has been customized for direct execution by an
//   action interpreter
//
@deftype Action <GetOwner>
-		getActionType;
@end

//
// ActionArgs -- supertype of ActionCall, ActionTo, and ActionForEach
//
// The ActionArgs subtypes all implement a specific, hard-coded method for
// binding an action type to a fixed number of arguments.  All the arguments
// must have types compatible with id type.  Eventually, more generic methods
// for binding an action type to any number and types of arguments and return
// values will also be provided.
//
@deftype ActionArgs <Action>
- (int)		getNArgs;
- (void)	setArg1: arg1;
-		getArg1;
- (void)	setArg2: arg2;
-		getArg2;
- (void)	setArg3: arg3;
-		getArg3;
@end

//
// ActionCall -- an action defined by calling a C function
//
@deftype ActionCall <ActionArgs>
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
// ActionForEach --
//   an action defined by sending a message to every member of a collection
//
@deftype ActionForEach <ActionTo>
@end


//
// Activity -- a level of processing by the interpreter of an action type
//
@deftype Activity <DefinedObject, Drop>
-		run;
-               stop;
- (void)	terminate;

-		next;
-		step;
-		stepEntry;
-		stepExit;

-		getStatus;
-		getHoldType;

-		getAction;
-		getActionType;

- (void)	setOwnerActivity: ownerActivity;
-		getOwnerActivity;

-		getSubactivities;

-		getControllingActivity;
-		getTopLevelActivity;
-		getSwarmActivity;
-		getScheduleActivity;

-		setSerialMode: (BOOL)serialMode;
- (BOOL)	getSerialMode;

-		getCurrentSubactivity;  // serial mode only
@end

// values returned by getStatus and getHoldType

extern id <Symbol>  Initialized, Running, Holding, Released,
                    Stopped, Terminated, Completed;
extern id <Symbol>  HoldStart, HoldEnd;


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

- (void)	setSynchronizedMode: (BOOL)synchronizedMode;
-		getSynchronizedMode;

- (timeval_t)   getCurrentTime;

-		stepUntil: (timeval_t)tVal;
@end

//
// SwarmActivity -- a collection of started subactivities
//
@deftype SwarmActivity <ScheduleActivity>
-		getSwarm;
-		getSynchronizationSchedule;
- (int)		getCurrentTimebase;
@end

//
// Macros to access to the current context in which an action is executing
//

#define getCurrentSwarm() \
({ id swarmActivity; \
( _activity_current && \
  (swarmActivity = [_activity_current getSwarmActivity]) ? \
[swarmActivity getActionType] : \
_activity_context_error( "getCurrentSwarm" ) ); })

#define getCurrentSchedule() \
({ id scheduleActivity; \
( ( _activity_current & \
    (scheduleActivity = [_activity_current getScheduleActivity]) ) ? \
[scheduleActivity getActionType] : \
_activity_context_error( "getCurrentSchedule" ) ); })

#define getCurrentTime() \
({ id scheduleActivity; \
( _activity_current && \
  (scheduleActivity = [_activity_current getScheduleActivity]) ? \
[scheduleActivity getCurrentTime] : \
(timeval_t)_activity_context_error( "getCurrentTime" ) ); })

#define getTopLevelActivity() \
( _activity_current ? [_activity_current getTopLevelActivity] : \
_activity_context_error( "getTopLevelActivity" ) )

#define getCurrentSwarmActivity() \
( _activity_current ? [_activity_current getSwarmActivity] : \
_activity_context_error( "getCurrentSwarmActivity" ) )

#define getCurrentScheduleActivity() \
( _activity_current ? [_activity_current getScheduleActivity] : \
_activity_context_error( "getCurrentScheduleActivity" ) )

#define getCurrentOwnerActivity() \
( _activity_current ? _activity_current : \
_activity_context_error( "getCurrentOwnerActivity" ) )

#define getCurrentAction() \
( _activity_current ? [_activity_current _getSubactivityAction_] : \
_activity_context_error( "getCurrentAction" ) )

@deftype GetSubactivityAction
-	_getSubactivityAction_;  // internal method for getCurrentAction()
@end

#define getCurrentActivity() \
( _activity_current ? [_activity_current getCurrentSubactivity] : nil )

//
// _activity_current, _activity_context_error() --
//   internal definitions used by current context macros
//
extern id  _activity_current;
extern id  _activity_context_error( char *macroName );


//
// ConcurrentGroup -- default type used as concurrent group of a schedule
//
@deftype ConcurrentGroup <ActionGroup, CREATABLE>
@end

//
// ConcurrentSchedule -- time-based map usable for concurrent group
//
@deftype ConcurrentSchedule <ActionGroup, CREATABLE>
@end

//
// ActivationOrder -- default type used as concurrent group of a swarm
//
@deftype ActivationOrder <ActionGroup, CREATABLE>
@end

//
// error symbols
//
extern id <Error>  InvalidSwarmZone;

//
// _activity_zone -- global variable for zone in which activity objects created
//
extern id  _activity_zone;

//
// _activity_trace --
//   global variable for function to be called on every change in the
//   activity tree
//
// Note: support for any specific form of this trace facility is not guaranteed
// in future versions.  Some form of trace facility will remain for debug
// purposes, however, at least until a full form of event history logging has
// been implemented as an integral part of the Activity type.
// 
extern BOOL (*_activity_trace)(id);  // trace function for activity execution

//
// ActivityCompatibility --
//   obsolete or future messages still supported during a period of transition
//
@deftype ActivityCompatibility
-		activate;
-		getSwarmActivity;
-		activate: anActionType;
-		at: (timeval_t)tVal activate: anActionType;
-		at: (int)timebase : (timeval_t)tVal activate: anActionType;
@end

//
// include automatically generated definitions for activity package
//
#import <activity/types.h>
