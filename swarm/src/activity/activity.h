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
// ActionType -- specification of a executable process
//
// ActionType will eventually consolidate support for parameterization
// of all actions, including both actions compiled in the host language
// (C functions and Objective C messages) and compound actions built at
// runtime for interpretation by the Swarm abstract machine.
//
// Parameterization of actions will be based a uniform framework that
// defines all input parameters, internal state variables, and/or return
// results, and that provides for binding of these data in the context of
// execution.  The Action type itself defines only those actions that have
// not yet been bound for execution in any specific context.  Once a specific
// context of execution has been bound, a new object of type Action is
// is created that records the specific context of its binding.
//
// For now, the parameterization framework is still being established, but
// the Action type is already used to refer to the common abstraction of an
// executable action, whether compiled or interpreted.
//
@deftype ActionType
-		activateIn: processContext;
@end

//
// CompoundAction --
//   a collection of actions to be performed
//   in any order that is consistent with a set of ordering constraints
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
// ActionCreating, TimeIndexing --
//   "mixin" protocols included by the creatable types defined below
//

//
// ActionCreating -- standard messages to create actions within an action plan
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
// TimeIndexing -- messages shared by Schedule and Swarm
//
@deftype TimeIndexing
CREATING
+		create: aZone setRepeatInterval: (timeval_t)tVal;

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
// (The Generic prefix on this name avoids conflict with the Swarm superclass
// from which user implementations of swarm behavior must currently inherit.)
//
@deftype GenericSwarm <ActionType, TimeIndexing, Zone, CREATABLE>
SETTING
-		setPopulation: aCollection;
USING
-		getPopulation;
-		getSubswarms;

-		getCurrentActivity;
@end

//
// ActionGroup --
//   a collection of actions under total or partial order constraints
//
@deftype ActionGroup <CompoundAction, ActionCreating, OrderedSet, CREATABLE>
@end

//
// ActivationOrder -- default concurrent group type for swarm
//
@deftype ActivationOrder <ActionGroup, CREATABLE>
@end

//
// ConcurrentGroup -- default concurrent group type for schedule
//
@deftype ConcurrentGroup <ActionGroup, CREATABLE>
@end

//
// Schedule -- collection of actions ordered by time values
//
@deftype Schedule <CompoundAction, TimeIndexing, ActionCreating, Map,
                   CREATABLE>
CREATING
- (void)	create: aZone setRepeatInterval: (timeval_t)repeatInterval;
- (void)	setRelativeTime: (BOOL)relative;
USING
- (BOOL)	getRelativeTime;

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
// Action -- a binding of an action type for execution in some context
//
@deftype Action <GetOwner>
-		getActionType;
@end

//
// ActionArgs -- supertype of ActionCall, ActionTo, and ActionForEach
//
// The ActionArgs subtypes all implement a specific, hard-coded method for
// binding an action to a fixed number of arguments that all have types
// compatible with id type.  Eventually, more generic forms of binding for
// any type of action along with its arguments and return values will also
// be provided.
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
// Activity -- state of processing within an action type
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

-		getActionType;
-		getAction;

- (void)	setOwnerActivity: ownerActivity;
-		getOwnerActivity;

-		getControllingActivity;
-		getTopLevelActivity;
-		getSwarmActivity;
-		getScheduleActivity;

-		getSubactivities;

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

-		stepUntil: (timeval_t)tVal;   // unimplemented
- (void)        shiftTimes: (timeval_t)tVal;  // unimplemented
@end

//
// SwarmActivity -- a collection of started subactivities
//
@deftype SwarmActivity <ScheduleActivity>
-		getSynchronizedSubactivities;
@end


//
// Macros to access to the current context in which an action is executing
//

#define getCurrentSwarm() \
({ id swarmActivity; \
 if ( ! _activity_current || \
   ! (swarmActivity = [_activity_current getSwarmActivity]) ) \
   _activity_context_error( "getCurrentSwarm" ); \
 [swarmActivity getActionType]; })

#define getCurrentSchedule() \
({ id scheduleActivity; \
 if ( ! _activity_current || \
    ! (scheduleActivity = [_activity_current getScheduleActivity]) ) \
  _activity_context_error( "getCurrentSchedule" ); \
 [scheduleActivity getActionType]; })

#define getCurrentTime() \
({ id scheduleActivity; \
 if ( ! _activity_current || \
  ! (scheduleActivity = [_activity_current getScheduleActivity]) ) \
  _activity_context_error( "getCurrentTime" ); \
 [scheduleActivity getCurrentTime]; })

#define getCurrentAction() \
 ( _activity_current ? [_activity_current getAction] : \
 _activity_context_error( "getCurrentAction" ) )

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

#define getCurrentActivity() \
 ( _activity_current ? [_activity_current getCurrentSubactivity] : \
 _activity_context_error( "getCurrentActivity" ) )

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
// internal support declarations
//

//
// _activity_current -- global variable containing the current activity
//
extern id  _activity_current;

//
// _activity_context_error() --
//   function to generate error message if invalid context query
//
extern id _activity_context_error( char *macroName );

//
// include automatically generated definitions for activity package
//
#import <activity/types.h>

//
// temporary access to current action from owner activity
//
@deftype GetSubaction
-		getSubaction;
@end
