// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         GenericSwarm.h
Description:  object to coordinate a collection of started activities
Library:      activity
*/

#import <activity/Schedule.h>
#import <activity/Activity.h>

@class SwarmActivity_c;

@interface GenericSwarm_c : CreateDrop_s
{
@public
  id               swarmObjects;     // objects comprising state of the Swarm
  SwarmActivity_c  *swarmActivity;   // activity running swarm subactivities
  id               subswarms;        // swarm objects running within swarm
  member_t         memberOfSub;      // membership link in owner subswarms list
  id               concrtGroupType;  // type for group of actions at same time
}
/*** methods in GenericSwarm_c (inserted from .m file) ***/
+ createBegin: aZone;
- (void) setConcurrentGroupType: groupType;
- (void) setSingletonGroups: (BOOL)singletonGroups;
- (void) setSwarmPlan: aSwarmPlan;
- createEnd;
- (void) setSwarmObjects: aCollection;
- getSwarmObjects;
- getConcurrentGroupType;
- (BOOL) getSingletonGroups;
- getSwarmActivity;
- getOwner;
- getSubswarms;
- activateIn: swarmContext;
- (void) mapAllocations: (mapalloc_t)mapalloc;
- (void) _performPlan_;
@end

@interface SwarmActivity_c : ScheduleActivity_c
{
@public
  GenericSwarm_c  *swarm;          // swarm object for swarm activity
  int             nextActivation;  // next unused activation number
}
/*** methods in SwarmActivity_c (inserted from .m file) ***/
- (void) terminate;
- getSubactivities;
- getSwarm;
- getMergingActivities;
- (void) mapAllocations: (mapalloc_t)mapalloc;
- (void) dropAllocations: (BOOL)componentAlloc;
@end

@interface ActionMerge_c : CAction
{
@public
  ScheduleActivity_c  *subactivity;  // activity holding for merge 
}
/*** methods in ActionMerge_c (inserted from .m file) ***/
- (void) _performAction_: callerActivity;
- (void) mapAllocations: (mapalloc_t)mapalloc;
@end

@interface ActivationOrder_c : Schedule_c
/*** methods in ActivationOrder_c (inserted from .m file) ***/
- (void) addLast: mergeAction;
- (void) mapAllocations: (mapalloc_t)mapalloc;
@end
