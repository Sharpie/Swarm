// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         GenericSwarm.h
Description:  object to coordinate a collection of started activities
Library:      activity
*/

#import <activity/ActionGroup.h>
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
/*** methods implemented in .m file ***/
- createEnd;
+ createBegin: aZone;
- (void) setConcurrentGroupType: groupType;
- (void) setSingletonGroups: (BOOL)singletonGroups;
- createEnd;
- (void) setSwarmObjects: aCollection;
- getSwarmObjects;
- getSwarmActivity;
- getConcurrentGroupType;
- (BOOL) getSingletonGroups;
- getOwner;
- getSubswarms;
- activateIn: swarmContext;
- (void) drop;
@end

@interface SwarmActivity_c : ScheduleActivity_c
{
@public
  GenericSwarm_c  *swarm;          // swarm object for swarm activity
  int             nextActivation;  // next unused activation number
}
/*** methods implemented in .m file ***/
- (void) terminate;
- getSubactivities;
- getSwarm;
- getMergingActivities;
- (void) _drop_;
@end

@interface ActionMerge_c : Action_c
{
@public
  Activity_c  *subactivity;  // activity holding for merge 
}
- (void) _performAction_: anActivity;
@end

@interface ActivationOrder_c : Schedule_c
- (void) addLast: anObject;
@end
