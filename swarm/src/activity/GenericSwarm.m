// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         GenericSwarm.m
Description:  object to coordinate a collection of running subactivities
Library:      activity
*/

#import <activity/GenericSwarm.h>


@implementation GenericSwarm_c

PHASE(Creating)

+ createBegin: aZone
{
  GenericSwarm_c  *newPlan;

  newPlan = [aZone allocIVars: self];
  newPlan->zone      = aZone;
  setClass( newPlan, self );
  return newPlan;
}

- (void) setConcurrentGroupType: groupType
{
  concrtGroupType = groupType;
  // setBit( bits, Bit_ConcrntGroupSet, 1 );
}

- (void) setSingletonGroups: (BOOL)singletonGroups
{
  // setBit( bits, Bit_SingletonGroups, singletonGroups );
}

//
// setSwarmPlan -- define startup plan for swarm (create-time only)
//
- (void) setSwarmPlan: aSwarmPlan
{
}

- createEnd
{
  if ( createByMessageToCopy( self, createEnd )  ) return self;

  // if ( variableDefs ) [(id)self _createVarDefsArray_];
  // [(id)self setIndexFromMemberLoc: offsetof( Action_c, ownerActions )];

  if ( ! concrtGroupType ) concrtGroupType = ActivationOrder;
  if ( getClass( self ) == GenericSwarm ) setNextPhase( self );
  return self;
}

PHASE(Setting)

//
// setSwarmObjects: -- define collection of objects comprising state of swarm
//
- (void) setSwarmObjects: aCollection
{
  swarmObjects = aCollection;
}

PHASE(Using)

//
// getSwarmObjects -- get collection of objects comprising state of the swarm
//
- getSwarmObjects
{
  return swarmObjects;
}

- getConcurrentGroupType
{
  return concrtGroupType;
}

- (BOOL) getSingletonGroups
{
  return 0; // bits & Bit_SingletonGroups;
}

//
// getSwarmActivity -- get activity object which runs the swarm subactivities
//
- getSwarmActivity
{
  return swarmActivity;
}

//
// getOwner -- get Swarm object under which this Swarm object is running
//
- getOwner
{
  SwarmActivity_c  *ownerSwarmActivity;

  if ( ! swarmActivity ) return nil;
  ownerSwarmActivity = [swarmActivity _getCurrentSwarmActivity_];
  if ( ! ownerSwarmActivity ) return nil;
  return ownerSwarmActivity->swarm;
}

//
// getSubswarms -- get set of Swarm objects running under this Swarm object
//
- getSubswarms
{
  return subswarms;
}

//
// activateIn: -- create an activity to run plan under swarm or top level
//
- activateIn: swarmContext
{
  id               activityZone, mergeSchedule;
  SwarmActivity_c  *newActivity;

  // get zone in which activities to be created

  activityZone = _activity_zone;
  if ( _activity_current )
    activityZone = ((Activity_c *)_activity_current)->zone;

  // create a special schedule to merge subschedule activities

  if ( 1 /* bits & Bit_ConcrntGroupSet */ ) {
    mergeSchedule = [Schedule createBegin: activityZone];
    [mergeSchedule setConcurrentGroupType: concrtGroupType];
    if ( 0 /* bits & Bit_SingletonGroups */ )
      [mergeSchedule setSingletonGroups: 1];
    mergeSchedule = [mergeSchedule createEnd];
  }

  // create a new swarm activity to process the dynamic merge schedule

  newActivity = [mergeSchedule _activateIn_: swarmContext
                               : id_SwarmActivity_c : id_ScheduleIndex_c];

  [newActivity getStatus];  // force dtable installation

  //
  // link swarm object into new swarm activity and create list for subswarms
  //
  swarmActivity = newActivity;
  newActivity->swarm = self;

  subswarms = [List createBegin: zone];
  [subswarms setIndexFromMemberLoc: offsetof( GenericSwarm_c, memberOfSub )];
  subswarms = [subswarms createEnd];

  return newActivity;
}

//
// drop -- customize drop method
//
- (void) drop
{
  if ( swarmActivity ) [swarmActivity drop];
  [subswarms drop];
  [zone freeIVars: self];

  // if ( ownerSwarm ) [((Swarm_super *)ownerSwarm)->subswarms remove: self];
}

@end


@implementation SwarmActivity_c

//
// terminate -- terminate activity and all its subactivities
//
- (void) terminate
{
  id             index, groupIndex;
  ActionMerge_c  *nextAction, *groupAction;

  // terminate all pending subactivities in the merge schedule

  index = [(id)((Index_any *)currentIndex)->collection begin: scratchZone];
  while ( (nextAction = [index next]) ) {
    if ( getClass( nextAction ) == id_ActionMerge_c ) {
      [nextAction->subactivity terminate];
      [index remove];
    } else {  // concurrent group
      groupIndex =
        [(id)((ActionConcurrent_c *)nextAction)->actionPlan begin: scratchZone];
      while ( (groupAction = [groupIndex next]) ) {
	[groupAction->subactivity terminate];
	[groupIndex remove];
      }
      [groupIndex drop];
    }
  }
  [index drop];

  // terminate running subactivities as well (not in merge schedule when active)

  if ( currentSubactivity ) [currentSubactivity terminate];
  status = Terminated;
}

//
// getSubactivities -- get collection of subactivities being merged 
//
- getSubactivities
{
  return ((Index_any *)currentIndex)->collection;
}

//
// getSwarm -- return swarm object containing this swarm activity, if any
//
- getSwarm
{
  return swarm;
}

//
// getMergingActivities
//
- getMergingActivities
{
  return [self getSubactivities];
}

//
// _drop_ -- special _drop_ method to remove swarm object reference
//
- (void) _drop_
{
  swarm->swarmActivity = nil;
  [super _drop_];
}

@end


//
// ActionMerge_c -- special action to process swarm subactivity
//

@implementation ActionMerge_c

//
// _performAction_: -- perform single step of an activity holding for merge
//
- (void) _performAction_: callerActivity
{
  //
  // Remove merge action from whatever activity is performing it (either a
  // swarm activity or a concurrent group activity within a swarm).
  //
  [((Activity_c *)_activity_current)->currentIndex remove];

  //
  // Return next pending subschedule activity to be run under current owner
  // (either the swarm activity itself, or an subactivity for a concurrent
  // group in the swarm merge schedule).  Start the subschedule activity with
  // a status of Released  so that the action pending at the current index
  // will be executed without an initial nextAction.
  //
  subactivity->ownerActivity = _activity_current;  // owner while sub is active
  subactivity->ownerActivity->currentSubactivity = subactivity;
  subactivity->breakFunction = subactivity->ownerActivity->breakFunction;
  if ( subactivity->status == Holding ) subactivity->status = Released;
  return;
}

@end

//
// ActivationOrder_c -- concurrent group to order merge by activation order
//
@implementation ActivationOrder_c

- (void) addLast: mergeAction
{
  [self at: (id)((ActionMerge_c *)mergeAction)->subactivity->activationNumber
        insert: mergeAction];
}

@end
