// Swarm library. Copyright � 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         SwarmProcess.m
Description:  object to coordinate a collection of running subactivities
Library:      activity
*/

#import <activity/SwarmProcess.h>
#import <activity/Schedule.h>
#import <defobj/defalloc.h>
#include <misc.h> // abort

#import <defobj/macros.h>
#import <activity/macros.h>

externvar id _activity_swarmSyncType;

@implementation CSwarmProcess

PHASE(Creating)

+ createBegin: aZone
{
  CSwarmProcess *newSwarm;

  newSwarm = [aZone allocIVars: self];
  setMappedAlloc (newSwarm);
  newSwarm->internalZone = Zone;
  setClass (newSwarm, self);
  return newSwarm;
}

- (void)setSynchronizationType: aScheduleType
{
  syncType = aScheduleType;
}

- (void)setInternalZoneType: internalZoneType
{
  internalZone = internalZoneType;
}

- (void)setPageSize: (size_t)pageSize
{
  [internalZone setPageSize: pageSize];
}

- (void)setInternalTimeMultiplier: (timeval_t)internalTimeMultiplier
{
  raiseEvent (NotImplemented, nil);
}

- createEnd
{
  if (createByMessageToCopy (self, createEnd))
    return self;

  if (internalZone)
    internalZone = [internalZone create: getCZone (getZone (self))];

  if (!syncType)
    syncType = _activity_swarmSyncType;

  if (getClass (self) == SwarmProcess)
    setNextPhase (self);
  return self;
}

PHASE(Using)

//
// getInternalTimeMultiplier -- return time multiplier for internal time values
//
- (timeval_t)getInternalTimeMultiplier
{
  return 1;
}

//
// getInternalZone -- return zone created to hold internal population of swarm
//
- getInternalZone
{
  return internalZone;
}

//
// getActivity --
//   return activity object that is running the subprocesses of the swarm
//
- (id <SwarmActivity>)getActivity
{
  return activity;
}

//
// getSwarmActivity -- old name still supported as synonym for getActivity
//
- (id <SwarmActivity>)getSwarmActivity
{
  return activity;
}

- getSynchronizationType
{
  return syncType;
}

//
// activate -- activate to run as a top-level, externally controllable process
//
- (id <Activity>)activate
{
  return [self activateIn: nil];
}

//
// activate: --
//   activate an action type to run as a subprocess controlled by the swarm
//
- (id <Activity>)activate: anActionType
{
  [anActionType activateIn: self->activity];
  return nil;  //!! later -- a special action for the whole activity
}

//
// at:activate: -- activate an action type at a particular time
//
- at: (timeval_t)tVal activate: anActionType
{
  raiseEvent (NotImplemented, nil);
  abort ();
}

//
// at::activate: --
//   activate an action type a particular time in a particular timebase
//
- at: (int)timebase : (timeval_t)tVal activate: anActionType
{
  raiseEvent (NotImplemented, nil);
  abort ();
}

//
// notifySwarm() -- function to notify swarm on completion of its activity
//
static void
notifySwarm (id anObject, id realloc, CSwarmProcess *swarm)
{
  if (!realloc )
    swarm->activity = nil;
  else
    {
      // (no reallocation implemented yet)
    }
}

//
// dropSwarmActivity() -- function to drop swarm activity on drop of swarm
//
static void
dropSwarmActivity (CSwarmProcess *swarm, id realloc, id unusedArg)
{
  if (!realloc)
    {
      if (swarm->activity)
        [swarm->activity drop];
    }
  else
    {
      // (no reallocation implemented yet)
    }
}

//
// activateIn: -- create an activity to run plan under swarm or top level
//
- (id <Activity>)activateIn: swarmContext
{
  id <Zone> activityZone;
  Schedule_c *mergeSchedule;

  // make sure that not already activated

  if (activity)
    raiseEvent (InvalidOperation,
                "> Swarm has already been activated.  A swarm cannot be activated more than\n"
                "> once.\n");
  
  // get zone in which activities to be created
  
  activityZone = swarmContext ? [swarmContext getZone] : _activity_zone;
  
  // create a special schedule to merge subschedule activities
  
  mergeSchedule = [syncType create: [activityZone getComponentZone]];
  
  // create a new swarm activity to process the dynamic merge schedule
  
  activity = [mergeSchedule _activateIn_: swarmContext
                            : id_SwarmActivity_c 
                            : id_ScheduleIndex_c
                            : activityZone];
  activity->swarm = self;
  activity->status = Initialized;
  
  // arrange to remove local activity reference on completion of activity
  
  [activity addRef: (notify_t) notifySwarm withArgument: self];
  
  // arrange to drop activity on drop of swarm object
  
  [self addRef: (notify_t) dropSwarmActivity withArgument: nil];

  return activity;
}

//
// mapAllocations: -- standard method to identify internal allocations
//
- (void) mapAllocations: (mapalloc_t)mapalloc
{
  if (internalZone)
    mapObject (mapalloc, internalZone);
}

//
// _performPlan_ -- create an activity to run plan under the current activity
//
- (void)_performPlan_
{
  Activity_c *newActivity;

  newActivity = [self activateIn: nil];
  newActivity->ownerActivity = _activity_current;
  newActivity->ownerActivity->currentSubactivity = newActivity;
}

//
// pass-through messages to internal zone
//

#define ZMSG_R(msg) \
{ if ( internalZone ) return [internalZone msg]; \
  else { raiseEvent( InvalidSwarmZone, 0 ); abort (); } }

#define ZMSG_V(msg) \
{ if ( internalZone ) [internalZone msg]; \
  else raiseEvent( InvalidSwarmZone, 0 ); }

- (size_t)getPageSize                ZMSG_R(getPageSize)
- allocIVars: aClass                 ZMSG_R(allocIVars: aClass)
- copyIVars: anObject                ZMSG_R(copyIVars: anObject)
- (void)freeIVars: anObject          ZMSG_V(freeIVars: anObject)
- allocIVarsComponent: aClass        ZMSG_R(allocIVarsComponent: aClass)
- copyIVarsComponent: anObject       ZMSG_R(copyIVarsComponent: anObject)
- (void)freeIVarsComponent: anObject ZMSG_V(freeIVarsComponent: anObject)
#if 0
- getComponentZone                   ZMSG_R(getComponentZone)
#else
- getComponentZone                   { abort (); return nil; }
#endif
- (void *)alloc: (size_t)size        ZMSG_R(alloc: size)
- (void) free: (void *) aBlock       ZMSG_V(free: aBlock)
- (void *)allocBlock: (size_t)size   ZMSG_R(allocBlock: size)
- (void)freeBlock: (void *)aBlock blockSize: (size_t)size
                                     ZMSG_V( freeBlock: aBlock blockSize: size)
- getPopulation                      ZMSG_R( getPopulation)
- (void)describeForEach: outputCharStream  
                                     ZMSG_V(describeForEach: outputCharStream)
- (void)describeForEachID: outputCharStream 
                                     ZMSG_V(describeForEachID: outputCharStream)
- getReclaimPolicy                   ZMSG_R(getReclaimPolicy)
- (BOOL)getStackedSubzones           ZMSG_R(getStackedSubzones)

@end


@implementation SwarmActivity_c
PHASE(Creating)
PHASE(Using)
//
// terminate -- terminate activity and all its subactivities
//
- (void)terminate
{
  id index, groupIndex;
  ActionMerge_c *nextAction, *groupAction;

  // terminate all pending subactivities in the merge schedule

  index = [(id) ((ScheduleIndex_c *) currentIndex)->collection begin: scratchZone];
  while ((nextAction = [index next]))
    {
      if (getClass (nextAction) == id_ActionMerge_c)
        [nextAction->subactivity terminate];
      else
        {
          // concurrent group
          groupIndex = [(id) ((ActionConcurrent_c *) nextAction)->concurrentGroup
                             begin: scratchZone];
          while ((groupAction = [groupIndex next]))
            [groupAction->subactivity terminate];
          DROP (groupIndex);
        }
    }
  DROP (index);
  
  // terminate running subactivities also (not in merge schedule when active)
  
  if (currentSubactivity)
    [currentSubactivity terminate];
  status = Terminated;
}

//
// getSubactivities -- get collection of subactivities being merged 
//
- getSubactivities
{
  return ((ScheduleIndex_c *) currentIndex)->collection;
}

//
// getSwarm -- return swarm object containing this swarm activity, if any
//
- getSwarm
{
  return swarm;
}

//
// getSynchronizationSchedule
//
- (id <Schedule>)getSynchronizationSchedule
{
  return (Schedule_c *) ((ScheduleIndex_c *) currentIndex)->collection;
}

//
// mapAllocations: -- standard method to map internal allocations
//
- (void)mapAllocations: (mapalloc_t)mapalloc
{
  id mergeSchedule = ((ScheduleIndex_c *) currentIndex)->collection;

  [super mapAllocations: mapalloc];
  mapObject (mapalloc, mergeSchedule);
}

@end


//
// ActionMerge_c -- special action to process swarm subactivity
//

@implementation ActionMerge_c
PHASE(Creating)
PHASE(Using)
//
// _performAction_: -- perform single step of an activity holding for merge
//
- (void)_performAction_: callerActivity
{
  //
  // Remove merge action from whatever activity is performing it (either a
  // swarm activity or a concurrent group activity within a swarm).
  //
  SCHEDULE_INDEX_REMOVE (((Activity_c *) _activity_current)->currentIndex);

  //
  // Return next pending subschedule activity to be run under current owner
  // (either the swarm activity itself, or an subactivity for a concurrent
  // group in the swarm merge schedule).  Start the subschedule activity with
  // a status of Released so that the action pending at the current index
  // will be executed without an initial nextAction.
  //
  subactivity->ownerActivity = _activity_current;  // owner while sub is active
  subactivity->ownerActivity->currentSubactivity = subactivity;
  subactivity->breakFunction = subactivity->ownerActivity->breakFunction;
  if (HOLDINGP (subactivity->status))
    subactivity->status = Released;
  return;
}

//
// mapAllocations: -- standard method to map internal allocations
//
- (void)mapAllocations: (mapalloc_t)mapalloc
{
#if 0
  //
  // mergeAction = nil -- special hack to break circular mapping for the
  // specific case of drop until a more general approach is resolved
  //
  subactivity->mergeAction = nil;
  mapObject (mapalloc, subactivity);
#endif
}

- (void)describe: stream
{
  char buffer[100];

  [stream catC: "["];
  _obj_formatIDString (buffer, self);
  [stream catC: buffer];
  [stream catC: " in schedule: "];
  _obj_formatIDString (buffer, collectionOfActions);
  [stream catC: buffer];
  [stream catC: " subactivity: "];
  _obj_formatIDString (buffer, subactivity);
  [stream catC: buffer];
  if ([subactivity conformsTo: @protocol (SwarmActivity)])
    {
      id swarm = [(id) subactivity getSwarm];

      [stream catC: " in Swarm "];
      [stream catPointer: swarm];
      if (swarm)
        {        
          [stream catC: " ("];
          [stream catC: [swarm getName]];
          [stream catC: ")\n"];
        }
    }
  else
    [stream catC: " non-Swarm ScheduleActivity\n"];
  [stream catC: "]\n"];
}

@end

