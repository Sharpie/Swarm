// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
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

extern id _activity_swarmSyncType;

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

- (void) setSynchronizationType: aScheduleType
{
  syncType = aScheduleType;
}

- (void) setInternalZoneType: internalZoneType
{
  internalZone = internalZoneType;
}

- (void) setInternalTimeMultiplier: (timeval_t)internalTimeMultiplier
{
  raiseEvent (NotImplemented, nil);
}

- createEnd
{
  if (createByMessageToCopy( self, createEnd))
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
- getActivity
{
  return activity;
}

//
// getSwarmActivity -- old name still supported as synonym for getActivity
//
- getSwarmActivity
{
  return activity;
}

//
// activate -- activate to run as a top-level, externally controllable process
//
- activate
{
  return [self activateIn: nil];
}

//
// activate: --
//   activate an action type to run as a subprocess controlled by the swarm
//
- activate: anActionType
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
  exit (0);
}

//
// at::activate: --
//   activate an action type a particular time in a particular timebase
//
- at: (int)timebase : (timeval_t)tVal activate: anActionType
{
  raiseEvent (NotImplemented, nil);
  exit (0);
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
static void dropSwarmActivity( CSwarmProcess *swarm, id realloc,
                               id unusedArg )
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
- activateIn: swarmContext
{
  id activityZone, mergeSchedule;

  // make sure that not already activated

  if (activity)
    raiseEvent (InvalidOperation,
                "> Swarm has already been activated.  A swarm cannot be activated more than\n"
                "> once.\n");
  
  // get zone in which activities to be created
  
  activityZone = (_activity_current
                  ? getZone ((Activity_c *) _activity_current)
                  : _activity_zone);
  
  // create a special schedule to merge subschedule activities
  
  mergeSchedule = [syncType create: [activityZone getComponentZone]];
  
  // create a new swarm activity to process the dynamic merge schedule
  
  activity = [mergeSchedule _activateIn_: swarmContext
                            : id_SwarmActivity_c : id_ScheduleIndex_c];
  activity->swarm = self;
  
  // arrange to remove local activity reference on completion of activity
  
  [activity addRef: (notify_t)notifySwarm withArgument: self];
  
  // arrange to drop activity on drop of swarm object
  
  [self addRef: (notify_t)dropSwarmActivity withArgument: nil];
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
  else { raiseEvent( InvalidSwarmZone, 0 ); exit(0); } }

#define ZMSG_V(msg) \
{ if ( internalZone ) [internalZone msg]; \
  else raiseEvent( InvalidSwarmZone, 0 ); }

- (int)getPageSize                   ZMSG_R(getPageSize)
- allocIVars: aClass                 ZMSG_R(allocIVars: aClass)
- copyIVars: anObject                ZMSG_R(copyIVars: anObject)
- (void)freeIVars: anObject          ZMSG_V(freeIVars: anObject)
- allocIVarsComponent: aClass        ZMSG_R(allocIVarsComponent: aClass)
- copyIVarsComponent: anObject       ZMSG_R(copyIVarsComponent: anObject)
- (void)freeIVarsComponent: anObject ZMSG_V(freeIVarsComponent: anObject)
- getComponentZone                   ZMSG_R(getComponentZone)
- (void *)alloc: (size_t)size        ZMSG_R(alloc: size)
- (void) free: (void *) aBlock       ZMSG_V(free: aBlock)
- (void *)allocBlock: (size_t)size   ZMSG_R(allocBlock: size)
- (void)freeBlock: (void *)aBlock blockSize: (size_t)size
                                     ZMSG_V( freeBlock: aBlock blockSize: size)
- getPopulation                      ZMSG_R( getPopulation)

@end


@implementation SwarmActivity_c

//
// terminate -- terminate activity and all its subactivities
//
- (void)terminate
{
  id index, groupIndex;
  ActionMerge_c *nextAction, *groupAction;

  // terminate all pending subactivities in the merge schedule

  index = [(id) ((Index_any *) currentIndex)->collection begin: scratchZone];
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
          [groupIndex drop];
        }
    }
  [index drop];
  
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
  return ((Index_any *) currentIndex)->collection;
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
- getSynchronizationSchedule
{
  return ((Index_any *) currentIndex)->collection;
}

//
// mapAllocations: -- standard method to map internal allocations
//
- (void) mapAllocations: (mapalloc_t)mapalloc
{
  id mergeSchedule;

  mergeSchedule = ((ScheduleIndex_c *) currentIndex)->collection;
  [super mapAllocations: mapalloc];
  mapObject (mapalloc, mergeSchedule);
}

@end


//
// ActionMerge_c -- special action to process swarm subactivity
//

@implementation ActionMerge_c

//
// _performAction_: -- perform single step of an activity holding for merge
//
- (void)_performAction_: callerActivity
{
  //
  // Remove merge action from whatever activity is performing it (either a
  // swarm activity or a concurrent group activity within a swarm).
  //
  [((Activity_c *) _activity_current)->currentIndex remove];

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
  subactivity->immediateReturnFlag = immediateReturnRequestFlag;
  immediateReturnRequestFlag = 0; 
  if (HOLDINGP (subactivity->status))
    subactivity->status = Released;
  return;
}

//
// mapAllocations: -- standard method to map internal allocations
//
- (void)mapAllocations: (mapalloc_t)mapalloc
{
  //
  // mergeAction = nil -- special hack to break circular mapping for the
  // specific case of drop until a more general approach is resolved
  //
  subactivity->mergeAction = nil;
  mapObject (mapalloc, subactivity);
}

- (void)describe: outputCharStream
{
  char buffer[100];

  [outputCharStream catC: "["];
  _obj_formatIDString (buffer, collectionOfActions);
  [outputCharStream catC: buffer];
  [outputCharStream catC: " (merge into swarm)]\n"];
}

@end

