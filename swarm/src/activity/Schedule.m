// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         Schedule.m
Description:  collection of actions ordered by time values 
Library:      activity
*/

#import <activity/Schedule.h>
#import <activity/ActionGroup.h>
#import <defobj/defalloc.h>

@implementation Schedule_c

PHASE(Creating)

//
// mix in action plan create-phase methods by source inclusion
//
#define  MIXIN_CREATE
#include "CompoundAction.m"

//
// create:setRepeatInterval: -- convenience create message
//
+ create: aZone setRepeatInterval: (timeval_t)rptInterval
{
  id  new;

  new = [self createBegin: aZone];
  [new setRepeatInterval: rptInterval];
  return [new createEnd];
}

- (void)setConcurrentGroupType: groupType
{
  concurrentGroupType = groupType;
}

- (void)setSingletonGroups: (BOOL)singletonGroups
{
  setBit (bits, BitSingletonGroups, singletonGroups);
}

- (void)setRelativeTime: (BOOL)relativeTime
{
  setBit (bits, BitRelativeTime, relativeTime);
  setBit (bits, BitRelTimeSet, 1);
}

- createEnd
{
  if (repeatInterval)
    {
      if ((bits & BitRelTimeSet) && !(bits & BitRelativeTime))
        raiseEvent (InvalidCombination,
                    "> cannot specify both a repeat interval and absolute time\n");
      
    setBit (bits, BitRelativeTime, 1);  // force relative time for repeat
  }
  [(id) self setCompareFunction: compareIDs];

  if (createByMessageToCopy (self, createEnd))
    return self;

  if (!concurrentGroupType)
    concurrentGroupType = ConcurrentGroup;
  [super createEnd];

  return self;
}

PHASE(Setting)

- (void)setRepeatInterval: (timeval_t)rptInterval
{
  if (rptInterval == 0)
    raiseEvent (InvalidArgument,
                "> repeat interval must be greater than zero\n");

  if (!getNextPhase (getClass (self)) && !(bits & BitRelativeTime))
    raiseEvent (InvalidCombination,
                "> cannot specify a repeat interval after schedule created without it\n" );

  repeatInterval = rptInterval;
}

PHASE(Using)

//
// mix in action plan finalized instance methods by source inclusion
//
#define  MIXIN_C

// override parameters for included start: method
#define  ACTIVITY_CLASS     ScheduleActivity_c
#define  ACTIVITY_CLASS_ID  id_ScheduleActivity_c
#define  INDEX_CLASS        ScheduleIndex_c
#define  INDEX_CLASS_ID     id_ScheduleIndex_c

#include "CompoundAction.m"

- getConcurrentGroupType
{
  return concurrentGroupType;
}

- (BOOL)getSingletonGroups
{
  return bits & BitSingletonGroups;
}

- (BOOL)getRelativeTime
{
  return bits & BitRelativeTime;
}

- (timeval_t)getRepeatInterval
{
  return repeatInterval;
}

//
// _activateUnderSwarm_::: -- release new activity to run under swarm
//
- _activateUnderSwarm_: activityClass : indexClass : swarmContext
{
  SwarmActivity_c     *swarmActivity;
  ScheduleActivity_c  *newActivity;
  ScheduleIndex_c     *newIndex, *swarmIndex;
  ActionMerge_c       *mergeAction, *mergeExternalAction;

  // initialize new activity to run underneath swarm

  swarmActivity = swarmContext;
  newActivity =
    [self _createActivity_: swarmActivity : activityClass : indexClass];
  newActivity->ownerActivity = nil;
  newActivity->swarmActivity = swarmActivity;
  newActivity->activationNumber = swarmActivity->nextActivation++;

  // create merge action for use by swarm in merging schedule subactivities

  newIndex = newActivity->currentIndex;
  mergeAction =
    [getZone (swarmActivity) allocIVarsComponent: id_ActionMerge_c];
  setMappedAlloc (mergeAction);
  mergeAction->subactivity = newActivity;
  mergeAction->collectionOfActions = self;
  mergeAction->immediateReturnRequestFlag = 0;
  newActivity->mergeAction = mergeAction;

  mergeExternalAction =
    [getZone (swarmActivity) allocIVarsComponent: id_ActionMerge_c];
  setMappedAlloc (mergeExternalAction);
  newActivity->mergeExternalAction = mergeExternalAction;
  mergeExternalAction->subactivity = newActivity;
  mergeExternalAction->immediateReturnRequestFlag = 0;
  newActivity->mergeExternalAction = mergeExternalAction;

  // set the starting and current times of the new activity

  swarmIndex = swarmActivity->currentIndex;
  newIndex->startTime = swarmIndex->currentTime;

  // Advance the new index to its first action. The nextAction: message
  // will automatically insert the merge action at schedule's pending time.

  [newIndex nextAction: (id *) &newActivity->status];

  return newActivity;
}

//
// createGroup() -- function to create new group for concurrent actions
//
static ActionConcurrent_c *
createGroup (Schedule_c *self)
{
  id newGroup;
  ActionConcurrent_c  *newAction;
  id zone = getZone (self);

  // create new concurrent group to receive new action

  newGroup = [self->concurrentGroupType createBegin: getCZone (zone)];
  [newGroup setDefaultOrder: [self getDefaultOrder]];
  newGroup = [newGroup createEnd];
  setBit( ((Collection_any *) newGroup)->bits, BitConcurrentGroup, 1);
  setBit( ((Collection_any *) newGroup)->bits, BitAutoDrop,
          getBit( self->bits, BitAutoDrop ) );

  newAction = [zone allocIVarsComponent: id_ActionConcurrent_c];
  setMappedAlloc (newAction);
  newAction->owner = (id) self;
  newAction->concurrentGroup = (id) newGroup;
  [newGroup _setActionConcurrent_: newAction];
  return newAction;
}


//
// _update_mergeSchedules: routine used to adjust merge schedule if actions
// are added dynamically and to the beginning of the schedule. If action is
// added to the beginning of the schedule, mergeAction for the schedule has
// to be resheduled, so that the new action gets merged too.
//


void
_update_mergeSchedules(Schedule_c *self, 
		       SwarmActivity_c *mergeScheduleActivity,
		       timeval_t oldTime,
		       timeval_t tVal)
{
  ScheduleIndex_c *mergeScheduleIndex;
  id mergeAction; 
  id mergeSchedule;
  
  if (!mergeScheduleActivity)
    return;
  mergeScheduleIndex =
    ((ScheduleActivity_c *) mergeScheduleActivity)->currentIndex;
  mergeSchedule = mergeScheduleIndex->collection;
  if (mergeScheduleIndex->currentTime > tVal)
    mergeScheduleIndex->currentTime = tVal;
  mergeAction=[mergeSchedule at: (id) oldTime];
  if (mergeAction)
    {
      if (getClass(mergeAction) == id_ActionConcurrent_c) 
	{
	  id concGroup;
	  id index;
	  
	  concGroup = ((ActionConcurrent_c *) mergeAction)->concurrentGroup;
	  index = [concGroup begin: scratchZone];
	  [index setLoc: Start];
	  mergeAction = nil;
	  while (!mergeAction)
	    {
	      id aMember;
	      
	      aMember = [index next];
	      if (!aMember) 
		raiseEvent (InvalidOperation, 
			    "> MergeSchedule is invalid. There is no",
			    "> mergeAction for schedule where action",
			    "> should be inserted!");
	      if (((ActionMerge_c *) aMember)->collectionOfActions == self)
		{
		  mergeAction = aMember;
		  [concGroup remove: aMember];
		  break;
		}
	    }
	} 
      else 
	mergeAction = [mergeSchedule removeKey: (id) oldTime];     
      _activity_insertAction (mergeSchedule, tVal, mergeAction);
    }
}



//
// _activity_insertAction: routine to create concurrent action group if needed
//

void 
_activity_insertAction (Schedule_c *self, timeval_t tVal, CAction *anAction)
{
  BOOL                newKey;
  id                  *memptr;
  CAction             *existingAction;
  ActionConcurrent_c  *newAction;
  ActionGroup_c       *existingGroup;
  
  if (_obj_debug && self->repeatInterval && (tVal >= self->repeatInterval))
    raiseEvent (InvalidArgument,
                "> cannot insert action at time greater than or equal to repeat interval\n");
  
  // attempt to insert as first action at key
  
  anAction->owner = (id) self;
  memptr = &anAction;
  newKey = [self at: (id) tVal memberSlot: &memptr];
  
  // if no previous action at key, then return unless singleton group required 
    
  if (newKey) 
    {
      // if _activity_current is not NULL, simulation is running
      
      if (_activity_current && getCurrentTime() < tVal) 
	{
	  id index = [self createIndex: scratchZone fromMember: anAction];
	  id successor_action;
	  
          successor_action = [index next];
	  
	  // Successor action is used for "adjacency test":
	  // this action is the successor of action that was
	  // just added to the schedule.
	  // If successor action is equal to the action that 
	  // currentIndex points to, that means that new action 
	  // was added before pending time for the schedule, therefor
	  // index and merge schedule have to be updated (they have to
	  // point to the new action.
	  // Since schedule can be activated in several different
	  // Swarms we have to perform "adjacency test" on each
	  // activity in activityRefs.
	  
	  if (successor_action)
	    {
	      timeval_t oldTime;  // time of the pending action
              id indexrefs;
              id activity;
	      
	      oldTime = (timeval_t) [index getKey];		   
	      indexrefs = [self->activityRefs begin: scratchZone];
	      [indexrefs setLoc: Start];
	      activity = [indexrefs next];
	      
	      while (activity)
		{
		  id scheduleIndex;
		  
		  scheduleIndex =
		    ((ScheduleActivity_c *) activity)->currentIndex;
		  if ([scheduleIndex get] == successor_action) 
		    {
		      id swarmActivity;
		      
		      // update of index, if we were to perform
		      // prev on scheduleIndex without update
		      // it would take us to Start, instead to
		      // the previous action - action that 
		      // was just added
  
		      // scheduleIndex lienar search is needed due
		      // to a bug in collections library
		      // faster way to deal with this would be:
		      // scheduleIndex->position++;

		      [scheduleIndex setLoc:Start];
		      [scheduleIndex findNext:successor_action];
		      
		      ((ScheduleIndex_c *) scheduleIndex)->currentAction = 
			[scheduleIndex prev];
		      ((ScheduleIndex_c *) scheduleIndex)->currentTime = 
			tVal;
		      swarmActivity = 
			((ScheduleActivity_c *) activity)->swarmActivity;
		      _update_mergeSchedules(self, 
					     ((SwarmActivity_c *)
					      swarmActivity), 
					     oldTime, 
					     tVal);
		    }
		  activity = [indexrefs next];
		}
	      [indexrefs drop];
	    }
	  [index drop];
	}
      if (!(self->bits & BitSingletonGroups))
        return;
      existingAction = anAction;      
    }
  else 
    {
      
      // if concurrent group already exists, then just add new action to group
      existingAction = *memptr;
      if (getClass(existingAction) == id_ActionConcurrent_c) 
	{
	  existingGroup =
	    (id) ((ActionConcurrent_c *) existingAction)->concurrentGroup;
	  anAction->owner = (id) existingGroup;
	  [existingGroup addLast: anAction];
	  return;
	}
    }

  // add initial actions to new group
  newAction = createGroup (self);
  newAction->ownerActions = existingAction->ownerActions;  // replace mem links
  *memptr = newAction;
  if (!newKey) 
    {
      existingAction->owner = (ActionType_c *) newAction->concurrentGroup;
      [(id) newAction->concurrentGroup addLast: existingAction];
    }
  anAction->owner = (id) newAction->concurrentGroup;
  [(id) newAction->concurrentGroup addLast: anAction];
}

//
// insertGroup: -- eventual Map function to insert group at key
//
- insertGroup: aKey
{
  ActionConcurrent_c *existingAction, *newAction;
  id *memptr;

  // obtain current entry at key and return if already a concurrent group

  existingAction = [self at: aKey];
  if (existingAction && getClass (existingAction) == id_ActionConcurrent_c)
    return existingAction->concurrentGroup;

  // insert action for new group as first at key

  newAction = createGroup (self);
  memptr = &newAction;
  [self at: aKey memberSlot: &memptr];
  if (!existingAction)
    return newAction->concurrentGroup;

  // if existing action at key then insert into new group and return group

  existingAction->owner = (ActionType_c *) newAction->concurrentGroup;
  newAction->ownerActions = existingAction->ownerActions;  // replace mem links
  [(id) newAction->concurrentGroup addLast: existingAction];
  *memptr = newAction;
  return newAction->concurrentGroup;
}

//
// remove: -- remove action from either schedule or concurrent group
//
- remove: anAction
{
  id  removedAction, emptyAction;

  if (_obj_debug && !respondsTo (anAction, M(_performAction_:)))
    raiseEvent (InvalidArgument,
                "> object to be removed from schedule is not an action\n");

  if (_obj_debug && ((CAction *) anAction)->owner == (id) self)
    removedAction = [super remove: anAction];
  else
    {
      // concurrent group

      if (_obj_debug && !respondsTo (((CAction *) anAction)->owner,
                                     M(_getEmptyActionConcurrent_)))
        raiseEvent (InvalidArgument,
                    "> action to be removed from schedule does not belong to schedule\n");
      
      removedAction = [(id) ((CAction *) anAction)->owner remove: anAction];
      
      emptyAction =
        [(id) ((CAction *) anAction)->owner _getEmptyActionConcurrent_];
      if (emptyAction)
        {
          [(id) ((CAction *) emptyAction)->owner remove: emptyAction];
          [emptyAction dropAllocations: YES];
        }
    }
  return removedAction;
}

//
// createAction... -- create actions comprising the action plan
//

- at: (timeval_t)tVal createAction: anActionType
{
  if (!respondsTo (anActionType, M(_performPlan_)))
    raiseEvent (InvalidArgument, nil);

  return [self at: (timeval_t) tVal
               createActionTo: anActionType message: M(_performPlan_)];
}

- at: (timeval_t)tVal createActionCall: (func_t)fptr
{
  ActionCall_0 *newAction;
  
  newAction = [getZone (self) allocIVarsComponent: id_ActionCall_0];
  newAction->funcPtr = fptr;
  _activity_insertAction (self, tVal, newAction);
  return newAction;
}

- at: (timeval_t)tVal createActionCall: (func_t)fptr : arg1
{
  ActionCall_1 *newAction;

  newAction = [getZone (self) allocIVarsComponent: id_ActionCall_1];
  newAction->funcPtr= fptr;
  newAction->arg1 = arg1;
  _activity_insertAction (self, tVal, newAction);
  return newAction;
}

- at: (timeval_t)tVal createActionCall: (func_t)fptr : arg1 : arg2
{
  ActionCall_2 *newAction;

  newAction = [getZone (self) allocIVarsComponent: id_ActionCall_2];
  newAction->funcPtr = fptr;
  newAction->arg1 = arg1;
  newAction->arg2 = arg2;
  _activity_insertAction (self, tVal, newAction);
  return newAction;
}

- at: (timeval_t)tVal createActionCall: (func_t)fptr : arg1 : arg2 : arg3
{
  ActionCall_3 *newAction;

  newAction = [getZone (self) allocIVarsComponent: id_ActionCall_3];
  newAction->funcPtr = fptr;
  newAction->arg1 = arg1;
  newAction->arg2 = arg2;
  newAction->arg3 = arg3;
  _activity_insertAction (self, tVal, newAction);
  return newAction;
}

- at: (timeval_t)tVal createActionTo: target message: (SEL)aSel
{
  ActionTo_0 *newAction;

  newAction = [getZone( self ) allocIVarsComponent: id_ActionTo_0];
  newAction->target = target;
  newAction->selector = aSel;
  _activity_insertAction (self, tVal, newAction);
  return newAction;
}

- at: (timeval_t)tVal createActionTo: target message: (SEL)aSel : arg1
{
  ActionTo_1 *newAction;

  newAction = [getZone (self) allocIVarsComponent: id_ActionTo_1];
  newAction->target = target;
  newAction->selector = aSel;
  newAction->arg1 = arg1;
  _activity_insertAction (self, tVal, newAction);
  return newAction;
}

- at: (timeval_t)tVal createActionTo: target message: (SEL)aSel : arg1 : arg2
{
  ActionTo_2 *newAction;

  newAction = [getZone (self) allocIVarsComponent: id_ActionTo_2];
  newAction->target = target;
  newAction->selector = aSel;
  newAction->arg1 = arg1;
  newAction->arg2 = arg2;

  _activity_insertAction (self, tVal, newAction);
  return newAction;
}

- at: (timeval_t)tVal createActionTo: target message: (SEL)aSel:arg1:arg2:arg3;
{
  ActionTo_3 *newAction;

  newAction = [getZone (self) allocIVarsComponent: id_ActionTo_3];
  newAction->target = target;
  newAction->selector = aSel;
  newAction->arg1 = arg1;
  newAction->arg2 = arg2;
  newAction->arg3 = arg3;
  _activity_insertAction (self, tVal, newAction);
  return newAction;
}

- at: (timeval_t)tVal createActionForEach: target message: (SEL)aSel
{
  ActionForEach_0 *newAction;

  newAction = [getZone (self) allocIVarsComponent: id_ActionForEach_0];
  newAction->target = target;
  newAction->selector = aSel;
  if ([self getDefaultOrder] == (id) Randomized)
    setBit (newAction->bits, BitRandomized, 1);
  _activity_insertAction (self, tVal, newAction);
  return newAction;
}

- at: (timeval_t)tVal createActionForEach: target message: (SEL)aSel : arg1
{
  ActionForEach_1 *newAction;

  newAction = [getZone (self) allocIVarsComponent: id_ActionForEach_1];
  newAction->target = target;
  newAction->selector = aSel;
  newAction->arg1 = arg1;
  if ([self getDefaultOrder] == (id) Randomized)
    setBit(newAction->bits, BitRandomized, 1);
  _activity_insertAction (self, tVal, newAction);
  return newAction;
}

- at: (timeval_t)tVal createActionForEach: target message: (SEL)aSel:arg1:arg2
{
  ActionForEach_2 *newAction;

  newAction = [getZone (self) allocIVarsComponent: id_ActionForEach_2];
  newAction->target = target;
  newAction->selector = aSel;
  newAction->arg1 = arg1;
  newAction->arg2 = arg2;
  if ([self getDefaultOrder] == (id) Randomized)
    setBit (newAction->bits, BitRandomized, 1);
  _activity_insertAction (self, tVal, newAction);
  return newAction;
}

- at: (timeval_t)tVal createActionForEach: t message: (SEL)aSel:arg1:arg2:arg3;
{
  ActionForEach_2 *newAction;

  newAction = [getZone (self) allocIVarsComponent: id_ActionForEach_3];
  newAction->target = t;
  newAction->selector = aSel;
  newAction->arg1 = arg1;
  newAction->arg2 = arg2;
  if ([self getDefaultOrder] == (id) Randomized)
    setBit (newAction->bits, BitRandomized, 1);
  _activity_insertAction (self, tVal, newAction);
  return newAction;
}

//
// createAction... -- convenience messages to create actions at time value zero
//

- createAction: anActionType
{
  return [self at: 0 createAction: anActionType];
}

- createActionCall: (func_t)fptr
{
  return [self at: 0 createActionCall: fptr];
}

- createActionCall: (func_t)fptr : arg1
{
  return [self at: 0 createActionCall: fptr : arg1];
}

- createActionCall: (func_t)fptr : arg1 : arg2
{
  return [self at: 0 createActionCall: fptr : arg1 : arg2];
}

- createActionCall: (func_t)fptr : arg1 : arg2 : arg3
{
  return [self at: 0 createActionCall: fptr : arg1 : arg2 : arg3];
}

- createActionTo: target message: (SEL)aSel
{
  return [self at: 0 createActionTo: target message: aSel];
}

- createActionTo: target message: (SEL)aSel : arg1
{
  return [self at: 0 createActionTo: target message: aSel : arg1];
}

- createActionTo: target message: (SEL)aSel : arg1 : arg2
{
  return [self at: 0 createActionTo: target message: aSel : arg1 : arg2];
}

- createActionTo: target message: (SEL)aSel : arg1 : arg2 : arg3
{
  return [self at: 0 createActionTo: target message: aSel : arg1:arg2:arg3];
}

- createActionForEach: target message: (SEL)aSel
{
  return [self at: 0 createActionForEach: target message: aSel];
}

- createActionForEach: target message: (SEL)aSel : arg1
{
  return [self at: 0 createActionForEach: target message: aSel : arg1];
}

- createActionForEach: target message: (SEL)aSel : arg1 : arg2
{
  return [self at: 0 createActionForEach: target message: aSel : arg1 : arg2];
}

- createActionForEach: target message: (SEL)aSel : arg1 : arg2 : arg3
{
  return [self at: 0 createActionForEach: target message: aSel:arg1:arg2:arg3];
}

//
// mapAllocations: -- standard method to identify internal allocations
//
- (void)mapAllocations: (mapalloc_t)mapalloc
{
  id  index, member, groupIndex, groupMember, nextMember;

  if (activityRefs)
    mapObject (mapalloc, activityRefs);

  index = [self begin: scratchZone];
  while ((member = [index next]))
    {
      // if action is for a concurrent group, then map all actions in the group
      
      if (getClass (member) == id_ActionConcurrent_c)
        {
          groupIndex = [(id) ((ActionConcurrent_c *) member)->concurrentGroup
                             begin: scratchZone];
          nextMember = [groupIndex next];
          while ((groupMember = nextMember))
            {
              nextMember = [groupIndex next];
              mapObject (mapalloc, groupMember);
            }
          [groupIndex drop];
        }
      
      // map the action contained in the schedule itself
      
      mapObject (mapalloc, member);
    }
  [index drop];
  [super mapAllocations: mapalloc];
}

- (void) describe: outputCharStream
{
  [super describe: outputCharStream];
}

- (void) describeForEach: outputCharStream
{
  char buffer[100];
  id index, actionAtTime;
  timeval_t timeOfAction;

  index = [self begin: scratchZone];
  while ((actionAtTime = [index next]))
    {
      timeOfAction = (timeval_t) [index getKey];
      sprintf (buffer, "at time: %lu action is: ", timeOfAction);
      [outputCharStream catC: buffer];
      [actionAtTime describe: outputCharStream];
    }
  [index drop]; 
}

- (void)describeForEachID: outputCharStream
{
  char buffer[100];
  id index, actionAtTime;
  timeval_t timeOfAction;

  index = [self begin: scratchZone];
  while ((actionAtTime = [index next])) 
    {
      timeOfAction = (timeval_t) [index getKey];
      sprintf (buffer, "at time: %lu action is: ", timeOfAction);
      [actionAtTime describeID: outputCharStream];
    }
  [index drop]; 
}

@end

//
// ActionConcurrent_c -- action to perform concurrent group within schedule
//

@implementation ActionConcurrent_c

- (void)_performAction_: anActivity
{
  [(id) concurrentGroup _performPlan_];
}

- (void)mapAllocations: (mapalloc_t)mapalloc
{
  // identify the action group that held the concurrent actions

  mapObject (mapalloc, concurrentGroup);
}

- (void)describe: outputCharStream
{
  char buffer[100];

  [outputCharStream catC: "["];
  _obj_formatIDString( buffer, concurrentGroup );
  [outputCharStream catC: buffer];
  [outputCharStream catC: " (concurrent group)]\n"];
}

@end

//
// ConcurrentSchedule_c --
//   concurrent group that supports ordering of actions by unsigned integer
//   keys
//

@implementation ConcurrentSchedule_c

//
// addLast: --
//  method to make a schedule usable in the special role as a concurrent group
//
- (void)addLast: anAction
{
  raiseEvent (SourceMessage,
              "> A concurrent schedule requires either that a subclass override the method\n"
              "> that adds a new action, or that all actions be added to the group by\n"
              "> explicit operations rather than by automatic addition at a key value.\n" );
  exit (0);
}

//
// _setActionConcurrent_ --
//   internal method to set link back to action that refers to concurrent group
//
- (void)_setActionConcurrent_: action
{
  actionConcurrent = action;
}

//
// _getEmptyActionConcurrent_ --
//   internal method to dispose of concurrent group if it has become empty
//
- _getEmptyActionConcurrent_
{
  if (count)
    return nil;
  return actionConcurrent;
}

- (void) mapAllocations: (mapalloc_t)mapalloc
{
  if (activityRefs)
    mapObject (mapalloc, activityRefs);

  //
  // Skip over normal inheritance from Schedule_c so as not to map any
  // member actions.  The schedule containing a concurrent group is
  // responsible for mapping all its actions including those in concurrent
  // groups; but a concurrent group must only map itself.  Unlike a
  // concurrent action group, the MappedAlloc bit must remain set, as the
  // underlying data structure of a Schedule requires this.
  //
  callMethodInClass (id_Map_c, M(mapAllocations:), mapalloc);
}

@end

//
// ActivationOrder_c --
//   concurrent group to order merge by activation order within swarm
//
@implementation ActivationOrder_c

//
// addLast: --
//   method to sort concurrent merge actions in the order of swarm activation
//
- (void)addLast: mergeAction
{
  [self at: (id) ((ActionMerge_c *) mergeAction)->subactivity->activationNumber
        insert: mergeAction];
}

//
// remove: --
//   method to remove concurrent merge action from sorted group
//
- remove: mergeAction
{
  return [self removeKey:
                 (id) ((ActionMerge_c *) mergeAction)->subactivity->activationNumber];
}
//
// _getEmptyActionConcurrent_ --
//   internal method to dispose of concurrent group if it has become empty
//
// For a merge schedule, never get rid of empty concurrent group because
// there could still be an activity running on it
//
- _getEmptyActionConcurrent_
{
  return nil;
}

@end

//
// ScheduleActivity_c -- activity to process a Schedule action plan
//

@implementation ScheduleActivity_c

//
// getCurrentTime -- get current time of activity (pending time if holding)
//
- (timeval_t)getCurrentTime
{
  return ((ScheduleIndex_c *) self->currentIndex)->currentTime;
}

//
// stepUntil: -- advance activity until requested time has been reached
//
- stepUntil: (timeval_t)tVal
{
  id  nextStatus = nil;

  while ([self getCurrentTime] < tVal
         && !COMPLETEDP (nextStatus = [self next]));
  return nextStatus ? nextStatus : [self getStatus];
}

//
// mapAllocations: -- standard method to identify internal allocations
//
- (void)mapAllocations: (mapalloc_t)mapalloc
{
  [super mapAllocations: mapalloc];
  if (mergeAction)
    {
      mapalloc->descriptor = t_LeafObject;
      mapAlloc (mapalloc, mergeAction);
    }
}

//
// dropAllocations: --
//   remove merge action from merge schedule if it is not a current subactivity
//
- (void)dropAllocations: (BOOL)componentAlloc
{
  // remove the merge action for the activity from the merge schedule

  if (mergeAction)
    [((Index_any *) swarmActivity->currentIndex)->collection
                                                remove: mergeAction];
  
  // complete the rest of the drop actions by standard means
  
  [super dropAllocations: componentAlloc];
}

@end


//
// ScheduleIndex_c -- index created to traverse member actions for an activity
//

@implementation ScheduleIndex_c

//
// mix in action plan index methods by source inclusion
//
#define  MIXIN_INDEX
#include "CompoundAction.m"

//
// nextAction: -- return next action and set currentTime to its key value
//
- nextAction: (id *)status
{
  id actionAtIndex, removedAction;
  ActionChanged_c *newAction;
  ScheduleIndex_c *swarmIndex;

  // finish any processing of previous action

  if (currentAction && !REMOVEDP (currentAction))
    {
      //
      // Check if the action at the current index has changed underneath.
      // Using public interfaces to a schedules and activities, the only way
      // this can occur is if the action at a timestep has changed from a
      // a single action to a concurrent group during execution of the initial
      // action.  To recover from this change, a special action (allocated in
      // the same zone as the index) is returned.  When performed, this action
      // starts the concurrent group like ActionPerform, but skips its first
      // (already executed) action before dropping itself and continuing.  
      //
      
      if (currentAction != (actionAtIndex = [super get]))
        {
          //!! (later -- when Map implementation has stabilized --
          //!! get actionAtIndex directly from the underlying implementation)
          newAction =
            [id_ActionChanged_c create: getZone ((Activity_c *) activity)];
          newAction->actionAtIndex = actionAtIndex;
          currentAction = newAction;
          setMappedAlloc (self);
          // return special object to handle _performAction_:
          // (change object need only be dropped to disappear without a trace)
          return newAction;  
        } 
      
      //
      // If AutoDrop option, then drop previous action before continuing.
      //
      if (((Schedule_c *) collection)->bits & BitAutoDrop)
        {
          removedAction = [super remove];
          [removedAction dropAllocations: YES];
        }
    }
  
  //
  // Get next action from index, adjust times, and return.
  //
  
  currentAction = [self next: (id *) &currentTime];
  
  if (currentAction)
    { 
      // action ready to be executed
      
      // if relative time then readjust current time by start time
      
      if (((Schedule_c *) collection)->bits & BitRelativeTime)
        currentTime += startTime;
    }
  else 
    {
      // no more actions to be executed
      
      // if repeat interval, then start over again
      
      if (((Schedule_c *) collection)->repeatInterval)
        {
          startTime += ((Schedule_c *) collection)->repeatInterval;
          if (startTime < currentTime)
            raiseEvent (SourceMessage,
                        "> schedule did not complete soon enough for its scheduled repeat\n");
          
          [self setLoc: Start];
          currentAction = [self next: (id *) &currentTime];
          currentTime += startTime;
        }
    }
  
  //
  // If running under swarm activity, and there are any other schedule
  // activities being merged, then reschedule merge action at the next
  // pending time to be processed from the merge schedule.  Remove the
  // current activity as a subactivity of its owner and set its status
  // to Holding, so that the run loop will not attempt further processing
  // at this time.
  //
  if (((ScheduleActivity_c *) activity)->swarmActivity
      && (currentAction
          || INITIALIZEDP (((ScheduleActivity_c *) activity)->swarmActivity->status)))
    {
      swarmIndex =
        ((ScheduleActivity_c *) activity)->swarmActivity->currentIndex;
      _activity_insertAction ((id) swarmIndex->collection, currentTime,
                              ((ScheduleActivity_c *) activity)->mergeAction);
      if (currentAction) 
        *status = Holding;
      else
        [self setLoc: Start];
      
      // if empty schedule just added to new swarm activity, then set to
      // reprocess schedule (should generalize to any empty schedule)
      
      return nil;  // indicate that activity is not ready to run 
    }
  
  // return next action to be processed
  
  if (!currentAction)
    *status = Completed;
  
  return currentAction;
}

- setCurrentTime : (timeval_t)tVal
{
  id member = [self setKey : (id)tVal];
  
  if (member == nil)
    abort ();
  currentTime = tVal;
  currentAction = member;
  return self;
}

//
// remove -- remove action at current index
//
- remove
{
  ActionConcurrent_c *actionAtIndex, *removedAction;
  
  //
  // Get the action at the current index to see if it has changed to a
  // concurrent group.  If it has, then remove the action as the first
  // member of the new group, leave the current action that performs
  // the entire remaining group, and mark currentAction as removed so
  // that the action will not be disturbed by nextAction.  Finally, back
  // up the index ([self prev]) so that a subsequent next will obtain the
  // concurrent group just added.
  //

  if (currentAction && currentAction != (actionAtIndex = [super get]))
    {
      removedAction = [(id)actionAtIndex->concurrentGroup removeFirst];
      [self prev];
    }
  else
    // just remove the action at the index
    removedAction = [super remove];
  currentAction = Removed;
  return removedAction;
}

//
// get -- obtain action at current index
//
- get
{
  if (REMOVEDP (currentAction) || COMPLETEDP (currentAction))
    return nil;
  return currentAction;
}

//
// getCurrentTime -- obtain current time from index, which holds it
//
- (timeval_t)getCurrentTime
{
  if (INDEXSTARTP ([self getLoc]))
    return startTime;
  return startTime + (timeval_t) ((mapentry_t) [listIndex get])->key;
}

//
// mapAllocations: -- standard method to identify internal allocations
//
- (void)mapAllocations: (mapalloc_t)mapalloc
{
  if (currentAction && getClass (currentAction) == id_ActionChanged_c)
    mapObject (mapalloc, currentAction);
  [super mapAllocations: mapalloc];
}

//
// dropAllocations: -- drop index as component of activity
//
- (void)dropAllocations: (BOOL)componentAlloc
{
  [((Schedule_c *) collection)->activityRefs remove: activity];
  [super dropAllocations: YES];
}

- (void) describe: outputCharStream
{
  char buffer[200];

  sprintf (buffer,"Current Time: %d \nStart Time: %d \n", 
           (int) currentTime, (int) startTime);
  [outputCharStream catC: buffer];
  [collection describeForEach: outputCharStream];
}

@end


@implementation ActionChanged_c

- (void)_performAction_: anActivity
{
  //
  // Create the activity for the new concurrent group action, but position it
  // to the first member so that a subsequent nextAction: will get the second
  // member.  Drop the ActionChanged_c object itself since it has served its
  // entire transitory role.
  //
  ScheduleIndex_c *schedIndex;
  id unusedStatus;

  schedIndex = ((Activity_c *) anActivity)->currentIndex;
  schedIndex->currentAction = actionAtIndex;
  unsetMappedAlloc (schedIndex);
  [actionAtIndex _performAction_: anActivity];
  [((Activity_c *) anActivity)->currentSubactivity->currentIndex
                              nextAction: &unusedStatus];
  [self drop];
}

- (void) describe: outputCharStream
{
  char buffer[100];
  
  _obj_formatIDString (buffer, actionAtIndex);
  [outputCharStream catC: buffer];
  [outputCharStream catC: " "];
}

@end

