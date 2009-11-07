// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

/*
Name:         Schedule.m
Description:  collection of actions ordered by time values 
Library:      activity
*/

#import <activity/Schedule.h>
#import <activity/ActionGroup.h>
#import <defobj/defalloc.h>
#import <activity/activity_classes.h>
#include <misc.h> // abort

#import <collections/List_mlinks.h> // beginMlinksList
#import <defobj/macros.h>
#import <collections/macros.h>
#import <activity/macros.h>

#define FAST

@implementation Schedule_c

PHASE(Creating)

//
// mix in action plan create-phase methods by source inclusion
//
#define  MIXIN_CREATE
#include "CompoundAction.m"

+ createBegin: (id <Zone>)aZone
{
  Schedule_c *obj = [super createBegin: aZone];
  
  obj->keepEmptyFlag = YES;
  return obj;
}
//
// create:setRepeatInterval: -- convenience create message
//
+ create: (id <Zone>)aZone setRepeatInterval: (timeval_t)rptInterval
{
  id  new;

  new = [self createBegin: aZone];
  [new setRepeatInterval: rptInterval];
  return [new createEnd];
}

+ create: (id <Zone>)aZone setAutoDrop: (BOOL)autoDrop
{
  id  new;

  new = [self createBegin: aZone];
  [new setAutoDrop: autoDrop];
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

- setRelativeTime: (BOOL)relativeTime
{
  setBit (bits, BitRelativeTime, relativeTime);
  setBit (bits, BitRelTimeSet, 1);
  return self;
}

- setKeepEmptyFlag: (BOOL)theKeepEmptyFlag
{
  keepEmptyFlag = theKeepEmptyFlag;
  return self;
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
  [(id) self setCompareFunction: compareUnsignedIntegers];

  if (createByMessageToCopy (self, createEnd))
    return self;

  if (!concurrentGroupType)
    concurrentGroupType = ConcurrentGroup;
  [super createEnd];

  return self;
}

PHASE(Setting)

- setRepeatInterval: (timeval_t)rptInterval
{
  if (rptInterval == 0)
    raiseEvent (InvalidArgument,
                "> repeat interval must be greater than zero\n");

  if (!getNextPhase (getClass (self)) && !(bits & BitRelativeTime))
    raiseEvent (InvalidCombination,
                "> cannot specify a repeat interval after schedule created without it\n" );

  repeatInterval = rptInterval;
  return self;
}

#define MIXIN_SET
#include "CompoundAction.m"

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
- _activateUnderSwarm_: (Class)activityClass : (Class)indexClass : swarmContext : (Zone_c *)swarmZone
{
  SwarmActivity_c *swarmActivity;
  ScheduleActivity_c *newActivity;
  ScheduleIndex_c *newIndex, *swarmIndex;
  ActionMerge_c *mergeAction;

  // initialize new activity to run underneath swarm

  swarmActivity = swarmContext;
  newActivity =
    [self _createActivity_: swarmActivity 
          : activityClass
          : indexClass
          : swarmZone];
  [newActivity setKeepEmptyFlag: keepEmptyFlag];
  newActivity->ownerActivity = nil;
  newActivity->swarmActivity = swarmActivity;
  newActivity->activationNumber = swarmActivity->nextActivation++;

  // create merge action for use by swarm in merging schedule subactivities

  newIndex = (ScheduleIndex_c *)newActivity->currentIndex;
  mergeAction = ALLOCIVARSCOMPONENT (swarmZone, id_ActionMerge_c);
  setMappedAlloc (mergeAction);
  mergeAction->subactivity = newActivity;
  mergeAction->collectionOfActions = self;
  newActivity->mergeAction = mergeAction;

  // set the starting and current times of the new activity

  swarmIndex = (ScheduleIndex_c *)swarmActivity->currentIndex;
  newIndex->currentTime = newIndex->startTime = swarmIndex->currentTime;

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
  ActionConcurrent_c *newAction;
  id zone = getZone (self);

  // create new concurrent group to receive new action

  newGroup = [self->concurrentGroupType create: getCZone (zone)];

  // forced on
  setBit (((Collection_any *) newGroup)->bits, BitConcurrentGroup, 1);

  // forced to policy of Schedule
  setBit (((Collection_any *) newGroup)->bits, BitAutoDrop,
          getBit (self->bits, BitAutoDrop));

  // if either sets these flags, use it
  ((Collection_any *) newGroup)->bits |= getBit (self->bits, BitRandomized);
  ((Collection_any *) newGroup)->bits |= getBit (self->bits, BitConcurrent);

  newAction = ALLOCIVARSCOMPONENT (zone, id_ActionConcurrent_c);
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
// to be rescheduled, so that the new action gets merged too.
//

void
_update_mergeSchedules (Schedule_c *self, 
                        ScheduleActivity_c *mergeScheduleActivity,
                        timeval_t oldTime,
                        timeval_t tVal)
{
  ScheduleIndex_c *mergeScheduleIndex;
  ActionMerge_c *mergeAction; 
  Schedule_c *mergeSchedule;

  if (!mergeScheduleActivity)
    return;
  mergeScheduleIndex = (ScheduleIndex_c *)mergeScheduleActivity->currentIndex;
  mergeSchedule = (Schedule_c *) mergeScheduleIndex->collection;
  if (mergeScheduleIndex->currentTime > tVal)
    mergeScheduleIndex->currentTime = tVal;
  mergeAction = [mergeSchedule at: (id) oldTime];
  if (mergeAction)
    {
      if (getClass (mergeAction) == id_ActionConcurrent_c) 
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
			    "> MergeSchedule is invalid. There is no\n"
			    "> mergeAction for schedule where action\n"
			    "> should be inserted!\n");
	      if (((ActionMerge_c *) aMember)->collectionOfActions == self)
		{
		  mergeAction = aMember;
		  [concGroup remove: aMember];
		  break;
		}
	    }
          DROP (index);
	} 
      else 
        mergeAction = [mergeSchedule removeKey: (id) oldTime];     
      _activity_insertAction (mergeSchedule, tVal, mergeAction);
    }
}

static void
ensureLeadingMerge (Schedule_c *self, id <Index> index, timeval_t tVal)
{
  if (!MAP_INDEX_PREV (index))
    {
      id indexrefs;
      ScheduleActivity_c *activity;

      indexrefs = MLIST_BEGIN_ZONE (self->activityRefs, scratchZone);
      MLIST_INDEX_SETLOC (indexrefs, Start);
      activity = [indexrefs next];
      
      while (activity)
        {
          ScheduleIndex_c *scheduleIndex, *mergeScheduleIndex;
          Schedule_c *mergeSchedule;
          Activity_c *swarmActivity;
          
          scheduleIndex =  (ScheduleIndex_c *)activity->currentIndex;
          MAP_INDEX_SETLOC (scheduleIndex, Start);
          scheduleIndex->currentAction = 0; 
          scheduleIndex->currentTime = tVal;
          if (scheduleIndex->startTime > tVal)
            scheduleIndex->startTime -= self->repeatInterval;
          swarmActivity = activity->swarmActivity;
          if (swarmActivity)
            {
              mergeScheduleIndex =  (ScheduleIndex_c *)swarmActivity->currentIndex;
              mergeSchedule = 
                (Schedule_c *) mergeScheduleIndex->collection;
              _activity_insertAction (mergeSchedule,
                                      tVal, 
                                      (CAction *)activity->mergeAction);
            }
          activity = [indexrefs next];
        }
      DROP (indexrefs);
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
  newKey = MAP_AT_MEMBERSLOT (self, (id) tVal, &memptr);
  
  // if no previous action at key, then return unless singleton group required 
  if (newKey) 
    {
      if (self->activityRefs
          && _activity_current && getCurrentTime () <= tVal) 
	{
          id index = MAP_CREATEINDEX_FROMMEMBER (self, getCZone (getZone (self)), anAction);
          id successor_action;

          successor_action = MAP_INDEX_NEXT (index);
          // Successor action is used for "adjacency test": this
          // action is the successor of action that was just added to
          // the schedule.  If successor action is equal to the action
          // that currentIndex points to, that means that new action
          // was added before the pending time for the schedule,
          // therefore the index and merge schedule have to be updated
          // (they have to point to the new action.  Since schedule
          // can be activated in several different Swarms we have to
          // perform "adjacency test" on each activity in
          // activityRefs.
          
          if (successor_action)
            {
              timeval_t oldTime;  // time of the pending action
              id indexrefs;
              ScheduleActivity_c *activity;
              
              oldTime = (timeval_t) [index getKey];		   
              indexrefs = MLIST_BEGIN_ZONE (self->activityRefs, scratchZone);
              MLIST_INDEX_SETLOC (indexrefs, Start);
              activity = [indexrefs next];
              
              while (activity)
                {
                  ScheduleIndex_c *scheduleIndex;
                  
                  scheduleIndex =  (ScheduleIndex_c *)activity->currentIndex;
                  if ([scheduleIndex get] == successor_action) 
                    {
                      // update of index, if we were to perform
                      // prev on scheduleIndex without update
                      // it would take us to Start, instead to
                      // the previous action - action that 
                      // was just added
                      
                      // scheduleIndex linear search is needed due
                      // to a bug in collections library
                      // faster way to deal with this would be:
                      // scheduleIndex->position++;
                      
                      MAP_INDEX_SETLOC (scheduleIndex, Start);
                      [scheduleIndex findNext: successor_action];
                      
                      scheduleIndex->currentAction =
			MAP_INDEX_PREV (scheduleIndex);
                      scheduleIndex->currentTime = tVal;
                      _update_mergeSchedules (self,
                                              activity->swarmActivity,
                                              oldTime, tVal);
                    }
                  activity = [indexrefs next];
                }
              DROP (indexrefs);
            }
          else
            {
              MAP_INDEX_PREV (index);
              ensureLeadingMerge (self, index, tVal);
            }
          DROP (index);
	}
      if (!(self->bits & BitSingletonGroups))
	return;
      existingAction = anAction;      
    }
  else 
    {
      // if concurrent group already exists, then just add new action to group
      existingAction = *memptr;
      if (getClass (existingAction) == id_ActionConcurrent_c) 
	{
	  existingGroup =
	    (id) ((ActionConcurrent_c *) existingAction)->concurrentGroup;
	  anAction->owner = (id) existingGroup;
	  GENERIC_ADD_LAST (existingGroup, anAction);
	  return;
	}
    }

  // add initial actions to new group
  newAction = createGroup (self);
  *memptr = newAction;
  if (!newKey) 
    {
      existingAction->owner = (ActionType_c *) newAction->concurrentGroup;
      GENERIC_ADD_LAST ((id) newAction->concurrentGroup, existingAction);
    }
  anAction->owner = (id) newAction->concurrentGroup;
  GENERIC_ADD_LAST ((id) newAction->concurrentGroup, anAction);
}

//
// insertGroup: -- eventual Map function to insert group at key
//
- (id <ActionGroup>)insertGroup: (timeval_t)aKey
{
  ActionConcurrent_c *existingAction, *newAction;
  id *memptr;

  // obtain current entry at key and return if already a concurrent group

  existingAction = [self at: (id) aKey];
  if (existingAction && getClass (existingAction) == id_ActionConcurrent_c)
    return existingAction->concurrentGroup;

  // insert action for new group as first at key

  newAction = createGroup (self);
  memptr = &newAction;
  [self at: (id) aKey memberSlot: &memptr];
  if (!existingAction)
    return newAction->concurrentGroup;

  // if existing action at key then insert into new group and return group

  existingAction->owner = (ActionType_c *) newAction->concurrentGroup;
  GENERIC_ADD_LAST ((id) newAction->concurrentGroup, existingAction);
  *memptr = newAction;
  return newAction->concurrentGroup;
}

//
// remove: -- remove action from either schedule or concurrent group
//
- remove: anAction
{
  id  removedAction, emptyAction;

#ifndef FAST
  if (_obj_debug && ![anAction conformsTo: @protocol (Action)])
    raiseEvent (InvalidArgument,
                "> object to be removed from schedule is not an action\n");
#endif

  if (((CAction *) anAction)->owner == (id) self)
    removedAction = [super remove: anAction];
  else
    {
      // concurrent group
#ifndef FAST
      if (_obj_debug && ![((CAction *) anAction)->owner conformsTo: @protocol (ConcurrentGroup)])
        raiseEvent (InvalidArgument,
                    "> action to be removed from schedule does not belong to schedule\n");
#endif
      
      removedAction = [(id <Schedule>) ((CAction *) anAction)->owner remove: anAction];
      
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

- (id <FAction>)at: (timeval_t)tVal createFAction: call
{
  id <FAction> faction =  [FAction createBegin: getCZone (getZone (self))];
  [faction setCall: call];
  faction = [faction createEnd];
  _activity_insertAction (self, tVal, (CAction *)faction);
  return faction;
}

- at: (timeval_t)tVal createAction: anActionType
{
  if (!respondsTo (anActionType, M(_performPlan_)))
    raiseEvent (InvalidArgument, nil);

  return [self at: (timeval_t) tVal
               createActionTo: anActionType message: M(_performPlan_)];
}

- (id <ActionCall>)at: (timeval_t)tVal createActionCall: (func_t)fptr
{
  id <ActionCall> newAction = 
    [ActionCall createBegin: getCZone (getZone (self))];
  [newAction setFunctionPointer: fptr];
  newAction = [newAction createEnd];
  _activity_insertAction (self, tVal, (CAction *)newAction);
  return newAction;
}

- (id <ActionCall>)at: (timeval_t)tVal createActionCall: (func_t)fptr : arg1
{
  id <ActionCall> newAction = [ActionCall createBegin: 
                                            getCZone (getZone (self))];
  [newAction setFunctionPointer: fptr];
  [newAction setArg1: arg1];
  newAction = [newAction createEnd];
  _activity_insertAction (self, tVal, (CAction *)newAction);
  return newAction;
}

- (id <ActionCall>)at: (timeval_t)tVal createActionCall: (func_t)fptr : arg1 : arg2
{
  id <ActionCall> newAction = [ActionCall createBegin: 
                                            getCZone (getZone (self))];
  [newAction setFunctionPointer: fptr];
  [newAction setArg1: arg1];
  [newAction setArg2: arg2];
  newAction = [newAction createEnd];
  _activity_insertAction (self, tVal, (CAction *)newAction);
  return newAction;
}

- (id <ActionCall>)at: (timeval_t)tVal createActionCall: (func_t)fptr : arg1 : arg2 : arg3
{
  id <ActionCall> newAction = 
    [ActionCall createBegin: getCZone (getZone (self))];
  [newAction setFunctionPointer: fptr];
  [newAction setArg1: arg1];
  [newAction setArg2: arg2];
  [newAction setArg3: arg3];
  newAction = [newAction createEnd];
  _activity_insertAction (self, tVal, (CAction *)newAction);
  return newAction;
}

- (id <ActionTo>)at: (timeval_t)tVal createActionTo: target message: (SEL)aSel
{
  id <ActionTo> newAction = [ActionTo createBegin: getCZone (getZone (self))];
  [newAction setTarget: target];
  [newAction setMessageSelector: aSel];
  newAction = [newAction createEnd];
  _activity_insertAction (self, tVal, (CAction *)newAction);
  return newAction;
}

- (id <ActionTo>)at: (timeval_t)tVal createActionTo: target message: (SEL)aSel : arg1
{
  id <ActionTo> newAction = [ActionTo createBegin: getCZone (getZone (self))];
  [newAction setTarget: target];
  [newAction setMessageSelector: aSel];
  [newAction setArg1: arg1];
  newAction = [newAction createEnd];
  _activity_insertAction (self, tVal, (CAction *)newAction);
  return newAction;
}

- (id <ActionTo>)at: (timeval_t)tVal createActionTo: target message: (SEL)aSel : arg1 : arg2
{
  id <ActionTo> newAction = [ActionTo createBegin: getCZone (getZone (self))];
  [newAction setTarget: target];
  [newAction setMessageSelector: aSel];
  [newAction setArg1: arg1];
  [newAction setArg2: arg2];
  newAction = [newAction createEnd];
  _activity_insertAction (self, tVal, (CAction *)newAction);
  return newAction;
}

- (id <ActionTo>)at: (timeval_t)tVal createActionTo: target message: (SEL)aSel:arg1:arg2:arg3;
{
  id <ActionTo> newAction = [ActionTo createBegin: getCZone (getZone (self))];
  [newAction setTarget: target];
  [newAction setMessageSelector: aSel];
  [newAction setArg1: arg1];
  [newAction setArg2: arg2];
  [newAction setArg3: arg3];
  newAction = [newAction createEnd];
  _activity_insertAction (self, tVal, (CAction *)newAction);
  return newAction;
}

- (id <ActionForEachHomogeneous>)at: (timeval_t)tVal createActionForEachHomogeneous: target message: (SEL)aSel
{
  id <ActionForEachHomogeneous> newAction =
    [ActionForEachHomogeneous createBegin:
                                getCZone (getZone (self))];
  [newAction setTarget: target];
  [newAction setMessageSelector: aSel];
  newAction = [newAction createEnd];
  _activity_insertAction (self, tVal, (CAction *)newAction);
  return newAction;
}

- (id <ActionForEach>)at: (timeval_t)tVal createActionForEach: target message: (SEL)aSel
{
  id <ActionForEach> newAction = [ActionForEach createBegin:
                                                  getCZone (getZone (self))];
  [newAction setTarget: target];
  [newAction setMessageSelector: aSel];
  newAction = [newAction createEnd];
  _activity_insertAction (self, tVal, (CAction *)newAction);
  return newAction;
}

- (id <ActionForEach>)at: (timeval_t)tVal createActionForEach: target message: (SEL)aSel : arg1
{
  id <ActionForEach> newAction = [ActionForEach createBegin:
                                                  getCZone (getZone (self))];
  [newAction setTarget: target];
  [newAction setMessageSelector: aSel];
  [newAction setArg1: arg1];
  newAction = [newAction createEnd];
  _activity_insertAction (self, tVal, (CAction *)newAction);
  return newAction;
}

- (id <ActionForEach>)at: (timeval_t)tVal createActionForEach: target message: (SEL)aSel:arg1:arg2
{
  id <ActionForEach> newAction = [ActionForEach createBegin:
                                                  getCZone (getZone (self))];
  [newAction setTarget: target];
  [newAction setMessageSelector: aSel];
  [newAction setArg1: arg1];
  [newAction setArg2: arg2];
  newAction = [newAction createEnd];
  _activity_insertAction (self, tVal, (CAction *)newAction);
  return newAction;
}

- (id <ActionForEach>)at: (timeval_t)tVal createActionForEach: target
                                  message: (SEL)aSel:arg1:arg2:arg3;
{
  id <ActionForEach> newAction = [ActionForEach createBegin:
                                                  getCZone (getZone (self))];
  [newAction setTarget: target];
  [newAction setMessageSelector: aSel];
  [newAction setArg1: arg1];
  [newAction setArg2: arg2];
  [newAction setArg3: arg2];
  newAction = [newAction createEnd];
  _activity_insertAction (self, tVal, (CAction *)newAction);
  return newAction;
}

- (id <FActionForEachHeterogeneous>)at: (timeval_t)tVal createFActionForEachHeterogeneous: target call: (id <FCall>)call
{
  id <FActionForEachHeterogeneous> newAction =
    [FActionForEachHeterogeneous createBegin: getCZone (getZone (self))];
  [newAction setCall: call];
  [newAction setTarget: target];
  newAction = [newAction createEnd];
  _activity_insertAction (self, tVal, (CAction *)newAction);
  return newAction;
}

- (id <FActionForEachHomogeneous>)at: (timeval_t)tVal createFActionForEachHomogeneous: target call: (id <FCall>)call
{
  id <FActionForEachHomogeneous> newAction =
    [FActionForEachHomogeneous createBegin: getCZone (getZone (self))];
  [newAction setCall: call];
  [newAction setTarget: target];
  newAction = [newAction createEnd];
  _activity_insertAction (self, tVal, (CAction *)newAction);
  return newAction;
}

//
// createAction... -- convenience messages to create actions at time value zero
//

- (id <FAction>)createFAction: call
{
  return [self at: 0 createFAction: call];
}

- createAction: anActionType
{
  return [self at: 0 createAction: anActionType];
}

- (id <ActionCall>)createActionCall: (func_t)fptr
{
  return [self at: 0 createActionCall: fptr];
}

- (id <ActionCall>)createActionCall: (func_t)fptr : arg1
{
  return [self at: 0 createActionCall: fptr : arg1];
}

- (id <ActionCall>)createActionCall: (func_t)fptr : arg1 : arg2
{
  return [self at: 0 createActionCall: fptr : arg1 : arg2];
}

- (id <ActionCall>)createActionCall: (func_t)fptr : arg1 : arg2 : arg3
{
  return [self at: 0 createActionCall: fptr : arg1 : arg2 : arg3];
}

- (id <ActionTo>)createActionTo: target message: (SEL)aSel
{
  return [self at: 0 createActionTo: target message: aSel];
}

- (id <ActionTo>)createActionTo: target message: (SEL)aSel : arg1
{
  return [self at: 0 createActionTo: target message: aSel : arg1];
}

- (id <ActionTo>)createActionTo: target message: (SEL)aSel : arg1 : arg2
{
  return [self at: 0 createActionTo: target message: aSel : arg1 : arg2];
}

- (id <ActionTo>)createActionTo: target message: (SEL)aSel : arg1 : arg2 : arg3
{
  return [self at: 0 createActionTo: target message: aSel : arg1:arg2:arg3];
}

- (id <ActionForEachHomogeneous>)createActionForEachHomogeneous: target message: (SEL)aSel
{
  return [self at: 0 createActionForEachHomogeneous: target message: aSel];
}

- (id <ActionForEach>)createActionForEach: target message: (SEL)aSel
{
  return [self at: 0 createActionForEach: target message: aSel];
}

- (id <ActionForEach>)createActionForEach: target message: (SEL)aSel : arg1
{
  return [self at: 0 createActionForEach: target message: aSel : arg1];
}

- (id <ActionForEach>)createActionForEach: target message: (SEL)aSel : arg1 : arg2
{
  return [self at: 0 createActionForEach: target message: aSel : arg1 : arg2];
}

- (id <ActionForEach>)createActionForEach: target message: (SEL)aSel : arg1 : arg2 : arg3
{
  return [self at: 0 createActionForEach: target message: aSel:arg1:arg2:arg3];
}

- (id <FActionForEachHeterogeneous>)createFActionForEachHeterogeneous: target call: (id <FCall>)call
{
  return [self at: 0 createFActionForEachHeterogeneous: target call: call];
}

- (id <FActionForEachHomogeneous>)createFActionForEachHomogeneous: target call: (id <FCall>)call
{
  return [self at: 0 createFActionForEachHomogeneous: target call: call];
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
          DROP (groupIndex);
        }
      
      // map the action contained in the schedule itself
      
      mapObject (mapalloc, member);
    }
  DROP (index);
  [super mapAllocations: mapalloc];
}

- (void)describe: outputCharStream
{
  [super describe: outputCharStream];
}

- (void)describeForEach: outputCharStream
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
  DROP (index);
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
  DROP (index);
}

@end

//
// ActionConcurrent_c -- action to perform concurrent group within schedule
//

@implementation ActionConcurrent_c
PHASE(Creating)
PHASE(Using)
- (void)_performAction_: (id <Activity>)anActivity
{
  [(id) concurrentGroup _performPlan_];
}

- (id <ActionGroup>)getConcurrentGroup
{
  return concurrentGroup;
}

- (void)mapAllocations: (mapalloc_t)mapalloc
{
  // identify the action group that held the concurrent actions
  mapObject (mapalloc, concurrentGroup);
}

- (void)describe: outputCharStream
{
  char buffer[100];

  [outputCharStream catC: "[ActionConcurrent_c "];
  _obj_formatIDString (buffer, self);
  [outputCharStream catC: buffer];
  [outputCharStream catC: " concurrentGroup:\n"];
  [concurrentGroup describeForEach: outputCharStream];
  [outputCharStream catC: "]\n"];
}

@end

//
// ConcurrentSchedule_c --
//   concurrent group that supports ordering of actions by unsigned integer
//   keys
//

@implementation ConcurrentSchedule_c
PHASE(Creating)

+ createBegin: (id <Zone>)aZone
{
  ActivationOrder_c *obj = [super createBegin: aZone];

  obj->keepEmptyFlag = NO;
  return obj;
}
PHASE(Setting)
PHASE(Using)
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

- (void)mapAllocations: (mapalloc_t)mapalloc
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
PHASE(Creating)
PHASE(Setting)
PHASE(Using)
//
// addLast: --
//   method to sort concurrent merge actions in the order of swarm activation
//
- (void)addLast: mergeAction
{
  MAP_AT_INSERT (self, 
		 (id) ((ActionMerge_c *) mergeAction)->subactivity->activationNumber,
		 mergeAction);
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
PHASE(Creating)
PHASE(Using)
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
  id <Symbol> nextStatus = nil;

  while ([self getCurrentTime] < tVal
         && !COMPLETEDP (nextStatus = [self nextAction]));
  return nextStatus ? nextStatus : [self getStatus];
}

//
// mapAllocations: -- standard method to identify internal allocations
//
- (void)mapAllocations: (mapalloc_t)mapalloc
{
  if (mergeAction)
    {
      mapalloc->descriptor = t_LeafObject;
      mapAlloc (mapalloc, mergeAction);
    }
  [super mapAllocations: mapalloc];
}

//
// dropAllocations: --
//   remove merge action from merge schedule if it is not a current subactivity
//
- (void)dropAllocations: (BOOL)componentAlloc
{
  // remove the merge action for the activity from the merge schedule

  if (mergeAction)
    [((ScheduleIndex_c *) swarmActivity->currentIndex)->collection
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
  id <Action> actionAtIndex, removedAction;
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
          newAction = [ActionChanged 
                        create: getCZone (getZone ((Activity_c *) activity))];
          newAction->actionAtIndex =  (ActionConcurrent_c *)actionAtIndex;
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
          [(id) removedAction dropAllocations: YES];
        }
    }
  
  //
  // Get next action from index, adjust times, and return.
  //
  
  currentAction = MAP_INDEX_NEXTKEY (self, (id *) &currentTime);
  
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
          
          MAP_INDEX_SETLOC (self, Start);
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
  if (activity->swarmActivity
      && (currentAction
          || INITIALIZEDP (activity->swarmActivity->status)))
    {
      swarmIndex = (ScheduleIndex_c *)activity->swarmActivity->currentIndex;

      _activity_insertAction ((Schedule_c *) swarmIndex->collection,
                              currentTime,
			      (CAction *)activity->mergeAction);
      if (currentAction) 
        *status = Holding;
      else
        MAP_INDEX_SETLOC (self, Start);
      
      // if empty schedule just added to new swarm activity, then set to
      // reprocess schedule (should generalize to any empty schedule)
      
      return nil;  // indicate that activity is not ready to run 
    }
  
  // return next action to be processed
  
  if (!currentAction)
    *status = Completed;
  
  return currentAction;
}

- setCurrentTime: (timeval_t)tVal
{
  id member = [self setKey: (id) tVal];
  
  if (member == nil)
    abort ();
  currentTime = tVal;
  currentAction = member;
  return self;
}

- setActivity: (id <Activity>)theActivity
{
  activity = (ScheduleActivity_c *)theActivity;
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
      removedAction = [(id) actionAtIndex->concurrentGroup removeFirst];
      [self prev];
    }
  else
    {
#ifdef FAST
      Class class = getClass (collection);
#endif
      // just remove the action at the index
      removedAction = [super remove];

      // Avoid leaving behind invalid owners in mergeAction
      // `owner' is used in [Schedule remove:], for example
      // first do some common cases before expensive conformsTo
#if SWARM_OSX
      if ([collection conformsToProtocol: @protocol (ConcurrentSchedule)])
#else
#ifdef FAST
      if (class != id_Schedule_c)
        if (class == id_ActivationOrder_c || class == id_ConcurrentSchedule_c
            || [collection conformsTo: @protocol (ConcurrentSchedule)])
#else
      if ([collection conformsTo: @protocol (ConcurrentSchedule)])
#endif
#endif
{
          removedAction->owner =
            ((ConcurrentSchedule_c *) collection)->actionConcurrent->owner;
}
    }
  currentAction = (id <Action>) Removed;
  return removedAction;
}

//
// get -- obtain action at current index
//
- get
{
  id status = currentAction;

  if (REMOVEDP (status) || COMPLETEDP (status))
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
  ORDEREDSET_REMOVE (((Schedule_c *) collection)->activityRefs, activity);
  [super dropAllocations: YES];
}

- (void) describe: outputCharStream
{
  char buffer[200];

  [outputCharStream catC: "["];
  _obj_formatIDString (buffer, self);
  [outputCharStream catC: buffer];
  [outputCharStream catC: "\n"];
  [[self getLoc] describe: outputCharStream];
  sprintf (buffer,"  current time: %ld start time: %ld\n",
           currentTime, startTime);
  [outputCharStream catC: buffer];
  [collection describeForEach: outputCharStream];
  [outputCharStream catC: "]\n"];
}

@end


@implementation ActionChanged_c
PHASE(Creating)
PHASE(Using)
- (void)_performAction_: (id <Activity>)anActivity
{
  //
  // Create the activity for the new concurrent group action, but position it
  // to the first member so that a subsequent nextAction: will get the second
  // member.  Drop the ActionChanged_c object itself since it has served its
  // entire transitory role.
  //
  ScheduleIndex_c *schedIndex;
  id unusedStatus;

  schedIndex = (ScheduleIndex_c *)((Activity_c *) anActivity)->currentIndex;
  schedIndex->currentAction = actionAtIndex;
  unsetMappedAlloc (schedIndex);
  [actionAtIndex _performAction_: anActivity];
  [((Activity_c *) anActivity)->currentSubactivity->currentIndex
                              nextAction: &unusedStatus];
  [self drop];
}

- (void)describe: outputCharStream
{
  char buffer[100];

  [outputCharStream catC: "[ActionChanged: "];
  _obj_formatIDString (buffer, self);
  [outputCharStream catC: buffer];
  [outputCharStream catC: " actionAtIndex: "];
  _obj_formatIDString (buffer, actionAtIndex);
  [outputCharStream catC: buffer];
  [outputCharStream catC: "]\n"];
}

- (void)mapAllocations: (mapalloc_t)mapalloc
{
}

@end

