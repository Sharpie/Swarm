// Swarm library. Copyright (C) 1996 Santa Fe Institute.
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


@implementation Schedule_c

PHASE(Creating)

//
// mix in action plan create-phase methods by source inclusion
//
#define  MIXIN_CREATE
#include "ActionPlan.m"

- (void) setConcurrentGroupType: groupType
{
  concurrentGroupType = groupType;
  setBit( bits, Bit_ConcrntGroupSet, 1 );
}

- (void) setSingletonGroups: (BOOL)singletonGroups
{
  setBit( bits, Bit_SingletonGroups, singletonGroups );
}

- (void) setRelativeTime: (BOOL)relativeTime
{
  setBit( bits, Bit_RelativeTime, relativeTime );
  setBit( bits, Bit_RelTimeSet, 1 );
}

- createEnd
{
  if ( repeatInterval ) {
    if ( ( bits & Bit_RelTimeSet ) && ! ( bits & Bit_RelativeTime ) )
      raiseEvent( InvalidCombination,
        "cannot specify both a repeat interval and absolute time\n" );

    setBit( bits, Bit_RelativeTime, 1 );  // force relative time for repeat
  }
  [(id)self setCompareFunction: _activity_compareIDs];

  if ( createByMessageToCopy( self, createEnd ) ) return self;

  if ( ! concurrentGroupType ) concurrentGroupType = ActionGroup;
  if ( variableDefs ) [self _createVarDefsArray_];
  [super createEnd];
  return self;
}

PHASE(Setting)

- (void) setRepeatInterval: (timeval_t)tVal
{
  if ( tVal == 0 )
    raiseEvent( InvalidArgument,
                "repeat interval must be greater than zero\n" );

  if ( ! getNextPhase( getClass( self ) ) && ! ( bits & Bit_RelativeTime ) )
    raiseEvent( InvalidCombination,
      "cannot specify a repeat interval after schedule created without it\n" );

  repeatInterval = tVal;
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

#include "ActionPlan.m"


- getConcurrentGroupType
{
  return concurrentGroupType;
}

- (BOOL) getSingletonGroups
{
  return bits & Bit_SingletonGroups;
}

- (BOOL) getRelativeTime
{
  return bits & Bit_RelativeTime;
}

- (timeval_t) getRepeatInterval
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
  ActionMerge_c       *mergeAction;

  // initialize new activity to run underneath swarm

  swarmActivity = swarmContext;
  newActivity = [self _createActivity_: activityClass : indexClass];
  newActivity->swarmActivity    = swarmActivity;
  newActivity->activationNumber = swarmActivity->nextActivation++;

  // create merge action for use by swarm in merging schedule subactivities

  newIndex = newActivity->currentIndex;
  mergeAction = [swarmActivity->zone allocIVars: id_ActionMerge_c];
  mergeAction->subactivity = newActivity;
  newActivity->mergeAction = mergeAction;

  // set the starting and current times of the new activity

  swarmIndex = swarmActivity->currentIndex;
  newIndex->startTime = swarmIndex->currentTime;

  // Advance the new index to its first action. The nextAction: message
  // will automatically insert the merge action at schedule's pending time.

  [newIndex nextAction: (id *)&newActivity->status];

  // keep ownerActivity set to nil when not actually running under swarm

  newActivity->ownerActivity = nil;
  return newActivity;
}

//
// createGroup() -- function to create new group for concurrent actions
//
static ActionConcurrent_c *createGroup( Schedule_c *self )
{
  ActionGroup_c       *newGroup;
  ActionConcurrent_c  *newAction;

  // create new concurrent group to receive new action

  newGroup = [self->concurrentGroupType create: self->zone];
  setBit( newGroup->bits, Bit_ConcurrentGroup, 1 );

  newAction = [self->zone allocIVars: id_ActionConcurrent_c];
  newAction->ownerPlan  = (id)self;
  newAction->actionPlan = (id)newGroup;
  return newAction;
}

//
// _activity_insertAction: routine to create concurrent action group if needed
//
void
_activity_insertAction( Schedule_c *self, timeval_t tVal, Action_c *anAction )
{
  BOOL                newKey;
  id                  *memptr;
  Action_c            *existingAction;
  ActionConcurrent_c  *newAction;
  ActionGroup_c       *existingGroup;

  if ( _obj_debug && self->repeatInterval && ( tVal >= self->repeatInterval ) )
    raiseEvent( InvalidArgument,
  "cannot insert action at time greater than or equal to repeat interval\n" );

  // attempt to insert as first action at key

  anAction->ownerPlan = (id)self;
  memptr = &anAction;
  newKey = [self at: (id)tVal memberSlot: &memptr];

  // if no previous action at key, then return unless singleton group required 

  if ( newKey ) {
    if ( ! (self->bits & Bit_SingletonGroups) ) return;
    existingAction = anAction;

  } else {

    // if concurrent group already exists, then just add new action to group

    existingAction = *memptr;
    if ( getClass( existingAction ) == id_ActionConcurrent_c ) {
      existingGroup = (id)((ActionConcurrent_c *)existingAction)->actionPlan;
      anAction->ownerPlan = (id)existingGroup;
      [existingGroup addLast: anAction];
      return;
    }
  }

  // add initial actions to new group

  newAction = createGroup( self );
  newAction->ownerActions = existingAction->ownerActions;  // replace mem links
  *memptr = newAction;
  if ( ! newKey ) {
    existingAction->ownerPlan = newAction->actionPlan;
    [(id)newAction->actionPlan addLast: existingAction];
  }
  anAction->ownerPlan = (id)newAction->actionPlan;
  [(id)newAction->actionPlan addLast: anAction];
}

//
// insertGroup: -- eventual Map function to insert group at key
//
- insertGroup: aKey
{
  ActionConcurrent_c  *existingAction, *newAction;
  id                  *memptr;

  // obtain current entry at key and return if already a concurrent group

  existingAction = [self at: aKey];
  if ( existingAction &&
       respondsTo( existingAction->actionPlan, M(getActivities) ) )
    return existingAction->actionPlan;

  // insert action for new group as first at key

  newAction = createGroup( self );
  memptr = &newAction;
  [self at: aKey memberSlot: &memptr];
  if ( ! existingAction ) return newAction->actionPlan;

  // if existing action at key then insert into new group and return group

  existingAction->ownerPlan = newAction->actionPlan;
  newAction->ownerActions = existingAction->ownerActions;  // replace mem links
  [(id)newAction->actionPlan addLast: existingAction];
  *memptr = newAction;
  return newAction->actionPlan;
}

//
// remove: -- remove action from either schedule or concurrent group
//
- remove: anAction
{
  if ( _obj_debug && ! respondsTo( anAction, M(_performAction_:) ) )
    raiseEvent( InvalidArgument,
      "object to be removed from schedule is not an action\n" );

  if ( respondsTo( ((Action_c *)anAction)->ownerPlan, M(getRelativeTime) ) ) {
    if ( _obj_debug && ((Action_c *)anAction)->ownerPlan != (id)self )
      raiseEvent( InvalidArgument,
        "action to be removed from schedule does not belong to schedule\n" );
    [super remove: anAction];
  } else {
    [(id)((Action_c *)anAction)->ownerPlan remove: anAction];
    if ( [((Action_c *)anAction)->ownerPlan getCount] == 0 ) {
      [super remove: anAction];
      [anAction _dropFrom_: zone];
    }
  }
  return anAction;
}

//
// createAction... -- create actions comprising the action plan
//

- at: (timeval_t)tVal createAction: anActionType
{
  if ( ! respondsTo( anActionType, M(_performPlan_) ) )
    raiseEvent( InvalidArgument, nil );

  return [self at: (timeval_t)tVal
           createActionTo: anActionType message: M(_performPlan_)];
}

- at: (timeval_t)tVal createActionCall: (func_t)fptr
{
  ActionCall_0  *newAction;

  newAction = [zone allocIVars: id_ActionCall_0];
  newAction->funcPtr = fptr;
  _activity_insertAction( self, tVal, newAction );
  return newAction;
}

- at: (timeval_t)tVal createActionCall: (func_t)fptr : arg1
{
  ActionCall_1  *newAction;

  newAction = [zone allocIVars: id_ActionCall_1];
  newAction->funcPtr = fptr;
  newAction->arg1    = arg1;
  _activity_insertAction( self, tVal, newAction );
  return newAction;
}

- at: (timeval_t)tVal createActionCall: (func_t)fptr : arg1 : arg2
{
  ActionCall_2  *newAction;

  newAction = [zone allocIVars: id_ActionCall_2];
  newAction->funcPtr = fptr;
  newAction->arg1    = arg1;
  newAction->arg2    = arg2;
  _activity_insertAction( self, tVal, newAction );
  return newAction;
}

- at: (timeval_t)tVal createActionCall: (func_t)fptr : arg1 : arg2 : arg3
{
  ActionCall_3  *newAction;

  newAction = [zone allocIVars: id_ActionCall_3];
  newAction->funcPtr = fptr;
  newAction->arg1    = arg1;
  newAction->arg2    = arg2;
  newAction->arg3    = arg3;
  _activity_insertAction( self, tVal, newAction );
  return newAction;
}

- at: (timeval_t)tVal createActionTo: target message: (SEL)aSel
{
  ActionTo_0  *newAction;

  newAction = [zone allocIVars: id_ActionTo_0];
  newAction->target   = target;
  newAction->selector = aSel;
  _activity_insertAction( self, tVal, newAction );
  return newAction;
}

- at: (timeval_t)tVal createActionTo: target message: (SEL)aSel : arg1
{
  ActionTo_1  *newAction;

  newAction = [zone allocIVars: id_ActionTo_1];
  newAction->target   = target;
  newAction->selector = aSel;
  newAction->arg1     = arg1;
  _activity_insertAction( self, tVal, newAction );
  return newAction;
}

- at: (timeval_t)tVal createActionTo: target message: (SEL)aSel : arg1 : arg2
{
  ActionTo_2  *newAction;

  newAction = [zone allocIVars: id_ActionTo_2];
  newAction->target   = target;
  newAction->selector = aSel;
  newAction->arg1     = arg1;
  newAction->arg2     = arg2;

  _activity_insertAction( self, tVal, newAction );
  return newAction;
}

- at: (timeval_t)tVal createActionTo: target message: (SEL)aSel:arg1:arg2:arg3;
{
  ActionTo_3  *newAction;

  newAction = [zone allocIVars: id_ActionTo_3];
  newAction->target   = target;
  newAction->selector = aSel;
  newAction->arg1     = arg1;
  newAction->arg2     = arg2;
  newAction->arg3     = arg3;
  _activity_insertAction( self, tVal, newAction );
  return newAction;
}

- at: (timeval_t)tVal createActionForEach: target message: (SEL)aSel
{
  ActionForEach_0  *newAction;

  newAction = [zone allocIVars: id_ActionForEach_0];
  newAction->target   = target;
  newAction->selector = aSel;
  _activity_insertAction( self, tVal, newAction );
  return newAction;
}

- at: (timeval_t)tVal createActionForEach: target message: (SEL)aSel : arg1
{
  ActionForEach_1  *newAction;

  newAction = [zone allocIVars: id_ActionForEach_1];
  newAction->target   = target;
  newAction->selector = aSel;
  newAction->arg1     = arg1;
  _activity_insertAction( self, tVal, newAction );
  return newAction;
}

- at: (timeval_t)tVal createActionForEach: target message: (SEL)aSel:arg1:arg2
{
  ActionForEach_2  *newAction;

  newAction = [zone allocIVars: id_ActionForEach_2];
  newAction->target   = target;
  newAction->selector = aSel;
  newAction->arg1     = arg1;
  newAction->arg2     = arg2;
  _activity_insertAction( self, tVal, newAction );
  return newAction;
}

- at: (timeval_t)tVal createActionForEach: t message: (SEL)aSel:arg1:arg2:arg3;
{
  ActionForEach_2  *newAction;

  newAction = [zone allocIVars: id_ActionForEach_3];
  newAction->target   = t;
  newAction->selector = aSel;
  newAction->arg1     = arg1;
  newAction->arg2     = arg2;
  _activity_insertAction( self, tVal, newAction );
  return newAction;
}

@end


//
// ActionConcurrent_c -- minimal action just for concurrent group of schedule
//
@implementation ActionConcurrent_c

- (void) _performAction_: anActivity
{
  [(id)actionPlan _performPlan_];
}

- (void) _dropFrom_: aZone
{
  id  index, member;

  // drop all the actions that were included in the concurrent group

  index = [(id)actionPlan begin: scratchZone];
  while ( (member = [index next]) ) {
    [index remove];
    [member _dropFrom_: aZone];
  }
  [index drop];

  // drop the concurrent group itself and the action that refers to it

  [actionPlan drop];
  [aZone freeIVars: self];
}

@end


//
// ScheduleActivity_c -- activity to process a Schedule action plan
//

@implementation ScheduleActivity_c

//
// getCurrentTime -- get current time of activity (pending time if holding)
//
- (timeval_t) getCurrentTime
{
  return ((ScheduleIndex_c *)self->currentIndex)->currentTime;
}

//
// _drop_ -- special _drop_ method to free mergeAction along with activity
//
- (void) _drop_
{
  if ( mergeAction ) [zone freeIVars: mergeAction];
  [super _drop_];
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
#include "ActionPlan.m"

//
// nextAction: -- return next action and set currentTime to its key value
//
- nextAction: (id *)status
{
  id                  actionAtIndex, removedAction;
  ActionChanged_c     *newAction;
  ScheduleIndex_c     *swarmIndex;

  // finish any processing of previous action

  if ( currentAction && currentAction != Removed ) {

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

    if ( currentAction != (actionAtIndex = [super get]) ) {
      //!! (later -- when Map implementation has stabilized --
      //!! get actionAtIndex directly from the underlying implementation)
      newAction = [id_ActionChanged_c create: ((Activity_c *)activity)->zone];
      newAction->actionAtIndex = actionAtIndex;
      currentAction = newAction;
      return newAction;     // return special object to handle _performAction_:
    }      // (change object need only be dropped to disappear without a trace)

    //
    // If AutoDrop option, then drop previous action before continuing.
    //
    if ( ((Schedule_c *)collection)->bits & Bit_AutoDrop ) {
      removedAction = [super remove];
      [removedAction _dropFrom_: ((Schedule_c *)collection)->zone];
    }
  }

  //
  // Get next action from index, adjust times, and return.
  //

  currentAction = [self next: (id *)&currentTime];

  if ( currentAction ) {  // action ready to be executed

    // if relative time then readjust current time by start time

    if ( ((Schedule_c *)collection)->bits & Bit_RelativeTime )
      currentTime += startTime;

  } else {  // no more actions to be executed

    // if repeat interval, then start over again

    if ( ((Schedule_c *)collection)->repeatInterval ) {

      startTime += ((Schedule_c *)collection)->repeatInterval;
      if ( startTime < currentTime )
	raiseEvent( SourceMessage,
	  "schedule did not complete soon enough for its scheduled repeat\n" );

      [self setLoc: Start];
      currentAction = [self next: (id *)&currentTime];
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
  if ( ((Activity_c *)activity)->swarmActivity &&
       ( currentAction ||
         ((Activity_c *)activity)->swarmActivity->status == Initialized )  ) {

    swarmIndex = ((Activity_c *)activity)->swarmActivity->currentIndex;
    _activity_insertAction( (id)swarmIndex->collection, currentTime,
			    ((ScheduleActivity_c *)activity)->mergeAction );
    if ( currentAction ) 
      *status = Holding;
    else
      [self setLoc: Start];

    // if empty schedule just added to new swarm activity, then set to
    // reprocess schedule (should generalize to any empty schedule)

    return nil;  // indicate that activity is not ready to run 
  }

  // return next action to be processed

  if ( ! currentAction ) *status = Completed;
  return currentAction;
}

//
// remove -- remove action at current index
//
- remove
{
  ActionConcurrent_c  *actionAtIndex, *removedAction;

  //
  // Get the action at the current index to see if it has changed to a
  // concurrent group.  If it has, then remove the action as the first
  // member of the new group, leave the current action that performs
  // the entire remaining group, and mark currentAction as removed so
  // that the action will not be disturbed by nextAction.  Finally, back
  // up the index ([self prev]) so that a subsequent next will obtain the
  // concurrent group just added.
  //

  if ( currentAction && currentAction != (actionAtIndex = [super get]) ) {

    removedAction = [(id)actionAtIndex->actionPlan removeFirst];
    [self prev];

  } else {  // just remove the action at the index

    removedAction = [super remove];
  }
  currentAction = Removed;
  return removedAction;
}

//
// get -- obtain action at current index
//
- get
{
  if ( currentAction == Removed || currentAction == Completed ) return nil;
  return currentAction;
}

//
// getCurrentTime -- obtain current time from index, which holds it
//
- (timeval_t) getCurrentTime
{
  if ( [self getLoc] == Start ) return startTime;
  return ( startTime + (timeval_t)((entry_t)[listIndex get])->key );
}

//
// drop -- standard drop message for index
//
- (void) drop
{
  [((Schedule_c *)collection)->activityRefs remove: activity];
  [super drop];
}

@end


@implementation ActionChanged_c

- (void) _performAction_: anActivity
{
  //
  // Create the activity for the new concurrent group action, but position it
  // to the first member so that a subsequent nextAction: will get the second
  // member.  Drop the ActionChanged_c object itself since it has served its
  // entire transitory role.
  //
  ScheduleIndex_c  *schedIndex;
  id               unusedStatus;

  schedIndex = ((Activity_c *)anActivity)->currentIndex;
  schedIndex->currentAction = actionAtIndex;
  [actionAtIndex _performAction_: anActivity];
  [((Activity_c *)anActivity)->currentSubactivity->currentIndex
    nextAction: &unusedStatus];
  [self drop];
}

@end
