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
  [(id)self setCompareFunction: compareIDs];

  if ( createByMessageToCopy( self, createEnd ) ) return self;

  if ( ! concurrentGroupType ) concurrentGroupType = ConcurrentGroup;
  [super createEnd];
  return self;
}

PHASE(Setting)

- (void) setRepeatInterval: (timeval_t)rptInterval
{
  if ( rptInterval == 0 )
    raiseEvent( InvalidArgument,
                "repeat interval must be greater than zero\n" );

  if ( ! getNextPhase( getClass( self ) ) && ! ( bits & Bit_RelativeTime ) )
    raiseEvent( InvalidCombination,
      "cannot specify a repeat interval after schedule created without it\n" );

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
  newActivity =
    [self _createActivity_: swarmActivity : activityClass : indexClass];
  newActivity->ownerActivity    = nil;
  newActivity->swarmActivity    = swarmActivity;
  newActivity->activationNumber = swarmActivity->nextActivation++;

  // create merge action for use by swarm in merging schedule subactivities

  newIndex = newActivity->currentIndex;
  mergeAction =
    [getZone( swarmActivity ) allocIVarsComponent: id_ActionMerge_c];
  setMappedAlloc( mergeAction );
  mergeAction->subactivity = newActivity;
  newActivity->mergeAction = mergeAction;

  // set the starting and current times of the new activity

  swarmIndex = swarmActivity->currentIndex;
  newIndex->startTime = swarmIndex->currentTime;

  // Advance the new index to its first action. The nextAction: message
  // will automatically insert the merge action at schedule's pending time.

  [newIndex nextAction: (id *)&newActivity->status];

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

  newGroup = [self->concurrentGroupType create: getComponentZone( self )];
  setBit( newGroup->bits, Bit_ConcurrentGroup, 1 );

  newAction = [getZone( self ) allocIVarsComponent: id_ActionConcurrent_c];
  setMappedAlloc( newAction );
  newAction->owner      = (id)self;
  newAction->actionPlan = (id)newGroup;
  return newAction;
}

//
// _activity_insertAction: routine to create concurrent action group if needed
//
void _activity_insertAction( Schedule_c *self, timeval_t tVal,
                             CAction *anAction )
{
  BOOL                newKey;
  id                  *memptr;
  CAction             *existingAction;
  ActionConcurrent_c  *newAction;
  ActionGroup_c       *existingGroup;

  if ( _obj_debug && self->repeatInterval && ( tVal >= self->repeatInterval ) )
    raiseEvent( InvalidArgument,
  "cannot insert action at time greater than or equal to repeat interval\n" );

  // attempt to insert as first action at key

  anAction->owner = (id)self;
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
      anAction->owner = (id)existingGroup;
      [existingGroup addLast: anAction];
      return;
    }
  }

  // add initial actions to new group

  newAction = createGroup( self );
  newAction->ownerActions = existingAction->ownerActions;  // replace mem links
  *memptr = newAction;
  if ( ! newKey ) {
    existingAction->owner = newAction->actionPlan;
    [(id)newAction->actionPlan addLast: existingAction];
  }
  anAction->owner = (id)newAction->actionPlan;
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

  existingAction->owner = newAction->actionPlan;
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

  if ( respondsTo( ((CAction *)anAction)->owner, M(getRelativeTime) ) ) {
    if ( _obj_debug && ((CAction *)anAction)->owner != (id)self )
      raiseEvent( InvalidArgument,
        "action to be removed from schedule does not belong to schedule\n" );
    [super remove: anAction];
  } else {  // concurrent group
    [(id)((CAction *)anAction)->owner remove: anAction];
    if ( [((CAction *)anAction)->owner getCount] == 0 ) {
      //!! ?? is this right?  how get the concurrent action to be removed?
      [super remove: anAction];
      raiseEvent( SourceMessage,
        "removing concurrent group but not action\n" );
      [anAction dropAllocations: 1];
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

  newAction = [getZone( self ) allocIVarsComponent: id_ActionCall_0];
  newAction->funcPtr = fptr;
  _activity_insertAction( self, tVal, newAction );
  return newAction;
}

- at: (timeval_t)tVal createActionCall: (func_t)fptr : arg1
{
  ActionCall_1  *newAction;

  newAction = [getZone( self ) allocIVarsComponent: id_ActionCall_1];
  newAction->funcPtr = fptr;
  newAction->arg1    = arg1;
  _activity_insertAction( self, tVal, newAction );
  return newAction;
}

- at: (timeval_t)tVal createActionCall: (func_t)fptr : arg1 : arg2
{
  ActionCall_2  *newAction;

  newAction = [getZone( self ) allocIVarsComponent: id_ActionCall_2];
  newAction->funcPtr = fptr;
  newAction->arg1    = arg1;
  newAction->arg2    = arg2;
  _activity_insertAction( self, tVal, newAction );
  return newAction;
}

- at: (timeval_t)tVal createActionCall: (func_t)fptr : arg1 : arg2 : arg3
{
  ActionCall_3  *newAction;

  newAction = [getZone( self ) allocIVarsComponent: id_ActionCall_3];
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

  newAction = [getZone( self ) allocIVarsComponent: id_ActionTo_0];
  newAction->target   = target;
  newAction->selector = aSel;
  _activity_insertAction( self, tVal, newAction );
  return newAction;
}

- at: (timeval_t)tVal createActionTo: target message: (SEL)aSel : arg1
{
  ActionTo_1  *newAction;

  newAction = [getZone( self ) allocIVarsComponent: id_ActionTo_1];
  newAction->target   = target;
  newAction->selector = aSel;
  newAction->arg1     = arg1;
  _activity_insertAction( self, tVal, newAction );
  return newAction;
}

- at: (timeval_t)tVal createActionTo: target message: (SEL)aSel : arg1 : arg2
{
  ActionTo_2  *newAction;

  newAction = [getZone( self ) allocIVarsComponent: id_ActionTo_2];
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

  newAction = [getZone( self ) allocIVarsComponent: id_ActionTo_3];
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

  newAction = [getZone( self ) allocIVarsComponent: id_ActionForEach_0];
  newAction->target   = target;
  newAction->selector = aSel;
  _activity_insertAction( self, tVal, newAction );
  return newAction;
}

- at: (timeval_t)tVal createActionForEach: target message: (SEL)aSel : arg1
{
  ActionForEach_1  *newAction;

  newAction = [getZone( self ) allocIVarsComponent: id_ActionForEach_1];
  newAction->target   = target;
  newAction->selector = aSel;
  newAction->arg1     = arg1;
  _activity_insertAction( self, tVal, newAction );
  return newAction;
}

- at: (timeval_t)tVal createActionForEach: target message: (SEL)aSel:arg1:arg2
{
  ActionForEach_2  *newAction;

  newAction = [getZone( self ) allocIVarsComponent: id_ActionForEach_2];
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

  newAction = [getZone( self ) allocIVarsComponent: id_ActionForEach_3];
  newAction->target   = t;
  newAction->selector = aSel;
  newAction->arg1     = arg1;
  newAction->arg2     = arg2;
  _activity_insertAction( self, tVal, newAction );
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
- (void) mapAllocations: (mapalloc_t)mapalloc
{
  id  index, member, groupIndex, groupMember, nextMember;

  if ( activityRefs ) mapObject( mapalloc, activityRefs );

  index = [self begin: scratchZone];
  while ( (member = [index next]) ) {

    // if action is for a concurrent group, then map all actions in the group

    if ( getClass( member ) == id_ActionConcurrent_c ) {
      groupIndex = [(id)((ActionConcurrent_c *)member)->actionPlan
                     begin: scratchZone];
      nextMember = [groupIndex next];
      while ( (groupMember = nextMember) ) {
        nextMember = [groupIndex next];
        mapObject( mapalloc, groupMember );
      }
      [groupIndex drop];
    }

    // map the action contained in the schedule itself

    mapObject( mapalloc, member );
  }
  [index drop];
  [super mapAllocations: mapalloc];
}

@end

//
// ConcurrentGroup_c -- action group that does not own its member actions
//
@implementation ConcurrentGroup_c

PHASE(Creating)

//
// createEnd --
//   same create method as ActionGroup, only do *not* set MappedAlloc bit
//
- createEnd
{
  if ( createByMessageToCopy( self, createEnd ) ) return self;

  [(id)self setIndexFromMemberLoc: offsetof( CAction, ownerActions )];
  setNextPhase( self );
  return self;
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

//
// mapAllocations: -- standard method to identify internal allocations
//
- (void) mapAllocations: (mapalloc_t)mapalloc
{
  // identify the action group that held the concurrent actions

  mapObject( mapalloc, actionPlan );
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
// stepUntil: -- advance activity until requested time has been reached
//
- stepUntil: (timeval_t)tVal
{
  id  nextStatus = nil;

  while ( [self getCurrentTime] < tVal &&
          (nextStatus = [self next]) != Completed );
  return ( nextStatus ? nextStatus : [self getStatus] );
}

//
// mapAllocations: -- standard method to identify internal allocations
//
- (void) mapAllocations: (mapalloc_t)mapalloc
{
  [super mapAllocations: mapalloc];
  if ( mergeAction ) {
    mapalloc->descriptor = t_LeafObject;
    mapAlloc( mapalloc, mergeAction );
  }
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
      newAction =
        [id_ActionChanged_c create: getZone( (Activity_c *)activity )];
      newAction->actionAtIndex = actionAtIndex;
      currentAction = newAction;
      setMappedAlloc( self );
      return newAction;     // return special object to handle _performAction_:
    }      // (change object need only be dropped to disappear without a trace)

    //
    // If AutoDrop option, then drop previous action before continuing.
    //
    if ( ((Schedule_c *)collection)->bits & Bit_AutoDrop ) {
      removedAction = [super remove];
      [removedAction dropAllocations: 1];
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
  if ( ((ScheduleActivity_c *)activity)->swarmActivity &&
       ( currentAction ||
         ((ScheduleActivity_c *)activity)->swarmActivity->status ==
         Initialized )  ) {

    swarmIndex =
      ((ScheduleActivity_c *)activity)->swarmActivity->currentIndex;
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
  return ( startTime + (timeval_t)((mapentry_t)[listIndex get])->key );
}

//
// mapAllocations: -- standard method to identify internal allocations
//
- (void) mapAllocations: (mapalloc_t)mapalloc
{
  if ( getClass( currentAction ) == id_ActionChanged_c )
    mapObject( mapalloc, currentAction );
}

//
// dropAllocations: -- drop index as component of activity
//
- (void) dropAllocations: (BOOL)componentAlloc
{
  [((Schedule_c *)collection)->activityRefs remove: activity];
  [super dropAllocations: 1];
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
  unsetMappedAlloc( schedIndex );
  [actionAtIndex _performAction_: anActivity];
  [((Activity_c *)anActivity)->currentSubactivity->currentIndex
    nextAction: &unusedStatus];
  [self drop];
}

@end
