// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         ActionGroup.m
Description:  collection of actions under partial order constraints 
Library:      activity
*/

#import <activity/ActionGroup.h>


@implementation ActionGroup_c

PHASE(Creating)

//
// mix in action plan create-phase methods by source inclusion
//
#define  MIXIN_CREATE
#include "ActionPlan.m"

- createEnd
{
  if ( createByMessageToCopy( self, createEnd ) ) return self;

  if ( variableDefs ) [self _createVarDefsArray_];
  [(id)self setIndexFromMemberLoc: offsetof( Action_c, ownerActions )];
  setNextPhase( self );
  return self;
}

PHASE(Using)

//
// mix in action plan finalized instance methods by source inclusion
//
#define  MIXIN_C

// override parameters for included _createSubactivity_: method
#define  ACTIVITY_CLASS     Activity_c
#define  ACTIVITY_CLASS_ID  id_Activity_c
#define  INDEX_CLASS        GroupIndex_c
#define  INDEX_CLASS_ID     id_GroupIndex_c
#include "ActionPlan.m"

//
// _activateUnderSwarm_::: -- start new activity to run under swarm
//
- _activateUnderSwarm_: activityClass : indexClass : swarmContext
{
  id          newSchedule;

  // create a special schedule of one action and start it to run under
  // swarm instead

  newSchedule = [Schedule createBegin: _activity_zone];
  [newSchedule setRelativeTime: 1];
  newSchedule = [newSchedule createEnd];

  [newSchedule at: 0 createAction: self];
  return [newSchedule activateIn: swarmContext];
}

//
// createAction... -- create actions comprising the action plan
//

- createAction: anActionType
{
  if ( ! respondsTo( anActionType, M(_performPlan_) ) )
    raiseEvent( InvalidArgument, nil );

  return [self createActionTo: anActionType message: M(_performPlan_)];
}

- createActionCall: (func_t)fptr
{
  ActionCall_0  *newAction;

  newAction = [zone allocIVars: id_ActionCall_0];
  newAction->funcPtr = fptr;
  [self addLast: newAction];
  return newAction;
}

- createActionCall: (func_t)fptr : arg1
{
  ActionCall_1  *newAction;

  newAction = [zone allocIVars: id_ActionCall_1];
  newAction->funcPtr = fptr;
  newAction->arg1    = arg1;
  [self addLast: newAction];
  return newAction;
}

- createActionCall: (func_t)fptr : arg1 : arg2
{
  ActionCall_2  *newAction;

  newAction = [zone allocIVars: id_ActionCall_2];
  newAction->funcPtr = fptr;
  newAction->arg1    = arg1;
  newAction->arg2    = arg2;
  [self addLast: newAction];
  return newAction;
}

- createActionCall: (func_t)fptr : arg1 : arg2 : arg3
{
  ActionCall_3  *newAction;

  newAction = [zone allocIVars: id_ActionCall_3];
  newAction->funcPtr = fptr;
  newAction->arg1    = arg1;
  newAction->arg2    = arg2;
  newAction->arg3    = arg3;
  [self addLast: newAction];
  return newAction;
}

- createActionTo: target message: (SEL)aSel
{
  ActionTo_0  *newAction;

  newAction = [zone allocIVars: id_ActionTo_0];
  newAction->target   = target;
  newAction->selector = aSel;
  [self addLast: newAction];
  return newAction;
}

- createActionTo: target message: (SEL)aSel : arg1
{
  ActionTo_1  *newAction;

  newAction = [zone allocIVars: id_ActionTo_1];
  newAction->target   = target;
  newAction->selector = aSel;
  newAction->arg1     = arg1;
  [self addLast: newAction];
  return newAction;
}

- createActionTo: target message: (SEL)aSel : arg1 : arg2
{
  ActionTo_2  *newAction;

  newAction = [zone allocIVars: id_ActionTo_2];
  newAction->target   = target;
  newAction->selector = aSel;
  newAction->arg1     = arg1;
  newAction->arg2     = arg2;
  [self addLast: newAction];
  return newAction;
}

- createActionTo: target message: (SEL)aSel : arg1 : arg2 : arg3
{
  ActionTo_3  *newAction;

  newAction = [zone allocIVars: id_ActionTo_3];
  newAction->target   = target;
  newAction->selector = aSel;
  newAction->arg1     = arg1;
  newAction->arg2     = arg2;
  newAction->arg3     = arg3;
  [self addLast: newAction];
  return newAction;
}

- createActionForEach: target message: (SEL)aSel
{
  ActionForEach_0  *newAction;

  newAction = [zone allocIVars: id_ActionForEach_0];
  newAction->target   = target;
  newAction->selector = aSel;
  [self addLast: newAction];
  return newAction;
}

- createActionForEach: target message: (SEL)aSel : arg1
{
  ActionForEach_1  *newAction;

  newAction = [zone allocIVars: id_ActionForEach_1];
  newAction->target   = target;
  newAction->selector = aSel;
  newAction->arg1     = arg1;
  [self addLast: newAction];
  return newAction;
}

- createActionForEach: target message: (SEL)aSel : arg1 : arg2
{
  ActionForEach_2  *newAction;

  newAction = [zone allocIVars: id_ActionForEach_2];
  newAction->target   = target;
  newAction->selector = aSel;
  newAction->arg1     = arg1;
  newAction->arg2     = arg2;
  [self addLast: newAction];
  return newAction;
}

- createActionForEach: target message: (SEL)aSel : arg1 : arg2 : arg3
{
  ActionForEach_3  *newAction;

  newAction = [zone allocIVars: id_ActionForEach_3];
  newAction->target   = target;
  newAction->selector = aSel;
  newAction->arg1     = arg1;
  newAction->arg2     = arg2;
  newAction->arg3     = arg3;
  [self addLast: newAction];
  return newAction;
}

@end

//
// GroupActivity_c -- activity to process a concurrent group of a schedule
//

@implementation GroupActivity_c
@end


//
// GroupIndex_c -- index to traverse actions of a group
//

@implementation GroupIndex_c

//
// mix in action plan index methods by source inclusion
//
#define  MIXIN_INDEX
#include "ActionPlan.m"


//
// nextAction: -- return next action, if any, or Holding status
//
- nextAction: (id *)status
{
  id removedAction, nextAction;

  // if AutoDrop option and index at a valid position then drop previous action

  if ( ((ActionGroup_c *)collection)->bits & Bit_AutoDrop && position > 0 ) {
    removedAction = [self remove];
    [((ActionGroup_c *)collection)->zone freeIVars: removedAction];
  }

  // get next action to be executed

  nextAction = [self next];
  if ( ! nextAction ) *status = Completed;
  return nextAction;
}

//
// drop -- standard drop message for index
//
- (void) drop
{
  [((ActionGroup_c *)collection)->activityRefs remove: activity];
  [super drop];
}

@end


//
// ForEachActivity_c -- activity to process members of a ForEach action
//

@implementation ForEachActivity_c

+ _create_: forEachAction : anActivity
{
  Activity_c      *owner, *newActivity;
  ForEachIndex_c  *newIndex;

  // create new activity containing custom index into target collection

  owner = anActivity;
  newActivity = [owner->zone allocIVars: id_ForEachActivity_c];
  newIndex    = [owner->zone allocIVars: id_ForEachIndex_c];

  newActivity->zone           = owner->zone;
  newActivity->ownerActivity  = anActivity;
  newActivity->status         = Initialized;
  newActivity->breakFunction  = owner->breakFunction;
  newActivity->currentIndex   = newIndex;
  newIndex->activity          = anActivity;

  newIndex->memberIndex =
    [((ActionForEach_0 *)forEachAction)->target begin: owner->zone];
  newIndex->memberAction = [owner->zone copyIVars: forEachAction];
  ((ActionForEach_0 *)newIndex->memberAction)->target = nil;

  // set currentSubactivity in the activity that called _performAction_

  owner->currentSubactivity = (id)newActivity;
  return newIndex->memberAction;
}

- getCurrentMember
{
  return [currentIndex get];
}

@end


@implementation ForEachIndex_c

- nextAction: (id *)status
{
  // reload and return the action being used to execute each member

  ((ActionForEach_0 *)memberAction)->target = [memberIndex next];
  if ( ((ActionForEach_0 *)memberAction)->target ) return memberAction;
  *status = Completed;
  return nil;
}

- get
{
  if ( memberAction->target == nil ) return nil;
  return memberAction;
}

- getLoc
{
  return [memberIndex getLoc];
}

- getHoldType
{
  return nil;
}

- (void) drop
{
  [(id)memberIndex drop];
  [((Activity_c *)activity)->zone freeIVars: memberAction];
  [((Activity_c *)activity)->zone freeIVars: self];
}

@end
