// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         ActionGroup.m
Description:  collection of actions under partial order constraints 
Library:      activity
*/

#import <activity/ActionGroup.h>
#import <defobj/defalloc.h>


@implementation ActionGroup_c

PHASE(Creating)

//
// mix in action plan create-phase methods by source inclusion
//
#define  MIXIN_CREATE
#include "CompoundAction.m"

- createEnd
{
  if ( createByMessageToCopy( self, createEnd ) ) return self;

  [(id)self setIndexFromMemberLoc: offsetof( CAction, ownerActions )];
  setNextPhase( self );
  setMappedAlloc( self );
  return self;
}

PHASE(Using)

//
// mix in using-phase methods from CompoundAction by source inclusion
//
#define  MIXIN_C

// override parameters for included _createSubactivity_: method
#define  ACTIVITY_CLASS     Activity_c
#define  ACTIVITY_CLASS_ID  id_Activity_c
#define  INDEX_CLASS        GroupIndex_c
#define  INDEX_CLASS_ID     id_GroupIndex_c
#include "CompoundAction.m"

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

  newAction = [getZone( self ) allocIVarsComponent: id_ActionCall_0];
  newAction->funcPtr = fptr;
  [self addLast: newAction];
  return newAction;
}

- createActionCall: (func_t)fptr : arg1
{
  ActionCall_1  *newAction;

  newAction = [getZone( self ) allocIVarsComponent: id_ActionCall_1];
  newAction->funcPtr = fptr;
  newAction->arg1    = arg1;
  [self addLast: newAction];
  return newAction;
}

- createActionCall: (func_t)fptr : arg1 : arg2
{
  ActionCall_2  *newAction;

  newAction = [getZone( self ) allocIVarsComponent: id_ActionCall_2];
  newAction->funcPtr = fptr;
  newAction->arg1    = arg1;
  newAction->arg2    = arg2;
  [self addLast: newAction];
  return newAction;
}

- createActionCall: (func_t)fptr : arg1 : arg2 : arg3
{
  ActionCall_3  *newAction;

  newAction = [getZone( self ) allocIVarsComponent: id_ActionCall_3];
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

  newAction = [getZone( self ) allocIVarsComponent: id_ActionTo_0];
  newAction->target   = target;
  newAction->selector = aSel;
  [self addLast: newAction];
  return newAction;
}

- createActionTo: target message: (SEL)aSel : arg1
{
  ActionTo_1  *newAction;

  newAction = [getZone( self ) allocIVarsComponent: id_ActionTo_1];
  newAction->target   = target;
  newAction->selector = aSel;
  newAction->arg1     = arg1;
  [self addLast: newAction];
  return newAction;
}

- createActionTo: target message: (SEL)aSel : arg1 : arg2
{
  ActionTo_2  *newAction;

  newAction = [getZone( self ) allocIVarsComponent: id_ActionTo_2];
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

  newAction = [getZone( self ) allocIVarsComponent: id_ActionTo_3];
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

  newAction = [getZone( self ) allocIVarsComponent: id_ActionForEach_0];
  newAction->target   = target;
  newAction->selector = aSel;
  [self addLast: newAction];
  return newAction;
}

- createActionForEach: target message: (SEL)aSel : arg1
{
  ActionForEach_1  *newAction;

  newAction = [getZone( self ) allocIVarsComponent: id_ActionForEach_1];
  newAction->target   = target;
  newAction->selector = aSel;
  newAction->arg1     = arg1;
  [self addLast: newAction];
  return newAction;
}

- createActionForEach: target message: (SEL)aSel : arg1 : arg2
{
  ActionForEach_2  *newAction;

  newAction = [getZone( self ) allocIVarsComponent: id_ActionForEach_2];
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

  newAction = [getZone( self ) allocIVarsComponent: id_ActionForEach_3];
  newAction->target   = target;
  newAction->selector = aSel;
  newAction->arg1     = arg1;
  newAction->arg2     = arg2;
  newAction->arg3     = arg3;
  [self addLast: newAction];
  return newAction;
}

//
// mapAllocations: -- standard method to identify internal allocations
//
- (void) mapAllocations: (mapalloc_t)mapalloc
{
  id  index, member, nextMember;

  if ( activityRefs ) mapObject( mapalloc, activityRefs );

  index = [self begin: scratchZone];
  nextMember = [index next];
  while ( (member = nextMember) ) {
    nextMember = [index next];
    mapObject( mapalloc, member );
  }
  // no [super mapAllocations: mapalloc] because all links are internal
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
// The schedule containing a concurrent group is responsible for mapping all
// its actions including those in concurrent groups.  The concurrent group
// itself must map only its own collection structure.  For the underlying
// allocation of an ActionGroup, simply not setting the MappedAlloc bit is
// sufficient to leave the actions alone.
//
- createEnd
{
  if ( createByMessageToCopy( self, createEnd ) ) return self;

  [(id)self setIndexFromMemberLoc: offsetof( CAction, ownerActions )];
  setMappedAlloc( self );
  setNextPhase( self );
  return self;
}

PHASE( Using )

//
// _setActionConcurrent_ --
//   internal method to set link back to action that refers to concurrent group
//
- (void) _setActionConcurrent_: action
{
  actionConcurrent = action;
}

//
// _getEmptyActionConcurrent_ --
//   internal method to dispose of concurrent group if it has become empty
//
- _getEmptyActionConcurrent_
{
  if ( count ) return nil;
  return actionConcurrent;
}

//
// mapAllocations: -- standard method to identify internal allocations
//
- (void) mapAllocations: (mapalloc_t)mapalloc
{
  if ( activityRefs ) mapObject( mapalloc, activityRefs );

  // Avoid drop of members that ActionGroup would perform, since Schedule is
  // responsible for dropping all members.
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
#include "CompoundAction.m"


//
// nextAction: -- return next action, if any, or Holding status
//
- nextAction: (id *)status
{
  id removedAction, nextAction;

  // if AutoDrop option and index at a valid position then drop previous action

  if ( ((ActionGroup_c *)collection)->bits & BitAutoDrop && position > 0 ) {
    removedAction = [self remove];
    [getZone( (ActionGroup_c *)collection ) freeIVarsComponent: removedAction];
  }

  // get next action to be executed

  nextAction = [self next];
  if ( ! nextAction ) *status = Completed;
  return nextAction;
}

//
// dropAllocations: -- drop index as component of activity
//
- (void) dropAllocations: (BOOL)componentAlloc
{
  [((ActionGroup_c *)collection)->activityRefs remove: activity];
  [super dropAllocations: 1];
}

@end


//
// ForEachActivity_c -- activity to process members of a ForEach action
//

@implementation ForEachActivity_c

+ _create_: forEachAction : anActivity
{
  Activity_c      *owner, *newActivity;
  id              ownerZone;
  ForEachIndex_c  *newIndex;

  // create new activity containing custom index into target collection

  owner = anActivity;
  ownerZone = getZone( owner );
  newActivity = [ownerZone allocIVarsComponent: id_ForEachActivity_c];
  newIndex    = [ownerZone allocIVarsComponent: id_ForEachIndex_c];

  setMappedAlloc( newActivity );
  setMappedAlloc( newIndex );

  newActivity->ownerActivity  = anActivity;
  newActivity->status         = Initialized;
  newActivity->breakFunction  = owner->breakFunction;
  newActivity->currentIndex   = newIndex;
  newIndex->activity          = anActivity;

  newIndex->memberIndex = [((ActionForEach_0 *)forEachAction)->target
                             begin: getCZone( ownerZone )];
  newIndex->memberAction = [ownerZone copyIVarsComponent: forEachAction];
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

- (void) mapAllocations: (mapalloc_t)mapalloc
{
  mapObject( mapalloc, memberIndex );
  mapObject( mapalloc, memberAction );
}

@end
