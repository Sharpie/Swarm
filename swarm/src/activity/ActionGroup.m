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
Name:         ActionGroup.m
Description:  collection of actions under partial order constraints 
Library:      activity
*/

#import <activity/ActionGroup.h>
#import <activity/Action.h> // CAction
#import <defobj/defalloc.h>
#include <misc.h> // abort

#import <defobj/macros.h>
#import <collections/macros.h>

@implementation ActionGroup_c

PHASE(Creating)

//
// mix in action plan create-phase methods by source inclusion
//
#define  MIXIN_CREATE
#include "CompoundAction.m"

- createEnd
{
  if (createByMessageToCopy (self, createEnd))
    return self;

  [(id) self setIndexFromMemberLoc: offsetof (CAction, ownerActions)];
  setNextPhase (self);
  setMappedAlloc (self);
  return self;
}

PHASE(Setting)

#define MIXIN_SET
#include "CompoundAction.m"

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
- (id)_activateUnderSwarm_: (Class)activityClass 
                                     : (Class)indexClass
                                     : swarmContext
                                     : (Zone_c *)swarmZone
{
  id newSchedule;

  // create a special schedule of one action and start it to run under
  // swarm instead

  newSchedule = [Schedule createBegin: swarmZone];
  [newSchedule setRelativeTime: YES];
  newSchedule = [newSchedule createEnd];

  [newSchedule at: 0 createAction: self];
  return [newSchedule activateIn: swarmContext];
}

//
// createAction... -- create actions comprising the action plan
//

- (id <FAction>)createFAction: call
{
  id <FAction> faction;
  faction = [FAction createBegin: getCZone (getZone (self))];
  [faction setCall: call];
  faction = [faction createEnd];
  [self addLast: faction];
  return faction;
}

- createAction: anActionType
{
  if (!respondsTo (anActionType, M(_performPlan_)))
    raiseEvent (InvalidArgument, nil);

  return [self createActionTo: anActionType message: M(_performPlan_)];
}

- (id <ActionCall>)createActionCall: (func_t)fptr
{
  id <ActionCall> newAction;

  newAction = [ActionCall createBegin: getCZone (getZone (self))];
  [newAction setFunctionPointer: fptr];
  newAction = [newAction createEnd];
  [self addLast: newAction];
  return newAction;
}

- (id <ActionCall>)createActionCall: (func_t)fptr : arg1
{
  id <ActionCall> newAction =
    [ActionCall createBegin: getCZone (getZone (self))];
  [newAction setFunctionPointer: fptr];
  [newAction setArg1: arg1];
  newAction = [newAction createEnd];
  [self addLast: newAction];
  return newAction;
}

- (id <ActionCall>)createActionCall: (func_t)fptr : arg1 : arg2
{
  id <ActionCall> newAction =
    [ActionCall createBegin: getCZone (getZone (self))];
  [newAction setFunctionPointer: fptr];
  [newAction setArg1: arg1];
  [newAction setArg2: arg2];
  newAction = [newAction createEnd];
  [self addLast: newAction];
  return newAction;
}

- (id <ActionCall>)createActionCall: (func_t)fptr : arg1 : arg2 : arg3
{
  id <ActionCall> newAction =
    [ActionCall createBegin: getCZone (getZone (self))];
  [newAction setFunctionPointer: fptr];
  [newAction setArg1: arg1];
  [newAction setArg2: arg2];
  [newAction setArg3: arg3];
  newAction = [newAction createEnd];
  [self addLast: newAction];
  return newAction;
}

- (id <ActionTo>)createActionTo: target message: (SEL)aSel
{
  id <ActionTo> newAction = [ActionTo createBegin: getCZone (getZone (self))];
  [newAction setTarget: target];
  [newAction setMessageSelector: aSel];
  newAction = [newAction createEnd];
  [self addLast: newAction];
  return newAction;
}

- (id <ActionTo>)createActionTo: target message: (SEL)aSel : arg1
{
  id <ActionTo> newAction = [ActionTo createBegin: getCZone (getZone (self))];
  [newAction setTarget: target];
  [newAction setMessageSelector: aSel];
  [newAction setArg1: arg1];
  newAction = [newAction createEnd];
  [self addLast: newAction];
  return newAction;
}

- (id <ActionTo>)createActionTo: target message: (SEL)aSel : arg1 : arg2
{
  id <ActionTo> newAction = [ActionTo createBegin: getCZone (getZone (self))];
  [newAction setTarget: target];
  [newAction setMessageSelector: aSel];
  [newAction setArg1: arg1];
  [newAction setArg2: arg2];
  newAction = [newAction createEnd];
  [self addLast: newAction];
  return newAction;
}

- (id <ActionTo>)createActionTo: target message: (SEL)aSel : arg1 : arg2 : arg3
{
  id <ActionTo> newAction = [ActionTo createBegin: getCZone (getZone (self))];
  [newAction setTarget: target];
  [newAction setMessageSelector: aSel];
  [newAction setArg1: arg1];
  [newAction setArg2: arg2];
  [newAction setArg3: arg3];
  newAction = [newAction createEnd];
  [self addLast: newAction];
  return newAction;
}

- (id <ActionForEachHomogeneous>)createActionForEachHomogeneous: target message: (SEL)aSel
{
  id <ActionForEachHomogeneous> newAction =
    [ActionForEachHomogeneous createBegin: getCZone (getZone (self))];
  [newAction setTarget: target];
  [newAction setMessageSelector: aSel];
  newAction = [newAction createEnd];
  [self addLast: newAction];
  return newAction;
}

- (id <ActionForEach>)createActionForEach: target message: (SEL)aSel
{
  id <ActionForEach> newAction =
    [ActionForEach createBegin: getCZone (getZone (self))];
  [newAction setTarget: target];
  [newAction setMessageSelector: aSel];
  newAction = [newAction createEnd];
  [self addLast: newAction];
  return newAction;
}

- (id <ActionForEach>)createActionForEach: target message: (SEL)aSel : arg1
{
  id <ActionForEach> newAction =
    [ActionForEach createBegin: getCZone (getZone (self))];
  [newAction setTarget: target];
  [newAction setMessageSelector: aSel];
  [newAction setArg1: arg1];
  newAction = [newAction createEnd];
  [self addLast: newAction];
  return newAction;
}

- (id <ActionForEach>)createActionForEach: target message: (SEL)aSel : arg1 : arg2
{
  id <ActionForEach> newAction =
    [ActionForEach createBegin: getCZone (getZone (self))];
  [newAction setTarget: target];
  [newAction setMessageSelector: aSel];
  [newAction setArg1: arg1];
  [newAction setArg2: arg2];
  newAction = [newAction createEnd];
  [self addLast: newAction];
  return newAction;
}

- (id <ActionForEach>)createActionForEach: target message: (SEL)aSel : arg1 : arg2 : arg3
{
  id <ActionForEach> newAction =
    [ActionForEach createBegin: getCZone (getZone (self))];
  [newAction setTarget: target];
  [newAction setMessageSelector: aSel];
  [newAction setArg1: arg1];
  [newAction setArg2: arg2];
  [newAction setArg3: arg3];
  newAction = [newAction createEnd];
  [self addLast: newAction];
  return newAction;
}

- (id <FActionForEachHomogeneous>)createFActionForEachHomogeneous: target call: (id <FCall>)call
{
  id <FActionForEachHomogeneous> newAction =
    [FActionForEachHomogeneous createBegin: getCZone (getZone (self))];
  [newAction setTarget: target];
  [newAction setCall: call];
  newAction = [newAction createEnd];
  [self addLast: newAction];
  return newAction;
}

- (id <FActionForEachHeterogeneous>)createFActionForEachHeterogeneous: target call: (id <FCall>)call
{
  id <FActionForEachHeterogeneous> newAction =
    [FActionForEachHeterogeneous createBegin: getCZone (getZone (self))];
  [newAction setTarget: target];
  [newAction setCall: call];
  newAction = [newAction createEnd];
  [self addLast: newAction];
  return newAction;
}
  
- _createPermutedIndex_: aZone activity: activity
{
  return [[[[GroupPermutedIndex_c createBegin: aZone]
             setCollection: self]
            setActivity: activity]
           createEnd];
}

//
// mapAllocations: -- standard method to identify internal allocations
//
- (void)mapAllocations: (mapalloc_t)mapalloc
{
  id  index, member, nextMember;

  if (activityRefs)
    mapObject (mapalloc, activityRefs);

  index = [self begin: scratchZone];
  nextMember = [index next];
  while ((member = nextMember))
    {
      nextMember = [index next];
      mapObject (mapalloc, member);
    }
  DROP (index);
  // no [super mapAllocations: mapalloc] because all links are internal
}

- (void)describe: outputCharStream
{
  [super describe: outputCharStream];
}

- (void)describeForEach: outputCharStream
{
  char buffer[100];
  id index, action;

  index = [self begin: scratchZone];
  while ((action = [index next])) 
    {
      sprintf (buffer, "action is: ");
      [outputCharStream catC: buffer];
      [action describe: outputCharStream];
    }
  DROP (index);
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
  if (createByMessageToCopy (self, createEnd))
    return self;
  
  [(id) self setIndexFromMemberLoc: offsetof (CAction, ownerActions)];
  setMappedAlloc (self);
  setNextPhase (self);
  return self;
}

PHASE(Setting)
PHASE(Using)

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

//
// mapAllocations: -- standard method to identify internal allocations
//
- (void)mapAllocations: (mapalloc_t)mapalloc
{
  if (activityRefs)
    mapObject (mapalloc, activityRefs);

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
PHASE(Creating)
//
// mix in action plan index methods by source inclusion
//
#define  MIXIN_INDEX
#include "CompoundAction.m"

- setActivity: (id <Activity>)theActivity
{
  activity = theActivity;
  return self;
}
PHASE(Using)
//
// nextAction: -- return next action, if any, or Holding status
//
- nextAction: (id *)status
{
  id nextAction;

  // if AutoDrop option and index at a valid position then drop previous action
 
  if (((ActionGroup_c *) collection)->bits & BitAutoDrop && position > 0)
    {
      id removedAction = [self remove];
      [removedAction dropAllocations: YES];
    }

  // get next action to be executed

  nextAction = [self next];
  
  if (!nextAction)
    *status = Completed;
  return nextAction;
}

//
// dropAllocations: -- drop index as component of activity
//
- (void)dropAllocations: (BOOL)componentAlloc
{
  [((ActionGroup_c *) collection)->activityRefs remove: activity];
  [super dropAllocations: YES];
}

@end


@implementation GroupPermutedIndex_c
PHASE(Creating)
- setActivity: theActivity
{
  activity = theActivity;
  return self;
}

PHASE(Using)
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
  id nextAction;

  if (((ActionGroup_c *)
       ((Permutation_c *) collection)->collection)->bits & BitAutoDrop
      && [self getLoc] == Member)
    {
      id removedAction = [self remove];
      [removedAction dropAllocations: YES];
    }

  nextAction = [self next];
  
  if (! nextAction)
    *status = Completed;
  return nextAction;
}

//
// dropAllocations: -- drop index as component of activity
//
- (void)dropAllocations: (BOOL)componentAlloc
{
  [((ActionGroup_c *)
    ((Permutation_c *) collection)->collection)->activityRefs 
                                               remove: activity];
  [super dropAllocations: YES];
}

@end


//
// ForEachActivity_c -- activity to process members of a ForEach action
//

@implementation ForEachActivity_c
PHASE(Creating)
+ _create_: forEachAction: anActivity
{
  Activity_c *owner, *newActivity;
  id ownerZone;
  ForEachIndex_c *newIndex;

  // create new activity containing custom index into target collection
  
  owner = anActivity;
  ownerZone = getZone (owner);
  newActivity = ALLOCIVARSCOMPONENT (ownerZone, id_ForEachActivity_c);
  newIndex = ALLOCIVARSCOMPONENT (ownerZone, id_ForEachIndex_c);

  setMappedAlloc (newActivity);
  setMappedAlloc (newIndex);

  registerSubactivity (ownerZone, owner, newActivity);
  
  newActivity->status = Initialized;
  newActivity->breakFunction = owner->breakFunction;
  newActivity->currentIndex = newIndex;
  newIndex->activity = anActivity;

  newIndex->memberIndex = [((PAction *) forEachAction)->target
                             begin: getCZone (ownerZone)];
  newIndex->memberAction = [ownerZone copyIVarsComponent: forEachAction];

  ((ActionForEach_c *) newIndex->memberAction)->target = nil;

  // set currentSubactivity in the activity that called _performAction_

  owner->currentSubactivity = (id) newActivity;

  return newIndex->memberAction;
}

+ _createRandom_:  forEachAction : anActivity
{
  Activity_c *owner, *newActivity;
  id ownerZone;
  ForEachIndex_c *newIndex;

  // create new activity containing custom index into target collection

  owner = anActivity;
  ownerZone = getZone (owner);
  newActivity = ALLOCIVARSCOMPONENT (ownerZone, id_ForEachActivity_c);
  newIndex = ALLOCIVARSCOMPONENT (ownerZone, id_ForEachIndex_c);

  setMappedAlloc (newActivity);
  setMappedAlloc (newIndex);

  newActivity->ownerActivity = anActivity;

  registerSubactivity (ownerZone, owner, newActivity);

  newActivity->status = Initialized;
  newActivity->breakFunction = owner->breakFunction;
  newActivity->currentIndex = newIndex;
  newIndex->activity = anActivity;

  newIndex->memberIndex = [((PAction *) forEachAction)->target
                             beginPermuted: getCZone (ownerZone)];
  newIndex->memberAction = [ownerZone copyIVarsComponent: forEachAction];
  ((PAction *) newIndex->memberAction)->target = nil;

  // set currentSubactivity in the activity that called _performAction_

  owner->currentSubactivity = (id) newActivity;
  return newIndex->memberAction;
}

PHASE(Using)
- getCurrentMember
{
  return [currentIndex get];
}
@end


@implementation ForEachIndex_c
PHASE(Using)
- nextAction: (id *)status
{
  // reload and return the action being used to execute each member

  ((ActionForEach_c *) memberAction)->target = [memberIndex next];
  if (((ActionForEach_c *) memberAction)->target)
    return memberAction;
  *status = Completed;
  return nil;
}

- get
{
  if (((ActionForEach_c *) memberAction)->target == nil)
    return nil;
  return memberAction;
}

- (id <Symbol>)getLoc
{
  return [memberIndex getLoc];
}

- getHoldType
{
  return nil;
}

- (void)mapAllocations: (mapalloc_t)mapalloc
{
  mapObject (mapalloc, memberIndex);
  mapObject (mapalloc, memberAction);
}

- setOffset: (unsigned)offset
{
  abort ();
}

- (unsigned)getOffset
{
  abort ();
}
    
- (void)setLoc: (id <Symbol>)locSymbol
{
  abort ();
}

- remove
{
  abort ();
}

- put: anObject
{
  abort ();
}

- findPrev: anObject
{
  abort ();
}

- findNext: anObject
{
  abort ();
}

- prev
{
  abort ();
}

- next
{
  abort ();
}

- getCollection
{
  abort ();
}

@end

