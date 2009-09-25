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
Name:         CompoundAction.m
Description:  a collection of actions to be performed in a defined order
Library:      activity
*/

#if !defined(MIXIN_CREATE) && !defined(MIXIN_SET) && !defined(MIXIN_C) && !defined(MIXIN_INDEX)
//
// this section compiled when not included for mixin inheritance
//

#import <activity/CompoundAction.h>
#import <activity/XActivity.h>

#import <defobj/macros.h>
#import <collections/macros.h>

void 
setDefaultOrder (unsigned *bits, id <Symbol> aSymbol)
{  
  if (aSymbol == Concurrent) 
    setBit (*bits, BitConcurrent, 1); 
  else if (aSymbol == Sequential)
    {
      setBit (*bits, BitConcurrent, 0);
      setBit (*bits, BitRandomized, 0);
    }
  else if (aSymbol == Randomized)
    setBit (*bits, BitRandomized, 1);
  else
    raiseEvent (InvalidArgument, nil);
}

id <Symbol>
getDefaultOrder (unsigned bits)
{
  if (bits & BitConcurrent)
    return Concurrent;
  if (bits & BitRandomized)
    return Randomized;
  return Sequential;
}

@implementation ActionType_c
@end

@implementation CompoundAction_c
@end



#elif defined (MIXIN_CREATE)
#undef MIXIN_CREATE
//
// mixin inheritance for create phase (provided by source inclusion)
//

- setAutoDrop: (BOOL)autoDrop
{
  setBit (bits, BitAutoDrop, autoDrop);
  return self;
}

#elif defined(MIXIN_SET)
#undef MIXIN_SET

- setDefaultOrder: (id <Symbol>)aSymbol
{
  setDefaultOrder (&bits, aSymbol);
  return self;
}

#elif defined(MIXIN_C)
#undef MIXIN_C

//
// mixin inheritance for finalized instance(provided by source inclusion)
//

- (BOOL)getAutoDrop
{
  return (bits & BitAutoDrop) == BitAutoDrop;
}

- (id <Symbol>)getDefaultOrder
{
  return getDefaultOrder (bits);
}

//
// activate -- activate to run as a top-level, externally controllable process
//
- (id <Activity>)activate
{
  return [self activateIn: nil];
}

//
// activateIn: -- create an activity to run plan under a swarm
//
- (id <Activity>)activateIn: swarmContext
{
  return
    [self _activateIn_: swarmContext
          : ACTIVITY_CLASS_ID
          : INDEX_CLASS_ID
          : swarmContext ? [swarmContext getZone] : _activity_zone];
}

//
// _activateIn_::: -- create an activity to run plan under swarm or top level
//
- (id)_activateIn_: swarmContext
                             : (Class)activityClass 
                             : (Class)indexClass
                             : (Zone_c *)swarmZone
{
  // if top-level activation requested then just create new activity

  if (!swarmContext)
    return [self _createActivity_ : nil
                 : activityClass 
                 : indexClass
                 : swarmZone];

  // otherwise create new activity to run under requested swarm context

  if (respondsTo (swarmContext, M(getSwarmActivity)))
    {
      swarmContext = [swarmContext getSwarmActivity];
      if (!swarmContext)
        raiseEvent (InvalidArgument,
                    "> requested swarm context has not yet been activated\n" );
      
    } 
  else if (!respondsTo (swarmContext, M(getSwarm)))
    raiseEvent (InvalidArgument,
                "> argument is neither nil nor a valid swarm context\n" );
  
  return
    [self _activateUnderSwarm_: activityClass
          : indexClass 
          : swarmContext
          : swarmZone];
}

//
// _performPlan_ -- create an activity to run plan under the current activity
//
- (void)_performPlan_
{
  Activity_c *newActivity;
  id aZone = (_activity_current
              ? getZone ((Activity_c *) _activity_current)
              : _activity_zone);

  newActivity = [self
    _createActivity_: _activity_current
                  : ACTIVITY_CLASS_ID
                  : INDEX_CLASS_ID
                  : aZone];
  newActivity->ownerActivity->currentSubactivity = newActivity;
}

static void
registerSubactivity (Zone_c *zone, Activity_c *owner, Activity_c *newActivity)
{
  if (!owner->activitySet)
    owner->activitySet = 
      [_activity_activitySetRefsType create: getCZone (zone)];
  MLIST_ADDLAST (owner->activitySet, newActivity);
  newActivity->ownerActivity  = owner;
  newActivity->registeredOwnerActivity = owner;
}

//
// _createActivity_:: -- create activity to perform a plan
//
- _createActivity_: (Activity_c *)ownerActivity : (Class)activityClass : (Class)indexClass : (Zone_c *)swarmZone;
{
  Activity_c *newActivity;

  // allocate and initialize a new activity

  newActivity = ALLOCIVARSCOMPONENT (swarmZone, activityClass);
  if (ownerActivity)
    registerSubactivity (swarmZone, ownerActivity, newActivity);
  else
    {
      newActivity->topLevelAction = ALLOCIVARSCOMPONENT (swarmZone, id_CAction);
      ((CAction *) newActivity->topLevelAction)->owner = (ActionType_c *) self;
    }
  setMappedAlloc (newActivity);
  
  // add new activity to list of activities running plan

  if (!activityRefs)
    activityRefs = [_activity_activityRefsType create: getCZone (swarmZone)];
  MLIST_ADDLAST (activityRefs, newActivity);
  
  // initialize status and set break function from owner

  newActivity->status = Initialized;
  if (_activity_current)
    newActivity->breakFunction =
     ((Activity_c *) _activity_current)->breakFunction;
  else
    newActivity->breakFunction = _activity_trace;

  // create index on the plan actions for traversal by the activity
  if ([self getDefaultOrder] == Randomized
      && [self conformsTo: @protocol (ActionGroup)])
    newActivity->currentIndex =
      [(ActionGroup_c *) self _createPermutedIndex_: getCZone (swarmZone)
                         activity: newActivity];
  else
    newActivity->currentIndex =
      [[self _createIndex_: getCZone (swarmZone)
             forIndexSubclass: indexClass]
        setActivity: newActivity];

  return newActivity;
}

//
// drop -- release resources used by action plan if no current references
//
- (void)drop
{
  if (activityRefs && [activityRefs getCount] > 0)
    raiseEvent (SourceMessage,
                "> cannot drop action plan still referenced by an uncompleted activity\n");
  [super drop];
}

#elif  defined (MIXIN_INDEX)
#undef MIXIN_INDEX
//
// mixin inheritance for index on action plan (provided by source inclusion)
//

- getHoldType
{
  return nil;
}

#endif
