// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         ActionPlan.m
Description:  a collection of actions to be performed in a defined order
Library:      activity
*/

#if ! defined( MIXIN_CREATE ) && ! defined( MIXIN_C ) && ! defined( MIXIN_INDEX)
//
// this section compiled when not included for mixin inheritance
//

#import <activity/ActionPlan.h>
#import <activity/Activity.h>

// define standard compare function for use with map keys

int _activity_compareIDs( id id1, id id2 )
{
  if ( (unsigned)id1 < (unsigned)id2 ) return -1;
  return ( (unsigned)id1 > (unsigned)id2 );
}


@implementation ActionPlan_c
@end


@implementation VariableDefinition_c

- getActionPlan
{
  return actionPlan;
}

@end

@implementation ArgumentDefinition_c
@end

@implementation ResultDefinition_c
@end


#elif  defined( MIXIN_CREATE )
#undef MIXIN_CREATE
//
// mixin inheritance for create phase (provided by source inclusion)
//

- (void) setDefaultOrder: aSymbol
{
  if ( aSymbol == Concurrent ) {
    setBit( bits, Bit_Concurrent, 1 );
  } else if ( aSymbol == Sequential ) {
    setBit( bits, Bit_Concurrent, 0 );
    setBit( bits, Bit_Randomized, 0 );
  } else if ( aSymbol == Randomized ) {
    setBit( bits, Bit_Randomized, 1 );
  } else {
    raiseEvent( InvalidArgument, nil );
  }
}

- (void) setAutoDrop: (BOOL)autoDrop
{
  setBit( bits, Bit_AutoDrop, autoDrop );
}

- defineVariable
{
  VariableDefinition_c  *newDef;

  if ( ! variableDefs ) variableDefs = [List create: zone];
  newDef = [zone allocIVars: id_VariableDefinition_c];
  newDef->actionPlan = (ActionPlan_c *)self;
  [variableDefs addFirst: newDef];
  return newDef;
}

- defineArgument
{
  ArgumentDefinition_c  *newDef;

  if ( ! variableDefs ) variableDefs = [List create: zone];
  newDef = [zone allocIVars: id_ArgumentDefinition_c];
  newDef->actionPlan = (ActionPlan_c *)self;
  [variableDefs addLast: newDef];
  return newDef;
}

- defineResult
{
  ResultDefinition_c  *newDef;

  if ( bits & Bit_ResultVarDef )
    raiseEvent( SourceMessage,
       "can only define a single result variable for an action plan\n" );
  setBit( bits, Bit_ResultVarDef, 1 );

  if ( ! variableDefs ) variableDefs = [List create: zone];
  newDef = [zone allocIVars: id_ResultDefinition_c];
  newDef->actionPlan = (ActionPlan_c *)self;
  [variableDefs addLast: newDef];
  return newDef;
}

- (void) _createVarDefsArray_
{
  int                   varOffset, argOffset;
  id                    varDefsArray, index;
  VariableDefinition_c  *varDef;

  argOffset = 0;
  if ( bits & Bit_ResultVarDef ) argOffset++;
  varOffset = [variableDefs getCount];

  varDefsArray = [Array createBegin: zone];
  [varDefsArray setCount: varOffset];
  varDefsArray = [varDefsArray createEnd];

  index = [variableDefs begin: scratchZone];
  while ( (varDef = [index next]) ) {
    if ( getClass( varDef ) == id_ResultDefinition_c ) {
      varDef->valueOffset = 0;
      [varDefsArray atOffset: 0 put: varDef];
    } else if ( getClass( varDef ) == id_ArgumentDefinition_c ) {
      varDef->valueOffset = argOffset;
      [varDefsArray atOffset: argOffset put: varDef];
      argOffset++;
    } else {
      varOffset--;
      varDef->valueOffset = varOffset;
      [varDefsArray atOffset: varOffset put: varDef];
    }
  }
  [index drop];
  [variableDefs drop];
  variableDefs = varDefsArray;
}

#elif   defined( MIXIN_C )
#undef  MIXIN_C

//
// mixin inheritance for finalized instance(provided by source inclusion)
//

- getDefaultOrder
{
  if ( bits & Bit_Concurrent ) return Concurrent;
  if ( bits & Bit_Randomized ) return Randomized;
  return Sequential;
}

- (BOOL) getAutoDrop
{
  return bits & Bit_AutoDrop;
}

- getActivities
{
  if ( ! activityRefs )
    activityRefs = [_activity_activityRefsType create: zone];
  return activityRefs;
}

- getVariableDefs
{
  // more to come...
  //if ( ! variableDefs ) variableDefs = [ create: zone];
  return variableDefs;
}

//
// _createActivity_:: -- create activity to perform a plan
//
- _createActivity_: activityClass : indexClass
{
  id               activityZone;
  Activity_c       *newActivity;
  INDEX_CLASS      *newIndex;

  // determine zone in which activities to be created

  activityZone = _activity_current ?
                    ((Activity_c *)_activity_current)->zone : _activity_zone;

  // allocate and initialize a new activity

  newActivity = [activityZone allocIVars: activityClass];
  newActivity->zone = activityZone;
  newActivity->ownerActivity = _activity_current;

  // raise error if owner activity has already been terminated

  if ( _activity_current && newActivity->ownerActivity->status == Completed )
    raiseEvent( SourceMessage,
     "cannot start or perform a new activity from an action running under a\n"
     "terminated current activity\n" );

  // create variable definitions array if any variables defined

  if ( variableDefs )
    newActivity->variableValues =
      [Array create: zone setCount: [variableDefs getCount]];

  // add new activity to list of activities running plan

  if ( ! activityRefs )
    activityRefs = [_activity_activityRefsType create: zone];
  [activityRefs add: newActivity];

  // initialize status and set break function from owner

  newActivity->status = Initialized;
  if ( _activity_current )
    newActivity->breakFunction =
     ((Activity_c *)_activity_current)->breakFunction;
  else
    newActivity->breakFunction = _activity_trace;

  // create index on the plan actions for traversal by the activity

  newIndex = [self _createIndex_: activityZone forIndexSubclass: indexClass];
  newIndex->activity = (id)newActivity;
  newActivity->currentIndex = newIndex;

  return newActivity;
}

//
// _performPlan_ -- create an activity to run plan under the current activity
//
- (void) _performPlan_
{
  Activity_c  *newActivity;

  newActivity = [self _createActivity_: ACTIVITY_CLASS_ID : INDEX_CLASS_ID];
  newActivity->ownerActivity->currentSubactivity = newActivity;
}


//
// activate -- create an activity to run plan at top control level
//

- activate
{
  return [self _createActivity_ : ACTIVITY_CLASS_ID : INDEX_CLASS_ID];
}

//
// _activateIn_::: -- create an activity to run plan under swarm or top level
//
- _activateIn_: swarmContext : activityClass : indexClass
{
  // if top-level run control requested then just create new activity

  if ( ! swarmContext )
    return [self _createActivity_ : activityClass : indexClass];

  // otherwise create new activity to run under requested swarm context

  if ( respondsTo( swarmContext, M(getSwarmActivity) ) ) {
    swarmContext = [swarmContext getSwarmActivity];
    if ( ! swarmContext )
      raiseEvent( InvalidArgument,
        "requested swarm context has not yet been activated\n" );

  } else if ( ! respondsTo( swarmContext, M(getSwarm) ) ) {
    raiseEvent( InvalidArgument,
      "argument is neither nil nor a valid swarm context\n" );
  }

  return
    [self _activateUnderSwarm_: activityClass : indexClass : swarmContext];
}

//
// activateIn: -- create an activity to run plan under a swarm
//

- activateIn: swarmContext
{
  return [self _activateIn_: swarmContext : ACTIVITY_CLASS_ID:INDEX_CLASS_ID];
}
- activateIn: swarmContext : arg1
{
  Activity_c  *newActivity;

  newActivity = [self activateIn: swarmContext];
  [newActivity setArg: 1 to: arg1];
  return newActivity;
}
- activateIn: swarmContext : arg1 : arg2
{
  Activity_c  *newActivity;

  newActivity = [self activateIn: swarmContext : arg1];
  [newActivity setArg: 2 to: arg2];
  return newActivity;
}
- activateIn: swarmContext: arg1 : arg2 : arg3;
{
  Activity_c  *newActivity;

  newActivity = [self activateIn: swarmContext : arg1 : arg2];
  [newActivity setArg: 3 to: arg3];
  return newActivity;
}

//
// drop -- release resources used by action plan if no current references
//
- (void) drop
{
  if ( activityRefs && [activityRefs getCount] > 0 )
    raiseEvent( SourceMessage,
      "cannot drop action plan still referenced by an uncompleted activity\n" );

  if ( variableDefs ) [variableDefs drop];
  if ( activityRefs ) [activityRefs drop];
  [super drop];
}

#elif  defined( MIXIN_INDEX )
#undef MIXIN_INDEX
//
// mixin inheritance for index on action plan (provided by source inclusion)
//

- getHoldType
{
  return nil;
}

#endif
