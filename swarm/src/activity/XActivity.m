// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         XActivity.m
Description:  state of processing within an action plan
Library:      activity
*/

#import <activity/XActivity.h>
#import <activity/Action.h>
#import <activity/ActionGroup.h>
#import <activity/Schedule.h>
#import <defobj/defalloc.h>


// define standard activity variables

id    _activity_current, _activity_zone;
BOOL  (*_activity_trace)( id );

//
// Activity_c -- activity to perform any compound action type
//

@implementation Activity_c

//
// auditRunRequest() -- common function for run and step messages
//
static void auditRunRequest( Activity_c *self, char *request )
{
  if ( ! self->topLevelAction )
    raiseEvent( SourceMessage,
"> can only %s a top-level activity (top-level activity will automatically\n"
"> %s the lowest level pending activity)\n", request, request );

  if ( self->status == Running )
    raiseEvent( SourceMessage, 
     "> cannot request to %s an activity while it is already running\n"
     "> (must stop first)\n", request );
  if ( self->status == Completed )
    raiseEvent( SourceMessage,
       "> cannot %s an activity that is already completed\n", request );
/*
Eventually, could request that a lower-level activity be advanced if not
holding on other activities, and all are stopped.  For now, no running or
stepping anyway except at top level, which can never be Holding.  When remove
restriction to run only at top level, must also add check for Holding.
*/
}

//
// run --
//  external method to advance top-level activity to a status of Stopped,
//  Terminated, or Completed
//
- run
{
  auditRunRequest( self, "run" );
  ownerActivity = _activity_current;
  // ownerActivity = [_activity_current getCurrentSubactivity];
  //!! (when have leaf activity implemented)
  _activity_current = self;
  [self _run_];
  _activity_current = [ownerActivity getOwnerActivity];
  ownerActivity = nil;
  return status;
}

//
// _run_ -- internal method to execute pending actions of the activity
//
// This is the core inner method of the action execution machinery.
// It attempts to run the activity to completion, and returns with a status
// of Completed when there are no remaining actions to run.  If an action
// creates a subactivity, the _run_ method calls itself recursively to
// run the subactivity to completion before continuing with any other
// actions.  An activity ordinarily starts with a status of Initialized
// and enters a status of Running while it is contained on the stack.
// A subactivity can also be added to the stack with a status of Released
// in which case it runs until it reaches a status of either Holding or
// Completed.
//
- _run_
{
  id  initStatus, subStatus, nextAction;

  // status is Running whenever actions being processed by run loop

  initStatus = status;
  status = Running;

  // if continuing from previous break then enter code below at proper point

  if ( initStatus != Initialized ) {
    if ( initStatus == Released ) {
      if ( (nextAction = [currentIndex get]) ) goto performBreak;
      status = Completed;
      goto actionsDone;
    } else if ( initStatus == Stopped && ! currentSubactivity ) {
      if ( (nextAction = [currentIndex get]) ) goto performBreak;
      status = Completed;
      goto actionsDone;
    } else if ( initStatus == Terminated ) {
      if ( ! currentSubactivity ) goto activityTerminated;
      goto actionsDone;
    }
  }

  // perform each successive action while also completing any subactivities

  while ( 1 ) {

    // else complete any current subactivity before continuing

    if ( currentSubactivity ) {
      _activity_current = currentSubactivity;
      subStatus = [(id)currentSubactivity _run_];  // try to run to completion
      _activity_current = self;

      if ( subStatus != Holding ) {         // if sub is holding just continue
        if ( subStatus == Completed ) {
          [currentSubactivity dropAllocations: 1];
                                    // drop completed subactivity and continue
          if ( status == Terminated ) goto activityTerminated;
        } else { // subStatus == Stopped ) {
          status = Stopped;
          return Stopped;                         // stop entire running stack
        }
      }
      currentSubactivity = nil;       // clear previous sub before next action
    }

    // obtain next action, call break function, and test if stop requested

    nextAction = [currentIndex nextAction: &status];
    if ( ! nextAction ) goto actionsDone;

  performBreak:
    if ( breakFunction && breakFunction( self ) ) {
      if ( status == Terminated ) goto activityTerminated;
      return status;  // status == Stopped
    }

    // perform action, which could create a new subactivity to be performed

    [nextAction _performAction_: self];

    //
    // The performed action, if it contains a ForEach or "perform" request,
    // may set currentSubactivity to a new subactivity.  The next iteration
    // of the loop will run the new subactivity to completion before
    // continuing this activity. 
    //
  }

actionsDone:  // status is Holding, Completed, or Terminated

  // give break function its chance to stop activity before final return

  if ( ! ( breakFunction && breakFunction( self ) && status == Terminated ) )
    return status;

activityTerminated:  // activity terminated since last _run_

  // set index to end to release any holds before final completion

  [currentIndex setLoc: End];    // release any holds before final completion
  currentSubactivity = nil;    // cancel any just-dropped subactivity, if any
  status = Completed;
  return status;
}

//
// terminateFunction -- break function to terminate a running leaf activity
//
static BOOL terminateFunction( id activity )
{
  ((Activity_c *)activity)->status = Terminated;
  if ( _activity_trace ) _activity_trace( activity );
  return 1;
}

//
// terminate -- terminate activity and all its subactivities
//
- (void) terminate
{
  if ( currentSubactivity ) {
    [currentSubactivity terminate];
  } else if ( status == Running ) {
    breakFunction = terminateFunction;
  }
  status = Terminated;
}

//
// stopFunction -- break function to stop a running leaf activity
//
static BOOL stopFunction( id activity )
{
  // cancel stop function in local activity and any owner activity that
  // just created local activity

  ((Activity_c *)activity)->breakFunction = _activity_trace;
  if ( ((Activity_c *)activity)->ownerActivity &&
       ((Activity_c *)activity)->ownerActivity->breakFunction == stopFunction )
    ((Activity_c *)activity)->ownerActivity->breakFunction = _activity_trace;

  // return up stack of activities with status set to Stopped

  if ( ((Activity_c *)activity)->status != Holding ) {
    ((Activity_c *)activity)->status = Stopped;
    return 1;

  // if Holding then defer stop to owner activity

  } else {
    ((Activity_c *)activity)->ownerActivity->breakFunction = stopFunction;
    return 0;
  }
}

//
// stop -- stop activity and all its subactivities
//
- stop
{
  if ( status == Terminated ) return self;
  if ( currentSubactivity ) {
    [currentSubactivity stop];
  } else {
    breakFunction = stopFunction;
  }
  return self;
}

//
// nextFunction -- break function to stop on return from 
//
static BOOL nextFunction( id activity )
{
  // cancel local next function

  ((Activity_c *)activity)->breakFunction = _activity_trace;

  // return if just created as new subactivity (leaving next function in owner)

  if ( ((Activity_c *)activity)->ownerActivity &&
       ((Activity_c *)activity)->ownerActivity->breakFunction == nextFunction )
    return 0;

  // if Holding then defer stop to owner activity

  if ( ((Activity_c *)activity)->status == Holding ) {
    ((Activity_c *)activity)->ownerActivity->breakFunction = stopFunction;
    return 0;
  }

  // return up stack of activities with status set to Stopped

  ((Activity_c *)activity)->status = Stopped;
  return 1;
}

//
// installNext() -- break function to stop activity after next action
//
static BOOL installNext( id activity )
{
  if ( ((Activity_c *)activity)->status != Completed )
    ((Activity_c *)activity)->breakFunction = nextFunction;
  else if ( ((Activity_c *)activity)->ownerActivity )
    ((Activity_c *)activity)->ownerActivity->breakFunction = nextFunction;

  if ( _activity_trace ) _activity_trace( activity );
  return 0;
}

//
// next -- advance activity to the next action within activity
//
- next
{
  Activity_c  *subactivity;

  auditRunRequest( self, "next" );

  for ( subactivity = self;
        subactivity->currentSubactivity;
        subactivity = subactivity->currentSubactivity );

  if ( subactivity->breakFunction == terminateFunction ) {
    subactivity = subactivity->ownerActivity;
    if ( subactivity ) subactivity->breakFunction = stopFunction;
    return [self run];
  }
  subactivity->breakFunction = installNext;
  return [self run];
}

//
// installStep() -- break function to stop activity after next subaction
//
static BOOL installStep( id activity )
{
  // stop in local activity, or new subactivity, if not completed

  if ( ((Activity_c *)activity)->status != Completed )
    ((Activity_c *)activity)->breakFunction = stopFunction;

  // else stop at next event of owner activity

  else if ( ((Activity_c *)activity)->ownerActivity )
    ((Activity_c *)activity)->ownerActivity->breakFunction = stopFunction;

  if ( _activity_trace ) _activity_trace( activity );
  return 0;
}

//
// step -- advance activity by a single action
//
- step
{
  Activity_c  *subactivity;

  auditRunRequest( self, "step" );

  for ( subactivity = self;
        subactivity->currentSubactivity;
        subactivity = subactivity->currentSubactivity );

  if ( subactivity->breakFunction == terminateFunction ) {
    subactivity = subactivity->ownerActivity;
    if ( subactivity ) subactivity->breakFunction = stopFunction;
    return [self run];
  }
  subactivity->breakFunction = installStep;
  return [self run];
}

//
// getStatus -- return symbol for current processing status
//              (overridden in ForEachActivity_c)
//
- getStatus
{
  return status;
}

//
// getHoldType -- return HoldStart or HoldEnd status as provided by index
//
- getHoldType
{
  return [currentIndex getHoldType];
}

//
// getActionType -- get action type of action being performed by activity
//
- getActionType
{
  return ((Index_any *)currentIndex)->collection;
}

//
// getAction -- get action containing parameter bindings for the local activity
//
- getAction
{
  if ( ! topLevelAction ) return [ownerActivity->currentIndex get];
  return topLevelAction;
}

//
// _getSubactivityAction_ --
//   internal method to access to local action without creating subactivity
//
- _getSubactivityAction_
{
  return [currentIndex get];
}

//
// setOwnerActivity: -- change owner from one swarm activity to another
//
- (void) setOwnerActivity: aSwarmActivity
{
  if ( getClass( aSwarmActivity ) != id_SwarmActivity_c )
    raiseEvent( SourceMessage,
     "> new owner activity is not a swarm activity\n" );
  if ( getClass( ownerActivity ) != id_SwarmActivity_c )
    raiseEvent( SourceMessage,
"> cannot reassign owner activity unless running under a swarm activity\n" );
  if ( status == Running )
    raiseEvent( SourceMessage,
      "> cannot change owner swarm while activity is running\n" );
  //
  // Still need to figure out error-checking logic if running underneath
  // concurrent group in merge schedule.
  //

  raiseEvent( NotImplemented, nil );
  //
  // Need to remove merge action from one swarm's merge schedule and then
  // insert into merge schedule for the other, or if not merging, then
  // remove from local subactivities set and add to the other.  Also handle
  // any survivorship constraints.
  //
}

//
// getOwnerActivity -- return activity under which this activity is running
//
- getOwnerActivity
{
  return ( topLevelAction ? nil : ownerActivity );
}

//
// getControllingActivity --
//   return activity that issued current run request on top-level activity
//
- getControllingActivity
{
  return ( topLevelAction ? ownerActivity : nil );

}

//
// getTopLevelActivity --
//   return top of activity tree running the local activity
//
- getTopLevelActivity
{
  Activity_c  *activity;

  for ( activity = self; ! activity->topLevelAction;
        activity = activity->ownerActivity );
  return activity;
}

//
// getSwarmActivity -- return most immediately containing Swarm activity
//
- getSwarmActivity
{
  Activity_c  *activity;

  for ( activity = self;
        getClass( activity ) != id_SwarmActivity_c;
        activity = activity->ownerActivity )
    if ( activity->topLevelAction ) return nil;

  return activity;
}

//
// getScheduleActivity -- return most immediately containing Schedule activity
//
- getScheduleActivity
{
  Activity_c  *activity;

  for ( activity = self;
        getClass( activity ) != id_ScheduleActivity_c &&
        getClass( activity ) != id_SwarmActivity_c;
        activity = activity->ownerActivity )
    if ( activity->topLevelAction ) return nil;

  return activity;
}

//
// getSubactivities -- return set of subactivities pending to be run
//
// This method is overridden by SwarmActivity_c, which returns the merge
// schedule as an initial form of subactivities collection.  For other types
// of activities, this method should return a collection of just one currently
// running subactivity, or else an empty collection.  A collection of one
// subactivity should be created only as needed.
//
- getSubactivities
{
  raiseEvent( NotImplemented, nil );
  return nil;
}

//
// setSerialMode -- set serial execution mode  
//
- (void) setSerialMode: (BOOL)serialMode
{
  if ( ! serialMode ) raiseEvent( NotImplemented,
"> A concurrent processing mode that would allow multiple simultaneous\n"
"> subactivities to be present has not been implemented for any type of\n"
"> activity except a Swarm.\n"
    );
}

//
// getSerialMode -- return indicator for serial execution mode 
//
- (BOOL) getSerialMode
{
  return 1;
}

//
// getCurrentSubactivity -- get running subactivity or next subactivity to run
//
- getCurrentSubactivity
{
  if ( ! currentSubactivity ) {
    if ( ! currentIndex ) return nil;
    raiseEvent( NotImplemented,
"> creation of a subactivity to obtain reference to leaf-level action has\n"
"> not yet been implemented\n" );
  }
  return currentSubactivity;
}

//
// mapAllocations: -- standard method to identify internal allocations
//
- (void) mapAllocations: (mapalloc_t)mapalloc
{
  if ( topLevelAction ) mapObject( mapalloc, topLevelAction );
  if ( currentSubactivity ) mapObject( mapalloc, currentSubactivity );
  mapObject( mapalloc, currentIndex );
}

//
// drop -- method for external caller to drop a completed activity
//
- (void) drop
{
  Activity_c  *activity;

  if ( ownerActivity ) {
    if ( status == Terminated ) {
      for ( activity = self; activity->currentSubactivity;
            activity = activity->currentSubactivity );
      if ( activity->breakFunction != terminateFunction ) {
        [self dropAllocations: 1];
        return;
      }
    }
    raiseEvent( SourceMessage,
"> can only drop a top-level activity or a terminated activity that is not\n"
"> currently running\n" );
  }

  if ( status == Running )
    raiseEvent( SourceMessage,
"> cannot drop an activity while it is running\n" );

  [self dropAllocations: 0];
}

- (void) describe: outputCharStream
{
  char  buffer[100];

  [super describe: outputCharStream];
  [outputCharStream catC: "> current activity status: "];
  [outputCharStream catC: [status getName]];
  [outputCharStream catC: "\n> compound action being processed: "];

  _obj_formatIDString( buffer, ((Index_any *)currentIndex)->collection );
  [outputCharStream catC: buffer];

  if ( currentSubactivity ) {
    [outputCharStream
      catC: "\n> describe of current subactivity follows:\n"];
    [currentSubactivity describe: outputCharStream];
  } else {
    [outputCharStream catC: "\n> activity has no current subactivity\n"];
  }
}

@end

//
// _activity_context_error() --
//   function to generate error message if invalid context query
//
extern id _activity_context_error( char *macroName )
{
  if ( ! _activity_current )
    raiseEvent( InvalidOperation,
"> %s(): there is no currently running activity from which\n"
"> to obtain requested activity context information.  The context query\n"
"> macros are available only within a compiled action being executed under\n"
"> a running activity.  They are not available when the activity is stopped\n"
"> or otherwise inactive.  This includes any external probe request.\n",
      macroName );

  raiseEvent( InvalidOperation,
"> %s(): an Swarm or Schedule does not exist in the current activity\n"
"> context from which to obtain the requested value.\n", macroName );

  return nil;
}
