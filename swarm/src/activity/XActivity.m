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
Name:         XActivity.m
Description:  state of processing within an action plan
Library:      activity
*/

#import <activity/XActivity.h>
#import <activity/Action.h>
#import <activity/ActionGroup.h>
#import <activity/Schedule.h>
#import <activity.h>
#import <defobj/defalloc.h>

#import <defobj/macros.h>
#import <activity/macros.h>

// define standard activity variables

externvardef id <Activity> _activity_current;
externvardef id <Zone> _activity_zone;
externvardef BOOL  (*_activity_trace) (id);

//
// Activity_c -- activity to perform any compound action type
//

@implementation Activity_c
PHASE(Creating)
PHASE(Using)
//
// auditRunRequest() -- common function for run and step messages
//
static void
auditRunRequest (Activity_c *self, const char *request)
{
  if (!self->topLevelAction)
    raiseEvent (SourceMessage,
                "> can only %s a top-level activity "
                "(top-level activity will automatically\n"
                "> %s the lowest level pending activity)\n",
                request, request);
  
  if (RUNNINGP (self->status))
    raiseEvent (SourceMessage, 
                "> cannot request to %s an activity while it "
                "is already running\n"
                "> (must stop first)\n",
                request);
  if (COMPLETEDP (self->status))
    raiseEvent (SourceMessage,
                "> cannot %s an activity that is already completed\n", 
                request);
  /*
    Eventually, could request that a lower-level activity be advanced if not
    holding on other activities, and all are stopped.  For now, no running or
    stepping anyway except at top level, which can never be Holding.
    When remove restriction to run only at top level, must also add check
    for Holding.
  */
}

//
// run --
//  external method to advance top-level activity to a status of Stopped,
//  Terminated, or Completed
//
- (id <Symbol>)run
{
  auditRunRequest (self, "run");

  ownerActivity = _activity_current;

  _activity_current = self;
  [self _run_];
  _activity_current = ownerActivity;

  //!! When have activity for leaf action implemented, code will change to:
  // ownerActivity = [_activity_current getCurrentSubactivity];
  // _activity_current = self;
  // [self _run_];
  // _activity_current = [ownerActivity getOwnerActivity];
  
  ownerActivity = nil;
  return status;
}

static BOOL
removeObsoleteMerges (id <ActivityIndex> currentIndex)
{
  CAction *action = GENERIC_GET (currentIndex);

  BOOL obsoletep ()
    {
      Activity_c *subactivity = ((ActionMerge_c *) action)->subactivity;

      return COMPLETEDP (subactivity->status) && !subactivity->keepEmptyFlag;
    }

  if (getClass (action) == id_ActionConcurrent_c)
    {
      id index =
        [((ActionConcurrent_c *) action)->concurrentGroup begin: scratchZone];
      CAction *caction;
      
      while ((caction = [index next]))
        {
          if (getClass (caction) == id_ActionMerge_c)
            {
              if (obsoletep ())
                [index remove];
            }
        }
      DROP (index);
      return NO;
    }
  else if (getClass (action) == id_ActionMerge_c)
    {
      if (obsoletep ())
        {
          [currentIndex remove];

          return YES;
        }
    }
  return NO;
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
- (id <Symbol>)_run_
{
  id <Symbol> initStatus, subStatus;
  id <Action> nextAction;

  // status is Running whenever actions being processed by run loop

  initStatus = status;
  status = Running;

  // if continuing from previous break then enter code below at proper point

  if (!INITIALIZEDP (initStatus))
    {
      if (RELEASEDP (initStatus))
        {
          if ((nextAction = GENERIC_GET (currentIndex)))
            goto performBreak;
          status = Completed;
          goto actionsDone;
        }
      else if (STOPPEDP (initStatus) && !currentSubactivity)
        {
          if ((nextAction = GENERIC_GET (currentIndex)))
            goto performBreak;
          status = Completed;
          goto actionsDone;
        }
      else if (TERMINATEDP (initStatus))
        {
          if (!currentSubactivity)
            goto activityTerminated;
          goto actionsDone;
        }
    }
  
  // perform each successive action while also completing any subactivities
  
  while (1)
    {
      // else complete any current subactivity before continuing
      
      if (currentSubactivity)
        {
          _activity_current = currentSubactivity;
          subStatus = [(id) currentSubactivity _run_];  // try to run to completion
          _activity_current = self;
          
          // if sub is holding just continue
          if (!HOLDINGP (subStatus))
            {
              if (COMPLETEDP (subStatus))
                {
                  if (!currentSubactivity->keepEmptyFlag)
                    [currentSubactivity dropAllocations: YES];

                  // drop completed subactivity and continue
                  if (TERMINATEDP (status))
                    goto activityTerminated;
                }
              else
                {
                  // subStatus == Stopped 
                  status = Stopped;
                  // stop entire running stack
                  return Stopped;
                }
            }
          currentSubactivity = nil;  // clear previous sub before next action
        }
      
      // obtain next action, call break function, and test if stop requested

      do {
        nextAction = GENERIC_NEXTACTION (currentIndex, &status);
        
        if (nextAction)
          {
            if (!removeObsoleteMerges (currentIndex))
              break;
          }
        else
          goto actionsDone;
      } while (1);
      
    performBreak:
      if (breakFunction && breakFunction (self))
        {
          if (TERMINATEDP (status))
            goto activityTerminated;
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
  if (!(breakFunction && breakFunction (self) && TERMINATEDP (status)))
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
static BOOL
terminateFunction (id activity)
{
  ((Activity_c *) activity)->status = Terminated;
  if (_activity_trace)
    _activity_trace (activity);
  return YES;
}

//
// terminate -- terminate activity and all its subactivities
//
- (void)terminate
{
  if (currentSubactivity)
    [currentSubactivity terminate];
  else if (RUNNINGP (status))
    breakFunction = terminateFunction;
  status = Terminated;
}

//
// stopFunction -- break function to stop a running leaf activity
//
static BOOL
stopFunction (id anObj)
{
  Activity_c *activity = anObj;
  // cancel stop function in local activity and any owner activity that
  // just created local activity
  
  activity->breakFunction = _activity_trace;
  if (activity->ownerActivity
      && (activity->ownerActivity->breakFunction == stopFunction))
    activity->ownerActivity->breakFunction = _activity_trace;
  
  // return up stack of activities with status set to Stopped
  
  if (!HOLDINGP (activity->status))
    {
      activity->status = Stopped;
      return YES;
    }
  else
    {
      // if Holding then defer stop to owner activity
      activity->ownerActivity->breakFunction = stopFunction;
      return NO;
    }
}

//
// stop -- stop activity and all its subactivities
//
- (void)stop
{
  if (TERMINATEDP (status))
    return;

  if (currentSubactivity)
    [currentSubactivity stop];
  else
    breakFunction = stopFunction;
}

//
// nextFunction -- break function to stop on return from 
//
static BOOL
nextFunction (id anObj)
{
  Activity_c *activity = anObj;
  // cancel local next function
  
  activity->breakFunction = _activity_trace;

  // return if just created as new subactivity (leaving next function in owner)

  if (activity->ownerActivity
      && activity->ownerActivity->breakFunction == nextFunction)
    return NO;
  
  // if Holding then defer stop to owner activity
  
  if (HOLDINGP (activity->status))
    {
      activity->ownerActivity->breakFunction = stopFunction;
      return NO;
    }
  // return up stack of activities with status set to Stopped
  
  activity->status = Stopped;
  return YES;
}

//
// installNext() -- break function to stop activity after next action
//
static BOOL
installNext (id anObj)
{
  Activity_c *activity = anObj;

  if (!COMPLETEDP (activity->status))
    activity->breakFunction = nextFunction;
  else if (activity->ownerActivity)
    activity->ownerActivity->breakFunction = nextFunction;
  
  if (_activity_trace)
    _activity_trace (activity);
  return NO;
}

//
// next -- advance activity to the next action within activity
//
- (id <Symbol>)nextAction
{
  Activity_c *subactivity;

  auditRunRequest (self, "next");
  
  for (subactivity = self;
       subactivity->currentSubactivity;
       subactivity = subactivity->currentSubactivity);
  
  if (subactivity->breakFunction == terminateFunction)
    {
      subactivity = subactivity->ownerActivity;
      if (subactivity)
        subactivity->breakFunction = stopFunction;
      return [self run];
    }
  subactivity->breakFunction = installNext;
  return [self run];
}

//
// installStep() -- break function to stop activity after next subaction
//
static BOOL
installStep (id anObj)
{
  Activity_c *activity = anObj;

  // stop in local activity, or new subactivity, if not completed
  
  if (!COMPLETEDP (activity->status))
    activity->breakFunction = stopFunction;
  
  // else stop at next event of owner activity

  else if (activity->ownerActivity)
    activity->ownerActivity->breakFunction = stopFunction;
  
  if (_activity_trace)
    _activity_trace (activity);
  return NO;
}

//
// step -- advance activity by a single action
//
- (id <Symbol>)stepAction
{
  Activity_c *subactivity;

  auditRunRequest (self, "step");

  for (subactivity = self;
       subactivity->currentSubactivity;
       subactivity = subactivity->currentSubactivity);
  
  if (subactivity->breakFunction == terminateFunction)
    {
      subactivity = subactivity->ownerActivity;
      if (subactivity)
        subactivity->breakFunction = stopFunction;
      return [self run];
    }
  subactivity->breakFunction = installStep;
  return [self run];
}

//
// getStatus -- return symbol for current processing status
//              (overridden in ForEachActivity_c)
//
- (id <Symbol>)getStatus
{
  return status;
}

//
// getHoldType -- return HoldStart or HoldEnd status as provided by index
//
- (id <Symbol>)getHoldType
{
  return [currentIndex getHoldType];
}

//
// getActionType -- get action type of action being performed by activity
//
- getActionType
{
  return ((Index_any *) currentIndex)->collection;
}

//
// getAction -- get action containing parameter bindings for the local activity
//
- (id <Action>)getAction
{
  if (!topLevelAction)
    return [ownerActivity->currentIndex get];
  return topLevelAction;
}

//
// _getSubactivityAction_ --
//   internal method to access to local action without creating subactivity
//
- _getSubactivityAction_
{
  return GENERIC_GET (currentIndex);
}

//
// setOwnerActivity: -- change owner from one swarm activity to another
//
- (void)setOwnerActivity: (id <SwarmActivity>)aSwarmActivity
{
  if (getClass (aSwarmActivity) != id_SwarmActivity_c)
    raiseEvent (SourceMessage,
                "> new owner activity is not a swarm activity\n");

  if (getClass (ownerActivity) != id_SwarmActivity_c)
    raiseEvent (SourceMessage,
                "> cannot reassign owner activity unless running under a swarm activity\n");

  if (RUNNINGP (status))
    raiseEvent (SourceMessage,
                "> cannot change owner swarm while activity is running\n");
  //
  // Still need to figure out error-checking logic if running underneath
  // concurrent group in merge schedule.
  //
  
  raiseEvent (NotImplemented, nil);
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
- (id <Activity>)getOwnerActivity
{
  return topLevelAction ? nil : (id <Activity>)ownerActivity;
}

//
// getControllingActivity --
//   return activity that issued current run request on top-level activity
//
- (id <Activity>)getControllingActivity
{
  return topLevelAction ? (id <Activity>)ownerActivity : nil;

}

//
// getTopLevelActivity --
//   return top of activity tree running the local activity
//
- (id <Activity>)getTopLevelActivity
{
  Activity_c *activity;

  for (activity = self;
       activity && ! activity->topLevelAction;
       activity = activity->ownerActivity);

  return activity;
}

//
// getSwarmActivity -- return most immediately containing Swarm activity
//
- (id <SwarmActivity>)getSwarmActivity
{
  Activity_c *activity;

  for (activity = self;
       getClass (activity) != id_SwarmActivity_c;
       activity = activity->ownerActivity)
    if (activity->topLevelAction)
      return nil;
  
  return (SwarmActivity_c *) activity;
}

//
// getScheduleActivity -- return most immediately containing Schedule activity
//
- (id <Activity>)getScheduleActivity
{
  Activity_c  *activity;

  for (activity = self;
       getClass (activity) != id_ScheduleActivity_c
         && getClass (activity) != id_SwarmActivity_c;
       activity = activity->ownerActivity)
    if (activity->topLevelAction)
      return nil;
  
  return (ScheduleActivity_c *) activity;
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
  raiseEvent (NotImplemented, nil);
  return nil;
}

//
// setSerialMode -- set serial execution mode  
//
- (void)setSerialMode: (BOOL)serialMode
{
  if (!serialMode)
    raiseEvent (NotImplemented,
                "> A concurrent processing mode that would allow multiple simultaneous\n"
                "> subactivities to be present has not been implemented for any type of\n"
                "> activity except a Swarm.\n"
                );
}

//
// getSerialMode -- return indicator for serial execution mode 
//
- (BOOL)getSerialMode
{
  return YES;
}

- setKeepEmptyFlag: (BOOL)theKeepEmptyFlag
{
  keepEmptyFlag = theKeepEmptyFlag;
  return self;
}

//
// getCurrentSubactivity -- get running subactivity or next subactivity to run
//
- (id <Activity>)getCurrentSubactivity
{
  if (!currentSubactivity)
    {
      if (!currentIndex)
        return nil;
      raiseEvent (NotImplemented,
                  "> creation of a subactivity to obtain reference to leaf-level action has\n"
                  "> not yet been implemented\n");
    }
  return currentSubactivity;
}

- (void)dropAllocations: (BOOL)components
{
  if (registeredOwnerActivity)
    [registeredOwnerActivity->activitySet remove: self];
  if (activitySet)
    {
      if ([activitySet getCount] > 0)
        {
          id index = [activitySet begin: scratchZone];
          
          [index next];
          while ([index getLoc] == Member)
            {
              Activity_c *activity = [index remove];
              [index prev];
              [index next];
              [activity dropAllocations: components];
            }
          DROP (index);
        }
      [activitySet dropAllocations: YES];
    }
  [super dropAllocations: components];
}

//
// mapAllocations: -- standard method to identify internal allocations
//
- (void)mapAllocations: (mapalloc_t)mapalloc
{
  if (topLevelAction)
    mapObject (mapalloc, topLevelAction);
#if 0
  if (currentSubactivity)
    mapObject (mapalloc, currentSubactivity);
#endif
  mapObject (mapalloc, currentIndex);
}

//
// drop -- method for external caller to drop a completed activity
//
- (void)drop
{
  Activity_c  *activity;
  
  if (ownerActivity)
    {
      if (TERMINATEDP (status))
        {
          for (activity = self;
               activity->currentSubactivity;
               activity = activity->currentSubactivity);

          if (activity->breakFunction != terminateFunction)
            {
              [self dropAllocations: YES];
              return;
            }
        }
      else if (COMPLETEDP (status))
        {
          [self dropAllocations: YES];
          return;
        }

      raiseEvent (SourceMessage,
                  "> can only drop a top-level activity or a terminated activity that is not\n"
                  "> currently running\n" );
    }
  
  if (RUNNINGP (status))
    raiseEvent (SourceMessage,
                "> cannot drop an activity while it is running\n" );
  
  [self dropAllocations: YES];
}

- (void)describe: outputCharStream
{
  char buffer[100];
  
  [super describe: outputCharStream];
  [outputCharStream catC: "> current activity status: "];
  [outputCharStream catC: [status getName]];
  [outputCharStream catC: "\n> compound action being processed: "];
  
  _obj_formatIDString (buffer, ((Index_any *) currentIndex)->collection);
  [outputCharStream catC: buffer];
  [outputCharStream catC: "\n> Index of activity: \n"];
  [currentIndex describe: outputCharStream];
  if (currentSubactivity)
    {
      [outputCharStream
        catC: "\n> describe of current subactivity follows:\n"];
      [currentSubactivity describe: outputCharStream];
    }
  else
    [outputCharStream catC: "\n> activity has no current subactivity\n"];
}

@end

//
// _activity_context_error() --
//   function to generate error message if invalid context query
//
id
_activity_context_error (const char *macroName)
{
  if (!_activity_current)
    raiseEvent (InvalidOperation,
                "> %s(): there is no currently running activity from which\n"
                "> to obtain requested activity context information.\n"
                "> The context query macros are available only within a"
                "> compiled action being executed under a running activity.\n"
                "> They are not available when the activity is stopped\n"
                "> or otherwise inactive.\n"
                "> This includes any external probe request.\n",
                macroName);
  
  raiseEvent (InvalidOperation,
              "> %s(): an Swarm or Schedule does not exist in the current"
              ">activity context from which to obtain the requested value.\n",
              macroName);
  
  return nil;
}
