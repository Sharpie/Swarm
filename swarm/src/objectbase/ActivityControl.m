// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular
// purpose.  See file LICENSE for details and terms of copying.

// Activity Controller class, for use on any activity in any
// Swarm. Controls the state of running activities, provides the
// "stopActivity," "nextAction," "stepAction," "stepUntil," and
// "runActivity," functions for any swarm.

#import <objectbase/ActivityControl.h>
#import <activity.h>
#import <defobj/defalloc.h> // getZone

@implementation ActivityControl
PHASE(Creating)
PHASE(Using)
// functional methods

//
//  runActivity -- Executes run on the activity
//
- (id <Symbol>)runActivity
{
  [self updateStateVar];
  if ((isTopLevelActivity) &&
      (status != Completed) &&
      (status != Holding) && 
      (status != Running))
    status = [activity run];
  
  return status;
}

//
//  stopActivity -- Stops the execution of the activity
//
- (id <Symbol>)stopActivity
{
  [self updateStateVar];
  // stop returns the activity, not the status
  if (status != Completed)
    [activity stop];
  if (!isTopLevelActivity)
    [[activity getOwnerActivity] stop];
  status = [activity getStatus];
  return status;
}

//
// nextAction -- Causes the activity to execute an entire cycle of the 
//               activity's schedule
- (id <Symbol>)nextAction
{
  [self updateStateVar];
  // next returns what "run" returns, i.e. status
  if ((isTopLevelActivity) &&
      (status != Completed) &&
      (status != Holding) &&
      (status != Running))
    status = [activity nextAction];
  return status;
}

//
//  stepAction -- Causes the activity to execute the next action on the 
//                schedule
- (id <Symbol>)stepAction
{
  [self updateStateVar];
  // step returns what run returns, i.e. status
  if ((isTopLevelActivity) &&
      (status != Completed) &&
      (status != Holding) &&
      (status != Running))
    status = [activity stepAction];
  return status;
}

//
//  stepUntil -- Causes the activity to continue execution until
//               (current time +1 == stopTime)
- (id <Symbol>)stepUntil: (timeval_t)stopTime
{
  [self updateStateVar];
  // stepUntil returns the status
  if ((isTopLevelActivity) &&
      (status != Completed) &&
      (status != Holding) &&
      (status != Running))
    status = [activity stepUntil: stopTime];
  return status;
}

//
//  terminate -- Causes the activity to recursively remove all its
//                       subactivities
- (void)terminate
{
  [self updateStateVar];
  [activity terminate];
  activity = nil;
}

// state manipulation methods

//
//  attachToActivity -- Sets up the pointer to the controlled activity
//                      and builds and activates the schedule that 
//                      updates the state variables
- (void)attachToActivity: (id <ScheduleActivity>)anActivity
{
  // A schedule must be merged with (activated in) the Swarm rather
  //    than simply adding a single action to the Swarm activity schedule
  //    because the Swarm is not designed to handle singleton actions.
  updateSchedule = [Schedule createBegin: getZone (self)];
  // The repeat interval could be made a function of the display freq.
  [updateSchedule setRepeatInterval: 1];
  updateSchedule = [updateSchedule createEnd];

  // set the pointer to the activity we're controlling
  activity = anActivity;

  // merge this schedule with the swarm activity
  [updateSchedule activateIn: activity];

  // set the rest of the state variables
  [self updateStateVar];

  // Create the probe map to be used if this object is probed.  This 
  //    probe remains the same regardless of the type of device that
  //    is using it to control an activity.
  [self _setup_ProbeMap];

  // Put the update action on the schedule.
  [updateSchedule at: 0 createActionTo: self message: M(updateStateVar)];
}

//
// updateStateVar -- Sets all the relevant variables
//
- (void)updateStateVar
{
  // if there isn't an activity to be controlled, this method should 
  //    never be called.
  if (!activity) 
    raiseEvent (InternalError,
                "Attempt to update the state variables on a nil activity.\n");
  
  // if it has an ownerActivity, then it is *not* controllable
  //    This will change with the control context book keeper.
  isTopLevelActivity = (([activity getOwnerActivity] == nil) ? YES : NO);
  
  // There's still a problem with what the probe displays in it's
  //    variable widgets with respect to "Symbol"s.
  status = [activity getStatus];
  // This is a pointer to the next action to be executed.
  //  currentAction = [activity getCurrentAction];
  // This might be more useful as a relative time, depending on 
  //    what run level the activity is that is being controlled.
  currentTime = [activity getCurrentTime];
}

//
// getStatus -- Simply returns the status
//
- (id <Symbol>)getStatus
{
  return status;
}

- (id <ScheduleActivity>)getActivity
{
  return activity;
}

//
// _setup_ProbeMap -- Designs a probe map that dictates the default
//                    probe for this class.
- (void)_setup_ProbeMap
{
  id <ProbeMap> probeMap;
  
  probeMap = [EmptyProbeMap createBegin: getZone (self)];
  [probeMap setProbedClass: getClass (self)];
  probeMap = [probeMap createEnd];

  [probeMap addProbe: [[probeLibrary getProbeForVariable: "currentTime"
				     inClass: getClass (self)]
			setNonInteractive]];
  [probeMap addProbe: [[probeLibrary getProbeForVariable: "status"
				     inClass: getClass (self)]
			setNonInteractive]];
  [probeMap addProbe: [[probeLibrary getProbeForVariable: "isTopLevelActivity"
				     inClass: getClass (self)]
			setNonInteractive]];

  [probeMap addProbe: [[probeLibrary getProbeForMessage: "runActivity"
                             inClass: getClass (self)]
                        setHideResult: 0]];
  [probeMap addProbe: [[probeLibrary getProbeForMessage: "stopActivity"
                             inClass: getClass (self)]
                        setHideResult: 0]];
  [probeMap addProbe: [[probeLibrary getProbeForMessage: "nextAction"
                             inClass: getClass (self)]
                        setHideResult: 0]];
  [probeMap addProbe: [[probeLibrary getProbeForMessage: "stepAction"
                             inClass: getClass (self)]
                        setHideResult: 0]];
  [probeMap addProbe: [[probeLibrary getProbeForMessage: "stepUntil:"
                             inClass: getClass (self)]
                        setHideResult: 0]];
  [probeMap addProbe: [[probeLibrary getProbeForMessage: "terminate"
                             inClass: getClass (self)]
                        setHideResult: 0]];

  [probeLibrary setProbeMap: probeMap ForObject: self];
}
@end
