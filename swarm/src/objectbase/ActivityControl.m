// Swarm library. Copyright (C) 1996 Santa Fe Institute.  This library
// is distributed without any warranty; without even the implied
// warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Activity Controller class, for use on any activity in any
// Swarm. Controls the state of running activities, provides the
// "stop," "next," "step," "stepUntil," and "run," functions for any
// swarm.

#import <swarmobject/ActivityControl.h>

@implementation ActivityControl

+ createBegin: aZone {
  return [super createBegin: aZone];
}

- createEnd {
  return [super createEnd];
}

// functional methods

//
//  run -- Executes run on the activity
//
- run {
  [self updateStateVar];
  if (
      (isTopLevelActivity) &&
      (status != Completed) &&
      (status != Holding) && 
      (status != Running) 
       ) {
    status = [activity run];
  }

  return status;
}
//
//  stop -- Stops the execution of the activity
//
- stop {
  [self updateStateVar];
  // stop returns the activity, not the status
  if (status != Completed) [activity stop];
  if (!isTopLevelActivity) [[activity getOwnerActivity] stop];
  status = [activity getStatus];
  return status;
}
//
// next -- Causes the activity to execute an entire cycle of the activity's
//         schedule
- next {
  [self updateStateVar];
  // next returns what "run" returns, i.e. status
  if (
      (isTopLevelActivity) &&
      (status != Completed) &&
      (status != Holding) &&
      (status != Running) )
    status = [activity next];
  return status;
}
//
//  step -- Causes the activity to execute the next action on the 
//          schedule
- step {
  [self updateStateVar];
  // step returns what run returns, i.e. status
  if (
      (isTopLevelActivity) &&
      (status != Completed) &&
      (status != Holding) &&
      (status != Running) )
    status = [activity step];
  return status;
}

//
//  stepUntil -- Causes the activity to continue execution until
//               (current time +1 == stopTime)
- stepUntil: (timeval_t) stopTime {
  [self updateStateVar];
  // stupUntil returns the status
  if ( 
      (isTopLevelActivity) &&
      (status != Completed) &&
      (status != Holding) &&
      (status != Running) )
    status = [activity stepUntil: stopTime];
  return status;
}

//
//  terminate -- Causes the activity to recursively remove all its
//                       subactivities
- (void) terminate {
  [self updateStateVar];
  [activity terminate];
  activity = nil;
}

// state manipulation methods

//
//  attachToActivity -- Sets up the pointer to the controlled activity
//                      and builds and activates the schedule that 
//                      updates the state variables
- attachToActivity: (id) anActivity {

  // A schedule must be merged with (activated in) the Swarm rather
  //    than simply adding a single action to the Swarm activity schedule
  //    because the Swarm is not designed to handle singleton actions.
  updateSchedule = [Schedule createBegin: [self getZone]];
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

  return self;
}
//
// updateStateVar -- Sets all the relevant variables
//
- updateStateVar {

  // if there isn't an activity to be controlled, this method should 
  //    never be called.
  if (!activity) 
     raiseEvent ( InternalError,
        "Attempt to update the state variables on a nil activity.\n");
     
  // if it has an ownerActivity, then it is *not* controllable
  //    This will change with the control context book keeper.
  isTopLevelActivity = (([activity getOwnerActivity] == nil) ? 1 : 0);

  // There's still a problem with what the probe displays in it's
  //    variable widgets with respect to "Symbol"s.
  status = [activity getStatus];
  // This is a pointer to the next action to be executed.
  //  currentAction = [activity getCurrentAction];
  // This might be more useful as a relative time, depending on 
  //    what run level the activity is that is being controlled.
  currentTime = [activity getCurrentTime];

  return self;
}

//
// getStatus -- Simply returns the status
//
- getStatus {
  return status;
}

//
// getInstanceName -- This returns the Display name to the probe
//                    When probes are fixed to read this string directly
//                    this method will be obsolete.
- (const char *) getInstanceName {
   return [self getDisplayName];
}

//
// _setup_ProbeMap -- Designs a probe map that dictates the default
//                    probe for this class.
- _setup_ProbeMap {
  id <ProbeMap> probeMap;

  probeMap = [EmptyProbeMap createBegin: [self getZone]];
  [probeMap setProbedClass: [self class]];
  probeMap = [probeMap createEnd];

  [probeMap addProbe: [[probeLibrary getProbeForVariable: "currentTime"
				     inClass: [self class]]
			setNonInteractive]];
  [probeMap addProbe: [[probeLibrary getProbeForVariable: "status"
				     inClass: [self class]]
			setNonInteractive]];
  [probeMap addProbe: [[probeLibrary getProbeForVariable: "isTopLevelActivity"
				     inClass: [self class]]
			setNonInteractive]];

  [probeMap addProbe: [[probeLibrary getProbeForMessage: "run"
                             inClass: [self class]]
                        setHideResult: 0]];
  [probeMap addProbe: [[probeLibrary getProbeForMessage: "stop"
                             inClass: [self class]]
                        setHideResult: 0]];
  [probeMap addProbe: [[probeLibrary getProbeForMessage: "next"
                             inClass: [self class]]
                        setHideResult: 0]];
  [probeMap addProbe: [[probeLibrary getProbeForMessage: "step"
                             inClass: [self class]]
                        setHideResult: 0]];
  [probeMap addProbe: [[probeLibrary getProbeForMessage: "stepUntil:"
                             inClass: [self class]]
                        setHideResult: 0]];
  [probeMap addProbe: [[probeLibrary getProbeForMessage: "terminate"
                             inClass: [self class]]
                        setHideResult: 0]];

  [probeLibrary setProbeMap: probeMap For: [self class]];

  return self;
}
@end
