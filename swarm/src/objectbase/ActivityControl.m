// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.  This library
// is distributed without any warranty; without even the implied
// warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Activity Controller class, for use on any activity in any
// Swarm. Controls the state of running activities, provides the
// "stop," "next," "step," "stepUntil," and "run," functions for any
// swarm.

#import <objectbase/ActivityControl.h>

//S: A class that provides an object interface to an activity.
//D: The ActivityControl class specifies an object that can be attached to 
//D: an activity (regardless of how or where that activity is created) for
//D: the purpose of explicitly controlling the execution of actions on that 
//D: activity's action plan. There is nothing that available through this 
//D: class that is not already available through the variables or messages 
//D: available via the activity itself. However, it packages up the main 
//D: control messages and makes them available to other objects that may need 
//D: control over an activity, thereby shielding the activity from directly
//D: receiving messages from outside objects and saving the user from having 
//D: to parse the more complex interface to the activity. 
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
//M:  The run method sends a run message to the activity if the conditions are
//M:  appropriate.  This message causes the activity to continue executing the
//M:  actions on its schedule until either no other actions are waiting, or 
//M:  until the execution of actions is stopped by a subactivity or stopped by
//M:  a stop message to the activity.  If the activity completes executing all 
//M:  the actions on its schedule, the run method returns Completed.
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
//M:  The stop method sends a stop message to the activity if the conditions 
//M:  are appropriate. This message causes the control to move back up the 
//M:  run-stack and resume at the place in the code where the run was first 
//M:  executed. The next action on the super-activity will begin without  
//M:  finishing the rest of the current activity's actions. 
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
//M:  The next method sends a next message to the activity if the conditions 
//M:  are appropriate. It runs an activity forward through as many actions as 
//M:  necessary until it hits a breakFunction, at which point it walks back up 
//M:  the tree of activities and returns Stopped. In most cases, this means
//M:  that an entire action or action group on the activity under control 
//M:  will be executed, including completion of all subactivities. 
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
//M:  The step method sends a step message to the activity if the conditions
//M:  are appropriate. It causes the execution of a single action. 
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
//M:  The stepUntil: method sends a stepUntil: message to the activity if 
//M:  conditions are appropriate. This causes all actions on the activity's 
//M:  schedule, including any actions on subactivities' schedules, to be 
//M:  executed until the activity's relative time is equal to stopTime - 1. 
- stepUntil: (timeval_t) stopTime {
  [self updateStateVar];
  // stepUntil returns the status
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
//M:  The terminate method sends the terminate message to the activity, which 
//M:  causes all actions and action groups to be removed from its schedule.
//M:  Note: if terminate is sent to an activity and subsequently, a run is 
//M:  attempted on that activity, the program will exit with an error. 
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
//M:  The attachToActivity: method sets an instance variable inside the 
//M:  ActivityControl object that points to the Activity to be controlled. 
//M:  It then creates a Schedule upon which it places a message to itself to
//M:  update its own variables.
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
//M:  The updateStateVar method updates the ActivityControl instance variables
//M:  and tests for the continued existence of the activity that is being 
//M:  controlled. This message is sent on each cycle of the schedule for the 
//M:  activity being controlled.
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
//M:  The getStatus method returns the status of the activity.
- getStatus {
  return status;
}

//
// getInstanceName -- This returns the Display name to the probe
//                    When probes are fixed to read this string directly
//                    this method will be obsolete.
//M:  The getInstanceName method returns the displayName from the object name
//M:  database. 
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
