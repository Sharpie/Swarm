// Swarm library. Copyright (C) 1996 Santa Fe Institute.  This library
// is distributed without any warranty; without even the implied
// warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Activity Controller class, for use on any activity in any
// Swarm. Controls the state of running activities, provides the
// "stop," "next," "step," "stepUntil," and "run," functions for any
// swarm.

#import <simtools/ActivityControl.h>

@implementation ActivityControl

// createBegin included in case there are accounting objects necessary
// in the future that will need to be allocated
+ createBegin: aZone {
  return [super createBegin: aZone];
}

// same with createEnd
- createEnd {
  return [super createEnd];
}

// functional methods

- run {
  [self updateStateVar];
  if ( (currentAction != nil) &&
       (!ownerActivity) && 
       (status != Completed) &&
       (status != Holding) &&
       (status != Running) )
    status = [activity run];

  return status;
}
- stop {
  [self updateStateVar];
  // stop returns the activity, not the status
  if (status != Completed) [activity stop];
  status = [activity getStatus];
  return status;
}
- next {
  [self updateStateVar];
  // next returns what "run" returns, i.e. status
  if ( (currentAction != nil) &&
       (status != Completed) &&
       (status != Holding) &&
       (status != Running) )
    status = [activity next];
  return status;
}
- step {
  [self updateStateVar];
  // step returns what run returns, i.e. status
  if ( (currentAction != nil) &&
       (status != Completed) &&
       (status != Holding) &&
       (status != Running) )
    status = [activity step];
  return status;
}
- stepUntil: (timeval_t) stopTime {
  [self updateStateVar];
  // stupUntil returns the status
  if ( (currentAction != nil) &&
       (!ownerActivity) &&
       (status != Completed) &&
       (status != Holding) &&
       (status != Running) )
    status = [activity stepUntil: stopTime];
  return status;
}
- (void) terminateActivity {
  [self updateStateVar];
  [activity terminate];
}

// state manipulation methods
- attachToActivity: (id) anActivity {

  updateSchedule = [Schedule createBegin: [self getZone]];
//  [updateSchedule setAutoDrop: 1];  can't use autodrop on  repeating schedule
  [updateSchedule setRepeatInterval: 1];
  updateSchedule = [updateSchedule createEnd];

  // set the pointer to the activity we're controlling
  activity = anActivity;

  // merge this schedule with the activity's
  [updateSchedule activateIn: activity];

  // set the rest of the state variables
  [self updateStateVar];

  [updateSchedule at: 0 createActionTo: self message: M(updateStateVar)];
  return self;
}

// state updater
- updateStateVar {

  (void) fprintf(stdout,"%s: ownerActivity = %x (pointer) %x (message)\n",
                 activityControlName, activity->ownerActivity, ownerActivity = [activity getOwnerActivity]);
  runContext = [activity getRunContext];
  status = [activity getStatus];
  actionPlan = [activity getActionPlan];
  currentAction = [activity getCurrentAction];
  currentTime = [activity getCurrentTime];

  (void) fprintf(stdout, "\n");

  return self;
}

- (char *) getInstanceName {
   return activityControlName;
}

- setInstanceNameTo: (char *) iName {
  activityControlName = iName;
  return self;
}

