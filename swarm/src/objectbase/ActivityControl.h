// Swarm library. Copyright © 1996-2000 Swarm Development Group.  This library
// is distributed without any warranty; without even the implied
// warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Activity Controller class, for use on any activity in any
// Swarm. Controls the state of running activities, provides the
// "stop," "next," "step," "stepUntil," and "run," functions for any
// swarm.

#import <Swarm/objectbase.h>
#import <Swarm/SwarmObject.h>
#import <Swarm/activity.h>

@interface ActivityControl: SwarmObject <ActivityControl>
{
@public
  timeval_t currentTime;  // my time index
  id <Symbol> status;  // my state
  BOOL isTopLevelActivity;  // 1 = top-level; 0=not
@private
  id <ScheduleActivity> activity;  // my pointer
  //  id currentAction;   // what I'm doing right now
  id <Schedule> updateSchedule; // schedule to merge with the activity 
}

// functional methods
- (id <Symbol>)runActivity;
- (id <Symbol>)stopActivity;
- (id <Symbol>)nextAction;
- (id <Symbol>)stepAction;
- (id <Symbol>)stepUntil: (timeval_t) stopTime;
- (void)terminate;

//  At some point, we may add -stepIntoSubActivity and
//  -finishThisActivity  or somesuch to provide activation graph
//  traversal

// state manipulation methods
//    attach will set all the state variables
- (void)attachToActivity: (id <ScheduleActivity>)anActivity;
- (void)updateStateVar;
- (id <Symbol>)getStatus;
- (void)_setup_ProbeMap;
- (id <ScheduleActivity>)getActivity;
@end








