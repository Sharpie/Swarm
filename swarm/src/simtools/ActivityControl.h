// Swarm library. Copyright (C) 1996 Santa Fe Institute.  This library
// is distributed without any warranty; without even the implied
// warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Activity Controller class, for use on any activity in any
// Swarm. Controls the state of running activities, provides the
// "stop," "next," "step," "stepUntil," and "run," functions for any
// swarm.

#import <swarmobject.h>
#import <activity.h>

@interface ActivityControl : SwarmObject {
@public
  id activity;    // my pointer
  id runContext;  // activity running me
  id status;  // my state
  id actionPlan;      // my plan
  id currentAction;   // what I'm doing right now
  timeval_t currentTime;  // my time index
  id updateSchedule; // schedule to merge with the activity 
  const char * activityControlName;  // name of this object
  int ownerActivity;  // Swarm I'm executing over
}

// instantiation methods
+ createBegin: aZone;
- createEnd;

// functional methods
- run;
- stop;
- next;
- step;
- stepUntil: (timeval_t) stopTime;
- (void) terminateActivity;

//  At some point, we may add -stepIntoSubActivity and
//  -finishThisActivity  or somesuch to provide activation graph
//  traversal

// state manipulation methods
//    attach will set all the state variables
- attachToActivity: (id) anActivity;
- updateStateVar;
- (char *) getInstanceName;
-                setInstanceNameTo: (char *) iName;
@end









