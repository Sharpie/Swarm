// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.  This library
// is distributed without any warranty; without even the implied
// warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Activity Controller class, for use on any activity in any
// Swarm. Controls the state of running activities, provides the
// "stop," "next," "step," "stepUntil," and "run," functions for any
// swarm.

#import <objectbase.h>   
#import <objectbase/SwarmObject.h>

@interface ActivityControl : SwarmObject {
@public
  timeval_t currentTime;  // my time index
  id status;  // my state
  BOOL isTopLevelActivity;  // 1 = top-level; 0=not
@private
  id activity;    // my pointer
  //  id currentAction;   // what I'm doing right now
  id updateSchedule; // schedule to merge with the activity 
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
- (void) terminate;

//  At some point, we may add -stepIntoSubActivity and
//  -finishThisActivity  or somesuch to provide activation graph
//  traversal

// state manipulation methods
//    attach will set all the state variables
- attachToActivity: (id) anActivity;
- updateStateVar;
- getStatus;
- (const char *) getInstanceName;
- _setup_ProbeMap;
@end









