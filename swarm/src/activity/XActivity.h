// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         XActivity.h
Description:  state of processing within an action plan
Library:      activity
*/

#import <defobj/Create.h>
#import <activity/Action.h>

//
// Activity_c -- state of processing within an action type
//
@interface Activity_c : Object_s
{
@public
  Activity_c  *ownerActivity;        // activity running this activity
  CAction     *topLevelAction;       // action binding at top of stack
  id          status;                // symbol for current status
  member_t    activityRefs;          // internal links in references to plan
  id          currentIndex;          // index to current action in plan
  Activity_c  *currentSubactivity;   // subactivity for current action
  BOOL        immediateReturnFlag;   // return after first _performAction_
  BOOL        (*breakFunction)(id);  // function to call on each step
}
/*** methods in Activity_c (inserted from .m file by m2h) ***/
- run;
- _run_;
- (void) terminate;
- stop;
- next;
- step;
- getStatus;
- getHoldType;
- getActionType;
- getAction;
- _getSubactivityAction_;
- (void) setOwnerActivity: aSwarmActivity;
- getOwnerActivity;
- getControllingActivity;
- getTopLevelActivity;
- getSwarmActivity;
- getScheduleActivity;
- getSubactivities;
- (void) setSerialMode: (BOOL)serialMode;
- (BOOL) getSerialMode;
- getCurrentSubactivity;
- (void) mapAllocations: (mapalloc_t)mapalloc;
- (void) drop;
- (void) describe: outputCharStream;
@end
