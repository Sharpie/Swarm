// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         XActivity.h
Description:  state of processing within an action plan
Library:      activity
*/

#import <defobj/Create.h>
#import <activity.h>

//
// Activity_c -- state of processing within an action type
//
@interface Activity_c: Object_s <Activity>
{
@public
  Activity_c *ownerActivity;          // activity running this activity
  id <Action> topLevelAction;         // action binding at top of stack
  id <Symbol> status;                 // symbol for current status
  member_t activityRefs;              // internal links in references to plan

  Activity_c *registeredOwnerActivity; // activity used te this activity
  id activitySet;                      // activities to cleanup on drop
  member_t activitySetRefs;            // internal links for owner

  id <ActivityIndex> currentIndex;     // index to current action in plan

  Activity_c *currentSubactivity;      // subactivity for current action
  BOOL (*breakFunction) (id);          // function to call on each step
  BOOL keepEmptyFlag;
}
/*** methods in Activity_c (inserted from .m file by m2h) ***/
- (id <Symbol>)run;
- (id <Symbol>)_run_;
- (void)terminate;
- (void)stop;
- (id <Symbol>)nextAction;
- (id <Symbol>)stepAction;
- (id <Symbol>)getStatus;
- (id <Symbol>)getHoldType;
- getActionType;
- (id <Action>)getAction;
- _getSubactivityAction_;
- (void)setOwnerActivity: (id <SwarmActivity>)aSwarmActivity;
- (id <Activity>)getOwnerActivity;
- (id <Activity>)getControllingActivity;
- (id <Activity>)getTopLevelActivity;
- (id <SwarmActivity>)getSwarmActivity;
- (id <ScheduleActivity>)getScheduleActivity;
- getSubactivities;
- (void)setSerialMode: (BOOL)serialMode;
- (BOOL)getSerialMode;
- setKeepEmptyFlag: (BOOL)keepEmptyFlag;
- (id <Activity>)getCurrentSubactivity;
- (void)mapAllocations: (mapalloc_t)mapalloc;
- (void)drop;
- (void)describe: outputCharStream;
@end

