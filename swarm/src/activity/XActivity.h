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
Name:         XActivity.h
Description:  state of processing within an action plan
Library:      activity
*/

#import <Swarm/Create.h>
#import <Swarm/activity.h>

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
- (id <Activity>)getScheduleActivity;
- getSubactivities;
- (void)setSerialMode: (BOOL)serialMode;
- (BOOL)getSerialMode;
- setKeepEmptyFlag: (BOOL)keepEmptyFlag;
- (id <Activity>)getCurrentSubactivity;
- (void)mapAllocations: (mapalloc_t)mapalloc;
- (void)drop;
- (void)describe: outputCharStream;
@end

