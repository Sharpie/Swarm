// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         init_activity.m
Description:  function to initialize activity group
Library:      activity
*/

#import "activity.xm"
#import <activity/XActivity.h>

externvardef id _activity_activityRefsType, _activity_activitySetRefsType;
externvardef id _activity_swarmSyncType;

void
_activity_implement (void)
{
  initModule (collections);

  // implement types from their defining classes
  [id_FAction_c setTypeImplemented: FAction];

  [id_ActionCall_c setTypeImplemented: ActionCall];
  [id_ActionTo_c setTypeImplemented: ActionTo];
  [id_ActionForEach_c setTypeImplemented: ActionForEach];
  [id_ActionForEachHomogeneous_c setTypeImplemented: ActionForEachHomogeneous];

  [id_FActionForEachHeterogeneous_c setTypeImplemented: FActionForEachHeterogeneous];
  [id_FActionForEachHomogeneous_c setTypeImplemented: FActionForEachHomogeneous];
  [id_ActionChanged_c setTypeImplemented: ActionChanged];
  
  [id_Activity_c setTypeImplemented: Activity];
  [id_ScheduleActivity_c setTypeImplemented: ScheduleActivity];
  [id_SwarmActivity_c setTypeImplemented: SwarmActivity];
  
  [id_ActionGroup_c setTypeImplemented: ActionGroup];
  [id_Schedule_c setTypeImplemented: Schedule];
  [id_CSwarmProcess setTypeImplemented: SwarmProcess];
  [id_ConcurrentGroup_c setTypeImplemented: ConcurrentGroup];
  [id_ConcurrentSchedule_c setTypeImplemented: ConcurrentSchedule];
  [id_ActivationOrder_c setTypeImplemented: ActivationOrder];
}

void
_activity_initialize (void)
{
  _activity_zone = globalZone;

  _activity_activityRefsType = [OrderedSet customizeBegin: globalZone];
  [_activity_activityRefsType
    setIndexFromMemberLoc: offsetof (Activity_c, activityRefs)];
  _activity_activityRefsType = [_activity_activityRefsType customizeEnd];

  _activity_activitySetRefsType = [OrderedSet customizeBegin: globalZone];
  [_activity_activitySetRefsType
    setIndexFromMemberLoc: offsetof (Activity_c, activitySetRefs)];
  _activity_activitySetRefsType = [_activity_activitySetRefsType customizeEnd];

  _activity_swarmSyncType = [Schedule customizeBegin: globalZone];
  [_activity_swarmSyncType setConcurrentGroupType: ActivationOrder];
  [_activity_swarmSyncType setAutoDrop: YES];
  _activity_swarmSyncType = [_activity_swarmSyncType customizeEnd];

  [InvalidSwarmZone setMessageString:
"> zone message invalid because swarm was created with no internal zone\n"];

  defsymbol (Randomized);
  defsymbol (Sequential);
  defsymbol (Concurrent);
}
