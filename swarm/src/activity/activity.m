// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
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

id  _activity_current, _activity_zone;
id  _activity_activityRefsType, _activity_swarmSyncType;

void _activity_implement( void )
{
  initModule( collections );

  // implement types from their defining classes

  [id_ActionGroup_c        setTypeImplemented: ActionGroup ];
  [id_Schedule_c           setTypeImplemented: Schedule    ];
  [id_CSwarmProcess        setTypeImplemented: SwarmProcess];
  [id_ConcurrentGroup_c    setTypeImplemented: ConcurrentGroup];
  [id_ConcurrentSchedule_c setTypeImplemented: ConcurrentSchedule];
  [id_ActivationOrder_c    setTypeImplemented: ActivationOrder];
}

void _activity_initialize( void )
{

  _activity_zone = globalZone;

  _activity_activityRefsType = [OrderedSet customizeBegin: globalZone];
  [_activity_activityRefsType
    setIndexFromMemberLoc: offsetof( Activity_c, activityRefs )];
  _activity_activityRefsType = [_activity_activityRefsType customizeEnd];

  _activity_swarmSyncType = [Schedule customizeBegin: globalZone];
  [_activity_swarmSyncType setConcurrentGroupType: ActivationOrder];
  [_activity_swarmSyncType setAutoDrop: 1];
  _activity_swarmSyncType = [_activity_swarmSyncType customizeEnd];

  [InvalidSwarmZone setMessageString:
"> zone message invalid because swarm was created with no internal zone\n"];
}
