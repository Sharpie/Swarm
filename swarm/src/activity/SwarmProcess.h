// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         SwarmProcess.h
Description:  object to coordinate a collection of started activities
Library:      activity
*/

#import <activity/XActivity.h>

@class SwarmActivity_c, ScheduleActivity_c;

@interface CSwarmProcess : CreateDrop_s
{
@public
  id               internalZone;  // internal zone to hold objects of swarm
  id               syncType;      // type of synchronization schedule, if any
  SwarmActivity_c  *activity;     // activity running swarm subactivities
  ref_t            activityRef;   // reference notification on activity
}
/*** Zone pass-through methods (manually inserted) ***/
- (int) getPageSize;
- allocIVars: aClass;
- copyIVars: anObject;
- (void) freeIVars: anObject;
- allocIVarsComponent: aClass;
- copyIVarsComponent: anObject;
- (void) freeIVarsComponent: anObject;
- getComponentZone;
- (void *) alloc: (size_t)size;
- (void) free: (void *) aBlock;
- (void *) allocBlock: (size_t)size;
- (void) freeBlock: (void *) aBlock blockSize: (size_t)size;
- getPopulation;

/*** methods in CSwarmProcess (inserted from .m file by m2h) ***/
+ createBegin: aZone;
- (void) setSynchronizationType: aScheduleType;
- (void) setInternalZoneType: internalZoneType;
- (void) setInternalTimeMultiplier: (timeval_t)internalTimeMultiplier;
- createEnd;
- (timeval_t) getInternalTimeMultiplier;
- getInternalZone;
- getActivity;
- getSwarmActivity;
- activate;
- activate: anActionType;
- at: (timeval_t)tVal activate: anActionType;
- at: (int)timebase : (timeval_t)tVal activate: anActionType;
- activateIn: swarmContext;
- (void) mapAllocations: (mapalloc_t)mapalloc;
- (void) _performPlan_;
@end

//
// SwarmActivity_c and ActionMerge_c declared in Schedule.h so that import is
// is minimized for user subclassing of a custom Swarm 
//
