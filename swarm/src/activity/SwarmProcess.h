// Swarm library. Copyright � 1996-1999 Santa Fe Institute.
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

@interface CSwarmProcess: CreateDrop_s <SwarmProcess>
{
@public
  id internalZone;           // internal zone to hold objects of swarm
  id syncType;               // type of synchronization schedule, if any
  SwarmActivity_c *activity; // activity running swarm subactivities
}
/*** Zone pass-through methods (manually inserted) ***/
- (size_t)getPageSize;
- allocIVars: aClass;
- copyIVars: anObject;
- (void)freeIVars: anObject;
- allocIVarsComponent: aClass;
- copyIVarsComponent: anObject;
- (void)freeIVarsComponent: anObject;
- getComponentZone;
- (void *)alloc: (size_t)size;
- (void)free: (void *) aBlock;
- (void *)allocBlock: (size_t)size;
- (void)freeBlock: (void *)aBlock blockSize: (size_t)size;
- getPopulation;
- (void) describeForEachID: (id) outputCharStream;
- (void) describeForEach: (id)  outputCharStream;
- (BOOL)containsAlloc:(void *) alloc;
- (BOOL)getStackedSubzones;
- getReclaimPolicy;

/*** methods in CSwarmProcess (inserted from .m file by m2h) ***/
+ createBegin: aZone;
- (void)setSynchronizationType: aScheduleType;
- (void)setInternalZoneType: internalZoneType;
- (void)setInternalTimeMultiplier: (timeval_t)internalTimeMultiplier;
- (void)setPageSize: (size_t)pageSize;
- createEnd;
- (timeval_t)getInternalTimeMultiplier;
- getInternalZone;
- (id <SwarmActivity>)getActivity;
- (id <SwarmActivity>)getSwarmActivity;
- getSynchronizationType;
- activate;
- activate: anActionType;
- at: (timeval_t)tVal activate: anActionType;
- at: (int)timebase : (timeval_t)tVal activate: anActionType;
- (id <Activity>)activateIn: swarmContext;
- (void)mapAllocations: (mapalloc_t)mapalloc;
- (void)_performPlan_;
@end

//
// SwarmActivity_c and ActionMerge_c declared in Schedule.h so that import is
// is minimized for user subclassing of a custom Swarm 
//
