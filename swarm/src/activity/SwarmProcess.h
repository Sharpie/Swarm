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
Name:         SwarmProcess.h
Description:  object to coordinate a collection of started activities
Library:      activity
*/

#import <Swarm/XActivity.h>

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
- (id <Activity>)activate;
- (id <Activity>)activate: anActionType;
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
