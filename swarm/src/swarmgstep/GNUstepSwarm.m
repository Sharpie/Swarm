// Swarm library. Copyright © 2003 Swarm Development Group.
//
// Author: Scott Christley
//
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

#import <swarmgstep/GNUstepSwarm.h>
#import <Foundation/NSThread.h>
#import <Foundation/NSRunLoop.h>
#import <Foundation/NSTimer.h>
#import <AppKit/NSView.h>

#define SG_STATE_NOTHING 0
#define SG_STATE_NEW 1
#define SG_STATE_READY 2
#define SG_STATE_STEP 3
#define SG_STATE_START 4
#define SG_STATE_STOP 5
#define SG_STATE_DROP 6

#define SG_COND_A 1
#define SG_COND_B 2

@implementation GNUstepSwarmController

- (void)spawnSimulation: (Class)aSwarmClass withDelegate: anObject
{
  fprintf(stderr, "[GNUstepSwarmController spawnSimulation:]\n");

  _delegate = anObject;
  _currentState = SG_STATE_NEW;
  _nextState = SG_STATE_NOTHING;
  _theLock = [NSLock new];
  _theConditionLock = [[NSConditionLock alloc] initWithCondition: SG_COND_B];

  [NSThread detachNewThreadSelector: @selector(newSimulation:)
	    toTarget: self withObject: aSwarmClass];

#if 1
  // Wait for swarm to be ready
  [_theConditionLock lockWhenCondition: SG_COND_A];
  [_theConditionLock unlockWithCondition: SG_COND_A];
#endif
}

- (void)newSimulation: (Class)aSwarmClass
{
  NSAutoreleasePool *pool;

  fprintf(stderr, "[GNUstepSwarmController newSimulation:]\n");

  pool = [NSAutoreleasePool new];
  _modelThread = [NSThread currentThread];

  _theSwarm = [aSwarmClass createBegin: globalZone];
  [_theSwarm setThreadController: self];
  [_theSwarm setDelegate: _delegate];
  _theSwarm = [_theSwarm createEnd];

  [_theSwarm buildObjects];
  [_theSwarm buildActions];
  [_theSwarm activateIn: nil];

  _currentState = SG_STATE_STOP;

  // signal the other thread we are initialized
  [_theConditionLock lockWhenCondition: SG_COND_B];
  [_theConditionLock unlockWithCondition: SG_COND_A];

  [NSTimer scheduledTimerWithTimeInterval: 0.000001
	   target: self selector: @selector(simulationControl:)
	   userInfo: nil repeats: YES];

  fprintf(stderr, "before run\n");
  [[NSRunLoop currentRunLoop] run];
  fprintf(stderr, "after run\n");

  [NSThread exit];
}

- (void)processStateFlagWithOriginal: (int)originalState
{
  switch (_currentState)
    {
    case SG_STATE_DROP:
      fprintf(stderr, "Terminating simulation\n");
      [getTopLevelActivity() terminate];
      break;
    case SG_STATE_START:
      [[_theSwarm getActivity] stepUntil: [[_theSwarm getActivity]
					    getCurrentTime] + 1];
      break;
    case SG_STATE_STEP:
      [[_theSwarm getActivity] stepUntil: [[_theSwarm getActivity]
					    getCurrentTime] + 1];
      _currentState = SG_STATE_STOP;
      break;
    case SG_STATE_STOP:
      break;
    }
}

- (void)simulationControl: (NSTimer *)aTimer
{
  int originalState = _currentState;

  // Check if the user initiated a new state
  if (_nextState != SG_STATE_NOTHING)
    {
      [_theLock lock];
      fprintf(stderr, "Got new state %d\n", _nextState);
      _currentState = _nextState;
      _nextState = SG_STATE_NOTHING;
      [_theLock unlock];
    }

  [self processStateFlagWithOriginal: originalState];
}

- getSwarm
{
  return _theSwarm;
}

- (void)startSwarmSimulation: sender
{
  [_theLock lock];
  _nextState = SG_STATE_START;
  [_theLock unlock];
}

- (void)stepSwarmSimulation: sender
{
  [_theLock lock];
  _nextState = SG_STATE_STEP;
  [_theLock unlock];
}

- (void)stopSwarmSimulation: sender
{
  [_theLock lock];
  _nextState = SG_STATE_STOP;
  [_theLock unlock];
}

- (void)dropSwarmSimulation: sender
{
  [_theLock lock];
  _nextState = SG_STATE_DROP;
  [_theLock unlock];
}

- (void) performDisplayUpdate: aView
{
#if 0
  fprintf(stderr, "performDisplayUpdate:\n");
#endif
  [aView setNeedsDisplay: YES];
}

- (void) viewNeedsDisplay: aView
{
#if 0
  fprintf(stderr, "viewNeedsDisplay:\n");
#endif
  if (!aView)
    return;

  // signal the main thread to perform the display updates
  [self performSelectorOnMainThread: @selector(performDisplayUpdate:)
	withObject: aView
	waitUntilDone: YES];
}

@end

@implementation GNUstepSwarm

PHASE(Creating)

+ createBegin: aZone
{
  GNUstepSwarm *obj;

  obj = [super createBegin: aZone];

  obj->_delegate = nil;

  return obj;
}

- (void)setThreadController: anObject
{
  _threadController = anObject;
}

- (void)setDelegate: anObject
{
  _delegate = anObject;
}

- createEnd
{
  return [super createEnd];
}

PHASE(Using)

- delegate
{
  return _delegate;
}

- buildObjects
{
  [super buildObjects];

  return self;
}

- buildActions
{
  [super buildActions];

#if 0
  // The _threadSchedule controls how often we check the thread
  // communication flag; by default each time step.
  _threadSchedule = [Schedule createBegin: self];
  [_threadSchedule setRepeatInterval: 1];
  _threadSchedule = [_threadSchedule createEnd];
  [_threadSchedule at: 0 createActionTo: _threadController
		   message: M(checkThreadFlag)];
#endif

  return self;
}

- activateIn: swarmContext
{
  [super activateIn: swarmContext];

  // Activate the thread checking schedule
  [_threadSchedule activateIn: self];

  return [self getActivity];
}

- (void)drop
{
  [super drop];
}

- (void) viewNeedsDisplay: aView
{
  [_threadController viewNeedsDisplay: aView];
}

@end
