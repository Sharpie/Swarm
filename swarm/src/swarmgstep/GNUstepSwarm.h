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

#import <Swarm/Swarm.h> // Swarm
#import <Foundation/NSObject.h>
#import <Foundation/NSLock.h>
#import <Foundation/NSTimer.h>

@interface GNUstepSwarmController : NSObject
{
  id _delegate;
  id _theSwarm;
  id _modelThread;
  NSLock *_theLock;
  NSConditionLock *_theConditionLock;
  int _currentState;
  int _nextState;
}

// These methods are called by the GNUstep application controller object
// to control the Swarm simulation.
- (void)spawnSimulation: (Class)aSwarmClass withDelegate: anObject;
- (void)newSimulation: (Class)aSwarmClass;
- (void)simulationControl: (NSTimer *)aTimer;
- (void)startSwarmSimulation: sender;
- (void)stepSwarmSimulation: sender;
- (void)stopSwarmSimulation: sender;
- (void)dropSwarmSimulation: sender;

- getSwarm;

@end

// A GNUstepSwarm is a Swarm with support for interacting with a
// GNUstep application.  A GNUstep Swarm runs in a separate thread
// then the main thread for the GNUstep application; this is so that
// the Swarm can continue execution without interferring with the
// graphical interface and its runloop and event processing.

@interface GNUstepSwarm: Swarm
{
  id _delegate;
  id _threadController;
  id _threadSchedule;
}

- (void)setThreadController: anObject;
- (void)setDelegate: anObject;
- delegate;
- (void) viewNeedsDisplay: aView;

@end
