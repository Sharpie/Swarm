// Swarm library. Copyright (c) 2009 Swarm Development Group.
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
#import <Cocoa/Cocoa.h>

@interface OpenStepSwarmController : NSDocument
{
  id _delegate;
  id _theSwarm;
  id _modelThread;
  NSLock *_theLock;
  NSConditionLock *_theConditionLock;
  int _currentState;
  int _nextState;

  NSMutableDictionary *simParameters;
}

- (Class)modelGUIClass;
- (NSDictionary *)simulationParameters;

// These methods are called by the GNUstep application controller object
// to control the Swarm simulation.
- (void)newSimulation: (Class)aSwarmClass;
- (void)simulationControl: (NSTimer *)aTimer;
- (void)initializeSwarmSimulation: sender;
- (void)startSwarmSimulation: sender;
- (void)stepSwarmSimulation: sender;
- (void)stopSwarmSimulation: sender;
- (void)dropSwarmSimulation: sender;

- (void)attachDisplays;
- (void)swarmHasUpdated: (id)sender;

- getSwarm;

@end

// An OpenStepGUISwarm is a Swarm with support for interacting with an
// OpenStep GUI application.  An OpenStepGUISwarm runs in a separate thread
// then the main thread for the OpenStep application; this is so that
// the Swarm can continue execution without interferring with the
// graphical interface and its runloop and event processing.

@interface OpenStepGUISwarm: Swarm
{
  id _delegate;
  id _threadController;
  id _threadSchedule;
}

- (NSDictionary *)simulationParameters;

- (void)setThreadController: anObject;
- (void)setDelegate: anObject;
- delegate;
- (void)swarmHasUpdatedNotification;

@end

// An OpenStepSwarmModel is a Swarm for the top-level model in an
// OpenStep Swarm application.  It has support to be created by
// either a GUI or batch observable, and initializes itself
// specifically through model parameters passed to it.

@interface OpenStepSwarmModel: Swarm
{
	NSDictionary *simulationParameters;
}

+ create: (id)aSwarm withParameters: (NSDictionary *)params;

@end
