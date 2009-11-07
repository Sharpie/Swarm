// Swarm library. Copyright � 2003 Swarm Development Group.
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

#import "OpenStepSwarm.h"

#define SG_STATE_NOTHING 0
#define SG_STATE_NEW 1
#define SG_STATE_READY 2
#define SG_STATE_STEP 3
#define SG_STATE_START 4
#define SG_STATE_STOP 5
#define SG_STATE_DROP 6

#define SG_COND_A 1
#define SG_COND_B 2
#define SG_COND_WAIT 3
#define SG_COND_RUN 4

@implementation OpenStepSwarmController

//
// Manage the user interface
//

- (BOOL)validateUserInterfaceItem:(id <NSValidatedUserInterfaceItem>)anItem
{
    SEL theAction = [anItem action];
	
	if (theAction == @selector(initializeSwarmSimulation:)) {
		if (!_theSwarm) {
			// Haven't been initialized yet.
			return YES;
		}
		return NO;
	} else if (theAction == @selector(startSwarmSimulation:)) {
		if ((_theSwarm) && (_currentState == SG_STATE_STOP)) {
			// We have to be initialized and stopped, in order to start
			return YES;
        }
        return NO;
	} else if (theAction == @selector(stopSwarmSimulation:)) {
		if ((_theSwarm) && (_currentState == SG_STATE_START)) {
			// We have to be initialized and running, in order to stop
			return YES;
		}
		return NO;
	} else if (theAction == @selector(stepSwarmSimulation:)) {
		if ((_theSwarm) && (_currentState == SG_STATE_STOP)) {
			// We have to be initialized and stopped, in order to step
			return YES;
        }
        return NO;
	}
	
	// subclass of NSDocument, so invoke super's implementation
	return [super validateUserInterfaceItem:anItem];
}

- (void)attachDisplays
{
	return;
}

- (void)swarmHasUpdated: (id)sender
{
	return;
}


//
// Reading/Writing model files
//
- (id)initWithType:(NSString *)typeName error:(NSError **)outError
{
	self = [super initWithType:typeName error:outError];
	if (self) {
		// Load the default parameters
		NSString *paramFile = [[NSBundle mainBundle] pathForResource: @"SwarmModel" ofType: typeName];
		simParameters = [[NSMutableDictionary dictionaryWithContentsOfFile: paramFile] retain];
		if (!simParameters) {
			NSLog(@"Could not get default parameter file\n");
		}
	}
	
	return self;
}


- (NSData *)dataOfType:(NSString *)typeName error:(NSError **)outError
{
	NSString *errDesc = nil;
    NSData *data = [NSPropertyListSerialization dataFromPropertyList: simParameters format: NSPropertyListXMLFormat_v1_0
													errorDescription: &errDesc];
	
	if (!data && outError) {
        *outError = [NSError errorWithDomain:NSCocoaErrorDomain
										code:NSFileWriteUnknownError userInfo:nil];
    }
    return data;
}

- (BOOL)readFromData:(NSData *)data ofType:(NSString *)typeName error:(NSError **)outError
{
	BOOL readSuccess = NO;
	NSString *errDesc = nil;
	simParameters = [NSPropertyListSerialization propertyListFromData: data mutabilityOption: NSPropertyListMutableContainersAndLeaves
															   format: NULL errorDescription: &errDesc];
    if (simParameters) {
        readSuccess = YES;
    }
	
	if ( outError != NULL ) {
		*outError = [NSError errorWithDomain:NSCocoaErrorDomain code:NSFileReadUnknownError userInfo:NULL];
	}
	
    return readSuccess;
}

//
// Manage simulation execution
//
- (Class)modelGUIClass { return NULL; }
- (NSDictionary *)simulationParameters { return simParameters; }

- (void)initializeSwarmSimulation: (id)sender
{
	printf("[OpenStepSwarmController initializeSwarmSimulation:]\n");
	
	if (_theSwarm) {
		NSLog(@"Tried to initialize an already initialized simulation.");
		return;
	}
	
	Class c = [self modelGUIClass];
	if (!c) {
		NSLog(@"Could not obtain GUI class.");
		NSString *s = [NSString stringWithFormat: @"Could not obtain GUI class.  Most likely your OpenStepSwarmController subclass (%s) has not overridden the -modelGUIClass method to return the SwarmGUI model class. Simulation cannot be initialized.", swarm_class_getName([self class])];
		NSRunAlertPanel(@"Internal Error: Simulation cannot be initialized", s, @"OK", NULL, NULL);
		return;
	}

	_currentState = SG_STATE_NEW;
	_nextState = SG_STATE_NOTHING;
	_theLock = [NSLock new];
	_theConditionLock = [[NSConditionLock alloc] initWithCondition: SG_COND_B];
	
	[NSThread detachNewThreadSelector: @selector(newSimulation:)
							 toTarget: self withObject: [self modelGUIClass]];
	
#if 1
	// Wait for swarm to be ready
	[_theConditionLock lockWhenCondition: SG_COND_A];
	[_theConditionLock unlockWithCondition: SG_COND_WAIT];
#endif
	
	// Now allow the user interface to attach any GUI displays
	// for the newly initialized simulation
	[self attachDisplays];
	[self swarmHasUpdated: self];
}

- (void)newSimulation: (Class)aSwarmClass
{
  NSAutoreleasePool *pool;

  printf("[GNUstepSwarmController newSimulation:]\n");

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

	[self simulationControl: nil];

#if 0
  [NSTimer scheduledTimerWithTimeInterval: 0.000001
	   target: self selector: @selector(simulationControl:)
	   userInfo: nil repeats: YES];

  printf("before run\n");
  [[NSRunLoop currentRunLoop] run];
  printf("after run\n");
#endif

  [NSThread exit];
}

//- (void)processStateFlagWithOriginal: (int)originalState
- (void)processStateFlag
{
  switch (_currentState)
    {
    case SG_STATE_DROP:
      printf("Terminating simulation\n");
      [getTopLevelActivity() terminate];
      break;
    case SG_STATE_START: {
      NSAutoreleasePool *pool = [NSAutoreleasePool new];
      [[_theSwarm getActivity] stepUntil: [[_theSwarm getActivity]
					    getCurrentTime] + 1];
      [pool release];
      break;
    }
    case SG_STATE_STEP: {
      NSAutoreleasePool *pool = [NSAutoreleasePool new];
      [[_theSwarm getActivity] stepUntil: [[_theSwarm getActivity]
					    getCurrentTime] + 1];
      [pool release];

      [_theConditionLock lockWhenCondition: SG_COND_RUN];
      [_theConditionLock unlockWithCondition: SG_COND_WAIT];
      _currentState = SG_STATE_STOP;
      break;
    }
    case SG_STATE_STOP:
      break;
    }
}

- (void)simulationControl: (NSTimer *)aTimer
{
	while (_currentState != SG_STATE_DROP) {
	
		// wait to start
		printf("waiting %d\n", _currentState);
		[_theConditionLock lockWhenCondition: SG_COND_RUN];
		_currentState = SG_STATE_START;
		[_theConditionLock unlockWithCondition: SG_COND_RUN];

		while ((_currentState == SG_STATE_START) 
				|| (_currentState == SG_STATE_STEP)) {

				//printf("running %d\n", [_theConditionLock condition]);

			// Check if the user initiated a new state
			if (_nextState != SG_STATE_NOTHING) {
				[_theLock lock];
				printf("Got new state %d\n", _nextState);
				_currentState = _nextState;
				_nextState = SG_STATE_NOTHING;
				[_theLock unlock];
			}

			//[self processStateFlagWithOriginal: originalState];
			[self processStateFlag];
		}
	}
}

- getSwarm
{
  return _theSwarm;
}

- (void)startSwarmSimulation: sender
{
	if ([_theConditionLock tryLockWhenCondition: SG_COND_WAIT]) {

		printf("startSwarmSimulation\n");
		[_theLock lock];
		_nextState = SG_STATE_START;
		[_theLock unlock];

		[_theConditionLock unlockWithCondition: SG_COND_RUN];

		// Change the menu item to Stop
		[sender setAction: @selector(stopSwarmSimulation:)];
		[sender setTitle: @"Stop"];
	}
}

- (void)stepSwarmSimulation: sender
{
	if ([_theConditionLock tryLockWhenCondition: SG_COND_WAIT]) {

		printf("stepSwarmSimulation\n");
		[_theLock lock];
		_nextState = SG_STATE_STEP;
		[_theLock unlock];
		[_theConditionLock unlockWithCondition: SG_COND_RUN];
	}
}

- (void)stopSwarmSimulation: sender
{
	if ([_theConditionLock tryLockWhenCondition: SG_COND_RUN]) {

		printf("stopSwarmSimulation\n");
		[_theLock lock];
		_nextState = SG_STATE_STOP;
		[_theLock unlock];

		[_theConditionLock unlockWithCondition: SG_COND_WAIT];

		// Change the menu item to Start
		[sender setAction: @selector(startSwarmSimulation:)];
		[sender setTitle: @"Start"];
	}
}

- (void)dropSwarmSimulation: sender
{
  [_theLock lock];
  _nextState = SG_STATE_DROP;
  [_theLock unlock];
}

@end

@implementation OpenStepGUISwarm

PHASE(Creating)

+ createBegin: aZone
{
  OpenStepGUISwarm *obj;

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

- (void)swarmHasUpdatedNotification
{
	// notify the main thread the display have updated
	[_threadController performSelectorOnMainThread: @selector(swarmHasUpdated:)
						   withObject: self
						waitUntilDone: YES];
}

- (NSDictionary *)simulationParameters
{
	return [_threadController simulationParameters];
}

@end

//
//
//

@implementation OpenStepSwarmModel

+ create: (id)aSwarm withParameters: (NSDictionary *)params
{
	OpenStepSwarmModel *obj;
	
	obj = [super createBegin: aSwarm];
	obj->simulationParameters = params;
	
	return obj;
}

@end
