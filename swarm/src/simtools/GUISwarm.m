// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtools/GUISwarm.h>

@implementation GUISwarm

-buildObjects {
  [super buildObjects];
  controlPanel = [ControlPanel create: [self getZone]];
  return self;
}

// The real work here is calling [swarmActivity run], which runs our own
// activity. In addition, control panel logic is being handled here.
-go {
  // infinite loop: this loop only exits on a return statement, either
  //   controlStateQuit: the user hit "quit" before the activities were done
  //   Completed: the activity library indicated it was done running us
  while (1) {
    id controlState, swarmStatus;
    // These variables indicate the running state: both the activity
    // library's notion of where we are, as well as the control panel.
    swarmStatus = [[self getActivity] getStatus];
    controlState = [controlPanel getState];

    // First check if we can exit because our Swarm thinks we're done.
    if (swarmStatus == Completed) {
      // We're ready to return a value - just check if the user hit "quit".
      if (controlState == ControlStateQuit) {
	// Ok, the user has hit the quit button while we were running.
	return ControlStateQuit;
      } else {
	// Looks like the schedule ran to completion, or at least exited
	// for some reason other than the user hitting "Quit".
	return Completed;
      }
    } else {
      // The activity library has not finished executing - that means we
      // need to arrange for some other sort of action.
      if (controlState == ControlStateRunning) {
	// Control panel says we're ready to run, so run!
	[[self getActivity] run];
      } else if (controlState == ControlStateStopped) {
	// Control panel says we're stopped, so let's wait for a go.
	[controlPanel waitForControlEvent];
      } else if (controlState == ControlStateQuit) {
	// Control panel says to quit, even though activity hasn't. Quit.
	return ControlStateQuit;
      } else if (controlState == ControlStateStepping) {
	[[self getActivity] step];
	[controlPanel setStateStopped];
      } else if (controlState == ControlStateNextTime) {
	[[self getActivity] stepUntil: [[self getActivity] getCurrentTime]+1 ];
	[controlPanel setStateStopped];
      } else {
	[controlPanel setStateStopped];
      }
    }
  }

  // That while loop should never exit, so just put a check here.
  raiseEvent(SourceMessage, "The observer Swarm go loop exited improperly.\n");
  return nil;
}

@end
