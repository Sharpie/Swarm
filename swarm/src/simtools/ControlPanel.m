// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtools/ControlPanel.h>
#import <tkobjc/global.h>
#import <activity.h>

// Rudimentary control panel. A lot of the work for making this useful
// falls on the shoulders of the controller that's using us, typically
// an observer swarm.

id ControlStateRunning, ControlStateStopped;
id ControlStateStepping, ControlStateNextTime, ControlStateQuit;

@implementation ControlPanel
-createEnd {
  [super createEnd];

  // Make a command for ourselves
  [globalTkInterp registerObject: self withName: "simctl"];
  
  // make a widget for us, too. Bind buttons to messages to ourself.
  panel = [ButtonPanel create: [self getZone]];
  [panel addButtonName: "Go" Command: "simctl setStateRunning"];
  [panel addButtonName: "Stop" Command: "simctl setStateStopped"];
#ifdef MICROSTEPBUTTON
  [panel addButtonName: "Step" Command: "simctl setStateStepping"];
#endif
  [panel addButtonName: "Time Step" Command: "simctl setStateNextTime"];
  [panel addButtonName: "Quit" Command: "simctl setStateQuit"];
  [panel setWindowTitle: "Swarm"];
  [panel pack];

  // default state is Stopped.
  state = ControlStateStopped;
  
  return self;
}

-(ButtonPanel *) getPanel {
  return panel;
}

-getState {
  return state;
}

-setState: (id) s {
  state = s;
  return self;
}


// doTkEvents as a side effect (via button bindings) will set our own state
// Wait for that state to change from Stopped, then return.
-waitForControlEvent {
  while (state == ControlStateStopped)
    Tk_DoOneEvent(TK_ALL_EVENTS);		  // block for user event.
  return self;
}

// Do the Tk Events.
// First, let Tk process all the events around.
// Second, if we're stopped and not ready to quit, keep processing events
//   until one of those conditions changes. (This lets the user press the
//   Go or Quit buttons to quit.)
// Finally, return a status that tells whether we need to quit.
-doTkEvents {
  // do all events pending, but don't block.
  while(Tk_DoOneEvent(TK_ALL_EVENTS|TK_DONT_WAIT))
    ;
  return self;
}


// These methods are bound to the Tk buttons. They get invoked directly
// by the Tk interpreter (via tclobjc). ObserverSwarm uses these states
// to control simulation execution.

// Run: just set our own state to running, let whatever object who
// is using us arrange for the run to go again.
-setStateRunning {
  return [self setState: ControlStateRunning];
}

// Stop: set state to stop, also stop activities.
-setStateStopped {
  //  if (getTopLevelActivity()){
  if (_activity_current) {
    [getTopLevelActivity() stop];
    return [self setState: ControlStateStopped];
  } else {
    [self setState: ControlStateStopped] ;
    [self waitForControlEvent];
    // Check now if the user hit the quit button: if so, abort.
    if ([self getState] == ControlStateQuit)
      exit(0) ;
    else
      return self ;
  }
}

// Step: first, stop the running activity (we're probably already stopped,
// though). Then set our own state to Stepping.
-setStateStepping {
  if (_activity_current)
  //  if (getTopLevelActivity())
    [getTopLevelActivity() stop];
  return [self setState: ControlStateStepping];
}

// Next time: first, stop the running activity (we're probably already stopped,
// though). Then set our own state to NextTime.
-setStateNextTime {
  if (_activity_current)
    //  if (getTopLevelActivity())
    [getTopLevelActivity() stop];
  return [self setState: ControlStateNextTime];
}

// Quit: set state to quit, also terminate activities.
-setStateQuit {
  //  if (getTopLevelActivity())
  if (_activity_current)
    [getTopLevelActivity() terminate];
  return [self setState: ControlStateQuit];
}


@end
