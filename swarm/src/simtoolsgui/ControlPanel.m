// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <activity.h>
#import <simtools.h>
#import <defobj.h> // archiverSave

#import <simtoolsgui/ControlPanel.h>
#import <simtoolsgui/global.h>

#import <gui.h>

#ifdef USE_JAVA
#include <awtobjc/JavaControlPanel.h>
#endif

// Rudimentary control panel. A lot of the work for making this useful
// falls on the shoulders of the controller that's using us, typically
// an observer swarm.

id <Symbol> ControlStateRunning, ControlStateStopped;
id <Symbol> ControlStateStepping, ControlStateNextTime, ControlStateQuit;

@implementation ControlPanel

PHASE(Creating)

- createEnd
{
  [super createEnd];
  
  // default state is Stopped.
  state = ControlStateStopped;
  
  return self;
}

PHASE(Using)

#ifndef USE_JAVA
- getState
{
  return state;
}

- setState: theState
{
  state = theState;

  return self;
}
#else
- getState
{
  return [ctlObj getState];
}

- setState: s
{
  return [ctlObj setState: s];
}
#endif

// Run: just set our own state to running, let whatever object who
// is using us arrange for the run to go again.
- startInActivity: activityID
{
  id controlState, activityState;
  
  while (YES)
    {
      controlState = [self getState];
      activityState = [activityID getStatus];
      
      if ((controlState == ControlStateRunning)
          && !RUNNINGP (activityState))
        activityState = [activityID run];
      else if (controlState == ControlStateStopped)
        {
          if ((state == ControlStateStopped) && _activity_current)
            return state;
          else
            [self setStateStopped];
        }
      else if (controlState == ControlStateQuit)
        return Completed;  // this returns to go,which returns to main
      else if (controlState == ControlStateStepping) 
        {
          [activityID step];
          [self setStateStopped];
        }
      else if (controlState == ControlStateNextTime)
        {
          [activityID stepUntil: [activityID getCurrentTime] + 1];
          [self setStateStopped];
        }
      else [self setStateStopped];
    }

  return [activityID getStatus];
}

#ifdef USE_JAVA
- setCtlObj: ctl
{
  ctlObj = ctl;

  return self;
}
#endif

// Stop: set state to stop, also stop activities.
- setStateStopped
{
  //  if (getTopLevelActivity()){
  [probeDisplayManager setDropImmediatelyFlag: YES];
  if (_activity_current)
    {
      [getTopLevelActivity() stop];
      return [self setState: ControlStateStopped];
    }
  else
    {
#ifndef USE_JAVA
#if 0
      [self waitForControlEvent];
#else
      [self setState: ControlStateStopped];
      while (state == ControlStateStopped)
        GUI_EVENT_SYNC ();
#endif
#else
      [self setState: ControlStateStopped];
      [ctlObj waitRun];
#endif
      // Check now if the user hit the quit button: if so, abort.
      if (state == ControlStateQuit)
        exit(0);
      else
        return self;
    }
}

- setStateSave
{
  archiverSave ();

  return self;
}

- setStateRunning 
{
  [probeDisplayManager setDropImmediatelyFlag: NO];

  return [self setState: ControlStateRunning];
}

// Step: first, stop the running activity (we're probably already stopped,
// though). Then set our own state to Stepping.
- setStateStepping
{
  [probeDisplayManager setDropImmediatelyFlag: YES];
  if (_activity_current)
  //  if (getTopLevelActivity())
    [getTopLevelActivity() stop];

  return [self setState: ControlStateStepping];
}

// Next time: first, stop the running activity (we're probably already stopped,
// though). Then set our own state to NextTime.
- setStateNextTime
{
  if (_activity_current)
    //  if (getTopLevelActivity())
    [getTopLevelActivity() stop];

  return [self setState: ControlStateNextTime];
}

// Quit: set state to quit, also terminate activities.
- setStateQuit
{
  //  if (getTopLevelActivity())
  if (_activity_current)
    [getTopLevelActivity() terminate];

  return [self setState: ControlStateQuit];
}

@end
