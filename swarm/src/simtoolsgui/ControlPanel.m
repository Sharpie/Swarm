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

#import <simtoolsgui.h> // ControlPanel
#import <simtoolsgui/ControlPanel.h>
#import <defobj.h> // hdf5Archiver, lispArchiver

#import <simtoolsgui.h> // probeDisplayManager

#import <gui.h>

#ifdef USE_JAVA
#include <awtobjc/JavaControlPanel.h>
#endif

#include <swarmconfig.h> // HAVE_HDF5

// Rudimentary control panel. A lot of the work for making this useful
// falls on the shoulders of the controller that's using us, typically
// an observer swarm.

externvardef id <Symbol> ControlStateRunning, ControlStateStopped;
externvardef id <Symbol> ControlStateStepping, ControlStateNextTime, ControlStateQuit;

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
- (id <Symbol>)getState
{
  return state;
}

- setState: (id <Symbol>)theState
{
  state = theState;

  if (state == ControlStateStopped)
    [probeDisplayManager setDropImmediatelyFlag: YES];
  else if (state == ControlStateRunning)
    [probeDisplayManager setDropImmediatelyFlag: NO];

  return self;
}
#else
- (id <Symbol>)getState
{
  return [ctlObj getState];
}

- setState: (id <Symbol>)s
{
  return [ctlObj setState: s];
}
#endif

// Run: just set our own state to running, let whatever object who
// is using us arrange for the run to go again.
- startInActivity: (id <SwarmActivity>)activityID
{
  id <Symbol> controlState, activityState;
  
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
          [activityID stepAction];
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
      return self;
    }
}

- setStateSave
{
  [lispArchiver sync];
#ifdef HAVE_HDF5
  [hdf5Archiver sync];
#endif
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
