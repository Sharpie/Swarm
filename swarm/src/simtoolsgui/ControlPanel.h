// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Control Panel class, for use in GUIs. Controls the state of running
// activities, provides the "stop" and "go" buttons. Also handles updating
// the Tk event loop.

#import <objectbase/SwarmObject.h>

@interface ControlPanel: SwarmObject
{
  id state;					  // uses activity.h symbols
#ifdef USE_JAVA
  id ctlObj;
#endif
}

- createEnd;
- getState;
- setState: s;

- startInActivity: activityID;
- setStateRunning;
- setStateStopped;
- setStateSave;
- setStateStepping;
- setStateQuit;
- setStateNextTime;

#ifdef USE_JAVA
- setCtlObj: ctl;
#endif

@end

extern id <Symbol> ControlStateRunning, ControlStateStopped, ControlStateStepping, ControlStateNextTime, ControlStateQuit;
